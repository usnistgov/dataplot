      SUBROUTINE DPI(NPLOTV,NPLOTP,NS,ICASPL,ISEED,IAND1,IAND2,   &
                     ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE ONE OF THE FOLLOWING 4 I PLOTS--
!
!              1) MEDIAN;
!              2) MEAN;
!              3) MIDRANGE;
!              4) MIDMEAN;
!              5) TRIMMED MEAN;
!              6) BIWEIGHT;
!
!              NOTE 2013/10: THIS PLOT HAS BEEN UPDATED IN THE
!              FOLLOWING WAYS:
!
!              1) ADD THE FOLLOWING VARIANTS:
!
!                    ONE STANDARD ERROR PLOT
!                    TWO STANDARD ERROR PLOT
!                    ONE STANDARD DEVIATION PLOT
!                    TWO STANDARD DEVIATION PLOT
!                    MEAN CONFIDENCE LIMIT PLOT
!                    MEDIAN CONFIDENCE LIMIT PLOT
!                    QUANTILE CONFIDENCE LIMIT PLOT
!                    TRIMMED MEAN CONFIDENCE LIMIT PLOT
!                    BIWEIGHT CONFIDENCE LIMIT PLOT
!                    NORMAL TOLERANCE LIMIT PLOT
!                    NORMAL PREDICTION LIMIT PLOT
!                    STANDARD DEVIATION CONFIDENCE LIMIT PLOT
!                    AGRESTI COUL CONFIDENCE LIMIT PLOT
!
!                    FOLLOWING ADDED 11/2017:
!                    DIFFERENCE OF PROPORTION CONFIDENCE LIMIT PLOT
!                    COEFFICENT OF VARIATION CONFIDENCE LIMIT PLOT
!                    COEFFICENT OF DISPERSION CONFIDENCE LIMIT PLOT
!
!                    FOLLOWING ADDED 12/2017:
!                    COEFFICENT OF QUARTILE DISPERSION CONFIDENCE LIMIT PLOT
!
!                    FOLLOWING ADDED 04/2018:
!                    CORRELATION CONFIDENCE LIMIT PLOT
!
!                    FOLLOWING ADDED 10/2019:
!                    RATIO OF MEANS CONFIDENCE LIMIT PLOT
!
!                 RATHER THAN THE LOCATION/MIN/MAX FORM OF THE
!                 PLOT, THESE WILL GENERATE A POINT ESTIMATE,
!                 LOWER INTERVAL, AND UPPER INTERVAL.
!
!              2) ADD A 3-VARIABLE FORM OF THE PLOT:
!
!                    I-PLOT Y X TAG
!
!                 THIS HANDLES REPLICATION IN A DIFFERENT WAY
!                 THAN THE "REPLICATED" OPTION.  WITH THE REPLICATION
!                 OPTION, GIVEN X1 = 1, 2, 3 AND X2 = 1, 2, THE
!                 X-COORDINATES FOR THE PLOT WILL BE
!
!                     X1     X2  |  X-COOR
!                     ====================
!                      1      1          1
!                      2      1          2
!                      3      1          3
!                      1      2          4
!                      2      2          5
!                      3      2          6
!
!                 WITH THE 3-VARIABLE FORM OF THE PLOT, THE
!                 X-COORDINATES WILL BE AS FOLLOWS
!
!
!                     X1     X2  |  X-COOR
!                     ====================
!                      1      1        0.8
!                      2      1        1.8
!                      3      1        2.8
!                      1      2        1.2
!                      2      2        2.2
!                      3      2        3.2
!
!                 ALSO, THE 3-VARIABLE FORM ALLOWS THE X2 GROUPS
!                 TO BE DRAWN WITH DIFFERENT ATTRIBUTES (E.G.,
!                 DIFFERENT COLORS) WHILE THE REPLICATED OPTION
!                 USES THE SAME ATTRIBUTES.
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JANUARY   1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR3 TO PERFORM
!                                       THE COMMAND PARSING
!     UPDATED         --FEBRUARY  2011. SUPPORT FOR "MULTIPLE" CASE
!     UPDATED         --FEBRUARY  2011. SUPPORT FOR TWO GROUP-ID VARIABLES
!     UPDATED         --OCTOBER   2013. SUPPORT FOR THREE-VARIABLE FORM
!     UPDATED         --OCTOBER   2013. SUPPORT FOR ALTERNATIVES TO
!                                       MEDIAN I PLOT
!     UPDATED         --OCTOBER   2013. SUPPORT FOR "INTERVAL" TYPE
!                                       PLOTS
!     UPDATED         --JULY      2016. FOR MULTIPLE CASE, ALLOW
!                                       CREATION OF "STACKED" VARIABLES
!                                       THAT ARE 5*MAXOBV
!     UPDATED         --NOVEMBER  2017. UPDATES TO AGRESTI-COUL
!                                       CONFIDENCE LIMTIS
!     UPDATED         --NOVEMBER  2017. DIFFERENCE OF PROPORTIONS
!     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEANS
!     UPDATED         --NOVEMBER  2017. COEFFICIENT OF VARIATION
!     UPDATED         --NOVEMBER  2017. COEFFICIENT OF DISPERSION
!     UPDATED         --DECEMBER  2017. COEFFICIENT OF QUARTILE DISPERSION
!     UPDATED         --APRIL     2018. CORRELATION
!     UPDATED         --JUNE      2018. SUPPORT FOR UNEQUAL SAMPLE SIZES
!                                       FOR DIFFERENCE OF MEAN AND
!                                       DIFFERENCE OF PROPORTION
!     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
!     UPDATED         --OCTOBER   2019. RATIO OF MEANS CONFIDENCE LIMIT
!                                       PLOT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IREPL
      CHARACTER*4 IREP2
      CHARACTER*4 IMULT
      CHARACTER*4 IWRITE
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
!
      CHARACTER*4 IA1
      CHARACTER*4 IA2
      CHARACTER*4 IA3
      CHARACTER*4 IA4
      CHARACTER*4 IA5
      CHARACTER*4 IA6
      CHARACTER*4 IA7
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y1(5*MAXOBV)
      DIMENSION Y2(5*MAXOBV)
      DIMENSION X1(5*MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION X4(MAXOBV)
      DIMENSION X5(MAXOBV)
      DIMENSION X6(MAXOBV)
!
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
!
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
      EQUIVALENCE (GARBAG(JGAR18),TEMP(1))
      EQUIVALENCE (GARBAG(IGAR11),Y1(1))
      EQUIVALENCE (GARBAG(IGAR16),Y2(1))
      EQUIVALENCE (GARBAG(IGAR21),X1(1))
!
!-----COMMON----------------------------------------------------------
!
      COMMON/IPLOT/NREPI1,NREPI2
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      IFOUN1='NO'
      IFOUN2='NO'
      IWRITE='OFF'
      ISUBN1='DPI '
      ISUBN2='    '
!
      NREPI1=0
      NREPI2=0
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               *******************************
!               **  TREAT THE I   PLOT CASE  **
!               *******************************
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,NS
   52   FORMAT('ICASPL,IAND1,IAND2,NS = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICONT,IBUGG2,IBUGG3,IBUGQ
   53   FORMAT('ICONT,IBUGG2,IBUGG3,IBUGQ = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  EXTRACT THE COMMAND                             **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:         **
!               **    1) I PLOT Y X1 ... X2                         **
!               **    2) MULTIPLE I PLOT Y1 ... YK                  **
!               **    3) REPLICATED I PLOT Y X1 X2                  **
!               **  THE "REPLICATION" CASE IS ACTUALLY THE DEFAULT  **
!               **  AND THE KEYWORD "REPLICATION" IS OPTIONAL.      **
!               **  HOWEVER, SUPPORT IT FOR COMPATABILITY WITH      **
!               **  OTHER COMMANDS.                                 **
!               ******************************************************
!
!     NOTE 2013/10: FOLLOWING ADDITIONAL COMMANDS ADDED:
!
!          ONE STANDARD ERROR PLOT
!          TWO STANDARD ERROR PLOT
!          MEAN CONFIDENCE LIMIT PLOT
!          MEDIAN CONFIDENCE LIMIT PLOT
!          QUANTILE CONFIDENCE LIMIT PLOT
!          TRIMMED MEAN CONFIDENCE LIMIT PLOT
!          BIWEIGHT CONFIDENCE LIMIT PLOT
!          STANDARD DEVIATION CONFIDENCE LIMIT PLOT
!          NORMAL TOLERANCE LIMIT PLOT
!          NORMAL PREDICTION LIMIT PLOT
!          AGRESTI COUL CONFIDENCE LIMIT PLOT
!
!     NOTE 2017/11: FOLLOWING ADDITIONAL COMMANDS ADDED:
!
!          DIFFERENCE OF MEANS CONFIDENCE LIMIT PLOT
!          DIFFERENCE OF PROPORTIONS CONFIDENCE LIMIT PLOT
!          STANDARD DEVIATION CONFIDENCE LIMIT PLOT
!          COEFFICIENT OF VARIATION CONFIDENCE LIMIT PLOT
!          COEFFICIENT OF DISPERSION CONFIDENCE LIMIT PLOT
!
!     NOTE 2017/11: FOLLOWING ADDITIONAL COMMANDS ADDED:
!
!          COEFFICIENT OF QUARTILE DISPERSION CONFIDENCE LIMIT PLOT
!
!     NOTE 2018/04: FOLLOWING ADDITIONAL COMMANDS ADDED:
!
!          CORRELATION CONFIDENCE LIMIT PLOT
!
!     NOTE 2019/10: FOLLOWING ADDITIONAL COMMANDS ADDED:
!
!          RATIO OF MEANS CONFIDENCE LIMIT PLOT
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'I')GO TO 89
      IF(ICOM.EQ.'MULT')GO TO 89
      IF(ICOM.EQ.'REPL')GO TO 89
      IF(ICOM.EQ.'MEAN')GO TO 89
      IF(ICOM.EQ.'MEDI')GO TO 89
      IF(ICOM.EQ.'MIDR')GO TO 89
      IF(ICOM.EQ.'MIDM')GO TO 89
      IF(ICOM.EQ.'MEDI')GO TO 89
      IF(ICOM.EQ.'QUAN')GO TO 89
      IF(ICOM.EQ.'TRIM')GO TO 89
      IF(ICOM.EQ.'BIWE')GO TO 89
      IF(ICOM.EQ.'NORM')GO TO 89
      IF(ICOM.EQ.'STAN' .AND. IHARG(1).EQ.'DEVI')GO TO 89
      IF(ICOM.EQ.'SD  ')GO TO 89
      IF(ICOM.EQ.'ONE' .OR. ICOM.EQ.'1')GO TO 89
      IF(ICOM.EQ.'TWO' .OR. ICOM.EQ.'2')GO TO 89
      IF(ICOM.EQ.'AGRE')GO TO 89
      IF(ICOM.EQ.'PROP')GO TO 89
      IF(ICOM.EQ.'BINO' .AND. IHARG(1).EQ.'PROP')GO TO 89
      IF(ICOM.EQ.'COEF')GO TO 89
      IF(ICOM.EQ.'DIFF')GO TO 89
      IF(ICOM.EQ.'CORR')GO TO 89
      IF(ICOM.EQ.'RATI' .AND. IHARG(1).EQ.'OF  ' .AND.   &
         IHARG(2).EQ.'MEAN')GO TO 89
      GO TO 9000
!
   89 CONTINUE
      ICASPL='MDIP'
      IMULT='OFF'
      IREPL='OFF'
      IREP2='OFF'
      ILASTC=-9999
      ISTOP=NUMARG
      NRESP=1
      DO 91 I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT')THEN
          ISTOP=I
          GO TO 93
        ENDIF
   91 CONTINUE
   93 CONTINUE
!
      DO 100 I=0,ISTOP
!
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
!
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(IA1.EQ.'REPL')THEN
          IREPL='ON'
        ELSEIF(IA1.EQ.'MULT')THEN
          IMULT='ON'
        ELSEIF(IA1.EQ.'I' .AND. IA2.EQ.'PLOT')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+1)
          GO TO 109
        ELSEIF(IA1.EQ.'MEDI' .AND. IA2.EQ.'I' .AND.   &
               IA3.EQ.'PLOT')THEN
          ICASPL='MDIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GO TO 109
        ELSEIF(IA1.EQ.'TRIM' .AND. IA2.EQ.'MEAN' .AND.   &
               IA3.EQ.'I   ' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='TMIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'MEAN' .AND. IA2.EQ.'I' .AND.   &
               IA3.EQ.'PLOT')THEN
          ICASPL='MEIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GO TO 109
        ELSEIF(IA1.EQ.'MIDR' .AND. IA2.EQ.'I' .AND.   &
               IA3.EQ.'PLOT')THEN
          ICASPL='MRIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GO TO 109
        ELSEIF(IA1.EQ.'MIDM' .AND. IA2.EQ.'I' .AND.   &
               IA3.EQ.'PLOT')THEN
          ICASPL='MMIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GO TO 109
        ELSEIF(IA1.EQ.'BIWE' .AND. IA2.EQ.'I' .AND.   &
               IA3.EQ.'PLOT')THEN
          ICASPL='BWIP'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          GO TO 109
        ELSEIF(IA1.EQ.'TRIM' .AND. IA2.EQ.'MEAN' .AND.   &
               IA3.EQ.'CONF' .AND. IA4.EQ.'LIMI' .AND.   &
               IA5.EQ.'PLOT')THEN
          ICASPL='TMCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+4)
          GO TO 109
        ELSEIF(IA1.EQ.'DIFF' .AND. IA2.EQ.'OF  ' .AND.   &
               IA3.EQ.'MEAN' .AND. IA4.EQ.'CONF' .AND.   &
               IA5.EQ.'LIMI' .AND. IA6.EQ.'PLOT')THEN
          ICASPL='DMEA'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+5)
          NRESP=2
          GO TO 109
        ELSEIF(IA1.EQ.'MEAN' .AND. IA2.EQ.'CONF' .AND.   &
               IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='MECL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'SD  ' .AND. IA2.EQ.'CONF' .AND.   &
               IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='SDCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'STAN' .AND. IA2.EQ.'DEVI' .AND.   &
               IA3.EQ.'CONF' .AND. IA4.EQ.'LIMI' .AND.   &
               IA5.EQ.'PLOT')THEN
          ICASPL='SDCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+4)
          GO TO 109
        ELSEIF(IA1.EQ.'MEDI' .AND. IA2.EQ.'CONF' .AND.   &
               IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='MDCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'QUAN' .AND. IA2.EQ.'CONF' .AND.   &
               IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='QUCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'BIWE' .AND. IA2.EQ.'CONF' .AND.   &
               IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='BWCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'DIFF' .AND. IA2.EQ.'OF  ' .AND.   &
               IA3.EQ.'PROP' .AND. IA4.EQ.'CONF' .AND.   &
               IA5.EQ.'LIMI' .AND. IA6.EQ.'PLOT')THEN
          ICASPL='DPRO'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+5)
          NRESP=2
          GO TO 109
        ELSEIF(IA1.EQ.'DIFF' .AND. IA2.EQ.'OF  ' .AND.   &
               IA3.EQ.'BINO' .AND. IA4.EQ.'PROP' .AND.   &
               IA5.EQ.'CONF' .AND. IA6.EQ.'LIMI' .AND.   &
               IA7.EQ.'PLOT')THEN
          ICASPL='DPRO'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+6)
          NRESP=2
          GO TO 109
        ELSEIF(IA1.EQ.'AGRE' .AND. IA2.EQ.'COUL' .AND.   &
               IA3.EQ.'CONF' .AND. IA4.EQ.'LIMI' .AND.   &
               IA5.EQ.'PLOT')THEN
!
!         2017/11: ADD THE FOLLOWING SYNONYMS FOR
!                  AGRESTI COUL
!         PROPORTION CONFIDENCE LIMIT
!         BINOMIAL PROPORTION CONFIDENCE LIMIT
!
          ICASPL='AGCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+4)
          GO TO 109
        ELSEIF(IA1.EQ.'BINO' .AND. IA2.EQ.'PROP' .AND.   &
               IA3.EQ.'CONF' .AND. IA4.EQ.'LIMI' .AND.   &
               IA5.EQ.'PLOT')THEN
          ICASPL='AGCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+4)
          GO TO 109
        ELSEIF(IA1.EQ.'PROP' .AND. IA2.EQ.'CONF' .AND.   &
               IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='AGCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'COEF' .AND. IA2.EQ.'OF  ' .AND.   &
               IA3.EQ.'VARI' .AND. IA4.EQ.'CONF' .AND.   &
               IA5.EQ.'LIMI' .AND. IA6.EQ.'PLOT')THEN
          ICASPL='CVCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+5)
          GO TO 109
        ELSEIF(IA1.EQ.'COEF' .AND. IA2.EQ.'OF  ' .AND.   &
               IA3.EQ.'DISP' .AND. IA4.EQ.'CONF' .AND.   &
               IA5.EQ.'LIMI' .AND. IA6.EQ.'PLOT')THEN
          ICASPL='CDCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+5)
          GO TO 109
        ELSEIF(IA1.EQ.'COEF' .AND. IA2.EQ.'OF  ' .AND.   &
               IA3.EQ.'QUAR' .AND. IA4.EQ.'DISP' .AND.   &
               IA5.EQ.'CONF' .AND. IA6.EQ.'LIMI' .AND.   &
               IA7.EQ.'PLOT')THEN
          ICASPL='CQCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+6)
          GO TO 109
        ELSEIF((IA1.EQ.'ONE ' .OR. IA1.EQ.'1') .AND.   &
                IA2.EQ.'STAN' .AND. IA3.EQ.'ERRO' .AND.   &
                IA4.EQ.'PLOT')THEN
          ICASPL='1SE '
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF((IA1.EQ.'TWO ' .OR. IA1.EQ.'2') .AND.   &
                IA2.EQ.'STAN' .AND. IA3.EQ.'ERRO' .AND.   &
                IA4.EQ.'PLOT')THEN
          ICASPL='2SE '
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
        ELSEIF((IA1.EQ.'ONE ' .OR. IA1.EQ.'1') .AND.   &
                IA2.EQ.'STAN' .AND. IA3.EQ.'DEVI' .AND.   &
                IA4.EQ.'PLOT')THEN
          ICASPL='1SD '
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF((IA1.EQ.'TWO ' .OR. IA1.EQ.'2') .AND.   &
                IA2.EQ.'STAN' .AND. IA3.EQ.'DEVI' .AND.   &
                IA4.EQ.'PLOT')THEN
          ICASPL='2SD '
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
        ELSEIF(IA1.EQ.'NORM' .AND. IA2.EQ.'TOLE' .AND.   &
                IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='NTOL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'NORM' .AND. IA2.EQ.'PRED' .AND.   &
                IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='NPRE'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          GO TO 109
        ELSEIF(IA1.EQ.'CORR' .AND. IA2.EQ.'CONF' .AND.   &
               IA3.EQ.'LIMI' .AND. IA4.EQ.'PLOT')THEN
          ICASPL='CRCL'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          NRESP=2
          GO TO 109
        ELSEIF(IA1.EQ.'RATI' .AND. IA2.EQ.'OF  ' .AND.   &
               IA3.EQ.'MEAN' .AND. IA4.EQ.'CONF' .AND.   &
               (IA5.EQ.'LIMI' .OR. IA5.EQ.'INTE') .AND.  &
               IA6.EQ.'PLOT')THEN
          ICASPL='RMEA'
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+3)
          NRESP=2
          GO TO 109
        ENDIF
  100 CONTINUE
  109 CONTINUE
!
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')IFOUND='YES'
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN I PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,107)
  107     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION" FOR THIS PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPI')THEN
        WRITE(ICOUT,112)ICASPL,IMULT,IREPL
  112   FORMAT('ICASPL,IMULT,IREPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
!     DIFFERENCE OF MEAN AND DIFFERENCE OF PROPORTION SUPPORT 4 VARIABLE
!     CASE (Y1 TAG1 Y2 TAG2) TO HANDLE UNEQUAL SAMPLE SIZES.  HOWEVER,
!     DO NOT SUPPORT THIS IF EITHER REPLICATION OR MULTIPLE SWITCH
!     IS ON.
!
      IF(IREPL.EQ.'OFF' .AND. IMULT.EQ.'OFF')THEN
        IF(ICASPL.EQ.'DMEA' .OR. ICASPL.EQ.'DPRO')THEN
          IFLAGE=19
          MINNVA=3
          MAXNVA=4
        ENDIF
      ENDIF
!
!     NOTE: NEED TO KEEP "I PLOT Y" AS VALID SYNTAX, SO
!           MINIMUM NUMBER OF VARIABLES IS 1 EVEN FOR REPLICATION
!           CASE.
!
      IF(IMULT.EQ.'ON')THEN
        MAXNVA=30
      ELSE
        MAXNVA=NRESP+6
      ENDIF
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,                                   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,       &
                  IVARN1,IVARN2,IVARTY,PVAR,                   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,      &
                  MINNVA,MAXNVA,                               &
                  IFLAGM,IFLAGP,                               &
                  IBUGG2,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
      IF(IREPL.EQ.'OFF' .AND. IMULT.EQ.'OFF' .AND.   &
         NUMVAR.EQ.NRESP+2)THEN
         IREP2='ON'
      ELSEIF(IREPL.EQ.'OFF' .AND. IMULT.EQ.'OFF')THEN
        IREPL='ON'
      ENDIF
!
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
          GO TO 9000
        ENDIF
      ELSE
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.0 .OR. NREPL.GT.6)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',   &
                 'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN 0 AND 6;  SUCH WAS NOT THE ',   &
                 'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PBOX')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IH='ALPH'
      IH2='A   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,                                      &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALPHA=0.05
        IF(ICASPL.EQ.'NTOL')ALPHA=0.95
      ELSE
        ALPHA=VALUE(ILOCP)
        IF(ALPHA.GE.1.0 .AND. ALPHA.LE.100.0)ALPHA=ALPHA/100.
        IF(ALPHA.GT.0.5 .AND. ALPHA.LT.1.0)ALPHA=1.0 - ALPHA
        IF(ALPHA.LE.0.0 .OR. ALPHA.GE.0.5)ALPHA=0.05
      ENDIF
!
      IF(ICASPL.EQ.'NTOL')THEN
        IH='GAMM'
        IH2='A   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,                                      &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
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
!
      IF(ICASPL.EQ.'NPRE')THEN
        IH='NNEW'
        IH2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,                                      &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'NO')THEN
          NNEW=INT(VALUE(ILOCP)+0.5)
        ELSE
          NNEW=1
        ENDIF
        IF(NNEW.LT.1)NNEW=1
      ELSE
        NNEW=0
      ENDIF
!
      IH='P1  '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,                                      &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        P1=0.25
      ELSE
        P1=VALUE(ILOCP)
        IF(P1.GE.1.0 .AND. P1.LE.50.0)P1=P1/100.
        IF(P1.LE.0.0 .OR. P1.GE.0.4)P1=0.25
      ENDIF
!
      IH='P2  '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,                                      &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        P2=0.75
      ELSE
        P2=VALUE(ILOCP)
        IF(P2.GE.50.0 .AND. P2.LE.100.0)P2=P2/100.
        IF(P2.LE.0.50 .OR. P2.GT.1.0)P2=0.75
      ENDIF
!
      IH='XQ  '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,                                      &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        P100=0.50
      ELSE
        P100=VALUE(ILOCP)
        IF(P100.GE.1.0 .AND. P100.LE.100.0)P100=P100/100.
        IF(P100.LE.0.0 .OR. P100.GE.1.0)P100=0.50
      ENDIF
!
!               ********************************************************
!               **  STEP 7--                                          **
!               **  FOR THE 1-VARIABLE CASE ONLY,                     *
!               **  DETERMINE IF THE ANALYST                          **
!               **  HAS SPECIFIED    THE GROUP SIZE,                  **
!               **  FOR THE I   PLOT      ANALYSIS.                   **
!               **  THE GROUP SIZE SETTING IS DEFINED BY SEARCHING THE**
!               **  INTERNAL TABLE FOR THE PARAMETER NAME      NI   ; **
!               **  IF FOUND, USE THE SPECIFIED VALUE.                **
!               **  IF NOT FOUND, GENERATE AN ERROR MESSAGE.          **
!               ********************************************************
!
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               **************************************************
!               **  STEP 7A--                                   **
!               **  CASE 1: NO "MULTIPLE" CASE--CAN HAVE EITHER **
!               **          1, 2, OR 3 VARIABLES.  THE FIRST    **
!               **          VARIABLE IS A RESPONSE VARIABLE     **
!               **          AND THE SECOND AND THIRD VARIABLES  **
!               **          ARE REPLICATION VARIABLES (IF       **
!               **          PRESENT).  NOTE THAT THIS VERSION   **
!               **          DOES NOT ACCEPT MATRIX ARGUMENTS    **
!               **          EVEN IF ONLY A SINGLE ARGUMENT IS   **
!               **          GIVEN (YOU CAN USE THE MULTIPLE     **
!               **          OPTION IN THAT CASE).               **
!               **************************************************
!
      IF(IREP2.EQ.'ON')THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        IF(NRESP.EQ.1)THEN
          CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,     &
                      INAME,IVARN1,IVARN2,IVARTY,            &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,      &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,           &
                      MAXCP4,MAXCP5,MAXCP6,                  &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,  &
                      Y1,X1,X2,X3,X4,X5,X6,NLOCAL,           &
                      IBUGG2,ISUBRO,IFOUND,IERROR)
        ELSE
          CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,     &
                      INAME,IVARN1,IVARN2,IVARTY,            &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,      &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,           &
                      MAXCP4,MAXCP5,MAXCP6,                  &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,  &
                      Y1,Y2,X1,X2,X3,X4,X5,NLOCAL,           &
                      IBUGG2,ISUBRO,IFOUND,IERROR)
        ENDIF
        IF(IERROR.EQ.'YES')GO TO 9000
!
!       FOR THIS CASE, THERE ARE EXACTLY TWO REPLICATION VARIABLES.
!       LOOP OVER THE DISTINCT VALUES IN THE SECOND REPLICATION
!       VARIABLE.
!
        CALL DISTIN(X1,NLOCAL,IWRITE,XTEMP3,NUMSE1,IBUGG2,IERROR)
        CALL DISTIN(X2,NLOCAL,IWRITE,XTEMP3,NUMSE2,IBUGG2,IERROR)
        CALL SORT(XTEMP3,NUMSE2,XTEMP3)
        CALL CODE(XTEMP3,NUMSE2,IWRITE,XTEMP4,XTEMP5,MAXOBV,   &
                  IBUGG2,IERROR)
        NREPI1=NUMSE1
        NREPI2=NUMSE2
!
!       RESTRICT SECOND REPLICATION VARIABLE TO A MAXIMUM OF
!       10 DISTINCT VALUE.
!
        IF(NUMSE2.GT.10)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7006)
 7006     FORMAT('      FOR THE 3-VARIABLE REPLICATION CASE, THE ',   &
                 'NUMBER OF REPLICATIONS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7007)
 7007     FORMAT('      FOR THE SECOND REPLICTATION VARIABLE ',   &
                 'IS GREATER THAN 10.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        IF(NUMSE2.EQ.1)THEN
          XSTRT=0.0
          XINC=0.4
        ELSEIF(NUMSE2.EQ.2)THEN
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
        DO 7001 K=1,NUMSE2
          ATEMP=XTEMP3(K)
          ICNT=0
          XFACT=XSTRT + (K-1)*XINC
          DO 7003 L=1,NLOCAL
            IF(ATEMP.EQ.X2(L))THEN
              ICNT=ICNT+1
              XTEMP5(ICNT)=Y1(L)
              TEMPZ(ICNT)=Y2(L)
              XTEMP6(ICNT)=X1(L) + XFACT
            ENDIF
 7003     CONTINUE
          NUMV2=NRESP+1
          IF(K.EQ.1)JD=0
          CALL DPI2(XTEMP5,XTEMP6,TEMPZ,XTEMP6,ICNT,ICNT,NUMV2,      &
                    ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,              &
                    ITOLGC,ITOLM2,PTOLDF,IIPLJI,                     &
                    ISIZE,ICONT,IQUAME,IQUASE,IRATME,MAXOBV,ISEED,   &
                    ALPHA,GAMMA,P1,P2,P100,NNEW,JD,                  &
                    XIDTEM,XIDTE2,TEMP,XTEMP4,TEMP1,TEMP2,           &
                    Y,X,D,NPLOTP,NPLOTV,IBUGG2,ISUBRO,IERROR)
 7001   CONTINUE
!
      ELSEIF(IMULT.EQ.'OFF')THEN
        ISTEPN='7B'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        IF(NRESP.EQ.1)THEN
          CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,      &
                      INAME,IVARN1,IVARN2,IVARTY,             &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,       &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,            &
                      MAXCP4,MAXCP5,MAXCP6,                   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y1,X1,X2,X3,X4,X5,X6,NLOCAL,            &
                      IBUGG2,ISUBRO,IFOUND,IERROR)
        ELSE
!
!         2018/06: HANDLE 4-VARIABLE CASE FOR DIFFERENCE OF MEANS
!                  AND DIFFERENCE OF PROPORTIONS SEPARATELY.
!
          IF(NUMVAR.EQ.4 .AND.   &
            (ICASPL.EQ.'DMEA' .OR. ICASPL.EQ.'DPRO'))THEN
!
            ICOL=1
            NUMVA2=2
            CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,    &
                        INAME,IVARN1,IVARN2,IVARTY,           &
                        ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,     &
                        MAXCOL,MAXCP1,MAXCP2,MAXCP3,          &
                        MAXCP4,MAXCP5,MAXCP6,                 &
                        V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO, &
                        Y1,X1,X1,X1,X1,X1,X1,NLOCAL,   &
                        IBUGG2,ISUBRO,IFOUND,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
!
            ICOL=3
            NUMVA2=2
            CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,    &
                        INAME,IVARN1,IVARN2,IVARTY,           &
                        ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,     &
                        MAXCOL,MAXCP1,MAXCP2,MAXCP3,          &
                        MAXCP4,MAXCP5,MAXCP6,                 &
                        V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO, &
                        Y2,X2,X2,X2,X2,X2,X2,NLOCA2,          &
                        IBUGG2,ISUBRO,IFOUND,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
!
            JD=0
            NPLOTP=0
            CALL DPI2(Y1,X1,Y2,X2,NLOCAL,NLOCA2,NUMVAR,                &
                      ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,              &
                      ITOLGC,ITOLM2,PTOLDF,IIPLJI,                     &
                      ISIZE,ICONT,IQUAME,IQUASE,IRATME,MAXOBV,ISEED,   &
                      ALPHA,GAMMA,P1,P2,P100,NNEW,JD,                  &
                      XIDTEM,XIDTE2,TEMP,TEMPZ,TEMP1,TEMP2,            &
                      Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
            GO TO 9000
          ENDIF
!
          CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,      &
                      INAME,IVARN1,IVARN2,IVARTY,             &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,       &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,            &
                      MAXCP4,MAXCP5,MAXCP6,                   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y1,Y2,X1,X2,X3,X4,X5,NLOCAL,            &
                      IBUGG2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          IF(NREPL.EQ.6)THEN
            ICOL=8
            NUMVA2=1
            CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,      &
                        INAME,IVARN1,IVARN2,IVARTY,             &
                        ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,       &
                        MAXCOL,MAXCP1,MAXCP2,MAXCP3,            &
                        MAXCP4,MAXCP5,MAXCP6,                   &
                        V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                        X6,X6,X6,X6,X6,X6,X6,NLOCA2,            &
                        IBUGG2,ISUBRO,IFOUND,IERROR)
          ENDIF
        ENDIF
        IF(IERROR.EQ.'YES')GO TO 9000
!
!       IF THERE ARE TWO OR MORE REPLICATION VARIABLES, COMBINE
!       THEM TO CREATE A SINGLE REPLICATION VARIABLE.
!
        IF(NUMVAR.EQ.NRESP+2)THEN
          CALL CODCT2(X1,X2,NLOCAL,ICCTOF,ICCTG1,IWRITE,   &
                      XTEMP0,XTEMP1,XTEMP2,                &
                      IBUGG2,ISUBRO,IERROR)
          DO 7011 I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7011     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.NRESP+3)THEN
          CALL CODCT3(X1,X2,X3,NLOCAL,ICCTOF,ICCTG1,ICCTG2,IWRITE,   &
                      XTEMP0,XTEMP1,XTEMP2,XTEMP3,                   &
                      IBUGG2,ISUBRO,IERROR)
          DO 7012 I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7012     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.NRESP+4)THEN
          CALL CODCT4(X1,X2,X3,X4,NLOCAL,                   &
                      ICCTOF,ICCTG1,ICCTG2,ICCTG3,IWRITE,   &
                      XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                      IBUGG2,ISUBRO,IERROR)
          DO 7013 I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7013     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.NRESP+5)THEN
          CALL CODCT5(X1,X2,X3,X4,X5,NLOCAL,                       &
                      ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,IWRITE,   &
                      XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                      IBUGG2,ISUBRO,IERROR)
          DO 7014 I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7014     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.NRESP+6)THEN
          CALL CODCT6(X1,X2,X3,X4,X5,X6,NLOCAL,                           &
                      ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,ICCTG5,IWRITE,   &
                      XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,XTEMP6,   &
                      IBUGG2,ISUBRO,IERROR)
          DO 7015 I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7015     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.LE.1)THEN
          IH='NI  '
          IH2='    '
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,                                      &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'YES')THEN
            ISIZE=NLOCAL
          ELSE
            ISIZE=INT(VALUE(ILOCP)+0.5)
          ENDIF
!
        ENDIF
!
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')THEN
          ISTEPN='7C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,791)NLOCAL
  791     FORMAT('DPI2 AFTER FORM REPLICATION VARIABLES: NLOCAL = ',I8)
          CALL DPWRST('XXX','BUG ')
          DO 793 I=1,NLOCAL
            WRITE(ICOUT,795)I,Y1(I),X1(I)
  795       FORMAT('I,Y1(I),X1(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
  793     CONTINUE
        ENDIF
!
!               *********************************************************
!               **  STEP 7B--                                         **
!               **  GENERATE THE I PLOT.                              **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).     **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).     **
!               *********************************************************
!
        JD=0
        NPLOTP=0
        CALL DPI2(Y1,X1,Y2,X1,NLOCAL,NLOCAL,NUMVAR,                &
                  ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,              &
                  ITOLGC,ITOLM2,PTOLDF,IIPLJI,                     &
                  ISIZE,ICONT,IQUAME,IQUASE,IRATME,MAXOBV,ISEED,   &
                  ALPHA,GAMMA,P1,P2,P100,NNEW,JD,                  &
                  XIDTEM,XIDTE2,TEMP,TEMPZ,TEMP1,TEMP2,            &
                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               ***********************************************
!               **  STEP 8A--                                **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES.     **
!               **          THESE CAN BE EITHER VARIABLE OR  **
!               **          MATRIX ARGUMENTS.                **
!               ***********************************************
!
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES.  NOTE THAT IN
!       THIS CASE, WE ARE ULTIMATELY CREATING A "Y X" SYNTAX, SO THE
!       LOOP IS MERELY ADDING A NEW GROUP.  NEED TO BE CAREFUL THAT
!       COMBINED DATA DOES NOT EXCEED MAXIMUM POINTS FOR AN ARRAY.
!
!       2016/07: ALLOW Y1 AND X1 TO HAVE DIMENSION 5*MAXOBV.  THIS
!                CAN BE HELPFUL FOR LARGER DATA SETS.
!
!       2017/11: NOTE THAT THIS CASE NOT SUPPORTED FOR THE CASES
!                WHERE THERE ARE TWO RESPONSE VARIABLES.  THIS IS
!                CHECKED FOR ABOVE.
!
        NPLOTP=0
        DO 810 IRESP=1,NRESP
          NCURVE=IRESP
!
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPI')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,811)IRESP,NCURVE
  811       FORMAT('IRESP,NCURVE = ',2I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          ICOL=IRESP
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,                 &
                      INAME,IVARN1,IVARN2,IVARTY,                        &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,                  &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,                       &
                      MAXCP4,MAXCP5,MAXCP6,                              &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,              &
                      XTEMP1,XTEMP2,XTEMP3,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGG2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          DO 815 JJ=1,NLOCAL
            NPLOTP=NPLOTP+1
!CCCC       IF(NPLOTP.GT.MAXOBV)THEN
            IF(NPLOTP.GT.5*MAXOBV)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,816)
  816         FORMAT('      FOR THE MULTIPLE CASE, THE MAXIMUM NUMBER ',   &
                     'OF POINTS HAS BEEN EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
            Y1(NPLOTP)=XTEMP1(JJ)
            X1(NPLOTP)=REAL(NCURVE)
  815     CONTINUE
!
  810   CONTINUE
        NLOCAL=NPLOTP
        NUMVAR=2
!
!               *****************************************************
!               **  STEP 8B--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!               *****************************************************
!
        NPLOTP=0
        JD=0
        CALL DPI2(Y1,X1,Y2,X1,NLOCAL,NLOCAL,NUMVAR,                &
                  ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,              &
                  ITOLGC,ITOLM2,PTOLDF,IIPLJI,                     &
                  ISIZE,ICONT,IQUAME,IQUASE,IRATME,MAXOBV,ISEED,   &
                  ALPHA,GAMMA,P1,P2,P100,NNEW,JD,                  &
                  XIDTEM,XIDTE2,TEMP,TEMPZ,TEMP1,TEMP2,            &
                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9013   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',   &
               I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ISIZE,NUMVAR
 9014   FORMAT('ISIZE,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO 9015 I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPI
      SUBROUTINE DPI2(Y,X,YZ,XZ,N,NZ,NUMV2,   &
                      ICASPL,IBINME,IBI2ME,ICVACI,PSTAMV,   &
                      ITOLGC,ITOLM2,PTOLDF,IIPLJI,   &
                      ISIZE,ICONT,IQUAME,IQUASE,IRATME,MAXNXT,ISEED,   &
                      ALPHA,GAMMA,P1,P2,P100,NNEW,JD,   &
                      XIDTEM,XIDTE2,TEMP,TEMPZ,XTEMP1,XTEMP2,   &
                      Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN I PLOT
!              OF THE FOLLOWING TYPES--
!                 1) (MEDIAN) I  PLOT;
!                 2) MEAN I  PLOT;
!                 3) MIDRANGE I  PLOT;
!                 4) MIDMEAN I  PLOT;
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--FEBRUARY  1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --FEBRUARY  2011. ISUBRO ARGUMENT
!     UPDATED         --OCTOBER   2013. ADD SUPPORT FOR NEW PLOT OPTIONS
!     UPDATED         --NOVEMBER  2017. UPDATES TO AGRESTI-COUL
!                                       CONFIDENCE LIMTIS
!     UPDATED         --NOVEMBER  2017. DIFFERENCE OF PROPORTIONS
!     UPDATED         --NOVEMBER  2017. COEFFICIENT OF VARIATION
!     UPDATED         --NOVEMBER  2017. COEFFICIENT OF DISPERSION
!     UPDATED         --DECEMBER  2017. COEFFICIENT OF QUARTILE DISPERSION
!     UPDATED         --APRIL     2018. CORRELATION
!     UPDATED         --JUNE      2018. SUPPORT UNEQUAL SAMPLE SIZES FOR:
!                                          DIFFERENCE OF MEAN
!                                          DIFFERENCE OF PROPORTIONS
!     UPDATED         --OCTOBER   2019. RATIO OF MEANS
!     UPDATED         --JANUARY   2020. SUPPORT JITTER
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICVACI
      CHARACTER*4 ICONT
      CHARACTER*4 IQUAME
      CHARACTER*4 IRATME
      CHARACTER*4 IQUASE
      CHARACTER*4 ITOLGC
      CHARACTER*4 ITOLM2
      CHARACTER*4 IIPLJI
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
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
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION YZ(*)
      DIMENSION X(*)
      DIMENSION XZ(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
!
      DIMENSION ALPHAT(1)
      DIMENSION ALOWLV(1)
      DIMENSION AUPPLV(1)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPI2'
      ISUBN2='    '
      IWRITE='OFF'
!
      I2=0
      AN=0.0
      N50=1
      NRESP=1
!
!     2018/06: DIFFERENCE OF MEAN AND DIFFERENCE OF PROPORTION SUPPORT
!              A 4 VARIABLE CASE (Y1 X1 Y2 X2) FOR UNEQUAL SAMPLE
!              SIZES.
!
      IFLAGD=0
      IF(ICASPL.EQ.'DPRO' .OR. ICASPL.EQ.'DMEA' .OR.   &
         ICASPL.EQ.'CRCL' .OR. ICASPL.EQ.'RMEA')THEN
        NRESP=2
        IF(ICASPL.EQ.'DPRO' .OR. ICASPL.EQ.'DMEA')THEN
          IF(NUMV2.EQ.4)IFLAGD=1
        ENDIF
      ENDIF
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
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
        GO TO 9000
      ENDIF
!
      IF(IFLAGD.EQ.1 .AND. NZ.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,42)
   42   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',   &
               'RESPONSE VARIABLE MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)NZ
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     DON'T TREAT ALL RESPONSE VALUES EQUAL AS AN ERROR
!
!CCCC HOLD=Y(1)
!CCCC DO60I=1,N
!CCCC   IF(Y(I).NE.HOLD)GO TO 69
!CC60 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,31)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,62)
!CC62 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,63)HOLD
!CC63 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!CC69 CONTINUE
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DPI2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ICONT,ICASPL,PSTAMV
   71   FORMAT('ICONT,ICASPL,PSTAMV = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,77)N,NZ,NRESP,NUMV2,ISIZE,IIPLJI
   77   FORMAT('N,NZ,NRESP,NUMV2,ISIZE,IIPLJI = ',5I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 72 I=1,MAX(N,NZ)
          WRITE(ICOUT,73)I,Y(I),X(I),YZ(I),XZ(I)
   73     FORMAT('I,Y(I),X(I),YZ(I),XZ(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
      ENDIF
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
!               **  FOR VARIABLE 2 (THE GROUP VARIABLE).              **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
!               **  WHICH IS AN ERROR CONDITION FOR AN I PLOT .       **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMV2.EQ.NRESP)THEN
        NUMSET=0
        DO 120 I=ISIZE,N,ISIZE
          I2=I
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=NUMSET
  120   CONTINUE
        IF(I2.LT.N)THEN
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=NUMSET
        ENDIF
        DO 145 I=1,N
          IGROUP=1+((I-1)/ISIZE)
          IMID=(IGROUP-1)*ISIZE+(ISIZE/2)
          X(I)=IMID
  145   CONTINUE
!
      ELSEIF(IFLAGD.EQ.1)THEN
!
        CALL DISTIN(X,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
        XID1=XIDTEM(1)
        XID2=XIDTEM(NUMSET)
        CALL DISTIN(XZ,NZ,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
!
!       CURRENTLY ONLY SUPPORT CASE WHERE THE GROUPS ARE THE SAME
!
        IF(NUMSET.EQ.NUMSE2)THEN
          DO 155 II=1,NUMSET
            IF(XIDTEM(II).NE.XIDTE2(II))THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,31)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,156)
  156         FORMAT('      THE GROUP-ID VARIABLES DO NOT HAVE THE ',   &
                     'SAME VALUES.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
  155     CONTINUE
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,157)
  157     FORMAT('      THE GROUP-ID VARIABLES DO NOT HAVE THE ',   &
                     'SAME NUMBER OF ELEMENTS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,158)NUMSET
  158     FORMAT('      GROUP-ID VARIABLE ONE HAS ',I8,' UNIQUE ',   &
                     'ELEMENTS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,159)NUMSE2
  159     FORMAT('      GROUP-ID VARIABLE TWO HAS ',I8,' UNIQUE ',   &
                     'ELEMENTS.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSEIF(NUMV2.EQ.NRESP+1)THEN
        CALL DISTIN(X,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
        XID1=XIDTEM(1)
        XID2=XIDTEM(NUMSET)
      ENDIF
!
      IF(NUMSET.EQ.0)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,191)
  191   FORMAT('      NUMSET = 0')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
!
      ELSEIF(NUMSET.EQ.N)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      NUMSET = N')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************************
!               **  STEP 4--                                          **
!               **  IN ORDER TO DETERMINE THE PROPER PLOT COOORDINATES**
!               **  FOR THE DESIRED PLOT,                             **
!               **  FIRST BRANCH TO THE PROPER SUBCASE--              **
!               **         1) (MEDIAN) I  PLOT;                       **
!               **         2) MEAN I  PLOT;                           **
!               ********************************************************
!
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ***************************************************
!               **  STEP 4A--                                    **
!               **  DETERMINE PLOT COORDINATES FOR 4 SUBCASES--  **
!               **      1) (MEDIAN) I  PLOT;                     **
!               **      2) MEAN I  PLOT;                         **
!               **      3) MIDRANGE I  PLOT;                     **
!               **      4) MIDMEAN I  PLOT;                      **
!               ***************************************************
!
      ISTEPN='4A'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      AN=N
      ANUMSE=NUMSET
!
!     2018/06: CHECK FOR "MISSING DATA" VALUES
!
      NUMCPL=11
      J=N2
      DO 1110 ISET=1,NUMSET
!
        K=0
        K2=0
        IF(IFLAGD.EQ.0)THEN
          DO 1120 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
!
!             NOTE: CORRELATION DATA VALUES MUST BE PAIRED, SO ELIMINATE
!                   ANY ROWS WHERE EITHER DATA VALUE IS MISSING
!
              IF(ICASPL.EQ.'CORR' .OR. ICASPL.EQ.'RMEA')THEN
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
          DO 1130 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              IF(Y(I).NE.PSTAMV)THEN
                K=K+1
                TEMP(K)=Y(I)
              ENDIF
            ENDIF
 1130     CONTINUE
          DO 1135 I=1,NZ
            IF(XZ(I).EQ.XIDTEM(ISET))THEN
              IF(YZ(I).NE.PSTAMV)THEN
                K2=K2+1
                TEMPZ(K2)=YZ(I)
              ENDIF
            ENDIF
 1135     CONTINUE
        ENDIF
!
        NI=K
        ANI=NI
        NI2=K2
        ANI2=NI2
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DPI2')THEN
          WRITE(ICOUT,1121)ISET,XIDTEM(ISET),NI,NI2
 1121     FORMAT('ISET,XIDTEM(ISET),NI,NI2 = ',I8,G15.7,2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
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
          GO TO 9000
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
          GO TO 9000
        ENDIF
!
        XMID=XIDTEM(ISET)
!
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
            CALL MEDIAN(TEMP,NI,IWRITE,XTEMP1,MAXNXT,XMED,   &
                        IBUGG3,IERROR)
            Y50=XMED
          ELSE
            CALL QUANT(P100,TEMP,NI,IWRITE,XTEMP1,MAXNXT,IQUAME,XQUANT,   &
                       IBUGG3,IERROR)
            Y50=XQUANT
          ENDIF
          CALL QUANSE(P100,TEMP,NI,IWRITE,XTEMP1,MAXNXT,IQUASE,XQUASE,   &
                      IBUGG3,IERROR)
          CDF=1.0 - (ALPHA/2.0)
          CALL NORCDF(CDF,TCVAL)
          YMAX=Y50 + TCVAL*XQUASE
          YMIN=Y50 - TCVAL*XQUASE
          print *,'mdcl: y50,ymin,ymax = ',y50,ymin,ymax
        ELSEIF(ICASPL.EQ.'TMCL' .OR. ICASPL.EQ.'TMIP')THEN
          NTRIM1=-1
          NTRIM2=-1
          CALL TRIMME(TEMP,NI,P1,P2,NTRIM1,NTRIM2,IWRITE,XTEMP1,   &
                      MAXNXT,Y50,IBUGG3,ISUBRO,IERROR)
          IF(ICASPL.EQ.'TMCL')THEN
            CALL TRIMSE(TEMP,NI,P1,P2,NRIM1,NTRIM2,IWRITE,XTEMP1,XTEMP2,   &
                        MAXNXT,YSTERR,IBUGG3,ISUBRO,IERROR)
!
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
          CALL BIWLOC(TEMP,NI,IWRITE,XTEMP1,XTEMP2,MAXNXT,Y50,   &
                      IBUGG3,IERROR)
          IF(ICASPL.EQ.'BWCL')THEN
            CALL BIWSCA(TEMP,NI,IWRITE,XTEMP1,XTEMP2,MAXNXT,YBSC,   &
                        IBUGG3,IERROR)
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
          CALL DPTOL3(TEMP,NI,XMEAN,XSD,AN,PTOLDF,   &
                      ICASAN,ALPHA,GAMMA,ITOLGC,ITOLM2,   &
                      AK,ALOWLM,AUPPLM,   &
                      ISUBRO,IBUGG3,IERROR)
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
          CALL DPPRL3(TEMP,NI,NNEW,ICASA2,ICASA3,ICASA4,ICASA5,   &
                      YMEAN,YSD,   &
                      ALPHAT,NALPHA,ALOWLV,AUPPLV,   &
                      ISUBRO,IBUGG3,IERROR)
          Y50=YMEAN
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
!
        ELSEIF(ICASPL.EQ.'SDCL')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          ICASA2='LIMI'
          ICASA3='UPPE'
          ICASA4='RAW '
          ICASA5='TWOS'
          CALL MEAN(TEMP,NI,IWRITE,YMEAN,IBUGG3,IERROR)
          CALL DPSDC3(TEMP,NI,ICASA2,ICASA3,ICASA4,ICASA5,   &
                      YSD,   &
                      ALPHAT,NALPHA,ALOWLV,AUPPLV,   &
                      ISUBRO,IBUGG3,IERROR)
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
          CALL DPCVC3(TEMP,NI,YMEAN,YSD,   &
                      ICASA2,ICASA3,ICASA4,ICASA5,   &
                      ISEED,MAXNXT,IDIST,   &
                      XTEMP1,XTEMP2,   &
                      ICVACI,ALPHAT,NALPHA,ALOWLV,AUPPLV,YCV,   &
                      ISUBRO,IBUGG3,IERROR)
          Y50=YCV
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
        ELSEIF(ICASPL.EQ.'CDCL')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          ICASA3='UPPE'
          ICASA5='TWOS'
          CALL DPCDC3(TEMP,NI,ICASA3,ICASA5,ISEED,MAXNXT,   &
                      XTEMP1,ALPHAT,NALPHA,ALOWLV,AUPPLV,   &
                      YCD,YMED,YAAD,   &
                      ISUBRO,IBUGG3,IERROR)
          Y50=YCD
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
        ELSEIF(ICASPL.EQ.'CRCL')THEN
          CALL CORR(TEMP,TEMPZ,NI,IWRITE,Y50,IBUGG3,IERROR)
          CALL DPCRC3(Y50,NI,ALPHA,U,Z,YMIN,YMAX,   &
                      IBUGG3,ISUBRO,IERROR)
        ELSEIF(ICASPL.EQ.'RMEA')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          IF(IRATME.EQ.'FIEL')THEN
            CALL DPMRC3(TEMP,TEMPZ,NI,ALPHAT,NALPHA,   &
                        RATIO,ALOWLV,AUPPLV,   &
                        YBAR,XBAR,YVAR,XVAR,   &
                        ISUBRO,IBUGG3,IERROR)
          ELSEIF(IRATME.EQ.'LSAM')THEN
            CALL DPMRC4(TEMP,TEMPZ,NI,ALPHAT,NALPHA,   &
                        RATIO,ALOWLV,AUPPLV,   &
                        YBAR,XBAR,YVAR,XVAR,XYCOV,   &
                        ISUBRO,IBUGG3,IERROR)
          ELSEIF(IRATME.EQ.'LRAT')THEN
            CALL DPMRC5(TEMP,TEMPZ,NI,ALPHAT,NALPHA,   &
                        RATIO,ALOWLV,AUPPLV,   &
                        YBAR,XBAR,YVAR,XVAR,XYCOV,   &
                        ISUBRO,IBUGG3,IERROR)
          ENDIF
          Y50=RATIO
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
        ELSEIF(ICASPL.EQ.'CQCL')THEN
          ALPHAT(1)=ALPHA
          NALPHA=1
          ICASA3='UPPE'
          ICASA5='TWOS'
          CALL DPCQD3(TEMP,NI,ICASA3,ICASA5,ISEED,MAXNXT,IQUAME,   &
                      XTEMP1,ALPHAT,NALPHA,ALOWLV,AUPPLV,   &
                      CQV,Q1,Q3,   &
                      ISUBRO,IBUGG3,IERROR)
          Y50=CQV
          YMAX=AUPPLV(1)
          YMIN=ALOWLV(1)
       ELSEIF(ICASPL.EQ.'AGCL')THEN
!
!         11/2017: USE DPPRC3 IN ORDER TO ACCOMODATE MULTIPLE METHODS
!                  FOR COMPUTING PROOPORTION CONFIDENCE LIMITS.
!
!CCCC     ISUCC=0
!CCCC     DO1126II=1,NI
!CCCC       IF(TEMP(II).GE.0.5 .AND. TEMP(II).LE.1.5)THEN
!CCCC         ISUCC=ISUCC+1
!CCCC       ENDIF
!1126     CONTINUE
!CCCC     Y50=REAL(ISUCC)/REAL(NI)
!CCCC     CALL DPAGCO(Y50,NI,ALPHA,IWRITE,YMIN,YMAX,IBUGG3,IERROR)
          CALL DPPRC3(TEMP,NI,ALPHA,PSTAMV,IBINME,XTEMP1,   &
                      Y50,YMIN,YMAX,   &
                      ISUBRO,IBUGG3,IERROR)
       ELSEIF(ICASPL.EQ.'DPRO')THEN
          CALL DPPRC4(TEMP,NI,TEMPZ,NI2,ALPHA,PSTAMV,IBI2ME,XTEMP1,   &
                      Y50,YMIN,YMAX,   &
                      ISUBRO,IBUGG3,IERROR)
        ELSEIF(ICASPL.EQ.'MDIP' .OR. ICASPL.EQ.'MEIP' .OR.   &
           ICASPL.EQ.'MRIP' .OR. ICASPL.EQ.'MMIP')THEN
!
          CALL SORT(TEMP,NI,TEMP)
!
!               ***************************
!               **  STEP 4.1--           **
!               **  COMPUTE THE MAXIMUM  **
!               ***************************
!
          YMAX=TEMP(NI)
!
!               *********************************
!               **  STEP 4.2--                 **
!               **  COMPUTE THE TYPICAL VALUE  **
!               **  (MEDIAN, MEAN,             **
!               **  MIDRANGE, OR TRIMMED MEAN) **
!               *********************************
!
          IF(ICASPL.EQ.'MDIP')THEN
            N50=NI/2
            N50P1=N50+1
            IEVODD=NI-2*(NI/2)
            IF(IEVODD.EQ.0)Y50=(TEMP(N50)+TEMP(N50P1))/2.0
            IF(IEVODD.EQ.1)Y50=TEMP(N50P1)
          ELSEIF(ICASPL.EQ.'MEIP')THEN
            SUM=0.0
            DO 1134 I=1,NI
              SUM=SUM+TEMP(I)
 1134       CONTINUE
            Y50=SUM/ANI
          ELSEIF(ICASPL.EQ.'MRIP')THEN
            Y50=(TEMP(1)+TEMP(NI))/2.0
          ELSEIF(ICASPL.EQ.'MMIP')THEN
            NP1=INT(P1*ANI+0.0001)
            NP2=INT(P2*ANI+0.0001)
            IMIN=NP1+1
            IMAX=N-NP2
            IF(IMIN.LT.1)IMIN=1
            IF(IMAX.GT.NI)IMAX=NI
            IF(IMIN.GT.IMAX)IMIN=IMAX
            Y50=TEMP(1)
            SUM=0.0
            L=0
            DO 1138 I=IMIN,IMAX
              L=L+1
              SUM=SUM+TEMP(I)
 1138       CONTINUE
            AL=L
            Y50=SUM/AL
          ENDIF
!
          NP1=INT(P1*ANI+0.0001)
          NP2=INT(P2*ANI+0.0001)
!
!               ***************************
!               **  STEP 4.3--           **
!               **  COMPUTE THE MINIMUM  **
!               ***************************
!
          YMIN=TEMP(1)
!
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
          GO TO 9000
        ENDIF
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DPI2')THEN
           WRITE(ICOUT,1151)YMIN,Y50,YMAX,ISET,K,TEMP(K)
 1151      FORMAT('YMIN,Y50,YMAX,ISET,K,TEMP(K) = ',3G15.7,2I8,G15.7)
           CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ********************************************
!               **  STEP 4.11--                           **
!               **  DEFINE THE CHARACTER AT THE MAXIMUM;  **
!               ********************************************
!
        CALL DPCHLI(ICONT,NUMCPL,YMAX,YMAX,XMID,XMID,J,JD,Y2,X2,D2,   &
                    IERROR)
!               ***************************************
!               **  STEP 4.12--                      **
!               **  DEFINE THE CHARACTER             **
!               **  FOR THE TYPICAL VALUE            **
!               **  (SUCH AS THE MEDIAN OR MEAN)     **
!               ***************************************
!
        CALL DPCHLI(ICONT,NUMCPL,Y50,Y50,XMID,XMID,J,JD,Y2,X2,D2,IERROR)
!
!               ********************************************
!               **  STEP 4.13--                           **
!               **  DEFINE THE CHARACTER AT THE MINIMUM.  **
!               ********************************************
!
        CALL DPCHLI(ICONT,NUMCPL,YMIN,YMIN,XMID,XMID,J,JD,Y2,X2,D2,   &
                    IERROR)
!
!               *************************************
!               **  STEP 4.14--                    **
!               **  DEFINE THE VERTICAL LINE FROM  **
!               **  THE MAX TO THE TYPICAL VALUE   **
!               *************************************
!
        CALL DPCHLI(ICONT,NUMCPL,YMAX,Y50,XMID,XMID,J,JD,Y2,X2,D2,   &
                    IERROR)
!
!               **********************************
!               **  STEP 4.15--                 **
!               **  DEFINE THE VERTICAL LINE    **
!               **  FROM THE TYPICAL VALUE      **
!               **  TO THE MIN                  **
!               **********************************
!
        CALL DPCHLI(ICONT,NUMCPL,Y50,YMIN,XMID,XMID,J,JD,Y2,X2,D2,   &
                    IERROR)
!
 1110 CONTINUE
!
      N2=J
      NPLOTV=3
      GO TO 9000
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPI2
      SUBROUTINE DPICHA(ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--RETURN THE COLLATING SEQUENCE NUMBER (USUALLY THE
!              ASCII INDEX NUMBER) IN A PREVIOUSLY DEFINED STRING.
!              THIS IS ESSENTIALLY EQUIVALENT TO USING THE
!              ICHAR FUNCTION IN FORTRAN.  NOTE THAT ALL MAJOR
!              PLATFORMS CURRENTLY USE THE ASCII COLLATING SEQUENCE,
!              BUT A FEWER OLDER PLATFORMS DO NOT (E.G., EBCDIC
!              ON SOME IBM).
!     EXAMPLE--LET IVAL = ICHAR S
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/11
!     ORIGINAL VERSION--NOVEMBER  2008.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEL
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*8 ISTR
!
!---------------------------------------------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOZI.INC'
!
      INTEGER ITEMP1(MAXOBV)
!
      EQUIVALENCE(ITEMP1(1),IGARBG(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIC'
      ISUBN2='HA  '
      IERROR='NO'
!
      ILOC3=0
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               *****************************************************
!               **  TREAT THE SUBCASE OF THE LET FUNCTION COMMAND  **
!               **  WHICH DEFINES A FUNCTION                       **
!               *****************************************************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ICHA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPICHA--')
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
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='NO'
      NEWCOL='NO'
      ICASEL='UNKN'
      NIOLD=0
      ICOLL=0
      ICOL2=0
!
!               ******************************************************
!               **  STEP 2--                                         *
!               **  EXAMINE THE LEFT-HAND SIDE--                     *
!               **  IF THIS IS A PREVIOUSLY DEFINED NAME, IT SHOULD  *
!               **  BE EITHER A PARAMETER OR A VARIABLE.             *
!               ******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
!
      DO 1910 I=1,4
        IF(IHLEFT(I:I).EQ.'(')THEN
          IHLEFT(I:4)=' '
          IHLEF2=' '
          ICASEL='ELEM'
          GO TO 1999
        ENDIF
 1910 CONTINUE
      DO 1920 I=1,4
        IF(IHLEF2(I:I).EQ.'(')THEN
          IHLEF2(I:4)=' '
          ICASEL='ELEM'
          GO TO 1999
        ENDIF
 1920 CONTINUE
 1999 CONTINUE
!
      DO 2000 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I2).EQ.'P')THEN
            ICASEL='PARA'
            ILISTL=I2
            GO TO 2900
          ELSEIF(IUSE(I2).EQ.'V')THEN
            ICASEL='VARI'
            ILISTL=I2
            ICOLL=IVALUE(ILISTL)
            NIOLD=IN(ILISTL)
            GO TO 2900
          ELSE
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
 2001       FORMAT('***** ERROR IN ICHAR--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2003)IHLEFT,IHLEF2
 2003       FORMAT('      THE NAME ON THE LEFT HAND SIDE (',   &
                   A4,A4,')')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2005)
 2005       FORMAT('      ALREADY EXISTS, BUT NOT AS A PARAMETER ',   &
                   'OR A VARIABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
 2000 CONTINUE
!
      NEWNAM='YES'
!
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND ',   &
               'FUNCTION')
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
 2206   FORMAT('      THEN REDEFINE OR DELETE SOME OF THE ALREADY ',   &
               'USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
 2900 CONTINUE
!
!               *****************************************************
!               **  STEP 3--                                       **
!               **  EXTRACT THE NAME ON THE RIGHT HAND SIDE        **
!               *****************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHRIGH=IHARG(4)
      IHRIG2=IHARG2(4)
      DO 3000 I=1,NUMNAM
        I4=I
        IF(IHRIGH.EQ.IHNAME(I).AND.IHRIG2.EQ.IHNAM2(I))THEN
          IF(IUSE(I4).NE.'F')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3003)IHRIGH,IHRIG2
 3003       FORMAT('      THE NAME ON THE RIGHT HAND SIDE (',   &
                   A4,A4,')')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3005)
 3005       FORMAT('      ALREADY EXISTS, BUT NOT AS A STRING.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ELSE
            ISTART=IVSTAR(I4)
            ISTOP=IVSTOP(I4)
            NLEN=ISTOP-ISTART+1
            DO 3010 J=1,NLEN
              IINDX=ISTART+J-1
              CALL DPCOAN(IFUNC(IINDX)(1:1),IVAL)
              ITEMP1(J)=IVAL
 3010       CONTINUE
            GO TO 3900
          ENDIF
        ENDIF
 3000 CONTINUE
!
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
      GO TO 9000
!
 3900 CONTINUE
!
!               *****************************************************
!               **  STEP 4--                                       **
!               **  SAVE PARAMETER                                 **
!               *****************************************************
!
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
!
      IF(NLEN.EQ.1)THEN
        IF(ICASEL.EQ.'UNKN')ICASEL='PARA'
      ELSEIF(NLEN.GT.1)THEN
        IF(ICASEL.EQ.'UNKN')ICASEL='VARI'
      ENDIF
!
      IF(ICASEL.EQ.'PARA')THEN
!
        ISTEPN='4A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='P'
        VALUE(ILISTL)=REAL(ITEMP1(1))
        IVALUE(ILISTL)=INT(VALUE(ILISTL)+0.5)
        IN(ILISTL)=1
        IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
!
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
!
        ISTEPN='4B'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(NEWNAM.EQ.'YES')THEN
          NUMNAM=NUMNAM+1
          NUMCOL=NUMCOL+1
          ICOLL=NUMCOL
        ENDIF
        DO 15200 I=1,NLEN
          RIGHT=REAL(ITEMP1(I))
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
!
15200   CONTINUE
!
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='V'
        IVALUE(ILISTL)=ICOLL
        VALUE(ILISTL)=ICOLL
        IN(ILISTL)=NLEN
!
!
        DO 15210 J4=1,NUMNAM
          IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)THEN
            IUSE(J4)='V'
            IVALUE(J4)=ICOLL
            VALUE(J4)=ICOLL
            IN(J4)=NLEN
            GO TO 15219
          ENDIF
15210   CONTINUE
15219   CONTINUE
!
        IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,15211)IHLEFT,IHLEF2,IHRIGH,IHRIG2
15211     FORMAT(A4,A4,' CONTAINS THE ASCII COLLATING SEQUENCE ',   &
                 'VALUES FOR ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(ICASEL.EQ.'ELEM')THEN
!
        ISTEPN='4C'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ICHA')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       SEARCH IANS STRING FOR "(xx) =".  IF NO PARENTHESIS
!       FOUND BEFORE "=", THEN DO NOT KNOW WHAT ROW OF THE
!       VARIABLE TO SAVE.  TREAT THIS AS AN ERROR.
!
        NLEFT=-1
        NRIGHT=-1
        NEQUAL=-1
        DO 16001 I=1,IWIDTH
          IF(IANS(I)(1:1).EQ.'(' .AND. NLEFT.LT.0)THEN
            NLEFT=I
          ELSEIF(IANS(I)(1:1).EQ.')' .AND. NRIGHT.LT.0)THEN
            NRIGHT=I
          ELSEIF(IANS(I)(1:1).EQ.'=' .AND. NEQUAL.LT.0)THEN
            NEQUAL=I
          ENDIF
16001   CONTINUE
!
!       NEED  NLEFT < NRIGHT < NEQUAL
!
        NSTRT=NLEFT+1
        NSTOP=NRIGHT-1
        NLEN=NSTOP-NSTRT+1
        IF(NLEFT.GT.NRIGHT .OR. NRIGHT.GT.NEQUAL .OR.   &
           NSTRT.GT.NSTOP .OR. NLEN.GT.8) THEN
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
          GO TO 9000
        ELSE
          ISTR=' '
          DO 16020 I=1,NLEN
            ISTR(I:I)=IANS(NSTRT+I-1)(1:1)
16020     CONTINUE
          READ(ISTR,'(I8)',ERR=16029)IARGL
          GO TO 16049
!
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
          GO TO 9000
!
16049     CONTINUE
        ENDIF
!
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
          GO TO 9000
        ENDIF
!
        IF(NEWNAM.EQ.'YES')THEN
          NIOLD=1
        ENDIF
        NINEW=NIOLD
        IF(IARGL.GT.NINEW)NINEW=IARGL
        NS2=1
!
        RIGHT=REAL(ITEMP1(1))
        IJ=MAXN*(ICOLL-1)+IARGL
        IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
        IF(ICOLL.EQ.MAXCP1)PRED(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP2)RES(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP3)YPLOT(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP4)XPLOT(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP5)X2PLOT(IARGL)=RIGHT
        IF(ICOLL.EQ.MAXCP6)TAGPLO(IARGL)=RIGHT
!
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='V'
        IVALUE(ILISTL)=ICOLL
        VALUE(ILISTL)=ICOLL
        IN(ILISTL)=NINEW
!
        IF(NEWNAM.EQ.'YES')THEN
          NUMNAM=NUMNAM+1
          NUMCOL=NUMCOL+1
        ENDIF
!
        DO 16200 J4=1,NUMNAM
          IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)THEN
            IUSE(J4)='V'
            IVALUE(J4)=ICOLL
            VALUE(J4)=ICOLL
            IN(J4)=NINEW
            GO TO 16209
          ENDIF
16200   CONTINUE
16209   CONTINUE
!
        IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,16211)IHRIGH,IHRIG2,ITEMP1(1)
16211     FORMAT('THE ASCII COLLATING SEQUENCE VALUE OF  ',A4,A4,   &
                 ' = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
      GO TO 9000
!
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
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
      END SUBROUTINE DPICHA
      SUBROUTINE DPICLA(ICOM,ICOM2,   &
                        IMACRO,IMACNU,IMACCS,   &
                        IMACL1,IMACL2,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                        IANSLC,IWIDTH,   &
                        IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                        IBUGS2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--WHEN THE CALL COMMAND IS ENTERED INSIDE OF A LOOP,
!              THE CALL COMMAND IS NOT INCLUDED IN THE SAVED LOOP
!              LINES.  RATHER THE CONTENTS OF THE CALL FILE ARE
!              SAVED.  ONE SIDE EFFECT OF THIS IS THAT ARGUMENTS
!              TO THE COMMAND LINE ARE NOT SAVED.
!
!              THIS COMMAND IMPLEMENTS THE COMMAND
!
!                   INSERT CALL ARGUMENTS <STRING>
!
!              WHERE <STRING> CONTAINS THE COMMAND LINE ARGUMENTS.
!
!              WHEN A CALL COMMAND IS EXECUTED IN LOOP STORE MODE AND
!              THE CALL COMMAND HAS ARGUMENTS, AN "INSERT CALL
!              ARGUMENTS" COMMAND WILL BE ENTERED INTO THE SAVED LOOP
!              LINES (THIS IS DONE IN THE "DPMACR" ROUTINE).  THIS
!              COMMAND THEN IMPLEMENTS THAT COMMAND WHEN THE LOOP LINES
!              ARE EXECUTED.
!
!     INPUT  ARGUMENTS--ICOM
!                     --ICOM2
!     INPUT  ARGUMENTS--IMACNU (AN INTEGER VALUE
!                              BY WHICH THE MACRO FILE/SUBFILE MAY BE
!                              REFERENCED IN A FORTRAN I/O STATEMENT.
!                     --IMACCS (A HOLLERITH VARIABLE CONTAINING STATUS
!                              INFORMATION FOR THE MACRO FILE/SUBFILE
!                     --IANSLC (A  HOLLERITH VECTOR WHOSE I-TH ELEMENT
!                              CONTAINS THE I-TH CHARACTER OF THE
!                              ORIGINAL INPUT COMMAND LINE.
!                     --IWIDTH (AN INTEGER VARIABLE WHICH CONTAINS THE
!                              NUMBER OF CHARACTERS IN THE ORIGINAL
!                              COMMAND LINE.
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IBUG   (A HOLLERITH VARIABLE FOR DEBUGGING
!     OUTPUT ARGUMENTS--IMACRO (AN INTEGER VARIABLE WHICH IF 'ON'
!                              INDICATES THAT CURRENT COMMANDS ARE ALSO
!                              BEING DIVERTED SO AS TO CONSTRUCT A MACRO;
!                              AND IF OFF INDICATES THAT A MACRO IS NOT
!                              BEING CONSTRUCTED.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 Gaithersburg, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2017/07
!     ORIGINAL VERSION--JULY      2017.
!     UPDATED         --MAY       2018. UPDATED TO SUPPORT NEW FORMS OF
!                                       PASSING ARGUMENTS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      INCLUDE 'DPCOPA.INC'
!
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
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFILQZ
      CHARACTER*4 IHYPS2
      CHARACTER (LEN=MAXSTR) :: ICANS
      CHARACTER (LEN=MAXSTR) :: ISTR
!
! ---------------------------------------------------------------------
!
      DIMENSION IANSLC(*)
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
!
!-----COMMON----------------------------------------------------------
!
      CHARACTER (LEN=MAXFNC) :: IMANAM(10)
      COMMON/IMAC/IMACN2,IMALE2,IMANAM
!
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIC'
      ISUBN2='LA  '
      IFOUND='YES'
      IERROR='NO'
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,51)
   51    FORMAT('***** AT THE BEGINNING OF DPICLA--')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,52)IMACRO,IMACNU,IMACCS,IMACL1,IMACL2
   52    FORMAT('IMACRO,IMACNU,IMACCS,IMACL1,IMACL2 = ',   &
                A4,I8,2X,A12,I8,I8)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,53)IBUGS2,IERROR,ICOM,ICOM2,MAXOBV,IWIDTH
   53    FORMAT('IBUGS2,IERROR,ICOM,ICOM2,MAXOBV,IWIDTH = ',   &
                4(A4,2X),2I8)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,56)(IANSLC(I),I=1,MIN(120,IWIDTH))
   56    FORMAT('IANSLC(.) = ',120A1)
         CALL DPWRST('XXX','BUG ')
!
         WRITE(ICOUT,57)NUMARG,MAXNAM
   57    FORMAT('NUMARG,MAXNAM = ',2I8)
         CALL DPWRST('XXX','BUG ')
         IF(NUMARG.GE.1)THEN
            DO 58 I=1,NUMARG
               WRITE(ICOUT,59)I,IHARG(I),IHARG2(I),IARGT(I),   &
                              IARG(I),ARG(I)
   59          FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),',   &
                      'ARG(I) = ',I8,2X,2A4,2X,A4,I8,G15.7)
               CALL DPWRST('XXX','BUG ')
   58       CONTINUE
         ENDIF
!
         WRITE(ICOUT,62)NUMNAM,NUMCHA
   62    FORMAT('NUMNAM,NUMCHA = ',2I8)
         CALL DPWRST('XXX','BUG ')
         IF(NUMNAM.GE.1)THEN
            DO 65 I=1,NUMNAM
               WRITE(ICOUT,66)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
               IVALUE(I),VALUE(I)
   66          FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),',   &
               'IVALUE(I),VALUE(I) = ',I8,2X,A4,2X,A4,2X,A4,I8,E15.7)
               CALL DPWRST('XXX','BUG ')
   65       CONTINUE
         ENDIF
      ENDIF
!
!               ***********************************************
!               **  STEP 1--                                 **
!               **  CHECK FOR                                **
!               **     INSERT CALL ARGUMENTS                 **
!               ***********************************************
!
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'INSE ' .AND. IHARG(1).EQ.'CALL' .AND.   &
         IHARG(2).EQ.'ARGU')THEN
        IFOUND='YES'
      ELSE
        IFOUND='NO'
        GO TO 9000
      ENDIF
!
!               **************************************************
!               **  STEP 63--                                   **
!               **  FIND THE FIRST AND LAST ROW OF THE SUB-CHUNK**
!               **  OF THE FILE BEING EXECUTED                  **
!               **  IMACL1 = FIRST LINE TO BE EXECUTED          **
!               **  IMACL2 = LAST  LINE TO BE EXECUTED          **
!               **  IMACLR = NUMBER OF LINES ALREADY EXECUTED   **
!               **************************************************
!
      ISTEPN='63'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICANS=' '
      DO 6310 I=1,IWIDTH
        ICANS(I:I)=IANSLC(I)(1:1)
 6310 CONTINUE
!
      IMACL1=1
      IMACL2=100000
      IHYPS2=IHYPSW
      ICOMCL='ON'
!
      DO 6380 II=1,50
        IMACAR(II)=' '
        IMACLA(II)=' '
        IMACLL(II)=0
 6380 CONTINUE
      NMACAG=0
      NMACLA=0
      NSARG=3
      IF(NUMARG.LT.NSARG)GO TO 9000
!
      IFILQZ=IFILQU
!
!     DPTYPE DOES NOT SPLIT WORDS IN THE WAY NEEDED IN PARSING
!     COMMAND LINE ARGUMENTS.  CALL DPNUWO TO DETERMINE THE
!     NUMBER OF WORDS ON THE COMMAND LINE.
!
      ISTR=' '
      DO 18394 II=1,IWIDTH
        ISTR(II:II)=IANSLC(II)(1:1)
18394 CONTINUE
      ISTART=1
      CALL DPNUWO(ISTR,ISTART,IWIDTH,NWORD,   &
                  IBUGS2,ISUBRO,IERROR)
!
      IFILQU='ON'
      DO 6370 J=NSARG+1,NWORD
        NMACAG=NMACAG+1
        IF(NMACAG.GT.50)GO TO 6379
        ISTART=1
        ISTOP=IWIDTH
        IWORD=J
        IHYPSW='OFF'
        CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,   &
                    ICOL1,ICOL2,IMACAR(NMACAG),NCTEMP,   &
                    IBUGS2,ISUBRO,IERROR)
        IHYPSW=IHYPS2
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
          ISTEPN='637'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,6394)J,IWORD,NMACAG,NCTEMP,IMACAR(NMACAG)
 6394     FORMAT('6370: J,IWORD,NMACAG,NCTEMP,IMACAR(NMACAG) = ',   &
                 4I5,2X,A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!       CHECK IF 80 CHARACTERS EXCEEDED (BE SURE TO MAKE LAST
!       CHARACTER A QUOTE IF FIRST CHARACTER IS A QUOTE).
!
        IF(NCTEMP.GT.80)THEN
          NCTEMP=80
          IF(IMACAR(NMACAG)(1:1).EQ.'"')   &
             IMACAR(NMACAG)(NCTEMP:NCTEMP)='"'
        ENDIF
!
!       REMOVE LEADING/TRAILING QUOTES IF NEEEDED
!
        IF(IMACAR(NMACAG)(1:1).EQ.'"' .AND.   &
           IMACAR(NMACAG)(NCTEMP:NCTEMP).EQ.'"')THEN
          IF(IQUOST.EQ.'ON')THEN
            IMACAR(NMACAG)(1:NCTEMP-2)=IMACAR(NMACAG)(2:NCTEMP-1)
            NCTEMP=NCTEMP-2
            IMACAR(NMACAG)(NCTEMP+1:NCTEMP+2)='  '
          ENDIF
        ENDIF
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,17394)
17394     FORMAT('AFTER STRIP QUOTES: NMACAG,IMACAR(NMACAG) =',   &
                 I5,2X,A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!       2016/09: IF FIRST ARGUMENT IS "NULL", THEN BLANK OUT
!                ARGUMENT LIST AND SET NUMBER OF ARGUMENTS TO 0.
!
        IF(NMACAG.EQ.1)THEN
          IF(IMACAR(1).EQ.'NULL' .OR. IMACAR(1).EQ.'null')THEN
            IMACAR(1)=' '
            NMACAG=0
            GO TO 6379
          ENDIF
        ENDIF
!
!       2018/05: ALLOW "EMPTY" ARGUMENTS, DO NOT SET TO ZZZZNULL
!
        IF(NCTEMP.EQ.1 .AND. IMACAR(NMACAG)(1:1).EQ.' ')THEN
          IMACAR(NMACAG)=' '
          NCTEMP=1
        ELSEIF(NCTEMP.EQ.0)THEN
          IMACAR(NMACAG)=' '
          NCTEMP=1
        ENDIF
!
!       2016/10: CHECK FOR NAMED ARGUMENTS
!
!       2018/04: CHECK FOR FOLLOWING 2 CASES
!
!                1.  "FRAME=FOR I = 1 1 50"
!                2.   FRAME="FOR I = 1 1 50"
!
!                AS FIRST STEP, CHECK FOR FIRST OCCURENCE OF QUOTE
!                (IF ANY) AND FIRST OCCURRENCE OF EQUAL SIGN.
!
!                NOTE THAT CODE WAS ADDED IN MAIN AND DPTYPE ROUTINES
!                SO THAT EQUAL SIGN WILL NOT BE A DELIMITER ON A CALL
!                COMMAND.
!
        IPOSQU=0
        IPOSEQ=0
!
!       CHECK FOR FIRST EQUAL CHARACTER.  HOWEVER, IF THE EQUAL
!       CHARACTER IS PRECEEDED BY AN ESCAPE CHARACTER ("\"), THEN
!       REMOVE THE ESCAPE CHARACTER BUT TREAT AS NO EQUAL CHARACTER
!       CASE.  START WITH CHARACTER POSITION 2 AS THERE NEEDS TO BE
!       AT LEAST ONE CHARACTER FOR THE ARGUMENT NAME.
!
        DO 36311 II=2,NCTEMP-1
          IF(IMACAR(NMACAG)(II:II).EQ.'=')THEN
            IF(II.GT.1 .AND. IMACAR(NMACAG)(II-1:II-1).EQ.'\')THEN
              IMACAR(NMACAG)(II-1:NCTEMP-1)=IMACAR(NMACAG)(II:NCTEMP)
              IMACAR(NMACAG)(NCTEMP:NCTEMP)=' '
              NCTEMP=NCTEMP-1
              GO TO 36319
            ENDIF
            IPOSEQ=II
            GO TO 36319
          ENDIF
36311   CONTINUE
36319   CONTINUE
!
!       NOW CHECK FOR OCCURENCE OF QUOTE.  NOTE THAT QUOTE IS ONLY
!       TREATED AS AN ARGUMENT DELIMITER IF IT IS THE FIRST CHARACTER
!       IN THE STRING OR THE FIRST CHARACTER AFTER THE EQUAL SIGN.  IN
!       ADDITION, IF A QUOTE DELIMITER IS FOUND, CHECK FOR A QUOTE AS
!       THE LAST CHARACTER.  IF NOT FOUND, THEN ADD IT.
!
        IF(IMACAR(NMACAG)(1:1).EQ.'"')THEN
          IPOSQU=1
        ELSEIF(IMACAR(NMACAG)(IPOSEQ+1:IPOSEQ+1).EQ.'"')THEN
          IPOSQU=IPOSEQ+1
        ENDIF
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,36394)IPOSEQ,IPOSQU,NCTEMP
36394     FORMAT('IPOSEQ,IPOSQU,NCTEMP = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!       IF ENDING QUOTE NOT PRESENT, ADD IT.
!
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
!
!       PROCESS STRING BASED ON WHETHER QUOTES/EQUAL SIGNS ARE PRESENT.
!
        IF(IPOSEQ.EQ.0 .AND. IPOSQU.EQ.0)THEN
!
!         CASE WITH NO EQUAL AND NO QUOTE.  IN THIS CASE, WE HAVE
!         A POSITIONAL ARGUMENT AND DO NOT NEED TO PROCESS QUOTES.
!
!         IN THIS CASE, NO ADDITIONAL PROCESSING IS REQUIRED.
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
            WRITE(ICOUT,46351)
46351       FORMAT('NO EQUAL, NO QUOTE CASE: NOTHING DONE')
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSEIF(IPOSEQ.EQ.0 .AND. IPOSQU.GE.1)THEN
!
!         CASE WITH NO EQUAL BUT WITH QUOTE.  IN THIS CASE, WE HAVE
!         A POSITIONAL ARGUMENT AND WE NEED TO PROCESS QUOTES.
!
!         ONLY PROCESSING REQUIRED IS TO STRIP OFF LEADING/TRAILING
!         QUOTE IF THAT OPTION SET.
!
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
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
            WRITE(ICOUT,46361)
46361       FORMAT('NO EQUAL, QUOTE CASE:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46363)NMACAG,NCTEMP,IMACAR(NMACAG)
46363       FORMAT('NMACAG,NCTEMP,IMACAR(NMACAG) = ',2I8,A80)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSEIF(IPOSEQ.GE.1 .AND. IPOSQU.EQ.0)THEN
!
!         CASE WITH EQUAL AND NO QUOTE.  IN THIS CASE, WE HAVE
!         A NAMED ARGUMENT AND DO NOT NEED TO PROCESS QUOTES.
!
!         IN THIS CASE, NEED TO MODIFY THE MACRO NAME TABLE AND
!         ALSO ADJUST THE ARGUMENT STRING.
!
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
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
            WRITE(ICOUT,46371)
46371       FORMAT('EQUAL, NO QUOTE CASE:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46373)NMACAG,NCTEMP,IMACAR(NMACAG)
46373       FORMAT('NMACAG,NCTEMP,IMACAR(NMACAG) = ',2I8,A80)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46375)NMACLA,IMACLL(NMACLA),IMACNC(NMACLA),   &
                              IMACLA(NMACLA)
46375       FORMAT('NMACLA,IMACLL(NMACLA),IMACNC(NMACLA),',  &
                   'IMACLA(NMACLA) = ',3I8,A80)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSEIF(IPOSEQ.GE.1 .AND. IPOSQU.GE.1)THEN
!
!         CASE WITH EQUAL AND QUOTE.  IN THIS CASE, WE HAVE
!         A NAMED ARGUMENT AND WE NEED TO PROCESS QUOTES.
!
!         TREAT CASE WHERE QUOTE IS FOR THE FULL STRING SEPARATELY
!         FROM CASE WHERE QUOTE IS FOR THE VALUE ONLY.
!
          IF(IPOSQU.EQ.1)THEN
            NMACLA=NMACLA+1
            IMACLL(NMACLA)=NMACAG
            ICNT=IPOSEQ-2
            IMACLA(NMACLA)(1:ICNT)=IMACAR(NMACAG)(2:IPOSEQ-1)
            IMACNC(NMACLA)=ICNT
            ICNT2=NCTEMP-IPOSEQ-1
            IF(ICNT2.GE.1)THEN
              IMACAR(NMACAG)(1:ICNT2)=   &
                IMACAR(NMACAG)(IPOSEQ+1:NCTEMP-1)
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
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
            WRITE(ICOUT,46381)
46381       FORMAT('EQUAL, QUOTE CASE:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46373)NMACAG,NCTEMP,IMACAR(NMACAG)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,46375)NMACLA,IMACLL(NMACLA),IMACNC(NMACLA),   &
                              IMACLA(NMACLA)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
!
        IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,6373)NMACAG,NCTEMP,IMACAR(NMACAG)
 6373     FORMAT('NMACAG,NCTEMP,IMACAR(NMACAG) = ',2I5,A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 6370 CONTINUE
 6379 CONTINUE
      IFILQU=IFILQZ
!
!     2018/04: CHECK FOR OPENING AND CLOSING PARENTHEIS AROUND FULL
!              ARGUMENT LIST (E.G., CALL (Y=Y,X=X)).
!
!              NOTE THAT THE LEFT PARENTHESIS MAY BE PART OF THE
!              ARGUMENT LABEL (IF GIVEN) OR THE ARGUMENT VALUE
!              (IF ARGUMENTS ENTERED BY POSITION).  THE RIGHT
!              PARENTHESIS WILL ALWAYS BE PART OF THE ARGUMENT
!              VALUE.
!
!              AS A FURTHER COMPLICATION, NEED TO CHECK IF THE
!              LEFT PARENTHESIS IS FOLLOWED BY A SPACE AND LIKEWISE
!              IF THE RIGHT PARENTHESIS IS PRECEEDED BY A SPACE.
!
      IF(NMACAG.GE.1)THEN
!
!       STEP 1: CHECK IF EITHER FIRST LABEL OR FIRST ARGUMENT IS
!               STARTS WITH A PARENTHESIS.  CHECK LABEL FIRST.
!
        IFLAG=0
        IF(IMACLA(1)(1:1).EQ.'(')THEN
          IFLAG=1
          DO 16392 KK=1,7
            IMACLA(1)(KK:KK)=IMACLA(1)(KK+1:KK+1)
16392     CONTINUE
          IMACLA(1)(8:8)=' '
          IMACNC(1)=IMACNC(1)-1
!
!         NOW REMOVE ANY LEADING SPACES FROM LABEL
!
          DO 16394 KK=1,IMACNC(1)
            IF(IMACLA(1)(KK:KK).NE.' ')THEN
              IF(KK.GT.1)THEN
                NCTEMP=IMACNC(1) - KK + 1
                IMACLA(1)(1:NCTEMP)=IMACLA(1)(KK:IMACNC(1))
                IMACLA(1)(NCTEMP+1:8)=' '
                IMACNC(1)=NCTEMP
              ENDIF
              GO TO 16396
            ENDIF
16394     CONTINUE
16396     CONTINUE
        ELSEIF(IMACAR(1)(1:1).EQ.'(')THEN
          IFLAG=2
          DO 16393 KK=1,79
            IMACAR(1)(KK:KK)=IMACAR(1)(KK+1:KK+1)
16393     CONTINUE
          IMACAR(1)(80:80)=' '
        ENDIF
!
        IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,46661)IFLAG
46661     FORMAT('IFLAG = ',I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!       STEP 2: CHECK IF LAST ARGUMENT VALUE ENDS WITH RIGHT PARENTHESIS
!
        DO 16391 JJ=80,1,-1
          IF(IMACAR(NMACAG)(JJ:JJ).EQ.')')THEN
            IMACAR(NMACAG)(JJ:JJ)=' '
          ENDIF
16391   CONTINUE
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'ICLA')THEN
          WRITE(ICOUT,46391)
46391     FORMAT('AFTER REMOVE LEADING/TRAILING PARENTHESIS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46392)NMACLA,IMACLL(1),IMACNC(1),   &
                            IMACNC(1),IMACLA(1)
46392     FORMAT('NMACLA,IMACLL(1),IMACNC(1),IMACLA(1) = ',   &
                 3I8,2X,A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,46375)NMACLA,IMACLL(NMACLA),   &
                            IMACNC(NMACLA),IMACLA(NMACLA)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ICLA')THEN
        WRITE(ICOUT,9010)NMACLA,NMACAG
 9010   FORMAT('NMACLA,NMACAG = ',2I5)
        CALL DPWRST('XXX','BUG ')
        IF(NMACLA.GE.1)THEN
          DO 9011 JJ=1,NMACLA
            WRITE(ICOUT,9013)JJ,IMACLL(JJ),IMACLA(JJ)
 9013       FORMAT('JJ,IMACLL(JJ),IMACLA(JJ) = ',2I5,2X,A8)
            CALL DPWRST('XXX','BUG ')
 9011     CONTINUE
        ENDIF
!
        IF(NMACAG.GE.1)THEN
          DO 9020 JJ=1,NMACAG
            WRITE(ICOUT,9021)JJ,IMACAR(JJ)
 9021       FORMAT('JJ,IMACAR(JJ) = ',I5,2X,A80)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPICLA
      SUBROUTINE DPICOM(Y,X,N,MINSIZ,         &
                        Y2,XLOW,XUPP,N2,      &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--FOR DISCRETE DISTRIBUTIONS, WE TYPICALLY WANT TO
!              GENERATE A FREQUENCY DISTRIBUTION FOR THE NON-NEGATIVE
!              INTEGERS.  THIS ROUTINE WILL DO THAT.  TWO ADDITIONAL
!              FEATURES:
!
!              1) FOR LONG-TAILED DISTRIBUTIONS (E.G., THE YULE
!                 OR ZETA DISTRIBUTIONS, WE WILL HAVE AN EXTREMELY
!                 LARGE NUMBER OF EMPTY CELLS IN THE TAIL.  SO
!                 THIS ROUTINE WILL RETURN THE FREQUENCY TABLE
!                 IN THE FORM:
!
!                    FREQ  CLASS-LOWER-LIMIT  CLASS-UPPER-LIMIT
!
!                 EMPTY CLASSES WILL BE COMBINED WITH THE NEXT
!                 HIGHEST NON-EMPTY CLASS.
!
!              2) FOR THE CHI-SQUARE GOODNESS OF FIT, IT IS
!                 RECOMMENDED THAT CLASSES WITH LESS THAN 5
!                 OBSERVATIONS BE COMBINED IN ORDER FOR THE CHI-SQUARE
!                 GOODNESS OF FIT TES TO BE VALID.  AFTER COMPUTING
!                 THE FREQUENCY TABLE, CLASSES WILL BE COMBINED SO
!                 THAT ALL CLASSES HAVE A FREQUENCY OF AT LEAST
!                 "MINSIZ" WHERE "MINSIZ" IS SET BY THE USER
!                 (THE DEFAULT VALUE IS 5).
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
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
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
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION XLOW(*)
      DIMENSION XUPP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIC'
      ISUBN2='OM  '
      IERROR='NO'
      IWRITE='NO'
!
      ASUM=0.0
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
        GO TO 9000
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ICOM')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPICOM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,MINSIZ
   72   FORMAT('N,MINSIZ = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 73 I=1,N
          WRITE(ICOUT,74)I,Y(I)
   74     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 2--                              **
!               **  ROUND TO NEAREST INTEGER VALUE (AND   **
!               **  CHECK FOR NEGARIVE VALUES)            **
!               ********************************************
!
      DO 100 I=1,N
        ITEMP=INT(Y(I)+0.5)
        IF(ITEMP.LT.0)THEN
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)I,Y(I)
  102     FORMAT('      ROW ',I8,' IS NON-POSITIVE.  VALUE = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        Y(I)=REAL(ITEMP)
  100 CONTINUE
!
!               ********************************************
!               **  STEP 3--                              **
!               **  1) SORT                               **
!               **  2) EXTRACT DISTINCT VALUES IN INPUT   **
!               **     VECTOR                             **
!               **  3) GENERATE THE FREQUENCY TABLE       **
!               ********************************************
!
      CALL SORT(Y,N,Y)
      CALL DISTIN(Y,N,IWRITE,X,NDIST,IBUGA3,IERROR)
!
!     CHECK IF ALL DATA VALUES EQUAL TO SAME VALUE.
!
      IF(NDIST.EQ.1)THEN
        Y2(1)=X(1)
        XLOW(1)=X(1)-0.5
        XUPP(1)=X(1)+0.5
        N2=1
        GO TO 9000
      ENDIF
!
      DO 200 I=1,NDIST
        Y2(I)=0.0
        XLOW(I)=0.0
        XUPP(I)=0.0
  200 CONTINUE
!
      DO 300 J=1,NDIST
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
        DO 310 I=1,N
          IF(Y(I).EQ.AHOLD)THEN
            Y2(J)=Y2(J)+1
          ENDIF
  310   CONTINUE
  300 CONTINUE
!
!
!               **********************************************
!               **  STEP 4--                                **
!               **  COMBINE CLASSES WITH A FREQUECNY LESS   **
!               **  THAN MINSIZ.                            **
!               **********************************************
!
      N2=0
      IFLAG=0
      ISTRT=1
      ICNT2=NDIST
      AMINSZ=REAL(MINSIZ)
      EPS=1.0E-10
!
!  RIGHT TAIL TO CENTER.  TEMPORARILY STORE IN UPPER PART OF
!  XLOW, XUPP, AND Y2 ARRARYS, WILL THEN FLIP THE SORT AT THE
!  END.
!
      DO 400 I=NDIST,1,-1
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
!
      IF(IFLAG.EQ.1 .AND. ASUM.GT.0.0)THEN
        XLOW(ICNT2)=XLOW(1)
        XUPP(ICNT2)=XLOW(ICNT2-1)
        Y2(ICNT2)=Y2(ICNT2) + ASUM
      ENDIF
      N2RGHT=ICNT2
!
!  NOW COPY REVERSE ORDER RIGHT TAIL ENTRIES
!
      ICNT=0
      DO 500 I=ICNT2,NDIST+1,-1
        ICNT=ICNT+1
        Y2(ICNT)=Y2(I)
        XLOW(ICNT)=XLOW(I)
        XUPP(ICNT)=XUPP(I)
  500 CONTINUE
      N2=ICNT
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),XLOW(I),XUPP(I)
 9016     FORMAT('I,Y2(I),XLOW(I),XUPP(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPICOM
      SUBROUTINE DPIF(ILOCS,ICASIF,IBUGQ,ISUBRO,IERROR)
!
!     PURPOSE--DEFINE A TRUE-FALSE CHARACTER VARIABLE
!              WHICH WILL BE USED IN OTHER SUBROUTINES
!              FOR THE CONDITIONAL EXECTUION OF STATEMENTS.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83/1
!     ORIGINAL VERSION--JANUARY   1983.
!     UPDATED         --AUGUST    1987. (TO ALLOW <> TO WORK)
!     UPDATED         --AUGUST    1992. TO ALLOW    ... NOT EXIST
!     UPDATED         --AUGUST    1997. TO ALLOW    ... EXIST
!     UPDATED         --FEBRUARY  1999. IF ERROR, SET IF TO FALSE
!     UPDATED         --JULY      2002. REDO IF NOT EXIST AND IF EXIST
!     UPDATED         --JULY      2002. ADD: IF STRING = "..."
!     UPDATED         --SEPTEMBER 2012. ADD ISUBRO
!     UPDATED         --NOVEMBER  2014. FIX BUG WITH "<>" FOR STRINGS
!     UPDATED         --NOVEMBER  2014. FOR STRINGS, LET RHS BE A
!                                       PRE-DEFINED STRING
!     UPDATED         --FEBRUARY  2015. SUPPORT "SET FATAL ERROR"
!     UPDATED         --OCTOBER   2016. IF COMMAND LINE ARGUMENT xx
!                                       EXISTS
!     UPDATED         --MARCH     2017. SUPPORT AND, OR, AND XOR
!                                       FOR TWO IF CLAUSES
!     UPDATED         --FEBRUARY  2018. SUPPORT FOR AND, OR, AND XOR
!                                       EXTENDED TO THREE OR MORE IF
!                                       CLAUSES
!     UPDATED         --FEBRUARY  2018. FOR CASE   IF A = 1
!                                       IF A DOES NOT EXIST, SET TO
!                                       FALSE BUT DO NOT GENERATE
!                                       ERROR MESSAGE
!     UPDATED         --MAY       2018. SUPPORT NUMERIC VALUE FOR LHS:
!                                          IF 3 > 2
!     UPDATED         --MAY       2018. SUPPORT QUOTED LITERAL STRING
!                                       FOR LHS:
!                                          IF "XXXX" = SOLD
!     UPDATED         --MAY       2018. COMMAND LINE ARGUMENT ... NOT EXIST
!     UPDATED         --MAY       2018. BETTER MESSAGING FOR SPECIAL
!                                       CASES
!     UPDATED         --JUNE      2018. FIXED ISSUE WITH
!                                         IF " " = ...
!     UPDATED         --SEPTEMBER 2018. SUPPORT "<", "<=", ">", ">="
!                                       FOR STRINGS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASIF
      CHARACTER*4 ICASI1
      CHARACTER*4 ICASI2
      CHARACTER*4 INOT
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPER(10)
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*8 ITYPE
      CHARACTER (LEN=MAXSTR) :: ITEXT1
      CHARACTER (LEN=MAXSTR) :: ITEXT2
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXSTR) :: ISTRI2
!CCCC CHARACTER*255 ITEXT1
!CCCC CHARACTER*255 ITEXT2
!CCCC CHARACTER*255 ISTRIN
!CCCC CHARACTER*255 ISTRI2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIF'
      ISUBN2='    '
      IERROR='NO'
      ICASIF='TRUE'
      ICASI1='NULL'
      ICASI2='NULL'
      INOT='OFF'
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
!
!               ********************************
!               **  TREAT THE IF     CASE     **
!               ********************************
!
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
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.   **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
      IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************************************
!               **  STEP 1B-                                          **
!               **  CHECK FOR LOGICAL OPERATORS:                      **
!               **            AND                                     **
!               **            OR                                      **
!               **            XOR                                     **
!               ********************************************************
!
!               NOTE: CURRENTLY ALLOW ONLY ONE "AND", "OR", "XOR"
!                     OPERATOR.
!
!               NOTE (2017/07): EXTEND TO ALLOW TWO "AND", "OR", "XOR"
!                               OPERATORS
!
!                               HAS NOW BEEN EXTENDED TO "MAXLOG"
!                               (CURRENTLY SET TO 10)
!
      ISTEPN='1B'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MAXLOG=10
      NOPER=0
      NPASS=1
      JPASS=0
      NPOSLO=101
      DO 59 II=1,MAXLOG
        IOPER(II)='    '
   59 CONTINUE
      ICNT3=0
      IWIDTT=IWIDTH
!
!     SEARCH FOR AND, OR, XOR IN IANS
!
1000  CONTINUE
!
      ISTEPN='1C'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMART=NUMARG
!
      IF(NUMARG.LE.1)THEN
        IF(JPASS.GE.1)THEN
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,60)ICASIF
   60       FORMAT('       COMBINED IF STATUS   = ',A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
           ICASIF='FALS'
           IF(IFEEDB.EQ.'ON')THEN
             WRITE(ICOUT,999)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,1011)
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,7112)
 7112        FORMAT('      THERE IS ONLY ONE (OR ZERO) ARGUMENT TO ',   &
                    'THE IF COMMAND')
             CALL DPWRST('XXX','BUG ')
             WRITE(ICOUT,7017)ICASIF
             CALL DPWRST('XXX','BUG ')
           ENDIF
           GO TO 9000
        ENDIF
        GO TO 9009
      ENDIF
!
      IFLAG=0
      DO 61 II=1,NUMARG
        IF(IHARG(II).EQ.'AND ' .OR. IHARG(II).EQ.'OR  ' .OR.   &
           IHARG(II).EQ.'XOR ')THEN
          NOPER=NOPER+1
          NPASS=NPASS+1
          IF(NOPER.LE.MAXLOG)IOPER(NOPER)=IHARG(II)
          NPOSLO=II
          IFLAG=1
          GO TO 63
        ENDIF
   61 CONTINUE
   63 CONTINUE
!
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
        WRITE(ICOUT,66)NOPER,NPASS,NPOSLO,NUMART,IFLAG
   66   FORMAT('NOPER,NPASS,NPOSLO,NUMART,IFLAG = ',5I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NOPER.GT.MAXLOG)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)MAXLOG
   68   FORMAT('      MORE THAN ',I2,' LOGICAL OPERATORS (AND, OR, ',   &
               'XOR) DETECTED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     FIND LOCATION IN STRING
!
      IF(NPASS.GE.2 .AND. IFLAG.EQ.1)THEN
        NUMART=NPOSLO-1
        IF(IOPER(NOPER).EQ.'AND ')THEN
          DO 71 I=1,IWIDTH
            IF(IANS(I)(1:1).EQ.' ' .AND. IANS(I+1)(1:1).EQ.'A' .AND.   &
               IANS(I+2)(1:1).EQ.'N' .AND. IANS(I+3)(1:1).EQ.'D' .AND.   &
               IANS(I+4)(1:1).EQ.' ')THEN
              IWIDTT=I-1
              ILOCP2=I+5
              ILOCSV=ILOCP2
              GO TO 79
            ENDIF
   71     CONTINUE
        ELSEIF(IOPER(NOPER).EQ.'OR  ')THEN
          DO 72 I=1,IWIDTH
            IF(IANS(I)(1:1).EQ.' ' .AND. IANS(I+1)(1:1).EQ.'O' .AND.   &
               IANS(I+2)(1:1).EQ.'R' .AND. IANS(I+3)(1:1).EQ.' ')THEN
              IWIDTT=I-1
              ILOCP2=I+5
              ILOCSV=ILOCP2
              GO TO 79
            ENDIF
   72     CONTINUE
        ELSEIF(IOPER(NOPER).EQ.'XOR ')THEN
          DO 73 I=1,IWIDTH
            IF(IANS(I)(1:1).EQ.' ' .AND. IANS(I+1)(1:1).EQ.'X' .AND.   &
               IANS(I+2)(1:1).EQ.'O' .AND. IANS(I+3)(1:1).EQ.'R' .AND.   &
               IANS(I+4)(1:1).EQ.' ')THEN
              IWIDTT=I-1
              ILOCP2=I+5
              ILOCSV=ILOCP2
              GO TO 79
            ENDIF
   73     CONTINUE
        ELSE
          IWIDTT=IWIDTH
          ILOCP2=IWIDTH
        ENDIF
!
   79   CONTINUE
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,74)IWIDTT,ILOCP2,ILOCSV
   74     FORMAT('AT 74: IWIDTT,ILOCP2,ILOCSV = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
!     2016/10: CHECK IF FIRST ARGUMENT IS "NOT"
!
      IF(NUMART.GE.1 .AND. IHARG(1).EQ.'NOT ')THEN
        INOT='ON'
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGQ,IERROR)
        NUMART=NUMART-1
      ENDIF
!
!
!CCCC THE FOLLOWING SECTION WAS ADDED AUGUST 1992
!               **************************************************
!               **  STEP 2.0--                                  **
!               **  TREAT THE     IF ... NOT EXIST CASE         **
!               **      IF ... NOT EXIST THEN ==> ICASIF = TRUE **
!               **      IF ... EXIST THEN ==> ICASIF = FALSE    **
!               **************************************************
!
!               2016/10: ADD
!
!                        IF COMMAND LINE ARGUMENT <name> EXISTS
!
!               2018/05: ADD
!
!                        IF COMMAND LINE ARGUMENT <name> NOT EXISTS
!
      ISTEPN='2'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMART.GE.5)THEN
        IF(IHARG(1).EQ.'COMM' .AND. IHARG(2).EQ.'LINE' .AND.   &
           IHARG(3).EQ.'ARGU' .AND. IHARG(5).EQ.'EXIS')THEN
!
          ISTRIN=' '
          ISTRI2=' '
          DO 75 II=1,IWIDTT
            ISTRIN(II:II)=IANSLC(II)(1:1)
   75     CONTINUE
          ISTART=1
          ISTOP=IWIDTT
          IWORD=5
          CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                      ICOL1,ICOL2,ISTRI2,NCSTRI,   &
                      IBUGQ,ISUBRO,IERROR)
          DO 77 II=1,NMACLA
            IF(ISTRI2(1:8).EQ.IMACLA(II)(1:8))THEN
              IEXIST='YES'
              ICASIF='TRUE'
              GO TO 7011
            ENDIF
   77     CONTINUE
          IEXIST='NO'
          ICASIF='FALS'
!
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
!
          GO TO 9000
!
        ELSEIF(IHARG(1).EQ.'COMM' .AND. IHARG(2).EQ.'LINE' .AND.   &
           IHARG(3).EQ.'ARGU' .AND. IHARG(5).EQ.'NOT ' .AND.   &
           IHARG(6).EQ.'EXIS')THEN
!
          ISTRIN=' '
          ISTRI2=' '
          DO 7078 II=1,IWIDTT
            ISTRIN(II:II)=IANSLC(II)(1:1)
 7078     CONTINUE
          ISTART=1
          ISTOP=IWIDTT
          IWORD=5
          CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                      ICOL1,ICOL2,ISTRI2,NCSTRI,   &
                      IBUGQ,ISUBRO,IERROR)
          DO 7079 II=1,NMACLA
            IF(ISTRI2(1:8).EQ.IMACLA(II)(1:8))THEN
              ICASIF='FALS'
              IEXIST='YES'
              GO TO 7081
            ENDIF
 7079     CONTINUE
          IEXIST='NO'
          ICASIF='TRUE'
!
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
!
          GO TO 9000
!
        ENDIF
      ENDIF
!
      IF(NUMART.GE.3)THEN
         IF(IHARG(2).EQ.'NOT'.AND.IHARG(3).EQ.'EXIS')THEN
!
            IH=IHARG(1)
            IH2=IHARG2(1)
            MESSAG='NO'
            CALL CHECKF(IH,IH2,IHWUSE,   &
                        IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                        NUMNAM,MAXNAM,   &
                        ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,ITYPE)
!
            IF(ITYPE.EQ.'NONE')THEN
              IEXIST='NO'
              ICASIF='TRUE'
            ELSE
              IEXIST='YES'
              ICASIF='FALS'
            ENDIF
            IERROR='NO'
!
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
!
            GO TO 9000
!
         ENDIF
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED NOVEMBER 1997
!               **************************************************
!               **  STEP 2.0A--                                 **
!               **  TREAT THE     IF ...     EXIST CASE         **
!               **      IF ...     EXIST THEN ==> ICASIF = TRUE **
!               **      IF ... NOT EXIST THEN ==> ICASIF = FALSE**
!               **************************************************
!
      ISTEPN='2A'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASIF='TRUE'
      IF(NUMART.GE.2)THEN
         IF(IHARG(2).EQ.'EXIS')THEN
!
            IH=IHARG(1)
            IH2=IHARG2(1)
            MESSAG='NO'
            ILOC=-99
            ITYPE='NONE'
            CALL CHECKF(IH,IH2,IHWUSE,   &
                        IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                        NUMNAM,MAXNAM,   &
                        ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,ITYPE)
!
            IF(ITYPE.EQ.'NONE')THEN
              ICASIF='FALS'
            ELSE
              ICASIF='TRUE'
            ENDIF
            IERROR='NO'
!
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
!
            GO TO 9000
!
         ENDIF
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED JULY 2002.
!               **************************************************
!               **  STEP 2.0B-                                  **
!               **  TREAT THE                                   **
!               **      IF STRING = "....."     CASE            **
!               **      IF STRING <> "..."      CASE            **
!               **************************************************
!
!     ORIGINAL IMPLEMENTATION REQUIRED PRE-DEFINED STRING ON
!     LEFT HAND SIDE OF "=" (OR "<>").
!
!     2018/05: FOR STRINGS, MAKE MATCH TRUE IF ONE SIDE IS A
!              SINGLE BLANK CHARACTER AND THE OTHER IS A
!              NULL (NO CHARACTERS) STRING.
!
!     2018/06: THE FOLLOWING WAS A PROBLEM
!
!                 IF " " = ....
!
!              IN THIS CASE, THE EQUAL SIGN IS IN IHARG(3), NOT
!              IHARG(2).  PACK THIS INTO "".
!
!              RELATED TO THIS, CONSTRUCTS LIKE
!
!                 IF "SUBSET Y 4" = ....
!
!              ARE AN ISSUE.   CHECK FOR LOCATION OF THE EQUAL SIGN.
!
      ISTEPN='2B'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'IF')THEN
!
        IF(IHARG(1).EQ.'"   ' .AND. IHARG(2).EQ.'"   ')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGQ,IERROR)
          IHARG(1)(1:1)='"'
          IHARG(1)(2:2)='"'
        ENDIF
!
        IPOSEQ=-1
        DO 147 KK=2,NUMARG-1
          IF(IHARG(KK).EQ.'=   ' .OR. IHARG(KK).EQ.'<>  ' .OR.   &
             IHARG(KK).EQ.'<=  ' .OR. IHARG(KK).EQ.'>=  ' .OR.   &
             IHARG(KK).EQ.'<   ' .OR. IHARG(KK).EQ.'>   ')THEN
            IPOSEQ=KK
            GO TO 148
          ENDIF
  147   CONTINUE
  148   CONTINUE
!
!CCCC   IF(NUMART.GE.3.AND.(IHARG(2).EQ.'='.OR.IHARG(2).EQ.'<>'))THEN
        IF(NUMART.GE.3.AND.IPOSEQ.GT.0)THEN
!
          IH=IHARG(1)
          IH2=IHARG2(1)
          MESSAG='NO'
          CALL CHECKF(IH,IH2,IHWUSE,   &
          IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
          ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,ITYPE)
!
!         IF FIRST ARGUMENT IS A PREVIOSLY DEFINED STRING, EXTRACT
!         THE VALUE.  OTHERWISE, CHECK TO SEE IF THE FIRST ARGUMENT
!         IS A QUOTED STRING.  LITERAL STRINGS MUST BE ENCLOSED IN
!         DOUBLE QUOTES (IF NOT, THE ARGUMENT IS INTERPRETED AS A
!         NAME AND A STRING COMPARISON IS NOT PERFORMED).
!
          IF(ITYPE.EQ.'STRING')THEN
            NTEXT1=0
            ITEXT1=' '
            NSTRT=IVSTAR(ILOC)
            NSTOP=IVSTOP(ILOC)
            DO 140 J=NSTRT,NSTOP
              NTEXT1=NTEXT1+1
              ITEXT1(NTEXT1:NTEXT1)=IFUNC(J)(1:1)
  140       CONTINUE
          ELSE
            ISTRIN=' '
            ISTRI2=' '
            DO 142 II=1,IWIDTT
              ISTRIN(II:II)=IANSLC(II)(1:1)
  142       CONTINUE
            ISTART=1
            ISTOP=IWIDTT
            IWORD=2
            IFILQZ=IFILQU
            IFILQU='ON'
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                        ICOL1,ICOL2,ISTRI2,NCSTR,   &
                        IBUGQ,ISUBRO,IERROR)
            IFILQU=IFILQZ
            IF(ISTRI2(1:1).EQ.'"' .AND. ISTRI2(NCSTR:NCSTR).EQ.'"')THEN
              ITEXT1(1:NCSTR-2)=ISTRI2(2:NCSTR-1)
              NTEXT1=NCSTR-2
            ELSE
              GO TO 199
            ENDIF
          ENDIF
!
          ISTEPN='2B1'
          IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
            GO TO 9000
          ENDIF
          IERROR='NO'
!
!CCCC SEARCH FOR STRING AFTER THE "=" OR "<>".
!
!CCCC 2014/11: CHECK IF PRE-DEFINED STRING GIVEN FIRST
!
          IF(IFLAG.EQ.3 .OR. IFLAG.EQ.5)THEN
            IH=IHARG(IPOSEQ+2)
            IH2=IHARG2(IPOSEQ+2)
          ELSE
            IH=IHARG(IPOSEQ+1)
            IH2=IHARG2(IPOSEQ+1)
          ENDIF
          MESSAG='NO'
          CALL CHECKF(IH,IH2,IHWUSE,   &
          IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
          ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,ITYPE)
!
          IF(ITYPE.EQ.'STRING')THEN
            NTEXT2=0
            ITEXT2=' '
            NSTRT=IVSTAR(ILOC)
            NSTOP=IVSTOP(ILOC)
            DO 143 J=NSTRT,NSTOP
              NTEXT2=NTEXT2+1
              ITEXT2(NTEXT2:NTEXT2)=IFUNC(J)(1:1)
  143       CONTINUE
          ELSE
            IF(IFLAG.EQ.0 .OR.IFLAG.EQ.2 .OR. IFLAG.EQ.4)THEN
!
              ISTEPN='2B2'
              IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
                CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
              DO 110 I=1,IWIDTT
                IF(IANSLC(I).EQ.'=')THEN
                  ISTRT=I+1
                  GO TO 119
                ENDIF
  110         CONTINUE
              IERROR='YES'
              GO TO 9000
  119         CONTINUE
            ELSEIF(IFLAG.EQ.1 .OR. IFLAG.EQ.3 .OR. IFLAG.EQ.5)THEN
!
              ISTEPN='2B2'
              IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
                CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
              DO 120 I=1,IWIDTT
                IF(IANSLC(I).EQ.'<' .AND. IANSLC(I+1).EQ.'>')THEN
                  ISTRT=I+2
                  GO TO 129
                ELSEIF(IANSLC(I).EQ.'<' .AND. IANSLC(I+1).EQ.'=')THEN
                  ISTRT=I+2
                  GO TO 129
                ELSEIF(IANSLC(I).EQ.'>' .AND. IANSLC(I+1).EQ.'=')THEN
                  ISTRT=I+2
                  GO TO 129
                ENDIF
  120         CONTINUE
              IERROR='YES'
              GO TO 9000
  129         CONTINUE
            ENDIF
!
            ISTEPN='2B3'
            IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
              CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
              WRITE(ICOUT,156)ISTRT
  156         FORMAT('ISTRT = ',I5)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            NTEXT2=0
            ITEXT2=' '
            DO 130 I=ISTRT,IWIDTT
              IF(IANSLC(I).EQ.' ')THEN
                GO TO 130
              ELSEIF(IANSLC(I).EQ.'"')THEN
                NSTRT=I+1
                ICOUNT=0
                DO 132 J=NSTRT,IWIDTT
                  IF(IANSLC(J).EQ.'"')THEN
                    NLAST=J-1
                    GO TO 134
                  ELSE
                    ICOUNT=ICOUNT+1
                    ITEXT2(ICOUNT:ICOUNT)=IANSLC(J)(1:1)
                  ENDIF
  132           CONTINUE
                NLAST=IWIDTH
  134           CONTINUE
                NTEXT2=NLAST-NSTRT+1
                GO TO 139
              ELSE
                NSTRT=I
                ICOUNT=0
                DO 137 J=NSTRT,IWIDTT
                  IF(IANSLC(J).EQ.' ')THEN
                    NLAST=J-1
                    GO TO 138
                  ELSE
                    ICOUNT=ICOUNT+1
                    ITEXT2(ICOUNT:ICOUNT)=IANSLC(J)(1:1)
                  ENDIF
  137           CONTINUE
                NLAST=IWIDTT
  138           CONTINUE
                NTEXT2=NLAST-NSTRT+1
                GO TO 139
              ENDIF
  130       CONTINUE
  139       CONTINUE
          ENDIF
!
!CCCC     NOW COMPARE THE TWO STRINGS (IMTCH=0 FOR MATCH,
!CCCC     1 FOR NO MATCH).
!CCCC
!CCCC     2018/05: MAKE SINGLE BLANK CHARACTER MATCH TRUE TO
!CCCC              EMPTY STRING.
!CCCC     2018/09: "<", "<=", ">", ">=" CASES HANDLED SEPARATELY.
!
          IF(IFLAG.EQ.2)THEN
!
!           LESS THAN CASE
!
            DO 1141 II=1,MAX(NTEXT1,NTEXT2)
              ICASIF='TRUE'
              IVAL1=0
              IVAL2=0
              ICNT=II
              IF(II.LE.NTEXT1)CALL DPCOAN(ITEXT1(II:II),IVAL1)
              IF(II.LE.NTEXT2)CALL DPCOAN(ITEXT2(II:II),IVAL2)
              IF(IVAL1.GT.IVAL2)THEN
                ICASIF='FALS'
                GO TO 1651
              ENDIF
 1141       CONTINUE
            IF(IVAL1.EQ.IVAL2)ICASIF='FALS'
            GO TO 1651
          ELSEIF(IFLAG.EQ.3)THEN
!
!           LESS THAN OR EQUAL TO CASE
!
            DO 1142 II=1,MAX(NTEXT1,NTEXT2)
              ICASIF='TRUE'
              IVAL1=0
              IVAL2=0
              IF(II.LE.NTEXT1)CALL DPCOAN(ITEXT1(II:II),IVAL1)
              IF(II.LE.NTEXT2)CALL DPCOAN(ITEXT2(II:II),IVAL2)
              IF(IVAL1.GT.IVAL2)THEN
                ICASIF='FALS'
                GO TO 1651
              ENDIF
 1142       CONTINUE
            GO TO 1651
          ELSEIF(IFLAG.EQ.4)THEN
!
!           GREATER THAN CASE
!
            DO 1143 II=1,MAX(NTEXT1,NTEXT2)
              ICASIF='TRUE'
              IVAL1=0
              IVAL2=0
              IF(II.LE.NTEXT1)CALL DPCOAN(ITEXT1(II:II),IVAL1)
              IF(II.LE.NTEXT2)CALL DPCOAN(ITEXT2(II:II),IVAL2)
              IF(IVAL1.LT.IVAL2)THEN
                ICASIF='FALS'
                GO TO 1651
              ENDIF
 1143       CONTINUE
            IF(IVAL1.EQ.IVAL2)ICASIF='FALS'
            GO TO 1651
          ELSEIF(IFLAG.EQ.5)THEN
!
!           GREATER THAN OR EQUAL TO CASE
!
            DO 1144 II=1,MAX(NTEXT1,NTEXT2)
              ICASIF='TRUE'
              IVAL1=0
              IVAL2=0
              IF(II.LE.NTEXT1)CALL DPCOAN(ITEXT1(II:II),IVAL1)
              IF(II.LE.NTEXT2)CALL DPCOAN(ITEXT2(II:II),IVAL2)
              IF(IVAL1.LT.IVAL2)THEN
                ICASIF='FALS'
                GO TO 1651
              ENDIF
 1144       CONTINUE
            GO TO 1651
          ENDIF
!
          IMTCH=0
          IF(NTEXT1.EQ.1 .AND. NTEXT2.EQ.0 .AND.   &
            ITEXT1(1:1).EQ.' ')THEN
              IMTCH=0
          ELSEIF(NTEXT1.EQ.1 .AND. NTEXT2.EQ.1 .AND.   &
            ITEXT1(1:1).EQ.' ' .AND. ITEXT2(1:1).EQ.' ')THEN
              IMTCH=0
          ELSEIF(NTEXT1.EQ.0 .AND. NTEXT2.EQ.0)THEN
              IMTCH=0
          ELSEIF(NTEXT1.EQ.0 .AND. NTEXT2.EQ.1 .AND.   &
            ITEXT2(1:1).EQ.' ')THEN
              IMTCH=0
          ELSEIF(NTEXT1.EQ.NTEXT2)THEN
            DO 150 J=1,NTEXT1
              IF(ITEXT1(J:J).NE.ITEXT2(J:J))THEN
                IMTCH=1
                GO TO 159
              ENDIF
  150       CONTINUE
  159       CONTINUE
          ELSE
            IMTCH=1
          ENDIF
!
!CCCC SET IF STATUS
!
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
!
 1651     CONTINUE
!
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
          GO TO 9000
        ENDIF
      ENDIF
!
  199 CONTINUE
!
!               ********************************************************
!               **  STEP 2.1-- C                                      **
!               **  INITIALIZE ALL ELEMENTS IN ISUB(.) TO 11 ISUB(.)  **
!               **  ISUB(.) WILL TAKE ON 4 VALUES AT MOST--00, 01,    **
!               **  10, 11 .  THE FIRST  DIGIT INDICATES WHETHER OR   **
!               **  NOT THE GIVEN ELEMENT IS OUT (0) OR IN (1) OF THE **
!               **  LOCAL  CUMULATIVE UNION SET.  THE SECOND DIGIT    **
!               **  INDICATES WHETHER OR NOT THE GIVEN ELEMENT IS OUT **
!               **  (0) OR IN (1) OF THE GLOBAL CUMULATIVE            **
!               **  INTERSECTIONS THE INITIALIZATION OF ALL ELEMENTS  **
!               **  TO 11 THUS INDICATES THAT INITIALLY ALL ELEMENTS  **
!               **  (TEMPORARILY) ARE IN THE LOCAL UNION SET, AND     **
!               **  INITIALLY ALL ELEMENTS ARE IN THE GLOBAL          **
!               **  INTERSECTION SET.                                 **
!               ********************************************************
!
      ISTEPN='2.1'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NIOLD=1
      DO 200 I=1,NIOLD
        ISUB(I)=11
  200 CONTINUE
!
!               *************************************************
!               **  STEP 2.2--                                 **
!               **  IF EXISTENT,                               **
!               **  PACK < = INTO <=                           **
!               **  PACK = < INTO =<                           **
!               **  PACK > = INTO >=                           **
!               **  PACK = > INTO =>                           **
!               **  THIS IS BECAUSE = SIGNS ARE AUTOMATICALLY  **
!               **  GIVEN A SPACE IN DPTYPE AND TREATED AS     **
!               **  AS A SEPARATE WORD.                        **
!               **  NOTE THAT NUMARG WILL BE CHANGED.          **
!               *************************************************
!
      ISTEPN='2.2'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL ADJUS2(IHARG,IHARG2,IARG,ARG,IARGT,NUMART)
!
!               ************************************************
!               **  STEP 3.1--                                **
!               **  CHECK TO SEE IF HAVE THE  IF      CASE.   **
!               **  LOCATE THE POSITION IN THE ARGUMENT LIST  **
!               **  OF THE WORD    IF   .                     **
!               ************************************************
!
      ISTEPN='3.1'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      JMAX=0
      ICASSC='SEAR'
      ICASQU='UNKN'
      NUMSV=0
      DO 300 IPASS=1,100
!
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
  303       FORMAT('ICASSC,ILOCTG,IHARG(ILOCTG),IHARG2(ILOCTG) = ',   &
                   A4,I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,304)ICASSC,ILOCTG
  304       FORMAT('ICASSC,ILOCTG = ',A4,2X,I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
        IF(ICASSC.EQ.'STOP')GO TO 1100
        JMIN=JMAX+1
        IF(JMIN.GT.NUMART)GO TO 1100
        IF(JMIN.EQ.NUMART.AND.IHARG(JMIN).EQ.'AND '.AND.   &
           IHARG2(JMIN).EQ.'    ')GO TO 1100
!
        IF(ICASSC.EQ.'CONT')GO TO 600
        DO 310 I=1,NIOLD
          ITEMP=ISUB(I)
          IF(ITEMP.EQ.00)ISUB(I)=00
          IF(ITEMP.EQ.10)ISUB(I)=00
          IF(ITEMP.EQ.01)ISUB(I)=00
          IF(ITEMP.EQ.11)ISUB(I)=11
  310   CONTINUE
!
        ICASQU='UNKN'
        DO 340 J=JMIN,NUMART
          J2=J
          IF(IHARG(J).EQ.'IF  '.AND.IHARG2(J).EQ.'    ')THEN
            ICASQU='IF  '
            ILOCS=J2
            GO TO 390
          ENDIF
  340   CONTINUE
        IF(JMIN.EQ.1.AND.   &
           ICOM.EQ.'IF  '.AND.ICOM2.EQ.'    ')THEN
          J2=0
          ICASQU='IF  '
          ILOCS=J2
          GO TO 390
        ENDIF
        ILOCS=NUMART+1
        GO TO 1100
!
  390   CONTINUE
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,391)IPASS,ICASQU,ILOCS
  391     FORMAT('IPASS,ICASQU,ILOCS = ',I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *******************************************
!               **  STEP 3.2--                           **
!               **  IF HAVE THE IF     CASE,             **
!               **  INITIALIZE ISUB(.) TO 0X--00 OR 01.  **
!               *******************************************
!
        ISTEPN='3.2'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(ICASQU.EQ.'IF  ')THEN
          DO 401 I=1,NIOLD
            ITEMP=ISUB(I)
            IF(ITEMP.EQ.00)ISUB(I)=00
            IF(ITEMP.EQ.10)ISUB(I)=00
            IF(ITEMP.EQ.01)ISUB(I)=01
            IF(ITEMP.EQ.11)ISUB(I)=01
  401     CONTINUE
        ELSE
          IERROR='YES'
          GO TO 9000
        ENDIF
!
!               ****************************************************
!               **  STEP 4--                                      **
!               **  CHECK VALIDITY OF FIRST ARGUMENT AFTER     IF **
!               **  THIS SHOULD BE THE IF PARAMETER               **
!               ****************************************************
!
!       2018/05: ALLOW SYNTAX LIKE
!
!                   IF 3 > 2
!                   IF 3 > B
!
!                THAT IS, THE ARGUMENT ON THE LEFT HAND SIDE CAN BE
!                A NUMERIC VALUE AS WELL AS A PARAMETER.
!
        ISTEPN='4'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          GO TO 8000
        ENDIF
!
        IHSET=IHARG(ILOCS1)
        IHSET2=IHARG2(ILOCS1)
!
!       2015/02: IF PARAMETER NOT FOUND, SET IF STATUS TO FALSE
!
        ICASPA='P   '
        IHWUSE='P'
!CCCC   2018/02: IF PARAMETER DOES NOT EXIST, SIMPLY SET CLAUSE TO
!CCCC            FALSE, BUT DO NOT GENERATE ERROR MESSAGE.
!CCCC   MESSAG='YES'
        MESSAG='NO'
        CALL CHECKN(IHSET,IHSET2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')THEN
          IF(IARGT(ILOCS1).EQ.'NUMB')THEN
            ASETV=ARG(ILOCS1)
          ELSE
            ICASIF='FALS'
            IERROR='FALS'
            GO TO 9000
          ENDIF
        ELSE
          ASETV=VALUE(ILOC)
        ENDIF
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='4A'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,451)ILOCS1,NUMART,IPASS,IHSET,IHSET2,ICASPA,ASETV
  451     FORMAT('ILOCS1,NUMARGT,IPASS,IHSET,IHSET2,ICASPA,ASETV = ',   &
                 3I8,3X,2A4,2X,A4,3X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ********************************************************
!               **  STEP 5--                                          **
!               **  CHECK TO SEE IF NEXT ARGUMENT IS                  **
!               **        <                                           **
!               **        <=                                          **
!               **        =                                           **
!               **        >=                                          **
!               **        >                                           **
!               **        <>                                          **
!               **  IF NONE OF THE ABOVE, THEN THE ASSUMED OPERATION  **
!               **  IS =   .                                          **
!               ********************************************************
!
        ISTEPN='5'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICASOP='UNKN'
        ILOCS2=ILOCS+2
        JMAX=ILOCS2
        IF(ILOCS2.GT.NUMART)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,411)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,502)
  502     FORMAT('      THE   IF   PARAMETER NAME WAS THE FINAL WORD ',   &
                 'ON')
          CALL DPWRST('XXX','BUG ')
          GO TO 8000
        ENDIF
!
        IHSET=IHARG(ILOCS2)
        IHSET2=IHARG2(ILOCS2)
!
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
        ELSEIF(IHSET.EQ.'<>  ' .OR. IHSET.EQ.'><  ' .OR.   &
               IHSET.EQ.'!=  ')THEN
          ICASOP='<>  '
          ILOCTG=ILOCS2
        ELSE
          ICASOP='=ASS'
          ILOCTG=ILOCS2-1
        ENDIF
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='5A'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,591)IPASS,ILOCTG,IHSET,IHSET2,ICASPA,ICASOP
  591     FORMAT('IPASS,ILOCTG,IHSET,IHSET2,ICASPA,ICASOP = ',   &
                 2I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               **********************************************************
!               **  STEP 6--                                            **
!               **  DETERMINE THE LOWER LIMIT OF THE INTERVAL OF INTEREST.
!               **  THIS IS DONE BY CHECKING THE FIRST (NEXT) ARGUMENT  **
!               **  IN THE LIST.                                        **
!               **  ALSO, FOR THOSE 4 CASES IN WHICH                    **
!               **  ICASOP IS   <   <=   >=   >                         **
!               **  DETERMINE THE UPPER LIMIT OF THE INTERVAL OF INTEREST.
!               **********************************************************
!
  600   CONTINUE
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='6'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,601)
  601     FORMAT('     AT THE BEGINNING OF STEP 6 IN DPIF--')
          CALL DPWRST('XXX','BUG ')
          DO 605 I=1,NIOLD
            WRITE(ICOUT,606)I,ISUB(I)
  606       FORMAT('I,ISUB(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
  605     CONTINUE
       ENDIF
!
        ILOCTG=ILOCTG+1
        JMAX=ILOCTG
        IF(ILOCTG.GT.NUMART)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,611)
  611     FORMAT('***** ERROR IN DPIF--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,612)
  612     FORMAT('      THE    IF    OPERATION   <   <=  =  >=  > ',   &
                 'WAS THE FINAL WORD ON')
          CALL DPWRST('XXX','BUG ')
          GO TO 8000
        ENDIF
!
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
!CCCC     MESSAG='YES'
          MESSAG='NO'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
!
          IF(IERROR.EQ.'YES')THEN
            ICASIF='FALS'
            IERROR='FALS'
            GO TO 9000
          ENDIF
!
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
          GO TO 9000
        ENDIF
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,691)IPASS,ICASPA,ICASOP,IH,IH2,DMIN,DMAX
  691     FORMAT('IPASS,ICASPA,ICASOP,IH,IH2,DMIN,DMAX = ',   &
                 I8,4(2X,A4,2X,A4,2X,A4,2X,A4),2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               **********************************************************
!               **  STEP 7--                                            **
!               **  DETERMINE THE UPPER LIMIT OF THE INTERVAL OF INTEREST.
!               **  NOTE THAT FOR THOSE 4 CASES IN WHICH ICASOP IS      **
!               **  <   <=   >=   >    THE UPPER LIMIT OF THE INTERVAL  **
!               **  HAS ALREADY BEEN DETERMINED AND SO ALL OF THE CODE  **
!               **  OF THIS SECTION MAY BE  SKIPPED.  ON THE OTHER HAND **
!               **  WHEN THE OPERATION IS    =  , (EXPLICITLY OR        **
!               **  ASSUMED),  OR <>    ,  THE UPPER LIMIT MUST BE      **
!               **  DETERMINED.  THIS IS DONE BY CHECKING THE NEXT      **
!               **  ARGUMENT IN THE LIST.  IF THIS NEXT ARGUMENT IS TO, **
!               **  THIS IMPLIES THAT AN UPPER LIMIT WILL BE PROVIDED   **
!               **  (IN THE ARGUMENT AFTER THE   TO   ).  HOWEVER, IF   **
!               **  THE NEXT ARGUMENT IS NOT A    TO   , THEN THIS      **
!               **  IMPLIES THAT THE LIST CONSISTS OF INDIVIDUAL        **
!               **  ELEMENTS OF THE SUBSET AND SO THE UPPER LIMIT WILL  **
!               **  BE IDENTICAL TO THE LOWER LIMIT.                    **
!               **********************************************************
!
        ISTEPN='7'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(ICASOP.EQ.'<   ' .OR. ICASOP.EQ.'<=  ' .OR.   &
           ICASOP.EQ.'>=  ' .OR. ICASOP.EQ.'>   ')THEN
          ICASSC='SEAR'
          GO TO 790
        ENDIF
!
        ILOCTG=ILOCTG+1
!
        IF(ILOCTG.GT.NUMART .OR.   &
           (ILOCTG.EQ.NUMART.AND.IHARG(ILOCTG).EQ.'AND '.AND.   &
           IHARG2(ILOCTG).EQ.'    '))THEN
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='STOP'
          DMAX=DMIN
          GO TO 790
        ELSEIF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'IF  '.AND.   &
               IHARG2(ILOCTG).EQ.'    ')THEN
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='SEAR'
          DMAX=DMIN
          GO TO 790
        ELSEIF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'TO  '.AND.   &
               IHARG2(ILOCTG).EQ.'    ')THEN
          ILOCTG=ILOCTG+1
          JMAX=ILOCTG
          IF(ILOCTG.GT.NUMART)GO TO 760
          IF(ILOCTG.EQ.NUMART.AND.IHARG(ILOCTG).EQ.'AND '.AND.   &
             IHARG2(ILOCTG).EQ.'    ')GO TO 760
          IF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'IF  '.AND.   &
             IHARG2(ILOCTG).EQ.'    ')GO TO 760
          IF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'TO  '.AND.   &
             IHARG2(ILOCTG).EQ.'    ')GO TO 760
          GO TO 770
!
        ELSE
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='CONT'
          DMAX=DMIN
          GO TO 790
        ENDIF
!
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
        GO TO 9000
!
  770   CONTINUE
        IF(IARGT(ILOCTG).EQ.'NUMB')THEN
          DMAX=ARG(ILOCTG)
        ELSEIF(IARGT(ILOCTG).EQ.'WORD')THEN
          IH=IHARG(ILOCTG)
          IH2=IHARG2(ILOCTG)
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'YES')THEN
            ICASIF='FALS'
            GO TO 9000
          ENDIF
          DMAX=VALUE(ILOC)
        ELSE
          IBRAN=770
          WRITE(ICOUT,631)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,771)IBRAN
  771     FORMAT('      IMPOSSIBLE BRANCH CONDITION AT BRANCH POINT = ',   &
                 I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,772)ILOCTG,IARGT(ILOCTG)
  772     FORMAT('ILOCTG, IARGT(ILOCTG) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        ILOCTG=ILOCTG+1
        ICASSC='CONT'
        IF(ILOCTG.GT.NUMART)ICASSC='STOP'
        IF(ILOCTG.EQ.NUMART.AND.IHARG(ILOCTG).EQ.'AND '.AND.   &
           IHARG2(ILOCTG).EQ.'    ')ICASSC='STOP'
        IF(ILOCTG.LE.NUMART.AND.IHARG(ILOCTG).EQ.'IF  '.AND.   &
           IHARG2(ILOCTG).EQ.'    ')ICASSC='SEAR'
        ILOCTG=ILOCTG-1
        JMAX=ILOCTG
!
  790   CONTINUE
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,791)IPASS,ICASPA,ICASOP,IH,IH2,DMIN,DMAX
  791     FORMAT('IPASS,ICASPA,ICASOP,IH,IH2,DMIN,DMAX = ',   &
                 I8,4(2X,A4),2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ***************************************************
!               **  STEP 8--                                     **
!               **  TO ALLOW FOR ROUNDOFF ERRORS IN THE          **
!               **  STORAGE OF NUMBERS,                          **
!               **  JUDICIOUSLY EXPAND THE INTERVAL OF INTEREST  **
!               **  BY AN    EPSILON    AMOUNT.                  **
!               ***************************************************
!
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
!
        IF(DMIN.GT.DMAX)THEN
          HOLD=DMIN
          DMIN=DMAX
          DMAX=HOLD
        ENDIF
!
        IF(DMIN.EQ.CPUMIN)GO TO 819
        IF(DMIN.EQ.CPUMAX)GO TO 819
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
!
        IF(DMAX.EQ.CPUMAX)GO TO 829
        IF(DMAX.EQ.CPUMIN)GO TO 829
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
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,891)IPASS,ICASPA,ICASOP,IH,IH2
  891     FORMAT('IPASS,ICASPA,ICASOP,IH,IH2 = ',I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,892)EPS,DMIN,DMAX,CPUMIN,CPUMAX
  892     FORMAT('EPS,DMIN,DMAX,CPUMIN,CPUMAX = ',5G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ****************************************************
!               **  STEP 9--                                      **
!               **  DEFINE THE ISUB(.) VECTOR--                   **
!               **  FOR ANY K (K = 1 TO NIOLD),                   **
!               **  IF THE K-TH ELEMENT OF THE                    **
!               **  SUBSET SPECIFICATION VARIABLE                 **
!               **  (THE VARIABLE SPECIFIED AFTER    SUBSET       **
!               **  IN THE COMMAND LINE)                          **
!               **  IS WITHIN THE SPECIFIED (DMIN,DMAX) LIMITS,   **
!               **  THEN ISUB(K) SHOULD RESULT IN A VALUE OF 1;   **
!               **  BUT IF THE K-TH ELEMENT OF THE                **
!               **  SUBSET SPECIFICATION VARIABLE                 **
!               **  IS OUTSIDE THE SPECIFIED (DMIN,DMAX) LIMITS,  **
!               **  THEN ISUB(K) SHOULD RESULT IN A 0 .           **
!               ****************************************************
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          ISTEPN='9'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,901)ILOCS1,IHSET,IHSET2,ICASPA,ASETV,MAXCOL
  901     FORMAT('ILOCS1,IHSET,IHSET2,ICASPA,ASETV,MAXCOL = ',   &
                 I8,3(2X,A4),G15.7,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(ICASPA.NE.'P   ')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,631)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,912)
  912     FORMAT('      IMPROPER VALUE FOR ICASPA AND/OR ASETV')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,913)ICASPA,ASETV,MAXCOL,MAXCP1,MAXCP2
  913     FORMAT('      ICASPA,ASETV,MAXCOL,MAXCP1,MAXCP2 = ',A4,   &
                 G15.7,3I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          ICASIF='FALS'
          GO TO 9000
        ENDIF
!
        NS=0
        ND=0
        DO 941 I=1,NIOLD
          VIJ=ASETV
!
          IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
            WRITE(9,947)I,NIOLD,ASETV,DMIN,DMAX,VIJ
  947       FORMAT('I,NIOLD,ASETV,DMIN,DMAX,VIJ = ',2I8,G15.7,3F12.5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          TARGET=VIJ
          ISTATI='FALS'
!
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
!
  941   CONTINUE
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,991)IPASS,ICASQU,DMIN,DMAX,EPS,NIOLD,NS,ND
  991     FORMAT('IPASS,ICASQU,DMIN,DMAX,EPS,NIOLD,NS,ND = ',   &
                 I8,2X,A4,3G15.7,3I8)
          CALL DPWRST('XXX','BUG ')
          DO 992 I=1,NIOLD
            WRITE(ICOUT,993)I,ISUB(I)
  993       FORMAT('I,ISUB(I) = ',I8,I8)
            CALL DPWRST('XXX','BUG ')
  992     CONTINUE
          WRITE(ICOUT,995)ITEMP,ISTATI
  995     FORMAT('ITEMP,ISTATI = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *************************************************
!               **  STEP 10--                                  **
!               **  WRITE OUT A MESSAGE FOR THIS STEP          **
!               **  INDICATING                                 **
!               **  THE SUBSET PARAMETER NAME,                 **
!               **  THE SUBSET MINIMUM,                        **
!               **  THE SUBSET MAXIMUM,                        **
!               **  THE SUBSET PARAMETER VALUE,                **
!               **  THE SUBSET PARAMETER STATUS,               **
!               *************************************************
!
        ISTEPN='10'
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
        NUMSV=IPASS
!
  300 CONTINUE
!
 1100 CONTINUE
      DO 1110 I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=00
        IF(ITEMP.EQ.10)ISUB(I)=00
        IF(ITEMP.EQ.01)ISUB(I)=00
        IF(ITEMP.EQ.11)ISUB(I)=11
 1110 CONTINUE
!
!               *************************************
!               **  STEP 11--                      **
!               **  PUT ISUB(.) IN FINAL 0,1 FORM  **
!               *************************************
!
      ISTEPN='11'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1210 I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=0
        IF(ITEMP.EQ.10)ISUB(I)=0
        IF(ITEMP.EQ.01)ISUB(I)=1
        IF(ITEMP.EQ.11)ISUB(I)=1
 1210 CONTINUE
!
!               *****************************************
!               **  STEP 12--                          **
!               **  IF THERE WERE 2 OR MORE SUBSET     **
!               **  VARIABLES, GATHER INFORMATION      **
!               **  FOR A FINAL SUMMARY MESSAGE BY     **
!               **  DETERMINING THE FINAL NUMBER OF    **
!               **  ELEMENTS IN THE SUBSET             **
!               **  (AFTER ALL VARIABLES HAVE          **
!               **  BEEN INDIVIDUALLY ACCOUNTED FOR).  **
!               *****************************************
!
      ISTEPN='12'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMSV.GT.1)THEN
        NS=0
        DO 1510 I=1,NIOLD
          IF(ISUB(I).EQ.1)NS=NS+1
 1510   CONTINUE
      ENDIF
!
!               *************************************************
!               **  STEP 13--                                  **
!               **  IF THERE WERE 2 OR MORE SUBSET VARIABLES,  **
!               **  WRITE OUT A FINAL MESSAGE                  **
!               **  SUMMARIZING FOR ALL VARIABLES              **
!               **  THE NUMBER OF SUBSET VARIABLES             **
!               **  THE INPUT NUMBER OF OBSERVATIONS (LOCAL),  **
!               **  THE NUMBER OF OBSERVATIONS IGNORED         **
!               **  AND THE OUTPUT NUMBER OF OBSERVATIONS      **
!               **  (THAT IS, THE SUBSET SAMPLE SIZE).         **
!               **  ALSO, CHECK THAT NS IS POSITIVE.           **
!               *************************************************
!
      ISTEPN='13'
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASIF='FALS'
      IF(ISUB(1).EQ.1)ICASIF='TRUE'
!
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
!
      GO TO 9000
!
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
!     CHECK FOR "NOT" STATUS
!
      IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
        WRITE(ICOUT,9001)JPASS,NPASS,NUMART,ICNT3,IWIDTT,ICASIF,IERROR
 9001   FORMAT('AT 9000: JPASS,NPASS,NUMART,ICNT3,IWIDTT,ICASIF,',   &
               'IERROR = ',5I5,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9002)IOPER(1),IOPER(2),IOPER(3),IOPER(4)
 9002   FORMAT('IOPER(1),IOPER(2),IOPER(3),IOPER(4) = ',   &
               3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(INOT.EQ.'ON')THEN
        IF(ICASIF.EQ.'TRUE')THEN
          ICASIF='FALS'
        ELSEIF(ICASIF.EQ.'FALS')THEN
          ICASIF='TRUE'
        ENDIF
      ENDIF
!
!     CHECKS FOR CASE WHEN AND/OR/XOR
!
      IF(IERROR.EQ.'YES' .OR. NPASS.LE.1)GO TO 9009
      IF(JPASS.EQ.0)THEN
        ICASI1=ICASIF
        ISHIFT=NUMART+1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGQ,IERROR)
        ICNT=0
        DO 9005 I=ILOCP2,IWIDTH
          ICNT=ICNT+1
          IANS(ICNT)=IANS(I)
          IANSLC(ICNT)=IANSLC(I)
 9005   CONTINUE
        IWIDTT=ICNT
        JPASS=1
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,9006)(IANS(JJ)(1:1),JJ=1,MIN(80,IWIDTT))
 9006     FORMAT('IANS: ',80A1)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9007)ICASI1,IWIDTT,NUMARG
 9007     FORMAT('JPASS = 0 CASE: ICASI1,IWIDTT,NUMARG = ',   &
                 A4,2X,2I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        NUMART=NUMARG
!
        GO TO 1000
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
!
        IF(IBUGQ.EQ.'ON' .OR. ISUBRO.EQ.'DPIF')THEN
          WRITE(ICOUT,9003)ICASI1,ICASI2,ICASIF
 9003     FORMAT('JPASS = 1 CASE: ICASI1,ICASI2,ICASIF = ',   &
                 2(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        ICASI1=ICASIF
        ISHIFT=NUMART+1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGQ,IERROR)
        ICNT=0
        DO 9008 I=ILOCP2,IWIDTH
          ICNT=ICNT+1
          IANS(ICNT)=IANS(I)
          IANSLC(ICNT)=IANSLC(I)
 9008   CONTINUE
        IWIDTT=ICNT
        GO TO 1000
      ENDIF
!
!  IF ERROR, THEN SET IF STATUS TO FALSE.    FEBRUARY 1999
!
!  2012/10: ADD PROMPT IF ERROR DETECTED.
!
 9009 CONTINUE
      IF(IERROR.EQ.'YES')THEN
        CALL DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,   &
                    ISUBN1,ISUBN2,ICASIF,   &
                    IBUGQ,ISUBRO,IERROR)
      ENDIF
!
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
        DO 9020 I=1,NIOLD
          WRITE(ICOUT,9021)I,ISUB(I)
 9021     FORMAT('I,ISUB(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        WRITE(ICOUT,9023)ISTATI,ICASIF,JMIN,JMAX,NUMARG
 9023   FORMAT('ISTATI,ICASIF,JMIN,JMAX,NUMARG = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPIF
      SUBROUTINE DPIMAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE AN IMAGE PLOT.  YOU CAN GENERATE EITHER
!              A GREY-SCALE IMAGE OR A FULL RGB COLOR IMAGE.
!              THE INPUT CAN BE EITHER VECTORS:
!                  RED    = VECTOR CONTAINING VALUES FOR "RED" COMPONENT
!                  BLUE   = VECTOR CONTAINING VALUES FOR "BLUE" COMPONENT
!                  GREEN  = VECToR CONTAINING VALUES FOR "GREEN" COMPONENT
!                  ROWID  = VECTOR CONTAINING THE ROW-ID
!                  COLID  = VECTOR CONTAINING THE COLUMN-ID
!
!                  GREY   = VECTOR CONTAINING VALUES FOR GREY SCALE
!                  ROWID  = VECTOR CONTAINING THE ROW-ID
!                  COLID  = VECTOR CONTAINING THE COLUMN-ID
!
!              OR MATRICES:
!                  RED    = MATRIX CONTAINING VALUES FOR "RED" COMPONENT
!                  BLUE   = MATRIX CONTAINING VALUES FOR "BLUE" COMPONENT
!                  GREEN  = MATRIX CONTAINING VALUES FOR "GREEN" COMPONENT
!
!                  GREY   = MATRIX CONTAINING VALUES FOR GREY SCALE
!
!     EXAMPLES--IMAGE PLOT GREY
!               IMAGE PLOT RED BLUE GREY
!               IMAGE PLOT GREY ROWID COLID
!               IMAGE PLOT RED BLUE GREEN ROWID COLID
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/3
!     ORIGINAL VERSION--MARCH     2008.
!     UPDATED         --MARCH     2011. USE DPPARS AND DPPAR3
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASE
      CHARACTER*4 ICASCO
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOCP.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
!---------------------------------------------------------------------
!
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
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIM'
      ISUBN2='AG  '
      IFOUND='NO'
      IERROR='NO'
!
      ICASCO='GREY'
      ICASE='VARI'
      ICASPL='IMAG'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
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
!
!               ***********************************
!               **  TREAT THE IMAGE PLOT CASE    **
!               ***********************************
!
      IFOUND='YES'
      ICASPL='IMAG'
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'IMAG')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!     IF VARIABLE ARGUMENTS GIVEN, THEN 3 TO 5 ARGUMENTS EXPECTED.
!     IF MATRIX ARGUMENTS GIVEN, THEN 1 TO 3 ARGUMENTS EXPECTED.
!
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
          GO TO 9000
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
          GO TO 9000
        ENDIF
      ENDIF
!
!     IF VARIABLE ARGUMENTS, PUT LAST TWO ARGUMENTS IN ROWID AND
!     COLUMN ID.  THE REST SHOULD GO IN YRED, YBLUE AND YGREEN.
!
      ICASCO='COLO'
      IF(IVARTY(1).EQ.'VARI')THEN
        IF(NUMVAR.LE.3)ICASCO='GREY'
        NUMVA2=NUMVAR-2
        ICOL=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    YRED,YBLUE,YGREEN,NS,NTEMP,NTEMP,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        NUMVA2=2
        ICOL=NUMVAR-1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    ROWID,COLID,COLID,NS,NTEMP,NTEMP,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
      ELSEIF(IVARTY(1).EQ.'MATR')THEN
!
        IF(NUMVAR.LE.1)ICASCO='GREY'
        DO 301 I=1,MAXOBV
          ROWID(I)=0.0
          COLID(I)=0.0
          YRED(I)=0.0
          YBLUE(I)=0.0
          YGREEN(I)=0.0
  301   CONTINUE
!
        ILISR=1
        ICOL31=IVALUE(ILISR)
        ICOL32=IVALU2(ILISR)
        NROW=IN(ILISR)
        NCOL=(ICOL32 - ICOL31) + 1
        ICNT=0
        DO 310 JCOL=1,NCOL
          DO 320 IROW=1,NROW
            ICNT=ICNT+1
            ROWID(ICNT)=REAL(IROW)
            COLID(ICNT)=REAL(JCOL)
  320     CONTINUE
  310   CONTINUE
!
        ICOL=1
        NUMVA2=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    YRED,YRED,YRED,NS,NTEMP,NTEMP,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(NUMVAR.GE.2)THEN
          ICOL=2
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      YBLUE,YBLUE,YBLUE,NS,NTEMP,NTEMP,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(NUMVAR.GE.3)THEN
          ICOL=3
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      YGREEN,YGREEN,YGREEN,NS,NTEMP,NTEMP,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
      ENDIF
!
!               ********************************************************
!               **  STEP 61--                                          *
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS VARIABLES    *
!               **  (Y(.) AND X(.), RESPECTIVELY) FOR THE PLOT.        *
!               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .        *
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).      *
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).      *
!               ********************************************************
!
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'IMAG')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASCO.EQ.'GREY')ICASPL='IMA2'
      CALL DPIMA2(YRED,YBLUE,YGREEN,YALPHA,ROWID,COLID,NS,   &
                  ICASCO,PCOLMX,   &
                  TEMP1,TEMP2,MAXOBV,   &
                  Y,X,D,DCOLOR,DFILL,DSYMB,DSIZE,NPLOTP,NPLOTV,   &
                  IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9013   FORMAT('NPLOTV,NPLOTP,NS,MAXN,IAND1,IAND2 = ',   &
               4I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO 9020 I=1,MIN(NPLOTP,200)
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
          DO 9030 I=1,MIN(NPLOTP,200)
            WRITE(ICOUT,9031)I,DCOLOR(I),DFILL(I),DSYMB(I)
 9031       FORMAT('I,DCOLOR(I),DFILL(I),DSYMB(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9030     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPIMAG
      SUBROUTINE DPIMA2(YRED,YBLUE,YGREEN,YALPHA,ROWID,COLID,N,   &
                        ICASCO,PCOLMX,   &
                        TEMP1,TEMP2,MAXOBV,   &
                        Y2,X2,D2,DCOLOR,DFILL,DSYMB,DSIZE,   &
                        NPLOTP,NPLOTV,   &
                        IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--FORM A IMAGE PLOT.  THE X AND Y COORDINATES WILL
!              BE ROW AND COLUMN ID'S, RESPECTIVELY.  THE
!              RGB COLORS CORRESPONDING TO EACH ROWID/COLUMM ID
!              WILL BE CONTAINED IN DCOLOR, DFILL, AND DSYMB
!              (DSIZE IS BEING RESERVED FOR AN "ALPHA" CHANNEL,
!              THE ALPHA CHANNEL IS NOT YET IMPLEMENTED, BUT
!              IS BEING RESERVED FOR FUTURE IMPLEMENTATION).
!              GREYSCALE IMAGES WILL ONLY USE DCOLOR.
!
!
!              COLORS WILL BE SCALED TO A (0,1) SCALE (THE
!              ROUTINES THAT ACTUALLY RENDER THE IMAGE WILL
!              CONVERT TO THE APPROPRIATE RESOLUTION FOR A
!              SPECIFIC DEVICE).
!     EXAMPLE--IMAGE PLOT RED BLUE GREEN ROWID COLID
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/3
!     ORIGINAL VERSION--MARCH     2008.
!
!-----COMMON----------------------------------------------------------
!
!---------------------------------------------------------------------
!
      CHARACTER*4 ICASCO
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRITE
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'IMA2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPIMA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,ICASCO,N,PCOLMX
   52   FORMAT('IBUGG3,ICASCO,ISUBRO,N,PCOLMX = ',3(A4,2X),I8,F10.5)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,MIN(N,100)
          WRITE(ICOUT,56)I,YRED(I),YBLUE(I),YGREEN(I),YALPHA(I),   &
                         ROWID(I),COLID(I)
   56     FORMAT('I,YRED(I),YBLUE(I),YGREEN(I),YALPHA(I),ROWID(I),',   &
                 'COLID(I) = ',I8,6G12.4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************************
!               **  STEP 1--                                         **
!               **  CREATE RED, BLUE, AND GREEN COMPONENTS           **
!               *******************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'IMA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     NOTE: IMAGE WILL BE DRAWN FROM TOP TO BOTTOM,
!           LEFT TO RIGHT.  SO CODE X2 AND Y2 APPROPRIATELY.
!
      IWRITE='OFF'
      CALL CODE(ROWID,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
      DO 910 I=1,N
        ROWID(I)=TEMP1(I)
  910 CONTINUE
      CALL CODE(COLID,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
      DO 920 I=1,N
        COLID(I)=TEMP1(I)
  920 CONTINUE
!
      AMAX=CPUMIN
      DO 1000 I=1,N
!
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
!
 1000 CONTINUE
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  NOW SCALE TO (0,1) SCALE                         **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'IMA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      AMAX=MAX(AMAX,PCOLMX)
      DO 2000 I=1,N
!
        IF(ICASCO.EQ.'GREY')THEN
          DCOLOR(I)=DCOLOR(I)/AMAX
        ELSE
          DCOLOR(I)=DCOLOR(I)/AMAX
          DFILL(I)=DFILL(I)/AMAX
          DSYMB(I)=DSYMB(I)/AMAX
        ENDIF
!
 2000 CONTINUE
!
      NPLOTP=N
      NPLOTV=2
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'IMA2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIMA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTP,NPLOTV
 9013   FORMAT('NPLOTP,NPLOTV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO 9015 I=1,MIN(200,NPLOTP)
            WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016       FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3F10.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
          DO 9025 I=1,MIN(200,NPLOTP)
            WRITE(ICOUT,9026)I,DCOLOR(I),DFILL(I),DSYMB(I),DSIZE(I)
 9026       FORMAT('I,DCOLOR(I),DFILL(I),DSYMB(I),DSIZE(I) = ',   &
                   I8,4G12.4)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPIMA2
      SUBROUTINE DPIMP1(IX2TSW,IY2TSW,IX2ZSW,IY2ZSW,NCY2LA,   &
      IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--THIS IS IMPLEMENTATION MODULE NUMBER 1.
!              THIS WILL RESULT IN--
!                 1) NO TIC MARKS OR TIC MARK LABELS ON UPPER FRAME LINE
!                 2) NO TIC MARKS OR TIC MARK LABELS ON RIGHT FRAME LINE
!                 3) NO VERTICAL LABEL (Y2LABEL) ON RIGHT FRAME LINE
!     NOTE--THIS SUBROUTINE WILL BE EXECUTED WHEN THE
!           ANALYST ENTERS THE COMMAND--
!                   IMPLEMENT 1
!     NOTE--THE IMPLEMENT COMMAND IS USEFUL FOR IMPLEMENTATION ,DEBUGGING,
!           AND FOR NON-STANDARD CONVENTIONS (E.G., PLOTS WITH NON-STANDARD
!           SIZE OR TIC MARK CONVENTIONS OTHER THAN DATAPLOT'S DEFAULT)
!     INPUT  ARGUMENTS--NONE
!     OUTPUT ARGUMENTS--
!                     --IX2TSW
!                     --IY2TSW
!                     --IX2ZSW
!                     --IY2ZSW
!                     --NCY2LA
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--OCTOBER   1981.
!     UPDATED         --APRIL     1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGS2
!
      CHARACTER*4 IX2TSW
      CHARACTER*4 IY2TSW
!
      CHARACTER*4 IX2ZSW
      CHARACTER*4 IY2ZSW
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='YES'
!
      IF(IBUGS2.EQ.'OFF')GO TO 90
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
!               ***********************************************
!               **  STEP 1--                                 **
!               **  DEFINE PARAMETER CHANGES TO BE MADE      **
!               **  FOR THIS IMPLEMENTATION MODULE NUMBER 1  **
!               ***********************************************
!
      IX2TSW='ON'
      IY2TSW='ON'
!
      IX2ZSW='ON'
      IY2ZSW='ON'
!
!CCCC NCY2LA=0
!
!               ***************************
!               **  STEP 2--             **
!               **  WRITE OUT A MESSAGE. **
!               ***************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIMP1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IX2TSW,IY2TSW,IX2ZSW,IY2ZSW
 9012   FORMAT('IX2TSW,IY2TSW,IX2ZSW,IY2ZSW = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NCY2LA
 9014   FORMAT('NCY2LA = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPIMP1
      SUBROUTINE DPIMP2(ANUMVP,ANUMHP,   &
      ISQUAR,   &
      PXMIN,PYMIN,PXMAX,PYMAX,   &
      IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--THIS IS IMPLEMENTATION MODULE NUMBER 2.
!              THIS WILL RESULT IN--
!                    THE PLOT FRAME CHANGED FROM RECTANGULAR
!                    TO SQUARE FOR ALL FUTURE PLOTS
!                    ON TEKTRONIX GRAPHICS DEVICES.
!     NOTE--THIS SUBROUTINE WILL BE EXECUTED WHEN THE
!           ANALYST ENTERS THE COMMAND--
!                   IMPLEMENT 2
!     NOTE--THE IMPLEMENT COMMAND IS USEFUL FOR IMPLEMENTATION ,DEBUGGING,
!           AND FOR NON-STANDARD CONVENTIONS (E.G., PLOTS WITH NON-STANDARD
!           SIZE OR TIC MARK CONVENTIONS OTHER THAN DATAPLOT'S DEFAULT)
!     INPUT  ARGUMENTS--NONE
!     OUTPUT ARGUMENTS--
!                     --PXMIN
!                     --PYMIN
!                     --PXMAX
!                     --PYMAX
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--OCTOBER   1981.
!     UPDATED         --APRIL     1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISQUAR
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='YES'
!
      IF(IBUGS2.EQ.'OFF')GO TO 90
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
!
!               ***********************************************
!               **  STEP 1--                                 **
!               **  DEFINE PARAMETER CHANGES TO BE MADE      **
!               **  FOR THIS IMPLEMENTATION MODULE NUMBER 2  **
!               ***********************************************
!
      ISQUAR='ON'
!
!CCCC PXMIN=15.0
!CCCC PYMIN=20.0
!CCCC PYMAX=90.0
!
!CCCC PYDEL=PYMAX-PYMIN
!CCCC PXDEL=PYDEL*(ANUMVP/ANUMHP)
!CCCC PXMAX=PXMIN+PXDEL
!
!               ***************************
!               **  STEP 2--             **
!               **  WRITE OUT A MESSAGE. **
!               ***************************
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIMP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ISQUAR,ANUMVP,ANUMHP
 9012   FORMAT('ISQUAR,ANUMVP,ANUMHP = ',A4,2X,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)PXMIN,PXMAX,PYMIN,PYMAX
 9014   FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)IBUGS2,IFOUND,IERROR
 9019   FORMAT('IBUGS2,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPIMP2
      SUBROUTINE DPIMPL(IHARG,IARGT,IARG,NUMARG,   &
                        IX2TSW,IY2TSW,IX2ZSW,IY2ZSW,NCY2LA,   &
                        ISQUAR,   &
                        PXMIN,PYMIN,PXMAX,PYMAX,   &
                        IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--REINITIALIZE A SET OF UNDERLYING
!              FORTRAN PARAMETERS SO AS TO ACHIEVE
!              ALTERNATE SETTINGS FOR SUCH PARAMETERS.
!     NOTE--THIS CAPABILITY IS USEFUL FOR IMPLEMENTATION ,DEBUGGING,
!           AND FOR NON-STANDARD CONVENTIONS (E.G., PLOTS WITH NON-STANDARD
!           SIZE OR NO TIC MARKS ON UPPER AND RIGHT FRAME).
!     INPUT  ARGUMENTS--
!                     --IHARG
!                     --IARGT
!                     --IARG
!                     --NUMARG
!                     --IBUGS2
!     OUTPUT ARGUMENTS--
!                     --IX2TSW
!                     --IY2TSW
!                     --IX2ZSW
!                     --IY2ZSW
!                     --NCY2LA
!
!                     --PXMIN
!                     --PYMIN
!                     --PXMAX
!                     --PYMAX
!
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--OCTOBER   1981.
!     UPDATED         --APRIL     1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISQUAR
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
!
      CHARACTER*4 IX2TSW
      CHARACTER*4 IY2TSW
!
      CHARACTER*4 IX2ZSW
      CHARACTER*4 IY2ZSW
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IHOLD=(-999)
      IMPLNU=(-999)
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1050
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1050
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1050
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1050
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1050
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1060
      GO TO 1040
!
 1040 CONTINUE
      IF(IHARG(NUMARG).EQ.'TICS')GO TO 1100
      IF(IHARG(NUMARG).EQ.'SQUA')GO TO 1200
      GO TO 8000
!
 1050 CONTINUE
      IHOLD=0
      GO TO 1070
!
 1060 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1070
!
 1070 CONTINUE
      IFOUND='YES'
      IMPLNU=IHOLD
!
      IF(IMPLNU.EQ.1)GO TO 1100
      IF(IMPLNU.EQ.2)GO TO 1200
      GO TO 8000
!
 1100 CONTINUE
      CALL DPIMP1(IX2TSW,IY2TSW,IX2ZSW,IY2ZSW,NCY2LA,   &
      IBUGS2,IFOUND,IERROR)
      GO TO 9000
!
 1200 CONTINUE
      CALL DPIMP2(ANUMVP,ANUMHP,   &
      ISQUAR,   &
      PXMIN,PYMIN,PXMAX,PYMAX,   &
      IBUGS2,IFOUND,IERROR)
      GO TO 9000
!
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
      GO TO 9000
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPIMPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IMPLNU,IHOLD
 9016   FORMAT('IMPLNU,IHOLD = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9029)IBUGS2,IFOUND,IERROR
 9029   FORMAT('IBUGS2,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPIMPL
      SUBROUTINE DPINCU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        MAXNXT,   &
                        ISEED,   &
                        ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE ONE OF THE FOLLOWING INFLUENCE CURVES--
!              AN INFLUENCE CURVE IS A MEASURE OF ROBUSTNESS.
!              IT PLOTS THE VALUE OF A STATISTIC WHEN ONE ADDITIONAL
!              VALUE IS ADDED.  FOR EXAMPLE,
!                 MEAN INFLUENCE CURVE Y XSEQ
!              CYCLES THROUGH THE POINTS IN XSEQ.  THE VERTICAL
!              AXIS IS THE VALUE OF THE MEAN FOR THE POINTS IN Y
!              WITH THE SINGLE VALUE IN XSEQ ADDED TO Y.
!
!              FOR THIS PLOT, ONLY ONE VARIABLE STATISTICS ARE
!              SUPPORTED (I.E., NO CORRELATION, ETC.).
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
!     VERSION NUMBER--2002/7
!     ORIGINAL VERSION--JULY      2002.
!     UPDATED         --MAY       2007. TRIMMED STANDARD DEVIATION
!     UPDATED         --AUGUST    2007. MOVE SOME ARRAY STORAGE TO
!                                       COMMON
!     UPDATED         --NOVEMBER  2007. DOUBLE PRECISION ARRAYS FOR
!                                       CMPSTA
!     UPDATED         --NOVEMBER  2007. LP LOCATION
!     UPDATED         --NOVEMBER  2007. VARIANCE LP LOCATION
!     UPDATED         --NOVEMBER  2007. SD LP LOCATION
!     UPDATED         --SEPTEMBER 2008. BINOMIAL PROBABILITY
!     UPDATED         --FEBRUARY  2009. GRUBB
!     UPDATED         --FEBRUARY  2009. ONE SAMPLE T TEST
!     UPDATED         --FEBRUARY  2009. CHI-SQUARE SD TEST
!     UPDATED         --FEBRUARY  2009. FREQUENCY TEST
!     UPDATED         --FEBRUARY  2009. FREQUENCY WITHIN A BLOCK TEST
!     UPDATED         --MARCH     2009. PARSE WITH "EXTSTA"
!     UPDATED         --MARCH     2011. USE DPPARS AND DPPAR3
!     UPDATED         --MARCH     2011. SUPPORT MULTIPLE CURVES (BUT
!                                       NOT REPLICATION)
!     UPDATED         --AUGUST    2023. CALL LIST TO EXTSTA, CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4  ISTADF
      CHARACTER*4  ISTARA
      CHARACTER*60 ISTANM
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
!
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XTEMP4(MAXOBV)
      DIMENSION XTEMP5(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y1(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP3(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB6),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB7),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB8),XTEMP4(1))
      EQUIVALENCE (GARBAG(IGARB9),XTEMP5(1))
!
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
!
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
!
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='INCU'
      ISUBN2='    '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      IMAXIN=0
      IMININ=0
!
!               **************************************
!               **  TREAT THE INFLUENCE CURVE CASE  **
!               **************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINCU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ
   52   FORMAT('ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPINCU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE IF OF THIS TYPE  **
!               **  AND BRANCH ACCORDINGLY.    **
!               *********************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.AND.ISUBRO.NE.'INCU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
!
!     MARCH 2009: USE EXTSTA TO PARSE STATISTIC
!
      JMIN=0
      JMAX=NUMARG
!
      DO 200 I=1,NUMARG
        IF(IHARG(I).EQ.'INFL')THEN
          JMAX=I-1
          ILASTC=I+1
          GO TO 209
        ENDIF
  200 CONTINUE
      IFOUND='NO'
      GO TO 9000
  209 CONTINUE
!
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,   &
                  ICASPL,ISTANM,ISTANR,ISTADF,ISTARA,   &
                  IFOUND,ILOCV,ISUBRO,IBUGG3,IERROR)
!
      IF(ISTANR.GE.2)IFOUND='NO'
      IF(IFOUND.EQ.'NO')GO TO 9000
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!     EXTRACT THE "SEQUENCE" VARIABLE.
!
      ICOL=NUMVAR
      NUMVA2=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  X1,X1,X1,NX,NX,NX,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***********************************************
!               **  STEP 8A--                                **
!               **  MULTIPLE RESPONSE VARIABLES.  THESE CAN  **
!               **  BE EITHER VARIABLE OR MATRIX ARGUMENTS.  **
!               ***********************************************
!
      ISTEPN='8A'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
      NPLOTP=0
      NUMVA2=1
      NUMVA3=2
!
      DO 810 IRESP=1,NUMVAR-1
        ICOL=IRESP
        NCURVE=IRESP
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,Y1,Y1,NY,NY,NY,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               *******************************************************
!               **  STEP 8B--                                        **
!               **  COMPUTE THE APPROPRIATE INFLUENCE CURVE --       **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS            **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.               **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).    **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).    **
!               *******************************************************
!
        ISTEPN='8B'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INCU')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        CALL DPINC2(Y1,X1,NX,NY,NUMVA3,ICASPL,ISTARA,ISIZE,ICONT,   &
                    TEMP,TEMP2,XTEMP1,XTEMP2,XTEMP3,   &
                    XTEMP4,XTEMP5,MAXNXT,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    Y,X,D,NPLOTP,NPLOTV,NCURVE,   &
                    ISUBRO,IBUGG3,IERROR)
!
!               *************************************************
!               **  STEP 29--                                  **
!               **  SAVE DIFFERENCE BETWEEN HIGHEST VALUE AND  **
!               **  LOWEST VALUE OF STATISTIC IN INTERNAL      **
!               **  PARAMETER ALOWHIGH                         **
!               *************************************************
!
!       CURRENTLY, ONLY DO THIS FOR FIRST CURVE.
!
        IF(NCURVE.EQ.1)THEN
          AMINS=CPUMAX
          AMAXS=CPUMIN
          DO 2910 I=1,NPLOTP
            IF(D(I).NE.1.0)GO TO 2910
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
!
          ISUBN0='INCU'
!
          IH='ALOW'
          IH2='HIGH'
          VALUE0=ADIFF
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGG3,IERROR)
        ENDIF
!
  810  CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(IFOUND.EQ.'YES'.AND.NPLOTP.GT.0)THEN
          DO 9025 I=1,NPLOTP
            WRITE(ICOUT,9026)I,Y(I),X(I),D(I)
 9026       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPINCU
      SUBROUTINE DPINC2(Y,X,NX,NY,NUMV2,ICASPL,ISTARA,ISIZE,ICONT,   &
                        TEMP,TEMPZ,XTEMP1,XTEMP2,XTEMP3,   &
                        XTEMP4,XTEMP5,MAXNXT,   &
                        ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        Y2,X2,D2,N2,NPLOTV,NCURVE,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--GENERATE ONE OF THE FOLLOWING INFLUENCE CURVES--
!              AN INFLUENCE CURVE IS A MEASURE OF ROBUSTNESS.
!              IT PLOTS THE VALUE OF A STATISTIC WHEN ONE ADDITIONAL
!              VALUE IS ADDED.  FOR EXAMPLE,
!                 MEAN INFLUENCE CURVE Y XSEQ
!              CYCLES THROUGH THE POINTS IN XSEQ.  THE VERTICAL
!              AXIS IS THE VALUE OF THE MEAN FOR THE POINTS IN Y
!              WITH THE SINGLE VALUE IN XSEQ ADDED TO Y.
!
!              FOR THIS PLOT, ONLY ONE VARIABLE STATISTICS ARE
!              SUPPORTED (I.E., NO CORRELATION, ETC.).
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--MOSTELLER AND TUKEY, "EXPLORATORY DATA ANALYSIS".
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2002/7
!     ORIGINAL VERSION--JULY      2002.
!     UPDATED         --AUGUST    2002. USE CMPSTA TO COMPUTE THE
!                                       STATISTIC.
!     UPDATED         --APRIL     2003. ADD SN AND QN.  REQUIRED
!                                       ADDITIONAL SCRATCH ARRAYS
!     UPDATED         --NOVEMBER  2007. DOUBLE PRECISION ARRAYS FOR
!                                       CMPSTA
!     UPDATED         --NOVEMBER  2007. LP LOCATION
!     UPDATED         --NOVEMBER  2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ISTARA
      CHARACTER*4 ICONT
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION XTEMP4(*)
      DIMENSION XTEMP5(*)
      DIMENSION ITEMP1(*)
      DIMENSION ITEMP2(*)
      DIMENSION ITEMP3(*)
      DIMENSION ITEMP4(*)
      DIMENSION ITEMP5(*)
      DIMENSION ITEMP6(*)
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIN'
      ISUBN2='C2  '
      IWRITE='OFF'
!
      I2=0
      ISIZE2=0
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(NY.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN INFLUENCE CURVE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE MUST BE AT LEAST 2.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)NY
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
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
        DO 73 I=1,MAX(NY,NX)
          WRITE(ICOUT,74)I,Y(I),X(I)
   74     FORMAT('I, Y(I),X(I) = ',I8,2F15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  SORT THE HORIZONTAL AXIS VARIABLE, EXTRACT        **
!               **  THE DISTINCT VALUES.                              **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
      CALL SORT(X,NX,X)
      CALL DISTIN(X,NX,IWRITE,X,NXDIST,IBUGG3,IERROR)
!
!               ******************************************
!               **  STEP 2--                            **
!               **  COMPUTE THE SPECIFIED STATISTIC     **
!               **  FOR EACH DISTINCT VALUE OF X ADDED  **
!               **  TO THE Y VARIABLE.                  **
!               ******************************************
!
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
!
      DO 11000 ISET=1,NXDIST
!
      ILAST=NY+1
      DO 11011 I=1,NY
        TEMP(I)=Y(I)
11011 CONTINUE
      TEMP(ILAST)=X(ISET)
      NS2=ILAST
!
      CALL CMPSTA(TEMP,TEMPZ,TEMPZ,XTEMP1,XTEMP2,XTEMP3,   &
                  XTEMP4,XTEMP5,   &
                  MAXNXT,NS2,NS2,NS2,NUMV2,ICASPL,ISTARA,   &
                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                  DTEMP1,DTEMP2,DTEMP3,   &
                  RIGHT,   &
                  ISUBRO,IBUGG3,IERROR)
!
!     ---------------------------
!
      N2=N2+1
      Y2(N2)=RIGHT
      X2(N2)=X(ISET)
      D2(N2)=REAL(NCURVE)
!
11000 CONTINUE
      NPLOTV=3
      GO TO 9000
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINC2--')
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPINC2
      SUBROUTINE DPIND2(X1,Y1,X2,Y2,PX,PY,   &
                        IFIG,ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!     PURPOSE--DRAW A INDUCTOR WITH ONE END AT (X1,Y1)
!              AND THE OTHER END AT (X2,Y2).
!     NOTE--THE HEIGHT OF EACH LOOP IS PTEXHE.
!           THE WIDTH  OF EACH LOOP IS PTEXWI.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
!     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPINDU
!                                       RATHER THAN DPIND2
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      DIMENSION PX(*)
      DIMENSION PY(*)
!
      CHARACTER*4 IFIG
!
      CHARACTER*4 ILINPA
      CHARACTER*4 ILINCO
!
      CHARACTER*4 IREBLI
      CHARACTER*4 IREBCO
      CHARACTER*4 IREFSW
      CHARACTER*4 IREFCO
      CHARACTER*4 IREPTY
      CHARACTER*4 IREPLI
      CHARACTER*4 IREPCO
!
      CHARACTER*4 IPATT
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
!
      DIMENSION ILINPA(*)
      DIMENSION ILINCO(*)
      DIMENSION ILINC2(MAXLN,3)
      DIMENSION PLINTH(*)
!
      DIMENSION AREGBA(*)
      DIMENSION IREBLI(*)
      DIMENSION IREBCO(*)
      DIMENSION IREBC2(MAXRG,3)
      DIMENSION PREBTH(*)
      DIMENSION IREFSW(*)
      DIMENSION IREFCO(*)
      DIMENSION IREFC2(MAXRG,3)
      DIMENSION IREPTY(*)
      DIMENSION IREPLI(*)
      DIMENSION IREPCO(*)
      DIMENSION IREPC2(MAXRG,3)
      DIMENSION PREPTH(*)
      DIMENSION PREPSP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IPATT=ILINPA(1)
      PTHICK=PLINTH(1)
      ICOL=ILINCO(1)
!
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
        WRITE(ICOUT,63)IREBLI(1),IREBCO(1),IREBC2(1,1),PREBTH(1)
   63   FORMAT('IREBLI(1),IREBCO(1),IREBC2(1,1),PREBTH(1) = ',   &
               2(A4,2X),I5,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IREFSW(1),IREFCO(1),IREFC2(1,1),IREPC2(1,1)
   64   FORMAT('IREFSW(1),IREFCO(1),IREFC2(1,1),IREPC2(1,1) = ',   &
               2(A4,2X),2I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65   FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',   &
               3(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PTEXHE,PTEXWI,PTEXVG,PTEXHG
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE FIGURE             **
!               *********************************
!
      DELX=X2-X1
      DELY=Y2-Y1
      ALEN=0.0
      TERM=(X2-X1)**2+(Y2-Y1)**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
!
      AJXMIN=PTEXWI
      AJXDEL=PTEXWI
      AJYDEL=PTEXHE
      AJXMAX=ALEN-2*AJXDEL
!
      XMIN=AJXMIN
      XDEL=AJXDEL
      YDEL=AJYDEL
      XMAX=AJXMAX
!
      K=0
!
      X=0
      Y=0
      K=K+1
      PX(K)=X1
      PY(K)=Y1
!
      X=XMIN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      AJX=AJXMIN-AJXDEL
!CCCC DO1450JX=JXMIN,JXMAX,JXDEL
 1440 CONTINUE
      AJX=AJX+AJXDEL
      IF(AJX.GT.AJXMAX)GO TO 1460
!
      X=AJX
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      AJX3=XP
      AJY3=YP
!
      X=AJX+AJXDEL
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      AJX4=XP
      AJY4=YP
!
      ICOLR=ILINC2(1,1)
      ICOLG=ILINC2(1,2)
      ICOLB=ILINC2(1,3)
      CALL DPIND3(AJX3,AJY3,AJX4,AJY4,PX,PY,K,   &
                  IFIG,IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB)
!
      GO TO 1440
!
 1460 CONTINUE
!
!CCCC X=XMAX
      X=ALEN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      NP=K
!
!               ***********************
!               **  STEP 2--         **
!               **  FILL THE FIGURE  **
!               **  (IF CALLED FOR)  **
!               ***********************
!
!CCCC IF(IREFSW(1).EQ.'OFF')GO TO 2190
!CCCC IPATT=IREPTY(1)
!CCCC PTHICK=PREPTH(1)
!CCCC PXGAP=PREPSP(1)
!CCCC PYGAP=PREPSP(1)
!CCCC ICOLF=IREFCO(1)
!CCCC ICOLP=IREPCO(1)
!CCCC CALL DPFIRE(PX,PY,NP,
!CCCC1            IFIG,IPATT,PTHICK,PXGAP,PYGAP,ICOLF,ICOLP)
!2190 CONTINUE
!
!               ***************************
!               **  STEP 3--             **
!               **  DRAW OUT THE FIGURE  **
!               ***************************
!
      IPATT=ILINPA(1)
      PTHICK=PLINTH(1)
      ICOL=ILINCO(1)
      IFLAG='ON'
      CALL DPDRPL(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,   &
                  ICOL,ICOLR,ICOLG,ICOLB,   &
                  JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'IND2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIND2--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9039)IERRG4
 9039   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPIND2
      SUBROUTINE DPIND3(X1,Y1,X2,Y2,PX,PY,K,   &
                        IFIG,IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB)
!
!     PURPOSE--DRAW A SEMI-CIRCLE FOR AN INDUCTOR
!              WITH ONE END OF THE DIAGONAL AT (X1,Y1)
!              AND THE OTHER END AT (X2,Y2).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1989.  MODIFY CALLS TO DPDRPL (ALAN)
!
!-----NON-COMMON VARIABLES-----------------------------------------
!
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
!
      DIMENSION PX(*)
      DIMENSION PY(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'IND3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPIND3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)X1,Y1,X2,Y2,K
   53   FORMAT('X1,Y1,X2,Y2,K = ',4G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DELX=X2-X1
      DELY=Y2-Y1
      ALEN=0.0
      TERM=(X2-X1)**2+(Y2-Y1)**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      R=ALEN/2.0
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
!
      X=0.0
      Y=0.0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      DO 3010 I=1,181,5
      IREV=181-I+1
      PHI2=IREV-1
      PHI2=PHI2*(2.0*3.1415926)/360.0
      X=R*COS(PHI2)+R
      Y=R*SIN(PHI2)
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      IF(K.LE.490)GO TO 3010
      NP=K
      IFLAG='ON'
      CALL DPDRPL(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,   &
                  ICOL,ICOLR,ICOLG,ICOLB,   &
                  JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
      K=0
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
 3010 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'IND3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPIND3--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,K
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9039)IBUGG4,ISUBG4,IERRG4
 9039   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPIND3
      SUBROUTINE DPINDM(Y1,N1,Y2,N2,ICASE,   &
                        STATVA,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE IMPLEMENTS THE FOLLOWING COMMANDS:
!
!                 LET A = INDEX FIRST MATCH Y1 Y2
!                 LET A = INDEX LAST  MATCH Y1 Y2
!                 LET A = INDEX FIRST NOT MATCH Y1 Y2
!                 LET A = INDEX LAST  NOT MATCH Y1 Y2
!
!              THAT IS, RETURN THE INDEX OF EITHER THE FIRST OR LAST
!              MATCHING (OR NON-MATCHING) ENTRIES FOR TWO ARRAYS.
!              NOTE THAT THE INPUT ARRAYS NEED NOT BE OF THE SAME
!              SIZE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/11
!     ORIGINAL VERSION--NOVEMBER  2011
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIN'
      ISUBN2='DM  '
      IERROR='NO'
!
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
        DO 56 I=1,N1
          WRITE(ICOUT,57)I,Y1(I)
   57     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        DO 66 I=1,N2
          WRITE(ICOUT,67)I,Y2(I)
   67     FORMAT('I,Y2(I) = ',I8,E15.7)
          CALL DPWRST('XXX','WRIT')
   66   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INDM')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN INDEX ... MATCH')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE FIRST ',   &
               'RESPONSE VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1115)N1
 1115   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1123)
 1123   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE SECOND ',   &
               'RESPONSE VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1115)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************************
!               **  STEP 21--               **
!               **  DETERMINE THE INDEX     **
!               ******************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INDM')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      STATVA=0.0
!
      IF(ICASE.EQ.'FMAT')THEN
        DO 100 I=1,MIN(N1,N2)
          IF(Y1(I).EQ.Y2(I))THEN
            STATVA=REAL(I)
            GO TO 9000
          ENDIF
  100   CONTINUE
      ELSEIF(ICASE.EQ.'LMAT')THEN
        DO 200 I=MIN(N1,N2),1,-1
          IF(Y1(I).EQ.Y2(I))THEN
            STATVA=REAL(I)
            GO TO 9000
          ENDIF
  200   CONTINUE
      ELSEIF(ICASE.EQ.'FNOM')THEN
        DO 300 I=1,MIN(N1,N2)
          IF(Y1(I).NE.Y2(I))THEN
            STATVA=REAL(I)
            GO TO 9000
          ENDIF
  300   CONTINUE
        IF(N1.NE.N2)STATVA=REAL(MIN(N1,N2)+1)
      ELSEIF(ICASE.EQ.'LNOM')THEN
        DO 400 I=MIN(N1,N2),1,-1
          IF(Y1(I).NE.Y2(I))THEN
            STATVA=REAL(I)
            GO TO 9000
          ENDIF
  400   CONTINUE
        IF(N1.NE.N2)STATVA=REAL(MAX(N1,N2))
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPINDM
      SUBROUTINE DPINDU(IHARG,IARGT,ARG,NUMARG,   &
                        PXSTAR,PYSTAR,PXEND,PYEND,   &
                        ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG,   &
                        IGRASW,IDIASW,   &
                        PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,   &
                        PDIAHE,PDIAWI,PDIAVG,PDIAHG,   &
                        NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                        IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                        IDNVOF,IDNHOF,IDFONT,UNITSW,PDSCAL,   &
                        IBACCO,IBACC2,   &
                        IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DRAW ONE OR MORE INDUCTORS (DEPENDING ON HOW MANY NUMBERS ARE
!              PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
!              OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE THE 2 ENDS OF THE INDUCTOR.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2
!          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
!     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN INDUCTOR WILL GO FROM THE
!           LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER ABSOLUTE OR
!           RELATIVE) AS DEFINED BY THE 2 NUMBERS.
!     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN INDUCTOR WILL GO FROM THE
!           ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST 2 NUMBERS TO THE
!           (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE THIRD
!           AND FOURTH NUMBERS.
!     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN INDUCTOR WILL GO FROM THE
!           (X,Y) POSITION AS RESULTING FROM THE THIRD AND FOURTH NUMBERS TO
!           THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
!           FIFTH AND SIXTH NUMBERS.
!     NOTE--AND SO FORTH FOR 8, 10, 12, ... NUMBERS.
!     INPUT  ARGUMENTS--IHARG
!                     --IARGT
!                     --ARG
!                     --NUMARG
!                     --PXSTAR
!                     --PYSTAR
!     OUTPUT ARGUMENTS--PXEND
!                     --PYEND
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --NOVEMBER  1982.
!     UPDATED         --JANUARY   1989. CALL LIST FOR OFFSET VAR (ALAN)
!     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
!     UPDATED         --JULY      1997. SUPPORT FOR "DATA" UNITS (ALAN)
!     UPDATED         --DECEMBER  2018. CHECK FOR DISCRETE, NULL, OR
!                                       NONE DEVICE
!     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
!                                       COMMAND
!     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPINDU
!                                       RATHER THAN DPIND2
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLORS
!
!-----NON-COMMON VARIABLES-----------------------------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
!
      CHARACTER*4 ILINPA
      CHARACTER*4 ILINCO
!
      CHARACTER*4 IREBLI
      CHARACTER*4 IREBCO
      CHARACTER*4 IREFSW
      CHARACTER*4 IREFCO
      CHARACTER*4 IREPTY
      CHARACTER*4 IREPLI
      CHARACTER*4 IREPCO
!
      CHARACTER*4 IGRASW
      CHARACTER*4 IDIASW
!
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDFONT
      CHARACTER*4 UNITSW
      CHARACTER*4 IDCOLO
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IBUGD2
      CHARACTER*4 IERROR
      CHARACTER*4 ISUBRO
!
      CHARACTER*4 IFIG
      CHARACTER*4 IBELSW
      CHARACTER*4 IERASW
      CHARACTER*4 IBACCO
      CHARACTER*4 ICOPSW
      CHARACTER*4 ITYPEO
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
!
      DIMENSION ILINPA(*)
      DIMENSION ILINCO(*)
      DIMENSION ILINC2(MAXLNZ,3)
      DIMENSION PLINTH(*)
!
      DIMENSION AREGBA(*)
      DIMENSION IREBLI(*)
      DIMENSION IREBCO(*)
      DIMENSION IREBC2(MAXRGZ,3)
      DIMENSION PREBTH(*)
      DIMENSION IREFSW(*)
      DIMENSION IREFCO(*)
      DIMENSION IREFC2(MAXRGZ,3)
      DIMENSION IREPTY(*)
      DIMENSION IREPLI(*)
      DIMENSION IREPCO(*)
      DIMENSION IREPC2(MAXRGZ,3)
      DIMENSION PREPTH(*)
      DIMENSION PREPSP(*)
      DIMENSION PDSCAL(*)
!
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
      DIMENSION IBACC2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      DIMENSION PX(1000)
      DIMENSION PY(1000)
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      IERRG4=IERROR
!
      ILOCFN=0
      NUMNUM=0
!
      X1=0.0
      Y1=0.0
      X2=0.0
      Y2=0.0
!
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
        DO 55 I=1,NUMARG
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
   63   FORMAT('IREBLI(1),IREBCO(1),PREBTH(1),AREGBA(1) = ',   &
               2(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64   FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65   FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',   &
               3(A4,2X),2G15.7)
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
        DO 81 I=1,NUMDEV
          WRITE(ICOUT,82)IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   82     FORMAT('IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',   &
                 3(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,83)IDPOWE(I),IDCONT(I),IDCOLO(I)
   83     FORMAT('IDPOWE(I),IDCONT(I),IDCOLO(I) = ',2(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,84)IDNVPP(I),IDNHPP(I),IDUNIT(I)
   84     FORMAT('IDNVPP(I),IDNHPP(I),IDUNIT(I) = ',3I8)
          CALL DPWRST('XXX','BUG ')
   81   CONTINUE
        WRITE(ICOUT,88)IBUGG4,IBUGD2,ISUBG4,IERRG4,IFOUND,IERROR
   88   FORMAT('IBUGG4,IBUGD2,ISUBG4,IERRG4,IFOUND,IERROR = ',   &
               5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IFIG='INDU'
      NUMPT=2
      NUMPT2=2*NUMPT
!
!               ********************************
!               **  STEP 0--                  **
!               **  STEP THROUGH EACH DEVICE  **
!               ********************************
!
      IF(NUMDEV.LE.0)GO TO 9000
      DO 8000 IDEVIC=1,NUMDEV
!
        IF(IDPOWE(IDEVIC).EQ.'OFF')GO TO 8000
        IF(IDMANU(IDEVIC).EQ.'OFF')GO TO 8000
        IF(IDMANU(IDEVIC).EQ.'NULL')GO TO 8000
        IF(IDMANU(IDEVIC).EQ.'NONE')GO TO 8000
        IF(IDMANU(IDEVIC).EQ.'DISC')GO TO 8000
!
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
!
!               ************************************
!               **  STEP 1--                      **
!               **  CARRY OUT OPENING OPERATIONS  **
!               **  ON THE GRAPHICS DEVICES       **
!               ************************************
!
        CALL DPOPDE
!
        IBELSW='OFF'
        NUMRIN=0
        IERASW='OFF'
!
        CALL DPOPPL(IGRASW,IBELSW,NUMRIN,IERASW,IBACCO,IBACC2)
!
!               *****************************************
!               **  STEP 2--                           **
!               **  SEARCH FOR COMMAND SPECIFICATIONS  **
!               *****************************************
!
        IF(NUMARG.GE.2.AND.   &
           IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')THEN
          ITYPEO='ABSO'
          ILOCFN=1
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'ABSO'.AND.   &
               IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')THEN
          ITYPEO='ABSO'
          ILOCFN=2
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'RELA'.AND.   &
               IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')THEN
          ITYPEO='RELA'
          ILOCFN=2
        ELSE
          GO TO 1130
        ENDIF
!
        IF(ILOCFN.GT.NUMARG)GO TO 1130
        DO 1120 I=ILOCFN,NUMARG
          IF(IARGT(I).NE.'NUMB')GO TO 1130
 1120   CONTINUE
        IFOUND='YES'
!
!               ****************************
!               **  STEP 3--              **
!               **  DRAW OUT THE LINE(S)  **
!               ****************************
!
        NUMNUM=NUMARG-ILOCFN+1
        IF(NUMNUM.LT.NUMPT2)THEN
          J=ILOCFN-1
          X1=PXSTAR
          Y1=PYSTAR
        ELSE
          J=ILOCFN
          IF(J.GT.NUMARG)GO TO 1190
          X1=ARG(J)
          IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X1,X1,   &
             IBUGD2,ISUBRO,IERROR)
          J=J+1
          IF(J.GT.NUMARG)GO TO 1190
          Y1=ARG(J)
          IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y1,Y1,   &
             IBUGD2,ISUBRO,IERROR)
        ENDIF
!
 1160   CONTINUE
        J=J+1
        IF(J.GT.NUMARG)GO TO 1190
        X2=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X2,X2,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')X2=X1+X2
        J=J+1
        IF(J.GT.NUMARG)GO TO 1190
        Y2=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y2,Y2,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
!
        CALL DPIND2(X1,Y1,X2,Y2,PX,PY,   &
                    IFIG,ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                    AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                    IREFSW,IREFCO,IREFC2,   &
                    IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                    PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
        X1=X2
        Y1=Y2
!
        GO TO 1160
 1190   CONTINUE
!
        PXEND=X2
        PYEND=Y2
!
!               ************************************
!               **  STEP 4--                      **
!               **  CARRY OUT CLOSING OPERATIONS  **
!               **  ON THE GRAPHICS DEVICES       **
!               ************************************
!
        ICOPSW='OFF'
        NUMCOP=0
        CALL DPCLPL(ICOPSW,NUMCOP,   &
                    PGRAXF,PGRAYF,   &
                    IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,   &
                    PDIAHE,PDIAWI,PDIAVG,PDIAHG)
!
        CALL DPCLDE
!
 8000 CONTINUE
      GO TO 9000
!
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
      GO TO 9000
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPINDU
      SUBROUTINE DPINFU(IFUNC3,N3,IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,   &
                        NUMNAM,IANS,IWIDTH,IHLEFT,IHLEF2,ILISTL,   &
                        NEWNAM,MAXN3,   &
                        IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
!
!     PURPOSE--INSERT (IF NECESSARY) THE FUNCTION IN IFUNC3(.) INTO THE
!              GENERAL DATAPLOT INTERNAL FUNCTION TABLE IFUNC(.).
!              ALSO, UPDATE INTERNAL DATAPLOT LISTS (IF NECESSARY).
!
!     INPUT  FUNCTION--IN IFUNC3(.)
!     OUTPUT FUNCTION--SOMEWHERE IN IFUNC(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1978.
!     UPDATED         --JANUARY   1979.
!     UPDATED         --JULY      1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1993. FIX BUG STATEMENT
!                                       MAXCHF => 120
!     UPDATED         --JANUARY   2012. IF N3 < 0, THEN DELETE THE
!                                       STRING
!     UPDATED         --MARCH     2015. UPDATE "IN" ARRAY TO 0 WHEN
!                                       DEFINING A NEW STRING
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 NEWNAM
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
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
!
      DIMENSION IANS(*)
!
      DIMENSION IFUNC3(*)
      DIMENSION IFUNC(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIN'
      ISUBN2='FU  '
      IERROR='NO'
!
      IDEL=0
!
!               ******************************************
!               **  INSERT A FUNCTION                   **
!               **  INTO THE GENERAL DATAPLOT FUNCTION  **
!               **  TABLE IFUNC(.).                     **
!               **  MAKE ADJUSTMENTS TO THE             **
!               **  INTERNAL DATAPLOT LISTS.            **
!               ******************************************
!
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
        DO 55 I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                         IVSTAR(I),IVSTOP(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',   &
                 I8,2X,A4,A4,2X,A4,2I8)
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
!CCCC   THE FOLLOWING LINE WAS CHANGED     DECEMBER 1993
!CCCC   WRITE(ICOUT,60)(IFUNC(I),I=1,MAXCHF)
        WRITE(ICOUT,60)(IFUNC(I),I=1,MIN(MAXCHF,120))
   60   FORMAT('IFUNC(.)  = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERROR='NO'
      NUMCH0=NUMCHF
!
!               *****************************************************
!               **  STEP 2--                                       **
!               **  DETERMINE IF THE ADDITION OF THE NEW FUNCTION  **
!               **  TO THE INTERNAL DATAPLOT TABLE                 **
!               **  WILL OVERFLOW THE TABLE (TYPICALLY             **
!               **  THERE IS A MAXCHF CHARACTER LIMIT                **
!               **  FOR THE SUM TOTAL OVER ALL FUNCTIONS).         **
!               *****************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NEWNAM.EQ.'YES')THEN
        IF(NUMNAM.GE.MAXN3)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1001)
 1001     FORMAT('***** ERROR IN DPINFU--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1003)IHLEFT,IHLEF2
 1003     FORMAT('      MAXIMUM NUMBER OF NAMES EXCEEDED.  STRING ',   &
                 2A4,' NOT UPDATED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
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
!
      IF(N0TEST.GT.MAXCHF)THEN
        WRITE(ICOUT,2301)
 2301   FORMAT('***** ERROR IN DPINFU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2302)
 2302   FORMAT('      ERROR CAUSED IN ENTERING THE FUNCTION INTO THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2304)
 2304   FORMAT('      INTERNAL DATAPLOT FUNCTION TABLE.  THE TOTAL ',   &
               'NUMBER OF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2306)MAXCHF
 2306   FORMAT('      CHARACTERS IN THAT TABLE (FOR ALL FUNCTIONS) ',   &
               'MAY NOT EXCEED ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2307)
 2307   FORMAT('      SUCH AN OVERFLOW CONDITION HAS JUST BEEN ',   &
               'ENCOUNTERED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2309)
 2309   FORMAT('      THE FUNCTION TABLE HAS BEEN RESET TO ITS STATUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2311)
 2311   FORMAT('      BEFORE ATTEMPTING TO ENTER THE LAST FUNCTION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2313)
 2313   FORMAT('      THE TOTAL NUMBER OF CHARACTERS IN THE FUNCTION ',   &
               'TABLE')
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
        GO TO 9000
      ENDIF
!
!               ***************************************************
!               **  STEP 3--                                     **
!               **  MOVE THE SEGMENT OF THE STRING IN IFUNC(.)   **
!               **  WHICH IS BEYOND THE FUNCTION OF INTEREST     **
!               **  OVER AN APPROPRIATE NUMBER OF SPACES.        **
!               ***************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,2401)N3,N3OLD,IDEL,ISTART,ISTOP
 2401   FORMAT('N3,N3OLD,IDEL,ISTART,ISTOP = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NEWNAM.EQ.'YES')GO TO 3290
      IF(N3.LT.0)THEN
        KMIN=ISTART
        KMAX=NUMCHF-IDEL
        DO 3205 K=KMIN,KMAX
          L=K+IDEL
          IFUNC(K)=IFUNC(L)
 3205   CONTINUE
      ELSE
        KMIN=ISTOP+1
        KMAX=NUMCHF+IDEL
!
        IF(IDEL.EQ.0)GO TO 3290
        IF(IDEL.GT.0)THEN
          DO 3215 K=KMIN,KMAX
            KREV=KMAX-K+KMIN
            LREV=KREV-IDEL
            IFUNC(KREV)=IFUNC(LREV)
 3215     CONTINUE
        ELSEIF(IDEL.LT.0)THEN
          DO 3225 K=KMIN,KMAX
            L=K-IDEL
            IFUNC(K)=IFUNC(L)
 3225     CONTINUE
        ENDIF
      ENDIF
!
 3290 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,3291)(IFUNC(I),I=1,MIN(MAXCHF,120))
 3291   FORMAT('AT 3290: IFUNC(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!
!               **************************************************
!               **  STEP 4--                                    **
!               **  MOVE THE NEW FUNCTION INTO THE APPROPRIATE  **
!               **  PLACE IN IFUNC(.).                          **
!               **************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N3.GE.0)THEN
        L=0
        DO 4200 K=ISTART,ISTOP
          L=L+1
          IFUNC(K)=IFUNC3(L)
 4200    CONTINUE
      ENDIF
!
!               ************************************
!               **  STEP 5--                      **
!               **  REDEFINE NUMCHF = THE UPDATED **
!               **  LENGTH OF IFUNC(.).           **
!               ************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMCHF=N0TEST
!
!               *************************************************
!               **  STEP 6--                                   **
!               **  MAKE THE ADJUSTMENTS TO THE INTERNAL LIST  **
!               *************************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NEWNAM.EQ.'YES')THEN
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='F'
        IN(ILISTL)=0
        IVSTAR(ILISTL)=ISTART
        IVSTOP(ILISTL)=ISTOP
        NUMNAM=NUMNAM+1
        GO TO 9000
      ELSEIF(N3.GE.0)THEN
        N3OLD=IVSTOP(ILISTL)-IVSTAR(ILISTL)+1
        IDEL=N3-N3OLD
!
        DO 6210 I=1,NUMNAM
          IF(IUSE(I).EQ.'F')THEN
            IF(IVSTAR(I).GT.ISTART)IVSTAR(I)=IVSTAR(I)+IDEL
            IF(IVSTOP(I).GE.ISTART)IVSTOP(I)=IVSTOP(I)+IDEL
          ENDIF
 6210   CONTINUE
      ELSE
        N3OLD=IVSTOP(ILISTL)-IVSTAR(ILISTL)+1
        IDEL=N3OLD
!
        DO 6220 I=1,NUMNAM
          IF(IUSE(I).EQ.'F')THEN
            IF(IVSTAR(I).GT.ISTART)IVSTAR(I)=IVSTAR(I)-IDEL
            IF(IVSTOP(I).GE.ISTART)IVSTOP(I)=IVSTOP(I)-IDEL
          ENDIF
 6220   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPINFU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NEWNAM,NUMNAM
 9013   FORMAT('IERROR,NEWNAM,NUMNAM = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                           IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',   &
                 I8,2X,A4,A4,2X,A4,2I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9018)NUMCH0,N3,NUMCHF,MAXN3,MAXCHF
 9018   FORMAT('NUMCH0,N3,NUMCHF,MAXN3,MAXCHF = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)(IFUNC3(I),I=1,MIN(N3,120))
 9019   FORMAT('IFUNC3(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
!CCCC   THE FOLLOWING LINE WAS CHANGED     DECEMBER 1993
!CCCC   WRITE(ICOUT,9020)(IFUNC(I),I=1,MAXCHF)
        WRITE(ICOUT,9020)(IFUNC(I),I=1,MIN(MAXCHF,120))
 9020   FORMAT('IFUNC(.)  = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPINFU
      SUBROUTINE DPINLE(ILEGN2,ISTH,N2,ILEGNA,ILEGST,ILEGSP,   &
      NUMLEG,MAXLEG,ILEGTE,NCLEG,MXCLEG,IANS,IWIDTH,IBUGIL,IERROR)
!
!     PURPOSE--INSERT(IF NECESSARY) THE HOLLERITH LEGEND
!              IN ISTH(.)
!              INTO (RESPECTIVELY) THE PACKED
!              INTERNAL DATAPLOT TABLES ILEGTE(.)
!              ALSO, UPDATE INTERNAL DATAPLOT LISTS
!              ILEGNA(.), ILEGST(.), AND ILEGSP(.).
!              A CHECK FOR N2 BEING POSITIVE IS DONE HEREIN.
!
!     NOTE--IT IS ASSUMED IN ALL CASES (EVEN FOR
!           A BLANKED-OUT LEGEND) THAT THE NUMBER
!           OF CHARACTERS IN THE LEGEND IS AT LEAST 1;
!           (THAT IS, THE INPUT N2 IS 1 OR LARGER).
!
!     INPUT  LEGENDS --IN ISTH(.)
!     OUTPUT LEGENDS --SOMEWHERE IN ILEGTE(.)
!
!     ILEGN2 = NAME FOR THE INPUT LEGEND.
!     ISTH   = VECTOR CONTAINING INPUT LEGEND STRING (IN HOLLERITH)
!     N2     = LENGTH OF INPUT LEGEND STRING.
!     ILEGNA = TABLE OF EXISTING LEGEND NAMES.
!     ILEGST = TABLE OF EXISTING START POSITIONS IN ILEGTE.
!     ILEGSP = TABLE OF EXISTING STOP  POSITIONS IN ILEGTE.
!     NUMLEG = NUMBER OF EXISTING LEGENDS.
!     MAXLEG = MAXIMUM NUMBER OF ALLOWABLE LEGENDS.
!     ILEGTE  = VECTOR OF PACKED LEGENDS (HOLLERITH) WHERE FINAL STORAGE IS DONE
!     NCLEG = NUMBER OF PACKED CHARACTERS IN ILEGTE(.)
!     MXCLEG = MAX NUMBER OF ALLOWABLE CHARACTERS IN ILEGTE(.)
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MARCH     1979.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1994.  BUG FIX
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ILEGN2
      CHARACTER*4 ISTH
      CHARACTER*4 ILEGNA
      CHARACTER*4 ILEGTE
      CHARACTER*4 IANS
      CHARACTER*4 IBUGIL
      CHARACTER*4 IERROR
!
      CHARACTER*4 NEWNAM
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION ISTH(*)
      DIMENSION ILEGNA(*)
      DIMENSION ILEGST(*)
      DIMENSION ILEGSP(*)
      DIMENSION ILEGTE(*)
      DIMENSION IANS(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIN'
      ISUBN2='LE  '
!
      ILISTL=0
      IDEL=0
!
      NEWNAM='UNKN'
!
!               ******************************************
!               **  INSERT A LEGEND                     **
!               **  INTO THE GENERAL DATAPLOT LEGEND    **
!               **  TABLES ILEGTE(.)        **
!               **  MAKE ADJUSTMENTS TO THE             **
!               **  INTERNAL DATAPLOT LISTS.            **
!               ******************************************
!
      IF(IBUGIL.EQ.'OFF')GO TO 90
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
      DO 82 I=1,NUMLEG
      WRITE(ICOUT,83)I,ILEGNA(I),ILEGST(I),ILEGSP(I)
   83 FORMAT('I,ILEGNA(I),ILEGST(I),ILEGSP(I) = ',I4,3X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
   82 CONTINUE
   90 CONTINUE
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERROR='NO'
      NUMCH0=NCLEG
!
      IF(N2.GE.1)GO TO 190
!
      WRITE(ICOUT,111)
  111 FORMAT('***** INTERNAL ERROR IN DPLEG--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('      THE INPUT LENGTH N2 OF THE STRING IS ',   &
      'NON-POSITIVE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)N2
  113 FORMAT('      N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  190 CONTINUE
!
!               ************************************
!               **  STEP 2--                      **
!               **  DETERMINE IF THE LEGEND NAME  **
!               **  ALREADY EXISTS IN THE TABLE.  **
!               ************************************
!
      ISTEPN='2'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='YES'
      IF(NUMLEG.LE.0)GO TO 250
      DO 210 I=1,NUMLEG
      I2=I
      IF(ILEGN2.EQ.ILEGNA(I))GO TO 220
  210 CONTINUE
      GO TO 250
!
  220 CONTINUE
      NEWNAM='NO'
      ILISTL=I2
      GO TO 290
!
  250 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMLEG+1
      GO TO 290
!
  290 CONTINUE
!
!               ***********************************************************
!               **  STEP 3--                                             **
!               **  FOR THE CASE WHEN HAVE A NEW NAME,                   **
!               **  DETERMINE IF THIS NEW NAME                           **
!               **  WILL OVERFLOW THE ALLOWABLE NUMBER OF LEGEND NAMES   **
!               **   IN TABLE ILEGNA(.).                                 **
!               ***********************************************************
!
      ISTEPN='3'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NEWNAM.EQ.'NO')GO TO 390
      IF(ILISTL.LE.MAXLEG)GO TO 390
!
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
  306 FORMAT('      TABLE (FOR ALL LEGENDS) MAY NOT EXCEED ',   &
      I8)
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
      GO TO 9000
!
  390 CONTINUE
!
!               *****************************************************
!               **  STEP 4--                                       **
!               **  DETERMINE IF THE ADDITION OF THE NEW LEGEND    **
!               **  STRING TO THE INTERNAL DATAPLOT TABLES         **
!               **  ILEGTE(.)                                  **
!               **  WILL OVERFLOW THE TABLE (TYPICALLY             **
!               **  THERE IS A 500 CHARACTER LIMIT                 **
!               **  FOR THE SUM TOTAL OVER ALL LEGENDS).           **
!               *****************************************************
!
      ISTEPN='4'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NEWNAM.EQ.'YES')GO TO 2100
      GO TO 2200
!
 2100 CONTINUE
      N0TEST=NCLEG+N2
      GO TO 2300
!
 2200 CONTINUE
      IMIN=ILEGST(ILISTL)
      IMAX=ILEGSP(ILISTL)
      N2OLD=IMAX-IMIN+1
      IDEL=N2-N2OLD
      N0TEST=NCLEG+IDEL
      GO TO 2300
!
 2300 CONTINUE
      IF(N0TEST.LE.MXCLEG)GO TO 2390
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
 2306 FORMAT('      TABLE (FOR ALL LEGEND  S) MAY NOT EXCEED ',   &
      I8)
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
      GO TO 9000
 2390 CONTINUE
!
!               ***************************************************
!               **  STEP 5--                                     **
!               **  MOVE THE SEGMENT OF THE STRING IN ILEGTE(.)   **
!               **  WHICH IS BEYOND THE LEGEND   OF INTEREST     **
!               **  OVER AN APPROPRIATE NUMBER OF SPACES.        **
!               ***************************************************
!
      ISTEPN='5'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NEWNAM.EQ.'YES')GO TO 3110
      GO TO 3120
!
 3110 CONTINUE
      ISTART=NCLEG+1
      ISTOP=ISTART+N2-1
      GO TO 3190
!
 3120 CONTINUE
      ISTART=ILEGST(ILISTL)
      ISTOP=ISTART+N2-1
      GO TO 3190
!
 3190 CONTINUE
!
      IF(NEWNAM.EQ.'YES')GO TO 3290
      KMIN=ISTOP+1
      KMAX=NCLEG+IDEL
!CCCC JUNE 1994.  FOLLOWING LINE CAUSED SPURIOUS CHARACTERS IF
!CCCC HIGHER LEGENDS BLANKED OUT, EARLIER LEGEND LONGER THAN THE
!CCCC ORIGINAL.
!CCCC IF(KMIN.GT.NCLEG)GO TO 3290
      IF(IDEL.LE.0)GO TO 3210
      GO TO 3220
!
 3210 CONTINUE
      NCLEGP=NCLEG+1
      DO 3211 K=KMIN,KMAX
      L=K-IDEL
      IF(L.GE.NCLEGP)GO TO 3211
      ILEGTE(K)=ILEGTE(L)
 3211 CONTINUE
      GO TO 3290
!
 3220 CONTINUE
      DO 3221 K=KMIN,KMAX
      KREV=KMAX-K+KMIN
      L=KREV-IDEL
      IF(L.LE.0)GO TO 3221
      ILEGTE(KREV)=ILEGTE(L)
 3221 CONTINUE
      GO TO 3290
!
 3290 CONTINUE
!
!               **************************************************
!               **  STEP 6--                                    **
!               **  MOVE THE NEW LEGEND   INTO THE APPROPRIATE  **
!               **  PLACE IN ILEGTE(.).                          **
!               **************************************************
!
      ISTEPN='6'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      L=0
      DO 4200 K=ISTART,ISTOP
      L=L+1
      ILEGTE(K)=ISTH(L)
 4200 CONTINUE
!
!               ************************************
!               **  STEP 7--                      **
!               **  REDEFINE NCLEG = THE UPDATED **
!               **  LENGTH OF ILEGTE(.).           **
!               ************************************
!
      ISTEPN='7'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NCLEG=N0TEST
!
!               *************************************************
!               **  STEP 8--                                   **
!               **  MAKE THE ADJUSTMENTS TO THE INTERNAL LIST  **
!               *************************************************
!
      ISTEPN='8'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NEWNAM.EQ.'YES')GO TO 6100
      GO TO 6200
!
 6100 CONTINUE
      ILEGNA(ILISTL)=ILEGN2
      ILEGST(ILISTL)=ISTART
      ILEGSP(ILISTL)=ISTOP
      NUMLEG=NUMLEG+1
      GO TO 9000
!
 6200 CONTINUE
      N2OLD=ILEGSP(ILISTL)-ILEGST(ILISTL)+1
      IDEL=N2-N2OLD
!
      DO 6210 I=1,NUMLEG
      IF(ILEGST(I).GT.ISTART)ILEGST(I)=ILEGST(I)+IDEL
      IF(ILEGSP(I).GE.ISTART)ILEGSP(I)=ILEGSP(I)+IDEL
 6210 CONTINUE
      GO TO 9000
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
!
      ISTEPN='9'
      IF(IBUGIL.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IBUGIL.EQ.'OFF')GO TO 9090
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
      DO 9022 I=1,NUMLEG
      WRITE(ICOUT,9023)I,ILEGNA(I),ILEGST(I),ILEGSP(I)
 9023 FORMAT('I,ILEGNA(I),ILEGST(I),ILEGSP(I) = ',I4,3X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPINLE
      SUBROUTINE DPINPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        MAXNPP,   &
                        ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--FORM AN INTERACTION PLOT, I.E.
!              INTERACTION PLOT Y X1 X2
!              IS A PLOT OF Y VERSUS X1*X2
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/10
!     ORIGINAL VERSION--OCTOBER   1999.
!     UPDATED         --NOVEMBER  2009. UPDATE PARSING TO USE DPPARS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
      CHARACTER*40 INAME
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIN'
      ISUBN2='PL  '
      IFOUND='YES'
      IAND2='NO'
      ICASPL='INTE'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      ATEMP=CPUMIN
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'INPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXNPP,NS
   53   FORMAT('ICASPL,IAND1,IAND2,MAXNPP,NS = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGG3,IBUGG2,ISUBRO,IFOUND,IERROR
   54   FORMAT('IBUGG3,IBUGG2,ISUBRO,IFOUND,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'INPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LT.1)GO TO 9000
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!               *******************************************************
!               **  STEP 16--                                        **
!               **  FORM THE PLOT COORIDINATES                       **
!               *******************************************************
!
      ISTEPN='15.2'
      IF(ISUBRO.EQ.'INPL'.OR.IBUGG3.EQ.'ON')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      L=0
      NLOCAL=NRIGHT(1)
!
      DO 1520 I=1,NLOCAL
        IF(ISUB(I).EQ.0)GO TO 1520
        L=L+1
!
        IF(L.GT.MAXNPP)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1521)
 1521     FORMAT('***** ERROR IN INTERACTION PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1523)MAXNPP
 1523     FORMAT('      THE NUMBER OF PLOT POINTS HAS JUST EXCEEDED ',   &
                 I8)
          CALL DPWRST('XXX','BUG ')
!
          IF(ISUBRO.EQ.'INPL' .OR. IBUGG2.EQ.'ON')THEN
             WRITE(ICOUT,1525)I,NLOCAL,L,MAXN,MAXNPP,NPLOTP
 1525        FORMAT('I,NLOCAL,L,MAXN,MAXNPP,NPLOTP = ',6I8)
             CALL DPWRST('XXX','BUG ')
          ENDIF
!
          GO TO 9000
        ENDIF
!
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
!
        IF(NUMVAR.GE.2)THEN
          DO 1530 K=2,NUMVAR
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
!
          D(L)=1.0
          NPLOTP=L
        ENDIF
!
 1520 CONTINUE
      NPLOTV=2
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9013   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',   &
               3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,Y(.),X(.),D(.),ISUB(.)--')
        CALL DPWRST('XXX','BUG ')
        DO 9021 I=1,NPLOTP
          WRITE(ICOUT,9022)I,Y(I),X(I),D(I),ISUB(I)
 9022     FORMAT(I8,F15.7,F15.7,F15.7,I8)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPINPL
      SUBROUTINE DPINQU(IBUGA3,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--SUPPORTS THE FOLLOWING COMMAND:
!
!                 LET IFLAG = INQUIRE <FILE>
!
!              WHERE IFLAG IS SET TO "1" IF FILE EXISTS AND SET TO "0"
!              IF IT DOES NOT.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015/12
!     ORIGINAL VERSION--DECEMBER  2015.
!     UPDATED         --FEBRUARY  2016. SUPPORT QUOTES
!     UPDATED         --JUNE      2016. SUPPORT FOR INTEGER VALUE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
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
!
      INCLUDE 'DPCOPA.INC'
!
!CCCC CHARACTER*80 IFILE
!CCCC CHARACTER*255 ICANS
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER (LEN=MAXSTR) :: ICANS
!
      LOGICAL LEXIST
      LOGICAL LOPEN
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN0='INQU'
      ISUBN1='DPIN'
      ISUBN2='QU  '
      IFOUND='YES'
      IERROR='NO'
      IEXIST='NO'
      IFILSV=IFILQU
      IFILQU='ON'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPINQU--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************************
!               **  STEP 1--                                      **
!               **  CHECK FOR VALID COMMAND.                      **
!               ****************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
!               *********************************************************
!               **  STEP 2--                                            *
!               **  EXAMINE THE LEFT-HAND SIDE--                        *
!               **  IS THE PARAMETER NAME TO LEFT OF = SIGN             *
!               **  ALREADY IN THE NAME LIST?                           *
!               **  NOTE THAT     IHLEFT    IS THE NAME OF THE VARIABLE *
!               **  ON THE LEFT.                                        *
!               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE    *
!               **  OF THE NAME ON THE LEFT.                            *
!               **  NOTE THAT     ICOLL    IS THE DATA COLUMN (1 TO 12) *
!               **  FOR THE NAME OF THE LEFT.                           *
!               *********************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='NO'
      NEWCOL='NO'
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO 200 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'P')THEN
            ILISTL=I2
            GO TO 290
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
            GO TO 9000
          ENDIF
        ENDIF
  200 CONTINUE
!
      ISTEPN='2B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,222)
  222   FORMAT('      THE NUMBER OF VARIABLE AND/OR PARAMETER NAMES ',   &
               'HAS JUST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,223)MAXNAM
  223   FORMAT('      EXCEEDED THE MAX ALLOWABLE ',I8,'  .  ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
  290 CONTINUE
!               ********************************************************
!               **  STEP 3--                                          **
!               **  EXTRACT THE FILE NAME                             **
!               ********************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     2016/02: DATAPLOT COMMANDS THAT USE FILE NAMES HAVE A COUPLE OF
!              SPECIFIC CRITERION:
!
!                 1. A "." IS REQUIRED IN THE NAME TO IDENTIFY THE
!                    STRING AS A FILE.  THE INQUIRE COMMAND DOES NOT
!                    HAVE THIS RESTRICTION.
!
!                 2. FILE NAMES THAT CONTAIN SPACES OR HYPHENS SHOULD
!                    BE ENCLOSED IN QUOTES.  THIS ALSO APPLIES TO THE
!                    INQUIRE COMMAND.
!
!                 3. DATAPLOT WILL LOOK IN THE DATAPLOT AUXILLARY
!                    DIRECTORIES IF IT DOES FIND THE FILE IN CURRENT
!                    DIRECTORY.  IN ADDITION, DATAPLOT WILL CHECK FOR
!                    ALL LOWER CASE OR ALL UPPER CASE CHARACTERS IN
!                    THE NAME.  THE INQUIRE COMMAND WILL ONLY CHECK THE
!                    FILE NAME AS GIVEN.
!
!     2016/06: CHECK IF THE ARGUMENT TYPE IS NUMERIC.  CAN DO AN INQUIRE
!              OF A UNIT NUMBER.
!
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
        GO TO 400
      ENDIF
!
!CCCC CALL DPFILE(IANSLC,IWIDTH,IWORD,
!CCCC1            IOFILE,IBUGA3,ISUBRO,IERROR)
      IOFILE='YES'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')THEN
        WRITE(ICOUT,301)IWORD,IWIDTH,IOFILE
  301   FORMAT('IWORD,IWIDTH,IOFILE = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************************
!               **  STEP 31--                               **
!               **  IF NO FILE NAME GIVEN,                  **
!               **  THEN GENERATE AN ERROR MESSAGE.         **
!               **********************************************
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)
  312   FORMAT('      NO FILE NAME WAS GIVEN.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *************************************
!               **  STEP 32--                      **
!               **  COPY THE FILE INTO THE STRING  **
!               **  "FILE"                         **
!               *************************************
!
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 320 I=1,MAXSTR
        ICANS(I:I)=IANSLC(I)(1:1)
  320 CONTINUE
!
      NCFILE=0
      IFILE=' '
      ISTART=1
      ISTOP=IWIDTH
      IWORD=5
      CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,   &
                  ICOL1,ICOL2,IFILE,NCFILE,   &
                  IBUGA3,ISUBRO,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')THEN
        WRITE(ICOUT,321)NCFILE,IFILE
  321   FORMAT('NCFILE,IFILE = ',I6,2X,A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NCFILE.GE.3)THEN
        IF(IFILE(1:1).EQ.'"' .AND. IFILE(NCFILE:NCFILE).EQ.'"')THEN
          ICNT=0
          DO 401 J=2,NCFILE-1
            ICNT=ICNT+1
            IFILE(ICNT:ICNT)=IFILE(J:J)
 401      CONTINUE
          NCFILE=NCFILE-2
          IFILE(NCFILE+1:NCFILE+2)=' '
        ENDIF
      ENDIF
!
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(NCFILE.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,322)
  322   FORMAT('      A USER FILE NAME IS REQUIRED IN THE ',   &
               'LET ... = INQUIRE ...  COMMAND')
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
        GO TO 9000
      ENDIF
!
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************
!               **  STEP 33--                           **
!               **  INQUIRE ABOUT EXISTENCE OF          **
!               **  SPECIFIED FILE                      **
!               ******************************************
!
      ISTEPN='33'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPINFI(IFILE,IEXIST,IOPEN,IACC,ISUBN0,IBUGA3,ISUBRO,IERROR)
!
!               *******************************************
!               **  STEP 4--                             **
!               **  UPDATE THE LHS PARAMETER             **
!               *******************************************
!
  400 CONTINUE
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC IHNAME(ILISTL)=IHLEFT
!CCCC IHNAM2(ILISTL)=IHLEF2
!CCCC IUSE(ILISTL)='P'
!CCCC VALUE(ILISTL)=1.0
!CCCC IF(IEXIST.EQ.'NO')VALUE(ILISTL)=0.0
!CCCC IVALUE(ILISTL)=VALUE(ILISTL)+0.5
!CCCC IN(ILISTL)=1
!CCCC IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
!
      IF(IEXIST.EQ.'NO')THEN
        VALUE0=0.0
      ELSE
        VALUE0=1.0
        IF(IOPEN.EQ.'YES')VALUE0=2.0
      ENDIF
      CALL DPADDP(IHLEFT,IHLEF2,VALUE0,IHOST1,ISUBN0,   &
                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                  IANS,IWIDTH,IBUGA3,IERROR)
!
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
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                  IANS,IWIDTH,IBUGA3,IERROR)
!
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IEXIST.EQ.'YES')THEN
          WRITE(ICOUT,601)IHLEFT,IHLEF2,IVALUE(ILISTL)
  601     FORMAT('THE SPECIFIED FILE EXISTS, THE PARAMETER ',   &
                 A4,A4,' = ',I8)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,602)IHLEFT,IHLEF2,IVALUE(ILISTL)
  602     FORMAT('THE SPECIFIED FILE DOES NOT EXIST, THE PARAMETER ',   &
                 A4,A4,' = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
!
      IFILQU=IFILSV
!
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
!
      RETURN
      END SUBROUTINE DPINQU
      SUBROUTINE DPINT2(MODEL,NUMCHA,PARAM,IPARN,IPARN2,NUMPV,   &
                        IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,   &
                        IVARN,IVARN2,NUMVAR,XMIN,XMAX,XINT,   &
                        IFLGFB,IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                        NUMNAM,MAXNAM,IFTEXP,IFTORD,IFORSW,   &
                        PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,V,MAXN,   &
                        MAXCOL,MAXCP1,MAXCP2,MAXCP3,MAXCP4,MAXCP5,   &
                        MAXCP6,   &
                        IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
!
!     2015/09: ADD LINES TO ARGUMENT LIST FOR FUNCTION BLOCK
!              AUGMENTATION
!
!     PURPOSE--COMPUTE THE INTEGRAL OF A FUNCTION
!              FROM THE LIMITS XMIN TO XMAX.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1978.
!     UPDATED         --JULY      1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --SEPTEMBER 2015. SUPPORT FUNCTION BLOCKS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IFOUND
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION W,Z
      DOUBLE PRECISION DMIN,DMAX,DNUMSE,DINT,DJ,DELTA2,DMIN2,DMAX2
      DOUBLE PRECISION DB0,DB1,DSUM2,DX,DY,DINT2
!
      DIMENSION MODEL(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN(*)
      DIMENSION IVARN2(*)
!
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
      DIMENSION PRED(*)
      DIMENSION RES(*)
      DIMENSION XPLOT(*)
      DIMENSION YPLOT(*)
      DIMENSION X2PLOT(*)
      DIMENSION TAGPLO(*)
      DIMENSION V(*)
!
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
!
      CHARACTER*4 IHNAME(*)
      CHARACTER*4 IHNAM2(*)
      CHARACTER*4 IUSE(*)
!
      DIMENSION ILOCV(10)
!
      DIMENSION W(16)
      DIMENSION Z(16)
!
!     2015/08: FUNCTION BLOCK
!
      INCLUDE 'DPCOFB.INC'
!
      CHARACTER*8 IFBNAM
      CHARACTER*8 IFBANS
!
      COMMON/IFBL2/IFLGF2
!
      CHARACTER*4 IFEESV
      COMMON/IFEED/IFEESV
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA Z(1),Z(2),Z(3),Z(4),Z(5),Z(6),Z(7),Z(8)   &
                         /-0.98940093499165D0,-0.944575023073233D0,   &
      -0.865631202387832D0,-0.755404408355003D0,-0.617876244402644D0,   &
      -0.458016777657227D0,-0.281603550779259D0,-0.095012509837637D0/
      DATA Z(9),Z(10),Z(11),Z(12),Z(13),Z(14),Z(15),Z(16)   &
      /0.095012509837637D0,0.281603550779259D0,0.458016777657227D0,   &
      0.617876244402644D0,0.755404408355003D0,0.865631202387832D0,   &
      0.944575023073233D0,0.989400934991650D0/
      DATA W(1),W(2),W(3),W(4),W(5),W(6),W(7),W(8)   &
                        /0.027152459411754D0,0.062253523938648D0,   &
      0.095158511682493D0,0.124628971255534D0,0.149595988816577D0,   &
      0.169156519395003D0,0.182603415044924D0,0.189450610455069D0/
      DATA W(9),W(10),W(11),W(12),W(13),W(14),W(15),W(16)   &
      /0.189450610455069D0,0.182603415044924D0,0.169156519395003D0,   &
      0.149595988816577D0,0.124628971255534D0,0.095158511682493D0,   &
      0.062253523938648D0,0.027152459411754D0/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIN'
      ISUBN2='T2  '
      IFOUND='NO  '
!
      CUTOFF=0.001
      ACCUR=0.0000001
      MAXSEG=20
      IPASS=2
      J2=0
      IFLGF2=IFLGFB
!
      ABSXIN=0.0
      XINT2=0.0
      DIFF=0.0
      RATIO=0.0
!
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
        DO 55 I=1,NUMPV
          WRITE(ICOUT,56)I,PARAM(I),IPARN(I),IPARN2(I)
   56     FORMAT('I,PARAM(I),IPARN(I),IPARN2(I) = ',I8,G15.7,2A4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO 60 I=1,NUMVAR
          WRITE(ICOUT,61)I,IN(I),IVARN(I),IVARN2(I)
   61     FORMAT('I,IN(I),IVARN(I) = ',2I8,2X,2A4)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
      ENDIF
!
!               ***************************************************
!               **  STEP 1--                                     **
!               **  DETERMINE THE LOCATIONS (IN THE LIST IPARN)  **
!               **  OF THE VARIABLES OF INTEGRATION.             **
!               ***************************************************
!
      IF(IFLGFB.LE.0)THEN
        DO 100 I=1,NUMVAR
          IH=IVARN(I)
          IH2=IVARN2(I)
          DO 200 J=1,NUMPV
            J2=J
            IF(IH.EQ.IPARN(J).AND.IH2.EQ.IPARN2(J))GO TO 210
  200     CONTINUE
  210     CONTINUE
          ILOCV(I)=J2
  100   CONTINUE
      ENDIF
!
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
!
!               **************************************************
!               **  STEP 2--                                    **
!               **  WRITE OUT  PRELIMINARY SUMMARY INFORMATION  **
!               **************************************************
!
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
!
!               ***********************************************
!               **  STEP 3--                                 **
!               **  STEP THROUGH 10 DIFFERENT SEGMENTATIONS  **
!               **  OF THE DOMAIN OF THE INTEGRAL.           **
!               ***********************************************
!
      DMIN=XMIN
      DMAX=XMAX
      DO 3100 NUMSEG=1,MAXSEG
!
!               ****************************************************
!               **  STEP 4--                                      **
!               **  WITHIN A GIVEN SEGMENTATION,                  **
!               **  APPLY THE 16-POINT GAUSSIAN QUADRATURE RULE.  **
!               ****************************************************
!
        DNUMSE=NUMSEG
        DELTA2=(DMAX-DMIN)/DNUMSE
        DINT=0.0D0
        DO 3200 J=1,NUMSEG
        DJ=J
        DMIN2=DMIN+(DJ-1.0D0)*DELTA2
        DMAX2=DMIN+DJ*DELTA2
        DB1=(DMAX2-DMIN2)/2.0D0
        DB0=(DMAX2+DMIN2)/2.0D0
!
        DSUM2=0.0D0
        DO 3300 I=1,16
          DX=DB0+DB1*Z(I)
          X=DX
!
          IF(IFLGFB.LE.0)THEN
            DO 3303 K=1,NUMVAR
              JLOC=ILOCV(K)
              PARAM(JLOC)=X
 3303       CONTINUE
            CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
                        IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,Y,   &
                        IBUGCO,IBUGEV,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
          ELSE
!
!           FUNCTION BLOCK CASE:
!
!           STEP 1: COMPUTE FUNCTION BLOCK (BUT FIRST SET CURRENT
!                   VALUE OF DESIRED PARAMETER)
!
            DO 3305 II=1,NUMNAM
              IF(IH.EQ.IHNAME(II) .AND. IH2.EQ.IHNAM2(II) .AND.   &
                 IUSE(II).EQ.'P')THEN
                VALUE(II)=X
                IVALUE(II)=INT(X+0.5)
                GO TO 3309
              ENDIF
 3305       CONTINUE
!
!           PARAMETER NAME NOT FOUND IN CURRENT LIST, SO NEED TO ADD
!           TO NAME LIST
!
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
!
 3309       CONTINUE
!
            IFEEDB='OFF'
            CALL DPFBEX(IFBNAM,IANGLU,ISEED,IFTEXP,IFTORD,IFORSW,   &
                        IBUGA3,IBUGA3,IBUGCO,IBUGEV,IBUGEV,   &
                        ISUBRO,IFOUND,IERROR)
            IFEEDB=IFEESV
!
!           STEP 2: RETRIEVE RESPONSE
!
            DO 3320 II=1,NUMNAM
              IF(IFBANS(1:4).EQ.IHNAME(II) .AND.   &
                 IFBANS(5:8).EQ.IHNAM2(II))THEN
                IF(IUSE(II).EQ.'P')THEN
                  Y=VALUE(II)
                  GO TO 3329
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
                  GO TO 3329
                ENDIF
              ENDIF
 3320       CONTINUE
!
!           PARAMETER/VARIABLE NAME NOT FOUND
!
            WRITE(ICOUT,3306)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3321)
 3321       FORMAT('      EXPECTED PARAMETER/VARIABLE NOT FOUND IN ',   &
                 'NAME TABLE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3323)IFBANS
 3323       FORMAT('      EXPECTED NAME = ',A8)
            CALL DPWRST('XXX','BUG ')
!
 3329       CONTINUE
!
          ENDIF
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INT2')THEN
            WRITE(ICOUT,3352)X,Y
 3352       FORMAT('X,Y = ',2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          DY=Y
          DSUM2=DSUM2+W(I)*DY
 3300   CONTINUE
        DINT2=DB1*DSUM2
        DINT=DINT+DINT2
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INT2')THEN
          WRITE(ICOUT,3361)NUMSEG,J,DSUM2,DB0,DB1,DINT2
 3361     FORMAT('NUMSEG,J,DSUM2,DB0,DB1,DINT2=',2I3,4D12.5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 3200 CONTINUE
!
!               ******************************
!               **  STEP 5--                **
!               **  WRITE OUT THE INTEGRAL  **
!               ******************************
!
        XINT=DINT
        IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,3503)NUMSEG,XINT
 3503     FORMAT(I8,'     * ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(NUMSEG.EQ.1)GO TO 3550
          ABSXIN=ABS(XINT)
          DIFF=ABS(XINT-XINT2)
          IF(ABSXIN.LE.CUTOFF.AND.DIFF.LE.ACCUR)GO TO 3500
          IF(ABSXIN.LE.CUTOFF.AND.DIFF.GT.ACCUR)GO TO 3550
          RATIO=ABS(DIFF/XINT)
          IF(ABSXIN.GT.CUTOFF.AND.RATIO.LE.ACCUR)GO TO 3500
          IF(ABSXIN.GT.CUTOFF.AND.RATIO.GT.ACCUR)GO TO 3550
 3550   CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INT2')THEN
        WRITE(ICOUT,3555)CUTOFF,ACCUR,DIFF,RATIO,ABSXIN
 3555   FORMAT('CUTOFF,ACCUR,DIFF,RATIO,ABSXIN = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      XINT2=XINT
!
 3100 CONTINUE
!
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPINT2
      SUBROUTINE DPINTE(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,   &
                        PARAM,IPARN,IPARN2,TEMP1,ITEMP1,   &
                        IANGLU,IFTEXP,IFTORD,IFORSW,   &
                        IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IERROR)
!
!     PURPOSE--TREAT THE LET CASE FOR
!              FINDING THE DEFINITE INTEGRAL OF AN FUNCTION.
!     EXAMPLE--LET A = INTEGRAL X**3+2*X**2-4*X+5 FOR X = 1 3
!            --LET X = INTEGRAL F1 FOR X = 0 B
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1978.
!     UPDATED         --JULY      1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --NOVEMBER  1989. FIX AJUNK & BJUNK DIMENSIONS
!     UPDATED         --JUNE      2013. SUPPORT INDEFINITE INTEGRALS
!                                       (USE QUADPACK ROUTINE QAGI)
!     UPDATED         --SEPTEMBER 2015. SUPPORT FOR FUNCTION BLOCKS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
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
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION TEMP1(*)
      DIMENSION ITEMP1(*)
!
!---------------------------------------------------------------------
!
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
!
      DIMENSION IDUMV(100)
      DIMENSION IDUMV2(100)
!
      DIMENSION ILAB(10)
      DIMENSION IOLD(10)
      DIMENSION IOLD2(10)
      DIMENSION INEW(10)
      DIMENSION INEW2(10)
!
!CCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1989
      DIMENSION BJUNK(1)
!
!-----MAKE DUMMY COMMON BLOCK-----------
!
      PARAMETER (IOPTCH=1000)
      PARAMETER (IOPTC2=100)
!
      CHARACTER*4 IBUGAZ
      CHARACTER*4 ZTYPEH
      CHARACTER*4 ZW21HO
      CHARACTER*4 ZW22HO
      CHARACTER*4 ZIPARN
      CHARACTER*4 ZPARN2
      CHARACTER*4 ZMODEL
      CHARACTER*4 ZIDUMV
      CHARACTER*4 ZDUMV2
!
      DIMENSION ZMODEL(IOPTCH)
      DIMENSION ZTYPEH(IOPTCH)
      DIMENSION ZW21HO(IOPTCH)
      DIMENSION ZW22HO(IOPTCH)
      DIMENSION Z2HOLD(IOPTCH)
!
      DIMENSION ZPARAM(IOPTC2)
      DIMENSION ZIPARN(IOPTC2)
      DIMENSION ZPARN2(IOPTC2)
      DIMENSION ZIDUMV(IOPTC2)
      DIMENSION ZDUMV2(IOPTC2)
      DIMENSION LOCDUM(IOPTC2)
!
      COMMON /OPTCMC/ IBUGAZ, ZTYPEH, ZW21HO, ZW22HO, ZIPARN, ZPARN2,   &
                      ZIDUMV, ZDUMV2, ZMODEL
      COMMON /OPTCMR/ ZPARAM, Z2HOLD,   &
                      NUMCHZ, NUMPVZ, NWHOLZ, NUMDVZ, LOCDUM
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFB.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               *******************************************
!               **  TREAT THE DEFINITE INTEGRAL SUBCASE  **
!               **  OF THE LET COMMAND                   **
!               *******************************************
!
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
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='NO'
      ISUBN1='DPIN'
      ISUBN2='TE  '
      IHLEFT='UNKN'
      IHLEF2='UNKN'
      IERROR='NO'
!
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
      XMIN=CPUMIN
      XMAX=CPUMAX
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  STEP 2--                                         *
!               **  EXAMINE THE LEFT-HAND SIDE--                     *
!               **  IS THE NAME     NAME TO LEFT OF = SIGN           *
!               **  ALREADY IN THE NAME LIST?                        *
!               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE *
!               **  OF THE NAME ON THE LEFT.                         *
!               ******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO 2000 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'V   ' .OR. IUSE(I).EQ.'P   ')THEN
            ILISTL=I2
            GO TO 2900
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2201)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,210)IHLEFT,IHLEF2
  210       FORMAT('      AN ATTEMPT WAS MADE TO USE ',2A4,' AS A ',   &
                 'VARIABLE OR PARAMETER')
            CALL DPWRST('XXX','BUG ')
            IF(IUSE(I).EQ.'F')THEN
              WRITE(ICOUT,211)
  211         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A STRING.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IUSE(I).EQ.'M')THEN
              WRITE(ICOUT,212)
  212         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A MATRIX.')
              CALL DPWRST('XXX','BUG ')
            ELSE
              WRITE(ICOUT,213)IUSE(I)
  213         FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A ',A4,'.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
            GO TO 9000
          ENDIF
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
 2205   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES, AND ',   &
               'THEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2206)
 2206   FORMAT('      REDEFINE (REUSE) SOME OF THE ALREADY USED NAMES')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
 2900 CONTINUE
!
!               ************************************************************
!               **  STEP 3.1--                                            **
!               **  EXTRACT THE RIGHT-SIDE FUNCTIONAL EXPRESSION FROM THE **
!               **  INPUT COMMAND LINE (STARTING WITH THE FIRST NON-BLANK **
!               **  LOCATION AFTER THE EQUAL SIGN AND ENDING WITH THE END **
!               **  OF THE LINE OR WITH THE LAST NON-BLANK CHARACTER      **
!               **  BEFORE     WRT  .  PLACE THE FUNCTION IN IFUNC2(.) .  **
!               ************************************************************
!
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     2015/09: CHECK TO SEE IF THE FIRST ARGUMENT ON RHS IS A FUNCTION
!              BLOCK NAME.
!
      IF(IHARG(4).EQ.IFBNA1(1:4) .AND. IHARG2(4).EQ.IFBNA1(5:8))THEN
        IFLGFB=1
      ELSEIF(IHARG(4).EQ.IFBNA2(1:4) .AND. IHARG2(4).EQ.IFBNA2(5:8))THEN
        IFLGFB=2
      ELSEIF(IHARG(4).EQ.IFBNA3(1:4) .AND. IHARG2(4).EQ.IFBNA3(5:8))THEN
        IFLGFB=3
      ELSE
        IFLGFB=0
      ENDIF
!
!
      IWD1=IHARG(3)
      IWD12=IHARG2(3)
      IWD2='WRT '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,   &
                  IFUNC2,N2,IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(IFOUND.EQ.'YES')GO TO 3500
!
      IWD1=IHARG(3)
      IWD12=IHARG2(3)
      IWD2='FOR '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,   &
                  IFUNC2,N2,IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(IFOUND.EQ.'YES')GO TO 3500
!
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
 3104 FORMAT('      LET ... = INTEGRAL ... WRT  ... ',   &
             'FOR ... = ... TO ...')
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
      GO TO 9000
!
 3500 CONTINUE
!
!               *********************************************************
!               **  STEP 4--                                            *
!               **  DETERMINE IF THE EXPRESSION HAS ANY FUNCTION NAMES  *
!               **  INBEDDED.  IF SO, REPLACE THE FUNCTION NAMES        *
!               **  BY EACH FUNCTION'S DEFINITION.  DO SO REPEATEDLY    *
!               **  UNTIL ALL FUNCTION REFERENCES HAVE BEEN ANNIHILATED *
!               **  AND THE EXPRESSION IS LEFT ONLY WITH CONSTANTS,     *
!               **  PARAMETERS, AND VARIABLES--NO FUNCTIONS.  PLACE THE *
!               **  RESULTING FUNCTIONAL EXPRESSION INTO IFUNC3(.)      *
!               *********************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFLGFB.LT.1)THEN
        CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,   &
                    NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,   &
                    IFUNC3,N3,MAXN3,   &
                    IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
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
!
      ENDIF
!
!               *************************************
!               **  STEP 5--                       **
!               **  EXTRACT QUALIFIER INFORMATION. **
!               *************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *********************************************************
!               **  STEP 5.1--                                         **
!               **  DETERMINE THE DUMMY VARIABLE FOR THE INTEGRATION.  **
!               *********************************************************
!
      IKEY='WRT '
      IKEY2='    '
      ISHIFT=1
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                  IHARG,IHARG2,NUMARG,   &
                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                  IUSE,IN,NUMNAM,   &
                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,   &
                  IVOUT,VOUT,IUOUT,   &
                  INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO' .OR. IFOUN2.EQ.'NO')THEN
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=1
        ILOCA=1
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                    IHARG,IHARG2,NUMARG,   &
                    INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                    IUSE,IN,NUMNAM,   &
                    IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,   &
                    IVOUT,VOUT,IUOUT,   &
                    INOUT,IBUGA3,IERROR)
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
          GO TO 9000
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
!
!               **************************************************
!               **  STEP 5.2--                                  **
!               **  DETERMINE THE LIMITS FOR   THE INTEGRATION. **
!               **************************************************
!
      ISTEPN='5.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMLIM=0
!
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=3
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                  IHARG,IHARG2,NUMARG,   &
                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                  IUSE,IN,NUMNAM,   &
                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,   &
                  VOUT,IUOUT,   &
                  INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')THEN
        XMIN=VOUT
        NUMLIM=NUMLIM+1
      ENDIF
!
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=4
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                  IHARG,IHARG2,NUMARG,   &
                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                  IUSE,IN,NUMNAM,   &
                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,   &
                  VOUT,IUOUT,   &
                  INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')THEN
        IF(IHOUT.EQ.'TO  ' .AND. IHOUT2.EQ.'    ')THEN
          CONTINUE
        ELSE
          XMAX=VOUT
          ILOCMX=ILOC2
          NUMLIM=NUMLIM+1
        ENDIF
      ENDIF
!
      IF(NUMLIM.LE.1)THEN
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=5
        ILOCA=1
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                    IHARG,IHARG2,NUMARG,   &
                    INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                    IUSE,IN,NUMNAM,   &
                    IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,   &
                    ILOUT,IVOUT,VOUT,IUOUT,   &
                    INOUT,IBUGA3,IERROR)
        IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')THEN
          XMAX=VOUT
          ILOCMX=ILOC2
          NUMLIM=NUMLIM+1
        ENDIF
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
!               **********************************************
!               **  STEP 6.3--                              **
!               **  SCAN THE QUALIFIERS FOR VARIABLE,       **
!               **  PARAMETER, FUNCTION, AND VALUE CHANGES  **
!               **  IN THE FUNCTION.                        **
!               **********************************************
!
      ISTEPN='6.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NCHANG=0
      DO 6300 IFORI=1,10
!
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=1
        IF(IFORI.EQ.1)ILOCA=ILOCMX
        IF(IFORI.NE.1)ILOCA=ILOC3
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                    IHARG,IHARG2,NUMARG,   &
                    INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                    IUSE,IN,NUMNAM,   &
                    IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,   &
                    VOUT,IUOUT,   &
                    INOUT,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 6380
        IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GO TO 6390
!
        ILOC3=ILOC2+2
        IF(ILOC3.GT.NUMARG)GO TO 6380
        NCHANG=NCHANG+1
        IOLD(NCHANG)=IHARG(ILOC2)
        IOLD2(NCHANG)=IHARG2(ILOC2)
        INEW(NCHANG)=IHARG(ILOC3)
        INEW2(NCHANG)=IHARG2(ILOC3)
!
 6300 CONTINUE
      GO TO 6390
!
 6380 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6302)
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
      GO TO 9000
!
 6390 CONTINUE
!
!               **********************************************
!               **  STEP 6.4--                              **
!               **  CARRY OUT THE VARIABLE,                 **
!               **  PARAMETER, AND FUNCTION CHANGES         **
!               **  AND THEN PRINT OUT A BRIEF MESSAGE      **
!               **  INDICATING THAT THE CHANGES             **
!               **  HAVE BEEN MADE.                         **
!               **********************************************
!
      ISTEPN='6.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON' .AND. NCHANG.GT.0 .AND.   &
         IFLGFB.LE.0)THEN
!
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
!
        CALL COMPIC(IFUNC3,N3,IOLD,IOLD2,INEW,INEW2,NCHANG,IFUNC3,N3,   &
                    IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        ILAB(1)='POST'
        ILAB(2)='-CHA'
        ILAB(3)='NGE '
        ILAB(4)='FUNC'
        ILAB(5)='TION'
        ILAB(6)='  = '
        NUMWDL=6
        CALL DPPRIF(ILAB,NUMWDL,IFUNC3,N3,IBUGA3)
!
      ENDIF
!
!               *********************************************************
!               **  STEP 6.7--                                         **
!               **  MAKE A NON-CALCULATING PASS AT THE FUNCTION        **
!               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES. **
!               *********************************************************
!
      ISTEPN='6.8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!
      IPASS=1
      IF(IFLGFB.LE.0)THEN
        CALL COMPIM(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
                    IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,   &
                    IBUGCO,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ELSE
        GO TO 7650
      ENDIF
!
!               ***********************************************
!               **  STEP 7--                                 **
!               **  CHECK THAT ALL PARAMETERS                **
!               **  IN THE FUNCTION ARE ALREADY PRESENT      **
!               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
!               ***********************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IP=0
      IV=0
      IF(NUMPV.LE.0)GO TO 7650
      DO 7600 J=1,NUMPV
        IHPARN=IPARN(J)
        IHPAR2=IPARN2(J)
        IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1))THEN
           IV=IV+1
           LOCDUM=J
           GO TO 7600
        ENDIF
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHPARN,IHPAR2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'YES')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7612)
 7612     FORMAT('      A PARAMETER/FUNCTION HAS BEEN ENCOUNTERED IN ',   &
                 'THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7613)
 7613     FORMAT('      FUNCTION TO BE INTEGRATED WHICH HAS NOT YET ',   &
                 'BEEN DEFINED')
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
          GO TO 9000
        ENDIF
!
        IP=IP+1
        PARAM(J)=VALUE(ILOCP)
!
 7600 CONTINUE
 7650 CONTINUE
!
!               ******************************
!               **  STEP 8--                **
!               **  DETERMINE THE INTEGRAL  **
!               ******************************
!
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
        DO 7714 I=1,NUMDV
          WRITE(ICOUT,7715)I,IDUMV(I),IDUMV2(I)
 7715     FORMAT('I,IDUMV(I),IDUMV2(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
 7714   CONTINUE
      ENDIF
!
!     2013/06: CALL QUADPACK ROUTINE "QAGI" IF AN INDEFINITE INTEGRAL
!              DETECTED.
!
      IF(XMIN.EQ.CPUMIN .OR. XMAX.EQ.CPUMAX)THEN
!
!       COPY OVER DUMMY COMMON BLOCKS FOR OPTFUN ROUTINE
!
        DO 7805 KK=1,MAXF3
          ZMODEL(KK)=IFUNC3(KK)
 7805   CONTINUE
        DO 7810 KK=1,IOPTCH
          ZTYPEH(KK)=ITYPEH(KK)
          ZW21HO(KK)=IW21HO(KK)
          ZW22HO(KK)=IW22HO(KK)
          Z2HOLD(KK)=W2HOLD(KK)
 7810   CONTINUE
        DO 7820 KK=1,IOPTC2
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
!
        IF(XMIN.EQ.CPUMIN .AND. XMAX.EQ.CPUMAX)THEN
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
!
        CALL QAGI(BOUND,INF,EPSABS,EPSREL,XINT,ABSERR,NEVAL,   &
                  IER,LIMIT,LENW,LAST,ITEMP1,TEMP1)
!
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
!
        IF(IER.GE.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
        ENDIF
!
        IF(IER.EQ.1)THEN
          WRITE(ICOUT,8103)
 8103     FORMAT('      QAGI: MAXIMUM AKMBER OF SUBDIVISIONS EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IER.EQ.2)THEN
          WRITE(ICOUT,8105)
 8105     FORMAT('      QAGI: ROUNDOFF ERROR PREVENTS REQUESTED ',   &
                 'TOLERANCE FROM BEING ACHIEVED.')
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
 8113     FORMAT('      QAGI: INVALID INPUT TO THE INTEGRATION ',   &
                 'ROUTINE.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ELSE
        CALL DPINT2(IFUNC3,N3,PARAM,IPARN,IPARN2,NUMPV,   &
                    IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,   &
                    IDUMV,IDUMV2,NUMDV,XMIN,XMAX,XINT,   &
                    IFLGFB,IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                    NUMNAM,MAXNAM,IFTEXP,IFTORD,IFORSW,   &
                    PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,V,MAXN,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,MAXCP4,MAXCP5,MAXCP6,   &
                    IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
      ENDIF
!
!               *****************************************************
!               **  STEP 9--                                       **
!               **  ENTER THE INTEGRATION VALUE INTO THE DATAPLOT  **
!               **  HOUSEKEEPING ARRAY                             **
!               *****************************************************
!
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'INTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHL=IHLEFT
      IHL2=IHLEF2
      ICASEL='P'
      IXINT=INT(XINT+0.5)
!CCCC THE FOLLOWING 2 LINES WERE ADDED NOVEMBER 1989
      BJUNK(1)=AJUNK
      NJUNK=1
!CCCC THE FOLLOWING LINE WAS CHANGED NOVEMBER 1989
!CCCC CALL DPINVP(IHL,IHL2,ICASEL,AJUNK,NJUNK,XINT,IXINT,
      CALL DPINVP(IHL,IHL2,ICASEL,BJUNK,NJUNK,XINT,IXINT,   &
                  ISUBN1,ISUBN2,IBUGA3,IERROR)
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT      **
!               ****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'INTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPINTE--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                           IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',   &
                 I8,2X,A4,A4,2X,A4,2I8)
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
!
      RETURN
      END SUBROUTINE DPINTE
      SUBROUTINE DPINVP(IHLEFT,IHLEF2,ICASEL,VLEFT,NLEFT,PLEFT,ILEFT,   &
      ISUBN3,ISUBN4,IBUGA3,IERROR)
!
!     PURPOSE--INSERT THE VARIABLE OR PARAMETER
!              WITH NAME   IHLEFT
!              INTO THE INTERNAL DATAPLOT TABLE.
!              ALSO, UPDATE INTERNAL DATAPLOT
!              LISTS (IF NECESSARY).
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JULY      1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 ICASEL
      CHARACTER*4 ISUBN3
      CHARACTER*4 ISUBN4
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION VLEFT(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIN'
      ISUBN2='VP  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
!
      IERROR='NO'
!
      ICOLL=0
!
!               ******************************************
!               **  INSERT A VARIABLE                   **
!               **  INTO THE GENERAL DATAPLOT           **
!               **  ARRAY V(.)  ; OR                    **
!               **  INSERT A PARAMETER VALUE            **
!               **  INTO THE INTERNAL DATAPLOT TABLE.   **
!               **  MAKE ADJUSTMENTS TO THE             **
!               **  INTERNAL DATAPLOT LISTS.            **
!               ******************************************
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPINVP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IHLEFT,IHLEF2,ICASEL,NLEFT,PLEFT
   52 FORMAT('IHLEFT,IHLEF2,ICASEL,NLEFT,PLEFT = ',   &
      A4,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)VLEFT(1),VLEFT(NLEFT)
   53 FORMAT('VLEFT(1),VLEFT(NLEFT) = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMNAM,MAXNAM,NUMCOL,MAXN,MAXCOL
   54 FORMAT('NUMNAM,MAXNAM,NUMCOL,MAXN,MAXCOL = ',5I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='NO'
      NEWCOL='NO'
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  DETERMINE WHETHER OR NOT THE NAME IN IHLEFT      **
!               **  ALREADY EXISTS IN THE INTERNAL IHNAME(.) TABLE.  **
!               **  THE 'YES' OR 'NO' RESULT IS PLACED IN    NEWNAM. **
!               **  THE LINE IN THE TABLE IS PLACED INTO ILISTL.     **
!               **  DETERMINE ALSO IF THE NUMBER OF NAMES            **
!               **  IN THE IHNAME(.) TABLE EXCEEDS THE               **
!               **  MAXIMUM ALLOWABLE NUMBER (MAXNAM).               **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 2000 I=1,NUMNAM
      I2=I
      IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))GO TO 2030
 2000 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      GO TO 2050
 2030 CONTINUE
      NEWNAM='NO'
      ILISTL=I2
 2050 CONTINUE
!
      IF(ILISTL.LE.MAXNAM)GO TO 2090
      WRITE(ICOUT,2051)ISUBN1,ISUBN2,ISUBN3,ISUBN4
 2051 FORMAT('***** ERROR IN ',A4,A4,'AS CALLED FROM ',A4,A4,'--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2052)
 2052 FORMAT('      THE NUMBER OF VARIABLE/PARAMETER',   &
      '/FUNCTION NAMES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2053)MAXNAM
 2053 FORMAT('      HAS JUST EXCEEDED THE MAX ALLOWABLE (= ',   &
      I8,')   .')
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
      GO TO 9000
 2090 CONTINUE
!
!               ***************************************
!               **  STEP 3--                         **
!               **  IF OUTPUT IS TO BE A VARIABLE,   **
!               **  DETERMINE WHAT COLUMN IN V(.)    **
!               **  THE OUTPUT WILL GO.              **
!               **  THE RESULT WILL BE PLACED        **
!               **  INTO  ICOLL    .                 **
!               ***************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEL.NE.'V')GO TO 3099
!
      IF(NEWNAM.EQ.'YES')NEWCOL='YES'
      IF(NEWNAM.EQ.'YES')ICOLL=NUMCOL+1
!
      IF(NEWNAM.EQ.'NO'.AND.IUSE(ILISTL).NE.'V')NEWCOL='YES'
      IF(NEWNAM.EQ.'NO'.AND.IUSE(ILISTL).NE.'V')ICOLL=NUMCOL+1
!
      IF(NEWNAM.EQ.'NO'.AND.IUSE(ILISTL).EQ.'V')NEWCOL='NO'
      IF(NEWNAM.EQ.'NO'.AND.IUSE(ILISTL).EQ.'V')ICOLL=IVALUE(ILISTL)
!
 3099 CONTINUE
!
!               *****************************************
!               **  STEP 4--                           **
!               **  DETERMINE IF THE COLUMN IN V(.)    **
!               **  WOULD EXCEED THE MAX ALLOWABLE     **
!               **  NUMBER OF COLUMNS.                 **
!               *****************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEL.NE.'V')GO TO 4099
      IF(ICASEL.EQ.'V'.AND.ICOLL.LE.MAXCOL)GO TO 4099
!
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
      GO TO 9000
 4099 CONTINUE
!
!               *******************************************
!               **  STEP 5--                             **
!               **  IF OUTPUT IS TO BE A VARIABLE,       **
!               **  ENTER THE CONTENTS OF VLEFT(.)       **
!               **  (ALL NLEFT ELEMENTS OF VLEFT(.))     **
!               **  INTO COLUMN     ICOLL    OF V(.)  .  **
!               *******************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEL.NE.'V')GO TO 5099
      IF(NLEFT.LE.0)GO TO 5099
      IF(NLEFT.LE.MAXN)GO TO 5039
!
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
      GO TO 9000
 5039 CONTINUE
!
      DO 5070 I=1,NLEFT
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)V(IJ)=VLEFT(I)
      IF(ICOLL.EQ.MAXCP1)PRED(I)=VLEFT(I)
      IF(ICOLL.EQ.MAXCP2)RES(I)=VLEFT(I)
 5070 CONTINUE
!
 5099 CONTINUE
!
!               *******************************************
!               **  STEP 7--                             **
!               **  CARRY OUT THE LIST UPDATING AND      **
!               **  GENERATE THE INFORMATIVE PRINTING    **
!               *******************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEL.EQ.'P')GO TO 7010
      IF(ICASEL.EQ.'V')GO TO 7020
      GO TO 9000
!
 7010 CONTINUE
      IHNAME(ILISTL)=IHLEFT
      IHNAM2(ILISTL)=IHLEF2
      VALUE(ILISTL)=PLEFT
      IVALUE(ILISTL)=ILEFT
      IN(ILISTL)=ILEFT
      IUSE(ILISTL)='P'
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
!
      IF(IFEEDB.EQ.'OFF')GO TO 7019
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7011)IHLEFT,IHLEF2,VALUE(ILISTL)
 7011 FORMAT('THE COMPUTED VALUE OF THE CONSTANT   ',A4,A4,   &
      ' = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 7019 CONTINUE
      GO TO 7190
!
 7020 CONTINUE
      IHNAME(ILISTL)=IHLEFT
      IHNAM2(ILISTL)=IHLEF2
      IUSE(ILISTL)='V'
      IVALUE(ILISTL)=ICOLL
      VALUE(ILISTL)=ICOLL
      IN(ILISTL)=NLEFT
!
!CCCC IUSE(ICOLL)='V'
!CCCC IVALUE(ICOLL)=ICOLL
!CCCC VALUE(ICOLL)=ICOLL
!CCCC IN(ICOLL)=NLEFT
!
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWCOL.EQ.'YES')NUMCOL=NUMCOL+1
!
      DO 7100 I=1,NUMNAM
      I2=I
      IF(IUSE(I).EQ.'V'.AND.IVALUE(I).EQ.ICOLL)GO TO 7105
      GO TO 7100
 7105 CONTINUE
      IUSE(I2)='V'
      IVALUE(I2)=ICOLL
      VALUE(I2)=ICOLL
      IN(I2)=NLEFT
 7100 CONTINUE
!
      NS=NLEFT
      IF(IFEEDB.EQ.'OFF')GO TO 7119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7111)IHLEFT,IHLEF2,NS
 7111 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
 7119 CONTINUE
!
      IROW1=1
      IROWN=NLEFT
!
      IF(IFEEDB.EQ.'OFF')GO TO 7149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IJ1=MAXN*(ICOLL-1)+IROW1
      IF(ICOLL.LE.MAXCOL)WRITE(ICOUT,7121)IHLEFT,IHLEF2,V(IJ1),   &
      IROW1
 7121 FORMAT('THE FIRST          COMPUTED VALUE OF ',A4,A4,   &
      ' = ',E15.7,' (ROW ',I5,')')
      IF(ICOLL.LE.MAXCOL)CALL DPWRST('XXX','BUG ')
      IF(ICOLL.EQ.MAXCP1)WRITE(ICOUT,7121)IHLEFT,IHLEF2,PRED(IROW1),   &
      IROW1
      IF(ICOLL.EQ.MAXCP1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL.EQ.MAXCP2)WRITE(ICOUT,7121)IHLEFT,IHLEF2,RES(IROW1),   &
      IROW1
      IF(ICOLL.EQ.MAXCP2)CALL DPWRST('XXX','BUG ')
      IJN=MAXN*(ICOLL-1)+IROWN
      IF(ICOLL.LE.MAXCOL.AND.   &
      NS.NE.1)WRITE(ICOUT,7131)NS,IHLEFT,IHLEF2,V(IJN),IROWN
      IF(ICOLL.LE.MAXCOL.AND.   &
      NS.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL.EQ.MAXCP1.AND.   &
      NS.NE.1)WRITE(ICOUT,7131)NS,IHLEFT,IHLEF2,PRED(IROWN),IROWN
      IF(ICOLL.EQ.MAXCP1.AND.   &
      NS.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL.EQ.MAXCP2.AND.   &
      NS.NE.1)WRITE(ICOUT,7131)NS,IHLEFT,IHLEF2,RES(IROWN),IROWN
 7131 FORMAT('THE LAST (',I5,'TH) COMPUTED VALUE OF ',A4,A4,   &
      ' = ',E15.7,' (ROW ',I5,')')
      IF(ICOLL.EQ.MAXCP2.AND.   &
      NS.NE.1)CALL DPWRST('XXX','BUG ')
      IF(NS.NE.1)GO TO 7180
!
      WRITE(ICOUT,7142)
 7142 FORMAT('NOTE--THE ABOVE VALUE WAS THE ONLY VALUE COMPUTED ',   &
      'FOR THIS VARIABLE.')
      CALL DPWRST('XXX','BUG ')
 7149 CONTINUE
 7180 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 7189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7182)IHLEFT,IHLEF2,ICOLL
 7182 FORMAT('THE CURRENT COLUMN FOR ',   &
      'THE VARIABLE ',A4,A4,'  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7183)IHLEFT,IHLEF2,NLEFT
 7183 FORMAT('THE CURRENT LENGTH OF  ',   &
      'THE VARIABLE ',A4,A4,'  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 7189 CONTINUE
!
 7190 CONTINUE
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT      **
!               ****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DPINVP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IHLEFT,IHLEF2,ICASEL,NLEFT,PLEFT,ILEFT
 9012 FORMAT('IHLEFT,IHLEF2,ICASEL,NLEFT,PLEFT,ILEFT = ',   &
      A4,A4,2X,A4,I8,E15.7,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)VLEFT(1),VLEFT(NLEFT)
 9013 FORMAT('VLEFT(1),VLEFT(NLEFT) = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NEWNAM,ILISTL,ICOLL,NUMNAM
 9015 FORMAT('NEWNAM,ILISTL,ICOLL,NUMNAM = ',A4,I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IHNAME(ILISTL),IHNAM2(ILISTL),IVALUE(ILISTL),   &
      VALUE(ILISTL)
 9016 FORMAT('IHNAME(ILISTL),IHNAM2(ILISTL),IVALUE(ILISTL),',   &
      'VALUE(ILISTL) = ',A4,A4,2X,I8,2X,E15.7)
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
!
      RETURN
      END SUBROUTINE DPINVP
      SUBROUTINE DPISNU(ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--GIVEN A STRING, DETERMINE IF THIS IS A VALID NUMBER
!              OR NOT.  IF IT IS A NUMBER, RETURN A 1, OTHERWISE
!              RETURN A 0.
!     EXAMPLE--LET IVAL = IS NUMBER SORG
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2020/11
!     ORIGINAL VERSION--NOVEMBER  2020.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEL
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*80 SORG
!
!---------------------------------------------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOZI.INC'
!
      INTEGER ITEMP1(MAXOBV)
!
      EQUIVALENCE(ITEMP1(1),IGARBG(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIS'
      ISUBN2='NU  '
      IERROR='NO'
!
!               *****************************************************
!               **  TREAT THE SUBCASE OF THE LET FUNCTION COMMAND  **
!               **  WHICH DEFINES A FUNCTION                       **
!               *****************************************************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ISNU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPISNU--')
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
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ISNU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='NO'
      NEWCOL='NO'
      ICASEL='UNKN'
      NIOLD=0
      ICOLL=0
      ICOL2=0
      ISNUM=0
!
!               ******************************************************
!               **  STEP 2--                                         *
!               **  EXAMINE THE LEFT-HAND SIDE--                     *
!               **  IF THIS IS A PREVIOUSLY DEFINED NAME, IT SHOULD  *
!               **  BE A PARAMETER.                                  *
!               ******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ISNU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
!
      DO 2000 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          IF(IUSE(I2).EQ.'P')THEN
            ICASEL='PARA'
            ILISTL=I2
            GO TO 2900
          ELSE
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
 2001       FORMAT('***** ERROR IN  IS NUMBER  (DPISNU)--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2003)IHLEFT,IHLEF2
 2003       FORMAT('      THE NAME ON THE LEFT HAND SIDE (',2A4,')')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2005)
 2005       FORMAT('      ALREADY EXISTS, BUT NOT AS A PARAMETER.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
 2000 CONTINUE
!
      NEWNAM='YES'
      ICASEL='PARA'
!
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND ',   &
               'FUNCTION')
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
 2206   FORMAT('      THEN REDEFINE OR DELETE SOME OF THE ALREADY ',   &
               'USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
 2900 CONTINUE
!
!               *****************************************************
!               **  STEP 3--                                       **
!               **  EXTRACT THE NAME ON THE RIGHT HAND SIDE        **
!               *****************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ISNU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHRIGH=IHARG(5)
      IHRIG2=IHARG2(5)
      DO 3000 I=1,NUMNAM
        I4=I
        IF(IHRIGH.EQ.IHNAME(I).AND.IHRIG2.EQ.IHNAM2(I))THEN
          IF(IUSE(I4).NE.'F')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3003)IHRIGH,IHRIG2
 3003       FORMAT('      THE NAME ON THE RIGHT HAND SIDE (',2A4,')')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3005)
 3005       FORMAT('      ALREADY EXISTS, BUT NOT AS A STRING.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ELSE
            ISTART=IVSTAR(I4)
            ISTOP=IVSTOP(I4)
            NLEN=ISTOP-ISTART+1
            IF(NLEN.GT.80)THEN
              ISNUM=0
              GO TO 3900
            ELSE
              SORG=' '
              DO 3010 J=1,NLEN
                IINDX=ISTART+J-1
                SORG(J:J)=IFUNC(IINDX)(1:1)
 3010         CONTINUE
!
              READ(SORG,*,IOSTAT=IE)AVAL
              IF(IE.EQ.0)ISNUM=1
              GO TO 3900
            ENDIF
          ENDIF
        ENDIF
 3000 CONTINUE
!
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
      GO TO 9000
!
 3900 CONTINUE
!
!               *****************************************************
!               **  STEP 4--                                       **
!               **  SAVE PARAMETER                                 **
!               *****************************************************
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ISNU')THEN
        ISTEPN='4'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,4011)ISTART,ISTOP,NLEN,ISNUM,IE
 4011   FORMAT('ISTART,ISTOP,NLEN,IE = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4013)ICASEL,SORG(1:NLEN)
 4013   FORMAT('ICASEL,SORG = ',A4,2X,A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ICASEL.EQ.'PARA')THEN
!
        ISTEPN='4A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ISNU')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IHNAME(ILISTL)=IHLEFT
        IHNAM2(ILISTL)=IHLEF2
        IUSE(ILISTL)='P'
        VALUE(ILISTL)=REAL(ISNUM)
        IVALUE(ILISTL)=INT(ISNUM)
        IN(ILISTL)=1
        IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
!
        IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,15111)IHLEFT,IHLEF2,ISNUM
15111     FORMAT(2A4,' = ',I6)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ISNU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPISNU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMNAM,ISTART,ISTOP,NLEN,IE,ISNUM
 9013   FORMAT('NUMNAM,ISTART,ISTOP,NLEN,IE,ISNUM = ',6I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                           IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),',   &
                 'IVSTOP(I)=',I8,2X,2A4,2X,A4,2I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPISNU
      SUBROUTINE DPISOP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A Z-SCORE VERSUS LAB AVERAGE PLOT AS GIVEN
!              IN THE ISO 13528 STANDARD.
!
!              THE COMMAND HAS THE FOLLOWING FORMAT:
!
!                  ISO 13528 PLOT Y Z ROUND LABID LAB
!
!              WHERE Y IS THE ORIGINAL RESPONSE, Z IS THE Z-SCORE OF THE
!              RESPONSE, ROUND IS THE ROUND-ID, LABID IS THE LAB-ID FOR
!              ALL LABS, AND LAB IDENTIFIES THE LABS FOR WHICH THE PLOT
!              WILL BE GENERATED (TYPICALLY, THIS WILL BE A SINGLE LAB).
!
!              IN SOME CASES, ONLY THE Z-SCORES WILL BE AVAILABLE.
!              IN THIS CASE, Y WILL DENOTE THE LAB AVERAGES IN THE
!              ORIGINAL UNITS.
!
!              THE PLOT IS:
!
!                  VERTICAL AXIS: FOR A GIVEN LAB, THE Z-SCORES FOR EACH
!                                 ROUND.
!                  HORIZONRAL AXIS: FOR EACH ROUND, COMPUTE THE AVERAGE
!                                   OVER ALL LABORATORIES.
!
!              YOU CAN OPTIONALLY PROVIDE A MATERIAL-ID VARIABLE.
!              THIS IS ESSENTIALLY A HIGHLIGHTING VARIABLE (I.E.,
!              DIFFERENT MATERIALS CAN BE PLOTTED WITH DIFFERENT
!              PLOT CHARACTERS).  THIS FORM HAS THE SYNTAX
!
!                  ISO 13528 PLOT Y Z ROUND LABID MATID LAB
!
!     EXAMPLE--ISO 13528 PLOT Y Z LAB LABA
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/2
!     ORIGINAL VERSION--FEBRUARY   2012.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
!
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
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPIS'
      ISUBN2='OP  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ****************************************
!               **  TREAT THE DEX CONTOUR PLOT CASE   **
!               ****************************************
!
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
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.2 .AND. ICOM.EQ.'ISO ' .AND.IHARG(1).EQ.'1352' .AND.   &
         IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
        ICASPL='1352'
      ELSE
        GO TO 9000
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!               **********************************************
!               **  STEP 33--                               **
!               **  FORM THE SUBSETTED VARIABLES            **
!               **       Y(.)                               **
!               **       Z(.)                               **
!               **       ROUND(.)                           **
!               **       ALABID(.)                          **
!               **       AMATID(.)                          **
!               **  CONTAINING                              **
!               **       THE RESPONSE VARIABLE (ORIGINAL    **
!               **           UNITS)                         **
!               **       THE Z-SCORE OF THE RESPONSE        **
!               **       THE ROUND-ID                       **
!               **       THE LAB-ID                         **
!               **       THE MATERIAL-ID                    **
!               **  RESPECTIVELY.                           **
!               **********************************************
!
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NRIGHT(1).EQ.NRIGHT(2))THEN
        ICOL=1
        NUMVA2=NUMVAR-1
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    YRAW,Z,ROUND,ALABID,AMATID,TEMP1,TEMP1,NS,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NAVE=NS
      ELSE
        ICOL=2
        NUMVA2=NUMVAR-2
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Z,ROUND,ALABID,TEMP1,TEMP1,TEMP1,TEMP1,NS,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        ICOL=1
        NUMVA2=1
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    YRAW,TEMP1,TEMP1,TEMP1,TEMP1,TEMP1,TEMP1,NAVE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
      IF(NUMVAR.EQ.5)THEN
        DO 3310 I=1,NS
          AMATID(I)=1.0
 3310   CONTINUE
      ENDIF
!
!               **********************************************
!               **  STEP 34--                               **
!               **  FORM THE FULL VARIABLE                  **
!               **       ALAB(.)                            **
!               **  CONTAINING THE VALUES OF THE LABS FOR   **
!               **  WHICH THE PLOT WILL BE GENERATED.       **
!               **********************************************
!
      ISTEPN='34'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOL=NUMVAR
      NUMVA2=1
      NQ=NRIGHT(ICOL)
      DO 3410 I=1,NQ
        ISUB(I)=1
 3410 CONTINUE
!
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  ALAB,TEMP1,TEMP1,NLAB,NLAB,NLAB,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               *******************************************************
!               **  STEP 8--                                         **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS            **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.               **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).    **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).    **
!               *******************************************************
!
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,5001)NS,NAVE,NLAB,ICASPL
 5001   FORMAT('NS,NAVE,NLAB,ICASPL=',3I8,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL DPISO2(YRAW,Z,ROUND,ALABID,AMATID,ALAB,NS,NLAB,NAVE,   &
                  ICASPL,NUMVAR,MAXOBV,IISOLA,IISOME,   &
                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,   &
                  PPA0,PPA1,PPA0SD,PPA1SD,   &
                  Y,X,D,   &
                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               ***************************************
!               **  STEP 9--                         **
!               **  GENERATE FIT FOR EACH LAB IN     **
!               **  ALAB VARIABLE.                   **
!               ***************************************
!
      ISTEPN='9'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ISOP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPISO4(PPA0,PPA1,PPA0SD,PPA1SD,NLAB,   &
                  IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 9--   **
!               **  EXIT       **
!               *****************
!
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
 9014   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',   &
               3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPISOP
      SUBROUTINE DPISO2(YRAW,Z,ROUND,ALABID,AMATID,ALAB,N,NLAB,NAVE,   &
                        ICASPL,NUMVAR,MAXOBV,IISOLA,IISOME,   &
                        YMEAN,XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,   &
                        PPA0,PPA1,PPA0SD,PPA1SD,   &
                        Y,X,D,   &
                        NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A Z-SCORE VERSUS LAB AVERAGE PLOT AS GIVEN
!              IN THE ISO 13528 STANDARD.
!
!              THE COMMAND HAS THE FOLLOWING FORMAT:
!
!                  ISO 13528 PLOT Y Z ROUND LABID MATID LAB
!
!              WHERE Y IS THE ORIGINAL RESPONSE, Z IS THE Z-SCORE OF THE
!              RESPONSE, ROUND IS THE ROUND-ID, LABID IS THE LAB-ID FOR
!              ALL LABS, MATID IS THE MATERIAL ID, AND LAB IDENTIFIES
!              THE LABS FOR WHICH THE PLOT WILL BE GENERATED (TYPICALLY,
!              THIS WILL BE A SINGLE LAB).
!
!              THE PLOT IS:
!
!                  VERTICAL AXIS: FOR A GIVEN LAB, THE Z-SCORES FOR EACH
!                                 ROUND.
!                  HORIZONRAL AXIS: FOR EACH ROUND, COMPUTE THE AVERAGE
!                                   OVER ALL LABORATORIES.
!
!     REFERENCE--ISO 13528 (2005), "Statistical Methods for use in
!                proficiency testing by interlaboratory comparisons,"
!                First Edition, 2005-09-01, pp. 56-57.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/2
!     ORIGINAL VERSION--FEBRUARY  2012.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IISOLA
      CHARACTER*4 IISOME
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Z(*)
      DIMENSION YRAW(*)
      DIMENSION ROUND(*)
      DIMENSION ALABID(*)
      DIMENSION AMATID(*)
      DIMENSION ALAB(*)
!
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
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
!
      DOUBLE PRECISION DSUM1
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIS'
      ISUBN2='O2  '
      IWRITE='OFF'
      IERROR='NO'
!
      NPLOTP=0
      NPLOTV=3
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ISO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPISO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IBUGG3,ISUBRO,ICASPL,IISOLA,N,NLAB,NAVE,NUMVAR
   72   FORMAT('IBUGG3,ISUBRO,ICASPL,IISOLA,N,NLAB,NUMVAR = ',   &
               4(A4,2X),4I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO 81 I=1,N
            WRITE(ICOUT,82)I,YRAW(I),Z(I),ROUND(I),ALABID(I),AMATID(I)
   82       FORMAT('I,YRAW(I),Z(I),ROUND(I),ALABID(I),AMATID(I) = ',   &
                   I8,5G15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
        IF(NLAB.GT.0)THEN
          DO 86 I=1,NLAB
            WRITE(ICOUT,87)I,ALAB(I)
   87       FORMAT('I,ALAB(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   86     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
        GO TO 9000
      ENDIF
!
!               ********************************************
!               **  STEP 2--                              **
!               **  COMPUTE LAB AVERAGES (IN ORIGINAL     **
!               **  UNITS) FOR EACH ROUND OVER ALL LABS.  **
!               ********************************************
!
      IWRITE='OFF'
      CALL CODE(AMATID,N,IWRITE,XIDTEM,XIDTE2,MAXOBV,IBUGG3,IERROR)
      AMAX=CPUMIN
      DO 110 I=1,N
        AMATID(I)=XIDTEM(I)
        IF(AMATID(I).GT.AMAX)AMAX=AMATID(I)
  110 CONTINUE
!
      CALL DISTIN(ROUND,N,IWRITE,XIDTEM,NROUND,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NROUND,XIDTEM)
      CALL DISTIN(AMATID,N,IWRITE,XIDTE2,NMAT,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NMAT,XIDTE2)
!
      IF(NAVE.EQ.N)THEN
        ICNT=0
        DO 1010 IRND=1,NROUND
          HOLD=XIDTEM(IRND)
          DO 1020 IMAT=1,NMAT
            HOLD2=XIDTE2(IMAT)
            DSUM1=0.0D0
            K=0
!
            DO 1030 J=1,N
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
                  CALL H15(TEMP1,K,C,NCUT,XMEAN,XSC,TEMP2,TEMP3,   &
                           MAXOBV,ISUBRO,IBUGG3)
                ELSEIF(IISOME.EQ.'MEDI')THEN
                  CALL MEDIAN(TEMP1,K,IWRITE,TEMP2,MAXOBV,XMEAN,   &
                              IBUGG3,IERROR)
                ENDIF
              ENDIF
            ELSEIF(IISOLA.EQ.'LAVE')THEN
              IF(K.EQ.0)THEN
                XMEAN=CPUMIN
              ELSE
                XMEAN=TEMP1(1)
              ENDIF
            ENDIF
!
            ICNT=ICNT+1
            YMEAN(ICNT)=XMEAN
!
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ISO2')THEN
              WRITE(ICOUT,1096)IRND,IMAT,ICNT,K,YMEAN(ICNT)
 1096         FORMAT('IRND,IMAT,ICNT,K,YMEAN(ICNT) = ',4I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
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
          GO TO 9000
        ENDIF
        ICNT=0
        DO 1040 I=1,NROUND
          HOLD=YRAW(I)
          DO 1050 J=1,NMAT
            ICNT=ICNT+1
            YMEAN(ICNT)=HOLD
 1050     CONTINUE
 1040   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 3--                              **
!               **  GENERATE THE PLOT COORDINATES.        **
!               ********************************************
!
      DO 2010 J=1,NLAB
        HOLD3=ALAB(J)
        NOLD=NPLOTP
        NTEMP=0
        ICNT=(J-1)*NMAT
!
        ICNT2=0
        DO 2020 IRND=1,NROUND
          HOLD=XIDTEM(IRND)
          DO 2030 IMAT=1,NMAT
            HOLD2=XIDTE2(IMAT)
            ICNT2=ICNT2+1
            AMEAN=YMEAN(ICNT2)
            DO 2035 IROW=1,N
              IF(ROUND(IROW).EQ.HOLD .AND. AMATID(IROW).EQ.HOLD2 .AND.   &
                 ALABID(IROW).EQ.HOLD3)THEN
                NPLOTP=NPLOTP+1
                Y(NPLOTP)=Z(IROW)
                X(NPLOTP)=AMEAN
                IINDX=ICNT+INT(AMATID(IROW)+0.1)
                D(NPLOTP)=REAL(IINDX)
                NTEMP=NTEMP+1
              ENDIF
 2035       CONTINUE
 2030     CONTINUE
 2020   CONTINUE
!
!       NOW COMPUTE A LINEAR FIT AND SAVE THE PARAMETER ESTIMATES
!       AND STANDARD ERRORS.  ADD OPTIONAL FITTED LINE TO GRAPH.
!
        ICNT=NLAB*NMAT + J
        IF(NTEMP.GE.2)THEN
          CALL LINFIT(Y(NOLD+1),X(NOLD+1),NTEMP,   &
                      ALOC,SLOPE,XRESSD,XRESDF,PPCC,A0SD,A1SD,CCALBE,   &
                      ISUBRO,IBUGG3,IERROR)
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
!
 2010 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9035 I=1,NPLOTP
            WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036       FORMAT('I,Y(I),X(I),D(I) = ',I8,2E15.7,F9.2)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPISO2
      SUBROUTINE DPISO4(PPA0,PPA1,PPA0SD,PPA1SD,NLAB,   &
                        IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--UTILITY ROUTINE USED BY DPISOP.  FOR EACH LAB, WRITE VALUES
!              OF FITTED LINE (INTERCEPT AND SLOPE WITH THEIR STANDARD ERRORS)
!              TO EXTERNAL FILE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORAOTRY
!                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/2
!     ORIGINAL VERSION--FEBRUARY  2012.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
!
      DIMENSION PPA0(*)
      DIMENSION PPA1(*)
      DIMENSION PPA0SD(*)
      DIMENSION PPA1SD(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               ***************************************
!               **  STEP 1--                         **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
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
!
      IERROR='NO'
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=0
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ISO4')THEN
        ISTEPN='2A'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,201)IOUNI1
  201   FORMAT('AFTER CALL DPOPFI, IOUNI1 = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      WRITE(IOUNI1,299)
  299 FORMAT(13X,'A0',13X,'A1',11X,'A0SD',11X,'A1SD',   &
             5X,'A0 t-VALUE',5X,'A1 t-VALUE')
!
      DO 1010 I=1,NLAB
        TVAL1=CPUMIN
        TVAL2=CPUMIN
        IF(PPA0SD(I).NE.CPUMIN .AND. PPA0SD(I).NE.0.0)THEN
          TVAL1=PPA0(I)/PPA0SD(I)
        ENDIF
        IF(PPA1SD(I).NE.CPUMIN .AND. PPA1SD(I).NE.0.0)THEN
          TVAL2=PPA1(I)/PPA1SD(I)
        ENDIF
        WRITE(IOUNI1,1031)PPA0(I),PPA1(I),PPA0SD(I),PPA1SD(I),   &
                          TVAL1,TVAL2
 1031   FORMAT(6E15.7)
 1010 CONTINUE
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ISO4')THEN
        ISTEPN='3A'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
  301   FORMAT('AFTER CALL DPCLFI')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ISO4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPISO4--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPISO4
      SUBROUTINE DPISP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                       MAXNXT,ISEED,ICONT,   &
                       ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A <STAT> INTERACTION PLOT
!              (SEE ROUTINE  EXTSTA  FOR A LIST OF SUPPORTED STATISTICS).
!              THESE DIFFER FROM THE STATISTIC PLOT CASE IN THAT THERE
!              CAN BE MORE THAN 1 X VARIABLE AND THESE ARE MULTIPLIED
!              TO GET THE INTERACTION X TERM.  THE MAIN APPLICATION
!              IS IN DESIGN OF EXPERIMENTS.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/10
!     ORIGINAL VERSION--OCTOBER   1999.
!     UPDATED         --JULY      2002. BIWEIGHT LOCATION
!     UPDATED         --JULY      2002. BIWEIGHT SCALE
!     UPDATED         --JULY      2002. WINSORIZED VARIANCE
!     UPDATED         --JULY      2002. WINSORIZED SD
!     UPDATED         --JULY      2002. ADD WINSORIZED COVARIANCE PLOT
!     UPDATED         --JULY      2002. ADD WINSORIZED CORRELATION PLOT
!     UPDATED         --JULY      2002. ADD BIWEIGHT MIDVARIANCE PLOT
!     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCOVARIANCE PLOT
!     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCORRELATION PLOT
!     UPDATED         --JULY      2002. ADD PERCENTAGE BEND MIDVARIANCE
!                                           PLOT
!     UPDATED         --JULY      2002. ADD PERCENTAGE BEND CORRELATION
!                                           PLOT
!     UPDATED         --JULY      2002. ADD HODGES LEHMAN PLOT
!     UPDATED         --JULY      2002. ADD QUANTILE PLOT
!     UPDATED         --JULY      2002. ADD QUANTILE STANDARD ERROR PLOT
!     UPDATED         --JULY      2002. ADD TRIMMED MEAN STANDARD
!                                       ERROR PLOT
!     UPDATED         --APRIL     2003. ADD SN AND QN, REQUIRED
!                                       ADDITIONAL SCRATCH ARRAYS
!     UPDATED         --AUGUST    2007. MOVE SOME ARRAY STORAGE TO COMMON
!     UPDATED         --NOVEMBER  2009. UPDATE PARSING:
!                                       1) USE "EXTSTA"
!                                       2) USE DPPARS
!     UPDATED         --NOVEMBER  2009. UPDATE CALL LIST TO DPSP2
!                                       (DPSP2 WAS MODIFIED TO ADD
!                                       SOME ENHANCEMENTS FOR THE
!                                       <STAT> PLOT COMMAND)
!     UPDATED         --JUNE      2010. UPDATE CALL LIST TO DPSP2
!     UPDATED         --APRIL     2015. UPDATE CALL LIST TO DPSP2
!     UPDATED         --FEBRUARY  2018. UPDATE CALL LIST TO DPSP2
!     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IERRO2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IGROUP
!
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 ICASE
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
      CHARACTER*40 INAME
      CHARACTER*60 ISTANM
      CHARACTER*4  ISTADF
      CHARACTER*4  ISTARA
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
!
      PARAMETER (MAXRES=25)
!
      DIMENSION Z(MAXOBV,MAXRES)
!
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION TEMP7(MAXOBV)
      DIMENSION TEMP8(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
!
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
      INTEGER ITEMP4(MAXOBV)
      INTEGER ITEMP5(MAXOBV)
      INTEGER ITEMP6(MAXOBV)
!
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
!
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP5(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP6(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP7(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP8(1))
      EQUIVALENCE (GARBAG(IGARB9),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGAR10),XTEMP2(1))
      EQUIVALENCE (GARBAG(JGAR11),Z(1,1))
!
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
!
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IGROUP='OFF'
      ISUBN1='DPIS'
      ISUBN2='P   '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      IMININ=0
      IMAXIN=0
!
!               *************************************************
!               **  TREAT THE INTERACTION STATISTIC PLOT CASE  **
!               *************************************************
!
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
!
!               **************************************
!               **  STEP 1--                       **
!               **  EXTRACT THE DESIRED STATISTIC  **
!               *************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PISP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 9000
!
      JMIN=0
      JMAX=NUMARG
      IFLAGZ=0
      IFLAGU=0
!
      DO 200 I=1,NUMARG-1
        IF(I.LT.NUMARG.AND.IHARG(I).EQ.'INTE'.AND.   &
               IHARG(I+1).EQ.'PLOT')THEN
          IF(JMAX.EQ.NUMARG)JMAX=I-1
          ILASTC=I+1
          GO TO 209
        ENDIF
  200 CONTINUE
      GO TO 9000
  209 CONTINUE
!
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,   &
                  ICASPL,ISTANM,ISTANR,ISTADF,ISTARA,   &
                  IFOUND,ILOCV,   &
                  ISUBRO,IBUGG3,IERROR)
!
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
!
      IF(IFOUND.EQ.'NO')GO TO 9000
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
      IF(IERROR.EQ.'YES')GO TO 9000
!
!     NEED FOLLOWING VARIABLES:
!     1) ONE RESPONSE VARIABLE FOR STATISTICS REQUIRING ONE VARIABLE
!     2) TWO RESPONSE VARIABLES FOR STATISTICS REQUIRING TWO VARIABLES
!     3) ONE OR MORE FACTOR VARIABLES (TYPICALLY THERE ARE TWO)
!
      ISIZE=-99
      MINVAR=1+ISTANR
      IF(NUMVAR.LT.MINVAR)THEN
!
        IF(NUMVAR.EQ.MINVAR-1)THEN
          IH='NI  '
          IH2='    '
          IHWUSE='P'
          MESSAG='NO'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
          IF(IERROR.EQ.'NO')THEN
            ISIZE=INT(VALUE(ILOCP)+0.5)
            GO TO 219
          ENDIF
        ENDIF
!
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
          GO TO 9000
        ENDIF
      ENDIF
!
  219 CONTINUE
!
!               *********************************
!               **  STEP 3--                   **
!               **  EXTRACT THE DATA           **
!               *********************************
!
      J=0
      IMAX=NRIGHT(1)
      IF(NQ.LT.NRIGHT(1))IMAX=NQ
      NFACT=NUMVAR-ISTANR
      EPS=1.0E-7
!
      NUMVA2=1
      DO 3010 K=1,ISTANR
        ICOL=K
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Z(1,K),TEMP1,TEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
 3010 CONTINUE
!
!     THE "INTERACTION" VARIABLE IS THE PRODUCT OF ALL THE FACTOR
!     VARIABLES.  NOTE THAT FOR THE INTERACTION PLOT, THIS PRODUCT
!     SHOULD BE "0", "+1", OR "-1".  REPORT AN ERROR IF IT IS NOT.
!
!     INTIALIZE COLUMN TO 1 AND THEN MULTIPLY BY EACH COLUMN.
!
      DO 3015 II=1,MAXOBV
        Z(II,ISTANR+1)=1.0
 3015 CONTINUE
!
      DO 3020 K=ISTANR+1,NUMVAR
        ICOL=K
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    TEMP1,TEMP2,TEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
!
        DO 3025 II=1,NLOCAL
          Z(II,ISTANR+1)=Z(II,ISTANR+1)*TEMP1(II)
          IFLAG=1
          IF(ABS(Z(II,ISTANR+1)).LE.EPS)IFLAG=0
          IF(ABS(Z(II,ISTANR+1)-1.0).LE.EPS)IFLAG=0
          IF(ABS(Z(II,ISTANR+1)+1.0).LE.EPS)IFLAG=0
!
          IF(IFLAG.EQ.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3301)
 3301       FORMAT('      A PRODUCT OF THE INDEPENDENT VARIABLES IS ',   &
                   'NOT EQUAL TO -1, 0, +1')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
 3025   CONTINUE
!
 3020 CONTINUE
!
      IF(ISIZE.GT.0)THEN
        NUMVAR=NUMVAR+1
        DO 3600 J=1,NLOCAL
          ITEMP=MOD(J,ISIZE)
          IF(ITEMP.EQ.0)ITEMP=ISIZE
          Z(J,NUMVAR)=REAL(ITEMP)
 3600   CONTINUE
      ENDIF
!
      NUMVA2=ISTANR+1
!
!               *****************************************************
!               **  STEP 28--                                      **
!               **  COMPUTE THE APPROPRIATE INTERACTION STATISTIC  **
!               **  PLOT STATISTIC--                               **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
!               *****************************************************
!
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
          DO 2815 I=1,NLOCAL
            WRITE(ICOUT,2817)I,Z(I,1),Z(I,2),Z(I,3)
 2817       FORMAT('I,Z(I,1),Z(I,2),Z(I,3) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 2815     CONTINUE
        ENDIF
      ENDIF
!
      CALL DPSP2(Z,MAXOBV,MAXRES,NLOCAL,NUMVA2,ISTANR,IFLAGZ,IFLAGU,   &
                 ICASPL,ISTARA,ISIZE,ICONT,   &
                 TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,TEMP8,   &
                 XTEMP1,XTEMP2,MAXNXT,   &
                 ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                 DTEMP1,DTEMP2,DTEMP3,   &
                 IQUAME,IQUASE,PSTAMV,ISTAFO,ISTASM,ISPLRL,IGROUP,   &
                 Y,X,D,NPLOTP,NPLOTV,NUMSET,GRAND,   &
                 ISUBRO,IBUGG3,IERROR)
!
!               *************************************************
!               **  STEP 29--                                  **
!               **  SAVE DIFFERENCE BETWEEN HIGHEST VALUE AND  **
!               **  LOWEST VALUE OF STATISTIC IN INTERNAL      **
!               **  PARAMETER ALOWHIGH                         **
!               *************************************************
!
      AMINS=CPUMAX
      AMAXS=CPUMIN
      DO 2910 I=1,NPLOTP
        IF(D(I).NE.1.0)GO TO 2910
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
!
      ISUBN0='PISP'
!
      IH='ALOW'
      IH2='HIGH'
      VALUE0=ADIFF
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGG3,IERROR)
!
      IF(GRAND.NE.CPUMIN)THEN
        IH='GRAN'
        IH2='DSTA'
        VALUE0=GRAND
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGG3,IERROR)
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(IFOUND.EQ.'YES' .AND. NPLOTP.GT.0)THEN
          DO 9025 I=1,NPLOTP
            WRITE(ICOUT,9026)I,Y(I),X(I),D(I)
 9026       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPISP
      SUBROUTINE DPJAB2(Y,N,   &
                        TEMP1,TEMP2,MAXNXT,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        STATVA,PVAL,CDF,   &
                        ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT THE JARQUE BERA TEST
!              FOR NORMALITY.  THIS TEST IS BASED ON THE SKEWNESS
!              AND KURTOSIS PARAMETERS.
!     EXAMPLE--JARQUE BERA NORMALITY TEST Y
!     REFERENCE--BRANI VIDAKOVIC (2011), "STATISTICS FOR
!                BIOENGINEERING SCIENCES: WITH MATLAB AND WINBUGS
!                SUPPORT", SPRINGER, PP. 521-522.
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
!     ORIGINAL VERSION--JUNE      2012.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
      CHARACTER*4 IRANAL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION PID(*)
!
      PARAMETER (NUMALP=7)
!CCCC REAL ALPHA(NUMALP)
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA ALPHA /50.0, 80.0, 90.0, 95.0, 97.5, 99.0, 99.9/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPJA'
      ISUBN2='B2  '
      IWRITE='OFF'
      IERROR='NO'
!
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
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'JAB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: JARQUE-BERA TEST--')
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
      HOLD=Y(1)
      DO 135 I=2,N
        IF(Y(I).NE.HOLD)GO TO 139
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
!               ******************************
!               **  STEP 11--               **
!               **  CARRY OUT CALCULATIONS  **
!               **  FOR JARQUE BERA         **
!               **  TEST                    **
!               ******************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'JAB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      CALL MINIM(Y,N,IWRITE,YMIN,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,YMAX,IBUGA3,IERROR)
!
      CALL DPJAB3(Y,N,ISEED,IRANAL,MAXNXT,   &
                  TEMP1,TEMP2,   &
                  YSKEW,YKURT,   &
                  STATVA,PVAL,CDF,   &
                  CUT25,CUT50,CUT75,CUT80,CUT90,   &
                  CUT95,CUT975,CUT99,CUT999,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               *********************************
!               **   STEP 42--                 **
!               **   WRITE OUT EVERYTHING      **
!               **   FOR JARQUE BERA TEST      **
!               *********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'JAB2')   &
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
      ITITLE='Jarque-Bera Test for Normality'
      NCTITL=30
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
      ITEXT(ICNT)='H0: The Data Are Normally Distributed'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Data Are Not Normally Distributed'
      NCTEXT(ICNT)=41
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
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
!
      NUMROW=ICNT
      DO 5010 I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE=' '
      NCTITL=0
!
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
!
      NMAX=0
      DO 4221 I=1,NUMCOL
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
      DO 4223 I=1,NUMROW
        DO 4225 J=1,NUMCOL
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
!
      IWHTML(1)=150
      IWHTML(2)=50
      IWHTML(3)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+500
      IWRTF(3)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.FALSE.
!
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'GRU2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CDF1=CUT90
      CDF2=CUT95
      CDF3=CUT975
      CDF4=CUT99
!
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
!
      NMAX=0
      DO 4321 I=1,NUMCOL
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
      DO 4323 I=1,NUMROW
        DO 4325 J=1,NUMCOL
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
!
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
!
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9016 I=1,N
          WRITE(ICOUT,9017)I,Y(I)
 9017     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPJAB2
      SUBROUTINE DPJAB3(Y,N,ISEED,IRANAL,MAXNXT,   &
                        TEMP1,TEMP2,   &
                        YSKEW,YKURT,   &
                        STATVA,PVAL,CDF,   &
                        CUT25,CUT50,CUT75,CUT80,CUT90,   &
                        CUT95,CUT975,CUT99,CUT999,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT THE JARQUE-BERA TEST
!              FOR NORMALITY.  EXTRACT FROM DPJAB3 IN ORDER TO
!              ALSO CALL BASIC COMPUTATION FROM CMPSTA.
!     REFERENCE--BRANI VIDAKOVIC (2011), "STATISTICS FOR
!                BIOENGINEERING SCIENCES: WITH MATLAB AND WINBUGS
!                SUPPORT", SPRINGER, PP. 521-522.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/12
!     ORIGINAL VERSION--DECEMBER  2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IRANAL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IBUGAZ
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
      CHARACTER*4 IRANSV
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPJA'
      ISUBN2='B3  '
      IWRITE='OFF'
      IERROR='NO'
!
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
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: JARQUE-BARE TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 5.',   &
               '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 135 I=2,N
        IF(Y(I).NE.HOLD)GO TO 139
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
!               ******************************
!               **  STEP 11--               **
!               **  CARRY OUT CALCULATIONS  **
!               **  FOR JARQUE-BERA         **
!               **  TEST                    **
!               ******************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JAB3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IBUGAZ='ON'
      IF(IBUGA3.EQ.'NO')IBUGAZ='OFF'
      IF(IBUGA3.EQ.'OFF')IBUGAZ='OFF'
      CALL STMOM3(Y,N,IWRITE,YSKEW,IBUGAZ,IERROR)
      CALL STMOM4(Y,N,IWRITE,YKURT,IBUGAZ,IERROR)
      AN=REAL(N)
      STATVA=(AN/6.0)*(YSKEW**2 + (YKURT - 3.0)**2/4.0)
!
!     FOR LARGE N, OBTAIN P-VALUE FROM CHI-SQUARE.  OTHERWISE,
!     PERFORM A SIMULATION.
!
      IF(N.LT.2000)THEN
        CALL MEAN(Y,N,IWRITE,YMEAN,IBUGAZ,IERROR)
        CALL SD(Y,N,IWRITE,YSD,IBUGAZ,IERROR)
!
!       NOW PERFORM 10,000 SIMULATIONS
!
        ISEESV=ISEED
        ISEED=2503
        IRANSV=IRANAL
        IRANAL='FINC'
        NSIM=100000
        DO 1000 I=1,NSIM
          CALL NORRAN(N,ISEED,TEMP1)
          DO 1010 J=1,N
            TEMP1(I)=YMEAN + YSD*TEMP1(I)
 1010     CONTINUE
          CALL STMOM3(TEMP1,N,IWRITE,YSKEW2,IBUGAZ,IERROR)
          CALL STMOM4(TEMP1,N,IWRITE,YKURT2,IBUGAZ,IERROR)
          AN=REAL(N)
          STATV2=(AN/6.0)*(YSKEW2**2 + (YKURT2 - 3.0)**2/4.0)
          TEMP2(I)=STATV2
 1000   CONTINUE
        IDIR='UPPE'
!CCCC   IDIR='LOWE'
        CALL DPGOF8(TEMP2,NSIM,STATVA,PVAL,IDIR,   &
                    IBUGAZ,ISUBRO,IERROR)
        CDF=1.0 - PVAL
        ISEED=ISEESV
        IRANAL=IRANSV
        PTEMP=25.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT25,   &
                    IBUGA3,IERROR)
        PTEMP=50.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT50,   &
                    IBUGA3,IERROR)
        PTEMP=75.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT75,   &
                    IBUGA3,IERROR)
        PTEMP=80.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT80,   &
                    IBUGA3,IERROR)
        PTEMP=90.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT90,   &
                    IBUGA3,IERROR)
        PTEMP=95.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT95,   &
                    IBUGA3,IERROR)
        PTEMP=97.5
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT975,   &
                    IBUGA3,IERROR)
        PTEMP=99.0
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT99,   &
                    IBUGA3,IERROR)
        PTEMP=99.9
        CALL PERCEN(PTEMP,TEMP2,NSIM,IWRITE,TEMP1,MAXNXT,CUT999,   &
                    IBUGA3,IERROR)
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPJAB3
      SUBROUTINE DPJBSP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBOOSS,ISEED,IBCABT,MAXNXT,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        CLLIMI,CLWIDT,   &
                        ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ,   &
                        IFOUND,IERROR)
!
!     PURPOSE--GENERATE A JACKNIFE OR BOOTSTRAP PLOT OF:
!
!              1) ANY DATAPLOT STATISTIC THAT REQUIRES
!                 EITHER ONE OR TWO RESPONSE VARIABLES
!                 (I.E., EXTSTA/CMPSTA)
!
!              2) GOODNESS OF FIT FOR DISTRIBUTIONS (FOR
!                 DISTRIBUTIONS WITH 0, 1, OR 2 SHAPE PARAMETERS).
!                 THE CURRENTLY SUPPORTED GOODNESS OF FIT
!                 STATISTCS ARE PPCC, KOLMOGOROV-SMIRNOV, AND
!                 ANDERSON-DARLING.
!
!              3) MAXIMUM LIKELIHOOD FOR DISTRIBUTIONS.
!
!              4) STATISTICS THAT ARE CURRENTLY SPECIFIC TO
!                 THE JACKNIFE/BOOTSTRAP.  THIS CURRENTY
!                 INCLUDES:
!
!                 a) LINEAR CALIBRATION
!                 b) QUADRATIC CALIBRATION
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
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO G2RBAGE COMMON
!     UPDATED         --FEBRUARY  1994. SYNONYMS FOR TAGUCHI
!     UPDATED         --MARCH     1995. MAD AND AAD PLOTS
!     UPDATED         --MARCH     1998. SAVE CERTAIN PERCENTILE PARAMETERS
!                                       AUTOMATICALLY
!     UPDATED         --MARCH     1998. ACTIVATE RELATIVE VARIANCE AND
!                                       COEFFICENT OF VARIATION
!     UPDATED         --NOVEMBER  1998. ADD PERCENTILE PLOTS
!     UPDATED         --MARCH     1999. ADD GEOMETRIC MEAN
!     UPDATED         --MARCH     1999. ADD GEOMETRIC STAND DEVIATION
!     UPDATED         --MARCH     1999. ADD HARMONIC MEAN
!     UPDATED         --SEPTEMBER 2001. ADD IQ RANGE
!     UPDATED         --NOVEMBER  2001. ADD BIWEIGHT LOCATION
!     UPDATED         --NOVEMBER  2001. ADD BIWEIGHT SCALE
!     UPDATED         --JULY      2002. ADD WINSORIZED VARIANCE
!     UPDATED         --JULY      2002. ADD WINSORIZED SD
!     UPDATED         --JULY      2002. ADD WINSORIZED COVARIANCE
!     UPDATED         --JULY      2002. ADD WINSORIZED CORRELATION
!     UPDATED         --JULY      2002. ADD BIWEIGHT MIDVARIANCE
!     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCOVARIANCE
!     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCORRELATION
!     UPDATED         --JULY      2002. ADD PERCENTAGE BEND MIDVARIANCE
!     UPDATED         --JULY      2002. ADD PERCENTAGE BEND CORRELATION
!     UPDATED         --JULY      2002. ADD HODGES LEHMAN
!     UPDATED         --JULY      2002. ADD QUANTILE
!     UPDATED         --JULY      2002. ADD QUANTILE STANDARD ERROR
!     UPDATED         --JULY      2002. ADD TRIMMED MEAN STANDARD ERROR
!     UPDATED         --JULY      2002. ADD LINEAR CALIBRATION
!     UPDATED         --JULY      2002. ADD QUADRATIC CALIBRATION
!     UPDATED         --MARCH     2003. ADD 34 "DIFFERENCE OF" STATS
!     UPDATED         --MARCH     2003. FOR "DIFFERENCE OF" STATS,
!                                       DISTINGUISH BETWEEN INDEPENDENT
!                                       AND DEPENDENT GROUPS
!     UPDATED         --APRIL     2003. ADD SN AND QN (AND DIFFERENCE
!                                       OF).  REQUIRED ADDITIONAL
!                                       SCRATCH ARRAYS.
!     UPDATED         --JULY      2003. SUPPORT FOR TWO GROUP VARIABLES
!     UPDATED         --SEPTEMBER 2003. SUPPORT FOR BCA CONFIDENCE INTERVAL
!     UPDATED         --JANUARY   2005. MAKE COMMAND SEARCH TABLE
!                                       DRIVEN
!     UPDATED         --JANUARY   2005. SUPPORT FOR BOOTSTRAPPING OF
!                                       DISTRIBUTIONAL MODELS
!     UPDATED         --MARCH     2005. ADD GENERALIZED PARETO MLE
!                                       AND MOMENTS
!     UPDATED         --MAY       2005. ADD FRECHET MLE
!     UPDATED         --AUGUST    2005. ADD INVERTED WEIBULL MLE
!     UPDATED         --SEPTEMBER 2005. ADD RATIO
!     UPDATED         --MARCH     2006. UNIFORM MLE PLOT AS SYNONYM
!                                       FOR UNIFORM MAXI LIKE
!     UPDATED         --MARCH     2006. ADD GENERALIZIED LOGISTIC
!                                       TYPE 2 - TYPE 5
!     UPDATED         --MARCH     2006. ADD BETA NORMAL
!     UPDATED         --OCTOBER   2006. MAXWELL KS PLOT
!     UPDATED         --FEBRUARY  2007. ADD SOME ADDITIONAL
!                                       DISTRIBUTUIONS
!     UPDATED         --MARCH     2007. ADD RELATIVE RISK
!     UPDATED         --MARCH     2007. ADD CRAMER CONTINCENCY COEFF
!     UPDATED         --MARCH     2007. ADD PEARSON CONTINCENCY COEFF
!     UPDATED         --MARCH     2007. FALSE POSITIVE
!     UPDATED         --MARCH     2007. FALSE NEGATIVE
!     UPDATED         --MARCH     2007. TRUE POSITIVE
!     UPDATED         --MARCH     2007. TRUE NEGATIVE
!     UPDATED         --MARCH     2007. TEST SENSITIVITY
!     UPDATED         --MARCH     2007. TEST SPECIFICITY
!     UPDATED         --APRIL     2007. POSITIVE PREDICTIVE VALUE
!     UPDATED         --APRIL     2007. NEGATIVE PREDICTIVE VALUE
!     UPDATED         --APRIL     2007. ADD LOG ODDS RATIO
!     UPDATED         --APRIL     2007. ADD LOG ODDS RATIO SE
!     UPDATED         --MAY       2007. ADD TRIMMED STAND DEVI
!     UPDATED         --MAY       2007. ADD TRIANGULAR MAXIMUM LIKELIHOOD
!     UPDATED         --JUNE      2007. ADD SLASH MAXIMUM LIKELIHOOD
!     UPDATED         --AUGUST    2007. ADD BETA NORMAL AND LOG BETA MLE
!     UPDATED         --SEPTEMBER 2007. ADD REFLECTED GENERALIZED TOPP
!                                       LEONE MLE
!     UPDATED         --OCTOBER   2007. ADD SLOPE, OGIVE
!     UPDATED         --OCTOBER   2007. ADD TWO-SIDED SLOPE
!     UPDATED         --OCTOBER   2007. ADD TWO-SIDED OGIVE
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 2
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 3
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 5
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 6
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 7
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 8
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 9
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 10
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 11
!     UPDATED         --OCTOBER   2007. ADD BURR TYPE 12
!     UPDATED         --NOVEMBER  2007. ADD DOUBLE PARETO UNIFORM
!     UPDATED         --NOVEMBER  2007. ADD KUMARASWAMY
!     UPDATED         --NOVEMBER  2007. ADD ALPHA
!     UPDATED         --NOVEMBER  2007. ADD EXPONENTIAL POWER
!     UPDATED         --NOVEMBER  2007. ADD FOLDED CAUCHY
!     UPDATED         --NOVEMBER  2007. LP LOCATION
!     UPDATED         --NOVEMBER  2007. VARIANCE OF LP LOCATION
!     UPDATED         --NOVEMBER  2007. SD OF LP LOCATION
!     UPDATED         --NOVEMBER  2007. DIFFERENCE OF LP LOCATION
!     UPDATED         --NOVEMBER  2007. DIFFERENCE OF VARI OF LP LOCATION
!     UPDATED         --NOVEMBER  2007. DIFFERENCE OF SD OF LP LOCATION
!     UPDATED         --DECEMBER  2007. POWER MLE
!     UPDATED         --DECEMBER  2007. REFLECTED POWER PPCC/KS
!     UPDATED         --JANUARY   2008. MUTH PPCC/KS
!     UPDATED         --FEBRUARY  2008. LOGISTIC-EXPONENTIAL PPCC/KS
!     UPDATED         --FEBRUARY  2008. LOGISTIC-EXPONENTIAL MLE
!     UPDATED         --MARCH     2008. TRUNCATED PARETO MLE
!     UPDATED         --MARCH     2008. REFLECTED POWER MLE
!     UPDATED         --JULY      2008. INVERTED GAMMA MLE
!     UPDATED         --JULY      2008. VON MISES MLE
!     UPDATED         --JULY      2008. MIELKE BETA-KAPPA PPCC/KS
!     UPDATED         --JULY      2008. KAPPA PPCC/KS/MLE
!     UPDATED         --JULY      2008. PEARSON TYPE 3 PPCC/KS/MLE
!     UPDATED         --FEBRUARY  2009. BINOMIAL PROPORTION
!     UPDATED         --FEBRUARY  2009. GRUBB
!     UPDATED         --FEBRUARY  2009. ONE SAMPLE T TEST
!     UPDATED         --FEBRUARY  2009. CHI-SQUARE SD TEST
!     UPDATED         --FEBRUARY  2009. FREQUENCY TEST
!     UPDATED         --FEBRUARY  2009. FREQUENCY WITHIN A BLOCK TEST
!     UPDATED         --MARCH     2010. RE-WRITE TO:
!                                       1) USE DPPARS
!                                       2) USE EXTSTA
!                                       3) USE EXTDIS
!                                       4) USE DIFFERENT SUBROUTINES
!                                          FOR DIFFERENT CASES TO KEEP
!                                          OVERALL CODE MORE DIGESTABLE
!     UPDATED         --SEPTEMBER 2010. SUPPORT A "LEVEL" VARIABLE
!                                       FOR BRITTLE FIBER WEIBULL
!                                       (MAY ADD TO A FEW OTHERS AT
!                                       A LATER TIME).  NOTE THAT THIS
!                                       IS CURRENTLY ONLY SUPPORTED
!                                       FOR THE SINGLE RESPONSE
!                                       VARIABLE RAW DATA CASE
!     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
!     UPDATED         --JULY      2019. CALL LIST TO DPJBS7, REDUCE
!                                       NUMBER OF SCRATCH ARRAYS
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IH41
      CHARACTER*4 IH42
!
      CHARACTER*4 ISTATN(17)
      CHARACTER*4 ISTAT2(17)
!
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
      CHARACTER*40 INAME
      CHARACTER*60 ISTANM
      CHARACTER*60 IDIST
      CHARACTER*4  ISTADF
      CHARACTER*4  ISTARA
!
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
!
      REAL KSLOC
      REAL KSSCAL
!
!---------------------------------------------------------------------
!
      PARAMETER (NUMCHS=2)
      CHARACTER*4 INAM2(NUMCHS,6)
      CHARACTER*4 INCASE(NUMCHS)
      CHARACTER*4 INFLAV(NUMCHS)
      CHARACTER*4 INFLAD(NUMCHS)
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZI.INC'
      PARAMETER (MAXBGR=2)
!
      DIMENSION Y1(MAXOBV)
      DIMENSION Z1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XLEVEL(MAXOBV)
!
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
!
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
!
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
!
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
      DOUBLE PRECISION DTEMP4(MAXOBV)
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
      EQUIVALENCE (DGARBG(IDGAR4),DTEMP4(1))
!
      PARAMETER(NPERC2=15)
      DIMENSION APERC(NPERC2)
      DIMENSION BPERC(NPERC2)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA (ISTATN(I),I=1,17)/   &
      'BSD ',   &
      'BMEA',   &
      'B975',   &
      'B025',   &
      'B001',   &
      'B005',   &
      'B01 ',   &
      'B05 ',   &
      'B10 ',   &
      'B20 ',   &
      'B50 ',   &
      'B80 ',   &
      'B90 ',   &
      'B95 ',   &
      'B99 ',   &
      'B995',   &
      'B999'/
      DATA (ISTAT2(I),I=1,17)/   &
      '    ',   &
      'N   ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    ',   &
      '    '/
!
      DATA APERC/ 0.1,  0.5,  1.0,  2.5,  5.0, 10.0, 20.0, 50.0,   &
                 80.0, 90.0, 95.0, 97.5, 99.0, 99.5, 99.9/
!
      DATA INCASE(1)/'LICA'/
      DATA (INAM2(1,J),J=1,6)/   &
      'LINE','CALI','    ','    ','    ','    '/
      DATA INFLAV(1)/'TWO '/
      DATA INFLAD(1)/'OFF '/
!
      DATA INCASE(2)/'QUCA'/
      DATA (INAM2(2,J),J=1,6)/   &
      'QUAD','CALI','    ','    ','    ','    '/
      DATA INFLAV(2)/'TWO '/
      DATA INFLAD(2)/'OFF '/
!
!-----START POINT-----------------------------------------------------
!
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
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      MAXV2=5
      MINN2=2
      ICOLL=0
      ICOLH=0
      ICOLX=0
      IVAL=0
!
!               **********************************************
!               **  TREAT THE BOOTSTRAP/JACKNIFE PLOT CASE  **
!               **********************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPJBSP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ
   52   FORMAT('ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',   &
               A4,2X,A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBOOSS
   54   FORMAT('IBOOSS = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE IF OF THIS TYPE  **
!               **  AND BRANCH ACCORDINGLY.    **
!               *********************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'JACK')THEN
        ICASJB='JACK'
      ELSEIF(ICOM.EQ.'BOOT')THEN
        ICASJB='BOOT'
      ELSE
        IFOUND='NO'
        GO TO 9000
      ENDIF
!
      IF(NUMARG.LE.1)GO TO 9000
!
!
!               ***********************************************
!               **  STEP 1B--                                **
!               **  EXTRACT THE COMMAND                      **
!               **  1) CHECK FOR STATISTICS/CASES UNIQUE TO  **
!               **     BOOTSTRAP/JACKNIFE COMMAND            **
!               **  2) CHECK FOR SUPPORTED STATISTICS IN     **
!               **     EXTSTA                                **
!               **  3) CHECK FOR SUPPORTED DISTRIBUTIONS IN  **
!               **     EXTDIS                                **
!               ***********************************************
!
      ISTEPN='1B'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     CASE 1: STATISTICS UNIQUE TO BOOTSTRAP/JACKNIFE COMMAND
!             (I.E., NOT IN EXTSTA OR EXTDIS)
!
      DO 100 I=1,NUMCHS
        IROW=I
        IF(INAM2(I,1).NE.ICOM)GO TO 100
        DO 102 J=2,6
          IF(INAM2(I,J).NE.'    ')GO TO 102
          ITEMP=J-1
          GO TO 104
  102   CONTINUE
        ITEMP=6
  104   CONTINUE
        ILASTC=0
        IF(ITEMP.GT.1)THEN
          DO 108 J=2,ITEMP
            IF(INAM2(I,J).NE.IHARG(J-1))GO TO 100
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
          GO TO 1000
        ELSEIF(IHARG(I1).EQ.'STAT'.AND.IHARG(I2).EQ.'PLOT')THEN
          ILASTC=I2
          ICASPL=INCASE(IROW)
          IFLAGV=INFLAV(IROW)
          IFLAGD=INFLAD(IROW)
          CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
          ICASEB='STAT'
          IFOUND='YES'
          GO TO 1000
        END IF
!
  100 CONTINUE
!
!     CASE 2: SUPPORTED STATISTICS
!
!             EXTRACT THE DESIRED STATISTIC
!
!             SEARCH FOR WORD "PLOT".
!
      JMIN=1
      JMAX=NUMARG
!
      DO 200 I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT')THEN
          JMAX=I-1
          ILASTC=I
          IFOUND='YES'
          GO TO 209
        ENDIF
  200 CONTINUE
      IFOUND='NO'
      GO TO 9000
  209 CONTINUE
!
      IFOUND='NO'
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,   &
                  ICASPL,ISTANM,ISTANR,ISTADF,ISTARA,   &
                  IFOUND,ILOCV,ISUBRO,IBUGG3,IERROR)
!
      IF(IFOUND.EQ.'YES')THEN
        ICASEB='STAT'
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,251)
  251   FORMAT('***** AFTER CALL EXTSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,252)ICASPL,ISTANR,ILOCV,IFOUND,ICASEB
  252   FORMAT('ICASPL,ISTANR,ILOCV,IFOUND,ICASEB = ',   &
               A4,2I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IFOUND.EQ.'YES')GO TO 1000
!
!     CASE 3: DISTRIBUTIONAL BOOTSTRAP CASES
!
!             SEARCH FOR:
!
!             1) PPCC PLOT (DEFAULT)
!             2) KS PLOT
!             3) ANDERSON-DARLING PLOT
!             4) CENSORED (CURRENTLY SUPPORTED FOR PPCC PLOT ONLY)
!             5) MAXIMUM LIKELIHOOD
!
      ICASP2='PPCC'
      JMAX2=JMAX
!
      DO 300 I=1,JMAX2
        IF(I.LT.NUMARG .AND. IHARG(I).EQ.'STAT' .AND.   &
               IHARG(I+1).EQ.'PLOT')THEN
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
        ELSEIF(I.LT.NUMARG .AND. IHARG(I).EQ.'KOLM' .AND.   &
               IHARG(I+1).EQ.'SMIR')THEN
          ICASP2='KS'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'AD')THEN
          ICASP2='AD'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(I.LT.NUMARG .AND. IHARG(I).EQ.'ANDE' .AND.   &
               IHARG(I+1).EQ.'DARL')THEN
          ICASP2='AD'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'PPCC')THEN
          ICASP2='PPCC'
          JMAX=MIN(JMAX,I-1)
          ILASTC=MAX(ILASTC,I)
        ELSEIF(I.LT.NUMARG .AND. IHARG(I).EQ.'MAXI' .AND.   &
               IHARG(I+1).EQ.'LIKE')THEN
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
      IFLAGV='ONE'
      IF(ICENSO.EQ.'ON')IFLAGV='TWO'
!
      CALL EXTDIS(ICOM,ICOM2,IHARG,IHARG2,NUMARG,JMIN,JMAX,   &
                  ICASPL,IDIST,NUMSHA,IFOUND,ILOCV,   &
                  ISUBRO,IBUGG3,IERROR)
!
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
!
      IF(IFOUND.EQ.'YES')THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ELSE
        GO TO 9000
      ENDIF
!
!               ***************************************************
!               **  STEP 3--EXTRACT THE SHAPE PARAMETERS FOR     **
!               **          THE SPECIFIED DISTRIBUTION.          **
!               ***************************************************
!
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHP='PPLO'
      IHP2='C   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        PPLOC=0.0
      ELSE
        PPLOC=VALUE(ILOCV)
      ENDIF
      IHP='PPSC'
      IHP2='ALE '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        PPSCAL=1.0
      ELSE
        PPSCAL=VALUE(ILOCV)
        IF(PPSCAL.LE.0.0)PPSCAL=1.0
      ENDIF
!
      IHP='KSLO'
      IHP2='C   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        KSLOC=CPUMIN
      ELSE
        KSLOC=VALUE(ILOCV)
      ENDIF
      IHP='KSSC'
      IHP2='ALE '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        KSSCAL=CPUMIN
      ELSE
        KSSCAL=VALUE(ILOCV)
        IF(KSSCAL.LE.0.0)KSSCAL=1.0
      ENDIF
!
      IFLAGL=0
      AL=CPUMIN
      IF(ICASPL.EQ.'WEIB' .OR. ICASPL.EQ.'3WEI')THEN
        IF(IWEIGL.EQ.'ON')THEN
          IHP='L   '
          IHP2='    '
          IHWUSE='P'
          MESSAG='NO'
          CALL CHECKN(IHP,IHP2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'NO')AL=VALUE(ILOCP)
          IF(AL.LE.0.0)THEN
            AL=CPUMIN
          ELSE
            IFLAGL=1
          ENDIF
        ENDIF
      ENDIF
!
      IF(ICASPL.EQ.'GMCL' .OR. ICASPL.EQ.'TRAP' .OR.   &
             ICASPL.EQ.'GTRA' .OR. ICASPL.EQ.'UTSP' .OR.   &
             ICASPL.EQ.'GLGP' .OR.   &
             ICASPL.EQ.'PARE' .OR. ICASPL.EQ.'PAR2'   &
        )THEN
        CONTINUE
      ELSE
        IHP='A   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
          A=0.0
        ELSE
          A=VALUE(ILOCV)
        ENDIF
!
        IHP='B   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
          B=1.0
        ELSE
          B=VALUE(ILOCV)
        ENDIF
!
      ENDIF
!
      IF(NUMSHA.GE.1)THEN
        CALL EXTPA2(ICASPL,IDIST,A,B,   &
                    SHAP11,SHAP12,SHAP21,SHAP22,   &
                    SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,   &
                    IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                    ILGADF,ISKNDF,IGLDDF,IBGEDF,   &
                    IGETDF,ICONDF,IGOMDF,IKATDF,   &
                    IGIGDF,IGEODF,   &
                    ISUBRO,IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
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
!
      ISTANR=1
!
 1000 CONTINUE
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      INAME='BOOTSTRAP PLOT'
      IF(ICASPL.EQ.'JACK')INAME='JACKNIFE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
!CCCC IFLAGE=1
      IFLAGE=0
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=-99
      MAXNVA=-99
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 1005 I=1,NUMVAR
            WRITE(ICOUT,1007)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
 1007       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
 1005     CONTINUE
        ENDIF
      ENDIF
!
!               *********************************
!               **  STEP 3--                   **
!               **  CREATE THE VARIABLES       **
!               *********************************
!
      IF(ICASEB.EQ.'STAT')THEN
        NRESP=ISTANR
        NGRPV=NUMVAR-NRESP
      ELSE
        NRESP=1
        NCEN=0
        NLEVEL=0
        IF(ICENSO.EQ.'ON')NCEN=1
        IF(ICASPL.EQ.'BFWE')THEN
          IF(IBFWTY.EQ.'ON' .AND. NUMVAR.GT.1 .AND.   &
             IFLAGM.EQ.0)THEN
             NLEVEL=1
             ILEVEL='ON'
          ENDIF
          NGRPV=NUMVAR-NRESP-NCEN-NLEVEL
        ELSE
          NGRPV=NUMVAR-NRESP-NCEN
        ENDIF
!
        IF(NGRPV.LT.0 .OR. NGRPV.GT.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2510)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,521)
  521     FORMAT('      THE NUMBER OF CLASS VARIABLES IS LESS THAN ',   &
                 'ZERO OR GREATER THAN TWO.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,523)NGROUP
  523     FORMAT('      THE NUMBER OF CLASS VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1101)NRESP,NGRPV,ICASPL,IBOOGR
 1101   FORMAT('NRESP,NGRPV,ICASPL,IBOOGR = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
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
        IF(ICASPL.EQ.'SGCD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SG2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SGLP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SGUP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'SGTE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'GMDR' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LRST' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LRSC' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LRSP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LRLP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LRUP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LRLC' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'LRUC' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FPCD' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FP2P' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FPLP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FPUP' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ICASPL.EQ.'FPTE' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(ISTARA.EQ.'ON  ' .AND. IBOOGR.EQ.'INDE')IFLAGI='INDE'
        IF(IFLAGI.EQ.'INDE')IFLAGD='INDE'
      ENDIF
!
!     NOTE 2011/10: IDENTIFY "SUMMARY" STATISTICS THAT ARE BASED
!                   ON MEAN, SD, AND SAMPLE SIZE VALUES.
!
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
!
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
        GO TO 9000
      ELSEIF(NGRPV.LT.0 .OR. NGRPV.GT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2510)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2521)
 2521   FORMAT('      THE NUMBER OF GROUP VARIABLES IS LESS THAN ',   &
               'ZERO OR GREATER THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2523)NGRPV
 2523   FORMAT('      THE NUMBER OF GROUP VARIABLES GIVEN: ',I5)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      NMAX=NRIGHT(1)
      IF(IFLAGI.EQ.'DEPE')THEN
        DO 2530 I=1,NUMVAR
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
            GO TO 9000
          ENDIF
 2530   CONTINUE
      ELSE
        NMAX=MAX(NRIGHT(1),NRIGHT(2))
      ENDIF
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        ISTEPN='26'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1111)NMAX,IFLAGI
 1111   FORMAT('NMAX,IFLAGI = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      J=0
      J2=0
      J3=0
      IMAX=NMAX
      IF(NQ.LT.NMAX)IMAX=NQ
      ICNT=1
      IF(NRESP.EQ.2 .OR. ICENSO.EQ.'ON')ICNT=2
!
      DO 2660 I=1,IMAX
!
!       FIRST RESPONSE VARIABLE
!
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
!
!       SECOND RESPONSE VARIABLE
!
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
!
!       THIRD RESPONSE VARIABLE
!
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
!
!       LENGTH VARIABLE
!
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
!
!
!       CENSORING VARIABLE
!
        IF(ICENSO.EQ.'ON' .AND. ISUB(I).EQ.1)THEN
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
!
        IF(NGRPV.GE.1 .AND. ISUB(I).EQ.1)THEN
          DO 2665 K=1,NGRPV
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
!
 2660 CONTINUE
!
      NLOCAL=J
      NLOCA2=J2
!
!               *******************************************************
!               **  STEP 28--                                        **
!               **  COMPUTE THE APPROPRIATE STATISTIC PLOT STATISTIC--*
!               **  (MEAN, STANDARD DEVIATION, RANGE, OR CUSUM).     **
!               **  COMPUTE CONFIDENCE LINES.                        **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS            **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.               **
!               **  DEFINE THE VECTOR D(.) TO 1'S, 2'S, AND 3'S      **
!               **  FOR THE PLOTTED VALUE, THE LOWER CONFIDENCE LINE,**
!               **  AND THE UPPER CONFIDENCE LINE.                   **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).    **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).    **
!               *******************************************************
!
      ISTEPN='28'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHP='ALPH'
      IHP2='A   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALPHA=0.05
      ELSE
        ALPHA=VALUE(ILOCP)
      ENDIF
      IF(ALPHA.LT.0.0 .OR. ALPHA.GT.1.0)ALPHA=0.05
      IF(ALPHA.GT.0.5)ALPHA=1.0-ALPHA
!
      IHP='LOWL'
      IHP2='IMIT'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALOWLM=CPUMIN
      ELSE
        ALOWLM=VALUE(ILOCP)
      ENDIF
!
      IHP='UPPL'
      IHP2='IMIT'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        AUPPLM=CPUMIN
      ELSE
        AUPPLM=VALUE(ILOCP)
      ENDIF
!
      IHP='ALPH'
      IHP2='ASV '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALPHSV=CPUMIN
      ELSE
        ALPHSV=VALUE(ILOCP)
      ENDIF
!
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
        CALL CHECKN(IH41,IH42,IHWUSE,   &
             IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
             ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
!
        IF(IERROR.EQ.'YES')THEN
          NPERC=0
        ELSE
          ICOLQP=IVALUE(ILOCV)
          NPERC=IN(ILOCV)
          ICNT=0
          DO 4180 I=1,NPERC
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
            DO 4183 II=1,NPERC
              QP(II)=QP(II)/100.0
 4183       CONTINUE
          ENDIF
!
        ENDIF
      ENDIF
!
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEB.EQ.'STAT')THEN
!
        CALL DPJBS6(Y1,Z1,X1,XDESGN,NLOCAL,NLOCA2,   &
                    NRESP,NGRPV,ICASPL,ISTARA,   &
                    ISIZE,ICONT,   &
                    ICASJB,IBOOSS,ISEED,IBCABT,ALPHA,IFLAGI,IFLAGD,   &
                    TEMP,TEMP2,TEMPL,TEMP3,XTEMP1,XTEMP2,XTEMP3,   &
                    TEMP7,TEMP8,   &
                    MAXNXT,MAXBGR,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    Y,X,D,NPLOTP,NPLOTV,   &
                    TEMP0,TEMPZ0,TEMPZL,RES1,RES2,TEMP4,TEMPTH,TEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    APERC,BPERC,NPERC2,   &
                    BMEAN,BSD,B001,B005,B01,B025,B05,B10,B20,B50,   &
                    B80,B90,B95,B975,B99,B995,B999,   &
                    ICAPSW,ICAPTY,IFORSW,IVARN1,IVARN2,ISTANM,   &
                    ISUBRO,IBUGG3,IERROR)
      ELSE
        CALL DPJBS7(Y1,X1,XLEVEL,XDESGN,NLOCAL,NRESP,NGRPV,   &
                    ICASPL,ICASP2,IDIST,   &
                    ICENSO,ISIZE,ICONT,NPERC,KSLOC,KSSCAL,   &
                    IMETHD,ILEVEL,   &
                    ICASJB,IBOOSS,ISEED,IBCABT,ALPHA,   &
                    TEMP,TEMP2,TEMP0,TEMPZ0,TEMPL,TEMPZL,   &
                    QP,XQP,XQPLCL,XQPUCL,   &
                    TEMP3,XTEMP1,XTEMP2,XTEMP3,TEMP4,   &
                    ZTEMP1,ZTEMP2,ZTEMP3,TEMP5,TEMPT2,TEMP7,   &
                    TEMP8,WEIGHH,RES1,RES2,TEMP6,TEMPTH,   &
                    MAXNXT,MAXBGR,   &
                    ITEMP1,DTEMP1,DTEMP2,DTEMP3,   &
                    YLOWLM,YUPPLM,A,B,MINMAX,   &
                    SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,   &
                    SHAPE6,SHAPE7,NUMSHA,   &
                    SHAP11,SHAP12,SHAP21,SHAP22,   &
                    Y,X,D,NPLOTP,NPLOTV,   &
                    APERC,BPERC,NPERC2,   &
                    BMEAN,BSD,B001,B005,B01,B025,B05,B10,B20,B50,   &
                    B80,B90,B95,B975,B99,B995,B999,   &
                    ICAPSW,ICAPTY,IFORSW,IVARN1,IVARN2,   &
                    CLLIMI,CLWIDT,IRELAT,   &
                    IFLAGL,AL,   &
                    ISUBRO,IBUGG3,IERROR)
      ENDIF
!
!  AUTOMATICALLY SAVE CERTAIN PERCENTILE PARAMETERS.  MARCH 1998
!  JANUARY 2005: ONLY SAVE IF 1 PARAMETER IS ESTIMARED (E.G.,
!                DISTRIBUTIONAL FITTING HAS 2 TO 4 PARAMETERS)
!
!
!               ***************************************
!               **  STEP 51--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      CUTOFF=REAL(I1MACH(9))
!
      ISTEPN='51'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC IF(NUMPAR.GT.1)GO TO 5199
      IF(ICASEB.NE.'STAT')GO TO 5199
      DO 5100 IPASS=1,17
        IH=ISTATN(IPASS)
        IH2=ISTAT2(IPASS)
        DO 5150 I=1,NUMNAM
          I2=I
          IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.   &
             IUSE(I).EQ.'P')GO TO 5180
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
          GO TO 9000
        ENDIF
!
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
        IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
        IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
        IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
        IVALUE(ILOC)=IVAL
        GO TO 5100
!
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
        IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
        IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
        IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
        IVALUE(I2)=IVAL
        GO TO 5100
!
 5100 CONTINUE
 5199 CONTINUE
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'JBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJBSP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ
 9012   FORMAT('ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',   &
               A4,2X,A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR,IBOOSS,ICASJB
 9013   FORMAT('IFOUND,IERROR,IBOOSS,ICASJB = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ISIZE,NUMVAR,NRESP,NGRPV
 9015   FORMAT('ISIZE,NUMVAR,NRESP,NGRPV = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.LE.0)THEN
          DO 9025 I=1,NPLOTP
            WRITE(ICOUT,9026)I,Y(I),X(I),D(I)
 9026       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPJBSP
      SUBROUTINE DPJBS3(TEMP1,N1,ICASJB,IJACIN,ISEED,TEMP2,N2,   &
                        INDX,AINDEX,   &
                        IBUGG3,IERROR)
!
!     PURPOSE--GENERATE 1 JACKNIFE  SUBSAMPLE OF SIZE N1-1
!              OR       1 BOOTSTRAP SUBSAMPLE OF SIZE N1
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!     UPDATED         --JULY      2002. ADD AN INDEX VARIABLE.  USE
!                                       FOR CASES WHERE NEED TO
!                                       KEEP TWO OR MORE RESPONSE
!                                       VARIABLES DEPENDENT (E.G.,
!                                       CORRELATION KEEPS PAIRING
!                                       INTACT).
!     UPDATED         --AUGUST    2005. DUNRAN WAS FIXED TO GO FROM
!                                       0 TO N.  THIS ROUTINE WAS
!                                       MODIFIED TO CALL A VERSION
!                                       THAT GOES FROM 1 TO N.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASJB
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
!CCCC INCLUDE 'DPCOPA.INC'
!
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION AINDEX(*)
      DIMENSION INDX(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
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
          DO 55 I=1,N1
            WRITE(ICOUT,56)I,TEMP1(I)
   56       FORMAT('I,TEMP1(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
      ENDIF
!
!               **************************************************
!               **  STEP 11--                                   **
!               **  CHECK THE INPUT NUMBER FOR ERRORS           **
!               **************************************************
!
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
        GO TO 9000
      ENDIF
!
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
          GO TO 9000
        ENDIF
      ENDIF
!
!               **************************************************
!               **  STEP 12--                                   **
!               **  GENERATE THE JACKNIFE OR BOOTSTRAP SAMPLE   **
!               **************************************************
!
      IF(ICASJB.EQ.'JACK')THEN
        J=0
        DO 1211 I=1,N1
          IF(I.EQ.IJACIN)GO TO 1211
          J=J+1
          TEMP2(J)=TEMP1(I)
          INDX(J)=I
 1211   CONTINUE
        N2=J
      ELSE
        CALL DUNRA2(N1,N1,ISEED,AINDEX)
        DO 1221 I=1,N1
          J=INT(AINDEX(I)+0.5)
          TEMP2(I)=TEMP1(J)
          INDX(I)=J
 1221   CONTINUE
        N2=N1
      ENDIF
!
!               *******************
!               **   STEP 90--   **
!               **   EXIT        **
!               *******************
!
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
          DO 9021 I=1,N1
            WRITE(ICOUT,9022)I,TEMP1(I),AINDEX(I),INDX(I)
 9022       FORMAT('I,TEMP1(I),AINDEX(I),INDX(I) = ',I8,2G15.7,I8)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
        ENDIF
        IF(N2.GT.0)THEN
          DO 9031 I=1,N2
            WRITE(ICOUT,9032)I,TEMP2(I)
 9032       FORMAT('I,TEMP2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9031     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPJBS3
      SUBROUTINE DPJBS4(ISET,NUMSET,J,J2,RIGHT,TAGID,XIDTEM,Y2,X2,D2)
!
!     PURPOSE--ADD A COMPUTED POINT TO THE OUTPUT PLOT VECTORS
!              FOR THE JACKNIFE AND BOOTSTRAP PLOTS.
!     CAUTION--THE INPUT ARGUMENT J CHANGES WITHIN
!              THIS ROUTINE AND IS ALSO AN OUTPUT ARGUMENT.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!     UPDATED         --MARCH     2003. FOR REPLICATION CASE, SET
!                                       TAGPLOT (D2) TO REFLECT
!                                       REPLICATION NUMBER
!     UPDATED         --JANUARY   2005. SET D2 FOR CASE WHERE MORE
!                                       THAN ONE STATISTIC ESTIMATED
!                                       (E.G., DISTRIBUTIONAL FITS),
!                                       TAGID IDENTIFIES WHICH
!                                       STATISTIC
!
!---------------------------------------------------------------------
!
      DIMENSION XIDTEM(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(NUMSET.LE.0)GO TO 1100
      GO TO 1200
!
!               **************************************************
!               **  STEP 11--                                   **
!               **  TREAT THE CASE WHEN HAVE NO (= FULL DATA) SUBSET  **
!               **************************************************
!
 1100 CONTINUE
!CCCC IF(ISET.LE.NUMSET)GO TO 1110
!CCCC GO TO 1120
!1110 CONTINUE
      J=J+1
      Y2(J)=RIGHT
      IF(TAGID.EQ.1.0)THEN
        J2=J2+1
      ENDIF
      X2(J)=J2
!CCCC D2(J)=1.0
      D2(J)=TAGID
      GO TO 1190
!1120 CONTINUE
!CCCC GO TO 9000
!CCCC J=J+1
!CCCC Y2(J)=RIGHT
!CCCC X2(J)=XIDTEM(1)
!CCCC D2(J)=2.0
!CCCC J=J+1
!CCCC Y2(J)=RIGHT
!CCCC X2(J)=XIDTEM(NUMSET)
!CCCC D2(J)=2.0
!CCCC GO TO 1190
 1190 CONTINUE
      GO TO 9000
!
!               **************************************************
!               **  STEP 12--                                   **
!               **  TREAT THE CASE WHEN HAVE 2 OR MORE SUBSETS  **
!               **************************************************
!
 1200 CONTINUE
      IF(ISET.LE.NUMSET)GO TO 1210
      GO TO 1220
 1210 CONTINUE
      J=J+1
      Y2(J)=RIGHT
      X2(J)=XIDTEM(ISET) + (TAGID-1.0)/10.0
!CCCC D2(J)=1.0
      D2(J)=(TAGID-1.0)*REAL(NUMSET) + REAL(ISET)
      GO TO 1290
 1220 CONTINUE
      GO TO 9000
!CCCC J=J+1
!CCCC Y2(J)=RIGHT
!CCCC X2(J)=XIDTEM(1)
!CCCC D2(J)=2.0
!CCCC J=J+1
!CCCC Y2(J)=RIGHT
!CCCC X2(J)=XIDTEM(NUMSET)
!CCCC D2(J)=2.0
!CCCC GO TO 1290
 1290 CONTINUE
      GO TO 9000
!
!               *******************
!               **   STEP 90--   **
!               **   EXIT        **
!               *******************
!
 9000 CONTINUE
!
      RETURN
      END SUBROUTINE DPJBS4
      SUBROUTINE DPJBS5(ISET1,ISET2,NUMSE2,J,RIGHT,XIDTEM,   &
                        Y2,X2,D2)
!
!     PURPOSE--ADD A COMPUTED POINT TO THE OUTPUT PLOT VECTORS
!              FOR THE JACKNIFE AND BOOTSTRAP PLOTS.
!              THIS IS A SPECIAL VERSION OF DPJBS4 FOR THE CASE
!              WHEN THERE ARE EXACTLY TWO GROUP VRIABLES.
!     CAUTION--THE INPUT ARGUMENT J CHANGES WITHIN
!              THIS ROUTINE AND IS ALSO AN OUTPUT ARGUMENT.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/7
!     ORIGINAL VERSION--JULY      2003.
!
!---------------------------------------------------------------------
!
      DIMENSION XIDTEM(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(ISET1.LE.0 .OR. ISET2.LE.0)GO TO 9000
!
!               **************************************************
!               **  STEP 12--                                   **
!               **  TREAT THE CASE WHEN HAVE 2 GROUPS           **
!               **************************************************
!
      AINC=0.4/REAL(NUMSE2)
      ASTRT=XIDTEM(ISET1) - 0.2
      XTEMP=ASTRT + REAL(ISET2-1)*AINC
      J=J+1
      Y2(J)=RIGHT
      X2(J)=XTEMP
      ITEMP=(ISET1-1)*NUMSE2 + ISET2
      D2(J)=REAL(ITEMP)
!
!               *******************
!               **   STEP 90--   **
!               **   EXIT        **
!               *******************
!
 9000 CONTINUE
!
      RETURN
      END SUBROUTINE DPJBS5
      SUBROUTINE DPJBS6(Y,Z,Z2,XDESGN,N,N2,NUMV2,NGRPV,ICASPL,ISTARA,   &
                        ISIZE,ICONT,   &
                        ICASJB,IBOOSS,ISEED,IBCABT,ALPHA,IFLAGI,IFLAGD,   &
                        TEMP,TEMPZ,TEMPZ2,XIDTEM,XTEMP1,XTEMP2,XTEMP3,   &
                        XTEMP4,XTEMP5,   &
                        MAXNXT,MAXBGR,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        Y2,X2,D2,NPLOTP,NPLOTV,   &
                        TEMP0,TEMPZ0,TMPZ20,RES1,RES2,   &
                        TEMP4,TEMPTH,TEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        APERC,BPERC,NPERC,   &
                        BMEAN,BSD,B001,B005,B01,B025,B05,B10,B20,B50,   &
                        B80,B90,B95,B975,B99,B995,B999,   &
                        ICAPSW,ICAPTY,IFORSW,IVARID,IVARI2,ISTANM,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A JACKNIFE OR BOOTSTRAP PLOT
!              (SEE DPJBSP FOR ALLOWABLE TYPES)
!
!              NOTE: THIS ROUTINE EXTRACTED FROM ORIGINAL DPJBS2.
!                    IT PERFORMS THE BOOTSTRAP FOR "STATISTICS" AND
!                    A FEW SPECIAL FITTING/CALIBRATION CASES.  THE
!                    DISTRIBUTIONAL BOOTSTRAP IS EXTRACTED TO DPJBS7.
!
!                    WITH THIS EXTRACTION, TAKE THE OPPORTUNITY TO
!                    SIMPLIFY THE CODE A BIT AS WELL.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/02
!     ORIGINAL VERSION--FEBRUARY  2010. EXTRACTED FROM DPJBS2
!     UPDATED         --JULY      2010. IN ADDITION TO PLOT,
!                                       GENERATE A NUMERIC TABLE
!     UPDATED         --SEPTEMBER 2010. ACCOMODATE UP TO 3 RESPONSE
!                                       VARIABLES
!     UPDATED         --OCTOBER   2011. SUPPORT FOR PERCENTILE-t
!                                       CONFIDENCE INTERVALS
!     UPDATED         --OCTOBER   2011. SUPPORT FOR SMOOTHED BOOTSTRAP
!     UPDATED         --OCTOBER   2011. SUPPORT FOR "SUMMARY" STATISTICS
!     UPDATED         --JUNE      2017. FIX BUG IN "UNPAIRED" SAMPLES
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBCABT
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASPL
      CHARACTER*4 ISTARA
      CHARACTER*4 ICONT
      CHARACTER*4 IFLAGD
      CHARACTER*4 IFLAGI
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
      CHARACTER*(*) ISTANM
!
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IWRITE
      CHARACTER*4 IBCASV
      CHARACTER*4 IBOOC2
      CHARACTER*4 ICASZZ
      CHARACTER*4 IOP
      CHARACTER*4 ICASJB
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IVRBSV
      CHARACTER*4 IDS4SV
!
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XDESGN(MAXNXT,MAXBGR)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XIDTEM(MAXNXT,MAXBGR)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION XTEMP4(*)
      DIMENSION XTEMP5(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMPTH(*)
      DIMENSION TEMP6(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
      INTEGER N
      INTEGER NUMSE1(10)
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
!
      DIMENSION TEMP0(*)
      DIMENSION TEMPZ0(*)
      DIMENSION TMPZ20(*)
      DIMENSION RES1(*)
      DIMENSION RES2(*)
!
      PARAMETER (MAXPAR=1)
      DIMENSION ZMEAN(MAXPAR)
      DIMENSION ZMED(MAXPAR)
      DIMENSION ZSD(MAXPAR)
      DIMENSION ZMAD(MAXPAR)
      DIMENSION NFAIL(MAXPAR)
!
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
!
      DIMENSION APERC(*)
      DIMENSION BPERC(*)
!
      PARAMETER (NUMALP=6)
      DIMENSION ALPHAV(NUMALP)
      DIMENSION ALOWPA(NUMALP,1)
      DIMENSION AUPPPA(NUMALP,1)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHAV /0.50, 0.25, 0.10, 0.05, 0.01, 0.001/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='JBS6'
      ISUBN2='    '
      IVRBSV=IVRBCM
      IDS4SV=IDS4CM
      IVRBCM='OFF'
      IDS4CM='OFF'
!
      RIGH1=CPUMIN
      RIGH2=CPUMIN
      RIGHT0=CPUMIN
      NRESAM=0
      NBELOW=0
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'JBS6')THEN
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
        DO 73 I=1,N
          WRITE(ICOUT,74)I,Y(I),XDESGN(I,1),Z(I),Z2(I)
   74     FORMAT('I, Y(I),XDESGN(I,1),Z(I),Z2(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
        WRITE(ICOUT,78)IBOOCI,PBOOTS,IBOOSM,PBOOSM
   78   FORMAT('IBOOCI,PBOOTS,IBOOSM,PBOOSM = ',2(A4,2X,G15.7))
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IWRITE='OFF'
!CCCC NOTE 2011/10: FOR STATISTIC CASE, THERE IS A SINGLE PARAMETER
!CCCC               COMPUTED.
!CCCC NUMPAR=NUMV2
      NUMPAR=1
      I2=0
      ISIZE2=0
      NUMSET=0
      DO 120 I=1,NGRPV
        NUMSE1(I)=0
  120 CONTINUE
!
      NACC=0
      NREJ=0
!
!     1) IF t-PERCENTILE REQUESTED, CHECK THAT A POSITIVE
!        STANDARD DEVIATION HAS BEEN ENTERED
!     2) IF BCA AND t-PERCENTILE BOTH SPECIFIED, USE t-PERCENTILE
!
      IBCASV=IBCABT
      IBOOC2=IBOOCI
!
      IF(IBOOCI.EQ.'T   ')THEN
        IF(PBOOTS.LE.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
  111     FORMAT('***** WARNING IN BOOTSTRAP/JACKNIFE PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,112)
  112     FORMAT('      WHEN THE t-PERCENTILE CONFIDENCE INTERVALS ',   &
                 'ARE RESQUESTED,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,113)
  113     FORMAT('      A POSITIVE STANDARD DEVIATION FOR THE ',   &
                 'SPECIFIED STATISTIC MUST BE GIVEN.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,114)
  114     FORMAT('      TO DO THIS, ENTER THE COMMAND:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,115)
  115     FORMAT('          SET BOOTSTRP T PERCENTILE STANDARD ',   &
                 'DEVIATION  <value>')
          CALL DPWRST('XXX','BUG ')
          IBOOCI='PERC'
        ELSE
          IBCABT='OFF'
        ENDIF
      ENDIF
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,132)
  132   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE MUST BE AT LEAST 1;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,134)N
  134   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
!               **  FOR THE GROUP VARIABLE (USUALLY VAR. 2)           **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
!               **  WHICH IS AN ERROR CONDITION FOR A PLOT.           **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NGRPV.GE.1)THEN
        NUMSET=1
        DO 170 J=1,NGRPV
          CALL DISTIN(XDESGN(1,J),N,IWRITE,XIDTEM(1,J),NUMSE1(J),   &
                      IBUGG3,IERROR)
          CALL SORT(XIDTEM(1,J),NUMSE1(J),XIDTEM(1,J))
          NUMSET=NUMSET*NUMSE1(J)
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')THEN
            WRITE(ICOUT,171)NGRPV,J,NUMSE1(J),NUMSET
  171       FORMAT('NGRPV,J,NUMSE1(J),NUMSET = ',4I8)
            CALL DPWRST('XXX','BUG ')
            DO 172 K=1,NUMSE1(J)
              WRITE(ICOUT,173)K,XIDTEM(K,J)
  173         FORMAT('K,XIDTEM(K,J) = ',I8,G15.7)
              CALL DPWRST('XXX','BUG ')
  172       CONTINUE
          ENDIF
!
          IF(NUMSE1(J).LT.1 .OR. NUMSE1(J).GE.N)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,181)
  181       FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,182)
  182       FORMAT('      THE NUMBER OF SETS FOR THE GROUP ONE ',   &
                   'VARIABLE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,183)
  183       FORMAT('      IS ZERO OR EQUAL TO THE NUMBER OF POINTS.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,184)NUMSE1(J)
  184       FORMAT('      NUMBER OF SETS = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
  170   CONTINUE
!
      ENDIF
!
      AN=N
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')IFLAG3=1
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
  211   FORMAT(6X,'LIMIT',11X,'LIMIT',12X,'Z0',13X,'A0',6X,'ALPHA1',   &
               3X,'ALPHA2')
        WRITE(IOUNI3,213)
  213   FORMAT('---------------------------------------------------',   &
               '-------------------------')
      ENDIF
!
!               ******************************************
!               **  STEP 11--                           **
!               **  COMPUTE THE SPECIFIED STATISTIC     **
!               **  FOR EACH SUBSET OF THE DATA, AND    **
!               **  THEN FOR THE FULL DATA SET          **
!               ******************************************
!
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      ICNT9=0
!
      J=0
      J2=0
      ISETMX=NUMSET+1
      NMAX=MAX(N,N2)
!
      DO 11000 ISET=1,ISETMX
!
        CALL DPJBS8(ISETMX,ISET,NUMSET,NUMSE1,N,N2,NGRPV,   &
                    MAXNXT,MAXBGR,NUMV2,   &
                    Y,Z,Z2,XDESGN,XIDTEM,TEMP0,TEMPZ0,TMPZ20,   &
                    NS2,NSS2,NI,NI2,ISET1,ISET2,   &
                    ISUBRO,IBUGG3,IERROR)
                                                                                                                                  
!
        NRESAM=NS2
        IF(ICASJB.EQ.'BOOT')NRESAM=IBOOSS
!
!       AUGUST 2002.  SIMPLIFY CODE BY USING "CMPSTA" TO COMPUTE
!       STATISTIC.  NOTE THAT THE FOLLOWING DISTINCT CASES ARE
!       SUPPORTED:
!
!       1) STATISTIC COMPUTED FROM A SINGLE RESPONSE VARIABLE
!          (MOST CASES IN THIS CATEGORY, E.G., THE MEAN)
!       2) STATISTIC COMPUTED FROM TWO RESPONSE VARIABLES,
!          RESPONSES ARE PAIRED (E.G., THE CORRELATION).
!       3) STATISTIC COMPUTED FROM TWO RESPONSE VARIABLES, THE
!          RESPONSES ARE NOT PAIRED (I.E., SAMPLE THE TWO VARIABLES
!          SEPARATELY).  CURRENTLY, NO CASES FOR THIS.
!       4) LINEAR AND QUADRATIC CALIBRATION HANDLED SEPARATELY.
!       5) LINEAR SLOPE, LINEAR CORRELATION, LINEAR RESSD,
!          LINEAR INTERCEPT HANDLED SEPARATELY.
!       6) SUMMARY STATISTIC CASE HANDLED SEPARATELY (USE THE
!          PARAMETERIC BOOTSTRAP)
!
!
!       HANDLE LINEAR CALIBRATION, QUADRATIC CALIBRATION SEPARATELY.
!
        IF(ICASPL.EQ.'LICA')GO TO 12240
        IF(ICASPL.EQ.'QUCA')GO TO 12240
!
!       FOR REMAINING CASES, DEFINE
!
!       ICASE = 1  - SINGLE RESPONSE VARIABLE
!       ICASE = 2  - PAIRED RESPONSE VARIABLES
!       ICASE = 3  - UNPAIRED RESPONSE VARIABLES
!       ICASE = 4  - LINEAR CORRELATION, LINEAR INTERCEPT,
!                    LINEAR SLOPE, LINEAR RESSD
!       ICASE = 5  - SUMMARY STATISTICS
!
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
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS6')THEN
          WRITE(ICOUT,1107)ICASE,IBOOGR,IFLAGD
 1107     FORMAT('ICASE,IBOOGR,IFLAGD ',I5,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!  CASES WITH TWO UNPAIRED RESPONSE VARIABLES (NOT NECESSARILY OF
!  SAME SIZE)
!
        IF(ICASPL.EQ.'ORSE' .OR. ICASPL.EQ.'ODRA' .OR.   &
           ICASPL.EQ.'LOSE' .OR. ICASPL.EQ.'LODR' .OR.   &
           ICASPL.EQ.'DBPR' .OR. ICASPL.EQ.'WOSM')THEN
           ICASE=3
           IBCABT='OFF'
        ENDIF
!
        IF(ICASPL.EQ.'LIIN' .OR. ICASPL.EQ.'LISL' .OR.   &
           ICASPL.EQ.'LIIS' .OR. ICASPL.EQ.'LISS' .OR.   &
           ICASPL.EQ.'CINT' .OR. ICASPL.EQ.'CSD ' .OR.   &
           ICASPL.EQ.'LIRE' .OR. ICASPL.EQ.'LICO')ICASE=4
!
!       FOR BCA OR t-PERCENTILE CONFIDENCE INTERVALS, COMPUTE
!       FULL-SAMPLE STATISTIC.
!
        IF(IBCABT.EQ.'ON'.AND.ICASJB.EQ.'BOOT')THEN
          CALL CMPSTA(   &
               TEMP0,TEMPZ0,TMPZ20,XTEMP1,XTEMP2,XTEMP3,   &
               XTEMP4,XTEMP5,   &
               MAXNXT,NS2,NS2,NS2,NUMV2,ICASPL,ISTARA,   &
               ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
               DTEMP1,DTEMP2,DTEMP3,   &
               THETHT,   &
               ISUBRO,IBUGG3,IERROR)
          NBELOW=0
        ELSEIF(IBOOCI.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
          CALL CMPSTA(   &
               TEMP0,TEMPZ0,TMPZ20,XTEMP1,XTEMP2,XTEMP3,   &
               XTEMP4,XTEMP5,   &
               MAXNXT,NS2,NS2,NS2,NUMV2,ICASPL,ISTARA,   &
               ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
               DTEMP1,DTEMP2,DTEMP3,   &
               STATD,   &
               ISUBRO,IBUGG3,IERROR)
          STATSD=PBOOTS
        ENDIF
!
        IF(ICASE.EQ.4)THEN
          IF(ICASPL.EQ.'CINT' .OR. ICASPL.EQ.'CSD')THEN
            CALL MEAN(TEMP0,NS2,IWRITE,ALPHA,IBUGG3,IERROR)
            CALL SD(TEMP0,NS2,IWRITE,SDALPH,IBUGG3,IERROR)
            BETA0=0.0
            SDBETA=0.0
          ELSE
            CALL LINFIT(TEMP0,TEMPZ0,NS2,   &
                        ALPHA,BETA,XRESSD,XRESDF,   &
                        CCXY,SDALPH,SDBETA,CCALBE,   &
                        ISUBRO,IBUGG3,IERROR)
            ALPHA0=ALPHA
            BETA0=BETA
          ENDIF
          DO 11031 I=1,NS2
            RES1(I)=TEMP0(I)-(ALPHA0+BETA0*TEMPZ0(I))
11031     CONTINUE
        ELSE
          DO 11033 I=1,NS2
            RES1(I)=TEMP0(I)
11033     CONTINUE
        ENDIF
!
        TAGID=1.0
        DO 11361 IRESAM=1,NRESAM
!
!         STEP 1: RESAMPLE ORIGINAL DATA.
!
          IF(ICASE.EQ.5)THEN
            DO 11300 IROW=1,NS2
              NTEMP=INT(TMPZ20(IROW)+0.5)
              AMEAN=TEMP0(IROW)
              ASD=TEMPZ0(IROW)
              CALL NORRAN(NTEMP,ISEED,XTEMP1)
              DO 11301 IJ=1,NTEMP
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
            CALL DPJBS3(RES1,NS2,ICASJB,IRESAM,ISEED,TEMP,NS3,ITEMP1,   &
                        TEMP4,IBUGG3,IERROR)
            IF(IBOOSM.EQ.'ON')THEN
              CALL NORRAN(NS3,ISEED,XTEMP1)
              IF(PBOOSM.EQ.CPUMIN)THEN
                AFACT=1.0/SQRT(REAL(NS3))
              ELSE
                AFACT=PBOOSM
              ENDIF
              DO 11311 IJ=1,NS3
                TEMP(IJ)=TEMP(IJ) + AFACT*XTEMP1(IJ)
11311         CONTINUE
            ENDIF
          ENDIF
!
!         CREATE ADDITIONAL RESPONSE VARIABLES FOR SPECIAL CASES
!         WHERE NEEDED.
!
          IF(ICASE.EQ.2)THEN
            NS32=NS3
            DO 11363 IJ=1,NS3
              TEMPZ(IJ)=TEMPZ0(ITEMP1(IJ))
11363       CONTINUE
          ELSEIF(ICASE.EQ.3)THEN
            NS32=NSS2
            IF(ICASPL.EQ.'WOSM')THEN
              DO 11323 IJ=1,NS3
                TEMPZ(IJ)=TEMPZ0(IJ)
11323         CONTINUE
            ELSE
              CALL DPJBS3(TEMPZ0,NSS2,ICASJB,IRESAM,ISEED,TEMPZ,NS32,   &
                          ITEMP1,TEMP4,IBUGG3,IERROR)
              IF(IBOOSM.EQ.'ON')THEN
                CALL NORRAN(NS32,ISEED,XTEMP1)
                IF(PBOOSM.EQ.CPUMIN)THEN
                  AFACT=1.0/SQRT(REAL(NS32))
                ELSE
                  AFACT=PBOOSM
                ENDIF
                DO 11321 IJ=1,NS3
                  TEMPZ(IJ)=TEMPZ(IJ) + AFACT*XTEMP1(IJ)
11321           CONTINUE
              ENDIF
            ENDIF
          ELSEIF(ICASE.EQ.4)THEN
            DO 11368 I=1,NS3
              TEMP0(I)=(ALPHA0+BETA0*TEMPZ0(I))+TEMP(I)
11368       CONTINUE
          ENDIF
!
!         STEP 2: COMPUTE THE STATISTIC
!
          CALL CMPSTA(   &
               TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
               XTEMP4,XTEMP5,   &
               MAXNXT,NS3,NS32,NS32,NUMV2,ICASPL,ISTARA,   &
               ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
               DTEMP1,DTEMP2,DTEMP3,   &
               RIGHT,   &
               ISUBRO,IBUGG3,IERROR)
!
!         STEP 3: COMPARE COMPUTED STATISTIC FROM BOOTSTRAP SAMPLE
!                 TO STATISTIC FROM ORIGINAL DATA (FOR BCA) AND ALSO
!                 COMPUTE PLOT COORDINATES.
!
          IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
            IF(RIGHT.LT.THETHT)NBELOW=NBELOW+1
            TEMP6(IRESAM)=RIGHT
          ENDIF
          IF(NGRPV.LE.1)THEN
            CALL DPJBS4(ISET,NUMSET,J,J2,RIGHT,TAGID,XIDTEM(1,1),   &
                        Y2,X2,D2)
          ELSEIF(NGRPV.EQ.2)THEN
            CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,RIGHT,   &
                        XIDTEM(1,1),Y2,X2,D2)
          ENDIF
11361   CONTINUE
!
!       FOR BCA CONFIDENCE INTERVAL, COMPUTE:
!       1) Z0HAT
!       2) JACKNIFE ESTIMATES
!
        IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
          CALL NORPPF(REAL(NBELOW)/REAL(NRESAM),Z0HAT)
          ICASZZ='JACK'
          DO 11371 IRESAM=1,NS2
!
            CALL DPJBS3(RES1,NS2,ICASZZ,IRESAM,ISEED,TEMP,NS3,ITEMP1,   &
                        TEMP4,IBUGG3,IERROR)
            IF(ICASE.EQ.2)THEN
              DO 11373 IJ=1,NS2
                TEMPZ(IJ)=TEMPZ0(ITEMP1(IJ))
                IF(NUMV2.GE.3)THEN
                  TEMPZ2(IJ)=TMPZ20(ITEMP1(IJ))
                ENDIF
11373         CONTINUE
            ELSEIF(ICASE.EQ.3)THEN
              CALL DPJBS3(TEMPZ0,NS22,ICASJB,IRESAM,ISEED,TEMPZ,NS32,   &
                          ITEMP1,TEMP4,IBUGG3,IERROR)
              IF(NUMV2.GE.3)THEN
                CALL DPJBS3(TMPZ20,NS22,ICASJB,IRESAM,ISEED,TEMPZ2,NS32,   &
                            ITEMP1,TEMP4,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASE.EQ.4)THEN
              DO 11378 I=1,NS3
                TEMP0(I)=(ALPHA0+BETA0*TEMPZ0(I))+TEMP(I)
11378         CONTINUE
            ENDIF
!
            CALL CMPSTA(   &
                 TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                 XTEMP4,XTEMP5,   &
                 MAXNXT,NS3,NS3,NS3,NUMV2,ICASPL,ISTARA,   &
                 ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                 DTEMP1,DTEMP2,DTEMP3,   &
                 RIGHT,   &
                 ISUBRO,IBUGG3,IERROR)
            TEMPTH(IRESAM)=RIGHT
11371     CONTINUE
          CALL MEAN(TEMPTH,NS2,IWRITE,THETDT,IBUGG3,IERROR)
          DSUM1=0.0D0
          DSUM2=0.0D0
          DTHETM=DBLE(THETDT)
          DO 11365 I=1,NS2
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
          CALL PERCEN(100.0*ALPHA2,TEMP6,NS2,IWRITE,TEMP4,MAXNXT,   &
                      BCAUL,IBUGG3,IERROR)
          CALL PERCEN(100.0*ALPHA1,TEMP6,NS2,IWRITE,TEMP4,MAXNXT,   &
                      BCALL,IBUGG3,IERROR)
          IF(NGRPV.EQ.1)THEN
            WRITE(IOUNI3,11388)BCALL,BCAUL,Z0HAT,A0HAT,ALPHA1,ALPHA2,   &
                               XIDTEM(ISET,1)
          ELSEIF(NGRPV.EQ.2)THEN
            WRITE(IOUNI3,11388)BCALL,BCAUL,Z0HAT,A0HAT,ALPHA1,ALPHA2,   &
                               XIDTEM(ISET1,1),XIDTEM(ISET2,2)
          ELSE
            WRITE(IOUNI3,11388)BCALL,BCAUL,Z0HAT,A0HAT,ALPHA1,ALPHA2
          ENDIF
11388     FORMAT(4E15.7,2F8.4,2F10.0)
!
!       2011/10: IMPLEMENT PERCENTILE T BOOTSTRAP:
!
!                   Z = (STAT(data) - STAT(boot))/SD(boot)
!
!                   LOWER CI: STAT(data) + SD(data)*Q(Z,alpha/2)
!                   UPPER CI: STAT(data) + SD(data)*Q(Z,1 - alpha/2)
!
        ELSEIF(IBOOCI.EQ.'T   ')THEN
        ENDIF
!
        GO TO 79000
!
!CCCC   NOTE: FOR CALIBRATION, THERE ARE TWO METHODS FOR PERFORMING
!CCCC         THE BOOTSTRAP.
!CCCC
!CCCC         1) "RESI" USES EFROM METHOD OF RESAMPLING THE RESIDUALS.
!CCCC
!CCCC         2) "DATA" USES WU METHOD OF RESAMPLING THE ORIGINAL
!CCCC            Y AND X.
!CCCC
!CCCC         IN EITHER CASE, THE PARAMETER Y0 SHOULD BE PRE-DEFINED.
!CCCC
!CCCC         AFTER QUADRATIC FIT, QUADRATIC FORMULA IS:
!CCCC             X = (-b +/- SQRT(b**2 - 4*a*c))/(2*a)
!
12240   CONTINUE
!
        IHP='Y0  '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
             IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
             ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          GO TO 9000
        ELSE
          Y0=VALUE(ILOCP)
        ENDIF
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
          DO 12249 I=1,NS2
            WRITE(ICOUT,12242)I,TEMP0(I),TEMPZ0(I)
12242       FORMAT('I,TEMP0(I),TEMPZ0(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
12249     CONTINUE
        ENDIF
!
        CALL MINIM(TEMPZ0,NS2,IWRITE,XLEFT,IBUGG3,IERROR)
        CALL MAXIM(TEMPZ0,NS2,IWRITE,XRIGHT,IBUGG3,IERROR)
!
        IF(IBOOME.EQ.'RESI')THEN
!
!  GENERATE FIT AND RESIDUALS FROM ORIGINAL DATA.
!
          IF(ICASPL.EQ.'LICA')THEN
            CALL LINFIT(TEMP0,TEMPZ0,NS2,   &
                        ALPHA,BETA,XRESSD,XRESDF,   &
                        CCXY,SDALPH,SDBETA,CCALBE,   &
                        ISUBRO,IBUGG3,IERROR)
            ALPHA0=ALPHA
            BETA0=BETA
            DO 12251 I=1,NS2
              RES1(I)=TEMP0(I)-(ALPHA0+BETA0*TEMPZ0(I))
12251       CONTINUE
!
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
              WRITE(ICOUT,12533)ALPHA0,BETA0
12533         FORMAT('ALPHA0,BETA0 = ',2G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSEIF(ICASPL.EQ.'QUCA')THEN
            CALL QUAFI2(TEMPZ0,TEMP0,NS2,   &
                        XTEMP1,   &
                        ALPHA,BETA1,BETA2,   &
                        ISUBRO,IBUGG3,IERROR)
            ALPHA0=ALPHA
            BETA10=BETA1
            BETA20=BETA2
!
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
!
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB6')THEN
              WRITE(ICOUT,12262)RIGH10,RIGH20
12262         FORMAT('FULL SAMPLE ROOTS: RIGH10,RIGH20 = ',2E15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            DO 12271 I=1,NS2
              AJUNK1=TEMPZ0(I)
              RES1(I)=TEMP0(I)-(ALPHA0+BETA10*AJUNK1+BETA20*AJUNK1**2)
12271       CONTINUE
!
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
              WRITE(ICOUT,12273)ALPHA0,BETA10,BETA20
12273         FORMAT('ALPHA0,BETA10,BETA20 = ',3G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ENDIF
!
!         RESAMPLE RESIDUALS.
!
          NREJ=0
          NNEG=0
          DO 12281 IRESAM=1,NRESAM
            CALL DPJBS3(RES1,NS2,ICASJB,IRESAM,ISEED,RES2,NS3,ITEMP1,   &
                        TEMP4,IBUGG3,IERROR)
            IF(ICASPL.EQ.'LICA')THEN
              DO 12282 I=1,NS3
                TEMP(I)=(ALPHA0+BETA0*TEMPZ0(I))+RES2(I)
12282         CONTINUE
              CALL LINFIT(TEMP,TEMPZ0,NS3,   &
                          ALPHA,BETA,XRESSD,XRESDF,   &
                          CCXY,SDALPH,SDBETA,CCALBE,   &
                          ISUBRO,IBUGG3,IERROR)
              A0=ALPHA
              A1=BETA
              RIGHT=(Y0-A0)/A1
!
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
                WRITE(ICOUT,12283)IRESAM,ALPHA,BETA,RIGHT
12283           FORMAT('IRESAM,ALPHA0,BETA0,RIGHT = ',I8,3G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
            ELSEIF(ICASPL.EQ.'QUCA')THEN
              DO 12286 I=1,NS3
                AJUNK1=TEMPZ0(I)
                TEMP(I)=(ALPHA0+BETA10*AJUNK1+BETA20*AJUNK1**2)+RES2(I)
12286         CONTINUE
              CALL QUAFI2(TEMPZ0,TEMP,NS3,   &
                          XTEMP1,   &
                          ALPHA,BETA1,BETA2,   &
                          ISUBRO,IBUGG3,IERROR)
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
                GO TO 12281
              ENDIF
!
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
                WRITE(ICOUT,12287)IRESAM,ALPHA,BETA1,BETA2,RIGHT
12287           FORMAT('IRESAM,ALPHA,BETA1,BETA2,RIGHT = ',I8,3G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,12288)A,B,C,TERM1
12288           FORMAT('A, B, C, TERM1 = ',4G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
            ENDIF
!
            TAGID=1.0
            IF(NGRPV.LE.1)THEN
              CALL DPJBS4(ISET,NUMSET,J,J2,RIGHT,TAGID,   &
                          XIDTEM(1,1),Y2,X2,D2)
            ELSE
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,RIGHT,   &
                          XIDTEM(1,1),Y2,X2,D2)
            ENDIF
12281     CONTINUE
!
        ELSE
!
!         RESAMPLE ORIGINAL Y AND X VALUES (ROWS OF Y AND X SHOULD
!         REMAIN PAIRED).
!
          NNEG=0
          NREJ=0
          DO 12291 IRESAM=1,NRESAM
            CALL DPJBS3(TEMP0,NS2,ICASJB,IRESAM,ISEED,TEMP,NS3,ITEMP1,   &
                        TEMP4,IBUGG3,IERROR)
            DO 12292 IJ=1,NS3
              TEMPZ(IJ)=TEMPZ0(ITEMP1(IJ))
12292       CONTINUE
            IF(ICASPL.EQ.'LICA')THEN
              CALL LINFIT(TEMP,TEMPZ,NS3,   &
                          ALPHA,BETA,XRESSD,XRESDF,   &
                          CCXY,SDALPH,SDBETA,CCALBE,   &
                          ISUBRO,IBUGG3,IERROR)
              A0=ALPHA
              A1=BETA
              RIGHT=(Y0-A0)/A1
!
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB6')THEN
                WRITE(ICOUT,12293)IRESAM,ALPHA,BETA,RIGHT
12293           FORMAT('IRESAM,ALPHA0,BETA0,RIGHT = ',I8,3G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
            ELSEIF(ICASPL.EQ.'QUCA')THEN
              CALL QUAFI2(TEMPZ,TEMP,NS3,   &
                   XTEMP1,   &
                   ALPHA,BETA1,BETA2,   &
                   ISUBRO,IBUGG3,IERROR)
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
                GO TO 12291
              ENDIF
!
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JSB2')THEN
                WRITE(ICOUT,12287)IRESAM,ALPHA,BETA1,BETA2,RIGHT
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
            ENDIF
!
            TAGID=1.0
            IF(NGRPV.LE.1)THEN
              CALL DPJBS4(ISET,NUMSET,J,J2,RIGHT,TAGID,   &
                          XIDTEM(1,1),Y2,X2,D2)
            ELSE
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,RIGHT,   &
                          XIDTEM(1,1),Y2,X2,D2)
            ENDIF
!
12291     CONTINUE
!
        ENDIF
!
        IF(NREJ.GT.0)THEN
          WRITE(ICOUT,12301)
12301     FORMAT('***** WARNING FROM BOOTSTRAP PLOT--',   &
                 'QUADRATIC CALIBRATION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12303)NREJ
12303     FORMAT('      FOR ',I8,' BOOTSTRAP SAMPLES, NO REAL ROOTS ',   &
                 'FOR THE QUADRATIC EQUATION.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(NNEG.GT.0)THEN
          WRITE(ICOUT,12301)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12305)NREJ
12305     FORMAT('      FOR ',I8,' BOOTSTRAP SAMPLES, NEGATIVE ROOT ',   &
                 'SELECTED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 79000
!
79000   CONTINUE
!
!               ************************************************
!               **   STEP 19--                                **
!               **   FOR GROUPED DATA, WRITE GROUP-ID, MEAN,  **
!               **   MEDIAN, B025, B975, B05, B90, B005, B995 **
!               **   TO DPST1F.DAT.                           **
!               ************************************************
!
!CCCC JANUARY 2005.  FOR UNGROUPED DATA, WRITE BOOTSTRAP ESTIMATES
!CCCC                TO FILE.  ALSO, ACCOMODATE CASE WHERE MORE
!CCCC                THAN ONE PARAMETER IS ESTIMATED.
!
        CALL DPJBS9(Y2,D2,TEMP,XTEMP1,XTEMP2,MAXNXT,IOUNI1,IOUNI2,   &
                    NUMPAR,NGRPV,NUMSET,ISET,ISET1,ISET2,NUMSE1,J,   &
                    APERC,BPERC,NPERC,   &
                    BMEAN,BSD,BMIN,BMAX,BMAD,   &
                    B001,B005,B01,B025,B05,B10,B20,B50,   &
                    B80,B90,B95,B975,B99,B995,B999,   &
                    ALOWPA,AUPPPA,ALPHAV,NUMALP,   &
                    ZMEAN,ZMED,ZSD,ZMAD,NFAIL,   &
                    ISUBRO,IBUGG3,IERROR)
!
         DO 79003 II=1,NUMPAR
           NFAIL(II)=NRESAM - NFAIL(II)
79003    CONTINUE
!
!       ************************************************
!       **   STEP 20--                                **
!       **   GENERATE  A NUMERIC TABLE OF THE RESULTS **
!       ************************************************
!
        IF(IPRINT.EQ.'OFF')GO TO 11000
!
        ICNT9=ICNT9+1
        IF(ICNT9.EQ.1)THEN
          ITITLE(1:27)='Bootstrap Analysis for the '
           DO 8211 II=60,1,-1
            IF(ISTANM(II:II).NE.' ')THEN
               NCSTAT=II
               GO TO 8219
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
!
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
!
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
!
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
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
!
        NUMROW=ICNT
        DO 8321 II=1,NUMROW
          NTOT(II)=15
 8321   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                    AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGG3,IERROR)
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
!
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
!
        NMAX=0
        DO 2521 II=1,NUMCOL
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
        DO 2523 II=1,NUMROW
          DO 2525 JJ=1,NUMCOL
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
!
        IWHTML(1)=150
        IWHTML(2)=50
        IWHTML(3)=150
        IWRTF(1)=2000
        IWRTF(2)=IWRTF(1)+500
        IWRTF(3)=IWRTF(2)+2000
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                    ITITLE,NCTITL,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUZ,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    ISUBRO,IBUGG3,IERROR)
!
        CALL DPDT8B(ALOWPA(1,1),AUPPPA(1,1),ALPHAV,NUMALP,   &
                    ICAPSW,ICAPTY,NUMDIG,   &
                    ISUBRO,IBUGG3,IERROR)
!
11000 CONTINUE
!
      NPLOTP=J
      NPLOTV=3
!
      IOP='CLOS'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')IFLAG3=1
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IFEEDB.EQ.'ON')THEN
!
          WRITE(ICOUT,8102)
 8102     FORMAT('THE FOLLOWING INFORMATION IS WRITTEN TO FILES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8104)
 8104     FORMAT('DPST1F.DAT: THE BOOTSTRAP VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8106)
 8106     FORMAT('            FOR GROUPED DATA, THE FIRST ONE (OR ',   &
                 'TWO) COLUMNS IDENTIFY THE GROUP(S).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8112)
 8112     FORMAT('DPST2F.DAT: STATISTICS BASED ON BOOTSTRAP VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8114)
 8114     FORMAT('            MEAN, SD, MEDIAN, B025, ',   &
                 'B975, B05, B95, B005, B995')
          CALL DPWRST('XXX','BUG ')
          IF(NUMPAR.GT.1)THEN
            WRITE(ICOUT,8118)
 8118       FORMAT('            THE FIRST COLUMN IDENTIFIES THE ',   &
                   'PARAMETER.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,8116)
 8116     FORMAT('            FOR GROUPED DATA, THE FIRST ONE (OR ',   &
                 'TWO) COLUMNS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8117)
 8117     FORMAT('            (AFTER THE PARAMETER ID) IDENTIFY ',   &
                 'THE GROUP(S).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
!
        ENDIF
!
!CCCC ENDIF
!
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')THEN
!
        IF(IFEEDB.EQ.'ON')THEN
!
          WRITE(ICOUT,8102)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8131)
 8131     FORMAT('DPST3F.DAT: BCa CONFIDENCE INTERVALS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8132)
 8132     FORMAT('LOWER INTERVAL, UPPER INTERVAL, Z0HAT, A0HAT, ',   &
               'ALPHA1, ALPHA2, GROUP 1 ID, GROUP 2 ID')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8134)
 8134     FORMAT('WITH 4E15.7,2F8.4,2F10.0 FORMAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
!
        ENDIF
!
      ENDIF
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IBCABT=IBCASV
      IBOOCI=IBOOC2
      IVRBCM=IVRBSV
      IDS4CM=IDS4SV
!
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
        DO 9020 I=1,NPLOTP
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPJBS6
      SUBROUTINE DPJBS7(Y,Z,XLEVEL,XDESGN,N,NUMV2,NGRPV,   &
                        ICASPL,ICASP2,IDIST,   &
                        ICENSO,ISIZE,ICONT,NPERC,KSLOC,KSSCAL,   &
                        IMETHD,ILEVEL,   &
                        ICASJB,IBOOSS,ISEED,IBCABT,ALPHA,   &
                        TEMP,TEMPZ,TEMP0,TEMPZ0,TEMPL,TEMPZL,   &
                        QP,XQP,XQPLCL,XQPUCL,   &
                        XIDTEM,XTEMP1,XTEMP2,XTEMP3,TEMP4,   &
                        ZTEMP1,ZTEMP2,ZTEMP3,ZTEMP4,ZTEMP5,ZTEMP6,   &
                        ZTEMP7,ZTEMP8,ZTEMP9,ZTMP10,ZTMP11,ZTMP12,   &
                        MAXNXT,MAXBGR,   &
                        ITEMP1,DTEMP1,DTEMP2,DTEMP3,   &
                        YLOWLM,YUPPLM,A,B,MINMAX,   &
                        SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,   &
                        SHAPE6,SHAPE7,NUMSHA,   &
                        SHAP11,SHAP12,SHAP21,SHAP22,   &
                        Y2,X2,D2,NPLOTP,NPLOTV,   &
                        APERC,BPERC,NPERC2,   &
                        BMEAN,BSD,B001,B005,B01,B025,B05,B10,B20,B50,   &
                        B80,B90,B95,B975,B99,B995,B999,   &
                        ICAPSW,ICAPTY,IFORSW,IVARID,IVARI2,   &
                        CLLIMI,CLWIDT,IRELAT,   &
                        IFLAGL,AL,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A JACKNIFE OR BOOTSTRAP PLOT
!              (SEE DPJBSP FOR ALLOWABLE TYPES)
!
!              NOTE: THIS ROUTINE EXTRACTED FROM ORIGINAL DPJBS2.
!                    IT PERFORMS THE BOOTSTRAP FOR "DISTRIBUTIONS".
!                    THE BOOTSTRAP FOR STATISTICS WAS EXTRACTED TO
!                    DPJBS6.
!
!                    WITH THIS EXTRACTION, TAKE THE OPPORTUNITY TO
!                    SIMPLIFY THE CODE A BIT AS WELL.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/05
!     ORIGINAL VERSION--MAY       2010. EXTRACTED FROM DPJBS2
!     UPDATED         --AUGUST    2011. OPTION FOR ONE-SIDED PERCENTILES
!                                       (= ONE-SIDED TOLERANCE INTERVALS)
!     UPDATED         --AUGUST    2011. SOME MODIFICATIONS FOR BETTER
!                                       HANDLING CASES WHERE THERE ARE
!                                       ERRORS IN THE PARAMETER
!                                       ESTIMATES
!     UPDATED         --MARCH     2013. FOR WEIBULL, ADJUST SCALE
!                                       PARAMETER IF GAUGE LENGTH
!                                       OPTION SPECIFIED
!     UPDATED         --JULY      2019. CALL LIST TO CMPDIS
!     UPDATED         --JULY      2019. REMOVE ZTMP13, ZTMP14 FROM
!                                       CALL LIST
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP2
      CHARACTER*4 ICONT
      CHARACTER*4 ICENSO
      CHARACTER*4 IMETHD
      CHARACTER*4 ILEVEL
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IRELAT
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
!
      CHARACTER*60 IDIST
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IWRITE
      CHARACTER*4 IBCABT
      CHARACTER*4 IOP
      CHARACTER*4 ICASJB
      CHARACTER*4 ILIMIT
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*25 IFORMT
      CHARACTER*25 IFORMZ
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION XLEVEL(*)
      DIMENSION XDESGN(MAXNXT,MAXBGR)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
      DIMENSION QP(*)
      DIMENSION XQP(*)
      DIMENSION XQPLCL(*)
      DIMENSION XQPUCL(*)
!
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
!
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
!
      DIMENSION CLWIDT(*)
      DIMENSION CLLIMI(*)
!
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
!
      CHARACTER*25 IPAR
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
      INTEGER N
      INTEGER NUMSE1(10)
      INTEGER ITEMP1(*)
!
      REAL KSLOC
      REAL KSSCAL
      REAL KSLSAV
      REAL KSSSAV
!
      PARAMETER(NUMCLI=3)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=20)
      CHARACTER*80 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*50 ITEXT(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL IFRST
      LOGICAL ILAST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHAV /0.50, 0.25, 0.10, 0.05, 0.025, 0.01, 0.005, 0.001/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='JBS7'
      ISUBN2='    '
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPJBS7--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IBUGG3,ISUBRO,ICASJB,IBOOSS
   71   FORMAT('IBUGG3,ISUBRO,ICASJB,IBOOSS = ',A4,2X,A4,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,ICASPL,NUMV2,ISIZE,ICONT,NGRPV,NUMSHA
   72   FORMAT('N,ICASPL,NUMV2,ISIZE,ICONT,NGRPV,NUMSHA = ',   &
               I8,2X,A4,I8,I8,2X,A4,2X,2I4)
        CALL DPWRST('XXX','BUG ')
        DO 73 I=1,MIN(N,100)
          WRITE(ICOUT,74)I,Y(I),XDESGN(I,1),Z(I)
   74     FORMAT('I, Y(I),XDESGN(I,1),Z(I) = ',I8,3F15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
!
      IWRITE='OFF'
      NUMPAR=NUMV2
      I2=0
      ISIZE2=0
      NUMSET=0
      DO 120 I=1,NGRPV
        NUMSE1(I)=0
  120 CONTINUE
!
      NACC=0
      NREJ=0
!
!     SPECIFY DISTRIBUTIONS THAT ESTIMATE LOWER/UPPER LIMIT
!     PARAMETERS RATHER THAN LOCATION/SCALE
!
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
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,131)
  131   FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,132)
  132   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE MUST BE AT LEAST 5;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,134)N
  134   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
!               **  FOR THE GROUP VARIABLE (USUALLY VAR. 2)           **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
!               **  WHICH IS AN ERROR CONDITION FOR A PLOT.           **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NGRPV.GE.1)THEN
        NUMSET=1
        DO 170 J=1,NGRPV
          CALL DISTIN(XDESGN(1,J),N,IWRITE,XIDTEM(1,J),NUMSE1(J),   &
                      IBUGG3,IERROR)
          CALL SORT(XIDTEM(1,J),NUMSE1(J),XIDTEM(1,J))
          NUMSET=NUMSET*NUMSE1(J)
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
            WRITE(ICOUT,171)NGRPV,J,NUMSE1(J),NUMSET
  171       FORMAT('NGRPV,J,NUMSE1(J),NUMSET = ',4I8)
            CALL DPWRST('XXX','BUG ')
            DO 172 K=1,NUMSE1(J)
              WRITE(ICOUT,173)K,XIDTEM(K,J)
  173         FORMAT('K,XIDTEM(K,J) = ',I8,G15.7)
              CALL DPWRST('XXX','BUG ')
  172       CONTINUE
          ENDIF
!
          IF(NUMSE1(J).LT.1 .OR. NUMSE1(J).GE.N)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,181)
  181       FORMAT('***** ERROR IN BOOTSTRAP/JACKNIFE PLOT--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,182)
  182       FORMAT('      THE NUMBER OF SETS FOR THE GROUP ONE ',   &
                   'VARIABLE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,183)
  183       FORMAT('      IS ZERO OR EQUAL TO THE NUMBER OF POINTS.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,184)NUMSE1(J)
  184       FORMAT('      NUMBER OF SETS = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
  170   CONTINUE
!
      ENDIF
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
      ICNT9=0
!
      AN=N
      IF(NPERC.GT.999)NPEC=0
!
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
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!     HEADER FOR PERCENTILE FILE IF NEEDED
!
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
!
!               ******************************************
!               **  STEP 11--                           **
!               **  COMPUTE THE SPECIFIED STATISTIC     **
!               **  FOR EACH SUBSET OF THE DATA, AND    **
!               **  THEN FOR THE FULL DATA SET          **
!               ******************************************
!
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     FOR PARAMETRIC BOOTSTRAP, COMPUTE INITIAL PARAMETER
!     ESTIMATES BASED ON FULL SAMPLE.
!
      KSLSAV=KSLOC
      KSSSAV=KSSCAL
!CCCC IF(IBOOPA.EQ.'PARA' .AND. ICENSO.EQ.'OFF')THEN
      IF(IBOOPA.EQ.'PARA' .AND. ICENSO.EQ.'OFF' .AND.   &
         ICASJB.EQ.'BOOT')THEN
        CALL CMPDIS(Y,Z,XLEVEL,N,MAXNXT,ICASPL,ICASP2,   &
                    XTEMP1,XTEMP2,XTEMP3,   &
                    DTEMP1,DTEMP2,DTEMP3,ITEMP1,   &
                    ZTEMP1,ZTEMP2,ZTEMP3,ZTEMP4,ZTEMP5,ZTEMP6,   &
                    ZTEMP7,ZTEMP8,ZTEMP9,ZTMP10,ZTMP11,   &
                    YLOWLM,YUPPLM,A,B,MINMAX,NUMSHA,   &
                    SHAP11,SHAP12,SHAP21,SHAP22,   &
                    IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                    ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,   &
                    IGOMDF,IKATDF,IGIGDF,IGEODF,IGAUDF,   &
                    IEXPBC,IWEIBC,ICENTY,IDFTTY,   &
                    MAXNXT,ICENSO,KSLOC,KSSCAL,IFORSW,ISEED,   &
                    IPPLDP,IMETHD,IPPCBW,IPPCCC,IPPCFO,   &
                    IPPCDP,IPPCAP,IPPCAO,PCHSLM,ILEVEL,   &
                    CLLIMI,CLWIDT,IHSTCW,IHSTOU,IRELAT,IRHSTG,   &
                    SH1,SH2,SH3,SH4,SH5,SH6,SH7,ALOC,ASCALE,STATVA,   &
                    IBUGG3,ISUBRO,IERROR)
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
          GO TO 9000
        ENDIF
!
        IF(IFLAGL.EQ.1 .AND. AL.GT.0.0 .AND. SH1.GT.0.0)THEN
          ASCALE=AL**(1.0/SH1)*ASCALE
        ENDIF
!
        IF(ILIMIT.EQ.'ON' .AND.   &
          (ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'AD  ' .OR.   &
           ICASP2.EQ.'KS  '))THEN
          AVAL=KSLOC + KSSCAL
          KSSCAL=AVAL
        ENDIF
      ENDIF
!
      J=0
      J2=0
      ISETMX=NUMSET+1
      NMAX=N
!
      DO 11000 ISET=1,ISETMX
!
        ISTEPN='12'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        CALL DPJBS8(ISETMX,ISET,NUMSET,NUMSE1,N,N,NGRPV,   &
                    MAXNXT,MAXBGR,NUMV2,   &
                    Y,Z,XLEVEL,XDESGN,XIDTEM,TEMP0,TEMPZ0,TEMPL,   &
                    NS2,NSS2,NI,NI2,ISET1,ISET2,   &
                    ISUBRO,IBUGG3,IERROR)
!
        NRESAM=NS2
        IF(ICASJB.EQ.'BOOT')NRESAM=IBOOSS
!
        IF(NPERC.GE.1)THEN
          IOP='OPEN'
          IFLAG1=0
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=1
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGG3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
!       SIMPLIFY CODE BY USING "CMPDIS" TO COMPUTE DISTRIBUTION
!       PARAMETERS.  NOTE THAT THE DISTRIBUTIONAL BOOTSTRAP
!       ASSUMES A SINGLE RESPONSE VARIABLE.  ALSO THE BCA CONFIDENCE
!       INTERVAL METHOD IS NOT CURRENTLY SUPPORTED FOR THE
!       DISTRIBUTIONAL BOOTSTRAP.
!
        TAGID=1.0
        DO 11361 IRESAM=1,NRESAM
!
!         STEP 1: THERE ARE TWO METHODS FOR RESAMPLING:
!
!                 1) RESAMPLE THE ORIGINAL DATA.
!                 2) GENERATE A RANDOM SAMPLE BASED ON PARAMETER
!                    ESTIMATES FROM FULL SAMPLE.
!
!                    THIS METHOD IS NOT SUPPORTED FOR CENSORED DATA.
!
          IF(IBOOPA.EQ.'PARA' .AND. ICENSO.EQ.'OFF' .AND.   &
             ICASJB.EQ.'BOOT')THEN
            CALL DPRAN2(ICASP2,ISEED,TEMP,NS2,ZTMP12,   &
                        A,B,MINMAX,   &
                        SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,   &
                        SHAPE6,SHAPE7,   &
                        IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                        ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,   &
                        IGOMDF,IKATDF,IGIGDF,IGEODF,   &
                        IBUGG3,ISUBRO,IFOUND,IERROR)
          ELSE
            CALL DPJBS3(TEMP0,NS2,ICASJB,IRESAM,ISEED,TEMP,NS3,ITEMP1,   &
                        TEMP4,IBUGG3,IERROR)
            IF(ICENSO.EQ.'ON')THEN
              NS32=NS3
              DO 11363 IJ=1,NS3
                TEMPZ(IJ)=TEMPZ0(ITEMP1(IJ))
                IF(ILEVEL.EQ.'ON')THEN
                  TEMPZL(IJ)=TEMPL(ITEMP1(IJ))
                ENDIF
11363         CONTINUE
            ENDIF
          ENDIF
!
!         STEP 2: COMPUTE THE STATISTIC
!
          KSLOC=KSLSAV
          KSSCAL=KSSSAV
          IF(ICASPL.EQ.'BFWE')SH2=SHAPE2
          CALL CMPDIS(TEMP,TEMPZ,TEMPZL,NS2,MAXNXT,ICASPL,ICASP2,   &
                      XTEMP1,XTEMP2,XTEMP3,   &
                      DTEMP1,DTEMP2,DTEMP3,ITEMP1,   &
                      ZTEMP1,ZTEMP2,ZTEMP3,ZTEMP4,ZTEMP5,ZTEMP6,   &
                      ZTEMP7,ZTEMP8,ZTEMP9,ZTMP10,ZTMP11,   &
                      YLOWLM,YUPPLM,A,B,MINMAX,NUMSHA,   &
                      SHAP11,SHAP12,SHAP21,SHAP22,   &
                      IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                      ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,   &
                      IGOMDF,IKATDF,IGIGDF,IGEODF,IGAUDF,   &
                      IEXPBC,IWEIBC,ICENTY,IDFTTY,   &
                      MAXNXT,ICENSO,KSLOC,KSSCAL,IFORSW,ISEED,   &
                      IPPLDP,IMETHD,IPPCBW,IPPCCC,IPPCFO,   &
                      IPPCDP,IPPCAP,IPPCAO,PCHSLM,ILEVEL,   &
                      CLLIMI,CLWIDT,IHSTCW,IHSTOU,IRELAT,IRHSTG,   &
                      SH1,SH2,SH3,SH4,SH5,SH6,SH7,ALOC,ASCALE,STATVA,   &
                      IBUGG3,ISUBRO,IERROR)
!CCCC     IF(IERROR.EQ.'YES')GO TO 9000
!
!         NOTE 09/2010: WHEN THERE IS A "LENGTH" VARIABLE, (I.E., THE
!                       BRITTLE FIBER WEIBULL DISTRIBUTION), THE
!                       STANDARD METHOD FOR COMPUTING THE PERCENTILES
!                       (USE ESTIMATES OF DISTRIBUTIONAL PARAMETERS IN
!                       THE PERCENT POINT FUNCTION) CANNOT BE
!                       IMPLEMENTED.
!
!                       AN ALTERNATIVE IS TO COMPUTE DATA PERCENTILES.
!
!                       SET QUANTILE TO CPUMIN IF THERE WAS AN ERROR
!                       IN THE ESTIMATION STEP.
!
          IF(IFLAGL.EQ.1 .AND. AL.GT.0.0 .AND. SH1.GT.0.0)THEN
            ASCALE=AL**(1.0/SH1)*ASCALE
          ENDIF
!
          IF(NPERC.GT.0 .AND. NPERC.LE.1000)THEN
            IF(IERROR.EQ.'YES')THEN
              DO 12112 I=1,NPERC
                XQP(I)=CPUMIN
12112         CONTINUE
            ELSE
              IF(IBOOPE.EQ.'DATA' .OR. ILEVEL.EQ.'YES')THEN
                DO 12110 I=1,NPERC
                  ATEMP=QP(I)
                  CALL PERCEN(ATEMP,TEMP,NS2,IWRITE,ZTMP12,MAXNXT,   &
                              BTEMP,IBUGG3,IERROR)
                  XQP(I)=BTEMP
12110           CONTINUE
              ELSE
                CALL DPPPF1(QP,XQP,NPERC,ICASPL,   &
                            SH1,SH2,SH3,SH4,   &
                            SH5,SH6,SH7,   &
                            YLOWLM,YUPPLM,A,B,MINMAX,   &
                            ICAPSW,ICAPTY,   &
                            IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                            ILGADF,ISKNDF,IGLDDF,IBGEDF,   &
                            IGETDF,ICONDF,IGOMDF,IKATDF,   &
                            IGIGDF,IGEODF,   &
                            ALOC,ASCALE,   &
                            IBUGG3,ISUBRO,IERROR)
              ENDIF
            ENDIF
!
            IF((NGRPV.GE.1 .AND. NPERC.GT.0 .AND. ISET.LE.NUMSET) .OR.   &
               (NPERC.GT.0 .AND. NGRPV.EQ.0 .AND. ISET.EQ.1))THEN
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
!
          IF(ILIMIT.EQ.'ON' .AND.   &
            (ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'AD  ' .OR.   &
             ICASP2.EQ.'KS  '))THEN
            AVAL=ALOC + ASCALE
            ASCALE=AVAL
          ENDIF
          IF(NGRPV.LE.1)THEN
            TAGID=0.0
            IF(ALOC.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,ALOC,TAGID,XIDTEM(1,1),   &
                          Y2,X2,D2)
            ENDIF
            IF(ASCALE.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,ASCALE,TAGID,XIDTEM(1,1),   &
                          Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.1 .AND. SH1.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH1,TAGID,XIDTEM(1,1),   &
                          Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.2 .AND. SH2.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH2,TAGID,XIDTEM(1,1),   &
                          Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.3 .AND. SH3.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH3,TAGID,XIDTEM(1,1),   &
                          Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.4 .AND. SH4.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH4,TAGID,XIDTEM(1,1),   &
                          Y2,X2,D2)
            ENDIF
            IF(NUMSHA.GE.5 .AND. SH5.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,SH5,TAGID,XIDTEM(1,1),   &
                          Y2,X2,D2)
            ENDIF
            IF(ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'KS' .OR.   &
               ICASP2.EQ.'AD' .AND. STATVA.NE.CPUMIN)THEN
              TAGID=TAGID+1.0
              CALL DPJBS4(ISET,NUMSET,J,J2,STATVA,TAGID,XIDTEM(1,1),   &
                          Y2,X2,D2)
            ENDIF
            NUMPAR=INT(TAGID+0.1)
          ELSEIF(NGRPV.EQ.2)THEN
            NUMPAR=0
            IF(ALOC.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,ALOC,   &
                          XIDTEM(1,1),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(ASCALE.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,ASCALE,   &
                          XIDTEM(1,1),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.1 .AND. SH1.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,SH1,   &
                          XIDTEM(1,1),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.2 .AND. SH2.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,SH2,   &
                          XIDTEM(1,1),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.3 .AND. SH3.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,SH3,   &
                          XIDTEM(1,1),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.4 .AND. SH4.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,SH4,   &
                          XIDTEM(1,1),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(NUMSHA.GE.5 .AND. SH5.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,SH5,   &
                          XIDTEM(1,1),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
            IF(ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'KS' .OR.   &
               ICASP2.EQ.'AD' .AND. STATVA.NE.CPUMIN)THEN
              CALL DPJBS5(ISET1,ISET2,NUMSE1(2),J,STATVA,   &
                          XIDTEM(1,1),Y2,X2,D2)
              NUMPAR=NUMPAR+1
            ENDIF
          ENDIF
11361   CONTINUE
!
!               ************************************************
!               **   STEP 19--                                **
!               **   FOR GROUPED DATA, WRITE GROUP-ID, MEAN,  **
!               **   MEDIAN, B025, B975, B05, B90, B005, B995 **
!               **   TO DPST1F.DAT.                           **
!               ************************************************
!
!CCCC JANUARY 2005.  FOR UNGROUPED DATA, WRITE BOOTSTRAP ESTIMATES
!CCCC                TO FILE.  ALSO, ACCOMODATE CASE WHERE MORE
!CCCC                THAN ONE PARAMETER IS ESTIMATED.
!
        IF(NPERC.GE.1)THEN
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGG3,ISUBRO,IERROR)
        ENDIF
!
        DO 6131 II=1,NUMALP
          DO 6133 JJ=1,9
            ALOWPA(II,JJ)=CPUMIN
            AUPPPA(II,JJ)=CPUMIN
 6133     CONTINUE
 6131     CONTINUE
!
        ISTEPN='19'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        CALL DPJBS9(Y2,D2,TEMP,XTEMP1,XTEMP2,MAXNXT,IOUNI1,IOUNI2,   &
                    NUMPAR,NGRPV,NUMSET,ISET,ISET1,ISET2,NUMSE1,J,   &
                    APERC,BPERC,NPERC2,   &
                    BMEAN,BSD,BMIN,BMAX,BMAD,   &
                    B001,B005,B01,B025,B05,B10,B20,B50,   &
                    B80,B90,B95,B975,B99,B995,B999,   &
                    ALOWPA,AUPPPA,ALPHAV,NUMALP,   &
                    ZMEAN,ZMED,ZSD,ZMAD,NFAIL,   &
                    ISUBRO,IBUGG3,IERROR)
!
         DO 6139 II=1,NUMPAR
           NFAIL(II)=NRESAM - NFAIL(II)
 6139    CONTINUE
!
!       ************************************************
!       **   STEP 20--                                **
!       **   GENERATE  A NUMERIC TABLE OF THE RESULTS **
!       ************************************************
!
        IF(IPRINT.EQ.'OFF')GO TO 11000
!
        ISTEPN='20'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          DO 8211 II=60,1,-1
            IF(IDIST(II:II).NE.' ')THEN
               NCDIST=II
               GO TO 8219
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
!
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
!
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
!
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
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
!
        IF(ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'AD' .OR.   &
           ICASP2.EQ.'KS')THEN
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
!
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        NUMROW=ICNT
        DO 8311 II=1,NUMROW
          NTOT(II)=15
 8311   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                    AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGG3,IERROR)
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
!
!       PRINT CONFIDENCE LIMITS FOR:
!
!       1) LOCATION PARAMETER
!       2) SCALE PARAMETER
!       3) SHAPE PARAMETER 1 - SHAPE PARAMETER 5
!       4) VALUE OF TEST STATISTIC
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
          WRITE(ICOUT,8321)
 8321     FORMAT('ALOWPA MATRIX:')
          CALL DPWRST('XXX','BUG ')
          DO 8320 L=1,NUMALP
            WRITE(ICOUT,8322)(ALOWPA(L,JJ),JJ=1,NUMPAR)
 8322       FORMAT(7G15.7)
            CALL DPWRST('XXX','BUG ')
 8320     CONTINUE
          WRITE(ICOUT,8331)
 8331     FORMAT('AUPPPA MATRIX:')
          CALL DPWRST('XXX','BUG ')
          DO 8330 L=1,NUMALP
            WRITE(ICOUT,8322)(AUPPPA(L,JJ),JJ=1,NUMPAR)
            CALL DPWRST('XXX','BUG ')
 8330     CONTINUE
        ENDIF
!
        ITAG=0
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
        ITITLZ=' '
        NCTITZ=0
!
!       LOCATION PARAMETER:
!
        IF(ALOC.NE.CPUMIN)THEN
          ITAG=ITAG+1
!
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
!
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
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
          NUMROW=ICNT
          DO 8312 II=1,NUMROW
            NTOT(II)=15
 8312     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IPAR='Location'
          NCPAR=8
          IF(ILIMIT.EQ.'ON')THEN
            IPAR='Lower Limit'
            NCPAR=11
          ENDIF
          CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,   &
                      ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,   &
                      ISUBRO,IBUGG3,IERROR)
        ENDIF
!
!       SCALE PARAMETER:
!
        IF(ASCALE.NE.CPUMIN)THEN
          ITAG=ITAG+1
!
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
!
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
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
          NUMROW=ICNT
          DO 8313 II=1,NUMROW
            NTOT(II)=15
 8313     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IPAR='Scale'
          NCPAR=5
          IF(ILIMIT.EQ.'ON')THEN
            IPAR='Upper Limit'
            NCPAR=11
          ENDIF
          CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,   &
                      ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,   &
                      ISUBRO,IBUGG3,IERROR)
        ENDIF
!
!       SHAPE PARAMETER ONE:
!
        IF(NUMSHA.GE.1 .AND. SH1.NE.CPUMIN)THEN
          ITAG=ITAG+1
!
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
!
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
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
          NUMROW=ICNT
          DO 8314 II=1,NUMROW
            NTOT(II)=15
 8314     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 1'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,   &
                        ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,   &
                        ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
!
!       SHAPE PARAMETER TWO:
!
        IF(NUMSHA.GE.2 .AND. SH2.NE.CPUMIN)THEN
          ITAG=ITAG+1
!
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
!
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
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
          NUMROW=ICNT
          DO 8315 II=1,NUMROW
            NTOT(II)=15
 8315     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 2'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,   &
                        ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,   &
                        ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
!
!       SHAPE PARAMETER THREE:
!
        IF(NUMSHA.GE.3 .AND. SH3.NE.CPUMIN)THEN
          ITAG=ITAG+1
!
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
!
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
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
          NUMROW=ICNT
          DO 8316 II=1,NUMROW
            NTOT(II)=15
 8316     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 3'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,   &
                        ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,   &
                        ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
!
!       SHAPE PARAMETER FOUR:
!
        IF(NUMSHA.GE.4 .AND. SH4.NE.CPUMIN)THEN
          ITAG=ITAG+1
!
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
!
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
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
          NUMROW=ICNT
          DO 8317 II=1,NUMROW
            NTOT(II)=15
 8317     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 4'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,   &
                        ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,   &
                        ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
!
!       SHAPE PARAMETER FIVE:
!
        IF(NUMSHA.GE.5 .AND. SH5.NE.CPUMIN)THEN
          ITAG=ITAG+1
!
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
!
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
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
          NUMROW=ICNT
          DO 8318 II=1,NUMROW
            NTOT(II)=15
 8318     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Shape Parameter 5'
            NCPAR=17
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,   &
                        ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,   &
                        ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
!
!       VALUE OF TEST STATISTIC FOR PPCC, AD, KS
!
        IF(ICASP2.EQ.'PPCC' .OR. ICASP2.EQ.'AD' .OR.   &
           ICASP2.EQ.'KS')THEN
          ITAG=ITAG+1
!
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
!
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
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
          NUMROW=ICNT
          DO 8319 II=1,NUMROW
            NTOT(II)=15
 8319     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IF(ALOWPA(1,ITAG).NE.CPUMIN)THEN
            IPAR='Value of Test Statistic'
            NCPAR=23
            CALL DPDT8C(ALOWPA(1,ITAG),AUPPPA(1,ITAG),ALPHAV,NUMALP,   &
                        ICAPSW,ICAPTY,NUMDIG,IPAR,NCPAR,   &
                        ISUBRO,IBUGG3,IERROR)
          ENDIF
        ENDIF
!
!       IF PERCENTILES REQUESTED, GENERATE CONFIDENCE LIMITS FOR
!       PERCENTILES
!
        IF(NPERC.GE.1 .AND. IBOODP.NE.'OFF')THEN
          NPERCT=MIN(30,NPERC)
          IFLAG1=0
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=1
          IOP='OPEN'
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGG3,ISUBRO,IERROR)
!
!         NOTE: SOMETIMES ESTIMATION METHODS CAN FAIL.  SO SKIP
!               OVER FAILED READS.
!
          DO 7010 L=1,NPERCT
!
            REWIND(IOUNI5)
            ICNT=0
            DO 7020 II=1,NRESAM
              READ(IOUNI5,'(30E15.7)',ERR=7020)(XTEMP2(JJ),JJ=1,NPERCT)
              IF(XTEMP2(L).LE.CPUMIN+100.0)GO TO 7020
              ICNT=ICNT+1
              XTEMP1(ICNT)=XTEMP2(L)
 7020       CONTINUE
            CALL MEDIAN(XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,XQPMED,   &
                        IBUGG3,IERROR)
            XQP(L)=XQPMED
            IF(IBOODP.EQ.'TWOS')THEN
              P100=100.0*(ALPHA/2.0)
              CALL PERCEN(P100,XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,   &
                          BTEMP,IBUGG3,IERROR)
              XQPLCL(L)=BTEMP
              P100=100.0*(1.0 - (ALPHA/2.0))
              CALL PERCEN(P100,XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,   &
                          BTEMP,IBUGG3,IERROR)
              XQPUCL(L)=BTEMP
            ELSEIF(IBOODP.EQ.'LOWE')THEN
              P100=100.0*(ALPHA)
              CALL PERCEN(P100,XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,   &
                          BTEMP,IBUGG3,IERROR)
              XQPLCL(L)=BTEMP
              XQPUCL(L)=CPUMIN
            ELSEIF(IBOODP.EQ.'UPPE')THEN
              P100=100.0*(1.0 - ALPHA)
              CALL PERCEN(P100,XTEMP1,ICNT,IWRITE,XTEMP3,MAXNXT,   &
                          BTEMP,IBUGG3,IERROR)
              XQPUCL(L)=BTEMP
              XQPLCL(L)=CPUMIN
          ENDIF
!
 7010     CONTINUE
!
          CALL DPDT9B(QP,XQP,XQPLCL,XQPUCL,NPERC,   &
                      ICAPSW,ICAPTY,NUMDIG,ALPHA,   &
                      ISUBRO,IBUGG3,IERROR)
!
          IFLAG5=1
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGG3,ISUBRO,IERROR)
        ENDIF
!
11000 CONTINUE
!
      NPLOTP=J
      NPLOTV=3
!
      IOP='CLOS'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IF(IBCABT.EQ.'ON' .AND. ICASJB.EQ.'BOOT')IFLAG3=1
      IFLAG4=0
      IF(NPERC.GT.0)IFLAG4=1
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IFEEDB.EQ.'ON')THEN
!
          WRITE(ICOUT,8102)
 8102     FORMAT('THE FOLLOWING INFORMATION IS WRITTEN TO FILES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8104)
 8104     FORMAT('DPST1F.DAT: THE BOOTSTRAP VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8106)
 8106     FORMAT('            FOR GROUPED DATA, THE FIRST ONE (OR ',   &
                 'TWO) COLUMNS IDENTIFY THE GROUP(S).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8112)
 8112     FORMAT('DPST2F.DAT: STATISTICS BASED ON BOOTSTRAP VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8114)
 8114     FORMAT('            MEAN, SD, MEDIAN, B025, ',   &
                 'B975, B05, B95, B005, B995')
          CALL DPWRST('XXX','BUG ')
          IF(NUMPAR.GT.1)THEN
            WRITE(ICOUT,8118)
 8118       FORMAT('            THE FIRST COLUMN IDENTIFIES THE ',   &
                   'PARAMETER.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,8116)
 8116     FORMAT('            FOR GROUPED DATA, THE FIRST ONE (OR ',   &
                 'TWO) COLUMNS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8117)
 8117     FORMAT('            (AFTER THE PARAMETER ID) IDENTIFY ',   &
                 'THE GROUP(S).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
!
        ENDIF
!
!CCCC ENDIF
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
!
!     REMOVE FAILED SAMPLES FROM PLOT
!
      ICOUNT=0
      DO 9005 I=1,NPLOTP
        IF(Y2(I).NE.CPUMIN)THEN
          ICOUNT=ICOUNT+1
          Y2(ICOUNT)=Y2(I)
          X2(ICOUNT)=X2(I)
          D2(ICOUNT)=D2(I)
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS7')THEN
            WRITE(ICOUT,9008)I,ICOUNT,Y2(ICOUNT),X2(ICOUNT),D2(ICOUNT)
 9008       FORMAT('I,ICOUNT,Y2(ICOUNT),X2(ICOUNT),D2(ICOUNT) = ',   &
                   2I8,2G15.7,F9.2)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
 9005 CONTINUE
      NPLOTP=ICOUNT
!
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
        DO 9020 I=1,NPLOTP
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPJBS7
      SUBROUTINE DPJBS8(ISETMX,ISET,NUMSET,NUMSE2,N1,N2,NGRP,   &
                        MAXOBV,MAXGRP,NUMV2,   &
                        Y,Z,TEMPL,XDESGN,XIDTEM,TEMP0,TEMPZ0,TEMPZL,   &
                        NS2,NSS2,NI,NI2,ISET1,ISET2,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--UTILITY ROUTINE FOR DPJBS6 AND DPJBS7.  FOR A
!              GIVEN REPLICATION, EXTRACT THE APPROPRIATE DATA.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/02
!     ORIGINAL VERSION--FEBRUARY  2010. EXTRACTED FROM DPJBS2
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION TEMPL(*)
      DIMENSION TEMP0(*)
      DIMENSION TEMPZ0(*)
      DIMENSION TEMPZL(*)
      DIMENSION XDESGN(MAXOBV,MAXGRP)
      DIMENSION XIDTEM(MAXOBV,MAXGRP)
!
      INTEGER NUMSE2(*)
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='JBS8'
      ISUBN2='    '
      IERROR='NO'
!
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
!
!               ******************************************
!               **  STEP 11--                           **
!               **  EXTRACT THE APPROPRIATE DATA        **
!               ******************************************
!
      ISTEPN='11'
      IF(IBUGG3.GE.'ON'.OR.ISUBRO.EQ.'JBS8')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ISET.EQ.ISETMX)THEN
        DO 1002 I=1,N1
          TEMP0(I)=Y(I)
 1002   CONTINUE
        IF(NUMV2.GE.2)THEN
          DO 1004 I=1,N2
            TEMPZ0(I)=Z(I)
 1004     CONTINUE
        ENDIF
        IF(NUMV2.GE.3)THEN
          DO 1006 I=1,N2
            TEMPZL(I)=TEMPL(I)
 1006     CONTINUE
        ENDIF
        NS2=N1
        NSS2=N2
        NI=N1
        NI2=N2
        ISET1=0
        ISET2=0
!
!       NOTE: FOR GROUPED CASE, BOTH RESPONSE VARIABLES
!             MUST HAVE THE SAME LENGTH.
!
      ELSEIF(NGRP.EQ.1 .AND. ISET.LT.ISETMX)THEN
        K=0
        DO 1102 I=1,N1
          IF(XDESGN(I,1).NE.XIDTEM(ISET,1))GO TO 1102
          K=K+1
          TEMP0(K)=Y(I)
          IF(NUMV2.GE.2)TEMPZ0(K)=Z(I)
          IF(NUMV2.GE.3)TEMPZL(K)=TEMPL(I)
!
          IF(IBUGG3.GE.'ON'.OR.ISUBRO.EQ.'JBS8')THEN
            WRITE(ICOUT,1108)I,K,XDESGN(I,1),TEMP0(K),Y(I)
 1108       FORMAT('AT 1102: I,K,XDESGN(I,1),TEMP0(K),Y(I) = ',   &
                   2I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
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
        DO 1202 I=1,N1
          IF(XDESGN(I,1).NE.XIDTEM(ISET1,1) .OR.   &
             XDESGN(I,2).NE.XIDTEM(ISET2,2))GO TO 1202
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
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9020 I=1,MAX(NS2,NSS2)
          WRITE(ICOUT,9021)I,TEMP0(I),TEMPZ0(I),TEMPZL(I)
 9021     FORMAT('I,TEMP0(I),TEMPZ0(I),TEMPZL(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPJBS8
      SUBROUTINE DPJBS9(Y2,D2,TEMP,XTEMP1,XTEMP2,MAXNXT,IOUNI1,IOUNI2,   &
                        NUMPAR,NGRPV,NUMSET,ISET,ISET1,ISET2,NUMSE1,J,   &
                        APERC,BPERC,NPERC,   &
                        BMEAN,BSD,BMIN,BMAX,BMAD,   &
                        B001,B005,B01,B025,B05,B10,B20,B50,   &
                        B80,B90,B95,B975,B99,B995,B999,   &
                        ALOWPA,AUPPPA,ALPHA,NUMALP,   &
                        ZMEAN,ZMED,ZSD,ZMAD,NFAIL,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--UTILITY ROUTINE FOR BOOTSTRAP/JACKNIFE PLOT.  THIS
!              ROUTINE WRITES INFORMATION TO DPST1F.DAT AND DPST2F.DAT
!              AND COMPUTES CERTAIN PARAMETERS (E.G., BMEAN, B10).
!
!              NOTE: THIS ROUTINE EXTRACTED FROM ORIGINAL DPJBS2.
!                    WITH THIS EXTRACTION, TAKE THE OPPORTUNITY TO
!                    SIMPLIFY THE CODE A BIT AS WELL.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/03
!     ORIGINAL VERSION--MARCH     2010. EXTRACTED FROM DPJBS2
!     UPDATED         --OCTOBER   2010. SAVE MEAN, MEDIAN, SD, AND MAD
!                                       FOR EACH PARAMETER FOR PRINT
!                                       ROUTINES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
!
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
!
      INTEGER NUMSE1(*)
!
      CHARACTER*4 IWRITE
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IWRITE='OFF'
!
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
        DO 73 I=1,J
          WRITE(ICOUT,74)I,Y2(I),D2(I)
   74     FORMAT('I,Y2(I),D2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
!
!               ************************************************
!               **   STEP 19--                                **
!               **   FOR GROUPED DATA, WRITE GROUP-ID, MEAN,  **
!               **   MEDIAN, B025, B975, B05, B90, B005, B995 **
!               **   TO DPST1F.DAT.                           **
!               ************************************************
!
!       CASE 1: NO GROUPS OR DATA AGGREGATED OVER ALL GROUPS FOR
!               GROUPED CASE.
!
        IF(NGRPV.LE.0 .OR. (NGRPV.GE.1.AND.ISET.GT.NUMSET))THEN
!
!CCCC     NLAST=J*NUMPAR
          DO 110 I=1,J,NUMPAR
            NSTOP=I+NUMPAR-1
            WRITE(IOUNI1,112)(Y2(K),K=I,NSTOP)
  112       FORMAT(8E15.7)
  110     CONTINUE
!
!         2014/04: SET NFAIL TO NUMBER OF SUCCESSFUL BOOTSTRAP.
!                  IN CALLING ROUTINE, WILL SUBTRACT THIS FROM
!                  NUMBER OF BOOTSTRAP SAMPLES TO OBTAIN THE NUMBER
!                  OF FAILURES.
!
          DO 115 IPAR=1,NUMPAR
            ICOUNT=0
            NFAIL(IPAR)=0
            DO 116 I=1,J,NUMPAR
              ATEMP=Y2(I+IPAR-1)
              IF(ATEMP.NE.CPUMIN)THEN
                ICOUNT=ICOUNT+1
                TEMP(ICOUNT)=Y2(I+IPAR-1)
                NFAIL(IPAR)=NFAIL(IPAR)+1
              ELSE
!CCCC           NFAIL(IPAR)=NFAIL(IPAR)+1
              ENDIF
  116       CONTINUE
!
            IF(IBUGG3.GE.'ON'.OR.ISUBRO.EQ.'JBS9')THEN
              WRITE(ICOUT,3118)I,ICOUNT,NFAIL(IPAR)
 3118         FORMAT('I,ICOUNT,NFAIL(IPAR)=',3I8)
              CALL DPWRST('XXX','BUG ')
              DO 3119 II=1,ICOUNT
                WRITE(ICOUT,3120)II,TEMP(ICOUNT)
 3120           FORMAT('II,TEMP(ICOUNT) = ',I8,G15.7)
              CALL DPWRST('XXX','BUG ')
 3119         CONTINUE
            ENDIF
!
!           NOTE OCTOBER 2010: FOR SOME DISTRIBUTIONS, PARAMETER
!           ESTIMATION CAN FAIL.  TO AVOID FAILURE IN GENERATING
!           PLOT/STATISTICS, KEEP TRACK OF NUMBER OF FAILURES AND
!           OMIT THESE FROM PLOT OUTPUT.
!
            CALL SORT(TEMP,ICOUNT,TEMP)
            BMIN=TEMP(1)
            BMAX=TEMP(ICOUNT)
            CALL MEAN(TEMP,ICOUNT,IWRITE,BMEANZ,IBUGG3,IERROR)
            CALL SD(TEMP,ICOUNT,IWRITE,BSDZ,IBUGG3,IERROR)
            CALL MEDIAN(TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,B50Z,   &
                        IBUGG3,IERROR)
            CALL MAD(TEMP,ICOUNT,IWRITE,XTEMP1,XTEMP2,MAXNXT,BMAD,   &
                        IBUGG3,IERROR)
            ZMEAN(IPAR)=BMEANZ
            ZMED(IPAR)=B50Z
            ZSD(IPAR)=BSDZ
            ZMAD(IPAR)=BMAD
!
!           COMPUTE SELECT PERCENTILES
!
!CCCC       IF(NUMPAR.EQ.1)THEN
              DO 117 L=1,NPERC
                ATEMP=APERC(L)
                CALL PERCEN(ATEMP,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                            BTEMP,IBUGG3,IERROR)
                BPERC(L)=BTEMP
  117         CONTINUE
!CCCC       ENDIF
!
!           COMPUTE SELECT CONFIDENCE INTERVALS
!
            DO 1118 L=1,NUMALP
              ALP=ALPHA(L)
              ATEMP1=100.0*(ALP/2.0)
              CALL PERCEN(ATEMP1,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                          BTEMP1,IBUGG3,IERROR)
              ALOWPA(L,IPAR)=BTEMP1
              ATEMP2=100.0*(1.0 - (ALP/2.0))
              CALL PERCEN(ATEMP2,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                          BTEMP2,IBUGG3,IERROR)
              AUPPPA(L,IPAR)=BTEMP2
 1118       CONTINUE
!
            IF(NUMPAR.EQ.1)THEN
              WRITE(IOUNI2,119)BMEANZ,BSDZ,B50Z,   &
                               (BPERC(LL),LL=1,15)
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
              WRITE(IOUNI2,118)IPAR,BMEANZ,BSDZ,B50Z,   &
                               (BPERC(LL),LL=1,15)
  118         FORMAT(I8,18E15.7)
            ENDIF
  115     CONTINUE
!
!       CASE 2: ONE GROUP CASE
!
        ELSEIF(NGRPV.EQ.1 .AND. ISET.LE.NUMSET)THEN
!
          DO 120 I=1,J,NUMPAR
            NSTOP=I+NUMPAR-1
            IF(INT(D2(I)+0.01).EQ.ISET)THEN
              WRITE(IOUNI1,122)ISET,(Y2(K),K=I,NSTOP)
  122         FORMAT(I8,8E15.7)
            ENDIF
  120     CONTINUE
!
          DO 125 IPAR=1,NUMPAR
            ICOUNT=0
            DO 126 I=1,J,NUMPAR
              IF(INT(D2(I)+0.01).EQ.ISET)THEN
                ICOUNT=ICOUNT+1
                TEMP(ICOUNT)=Y2(I+IPAR-1)
              ENDIF
  126       CONTINUE
!
            CALL SORT(TEMP,ICOUNT,TEMP)
            BMIN=TEMP(1)
            BMAX=TEMP(ICOUNT)
            CALL MEAN(TEMP,ICOUNT,IWRITE,BMEAN,IBUGG3,IERROR)
            CALL SD(TEMP,ICOUNT,IWRITE,BSD,IBUGG3,IERROR)
            CALL MEDIAN(TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,B50,   &
                        IBUGG3,IERROR)
            CALL MAD(TEMP,ICOUNT,IWRITE,XTEMP1,XTEMP2,MAXNXT,BMAD,   &
                     IBUGG3,IERROR)
!
!           COMPUTE SELECT PERCENTILES
!
            DO 127 L=1,NPERC
              ATEMP=APERC(L)
              CALL PERCEN(ATEMP,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                          BTEMP,IBUGG3,IERROR)
              BPERC(L)=BTEMP
  127       CONTINUE
!
!           COMPUTE SELECT CONFIDENCE INTERVALS
!
            DO 1128 L=1,NUMALP
              ALP=ALPHA(L)
              ATEMP1=100.0*(ALP/2.0)
              CALL PERCEN(ATEMP1,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                          BTEMP1,IBUGG3,IERROR)
              ALOWPA(L,IPAR)=BTEMP1
              ATEMP2=100.0*(1.0 - (ALP/2.0))
              CALL PERCEN(ATEMP2,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                          BTEMP2,IBUGG3,IERROR)
              AUPPPA(L,IPAR)=BTEMP2
 1128       CONTINUE
!
            IF(NUMPAR.EQ.1)THEN
              WRITE(IOUNI2,129)ISET,BMEAN,BSD,B50,   &
                               (BPERC(LL),LL=1,15)
  129         FORMAT(I8,18E15.7)
            ELSE
              WRITE(IOUNI2,128)IPAR,ISET,BMEAN,BSD,B50,   &
                               (BPERC(LL),LL=1,15)
  128         FORMAT(2I8,18E15.7)
            ENDIF
  125     CONTINUE
!
!       CASE 3: TWO GROUPS CASE
!
        ELSEIF(NGRPV.EQ.2 .AND.ISET.LE.NUMSET)THEN
!
          ITAG=(ISET1-1)*NUMSE1(2) + ISET2
          DO 130 I=1,J,NUMPAR
            NSTOP=I+NUMPAR-1
            IF(INT(D2(I)+0.01).EQ.ITAG)THEN
              WRITE(IOUNI1,132)ISET1,ISET2,(Y2(K),K=I,NSTOP)
  132         FORMAT(2I8,8E15.7)
            ENDIF
  130     CONTINUE
!
          DO 135 IPAR=1,NUMPAR
            ICOUNT=0
            DO 136 I=1,J,NUMPAR
              IF(INT(D2(I)+0.01).EQ.ITAG)THEN
                ICOUNT=ICOUNT+1
                TEMP(ICOUNT)=Y2(I+IPAR-1)
              ENDIF
  136       CONTINUE
!
            CALL SORT(TEMP,ICOUNT,TEMP)
            BMIN=TEMP(1)
            BMAX=TEMP(ICOUNT)
            CALL MEAN(TEMP,ICOUNT,IWRITE,BMEAN,IBUGG3,IERROR)
            CALL SD(TEMP,ICOUNT,IWRITE,BSD,IBUGG3,IERROR)
            CALL MEDIAN(TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,B50,   &
                        IBUGG3,IERROR)
            CALL MAD(TEMP,ICOUNT,IWRITE,XTEMP1,XTEMP2,MAXNXT,BMAD,   &
                     IBUGG3,IERROR)
!
!           COMPUTE SELECT PERCENTILES
!
            DO 137 L=1,NPERC
              ATEMP=APERC(L)
              CALL PERCEN(APERC(L),TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                          BTEMP,IBUGG3,IERROR)
              BPERC(L)=BTEMP
  137       CONTINUE
!
!           COMPUTE SELECT CONFIDENCE INTERVALS
!
            DO 1130 II=1,NUMALP
              ALP=ALPHA(L)
              ATEMP1=100.0*(ALP/2.0)
              CALL PERCEN(ATEMP1,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                          BTEMP1,IBUGG3,IERROR)
              ALOWPA(L,IPAR)=BTEMP1
              ATEMP2=100.0*(1.0 - (ALP/2.0))
              CALL PERCEN(ATEMP2,TEMP,ICOUNT,IWRITE,XTEMP1,MAXNXT,   &
                          BTEMP2,IBUGG3,IERROR)
              AUPPPA(L,IPAR)=BTEMP2
 1130       CONTINUE
!
            IF(NUMPAR.EQ.1)THEN
              WRITE(IOUNI2,139)ISET1,ISET2,BMEAN,BSD,B50,   &
                               (BPERC(LL),LL=1,15)
  139         FORMAT(2I8,18E15.7)
            ELSE
              WRITE(IOUNI2,138)IPAR,ISET1,ISET2,BMEAN,BSD,B50,   &
                               (BPERC(LL),LL=1,15)
  138         FORMAT(3I8,18E15.7)
            ENDIF
  135     CONTINUE
        ENDIF
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'JBS9')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJBS9--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPJBS9
      SUBROUTINE DPJTTE(MAXNXT,ICAPSW,IFORSW,ISEED,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A TJONCKHEERE-TERPSTRA TEST
!     EXAMPLE--JONCKHEERE TERPSTRA TEST Y X
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2024/01
!     ORIGINAL VERSION--MARCH     2024.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASAN
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASE
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
!
      DIMENSION TAG1(2*MAXOBV)
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION YSAVE(MAXOBV)
      DIMENSION XDIST(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION SSIZES(MAXOBV)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB5),YSAVE(1))
      EQUIVALENCE (GARBAG(IGARB6),TAG1(1))
      EQUIVALENCE (GARBAG(IGARB7),Y1(1))
      EQUIVALENCE (GARBAG(IGARB8),Y2(1))
      EQUIVALENCE (GARBAG(IGARB9),XDIST(1))
      EQUIVALENCE (GARBAG(IGAR10),SSIZES(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPJT'
      ISUBN2='ES  '
      IFOUND='NO'
      IERROR='NO'
!
      MAXNXT=MAXOBV
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               **************************************************
!               **  TREAT THE JONCKHEERE_TERPSTRA TEST CASE     **
!               **************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'JTTE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPJTTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'JTTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTZ=9999
      ICASAN='JTTE'
!
      ISTRT=0
      IF(ICOM.EQ.'JONC' .AND. IHARG(1).EQ.'TERP' .AND.   &
         IHARG(2).EQ.'TEST')THEN
        ILASTZ=2
        IFOUND='YES'
      ELSEIF(ICOM.EQ.'JONC' .AND. IHARG(1).EQ.'TERP')THEN
        ILASTZ=1
        IFOUND='YES'
      ENDIF
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      DO 100 I=1,NUMARG-2
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
        ENDIF
  100 CONTINUE
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'JTTE')THEN
        WRITE(ICOUT,91)ICASAN,ISHIFT
   91   FORMAT('DPJTTE: ICASAN,ISHIFT = ',A4,2X,I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'JTTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='JONCKHEERE-TERPSTRA TEST'
      MINNA=2
      MAXNA=100
      MINN2=4
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=2
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'JTTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'JTTE')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVA2=2
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,Y2,Y1,N,NLOCA2,NLOCA3,ICASE,   &
                  IBUGA3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ****************************************
!               **  STEP 62--                         **
!               **  PERFORM JONCKHEERE-TERPSTRA TEST  **
!               ****************************************
!
      ISTEPN='62'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'JTTE')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6211)
 6211   FORMAT('***** FROM DPJTTE, BEFORE CALL DPJTT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6212)N,MAXN
 6212   FORMAT('N,MAXN = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 6215 II=1,N
          WRITE(ICOUT,6216)II,Y1(II),Y2(II)
 6216     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 6215   CONTINUE
      ENDIF
!
      IVARID=IVARN1(1)
      IVARI2=IVARN2(1)
      IVARI3=IVARN1(2)
      IVARI4=IVARN2(2)
      CALL DPJTT2(Y1,Y2,N,YSAVE,XDIST,   &
                  TAG1,TEMP1,TEMP2,TEMP3,TEMP4,SSIZES,MAXOBV,   &
                  ICAPSW,ICAPTY,IFORSW,ISEED,   &
                  IPTESS,PPTEVA,   &
                  IVARID,IVARI2,IVARI3,IVARI4,   &
                  STATVA,STATCD,PVALUT,   &
                  PMEAN,PMED,PSD,P001,P005,P01,P025,P05,P10,P20,P50,   &
                  P80,P90,P95,P975,P99,P995,P999,   &
                  IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8D--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='8D'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FTE2')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(STATVA.NE.CPUMIN)THEN
        IH='STAT'
        IH2='VAL '
        VALUE0=STATVA
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(STATCD.NE.CPUMIN)THEN
        IH='STAT'
        IH2='CDF '
        VALUE0=STATCD
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(PVALUT.NE.CPUMIN)THEN
        IH='PVAL'
        IH2='UE  '
        VALUE0=PVALUT
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P001.NE.CPUMIN)THEN
        IH='P001'
        IH2='    '
        VALUE0=P001
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P005.NE.CPUMIN)THEN
        IH='P005'
        IH2='    '
        VALUE0=P005
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P01.NE.CPUMIN)THEN
        IH='P01'
        IH2='    '
        VALUE0=P01
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P025.NE.CPUMIN)THEN
        IH='P025'
        IH2='    '
        VALUE0=P025
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P05.NE.CPUMIN)THEN
        IH='P05'
        IH2='    '
        VALUE0=P05
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P10.NE.CPUMIN)THEN
        IH='P10'
        IH2='    '
        VALUE0=P10
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P20.NE.CPUMIN)THEN
        IH='P20'
        IH2='    '
        VALUE0=P20
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P50.NE.CPUMIN)THEN
        IH='P50'
        IH2='    '
        VALUE0=P50
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P80.NE.CPUMIN)THEN
        IH='P80'
        IH2='    '
        VALUE0=P80
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P90.NE.CPUMIN)THEN
        IH='P90'
        IH2='    '
        VALUE0=P90
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P95.NE.CPUMIN)THEN
        IH='P95'
        IH2='    '
        VALUE0=P95
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P975.NE.CPUMIN)THEN
        IH='P975'
        IH2='    '
        VALUE0=P975
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P99.NE.CPUMIN)THEN
        IH='P99'
        IH2='    '
        VALUE0=P99
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P995.NE.CPUMIN)THEN
        IH='P995'
        IH2='    '
        VALUE0=P995
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P999.NE.CPUMIN)THEN
        IH='P999'
        IH2='    '
        VALUE0=P999
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'JTTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJTTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR= ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPJTTE
      SUBROUTINE DPJTT2(Y,X,N,YSAVE,XDIST,   &
                        TAG1,TEMP1,TEMP2,XTEMP1,XTEMP2,SSIZES,MAXNXT,   &
                        ICAPSW,ICAPTY,IFORSW,ISEED,   &
                        IPTESS,PPTEVA,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        STATVA,STATCD,PVALUT,   &
                        PMEAN,PMED,PSD,P001,P005,P01,P025,P05,P10,P20,   &
                        P50,P80,P90,P95,P975,P99,P995,P999,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT JONCKHEERE-TERPSTRA (JT) TEST
!              NON-PARAMETRIC ONE-WAY ANOVA FOR ORDERED ALTERNATIVES.
!
!               DATAPLOT USES THE FORMULATION GIVEN ON PAGE 101 OF
!               THE HIGGINS TEXT.
!     EXAMPLE--JONCKHEERE TERPSTRA TEST Y TAG
!     DESCRIPTION--THE STEPS FOR THE JT TEST ARE:
!
!                  1. GIVEN K SAMPLES, THE HYPOTHESIS BEING TESTED IS
!
!                     H0: F(1)(X) = F(2)(x) = ... = F(k)(X)
!                     Ha: F(1)(X) >= F(2)(X) >= ... >= F(k)(X)
!
!                  2. COMPUTE THE TEST STATISTIC
!
!                     T = SUM[i < j][T(ij)]
!
!                     WHERE T(ij) IS A TEST FOR
!
!                     H0: F(i)(X) = F(j)(X)
!                     Ha: F(i)(X) >= F(j)(X)
!
!                     WE FOLLOW HIGGINS IN USING THE MANN-WHITNEY
!                     UPPER-TAILED TEST.  ALTERNATIVELY, A WILCOXON
!                     RANK SUM TEST COULD BE USED.
!
!                  3. COMPUTE JT(obs), THE OBSERVED VALUE OF T FROM
!                     THE ORIGINAL DATA.
!
!                  4. RANDOMLY GENERATE A PERMUTATION OF THE ORIGINAL
!                     DATA AND COMPUTE THE T STATISTIC FROM THE
!                     PERMUTATION.
!
!                  5. REPEAT STEP 4 NITER TIMES.  THIS PROVIDES THE
!                     REFERENCE DISTRIBUTION FOR THE JT TEST.
!
!                  NOTE THAT IT IS ASSUMED THAT THE X VARIABLE IS
!                  CODED FROM LOWEST TO HIGHEST EXPECTED RESPONSE.
!
!     REFERENCE--JONCKHEERE (1954), "Distribution-Free k-Sample Test
!                Against Ordered Alternatives", Biometrika,
!                VOL. 41, NO. 1/2, PP. 133-145.
!              --HIGGINS (2004), "INTRODUCTION TO MODERN NONPARAMETRIC
!                STATISTICS", DUXBURY PRESS, CHAPTER 3, P. 101.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023/09
!     ORIGINAL VERSION--SEPTEMBER 2023.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*4 IDIR
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION YSAVE(*)
      DIMENSION XDIST(*)
      DIMENSION TAG1(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION SSIZES(*)
!
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=3)
      PARAMETER(NUMALP=4)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=30)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPJT'
      ISUBN2='T2  '
      IERROR='NO'
!
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVALUT=CPUMIN
      BMEAN=CPUMIN
      PSD=CPUMIN
      P001=CPUMIN
      P005=CPUMIN
      P01=CPUMIN
      P025=CPUMIN
      P05=CPUMIN
      P10=CPUMIN
      P20=CPUMIN
      P50=CPUMIN
      P80=CPUMIN
      P90=CPUMIN
      P95=CPUMIN
      P975=CPUMIN
      P99=CPUMIN
      P995=CPUMIN
      P999=CPUMIN
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'JTT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPJTT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT,ISEED
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT,ISEED = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)ICASAN,IPTESS,PPTEVA
   53   FORMAT('ICASAN,IPTESS,PPTEVA = ',A4,2X,I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),X(I)
   57     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LE.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN JONCKHEERE-TERPSTRA TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE 4 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS   = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************
!               **  STEP 2--                              **
!               **  COMPUTE STATISTIC FOR ORIGINAL DATA   **
!               ********************************************
!
!     NOTE: COMPUTE THE
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     CODE X VARIABLE
!
      CALL CODE(X,N,IWRITE,TAG1,XTEMP1,MAXNXT,IBUGA3,IERROR)
      DO 205 II=1,N
        X(II)=TAG1(II)
  205 CONTINUE
      IF(IERROR.EQ.'YES')GO TO 9000
      CALL SORTC(X,Y,N,X,TEMP1)
      DO 210 II=1,N
        Y(II)=TEMP1(II)
  210 CONTINUE
!
      CALL DISTIN(X,N,IWRITE,XDIST,NDIST,IBUGA3,IERROR)
!
!     COMPUTE THE JONCKHEERE-TERPSTRA STATISTIC
!
      SUM1=0.0
      SUM2=0.0
      DO 220 II=1,NDIST-1
        HOLD1=XDIST(II)
        NII=0
        DO 222 KK=1,N
          IF(X(KK).EQ.HOLD1)THEN
            NII=NII+1
            TEMP1(NII)=Y(KK)
          ENDIF
  222   CONTINUE
        IF(NII.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,224)II
  224     FORMAT('      GROUP ',I5,' HAS NO OBSERVATIONS.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        SSIZES(II)=REAL(NII)
!
        DO 230 JJ=II+1,NDIST
          HOLD2=XDIST(JJ)
          NJJ=0
          DO 235 KK=1,N
            IF(X(KK).EQ.HOLD2)THEN
              NJJ=NJJ+1
              TEMP2(NJJ)=Y(KK)
            ENDIF
  235     CONTINUE
          IF(NJJ.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,224)JJ
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          IF(JJ.EQ.NDIST)THEN
            SSIZES(NDIST)=REAL(JJ)
          ENDIF
!
          MIJ=NII*NJJ
          SUM2=SUM2 + REAL(MIJ)
          PIJ=0.0
          DO 226 KK=1,NII
            AVAL1=TEMP1(KK)
            DO 227 LL=1,NJJ
              AVAL2=TEMP2(LL)
              IF(AVAL1.LE.AVAL2)PIJ=PIJ+1.0
  227       CONTINUE
  226     CONTINUE
          SUM1=SUM1 + PIJ
!
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
            WRITE(ICOUT,228)II,JJ,NII,NJJ,HOLD1,HOLD2
  228       FORMAT('II,JJ,NII,NJJ,HOLD1,HOLD2 = ',4I6,2G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,229)MIJ,SUM2,PIJ,SUM1
  229       FORMAT('MIJ,SUM2,PIJ,SUM1 = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
  230   CONTINUE
  220 CONTINUE
!
      STATVA=2.0*SUM1 - SUM2
!
!     COMPUTE QUANTITIES NEEDED FOR T-APPROXIMATION OF
!     CRITICAL VALUES
!
!     NOT GETTING REASONABLE RESULTS WITH T APPROXIMATION,
!     IMPLEMENT NORMAL APPROXIMATION INSTEAD
!
!CCCC SUM1=0.0
!CCCC SUM2=0.0
      SUM3=0.0
      AN=REAL(N)
      DO 233 II=1,NDIST
        AVAL=SSIZES(II)
!CCCC   SUM1=SUM1 + (AVAL/AN)**2
!CCCC   SUM2=SUM2 + AVAL**3*(6.0*AVAL**2 + 15.0*AVAL + 10.0)
        SUM3=SUM3 + AVAL**2*(2.0*AVAL + 3.0)
  233 CONTINUE
!CCCC U2=1.0 - SUM1
!CCCC C1=-36.0/25.0
!CCCC C2=AN**3*(6.0*AN**2 + 15.0*AN + 10.0)
!CCCC ANUM=C1*(C2 - SUM2)
!CCCC C3=AN**2*(2.0*AN+3.0)
!CCCC ADENOM=(C3 - SUM3)**2
!CCCC GAMMA2=ANUM/ADENOM
!CCCC ANU=-3.0*(2.0+GAMMA2)/GAMMA2
!CCCC IFLAGT=1
!CCCC IF(ANU.LT.1.0)IFLAGT=0
!CCCC IF(IFLAGT.EQ.1)THEN
!CCCC   ADEN2=(ANU+1.0)*U2 - STATVA**2
!CCCC   IF(ADEN2.LE.0.0)THEN
!CCCC     IFLAGT=0
!CCCC     GO TO 234
!CCCC   ENDIF
!CCCC   STATTV=STATVA*SQRT(ANU/ADEN2)
!CCCC ENDIF
!C234 CONTINUE
!
      C1=1.0/18.0
      C2=AN**2*(2.0*AN+3.0)
      ADENOM=C1*(C2 - SUM3)
      IFLAGT=1
      IF(ADENOM.LE.0.0)IFLAGT=0
      IF(IFLAGT.EQ.1)THEN
        STATTV=STATVA/SQRT(ADENOM)
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
        WRITE(ICOUT,236)STATVA,C1,C2,ADENOM,IFLAGT
  236   FORMAT('STATVA,C1,C2,ADENOM,FLAGT = ',4G15.7,I6)
        CALL DPWRST('XXX','BUG ')
        IF(IFLAGT.EQ.1)THEN
          WRITE(ICOUT,237)STATTV
  237     FORMAT('STATTV = ',G15.7)
          CALL DPWRST('XXX','BUG ')
!CCCC   ELSE
!CCCC     WRITE(ICOUT,238)C1,C2,C3,ANUM,ADENOM,ADEN2
!C238     FORMAT('C1,C2,C3,ANUM,ADENON,ADEN2 = ',6G15.7)
!CCCC     CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
!               **********************************************
!               **   STEP 3--                               **
!               **   GENERATE THE RANDOM PERMUTATIONS AND   **
!               **   COMPUTE THE DESIRED STATISTIC FOR EACH **
!               **   RANDOM PERMUTATION.                    **
!               **********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     DETERMINE TOTAL SIZE (N*NUMBER OF PERMUTATION SAMPLES)
!
      NITER=IPTESS
      NITEMP=MAXNXT/N
      IF(NITEMP.GT.NITER)NITEMP=NITER
      IF(NITEMP.GT.1600)NITEMP=1600
      ICNT2=0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
        WRITE(ICOUT,301)IPTESS,NITER,NITEMP
  301   FORMAT('IPTESS,NITER,NITEMP = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      WRITE(IOUNI2,*) 'X             TAG              Y'
!
      ILOOP=0
  310 CONTINUE
!
!       GENERATE NITEMP RANDOM PERMUTATIONS
!
        ILOOP=ILOOP+1
        NKEEP=N
        PVAL=1.0
        IDIST=0
        CALL RANPE2(N,NKEEP,PVAL,NITEMP,IDIST,MAXNXT,ISEED,   &
                    TEMP1,TAG1,XTEMP1,NOUT,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
          WRITE(ICOUT,314)MAXNXT,NOUT
  314     FORMAT('AFTER RANPE2, MAXNXT,NOUT = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(ILOOP.GT.1)THEN
          IINC=(ILOOP-1)*NITEMP
          DO 315 KK=1,NOUT
            TAG1(KK)=REAL(IINC) + TAG1(KK)
  315     CONTINUE
        ENDIF
        DO 316 KK=1,NOUT
          IVAL=INT(TAG1(KK)+0.1)
          IF(IVAL.GT.NITER)GO TO 318
          YVAL=Y(INT(TEMP1(KK)+0.1))
          WRITE(IOUNI2,'(3E15.7)')TEMP1(KK),TAG1(KK),YVAL
  316   CONTINUE
  318   CONTINUE
!
        DO 320 J=1,NITEMP
          ICNT2=ICNT2+1
          IF(ICNT2.GT.NITER)THEN
            ICNT2=ICNT2-1
            GO TO 390
          ENDIF
!
!         COMPUTE STATISTIC FOR THE RANDOM PERMUTATIONS
!
          NSTRT1=(J-1)*N + 1
          NSTOP1=NSTRT1+N-1
!
!         EXTRACT THE PERMUTED VARIABLES
!
          ICNT3=0
          ICNT4=0
          DO 328 KK=NSTRT1,NSTOP1
            ICNT3=ICNT3+1
            TEMP2(ICNT3)=Y(INT(TEMP1(KK)+0.1))
  328     CONTINUE
!
          IF(ICNT3.NE.N)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,321)ICNT2
  321       FORMAT('      NUMBER OF VALUES IN PERMUTED SAMPLE ',I6,   &
                   ' IS NOT EQUAL TO SAMPLE SIZE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,323)N1
  323       FORMAT('      EXPECTED SAMPLE SIZE: ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,325)ICNT3
  325       FORMAT('      RETURNED SAMPLE SIZE: ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
!         COMPUTE THE JONCKHEERE-TERPSTRA STATISTIC
!
          YSAVE(ICNT2)=0.0
          SUM1=0.0
          SUM2=0.0
          DO 340 II=1,NDIST-1
            HOLD1=XDIST(II)
            NII=0
            DO 345 KK=1,N
              IF(X(KK).EQ.HOLD1)THEN
                NII=NII+1
                XTEMP1(NII)=TEMP2(KK)
              ENDIF
  345       CONTINUE
!
            DO 350 JJ=II+1,NDIST
              HOLD2=XDIST(JJ)
              NJJ=0
              DO 355 KK=1,N
                IF(X(KK).EQ.HOLD2)THEN
                  NJJ=NJJ+1
                  XTEMP2(NJJ)=TEMP2(KK)
                ENDIF
  355         CONTINUE
!
              MIJ=NII*NJJ
              SUM2=SUM2 + REAL(MIJ)
              PIJ=0.0
              DO 356 KK=1,NII
                AVAL1=XTEMP1(KK)
                DO 357 LL=1,NJJ
                  AVAL2=XTEMP2(LL)
                  IF(AVAL1.LE.AVAL2)PIJ=PIJ+1.0
  357           CONTINUE
  356         CONTINUE
              SUM1=SUM1 + PIJ
!
              IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
                WRITE(ICOUT,228)II,JJ,NII,NJJ,HOLD1,HOLD2
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,229)MIJ,SUM2,PIJ,SUM1
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              YSAVE(ICNT2)=2.0*SUM1 - SUM2
!
              IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
                WRITE(ICOUT,358)YSAVE(ICNT2)
  358           FORMAT('STATVA = ',G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
                WRITE(ICOUT,351)II,JJ,STTVAT,YSAVE(ICNT2)
  351           FORMAT('II,JJ,STTVAT,YSAVE(ICNT2) = ',2I6,2G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
  350       CONTINUE
  340     CONTINUE
  320   CONTINUE
        IF(ICNT2.GE.NITER)GO TO 390
        GO TO 310
!
  390 CONTINUE
!
!               **************************************************
!               **  STEP 4--                                    **
!               **  WRITE OUT COMPUTED STATISTICS TO DPST1F.DAT **
!               **************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WRITE(IOUNI1,412)
  412 FORMAT('COMPUTED STATISTICS FROM JONCKHEERE-TERPSTRA TEST')
      DO 410 II=1,ICNT2
        WRITE(IOUNI1,'(E15.7)')YSAVE(II)
  410 CONTINUE
!
!
!               **************************************************
!               **  STEP 5--                                    **
!               **  DETERMINE SELECTED PERCENTILES              **
!               **************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,506)ICNT2
  506   FORMAT('ICNT2 = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL SORT(YSAVE,ICNT2,YSAVE)
      CALL MEAN(YSAVE,ICNT2,IWRITE,PMEAN,IBUGA3,IERROR)
      CALL MEDIAN(YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,PMED,IBUGA3,IERROR)
      CALL SD(YSAVE,ICNT2,IWRITE,PSD,IBUGA3,IERROR)
!
      IDIR='UPPE'
      CALL DPGOF8(YSAVE,ICNT2,STATVA,PVALUT,IDIR,IBUGA3,ISUBRO,IERROR)
      STATCD=1.0-PVALUT
!
      PPERC=0.1
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P001,IBUGA3,IERROR)
!
      PPERC=0.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P005,IBUGA3,IERROR)
!
      PPERC=1.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P01,IBUGA3,IERROR)
!
      PPERC=2.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P025,IBUGA3,IERROR)
!
      PPERC=5.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P05,IBUGA3,IERROR)
!
      PPERC=10.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P10,IBUGA3,IERROR)
!
      PPERC=20.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P20,IBUGA3,IERROR)
!
      PPERC=50.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P50,IBUGA3,IERROR)
!
      PPERC=80.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P80,IBUGA3,IERROR)
!
      PPERC=90.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P90,IBUGA3,IERROR)
!
      PPERC=95.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P95,IBUGA3,IERROR)
!
      PPERC=97.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P975,IBUGA3,IERROR)
!
      PPERC=99.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P99,IBUGA3,IERROR)
!
      PPERC=99.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P995,IBUGA3,IERROR)
!
      PPERC=99.9
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P999,IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,501)PMEAN,PMED,PSD,ICNT2
  501   FORMAT('PMEAN,PMED,PSD,ICNT2 = ',3G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,503)P001,P005,P01,P025
  503   FORMAT('P001,P005,P01,P025 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,505)P05,P10,P20,P50
  505   FORMAT('P05,P10,P20,P50 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)P80,P90,P95,P975
  507   FORMAT('P80,P90,P95,P975 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,509)P99,P995,P999
  509   FORMAT('P99,P995,P999 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **   STEP 6--                                        **
!               **   WRITE OUT EVERYTHING  FOR THE PERMUTATION TEST  **
!               *******************************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')   &
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
      ISTEPN='6A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Jonckheere-Terpstra Permutation Test'
      NCTITL=37
      ITITLZ='(Critical Values Determined from Permutation Test)'
      NCTITZ=51
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable:  '
      WRITE(ITEXT(ICNT)(21:25),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Group-ID Variable:  '
      WRITE(ITEXT(ICNT)(21:25),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: F1(x) = F2(x) = ... = Fk(x)'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: F1(x) >= F2(x) >= ... >= Fk(x)'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Permutation Samples:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=REAL(NITER)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Statistic Value:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test CDF Value:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test P-Value:'
      NCTEXT(ICNT)=13
      AVALUE(ICNT)=PVALUT
      IDIGIT(ICNT)=NUMDIG
!
      NUMROW=ICNT
      DO 5010 I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      ITITL9=' '
      NCTIT9=0
!
      DO 5030 J=1,NUMCLI
        DO 5040 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5040   CONTINUE
 5030 CONTINUE
!
      ITITL2(2,1)='Significance'
      NCTIT2(2,1)=12
      ITITL2(3,1)='Level'
      NCTIT2(3,1)=5
!
      ITITL2(2,2)='Test '
      NCTIT2(2,2)=4
      ITITL2(3,2)='Statistic'
      NCTIT2(3,2)=9
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Region (>=)'
      NCTIT2(3,3)=11
      ICNT=3
!
      ICNT=ICNT+1
      ITITL2(1,ICNT)='Null'
      NCTIT2(1,ICNT)=4
      ITITL2(2,ICNT)='Hypothesis'
      NCTIT2(2,ICNT)=10
      ITITL2(3,ICNT)='Conclusion'
      NCTIT2(3,ICNT)=10
!
      NMAX=0
      NUMCOL=4
      DO 5050 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.ICNT)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 5050 CONTINUE
!
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
!
      DO 5060 J=1,NUMALP
!
        IF(J.EQ.1)THEN
          IVALUE(J,1)='80.0%'
          NCVALU(J,1)=5
          AMAT(J,3)=P80
        ELSEIF(J.EQ.2)THEN
          IVALUE(J,1)='90.0%'
          NCVALU(J,1)=5
          AMAT(J,3)=P90
        ELSEIF(J.EQ.3)THEN
          IVALUE(J,1)='95.0%'
          NCVALU(J,1)=5
          AMAT(J,3)=P95
        ELSEIF(J.EQ.4)THEN
          IVALUE(J,1)='99.0%'
          NCVALU(J,1)=5
          AMAT(J,3)=P99
        ENDIF
        AMAT(J,2)=STATVA
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.LT.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
!
 5060 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(IFLAGT.EQ.0)GO TO 9000
!
      ISTEPN='6B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'JTT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Jonckheere-Terpstra Test'
      NCTITL=24
      ITITLZ='(Critical Values Determined from Normal Approximation)'
      NCTITZ=54
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable:  '
      WRITE(ITEXT(ICNT)(21:25),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Group-ID Variable:  '
      WRITE(ITEXT(ICNT)(21:25),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: F1(x) = F2(x) = ... = Fk(x)'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: F1(x) >= F2(x) >= ... >= Fk(x)'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Permutation Samples:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=REAL(NITER)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Statistic Value:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Adjusted Statistic Value:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=STATTV
      IDIGIT(ICNT)=NUMDIG
!
      CALL NORCDF(STATTV,STATC2)
      PVAL2=1.0-STATC2
      ICNT=ICNT+1
      ITEXT(ICNT)='Test CDF Value:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=STATC2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test P-Value:'
      NCTEXT(ICNT)=13
      AVALUE(ICNT)=PVAL2
      IDIGIT(ICNT)=NUMDIG
!
      NUMROW=ICNT
      DO 6010 I=1,NUMROW
        NTOT(I)=15
 6010 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      ITITL9=' '
      NCTIT9=0
!
      DO 6030 J=1,NUMCLI
        DO 6040 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 6040   CONTINUE
 6030 CONTINUE
!
      ITITL2(2,1)='Significance'
      NCTIT2(2,1)=12
      ITITL2(3,1)='Level'
      NCTIT2(3,1)=5
!
      ITITL2(2,2)='Adjusted Test'
      NCTIT2(2,2)=13
      ITITL2(3,2)='Statistic'
      NCTIT2(3,2)=9
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Region (>=)'
      NCTIT2(3,3)=11
      ICNT=3
!
      ICNT=ICNT+1
      ITITL2(1,ICNT)='Null'
      NCTIT2(1,ICNT)=4
      ITITL2(2,ICNT)='Hypothesis'
      NCTIT2(2,ICNT)=10
      ITITL2(3,ICNT)='Conclusion'
      NCTIT2(3,ICNT)=10
!
      NMAX=0
      NUMCOL=4
      DO 6050 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.ICNT)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 6050 CONTINUE
!
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
!
      DO 6060 J=1,NUMALP
!
        IF(J.EQ.1)THEN
          IVALUE(J,1)='80.0%'
          NCVALU(J,1)=5
          CALL NORPPF(0.80,AVAL)
          AMAT(J,3)=AVAL
        ELSEIF(J.EQ.2)THEN
          IVALUE(J,1)='90.0%'
          NCVALU(J,1)=5
          CALL NORPPF(0.90,AVAL)
          AMAT(J,3)=AVAL
        ELSEIF(J.EQ.3)THEN
          IVALUE(J,1)='95.0%'
          NCVALU(J,1)=5
          CALL NORPPF(0.95,AVAL)
          AMAT(J,3)=AVAL
        ELSEIF(J.EQ.4)THEN
          IVALUE(J,1)='99.0%'
          NCVALU(J,1)=5
          CALL NORPPF(0.99,AVAL)
          AMAT(J,3)=AVAL
        ENDIF
        AMAT(J,2)=STATTV
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATTV.LT.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
!
 6060 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
 9000 CONTINUE
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'JTT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPJTT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPJTT2
      SUBROUTINE DPJUST(ICOM,IHARG,NUMARG,   &
      IDEFJU,   &
      ITEXJU,   &
      IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE JUSTIFICATION TYPE FOR
!              TEXT SCRIPT
!              ON A PLOT.
!              THE JUSTIFICATION FOR THE TEXT WILL BE PLACED
!              IN THE CHARACTER VARIABLE ITEXJU.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDEFJU
!                     --IBUGD2
!     OUTPUT ARGUMENTS--ITEXJU
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFJU
      CHARACTER*4 ITEXJU
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPJUST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICOM,NUMARG,IDEFJU
   53 FORMAT('ICOM,NUMARG,IDEFJU = ',A4,2X,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               ************************************
!               **  TREAT THE JUSTIFICATION CASE  **
!               ************************************
!
      IF(ICOM.EQ.'JUST')GO TO 1120
      IF(ICOM.EQ.'LEFT')GO TO 1130
      IF(ICOM.EQ.'CENT')GO TO 1140
      IF(ICOM.EQ.'RIGH')GO TO 1150
!
 1120 CONTINUE
      IF(NUMARG.LE.0)GO TO 1161
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1161
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1161
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1161
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1165
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LEFT')GO TO 1161
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CENT')GO TO 1162
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'RIGH')GO TO 1163
      GO TO 1170
!
 1130 CONTINUE
      IF(NUMARG.LE.0)GO TO 9000
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1161
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1161
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1161
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1165
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'JUST')GO TO 1161
      GO TO 9000
!
 1140 CONTINUE
      IF(NUMARG.LE.0)GO TO 9000
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1162
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1162
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1162
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1165
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'JUST')GO TO 1162
      GO TO 9000
!
 1150 CONTINUE
      IF(NUMARG.LE.0)GO TO 9000
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1163
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1163
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1163
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1165
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'JUST')GO TO 1163
      GO TO 9000
!
 1161 CONTINUE
      ITEXJU='LEFT'
      GO TO 1180
!
 1162 CONTINUE
      ITEXJU='CENT'
      GO TO 1180
!
 1163 CONTINUE
      ITEXJU='RIGH'
      GO TO 1180
!
 1165 CONTINUE
      ITEXJU=IDEFJU
      GO TO 1180
!
 1170 CONTINUE
!CCCC IERROR='YES'
!CCCC WRITE(ICOUT,1171)
!1171 FORMAT('***** ERROR IN DPJUST--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1172)
!1172 FORMAT('      ILLEGAL ENTRY FOR JUSTIFICATION ',
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC1'COMMAND.')
!CCCC WRITE(ICOUT,1173)
!1173 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC1'PROPER FORM--')
!CCCC WRITE(ICOUT,1174)
!1174 FORMAT('      SUPPOSE THE THE ANALYST WISHES ')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1175)
!1175 FORMAT('      TO HAVE ALL LEGENDS CENTERED,')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1177)
!1177 FORMAT('      THEN ALLOWABLE FORMS ARE--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1178)
!1178 FORMAT('           JUSTIFICATION CENTER ')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1179)
!1179 FORMAT('           CENTER JUSTIFICATION ')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC GO TO 9000
      ITEXJU=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE JUSTIFICATION (FOR PLOT SCRIPT AND TEXT) ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)ITEXJU
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 9000
!
!               ********************************************
!               **  STEP 81--                             **
!               **  TREAT THE    ?    CASE--              **
!               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
!               ********************************************
!
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
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPJUST
      SUBROUTINE DPKAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN KAPLAN-MEIER PLOT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/5
!     ORIGINAL VERSION--MAY       1998.
!     UPDATED         --JULY      2005. SUPPORT SWITCH FOR WHETHER
!                                       SURVIVAL CURVE (DEFAULT) OR
!                                       CDF CURVE DRAWN
!     UPDATED         --JANUARY   2012. USE DPPARS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION TAG1(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),TAG1(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPKA'
      ISUBN2='PL  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
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
!
!
!               **********************************
!               **  TREAT THE KAPLAN-MEIER PLOT **
!               **********************************
!
!               *******************************************
!               **  STEP 1--                             **
!               **  SEARCH FOR KAPLAN MEIER, KAPLAN-MEIER**
!               **  MODIFIED KAPLAN MEIER, OR MODIFIED   **
!               **  KAPLAN-MEIER                         **
!               *******************************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'KAPL'.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        ICASPL='KAPL'
      ELSEIF(NUMARG.GE.2.AND.ICOM.EQ.'KAPL'.AND.   &
             IHARG(1).EQ.'MEIE'.AND.IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        ICASPL='KAPL'
      ELSEIF(NUMARG.GE.2.AND.ICOM.EQ.'MODI'.AND.   &
             IHARG(1).EQ.'KAPL'.AND.IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        ICASPL='MKAP'
      ELSEIF(NUMARG.GE.3.AND.ICOM.EQ.'MODI'.AND.   &
             IHARG(1).EQ.'KAPL'.AND.IHARG(2).EQ.'MEIE'.AND.   &
             IHARG(3).EQ.'PLOT')THEN
        ILASTC=3
        ICASPL='MKAP'
      ELSE
        ICASPL='    '
        IFOUND='NO'
        GO TO 9000
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!
!               *********************************************************
!               **  STEP 41--                                          **
!               **  FORM THE VERTICAL AND HORIZONTALAXIS               **
!               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY)FOR THE     **
!               **  PLOT.  FORM THE CURVE DESIGNATION VARIABLED(.)  .  **
!               **  THIS WILL BE ALL ONES.                             **
!               **  DEFINE THE NUMBER OF PLOT POINTS   (NPLOTP).       **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES(NPLOTV).       **
!               *********************************************************
!
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KAPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,TAG1,Y1,NS,NS,NS,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      CALL DPKAP2(Y1,TAG1,NS,NUMVAR,ICASPL,MAXN,   &
                  IKAPSW,   &
                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9015 I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPKAPL
      SUBROUTINE DPKAP2(Y1,TAG1,N,NUMV,ICASPL,MAXN,   &
                        IKAPSW,   &
                        Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN KAPLAN-MEIER PLOT
!     INPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED) OBSERVATIONS
!                               FOR THE FIRST  VARIABLE.
!                      TAG1   = 1 = FAILURE TIME, 0 = CENSORED
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!     CAUTION--THE INPUT VARIABLE Y1(.) WILL BE CHANGED HEREIN
!              (IT WILL BE SORTED)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/5
!     ORIGINAL VERSION--MAY       1998.
!     UPDATED         --JULY      2005. SWITCH TO SPECIFY WHETHER
!                                       SURVIVAL CURVE (DEFAULT) OR
!                                       CDF CURVE DRAWN
!     UPDATED         --JUNE      2008. ACCOMODATE NEGATIVE DATA
!                                       (E.G. FOR REVERSE WEIBULL
!                                       OR REVERSE FRECHET)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IKAPSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DPROD
      DOUBLE PRECISION DCURR
      DOUBLE PRECISION DN
      DOUBLE PRECISION DCORR
!
      DIMENSION Y1(*)
      DIMENSION TAG1(*)
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKA'
      ISUBN2='P2  '
      IERROR='NO'
!
      J=0
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y1(I),TAG1(I)
   56     FORMAT('I, Y1(I), TAG1(I), = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
        GO TO 9000
      ENDIF
!
      HOLD=Y1(1)
      DO 120 I=1,N
        IF(Y1(I).NE.HOLD)GO TO 129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,122)HOLD
  122 FORMAT('      ALL ELEMENTS IN THE RESPONSE VARIABLE ARE ',   &
             'IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  129 CONTINUE
!
!               ***********************************************
!               **  STEP 12--                                **
!               **  COMPUTE COORDINATES FOR KAPLAN MEIER PLOT**
!               **  (INCORPORATE STAIR-STEP APPEARANCE)      **
!               ***********************************************
!
      CALL SORTC(Y1,TAG1,N,Y1,TAG1)
!
      XMIN=Y1(1)
      XMAX=Y1(N)
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KAP2')THEN
        DO 135 I=1,N
        WRITE(ICOUT,136)I,Y1(I),TAG1(I)
  136   FORMAT('I, Y1(I), TAG1(I), = ',I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
  135   CONTINUE
      ENDIF
!
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
!
        DPROD=1.0D0
        DO 200 I=1,N
          IF(NUMV.GE.2 .AND. ABS(TAG1(I)).LT.0.5)GO TO 200
          J=J+1
          X(J)=Y1(I)
          Y(J)=Y(J-1)
          D(J)=1.0
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KAP2')THEN
            WRITE(ICOUT,203)I,J,X(J),Y(J)
  203       FORMAT('I,J,X(J),Y(J)=',2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          DCURR=(DN - DBLE(I))/(DN - DBLE(I) + 1.0D0)
          DPROD=DPROD*DCURR
          J=J+1
          X(J)=Y1(I)
          Y(J)=REAL(DPROD)
          D(J)=1.0
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KAP2')THEN
            WRITE(ICOUT,204)I,J,X(J),Y(J)
  204       FORMAT('I,J,X(J),Y(J)=',2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
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
!
        DPROD=1.0D0
        DCORR=(DN + 0.7D0)/(DN + 0.4D0)
        DO 400 I=1,N
          IF(NUMV.GE.2 .AND. ABS(TAG1(I)).LT.0.5)GO TO 400
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
!
      NPLOTP=J
      NPLOTV=2
!
!CCCC JULY 2005: CONVERT TO CDF FORMAT
!
      IF(IKAPSW.EQ.'CDF')THEN
        DO 510 I=1,NPLOTP
          Y(I)=1.0 - Y(I)
  510   CONTINUE
      ENDIF
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9022 I=1,NPLOTP
          WRITE(ICOUT,9023)I,Y(I),X(I),D(I)
 9023     FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9022   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPKAP2
      SUBROUTINE DPKAR3(Y,N,TEMP1,MAXNXT,   &
                        STATVA,CUTOFF,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES KAPPENMAN'S STATISTIC FOR
!              DISTINGUISHING BETWEEN A WEIBULL AND A LOGNORMAL
!              DISTRIBUTION.  THE TEST STATISTIC IS:
!
!                 R = (A3 - A2)/(A2 - A1)
!
!              WHERE
!
!                 A1 = AVERAGE OF THE LOWER 5% OF THE ORDERED LOGARITHMS
!                      OF THE DATA
!                 A3 = AVERAGE OF THE UPPER 5% OF THE ORDERED LOGARITHMS
!                      OF THE DATA
!                 A2 = AVERAGE OF THE ORDERED LOGARITHMS OF THE DATA
!                      AFTER TRIMMING THE LOWER 20% AND THE UPPER 20%.
!              THEN IF R > 0.7477 SELECT LOGNORMAL AND IF R < 0.7477
!              CHOOSE WEIBULL.
!
!              RESTRICT THIS TEST TO A MINIMUM SAMPLE SIZE OF 20.
!
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION RESPONSE VARIABLE.
!                     --N      = AN INTEGER PARAMETER THAT SPECIFIES THE
!                                NUMBER OF VALUES IN THE RESPONSE
!                                VARIABLE.
!     OUTPUT ARGUMENTS--R      = THE SINGLE PRECISION VALUE OF THE TEST
!                                STATISTIC.
!     OUTPUT--THE SINGLE PRECISION VALUE OF THE TEST STATISTIC.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--SAMPLE SIZE SHOULD BE AT LEAST 20.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, TRIMME.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE --KAPPENMAN (1988), "A SIMPLE METHOD FOR CHOOSING
!                 BETWEEN THE LOGNORMAL AND WEIBULL MODELS", STATISTICS
!                 & PROBABILITY LETTERS", VOL. 7, NO. 2, PP. 123-126.
!               --JOHN MCCOOL (2012), "USING THE WEIBULL DISTRIBUTION:
!                 RELIABILITY, MODELING, AND INFERENCE", WILEY,
!                 PP. 207-210.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2014/05
!     ORIGINAL VERSION--MAY       2014.
!
!---------------------------------------------------------------------
!
      REAL Y(*)
      REAL TEMP1(*)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(ISUBRO.EQ.'KAR3' .OR. IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPKAR3')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
      STATVA=CPUMIN
      CUTOFF=0.7477
!
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
        GO TO 9000
      ELSE
        DO 96 I=1,N
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,91)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,98)I,Y(I)
   98       FORMAT('      ROW ',I8,' IS NON-POSITIVE.  IT HAS THE ',   &
                   'VALUE ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ELSE
            Y(I)=LOG(Y(I))
          ENDIF
   96   CONTINUE
      ENDIF
!
      CALL SORT(Y,N,Y)
!
!     TRIMMED MEAN WITH 20% TRIMMING ON EACH TAIL
!
      PROP1=20.0
      PROP2=20.0
      NTRIM1=0
      NTRIM2=0
      IWRITE='OFF'
      CALL TRIMME(Y,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,TEMP1,   &
                  MAXNXT,A2,   &
                  IBUGA3,ISUBRO,IERROR)
!
!     AVERAGE OF LOWER 5% OF VALUES
!
      AFRAC=0.05*REAL(N)
      IFRAC=INT(AFRAC)
      REM=AFRAC-REAL(IFRAC)
      IF(IFRAC.GE.1)THEN
        CALL MEAN(Y,IFRAC,IWRITE,XMEAN,IBUGA3,IERROR)
        A1=(XMEAN + REM*Y(IFRAC+1))/(REAL(IFRAC) + REM)
      ELSE
        A1=Y(1)
      ENDIF
!
!     AVERAGE OF UPPER 5% OF VALUES
!
      IF(IFRAC.GE.1)THEN
        NSTRT=N-IFRAC+1
        SUM=0.0
        DO 110 I=NSTRT,N
          SUM=SUM + Y(I)
  110   CONTINUE
        XMEAN=SUM/REAL(IFRAC)
        A3=(XMEAN + REM*Y(NSTRT-1))/(REAL(IFRAC) + REM)
      ELSE
        A3=Y(N)
      ENDIF
!
      STATVA=(A3-A2)/(A2-A1)
!
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
!
      RETURN
      END SUBROUTINE DPKAR3
      SUBROUTINE DPKDEN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IKDENP,PKDEWI,ISEED,   &
                        ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A KERNEL DENSITY PLOT USING A
!              GAUSSIAN WINDOW.  USES APPLIED STATISTICS
!              ALGORITHM 176 (BY B. W. SILVERMAN).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001/8
!     ORIGINAL VERSION--AUGUST    2001.
!     UPDATED         --FEBRUARY  2010. USE DPPARS
!     UPDATED         --FEBRUARY  2010. SUPPORT FOR "MULTIPLE" AND
!                                       "REPLICATION"
!     UPDATED         --MARCH     2010. USE DPPAR3 FOR SINGLE RESPONSE
!                                       VARIABLE OR MULTIPLE CASES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZZ.INC'
!
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
!
      EQUIVALENCE (DGARBG(IDGAR1),Y1(1))
      EQUIVALENCE (DGARBG(IDGAR2),SMOOTH(1))
      EQUIVALENCE (DGARBG(IDGAR3),FT(1))
      EQUIVALENCE (DGARBG(IDGAR4),ZY(1))
!
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
!
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPKD'
      ISUBN2='EN  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      MAXV2=1
      MINN2=20
!
!               ***************************************************
!               **  TREAT THE KERNEL DENSITY PLOT                **
!               ***************************************************
!
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
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  EXTRACT THE COMMAND                             **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:         **
!               **    1) KERNEL DENSITY PLOT Y                      **
!               **    2) MULTIPLE KERNEL DENSITY PLOT Y1 ... YK     **
!               **    3) REPLICATED KERNEL DENSITY PLOT Y X1  X2    **
!               ******************************************************
!
!     NOTE: KERNEL DENSITY, KERNEL PLOT, DENSITY TRACE ARE SYNONYMS
!           FOR KERNEL DENSITY PLOT.
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'KERN')GO TO 89
      IF(ICOM.EQ.'MULT')GO TO 89
      IF(ICOM.EQ.'REPL')GO TO 89
      GO TO 9000
!
   89 CONTINUE
      ICASPL='KDEN'
      IMULT='OFF'
      IREPL='OFF'
      ILASTC=-9999
!
      IF(ICOM.EQ.'KERN')THEN
        IFOUN1='YES'
      ELSEIF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
      ELSEIF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
      ENDIF
!
      ISTOP=NUMARG-1
      DO 90 I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT' .OR. IHARG(I).EQ.'TRAC')THEN
          ISTOP=I
          GO TO 99
        ENDIF
   90 CONTINUE
   99 CONTINUE
!
      IFOUND='NO'
      DO 100 I=1,ISTOP
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
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
!
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')IFOUND='YES'
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN KERNEL DENSITY PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION" FOR THE KERNEL DENSITY PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'KDEN')THEN
        WRITE(ICOUT,112)ICASPL,IMULT,IREPL
  112   FORMAT('ICASPL,IMULT,IREPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
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
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',   &
                 'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN 1 AND 2;  SUCH WAS NOT THE ',   &
                 'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
        NRESP=1
      ENDIF
!
!               ********************************************
!               **  STEP 6--                              **
!               **  GENERATE THE KERNEL DENISTY PLOTS FOR **
!               **  THE VARIOUS CASES.                    **
!               ********************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *************************************************
!               **  STEP 7A--                                  **
!               **  CASE 1: SINGLE RESPONSE VARIABLE WITH NO   **
!               **          REPLICATION (RESPONSE VARIABLE CAN **
!               **          BE A MATRIX).                      **
!               *************************************************
!
      IF(NRESP.EQ.1 .AND. NREPL.EQ.0)THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    XTEMP1,XTEMP2,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        DO 701 I=1,NLOCAL
          Y1(I)=DBLE(XTEMP1(I))
  701   CONTINUE
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************************
!               **  STEP 7B--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!               **  RESET THE VECTOR D(.) TO ALL ONES.             **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
!               *****************************************************
!
        NCURVE=1
        NPLOTP=0
        CALL DPKDE2(Y1,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,   &
                    NLOCAL,ICASPL,IKDENP,PKDEWI,IKDERN,IKDEPF,   &
                    ISEED,MINN2,   &
                    WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,   &
                    WORK8,WORK9,WORK10,WORK11,WORK12,MAXOBV,   &
                    Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               ******************************************
!               **  STEP 8A--                           **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES **
!               ******************************************
!
      ELSEIF(NRESP.GT.1)THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
        NPLOTP=0
        DO 810 IRESP=1,NRESP
          NCURVE=IRESP
!
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,811)IRESP,NCURVE
  811       FORMAT('IRESP,NCURVE = ',2I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          ICOL=IRESP
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      XTEMP1,XTEMP2,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
        DO 801 I=1,NLOCAL
          Y1(I)=DBLE(XTEMP1(I))
  801   CONTINUE
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************************
!               **  STEP 8B--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!               *****************************************************
!
          CALL DPKDE2(Y1,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,   &
                      NLOCAL,ICASPL,IKDENP,PKDEWI,IKDERN,IKDEPF,   &
                      ISEED,MINN2,   &
                      WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,   &
                      WORK8,WORK9,WORK10,WORK11,WORK12,MAXOBV,   &
                      Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
  810   CONTINUE
!
!               *****************************************************
!               **  STEP 9A--                                      **
!               **  CASE 3: ONE OR TWO  REPLICATION VARIABLES.     **
!               **          FOR THIS CASE, THE NUMBER OF RESPONSE  **
!               **          VARIABLES MUST BE EXACTLY 1.           **
!               **          CURRENTLY, GROUPED DATA NOT SUPPORTED  **
!               **          WITH REPLICATION.                      **
!               *****************************************************
!
      ELSEIF(NRESP.EQ.1 .AND. NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KDEN')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO 910 I=1,IMAX
          IF(ISUB(I).EQ.0)GO TO 910
          J=J+1
!
!         RESPONSE VARIABLE IN Y1
!
          IJ=MAXN*(ICOLR(1)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
!
          ICOLC=1
          DO 920 IR=1,MIN(NREPL,2)
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
!
  910   CONTINUE
        NLOCAL=J
!
!       *****************************************************
!       **  STEP 9B--                                      **
!       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!       **                                                 **
!       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
!       **  VARIOUS REPLICATIONS.                          **
!       *****************************************************
!
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
            DO 935 I=1,NLOCAL
              WRITE(ICOUT,936)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  936         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2) = ',I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
  935       CONTINUE
          ENDIF
        ENDIF
!
!       *****************************************************
!       **  STEP 9C--                                      **
!       **  FIND THE DISTINCT VALUES IN EACH OF THE        **
!       **  REPLICATION VARIABLES.                         **
!       *****************************************************
!
        CALL DPFRE5(XDESGN(1,1),XDESGN(1,2),   &
                   NREPL,NLOCAL,MAXOBV,   &
                   XIDTEM,XIDTE2,   &
                   XTEMP1,XTEMP2,   &
                   NUMSE1,NUMSE2,   &
                   IBUGG3,ISUBRO,IERROR)
!
!       *****************************************************
!       **  STEP 9D--                                      **
!       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
!       *****************************************************
!
        NPLOTP=0
        NCURVE=0
        IF(NREPL.EQ.1)THEN
          J=0
          DO 1110 ISET1=1,NUMSE1
            K=0
            DO 1130 I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                ZY(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPKDE2(ZY,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,   &
                          NTEMP,ICASPL,IKDENP,PKDEWI,   &
                          IKDERN,IKDEPF,ISEED,MINN2,   &
                          WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,   &
                          WORK8,WORK9,WORK10,WORK11,WORK12,MAXOBV,   &
                          Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO 1210 ISET1=1,NUMSE1
          DO 1220 ISET2=1,NUMSE2
            K=0
            DO 1290 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2)   &
                )THEN
                K=K+1
                ZY(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPKDE2(ZY,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,   &
                          NTEMP,ICASPL,IKDENP,PKDEWI,   &
                          IKDERN,IKDEPF,ISEED,MINN2,   &
                          WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,   &
                          WORK8,WORK9,WORK10,WORK11,WORK12,MAXOBV,   &
                          Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1220     CONTINUE
 1210     CONTINUE
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IKDENP,PKDEWI
 9014   FORMAT('IKDENP,PKDEWI = ',I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.LE.0)GO TO 9090
        DO 9015 I=1,NPLOTP
          WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016     FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
         CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
 9090   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPKDEN
      SUBROUTINE DPKDE2(Y,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVE,   &
                        N,ICASPL,IKDENP,PKDEWI,   &
                        IKDERN,IKDEPF,ISEED,MINN2,   &
                        WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,   &
                        WORK8,WORK9,WORK10,WORK11,WORK12,MAXNXT,   &
                        Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A KERNEL DENSITY PLOT.  USES THE
!              APPLIED STATISTICS ALGORITHM 176 OF B. W. SILVERMAN
!              (COMPUTES KERNEL ESTIMATE USING THE FFT).
!              CURRENTLY, ONLY A GAUSSIAN KERNEL FUNCTION IS
!              SUPPORTED.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001/8
!     ORIGINAL VERSION--AUGUST    2001.
!     UPDATED         --FEBRUARY  2010. SUPPORT FOR "MULTIPLE" AND
!                                       "REPLICATION" CASES
!     UPDATED         --JULY      2018. OPTIONALLY GENERATE THE CDF
!                                       OR PPF INSTEAD OF PDF
!     UPDATED         --JULY      2018. OPTIONALLY GENERATE RANDOM
!                                       NUMBERS BASED ON KERNEL DENSITY
!     UPDATED         --JULY      2019. SCRATCH STORAGE FOR INTERP
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IKDEPF
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
      CHARACTER*4 IOP
!
      DOUBLE PRECISION DH
      DOUBLE PRECISION DHI
      DOUBLE PRECISION DLO
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DX
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
!
!---------------------------------------------------------------------
!
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
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKD'
      ISUBN2='E2  '
      IERROR='NO'
!
      DLO=0.0D0
      DHI=0.0D0
      DH=0.0D0
      DSD=0.0D0
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 60 I=1,N
      IF(Y(I).NE.HOLD)GO TO 69
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
      GO TO 9000
   69 CONTINUE
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'KDE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPKDE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,IKDENP,PKDEWI
   72   FORMAT('N,IKDENP,PKDEWI = ',I6,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 73 I=1,N
        WRITE(ICOUT,74)I,REAL(Y(I))
   74   FORMAT('I, Y(I) = ',I8,G15.7)
        CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
!
!               **********************************************
!               **  STEP 2--                                **
!               **  CALL DENEST ROUTINE TO COMPUTE THE      **
!               **  KERNEL DENSITY ESTIMATE.                **
!               **********************************************
!
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERROR='NO'
      ICAL=0
      KFLAG=1
      CALL DSORT(Y,Y,N,KFLAG,IERROR)
      DH=DBLE(PKDEWI)
      IF(PKDEWI.LE.0)THEN
        DN=N
        DSUM=0.0D0
        DO 200 I=1,N
          DX=Y(I)
          DSUM=DSUM+DX
  200   CONTINUE
        DMEAN=DSUM/DN
        DSUM=0.0D0
        DO 300 I=1,N
          DX=Y(I)
          DSUM=DSUM+(DX-DMEAN)**2
  300   CONTINUE
        DVAR=DSUM/(DN-1.0D0)
        DSD=0.0D0
        IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
!
        P=0.25
        AN=REAL(N)
        ANI=P*(AN+1.0)
        NI=INT(ANI+0.1)
        A2NI=REAL(NI)
        REM=ANI-A2NI
        NIP1=NI+1
        IF(NI.LE.1)NI=1
        IF(NI.GE.N)NI=N
        IF(NIP1.LE.1)NIP1=1
        IF(NIP1.GE.N)NIP1=N
        XPERC1=(1.0-REM)*Y(NI)+REM*Y(NIP1)
!
        P=0.75
        ANI=P*(AN+1.0)
        NI=INT(ANI+0.1)
        A2NI=REAL(NI)
        REM=ANI-A2NI
        NIP1=NI+1
        IF(NI.LE.1)NI=1
        IF(NI.GE.N)NI=N
        IF(NIP1.LE.1)NIP1=1
        IF(NIP1.GE.N)NIP1=N
        XPERC2=(1.0-REM)*Y(NI)+REM*Y(NIP1)
        AIQ=(XPERC2-XPERC1)/1.34
!
!CCCC   DH=DBLE(1.06)*DSD*DN**(-1.0D0/5.0D0)
        DH=0.9D0*MIN(DSD,DBLE(AIQ))*DN**(-1.0D0/5.0D0)
      ENDIF
      DLO=Y(1) - 3.0D0*DH
      DHI=Y(N) + 3.0D0*DH
!
      CALL DENEST(Y,N,DLO,DHI,DH,FT,SMOOTH,IKDENP,ICAL,IERROR)
!
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               **********************************************
!               **  STEP 3--                                **
!               **  GENERATE CDF OR PPF IF REQUESTED        **
!               **********************************************
!
      ISTEPN='3'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFRST=N2+1
      ILAST=N2+IKDENP
      IF(IKDEPF.EQ.'PDF')THEN
        DO 410 I=1,IKDENP
          N2=N2+1
          Y2(N2)=REAL(SMOOTH(I))
          X2(N2)=REAL(DLO + (DBLE(I) - 0.5D0)*(DHI-DLO)/DBLE(IKDENP))
          D2(N2)=REAL(NCURVE)
  410   CONTINUE
      ELSEIF(IKDEPF.EQ.'CDF' .OR. IKDEPF.EQ.'PPF')THEN
        DO 420 I=1,IKDENP
          N2=N2+1
          X2(N2)=REAL(DLO + (DBLE(I) - 0.5D0)*(DHI-DLO)/DBLE(IKDENP))
          TEMP1(N2)=REAL(SMOOTH(I))
          D2(N2)=REAL(NCURVE)
  420   CONTINUE
        NTEMP=2
        IWRITE='OFF'
        CALL CUMINT(TEMP1,X2,IKDENP,NTEMP,IWRITE,Y2,IBUGG3,IERROR)
        IF(IKDEPF.EQ.'PPF')THEN
          DO 430 I=IFRST,ILAST
            AVAL=Y2(I)
            Y2(I)=X2(I)
            X2(I)=AVAL
  430     CONTINUE
        ENDIF
      ENDIF
!
!               **********************************************
!               **  STEP 4--                                **
!               **  GENERATE RANDOM NUMBERS BASED ON THE    **
!               **  KERNEL DENSITY APPROXIMATION            **
!               **********************************************
!
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IKDERN.GE.1)THEN
        IF(IKDEPF.EQ.'PDF')THEN
          NTEMP=2
          IWRITE='OFF'
          CALL CUMINT(Y2(IFRST+1),X2(IFRST+1),IKDENP,NTEMP,IWRITE,TEMP3,   &
                      IBUGG3,IERROR)
          ICNT=0
          DO 440 I=IFRST,ILAST
            ICNT=ICNT+1
            TEMP1(ICNT)=X2(I)
            TEMP2(ICNT)=TEMP3(ICNT)
  440     CONTINUE
        ELSEIF(IKDEPF.EQ.'CDF')THEN
          ICNT=0
          DO 460 I=IFRST,ILAST
            ICNT=ICNT+1
            TEMP1(ICNT)=X2(I)
            TEMP2(ICNT)=Y2(I)
  460     CONTINUE
        ELSEIF(IKDEPF.EQ.'PPF')THEN
          ICNT=0
          DO 470 I=IFRST,ILAST
            ICNT=ICNT+1
            TEMP1(ICNT)=Y2(I)
            TEMP2(ICNT)=X2(I)
  470     CONTINUE
        ENDIF
!
        ISTEPN='4B'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        CALL UNIRAN(IKDERN,ISEED,TEMP3)
        CALL SORT(TEMP3,IKDERN,TEMP3)
        CALL INTERP(TEMP1,TEMP2,IKDENP,TEMP3,IKDERN,IWRITE,TEMP4,   &
                    WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,   &
                    WORK8,WORK9,WORK10,WORK11,WORK12,MAXNXT,   &
                    IBUGG3,ISUBRO,IERROR)
!
!       WRITE RESULTS TO "dpst1f.dat"
!
        ISTEPN='4C'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KDE2')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IOP='OPEN'
        IFLAG1=1
        IFLAG2=0
        IFLAG3=0
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        DO 490 I=1,IKDERN
          WRITE(IOUNI1,'(E15.7)')TEMP4(I)
  490   CONTINUE
!
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGG3,ISUBRO,IERROR)
      ENDIF
!
      NPLOTV=2
      GO TO 9000
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9015 I=1,N2
        WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016   FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
        CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPKDE2
      SUBROUTINE DPKDNP(IHARG,IARGT,ARG,NUMARG,   &
      IKDENP,IDEFKN,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE NUMBER OF POINTS USED FOR THE KERNEL DENSITY
!              CURVE IN THE KERNEL DENSITY PLOT COMMAND.
!              THE SPECIFIED KERNEL DENSITY POINTS VALUE WILL BE PLACED
!              IN THE FLOATING POINT VARIABLE IKDENP.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFKN (A FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--IKDENP  (A  FLOATING POINT VARIABLE)
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001/8
!     ORIGINAL VERSION--AUGUST    2001.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)GO TO 9000
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'POIN')GO TO 1110
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
      GO TO 9000
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'POIN')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPKDNP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR KERNEL DENSITY POINTS COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1150 CONTINUE
      HOLD=IDEFKN
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
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
        DO 1185 K=IKLOW,IKHIGH
          IF(IKDENP.GT.2**(K-1).AND.IKDENP.LE.2**K)THEN
            IKDENP=2**K
            GO TO 1189
          ENDIF
 1185   CONTINUE
      ENDIF
 1189 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)IKDENP
 1281 FORMAT('THE KERNEL DENSITY POINTS HAS JUST BEEN SET ',   &
             'TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 9000
!
!               ********************************************
!               **  STEP 81--                             **
!               **  TREAT THE    ?    CASE--              **
!               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
!               ********************************************
!
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
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPKDNP
      SUBROUTINE DPKDWI(IHARG,IARGT,ARG,NUMARG,   &
      PKDEWI,DEFKWI,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE SMOOTHING WIDTH FOR THE
!              TO BE USED FOR THE KERNEL DENSITY ESTIMATOR.
!              THE SPECIFIED KERNEL DENSITY WIDTH VALUE WILL BE PLACED
!              IN THE FLOATING POINT VARIABLE PKDEWI.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --DEFKWI (A FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--PKDEWI  (A  FLOATING POINT VARIABLE)
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2001/8
!     ORIGINAL VERSION--AUGUST    2001.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)GO TO 9000
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WIDT')GO TO 1110
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
      GO TO 9000
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'WIDT')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPKDWI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR KERNEL DENSITY WIDTH COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1150 CONTINUE
      HOLD=DEFKWI
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PKDEWI=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      IF(PKDEWI.NE.DEFKWI)THEN
        WRITE(ICOUT,1281)PKDEWI
 1281   FORMAT('THE KERNEL DENSITY WIDTH HAS JUST BEEN SET ',   &
               'TO ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ELSE
        WRITE(ICOUT,1291)
 1291   FORMAT('THE KERNEL DENSITY WIDTH HAS JUST BEEN SET ',   &
               'TO THE DEFAULT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1293)
 1293   FORMAT('DATAPLOT WILL SELECT THE WIDTH BASED ON THE DATA.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
 1289 CONTINUE
      GO TO 9000
!
!               ********************************************
!               **  STEP 81--                             **
!               **  TREAT THE    ?    CASE--              **
!               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
!               ********************************************
!
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
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPKDWI
      SUBROUTINE DPKEEP(X,N,XREF,NREF,IOP,TAG,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--GIVEN A GROUP-ID VARIABLE (X), IT MAY BE CONVENIENT
!              AT TIMES TO CREATE A TAG VARIABLE BASED ON A LIST
!              OF LABS TO EITHER KEEP OR OMIT FROM AN ANALYSIS.
!              THE VARIABLE TAG WILL BE SET TO 1 IF THE LAB IS
!              TO BE KEPT OR TO 0 IF THE LAB IS TO BE OMITTED.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                OBSERVATIONS CONTAINING THE GROUP-ID's.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X AND TAG.
!                     --XREF   = THE SINGLE PRECISION VECTOR OF
!                                GROUP-ID's TO BE EITHER KEPT OR
!                                OMITTED.
!                     --NREF   = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR XREF.
!                     --IOP    = A CHARACTER SCALAR THAT SPECIFIES
!                                WHETHER TO KEEP OR OMIT LABS BASED
!                                ON XREF.
!     OUTPUT ARGUMENTS--TAG    = THE SINGLE PRECISION VECTOR WHICH WILL
!                                BE CODED AS EITHER 0 OR 1 DEPENDING ON
!                                WHETHER THE LAB WILL BE OMITTED OR
!                                RETAINED.
!     OUTPUT--THE SINGLE PRECISION VECTOR TAG
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     ORIGINAL VERSION--APRIL     2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XREF(*)
      DIMENSION TAG(*)
!
      CHARACTER*4 IOP
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     INITIALIZE TAG VARIABLE
!
      IF(IOP.EQ.'KEEP')THEN
        ATEMP=0.0
        IF(NREF.LE.0)ATEMP=1.0
        DO 21 I=1,N
          TAG(I)=ATEMP
   21   CONTINUE
      ELSE
        ATEMP=1.0
        IF(NREF.LE.0)ATEMP=0.0
        DO 26 I=1,N
          TAG(I)=ATEMP
   26   CONTINUE
      ENDIF
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR IN DPKEEP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,17)
   17   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NREF.LE.0)THEN
!
!       IF NO LIST OF OMITTED/RETAINED ID'S GIVEN, SIMPLY RETURN.
!
        GO TO 9000
      ENDIF
!
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
        DO 112 I=1,N
          WRITE(ICOUT,113)I,X(I)
  113     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
  112   CONTINUE
        DO 117 I=1,NREF
          WRITE(ICOUT,119)I,XREF(I)
  119     FORMAT('I,XREF(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
  117   CONTINUE
      ENDIF
!
      ATEMP=1.0
      IF(IOP.EQ.'OMIT')ATEMP=0.0
      DO 1200 I=1,NREF
        XREFI=XREF(I)
        DO 1300 J=1,N
          IF(X(J).EQ.XREFI)TAG(J)=ATEMP
 1300   CONTINUE
 1200 CONTINUE
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KEEP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKEEP--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,X(I),TAG(I)
 9016     FORMAT('I,X(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPKEEP
      SUBROUTINE DPKMEA(ICAPSW,IFORSW,ISEED,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--PERFORM EITHER
!                 1. A K-MEANS CLUSTER ANALYSIS (HARTIGAN) OR
!                 2. A MIXTURE OF NORMALS CLUSTER ANALYSIS (HARTIGAN) OR
!                 3. A SINGLE LINKAGE (NEAREST NEIGHBORS) CLUSTER
!                    ANALYSIS (HARTIGAN)
!                 4. A K-MEDOIDS CLUSTER ANALYSIS (ROUSSEEUW AND
!                    KAUFFMAN, CLARA AND PAM)
!                 5. AGNES CLUSTER ANALYSIS (ROUSSEEUW AND
!                    KAUFFMAN, AGNES AND DIANA)
!     REFERENCES--JOHN HARTIGAN (1979), "ALGORITHM AS 136", APPLIED
!                 STATISTICS, VOL. 28, NO. 1.
!               --JOHN HARTIGAN (1975), "CLUSTERING ALGORITHMS",
!                 WILEY.
!               --KAUFMAN AND ROUSSEEUW (1990), "FINDING GROUPS IN
!                 DATA", WILEY.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2017/03
!     ORIGINAL VERSION--MARCH     2017.
!     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=100)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASAN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZC.INC'
!
      DIMENSION YA(20*MAXOBV)
      DIMENSION YC(MAXOBV)
      DIMENSION YD(MAXOBV)
      DIMENSION AN1(MAXOBV)
      DIMENSION AN2(MAXOBV)
      DIMENSION WSS(MAXOBV)
      DIMENSION TEMP1(3*MAXOBV)
!
      DIMENSION IC1(MAXOBV)
      DIMENSION IC2(MAXOBV)
      DIMENSION NC(MAXOBV)
      DIMENSION NCP(MAXOBV)
      DIMENSION ITRAN(MAXOBV)
      DIMENSION ILIVE(MAXOBV)
!
      CHARACTER*8 RLAB(MAXOBV)
!
      EQUIVALENCE (GARBAG(IGARB1),YC(1))
      EQUIVALENCE (GARBAG(IGARB2),YD(1))
      EQUIVALENCE (GARBAG(IGARB3),AN1(1))
      EQUIVALENCE (GARBAG(IGARB4),AN2(1))
      EQUIVALENCE (GARBAG(IGARB5),WSS(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP1(1))
      EQUIVALENCE (GARBAG(IGAR10),YA(1))
!
      EQUIVALENCE (IGARBG(IIGAR1),IC1(1))
      EQUIVALENCE (IGARBG(IIGAR2),IC2(1))
      EQUIVALENCE (IGARBG(IIGAR3),NC(1))
      EQUIVALENCE (IGARBG(IIGAR4),NCP(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITRAN(1))
      EQUIVALENCE (IGARBG(IIGAR6),ILIVE(1))
!
      EQUIVALENCE (CGARBG(1),RLAB(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DPKM'
      ISUBN2='EA  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               *********************************************
!               **  TREAT THE K-MEANS                CASE  **
!               *********************************************
!
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
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEAN' .AND.   &
         IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='KMEA'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEAN')THEN
        ILASTC=1
        ICASAN='KMEA'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEDO' .AND.   &
         IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='KMED'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEDO')THEN
        ILASTC=1
        ICASAN='KMED'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEDI' .AND.   &
         IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='KMED'
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'MEDI')THEN
        ILASTC=1
        ICASAN='KMED'
      ELSEIF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'MIXT' .AND.   &
         IHARG(2).EQ.'CLUS')THEN
        ILASTC=2
        ICASAN='NMIX'
      ELSEIF(ICOM.EQ.'SING' .AND. IHARG(1).EQ.'LINK' .AND.   &
         IHARG(2).EQ.'CLUS')THEN
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
        GO TO 9000
      ENDIF
!
      IFOUND='YES'
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 3--                          **
!               **  EXTRACT THE DATA                  **
!               ****************************************
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASAN.NE.'AGNE' .AND. ICASAN.NE.'DIAN')THEN
        IHP='NCLU'
        IHP2='STER'
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        AVAL=VALUE(ILOCV)
        NCLUST=INT(AVAL+0.1)
        IF(NCLUST.LT.2)THEN
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
        NCLUST=0
      ENDIF
!
      MAXNXT=20*MAXOBV
      ICOL=1
      CALL DPPARY(ICOL,IVALUE,IVALU2,IN,MAXN,MAXNXT,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  YA,NLOCAL,NROW,NCOL,   &
                  IBUGA3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')THEN
        WRITE(ICOUT,352)NLOCAL,NROW,NCOL,NCLUST
  352   FORMAT('NLOCAL,NROW,NCOL,NCLUST = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO 360 II=1,MIN(NLOCAL,1000)
          WRITE(ICOUT,362)II,YA(II)
  362     FORMAT('II,YA(II) = ',I8,2X,G15.7)
          CALL DPWRST('XXX','BUG ')
  360   CONTINUE
      ENDIF
!
!               *****************************************************
!               **  STEP 4--                                       **
!               **  PERFORM THE CLUSTER ANALYSIS                   **
!               *****************************************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KMEA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASAN.EQ.'KMEA')THEN
        IF(IKMERL.EQ.'ON')THEN
          J=0
          DO 401 II=1,NROW
            IF(ISUB(I).EQ.0)GO TO 401
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  401     CONTINUE
        ELSE
          J=0
          DO 403 II=1,NQ
            IF(ISUB(I).EQ.0)GO TO 403
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  403     CONTINUE
        ENDIF
        CALL DPKME2(YA,NROW,NCOL,NCLUST,   &
                    YC,YD,AN1,AN2,WSS,TEMP1,   &
                    IC1,IC2,NC,NCP,ITRAN,ILIVE,   &
                    ICAPSW,ICAPTY,IFORSW,MAXOBV,ISEED,   &
                    ISUBRO,IBUGA3,IERROR)
      ELSEIF(ICASAN.EQ.'NMIX')THEN
        IF(INMCRL.EQ.'ON')THEN
          J=0
          DO 411 II=1,NROW
            IF(ISUB(I).EQ.0)GO TO 411
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  411     CONTINUE
        ELSE
          J=0
          DO 413 II=1,NROW
            IF(ISUB(I).EQ.0)GO TO 413
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  413     CONTINUE
        ENDIF
        CALL DPNMI2(YA,NROW,NCOL,NCLUST,   &
                    GARBAG(1),YC,AN1,AN2,IC1,   &
                    IVARN1,IVARN2,RLAB,   &
                    ICAPTY,ICAPSW,IFORSW,   &
                    ISUBRO,IBUGA3,IERROR)
      ELSEIF(ICASAN.EQ.'KMED')THEN
        IF(IKMERL.EQ.'ON')THEN
          J=0
          DO 421 II=1,NROW
            IF(ISUB(I).EQ.0)GO TO 421
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  421     CONTINUE
        ELSE
          J=0
          DO 423 II=1,NQ
            IF(ISUB(I).EQ.0)GO TO 423
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  423     CONTINUE
        ENDIF
        IF(NROW.GT.IKMDPN)THEN
          CALL DPCLA2(YA,NROW,NCOL,NCLUST,IVARN1,IVARN2,   &
                      TEMP1(1),AN1,AN2,YC,   &
                      WSS(1),WSS(10000),WSS(20000),   &
                      WSS(30000),WSS(40000),   &
                      WSS(50000),WSS(60000),WSS(70000),   &
                      IC1,IC2,NC,NCP,   &
                      ITRAN(1),ITRAN(101),ITRAN(10001),   &
                      ITRAN(30001),ITRAN(40001),ITRAN(50001),   &
                      ITRAN(60001),   &
                      TEMP1(MAXOBV+1),ILIVE,   &
                      ICAPSW,ICAPTY,IFORSW,MAXOBV,ISEED,   &
                      ISUBRO,IBUGA3,IERROR)
        ELSE
          CALL DPPAM2(YA,NROW,NCOL,NCLUST,IVARN1,IVARN2,   &
                      YC,YD,AN1,AN2,WSS,   &
                      TEMP1(1),TEMP1(MAXOBV+1),TEMP1(MAXOBV+2),   &
                      IC1,IC2,NC,NCP,ITRAN,   &
                      ICAPSW,ICAPTY,IFORSW,MAXOBV,   &
                      ISUBRO,IBUGA3,IERROR)
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
          GO TO 9000
        ENDIF
!
        IF(IKMERL.EQ.'ON')THEN
          J=0
          DO 431 II=1,NROW
            IF(ISUB(I).EQ.0)GO TO 431
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  431     CONTINUE
        ELSE
          J=0
          DO 433 II=1,NQ
            IF(ISUB(I).EQ.0)GO TO 433
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  433     CONTINUE
        ENDIF
!
        CALL DPAGN2(YA,NROW,NCOL,IVARN1,IVARN2,   &
                      YC,YD,AN1,AN2,WSS,TEMP1,TEMP1(MAXOBV+1),   &
                      IC1,IC2,NC,NCP,ITRAN,   &
                      ICASAN,ICAPSW,ICAPTY,IFORSW,MAXOBV,   &
                      ISUBRO,IBUGA3,IERROR)
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
          GO TO 9000
        ENDIF
!
        IF(IKMERL.EQ.'ON')THEN
          J=0
          DO 441 II=1,NROW
            IF(ISUB(I).EQ.0)GO TO 441
            J=J+1
            RLAB(J)(1:8)=IROWLB(II)(1:8)
  441     CONTINUE
        ELSE
          J=0
          DO 443 II=1,NQ
            IF(ISUB(I).EQ.0)GO TO 443
            J=J+1
            WRITE(RLAB(J)(1:8),'(I8)')II
  443     CONTINUE
        ENDIF
        CALL DPFAN2(YA,NROW,NCOL,NCLUST,IVARN1,IVARN2,   &
                    AN1,AN2,TEMP1(1),TEMP1(MAXOBV+1),   &
                    TEMP1(2*MAXOBV+1),YC,YD,WSS,   &
                    IC1,IC2,NC,   &
                    ICAPSW,ICAPTY,IFORSW,MAXOBV,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPKMEA
      SUBROUTINE DPKME2(YA,NROW,NCOL,NCLUST,   &
                        YC,YD,AN1,AN2,WSS,TEMP1,   &
                        IC1,IC2,NC,NCP,ITRAN,ILIVE,   &
                        ICAPSW,ICAPTY,IFORSW,MAXNXT,ISEED,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--PERFORM A K-MEANS CLUSTER ANALYSIS USING HARTIGAN'S
!              APPLIED STATISTICS 136 ALGORITHM.
!     REFERENCES--JOHN HARTIGAN (1979), "ALGORITHM AS 136", APPLIED
!                 STATISTICS, VOL. 28, NO. 1.
!               --ROUSSEEUW (1987), "SILHOUETTES: A GRAPHICAL AID TO THE
!                 INTERPRETATION AND VALIDATION OF CLUSTER ANALYSIS",
!                 JOURNAL OF COMPUTATIONAL AND APPLIED MATHEMATICS,
!                 VOL. 20, PP. 53-65, NORTH HOLLAND.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2017/03
!     ORIGINAL VERSION--MARCH       2017.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION YA(NROW,NCOL)
      DIMENSION YC(NCLUST,NCOL)
      DIMENSION YD(*)
      DIMENSION AN1(*)
      DIMENSION AN2(*)
      DIMENSION WSS(*)
      DIMENSION TEMP1(*)
!
      INTEGER IC1(*)
      INTEGER IC2(*)
      INTEGER NC(*)
      INTEGER NCP(*)
      INTEGER ITRAN(*)
      INTEGER ILIVE(*)
!
      DIMENSION ALOCSV(200)
      DIMENSION SCALSV(200)
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASPL
      CHARACTER*4 ITYP3
      CHARACTER*4 IOP
      CHARACTER*10 IFORMT
!
      INCLUDE 'DPCOST.INC'
!
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
!
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKM'
      ISUBN2='E2  '
      IWRITE='OFF'
!
      ICNT=0
      ICNT2=0
      ICNT3=0
!
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
        DO 75 I=1,NROW
          WRITE(ICOUT,77)I,(YA(I,J),J=1,MIN(NCOL,3))
   77     FORMAT('I,YA(I,1),YA(I,2),YA(I,3) = ',I8,2X,3G15.7)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
!
!               ******************************
!               **   STEP 1A--              **
!               **   SCALE IF REQUESTED     **
!               ******************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KME2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 90 I=1,200
        ALOCSV(I)=0.0
        SCALSV(I)=1.0
   90 CONTINUE
!
      IF(IKMESC.EQ.'OFF')GO TO 199
!
      DO 101 JJ=1,NCOL
        DO 103 II=1,NROW
          TEMP1(II)=YA(II,JJ)
  103   CONTINUE
        IF(ISTALO.EQ.'MEAN')THEN
          CALL MEAN(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MEDI')THEN
          CALL MEDIAN(TEMP1,NROW,IWRITE,AN1,MAXNXT,XMEAN,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MIDM')THEN
          CALL MIDMEA(TEMP1,NROW,IWRITE,AN1,MAXNXT,XMEAN,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'HARM')THEN
          CALL HARMEA(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MINI')THEN
          CALL MINIM(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'GEOM')THEN
          CALL GEOMEA(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'BILO')THEN
          CALL BIWLOC(TEMP1,NROW,IWRITE,AN1,AN2,MAXNXT,XMEAN,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H15 ')THEN
          NCUT=0
          C=1.5
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H10 ')THEN
          NCUT=0
          C=1.0
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H12 ')THEN
          NCUT=0
          C=1.2
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H17 ')THEN
          NCUT=0
          C=1.7
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H20 ')THEN
          NCUT=0
          C=2.0
          CALL H15(TEMP1,NROW,C,NCUT,XMEAN,XSC,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSE
          CALL MEAN(TEMP1,NROW,IWRITE,XMEAN,IBUGA3,IERROR)
        ENDIF
!
        IF(ISTASC.EQ.'SD  ')THEN
          CALL SD(TEMP1,NROW,IWRITE,XSD,IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H15S')THEN
          NCUT=0
          C=1.5
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H10S')THEN
          NCUT=0
          C=1.0
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H12S')THEN
          NCUT=0
          C=1.2
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H17S')THEN
          NCUT=0
          C=1.7
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H20S')THEN
          NCUT=0
          C=2.0
          CALL H15(TEMP1,NROW,C,NCUT,XLOC,XSD,AN1,AN2,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'BISC')THEN
          CALL BIWSCA(TEMP1,NROW,IWRITE,AN1,AN2,MAXNXT,XSD,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'MAD ')THEN
          CALL MAD(TEMP1,NROW,IWRITE,AN1,AN2,MAXNXT,XSD,   &
                   IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'MADN')THEN
          CALL MAD(TEMP1,NROW,IWRITE,AN1,AN2,MAXNXT,XSD,   &
                   IBUGA3,IERROR)
          XSD=XSD/0.67449
        ELSEIF(ISTASC.EQ.'AAD ')THEN
          CALL AAD(TEMP1,NROW,IWRITE,AN1,MAXNXT,XSD,'MEAN',   &
                   IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'IQRA')THEN
          CALL LOWQUA(TEMP1,NROW,IWRITE,AN1,MAXNXT,RIGH1,   &
                      IBUGA3,IERROR)
          CALL UPPQUA(TEMP1,NROW,IWRITE,AN1,MAXNXT,RIGH2,   &
                      IBUGA3,IERROR)
          XSD=RIGH2-RIGH1
        ELSEIF(ISTASC.EQ.'NIQR')THEN
          CALL LOWQUA(TEMP1,NROW,IWRITE,AN1,MAXNXT,RIGH1,   &
                      IBUGA3,IERROR)
          CALL UPPQUA(TEMP1,NROW,IWRITE,AN1,MAXNXT,RIGH2,   &
                      IBUGA3,IERROR)
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
!
        IF(XSD.LE.0.0)THEN
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,106)JJ
  106     FORMAT('       VARIABLE ',I4,' HAS ZERO STANDARD DEVIATION ',   &
                 'WHEN SCALING REQUESTED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        ALOCSV(JJ)=XMEAN
        SCALSV(JJ)=XSD
        DO 105 II=1,NROW
          AVAL=(YA(II,JJ)-XMEAN)/XSD
          YA(II,JJ)=AVAL
  105   CONTINUE
  101 CONTINUE
!
  199 CONTINUE
!
!               ******************************
!               **   STEP 1B--              **
!               **   CREATE INITIAL CLUSTER **
!               ******************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KME2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!
!     RANDOMLY SELECT ROWS TO USE AS INITIAL CLUSTER
!     CENTERS.
!
      IF(IKMEIN.EQ.'RAND')THEN
        CALL RANPER(NROW,ISEED,AN1)
        DO 110 II=1,NCLUST
          IROWT=INT(AN1(II)+0.1)
          DO 120 JJ=1,NCOL
            YC(II,JJ)=YA(IROWT,JJ)
  120     CONTINUE
  110   CONTINUE
      ELSE
!
!       USE ALGORITHM SUGGESTED BY HARTIGAN TO DEFINE
!       INITIAL CLUSTER CENTERS.
!
!       FIND COLUMN MEANS
!
        DO 150 II=1,NCOL
          CALL MEAN(YA(1,II),NROW,IWRITE,XMEAN,IBUGA3,IERROR)
          AN1(II)=XMEAN
  150   CONTINUE
!
!       FIND DISTANCE FROM EACH ROW TO MEAN MATRIX
!
        DO 160 II=1,NROW
          DO 165 JJ=1,NCOL
            AN2(JJ)=YA(II,JJ)
  165     CONTINUE
          ICASPL='VEDI'
          CALL VECARI(AN1,AN2,NCOL,ICASPL,IWRITE,   &
                      TEMP1,N3,ADIST,ITYP3,   &
                      IBUGA3,ISUBRO,IERROR)
          YD(II)=ADIST
  160   CONTINUE
        CALL SORTI(YD,NROW,YD,WSS)
!
!       SELECT ROW 1 + (L-1)*(NROW/NCLUST) FOR CLUSTER L
!
        DO 170 II=1,NCLUST
          AVAL=1.0 + REAL(II-1)*(REAL(NROW)/REAL(NCLUST))
          IVAL=INT(AVAL+0.1)
          IF(IVAL.LT.1)IVAL=1
          IF(IVAL.GT.NROW)IVAL=NROW
          DO 180 JJ=1,NROW
            JROWT=INT(WSS(JJ)+0.1)
            IF(IVAL.EQ.JROWT)THEN
              IROWT=JJ
              GO TO 189
            ENDIF
  180     CONTINUE
          IROWT=NROW
  189     CONTINUE
          DO 190 JJ=1,NCOL
            YC(II,JJ)=YA(IROWT,JJ)
  190     CONTINUE
  170   CONTINUE
      ENDIF
!
!               ************************************
!               **   STEP 2--                     **
!               **   PERFORM THE CLUSTER ANALYSIS **
!               ************************************
!
      ITER=50
      CALL KMNS(YA,NROW,NCOL,YC,NCLUST,   &
                IC1,IC2,NC,AN1,AN2,NCP,YD,ITRAN,ILIVE,   &
                ITER,WSS,IFAULT)
!
      IF(IFAULT.EQ.1)THEN
        WRITE(ICOUT,211)
  211   FORMAT('****** ERROR IN K-MEANS CLUSTERING--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)
  213   FORMAT('       AT LEAST ONE CLUSTER IS EMPTY AFTER THE ',   &
               'INITIAL ASSIGNMENT.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(IFAULT.EQ.2)THEN
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,221)ITER
  221   FORMAT('       THE MAXIMUM NUMBER IF ITERATIONS (',I3,') WAS ',   &
               'EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(IFAULT.EQ.3)THEN
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,231)NCLUST
  231   FORMAT('       THE NUMBER OF CLUSTERS (',I5,') IS LESS THAN ',   &
               'TWO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,233)NROW
  233   FORMAT('       OR GREATER THAN THE NUMBER OF OBSERVATIONS (',   &
               I8,').')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************************
!               **   STEP 3--               **
!               **   WRITE OUT EVERYTHING   **
!               ******************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CWS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 8000
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
      ITITLE='Summary of K-Means Cluster Analysis'
      NCTITL=35
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=3
      NUMLIN=3
!
      ITITL2(1,1)=' '
      NCTIT2(1,1)=0
      NCOLSP(1,1)=1
      ITITL2(1,2)='Number'
      NCTIT2(1,2)=6
      NCOLSP(1,2)=1
      ITITL2(1,3)='Within'
      NCTIT2(1,3)=6
      NCOLSP(1,3)=1
!
      ITITL2(2,1)=' '
      NCTIT2(2,1)=0
      NCOLSP(2,1)=1
      ITITL2(2,2)='of Points'
      NCTIT2(2,2)=9
      NCOLSP(2,2)=1
      ITITL2(2,3)='Cluster'
      NCTIT2(2,3)=7
      NCOLSP(2,3)=1
!
      ITITL2(3,1)='Cluster'
      NCTIT2(3,1)=7
      NCOLSP(3,1)=1
      ITITL2(3,2)='in Cluster'
      NCTIT2(3,2)=10
      NCOLSP(3,2)=1
      ITITL2(3,3)='Sum of Squares'
      NCTIT2(3,3)=14
      NCOLSP(3,3)=1
!
      IWHTML(1)=150
      IWHTML(2)=200
      IWHTML(3)=200
      IINC1=1200
      IINC2=1800
      IWRTF(1)=IINC1
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC2
!
      NMAX=0
      ICNT=0
      ICNT2=0
      DO 3010 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=12
        IF(I.EQ.3)NTOT(I)=18
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
 3010 CONTINUE
!
      DO 3020 J=1,NCLUST
!
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
            IF(J.EQ.NCLUST)THEN
              ILAST=.TRUE.
              IFLAGE=.TRUE.
            ENDIF
          ENDIF
          CALL DPDT5B(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      NCOLSP,ROWSEP,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
          ICNT=1
          ICNT2=ICNT2+1
        ENDIF
!
        DO 3030 I=1,NUMCOL
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
!
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
        CALL DPDT5B(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    NCOLSP,ROWSEP,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
!               ************************************
!               **   STEP 4A--                    **
!               **   WRITE INFORMATION TO FILES   **
!               ************************************
!
 8000 CONTINUE
!
      ISTEPN='4A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KME2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOP='OPEN'
      IFLG11=1
      IFLG21=1
      IFLG31=1
      IFLAG4=0
      IF(IKMESI.EQ.'ON')IFLAG4=1
      IFLAG5=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      DO 8010 I=1,NROW
        WRITE(IOUNI1,'(E15.7)')REAL(IC1(I))
 8010 CONTINUE
!
      DO 8020 I=1,NCLUST
        WRITE(IOUNI2,'(2E15.7)')WSS(I),REAL(NC(I))
 8020 CONTINUE
!
      IFORMT='(   E15.7)'
      WRITE(IFORMT(2:4),'(I3)')NCOL
      DO 8030 I=1,NCLUST
        DO 8035 J=1,MIN(NCOL,200)
          YC(I,J)=ALOCSV(J) + SCALSV(J)*YC(I,J)
 8035   CONTINUE
        WRITE(IOUNI3,IFORMT)(YC(I,J),J=1,NCOL)
 8030 CONTINUE
!
!               *****************************************
!               **   STEP 4B--                         **
!               **   CREATE VALUES FOR SILHOUETTE PLOT **
!               *****************************************
!
      ISTEPN='4B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CWS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COMPUTE THE s(i) VALUE AS
!
!        s(i) = (b(i) - a(i))/max{a(i),b(i)}
!
!     WHERE
!
!        a(i)   = AVERAGE DISSIMILARITY OF THE i-TH POINT WITHH
!                 ALL OTHER POINTS IN THE CLUSTER TO WHICH IT
!                 BELONGS
!
!        b(i)   = LOWEST AVERAGE DISSIMILARITY OF THE i-TH POINT
!                 WITH ALL OTHER CLUSTERS.
!
!     USE ONE-PASS MEAN ALGORITHMS TO KEEP TRACK OF AVERAGE
!     DISSIMILARITY OF ALL CLUSTERS.  THE ONE-PASS FORMUALA IS
!
!         M(K)=X1                            K = 1
!             =M(K-1) + (X(K) - M(K-1))/K    K = 2, ...., N
!
      IF(IKMESI.EQ.'ON')THEN
!
        DO 8110 II=1,NROW
          ICLUS1=IC1(II)
          DO 8112 JJ=1,NCOL
            AN1(JJ)=YA(II,JJ)
 8112     CONTINUE
          ICASPL='VEDI'
          DO 8114 KK=1,NCLUST
            YD(KK)=CPUMIN
            IC2(KK)=0
 8114     CONTINUE
!
          DO 8120 JJ=1,NROW
            IF(II.EQ.JJ)GO TO 8120
            ICLUS2=IC1(JJ)
            DO 8122 KK=1,NCOL
              AN2(KK)=YA(JJ,KK)
 8122       CONTINUE
            CALL VECARI(AN1,AN2,NCOL,ICASPL,IWRITE,   &
                        TEMP1,N3,ADIST,ITYP3,   &
                        IBUGA3,ISUBRO,IERROR)
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
!
          AI=YD(ICLUS1)
          BI=CPUMAX
          DO 8130 JJ=1,NCLUST
            IF(JJ.EQ.ICLUS1)GO TO 8130
            IF(YD(JJ).LT.BI)BI=YD(JJ)
 8130     CONTINUE
          WSS(II)=(BI - AI)/MAX(AI,BI)
!
 8110   CONTINUE
!
        DO 8140 I=1,NROW
          WRITE(IOUNI4,'(2E15.7)')REAL(IC1(I)),WSS(I)
 8140   CONTINUE
      ENDIF
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8091)
 8091   FORMAT('THE CLUSTER ID VALUES ARE WRITTEN TO dpst1f.dat')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8093)
 8093   FORMAT('THE WITHIN-CLUSTER SUM OF SQUARES AND ',   &
               'THE NUMBER OF POINTS')
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
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KME2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKME2--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPKME2
      SUBROUTINE DPKNND(NPLOTV,NPLOTP,ICASPL,ICAPSW,IFORSW,   &
                        ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--PERFORM A K NEAREST NEIGHBORS DISCRIMINANT ANALYSIS
!     REFERENCES--HASTIE, TIBSHIRANI, AND FRIEDMAN (2001), "THE ELEMENTS
!                 OF STATISTICAL LEARNING: DATA MINING, INFERENCE, AND
!                 PREDICTION", SPRINGER, CHAPTERS 2 AND 4.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2024/07
!     ORIGINAL VERSION--JULY      2024.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!     SET MAXIMUM NUMBER OF VARIABLES (MAXVAR) AND MAXIMUM NUMBER
!     OF CLASSES (MAXCLS)
!
      PARAMETER (MAXVAR=50)
      PARAMETER (MAXCLS=50)
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=MAXVAR+1)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASPL
      CHARACTER*4 PCCASE
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*20 IFORMT
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZC.INC'
!
      DIMENSION XMAT(20*MAXOBV)
      DIMENSION DIST(10*MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TAG(MAXOBV)
      DIMENSION PC1(MAXOBV)
      DIMENSION PC2(MAXOBV)
      DIMENSION TDIST(MAXCLS)
      DIMENSION TCODE(MAXCLS)
      DIMENSION CUMDIS(MAXCLS)
      DIMENSION AINDX(MAXVAR)
!
      INTEGER COUNT(MAXCLS)
      INTEGER ISEQ(MAXOBV)
!
      EQUIVALENCE (GARBAG(1),XMAT(1))
      EQUIVALENCE (GARBAG(IGAR11),TEMP1(1))
      EQUIVALENCE (GARBAG(IGAR12),TEMP2(1))
      EQUIVALENCE (GARBAG(IGAR13),TAG(1))
      EQUIVALENCE (GARBAG(IGAR14),PC1(1))
      EQUIVALENCE (GARBAG(IGAR15),PC2(1))
      EQUIVALENCE (GARBAG(IGAR16),TDIST(1))
      EQUIVALENCE (GARBAG(IGAR17),TCODE(1))
      EQUIVALENCE (GARBAG(IGAR18),CUMDIS(1))
      EQUIVALENCE (GARBAG(IGAR19),AINDX(1))
      EQUIVALENCE (DGARBG(IDGAR1),DIST(1))
      EQUIVALENCE (IGARBG(1),ISEQ(1))
      EQUIVALENCE (IGARBG(IIGAR2),COUNT(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DPKN'
      ISUBN2='ND  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               *********************************************
!               **  TREAT THE DISCRIMINANT           CASE  **
!               *********************************************
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'KNND')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPKNND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,ISUBRO,IKNNPC,IKNNN
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO,IKNNPC,IKNNN = ',   &
               5(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ICOM,IHARG(1),IHARG(2)
   55   FORMAT('ICOM,IHARG(1),IHARG(2) = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KNND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'NEAR' .AND.   &
         IHARG(2).EQ.'NEIG' .AND. IHARG(3).EQ.'PLOT')THEN
        ILASTC=3
        ICASPL='KNN '
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'NEAR' .AND.   &
         IHARG(2).EQ.'NEIG' .AND. IHARG(3).EQ.'DISC' .AND.   &
         IHARG(4).EQ.'PLOT')THEN
        ILASTC=4
        ICASPL='KNN '
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'NEAR' .AND.   &
         IHARG(2).EQ.'NEIG' .AND. IHARG(3).EQ.'CLAS' .AND.   &
         IHARG(4).EQ.'PLOT')THEN
        ILASTC=4
        ICASPL='KNN '
      ELSEIF(ICOM.EQ.'KNN ' .AND. IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        ICASPL='KNN '
      ELSEIF(ICOM.EQ.'KNN ' .AND. IHARG(1).EQ.'DISC' .AND.   &
         IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        ICASPL='KNN '
      ELSEIF(ICOM.EQ.'KNN ' .AND. IHARG(1).EQ.'CLAS' .AND.   &
         IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        ICASPL='KNN '
      ELSEIF(ICOM.EQ.'KNN ' .AND. IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        ICASPL='KNN '
      ELSE
        IFOUND='NO'
        GO TO 9000
      ENDIF
!
      IFOUND='YES'
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KNND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='K NEAREST NEIGHBOR DISCRIMINANT PLOT'
      MINNA=1
      MAXNA=100
      MINN2=5
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=MAXSPN
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KNND')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 3--                          **
!               **  EXTRACT THE DATA                  **
!               ****************************************
!
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KNND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     EXTRACT THE DATA.  NOTE THE FOLLOWING:
!
!       THE LAST VARIABLE IS THE GROUP-ID VARIABLE.  A VALUE
!       OF 0 INDICATES THAT ROW OF DATA IS TO BE CLASSIFIED,
!       ALL OTHER VALUES INDICATE A TRAINING ROW.
!
      NTEMP=NRIGHT(1)
      NROW=0
      NTRAIN=0
      NPRED=0
      J=0
      IMAX=NTEMP
      AVAL=0.0
      IF(NQ.LT.NTEMP)IMAX=NQ
      J=0
      AVAL=0.0
!
!     FIRST PASS TO DETERMINE NUMBER OF ROWS AND THE
!     NUMBER OF TRAINING ROWS AND PREDICTION ROWS
!
      DO 310 I=1,IMAX
!
        IF(ISUB(I).EQ.0)GO TO 310
        J=J+1
        ICOL=NUMVAR
        IJ=MAXN*(ICOLR(ICOL)-1)+I
        IF(ICOLR(ICOL).LE.MAXCOL)AVAL=V(IJ)
        IF(ICOLR(ICOL).EQ.MAXCP1)AVAL=PRED(I)
        IF(ICOLR(ICOL).EQ.MAXCP2)AVAL=RES(I)
        IF(ICOLR(ICOL).EQ.MAXCP3)AVAL=YPLOT(I)
        IF(ICOLR(ICOL).EQ.MAXCP4)AVAL=XPLOT(I)
        IF(ICOLR(ICOL).EQ.MAXCP5)AVAL=X2PLOT(I)
        IF(ICOLR(ICOL).EQ.MAXCP6)AVAL=TAGPLO(I)
        IVAL=INT(AVAL+0.5)
        IF(IVAL.EQ.0)THEN
          NPRED=NPRED+1
        ELSE
          NTRAIN=NTRAIN+1
        ENDIF
        NROW=NROW+1
        TAG(NROW)=AVAL
        ISEQ(NROW)=I
  310 CONTINUE
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KNND')THEN
        WRITE(ICOUT,312)NROW,NTRAIN,NPRED
  312   FORMAT('NROW,NTRAIN,NPRED = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      J=0
      N1=0
      AVAL=0.0
      DO 340 I=1,IMAX
        IF(ISUB(I).EQ.0)GO TO 340
        J=J+1
        N1=N1+1
        DO 330 KK=1,NUMVAR-1
          ICOL=KK
          IJ=MAXN*(ICOLR(ICOL)-1)+I
          IF(ICOLR(ICOL).LE.MAXCOL)AVAL=V(IJ)
          IF(ICOLR(ICOL).EQ.MAXCP1)AVAL=PRED(I)
          IF(ICOLR(ICOL).EQ.MAXCP2)AVAL=RES(I)
          IF(ICOLR(ICOL).EQ.MAXCP3)AVAL=YPLOT(I)
          IF(ICOLR(ICOL).EQ.MAXCP4)AVAL=XPLOT(I)
          IF(ICOLR(ICOL).EQ.MAXCP5)AVAL=X2PLOT(I)
          IF(ICOLR(ICOL).EQ.MAXCP6)AVAL=TAGPLO(I)
          IINDX=(KK-1)*NROW + N1
          XMAT(IINDX)=AVAL
  330   CONTINUE
!
  340 CONTINUE
!
      IF(NTRAIN.LT.5)THEN
        WRITE(ICOUT,101)
  101   FORMAT('****** ERROR IN K NEAREST NEIGHBOR PLOT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,342)
  342   FORMAT('     LESS THAN 5 TRAINING ROWS DETECTED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,344)NTRAIN
  344   FORMAT('     NUMBER OF ROWS: ',I5)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NPRED.LT.1)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,347)
  347   FORMAT('     NO CLASSIFICATION ROWS DETECTED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KNND')THEN
        IFORMT='(I8,2X,  G15.7)'
        WRITE(IFORMT(8:9),'(I2)')NUMVAR
        IF(NROW.GE.1)THEN
          WRITE(ICOUT,357)
  357     FORMAT('GROUP-ID VARIABLE:')
          CALL DPWRST('XXX','BUG ')
          DO 358 II=1,NROW
            WRITE(ICOUT,359)II,TAG(II)
  359       FORMAT('II,TAG(II) = ',I5,2X,G15.7)
            CALL DPWRST('XXX','BUG ')
  358     CONTINUE
          WRITE(ICOUT,362)
  362     FORMAT('DATA:')
          CALL DPWRST('XXX','BUG ')
          DO 360 II=1,NROW*(NUMVAR-1)
            WRITE(ICOUT,364)II,XMAT(II)
  364       FORMAT('II,X(II) = ',I5,2X,G15.7)
            CALL DPWRST('XXX','BUG ')
  360     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************************
!               **  STEP 4--                                          **
!               **  GENERATE THE K NEAREST NEIGHBOR DISCRIMINANT PLOT **
!               ********************************************************
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KNND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASPL.EQ.'KNN ')THEN
        NUMVA2=NUMVAR-1
!
!       COMPUTE THE FIRST TWO PRINCIPAL COMPONENTS IF NEEDED
!
        IF(NUMVA2.GT.2 .AND. IKNNPC.EQ.'YES')THEN
          PCCASE=IPCMTY
          IF(PCCASE(1:2).EQ.'CV' .OR. PCCASE(1:2).EQ.'CR')   &
             PCCASE(1:2)='DA'
          CALL PRCOMP(XMAT,NROW,NUMVA2,PCCASE,   &
                      TEMP1(1),TEMP1(1000),TEMP1(10000),TEMP1(20000),   &
                      TEMP1(30000),TEMP1(40000),TEMP1(50000),   &
                      PC1,PC2,   &
                      IBUGG3,ISUBRO,IERROR)
!
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'KNND')THEN
            DO 401 II=1,NROW
              WRITE(ICOUT,403)II,PC1(II),PC2(II)
  403         FORMAT('II,PC1(II),PC2(II) = ',I5,2X,2G15.7)
              CALL DPWRST('XXX','BUG ')
  401       CONTINUE
          ENDIF
!
        ENDIF
!
        IHP='P   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
             IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
             ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          P=2.0
        ELSE
          P=VALUE(ILOCP)
        ENDIF
!
        CALL DPKNN2(XMAT,NROW,NUMVA2,DIST,TAG,PC1,PC2,TDIST,TCODE,   &
                    AINDX,TEMP1,TEMP2,COUNT,CUMDIS,ISEQ,   &
                    ICAPSW,ICAPTY,IFORSW,MAXOBV,IKNNPC,IKNNN,IKNNDI,P,   &
                    MAXCLS,MAXVAR,   &
                    Y,X,D,NPLOTP,NPLOTV,   &
                    ISUBRO,IBUGG3,IERROR)
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'KNND')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKNND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPKNND
      SUBROUTINE DPKNN2(X,NROW,NP,DIST,TAG,PC1,PC2,TDIST,TCODE,   &
                        AINDX,TEMP1,TEMP2,COUNT,CUMDIST,ISEQ,   &
                        ICAPSW,ICAPTY,IFORSW,MAXNXT,IKNNPC,KN,IKNNDI,P,   &
                        MAXCLS,MAXVAR,   &
                        Y2,X2,D2,N2,NPLOTV,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--PERFORM A K NEAREST NEIGHBORS DISCRIMINATION ANALYSIS
!     REFERENCES--HASTIE, TIBSHIRANI, AND FRIEDMAN (2001), "THE ELEMENTS
!                 OF STATISTICAL LEARNING: DATA MINING, INFERENCE, AND
!                 PREDICTION", SPRINGER, CHAPTERS 2 AND 4.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2024/07
!     ORIGINAL VERSION--JULY        2024.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION X(NROW,NP)
      DIMENSION DIST(*)
      DIMENSION TAG(*)
      DIMENSION PC1(*)
      DIMENSION PC2(*)
      DIMENSION TDIST(*)
      DIMENSION TCODE(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION AINDX(*)
      DIMENSION CUMDIST(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
      INTEGER COUNT(*)
      INTEGER ISEQ(*)
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IKNNPC
      CHARACTER*4 IKNNDI
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFLGPC
      CHARACTER*4 IOP
!
      EXTERNAL KINDEX
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKN'
      ISUBN2='N2  '
      IWRITE='OFF'
      IFLAGO=0
      XCOOR=0.0
      YCOOR=0.0
      NPLOTV=2
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'KNN2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10)
   10   FORMAT('AT THE BEGINNING OF DPKNN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)IKNNPC,IKNNDI,MAXVAR,MAXCLS,NROW,NP,KN,P
   12   FORMAT('IKNNPC,IKNNDI,MAXVAR,MAXCLS,NROW,NP,KN,P = ',   &
               2(A4,2X),5I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)ICAPSW,ICAPTY,ISUBRO,IBUGG3
   13   FORMAT('ICAPSW,ICAPTY,ISUBRO,IBUGG3 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO 15 I=1,NROW
          WRITE(ICOUT,17)I,TAG(I),PC1(I),PC2(I),(X(I,J),J=1,MIN(NP,8))
   17     FORMAT('I,TAG(I),PC1(I),PC2(I),X(I,J),J=1,8) = ',   &
                 I8,2X,9G15.7)
          CALL DPWRST('XXX','BUG ')
   15   CONTINUE
      ENDIF
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
      IFLGPC='NO'
      IF(NP.GT.2 .AND. IKNNPC.EQ.'YES')IFLGPC='YES'
!
!               *****************************************************
!               **   STEP 1--                                      **
!               **   CHECK IF TAG IS CATEGORICAL AND DETERMINE THE **
!               **   CLASSES (SHOULD BE BETWEEN 2 AND MAXCLS)      **
!               *****************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     NOTE: TAG DEFINES THE GROUPS.
!
      CALL DISTIN(TAG,NROW,IWRITE,TEMP1,NDIST,IBUGG3,IERROR)
!
      IFLAGC=0
      NGROUP=0
      DO 100 II=1,NDIST
        IINDX=INT(TEMP1(II)+0.5)
        IF(IINDX.EQ.0)THEN
          IFLAGC=1
        ELSE
          NGROUP=NGROUP+1
          TDIST(NGROUP)=TEMP1(II)
        ENDIF
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')THEN
          WRITE(ICOUT,102)II,TEMP1(II),IINDX,NGROUP,IFLAGC
  102     FORMAT('II,TEMP1(II),IINDX,NGROUP,IFLAGC = ',I6,F10.2,3I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
  100 CONTINUE
!
      IF(IFLAGC.EQ.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('****** ERROR IN K NEAREST NEIGHBOR DISCRIMINANT ',   &
               'ANALYSIS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)
  103   FORMAT('       THERE ARE NO OBSERVATIONS TO CLASSIFY, ',   &
               'NOTHING DONE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NGROUP.GT.NROW/2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('       THE NUMBER OF UNIQUE VALUES IN THE CLASS ',   &
               'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)
  115   FORMAT('       IS GREATER THAN (NUMBER OF OBSERVATIONS/2).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)NROW
  117   FORMAT('       NUMBER OF OBSERVATIONS:   ',I10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,119)NGROUP
  119   FORMAT('       NUMBER OF GROUPS:         ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NGROUP.GT.MAXCLS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('       THE NUMBER OF UNIQUE VALUES IN THE GROUP ',   &
               'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,125)
  125   FORMAT('       IS GREATER THAN THE MAXIMIM NUMBER OF GROUPS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,127)MAXCLA
  127   FORMAT('       MAXIMUM NUMBER OF GROUPS:   ',I10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,119)NGROUP
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NGROUP.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('       THE NUMBER OF GROUPS IS LESS THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      CALL SORT(TDIST,NGROUP,TDIST)
!
!     WRITE OUT VARIOUS DATA TO AUXILIARY FILES
!
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      IOP='OPEN'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'NO')IFLAGO=1
      WRITE(IOUNI1,*)'Classification of New Observations'
      WRITE(IOUNI1,*)'Row-ID    Group Classification'
      WRITE(IOUNI2,*)'Distances'
      WRITE(IOUNI2,*)'ROW    COLUMN  DISTANCE'
!
!               ********************************************************
!               **   STEP 2--                                         **
!               **   COMPUTE THE DISTANCE MATRIX                      **
!               ********************************************************
!
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICNT=0
      DO 210 II=1,NROW
        DO 215 KK=1,NP                   !  EXTRACT I-TH ROW
          TEMP1(KK)=X(II,KK)
  215   CONTINUE
        DO 220 JJ=II+1,NROW
          DO 223 KK=1,NP                 !  EXTRACT J-TH ROW
            TEMP2(KK)=X(JJ,KK)
  223     CONTINUE
          SUM=0.0
!
!        COMPUTE THE EUCLIDEAN DISTANCE
!
          IF(IKNNDI.EQ.'EUC')THEN
            DO 225 KK=1,NP
              SUM=SUM + (TEMP1(KK) - TEMP2(KK))**2
  225       CONTINUE
            ADIST=SQRT(SUM)
          ELSE
                                                                                                                                  
            CALL EUCDI2(TEMP1,TEMP2,NP,IKNNDI,P,IWRITE,ADIST,   &
                        IBUGG3,ISUBRO,IERROR)
          ENDIF
          ICNT=ICNT+1
          DIST(ICNT)=ADIST
          WRITE(IOUNI2,'(2I8,E15.7)')II,JJ,DIST(ICNT)
  220   CONTINUE
  210 CONTINUE
!
!               *****************************************************
!               **   STEP 3--                                      **
!               **   NOW LOOP THROUGH POINTS THAT NEED TO BE       **
!               **   CLASSIFIED                                    **
!               *****************************************************
!
      ISTEPN='3'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL SORT(TDIST,NGROUP,TDIST)
      CALL CODE(TDIST,NGROUP,IWRITE,TCODE,TEMP2,MAXNXT,IBUGG3,IERROR)
      ICNT=0
      DO 310 II=1,NROW
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')THEN
          WRITE(ICOUT,309)II
  309     FORMAT('II = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IVAL=INT(TAG(II)+0.5)
        ICNT2=0
        IF(IVAL.EQ.0)THEN
          ICNT=ICNT+1
          IF(IFLGPC.EQ.'NO')THEN
            Y2(ICNT)=X(II,1)
            X2(ICNT)=X(II,2)
          ELSE
            Y2(ICNT)=PC1(II)
            X2(ICNT)=PC2(II)
          ENDIF
!
!         FOR I-TH ROW, EXTRACT DISTANCES FROM I-TH ROW
!         OF THE DISTANCE MATRIX.  ONLY THE UPPER TRIANGULAR
!         PORTION OF THE DISTANCE MATRIX IS STORED (AS A 1-D
!         ARRAY), SO NEED TO EXTRACT THE APPROPIATE INDEX NUMBER.
!
          IF(II.LT.NROW)THEN
            DO 320 JJ=II+1,NROW
              ICNT2=ICNT2+1
              IVAL=KINDEX(NROW,II,JJ)
              TEMP1(ICNT2)=DIST(IVAL)
              AINDX(ICNT2)=REAL(JJ)
  320       CONTINUE
          ENDIF
!
          IF(II.GT.1)THEN
            DO 321 JJ=1,II-1
              ICNT2=ICNT2+1
              IVAL=KINDEX(NROW,JJ,II)
              TEMP1(ICNT2)=DIST(IVAL)
              AINDX(ICNT2)=REAL(JJ)
  321       CONTINUE
          ENDIF
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')THEN
            DO 311 JJ=1,ICNT2
              WRITE(ICOUT,313)JJ,TEMP1(JJ),AINDX(JJ)
  313         FORMAT('JJ,TEMP1(JJ),AINDX(JJ) = ',I6,2G15.7)
              CALL DPWRST('XXX','BUG ')
  311       CONTINUE
          ENDIF
!
          CALL SORTC(TEMP1,AINDX,ICNT2,TEMP1,TEMP2)
          DO 322 KK=1,ICNT2
            AINDX(KK)=TEMP2(KK)
  322     CONTINUE
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')THEN
            WRITE(ICOUT,316)
  316       FORMAT('AFTER SORTING THE DISTANCES:')
            CALL DPWRST('XXX','BUG ')
            DO 317 JJ=1,ICNT2
              WRITE(ICOUT,318)JJ,TEMP1(JJ),AINDX(JJ)
  318         FORMAT('JJ,TEMP1(JJ),AINDX(JJ) = ',I6,2G15.7)
              CALL DPWRST('XXX','BUG ')
  317       CONTINUE
          ENDIF
!
          IF(KN.EQ.1 .OR. KN.EQ.2)THEN
            IVAL=INT(AINDX(1)+0.1)
            AVAL=TAG(IVAL)
            IVAL2=-1
            DO 324 LL=1,NDIST
              IF(AVAL.EQ.TDIST(LL))THEN
                IVAL2=LL
                GO TO 325
              ENDIF
  324       CONTINUE
  325       CONTINUE
            D2(ICNT)=TCODE(IVAL2)+REAL(NGROUP)
            WRITE(IOUNI1,'(I8,E15.7)')ISEQ(II),TDIST(IVAL2)
          ELSE
            DO 330 LL=1,MAXCLS
              COUNT(LL)=0
              CUMDIST(LL)=0.0
  330       CONTINUE
!
!           COMPUTE COUNTS FOR EACH CLASS
!
            DO 340 KK=1,NGROUP
              HOLD=TDIST(KK)
!
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')THEN
                WRITE(ICOUT,341)KK,TDIST(KK),HOLD
  341           FORMAT('KK,TDIST(KK),HOLD = ',I6,2G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              DO 345 LL=1,KN
                AVAL=TAG(INT(AINDX(LL)+0.1))
!
                IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')THEN
                  WRITE(ICOUT,342)LL,AVAL
  342             FORMAT('LL,AVAL = ',I6,G15.7)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
!
                IF(AVAL.EQ.HOLD)THEN
                  COUNT(KK)=COUNT(KK)+1
                  CUMDIST(KK)=CUMDIST(KK) + TEMP1(LL)
                ENDIF
  345         CONTINUE
  340       CONTINUE
!
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')THEN
              DO 347 KK=1,NGROUP
                WRITE(ICOUT,348)KK,COUNT(KK),CUMDIST(KK)
  348           FORMAT('KK,COUNT(KK),CUMDIST(KK) = ',2I6,G15.7)
                CALL DPWRST('XXX','BUG ')
  347         CONTINUE
            ENDIF
!
!           NOW DETERMINE MAXIMUM COUNT AND INDEX OF MAXIMUM COUNT
!
            INDMAX=0
            IMAX=0
            DO 350 KK=1,NGROUP
              IF(COUNT(KK).GT.IMAX)THEN
                INDMAX=KK
                IMAX=COUNT(KK)
              ENDIF
  350       CONTINUE
!
!           CHECK FOR TIES
!
            ADIST=CUMDIST(INDMAX)
            DO 360 KK=1,NGROUP
              IF(COUNT(KK).EQ.IMAX .AND. CUMDIST(KK).LT.ADIST)THEN
                INDMAX=KK
                ADIST=CUMDIST(KK)
              ENDIF
  360       CONTINUE
            D2(ICNT)=REAL(NGROUP) + TCODE(INDMAX)
            WRITE(IOUNI1,'(I8,F8.0)')ISEQ(II),TCODE(INDMAX)
!
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'KNN2')THEN
              WRITE(ICOUT,366)IMAX,INDMAX,ADIST
  366         FORMAT('IMAX,INDMAX,ADIST = ',2I8,G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ENDIF
        ELSE
!
!         OBSERVATION DOES NOT NEED TO BE CLASSIFIED.  PLOT
!         BASED ON FIRST TWO VARIABLES OR FIRST TWO PRINCIPAL
!         COMPONENTS
!
          ICNT=ICNT+1
          IF(IFLGPC.EQ.'YES')THEN
            Y2(ICNT)=PC1(II)
            X2(ICNT)=PC2(II)
          ELSE
            Y2(ICNT)=X(II,1)
            X2(ICNT)=X(II,2)
          ENDIF
          AVAL=TAG(II)
          IVAL=-1
          DO 380 LL=1,NGROUP
            IF(AVAL.EQ.TDIST(LL))THEN
              IVAL=LL
              GO TO 389
            ENDIF
  380     CONTINUE
  389     CONTINUE
          D2(ICNT)=TCODE(IVAL)
        ENDIF
  310 CONTINUE
!
      N2=ICNT
      NPLOTV=2
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
!
 9000 CONTINUE
!
!     CLOSE THE AUXILIARY FILES
!
      IF(IFLAGO.EQ.1)THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGG3,ISUBRO,IERROR)
      ENDIF
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'KNN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKNN2--')
        CALL DPWRST('XXX','BUG ')
        IF(N2.GT.0)THEN
          DO 920 II=1,N2
            WRITE(ICOUT,9021)II,Y2(II),X2(II),D2(II)
 9021       FORMAT('II,Y2(II),X2(II),D2(II) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
  920     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPKNN2
      SUBROUTINE DPKNOT(IHARG,IHARG2,NUMARG,IDEFK1,IDEFK2,   &
      IKNOT1,IKNOT2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE USER VARIABLE NAME IN WHICH
!              THE KNOTS FOR SPLINE FITTING RESIDE.
!              CHARACTERS 1 TO 4 OF THE SPECIFIED KNOT NAME
!              WILL BE PLACED IN THE HOLLERITH VARIABLE IKNOT1;
!              CHARACTERS 5 TO 8 OF THE SPECIFIED KNOT NAME
!              WILL BE PLACED IN THE HOLLERITH VARIABLE IKNOT2.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IHARG2 (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFK1 (A  HOLLERITH VARIABLE)
!                     --IDEFK2 (A  HOLLERITH VARIABLE)
!     OUTPUT ARGUMENTS--IKNOT1 (A  HOLLERITH VARIABLE)
!                     --IKNOT2 (A  HOLLERITH VARIABLE)
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER 1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IDEFK1
      CHARACTER*4 IDEFK2
      CHARACTER*4 IKNOT1
      CHARACTER*4 IKNOT2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      GO TO 1110
!
 1110 CONTINUE
      IF(NUMARG.LE.0)GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD1=IDEFK1
      IHOLD2=IDEFK2
      GO TO 1180
!
 1160 CONTINUE
      IHOLD1=IHARG(NUMARG)
      IHOLD2=IHARG2(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IKNOT1=IHOLD1
      IKNOT2=IHOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IKNOT1,IKNOT2
 1181 FORMAT('THE KNOTS VARIABLE HAS JUST BEEN DESIGNATED AS ',   &
      A4,A4)
      CALL DPWRST('XXX','BUG ')
      IF(IKNOT1.EQ.'    '.AND.IKNOT2.EQ.'    ')WRITE(ICOUT,1182)
 1182 FORMAT('(THAT IS, THE NO-KNOTS CASE IS BEING ASSUMED)')
      IF(IKNOT1.EQ.'    '.AND.IKNOT2.EQ.'    ')CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPKNOT
      SUBROUTINE DPKLOT(MAXNXT,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A 2-SAMPLE KLOTZ TEST FOR EQUAL VARIANCES
!     EXAMPLE--KLOTZ TEST Y1 Y2
!              KLOTZ TEST Y1 Y2 Y3 Y4
!              KLOTZ TEST Y1 TO Y10
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/5
!     ORIGINAL VERSION--MAY       2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
!
!---------------------------------------------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
      DIMENSION YRANK(2*MAXOBV)
      DIMENSION YTEMP(2*MAXOBV)
      DIMENSION XTEMP3(2*MAXOBV)
      EQUIVALENCE(GARBAG(IGARB1),YRANK(1))
      EQUIVALENCE(GARBAG(IGARB3),YTEMP(1))
      EQUIVALENCE(GARBAG(IGARB5),XTEMP3(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKL'
      ISUBN2='OT  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IFOUND='NO'
      IERROR='NO'
!
!               ************************************************
!               **  TREAT THE KLOTZ TEST CASE                 **
!               ************************************************
!
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
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTZ=9999
      ICASAN='KLOT'
      ICASA2='TWOT'
!
!     LOOK FOR:
!
!          KLOTZ TEST
!          LOWER TAILED
!          UPPER TAILED
!
      DO 100 I=0,NUMARG-1
!
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
        ELSE
          ICTMP1=IHARG(I)
        ENDIF
        ICTMP2=IHARG(I+1)
!
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
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
!
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')THEN
        WRITE(ICOUT,91)ICASAN,ICASA2,ISHIFT
   91   FORMAT('DPKLOT: ICASAN,ICASA2,ISHIFT = ',   &
               2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!               ******************************************************
!               **  STEP 3A--                                       **
!               **  CASE 1: TWO RESPONSE VARIABLES, NO REPLICATION  **
!               **          HANDLE MULTIPLE RESPONSE VARIABLES      **
!               **          DIFFERENTLY FOR ONE SAMPLE AND TWO      **
!               **          SAMPLE TESTS.                           **
!               ******************************************************
!
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVA2=1
      DO 5210 I=1,NUMVAR
        ICOL=I
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y,Y,Y,NS1,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        ISTRT2=I+1
        ISTOP2=NUMVAR
!
        DO 5220 J=ISTRT2,ISTOP2
!
          ICOL=J
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      X,X,X,NS2,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               *******************************************
!               **  STEP 52--                            **
!               **  PERFORM A KLOTZ RANK SUM TEST        **
!               *******************************************
!
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
            DO 5215 II=1,MAX(NS1,NS2)
              WRITE(ICOUT,5216)II,Y(II),X(II)
 5216         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
!
          IVARID=IVARN1(I)
          IVARI2=IVARN2(I)
          IVARI3=IVARN1(J)
          IVARI4=IVARN2(J)
          CALL DPKLO2(Y,NS1,X,NS2,ICASA2,   &
                     YRANK,YTEMP,XTEMP3,MAXNXT,   &
                     ICAPSW,ICAPTY,IFORSW,   &
                     IVARID,IVARI2,IVARI3,IVARI4,   &
                     STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                     CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,   &
                     CTU999,CTU995,CTU990,CT975,CTU950,CTU900,   &
                     IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KLOT')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IF(NUMVAR.GT.2)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
          IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
          CALL DPMNN5(ICASA2,   &
                      STATVA,STATCD,   &
                      PVAL2T,PVALLT,PVALUT,   &
                      CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,   &
                      CTU999,CTU995,CTU990,CT975,CTU950,CTU900,   &
                      IFLAGU,IFRST,ILAST,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
!
 5220   CONTINUE
 5210 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPKLOT
      SUBROUTINE DPKLO2(Y1,N1,Y2,N2,ICASAN,   &
                        TEMP1,TEMP2,TEMP3,MAXNXT,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                        CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,   &
                        CTU999,CTU995,CTU990,CTU975,CTU950,CTU900,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT A 2-SAMPLE KLOTZ TEST FOR
!              EQUAL VARIANCES
!     EXAMPLE--KLOTZ TEST Y1 Y2
!     SAMPLE 1 IS IN INPUT VECTOR Y1
!              (WITH N1 OBSERVATIONS).
!     SAMPLE 2 IS IN INPUT VECTOR Y2
!              (WITH N1 OBSERVATIONS).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/5
!     ORIGINAL VERSION--MAY       2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
!
      PARAMETER (NUMALP=6)
      REAL ALPHA(NUMALP)
      PARAMETER (NUMAL2=4)
      REAL ALPHA2(NUMAL2)
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA/0.90, 0.95, 0.975, 0.99, 0.995, 0.999/
      DATA ALPHA2/0.80, 0.90, 0.95, 0.99/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKL'
      ISUBN2='O2  '
!
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
!
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
        DO 56 I=1,MAX(N1,N2)
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ************************************
!               **   STEP 1--                     **
!               **   CALL DPKLO3 TO COMPUTE THE   **
!               **   BASIC TEST STATISTIC.        **
!               ************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPKLO3(Y1,N1,Y2,N2,   &
                  TEMP1,TEMP2,TEMP3,MAXNXT,   &
                  STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                  IBUGA3,ISUBRO,IERROR)
      CALL MEAN(Y1,N1,IWRITE,YMEAN1,IBUGA3,IERROR)
      CALL VAR(Y1,N1,IWRITE,YVAR1,IBUGA3,IERROR)
      CALL MEAN(Y2,N2,IWRITE,YMEAN2,IBUGA3,IERROR)
      CALL VAR(Y2,N2,IWRITE,YVAR2,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 21--                        **
!               **  COMPUTE THE CRITICAL VALUES FOR  **
!               **  VARIOUS VALUES OF ALPHA          **
!               ***************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     LARGE SAMPLE NORMAL APPROXIMATION VALUES FIRST
!
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
!
!               *************************************************
!               **   STEP 22--                                 **
!               **   WRITE OUT EVERYTHING                      **
!               **   FOR A KLOTZ  TEST                         **
!               *************************************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
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
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Test (Normal Approximation):'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      NUMROW=ICNT
      DO 2110 I=1,NUMROW
        NTOT(I)=15
 2110 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='21A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Two-Tailed Test: Normal Approximation'
      NCTITL=37
      ITITL9='H0: Var(Y1) = Var(Y2); Ha: Var(Y1) <> Var(Y2)'
      NCTIT9=45
!
      DO 2130 J=1,NUMCLI
        DO 2140 I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2140   CONTINUE
 2130 CONTINUE
!
      NUMCOL=4
      ITITL2(2,1)='Significance'
      NCTIT2(2,1)=12
      ITITL2(3,1)='Level'
      NCTIT2(3,1)=5
!
      ITITL2(2,2)='Test '
      NCTIT2(2,2)=4
      ITITL2(3,2)='Statistic'
      NCTIT2(3,2)=9
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (+/-)'
      NCTIT2(3,3)=11
!
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
!
      NMAX=0
      DO 2150 I=1,NUMCOL
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
!
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
!
      ICNT=NUMAL2
      DO 2160 J=1,NUMAL2
!
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
!
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 2160 CONTINUE
!
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
!
      IF(ICASAN.EQ.'TWOT')THEN
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
      IF(ICASAN.EQ.'LOWE')THEN
!
        ITITLE='Lower-Tailed Test: Normal Approximation'
        NCTITL=39
        ITITL9='H0: Var(Y1) = Var(Y2); Ha: Var(Y1) < Var(Y2)'
        NCTIT9=44
!
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (<)'
        NCTIT2(3,3)=9
        NUMCOL=4
!
        NMAX=0
        DO 2250 I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 2250   CONTINUE
!
        ICNT=NUMALP
        DO 2260 J=1,NUMALP
!
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
!
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
      IF(ICASAN.EQ.'UPPE')THEN
!
        ITITLE='Upper-Tailed Test: Normal Approximation'
        NCTITL=39
        ITITL9='H0: Var(Y1) = Var(Y2); Ha: Var(Y1) > Var(Y2)'
        NCTIT9=44
!
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (>)'
        NCTIT2(3,3)=9
        NUMCOL=4
!
        NMAX=0
        DO 2350 I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 2350   CONTINUE
!
          ICNT=NUMALP
        DO 2360 J=1,NUMALP
!
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
!
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPKLO2
      SUBROUTINE DPKLO3(Y1,N1,Y2,N2,   &
                        TEMP1,TEMP2,YRANK,MAXNXT,   &
                        STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE KLOTZ 2-SAMPLE TEST STATISTIC
!              FOR EQUAL VARIANCES AND ASSOCIATED CDF AND P-VALUES.
!
!              THIS PART IS EXTRACTED FROM DPKLO2 IN ORDER TO
!              ALLOW IT TO BE COMPUTED FROM THE "STATISTICS" ROUTINES
!              (E.G., STATISTIC PLOT, BOOTSTRAP).
!
!     EXAMPLE--KLOTZ TEST Y1 Y2
!              SAMPLE 1 IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS)
!              SAMPLE 2 IS IN INPUT VECTOR Y2 (WITH N2 OBSERVATIONS).
!     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
!                THIRD EDITION, WILEY, PP. 401 - 402.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/5
!     ORIGINAL VERSION--MAY       2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DOUBLE PRECISION RSUM1
      DOUBLE PRECISION RSUM2
      DOUBLE PRECISION RSUM3
      DOUBLE PRECISION C1
      DOUBLE PRECISION C2
      DOUBLE PRECISION DNUM
      DOUBLE PRECISION DENOM
      DOUBLE PRECISION DRANK
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION YRANK(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKL'
      ISUBN2='O3  '
!
      IERROR='NO'
      IWRITE='OFF'
!
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL2T=CPUMIN
      PVALLT=CPUMIN
      PVALUT=CPUMIN
!
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
        DO 56 I=1,MAX(N1,N2)
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 01--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='01'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN KLOTZ TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',   &
               'FIRST RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE MUST BE 2 OR LARGER.  SUCH WAS NOT THE ',   &
               'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N1
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS   = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N2.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,122)
  122   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',   &
               'SECOND RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y1(1)
      DO 135 I=2,N1
        IF(Y1(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
  139 CONTINUE
!
      HOLD=Y2(1)
      DO 145 I=2,N1
        IF(Y2(I).NE.HOLD)GO TO 149
  145 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,141)HOLD
  141 FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
  149 CONTINUE
!
!               ************************************
!               **   STEP 11--                    **
!               **   COMPUTE KLOTZ    TEST        **
!               ************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COMPUTE RANKS, BUT SUBTRACT MEANS FROM DATA FIRST
!
      CALL MEAN(Y1,N1,IWRITE,YMEAN1,IBUGA3,IERROR)
      CALL MEAN(Y2,N2,IWRITE,YMEAN2,IBUGA3,IERROR)
      DO 1100 I=1,N1
        TEMP1(I)=Y1(I) - YMEAN1
 1100 CONTINUE
      NTOT=N1
      DO 1110 I=1,N2
        NTOT=NTOT+1
        TEMP1(NTOT)=Y2(I) - YMEAN2
 1110 CONTINUE
      CALL RANK(TEMP1,NTOT,IWRITE,YRANK,TEMP2,MAXNXT,IBUGA3,IERROR)
!
!     NOW COMPUTE NORMAL SCORES
!
      DO 1120 I=1,NTOT
        ATEMP=YRANK(I)/REAL(NTOT+1)
        CALL NORPPF(ATEMP,APPF)
        YRANK(I)=APPF
 1120 CONTINUE
!
!     COMPUTE KLOTZ TEST STATISTIC:
!
!         T = SUM[i=1 to N1][A(i)**2 - (N1/N)*SUM[i=1 to N][A(i)**2]/
!             SQRT{(N1*N2/(N*(N-1))*[SUM[i=1 to N][A(i)**4] -
!             (1/N)*(SUM[i=1 to N][A(i)**2)**2]}
!
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KLO3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      RSUM1=0.0D0
      RSUM2=0.0D0
      RSUM3=0.0D0
!
      DO 1210 I=1,N1
        DRANK=DBLE(YRANK(I))
        RSUM1=RSUM1 + DRANK**2
 1210 CONTINUE
!
      DO 1220 I=1,NTOT
        DRANK=DBLE(YRANK(I))
        RSUM2=RSUM2 + DRANK**2
        RSUM3=RSUM3 + DRANK**4
 1220 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
!     CDF AND P-VALUES COMPUTED FROM STANDARD NORMAL APPROXIMATION
!
      CALL NORCDF(STATVA,VAL1)
      VAL2=1.0 - VAL1
      VAL=MIN(VAL1,VAL2)
      PVAL2T=2.0*VAL
      PVALLT=VAL1
      PVALUT=VAL2
      STATCD=VAL1
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPKLO3
      SUBROUTINE DPKRUS(TEMP4,TEMP5,MAXNXT,   &
                        ICAPSW,IFORSW,IMULT,   &
                        ISUBRO,IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT KRUSKAL-WALLIS TEST
!              NON-PARAMETRIC ONE-WAY ANOVA
!     EXAMPLE--KRUSKAL-WALLIS TEST Y X
!     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
!                THIRD EDITION, WILEY, PP. 288-297.
!              --WALPOLE AND MEYERS (1978), "PROBABILITY AND
!                STATISTICS", SECOND EDITION, MACMILLIAN.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/6
!     ORIGINAL VERSION--JUNE      1999.
!     UPDATED         --OCTOBER   2004. SUPPORT FOR HTML AND LATEX
!                                       OUTPUT
!     UPDATED         --FEBRUARY  2011. USE DPPARS
!     UPDATED         --FEBRUARY  2011. SUPPORT FOR "MULTIPLE" CASE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFLAGU
!
      LOGICAL IFRST
      LOGICAL ILAST
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
!---------------------------------------------------------------------
!
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION DTAG(MAXOBV)
      DIMENSION ARANK(MAXOBV)
      DIMENSION NRANK(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION RTEMP(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE(GARBAG(IGARB1),DTAG(1))
      EQUIVALENCE(GARBAG(IGARB2),ARANK(1))
      EQUIVALENCE(GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE(GARBAG(IGARB4),TEMP2(1))
      EQUIVALENCE(GARBAG(IGARB5),TEMP3(1))
      EQUIVALENCE(GARBAG(IGARB6),RTEMP(1))
!
      INCLUDE 'DPCOZI.INC'
      EQUIVALENCE(IGARBG(IIGAR1),NRANK(1))
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKR'
      ISUBN2='US  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IFOUND='YES'
      IERROR='NO'
!
!               ******************************************
!               **  TREAT THE KRUSKAL-WALLIS TEST CASE  **
!               ******************************************
!
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 185 I=1,NUMVAR
            WRITE(ICOUT,187)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  187       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  185     CONTINUE
        ENDIF
      ENDIF
!
!               *******************************************************
!               **  STEP 3--                                         **
!               **  GENERATE THE KRUSKAL WALLIS TEST FOR THE VARIOUS **
!               **  CASES                                            **
!               *******************************************************
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************************************
!               **  STEP 3A--                          **
!               **  CASE 1: TWO RESPONSE VARIABLES     **
!               **          WITH NO REPLICATION        **
!               *****************************************
!
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        NUMVA2=2
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y,X,X,NLOCAL,NLOCA2,NLOCA2,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!
!               ******************************************************
!               **  STEP 3B--
!               **  PREPARE FOR ENTRANCE INTO DPKRU2--
!               ******************************************************
!
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
          DO 335 I=1,NLOCAL
            WRITE(ICOUT,336)I,Y(I),X(I)
  336       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
  335     CONTINUE
        ENDIF
!
        CALL DPKRU2(Y,X,NLOCAL,IVARN1,IVARN2,   &
                    DTAG,ARANK,NRANK,MAXNXT,   &
                    RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                    STATVA,STATCD,PVAL,   &
                    CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,   &
                    ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,IKRUMC,   &
                    ISUBRO,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IFLAGU='ON'
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPFRT5(STATVA,STATCD,PVAL,   &
                      CUT0,CUT50,CUT75,CUT90,CUT95,   &
                      CUT975,CUT99,CUT999,   &
                      IFLAGU,IFRST,ILAST,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!               *******************************************************
!               **  STEP 4A--                                        **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES.  NOTE THAT  **
!               **          FOR KRUSKAL-WALLIS TEST, THE MULTIPLE    **
!               **          LABS ARE CONVERTED INTO A "Y X" STACKED  **
!               **          PAIR WHERE "X" IS THE LAB-ID VARIABLE.   **
!               *******************************************************
!
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        NUMVA2=NUMVAR
        CALL DPPAR8(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    TEMP1,Y,X,NLOCAL,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        NUMVAR=2
        IF(IERROR.EQ.'YES')GO TO 9000
!
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
            DO 445 I=1,NLOCAL
              WRITE(ICOUT,446)I,Y(I),X(I)
  446         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
  445       CONTINUE
          ENDIF
        ENDIF
!
        CALL DPKRU2(Y,X,NLOCAL,IVARN1,IVARN2,   &
                    DTAG,ARANK,NRANK,MAXNXT,   &
                    RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                    STATVA,STATCD,PVAL,   &
                    CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,   &
                    ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,IKRUMC,   &
                    ISUBRO,IBUGA3,IERROR)
!
!         ***************************************
!         **  STEP 8C--                        **
!         **  UPDATE INTERNAL DATAPLOT TABLES  **
!         ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IFLAGU='ON'
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPFRT5(STATVA,STATCD,PVAL,   &
                      CUT0,CUT50,CUT75,CUT90,CUT95,   &
                      CUT975,CUT99,CUT999,   &
                      IFLAGU,IFRST,ILAST,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
!
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPKRUS
      SUBROUTINE DPKRU2(Y,TAG,N,IVARID,IVARI2,   &
                        DTAG,ARANK,NRANK,MAXNXT,   &
                        RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                        STATVA,STATCD,PVAL,   &
                        CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,   &
                        CUT99,CUT999,   &
                        ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,IKRUMC,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT KRUSKALL-WALLIS'S TEST
!              NON-PARAMETRIC ONE-WAY ANOVA
!     EXAMPLE--KRUSKALL-WALLIS TEST Y TAG
!     REFERENCE--W. J. CONOVER, "PRACTICAL NONPARAMETRIC
!                STATISTICS", THIRD EDITION, 1999, WILEY,
!                PP. 288-297.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/6
!     ORIGINAL VERSION--JUNE      1999.
!     UPDATED         --OCTOBER   2004. SUPPORT FOR HTML AND LATEX
!                                       OUTPUT
!     UPDATED         --OCTOBER   2004. ADD MULTIPLE COMPARISONS
!     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
!     UPDATED         --JANUARY   2007. CALL LIST TO RANK
!     UPDATED         --FEBRUARY  2009. SORT BY GROUP-ID VARIABLE
!                                       FIRST (THIS INSURES MULTIPLE
!                                       COMPARISONS ARE PRINTED IN
!                                       CORRECT ORDER).
!     UPDATED         --FEBRUARY  2009. ADD SOME DEBUGGING CODE
!     UPDATED         --FEBRUARY  2011. USE DPDTA1 AND DPDTA4 TO PRINT
!                                       OUTPUT TABLES.  THIS ADDS RTF
!                                       SUPPORT AND SPECIFICATION OF
!                                       THE NUMBER OF DIGITS.
!     UPDATED         --FEBRUARY  2011. OPTION TO PRINT GROUP
!                                       STATISTICS
!     UPDATED         --JULY      2011. SPLIT OFF DPKRU3, MAKE MORE
!                                       EFFICIENT USE OF STORAGE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IATEMP
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
!
!---------------------------------------------------------------------
!
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
!
!---------------------------------------------------------------------
!
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
!
      PARAMETER(NUMCLI=7)
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DATA ALPHA/   &
       0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.9/
!
      ISUBN1='DPKR'
      ISUBN2='U2  '
      ISUBN0='    '
!
      IERROR='NO'
!
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
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
      CALL DPKRU3(Y,TAG,N,   &
                  DTAG,ARANK,NRANK,MAXNXT,   &
                  RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                  STATVA,STATCD,PVAL,NUMDF,NUMDIS,S2,   &
                  IKRUGS,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      CUT0=0.0
      CALL CHSPPF(.50,NUMDF,CUT50)
      CALL CHSPPF(.75,NUMDF,CUT75)
      CALL CHSPPF(.90,NUMDF,CUT90)
      CALL CHSPPF(.95,NUMDF,CUT95)
      CALL CHSPPF(.975,NUMDF,CUT975)
      CALL CHSPPF(.99,NUMDF,CUT99)
      CALL CHSPPF(.999,NUMDF,CUT999)
!
      IOP='OPEN'
      IFLG1=1
      IFLG2=0
      IFLG3=0
      IFLG4=0
      IFLG5=0
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      WRITE(IOUNI1,2305)
 2305 FORMAT('     I       J    ',   &
             '|Ri/Ni-Rj/nj|      ',   &
             '90% CV        ',   &
             '95% CV        ',   &
             '99% CV        ')
!
      IDF=N-NUMDIS
      ALPHAT=0.05
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT95)
      ALPHAT=0.10
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT90)
      ALPHAT=0.01
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT99)
      AN=REAL(N)
      AFACT2=SQRT(S2*(REAL(N)-1.0-STATVA)/REAL(N-NUMDIS))
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')THEN
        WRITE(ICOUT,2321)AFACT2
 2321   FORMAT('BEFORE MULTIPLE COMPARISONS: AFACT2 = ',G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      DO 2330 I=1,NUMDIS
        DO 2339 J=1,NUMDIS
          IF(I.LT.J)THEN
            ANI=REAL(NRANK(I))
            ANJ=REAL(NRANK(J))
            ADIFF=ABS((ARANK(I)/ANI) - (ARANK(J)/ANJ))
            AFACT3=SQRT((1.0/ANI) + (1.0/ANJ))
            ACV90=AT90*AFACT2*AFACT3
            ACV95=AT95*AFACT2*AFACT3
            ACV99=AT99*AFACT2*AFACT3
            CALL TCDF(ADIFF,REAL(IDF),ACDF)
            PVALZ=1.0 - ACDF
            IATEMP='    '
            IF(ADIFF.GE.ACV90)IATEMP(2:2)='*'
            IF(ADIFF.GE.ACV95)IATEMP(3:3)='*'
            IF(ADIFF.GE.ACV99)IATEMP(4:4)='*'
            WRITE(IOUNI1,2337)I,J,ADIFF,ACV90,ACV95,ACV99,IATEMP,PVALZ
 2337       FORMAT(I6,2X,I6,2X,4E15.7,A4,E15.7)
!
            IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')THEN
              WRITE(ICOUT,2341)I,J,ANI,ANJ,ARANK(I),ARANK(J)
 2341         FORMAT('I,J,ANI,ANJ,ARANK(I),ARANK(J) = ',2I8,4G15.7)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,2343)AFACT3,ADIFF
 2343         FORMAT('AFACT3,ADIFF = ',2G15.7)
              CALL DPWRST('XXX','WRIT')
            ENDIF
!
          ENDIF
 2339   CONTINUE
 2330 CONTINUE
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************
!               **   STEP 42--                **
!               **   WRITE OUT EVERYTHING     **
!               **   FOR KRUSKALL-WALLIS TEST **
!               ********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')   &
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
      ITITLE='Kruskal-Wallis One Factor Test'
      NCTITL=32
      ITITLZ=' '
      NCTITZ=0
!
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
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Group-ID Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
!     IF REQUESTED, PRINT OUT GROUP INFORMATION.  SINCE NUMBER
!     OF GROUPS IS UNKNOWN (AND POTENTIALLY LARGE, PRINT EACH
!     GROUP AS A SEPARATE TABLE.
!
      IF(IKRUGS.EQ.'ON')THEN
!
        DO 2160 I=1,NUMDIS
!
          NUMROW=ICNT
          DO 2165 II=1,NUMROW
            NTOT(II)=15
 2165     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
!
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGA3,IERROR)
          ICNT=0
          ITITLE=' '
          NCTITL=0
          ITITLZ=' '
          NCTITZ=0
!
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=1
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
!
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
!
        IF(ICNT.GT.0)THEN
          NUMROW=ICNT
          DO 2168 II=1,NUMROW
            NTOT(II)=15
 2168     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
!
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                      AVALUE,IDIGIT,   &
                      NTOT,NUMROW,   &
                      ICAPSW,ICAPTY,ILAST,IFRST,   &
                      ISUBRO,IBUGA3,IERROR)
          ICNT=0
        ENDIF
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
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
!
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
!
      NUMROW=ICNT
      DO 4210 I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE=' '
      NCTITL=0
!
      ITITL9=' '
      NCTIT9=0
      ITITLE(1:55)=   &
      'Percent Points of the Chi-Square Reference Distribution'
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
!
      NMAX=0
      DO 4221 I=1,NUMCOL
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
      DO 4223 I=1,NUMROW
        DO 4225 J=1,NUMCOL
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
!
      IWHTML(1)=150
      IWHTML(2)=50
      IWHTML(3)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+500
      IWRTF(3)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.FALSE.
!
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CDF1=CUT90
      CDF2=CUT95
      CDF3=CUT975
      CDF4=CUT99
!
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
!
      NMAX=0
      DO 4321 I=1,NUMCOL
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
      DO 4323 I=1,NUMROW
        DO 4325 J=1,NUMCOL
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
!
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
!
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(IKRUMC.EQ.'OFF')GO TO 9000
!
      ITITLE(1:26)='Multiple Comparisons Table'
      NCTITL=26
      ITITL9=' '
      NCTIT9=0
!
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
      ITITL2(1,7)='P-VALUE'
      NCTIT2(1,7)=7
!
      NMAX=0
      NUMCOL=7
      DO 4010 I=1,NUMCOL
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
      IWHTML(7)=150
      IINC=1600
      IINC2=200
      IINC3=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
      IWRTF(7)=IWRTF(6)+IINC
!
      ICNT=0
      DO 4081 I=1,NUMDIS
        DO 4083 J=1,NUMDIS
          IF(I.LT.J)THEN
!
            ANI=REAL(NRANK(I))
            ANJ=REAL(NRANK(J))
            ADIFF=ABS((ARANK(I)/ANI) - (ARANK(J)/ANJ))
            AFACT3=SQRT((1.0/ANI) + (1.0/ANJ))
            ACV90=AT90*AFACT2*AFACT3
            ACV95=AT95*AFACT2*AFACT3
            ACV99=AT99*AFACT2*AFACT3
            CALL TCDF(ADIFF,REAL(IDF),ACDF)
            PVALZ=1.0 - ACDF
!
            IF(ICNT.GE.MAXROW)THEN
              NUMLIN=1
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
              CALL DPDTA5(ITITLE,NCTITL,   &
                          ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                          MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                          IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                          IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                          ICAPSW,ICAPTY,IFRST,ILAST,   &
                          IFLAGS,IFLAGE,   &
                          ISUBRO,IBUGA3,IERROR)
              ICNT=0
            ENDIF
!
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
            IVALUE(ICNT,7)=' '
            NCVALU(ICNT,7)=0
            AMAT(ICNT,7)=PVALZ
          ENDIF
 4083   CONTINUE
 4081 CONTINUE
!
      IF(ICNT.GE.1)THEN
        NUMLIN=1
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
       ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPKRU2
      SUBROUTINE DPKRU3(Y,TAG,N,   &
                        DTAG,ARANK,NRANK,MAXNXT,   &
                        RTEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                        STATVA,STATCD,PVAL,NUMDF,NUMDIS,S2,   &
                        IKRUGS,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT KRUSKALL-WALLIS'S TEST
!              NON-PARAMETRIC ONE-WAY ANOVA
!     EXAMPLE--KRUSKALL-WALLIS TEST Y TAG
!     REFERENCE--W. J. CONOVER, "PRACTICAL NONPARAMETRIC
!                STATISTICS", THIRD EDITION, 1999, WILEY,
!                PP. 288-297.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/7
!     ORIGINAL VERSION--JULY      2011. EXTRACTED FROM DPKRU3 TO ALLOW
!                                       IT TO BE CALLED FROM CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IKRUGS
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
!
!---------------------------------------------------------------------
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKR'
      ISUBN2='U3  '
      ISUBN0='    '
!
      IERROR='NO'
      IWRITE='OFF'
!
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
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN KRUSKAL-WALLIS TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 2.')
        WRITE(ICOUT,1115)N
 1115   FORMAT('      THE SAMPLE SIZE = ',I8)
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
      GO TO 9000
 1139 CONTINUE
!
      HOLD=TAG(1)
      DO 1235 I=2,N
        IF(TAG(I).NE.HOLD)GO TO 1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)HOLD
 1231 FORMAT('      THE GROUP-ID VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
 1239 CONTINUE
!
!               ********************************
!               **  STEP 41--                 **
!               **  CARRY OUT CALCULATIONS    **
!               **  FOR KRUSKALL-WALLIS TEST  **
!               ********************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL SORTC(TAG,Y,N,TAG,TEMP1)
      DO 2101 I=1,N
        Y(I)=TEMP1(I)
 2101 CONTINUE
      CALL DISTIN(TAG,N,IWRITE,DTAG,NUMDIS,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      CALL RANK(Y,N,IWRITE,RTEMP,TEMP1,MAXNXT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU3')THEN
        DO 4110 I=1,N
          WRITE(ICOUT,4133)I,TAG(I),Y(I),RTEMP(I)
 4133     FORMAT('I,TAG(I),Y(I),RTEMP(I) = ',I8,2X,3G15.7)
          CALL DPWRST('XXX','WRIT')
 4110   CONTINUE
      ENDIF
!
!CCCC OCTOBER 2004: THE KRUSKAL-WALLIS STATISTIC FOR THE CASE
!CCCC WITH NO TIES IS:
!CCCC
!CCCC    H = [12/(N*(N+1)]*SUM[i=1 to k][R(i)**2/N(i)] - 3*(N+1)
!CCCC
!CCCC THE FORMULA WITH TIES IS:
!CCCC
!CCCC    H = (1/S**2)*{SUM[i=1 to k][R(i)**2/N(i) - N*(N+1)**2/4}
!CCCC
!CCCC GO AHEAD AND USE THE TIES FORMULA SINCE IT IS JUST AS EASY
!CCCC AND IT ALSO FACILATES THE COMPUTATION OF MULTIPLE COMPARISONS.
!
!CCCC AFACT=12.0/(REAL(N)*REAL(N+1))
      AN=REAL(N)
      AFACT=AN*(AN+1.0)**2/4.0
!
      DSUM1=0.0D0
      DO 2190 I=1,N
        DSUM1=DSUM1 + DBLE(RTEMP(I))**2
 2190 CONTINUE
      S2=REAL((DSUM1 - DBLE(AFACT))/DBLE(N-1))
!
      DSUM1=0.0D0
      DO 2200 IDIS=1,NUMDIS
         J=0
         DSUM2=0.0D0
         DO 2210 I=1,N
            IF(TAG(I).EQ.DTAG(IDIS))THEN
               J=J+1
               DSUM2=DSUM2 + DBLE(RTEMP(I))
               IF(IKRUGS.EQ.'ON')TEMP1(J)=Y(I)
            ENDIF
 2210    CONTINUE
         IF(IKRUGS.EQ.'ON')THEN
           CALL MEDIAN(TEMP1,J,IWRITE,TEMP5,MAXNXT,YMED,   &
                       IBUGA3,IERROR)
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
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU3')THEN
        WRITE(ICOUT,2221)NUMDIS,AFACT,S2
 2221   FORMAT('NUMDIS,AFACT,S2 = ',I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 2220 I=1,NUMDIS
          WRITE(ICOUT,2223)I,NRANK(I),ARANK(I)
 2223     FORMAT('I,NRANK(I),ARANK(I) = ',2I8,2X,G15.7)
          CALL DPWRST('XXX','WRIT')
 2220   CONTINUE
      ENDIF
!
      DTERM1=DSUM1 - DBLE(AFACT)
      STATVA=DTERM1/DBLE(S2)
      NUMDF=NUMDIS-1
      CALL CHSCDF(STATVA,NUMDF,STATCD)
      PVAL=1.0 - STATCD
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPKRU3
      SUBROUTINE DPKUO2(Y,X,N,MAXNXT,IKUOTA,   &
                        TEMP1,TEMP2,PID,IVARID,IVARI2,NREPL,NLABID,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,   &
                        CUT975,CUT99,CUT995,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES AN OUTLIER TEST BASED ON THE
!              SAMPLE KURTOSIS.  IF THE SAMPLE KURTOSIS IS ABOVE THE
!              CRITICAL VALUE, ASSUME THE OBSERVATION FURTHERST FROM
!              THE MEAN IS AN OUTLIER.  THIS TEST CAN BE REPEATED BY
!              REMOVING THE OUTLIER AND REPEATING THE TEST UNTIL THE
!              TEST INDICATES NO OUTLIER.  THIS TEST ASSUMES NORMALITY.
!              THIS ROUTINE ONLY TESTS FOR A SINGLE OUTLIER.  FOR
!              MULTIPLE OUTLIERS, THE USER SHOULD DELETE THE CURRENT
!              OUTLIER AND REPEAT THE TEST.
!
!              THIS TEST WAS ADDED TO SUPPORT THE ASTM E-178 STANDARD
!              (2016 EDITION).
!
!              CRITICAL VALUES CAN BE DETERMINED IN THE FOLLOWING
!              WAYS:
!
!                1. TABLES FROM ASTM E178 - 16a
!                2. SIMULATION
!
!     REFERENCE--E178 - 16A (2016), "Standard Practice for Dealing with
!                Outlying Observations", ASTM International, 100 Barr
!                Harbor Drive, PO BOX C700, West Conshohocken, PA
!                19428-2959, USA.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2019/10
!     ORIGINAL VERSION--OCTOBER   2019.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IKUOTA
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IKUOT2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      REAL ALPHA(6)
      REAL CV(6)
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=30)
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
      LOGICAL IFLAGA
      LOGICAL IFLAGB
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION PID(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKU'
      ISUBN2='O2  '
      IERROR='NO'
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
      CUT80=CPUMIN
      CUT90=CPUMIN
      CUT95=CPUMIN
      CUT975=CPUMIN
      CUT99=CPUMIN
      CUT995=CPUMIN
!
      IKUOT2=IKUOTA
      IF(IKUOTA.EQ.'ASTM' .AND. N.GT.50)IKUOT2='SIMU'
      IF(IKUOT2.EQ.'ASTM')THEN
        NALPHA=3
        ALPHA(1)=90.
        ALPHA(2)=95.
        ALPHA(3)=99.
      ELSE
        NALPHA=6
        ALPHA(1)=80.
        ALPHA(2)=90.
        ALPHA(3)=95.
        ALPHA(4)=97.5
        ALPHA(5)=99.
        ALPHA(6)=99.5
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPKUO2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)ISUBRO,IBUGA3,IKUOTA,N,MAXNXT
   52   FORMAT('ISUBRO,IBUGA3,IKUOTA,N,MAXNXT = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),X(I)
   57     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN KURTOSIS OUTLIER TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 4.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1114)N
 1114   FORMAT('SAMPLE SIZE = ',I8)
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
      WRITE(ICOUT,1131)HOLD
 1131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1139 CONTINUE
!
!               ***********************************
!               **  STEP 21--                    **
!               **  CARRY OUT CALCULATIONS       **
!               **  FOR  KURTOSIS OUTLIER  TEST  **
!               ***********************************
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KUO2')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
      CALL DPKUO3(Y,N,TEMP1,TEMP2,IWRITE,PSTAMV,   &
                  MAXNXT,IKUOT2,ISEED,   &
                  ALPHA,CV,NALPHA,   &
                  STATVA,YMEAN,YSD,YMIN,YMAX,YKURT,   &
                  PVAL,STATCD,YIND,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IKUOT2.EQ.'ASTM')THEN
        CUT90=CV(1)
        CUT95=CV(2)
        CUT99=CV(3)
        NCDF=3
      ELSE
        CUT80=CV(1)
        CUT90=CV(2)
        CUT95=CV(3)
        CUT975=CV(4)
        CUT99=CV(5)
        CUT995=CV(6)
        NCDF=6
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')THEN
        WRITE(ICOUT,2111)YMEAN,YSD,YKURT
 2111   FORMAT('YMEAN,YSD,YKURT=',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2113)STATVA,PVAL,CDF,YIND
 2113   FORMAT('STATVA,PVAL,CDF,YIND =',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!
!               *********************************
!               **   STEP 42--                 **
!               **   WRITE OUT EVERYTHING      **
!               **   FOR KURTOSIS OUTLIER TEST **
!               *********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')   &
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
      ITITLE(1:26)='Kurtosis Test for Outliers'
      NCTITL=26
      ITITLZ='(Assumption: Normality)'
      NCTITZ=23
!
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
!
      IF(NREPL.GT.0)THEN
        NRESP=1
        IADD=NLABID+NRESP
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
      ITEXT(ICNT)='H0: The most extreme point is not'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    an outlier'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      AVALUE(ICNT)=0.0
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The most extreme point is not'
      NCTEXT(ICNT)=33
      IDIGIT(ICNT)=-1
      AVALUE(ICNT)=0.0
      ICNT=ICNT+1
      ITEXT(ICNT)='    an outlier'
      NCTEXT(ICNT)=14
      IDIGIT(ICNT)=-1
      AVALUE(ICNT)=0.0
!
      IINDX=INT(YIND+0.1)
      YEXT=Y(IINDX)
      ICNT=ICNT+1
      ITEXT(ICNT)='Potential outlier value tested:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=YEXT
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='ID for potential outlier:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=X(INT(YIND))
      IDIGIT(ICNT)=0
!
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
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
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
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample SD:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=YSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Kurtosis:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=YKURT
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Kurtosis Outlier Test Statistic Value:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
!
!CCCC NOTE: CDF AND P-VALUE ONLY PRINTED IF CRITICAL
!CCCC       VALUES DETERMINED FROM SIMUMLATION
!
      IF(IKUOT2.EQ.'SIMU')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='CDF Value:'
        NCTEXT(ICNT)=10
        AVALUE(ICNT)=STATCD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='P-Value:'
        NCTEXT(ICNT)=7
        AVALUE(ICNT)=PVAL
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
      NUMROW=ICNT
      DO 4210 I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      NUMLIN=1
      NUMROW=NCDF
      NUMCOL=5
      ITITL2(1,1)='Alpha'
      ITITL2(1,2)='CDF'
      ITITL2(1,3)='Statistic'
      ITITL2(1,4)='Critical Value'
      ITITL2(1,5)='Conclusion'
      NCTIT2(1,1)=5
      NCTIT2(1,2)=3
      NCTIT2(1,3)=9
      NCTIT2(1,4)=14
      NCTIT2(1,5)=10
!
      NMAX=0
      DO 4321 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
        IF(I.EQ.4)NTOT(I)=17
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=3
        ITYPCO(I)='ALPH'
 4321 CONTINUE
      ITYPCO(3)='NUME'
      ITYPCO(4)='NUME'
      IDIGIT(1)=0
      IDIGIT(2)=0
      DO 4323 I=1,NUMROW
        DO 4325 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
 4325   CONTINUE
 4323 CONTINUE
      IF(IKUOT2.EQ.'ASTM')THEN
        IVALUE(1,1)='10%'
        IVALUE(2,1)='5%'
        IVALUE(3,1)='1%'
        IVALUE(1,2)='90%'
        IVALUE(2,2)='95%'
        IVALUE(3,2)='99%'
        NCVALU(1,1)=3
        NCVALU(2,1)=2
        NCVALU(3,1)=2
        NCVALU(1,2)=3
        NCVALU(2,2)=3
        NCVALU(3,2)=3
      ELSE
        IVALUE(1,1)='20%'
        IVALUE(2,1)='10%'
        IVALUE(3,1)='5%'
        IVALUE(4,1)='2.5%'
        IVALUE(5,1)='1%'
        IVALUE(6,1)='0.5%'
        IVALUE(1,2)='80%'
        IVALUE(2,2)='90%'
        IVALUE(3,2)='95%'
        IVALUE(4,2)='97.5%'
        IVALUE(5,2)='99%'
        IVALUE(6,2)='99.5%'
        NCVALU(1,1)=3
        NCVALU(2,1)=3
        NCVALU(3,1)=2
        NCVALU(4,1)=4
        NCVALU(5,1)=2
        NCVALU(6,1)=4
        NCVALU(1,2)=3
        NCVALU(2,2)=3
        NCVALU(3,2)=3
        NCVALU(4,2)=5
        NCVALU(5,2)=3
        NCVALU(6,2)=5
      ENDIF
      IVALUE(1,5)='Accept H0'
      IVALUE(2,5)='Accept H0'
      IVALUE(3,5)='Accept H0'
      IVALUE(4,5)='Accept H0'
      IVALUE(5,5)='Accept H0'
      IVALUE(6,5)='Accept H0'
      NCVALU(1,5)=9
      NCVALU(2,5)=9
      NCVALU(3,5)=9
      NCVALU(4,5)=9
      NCVALU(5,5)=9
      NCVALU(6,5)=9
      DO 4410 KK=1,NCDF
        AMAT(KK,3)=STATVA
        IF(STATVA.GT.CV(KK))IVALUE(KK,5)='Reject H0'
!CCC    AMAT(KK,4)=RND(CV(KK),IDIGIT(3))
        AMAT(KK,4)=CV(KK)
 4410 CONTINUE
!
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWRTF(1)=1500
      IWRTF(2)=IWRTF(1)+1500
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(3)+2000
      IWRTF(5)=IWRTF(4)+2000
      IFRST=.FALSE.
      ILAST=.TRUE.
!
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(IKUOT2.EQ.'SIMU')THEN
        ITITLE='Critical Values Based on 50,000 Simulations'
        NCTEMP=43
      ELSEIF(IKUOT2.EQ.'ASTM')THEN
        ITITLE='Critical Values Based on ASTM E-178 Tables'
        NCTEMP=42
      ENDIF
      IRTFMD='OFF'
      IFNTSZ=-1
      IFLAGA=.TRUE.
      IFLAGB=.TRUE.
      ISIZE=-1
      NTOTAL=NCTEMP
      NBLNK1=2
      NBLNK2=1
      ITYPE=2
      AVAL=CPUMIN
      CALL DPDTXT(ITITLE,NCTEMP,AVAL,NUMDIG,   &
                  NTOTAL,NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,   &
                  ISUBRO,IBUGA3,IERROR)
      ISIZE=-99
      IFNTSZ=0
!
      ISTEPN='42F'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KUO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKUO2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)IERROR,STATVA,STATCD,PVAL
 9013   FORMAT('IERROR,STATVA,STATCD,PVAL = ',A4,2X,3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPKUO2
      SUBROUTINE DPKUO3(X,N,TEMP1,TEMP2,IWRITE,PSTAMV,   &
                        MAXNXT,IKUOTA,ISEED,   &
                        ALPHA,CV,NALPHA,   &
                        STATVA,XMEAN,XSD,XMIN,XMAX,XKURT,   &
                        PVAL,STATCD,XIND,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES AN OUTLIER TEST BASED ON THE
!              SAMPLE KURTOSIS.  IF THE SAMPLE KURTOSIS IS ABOVE THE
!              CRITICAL VALUE, ASSUME THE OBSERVATION FURTHERST FROM
!              THE MEAN IS AN OUTLIER.  THIS TEST CAN BE REPEATED BY
!              REMOVING THE OUTLIER AND REPEATING THE TEST UNTIL THE
!              TEST INDICATES NO OUTLIER.  THIS TEST ASSUMES NORMALITY.
!              THIS ROUTINE ONLY TESTS FOR A SINGLE OUTLIER.  FOR
!              MULTIPLE OUTLIERS, THE USER SHOULD DELETE THE CURRENT
!              OUTLIER AND REPEAT THE TEST.
!
!              THIS TEST WAS ADDED TO SUPPORT THE ASTM E-178 STANDARD
!              (2016 EDITION).
!
!              CRITICAL VALUES CAN BE DETERMINED IN THE FOLLOWING
!              WAYS:
!
!                1. TABLES FROM ASTM E178 - 16a
!                2. SIMULATION
!
!     REFERENCE--E178 - 16A (2016), "Standard Practice for Dealing with
!                Outlying Observations", ASTM International, 100 Barr
!                Harbor Drive, PO BOX C700, West Conshohocken, PA
!                19428-2959, USA.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED DAVID STATISTIC.
!                     --STATCD = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE TEST STATISTIC.
!                     --PVAL   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED P-VALUE OF THE TEST STATISTIC.
!                     --XIND   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED INDEX OF THE POTENTIAL
!                                OUTLIER.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE STATISTIC.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, MEAN, SD.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
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
!     VERSION NUMBER--2019.10
!     ORIGINAL VERSION--OCTOBER   2019.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IKUOTA
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IKUOT2
      CHARACTER*4 IOP
      CHARACTER*4 IDIR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION ALPHA(*)
      DIMENSION CV(*)
!
      REAL LININ3
      EXTERNAL LININ3
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DCORR
      DOUBLE PRECISION DFACT
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DENOM
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPKU'
      ISUBN2='O3  '
      IERROR='NO'
      IKUOT2=IKUOTA
      IF(IKUOTA.EQ.'ASTM')THEN
        IF(N.GT.50)IKUOT2='SIMU'
        IF(NALPHA.EQ.1)THEN
          ALPT=ALPHA(1)
          IF(ALPT.GT.1.0 .AND. ALPT.LT.100.0)ALPT=ALPT/100.0
          IF(ALPT.LT.0.5)ALPT=1.0 - ALPT
          IF(ALPT.NE.0.90 .AND. ALPT.NE.0.95 .AND.   &
             ALPT.NE.0.99)IKUOT2='SIMU'
        ENDIF
      ENDIF
!
      STATVA=CPUMIN
      XSD=CPUMIN
      XMEAN=CPUMIN
      XMIN=CPUMIN
      XMAX=CPUMIN
      XIND=CPUMIN
      XKURT=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
      XIND=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KUO3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPKUO3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IKUOTA,IKUOT2,ISEED,N
   52   FORMAT('IBUGA3,ISUBRO,IKUOTA,IKUOT2,ISEED,N = ',4(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NALPHA,ALPHA(1)
   54   FORMAT('NALPHA,ALPHA(1) = ',I5,F10.5)
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
      IF(N.LT.4)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN KURTOSIS OUTLIER TEST STATISTIC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE MUST BE AT LEAST 4.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ******************************************
!               **  STEP 2--                            **
!               **  COMPUTE THE SKEW OUTLIER STATISTIC. **
!               ******************************************
!
      IWRITE='OFF'
      CALL SORT(X,N,TEMP1)
      XMIN=TEMP1(1)
      XMAX=TEMP1(N)
      CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
      CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
!
!     USE DEFINITION OF KURTOSIS GIVEN IN ASTM E-178
!
!         N*(N+1)*SUM[(X(i)-XBAR)**2]/((N-1)*(N-2)*(N-3)*S**4) -
!         3*(N-1)**2/((N-2)*(N-3))
!
      DN=DBLE(N)
      DCORR=3.0D0*(DN - 1.0D0)**2/((DN - 2.0D0)*(DN - 3.0D0))
      DFACT=DN*(DN + 1.0D0)/((DN - 1.0D0)*(DN - 2.0D0)*(DN - 3.D0))
!
      DENOM=DBLE(XSD)**4
      DSUM1=0.0D0
      DO 210 II=1,N
        DSUM1=DSUM1 + (DBLE(X(II)) - DBLE(XMEAN))**4
  210 CONTINUE
      XKURT=REAL((DFACT*DSUM1/DENOM) - DCORR)
      STATVA=XKURT
!
      CALL MAXIND(X,N,IWRITE,PSTAMV,XINDMX,ISUBRO,IBUGA3,IERROR)
      CALL MININD(X,N,IWRITE,PSTAMV,XINDMN,ISUBRO,IBUGA3,IERROR)
      D1=XMEAN - XMIN
      D2=XMAX - XMEAN
      IF(D1.GT.D2)THEN
        XIND=XINDMN
      ELSE
        XIND=XINDMX
      ENDIF
!
!               *****************************************
!               **  STEP 3--                           **
!               **  COMPUTE THE CRITICAL VALUES        **
!               *****************************************
!
      IF(IKUOT2.EQ.'SIMU')THEN
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
!
        NSIM=50000
        DO 3200 II=1,NSIM
          CALL NORRAN(N,ISEED,TEMP2)
          CALL MEAN(TEMP2,N,IWRITE,XMEAN2,IBUGA3,IERROR)
          CALL SD(TEMP2,N,IWRITE,XSD2,IBUGA3,IERROR)
          DENOM=DBLE(XSD2)**4
          DSUM1=0.0D0
          DO 3201 JJ=1,N
            DSUM1=DSUM1 + (DBLE(TEMP2(JJ)) - DBLE(XMEAN2))**4
 3201     CONTINUE
          TEMP1(II)=REAL((DFACT*DSUM1/DENOM) - DCORR)
          WRITE(IOUNI2,'(E15.7)')TEMP1(II)
 3200   CONTINUE
!
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        CALL SORT(TEMP1,NSIM,TEMP1)
!
        DO 3210 II=1,NALPHA
          ALPT=ALPHA(II)
          IF(ALPT.GT.1.0 .AND. ALPT.LT.100.0)ALPT=ALPT/100.0
          IF(ALPT.LE.0.0 .OR. ALPT.GT.1.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3111)ALPHA(II)
 3111       FORMAT('      INVALID VALUE OF ALPHA (',G15.7,'),')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
          IF(ALPT.LT.0.5)ALPT=1.0 - ALPT
          P100=100.0*ALPT
          CALL PERCEN(P100,TEMP1,NSIM,IWRITE,TEMP2,MAXNXT,   &
                      XPERC,IBUGA3,IERROR)
          CV(II)=XPERC
 3210   CONTINUE
        IDIR='UPPE'
        CALL DPGOF8(TEMP1,NSIM,STATVA,PVAL,IDIR,IBUGA3,ISUBRO,IERROR)
        STATCD=1.0 - PVAL
      ELSEIF(IKUOT2.EQ.'ASTM')THEN
        DO 3300 II=1,NALPHA
          ALPT=ALPHA(II)
          IF(ALPT.GT.1.0 .AND. ALPT.LT.100.0)ALPT=ALPT/100.0
          IF(ALPT.LE.0.0 .OR. ALPT.GT.1.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3111)ALPHA(II)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
          IF(ALPT.LT.0.5)ALPT=1.0 - ALPT
          IF(ALPT.EQ.0.90)THEN
            IF(N.EQ.4)THEN
              CV(II)=3.075
            ELSEIF(N.EQ.5)THEN
              CV(II)=2.772
            ELSEIF(N.EQ.6)THEN
              CV(II)=2.482
            ELSEIF(N.EQ.7)THEN
              CV(II)=2.257
            ELSEIF(N.EQ.8)THEN
              CV(II)=2.067
            ELSEIF(N.EQ.9)THEN
              CV(II)=1.904
            ELSEIF(N.EQ.10)THEN
              CV(II)=1.778
            ELSEIF(N.EQ.11)THEN
              CV(II)=1.678
            ELSEIF(N.EQ.12)THEN
              CV(II)=1.597
            ELSEIF(N.EQ.13)THEN
              CV(II)=1.529
            ELSEIF(N.EQ.14)THEN
              CV(II)=1.471
            ELSEIF(N.EQ.15)THEN
              CV(II)=1.422
            ELSEIF(N.EQ.16)THEN
              CV(II)=1.378
            ELSEIF(N.EQ.17)THEN
              CV(II)=1.340
            ELSEIF(N.EQ.18)THEN
              CV(II)=1.303
            ELSEIF(N.EQ.19)THEN
              CV(II)=1.271
            ELSEIF(N.EQ.20)THEN
              CV(II)=1.243
            ELSEIF(N.EQ.21)THEN
              CV(II)=1.214
            ELSEIF(N.EQ.22)THEN
              CV(II)=1.188
            ELSEIF(N.EQ.23)THEN
              CV(II)=1.167
            ELSEIF(N.EQ.24)THEN
              CV(II)=1.143
            ELSEIF(N.EQ.25)THEN
              CV(II)=1.123
            ELSEIF(N.EQ.26)THEN
              CV(II)=1.102
            ELSEIF(N.EQ.27)THEN
              CV(II)=1.085
            ELSEIF(N.EQ.28)THEN
              CV(II)=1.066
            ELSEIF(N.EQ.29)THEN
              CV(II)=1.052
            ELSEIF(N.EQ.30)THEN
              CV(II)=1.035
            ELSEIF(N.GE.31 .AND. N.LE.34)THEN
              X1=30.
              X2=35.
              X3=REAL(N)
              Y1=1.035
              Y2=0.969
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.35)THEN
              CV(II)=0.969
            ELSEIF(N.GE.36 .AND. N.LE.39)THEN
              X1=35.
              X2=40.
              X3=REAL(N)
              Y1=0.969
              Y2=0.913
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.40)THEN
              CV(II)=0.913
            ELSEIF(N.GE.41 .AND. N.LE.44)THEN
              X1=40.
              X2=45.
              X3=REAL(N)
              Y1=0.913
              Y2=0.867
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.45)THEN
              CV(II)=0.867
            ELSEIF(N.GE.46 .AND. N.LE.49)THEN
              X1=45.
              X2=50.
              X3=REAL(N)
              Y1=0.867
              Y2=0.830
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.50)THEN
              CV(II)=0.830
            ENDIF
          ELSEIF(ALPT.EQ.0.95)THEN
            IF(N.EQ.4)THEN
              CV(II)=3.518
            ELSEIF(N.EQ.5)THEN
              CV(II)=3.506
            ELSEIF(N.EQ.6)THEN
              CV(II)=3.319
            ELSEIF(N.EQ.7)THEN
              CV(II)=3.110
            ELSEIF(N.EQ.8)THEN
              CV(II)=2.935
            ELSEIF(N.EQ.9)THEN
              CV(II)=2.772
            ELSEIF(N.EQ.10)THEN
              CV(II)=2.627
            ELSEIF(N.EQ.11)THEN
              CV(II)=2.505
            ELSEIF(N.EQ.12)THEN
              CV(II)=2.399
            ELSEIF(N.EQ.13)THEN
              CV(II)=2.300
            ELSEIF(N.EQ.14)THEN
              CV(II)=2.217
            ELSEIF(N.EQ.15)THEN
              CV(II)=2.145
            ELSEIF(N.EQ.16)THEN
              CV(II)=2.081
            ELSEIF(N.EQ.17)THEN
              CV(II)=2.021
            ELSEIF(N.EQ.18)THEN
              CV(II)=1.966
            ELSEIF(N.EQ.19)THEN
              CV(II)=1.921
            ELSEIF(N.EQ.20)THEN
              CV(II)=1.873
            ELSEIF(N.EQ.21)THEN
              CV(II)=1.831
            ELSEIF(N.EQ.22)THEN
              CV(II)=1.788
            ELSEIF(N.EQ.23)THEN
              CV(II)=1.757
            ELSEIF(N.EQ.24)THEN
              CV(II)=1.719
            ELSEIF(N.EQ.25)THEN
              CV(II)=1.690
            ELSEIF(N.EQ.26)THEN
              CV(II)=1.658
            ELSEIF(N.EQ.27)THEN
              CV(II)=1.630
            ELSEIF(N.EQ.28)THEN
              CV(II)=1.601
            ELSEIF(N.EQ.29)THEN
              CV(II)=1.578
            ELSEIF(N.EQ.30)THEN
              CV(II)=1.550
            ELSEIF(N.GE.31 .AND. N.LE.34)THEN
              X1=30.
              X2=35.
              X3=REAL(N)
              Y1=1.550
              Y2=1.446
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.35)THEN
              CV(II)=1.446
            ELSEIF(N.GE.36 .AND. N.LE.39)THEN
              X1=35.
              X2=40.
              X3=REAL(N)
              Y1=1.446
              Y2=1.358
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.40)THEN
              CV(II)=1.358
            ELSEIF(N.GE.41 .AND. N.LE.44)THEN
              X1=40.
              X2=45.
              X3=REAL(N)
              Y1=1.358
              Y2=1.285
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.45)THEN
              CV(II)=1.285
            ELSEIF(N.GE.46 .AND. N.LE.49)THEN
              X1=45.
              X2=50.
              X3=REAL(N)
              Y1=1.285
              Y2=1.223
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.50)THEN
              CV(II)=1.223
            ENDIF
          ELSEIF(ALPT.EQ.0.99)THEN
            IF(N.EQ.4)THEN
              CV(II)=3.900
            ELSEIF(N.EQ.5)THEN
              CV(II)=4.454
            ELSEIF(N.EQ.6)THEN
              CV(II)=4.685
            ELSEIF(N.EQ.7)THEN
              CV(II)=4.735
            ELSEIF(N.EQ.8)THEN
              CV(II)=4.687
            ELSEIF(N.EQ.9)THEN
              CV(II)=4.586
            ELSEIF(N.EQ.10)THEN
              CV(II)=4.467
            ELSEIF(N.EQ.11)THEN
              CV(II)=4.350
            ELSEIF(N.EQ.12)THEN
              CV(II)=4.234
            ELSEIF(N.EQ.13)THEN
              CV(II)=4.106
            ELSEIF(N.EQ.14)THEN
              CV(II)=4.000
            ELSEIF(N.EQ.15)THEN
              CV(II)=3.887
            ELSEIF(N.EQ.16)THEN
              CV(II)=3.784
            ELSEIF(N.EQ.17)THEN
              CV(II)=3.702
            ELSEIF(N.EQ.18)THEN
              CV(II)=3.605
            ELSEIF(N.EQ.19)THEN
              CV(II)=3.524
            ELSEIF(N.EQ.20)THEN
              CV(II)=3.450
            ELSEIF(N.EQ.21)THEN
              CV(II)=3.370
            ELSEIF(N.EQ.22)THEN
              CV(II)=3.298
            ELSEIF(N.EQ.23)THEN
              CV(II)=3.233
            ELSEIF(N.EQ.24)THEN
              CV(II)=3.169
            ELSEIF(N.EQ.25)THEN
              CV(II)=3.116
            ELSEIF(N.EQ.26)THEN
              CV(II)=3.051
            ELSEIF(N.EQ.27)THEN
              CV(II)=2.995
            ELSEIF(N.EQ.28)THEN
              CV(II)=2.943
            ELSEIF(N.EQ.29)THEN
              CV(II)=2.903
            ELSEIF(N.EQ.30)THEN
              CV(II)=2.845
            ELSEIF(N.GE.31 .AND. N.LE.34)THEN
              X1=30.
              X2=35.
              X3=REAL(N)
              Y1=2.845
              Y2=2.642
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.35)THEN
              CV(II)=2.642
            ELSEIF(N.GE.36 .AND. N.LE.39)THEN
              X1=35.
              X2=40.
              X3=REAL(N)
              Y1=2.642
              Y2=2.470
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.40)THEN
              CV(II)=2.470
            ELSEIF(N.GE.41 .AND. N.LE.44)THEN
              X1=40.
              X2=45.
              X3=REAL(N)
              Y1=2.470
              Y2=2.322
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.45)THEN
              CV(II)=2.322
            ELSEIF(N.GE.46 .AND. N.LE.49)THEN
              X1=45.
              X2=50.
              X3=REAL(N)
              Y1=2.322
              Y2=2.210
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.50)THEN
              CV(II)=2.210
            ENDIF
          ELSE
            CV(II)=CPUMIN
          ENDIF
          IF(CV(II).NE.CPUMIN)CV(II)=CV(II)
 3300   CONTINUE
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
        WRITE(ICOUT,811)N,STATVA
  811   FORMAT('THE VALUE OF THE KURTOSIS OUTLIER STATISTIC OF THE ',   &
               I8,' OBSERVATIONS = ',G15.7)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'KUO3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPKUO3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N
 9013   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)XMIN,XMAX,XMEAN,XSD,XKURT
 9015   FORMAT('XMIN,XMAX,XMEAN,XSD,XKURT = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)STATVA,CDF,PVAL,XIND
 9016   FORMAT('STATVA,CDF,PVAL,XIND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPKUO3

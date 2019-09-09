      SUBROUTINE DPTIQP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A TRUNCATED INFORMATIVE QUANTILE
C              (TIQ) PLOT.
C     EXAMPLES--NORMAL TIQ PLOT Y
C               LOGNORMAL TIQ PLOT Y
C               UNIFORM TIQ PLOT Y
C               GUMBEL TIQ PLOT Y
C               WEIBULL TIQ PLOT Y
C               LOGISTIC TIQ PLOT Y
C               DOUBLE EXPONENTIAL TIQ PLOT Y
C               CAUCHY TIQ PLOT Y
C               SEMICIRCULAR TIQ PLOT Y
C               COSINE TIQ PLOT Y
C               ANGLIT TIQ PLOT Y
C               HYPERBOLIC SECANT TIQ PLOT Y
C               HALF-NORMAL TIQ PLOT Y
C               ARCSINE TIQ PLOT Y
C               EXPONENTIAL TIQ PLOT Y
C               HALF-CAUCHY TIQ PLOT Y
C               SLASH TIQ PLOT Y
C               RAYLEIGH TIQ PLOT Y
C               MAXWELL TIQ PLOT Y
C
C     REFERENCE--"MIL-HDBK-17-1F Volume 1: Guidelines for Characterization
C                of Structural Materials", Depeartment of Defense,
C                chapter 8, 2002.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2017/03
C     ORIGINAL VERSION--MARCH     2017.
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
      CHARACTER*4 ICASE2
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
C
      DIMENSION Y1(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP4(1))
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
      INCLUDE 'DPCOS2.INC'
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
      ISUBN1='DPTI'
      ISUBN2='QP  '
      ICASPL='TIQP'
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
C               **  TREAT THE TIQ            PLOT                **
C               ***************************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTIQP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  EXTRACT THE COMMAND                             **
C               **  REPLICATION AND MULTIPLE NOT SUPPORTED FOR THIS **
C               **  COMMAND.                                        **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C

      IF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1   IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='NORM'
      ELSEIF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='NORM'
      ELSEIF(ICOM.EQ.'LOGN' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='LOGN'
      ELSEIF(ICOM.EQ.'LOGN' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='LOGN'
      ELSEIF(ICOM.EQ.'WEIB' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='WEIB'
      ELSEIF(ICOM.EQ.'WEIB' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='WEIB'
      ELSEIF(ICOM.EQ.'GUMB' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='GUMB'
      ELSEIF(ICOM.EQ.'GUMB' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='GUMB'
      ELSEIF(ICOM.EQ.'EXTR' .AND. IHARG(1).EQ.'VALU' .AND.
     1       IHARG(2).EQ.'TYPE' .AND.
     1      (IHARG(3).EQ.'1   ' .OR. IHARG(3).EQ.'I   ')  .AND.
     1       IHARG(4).EQ.'TIQ ' .AND.
     1       IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='GUMB'
      ELSEIF(ICOM.EQ.'EXTR' .AND. IHARG(1).EQ.'VALU' .AND.
     1       IHARG(2).EQ.'TYPE' .AND.
     1      (IHARG(3).EQ.'1   ' .OR.  IHARG(3).EQ.'I   ')  .AND.
     1       IHARG(4).EQ.'TRUN' .AND. IHARG(5).EQ.'INFO' .AND.
     1       IHARG(6).EQ.'QUAN' .AND. IHARG(7).EQ.'PLOT')THEN
         ILASTC=7
         ICASE='GUMB'
      ELSEIF(ICOM.EQ.'UNIF' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='UNIF'
      ELSEIF(ICOM.EQ.'UNIF' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='UNIF'
      ELSEIF(ICOM.EQ.'LOGI' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='LOGI'
      ELSEIF(ICOM.EQ.'LOGI' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='LOGI'
      ELSEIF(ICOM.EQ.'EXPO' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='EXPO'
      ELSEIF(ICOM.EQ.'EXPO' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='EXPO'
      ELSEIF(ICOM.EQ.'ARCS' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='ARCS'
      ELSEIF(ICOM.EQ.'ARCS' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='ARCS'
      ELSEIF(ICOM.EQ.'ANGL' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='ANGL'
      ELSEIF(ICOM.EQ.'ANGL' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='ANGL'
      ELSEIF(ICOM.EQ.'COSI' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='COSI'
      ELSEIF(ICOM.EQ.'COSI' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='COSI'
      ELSEIF(ICOM.EQ.'CAUC' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='CAUC'
      ELSEIF(ICOM.EQ.'CAUC' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='CAUC'
      ELSEIF(ICOM.EQ.'SLAS' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='SLAS'
      ELSEIF(ICOM.EQ.'SLAS' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='SLAS'
      ELSEIF(ICOM.EQ.'SLAS' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='SLAS'
      ELSEIF(ICOM.EQ.'SLAS' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='SLAS'
      ELSEIF(ICOM.EQ.'RAYL' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='RAYL'
      ELSEIF(ICOM.EQ.'RAYL' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='RAYL'
      ELSEIF(ICOM.EQ.'MAXW' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='MAXW'
      ELSEIF(ICOM.EQ.'MAXW' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='MAXW'
      ELSEIF(ICOM.EQ.'HALF' .AND. IHARG(1).EQ.'NORM' .AND.
     1       IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='HANO'
      ELSEIF(ICOM.EQ.'HALF' .AND. IHARG(1).EQ.'NORM' .AND.
     1       IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.
     1       IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='HANO'
      ELSEIF(ICOM.EQ.'HALF' .AND. IHARG(1).EQ.'CAUC' .AND.
     1       IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='HACA'
      ELSEIF(ICOM.EQ.'HALF' .AND. IHARG(1).EQ.'CAUC' .AND.
     1       IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.
     1       IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='HACO'
      ELSEIF(ICOM.EQ.'HYPE' .AND. IHARG(1).EQ.'SECA' .AND.
     1       IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='HSE '
      ELSEIF(ICOM.EQ.'HYPE' .AND. IHARG(1).EQ.'SECA' .AND.
     1       IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.
     1       IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='HSE '
      ELSEIF(ICOM.EQ.'LAPL' .AND. IHARG(1).EQ.'TIQ ' .AND.
     1       IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='DEX '
      ELSEIF(ICOM.EQ.'LAPL' .AND. IHARG(1).EQ.'TRUN' .AND.
     1       IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.
     1       IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='DEX '
      ELSEIF(ICOM.EQ.'DOUB' .AND. IHARG(1).EQ.'EXPO' .AND.
     1       IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='DEX '
      ELSEIF(ICOM.EQ.'DOUB' .AND. IHARG(1).EQ.'EXPO' .AND.
     1       IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.
     1       IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='DEX '
      ELSEIF(ICOM.EQ.'SEMI' .AND. IHARG(1).EQ.'CIRC' .AND.
     1       IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='SEMC'
      ELSEIF(ICOM.EQ.'SEMI' .AND. IHARG(1).EQ.'CIRC' .AND.
     1       IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.
     1       IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='SEMC'
      ELSE
        GOTO9000
      ENDIF
C
      IFOUND='YES'
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'TIQP')THEN
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='TRUNCATED INFORMATIVE QUANTILE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=5
      IFLAGE=1
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=1
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')THEN
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
C               ********************************************
C               **  STEP 6--                              **
C               **  GENERATE THE TIQ            PLOTS FOR **
C               **  THE VARIOUS CASES.                    **
C               ********************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************
C               **  STEP 8A--                           **
C               ******************************************
C
      ISTEPN='8A'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NPLOTP=0
      ICOL=1
      NUMVA2=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,TEMP1,TEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE2,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 8B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               *****************************************************
C
      CALL DPTIQ2(Y1,TEMP1,TEMP2,TEMP3,TEMP4,NLOCAL,
     1            ICASE,MINMAX,
     1            Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTIQP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASPL,IAND1,IAND2
 9012   FORMAT('IFOUND,IERROR,ICASPL,IAND1,IAND2 = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NLOCAL
 9013   FORMAT('NPLOTV,NPLOTP,NLOCAL = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
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
      SUBROUTINE DPTIQ2(Y,AIQHAT,TIQHAT,UTEMP,QUHAT,N,
     1                  ICASPL,MINMAX,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A TRUNCATED INFORMATIVE QUANTILE
C              (TIQ) PLOT.
C
C     REFERENCE--"MIL-HDBK-17-1F Volume 1: Guidelines for Characterization
C                of Structural Materials", Depeartment of Defense,
C                chapter 8, 2002.
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
C     VERSION NUMBER--2017/03
C     ORIGINAL VERSION--MARCH     2017.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRITE
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION UTEMP(*)
      DIMENSION AIQHAT(*)
      DIMENSION TIQHAT(*)
      DIMENSION QUHAT(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      INCLUDE 'DPCOST.INC'
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
      ISUBN1='DPTI'
      ISUBN2='Q2  '
      IWRITE='OFF '
      IERROR='NO'
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TIQ2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPTIQ2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,MINMAX,ICASPL
   72   FORMAT('N,MINMAX,ICASPL = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,N
          WRITE(ICOUT,74)I,Y(I)
   74     FORMAT('I, Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN TRUNCATED INFORMATIVE QUANTILE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 5;')
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
C               **********************************************
C               **  STEP 2--                                **
C               **  FOR WEIBULL AND LOGNORMAL, NEED TO TAKE **
C               **  LOG OF THE DATA.                        **
C               **********************************************
C
      IF(ICASPL.EQ.'WEIB' .OR. ICASPL.EQ.'LOGN')THEN
        DO200I=1,N
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,202)I
  202       FORMAT('      ROW ',I8,' OF THE RESPONSE VARIABLE IS ',
     1             'NON-POSITIVE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,204)Y(I)
  204       FORMAT('      IT HAS THE VALUE ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          Y(I)=LOG(Y(I))
  200   CONTINUE
      ENDIF
C
C               **********************************************
C               **  STEP 3--                                **
C               **  CALL EMPQUA ROUTINE TO COMPUTE THE      **
C               **  TRUNCATED INFORMATIVE QUANTILE FUNCTION **
C               **********************************************
C
C
      CALL EMPTIQ(Y,N,IWRITE,AIQHAT,TIQHAT,UTEMP,QUHAT,NOUT,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      DO310I=1,NOUT
        N2=N2+1
        Y2(N2)=TIQHAT(I)
        X2(N2)=100.0*UTEMP(I)
        D2(N2)=1.0
  310 CONTINUE
C
C               **********************************************
C               **  STEP 4--                                **
C               **  NOW COMPUTE THE TRUNCATED INFORMATIVE   **
C               **  QUANTILE FUNCTION FOR A THEORETICAL     **
C               **  DISTRIBUTION.                           **
C               **********************************************
C
C
      U25=0.25
      U50=0.50
      U75=0.75
      PINC=0.01
C
      IF(ICASPL.EQ.'NORM' .OR. ICASPL.EQ.'LOGN')THEN
        CALL NORPPF(U25,QU25)
        CALL NORPPF(U50,QU50)
        CALL NORPPF(U75,QU75)
        PVAL=0.01
        DO401I=1,99
          CALL NORPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  401   CONTINUE
      ELSEIF(ICASPL.EQ.'UNIF')THEN
        CALL UNIPPF(U25,QU25)
        CALL UNIPPF(U50,QU50)
        CALL UNIPPF(U75,QU75)
        PVAL=0.01
        DO403I=1,99
          CALL UNIPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  403   CONTINUE
      ELSEIF(ICASPL.EQ.'GUMB' .OR. ICASPL.EQ.'WEIB')THEN
        MINMX2=MINMAX
        IF(ICASPL.EQ.'WEIB')MINMX2=1
        CALL EV1PPF(U25,MINMX2,QU25)
        CALL EV1PPF(U50,MINMX2,QU50)
        CALL EV1PPF(U75,MINMX2,QU75)
        PVAL=0.01
        DO405I=1,99
          CALL EV1PPF(PVAL,MINMX2,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  405   CONTINUE
      ELSEIF(ICASPL.EQ.'LOGI')THEN
        CALL LOGPPF(U25,QU25)
        CALL LOGPPF(U50,QU50)
        CALL LOGPPF(U75,QU75)
        PVAL=0.01
        DO411I=1,99
          CALL LOGPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  411   CONTINUE
      ELSEIF(ICASPL.EQ.'DEX ')THEN
        CALL DEXPPF(U25,QU25)
        CALL DEXPPF(U50,QU50)
        CALL DEXPPF(U75,QU75)
        PVAL=0.01
        DO413I=1,99
          CALL DEXPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  413   CONTINUE
      ELSEIF(ICASPL.EQ.'CAUC')THEN
        CALL CAUPPF(U25,QU25)
        CALL CAUPPF(U50,QU50)
        CALL CAUPPF(U75,QU75)
        PVAL=0.01
        DO415I=1,99
          CALL CAUPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  415   CONTINUE
      ELSEIF(ICASPL.EQ.'SEMC')THEN
        R=1.0
        CALL SEMPPF(U25,R,QU25)
        CALL SEMPPF(U50,R,QU50)
        CALL SEMPPF(U75,R,QU75)
        PVAL=0.01
        DO417I=1,99
          CALL SEMPPF(PVAL,R,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  417   CONTINUE
      ELSEIF(ICASPL.EQ.'COSI')THEN
        CALL COSPPF(U25,QU25)
        CALL COSPPF(U50,QU50)
        CALL COSPPF(U75,QU75)
        PVAL=0.01
        DO419I=1,99
          CALL COSPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  419   CONTINUE
      ELSEIF(ICASPL.EQ.'ANGL')THEN
        CALL ANGPPF(U25,QU25)
        CALL ANGPPF(U50,QU50)
        CALL ANGPPF(U75,QU75)
        PVAL=0.01
        DO421I=1,99
          CALL ANGPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  421   CONTINUE
      ELSEIF(ICASPL.EQ.'HSE ')THEN
        CALL HSEPPF(U25,QU25)
        CALL HSEPPF(U50,QU50)
        CALL HSEPPF(U75,QU75)
        PVAL=0.01
        DO423I=1,99
          CALL HSEPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  423   CONTINUE
      ELSEIF(ICASPL.EQ.'HANO')THEN
        CALL HFNPPF(U25,QU25)
        CALL HFNPPF(U50,QU50)
        CALL HFNPPF(U75,QU75)
        PVAL=0.01
        DO425I=1,99
          CALL HFNPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  425   CONTINUE
      ELSEIF(ICASPL.EQ.'ARCS')THEN
        print *,'at arcsine case'
        CALL ARSPPF(U25,QU25)
        CALL ARSPPF(U50,QU50)
        CALL ARSPPF(U75,QU75)
        PVAL=0.01
        DO427I=1,99
          CALL ARSPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  427   CONTINUE
      ELSEIF(ICASPL.EQ.'EXPO')THEN
        CALL EXPPPF(U25,QU25)
        CALL EXPPPF(U50,QU50)
        CALL EXPPPF(U75,QU75)
        PVAL=0.01
        DO429I=1,99
          CALL EXPPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.GT.1.0)AVAL=1.0
          IF(AVAL.LE.-1.0)AVAL=-1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  429   CONTINUE
      ELSEIF(ICASPL.EQ.'HACA')THEN
        CALL HFCPPF(U25,QU25)
        CALL HFCPPF(U50,QU50)
        CALL HFCPPF(U75,QU75)
        PVAL=0.01
        DO431I=1,99
          CALL HFCPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  431   CONTINUE
      ELSEIF(ICASPL.EQ.'SLAS')THEN
        CALL SLAPPF(U25,QU25)
        CALL SLAPPF(U50,QU50)
        CALL SLAPPF(U75,QU75)
        PVAL=0.01
        DO433I=1,99
          CALL SLAPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  433   CONTINUE
      ELSEIF(ICASPL.EQ.'RAYL')THEN
        CALL RAYPPF(U25,QU25)
        CALL RAYPPF(U50,QU50)
        CALL RAYPPF(U75,QU75)
        PVAL=0.01
        DO435I=1,99
          CALL RAYPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  435   CONTINUE
      ELSEIF(ICASPL.EQ.'MAXW')THEN
        CALL MAXPPF(U25,QU25)
        CALL MAXPPF(U50,QU50)
        CALL MAXPPF(U75,QU75)
        PVAL=0.01
        DO437I=1,99
          CALL MAXPPF(PVAL,PPF)
          AVAL=(PPF - QU50)/(2.0*(QU75-QU25))
          IF(AVAL.LE.-1.0)AVAL=-1.0
          IF(AVAL.GT.1.0)AVAL=1.0
          N2=N2+1
          Y2(N2)=AVAL
          X2(N2)=100.0*PVAL
          D2(N2)=2.0
          PVAL=PVAL + PINC
  437   CONTINUE
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
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TIQ2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTIQ2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,IERROR,N2
 9012   FORMAT('ICASPL,IERROR,N2 = ',2(A4,2X),I8)
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
      SUBROUTINE DPTISC(ICOM,IHARG,NUMARG,
     1IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC SCALES CONTAINED IN THE
C              4 VARIABLES IX1TSC,IX2TSC,IY1TSC,IY2TSC  .
C              SUCH TIC SCALE SWITCHES DEFINE THE SCALES
C              (LINEAR OR WEIBULL OR NORMAL)
C              FOR THE TICS ON THE 4 FRAME LINES OF A PLOT.
C     FOCUS OF SUBROUTINE DPTISC--LOG
C                         DPTIS2--WEIBULL
C                         DPTIS3--NORMAL
C
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
C                     --IX1TSC = LOWER HORIZONTAL TIC SCALE
C                     --IX2TSC = UPPER HORIZONTAL TIC SCALE
C                     --IY1TSC = LEFT  VERTICAL   TIC SCALE
C                     --IY2TSC = RIGHT VERTICAL   TIC SCALE
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
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
C
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
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORN')GOTO1900
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                           **
C               **  BOTH HORIZONTAL LOG SCALES  ARE TO BE LOG      **
C               *****************************************************
C
      IF(ICOM.EQ.'XLOG')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(NUMARG.LE.0)GOTO1110
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
      IERROR='YES'
      GOTO1900
C
 1110 CONTINUE
      IFOUND='YES'
      IX1TSC='LOG'
      IX2TSC='LOG'
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1115)
 1115 FORMAT('THE XLOG SWITCH (FOR BOTH HORIZONTAL LOG SCALES ) ',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO1900
C
 1120 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('THE XLOG SWITCH (FOR BOTH HORIZONTAL LOG SCALES ) ',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL FRAME LINE IS TO BE LOG      **
C               **************************************************************
C
      IF(ICOM.EQ.'X1LO')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(NUMARG.LE.0)GOTO1210
      IF(IHARG(NUMARG).EQ.'ON')GOTO1210
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1220
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1210
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1210
      IERROR='YES'
      GOTO1900
C
 1210 CONTINUE
      IFOUND='YES'
      IX1TSC='LOG'
C
      IF(IFEEDB.EQ.'OFF')GOTO1219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('THE X1LOG   SWITCH (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1219 CONTINUE
      GOTO1900
C
 1220 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1225)
 1225 FORMAT('THE X1LOG   SWITCH (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL FRAME LINE IS TO BE LOG      **
C               **************************************************************
C
      IF(ICOM.EQ.'X2LO')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(NUMARG.LE.0)GOTO1310
      IF(IHARG(NUMARG).EQ.'ON')GOTO1310
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1320
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1310
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1310
      IERROR='YES'
      GOTO1900
C
 1310 CONTINUE
      IFOUND='YES'
      IX2TSC='LOG'
C
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1315)
 1315 FORMAT('THE X2LOG   SWITCH (FOR THE TOP HORIZONTAL ',
     1'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      GOTO1900
C
 1320 CONTINUE
      IFOUND='YES'
      IX2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1329
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1325)
 1325 FORMAT('THE X2LOG   SWITCH (FOR THE TOP HORIZONTAL ',
     1'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1329 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               ***************************************************
C               **  TREAT THE CASE WHEN                          **
C               **  BOTH VERTICAL LOG SCALES  ARE TO BE LOG      **
C               ***************************************************
C
      IF(ICOM.EQ.'YLOG')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(NUMARG.LE.0)GOTO1410
      IF(IHARG(NUMARG).EQ.'ON')GOTO1410
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1420
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1410
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1410
      IERROR='YES'
      GOTO1900
C
 1410 CONTINUE
      IFOUND='YES'
      IY1TSC='LOG'
      IY2TSC='LOG'
C
      IF(IFEEDB.EQ.'OFF')GOTO1419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1415)
 1415 FORMAT('THE YLOG   SWITCH (FOR BOTH VERTICAL LOG SCALES ) ',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1419 CONTINUE
      GOTO1900
C
 1420 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1425)
 1425 FORMAT('THE YLOG   SWITCH (FOR BOTH VERTICAL LOG SCALES ) ',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1429 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   FRAME LINE IS TO BE LOG      **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1LO')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(NUMARG.LE.0)GOTO1510
      IF(IHARG(NUMARG).EQ.'ON')GOTO1510
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1520
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1510
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1510
      IERROR='YES'
      GOTO1900
C
 1510 CONTINUE
      IFOUND='YES'
      IY1TSC='LOG'
C
      IF(IFEEDB.EQ.'OFF')GOTO1519
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1515)
 1515 FORMAT('THE Y1LOG   SWITCH (FOR THE LEFT VERTICAL ',
     1'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1519 CONTINUE
      GOTO1900
C
 1520 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1529
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1525)
 1525 FORMAT('THE Y1LOG   SWITCH (FOR THE LEFT VERTICAL ',
     1'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1529 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTCIAL   FRAME LINE IS TO BE LOG      **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2LO')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(NUMARG.LE.0)GOTO1610
      IF(IHARG(NUMARG).EQ.'ON')GOTO1610
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1620
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1610
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1610
      IERROR='YES'
      GOTO1900
C
 1610 CONTINUE
      IFOUND='YES'
      IY2TSC='LOG'
C
      IF(IFEEDB.EQ.'OFF')GOTO1619
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1615)
 1615 FORMAT('THE Y2LOG   SWITCH (FOR THE RIGHT VERTICAL ',
     1'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1619 CONTINUE
      GOTO1900
C
 1620 CONTINUE
      IFOUND='YES'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1625)
 1625 FORMAT('THE Y2LOG   SWITCH (FOR THE RIGHT VERTICAL ',
     1'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1629 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               **************************************************
C               **  TREAT THE CASE WHEN                         **
C               **  THE ENTIRE 4-SIDED FRAME IS TO BE LOG       **
C               **************************************************
C
      IF(ICOM.EQ.'XYLO')GOTO1700
      IF(ICOM.EQ.'YXLO')GOTO1700
      IF(ICOM.EQ.'LOG ')GOTO1700
      IF(ICOM.EQ.'LOGL')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(NUMARG.LE.0)GOTO1710
      IF(IHARG(NUMARG).EQ.'ON')GOTO1710
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1720
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1710
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1710
      IERROR='YES'
      GOTO1900
C
 1710 CONTINUE
      IFOUND='YES'
      IX1TSC='LOG'
      IX2TSC='LOG'
      IY1TSC='LOG'
      IY2TSC='LOG'
C
      IF(IFEEDB.EQ.'OFF')GOTO1719
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1715)
 1715 FORMAT('THE LOG   SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1719 CONTINUE
      GOTO1900
C
 1720 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
      IY1TSC='LINE'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1729
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1725)
 1725 FORMAT('THE LOG   SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1729 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTIS2(ICOM,IHARG,NUMARG,
     1IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC SCALES CONTAINED IN THE
C              4 VARIABLES IX1TSC,IX2TSC,IY1TSC,IY2TSC  .
C              SUCH TIC SCALE SWITCHES DEFINE THE SCALES
C              (LINEAR OR WEIBULL OR NORMAL)
C              FOR THE TICS ON THE 4 FRAME LINES OF A PLOT.
C     FOCUS OF SUBROUTINE DPTISC--LOG
C                         DPTIS2--WEIBULL
C                         DPTIS3--NORMAL
C
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
C                     --IX1TSC = LOWER HORIZONTAL TIC SCALE
C                     --IX2TSC = UPPER HORIZONTAL TIC SCALE
C                     --IY1TSC = LEFT  VERTICAL   TIC SCALE
C                     --IY2TSC = RIGHT VERTICAL   TIC SCALE
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
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
C
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
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORN')GOTO1900
C
C               ********************************************************
C               **  TREAT THE CASE WHEN                               **
C               **  BOTH HORIZONTAL FRAME LINES     ARE TO BE WEIBULL **
C               ********************************************************
C
      IF(ICOM.EQ.'XWEI')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(NUMARG.LE.0)GOTO1110
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
      IERROR='YES'
      GOTO1900
C
 1110 CONTINUE
      IFOUND='YES'
      IX1TSC='WEIB'
      IX2TSC='WEIB'
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1115)
 1115 FORMAT('THE XWEIB SWITCH (FOR BOTH HORIZ. WEIBULL SCALES)',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO1900
C
 1120 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('THE XWEIB SWITCH (FOR BOTH HORIZ. WEIBULL SCALES)',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               ********************************************************
C               **  TREAT THE CASE WHEN
C               **  ONLY THE BOTTOM HORIZONTAL FRAME LINE IS TO BE WEIBU
C               ********************************************************
C
      IF(ICOM.EQ.'X1WE')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(NUMARG.LE.0)GOTO1210
      IF(IHARG(NUMARG).EQ.'ON')GOTO1210
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1220
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1210
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1210
      IERROR='YES'
      GOTO1900
C
 1210 CONTINUE
      IFOUND='YES'
      IX1TSC='WEIB'
C
      IF(IFEEDB.EQ.'OFF')GOTO1219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('THE X1WEIB   SWITCH (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1219 CONTINUE
      GOTO1900
C
 1220 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1225)
 1225 FORMAT('THE X1WEIB   SWITCH (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               ********************************************************
C               **  TREAT THE CASE WHEN
C               **  ONLY THE TOP    HORIZONTAL FRAME LINE IS TO BE WEIBU
C               ********************************************************
C
      IF(ICOM.EQ.'X2WE')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(NUMARG.LE.0)GOTO1310
      IF(IHARG(NUMARG).EQ.'ON')GOTO1310
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1320
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1310
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1310
      IERROR='YES'
      GOTO1900
C
 1310 CONTINUE
      IFOUND='YES'
      IX2TSC='WEIB'
C
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1315)
 1315 FORMAT('THE X2WEIB   SWITCH (FOR THE TOP HORIZONTAL ',
     1'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      GOTO1900
C
 1320 CONTINUE
      IFOUND='YES'
      IX2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1329
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1325)
 1325 FORMAT('THE X2WEIB   SWITCH (FOR THE TOP HORIZONTAL ',
     1'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1329 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               ******************************************************
C               **  TREAT THE CASE WHEN                             **
C               **  BOTH VERTICAL FRAME LINES     ARE TO BE WEIBULL **
C               ******************************************************
C
      IF(ICOM.EQ.'YWEI')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(NUMARG.LE.0)GOTO1410
      IF(IHARG(NUMARG).EQ.'ON')GOTO1410
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1420
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1410
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1410
      IERROR='YES'
      GOTO1900
C
 1410 CONTINUE
      IFOUND='YES'
      IY1TSC='WEIB'
      IY2TSC='WEIB'
C
      IF(IFEEDB.EQ.'OFF')GOTO1419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1415)
 1415 FORMAT('THE YWEIB   SWITCH (FOR BOTH VERT. WEIBULL SCALES)',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1419 CONTINUE
      GOTO1900
C
 1420 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1425)
 1425 FORMAT('THE YWEIB   SWITCH (FOR BOTH VERT. WEIBULL SCALES)',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1429 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               ********************************************************
C               **  TREAT THE CASE WHEN
C               **  ONLY THE LEFT   VERTICAL   FRAME LINE IS TO BE WEIBU
C               ********************************************************
C
      IF(ICOM.EQ.'Y1WE')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(NUMARG.LE.0)GOTO1510
      IF(IHARG(NUMARG).EQ.'ON')GOTO1510
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1520
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1510
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1510
      IERROR='YES'
      GOTO1900
C
 1510 CONTINUE
      IFOUND='YES'
      IY1TSC='WEIB'
C
      IF(IFEEDB.EQ.'OFF')GOTO1519
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1515)
 1515 FORMAT('THE Y1WEIB   SWITCH (FOR THE LEFT VERTICAL ',
     1'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1519 CONTINUE
      GOTO1900
C
 1520 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1529
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1525)
 1525 FORMAT('THE Y1WEIB   SWITCH (FOR THE LEFT VERTICAL ',
     1'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1529 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               ********************************************************
C               **  TREAT THE CASE WHEN
C               **  ONLY THE RIGHT  VERTCIAL   FRAME LINE IS TO BE WEIBU
C               ********************************************************
C
      IF(ICOM.EQ.'Y2WE')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(NUMARG.LE.0)GOTO1610
      IF(IHARG(NUMARG).EQ.'ON')GOTO1610
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1620
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1610
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1610
      IERROR='YES'
      GOTO1900
C
 1610 CONTINUE
      IFOUND='YES'
      IY2TSC='WEIB'
C
      IF(IFEEDB.EQ.'OFF')GOTO1619
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1615)
 1615 FORMAT('THE Y2WEIB   SWITCH (FOR THE RIGHT VERTICAL ',
     1'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1619 CONTINUE
      GOTO1900
C
 1620 CONTINUE
      IFOUND='YES'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1625)
 1625 FORMAT('THE Y2WEIB   SWITCH (FOR THE RIGHT VERTICAL ',
     1'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1629 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               **************************************************
C               **  TREAT THE CASE WHEN                         **
C               **  THE ENTIRE 4-SIDED FRAME IS TO BE WEIBULL       **
C               **************************************************
C
      IF(ICOM.EQ.'XYWE')GOTO1700
      IF(ICOM.EQ.'YXWE')GOTO1700
      IF(ICOM.EQ.'WEIB')GOTO1700
CCCCC IF(ICOM.EQ.'WEIW'GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(NUMARG.LE.0)GOTO1710
      IF(IHARG(NUMARG).EQ.'ON')GOTO1710
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1720
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1710
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1710
      IERROR='YES'
      GOTO1900
C
 1710 CONTINUE
      IFOUND='YES'
      IX1TSC='WEIB'
      IX2TSC='WEIB'
      IY1TSC='WEIB'
      IY2TSC='WEIB'
C
      IF(IFEEDB.EQ.'OFF')GOTO1719
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1715)
 1715 FORMAT('THE WEIBULL   SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1719 CONTINUE
      GOTO1900
C
 1720 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
      IY1TSC='LINE'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1729
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1725)
 1725 FORMAT('THE WEIBULL   SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1729 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTIS3(ICOM,IHARG,NUMARG,
     1IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC SCALES CONTAINED IN THE
C              4 VARIABLES IX1TSC,IX2TSC,IY1TSC,IY2TSC  .
C              SUCH TIC SCALE SWITCHES DEFINE THE SCALES
C              (LINEAR OR WEIBULL OR NORMAL)
C              FOR THE TICS ON THE 4 FRAME LINES OF A PLOT.
C     FOCUS OF SUBROUTINE DPTISC--LOG
C                         DPTIS2--WEIBULL
C                         DPTIS3--NORMAL
C
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
C                     --IX1TSC = LOWER HORIZONTAL TIC SCALE
C                     --IX2TSC = UPPER HORIZONTAL TIC SCALE
C                     --IY1TSC = LEFT  VERTICAL   TIC SCALE
C                     --IY2TSC = RIGHT VERTICAL   TIC SCALE
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
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
C
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
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORN')GOTO1900
C
C               ********************************************************
C               **  TREAT THE CASE WHEN                               **
C               **  BOTH HORIZONTAL FRAME LINES     ARE TO BE NORMAL  **
C               ********************************************************
C
      IF(ICOM.EQ.'XNOR')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(NUMARG.LE.0)GOTO1110
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
      IERROR='YES'
      GOTO1900
C
 1110 CONTINUE
      IFOUND='YES'
      IX1TSC='NORM'
      IX2TSC='NORM'
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1115)
 1115 FORMAT('THE XNORM SWITCH (FOR BOTH HORIZ. NORMAL  SCALES)',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO1900
C
 1120 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('THE XNORM SWITCH (FOR BOTH HORIZ. NORMAL  SCALES)',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               ********************************************************
C               **  TREAT THE CASE WHEN
C               **  ONLY THE BOTTOM HORIZONTAL FRAME LINE IS TO BE NOR
C               ********************************************************
C
      IF(ICOM.EQ.'X1NO')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(NUMARG.LE.0)GOTO1210
      IF(IHARG(NUMARG).EQ.'ON')GOTO1210
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1220
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1210
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1210
      IERROR='YES'
      GOTO1900
C
 1210 CONTINUE
      IFOUND='YES'
      IX1TSC='NORM'
C
      IF(IFEEDB.EQ.'OFF')GOTO1219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('THE X1NORMAL SWITCH (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1219 CONTINUE
      GOTO1900
C
 1220 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1225)
 1225 FORMAT('THE X1NORMAL SWITCH (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               ********************************************************
C               **  TREAT THE CASE WHEN
C               **  ONLY THE TOP    HORIZONTAL FRAME LINE IS TO BE NORM
C               ********************************************************
C
      IF(ICOM.EQ.'X2NO')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(NUMARG.LE.0)GOTO1310
      IF(IHARG(NUMARG).EQ.'ON')GOTO1310
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1320
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1310
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1310
      IERROR='YES'
      GOTO1900
C
 1310 CONTINUE
      IFOUND='YES'
      IX2TSC='NORM'
C
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1315)
 1315 FORMAT('THE X2NORMAL SWITCH (FOR THE TOP HORIZONTAL ',
     1'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      GOTO1900
C
 1320 CONTINUE
      IFOUND='YES'
      IX2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1329
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1325)
 1325 FORMAT('THE X2NORMAL SWITCH (FOR THE TOP HORIZONTAL ',
     1'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1329 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               ******************************************************
C               **  TREAT THE CASE WHEN                             **
C               **  BOTH VERTICAL FRAME LINES     ARE TO BE NORMAL  **
C               ******************************************************
C
      IF(ICOM.EQ.'YNOR')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(NUMARG.LE.0)GOTO1410
      IF(IHARG(NUMARG).EQ.'ON')GOTO1410
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1420
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1410
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1410
      IERROR='YES'
      GOTO1900
C
 1410 CONTINUE
      IFOUND='YES'
      IY1TSC='NORM'
      IY2TSC='NORM'
C
      IF(IFEEDB.EQ.'OFF')GOTO1419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1415)
 1415 FORMAT('THE YNORM   SWITCH (FOR BOTH VERT. NORMAL  SCALES)',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1419 CONTINUE
      GOTO1900
C
 1420 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1425)
 1425 FORMAT('THE YNORM   SWITCH (FOR BOTH VERT. NORMAL  SCALES)',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1429 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               ********************************************************
C               **  TREAT THE CASE WHEN
C               **  ONLY THE LEFT   VERTICAL   FRAME LINE IS TO BE NORM
C               ********************************************************
C
      IF(ICOM.EQ.'Y1NO')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(NUMARG.LE.0)GOTO1510
      IF(IHARG(NUMARG).EQ.'ON')GOTO1510
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1520
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1510
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1510
      IERROR='YES'
      GOTO1900
C
 1510 CONTINUE
      IFOUND='YES'
      IY1TSC='NORM'
C
      IF(IFEEDB.EQ.'OFF')GOTO1519
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1515)
 1515 FORMAT('THE Y1NORMAL SWITCH (FOR THE LEFT VERTICAL ',
     1'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1519 CONTINUE
      GOTO1900
C
 1520 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1529
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1525)
 1525 FORMAT('THE Y1NORMAL SWITCH (FOR THE LEFT VERTICAL ',
     1'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1529 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               ********************************************************
C               **  TREAT THE CASE WHEN
C               **  ONLY THE RIGHT  VERTCIAL   FRAME LINE IS TO BE NORM
C               ********************************************************
C
      IF(ICOM.EQ.'Y2NO')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(NUMARG.LE.0)GOTO1610
      IF(IHARG(NUMARG).EQ.'ON')GOTO1610
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1620
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1610
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1610
      IERROR='YES'
      GOTO1900
C
 1610 CONTINUE
      IFOUND='YES'
      IY2TSC='NORM'
C
      IF(IFEEDB.EQ.'OFF')GOTO1619
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1615)
 1615 FORMAT('THE Y2NORMAL SWITCH (FOR THE RIGHT VERTICAL ',
     1'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1619 CONTINUE
      GOTO1900
C
 1620 CONTINUE
      IFOUND='YES'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1625)
 1625 FORMAT('THE Y2NORMAL SWITCH (FOR THE RIGHT VERTICAL ',
     1'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1629 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               **************************************************
C               **  TREAT THE CASE WHEN                         **
C               **  THE ENTIRE 4-SIDED FRAME IS TO BE NORMAL        **
C               **************************************************
C
      IF(ICOM.EQ.'XYNO')GOTO1700
      IF(ICOM.EQ.'YXNO')GOTO1700
CCCCC IF(ICOM.EQ.'NORM')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(NUMARG.LE.0)GOTO1710
      IF(IHARG(NUMARG).EQ.'ON')GOTO1710
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1720
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1710
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1710
      IERROR='YES'
      GOTO1900
C
 1710 CONTINUE
      IFOUND='YES'
      IX1TSC='NORM'
      IX2TSC='NORM'
      IY1TSC='NORM'
      IY2TSC='NORM'
C
      IF(IFEEDB.EQ.'OFF')GOTO1719
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1715)
 1715 FORMAT('THE NORMAL    SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1719 CONTINUE
      GOTO1900
C
 1720 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
      IY1TSC='LINE'
      IY2TSC='LINE'
C
      IF(IFEEDB.EQ.'OFF')GOTO1729
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1725)
 1725 FORMAT('THE NORMAL    SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1729 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTISZ(IHARG,IARGT,ARG,NUMARG,
     1PDEFHE,PDEFWI,
     1PTITHE,PTITWI,PTITVG,PTITHG,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE SIZE FOR THE TITLE
C              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
C              THE SIZE FOR THE TITLE WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE PTITHE.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --PDEFHE
C                     --PDEFWI
C     OUTPUT ARGUMENTS--PTITHE = TITLE HEIGHT
C                     --PTITWI = TITLE WIDTH
C                     --PTITVG = TITLE VERTICAL GAP
C                     --PTITHG = TITLE HORIZONTAL GAP
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
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1988.  DEFAULT WIDTH
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
      IF(NUMARG.LE.0)GOTO1199
      IF(IHARG(1).NE.'SIZE')GOTO1199
      IF(NUMARG.EQ.1)GOTO1150
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      GOTO1110
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
C
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPTISZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR TITLE SIZE ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE IT IS DESIRED TO HAVE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      THE TITLE ONE AND ONE HALF TIMES AS BIG ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      AS THE DEFAULT SIZE (WHICH IS SIZE 1), ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      TITLE SIZE 1.5 ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      PTITHE=PDEFHE
      PTITWI=PDEFWI
      GOTO1180
C
 1160 CONTINUE
      PTITHE=ARG(NUMARG)
      PTITWI=PTITHE*0.5
      PTITVG=PTITHE*0.375
      PTITHG=PTITHE*0.125
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)PTITHE
 1181 FORMAT('THE TITLE SIZE HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPTIT(IANS,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,
CCCCC SUBROUTINE DPTIT(IANS,IWIDTH,IHARG,IHARG2,NUMARG,
CCCCC THE ABOVE LINE WAS CHANGED        SEPTEMBER 1993
CCCCC SO AS TO ALLOW FOR LOWER CASE     SEPTEMBER 1993
CCCCC SUBROUTINE DPTIT(IANS,IWIDTH,IHARG,NUMARG,
CCCCC THE ABOVE LINE WAS AUGMENTED AUGUST 1992
CCCCC THE FOLLOWING LINE WAS AUGMENTED AUGUST 1992
CCCCC1ITITTE,NCTITL,IBUGP2,IFOUND,IERROR)
     1ITITTE,NCTITL,ITIAUT,IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--EXTRACT THE STRING TO BE USED AS A TITLE;
C              SAVE THIS STRING FOR USE ON PRINTER PLOTS;
C              ALSO, CONVERT THIS STRING INTO PROPER FORM
C              (ASCII INTEGER REPRESENTATION) FOR USE
C              WITH TEKTRONIX (OR EQUIVALENT) SOFTWARE.
C     INPUT  ARGUMENTS--IANS   (A  CHARACTER VECTOR)
C                     --IWIDTH
C                     --IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2  (A  CHARACTER VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--ITITTE (A CHARACTER VECTOR
C                              CONTAINING THE STRING FOR THE TITLE).
C                     --NCTITL  (AN INTEGER VARIABLE
C                              CONTAINING THE
C                              NUMBER OF CHARACTERS IN THE TITLE).
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
C     ORIGINAL VERSION--JANUARY    1978.
C     UPDATED         --JUNE       1978.
C     UPDATED         --JUNE       1979.
C     UPDATED         --SEPTEMBER  1980.
C     UPDATED         --MARCH      1981.
C     UPDATED         --DECEMBER   1981.
C     UPDATED         --MAY        1982.
C     UPDATED         --AUGUST     1992. ADD TITLE SWITCH
C                                        FOR AUTOMATIC
C     UPDATED         --SEPTEMBER  1993. ALLOW LOWER CASE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS
CCCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
CCCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      CHARACTER*4 IANSLC
      CHARACTER*4 IHARG
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
      CHARACTER*4 IHARG2
C
      CHARACTER*4 ITITTE
C
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
      CHARACTER*4 ITIAUT
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
CCCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
CCCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      DIMENSION IANSLC(*)
      DIMENSION IHARG(*)
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
      DIMENSION IHARG2(*)
C
      DIMENSION ITITTE(*)
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
      IF(IBUGP2.NE.'ON')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('AT THE BEGINNING OF DPTIT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NCTITL
   53 FORMAT('NCTITL = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      ILENT=NCTITL
      WRITE(ICOUT,41)(ITITTE(I),I=1,ILENT)
   41 FORMAT('CHARACTER ITITTE(.) --',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C     *****************************************
C     **  STEP 1--                           **
C     **  DETERMINE THE COMMAND              **
C     **  (TITLE) AND ITS LOCATION           **
C     **  ON THE LINE.                       **
C     *****************************************
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'COLO')GOTO9000
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'COLO')GOTO9000
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'SIZE')GOTO9000
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'SIZE')GOTO9000
C
      DO1000I=1,IWIDTH
      I2=I
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
      IP6=I+6
      IF(IANS(I).EQ.'T'.AND.IANS(IP1).EQ.'I'
     1.AND.IANS(IP2).EQ.'T'.AND.IANS(IP3).EQ.'L'
     1.AND.IANS(IP4).EQ.'E')
     1GOTO100
C
 1000 CONTINUE
      WRITE(ICOUT,1001)
 1001 FORMAT('***** ERROR IN DPTIT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1002)
 1002 FORMAT('      NO MATCH FOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO800
C
C     **********************************************************
C     **  STEP 2--                                            **
C     **  DEFINE THE START POSITION (ISTART) FOR THE STRING.  **
C     **********************************************************
C
  100 CONTINUE
      ISTART=I2+6
      GOTO300
C
C     ********************************************************
C     **  STEP 3--                                          **
C     **  DEFINE THE STOP POSITION (ISTOP) FOR THE STRING.  **
C     ********************************************************
C
  300 CONTINUE
      IFOUND='YES'
      ISTOP=0
      IF(ISTART.GT.IWIDTH)GOTO329
      DO320I=ISTART,IWIDTH
      IREV=IWIDTH-I+ISTART
      IF(IANS(IREV).NE.' ')GOTO325
  320 CONTINUE
      GOTO329
  325 CONTINUE
      ISTOP=IREV
  329 CONTINUE
C
C     *****************************************
C     **  STEP 4--                           **
C     **  COPY OVER THE STRING OF INTEREST.  **
C     *****************************************
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'ON')GOTO359
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'OFF')GOTO359
CCCCC IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'AUTO')GOTO359
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'DEFA')GOTO359
      IF(NUMARG.EQ.0)GOTO359
C
      IF(ISTART.GT.ISTOP)GOTO359
      IF(ISTOP.EQ.0)GOTO359
      J=0
      DO350I=ISTART,ISTOP
      J=J+1
CCCCC THE FOLLOWING LINE WAS   CHANGED   SEPTEMBER 1993
CCCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
CCCCC ITITTE(J)=IANS(I)
      ITITTE(J)=IANSLC(I)
  350 CONTINUE
      NCTITL=J
      GOTO800
  359 CONTINUE
C
C     ************************************
C     **  STEP 5--                      **
C     **  TREAT THE EMPTY-STRING CASE.  **
C     ************************************
C
      NCTITL=0
      GOTO800
C
C     ***************************
C     **  STEP 6--             **
C     **  PRINT OUT A MESSAGE  **
C     ***************************
C
  800 CONTINUE
      ILENT=NCTITL
C
CCCCC THE FOLLOWING 6 LINES WERE ADDED AUGUST 1992
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'AUTO'.AND.
     1IHARG2(1).EQ.'MATI')THEN
         ITIAUT='ON'
      ELSE
         ITIAUT='OFF'
      ENDIF
      IF(IFEEDB.EQ.'OFF')GOTO889
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)
  811 FORMAT('THE TITLE HAS JUST BEEN SET TO')
      CALL DPWRST('XXX','BUG ')
      IF(ILENT.EQ.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ILENT.GE.1)THEN
        WRITE(ICOUT,812)(ITITTE(I),I=1,MIN(ILENT,120))
  812   FORMAT(10X,120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
  889 CONTINUE
      GOTO9000
C
C     ****************
C     **  STEP 90-- **
C     **  EXIT      **
C     ****************
C
 9000 CONTINUE
      IF(IBUGP2.NE.'ON')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DPTIT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)NCTITL
 9012 FORMAT('NCTITL = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      ILENT=NCTITL
      WRITE(ICOUT,9021)(ITITTE(I),I=1,ILENT)
 9021 FORMAT('CHARACTER ITITTE(.) --',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTIDS(IHARG,ARG,NUMARG,PDEFDS,PTITDS,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE DISPLACEMENT FOR THE TITLE
C              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
C              THE DISPLACEMENT FOR THE TITLE WILL BE PLACED
C              IN THE REAL VARIABLE PTITDS.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --ARG    (A  REAL VECTOR)
C                     --NUMARG
C                     --PDEFDS
C     OUTPUT ARGUMENTS--PTITDS
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
C     VERSION NUMBER--89/8
C     ORIGINAL VERSION--JULY      1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
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
      IF(NUMARG.LE.0)GOTO1199
      IF(IHARG(1).EQ.'DISP')GOTO1110
      IF(IHARG(1).EQ.'OFFS')GOTO1110
      IF(IHARG(1).EQ.'GAP')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(NUMARG.EQ.1)GOTO1150
      GOTO1160
C
 1150 CONTINUE
      PTITDS=PDEFDS
      GOTO1180
C
 1160 CONTINUE
      PTITDS=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)PTITDS
 1181 FORMAT('THE TITLE DISPLACEMENT HAS JUST BEEN ',
     1'SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPTITH(IHARG,ARG,NUMARG,PDEFTH,PTITTH,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE THICKNESS FOR THE TITLE
C              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
C              THE THICKNESS FOR THE TITLE WILL BE PLACED
C              IN THE REAL VARIABLE PTITTH.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --ARG    (A  REAL VECTOR)
C                     --NUMARG
C                     --PDEFTH
C     OUTPUT ARGUMENTS--PTITTH
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 COMPUTER SERVICES DIVISION
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
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
      IF(NUMARG.LE.0)GOTO1199
      IF(IHARG(1).EQ.'THIC')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(NUMARG.EQ.1)GOTO1150
      GOTO1160
C
 1150 CONTINUE
      PTITTH=PDEFTH
      GOTO1180
C
 1160 CONTINUE
      PTITTH=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)PTITTH
 1181 FORMAT('THE TITLE THICKNESS HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPTL(ICOM,IHARG,NUMARG,
     1IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL SWITCHES CONTAINED IN THE
C              4 VARIABLES IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW
C              SUCH TIC LABEL SWITCHES TURN ON OR OFF
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
C                     --IX1ZSW = LOWER HORIZONTAL TIC LABELS
C                     --IX2ZSW = UPPER HORIZONTAL TIC LABELS
C                     --IY1ZSW = LEFT  VERTICAL   TIC LABELS
C                     --IY2ZSW = RIGHT VERTICAL   TIC LABELS
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
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IX1ZSW
      CHARACTER*4 IX2ZSW
      CHARACTER*4 IY1ZSW
      CHARACTER*4 IY2ZSW
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.0)GOTO1900
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NUMB')GOTO1900
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'NUMB')GOTO1900
C  FOLLOWING 4 LINES ADDED MAY, 1990.
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'OFFS')GOTO1900
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'OFFS')GOTO1900
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'COLO')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'SIZE')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'HW')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'FORM')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'CONT')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'NUMB')GOTO1900
C
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'COLO')GOTO1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'SIZE')GOTO1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'HW')GOTO1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'FORM')GOTO1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'CONT')GOTO1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'NUMB')GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'LABE')GOTO1160
      GOTO1150
C
 1150 CONTINUE
      IHOLD='ON'
      GOTO1180
C
 1160 CONTINUE
      IHOLD='OFF'
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1ZSW=IHOLD
      IX2ZSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1260
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'LABE')GOTO1260
      GOTO1250
C
 1250 CONTINUE
      IHOLD='ON'
      GOTO1280
C
 1260 CONTINUE
      IHOLD='OFF'
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1ZSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1360
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'LABE')GOTO1360
      GOTO1350
C
 1350 CONTINUE
      IHOLD='ON'
      GOTO1380
C
 1360 CONTINUE
      IHOLD='OFF'
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2ZSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1460
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'LABE')GOTO1460
      GOTO1450
C
 1450 CONTINUE
      IHOLD='ON'
      GOTO1480
C
 1460 CONTINUE
      IHOLD='OFF'
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1ZSW=IHOLD
      IY2ZSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1560
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'LABE')GOTO1560
      GOTO1550
C
 1550 CONTINUE
      IHOLD='ON'
      GOTO1580
C
 1560 CONTINUE
      IHOLD='OFF'
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1ZSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1660
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'LABE')GOTO1660
      GOTO1650
C
 1650 CONTINUE
      IHOLD='ON'
      GOTO1680
C
 1660 CONTINUE
      IHOLD='OFF'
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2ZSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1760
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'LABE')GOTO1760
      GOTO1750
C
 1750 CONTINUE
      IHOLD='ON'
      GOTO1780
C
 1760 CONTINUE
      IHOLD='OFF'
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1ZSW=IHOLD
      IX2ZSW=IHOLD
      IY1ZSW=IHOLD
      IY2ZSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLAN(ICOM,IHARG,ARG,NUMARG,
     1PDEFAN,
     1PX1ZAN,PX2ZAN,PY1ZAN,PY2ZAN,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL ANGLES CONTAINED IN THE
C              4 VARIABLES PX1ZAN,PX2ZAN,PY1ZAN,PY2ZAN
C              SUCH TIC LABEL ANGLES DEFINE THE ANGLES FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --ARG    (A REAL VECTOR)
C                     --NUMARG
C                     --PDEFAN
C     OUTPUT ARGUMENTS--
C                     --PX1ZAN = LOWER HORIZONTAL TIC LABEL ANGLE
C                     --PX2ZAN = UPPER HORIZONTAL TIC LABEL ANGLE
C                     --PY1ZAN = LEFT  VERTICAL   TIC LABEL ANGLE
C                     --PY2ZAN = RIGHT VERTICAL   TIC LABEL ANGLE
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 COMPUTER SERVICES DIVISION
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'ANGL')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'ANGL')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'ANGL')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      PHOLD=PDEFAN
      GOTO1180
C
 1160 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PX1ZAN=PHOLD
      PX2ZAN=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL ANGLE (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'ANGL')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      PHOLD=PDEFAN
      GOTO1280
C
 1260 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      PX1ZAN=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL ANGLE (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)PHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'ANGL')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      PHOLD=PDEFAN
      GOTO1380
C
 1360 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      PX2ZAN=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL ANGLE (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)PHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'ANGL')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      PHOLD=PDEFAN
      GOTO1480
C
 1460 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      PY1ZAN=PHOLD
      PY2ZAN=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL ANGLE (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)PHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'ANGL')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      PHOLD=PDEFAN
      GOTO1580
C
 1560 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      PY1ZAN=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL ANGLE (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)PHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'ANGL')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      PHOLD=PDEFAN
      GOTO1680
C
 1660 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      PY2ZAN=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL ANGLE (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)PHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'ANGL')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      PHOLD=PDEFAN
      GOTO1780
C
 1760 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      PX1ZAN=PHOLD
      PX2ZAN=PHOLD
      PY1ZAN=PHOLD
      PY2ZAN=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL ANGLE (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)PHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLCA(ICOM,IHARG,NUMARG,
     1IDEFCA,
     1IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL CASES CONTAINED IN THE
C              4 VARIABLES IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA
C              SUCH TIC LABEL CASES DEFINE THE CASES FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFCA
C     OUTPUT ARGUMENTS--
C                     --IX1ZCA = LOWER HORIZONTAL TIC LABEL CASE
C                     --IX2ZCA = UPPER HORIZONTAL TIC LABEL CASE
C                     --IY1ZCA = LEFT  VERTICAL   TIC LABEL CASE
C                     --IY2ZCA = RIGHT VERTICAL   TIC LABEL CASE
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 COMPUTER SERVICES DIVISION
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IDEFCA
C
      CHARACTER*4 IX1ZCA
      CHARACTER*4 IX2ZCA
      CHARACTER*4 IY1ZCA
      CHARACTER*4 IY2ZCA
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'CASE')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'CASE')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'CASE')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFCA
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1ZCA=IHOLD
      IX2ZCA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL CASE (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'CASE')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFCA
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1ZCA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL CASE (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'CASE')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFCA
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2ZCA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL CASE (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'CASE')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFCA
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1ZCA=IHOLD
      IY2ZCA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL CASE (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'CASE')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFCA
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1ZCA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL CASE (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'CASE')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFCA
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2ZCA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL CASE (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'CASE')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFCA
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1ZCA=IHOLD
      IX2ZCA=IHOLD
      IY1ZCA=IHOLD
      IY2ZCA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL CASE (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLCL(ICOM,IHARG,NUMARG,
     1IDEFCO,
     1IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL COLORS CONTAINED IN THE
C              4 VARIABLES IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO
C              SUCH TIC LABEL COLORS DEFINE THE COLORS FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFCO
C     OUTPUT ARGUMENTS--
C                     --IX1ZCO = LOWER HORIZONTAL TIC LABEL COLOR
C                     --IX2ZCO = UPPER HORIZONTAL TIC LABEL COLOR
C                     --IY1ZCO = LEFT  VERTICAL   TIC LABEL COLOR
C                     --IY2ZCO = RIGHT VERTICAL   TIC LABEL COLOR
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
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IDEFCO
C
      CHARACTER*4 IX1ZCO
      CHARACTER*4 IX2ZCO
      CHARACTER*4 IY1ZCO
      CHARACTER*4 IY2ZCO
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'COLO')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'COLO')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFCO
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1ZCO=IHOLD
      IX2ZCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL COLOR (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFCO
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1ZCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL COLOR (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFCO
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2ZCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL COLOR (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFCO
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1ZCO=IHOLD
      IY2ZCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL COLOR (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFCO
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1ZCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL COLOR (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFCO
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2ZCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL COLOR (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFCO
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1ZCO=IHOLD
      IX2ZCO=IHOLD
      IY1ZCO=IHOLD
      IY2ZCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL COLOR (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLCN(ICOM,IHARG,NUMARG,
CCCCC                   THE FOLLOWING LINE WAS CHANGED     SEPTEMBER 1993
CCCCC                   TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
CCCCC1                  IANS,IWIDTH,
     1                  IANS,IANSLC,IWIDTH,
     1                  IX1ZCN,IX2ZCN,IY1ZCN,IY2ZCN,
     1                  IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL CONTENTS CONTAINED IN THE
C              4 VARIABLES IX1ZCN,IX2ZCN,IY1ZCN,IY2ZCN
C              SUCH TIC LABEL CONTENTS DEFINE THE CONTENTS FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
C                     --IX1ZCN = LOWER HORIZONTAL TIC LABEL CONTENTS
C                     --IX2ZCN = UPPER HORIZONTAL TIC LABEL CONTENTS
C                     --IY1ZCN = LEFT  VERTICAL   TIC LABEL CONTENTS
C                     --IY2ZCN = RIGHT VERTICAL   TIC LABEL CONTENTS
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
C     VERSION NUMBER--88/2
C     ORIGINAL VERSION--JANUARY   1988.
C     UPDATED         --AUGUST    2001. UPDATE DIMENSIONS FROM 130
C                                       TO 160
C     UPDATED         --SEPTEMBER 2014. UPDATE DIMENSIONS FROM 512 TO
C                                       2048
C     UPDATED         --APRIL     2017. SOME RECODING FOR BETTER READABILITY
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS
CCCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
CCCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      CHARACTER*4 IANSLC
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*2048 IHOLCN
      CHARACTER*2048 ICJUNK
C
      CHARACTER*2048 IX1ZCN
      CHARACTER*2048 IX2ZCN
      CHARACTER*2048 IY1ZCN
      CHARACTER*2048 IY2ZCN
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
      DIMENSION IANS(*)
CCCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
CCCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      DIMENSION IANSLC(*)
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
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1   IHARG(2).EQ.'CONT')GOTO1009
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1   IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'CONT')GOTO1009
      GOTO9000
 1009 CONTINUE
C
C               ************************************
C               **  EXTRACT THE FULL STRING       **
C               ************************************
C
      DO1010I=1,IWIDTH-6
        I2=I
        IF(IANS(I).EQ.'C'.AND.IANS(I+1).EQ.'O'.AND.
     1     IANS(I+2).EQ.'N'.AND.IANS(I+3).EQ.'T'.AND.
     1     IANS(I+4).EQ.'E'.AND.IANS(I+5).EQ.'N'.AND.
     1     IANS(I+6).EQ.'T')THEN
          IFOUND='YES'
          ISTART=I+8
          IF(IANS(I+7).EQ.'S')ISTART=ISTART+1
          GOTO1019
        ENDIF
 1010 CONTINUE
C
      WRITE(ICOUT,1011)
 1011 FORMAT('***** ERROR IN TIC MARK LABEL CONTENT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1012)
 1012 FORMAT('      NO MATCH FOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 1019 CONTINUE
C
      ISTOP=0
      IF(ISTART.GT.IWIDTH)GOTO1039
      DO1030I=ISTART,IWIDTH
        IREV=IWIDTH-I+ISTART
        IF(IANS(IREV).NE.' ')THEN
          ISTOP=IREV
          GOTO1039
        ENDIF
 1030 CONTINUE
 1039 CONTINUE
C
      ICJUNK=' '
      NCJUNK=0
      IF(ISTART.LE.ISTOP .AND. ISTOP.GT.0)THEN
        J=0
        DO1040I=ISTART,ISTOP
          J=J+1
CCCCC     THE FOLLOWING LINE WAS CHANGED     SEPTEMBER 1993
CCCCC     TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
CCCCC     ICJUNK(J:J)=IANS(I)
          ICJUNK(J:J)=IANSLC(I)
 1040   CONTINUE
        NCJUNK=J
      ENDIF
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
C
        IFOUND='YES'
        IX1ZCN=IHOLCN
        IX2ZCN=IHOLCN
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
 1181     FORMAT('THE TIC MARK LABEL CONTENTS FOR BOTH HORIZONTAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1183)
 1183     FORMAT('FRAME LINES HAS JUST BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          IF(NCJUNK.LE.0)THEN
            WRITE(ICOUT,1185)
 1185       FORMAT('FLOAT WITH THE DATA.')
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,1184)(IHOLCN(I:I),I=1,MIN(80,NCJUNK))
 1184       FORMAT(80A1)
            CALL DPWRST('XXX','BUG ')
            IF(NCJUNK.GE.81)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=81,MIN(160,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IF(NCJUNK.GE.161)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=161,MIN(240,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
        GOTO9000
      ENDIF
C
C               ******************************************************
C               **  TREAT THE CASE WHEN                             **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE  **
C               **  CHANGED                                         **
C               ******************************************************
C
      IF(ICOM.EQ.'X1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IX1ZCN=IHOLCN
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1281)
 1281     FORMAT('THE TIC MARK LABEL CONTENTS FOR THE BOTTOM ',
     1           'HORIZONTAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1283)
 1283     FORMAT('FRAME LINE HAS JUST BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          IF(NCJUNK.LE.0)THEN
            WRITE(ICOUT,1185)
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,1184)(IHOLCN(I:I),I=1,MIN(80,NCJUNK))
            CALL DPWRST('XXX','BUG ')
            IF(NCJUNK.GE.81)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=81,MIN(160,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IF(NCJUNK.GE.161)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=161,MIN(NCJUNK,240))
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
        GOTO9000
      ENDIF
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IX2ZCN=IHOLCN
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1381)
 1381     FORMAT('THE TIC MARK LABEL CONTENTS FOR THE TOP HORIZONTAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1383)
 1383     FORMAT('FRAME LINE HAS JUST BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          IF(NCJUNK.LE.0)THEN
            WRITE(ICOUT,1185)
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,1184)(IHOLCN(I:I),I=1,MIN(80,NCJUNK))
            CALL DPWRST('XXX','BUG ')
            IF(NCJUNK.GE.81)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=81,MIN(160,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IF(NCJUNK.GE.161)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=161,MIN(240,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
        GOTO9000
      ENDIF
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IY1ZCN=IHOLCN
        IY2ZCN=IHOLCN
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1481)
 1481     FORMAT('THE TIC MARK LABEL CONTENTS FOR BOTH VERTICAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1483)
 1483     FORMAT('FRAME LINES HAS JUST BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          IF(NCJUNK.LE.0)THEN
            WRITE(ICOUT,1185)
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,1184)(IHOLCN(I:I),I=1,MIN(80,NCJUNK))
            CALL DPWRST('XXX','BUG ')
            IF(NCJUNK.GE.81)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=81,MIN(160,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IF(NCJUNK.GE.161)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=161,MIN(240,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
        GOTO9000
      ENDIF
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IY1ZCN=IHOLCN
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1581)
 1581     FORMAT('THE TIC MARK LABEL CONTENTS FOR THE LEFT VERTICAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1583)
 1583     FORMAT('FRAME LINE HAS JUST BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          IF(NCJUNK.LE.0)THEN
            WRITE(ICOUT,1185)
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,1184)(IHOLCN(I:I),I=1,MIN(80,NCJUNK))
            CALL DPWRST('XXX','BUG ')
            IF(NCJUNK.GE.81)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=81,MIN(160,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IF(NCJUNK.GE.161)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=161,MIN(240,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
        GOTO9000
      ENDIF
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IY2ZCN=IHOLCN
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1681)
 1681     FORMAT('THE TIC MARK LABEL CONTENTS OR THE RIGHT VERTICAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1683)
 1683     FORMAT('FRAME LINE HAS JUST BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          IF(NCJUNK.LE.0)THEN
            WRITE(ICOUT,1185)
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,1184)(IHOLCN(I:I),I=1,MIN(80,NCJUNK))
            CALL DPWRST('XXX','BUG ')
            IF(NCJUNK.GE.81)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=81,MIN(160,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IF(NCJUNK.GE.161)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=161,MIN(240,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
        GOTO9000
      ENDIF
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.
     1   ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IX1ZCN=IHOLCN
        IX2ZCN=IHOLCN
        IY1ZCN=IHOLCN
        IY2ZCN=IHOLCN
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1781)
 1781     FORMAT('THE TIC MARK LABEL CONTENTS FOR ALL 4 FRAME LINES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1783)
 1783     FORMAT('HAVE JUST BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          IF(NCJUNK.LE.0)THEN
            WRITE(ICOUT,1185)
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,1184)(IHOLCN(I:I),I=1,MIN(80,NCJUNK))
            CALL DPWRST('XXX','BUG ')
            IF(NCJUNK.GE.81)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=81,MIN(160,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IF(NCJUNK.GE.161)THEN
              WRITE(ICOUT,1184)(IHOLCN(I:I),I=161,MIN(240,NCJUNK))
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
        GOTO9000
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLDI(ICOM,IHARG,NUMARG,
     1IDEFDI,
     1IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL DIRECTIONS CONTAINED IN THE
C              4 VARIABLES IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI
C              SUCH TIC LABEL DIRECTIONS DEFINE THE DIRECTIONS FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFDI
C     OUTPUT ARGUMENTS--
C                     --IX1ZDI = LOWER HORIZONTAL TIC LABEL DIRECTION
C                     --IX2ZDI = UPPER HORIZONTAL TIC LABEL DIRECTION
C                     --IY1ZDI = LEFT  VERTICAL   TIC LABEL DIRECTION
C                     --IY2ZDI = RIGHT VERTICAL   TIC LABEL DIRECTION
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 COMPUTER SERVICES DIVISION
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IDEFDI
C
      CHARACTER*4 IX1ZDI
      CHARACTER*4 IX2ZDI
      CHARACTER*4 IY1ZDI
      CHARACTER*4 IY2ZDI
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'DIRE')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'DIRE')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'DIRE')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFDI
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1ZDI=IHOLD
      IX2ZDI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL DIRECTION (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'DIRE')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFDI
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1ZDI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL DIRECTION (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'DIRE')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFDI
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2ZDI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL DIRECTION (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'DIRE')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFDI
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1ZDI=IHOLD
      IY2ZDI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL DIRECTION (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'DIRE')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFDI
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1ZDI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL DIRECTION (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'DIRE')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFDI
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2ZDI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL DIRECTION (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'DIRE')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFDI
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1ZDI=IHOLD
      IX2ZDI=IHOLD
      IY1ZDI=IHOLD
      IY2ZDI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL DIRECTION (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLDS(ICOM,IHARG,IARGT,ARG,NUMARG,
     1PDEFHG,PDEFVG,
     1PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK LABEL DISPLACEMENT SWITCHES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK SWITCHES DEFINE THE DISPLACEMENT
C              OF THE TIC MARK LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C                     --PDEFHG
C                     --PDEFVG
C     OUTPUT ARGUMENTS--
C                     --PX1ZDS,
C                     --PX2ZDS,
C                     --PY1ZDS,
C                     --PY2ZDS,
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
C     VERSION NUMBER--91/9
C     ORIGINAL VERSION--AUGUST    1991.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
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
CCCCC IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'DISP')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'DISP')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'OFFS')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'OFFS')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'GAP')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'GAP')GOTO1090
CCCCC GOTO1900
      GOTO9000
 1090 CONTINUE
      HOLD1=(-999.9)
      HOLD2=(-999.9)
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'DISP')GOTO1150
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      IERROR='YES'
      GOTO9000
C
 1150 CONTINUE
      HOLD1=PDEFHG
      GOTO1180
C
 1160 CONTINUE
      HOLD1=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PX1ZDS=HOLD1
      PX2ZDS=HOLD1
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL DISPLACEMENT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('(FOR BOTH HORIZONTAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)HOLD1
 1183 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'DISP')GOTO1250
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1260
      IERROR='YES'
      GOTO9000
C
 1250 CONTINUE
      HOLD1=PDEFHG
      GOTO1280
C
 1260 CONTINUE
      HOLD1=ARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      PX1ZDS=HOLD1
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)
 1282 FORMAT('(FOR THE BOTTOM HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)HOLD1
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'DISP')GOTO1350
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1360
      IERROR='YES'
      GOTO9000
C
 1350 CONTINUE
      HOLD1=PDEFHG
      GOTO1380
C
 1360 CONTINUE
      HOLD1=ARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      PX2ZDS=HOLD1
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)
 1382 FORMAT('(FOR THE TOP HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)HOLD1
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'DISP')GOTO1450
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1460
      IERROR='YES'
      GOTO9000
C
 1450 CONTINUE
      HOLD1=PDEFVG
      GOTO1480
C
 1460 CONTINUE
      HOLD1=ARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      PY1ZDS=HOLD1
      PY2ZDS=HOLD1
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)
 1482 FORMAT('(FOR BOTH VERTICAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)HOLD1
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'DISP')GOTO1550
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1560
      IERROR='YES'
      GOTO9000
C
 1550 CONTINUE
      HOLD1=PDEFVG
      GOTO1580
C
 1560 CONTINUE
      HOLD1=ARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      PY1ZDS=HOLD1
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)
 1582 FORMAT('(FOR THE LEFT VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)HOLD1
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'DISP')GOTO1650
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1660
      IERROR='YES'
      GOTO9000
C
 1650 CONTINUE
      HOLD1=PDEFVG
      GOTO1680
C
 1660 CONTINUE
      HOLD1=ARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      PY2ZDS=HOLD1
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)
 1682 FORMAT('(FOR THE RIGHT VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)HOLD1
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'DISP')GOTO1750
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1760
      IERROR='YES'
      GOTO9000
C
 1750 CONTINUE
      HOLD1=PDEFHG
      HOLD2=PDEFVG
      GOTO1780
C
 1760 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=ARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      PX1ZDS=HOLD1
      PX2ZDS=HOLD1
      PY1ZDS=HOLD2
      PY2ZDS=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)
 1782 FORMAT('(FOR BOTH HORIZONTAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)HOLD1
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1784)
 1784 FORMAT('(FOR BOTH VERTICAL   FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)HOLD2
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
C
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLFI(ICOM,IHARG,NUMARG,
     1IDEFFI,
     1IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL FILLS CONTAINED IN THE
C              4 VARIABLES IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI
C              SUCH TIC LABEL FILLS DEFINE THE FILLS FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFFI
C     OUTPUT ARGUMENTS--
C                     --IX1ZFI = LOWER HORIZONTAL TIC LABEL FILL
C                     --IX2ZFI = UPPER HORIZONTAL TIC LABEL FILL
C                     --IY1ZFI = LEFT  VERTICAL   TIC LABEL FILL
C                     --IY2ZFI = RIGHT VERTICAL   TIC LABEL FILL
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 COMPUTER SERVICES DIVISION
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IDEFFI
C
      CHARACTER*4 IX1ZFI
      CHARACTER*4 IX2ZFI
      CHARACTER*4 IY1ZFI
      CHARACTER*4 IY2ZFI
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'FILL')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'FILL')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'FILL')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFFI
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1ZFI=IHOLD
      IX2ZFI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL FILL (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'FILL')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFFI
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1ZFI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL FILL (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'FILL')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFFI
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2ZFI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL FILL (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'FILL')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFFI
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1ZFI=IHOLD
      IY2ZFI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL FILL (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'FILL')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFFI
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1ZFI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL FILL (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'FILL')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFFI
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2ZFI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL FILL (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'FILL')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFFI
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1ZFI=IHOLD
      IX2ZFI=IHOLD
      IY1ZFI=IHOLD
      IY2ZFI=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL FILL (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLFM(ICOM,IHARG,NUMARG,
     1                  IDETLF,
     1                  IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM,
     1                  IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL FORMATS CONTAINED IN THE
C              4 VARIABLES IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM
C              SUCH TIC LABEL FORMATS DEFINE THE FORMATS FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDETLF
C     OUTPUT ARGUMENTS--
C                     --IX1ZFM = LOWER HORIZONTAL TIC LABEL FORMAT
C                     --IX2ZFM = UPPER HORIZONTAL TIC LABEL FORMAT
C                     --IY1ZFM = LEFT  VERTICAL   TIC LABEL FORMAT
C                     --IY2ZFM = RIGHT VERTICAL   TIC LABEL FORMAT
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
C     VERSION NUMBER--88/2
C     ORIGINAL VERSION--FEBRUARY  1988.
C     UPDATED         --JANUARY   2004. ADD SUPPORT FOR:
C                                           ROW LABEL
C                                           GROUP LABEL
C                                           VARIABLE
C     UPDATED         --APRIL     2017. ROW LABEL CAN HAVE A
C                                       A START ROW AND A STOP
C                                       ROW.  ALSO SOME RE-CODING
C                                       FOR BETTER READABILTY
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IDETLF
      CHARACTER*4 IX1ZFM
      CHARACTER*4 IX2ZFM
      CHARACTER*4 IY1ZFM
      CHARACTER*4 IY2ZFM
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'FORM')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'FORM')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
C
        IFOUND='YES'
        IX1ZFM=IHOLD
        IX2ZFM=IHOLD
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
 1181     FORMAT('THE TIC MARK LABEL FORMAT (FOR BOTH HORIZONTAL ',
     1           'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1182)IHOLD
 1182     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO1900
      ENDIF
C
C               *******************************************************
C               **  TREAT THE CASE WHEN ONLY THE BOTTOM              **
C               **  HORIZONTAL TIC MARKS ARE TO BE CHANGED           **
C               *******************************************************
C
      IF(ICOM.EQ.'X1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
C
        IFOUND='YES'
        IX1ZFM=IHOLD
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1281)
 1281     FORMAT('THE TIC MARK LABEL FORMAT (FOR THE BOTTOM ',
     1           'HORIZONTAL FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1282)IHOLD
 1282     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO1900
      ENDIF
C
C               *******************************************************
C               **  TREAT THE CASE WHEN ONLY THE TOP                 **
C               **  HORIZONTAL TIC MARKS ARE TO BE CHANGED           **
C               *******************************************************
C
      IF(ICOM.EQ.'X2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
C
        IFOUND='YES'
        IX2ZFM=IHOLD
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1381)
 1381     FORMAT('THE TIC MARK LABEL FORMAT (FOR THE TOP HORIZONTAL ',
     1           'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1382)IHOLD
 1382     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO1900
      ENDIF
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
C
        IFOUND='YES'
        IY1ZFM=IHOLD
        IY2ZFM=IHOLD
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1481)
 1481     FORMAT('THE TIC MARK LABEL FORMAT (FOR BOTH VERTICAL ',
     1           'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1482)IHOLD
 1482     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO1900
      ENDIF
C
C               *******************************************************
C               **  TREAT THE CASE WHEN ONLY THE LEFT                **
C               **  VERTICAL   TIC MARKS ARE TO BE CHANGED           **
C               *******************************************************
C
      IF(ICOM.EQ.'Y1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
C
        IFOUND='YES'
        IY1ZFM=IHOLD
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1581)
 1581     FORMAT('THE TIC MARK LABEL FORMAT (FOR THE LEFT VERTICAL ',
     1           'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1582)IHOLD
 1582     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO1900
      ENDIF
C
C               *******************************************************
C               **  TREAT THE CASE WHEN ONLY THE RIGHT               **
C               **  VERTICAL   TIC MARKS ARE TO BE CHANGED           **
C               *******************************************************
C
      IF(ICOM.EQ.'Y2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
C
        IFOUND='YES'
        IY2ZFM=IHOLD
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1681)
 1681     FORMAT('THE TIC MARK LABEL FORMAT (FOR THE RIGHT VERTICAL ',
     1           'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1682)IHOLD
 1682     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO1900
      ENDIF
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.
     1   ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')
     1  THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
C
        IFOUND='YES'
        IX1ZFM=IHOLD
        IX2ZFM=IHOLD
        IY1ZFM=IHOLD
        IY2ZFM=IHOLD
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1781)
 1781     FORMAT('THE TIC MARK LABEL FORMAT (FOR ALL 4 ',
     1           'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1782)IHOLD
 1782     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO1900
      ENDIF
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLFO(ICOM,IHARG,NUMARG,
     1IDEFFO,
     1IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL FONTS CONTAINED IN THE
C              4 VARIABLES IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO
C              SUCH TIC LABEL FONTS DEFINE THE FONTS FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFFO
C     OUTPUT ARGUMENTS--
C                     --IX1ZFO = LOWER HORIZONTAL TIC LABEL FONT
C                     --IX2ZFO = UPPER HORIZONTAL TIC LABEL FONT
C                     --IY1ZFO = LEFT  VERTICAL   TIC LABEL FONT
C                     --IY2ZFO = RIGHT VERTICAL   TIC LABEL FONT
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IDEFFO
C
      CHARACTER*4 IX1ZFO
      CHARACTER*4 IX2ZFO
      CHARACTER*4 IY1ZFO
      CHARACTER*4 IY2ZFO
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'FONT')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'FONT')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'FONT')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFFO
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1ZFO=IHOLD
      IX2ZFO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL FONT (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'FONT')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFFO
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1ZFO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL FONT (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'FONT')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFFO
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2ZFO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL FONT (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'FONT')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFFO
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1ZFO=IHOLD
      IY2ZFO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL FONT (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'FONT')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFFO
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1ZFO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL FONT (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'FONT')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFFO
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2ZFO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL FONT (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'FONT')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFFO
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1ZFO=IHOLD
      IX2ZFO=IHOLD
      IY1ZFO=IHOLD
      IY2ZFO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL FONT (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLHW(ICOM,IHARG,IARGT,ARG,NUMARG,
     1PDEFHE,PDEFWI,
     1PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,
     1PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,
     1PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,
     1PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK LABEL HEIGHT AND WIDTH SWITCHES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK SWITCHES DEFINE THE HEIGHT AND WIDTH
C              OF THE TIC MARK LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C                     --PDEFHE
C                     --PDEFWI
C     OUTPUT ARGUMENTS--
C                     --PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,
C                     --PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,
C                     --PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,
C                     --PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,
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
C     ORIGINAL VERSION--JULY      1987.
C     UPDATED         --DECEMBER  1988.    ADD DEFAULT WIDTH
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
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
      NUMAM1=NUMARG-1
C
CCCCC IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'HW')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'HW')GOTO1090
CCCCC GOTO1900
      GOTO9000
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'HW')GOTO1150
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.
     1   IARGT(NUMARG).EQ.'NUMB')GOTO1160
      IERROR='YES'
      GOTO9000
C
 1150 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1180
C
 1160 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX2ZHE=HOLD1
      PX1ZWI=HOLD2
      PX2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR BOTH ',
     1'HORIZONTAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)HOLD1,HOLD2
 1182 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'HW')GOTO1250
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.
     1   IARGT(NUMARG).EQ.'NUMB')GOTO1260
      IERROR='YES'
      GOTO9000
C
 1250 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1280
C
 1260 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX1ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)HOLD1,HOLD2
 1282 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'HW')GOTO1350
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.
     1   IARGT(NUMARG).EQ.'NUMB')GOTO1360
      IERROR='YES'
      GOTO9000
C
 1350 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1380
C
 1360 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      PX2ZHE=HOLD1
      PX2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR THE TOP ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)HOLD1,HOLD2
 1382 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'HW')GOTO1450
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.
     1   IARGT(NUMARG).EQ.'NUMB')GOTO1460
      IERROR='YES'
      GOTO9000
C
 1450 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1480
C
 1460 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      PY1ZHE=HOLD1
      PY2ZHE=HOLD1
      PY1ZWI=HOLD2
      PY2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR BOTH ',
     1'VERTICAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)HOLD1,HOLD2
 1482 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'HW')GOTO1550
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.
     1   IARGT(NUMARG).EQ.'NUMB')GOTO1560
      IERROR='YES'
      GOTO9000
C
 1550 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1580
C
 1560 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      PY1ZHE=HOLD1
      PY1ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR THE LEFT ',
     1'VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)HOLD1,HOLD2
 1582 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'HW')GOTO1650
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.
     1   IARGT(NUMARG).EQ.'NUMB')GOTO1660
      IERROR='YES'
      GOTO9000
C
 1650 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1680
C
 1660 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      PY2ZHE=HOLD1
      PY2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR THE RIGHT ',
     1'VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)HOLD1,HOLD2
 1682 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'HW')GOTO1750
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.
     1   IARGT(NUMARG).EQ.'NUMB')GOTO1760
      IERROR='YES'
      GOTO9000
C
 1750 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1780
C
 1760 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX2ZHE=HOLD1
      PY1ZHE=HOLD1
      PY2ZHE=HOLD1
      PX1ZWI=HOLD2
      PX2ZWI=HOLD2
      PY1ZWI=HOLD2
      PY2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR ',
     1'ALL 4 FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)HOLD1,HOLD2
 1782 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
C
      PX1ZVG=PX1ZHE*0.375
      PX2ZVG=PX2ZHE*0.375
      PY1ZVG=PY1ZHE*0.375
      PY2ZVG=PY2ZHE*0.375
C
      PX1ZHG=PX1ZHE*0.125
      PX2ZHG=PX2ZHE*0.125
      PY1ZHG=PY1ZHE*0.125
      PY2ZHG=PY2ZHE*0.125
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLJU(ICOM,IHARG,NUMARG,
     1IDEFJU,
     1IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL JUSTIFICATIONS CONTAINED IN THE
C              4 VARIABLES IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU
C              SUCH TIC LABEL JUSTIFICATIONS DEFINE THE JUSTIFICATIONS FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFJU
C     OUTPUT ARGUMENTS--
C                     --IX1ZJU = LOWER HORIZONTAL TIC LABEL JUSTIFICATION
C                     --IX2ZJU = UPPER HORIZONTAL TIC LABEL JUSTIFICATION
C                     --IY1ZJU = LEFT  VERTICAL   TIC LABEL JUSTIFICATION
C                     --IY2ZJU = RIGHT VERTICAL   TIC LABEL JUSTIFICATION
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IDEFJU
C
      CHARACTER*4 IX1ZJU
      CHARACTER*4 IX2ZJU
      CHARACTER*4 IY1ZJU
      CHARACTER*4 IY2ZJU
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'JUST')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'JUST')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'JUST')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFJU
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1ZJU=IHOLD
      IX2ZJU=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR BOTH ',
     1'HORIZONTAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'JUST')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFJU
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1ZJU=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'JUST')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFJU
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2ZJU=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR THE TOP ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'JUST')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFJU
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1ZJU=IHOLD
      IY2ZJU=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'JUST')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFJU
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1ZJU=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR THE LEFT ',
     1'VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'JUST')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFJU
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2ZJU=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR THE RIGHT ',
     1'VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'JUST')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFJU
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1ZJU=IHOLD
      IX2ZJU=IHOLD
      IY1ZJU=IHOLD
      IY2ZJU=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLSZ(ICOM,IHARG,IARGT,ARG,NUMARG,
     1PDEFHE,PDEFWI,
     1PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,
     1PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,
     1PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,
     1PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK LABEL SIZE SWITCHES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK SWITCHES DEFINE THE SIZE (HEIGHT)
C              OF THE TIC MARK LABELS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C                     --PDEFHE
C     OUTPUT ARGUMENTS--
C                     --PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,
C                     --PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,
C                     --PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,
C                     --PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,
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
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1988.  DEFAULT WIDTH
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
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
CCCCC IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'SIZE')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'SIZE')GOTO1090
CCCCC GOTO1900
      GOTO9000
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1150
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      IERROR='YES'
      GOTO9000
C
 1150 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1180
C
 1160 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX2ZHE=HOLD1
      PX1ZWI=HOLD2
      PX2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL SIZE (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)HOLD1
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1250
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1260
      IERROR='YES'
      GOTO9000
C
 1250 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1280
C
 1260 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX1ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL SIZE (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)HOLD1
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1350
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1360
      IERROR='YES'
      GOTO9000
C
 1350 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1380
C
 1360 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      PX2ZHE=HOLD1
      PX2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL SIZE (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)HOLD1
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1450
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1460
      IERROR='YES'
      GOTO9000
C
 1450 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1480
C
 1460 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      PY1ZHE=HOLD1
      PY2ZHE=HOLD1
      PY1ZWI=HOLD2
      PY2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL SIZE (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)HOLD1
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1550
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1560
      IERROR='YES'
      GOTO9000
C
 1550 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1580
C
 1560 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      PY1ZHE=HOLD1
      PY1ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL SIZE (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)HOLD1
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1650
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1660
      IERROR='YES'
      GOTO9000
C
 1650 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1680
C
 1660 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      PY2ZHE=HOLD1
      PY2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL SIZE (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)HOLD1
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1750
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1760
      IERROR='YES'
      GOTO9000
C
 1750 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GOTO1780
C
 1760 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX2ZHE=HOLD1
      PY1ZHE=HOLD1
      PY2ZHE=HOLD1
      PX1ZWI=HOLD2
      PX2ZWI=HOLD2
      PY1ZWI=HOLD2
      PY2ZWI=HOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL SIZE (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)HOLD1
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
C
      PX1ZVG=PX1ZHE*0.375
      PX2ZVG=PX2ZHE*0.375
      PY1ZVG=PY1ZHE*0.375
      PY2ZVG=PY2ZHE*0.375
C
      PX1ZHG=PX1ZHE*0.125
      PX2ZHG=PX2ZHE*0.125
      PY1ZHG=PY1ZHE*0.125
      PY2ZHG=PY2ZHE*0.125
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPTLTH(ICOM,IHARG,ARG,NUMARG,
     1PDEFTH,
     1PTIZTH,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC LABEL THICKNESSS CONTAINED IN THE
C              4 VARIABLES PTIZTH,PTIZTH,PTIZTH,PTIZTH
C              SUCH TIC LABEL THICKNESSS DEFINE THE THICKNESSS FOR
C              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
C              NOTE: ALL 4 THICKNESS CURRENTLY LIMITED TO ONE
C                    SETTING, PTIZTH
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --ARG    (A REAL VECTOR)
C                     --NUMARG
C                     --PDEFTH
C     OUTPUT ARGUMENTS--
C                     --PTIZTH = LOWER HORIZONTAL TIC LABEL THICKNESS
C                     --PTIZTH = UPPER HORIZONTAL TIC LABEL THICKNESS
C                     --PTIZTH = LEFT  VERTICAL   TIC LABEL THICKNESS
C                     --PTIZTH = RIGHT VERTICAL   TIC LABEL THICKNESS
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
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
      IF(NUMARG.LE.1)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'THIC')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'THIC')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      PHOLD=PDEFTH
      GOTO1180
C
 1160 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      PHOLD=PDEFTH
      GOTO1280
C
 1260 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL THICKNESS (ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)PHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      PHOLD=PDEFTH
      GOTO1380
C
 1360 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)PHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      PHOLD=PDEFTH
      GOTO1480
C
 1460 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)PHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      PHOLD=PDEFTH
      GOTO1580
C
 1560 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)PHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      PHOLD=PDEFTH
      GOTO1680
C
 1660 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)PHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      PHOLD=PDEFTH
      GOTO1780
C
 1760 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)PHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTMCO(XTEMP1,XTEMP2,MAXNXT,ICASAN,
     1                  ICAPSW,IFORSW,IMULT,IREPL,
     1                  ISUBRO,IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE CONFIDENCE LIMITS FOR THE TRIMMED MEAN
C              FOR PROBABILITY VALUE P = .90, .95, .99, .999, AND .9999.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     REFERENCE--"INTRODUCTION TO ROBUST ESTIMATION AND HYPOTHESIS
C                TESTING", RAND R. WILCOX, ACADEMIC PRESS, 1997.
C                1977.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/2
C     ORIGINAL VERSION--FEBRUARY  2003.
C     UPDATED         --OCTOBER   2003. ADD SUPPORT FOR HTML, LATEX
C                                       OUTPUT
C     UPDATED         --MARCH     2010. USE DPDTA1, DPDTA4 TO GENERATE
C                                       HTML, LATEX, RTF FORMAT
C     UPDATED         --MARCH     2010. SUPPORT FOR MULTIPLE RESPONSE
C                                       VARIABLES AND FOR GROUP-ID
C                                       VARIABLES (I.E., REPLICATION
C                                       CASE)
C     UPDATED         --MARCH     2010. USE DPPAR3 TO EXTRACT EITHER A
C                                       RESPONSE VARIABLE OR A MATRIX
C                                       NAME
C     UPDATED         --OCTOBER   2012. TRIMMING CAN BE SPECIFIED EITHER
C                                       AS A PROPORTION OR AS A SPECIFIC
C                                       NUMBER TO TRIM
C     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASE
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IFLAGU
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
C
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      CHARACTER*4 IVARID(MAXSPN)
      CHARACTER*4 IVARI2(MAXSPN)
      REAL PVAR(MAXSPN)
      REAL PID(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION W(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
C
      DIMENSION XDESGN(MAXOBV,6)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB2),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTE4(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE5(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE6(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB9),W(1))
      EQUIVALENCE (GARBAG(IGAR10),XDESGN(1,1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
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
      ISUBN1='DPTM'
      ISUBN2='CO  '
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
C               *****************************************************
C               **  TREAT THE TRIMMED MEAN CONFIDENCE LIMITS CASE  **
C               *****************************************************
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTMCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ICASAN,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ICASAN,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='TRIMMED MEAN CONFIDENCE LIMITS'
      MAXNA=100
      MINNVA=1
      MAXNVA=100
      MINNA=1
      IFLAGE=1
      IF(IREPL.EQ.'ON')THEN
        MAXNVA=7
      ELSE
        MAXNVA=100
        IFLAGE=0
      ENDIF
      MINN2=2
      IFLAGM=1
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
      IF(NUMVAR.GT.1 .AND. IREPL.EQ.'OFF')IMULT='ON'
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,181)
  181   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,182)NQ,NUMVAR,IMULT,IREPL
  182   FORMAT('NQ,NUMVAR,IMULT,IREPL = ',2I8,2X,A4,2X,A4)
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
C               ***********************************************
C               **  STEP 2--                                 **
C               **  DETERMINE:                               **
C               **  1) NUMBER OF REPLICATION VARIABLES (0-6) **
C               **  2) NUMBER OF RESPONSE    VARIABLES (>= 1)**
C               ***********************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NRESP=0
      NREPL=0
C
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSEIF(IREPL.EQ.'ON')THEN
        NRESP=1
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.1 .OR. NREPL.GT.6)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN TRIMMED MEAN CONFIDENCE LIMITS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
  211     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,213)NREPL
  213     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        NRESP=1
      ENDIF
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')THEN
        WRITE(ICOUT,221)NRESP,NREPL
  221   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO230I=1,MAXN
        W(I)=1.0
  230 CONTINUE
C
C     ******************************************************
C     **  STEP 3--                                        **
C     **  DETERMINE VALUE OF TRIMMING CONSTANTS (OBTAINED **
C     **  FROM PARAMETERS P1 AND P2)                      **
C     ******************************************************
C
C
C        2012/10: FOR TRIMMED MEAN, CAN SPECIFY EITHER A SPECIFIC NUMBER
C                 TO TRIM OR A PERCENTAGE TO TRIM.  CHECK FOR SPECIFIC
C                 NUMBER FIRST AND IF NOT SPECIFIED, CHECK FOR A
C                 PERCENTAGE.
C
        NTRIM1=-1
        NTRIM2=-1
        P1=-99.0
        P2=-99.0
C
        IH='NTRI'
        IH2='M1  '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'NO')THEN
          NTRIM1=INT(VALUE(ILOCP)+0.1)
          IF(NTRIM1.LT.0)NTRIM1=0
        ENDIF
C
        IH='NTRI'
        IH2='M2  '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'NO')THEN
          NTRIM2=INT(VALUE(ILOCP)+0.1)
          IF(NTRIM2.LT.0)NTRIM2=0
        ENDIF
C
        IF(NTRIM1.LE.0)THEN
          IH='P1  '
          IH2='    '
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          IF(PROP1.LT.0.0 .OR. PROP1.GT.100.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)
  301       FORMAT('***** ERROR IN TRIMMED MEAN CONFIDENCE LIMITS--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,302)
  302       FORMAT('      THE PROPORTION FOR TRIMMING BELOW MUST BE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,303)
  303       FORMAT('      BETWEEN 0 AND 100, BUT WAS NOT.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,304)PROP1
  304       FORMAT('      PARAMETER P1 = LOWER PROPORTION = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,305)
  305       FORMAT('      USE THE LET COMMAND TO PRE-DEFINE P1 AS IN')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,306)
  306       FORMAT('      LET P1 = 25')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSE
            PROP1=VALUE(ILOCP)
          ENDIF
        ENDIF
C
        IF(NTRIM2.LE.0)THEN
          IH='P2  '
          IH2='    '
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          IF(PROP2.LT.0.0 .OR. PROP2.GT.100.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,312)
  312       FORMAT('      THE PROPORTION FOR TRIMMING ABOVE MUST BE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,303)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,314)PROP2
  314       FORMAT('      PARAMETER P2 = LOWER PROPORTION = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,315)
  315       FORMAT('      USE THE LET COMMAND TO PRE-DEFINE P2 AS IN')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,316)
  316       FORMAT('      LET P2 = 25')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSE
            PROP2=VALUE(ILOCP)
          ENDIF
        ENDIF
C
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  GENERATE THE CONFIDENCE LIMITS FOR THE VARIOUS  **
C               **  CASES                                           **
C               ******************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 3A--                          **
C               **  CASE 1: SINGLE RESPONSE VARIABLE   **
C               **          WITH NO REPLICATION        **
C               *****************************************
C
      IF(IMULT.EQ.'OFF' .AND. NREPL.EQ.0)THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
C
        ICOL=1
        NUMVA2=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 3B--                                       **
C               **  PREPARE FOR ENTRANCE INTO DPTMC2--              **
C               **  SET THE WEIGHT VECTOR TO UNITY THROUGHOUT.      **
C               ******************************************************
C
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')THEN
          ISTEPN='3B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,331)
  331     FORMAT('***** FROM DPTMCO, AS WE ARE ABOUT TO CALL DPTMC2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,332)NLOCAL,MAXN
  332     FORMAT('NLOCAL,MAXN = ',2I8)
          CALL DPWRST('XXX','BUG ')
          DO335I=1,N
            WRITE(ICOUT,336)I,Y(I)
  336       FORMAT('I,Y(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  335     CONTINUE
        ENDIF
C
        CALL DPTMC2(Y,NLOCAL,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1              XTEMP1,XTEMP2,MAXNXT,
     1              PID,IVARID,IVARI2,NREPL,
     1              CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1              CTL999,CTU999,
     1              ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1              ICASAN,ISUBRO,IBUGA3,IERROR)
C
        IFLAGU='ON'
        IFRST=.FALSE.
        ILAST=.FALSE.
        CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1              CTL999,CTU999,
     1              IFLAGU,IFRST,ILAST,ICASAN,
     1              IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C               *******************************************
C               **  STEP 4A--                            **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES  **
C               *******************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
        NCURVE=0
        DO410IRESP=1,NRESP
          NCURVE=NCURVE+1
C
          IINDX=ICOLR(IRESP)
          PID(1)=CPUMIN
          IVARID(1)=IVARN1(IRESP)
          IVARI2(1)=IVARN2(IRESP)
C
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,411)IRESP,NCURVE
  411       FORMAT('IRESP,NCURVE = ',2I5)
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
     1                Y,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C         *****************************************************
C         **  STEP 4B--                                      **
C         *****************************************************
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TMCO')THEN
            ISTEPN='4B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,422)
  422       FORMAT('***** FROM THE MIDDLE  OF DPTMCO--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,423)ICASAN,NUMVAR,NLOCAL,IRESP
  423       FORMAT('ICASAN,NUMVAR,NLOCAL,IRESP = ',A4,3I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO425I=1,NLOCAL
                WRITE(ICOUT,426)I,Y(I)
  426           FORMAT('I,Y(I) = ',I8,F12.5)
                CALL DPWRST('XXX','BUG ')
  425         CONTINUE
            ENDIF
          ENDIF
C
          CALL DPTMC2(Y,NLOCAL,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1                XTEMP1,XTEMP2,MAXNXT,
     1                PID,IVARID,IVARI2,NREPL,
     1                CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                CTL999,CTU999,
     1                ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                ICASAN,ISUBRO,IBUGA3,IERROR)
C
          IFLAGU='FILE'
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                CTL999,CTU999,
     1                IFLAGU,IFRST,ILAST,ICASAN,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
  410   CONTINUE
C
C               ****************************************************
C               **  STEP 5A--                                     **
C               **  CASE 3: ONE OR MORE REPLICATION VARIABLES.    **
C               **          FOR THIS CASE, ALL VARIABLES MUST     **
C               **          HAVE THE SAME LENGTH.                 **
C               ****************************************************
C
      ELSEIF(IREPL.EQ.'ON')THEN
        ISTEPN='5A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO510I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO510
          J=J+1
C
C         RESPONSE VARIABLE IN Y
C
          ICOLC=1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)Y(J)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)Y(J)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)Y(J)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)Y(J)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)Y(J)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)Y(J)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)Y(J)=TAGPLO(I)
C
          IF(NREPL.GE.1)THEN
            DO520IR=1,MIN(NREPL,6)
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
  520       CONTINUE
          ENDIF
C
  510   CONTINUE
        NLOCAL=J
C
        ISTEPN='5B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
        IADD=1
        DO540II=1,NREPL
          IVARID(II+IADD)=IVARN1(II+IADD)
          IVARI2(II+IADD)=IVARN2(II+IADD)
  540   CONTINUE
C
C       *****************************************************
C       **  STEP 5C--                                      **
C       **                                                 **
C       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
C       **  VARIOUS REPLICATIONS.                          **
C       *****************************************************
C
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TMCO')THEN
          ISTEPN='5C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,541)
  541     FORMAT('***** FROM THE MIDDLE  OF DPTMCO--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,542)ICASAN,NUMVAR,NLOCAL,NREPL
  542     FORMAT('ICASAN,NUMVAR,NLOCAL,NREPL = ',A4,2X,3I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO545I=1,NLOCAL
              WRITE(ICOUT,546)I,Y(I),XDESGN(I,1),XDESGN(I,2)
  546         FORMAT('I,Y(I),XDESGN(I,1),XDESGN(I,2) = ',
     1               I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
  545       CONTINUE
          ENDIF
        ENDIF
C
C       *****************************************************
C       **  STEP 5C--                                      **
C       **  FIND THE DISTINCT VALUES IN EACH OF THE        **
C       **  REPLICATION VARIABLES.                         **
C       *****************************************************
C
        CALL DPPP5(XDESGN(1,1),XDESGN(1,2),XDESGN(1,3),
     1             XDESGN(1,4),XDESGN(1,5),XDESGN(1,6),
     1             NREPL,NLOCAL,MAXOBV,
     1             XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,
     1             XTEMP1,XTEMP2,
     1             NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,
     1             IBUGA3,ISUBRO,IERROR)
C
C       *****************************************************
C       **  STEP 5D--                                      **
C       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
C       *****************************************************
C
        NPLOTP=0
        NCURVE=0
        IF(NREPL.EQ.1)THEN
          J=0
          DO1110ISET1=1,NUMSE1
            K=0
            PID(IADD+1)=XIDTEM(ISET1)
            DO1130I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1                    XTEMP1,XTEMP2,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
C
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NUMSE1)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO1210ISET1=1,NUMSE1
          DO1220ISET2=1,NUMSE2
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            DO1290I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2)
     1          )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1                    XTEMP1,XTEMP2,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
 1220     CONTINUE
 1210     CONTINUE
        ELSEIF(NREPL.EQ.3)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3
          DO1310ISET1=1,NUMSE1
          DO1320ISET2=1,NUMSE2
          DO1330ISET3=1,NUMSE3
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            DO1390I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.
     1           XIDTE3(ISET3).EQ.XDESGN(I,3)
     1          )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1                    XTEMP1,XTEMP2,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
 1330     CONTINUE
 1320     CONTINUE
 1310     CONTINUE
        ELSEIF(NREPL.EQ.4)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4
          DO1410ISET1=1,NUMSE1
          DO1420ISET2=1,NUMSE2
          DO1430ISET3=1,NUMSE3
          DO1440ISET4=1,NUMSE4
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            DO1490I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.
     1           XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.
     1           XIDTE4(ISET4).EQ.XDESGN(I,4)
     1          )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1                    XTEMP1,XTEMP2,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
 1440     CONTINUE
 1430     CONTINUE
 1420     CONTINUE
 1410     CONTINUE
        ELSEIF(NREPL.EQ.5)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4*NUMSE5
          DO1510ISET1=1,NUMSE1
          DO1520ISET2=1,NUMSE2
          DO1530ISET3=1,NUMSE3
          DO1540ISET4=1,NUMSE4
          DO1550ISET5=1,NUMSE5
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            PID(5+IADD)=XIDTE5(ISET4)
            DO1590I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.
     1           XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.
     1           XIDTE4(ISET4).EQ.XDESGN(I,4) .AND.
     1           XIDTE5(ISET5).EQ.XDESGN(I,5)
     1          )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1                    XTEMP1,XTEMP2,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
 1550     CONTINUE
 1540     CONTINUE
 1530     CONTINUE
 1520     CONTINUE
 1510     CONTINUE
        ELSEIF(NREPL.EQ.6)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4*NUMSE5*NUMSE6
          DO1610ISET1=1,NUMSE1
          DO1620ISET2=1,NUMSE2
          DO1630ISET3=1,NUMSE3
          DO1640ISET4=1,NUMSE4
          DO1650ISET5=1,NUMSE5
          DO1660ISET6=1,NUMSE6
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            PID(5+IADD)=XIDTE5(ISET4)
            PID(6+IADD)=XIDTE6(ISET4)
            DO1690I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.
     1           XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.
     1           XIDTE4(ISET4).EQ.XDESGN(I,4) .AND.
     1           XIDTE5(ISET5).EQ.XDESGN(I,5) .AND.
     1           XIDTE6(ISET6).EQ.XDESGN(I,6)
     1          )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1                    XTEMP1,XTEMP2,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
 1660     CONTINUE
 1650     CONTINUE
 1640     CONTINUE
 1630     CONTINUE
 1620     CONTINUE
 1610     CONTINUE
        ENDIF
C
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTMCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASEQ,NRIGHT(1),NS
 9014   FORMAT('ICASEQ,NRIGHT(1),NS = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTMC2(Y,N,W,PROP1,PROP2,NTRIM1,NTRIM2,
     1                  XTEMP1,XTEMP2,MAXNXT,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                  ICASAN,ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE GENERATES TRIMMED MEAN CONFIDENCE LIMITS
C              FOR THE DATA IN THE INPUT VECTOR Y.
C     NOTE--ASSUMPTION--MODEL IS   RESPONSE = CONSTANT + ERROR.
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS
C                       N      = THE INTEGER NUMBER OF
C                                OBSERVATIONS IN THE VECTOR Y.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/2
C     ORIGINAL VERSION--FEBRUARY  2003.
C     UPDATED         --OCTOBER   2003. ADD SUPPORT FOR HTML, LATEX
C                                       OUTPUT
C
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --MARCH     2010. USE DPDTA2 AND DPDTA4 TO
C                                       GENERATE OUTPUT (ADDS RTF
C                                       SUPPORT)
C     UPDATED         --MARCH     2010. SOME MODIFICATIONS TO THE
C                                       OUTPUT (AESTHETIC, NOT
C                                       SUBSTANTIVE)
C     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*40 IRTFFF
      CHARACTER*40 IRTFFP
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION W(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION PID(*)
C
      PARAMETER (NUMALP=8)
C
      DIMENSION CONF(NUMALP)
      DIMENSION T(NUMALP)
      DIMENSION TSDM(NUMALP)
      DIMENSION ALOWER(NUMALP)
      DIMENSION AUPPER(NUMALP)
C
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=20)
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
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
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
      ISUBN1='DPTM'
      ISUBN2='C2  '
      IWRITE='OFF'
      IERROR='NO'
      ICASA2='TMCO'
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTMC2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)N,NUMDIG,PROP1,PROP2,IBUGA3,ICASAN
   52   FORMAT('N,NUMDIG,PROP1,PROP2,IBUGA3,ICASAN = ',
     1         2I8,2X,2G15.7,2X,A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),W(I)
   57     FORMAT('I,Y(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN TRIMMED MEAN--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE IS LESS THAN 5')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N
  113   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO135I=2,N
      IF(Y(I).NE.HOLD)GOTO139
  135 CONTINUE
  130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  139 CONTINUE
C
C               ***************************************************
C               **  STEP 3--                                     **
C               **  COMPUTE THE TRIMMED MEAN LOCATION ESTIMATE   **
C               **  COMPUTE THE TRIMMED MEAN STANDARD ERROR      **
C               ***************************************************
C
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      CALL TRIMME(Y,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,XTEMP1,
     1            MAXNXT,YTRMME,
     1            IBUGA3,ISUBRO,IERROR)
      CALL TRIMSE(Y,N,PROP1,PROP2,NRIM1,NTRIM2,IWRITE,XTEMP1,XTEMP2,
     1            MAXNXT,YTRMSE,
     1            IBUGA3,ISUBRO,IERROR)
C
      AN1=N
      LAMBDA=INT(AN1*(PROP1+PROP2)/100.)
      V=0.7*(AN1-1.0)
      IV=N - LAMBDA - 1
      IF(IV.LT.1)IV=1
C
C               ***************************************
C               **  STEP 4--                         **
C               **  COMPUTE CONFIDENCE LIMITS        **
C               **  FOR VARIOUS PROBABILITY VALUES.  **
C               ***************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CONF(1)=50.0
      CONF(2)=75.0
      CONF(3)=90.0
      CONF(4)=95.0
      CONF(5)=99.0
      CONF(6)=99.9
      CONF(7)=99.99
      CONF(8)=99.999
C
      DO1400I=1,8
        PCONF=CONF(I)/100.0
        CDF=0.5+PCONF/2.0
        CALL TPPF(CDF,REAL(IV),T(I))
        TSDM(I)=T(I)*YTRMSE
        ALOWER(I)=YTRMME-TSDM(I)
        AUPPER(I)=YTRMME+TSDM(I)
 1400 CONTINUE
      CUTL90=ALOWER(3)
      CUTU90=AUPPER(3)
      CUTL95=ALOWER(4)
      CUTU95=AUPPER(4)
      CUTL99=ALOWER(5)
      CUTU99=AUPPER(5)
      CTL999=ALOWER(6)
      CTU999=AUPPER(6)
C
C     ADD A FUDGE FACTOR SO THAT CONFIDENCE LEVEL WILL
C     BE PRINTED CORRECTLY TO 3 DECIMAL PLACES.
C
      CONF(1)=50.0001
      CONF(2)=75.0001
      CONF(3)=90.0001
      CONF(4)=95.0001
      CONF(5)=99.0001
      CONF(6)=99.9001
      CONF(7)=99.9901
      CONF(8)=99.9991
C
C               ****************************
C               **  STEP 7--              **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      ITITLE='Confidence Limits for the Trimmed Mean'
      NCTITL=38
      ITITLZ='(Two-Sided)'
      NCTITZ=11
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
C
      IF(NREPL.GT.0)THEN
        NRESP=1
        DO4101I=1,NREPL
          ICNT=ICNT+1
          ITEMP=I+NRESP
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
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Percentage Trimmed Below:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=PROP1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Percentage Trimmed Above:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=PROP2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Trimmed Mean:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=YTRMME
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Trimmed Mean Standard Error:'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=YTRMSE
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Degrees of Freedom:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=REAL(IV)
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO4210I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='5A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='5B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CNF2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDT11(CONF,T,TSDM,ALOWER,AUPPER,
     1            ICASA2,ICAPSW,ICAPTY,NUMDIG,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')THEN
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTMC2--')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012 FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,9013)YTRMME,YTRMSE,IV
 9013 FORMAT('YTRMME,YTRMSE,IV = ',2G15.7,I8)
      CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTNS1(Y,X,N,T,
     1                  TEMP1,
     1                  MUMOME,SDMOME,MUML,SDML,
     1                  MUMLSE,SDMLSE,COVSE,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE ESTIMATES THE PARAMETERS FOR THE
C              "DETECTION LIMIT PLOT" COMMAND.  NOTE THAT THIS
C              IS ACTUALLY A SINGLY LEFT CENSORED PROBLEM (THE
C              DISTINCTION BETWEEN CENSORING AND TRUNCATION IS
C              THAT FOR THE CENSORED CASE WE KNOW HOW MANY
C              MEASUREMENTS ARE RESTRICTED WHILE FOR THE TRUNCATED
C              CASE WE DO NOT.
C
C              THE 3-MOMENT  ESTIMATES ARE:
C
C                  SIGMA* = SQRT{(V1P**2 - V1P*V2P)/(V2P - 2*V1P**2)}
C                  MU*    = T + A*
C
C              WHERE
C
C                  A*   = (V3P - 2*V1P*V2P)/(V2P - 2*V1P**2)
C                  V1P  = XBAR - T
C                  V2P = S**2 + (XBAR - T)**2
C                  V3P = SUM[i=1 to n][(X(i) - XBAR)**3]/n
C
C              THE MAXIMUM LIKELIHOOD ESTIMATES ARE:
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
      DIMENSION TEMP1(*)
C
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVARI
      DOUBLE PRECISION DT
      DOUBLE PRECISION V1P
      DOUBLE PRECISION V2P
      DOUBLE PRECISION V3P
      DOUBLE PRECISION DNTOT
      DOUBLE PRECISION DNFULL
      DOUBLE PRECISION DC
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DNUM1
      DOUBLE PRECISION DNUM2
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DDENO2
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DOMEGA
      DOUBLE PRECISION DLAMB
      DOUBLE PRECISION DQ
      DOUBLE PRECISION DPHI11
      DOUBLE PRECISION DPHI12
      DOUBLE PRECISION DPHI22
      DOUBLE PRECISION DU11
      DOUBLE PRECISION DU12
      DOUBLE PRECISION DU22
C
      REAL MUMOME
      REAL SDMOME
      REAL MUML
      REAL SDML
      REAL MUMLSE
      REAL SDMLSE
C
      DOUBLE PRECISION AE
      DOUBLE PRECISION RE
      DOUBLE PRECISION XLOW
      DOUBLE PRECISION XUP
      DOUBLE PRECISION XMID
      DOUBLE PRECISION XI
C
      DOUBLE PRECISION TNRFUN
      EXTERNAL TNRFUN
C
      DOUBLE PRECISION DC1
      DOUBLE PRECISION DH
      COMMON/TNRCOM/DC1,DH
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
      ISUBN1='DPTN'
      ISUBN2='S1  '
C
      IERROR='NO'
      IWRITE='OFF'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LE.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN TRUNCATED NORMAL SINGLY TRUNCATED ',
     1         'PARAMETER ESTIMATION--')
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TNS1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPTNS1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)N
   71   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,N
          WRITE(ICOUT,74)I,Y(I),X(I)
   74     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C               **********************************************
C               **  STEP 2--                                **
C               **  COMPUTE SUMMARY STATISTICS              **
C               **********************************************
C
      MUMOME=0.0
      SDMOME=0.0
      MUML=0.0
      SDML=0.0
C
      NC=0
      NFULL=0
      YMIN=CPUMAX
      DSUM1=0.0D0
C
      DO1010I=1,N
        IF(X(I).GT.0.0)THEN
          NFULL=NFULL+1
          TEMP1(NFULL)=Y(I)
          DSUM1=DSUM1 + DBLE(Y(I))
          IF(Y(I).LT.YMIN)YMIN=Y(I)
        ELSE
          NC=NC+1
        ENDIF
 1010 CONTINUE
      DNFULL=DBLE(NFULL)
      DNC=DBLE(NC)
      DNTOT=DBLE(N)
      DMEAN=DSUM1/DNFULL
      IF(T.GT.CPUMIN .AND. T.LE.YMIN)THEN
        DT=DBLE(T)
      ELSE
        DT=DBLE(YMIN)
      ENDIF
C
      IF(NFULL.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1012)
 1012   FORMAT('      THE NUMBER OF UNTRUNCATED OBSERVATIONS MUST BE ',
     1         'AT LEAST 2.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1014)NFULL
 1014   FORMAT('      THE NUMBER OF UNTRUNCATED OBSERVATIONS HERE = ',
     1         I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DVARI=0.0D0
      V3P=0.0D0
      DO1020I=1,NFULL
        DVARI=DVARI + (DBLE(TEMP1(I)) - DMEAN)**2/DNFULL
        V3P=V3P + (DBLE(TEMP1(I)) - DT)**3/DNFULL
 1020 CONTINUE
      V1P=DMEAN - DT
      V2P=DVARI + (DMEAN - DT)**2
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TNS1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1031)
 1031   FORMAT('***** DPTNS1: AFTER COMPUTE SUMMARY STATISTICS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1032)N,NFULL,NC
 1032   FORMAT('N,NFULL,NC = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1033)DMEAN,DVARI,DT,V1P,V2P,V3P
 1033   FORMAT('DMEAN,DVARI,DT,V1P,V2P,V3P = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************************
C               **  STEP 3--                                **
C               **  COMPUTE 3-MOMENT ESTIMATES              **
C               **********************************************
C
      DNUM1=V2P**2 - V1P*V3P
      DDENOM=V2P - 2.0D0*V1P**2
      SDMOME=REAL(DSQRT(DNUM1/DDENOM))
      DNUM2=V3P - 2.0D0*V1P*V2P
      DDENO2=V2P - 2.0D0*V1P**2
      MUMOME=REAL(DT + (DNUM2/DDENO2))
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TNS1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1101)
 1101   FORMAT('***** DPTNS1: AFTER COMPUTE 3-MOMENT ESTIMATES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1102)DNUM1,DDENOM,SDMOME
 1102   FORMAT('DNUM1,DDENOM,SDMOME = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1103)DNUM2,DDENO2,MUMOME
 1103   FORMAT('DNUM2,DENO2,MUMOME = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************************
C               **  STEP 4--                                **
C               **  COMPUTE MAXIMUM LIKELIHOOD ESTIMATES    **
C               **********************************************
C
C     DEFINE SOME CONSTANTS FOR THE FUNCTION SOLVER
C
      DH=DNC/DNTOT
      DC1=DVARI/(DMEAN - DT)**2
C
C     USE DFZERO TO SOLVE THE LAMBDAHAT FUNCTION
C
      AE=1.D-7
      RE=1.D-7
      XLOW=-10.0D0
      XUP=10.0D0
      IF(DMEAN.GT.DT)THEN
        XMID=-1.0D0
      ELSE
        XMID=1.0D0
      ENDIF
      ITER=0
C
 1410 CONTINUE
      CALL DFZERO(TNRFUN,XLOW,XUP,XMID,RE,AE,IFLAG)
      XI=XLOW
C
C     NOW EVALUATE - CHECK FOR POSITIVE RESULT
C
      CALL NODPDF(XI,DPDF)
      CALL NODCDF(XI,DCDF)
      DOMEGA=(DH/(1.0D0-DH))*DPDF/DCDF
      DLAMB=DOMEGA/(DOMEGA - XI)
      IF(DLAMB.LT.0.0D0)THEN
        IF(ITER.EQ.0)THEN
          ITER=1
          XLOW=-10.0D0
          XUP=XI-0.1D0
          XMID=(XLOW+XUP)/2.0D0
          GOTO1410
        ELSEIF(ITER.EQ.1)THEN
          ITER=2
          XLOW=XI+0.1D0
          XUP=10.0D0
          XMID=(XLOW+XUP)/2.0D0
          GOTO1410
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1413)
 1413     FORMAT('      UNABLE TO DETERMINE MAXIMUM LIKELIHOOD ',
     1           'ESTIMATES.')
          CALL DPWRST('XXX','BUG ')
          GOTO1499
        ENDIF
      ENDIF
C
      SDML=REAL(DSQRT(DVARI + DLAMB*(DMEAN - DT)**2))
      MUML=REAL(DMEAN - DLAMB*(DMEAN - DT))
C
C     NOW COMPUTE STANDARD ERRORS
C
      IF(DCDF.GE.1.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1431)
 1431   FORMAT('***** WARNING IN TRUNCATED NORMAL SINGLY TRUNCATED ',
     1         'PARAMETER ESTIMATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1433)
 1433   FORMAT('      UNABLE TO COMPUTE STANDARD ERRORS OF THE ',
     1         'MAXIMUM LIKELIHOOD ESTIMATES.')
        CALL DPWRST('XXX','BUG ')
        GOTO1499
      ENDIF
C
      DQ=DPDF/(1.0D0 - DCDF)
      DPHI11=1.0D0 - DQ*(DQ - XI)
      DPHI12=DQ*(1.0D0 - XI*(DQ - XI))
      DPHI22=2.0D0 + XI*DPHI12
      DDENOM=DPHI11*DPHI22 - DPHI12**2
      DU11=DPHI22/DDENOM
      DU22=DPHI11/DDENOM
      DU12=-DPHI12/DDENOM
CCCCC DTERM1=DBLE(SDML)**2/DBLE(NFULL)
      DTERM1=DBLE(SDML)**2/DNTOT
      MUMLSE=REAL(DSQRT(DTERM1*DU11))
      SDMLSE=REAL(DSQRT(DTERM1*DU22))
      COVSE=REAL(DTERM1*DU12)
C
 1499 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TNS1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** DPTNS1: AFTER COMPUTE ML ESTIMATES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)DH,XI,DPDF,DCDF
 1112   FORMAT('DH,XI,DPDF,DCDF = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)DTERM1,DOMEGA,DLAMB
 1113   FORMAT('DTERM1,DOMEGA,DLAMB = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)MUML,SDML
 1114   FORMAT('MUML,SDML = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1115)DQ,DPHI11,DPHI12,DPHI22
 1115   FORMAT('DQ,DPHI11,DPHI12,DPHI22 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1116)DDENOM,DU11,DU22,DU12
 1116   FORMAT('DDENOM,DU11,DU22,DU12 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TNS1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTNS1--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTOLI(XTEMP1,XTEMP2,XTEMP3,MAXNXT,
     1                  ICASAN,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE TOLERANCE LIMITS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     EXAMPLE--TOLERANCE LIMITS Y
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/11
C     ORIGINAL VERSION--NOVEMBER  1998.
C     UPDATED         --MARCH     2011. USE DPPARS ROUTINE
C     UPATED          --MARCH     2011. REWRITTEN TO HANDLE MULTIPLE
C                                       RESPONSE VARIABLES, GROUP-ID
C                                       VARIABLES, OR A LAB-ID VARIABLE
C     UPATED          --AUGUST    2011. CHECK FOR CONFLICT WITH ABASIS AND
C                                       BBASIS TOLERANCE INTERVALS
C     UPATED          --AUGUST    2011. ADD ONE-SIDED CASE FOR NORMAL TOLERANCE
C                                       LIMITS
C     UPATED          --AUGUST    2011. ADD SUMMARY DATA FOR NORMAL TOLERANCE
C                                       LIMITS (I.E., MEAN, SD, SAMPLE SIZE)
C     UPATED          --AUGUST    2011. ADD WEIBULL TOLERANCE LIMITS
C     UPATED          --MAY       2014. ADD LOGNORMAL TOLERANCE LIMITS
C     UPATED          --MAY       2014. ADD BOX COX TOLERANCE LIMITS
C     UPATED          --JULY      2019. TWEAK SCRATCH SPACE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPAN
      CHARACTER*4 ICASDI
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASP2
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IDATSW
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*4 LOWLTY
      CHARACTER*4 UPPLTY
      CHARACTER*4 IMETHD
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
      CHARACTER*4 ICASE
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=30)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      CHARACTER*4 IVARID(1)
      CHARACTER*4 IVARI2(1)
      REAL PVAR(MAXSPN)
      REAL PID(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION Y1(MAXOBV)
C
      DIMENSION XDESGN(MAXOBV,7)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
C
      DIMENSION TEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP1(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE4(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE5(1))
      EQUIVALENCE (GARBAG(IGARB8),XIDTE6(1))
      EQUIVALENCE (GARBAG(IGARB9),XDESGN(1,1))
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOMC.INC'
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
      ICASAN='TOLE'
      ICASA2='TWOS'
      ICASDI='NORM'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPTO'
      ISUBN2='LI  '
      XMEAN=CPUMIN
      XSD=CPUMIN
      AN=CPUMIN
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***********************************************
C               **  TREAT THE TOLERANCE LIMITS TEST  CASE    **
C               ***********************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TOLI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTOLI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASAN
   52   FORMAT('ICASAN = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE COMMAND                            **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:        **
C               **    1) TOLERANCE LIMITS Y                        **
C               **    2) MULTIPLE TOLERANCE LIMITS Y1 ... YK       **
C               **    3) REPLICATED TOLERANCE LIMITS Y X1 ... XK   **
C               *****************************************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTC=9999
      ILASTZ=9999
      ICASAN='TOLE'
      IDATSW='RAW'
C
      DO100I=0,NUMARG-1
C
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
        ELSE
          ICTMP1=IHARG(I)
        ENDIF
        ICTMP2=IHARG(I+1)
        ICTMP3=IHARG(I+2)
        ICTMP4=IHARG(I+3)
C
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'ABAS')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'A   ' .AND. ICTMP2.EQ.'BASI')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'BBAS')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'B   ' .AND. ICTMP2.EQ.'BASI')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'TOLE' .AND.
     1        (ICTMP2.EQ.'LIMI' .OR. ICTMP2.EQ.'INTE'))THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'TOLE')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'MULT')THEN
          IMULT='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'NORM')THEN
          ICASAN='NTOL'
          ICASDI='NORM'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'WEIB')THEN
          ICASDI='WEIB'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'LOGN')THEN
          ICASAN='LNTO'
          ICASDI='LOGN'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'BOXC')THEN
          ICASAN='BCTO'
          ICASDI='BOXC'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'BOX' .AND. ICTMP2.EQ.'COX')THEN
          ICASAN='BCTO'
          ICASDI='BOXC'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'LOWE')THEN
          ICASA2='LOWE'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'UPPE')THEN
          ICASA2='UPPE'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'SUMM')THEN
          IDATSW='SUMM'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'NONP')THEN
          ICASAN='NPTO'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ENDIF
  100 CONTINUE
C
      IF(IFOUND.EQ.'NO')GOTO9000
C
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGA2,IERROR)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')THEN
        WRITE(ICOUT,91)ICASAN,IMULT,IREPL,ISHIFT
   91   FORMAT('DPTOLI: ICASAN,IMULT,IREPL,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN TOLERANCE LIMITS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE TOLERANCE LIMITS TEST COMMAND.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(IDATSW.EQ.'SUMM')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,112)
  112     FORMAT('      YOU CANNOT SPECIFY BOTH "SUMMARY" AND ',
     1           '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(IMULT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)
  122     FORMAT('      YOU CANNOT SPECIFY BOTH "SUMMARY" AND ',
     1           '"MULTIPLE"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(ICASDI.EQ.'WEIB')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,132)
  132     FORMAT('      YOU CANNOT SPECIFY BOTH "SUMMARY" AND ',
     1           '"WEIBULL"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
        ELSEIF(ICASDI.EQ.'LOGN')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,142)
  142     FORMAT('      YOU CANNOT SPECIFY BOTH "SUMMARY" AND ',
     1           '"LOGNORMAL"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
        ENDIF
      ENDIF
C
C               *********************************
C               **  STEP 4--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='TOLERANCE LIMITS'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      IF(IREPL.EQ.'ON')THEN
        IFLAGM=0
        IFLAGE=1
      ENDIF
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=MAXSPN
      IF(IDATSW.EQ.'SUMM')THEN
        MINN2=1
        IFLAGM=0
        IFLAGP=19
        MINNVA=3
        MAXNVA=3
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
     1            IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')THEN
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
C               ***********************************************
C               **  STEP 5--                                 **
C               **  DETERMINE:                               **
C               **  1) NUMBER OF REPLICATION VARIABLES (0-6) **
C               **  2) NUMBER OF RESPONSE    VARIABLES (>= 1)**
C               ***********************************************
C
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IDATSW.EQ.'SUMM')GOTO599
      NRESP=0
      NREPL=0
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSEIF(IREPL.EQ.'ON')THEN
        NRESP=1
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.1 .OR. NREPL.GT.6)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN ONE AND SIX.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        NRESP=NUMVAR
        IMULT='ON'
      ENDIF
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
  599 CONTINUE
C
C               ******************************************************
C               **  STEP 6--                                        **
C               **  GENERATE THE TOLERANCE LIMITS TEST FOR THE      **
C               **  VARIOUS CASES                                   **
C               ******************************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************************
C               **  STEP 7A--                           **
C               **  CASE 0: SUMMARY CASE                **
C               ******************************************
C
      IF(IDATSW.EQ.'SUMM')THEN
        ISTEPN='7A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       TWO CASES: EITHER DATA ENTERED AS 3 PARAMETERS OR
C                  AS 3 VARIABLES
C
        NREPL=0
        IF(IVARTY(1).EQ.'PARA')THEN
          XMEAN=PVAR(1)
          XSD=PVAR(2)
          AN=PVAR(3)
          PID(1)=CPUMIN
          IVARID(1)='ROW '
          IVARI2(1)='1  '
          IF(ICASA2.EQ.'LOWE')THEN
            CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,
     1                ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                PID,IVARID,IVARI2,NREPL,
     1                ISUBRO,IBUGA3,IERROR)
          ELSEIF(ICASA2.EQ.'UPPE')THEN
            CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,
     1                ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                PID,IVARID,IVARI2,NREPL,
     1                ISUBRO,IBUGA3,IERROR)
          ELSE
            CALL TOL(Y1,NLOCAL,XMEAN,XSD,AN,
     1               XTEMP1,XTEMP2,XTEMP3,
     1               ICASAN,ICAPSW,ICAPTY,IFORSW,
     1               PID,IVARID,IVARI2,NREPL,
     1               ISUBRO,IBUGA3,IERROR)
          ENDIF
        ELSE
          ICOL=1
          NUMVA2=3
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          DO710IROW=1,NLOCAL
C
            PID(1)=CPUMIN
            IVARID(1)='ROW '
            WRITE(IVARI2(1)(1:4),'(I4)')IROW
            XMEAN=Y1(IROW)
            XSD=XTEMP1(IROW)
            AN=XTEMP2(IROW)
C
            IF(ICASA2.EQ.'LOWE')THEN
              CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,
     1                  ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  ISUBRO,IBUGA3,IERROR)
            ELSEIF(ICASA2.EQ.'UPPE')THEN
              CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,
     1                  ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  ISUBRO,IBUGA3,IERROR)
            ELSE
              CALL TOL(Y1,NLOCAL,XMEAN,XSD,AN,
     1                 XTEMP1,XTEMP2,XTEMP3,
     1                 ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                 PID,IVARID,IVARI2,NREPL,
     1                 ISUBRO,IBUGA3,IERROR)
            ENDIF
C
  710     CONTINUE
        ENDIF
        GOTO9000
      ENDIF
C
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 1: NO REPLICATION VARIABLES    **
C               ******************************************
C
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
        NCURVE=0
        DO810IRESP=1,NRESP
          NCURVE=NCURVE+1
C
          IINDX=ICOLR(IRESP)
          PID(1)=CPUMIN
          IVARID(1)=IVARN1(IRESP)
          IVARI2(1)=IVARN2(IRESP)
C
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')THEN
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
     1                Y1,XTEMP1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C         *****************************************************
C         **  STEP 8B--                                      **
C         *****************************************************
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TOLI')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPTOLI--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,IDATSW,NLOCAL
  823       FORMAT('ICASAN,NUMVAR,IDATSW,NQ = ',
     1             A4,I8,2X,A4,I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO825I=1,NLOCAL
                WRITE(ICOUT,826)I,Y1(I)
  826           FORMAT('I,Y1(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
C
          IF(ICASDI.EQ.'WEIB')THEN
            CALL TOLWEI(Y1,NLOCAL,
     1                  MINMAX,IWEIBC,XTEMP1,DTEMP1,
     1                  ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  ISUBRO,IBUGA3,IERROR)
          ELSEIF(ICASA2.EQ.'LOWE')THEN
            CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,
     1                ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                PID,IVARID,IVARI2,NREPL,
     1                ISUBRO,IBUGA3,IERROR)
          ELSEIF(ICASA2.EQ.'UPPE')THEN
            CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,
     1                ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                PID,IVARID,IVARI2,NREPL,
     1                ISUBRO,IBUGA3,IERROR)
          ELSE
            CALL TOL(Y1,NLOCAL,XMEAN,XSD,AN,
     1               XTEMP1,XTEMP2,XTEMP3,
     1               ICASAN,ICAPSW,ICAPTY,IFORSW,
     1               PID,IVARID,IVARI2,NREPL,
     1               ISUBRO,IBUGA3,IERROR)
          ENDIF
C
  810   CONTINUE
C
C               ****************************************************
C               **  STEP 9A--                                     **
C               **  CASE 3: ONE OR MORE REPLICATION VARIABLES.    **
C               **          FOR THIS CASE, THE NUMBER OF RESPONSE **
C               **          VARIABLES MUST BE EXACTLY 1.          **
C               **          FOR THIS CASE, ALL VARIABLES MUST     **
C               **          HAVE THE SAME LENGTH.                 **
C               ****************************************************
C
      ELSEIF(NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')
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
          ICOLC=1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)Y1(J)=TAGPLO(I)
C
          IF(NREPL.GE.1)THEN
            DO920IR=1,MIN(NREPL,6)
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
  920       CONTINUE
          ENDIF
C
  910   CONTINUE
        NLOCAL=J
C
C       *****************************************************
C       **  STEP 9B--                                      **
C       **  CALL TOL    TO PERFORM TOLERANCE LIMITS TEST.  **
C       *****************************************************
C
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TOLI')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPTOLI--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,942)ICASAN,NUMVAR,NLOCAL,NREPL
  942     FORMAT('ICASAN,NUMVAR,NLOCAL,NREPL = ',
     1           A4,3I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO945I=1,NLOCAL
              WRITE(ICOUT,946)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  946         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2) = ',
     1               I8,4F12.5)
              CALL DPWRST('XXX','BUG ')
  945       CONTINUE
          ENDIF
        ENDIF
C
C       *****************************************************
C       **  STEP 9C--                                      **
C       **  FIND THE DISTINCT VALUES IN EACH OF THE        **
C       **  REPLICATION VARIABLES.                         **
C       *****************************************************
C
        CALL DPPP5(XDESGN(1,1),XDESGN(1,2),XDESGN(1,3),
     1             XDESGN(1,4),XDESGN(1,5),XDESGN(1,6),
     1             NREPL,NLOCAL,MAXOBV,
     1             XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,
     1             XTEMP1,XTEMP2,
     1             NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,
     1             IBUGA3,ISUBRO,IERROR)
C
C       *****************************************************
C       **  STEP 9D--                                      **
C       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
C       *****************************************************
C
        NPLOTP=0
        NCURVE=0
        IADD=1
C
        IF(NREPL.EQ.1)THEN
          J=0
          DO1110ISET1=1,NUMSE1
            K=0
            PID(IADD+1)=XIDTEM(ISET1)
            DO1130I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,
     1                      MINMAX,IWEIBC,XTEMP1,DTEMP1,
     1                      ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                      PID,IVARID,IVARI2,NREPL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                   XTEMP1,XTEMP2,XTEMP3,
     1                   ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                   PID,IVARN1,IVARN2,NREPL,
     1                   ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO1210ISET1=1,NUMSE1
          DO1220ISET2=1,NUMSE2
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            DO1290I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2)
     1          )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,
     1                      MINMAX,IWEIBC,XTEMP1,DTEMP1,
     1                      ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                      PID,IVARID,IVARI2,NREPL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                   XTEMP1,XTEMP2,XTEMP3,
     1                   ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                   PID,IVARN1,IVARN2,NREPL,
     1                   ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1220     CONTINUE
 1210     CONTINUE
        ELSEIF(NREPL.EQ.3)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3
          DO1310ISET1=1,NUMSE1
          DO1320ISET2=1,NUMSE2
          DO1330ISET3=1,NUMSE3
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            DO1390I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.
     1           XIDTE3(ISET3).EQ.XDESGN(I,3)
     1          )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,
     1                      MINMAX,IWEIBC,XTEMP1,DTEMP1,
     1                      ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                      PID,IVARID,IVARI2,NREPL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                   XTEMP1,XTEMP2,XTEMP3,
     1                   ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                   PID,IVARN1,IVARN2,NREPL,
     1                   ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1330     CONTINUE
 1320     CONTINUE
 1310     CONTINUE
        ELSEIF(NREPL.EQ.4)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4
          DO1410ISET1=1,NUMSE1
          DO1420ISET2=1,NUMSE2
          DO1430ISET3=1,NUMSE3
          DO1440ISET4=1,NUMSE4
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            DO1490I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.
     1           XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.
     1           XIDTE4(ISET4).EQ.XDESGN(I,4)
     1          )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,
     1                      MINMAX,IWEIBC,XTEMP1,DTEMP1,
     1                      ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                      PID,IVARID,IVARI2,NREPL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                   XTEMP1,XTEMP2,XTEMP3,
     1                   ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                   PID,IVARN1,IVARN2,NREPL,
     1                   ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1440     CONTINUE
 1430     CONTINUE
 1420     CONTINUE
 1410     CONTINUE
        ELSEIF(NREPL.EQ.5)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4*NUMSE5
          DO1510ISET1=1,NUMSE1
          DO1520ISET2=1,NUMSE2
          DO1530ISET3=1,NUMSE3
          DO1540ISET4=1,NUMSE4
          DO1550ISET5=1,NUMSE5
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            PID(5+IADD)=XIDTE5(ISET4)
            DO1590I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.
     1           XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.
     1           XIDTE4(ISET4).EQ.XDESGN(I,4) .AND.
     1           XIDTE5(ISET5).EQ.XDESGN(I,5)
     1          )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,
     1                      MINMAX,IWEIBC,XTEMP1,DTEMP1,
     1                      ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                      PID,IVARID,IVARI2,NREPL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                   XTEMP1,XTEMP2,XTEMP3,
     1                   ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                   PID,IVARN1,IVARN2,NREPL,
     1                   ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1550     CONTINUE
 1540     CONTINUE
 1530     CONTINUE
 1520     CONTINUE
 1510     CONTINUE
        ELSEIF(NREPL.EQ.6)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4*NUMSE5*NUMSE6
          DO1610ISET1=1,NUMSE1
          DO1620ISET2=1,NUMSE2
          DO1630ISET3=1,NUMSE3
          DO1640ISET4=1,NUMSE4
          DO1650ISET5=1,NUMSE5
          DO1660ISET6=1,NUMSE6
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            PID(5+IADD)=XIDTE5(ISET4)
            PID(6+IADD)=XIDTE6(ISET4)
            DO1690I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.
     1           XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.
     1           XIDTE4(ISET4).EQ.XDESGN(I,4) .AND.
     1           XIDTE5(ISET5).EQ.XDESGN(I,5) .AND.
     1           XIDTE6(ISET6).EQ.XDESGN(I,6)
     1          )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,
     1                      MINMAX,IWEIBC,XTEMP1,DTEMP1,
     1                      ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                      PID,IVARID,IVARI2,NREPL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                    ICASA2,ICAPSW,ICAPTY,IFORSW,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,
     1                   XTEMP1,XTEMP2,XTEMP3,
     1                   ICASAN,ICAPSW,ICAPTY,IFORSW,
     1                   PID,IVARN1,IVARN2,NREPL,
     1                   ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1660     CONTINUE
 1650     CONTINUE
 1640     CONTINUE
 1630     CONTINUE
 1620     CONTINUE
 1610     CONTINUE
        ENDIF
C
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IERROR.EQ.'YES')THEN
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9001)(IANS(I),I=1,MIN(100,IWIDTH))
 9001     FORMAT(100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TOLI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTOLI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASAN
 9012   FORMAT('IFOUND,IERROR,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTOL3(X,N,XMEAN,XSD,AN,ANU,
     1                 ICASAN,ALPHA,GAMMA,ITOLGC,ITOLM2,
     1                 AK,ALOWLM,AUPPLM,
     1                 ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES NORMAL ONE-SIDED AND
C              TWO-SIDED NORMAL TOLERANCE LOWER AND UPPER LIMITS
C              AND K-FACTORS.  THIS IS FOR USE BY THE "STATISTICS"
C              COMMAND.
C     INPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED OR SORTED) OBSERVATIONS.
C                      N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X.
C     OTHER DATAPAC   SUBROUTINES NEEDED--CHSPPF, NORPPF, NCTPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--GARDINER AND HULL, TECHNOMETRICS, 1966, PAGES 115-122
C               --WILKS, ANNALS OF MATHEMATICAL STATISTICS, 1941, PAGE 92
C               --MOOD AND GRABLE, PAGES 416-417
C               --HOWE (1969), "TWO-SIDED TOLERANCE LIMITS FOR NORMAL
C                 POPULATIONS - SOME IMPROVEMENTS", JOURNAL OF THE
C                 AMERICAN STATISTICAL ASSOCIATION, VOL. 64, PP.
C                 610-620.
C               --GUENTHER (1977), "SAMPLING INSPECTION IN STATISTICAL
C                 QUALITY CONTROL", GRIFFIN'S STATISTICAL MONOGRAPHS,
C                 NUMBER 37, LONDON.
C               --MARY NATRELLA (1963), "EXPERIMENTAL STATISTICS, NBS
C                 HANDBOOK 91", US DEPARTMENT OF COMMERCE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     ORIGINAL VERSION--MARCH     2011. 
C     UPDATED         --MAY       2018. OPTION FOR NU (DEGREES OF
C                                       FREEDOM INDEPENDENT OF
C                                       CURRENT SAMPLE)
C     UPDATED         --MAY       2018. FOR 2-SIDED CASE, ADJUST FORMULA
C                                       SO THAT COVERAGE FACTORS < 0.5
C                                       WILL BE COMPUTED CORRECTLY.
C     UPDATED         --MAY       2018. OPTIONALLY COMPUTE GUENTHER
C                                       CORRECTION TO HOWE FORMULA
C                                       FOR TWO-SIDED CASE
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ITOLGC
      CHARACTER*4 ITOLM2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      DOUBLE PRECISION DTEMP
      DOUBLE PRECISION DTEMP2
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DTERM1
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
      ISUBN1='TOL3'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTOL3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,N,ALPHA,GAMMA
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,N,ALPHA,GAMMA = ',
     1         3(A4,2X),I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)XMEAN,XSD,ITOLGC,ITOLM2
   53   FORMAT('XMEAN,XSD,ITOLGC,ITOLM2 = ',2G15.7,2(2X,A4))
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
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL3')
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
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     COMPUTE MEAN AND STANDARD DEVIATION
C
      ALOWLM=CPUMIN
      AUPPLM=CPUMIN
      AK=CPUMIN
      AN=REAL(N)
      IF(XMEAN.EQ.CPUMIN)THEN
        CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
        CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
      ELSE
         N=INT(AN+0.1)
      ENDIF
C
C     NOTE: ALPHA IS THE CONFIDENCE AND GAMMA IS THE COVERAGE
C
      IF(ALPHA.GE.1.0 .AND. ALPHA.LT.100.0)THEN
        ALPHA=ALPHA/100.
      ENDIF
      IF(ALPHA.GT.0.0 .AND. ALPHA.LT.1.0)THEN
        IF(ALPHA.LT.0.5)ALPHA=1.0 - ALPHA
      ELSE
        ALPHA=0.95
      ENDIF
C
      IF(GAMMA.GE.1.0 .AND. GAMMA.LT.100.0)THEN
        GAMMA=GAMMA/100.
      ENDIF
      IF(GAMMA.GT.0.0 .AND. GAMMA.LT.1.0)THEN
        IF(GAMMA.LT.0.5)GAMMA=1.0 - GAMMA
      ELSE
        GAMMA=0.95
      ENDIF
C
C     COMPUTE THE NORMAL TWO-SIDED TOLERANCE LIMITS USING HOWE'S METHOD,
C     OPTIONALLY APPLY GUENTHER'S CORRECTION
C
      AN=REAL(N)
      IF(ICASAN(1:1).EQ.'2')THEN
        IF(PTOLDF.GT.0.0)THEN
          NU=INT(ANU+0.5)
        ELSE
          NU=N-1
        ENDIF
        IF(NU.LT.1)NU=1
        AN=REAL(N)
        ANU=REAL(NU)
        TERM2=ANU*(1.0 + (1.0/AN))
        PCOV=GAMMA
        PCONF=ALPHA
        TERM1=(1.0 + PCOV)/2.0
        CALL NORPPF(TERM1,Z)
        AVAL=1.0 - PCONF
        CALL CHSPPF(AVAL,NU,TERM3)
        AK=Z*SQRT(TERM2/TERM3)
C
C       APPLY GUENTHER CORRECTION IF REQUESTED
C
        IF(ITOLGC.EQ.'ON')THEN
           ANUM=AN - 3.0 - TERM3
           DENOM=2.0*(AN+1)**2
           TERM4=SQRT(1.0 + (ANUM/DENOM))
           AK=TERM4*AK
        ENDIF
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TOL3')THEN
          WRITE(ICOUT,201)AN,ANU,TERM2,TERM1,Z,AVAL,TERM3
  201     FORMAT('AN,ANU,TERM2,TERM1,Z,AVAL,TERM3 = ',7G15.7)
          CALL DPWRST('XXX','WRIT')
          IF(ITOLGC.EQ.'ON')THEN
            WRITE(ICOUT,203)ANUM,DENOM,TERM4
  203       FORMAT('ANUM,DENOM,TERM4 = ',3G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
        ENDIF
C
      ELSEIF(ICASAN(1:1).EQ.'1')THEN
C
C       FOR ONE-SIDED INTERVAL, USE EITHER APPROXIMATION BASED ON
C       NON-CENTRAL T OR APPROXIMATION BASED ON NORMAL.  THE
C       NON-CENTRAL T IS CONSIDERED MORE ACCURATE, BUT NON-CENTRAL T
C       MAY LOSE ACCURACY AS N BECOMES LARGE.
C
        IF(ITOLM2.EQ.'NONC')THEN
          IF(PTOLDF.GT.0.0)THEN
            NU=INT(PTOLDF+0.5)
            IF(NU.LT.2)NU=2
            AF=REAL(NU-1)
          ELSE
            AF=AN - 1.0
          ENDIF
          CALL NODPPF(DBLE(GAMMA),DTEMP)
          DELTA=REAL(DTEMP*DSQRT(DBLE(N)))
          CALL NCTPPF(ALPHA,AF,DELTA,PPF)
          AK=PPF/SQRT(AN)
        ELSE
          CALL NODPPF(DBLE(ALPHA),DTEMP)
          DA=1.0D0 - DTEMP**2/(2.0*(DBLE(N) - 1.0D0))
          CALL NODPPF(DBLE(GAMMA),DTEMP2)
          DB=DTEMP2**2 - DTEMP**2/DBLE(N)
          DTERM1=(DTEMP2 + DSQRT(DTEMP2**2 - DA*DB))/DA
          AK=REAL(DTERM1)
        ENDIF
      ELSE
        IERROR='YES'
        GOTO9000
      ENDIF
      ALOWLM=XMEAN - AK*XSD
      AUPPLM=XMEAN + AK*XSD
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TOL3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9051)
 9051   FORMAT('**** AT THE END OF DPTOL3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9052)XBAR,XSD,AK,ALOWLM,AUPPLM
 9052   FORMAT('XBAR,XSD,AK,ALOWLM,AUPPLM = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9054)ALPHA,GAMMA,AN
 9054   FORMAT('ALPHA,GAMMA,N = ',2G15.7,I8)
        CALL DPWRST('XXX','WRIT')
        IF(ICASAN(1:1).EQ.'2')THEN
          WRITE(ICOUT,9056)NU,DTEMP,ANP,AK
 9056     FORMAT('NU,DTEMP,ANP,AK = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
        ELSE
          WRITE(ICOUT,9058)AF,DTEMP,DELTA,PPF
 9058     FORMAT('AF,DTEMP,DELTA,PPF = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
      ENDIF
C
      RETURN
      END 
      SUBROUTINE DPTPCO(IHARG,NUMARG,IDETPC,MAXTEX,ITEPCO,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TEXT PATTERN COLORS = THE COLORS
C              OF THE LINES MAKING UP A PATTERN WITHIN A TEXT.
C              THESE ARE LOCATED IN THE VECTOR ITEPCO(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDETPC
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ITEPCO (A CHARACTER VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDETPC
      CHARACTER*4 ITEPCO
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION ITEPCO(*)
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
      ISUBN1='DPTP'
      ISUBN2='CO  '
C
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTPCO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXTEX,NUMTEX
   53 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,IHOLD2
   54 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IDETPC
   55 FORMAT('IDETPC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEPCO(1)
   70 FORMAT('ITEPCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ITEPCO(I)
   76 FORMAT('I,ITEPCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      GOTO1150
C
 1120 CONTINUE
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      ITEPCO(1)=IDETPC
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPC
      ITEPCO(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEPCO(I)
 1276 FORMAT('THE COLOR OF TEXT PATTERN ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPC
      DO1315I=1,NUMTEX
      ITEPCO(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEPCO(I)
 1316 FORMAT('THE COLOR OF ALL TEXT PATTERNS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTPCO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXTEX,NUMTEX
 9013 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,IHOLD2
 9014 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IDETPC
 9015 FORMAT('IDETPC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEPCO(1)
 9030 FORMAT('ITEPCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,ITEPCO(I)
 9036 FORMAT('I,ITEPCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTPLI(IHARG,IHARG2,NUMARG,IDETPL,MAXTEX,ITEPLI,
CCCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
CCCC  SUBROUTINE DPTPLI(IHARG,NUMARG,IDETPL,MAXTEX,ITEPLI,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE PATTERN LINES = THE LINES TYPES
C              OF THE PATTERN WITHIN THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR ITEPLI(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDETPL
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ITEPLI (A CHARACTER VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C     UPDATED         --AUGUST    1995.  DASH2 BUG
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IDETPL
      CHARACTER*4 ITEPLI
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
      DIMENSION ITEPLI(*)
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
      ISUBN1='DPTP'
      ISUBN2='LI  '
C
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTPLI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXTEX,NUMTEX
   53 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,IHOLD2
   54 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IDETPL
   55 FORMAT('IDETPL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEPLI(1)
   70 FORMAT('ITEPLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ITEPLI(I)
   76 FORMAT('I,ITEPLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO9000
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      IF(NUMARG.EQ.5)GOTO1150
      GOTO1160
C
 1130 CONTINUE
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(5).EQ.'ALL')IHOLD1='    '
      IF(IHARG(5).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
CCCCC APRIL 1996.  CHANGE IHOLD TO IHOLD1 BELOW
      IF(IHARG(5).EQ.'ALL')THEN
        IHOLD1=IHARG(6)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'5')IHOLD1='DA5'
        GOTO1300
      ENDIF
      IF(IHARG(6).EQ.'ALL')THEN
        IHOLD1=IHARG(5)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'5')IHOLD1='DA5'
        GOTO1300
      ENDIF
      GOTO1200
C
 1160 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.3)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      ITEPLI(1)='    '
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-3
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+3
      IHOLD1=IHARG(J)
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'2')IHOLD1='DA2'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'3')IHOLD1='DA3'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'4')IHOLD1='DA4'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'5')IHOLD1='DA5'
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPL
      ITEPLI(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEPLI(I)
 1276 FORMAT('THE LINE TYPE FOR TEXT PATTERN ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPL
      DO1315I=1,NUMTEX
      ITEPLI(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEPLI(I)
 1316 FORMAT('THE LINE TYPE FOR ALL TEXT PATTERNS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTPLI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXTEX,NUMTEX
 9013 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,IHOLD2
 9014 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IDETPL
 9015 FORMAT('IDETPL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEPLI(1)
 9030 FORMAT('ITEPLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,ITEPLI(I)
 9036 FORMAT('I,ITEPLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTPSP(IHARG,IARGT,ARG,NUMARG,PDETPS,MAXTEX,PTEPSP,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TEXT PATTERN SPACINGS = THE SPACINGS
C              BETWEEN THE LINES WHICH MAKE UP THE PATTERNS WITHIN THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR PTEPSP(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --PDETPS
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--PTEPSP (A FLOATING POINT VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PTEPSP(*)
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
      ISUBN1='DPTP'
      ISUBN2='SP  '
C
      NUMTEX=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTPSP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXTEX,NUMTEX
   53 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,HOLD1,HOLD2
   54 FORMAT('IHOLD1,HOLD1,HOLD2 = ',A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)PDETPS
   55 FORMAT('PDETPS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PTEPSP(1)
   70 FORMAT('PTEPSP(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,PTEPSP(I)
   76 FORMAT('I,PTEPSP(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      GOTO1150
C
 1120 CONTINUE
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDETPS
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      PTEPSP(1)=PDETPS
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      HOLD1=ARG(J)
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETPS
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETPS
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETPS
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETPS
      PTEPSP(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,PTEPSP(I)
 1276 FORMAT('THE SPACING BETWEEN (LINES WITHIN) PATTERN ',I6,
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETPS
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETPS
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETPS
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETPS
      DO1315I=1,NUMTEX
      PTEPSP(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PTEPSP(I)
 1316 FORMAT('THE SPACING BETWEEN (LINES WITHIN) ALL PATTERNS',
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTPSP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXTEX,NUMTEX
 9013 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,HOLD1,HOLD2
 9014 FORMAT('IHOLD1,HOLD1,HOLD2 = ',A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)PDETPS
 9015 FORMAT('PDETPS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PTEPSP(1)
 9030 FORMAT('PTEPSP(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,PTEPSP(I)
 9036 FORMAT('I,PTEPSP(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTPTH(IHARG,IARGT,ARG,NUMARG,PDETPT,MAXTEX,PTEPTH,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TEXT PATTERN THICKNESSES = THE THICKNESSES
C              OF THE LINES WHICH MAKE UP THE PATTERNS WITHIN THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR PTEPTH(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --PDETPT
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--PTEPTH (A FLOATING POINT VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PTEPTH(*)
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
      ISUBN1='DPTP'
      ISUBN2='TH  '
C
      NUMTEX=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTPTH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXTEX,NUMTEX
   53 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,HOLD1,HOLD2
   54 FORMAT('IHOLD1,HOLD1,HOLD2 = ',A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)PDETPT
   55 FORMAT('PDETPT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PTEPTH(1)
   70 FORMAT('PTEPTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,PTEPTH(I)
   76 FORMAT('I,PTEPTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      GOTO1150
C
 1120 CONTINUE
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDETPT
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(2)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      PTEPTH(1)=PDETPT
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      HOLD1=ARG(J)
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETPT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETPT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETPT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETPT
      PTEPTH(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,PTEPTH(I)
 1276 FORMAT('THE THICKNESS OF (LINES WITHIN) PATTERN ',I6,
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETPT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETPT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETPT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETPT
      DO1315I=1,NUMTEX
      PTEPTH(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PTEPTH(I)
 1316 FORMAT('THE THICKNESS OF (LINES WITHIN) ALL PATTERNS',
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTPTH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXTEX,NUMTEX
 9013 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,HOLD1,HOLD2
 9014 FORMAT('IHOLD1,HOLD1,HOLD2 = ',A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)PDETPT
 9015 FORMAT('PDETPT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PTEPTH(1)
 9030 FORMAT('PTEPTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,PTEPTH(I)
 9036 FORMAT('I,PTEPTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTPTY(IHARG,NUMARG,IDETPT,MAXTEX,ITEPTY,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE PATTERN TYPES = THE TYPES
C              OF THE PATTERN WITHIN THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR ITEPTY(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDETPT
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ITEPTY (A CHARACTER VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDETPT
      CHARACTER*4 ITEPTY
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION ITEPTY(*)
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
      ISUBN1='DPTP'
      ISUBN2='TY  '
C
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTPTY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXTEX,NUMTEX
   53 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,IHOLD2
   54 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IDETPT
   55 FORMAT('IDETPT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEPTY(1)
   70 FORMAT('ITEPTY(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ITEPTY(I)
   76 FORMAT('I,ITEPTY(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      GOTO1150
C
 1120 CONTINUE
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      ITEPTY(1)='    '
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPT
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPT
      ITEPTY(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEPTY(I)
 1276 FORMAT('THE TYPE FOR TEXT PATTERN ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPT
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPT
      DO1315I=1,NUMTEX
      ITEPTY(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEPTY(I)
 1316 FORMAT('THE TYPE FOR ALL TEXT PATTERNS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTPTY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXTEX,NUMTEX
 9013 FORMAT('MAXTEX,NUMTEX = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,IHOLD2
 9014 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IDETPT
 9015 FORMAT('IDETPT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEPTY(1)
 9030 FORMAT('ITEPTY(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,ITEPTY(I)
 9036 FORMAT('I,ITEPTY(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTREN(XTEMP1,XTEMP2,MAXNXT,
     1                  ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT 3 TRENDS TEST FOR RELIABILITY ANALYSIS.
C              1) REVERSE ARRANGEMENTS TEST
C              2) MILITARY HANDBOOK TEST
C              3) LAPLACE TEST
C     EXAMPLES--LET TEND = <VALUE>; RELIABILITY TREND TEST Y
C             --LET TEND = <VALUE>; RELIABILITY TREND TEST Y GROUPID
C             --RELIABILITY TREND TEST Y GROUPID CENSOR
C     REFERENCE--TOBIAS AND TRINDADE (1995), "APPLIED RELIABILITY
C                ANALYSIS", SECOND EDITION, CHAPMAN & HALL/CRC,
C                PP. 344-354.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/5
C     ORIGINAL VERSION--MAY       1998.
C     UPDATED         --OCTOBER   2006. SUPPORT FOR MULTIPLE SYSTEMS
C     UPDATED         --OCTOBER   2006. CAPTURE HTML/LATEX/RTF
C     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR3
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
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 ICASE
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
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
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
C
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XCEN(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB3),XCEN(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP5(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMP6(1))
C
C-----COMMON----------------------------------------------------------
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
      ISUBN1='DPTR'
      ISUBN2='EN  '
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
      NGROUP=0
      NCENS=0
C
C               **********************************
C               **  TREAT THE TRENDS TEST CASE  **
C               **********************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TREN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTREN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TREN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='RELIABILITY TREND TEST'
      MINNA=1
      MAXNA=100
      MINN2=4
      IFLAGE=1
      IFLAGM=9
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=3
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TREN')THEN
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
     1                      ICOLR(I),PVAR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I),PVAR(I) = ',I8,2X,A4,A4,2X,3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,X1,XCEN,NS,NGROUP,NCENS,ICASE,
     1            IBUGA3,ISUBRO,IFOUND,IERROR)
C
C               *****************************************
C               **  STEP 3--                           **
C               **  CHECK TO SEE THE IF THE PARAMETER  **
C               **  TEND (TO SPECIFY THE CENSORING TIME)*
C               *****************************************
C
      IHP='TEND'
      IHP2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        TEND=CPUMIN
      ELSE
        TEND=VALUE(ILOCP)
      ENDIF
C
C               ***********************************************
C               **  STEP 4--                                 **
C               **  PREPARE FOR ENTRANCE INTO DPTREN2--      **
C               ***********************************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TREN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TREN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** FROM DPTREN, AS WE ARE ABOUT TO CALL DPTRE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)NS
 1212   FORMAT('NS = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO1215I=1,NS
          WRITE(ICOUT,1216)I,Y1(I),X1(I),XCEN(I)
 1216     FORMAT('I,Y1(I),X1(I),XCEN(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 1215   CONTINUE
      ENDIF
C
      CALL DPTRE2(Y1,NS,X1,NGROUP,XCEN,NCENS,
     1            ICAPSW,ICAPTY,IFORSW,
     1            XTEMP1,XTEMP2,
     1            TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,
     1            TEND,MAXNXT,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TREN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTREN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTRE2(Y,N,X1,NGROUP,XCEN,NCENS,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  XTEMP1,XTEMP2,
     1                  XIDTEM,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,
     1                  TEND,MAXNXT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT A TRENDS ANALYSIS
C              FOR THE DATA IN THE INPUT VECTOR Y.
C     INPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED) REPAIR/CENSORING TIMES.
C                    --X1     = THE OPTIONAL SINGLE PRECISION VECTOR
C                               GROUP-ID VALUES
C                    --XCEN   = THE OPTIONAL SINGLE PRECISION VECTOR
C                               OF CENSOR VALUES (1 = REPAIR
C                               TIME, 0 = CENSOR TIME).
C                      NY     = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR Y.
C                      NX     = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X1.
C                      NC     = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR XCEN.
C     REFERENCE--TOBIAS AND TRINDADE (1995), "APPLIED
C                RELIABILITY", SECOND EDITION, CHAPMAN AND HALL,
C                PP. 314.
C     NOTE--3 TRENDS TESTS ARE PERFORMED:
C           1) REVERSE ARRANGEMENT TEST
C           2) MILITARY HANDBOOK TEST
C           3) LAPLACE TEST
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI 77 FORTRAN.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/5
C     ORIGINAL VERSION--MAY       1998.
C     UPDATED         --OCTOBER   2006. SUPPORT FOR MULTIPLE SYSTEMS
C     UPDATED         --OCTOBER   2006. SUPPORT FOR HTML/LATEX/RFT
C                                       OUTPUT
C     UPDATED         --OCTOBER   2006. CHANGE OUTPUT FORMAT FOR
C                                       REVERSE ARRANGEMENT TEST
C                                       AND CORRECTED BUG IN THIS
C                                       TEST
C     UPDATED         --OCTOBER   2006. CODE FOR SINGLE TEST
C                                       EXTRACTED TO DPTRE3
C     UPDATED         --FEBRUARY  2011. USE DPDTA1 AND DPDTA5 TO PRINT
C                                       TABLES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
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
C
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DVAL2
      DOUBLE PRECISION DVAL3
C
      REAL MHTPVA
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DSUM
C
      DIMENSION Y(*)
      DIMENSION X1(*)
      DIMENSION XCEN(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XIDTEM(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION TEMP6(*)
C
      PARAMETER (NUMALP=3)
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=25)
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
      ISUBN1='DPTR'
      ISUBN2='E2  '
C
      IERROR='NO'
C
      MAXSYS=10000
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRE2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTRE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N,IBUGA3,ISUBRO
   52   FORMAT('N,IBUGA3,ISUBRO = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO56I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I),X1(I),XCEN(I)
   57     FORMAT('I,Y(I),X1(I),XCEN(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN RELIABILITY TREND TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1        'VARIABLE IS LESS THAN 4.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
  115   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO135I=2,N
      IF(Y(I).NE.HOLD)GOTO139
  135 CONTINUE
  130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  139 CONTINUE
C
C               ********************************************
C               **  STEP 11--                             **
C               **  GENERATE THE RELIABILITY TREND TESTS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CASE 1: NO GROUP OR CENSORING VARIABLE
C
      IF(NGROUP.EQ.0 .AND. NCENS.EQ.0)THEN
        ISET=1
        CALL DPTRE3(Y,N,XTEMP1,XTEMP2,TEND,MAXNXT,
     1              RATPVA,MHTPVA,DSUM1,DVAL2,DVAL3,
     1              ISET,ICAPSW,ICAPTY,IFORSW,
     1              ISUBRO,IBUGA3,IERROR)
        NUMSET=1
C
C       CASE 2: GROUP VARIABLE, BUT NO CENSORING VARIABLE
C
      ELSEIF(NCENS.EQ.0)THEN
C
C       STEP 1: DETERMINE UNIQUE GROUPS
C
        NUMSET=0
        DO1051I=1,N
          IF(NUMSET.EQ.0)GOTO1053
          DO1052J=1,NUMSET
            IF(X1(I).EQ.XIDTEM(J))GOTO1051
 1052     CONTINUE
 1053     CONTINUE
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=X1(I)
 1051   CONTINUE
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
C       STEP 2: GENERATE TRACES FOR EACH GROUP
C
        J=0
        DO1090ISET=1,NUMSET
C
          K=0
          DO1091I=1,N
            IF(X1(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP2(K)=Y(I)
            ENDIF
1091      CONTINUE
          NI=K
          CALL DPTRE3(TEMP2,NI,XTEMP1,XTEMP2,TEND,MAXNXT,
     1                RATPVA,MHTPVA,DSUM1,DVAL2,DVAL3,
     1                ISET,ICAPSW,ICAPTY,IFORSW,
     1                ISUBRO,IBUGA3,IERROR)
          TEMP6(ISET)=RATPVA
          TEMP6(MAXSYS+ISET)=MHTPVA
          TEMP6(2*MAXSYS+ISET)=REAL(DSUM1)
          TEMP6(3*MAXSYS+ISET)=REAL(DVAL2)
          TEMP6(4*MAXSYS+ISET)=REAL(DVAL3)
1090    CONTINUE
C
C       CASE 3: BOTH GROUP VARIABLE AND CENSORING VARIABLE
C
      ELSE
C
C       STEP 1: DETERMINE UNIQUE GROUPS
C
        NUMSET=0
        DO1111I=1,N
          IF(NUMSET.EQ.0)GOTO1113
          DO1112J=1,NUMSET
            IF(X1(I).EQ.XIDTEM(J))GOTO1111
 1112     CONTINUE
 1113     CONTINUE
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=X1(I)
 1111   CONTINUE
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
C       STEP 2A: EXTRACT RESPONSE AND CENSORING DATA FOR EACH
C                GROUP
C
        J=0
        ISETMX=NUMSET
        DO1120ISET=1,NUMSET
C
          K=0
          DO1121I=1,N
            IF(X1(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP2(K)=Y(I)
              TEMP3(K)=XCEN(I)
            ENDIF
1121      CONTINUE
          NI=K
C
C       STEP 2B: PROCESS THE CENSORING VARIABLE.  THERE CAN
C                BE AT MOST ONE CENSORING POINT FOR EACH
C                GROUP.
C
          CALL SORTC(TEMP2,TEMP3,NI,TEMP4,TEMP5)
          DO1160I=1,NI
            TEMP2(I)=TEMP4(I)
            TEMP3(I)=TEMP5(I)
 1160     CONTINUE
          AREP=TEMP3(1)
          ACEN=TEMP2(NI)
          IF(NI.LE.1)THEN
            NTEMPR=1
            NTEMPC=0
          ELSE
            IF(AREP.EQ.ACEN)THEN
              NTEMPR=NI
              NTEMPC=0
              DO1170I=1,NI
                IF(TEMP3(I).NE.AREP)THEN
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,111)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1171)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1172)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1173)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1174)XIDTEM(ISET)
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GOTO1120
                ENDIF
 1170         CONTINUE
            ELSE
              TEND=TEMP2(NI)
              NTEMPR=NI-1
              NTEMPC=1
              DO1180I=1,NTEMPR
                IF(TEMP3(I).NE.AREP)THEN
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,111)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1171)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1172)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1173)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1174)XIDTEM(ISET)
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GOTO1120
                ENDIF
 1180         CONTINUE
            ENDIF
          ENDIF
 1171 FORMAT('      FOR EACH SYSTEM, THERE SHOULD BE AT MOST')
 1172 FORMAT('      ONE CENSORING TIME AND IT MUST BE THE MAXIMUM')
 1173 FORMAT('      VALUE FOR THAT SYSTEM.')
 1174 FORMAT('      SUCH WAS NOT THE CASE FOR SYSTEM ',G15.7)
C
C       STEP 2C: COMPUTE THE TREND TEST FOR A SINGLE SYSTEM
C
          TEND=ACEN
          CALL DPTRE3(TEMP2,NTEMPR,XTEMP1,XTEMP2,TEND,MAXNXT,
     1                RATPVA,MHTPVA,DSUM1,DVAL2,DVAL3,
     1                ISET,ICAPSW,ICAPTY,IFORSW,
     1                ISUBRO,IBUGA3,IERROR)
          TEMP6(ISET)=RATPVA
          TEMP6(MAXSYS+ISET)=MHTPVA
          TEMP6(2*MAXSYS+ISET)=REAL(DSUM1)
          TEMP6(3*MAXSYS+ISET)=REAL(DVAL2)
          TEMP6(4*MAXSYS+ISET)=REAL(DVAL3)
C
1120    CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  PERFORM COMPOSITE TESTS               **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMSET.LE.1)GOTO9000
C
C     COMPOSITE TESTS
C
C     PRINT SUMMARY STATISTICS TABLE
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
      IDF=2
      ISUM=0
      SUM1=0.0
      SUM2=0.0
      DO2010I=1,NUMSET
        PVAL=TEMP6(I)
        ATERM1=-2.0*LOG(PVAL)
        SUM1=SUM1 + PVAL
        SUM2=SUM2 + ATERM1
        ISUM=ISUM+IDF
 2010 CONTINUE
C
      ALP90=0.90
      CALL CHSPPF(ALP90,ISUM,CV1)
      ALP95=0.95
      CALL CHSPPF(ALP95,ISUM,CV2)
      ALP99=0.99
      CALL CHSPPF(ALP99,ISUM,CV3)
C
      ITITLE='Reverse Arrangements Test: Fisher Composite Test'
      NCTITL=48
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Systems:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=REAL(NUMSET)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sum of -2*LN(p-value):'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=SUM2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Degrees of Freedom:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=REAL(ISUM)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: There is a Trend for Interarrival Times'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO2020I=1,NUMROW
        NTOT(I)=15
 2020 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      DO2030J=1,5
        DO2040I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2040   CONTINUE
 2030 CONTINUE
C
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
C
      ITITL2(2,2)='Significance'
      NCTIT2(2,2)=12
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
C
      ITITL2(2,3)='Chi-Square'
      NCTIT2(2,3)=10
      ITITL2(3,3)='Test Statistic'
      NCTIT2(3,3)=14
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
C
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
C
      NMAX=0
      NUMCOL=5
      DO2050I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IINC=1600
        IINC2=1400
        IINC3=2200
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC
C
        DO2060J=1,3
          IVALUE(J,1)='No Trend'
          NCVALU(J,1)=8
          IF(J.EQ.1)THEN
            IVALUE(J,2)='0.90'
            NCVALU(J,2)=4
            AMAT(J,3)=SUM2
            AMAT(J,4)=CV1
            IF(SUM2.GT.CV1)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.95'
            NCVALU(J,2)=4
            AMAT(J,3)=SUM2
            AMAT(J,4)=CV2
            IF(SUM2.GT.CV2)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='0.99'
            NCVALU(J,2)=4
            AMAT(J,3)=SUM2
            AMAT(J,4)=CV3
            IF(SUM2.GT.CV3)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 2060   CONTINUE
C
 2050 CONTINUE
C
      ICNT=3
      NUMLIN=3
      NUMCOL=5
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C     COMPOSITE TEST FOR MILITARY HANDBOOK TEST
C
        IDF=2
        ISUM=0
        SUM1=0.0
        SUM2=0.0
        DO3010I=1,NUMSET
          PVAL=TEMP6(MAXSYS+I)
          ATERM1=-2.0*LOG(PVAL)
          SUM1=SUM1 + PVAL
          SUM2=SUM2 + ATERM1
          ISUM=ISUM+IDF
 3010   CONTINUE
C
      ALP90=0.90
      CALL CHSPPF(ALP90,ISUM,CV1)
      ALP95=0.95
      CALL CHSPPF(ALP95,ISUM,CV2)
      ALP99=0.99
      CALL CHSPPF(ALP99,ISUM,CV3)
C
      ITITLE='Military Handbook Test: Fisher Composite Test'
      NCTITL=45
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Systems:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=REAL(NUMSET)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sum of -2*LN(p-value):'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=SUM2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Degrees of Freedom:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=REAL(ISUM)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: There is a Trend for Interarrival Times'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: There is a Trend Following a NHPP'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Power Law Model'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO3020I=1,NUMROW
        NTOT(I)=15
 3020 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      NMAX=0
      NUMCOL=5
      DO3050I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
C
        DO3060J=1,3
          IF(J.EQ.1)THEN
            AMAT(J,3)=SUM2
            AMAT(J,4)=CV1
            IF(SUM2.GT.CV1)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=SUM2
            AMAT(J,4)=CV2
            IF(SUM2.GT.CV2)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=SUM2
            AMAT(J,4)=CV3
            IF(SUM2.GT.CV3)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 3060   CONTINUE
C
 3050 CONTINUE
C
      ICNT=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C     LAPLACE COMPOSITE TEST
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM3=0.0D0
      DO4010I=1,NUMSET
        VAL1=TEMP6(2*MAXSYS+I)
        VAL2=TEMP6(3*MAXSYS+I)
        VAL3=TEMP6(4*MAXSYS+I)
        DSUM1=DSUM1 + DBLE(VAL1)
        DSUM2=DSUM2 + DBLE(VAL2)
        DSUM3=DSUM3 + DBLE(VAL3)
 4010 CONTINUE
      DSUM2=-0.5D0*DSUM2
      Z=REAL((DSUM1 + DSUM2)/DSQRT(DSUM3/12.0D0))
      CALL NORCDF(Z,CDF)
      ALP01=0.01
      CALL NORPPF(ALP01,CV1)
      ALP05=0.05
      CALL NORPPF(ALP05,CV2)
      ALP10=0.10
      CALL NORPPF(ALP10,CV3)
      ALP90=0.90
      CALL NORPPF(ALP90,CV4)
      ALP95=0.95
      CALL NORPPF(ALP95,CV5)
      ALP99=0.99
      CALL NORPPF(ALP99,CV6)
C
      ITITLE='Laplace Test: Composite Test'
      NCTITL=28
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Normal Test Statistic Value:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=Z
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Normal Test Statistic CDF Value:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=CDF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: There is a Trend Following a NHPP'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Exponential Law Model'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO2310I=1,NUMROW
        NTOT(I)=15
 2310 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(2,3)='Normal'
      NCTIT2(2,3)=6
      ITITL2(3,3)='Test Statistic'
      NCTIT2(3,3)=14
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
C
      NMAX=0
      DO4050I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
C
        DO4060J=1,3
          IF(J.EQ.1)THEN
            IVALUE(J,2)='0.01'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV1
            IF(Z.LE.CV1)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.05'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV2
            IF(Z.LE.CV2)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='0.10'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV3
            IF(Z.LE.CV3)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 4060   CONTINUE
C
 4050 CONTINUE
C
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (<=)'
      NCTIT2(3,4)=11
C
      DO4150I=1,NUMCOL
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
C
        DO4160J=1,3
          IF(J.EQ.1)THEN
            IVALUE(J,2)='0.90'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV4
            IF(Z.GE.CV4)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.95'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV5
            IF(Z.GE.CV5)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='0.99'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV6
            IF(Z.GE.CV6)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 4160   CONTINUE
C
 4150 CONTINUE
C
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTRE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9016I=1,N
          WRITE(ICOUT,9017)I,Y(I)
 9017     FORMAT('I,Y(I),W(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTRE3(Y,N,XTEMP1,XTEMP2,TEND,MAXNXT,
     1                  RATPVA,MHTPVA,DSUM1,DVAL2,DVAL3,
     1                  ISET,ICAPSW,ICAPTY,IFORSW,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT A TRENDS ANALYSIS
C              FOR THE DATA IN THE INPUT VECTOR Y.
C     NOTE--DPTRE2 CAN LOOP THROUGH MULTIPLE SYSTEMS.
C           THIS ROUTINE IS USED TO COMPUTE THE TESTS FOR
C           A SINGLE SYSTEM.
C     NOTE--3 TRENDS TESTS ARE PERFORMED:
C           1) REVERSE ARRANGEMENT TEST
C           2) MILITARY HANDBOOK TEST
C           3) LAPLACE TEST
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                OF FAILURE TIMES
C                       N      = THE INTEGER NUMBER OF
C                                OBSERVATIONS IN THE VECTOR Y.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI 77 FORTRAN.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/10
C     ORIGINAL VERSION--OCTOBER   2006. EXTRACTED FROM DPTRE3
C     UPDATED         --FEBRUARY  2011. USE DPDTA1 AND DPDTA5 TO
C                                       PRINT TABLES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DVAL2
      DOUBLE PRECISION DVAL3
C
      REAL MHTPVA
C
      DIMENSION Y(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
C
      PARAMETER (NUMALP=3)
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=25)
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
      ISUBN1='DPTR'
      ISUBN2='E3  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRE3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTRE3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N,IBUGA3
   52   FORMAT('N,IBUGA3 = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)ISET
  111   FORMAT('***** ERROR IN RELIABILITY TREND TEST--SYSTEM ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATONS IS LESS THAN 4.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)N
  112   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO135I=2,N
        IF(Y(I).NE.HOLD)GOTO139
  135 CONTINUE
  130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)ISET
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
  139 CONTINUE
C
C               ********************************************
C               **  STEP 11--                             **
C               **  REVERSE ARRANGEMENTS TEST             **
C               ********************************************
C
C               ********************************************
C               **  STEP 11A-                             **
C               **  CREATE INTERARRIVAL TIME ARRAY        **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='NO'
      CALL INTARR(Y,N,IWRITE,XTEMP1,NX,IBUGA3,IERROR)
  100 CONTINUE
C
C               ********************************************
C               **  STEP 11B-                             **
C               **  CALCULATE NUMBER OF REVERSALS         **
C               ********************************************
      IREV=0
      DO140J=1,N-1
        DO149K=J+1,N
          IF(XTEMP1(K).GT.XTEMP1(J))IREV=IREV+1
  149   CONTINUE
  140 CONTINUE
      IRMAX=N*(N-1)/2
      AN=REAL(N)
      REXP=AN*(AN-1.0)/4.0
      RVAR=(2.0*AN + 5.0)*(AN - 1.0)*AN/72.0
      RSD=SQRT(RVAR)
C
      R=REAL(IREV)
      ANUM=R + 0.5 - REXP
      Z=ANUM/RSD
      CALL NORCDF(Z,CDF)
      RATPVA=CDF
C
C               *************************
C               **  STEP 11C-          **
C               **  FORM Z STATISTICS  **
C               *************************
C
      ISTEPN='11C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ALP01=0.01
      CALL NORPPF(ALP01,PPF01)
      ALP05=0.05
      CALL NORPPF(ALP05,PPF05)
      ALP10=0.10
      CALL NORPPF(ALP10,PPF10)
      ALP90=0.90
      CALL NORPPF(ALP90,PPF90)
      ALP95=0.95
      CALL NORPPF(ALP95,PPF95)
      ALP99=0.99
      CALL NORPPF(ALP99,PPF99)
      IF(N.EQ.4)THEN
        IRMN01=-1
        IRMN05=0
        IRMN10=0
        IRMN90=6
        IRMN95=6
        IRMN99=-1
      ELSEIF(N.EQ.5)THEN
        IRMN01=0
        IRMN05=1
        IRMN10=1
        IRMN90=9
        IRMN95=9
        IRMN99=10
      ELSEIF(N.EQ.6)THEN
        IRMN01=1
        IRMN05=2
        IRMN10=3
        IRMN90=12
        IRMN95=13
        IRMN99=14
      ELSEIF(N.EQ.7)THEN
        IRMN01=2
        IRMN05=4
        IRMN10=5
        IRMN90=16
        IRMN95=17
        IRMN99=19
      ELSEIF(N.EQ.8)THEN
        IRMN01=4
        IRMN05=6
        IRMN10=8
        IRMN90=20
        IRMN95=22
        IRMN99=24
      ELSEIF(N.EQ.9)THEN
        IRMN01=6
        IRMN05=9
        IRMN10=11
        IRMN90=25
        IRMN95=27
        IRMN99=30
      ELSEIF(N.EQ.10)THEN
        IRMN01=9
        IRMN05=12
        IRMN10=14
        IRMN90=31
        IRMN95=33
        IRMN99=36
      ELSEIF(N.EQ.11)THEN
        IRMN01=12
        IRMN05=16
        IRMN10=18
        IRMN90=37
        IRMN95=39
        IRMN99=43
      ELSEIF(N.EQ.12)THEN
        IRMN01=16
        IRMN05=20
        IRMN10=23
        IRMN90=43
        IRMN95=46
        IRMN99=50
      ELSEIF(N.GT.12)THEN
        IRMN01=PPF01*RSD + REXP - 0.5
        IRMN05=PPF05*RSD + REXP - 0.5
        IRMN10=PPF10*RSD + REXP - 0.5
        IRMN90=PPF90*RSD + REXP - 0.5
        IRMN95=PPF95*RSD + REXP - 0.5
        IRMN99=PPF99*RSD + REXP - 0.5
      ENDIF
C
C               ****************************
C               **  STEP 11D-             **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='11D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     PRINT SUMMARY STATISTICS TABLE
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
      ITITLE='Reverse Arrangements Test: (System      )'
      NCTITL=41
      WRITE(ITITLE(36:40),'(I5)')ISET
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Failure Times:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Observed Number of Reversals:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(IREV)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Maximum Possible Number of Reversals:'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=REAL(IRMAX)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Expected Number of Reversals:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REXP
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Variance(Expected Number of Reversals):'
      NCTEXT(ICNT)=39
      AVALUE(ICNT)=RVAR
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Value of Test Statistic (Z-Score):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=Z
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Z-Score CDF Value:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=CDF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Improvement Test'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Increasing Trend for Interarrival Times'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO2310I=1,NUMROW
        NTOT(I)=15
 2310 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      DO2320J=1,5
        DO2325I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2325   CONTINUE
 2320 CONTINUE
C
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
C
      ITITL2(2,2)='Significance'
      NCTIT2(2,2)=12
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
C
      ITITL2(2,3)='Number of'
      NCTIT2(2,3)=9
      ITITL2(3,3)='Reversals'
      NCTIT2(3,3)=9
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
C
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
C
      NMAX=0
      NUMCOL=5
      DO5210I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IF(I.EQ.3 .OR. I.EQ.4)THEN
          IDIGIT(I)=0
        ENDIF
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IINC=1600
        IINC2=1400
        IINC3=2200
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC
C
        DO5289J=1,3
          IVALUE(J,1)='No Trend'
          NCVALU(J,1)=8
          IF(J.EQ.1)THEN
            IVALUE(J,2)='0.90'
            NCVALU(J,2)=4
            AMAT(J,3)=REAL(IREV)
            AMAT(J,4)=REAL(IRMN90)
            IF(IREV.GE.IRMN90)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.95'
            NCVALU(J,2)=4
            AMAT(J,3)=REAL(IREV)
            AMAT(J,4)=REAL(IRMN95)
            IF(IREV.GE.IRMN95)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='0.99'
            NCVALU(J,2)=4
            AMAT(J,3)=REAL(IREV)
            AMAT(J,4)=REAL(IRMN99)
            IF(IREV.GE.IRMN99)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 5289   CONTINUE
C
 5210 CONTINUE
C
      ICNT=3
      NUMLIN=3
      NUMCOL=5
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Degradation Test'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Declining Trend for Interarrival Times'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO6210I=1,NUMROW
        NTOT(I)=15
 6210 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (<=)'
      NCTIT2(3,4)=11
C
      DO6310I=1,NUMCOL
C
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IF(I.EQ.3 .OR. I.EQ.4)THEN
          IDIGIT(I)=0
        ENDIF
C
        DO6389J=1,3
          IF(J.EQ.3)THEN
            IVALUE(J,2)='0.01'
            NCVALU(J,2)=4
            AMAT(J,4)=REAL(IRMN01)
            IF(IREV.LE.IRMN01)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.05'
            NCVALU(J,2)=4
            AMAT(J,4)=REAL(IRMN05)
            IF(IREV.LE.IRMN05)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.1)THEN
            IVALUE(J,2)='0.10'
            NCVALU(J,2)=4
            AMAT(J,4)=REAL(IRMN10)
            IF(IREV.LE.IRMN10)THEN
              IVALUE(J,5)(1:6)='REJECT'
            ELSE
              IVALUE(J,5)(1:6)='ACCEPT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 6389   CONTINUE
C
 6310 CONTINUE
C
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C               ********************************************
C               **  STEP 21--                             **
C               **  MILITARY HANDBOOK    TEST             **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ********************************************
C               **  STEP 21B-                             **
C               **  CALCULATE TEST STATISTIC              **
C               ********************************************
C
      DSUM=0.0D0
      DO310I=1,N
        IF(Y(I).GE.TEND)THEN
          WRITE(ICOUT,311)
  311     FORMAT('***** ERROR FROM MILITARY HANDBOOK TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,312)ISET
  312     FORMAT('      FOR SYSTEM ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,313)TEND
  313     FORMAT('      THE SPECIFIED CENSORING TIME,',G15.7,',')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,314)
  314     FORMAT('      IS LESS THAN AT LEAST ONE FAILURE TIME.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,316)I,Y(I)
  316     FORMAT('      FAILURE TIME ',I8,' = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(Y(I).LE.0.0)THEN
          WRITE(ICOUT,311)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,317)I
  317     FORMAT('      FAILURE ',I8,' IS NON-POSITIVE. ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,318)Y(I)
  318     FORMAT('      IT HAS THE VALUE ',G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        DSUM=DSUM + DLOG(DBLE(TEND/Y(I)))
  310 CONTINUE
      Z=REAL(2.0D0*DSUM)
      INU=2*N
      CALL CHSCDF(Z,INU,CDF)
      MHTPVA=CDF
C
      ALP01=0.01
      CALL CHSPPF(ALP01,INU,CV1)
      ALP05=0.05
      CALL CHSPPF(ALP05,INU,CV2)
      ALP10=0.10
      CALL CHSPPF(ALP10,INU,CV3)
      ALP90=0.90
      CALL CHSPPF(ALP90,INU,CV4)
      ALP95=0.95
      CALL CHSPPF(ALP95,INU,CV5)
      ALP99=0.99
      CALL CHSPPF(ALP99,INU,CV6)
C
C               ****************************
C               **  STEP 21B-             **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE='Military Handbook Test: (System      )'
      NCTITL=38
      WRITE(ITITLE(33:37),'(I5)')ISET
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Failure Times:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Chi-Square Test Statistic Value:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=Z
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Chi-Square Test Statistic CDF Value:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=CDF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Improvement Test'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: There is a Trend Following a NHPP'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Power Law Model'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO7310I=1,NUMROW
        NTOT(I)=15
 7310 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(2,3)='Chi-Square'
      NCTIT2(2,3)=10
      ITITL2(3,3)='Test Statistic'
      NCTIT2(3,3)=14
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
C
      DO5310I=1,NUMCOL
C
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IF(I.EQ.3 .OR. I.EQ.4)THEN
          IDIGIT(I)=NUMDIG
        ENDIF
C
        DO5389J=1,3
          IF(J.EQ.1)THEN
            IVALUE(J,2)='0.90'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV4
            IF(0.000.LE.CDF.AND.CDF.LE.0.9)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.95'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV5
            IF(0.000.LE.CDF.AND.CDF.LE.0.95)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='0.99'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV6
            IF(0.000.LE.CDF.AND.CDF.LE.0.99)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 5389   CONTINUE
C
 5310 CONTINUE
C
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Degradation Test'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: There is a Trend Following a NHPP'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Power Law Model'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO7390I=1,NUMROW
        NTOT(I)=15
 7390 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (<=)'
      NCTIT2(3,4)=11
C
      DO7410I=1,NUMCOL
C
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IF(I.EQ.3 .OR. I.EQ.4)THEN
          IDIGIT(I)=NUMDIG
        ENDIF
C
        DO7489J=1,3
          IF(J.EQ.3)THEN
            IVALUE(J,2)='0.01'
            NCVALU(J,2)=4
            AMAT(J,4)=CV1
            IF(CDF.GE.0.01)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.05'
            NCVALU(J,2)=4
            AMAT(J,4)=CV2
            IF(CDF.GE.0.05)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.1)THEN
            IVALUE(J,2)='0.10'
            NCVALU(J,2)=4
            AMAT(J,4)=CV3
            IF(CDF.GE.0.10)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 7489   CONTINUE
C
 7410 CONTINUE
C
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C               ********************************************
C               **  STEP 31--                             **
C               **  LAPLACE              TEST             **
C               ********************************************
C
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ********************************************
C               **  STEP 31B-                             **
C               **  CALCULATE TEST STATISTIC              **
C               ********************************************
C
       DSUM=0.0D0
       DSUM1=0.0D0
       DO510I=1,N
         IF(Y(I).GE.TEND)THEN
           WRITE(ICOUT,511)TEND
  511      FORMAT('***** ERROR FROM LAPLACE TREND TEST--')
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,512)ISET
  512      FORMAT('      FOR SYSTEM ',I8)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,513)TEND
  513      FORMAT('      THE SPECIFIED CENSORING TIME, ',G15.7)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,514)
  514      FORMAT('      IS LESS THAN AT LEAST ONE FAILURE TIME.')
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,516)I,Y(I)
  516      FORMAT('      FAILURE TIME ',I8,' = ',G15.7)
           CALL DPWRST('XXX','BUG ')
           IERROR='YES'
           GOTO9000
         ENDIF
         IF(Y(I).LE.0.0)THEN
           WRITE(ICOUT,511)TEND
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,512)ISET
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,521)I
  521      FORMAT('      FAILURE ',I8,' IS NOT POSITIVE.')
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,523)Y(I)
  523      FORMAT('      IT HAS THE VALUE ',G15.7)
           CALL DPWRST('XXX','BUG ')
           IERROR='YES'
           GOTO9000
         ENDIF
         DSUM=DSUM + DBLE(Y(I)-TEND/2.0)
         DSUM1=DSUM1 + DBLE(Y(I))
  510 CONTINUE
      DVAL2=DBLE(N)*DBLE(TEND)
      DVAL3=DBLE(N)*DBLE(TEND)**2
C
      AN=REAL(N)
      Z=REAL(DBLE(SQRT(12.0*AN))*DSUM/DBLE(AN*TEND))
      CALL NORCDF(Z,CDF)
C
      ALP01=0.01
      CALL NORPPF(ALP01,CV1)
      ALP05=0.05
      CALL NORPPF(ALP05,CV2)
      ALP10=0.10
      CALL NORPPF(ALP10,CV3)
      ALP90=0.90
      CALL NORPPF(ALP90,CV4)
      ALP95=0.95
      CALL NORPPF(ALP95,CV5)
      ALP99=0.99
      CALL NORPPF(ALP99,CV6)
C
C               ****************************
C               **  STEP 31B-             **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='31B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE='Laplace Test: (System      )'
      NCTITL=28
      WRITE(ITITLE(23:27),'(I5)')ISET
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Failure Times:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Normal Test Statistic Value:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=Z
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Normal Test Statistic CDF Value:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=CDF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Improvement Test'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: There is a Trend Following a NHPP'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Exponential Law Model'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO8210I=1,NUMROW
        NTOT(I)=15
 8210 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(2,3)='Normal'
      NCTIT2(2,3)=6
      ITITL2(3,3)='Test Statistic'
      NCTIT2(3,3)=14
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
C
      DO8310I=1,NUMCOL
C
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IF(I.EQ.3 .OR. I.EQ.4)THEN
          IDIGIT(I)=NUMDIG
        ENDIF
C
        DO8389J=1,3
          IF(J.EQ.1)THEN
            IVALUE(J,2)='0.90'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV4
            IF(0.000.LE.CDF.AND.CDF.LE.0.9)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.95'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV5
            IF(0.000.LE.CDF.AND.CDF.LE.0.95)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='0.99'
            NCVALU(J,2)=4
            AMAT(J,3)=Z
            AMAT(J,4)=CV6
            IF(0.000.LE.CDF.AND.CDF.LE.0.99)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 8389   CONTINUE
C
 8310 CONTINUE
C
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Degradation Test'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: No Trend for Interarrival Times'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: There is a Trend Following a NHPP'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Exponential Law Model'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO8390I=1,NUMROW
        NTOT(I)=15
 8390 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (<=)'
      NCTIT2(3,4)=11
C
      DO8410I=1,NUMCOL
C
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IF(I.EQ.3 .OR. I.EQ.4)THEN
          IDIGIT(I)=NUMDIG
        ENDIF
C
        DO8489J=1,3
          IF(J.EQ.3)THEN
            IVALUE(J,2)='0.01'
            NCVALU(J,2)=4
            AMAT(J,4)=CV1
            IF(CDF.GE.0.01)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='0.05'
            NCVALU(J,2)=4
            AMAT(J,4)=CV2
            IF(CDF.GE.0.05)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ELSEIF(J.EQ.1)THEN
            IVALUE(J,2)='0.10'
            NCVALU(J,2)=4
            AMAT(J,4)=CV3
            IF(CDF.GE.0.10)THEN
              IVALUE(J,5)(1:6)='ACCEPT'
            ELSE
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,5)=6
          ENDIF
 8489   CONTINUE
C
 8410 CONTINUE
C
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRE3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTRE3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9016I=1,N
          WRITE(ICOUT,9017)I,Y(I),XTEMP1(I)
 9017     FORMAT('I,Y(I),XTEMP1(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTRI2(X1,Y1,X2,Y2,X3,Y3,
     1IFIG,
     1ILINPA,ILINCO,PLINTH,
     1AREGBA,
     1IREBLI,IREBCO,PREBTH,
     1IREFSW,IREFCO,
     1IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
C     PURPOSE--DRAW A TRIANGLE
C              WITH FRONT FACE VERTICES AT (X1,Y1),
C              (X2,Y2), AND (X3,Y3).
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
C     UPDATED         --JANUARY   1989.  MODIFY CALL  TO DPFIRE (ALAN)
C
C-----NON-COMMON VARIABLES-------------------------------------
C
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT2
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
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
C
      DIMENSION PX(10)
      DIMENSION PY(10)
CCCCC DIMENSION PX3(10)
CCCCC DIMENSION PY3(10)
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
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRI2')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTRI2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)X1,Y1
   53 FORMAT('X1,Y1 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)X2,Y2
   54 FORMAT('X2,Y2 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IFIG
   59 FORMAT('IFIG = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)ILINPA(1),ILINCO(1),PLINTH(1)
   61 FORMAT('ILINPA(1),ILINCO(1),PLINTH(1) = ',A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)AREGBA(1)
   62 FORMAT('AREGBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)IREBLI(1),IREBCO(1),PREBTH(1)
   63 FORMAT('IREBLI(1),IREBCO(1),PREBTH(1) = ',A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64 FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65 FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',
     1A4,2X,A4,2X,A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,69)PTEXHE,PTEXWI
   69 FORMAT('PTEXHE,PTEXWI= ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,70)PTEXVG,PTEXHG
   70 FORMAT('PTEXVG,PTEXHG= ',2E15.6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *********************************
C               **  STEP 1--                   **
C               **  DETERMINE THE COORDINATES  **
C               **  FOR THE TRIANGLE           **
C               *********************************
C
      PX(1)=X1
      PY(1)=Y1
C
      PX(2)=X2
      PY(2)=Y2
C
      PX(3)=X3
      PY(3)=Y3
C
      PX(4)=X1
      PY(4)=Y1
C
      NP=4
C
C
C               ***********************
C               **  STEP 2--         **
C               **  FILL THE FIGURE  **
C               **  (IF CALLED FOR)  **
C               ***********************
C
      IF(IREFSW(1).EQ.'OFF')GOTO2190
      IPATT=IREPTY(1)
      IPATT2='SOLI'
      PTHICK=PREPTH(1)
      PXGAP=PREPSP(1)
      PYGAP=PREPSP(1)
      ICOLF=IREFCO(1)
      ICOLP=IREPCO(1)
      CALL DPFIRE(PX,PY,NP,
     1IFIG,IPATT,PTHICK,PXGAP,PYGAP,ICOLF,ICOLP,IPATT2)
 2190 CONTINUE
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
CCCCC CALL DPDRPL(PX,PY,NP,PX3,PY3,NP3,
CCCCC1IFIG,IPATT,PTHICK,ICOL)
      CALL DPDRPL(PX,PY,NP,
     1IFIG,IPATT,PTHICK,ICOL,
     1JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRI2')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTRI2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NP
 9013 FORMAT('NP = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NP
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
      SUBROUTINE DPTRIA(IHARG,IARGT,ARG,NUMARG,
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
C     PURPOSE--DRAW ONE OR MORE TRIANGLES
C              (DEPENDING ON HOW MANY NUMBERS ARE PROVIDED).
C              THE COORDINATES ARE IN STANDARDIZED UNITS
C              OF 0 TO 100.
C     NOTE--THE INPUT COORDINATES DEFINE THE VERTICES
C           OF THE TRIANGLE.
C     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 3
C          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*3 = 6.
C     NOTE--IF 4 NUMBERS ARE PROVIDED,
C           THEN THE DRAWN TRIANGLE WILL GO
C           FROM THE LAST CURSOR POSITION
C           (ASSUMED TO BE AT VERTEX 1)
C           THROUGH THE (X,Y) POINT
C           (EITHER ABSOLUTE OR RELATIVE)
C           AS DEFINED BY THE FIRST AND SECOND NUMBERS
C           (ASSUMED TO BE AT VERTEX 2)
C           TO THE (X,Y) POINT
C           (EITHER ABSOLUTE OR RELATIVE)
C           AS DEFINED BY THE THIRD AND FOURTH NUMBERS
C           (ASSUMED TO BE AT VERTEX 3)
C           AND CONTINUING BACK THE START POINT TO CLOSE THE TRIANGLE.
C     NOTE--IF 6 NUMBERS ARE PROVIDED,
C           THEN THE DRAWN TRIANGLE WILL GO
C           FROM THE ABSOLUTE (X,Y) POSITION
C           AS RESULTING FORM THE FIRST AND SECOND NUMBERS
C           (ASSUMED TO BE AT VERTEX 1)
C           THROUGH THE (X,Y) POINT
C           (EITHER ABSOLUTE OR RELATIVE)
C           AS DEFINED BY THE THIRD AND FOURTH NUMBERS
C           (ASSUMED TO BE AT VERTEX 2)
C           TO THE (X,Y) POINT
C           (EITHER ABSOLUTE OR RELATIVE)
C           AS DEFINED BY THE FIFTH AND SIXTH NUMBERS
C           (ASSUMED TO BE AT VERTEX 3)
C           AND THEN CONTINUING BACK THE START POINT TO CLOSE THE TRIANGLE.
C     NOTE--AND SO FORTH FOR 10, 14, 18, ... NUMBERS.
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --NOVEMBER  1982.
C     UPDATED         --JANUARY   1989. CALL LIST FOR OFFSET VAR (ALAN)
C     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
C     UPDATED         --JULY      1997. SUPPORT FOR "DATA" UNITS (ALAN)
C     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
C                                       COMMAND
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
      CHARACTER*4 IDCOLO
CCCCC ADD FOLLOWING LINE MARCH 1997.
      CHARACTER*4 IDFONT
CCCCC ADD FOLLOWING LINE JULY 1997.
      CHARACTER*4 UNITSW
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
CCCCC ADD FOLLOWING LINE MARCH 1997.
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
C
      DIMENSION IDNVOF(*)
      DIMENSION IDNHOF(*)
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
      IFOUND='NO'
      IERROR='NO'
      IERRG4=IERROR
CCCCC IBUGG4=IBUGD2
CCCCC ISUBG4=ISUBRO
C
      ILOCFN=0
      NUMNUM=0
C
      X1=0.0
      Y1=0.0
      X2=0.0
      Y2=0.0
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRIA')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTRIA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NUMARG
   53 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I),IARGT(I),ARG(I)
   56 FORMAT('I,IHARG(I),IARGT(I),ARG(I) = ',I8,2X,A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,57)PXSTAR,PYSTAR
   57 FORMAT('PXSTAR,PYSTAR = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,58)PXEND,PYEND
   58 FORMAT('PXEND,PYEND = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)ILINPA(1),ILINCO(1),PLINTH(1)
   61 FORMAT('ILINPA(1),ILINCO(1),PLINTH(1) = ',A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)AREGBA(1)
   62 FORMAT('AREGBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)IREBLI(1),IREBCO(1),PREBTH(1)
   63 FORMAT('IREBLI(1),IREBCO(1),PREBTH(1) = ',A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64 FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65 FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',
     1A4,2X,A4,2X,A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,69)PTEXHE,PTEXWI
   69 FORMAT('PTEXHE,PTEXWI= ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,70)PTEXVG,PTEXHG
   70 FORMAT('PTEXVG,PTEXHG= ',2E15.6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,76)IGRASW,IDIASW
   76 FORMAT('IGRASW,IDIASW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,77)PGRAXF,PGRAYF,PDIAXC,PDIAYC
   77 FORMAT('PGRAXF,PGRAYF,PDIAXC,PDIAYC = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,78)PDIAHE,PDIAWI,PDIAVG,PDIAHG
   78 FORMAT('PDIAHE,PDIAWI,PDIAVG,PDIAHG = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,80)NUMDEV
   80 FORMAT('NUMDEV= ',I8)
      CALL DPWRST('XXX','BUG ')
      DO81I=1,NUMDEV
      WRITE(ICOUT,82)IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   82 FORMAT('IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',
     1A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,83)IDPOWE(I),IDCONT(I),IDCOLO(I)
   83 FORMAT('IDPOWE(I),IDCONT(I),IDCOLO(I) = ',
     1A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,84)IDNVPP(I),IDNHPP(I),IDUNIT(I)
   84 FORMAT('IDNVPP(I),IDNHPP(I),IDUNIT(I) = ',
     1I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
   81 CONTINUE
      WRITE(ICOUT,87)IFOUND
   87 FORMAT('IFOUND= ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,88)IBUGG4,ISUBG4,IERRG4
   88 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,89)IBUGD2,IERROR
   89 FORMAT('IBUGD2,IERROR= ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      IFIG='TRIA'
      NUMPT=3
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
      CALL DPOPPL(IGRASW,
     1IBELSW,NUMRIN,IERASW,
     1IBACCO)
C
C               *****************************************
C               **  STEP 2--                           **
C               **  SEARCH FOR COMMAND SPECIFICATIONS  **
C               *****************************************
C
      IF(NUMARG.GE.2.AND.
     1IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')
     1GOTO1111
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ABSO'.AND.
     1IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')
     1GOTO1112
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'RELA'.AND.
     1IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')
     1GOTO1113
      GOTO1130
C
 1111 CONTINUE
      ITYPEO='ABSO'
      ILOCFN=1
      GOTO1119
C
 1112 CONTINUE
      ITYPEO='ABSO'
      ILOCFN=2
      GOTO1119
C
 1113 CONTINUE
      ITYPEO='RELA'
      ILOCFN=2
      GOTO1119
 1119 CONTINUE
C
      IF(ILOCFN.GT.NUMARG)GOTO1129
      DO1120I=ILOCFN,NUMARG
      IF(IARGT(I).EQ.'NUMB')GOTO1120
      GOTO1129
 1120 CONTINUE
      IFOUND='YES'
      GOTO1149
 1129 CONTINUE
      GOTO1130
C
 1130 CONTINUE
      IERRG4='YES'
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN DPTRIA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR DRAW ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW A TRIANGLE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      WITH VERTICES (20,20), (50,20), (35,40)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      TRIANGLE 20 20 50 20 35 40')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      TRIANGLE ABSOLUTE 20 20 50 20 35 40')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1149 CONTINUE
C
C               ****************************
C               **  STEP 3--              **
C               **  DRAW OUT THE LINE(S)  **
C               ****************************
C
      NUMNUM=NUMARG-ILOCFN+1
      IF(NUMNUM.LT.NUMPT2)GOTO1151
      GOTO1152
C
 1151 CONTINUE
      J=ILOCFN-1
      X1=PXSTAR
      Y1=PYSTAR
      GOTO1159
C
 1152 CONTINUE
      J=ILOCFN
      IF(J.GT.NUMARG)GOTO1190
      X1=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X1,X1,IBUGD2,ISUBRO,IERROR)
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      Y1=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y1,Y1,IBUGD2,ISUBRO,IERROR)
      GOTO1159
 1159 CONTINUE
C
 1160 CONTINUE
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      X2=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X2,X2,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')X2=X1+X2
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      Y2=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y2,Y2,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
C
 1170 CONTINUE
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      X3=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X3,X3,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')X3=X2+X3
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      Y3=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y3,Y3,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')Y3=Y2+Y3
C
      CALL DPTRI2(X1,Y1,X2,Y2,X3,Y3,
     1IFIG,
     1ILINPA,ILINCO,PLINTH,
     1AREGBA,
     1IREBLI,IREBCO,PREBTH,
     1IREFSW,IREFCO,
     1IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
      X1=X3
      Y1=Y3
C
      GOTO1160
 1190 CONTINUE
C
      PXEND=X3
      PYEND=Y3
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
     1PGRAXF,PGRAYF,
     1IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1PDIAHE,PDIAWI,PDIAVG,PDIAHG)
C
      CALL DPCLDE
C
 8000 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRIA')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTRIA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ILOCFN,NUMNUM
 9012 FORMAT('ILOCFN,NUMNUM = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)X1,Y1,X2,Y2,X3,Y3
 9013 FORMAT('X1,Y1,X2,Y2,X3,Y3 = ',6E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)PXSTAR,PYSTAR
 9015 FORMAT('PXSTAR,PYSTAR = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)PXEND,PYEND
 9016 FORMAT('PXEND,PYEND = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IFIG
 9017 FORMAT('IFIG = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9027)IFOUND
 9027 FORMAT('IFOUND = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9028)IBUGG4,ISUBG4,IERRG4
 9028 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9029)IBUGD2,IERROR
 9029 FORMAT('IBUGD2,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTRIP(IHARG,NUMARG,IDEFPR,IHMXPR,
     1IPREC,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE PRECISION SWITCH
C              AS TRIPLE PRECISION.
C              THIS IN TURN SPECIFIES THAT SUBSEQUENT
C              CALCULATIONS WILL ALL BE CARRIED OUT
C              IN TRIPLE PRECISION.
C              THE SPECIFIED PRECISION SWITCH SPECIFICATION
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IPREC.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFPR (A  HOLLERITH VARIABLE)
C                     --IHMXPR (A  HOLLERITH VARIABLE)
C     OUTPUT ARGUMENTS--IPREC  (A HOLLERITH VARIABLE)
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
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFPR
      CHARACTER*4 IHMXPR
      CHARACTER*4 IPREC
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
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
      IFOUND='YES'
C
 1110 CONTINUE
      IF(NUMARG.LE.0)GOTO1120
      IF(IHARG(NUMARG).EQ.'ON')GOTO1130
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1130
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1120
      GOTO1130
C
 1120 CONTINUE
      IHOLD=IDEFPR
      GOTO1160
C
 1130 CONTINUE
      IHOLD='TRIP'
      GOTO1160
C
 1160 CONTINUE
      IF(IHOLD.EQ.'DOUB'.AND.IHMXPR.EQ.'SING')GOTO1170
      IF(IHOLD.EQ.'TRIP'.AND.IHMXPR.EQ.'SING')GOTO1170
      IF(IHOLD.EQ.'TRIP'.AND.IHMXPR.EQ.'DOUB')GOTO1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'SING')GOTO1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'DOUB')GOTO1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'TRIP')GOTO1170
      GOTO1180
C
 1170 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1172)
 1172 FORMAT('***** ERROR IN DPTRIP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1173)
 1173 FORMAT('      THE DESIRED PRECISION IS HIGHER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1174)
 1174 FORMAT('      THAN PERMITTED ON THIS COMPUTER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1175)IHOLD
 1175 FORMAT('      DESIRED PRECISION           = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1176)IHMXPR
 1176 FORMAT('      MAXIMUM ALLOWABLE PRECISION = ',A4)
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1180 CONTINUE
      IPREC=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IPREC
 1188 FORMAT('THE PRECISION SWITCH HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPTRPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A TRILINEAR PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/12
C     ORIGINAL VERSION--DECEMBER  2006.
C     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR3
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IREPL
C
      CHARACTER*4 ICASE
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
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
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION GROUP(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),Y3(1))
      EQUIVALENCE (GARBAG(IGARB4),GROUP(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP4(1))
C
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
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPTR'
      ISUBN2='PL  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***************************
C               **  TREAT THE PLOT CASE  **
C               ***************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'TRPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTRPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXNPP
   53   FORMAT('ICASPL,IAND1,IAND2,MAXNPP = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGG2,IBUGG3,ISUBRO,IBUGQ
   54   FORMAT('IBUGG2,IBUGG3,ISUBRO,IBUGQ = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IFOUND,IERROR
   55   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 1--                             **
C               **  SEARCH FOR TRILINEAR PLOT            **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TRPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASPL='TRPL'
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
        IHARG(NUMARG+1)='    '
        IHARG2(NUMARG+1)='    '
      ELSE
        ICASPL='    '
        IFOUND='NO'
        GOTO9000
      ENDIF
C
C               *********************************
C               **  STEP 4--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TRPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='TRILINEAR PLOT'
      MINNA=3
      MAXNA=100
      MINN2=1
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=3
      MAXNVA=4
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TRPL')THEN
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
     1                      ICOLR(I),PVAR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I),PVAR(I) = ',I8,2X,A4,A4,2X,3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y2,Y3,GROUP,GROUP,GROUP,GROUP,NS,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
C
C               *****************************************************
C               **  STEP 41--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY) FOR    **
C               **  THE PLOT.                                      **
C               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .    **
C               **  DEFINE THE NUMBER OF PLOT POINTS   (NPLOTP).   **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES(NPLOTV).   **
C               *****************************************************
C
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TRPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPTRP2(Y1,Y2,Y3,GROUP,NS,
     1            ICASPL,IREPL,MAXN,
     1            TEMP1,TEMP2,TEMP3,TEMP4,
     1            Y,X,X3D,D,NPLOTP,NPLOTV,
     1            IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'TRPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPTRPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IBUGG2,IBUGG3
 9014   FORMAT('IBUGG2,IBUGG3 = ', A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,Y(.),X(.),D(.),ISUB(.)--')
        CALL DPWRST('XXX','BUG ')
        DO9021I=1,NPLOTP
          WRITE(ICOUT,9022)I,Y(I),X(I),D(I),ISUB(I)
 9022     FORMAT(I8,E15.7,E15.7,E15.7,I8)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTRP2(Y1,Y2,Y3,GROUP,NS,
     1            ICASPL,IREPL,MAXN,
     1            TEMP1,TEMP2,TEMP3,TEMP4,
     1            Y,X,X3D,D,NPLOTP,NPLOTV,
     1            IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--FORM A TRILINEAR PLOT.
C     REFERENCE--WAINER (1997), "VISUAL REVELATIONS",
C                COPERNICUS, PP. 111-118.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/12
C     ORIGINAL VERSION--DECEMBER  2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IREPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION GROUP(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION X3D(*)
      DIMENSION D(*)
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
      ISUBN1='DPTR'
      ISUBN2='PL  '
      IERROR='NO'
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TRP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTRPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPLOTV,NPLOTP,NS,MAXN
   52   FORMAT('NPLOTV,NPLOTP,NS,MAXN = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IREPL,IBUGG3,IERROR
   53   FORMAT('ICASPL,IREPL,IBUGG3,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,MIN(NS,100)
          WRITE(ICOUT,56)I,Y1(I),Y2(I),Y3(I),GROUP(I)
   56     FORMAT('I,Y1(I),Y2(I),Y3(I),GROUP(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TRP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               **  1) THE SUM OF Y1, Y2, AND Y3 MUST BE  **
C               **     EITHER 1 OR 100 (FOR PERCENTAGE    **
C               **     UNITS).                            **
C               **  2) EACH OF THE COMPONENTS MUST BE IN  **
C               **     THE INTERVAL (0,1) OR (0,100).     **
C               ********************************************
C
      N=NS
      ACASE=1.0
C
      DO120I=1,N
        ASUM=Y1(I)+Y2(I)+Y3(I)
        IF(I.EQ.1)THEN
          IF(ABS(ASUM - 1.0).LE.0.001)THEN
            ACASE=1.0
            EPS=0.001
          ELSEIF(ABS(ASUM - 100.0).LE.0.1)THEN
            ACASE=100.0
            EPS=0.1
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,121)
  121       FORMAT('***** ERROR IN TRILINEAR PLOT--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,123)I
  123       FORMAT('      FOR ROW ',I8,', THE COMPONENTS DO NOT ',
     1             'SUM TO EITHER 1 OR 100')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,124)ASUM
  124       FORMAT('      SUM              = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,125)Y1(I)
  125       FORMAT('      COMPONENT 1      = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,126)Y2(I)
  126       FORMAT('      COMPONENT 2      = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,127)Y3(I)
  127       FORMAT('      COMPONENT 3      = ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
        ELSE
          IF(ABS(ASUM - ACASE).GT.EPS)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,121)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,133)I,ACASE
  133       FORMAT('      FOR ROW ',I8,', THE COMPONENTS DO NOT ',
     1             'SUM TO ',F7.1)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,124)ASUM
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,125)Y1(I)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,126)Y2(I)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,127)Y3(I)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
        ENDIF
  120 CONTINUE
C
C               ****************************************************
C               **  STEP 2--                                      **
C               **  COMPUTE COORDINATES FOR TRILINEAR PLOT        **
C               ****************************************************
C
      IF(IREPL.EQ.'ON')THEN
        CALL DISTIN(GROUP,N,IWRITE,TEMP1,NDIST,IBUGG3,IERROR)
        DO1010I=1,N
          Y(I)=Y1(I)
          X(I)=Y2(I)
          X3D(I)=Y3(I)
          D(I)=1.0
          DO1020J=1,NDIST
            IF(GROUP(I).EQ.TEMP1(J))THEN
              D(I)=REAL(J)
              GOTO1029
            ENDIF
 1020     CONTINUE
 1029     CONTINUE
 1010   CONTINUE
        NPLOTP=N
        NPLOTV=3
      ELSE
        DO2010I=1,N
          Y(I)=Y1(I)
          X(I)=Y2(I)
          X3D(I)=Y3(I)
          D(I)=1.0
 2010   CONTINUE
        NPLOTP=N
        NPLOTV=3
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TRP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPTRPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL = ',
     1         I8,I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,Y(.),X(.),X3D(.),D(.)--')
        CALL DPWRST('XXX','BUG ')
        DO9021I=1,NPLOTP
          WRITE(ICOUT,9022)I,Y(I),X(I),X3D(I),D(I)
 9022     FORMAT(I8,4F15.7)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTRPO(X,Y,N,
     1                  TX,TY,SX,SY,THETA,
     1                  X2,Y2,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--GIVEN A SET OF (X,Y) PAIRS, PERFORM A TRANSLATION,
C              SCALING, AND ROTATION TRANSFORMATION.
C
C              THE TRANSLATION CAN BE IMPLEMENTED AS:
C
C                  X'=X - Tx
C                  Y'=Y - Ty
C
C              THE SCALING CAN BE IMPLENENTED AS:
C
C                 X'=X*Sx
C                 Y'=Y*Sy
C
C              THE ROTATION CAN BE IMPLEMENTED AS:
C
C                 X'=COS(THETA)*X + SIN(THETA)*Y
C                 Y'=-SIN(THETA)*X + COS(THETA)*Y
C
C     INPUT  ARGUMENTS--X      = A REAL VECTOR CONTAINING THE X
C                                COORDINATES OF THE POINTS
C                     --Y      = A REAL VECTOR CONTAINING THE Y
C                                COORDINATES OF THE POINTS
C                     --N      = NUMBER OF POINTS IN X, Y
C                     --TX     = TRANSLATION IN X DIRECTION
C                     --TY     = TRANSLATION IN Y DIRECTION
C                     --SX     = SCALING IN X DIRECTION
C                     --SY     = SCALING IN Y DIRECTION
C                     --THETA  = ANGLE OF ROTATION (IN COUNTER CLOCKWISE
C                                DIRECTION) IN RADIANS
C     OUTPUT ARGUMENTS--X2     = A REAL VECTOR CONTAINING THE X
C                                COORDINATES OF THE TRANSFORMED POINTS
C                     --Y      = A REAL VECTOR CONTAINING THE Y
C                                COORDINATES OF THE TRANSFORMED POINTS
C     REFERENCE--XXXXX
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
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012.10
C     ORIGINAL VERSION--OCTOBER   2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      REAL X(*)
      REAL Y(*)
      REAL X2(*)
      REAL Y2(*)
C
      DOUBLE PRECISION PI
      DOUBLE PRECISION DX
      DOUBLE PRECISION DY
      DOUBLE PRECISION DXP
      DOUBLE PRECISION DYP
      DOUBLE PRECISION DTHETA
C
      INTEGER N
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA PI / 3.1415926535 8979323846 2643383279 503 D0 /
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRPO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTRPO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)TX,TY,SX,SY,THETA
   54   FORMAT('TX,TY,SX,SY,THETA = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO65I=1,N
            WRITE(ICOUT,66)I,X(I),Y(I)
   66       FORMAT('I,X(I),Y(I) = ',I8,2X,2G15.7)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
      ENDIF
C
      IF(SX.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN  TRANSFORM POINTS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)SX
  103   FORMAT('      THE SCALING FACTOR ',G15.7,' FOR THE X ',
     1         'DIRECTION IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(SY.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,108)SY
  108   FORMAT('      THE SCALING FACTOR ',G15.7,' FOR THE Y ',
     1         'DIRECTION IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DTHETA=DBLE(THETA)
      IF((DTHETA.LT.-PI) .OR. (DTHETA.GT.PI))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
  113   FORMAT('      THE ROTATION FACTOR ',G15.7,
     1         'IS OUTSIDE THE (-PI,PI) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO100IROW=1,N
        DX=DBLE(X(IROW))
        DY=DBLE(Y(IROW))
        DXP= DCOS(DTHETA)*DX + DSIN(DTHETA)*DY
        DYP=-DSIN(DTHETA)*DX + DCOS(DTHETA)*DY
        DXP=DXP - DBLE(TX)
        DYP=DYP - DBLE(TY)
        DXP=DXP*DBLE(SX)
        DYP=DYP*DBLE(SY)
        X2(IROW)=REAL(DXP)
        Y2(IROW)=REAL(DYP)
  100 CONTINUE
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRPO')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DPTRPO--')
        CALL DPWRST('XXX','BUG ')
        DO9055I=1,N
          WRITE(ICOUT,9056)I,X2(I),Y2(I)
 9056     FORMAT('I,X2(I),Y2(I) = ',I8,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9055   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTTES(XTEMP1,XTEMP2,MAXNXT,
     1                  ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A 1-SAMPLE OR A 2-SAMPLE T TEST
C     EXAMPLE--T TEST Y MU
C              T TEST MU Y
C              T TEST Y1 Y2
C              T TEST Y1 Y2 Y3 Y4 MU
C              T TEST Y1 Y2 Y3 Y4 Y5
C              PAIRED T TEST Y1 Y2
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JULY      1984.
C     UPDATED         --FEBRUARY  1994.  ADD COMMENTS ABOVE
C     UPDATED         --DECEMBER  1994.  COPY T TEST PARAMETERS
C     UPDATED         --MAY       1995.  BUG FIX (DECLARATIONS)
C     UPDATED         --MARCH     2011.  USE DPPARS AND DPPAR3
C     UPDATED         --MARCH     2011.  SUPPORT FOR PAIRED T-TEST
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
      CHARACTER*4 ICASA3
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 IPAIR
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
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
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
      ISUBN1='DPTT'
      ISUBN2='ES  '
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
C               ********************************
C               **  TREAT THE T TEST CASE  **
C               ********************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TTES')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTTES--')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTC=9999
      ILASTZ=9999
      ICASAN='TTES'
      ICASA2='UNKN'
      ICASA3='UNKN'
      IPAIR='OFF'
      IREPL='OFF'
      IMULT='OFF'
C
C     LOOK FOR:
C
C          T TEST/TTEST
C          MULTIPLE
C          REPLICATED
C          PAIRED
C          ONE SAMPLE (OR 1 SAMPLE)
C          TWO SAMPLE (OR 2 SAMPLE)
C
      DO100I=0,NUMARG-1
C
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
        ELSE
          ICTMP1=IHARG(I)
        ENDIF
        ICTMP2=IHARG(I+1)
        ICTMP3=IHARG(I+2)
C
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'T   ' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='TTES'
          ILASTC=I
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'TTES')THEN
          IFOUND='YES'
          ICASAN='TTES'
          ILASTC=I
          ILASTZ=I
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'MULT')THEN
          IMULT='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'PAIR')THEN
          IPAIR='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'ONE' .AND. ICTMP2.EQ.'SAMP')THEN
          ICASA2='ONES'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'1' .AND. ICTMP2.EQ.'SAMP')THEN
          ICASA2='ONES'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'TWO' .AND. ICTMP2.EQ.'SAMP')THEN
          ICASA2='TWOS'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'2' .AND. ICTMP2.EQ.'SAMP')THEN
          ICASA2='TWOS'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'LOWE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA3='LOWE'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'UPPE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA3='UPPE'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'TWO' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA3='TWOT'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'2' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA3='TWOT'
          ILASTC=MIN(ILASTC,I)
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')THEN
        WRITE(ICOUT,91)ICASAN,ICASA2,IMULT,IREPL,ISHIFT
   91   FORMAT('DPTTES: ICASAN,ICASA2,IMULT,IREPL,ISHIFT = ',
     1         4(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFOUND.EQ.'NO')GOTO9000
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN T-TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      "REPLICATION" FOR THE T-TEST COMMAND. ')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='T-TEST'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
      IFLAGP=29
      IF(IREPL.EQ.'ON')THEN
        IFLAGE=1
        IFLAGM=0
      ENDIF
      IF(IPAIR.EQ.'ON')THEN
        IFLAGE=1
        ICASA2='TWOS'
      ENDIF
      IF(ICASA2.EQ.'TWOS')THEN
        IFLAGP=0
      ENDIF
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')THEN
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
C     IF EITHER FIRST OR LAST ARGUMENT IS A PARAMETER, THEN
C     WE HAVE THE ONE-SAMPLE T-TEST.  OTHERWISE, HAVE ASSUME
C     A TWO-SAMPLE T-TEST.
C
      IF(ICASA2.EQ.'ONES')THEN
        IF(IVARTY(1).NE.'PARA' .AND. IVARTY(NUMVAR).NE.'PARA')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,292)
  292     FORMAT('      FOR THE ONE-SAMPLE TEST, EITHER THE FIRST OR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,294)
  294     FORMAT('      THE LAST ARGUMENT MUST BE A PARAMETER.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSEIF(IVARTY(1).EQ.'PARA')THEN
        ICASA2='ONES'
        ISTART=2
        ISTOP=NUMVAR
        AMU0=PVAR(1)
      ELSEIF(IVARTY(NUMVAR).EQ.'PARA')THEN
        ICASA2='ONES'
        ISTART=1
        ISTOP=NUMVAR-1
        AMU0=PVAR(NUMVAR)
      ELSE
        ICASA2='TWOS'
        ISTART=1
        ISTOP=NUMVAR
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVA2=1
      DO5210I=ISTART,ISTOP
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
        IF(ICASA2.EQ.'ONES')THEN
          ISTRT2=1
          ISTOP2=1
        ELSE
          ISTRT2=I+1
          ISTOP2=ISTOP
        ENDIF
C
        DO5220J=ISTRT2,ISTOP2
C
          IF(ICASA2.EQ.'TWOS')THEN
            ICOL=J
            CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                  INAME,IVARN1,IVARN2,IVARTY,
     1                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                  MAXCP4,MAXCP5,MAXCP6,
     1                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                  X,X,X,NS2,NLOCA2,NLOCA3,ICASE,
     1                  IBUGA3,ISUBRO,IFOUND,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
          ENDIF
C
C               *****************************************
C               **  STEP 52--                          **
C               **  PERFORM 2-SAMPLE T-TEST            **
C               *****************************************
C
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TTES')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DPTTES, BEFORE CALL DPTTE2--')
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
          CALL DPTTE2(Y,NS1,X,NS2,AMU0,ICASA2,ICASA3,IPAIR,
     1                XTEMP1,XTEMP2,MAXNXT,
     1                ICAPSW,ICAPTY,IFORSW,ITTEVA,
     1                IVARID,IVARI2,IVARI3,IVARI4,
     1                STATVA,STATCD,STATNU,POOLSD,
     1                STATV2,STATC2,STATN2,
     1                PVAL2T,PVALLT,PVALUT,
     1                CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,
     1                CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,
     1                IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(ICASA2.EQ.'TWOS')THEN
            IF(NUMVAR.GT.2)THEN
              IFLAGU='FILE'
            ELSE
              IFLAGU='ON'
            ENDIF
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
            IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
            IF(IPAIR.EQ.'OFF')THEN
              IF(ITTEVA.EQ.'EQUA')THEN
                STATV2=STATVA
                STATC2=STATCD
                STATN2=STATNU
              ENDIF
            ENDIF
          ELSE
            IF(ISTOP-ISTART.GT.0)THEN
              IFLAGU='FILE'
            ELSE
              IFLAGU='ON'
            ENDIF
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(I.EQ.ISTART)IFRST=.TRUE.
            IF(I.EQ.ISTOP)ILAST=.TRUE.
          ENDIF
          CALL DPTTE5(ICASA2,STATVA,STATCD,STATNU,
     1                STATV2,STATC2,STATN2,
     1                PVAL2T,PVALLT,PVALUT,
     1                CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,
     1                CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TTES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTTES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTTE2(Y1,N1,Y2,N2,AMU0,ICASA2,ICASA3,IPAIR,
     1                  XTEMP1,XTEMP2,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,ITTEVA,
     1                  IVARID,IVARI2,IVARI3,IVARI4,
     1                  STATVA,STATCD,STATNU,POOLSD,
     1                  STATV2,STATC2,STATN2,
     1                  PVAL2T,PVALLT,PVALUT,
     1                  CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,
     1                  CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT A T TEST
C              (1-SAMPLE OR UNPAIRED 2-SAMPLE)
C     EXAMPLE--T TEST Y MU
C              T TEST MU Y
C              T TEST Y1 Y2
C     SAMPLE 1 IS IN INPUT VECTOR Y1
C              (WITH N1 OBSERVATIONS).
C     SAMPLE 2 IS IN INPUT VECTOR Y2
C              (WITH N2 OBSERVATIONS).
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
C     ORIGINAL VERSION--MAY       1984.
C     UPDATED         --APRIL     1987.  (LARRY KNAB CORRECTION--
C                                        BROWNLEE, P. 225)
C     UPDATED         --FEBRUARY  1994.  REFORMAT OUTPUT
C     UPDATED         --FEBRUARY  1994.  DPWRST: 'BUG ' => 'WRIT'
C     UPDATED         --DECEMBER  1994.  COPY T TEST PARAMETERS
C     UPDATED         --OCTOBER   2006.  CALL LIST TO TCDF/TPPF
C     UPDATED         --NOVEMBER  2007.  ALLOW USER-SPECIFIED
C                                        SIGNIFICANCE LEVEL
C     UPDATED         --APRIL     2011.  USE DPDTA1, DPDTA5 TO PRINT
C                                        OUTPUT.  REFORMAT OUTPUT
C                                        SOMEWHAT AS WELL.
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
      CHARACTER*4 ITTEVA
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
      CHARACTER*4 IPAIR
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
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
C
      PARAMETER (NUMALP=6)
      REAL ALPHA(NUMALP)
C
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=40)
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
      DATA ALPHA/0.50, 0.80, 0.90, 0.95, 0.99, 0.999/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTT'
      ISUBN2='E2  '
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTE2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTTE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASA2,ITTEVA
   52   FORMAT('IBUGA3,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N1,N2,NUMDIG,AMU
   55   FORMAT('N1,N2,NUMDIG,AMU = ',3I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        IF(N1.GE.1)THEN
          DO56I=1,N1
            WRITE(ICOUT,57)I,Y1(I)
   57       FORMAT('I,Y1(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
        IF(N2.GE.1 .AND. ICASA2.EQ.'TWOS')THEN
          DO66I=1,N2
            WRITE(ICOUT,67)I,Y2(I)
   67       FORMAT('I,Y2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   66     CONTINUE
        ENDIF
      ENDIF
C
C               ************************************
C               **   STEP 1--                     **
C               **   BRANCH DEPENDING ON WHETHER  **
C               **   1-SAMPLE T TEST OR           **
C               **   2-SAMPLE T TEST.             **
C               ************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASA2.EQ.'ONES')THEN
        GOTO2100
      ELSEIF(ICASA2.EQ.'TWOS')THEN
        IF(IPAIR.EQ.'OFF')GOTO3100
        IF(IPAIR.EQ.'ON')GOTO4100
      ELSE
        GOTO9000
      ENDIF
C
C               ******************************
C               **  STEP 21--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR A 1-SAMPLE T TEST   **
C               ******************************
C
 2100 CONTINUE
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPTTE3(Y1,N1,AMU0,IWRITE,STATVA,STATCD,STATNU,
     1            YMEAN,YSD,YSDM,DEL,
     1            PVAL2T,PVALLT,PVALUT,
     1            ISUBRO,IBUGA3,IERROR)
C
      CALL TPPF(.0005,STATNU,CTL999)
      CALL TPPF(.005,STATNU,CUTL99)
      CALL TPPF(.025,STATNU,CUTL95)
      CALL TPPF(.05,STATNU,CUTL90)
      CALL TPPF(.1,STATNU,CUTL80)
      CALL TPPF(.25,STATNU,CUTL50)
      CALL TPPF(.75,STATNU,CUTU50)
      CALL TPPF(.90,STATNU,CUTU80)
      CALL TPPF(.95,STATNU,CUTU90)
      CALL TPPF(.975,STATNU,CUTU95)
      CALL TPPF(.995,STATNU,CUTU99)
      CALL TPPF(.9995,STATNU,CTU999)
C
C               ******************************
C               **   STEP 22--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR A 1-SAMPLE T TEST  **
C               ******************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      ITITLE='One Sample t-Test for the Mean'
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
      WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=27
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
      ITEXT(ICNT)='H0: Mean Equal'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=AMU0
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Mean Not Equal'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=AMU0
      IDIGIT(ICNT)=NUMDIG
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
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N1)
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
      ITEXT(ICNT)='Sample Standard Deviation of the Mean:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=YSDM
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Mean - Mu0:'
      NCTEXT(ICNT)=11
      AVALUE(ICNT)=DEL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='t-Test Statistic Value:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Degrees of Freedom:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=INT(STATNU+0.1)
      IDIGIT(ICNT)=0
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE='Two-Tailed Test'
      NCTITL=15
      ITITL9='H0: u = m0; Ha: u <> m0'
      NCTIT9=23
C
      DO2130J=1,4
        DO2140I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2140   CONTINUE
 2130 CONTINUE
C
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
      NUMCOL=4
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
      DO2160J=1,NUMALP
C
        AMAT(J,2)=STATVA
        IF(J.EQ.1)THEN
          AMAT(J,3)=CUTU50
        ELSEIF(J.EQ.2)THEN
          AMAT(J,3)=CUTU80
        ELSEIF(J.EQ.3)THEN
          AMAT(J,3)=CUTU90
        ELSEIF(J.EQ.4)THEN
          AMAT(J,3)=CUTU95
        ELSEIF(J.EQ.5)THEN
          AMAT(J,3)=CUTU99
        ELSEIF(J.EQ.6)THEN
          AMAT(J,3)=CTU999
        ENDIF
        IVALUE(J,4)(1:6)='REJECT'
        IF(ABS(STATVA).LT.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
C
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 2160 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      NUMCOL=4
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      IF(ICASA3.NE.'LOWE' .AND. ICASA3.NE.'UPPE')THEN
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
      IF(ICASA3.EQ.'TWOT')GOTO9000
C
      ITITLE='Lower One-Tailed Test'
      NCTITL=21
      ITITL9='H0: u = m0; Ha: u < m0'
      NCTIT9=22
C
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
C
      NMAX=0
      NUMCOL=4
      DO2250I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 2250 CONTINUE
C
      DO2260J=1,NUMALP
        ALPHAT=1.0 - ALPHA(J)
        CALL TPPF(ALPHAT,STATNU,ATEMP)
        AMAT(J,3)=ATEMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.GE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 2260 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      IF(ICASA3.NE.'UPPE')THEN
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
      IF(ICASA3.EQ.'LOWE')GOTO9000
C
      ITITLE='Upper One-Tailed Test'
      NCTITL=21
      ITITL9='H0: u = m0; Ha: u > m0'
      NCTIT9=22
C
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (>)'
      NCTIT2(3,3)=9
C
      NMAX=0
      NUMCOL=4
      DO2350I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 2350 CONTINUE
C
      DO2360J=1,NUMALP
        ALPHAT=ALPHA(J)
        CALL TPPF(ALPHAT,STATNU,ATEMP)
        AMAT(J,3)=ATEMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.LE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 2360 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
      GOTO9000
C
C               ****************************************
C               **  STEP 31--                         **
C               **  CARRY OUT CALCULATIONS            **
C               **  FOR AN UNPAIRED 2-SAMPLE T TEST   **
C               ****************************************
C
 3100 CONTINUE
C
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPTTE4(Y1,N1,Y2,N2,IWRITE,
     1            STATVA,STATCD,STATNU,
     1            STATV2,STATC2,STATN2,
     1            Y1MEAN,Y1SD,Y1SDM,
     1            Y2MEAN,Y2SD,Y2SDM,
     1            DEL,POOLSD,DELSD,DELSD2,CDFBAR,
     1            PVAL2T,PVALLT,PVALUT,
     1            ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************
C               **   STEP 32--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR A 2-SAMPLE T TEST  **
C               ******************************
C
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      ITITLE='Two Sample t-Test for Equal Means'
      NCTITL=34
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
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: Population Means Are Equal (u1=u2)'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Population Means Are Not Equal'
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
      ITEXT(ICNT)='Sample One Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y1MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y1SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation of the Mean:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=Y1SDM
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Two Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N2)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y2MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y2SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation of the Mean:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=Y2SDM
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(ITTEVA.EQ.'EQUA' .OR. ITTEVA.EQ.'BOTH')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Test When Assume Equal Variances:'
        NCTEXT(ICNT)=33
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Pooled Standard Deviation:'
        NCTEXT(ICNT)=26
        AVALUE(ICNT)=POOLSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Difference (Delta) in Means:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=DEL
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Standard Deviation of Delta:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=DELSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='t-Test Statistic Value:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=STATVA
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Degrees of Freedom:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=STATNU
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='CDF Value:'
        NCTEXT(ICNT)=10
        AVALUE(ICNT)=STATCD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='P-Value (2-tailed test):'
        NCTEXT(ICNT)=24
        IF(STATVA.LE.0.0)THEN
          ATEMP=2.0*STATCD
        ELSE
          ATEMP=2.0*(1.0-STATCD)
        ENDIF
        AVALUE(ICNT)=ATEMP
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='P-Value (lower-tailed test):'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=STATCD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='P-Value (upper-tailed test):'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=1.0 - STATCD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
C
      IF(ITTEVA.EQ.'UNEQ' .OR. ITTEVA.EQ.'BOTH')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Test When Assume Unequal Variances:'
        NCTEXT(ICNT)=35
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Bartlett CDF Value:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=CDFBAR
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Difference (Delta) in Means:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=DEL
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Standard Deviation of Delta:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=DELSD2
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='t-Test Statistic Value:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=STATV2
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Degrees of Freedom:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=STATN2
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='CDF Value:'
        NCTEXT(ICNT)=10
        AVALUE(ICNT)=STATC2
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
      ENDIF
C
      NUMROW=ICNT
      DO3110I=1,NUMROW
        NTOT(I)=15
 3110 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='31A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='31B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO3199ICASE=1,2
C
        IF(ICASE.EQ.1 .AND. ITTEVA.EQ.'UNEQ')GOTO3199
        IF(ICASE.EQ.2 .AND. ITTEVA.EQ.'EQUA')GOTO3199
C
        IF(ICASE.EQ.1)THEN
          ITITLE='Two-Tailed Test (Assume Equal Variances)'
          NCTITL=40
          STATV=STATVA
          STATC=STATCD
          STATN=STATNU
          PVALL=STATCD
          PVALU=1.0 - STATCD
          IF(STATVA.LE.0.0)THEN
            PVAL2=2.0*STATCD
          ELSE
            PVAL2=2.0*(1.0 - STATCD)
          ENDIF
        ELSEIF(ICASE.EQ.2)THEN
          ITITLE='Two-Tailed Test (Assume Unequal Variances)'
          NCTITL=42
          STATV=STATV2
          STATC=STATC2
          STATN=STATN2
          PVAL2=PVAL2T
          PVALL=PVALLT
          PVALU=PVALUT
        ENDIF
C
        CALL TPPF(.0005,STATN,CTL999)
        CALL TPPF(.005,STATN,CUTL99)
        CALL TPPF(.025,STATN,CUTL95)
        CALL TPPF(.05,STATN,CUTL90)
        CALL TPPF(.1,STATN,CUTL80)
        CALL TPPF(.25,STATN,CUTL50)
        CALL TPPF(.75,STATN,CUTU50)
        CALL TPPF(.90,STATN,CUTU80)
        CALL TPPF(.95,STATN,CUTU90)
        CALL TPPF(.975,STATN,CUTU95)
        CALL TPPF(.995,STATN,CUTU99)
        CALL TPPF(.9995,STATN,CTU999)
C
        ITITL9='H0: u1 = u2; Ha: u1 <> u2'
        NCTIT9=25
C
        DO3130J=1,4
          DO3140I=1,3
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
 3140     CONTINUE
 3130   CONTINUE
C
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
        NUMCOL=4
        DO3150I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          IF(I.EQ.1 .OR. I.EQ.4)THEN
            ITYPCO(I)='ALPH'
          ENDIF
 3150   CONTINUE
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
        DO3160J=1,NUMALP
          AMAT(J,2)=STATV
          IF(J.EQ.1)THEN
            AMAT(J,3)=CUTU50
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CUTU80
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CUTU90
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CUTU95
          ELSEIF(J.EQ.5)THEN
            AMAT(J,3)=CUTU99
          ELSEIF(J.EQ.6)THEN
            AMAT(J,3)=CTU999
          ENDIF
          IVALUE(J,4)(1:6)='REJECT'
          IF(ABS(STATV).LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
C
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 3160   CONTINUE
C
        ICNT=NUMALP
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        IF(ICASA3.NE.'LOWE' .AND. ICASA3.NE.'UPPE')THEN
          CALL DPDTA5(ITITLE,NCTITL,
     1                ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                IFLAGS,IFLAGE,
     1                ISUBRO,IBUGA3,IERROR)
        ENDIF
        IF(ICASA3.EQ.'TWOT')GOTO3199
C
        IF(ICASE.EQ.1)THEN
          ITITLE='Lower One-Tailed Test (Assume Equal Variances)'
          NCTITL=46
        ELSEIF(ICASE.EQ.2)THEN
          ITITLE='Lower One-Tailed Test (Assume Unequal Variances)'
          NCTITL=48
        ENDIF
C
        ITITL9='H0: u1 = u2; Ha: u1 < u2'
        NCTIT9=24
C
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (<)'
        NCTIT2(3,3)=9
C
        NMAX=0
        NUMCOL=4
        DO3250I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 3250   CONTINUE
C
        DO3260J=1,NUMALP
          ALPHAT=ALPHA(J)
          CALL TPPF(ALPHAT,STATN,ATEMP)
          AMAT(J,3)=-ATEMP
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATV.GE.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
 3260   CONTINUE
C
        ICNT=NUMALP
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        IF(ICASA3.NE.'UPPE')THEN
          CALL DPDTA5(ITITLE,NCTITL,
     1                ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                IFLAGS,IFLAGE,
     1                ISUBRO,IBUGA3,IERROR)
        ENDIF
C
        IF(ICASA3.EQ.'LOWE')GOTO3199
C
        IF(ICASE.EQ.1)THEN
          ITITLE='Upper One-Tailed Test (Assume Equal Variances)'
          NCTITL=46
        ELSEIF(ICASE.EQ.2)THEN
          ITITLE='Upper One-Tailed Test (Assume Unequal Variances)'
          NCTITL=48
        ENDIF
C
        ITITL9='H0: u1 = u2; Ha: u1 > u2'
        NCTIT9=24
C
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (>)'
        NCTIT2(3,3)=9
C
        NMAX=0
        NUMCOL=4
        DO3350I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 3350   CONTINUE
C
        DO3360J=1,NUMALP
          ALPHAT=ALPHA(J)
          CALL TPPF(ALPHAT,STATN,ATEMP)
          AMAT(J,3)=ATEMP
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATV.LE.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
 3360   CONTINUE
C
        ICNT=NUMALP
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
C
 3199 CONTINUE
C
      GOTO9000
C
 4100 CONTINUE
C
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPTTE6(Y1,N1,Y2,N2,XTEMP1,IWRITE,
     1            STATVA,STATCD,STATNU,
     1            Y1MEAN,Y1SD,Y1SDM,
     1            Y2MEAN,Y2SD,Y2SDM,
     1            YDMEAN,YDSD,YDSDM,
     1            PVAL2T,PVALLT,PVALUT,
     1            ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      CALL TPPF(.0005,STATNU,CTL999)
      CALL TPPF(.005,STATNU,CUTL99)
      CALL TPPF(.025,STATNU,CUTL95)
      CALL TPPF(.05,STATNU,CUTL90)
      CALL TPPF(.1,STATNU,CUTL80)
      CALL TPPF(.25,STATNU,CUTL50)
      CALL TPPF(.75,STATNU,CUTU50)
      CALL TPPF(.90,STATNU,CUTU80)
      CALL TPPF(.95,STATNU,CUTU90)
      CALL TPPF(.975,STATNU,CUTU95)
      CALL TPPF(.995,STATNU,CUTU99)
      CALL TPPF(.9995,STATNU,CTU999)
C
C               ******************************
C               **   STEP 32--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR A 2-SAMPLE T TEST  **
C               ******************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      ITITLE='Two Sample Paired t-Test for Equal Means'
      NCTITL=41
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
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: Population Means Are Equal (u1=u2)'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Population Means Are Not Equal'
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
      ITEXT(ICNT)='Sample One Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y1MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y1SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Two Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N2)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y2MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y2SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics of Paired Data:'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YDMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YDSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation of the Mean:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=YDSDM
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Difference (Delta) in Means:'
      NCTEXT(ICNT)=28
      DEL=Y1MEAN-Y2MEAN
      AVALUE(ICNT)=DEL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='t-Test Statistic Value:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Degrees of Freedom:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=STATNU
      IDIGIT(ICNT)=0
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
      NUMROW=ICNT
      DO4110I=1,NUMROW
        NTOT(I)=15
 4110 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='31A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='31B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE='Two-Tailed Test'
      NCTITL=15
      ITITL9='H0: u1 = u2; Ha: u1 <> u2'
      NCTIT9=25
C
      DO4130J=1,4
        DO4140I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 4140   CONTINUE
 4130 CONTINUE
C
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
      NUMCOL=4
      DO4150I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.4)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 4150 CONTINUE
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
      DO4160J=1,NUMALP
        AMAT(J,2)=STATVA
        IF(J.EQ.1)THEN
          AMAT(J,3)=CUTU50
        ELSEIF(J.EQ.2)THEN
          AMAT(J,3)=CUTU80
        ELSEIF(J.EQ.3)THEN
          AMAT(J,3)=CUTU90
        ELSEIF(J.EQ.4)THEN
          AMAT(J,3)=CUTU95
        ELSEIF(J.EQ.5)THEN
          AMAT(J,3)=CUTU99
        ELSEIF(J.EQ.6)THEN
          AMAT(J,3)=CTU999
        ENDIF
        IVALUE(J,4)(1:6)='REJECT'
        IF(ABS(STATVA).LT.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
C
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 4160 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      IF(ICASA3.NE.'LOWE' .AND. ICASA3.NE.'UPPE')THEN
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
      IF(ICASA3.EQ.'TWOT')GOTO9000
C
      ITITLE='Lower One-Tailed Test'
      NCTITL=21
      ITITL9='H0: u1 = u2; Ha: u1 < u2'
      NCTIT9=24
C
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
C
      NMAX=0
      NUMCOL=4
      DO4250I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 4250 CONTINUE
C
      DO4260J=1,NUMALP
        ALPHAT=ALPHA(J)
        CALL TPPF(ALPHAT,STATNU,ATEMP)
        AMAT(J,3)=-ATEMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.GE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 4260 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      IF(ICASA3.NE.'UPPE')THEN
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
      IF(ICASA3.EQ.'LOWE')GOTO9000
C
      ITITLE='Upper One-Tailed Test'
      NCTITL=21
      ITITL9='H0: u1 = u2; Ha: u1 > u2'
      NCTIT9=24
C
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (>)'
      NCTIT2(3,3)=9
C
      NMAX=0
      NUMCOL=4
      DO4350I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 4350 CONTINUE
C
      DO4360J=1,NUMALP
        ALPHAT=ALPHA(J)
        CALL TPPF(ALPHAT,STATNU,ATEMP)
        AMAT(J,3)=ATEMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.LE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 4360 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
      GOTO9000
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTTE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)STATVA,STATCD,PVAL2T,PVALLT,PVALUT
 9013   FORMAT('STATVA,STATCD,PVAL2T,PVALLT,PVALUT = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTTE3(X,N,AMU,IWRITE,STATVA,STATCD,STATNU,
     1                  XMEAN,XSD,XSDM,DEL,
     1                  PVAL2T,PVALLT,PVALUT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE ONE SAMPLE T-TEST (AND
C              ALTERNATIVELY THE CDF VALUE).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --AMU    = THE SINGLE PRECISION VALUE FOR WHICH
C                                THE TEST IS PERFORMED (I.E.,
C                                H0: MU = AMU).
C     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED STATISTIC.
C                     --STATCD = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED CDF OF THE TEST STATISTIC.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             TEST STATISTIC.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--TPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009.2
C     ORIGINAL VERSION--FEBRUARY  2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRTSV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
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
      ISUBN1='DPTT'
      ISUBN2='E3  '
      IWRTSV=IWRITE
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTE3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTTE3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3
   52   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,ANU
   53   FORMAT('N,AMU = ',I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *********************************
C               **  COMPUTE ONE SAMPLE T-TEST  **
C               *********************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      STATVA=-99.0
      STATCD=-99.0
      STATNU=-99.0
      PVAL2T=-99.0
      PVALLT=-99.0
      PVALUT=-99.0
      IWRITE='OFF'
C
      AN=N
C
      IF(N.LE.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN ONE SAMPLE T-TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',
     1         'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE MUST BE 2 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1         '.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               *****************************************
C               **  STEP 2--                           **
C               **  COMPUTE THE ONE SAMPLE T-TEST.     **
C               *****************************************
C
      CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
      CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
      CALL SDMEAN(X,N,IWRITE,XSDM,IBUGA3,IERROR)
      DEL=XMEAN-AMU
      STATVA=DEL/XSDM
      IDF=N-1
      STATNU=REAL(IDF)
      CALL TCDF(STATVA,STATNU,STATCD)
C
      PVALLT=STATCD
      PVALUT=1.0 - STATCD
      IF(STATVA.LE.0.0)THEN
        PVAL2T=2.0*PVALLT
      ELSE
        PVAL2T=2.0*PVALUT
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
  800 CONTINUE
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,STATVA
  811   FORMAT('THE VALUE OF THE ONE SAMPLE T-TEST OF THE ',I8,
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
C
      IWRITE=IWRTSV
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTE3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTTE3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)STATVA,STATCD
 9015   FORMAT('STATVA,STATCD = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)XMEAN,XSD,XSDM
 9016   FORMAT('XMEAN,XSD,XSDM = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTTE4(Y1,N1,Y2,N2,IWRITE,
     1                  STATVA,STATCD,STATNU,
     1                  STATV2,STATC2,STATN2,
     1                  Y1MEAN,Y1SD,Y1SDM,
     1                  Y2MEAN,Y2SD,Y2SDM,
     1                  DEL,POOLSD,DELSD,DELSD2,CDFBAR,
     1                  PVAL2T,PVALLT,PVALUT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE UNPAIRED TWO SAMPLE T-TEST
C              (AND ALTERNATIVELY THE CDF OR P-VALUES).
C     INPUT  ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS
C                                FOR THE FIRST RESPONSE VARIABLE.
C                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y1.
C                     --Y2     = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS
C                                FOR THE SECOND RESPONSE VARIABLE.
C                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y2.
C     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED STATISTIC.
C                     --STATCD = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED CDF OF THE TEST STATISTIC.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             TEST STATISTIC.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--TPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011.4
C     ORIGINAL VERSION--APRIL     2011. EXTRACTED FROM DPTTE2 TO
C                                       ALLOWED IT TO BE CALLED FROM
C                                       CMPSTA (I.E., FOR USE AS A
C                                       "STATISTIC")
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRTSV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
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
      ISUBN1='DPTT'
      ISUBN2='E4  '
      IWRTSV=IWRITE
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTE4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTTE4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N1,N2
   53   FORMAT('N1,N2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N1
          WRITE(ICOUT,56)I,Y1(I)
   56     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO65I=1,N1
          WRITE(ICOUT,66)I,Y2(I)
   66     FORMAT('I,Y2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
      ENDIF
C
C               *********************************
C               **  COMPUTE TWO SAMPLE T-TEST  **
C               *********************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      STATVA=-99.0
      STATCD=-99.0
      STATNU=-99.0
      STATV2=-99.0
      STATC2=-99.0
      STATN2=-99.0
      PVAL2T=-99.0
      PVALLT=-99.0
      PVALUT=-99.0
      IWRITE='OFF'
C
      AN=N
C
      IF(N1.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN T-TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',
     1         'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N1
  113   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y1(1)
      DO135I=2,N1
        IF(Y1(I).NE.HOLD)GOTO139
  135 CONTINUE
  130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  139 CONTINUE
C
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,142)
  142   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',
     1         'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y2(1)
      DO155I=2,N1
        IF(Y2(I).NE.HOLD)GOTO159
  155 CONTINUE
  150 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,151)HOLD
  151 FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  159 CONTINUE
C
C               **************************************************
C               **  STEP 2--                                    **
C               **  COMPUTE THE TWO SAMPLE UNPAIRED T-TEST.     **
C               **************************************************
C
      CALL MEAN(Y1,N1,IWRITE,Y1MEAN,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,Y1SD,IBUGA3,IERROR)
      Y1VAR=Y1SD**2
      CALL SDMEAN(Y1,N1,IWRITE,Y1SDM,IBUGA3,IERROR)
C
      CALL MEAN(Y2,N2,IWRITE,Y2MEAN,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,Y2SD,IBUGA3,IERROR)
      Y2VAR=Y2SD**2
      CALL SDMEAN(Y2,N2,IWRITE,Y2SDM,IBUGA3,IERROR)
C
      AN1=N1
      AN2=N2
C
      DEL=Y1MEAN-Y2MEAN
      POOLSS=(AN1-1.0)*Y1VAR+(AN2-1.0)*Y2VAR
      POOLVA=POOLSS/(AN1+AN2-2.0)
      POOLSD=SQRT(POOLVA)
      POOLN=1.0/((1.0/AN1)+(1.0/AN2))
      DELSD=POOLSD/SQRT(POOLN)
      STATVA=DEL/DELSD
      IDF=N1+N2-2
      STATNU=REAL(IDF)
      CALL TCDF(STATVA,STATNU,STATCD)
C
      DEL2=DEL
      DELVA2=(Y1VAR/AN1)+(Y2VAR/AN2)
      DELSD2=SQRT(DELVA2)
      STATV2=DEL2/DELSD2
      C=(Y1VAR/AN1)/((Y1VAR/AN1)+(Y2VAR/AN2))
      TERM1=C*C/(AN1-1.0)
      TERM2=(1-C)*(1-C)/(AN2-1.0)
      SUM=TERM1+TERM2
      STATN2=1.0/SUM
      CALL TCDF(STATV2,STATN2,STATC2)
C
      TERM11=1.0/(AN1-1.0)
      TERM12=1.0/(AN2-1.0)
      TERM13=1.0/(AN1+AN2-2.0)
      SUMC=TERM11+TERM12-TERM13
      CBART=1.0+SUMC/3.0
      TERM21=(AN1-1.0)*2*LOG(Y1SD/POOLSD)
      TERM22=(AN2-1.0)*2*LOG(Y2SD/POOLSD)
      BBART=(-TERM21-TERM22)
      BART=BBART/CBART
      IDFBAR=1
      CALL CHSCDF(BART,IDFBAR,CDFBAR)
C
      PVALLT=STATC2
      PVALUT=1.0 - STATC2
      IF(STATV2.LE.0.0)THEN
        PVAL2T=2.0*PVALLT
      ELSE
        PVAL2T=2.0*PVALUT
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
  800 CONTINUE
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)STATVA
  811   FORMAT('THE VALUE OF THE TWO SAMPLE T-TEST = ',G15.7)
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
      IWRITE=IWRTSV
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTE4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTTE4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)STATVA,STATCD,STATNU
 9015   FORMAT('STATVA,STATCD,STATNU = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)STATV2,STATC2,STATN2
 9016   FORMAT('STATV2,STATC2,STATN2 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)Y1MEAN,Y1SD,Y1SDM
 9017   FORMAT('Y1MEAN,Y1SD,Y1SDM = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)Y2MEAN,Y2SD,Y2SDM
 9018   FORMAT('Y2MEAN,Y2SD,Y2SDM = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTTE5(ICASAN,STATVA,STATCD,STATNU,
     1                  STATV2,STATC2,STATN2,
     1                  PVAL2T,PVALLT,PVALUT,
     1                  CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,
     1                  CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,
     1                  IFLAGU,IFRST,ILAST,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPTTES TO UPDATE VARIOUS
C              INTERNAL PARAMETERS AFTER A T-TEST.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/4
C     ORIGINAL VERSION--APRIL     2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 IFLAGU
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
C
      CHARACTER*4 IOP
      SAVE IOUNI1
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TTE5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTTE5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)STATVA,STATCD,STATNU,PVAL2T,PVALLT,PVALUT
   53   FORMAT('STATVA,STATCD,STATNU,PVAL2T,PVALLT,PVALUT = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)CUTL50,CUTL80,CUTL90,CUTL95,CUTL99,CTL999
   54   FORMAT('CUTL50,CUTL80,CUTL90,CUTL95,CUTL99,CTL999 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)CUTU50,CUTU80,CUTU90,CUTU95,CUTU99,CTU999
   55   FORMAT('CUTU50,CUTU80,CUTU90,CUTU95,CUTU99,CTU999 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASAN.EQ.'ONES' .OR. ICASAN.EQ.'PDTE')THEN
        STATV=STATVA
        STATC=STATCD
        STATN=STATNU
      ELSE
        STATV=STATV2
        STATC=STATC2
        STATN=STATN2
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
C
        IF(IFRST)THEN
          IOP='OPEN'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          WRITE(IOUNI1,295)
  295     FORMAT(11X,'STATVAL',8X,'STATCDF',8X,'STATNU',
     1            9X,'PVAL2T',9X,'PVALLT',X,'PVALUT',
     1            7X,'CUTLOW50',7X,'CUTLOW80',7X,'CUTLOW90',
     1            7X,'CUTLOW95',7X,'CUTLOW99',7X,'CUTLO999',
     1            7X,'CUTUPP50',7X,'CUTUPP80',7X,'CUTUPP90',
     1            7X,'CUTUPP95',7X,'CUTUPP99',7X,'CUTUP999')
        ENDIF
        WRITE(IOUNI1,299)STATV,STATC,STATN,PVAL2T,PVALLT,PVALUT,
     1                   CUTL50,CUTL80,CUTL90,CUTL95,CUTL99,CTL999,
     1                   CUTU50,CUTU80,CUTU90,CUTU95,CUTU99,CTU999
  299   FORMAT(18E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(STATV.NE.CPUMIN)THEN
          IH='STAT'
          IH2='VAL '
          VALUE0=STATV
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(STATC.NE.CPUMIN)THEN
          IH='STAT'
          IH2='CDF '
          VALUE0=STATC
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(STATN.NE.CPUMIN)THEN
          IH='STAT'
          IH2='NU  '
          VALUE0=STATN
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(PVAL2T.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UE  '
          VALUE0=PVAL2T
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(PVALLT.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UELT'
          VALUE0=PVALLT
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(PVALUT.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UEUT'
          VALUE0=PVALUT
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU50.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP50'
          VALUE0=CUTU50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTUL0.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW50'
          VALUE0=CUTU50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU80.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP80'
          VALUE0=CUTU80
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTL80.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW80'
          VALUE0=CUTL80
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU90.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP90'
          VALUE0=CUTU90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTL90.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW90'
          VALUE0=CUTL90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU95.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP95'
          VALUE0=CUTU95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTL95.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW95'
          VALUE0=CUTL95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU99.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP99'
          VALUE0=CUTU99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTL99.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW99'
          VALUE0=CUTL99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CTU999.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='P999'
          VALUE0=CTU999
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CTL999.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='O999'
          VALUE0=CTL999
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
        IF(ILAST)THEN
          IOP='CLOS'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TTE5')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR
  301       FORMAT('AFTER CALL DPCLFI, IERROR = ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TTE5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPTTE5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTTE6(Y1,N1,Y2,N2,YTEMP,IWRITE,
     1                  STATVA,STATCD,STATNU,
     1                  Y1MEAN,Y1SD,Y1SDM,
     1                  Y2MEAN,Y2SD,Y2SDM,
     1                  YDMEAN,YDSD,YDSDM,
     1                  PVAL2T,PVALLT,PVALUT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PAIRED TWO SAMPLE T-TEST
C              (AND ALTERNATIVELY THE CDF OR P-VALUES).
C     INPUT  ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS
C                                FOR THE FIRST RESPONSE VARIABLE.
C                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y1.
C                     --Y2     = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS
C                                FOR THE SECOND RESPONSE VARIABLE.
C                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y2.
C     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED STATISTIC.
C                     --STATCD = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED CDF OF THE TEST STATISTIC.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             TEST STATISTIC.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--TCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2888
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011.4
C     ORIGINAL VERSION--APRIL     2011
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRTSV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION YTEMP(*)
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
      ISUBN1='DPTT'
      ISUBN2='E6  '
      IWRTSV=IWRITE
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTE6')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTTE6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N1,N2
   53   FORMAT('N1,N2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,MIN(N1,N2)
          WRITE(ICOUT,56)I,Y1(I),Y2(I)
   56     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ****************************************
C               **  COMPUTE TWO SAMPLE PAIRED T-TEST  **
C               ****************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      STATVA=-99.0
      STATCD=-99.0
      STATNU=-99.0
      PVAL2T=-99.0
      PVALLT=-99.0
      PVALUT=-99.0
      IWRITE='OFF'
C
      IF(N1.NE.N2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      FOR THE PAIRED TEST, THE SAMPLE SIZES FOR THE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)
  103   FORMAT('      RESPONSE VARIABLES MUST BE EQUAL.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,104)N1
  104   FORMAT('SAMPLE SIZE FOR THE FIRST  RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,105)N2
  105   FORMAT('SAMPLE SIZE FOR THE SECOND RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N1.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN PAIRED T-TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',
     1         'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N1
  113   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y1(1)
      DO135I=2,N1
        IF(Y1(I).NE.HOLD)GOTO139
  135 CONTINUE
  130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  139 CONTINUE
C
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,142)
  142   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',
     1         'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y2(1)
      DO155I=2,N1
        IF(Y2(I).NE.HOLD)GOTO159
  155 CONTINUE
  150 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,151)HOLD
  151 FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  159 CONTINUE
C
C               **************************************************
C               **  STEP 2--                                    **
C               **  COMPUTE THE TWO SAMPLE PAIRED T-TEST.       **
C               **************************************************
C
      DO200I=1,N1
        YTEMP(I)=Y1(I) - Y2(I)
  200 CONTINUE
C
      CALL MEAN(Y1,N1,IWRITE,Y1MEAN,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,Y1SD,IBUGA3,IERROR)
      Y1VAR=Y1SD**2
      CALL SDMEAN(Y1,N1,IWRITE,Y1SDM,IBUGA3,IERROR)
C
      CALL MEAN(Y2,N2,IWRITE,Y2MEAN,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,Y2SD,IBUGA3,IERROR)
      Y2VAR=Y2SD**2
      CALL SDMEAN(Y2,N2,IWRITE,Y2SDM,IBUGA3,IERROR)
C
      CALL MEAN(YTEMP,N2,IWRITE,YDMEAN,IBUGA3,IERROR)
      CALL SD(YTEMP,N2,IWRITE,YDSD,IBUGA3,IERROR)
      YDVAR=YDSD**2
      CALL SDMEAN(YTEMP,N2,IWRITE,YDSDM,IBUGA3,IERROR)
C
      AN1=N1
      AN2=N2
C
      DEL=Y1MEAN-Y2MEAN
      STATVA=DEL/YDSDM
      IDF=N1-1
      STATNU=REAL(IDF)
      CALL TCDF(STATVA,STATNU,STATCD)
C
      PVALLT=STATCD
      PVALUT=1.0 - STATCD
      IF(STATVA.LE.0.0)THEN
        PVAL2T=2.0*PVALLT
      ELSE
        PVAL2T=2.0*PVALUT
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
  800 CONTINUE
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)STATVA
  811   FORMAT('THE VALUE OF THE PAIRED TWO SAMPLE T-TEST = ',G15.7)
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
      IWRITE=IWRTSV
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TTE6')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTTE6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)STATVA,STATCD,STATNU
 9015   FORMAT('STATVA,STATCD,STATNU = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)Y1MEAN,Y1SD,Y1SDM
 9017   FORMAT('Y1MEAN,Y1SD,Y1SDM = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)Y2MEAN,Y2SD,Y2SDM
 9018   FORMAT('Y2MEAN,Y2SD,Y2SDM = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)YDMEAN,YDSD,YDSDM
 9019   FORMAT('YDMEAN,YDSD,YDSDM = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTUMD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IANGLU,MAXNPP,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--FORM A TUKEY MEAN DIFFERENCE PLOT
C              (USEFUL FOR DISTRIBUTIONALLY COMPARING 2 DATA SETS).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/8
C     ORIGINAL VERSION--SEPTEMBER 1999 .
C     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR "HIGHLIGHTED" OPTION
C     UPDATED         --JUNE      2016. ALLOW USER-SPECIFED PERCENTILES
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASE
      CHARACTER*4 IHIGH
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
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
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION Y4(MAXOBV)
      DIMENSION XD(MAXOBV)
      DIMENSION YD(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION XDIST(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      DIMENSION YLARGE(MAXOBV)
      DIMENSION YSMALL(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),Y3(1))
      EQUIVALENCE (GARBAG(IGARB4),Y4(1))
      EQUIVALENCE (GARBAG(IGARB5),XD(1))
      EQUIVALENCE (GARBAG(IGARB6),YD(1))
      EQUIVALENCE (GARBAG(IGARB7),YLARGE(1))
      EQUIVALENCE (GARBAG(IGARB8),YSMALL(1))
      EQUIVALENCE (GARBAG(IGARB9),XHIGH(1))
      EQUIVALENCE (GARBAG(IGAR10),XDIST(1))
      EQUIVALENCE (GARBAG(JGAR11),TEMP1(1))
      EQUIVALENCE (GARBAG(JGAR12),TEMP2(1))
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
      ISUBN1='DPTU'
      ISUBN2='MD  '
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTUMD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPLOTV,NPLOTP,NS,MAXN,MAXNPP,IQQNPR
   52   FORMAT('NPLOTV,NPLOTP,NS,MAXN,MAXNPP,IQQNPR = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IANGLU,IBUGG2,IBUGG3,IBUGQ,ISUBRO
   54   FORMAT('IANGLU,IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IFOUND,IERROR
   57   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  TREAT THE TUKEY MEAN-DIFFERENCE CASE **
C               *******************************************
C
C               ***************************
C               **  STEP 11--            **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHIGH='OFF'
      IF(ICOM.EQ.'TUKE')THEN
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MEAN'.AND.
     1    IHARG(2).EQ.'DIFF')THEN
           IF((IHARG(3).EQ.'HIGH' .OR. IHARG(3).EQ.'SUBS') .AND.
     1       IHARG(4).EQ.'PLOT')THEN
             IHIGH='ON'
             ILASTC=4
           ELSEIF(IHARG(3).EQ.'PLOT')THEN
             ILASTC=3
           ELSE
             GOTO9000
           ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'M   '.AND.
     1        IHARG(2).EQ.'D   ')THEN
          IF((IHARG(3).EQ.'HIGH' .OR. IHARG(3).EQ.'SUBS') .AND.
     1      IHARG(4).EQ.'PLOT')THEN
             ILASTC=4
             IHIGH='ON'
          ELSEIF(IHARG(3).EQ.'PLOT')THEN
             ILASTC=3
          ELSE
             GOTO9000
          ENDIF
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'MD  ')THEN
          IF((IHARG(2).EQ.'HIGH' .OR. IHARG(2).EQ.'SUBS') .AND.
     1      IHARG(3).EQ.'PLOT')THEN
             ILASTC=3
             IHIGH='ON'
          ELSEIF(IHARG(2).EQ.'PLOT')THEN
             ILASTC=2
          ELSE
             GOTO9000
          ENDIF
        ENDIF
      ELSEIF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')THEN
        IHIGH='ON'
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'TUKE'.AND.
     1    IHARG(2).EQ.'MEAN'.AND.IHARG(3).EQ.'DIFF'.AND.
     1    IHARG(4).EQ.'PLOT')THEN
             IHIGH='ON'
             ILASTC=4
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'TUKE'.AND.
     1    IHARG(2).EQ.'M   '.AND.IHARG(3).EQ.'D   '.AND.
     1    IHARG(4).EQ.'PLOT')THEN
             ILASTC=3
             IHIGH='ON'
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'TUKE'.AND.
     1    IHARG(2).EQ.'MD  '.AND.IHARG(3).EQ.'PLOT')THEN
             ILASTC=3
        ELSE
          GOTO9000
        ENDIF
      ELSE
        GOTO9000
      ENDIF
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
      ICASPL='TUMD'
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='TUKEY MEAN-DIFFERENCE PLOT'
      MINNA=2
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=2
      IF(IHIGH.EQ.'ON')THEN
        MINNA=3
        MINNVA=3
        MAXNVA=3
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')THEN
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
      DO290I=1,MAX(NRIGHT(1),NRIGHT(2))
        XHIGH(I)=1.0
  290 CONTINUE
C
C     IN ORDER TO ACCOMODATE MATRIX ARGUMENTS, CALL EACH
C     VARIABLE SEPARATELY.
C
      NUMVA2=1
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y1,Y1,NS1,NTEMP,NTEMP,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      ICOL=2
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y2,Y2,Y2,NS2,NTEMP,NTEMP,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
C
      IF(IHIGH.EQ.'ON')THEN
        ICOL=3
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              XHIGH,XHIGH,XHIGH,NHIGH,NTEMP,NTEMP,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
      ELSE
        NHIGH=0
      ENDIF
C
C               ****************************************************
C               **  STEP 41--                                      *
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          *
C               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY) FOR    *
C               **   THE PLOT.                                     *
C               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .    *
C               **  THIS WILL BE BOTH ONES FOR BOTH CASES          *
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  *
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  *
C               ****************************************************
C
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NS=NS1
      IF(NS2.GT.NS1)NS=NS2
      CALL DPTUM2(Y1,NS1,Y2,NS2,ICASPL,MAXN,IQQNPR,
     1            Y,X,D,NPLOTP,NPLOTV,
     1            YLARGE,YSMALL,TEMP1,TEMP2,
     1            XHIGH,NHIGH,XDIST,
     1            IBUGG3,ISUBRO,IERROR)
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTUMD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NUMVAR,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NUMVAR,NS,ICASPL,IAND1,IAND2 = ',
     1         4I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASPL,MAXN,NUMVAR
 9014   FORMAT('ICASPL,MAXN,NUMVAR = ',A4,I8,I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9020I=1,NPLOTP
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
 9029   ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTUM2(Y,NY,X,NX,ICASPL,MAXN,IQQNPR,
     1                  Y2,X2,D2,N2,NPLOTV,
     1                  YLARGE,YSMALL,TEMP1,TEMP2,
     1                  XHIGH,NHIGH,XDIST,
     1                  IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A TUKEY MEAN-DIFFERENCE PLOT
C              (USEFUL FOR DISTRIBUTIONALLY COMPARING 2 DATA SETS).
C              AFTER CALCULATING COORDINATES FOR Q-Q PLOT, CALCULATE
C              (Bi - Ti) VERSUS (Bi+Ti)/2 WHERE Bi AND Ti ARE
C              THE QUANTILES FOR THE RESPECTIVE DATA SETS.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/9
C     ORIGINAL VERSION--SEPTEMBER 1999.
C     UPDATED         --FEBRUARY  2011.
C     UPDATED         --JUNE      2016. ALLOW USER-SPECIFED PERCENTILES
C     UPDATED         --JUNE      2016. DON'T TREAT N=1 OR ALL DATA
C                                       VALUES EQUAL AS AN ERROR.  TREAT
C                                       AS A "DEGENERATE" CASE.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASE
      CHARACTER*4 ICASPL
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION XHIGH(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION YLARGE(*)
      DIMENSION YSMALL(*)
      DIMENSION XDIST(*)
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
      ISUBN1='DPQU'
      ISUBN2='M2  '
      IERROR='NO'
      IWRITE='OFF'
      ICASE=ICASPL
C
      ANY=NY
      ANX=NX
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TUM2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTUM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,ICASPL
   52   FORMAT('IBUGG3,ISUBRO,ICASPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NX,NY,NHIGH,IQQNPR
   53   FORMAT('NX,NY.NHIGH,IQQNPR = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NY.GE.1)THEN
          DO61I=1,NY
            WRITE(ICOUT,62)I,Y(I)
   62       FORMAT('I,Y(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
        IF(NX.GE.1)THEN
          DO71I=1,NX
           WRITE(ICOUT,72)I,X(I)
   72      FORMAT('I,X(I) = ',I8,G15.7)
           CALL DPWRST('XXX','BUG ')
   71    CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
C     2016/06: ONLY REQUIRE N >= 1.
C
CCCCC IF(NY.LT.2)THEN
      IF(NY.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN TUKEY MEAN DIFFERENCE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',
     1         'RESPONSE VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)
 1113   FORMAT('      MUST BE AT LEAST 1;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NY
 1114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NX.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1122)
 1122   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',
     1         'RESPONSE VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NX
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NHIGH.GT.0 .AND. NHIGH.NE.MIN(NX,NY))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1125)
 1125   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE HIGHLIGHTING ',
     1         'VARIABLE IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1126)
 1126   FORMAT('      NOT EQUAL TO THE NUMBER OF OBSERVATIONS IN THE ',
     1         'SHORTER RESPONSE VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1127)NY
 1127   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST     ',
     1         'RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1128)NX
 1128   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND    ',
     1         'RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1129)NHIGH
 1129   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE HIGHLIGHT ',
     1         'VARIABLE          = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
CCCCC HOLD=Y(1)
CCCCC DO1130I=1,NY
CCCCC   IF(Y(I).NE.HOLD)GOTO1139
C1130 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1111)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1132)
C1132 FORMAT('      ALL INPUT ELEMENTS FOR THE FIRST RESPONSE VARIABLE')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1133)HOLD
C1133 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C1139 CONTINUE
C
CCCCC HOLD=X(1)
CCCCC DO1140I=1,NY
CCCCC   IF(X(I).NE.HOLD)GOTO1149
C1140 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1111)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1142)
C1142 FORMAT('      ALL INPUT ELEMENTS FOR THE SECOND RESPONSE ',
CCCCC1       'VARIABLE')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1133)HOLD
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C1149 CONTINUE
C
C               ****************************************************
C               **  STEP 21--                                     **
C               **  SORT Y AND SORT X                             **
C               ****************************************************
C
      IF(NHIGH.LE.0)THEN
        IF(IQQNPR.GT.0)THEN
          CALL PERCE2(IQQNPR,X,NX,IWRITE,TEMP2,MAXN,TEMP1,
     1                IBUGG3,ISUBRO,IERROR)
          DO2010II=1,IQQNPR
            X(II)=TEMP1(II)
 2010     CONTINUE
          NX=IQQNPR
C
          CALL PERCE2(IQQNPR,Y,NY,IWRITE,TEMP2,MAXN,TEMP1,
     1                IBUGG3,ISUBRO,IERROR)
          DO2020II=1,IQQNPR
            Y(II)=TEMP2(II)
 2020     CONTINUE
          NY=IQQNPR
C
        ELSE
          CALL SORT(X,NX,X)
          CALL SORT(Y,NY,Y)
        ENDIF
      ELSEIF(NY.LE.NX)THEN
        CALL SORT(X,NX,X)
        CALL SORTC(Y,XHIGH,NY,Y,XDIST)
        DO2101I=1,NY
          XHIGH(I)=XDIST(I)
 2101   CONTINUE
      ELSEIF(NY.GT.NX)THEN
        CALL SORT(Y,NY,Y)
        CALL SORTC(X,XHIGH,NX,X,XDIST)
        DO2103I=1,NX
          XHIGH(I)=XDIST(I)
 2103   CONTINUE
      ENDIF
C
C               *****************************************
C               **  STEP 22--                          **
C               **  DETERMINE THE TYPE CASE            **
C               **  EQUAL SAMPLE SIZES OR NOT)         **
C               **  AND BRANCH ACORDINGLY              **
C               *****************************************
C
      ICASE='UNEQ'
      IF(NY.EQ.NX)ICASE='EQUA'
      IF(ICASE.EQ.'EQUA')GOTO5100
C
C               **************************************************
C               **  STEP 23--                                   **
C               **  DETERMINE THE SMALLER OF THE 2--            **
C               **  NY OR NX                                    **
C               **  DETERMINE THE LARGER OF THE 2--             **
C               **  NY OR NX                                    **
C               **************************************************
C
      NSMALL=NX
      IF(NY.LT.NX)NSMALL=NY
      ANSMAL=NSMALL
C
      NLARGE=NX
      IF(NY.GT.NX)NLARGE=NY
      ANLARG=NLARGE
C
C               ****************************************************
C               **  STEP 24--                                     **
C               **  STEP THROUGH THE VARIOUS SORTED VALUES OF     **
C               **  THE SMALLER OF Y OR X.                        **
C               **  COMPUTE A CORRESPONDING PERCENTAGE.           **
C               **  ESTIMATE THIS PERCENT  POINT                  **
C               **  IN THE LARGER OF Y OR X.                      **
C               ****************************************************
C
      DO2400I=1,NSMALL
        AI=I
        PSMALL=(AI-0.5)/ANSMAL
        IF(NY.LE.NX)YSMALL(I)=Y(I)
        IF(NY.GT.NX)YSMALL(I)=X(I)
C
        PLARGE=0.0
        DO2410J=1,NLARGE
          AJ=J
          J2=J
          J2M1=J2-1
          PPRIOR=PLARGE
          PLARGE=(AJ-0.5)/ANLARG
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TUM2')THEN
            WRITE(ICOUT,777)I,J,J2,J2M1,PSMALL,PLARGE,PPRIOR
  777       FORMAT('I,J,J2,J2M1,PSMALL,PLARGE,PPRIOR = ',4I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(PLARGE.LT.PSMALL)GOTO2410
          IF(PLARGE.EQ.PSMALL)THEN
            IF(NY.LE.NX)YLARGE(I)=X(J2)
            IF(NY.GT.NX)YLARGE(I)=Y(J2)
          ELSE
            RATIO=(PSMALL-PPRIOR)/(PLARGE-PPRIOR)
            IF(NY.LE.NX)YLARGE(I)=RATIO*X(J2M1)+(1.0-RATIO)*X(J2)
            IF(NY.GT.NX)YLARGE(I)=RATIO*Y(J2M1)+(1.0-RATIO)*Y(J2)
          ENDIF
          GOTO2400
 2410   CONTINUE
 2400 CONTINUE
C
C               *******************************************
C               **  STEP 51--                            **
C               **  FORM PLOT COORDINATES                **
C               *******************************************
C
 5100 CONTINUE
C
      ISTEPN='51'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TUM2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NHIGH.GT.0)THEN
        CALL CODE(XHIGH,NHIGH,IWRITE,XDIST,D2,MAXN,IBUGG3,IERROR)
        CALL MAXIM(XDIST,NHIGH,IWRITE,XMAX,IBUGG3,IERROR)
      ELSE
        XMAX=1.0
      ENDIF
C
      IF(ICASE.EQ.'EQUA')THEN
        J=0
        DO5111I=1,NY
          J=J+1
          ADIFF=Y(I)-X(I)
          AMEAN=(Y(I)+X(I))/2.0
          Y2(J)=ADIFF
          X2(J)=AMEAN
          IF(NHIGH.EQ.0)THEN
            D2(J)=1.0
          ELSE
            D2(J)=XDIST(J)
          ENDIF
 5111   CONTINUE
        J=J+1
        X2(J)=X2(1)
        Y2(J)=0.0
        D2(J)=XMAX+1.0
        J=J+1
        X2(J)=X2(NY)
        Y2(J)=0.0
        D2(J)=XMAX+1.0
C
      ELSE
C
        J=0
        DO5121I=1,NSMALL
          J=J+1
          IF(NY.LE.NX)Y2(J)=YSMALL(I)
          IF(NY.GT.NX)Y2(J)=YLARGE(I)
          IF(NY.LE.NX)X2(J)=YLARGE(I)
          IF(NY.GT.NX)X2(J)=YSMALL(I)
          IF(NHIGH.EQ.0)THEN
            D2(J)=1.0
          ELSE
            D2(J)=XDIST(J)
          ENDIF
          ADIFF=Y2(J)-X2(J)
          AMEAN=(Y2(J)+X2(J))/2.0
          Y2(J)=ADIFF
          X2(J)=AMEAN
 5121   CONTINUE
C
        J=J+1
        X2(J)=X2(1)
        Y2(J)=0.0
        D2(J)=XMAX+1.0
        J=J+1
        X2(J)=X2(NSMALL)
        Y2(J)=0.0
        D2(J)=XMAX+1.0
      ENDIF
C
      N2=J
      NPLOTV=3
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TUM2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTUM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,ICASE,IERROR,MAXNXT,N2
 9012   FORMAT('ICASPL,ICASE,IERROR,MAXNXT,N2 = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9031)NY,NX,NSMALL,NLARGE,RATIO
 9031   FORMAT('NY,NX,NSMALL,NLARGE,RATIO = ',4I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO9032I=1,NLARGE
          WRITE(ICOUT,9033)I,YLARGE(I)
 9033     FORMAT('I,YLARGE(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9032   CONTINUE
        DO9042I=1,NSMALL
          WRITE(ICOUT,9043)I,YSMALL(I)
 9043     FORMAT('I,YSMALL(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9042   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTWFP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GIVEN DATA OF THE FORM
C
C                 RESPONSE   LAB-ID  MAT-ID
C
C              GENERATE EITHER A "LABORATORIES WITHIN MATERIALS"
C              OR A "MATERIALS WITHIN LABORATORIES" PLOT.
C
C              THIS IS ESSENTIALLY A RUN SEQUENCE PLOT SORTED
C              BY 2 FACTOR VARIABLES.
C
C              THIS PLOT IS MOTIVATED BY THE DESIRE TO PLOT
C              RESIDUALS FOR THE "PHASE 3" ANALYSIS IN THE
C              ASTM E-691 STANDARD.  THE PHASE 3 ANALYSIS
C              (ESSENTIALLY A ROW-LINEAR MODEL FOR THE TABLE)
C              WAS SUGGESTED BY JOHN MANDEL (SEE REFERENCES
C              BELOW) AS AN ADDITIONAL STEP IN THE E-691 ANALYSIS
C              (THE PHASE 3 ANALYSIS IS NOT PART OF THE STANDARD).
C              IN PARTICULAR, HE RECOMMENDED A PLOT OF THE
C              STANDARDIZED RESIDUALS FROM THE ROW-LINEAR MODEL
C              (SPECIFIC PLOTS FOR THE H AND K CONSISTENCY
C              STATISTICS CAN ALREADY BE GENERATED USING THE
C              H CONSISTENCY PLOT COMMAND).
C
C              ALTHOUGH MOTIVATED BY THE EXTENSION TO THE
C              ASTM E-691 STANDARD, THIS PLOT CAN BE APPLIED
C              TO ANY TWO FACTOR SET OF DATA.
C
C              THERE ARE TWO FORMATS FOR THE PLOT:
C
C              1) THE VALUES ARE PLOTTED LINEARLY.  THAT IS,
C
C                 LAB:  1  2  3  1  2  3  1  2  3
C                 MAT:  1  1  1  2  2  2  3  3  3
C
C              2) YOU CAN STACK THE LAB VALUES VERTICALLY
C
C                 LAB:  1  1  1
C                       2  2  2
C                       3  3  3
C                 MAT:  1  2  3
C
C              MULTIPLE AND REPLICATION OPTIONS ARE NOT SUPPORTED
C              FOR THIS PLOT.
C
C
C     EXAMPLE--TWO FACTOR PLOT Y LABID MATID
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/6
C     ORIGINAL VERSION--JUNE       2015.
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
      CHARACTER*4 IH2
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
      REAL Y1(MAXOBV)
      REAL MATID(MAXOBV)
      REAL LABID(MAXOBV)
      REAL XIDTEM(MAXOBV)
      REAL XIDTE2(MAXOBV)
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),MATID(1))
      EQUIVALENCE (GARBAG(IGARB3),LABID(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP2(1))
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
      ISUBN1='DPTW'
      ISUBN2='FP  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ****************************************
C               **  TREAT THE TWO FACTOR PLOT CASE    **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTWFP--')
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.2.AND.ICOM.EQ.'TWO '.AND.IHARG(1).EQ.'FACT'.AND.
     1  IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        ICASPL='TWFP'
      ELSE
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='TWO FACTOR PLOT'
      MINNA=3
      MAXNA=100
      MINN2=5
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=3
      MAXNVA=3
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')THEN
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
C               **       LABID(.)                           **
C               **       MATID(.)                           **
C               **********************************************
C
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,LABID,MATID,XIDTEM,XIDTEM,XIDTEM,XIDTEM,NS,
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPTWF2(Y1,LABID,MATID,NS,NUMVAR,ICASPL,
     1            ITWFPT,ITWFGP,ITWFLM,
     1            ITWFM1,ITWFM2,ITWFL1,ITWFL2,
     1            XIDTEM,XIDTE2,TEMP1,TEMP2,
     1            Y,X,D,
     1            NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTWFP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTWF2(Y1,LABID,MATID,N,NUMVAR,ICASPL,
     1                  ITWFPT,ITWFGP,ITWFLM,
     1                  ITWFM1,ITWFM2,ITWFL1,ITWFL2,
     1                  XIDTEM,XIDTE2,TEMP1,TEMP2,
     1                  Y,X,D,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GIVEN DATA OF THE FORM
C
C                 RESPONSE   LAB-ID  MAT-ID
C
C              GENERATE EITHER A "LABORATORIES WITHIN MATERIALS"
C              OR A "MATERIALS WITHIN LABORATORIES" PLOT.
C
C              THIS IS ESSENTIALLY A RUN SEQUENCE PLOT SORTED
C              BY 2 FACTOR VARIABLES.
C
C              THIS PLOT IS MOTIVATED BY THE DESIRE TO PLOT
C              RESIDUALS FOR THE "PHASE 3" ANALYSIS IN THE
C              ASTM E-691 STANDARD.  THE PHASE 3 ANALYSIS
C              (ESSENTIALLY A ROW-LINEAR MODEL FOR THE TABLE)
C              WAS SUGGESTED BY JOHN MANDEL (SEE REFERENCES
C              BELOW) AS AN ADDITIONAL STEP IN THE E-691 ANALYSIS
C              (THE PHASE 3 ANALYSIS IS NOT PART OF THE STANDARD).
C              IN PARTICULAR, HE RECOMMENDED A PLOT OF THE
C              STANDARDIZED RESIDUALS FROM THE ROW-LINEAR MODEL
C              (SPECIFIC PLOTS FOR THE H AND K CONSISTENCY
C              STATISTICS CAN ALREADY BE GENERATED USING THE
C              H CONSISTENCY PLOT COMMAND).
C
C              ALTHOUGH MOTIVATED BY THE EXTENSION TO THE
C              ASTM E-691 STANDARD, THIS PLOT CAN BE APPLIED
C              TO ANY TWO FACTOR SET OF DATA.
C
C              THERE ARE TWO FORMATS FOR THE PLOT:
C
C              1) THE VALUES ARE PLOTTED LINEARLY.  THAT IS,
C
C                 LAB:  1  2  3  1  2  3  1  2  3
C                 MAT:  1  1  1  2  2  2  3  3  3
C
C              2) YOU CAN STACK THE LAB VALUES VERTICALLY
C
C                 LAB:  1  1  1
C                       2  2  2
C                       3  3  3
C                 MAT:  1  2  3
C
C              MULTIPLE AND REPLICATION OPTIONS ARE NOT SUPPORTED
C              FOR THIS PLOT.
C
C     REFERENCES--"Standard Practice for Conducting an Interlaboratory
C                 Study to Determine the Precision of a Test Method",
C                 ASTM International, 100 Barr Harbor Drive, PO BOX C700,
C                 West Conshohoceken, PA 19428-2959, USA.
C               --Mandel (1994), "Analyzing Interlaboratory Data
C                 According to ASTM Standard E691", Quality and
C                 Statistics: Total Quality Management,ASTM STP 1209,
C                 Kowalewski, Ed., American Society for Testing and
C                 Materials, Philadelphia, PA 1994, pp. 59-70.
C               --Mandel (1995), "Structure and Outliers in
C                 Interlaboratory Studies", Journal of Testing and
C                 Evaluation, Vol. 23, No. 5, pp. 364-369.
C               --Mandel (1991), "Evaluation and Control of
C                 Measurements", Marcel Dekker, Inc., chapter 7.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/6
C     ORIGINAL VERSION--JUNE      2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ITWFPT
      CHARACTER*4 ITWFLM
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASAN
C
C---------------------------------------------------------------------
C
      REAL Y1(*)
      REAL MATID(*)
      REAL LABID(*)
      REAL XIDTEM(*)
      REAL XIDTE2(*)
      REAL TEMP1(*)
      REAL TEMP2(*)
C
      REAL Y(*)
      REAL X(*)
      REAL D(*)
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
      ISUBN1='DPTW'
      ISUBN2='F2  '
      IWRITE='OFF'
      IERROR='NO'
      NPLOTP=0
      NPLOTV=3
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWF2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPTWF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IBUGG3,ISUBRO,ICASPL,N,NUMVAR
   72   FORMAT('IBUGG3,ISUBRO,ICASPL,N,NUMVAR = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO81I=1,N
            WRITE(ICOUT,82)I,Y1(I),MATID(I),LABID(I)
   82       FORMAT('I,Y1(I),MATID(I),LABID(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN TWO FACTOR PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 5.')
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
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (LABID, MATID).         **
C               **  CHECK FOR MISSING CELLS AND FOR REPLICATION     **
C               **  WITHIN CELLS (REPLICATED VALUES WILL BE         **
C               **  REPLACED WITH THEIR MEAN VALUE).                **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWF2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DISTIN(LABID,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(MATID,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
C     CHECK FOR MISSING CELLS (PLOT CURRENTLY NOT SUPPORTED FOR
C     CASE WHERE THERE IS MISSING CELLS).  IF REPLICATION IS DETECTED,
C     REPLACE RAW DATA WITH CELL AVERAGES.
C
      IREPL=0
      DO110ISET1=1,NUMSE1
        AHOLD1=XIDTEM(ISET1)
        DO120ISET2=1,NUMSE2
          AHOLD2=XIDTE2(ISET2)
          K=0
          DO130I=1,N
            IF(LABID(I).EQ.AHOLD1 .AND. MATID(I).EQ.AHOLD2)THEN
              K=K+1
              GOTO139
            ENDIF
  130     CONTINUE
  139     CONTINUE
          IF(K.EQ.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,142)
  142       FORMAT('      THERE IS NO DATA FOR:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,144)AHOLD1
  144       FORMAT('      GROUP ONE VARIABLE WITH VALUE: ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,146)AHOLD2
  146       FORMAT('      GROUP TWO VARIABLE WITH VALUE: ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,148)
  148       FORMAT('      THIS COMMAND IS NOT SUPPORTED FOR THE CASE ',
     1             'WHERE THERE ARE MISSING CELLS.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSEIF(K.GT.1)THEN
            IREPL=1
          ENDIF
  120   CONTINUE
  110 CONTINUE
C
C     IF REPLICATION DETECTED, REPLACE RAW VALUES WITH MEANS
C
      ICNT=0
      IF(IREPL.EQ.1)THEN
        DO210ISET1=1,NUMSE1
          AHOLD1=XIDTEM(ISET1)
          DO220ISET2=1,NUMSE2
            AHOLD2=XIDTE2(ISET2)
            K=0
            DO230I=1,N
              IF(LABID(I).EQ.AHOLD1 .AND. MATID(I).EQ.AHOLD2)THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
  230       CONTINUE
C
            ICNT=ICNT+1
            IF(K.EQ.1)THEN
              TEMP2(ICNT)=TEMP1(1)
            ELSE
              CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
              TEMP2(ICNT)=XMEAN
            ENDIF
            XIDTEM(ICNT)=AHOLD1
            XIDTE2(ICNT)=AHOLD2
C
  220     CONTINUE
  210   CONTINUE
C
        DO310I=1,ICNT
          Y(I)=TEMP2(I)
          LABID(I)=XIDTEM(I)
          MATID(I)=XIDTE2(I)
  310   CONTINUE
        N=ICNT
      ENDIF
C
C
C
C               ********************************************
C               **  STEP 2--                              **
C               **  GENERATE THE PLOT COORDINATES.        **
C               ********************************************
C
C       NOTE: TYPICALLY, WE WANT TO COMPUTE THE H AND K CONSISTENCY
C             STATISTICS BASED ON ALL LAB's AND MATERIALS.  HOWEVER,
C             WE SOMETIMES WANT TO RESTRICT THE PLOT TO A SUBSET
C             OF MATERIALS OR LABORATORIES FOR BETTER PLOT
C             RESOLUTION.
C
C             TO ADDRESS THIS, THE FOLLOWING COMMANDS WERE ADDED:
C
C                 SET TWO FACTOR PLOT MATERIAL   FIRST <value>
C                 SET TWO FACTOR PLOT MATERIAL   LAST  <value>
C                 SET TWO FACTOR PLOT LABORATORY FIRST <value>
C                 SET TWO FACTOR PLOT LABORATORY LAST  <value>
C
      IWRITE='OFF'
      CALL DISTIN(LABID,N,IWRITE,XIDTEM,NLAB,IBUGG3,IERROR)
      CALL DISTIN(MATID,N,IWRITE,XIDTE2,NMAT,IBUGG3,IERROR)
      NTOT=NLAB*NMAT
      IMAT1=ITWFM1
      IF(IMAT1.LT.1 .OR. IMAT1.GT.NMAT)IMAT1=1
      IMAT2=ITWFM2
      IF(IMAT2.LT.1 .OR. IMAT2.GT.NMAT)IMAT2=NMAT
      ILAB1=ITWFL1
      IF(ILAB1.LT.1 .OR. ILAB1.GT.NLAB)ILAB1=1
      ILAB2=ITWFL2
      IF(ILAB2.LT.1 .OR. ILAB2.GT.NLAB)ILAB2=NLAB
      IF(IMAT1.GT.IMAT2)IMAT1=IMAT2
      IF(ILAB1.GT.ILAB2)ILAB1=ILAB2
C
      NPLOTP=0
C
      IF(ITWFPT.EQ.'DEFA')THEN
        IXCNT=0
        IXCNT2=0
        IF(ITWFLM.EQ.'LABO')THEN
          DO1010J=1,NMAT
            DO1020I=1,NLAB
              IXCNT=IXCNT+1
              IF(J.LT.IMAT1 .OR. J.GT.IMAT2)GOTO1020
              IF(I.LT.ILAB1 .OR. I.GT.ILAB2)GOTO1020
              IXCNT2=IXCNT2+1
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=Y1(IXCNT)
              X(NPLOTP)=REAL(IXCNT2)
              D(NPLOTP)=1.0
 1020       CONTINUE
            IF(ITWFGP.GT.0 .AND. J.LT.NMAT)IXCNT2=IXCNT2+ITWFGP
 1010     CONTINUE
          ITAG=1
          NLAST=IXCNT2
        ELSE
          DO1030J=1,NLAB
            DO1040I=1,NMAT
              IF(J.LT.ILAB1 .OR. J.GT.ILAB2)GOTO1040
              IF(I.LT.IMAT1 .OR. I.GT.IMAT2)GOTO1040
              IXCNT=IXCNT+1
              IXCNT2=IXCNT2+1
              IXCNT3=(I-1)*NLAB + J
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=Y1(IXCNT3)
              X(NPLOTP)=REAL(IXCNT2)
              D(NPLOTP)=1.0
 1040       CONTINUE
            IF(ITWFGP.GT.0 .AND. J.LT.NMAT)IXCNT2=IXCNT2+ITWFGP
 1030     CONTINUE
          ITAG=1
          NLAST=IXCNT2
        ENDIF
      ELSE
        IXCNT=0
        IF(ITWFLM.EQ.'LABO')THEN
          DO1110J=1,NMAT
            DO1120I=1,NLAB
              IXCNT=IXCNT+1
              IF(J.LT.IMAT1 .OR. J.GT.IMAT2)GOTO1120
              IF(I.LT.ILAB1 .OR. I.GT.ILAB2)GOTO1120
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=Y1(IXCNT)
              X(NPLOTP)=REAL(J)
              D(NPLOTP)=REAL(I)
 1120       CONTINUE
 1110     CONTINUE
          ITAG=NLAB
          NLAST=NMAT
        ELSE
          DO1130J=1,NLAB
            DO1140I=1,NMAT
              IF(J.LT.ILAB1 .OR. J.GT.ILAB2)GOTO1140
              IF(I.LT.IMAT1 .OR. I.GT.IMAT2)GOTO1140
              IXCNT=IXCNT+1
              NPLOTP=NPLOTP+1
              IXCNT3=(I-1)*NLAB + J
              Y(NPLOTP)=Y1(IXCNT3)
              X(NPLOTP)=REAL(J)
              D(NPLOTP)=REAL(I)
 1140       CONTINUE
 1130     CONTINUE
          ITAG=NMAT
          NLAST=NLAB
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWF2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTWF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NPLOTP,NPLOTV
 9013   FORMAT('IERROR,NPLOTP,NPLOTV = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO9035I=1,NPLOTP
            WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036       FORMAT('I,Y(I),X(I),D(I) = ',I8,2G15.7,F9.2)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTWPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE MANDEL'S ROW-LINEAR OR COLUMN-LINEAR PLOTS FOR
C              JOHN MANDEL'S TWO-WAY TABLE ANALYSIS
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
C     VERSION NUMBER--2015/06
C     ORIGINAL VERSION--JUNE      2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP2
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
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH
      CHARACTER*4 IH2
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
      CHARACTER*12 IX1LAB
      CHARACTER*12 IX2LAB
      CHARACTER*25 IYLAB
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION TAG1(MAXOBV)
      DIMENSION TAG2(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP5(1))
      EQUIVALENCE (GARBAG(IGARB9),TAG1(1))
      EQUIVALENCE (GARBAG(IGAR10),TAG2(1))
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
      ISUBN1='DPTW'
      ISUBN2='PL  '
      ICASPL='TWOW'
      ICASP2='ROW'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ******************************************
C               **  TREAT THE CROSS TABULATE PLOT CASE  **
C               ******************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTWPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ
   52   FORMAT('ICONT,ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************
C               **  STEP 1--                       **
C               **  EXTRACT THE COMMAND            **
C               **  COMMAND SYNTAX IS:             **
C               **  TWO-WAY <ROW/COLUMN> PLOT      **
C               *************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO9000
      IF(ICOM.EQ.'TWO ' .AND. IHARG(1).EQ.'WAY')THEN
        IF(IHARG(2).EQ.'PLOT')THEN
          IFOUND='YES'
          ILASTC=2
        ELSEIF(IHARG(2).EQ.'ROW ' .AND. IHARG(3).EQ.'PLOT')THEN
          IFOUND='YES'
          ILASTC=3
        ELSEIF(IHARG(2).EQ.'COLU' .AND. IHARG(3).EQ.'PLOT')THEN
          ICASP2='COLU'
          IFOUND='YES'
          ILASTC=3
        ELSE
          IFOUND='NO'
          GOTO9000
        ENDIF
      ENDIF
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      INAME='TWO-WAY ROW PLOT'
      IF(ICASP2.EQ.'COLU') INAME='TWO-WAY COLUMN PLOT'
      MINNA=1
      MAXNA=100
      MINN2=5
      IFLAGE=1
      IFLAGM=8
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=3
      MAXNVA=3
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')THEN
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
C     NEED FOLLOWING VARIABLES:
C     1) TWO GROUP-ID VARIABLE
C     2) ONE RESPONSE VARIABLE
C        VARIABLES
C
C
C               ********************************
C               **  STEP 3--                  **
C               **  EXTRACT THE DATA          **
C               ********************************
C
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IYLAB=' '
      IX1LAB=' '
      IX2LAB=' '
      IF(NUMVAR.EQ.1)THEN
        ICOL=1
        IF(IVARLB(ICOLR(1)).EQ.' ')THEN
          IYLAB(1:4)=IVARN1(1)(1:4)
          IYLAB(5:8)=IVARN2(1)(1:4)
        ELSE
          IYLAB(1:25)=IVARLB(ICOLR(1))(1:25)
        ENDIF
        IX1LAB='ROW'
        IX2LAB='COLUMN'
        CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,TAG1,TAG2,NLOCAL,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
      ELSE
        ICOL=1
        IF(IVARLB(ICOLR(1)).EQ.' ')THEN
          IYLAB(1:4)=IVARN1(1)(1:4)
          IYLAB(5:8)=IVARN2(1)(1:4)
        ELSE
          IYLAB(1:25)=IVARLB(ICOLR(1))(1:25)
        ENDIF
        IF(IVARLB(ICOLR(2)).EQ.' ')THEN
          IX1LAB(1:4)=IVARN1(2)(1:4)
          IX1LAB(5:8)=IVARN2(2)(1:4)
        ELSE
          IX1LAB(1:12)=IVARLB(ICOLR(2))(1:12)
        ENDIF
        IF(IVARLB(ICOLR(3)).EQ.' ')THEN
          IX2LAB(1:4)=IVARN1(3)(1:4)
          IX2LAB(5:8)=IVARN2(3)(1:4)
        ELSE
          IX2LAB(1:12)=IVARLB(ICOLR(3))(1:12)
        ENDIF
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,TAG1,TAG2,TEMP1,TEMP1,TEMP1,TEMP1,NLOCAL,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
      ENDIF
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 4--                                        **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS           **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.              **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).   **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).   **
C               ******************************************************
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPTWP2(Y1,TAG1,TAG2,NLOCAL,NUMVAR,ICASPL,ICASP2,
     1            IYLAB,IX1LAB,IX2LAB,
     1            ICAPSW,ICAPTY,IFORSW,
     1            TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,XIDTEM,XIDTE2,
     1            Y,X,D,NPLOTP,NPLOTV,ISUBRO,IBUGG3,IERROR)
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTWPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR,NPLOTV,NPLOTP,NS
 9013   FORMAT('IFOUND,IERROR,NPLOTV,NPLOTP,NS = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IHLEFT,IHLEF2,ICOLL,NLEFT
 9017   FORMAT('IHLEFT,IHLEF2,ICOLL,NLEFT = ',A4,2X,A4,I8,I8)
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
      SUBROUTINE DPTWP2(Y,TAG1,TAG2,N,NUMV2,ICASPL,ICASP2,
     1                  IYLAB,IX1LAB,IX2LAB,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  TEMP1,TEMP2,COLAVE,ROWAVE,SLOPES,XIDTEM,XIDTE2,
     1                  Y2,X2,D2,N2,NPLOTV,ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE MANDEL'S ROW-LINEAR OR COLUMN-LINEAR PLOTS FOR
C              JOHN MANDEL'S TWO-WAY TABLE ANALYSIS
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/06
C     ORIGINAL VERSION--JUNE      2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP2
      CHARACTER*25 IYLAB
      CHARACTER*12 IX1LAB
      CHARACTER*12 IX2LAB
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION COLAVE(*)
      DIMENSION ROWAVE(*)
      DIMENSION SLOPES(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
C
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=45)
      CHARACTER*65 ITITLE
      CHARACTER*65 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*40 ITTEMP
      CHARACTER*20 IVALUE(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      CHARACTER*1  IBASLC
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
      LOGICAL IFLAGA
      LOGICAL IFLAGB
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DSSTO
      DOUBLE PRECISION DSSROW
      DOUBLE PRECISION DSSCOL
      DOUBLE PRECISION DSSERR
      DOUBLE PRECISION DSSSL
      DOUBLE PRECISION DSSER2
      DOUBLE PRECISION DSSRGR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
C
      CHARACTER*4 IRTFMZ
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
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
      ISUBN1='DPTW'
      ISUBN2='P2  '
      IWRITE='OFF'
      IRTFSV=IRTFPS
      IRTFPS=16
      CALL DPCONA(92,IBASLC)
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'RTF')THEN
        WRITE(ICOUT,103)IBASLC,IRTFPS
  103   FORMAT(A1,'fs',I2)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWP2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPTWP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IBUGG3,ISUBRO,ICASPL,ICASP2,N
   71   FORMAT('IBUGG3,ISUBRO,ICASPL,ICASP2,N = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ITWOYA,ITWOFI,ITWOAV,ITWOAN
   72   FORMAT('ITWOYA,ITWOFI,ITWOAV,ITWOAN = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,N
          WRITE(ICOUT,74)I,Y(I),TAG1(I),TAG2(I)
   74     FORMAT('I, Y(I),TAG1(I)TAG2(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C     WRITE FOLLOWING TO DPST1F.DAT
C
C       1. DPST1F.DAT: ROW, HEIGHT, SLOPE, RESSD, STANDARD ERROR OF SLOPE
C       2. DPST2F.DAT: COLUMN, COLUMN AVERAGE
C       3. DPST3F.DAT: ROW, COL, Y(ROW,COL), RES(ROW,COL), PRED(ROW,COL)
C
      IOP='OPEN'
      IFLG11=1
      IFLG21=1
      IFLG31=1
      IFLG41=0
      IFLG51=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLA41,IFLG51,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN TWO-WAY <ROW/COLUMN> PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 5;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (TAG1, TAG2)            **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
C               **  WHICH IS AN ERROR CONDITION FOR A PLOT.         **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DISTIN(TAG1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(TAG2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
C     CHECK FOR MISSING CELLS (PLOT CURRENTLY NOT SUPPORTED FOR
C     CASE WHERE THERE IS MISSING CELLS).  IF REPLICATION IS DETECTED,
C     REPLACE RAW DATA WITH CELL AVERAGES.
C
      IREPL=0
      DO110ISET1=1,NUMSE1
        AHOLD1=XIDTEM(ISET1)
        DO120ISET2=1,NUMSE2
          AHOLD2=XIDTE2(ISET2)
          K=0
          DO130I=1,N
            IF(TAG1(I).EQ.AHOLD1 .AND. TAG2(I).EQ.AHOLD2)THEN
              K=K+1
              GOTO139
            ENDIF
  130     CONTINUE
  139     CONTINUE
          IF(K.EQ.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,142)
  142       FORMAT('      THERE IS NO DATA FOR:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,144)AHOLD1
  144       FORMAT('      GROUP ONE VARIABLE WITH VALUE: ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,146)AHOLD2
  146       FORMAT('      GROUP TWO VARIABLE WITH VALUE: ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,148)
  148       FORMAT('      THIS COMMAND IS NOT SUPPORTED FOR THE CASE ',
     1             'WHERE THERE ARE MISSING CELLS.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSEIF(K.GT.1)THEN
            IREPL=1
          ENDIF
  120   CONTINUE
  110 CONTINUE
C
C     IF REPLICATION DETECTED, REPLACE RAW VALUES WITH MEANS
C
      ICNT=0
      IF(IREPL.EQ.1)THEN
        DO210ISET1=1,NUMSE1
          AHOLD1=XIDTEM(ISET1)
          DO220ISET2=1,NUMSE2
            AHOLD2=XIDTE2(ISET2)
            K=0
            DO230I=1,N
              IF(TAG1(I).EQ.AHOLD1 .AND. TAG2(I).EQ.AHOLD2)THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
  230       CONTINUE
C
            ICNT=ICNT+1
            IF(K.EQ.1)THEN
              TEMP2(ICNT)=TEMP1(1)
            ELSE
              CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
              TEMP2(ICNT)=XMEAN
            ENDIF
            XIDTEM(ICNT)=AHOLD1
            XIDTE2(ICNT)=AHOLD2
C
  220     CONTINUE
  210   CONTINUE
C
        DO310I=1,ICNT
          Y(I)=TEMP2(I)
          TAG1(I)=XIDTEM(I)
          TAG2(I)=XIDTE2(I)
  310   CONTINUE
        N=ICNT
      ENDIF
C
      AN=N
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
C
      IF(IPRINT.EQ.'ON')THEN
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
      ENDIF
C
      NCX1=1
      DO906I=12,1,-1
        IF(IX1LAB(I:I).NE.' ')THEN
          NCX1=I
          GOTO908
        ENDIF
  906 CONTINUE
  908 CONTINUE
C
      NCX2=1
      DO916I=12,1,-1
        IF(IX2LAB(I:I).NE.' ')THEN
          NCX2=I
          GOTO918
        ENDIF
  916 CONTINUE
  918 CONTINUE
C
C
C               ****************************************************
C               **  STEP 11--                                     **
C               **  COMPUTE THE SPECIFIED STATISTIC               **
C               **  FOR EACH CROSS-TAB CATEGORY OF THE DATA, AND  **
C               **  THEN FOR THE FULL DATA SET                    **
C               ****************************************************
C
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     FOR "ROW" CASE, PLOT Y(ij) - R(i) VERSUS R(i) FOR ALL j.
C
C     THAT IS, WE PLOT DEVIATION FROM COLUMN AVERAGE VERSUS THE COLUMN AVERAGE
C
      IF(ICASP2.EQ.'ROW')THEN
C
        WRITE(IOUNI1,1001)
 1001   FORMAT(5X,'ROW',11X,'HEIGHT',10X,'SLOPE',10X,'RESSD',4X,
     1         'SD OF SLOPE',4X,'CORRELATION')
        WRITE(IOUNI2,1002)
 1002   FORMAT(2X,'COLUMN',6X,'COLUMN MEAN')
        WRITE(IOUNI3,1003)
 1003   FORMAT(2X,'ROW-ID',2X,'COL-ID',10X,'Y(ij)',7X,'PRED(ij)',8X,
     1         'RES(ij)',1X,'STAND. RES(ij)')
C
        ITITL9=' '
        NCTIT9=0
        ITITLE='Parameters of Row-Linear Fit for '
        WRITE(ITITLE(34:58),'(A25)')IYLAB
        NCTITL=58
C
        NUMLIN=2
        NUMCOL=6
        ITITL2(1,1)=' '
        ITITL2(2,1)=IX1LAB(1:NCX1)
        NCTIT2(1,1)=0
        NCTIT2(2,1)=NCX1
        ITITL2(1,2)=' '
        ITITL2(2,2)='Height'
        NCTIT2(1,2)=0
        NCTIT2(2,2)=6
        ITITL2(1,3)=' '
        ITITL2(2,3)='Slope'
        NCTIT2(1,3)=0
        NCTIT2(2,3)=5
        ITITL2(1,4)=' '
        ITITL2(2,4)='RESSD'
        NCTIT2(1,4)=0
        NCTIT2(2,4)=5
        ITITL2(1,5)='Standard Error'
        ITITL2(2,5)='of Slope'
        NCTIT2(1,5)=14
        NCTIT2(2,5)=8
        ITITL2(1,6)='Correlation'
        ITITL2(2,6)='Coefficient'
        NCTIT2(1,6)=11
        NCTIT2(2,6)=11
C
        NMAX=0
        NUMROW=NUMSE1
        IF(NUMROW.GT.MAXROW)NUMROW=NUMSE1
        DO1032II=1,NUMCOL
          VALIGN(II)='b'
          ALIGN(II)='r'
          NTOT(II)=15
          IF(II.EQ.1)NTOT(II)=10
          NMAX=NMAX+NTOT(II)
          IDIGIT(II)=NUMDIG
          ITYPCO(II)='NUME'
 1032   CONTINUE
        IDIGIT(1)=0
        IDIGIT(6)=4
        IF(ITWOLA.EQ.'VALU' .AND. ITWODE.GT.0)THEN
          IDIGIT(1)=ITWODE
        ENDIF
        DO1033II=1,MAXROW
        DO1035JJ=1,NUMCOL
          NCVALU(II,JJ)=0
          IVALUE(II,JJ)=' '
          NCVALU(II,JJ)=0
          AMAT(II,JJ)=0.0
 1035   CONTINUE
 1033   CONTINUE
C
        IWHTML(1)=125
        IWHTML(2)=150
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IWHTML(6)=150
        IWRTF(1)=1300
        IWRTF(2)=IWRTF(1)+1700
        IWRTF(3)=IWRTF(2)+1700
        IWRTF(4)=IWRTF(3)+1700
        IWRTF(5)=IWRTF(4)+1700
        IWRTF(6)=IWRTF(5)+1500
        IFRST=.TRUE.
        ILAST=.TRUE.
C
C       COMPUTE THE COLUMN AVERAGES
C
        DO1000ISET2=1,NUMSE2
C
          K=0
          DO1005I=1,N
            IF(TAG2(I).EQ.XIDTE2(ISET2))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 1005     CONTINUE
C
          CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
          COLAVE(ISET2)=XMEAN
C
          IF(ITWOLA.EQ.'VALU')THEN
            WRITE(IOUNI2,1008)XIDTE2(ISET2),COLAVE(ISET2)
 1008       FORMAT(2E15.7)
          ELSE
            WRITE(IOUNI2,1007)ISET2,COLAVE(ISET2)
 1007       FORMAT(I8,2X,E15.7)
          ENDIF
C
 1000   CONTINUE
        CALL MINIM(COLAVE,NUMSE2,IWRITE,XMIN,IBUGG3,IERROR)
        CALL MAXIM(COLAVE,NUMSE2,IWRITE,XMAX,IBUGG3,IERROR)
        CALL MEAN(COLAVE,NUMSE2,IWRITE,XGRAND,IBUGG3,IERROR)
C
C       NOW COMPUTE DEVIATIONS FROM COLUMN AVERAGES FOR EACH ROW
C
        J=0
        ATAG1=0.0
        DSUM=0.0D0
        ICNTRW=0
        DO1100ISET1=1,NUMSE1
C
          K=0
          DO1101I=1,N
            IF(TAG1(I).EQ.XIDTEM(ISET1))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 1101     CONTINUE
C
C         NOW COMPUTE ROW AVERAGE
C
          CALL MEAN(TEMP1,K,IWRITE,AVAL,IBUGG3,IERROR)
          ROWAVE(ISET1)=AVAL
C
          ATAG=ATAG+1.0
          IF(ITWOYA.EQ.'RAW')THEN
            DO1103I=1,K
              J=J+1
              X2(J)=COLAVE(I)
              TEMP2(I)=TEMP1(I)
              Y2(J)=TEMP1(I)
              D2(J)=ATAG
 1103       CONTINUE
          ELSE
            DO1105I=1,K
              J=J+1
              X2(J)=COLAVE(I)
              TEMP2(I)=TEMP1(I) - COLAVE(I)
              Y2(J)=TEMP2(I)
              D2(J)=ATAG
 1105       CONTINUE
          ENDIF
          CALL LINFIT(TEMP2,COLAVE,K,
     1                PPA0,PPA1,XRESSD,XRESDF,PPCC,SDPPA0,SDPPA1,CCALBE,
     1                ISUBRO,IBUGG3,IERROR)
          ATAG=ATAG+1.0
          AY1=PPA0 + PPA1*XMIN
          AY2=PPA0 + PPA1*XMAX
          SLOPES(ISET1)=PPA1
C
          DO1108II=1,K
            PREDVA=PPA0 + PPA1*COLAVE(II)
            RESVA=TEMP2(II) - PREDVA
            RESVA2=RESVA/XRESSD
            IF(ITWOLA.EQ.'VALU')THEN
              WRITE(IOUNI3,1119)XIDTEM(ISET1),II,TEMP2(II),PREDVA,
     1                          RESVA,RESVA2
 1119         FORMAT(E15.7,I8,4E15.7)
            ELSE
              WRITE(IOUNI3,1109)ISET1,II,TEMP2(II),PREDVA,
     1                          RESVA,RESVA2
 1109         FORMAT(2I8,4E15.7)
            ENDIF
 1108     CONTINUE
C
          CALL MEAN(TEMP2,K,IWRITE,AVAL,IBUGG3,IERROR)
          ICNTRW=ICNTRW+1
          IF(ISET1.GT.MAXROW)THEN
            IF(ITWOFI.EQ.'ON')THEN
              CALL DPDTA4(ITITL9,NCTIT9,
     1                    ITITLE,NCTITL,ITITL2,NCTIT2,
     1                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,
     1                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                    ICAPSW,ICAPTY,IFRST,ILAST,
     1                    ISUBRO,IBUGG3,IERROR)
            ENDIF
            ICNTRW=1
          ENDIF
          IF(ITWOLA.EQ.'VALU')THEN
            AMAT(ICNTRW,1)=XIDTEM(ISET1)
          ELSE
            AMAT(ICNTRW,1)=REAL(ISET1)
          ENDIF
          AMAT(ICNTRW,2)=AVAL
          AMAT(ICNTRW,3)=PPA1
          AMAT(ICNTRW,4)=XRESSD
          AMAT(ICNTRW,5)=SDPPA1
          AMAT(ICNTRW,6)=PPCC
          DSUM=DSUM + DBLE(XRESSD)**2
C
          IF(ITWOLA.EQ.'VALU')THEN
            WRITE(IOUNI1,1106)XIDTEM(ISET1),AVAL,PPA1,XRESSD,SDPPA1,PPCC
 1106       FORMAT(6E15.7)
          ELSE
            WRITE(IOUNI1,1107)ISET1,AVAL,PPA1,XRESSD,SDPPA1,PPCC
 1107       FORMAT(I8,2X,5E15.7)
          ENDIF
C
          J=J+1
          X2(J)=XMIN
          Y2(J)=AY1
          D2(J)=ATAG
          J=J+1
          X2(J)=XMAX
          Y2(J)=AY2
          D2(J)=ATAG
 1100   CONTINUE
C
        IF(ITWOFI.EQ.'ON')THEN
          CALL DPDTA4(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                ISUBRO,IBUGG3,IERROR)
C
          CALL SD(SLOPES,NUMSE1,IWRITE,SDSLOP,IBUGG3,IERROR)
C
          IRTFMZ=IRTFMD
          IRTFMD='OFF'
          IFLAGA=.TRUE.
          IFLAGB=.FALSE.
          ISIZE=0
          NTOTAL=40
          NBLNK1=0
          NBLNK2=0
          ITYPE=3
          ITTEMP='Standard Deviation of Slopes: '
          NCTEMP=30
          CALL DPDTXT(ITTEMP,NCTEMP,SDSLOP,NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
C
          DTERM1=DSUM/DBLE(NUMSE1-1)
          AVAL=REAL(DSQRT(DTERM1))
          IFLAGA=.FALSE.
          IFLAGB=.TRUE.
          NBLNK2=2
          ITTEMP='Pooled Standard Deviation of Fit: '
          NCTEMP=34
          CALL DPDTXT(ITTEMP,NCTEMP,AVAL,NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
          IRTFMD=IRTFMZ
        ENDIF
C
        ITITL9=' '
        NCTIT9=0
        ITITLE='Column Averages'
        NCTITL=15
C
        NUMLIN=2
        NUMCOL=2
        ITITL2(1,1)=' '
        ITITL2(2,1)=IX2LAB(1:NCX2)
        NCTIT2(1,1)=0
        NCTIT2(2,1)=NCX2
        ITITL2(1,2)='Column'
        ITITL2(2,2)='Average'
        NCTIT2(1,2)=6
        NCTIT2(2,2)=7
C
        NMAX=0
        NUMROW=NUMSE2
        DO1042II=1,NUMCOL
          VALIGN(II)='b'
          ALIGN(II)='r'
          NTOT(II)=15
          IF(II.EQ.1)NTOT(II)=12
          NMAX=NMAX+NTOT(II)
          IDIGIT(II)=NUMDIG
          ITYPCO(II)='NUME'
 1042   CONTINUE
        IDIGIT(1)=0
        IF(ITWOLA.EQ.'VALU' .AND. ITWODE.GT.0)THEN
          IDIGIT(1)=ITWODE
        ENDIF
        DO1043II=1,MAXROW
        DO1045JJ=1,NUMCOL
          NCVALU(II,JJ)=0
          IVALUE(II,JJ)=' '
          NCVALU(II,JJ)=0
          AMAT(II,JJ)=0.0
 1045   CONTINUE
 1043   CONTINUE
C
        IWHTML(1)=125
        IWHTML(2)=150
        IWRTF(1)=1300
        IWRTF(2)=IWRTF(1)+1700
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        DO1051ISET2=1,NUMSE2
          IF(ITWOLA.EQ.'VALU')THEN
            AMAT(ISET2,1)=XIDTE2(ISET2)
          ELSE
            AMAT(ISET2,1)=REAL(ISET2)
          ENDIF
          AMAT(ISET2,2)=COLAVE(ISET2)
 1051   CONTINUE
C
        IF(ITWOAV.EQ.'ON')THEN
          CALL DPDTA4(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                ISUBRO,IBUGG3,IERROR)
C
          CALL MEAN(COLAVE,NUMSE2,IWRITE,GMEAN,IBUGG3,IERROR)
          IRTFMZ=IRTFMD
          IRTFMD='OFF'
          IFLAGA=.TRUE.
          IFLAGB=.TRUE.
          ISIZE=0
          NTOTAL=30
          NBLNK1=0
          NBLNK2=2
          ITYPE=3
          ITTEMP='Mean of Column Means: '
          NCTEMP=22
          CALL DPDTXT(ITTEMP,NCTEMP,GMEAN,NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
          IRTFMD=IRTFMZ
        ENDIF
C
C       IF REQUESTED, GENERATE AN EXTENDED ANOVA TABLE
C
        IF(ITWOAN.EQ.'ON')THEN
C
C         COMPUTE SSTO
C
          DSUM1=0.0D0
          DTERM1=DBLE(NUMSE1)*DBLE(NUMSE2)*DBLE(XGRAND)**2
C
          DO1610II=1,N
            DSUM1=DSUM1 + DBLE(Y(II))**2
 1610     CONTINUE
          DSSTO=DSUM1 -DTERM1
C
C         COMPUTE SS(ROWS)
C
          DSUM1=0.0D0
          DO1620II=1,NUMSE1
            DSUM1=DSUM1 + DBLE(ROWAVE(II))**2
 1620     CONTINUE
          DSSROW=DBLE(NUMSE2)*DSUM1 - DTERM1
C
C         COMPUTE SS(COLS)
C
          DSUM1=0.0D0
          DO1630II=1,NUMSE2
            DSUM1=DSUM1 + DBLE(COLAVE(II))**2
 1630     CONTINUE
          DSSCOL=DBLE(NUMSE1)*DSUM1 - DTERM1
C
C         COMPUTE ERROR SUM OF SQUARES
C
          DSSERR=DSSTO - DSSROW - DSSCOL
C
          DSUM1=0.0D0
          DO1640II=1,NUMSE1
            DTERM1=DBLE(SLOPES(II) - 1.0)**2
            DSUM1=DSUM1 + DTERM1
 1640     CONTINUE
C
          DSUM2=0.0D0
          DO1650II=1,NUMSE2
            DTERM2=DBLE((COLAVE(II) - XGRAND)**2)
            DSUM2=DSUM2 + DTERM2
 1650     CONTINUE
          DSSSL=DSUM1*DSUM2
          DSSER2=DSSERR-DSSSL
C
C         COMPUTE SUM OF SQUARES FOR COHERENCE.  FORMULAS FROM
C         MANDEL's 1961 JASA PAPER.  THESE FURTHER DECOMPOSE
C         "SLOPE" SUM OF SQUARES INTO "CONCURRENCE" AND
C         "NON-CONCURRENCE".
C
          DSUM1=0.0D0
          DSUM2=0.0D0
          DSUM3=0.0D0
          DO1660II=1,NUMSE1
            DTERM1=DBLE(ROWAVE(II)-XGRAND)
            DSUM1=DSUM1 + DTERM1*DBLE(SLOPES(II))
            DSUM2=DSUM2 + DTERM1**2
 1660     CONTINUE
          DO1670JJ=1,NUMSE2
            DTERM1=DBLE(COLAVE(JJ)-XGRAND)
            DSUM3=DSUM3 + DTERM1**2
 1670     CONTINUE
          DSSRGR=(DSUM1**2/DSUM2)*DSUM3
          DSSNCN=DSSSL - DSSRGR
C
          ITITL9=' '
          NCTIT9=0
          ITITLE='ANOVA Table for Row-Linear Fit'
          NCTITL=30
C
          NUMLIN=2
          NUMCOL=4
          ITITL2(1,1)=' '
          ITITL2(2,1)='Source'
          NCTIT2(1,1)=0
          NCTIT2(2,1)=6
          ITITL2(1,2)='Degrees of'
          ITITL2(2,2)='Freedom'
          NCTIT2(1,2)=10
          NCTIT2(2,2)=7
          ITITL2(1,3)='Sum of'
          ITITL2(2,3)='Squares'
          NCTIT2(1,3)=6
          NCTIT2(2,3)=7
          ITITL2(1,4)='Mean'
          ITITL2(2,4)='Square'
          NCTIT2(1,4)=4
          NCTIT2(2,4)=6
          ITITL2(1,5)='F'
          ITITL2(2,5)='Statistic'
          NCTIT2(1,5)=1
          NCTIT2(2,5)=9
          ITITL2(1,6)=' '
          ITITL2(2,6)='F CDF'
          NCTIT2(1,6)=0
          NCTIT2(2,6)=5
C
          NMAX=0
          NUMROW=8
          IWRTF(1)=1900
          DO1742II=1,NUMCOL
            VALIGN(II)='b'
            ALIGN(II)='r'
            NTOT(II)=15
            IF(II.EQ.1)NTOT(II)=20
            NMAX=NMAX+NTOT(II)
            IDIGIT(II)=NUMDIG
            IF(II.GE.3.AND.II.LE.4.AND.ITWOAD.GE.-9)IDIGIT(II)=ITWOAD
            ITYPCO(II)='NUME'
            IWHTML(II)=150
            IF(II.GE.2)IWRTF(II)=IWRTF(II-1)+1700
 1742     CONTINUE
          IDIGIT(1)=0
          IDIGIT(2)=0
          ALIGN(1)='l'
          ITYPCO(1)='ALPH'
          DO1743II=1,MAXROW
          DO1745JJ=1,NUMCOL
            NCVALU(II,JJ)=0
            IVALUE(II,JJ)=' '
            NCVALU(II,JJ)=0
            AMAT(II,JJ)=0.0
 1745     CONTINUE
 1743     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
C
C         LABEL
C
          IVALUE(1,1)='Total'
          NCVALU(1,1)=5
          IVALUE(2,1)='Rows'
          NCVALU(2,1)=4
          IVALUE(3,1)='Columns'
          NCVALU(3,1)=6
          IVALUE(4,1)='Error'
          NCVALU(4,1)=5
          IVALUE(5,1)='  Residuals'
          NCVALU(5,1)=11
          IVALUE(6,1)='  Slopes'
          NCVALU(6,1)=8
          IVALUE(7,1)='    Concurrence'
          NCVALU(7,1)=15
          IVALUE(8,1)='    Non-Concurrence'
          NCVALU(8,1)=19
C
C         DEGREES OF FREEDOM
C
          AMAT(1,2)=REAL(NUMSE1-1) + REAL(NUMSE2-1) + 
     1              REAL((NUMSE1-1)*(NUMSE2-1))
          AMAT(2,2)=REAL(NUMSE1-1)
          AMAT(3,2)=REAL(NUMSE2-1)
          AMAT(4,2)=REAL((NUMSE1-1)*(NUMSE2-1))
          AMAT(5,2)=REAL((NUMSE1-1)*(NUMSE2-1)) - REAL(NUMSE1-1)
          AMAT(6,2)=REAL(NUMSE1-1)
          AMAT(7,2)=1.0
          AMAT(8,2)=REAL(NUMSE1-2)
C
C         SUM OF SQUARES
C
          AMAT(1,3)=REAL(DSSTO)
          AMAT(2,3)=REAL(DSSROW)
          AMAT(3,3)=REAL(DSSCOL)
          AMAT(4,3)=REAL(DSSERR)
          AMAT(5,3)=REAL(DSSER2)
          AMAT(6,3)=REAL(DSSSL)
          AMAT(7,3)=REAL(DSSRGR)
          AMAT(8,3)=REAL(DSSNCN)
C
C         MEAN SQUARE
C
          AMAT(1,4)=AMAT(1,3)/AMAT(1,2)
          AMAT(2,4)=AMAT(2,3)/AMAT(2,2)
          AMAT(3,4)=AMAT(3,3)/AMAT(3,2)
          AMAT(4,4)=AMAT(4,3)/AMAT(4,2)
          AMAT(5,4)=AMAT(5,3)/AMAT(5,2)
          AMAT(6,4)=AMAT(6,3)/AMAT(6,2)
          AMAT(7,4)=AMAT(7,3)/AMAT(7,2)
          AMAT(8,4)=AMAT(8,3)/AMAT(8,2)
C
          CALL DPDTA4(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                ISUBRO,IBUGG3,IERROR)
C
        ENDIF
C
C     FOR "COLUMN" CASE, PLOT Y(ij) - R(i) VERSUS R(i) FOR ALL j.
C
C     THAT IS, WE PLOT DEVIATION FROM ROW AVERAGE VERSUS THE ROW AVERAGE
C
      ELSE
C
        WRITE(IOUNI1,2001)
 2001   FORMAT(2X,'COLUMN',11X,'HEIGHT',10X,'SLOPE',10X,'RESSD',4X,
     1         'SD OF SLOPE',4X,'CORRELATION')
        WRITE(IOUNI2,2002)
 2002   FORMAT(5X,'ROW',9X,'ROW MEAN')
        WRITE(IOUNI3,2003)
 2003   FORMAT(2X,'ROW-ID',2X,'COL-ID',10X,'Y(ij)',7X,'PRED(ij)',8X,
     1         'RES(ij)',1X,'STAND. RES(ij)')
C
        ITITL9=' '
        NCTIT9=0
        ITITLE='Parameters of Column-Linear Fit for '
        WRITE(ITITLE(37:61),'(A25)')IYLAB
        NCTITL=61
C
        NUMLIN=2
        NUMCOL=6
        ITITL2(1,1)=' '
        ITITL2(2,1)=IX2LAB(1:NCX2)
        NCTIT2(1,1)=0
        NCTIT2(2,1)=NCX2
        ITITL2(1,2)=' '
        ITITL2(2,2)='Height'
        NCTIT2(1,2)=0
        NCTIT2(2,2)=6
        ITITL2(1,3)=' '
        ITITL2(2,3)='Slope'
        NCTIT2(1,3)=0
        NCTIT2(2,3)=5
        ITITL2(1,4)=' '
        ITITL2(2,4)='RESSD'
        NCTIT2(1,4)=0
        NCTIT2(2,4)=5
        ITITL2(1,5)='Standard Error'
        ITITL2(2,5)='of Slope'
        NCTIT2(1,5)=14
        NCTIT2(2,5)=8
        ITITL2(1,6)='Correlation'
        ITITL2(2,6)='Coefficient'
        NCTIT2(1,6)=11
        NCTIT2(2,6)=11
C
        NMAX=0
        NUMROW=NUMSE2
        IF(NUMROW.GT.MAXROW)NUMROW=NUMSE2
        DO2032II=1,NUMCOL
          VALIGN(II)='b'
          ALIGN(II)='r'
          NTOT(II)=15
          IF(II.EQ.1)NTOT(II)=12
          NMAX=NMAX+NTOT(II)
          IDIGIT(II)=NUMDIG
          ITYPCO(II)='NUME'
 2032   CONTINUE
        IDIGIT(1)=0
        IDIGIT(6)=4
        IF(ITWOLA.EQ.'VALU' .AND. ITWODE.GT.0)THEN
          IDIGIT(1)=ITWODE
        ENDIF
        DO2033II=1,MAXROW
        DO2035JJ=1,NUMCOL
          NCVALU(II,JJ)=0
          IVALUE(II,JJ)=' '
          NCVALU(II,JJ)=0
          AMAT(II,JJ)=0.0
 2035   CONTINUE
 2033   CONTINUE
C
        IWHTML(1)=125
        IWHTML(2)=150
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IWHTML(6)=150
        IWRTF(1)=1300
        IWRTF(2)=IWRTF(1)+1700
        IWRTF(3)=IWRTF(2)+1700
        IWRTF(4)=IWRTF(3)+1700
        IWRTF(5)=IWRTF(4)+1700
        IWRTF(6)=IWRTF(5)+1700
        IFRST=.TRUE.
        ILAST=.TRUE.
C
C       COMPUTE THE ROW AVERAGES
C
        DO2000ISET1=1,NUMSE1
C
          K=0
          DO2005I=1,N
            IF(TAG1(I).EQ.XIDTEM(ISET1))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 2005     CONTINUE
C
          CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
          COLAVE(ISET1)=XMEAN
C
          IF(ITWOLA.EQ.'VALU')THEN
            WRITE(IOUNI2,2008)XIDTEM(ISET1),COLAVE(ISET1)
 2008       FORMAT(2E15.7)
          ELSE
            WRITE(IOUNI2,2007)ISET1,COLAVE(ISET1)
 2007       FORMAT(I8,2X,E15.7)
        ENDIF
C
 2000   CONTINUE
        CALL MINIM(COLAVE,NUMSE1,IWRITE,XMIN,IBUGG3,IERROR)
        CALL MAXIM(COLAVE,NUMSE1,IWRITE,XMAX,IBUGG3,IERROR)
C
C       NOW COMPUTE DEVIATIONS FROM ROW AVERAGES FOR EACH COLUMN
C
        J=0
        ATAG1=0.0
        DSUM=0.0D0
        ICNTRW=0
        DO2100ISET2=1,NUMSE2
C
          K=0
          DO2101I=1,N
            IF(TAG2(I).EQ.XIDTE2(ISET2))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 2101     CONTINUE
C
C         NOW COMPUTE COLUMN AVERAGE
C
          CALL MEAN(TEMP1,K,IWRITE,AVAL,IBUGG3,IERROR)
          ROWAVE(ISET2)=AVAL
C
          ATAG=ATAG+1.0
          IF(ITWOYA.EQ.'RAW')THEN
            DO2103I=1,K
              J=J+1
              X2(J)=COLAVE(I)
              TEMP2(I)=TEMP1(I)
              Y2(J)=TEMP1(I)
              D2(J)=ATAG
 2103       CONTINUE
          ELSE
            DO2105I=1,K
              J=J+1
              X2(J)=COLAVE(I)
              TEMP2(I)=TEMP1(I) - COLAVE(I)
              Y2(J)=TEMP2(I)
              D2(J)=ATAG
 2105       CONTINUE
          ENDIF
          CALL LINFIT(TEMP2,COLAVE,K,
     1                PPA0,PPA1,XRESSD,XRESDF,PPCC,SDPPA0,SDPPA1,CCALBE,
     1                ISUBRO,IBUGG3,IERROR)
          ATAG=ATAG+1.0
          AY1=PPA0 + PPA1*XMIN
          AY2=PPA0 + PPA1*XMAX
          SLOPES(ISET2)=PPA1
C
          DO2108II=1,K
            PREDVA=PPA0 + PPA1*COLAVE(II)
            RESVA=TEMP2(II) - PREDVA
            RESVA2=RESVA2/XRESSD
            IF(ITWOLA.EQ.'VALU')THEN
              WRITE(IOUNI3,2119)XIDTE2(ISET2),II,TEMP2(II),PREDVA,
     1                          RESVA,RESVA2
 2119         FORMAT(E15.7,I8,4E15.7)
            ELSE
              WRITE(IOUNI3,2109)ISET2,II,TEMP2(II),PREDVA,
     1                          RESVA,RESVA2
 2109         FORMAT(2I8,4E15.7)
            ENDIF
 2108     CONTINUE
C
          CALL MEAN(TEMP2,K,IWRITE,AVAL,IBUGG3,IERROR)
          ICNTRW=ICNTRW+1
          IF(ISET2.GT.MAXROW)THEN
            IF(ITWOFI.EQ.'ON')THEN
              CALL DPDTA4(ITITL9,NCTIT9,
     1                    ITITLE,NCTITL,ITITL2,NCTIT2,
     1                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,
     1                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                    ICAPSW,ICAPTY,IFRST,ILAST,
     1                    ISUBRO,IBUGG3,IERROR)
            ENDIF
            ICNTRW=ICNTRW+1
          ENDIF
          IF(ITWOLA.EQ.'VALU')THEN
            AMAT(ICNTRW,1)=XIDTE2(ISET2)
          ELSE
            AMAT(ICNTRW,1)=REAL(ISET2)
          ENDIF
          AMAT(ICNTRW,2)=AVAL
          AMAT(ICNTRW,3)=PPA1
          AMAT(ICNTRW,4)=XRESSD
          AMAT(ICNTRW,5)=SDPPA1
          AMAT(ICNTRW,6)=PPCC
          DSUM=DSUM + DBLE(XRESSD)**2
C
          IF(ITWOLA.EQ.'VALU')THEN
            WRITE(IOUNI1,2106)XIDTE2(ISET2),AVAL,PPA1,XRESSD,SDPPA1,PPCC
 2106       FORMAT(6E15.7)
          ELSE
            WRITE(IOUNI1,2107)ISET2,AVAL,PPA1,XRESSD,SDPPA1,PPCC
 2107       FORMAT(I8,2X,5E15.7)
          ENDIF
C
          J=J+1
          X2(J)=XMIN
          Y2(J)=AY1
          D2(J)=ATAG
          J=J+1
          X2(J)=XMAX
          Y2(J)=AY2
          D2(J)=ATAG
 2100   CONTINUE
C
        IF(ITWOFI.EQ.'ON')THEN
          CALL DPDTA4(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                ISUBRO,IBUGG3,IERROR)
C
          CALL SD(SLOPES,NUMSE2,IWRITE,SDSLOP,IBUGG3,IERROR)
C
          IRTFMZ=IRTFMD
          IRTFMD='OFF'
          IFLAGA=.TRUE.
          IFLAGB=.FALSE.
          ISIZE=0
          NTOTAL=40
          NBLNK1=0
          NBLNK2=0
          ITYPE=2
          ITTEMP='Standard Deviation of Slopes: '
          NCTEMP=30
          CALL DPDTXT(ITTEMP,NCTEMP,SDSLOP,NUMDIG,NTOTAL,
     1              NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1              ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
C
          DTERM1=DSUM/DBLE(NUMSE2-1)
          AVAL=REAL(DSQRT(DTERM1))
          IFLAGA=.FALSE.
          IFLAGB=.TRUE.
          NBLNK1=0
          NBLNK2=2
          ITYPE=2
          ITTEMP='Pooled Standard Deviation of Fit: '
          NCTEMP=34
          CALL DPDTXT(ITTEMP,NCTEMP,AVAL,NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
          IRTFMD=IRTFMZ
        ENDIF
C
        ITITL9=' '
        NCTIT9=0
        ITITLE='Row Averages'
        NCTITL=12
C
        NUMLIN=2
        NUMCOL=2
        ITITL2(1,1)=' '
        ITITL2(2,1)=IX1LAB(1:NCX1)
        NCTIT2(1,1)=0
        NCTIT2(2,1)=NCX1
        ITITL2(1,2)='Row'
        ITITL2(2,2)='Average'
        NCTIT2(1,2)=3
        NCTIT2(2,2)=7
C
        NMAX=0
        NUMROW=NUMSE1
        DO2042II=1,NUMCOL
          VALIGN(II)='b'
          ALIGN(II)='r'
          NTOT(II)=15
          IF(II.EQ.1)NTOT(II)=12
          NMAX=NMAX+NTOT(II)
          IDIGIT(II)=NUMDIG
          ITYPCO(II)='NUME'
 2042   CONTINUE
        IDIGIT(1)=0
        IF(ITWOLA.EQ.'VALU' .AND. ITWODE.GT.0)THEN
          IDIGIT(1)=ITWODE
        ENDIF
        DO2043II=1,MAXROW
        DO2045JJ=1,NUMCOL
          NCVALU(II,JJ)=0
          IVALUE(II,JJ)=' '
          NCVALU(II,JJ)=0
          AMAT(II,JJ)=0.0
 2045   CONTINUE
 2043   CONTINUE
C
        IWHTML(1)=125
        IWHTML(2)=150
        IWRTF(1)=1300
        IWRTF(2)=IWRTF(1)+1700
        IFRST=.TRUE.
        ILAST=.TRUE.
C
        DO2051ISET1=1,NUMSE1
          IF(ITWOLA.EQ.'VALU')THEN
            AMAT(ISET1,1)=XIDTEM(ISET1)
          ELSE
            AMAT(ISET1,1)=REAL(ISET1)
          ENDIF
          AMAT(ISET1,2)=COLAVE(ISET1)
 2051   CONTINUE
C
        IF(ITWOAV.EQ.'ON')THEN
          CALL DPDTA4(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                ISUBRO,IBUGG3,IERROR)
C
          CALL MEAN(COLAVE,NUMSE1,IWRITE,GMEAN,IBUGG3,IERROR)
          IRTFMZ=IRTFMD
          IRTFMD='OFF'
          IFLAGA=.TRUE.
          IFLAGB=.TRUE.
          ISIZE=0
          NTOTAL=30
          NBLNK1=0
          NBLNK2=2
          ITYPE=2
          ITTEMP='Mean of Row Means: '
          NCTEMP=19
          CALL DPDTXT(ITTEMP,NCTEMP,GMEAN,NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
          IRTFMD=IRTFMZ
        ENDIF
C
C       IF REQUESTED, GENERATE AN EXTENDED ANOVA TABLE
C
        IF(ITWOAN.EQ.'ON')THEN
C
C         COMPUTE SSTO
C
          DSUM1=0.0D0
          DTERM1=DBLE(NUMSE1)*DBLE(NUMSE2)*DBLE(XGRAND)**2
C
          DO2610II=1,N
            DSUM1=DSUM1 + DBLE(Y(II))**2
 2610     CONTINUE
          DSSTO=DSUM1 -DTERM1
C
C         COMPUTE SS(ROWS)
C
          DSUM1=0.0D0
          DO2620II=1,NUMSE1
            DSUM1=DSUM1 + DBLE(ROWAVE(II))**2
 2620     CONTINUE
          DSSROW=DBLE(NUMSE2)*DSUM1 - DTERM1
C
C         COMPUTE SS(COLS)
C
          DSUM1=0.0D0
          DO2630II=1,NUMSE2
            DSUM1=DSUM1 + DBLE(COLAVE(II))**2
 2630     CONTINUE
          DSSCOL=DBLE(NUMSE1)*DSUM1 - DTERM1
C
          DSSERR=DSSTO - DSSROW - DSSCOL
C
          DSUM1=0.0D0
          DO2640II=1,NUMSE2
            DTERM1=DBLE(SLOPES(II) - 1.0)**2
            DSUM1=DSUM1 + DTERM1
 2640     CONTINUE
C
          DSUM2=0.0D0
          DO2650II=1,NUMSE1
            DTERM2=DBLE((COLAVE(II) - XGRAND)**2)
            DSUM2=DSUM2 + DTERM2
 2650     CONTINUE
          DSSSL=DSUM1*DSUM2
          DSSER2=DSSERR-DSSSL
C
C         COMPUTE SUM OF SQUARES FOR COHERENCE.  FORMULAS FROM
C         MANDEL's 1961 JASA PAPER.  THESE FURTHER DECOMPOSE
C         "SLOPE" SUM OF SQUARES INTO "CONCURRENCE" AND
C         "NON-CONCURRENCE".
C
          DSUM1=0.0D0
          DSUM2=0.0D0
          DSUM3=0.0D0
          DO2660II=1,NUMSE1
            DTERM1=DBLE(ROWAVE(II)-XGRAND)
            DSUM1=DSUM1 + DTERM1*DBLE(SLOPES(II))
            DSUM2=DSUM2 + DTERM1**2
 2660     CONTINUE
          DO2670JJ=1,NUMSE2
            DTERM1=DBLE(COLAVE(JJ)-XGRAND)
            DSUM3=DSUM3 + DTERM1**2
 2670     CONTINUE
          DSSRGR=(DSUM1**2/DSUM2)*DSUM3
          DSSNCN=DSSSL - DSSRGR
C
          ITITL9=' '
          NCTIT9=0
          ITITLE='ANOVA Table for Column-Linear Fit'
          NCTITL=33
C
          NUMLIN=2
          NUMCOL=4
          ITITL2(1,1)=' '
          ITITL2(2,1)='Source'
          NCTIT2(1,1)=0
          NCTIT2(2,1)=6
          ITITL2(1,2)='Degrees of'
          ITITL2(2,2)='Freedom'
          NCTIT2(1,2)=10
          NCTIT2(2,2)=7
          ITITL2(1,3)='Sum of'
          ITITL2(2,3)='Squares'
          NCTIT2(1,3)=6
          NCTIT2(2,3)=7
          ITITL2(1,4)='Mean'
          ITITL2(2,4)='Square'
          NCTIT2(1,4)=4
          NCTIT2(2,4)=6
          ITITL2(1,5)='F'
          ITITL2(2,5)='Statistic'
          NCTIT2(1,5)=1
          NCTIT2(2,5)=9
          ITITL2(1,6)=' '
          ITITL2(2,6)='F CDF'
          NCTIT2(1,6)=0
          NCTIT2(2,6)=5
C
          NMAX=0
          NUMROW=8
          IWRTF(1)=1900
          DO2742II=1,NUMCOL
            VALIGN(II)='b'
            ALIGN(II)='r'
            NTOT(II)=15
            IF(II.EQ.1)NTOT(II)=20
            NMAX=NMAX+NTOT(II)
            IDIGIT(II)=NUMDIG
            IF(II.GE.3.AND.II.LE.4.AND.ITWOAD.GE.-9)IDIGIT(II)=ITWOAD
            ITYPCO(II)='NUME'
            IWHTML(II)=150
            IF(II.GE.2)IWRTF(II)=IWRTF(II-1)+1700
 2742     CONTINUE
          IDIGIT(1)=0
          IDIGIT(2)=0
          ALIGN(1)='l'
          ITYPCO(1)='ALPH'
          DO2743II=1,MAXROW
          DO2745JJ=1,NUMCOL
            NCVALU(II,JJ)=0
            IVALUE(II,JJ)=' '
            NCVALU(II,JJ)=0
            AMAT(II,JJ)=0.0
 2745     CONTINUE
 2743     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
C
C         LABEL
C
          IVALUE(1,1)='Total'
          NCVALU(1,1)=5
          IVALUE(2,1)='Rows'
          NCVALU(2,1)=4
          IVALUE(3,1)='Columns'
          NCVALU(3,1)=6
          IVALUE(4,1)='Error'
          NCVALU(4,1)=5
          IVALUE(5,1)='  Residuals'
          NCVALU(5,1)=11
          IVALUE(6,1)='  Slopes'
          NCVALU(6,1)=8
          IVALUE(7,1)='    Concurrence'
          NCVALU(7,1)=15
          IVALUE(8,1)='    Non-Concurrence'
          NCVALU(8,1)=19
C
C         DEGREES OF FREEDOM
C
          AMAT(1,2)=REAL(NUMSE1-1) + REAL(NUMSE2-1) + 
     1              REAL((NUMSE1-1)*(NUMSE2-1))
          AMAT(2,2)=REAL(NUMSE1-1)
          AMAT(3,2)=REAL(NUMSE2-1)
          AMAT(4,2)=REAL((NUMSE1-1)*(NUMSE2-1))
          AMAT(5,2)=REAL((NUMSE1-1)*(NUMSE2-1)) - REAL(NUMSE1-1)
          AMAT(6,2)=REAL(NUMSE1-1)
          AMAT(7,2)=1.0
          AMAT(8,2)=REAL(NUMSE1-2)
C
C         SUM OF SQUARES
C
          AMAT(1,3)=REAL(DSSTO)
          AMAT(2,3)=REAL(DSSROW)
          AMAT(3,3)=REAL(DSSCOL)
          AMAT(4,3)=REAL(DSSERR)
          AMAT(5,3)=REAL(DSSER2)
          AMAT(6,3)=REAL(DSSSL)
          AMAT(7,3)=REAL(DSSRGR)
          AMAT(8,3)=REAL(DSSNCN)
C
C         MEAN SQUARE
C
          AMAT(1,4)=AMAT(1,3)/AMAT(1,2)
          AMAT(2,4)=AMAT(2,3)/AMAT(2,2)
          AMAT(3,4)=AMAT(3,3)/AMAT(3,2)
          AMAT(4,4)=AMAT(4,3)/AMAT(4,2)
          AMAT(5,4)=AMAT(5,3)/AMAT(5,2)
          AMAT(6,4)=AMAT(6,3)/AMAT(6,2)
          AMAT(7,4)=AMAT(7,3)/AMAT(7,2)
          AMAT(8,4)=AMAT(8,3)/AMAT(8,2)
C
          CALL DPDTA4(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                ISUBRO,IBUGG3,IERROR)
C
        ENDIF
C
      ENDIF
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
C
      IRTFPS=IRTFSV
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'RTF')THEN
        WRITE(ICOUT,103)IBASLC,IRTFPS
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLA41,IFLG51,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTWP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NUMSE1,NUMSE2,N2
 9013   FORMAT('IERROR,NUMSE1,NUMSE2,N2 = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,
     1                  IBUGA3,
     1                  IFOUZ2,ISTAR2,ISTOP2,
     1                  ITYPE2,IHOL,IHOL2,INT,FLOAT,IERROR)
C
C     NOTE--THIS SUBROUTINE IS IDENTICAL TO DPTYP3
C           AND HAS BEEN DUPLICATED ONLY FOR MAPPING PURPOSES.
C           DATE--SEPTEMBER 5, 1981.
C
C     PURPOSE--SCAN THE CHARACTER ARRAY IANS(.) BETWEEN
C              COLUMNS ISTAR1 AND ISTOP1
C              FOR THE STRING DEFINED IN STRIN AND ISTRI2.
C     NOTE THAT THE STRING DEFINED IN ISTRIN AND ISTRI2
C     MAY BE EXPRESSED IN SEVERAL WAYS--
C          1) EXPLICITELY, E.G., LET    FOR    SUBSET, ETC.
C          2) IMPLICITELY WITH ! REPRESENTING THE FIRST
C             NON-BLANK CHARACTER THAT IS ENCOUNTERED;
C          3) IMPLICITELY WITH ; REPRESENTING ANY STRING
C             (INCLUDING ALL CHARACTERS, EVEN BLANKS));
C          4) IMPLICITELY WITH : REPRESENTING THE FIRST
C            BLANK CHARACTER THAT IS ENCOUNTERED.
C     NOTE--A GIVEN ARGUMENT MAY END UP WITH
C            3 DIFFERENT REPRESENTATIONS--
C            HOLLERITH, INTEGER, AND FLOATING POINT.
C     INPUT  ARGUMENTS--IANS   = A HOLLERITH 1-CHARACTER-PER-WORD
C                                VARIABLE CONTAINING THE INPUT LINE
C                                TO BE EXAMINED.
C                     --IWIDTH = THE (FULL) WIDTH OF THE INPUT LINE
C                                (THAT IS, THE NUMBER OF COLUMNS)
C                     --ISTAR1 = THE FIRST COLUMN FOR WHICH THE
C                                SCAN IS TO BE CARRIED OUT.
C                     --ISTOP1 = THE LAST  COLUMN FOR WHICH THE
C                                SCAN IS TO BE CARRIED OUT.
C                     --ISTRIN = THE HOLLERITH VARIABLE
C                                WHICH CONTAINS CHARACTERS 1 TO 4
C                                OF THE STRING TO BE SEARCHED FOR.
C                                THE DEFINITION OF THE STRING IN ISTRIN MAY
C                                MAY BE DONE EXPLICTELY (BUT IS LIMITED
C                                TO 4 CHARACTERS) OR IMPLICITELY
C                                WHICH IS NOT LIMITED TO 4 CHARACTERS AND IS MOR
C                                IS MORE GENERAL IN
C                                OTHER WAYS ALSO.
C                     --ISTRI2 = THE HOLLERITH VARIABLE
C                                WHICH CONTAINS CHARACTERS 5 TO 8
C                                OF THE STRING TO BE SEARCHED FOR.
C                                THE DEFINITION OF THE STRING IN ISTRIN MAY
C                                MAY BE DONE EXPLICTELY (BUT IS LIMITED
C                                TO 4 CHARACTERS) OR IMPLICITELY
C                                WHICH IS NOT LIMITED TO 4 CHARACTERS AND IS MOR
C                                IS MORE GENERAL IN
C                                OTHER WAYS ALSO.
C                     --INEX   = A HOLLERITH VARIABLE WHICH
C                                WILL CONTAIN ONE OF THE FOLLOWING 4 VALUES--
C                                II, IE, EI, EE THAT STANDS FOR
C                                WHERE I STANDS FOR INCLUSIVE AND
C                                WHERE E STANDS FOR EXCLUSIVE;
C                                INEX SPECIFIES WHETHER THE FIRST OR LAST CHARAC
C                                CHARACTER IS TO BE INCLUDED OR EXCLUDED IN
C                                IN DEFINING ISTAR2 AND ISTOP2.
C     OUTPUT ARGUMENTS--IFOUZ2 = A HOLLERITH VARIABLE
C                                WITH THE VALUE 'YES'
C                                IF THE STRING WAS FOUND;
C                                AND THE VALUE 'NO'
C                                IF THE STRING WAS NOT FOUND.
C                     --ISTAR2 = THE START COLUMN OF THE FOUND STRING
C                     --ISTOP2 = THE STOP COLUMN OF THE FIUND STRING.
C                     --ITYPE2 = A HOLLERITH VARIABLE
C                                WITH THE VALUE 'WORD' IF THE STRING CONTAINS
C                                ANY NON-NUMERIC (EXCLUDING BLANKS) CHARACTER;
C                                AND WITH THE VALUE 'NUMB' IF THE STRING CONTA
C                                ALL NUMERIC VALUES OR DECIMAL POINT OR + OR -
C                                (WITH INTERMITTENT BLANKS IGNORED).
C                     --IHOL   = THE HOLLERITH VARIABLE
C                                CONTAINING THE PACKED (4 CHARACTERS) VERSION
C                                OF CHARACTERS 1 TO 4 OF THE FOUND STRING.
C                     --IHOL2  = THE HOLLERITH VARIABLE
C                                CONTAINING THE PACKED (4 CHARACTERS) VERSION
C                                OF CHARACTERS 5 TO 8 OF THE FOUND STRING.
C                     --INT    = THE INTEGER VARIABLE
C                                CONTAINING THE INTEGER REPRESENTATION
C                                (IF POSSIBLE) OF THE FOUND STRING.
C                     --FLOAT  = THE FLOATING POINT VARIABLE
C                                CONTAINING THE FLOATING POINT REPRESENTATION
C                                (IF POSSIBLE) OF THE FOUND STRING.
C                     --IERROR = A HOLLERITH VARIABLE WITH VALUE
C                                'YES' OR 'NO' INDICATING IF AN
C                                ERROR CONDITION EXISTS.
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
C     ORIGINAL VERSION--FEBRUARY  1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS
      CHARACTER*4 ISTRIN
      CHARACTER*4 ISTRI2
      CHARACTER*4 INEX
      CHARACTER*4 IBUGA3
      CHARACTER*4 IFOUZ2
      CHARACTER*4 ITYPE2
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IERROR
C
      CHARACTER*4 IBUG1
      CHARACTER*4 IBUG2
      CHARACTER*4 ITEMP
      CHARACTER*4 IFLUNK
      CHARACTER*4 ISTRI3
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
C
      DIMENSION ISTRI3(20)
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
      ISUBN1='DPTY'
      ISUBN2='P3  '
C
      IERROR='NO'
C
      I2=0
      IPJM1=0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTY3B--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ISTAR1,ISTOP1
   53 FORMAT('ISTAR1,ISTOP1 = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)ISTRIN,ISTRI2
   54 FORMAT('ISTRIN,ISTRI2 = ',A4,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      NUMASC=4
C
      IBUG1='OFF'
      IBUG2='OFF'
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  INITIALIZE THE OUTPUT PARAMETERS AND VARIABLES  **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUG1.EQ.'OFF')GOTO150
      WRITE(ICOUT,101)
  101 FORMAT('AT THE BEGINNING OF DPTY3B--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,102)IWIDTH
  102 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,103)(IANS(I),I=1,IWIDTH)
  103 FORMAT('IANS(.) = ',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,104)ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX
  104 FORMAT('ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX = ',I8,I8,A4,A4,A4)
      CALL DPWRST('XXX','BUG ')
  150 CONTINUE
      IFOUZ2='NO'
      ISTAR2=-1
      ISTOP2=-1
      ITYPE2='9999'
      IHOL ='9999'
      IHOL2='9999'
      INT = -999999
      FLOAT=-999999.0
C
C               ************************************************************
C               **  STEP 2--                                              **
C               **  DECOMPOSE THE INPUT SEARCH STRING INTO A1 CHARACTERS  **
C               ************************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IMAX=2*NUMASC
      DO300I=1,IMAX
      I2=I
      J=I
      IF(I.GT.NUMASC)J=I-NUMASC
      ISTAR3=NUMBPC*(J-1)
      ISTAR3=IABS(ISTAR3)
      ITEMP='    '
      IF(I.LE.NUMASC)CALL DPCHEX(ISTAR3,NUMBPC,ISTRIN,0,NUMBPC,ITEMP)
      IF(I.GT.NUMASC)CALL DPCHEX(ISTAR3,NUMBPC,ISTRI2,0,NUMBPC,ITEMP)
      IF(ITEMP.EQ.'    ')GOTO350
      ISTRI3(I)=ITEMP
  300 CONTINUE
      ILEN2=I2
      GOTO390
  350 CONTINUE
      ILEN2=I2-1
  390 CONTINUE
C
      IF(IBUG2.EQ.'OFF')GOTO399
      WRITE(ICOUT,391)
  391 FORMAT('IN THE MIDDLE OF DPTY3B (AFTER STEP 2)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,392)ILEN2
  392 FORMAT('ILEN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,393)(ISTRI3(I),I=1,ILEN2)
  393 FORMAT('ISTRI3(.) = ',6A1)
      CALL DPWRST('XXX','BUG ')
  399 CONTINUE
C
C               ****************************************************************
C               **  STEP 3--
C               **  DISTINGUISH BETWEEN THE 3 TYPES OF POSSIBLE SEARCH STRINGS--
C               **  1) AN EXPLICITELY-DEFINED STRING; E.G.,
C               **     LET     FOR     SUBSET     =     5.3     -2.6666666
C               **     (AS IN COMMANDS, KEY WORDS, AND NUMBERS);
C               **  2) A STRING STARTING WITH THE FIRST NON-BLANK CHARACTER
C               **     AND ENDING WITH SOME SPECIFIED CHARACTER; E.G., XXXXX(
C               **     (AS IN THE VARIABLE NAME OF A SUBSCRIPTED VARIABLE,
C               **     OR THE ARGUMENT (I. E., THE SUBSCRIPT) IN A SUBSCRIPTED
C               **     VARIABLE);
C               **  3) A STRING STARTING WITH THE FIRST NON-BLANK CHARACTER
C               **     AND ENDING WITH THE FIRST SUBSEQUENT BLANK CHARCTER (EXCL
C               **     E.G., XXXX
C               **     (AS IN SOME UNSPECIFIED PARAMETER OR VARIABLE NAME).
C               ****************************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ICASE=1
      IF(ISTRI3(1).NE.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).NE.':')
     1ICASE=2
      IF(ISTRI3(1).EQ.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).NE.':')
     1ICASE=3
      IF(ISTRI3(1).EQ.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).EQ.':')
     1ICASE=4
      IF(ILEN2.EQ.1.OR.ILEN2.EQ.2)ICASE=1
C
      IF(IBUG2.EQ.'OFF')GOTO398
      WRITE(ICOUT,395)
  395 FORMAT('AFTER STEP 3 OF DPTY3B--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,396)ICASE
  396 FORMAT('ICASE = ',I8)
      CALL DPWRST('XXX','BUG ')
  398 CONTINUE
C
C               *********************************************************
C               **  STEP 4--                                           **
C               **  DETERMINE IF THE DESIRED SEARCH STRING IS PRESENT  **
C               *********************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(ICASE.EQ.1)GOTO400
      IF(ICASE.EQ.2)GOTO500
      IF(ICASE.EQ.3)GOTO600
      IF(ICASE.EQ.4)GOTO700
C
  400 CONTINUE
      DO410I=ISTAR1,ISTOP1
      I2=I
      IF(IANS(I).EQ.ISTRI3(1))GOTO420
      GOTO410
  420 CONTINUE
      DO430J=1,ILEN2
      IPJM1=J+I-1
      IF(IPJM1.GT.ISTOP1)GOTO410
      IF(IANS(IPJM1).EQ.ISTRI3(J))GOTO430
      GOTO410
  430 CONTINUE
      IFOUZ2='YES'
      IF(INEX.EQ.'II')ISTAR2=I2
      IF(INEX.EQ.'IE')ISTAR2=I2
      IF(INEX.EQ.'EI')ISTAR2=I2+1
      IF(INEX.EQ.'EE')ISTAR2=I2+1
      IF(INEX.EQ.'II')ISTOP2=IPJM1
      IF(INEX.EQ.'IE')ISTOP2=IPJM1-1
      IF(INEX.EQ.'EI')ISTOP2=IPJM1
      IF(INEX.EQ.'EE')ISTOP2=IPJM1-1
      IF(ISTAR2.LE.ISTOP2)GOTO990
      GOTO900
  410 CONTINUE
      IFOUZ2='NO'
      GOTO9000
C
  500 CONTINUE
      DO510I=ISTAR1,ISTOP1
      I2=I
      IF(IANS(I).EQ.ISTRI3(1))GOTO520
  510 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  520 CONTINUE
      IMIN=I2
      DO530I=IMIN,ISTOP1
      I2=I
      IF(IANS(I).EQ.ISTRI3(ILEN2))GOTO540
  530 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  540 CONTINUE
      IFOUZ2='YES'
      IF(INEX.EQ.'II')ISTAR2=IMIN
      IF(INEX.EQ.'IE')ISTAR2=IMIN
      IF(INEX.EQ.'EI')ISTAR2=IMIN+1
      IF(INEX.EQ.'EE')ISTAR2=IMIN+1
      IF(INEX.EQ.'II')ISTOP2=I2
      IF(INEX.EQ.'IE')ISTOP2=I2-1
      IF(INEX.EQ.'EI')ISTOP2=I2
      IF(INEX.EQ.'EE')ISTOP2=I2-1
      IF(ISTAR2.LE.ISTOP2)GOTO990
      GOTO900
C
  600 CONTINUE
      DO610I=ISTAR1,ISTOP1
      I2=I
      IF(IANS(I).NE.' ')GOTO620
  610 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  620 CONTINUE
      IMIN=I2
      DO630I=IMIN,ISTOP1
      I2=I
      IF(IANS(I).EQ.ISTRI3(ILEN2))GOTO640
  630 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  640 CONTINUE
      IFOUZ2='YES'
      IF(INEX.EQ.'II')ISTAR2=IMIN
      IF(INEX.EQ.'IE')ISTAR2=IMIN
      IF(INEX.EQ.'EI')ISTAR2=IMIN+1
      IF(INEX.EQ.'EE')ISTAR2=IMIN+1
      IF(INEX.EQ.'II')ISTOP2=I2
      IF(INEX.EQ.'IE')ISTOP2=I2-1
      IF(INEX.EQ.'EI')ISTOP2=I2
      IF(INEX.EQ.'EE')ISTOP2=I2-1
      IF(ISTAR2.LE.ISTOP2)GOTO990
      GOTO900
C
  700 CONTINUE
      DO710I=ISTAR1,ISTOP1
      I2=I
      IF(IANS(I).NE.' ')GOTO720
  710 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  720 CONTINUE
      IMIN=I2
      DO730I=IMIN,ISTOP1
      I2=I
      IF(IANS(I).EQ.' ')GOTO740
  730 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  740 CONTINUE
      IFOUZ2='YES'
      IF(INEX.EQ.'II')ISTAR2=IMIN
      IF(INEX.EQ.'IE')ISTAR2=IMIN
      IF(INEX.EQ.'EI')ISTAR2=IMIN+1
      IF(INEX.EQ.'EE')ISTAR2=IMIN+1
      IF(INEX.EQ.'II')ISTOP2=I2
      IF(INEX.EQ.'IE')ISTOP2=I2-1
      IF(INEX.EQ.'EI')ISTOP2=I2
      IF(INEX.EQ.'EE')ISTOP2=I2-1
      IF(ISTAR2.LE.ISTOP2)GOTO990
      GOTO900
C
  900 CONTINUE
C
C     NOTE--THE FOLLOWING SECTION HAS BEEN 'BUGGED' OUT
C           TO CIRCUMVENT A PROBLEM WITH Y=(...
C           WHILE IT STILL LOOKED FOR A VARIABLE NAME
C           BETWEEN THE = AND THE (     .
C     CAUTION--WHEN IBUGA3 = 'OFF', AS IT USUALLY IS,
C              IERROR CAN NEVER BE 'YES'
C              UPON RETURN FROM DPTY3B:
C              BUT WHEN IBUGA3 = 'ON' (AS IN ERROR TRACING)
C              IERROR MAY = 'YES' WHICH MAY CHANGE THE
C              LOGIC PATH BACK IN DPTYP2.
C
      IF(IBUGA3.EQ.'OFF')GOTO9000
      WRITE(ICOUT,921)
  921 FORMAT('***** INTERNAL ERROR IN DPTY3B SUBROUTINE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,922)
  922 FORMAT('ISTAR2 GREATER THAN ISTOP2')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,923)ISTAR2,ISTOP2
  923 FORMAT('ISTAR2, ISTOP2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,924)ICASE
  924 FORMAT('ICASE = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,925)IWIDTH
  925 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,926)(IANS(I),I=1,IWIDTH)
  926 FORMAT('IANS(.) = ',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,927)ISTAR1,ISTOP1
  927 FORMAT('ISTAR1, ISTOP1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,928)ILEN2
  928 FORMAT('ILEN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,929)(ISTRI3(I),I=1,ILEN2)
  929 FORMAT('ISTRI3(.) = ',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,930)ISTRIN,ISTRI2
  930 FORMAT('ISTRIN,ISTRI2 = ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,931)INEX
  931 FORMAT('INEX = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  990 CONTINUE
C
C               ********************************************************
C               **  STEP 5--                                          **
C               **  CONVERT THE STRING INTO 2 HOLLERITH A4 WORDS.     **
C               **  IF MORE THAN 8 CHARACTERS, CONVERT ONLY           **
C               **  THE FIRST 8 CHARACTERS.                           **
C               **  OUTPUT THESE HOLLERITH WORDS AS IHOL AND IHOL2.   **
C               ********************************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IHOL ='    '
      IHOL2='    '
      IMAX=2*NUMASC
      J=0
      DO1000I=ISTAR2,ISTOP2
      J=J+1
      K=J
      IF(J.GT.NUMASC)K=J-NUMASC
      ISTAR3=NUMBPC*(K-1)
      ISTAR3=IABS(ISTAR3)
      IF(J.LE.NUMASC)CALL DPCHEX(0,NUMBPC,IANS(I),ISTAR3,NUMBPC,IHOL)
      IF(J.GT.NUMASC)CALL DPCHEX(0,NUMBPC,IANS(I),ISTAR3,NUMBPC,IHOL2)
      IF(J.GE.IMAX)GOTO1050
 1000 CONTINUE
 1050 CONTINUE
C
C               ****************************************************************
C               **  STEP 6--
C               **  CONVERT (IF POSSIBLE) THE STRING INTO AN INTEGER ARGUMENT.
C               **  OUTPUT  THIS INTEGER VALUE IN INT.
C               ****************************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IFLUNK='NO'
      ITYPE2='NUMB'
      IDIG=0
      ISIGN=0
      IDECPT=0
      ISUM=0
      DO2700I=ISTAR2,ISTOP2
      IREV=ISTOP2-(I-ISTAR2)
      IF(IANS(IREV).EQ.' ')GOTO2700
      IF(IANS(IREV).EQ.'0')GOTO2710
      IF(IANS(IREV).EQ.'1')GOTO2711
      IF(IANS(IREV).EQ.'2')GOTO2712
      IF(IANS(IREV).EQ.'3')GOTO2713
      IF(IANS(IREV).EQ.'4')GOTO2714
      IF(IANS(IREV).EQ.'5')GOTO2715
      IF(IANS(IREV).EQ.'6')GOTO2716
      IF(IANS(IREV).EQ.'7')GOTO2717
      IF(IANS(IREV).EQ.'8')GOTO2718
      IF(IANS(IREV).EQ.'9')GOTO2719
      IF(IANS(IREV).EQ.'+')GOTO2720
      IF(IANS(IREV).EQ.'-')GOTO2721
      IF(IANS(IREV).EQ.'.')GOTO2722
      IFLUNK='YES'
      GOTO2800
 2710 ITERM=0
      GOTO2725
 2711 ITERM=1
      GOTO2725
 2712 ITERM=2
      GOTO2725
 2713 ITERM=3
      GOTO2725
 2714 ITERM=4
      GOTO2725
 2715 ITERM=5
      GOTO2725
 2716 ITERM=6
      GOTO2725
 2717 ITERM=7
      GOTO2725
 2718 ITERM=8
      GOTO2725
 2719 ITERM=9
      GOTO2725
 2720 ISIGN=ISIGN+1
      GOTO2700
 2721 ISIGN=ISIGN+1
      ISUM=-ISUM
      GOTO2700
 2722 IDECPT=IDECPT+1
      IF(IDECPT.EQ.1.AND.IDIG.EQ.0)GOTO2700
      GOTO2800
 2725 IDIG=IDIG+1
      TERM2=10.0**(IDIG-1)
      ITERM2=TERM2 + 0.01
      ISUM=ISUM+ITERM*ITERM2
 2700 CONTINUE
      IF(IDIG.LE.0)GOTO2800
      IF(ISIGN.GE.2)GOTO2800
      INT=ISUM
 2800 CONTINUE
      IF(IFLUNK.EQ.'YES')ITYPE2='WORD'
 2100 CONTINUE
 2999 CONTINUE
C
C               ********************************************************
C               **  STEP 7--                                          **
C               **  CONVERT (IF POSSIBLE) THE STRING INTO A FLOATING  **
C               **  POINT ARGUMENT.                                   **
C               **  OUTPUT THIS FLOATING POINT VALUE IN FLOAT.        **
C               ********************************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      AMIN=-1000000.
      AMAX=+1000000.
      IFLUNK='NO'
      ITYPE2='NUMB'
      FLOAT=-1.0
C
      ILOC=0
      IDECPT=0
      DO3060I=ISTAR2,ISTOP2
      IF(IANS(I).EQ.'.')ILOC=I
      IF(IANS(I).EQ.'.')IDECPT=IDECPT+1
 3060 CONTINUE
      IF(IDECPT.GE.2)GOTO3900
      IF(IDECPT.EQ.1)GOTO3150
      DO3100I=ISTAR2,ISTOP2
      IREV=ISTOP2-(I-ISTAR2)
      IF(IANS(IREV).EQ.' ')GOTO3100
      IF(IANS(IREV).EQ.'0')GOTO3110
      IF(IANS(IREV).EQ.'1')GOTO3110
      IF(IANS(IREV).EQ.'2')GOTO3110
      IF(IANS(IREV).EQ.'3')GOTO3110
      IF(IANS(IREV).EQ.'4')GOTO3110
      IF(IANS(IREV).EQ.'5')GOTO3110
      IF(IANS(IREV).EQ.'6')GOTO3110
      IF(IANS(IREV).EQ.'7')GOTO3110
      IF(IANS(IREV).EQ.'8')GOTO3110
      IF(IANS(IREV).EQ.'9')GOTO3110
      IFLUNK='YES'
      IF(IANS(IREV).EQ.'+')GOTO3900
      IF(IANS(IREV).EQ.'-')GOTO3900
      GOTO3900
 3100 CONTINUE
      IFLUNK='YES'
      GOTO3900
 3110 ILOC=IREV+1
 3150 CONTINUE
      IF(IBUG2.EQ.'ON')WRITE(ICOUT,3111)ILOC,IDECPT
 3111 FORMAT('ILOC = ',I8,'    IDECPT = ',I8)
      IF(IBUG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
C     SECONDLY, COMPUTE THE INTEGER PART OF THE VALUE
C
      SIGN=1.0
      IDIGI=0
      ISIGN=0
      SUMI=0
      ILOCM1=ILOC-1
      IF(ILOCM1.LT.ISTAR2)GOTO3250
      DO3200I=ISTAR2,ILOCM1
      IREV=ILOCM1-(I-ISTAR2)
      IF(IANS(IREV).EQ.' ')GOTO3200
      IF(IANS(IREV).EQ.'0')GOTO3210
      IF(IANS(IREV).EQ.'1')GOTO3211
      IF(IANS(IREV).EQ.'2')GOTO3232
      IF(IANS(IREV).EQ.'3')GOTO3213
      IF(IANS(IREV).EQ.'4')GOTO3214
      IF(IANS(IREV).EQ.'5')GOTO3215
      IF(IANS(IREV).EQ.'6')GOTO3216
      IF(IANS(IREV).EQ.'7')GOTO3217
      IF(IANS(IREV).EQ.'8')GOTO3218
      IF(IANS(IREV).EQ.'9')GOTO3219
      IF(IANS(IREV).EQ.'+')GOTO3220
      IF(IANS(IREV).EQ.'-')GOTO3221
      IFLUNK='YES'
      GOTO3900
 3210 ITERM=0
      GOTO3225
 3211 ITERM=1
      GOTO3225
 3232 ITERM=2
      GOTO3225
 3213 ITERM=3
      GOTO3225
 3214 ITERM=4
      GOTO3225
 3215 ITERM=5
      GOTO3225
 3216 ITERM=6
      GOTO3225
 3217 ITERM=7
      GOTO3225
 3218 ITERM=8
      GOTO3225
 3219 ITERM=9
      GOTO3225
 3220 ISIGN=ISIGN+1
      GOTO3200
 3221 ISIGN=ISIGN+1
      SIGN=-SIGN
      GOTO3200
 3225 IDIGI=IDIGI+1
      TERM=ITERM
      IEXP=IDIGI-1
      SUMI=SUMI+TERM*(10.0**IEXP)
 3200 CONTINUE
 3250 CONTINUE
      IF(ISIGN.GE.2)GOTO3900
      IF(IBUG2.EQ.'ON')WRITE(ICOUT,3255)IDIGI,SUMI
 3255 FORMAT('IDIGI = ',I8,'    SUMI = ',F20.10)
      IF(IBUG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
C     THIRDLY, COMPUTE THE DECIMAL PART OF THE VALUE
C
      IDIGD=0
      SUMD=0.0
      ILOCP1=ILOC+1
      IF(ILOCP1.GT.ISTOP2)GOTO3350
      DO3300I=ILOCP1,ISTOP2
      IF(IANS(I).EQ.' ')GOTO3300
      IF(IANS(I).EQ.'0')GOTO3310
      IF(IANS(I).EQ.'1')GOTO3311
      IF(IANS(I).EQ.'2')GOTO3312
      IF(IANS(I).EQ.'3')GOTO3333
      IF(IANS(I).EQ.'4')GOTO3314
      IF(IANS(I).EQ.'5')GOTO3315
      IF(IANS(I).EQ.'6')GOTO3316
      IF(IANS(I).EQ.'7')GOTO3317
      IF(IANS(I).EQ.'8')GOTO3318
      IF(IANS(I).EQ.'9')GOTO3319
      IFLUNK='YES'
      GOTO3900
 3310 ITERM=0
      GOTO3325
 3311 ITERM=1
      GOTO3325
 3312 ITERM=2
      GOTO3325
 3333 ITERM=3
      GOTO3325
 3314 ITERM=4
      GOTO3325
 3315 ITERM=5
      GOTO3325
 3316 ITERM=6
      GOTO3325
 3317 ITERM=7
      GOTO3325
 3318 ITERM=8
      GOTO3325
 3319 ITERM=9
      GOTO3325
 3325 IDIGD=IDIGD+1
      TERM=ITERM
      SUMD=SUMD+TERM/(10.0**IDIGD)
 3300 CONTINUE
 3350 CONTINUE
      IF(IBUG2.EQ.'ON')WRITE(ICOUT,3355)IDIGD,SUMD
 3355 FORMAT('IDIGD = ',I8,'    SUMD = ',F20.10)
      IF(IBUG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IDIGT=IDIGI+IDIGD
      IF(IDIGT.LE.0)GOTO3900
      FLOAT=SUMI+SUMD
      IF(SIGN.LT.0.0)FLOAT=-FLOAT
      IF(AMIN.LE.FLOAT.AND.FLOAT.LE.AMAX)GOTO3000
      GOTO3900
C
 3900 CONTINUE
      IF(IFLUNK.EQ.'YES')ITYPE2='WORD'
 3000 CONTINUE
 3999 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9900
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9001)
 9001 FORMAT('AT THE END OF DPTY3B--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9002)IFOUZ2,ISTAR2,ISTOP2
 9002 FORMAT('IFOUZ2, ISTAR2, ISTOP2 = ',A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9003)ITYPE2,IHOL,IHOL2,INT,FLOAT,IERROR
 9003 FORMAT('ITYPE2,IHOL,IHOL2,INT,FLOAT,IERROR = ',A4,2X,A4,A4,2X,
     1I8,F15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
C
 9900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTYP2(IANS,IWIDTH,IHNAME,IHNAM2,NUMNAM,MAXNAM,IBUGA3,
     1           IUSE,IVALUE,VALUE,IN,
     1           IFOUNZ,IBEGIN,IEND,
     1           ITYPE,IHOL,IHOL2,INT1,FLOAT1,IERRO1,
     1           NUMCL,NUMPL,NUMAOL,ITYW1L,ICAT1L,INLI1L,ITYW2L,
     1           NUMCR,NUMPR,NUMAOR,ITYW1R,ICAT1R,INLI1R,ITYW2R)
C
C     PURPOSE--SCAN THE CHARACTER ARRAY IANS(.)
C              AND EXTRACT INFORMATION
C              REGARDING THE EXISTENCE AND LOACTION
C              OF CERTAIN SUBSTRINGS USED IN THE LET COMMAND.
C     THIS SUBROUTINE (DPTYP2) IS CALLED BY DPLET.
C     OTHER SUBROUINTES NEEDED--DPTYP3
C     MOST GENERAL FORM--LET X(I) = XXX FOR I = A B C
C                      --LET X(I) = XXX SUBSET XX A B
C     INPUT  ARGUMENTS--IANS   = A HOLLERITH 1-CHARACTER-PER-WORD
C                                VARIABLE CONTAINING THE INPUT LINE
C                                TO BE EXAMINED.
C                     --IWIDTH = AN INTEGER VARIABLE CONTAINING
C                                THE (FULL) WIDTH OF THE INPUT LINE
C                                (THAT IS, THE NUMBER OF COLUMNS)
C     OUTPUT ARGUMENTS--IFOUNZ = A HOLLERITH ARRAY
C                                WITH THE VALUE 'YES'
C                                IF THE SUBSTRING WAS FOUND;
C                                AND THE VALUE 'NO'
C                                IF THE SUBSTRING WAS NOT FOUND.
C                     --IBEGIN = AN INTEGER ARRAY WITH
C                                THE START COLUMN OF THE FOUND SUBSTRING
C                     --IEND   = AN INTEGER ARRAY WITH
C                                THE STOP COLUMN OF THE FIUND SUBSTRING.
C                     --ITYPE  = A HOLLERITH ARRAY
C                                WITH THE VALUE 'WORD' IF THE SUBSTRING CONTAINS
C                                ANY NON-NUMERIC (EXCLUDING BLANKS) CHARACTER;
C                                AND WITH THE VALUE 'NUMB' IF THE SUBSTRING CO
C                                ALL NUMERIC VALUES OR DECIMAL POINT OR + OR -
C                                (WITH INTERMITTENT BLANKS IGNORED).
C                     --IHOL   = AN HOLLERITH ARRAY
C                                CONTAINING THE PACKED (FIRST 4 CHARACTERS) VERS
C                                OF THE FOUND SUBSTRING.
C                     --IHOL2  = AN HOLLERITH ARRAY
C                                CONTAINING THE PACKED (NEXT 4 CHARACTERS) VERSI
C                                OF THE FOUND SUBSTRING.
C                     --INT1   = AN INTEGER ARRAY
C                                CONTAINING THE INTEGER REPRESENTATION
C                                (IF POSSIBLE) OF THE FOUND SUBSTRING.
C                     --FLOAT1 = AN FLOATING POINT ARRAY
C                                CONTAINING THE FLOATING POINT REPRESENTATION
C                                (IF POSSIBLE) OF THE FOUND SUBSTRING.
C                     --IERRO1 = AN HOLLERITH ARRAY
C                                WITH THE VALUE 'NO' IF
C                                NO ERROR HAS BEEN ENCOUNTERED,
C                                AND THE VALUE 'YES' IF AN
C                                ERROR HAS BEEN ENCOUNTERED.
C                     --NUMCL  = AN INTEGER VARIABLE CONTAINING THE
C                                NUMBER OF COMPONENTS
C                                ON THE LEFT SIDE
C                                (NOT COUNTING LET OR THE = SIGN).
C                     --NUMPL  = AN INTEGER VARIABLE CONTAINING THE
C                                NUMBER OF PARENTHESES (LEFT + RIGHT)
C                                ON THE LEFT SIDE
C                                (NOT COUNTING LET OR THE = SIGN).
C                     --NUMAOL = AN INTEGER VARIABLE CONTAINING THE
C                                NUMBER OF ARITHMETIC OPERATIONS
C                                ON THE LEFT SIDE
C                                (NOT COUNTING LET OR THE = SIGN).
C                     --ITYW1L = A HOLLERITH VARIABLE CONTAINING THE
C                                TYPE ('WORD' VERSUS 'NUMB')
C                                FOR THE FIRST WORD
C                                (THAT IS, THE VARIABLE
C                                OR PARAMETER NAME)
C                                ON THE LEFT SIDE
C                                (NOT COUNTING LET OR THE = SIGN).
C                     --ITYW2L = A HOLLERITH VARIABLE CONTAINING THE
C                                TYPE ('WORD' VERSUS 'NUMB')
C                                FOR THE SECOND WORD
C                                (THAT IS, THE ARGUMENT)
C                                ON THE LEFT SIDE
C                                (NOT COUNTING LET OR THE = SIGN).
C                     --INLI1L = A HOLLERITH VARIABLE CONTAINING THE
C                                ANSWER ('YES' VERSUS 'NO')
C                                TO THE QUESTION AS TO WHETHER
C                                THE FIRST WORD ON THE LEFT
C                                (THAT IS, THE VARIABLE
C                                OR PARAMETER NAME)
C                                IS ALREADY EXISTENT IN THE
C                                INTERNAL DATAPLOT NAME LIST
C                                (NOT COUNTING LET OR THE = SIGN).
C                     --NUMCR  = AN INTEGER VARIABLE CONTAINING THE
C                                NUMBER OF COMPONENTS
C                                ON THE RIGHT SIDE
C                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
C                     --NUMPR  = AN INTEGER VARIABLE CONTAINING THE
C                                NUMBER OF PARENTHESES (RIGHT + RIGHT)
C                                ON THE RIGHT SIDE
C                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
C                     --NUMAOR = AN INTEGER VARIABLE CONTAINING THE
C                                NUMBER OF ARITHMETIC OPERATIONS
C                                ON THE RIGHT SIDE
C                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
C                     --ITYW1R = A HOLLERITH VARIABLE CONTAINING THE
C                                TYPE ('WORD' VERSUS 'NUMB')
C                                FOR THE FIRST WORD
C                                (THAT IS, THE VARIABLE
C                                OR PARAMETER NAME)
C                                ON THE RIGHT SIDE
C                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
C                     --ITYW2R = A HOLLERITH VARIABLE CONTAINING THE
C                                TYPE ('WORD' VERSUS 'NUMB')
C                                FOR THE SECOND WORD
C                                (THAT IS, THE ARGUMENT)
C                                ON THE RIGHT SIDE
C                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
C                     --INLI1R = A HOLLERITH VARIABLE CONTAINING THE
C                                ANSWER ('YES' VERSUS 'NO')
C                                TO THE QUESTION AS TO WHETHER
C                                THE FIRST WORD ON THE RIGHT
C                                (THAT IS, THE VARIABLE
C                                OR PARAMETER NAME)
C                                IS ALREADY EXISTENT IN THE
C                                INTERNAL DATAPLOT NAME LIST
C                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
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
C     ORIGINAL VERSION--MARCH     1978
C     UPDATED         --JUNE      1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --JUNE      1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --JANUARY   1982.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1983.
C     UPDATED         --DECEMBER  1988.  ELIM. SPUR. ERROR MESS. FOR IFRINGE
C     UPDATED         --JANAURY   1989.  IANS(IENDP) WITH IENDP = 0 (ALAN)
C     UPDATED         --NOVEMBER  1989.  FIX IANS(IENDP=0) (NELSON)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IUSE
      CHARACTER*4 IFOUNZ
      CHARACTER*4 ITYPE
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IERRO1
      CHARACTER*4 ITYW1L
      CHARACTER*4 ICAT1L
      CHARACTER*4 INLI1L
      CHARACTER*4 ITYW2L
      CHARACTER*4 ITYW1R
      CHARACTER*4 ICAT1R
      CHARACTER*4 INLI1R
      CHARACTER*4 ITYW2R
C
      CHARACTER*4 ISTRIN
      CHARACTER*4 ISTRI2
      CHARACTER*4 INEX
      CHARACTER*4 IVARL
      CHARACTER*4 IVARL2
      CHARACTER*4 IVARR
      CHARACTER*4 IVARR2
      CHARACTER*4 IQUAL
      CHARACTER*4 IHSTAT
      CHARACTER*4 IHSTA2
      CHARACTER*4 IHMAN
      CHARACTER*4 IHMAN2
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
C
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IN(*)
C
      DIMENSION IFOUNZ(*)
      DIMENSION IBEGIN(*)
      DIMENSION IEND(*)
      DIMENSION ITYPE(*)
      DIMENSION IHOL(*)
      DIMENSION IHOL2(*)
      DIMENSION INT1(*)
      DIMENSION FLOAT1(*)
      DIMENSION IERRO1(*)
C
      DIMENSION IHMAN(10)
      DIMENSION IHMAN2(10)
      DIMENSION IHSTAT(25)
      DIMENSION IHSTA2(25)
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
      DATA NUMMAN/8/
C
      DATA IHMAN(1),IHMAN2(1)/'SORT','    '/
      DATA IHMAN(2),IHMAN2(2)/'RANK','    '/
      DATA IHMAN(3),IHMAN2(3)/'CODE','    '/
      DATA IHMAN(4),IHMAN2(4)/'DIST','INCT'/
      DATA IHMAN(5),IHMAN2(5)/'SEQU','ENTI'/
      DATA IHMAN(6),IHMAN2(6)/'CUMU','LATI'/
      DATA IHMAN(7),IHMAN2(7)/'CUMU','LATI'/
      DATA IHMAN(8),IHMAN2(8)/'CUMU','LATI'/
C
      DATA NUMSTA/22/
C
      DATA IHSTAT(1),IHSTA2(1)/'SIZE','    '/
      DATA IHSTAT(2),IHSTA2(2)/'NUMB','ER  '/
      DATA IHSTAT(3),IHSTA2(3)/'SUM ','    '/
      DATA IHSTAT(4),IHSTA2(4)/'MIDR','ANGE'/
      DATA IHSTAT(5),IHSTA2(5)/'MEAN','    '/
      DATA IHSTAT(6),IHSTA2(6)/'AVER','AGE '/
      DATA IHSTAT(7),IHSTA2(7)/'MIDM','EAN '/
      DATA IHSTAT(8),IHSTA2(8)/'MEDI','AN  '/
      DATA IHSTAT(9),IHSTA2(9)/'STAN','ARD '/
      DATA IHSTAT(10),IHSTA2(10)/'VARI','ANCE'/
      DATA IHSTAT(11),IHSTA2(11)/'RELA','TIVE'/
      DATA IHSTAT(12),IHSTA2(12)/'RANG','E   '/
      DATA IHSTAT(13),IHSTA2(13)/'MINI','MUM '/
      DATA IHSTAT(14),IHSTA2(14)/'MAXI','MUM '/
      DATA IHSTAT(15),IHSTA2(15)/'STAN','DARD'/
      DATA IHSTAT(16),IHSTA2(16)/'SKEW','NESS'/
      DATA IHSTAT(17),IHSTA2(17)/'STAN','DARD'/
      DATA IHSTAT(18),IHSTA2(18)/'KURT','OSIS'/
      DATA IHSTAT(19),IHSTA2(19)/'AUTO','CORR'/
      DATA IHSTAT(20),IHSTA2(20)/'STAN','DARD'/
      DATA IHSTAT(21),IHSTA2(21)/'CORR','ELAT'/
      DATA IHSTAT(22),IHSTA2(22)/'RANK','    '/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTY'
      ISUBN2='P2  '
C
      IERROR='NO'
C
      IMAXR=0
C
      IQUAL='UNKN'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTYP2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IWIDTH
   53 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)(IANS(I),I=1,IWIDTH)
   54 FORMAT('IANS(.) = ',80A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  INITIALIZE THE OUTPUT PARAMETERS AND VARIABLES  **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO100I=1,30
      IFOUNZ(I)='NO'
      IBEGIN(I)=-1
      IEND(I)=-1
      ITYPE(I)='9999'
      IHOL(I)='9999'
      IHOL2(I)='9999'
      INT1(I)=-999999
      FLOAT1(I)=-999999.0
      IERRO1(I)='NO'
  100 CONTINUE
C
      NUMCL=0
      NUMPL=0
      NUMAOL=0
      ITYW1L='9999'
      ICAT1L='9999'
      INLI1L='9999'
      ITYW2L='9999'
      NUMCR=0
      NUMPR=0
      NUMAOR=0
      ITYW1R='9999'
      ICAT1R='9999'
      INLI1R='9999'
      ITYW2R='9999'
C
C               ****************************************************************
C               **  STEP 2--
C               **  EXAMINE THE LEFT-HAND SIDE OF EXPRESSION.
C               **  DETERMINE IF PARAMETER OR VARIABLE NAME TO LEFT OF = SIGN
C               **  HAS PARENTHESES.
C               **  IF IT HAS PARENTHESES, THIS MEANS THAT WE WILL BE
C               **  DEFINING    PART     OF A VARIABLE.
C               **  COMPONENT 1  = LET
C               **  COMPONENT 2  = VARIABLE NAME
C               **  COMPONENT 3  = (                             (IF IT EXISTS)
C               **  COMPONENT 4  = ARGUMENT (I.E., ROW OF TABLE) (IF IT EXISTS)
C               **  COMPONENT 5  = )                             (IF IT EXISTS)
C               **  COMPONENT 6  = =
C               ****************************************************************
C
C     MOST GENERAL FORM--LET X(I) = XXX FOR I = A B C
C                      --LET X(I) = XXX SUBSET XX A B
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     STEP 2.1--SEARCH FOR LET.
C
      ISTAR1=1
      ISTOP1=IWIDTH
      ISTRIN='LET'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(1),IBEGIN(1),IEND(1),
     1      ITYPE(1),IHOL(1),IHOL2(1),INT1(1),FLOAT1(1),IERRO1(1))
      IF(IFOUNZ(1).EQ.'YES')GOTO2190
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 2190 CONTINUE
C
C     STEP 2.2--SEARCH FOR = SIGN.
C
      ISTAR1=IEND(1)+1
      ISTOP1=IWIDTH
      ISTRIN='='
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(6),IBEGIN(6),IEND(6),
     1      ITYPE(6),IHOL(6),IHOL2(6),INT1(6),FLOAT1(6),IERRO1(6))
      IF(IFOUNZ(6).EQ.'YES')GOTO2290
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 2290 CONTINUE
C
C     STEP 2.3--SEARCH FOR LEFT-HAND SIDE (;
C     SEARCH BETWEEN LET AND =.
C
      ISTAR1=IEND(1)+1
      ISTOP1=IBEGIN(6)-1
      ISTRIN='('
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(3),IBEGIN(3),IEND(3),
     1      ITYPE(3),IHOL(3),IHOL2(3),INT1(3),FLOAT1(3),IERRO1(3))
      IF(IFOUNZ(3).EQ.'YES')GOTO2390
      GOTO2500
 2390 CONTINUE
C
C     STEP 2.4--SEARCH FOR LEFT-HAND SIDE );
C     SEARCH BETWEEN ( AND =.
C
      ISTAR1=IEND(3)+1
      ISTOP1=IBEGIN(6)-1
      ISTRIN=')'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(5),IBEGIN(5),IEND(5),
     1      ITYPE(5),IHOL(5),IHOL2(5),INT1(5),FLOAT1(5),IERRO1(5))
      IF(IFOUNZ(5).EQ.'YES')GOTO2490
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 2490 CONTINUE
      GOTO2600
C
C     STEP 2.5--IF NO LEFT-HAND SIDE PARENTHESES FOUND,
C     EXTRACT VARIABLE NAME;
C     SEARCH BETWEEN LET AND =.
C
 2500 CONTINUE
      ISTAR1=IEND(1)+1
      ISTOP1=IBEGIN(6)
      ISTRIN='!;='
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(2),IBEGIN(2),IEND(2),
     1      ITYPE(2),IHOL(2),IHOL2(2),INT1(2),FLOAT1(2),IERRO1(2))
      IF(IFOUNZ(2).EQ.'YES')GOTO2590
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 2590 CONTINUE
      GOTO2800
C
C     STEP 2.6--IF LEFT-HAND SIDE PARENTHESES FOUND,
C     FIRST EXTRACT VARIABLE NAME;
C     SEARCH BETWEEN LET AND (.
C
 2600 CONTINUE
      ISTAR1=IEND(1)+1
      ISTOP1=IBEGIN(3)
      ISTRIN='!;('
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(2),IBEGIN(2),IEND(2),
     1      ITYPE(2),IHOL(2),IHOL2(2),INT1(2),FLOAT1(2),IERRO1(2))
      IF(IFOUNZ(2).EQ.'YES')GOTO2690
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 2690 CONTINUE
C
C     STEP 2.7--ALSO IF LEFT-HAND SIDE PARENTHESES FOUND,
C     SEARCH FOR LEFT-HAND SIDE ARGUMENT NAME OR VALUE;
C     SEARCH BETWEEN ( AND ).
C
      ISTAR1=IEND(3)
      ISTOP1=IBEGIN(5)
      ISTRIN='(;)'
      ISTRI2='    '
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(4),IBEGIN(4),IEND(4),
     1      ITYPE(4),IHOL(4),IHOL2(4),INT1(4),FLOAT1(4),IERRO1(4))
      IF(IFOUNZ(4).EQ.'YES')GOTO2790
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 2790 CONTINUE
      K=4
      IF(ITYPE(K).EQ.'WORD')
     1CALL DPCHEC(K,IHOL,IHOL2,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1INT1,FLOAT1,IBUGA3,IERROR)
C
 2800 CONTINUE
C
C               *******************************************************
C               **  STEP 3--                                         **
C               **  EXAMINE THE RIGHT-HAND SIDE OF EXPRESSION.       **
C               **  DETERMINE WHICH OF THE 3 CASES WE HAVE--         **
C               **      1) LET X(I) =                                **
C               **      2) LET X(I) =       SUBSET XX  A  B          **
C               **      3) LET X(I) =       FOR XX = A  B  C         **
C               **  IF CASE 1 (THE NON-SUBSET AND NON-FOR CASE),     **
C               **  SEARCH FOR COMPONENTS 7, 8, 9, AND 10--          **
C               **  COMPONENT 7  = VARIABLE NAME                     **
C               **  COMPONENT 8  = (                                 **
C               **  COMPONENT 9  = ARGUMENT (THAT IS, ROW OF TABLE)  **
C               **  COMPONENT 10 = )                                 **
C               **  IF CASE 2 (THE SUBSET CASE), JUMP TO STEP 4      **
C               **  IF CASE 3 (THE FOR CASE), JUMP TO STEP 5.        **
C               *******************************************************
C
 3000 CONTINUE
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     STEP 3.1A--SEARCH FOR SUBSET.
C
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='SUBS'
      ISTRI2='ET  '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(11),IBEGIN(11),IEND(11),
     1      ITYPE(11),IHOL(11),IHOL2(11),INT1(11),FLOAT1(11),IERRO1(11))
CCCCC THE FOLLOWING SECTION WAS ADDED DECEMBER 1988 TO AVOID
CCCCC SPURIOUS ERROR MESSAGES WITH A LONG VARIABLE NAME LIKE SUBSETXX
CCCCC THE SECTION WAS CORRECTED ALSO IN JANUARY 1988 AND NOVEMBER 1989
      IENDP=IEND(11)+1
      IF(IENDP.LE.0)IFOUNZ(11)='NO'
      IF(IENDP.LE.0)GOTO3119
      IF(IFOUNZ(11).EQ.'YES'.AND.
     1   IENDP.LE.ISTOP1.AND.
     1   IANS(IENDP).NE.' ')IFOUNZ(11)='NO'
      IF(IFOUNZ(11).EQ.'YES')GOTO4000
 3119 CONTINUE
C
C     STEP 3.1B--SEARCH FOR EXCEPT.
C
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='EXCE'
      ISTRI2='PT  '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(11),IBEGIN(11),IEND(11),
     1      ITYPE(11),IHOL(11),IHOL2(11),INT1(11),FLOAT1(11),IERRO1(11))
CCCCC THE FOLLOWING SECTION WAS ADDED DECEMBER 1988 TO AVOID
CCCCC SPURIOUS ERROR MESSAGES WITH A LONG VARIABLE NAME LIKE EXCEPTXX
CCCCC THE SECTION WAS CORRECTED ALSO IN JANUARY 1988 AND NOVEMBER 1989
      IENDP=IEND(11)+1
      IF(IENDP.LE.0)IFOUNZ(11)='NO'
      IF(IENDP.LE.0)GOTO3129
      IF(IFOUNZ(11).EQ.'YES'.AND.
     1   IENDP.LE.ISTOP1.AND.
     1   IANS(IENDP).NE.' ')IFOUNZ(11)='NO'
      IF(IFOUNZ(11).EQ.'YES')GOTO4000
 3129 CONTINUE
C
C     STEP 3.1C--SEARCH FOR FOR.
C
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='FOR'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(21),IBEGIN(21),IEND(21),
     1      ITYPE(21),IHOL(21),IHOL2(21),INT1(21),FLOAT1(21),IERRO1(21))
CCCCC THE FOLLOWING SECTION WAS ADDED DECEMBER 1988 TO AVOID
CCCCC SPURIOUS ERROR MESSAGES WITH A LONG VARIABLE NAME LIKE FORTUNE
CCCCC THE SECTION WAS CORRECTED ALSO IN JANUARY 1988 AND NOVEMBER 1989
      IENDP=IEND(21)+1
      IF(IENDP.LE.0)IFOUNZ(21)='NO'
      IF(IENDP.LE.0)GOTO3139
      IF(IFOUNZ(21).EQ.'YES'.AND.
     1   IENDP.LE.ISTOP1.AND.
     1   IANS(IENDP).NE.' ')IFOUNZ(21)='NO'
      IF(IFOUNZ(21).EQ.'YES')GOTO5000
 3139 CONTINUE
C
C     STEP 3.1D--SEARCH FOR IF.
C
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='IF  '
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(11),IBEGIN(11),IEND(11),
     1      ITYPE(11),IHOL(11),IHOL2(11),INT1(11),FLOAT1(11),IERRO1(11))
CCCCC THE FOLLOWING SECTION WAS ADDED DECEMBER 1988 TO AVOID
CCCCC SPURIOUS ERROR MESSAGES WITH A LONG VARIABLE NAME LIKE IFRING
CCCCC THE SECTION WAS CORRECTED ALSO IN JANUARY 1988 AND NOVEMBER 1989
      IENDP=IEND(11)+1
      IF(IENDP.LE.0)IFOUNZ(11)='NO'
      IF(IENDP.LE.0)GOTO3149
      IF(IFOUNZ(11).EQ.'YES'.AND.
     1   IENDP.LE.ISTOP1.AND.
     1   IANS(IENDP).NE.' ')IFOUNZ(11)='NO'
      IF(IFOUNZ(11).EQ.'YES')GOTO4000
 3149 CONTINUE
C
C     STEP 3.2--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
C     SEARCH FOR RIGHT-HAND SIDE (;
C     SEARCH BETWEEN = AND END OF LINE.
C
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='('
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(8),IBEGIN(8),IEND(8),
     1      ITYPE(8),IHOL(8),IHOL2(8),INT1(8),FLOAT1(8),IERRO1(8))
      IF(IFOUNZ(8).EQ.'YES')GOTO3290
      GOTO3400
 3290 CONTINUE
C
C     STEP 3.3--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
C     SEARCH FOR RIGHT-HAND SIDE );
C     SEARCH BETWEEN ( AND END OF LINE.
C
      ISTAR1=IEND(8)+1
      ISTOP1=IWIDTH
      ISTRIN=')'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(10),IBEGIN(10),IEND(10),
     1      ITYPE(10),IHOL(10),IHOL2(10),INT1(10),FLOAT1(10),IERRO1(10))
      IF(IFOUNZ(10).EQ.'YES')GOTO3390
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 3390 CONTINUE
      GOTO3500
C
C     STEP 3.4--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
C     IF NO RIGHT-HAND SIDE PARENTHESES FOUND,
C     EXTRACT VARIABLE NAME OR VALUE;
C     SEARCH BETWEEN = AND END OF LINE.
C     ALSO, TO HANDLE THE COLUMN NAMING CASE
C     (E.G., LET X = COLUMN 1),
C     CHECK TO SEE IF ANOTHER ITEM
C     FOLLOWS THE VARIABLE NAME OR VALUE.
C     AND FURTERMORE, TO HANDLE THE DATA GENERATION CASE
C     (E.G., LET X = 1 1 10),
C     CHECK TO SEE OF 2 ITEMS
C     FOLLOW THE FIRST VALUE.
C
 3400 CONTINUE
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(7),IBEGIN(7),IEND(7),
     1      ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GOTO3410
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
C
 3410 CONTINUE
      ISTAR1=IEND(7)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(8),IBEGIN(8),IEND(8),
     1      ITYPE(8),IHOL(8),IHOL2(8),INT1(8),FLOAT1(8),IERRO1(8))
      IF(IFOUNZ(8).EQ.'YES')GOTO3420
      GOTO3900
C
 3420 CONTINUE
      ISTAR1=IEND(8)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(9),IBEGIN(9),IEND(9),
     1      ITYPE(9),IHOL(9),IHOL2(9),INT1(9),FLOAT1(9),IERRO1(9))
      GOTO3900
C
C     STEP 3.5--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
C     IF RIGHT-HAND SIDE PARENTHESES FOUND,
C     FIRST EXTRACT VARIABLE NAME;
C     SEARCH BETWEEN = AND (.
C
 3500 CONTINUE
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(8)
      ISTRIN='!;('
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(7),IBEGIN(7),IEND(7),
     1      ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GOTO3590
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 3590 CONTINUE
C
C     STEP 3.6--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
C     ALSO IF RIGHT-HAND SIDE PARENTHESES FOUND,
C     SEARCH FOR RIGHT-HAND SIDE ARGUMENT NAME OR VALUE;
C     SEARCH BETWEEN ( AND ).
C
      ISTAR1=IEND(8)
      ISTOP1=IBEGIN(10)
      ISTRIN='(;)'
      ISTRI2='    '
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(9),IBEGIN(9),IEND(9),
     1      ITYPE(9),IHOL(9),IHOL2(9),INT1(9),FLOAT1(9),IERRO1(9))
      IF(IFOUNZ(9).EQ.'YES')GOTO3690
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 3690 CONTINUE
      K=9
      IF(ITYPE(K).EQ.'WORD')
     1CALL DPCHEC(K,IHOL,IHOL2,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1INT1,FLOAT1,IBUGA3,IERROR)
C
 3900 CONTINUE
      GOTO6000
C
C               **********************************************************
C               **  STEP 4--                                            **
C               **  FOR THE CASE WHEN HAVE     LET X(I) =               **
C               **  EXAMINE THE RIGHT-HAND SIDE FOR    SUBSET XX  A  B  **
C               **  COMPONENT 7  = VARIABLE NAME                        **
C               **  COMPONENT 8  = (                                    **
C               **  COMPONENT 9  = ARGUMENT (THAT IS, ROW OF TABLE)     **
C               **  COMPONENT 10 = )                                    **
C               **  COMPONENT 11 = SUBSET                               **
C               **  COMPONENT 12 = LOWER LIMIT             OF SUBSET    **
C               **  COMPONENT 13 = UPPER LIMIT (IF EXISTS) OF SUBSET    **
C               **********************************************************
C
 4000 CONTINUE
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     STEP 4.2--IF SUBSET HAS BEEN FOUND,
C     SEARCH FOR RIGHT-HAND SIDE (;
C     SEARCH BETWEEN = AND SUBSET.
C
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(11)-1
      ISTRIN='('
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(8),IBEGIN(8),IEND(8),
     1      ITYPE(8),IHOL(8),IHOL2(8),INT1(8),FLOAT1(8),IERRO1(8))
      IF(IFOUNZ(8).EQ.'YES')GOTO4090
      GOTO4400
 4090 CONTINUE
C
C     STEP 4.3--IF SUBSET HAS BEEN FOUND,
C     SEARCH FOR RIGHT-HAND SIDE );
C     SEARCH BETWEEN ( AND SUBSET.
C
      ISTAR1=IEND(8)+1
      ISTOP1=IBEGIN(11)-1
      ISTRIN=')'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(10),IBEGIN(10),IEND(10),
     1      ITYPE(10),IHOL(10),IHOL2(10),INT1(10),FLOAT1(10),IERRO1(10))
      IF(IFOUNZ(10).EQ.'YES')GOTO4390
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 4390 CONTINUE
      GOTO4500
C
C     STEP 4.4--IF SUBSET HAS BEEN FOUND,
C     IF NO RIGHT-HAND SIDE PARENTHESES FOUND,
C     EXTRACT VARIABLE NAME OR VALUE;
C     SEARCH BETWEEN = AND SUBSET.
C
 4400 CONTINUE
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(11)
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(7),IBEGIN(7),IEND(7),
     1      ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GOTO4490
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 4490 CONTINUE
      GOTO4700
C
C     STEP 4.5--IF SUBSET HAS BEEN FOUND,
C     IF RIGHT-HAND SIDE PARENTHESES FOUND,
C     FIRST EXTRACT VARIABLE NAME;
C     SEARCH BETWEEN = AND (.
C
 4500 CONTINUE
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(8)
      ISTRIN='!;('
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(7),IBEGIN(7),IEND(7),
     1      ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GOTO4590
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 4590 CONTINUE
C
C     STEP 4.6--IF SUBSET HAS BEEN FOUND,
C     ALSO IF RIGHT-HAND SIDE PARENTHESES FOUND,
C     SEARCH FOR RIGHT-HAND SIDE ARGUMENT NAME OR VALUE;
C     SEARCH BETWEEN ( AND ).
C
      ISTAR1=IEND(8)
      ISTOP1=IBEGIN(10)
      ISTRIN='(;)'
      ISTRI2='    '
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(9),IBEGIN(9),IEND(9),
     1      ITYPE(9),IHOL(9),IHOL2(9),INT1(9),FLOAT1(9),IERRO1(9))
      IF(IFOUNZ(9).EQ.'YES')GOTO4690
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 4690 CONTINUE
      K=9
      IF(ITYPE(K).EQ.'WORD')
     1CALL DPCHEC(K,IHOL,IHOL2,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1INT1,FLOAT1,IBUGA3,IERROR)
C
C     STEP 4.7--IF SUBSET HAS BEEN FOUND,
C     SEARCH FOR VARIABLE NAME AFTER SUBSET;
C     SEARCH BETWEEN SUBSET AND THE END OF THE LINE.
C
 4700 CONTINUE
      ISTAR1=IEND(11)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(12),IBEGIN(12),IEND(12),
     1      ITYPE(12),IHOL(12),IHOL2(12),INT1(12),FLOAT1(12),IERRO1(12))
      IF(IFOUNZ(12).EQ.'YES')GOTO4790
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 4790 CONTINUE
C
C     STEP 4.8--IF SUBSET HAS BEEN FOUND,
C     SEARCH FOR LOWER LIMIT VALUE AFTER     SUBSET XXX
C     SEARCH BETWEEN VARIABLE NAME AND THE END OF THE LINE.
C
      ISTAR1=IEND(12)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(13),IBEGIN(13),IEND(13),
     1      ITYPE(13),IHOL(13),IHOL2(13),INT1(13),FLOAT1(13),IERRO1(13))
      IF(IFOUNZ(13).EQ.'YES')GOTO4890
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 4890 CONTINUE
C
C     STEP 4.9--IF SUBSET HAS BEEN FOUND,
C     SEARCH FOR UPPER LIMIT (IF EXISTENT) AFTER     SUBSET XXX
C     SEARCH BETWEEN LOWER LIMIT AND THE END OF THE LINE.
C
      ISTAR1=IEND(13)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(14),IBEGIN(14),IEND(14),
     1      ITYPE(14),IHOL(14),IHOL2(14),INT1(14),FLOAT1(14),IERRO1(14))
 4900 CONTINUE
      GOTO6000
C
C               **********************************************************
C               **  STEP 5--                                            **
C               **  FOR THE CASE WHEN HAVE     LET X(I) =               **
C               **  EXAMINE THE RIGHT-HAND SIDE FOR    FOR I = A  B  C  **
C               **  COMPONENT 7  = VARIABLE NAME                        **
C               **  COMPONENT 8  = (                                    **
C               **  COMPONENT 9  = ARGUMENT (THAT IS, ROW OF TABLE)     **
C               **  COMPONENT 10 = )                                    **
C               **  COMPONENT 21 = FOR                                  **
C               **  COMPONENT 22 = =                                    **
C               **  COMPONENT 23 = START     VALUE FOR DUMMY INDEX      **
C               **  COMPONENT 24 = INCREMENT VALUE FOR DUMMY INDEX      **
C               **  COMPONENT 25 = STOP      VALUE FOR SUMMY INDEX      **
C               **********************************************************
C
 5000 CONTINUE
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     STEP 5.2--IF FOR HAS BEEN FOUND,
C     SEARCH FOR RIGHT-HAND SIDE (;
C     SEARCH BETWEEN = AND FOR.
C
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(21)-1
      ISTRIN='('
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(8),IBEGIN(8),IEND(8),
     1      ITYPE(8),IHOL(8),IHOL2(8),INT1(8),FLOAT1(8),IERRO1(8))
      IF(IFOUNZ(8).EQ.'YES')GOTO5290
      GOTO5400
 5290 CONTINUE
C
C     STEP 5.3--IF FOR HAS BEEN FOUND,
C     SEARCH FOR RIGHT-HAND SIDE );
C     SEARCH BETWEEN ( AND FOR.
C
      ISTAR1=IEND(8)+1
      ISTOP1=IBEGIN(21)-1
      ISTRIN=')'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(10),IBEGIN(10),IEND(10),
     1      ITYPE(10),IHOL(10),IHOL2(10),INT1(10),FLOAT1(10),IERRO1(10))
      IF(IFOUNZ(10).EQ.'YES')GOTO5390
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5390 CONTINUE
      GOTO5500
C
C     STEP 5.4--IF FOR HAS BEEN FOUND,
C     IF NO RIGHT-HAND SIDE PARENTHESES FOUND,
C     EXTRACT VARIABLE NAME OR VALUE;
C     SEARCH BETWEEN = AND FOR.
C
 5400 CONTINUE
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(21)
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(7),IBEGIN(7),IEND(7),
     1      ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GOTO5490
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5490 CONTINUE
      GOTO5700
C
C     STEP 5.5--IF FOR HAS BEEN FOUND,
C     IF RIGHT-HAND SIDE PARENTHESES FOUND,
C     FIRST EXTRACT VARIABLE NAME;
C     SEARCH BETWEEN = AND (.
C
 5500 CONTINUE
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(8)
      ISTRIN='!;('
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(7),IBEGIN(7),IEND(7),
     1      ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GOTO5590
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5590 CONTINUE
C
C     STEP 5.6--IF FOR HAS BEEN FOUND,
C     ALSO IF RIGHT-HAND SIDE PARENTHESES FOUND,
C     SEARCH FOR RIGHT-HAND SIDE ARGUMENT NAME OR VALUE;
C     SEARCH BETWEEN ( AND ).
C
      ISTAR1=IEND(8)
      ISTOP1=IBEGIN(10)
      ISTRIN='(;)'
      ISTRI2='    '
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(9),IBEGIN(9),IEND(9),
     1      ITYPE(9),IHOL(9),IHOL2(9),INT1(9),FLOAT1(9),IERRO1(9))
      IF(IFOUNZ(9).EQ.'YES')GOTO5690
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5690 CONTINUE
      K=9
      IF(ITYPE(K).EQ.'WORD')
     1CALL DPCHEC(K,IHOL,IHOL2,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1INT1,FLOAT1,IBUGA3,IERROR)
C
C     STEP 5.7--IF FOR HAS BEEN FOUND,
C     SEARCH FOR VARIABLE NAME AFTER FOR;
C     SEARCH BETWEEN FOR AND THE END OF THE LINE.
C
 5700 CONTINUE
      ISTAR1=IEND(21)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(22),IBEGIN(22),IEND(22),
     1      ITYPE(22),IHOL(22),IHOL2(22),INT1(22),FLOAT1(22),IERRO1(22))
      IF(IFOUNZ(22).EQ.'YES')GOTO5790
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5790 CONTINUE
C
C     STEP 5.8--IF FOR HAS BEEN FOUND,
C     SEARCH FOR = SIGN AFTER    FOR XXX
C     SEARCH BETWEEN VARIABLE NAME AND END OF LINE.
C
      ISTAR1=IEND(22)+1
      ISTOP1=IWIDTH
      ISTRIN='='
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(23),IBEGIN(23),IEND(23),
     1      ITYPE(23),IHOL(23),IHOL2(23),INT1(23),FLOAT1(23),IERRO1(23))
      IF(IFOUNZ(23).EQ.'YES')GOTO5890
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5890 CONTINUE
C
C     STEP 5.9--IF FOR HAS BEEN FOUND,
C     SEARCH FOR START VALUE AFTER     FOR XXX =
C     SEARCH BETWEEN = AND THE END OF THE LINE.
C
      ISTAR1=IEND(23)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(24),IBEGIN(24),IEND(24),
     1      ITYPE(24),IHOL(24),IHOL2(24),INT1(24),FLOAT1(24),IERRO1(24))
      IF(IFOUNZ(24).EQ.'YES')GOTO5990
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5990 CONTINUE
C
C     STEP 5.10--IF FOR HAS BEEN FOUND,
C     SEARCH FOR INCREMENT VALUE AFTER     FOR XXX =
C     SEARCH BETWEEN START VALUE AND THE END OF THE LINE.
C
      ISTAR1=IEND(24)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(25),IBEGIN(25),IEND(25),
     1      ITYPE(25),IHOL(25),IHOL2(25),INT1(25),FLOAT1(25),IERRO1(25))
      IF(IFOUNZ(25).EQ.'YES')GOTO5930
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5930 CONTINUE
C
C     STEP 5.11--IF FOR HAS BEEN FOUND,
C     SEARCH FOR STOP VALUE AFTER     FOR XXX =
C     SEARCH BETWEEN INCREMENT VALUE AND THE END OF THE LINE.
C
      ISTAR1=IEND(25)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,
     1      IFOUNZ(26),IBEGIN(26),IEND(26),
     1      ITYPE(26),IHOL(26),IHOL2(26),INT1(26),FLOAT1(26),IERRO1(26))
      IF(IFOUNZ(26).EQ.'YES')GOTO5950
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GOTO9000
 5950 CONTINUE
      GOTO6000
C
C               ************************************************
C               **  STEP 6--                                  **
C               **  DETERMINE VARIOUS SUMMARY VARIABLES       **
C               **  FOR THE LEFT SIDE                         **
C               **  OF THE COMMAND LINE                       **
C               **  WHICH WILL BE HELPFUL BACK IN DPLET       **
C               **  FOR BRANCHING TO THE CORRECT              **
C               **  TYPE OF OPERATION.                        **
C               **  NOTE THAT THE    LEFT SIDE                    **
C               **  WILL BE FROM     LET                      **
C               **  TO THE           = SIGN                   **
C               **  BUT WILL NOT INCLUDE EITHER.              **
C               ************************************************
C
 6000 CONTINUE
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     STEP 6.0--
C     DETERMINE THE LIMITS OF THE LEFT SIDE
C
      IMINL=0
      IF(IFOUNZ(1).EQ.'YES')IMINL=IEND(1)+1
C
      IMAXL=0
      IF(IFOUNZ(6).EQ.'YES')IMAXL=IBEGIN(6)-1
C
      IF(IMINL.LE.0)GOTO6900
      IF(IMAXL.LE.0)GOTO6900
      IF(IMINL.GT.IMAXL)GOTO6900
C
C     STEP 6.1--
C     DETERMINE THE NUMBER OF COMPONENTS ON THE LEFT.
C     A COMPONET HERE = A WORD OR A PARENTHESIS.
C
      ISUM=0
      IMIN=2
      IMAX=5
      DO6100I=IMIN,IMAX
      IF(IFOUNZ(I).EQ.'YES')ISUM=ISUM+1
 6100 CONTINUE
      NUMCL=ISUM
C
C     STEP 6.2--
C     DETERMINE THE NUMBER OF PARENTHESES (LEFT + RIGHT).
C
      ISUM=0
      IMIN=IMINL
      IMAX=IMAXL
      DO6200I=IMIN,IMAX
      IF(IANS(I).EQ.'('.OR.IANS(I).EQ.')')ISUM=ISUM+1
 6200 CONTINUE
 6250 CONTINUE
      NUMPL=ISUM
C
C     STEP 6.3--
C     DETERMINE THE NUMBER OF ARITHMETIC OPERATIONS
C     +  -  *  /      ON THE LEFT
C     (IT SHOULD BE 0).
C     NOTE THAT THE ARITHMETIC OPERATION   **
C     WILL BE LUMPED IN WITH    *    .
C
      ISUM=0
      IMIN=IMINL
      IMAX=IMAXL
      DO6300I=IMIN,IMAX
      IF(IANS(I).EQ.'+'.OR.IANS(I).EQ.'-'.
     1OR.IANS(I).EQ.'*'.OR.IANS(I).EQ.'/')ISUM=ISUM+1
 6300 CONTINUE
 6350 CONTINUE
      NUMAOL=ISUM
C
C     STEP 6.4--
C     DETERMINE THE TYPE ('NUMB' OR 'WORD')
C     FOR THE FIRST WORD ON THE LEFT.
C     THIS SHOULD BE THE VARIABLE OR PARAMETER
C     DESIGNATION,
C     AND IT SHOULD BE A 'WORD'.
C
      ITYW1L=ITYPE(2)
C
C     STEP 6.5--
C     DETERMINE IF FIRST WORD ON THE LEFT
C     IS ALREADY IN THE NAME LIST OR NOT.
C
      INLI1L='NO'
      IVARL=IHOL(2)
      IVARL2=IHOL2(2)
      DO6500I=1,NUMNAM
      IF(IVARL.EQ.IHNAME(I).AND.IVARL2.EQ.IHNAM2(I))INLI1L='YES'
 6500 CONTINUE
C
C     STEP 6.6--
C     DETERMINE IF FIRST WORD ON THE LEFT
C     IS IN THE VARIABLE/PARAMETER NAME LIST, OR
C     IS A COLUMN NAMING (I.E., THE WORD 'COLU' OR 'COL', OR
C     IS A DATA MANIPULATION FUNCTION, OR
C     IS A STATISTICAL CALCULATION FUNCTION
C     (SEARCH IS DONE IN THAT ORDER).
C
C
      ICAT1L='NONE'
      IVARL=IHOL(2)
      IVARL2=IHOL2(2)
C
 6610 CONTINUE
      IF(INLI1L.EQ.'YES'.AND.IVARL.NE.'COLU')GOTO6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.NE.'COL ')GOTO6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.EQ.'COLU'.AND.IVARL2.EQ.'MN  '.AND.
     1IFOUNZ(3).EQ.'NO')GOTO6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.EQ.'COL '.AND.IVARL2.EQ.'    '.AND.
     1IFOUNZ(3).EQ.'NO')GOTO6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.EQ.'COLU'.AND.IVARL2.EQ.'MN  '.AND.
     1IFOUNZ(3).EQ.'YES'.AND.ITYPE(3).NE.'NUMB')GOTO6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.EQ.'COL '.AND.IVARL2.EQ.'    '.AND.
     1IFOUNZ(3).EQ.'YES'.AND.ITYPE(3).NE.'NUMB')GOTO6615
      GOTO6620
 6615 CONTINUE
      ICAT1L='VARP'
      GOTO6690
C
 6620 CONTINUE
      IF(IVARL.EQ.'COLU'.AND.IVARL2.EQ.'MN  '.AND.
     1IFOUNZ(3).EQ.'YES'.AND.ITYPE(3).EQ.'NUMB')GOTO6625
      IF(IVARL.EQ.'COL '.AND.IVARL2.EQ.'    '.AND.
     1IFOUNZ(3).EQ.'YES'.AND.ITYPE(3).EQ.'NUMB')GOTO6625
      GOTO6630
 6625 CONTINUE
      ICAT1L='COL'
      GOTO6690
C
 6630 CONTINUE
      DO6632I=1,NUMMAN
      IF(IVARL.EQ.IHMAN(I).AND.IVARL2.EQ.IHMAN2(I))GOTO6635
 6632 CONTINUE
      GOTO6640
 6635 CONTINUE
      ICAT1L='MANI'
      GOTO6690
C
 6640 CONTINUE
      DO6642I=1,NUMSTA
      IF(IVARL.EQ.IHSTAT(I).AND.IVARL2.EQ.IHSTA2(I))GOTO6645
 6642 CONTINUE
      GOTO6690
 6645 CONTINUE
      ICAT1L='STAT'
      GOTO6690
C
 6690 CONTINUE
C
C     STEP 6.7--
C     DETERMINE THE TYPE ('NUMB' OR 'WORD')
C     FOR THE SECOND WORD
C     (AS OPPOSED TO THE SECOND COMPONENT)
C     ON THE LEFT.
C     IF EXISTENT, THIS SHOULD BE THE ARGUMENT DESIGNATION
C     OF A VARIABLE,
C     AND IT MAY BE EITHER A 'WORD' OR A 'NUMB'.
C
      ITYW2L=ITYPE(4)
C
 6900 CONTINUE
C
C               *********************************************************
C               **  STEP 7--                                           **
C               **  DETERMINE VARIOUS SUMMARY VARIABLES                **
C               **  FOR THE RIGHT SIDE                                 **
C               **  OF THE COMMAND LINE                                **
C               **  WHICH WILL BE HELPFUL BACK IN DPLET                **
C               **  FOR BRANCHING TO THE CORRECT                       **
C               **  TYPE OF OPERATION.                                 **
C               **  NOTE THAT THE    RIGHT SIDE                            **
C               **  WILL BE FROM THE = SIGN                            **
C               **  TO THE           END OF THE LINE,                  **
C               **  OR TO AN         ISOLATED FOR,                     **
C               **  OR TO AN         ISOLATED SUBSET                   **
C               **  (WHICHEVER OF THE 3 IS SMALLEST).                  **
C               **  ALSO DETERMINE WHETHER THE QUALIFICATION           **
C               **  ON THE FAR RIGHT OF THE CARD IS                    **
C               **           1) BLANK (THAT IS, NO QUALIFICATION)      **
C               **           2) SUBSET                                 **
C               **           3) FOR                                    **
C               **  THE VARIABLE IQUAL WILL BE DEFINED IN              **
C               **  THIS REGARD                                        **
C               **  IQUAL WILL = 'NONE', 'FOR', 'SUBS', OR 'ERRO'.  **
C               *********************************************************
C
 7000 CONTINUE
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     STEP 7.0--
C     DETERMINE THE LIMITS OF THE    RIGHT SIDE
C
      IMINR=0
      IF(IFOUNZ(6).EQ.'YES')IMINR=IEND(6)+1
C
      IF(IFOUNZ(11).EQ.'YES'.AND.IFOUNZ(21).EQ.'YES')GOTO7020
      GOTO7030
C
 7020 CONTINUE
      WRITE(ICOUT,7021)
 7021 FORMAT('***** ERROR IN DPTYP2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7022)
 7022 FORMAT('      BOTH FOR CASE AND SUBSET CASE FOUND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7023)IWIDTH
 7023 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7024)
 7024 FORMAT('THE COMMAND LINE IS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7025)(IANS(I),I=1,IWIDTH)
 7025 FORMAT(80A1)
      CALL DPWRST('XXX','BUG ')
      IQUAL = 'ERRO'
      IMAXR=0
      GOTO7090
C
 7030 CONTINUE
      IF(IFOUNZ(11).EQ.'NO'.AND.IFOUNZ(21).EQ.'NO')IQUAL='NONE'
      IF(IFOUNZ(11).EQ.'YES')IQUAL='SUBS'
      IF(IFOUNZ(21).EQ.'YES')IQUAL='FOR'
      IF(IQUAL.EQ.'NONE')IMAXR=IWIDTH
      IF(IQUAL.EQ.'SUBS')IMAXR=IBEGIN(11)-1
      IF(IQUAL.EQ.'FOR')IMAXR=IBEGIN(21)-1
C
 7090 CONTINUE
      IF(IMINR.LE.0)GOTO7900
      IF(IMAXR.LE.0)GOTO7900
      IF(IMINR.GT.IMAXR)GOTO7900
C
C     STEP 7.1--
C     DETERMINE THE NUMBER OF COMPONENTS ON THE RIGHT.
C     A COMPONENT HERE = A WORD OR A PARENTHESIS.
C
      ISUM=0
      IMIN=7
      IMAX=10
      DO7100I=IMIN,IMAX
      IF(IFOUNZ(I).EQ.'YES')ISUM=ISUM+1
 7100 CONTINUE
      NUMCR=ISUM
C
C     STEP 7.2--
C     DETERMINE THE NUMBER OF PARENTHESES (LEFT + RIGHT).
C
      ISUM=0
      IMIN=IMINR
      IMAX=IMAXR
      DO7200I=IMIN,IMAX
      IF(IANS(I).EQ.'('.OR.IANS(I).EQ.')')ISUM=ISUM+1
 7200 CONTINUE
 7250 CONTINUE
      NUMPR=ISUM
C
C     STEP 7.3--
C     DETERMINE THE NUMBER OF ARITHMETIC OPERATIONS
C     +  -  *  /      ON THE RIGHT
C     (IT SHOULD BE 0).
C     NOTE THAT THE ARITHMETIC OPERATION   **
C     WILL BE LUMPED IN WITH    *    .
C
      ISUM=0
      IMIN=IMINR
      IMAX=IMAXR
      DO7300I=IMIN,IMAX
      IF(IANS(I).EQ.'+'.OR.IANS(I).EQ.'-'.
     1OR.IANS(I).EQ.'*'.OR.IANS(I).EQ.'/')ISUM=ISUM+1
 7300 CONTINUE
 7350 CONTINUE
      NUMAOR=ISUM
C
C     STEP 7.4--
C     DETERMINE THE TYPE ('NUMB' OR 'WORD')
C     FOR THE FIRST WORD ON THE RIGHT.
C     THIS SHOULD BE THE VARIABLE OR PARAMETER
C     DESIGNATION,
C     AND IT SHOULD BE A 'WORD'.
C
      ITYW1R=ITYPE(7)
C
C     STEP 7.5--
C     DETERMINE IF FIRST WORD ON THE RIGHT
C     IS ALREADY IN THE NAME LIST OR NOT.
C
      INLI1R='NO'
      IVARR=IHOL(7)
      IVARR2=IHOL2(7)
      DO7500I=1,NUMNAM
      IF(IVARR.EQ.IHNAME(I).AND.IVARR2.EQ.IHNAM2(I))INLI1R='YES'
 7500 CONTINUE
C
C     STEP 7.6--
C     DETERMINE IF FIRST WORD ON THE RIGHT
C     IS IN THE VARIABLE/PARAMETER NAME LIST, OR
C     IS A COLUMN NAMING (I.E., THE WORD 'COLU' OR 'COL', OR
C     IS A DATA MANIPULATION FUNCTION, OR
C     IS A STATISTICAL CALCULATION FUNCTION
C     (SEARCH IS DONE IN THAT ORDER).
C
      ICAT1R='NONE'
      IVARR=IHOL(7)
      IVARR2=IHOL2(7)
C
 7610 CONTINUE
      IF(INLI1R.EQ.'YES'.AND.IVARR.NE.'COLU')GOTO7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.NE.'COL ')GOTO7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.EQ.'COLU'.AND.IVARR2.EQ.'MN  '.AND.
     1IFOUNZ(8).EQ.'NO')GOTO7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.EQ.'COL '.AND.IVARR2.EQ.'    '.AND.
     1IFOUNZ(8).EQ.'NO')GOTO7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.EQ.'COLU'.AND.IVARR2.EQ.'MN  '.AND.
     1IFOUNZ(8).EQ.'YES'.AND.ITYPE(8).NE.'NUMB')GOTO7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.EQ.'COL '.AND.IVARR2.EQ.'    '.AND.
     1IFOUNZ(8).EQ.'YES'.AND.ITYPE(8).NE.'NUMB')GOTO7615
      GOTO7620
 7615 CONTINUE
      ICAT1R='VARP'
      GOTO7690
C
 7620 CONTINUE
      IF(IVARR.EQ.'COLU'.AND.IVARR2.EQ.'MN  '.AND.
     1IFOUNZ(8).EQ.'YES'.AND.ITYPE(8).EQ.'NUMB')GOTO7625
      IF(IVARR.EQ.'COL '.AND.IVARR2.EQ.'    '.AND.
     1IFOUNZ(8).EQ.'YES'.AND.ITYPE(8).EQ.'NUMB')GOTO7625
      GOTO7630
 7625 CONTINUE
      ICAT1R='COL'
      GOTO7690
C
 7630 CONTINUE
      DO7632I=1,NUMMAN
      IF(IVARR.EQ.IHMAN(I).AND.IVARR2.EQ.IHMAN2(I))GOTO7635
 7632 CONTINUE
      GOTO7640
 7635 CONTINUE
      ICAT1R='MANI'
      GOTO7690
C
 7640 CONTINUE
      DO7642I=1,NUMSTA
      IF(IVARR.EQ.IHSTAT(I).AND.IVARR2.EQ.IHSTA2(I))GOTO7645
 7642 CONTINUE
      GOTO7690
 7645 CONTINUE
      ICAT1R='STAT'
      GOTO7690
C
 7690 CONTINUE
C
C     STEP 7.7--
C     DETERMINE THE TYPE ('NUMB' OR 'WORD')
C     FOR THE SECOND WORD
C     (AS OPPOSED TO THE SECOND COMPONENT)
C     ON THE RIGHT.
C     IF EXISTENT, THIS SHOULD BE THE ARGUMENT DESIGNATION
C     OF A VARIABLE,
C     AND IT MAY BE EITHER A 'WORD' OR A 'NUMB'.
C
      ITYW2R=ITYPE(9)
C
 7900 CONTINUE
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
 9011 FORMAT('****** AT THE END      OF DPTYP2--')
      CALL DPWRST('XXX','BUG ')
      DO9020I=1,30
      IF(18.LE.I.AND.I.LE.20)GOTO9020
      IF(I.GE.25)GOTO9020
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)
 9022 FORMAT('I--IFOUNZ,IBEGIN,IEND,',
     1'ITYPE,IHOL,IHOL2,INT1,FLOAT1,IERRO1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9025)I,IFOUNZ(I),IBEGIN(I),IEND(I),
     1ITYPE(I),IHOL(I),IHOL2(I),INT1(I),FLOAT1(I),IERRO1(I)
 9025 FORMAT(I3,'--',A4,2X,I2,2X,I2,4X,
     1A4,2X,A4,2X,A4,2X,I8,2X,D15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NUMCL,NUMPL,NUMAOL,ITYW1L,ITYW2L,INLI1L,ICAT1L
 9031 FORMAT('NUMCL,NUMPL,NUMAOL,ITYW1L,ITYW2L,INLI1L,ICAT1L = ',
     1I8,I8,I8,2X,A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)NUMCR,NUMPR,NUMAOR,ITYW1R,ITYW2R,INLI1R,ICAT1R
 9032 FORMAT('NUMCR,NUMPR,NUMAOR,ITYW1R,ITYW2R,INLI1R,ICAT1R = ',
     1I8,I8,I8,2X,A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      GOTO9090
C
 9090 CONTINUE
      RETURN
      END
      SUBROUTINE DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,
     1                  IBUGA3,
     1                  IFOUZ2,ISTAR2,ISTOP2,
     1                  ITYPE2,IHOL,IHOL2,INT,FLOAT,IERROR)
C
C     NOTE--THIS SUBROUTINE IS IDENTICAL TO DPTY3C
C           AND HAS BEEN DUPLICATED ONLY FOR MAPPING PURPOSES.
C           DATE--JULY 7, 1978.
C
C     PURPOSE--SCAN THE CHARACTER ARRAY IANS(.) BETWEEN
C              COLUMNS ISTAR1 AND ISTOP1
C              FOR THE STRING DEFINED IN STRIN AND ISTRI2.
C     NOTE THAT THE STRING DEFINED IN ISTRIN AND ISTRI2
C     MAY BE EXPRESSED IN SEVERAL WAYS--
C          1) EXPLICITELY, E.G., LET    FOR    SUBSET, ETC.
C          2) IMPLICITELY WITH ! REPRESENTING THE FIRST
C             NON-BLANK CHARACTER THAT IS ENCOUNTERED;
C          3) IMPLICITELY WITH ; REPRESENTING ANY STRING
C             (INCLUDING ALL CHARACTERS, EVEN BLANKS));
C          4) IMPLICITELY WITH : REPRESENTING THE FIRST
C            BLANK CHARACTER THAT IS ENCOUNTERED.
C     NOTE--A GIVEN ARGUMENT MAY END UP WITH
C            3 DIFFERENT REPRESENTATIONS--
C            HOLLERITH, INTEGER, AND FLOATING POINT.
C     INPUT  ARGUMENTS--IANS   = A HOLLERITH 1-CHARACTER-PER-WORD
C                                VARIABLE CONTAINING THE INPUT LINE
C                                TO BE EXAMINED.
C                     --IWIDTH = THE (FULL) WIDTH OF THE INPUT LINE
C                                (THAT IS, THE NUMBER OF COLUMNS)
C                     --ISTAR1 = THE FIRST COLUMN FOR WHICH THE
C                                SCAN IS TO BE CARRIED OUT.
C                     --ISTOP1 = THE LAST  COLUMN FOR WHICH THE
C                                SCAN IS TO BE CARRIED OUT.
C                     --ISTRIN = THE HOLLERITH VARIABLE
C                                WHICH CONTAINS CHARACTERS 1 TO 4
C                                OF THE STRING TO BE SEARCHED FOR.
C                                THE DEFINITION OF THE STRING IN ISTRIN MAY
C                                MAY BE DONE EXPLICTELY (BUT IS LIMITED
C                                TO 4 CHARACTERS) OR IMPLICITELY
C                                WHICH IS NOT LIMITED TO 4 CHARACTERS AND IS MOR
C                                IS MORE GENERAL IN
C                                OTHER WAYS ALSO.
C                     --ISTRI2 = THE HOLLERITH VARIABLE
C                                WHICH CONTAINS CHARACTERS 5 TO 8
C                                OF THE STRING TO BE SEARCHED FOR.
C                                THE DEFINITION OF THE STRING IN ISTRIN MAY
C                                MAY BE DONE EXPLICTELY (BUT IS LIMITED
C                                TO 4 CHARACTERS) OR IMPLICITELY
C                                WHICH IS NOT LIMITED TO 4 CHARACTERS AND IS MOR
C                                IS MORE GENERAL IN
C                                OTHER WAYS ALSO.
C                     --INEX   = A HOLLERITH VARIABLE WHICH
C                                WILL CONTAIN ONE OF THE FOLLOWING 4 VALUES--
C                                II, IE, EI, EE THAT STANDS FOR
C                                WHERE I STANDS FOR INCLUSIVE AND
C                                WHERE E STANDS FOR EXCLUSIVE;
C                                INEX SPECIFIES WHETHER THE FIRST OR LAST CHARAC
C                                CHARACTER IS TO BE INCLUDED OR EXCLUDED IN
C                                IN DEFINING ISTAR2 AND ISTOP2.
C     OUTPUT ARGUMENTS--IFOUZ2 = A HOLLERITH VARIABLE
C                                WITH THE VALUE 'YES'
C                                IF THE STRING WAS FOUND;
C                                AND THE VALUE 'NO'
C                                IF THE STRING WAS NOT FOUND.
C                     --ISTAR2 = THE START COLUMN OF THE FOUND STRING
C                     --ISTOP2 = THE STOP COLUMN OF THE FIUND STRING.
C                     --ITYPE2 = A HOLLERITH VARIABLE
C                                WITH THE VALUE 'WORD' IF THE STRING CONTAINS
C                                ANY NON-NUMERIC (EXCLUDING BLANKS) CHARACTER;
C                                AND WITH THE VALUE 'NUMB' IF THE STRING CONTA
C                                ALL NUMERIC VALUES OR DECIMAL POINT OR + OR -
C                                (WITH INTERMITTENT BLANKS IGNORED).
C                     --IHOL   = THE HOLLERITH VARIABLE
C                                CONTAINING THE PACKED (4 CHARACTERS) VERSION
C                                OF CHARACTERS 1 TO 4 OF THE FOUND STRING.
C                     --IHOL2  = THE HOLLERITH VARIABLE
C                                CONTAINING THE PACKED (4 CHARACTERS) VERSION
C                                OF CHARACTERS 5 TO 8 OF THE FOUND STRING.
C                     --INT    = THE INTEGER VARIABLE
C                                CONTAINING THE INTEGER REPRESENTATION
C                                (IF POSSIBLE) OF THE FOUND STRING.
C                     --FLOAT  = THE FLOATING POINT VARIABLE
C                                CONTAINING THE FLOATING POINT REPRESENTATION
C                                (IF POSSIBLE) OF THE FOUND STRING.
C                     --IERROR = A HOLLERITH VARIABLE WITH VALUE
C                                'YES' OR 'NO' INDICATING IF AN
C                                ERROR CONDITION EXISTS.
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
C     ORIGINAL VERSION--FEBRUARY  1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS
      CHARACTER*4 ISTRIN
      CHARACTER*4 ISTRI2
      CHARACTER*4 INEX
      CHARACTER*4 IBUGA3
      CHARACTER*4 IFOUZ2
      CHARACTER*4 ITYPE2
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IERROR
C
      CHARACTER*4 ITEMP
      CHARACTER*4 IFLUNK
      CHARACTER*4 ISTRI3
      CHARACTER*4 ILAST
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
C
      DIMENSION ISTRI3(20)
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
      ISUBN1='DPTY'
      ISUBN2='P3  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTYP3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ISTAR1,ISTOP1
   53 FORMAT('ISTAR1,ISTOP1 = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)ISTRIN,ISTRI2
   54 FORMAT('ISTRIN,ISTRI2 = ',A4,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      NUMASC=4
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  INITIALIZE THE OUTPUT PARAMETERS AND VARIABLES  **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGA3.EQ.'OFF')GOTO150
      WRITE(ICOUT,101)
  101 FORMAT('AT THE BEGINNING OF DPTYP3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,102)IWIDTH
  102 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,103)(IANS(I),I=1,IWIDTH)
  103 FORMAT('IANS(.) = ',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,104)ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX
  104 FORMAT('ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX = ',I8,I8,A4,A4,A4)
      CALL DPWRST('XXX','BUG ')
  150 CONTINUE
      IFOUZ2='NO'
      ISTAR2=-1
      ISTOP2=-1
      ITYPE2='9999'
      IHOL ='9999'
      IHOL2='9999'
      INT = -999999
      FLOAT=-999999.0
C
C               ************************************************************
C               **  STEP 2--                                              **
C               **  DECOMPOSE THE INPUT SEARCH STRING INTO A1 CHARACTERS  **
C               ************************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IMAX=2*NUMASC
      DO300I=1,IMAX
      I2=I
      J=I
      IF(I.GT.NUMASC)J=I-NUMASC
      ISTAR3=NUMBPC*(J-1)
      ISTAR3=IABS(ISTAR3)
      ITEMP='    '
      IF(I.LE.NUMASC)CALL DPCHEX(ISTAR3,NUMBPC,ISTRIN,0,NUMBPC,ITEMP)
      IF(I.GT.NUMASC)CALL DPCHEX(ISTAR3,NUMBPC,ISTRI2,0,NUMBPC,ITEMP)
      IF(ITEMP.EQ.'    ')GOTO350
      ISTRI3(I)=ITEMP
  300 CONTINUE
      ILEN2=I2
      GOTO390
  350 CONTINUE
      ILEN2=I2-1
  390 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO399
      WRITE(ICOUT,391)
  391 FORMAT('IN THE MIDDLE OF DPTYP3 (AFTER STEP 2)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,392)ILEN2
  392 FORMAT('ILEN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,393)(ISTRI3(I),I=1,ILEN2)
  393 FORMAT('ISTRI3(.) = ',6A1)
      CALL DPWRST('XXX','BUG ')
  399 CONTINUE
C
C               ****************************************************************
C               **  STEP 3--
C               **  DISTINGUISH BETWEEN THE 3 TYPES OF POSSIBLE SEARCH STRINGS--
C               **  1) AN EXPLICITELY-DEFINED STRING; E.G.,
C               **     LET     FOR     SUBSET     =     5.3     -2.6666666
C               **     (AS IN COMMANDS, KEY WORDS, AND NUMBERS);
C               **  2) A STRING STARTING WITH THE FIRST NON-BLANK CHARACTER
C               **     AND ENDING WITH SOME SPECIFIED CHARACTER; E.G., XXXXX(
C               **     (AS IN THE VARIABLE NAME OF A SUBSCRIPTED VARIABLE,
C               **     OR THE ARGUMENT (I. E., THE SUBSCRIPT) IN A SUBSCRIPTED
C               **     VARIABLE);
C               **  3) A STRING STARTING WITH THE FIRST NON-BLANK CHARACTER
C               **     AND ENDING WITH THE FIRST SUBSEQUENT BLANK CHARACTER
C               **     (OR ENDING WITH THE END OF THE LINE).
C               **     E.G., XXXX
C               **     (AS IN SOME UNSPECIFIED PARAMETER OR VARIABLE NAME).
C               ****************************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ICASE=1
      IF(ISTRI3(1).NE.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).NE.':')
     1ICASE=2
      IF(ISTRI3(1).EQ.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).NE.':')
     1ICASE=3
      IF(ISTRI3(1).EQ.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).EQ.':')
     1ICASE=4
      IF(ILEN2.EQ.1.OR.ILEN2.EQ.2)ICASE=1
C
      IF(IBUGA3.EQ.'OFF')GOTO398
      WRITE(ICOUT,395)
  395 FORMAT('AFTER STEP 3 OF DPTYP3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,396)ICASE
  396 FORMAT('ICASE = ',I8)
      CALL DPWRST('XXX','BUG ')
  398 CONTINUE
C
C               *********************************************************
C               **  STEP 4--                                           **
C               **  DETERMINE IF THE DESIRED SEARCH STRING IS PRESENT  **
C               *********************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(ICASE.EQ.1)GOTO400
      IF(ICASE.EQ.2)GOTO500
      IF(ICASE.EQ.3)GOTO600
      IF(ICASE.EQ.4)GOTO700
C
  400 CONTINUE
      DO410I=ISTAR1,ISTOP1
      I2=I
      IF(IANS(I).EQ.ISTRI3(1))GOTO420
      GOTO410
  420 CONTINUE
      DO430J=1,ILEN2
      IPJM1=J+I-1
      IF(IPJM1.GT.ISTOP1)GOTO410
      IF(IANS(IPJM1).EQ.ISTRI3(J))GOTO430
      GOTO410
  430 CONTINUE
      IFOUZ2='YES'
      IF(INEX.EQ.'II')ISTAR2=I2
      IF(INEX.EQ.'IE')ISTAR2=I2
      IF(INEX.EQ.'EI')ISTAR2=I2+1
      IF(INEX.EQ.'EE')ISTAR2=I2+1
      IF(INEX.EQ.'II')ISTOP2=IPJM1
      IF(INEX.EQ.'IE')ISTOP2=IPJM1-1
      IF(INEX.EQ.'EI')ISTOP2=IPJM1
      IF(INEX.EQ.'EE')ISTOP2=IPJM1-1
      IF(ISTAR2.LE.ISTOP2)GOTO990
      GOTO900
  410 CONTINUE
      IFOUZ2='NO'
      GOTO9000
C
  500 CONTINUE
      DO510I=ISTAR1,ISTOP1
      I2=I
      IF(IANS(I).EQ.ISTRI3(1))GOTO520
  510 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  520 CONTINUE
      IMIN=I2
      DO530I=IMIN,ISTOP1
      I2=I
      IF(IANS(I).EQ.ISTRI3(ILEN2))GOTO540
  530 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  540 CONTINUE
      IFOUZ2='YES'
      IF(INEX.EQ.'II')ISTAR2=IMIN
      IF(INEX.EQ.'IE')ISTAR2=IMIN
      IF(INEX.EQ.'EI')ISTAR2=IMIN+1
      IF(INEX.EQ.'EE')ISTAR2=IMIN+1
      IF(INEX.EQ.'II')ISTOP2=I2
      IF(INEX.EQ.'IE')ISTOP2=I2-1
      IF(INEX.EQ.'EI')ISTOP2=I2
      IF(INEX.EQ.'EE')ISTOP2=I2-1
      IF(ISTAR2.LE.ISTOP2)GOTO990
      GOTO900
C
  600 CONTINUE
      DO610I=ISTAR1,ISTOP1
      I2=I
      IF(IANS(I).NE.' ')GOTO620
  610 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  620 CONTINUE
      IMIN=I2
      DO630I=IMIN,ISTOP1
      I2=I
      IF(IANS(I).EQ.ISTRI3(ILEN2))GOTO640
  630 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  640 CONTINUE
      IFOUZ2='YES'
      IF(INEX.EQ.'II')ISTAR2=IMIN
      IF(INEX.EQ.'IE')ISTAR2=IMIN
      IF(INEX.EQ.'EI')ISTAR2=IMIN+1
      IF(INEX.EQ.'EE')ISTAR2=IMIN+1
      IF(INEX.EQ.'II')ISTOP2=I2
      IF(INEX.EQ.'IE')ISTOP2=I2-1
      IF(INEX.EQ.'EI')ISTOP2=I2
      IF(INEX.EQ.'EE')ISTOP2=I2-1
      IF(ISTAR2.LE.ISTOP2)GOTO990
      GOTO900
C
  700 CONTINUE
      ILAST='BLAN'
      DO710I=ISTAR1,ISTOP1
      I2=I
      IF(IANS(I).NE.' ')GOTO720
  710 CONTINUE
      IFOUZ2='NO'
      GOTO9000
  720 CONTINUE
      IMIN=I2
      DO730I=IMIN,ISTOP1
      I2=I
      IF(IANS(I).EQ.' ')GOTO740
  730 CONTINUE
      ILAST='NOBL'
      IF(ISTOP1.EQ.IWIDTH)GOTO740
      IFOUZ2='NO'
      GOTO9000
  740 CONTINUE
      IFOUZ2='YES'
      IF(INEX.EQ.'II')ISTAR2=IMIN
      IF(INEX.EQ.'IE')ISTAR2=IMIN
      IF(INEX.EQ.'EI')ISTAR2=IMIN+1
      IF(INEX.EQ.'EE')ISTAR2=IMIN+1
      IF(INEX.EQ.'II'.AND.ISTOP1.NE.IWIDTH)
     1ISTOP2=I2
      IF(INEX.EQ.'II'.AND.ISTOP1.EQ.IWIDTH.AND.ILAST.EQ.'BLAN')
     1ISTOP2=I2
      IF(INEX.EQ.'II'.AND.ISTOP1.EQ.IWIDTH.AND.ILAST.NE.'BLAN')
     1ISTOP2=I2
      IF(INEX.EQ.'IE'.AND.ISTOP1.NE.IWIDTH)
     1ISTOP2=I2-1
      IF(INEX.EQ.'IE'.AND.ISTOP1.EQ.IWIDTH.AND.ILAST.EQ.'BLAN')
     1ISTOP2=I2-1
      IF(INEX.EQ.'IE'.AND.ISTOP1.EQ.IWIDTH.AND.ILAST.NE.'BLAN')
     1ISTOP2=I2
      IF(INEX.EQ.'EI'.AND.ISTOP1.NE.IWIDTH)
     1ISTOP2=I2
      IF(INEX.EQ.'EI'.AND.ISTOP1.EQ.IWIDTH.AND.ILAST.EQ.'BLAN')
     1ISTOP2=I2
      IF(INEX.EQ.'EI'.AND.ISTOP1.EQ.IWIDTH.AND.ILAST.NE.'BLAN')
     1ISTOP2=I2
      IF(INEX.EQ.'EE'.AND.ISTOP1.NE.IWIDTH)
     1ISTOP2=I2-1
      IF(INEX.EQ.'EE'.AND.ISTOP1.EQ.IWIDTH.AND.ILAST.EQ.'BLAN')
     1ISTOP2=I2-1
      IF(INEX.EQ.'EE'.AND.ISTOP1.EQ.IWIDTH.AND.ILAST.NE.'BLAN')
     1ISTOP2=I2
      IF(ISTAR2.LE.ISTOP2)GOTO990
      GOTO900
C
  900 CONTINUE
C
C     NOTE--THE FOLLOWING SECTION HAS BEEN 'BUGGED' OUT
C           TO CIRCUMVENT A PROBLEM WITH Y=(...
C           WHILE IT STILL LOOKED FOR A VARIABLE NAME
C           BETWEEN THE = AND THE (     .
C     CAUTION--WHEN IBUGA3 = 'OFF', AS IT USUALLY IS,
C              IERROR CAN NEVER BE 'YES'
C              UPON RETURN FROM DPTYP3:
C              BUT WHEN IBUGA3 = 'ON' (AS IN ERROR TRACING)
C              IERROR MAY = 'YES' WHICH MAY CHANGE THE
C              LOGIC PATH BACK IN DPTYP2.
C
      IF(IBUGA3.EQ.'OFF')GOTO9000
      WRITE(ICOUT,921)
  921 FORMAT('***** INTERNAL ERROR IN DPTYP3 SUBROUTINE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,922)
  922 FORMAT('ISTAR2 GREATER THAN ISTOP2')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,923)ISTAR2,ISTOP2
  923 FORMAT('ISTAR2, ISTOP2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,924)ICASE
  924 FORMAT('ICASE = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,925)IWIDTH
  925 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,926)(IANS(I),I=1,IWIDTH)
  926 FORMAT('IANS(.) = ',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,927)ISTAR1,ISTOP1
  927 FORMAT('ISTAR1, ISTOP1 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,928)ILEN2
  928 FORMAT('ILEN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,929)(ISTRI3(I),I=1,ILEN2)
  929 FORMAT('ISTRI3(.) = ',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,930)ISTRIN,ISTRI2
  930 FORMAT('ISTRIN,ISTRI2 = ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,931)INEX
  931 FORMAT('INEX = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  990 CONTINUE
C
C               ********************************************************
C               **  STEP 5--                                          **
C               **  CONVERT THE STRING INTO 2 HOLLERITH A4 WORDS.     **
C               **  IF MORE THAN 8 CHARACTERS, CONVERT ONLY           **
C               **  THE FIRST 8 CHARACTERS.                           **
C               **  OUTPUT THESE HOLLERITH WORDS AS IHOL AND IHOL2.   **
C               ********************************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IHOL ='    '
      IHOL2='    '
      IMAX=2*NUMASC
      J=0
      DO1000I=ISTAR2,ISTOP2
      J=J+1
      K=J
      IF(J.GT.NUMASC)K=J-NUMASC
      ISTAR3=NUMBPC*(K-1)
      ISTAR3=IABS(ISTAR3)
      IF(J.LE.NUMASC)CALL DPCHEX(0,NUMBPC,IANS(I),ISTAR3,NUMBPC,IHOL)
      IF(J.GT.NUMASC)CALL DPCHEX(0,NUMBPC,IANS(I),ISTAR3,NUMBPC,IHOL2)
      IF(J.GE.IMAX)GOTO1050
 1000 CONTINUE
 1050 CONTINUE
C
C               ****************************************************************
C               **  STEP 6--
C               **  CONVERT (IF POSSIBLE) THE STRING INTO AN INTEGER ARGUMENT.
C               **  OUTPUT  THIS INTEGER VALUE IN INT.
C               ****************************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IFLUNK='NO'
      ITYPE2='NUMB'
      IDIG=0
      ISIGN=0
      IDECPT=0
      ISUM=0
      DO2700I=ISTAR2,ISTOP2
      IREV=ISTOP2-(I-ISTAR2)
      IF(IANS(IREV).EQ.' ')GOTO2700
      IF(IANS(IREV).EQ.'0')GOTO2710
      IF(IANS(IREV).EQ.'1')GOTO2711
      IF(IANS(IREV).EQ.'2')GOTO2712
      IF(IANS(IREV).EQ.'3')GOTO2713
      IF(IANS(IREV).EQ.'4')GOTO2714
      IF(IANS(IREV).EQ.'5')GOTO2715
      IF(IANS(IREV).EQ.'6')GOTO2716
      IF(IANS(IREV).EQ.'7')GOTO2717
      IF(IANS(IREV).EQ.'8')GOTO2718
      IF(IANS(IREV).EQ.'9')GOTO2719
      IF(IANS(IREV).EQ.'+')GOTO2720
      IF(IANS(IREV).EQ.'-')GOTO2721
      IF(IANS(IREV).EQ.'.')GOTO2722
      IFLUNK='YES'
      GOTO2800
 2710 ITERM=0
      GOTO2725
 2711 ITERM=1
      GOTO2725
 2712 ITERM=2
      GOTO2725
 2713 ITERM=3
      GOTO2725
 2714 ITERM=4
      GOTO2725
 2715 ITERM=5
      GOTO2725
 2716 ITERM=6
      GOTO2725
 2717 ITERM=7
      GOTO2725
 2718 ITERM=8
      GOTO2725
 2719 ITERM=9
      GOTO2725
 2720 ISIGN=ISIGN+1
      GOTO2700
 2721 ISIGN=ISIGN+1
      ISUM=-ISUM
      GOTO2700
 2722 IDECPT=IDECPT+1
      IF(IDECPT.EQ.1.AND.IDIG.EQ.0)GOTO2700
      GOTO2800
 2725 IDIG=IDIG+1
      TERM2=10.0**(IDIG-1)
      ITERM2=TERM2 + 0.01
      ISUM=ISUM+ITERM*ITERM2
 2700 CONTINUE
      IF(IDIG.LE.0)GOTO2800
      IF(ISIGN.GE.2)GOTO2800
      INT=ISUM
 2800 CONTINUE
      IF(IFLUNK.EQ.'YES')ITYPE2='WORD'
 2100 CONTINUE
 2999 CONTINUE
C
C               ********************************************************
C               **  STEP 7--                                          **
C               **  CONVERT (IF POSSIBLE) THE STRING INTO A FLOATING  **
C               **  POINT ARGUMENT.                                   **
C               **  OUTPUT THIS FLOATING POINT VALUE IN FLOAT.        **
C               ********************************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      AMIN=-1000000.
      AMAX=+1000000.
      IFLUNK='NO'
      ITYPE2='NUMB'
      FLOAT=-1.0
C
      ILOC=0
      IDECPT=0
      DO3060I=ISTAR2,ISTOP2
      IF(IANS(I).EQ.'.')ILOC=I
      IF(IANS(I).EQ.'.')IDECPT=IDECPT+1
 3060 CONTINUE
      IF(IDECPT.GE.2)GOTO3900
      IF(IDECPT.EQ.1)GOTO3150
      DO3100I=ISTAR2,ISTOP2
      IREV=ISTOP2-(I-ISTAR2)
      IF(IANS(IREV).EQ.' ')GOTO3100
      IF(IANS(IREV).EQ.'0')GOTO3110
      IF(IANS(IREV).EQ.'1')GOTO3110
      IF(IANS(IREV).EQ.'2')GOTO3110
      IF(IANS(IREV).EQ.'3')GOTO3110
      IF(IANS(IREV).EQ.'4')GOTO3110
      IF(IANS(IREV).EQ.'5')GOTO3110
      IF(IANS(IREV).EQ.'6')GOTO3110
      IF(IANS(IREV).EQ.'7')GOTO3110
      IF(IANS(IREV).EQ.'8')GOTO3110
      IF(IANS(IREV).EQ.'9')GOTO3110
      IFLUNK='YES'
      IF(IANS(IREV).EQ.'+')GOTO3900
      IF(IANS(IREV).EQ.'-')GOTO3900
      GOTO3900
 3100 CONTINUE
      IFLUNK='YES'
      GOTO3900
 3110 ILOC=IREV+1
 3150 CONTINUE
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,3111)ILOC,IDECPT
 3111 FORMAT('ILOC = ',I8,'    IDECPT = ',I8)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
C     SECONDLY, COMPUTE THE INTEGER PART OF THE VALUE
C
      SIGN=1.0
      IDIGI=0
      ISIGN=0
      SUMI=0
      ILOCM1=ILOC-1
      IF(ILOCM1.LT.ISTAR2)GOTO3250
      DO3200I=ISTAR2,ILOCM1
      IREV=ILOCM1-(I-ISTAR2)
      IF(IANS(IREV).EQ.' ')GOTO3200
      IF(IANS(IREV).EQ.'0')GOTO3210
      IF(IANS(IREV).EQ.'1')GOTO3211
      IF(IANS(IREV).EQ.'2')GOTO3232
      IF(IANS(IREV).EQ.'3')GOTO3213
      IF(IANS(IREV).EQ.'4')GOTO3214
      IF(IANS(IREV).EQ.'5')GOTO3215
      IF(IANS(IREV).EQ.'6')GOTO3216
      IF(IANS(IREV).EQ.'7')GOTO3217
      IF(IANS(IREV).EQ.'8')GOTO3218
      IF(IANS(IREV).EQ.'9')GOTO3219
      IF(IANS(IREV).EQ.'+')GOTO3220
      IF(IANS(IREV).EQ.'-')GOTO3221
      IFLUNK='YES'
      GOTO3900
 3210 ITERM=0
      GOTO3225
 3211 ITERM=1
      GOTO3225
 3232 ITERM=2
      GOTO3225
 3213 ITERM=3
      GOTO3225
 3214 ITERM=4
      GOTO3225
 3215 ITERM=5
      GOTO3225
 3216 ITERM=6
      GOTO3225
 3217 ITERM=7
      GOTO3225
 3218 ITERM=8
      GOTO3225
 3219 ITERM=9
      GOTO3225
 3220 ISIGN=ISIGN+1
      GOTO3200
 3221 ISIGN=ISIGN+1
      SIGN=-SIGN
      GOTO3200
 3225 IDIGI=IDIGI+1
      TERM=ITERM
      IEXP=IDIGI-1
      SUMI=SUMI+TERM*(10.0**IEXP)
 3200 CONTINUE
 3250 CONTINUE
      IF(ISIGN.GE.2)GOTO3900
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,3255)IDIGI,SUMI
 3255 FORMAT('IDIGI = ',I8,'    SUMI = ',F20.10)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
C     THIRDLY, COMPUTE THE DECIMAL PART OF THE VALUE
C
      IDIGD=0
      SUMD=0.0
      ILOCP1=ILOC+1
      IF(ILOCP1.GT.ISTOP2)GOTO3350
      DO3300I=ILOCP1,ISTOP2
      IF(IANS(I).EQ.' ')GOTO3300
      IF(IANS(I).EQ.'0')GOTO3310
      IF(IANS(I).EQ.'1')GOTO3311
      IF(IANS(I).EQ.'2')GOTO3312
      IF(IANS(I).EQ.'3')GOTO3333
      IF(IANS(I).EQ.'4')GOTO3314
      IF(IANS(I).EQ.'5')GOTO3315
      IF(IANS(I).EQ.'6')GOTO3316
      IF(IANS(I).EQ.'7')GOTO3317
      IF(IANS(I).EQ.'8')GOTO3318
      IF(IANS(I).EQ.'9')GOTO3319
      IFLUNK='YES'
      GOTO3900
 3310 ITERM=0
      GOTO3325
 3311 ITERM=1
      GOTO3325
 3312 ITERM=2
      GOTO3325
 3333 ITERM=3
      GOTO3325
 3314 ITERM=4
      GOTO3325
 3315 ITERM=5
      GOTO3325
 3316 ITERM=6
      GOTO3325
 3317 ITERM=7
      GOTO3325
 3318 ITERM=8
      GOTO3325
 3319 ITERM=9
      GOTO3325
 3325 IDIGD=IDIGD+1
      TERM=ITERM
      SUMD=SUMD+TERM/(10.0**IDIGD)
 3300 CONTINUE
 3350 CONTINUE
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,3355)IDIGD,SUMD
 3355 FORMAT('IDIGD = ',I8,'    SUMD = ',F20.10)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IDIGT=IDIGI+IDIGD
      IF(IDIGT.LE.0)GOTO3900
      FLOAT=SUMI+SUMD
      IF(SIGN.LT.0.0)FLOAT=-FLOAT
      IF(AMIN.LE.FLOAT.AND.FLOAT.LE.AMAX)GOTO3000
      GOTO3900
C
 3900 CONTINUE
      IF(IFLUNK.EQ.'YES')ITYPE2='WORD'
 3000 CONTINUE
 3999 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9900
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9001)
 9001 FORMAT('****** AT THE END       OF DPTYP3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9002)IFOUZ2,ISTAR2,ISTOP2
 9002 FORMAT('IFOUZ2, ISTAR2, ISTOP2 = ',A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9003)ITYPE2,IHOL,IHOL2,INT,FLOAT,IERROR
 9003 FORMAT('ITYPE2,IHOL,IHOL2,INT,FLOAT,IERROR = ',A4,2X,A4,A4,2X,
     1I8,F15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
C
 9900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTYPE(IANSLC,IWIDTH,IBUGTY,
     1                  ICOM,ICOM2,ICOMT,ICOMI,ACOM,ICOMLC,ICOML2,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IHARG,IHARG2,IARGT,IARG,ARG,
     1                  IHARLC,IHARL2,NUMARG,
     1                  IHOST1,IHOST2)
C
C     PUTPOSE--TAKE THE COMPONENTS OF AN INPUT COMMAND LINE
C              AND COMPUTE HOLLERITH, INTEGER, AND FLOATING POINT
C              EQUIVALENTS FOR EACH COMPONENT.
C     INPUT  ARGUMENTS--IANSLC   (A HOLLERITH VECTOR)
C                     --IWIDTH (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--ICOM   (AN A4 HOLLERITH VALUE FOR COMMAND)
C                     --ICOM2  (AN A4 HOLLERITH VALUE FOR COMMAND)
C                     --ICOMLC  (AN A4 HOLLERITH VALUE FOR COMMAND)
C                     --ICOML2  (AN A4 HOLLERITH VALUE FOR COMMAND)
C                     --IHARG  (AN A4 HOLLERITH VECTOR)
C                     --IHARG2 (AN A4 HOLLERITH VECTOR)
C                     --IARG   (AN INTEGER VECTOR)
C                     --ARG    (A FLOATING POINT VECTOR)
C                     --IHARLC (AN A4 HOLLERITH VECTOR)
C                     --IHARL2 (AN A4 HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C      NOTE--A GIVEN ARGUMENT MAY END UP WITH
C            3 DIFFERENT REPRESENTATIONS--
C            HOLLERITH, INTEGER, AND FLOATING POINT.
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
C     ORIGINAL VERSION--NOVEMBER 10, 1977.
C     UPDATED         --MAY       1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --NOVEMBER  1982.
C     UPDATED         --SEPTEMBER 1986.
C     UPDATED         --FEBRUARY  1989. ADJUST <> CASE (ALAN)
C     UPDATED         --AUGUST    1990. FIX HONEYWELL/PRIME > PROBLEM
C     UPDATED         --OCTOBER   1997. CHECK FOR EXPONENTIAL NUMBERS
C     UPDATED         --OCTOBER   2001. BUG ON SUN
C     UPDATED         --APRIL     2018. TREAT COMMA AS DELIMITER (IN
C                                       ADDITION TO SPACE AND HYPHEN)
C     UPDATED         --APRIL     2018. OPTIONS TO TURN OFF HYPHEN, 
C                                       COMMA, OR EQUAL AS A DELIMITER
C     UPDATED         --APRIL     2018. IF EQUAL SIGN NOT A DELIMITER,
C                                       THEN QUOTE TO RIGHT OF EQUAL
C                                       DOES NOT START A NEW WORD
C                                       (BUT DO TURN QUOTING ON),
C                                       NEEDED FOR:
C                                          CALL TITLE="Sample Title"
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IERROR
      CHARACTER*4 IANSLC
      CHARACTER*4 IBUGTY
      CHARACTER*4 ICOM
      CHARACTER*4 ICOM2
      CHARACTER*4 ICOMT
      CHARACTER*4 ICOMLC
      CHARACTER*4 ICOML2
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
      CHARACTER*4 IHARLC
      CHARACTER*4 IHARL2
      CHARACTER*4 IHOST1
      CHARACTER*4 IHOST2
C
      CHARACTER*4 IFLUNK
      CHARACTER*4 IB
      CHARACTER*4 IANS1
      CHARACTER*4 IANS2
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*10 ICJUNK
      CHARACTER*5 IFRMT
C
C---------------------------------------------------------------------
C
      DIMENSION IANSLC(*)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IHARLC(*)
      DIMENSION IHARL2(*)
C
CCCCC PARAMETER (MAXZZZ=255)
      PARAMETER (MAXZZZ=1024)
C
      DIMENSION ISTART(MAXZZZ)
      DIMENSION ISTOP(MAXZZZ)
      DIMENSION IB(MAXZZZ)
C
      INCLUDE 'DPCOST.INC'
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
      ISUBN1='DPTY'
      ISUBN2='PE  '
      IERROR='OFF'
C
      IF(IBUGTY.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTYPE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)(IANSLC(I),I=1,MIN(120,IWIDTH))
   53   FORMAT('(IANSLC(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)IWIDTH,IHOST1,IHOST2
   61   FORMAT('IWIDTH,IHOST1,IHOST2 = ',I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ************************************************************
C               **  DEFINE NUMASC = NUMBER OF ASCII CHARACTERS PER WORD.  **
C               **  THIS IS 4 REGARDLESS OF THE COMPUTER MAKE AND         **
C               **  REGARDLESS OF THE WORD SIZE.                          **
C               ************************************************************
C
      NUMASC=4
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOM='    '
      ICOM2='    '
      ICOMT='NUMB'
      ICOMI=(-1)
      ACOM=(-1.0)
      ICOMLC='    '
      ICOML2='    '
      DO110I=1,100
        IHARG(I)='    '
        IHARG2(I)='    '
        IARGT(I)='NUMB'
        IARG(I)=(-1)
        ARG(I)=(-1.0)
        IHARLC(I)='    '
        IHARL2(I)='    '
  110 CONTINUE
      NUMARG=(-1)
C
C               **********************************************************
C               **  STEP 2--                                             *
C               **  SEPARATE IANSLC(.) INTO COMPONENTS WHERE A COMPONENT *
C               **  IS DEFINED AS THAT SEPARATED BY 1 OR MORE BLANKS     *
C               **  IN ADDITION, AN EQUAL SIGN (=),                      *
C               **  IN ADDITION, A COMMA (,), (2018/04)                  *
C
CCCCC --------------------------------------------------------------------
CCCCC THE FOLLOWING DEALING WITH > AND < WAS DEACTIVATED AUGUST 1990
CCCCC DUE TO FACT THAT > IS A DIRECTORY SEPARATOR FOR   AUGUST 1990
CCCCC CERTAIN COMPUTERS (E.G., HONEYWELL, PRIME).  AUGUST 1990
CCCCC AND     CALL DATAPLOT>DPSYSF.TEX    WAS BOMBING      AUGUST 1990
CCCCC WITH ARRAY OVERFLOW.                              AUGUST 1990
CCCCC THEREFORE--USER MUST MANUALLY MAKE SURE THAT > AND < AUGUST 1990
CCCCC            ARE SURROUNDED BY SPACES IN MATH COMMANDS.  AUGUST 1990
C
C               **  A GREATER-THAN SIGN (>), AND A LESS-THAN SIGN (<)    *
C               **  ARE ALSO CONSIDERED AS A COMPONENT UNTO ITSELF       *
C               **  REGARDLESS OF WHETHER OR NOT                         *
C               **  IT HAS PRECEEDING AND SUCCEEDING BLANKS.             *
CCCCC --------------------------------------------------------------------
C               **  FINALLY, A HYPHEN WHEN IMMEDIATELY PRECEDED          *
C               **  AND SUCCEEDED BY A NON-BLANK CHARACTER               *
C               **  WILL ALSO BE CONSIDERED AS A SEPARATOR               *
C               **  AND SO WILL NOT BE COPIED AS A CHARACTER.            *
C               **  HOWEVER, IF THERE IS A BLANK BEFORE OR AFTER THE     *
C               **  HYPEN (AS IN DEFINING THE    -    AS A PLOT CHARACTER*
C               **  TYPE), THEN THE HYPHEN WILL BE TREATED AND COPIED    *
C               **  AS A SEPARATE COMPONENT.                             *
C               **  OCTOBER 1997: CHECK FOR EXPONENTIAL NOTATION, I.E.   *
C               **      1.2E02, 1.2E-02, 1.2E+02, 1.2D02, 1.2D-02, 1.2D+02
C               **  TREAT THE CASE WHERE THE ORIGINAL LINE IANSLC(.) WAS NON-EMP
C               **  LOCATE THE START AND STOP COLUMNS FOR EACH 'WORD'.   *
C               **********************************************************
C
      ISTEPN='2'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMWD=0
      DO300I=1,IWIDTH
        IM1=I-1
        IM2=I-2
        IP1=I+1
C
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(I).EQ.'=')GOTO350
        IF(IHOST1.EQ.'HONE')THEN
          IF(IANSLC(I).EQ.'>')GOTO350
          IF(IANSLC(I).EQ.'<')GOTO350
        ENDIF
C       ADD "<>  " CASE
        IF(I.GT.1.AND.IANSLC(I).EQ.'>'.AND.IANSLC(I-1).EQ.'<')GOTO300
C
CCCCC   THE FOLLOWING LINE WAS COMMENTED OUT AUGUST 1990
CCCCC   DUE TO BOMB ON HONEYWELL/PRIME WHEN TRYING TO EXECUTE  AUGUST 1990
CCCCC   CALL DATAPLOT>DPSYSF.TEX   (> IS A DIRECTORY SYMBOL   AUGUST 1990
CCCCC   ON HONEYWELL AND PRIME)               AUGUST 1990
CCCCC   IF(IANSLC(I).EQ.'>')GOTO350
        IF(IANSLC(I).EQ.'<'.AND.IANSLC(I+1).EQ.'>')GOTO345
C
CCCCC   THE FOLLOWING LINE WAS COMMENTED OUT AUGUST 1990
CCCCC   TO PARALLEL THE COMMENTING OUT FOR    >   2 LINES ABOVE  AUGUST 1990
CCCCC   IF(IANSLC(I).EQ.'<')GOTO350
C
        IF(IANSLC(I).NE.' '.AND.I.LE.1)GOTO350
C
        IF(I.LE.1)GOTO360
        IF(IANSLC(I).NE.' '.AND.IANSLC(IM1).EQ.' ')GOTO350
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.
     1     IANSLC(IM1).EQ.'=')GOTO350
        IF(IHOST1.EQ.'HONE')THEN
          IF(IANSLC(I).NE.' '.AND.IANSLC(IM1).EQ.'>')GOTO350
          IF(IANSLC(I).NE.' '.AND.IANSLC(IM1).EQ.'<')GOTO350
        ENDIF
C
        IF(I.LE.2)GOTO360
C
CCCCC   OCTOBER 1997.  CHECK FOR EXPONENTIAL NOTATION,
CCCCC                  I.E., IF "-" IS PRECEDED BY AN "E" AND SUCCEDED
CCCCC                  BY ANUMBER.
C
        IF(IANSLC(IM1).EQ.'-')THEN
          IF(IANSLC(IM2).EQ.'E' .OR. IANSLC(IM2).EQ.'e')THEN
            CALL DPCOAN(IANSLC(I),IJUNK)
            IF(IJUNK.GE.48 .AND. IJUNK.LE.57)GOTO370
          ENDIF
        ENDIF
C
        IF(IHYPCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.
     1     IANSLC(IM1).EQ.'-')GOTO340
        IF(ICOMCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.
     1     IANSLC(IM1).EQ.',')GOTO340
        GOTO360
C
  340   CONTINUE
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(IM2).EQ.'=')GOTO360
        IF(IHYPCL.EQ.'ON'.AND.IANSLC(IM2).EQ.'-')GOTO355
        IF(ICOMCL.EQ.'ON'.AND.IANSLC(IM2).EQ.',')GOTO355
        IF(IANSLC(IM2).NE.' ')GOTO350
        GOTO360
C
C  ADD "<>  " CASE
  345   CONTINUE
        NUMWD=NUMWD+1
        ISTART(NUMWD)=I
        ISTOP(NUMWD)=I+1
        GOTO390
C       END ADD
  350   CONTINUE
        NUMWD=NUMWD+1
C
  355   CONTINUE
        ISTART(NUMWD)=I
C
  360   CONTINUE
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(I).EQ.'=')GOTO370
CCCCC   IF(IANSLC(I).EQ.'>')GOTO370
CCCCC   IF(IANSLC(I).EQ.'<')GOTO370
        IF(IANSLC(I).NE.' '.AND.I.GE.IWIDTH)GOTO370
C
        IF(I.GE.IWIDTH)GOTO390
        IF(IANSLC(I).NE.' '.AND.IANSLC(IP1).EQ.' ')GOTO370
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.
     1     IANSLC(IP1).EQ.'=')GOTO370
CCCCC   IF(IANSLC(I).NE.' '.AND.IANSLC(IP1).EQ.'>')GOTO370
CCCCC   IF(IANSLC(I).NE.' '.AND.IANSLC(IP1).EQ.'<')GOTO370
        IF(IHYPCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.
     1     IANSLC(IP1).EQ.'-')GOTO370
        IF(ICOMCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.
     1     IANSLC(IP1).EQ.',')GOTO370
C
        GOTO390
C
  370   CONTINUE
        ISTOP(NUMWD)=I
C
  390   CONTINUE
C
        IF(IBUGTY.EQ.'ON')THEN
          WRITE(ICOUT,391)NUMWD
  391     FORMAT('NUMWD = ',I8)
          CALL DPWRST('XXX','BUG ')
          IF(NUMWD.GE.1)THEN
            WRITE(ICOUT,392)I,NUMWD,ISTART(NUMWD),ISTOP(NUMWD)
  392       FORMAT('I,NUMWD,ISTART(NUMWD),ISTOP(NUMWD) = ',4I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
C
  300 CONTINUE
      IF(NUMWD.LE.0)GOTO9000
C
C               ***********************************************************
C               **  STEP 3--                                             **
C               **  CONVERT THE FIRST STRING TO A COMMAND                **
C               **  EXTRACT THE FIRST 4 CHARACTERS OF                    **
C               **  THE COMMAND.  PACK THESE 4 CHARACTERS                **
C               **  INTO THE HOLLERITH VARIABLE ICOM.                    **
C               **  ONLY 4 CHARACTERS ARE RETAINED                       **
C               **  REGARDLESS OF THE MAX NUMBER OF                      **
C               **  CHARACTERS PER WORD ON A GIVEN                       **
C               **  COMPUTER (E.G., EVEN THOUGH UNIVAC                   **
C               **  COULD RETAIN 6 CHARACTERS PER WORD,                  **
C               **  IT IS SUFFICIENT              TO RETAIN              **
C               **  ONLY 4 CHARACTERS PER WORD--ON A UNIVAC              **
C               **  OR ANY OTHER COMPUTER.                               **
C               **  OR ANY OTHER COMPUTER.                               **
C               **  ALSO, IF THE NUMBER OF CHARACTERS                    **
C               **  IN THE FIRST WORD IS 5 OR MORE,                      **
C               **  THEN PACK CHARACTERS 5 THROUGH 8                     **
C               **  (OR CHARACTERS 5 THROUGH THE END OF THE WORD         **
C               **  IF THE END OF THE WORD IS BEFORE CHARACTER 8)        **
C               **  INTO THE 4-CHARACTER WORD ICOM2.                     **
C               ***********************************************************
C
      ISTEPN='3'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWORD=1
      IWID=ISTOP(IWORD)-ISTART(IWORD)+1
      JMIN=ISTART(IWORD)
      JMAX=ISTOP(IWORD)
      I=0
      DO800J=JMIN,JMAX
        I=I+1
        IB(I)=IANSLC(J)
  800 CONTINUE
C
      IANS1='    '
      IANS2='    '
      IMAX=2*NUMASC
      IF(IWID.LT.IMAX)IMAX=IWID
C
      IF(IBUGTY.EQ.'ON')THEN
        WRITE(ICOUT,901)IMAX
  901   FORMAT('IMAX = ',I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO900I=1,IMAX
        IF(IB(I).EQ.' ')GOTO910
        IM4=I-4
        IF(I.LE.NUMASC)IANS1(I:I)=IB(I)
        IF(I.GT.NUMASC)IANS2(IM4:IM4)=IB(I)
  900 CONTINUE
  910 CONTINUE
      ICOMLC=IANS1
      ICOML2=IANS2
      CALL DPUPP4(ICOMLC,ICOM,IBUGTY,IERROR)
      CALL DPUPP4(ICOML2,ICOM2,IBUGTY,IERROR)
C
C               ********************************************
C               **  STEP 4--                              **
C               **  CONVERT STRINGS 2 THROUGH END         **
C               **  TO HOLLERITH A4 ARGUMENTS.            **
C               **  IF MORE THAN 8 CHARACTERS,            **
C               **  CONVERT ONLY THE FIRST 8 CHARACTERS   **
C               **  (REGARDLESS OF THE COMPUTER TYPE).    **
C               ********************************************
C
      ISTEPN='4'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMARG=NUMWD-1
      IF(NUMWD.LE.1)GOTO1999
      DO1000IWORD=2,NUMWD
        IWID=ISTOP(IWORD)-ISTART(IWORD)+1
C
        JMIN=ISTART(IWORD)
        JMAX=ISTOP(IWORD)
        I=0
        DO1100J=JMIN,JMAX
          I=I+1
          IB(I)=IANSLC(J)
 1100   CONTINUE
C
        IANS1='    '
        IANS2='    '
        IMAX=2*NUMASC
        IF(IWID.LT.IMAX)IMAX=IWID
        DO1200I=1,IMAX
          IF(IB(I).EQ.' ')GOTO1210
          IM4=I-4
          IF(I.LE.NUMASC)IANS1(I:I)=IB(I)
          IF(I.GT.NUMASC)IANS2(IM4:IM4)=IB(I)
 1200   CONTINUE
 1210   CONTINUE
        IWORM1=IWORD-1
        IHARLC(IWORM1)=IANS1
        IHARL2(IWORM1)=IANS2
C
 1000 CONTINUE
 1999 CONTINUE
C
C               **********************************************************
C               **  STEP 4.5--                                          **
C               **  CONVERT EACH ARGUMENT TO UPPER CASE.                **
C               **********************************************************
C
      ISTEPN='4.5'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.0)GOTO1390
      DO1300I=1,NUMARG
        CALL DPUPP4(IHARLC(I),IHARG(I),IBUGTY,IERROR)
        CALL DPUPP4(IHARL2(I),IHARG2(I),IBUGTY,IERROR)
 1300 CONTINUE
 1390 CONTINUE
C
C               **********************************************************
C               **  STEP 5--                                            **
C               **  CONVERT STRINGS 1 THROUGH END TO INTEGER ARGUMENTS  **
C               **********************************************************
C
      ISTEPN='5'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMWD.LE.0)GOTO2999
      DO2000IWORD=1,NUMWD
        IWORM1=IWORD-1
C
        IF(IWORD.LE.1)THEN
          IH=ICOM
          IH2=ICOM2
        ELSE
          IH=IHARG(IWORM1)
          IH2=IHARG2(IWORM1)
        ENDIF
C
        IF(NUMNAM.LE.0)GOTO2040
        DO2010INAME=1,NUMNAM
          IF(IH.EQ.IHNAME(INAME).AND.IH2.EQ.IHNAM2(INAME))THEN
            IF(IUSE(INAME).EQ.'P')THEN
              IF(IWORM1.GT.0)IARGT(IWORM1)='NUMB'
              IF(IWORM1.GT.0)IARG(IWORM1)=IVALUE(INAME)
              GOTO2000
            ELSE
              GOTO2040
            ENDIF
          ENDIF
 2010   CONTINUE
 2040   CONTINUE
C
        IFLUNK='NO'
        IANS3=(-1)
        IWID=ISTOP(IWORD)-ISTART(IWORD)+1
        JMIN=ISTART(IWORD)
        JMAX=ISTOP(IWORD)
        I=0
        DO2100J=JMIN,JMAX
          I=I+1
          IB(I)=IANSLC(J)
 2100   CONTINUE
C
        IDIG=0
        ISIGN=0
        IDECP2=0
        ISUM=0
        DO2700I=1,IWID
          IREV=IWID-I+1
          IF(IB(IREV).EQ.' ')THEN
            GOTO2700
          ELSEIF(IB(IREV).EQ.'0')THEN
            ITERM=0
            GOTO2725
          ELSEIF(IB(IREV).EQ.'1')THEN
            ITERM=1
            GOTO2725
          ELSEIF(IB(IREV).EQ.'2')THEN
            ITERM=2
            GOTO2725
          ELSEIF(IB(IREV).EQ.'3')THEN
            ITERM=3
            GOTO2725
          ELSEIF(IB(IREV).EQ.'4')THEN
            ITERM=4
            GOTO2725
          ELSEIF(IB(IREV).EQ.'5')THEN
            ITERM=5
            GOTO2725
          ELSEIF(IB(IREV).EQ.'6')THEN
            ITERM=6
            GOTO2725
          ELSEIF(IB(IREV).EQ.'7')THEN
            ITERM=7
            GOTO2725
          ELSEIF(IB(IREV).EQ.'8')THEN
            ITERM=8
            GOTO2725
          ELSEIF(IB(IREV).EQ.'9')THEN
            ITERM=9
            GOTO2725
          ELSEIF(IB(IREV).EQ.'+')THEN
            ISIGN=ISIGN+1
            GOTO2700
          ELSEIF(IB(IREV).EQ.'-')THEN
            ISIGN=ISIGN+1
            ISUM=-ISUM
            GOTO2700
          ELSEIF(IB(IREV).EQ.'.')THEN
            IDECP2=IDECP2+1
            IF(IDECP2.EQ.1.AND.IDIG.EQ.0)GOTO2700
            GOTO2800
          ELSE
            IFLUNK='YES'
            GOTO2800
          ENDIF
C
 2725     CONTINUE
          IDIG=IDIG+1
          IF(IDIG.EQ.1)THEN
            ISUM=ISUM+ITERM
          ELSE
CCCCC       FOLLOWING FIXES WHAT APPEARS TO BE COMPILER BUG ON LAHEY 95
CCCCC       COMPILER.  MAY 2001
CCCCC       SPECIFICALLY, 10**IPOW SEEMS TO RETURN A 0.
CCCCC       ISUM=ISUM+ITERM*10**(IDIG-1)
            ITERM1=IDIG-1
            ITERM2=INT(10.0**ITERM1 + 0.01)
            ISUM=ISUM+ITERM*ITERM2
          ENDIF
C
 2700   CONTINUE
        IF(IDIG.LE.0)GOTO2800
        IF(ISIGN.GE.2)GOTO2800
        IANS3=ISUM
C
 2800   CONTINUE
        IWORM1=IWORD-1
        IF(IWORD.LE.1)ICOMI=IANS3
        IF(IWORD.GE.2)IARG(IWORM1)=IANS3
        IF(IWORD.LE.1.AND.IFLUNK.EQ.'YES')ICOMT='WORD'
        IF(IWORD.GE.2.AND.IFLUNK.EQ.'YES')IARGT(IWORM1)='WORD'
 2000 CONTINUE
 2999 CONTINUE
C
C               ***************************************************************
C               **  STEP 6--                                                 **
C               **  CONVERT STRINGS 2 THROUGH N TO FLOATING POINT ARGUMENTS  **
C               ***************************************************************
C
      ISTEPN='6'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ************************************************************
C               **  STEP 6.1--                                            **
C               **  FIRST OF ALL, LOCATE THE DECIMAL POINT (IF EXISTENT)  **
C               **  OCTOBER 1997.  CHECK FOR EXPONENTIAL NOTATION.   I.E. **
C               **  1.2E02, 1.2E-02, 1.2E+02                              **
C               ************************************************************
C
      ISTEPN='6.1'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OCTOBER 1997.  FOR EXPONENTIAL NOTATION, NEED TO ALLOW LARGER NUMBERS
CCCCC AMIN=-1000000.
CCCCC AMAX=+1000000.
      AMIN=CPUMIN
      AMAX=CPUMAX
      NUMARG=NUMWD-1
CCCCC IF(NUMARG.LE.0)GOTO3999
      IF(NUMWD.LE.0)GOTO3999
      DO3000IWORD=1,NUMWD
C
        IWORM1=IWORD-1
        IF(IWORD.LE.1)THEN
          IH=ICOM
          IH2=ICOM2
        ELSE
          IH=IHARG(IWORM1)
          IH2=IHARG2(IWORM1)
        ENDIF
C
        IF(NUMNAM.LE.0)GOTO3040
        DO3010INAME=1,NUMNAM
          IF(IH.EQ.IHNAME(INAME).AND.IH2.EQ.IHNAM2(INAME))THEN
            IF(IUSE(INAME).EQ.'P')THEN
              IF(IWORD.LE.1)ICOMT='NUMB'
              IF(IWORD.GE.2)IARGT(IWORM1)='NUMB'
              IF(IWORD.LE.1)ACOM=VALUE(INAME)
              IF(IWORD.GE.2)ARG(IWORM1)=VALUE(INAME)
              GOTO3000
            ELSE
              GOTO3040
            ENDIF
          ENDIF
 3010   CONTINUE
 3040   CONTINUE
C
        IFLUNK='NO'
        ANS2=(-1.0)
        IWID=ISTOP(IWORD)-ISTART(IWORD)+1
        JMIN=ISTART(IWORD)
        JMAX=ISTOP(IWORD)
        I=0
        DO3050J=JMIN,JMAX
          I=I+1
          IB(I)=IANSLC(J)
 3050   CONTINUE
C
        ILOC=0
        IDECP2=0
        ILOCE=0
        IEXPPT=0
        DO3060I=1,IWID
          IF(IB(I).EQ.'.')ILOC=I
          IF(IB(I).EQ.'.')IDECP2=IDECP2+1
          IF(IB(I).EQ.'E'.OR.IB(I).EQ.'e')ILOCE=I
          IF(IB(I).EQ.'E'.OR.IB(I).EQ.'e')IEXPPT=IEXPPT+1
 3060   CONTINUE
        IF(IDECP2.GE.2)GOTO3900
        IF(IEXPPT.GE.2)GOTO3900
C
        IESCAL=0
        IESIGN=1
        IWID2=IWID
        IF(ILOCE+1.GT.IWID)THEN
          IFLUNK='YES'
          GOTO3900
        ENDIF
        IF(IEXPPT.EQ.1)THEN
          IWID=ILOCE-1
          IF(IB(ILOCE+1).EQ.'-')THEN
            IESIGN=-1
            ISTRT2=ILOCE+2
          ELSEIF(IB(ILOCE+1).EQ.'+')THEN
            IESIGN=1
            ISTRT2=ILOCE+2
          ELSE
            IESIGN=1
            ISTRT2=ILOCE+1
          ENDIF
          ICOUNT=0
          ICJUNK='        '
          IF(ISTRT2.GT.IWID2)THEN
            IFLUNK='YES'
            GOTO3900
          ENDIF
          DO13065I=ISTRT2,IWID2
            IF(IB(I).EQ.' ')GOTO13065
            IF(IB(I).EQ.'0')GOTO13060
            IF(IB(I).EQ.'1')GOTO13060
            IF(IB(I).EQ.'2')GOTO13060
            IF(IB(I).EQ.'3')GOTO13060
            IF(IB(I).EQ.'4')GOTO13060
            IF(IB(I).EQ.'5')GOTO13060
            IF(IB(I).EQ.'6')GOTO13060
            IF(IB(I).EQ.'7')GOTO13060
            IF(IB(I).EQ.'8')GOTO13060
            IF(IB(I).EQ.'9')GOTO13060
            IFLUNK='YES'
            GOTO3900
C
13060       CONTINUE
            ICOUNT=ICOUNT+1
            ICJUNK(ICOUNT:ICOUNT)=IB(I)(1:1)
C
13065     CONTINUE
CCCCC     FOLLOWING TO ADDRESS BUG ON SUN.  OCTOBER 2001.
          IFRMT(1:5)='(I  )'
          IF(ICOUNT.LE.9)THEN
            WRITE(IFRMT(3:3),'(I1)')ICOUNT
          ELSE
            WRITE(IFRMT(3:4),'(I2)')ICOUNT
          ENDIF
          READ(ICJUNK(1:ICOUNT),IFRMT)IESCAL
        ENDIF
C
        IF(IDECP2.EQ.1)GOTO3150
        DO3100I=1,IWID
          IREV=IWID-I+1
          IF(IB(IREV).EQ.' ')GOTO3100
          IF(IB(IREV).EQ.'0' .OR. IB(IREV).EQ.'1' .OR.
     1           IB(IREV).EQ.'2' .OR. IB(IREV).EQ.'3' .OR.
     1           IB(IREV).EQ.'4' .OR. IB(IREV).EQ.'5' .OR.
     1           IB(IREV).EQ.'6' .OR. IB(IREV).EQ.'7' .OR.
     1           IB(IREV).EQ.'8' .OR. IB(IREV).EQ.'9')THEN
            GOTO3110
          ENDIF
          IFLUNK='YES'
          IF(IB(IREV).EQ.'+')GOTO3900
          IF(IB(IREV).EQ.'-')GOTO3900
          GOTO3900
C
 3100   CONTINUE
        IFLUNK='YES'
        GOTO3900
C
 3110   ILOC=IREV+1
 3150   CONTINUE
C
        IF(IBUGTY.NE.'OFF')THEN
          WRITE(ICOUT,3111)ILOC,IDECP2
 3111     FORMAT('ILOC = ',I8,'    IDECP2 = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               *******************************************************
C               **  STEP 6.2--                                       **
C               **  SECONDLY, COMPUTE THE INTEGER PART OF THE VALUE  **
C               *******************************************************
C
        ISTEPN='6.2'
        IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        SIGN=1.0
        IDIGI=0
        ISIGN=0
        SUMI=0
        ILOCM1=ILOC-1
        IF(ILOCM1.LT.1)GOTO3250
        DO3200I=1,ILOCM1
          IREV=ILOCM1-I+1
          IF(IB(IREV).EQ.' ')THEN
            GOTO3200
          ELSEIF(IB(IREV).EQ.'0')THEN
            ITERM=0
            GOTO3225
          ELSEIF(IB(IREV).EQ.'1')THEN
            ITERM=1
            GOTO3225
          ELSEIF(IB(IREV).EQ.'2')THEN
            ITERM=2
            GOTO3225
          ELSEIF(IB(IREV).EQ.'3')THEN
            ITERM=3
            GOTO3225
          ELSEIF(IB(IREV).EQ.'4')THEN
            ITERM=4
            GOTO3225
          ELSEIF(IB(IREV).EQ.'5')THEN
            ITERM=5
            GOTO3225
          ELSEIF(IB(IREV).EQ.'6')THEN
            ITERM=6
            GOTO3225
          ELSEIF(IB(IREV).EQ.'7')THEN
            ITERM=7
            GOTO3225
          ELSEIF(IB(IREV).EQ.'8')THEN
            ITERM=8
            GOTO3225
          ELSEIF(IB(IREV).EQ.'9')THEN
            ITERM=9
            GOTO3225
          ELSEIF(IB(IREV).EQ.'+')THEN
            ISIGN=ISIGN+1
            GOTO3200
          ELSEIF(IB(IREV).EQ.'-')THEN
            ISIGN=ISIGN+1
            SIGN=-SIGN
            GOTO3200
          ELSE
            IFLUNK='YES'
            GOTO3900
          ENDIF
C
 3225     CONTINUE
          IDIGI=IDIGI+1
          TERM=ITERM
          IEXP=IDIGI-1
          SUMI=SUMI+TERM*(10.0          **IEXP)
 3200   CONTINUE
 3250   CONTINUE
        IF(ISIGN.GE.2)GOTO3900
C
        IF(IBUGTY.NE.'OFF')THEN
          WRITE(ICOUT,3255)IDIGI,SUMI
 3255     FORMAT('IDIGI = ',I8,'    SUMI = ',F20.10)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ******************************************************
C               **  STEP 6.3--                                      **
C               **  THIRDLY, COMPUTE THE DECIMAL PART OF THE VALUE  **
C               ******************************************************
C
        ISTEPN='6.3'
        IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IDIGD=0
        SUMD=0.0
        ILOCP1=ILOC+1
        IF(ILOCP1.GT.IWID)GOTO3350
        DO3300I=ILOCP1,IWID
          IF(IB(I).EQ.' ')THEN
            GOTO3300
          ELSEIF(IB(I).EQ.'0')THEN
            ITERM=0
            GOTO3325
          ELSEIF(IB(I).EQ.'1')THEN
            ITERM=1
            GOTO3325
          ELSEIF(IB(I).EQ.'2')THEN
            ITERM=2
            GOTO3325
          ELSEIF(IB(I).EQ.'3')THEN
            ITERM=3
            GOTO3325
          ELSEIF(IB(I).EQ.'4')THEN
            ITERM=4
            GOTO3325
          ELSEIF(IB(I).EQ.'5')THEN
            ITERM=5
            GOTO3325
          ELSEIF(IB(I).EQ.'6')THEN
            ITERM=6
            GOTO3325
          ELSEIF(IB(I).EQ.'7')THEN
            ITERM=7
            GOTO3325
          ELSEIF(IB(I).EQ.'8')THEN
            ITERM=8
            GOTO3325
          ELSEIF(IB(I).EQ.'9')THEN
            ITERM=9
            GOTO3325
          ELSE
            IFLUNK='YES'
            GOTO3900
          ENDIF
C
 3325     IDIGD=IDIGD+1
          TERM=ITERM
          SUMD=SUMD+TERM/(10.0**IDIGD)
C
 3300   CONTINUE
 3350   CONTINUE
C
        IF(IBUGTY.EQ.'ON')THEN
          WRITE(ICOUT,3355)IDIGD,SUMD
 3355     FORMAT('IDIGD = ',I8,'    SUMD = ',F20.10)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IDIGT=IDIGI+IDIGD
        IF(IDIGT.LE.0)GOTO3900
        ANS2=SUMI+SUMD
        IF(SIGN.LT.0.0)ANS2=-ANS2
        ANS2=ANS2*10.0**(IESIGN*IESCAL)
        IWORM1=IWORD-1
        IF(IWORD.LE.1)ACOM=ANS2
        IF(IWORD.GE.2)ARG(IWORM1)=ANS2
CCCC    OCTOBER 1997.  IF EXPONENTIAL NUMBER, NEED TO RESET IARGT
        IF(AMIN.LE.ANS2.AND.ANS2.LE.AMAX)THEN
          IF(IWORM1.GE.1)IARGT(IWORM1)='NUMB'
          GOTO3000
        ELSE
          GOTO3900
        ENDIF
C
 3900   CONTINUE
        IF(IWORM1.LT.1) GOTO 3000
        IWORM1=IWORD-1
        ARG(IWORM1)=ANS2
        IF(IFLUNK.EQ.'YES')IARGT(IWORM1)='WORD'
 3000 CONTINUE
 3999 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGTY.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTYPE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICOM,ICOM2,ICOMT,ACOM,ICOMI
 9012   FORMAT('ICOM,ICOM2,ICOMT,ACOM,ICOMI = ',
     1         2(A4,2X),A4,E15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ICOMLC,ICOML2,NUMARG
 9013   FORMAT('ICOMLC,ICOML2,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMARG
          WRITE(ICOUT,9016)I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I)
 9016     FORMAT('I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I) = ',
     1           I6,2(1X,A4),1X,I6,1X,E15.7,1X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9017)I,IHARLC(I),IHARL2(I)
 9017     FORMAT('I,IHARLC(I),IHARL2(I) = ',I6,1X,A4,1X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9021)IHOST1,IHOST2
 9021   FORMAT('IHOST1,IHOST2 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END

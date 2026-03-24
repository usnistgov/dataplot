      SUBROUTINE DPTIQP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A TRUNCATED INFORMATIVE QUANTILE
!              (TIQ) PLOT.
!     EXAMPLES--NORMAL TIQ PLOT Y
!               LOGNORMAL TIQ PLOT Y
!               UNIFORM TIQ PLOT Y
!               GUMBEL TIQ PLOT Y
!               WEIBULL TIQ PLOT Y
!               LOGISTIC TIQ PLOT Y
!               DOUBLE EXPONENTIAL TIQ PLOT Y
!               CAUCHY TIQ PLOT Y
!               SEMICIRCULAR TIQ PLOT Y
!               COSINE TIQ PLOT Y
!               ANGLIT TIQ PLOT Y
!               HYPERBOLIC SECANT TIQ PLOT Y
!               HALF-NORMAL TIQ PLOT Y
!               ARCSINE TIQ PLOT Y
!               EXPONENTIAL TIQ PLOT Y
!               HALF-CAUCHY TIQ PLOT Y
!               SLASH TIQ PLOT Y
!               RAYLEIGH TIQ PLOT Y
!               MAXWELL TIQ PLOT Y
!
!     REFERENCE--"MIL-HDBK-17-1F Volume 1: Guidelines for Characterization
!                of Structural Materials", Depeartment of Defense,
!                chapter 8, 2002.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2017/03
!     ORIGINAL VERSION--MARCH     2017.
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
      CHARACTER*4 ICASE2
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP4(1))
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
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTI'
      ISUBN2='QP  '
      ICASPL='TIQP'
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
!               **  TREAT THE TIQ            PLOT                **
!               ***************************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTIQP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,NS
   52   FORMAT('ICASPL,IAND1,IAND2,NS = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  EXTRACT THE COMMAND                             **
!               **  REPLICATION AND MULTIPLE NOT SUPPORTED FOR THIS **
!               **  COMMAND.                                        **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
                                                                                                                                  
      IF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
         IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='NORM'
      ELSEIF(ICOM.EQ.'NORM' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='NORM'
      ELSEIF(ICOM.EQ.'LOGN' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='LOGN'
      ELSEIF(ICOM.EQ.'LOGN' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='LOGN'
      ELSEIF(ICOM.EQ.'WEIB' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='WEIB'
      ELSEIF(ICOM.EQ.'WEIB' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='WEIB'
      ELSEIF(ICOM.EQ.'GUMB' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='GUMB'
      ELSEIF(ICOM.EQ.'GUMB' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='GUMB'
      ELSEIF(ICOM.EQ.'EXTR' .AND. IHARG(1).EQ.'VALU' .AND.   &
             IHARG(2).EQ.'TYPE' .AND.   &
            (IHARG(3).EQ.'1   ' .OR. IHARG(3).EQ.'I   ')  .AND.   &
             IHARG(4).EQ.'TIQ ' .AND.   &
             IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='GUMB'
      ELSEIF(ICOM.EQ.'EXTR' .AND. IHARG(1).EQ.'VALU' .AND.   &
             IHARG(2).EQ.'TYPE' .AND.   &
            (IHARG(3).EQ.'1   ' .OR.  IHARG(3).EQ.'I   ')  .AND.   &
             IHARG(4).EQ.'TRUN' .AND. IHARG(5).EQ.'INFO' .AND.   &
             IHARG(6).EQ.'QUAN' .AND. IHARG(7).EQ.'PLOT')THEN
         ILASTC=7
         ICASE='GUMB'
      ELSEIF(ICOM.EQ.'UNIF' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='UNIF'
      ELSEIF(ICOM.EQ.'UNIF' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='UNIF'
      ELSEIF(ICOM.EQ.'LOGI' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='LOGI'
      ELSEIF(ICOM.EQ.'LOGI' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='LOGI'
      ELSEIF(ICOM.EQ.'EXPO' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='EXPO'
      ELSEIF(ICOM.EQ.'EXPO' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='EXPO'
      ELSEIF(ICOM.EQ.'ARCS' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='ARCS'
      ELSEIF(ICOM.EQ.'ARCS' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='ARCS'
      ELSEIF(ICOM.EQ.'ANGL' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='ANGL'
      ELSEIF(ICOM.EQ.'ANGL' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='ANGL'
      ELSEIF(ICOM.EQ.'COSI' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='COSI'
      ELSEIF(ICOM.EQ.'COSI' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='COSI'
      ELSEIF(ICOM.EQ.'CAUC' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='CAUC'
      ELSEIF(ICOM.EQ.'CAUC' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='CAUC'
      ELSEIF(ICOM.EQ.'SLAS' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='SLAS'
      ELSEIF(ICOM.EQ.'SLAS' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='SLAS'
      ELSEIF(ICOM.EQ.'SLAS' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='SLAS'
      ELSEIF(ICOM.EQ.'SLAS' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='SLAS'
      ELSEIF(ICOM.EQ.'RAYL' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='RAYL'
      ELSEIF(ICOM.EQ.'RAYL' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='RAYL'
      ELSEIF(ICOM.EQ.'MAXW' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='MAXW'
      ELSEIF(ICOM.EQ.'MAXW' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='MAXW'
      ELSEIF(ICOM.EQ.'HALF' .AND. IHARG(1).EQ.'NORM' .AND.   &
             IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='HANO'
      ELSEIF(ICOM.EQ.'HALF' .AND. IHARG(1).EQ.'NORM' .AND.   &
             IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.   &
             IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='HANO'
      ELSEIF(ICOM.EQ.'HALF' .AND. IHARG(1).EQ.'CAUC' .AND.   &
             IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='HACA'
      ELSEIF(ICOM.EQ.'HALF' .AND. IHARG(1).EQ.'CAUC' .AND.   &
             IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.   &
             IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='HACO'
      ELSEIF(ICOM.EQ.'HYPE' .AND. IHARG(1).EQ.'SECA' .AND.   &
             IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='HSE '
      ELSEIF(ICOM.EQ.'HYPE' .AND. IHARG(1).EQ.'SECA' .AND.   &
             IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.   &
             IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='HSE '
      ELSEIF(ICOM.EQ.'LAPL' .AND. IHARG(1).EQ.'TIQ ' .AND.   &
             IHARG(2).EQ.'PLOT')THEN
         ILASTC=2
         ICASE='DEX '
      ELSEIF(ICOM.EQ.'LAPL' .AND. IHARG(1).EQ.'TRUN' .AND.   &
             IHARG(2).EQ.'INFO' .AND. IHARG(3).EQ.'QUAN' .AND.   &
             IHARG(4).EQ.'PLOT')THEN
         ILASTC=4
         ICASE='DEX '
      ELSEIF(ICOM.EQ.'DOUB' .AND. IHARG(1).EQ.'EXPO' .AND.   &
             IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='DEX '
      ELSEIF(ICOM.EQ.'DOUB' .AND. IHARG(1).EQ.'EXPO' .AND.   &
             IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.   &
             IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='DEX '
      ELSEIF(ICOM.EQ.'SEMI' .AND. IHARG(1).EQ.'CIRC' .AND.   &
             IHARG(2).EQ.'TIQ ' .AND. IHARG(3).EQ.'PLOT')THEN
         ILASTC=3
         ICASE='SEMC'
      ELSEIF(ICOM.EQ.'SEMI' .AND. IHARG(1).EQ.'CIRC' .AND.   &
             IHARG(2).EQ.'TRUN' .AND. IHARG(3).EQ.'INFO' .AND.   &
             IHARG(4).EQ.'QUAN' .AND. IHARG(5).EQ.'PLOT')THEN
         ILASTC=5
         ICASE='SEMC'
      ELSE
        GO TO 9000
      ENDIF
!
      IFOUND='YES'
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'TIQP')THEN
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!               ********************************************
!               **  STEP 6--                              **
!               **  GENERATE THE TIQ            PLOTS FOR **
!               **  THE VARIOUS CASES.                    **
!               ********************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************
!               **  STEP 8A--                           **
!               ******************************************
!
      ISTEPN='8A'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TIQP')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NPLOTP=0
      ICOL=1
      NUMVA2=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,TEMP1,TEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE2,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************************
!               **  STEP 8B--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!               *****************************************************
!
      CALL DPTIQ2(Y1,TEMP1,TEMP2,TEMP3,TEMP4,NLOCAL,   &
                  ICASE,MINMAX,   &
                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9015 I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPTIQP
      SUBROUTINE DPTIQ2(Y,AIQHAT,TIQHAT,UTEMP,QUHAT,N,   &
                        ICASPL,MINMAX,   &
                        Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A TRUNCATED INFORMATIVE QUANTILE
!              (TIQ) PLOT.
!
!     REFERENCE--"MIL-HDBK-17-1F Volume 1: Guidelines for Characterization
!                of Structural Materials", Depeartment of Defense,
!                chapter 8, 2002.
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
!     VERSION NUMBER--2017/03
!     ORIGINAL VERSION--MARCH     2017.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION UTEMP(*)
      DIMENSION AIQHAT(*)
      DIMENSION TIQHAT(*)
      DIMENSION QUHAT(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTI'
      ISUBN2='Q2  '
      IWRITE='OFF '
      IERROR='NO'
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TIQ2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPTIQ2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,MINMAX,ICASPL
   72   FORMAT('N,MINMAX,ICASPL = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 73 I=1,N
          WRITE(ICOUT,74)I,Y(I)
   74     FORMAT('I, Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
!               **********************************************
!               **  STEP 2--                                **
!               **  FOR WEIBULL AND LOGNORMAL, NEED TO TAKE **
!               **  LOG OF THE DATA.                        **
!               **********************************************
!
      IF(ICASPL.EQ.'WEIB' .OR. ICASPL.EQ.'LOGN')THEN
        DO 200 I=1,N
          IF(Y(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,202)I
  202       FORMAT('      ROW ',I8,' OF THE RESPONSE VARIABLE IS ',   &
                   'NON-POSITIVE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,204)Y(I)
  204       FORMAT('      IT HAS THE VALUE ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          Y(I)=LOG(Y(I))
  200   CONTINUE
      ENDIF
!
!               **********************************************
!               **  STEP 3--                                **
!               **  CALL EMPQUA ROUTINE TO COMPUTE THE      **
!               **  TRUNCATED INFORMATIVE QUANTILE FUNCTION **
!               **********************************************
!
!
      CALL EMPTIQ(Y,N,IWRITE,AIQHAT,TIQHAT,UTEMP,QUHAT,NOUT,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      DO 310 I=1,NOUT
        N2=N2+1
        Y2(N2)=TIQHAT(I)
        X2(N2)=100.0*UTEMP(I)
        D2(N2)=1.0
  310 CONTINUE
!
!               **********************************************
!               **  STEP 4--                                **
!               **  NOW COMPUTE THE TRUNCATED INFORMATIVE   **
!               **  QUANTILE FUNCTION FOR A THEORETICAL     **
!               **  DISTRIBUTION.                           **
!               **********************************************
!
!
      U25=0.25
      U50=0.50
      U75=0.75
      PINC=0.01
!
      IF(ICASPL.EQ.'NORM' .OR. ICASPL.EQ.'LOGN')THEN
        CALL NORPPF(U25,QU25)
        CALL NORPPF(U50,QU50)
        CALL NORPPF(U75,QU75)
        PVAL=0.01
        DO 401 I=1,99
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
        DO 403 I=1,99
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
        DO 405 I=1,99
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
        DO 411 I=1,99
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
        DO 413 I=1,99
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
        DO 415 I=1,99
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
        DO 417 I=1,99
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
        DO 419 I=1,99
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
        DO 421 I=1,99
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
        DO 423 I=1,99
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
        DO 425 I=1,99
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
        DO 427 I=1,99
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
        DO 429 I=1,99
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
        DO 431 I=1,99
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
        DO 433 I=1,99
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
        DO 435 I=1,99
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
        DO 437 I=1,99
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
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TIQ2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTIQ2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,IERROR,N2
 9012   FORMAT('ICASPL,IERROR,N2 = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTIQ2
      SUBROUTINE DPTISC(ICOM,IHARG,NUMARG,   &
      IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC SCALES CONTAINED IN THE
!              4 VARIABLES IX1TSC,IX2TSC,IY1TSC,IY2TSC  .
!              SUCH TIC SCALE SWITCHES DEFINE THE SCALES
!              (LINEAR OR WEIBULL OR NORMAL)
!              FOR THE TICS ON THE 4 FRAME LINES OF A PLOT.
!     FOCUS OF SUBROUTINE DPTISC--LOG
!                         DPTIS2--WEIBULL
!                         DPTIS3--NORMAL
!
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--
!                     --IX1TSC = LOWER HORIZONTAL TIC SCALE
!                     --IX2TSC = UPPER HORIZONTAL TIC SCALE
!                     --IY1TSC = LEFT  VERTICAL   TIC SCALE
!                     --IY2TSC = RIGHT VERTICAL   TIC SCALE
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
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORN')GO TO 1900
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                           **
!               **  BOTH HORIZONTAL LOG SCALES  ARE TO BE LOG      **
!               *****************************************************
!
      IF(ICOM.EQ.'XLOG')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(NUMARG.LE.0)GO TO 1110
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1120
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
      IERROR='YES'
      GO TO 1900
!
 1110 CONTINUE
      IFOUND='YES'
      IX1TSC='LOG'
      IX2TSC='LOG'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1115)
 1115 FORMAT('THE XLOG SWITCH (FOR BOTH HORIZONTAL LOG SCALES ) ',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 1900
!
 1120 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('THE XLOG SWITCH (FOR BOTH HORIZONTAL LOG SCALES ) ',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL FRAME LINE IS TO BE LOG      **
!               **************************************************************
!
      IF(ICOM.EQ.'X1LO')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(NUMARG.LE.0)GO TO 1210
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1210
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1220
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1210
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1210
      IERROR='YES'
      GO TO 1900
!
 1210 CONTINUE
      IFOUND='YES'
      IX1TSC='LOG'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('THE X1LOG   SWITCH (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1219 CONTINUE
      GO TO 1900
!
 1220 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1225)
 1225 FORMAT('THE X1LOG   SWITCH (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL FRAME LINE IS TO BE LOG      **
!               **************************************************************
!
      IF(ICOM.EQ.'X2LO')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(NUMARG.LE.0)GO TO 1310
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1310
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1320
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1310
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1310
      IERROR='YES'
      GO TO 1900
!
 1310 CONTINUE
      IFOUND='YES'
      IX2TSC='LOG'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1315)
 1315 FORMAT('THE X2LOG   SWITCH (FOR THE TOP HORIZONTAL ',   &
      'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      GO TO 1900
!
 1320 CONTINUE
      IFOUND='YES'
      IX2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1329
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1325)
 1325 FORMAT('THE X2LOG   SWITCH (FOR THE TOP HORIZONTAL ',   &
      'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1329 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               ***************************************************
!               **  TREAT THE CASE WHEN                          **
!               **  BOTH VERTICAL LOG SCALES  ARE TO BE LOG      **
!               ***************************************************
!
      IF(ICOM.EQ.'YLOG')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(NUMARG.LE.0)GO TO 1410
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1410
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1420
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1410
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1410
      IERROR='YES'
      GO TO 1900
!
 1410 CONTINUE
      IFOUND='YES'
      IY1TSC='LOG'
      IY2TSC='LOG'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1415)
 1415 FORMAT('THE YLOG   SWITCH (FOR BOTH VERTICAL LOG SCALES ) ',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1419 CONTINUE
      GO TO 1900
!
 1420 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1425)
 1425 FORMAT('THE YLOG   SWITCH (FOR BOTH VERTICAL LOG SCALES ) ',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1429 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   FRAME LINE IS TO BE LOG      **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1LO')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(NUMARG.LE.0)GO TO 1510
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1510
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1520
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1510
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1510
      IERROR='YES'
      GO TO 1900
!
 1510 CONTINUE
      IFOUND='YES'
      IY1TSC='LOG'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1519
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1515)
 1515 FORMAT('THE Y1LOG   SWITCH (FOR THE LEFT VERTICAL ',   &
      'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1519 CONTINUE
      GO TO 1900
!
 1520 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1529
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1525)
 1525 FORMAT('THE Y1LOG   SWITCH (FOR THE LEFT VERTICAL ',   &
      'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1529 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTCIAL   FRAME LINE IS TO BE LOG      **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2LO')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(NUMARG.LE.0)GO TO 1610
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1610
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1620
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1610
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1610
      IERROR='YES'
      GO TO 1900
!
 1610 CONTINUE
      IFOUND='YES'
      IY2TSC='LOG'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1619
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1615)
 1615 FORMAT('THE Y2LOG   SWITCH (FOR THE RIGHT VERTICAL ',   &
      'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1619 CONTINUE
      GO TO 1900
!
 1620 CONTINUE
      IFOUND='YES'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1625)
 1625 FORMAT('THE Y2LOG   SWITCH (FOR THE RIGHT VERTICAL ',   &
      'FRAME LOG SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1629 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               **************************************************
!               **  TREAT THE CASE WHEN                         **
!               **  THE ENTIRE 4-SIDED FRAME IS TO BE LOG       **
!               **************************************************
!
      IF(ICOM.EQ.'XYLO')GO TO 1700
      IF(ICOM.EQ.'YXLO')GO TO 1700
      IF(ICOM.EQ.'LOG ')GO TO 1700
      IF(ICOM.EQ.'LOGL')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(NUMARG.LE.0)GO TO 1710
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1710
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1720
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1710
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1710
      IERROR='YES'
      GO TO 1900
!
 1710 CONTINUE
      IFOUND='YES'
      IX1TSC='LOG'
      IX2TSC='LOG'
      IY1TSC='LOG'
      IY2TSC='LOG'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1719
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1715)
 1715 FORMAT('THE LOG   SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1719 CONTINUE
      GO TO 1900
!
 1720 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
      IY1TSC='LINE'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1729
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1725)
 1725 FORMAT('THE LOG   SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1729 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTISC
      SUBROUTINE DPTIS2(ICOM,IHARG,NUMARG,   &
      IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC SCALES CONTAINED IN THE
!              4 VARIABLES IX1TSC,IX2TSC,IY1TSC,IY2TSC  .
!              SUCH TIC SCALE SWITCHES DEFINE THE SCALES
!              (LINEAR OR WEIBULL OR NORMAL)
!              FOR THE TICS ON THE 4 FRAME LINES OF A PLOT.
!     FOCUS OF SUBROUTINE DPTISC--LOG
!                         DPTIS2--WEIBULL
!                         DPTIS3--NORMAL
!
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--
!                     --IX1TSC = LOWER HORIZONTAL TIC SCALE
!                     --IX2TSC = UPPER HORIZONTAL TIC SCALE
!                     --IY1TSC = LEFT  VERTICAL   TIC SCALE
!                     --IY2TSC = RIGHT VERTICAL   TIC SCALE
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
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORN')GO TO 1900
!
!               ********************************************************
!               **  TREAT THE CASE WHEN                               **
!               **  BOTH HORIZONTAL FRAME LINES     ARE TO BE WEIBULL **
!               ********************************************************
!
      IF(ICOM.EQ.'XWEI')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(NUMARG.LE.0)GO TO 1110
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1120
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
      IERROR='YES'
      GO TO 1900
!
 1110 CONTINUE
      IFOUND='YES'
      IX1TSC='WEIB'
      IX2TSC='WEIB'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1115)
 1115 FORMAT('THE XWEIB SWITCH (FOR BOTH HORIZ. WEIBULL SCALES)',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 1900
!
 1120 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('THE XWEIB SWITCH (FOR BOTH HORIZ. WEIBULL SCALES)',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               ********************************************************
!               **  TREAT THE CASE WHEN
!               **  ONLY THE BOTTOM HORIZONTAL FRAME LINE IS TO BE WEIBU
!               ********************************************************
!
      IF(ICOM.EQ.'X1WE')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(NUMARG.LE.0)GO TO 1210
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1210
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1220
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1210
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1210
      IERROR='YES'
      GO TO 1900
!
 1210 CONTINUE
      IFOUND='YES'
      IX1TSC='WEIB'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('THE X1WEIB   SWITCH (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1219 CONTINUE
      GO TO 1900
!
 1220 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1225)
 1225 FORMAT('THE X1WEIB   SWITCH (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               ********************************************************
!               **  TREAT THE CASE WHEN
!               **  ONLY THE TOP    HORIZONTAL FRAME LINE IS TO BE WEIBU
!               ********************************************************
!
      IF(ICOM.EQ.'X2WE')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(NUMARG.LE.0)GO TO 1310
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1310
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1320
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1310
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1310
      IERROR='YES'
      GO TO 1900
!
 1310 CONTINUE
      IFOUND='YES'
      IX2TSC='WEIB'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1315)
 1315 FORMAT('THE X2WEIB   SWITCH (FOR THE TOP HORIZONTAL ',   &
      'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      GO TO 1900
!
 1320 CONTINUE
      IFOUND='YES'
      IX2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1329
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1325)
 1325 FORMAT('THE X2WEIB   SWITCH (FOR THE TOP HORIZONTAL ',   &
      'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1329 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               ******************************************************
!               **  TREAT THE CASE WHEN                             **
!               **  BOTH VERTICAL FRAME LINES     ARE TO BE WEIBULL **
!               ******************************************************
!
      IF(ICOM.EQ.'YWEI')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(NUMARG.LE.0)GO TO 1410
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1410
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1420
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1410
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1410
      IERROR='YES'
      GO TO 1900
!
 1410 CONTINUE
      IFOUND='YES'
      IY1TSC='WEIB'
      IY2TSC='WEIB'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1415)
 1415 FORMAT('THE YWEIB   SWITCH (FOR BOTH VERT. WEIBULL SCALES)',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1419 CONTINUE
      GO TO 1900
!
 1420 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1425)
 1425 FORMAT('THE YWEIB   SWITCH (FOR BOTH VERT. WEIBULL SCALES)',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1429 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               ********************************************************
!               **  TREAT THE CASE WHEN
!               **  ONLY THE LEFT   VERTICAL   FRAME LINE IS TO BE WEIBU
!               ********************************************************
!
      IF(ICOM.EQ.'Y1WE')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(NUMARG.LE.0)GO TO 1510
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1510
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1520
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1510
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1510
      IERROR='YES'
      GO TO 1900
!
 1510 CONTINUE
      IFOUND='YES'
      IY1TSC='WEIB'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1519
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1515)
 1515 FORMAT('THE Y1WEIB   SWITCH (FOR THE LEFT VERTICAL ',   &
      'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1519 CONTINUE
      GO TO 1900
!
 1520 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1529
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1525)
 1525 FORMAT('THE Y1WEIB   SWITCH (FOR THE LEFT VERTICAL ',   &
      'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1529 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               ********************************************************
!               **  TREAT THE CASE WHEN
!               **  ONLY THE RIGHT  VERTCIAL   FRAME LINE IS TO BE WEIBU
!               ********************************************************
!
      IF(ICOM.EQ.'Y2WE')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(NUMARG.LE.0)GO TO 1610
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1610
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1620
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1610
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1610
      IERROR='YES'
      GO TO 1900
!
 1610 CONTINUE
      IFOUND='YES'
      IY2TSC='WEIB'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1619
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1615)
 1615 FORMAT('THE Y2WEIB   SWITCH (FOR THE RIGHT VERTICAL ',   &
      'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1619 CONTINUE
      GO TO 1900
!
 1620 CONTINUE
      IFOUND='YES'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1625)
 1625 FORMAT('THE Y2WEIB   SWITCH (FOR THE RIGHT VERTICAL ',   &
      'FRAME WEIBULL SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1629 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               **************************************************
!               **  TREAT THE CASE WHEN                         **
!               **  THE ENTIRE 4-SIDED FRAME IS TO BE WEIBULL       **
!               **************************************************
!
      IF(ICOM.EQ.'XYWE')GO TO 1700
      IF(ICOM.EQ.'YXWE')GO TO 1700
      IF(ICOM.EQ.'WEIB')GO TO 1700
!CCCC IF(ICOM.EQ.'WEIW'GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(NUMARG.LE.0)GO TO 1710
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1710
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1720
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1710
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1710
      IERROR='YES'
      GO TO 1900
!
 1710 CONTINUE
      IFOUND='YES'
      IX1TSC='WEIB'
      IX2TSC='WEIB'
      IY1TSC='WEIB'
      IY2TSC='WEIB'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1719
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1715)
 1715 FORMAT('THE WEIBULL   SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1719 CONTINUE
      GO TO 1900
!
 1720 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
      IY1TSC='LINE'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1729
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1725)
 1725 FORMAT('THE WEIBULL   SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1729 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTIS2
      SUBROUTINE DPTIS3(ICOM,IHARG,NUMARG,   &
      IX1TSC,IX2TSC,IY1TSC,IY2TSC,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC SCALES CONTAINED IN THE
!              4 VARIABLES IX1TSC,IX2TSC,IY1TSC,IY2TSC  .
!              SUCH TIC SCALE SWITCHES DEFINE THE SCALES
!              (LINEAR OR WEIBULL OR NORMAL)
!              FOR THE TICS ON THE 4 FRAME LINES OF A PLOT.
!     FOCUS OF SUBROUTINE DPTISC--LOG
!                         DPTIS2--WEIBULL
!                         DPTIS3--NORMAL
!
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--
!                     --IX1TSC = LOWER HORIZONTAL TIC SCALE
!                     --IX2TSC = UPPER HORIZONTAL TIC SCALE
!                     --IY1TSC = LEFT  VERTICAL   TIC SCALE
!                     --IY2TSC = RIGHT VERTICAL   TIC SCALE
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
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORN')GO TO 1900
!
!               ********************************************************
!               **  TREAT THE CASE WHEN                               **
!               **  BOTH HORIZONTAL FRAME LINES     ARE TO BE NORMAL  **
!               ********************************************************
!
      IF(ICOM.EQ.'XNOR')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(NUMARG.LE.0)GO TO 1110
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1120
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
      IERROR='YES'
      GO TO 1900
!
 1110 CONTINUE
      IFOUND='YES'
      IX1TSC='NORM'
      IX2TSC='NORM'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1115)
 1115 FORMAT('THE XNORM SWITCH (FOR BOTH HORIZ. NORMAL  SCALES)',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 1900
!
 1120 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('THE XNORM SWITCH (FOR BOTH HORIZ. NORMAL  SCALES)',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               ********************************************************
!               **  TREAT THE CASE WHEN
!               **  ONLY THE BOTTOM HORIZONTAL FRAME LINE IS TO BE NOR
!               ********************************************************
!
      IF(ICOM.EQ.'X1NO')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(NUMARG.LE.0)GO TO 1210
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1210
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1220
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1210
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1210
      IERROR='YES'
      GO TO 1900
!
 1210 CONTINUE
      IFOUND='YES'
      IX1TSC='NORM'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('THE X1NORMAL SWITCH (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1219 CONTINUE
      GO TO 1900
!
 1220 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1225)
 1225 FORMAT('THE X1NORMAL SWITCH (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               ********************************************************
!               **  TREAT THE CASE WHEN
!               **  ONLY THE TOP    HORIZONTAL FRAME LINE IS TO BE NORM
!               ********************************************************
!
      IF(ICOM.EQ.'X2NO')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(NUMARG.LE.0)GO TO 1310
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1310
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1320
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1310
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1310
      IERROR='YES'
      GO TO 1900
!
 1310 CONTINUE
      IFOUND='YES'
      IX2TSC='NORM'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1315)
 1315 FORMAT('THE X2NORMAL SWITCH (FOR THE TOP HORIZONTAL ',   &
      'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      GO TO 1900
!
 1320 CONTINUE
      IFOUND='YES'
      IX2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1329
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1325)
 1325 FORMAT('THE X2NORMAL SWITCH (FOR THE TOP HORIZONTAL ',   &
      'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1329 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               ******************************************************
!               **  TREAT THE CASE WHEN                             **
!               **  BOTH VERTICAL FRAME LINES     ARE TO BE NORMAL  **
!               ******************************************************
!
      IF(ICOM.EQ.'YNOR')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(NUMARG.LE.0)GO TO 1410
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1410
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1420
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1410
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1410
      IERROR='YES'
      GO TO 1900
!
 1410 CONTINUE
      IFOUND='YES'
      IY1TSC='NORM'
      IY2TSC='NORM'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1415)
 1415 FORMAT('THE YNORM   SWITCH (FOR BOTH VERT. NORMAL  SCALES)',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1419 CONTINUE
      GO TO 1900
!
 1420 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1425)
 1425 FORMAT('THE YNORM   SWITCH (FOR BOTH VERT. NORMAL  SCALES)',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1429 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               ********************************************************
!               **  TREAT THE CASE WHEN
!               **  ONLY THE LEFT   VERTICAL   FRAME LINE IS TO BE NORM
!               ********************************************************
!
      IF(ICOM.EQ.'Y1NO')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(NUMARG.LE.0)GO TO 1510
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1510
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1520
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1510
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1510
      IERROR='YES'
      GO TO 1900
!
 1510 CONTINUE
      IFOUND='YES'
      IY1TSC='NORM'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1519
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1515)
 1515 FORMAT('THE Y1NORMAL SWITCH (FOR THE LEFT VERTICAL ',   &
      'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1519 CONTINUE
      GO TO 1900
!
 1520 CONTINUE
      IFOUND='YES'
      IY1TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1529
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1525)
 1525 FORMAT('THE Y1NORMAL SWITCH (FOR THE LEFT VERTICAL ',   &
      'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1529 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               ********************************************************
!               **  TREAT THE CASE WHEN
!               **  ONLY THE RIGHT  VERTCIAL   FRAME LINE IS TO BE NORM
!               ********************************************************
!
      IF(ICOM.EQ.'Y2NO')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(NUMARG.LE.0)GO TO 1610
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1610
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1620
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1610
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1610
      IERROR='YES'
      GO TO 1900
!
 1610 CONTINUE
      IFOUND='YES'
      IY2TSC='NORM'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1619
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1615)
 1615 FORMAT('THE Y2NORMAL SWITCH (FOR THE RIGHT VERTICAL ',   &
      'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1619 CONTINUE
      GO TO 1900
!
 1620 CONTINUE
      IFOUND='YES'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1625)
 1625 FORMAT('THE Y2NORMAL SWITCH (FOR THE RIGHT VERTICAL ',   &
      'FRAME NORMAL  SCALE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1629 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               **************************************************
!               **  TREAT THE CASE WHEN                         **
!               **  THE ENTIRE 4-SIDED FRAME IS TO BE NORMAL        **
!               **************************************************
!
      IF(ICOM.EQ.'XYNO')GO TO 1700
      IF(ICOM.EQ.'YXNO')GO TO 1700
!CCCC IF(ICOM.EQ.'NORM')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(NUMARG.LE.0)GO TO 1710
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1710
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1720
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1710
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1710
      IERROR='YES'
      GO TO 1900
!
 1710 CONTINUE
      IFOUND='YES'
      IX1TSC='NORM'
      IX2TSC='NORM'
      IY1TSC='NORM'
      IY2TSC='NORM'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1719
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1715)
 1715 FORMAT('THE NORMAL    SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',   &
      'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1719 CONTINUE
      GO TO 1900
!
 1720 CONTINUE
      IFOUND='YES'
      IX1TSC='LINE'
      IX2TSC='LINE'
      IY1TSC='LINE'
      IY2TSC='LINE'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1729
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1725)
 1725 FORMAT('THE NORMAL    SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',   &
      'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1729 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTIS3
      SUBROUTINE DPTISZ(IHARG,IARGT,ARG,NUMARG,   &
      PDEFHE,PDEFWI,   &
      PTITHE,PTITWI,PTITVG,PTITHG,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE SIZE FOR THE TITLE
!              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
!              THE SIZE FOR THE TITLE WILL BE PLACED
!              IN THE FLOATING POINT VARIABLE PTITHE.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --PDEFHE
!                     --PDEFWI
!     OUTPUT ARGUMENTS--PTITHE = TITLE HEIGHT
!                     --PTITWI = TITLE WIDTH
!                     --PTITVG = TITLE VERTICAL GAP
!                     --PTITHG = TITLE HORIZONTAL GAP
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
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1988.  DEFAULT WIDTH
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1199
      IF(IHARG(1).NE.'SIZE')GO TO 1199
      IF(NUMARG.EQ.1)GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1110
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
!
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPTISZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR TITLE SIZE ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
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
      GO TO 9000
!
 1150 CONTINUE
      PTITHE=PDEFHE
      PTITWI=PDEFWI
      GO TO 1180
!
 1160 CONTINUE
      PTITHE=ARG(NUMARG)
      PTITWI=PTITHE*0.5
      PTITVG=PTITHE*0.375
      PTITHG=PTITHE*0.125
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)PTITHE
 1181 FORMAT('THE TITLE SIZE HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPTISZ
      SUBROUTINE DPTIT(IANS,IANSLC,IWIDTH,IHARG,IHARG2,NUMARG,   &
                       ITITTE,NCTITL,ITIAUT,IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--EXTRACT THE STRING TO BE USED AS A TITLE;
!              SAVE THIS STRING FOR USE ON PRINTER PLOTS;
!              ALSO, CONVERT THIS STRING INTO PROPER FORM
!              (ASCII INTEGER REPRESENTATION) FOR USE
!              WITH TEKTRONIX (OR EQUIVALENT) SOFTWARE.
!     INPUT  ARGUMENTS--IANS   (A  CHARACTER VECTOR)
!                     --IWIDTH
!                     --IHARG  (A  CHARACTER VECTOR)
!                     --IHARG2  (A  CHARACTER VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--ITITTE (A CHARACTER VECTOR
!                              CONTAINING THE STRING FOR THE TITLE).
!                     --NCTITL  (AN INTEGER VARIABLE
!                              CONTAINING THE
!                              NUMBER OF CHARACTERS IN THE TITLE).
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
!     ORIGINAL VERSION--JANUARY    1978.
!     UPDATED         --JUNE       1978.
!     UPDATED         --JUNE       1979.
!     UPDATED         --SEPTEMBER  1980.
!     UPDATED         --MARCH      1981.
!     UPDATED         --DECEMBER   1981.
!     UPDATED         --MAY        1982.
!     UPDATED         --AUGUST     1992. ADD TITLE SWITCH
!                                        FOR AUTOMATIC
!     UPDATED         --SEPTEMBER  1993. ALLOW LOWER CASE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IANS
!CCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
!CCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      CHARACTER*4 IANSLC
      CHARACTER*4 IHARG
!CCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
      CHARACTER*4 IHARG2
!
      CHARACTER*4 ITITTE
!
!CCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
      CHARACTER*4 ITIAUT
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IANS(*)
!CCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
!CCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      DIMENSION IANSLC(*)
      DIMENSION IHARG(*)
!CCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
      DIMENSION IHARG2(*)
!
      DIMENSION ITITTE(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(IBUGP2.NE.'ON')GO TO 90
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
!
!     *****************************************
!     **  STEP 1--                           **
!     **  DETERMINE THE COMMAND              **
!     **  (TITLE) AND ITS LOCATION           **
!     **  ON THE LINE.                       **
!     *****************************************
!
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'COLO')GO TO 9000
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'COLO')GO TO 9000
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'SIZE')GO TO 9000
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'SIZE')GO TO 9000
!
      DO 1000 I=1,IWIDTH
      I2=I
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
      IP6=I+6
      IF(IANS(I).EQ.'T'.AND.IANS(IP1).EQ.'I'   &
      .AND.IANS(IP2).EQ.'T'.AND.IANS(IP3).EQ.'L'   &
      .AND.IANS(IP4).EQ.'E')   &
      GO TO 100
!
 1000 CONTINUE
      WRITE(ICOUT,1001)
 1001 FORMAT('***** ERROR IN DPTIT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1002)
 1002 FORMAT('      NO MATCH FOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 800
!
!     **********************************************************
!     **  STEP 2--                                            **
!     **  DEFINE THE START POSITION (ISTART) FOR THE STRING.  **
!     **********************************************************
!
  100 CONTINUE
      ISTART=I2+6
      GO TO 300
!
!     ********************************************************
!     **  STEP 3--                                          **
!     **  DEFINE THE STOP POSITION (ISTOP) FOR THE STRING.  **
!     ********************************************************
!
  300 CONTINUE
      IFOUND='YES'
      ISTOP=0
      IF(ISTART.GT.IWIDTH)GO TO 329
      DO 320 I=ISTART,IWIDTH
      IREV=IWIDTH-I+ISTART
      IF(IANS(IREV).NE.' ')GO TO 325
  320 CONTINUE
      GO TO 329
  325 CONTINUE
      ISTOP=IREV
  329 CONTINUE
!
!     *****************************************
!     **  STEP 4--                           **
!     **  COPY OVER THE STRING OF INTEREST.  **
!     *****************************************
!
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'ON')GO TO 359
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'OFF')GO TO 359
!CCCC IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'AUTO')GO TO 359
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'DEFA')GO TO 359
      IF(NUMARG.EQ.0)GO TO 359
!
      IF(ISTART.GT.ISTOP)GO TO 359
      IF(ISTOP.EQ.0)GO TO 359
      J=0
      DO 350 I=ISTART,ISTOP
      J=J+1
!CCCC THE FOLLOWING LINE WAS   CHANGED   SEPTEMBER 1993
!CCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
!CCCC ITITTE(J)=IANS(I)
      ITITTE(J)=IANSLC(I)
  350 CONTINUE
      NCTITL=J
      GO TO 800
  359 CONTINUE
!
!     ************************************
!     **  STEP 5--                      **
!     **  TREAT THE EMPTY-STRING CASE.  **
!     ************************************
!
      NCTITL=0
      GO TO 800
!
!     ***************************
!     **  STEP 6--             **
!     **  PRINT OUT A MESSAGE  **
!     ***************************
!
  800 CONTINUE
      ILENT=NCTITL
!
!CCCC THE FOLLOWING 6 LINES WERE ADDED AUGUST 1992
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'AUTO'.AND.   &
      IHARG2(1).EQ.'MATI')THEN
         ITIAUT='ON'
      ELSE
         ITIAUT='OFF'
      ENDIF
      IF(IFEEDB.EQ.'OFF')GO TO 889
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
      GO TO 9000
!
!     ****************
!     **  STEP 90-- **
!     **  EXIT      **
!     ****************
!
 9000 CONTINUE
      IF(IBUGP2.NE.'ON')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPTIT
      SUBROUTINE DPTIDS(IHARG,ARG,NUMARG,PDEFDS,PTITDS,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE DISPLACEMENT FOR THE TITLE
!              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
!              THE DISPLACEMENT FOR THE TITLE WILL BE PLACED
!              IN THE REAL VARIABLE PTITDS.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --ARG    (A  REAL VECTOR)
!                     --NUMARG
!                     --PDEFDS
!     OUTPUT ARGUMENTS--PTITDS
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
!     VERSION NUMBER--89/8
!     ORIGINAL VERSION--JULY      1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1199
      IF(IHARG(1).EQ.'DISP')GO TO 1110
      IF(IHARG(1).EQ.'OFFS')GO TO 1110
      IF(IHARG(1).EQ.'GAP')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(NUMARG.EQ.1)GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      PTITDS=PDEFDS
      GO TO 1180
!
 1160 CONTINUE
      PTITDS=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)PTITDS
 1181 FORMAT('THE TITLE DISPLACEMENT HAS JUST BEEN ',   &
      'SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPTIDS
      SUBROUTINE DPTITH(IHARG,ARG,NUMARG,PDEFTH,PTITTH,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE THICKNESS FOR THE TITLE
!              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
!              THE THICKNESS FOR THE TITLE WILL BE PLACED
!              IN THE REAL VARIABLE PTITTH.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --ARG    (A  REAL VECTOR)
!                     --NUMARG
!                     --PDEFTH
!     OUTPUT ARGUMENTS--PTITTH
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 COMPUTER SERVICES DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1199
      IF(IHARG(1).EQ.'THIC')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(NUMARG.EQ.1)GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      PTITTH=PDEFTH
      GO TO 1180
!
 1160 CONTINUE
      PTITTH=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)PTITTH
 1181 FORMAT('THE TITLE THICKNESS HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPTITH
      SUBROUTINE DPTL(ICOM,IHARG,NUMARG,   &
      IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL SWITCHES CONTAINED IN THE
!              4 VARIABLES IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW
!              SUCH TIC LABEL SWITCHES TURN ON OR OFF
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--
!                     --IX1ZSW = LOWER HORIZONTAL TIC LABELS
!                     --IX2ZSW = UPPER HORIZONTAL TIC LABELS
!                     --IY1ZSW = LEFT  VERTICAL   TIC LABELS
!                     --IY2ZSW = RIGHT VERTICAL   TIC LABELS
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
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IX1ZSW
      CHARACTER*4 IX2ZSW
      CHARACTER*4 IY1ZSW
      CHARACTER*4 IY2ZSW
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1900
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NUMB')GO TO 1900
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'NUMB')GO TO 1900
!  FOLLOWING 4 LINES ADDED MAY, 1990.
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'OFFS')GO TO 1900
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'OFFS')GO TO 1900
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'SIZE')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'HW')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'FORM')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'CONT')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'NUMB')GO TO 1900
!
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'SIZE')GO TO 1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'HW')GO TO 1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'FORM')GO TO 1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'CONT')GO TO 1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'NUMB')GO TO 1900
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1160
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'LABE')GO TO 1160
      GO TO 1150
!
 1150 CONTINUE
      IHOLD='ON'
      GO TO 1180
!
 1160 CONTINUE
      IHOLD='OFF'
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IX1ZSW=IHOLD
      IX2ZSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1260
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'LABE')GO TO 1260
      GO TO 1250
!
 1250 CONTINUE
      IHOLD='ON'
      GO TO 1280
!
 1260 CONTINUE
      IHOLD='OFF'
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      IX1ZSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1360
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'LABE')GO TO 1360
      GO TO 1350
!
 1350 CONTINUE
      IHOLD='ON'
      GO TO 1380
!
 1360 CONTINUE
      IHOLD='OFF'
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      IX2ZSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1460
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'LABE')GO TO 1460
      GO TO 1450
!
 1450 CONTINUE
      IHOLD='ON'
      GO TO 1480
!
 1460 CONTINUE
      IHOLD='OFF'
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      IY1ZSW=IHOLD
      IY2ZSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1560
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'LABE')GO TO 1560
      GO TO 1550
!
 1550 CONTINUE
      IHOLD='ON'
      GO TO 1580
!
 1560 CONTINUE
      IHOLD='OFF'
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      IY1ZSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1660
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'LABE')GO TO 1660
      GO TO 1650
!
 1650 CONTINUE
      IHOLD='ON'
      GO TO 1680
!
 1660 CONTINUE
      IHOLD='OFF'
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      IY2ZSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1760
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'LABE')GO TO 1760
      GO TO 1750
!
 1750 CONTINUE
      IHOLD='ON'
      GO TO 1780
!
 1760 CONTINUE
      IHOLD='OFF'
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      IX1ZSW=IHOLD
      IX2ZSW=IHOLD
      IY1ZSW=IHOLD
      IY2ZSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTL
      SUBROUTINE DPTLAN(ICOM,IHARG,ARG,NUMARG,   &
      PDEFAN,   &
      PX1ZAN,PX2ZAN,PY1ZAN,PY2ZAN,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL ANGLES CONTAINED IN THE
!              4 VARIABLES PX1ZAN,PX2ZAN,PY1ZAN,PY2ZAN
!              SUCH TIC LABEL ANGLES DEFINE THE ANGLES FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --ARG    (A REAL VECTOR)
!                     --NUMARG
!                     --PDEFAN
!     OUTPUT ARGUMENTS--
!                     --PX1ZAN = LOWER HORIZONTAL TIC LABEL ANGLE
!                     --PX2ZAN = UPPER HORIZONTAL TIC LABEL ANGLE
!                     --PY1ZAN = LEFT  VERTICAL   TIC LABEL ANGLE
!                     --PY2ZAN = RIGHT VERTICAL   TIC LABEL ANGLE
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 COMPUTER SERVICES DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'ANGL')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'ANGL')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ANGL')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      PHOLD=PDEFAN
      GO TO 1180
!
 1160 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PX1ZAN=PHOLD
      PX2ZAN=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL ANGLE (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'ANGL')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      PHOLD=PDEFAN
      GO TO 1280
!
 1260 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      PX1ZAN=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL ANGLE (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)PHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'ANGL')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      PHOLD=PDEFAN
      GO TO 1380
!
 1360 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      PX2ZAN=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL ANGLE (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)PHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'ANGL')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      PHOLD=PDEFAN
      GO TO 1480
!
 1460 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      PY1ZAN=PHOLD
      PY2ZAN=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL ANGLE (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)PHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'ANGL')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      PHOLD=PDEFAN
      GO TO 1580
!
 1560 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      PY1ZAN=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL ANGLE (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)PHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'ANGL')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      PHOLD=PDEFAN
      GO TO 1680
!
 1660 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      PY2ZAN=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL ANGLE (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)PHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'ANGL')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      PHOLD=PDEFAN
      GO TO 1780
!
 1760 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      PX1ZAN=PHOLD
      PX2ZAN=PHOLD
      PY1ZAN=PHOLD
      PY2ZAN=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL ANGLE (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)PHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLAN
      SUBROUTINE DPTLCA(ICOM,IHARG,NUMARG,   &
      IDEFCA,   &
      IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL CASES CONTAINED IN THE
!              4 VARIABLES IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA
!              SUCH TIC LABEL CASES DEFINE THE CASES FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFCA
!     OUTPUT ARGUMENTS--
!                     --IX1ZCA = LOWER HORIZONTAL TIC LABEL CASE
!                     --IX2ZCA = UPPER HORIZONTAL TIC LABEL CASE
!                     --IY1ZCA = LEFT  VERTICAL   TIC LABEL CASE
!                     --IY2ZCA = RIGHT VERTICAL   TIC LABEL CASE
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 COMPUTER SERVICES DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IDEFCA
!
      CHARACTER*4 IX1ZCA
      CHARACTER*4 IX2ZCA
      CHARACTER*4 IY1ZCA
      CHARACTER*4 IY2ZCA
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'CASE')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'CASE')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'CASE')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD=IDEFCA
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IX1ZCA=IHOLD
      IX2ZCA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL CASE (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'CASE')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      IHOLD=IDEFCA
      GO TO 1280
!
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      IX1ZCA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL CASE (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'CASE')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      IHOLD=IDEFCA
      GO TO 1380
!
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      IX2ZCA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL CASE (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'CASE')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      IHOLD=IDEFCA
      GO TO 1480
!
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      IY1ZCA=IHOLD
      IY2ZCA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL CASE (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'CASE')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      IHOLD=IDEFCA
      GO TO 1580
!
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      IY1ZCA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL CASE (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'CASE')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      IHOLD=IDEFCA
      GO TO 1680
!
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      IY2ZCA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL CASE (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'CASE')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      IHOLD=IDEFCA
      GO TO 1780
!
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      IX1ZCA=IHOLD
      IX2ZCA=IHOLD
      IY1ZCA=IHOLD
      IY2ZCA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL CASE (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLCA
      SUBROUTINE DPTLCL(ICOM,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                        IDEFCO,IRGBMX,   &
                        IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO,   &
                        IX1ZC2,IX2ZC2,IY1ZC2,IY2ZC2,   &
                        IBUGPC,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL COLORS CONTAINED IN THE
!              4 VARIABLES IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO
!              SUCH TIC LABEL COLORS DEFINE THE COLORS FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFCO
!     OUTPUT ARGUMENTS--
!                     --IX1ZCO = LOWER HORIZONTAL TIC LABEL COLOR
!                     --IX2ZCO = UPPER HORIZONTAL TIC LABEL COLOR
!                     --IY1ZCO = LEFT  VERTICAL   TIC LABEL COLOR
!                     --IY2ZCO = RIGHT VERTICAL   TIC LABEL COLOR
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
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IDEFCO
      CHARACTER*4 IX1ZCO
      CHARACTER*4 IX2ZCO
      CHARACTER*4 IY1ZCO
      CHARACTER*4 IY2ZCO
      CHARACTER*4 IBUGPC
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
      CHARACTER*4 ICASCL
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IHARG2(*)
      CHARACTER*4 IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IX1ZC2(3)
      DIMENSION IX2ZC2(3)
      DIMENSION IY1ZC2(3)
      DIMENSION IY2ZC2(3)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ICASCL='STAN'
      IHOLD=' '
      JHOLD1=-1
      JHOLD2=-1
      JHOLD3=-1
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
         IHARG(2).EQ.'COLO')THEN
        GO TO 1090
      ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
             IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'COLO')THEN
        GO TO 1090
      ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'LABE'.AND.   &
             IHARG(2).EQ.'RGB '.AND.IHARG(3).EQ.'COLO')THEN
        ICASCL='RGB '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGPC,IERROR)
        IHARG(1)='LABE'
        IHARG2(1)='EL  '
        IHARG(2)='COLO'
        IHARG2(2)='R   '
        GO TO 1090
      ELSEIF(NUMARG.GE.4.AND.IHARG(1).EQ.'MARK'.AND.   &
             IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'RGB '.AND.   &
             IHARG(4).EQ.'COLO')THEN
        ICASCL='RGB '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGPC,IERROR)
        IHARG(1)='MARK'
        IHARG2(1)='    '
        IHARG(2)='LABE'
        IHARG2(2)='EL  '
        IHARG(3)='COLO'
        IHARG2(3)='R   '
        GO TO 1090
      ENDIF
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IX1ZC2(1)=JHOLD1
          IX1ZC2(2)=JHOLD2
          IX1ZC2(3)=JHOLD3
          IX2ZC2(1)=JHOLD1
          IX2ZC2(2)=JHOLD2
          IX2ZC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1186)
 1186       FORMAT('THE TIC MARK LABEL RGB COLOR (FOR BOTH HORIZONTAL ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
 1187       FORMAT('HAS JUST BEEN SET TO ',3I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IX1ZCO=IHOLD
          IX2ZCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1181)
 1181       FORMAT('THE TIC MARK LABEL COLOR (FOR BOTH HORIZONTAL ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
 1182       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
        GO TO 1900
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      ELSEIF(ICOM.EQ.'X1TI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IX1ZC2(1)=JHOLD1
          IX1ZC2(2)=JHOLD2
          IX1ZC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1286)
 1286       FORMAT('THE TIC MARK LABEL RGB COLOR (FOR THE BOTTOM ',   &
                   'HORIZONTAL FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IX1ZCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1281)
 1281       FORMAT('THE TIC MARK LABEL COLOR (FOR THE BOTTOM ',   &
                   'HORIZONTAL FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      ELSEIF(ICOM.EQ.'X2TI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IX2ZC2(1)=JHOLD1
          IX2ZC2(2)=JHOLD2
          IX2ZC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1386)
 1386       FORMAT('THE TIC MARK LABEL RGB COLOR (FOR THE TOP ',   &
                   'HORIZONTAL FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IX2ZCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1381)
 1381       FORMAT('THE TIC MARK LABEL COLOR (FOR THE TOP HORIZONTAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      ELSEIF(ICOM.EQ.'YTIC')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IY1ZC2(1)=JHOLD1
          IY1ZC2(2)=JHOLD2
          IY1ZC2(3)=JHOLD3
          IY2ZC2(1)=JHOLD1
          IY2ZC2(2)=JHOLD2
          IY2ZC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1486)
 1486       FORMAT('THE TIC MARK LABEL RGB COLOR (FOR BOTH VERTICAL ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IY1ZCO=IHOLD
          IY2ZCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1481)
 1481       FORMAT('THE TIC MARK LABEL COLOR (FOR BOTH VERTICAL ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      ELSEIF(ICOM.EQ.'Y1TI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IY1ZC2(1)=JHOLD1
          IY1ZC2(2)=JHOLD2
          IY1ZC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1586)
 1586       FORMAT('THE TIC MARK LABEL RGB COLOR (FOR THE LEFT ',   &
                   'VERTICAL FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IY1ZCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1581)
 1581       FORMAT('THE TIC MARK LABEL COLOR (FOR THE LEFT VERTICAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      ELSEIF(ICOM.EQ.'Y2TI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IY2ZC2(1)=JHOLD1
          IY2ZC2(2)=JHOLD2
          IY2ZC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1686)
 1686       FORMAT('THE TIC MARK LABEL RGB COLOR (FOR THE RIGHT ',   &
                   'VERTICAL FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IY2ZCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1681)
 1681       FORMAT('THE TIC MARK LABEL COLOR (FOR THE RIGHT VERTICAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      ELSEIF(ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.   &
             ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IX1ZC2(1)=JHOLD1
          IX1ZC2(2)=JHOLD2
          IX1ZC2(3)=JHOLD3
          IX2ZC2(1)=JHOLD1
          IX2ZC2(2)=JHOLD2
          IX2ZC2(3)=JHOLD3
          IY1ZC2(1)=JHOLD1
          IY1ZC2(2)=JHOLD2
          IY1ZC2(3)=JHOLD3
          IY2ZC2(1)=JHOLD1
          IY2ZC2(2)=JHOLD2
          IY2ZC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1786)
 1786       FORMAT('THE TIC MARK LABEL RGB COLOR (FOR ALL 4 ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IX1ZCO=IHOLD
          IX2ZCO=IHOLD
          IY1ZCO=IHOLD
          IY2ZCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1781)
 1781       FORMAT('THE TIC MARK LABEL COLOR (FOR ALL 4 ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
!
      ENDIF
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLCL
      SUBROUTINE DPTLCN(ICOM,IHARG,NUMARG,   &
                        IANS,IANSLC,IWIDTH,   &
                        IX1ZCN,IX2ZCN,IY1ZCN,IY2ZCN,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL CONTENTS CONTAINED IN THE
!              4 VARIABLES IX1ZCN,IX2ZCN,IY1ZCN,IY2ZCN
!              SUCH TIC LABEL CONTENTS DEFINE THE CONTENTS FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--
!                     --IX1ZCN = LOWER HORIZONTAL TIC LABEL CONTENTS
!                     --IX2ZCN = UPPER HORIZONTAL TIC LABEL CONTENTS
!                     --IY1ZCN = LEFT  VERTICAL   TIC LABEL CONTENTS
!                     --IY2ZCN = RIGHT VERTICAL   TIC LABEL CONTENTS
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
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--JANUARY   1988.
!     UPDATED         --AUGUST    2001. UPDATE DIMENSIONS FROM 130
!                                       TO 160
!     UPDATED         --SEPTEMBER 2014. UPDATE DIMENSIONS FROM 512 TO
!                                       2048
!     UPDATED         --APRIL     2017. SOME RECODING FOR BETTER READABILITY
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IANS
!CCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
!CCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      CHARACTER*4 IANSLC
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*2048 IHOLCN
      CHARACTER*2048 ICJUNK
!
      CHARACTER*2048 IX1ZCN
      CHARACTER*2048 IX2ZCN
      CHARACTER*2048 IY1ZCN
      CHARACTER*2048 IY2ZCN
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
      DIMENSION IANS(*)
!CCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
!CCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      DIMENSION IANSLC(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
         IHARG(2).EQ.'CONT')GO TO 1009
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'CONT')GO TO 1009
      GO TO 9000
 1009 CONTINUE
!
!               ************************************
!               **  EXTRACT THE FULL STRING       **
!               ************************************
!
      DO 1010 I=1,IWIDTH-6
        I2=I
        IF(IANS(I).EQ.'C'.AND.IANS(I+1).EQ.'O'.AND.   &
           IANS(I+2).EQ.'N'.AND.IANS(I+3).EQ.'T'.AND.   &
           IANS(I+4).EQ.'E'.AND.IANS(I+5).EQ.'N'.AND.   &
           IANS(I+6).EQ.'T')THEN
          IFOUND='YES'
          ISTART=I+8
          IF(IANS(I+7).EQ.'S')ISTART=ISTART+1
          GO TO 1019
        ENDIF
 1010 CONTINUE
!
      WRITE(ICOUT,1011)
 1011 FORMAT('***** ERROR IN TIC MARK LABEL CONTENT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1012)
 1012 FORMAT('      NO MATCH FOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
 1019 CONTINUE
!
      ISTOP=0
      IF(ISTART.GT.IWIDTH)GO TO 1039
      DO 1030 I=ISTART,IWIDTH
        IREV=IWIDTH-I+ISTART
        IF(IANS(IREV).NE.' ')THEN
          ISTOP=IREV
          GO TO 1039
        ENDIF
 1030 CONTINUE
 1039 CONTINUE
!
      ICJUNK=' '
      NCJUNK=0
      IF(ISTART.LE.ISTOP .AND. ISTOP.GT.0)THEN
        J=0
        DO 1040 I=ISTART,ISTOP
          J=J+1
!CCCC     THE FOLLOWING LINE WAS CHANGED     SEPTEMBER 1993
!CCCC     TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
!CCCC     ICJUNK(J:J)=IANS(I)
          ICJUNK(J:J)=IANSLC(I)
 1040   CONTINUE
        NCJUNK=J
      ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
!
        IFOUND='YES'
        IX1ZCN=IHOLCN
        IX2ZCN=IHOLCN
!
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
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  TREAT THE CASE WHEN                             **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE  **
!               **  CHANGED                                         **
!               ******************************************************
!
      IF(ICOM.EQ.'X1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IX1ZCN=IHOLCN
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1281)
 1281     FORMAT('THE TIC MARK LABEL CONTENTS FOR THE BOTTOM ',   &
                 'HORIZONTAL')
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
        GO TO 9000
      ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IX2ZCN=IHOLCN
!
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
        GO TO 9000
      ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IY1ZCN=IHOLCN
        IY2ZCN=IHOLCN
!
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
        GO TO 9000
      ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IY1ZCN=IHOLCN
!
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
        GO TO 9000
      ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IY2ZCN=IHOLCN
!
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
        GO TO 9000
      ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.   &
         ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'CONT')THEN
          IHOLCN='DEFAULT'
        ELSE
          IHOLCN=ICJUNK
        ENDIF
        IFOUND='YES'
        IX1ZCN=IHOLCN
        IX2ZCN=IHOLCN
        IY1ZCN=IHOLCN
        IY2ZCN=IHOLCN
!
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
        GO TO 9000
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPTLCN
      SUBROUTINE DPTLDI(ICOM,IHARG,NUMARG,   &
      IDEFDI,   &
      IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL DIRECTIONS CONTAINED IN THE
!              4 VARIABLES IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI
!              SUCH TIC LABEL DIRECTIONS DEFINE THE DIRECTIONS FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFDI
!     OUTPUT ARGUMENTS--
!                     --IX1ZDI = LOWER HORIZONTAL TIC LABEL DIRECTION
!                     --IX2ZDI = UPPER HORIZONTAL TIC LABEL DIRECTION
!                     --IY1ZDI = LEFT  VERTICAL   TIC LABEL DIRECTION
!                     --IY2ZDI = RIGHT VERTICAL   TIC LABEL DIRECTION
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 COMPUTER SERVICES DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IDEFDI
!
      CHARACTER*4 IX1ZDI
      CHARACTER*4 IX2ZDI
      CHARACTER*4 IY1ZDI
      CHARACTER*4 IY2ZDI
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'DIRE')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'DIRE')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DIRE')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD=IDEFDI
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IX1ZDI=IHOLD
      IX2ZDI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL DIRECTION (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DIRE')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      IHOLD=IDEFDI
      GO TO 1280
!
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      IX1ZDI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL DIRECTION (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DIRE')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      IHOLD=IDEFDI
      GO TO 1380
!
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      IX2ZDI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL DIRECTION (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DIRE')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      IHOLD=IDEFDI
      GO TO 1480
!
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      IY1ZDI=IHOLD
      IY2ZDI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL DIRECTION (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DIRE')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      IHOLD=IDEFDI
      GO TO 1580
!
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      IY1ZDI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL DIRECTION (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DIRE')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      IHOLD=IDEFDI
      GO TO 1680
!
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      IY2ZDI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL DIRECTION (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DIRE')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      IHOLD=IDEFDI
      GO TO 1780
!
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      IX1ZDI=IHOLD
      IX2ZDI=IHOLD
      IY1ZDI=IHOLD
      IY2ZDI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL DIRECTION (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLDI
      SUBROUTINE DPTLDS(ICOM,IHARG,IARGT,ARG,NUMARG,   &
      PDEFHG,PDEFVG,   &
      PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK LABEL DISPLACEMENT SWITCHES
!              FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK SWITCHES DEFINE THE DISPLACEMENT
!              OF THE TIC MARK LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG
!                     --PDEFHG
!                     --PDEFVG
!     OUTPUT ARGUMENTS--
!                     --PX1ZDS,
!                     --PX2ZDS,
!                     --PY1ZDS,
!                     --PY2ZDS,
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
!     VERSION NUMBER--91/9
!     ORIGINAL VERSION--AUGUST    1991.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
!CCCC IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'DISP')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'DISP')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'OFFS')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'OFFS')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'GAP')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'GAP')GO TO 1090
!CCCC GO TO 1900
      GO TO 9000
 1090 CONTINUE
      HOLD1=(-999.9)
      HOLD2=(-999.9)
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DISP')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      IERROR='YES'
      GO TO 9000
!
 1150 CONTINUE
      HOLD1=PDEFHG
      GO TO 1180
!
 1160 CONTINUE
      HOLD1=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PX1ZDS=HOLD1
      PX2ZDS=HOLD1
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
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
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DISP')GO TO 1250
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1260
      IERROR='YES'
      GO TO 9000
!
 1250 CONTINUE
      HOLD1=PDEFHG
      GO TO 1280
!
 1260 CONTINUE
      HOLD1=ARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      PX1ZDS=HOLD1
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
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
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DISP')GO TO 1350
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1360
      IERROR='YES'
      GO TO 9000
!
 1350 CONTINUE
      HOLD1=PDEFHG
      GO TO 1380
!
 1360 CONTINUE
      HOLD1=ARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      PX2ZDS=HOLD1
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
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
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DISP')GO TO 1450
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1460
      IERROR='YES'
      GO TO 9000
!
 1450 CONTINUE
      HOLD1=PDEFVG
      GO TO 1480
!
 1460 CONTINUE
      HOLD1=ARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      PY1ZDS=HOLD1
      PY2ZDS=HOLD1
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
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
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DISP')GO TO 1550
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1560
      IERROR='YES'
      GO TO 9000
!
 1550 CONTINUE
      HOLD1=PDEFVG
      GO TO 1580
!
 1560 CONTINUE
      HOLD1=ARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      PY1ZDS=HOLD1
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
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
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DISP')GO TO 1650
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1660
      IERROR='YES'
      GO TO 9000
!
 1650 CONTINUE
      HOLD1=PDEFVG
      GO TO 1680
!
 1660 CONTINUE
      HOLD1=ARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      PY2ZDS=HOLD1
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
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
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DISP')GO TO 1750
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1760
      IERROR='YES'
      GO TO 9000
!
 1750 CONTINUE
      HOLD1=PDEFHG
      HOLD2=PDEFVG
      GO TO 1780
!
 1760 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=ARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      PX1ZDS=HOLD1
      PX2ZDS=HOLD1
      PY1ZDS=HOLD2
      PY2ZDS=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
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
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
!
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPTLDS
      SUBROUTINE DPTLFI(ICOM,IHARG,NUMARG,   &
      IDEFFI,   &
      IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL FILLS CONTAINED IN THE
!              4 VARIABLES IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI
!              SUCH TIC LABEL FILLS DEFINE THE FILLS FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFFI
!     OUTPUT ARGUMENTS--
!                     --IX1ZFI = LOWER HORIZONTAL TIC LABEL FILL
!                     --IX2ZFI = UPPER HORIZONTAL TIC LABEL FILL
!                     --IY1ZFI = LEFT  VERTICAL   TIC LABEL FILL
!                     --IY2ZFI = RIGHT VERTICAL   TIC LABEL FILL
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 COMPUTER SERVICES DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IDEFFI
!
      CHARACTER*4 IX1ZFI
      CHARACTER*4 IX2ZFI
      CHARACTER*4 IY1ZFI
      CHARACTER*4 IY2ZFI
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'FILL')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'FILL')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'FILL')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD=IDEFFI
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IX1ZFI=IHOLD
      IX2ZFI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL FILL (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'FILL')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      IHOLD=IDEFFI
      GO TO 1280
!
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      IX1ZFI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL FILL (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'FILL')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      IHOLD=IDEFFI
      GO TO 1380
!
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      IX2ZFI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL FILL (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'FILL')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      IHOLD=IDEFFI
      GO TO 1480
!
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      IY1ZFI=IHOLD
      IY2ZFI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL FILL (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'FILL')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      IHOLD=IDEFFI
      GO TO 1580
!
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      IY1ZFI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL FILL (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'FILL')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      IHOLD=IDEFFI
      GO TO 1680
!
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      IY2ZFI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL FILL (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'FILL')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      IHOLD=IDEFFI
      GO TO 1780
!
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      IX1ZFI=IHOLD
      IX2ZFI=IHOLD
      IY1ZFI=IHOLD
      IY2ZFI=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL FILL (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLFI
      SUBROUTINE DPTLFM(ICOM,IHARG,NUMARG,   &
                        IDETLF,   &
                        IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL FORMATS CONTAINED IN THE
!              4 VARIABLES IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM
!              SUCH TIC LABEL FORMATS DEFINE THE FORMATS FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDETLF
!     OUTPUT ARGUMENTS--
!                     --IX1ZFM = LOWER HORIZONTAL TIC LABEL FORMAT
!                     --IX2ZFM = UPPER HORIZONTAL TIC LABEL FORMAT
!                     --IY1ZFM = LEFT  VERTICAL   TIC LABEL FORMAT
!                     --IY2ZFM = RIGHT VERTICAL   TIC LABEL FORMAT
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
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--FEBRUARY  1988.
!     UPDATED         --JANUARY   2004. ADD SUPPORT FOR:
!                                           ROW LABEL
!                                           GROUP LABEL
!                                           VARIABLE
!     UPDATED         --APRIL     2017. ROW LABEL CAN HAVE A
!                                       A START ROW AND A STOP
!                                       ROW.  ALSO SOME RE-CODING
!                                       FOR BETTER READABILTY
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IDETLF
      CHARACTER*4 IX1ZFM
      CHARACTER*4 IX2ZFM
      CHARACTER*4 IY1ZFM
      CHARACTER*4 IY2ZFM
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'FORM')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'FORM')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
!
        IFOUND='YES'
        IX1ZFM=IHOLD
        IX2ZFM=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
 1181     FORMAT('THE TIC MARK LABEL FORMAT (FOR BOTH HORIZONTAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1182)IHOLD
 1182     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 1900
      ENDIF
!
!               *******************************************************
!               **  TREAT THE CASE WHEN ONLY THE BOTTOM              **
!               **  HORIZONTAL TIC MARKS ARE TO BE CHANGED           **
!               *******************************************************
!
      IF(ICOM.EQ.'X1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
!
        IFOUND='YES'
        IX1ZFM=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1281)
 1281     FORMAT('THE TIC MARK LABEL FORMAT (FOR THE BOTTOM ',   &
                 'HORIZONTAL FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1282)IHOLD
 1282     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 1900
      ENDIF
!
!               *******************************************************
!               **  TREAT THE CASE WHEN ONLY THE TOP                 **
!               **  HORIZONTAL TIC MARKS ARE TO BE CHANGED           **
!               *******************************************************
!
      IF(ICOM.EQ.'X2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
!
        IFOUND='YES'
        IX2ZFM=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1381)
 1381     FORMAT('THE TIC MARK LABEL FORMAT (FOR THE TOP HORIZONTAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1382)IHOLD
 1382     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 1900
      ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
!
        IFOUND='YES'
        IY1ZFM=IHOLD
        IY2ZFM=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1481)
 1481     FORMAT('THE TIC MARK LABEL FORMAT (FOR BOTH VERTICAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1482)IHOLD
 1482     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 1900
      ENDIF
!
!               *******************************************************
!               **  TREAT THE CASE WHEN ONLY THE LEFT                **
!               **  VERTICAL   TIC MARKS ARE TO BE CHANGED           **
!               *******************************************************
!
      IF(ICOM.EQ.'Y1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
!
        IFOUND='YES'
        IY1ZFM=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1581)
 1581     FORMAT('THE TIC MARK LABEL FORMAT (FOR THE LEFT VERTICAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1582)IHOLD
 1582     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 1900
      ENDIF
!
!               *******************************************************
!               **  TREAT THE CASE WHEN ONLY THE RIGHT               **
!               **  VERTICAL   TIC MARKS ARE TO BE CHANGED           **
!               *******************************************************
!
      IF(ICOM.EQ.'Y2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
!
        IFOUND='YES'
        IY2ZFM=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1681)
 1681     FORMAT('THE TIC MARK LABEL FORMAT (FOR THE RIGHT VERTICAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1682)IHOLD
 1682     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 1900
      ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.   &
         ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FORM')THEN
          IHOLD=IDETLF
        ELSEIF(IHARG(NUMARG).EQ.'ROWL')THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'ROW '.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
          IHOLD='ROWL'
        ELSEIF(IHARG(NUMARG-1).EQ.'GROU'.AND.IHARG(NUMARG).EQ.'LABE')   &
        THEN
           IHOLD='GLAB'
        ELSEIF(IHARG(NUMARG).EQ.'VARI')THEN
           IHOLD='VARI'
        ELSE
          IHOLD=IHARG(NUMARG)
          IF(IHOLD.EQ.'FIXE')IHOLD='REAL'
        ENDIF
!
        IFOUND='YES'
        IX1ZFM=IHOLD
        IX2ZFM=IHOLD
        IY1ZFM=IHOLD
        IY2ZFM=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1781)
 1781     FORMAT('THE TIC MARK LABEL FORMAT (FOR ALL 4 ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1782)IHOLD
 1782     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 1900
      ENDIF
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLFM
      SUBROUTINE DPTLFO(ICOM,IHARG,NUMARG,IDEFFO,   &
                        IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL FONTS CONTAINED IN THE
!              4 VARIABLES IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO
!              SUCH TIC LABEL FONTS DEFINE THE FONTS FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFFO
!     OUTPUT ARGUMENTS--
!                     --IX1ZFO = LOWER HORIZONTAL TIC LABEL FONT
!                     --IX2ZFO = UPPER HORIZONTAL TIC LABEL FONT
!                     --IY1ZFO = LEFT  VERTICAL   TIC LABEL FONT
!                     --IY2ZFO = RIGHT VERTICAL   TIC LABEL FONT
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IDEFFO
!
      CHARACTER*4 IX1ZFO
      CHARACTER*4 IX2ZFO
      CHARACTER*4 IY1ZFO
      CHARACTER*4 IY2ZFO
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'FONT')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'FONT')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FONT')THEN
          IHOLD=IDEFFO
          GO TO 1180
        ELSE
          IHOLD=IHARG(NUMARG)
          GO TO 1180
        ENDIF
      ELSE
        GO TO 1199
      ENDIF
!
 1180 CONTINUE
      IFOUND='YES'
      IF(IHOLD.EQ.'HARD')IHOLD='TEKT'
      IX1ZFO=IHOLD
      IX2ZFO=IHOLD
      IF(IX1ZFO.EQ.'HARD')IX1ZFO='TEKT'
      IF(IX2ZFO.EQ.'HARD')IX2ZFO='TEKT'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
 1181   FORMAT('THE TIC MARK LABEL FONT (FOR BOTH HORIZONTAL ',   &
               'FRAME LINES)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)IHOLD
 1182   FORMAT('HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1900
!
 1199 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN ONLY THE                   **
!               **  BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               *****************************************************
!
      IF(ICOM.EQ.'X1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FONT')THEN
          IHOLD=IDEFFO
          GO TO 1280
        ELSE
          IHOLD=IHARG(NUMARG)
          GO TO 1280
        ENDIF
      ELSE
        GO TO 1299
      ENDIF
!
 1280 CONTINUE
      IFOUND='YES'
      IX1ZFO=IHOLD
      IF(IX1ZFO.EQ.'HARD')IX1ZFO='TEKT'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1281)
 1281   FORMAT('THE TIC MARK LABEL FONT (FOR THE BOTTOM ',   &
               'HORIZONTAL FRAME LINE)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1282)IX1ZFO
 1282   FORMAT('HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1900
!
 1299 CONTINUE
!
!               **********************************************
!               **  TREAT THE CASE WHEN ONLY THE TOP        **
!               **  HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **********************************************
!
      IF(ICOM.EQ.'X2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FONT')THEN
          IHOLD=IDEFFO
          GO TO 1380
        ELSE
          IHOLD=IHARG(NUMARG)
          GO TO 1380
        ENDIF
      ELSE
        GO TO 1399
      ENDIF
!
 1380 CONTINUE
      IFOUND='YES'
      IX2ZFO=IHOLD
      IF(IX2ZFO.EQ.'HARD')IX2ZFO='TEKT'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1381)
 1381   FORMAT('THE TIC MARK LABEL FONT (FOR THE TOP HORIZONTAL ',   &
               'FRAME LINE)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1382)IX2ZFO
 1382   FORMAT('HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FONT')THEN
          IHOLD=IDEFFO
          GO TO 1480
        ELSE
          IHOLD=IHARG(NUMARG)
          GO TO 1480
        ENDIF
      ELSE
        GO TO 1499
      ENDIF
!
 1480 CONTINUE
      IFOUND='YES'
      IF(IHOLD.EQ.'HARD')IHOLD='TEKT'
      IY1ZFO=IHOLD
      IY2ZFO=IHOLD
      IF(IY1ZFO.EQ.'HARD')IY1ZFO='TEKT'
      IF(IY2ZFO.EQ.'HARD')IY2ZFO='TEKT'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1481)
 1481   FORMAT('THE TIC MARK LABEL FONT (FOR BOTH VERTICAL ',   &
               'FRAME LINES)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1482)IHOLD
 1482   FORMAT('HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1900
!
 1499 CONTINUE
!
!               **********************************************
!               **  TREAT THE CASE WHEN ONLY THE LEFT       **
!               **  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **********************************************
!
      IF(ICOM.EQ.'Y1TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FONT')THEN
          IHOLD=IDEFFO
          GO TO 1580
        ELSE
          IHOLD=IHARG(NUMARG)
          GO TO 1580
        ENDIF
      ELSE
        GO TO 1599
      ENDIF
!
 1580 CONTINUE
      IFOUND='YES'
      IY1ZFO=IHOLD
      IF(IY1ZFO.EQ.'HARD')IY1ZFO='TEKT'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1581)
 1581   FORMAT('THE TIC MARK LABEL FONT (FOR THE LEFT VERTICAL ',   &
               'FRAME LINE)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1582)IY1ZFO
 1582   FORMAT('HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1900
!
 1599 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN  ONLY THE                  **
!               **  RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               *****************************************************
!
      IF(ICOM.EQ.'Y2TI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FONT')THEN
          IHOLD=IDEFFO
          GO TO 1680
        ELSE
          IHOLD=IHARG(NUMARG)
          GO TO 1680
        ENDIF
      ELSE
        GO TO 1699
      ENDIF
!
 1680 CONTINUE
      IFOUND='YES'
      IY2ZFO=IHOLD
      IF(IY2ZFO.EQ.'HARD')IY2ZFO='TEKT'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1681)
 1681   FORMAT('THE TIC MARK LABEL FONT (FOR THE RIGHT VERTICAL ',   &
               'FRAME LINE)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1682)IY2ZFO
 1682   FORMAT('HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.   &
         ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'FONT')THEN
          IHOLD=IDEFFO
          GO TO 1780
        ELSE
          IHOLD=IHARG(NUMARG)
          GO TO 1780
        ENDIF
      ELSE
        GO TO 1799
      ENDIF
!
 1780 CONTINUE
      IFOUND='YES'
      IF(IHOLD.EQ.'HARD')IHOLD='TEKT'
      IX1ZFO=IHOLD
      IX2ZFO=IHOLD
      IY1ZFO=IHOLD
      IY2ZFO=IHOLD
      IF(IX1ZFO.EQ.'HARD')IX1ZFO='TEKT'
      IF(IX2ZFO.EQ.'HARD')IX2ZFO='TEKT'
      IF(IY1ZFO.EQ.'HARD')IY1ZFO='TEKT'
      IF(IY2ZFO.EQ.'HARD')IY2ZFO='TEKT'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1781)
 1781   FORMAT('THE TIC MARK LABEL FONT (FOR ALL 4 ',   &
               'FRAME LINES)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1782)IHOLD
 1782   FORMAT('HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLFO
      SUBROUTINE DPTLHW(ICOM,IHARG,IARGT,ARG,NUMARG,   &
      PDEFHE,PDEFWI,   &
      PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,   &
      PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,   &
      PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,   &
      PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK LABEL HEIGHT AND WIDTH SWITCHES
!              FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK SWITCHES DEFINE THE HEIGHT AND WIDTH
!              OF THE TIC MARK LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG
!                     --PDEFHE
!                     --PDEFWI
!     OUTPUT ARGUMENTS--
!                     --PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,
!                     --PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,
!                     --PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,
!                     --PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,
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
!     ORIGINAL VERSION--JULY      1987.
!     UPDATED         --DECEMBER  1988.    ADD DEFAULT WIDTH
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      NUMAM1=NUMARG-1
!
!CCCC IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'HW')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'HW')GO TO 1090
!CCCC GO TO 1900
      GO TO 9000
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'HW')GO TO 1150
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.   &
         IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      IERROR='YES'
      GO TO 9000
!
 1150 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1180
!
 1160 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX2ZHE=HOLD1
      PX1ZWI=HOLD2
      PX2ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR BOTH ',   &
      'HORIZONTAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)HOLD1,HOLD2
 1182 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'HW')GO TO 1250
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.   &
         IARGT(NUMARG).EQ.'NUMB')GO TO 1260
      IERROR='YES'
      GO TO 9000
!
 1250 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1280
!
 1260 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX1ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)HOLD1,HOLD2
 1282 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'HW')GO TO 1350
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.   &
         IARGT(NUMARG).EQ.'NUMB')GO TO 1360
      IERROR='YES'
      GO TO 9000
!
 1350 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1380
!
 1360 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      PX2ZHE=HOLD1
      PX2ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR THE TOP ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)HOLD1,HOLD2
 1382 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'HW')GO TO 1450
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.   &
         IARGT(NUMARG).EQ.'NUMB')GO TO 1460
      IERROR='YES'
      GO TO 9000
!
 1450 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1480
!
 1460 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      PY1ZHE=HOLD1
      PY2ZHE=HOLD1
      PY1ZWI=HOLD2
      PY2ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR BOTH ',   &
      'VERTICAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)HOLD1,HOLD2
 1482 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'HW')GO TO 1550
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.   &
         IARGT(NUMARG).EQ.'NUMB')GO TO 1560
      IERROR='YES'
      GO TO 9000
!
 1550 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1580
!
 1560 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      PY1ZHE=HOLD1
      PY1ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR THE LEFT ',   &
      'VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)HOLD1,HOLD2
 1582 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'HW')GO TO 1650
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.   &
         IARGT(NUMARG).EQ.'NUMB')GO TO 1660
      IERROR='YES'
      GO TO 9000
!
 1650 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1680
!
 1660 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      PY2ZHE=HOLD1
      PY2ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR THE RIGHT ',   &
      'VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)HOLD1,HOLD2
 1682 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'HW')GO TO 1750
      IF(IARGT(NUMAM1).EQ.'NUMB'.AND.   &
         IARGT(NUMARG).EQ.'NUMB')GO TO 1760
      IERROR='YES'
      GO TO 9000
!
 1750 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1780
!
 1760 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GO TO 1780
!
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
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL HEIGHT & WIDTH (FOR ',   &
      'ALL 4 FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)HOLD1,HOLD2
 1782 FORMAT('HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
!
      PX1ZVG=PX1ZHE*0.375
      PX2ZVG=PX2ZHE*0.375
      PY1ZVG=PY1ZHE*0.375
      PY2ZVG=PY2ZHE*0.375
!
      PX1ZHG=PX1ZHE*0.125
      PX2ZHG=PX2ZHE*0.125
      PY1ZHG=PY1ZHE*0.125
      PY2ZHG=PY2ZHE*0.125
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPTLHW
      SUBROUTINE DPTLJU(ICOM,IHARG,NUMARG,   &
      IDEFJU,   &
      IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL JUSTIFICATIONS CONTAINED IN THE
!              4 VARIABLES IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU
!              SUCH TIC LABEL JUSTIFICATIONS DEFINE THE JUSTIFICATIONS FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFJU
!     OUTPUT ARGUMENTS--
!                     --IX1ZJU = LOWER HORIZONTAL TIC LABEL JUSTIFICATION
!                     --IX2ZJU = UPPER HORIZONTAL TIC LABEL JUSTIFICATION
!                     --IY1ZJU = LEFT  VERTICAL   TIC LABEL JUSTIFICATION
!                     --IY2ZJU = RIGHT VERTICAL   TIC LABEL JUSTIFICATION
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IDEFJU
!
      CHARACTER*4 IX1ZJU
      CHARACTER*4 IX2ZJU
      CHARACTER*4 IY1ZJU
      CHARACTER*4 IY2ZJU
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'JUST')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'JUST')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'JUST')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD=IDEFJU
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IX1ZJU=IHOLD
      IX2ZJU=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR BOTH ',   &
      'HORIZONTAL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'JUST')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      IHOLD=IDEFJU
      GO TO 1280
!
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      IX1ZJU=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'JUST')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      IHOLD=IDEFJU
      GO TO 1380
!
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      IX2ZJU=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR THE TOP ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'JUST')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      IHOLD=IDEFJU
      GO TO 1480
!
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      IY1ZJU=IHOLD
      IY2ZJU=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'JUST')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      IHOLD=IDEFJU
      GO TO 1580
!
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      IY1ZJU=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR THE LEFT ',   &
      'VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'JUST')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      IHOLD=IDEFJU
      GO TO 1680
!
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      IY2ZJU=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR THE RIGHT ',   &
      'VERTICAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'JUST')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      IHOLD=IDEFJU
      GO TO 1780
!
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      IX1ZJU=IHOLD
      IX2ZJU=IHOLD
      IY1ZJU=IHOLD
      IY2ZJU=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL JUSTIFICATION (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLJU
      SUBROUTINE DPTLSZ(ICOM,IHARG,IARGT,ARG,NUMARG,   &
      PDEFHE,PDEFWI,   &
      PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,   &
      PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,   &
      PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,   &
      PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK LABEL SIZE SWITCHES
!              FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK SWITCHES DEFINE THE SIZE (HEIGHT)
!              OF THE TIC MARK LABELS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG
!                     --PDEFHE
!     OUTPUT ARGUMENTS--
!                     --PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG,
!                     --PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG,
!                     --PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG,
!                     --PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG,
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
!     ORIGINAL VERSION--OCTOBER   1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1988.  DEFAULT WIDTH
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
!CCCC IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'SIZE')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'SIZE')GO TO 1090
!CCCC GO TO 1900
      GO TO 9000
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      IERROR='YES'
      GO TO 9000
!
 1150 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1180
!
 1160 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX2ZHE=HOLD1
      PX1ZWI=HOLD2
      PX2ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL SIZE (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)HOLD1
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1250
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1260
      IERROR='YES'
      GO TO 9000
!
 1250 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1280
!
 1260 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      PX1ZHE=HOLD1
      PX1ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL SIZE (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)HOLD1
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1350
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1360
      IERROR='YES'
      GO TO 9000
!
 1350 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1380
!
 1360 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      PX2ZHE=HOLD1
      PX2ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL SIZE (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)HOLD1
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1450
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1460
      IERROR='YES'
      GO TO 9000
!
 1450 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1480
!
 1460 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      PY1ZHE=HOLD1
      PY2ZHE=HOLD1
      PY1ZWI=HOLD2
      PY2ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL SIZE (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)HOLD1
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1550
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1560
      IERROR='YES'
      GO TO 9000
!
 1550 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1580
!
 1560 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      PY1ZHE=HOLD1
      PY1ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL SIZE (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)HOLD1
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1650
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1660
      IERROR='YES'
      GO TO 9000
!
 1650 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1680
!
 1660 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      PY2ZHE=HOLD1
      PY2ZWI=HOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL SIZE (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)HOLD1
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1750
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1760
      IERROR='YES'
      GO TO 9000
!
 1750 CONTINUE
      HOLD1=PDEFHE
      HOLD2=PDEFWI
      GO TO 1780
!
 1760 CONTINUE
      HOLD1=ARG(NUMARG)
      HOLD2=HOLD1*0.5
      GO TO 1780
!
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
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL SIZE (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)HOLD1
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
!
      PX1ZVG=PX1ZHE*0.375
      PX2ZVG=PX2ZHE*0.375
      PY1ZVG=PY1ZHE*0.375
      PY2ZVG=PY2ZHE*0.375
!
      PX1ZHG=PX1ZHE*0.125
      PX2ZHG=PX2ZHE*0.125
      PY1ZHG=PY1ZHE*0.125
      PY2ZHG=PY2ZHE*0.125
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPTLSZ
      SUBROUTINE DPTLTH(ICOM,IHARG,ARG,NUMARG,   &
      PDEFTH,   &
      PTIZTH,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC LABEL THICKNESSS CONTAINED IN THE
!              4 VARIABLES PTIZTH,PTIZTH,PTIZTH,PTIZTH
!              SUCH TIC LABEL THICKNESSS DEFINE THE THICKNESSS FOR
!              THE TIC LABELS ON THE 4 FRAME LINES OF A PLOT.
!              NOTE: ALL 4 THICKNESS CURRENTLY LIMITED TO ONE
!                    SETTING, PTIZTH
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --ARG    (A REAL VECTOR)
!                     --NUMARG
!                     --PDEFTH
!     OUTPUT ARGUMENTS--
!                     --PTIZTH = LOWER HORIZONTAL TIC LABEL THICKNESS
!                     --PTIZTH = UPPER HORIZONTAL TIC LABEL THICKNESS
!                     --PTIZTH = LEFT  VERTICAL   TIC LABEL THICKNESS
!                     --PTIZTH = RIGHT VERTICAL   TIC LABEL THICKNESS
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.1)GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'THIC')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.IHARG(3).EQ.'THIC')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      PHOLD=PDEFTH
      GO TO 1180
!
 1160 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      PHOLD=PDEFTH
      GO TO 1280
!
 1260 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK LABEL THICKNESS (ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)PHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      PHOLD=PDEFTH
      GO TO 1380
!
 1360 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)PHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      PHOLD=PDEFTH
      GO TO 1480
!
 1460 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)PHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      PHOLD=PDEFTH
      GO TO 1580
!
 1560 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)PHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      PHOLD=PDEFTH
      GO TO 1680
!
 1660 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)PHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      PHOLD=PDEFTH
      GO TO 1780
!
 1760 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      PTIZTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK LABEL THICKNESS (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)PHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTLTH
      SUBROUTINE DPTMCO(XTEMP1,XTEMP2,MAXNXT,ICASAN,   &
                        ICAPSW,IFORSW,IMULT,IREPL,   &
                        ISUBRO,IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE CONFIDENCE LIMITS FOR THE TRIMMED MEAN
!              FOR PROBABILITY VALUE P = .90, .95, .99, .999, AND .9999.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     REFERENCE--"INTRODUCTION TO ROBUST ESTIMATION AND HYPOTHESIS
!                TESTING", RAND R. WILCOX, ACADEMIC PRESS, 1997.
!                1977.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/2
!     ORIGINAL VERSION--FEBRUARY  2003.
!     UPDATED         --OCTOBER   2003. ADD SUPPORT FOR HTML, LATEX
!                                       OUTPUT
!     UPDATED         --MARCH     2010. USE DPDTA1, DPDTA4 TO GENERATE
!                                       HTML, LATEX, RTF FORMAT
!     UPDATED         --MARCH     2010. SUPPORT FOR MULTIPLE RESPONSE
!                                       VARIABLES AND FOR GROUP-ID
!                                       VARIABLES (I.E., REPLICATION
!                                       CASE)
!     UPDATED         --MARCH     2010. USE DPPAR3 TO EXTRACT EITHER A
!                                       RESPONSE VARIABLE OR A MATRIX
!                                       NAME
!     UPDATED         --OCTOBER   2012. TRIMMING CAN BE SPECIFIED EITHER
!                                       AS A PROPORTION OR AS A SPECIFIC
!                                       NUMBER TO TRIM
!     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFLAGU
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
!
      LOGICAL IFRST
      LOGICAL ILAST
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION W(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
!
      DIMENSION XDESGN(MAXOBV,6)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTM'
      ISUBN2='CO  '
      IFOUND='YES'
      IERROR='NO'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               *****************************************************
!               **  TREAT THE TRIMMED MEAN CONFIDENCE LIMITS CASE  **
!               *****************************************************
!
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      IF(NUMVAR.GT.1 .AND. IREPL.EQ.'OFF')IMULT='ON'
!
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
!               ***********************************************
!               **  STEP 2--                                 **
!               **  DETERMINE:                               **
!               **  1) NUMBER OF REPLICATION VARIABLES (0-6) **
!               **  2) NUMBER OF RESPONSE    VARIABLES (>= 1)**
!               ***********************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NRESP=0
      NREPL=0
!
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
  211     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',   &
                 'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,213)NREPL
  213     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
        NRESP=1
      ENDIF
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')THEN
        WRITE(ICOUT,221)NRESP,NREPL
  221   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 230 I=1,MAXN
        W(I)=1.0
  230 CONTINUE
!
!     ******************************************************
!     **  STEP 3--                                        **
!     **  DETERMINE VALUE OF TRIMMING CONSTANTS (OBTAINED **
!     **  FROM PARAMETERS P1 AND P2)                      **
!     ******************************************************
!
!
!        2012/10: FOR TRIMMED MEAN, CAN SPECIFY EITHER A SPECIFIC NUMBER
!                 TO TRIM OR A PERCENTAGE TO TRIM.  CHECK FOR SPECIFIC
!                 NUMBER FIRST AND IF NOT SPECIFIED, CHECK FOR A
!                 PERCENTAGE.
!
        NTRIM1=-1
        NTRIM2=-1
        P1=-99.0
        P2=-99.0
!
        IH='NTRI'
        IH2='M1  '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'NO')THEN
          NTRIM1=INT(VALUE(ILOCP)+0.1)
          IF(NTRIM1.LT.0)NTRIM1=0
        ENDIF
!
        IH='NTRI'
        IH2='M2  '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'NO')THEN
          NTRIM2=INT(VALUE(ILOCP)+0.1)
          IF(NTRIM2.LT.0)NTRIM2=0
        ENDIF
!
        IF(NTRIM1.LE.0)THEN
          IH='P1  '
          IH2='    '
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
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
            GO TO 9000
          ELSE
            PROP1=VALUE(ILOCP)
          ENDIF
        ENDIF
!
        IF(NTRIM2.LE.0)THEN
          IH='P2  '
          IH2='    '
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
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
            GO TO 9000
          ELSE
            PROP2=VALUE(ILOCP)
          ENDIF
        ENDIF
!
!
!               ******************************************************
!               **  STEP 3--                                        **
!               **  GENERATE THE CONFIDENCE LIMITS FOR THE VARIOUS  **
!               **  CASES                                           **
!               ******************************************************
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************************************
!               **  STEP 3A--                          **
!               **  CASE 1: SINGLE RESPONSE VARIABLE   **
!               **          WITH NO REPLICATION        **
!               *****************************************
!
      IF(IMULT.EQ.'OFF' .AND. NREPL.EQ.0)THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
!
        ICOL=1
        NUMVA2=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  STEP 3B--                                       **
!               **  PREPARE FOR ENTRANCE INTO DPTMC2--              **
!               **  SET THE WEIGHT VECTOR TO UNITY THROUGHOUT.      **
!               ******************************************************
!
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
          DO 335 I=1,N
            WRITE(ICOUT,336)I,Y(I)
  336       FORMAT('I,Y(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  335     CONTINUE
        ENDIF
!
        CALL DPTMC2(Y,NLOCAL,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                    XTEMP1,XTEMP2,MAXNXT,   &
                    PID,IVARID,IVARI2,NREPL,   &
                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                    CTL999,CTU999,   &
                    ICAPSW,ICAPTY,IFORSW,   &
                    ICASAN,ISUBRO,IBUGA3,IERROR)
!
        IFLAGU='ON'
        IFRST=.FALSE.
        ILAST=.FALSE.
        CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                    CTL999,CTU999,   &
                    IFLAGU,IFRST,ILAST,ICASAN,   &
                    IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!               *******************************************
!               **  STEP 4A--                            **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES  **
!               *******************************************
!
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
        NCURVE=0
        DO 410 IRESP=1,NRESP
          NCURVE=NCURVE+1
!
          IINDX=ICOLR(IRESP)
          PID(1)=CPUMIN
          IVARID(1)=IVARN1(IRESP)
          IVARI2(1)=IVARN2(IRESP)
!
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,411)IRESP,NCURVE
  411       FORMAT('IRESP,NCURVE = ',2I5)
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
                      Y,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!         *****************************************************
!         **  STEP 4B--                                      **
!         *****************************************************
!
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
              DO 425 I=1,NLOCAL
                WRITE(ICOUT,426)I,Y(I)
  426           FORMAT('I,Y(I) = ',I8,F12.5)
                CALL DPWRST('XXX','BUG ')
  425         CONTINUE
            ENDIF
          ENDIF
!
          CALL DPTMC2(Y,NLOCAL,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                      XTEMP1,XTEMP2,MAXNXT,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                      CTL999,CTU999,   &
                      ICAPSW,ICAPTY,IFORSW,   &
                      ICASAN,ISUBRO,IBUGA3,IERROR)
!
          IFLAGU='FILE'
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                      CTL999,CTU999,   &
                      IFLAGU,IFRST,ILAST,ICASAN,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
!
  410   CONTINUE
!
!               ****************************************************
!               **  STEP 5A--                                     **
!               **  CASE 3: ONE OR MORE REPLICATION VARIABLES.    **
!               **          FOR THIS CASE, ALL VARIABLES MUST     **
!               **          HAVE THE SAME LENGTH.                 **
!               ****************************************************
!
      ELSEIF(IREPL.EQ.'ON')THEN
        ISTEPN='5A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO 510 I=1,IMAX
          IF(ISUB(I).EQ.0)GO TO 510
          J=J+1
!
!         RESPONSE VARIABLE IN Y
!
          ICOLC=1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)Y(J)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)Y(J)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)Y(J)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)Y(J)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)Y(J)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)Y(J)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)Y(J)=TAGPLO(I)
!
          IF(NREPL.GE.1)THEN
            DO 520 IR=1,MIN(NREPL,6)
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
!
  510   CONTINUE
        NLOCAL=J
!
        ISTEPN='5B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TMCO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
        IADD=1
        DO 540 II=1,NREPL
          IVARID(II+IADD)=IVARN1(II+IADD)
          IVARI2(II+IADD)=IVARN2(II+IADD)
  540   CONTINUE
!
!       *****************************************************
!       **  STEP 5C--                                      **
!       **                                                 **
!       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
!       **  VARIOUS REPLICATIONS.                          **
!       *****************************************************
!
!
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
            DO 545 I=1,NLOCAL
              WRITE(ICOUT,546)I,Y(I),XDESGN(I,1),XDESGN(I,2)
  546         FORMAT('I,Y(I),XDESGN(I,1),XDESGN(I,2) = ',   &
                     I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
  545       CONTINUE
          ENDIF
        ENDIF
!
!       *****************************************************
!       **  STEP 5C--                                      **
!       **  FIND THE DISTINCT VALUES IN EACH OF THE        **
!       **  REPLICATION VARIABLES.                         **
!       *****************************************************
!
        CALL DPPP5(XDESGN(1,1),XDESGN(1,2),XDESGN(1,3),   &
                   XDESGN(1,4),XDESGN(1,5),XDESGN(1,6),   &
                   NREPL,NLOCAL,MAXOBV,   &
                   XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,   &
                   XTEMP1,XTEMP2,   &
                   NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,   &
                   IBUGA3,ISUBRO,IERROR)
!
!       *****************************************************
!       **  STEP 5D--                                      **
!       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
!       *****************************************************
!
        NPLOTP=0
        NCURVE=0
        IF(NREPL.EQ.1)THEN
          J=0
          DO 1110 ISET1=1,NUMSE1
            K=0
            PID(IADD+1)=XIDTEM(ISET1)
            DO 1130 I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                          XTEMP1,XTEMP2,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
!
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NUMSE1)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO 1210 ISET1=1,NUMSE1
          DO 1220 ISET2=1,NUMSE2
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            DO 1290 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2)   &
                )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                          XTEMP1,XTEMP2,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
 1220     CONTINUE
 1210     CONTINUE
        ELSEIF(NREPL.EQ.3)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3
          DO 1310 ISET1=1,NUMSE1
          DO 1320 ISET2=1,NUMSE2
          DO 1330 ISET3=1,NUMSE3
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            DO 1390 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.   &
                 XIDTE3(ISET3).EQ.XDESGN(I,3)   &
                )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                          XTEMP1,XTEMP2,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
 1330     CONTINUE
 1320     CONTINUE
 1310     CONTINUE
        ELSEIF(NREPL.EQ.4)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4
          DO 1410 ISET1=1,NUMSE1
          DO 1420 ISET2=1,NUMSE2
          DO 1430 ISET3=1,NUMSE3
          DO 1440 ISET4=1,NUMSE4
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            DO 1490 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.   &
                 XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.   &
                 XIDTE4(ISET4).EQ.XDESGN(I,4)   &
                )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                          XTEMP1,XTEMP2,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
 1440     CONTINUE
 1430     CONTINUE
 1420     CONTINUE
 1410     CONTINUE
        ELSEIF(NREPL.EQ.5)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4*NUMSE5
          DO 1510 ISET1=1,NUMSE1
          DO 1520 ISET2=1,NUMSE2
          DO 1530 ISET3=1,NUMSE3
          DO 1540 ISET4=1,NUMSE4
          DO 1550 ISET5=1,NUMSE5
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            PID(5+IADD)=XIDTE5(ISET4)
            DO 1590 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.   &
                 XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.   &
                 XIDTE4(ISET4).EQ.XDESGN(I,4) .AND.   &
                 XIDTE5(ISET5).EQ.XDESGN(I,5)   &
                )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                          XTEMP1,XTEMP2,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
 1550     CONTINUE
 1540     CONTINUE
 1530     CONTINUE
 1520     CONTINUE
 1510     CONTINUE
        ELSEIF(NREPL.EQ.6)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4*NUMSE5*NUMSE6
          DO 1610 ISET1=1,NUMSE1
          DO 1620 ISET2=1,NUMSE2
          DO 1630 ISET3=1,NUMSE3
          DO 1640 ISET4=1,NUMSE4
          DO 1650 ISET5=1,NUMSE5
          DO 1660 ISET6=1,NUMSE6
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            PID(5+IADD)=XIDTE5(ISET4)
            PID(6+IADD)=XIDTE6(ISET4)
            DO 1690 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.   &
                 XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.   &
                 XIDTE4(ISET4).EQ.XDESGN(I,4) .AND.   &
                 XIDTE5(ISET5).EQ.XDESGN(I,5) .AND.   &
                 XIDTE6(ISET6).EQ.XDESGN(I,6)   &
                )THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTMC2(TEMP1,NTEMP,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                          XTEMP1,XTEMP2,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
 1660     CONTINUE
 1650     CONTINUE
 1640     CONTINUE
 1630     CONTINUE
 1620     CONTINUE
 1610     CONTINUE
        ENDIF
!
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPTMCO
      SUBROUTINE DPTMC2(Y,N,W,PROP1,PROP2,NTRIM1,NTRIM2,   &
                        XTEMP1,XTEMP2,MAXNXT,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        ICASAN,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE GENERATES TRIMMED MEAN CONFIDENCE LIMITS
!              FOR THE DATA IN THE INPUT VECTOR Y.
!     NOTE--ASSUMPTION--MODEL IS   RESPONSE = CONSTANT + ERROR.
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS
!                       N      = THE INTEGER NUMBER OF
!                                OBSERVATIONS IN THE VECTOR Y.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/2
!     ORIGINAL VERSION--FEBRUARY  2003.
!     UPDATED         --OCTOBER   2003. ADD SUPPORT FOR HTML, LATEX
!                                       OUTPUT
!
!     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
!     UPDATED         --MARCH     2010. USE DPDTA2 AND DPDTA4 TO
!                                       GENERATE OUTPUT (ADDS RTF
!                                       SUPPORT)
!     UPDATED         --MARCH     2010. SOME MODIFICATIONS TO THE
!                                       OUTPUT (AESTHETIC, NOT
!                                       SUBSTANTIVE)
!     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION W(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION PID(*)
!
      PARAMETER (NUMALP=8)
!
      DIMENSION CONF(NUMALP)
      DIMENSION T(NUMALP)
      DIMENSION TSDM(NUMALP)
      DIMENSION ALOWER(NUMALP)
      DIMENSION AUPPER(NUMALP)
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITEXT(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL IFRST
      LOGICAL ILAST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTM'
      ISUBN2='C2  '
      IWRITE='OFF'
      IERROR='NO'
      ICASA2='TMCO'
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTMC2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)N,NUMDIG,PROP1,PROP2,IBUGA3,ICASAN
   52   FORMAT('N,NUMDIG,PROP1,PROP2,IBUGA3,ICASAN = ',   &
               2I8,2X,2G15.7,2X,A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),W(I)
   57     FORMAT('I,Y(I),W(I) = ',I8,2G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN TRIMMED MEAN--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 5')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N
  113   FORMAT('SAMPLE SIZE = ',I8)
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
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
  139 CONTINUE
!
!               ***************************************************
!               **  STEP 3--                                     **
!               **  COMPUTE THE TRIMMED MEAN LOCATION ESTIMATE   **
!               **  COMPUTE THE TRIMMED MEAN STANDARD ERROR      **
!               ***************************************************
!
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
!
      CALL TRIMME(Y,N,PROP1,PROP2,NTRIM1,NTRIM2,IWRITE,XTEMP1,   &
                  MAXNXT,YTRMME,   &
                  IBUGA3,ISUBRO,IERROR)
      CALL TRIMSE(Y,N,PROP1,PROP2,NRIM1,NTRIM2,IWRITE,XTEMP1,XTEMP2,   &
                  MAXNXT,YTRMSE,   &
                  IBUGA3,ISUBRO,IERROR)
!
      AN1=N
      LAMBDA=INT(AN1*(PROP1+PROP2)/100.)
      V=0.7*(AN1-1.0)
      IV=N - LAMBDA - 1
      IF(IV.LT.1)IV=1
!
!               ***************************************
!               **  STEP 4--                         **
!               **  COMPUTE CONFIDENCE LIMITS        **
!               **  FOR VARIOUS PROBABILITY VALUES.  **
!               ***************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CONF(1)=50.0
      CONF(2)=75.0
      CONF(3)=90.0
      CONF(4)=95.0
      CONF(5)=99.0
      CONF(6)=99.9
      CONF(7)=99.99
      CONF(8)=99.999
!
      DO 1400 I=1,8
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
!
!     ADD A FUDGE FACTOR SO THAT CONFIDENCE LEVEL WILL
!     BE PRINTED CORRECTLY TO 3 DECIMAL PLACES.
!
      CONF(1)=50.0001
      CONF(2)=75.0001
      CONF(3)=90.0001
      CONF(4)=95.0001
      CONF(5)=99.0001
      CONF(6)=99.9001
      CONF(7)=99.9901
      CONF(8)=99.9991
!
!               ****************************
!               **  STEP 7--              **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
      ITITLE='Confidence Limits for the Trimmed Mean'
      NCTITL=38
      ITITLZ='(Two-Sided)'
      NCTITZ=11
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
        DO 4101 I=1,NREPL
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
!
      NUMROW=ICNT
      DO 4210 I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='5A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TMC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='5B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CNF2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDT11(CONF,T,TSDM,ALOWER,AUPPER,   &
                  ICASA2,ICAPSW,ICAPTY,NUMDIG,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPTMC2
      SUBROUTINE DPTNS1(Y,X,N,T,   &
                        TEMP1,   &
                        MUMOME,SDMOME,MUML,SDML,   &
                        MUMLSE,SDMLSE,COVSE,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE ESTIMATES THE PARAMETERS FOR THE
!              "DETECTION LIMIT PLOT" COMMAND.  NOTE THAT THIS
!              IS ACTUALLY A SINGLY LEFT CENSORED PROBLEM (THE
!              DISTINCTION BETWEEN CENSORING AND TRUNCATION IS
!              THAT FOR THE CENSORED CASE WE KNOW HOW MANY
!              MEASUREMENTS ARE RESTRICTED WHILE FOR THE TRUNCATED
!              CASE WE DO NOT.
!
!              THE 3-MOMENT  ESTIMATES ARE:
!
!                  SIGMA* = SQRT{(V1P**2 - V1P*V2P)/(V2P - 2*V1P**2)}
!                  MU*    = T + A*
!
!              WHERE
!
!                  A*   = (V3P - 2*V1P*V2P)/(V2P - 2*V1P**2)
!                  V1P  = XBAR - T
!                  V2P = S**2 + (XBAR - T)**2
!                  V3P = SUM[i=1 to n][(X(i) - XBAR)**3]/n
!
!              THE MAXIMUM LIKELIHOOD ESTIMATES ARE:
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
      DIMENSION TEMP1(*)
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVARI
      DOUBLE PRECISION DT
      DOUBLE PRECISION V1P
      DOUBLE PRECISION V2P
      DOUBLE PRECISION V3P
      DOUBLE PRECISION DNTOT
      DOUBLE PRECISION DNFULL
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DNUM1
      DOUBLE PRECISION DNUM2
      DOUBLE PRECISION DDENOM
      DOUBLE PRECISION DDENO2
      DOUBLE PRECISION DOMEGA
      DOUBLE PRECISION DLAMB
      DOUBLE PRECISION DQ
      DOUBLE PRECISION DPHI11
      DOUBLE PRECISION DPHI12
      DOUBLE PRECISION DPHI22
      DOUBLE PRECISION DU11
      DOUBLE PRECISION DU12
      DOUBLE PRECISION DU22
!
      REAL MUMOME
      REAL SDMOME
      REAL MUML
      REAL SDML
      REAL MUMLSE
      REAL SDMLSE
!
      DOUBLE PRECISION AE
      DOUBLE PRECISION RE
      DOUBLE PRECISION XLOW
      DOUBLE PRECISION XUP
      DOUBLE PRECISION XMID
      DOUBLE PRECISION XI
!
      DOUBLE PRECISION TNRFUN
      EXTERNAL TNRFUN
!
      DOUBLE PRECISION DC1
      DOUBLE PRECISION DH
      COMMON/TNRCOM/DC1,DH
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTN'
      ISUBN2='S1  '
      IERROR='NO'
      IWRITE='OFF'
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LE.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN TRUNCATED NORMAL SINGLY TRUNCATED ',   &
               'PARAMETER ESTIMATION--')
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TNS1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPTNS1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)N
   71   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 73 I=1,N
          WRITE(ICOUT,74)I,Y(I),X(I)
   74     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
!
!               **********************************************
!               **  STEP 2--                                **
!               **  COMPUTE SUMMARY STATISTICS              **
!               **********************************************
!
      MUMOME=0.0
      SDMOME=0.0
      MUML=0.0
      SDML=0.0
!
      NC=0
      NFULL=0
      YMIN=CPUMAX
      DSUM1=0.0D0
!
      DO 1010 I=1,N
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
!
      IF(NFULL.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1012)
 1012   FORMAT('      THE NUMBER OF UNTRUNCATED OBSERVATIONS MUST BE ',   &
               'AT LEAST 2.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1014)NFULL
 1014   FORMAT('      THE NUMBER OF UNTRUNCATED OBSERVATIONS HERE = ',   &
               I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DVARI=0.0D0
      V3P=0.0D0
      DO 1020 I=1,NFULL
        DVARI=DVARI + (DBLE(TEMP1(I)) - DMEAN)**2/DNFULL
        V3P=V3P + (DBLE(TEMP1(I)) - DT)**3/DNFULL
 1020 CONTINUE
      V1P=DMEAN - DT
      V2P=DVARI + (DMEAN - DT)**2
!
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
!
!               **********************************************
!               **  STEP 3--                                **
!               **  COMPUTE 3-MOMENT ESTIMATES              **
!               **********************************************
!
      DNUM1=V2P**2 - V1P*V3P
      DDENOM=V2P - 2.0D0*V1P**2
      SDMOME=REAL(DSQRT(DNUM1/DDENOM))
      DNUM2=V3P - 2.0D0*V1P*V2P
      DDENO2=V2P - 2.0D0*V1P**2
      MUMOME=REAL(DT + (DNUM2/DDENO2))
!
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
!
!               **********************************************
!               **  STEP 4--                                **
!               **  COMPUTE MAXIMUM LIKELIHOOD ESTIMATES    **
!               **********************************************
!
!     DEFINE SOME CONSTANTS FOR THE FUNCTION SOLVER
!
      DH=DNC/DNTOT
      DC1=DVARI/(DMEAN - DT)**2
!
!     USE DFZERO TO SOLVE THE LAMBDAHAT FUNCTION
!
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
!
 1410 CONTINUE
      CALL DFZERO(TNRFUN,XLOW,XUP,XMID,RE,AE,IFLAG)
      XI=XLOW
!
!     NOW EVALUATE - CHECK FOR POSITIVE RESULT
!
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
          GO TO 1410
        ELSEIF(ITER.EQ.1)THEN
          ITER=2
          XLOW=XI+0.1D0
          XUP=10.0D0
          XMID=(XLOW+XUP)/2.0D0
          GO TO 1410
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1413)
 1413     FORMAT('      UNABLE TO DETERMINE MAXIMUM LIKELIHOOD ',   &
                 'ESTIMATES.')
          CALL DPWRST('XXX','BUG ')
          GO TO 1499
        ENDIF
      ENDIF
!
      SDML=REAL(DSQRT(DVARI + DLAMB*(DMEAN - DT)**2))
      MUML=REAL(DMEAN - DLAMB*(DMEAN - DT))
!
!     NOW COMPUTE STANDARD ERRORS
!
      IF(DCDF.GE.1.0D0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1431)
 1431   FORMAT('***** WARNING IN TRUNCATED NORMAL SINGLY TRUNCATED ',   &
               'PARAMETER ESTIMATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1433)
 1433   FORMAT('      UNABLE TO COMPUTE STANDARD ERRORS OF THE ',   &
               'MAXIMUM LIKELIHOOD ESTIMATES.')
        CALL DPWRST('XXX','BUG ')
        GO TO 1499
      ENDIF
!
      DQ=DPDF/(1.0D0 - DCDF)
      DPHI11=1.0D0 - DQ*(DQ - XI)
      DPHI12=DQ*(1.0D0 - XI*(DQ - XI))
      DPHI22=2.0D0 + XI*DPHI12
      DDENOM=DPHI11*DPHI22 - DPHI12**2
      DU11=DPHI22/DDENOM
      DU22=DPHI11/DDENOM
      DU12=-DPHI12/DDENOM
!CCCC DTERM1=DBLE(SDML)**2/DBLE(NFULL)
      DTERM1=DBLE(SDML)**2/DNTOT
      MUMLSE=REAL(DSQRT(DTERM1*DU11))
      SDMLSE=REAL(DSQRT(DTERM1*DU22))
      COVSE=REAL(DTERM1*DU12)
!
 1499 CONTINUE
!
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
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TNS1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTNS1--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPTNS1
      SUBROUTINE DPTOLI(XTEMP1,XTEMP2,XTEMP3,MAXNXT,   &
                        ICASAN,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE TOLERANCE LIMITS
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     EXAMPLE--TOLERANCE LIMITS Y
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/11
!     ORIGINAL VERSION--NOVEMBER  1998.
!     UPDATED         --MARCH     2011. USE DPPARS ROUTINE
!     UPATED          --MARCH     2011. REWRITTEN TO HANDLE MULTIPLE
!                                       RESPONSE VARIABLES, GROUP-ID
!                                       VARIABLES, OR A LAB-ID VARIABLE
!     UPATED          --AUGUST    2011. CHECK FOR CONFLICT WITH ABASIS AND
!                                       BBASIS TOLERANCE INTERVALS
!     UPATED          --AUGUST    2011. ADD ONE-SIDED CASE FOR NORMAL TOLERANCE
!                                       LIMITS
!     UPATED          --AUGUST    2011. ADD SUMMARY DATA FOR NORMAL TOLERANCE
!                                       LIMITS (I.E., MEAN, SD, SAMPLE SIZE)
!     UPATED          --AUGUST    2011. ADD WEIBULL TOLERANCE LIMITS
!     UPATED          --MAY       2014. ADD LOGNORMAL TOLERANCE LIMITS
!     UPATED          --MAY       2014. ADD BOX COX TOLERANCE LIMITS
!     UPATED          --JULY      2019. TWEAK SCRATCH SPACE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICASDI
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IDATSW
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
      CHARACTER*4 ICASE
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION Y1(MAXOBV)
!
      DIMENSION XDESGN(MAXOBV,7)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
!
      DIMENSION TEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP1(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ***********************************************
!               **  TREAT THE TOLERANCE LIMITS TEST  CASE    **
!               ***********************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TOLI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTOLI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASAN,MAXNXT
   52   FORMAT('ICASAN,MAXNXT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE COMMAND                            **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:        **
!               **    1) TOLERANCE LIMITS Y                        **
!               **    2) MULTIPLE TOLERANCE LIMITS Y1 ... YK       **
!               **    3) REPLICATED TOLERANCE LIMITS Y X1 ... XK   **
!               *****************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTC=9999
      ILASTZ=9999
      IEQLAST=9999
      ICASAN='TOLE'
      IDATSW='RAW'
!
      DO 100 I=0,NUMARG-1
!
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
        ELSE
          ICTMP1=IHARG(I)
        ENDIF
        ICTMP2=IHARG(I+1)
        ICTMP3=IHARG(I+2)
        ICTMP4=IHARG(I+3)
!
        IF(IHARG(I).EQ.'SUBS' .AND. IHARG2(I).EQ.'ET  ')IEQLAST=I-1
        IF(ICTMP1.EQ.'=' .AND. I.LE.IEQLAST)THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'ABAS')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'A   ' .AND. ICTMP2.EQ.'BASI')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'BBAS')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'B   ' .AND. ICTMP2.EQ.'BASI')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'TOLE' .AND.   &
              (ICTMP2.EQ.'LIMI' .OR. ICTMP2.EQ.'INTE'))THEN
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
!
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')THEN
        WRITE(ICOUT,91)ICASAN,IMULT,IREPL,ISHIFT
   91   FORMAT('DPTOLI: ICASAN,IMULT,IREPL,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN TOLERANCE LIMITS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE TOLERANCE LIMITS TEST COMMAND.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(IDATSW.EQ.'SUMM')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,112)
  112     FORMAT('      YOU CANNOT SPECIFY BOTH "SUMMARY" AND ',   &
                 '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(IMULT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)
  122     FORMAT('      YOU CANNOT SPECIFY BOTH "SUMMARY" AND ',   &
                 '"MULTIPLE"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(ICASDI.EQ.'WEIB')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,132)
  132     FORMAT('      YOU CANNOT SPECIFY BOTH "SUMMARY" AND ',   &
                 '"WEIBULL"')
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
  142     FORMAT('      YOU CANNOT SPECIFY BOTH "SUMMARY" AND ',   &
                 '"LOGNORMAL"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
        ENDIF
      ENDIF
!
!               *********************************
!               **  STEP 4--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!               ***********************************************
!               **  STEP 5--                                 **
!               **  DETERMINE:                               **
!               **  1) NUMBER OF REPLICATION VARIABLES (0-6) **
!               **  2) NUMBER OF RESPONSE    VARIABLES (>= 1)**
!               ***********************************************
!
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IDATSW.EQ.'SUMM')GO TO 599
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
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',   &
                 'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN ONE AND SIX.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
        NRESP=NUMVAR
        IMULT='ON'
      ENDIF
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
  599 CONTINUE
!
!               ******************************************************
!               **  STEP 6--                                        **
!               **  GENERATE THE TOLERANCE LIMITS TEST FOR THE      **
!               **  VARIOUS CASES                                   **
!               ******************************************************
!
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ******************************************
!               **  STEP 7A--                           **
!               **  CASE 0: SUMMARY CASE                **
!               ******************************************
!
      IF(IDATSW.EQ.'SUMM')THEN
        ISTEPN='7A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       TWO CASES: EITHER DATA ENTERED AS 3 PARAMETERS OR
!                  AS 3 VARIABLES
!
        NREPL=0
        IF(IVARTY(1).EQ.'PARA')THEN
          XMEAN=PVAR(1)
          XSD=PVAR(2)
          AN=PVAR(3)
          PID(1)=CPUMIN
          IVARID(1)='ROW '
          IVARI2(1)='1  '
          IF(ICASA2.EQ.'LOWE')THEN
            CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,   &
                      ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      ISUBRO,IBUGA3,IERROR)
          ELSEIF(ICASA2.EQ.'UPPE')THEN
            CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,   &
                      ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      ISUBRO,IBUGA3,IERROR)
          ELSE
            CALL TOL(Y1,NLOCAL,XMEAN,XSD,AN,   &
                     XTEMP1,XTEMP2,XTEMP3,   &
                     ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                     PID,IVARID,IVARI2,NREPL,   &
                     ISUBRO,IBUGA3,IERROR)
          ENDIF
        ELSE
          ICOL=1
          NUMVA2=3
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y1,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          DO 710 IROW=1,NLOCAL
!
            PID(1)=CPUMIN
            IVARID(1)='ROW '
            WRITE(IVARI2(1)(1:4),'(I4)')IROW
            XMEAN=Y1(IROW)
            XSD=XTEMP1(IROW)
            AN=XTEMP2(IROW)
!
            IF(ICASA2.EQ.'LOWE')THEN
              CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,   &
                        ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        ISUBRO,IBUGA3,IERROR)
            ELSEIF(ICASA2.EQ.'UPPE')THEN
              CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,   &
                        ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        ISUBRO,IBUGA3,IERROR)
            ELSE
              CALL TOL(Y1,NLOCAL,XMEAN,XSD,AN,   &
                       XTEMP1,XTEMP2,XTEMP3,   &
                       ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                       PID,IVARID,IVARI2,NREPL,   &
                       ISUBRO,IBUGA3,IERROR)
            ENDIF
!
  710     CONTINUE
        ENDIF
        GO TO 9000
      ENDIF
!
!
!               ******************************************
!               **  STEP 8A--                           **
!               **  CASE 1: NO REPLICATION VARIABLES    **
!               ******************************************
!
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
        NCURVE=0
        DO 810 IRESP=1,NRESP
          NCURVE=NCURVE+1
!
          IINDX=ICOLR(IRESP)
          PID(1)=CPUMIN
          IVARID(1)=IVARN1(IRESP)
          IVARI2(1)=IVARN2(IRESP)
!
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')THEN
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
                      Y1,XTEMP1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!         *****************************************************
!         **  STEP 8B--                                      **
!         *****************************************************
!
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TOLI')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPTOLI--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,IDATSW,NLOCAL
  823       FORMAT('ICASAN,NUMVAR,IDATSW,NQ = ',   &
                   A4,I8,2X,A4,I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO 825 I=1,NLOCAL
                WRITE(ICOUT,826)I,Y1(I)
  826           FORMAT('I,Y1(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
!
          IF(ICASDI.EQ.'WEIB')THEN
            CALL TOLWEI(Y1,NLOCAL,   &
                        MINMAX,IWEIBC,XTEMP1,DTEMP1,   &
                        ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        ISUBRO,IBUGA3,IERROR)
          ELSEIF(ICASA2.EQ.'LOWE')THEN
            CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,   &
                      ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      ISUBRO,IBUGA3,IERROR)
          ELSEIF(ICASA2.EQ.'UPPE')THEN
            CALL TOL2(Y1,NLOCAL,XMEAN,XSD,AN,   &
                      ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      ISUBRO,IBUGA3,IERROR)
          ELSE
            CALL TOL(Y1,NLOCAL,XMEAN,XSD,AN,   &
                     XTEMP1,XTEMP2,XTEMP3,   &
                     ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                     PID,IVARID,IVARI2,NREPL,   &
                     ISUBRO,IBUGA3,IERROR)
          ENDIF
!
  810   CONTINUE
!
!               ****************************************************
!               **  STEP 9A--                                     **
!               **  CASE 3: ONE OR MORE REPLICATION VARIABLES.    **
!               **          FOR THIS CASE, THE NUMBER OF RESPONSE **
!               **          VARIABLES MUST BE EXACTLY 1.          **
!               **          FOR THIS CASE, ALL VARIABLES MUST     **
!               **          HAVE THE SAME LENGTH.                 **
!               ****************************************************
!
      ELSEIF(NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TOLI')   &
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
          ICOLC=1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)Y1(J)=TAGPLO(I)
!
          IF(NREPL.GE.1)THEN
            DO 920 IR=1,MIN(NREPL,6)
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
!
  910   CONTINUE
        NLOCAL=J
!
!       *****************************************************
!       **  STEP 9B--                                      **
!       **  CALL TOL    TO PERFORM TOLERANCE LIMITS TEST.  **
!       *****************************************************
!
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TOLI')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPTOLI--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,942)ICASAN,NUMVAR,NLOCAL,NREPL
  942     FORMAT('ICASAN,NUMVAR,NLOCAL,NREPL = ',   &
                 A4,3I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 945 I=1,NLOCAL
              WRITE(ICOUT,946)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  946         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2) = ',   &
                     I8,4F12.5)
              CALL DPWRST('XXX','BUG ')
  945       CONTINUE
          ENDIF
        ENDIF
!
!       *****************************************************
!       **  STEP 9C--                                      **
!       **  FIND THE DISTINCT VALUES IN EACH OF THE        **
!       **  REPLICATION VARIABLES.                         **
!       *****************************************************
!
        CALL DPPP5(XDESGN(1,1),XDESGN(1,2),XDESGN(1,3),   &
                   XDESGN(1,4),XDESGN(1,5),XDESGN(1,6),   &
                   NREPL,NLOCAL,MAXOBV,   &
                   XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,   &
                   XTEMP1,XTEMP2,   &
                   NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,   &
                   IBUGA3,ISUBRO,IERROR)
!
!       *****************************************************
!       **  STEP 9D--                                      **
!       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
!       *****************************************************
!
        NPLOTP=0
        NCURVE=0
        IADD=1
!
        IF(NREPL.EQ.1)THEN
          J=0
          DO 1110 ISET1=1,NUMSE1
            K=0
            PID(IADD+1)=XIDTEM(ISET1)
            DO 1130 I=1,NLOCAL
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
                CALL TOLWEI(TEMP1,NTEMP,   &
                            MINMAX,IWEIBC,XTEMP1,DTEMP1,   &
                            ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                            PID,IVARID,IVARI2,NREPL,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                         XTEMP1,XTEMP2,XTEMP3,   &
                         ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                         PID,IVARN1,IVARN2,NREPL,   &
                         ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO 1210 ISET1=1,NUMSE1
          DO 1220 ISET2=1,NUMSE2
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            DO 1290 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2)   &
                )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,   &
                            MINMAX,IWEIBC,XTEMP1,DTEMP1,   &
                            ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                            PID,IVARID,IVARI2,NREPL,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                         XTEMP1,XTEMP2,XTEMP3,   &
                         ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                         PID,IVARN1,IVARN2,NREPL,   &
                         ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1220     CONTINUE
 1210     CONTINUE
        ELSEIF(NREPL.EQ.3)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3
          DO 1310 ISET1=1,NUMSE1
          DO 1320 ISET2=1,NUMSE2
          DO 1330 ISET3=1,NUMSE3
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            DO 1390 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.   &
                 XIDTE3(ISET3).EQ.XDESGN(I,3)   &
                )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,   &
                            MINMAX,IWEIBC,XTEMP1,DTEMP1,   &
                            ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                            PID,IVARID,IVARI2,NREPL,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                         XTEMP1,XTEMP2,XTEMP3,   &
                         ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                         PID,IVARN1,IVARN2,NREPL,   &
                         ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1330     CONTINUE
 1320     CONTINUE
 1310     CONTINUE
        ELSEIF(NREPL.EQ.4)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4
          DO 1410 ISET1=1,NUMSE1
          DO 1420 ISET2=1,NUMSE2
          DO 1430 ISET3=1,NUMSE3
          DO 1440 ISET4=1,NUMSE4
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            DO 1490 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.   &
                 XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.   &
                 XIDTE4(ISET4).EQ.XDESGN(I,4)   &
                )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,   &
                            MINMAX,IWEIBC,XTEMP1,DTEMP1,   &
                            ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                            PID,IVARID,IVARI2,NREPL,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                         XTEMP1,XTEMP2,XTEMP3,   &
                         ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                         PID,IVARN1,IVARN2,NREPL,   &
                         ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1440     CONTINUE
 1430     CONTINUE
 1420     CONTINUE
 1410     CONTINUE
        ELSEIF(NREPL.EQ.5)THEN
          J=0
          NTOT=NUMSE1*NUMSE2*NUMSE3*NUMSE4*NUMSE5
          DO 1510 ISET1=1,NUMSE1
          DO 1520 ISET2=1,NUMSE2
          DO 1530 ISET3=1,NUMSE3
          DO 1540 ISET4=1,NUMSE4
          DO 1550 ISET5=1,NUMSE5
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            PID(5+IADD)=XIDTE5(ISET4)
            DO 1590 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.   &
                 XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.   &
                 XIDTE4(ISET4).EQ.XDESGN(I,4) .AND.   &
                 XIDTE5(ISET5).EQ.XDESGN(I,5)   &
                )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,   &
                            MINMAX,IWEIBC,XTEMP1,DTEMP1,   &
                            ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                            PID,IVARID,IVARI2,NREPL,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                         XTEMP1,XTEMP2,XTEMP3,   &
                         ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                         PID,IVARN1,IVARN2,NREPL,   &
                         ISUBRO,IBUGA3,IERROR)
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
          DO 1610 ISET1=1,NUMSE1
          DO 1620 ISET2=1,NUMSE2
          DO 1630 ISET3=1,NUMSE3
          DO 1640 ISET4=1,NUMSE4
          DO 1650 ISET5=1,NUMSE5
          DO 1660 ISET6=1,NUMSE6
            K=0
            PID(1+IADD)=XIDTEM(ISET1)
            PID(2+IADD)=XIDTE2(ISET2)
            PID(3+IADD)=XIDTE3(ISET3)
            PID(4+IADD)=XIDTE4(ISET4)
            PID(5+IADD)=XIDTE5(ISET4)
            PID(6+IADD)=XIDTE6(ISET4)
            DO 1690 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2) .AND.   &
                 XIDTE3(ISET3).EQ.XDESGN(I,3) .AND.   &
                 XIDTE4(ISET4).EQ.XDESGN(I,4) .AND.   &
                 XIDTE5(ISET5).EQ.XDESGN(I,5) .AND.   &
                 XIDTE6(ISET6).EQ.XDESGN(I,6)   &
                )THEN
                K=K+1
                TEMP1(K)=Y1(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASDI.EQ.'WEIB')THEN
                CALL TOLWEI(TEMP1,NTEMP,   &
                            MINMAX,IWEIBC,XTEMP1,DTEMP1,   &
                            ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                            PID,IVARID,IVARI2,NREPL,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'LOWE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSEIF(ICASA2.EQ.'UPPE')THEN
                CALL TOL2(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                          ICASA2,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL TOL(TEMP1,NTEMP,XMEAN,XSD,AN,   &
                         XTEMP1,XTEMP2,XTEMP3,   &
                         ICASAN,ICAPSW,ICAPTY,IFORSW,   &
                         PID,IVARN1,IVARN2,NREPL,   &
                         ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
 1660     CONTINUE
 1650     CONTINUE
 1640     CONTINUE
 1630     CONTINUE
 1620     CONTINUE
 1610     CONTINUE
        ENDIF
!
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IF(IERROR.EQ.'YES')THEN
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9001)(IANS(I),I=1,MIN(100,IWIDTH))
 9001     FORMAT(100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
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
!
      RETURN
      END SUBROUTINE DPTOLI
      SUBROUTINE DPTOL3(X,N,XMEAN,XSD,AN,ANU,   &
                       ICASAN,ALPHA,GAMMA,ITOLGC,ITOLM2,   &
                       AK,ALOWLM,AUPPLM,   &
                       ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES NORMAL ONE-SIDED AND
!              TWO-SIDED NORMAL TOLERANCE LOWER AND UPPER LIMITS
!              AND K-FACTORS.  THIS IS FOR USE BY THE "STATISTICS"
!              COMMAND.
!     INPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED OR SORTED) OBSERVATIONS.
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!     OTHER DATAPAC   SUBROUTINES NEEDED--CHSPPF, NORPPF, NCTPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--GARDINER AND HULL, TECHNOMETRICS, 1966, PAGES 115-122
!               --WILKS, ANNALS OF MATHEMATICAL STATISTICS, 1941, PAGE 92
!               --MOOD AND GRABLE, PAGES 416-417
!               --HOWE (1969), "TWO-SIDED TOLERANCE LIMITS FOR NORMAL
!                 POPULATIONS - SOME IMPROVEMENTS", JOURNAL OF THE
!                 AMERICAN STATISTICAL ASSOCIATION, VOL. 64, PP.
!                 610-620.
!               --GUENTHER (1977), "SAMPLING INSPECTION IN STATISTICAL
!                 QUALITY CONTROL", GRIFFIN'S STATISTICAL MONOGRAPHS,
!                 NUMBER 37, LONDON.
!               --MARY NATRELLA (1963), "EXPERIMENTAL STATISTICS, NBS
!                 HANDBOOK 91", US DEPARTMENT OF COMMERCE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     ORIGINAL VERSION--MARCH     2011.
!     UPDATED         --MAY       2018. OPTION FOR NU (DEGREES OF
!                                       FREEDOM INDEPENDENT OF
!                                       CURRENT SAMPLE)
!     UPDATED         --MAY       2018. FOR 2-SIDED CASE, ADJUST FORMULA
!                                       SO THAT COVERAGE FACTORS < 0.5
!                                       WILL BE COMPUTED CORRECTLY.
!     UPDATED         --MAY       2018. OPTIONALLY COMPUTE GUENTHER
!                                       CORRECTION TO HOWE FORMULA
!                                       FOR TWO-SIDED CASE
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ITOLGC
      CHARACTER*4 ITOLM2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DOUBLE PRECISION DTEMP
      DOUBLE PRECISION DTEMP2
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DTERM1
!
      CHARACTER*4 IWRITE
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
!
      ISUBN1='TOL3'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TOL3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTOL3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,N,ALPHA,GAMMA
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,N,ALPHA,GAMMA = ',   &
               3(A4,2X),I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)XMEAN,XSD,ITOLGC,ITOLM2
   53   FORMAT('XMEAN,XSD,ITOLGC,ITOLM2 = ',2G15.7,2(2X,A4))
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
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL3')   &
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
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'TOL3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COMPUTE MEAN AND STANDARD DEVIATION
!
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
!
!     NOTE: ALPHA IS THE CONFIDENCE AND GAMMA IS THE COVERAGE
!
      IF(ALPHA.GE.1.0 .AND. ALPHA.LT.100.0)THEN
        ALPHA=ALPHA/100.
      ENDIF
      IF(ALPHA.GT.0.0 .AND. ALPHA.LT.1.0)THEN
        IF(ALPHA.LT.0.5)ALPHA=1.0 - ALPHA
      ELSE
        ALPHA=0.95
      ENDIF
!
      IF(GAMMA.GE.1.0 .AND. GAMMA.LT.100.0)THEN
        GAMMA=GAMMA/100.
      ENDIF
      IF(GAMMA.GT.0.0 .AND. GAMMA.LT.1.0)THEN
        IF(GAMMA.LT.0.5)GAMMA=1.0 - GAMMA
      ELSE
        GAMMA=0.95
      ENDIF
!
!     COMPUTE THE NORMAL TWO-SIDED TOLERANCE LIMITS USING HOWE'S METHOD,
!     OPTIONALLY APPLY GUENTHER'S CORRECTION
!
      AN=REAL(N)
      IF(ICASAN(1:1).EQ.'2')THEN
        IF(ANU.GT.0.0)THEN
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
!
!       APPLY GUENTHER CORRECTION IF REQUESTED
!
        IF(ITOLGC.EQ.'ON')THEN
           ANUM=AN - 3.0 - TERM3
           DENOM=2.0*(AN+1)**2
           TERM4=SQRT(1.0 + (ANUM/DENOM))
           AK=TERM4*AK
        ENDIF
!
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
!
      ELSEIF(ICASAN(1:1).EQ.'1')THEN
!
!       FOR ONE-SIDED INTERVAL, USE EITHER APPROXIMATION BASED ON
!       NON-CENTRAL T OR APPROXIMATION BASED ON NORMAL.  THE
!       NON-CENTRAL T IS CONSIDERED MORE ACCURATE, BUT NON-CENTRAL T
!       MAY LOSE ACCURACY AS N BECOMES LARGE.
!
!       FEBRUARY 2021: "DEFAULT" METHOD SHOULD USE NON-CENTRAL T FOR
!                      N <= 100, NORMAL APPROXIMATION OTHERWISE
!
        IF(ITOLM2.EQ.'NONC' .OR.   &
          (ITOLM2.EQ.'DEFA' .AND. N.LE.100))THEN
          IF(ANU.GT.0.0)THEN
            NU=INT(ANU+0.5)
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
        GO TO 9000
      ENDIF
      ALOWLM=XMEAN - AK*XSD
      AUPPLM=XMEAN + AK*XSD
!
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
!
      RETURN
      END SUBROUTINE DPTOL3
      SUBROUTINE DPTPCO(IHARG,NUMARG,IDETPC,MAXTEX,ITEPCO,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TEXT PATTERN COLORS = THE COLORS
!              OF THE LINES MAKING UP A PATTERN WITHIN A TEXT.
!              THESE ARE LOCATED IN THE VECTOR ITEPCO(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDETPC
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--ITEPCO (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDETPC
      CHARACTER*4 ITEPCO
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION ITEPCO(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTP'
      ISUBN2='CO  '
!
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEPCO(1)
   70 FORMAT('ITEPCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,ITEPCO(I)
   76 FORMAT('I,ITEPCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      ITEPCO(1)=IDETPC
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPC
      ITEPCO(I)=IHOLD2
 1225 CONTINUE
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEPCO(I)
 1276 FORMAT('THE COLOR OF TEXT PATTERN ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPC
      DO 1315 I=1,NUMTEX
      ITEPCO(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEPCO(I)
 1316 FORMAT('THE COLOR OF ALL TEXT PATTERNS',   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEPCO(1)
 9030 FORMAT('ITEPCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,ITEPCO(I)
 9036 FORMAT('I,ITEPCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTPCO
      SUBROUTINE DPTPLI(IHARG,IHARG2,NUMARG,IDETPL,MAXTEX,ITEPLI,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE PATTERN LINES = THE LINES TYPES
!              OF THE PATTERN WITHIN THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR ITEPLI(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDETPL
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--ITEPLI (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!     UPDATED         --AUGUST    1995.  DASH2 BUG
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IDETPL
      CHARACTER*4 ITEPLI
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
      DIMENSION ITEPLI(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTP'
      ISUBN2='LI  '
!
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEPLI(1)
   70 FORMAT('ITEPLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,ITEPLI(I)
   76 FORMAT('I,ITEPLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 9000
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      IF(NUMARG.EQ.5)GO TO 1150
      GO TO 1160
!
 1130 CONTINUE
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(5).EQ.'ALL')IHOLD1='    '
      IF(IHARG(5).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
!CCCC APRIL 1996.  CHANGE IHOLD TO IHOLD1 BELOW
      IF(IHARG(5).EQ.'ALL')THEN
        IHOLD1=IHARG(6)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'5')IHOLD1='DA5'
        GO TO 1300
      ENDIF
      IF(IHARG(6).EQ.'ALL')THEN
        IHOLD1=IHARG(5)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'5')IHOLD1='DA5'
        GO TO 1300
      ENDIF
      GO TO 1200
!
 1160 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.3)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      ITEPLI(1)='    '
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-3
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEPLI(I)
 1276 FORMAT('THE LINE TYPE FOR TEXT PATTERN ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPL
      DO 1315 I=1,NUMTEX
      ITEPLI(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEPLI(I)
 1316 FORMAT('THE LINE TYPE FOR ALL TEXT PATTERNS',   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEPLI(1)
 9030 FORMAT('ITEPLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,ITEPLI(I)
 9036 FORMAT('I,ITEPLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTPLI
      SUBROUTINE DPTPSP(IHARG,IARGT,ARG,NUMARG,PDETPS,MAXTEX,PTEPSP,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TEXT PATTERN SPACINGS = THE SPACINGS
!              BETWEEN THE LINES WHICH MAKE UP THE PATTERNS WITHIN THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR PTEPSP(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT  (A  CHARACTER VECTOR)
!                     --ARG
!                     --NUMARG
!                     --PDETPS
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--PTEPSP (A FLOATING POINT VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PTEPSP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTP'
      ISUBN2='SP  '
!
      NUMTEX=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PTEPSP(1)
   70 FORMAT('PTEPSP(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,PTEPSP(I)
   76 FORMAT('I,PTEPSP(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDETPS
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      PTEPSP(1)=PDETPS
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,PTEPSP(I)
 1276 FORMAT('THE SPACING BETWEEN (LINES WITHIN) PATTERN ',I6,   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETPS
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETPS
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETPS
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETPS
      DO 1315 I=1,NUMTEX
      PTEPSP(I)=HOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PTEPSP(I)
 1316 FORMAT('THE SPACING BETWEEN (LINES WITHIN) ALL PATTERNS',   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PTEPSP(1)
 9030 FORMAT('PTEPSP(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,PTEPSP(I)
 9036 FORMAT('I,PTEPSP(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTPSP
      SUBROUTINE DPTPTH(IHARG,IARGT,ARG,NUMARG,PDETPT,MAXTEX,PTEPTH,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TEXT PATTERN THICKNESSES = THE THICKNESSES
!              OF THE LINES WHICH MAKE UP THE PATTERNS WITHIN THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR PTEPTH(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT  (A  CHARACTER VECTOR)
!                     --ARG
!                     --NUMARG
!                     --PDETPT
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--PTEPTH (A FLOATING POINT VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PTEPTH(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTP'
      ISUBN2='TH  '
!
      NUMTEX=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PTEPTH(1)
   70 FORMAT('PTEPTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,PTEPTH(I)
   76 FORMAT('I,PTEPTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDETPT
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(2)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      PTEPTH(1)=PDETPT
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,PTEPTH(I)
 1276 FORMAT('THE THICKNESS OF (LINES WITHIN) PATTERN ',I6,   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETPT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETPT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETPT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETPT
      DO 1315 I=1,NUMTEX
      PTEPTH(I)=HOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PTEPTH(I)
 1316 FORMAT('THE THICKNESS OF (LINES WITHIN) ALL PATTERNS',   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PTEPTH(1)
 9030 FORMAT('PTEPTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,PTEPTH(I)
 9036 FORMAT('I,PTEPTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTPTH
      SUBROUTINE DPTPTY(IHARG,NUMARG,IDETPT,MAXTEX,ITEPTY,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE PATTERN TYPES = THE TYPES
!              OF THE PATTERN WITHIN THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR ITEPTY(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDETPT
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--ITEPTY (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDETPT
      CHARACTER*4 ITEPTY
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION ITEPTY(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTP'
      ISUBN2='TY  '
!
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEPTY(1)
   70 FORMAT('ITEPTY(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,ITEPTY(I)
   76 FORMAT('I,ITEPTY(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      ITEPTY(1)='    '
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPT
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPT
      ITEPTY(I)=IHOLD2
 1225 CONTINUE
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEPTY(I)
 1276 FORMAT('THE TYPE FOR TEXT PATTERN ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETPT
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETPT
      DO 1315 I=1,NUMTEX
      ITEPTY(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEPTY(I)
 1316 FORMAT('THE TYPE FOR ALL TEXT PATTERNS',   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEPTY(1)
 9030 FORMAT('ITEPTY(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,ITEPTY(I)
 9036 FORMAT('I,ITEPTY(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTPTY
      SUBROUTINE DPTREN(XTEMP2,MAXNXT,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT 3 TRENDS TEST FOR RELIABILITY ANALYSIS.
!              1) REVERSE ARRANGEMENTS TEST
!              2) MILITARY HANDBOOK TEST
!              3) LAPLACE TEST
!     EXAMPLES--LET TEND = <VALUE>; RELIABILITY TREND TEST Y
!             --LET TEND = <VALUE>; RELIABILITY TREND TEST Y GROUPID
!             --RELIABILITY TREND TEST Y GROUPID CENSOR
!     REFERENCE--TOBIAS AND TRINDADE (1995), "APPLIED RELIABILITY
!                ANALYSIS", SECOND EDITION, CHAPMAN & HALL/CRC,
!                PP. 344-354.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/5
!     ORIGINAL VERSION--MAY       1998.
!     UPDATED         --OCTOBER   2006. SUPPORT FOR MULTIPLE SYSTEMS
!     UPDATED         --OCTOBER   2006. CAPTURE HTML/LATEX/RTF
!     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR3
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
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION XTEMP2(*)
!
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XCEN(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTR'
      ISUBN2='EN  '
      IFOUND='YES'
      IERROR='NO'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      NGROUP=0
      NCENS=0
!
!               **********************************
!               **  TREAT THE TRENDS TEST CASE  **
!               **********************************
!
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TREN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I),PVAR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),PVAR(I) = ',I8,2X,A4,A4,2X,3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,X1,XCEN,NS,NGROUP,NCENS,ICASE,   &
                  IBUGA3,ISUBRO,IFOUND,IERROR)
!
!               *****************************************
!               **  STEP 3--                           **
!               **  CHECK TO SEE THE IF THE PARAMETER  **
!               **  TEND (TO SPECIFY THE CENSORING TIME)*
!               *****************************************
!
      IHP='TEND'
      IHP2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        TEND=CPUMIN
      ELSE
        TEND=VALUE(ILOCP)
      ENDIF
!
!               ***********************************************
!               **  STEP 4--                                 **
!               **  PREPARE FOR ENTRANCE INTO DPTREN2--      **
!               ***********************************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TREN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TREN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** FROM DPTREN, AS WE ARE ABOUT TO CALL DPTRE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)NS
 1212   FORMAT('NS = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 1215 I=1,NS
          WRITE(ICOUT,1216)I,Y1(I),X1(I),XCEN(I)
 1216     FORMAT('I,Y1(I),X1(I),XCEN(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 1215   CONTINUE
      ENDIF
!
      CALL DPTRE2(Y1,NS,X1,NGROUP,XCEN,NCENS,   &
                  ICAPSW,ICAPTY,IFORSW,   &
                  XTEMP2,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,   &
                  TEND,MAXNXT,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPTREN
      SUBROUTINE DPTRE2(Y,N,X1,NGROUP,XCEN,NCENS,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        XTEMP1,XIDTEM,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,   &
                        TEND,MAXNXT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT A TRENDS ANALYSIS
!              FOR THE DATA IN THE INPUT VECTOR Y.
!     INPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED) REPAIR/CENSORING TIMES.
!                    --X1     = THE OPTIONAL SINGLE PRECISION VECTOR
!                               GROUP-ID VALUES
!                    --XCEN   = THE OPTIONAL SINGLE PRECISION VECTOR
!                               OF CENSOR VALUES (1 = REPAIR
!                               TIME, 0 = CENSOR TIME).
!                      NY     = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR Y.
!                      NX     = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X1.
!                      NC     = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR XCEN.
!     REFERENCE--TOBIAS AND TRINDADE (1995), "APPLIED
!                RELIABILITY", SECOND EDITION, CHAPMAN AND HALL,
!                PP. 314.
!     NOTE--3 TRENDS TESTS ARE PERFORMED:
!           1) REVERSE ARRANGEMENT TEST
!           2) MILITARY HANDBOOK TEST
!           3) LAPLACE TEST
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI 77 FORTRAN.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/5
!     ORIGINAL VERSION--MAY       1998.
!     UPDATED         --OCTOBER   2006. SUPPORT FOR MULTIPLE SYSTEMS
!     UPDATED         --OCTOBER   2006. SUPPORT FOR HTML/LATEX/RFT
!                                       OUTPUT
!     UPDATED         --OCTOBER   2006. CHANGE OUTPUT FORMAT FOR
!                                       REVERSE ARRANGEMENT TEST
!                                       AND CORRECTED BUG IN THIS
!                                       TEST
!     UPDATED         --OCTOBER   2006. CODE FOR SINGLE TEST
!                                       EXTRACTED TO DPTRE3
!     UPDATED         --FEBRUARY  2011. USE DPDTA1 AND DPDTA5 TO PRINT
!                                       TABLES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DVAL2
      DOUBLE PRECISION DVAL3
!
      REAL MHTPVA
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X1(*)
      DIMENSION XCEN(*)
      DIMENSION XTEMP1(*)
      DIMENSION XIDTEM(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION TEMP6(*)
!
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
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTR'
      ISUBN2='E2  '
      IERROR='NO'
!
      MAXSYS=10000
!
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
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I),X1(I),XCEN(I)
   57     FORMAT('I,Y(I),X1(I),XCEN(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN RELIABILITY TREND TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
              'VARIABLE IS LESS THAN 4.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
  115   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 135 I=2,N
      IF(Y(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  139 CONTINUE
!
!               ********************************************
!               **  STEP 11--                             **
!               **  GENERATE THE RELIABILITY TREND TESTS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     CASE 1: NO GROUP OR CENSORING VARIABLE
!
      IF(NGROUP.EQ.0 .AND. NCENS.EQ.0)THEN
        ISET=1
        CALL DPTRE3(Y,N,XTEMP1,TEND,MAXNXT,   &
                    RATPVA,MHTPVA,DSUM1,DVAL2,DVAL3,   &
                    ISET,ICAPSW,ICAPTY,IFORSW,   &
                    ISUBRO,IBUGA3,IERROR)
        NUMSET=1
!
!       CASE 2: GROUP VARIABLE, BUT NO CENSORING VARIABLE
!
      ELSEIF(NCENS.EQ.0)THEN
!
!       STEP 1: DETERMINE UNIQUE GROUPS
!
        NUMSET=0
        DO 1051 I=1,N
          IF(NUMSET.EQ.0)GO TO 1053
          DO 1052 J=1,NUMSET
            IF(X1(I).EQ.XIDTEM(J))GO TO 1051
 1052     CONTINUE
 1053     CONTINUE
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=X1(I)
 1051   CONTINUE
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
!
!       STEP 2: GENERATE TRACES FOR EACH GROUP
!
        J=0
        DO 1090 ISET=1,NUMSET
!
          K=0
          DO 1091 I=1,N
            IF(X1(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP2(K)=Y(I)
            ENDIF
1091      CONTINUE
          NI=K
          CALL DPTRE3(TEMP2,NI,XTEMP1,TEND,MAXNXT,   &
                      RATPVA,MHTPVA,DSUM1,DVAL2,DVAL3,   &
                      ISET,ICAPSW,ICAPTY,IFORSW,   &
                      ISUBRO,IBUGA3,IERROR)
          TEMP6(ISET)=RATPVA
          TEMP6(MAXSYS+ISET)=MHTPVA
          TEMP6(2*MAXSYS+ISET)=REAL(DSUM1)
          TEMP6(3*MAXSYS+ISET)=REAL(DVAL2)
          TEMP6(4*MAXSYS+ISET)=REAL(DVAL3)
1090    CONTINUE
!
!       CASE 3: BOTH GROUP VARIABLE AND CENSORING VARIABLE
!
      ELSE
!
!       STEP 1: DETERMINE UNIQUE GROUPS
!
        NUMSET=0
        DO 1111 I=1,N
          IF(NUMSET.EQ.0)GO TO 1113
          DO 1112 J=1,NUMSET
            IF(X1(I).EQ.XIDTEM(J))GO TO 1111
 1112     CONTINUE
 1113     CONTINUE
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=X1(I)
 1111   CONTINUE
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
!
!       STEP 2A: EXTRACT RESPONSE AND CENSORING DATA FOR EACH
!                GROUP
!
        J=0
        ISETMX=NUMSET
        DO 1120 ISET=1,NUMSET
!
          K=0
          DO 1121 I=1,N
            IF(X1(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP2(K)=Y(I)
              TEMP3(K)=XCEN(I)
            ENDIF
1121      CONTINUE
          NI=K
!
!       STEP 2B: PROCESS THE CENSORING VARIABLE.  THERE CAN
!                BE AT MOST ONE CENSORING POINT FOR EACH
!                GROUP.
!
          CALL SORTC(TEMP2,TEMP3,NI,TEMP4,TEMP5)
          DO 1160 I=1,NI
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
              DO 1170 I=1,NI
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
                  GO TO 1120
                ENDIF
 1170         CONTINUE
            ELSE
              TEND=TEMP2(NI)
              NTEMPR=NI-1
              NTEMPC=1
              DO 1180 I=1,NTEMPR
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
                  GO TO 1120
                ENDIF
 1180         CONTINUE
            ENDIF
          ENDIF
 1171 FORMAT('      FOR EACH SYSTEM, THERE SHOULD BE AT MOST')
 1172 FORMAT('      ONE CENSORING TIME AND IT MUST BE THE MAXIMUM')
 1173 FORMAT('      VALUE FOR THAT SYSTEM.')
 1174 FORMAT('      SUCH WAS NOT THE CASE FOR SYSTEM ',G15.7)
!
!       STEP 2C: COMPUTE THE TREND TEST FOR A SINGLE SYSTEM
!
          TEND=ACEN
          CALL DPTRE3(TEMP2,NTEMPR,XTEMP1,TEND,MAXNXT,   &
                      RATPVA,MHTPVA,DSUM1,DVAL2,DVAL3,   &
                      ISET,ICAPSW,ICAPTY,IFORSW,   &
                      ISUBRO,IBUGA3,IERROR)
          TEMP6(ISET)=RATPVA
          TEMP6(MAXSYS+ISET)=MHTPVA
          TEMP6(2*MAXSYS+ISET)=REAL(DSUM1)
          TEMP6(3*MAXSYS+ISET)=REAL(DVAL2)
          TEMP6(4*MAXSYS+ISET)=REAL(DVAL3)
!
1120    CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 21--                             **
!               **  PERFORM COMPOSITE TESTS               **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMSET.LE.1)GO TO 9000
!
!     COMPOSITE TESTS
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
      IDF=2
      ISUM=0
      SUM1=0.0
      SUM2=0.0
      DO 2010 I=1,NUMSET
        PVAL=TEMP6(I)
        ATERM1=-2.0*LOG(PVAL)
        SUM1=SUM1 + PVAL
        SUM2=SUM2 + ATERM1
        ISUM=ISUM+IDF
 2010 CONTINUE
!
      ALP90=0.90
      CALL CHSPPF(ALP90,ISUM,CV1)
      ALP95=0.95
      CALL CHSPPF(ALP95,ISUM,CV2)
      ALP99=0.99
      CALL CHSPPF(ALP99,ISUM,CV3)
!
      ITITLE='Reverse Arrangements Test: Fisher Composite Test'
      NCTITL=48
      ITITLZ=' '
      NCTITZ=0
!
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
!
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
!
      NUMROW=ICNT
      DO 2020 I=1,NUMROW
        NTOT(I)=15
 2020 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      DO 2030 J=1,5
        DO 2040 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2040   CONTINUE
 2030 CONTINUE
!
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
!
      ITITL2(2,2)='Significance'
      NCTIT2(2,2)=12
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
!
      ITITL2(2,3)='Chi-Square'
      NCTIT2(2,3)=10
      ITITL2(3,3)='Test Statistic'
      NCTIT2(3,3)=14
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
!
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
!
      NMAX=0
      NUMCOL=5
      DO 2050 I=1,NUMCOL
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
!
        DO 2060 J=1,3
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
!
 2050 CONTINUE
!
      ICNT=3
      NUMLIN=3
      NUMCOL=5
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
!     COMPOSITE TEST FOR MILITARY HANDBOOK TEST
!
        IDF=2
        ISUM=0
        SUM1=0.0
        SUM2=0.0
        DO 3010 I=1,NUMSET
          PVAL=TEMP6(MAXSYS+I)
          ATERM1=-2.0*LOG(PVAL)
          SUM1=SUM1 + PVAL
          SUM2=SUM2 + ATERM1
          ISUM=ISUM+IDF
 3010   CONTINUE
!
      ALP90=0.90
      CALL CHSPPF(ALP90,ISUM,CV1)
      ALP95=0.95
      CALL CHSPPF(ALP95,ISUM,CV2)
      ALP99=0.99
      CALL CHSPPF(ALP99,ISUM,CV3)
!
      ITITLE='Military Handbook Test: Fisher Composite Test'
      NCTITL=45
      ITITLZ=' '
      NCTITZ=0
!
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
!
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
!
      NUMROW=ICNT
      DO 3020 I=1,NUMROW
        NTOT(I)=15
 3020 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                  NCTEXT,AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      NMAX=0
      NUMCOL=5
      DO 3050 I=1,NUMCOL
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
!
        DO 3060 J=1,3
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
!
 3050 CONTINUE
!
      ICNT=3
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
!     LAPLACE COMPOSITE TEST
!
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM3=0.0D0
      DO 4010 I=1,NUMSET
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
!
      ITITLE='Laplace Test: Composite Test'
      NCTITL=28
      ITITLZ=' '
      NCTITZ=0
!
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
!
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
!
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
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(2,3)='Normal'
      NCTIT2(2,3)=6
      ITITL2(3,3)='Test Statistic'
      NCTIT2(3,3)=14
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
!
      NMAX=0
      DO 4050 I=1,NUMCOL
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
!
        DO 4060 J=1,3
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
!
 4050 CONTINUE
!
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (<=)'
      NCTIT2(3,4)=11
!
      DO 4150 I=1,NUMCOL
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
!
        DO 4160 J=1,3
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
!
 4150 CONTINUE
!
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9016 I=1,N
          WRITE(ICOUT,9017)I,Y(I)
 9017     FORMAT('I,Y(I),W(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTRE2
      SUBROUTINE DPTRE3(Y,N,XTEMP1,TEND,MAXNXT,   &
                        RATPVA,MHTPVA,DSUM1,DVAL2,DVAL3,   &
                        ISET,ICAPSW,ICAPTY,IFORSW,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT A TRENDS ANALYSIS
!              FOR THE DATA IN THE INPUT VECTOR Y.
!     NOTE--DPTRE2 CAN LOOP THROUGH MULTIPLE SYSTEMS.
!           THIS ROUTINE IS USED TO COMPUTE THE TESTS FOR
!           A SINGLE SYSTEM.
!     NOTE--3 TRENDS TESTS ARE PERFORMED:
!           1) REVERSE ARRANGEMENT TEST
!           2) MILITARY HANDBOOK TEST
!           3) LAPLACE TEST
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                OF FAILURE TIMES
!                       N      = THE INTEGER NUMBER OF
!                                OBSERVATIONS IN THE VECTOR Y.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI 77 FORTRAN.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/10
!     ORIGINAL VERSION--OCTOBER   2006. EXTRACTED FROM DPTRE3
!     UPDATED         --FEBRUARY  2011. USE DPDTA1 AND DPDTA5 TO
!                                       PRINT TABLES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DVAL2
      DOUBLE PRECISION DVAL3
!
      REAL MHTPVA
!
      DIMENSION Y(*)
      DIMENSION XTEMP1(*)
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTR'
      ISUBN2='E3  '
      IERROR='NO'
!
      IRMN01=0
      IRMN05=0
      IRMN10=0
      IRMN90=0
      IRMN95=0
      IRMN99=0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRE3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTRE3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N,MAXNXT,IBUGA3
   52   FORMAT('N,MAXNXT,IBUGA3 = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 135 I=2,N
        IF(Y(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)ISET
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
  139 CONTINUE
!
!               ********************************************
!               **  STEP 11--                             **
!               **  REVERSE ARRANGEMENTS TEST             **
!               ********************************************
!
!               ********************************************
!               **  STEP 11A-                             **
!               **  CREATE INTERARRIVAL TIME ARRAY        **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='NO'
      CALL INTARR(Y,N,IWRITE,XTEMP1,NX,IBUGA3,IERROR)
!
!               ********************************************
!               **  STEP 11B-                             **
!               **  CALCULATE NUMBER OF REVERSALS         **
!               ********************************************
      IREV=0
      DO 140 J=1,N-1
        DO 149 K=J+1,N
          IF(XTEMP1(K).GT.XTEMP1(J))IREV=IREV+1
  149   CONTINUE
  140 CONTINUE
      IRMAX=N*(N-1)/2
      AN=REAL(N)
      REXP=AN*(AN-1.0)/4.0
      RVAR=(2.0*AN + 5.0)*(AN - 1.0)*AN/72.0
      RSD=SQRT(RVAR)
!
      R=REAL(IREV)
      ANUM=R + 0.5 - REXP
      Z=ANUM/RSD
      CALL NORCDF(Z,CDF)
      RATPVA=CDF
!
!               *************************
!               **  STEP 11C-          **
!               **  FORM Z STATISTICS  **
!               *************************
!
      ISTEPN='11C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        IRMN01=INT(PPF01*RSD + REXP - 0.5)
        IRMN05=INT(PPF05*RSD + REXP - 0.5)
        IRMN10=INT(PPF10*RSD + REXP - 0.5)
        IRMN90=INT(PPF90*RSD + REXP - 0.5)
        IRMN95=INT(PPF95*RSD + REXP - 0.5)
        IRMN99=INT(PPF99*RSD + REXP - 0.5)
      ENDIF
!
!               ****************************
!               **  STEP 11D-             **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='11D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      ITITLE='Reverse Arrangements Test: (System      )'
      NCTITL=41
      WRITE(ITITLE(36:40),'(I5)')ISET
      ITITLZ=' '
      NCTITZ=0
!
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
!
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
!
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
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      DO 2320 J=1,5
        DO 2325 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2325   CONTINUE
 2320 CONTINUE
!
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
!
      ITITL2(2,2)='Significance'
      NCTIT2(2,2)=12
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
!
      ITITL2(2,3)='Number of'
      NCTIT2(2,3)=9
      ITITL2(3,3)='Reversals'
      NCTIT2(3,3)=9
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
!
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
!
      NMAX=0
      NUMCOL=5
      DO 5210 I=1,NUMCOL
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
!
        DO 5289 J=1,3
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
!
 5210 CONTINUE
!
      ICNT=3
      NUMLIN=3
      NUMCOL=5
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
!
      NUMROW=ICNT
      DO 6210 I=1,NUMROW
        NTOT(I)=15
 6210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                  NCTEXT,AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (<=)'
      NCTIT2(3,4)=11
!
      DO 6310 I=1,NUMCOL
!
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
!
        DO 6389 J=1,3
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
!
 6310 CONTINUE
!
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               ********************************************
!               **  STEP 21--                             **
!               **  MILITARY HANDBOOK    TEST             **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ********************************************
!               **  STEP 21B-                             **
!               **  CALCULATE TEST STATISTIC              **
!               ********************************************
!
      DSUM=0.0D0
      DO 310 I=1,N
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
          GO TO 9000
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
          GO TO 9000
        ENDIF
        DSUM=DSUM + DLOG(DBLE(TEND/Y(I)))
  310 CONTINUE
      Z=REAL(2.0D0*DSUM)
      INU=2*N
      CALL CHSCDF(Z,INU,CDF)
      MHTPVA=CDF
!
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
!
!               ****************************
!               **  STEP 21B-             **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Military Handbook Test: (System      )'
      NCTITL=38
      WRITE(ITITLE(33:37),'(I5)')ISET
      ITITLZ=' '
      NCTITZ=0
!
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
!
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
!
      NUMROW=ICNT
      DO 7310 I=1,NUMROW
        NTOT(I)=15
 7310 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                  NCTEXT,AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(2,3)='Chi-Square'
      NCTIT2(2,3)=10
      ITITL2(3,3)='Test Statistic'
      NCTIT2(3,3)=14
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
!
      DO 5310 I=1,NUMCOL
!
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
!
        DO 5389 J=1,3
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
!
 5310 CONTINUE
!
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
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
!
      NUMROW=ICNT
      DO 7390 I=1,NUMROW
        NTOT(I)=15
 7390 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                  NCTEXT,AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (<=)'
      NCTIT2(3,4)=11
!
      DO 7410 I=1,NUMCOL
!
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
!
        DO 7489 J=1,3
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
!
 7410 CONTINUE
!
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               ********************************************
!               **  STEP 31--                             **
!               **  LAPLACE              TEST             **
!               ********************************************
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ********************************************
!               **  STEP 31B-                             **
!               **  CALCULATE TEST STATISTIC              **
!               ********************************************
!
       DSUM=0.0D0
       DSUM1=0.0D0
       DO 510 I=1,N
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
           GO TO 9000
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
           GO TO 9000
         ENDIF
         DSUM=DSUM + DBLE(Y(I)-TEND/2.0)
         DSUM1=DSUM1 + DBLE(Y(I))
  510 CONTINUE
      DVAL2=DBLE(N)*DBLE(TEND)
      DVAL3=DBLE(N)*DBLE(TEND)**2
!
      AN=REAL(N)
      Z=REAL(DBLE(SQRT(12.0*AN))*DSUM/DBLE(AN*TEND))
      CALL NORCDF(Z,CDF)
!
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
!
!               ****************************
!               **  STEP 31B-             **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='31B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TRE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Laplace Test: (System      )'
      NCTITL=28
      WRITE(ITITLE(23:27),'(I5)')ISET
      ITITLZ=' '
      NCTITZ=0
!
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
!
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
!
      NUMROW=ICNT
      DO 8210 I=1,NUMROW
        NTOT(I)=15
 8210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                  NCTEXT,AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(2,3)='Normal'
      NCTIT2(2,3)=6
      ITITL2(3,3)='Test Statistic'
      NCTIT2(3,3)=14
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
!
      DO 8310 I=1,NUMCOL
!
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
!
        DO 8389 J=1,3
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
!
 8310 CONTINUE
!
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
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
!
      NUMROW=ICNT
      DO 8390 I=1,NUMROW
        NTOT(I)=15
 8390 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                  NCTEXT,AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE(1:25)=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (<=)'
      NCTIT2(3,4)=11
!
      DO 8410 I=1,NUMCOL
!
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
!
        DO 8489 J=1,3
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
!
 8410 CONTINUE
!
      ICNT=3
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9016 I=1,N
          WRITE(ICOUT,9017)I,Y(I),XTEMP1(I)
 9017     FORMAT('I,Y(I),XTEMP1(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTRE3
      SUBROUTINE DPTRI2(X1,Y1,X2,Y2,X3,Y3,IFIG,   &
                        ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!     PURPOSE--DRAW A TRIANGLE WITH FRONT FACE VERTICES AT (X1,Y1),
!              (X2,Y2), AND (X3,Y3).
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
!     UPDATED         --JANUARY   1989. MODIFY CALL  TO DPFIRE (ALAN)
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT2
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
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
!
      DIMENSION PX(10)
      DIMENSION PY(10)
!CCCC DIMENSION PX3(10)
!CCCC DIMENSION PY3(10)
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
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRI2')GO TO 90
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
   65 FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',   &
      A4,2X,A4,2X,A4,2E15.7)
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE TRIANGLE           **
!               *********************************
!
      PX(1)=X1
      PY(1)=Y1
!
      PX(2)=X2
      PY(2)=Y2
!
      PX(3)=X3
      PY(3)=Y3
!
      PX(4)=X1
      PY(4)=Y1
!
      NP=4
!
!
!               ***********************
!               **  STEP 2--         **
!               **  FILL THE FIGURE  **
!               **  (IF CALLED FOR)  **
!               ***********************
!
      IF(IREFSW(1).EQ.'OFF')GO TO 2190
      IPATT=IREPTY(1)
      IPATT2='SOLI'
      PTHICK=PREPTH(1)
      PXGAP=PREPSP(1)
      PYGAP=PREPSP(1)
      ICOLF=IREFCO(1)
      ICOLFR=IREFC2(1,1)
      ICOLFG=IREFC2(1,2)
      ICOLFB=IREFC2(1,3)
      ICOLP=IREPCO(1)
      ICOLPR=IREPC2(1,1)
      ICOLPG=IREPC2(1,2)
      ICOLPB=IREPC2(1,3)
      ICOLBR=IREBC2(1,1)
      ICOLBG=IREBC2(1,2)
      ICOLBB=IREBC2(1,3)
      CALL DPFIRE(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,   &
                  ICOLF,ICOLFR,ICOLFG,ICOLFB,   &
                  ICOLP,ICOLPR,ICOLPG,ICOLPB,   &
                  IPATT2)
 2190 CONTINUE
!
!               ***************************
!               **  STEP 3--             **
!               **  DRAW OUT THE FIGURE  **
!               ***************************
!
      IPATT=ILINPA(1)
      PTHICK=PLINTH(1)
      ICOL=ILINCO(1)
      ICOLR=ILINC2(1,1)
      ICOLG=ILINC2(1,2)
      ICOLB=ILINC2(1,3)
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
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRI2')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTRI2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NP
 9013 FORMAT('NP = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NP
      WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016 FORMAT('I,PX(I),PY(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9039)IBUGG4,ISUBG4,IERRG4
 9039 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTRI2
      SUBROUTINE DPTRIA(IHARG,IARGT,ARG,NUMARG,   &
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
!     PURPOSE--DRAW ONE OR MORE TRIANGLES
!              (DEPENDING ON HOW MANY NUMBERS ARE PROVIDED).
!              THE COORDINATES ARE IN STANDARDIZED UNITS
!              OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE THE VERTICES
!           OF THE TRIANGLE.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 3
!          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*3 = 6.
!     NOTE--IF 4 NUMBERS ARE PROVIDED,
!           THEN THE DRAWN TRIANGLE WILL GO
!           FROM THE LAST CURSOR POSITION
!           (ASSUMED TO BE AT VERTEX 1)
!           THROUGH THE (X,Y) POINT
!           (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE FIRST AND SECOND NUMBERS
!           (ASSUMED TO BE AT VERTEX 2)
!           TO THE (X,Y) POINT
!           (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE THIRD AND FOURTH NUMBERS
!           (ASSUMED TO BE AT VERTEX 3)
!           AND CONTINUING BACK THE START POINT TO CLOSE THE TRIANGLE.
!     NOTE--IF 6 NUMBERS ARE PROVIDED,
!           THEN THE DRAWN TRIANGLE WILL GO
!           FROM THE ABSOLUTE (X,Y) POSITION
!           AS RESULTING FORM THE FIRST AND SECOND NUMBERS
!           (ASSUMED TO BE AT VERTEX 1)
!           THROUGH THE (X,Y) POINT
!           (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE THIRD AND FOURTH NUMBERS
!           (ASSUMED TO BE AT VERTEX 2)
!           TO THE (X,Y) POINT
!           (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE FIFTH AND SIXTH NUMBERS
!           (ASSUMED TO BE AT VERTEX 3)
!           AND THEN CONTINUING BACK THE START POINT TO CLOSE THE TRIANGLE.
!     NOTE--AND SO FORTH FOR 10, 14, 18, ... NUMBERS.
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
!     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
!                                       COMMAND
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
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
      CHARACTER*4 IDCOLO
!CCCC ADD FOLLOWING LINE MARCH 1997.
      CHARACTER*4 IDFONT
!CCCC ADD FOLLOWING LINE JULY 1997.
      CHARACTER*4 UNITSW
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
!CCCC ADD FOLLOWING LINE MARCH 1997.
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
!
      DIMENSION IDNVOF(*)
      DIMENSION IDNHOF(*)
      DIMENSION IBACC2(*)
!
!-----COMMON----------------------------------------------------------
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
!CCCC IBUGG4=IBUGD2
!CCCC ISUBG4=ISUBRO
!
      ILOCFN=0
      NUMNUM=0
!
      X1=0.0
      Y1=0.0
      X2=0.0
      Y2=0.0
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRIA')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTRIA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NUMARG
   53 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NUMARG
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
   65 FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',   &
      A4,2X,A4,2X,A4,2E15.7)
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
      DO 81 I=1,NUMDEV
      WRITE(ICOUT,82)IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   82 FORMAT('IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',   &
      A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,83)IDPOWE(I),IDCONT(I),IDCOLO(I)
   83 FORMAT('IDPOWE(I),IDCONT(I),IDCOLO(I) = ',   &
      A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,84)IDNVPP(I),IDNHPP(I),IDUNIT(I)
   84 FORMAT('IDNVPP(I),IDNHPP(I),IDUNIT(I) = ',   &
      I8,I8,I8)
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
!
      IFIG='TRIA'
      NUMPT=3
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
      IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')   &
      GO TO 1111
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ABSO'.AND.   &
      IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')   &
      GO TO 1112
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'RELA'.AND.   &
      IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')   &
      GO TO 1113
      GO TO 1130
!
 1111 CONTINUE
      ITYPEO='ABSO'
      ILOCFN=1
      GO TO 1119
!
 1112 CONTINUE
      ITYPEO='ABSO'
      ILOCFN=2
      GO TO 1119
!
 1113 CONTINUE
      ITYPEO='RELA'
      ILOCFN=2
      GO TO 1119
 1119 CONTINUE
!
      IF(ILOCFN.GT.NUMARG)GO TO 1129
      DO 1120 I=ILOCFN,NUMARG
      IF(IARGT(I).EQ.'NUMB')GO TO 1120
      GO TO 1129
 1120 CONTINUE
      IFOUND='YES'
      GO TO 1149
 1129 CONTINUE
      GO TO 1130
!
 1130 CONTINUE
      IERRG4='YES'
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN DPTRIA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR DRAW ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
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
      GO TO 9000
 1149 CONTINUE
!
!               ****************************
!               **  STEP 3--              **
!               **  DRAW OUT THE LINE(S)  **
!               ****************************
!
      NUMNUM=NUMARG-ILOCFN+1
      IF(NUMNUM.LT.NUMPT2)GO TO 1151
      GO TO 1152
!
 1151 CONTINUE
      J=ILOCFN-1
      X1=PXSTAR
      Y1=PYSTAR
      GO TO 1159
!
 1152 CONTINUE
      J=ILOCFN
      IF(J.GT.NUMARG)GO TO 1190
      X1=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X1,X1,IBUGD2,ISUBRO,IERROR)
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      Y1=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y1,Y1,IBUGD2,ISUBRO,IERROR)
      GO TO 1159
 1159 CONTINUE
!
 1160 CONTINUE
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      X2=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X2,X2,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')X2=X1+X2
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      Y2=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y2,Y2,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
!
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      X3=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X3,X3,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')X3=X2+X3
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      Y3=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y3,Y3,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')Y3=Y2+Y3
!
      CALL DPTRI2(X1,Y1,X2,Y2,X3,Y3,IFIG,   &
                  ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                  AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                  IREFSW,IREFCO,IREFC2,   &
                  IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                  PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
      X1=X3
      Y1=Y3
!
      GO TO 1160
 1190 CONTINUE
!
      PXEND=X3
      PYEND=Y3
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRIA')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPTRIA
      SUBROUTINE DPTRIP(IHARG,NUMARG,IDEFPR,IHMXPR,   &
      IPREC,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE PRECISION SWITCH
!              AS TRIPLE PRECISION.
!              THIS IN TURN SPECIFIES THAT SUBSEQUENT
!              CALCULATIONS WILL ALL BE CARRIED OUT
!              IN TRIPLE PRECISION.
!              THE SPECIFIED PRECISION SWITCH SPECIFICATION
!              WILL BE PLACED IN THE HOLLERITH VARIABLE IPREC.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFPR (A  HOLLERITH VARIABLE)
!                     --IHMXPR (A  HOLLERITH VARIABLE)
!     OUTPUT ARGUMENTS--IPREC  (A HOLLERITH VARIABLE)
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
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFPR
      CHARACTER*4 IHMXPR
      CHARACTER*4 IPREC
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      IFOUND='YES'
!
      IF(NUMARG.LE.0)GO TO 1120
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1130
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1120
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1130
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1120
      GO TO 1130
!
 1120 CONTINUE
      IHOLD=IDEFPR
      GO TO 1160
!
 1130 CONTINUE
      IHOLD='TRIP'
      GO TO 1160
!
 1160 CONTINUE
      IF(IHOLD.EQ.'DOUB'.AND.IHMXPR.EQ.'SING')GO TO 1170
      IF(IHOLD.EQ.'TRIP'.AND.IHMXPR.EQ.'SING')GO TO 1170
      IF(IHOLD.EQ.'TRIP'.AND.IHMXPR.EQ.'DOUB')GO TO 1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'SING')GO TO 1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'DOUB')GO TO 1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'TRIP')GO TO 1170
      GO TO 1180
!
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
      GO TO 1199
!
 1180 CONTINUE
      IPREC=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IPREC
 1188 FORMAT('THE PRECISION SWITCH HAS JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPTRIP
      SUBROUTINE DPTRPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A TRILINEAR PLOT.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/12
!     ORIGINAL VERSION--DECEMBER  2006.
!     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR3
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
      CHARACTER*4 IREPL
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION GROUP(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),Y3(1))
      EQUIVALENCE (GARBAG(IGARB4),GROUP(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP4(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTR'
      ISUBN2='PL  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ***************************
!               **  TREAT THE PLOT CASE  **
!               ***************************
!
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
!
!               *******************************************
!               **  STEP 1--                             **
!               **  SEARCH FOR TRILINEAR PLOT            **
!               *******************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TRPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
!               *********************************
!               **  STEP 4--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TRPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I),PVAR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),PVAR(I) = ',I8,2X,A4,A4,2X,3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,Y2,Y3,GROUP,GROUP,GROUP,GROUP,NS,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
!
!               *****************************************************
!               **  STEP 41--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY) FOR    **
!               **  THE PLOT.                                      **
!               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .    **
!               **  DEFINE THE NUMBER OF PLOT POINTS   (NPLOTP).   **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES(NPLOTV).   **
!               *****************************************************
!
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TRPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPTRP2(Y1,Y2,Y3,GROUP,NS,   &
                  ICASPL,IREPL,MAXN,TEMP1,   &
                  Y,X,X3D,D,NPLOTP,NPLOTV,   &
                  IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
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
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IBUGG2,IBUGG3
 9014   FORMAT('IBUGG2,IBUGG3 = ', A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,Y(.),X(.),D(.),ISUB(.)--')
        CALL DPWRST('XXX','BUG ')
        DO 9021 I=1,NPLOTP
          WRITE(ICOUT,9022)I,Y(I),X(I),D(I),ISUB(I)
 9022     FORMAT(I8,E15.7,E15.7,E15.7,I8)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTRPL
      SUBROUTINE DPTRP2(Y1,Y2,Y3,GROUP,NS,   &
                  ICASPL,IREPL,MAXN,TEMP1,   &
                  Y,X,X3D,D,NPLOTP,NPLOTV,   &
                  IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--FORM A TRILINEAR PLOT.
!     REFERENCE--WAINER (1997), "VISUAL REVELATIONS",
!                COPERNICUS, PP. 111-118.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/12
!     ORIGINAL VERSION--DECEMBER  2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IREPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION GROUP(*)
      DIMENSION TEMP1(*)
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION X3D(*)
      DIMENSION D(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTR'
      ISUBN2='PL  '
      IERROR='NO'
!
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
        DO 55 I=1,MIN(NS,100)
          WRITE(ICOUT,56)I,Y1(I),Y2(I),Y3(I),GROUP(I)
   56     FORMAT('I,Y1(I),Y2(I),Y3(I),GROUP(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TRP2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               **  1) THE SUM OF Y1, Y2, AND Y3 MUST BE  **
!               **     EITHER 1 OR 100 (FOR PERCENTAGE    **
!               **     UNITS).                            **
!               **  2) EACH OF THE COMPONENTS MUST BE IN  **
!               **     THE INTERVAL (0,1) OR (0,100).     **
!               ********************************************
!
      N=NS
      ACASE=1.0
!
      DO 120 I=1,N
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
  123       FORMAT('      FOR ROW ',I8,', THE COMPONENTS DO NOT ',   &
                   'SUM TO EITHER 1 OR 100')
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
            GO TO 9000
          ENDIF
        ELSE
          IF(ABS(ASUM - ACASE).GT.EPS)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,121)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,133)I,ACASE
  133       FORMAT('      FOR ROW ',I8,', THE COMPONENTS DO NOT ',   &
                   'SUM TO ',F7.1)
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
            GO TO 9000
          ENDIF
        ENDIF
  120 CONTINUE
!
!               ****************************************************
!               **  STEP 2--                                      **
!               **  COMPUTE COORDINATES FOR TRILINEAR PLOT        **
!               ****************************************************
!
      IF(IREPL.EQ.'ON')THEN
        CALL DISTIN(GROUP,N,IWRITE,TEMP1,NDIST,IBUGG3,IERROR)
        DO 1010 I=1,N
          Y(I)=Y1(I)
          X(I)=Y2(I)
          X3D(I)=Y3(I)
          D(I)=1.0
          DO 1020 J=1,NDIST
            IF(GROUP(I).EQ.TEMP1(J))THEN
              D(I)=REAL(J)
              GO TO 1029
            ENDIF
 1020     CONTINUE
 1029     CONTINUE
 1010   CONTINUE
        NPLOTP=N
        NPLOTV=3
      ELSE
        DO 2010 I=1,N
          Y(I)=Y1(I)
          X(I)=Y2(I)
          X3D(I)=Y3(I)
          D(I)=1.0
 2010   CONTINUE
        NPLOTP=N
        NPLOTV=3
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL = ',   &
               I8,I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,Y(.),X(.),X3D(.),D(.)--')
        CALL DPWRST('XXX','BUG ')
        DO 9021 I=1,NPLOTP
          WRITE(ICOUT,9022)I,Y(I),X(I),X3D(I),D(I)
 9022     FORMAT(I8,4F15.7)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTRP2
      SUBROUTINE DPTRPO(X,Y,N,   &
                        TX,TY,SX,SY,THETA,   &
                        X2,Y2,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--GIVEN A SET OF (X,Y) PAIRS, PERFORM A TRANSLATION,
!              SCALING, AND ROTATION TRANSFORMATION.
!
!              THE TRANSLATION CAN BE IMPLEMENTED AS:
!
!                  X'=X - Tx
!                  Y'=Y - Ty
!
!              THE SCALING CAN BE IMPLENENTED AS:
!
!                 X'=X*Sx
!                 Y'=Y*Sy
!
!              THE ROTATION CAN BE IMPLEMENTED AS:
!
!                 X'=COS(THETA)*X + SIN(THETA)*Y
!                 Y'=-SIN(THETA)*X + COS(THETA)*Y
!
!     INPUT  ARGUMENTS--X      = A REAL VECTOR CONTAINING THE X
!                                COORDINATES OF THE POINTS
!                     --Y      = A REAL VECTOR CONTAINING THE Y
!                                COORDINATES OF THE POINTS
!                     --N      = NUMBER OF POINTS IN X, Y
!                     --TX     = TRANSLATION IN X DIRECTION
!                     --TY     = TRANSLATION IN Y DIRECTION
!                     --SX     = SCALING IN X DIRECTION
!                     --SY     = SCALING IN Y DIRECTION
!                     --THETA  = ANGLE OF ROTATION (IN COUNTER CLOCKWISE
!                                DIRECTION) IN RADIANS
!     OUTPUT ARGUMENTS--X2     = A REAL VECTOR CONTAINING THE X
!                                COORDINATES OF THE TRANSFORMED POINTS
!                     --Y      = A REAL VECTOR CONTAINING THE Y
!                                COORDINATES OF THE TRANSFORMED POINTS
!     REFERENCE--XXXXX
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
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012.10
!     ORIGINAL VERSION--OCTOBER   2012.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!
      REAL X(*)
      REAL Y(*)
      REAL X2(*)
      REAL Y2(*)
!
      DOUBLE PRECISION PI
      DOUBLE PRECISION DX
      DOUBLE PRECISION DY
      DOUBLE PRECISION DXP
      DOUBLE PRECISION DYP
      DOUBLE PRECISION DTHETA
!
      INTEGER N
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA PI / 3.141592653589793238462643383279503D0 /
!
!-----START POINT-----------------------------------------------------
!
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
          DO 65 I=1,N
            WRITE(ICOUT,66)I,X(I),Y(I)
   66       FORMAT('I,X(I),Y(I) = ',I8,2X,2G15.7)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
      ENDIF
!
      IF(SX.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN  TRANSFORM POINTS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)SX
  103   FORMAT('      THE SCALING FACTOR ',G15.7,' FOR THE X ',   &
               'DIRECTION IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(SY.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,108)SY
  108   FORMAT('      THE SCALING FACTOR ',G15.7,' FOR THE Y ',   &
               'DIRECTION IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DTHETA=DBLE(THETA)
      IF((DTHETA.LT.-PI) .OR. (DTHETA.GT.PI))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)THETA
  113   FORMAT('      THE ROTATION FACTOR ',G15.7,   &
               'IS OUTSIDE THE (-PI,PI) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 100 IROW=1,N
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
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TRPO')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DPTRPO--')
        CALL DPWRST('XXX','BUG ')
        DO 9055 I=1,N
          WRITE(ICOUT,9056)I,X2(I),Y2(I)
 9056     FORMAT('I,X2(I),Y2(I) = ',I8,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9055   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTRPO
      SUBROUTINE DPTTES(XTEMP1,MAXNXT,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A 1-SAMPLE OR A 2-SAMPLE T TEST
!     EXAMPLE--T TEST Y MU
!              T TEST MU Y
!              T TEST Y1 Y2
!              T TEST Y1 Y2 Y3 Y4 MU
!              T TEST Y1 Y2 Y3 Y4 Y5
!              PAIRED T TEST Y1 Y2
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JULY      1984.
!     UPDATED         --FEBRUARY  1994.  ADD COMMENTS ABOVE
!     UPDATED         --DECEMBER  1994.  COPY T TEST PARAMETERS
!     UPDATED         --MAY       1995.  BUG FIX (DECLARATIONS)
!     UPDATED         --MARCH     2011.  USE DPPARS AND DPPAR3
!     UPDATED         --MARCH     2011.  SUPPORT FOR PAIRED T-TEST
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
      CHARACTER*4 ICASA3
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 IPAIR
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
      DIMENSION XTEMP1(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTT'
      ISUBN2='ES  '
      IFOUND='NO'
      IERROR='NO'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ********************************
!               **  TREAT THE T TEST CASE  **
!               ********************************
!
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
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTC=9999
      ILASTZ=9999
      ICASAN='TTES'
      ICASA2='UNKN'
      ICASA3='UNKN'
      IPAIR='OFF'
      IREPL='OFF'
      IMULT='OFF'
!
!     LOOK FOR:
!
!          T TEST/TTEST
!          MULTIPLE
!          REPLICATED
!          PAIRED
!          ONE SAMPLE (OR 1 SAMPLE)
!          TWO SAMPLE (OR 2 SAMPLE)
!
      DO 100 I=0,NUMARG-1
!
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
        ELSE
          ICTMP1=IHARG(I)
        ENDIF
        ICTMP2=IHARG(I+1)
        ICTMP3=IHARG(I+2)
!
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
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
!
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')THEN
        WRITE(ICOUT,91)ICASAN,ICASA2,IMULT,IREPL,ISHIFT
   91   FORMAT('DPTTES: ICASAN,ICASA2,IMULT,IREPL,ISHIFT = ',   &
               4(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IFOUND.EQ.'NO')GO TO 9000
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
          GO TO 9000
        ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),IVARTY(I),   &
                            ILIS(I),NRIGHT(I),ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),IVARTY(I),ILIS(I),',   &
                   'NRIGHT(I),ICOLR(I) = ',I8,2X,2A4,2X,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!     IF EITHER FIRST OR LAST ARGUMENT IS A PARAMETER, THEN
!     WE HAVE THE ONE-SAMPLE T-TEST.  OTHERWISE, HAVE ASSUME
!     A TWO-SAMPLE T-TEST.
!
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
          GO TO 9000
        ENDIF
        ISTART=1
        ISTOP=NUMVAR-1
        AMU0=PVAR(NUMVAR)
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTES')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVA2=1
      DO 5210 I=ISTART,ISTOP
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
        IF(ICASA2.EQ.'ONES')THEN
          ISTRT2=1
          ISTOP2=1
        ELSE
          ISTRT2=I+1
          ISTOP2=ISTOP
        ENDIF
!
        DO 5220 J=ISTRT2,ISTOP2
!
          IF(ICASA2.EQ.'TWOS')THEN
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
          ENDIF
!
!               *****************************************
!               **  STEP 52--                          **
!               **  PERFORM 2-SAMPLE T-TEST            **
!               *****************************************
!
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
            IF(ICASA2.EQ.'ONES')NS2=NS1
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
          CALL DPTTE2(Y,NS1,X,NS2,AMU0,ICASA2,ICASA3,IPAIR,   &
                      XTEMP1,MAXNXT,   &
                      ICAPSW,ICAPTY,IFORSW,ITTEVA,   &
                      IVARID,IVARI2,IVARI3,IVARI4,   &
                      STATVA,STATCD,STATNU,POOLSD,   &
                      STATV2,STATC2,STATN2,   &
                      PVAL2T,PVALLT,PVALUT,   &
                      CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,   &
                      CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,   &
                      IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          CALL DPTTE5(ICASA2,STATVA,STATCD,STATNU,   &
                      STATV2,STATC2,STATN2,   &
                      PVAL2T,PVALLT,PVALUT,   &
                      CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,   &
                      CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,   &
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
!
      RETURN
      END SUBROUTINE DPTTES
      SUBROUTINE DPTTE2(Y1,N1,Y2,N2,AMU0,ICASA2,ICASA3,IPAIR,   &
                        XTEMP1,MAXNXT,   &
                        ICAPSW,ICAPTY,IFORSW,ITTEVA,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        STATVA,STATCD,STATNU,POOLSD,   &
                        STATV2,STATC2,STATN2,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,   &
                        CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT A T TEST
!              (1-SAMPLE OR UNPAIRED 2-SAMPLE)
!     EXAMPLE--T TEST Y MU
!              T TEST MU Y
!              T TEST Y1 Y2
!     SAMPLE 1 IS IN INPUT VECTOR Y1
!              (WITH N1 OBSERVATIONS).
!     SAMPLE 2 IS IN INPUT VECTOR Y2
!              (WITH N2 OBSERVATIONS).
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
!     ORIGINAL VERSION--MAY       1984.
!     UPDATED         --APRIL     1987.  (LARRY KNAB CORRECTION--
!                                        BROWNLEE, P. 225)
!     UPDATED         --FEBRUARY  1994.  REFORMAT OUTPUT
!     UPDATED         --FEBRUARY  1994.  DPWRST: 'BUG ' => 'WRIT'
!     UPDATED         --DECEMBER  1994.  COPY T TEST PARAMETERS
!     UPDATED         --OCTOBER   2006.  CALL LIST TO TCDF/TPPF
!     UPDATED         --NOVEMBER  2007.  ALLOW USER-SPECIFIED
!                                        SIGNIFICANCE LEVEL
!     UPDATED         --APRIL     2011.  USE DPDTA1, DPDTA5 TO PRINT
!                                        OUTPUT.  REFORMAT OUTPUT
!                                        SOMEWHAT AS WELL.
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
      CHARACTER*4 ITTEVA
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
      CHARACTER*4 IPAIR
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION XTEMP1(*)
!
      PARAMETER (NUMALP=6)
      REAL ALPHA(NUMALP)
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA/0.50, 0.80, 0.90, 0.95, 0.99, 0.999/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTT'
      ISUBN2='E2  '
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
        WRITE(ICOUT,55)N1,N2,NUMDIG,MAXNXT,AMU
   55   FORMAT('N1,N2,NUMDIG,MAXNXT,AMU = ',4I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        IF(N1.GE.1)THEN
          DO 56 I=1,N1
            WRITE(ICOUT,57)I,Y1(I)
   57       FORMAT('I,Y1(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
        IF(N2.GE.1 .AND. ICASA2.EQ.'TWOS')THEN
          DO 66 I=1,N2
            WRITE(ICOUT,67)I,Y2(I)
   67       FORMAT('I,Y2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   66     CONTINUE
        ENDIF
      ENDIF
!
!               ************************************
!               **   STEP 1--                     **
!               **   BRANCH DEPENDING ON WHETHER  **
!               **   1-SAMPLE T TEST OR           **
!               **   2-SAMPLE T TEST.             **
!               ************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASA2.EQ.'ONES')THEN
        GO TO 2100
      ELSEIF(ICASA2.EQ.'TWOS')THEN
        IF(IPAIR.EQ.'OFF')GO TO 3100
        IF(IPAIR.EQ.'ON')GO TO 4100
      ELSE
        GO TO 9000
      ENDIF
!
!               ******************************
!               **  STEP 21--               **
!               **  CARRY OUT CALCULATIONS  **
!               **  FOR A 1-SAMPLE T TEST   **
!               ******************************
!
 2100 CONTINUE
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPTTE3(Y1,N1,AMU0,IWRITE,STATVA,STATCD,STATNU,   &
                  YMEAN,YSD,YSDM,DEL,   &
                  PVAL2T,PVALLT,PVALUT,   &
                  ISUBRO,IBUGA3,IERROR)
!
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
!
!               ******************************
!               **   STEP 22--              **
!               **   WRITE OUT EVERYTHING   **
!               **   FOR A 1-SAMPLE T TEST  **
!               ******************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
      ITITLE='One Sample t-Test for the Mean'
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
      WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=27
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
      ITEXT(ICNT)='H0: Mean Equal'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=AMU0
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Mean Not Equal'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=AMU0
      IDIGIT(ICNT)=NUMDIG
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
!
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Two-Tailed Test'
      NCTITL=15
      ITITL9='H0: u = m0; Ha: u <> m0'
      NCTIT9=23
!
      DO 2130 J=1,4
        DO 2140 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2140   CONTINUE
 2130 CONTINUE
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
      NUMCOL=4
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
      DO 2160 J=1,NUMALP
!
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
!
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 2160 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      NUMCOL=4
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      IF(ICASA3.NE.'LOWE' .AND. ICASA3.NE.'UPPE')THEN
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
      IF(ICASA3.EQ.'TWOT')GO TO 9000
!
      ITITLE='Lower One-Tailed Test'
      NCTITL=21
      ITITL9='H0: u = m0; Ha: u < m0'
      NCTIT9=22
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
!
      NMAX=0
      NUMCOL=4
      DO 2250 I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 2250 CONTINUE
!
      DO 2260 J=1,NUMALP
        ALPHAT=1.0 - ALPHA(J)
        CALL TPPF(ALPHAT,STATNU,ATEMP)
        AMAT(J,3)=ATEMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.GE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 2260 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      IF(ICASA3.NE.'UPPE')THEN
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
      IF(ICASA3.EQ.'LOWE')GO TO 9000
!
      ITITLE='Upper One-Tailed Test'
      NCTITL=21
      ITITL9='H0: u = m0; Ha: u > m0'
      NCTIT9=22
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (>)'
      NCTIT2(3,3)=9
!
      NMAX=0
      NUMCOL=4
      DO 2350 I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 2350 CONTINUE
!
      DO 2360 J=1,NUMALP
        ALPHAT=ALPHA(J)
        CALL TPPF(ALPHAT,STATNU,ATEMP)
        AMAT(J,3)=ATEMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.LE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 2360 CONTINUE
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
      GO TO 9000
!
!               ****************************************
!               **  STEP 31--                         **
!               **  CARRY OUT CALCULATIONS            **
!               **  FOR AN UNPAIRED 2-SAMPLE T TEST   **
!               ****************************************
!
 3100 CONTINUE
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPTTE4(Y1,N1,Y2,N2,IWRITE,ITTEVA,   &
                  STATVA,STATCD,STATNU,   &
                  STATV2,STATC2,STATN2,   &
                  Y1MEAN,Y1SD,Y1SDM,   &
                  Y2MEAN,Y2SD,Y2SDM,   &
                  DEL,POOLSD,DELSD,DELSD2,CDFBAR,   &
                  PVAL2T,PVALLT,PVALUT,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************
!               **   STEP 32--              **
!               **   WRITE OUT EVERYTHING   **
!               **   FOR A 2-SAMPLE T TEST  **
!               ******************************
!
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
      ITITLE='Two Sample t-Test for Equal Means'
      NCTITL=34
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
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
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
!
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
!
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
!
      NUMROW=ICNT
      DO 3110 I=1,NUMROW
        NTOT(I)=15
 3110 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='31A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='31B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 3199 ICASE=1,2
!
        IF(ICASE.EQ.1 .AND. ITTEVA.EQ.'UNEQ')GO TO 3199
        IF(ICASE.EQ.2 .AND. ITTEVA.EQ.'EQUA')GO TO 3199
!
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
!
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
!
        ITITL9='H0: u1 = u2; Ha: u1 <> u2'
        NCTIT9=25
!
        DO 3130 J=1,4
          DO 3140 I=1,3
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
 3140     CONTINUE
 3130   CONTINUE
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
        NUMCOL=4
        DO 3150 I=1,NUMCOL
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
        DO 3160 J=1,NUMALP
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
!
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 3160   CONTINUE
!
        ICNT=NUMALP
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        IF(ICASA3.NE.'LOWE' .AND. ICASA3.NE.'UPPE')THEN
          CALL DPDTA5(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
        ENDIF
        IF(ICASA3.EQ.'TWOT')GO TO 3199
!
        IF(ICASE.EQ.1)THEN
          ITITLE='Lower One-Tailed Test (Assume Equal Variances)'
          NCTITL=46
        ELSEIF(ICASE.EQ.2)THEN
          ITITLE='Lower One-Tailed Test (Assume Unequal Variances)'
          NCTITL=48
        ENDIF
!
        ITITL9='H0: u1 = u2; Ha: u1 < u2'
        NCTIT9=24
!
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (<)'
        NCTIT2(3,3)=9
!
        NMAX=0
        NUMCOL=4
        DO 3250 I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 3250   CONTINUE
!
        DO 3260 J=1,NUMALP
          ALPHAT=ALPHA(J)
          CALL TPPF(ALPHAT,STATN,ATEMP)
          AMAT(J,3)=-ATEMP
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATV.GE.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
 3260   CONTINUE
!
        ICNT=NUMALP
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        IF(ICASA3.NE.'UPPE')THEN
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
        IF(ICASA3.EQ.'LOWE')GO TO 3199
!
        IF(ICASE.EQ.1)THEN
          ITITLE='Upper One-Tailed Test (Assume Equal Variances)'
          NCTITL=46
        ELSEIF(ICASE.EQ.2)THEN
          ITITLE='Upper One-Tailed Test (Assume Unequal Variances)'
          NCTITL=48
        ENDIF
!
        ITITL9='H0: u1 = u2; Ha: u1 > u2'
        NCTIT9=24
!
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (>)'
        NCTIT2(3,3)=9
!
        NMAX=0
        NUMCOL=4
        DO 3350 I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 3350   CONTINUE
!
        DO 3360 J=1,NUMALP
          ALPHAT=ALPHA(J)
          CALL TPPF(ALPHAT,STATN,ATEMP)
          AMAT(J,3)=ATEMP
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATV.LE.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
 3360   CONTINUE
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
 3199 CONTINUE
!
      GO TO 9000
!
 4100 CONTINUE
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPTTE6(Y1,N1,Y2,N2,XTEMP1,IWRITE,   &
                  STATVA,STATCD,STATNU,   &
                  Y1MEAN,Y1SD,Y1SDM,   &
                  Y2MEAN,Y2SD,Y2SDM,   &
                  YDMEAN,YDSD,YDSDM,   &
                  PVAL2T,PVALLT,PVALUT,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
!
!               ******************************
!               **   STEP 32--              **
!               **   WRITE OUT EVERYTHING   **
!               **   FOR A 2-SAMPLE T TEST  **
!               ******************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
      ITITLE='Two Sample Paired t-Test for Equal Means'
      NCTITL=41
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
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
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
!
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
!
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
      DO 4110 I=1,NUMROW
        NTOT(I)=15
 4110 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='31A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='31B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Two-Tailed Test'
      NCTITL=15
      ITITL9='H0: u1 = u2; Ha: u1 <> u2'
      NCTIT9=25
!
      DO 4130 J=1,4
        DO 4140 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 4140   CONTINUE
 4130 CONTINUE
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
      NUMCOL=4
      DO 4150 I=1,NUMCOL
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
      DO 4160 J=1,NUMALP
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
!
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 4160 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      IF(ICASA3.NE.'LOWE' .AND. ICASA3.NE.'UPPE')THEN
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
      IF(ICASA3.EQ.'TWOT')GO TO 9000
!
      ITITLE='Lower One-Tailed Test'
      NCTITL=21
      ITITL9='H0: u1 = u2; Ha: u1 < u2'
      NCTIT9=24
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
!
      NMAX=0
      NUMCOL=4
      DO 4250 I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 4250 CONTINUE
!
      DO 4260 J=1,NUMALP
        ALPHAT=ALPHA(J)
        CALL TPPF(ALPHAT,STATNU,ATEMP)
        AMAT(J,3)=-ATEMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.GE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 4260 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      IF(ICASA3.NE.'UPPE')THEN
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
      IF(ICASA3.EQ.'LOWE')GO TO 9000
!
      ITITLE='Upper One-Tailed Test'
      NCTITL=21
      ITITL9='H0: u1 = u2; Ha: u1 > u2'
      NCTIT9=24
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (>)'
      NCTIT2(3,3)=9
!
      NMAX=0
      NUMCOL=4
      DO 4350 I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 4350 CONTINUE
!
      DO 4360 J=1,NUMALP
        ALPHAT=ALPHA(J)
        CALL TPPF(ALPHAT,STATNU,ATEMP)
        AMAT(J,3)=ATEMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.LE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 4360 CONTINUE
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
      GO TO 9000
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPTTE2
      SUBROUTINE DPTTE3(X,N,AMU,IWRITE,STATVA,STATCD,STATNU,   &
                        XMEAN,XSD,XSDM,DEL,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE ONE SAMPLE T-TEST (AND
!              ALTERNATIVELY THE CDF VALUE).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --AMU    = THE SINGLE PRECISION VALUE FOR WHICH
!                                THE TEST IS PERFORMED (I.E.,
!                                H0: MU = AMU).
!     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED STATISTIC.
!                     --STATCD = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE TEST STATISTIC.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             TEST STATISTIC.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--TPPF.
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009.2
!     ORIGINAL VERSION--FEBRUARY  2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRTSV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
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
      ISUBN1='DPTT'
      ISUBN2='E3  '
      IWRTSV=IWRITE
!
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *********************************
!               **  COMPUTE ONE SAMPLE T-TEST  **
!               *********************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      STATVA=-99.0
      STATCD=-99.0
      STATNU=-99.0
      PVAL2T=-99.0
      PVALLT=-99.0
      PVALUT=-99.0
      IWRITE='OFF'
!
      AN=N
!
      IF(N.LE.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN ONE SAMPLE T-TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',   &
               'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE MUST BE 2 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
               '.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               *****************************************
!               **  STEP 2--                           **
!               **  COMPUTE THE ONE SAMPLE T-TEST.     **
!               *****************************************
!
      CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
      CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
      CALL SDMEAN(X,N,IWRITE,XSDM,IBUGA3,IERROR)
      DEL=XMEAN-AMU
      STATVA=DEL/XSDM
      IDF=N-1
      STATNU=REAL(IDF)
      CALL TCDF(STATVA,STATNU,STATCD)
!
      PVALLT=STATCD
      PVALUT=1.0 - STATCD
      IF(STATVA.LE.0.0)THEN
        PVAL2T=2.0*PVALLT
      ELSE
        PVAL2T=2.0*PVALUT
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
  811   FORMAT('THE VALUE OF THE ONE SAMPLE T-TEST OF THE ',I8,   &
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
!
      IWRITE=IWRTSV
!
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
!
      RETURN
      END SUBROUTINE DPTTE3
      SUBROUTINE DPTTE4(Y1,N1,Y2,N2,IWRITE,ITTEVA,   &
                        STATVA,STATCD,STATNU,   &
                        STATV2,STATC2,STATN2,   &
                        Y1MEAN,Y1SD,Y1SDM,   &
                        Y2MEAN,Y2SD,Y2SDM,   &
                        DEL,POOLSD,DELSD,DELSD2,CDFBAR,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE UNPAIRED TWO SAMPLE T-TEST
!              (AND ALTERNATIVELY THE CDF OR P-VALUES).
!     INPUT  ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS
!                                FOR THE FIRST RESPONSE VARIABLE.
!                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y1.
!                     --Y2     = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS
!                                FOR THE SECOND RESPONSE VARIABLE.
!                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y2.
!     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED STATISTIC.
!                     --STATCD = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE TEST STATISTIC.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             TEST STATISTIC.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--TPPF.
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011.4
!     ORIGINAL VERSION--APRIL     2011. EXTRACTED FROM DPTTE2 TO
!                                       ALLOWED IT TO BE CALLED FROM
!                                       CMPSTA (I.E., FOR USE AS A
!                                       "STATISTIC")
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ITTEVA
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRTSV
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTT'
      ISUBN2='E4  '
      IWRTSV=IWRITE
      IERROR='NO'
!
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
        DO 55 I=1,N1
          WRITE(ICOUT,56)I,Y1(I)
   56     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO 65 I=1,N1
          WRITE(ICOUT,66)I,Y2(I)
   66     FORMAT('I,Y2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
      ENDIF
!
!               *********************************
!               **  COMPUTE TWO SAMPLE T-TEST  **
!               *********************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
!
      IF(N1.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN T-TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',   &
               'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N1
  113   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
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
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
  139 CONTINUE
!
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,142)
  142   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',   &
               'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y2(1)
      DO 155 I=2,N1
        IF(Y2(I).NE.HOLD)GO TO 159
  155 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,151)HOLD
  151 FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
  159 CONTINUE
!
!               **************************************************
!               **  STEP 2--                                    **
!               **  COMPUTE THE TWO SAMPLE UNPAIRED T-TEST.     **
!               **************************************************
!
      CALL MEAN(Y1,N1,IWRITE,Y1MEAN,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,Y1SD,IBUGA3,IERROR)
      Y1VAR=Y1SD**2
      CALL SDMEAN(Y1,N1,IWRITE,Y1SDM,IBUGA3,IERROR)
!
      CALL MEAN(Y2,N2,IWRITE,Y2MEAN,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,Y2SD,IBUGA3,IERROR)
      Y2VAR=Y2SD**2
      CALL SDMEAN(Y2,N2,IWRITE,Y2SDM,IBUGA3,IERROR)
!
      AN1=N1
      AN2=N2
!
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
!
      DEL2=DEL
      DELVA2=(Y1VAR/AN1)+(Y2VAR/AN2)
      DELSD2=SQRT(DELVA2)
      STATV2=DEL2/DELSD2
      C=(Y1VAR/AN1)/((Y1VAR/AN1)+(Y2VAR/AN2))
      TERM1=C*C/(AN1-1.0)
      TERM2=(1-C)*(1-C)/(AN2-1.0)
      SUM=TERM1+TERM2
      STATN2=1.0/SUM
!
!     2021/12: ROUND FRACTIONAL DEGREES OF FREEDOM TO NEAREST
!              INTEGER VALUE
!
      IVAL=INT(STATN2+0.5)
      IF(IVAL.LE.1)IVAL=2
      STATN2=REAL(IVAL)
      CALL TCDF(STATV2,STATN2,STATC2)
!
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
!
      IF(ITTEVA.EQ.'EQUA')THEN
        PVALLT=STATCD
        PVALUT=1.0 - STATCD
        IF(STATVA.LE.0.0)THEN
          PVAL2T=2.0*PVALLT
        ELSE
          PVAL2T=2.0*PVALUT
        ENDIF
      ELSE
        PVALLT=STATC2
        PVALUT=1.0 - STATC2
        IF(STATV2.LE.0.0)THEN
          PVAL2T=2.0*PVALLT
        ELSE
          PVAL2T=2.0*PVALUT
        ENDIF
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
        WRITE(ICOUT,811)STATVA
  811   FORMAT('THE VALUE OF THE TWO SAMPLE T-TEST = ',G15.7)
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
      IWRITE=IWRTSV
!
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
!
      RETURN
      END SUBROUTINE DPTTE4
      SUBROUTINE DPTTE5(ICASAN,STATVA,STATCD,STATNU,   &
                        STATV2,STATC2,STATN2,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,   &
                        CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,   &
                        IFLAGU,IFRST,ILAST,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--UTILITY ROUTINE USED BY DPTTES TO UPDATE VARIOUS
!              INTERNAL PARAMETERS AFTER A T-TEST.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORAOTRY
!                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/4
!     ORIGINAL VERSION--APRIL     2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
      CHARACTER*4 IFLAGU
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      LOGICAL IFRST
      LOGICAL ILAST
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
!
      CHARACTER*4 IOP
      SAVE IOUNI1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
!
      IF(ICASAN.EQ.'ONES' .OR. ICASAN.EQ.'PDTE')THEN
        STATV=STATVA
        STATC=STATCD
        STATN=STATNU
      ELSE
        STATV=STATV2
        STATC=STATC2
        STATN=STATN2
      ENDIF
!
      IF(IFLAGU.EQ.'FILE')THEN
!
        IF(IFRST)THEN
          IOP='OPEN'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          WRITE(IOUNI1,295)
  295     FORMAT(11X,'STATVAL',8X,'STATCDF',8X,'STATNU',   &
                  9X,'PVAL2T',9X,'PVALLT',X,'PVALUT',   &
                  7X,'CUTLOW50',7X,'CUTLOW80',7X,'CUTLOW90',   &
                  7X,'CUTLOW95',7X,'CUTLOW99',7X,'CUTLO999',   &
                  7X,'CUTUPP50',7X,'CUTUPP80',7X,'CUTUPP90',   &
                  7X,'CUTUPP95',7X,'CUTUPP99',7X,'CUTUP999')
        ENDIF
        WRITE(IOUNI1,299)STATV,STATC,STATN,PVAL2T,PVALLT,PVALUT,   &
                         CUTL50,CUTL80,CUTL90,CUTL95,CUTL99,CTL999,   &
                         CUTU50,CUTU80,CUTU90,CUTU95,CUTU99,CTU999
  299   FORMAT(18E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(STATV.NE.CPUMIN)THEN
          IH='STAT'
          IH2='VAL '
          VALUE0=STATV
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(STATC.NE.CPUMIN)THEN
          IH='STAT'
          IH2='CDF '
          VALUE0=STATC
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(STATN.NE.CPUMIN)THEN
          IH='STAT'
          IH2='NU  '
          VALUE0=STATN
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(PVAL2T.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UE  '
          VALUE0=PVAL2T
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(PVALLT.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UELT'
          VALUE0=PVALLT
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(PVALUT.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UEUT'
          VALUE0=PVALUT
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTU50.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP50'
          VALUE0=CUTU50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTL50.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW50'
          VALUE0=CUTU50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTU80.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP80'
          VALUE0=CUTU80
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTL80.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW80'
          VALUE0=CUTL80
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTU90.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP90'
          VALUE0=CUTU90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTL90.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW90'
          VALUE0=CUTL90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTU95.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP95'
          VALUE0=CUTU95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTL95.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW95'
          VALUE0=CUTL95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTU99.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP99'
          VALUE0=CUTU99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTL99.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW99'
          VALUE0=CUTL99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CTU999.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='P999'
          VALUE0=CTU999
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CTL999.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='O999'
          VALUE0=CTL999
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
      ENDIF
!
      IF(IFLAGU.EQ.'FILE')THEN
        IF(ILAST)THEN
          IOP='CLOS'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGA3,ISUBRO,IERROR)
!
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TTE5')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR
  301       FORMAT('AFTER CALL DPCLFI, IERROR = ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(IERROR.EQ.'YES')GO TO 9000
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TTE5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPTTE5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPTTE5
      SUBROUTINE DPTTE6(Y1,N1,Y2,N2,YTEMP,IWRITE,   &
                        STATVA,STATCD,STATNU,   &
                        Y1MEAN,Y1SD,Y1SDM,   &
                        Y2MEAN,Y2SD,Y2SDM,   &
                        YDMEAN,YDSD,YDSDM,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PAIRED TWO SAMPLE T-TEST
!              (AND ALTERNATIVELY THE CDF OR P-VALUES).
!     INPUT  ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS
!                                FOR THE FIRST RESPONSE VARIABLE.
!                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y1.
!                     --Y2     = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS
!                                FOR THE SECOND RESPONSE VARIABLE.
!                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR Y2.
!     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED STATISTIC.
!                     --STATCD = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE TEST STATISTIC.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             TEST STATISTIC.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--TCDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2888
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011.4
!     ORIGINAL VERSION--APRIL     2011
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRTSV
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
      DIMENSION YTEMP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTT'
      ISUBN2='E6  '
      IWRTSV=IWRITE
      IERROR='NO'
!
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
        DO 55 I=1,MIN(N1,N2)
          WRITE(ICOUT,56)I,Y1(I),Y2(I)
   56     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ****************************************
!               **  COMPUTE TWO SAMPLE PAIRED T-TEST  **
!               ****************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      STATVA=-99.0
      STATCD=-99.0
      STATNU=-99.0
      PVAL2T=-99.0
      PVALLT=-99.0
      PVALUT=-99.0
      IWRITE='OFF'
!
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
        GO TO 9000
      ENDIF
!
      IF(N1.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN PAIRED T-TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',   &
               'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N1
  113   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
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
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
  139 CONTINUE
!
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,142)
  142   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',   &
               'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y2(1)
      DO 155 I=2,N1
        IF(Y2(I).NE.HOLD)GO TO 159
  155 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,151)HOLD
  151 FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
  159 CONTINUE
!
!               **************************************************
!               **  STEP 2--                                    **
!               **  COMPUTE THE TWO SAMPLE PAIRED T-TEST.       **
!               **************************************************
!
      DO 200 I=1,N1
        YTEMP(I)=Y1(I) - Y2(I)
  200 CONTINUE
!
      CALL MEAN(Y1,N1,IWRITE,Y1MEAN,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,Y1SD,IBUGA3,IERROR)
      Y1VAR=Y1SD**2
      CALL SDMEAN(Y1,N1,IWRITE,Y1SDM,IBUGA3,IERROR)
!
      CALL MEAN(Y2,N2,IWRITE,Y2MEAN,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,Y2SD,IBUGA3,IERROR)
      Y2VAR=Y2SD**2
      CALL SDMEAN(Y2,N2,IWRITE,Y2SDM,IBUGA3,IERROR)
!
      CALL MEAN(YTEMP,N2,IWRITE,YDMEAN,IBUGA3,IERROR)
      CALL SD(YTEMP,N2,IWRITE,YDSD,IBUGA3,IERROR)
      YDVAR=YDSD**2
      CALL SDMEAN(YTEMP,N2,IWRITE,YDSDM,IBUGA3,IERROR)
!
      AN1=N1
      AN2=N2
!
      DEL=Y1MEAN-Y2MEAN
      STATVA=DEL/YDSDM
      IDF=N1-1
      STATNU=REAL(IDF)
      CALL TCDF(STATVA,STATNU,STATCD)
!
      PVALLT=STATCD
      PVALUT=1.0 - STATCD
      IF(STATVA.LE.0.0)THEN
        PVAL2T=2.0*PVALLT
      ELSE
        PVAL2T=2.0*PVALUT
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
        WRITE(ICOUT,811)STATVA
  811   FORMAT('THE VALUE OF THE PAIRED TWO SAMPLE T-TEST = ',G15.7)
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
      IWRITE=IWRTSV
!
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
!
      RETURN
      END SUBROUTINE DPTTE6
      SUBROUTINE DPTTTP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN EMPIRICAL TOTAL TIME ON TEST PLOT.
!
!              FOR UNCENSORED DATA, THE TOTAL TIME ON TEST STATISTIC IS
!              (X IS SORTED)
!
!                  TTT(i) = SUM[j=1 to i][(N-j+1)*(X(j) - X(j-1))]
!                  X(0) = 0
!
!              THE SCALED TOTAL TIME ON TEST IS
!
!                  TTT*(i) = TTT(i)/TTT(n)
!
!              FOR CENSORED DATA, USE
!
!                  TTT*(i) = TTT(i)/TTT(r)
!
!              WHERE r IS THE MAXIMUM UNCENSORED FAILURE TIME
!
!              FOR THIS PLOT, TTT*(I) IS PLOTTED AGAINST i/N.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     REFERENCE--HORST RINNE (2010), "Location–Scale Distributions:
!                Linear Estimation and Probability Plotting Using
!                MATLAB".
!              --EPSTEIN, B. AND SOBEL, M. (1953), "Life testing",
!                Journal of the American Statistical Association,
!                48, 486–502.
!              --BARLOW, R.E. and CAMPO, R. (1975), "Time on test
!                processes and applications to failure data analysis",
!                in BARLOW, FUSSSELL, and SINGPURWALLA(eds.) "Reliability
!                and Fault Tree Analysis", SIAM, Philadelphia, 451–481.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2020/06
!     ORIGINAL VERSION--JUNE      2020.
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
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),TAG1(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP2(1))
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
      ISUBN1='DPTT'
      ISUBN2='TP  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TTTP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTTTP--')
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
!               ****************************************
!               **  TREAT THE TOTAL TIME ON TEST PLOT **
!               ****************************************
!
!               *******************************************
!               **  STEP 1--                             **
!               **  SEARCH FOR COMMAND                   **
!               *******************************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TTTP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.1.AND.ICOM.EQ.'TTT '.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        ICASPL='TTTP'
      ELSEIF(NUMARG.GE.4.AND.ICOM.EQ.'TOTA'.AND.   &
             IHARG(1).EQ.'TIME' .AND. IHARG(2).EQ.'ON  '.AND.   &
             IHARG(3).EQ.'TEST' .AND. IHARG(4).EQ.'PLOT')THEN
        ILASTC=4
        ICASPL='TTTP'
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TTTP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='TOTAL TIME ON TEST PLOT'
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TTTP')THEN
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
                   'ICOLR(I) = ',I8,2X,2A4,2X,3I8)
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TTTP')   &
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
      CALL DPTTT2(Y1,TAG1,NS,NUMVAR,ICASPL,MAXOBV,   &
                  TEMP1,TEMP2,   &
                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TTTP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTTTP--')
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
      END SUBROUTINE DPTTTP
      SUBROUTINE DPTTT2(Y1,TAG1,N,NUMV,ICASPL,MAXNXT,   &
                        TEMP1,TEMP2,   &
                        Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN EMPIRICAL TOTAL TIME ON TEST PLOT.
!
!              FOR UNCENSORED DATA, THE TOTAL TIME ON TEST STATISTIC IS
!              (X IS SORTED)
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
!              FOR THIS PLOT, TTT*(I) IS PLOTTED AGAINST i/N.
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
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN EMPIRICAL TOTAL TIME ON TEST PLOT.
!
!              FOR UNCENSORED DATA, THE TOTAL TIME ON TEST STATISTIC IS
!              (X IS SORTED)
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
!              FOR THIS PLOT, TTT*(I) IS PLOTTED AGAINST i/N.
!
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2020/06
!     ORIGINAL VERSION--JUNE      2020.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASE
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION TAG1(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
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
      ISUBN1='DPTT'
      ISUBN2='T2  '
      IERROR='NO'
!
      NPLOTP=0
      NPLOTV=0
      J=0
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TTT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTTT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,IERROR
   52   FORMAT('IBUGG3,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,N,MAXNXT,NUMV
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
  111   FORMAT('***** ERROR IN TOTAL TIME ON TEST PLOT--')
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
      IF(NUMV.EQ.1)THEN
        DO 130 I=1,N
          TAG1(I)=1.0
  130   CONTINUE
      ENDIF
!
      ICASE='SCAL'
      CALL TTT(Y1,TAG1,N,ICASE,IWRITE,Y,NPLOTP,TEMP1,TEMP2,MAXNXT,   &
               IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      AN=REAL(N)
      DO 150 I=1,NPLOTP
        X(I)=REAL(I)/AN
        D(I)=1.0
  150 CONTINUE
!
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=2.0
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=2.0
!
      NPLOTV=2
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TTT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTTT2--')
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
      END SUBROUTINE DPTTT2
      SUBROUTINE DPTUMD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IANGLU,MAXNPP,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--FORM A TUKEY MEAN DIFFERENCE PLOT
!              (USEFUL FOR DISTRIBUTIONALLY COMPARING 2 DATA SETS).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/8
!     ORIGINAL VERSION--SEPTEMBER 1999 .
!     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3
!     UPDATED         --FEBRUARY  2011. SUPPORT FOR "HIGHLIGHTED" OPTION
!     UPDATED         --JUNE      2016. ALLOW USER-SPECIFED PERCENTILES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASE
      CHARACTER*4 IHIGH
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
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
!
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
      ISUBN1='DPTU'
      ISUBN2='MD  '
      IFOUND='NO'
      IERROR='NO'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
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
!
!               *******************************************
!               **  TREAT THE TUKEY MEAN-DIFFERENCE CASE **
!               *******************************************
!
!               ***************************
!               **  STEP 11--            **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHIGH='OFF'
      IF(ICOM.EQ.'TUKE')THEN
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MEAN'.AND.   &
          IHARG(2).EQ.'DIFF')THEN
           IF((IHARG(3).EQ.'HIGH' .OR. IHARG(3).EQ.'SUBS') .AND.   &
             IHARG(4).EQ.'PLOT')THEN
             IHIGH='ON'
             ILASTC=4
           ELSEIF(IHARG(3).EQ.'PLOT')THEN
             ILASTC=3
           ELSE
             GO TO 9000
           ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'M   '.AND.   &
              IHARG(2).EQ.'D   ')THEN
          IF((IHARG(3).EQ.'HIGH' .OR. IHARG(3).EQ.'SUBS') .AND.   &
            IHARG(4).EQ.'PLOT')THEN
             ILASTC=4
             IHIGH='ON'
          ELSEIF(IHARG(3).EQ.'PLOT')THEN
             ILASTC=3
          ELSE
             GO TO 9000
          ENDIF
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'MD  ')THEN
          IF((IHARG(2).EQ.'HIGH' .OR. IHARG(2).EQ.'SUBS') .AND.   &
            IHARG(3).EQ.'PLOT')THEN
             ILASTC=3
             IHIGH='ON'
          ELSEIF(IHARG(2).EQ.'PLOT')THEN
             ILASTC=2
          ELSE
             GO TO 9000
          ENDIF
        ENDIF
      ELSEIF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')THEN
        IHIGH='ON'
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'TUKE'.AND.   &
          IHARG(2).EQ.'MEAN'.AND.IHARG(3).EQ.'DIFF'.AND.   &
          IHARG(4).EQ.'PLOT')THEN
             IHIGH='ON'
             ILASTC=4
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'TUKE'.AND.   &
          IHARG(2).EQ.'M   '.AND.IHARG(3).EQ.'D   '.AND.   &
          IHARG(4).EQ.'PLOT')THEN
             ILASTC=3
             IHIGH='ON'
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'TUKE'.AND.   &
          IHARG(2).EQ.'MD  '.AND.IHARG(3).EQ.'PLOT')THEN
             ILASTC=3
        ELSE
          GO TO 9000
        ENDIF
      ELSE
        GO TO 9000
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
      ICASPL='TUMD'
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      DO 290 I=1,MAX(NRIGHT(1),NRIGHT(2))
        XHIGH(I)=1.0
  290 CONTINUE
!
!     IN ORDER TO ACCOMODATE MATRIX ARGUMENTS, CALL EACH
!     VARIABLE SEPARATELY.
!
      NUMVA2=1
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,Y1,Y1,NS1,NTEMP,NTEMP,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      ICOL=2
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y2,Y2,Y2,NS2,NTEMP,NTEMP,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
!
      IF(IHIGH.EQ.'ON')THEN
        ICOL=3
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    XHIGH,XHIGH,XHIGH,NHIGH,NTEMP,NTEMP,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
      ELSE
        NHIGH=0
      ENDIF
!
!               ****************************************************
!               **  STEP 41--                                      *
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          *
!               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY) FOR    *
!               **   THE PLOT.                                     *
!               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .    *
!               **  THIS WILL BE BOTH ONES FOR BOTH CASES          *
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  *
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  *
!               ****************************************************
!
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TUMD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NS=NS1
      IF(NS2.GT.NS1)NS=NS2
      CALL DPTUM2(Y1,NS1,Y2,NS2,ICASPL,MAXN,IQQNPR,   &
                  Y,X,D,NPLOTP,NPLOTV,   &
                  YLARGE,YSMALL,TEMP1,TEMP2,   &
                  XHIGH,NHIGH,XDIST,   &
                  IBUGG3,ISUBRO,IERROR)
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9013   FORMAT('NPLOTV,NPLOTP,NUMVAR,NS,ICASPL,IAND1,IAND2 = ',   &
               4I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASPL,MAXN,NUMVAR
 9014   FORMAT('ICASPL,MAXN,NUMVAR = ',A4,I8,I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO 9020 I=1,NPLOTP
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPTUMD
      SUBROUTINE DPTUM2(Y,NY,X,NX,ICASPL,MAXN,IQQNPR,   &
                        Y2,X2,D2,N2,NPLOTV,   &
                        YLARGE,YSMALL,TEMP1,TEMP2,   &
                        XHIGH,NHIGH,XDIST,   &
                        IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A TUKEY MEAN-DIFFERENCE PLOT
!              (USEFUL FOR DISTRIBUTIONALLY COMPARING 2 DATA SETS).
!              AFTER CALCULATING COORDINATES FOR Q-Q PLOT, CALCULATE
!              (Bi - Ti) VERSUS (Bi+Ti)/2 WHERE Bi AND Ti ARE
!              THE QUANTILES FOR THE RESPECTIVE DATA SETS.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/9
!     ORIGINAL VERSION--SEPTEMBER 1999.
!     UPDATED         --FEBRUARY  2011.
!     UPDATED         --JUNE      2016. ALLOW USER-SPECIFED PERCENTILES
!     UPDATED         --JUNE      2016. DON'T TREAT N=1 OR ALL DATA
!                                       VALUES EQUAL AS AN ERROR.  TREAT
!                                       AS A "DEGENERATE" CASE.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASE
      CHARACTER*4 ICASPL
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION XHIGH(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
      DIMENSION YLARGE(*)
      DIMENSION YSMALL(*)
      DIMENSION XDIST(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPQU'
      ISUBN2='M2  '
      IERROR='NO'
      IWRITE='OFF'
      ICASE=ICASPL
!
      ANY=NY
      ANX=NX
!
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
          DO 61 I=1,NY
            WRITE(ICOUT,62)I,Y(I)
   62       FORMAT('I,Y(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
        IF(NX.GE.1)THEN
          DO 71 I=1,NX
           WRITE(ICOUT,72)I,X(I)
   72      FORMAT('I,X(I) = ',I8,G15.7)
           CALL DPWRST('XXX','BUG ')
   71    CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!     2016/06: ONLY REQUIRE N >= 1.
!
!CCCC IF(NY.LT.2)THEN
      IF(NY.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN TUKEY MEAN DIFFERENCE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',   &
               'RESPONSE VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)
 1113   FORMAT('      MUST BE AT LEAST 1;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NY
 1114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NX.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1122)
 1122   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',   &
               'RESPONSE VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NX
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NHIGH.GT.0 .AND. NHIGH.NE.MIN(NX,NY))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1125)
 1125   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE HIGHLIGHTING ',   &
               'VARIABLE IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1126)
 1126   FORMAT('      NOT EQUAL TO THE NUMBER OF OBSERVATIONS IN THE ',   &
               'SHORTER RESPONSE VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1127)NY
 1127   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST     ',   &
               'RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1128)NX
 1128   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND    ',   &
               'RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1129)NHIGH
 1129   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE HIGHLIGHT ',   &
               'VARIABLE          = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!CCCC HOLD=Y(1)
!CCCC DO1130I=1,NY
!CCCC   IF(Y(I).NE.HOLD)GO TO 1139
!1130 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1111)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1132)
!1132 FORMAT('      ALL INPUT ELEMENTS FOR THE FIRST RESPONSE VARIABLE')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1133)HOLD
!1133 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!1139 CONTINUE
!
!CCCC HOLD=X(1)
!CCCC DO1140I=1,NY
!CCCC   IF(X(I).NE.HOLD)GO TO 1149
!1140 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1111)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1142)
!1142 FORMAT('      ALL INPUT ELEMENTS FOR THE SECOND RESPONSE ',
!CCCC1       'VARIABLE')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1133)HOLD
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!1149 CONTINUE
!
!               ****************************************************
!               **  STEP 21--                                     **
!               **  SORT Y AND SORT X                             **
!               ****************************************************
!
      IF(NHIGH.LE.0)THEN
        IF(IQQNPR.GT.0)THEN
          CALL PERCE2(IQQNPR,X,NX,IWRITE,TEMP2,MAXN,TEMP1,   &
                      IBUGG3,ISUBRO,IERROR)
          DO 2010 II=1,IQQNPR
            X(II)=TEMP1(II)
 2010     CONTINUE
          NX=IQQNPR
!
          CALL PERCE2(IQQNPR,Y,NY,IWRITE,TEMP2,MAXN,TEMP1,   &
                      IBUGG3,ISUBRO,IERROR)
          DO 2020 II=1,IQQNPR
            Y(II)=TEMP2(II)
 2020     CONTINUE
          NY=IQQNPR
!
        ELSE
          CALL SORT(X,NX,X)
          CALL SORT(Y,NY,Y)
        ENDIF
      ELSEIF(NY.LE.NX)THEN
        CALL SORT(X,NX,X)
        CALL SORTC(Y,XHIGH,NY,Y,XDIST)
        DO 2101 I=1,NY
          XHIGH(I)=XDIST(I)
 2101   CONTINUE
      ELSEIF(NY.GT.NX)THEN
        CALL SORT(Y,NY,Y)
        CALL SORTC(X,XHIGH,NX,X,XDIST)
        DO 2103 I=1,NX
          XHIGH(I)=XDIST(I)
 2103   CONTINUE
      ENDIF
!
!               *****************************************
!               **  STEP 22--                          **
!               **  DETERMINE THE TYPE CASE            **
!               **  EQUAL SAMPLE SIZES OR NOT)         **
!               **  AND BRANCH ACORDINGLY              **
!               *****************************************
!
      ICASE='UNEQ'
      IF(NY.EQ.NX)ICASE='EQUA'
      IF(ICASE.EQ.'EQUA')GO TO 5100
!
!               **************************************************
!               **  STEP 23--                                   **
!               **  DETERMINE THE SMALLER OF THE 2--            **
!               **  NY OR NX                                    **
!               **  DETERMINE THE LARGER OF THE 2--             **
!               **  NY OR NX                                    **
!               **************************************************
!
      NSMALL=NX
      IF(NY.LT.NX)NSMALL=NY
      ANSMAL=NSMALL
!
      NLARGE=NX
      IF(NY.GT.NX)NLARGE=NY
      ANLARG=NLARGE
!
!               ****************************************************
!               **  STEP 24--                                     **
!               **  STEP THROUGH THE VARIOUS SORTED VALUES OF     **
!               **  THE SMALLER OF Y OR X.                        **
!               **  COMPUTE A CORRESPONDING PERCENTAGE.           **
!               **  ESTIMATE THIS PERCENT  POINT                  **
!               **  IN THE LARGER OF Y OR X.                      **
!               ****************************************************
!
      DO 2400 I=1,NSMALL
        AI=I
        PSMALL=(AI-0.5)/ANSMAL
        IF(NY.LE.NX)YSMALL(I)=Y(I)
        IF(NY.GT.NX)YSMALL(I)=X(I)
!
        PLARGE=0.0
        DO 2410 J=1,NLARGE
          AJ=J
          J2=J
          J2M1=J2-1
          PPRIOR=PLARGE
          PLARGE=(AJ-0.5)/ANLARG
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TUM2')THEN
            WRITE(ICOUT,777)I,J,J2,J2M1,PSMALL,PLARGE,PPRIOR
  777       FORMAT('I,J,J2,J2M1,PSMALL,PLARGE,PPRIOR = ',4I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(PLARGE.LT.PSMALL)GO TO 2410
          IF(PLARGE.EQ.PSMALL)THEN
            IF(NY.LE.NX)YLARGE(I)=X(J2)
            IF(NY.GT.NX)YLARGE(I)=Y(J2)
          ELSE
            RATIO=(PSMALL-PPRIOR)/(PLARGE-PPRIOR)
            IF(NY.LE.NX)YLARGE(I)=RATIO*X(J2M1)+(1.0-RATIO)*X(J2)
            IF(NY.GT.NX)YLARGE(I)=RATIO*Y(J2M1)+(1.0-RATIO)*Y(J2)
          ENDIF
          GO TO 2400
 2410   CONTINUE
 2400 CONTINUE
!
!               *******************************************
!               **  STEP 51--                            **
!               **  FORM PLOT COORDINATES                **
!               *******************************************
!
 5100 CONTINUE
!
      ISTEPN='51'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TUM2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NHIGH.GT.0)THEN
        CALL CODE(XHIGH,NHIGH,IWRITE,XDIST,D2,MAXN,IBUGG3,IERROR)
        CALL MAXIM(XDIST,NHIGH,IWRITE,XMAX,IBUGG3,IERROR)
      ELSE
        XMAX=1.0
      ENDIF
!
      IF(ICASE.EQ.'EQUA')THEN
        J=0
        DO 5111 I=1,NY
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
!
      ELSE
!
        J=0
        DO 5121 I=1,NSMALL
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
!
        J=J+1
        X2(J)=X2(1)
        Y2(J)=0.0
        D2(J)=XMAX+1.0
        J=J+1
        X2(J)=X2(NSMALL)
        Y2(J)=0.0
        D2(J)=XMAX+1.0
      ENDIF
!
      N2=J
      NPLOTV=3
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9031)NY,NX,NSMALL,NLARGE,RATIO
 9031   FORMAT('NY,NX,NSMALL,NLARGE,RATIO = ',4I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 9032 I=1,NLARGE
          WRITE(ICOUT,9033)I,YLARGE(I)
 9033     FORMAT('I,YLARGE(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9032   CONTINUE
        DO 9042 I=1,NSMALL
          WRITE(ICOUT,9043)I,YSMALL(I)
 9043     FORMAT('I,YSMALL(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9042   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTUM2
      SUBROUTINE DPTWFP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GIVEN DATA OF THE FORM
!
!                 RESPONSE   LAB-ID  MAT-ID
!
!              GENERATE EITHER A "LABORATORIES WITHIN MATERIALS"
!              OR A "MATERIALS WITHIN LABORATORIES" PLOT.
!
!              THIS IS ESSENTIALLY A RUN SEQUENCE PLOT SORTED
!              BY 2 FACTOR VARIABLES.
!
!              THIS PLOT IS MOTIVATED BY THE DESIRE TO PLOT
!              RESIDUALS FOR THE "PHASE 3" ANALYSIS IN THE
!              ASTM E-691 STANDARD.  THE PHASE 3 ANALYSIS
!              (ESSENTIALLY A ROW-LINEAR MODEL FOR THE TABLE)
!              WAS SUGGESTED BY JOHN MANDEL (SEE REFERENCES
!              BELOW) AS AN ADDITIONAL STEP IN THE E-691 ANALYSIS
!              (THE PHASE 3 ANALYSIS IS NOT PART OF THE STANDARD).
!              IN PARTICULAR, HE RECOMMENDED A PLOT OF THE
!              STANDARDIZED RESIDUALS FROM THE ROW-LINEAR MODEL
!              (SPECIFIC PLOTS FOR THE H AND K CONSISTENCY
!              STATISTICS CAN ALREADY BE GENERATED USING THE
!              H CONSISTENCY PLOT COMMAND).
!
!              ALTHOUGH MOTIVATED BY THE EXTENSION TO THE
!              ASTM E-691 STANDARD, THIS PLOT CAN BE APPLIED
!              TO ANY TWO FACTOR SET OF DATA.
!
!              THERE ARE TWO FORMATS FOR THE PLOT:
!
!              1) THE VALUES ARE PLOTTED LINEARLY.  THAT IS,
!
!                 LAB:  1  2  3  1  2  3  1  2  3
!                 MAT:  1  1  1  2  2  2  3  3  3
!
!              2) YOU CAN STACK THE LAB VALUES VERTICALLY
!
!                 LAB:  1  1  1
!                       2  2  2
!                       3  3  3
!                 MAT:  1  2  3
!
!              MULTIPLE AND REPLICATION OPTIONS ARE NOT SUPPORTED
!              FOR THIS PLOT.
!
!
!     EXAMPLE--TWO FACTOR PLOT Y LABID MATID
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015/6
!     ORIGINAL VERSION--JUNE       2015.
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
      REAL Y1(MAXOBV)
      REAL MATID(MAXOBV)
      REAL LABID(MAXOBV)
      REAL XIDTEM(MAXOBV)
      REAL XIDTE2(MAXOBV)
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),MATID(1))
      EQUIVALENCE (GARBAG(IGARB3),LABID(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP2(1))
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
      ISUBN1='DPTW'
      ISUBN2='FP  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ****************************************
!               **  TREAT THE TWO FACTOR PLOT CASE    **
!               ****************************************
!
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
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.2.AND.ICOM.EQ.'TWO '.AND.IHARG(1).EQ.'FACT'.AND.   &
        IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        ICASPL='TWFP'
      ELSE
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!               **       LABID(.)                           **
!               **       MATID(.)                           **
!               **********************************************
!
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,LABID,MATID,XIDTEM,XIDTEM,XIDTEM,XIDTEM,NS,   &
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWFP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPTWF2(Y1,LABID,MATID,NS,NUMVAR,ICASPL,   &
                  ITWFPT,ITWFGP,ITWFLM,   &
                  ITWFM1,ITWFM2,ITWFL1,ITWFL2,   &
                  XIDTEM,XIDTE2,TEMP1,TEMP2,   &
                  Y,X,D,   &
                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 9--   **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPTWFP
      SUBROUTINE DPTWF2(Y1,LABID,MATID,N,NUMVAR,ICASPL,   &
                        ITWFPT,ITWFGP,ITWFLM,             &
                        ITWFM1,ITWFM2,ITWFL1,ITWFL2,      &
                        XIDTEM,XIDTE2,TEMP1,TEMP2,        &
                        Y,X,D,                            &
                        NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GIVEN DATA OF THE FORM
!
!                 RESPONSE   LAB-ID  MAT-ID
!
!              GENERATE EITHER A "LABORATORIES WITHIN MATERIALS"
!              OR A "MATERIALS WITHIN LABORATORIES" PLOT.
!
!              THIS IS ESSENTIALLY A RUN SEQUENCE PLOT SORTED
!              BY 2 FACTOR VARIABLES.
!
!              THIS PLOT IS MOTIVATED BY THE DESIRE TO PLOT
!              RESIDUALS FOR THE "PHASE 3" ANALYSIS IN THE
!              ASTM E-691 STANDARD.  THE PHASE 3 ANALYSIS
!              (ESSENTIALLY A ROW-LINEAR MODEL FOR THE TABLE)
!              WAS SUGGESTED BY JOHN MANDEL (SEE REFERENCES
!              BELOW) AS AN ADDITIONAL STEP IN THE E-691 ANALYSIS
!              (THE PHASE 3 ANALYSIS IS NOT PART OF THE STANDARD).
!              IN PARTICULAR, HE RECOMMENDED A PLOT OF THE
!              STANDARDIZED RESIDUALS FROM THE ROW-LINEAR MODEL
!              (SPECIFIC PLOTS FOR THE H AND K CONSISTENCY
!              STATISTICS CAN ALREADY BE GENERATED USING THE
!              H CONSISTENCY PLOT COMMAND).
!
!              ALTHOUGH MOTIVATED BY THE EXTENSION TO THE
!              ASTM E-691 STANDARD, THIS PLOT CAN BE APPLIED
!              TO ANY TWO FACTOR SET OF DATA.
!
!              THERE ARE TWO FORMATS FOR THE PLOT:
!
!              1) THE VALUES ARE PLOTTED LINEARLY.  THAT IS,
!
!                 LAB:  1  2  3  1  2  3  1  2  3
!                 MAT:  1  1  1  2  2  2  3  3  3
!
!              2) YOU CAN STACK THE LAB VALUES VERTICALLY
!
!                 LAB:  1  1  1
!                       2  2  2
!                       3  3  3
!                 MAT:  1  2  3
!
!              MULTIPLE AND REPLICATION OPTIONS ARE NOT SUPPORTED
!              FOR THIS PLOT.
!
!     REFERENCES--"Standard Practice for Conducting an Interlaboratory
!                 Study to Determine the Precision of a Test Method",
!                 ASTM International, 100 Barr Harbor Drive, PO BOX C700,
!                 West Conshohoceken, PA 19428-2959, USA.
!               --Mandel (1994), "Analyzing Interlaboratory Data
!                 According to ASTM Standard E691", Quality and
!                 Statistics: Total Quality Management,ASTM STP 1209,
!                 Kowalewski, Ed., American Society for Testing and
!                 Materials, Philadelphia, PA 1994, pp. 59-70.
!               --Mandel (1995), "Structure and Outliers in
!                 Interlaboratory Studies", Journal of Testing and
!                 Evaluation, Vol. 23, No. 5, pp. 364-369.
!               --Mandel (1991), "Evaluation and Control of
!                 Measurements", Marcel Dekker, Inc., chapter 7.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015/6
!     ORIGINAL VERSION--JUNE      2015.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ITWFPT
      CHARACTER*4 ITWFLM
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      REAL Y1(*)
      REAL MATID(*)
      REAL LABID(*)
      REAL XIDTEM(*)
      REAL XIDTE2(*)
      REAL TEMP1(*)
      REAL TEMP2(*)
!
      REAL Y(*)
      REAL X(*)
      REAL D(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTW'
      ISUBN2='F2  '
      IWRITE='OFF'
      IERROR='NO'
      NPLOTP=0
      NPLOTV=3
!
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
          DO 81 I=1,N
            WRITE(ICOUT,82)I,Y1(I),MATID(I),LABID(I)
   82       FORMAT('I,Y1(I),MATID(I),LABID(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
!               **  FOR THE GROUP VARIABLES (LABID, MATID).         **
!               **  CHECK FOR MISSING CELLS AND FOR REPLICATION     **
!               **  WITHIN CELLS (REPLICATED VALUES WILL BE         **
!               **  REPLACED WITH THEIR MEAN VALUE).                **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWF2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(LABID,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(MATID,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
!
!     CHECK FOR MISSING CELLS (PLOT CURRENTLY NOT SUPPORTED FOR
!     CASE WHERE THERE IS MISSING CELLS).  IF REPLICATION IS DETECTED,
!     REPLACE RAW DATA WITH CELL AVERAGES.
!
      IREPL=0
      DO 110 ISET1=1,NUMSE1
        AHOLD1=XIDTEM(ISET1)
        DO 120 ISET2=1,NUMSE2
          AHOLD2=XIDTE2(ISET2)
          K=0
          DO 130 I=1,N
            IF(LABID(I).EQ.AHOLD1 .AND. MATID(I).EQ.AHOLD2)THEN
              K=K+1
              GO TO 139
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
  148       FORMAT('      THIS COMMAND IS NOT SUPPORTED FOR THE CASE ',   &
                   'WHERE THERE ARE MISSING CELLS.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ELSEIF(K.GT.1)THEN
            IREPL=1
          ENDIF
  120   CONTINUE
  110 CONTINUE
!
!     IF REPLICATION DETECTED, REPLACE RAW VALUES WITH MEANS
!
      ICNT=0
      IF(IREPL.EQ.1)THEN
        DO 210 ISET1=1,NUMSE1
          AHOLD1=XIDTEM(ISET1)
          DO 220 ISET2=1,NUMSE2
            AHOLD2=XIDTE2(ISET2)
            K=0
            DO 230 I=1,N
              IF(LABID(I).EQ.AHOLD1 .AND. MATID(I).EQ.AHOLD2)THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
  230       CONTINUE
!
            ICNT=ICNT+1
            IF(K.EQ.1)THEN
              TEMP2(ICNT)=TEMP1(1)
            ELSE
              CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
              TEMP2(ICNT)=XMEAN
            ENDIF
            XIDTEM(ICNT)=AHOLD1
            XIDTE2(ICNT)=AHOLD2
!
  220     CONTINUE
  210   CONTINUE
!
        DO 310 I=1,ICNT
          Y(I)=TEMP2(I)
          LABID(I)=XIDTEM(I)
          MATID(I)=XIDTE2(I)
  310   CONTINUE
        N=ICNT
      ENDIF
!
!
!
!               ********************************************
!               **  STEP 2--                              **
!               **  GENERATE THE PLOT COORDINATES.        **
!               ********************************************
!
!       NOTE: TYPICALLY, WE WANT TO COMPUTE THE H AND K CONSISTENCY
!             STATISTICS BASED ON ALL LAB's AND MATERIALS.  HOWEVER,
!             WE SOMETIMES WANT TO RESTRICT THE PLOT TO A SUBSET
!             OF MATERIALS OR LABORATORIES FOR BETTER PLOT
!             RESOLUTION.
!
!             TO ADDRESS THIS, THE FOLLOWING COMMANDS WERE ADDED:
!
!                 SET TWO FACTOR PLOT MATERIAL   FIRST <value>
!                 SET TWO FACTOR PLOT MATERIAL   LAST  <value>
!                 SET TWO FACTOR PLOT LABORATORY FIRST <value>
!                 SET TWO FACTOR PLOT LABORATORY LAST  <value>
!
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
!
      NPLOTP=0
!
      IF(ITWFPT.EQ.'DEFA')THEN
        IXCNT=0
        IXCNT2=0
        IF(ITWFLM.EQ.'LABO')THEN
          DO 1010 J=1,NMAT
            DO 1020 I=1,NLAB
              IXCNT=IXCNT+1
              IF(J.LT.IMAT1 .OR. J.GT.IMAT2)GO TO 1020
              IF(I.LT.ILAB1 .OR. I.GT.ILAB2)GO TO 1020
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
          DO 1030 J=1,NLAB
            DO 1040 I=1,NMAT
              IF(J.LT.ILAB1 .OR. J.GT.ILAB2)GO TO 1040
              IF(I.LT.IMAT1 .OR. I.GT.IMAT2)GO TO 1040
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
          DO 1110 J=1,NMAT
            DO 1120 I=1,NLAB
              IXCNT=IXCNT+1
              IF(J.LT.IMAT1 .OR. J.GT.IMAT2)GO TO 1120
              IF(I.LT.ILAB1 .OR. I.GT.ILAB2)GO TO 1120
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=Y1(IXCNT)
              X(NPLOTP)=REAL(J)
              D(NPLOTP)=REAL(I)
 1120       CONTINUE
 1110     CONTINUE
          ITAG=NLAB
          NLAST=NMAT
        ELSE
          DO 1130 J=1,NLAB
            DO 1140 I=1,NMAT
              IF(J.LT.ILAB1 .OR. J.GT.ILAB2)GO TO 1140
              IF(I.LT.IMAT1 .OR. I.GT.IMAT2)GO TO 1140
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9035 I=1,NPLOTP
            WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036       FORMAT('I,Y(I),X(I),D(I) = ',I8,2G15.7,F9.2)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPTWF2
      SUBROUTINE DPTWPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE MANDEL'S ROW-LINEAR OR COLUMN-LINEAR PLOTS FOR
!              JOHN MANDEL'S TWO-WAY TABLE ANALYSIS
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
!     VERSION NUMBER--2015/06
!     ORIGINAL VERSION--JUNE      2015.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
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
      CHARACTER*12 IX1LAB
      CHARACTER*12 IX2LAB
      CHARACTER*25 IYLAB
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
!
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
!
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
      ISUBN1='DPTW'
      ISUBN2='PL  '
      ICASPL='TWOW'
      ICASP2='ROW'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ******************************************
!               **  TREAT THE CROSS TABULATE PLOT CASE  **
!               ******************************************
!
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
!
!               *************************************
!               **  STEP 1--                       **
!               **  EXTRACT THE COMMAND            **
!               **  COMMAND SYNTAX IS:             **
!               **  TWO-WAY <ROW/COLUMN> PLOT      **
!               *************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 9000
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
          GO TO 9000
        ENDIF
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
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
!     NEED FOLLOWING VARIABLES:
!     1) TWO GROUP-ID VARIABLE
!     2) ONE RESPONSE VARIABLE
!        VARIABLES
!
!
!               ********************************
!               **  STEP 3--                  **
!               **  EXTRACT THE DATA          **
!               ********************************
!
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,TAG1,TAG2,NLOCAL,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
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
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,TAG1,TAG2,TEMP1,TEMP1,TEMP1,TEMP1,NLOCAL,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
      ENDIF
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  STEP 4--                                        **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS           **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.              **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).   **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).   **
!               ******************************************************
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TWPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPTWP2(Y1,TAG1,TAG2,NLOCAL,NUMVAR,ICASPL,ICASP2,   &
                  IYLAB,IX1LAB,IX2LAB,   &
                  ICAPSW,ICAPTY,IFORSW,   &
                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,XIDTEM,XIDTE2,   &
                  Y,X,D,NPLOTP,NPLOTV,ISUBRO,IBUGG3,IERROR)
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9025 I=1,NPLOTP
            WRITE(ICOUT,9026)I,Y(I),X(I),D(I)
 9026       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPTWPL
      SUBROUTINE DPTWP2(Y,TAG1,TAG2,N,NUMV2,ICASPL,ICASP2,   &
                        IYLAB,IX1LAB,IX2LAB,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        TEMP1,TEMP2,COLAVE,ROWAVE,SLOPES,XIDTEM,XIDTE2,   &
                        Y2,X2,D2,N2,NPLOTV,ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--GENERATE MANDEL'S ROW-LINEAR OR COLUMN-LINEAR PLOTS FOR
!              JOHN MANDEL'S TWO-WAY TABLE ANALYSIS
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015/06
!     ORIGINAL VERSION--JUNE      2015.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION COLAVE(*)
      DIMENSION ROWAVE(*)
      DIMENSION SLOPES(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
!
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=45)
      CHARACTER*65 ITITLE
      CHARACTER*1  ITITL9
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
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
      LOGICAL IFLAGA
      LOGICAL IFLAGB
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOST.INC'
!
      CHARACTER*4 IRTFMZ
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
!
      ATAG=0.0
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWP2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPTWP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IBUGG3,ISUBRO,ICASPL,ICASP2,N,NUMV2
   71   FORMAT('IBUGG3,ISUBRO,ICASPL,ICASP2,N,NUMV2 = ',4(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ITWOYA,ITWOFI,ITWOAV,ITWOAN
   72   FORMAT('ITWOYA,ITWOFI,ITWOAV,ITWOAN = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO 73 I=1,N
          WRITE(ICOUT,74)I,Y(I),TAG1(I),TAG2(I)
   74     FORMAT('I, Y(I),TAG1(I)TAG2(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
!
!     WRITE FOLLOWING TO DPST1F.DAT
!
!       1. DPST1F.DAT: ROW, HEIGHT, SLOPE, RESSD, STANDARD ERROR OF SLOPE
!       2. DPST2F.DAT: COLUMN, COLUMN AVERAGE
!       3. DPST3F.DAT: ROW, COL, Y(ROW,COL), RES(ROW,COL), PRED(ROW,COL)
!
      IOP='OPEN'
      IFLG11=1
      IFLG21=1
      IFLG31=1
      IFLG41=0
      IFLG51=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLA41,IFLG51,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
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
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
!               **  FOR THE GROUP VARIABLES (TAG1, TAG2)            **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
!               **  WHICH IS AN ERROR CONDITION FOR A PLOT.         **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWP2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(TAG1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(TAG2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
!
!     CHECK FOR MISSING CELLS (PLOT CURRENTLY NOT SUPPORTED FOR
!     CASE WHERE THERE IS MISSING CELLS).  IF REPLICATION IS DETECTED,
!     REPLACE RAW DATA WITH CELL AVERAGES.
!
      IREPL=0
      DO 110 ISET1=1,NUMSE1
        AHOLD1=XIDTEM(ISET1)
        DO 120 ISET2=1,NUMSE2
          AHOLD2=XIDTE2(ISET2)
          K=0
          DO 130 I=1,N
            IF(TAG1(I).EQ.AHOLD1 .AND. TAG2(I).EQ.AHOLD2)THEN
              K=K+1
              GO TO 139
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
  148       FORMAT('      THIS COMMAND IS NOT SUPPORTED FOR THE CASE ',   &
                   'WHERE THERE ARE MISSING CELLS.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ELSEIF(K.GT.1)THEN
            IREPL=1
          ENDIF
  120   CONTINUE
  110 CONTINUE
!
!     IF REPLICATION DETECTED, REPLACE RAW VALUES WITH MEANS
!
      ICNT=0
      IF(IREPL.EQ.1)THEN
        DO 210 ISET1=1,NUMSE1
          AHOLD1=XIDTEM(ISET1)
          DO 220 ISET2=1,NUMSE2
            AHOLD2=XIDTE2(ISET2)
            K=0
            DO 230 I=1,N
              IF(TAG1(I).EQ.AHOLD1 .AND. TAG2(I).EQ.AHOLD2)THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
  230       CONTINUE
!
            ICNT=ICNT+1
            IF(K.EQ.1)THEN
              TEMP2(ICNT)=TEMP1(1)
            ELSE
              CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
              TEMP2(ICNT)=XMEAN
            ENDIF
            XIDTEM(ICNT)=AHOLD1
            XIDTE2(ICNT)=AHOLD2
!
  220     CONTINUE
  210   CONTINUE
!
        DO 310 I=1,ICNT
          Y(I)=TEMP2(I)
          TAG1(I)=XIDTEM(I)
          TAG2(I)=XIDTE2(I)
  310   CONTINUE
        N=ICNT
      ENDIF
!
      AN=N
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
!
      NUMDIG=0
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
!
      NCX1=1
      DO 906 I=12,1,-1
        IF(IX1LAB(I:I).NE.' ')THEN
          NCX1=I
          GO TO 908
        ENDIF
  906 CONTINUE
  908 CONTINUE
!
      NCX2=1
      DO 916 I=12,1,-1
        IF(IX2LAB(I:I).NE.' ')THEN
          NCX2=I
          GO TO 918
        ENDIF
  916 CONTINUE
  918 CONTINUE
!
!
!               ****************************************************
!               **  STEP 11--                                     **
!               **  COMPUTE THE SPECIFIED STATISTIC               **
!               **  FOR EACH CROSS-TAB CATEGORY OF THE DATA, AND  **
!               **  THEN FOR THE FULL DATA SET                    **
!               ****************************************************
!
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWP2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     FOR "ROW" CASE, PLOT Y(ij) - R(i) VERSUS R(i) FOR ALL j.
!
!     THAT IS, WE PLOT DEVIATION FROM COLUMN AVERAGE VERSUS THE COLUMN AVERAGE
!
      IF(ICASP2.EQ.'ROW')THEN
!
        WRITE(IOUNI1,1001)
 1001   FORMAT(5X,'ROW',11X,'HEIGHT',10X,'SLOPE',10X,'RESSD',4X,   &
               'SD OF SLOPE',4X,'CORRELATION')
        WRITE(IOUNI2,1002)
 1002   FORMAT(2X,'COLUMN',6X,'COLUMN MEAN')
        WRITE(IOUNI3,1003)
 1003   FORMAT(2X,'ROW-ID',2X,'COL-ID',10X,'Y(ij)',7X,'PRED(ij)',8X,   &
               'RES(ij)',1X,'STAND. RES(ij)')
!
        ITITL9=' '
        NCTIT9=0
        ITITLE='Parameters of Row-Linear Fit for '
        WRITE(ITITLE(34:58),'(A25)')IYLAB
        NCTITL=58
!
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
!
        NMAX=0
        NUMROW=NUMSE1
        IF(NUMROW.GT.MAXROW)NUMROW=NUMSE1
        DO 1032 II=1,NUMCOL
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
        DO 1033 II=1,MAXROW
        DO 1035 JJ=1,NUMCOL
          NCVALU(II,JJ)=0
          IVALUE(II,JJ)=' '
          NCVALU(II,JJ)=0
          AMAT(II,JJ)=0.0
 1035   CONTINUE
 1033   CONTINUE
!
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
!
!       COMPUTE THE COLUMN AVERAGES
!
        DO 1000 ISET2=1,NUMSE2
!
          K=0
          DO 1005 I=1,N
            IF(TAG2(I).EQ.XIDTE2(ISET2))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 1005     CONTINUE
!
          CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
          COLAVE(ISET2)=XMEAN
!
          IF(ITWOLA.EQ.'VALU')THEN
            WRITE(IOUNI2,1008)XIDTE2(ISET2),COLAVE(ISET2)
 1008       FORMAT(2E15.7)
          ELSE
            WRITE(IOUNI2,1007)ISET2,COLAVE(ISET2)
 1007       FORMAT(I8,2X,E15.7)
          ENDIF
!
 1000   CONTINUE
        CALL MINIM(COLAVE,NUMSE2,IWRITE,XMIN,IBUGG3,IERROR)
        CALL MAXIM(COLAVE,NUMSE2,IWRITE,XMAX,IBUGG3,IERROR)
        CALL MEAN(COLAVE,NUMSE2,IWRITE,XGRAND,IBUGG3,IERROR)
!
!       NOW COMPUTE DEVIATIONS FROM COLUMN AVERAGES FOR EACH ROW
!
        J=0
        ATAG=0.0
        DSUM=0.0D0
        ICNTRW=0
        DO 1100 ISET1=1,NUMSE1
!
          K=0
          DO 1101 I=1,N
            IF(TAG1(I).EQ.XIDTEM(ISET1))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 1101     CONTINUE
!
!         NOW COMPUTE ROW AVERAGE
!
          CALL MEAN(TEMP1,K,IWRITE,AVAL,IBUGG3,IERROR)
          ROWAVE(ISET1)=AVAL
!
          ATAG=ATAG+1.0
          IF(ITWOYA.EQ.'RAW')THEN
            DO 1103 I=1,K
              J=J+1
              X2(J)=COLAVE(I)
              TEMP2(I)=TEMP1(I)
              Y2(J)=TEMP1(I)
              D2(J)=ATAG
 1103       CONTINUE
          ELSE
            DO 1105 I=1,K
              J=J+1
              X2(J)=COLAVE(I)
              TEMP2(I)=TEMP1(I) - COLAVE(I)
              Y2(J)=TEMP2(I)
              D2(J)=ATAG
 1105       CONTINUE
          ENDIF
          CALL LINFIT(TEMP2,COLAVE,K,   &
                      PPA0,PPA1,XRESSD,XRESDF,PPCC,SDPPA0,SDPPA1,CCALBE,   &
                      ISUBRO,IBUGG3,IERROR)
          ATAG=ATAG+1.0
          AY1=PPA0 + PPA1*XMIN
          AY2=PPA0 + PPA1*XMAX
          SLOPES(ISET1)=PPA1
!
          DO 1108 II=1,K
            PREDVA=PPA0 + PPA1*COLAVE(II)
            RESVA=TEMP2(II) - PREDVA
            RESVA2=RESVA/XRESSD
            IF(ITWOLA.EQ.'VALU')THEN
              WRITE(IOUNI3,1119)XIDTEM(ISET1),II,TEMP2(II),PREDVA,   &
                                RESVA,RESVA2
 1119         FORMAT(E15.7,I8,4E15.7)
            ELSE
              WRITE(IOUNI3,1109)ISET1,II,TEMP2(II),PREDVA,   &
                                RESVA,RESVA2
 1109         FORMAT(2I8,4E15.7)
            ENDIF
 1108     CONTINUE
!
          CALL MEAN(TEMP2,K,IWRITE,AVAL,IBUGG3,IERROR)
          ICNTRW=ICNTRW+1
          IF(ISET1.GT.MAXROW)THEN
            IF(ITWOFI.EQ.'ON')THEN
              CALL DPDTA4(ITITL9,NCTIT9,   &
                          ITITLE,NCTITL,ITITL2,NCTIT2,   &
                          MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                          IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,   &
                          IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                          ICAPSW,ICAPTY,IFRST,ILAST,   &
                          ISUBRO,IBUGG3,IERROR)
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
!
          IF(ITWOLA.EQ.'VALU')THEN
            WRITE(IOUNI1,1106)XIDTEM(ISET1),AVAL,PPA1,XRESSD,SDPPA1,PPCC
 1106       FORMAT(6E15.7)
          ELSE
            WRITE(IOUNI1,1107)ISET1,AVAL,PPA1,XRESSD,SDPPA1,PPCC
 1107       FORMAT(I8,2X,5E15.7)
          ENDIF
!
          J=J+1
          X2(J)=XMIN
          Y2(J)=AY1
          D2(J)=ATAG
          J=J+1
          X2(J)=XMAX
          Y2(J)=AY2
          D2(J)=ATAG
 1100   CONTINUE
!
        IF(ITWOFI.EQ.'ON')THEN
          CALL DPDTA4(ITITL9,NCTIT9,   &
                      ITITLE,NCTITL,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          CALL SD(SLOPES,NUMSE1,IWRITE,SDSLOP,IBUGG3,IERROR)
!
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
          CALL DPDTXT(ITTEMP,NCTEMP,SDSLOP,NUMDIG,NTOTAL,   &
                      NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                      ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
!
          DTERM1=DSUM/DBLE(NUMSE1-1)
          AVAL=REAL(DSQRT(DTERM1))
          IFLAGA=.FALSE.
          IFLAGB=.TRUE.
          NBLNK2=2
          ITTEMP='Pooled Standard Deviation of Fit: '
          NCTEMP=34
          CALL DPDTXT(ITTEMP,NCTEMP,AVAL,NUMDIG,NTOTAL,   &
                      NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                      ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
          IRTFMD=IRTFMZ
        ENDIF
!
        ITITL9=' '
        NCTIT9=0
        ITITLE='Column Averages'
        NCTITL=15
!
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
!
        NMAX=0
        NUMROW=NUMSE2
        DO 1042 II=1,NUMCOL
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
        DO 1043 II=1,MAXROW
        DO 1045 JJ=1,NUMCOL
          NCVALU(II,JJ)=0
          IVALUE(II,JJ)=' '
          NCVALU(II,JJ)=0
          AMAT(II,JJ)=0.0
 1045   CONTINUE
 1043   CONTINUE
!
        IWHTML(1)=125
        IWHTML(2)=150
        IWRTF(1)=1300
        IWRTF(2)=IWRTF(1)+1700
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        DO 1051 ISET2=1,NUMSE2
          IF(ITWOLA.EQ.'VALU')THEN
            AMAT(ISET2,1)=XIDTE2(ISET2)
          ELSE
            AMAT(ISET2,1)=REAL(ISET2)
          ENDIF
          AMAT(ISET2,2)=COLAVE(ISET2)
 1051   CONTINUE
!
        IF(ITWOAV.EQ.'ON')THEN
          CALL DPDTA4(ITITL9,NCTIT9,   &
                      ITITLE,NCTITL,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      ISUBRO,IBUGG3,IERROR)
!
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
          CALL DPDTXT(ITTEMP,NCTEMP,GMEAN,NUMDIG,NTOTAL,   &
                      NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                      ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
          IRTFMD=IRTFMZ
        ENDIF
!
!       IF REQUESTED, GENERATE AN EXTENDED ANOVA TABLE
!
        IF(ITWOAN.EQ.'ON')THEN
!
!         COMPUTE SSTO
!
          DSUM1=0.0D0
          DTERM1=DBLE(NUMSE1)*DBLE(NUMSE2)*DBLE(XGRAND)**2
!
          DO 1610 II=1,N
            DSUM1=DSUM1 + DBLE(Y(II))**2
 1610     CONTINUE
          DSSTO=DSUM1 -DTERM1
!
!         COMPUTE SS(ROWS)
!
          DSUM1=0.0D0
          DO 1620 II=1,NUMSE1
            DSUM1=DSUM1 + DBLE(ROWAVE(II))**2
 1620     CONTINUE
          DSSROW=DBLE(NUMSE2)*DSUM1 - DTERM1
!
!         COMPUTE SS(COLS)
!
          DSUM1=0.0D0
          DO 1630 II=1,NUMSE2
            DSUM1=DSUM1 + DBLE(COLAVE(II))**2
 1630     CONTINUE
          DSSCOL=DBLE(NUMSE1)*DSUM1 - DTERM1
!
!         COMPUTE ERROR SUM OF SQUARES
!
          DSSERR=DSSTO - DSSROW - DSSCOL
!
          DSUM1=0.0D0
          DO 1640 II=1,NUMSE1
            DTERM1=DBLE(SLOPES(II) - 1.0)**2
            DSUM1=DSUM1 + DTERM1
 1640     CONTINUE
!
          DSUM2=0.0D0
          DO 1650 II=1,NUMSE2
            DTERM2=DBLE((COLAVE(II) - XGRAND)**2)
            DSUM2=DSUM2 + DTERM2
 1650     CONTINUE
          DSSSL=DSUM1*DSUM2
          DSSER2=DSSERR-DSSSL
!
!         COMPUTE SUM OF SQUARES FOR COHERENCE.  FORMULAS FROM
!         MANDEL's 1961 JASA PAPER.  THESE FURTHER DECOMPOSE
!         "SLOPE" SUM OF SQUARES INTO "CONCURRENCE" AND
!         "NON-CONCURRENCE".
!
          DSUM1=0.0D0
          DSUM2=0.0D0
          DSUM3=0.0D0
          DO 1660 II=1,NUMSE1
            DTERM1=DBLE(ROWAVE(II)-XGRAND)
            DSUM1=DSUM1 + DTERM1*DBLE(SLOPES(II))
            DSUM2=DSUM2 + DTERM1**2
 1660     CONTINUE
          DO 1670 JJ=1,NUMSE2
            DTERM1=DBLE(COLAVE(JJ)-XGRAND)
            DSUM3=DSUM3 + DTERM1**2
 1670     CONTINUE
          DSSRGR=(DSUM1**2/DSUM2)*DSUM3
          DSSNCN=DSSSL - DSSRGR
!
          ITITL9=' '
          NCTIT9=0
          ITITLE='ANOVA Table for Row-Linear Fit'
          NCTITL=30
!
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
!
          NMAX=0
          NUMROW=8
          IWRTF(1)=1900
          DO 1742 II=1,NUMCOL
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
          DO 1743 II=1,MAXROW
          DO 1745 JJ=1,NUMCOL
            NCVALU(II,JJ)=0
            IVALUE(II,JJ)=' '
            NCVALU(II,JJ)=0
            AMAT(II,JJ)=0.0
 1745     CONTINUE
 1743     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
!
!         LABEL
!
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
!
!         DEGREES OF FREEDOM
!
          AMAT(1,2)=REAL(NUMSE1-1) + REAL(NUMSE2-1) +   &
                    REAL((NUMSE1-1)*(NUMSE2-1))
          AMAT(2,2)=REAL(NUMSE1-1)
          AMAT(3,2)=REAL(NUMSE2-1)
          AMAT(4,2)=REAL((NUMSE1-1)*(NUMSE2-1))
          AMAT(5,2)=REAL((NUMSE1-1)*(NUMSE2-1)) - REAL(NUMSE1-1)
          AMAT(6,2)=REAL(NUMSE1-1)
          AMAT(7,2)=1.0
          AMAT(8,2)=REAL(NUMSE1-2)
!
!         SUM OF SQUARES
!
          AMAT(1,3)=REAL(DSSTO)
          AMAT(2,3)=REAL(DSSROW)
          AMAT(3,3)=REAL(DSSCOL)
          AMAT(4,3)=REAL(DSSERR)
          AMAT(5,3)=REAL(DSSER2)
          AMAT(6,3)=REAL(DSSSL)
          AMAT(7,3)=REAL(DSSRGR)
          AMAT(8,3)=REAL(DSSNCN)
!
!         MEAN SQUARE
!
          AMAT(1,4)=AMAT(1,3)/AMAT(1,2)
          AMAT(2,4)=AMAT(2,3)/AMAT(2,2)
          AMAT(3,4)=AMAT(3,3)/AMAT(3,2)
          AMAT(4,4)=AMAT(4,3)/AMAT(4,2)
          AMAT(5,4)=AMAT(5,3)/AMAT(5,2)
          AMAT(6,4)=AMAT(6,3)/AMAT(6,2)
          AMAT(7,4)=AMAT(7,3)/AMAT(7,2)
          AMAT(8,4)=AMAT(8,3)/AMAT(8,2)
!
          CALL DPDTA4(ITITL9,NCTIT9,   &
                      ITITLE,NCTITL,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      ISUBRO,IBUGG3,IERROR)
!
        ENDIF
!
!     FOR "COLUMN" CASE, PLOT Y(ij) - R(i) VERSUS R(i) FOR ALL j.
!
!     THAT IS, WE PLOT DEVIATION FROM ROW AVERAGE VERSUS THE ROW AVERAGE
!
      ELSE
!
        WRITE(IOUNI1,2001)
 2001   FORMAT(2X,'COLUMN',11X,'HEIGHT',10X,'SLOPE',10X,'RESSD',4X,   &
               'SD OF SLOPE',4X,'CORRELATION')
        WRITE(IOUNI2,2002)
 2002   FORMAT(5X,'ROW',9X,'ROW MEAN')
        WRITE(IOUNI3,2003)
 2003   FORMAT(2X,'ROW-ID',2X,'COL-ID',10X,'Y(ij)',7X,'PRED(ij)',8X,   &
               'RES(ij)',1X,'STAND. RES(ij)')
!
        ITITL9=' '
        NCTIT9=0
        ITITLE='Parameters of Column-Linear Fit for '
        WRITE(ITITLE(37:61),'(A25)')IYLAB
        NCTITL=61
!
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
!
        NMAX=0
        NUMROW=NUMSE2
        IF(NUMROW.GT.MAXROW)NUMROW=NUMSE2
        DO 2032 II=1,NUMCOL
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
        DO 2033 II=1,MAXROW
        DO 2035 JJ=1,NUMCOL
          NCVALU(II,JJ)=0
          IVALUE(II,JJ)=' '
          NCVALU(II,JJ)=0
          AMAT(II,JJ)=0.0
 2035   CONTINUE
 2033   CONTINUE
!
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
!
!       COMPUTE THE ROW AVERAGES
!
        DO 2000 ISET1=1,NUMSE1
!
          K=0
          DO 2005 I=1,N
            IF(TAG1(I).EQ.XIDTEM(ISET1))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 2005     CONTINUE
!
          CALL MEAN(TEMP1,K,IWRITE,XMEAN,IBUGG3,IERROR)
          COLAVE(ISET1)=XMEAN
!
          IF(ITWOLA.EQ.'VALU')THEN
            WRITE(IOUNI2,2008)XIDTEM(ISET1),COLAVE(ISET1)
 2008       FORMAT(2E15.7)
          ELSE
            WRITE(IOUNI2,2007)ISET1,COLAVE(ISET1)
 2007       FORMAT(I8,2X,E15.7)
        ENDIF
!
 2000   CONTINUE
        CALL MINIM(COLAVE,NUMSE1,IWRITE,XMIN,IBUGG3,IERROR)
        CALL MAXIM(COLAVE,NUMSE1,IWRITE,XMAX,IBUGG3,IERROR)
!
!       NOW COMPUTE DEVIATIONS FROM ROW AVERAGES FOR EACH COLUMN
!
        J=0
        ATAG1=0.0
        DSUM=0.0D0
        ICNTRW=0
        DO 2100 ISET2=1,NUMSE2
!
          K=0
          DO 2101 I=1,N
            IF(TAG2(I).EQ.XIDTE2(ISET2))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 2101     CONTINUE
!
!         NOW COMPUTE COLUMN AVERAGE
!
          CALL MEAN(TEMP1,K,IWRITE,AVAL,IBUGG3,IERROR)
          ROWAVE(ISET2)=AVAL
!
          ATAG=ATAG+1.0
          IF(ITWOYA.EQ.'RAW')THEN
            DO 2103 I=1,K
              J=J+1
              X2(J)=COLAVE(I)
              TEMP2(I)=TEMP1(I)
              Y2(J)=TEMP1(I)
              D2(J)=ATAG
 2103       CONTINUE
          ELSE
            DO 2105 I=1,K
              J=J+1
              X2(J)=COLAVE(I)
              TEMP2(I)=TEMP1(I) - COLAVE(I)
              Y2(J)=TEMP2(I)
              D2(J)=ATAG
 2105       CONTINUE
          ENDIF
          CALL LINFIT(TEMP2,COLAVE,K,   &
                      PPA0,PPA1,XRESSD,XRESDF,PPCC,SDPPA0,SDPPA1,CCALBE,   &
                      ISUBRO,IBUGG3,IERROR)
          ATAG=ATAG+1.0
          AY1=PPA0 + PPA1*XMIN
          AY2=PPA0 + PPA1*XMAX
          SLOPES(ISET2)=PPA1
!
          DO 2108 II=1,K
            PREDVA=PPA0 + PPA1*COLAVE(II)
            RESVA=TEMP2(II) - PREDVA
            RESVA2=RESVA2/XRESSD
            IF(ITWOLA.EQ.'VALU')THEN
              WRITE(IOUNI3,2119)XIDTE2(ISET2),II,TEMP2(II),PREDVA,   &
                                RESVA,RESVA2
 2119         FORMAT(E15.7,I8,4E15.7)
            ELSE
              WRITE(IOUNI3,2109)ISET2,II,TEMP2(II),PREDVA,   &
                                RESVA,RESVA2
 2109         FORMAT(2I8,4E15.7)
            ENDIF
 2108     CONTINUE
!
          CALL MEAN(TEMP2,K,IWRITE,AVAL,IBUGG3,IERROR)
          ICNTRW=ICNTRW+1
          IF(ISET2.GT.MAXROW)THEN
            IF(ITWOFI.EQ.'ON')THEN
              CALL DPDTA4(ITITL9,NCTIT9,   &
                          ITITLE,NCTITL,ITITL2,NCTIT2,   &
                          MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                          IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,   &
                          IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                          ICAPSW,ICAPTY,IFRST,ILAST,   &
                          ISUBRO,IBUGG3,IERROR)
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
!
          IF(ITWOLA.EQ.'VALU')THEN
            WRITE(IOUNI1,2106)XIDTE2(ISET2),AVAL,PPA1,XRESSD,SDPPA1,PPCC
 2106       FORMAT(6E15.7)
          ELSE
            WRITE(IOUNI1,2107)ISET2,AVAL,PPA1,XRESSD,SDPPA1,PPCC
 2107       FORMAT(I8,2X,5E15.7)
          ENDIF
!
          J=J+1
          X2(J)=XMIN
          Y2(J)=AY1
          D2(J)=ATAG
          J=J+1
          X2(J)=XMAX
          Y2(J)=AY2
          D2(J)=ATAG
 2100   CONTINUE
!
        IF(ITWOFI.EQ.'ON')THEN
          CALL DPDTA4(ITITL9,NCTIT9,   &
                      ITITLE,NCTITL,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      ISUBRO,IBUGG3,IERROR)
!
          CALL SD(SLOPES,NUMSE2,IWRITE,SDSLOP,IBUGG3,IERROR)
!
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
          CALL DPDTXT(ITTEMP,NCTEMP,SDSLOP,NUMDIG,NTOTAL,   &
                    NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                    ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
!
          DTERM1=DSUM/DBLE(NUMSE2-1)
          AVAL=REAL(DSQRT(DTERM1))
          IFLAGA=.FALSE.
          IFLAGB=.TRUE.
          NBLNK1=0
          NBLNK2=2
          ITYPE=2
          ITTEMP='Pooled Standard Deviation of Fit: '
          NCTEMP=34
          CALL DPDTXT(ITTEMP,NCTEMP,AVAL,NUMDIG,NTOTAL,   &
                      NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                      ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
          IRTFMD=IRTFMZ
        ENDIF
!
        ITITL9=' '
        NCTIT9=0
        ITITLE='Row Averages'
        NCTITL=12
!
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
!
        NMAX=0
        NUMROW=NUMSE1
        DO 2042 II=1,NUMCOL
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
        DO 2043 II=1,MAXROW
        DO 2045 JJ=1,NUMCOL
          NCVALU(II,JJ)=0
          IVALUE(II,JJ)=' '
          NCVALU(II,JJ)=0
          AMAT(II,JJ)=0.0
 2045   CONTINUE
 2043   CONTINUE
!
        IWHTML(1)=125
        IWHTML(2)=150
        IWRTF(1)=1300
        IWRTF(2)=IWRTF(1)+1700
        IFRST=.TRUE.
        ILAST=.TRUE.
!
        DO 2051 ISET1=1,NUMSE1
          IF(ITWOLA.EQ.'VALU')THEN
            AMAT(ISET1,1)=XIDTEM(ISET1)
          ELSE
            AMAT(ISET1,1)=REAL(ISET1)
          ENDIF
          AMAT(ISET1,2)=COLAVE(ISET1)
 2051   CONTINUE
!
        IF(ITWOAV.EQ.'ON')THEN
          CALL DPDTA4(ITITL9,NCTIT9,   &
                      ITITLE,NCTITL,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      ISUBRO,IBUGG3,IERROR)
!
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
          CALL DPDTXT(ITTEMP,NCTEMP,GMEAN,NUMDIG,NTOTAL,   &
                      NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                      ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGG3,IERROR)
          IRTFMD=IRTFMZ
        ENDIF
!
!       IF REQUESTED, GENERATE AN EXTENDED ANOVA TABLE
!
        IF(ITWOAN.EQ.'ON')THEN
!
!         COMPUTE SSTO
!
          DSUM1=0.0D0
          DTERM1=DBLE(NUMSE1)*DBLE(NUMSE2)*DBLE(XGRAND)**2
!
          DO 2610 II=1,N
            DSUM1=DSUM1 + DBLE(Y(II))**2
 2610     CONTINUE
          DSSTO=DSUM1 -DTERM1
!
!         COMPUTE SS(ROWS)
!
          DSUM1=0.0D0
          DO 2620 II=1,NUMSE1
            DSUM1=DSUM1 + DBLE(ROWAVE(II))**2
 2620     CONTINUE
          DSSROW=DBLE(NUMSE2)*DSUM1 - DTERM1
!
!         COMPUTE SS(COLS)
!
          DSUM1=0.0D0
          DO 2630 II=1,NUMSE2
            DSUM1=DSUM1 + DBLE(COLAVE(II))**2
 2630     CONTINUE
          DSSCOL=DBLE(NUMSE1)*DSUM1 - DTERM1
!
          DSSERR=DSSTO - DSSROW - DSSCOL
!
          DSUM1=0.0D0
          DO 2640 II=1,NUMSE2
            DTERM1=DBLE(SLOPES(II) - 1.0)**2
            DSUM1=DSUM1 + DTERM1
 2640     CONTINUE
!
          DSUM2=0.0D0
          DO 2650 II=1,NUMSE1
            DTERM2=DBLE((COLAVE(II) - XGRAND)**2)
            DSUM2=DSUM2 + DTERM2
 2650     CONTINUE
          DSSSL=DSUM1*DSUM2
          DSSER2=DSSERR-DSSSL
!
!         COMPUTE SUM OF SQUARES FOR COHERENCE.  FORMULAS FROM
!         MANDEL's 1961 JASA PAPER.  THESE FURTHER DECOMPOSE
!         "SLOPE" SUM OF SQUARES INTO "CONCURRENCE" AND
!         "NON-CONCURRENCE".
!
          DSUM1=0.0D0
          DSUM2=0.0D0
          DSUM3=0.0D0
          DO 2660 II=1,NUMSE1
            DTERM1=DBLE(ROWAVE(II)-XGRAND)
            DSUM1=DSUM1 + DTERM1*DBLE(SLOPES(II))
            DSUM2=DSUM2 + DTERM1**2
 2660     CONTINUE
          DO 2670 JJ=1,NUMSE2
            DTERM1=DBLE(COLAVE(JJ)-XGRAND)
            DSUM3=DSUM3 + DTERM1**2
 2670     CONTINUE
          DSSRGR=(DSUM1**2/DSUM2)*DSUM3
          DSSNCN=DSSSL - DSSRGR
!
          ITITL9=' '
          NCTIT9=0
          ITITLE='ANOVA Table for Column-Linear Fit'
          NCTITL=33
!
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
!
          NMAX=0
          NUMROW=8
          IWRTF(1)=1900
          DO 2742 II=1,NUMCOL
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
          DO 2743 II=1,MAXROW
          DO 2745 JJ=1,NUMCOL
            NCVALU(II,JJ)=0
            IVALUE(II,JJ)=' '
            NCVALU(II,JJ)=0
            AMAT(II,JJ)=0.0
 2745     CONTINUE
 2743     CONTINUE
!
          IFRST=.TRUE.
          ILAST=.TRUE.
!
!         LABEL
!
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
!
!         DEGREES OF FREEDOM
!
          AMAT(1,2)=REAL(NUMSE1-1) + REAL(NUMSE2-1) +   &
                    REAL((NUMSE1-1)*(NUMSE2-1))
          AMAT(2,2)=REAL(NUMSE1-1)
          AMAT(3,2)=REAL(NUMSE2-1)
          AMAT(4,2)=REAL((NUMSE1-1)*(NUMSE2-1))
          AMAT(5,2)=REAL((NUMSE1-1)*(NUMSE2-1)) - REAL(NUMSE1-1)
          AMAT(6,2)=REAL(NUMSE1-1)
          AMAT(7,2)=1.0
          AMAT(8,2)=REAL(NUMSE1-2)
!
!         SUM OF SQUARES
!
          AMAT(1,3)=REAL(DSSTO)
          AMAT(2,3)=REAL(DSSROW)
          AMAT(3,3)=REAL(DSSCOL)
          AMAT(4,3)=REAL(DSSERR)
          AMAT(5,3)=REAL(DSSER2)
          AMAT(6,3)=REAL(DSSSL)
          AMAT(7,3)=REAL(DSSRGR)
          AMAT(8,3)=REAL(DSSNCN)
!
!         MEAN SQUARE
!
          AMAT(1,4)=AMAT(1,3)/AMAT(1,2)
          AMAT(2,4)=AMAT(2,3)/AMAT(2,2)
          AMAT(3,4)=AMAT(3,3)/AMAT(3,2)
          AMAT(4,4)=AMAT(4,3)/AMAT(4,2)
          AMAT(5,4)=AMAT(5,3)/AMAT(5,2)
          AMAT(6,4)=AMAT(6,3)/AMAT(6,2)
          AMAT(7,4)=AMAT(7,3)/AMAT(7,2)
          AMAT(8,4)=AMAT(8,3)/AMAT(8,2)
!
          CALL DPDTA4(ITITL9,NCTIT9,   &
                      ITITLE,NCTITL,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      ISUBRO,IBUGG3,IERROR)
!
        ENDIF
!
      ENDIF
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
!
      IRTFPS=IRTFSV
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'RTF')THEN
        WRITE(ICOUT,103)IBASLC,IRTFPS
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLA41,IFLG51,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TWP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTWP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NUMSE1,NUMSE2,N2
 9013   FORMAT('IERROR,NUMSE1,NUMSE2,N2 = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTWP2
      SUBROUTINE DPTYP2(IANS,IWIDTH,IHNAME,IHNAM2,NUMNAM,MAXNAM,IBUGA3,   &
                        IUSE,IVALUE,VALUE,IN,   &
                        IFOUNZ,IBEGIN,IEND,   &
                        ITYPE,IHOL,IHOL2,INT1,FLOAT1,IERRO1,   &
                        NUMCL,NUMPL,NUMAOL,ITYW1L,ICAT1L,INLI1L,ITYW2L,   &
                        NUMCR,NUMPR,NUMAOR,ITYW1R,ICAT1R,INLI1R,ITYW2R)
!
!     PURPOSE--SCAN THE CHARACTER ARRAY IANS(.) AND EXTRACT INFORMATION
!              REGARDING THE EXISTENCE AND LOCATION OF CERTAIN SUBSTRINGS
!              USED IN THE LET COMMAND.  THIS SUBROUTINE (DPTYP2) IS
!              CALLED BY DPLET.
!     OTHER SUBROUINTES NEEDED--DPTYP3
!     MOST GENERAL FORM--LET X(I) = XXX FOR I = A B C
!                      --LET X(I) = XXX SUBSET XX A B
!     INPUT  ARGUMENTS--IANS   = A HOLLERITH 1-CHARACTER-PER-WORD
!                                VARIABLE CONTAINING THE INPUT LINE
!                                TO BE EXAMINED.
!                     --IWIDTH = AN INTEGER VARIABLE CONTAINING
!                                THE (FULL) WIDTH OF THE INPUT LINE
!                                (THAT IS, THE NUMBER OF COLUMNS)
!     OUTPUT ARGUMENTS--IFOUNZ = A HOLLERITH ARRAY WITH THE VALUE 'YES'
!                                IF THE SUBSTRING WAS FOUND; AND THE
!                                VALUE 'NO' IF THE SUBSTRING WAS NOT
!                                FOUND.
!                     --IBEGIN = AN INTEGER ARRAY WITH
!                                THE START COLUMN OF THE FOUND SUBSTRING
!                     --IEND   = AN INTEGER ARRAY WITH
!                                THE STOP COLUMN OF THE FOUND SUBSTRING.
!                     --ITYPE  = A HOLLERITH ARRAY WITH THE VALUE 'WORD'
!                                IF THE SUBSTRING CONTAINS ANY NON-NUMERIC
!                                (EXCLUDING BLANKS) CHARACTER; AND WITH
!                                THE VALUE 'NUMB' IF THE SUBSTRING
!                                CONTAINS ALL NUMERIC VALUES OR DECIMAL
!                                POINT OR + OR - (WITH INTERMITTENT
!                                BLANKS IGNORED).
!                     --IHOL   = AN HOLLERITH ARRAY CONTAINING THE PACKED
!                                (FIRST 4 CHARACTERS) OF THE FOUND
!                                SUBSTRING.
!                     --IHOL2  = AN HOLLERITH ARRAY CONTAINING THE PACKED
!                                (NEXT 4 CHARACTERS) OF THE FOUND
!                                SUBSTRING.
!                     --INT1   = AN INTEGER ARRAY
!                                CONTAINING THE INTEGER REPRESENTATION
!                                (IF POSSIBLE) OF THE FOUND SUBSTRING.
!                     --FLOAT1 = AN FLOATING POINT ARRAY
!                                CONTAINING THE FLOATING POINT REPRESENTATION
!                                (IF POSSIBLE) OF THE FOUND SUBSTRING.
!                     --IERRO1 = AN HOLLERITH ARRAY
!                                WITH THE VALUE 'NO' IF
!                                NO ERROR HAS BEEN ENCOUNTERED,
!                                AND THE VALUE 'YES' IF AN
!                                ERROR HAS BEEN ENCOUNTERED.
!                     --NUMCL  = AN INTEGER VARIABLE CONTAINING THE
!                                NUMBER OF COMPONENTS
!                                ON THE LEFT SIDE
!                                (NOT COUNTING LET OR THE = SIGN).
!                     --NUMPL  = AN INTEGER VARIABLE CONTAINING THE
!                                NUMBER OF PARENTHESES (LEFT + RIGHT)
!                                ON THE LEFT SIDE
!                                (NOT COUNTING LET OR THE = SIGN).
!                     --NUMAOL = AN INTEGER VARIABLE CONTAINING THE
!                                NUMBER OF ARITHMETIC OPERATIONS
!                                ON THE LEFT SIDE
!                                (NOT COUNTING LET OR THE = SIGN).
!                     --ITYW1L = A HOLLERITH VARIABLE CONTAINING THE
!                                TYPE ('WORD' VERSUS 'NUMB')
!                                FOR THE FIRST WORD
!                                (THAT IS, THE VARIABLE
!                                OR PARAMETER NAME)
!                                ON THE LEFT SIDE
!                                (NOT COUNTING LET OR THE = SIGN).
!                     --ITYW2L = A HOLLERITH VARIABLE CONTAINING THE
!                                TYPE ('WORD' VERSUS 'NUMB')
!                                FOR THE SECOND WORD
!                                (THAT IS, THE ARGUMENT)
!                                ON THE LEFT SIDE
!                                (NOT COUNTING LET OR THE = SIGN).
!                     --INLI1L = A HOLLERITH VARIABLE CONTAINING THE
!                                ANSWER ('YES' VERSUS 'NO')
!                                TO THE QUESTION AS TO WHETHER
!                                THE FIRST WORD ON THE LEFT
!                                (THAT IS, THE VARIABLE
!                                OR PARAMETER NAME)
!                                IS ALREADY EXISTENT IN THE
!                                INTERNAL DATAPLOT NAME LIST
!                                (NOT COUNTING LET OR THE = SIGN).
!                     --NUMCR  = AN INTEGER VARIABLE CONTAINING THE
!                                NUMBER OF COMPONENTS
!                                ON THE RIGHT SIDE
!                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
!                     --NUMPR  = AN INTEGER VARIABLE CONTAINING THE
!                                NUMBER OF PARENTHESES (RIGHT + RIGHT)
!                                ON THE RIGHT SIDE
!                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
!                     --NUMAOR = AN INTEGER VARIABLE CONTAINING THE
!                                NUMBER OF ARITHMETIC OPERATIONS
!                                ON THE RIGHT SIDE
!                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
!                     --ITYW1R = A HOLLERITH VARIABLE CONTAINING THE
!                                TYPE ('WORD' VERSUS 'NUMB')
!                                FOR THE FIRST WORD
!                                (THAT IS, THE VARIABLE
!                                OR PARAMETER NAME)
!                                ON THE RIGHT SIDE
!                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
!                     --ITYW2R = A HOLLERITH VARIABLE CONTAINING THE
!                                TYPE ('WORD' VERSUS 'NUMB')
!                                FOR THE SECOND WORD
!                                (THAT IS, THE ARGUMENT)
!                                ON THE RIGHT SIDE
!                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
!                     --INLI1R = A HOLLERITH VARIABLE CONTAINING THE
!                                ANSWER ('YES' VERSUS 'NO')
!                                TO THE QUESTION AS TO WHETHER
!                                THE FIRST WORD ON THE RIGHT
!                                (THAT IS, THE VARIABLE
!                                OR PARAMETER NAME)
!                                IS ALREADY EXISTENT IN THE
!                                INTERNAL DATAPLOT NAME LIST
!                                (NOT COUNTING THE = SIGN OR SUBSET OR FOR).
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
!     ORIGINAL VERSION--MARCH     1978
!     UPDATED         --JUNE      1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --JUNE      1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --JANUARY   1982.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1983.
!     UPDATED         --DECEMBER  1988. ELIM. SPUR. ERROR MESS. FOR IFRINGE
!     UPDATED         --JANAURY   1989. IANS(IENDP) WITH IENDP = 0 (ALAN)
!     UPDATED         --NOVEMBER  1989. FIX IANS(IENDP=0) (NELSON)
!     UPDATED         --AUGUST    2021. WHEN CHECKING FOR "SUBSET",
!                                       "EXCEPT", "FOR" AND "IF" SHOULD
!                                       INCLUDE A LEADING SPACE TO AVOID
!                                       CONFLICT WITH VARIABLE NAMES
!                                       THAT MIGHT CONTAIN THESE
!                                       CHARACTERS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IANS(*)
      CHARACTER*4 IHNAME(*)
      CHARACTER*4 IHNAM2(*)
      CHARACTER*4 IUSE(*)
      CHARACTER*4 IFOUNZ(*)
      CHARACTER*4 ITYPE(*)
      CHARACTER*4 IHOL(*)
      CHARACTER*4 IHOL2(*)
      CHARACTER*4 IERRO1(*)
      CHARACTER*4 IBUGA3
      CHARACTER*4 ITYW1L
      CHARACTER*4 ICAT1L
      CHARACTER*4 INLI1L
      CHARACTER*4 ITYW2L
      CHARACTER*4 ITYW1R
      CHARACTER*4 ICAT1R
      CHARACTER*4 INLI1R
      CHARACTER*4 ITYW2R
!
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IN(*)
      DIMENSION IBEGIN(*)
      DIMENSION IEND(*)
      DIMENSION INT1(*)
      DIMENSION FLOAT1(*)
!
      DIMENSION IHMAN(10)
      DIMENSION IHMAN2(10)
      DIMENSION IHSTAT(25)
      DIMENSION IHSTA2(25)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA NUMMAN/8/
!
      DATA IHMAN(1),IHMAN2(1)/'SORT','    '/
      DATA IHMAN(2),IHMAN2(2)/'RANK','    '/
      DATA IHMAN(3),IHMAN2(3)/'CODE','    '/
      DATA IHMAN(4),IHMAN2(4)/'DIST','INCT'/
      DATA IHMAN(5),IHMAN2(5)/'SEQU','ENTI'/
      DATA IHMAN(6),IHMAN2(6)/'CUMU','LATI'/
      DATA IHMAN(7),IHMAN2(7)/'CUMU','LATI'/
      DATA IHMAN(8),IHMAN2(8)/'CUMU','LATI'/
!
      DATA NUMSTA/22/
!
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
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTY'
      ISUBN2='P2  '
      IERROR='NO'
      IQUAL='UNKN'
!
      IMAXR=0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTYP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWIDTH,MAXNAM,IN(1)
   52   FORMAT('IBUGA3,IWIDTH,MAXNAM,IN(1) = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANS(I),I=1,MIN(80,IWIDTH))
   54   FORMAT('IANS(.) = ',80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  INITIALIZE THE OUTPUT PARAMETERS AND VARIABLES  **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 100 I=1,30
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
!
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
!
!               ****************************************************************
!               **  STEP 2--
!               **  EXAMINE THE LEFT-HAND SIDE OF EXPRESSION.
!               **  DETERMINE IF PARAMETER OR VARIABLE NAME TO LEFT OF = SIGN
!               **  HAS PARENTHESES.
!               **  IF IT HAS PARENTHESES, THIS MEANS THAT WE WILL BE
!               **  DEFINING    PART     OF A VARIABLE.
!               **  COMPONENT 1  = LET
!               **  COMPONENT 2  = VARIABLE NAME
!               **  COMPONENT 3  = (                             (IF IT EXISTS)
!               **  COMPONENT 4  = ARGUMENT (I.E., ROW OF TABLE) (IF IT EXISTS)
!               **  COMPONENT 5  = )                             (IF IT EXISTS)
!               **  COMPONENT 6  = =
!               ****************************************************************
!
!     MOST GENERAL FORM--LET X(I) = XXX FOR I = A B C
!                      --LET X(I) = XXX SUBSET XX A B
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     STEP 2.1--SEARCH FOR LET.
!
      ISTAR1=1
      ISTOP1=IWIDTH
      ISTRIN='LET'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(1),IBEGIN(1),IEND(1),   &
            ITYPE(1),IHOL(1),IHOL2(1),INT1(1),FLOAT1(1),IERRO1(1))
      IF(IFOUNZ(1).EQ.'YES')GO TO 2190
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 2190 CONTINUE
!
!     STEP 2.2--SEARCH FOR = SIGN.
!
      ISTEPN='2.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(1)+1
      ISTOP1=IWIDTH
      ISTRIN='='
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(6),IBEGIN(6),IEND(6),   &
            ITYPE(6),IHOL(6),IHOL2(6),INT1(6),FLOAT1(6),IERRO1(6))
      IF(IFOUNZ(6).EQ.'YES')GO TO 2290
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 2290 CONTINUE
!
!     STEP 2.3--SEARCH FOR LEFT-HAND SIDE (;
!     SEARCH BETWEEN LET AND =.
!
      ISTEPN='2.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(1)+1
      ISTOP1=IBEGIN(6)-1
      ISTRIN='('
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(3),IBEGIN(3),IEND(3),   &
            ITYPE(3),IHOL(3),IHOL2(3),INT1(3),FLOAT1(3),IERRO1(3))
      IF(IFOUNZ(3).EQ.'YES')GO TO 2390
      GO TO 2500
 2390 CONTINUE
!
!     STEP 2.4--SEARCH FOR LEFT-HAND SIDE );
!     SEARCH BETWEEN ( AND =.
!
      ISTEPN='2.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(3)+1
      ISTOP1=IBEGIN(6)-1
      ISTRIN=')'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(5),IBEGIN(5),IEND(5),   &
            ITYPE(5),IHOL(5),IHOL2(5),INT1(5),FLOAT1(5),IERRO1(5))
      IF(IFOUNZ(5).EQ.'YES')GO TO 2490
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 2490 CONTINUE
      GO TO 2600
!
!     STEP 2.5--IF NO LEFT-HAND SIDE PARENTHESES FOUND,
!     EXTRACT VARIABLE NAME;
!     SEARCH BETWEEN LET AND =.
!
 2500 CONTINUE
!
      ISTEPN='2.5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(1)+1
      ISTOP1=IBEGIN(6)
      ISTRIN='!;='
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(2),IBEGIN(2),IEND(2),   &
            ITYPE(2),IHOL(2),IHOL2(2),INT1(2),FLOAT1(2),IERRO1(2))
      IF(IFOUNZ(2).EQ.'YES')GO TO 2590
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 2590 CONTINUE
      GO TO 2800
!
!     STEP 2.6--IF LEFT-HAND SIDE PARENTHESES FOUND,
!     FIRST EXTRACT VARIABLE NAME;
!     SEARCH BETWEEN LET AND (.
!
 2600 CONTINUE
!
      ISTEPN='2.6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(1)+1
      ISTOP1=IBEGIN(3)
      ISTRIN='!;('
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(2),IBEGIN(2),IEND(2),   &
            ITYPE(2),IHOL(2),IHOL2(2),INT1(2),FLOAT1(2),IERRO1(2))
      IF(IFOUNZ(2).EQ.'YES')GO TO 2690
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 2690 CONTINUE
!
!     STEP 2.7--ALSO IF LEFT-HAND SIDE PARENTHESES FOUND,
!     SEARCH FOR LEFT-HAND SIDE ARGUMENT NAME OR VALUE;
!     SEARCH BETWEEN ( AND ).
!
      ISTEPN='2.7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(3)
      ISTOP1=IBEGIN(5)
      ISTRIN='(;)'
      ISTRI2='    '
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(4),IBEGIN(4),IEND(4),   &
            ITYPE(4),IHOL(4),IHOL2(4),INT1(4),FLOAT1(4),IERRO1(4))
      IF(IFOUNZ(4).EQ.'YES')GO TO 2790
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 2790 CONTINUE
      K=4
      IF(ITYPE(K).EQ.'WORD')   &
      CALL DPCHEC(K,IHOL,IHOL2,   &
      IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
      INT1,FLOAT1,IBUGA3,IERROR)
!
 2800 CONTINUE
!
!               *******************************************************
!               **  STEP 3--                                         **
!               **  EXAMINE THE RIGHT-HAND SIDE OF EXPRESSION.       **
!               **  DETERMINE WHICH OF THE 3 CASES WE HAVE--         **
!               **      1) LET X(I) =                                **
!               **      2) LET X(I) =       SUBSET XX  A  B          **
!               **      3) LET X(I) =       FOR XX = A  B  C         **
!               **  IF CASE 1 (THE NON-SUBSET AND NON-FOR CASE),     **
!               **  SEARCH FOR COMPONENTS 7, 8, 9, AND 10--          **
!               **  COMPONENT 7  = VARIABLE NAME                     **
!               **  COMPONENT 8  = (                                 **
!               **  COMPONENT 9  = ARGUMENT (THAT IS, ROW OF TABLE)  **
!               **  COMPONENT 10 = )                                 **
!               **  IF CASE 2 (THE SUBSET CASE), JUMP TO STEP 4      **
!               **  IF CASE 3 (THE FOR CASE), JUMP TO STEP 5.        **
!               *******************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     STEP 3.1A--SEARCH FOR SUBSET.
!
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='SUBS'
      ISTRI2='ET  '
      INEX='IIWO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,3001)ISTAR1,ISTOP1
 3001   FORMAT('SEARCH FOR SUBSET: ISTAR1,ISTOP1 = ',2I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(11),IBEGIN(11),IEND(11),   &
            ITYPE(11),IHOL(11),IHOL2(11),INT1(11),FLOAT1(11),IERRO1(11))
!CCCC THE FOLLOWING SECTION WAS ADDED DECEMBER 1988 TO AVOID
!CCCC SPURIOUS ERROR MESSAGES WITH A LONG VARIABLE NAME LIKE SUBSETXX
!CCCC THE SECTION WAS CORRECTED ALSO IN JANUARY 1988 AND NOVEMBER 1989
      IENDP=IEND(11)+1
      IF(IENDP.LE.0)IFOUNZ(11)='NO'
      IF(IENDP.LE.0)GO TO 3119
      IF(IFOUNZ(11).EQ.'YES'.AND.   &
         IENDP.LE.ISTOP1.AND.   &
         IANS(IENDP).NE.' ')IFOUNZ(11)='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,3003)IENDP,IFOUNZ(11)
 3003   FORMAT('AFTER DPTYP3: IENDP,IFOUNZ(11) = ',I6,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IFOUNZ(11).EQ.'YES')GO TO 4000
 3119 CONTINUE
!
!     STEP 3.1B--SEARCH FOR EXCEPT.
!
      ISTEPN='3.1B'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='EXCE'
      ISTRI2='PT  '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(11),IBEGIN(11),IEND(11),   &
            ITYPE(11),IHOL(11),IHOL2(11),INT1(11),FLOAT1(11),IERRO1(11))
!CCCC THE FOLLOWING SECTION WAS ADDED DECEMBER 1988 TO AVOID
!CCCC SPURIOUS ERROR MESSAGES WITH A LONG VARIABLE NAME LIKE EXCEPTXX
!CCCC THE SECTION WAS CORRECTED ALSO IN JANUARY 1988 AND NOVEMBER 1989
      IENDP=IEND(11)+1
      IF(IENDP.LE.0)IFOUNZ(11)='NO'
      IF(IENDP.LE.0)GO TO 3129
      IF(IFOUNZ(11).EQ.'YES'.AND.   &
         IENDP.LE.ISTOP1.AND.   &
         IANS(IENDP).NE.' ')IFOUNZ(11)='NO'
      IF(IFOUNZ(11).EQ.'YES')GO TO 4000
 3129 CONTINUE
!
!     STEP 3.1C--SEARCH FOR FOR.
!
      ISTEPN='3.1C'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='FOR '
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(21),IBEGIN(21),IEND(21),   &
            ITYPE(21),IHOL(21),IHOL2(21),INT1(21),FLOAT1(21),IERRO1(21))
!CCCC THE FOLLOWING SECTION WAS ADDED DECEMBER 1988 TO AVOID
!CCCC SPURIOUS ERROR MESSAGES WITH A LONG VARIABLE NAME LIKE FORTUNE
!CCCC THE SECTION WAS CORRECTED ALSO IN JANUARY 1988 AND NOVEMBER 1989
      IENDP=IEND(21)+1
      IF(IENDP.LE.0)IFOUNZ(21)='NO'
      IF(IENDP.LE.0)GO TO 3139
      IF(IFOUNZ(21).EQ.'YES'.AND.   &
         IENDP.LE.ISTOP1.AND.   &
         IANS(IENDP).NE.' ')IFOUNZ(21)='NO'
      IF(IFOUNZ(21).EQ.'YES')GO TO 5000
 3139 CONTINUE
!
!     STEP 3.1D--SEARCH FOR IF.
!
      ISTEPN='3.1D'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='IF  '
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(11),IBEGIN(11),IEND(11),   &
            ITYPE(11),IHOL(11),IHOL2(11),INT1(11),FLOAT1(11),IERRO1(11))
!CCCC THE FOLLOWING SECTION WAS ADDED DECEMBER 1988 TO AVOID
!CCCC SPURIOUS ERROR MESSAGES WITH A LONG VARIABLE NAME LIKE IFRING
!CCCC THE SECTION WAS CORRECTED ALSO IN JANUARY 1988 AND NOVEMBER 1989
      IENDP=IEND(11)+1
      IF(IENDP.LE.0)IFOUNZ(11)='NO'
      IF(IENDP.LE.0)GO TO 3149
      IF(IFOUNZ(11).EQ.'YES'.AND.   &
         IENDP.LE.ISTOP1.AND.   &
         IANS(IENDP).NE.' ')IFOUNZ(11)='NO'
      IF(IFOUNZ(11).EQ.'YES')GO TO 4000
 3149 CONTINUE
!
!     STEP 3.2--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
!     SEARCH FOR RIGHT-HAND SIDE (;
!     SEARCH BETWEEN = AND END OF LINE.
!
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='('
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(8),IBEGIN(8),IEND(8),   &
            ITYPE(8),IHOL(8),IHOL2(8),INT1(8),FLOAT1(8),IERRO1(8))
      IF(IFOUNZ(8).EQ.'YES')GO TO 3290
      GO TO 3400
 3290 CONTINUE
!
!     STEP 3.3--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
!     SEARCH FOR RIGHT-HAND SIDE );
!     SEARCH BETWEEN ( AND END OF LINE.
!
      ISTEPN='3.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(8)+1
      ISTOP1=IWIDTH
      ISTRIN=')'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(10),IBEGIN(10),IEND(10),   &
            ITYPE(10),IHOL(10),IHOL2(10),INT1(10),FLOAT1(10),IERRO1(10))
      IF(IFOUNZ(10).EQ.'YES')GO TO 3390
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 3390 CONTINUE
      GO TO 3500
!
!     STEP 3.4--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
!     IF NO RIGHT-HAND SIDE PARENTHESES FOUND,
!     EXTRACT VARIABLE NAME OR VALUE;
!     SEARCH BETWEEN = AND END OF LINE.
!     ALSO, TO HANDLE THE COLUMN NAMING CASE
!     (E.G., LET X = COLUMN 1),
!     CHECK TO SEE IF ANOTHER ITEM
!     FOLLOWS THE VARIABLE NAME OR VALUE.
!     AND FURTERMORE, TO HANDLE THE DATA GENERATION CASE
!     (E.G., LET X = 1 1 10),
!     CHECK TO SEE OF 2 ITEMS
!     FOLLOW THE FIRST VALUE.
!
 3400 CONTINUE
!
      ISTEPN='3.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(7),IBEGIN(7),IEND(7),   &
            ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GO TO 3410
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
!
 3410 CONTINUE
!
      ISTEPN='3.41'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(7)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(8),IBEGIN(8),IEND(8),   &
            ITYPE(8),IHOL(8),IHOL2(8),INT1(8),FLOAT1(8),IERRO1(8))
      IF(IFOUNZ(8).EQ.'YES')GO TO 3420
      GO TO 3900
!
 3420 CONTINUE
!
      ISTEPN='3.42'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(8)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(9),IBEGIN(9),IEND(9),   &
            ITYPE(9),IHOL(9),IHOL2(9),INT1(9),FLOAT1(9),IERRO1(9))
      GO TO 3900
!
!     STEP 3.5--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
!     IF RIGHT-HAND SIDE PARENTHESES FOUND,
!     FIRST EXTRACT VARIABLE NAME;
!     SEARCH BETWEEN = AND (.
!
 3500 CONTINUE
!
      ISTEPN='3.5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(8)
      ISTRIN='!;('
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(7),IBEGIN(7),IEND(7),   &
            ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GO TO 3590
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 3590 CONTINUE
!
!     STEP 3.6--IF NEITHER SUBSET NOR FOR HAVE BEEN FOUND,
!     ALSO IF RIGHT-HAND SIDE PARENTHESES FOUND,
!     SEARCH FOR RIGHT-HAND SIDE ARGUMENT NAME OR VALUE;
!     SEARCH BETWEEN ( AND ).
!
      ISTEPN='3.6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(8)
      ISTOP1=IBEGIN(10)
      ISTRIN='(;)'
      ISTRI2='    '
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(9),IBEGIN(9),IEND(9),   &
            ITYPE(9),IHOL(9),IHOL2(9),INT1(9),FLOAT1(9),IERRO1(9))
      IF(IFOUNZ(9).EQ.'YES')GO TO 3690
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 3690 CONTINUE
      K=9
      IF(ITYPE(K).EQ.'WORD')   &
      CALL DPCHEC(K,IHOL,IHOL2,   &
      IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
      INT1,FLOAT1,IBUGA3,IERROR)
!
 3900 CONTINUE
      GO TO 6000
!
!               **********************************************************
!               **  STEP 4--                                            **
!               **  FOR THE CASE WHEN HAVE     LET X(I) =               **
!               **  EXAMINE THE RIGHT-HAND SIDE FOR    SUBSET XX  A  B  **
!               **  COMPONENT 7  = VARIABLE NAME                        **
!               **  COMPONENT 8  = (                                    **
!               **  COMPONENT 9  = ARGUMENT (THAT IS, ROW OF TABLE)     **
!               **  COMPONENT 10 = )                                    **
!               **  COMPONENT 11 = SUBSET                               **
!               **  COMPONENT 12 = LOWER LIMIT             OF SUBSET    **
!               **  COMPONENT 13 = UPPER LIMIT (IF EXISTS) OF SUBSET    **
!               **********************************************************
!
 4000 CONTINUE
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     STEP 4.2--IF SUBSET HAS BEEN FOUND,
!     SEARCH FOR RIGHT-HAND SIDE (;
!     SEARCH BETWEEN = AND SUBSET.
!
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(11)-1
      ISTRIN='('
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(8),IBEGIN(8),IEND(8),   &
            ITYPE(8),IHOL(8),IHOL2(8),INT1(8),FLOAT1(8),IERRO1(8))
      IF(IFOUNZ(8).EQ.'YES')GO TO 4090
      GO TO 4400
 4090 CONTINUE
!
!     STEP 4.3--IF SUBSET HAS BEEN FOUND,
!     SEARCH FOR RIGHT-HAND SIDE );
!     SEARCH BETWEEN ( AND SUBSET.
!
      ISTEPN='4.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(8)+1
      ISTOP1=IBEGIN(11)-1
      ISTRIN=')'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(10),IBEGIN(10),IEND(10),   &
            ITYPE(10),IHOL(10),IHOL2(10),INT1(10),FLOAT1(10),IERRO1(10))
      IF(IFOUNZ(10).EQ.'YES')GO TO 4390
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 4390 CONTINUE
      GO TO 4500
!
!     STEP 4.4--IF SUBSET HAS BEEN FOUND,
!     IF NO RIGHT-HAND SIDE PARENTHESES FOUND,
!     EXTRACT VARIABLE NAME OR VALUE;
!     SEARCH BETWEEN = AND SUBSET.
!
 4400 CONTINUE
!
      ISTEPN='4.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(11)
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(7),IBEGIN(7),IEND(7),   &
            ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GO TO 4490
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 4490 CONTINUE
      GO TO 4700
!
!     STEP 4.5--IF SUBSET HAS BEEN FOUND,
!     IF RIGHT-HAND SIDE PARENTHESES FOUND,
!     FIRST EXTRACT VARIABLE NAME;
!     SEARCH BETWEEN = AND (.
!
 4500 CONTINUE
!
      ISTEPN='4.5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(8)
      ISTRIN='!;('
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(7),IBEGIN(7),IEND(7),   &
            ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GO TO 4590
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 4590 CONTINUE
!
!     STEP 4.6--IF SUBSET HAS BEEN FOUND,
!     ALSO IF RIGHT-HAND SIDE PARENTHESES FOUND,
!     SEARCH FOR RIGHT-HAND SIDE ARGUMENT NAME OR VALUE;
!     SEARCH BETWEEN ( AND ).
!
      ISTEPN='4.6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(8)
      ISTOP1=IBEGIN(10)
      ISTRIN='(;)'
      ISTRI2='    '
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(9),IBEGIN(9),IEND(9),   &
            ITYPE(9),IHOL(9),IHOL2(9),INT1(9),FLOAT1(9),IERRO1(9))
      IF(IFOUNZ(9).EQ.'YES')GO TO 4690
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 4690 CONTINUE
      K=9
      IF(ITYPE(K).EQ.'WORD')   &
      CALL DPCHEC(K,IHOL,IHOL2,   &
      IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
      INT1,FLOAT1,IBUGA3,IERROR)
!
!     STEP 4.7--IF SUBSET HAS BEEN FOUND,
!     SEARCH FOR VARIABLE NAME AFTER SUBSET;
!     SEARCH BETWEEN SUBSET AND THE END OF THE LINE.
!
 4700 CONTINUE
!
      ISTEPN='4.7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(11)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(12),IBEGIN(12),IEND(12),   &
            ITYPE(12),IHOL(12),IHOL2(12),INT1(12),FLOAT1(12),IERRO1(12))
      IF(IFOUNZ(12).EQ.'YES')GO TO 4790
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 4790 CONTINUE
!
!     STEP 4.8--IF SUBSET HAS BEEN FOUND,
!     SEARCH FOR LOWER LIMIT VALUE AFTER     SUBSET XXX
!     SEARCH BETWEEN VARIABLE NAME AND THE END OF THE LINE.
!
      ISTEPN='4.8'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(12)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(13),IBEGIN(13),IEND(13),   &
            ITYPE(13),IHOL(13),IHOL2(13),INT1(13),FLOAT1(13),IERRO1(13))
      IF(IFOUNZ(13).EQ.'YES')GO TO 4890
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 4890 CONTINUE
!
!     STEP 4.9--IF SUBSET HAS BEEN FOUND,
!     SEARCH FOR UPPER LIMIT (IF EXISTENT) AFTER     SUBSET XXX
!     SEARCH BETWEEN LOWER LIMIT AND THE END OF THE LINE.
!
      ISTEPN='4.9'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(13)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(14),IBEGIN(14),IEND(14),   &
            ITYPE(14),IHOL(14),IHOL2(14),INT1(14),FLOAT1(14),IERRO1(14))
      GO TO 6000
!
!               *******************************************************
!               **  STEP 5--                                         **
!               **  FOR THE CASE WHEN HAVE     LET X(I) =            **
!               **  EXAMINE THE RIGHT-HAND SIDE FOR    FOR I = A  B  C*
!               **  COMPONENT 7  = VARIABLE NAME                     **
!               **  COMPONENT 8  = (                                 **
!               **  COMPONENT 9  = ARGUMENT (THAT IS, ROW OF TABLE)  **
!               **  COMPONENT 10 = )                                 **
!               **  COMPONENT 21 = FOR                               **
!               **  COMPONENT 22 = =                                 **
!               **  COMPONENT 23 = START     VALUE FOR DUMMY INDEX   **
!               **  COMPONENT 24 = INCREMENT VALUE FOR DUMMY INDEX   **
!               **  COMPONENT 25 = STOP      VALUE FOR SUMMY INDEX   **
!               *******************************************************
!
 5000 CONTINUE
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     STEP 5.2--IF FOR HAS BEEN FOUND,
!     SEARCH FOR RIGHT-HAND SIDE (;
!     SEARCH BETWEEN = AND FOR.
!
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(21)-1
      ISTRIN='('
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(8),IBEGIN(8),IEND(8),   &
            ITYPE(8),IHOL(8),IHOL2(8),INT1(8),FLOAT1(8),IERRO1(8))
      IF(IFOUNZ(8).EQ.'YES')GO TO 5290
      GO TO 5400
 5290 CONTINUE
!
!     STEP 5.3--IF FOR HAS BEEN FOUND,
!     SEARCH FOR RIGHT-HAND SIDE );
!     SEARCH BETWEEN ( AND FOR.
!
      ISTEPN='5.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(8)+1
      ISTOP1=IBEGIN(21)-1
      ISTRIN=')'
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(10),IBEGIN(10),IEND(10),   &
            ITYPE(10),IHOL(10),IHOL2(10),INT1(10),FLOAT1(10),IERRO1(10))
      IF(IFOUNZ(10).EQ.'YES')GO TO 5390
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5390 CONTINUE
      GO TO 5500
!
!     STEP 5.4--IF FOR HAS BEEN FOUND,
!     IF NO RIGHT-HAND SIDE PARENTHESES FOUND,
!     EXTRACT VARIABLE NAME OR VALUE;
!     SEARCH BETWEEN = AND FOR.
!
 5400 CONTINUE
!
      ISTEPN='5.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(21)
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(7),IBEGIN(7),IEND(7),   &
            ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GO TO 5490
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5490 CONTINUE
      GO TO 5700
!
!     STEP 5.5--IF FOR HAS BEEN FOUND,
!     IF RIGHT-HAND SIDE PARENTHESES FOUND,
!     FIRST EXTRACT VARIABLE NAME;
!     SEARCH BETWEEN = AND (.
!
 5500 CONTINUE
!
      ISTEPN='5.5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(6)+1
      ISTOP1=IBEGIN(8)
      ISTRIN='!;('
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(7),IBEGIN(7),IEND(7),   &
            ITYPE(7),IHOL(7),IHOL2(7),INT1(7),FLOAT1(7),IERRO1(7))
      IF(IFOUNZ(7).EQ.'YES')GO TO 5590
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5590 CONTINUE
!
!     STEP 5.6--IF FOR HAS BEEN FOUND,
!     ALSO IF RIGHT-HAND SIDE PARENTHESES FOUND,
!     SEARCH FOR RIGHT-HAND SIDE ARGUMENT NAME OR VALUE;
!     SEARCH BETWEEN ( AND ).
!
      ISTEPN='5.6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(8)
      ISTOP1=IBEGIN(10)
      ISTRIN='(;)'
      ISTRI2='    '
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(9),IBEGIN(9),IEND(9),   &
            ITYPE(9),IHOL(9),IHOL2(9),INT1(9),FLOAT1(9),IERRO1(9))
      IF(IFOUNZ(9).EQ.'YES')GO TO 5690
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5690 CONTINUE
      K=9
      IF(ITYPE(K).EQ.'WORD')   &
      CALL DPCHEC(K,IHOL,IHOL2,   &
      IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
      INT1,FLOAT1,IBUGA3,IERROR)
!
!     STEP 5.7--IF FOR HAS BEEN FOUND,
!     SEARCH FOR VARIABLE NAME AFTER FOR;
!     SEARCH BETWEEN FOR AND THE END OF THE LINE.
!
 5700 CONTINUE
!
      ISTEPN='5.7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(21)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(22),IBEGIN(22),IEND(22),   &
            ITYPE(22),IHOL(22),IHOL2(22),INT1(22),FLOAT1(22),IERRO1(22))
      IF(IFOUNZ(22).EQ.'YES')GO TO 5790
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5790 CONTINUE
!
!     STEP 5.8--IF FOR HAS BEEN FOUND,
!     SEARCH FOR = SIGN AFTER    FOR XXX
!     SEARCH BETWEEN VARIABLE NAME AND END OF LINE.
!
      ISTEPN='5.8'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(22)+1
      ISTOP1=IWIDTH
      ISTRIN='='
      ISTRI2='    '
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(23),IBEGIN(23),IEND(23),   &
            ITYPE(23),IHOL(23),IHOL2(23),INT1(23),FLOAT1(23),IERRO1(23))
      IF(IFOUNZ(23).EQ.'YES')GO TO 5890
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5890 CONTINUE
!
!     STEP 5.9--IF FOR HAS BEEN FOUND,
!     SEARCH FOR START VALUE AFTER     FOR XXX =
!     SEARCH BETWEEN = AND THE END OF THE LINE.
!
      ISTEPN='5.9'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(23)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(24),IBEGIN(24),IEND(24),   &
            ITYPE(24),IHOL(24),IHOL2(24),INT1(24),FLOAT1(24),IERRO1(24))
      IF(IFOUNZ(24).EQ.'YES')GO TO 5990
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5990 CONTINUE
!
!     STEP 5.10--IF FOR HAS BEEN FOUND,
!     SEARCH FOR INCREMENT VALUE AFTER     FOR XXX =
!     SEARCH BETWEEN START VALUE AND THE END OF THE LINE.
!
      ISTEPN='5.10'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(24)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(25),IBEGIN(25),IEND(25),   &
            ITYPE(25),IHOL(25),IHOL2(25),INT1(25),FLOAT1(25),IERRO1(25))
      IF(IFOUNZ(25).EQ.'YES')GO TO 5930
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5930 CONTINUE
!
!     STEP 5.11--IF FOR HAS BEEN FOUND,
!     SEARCH FOR STOP VALUE AFTER     FOR XXX =
!     SEARCH BETWEEN INCREMENT VALUE AND THE END OF THE LINE.
!
      ISTEPN='5.11'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IEND(25)+1
      ISTOP1=IWIDTH
      ISTRIN='!;:'
      ISTRI2='    '
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGA3,   &
            IFOUNZ(26),IBEGIN(26),IEND(26),   &
            ITYPE(26),IHOL(26),IHOL2(26),INT1(26),FLOAT1(26),IERRO1(26))
      IF(IFOUNZ(26).EQ.'YES')GO TO 5950
      CALL DPLETE(IANS,IWIDTH)
      IERROR='YES'
      GO TO 9000
 5950 CONTINUE
      GO TO 6000
!
!               ************************************************
!               **  STEP 6--                                  **
!               **  DETERMINE VARIOUS SUMMARY VARIABLES       **
!               **  FOR THE LEFT SIDE                         **
!               **  OF THE COMMAND LINE                       **
!               **  WHICH WILL BE HELPFUL BACK IN DPLET       **
!               **  FOR BRANCHING TO THE CORRECT              **
!               **  TYPE OF OPERATION.                        **
!               **  NOTE THAT THE    LEFT SIDE                    **
!               **  WILL BE FROM     LET                      **
!               **  TO THE           = SIGN                   **
!               **  BUT WILL NOT INCLUDE EITHER.              **
!               ************************************************
!
 6000 CONTINUE
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     STEP 6.0--
!     DETERMINE THE LIMITS OF THE LEFT SIDE
!
      IMINL=0
      IF(IFOUNZ(1).EQ.'YES')IMINL=IEND(1)+1
!
      IMAXL=0
      IF(IFOUNZ(6).EQ.'YES')IMAXL=IBEGIN(6)-1
!
      IF(IMINL.LE.0)GO TO 6900
      IF(IMAXL.LE.0)GO TO 6900
      IF(IMINL.GT.IMAXL)GO TO 6900
!
!     STEP 6.1--
!     DETERMINE THE NUMBER OF COMPONENTS ON THE LEFT.
!     A COMPONET HERE = A WORD OR A PARENTHESIS.
!
      ISTEPN='6.1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUM=0
      IMIN=2
      IMAX=5
      DO 6100 I=IMIN,IMAX
      IF(IFOUNZ(I).EQ.'YES')ISUM=ISUM+1
 6100 CONTINUE
      NUMCL=ISUM
!
!     STEP 6.2--
!     DETERMINE THE NUMBER OF PARENTHESES (LEFT + RIGHT).
!
      ISTEPN='6.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUM=0
      IMIN=IMINL
      IMAX=IMAXL
      DO 6200 I=IMIN,IMAX
      IF(IANS(I).EQ.'('.OR.IANS(I).EQ.')')ISUM=ISUM+1
 6200 CONTINUE
      NUMPL=ISUM
!
!     STEP 6.3--
!     DETERMINE THE NUMBER OF ARITHMETIC OPERATIONS
!     +  -  *  /      ON THE LEFT
!     (IT SHOULD BE 0).
!     NOTE THAT THE ARITHMETIC OPERATION   **
!     WILL BE LUMPED IN WITH    *    .
!
      ISTEPN='6.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUM=0
      IMIN=IMINL
      IMAX=IMAXL
      DO 6300 I=IMIN,IMAX
      IF(IANS(I).EQ.'+' .OR. IANS(I).EQ.'-' .OR. IANS(I).EQ.'*' .OR.   &
         IANS(I).EQ.'/')ISUM=ISUM+1
 6300 CONTINUE
      NUMAOL=ISUM
!
!     STEP 6.4--
!     DETERMINE THE TYPE ('NUMB' OR 'WORD')
!     FOR THE FIRST WORD ON THE LEFT.
!     THIS SHOULD BE THE VARIABLE OR PARAMETER
!     DESIGNATION,
!     AND IT SHOULD BE A 'WORD'.
!
      ISTEPN='6.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITYW1L=ITYPE(2)
!
!     STEP 6.5--
!     DETERMINE IF FIRST WORD ON THE LEFT
!     IS ALREADY IN THE NAME LIST OR NOT.
!
      ISTEPN='6.5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INLI1L='NO'
      IVARL=IHOL(2)
      IVARL2=IHOL2(2)
      DO 6500 I=1,NUMNAM
      IF(IVARL.EQ.IHNAME(I).AND.IVARL2.EQ.IHNAM2(I))INLI1L='YES'
 6500 CONTINUE
!
!     STEP 6.6--
!     DETERMINE IF FIRST WORD ON THE LEFT
!     IS IN THE VARIABLE/PARAMETER NAME LIST, OR
!     IS A COLUMN NAMING (I.E., THE WORD 'COLU' OR 'COL', OR
!     IS A DATA MANIPULATION FUNCTION, OR
!     IS A STATISTICAL CALCULATION FUNCTION
!     (SEARCH IS DONE IN THAT ORDER).
!
!
      ISTEPN='6.6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICAT1L='NONE'
      IVARL=IHOL(2)
      IVARL2=IHOL2(2)
!
      IF(INLI1L.EQ.'YES'.AND.IVARL.NE.'COLU')GO TO 6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.NE.'COL ')GO TO 6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.EQ.'COLU'.AND.IVARL2.EQ.'MN  '.AND.   &
      IFOUNZ(3).EQ.'NO')GO TO 6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.EQ.'COL '.AND.IVARL2.EQ.'    '.AND.   &
      IFOUNZ(3).EQ.'NO')GO TO 6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.EQ.'COLU'.AND.IVARL2.EQ.'MN  '.AND.   &
      IFOUNZ(3).EQ.'YES'.AND.ITYPE(3).NE.'NUMB')GO TO 6615
      IF(INLI1L.EQ.'YES'.AND.IVARL.EQ.'COL '.AND.IVARL2.EQ.'    '.AND.   &
      IFOUNZ(3).EQ.'YES'.AND.ITYPE(3).NE.'NUMB')GO TO 6615
      GO TO 6620
 6615 CONTINUE
      ICAT1L='VARP'
      GO TO 6690
!
 6620 CONTINUE
      IF(IVARL.EQ.'COLU'.AND.IVARL2.EQ.'MN  '.AND.   &
      IFOUNZ(3).EQ.'YES'.AND.ITYPE(3).EQ.'NUMB')GO TO 6625
      IF(IVARL.EQ.'COL '.AND.IVARL2.EQ.'    '.AND.   &
      IFOUNZ(3).EQ.'YES'.AND.ITYPE(3).EQ.'NUMB')GO TO 6625
      GO TO 6630
 6625 CONTINUE
      ICAT1L='COL'
      GO TO 6690
!
 6630 CONTINUE
      DO 6632 I=1,NUMMAN
      IF(IVARL.EQ.IHMAN(I).AND.IVARL2.EQ.IHMAN2(I))GO TO 6635
 6632 CONTINUE
      GO TO 6640
 6635 CONTINUE
      ICAT1L='MANI'
      GO TO 6690
!
 6640 CONTINUE
      DO 6642 I=1,NUMSTA
      IF(IVARL.EQ.IHSTAT(I).AND.IVARL2.EQ.IHSTA2(I))GO TO 6645
 6642 CONTINUE
      GO TO 6690
 6645 CONTINUE
      ICAT1L='STAT'
      GO TO 6690
!
 6690 CONTINUE
!
!     STEP 6.7--
!     DETERMINE THE TYPE ('NUMB' OR 'WORD')
!     FOR THE SECOND WORD
!     (AS OPPOSED TO THE SECOND COMPONENT)
!     ON THE LEFT.
!     IF EXISTENT, THIS SHOULD BE THE ARGUMENT DESIGNATION
!     OF A VARIABLE,
!     AND IT MAY BE EITHER A 'WORD' OR A 'NUMB'.
!
      ISTEPN='6.7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITYW2L=ITYPE(4)
!
 6900 CONTINUE
!
!               *******************************************************
!               **  STEP 7--                                         **
!               **  DETERMINE VARIOUS SUMMARY VARIABLES FOR THE      **
!               **  RIGHT SIDE OF THE COMMAND LINE WHICH WILL BE     **
!               **  HELPFUL BACK IN DPLET FOR BRANCHING TO THE       **
!               **  CORRECT TYPE OF OPERATION.  NOTE THAT THE        **
!               **  RIGHT SIDE WILL BE FROM THE = SIGN               **
!               **  TO THE           END OF THE LINE,                **
!               **  OR TO AN         ISOLATED FOR,                   **
!               **  OR TO AN         ISOLATED SUBSET                 **
!               **  (WHICHEVER OF THE 3 IS SMALLEST).                **
!               **  ALSO DETERMINE WHETHER THE QUALIFICATION         **
!               **  ON THE FAR RIGHT OF THE CARD IS                  **
!               **           1) BLANK (THAT IS, NO QUALIFICATION)    **
!               **           2) SUBSET                               **
!               **           3) FOR                                  **
!               **  THE VARIABLE IQUAL WILL BE DEFINED IN            **
!               **  THIS REGARD                                      **
!               **  IQUAL WILL = 'NONE', 'FOR', 'SUBS', OR 'ERRO'.   **
!               *******************************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     STEP 7.0--
!     DETERMINE THE LIMITS OF THE    RIGHT SIDE
!
      IMINR=0
      IF(IFOUNZ(6).EQ.'YES')IMINR=IEND(6)+1
!
      IF(IFOUNZ(11).EQ.'YES'.AND.IFOUNZ(21).EQ.'YES')GO TO 7020
      GO TO 7030
!
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
      GO TO 7090
!
 7030 CONTINUE
      IF(IFOUNZ(11).EQ.'NO'.AND.IFOUNZ(21).EQ.'NO')IQUAL='NONE'
      IF(IFOUNZ(11).EQ.'YES')IQUAL='SUBS'
      IF(IFOUNZ(21).EQ.'YES')IQUAL='FOR'
      IF(IQUAL.EQ.'NONE')IMAXR=IWIDTH
      IF(IQUAL.EQ.'SUBS')IMAXR=IBEGIN(11)-1
      IF(IQUAL.EQ.'FOR')IMAXR=IBEGIN(21)-1
!
 7090 CONTINUE
      IF(IMINR.LE.0)GO TO 7900
      IF(IMAXR.LE.0)GO TO 7900
      IF(IMINR.GT.IMAXR)GO TO 7900
!
!     STEP 7.1--
!     DETERMINE THE NUMBER OF COMPONENTS ON THE RIGHT.
!     A COMPONENT HERE = A WORD OR A PARENTHESIS.
!
      ISTEPN='7.1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUM=0
      IMIN=7
      IMAX=10
      DO 7100 I=IMIN,IMAX
      IF(IFOUNZ(I).EQ.'YES')ISUM=ISUM+1
 7100 CONTINUE
      NUMCR=ISUM
!
!     STEP 7.2--
!     DETERMINE THE NUMBER OF PARENTHESES (LEFT + RIGHT).
!
      ISTEPN='7.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUM=0
      IMIN=IMINR
      IMAX=IMAXR
      DO 7200 I=IMIN,IMAX
      IF(IANS(I).EQ.'('.OR.IANS(I).EQ.')')ISUM=ISUM+1
 7200 CONTINUE
      NUMPR=ISUM
!
!     STEP 7.3--
!     DETERMINE THE NUMBER OF ARITHMETIC OPERATIONS
!     +  -  *  /      ON THE RIGHT
!     (IT SHOULD BE 0).
!     NOTE THAT THE ARITHMETIC OPERATION   **
!     WILL BE LUMPED IN WITH    *    .
!
      ISTEPN='7.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUM=0
      IMIN=IMINR
      IMAX=IMAXR
      DO 7300 I=IMIN,IMAX
      IF(IANS(I).EQ.'+'.OR.IANS(I).EQ.'-'.OR.   &
         IANS(I).EQ.'*'.OR.IANS(I).EQ.'/')ISUM=ISUM+1
 7300 CONTINUE
      NUMAOR=ISUM
!
!     STEP 7.4--
!     DETERMINE THE TYPE ('NUMB' OR 'WORD')
!     FOR THE FIRST WORD ON THE RIGHT.
!     THIS SHOULD BE THE VARIABLE OR PARAMETER
!     DESIGNATION,
!     AND IT SHOULD BE A 'WORD'.
!
      ISTEPN='7.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITYW1R=ITYPE(7)
!
!     STEP 7.5--
!     DETERMINE IF FIRST WORD ON THE RIGHT
!     IS ALREADY IN THE NAME LIST OR NOT.
!
      ISTEPN='7.5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INLI1R='NO'
      IVARR=IHOL(7)
      IVARR2=IHOL2(7)
      DO 7500 I=1,NUMNAM
      IF(IVARR.EQ.IHNAME(I).AND.IVARR2.EQ.IHNAM2(I))INLI1R='YES'
 7500 CONTINUE
!
!     STEP 7.6--
!     DETERMINE IF FIRST WORD ON THE RIGHT
!     IS IN THE VARIABLE/PARAMETER NAME LIST, OR
!     IS A COLUMN NAMING (I.E., THE WORD 'COLU' OR 'COL', OR
!     IS A DATA MANIPULATION FUNCTION, OR
!     IS A STATISTICAL CALCULATION FUNCTION
!     (SEARCH IS DONE IN THAT ORDER).
!
      ISTEPN='7.6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICAT1R='NONE'
      IVARR=IHOL(7)
      IVARR2=IHOL2(7)
!
      IF(INLI1R.EQ.'YES'.AND.IVARR.NE.'COLU')GO TO 7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.NE.'COL ')GO TO 7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.EQ.'COLU'.AND.IVARR2.EQ.'MN  '.AND.   &
      IFOUNZ(8).EQ.'NO')GO TO 7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.EQ.'COL '.AND.IVARR2.EQ.'    '.AND.   &
      IFOUNZ(8).EQ.'NO')GO TO 7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.EQ.'COLU'.AND.IVARR2.EQ.'MN  '.AND.   &
      IFOUNZ(8).EQ.'YES'.AND.ITYPE(8).NE.'NUMB')GO TO 7615
      IF(INLI1R.EQ.'YES'.AND.IVARR.EQ.'COL '.AND.IVARR2.EQ.'    '.AND.   &
      IFOUNZ(8).EQ.'YES'.AND.ITYPE(8).NE.'NUMB')GO TO 7615
      GO TO 7620
 7615 CONTINUE
      ICAT1R='VARP'
      GO TO 7690
!
 7620 CONTINUE
      IF(IVARR.EQ.'COLU'.AND.IVARR2.EQ.'MN  '.AND.   &
      IFOUNZ(8).EQ.'YES'.AND.ITYPE(8).EQ.'NUMB')GO TO 7625
      IF(IVARR.EQ.'COL '.AND.IVARR2.EQ.'    '.AND.   &
      IFOUNZ(8).EQ.'YES'.AND.ITYPE(8).EQ.'NUMB')GO TO 7625
      GO TO 7630
 7625 CONTINUE
      ICAT1R='COL'
      GO TO 7690
!
 7630 CONTINUE
      DO 7632 I=1,NUMMAN
      IF(IVARR.EQ.IHMAN(I).AND.IVARR2.EQ.IHMAN2(I))GO TO 7635
 7632 CONTINUE
      GO TO 7640
 7635 CONTINUE
      ICAT1R='MANI'
      GO TO 7690
!
 7640 CONTINUE
      DO 7642 I=1,NUMSTA
      IF(IVARR.EQ.IHSTAT(I).AND.IVARR2.EQ.IHSTA2(I))GO TO 7645
 7642 CONTINUE
      GO TO 7690
 7645 CONTINUE
      ICAT1R='STAT'
      GO TO 7690
!
 7690 CONTINUE
!
!     STEP 7.7--
!     DETERMINE THE TYPE ('NUMB' OR 'WORD')
!     FOR THE SECOND WORD
!     (AS OPPOSED TO THE SECOND COMPONENT)
!     ON THE RIGHT.
!     IF EXISTENT, THIS SHOULD BE THE ARGUMENT DESIGNATION
!     OF A VARIABLE,
!     AND IT MAY BE EITHER A 'WORD' OR A 'NUMB'.
!
      ISTEPN='7.7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITYW2R=ITYPE(9)
!
 7900 CONTINUE
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
 9011   FORMAT('****** AT THE END      OF DPTYP2--')
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,30
          IF(18.LE.I.AND.I.LE.20)GO TO 9020
          IF(I.GE.25)GO TO 9020
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9022)
 9022     FORMAT('I--IFOUNZ,IBEGIN,IEND,',   &
                 'ITYPE,IHOL,IHOL2,INT1,FLOAT1,IERRO1')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9025)I,IFOUNZ(I),IBEGIN(I),IEND(I),ITYPE(I),   &
                           IHOL(I),IHOL2(I),INT1(I),FLOAT1(I),IERRO1(I)
 9025     FORMAT(I3,'--',A4,2(2X,I2),4X,3(A4,2X),I8,2X,D15.7,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)NUMCL,NUMPL,NUMAOL,ITYW1L,ITYW2L,INLI1L,ICAT1L
 9031   FORMAT('NUMCL,NUMPL,NUMAOL,ITYW1L,ITYW2L,INLI1L,ICAT1L = ',   &
               3I8,4(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)NUMCR,NUMPR,NUMAOR,ITYW1R,ITYW2R,INLI1R,ICAT1R
 9032   FORMAT('NUMCR,NUMPR,NUMAOR,ITYW1R,ITYW2R,INLI1R,ICAT1R = ',   &
               3I8,4(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPTYP2
      SUBROUTINE DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,   &
                        IBUGA3,   &
                        IFOUZ2,ISTAR2,ISTOP2,   &
                        ITYPE2,IHOL,IHOL2,INTZ,FLOATZ,IERROR)
!
!     PURPOSE--SCAN THE CHARACTER ARRAY IANS(.) BETWEEN COLUMNS ISTAR1
!              AND ISTOP1 FOR THE STRING DEFINED IN STRIN AND ISTRI2.
!     NOTE THAT THE STRING DEFINED IN ISTRIN AND ISTRI2
!     MAY BE EXPRESSED IN SEVERAL WAYS--
!          1) EXPLICITLY, E.G., LET    FOR    SUBSET, ETC.
!          2) IMPLICITLY WITH ! REPRESENTING THE FIRST
!             NON-BLANK CHARACTER THAT IS ENCOUNTERED;
!          3) IMPLICITLY WITH ; REPRESENTING ANY STRING
!             (INCLUDING ALL CHARACTERS, EVEN BLANKS));
!          4) IMPLICITLY WITH : REPRESENTING THE FIRST
!            BLANK CHARACTER THAT IS ENCOUNTERED.
!     NOTE--A GIVEN ARGUMENT MAY END UP WITH
!            3 DIFFERENT REPRESENTATIONS--
!            HOLLERITH, INTEGER, AND FLOATING POINT.
!     INPUT  ARGUMENTS--IANS   = A HOLLERITH 1-CHARACTER-PER-WORD
!                                VARIABLE CONTAINING THE INPUT LINE
!                                TO BE EXAMINED.
!                     --IWIDTH = THE (FULL) WIDTH OF THE INPUT LINE
!                                (THAT IS, THE NUMBER OF COLUMNS)
!                     --ISTAR1 = THE FIRST COLUMN FOR WHICH THE
!                                SCAN IS TO BE CARRIED OUT.
!                     --ISTOP1 = THE LAST  COLUMN FOR WHICH THE
!                                SCAN IS TO BE CARRIED OUT.
!                     --ISTRIN = THE HOLLERITH VARIABLE WHICH CONTAINS
!                                CHARACTERS 1 TO 4 OF THE STRING TO BE
!                                SEARCHED FOR.  THE DEFINITION OF THE
!                                STRING IN ISTRIN MAY MAY BE DONE
!                                EXPLICITLY (BUT IS LIMITED TO 4
!                                CHARACTERS) OR IMPLICITLY WHICH IS NOT
!                                LIMITED TO 4 CHARACTERS AND IS MORE
!                                GENERAL IN OTHER WAYS ALSO.
!                     --ISTRI2 = THE HOLLERITH VARIABLE WHICH CONTAINS
!                                CHARACTERS 5 TO 8 OF THE STRING TO BE
!                                SEARCHED FOR.  THE DEFINITION OF THE
!                                STRING IN ISTRIN MAY MAY BE DONE
!                                EXPLICITLY (BUT IS LIMITED TO 4
!                                CHARACTERS) OR IMPLICITLY WHICH IS NOT
!                                LIMITED TO 4 CHARACTERS AND IS MORE
!                                GENERAL IN OTHER WAYS ALSO.
!                     --INEX   = A HOLLERITH VARIABLE WHICH CONTAINS ONE
!                                OF THE FOLLOWING 4 VALUES--II, IE, EI, EE
!                                WHERE I STANDS FOR INCLUSIVE AND E
!                                STANDS FOR EXCLUSIVE;
!                                INEX SPECIFIES WHETHER THE FIRST OR LAST
!                                CHARACTER IS TO BE INCLUDED OR EXCLUDED IN
!                                IN DEFINING ISTAR2 AND ISTOP2.
!     OUTPUT ARGUMENTS--IFOUZ2 = A HOLLERITH VARIABLE WITH THE VALUE 'YES'
!                                IF THE STRING WAS FOUND AND THE VALUE
!                                'NO' IF THE STRING WAS NOT FOUND.
!                     --ISTAR2 = THE START COLUMN OF THE FOUND STRING
!                     --ISTOP2 = THE STOP COLUMN OF THE FIUND STRING.
!                     --ITYPE2 = A HOLLERITH VARIABLE WITH THE VALUE
!                                'WORD' IF THE STRING CONTAINS ANY
!                                NON-NUMERIC (EXCLUDING BLANKS) CHARACTER
!                                AND THE VALUE 'NUMB' IF THE STRING CONTAINS
!                                ALL NUMERIC VALUES OR DECIMAL POINT OR + OR -
!                                (WITH INTERMITTENT BLANKS IGNORED).
!                     --IHOL   = THE HOLLERITH VARIABLE CONTAINING THE
!                                PACKED (4 CHARACTERS) VERSION OF
!                                CHARACTERS 1 TO 4 OF THE FOUND STRING.
!                     --IHOL2  = THE HOLLERITH VARIABLE CONTAINING THE
!                                PACKED (4 CHARACTERS) VERSION OF
!                                CHARACTERS 5 TO 8 OF THE FOUND STRING.
!                     --INT    = THE INTEGER VARIABLE CONTAINING THE
!                                INTEGER REPRESENTATION (IF POSSIBLE) OF
!                                THE FOUND STRING.
!                     --FLOAT  = THE FLOATING POINT VARIABLE CONTAINING
!                                THE FLOATING POINT REPRESENTATION
!                                (IF POSSIBLE) OF THE FOUND STRING.
!                     --IERROR = A HOLLERITH VARIABLE WITH VALUE
!                                'YES' OR 'NO' INDICATING IF AN
!                                ERROR CONDITION EXISTS.
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
!     ORIGINAL VERSION--FEBRUARY  1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --OCTOBER   1978.
!     UPDATED         --NOVEMBER  1980.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --JUNE      1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --AUGUST    2021. SOME RECODING FOR BETTER
!                                       READABILITY
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IANS(*)
      CHARACTER*4 ISTRIN
      CHARACTER*4 ISTRI2
      CHARACTER*4 INEX
      CHARACTER*4 IBUGA3
      CHARACTER*4 IFOUZ2
      CHARACTER*4 ITYPE2
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IERROR
!
      CHARACTER*4 ITEMP
      CHARACTER*4 IFLUNK
      CHARACTER*4 ISTRI3(20)
      CHARACTER*4 ILAST
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
      ISUBN1='DPTY'
      ISUBN2='P3  '
      IERROR='NO'
!
      IPJM1=0
      NUMASC=4
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTYP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ISTAR1,ISTOP1,IWIDTH
   53   FORMAT('ISTAR1,ISTOP1,IWIDTH = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGA3,ISTRIN,ISTRI2,INEX
   54   FORMAT('IBUGA3,ISTRIN,ISTRI2,INEX = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)(IANS(I),I=1,MIN(80,IWIDTH))
   61   FORMAT('IANS(.) = ',80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  INITIALIZE THE OUTPUT PARAMETERS AND VARIABLES  **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFOUZ2='NO'
      ISTAR2=-1
      ISTOP2=-1
      ITYPE2='9999'
      IHOL ='9999'
      IHOL2='9999'
      FLOATZ=-999999.0
!
!               *********************************************************
!               **  STEP 2--                                           **
!               **  DECOMPOSE THE INPUT SEARCH STRING INTO A1 CHARACTERS*
!               *********************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IMAX=2*NUMASC
      DO 300 I=1,IMAX
        I2=I
        J=I
        IF(I.GT.NUMASC)J=I-NUMASC
        ISTAR3=NUMBPC*(J-1)
        ISTAR3=IABS(ISTAR3)
        ITEMP='    '
        IVALT=0
        IF(I.LE.NUMASC)THEN
          CALL DPCHEX(ISTAR3,NUMBPC,ISTRIN,IVALT,NUMBPC,ITEMP)
        ELSE
          CALL DPCHEX(ISTAR3,NUMBPC,ISTRI2,IVALT,NUMBPC,ITEMP)
        ENDIF
        IF(ITEMP.EQ.'    ')THEN
          ILEN2=I2-1
          GO TO 390
        ENDIF
        ISTRI3(I)=ITEMP
  300 CONTINUE
      ILEN2=I2
!
  390 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,391)
  391   FORMAT('IN THE MIDDLE OF DPTYP3 (AFTER STEP 2)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,392)ILEN2,ISTRIN,ISTRI2
  392   FORMAT('ILEN2,ISTRIN,ISTRI2 = ',I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,393)(ISTRI3(I),I=1,ILEN2)
  393   FORMAT('ISTRI3(.) = ',6A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************************************
!               **  STEP 3--                                          **
!               **  DISTINGUISH BETWEEN THE 3 TYPES OF POSSIBLE       **
!               **  SEARCH STRINGS--                                  **
!               **  1) AN EXPLICITELY-DEFINED STRING; E.G.,           **
!               **     LET   FOR   SUBSET   =   5.3   -2.6666666      **
!               **     (AS IN COMMANDS, KEY WORDS, AND NUMBERS);      **
!               **  2) A STRING STARTING WITH THE FIRST NON-BLANK     **
!               **     CHARACTER AND ENDING WITH SOME SPECIFIED       **
!               **     CHARACTER; E.G., XXXXX( (AS IN THE VARIABLE    **
!               **     NAME OF A SUBSCRIPTED VARIABLE, OR THE ARGUMENT**
!               **     (I. E., THE SUBSCRIPT) IN A SUBSCRIPTED        **
!               **     VARIABLE);                                     **
!               **  3) A STRING STARTING WITH THE FIRST NON-BLANK     **
!               **     CHARACTER AND ENDING WITH THE FIRST SUBSEQUENT **
!               **     BLANK CHARACTER (OR ENDING WITH THE END OF THE **
!               **     LINE).  E.G., XXXX  (AS IN SOME UNSPECIFIED    **
!               **     PARAMETER OR VARIABLE NAME).                   **
!               ********************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASE=1
      IF(ISTRI3(1).NE.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).NE.':')   &
      ICASE=2
      IF(ISTRI3(1).EQ.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).NE.':')   &
      ICASE=3
      IF(ISTRI3(1).EQ.'!'.AND.ISTRI3(2).EQ.';'.AND.ISTRI3(3).EQ.':')   &
      ICASE=4
      IF(ILEN2.EQ.1.OR.ILEN2.EQ.2)ICASE=1
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,395)
  395   FORMAT('AFTER STEP 3 OF DPTYP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,396)ICASE
  396   FORMAT('ICASE = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 4--                                           **
!               **  DETERMINE IF THE DESIRED SEARCH STRING IS PRESENT  **
!               *********************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     2021/10: ADD INEX="IIWO" OPTION.  THIS IS INTENDED FOR
!              "SUBSET", "EXCEPT", "FOR" AND "IF" CLAUSES.
!              SPECIFICALLY, CHECK IF CHARACTER BEFORE AND THE
!              CHARACTER AFTER ARE BLANK.  DO THIS TO ADDRESS
!              ERROR WHEN VARIABLE NAME INCLUDES THESE AS PART
!              OF THE VARIABLE NAME.
!
      IF(ICASE.EQ.1)THEN
  401   CONTINUE
        DO 410 I=ISTAR1,ISTOP1
          I2=I
          IF(IANS(I).NE.ISTRI3(1))GO TO 410
          DO 430 J=1,ILEN2
            IPJM1=J+I-1
            IF(IPJM1.GT.ISTOP1)GO TO 410
            IF(IANS(IPJM1).EQ.ISTRI3(J))GO TO 430
            GO TO 410
  430     CONTINUE
          IFOUZ2='YES'
          IF(INEX.EQ.'IIWO')THEN
            IF(I2.GT.1 .AND. IANS(I2-1).NE.' ')THEN
              IFOUZ2='NO'
              ISTAR1=IPJM1+1
              GO TO 401
            ELSEIF(I2.LT.IWIDTH .AND. IANS(IPJM1+1).NE.' ')THEN
              IFOUZ2='NO'
              ISTAR1=IPJM1+1
              GO TO 401
            ENDIF
            ISTAR2=I2
            ISTOP2=IPJM1
          ELSEIF(INEX.EQ.'II')THEN
            ISTAR2=I2
            ISTOP2=IPJM1
          ELSEIF(INEX.EQ.'IE')THEN
            ISTAR2=I2
            ISTOP2=IPJM1-1
          ELSEIF(INEX.EQ.'EI')THEN
            ISTAR2=I2+1
            ISTOP2=IPJM1
          ELSEIF(INEX.EQ.'EE')THEN
            ISTAR2=I2+1
            ISTOP2=IPJM1-1
          ENDIF
          IF(ISTAR2.LE.ISTOP2)GO TO 990
          GO TO 900
  410   CONTINUE
        IFOUZ2='NO'
        GO TO 9000
      ELSEIF(ICASE.EQ.2)THEN
        DO 510 I=ISTAR1,ISTOP1
          I2=I
          IF(IANS(I).EQ.ISTRI3(1))GO TO 520
  510   CONTINUE
        IFOUZ2='NO'
        GO TO 9000
  520   CONTINUE
        IMIN=I2
        DO 530 I=IMIN,ISTOP1
          I2=I
          IF(IANS(I).EQ.ISTRI3(ILEN2))GO TO 540
  530   CONTINUE
        IFOUZ2='NO'
        GO TO 9000
  540   CONTINUE
        IFOUZ2='YES'
        IF(INEX.EQ.'II')THEN
          ISTAR2=IMIN
          ISTOP2=I2
        ELSEIF(INEX.EQ.'IE')THEN
          ISTAR2=IMIN
          ISTOP2=I2-1
        ELSEIF(INEX.EQ.'EI')THEN
          ISTAR2=IMIN+1
          ISTOP2=I2
        ELSEIF(INEX.EQ.'EE')THEN
          ISTAR2=IMIN+1
          ISTOP2=I2-1
        ENDIF
        IF(ISTAR2.LE.ISTOP2)GO TO 990
        GO TO 900
      ELSEIF(ICASE.EQ.3)THEN
        DO 610 I=ISTAR1,ISTOP1
          I2=I
          IF(IANS(I).NE.' ')GO TO 620
  610   CONTINUE
        IFOUZ2='NO'
        GO TO 9000
  620   CONTINUE
        IMIN=I2
        DO 630 I=IMIN,ISTOP1
          I2=I
          IF(IANS(I).EQ.ISTRI3(ILEN2))GO TO 640
  630   CONTINUE
        IFOUZ2='NO'
        GO TO 9000
  640   CONTINUE
        IFOUZ2='YES'
        IF(INEX.EQ.'II')THEN
          ISTAR2=IMIN
          ISTOP2=I2
        ELSEIF(INEX.EQ.'IE')THEN
          ISTAR2=IMIN
          ISTOP2=I2-1
        ELSEIF(INEX.EQ.'EI')THEN
          ISTAR2=IMIN+1
          ISTOP2=I2
        ELSEIF(INEX.EQ.'EE')THEN
          ISTAR2=IMIN+1
          ISTOP2=I2-1
        ENDIF
        IF(ISTAR2.LE.ISTOP2)GO TO 990
        GO TO 900
      ELSEIF(ICASE.EQ.4)THEN
        ILAST='BLAN'
        DO 710 I=ISTAR1,ISTOP1
          I2=I
          IF(IANS(I).NE.' ')GO TO 720
  710   CONTINUE
        IFOUZ2='NO'
        GO TO 9000
  720   CONTINUE
        IMIN=I2
        DO 730 I=IMIN,ISTOP1
          I2=I
          IF(IANS(I).EQ.' ')GO TO 740
  730   CONTINUE
        ILAST='NOBL'
        IF(ISTOP1.EQ.IWIDTH)GO TO 740
        IFOUZ2='NO'
        GO TO 9000
  740   CONTINUE
        IFOUZ2='YES'
        IF(INEX.EQ.'II')THEN
          ISTAR2=IMIN
          IF(ISTOP1.NE.IWIDTH)ISTOP2=I2
          IF(ISTOP1.EQ.IWIDTH.AND.ILAST.EQ.'BLAN')ISTOP2=I2
          IF(ISTOP1.EQ.IWIDTH.AND.ILAST.NE.'BLAN')ISTOP2=I2
        ELSEIF(INEX.EQ.'IE')THEN
          ISTAR2=IMIN
          IF(ISTOP1.NE.IWIDTH)ISTOP2=I2-1
          IF(ISTOP1.EQ.IWIDTH.AND.ILAST.EQ.'BLAN')ISTOP2=I2-1
          IF(ISTOP1.EQ.IWIDTH.AND.ILAST.NE.'BLAN')ISTOP2=I2
        ELSEIF(INEX.EQ.'EI')THEN
          ISTAR2=IMIN+1
          IF(ISTOP1.NE.IWIDTH)ISTOP2=I2
          IF(ISTOP1.EQ.IWIDTH.AND.ILAST.EQ.'BLAN')ISTOP2=I2
          IF(ISTOP1.EQ.IWIDTH.AND.ILAST.NE.'BLAN')ISTOP2=I2
        ELSEIF(INEX.EQ.'EE')THEN
          ISTAR2=IMIN+1
          IF(ISTOP1.NE.IWIDTH)ISTOP2=I2-1
          IF(ISTOP1.EQ.IWIDTH.AND.ILAST.EQ.'BLAN')ISTOP2=I2-1
          IF(ISTOP1.EQ.IWIDTH.AND.ILAST.NE.'BLAN')ISTOP2=I2
        ENDIF
        IF(ISTAR2.LE.ISTOP2)GO TO 990
        GO TO 900
      ENDIF
!
  900 CONTINUE
!
!     NOTE--THE FOLLOWING SECTION HAS BEEN 'BUGGED' OUT
!           TO CIRCUMVENT A PROBLEM WITH Y=(...
!           WHILE IT STILL LOOKED FOR A VARIABLE NAME
!           BETWEEN THE = AND THE (     .
!     CAUTION--WHEN IBUGA3 = 'OFF', AS IT USUALLY IS,
!              IERROR CAN NEVER BE 'YES'
!              UPON RETURN FROM DPTYP3:
!              BUT WHEN IBUGA3 = 'ON' (AS IN ERROR TRACING)
!              IERROR MAY = 'YES' WHICH MAY CHANGE THE
!              LOGIC PATH BACK IN DPTYP2.
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,921)
  921   FORMAT('***** INTERNAL ERROR IN DPTYP3 SUBROUTINE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,922)
  922   FORMAT('ISTAR2 GREATER THAN ISTOP2')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,923)ISTAR2,ISTOP2,ICASE
  923   FORMAT('ISTAR2,ISTOP2,ICASE = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,925)IWIDTH
  925   FORMAT('IWIDTH = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,926)(IANS(I),I=1,MIN(80,IWIDTH))
  926   FORMAT('IANS(.) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,927)ISTAR1,ISTOP1,ILEN2
  927   FORMAT('ISTAR1,ISTOP1,ILEN2 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,929)(ISTRI3(I),I=1,ILEN2)
  929   FORMAT('ISTRI3(.) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,930)ISTRIN,ISTRI2,INEX
  930   FORMAT('ISTRIN,ISTRI2,INEX = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
  990 CONTINUE
!               ********************************************************
!               **  STEP 5--                                          **
!               **  CONVERT THE STRING INTO 2 HOLLERITH A4 WORDS.     **
!               **  IF MORE THAN 8 CHARACTERS, CONVERT ONLY           **
!               **  THE FIRST 8 CHARACTERS.                           **
!               **  OUTPUT THESE HOLLERITH WORDS AS IHOL AND IHOL2.   **
!               ********************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHOL ='    '
      IHOL2='    '
      IMAX=2*NUMASC
      J=0
      DO 1000 I=ISTAR2,ISTOP2
        J=J+1
        K=J
        IF(J.GT.NUMASC)K=J-NUMASC
        ISTAR3=NUMBPC*(K-1)
        ISTAR3=IABS(ISTAR3)
        IF(J.LE.NUMASC)THEN
          CALL DPCHEX(0,NUMBPC,IANS(I),ISTAR3,NUMBPC,IHOL)
        ELSE
          CALL DPCHEX(0,NUMBPC,IANS(I),ISTAR3,NUMBPC,IHOL2)
        ENDIF
        IF(J.GE.IMAX)GO TO 1050
 1000 CONTINUE
 1050 CONTINUE
!
!               ****************************************************************
!               **  STEP 6--
!               **  CONVERT (IF POSSIBLE) THE STRING INTO AN INTEGER ARGUMENT.
!               **  OUTPUT  THIS INTEGER VALUE IN INT.
!               ****************************************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFLUNK='NO'
      ITYPE2='NUMB'
      IDIG=0
      ISIGN=0
      IDECPT=0
      ISUM=0
      DO 2700 I=ISTAR2,ISTOP2
        IREV=ISTOP2-(I-ISTAR2)
        IF(IANS(IREV).EQ.' ')GO TO 2700
        IVALT=ICHAR(IANS(IREV)(1:1))
        IF(IVALT.GE.48 .AND. IVALT.LE.57)THEN
          ITERM=IVALT-48
          IDIG=IDIG+1
          TERM2=10.0**(IDIG-1)
          ITERM2=INT(TERM2 + 0.01)
          ISUM=ISUM+ITERM*ITERM2
        ELSEIF(IANS(IREV).EQ.'+')THEN
          ISIGN=ISIGN+1
          GO TO 2700
        ELSEIF(IANS(IREV).EQ.'-')THEN
          ISIGN=ISIGN+1
          ISUM=-ISUM
          GO TO 2700
        ELSEIF(IANS(IREV).EQ.'.')THEN
          IDECPT=IDECPT+1
          IF(IDECPT.EQ.1.AND.IDIG.EQ.0)GO TO 2700
          GO TO 2800
        ELSE
          IFLUNK='YES'
          GO TO 2800
        ENDIF
!
 2700 CONTINUE
!
      IF(IDIG.LE.0)GO TO 2800
      IF(ISIGN.GE.2)GO TO 2800
      INTZ=ISUM
 2800 CONTINUE
      IF(IFLUNK.EQ.'YES')ITYPE2='WORD'
!
!               *******************************************************
!               **  STEP 7--                                         **
!               **  CONVERT (IF POSSIBLE) THE STRING INTO A FLOATING **
!               **  POINT ARGUMENT.                                  **
!               **  OUTPUT THIS FLOATING POINT VALUE IN FLOAT.       **
!               *******************************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      AMIN=-1000000.
      AMAX=+1000000.
      IFLUNK='NO'
      ITYPE2='NUMB'
      FLOATZ=-1.0
!
      ILOC=0
      IDECPT=0
      DO 3060 I=ISTAR2,ISTOP2
        IF(IANS(I).EQ.'.')THEN
          ILOC=I
          IDECPT=IDECPT+1
        ENDIF
 3060 CONTINUE
      IF(IDECPT.GE.2)GO TO 3900
      IF(IDECPT.EQ.1)GO TO 3150
      DO 3100 I=ISTAR2,ISTOP2
        IREV=ISTOP2-(I-ISTAR2)
        IF(IANS(IREV).EQ.' ')GO TO 3100
        IF(IANS(IREV).EQ.'0')GO TO 3110
        IF(IANS(IREV).EQ.'1')GO TO 3110
        IF(IANS(IREV).EQ.'2')GO TO 3110
        IF(IANS(IREV).EQ.'3')GO TO 3110
        IF(IANS(IREV).EQ.'4')GO TO 3110
        IF(IANS(IREV).EQ.'5')GO TO 3110
        IF(IANS(IREV).EQ.'6')GO TO 3110
        IF(IANS(IREV).EQ.'7')GO TO 3110
        IF(IANS(IREV).EQ.'8')GO TO 3110
        IF(IANS(IREV).EQ.'9')GO TO 3110
        IFLUNK='YES'
        IF(IANS(IREV).EQ.'+')GO TO 3900
        IF(IANS(IREV).EQ.'-')GO TO 3900
        GO TO 3900
 3100 CONTINUE
      IFLUNK='YES'
      GO TO 3900
 3110 ILOC=IREV+1
 3150 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,3111)ILOC,IDECPT
 3111   FORMAT('ILOC = ',I8,'    IDECPT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     SECONDLY, COMPUTE THE INTEGER PART OF THE VALUE
!
      SIGN=1.0
      IDIGI=0
      ISIGN=0
      SUMI=0
      ILOCM1=ILOC-1
      IF(ILOCM1.LT.ISTAR2)GO TO 3250
      DO 3200 I=ISTAR2,ILOCM1
        IREV=ILOCM1-(I-ISTAR2)
        IF(IANS(IREV).EQ.' ')GO TO 3200
        IVALT=ICHAR(IANS(IREV)(1:1))
        IF(IVALT.GE.48 .AND. IVALT.LE.57)THEN
          ITERM=IVALT-48
          IDIGI=IDIGI+1
          TERM=ITERM
          IEXP=IDIGI-1
          SUMI=SUMI+TERM*(10.0**IEXP)
        ELSEIF(IANS(IREV).EQ.'+')THEN
          ISIGN=ISIGN+1
          GO TO 3200
        ELSEIF(IANS(IREV).EQ.'-')THEN
          ISIGN=ISIGN+1
          SIGN=-SIGN
          GO TO 3200
        ELSE
          IFLUNK='YES'
          GO TO 3900
        ENDIF
 3200 CONTINUE
 3250 CONTINUE
      IF(ISIGN.GE.2)GO TO 3900
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,3255)IDIGI,SUMI
 3255   FORMAT('IDIGI = ',I8,'    SUMI = ',F20.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     THIRDLY, COMPUTE THE DECIMAL PART OF THE VALUE
!
      IDIGD=0
      SUMD=0.0
      ILOCP1=ILOC+1
      IF(ILOCP1.GT.ISTOP2)GO TO 3350
      DO 3300 I=ILOCP1,ISTOP2
        IF(IANS(I).EQ.' ')GO TO 3300
        IVALT=ICHAR(IANS(I)(1:1))
        IF(IVALT.GE.48 .AND. IVALT.LE.57)THEN
          ITERM=IVALT-48
          IDIGD=IDIGD+1
          TERM=ITERM
          SUMD=SUMD+TERM/(10.0**IDIGD)
        ELSE
          IFLUNK='YES'
          GO TO 3900
        ENDIF
 3300 CONTINUE
 3350 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,3355)IDIGD,SUMD
 3355   FORMAT('IDIGD = ',I8,'    SUMD = ',F20.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IDIGT=IDIGI+IDIGD
      IF(IDIGT.LE.0)GO TO 3900
      FLOATZ=SUMI+SUMD
      IF(SIGN.LT.0.0)FLOATZ=-FLOATZ
      IF(AMIN.LE.FLOATZ.AND.FLOATZ.LE.AMAX)GO TO 3000
      GO TO 3900
!
 3900 CONTINUE
      IF(IFLUNK.EQ.'YES')ITYPE2='WORD'
 3000 CONTINUE
      GO TO 9000
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001)
 9001   FORMAT('****** AT THE END       OF DPTYP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9002)IERROR,IFOUZ2,ISTAR2,ISTOP2
 9002   FORMAT('IERROR,IFOUZ2,ISTAR2,ISTOP2 = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9003)ITYPE2,IHOL,IHOL2,INTZ,FLOATZ
 9003   FORMAT('ITYPE2,IHOL,IHOL2,INTZ,FLOATZ = ',3(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPTYP3
      SUBROUTINE DPTYPE(IANSLC,IWIDTH,IBUGTY,   &
                        ICOM,ICOM2,ICOMT,ICOMI,ACOM,ICOMLC,ICOML2,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                        IHARG,IHARG2,IARGT,IARG,ARG,   &
                        IHARLC,IHARL2,NUMARG,   &
                        IHOST1,IHOST2)
!
!     PUTPOSE--TAKE THE COMPONENTS OF AN INPUT COMMAND LINE
!              AND COMPUTE HOLLERITH, INTEGER, AND FLOATING POINT
!              EQUIVALENTS FOR EACH COMPONENT.
!     INPUT  ARGUMENTS--IANSLC   (A HOLLERITH VECTOR)
!                     --IWIDTH (AN INTEGER VARIABLE)
!     OUTPUT ARGUMENTS--ICOM   (AN A4 HOLLERITH VALUE FOR COMMAND)
!                     --ICOM2  (AN A4 HOLLERITH VALUE FOR COMMAND)
!                     --ICOMLC  (AN A4 HOLLERITH VALUE FOR COMMAND)
!                     --ICOML2  (AN A4 HOLLERITH VALUE FOR COMMAND)
!                     --IHARG  (AN A4 HOLLERITH VECTOR)
!                     --IHARG2 (AN A4 HOLLERITH VECTOR)
!                     --IARG   (AN INTEGER VECTOR)
!                     --ARG    (A FLOATING POINT VECTOR)
!                     --IHARLC (AN A4 HOLLERITH VECTOR)
!                     --IHARL2 (AN A4 HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!      NOTE--A GIVEN ARGUMENT MAY END UP WITH
!            3 DIFFERENT REPRESENTATIONS--
!            HOLLERITH, INTEGER, AND FLOATING POINT.
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
!     ORIGINAL VERSION--NOVEMBER 10, 1977.
!     UPDATED         --MAY       1978.
!     UPDATED         --OCTOBER   1978.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --NOVEMBER  1980.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --NOVEMBER  1982.
!     UPDATED         --SEPTEMBER 1986.
!     UPDATED         --FEBRUARY  1989. ADJUST <> CASE (ALAN)
!     UPDATED         --AUGUST    1990. FIX HONEYWELL/PRIME > PROBLEM
!     UPDATED         --OCTOBER   1997. CHECK FOR EXPONENTIAL NUMBERS
!     UPDATED         --OCTOBER   2001. BUG ON SUN
!     UPDATED         --APRIL     2018. TREAT COMMA AS DELIMITER (IN
!                                       ADDITION TO SPACE AND HYPHEN)
!     UPDATED         --APRIL     2018. OPTIONS TO TURN OFF HYPHEN,
!                                       COMMA, OR EQUAL AS A DELIMITER
!     UPDATED         --APRIL     2018. IF EQUAL SIGN NOT A DELIMITER,
!                                       THEN QUOTE TO RIGHT OF EQUAL
!                                       DOES NOT START A NEW WORD
!                                       (BUT DO TURN QUOTING ON),
!                                       NEEDED FOR:
!                                          CALL TITLE="Sample Title"
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IFLUNK
      CHARACTER*4 IB
      CHARACTER*4 IANS1
      CHARACTER*4 IANS2
      CHARACTER*4 IH
      CHARACTER*4 IH2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*10 ICJUNK
      CHARACTER*5 IFRMT
!
!---------------------------------------------------------------------
!
      DIMENSION IANSLC(*)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
!
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IHARLC(*)
      DIMENSION IHARL2(*)
!
!CCCC PARAMETER (MAXZZZ=255)
      PARAMETER (MAXZZZ=1024)
!
      DIMENSION ISTART(MAXZZZ)
      DIMENSION ISTOP(MAXZZZ)
      DIMENSION IB(MAXZZZ)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTY'
      ISUBN2='PE  '
      IERROR='OFF'
!
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
!
!               ************************************************************
!               **  DEFINE NUMASC = NUMBER OF ASCII CHARACTERS PER WORD.  **
!               **  THIS IS 4 REGARDLESS OF THE COMPUTER MAKE AND         **
!               **  REGARDLESS OF THE WORD SIZE.                          **
!               ************************************************************
!
      NUMASC=4
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOM='    '
      ICOM2='    '
      ICOMT='NUMB'
      ICOMI=(-1)
      ACOM=(-1.0)
      ICOMLC='    '
      ICOML2='    '
      DO 110 I=1,100
        IHARG(I)='    '
        IHARG2(I)='    '
        IARGT(I)='NUMB'
        IARG(I)=(-1)
        ARG(I)=(-1.0)
        IHARLC(I)='    '
        IHARL2(I)='    '
  110 CONTINUE
      NUMARG=(-1)
!
!               **********************************************************
!               **  STEP 2--                                             *
!               **  SEPARATE IANSLC(.) INTO COMPONENTS WHERE A COMPONENT *
!               **  IS DEFINED AS THAT SEPARATED BY 1 OR MORE BLANKS     *
!               **  IN ADDITION, AN EQUAL SIGN (=),                      *
!               **  IN ADDITION, A COMMA (,), (2018/04)                  *
!
!CCCC --------------------------------------------------------------------
!CCCC THE FOLLOWING DEALING WITH > AND < WAS DEACTIVATED AUGUST 1990
!CCCC DUE TO FACT THAT > IS A DIRECTORY SEPARATOR FOR   AUGUST 1990
!CCCC CERTAIN COMPUTERS (E.G., HONEYWELL, PRIME).  AUGUST 1990
!CCCC AND     CALL DATAPLOT>DPSYSF.TEX    WAS BOMBING      AUGUST 1990
!CCCC WITH ARRAY OVERFLOW.                              AUGUST 1990
!CCCC THEREFORE--USER MUST MANUALLY MAKE SURE THAT > AND < AUGUST 1990
!CCCC            ARE SURROUNDED BY SPACES IN MATH COMMANDS.  AUGUST 1990
!
!               **  A GREATER-THAN SIGN (>), AND A LESS-THAN SIGN (<)    *
!               **  ARE ALSO CONSIDERED AS A COMPONENT UNTO ITSELF       *
!               **  REGARDLESS OF WHETHER OR NOT                         *
!               **  IT HAS PRECEEDING AND SUCCEEDING BLANKS.             *
!CCCC --------------------------------------------------------------------
!               **  FINALLY, A HYPHEN WHEN IMMEDIATELY PRECEDED          *
!               **  AND SUCCEEDED BY A NON-BLANK CHARACTER               *
!               **  WILL ALSO BE CONSIDERED AS A SEPARATOR               *
!               **  AND SO WILL NOT BE COPIED AS A CHARACTER.            *
!               **  HOWEVER, IF THERE IS A BLANK BEFORE OR AFTER THE     *
!               **  HYPEN (AS IN DEFINING THE    -    AS A PLOT CHARACTER*
!               **  TYPE), THEN THE HYPHEN WILL BE TREATED AND COPIED    *
!               **  AS A SEPARATE COMPONENT.                             *
!               **  OCTOBER 1997: CHECK FOR EXPONENTIAL NOTATION, I.E.   *
!               **      1.2E02, 1.2E-02, 1.2E+02, 1.2D02, 1.2D-02, 1.2D+02
!               **  TREAT THE CASE WHERE THE ORIGINAL LINE IANSLC(.) WAS NON-EMP
!               **  LOCATE THE START AND STOP COLUMNS FOR EACH 'WORD'.   *
!               **********************************************************
!
      ISTEPN='2'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMWD=0
      DO 300 I=1,IWIDTH
        IM1=I-1
        IM2=I-2
        IP1=I+1
!
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(I).EQ.'=')GO TO 350
        IF(IHOST1.EQ.'HONE')THEN
          IF(IANSLC(I).EQ.'>')GO TO 350
          IF(IANSLC(I).EQ.'<')GO TO 350
        ENDIF
!       ADD "<>  " CASE
        IF(I.GT.1.AND.IANSLC(I).EQ.'>'.AND.IANSLC(I-1).EQ.'<')GO TO 300
!
!CCCC   THE FOLLOWING LINE WAS COMMENTED OUT AUGUST 1990
!CCCC   DUE TO BOMB ON HONEYWELL/PRIME WHEN TRYING TO EXECUTE  AUGUST 1990
!CCCC   CALL DATAPLOT>DPSYSF.TEX   (> IS A DIRECTORY SYMBOL   AUGUST 1990
!CCCC   ON HONEYWELL AND PRIME)               AUGUST 1990
!CCCC   IF(IANSLC(I).EQ.'>')GO TO 350
        IF(IANSLC(I).EQ.'<'.AND.IANSLC(I+1).EQ.'>')GO TO 345
!
!CCCC   THE FOLLOWING LINE WAS COMMENTED OUT AUGUST 1990
!CCCC   TO PARALLEL THE COMMENTING OUT FOR    >   2 LINES ABOVE  AUGUST 1990
!CCCC   IF(IANSLC(I).EQ.'<')GO TO 350
!
        IF(IANSLC(I).NE.' '.AND.I.LE.1)GO TO 350
!
        IF(I.LE.1)GO TO 360
        IF(IANSLC(I).NE.' '.AND.IANSLC(IM1).EQ.' ')GO TO 350
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.   &
           IANSLC(IM1).EQ.'=')GO TO 350
        IF(IHOST1.EQ.'HONE')THEN
          IF(IANSLC(I).NE.' '.AND.IANSLC(IM1).EQ.'>')GO TO 350
          IF(IANSLC(I).NE.' '.AND.IANSLC(IM1).EQ.'<')GO TO 350
        ENDIF
!
        IF(I.LE.2)GO TO 360
!
!CCCC   OCTOBER 1997.  CHECK FOR EXPONENTIAL NOTATION,
!CCCC                  I.E., IF "-" IS PRECEDED BY AN "E" AND SUCCEDED
!CCCC                  BY ANUMBER.
!
        IF(IANSLC(IM1).EQ.'-')THEN
          IF(IANSLC(IM2).EQ.'E' .OR. IANSLC(IM2).EQ.'e')THEN
            CALL DPCOAN(IANSLC(I),IJUNK)
            IF(IJUNK.GE.48 .AND. IJUNK.LE.57)GO TO 370
          ENDIF
        ENDIF
!
        IF(IHYPCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.   &
           IANSLC(IM1).EQ.'-')GO TO 340
        IF(ICOMCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.   &
           IANSLC(IM1).EQ.',')GO TO 340
        GO TO 360
!
  340   CONTINUE
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(IM2).EQ.'=')GO TO 360
        IF(IHYPCL.EQ.'ON'.AND.IANSLC(IM2).EQ.'-')GO TO 355
        IF(ICOMCL.EQ.'ON'.AND.IANSLC(IM2).EQ.',')GO TO 355
        IF(IANSLC(IM2).NE.' ')GO TO 350
        GO TO 360
!
!  ADD "<>  " CASE
  345   CONTINUE
        NUMWD=NUMWD+1
        ISTART(NUMWD)=I
        ISTOP(NUMWD)=I+1
        GO TO 390
!       END ADD
  350   CONTINUE
        NUMWD=NUMWD+1
!
  355   CONTINUE
        ISTART(NUMWD)=I
!
  360   CONTINUE
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(I).EQ.'=')GO TO 370
!CCCC   IF(IANSLC(I).EQ.'>')GO TO 370
!CCCC   IF(IANSLC(I).EQ.'<')GO TO 370
        IF(IANSLC(I).NE.' '.AND.I.GE.IWIDTH)GO TO 370
!
        IF(I.GE.IWIDTH)GO TO 390
        IF(IANSLC(I).NE.' '.AND.IANSLC(IP1).EQ.' ')GO TO 370
        IF(IEQUCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.   &
           IANSLC(IP1).EQ.'=')GO TO 370
!CCCC   IF(IANSLC(I).NE.' '.AND.IANSLC(IP1).EQ.'>')GO TO 370
!CCCC   IF(IANSLC(I).NE.' '.AND.IANSLC(IP1).EQ.'<')GO TO 370
        IF(IHYPCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.   &
           IANSLC(IP1).EQ.'-')GO TO 370
        IF(ICOMCL.EQ.'ON'.AND.IANSLC(I).NE.' '.AND.   &
           IANSLC(IP1).EQ.',')GO TO 370
!
        GO TO 390
!
  370   CONTINUE
        ISTOP(NUMWD)=I
!
  390   CONTINUE
!
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
!
  300 CONTINUE
      IF(NUMWD.LE.0)GO TO 9000
!
!               ***********************************************************
!               **  STEP 3--                                             **
!               **  CONVERT THE FIRST STRING TO A COMMAND                **
!               **  EXTRACT THE FIRST 4 CHARACTERS OF                    **
!               **  THE COMMAND.  PACK THESE 4 CHARACTERS                **
!               **  INTO THE HOLLERITH VARIABLE ICOM.                    **
!               **  ONLY 4 CHARACTERS ARE RETAINED                       **
!               **  REGARDLESS OF THE MAX NUMBER OF                      **
!               **  CHARACTERS PER WORD ON A GIVEN                       **
!               **  COMPUTER (E.G., EVEN THOUGH UNIVAC                   **
!               **  COULD RETAIN 6 CHARACTERS PER WORD,                  **
!               **  IT IS SUFFICIENT              TO RETAIN              **
!               **  ONLY 4 CHARACTERS PER WORD--ON A UNIVAC              **
!               **  OR ANY OTHER COMPUTER.                               **
!               **  OR ANY OTHER COMPUTER.                               **
!               **  ALSO, IF THE NUMBER OF CHARACTERS                    **
!               **  IN THE FIRST WORD IS 5 OR MORE,                      **
!               **  THEN PACK CHARACTERS 5 THROUGH 8                     **
!               **  (OR CHARACTERS 5 THROUGH THE END OF THE WORD         **
!               **  IF THE END OF THE WORD IS BEFORE CHARACTER 8)        **
!               **  INTO THE 4-CHARACTER WORD ICOM2.                     **
!               ***********************************************************
!
      ISTEPN='3'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWORD=1
      IWID=ISTOP(IWORD)-ISTART(IWORD)+1
      JMIN=ISTART(IWORD)
      JMAX=ISTOP(IWORD)
      I=0
      DO 800 J=JMIN,JMAX
        I=I+1
        IB(I)=IANSLC(J)
  800 CONTINUE
!
      IANS1='    '
      IANS2='    '
      IMAX=2*NUMASC
      IF(IWID.LT.IMAX)IMAX=IWID
!
      IF(IBUGTY.EQ.'ON')THEN
        WRITE(ICOUT,901)IMAX
  901   FORMAT('IMAX = ',I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 900 I=1,IMAX
        IF(IB(I).EQ.' ')GO TO 910
        IM4=I-4
        IF(I.LE.NUMASC)IANS1(I:I)=IB(I)
        IF(I.GT.NUMASC)IANS2(IM4:IM4)=IB(I)
  900 CONTINUE
  910 CONTINUE
      ICOMLC=IANS1
      ICOML2=IANS2
      CALL DPUPP4(ICOMLC,ICOM,IBUGTY,IERROR)
      CALL DPUPP4(ICOML2,ICOM2,IBUGTY,IERROR)
!
!               ********************************************
!               **  STEP 4--                              **
!               **  CONVERT STRINGS 2 THROUGH END         **
!               **  TO HOLLERITH A4 ARGUMENTS.            **
!               **  IF MORE THAN 8 CHARACTERS,            **
!               **  CONVERT ONLY THE FIRST 8 CHARACTERS   **
!               **  (REGARDLESS OF THE COMPUTER TYPE).    **
!               ********************************************
!
      ISTEPN='4'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMARG=NUMWD-1
      IF(NUMWD.LE.1)GO TO 1999
      DO 1000 IWORD=2,NUMWD
        IWID=ISTOP(IWORD)-ISTART(IWORD)+1
!
        JMIN=ISTART(IWORD)
        JMAX=ISTOP(IWORD)
        I=0
        DO 1100 J=JMIN,JMAX
          I=I+1
          IB(I)=IANSLC(J)
 1100   CONTINUE
!
        IANS1='    '
        IANS2='    '
        IMAX=2*NUMASC
        IF(IWID.LT.IMAX)IMAX=IWID
        DO 1200 I=1,IMAX
          IF(IB(I).EQ.' ')GO TO 1210
          IM4=I-4
          IF(I.LE.NUMASC)IANS1(I:I)=IB(I)
          IF(I.GT.NUMASC)IANS2(IM4:IM4)=IB(I)
 1200   CONTINUE
 1210   CONTINUE
        IWORM1=IWORD-1
        IHARLC(IWORM1)=IANS1
        IHARL2(IWORM1)=IANS2
!
 1000 CONTINUE
 1999 CONTINUE
!
!               **********************************************************
!               **  STEP 4.5--                                          **
!               **  CONVERT EACH ARGUMENT TO UPPER CASE.                **
!               **********************************************************
!
      ISTEPN='4.5'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.0)GO TO 1390
      DO 1300 I=1,NUMARG
        CALL DPUPP4(IHARLC(I),IHARG(I),IBUGTY,IERROR)
        CALL DPUPP4(IHARL2(I),IHARG2(I),IBUGTY,IERROR)
 1300 CONTINUE
 1390 CONTINUE
!
!               **********************************************************
!               **  STEP 5--                                            **
!               **  CONVERT STRINGS 1 THROUGH END TO INTEGER ARGUMENTS  **
!               **********************************************************
!
      ISTEPN='5'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMWD.LE.0)GO TO 2999
      DO 2000 IWORD=1,NUMWD
        IWORM1=IWORD-1
!
        IF(IWORD.LE.1)THEN
          IH=ICOM
          IH2=ICOM2
        ELSE
          IH=IHARG(IWORM1)
          IH2=IHARG2(IWORM1)
        ENDIF
!
        IF(NUMNAM.LE.0)GO TO 2040
        DO 2010 INAME=1,NUMNAM
          IF(IH.EQ.IHNAME(INAME).AND.IH2.EQ.IHNAM2(INAME))THEN
            IF(IUSE(INAME).EQ.'P')THEN
              IF(IWORM1.GT.0)IARGT(IWORM1)='NUMB'
              IF(IWORM1.GT.0)IARG(IWORM1)=IVALUE(INAME)
              GO TO 2000
            ELSE
              GO TO 2040
            ENDIF
          ENDIF
 2010   CONTINUE
 2040   CONTINUE
!
        IFLUNK='NO'
        IANS3=(-1)
        IWID=ISTOP(IWORD)-ISTART(IWORD)+1
        JMIN=ISTART(IWORD)
        JMAX=ISTOP(IWORD)
        I=0
        DO 2100 J=JMIN,JMAX
          I=I+1
          IB(I)=IANSLC(J)
 2100   CONTINUE
!
        IDIG=0
        ISIGN=0
        IDECP2=0
        ISUM=0
        DO 2700 I=1,IWID
          IREV=IWID-I+1
          IF(IB(IREV).EQ.' ')THEN
            GO TO 2700
          ELSEIF(IB(IREV).EQ.'0')THEN
            ITERM=0
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'1')THEN
            ITERM=1
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'2')THEN
            ITERM=2
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'3')THEN
            ITERM=3
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'4')THEN
            ITERM=4
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'5')THEN
            ITERM=5
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'6')THEN
            ITERM=6
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'7')THEN
            ITERM=7
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'8')THEN
            ITERM=8
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'9')THEN
            ITERM=9
            GO TO 2725
          ELSEIF(IB(IREV).EQ.'+')THEN
            ISIGN=ISIGN+1
            GO TO 2700
          ELSEIF(IB(IREV).EQ.'-')THEN
            ISIGN=ISIGN+1
            ISUM=-ISUM
            GO TO 2700
          ELSEIF(IB(IREV).EQ.'.')THEN
            IDECP2=IDECP2+1
            IF(IDECP2.EQ.1.AND.IDIG.EQ.0)GO TO 2700
            GO TO 2800
          ELSE
            IFLUNK='YES'
            GO TO 2800
          ENDIF
!
 2725     CONTINUE
          IDIG=IDIG+1
          IF(IDIG.EQ.1)THEN
            ISUM=ISUM+ITERM
          ELSE
!CCCC       FOLLOWING FIXES WHAT APPEARS TO BE COMPILER BUG ON LAHEY 95
!CCCC       COMPILER.  MAY 2001
!CCCC       SPECIFICALLY, 10**IPOW SEEMS TO RETURN A 0.
!CCCC       ISUM=ISUM+ITERM*10**(IDIG-1)
            ITERM1=IDIG-1
            ITERM2=INT(10.0**ITERM1 + 0.01)
            ISUM=ISUM+ITERM*ITERM2
          ENDIF
!
 2700   CONTINUE
        IF(IDIG.LE.0)GO TO 2800
        IF(ISIGN.GE.2)GO TO 2800
        IANS3=ISUM
!
 2800   CONTINUE
        IWORM1=IWORD-1
        IF(IWORD.LE.1)ICOMI=IANS3
        IF(IWORD.GE.2)IARG(IWORM1)=IANS3
        IF(IWORD.LE.1.AND.IFLUNK.EQ.'YES')ICOMT='WORD'
        IF(IWORD.GE.2.AND.IFLUNK.EQ.'YES')IARGT(IWORM1)='WORD'
 2000 CONTINUE
 2999 CONTINUE
!
!               ***************************************************************
!               **  STEP 6--                                                 **
!               **  CONVERT STRINGS 2 THROUGH N TO FLOATING POINT ARGUMENTS  **
!               ***************************************************************
!
      ISTEPN='6'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ************************************************************
!               **  STEP 6.1--                                            **
!               **  FIRST OF ALL, LOCATE THE DECIMAL POINT (IF EXISTENT)  **
!               **  OCTOBER 1997.  CHECK FOR EXPONENTIAL NOTATION.   I.E. **
!               **  1.2E02, 1.2E-02, 1.2E+02                              **
!               ************************************************************
!
      ISTEPN='6.1'
      IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC OCTOBER 1997.  FOR EXPONENTIAL NOTATION, NEED TO ALLOW LARGER NUMBERS
!CCCC AMIN=-1000000.
!CCCC AMAX=+1000000.
      AMIN=CPUMIN
      AMAX=CPUMAX
      NUMARG=NUMWD-1
!CCCC IF(NUMARG.LE.0)GO TO 3999
      IF(NUMWD.LE.0)GO TO 3999
      DO 3000 IWORD=1,NUMWD
!
        IWORM1=IWORD-1
        IF(IWORD.LE.1)THEN
          IH=ICOM
          IH2=ICOM2
        ELSE
          IH=IHARG(IWORM1)
          IH2=IHARG2(IWORM1)
        ENDIF
!
        IF(NUMNAM.LE.0)GO TO 3040
        DO 3010 INAME=1,NUMNAM
          IF(IH.EQ.IHNAME(INAME).AND.IH2.EQ.IHNAM2(INAME))THEN
            IF(IUSE(INAME).EQ.'P')THEN
              IF(IWORD.LE.1)ICOMT='NUMB'
              IF(IWORD.GE.2)IARGT(IWORM1)='NUMB'
              IF(IWORD.LE.1)ACOM=VALUE(INAME)
              IF(IWORD.GE.2)ARG(IWORM1)=VALUE(INAME)
              GO TO 3000
            ELSE
              GO TO 3040
            ENDIF
          ENDIF
 3010   CONTINUE
 3040   CONTINUE
!
        IFLUNK='NO'
        ANS2=(-1.0)
        IWID=ISTOP(IWORD)-ISTART(IWORD)+1
        JMIN=ISTART(IWORD)
        JMAX=ISTOP(IWORD)
        I=0
        DO 3050 J=JMIN,JMAX
          I=I+1
          IB(I)=IANSLC(J)
 3050   CONTINUE
!
        ILOC=0
        IDECP2=0
        ILOCE=0
        IEXPPT=0
        DO 3060 I=1,IWID
          IF(IB(I).EQ.'.')ILOC=I
          IF(IB(I).EQ.'.')IDECP2=IDECP2+1
          IF(IB(I).EQ.'E'.OR.IB(I).EQ.'e')ILOCE=I
          IF(IB(I).EQ.'E'.OR.IB(I).EQ.'e')IEXPPT=IEXPPT+1
 3060   CONTINUE
        IF(IDECP2.GE.2)GO TO 3900
        IF(IEXPPT.GE.2)GO TO 3900
!
        IESCAL=0
        IESIGN=1
        IWID2=IWID
        IF(ILOCE+1.GT.IWID)THEN
          IFLUNK='YES'
          GO TO 3900
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
            GO TO 3900
          ENDIF
          DO 13065 I=ISTRT2,IWID2
            IF(IB(I).EQ.' ')GO TO 13065
            IF(IB(I).EQ.'0')GO TO 13060
            IF(IB(I).EQ.'1')GO TO 13060
            IF(IB(I).EQ.'2')GO TO 13060
            IF(IB(I).EQ.'3')GO TO 13060
            IF(IB(I).EQ.'4')GO TO 13060
            IF(IB(I).EQ.'5')GO TO 13060
            IF(IB(I).EQ.'6')GO TO 13060
            IF(IB(I).EQ.'7')GO TO 13060
            IF(IB(I).EQ.'8')GO TO 13060
            IF(IB(I).EQ.'9')GO TO 13060
            IFLUNK='YES'
            GO TO 3900
!
13060       CONTINUE
            ICOUNT=ICOUNT+1
            ICJUNK(ICOUNT:ICOUNT)=IB(I)(1:1)
!
13065     CONTINUE
!CCCC     FOLLOWING TO ADDRESS BUG ON SUN.  OCTOBER 2001.
          IFRMT(1:5)='(I  )'
          IF(ICOUNT.LE.9)THEN
            WRITE(IFRMT(3:3),'(I1)')ICOUNT
          ELSE
            WRITE(IFRMT(3:4),'(I2)')ICOUNT
          ENDIF
          READ(ICJUNK(1:ICOUNT),IFRMT)IESCAL
        ENDIF
!
        IF(IDECP2.EQ.1)GO TO 3150
        DO 3100 I=1,IWID
          IREV=IWID-I+1
          IF(IB(IREV).EQ.' ')GO TO 3100
          IF(IB(IREV).EQ.'0' .OR. IB(IREV).EQ.'1' .OR.   &
                 IB(IREV).EQ.'2' .OR. IB(IREV).EQ.'3' .OR.   &
                 IB(IREV).EQ.'4' .OR. IB(IREV).EQ.'5' .OR.   &
                 IB(IREV).EQ.'6' .OR. IB(IREV).EQ.'7' .OR.   &
                 IB(IREV).EQ.'8' .OR. IB(IREV).EQ.'9')THEN
            GO TO 3110
          ENDIF
          IFLUNK='YES'
          IF(IB(IREV).EQ.'+')GO TO 3900
          IF(IB(IREV).EQ.'-')GO TO 3900
          GO TO 3900
!
 3100   CONTINUE
        IFLUNK='YES'
        GO TO 3900
!
 3110   ILOC=IREV+1
 3150   CONTINUE
!
        IF(IBUGTY.NE.'OFF')THEN
          WRITE(ICOUT,3111)ILOC,IDECP2
 3111     FORMAT('ILOC = ',I8,'    IDECP2 = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *******************************************************
!               **  STEP 6.2--                                       **
!               **  SECONDLY, COMPUTE THE INTEGER PART OF THE VALUE  **
!               *******************************************************
!
        ISTEPN='6.2'
        IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        SIGN=1.0
        IDIGI=0
        ISIGN=0
        SUMI=0
        ILOCM1=ILOC-1
        IF(ILOCM1.LT.1)GO TO 3250
        DO 3200 I=1,ILOCM1
          IREV=ILOCM1-I+1
          IF(IB(IREV).EQ.' ')THEN
            GO TO 3200
          ELSEIF(IB(IREV).EQ.'0')THEN
            ITERM=0
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'1')THEN
            ITERM=1
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'2')THEN
            ITERM=2
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'3')THEN
            ITERM=3
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'4')THEN
            ITERM=4
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'5')THEN
            ITERM=5
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'6')THEN
            ITERM=6
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'7')THEN
            ITERM=7
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'8')THEN
            ITERM=8
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'9')THEN
            ITERM=9
            GO TO 3225
          ELSEIF(IB(IREV).EQ.'+')THEN
            ISIGN=ISIGN+1
            GO TO 3200
          ELSEIF(IB(IREV).EQ.'-')THEN
            ISIGN=ISIGN+1
            SIGN=-SIGN
            GO TO 3200
          ELSE
            IFLUNK='YES'
            GO TO 3900
          ENDIF
!
 3225     CONTINUE
          IDIGI=IDIGI+1
          TERM=ITERM
          IEXP=IDIGI-1
          SUMI=SUMI+TERM*(10.0          **IEXP)
 3200   CONTINUE
 3250   CONTINUE
        IF(ISIGN.GE.2)GO TO 3900
!
        IF(IBUGTY.NE.'OFF')THEN
          WRITE(ICOUT,3255)IDIGI,SUMI
 3255     FORMAT('IDIGI = ',I8,'    SUMI = ',F20.10)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ******************************************************
!               **  STEP 6.3--                                      **
!               **  THIRDLY, COMPUTE THE DECIMAL PART OF THE VALUE  **
!               ******************************************************
!
        ISTEPN='6.3'
        IF(IBUGTY.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IDIGD=0
        SUMD=0.0
        ILOCP1=ILOC+1
        IF(ILOCP1.GT.IWID)GO TO 3350
        DO 3300 I=ILOCP1,IWID
          IF(IB(I).EQ.' ')THEN
            GO TO 3300
          ELSEIF(IB(I).EQ.'0')THEN
            ITERM=0
            GO TO 3325
          ELSEIF(IB(I).EQ.'1')THEN
            ITERM=1
            GO TO 3325
          ELSEIF(IB(I).EQ.'2')THEN
            ITERM=2
            GO TO 3325
          ELSEIF(IB(I).EQ.'3')THEN
            ITERM=3
            GO TO 3325
          ELSEIF(IB(I).EQ.'4')THEN
            ITERM=4
            GO TO 3325
          ELSEIF(IB(I).EQ.'5')THEN
            ITERM=5
            GO TO 3325
          ELSEIF(IB(I).EQ.'6')THEN
            ITERM=6
            GO TO 3325
          ELSEIF(IB(I).EQ.'7')THEN
            ITERM=7
            GO TO 3325
          ELSEIF(IB(I).EQ.'8')THEN
            ITERM=8
            GO TO 3325
          ELSEIF(IB(I).EQ.'9')THEN
            ITERM=9
            GO TO 3325
          ELSE
            IFLUNK='YES'
            GO TO 3900
          ENDIF
!
 3325     IDIGD=IDIGD+1
          TERM=ITERM
          SUMD=SUMD+TERM/(10.0**IDIGD)
!
 3300   CONTINUE
 3350   CONTINUE
!
        IF(IBUGTY.EQ.'ON')THEN
          WRITE(ICOUT,3355)IDIGD,SUMD
 3355     FORMAT('IDIGD = ',I8,'    SUMD = ',F20.10)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IDIGT=IDIGI+IDIGD
        IF(IDIGT.LE.0)GO TO 3900
        ANS2=SUMI+SUMD
        IF(SIGN.LT.0.0)ANS2=-ANS2
        ANS2=ANS2*10.0**(IESIGN*IESCAL)
        IWORM1=IWORD-1
        IF(IWORD.LE.1)ACOM=ANS2
        IF(IWORD.GE.2)ARG(IWORM1)=ANS2
!CCC    OCTOBER 1997.  IF EXPONENTIAL NUMBER, NEED TO RESET IARGT
        IF(AMIN.LE.ANS2.AND.ANS2.LE.AMAX)THEN
          IF(IWORM1.GE.1)IARGT(IWORM1)='NUMB'
          GO TO 3000
        ELSE
          GO TO 3900
        ENDIF
!
 3900   CONTINUE
        IF(IWORM1.LT.1) GO TO  3000
        IWORM1=IWORD-1
        ARG(IWORM1)=ANS2
        IF(IFLUNK.EQ.'YES')IARGT(IWORM1)='WORD'
 3000 CONTINUE
 3999 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGTY.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTYPE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICOM,ICOM2,ICOMT,ACOM,ICOMI
 9012   FORMAT('ICOM,ICOM2,ICOMT,ACOM,ICOMI = ',   &
               2(A4,2X),A4,E15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ICOMLC,ICOML2,NUMARG
 9013   FORMAT('ICOMLC,ICOML2,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NUMARG
          WRITE(ICOUT,9016)I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I)
 9016     FORMAT('I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I) = ',   &
                 I6,2(1X,A4),1X,I6,1X,E15.7,1X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9017)I,IHARLC(I),IHARL2(I)
 9017     FORMAT('I,IHARLC(I),IHARL2(I) = ',I6,1X,A4,1X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9021)IHOST1,IHOST2
 9021   FORMAT('IHOST1,IHOST2 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPTYPE

C  THIS FILE CONTAINS THE FOLLOWING:
C
C     1. DPFIT   - DRIVER FOR FIT COMMAND
C
C        DPFIT2  - NON-LINEAR FIT
C        DPFIT3  - LINEAR FIT (ROUTINES FROM OMNITAB)
C        LSQRT
C        LSQ
C        SCALDP
C        PDECOM
C        SLVE
C        DSUMAL
C        SDPRED
C        PINVRT
C        DPDIV
C        SPDIV
C        DPCON
C        DPSQRT
C        SPSQRT
C        SPLO10
C        IDIV
C
C     2. BACK    - BEST SUBSETS FOR LINEAR FITS (ROUTINES FROM OMNITAB)
C        CODEXY
C        COEF
C        CPSTRE
C        CRSPRD
C        FDDIV
C        FDIV
C        FDPCON
C        FDSQRT
C        FLOG10
C        PIVOT
C        RFORMT
C        SCREEN
C
C     3. ACM591  - ANOVA ROUTINES FROM ACM 591.  NOTE THAT THESE
C        DECOMP    ARE NOT CURRENTLY IMPLEMENTED BY DATAPLOT'S 
C        SCAN      ANOVA COMMAND.  INCLUDED FOR FUTURE IMPLEMENTATION.
C        STEP
C        PART1
C        PART2
C        POOL
C        IGET
C        LABEL
C
C     4. SNSQE   - NON-LINEAR EQUATIONS, SINGLE PRECISION (FROM CMLIB)
C        SNSQ
C        FDJAC1
C        QRFAC
C        QFORM
C        DOGLEG
C        R1UPDT
C        R1MPYQ
C
C     5. DNSQE   - NON-LINEAR EQUATIONS, DOUBLE PRECISION (FROM CMLIB)
C        DNSQ
C        DFDJC1
C        DQRFAC
C        DENORM
C        DQFORM
C        DDOGLG
C        D1UPDT
C        D1MPYQ
C
      SUBROUTINE DPFIT(ICAPSW,IFORSW,
     1                 IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,
     1                 IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A LEAST SQUARES FIT
C              FOR LINEAR AND NON-LINEAR MODELS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/2
C      FIX IN HERE
C     ORIGINAL VERSION--FEBRUARY 1988.
C     UPDATED         --FEBRUARY 1988.  (SIMPLIFY THE CALL TO DPFIT3)
C     UPDATED         --MARCH    1988.  (ALLOW B0 IN MULTILINEAR FIT)
C     UPDATED         --MARCH    1988.  ADD LOFCDF
C     UPDATED         --MAY      1989.  ALLOW OMNITAB FIT BEYOND 5 VAR.
C     UPDATED         --MAY      1989.  ADDED ISUBRO IN CALL TO DPFIT3
C     UPDATED         --MAY      1989.  AUTO COEF--A11, A12, A13, ...
C     UPDATED         --AUGUST   1989.  NUMPAR FIXED FOR POLY FIT
C     UPDATED         --JUNE     1990.  TEMPORARY ARRAYS TO GARBAGE COMMON
C                                       ALSO, MOVE SOME DIMENSIONS FROM DPFIT2
C                                       AND DPFIT3 TO DPFIT
C     UPDATED         --JUNE     1991.  REPLICATION BUG FOR POLY FIT
C     UPDATED         --SEPT     1991.  EXPAND IND. VAR. 5 TO 15
C     UPDATED         --MARCH    1992.  FIX INSTAB. MESSAGE (WEIGHTS)
C     UPDATED         --MARCH    1992.  ISUBRO ADDED TO DPFIT2 ARG LIST
C     UPDATED         --MAY      1995.  FIX SOME I/O
C     UPDATED         --MAY      1995.  ADDITIONAL EQUIVALENCE
C     UPDATED         --APRIL    2002.  OPTION TO OMIT CONSTANT TERM
C                                       FOR MULTILINEAR FIT
C     UPDATED         --JULY     2003.  MODIFY STORAGE FOR LINEAR FIT
C                                       SO THAT > MAXCMF DEPENDENT
C                                       VARIABLES CAN BE USED (I.E.,
C                                       ADD VARIABLES AT EXPENSE OF
C                                       FEWER ROWS)
C     UPDATED         --NOVEMBER 2003.  CAPTURE HTML AND LATEX FORMATS
C     UPDATED         --MAY      2009.  WITH THE INCREASED DATA SET
C                                       SIZE ALLOWED, THE DPSWAP ROUTINE
C                                       WAS BECOMING A SERIOUS BOTTLE
C                                       NECK IN SOME CASES.  USE
C                                       DPCOZD.INC IN PLACE OF DPSWAP
C     UPDATED         --NOVEMBER 2016.  SET FIT ADDITIVE CONSTANT FOR
C                                       POLYNOMIAL FITS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASFI
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASEQ
      CHARACTER*4 IKEY
      CHARACTER*4 IWD
      CHARACTER*4 IWD1
      CHARACTER*4 IWD2
      CHARACTER*4 IWD12
      CHARACTER*4 IWD22
      CHARACTER*4 IHPARN
      CHARACTER*4 IHPAR2
      CHARACTER*4 IPAROC
      CHARACTER*4 IPARO3
      CHARACTER*4 ICH
      CHARACTER*4 IOP
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW2HOL
      CHARACTER*4 IW22HO
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IPARN3
      CHARACTER*4 IPARN4
      CHARACTER*4 IVARN3
      CHARACTER*4 IVARN4
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IREP
C
CCCCC THE FOLLOWING 5 LINES WERE ADDED MAY 1989
      CHARACTER*4 IHOUT
      CHARACTER*4 IVALID
      CHARACTER*4 IHOUT1
      CHARACTER*4 IHOUT2
      CHARACTER*4 IHOUT3
C
CCCCC THE FOLLOWING 2 LINES WERE ADDED MAY 1989
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
C
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN0
C
C---------------------------------------------------------------------
C
CCCCC JULY 2003: MAKE MAXIMUM NUMBER OF PARAMETERS SETTABLE VIA
CCCCC SINGLE PARAMETER STATEMENT.
C
      PARAMETER(MAXPAR=300)
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHO.INC'
C
      DIMENSION IPAROC(MAXPAR)
C
CCCCC DIMENSION ITYPEH(225)
CCCCC DIMENSION IW2HOL(225)
CCCCC DIMENSION IW22HO(225)
CCCCC DIMENSION W2HOLD(225)
      DIMENSION ITYPEH(1000)
      DIMENSION IW2HOL(1000)
      DIMENSION IW22HO(1000)
      DIMENSION W2HOLD(1000)
C
      DIMENSION PARAM(MAXPAR)
      DIMENSION IPARN(MAXPAR)
      DIMENSION IPARN2(MAXPAR)
C
      DIMENSION X1(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION X4(MAXOBV)
      DIMENSION X5(MAXOBV)
CCCCC THE FOLLOWING 10 LINES WERE ADDED SEPTEMBER 1991
CCCCC (NO EQUIVALENCE DONE)             SEPTEMBER 1991
      DIMENSION X6(MAXOBV)
      DIMENSION X7(MAXOBV)
      DIMENSION X8(MAXOBV)
      DIMENSION X9(MAXOBV)
      DIMENSION X10(MAXOBV)
      DIMENSION X11(MAXOBV)
      DIMENSION X12(MAXOBV)
      DIMENSION X13(MAXOBV)
      DIMENSION X14(MAXOBV)
      DIMENSION X15(MAXOBV)
C
      DIMENSION W(MAXOBV)
      DIMENSION VSDPRD(MAXOBV)
C
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
C
      DIMENSION VSCRT(10*MAXOBV)
C
CCCCC THE FOLLOWING LINE WAS INSERTED MAY 1989
CCCCC JULY 2003: MAKE 1D ARRAY TO ALLOW MORE FLEXIBILITY BETWEEN
CCCCC ALLOCATION OF ROWS AND COLUMNS.
C
CCCCC DIMENSION XMAT(MAXOBV,MAXCMF)
      DIMENSION XMAT(MAXOBV*MAXCMF)
      DIMENSION PARCOV(MAXPAR+1,MAXPAR+1)
C
      DIMENSION PARAM3(MAXPAR)
      DIMENSION IPARN3(MAXPAR)
      DIMENSION IPARN4(MAXPAR)
      DIMENSION ICON3(MAXPAR)
      DIMENSION IPARO3(MAXPAR)
      DIMENSION PARLI3(MAXPAR)
      DIMENSION IVARN3(MAXPAR)
      DIMENSION IVARN4(MAXPAR)
      DIMENSION ICOLV3(MAXPAR)
      DIMENSION NIV(MAXPAR)
C
      DIMENSION ICH(10)
C
CCCCC THE FOLLOWING LINE WAS ADDED MAY 1989
      DIMENSION IHOUT(10)
C
CCCCC THE FOLLOWING 2 LINES WERE ADDED MAY 1989
      DIMENSION IVARN1(MAXPAR)
      DIMENSION IVARN2(MAXPAR)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      EQUIVALENCE (W(1),X3D(1))
      EQUIVALENCE (PRED2(1),X(1))
      EQUIVALENCE (RES2(1),D(1))
      EQUIVALENCE (DFILL(1),VSDPRD(1))
C
CCCCC FOLLOWING LINES ADDED JUNE, 1990.  THE DUMMY ARRAYS ARE ONES THAT
CCCCC WERE PREVIOUSLY DIMENSIONED IN DPFIT2 AND DPFIT3.  DIMENSIONING MOVED
CCCCC HERE TO ALLOW EQUIVALENE TO GARBAGE ARRAY (THEY ARE NAMED CORRECTLY
CCCCC IN THE RECIEVING ROUTINES).
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZ2.INC'
      INCLUDE 'DPCOZD.INC'
      DIMENSION DUMMY1(MAXOBV)
      DIMENSION DUMMY2(MAXOBV)
      DIMENSION DUMMY3(MAXOBV)
      DIMENSION DUMMY4(MAXOBV)
      DIMENSION DUMMY5(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),X2(1))
      EQUIVALENCE (GARBAG(IGARB3),X3(1))
      EQUIVALENCE (GARBAG(IGARB4),X4(1))
      EQUIVALENCE (GARBAG(IGARB5),X5(1))
      EQUIVALENCE (GARBAG(IGARB6),DUMMY1(1))
      EQUIVALENCE (GARBAG(IGARB7),DUMMY2(1))
      EQUIVALENCE (GARBAG(IGARB8),DUMMY3(1))
      EQUIVALENCE (GARBAG(IGARB9),DUMMY4(1))
      EQUIVALENCE (GARBAG(IGAR10),DUMMY5(1))
CCCCC MAY 1995. ADD FOLLOWING 10 LINES
      EQUIVALENCE (GARBAG(JGAR11),X6(1))
      EQUIVALENCE (GARBAG(JGAR12),X7(1))
      EQUIVALENCE (GARBAG(JGAR13),X8(1))
      EQUIVALENCE (GARBAG(JGAR14),X9(1))
      EQUIVALENCE (GARBAG(JGAR15),X10(1))
      EQUIVALENCE (GARBAG(JGAR16),X11(1))
      EQUIVALENCE (GARBAG(JGAR17),X12(1))
      EQUIVALENCE (GARBAG(JGAR18),X13(1))
      EQUIVALENCE (GARBAG(JGAR19),X14(1))
      EQUIVALENCE (GARBAG(JGAR20),X15(1))
C
CCCCC EQUIVALENCE (G2RBAG(IGAR11),XMAT(1,1))
      EQUIVALENCE (G2RBAG(IGAR11),XMAT(1))
      EQUIVALENCE (DGARBG(IDGAR1),VSCRT(1))
CCCCC END CHANGE
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
      ISUBN1='DPFI'
      ISUBN2='T   '
C
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IPAROC(1)='NONE'
C
CCCCC THE FOLLOWING LINE WAS CHANGED SEPTEMBER 1991
CCCCC MAXV2=5
      MAXV2=15
      MINN2=2
C
      MAXITS=IFITIT
      CPUEPS=R1MACH(3)
C
      MAXN2=MAXCHF
      MAXN3=MAXCHF
      MAXN4=MAXCHF
C
      NUMPV=(-999)
      IP=(-999)
      IV=(-999)
      IWIDMO=(-999)
      NUMIND=(-999)
C
CCCCC CUTOFF=2**(NUMBPW-3)
      ICUTMX=NUMBPW
      IF(IHOST1.EQ.'CDC '.OR.IHOST1.EQ.'CYBE')ICUTMX=48
      IF(IHOST1.EQ.'205 ')ICUTMX=48
      CUTOFF=2**(ICUTMX-3)
C
C
C               **************************
C               **  TREAT THE FIT CASE  **
C               **************************
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IFITAC,IBUGA2,IBUGA3,NUMNAM
   53   FORMAT('IFITAC,IBUGA2,IBUGA3,NUMNAM = ',3(A4,A4),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGCO,IBUGEV,IBUGQ
   54   FORMAT('IBUGCO,IBUGEV,IBUGQ = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO57I=1,NUMNAM
          WRITE(ICOUT,58)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I),
     1                   VALUE(I)
   58     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I)',
     1           'VALUE(I) = ',I8,2X,A4,A4,2X,A4,2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL CKFIT(ICASFI,ILOCFI,IBUGA3,IFOUND,IERROR)
      IF(ICASFI.EQ.'    '.OR.IFOUND.EQ.'NO')GOTO9000
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1            IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 3--
C               **  FOR THE CASES WHEN HAVE FIT Y = SOME EXPRESSION
C               **                   ROBUST FIT Y = SOME EXPRESSION,
C               **  DETERMINE IF WE HAVE A VALID FUNCTIONAL EXPRESSION--
C               **  IN PARTICULAR, CHECK THAT THE NUMBER OF ARGUMENTS
C               **  IS AT LEAST 1,
C               **  AND ALSO CHECK THAT THERE IS EXACTLY 1 EQUAL SIGN
C               **  AND THAT THIS EQUAL SIGN OCCURS AS THE SECOND ARGUMENT.
C               ******************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LT.1)THEN
        WRITE(ICOUT,2001)
 2001   FORMAT('***** ERROR IN DPFIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2002)
 2002   FORMAT('      NUMBER OF ARGUMENTS DETECTED = 0.  NUMARG = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2007)
 2007   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
           WRITE(ICOUT,2008)(IANS(J),J=1,MIN(100,IWIDTH))
 2008      FORMAT('      COMMAND LINE--',100A1)
           CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO2100J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GOTO2110
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GOTO2110
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GOTO2110
 2100 CONTINUE
      ILOCQ=NUMARG+1
      GOTO2120
 2110 CONTINUE
      ILOCQ=J1
      GOTO2120
 2120 CONTINUE
C
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        NUMEQ=0
        IMAX=ILOCQ-1
        DO2130I=1,IMAX
          IF(IHARG(I).EQ.'=   '.AND.IHARG2(I).EQ.'    ')NUMEQ=NUMEQ+1
 2130   CONTINUE
        IF(NUMEQ.NE.1)THEN
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2132)
 2132     FORMAT('      THE NUMBER OF EQUAL SIGNS DETECTED, ',I6,
     1           ', IN MODEL NOT EQUAL 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2134)NUMARG,IMAX
 2134     FORMAT('      NUMARG, IMAX = ',2I10)
          CALL DPWRST('XXX','BUG ')
          DO2135I=1,NUMARG
            WRITE(ICOUT,2136)I,IHARG(I),IHARG2(I)
 2136       FORMAT('I,IHARG(I),IHARG2(I) = ',I8,2A4)
            CALL DPWRST('XXX','BUG ')
 2135     CONTINUE
          WRITE(ICOUT,2007)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,2008)(IANS(J),J=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ICASFI.EQ.'FIT'.AND.IHARG(2).NE.'=')GOTO2200
      IF(ICASFI.EQ.'RFIT'.AND.IHARG(3).NE.'=')GOTO2200
      GOTO2290
C
 2200 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2202)
 2202 FORMAT('      WHEN FITTING GENERAL EXPRESSIONS, THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2203)
 2203 FORMAT('      SECOND ARGUMENT AFTER THE WORD     FIT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2204)
 2204 FORMAT('      SHOULD BE (BUT WAS NOT) AN EQUAL SIGN.')
      CALL DPWRST('XXX','BUG ')
      IF(ICASFI.EQ.'FIT')THEN
        WRITE(ICOUT,2205)IHARG(2),IHARG2(2)
 2205   FORMAT('     THE ARGUMENT WAS ',2A4)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASFI.EQ.'RFIT')THEN
        WRITE(ICOUT,2205)IHARG(3),IHARG2(3)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      WRITE(ICOUT,2007)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,2008)(IANS(J),J=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
 2290 CONTINUE
C
C               ******************************************************
C               **  STEP 4--                                        **
C               **  FOR ALL VARIATIONS OF THE FIT COMMAND,          **
C               **  THE WORD AFTER     FIT     SHOULD BE THE RESPONSE*
C               **  VARIABLE (= THE DEPENDENT VARIABLE).            **
C               **  EXTRACT THE RESPONSE VARIABLE AND DETERMINE     **
C               **  IF IT IS ALREADY IN THE NAME LIST AND IS, IN FACT,*
C               **  A VARIABLE (AS OPPOSED TO A PARAMETER).         **
C               ******************************************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I2=0
C
      IF(ICASFI.EQ.'RFIT')THEN
        IMAX=ILOCQ-1
        DO2330I=1,IMAX
          I2=I
          IF(IHARG(I).EQ.'FIT')GOTO2349
 2330   CONTINUE
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2332)
 2332   FORMAT('      THE WORD    FIT   NOT FOUND IN THE ARGUMENT LIST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3334)
 3334   FORMAT('      EVEN THOUGH IT HAD BEEN PREVIOUSLY FOUND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2335)NUMARG,IMAX
 2335   FORMAT('      NUMARG, IMAX = ',2I10)
        CALL DPWRST('XXX','BUG ')
        DO2336I=1,NUMARG
          WRITE(ICOUT,2337)I,IHARG(I),IHARG2(I)
 2337     FORMAT('I,IHARG(I),IHARG2(I) = ',I8,A4,A4)
          CALL DPWRST('XXX','BUG ')
 2336   CONTINUE
        WRITE(ICOUT,2007)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2008)(IANS(J),J=1,IWIDTH)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
 2349 CONTINUE
      ILOCFI=I2
C
      ILOCF1=ILOCFI+1
      IHLEFT=IHARG(ILOCF1)
      IHLEF2=IHARG2(ILOCF1)
      DO2350I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2).AND.
     1     IUSE(I2).EQ.'V')GOTO2379
 2350 CONTINUE
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2362)
 2362 FORMAT('      THE NAME FOLLOWING THE WORD     FIT    (WHICH ',
     1       'SHOULD BE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2363)
 2363 FORMAT('      THE RESPONSE VARIABLE) EITHER DOES NOT EXIST OR ',
     1       'IS A')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2366)
 2366 FORMAT('      PARAMETER (AS OPPOSED TO A VARIABLE) IN THE ',
     1       'CURRENT LIST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2367)
 2367 FORMAT('      OF AVAILABLE VARIABLE AND PARAMETER NAMES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2369)IHLEFT,IHLEF2
 2369 FORMAT('      NAME AFTER THE WORD      FIT = ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2007)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,2008)(IANS(J),J=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
 2379 CONTINUE
      ILOCV=I2
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
 2390 CONTINUE
C
C               *******************************************************
C               **  STEP 5--                                         **
C               **  FOR ALL VARIATIONS OF THE FIT COMMAND,           **
C               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)
C               **  FOR THE RESPONSE VARIABLE IS 2 OR LARGER.        **
C               *******************************************************
C
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NLEFT.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)IHLEFT,IHLEF2
  312   FORMAT('      THE NUMBER OF OBSERVATIONS IN VARIABLE ',A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,313)
  313   FORMAT('      (FOR WHICH A LEAST-SQUARES FIT WAS TO HAVE BEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,315)MINN2
  315   FORMAT('      PERFORMED) MUST BE ',I8,' OR LARGER;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,316)
  316   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,317)NLEFT
  317   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS NLEFT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,318)
  318   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2007)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2008)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ************************************************
C               **  STEP 5.1--                                **
C               **  CHECK TO SEE IF HAVE A WEIGHTS VARIABLE.  **
C               **  IF DO HAVE, CHECK TO SEE IF A VARIABLE    **
C               **  (AS OPPOSED TO A PARAMETER).              **
C               ************************************************
C
      ISTEPN='5.1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCW=-99
      ICOLW=-99
      NWEIGH=-99
      IF(IWEIGH.EQ.'ON')THEN
        DO2450I=1,NUMNAM
          I2=I
          IF(IWEIG1.EQ.IHNAME(I2).AND.IWEIG2.EQ.IHNAM2(I2).AND.
     1       IUSE(I2).EQ.'V')GOTO2479
 2450   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2463)
 2463   FORMAT('      THE WEIGHTS VARIABLE (AS SPECIFIED VIA THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2464)
 2464   FORMAT('      WEIGHTS COMMAND) EITHER DOES NOT EXIST, OR IS A')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2466)
 2466   FORMAT('      PARAMETER (AS OPPOSED TO A VARIABLE) IN THE ',
     1         'CURRENT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2467)
 2467   FORMAT('      LIST OF AVAILABLE VARIABLE AND PARAMETER NAMES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2469)IWEIG1,IWEIG2
 2469   FORMAT('      NAME OF SPECIFIED WEIGHTS VARIABLE = ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2007)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2008)(IANS(J),J=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
 2479 CONTINUE
      ILOCW=I2
      ICOLW=IVALUE(ILOCW)
      NWEIGH=IN(ILOCW)
 2490 CONTINUE
C
C               ****************************************************************
C               **  STEP 6.1--
C               **  FOR THE CASES WHEN HAVE FIT Y = SOME EXPRESSION
C               **                   ROBUST FIT Y = SOME EXPRESSION       ,
C               **  EXTRACT THE ENTIRE (LEFT AND RIGHT SIDE) FUNCTIONAL
C               **  EXPRESSION FROM THE INPUT COMMAND LINE.
C               **  COPY OUT TO IWIDTH, OR OUT TO 'SUBS' (EXCLUSIVE),
C               **  OR OUT THE 'EXCE' (EXCLUSIVE)
C               **  OR OUT THE 'FOR' (EXCLUSIVE).
C               ****************************************************************
C
      ISTEPN='6.1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        IF(NUMARG.EQ.0)GOTO4160
        IF(IHARG(1).EQ.'SUBS'.AND.IHARG2(1).EQ.'ET  ')GOTO4160
        IF(IHARG(1).EQ.'EXCE'.AND.IHARG2(1).EQ.'PT  ')GOTO4160
        IF(IHARG(1).EQ.'FOR '.AND.IHARG2(1).EQ.'    ')GOTO4160
        ISTART=-99
        ISTOP=-99
        DO4110I=1,IWIDTH
          IP1=I+1
          IP2=I+2
          IP3=I+3
          IP4=I+4
          IP5=I+5
          IP6=I+6
          IP7=I+7
C
          IF(IP2.GT.IWIDTH)GOTO4120
          IF(IANS(I).EQ.'F'.AND.IANS(IP1).EQ.'I'.AND.
     1       IANS(IP2).EQ.'T')ISTART=IP3
C
          IF(IP4.GT.IWIDTH)GOTO4120
          IF(IANS(I).EQ.' '.AND.IANS(IP1).EQ.'F'.AND.
     1       IANS(IP2).EQ.'O'.AND.IANS(IP3).EQ.'R'.AND.
     1       IANS(IP4).EQ.' ')ISTOP=I
C
          IF(IP7.GT.IWIDTH)GOTO4120
          IF(IANS(I).EQ.' '.AND.IANS(IP1).EQ.'S'.AND.
     1       IANS(IP2).EQ.'U'.AND.IANS(IP3).EQ.'B'.AND.
     1       IANS(IP4).EQ.'S'.AND.IANS(IP5).EQ.'E'.AND.
     1       IANS(IP6).EQ.'T'.AND.IANS(IP7).EQ.' ')ISTOP=I
C
 4110   CONTINUE
 4120   CONTINUE
        IF(ISTART.LT.1)THEN
          IBRAN=4120
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4121)IBRAN
 4121     FORMAT('     IMPOSSIBLE CONDITION AT BRANCH POINT = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4122)
 4122     FORMAT('THE STRING    FIT    NOT FOUND FOR MODEL EXTRACTION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2007)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,4124)(IANS(I),I=1,MIN(100,IWIDTH))
 4124       FORMAT('      ',100A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(ISTOP.EQ.-99)ISTOP=IWIDTH
        IF(ISTART.GT.ISTOP)THEN
          IBRAN=4130
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4132)IBRAN
 4132     FORMAT('      AT BRANCH POINT = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4133)
 4133     FORMAT('      ISTART GREATER THAN ISTOP FOR MODEL EXTRACTION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4134)ISTART,ISTOP
 4134     FORMAT('      ISTART, ISTOP = ',2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2007)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,2008)(IANS(I),I=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        J=0
        DO4150I=ISTART,ISTOP
          J=J+1
          MODEL(J)=IANS(I)
 4150   CONTINUE
        NUMCHA=ISTOP-ISTART+1
 4160   CONTINUE
      ENDIF
C
C               ***************************************************
C               **  STEP 6.2--                                   **
C               **  FOR THE CASES WHEN HAVE ... FIT Y X       ,  **
C               **  EXTRACT THE INDEPENDENT VARIABLE,            **
C               **  AND FORM THE 1 CHARACTER PER WORD            **
C               **  REPRESENTATION OF THE MODEL.                 **
C               ***************************************************
C
      ISTEPN='6.2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASFI.EQ.'FIT')GOTO4290
      IF(ICASFI.EQ.'RFIT')GOTO4290
      IF(ICASFI.EQ.'MFIT')GOTO4290
C
      ILOCRV=ILOCFI+1
      ILOCIV=ILOCFI+2
C
      IDEGRE=0
      IF(ICASFI.EQ.'0FIT')IDEGRE=0
      IF(ICASFI.EQ.'1FIT')IDEGRE=1
      IF(ICASFI.EQ.'2FIT')IDEGRE=2
      IF(ICASFI.EQ.'3FIT')IDEGRE=3
      IF(ICASFI.EQ.'4FIT')IDEGRE=4
      IF(ICASFI.EQ.'5FIT')IDEGRE=5
      IF(ICASFI.EQ.'6FIT')IDEGRE=6
      IF(ICASFI.EQ.'7FIT')IDEGRE=7
      IF(ICASFI.EQ.'8FIT')IDEGRE=8
      IF(ICASFI.EQ.'9FIT')IDEGRE=9
      IF(ICASFI.EQ.'10FI')IDEGRE=10
      K1=IDEGRE+1
C
      I=0
C
      IWD=IHARG(ILOCRV)
      CALL DPXH1H(IWD,ICH,IEND,IBUGA3)
      IF(IEND.LE.0)GOTO4219
      DO4210J=1,IEND
        I=I+1
        MODEL(I)=ICH(J)
 4210 CONTINUE
 4219 CONTINUE
C
      IWD=IHARG2(ILOCRV)
      CALL DPXH1H(IWD,ICH,IEND,IBUGA3)
      IF(IEND.GT.0)THEN
        DO4220J=1,IEND
          I=I+1
          MODEL(I)=ICH(J)
 4220   CONTINUE
      ENDIF
C
      KMAX=IDEGRE+1
      I=I+1
      MODEL(I)='='
C
      KMAX=IDEGRE+1
C
C     IF SET FIT ADDITIVE COMMAND ENTERED, THEN DO NOT INCLUDE
C     CONSTANT TERM.
C
      DO4250K=1,KMAX
        KTEMP=0
        IF(IFITAC.EQ.'OFF')THEN
          IF(K.EQ.1)GOTO4250
          KTEMP=1
        ENDIF
        KM1=K-1
C
        IF(KM1.GT.KTEMP)THEN
          I=I+1
          MODEL(I)='+'
        ENDIF
C
        I=I+1
        MODEL(I)='A'
C
        IF(0.LE.KM1.AND.KM1.LE.10)I=I+1
        IF(KM1.EQ.0)MODEL(I)='0'
        IF(KM1.EQ.1)MODEL(I)='1'
        IF(KM1.EQ.2)MODEL(I)='2'
        IF(KM1.EQ.3)MODEL(I)='3'
        IF(KM1.EQ.4)MODEL(I)='4'
        IF(KM1.EQ.5)MODEL(I)='5'
        IF(KM1.EQ.6)MODEL(I)='6'
        IF(KM1.EQ.7)MODEL(I)='7'
        IF(KM1.EQ.8)MODEL(I)='8'
        IF(KM1.EQ.9)MODEL(I)='9'
        IF(KM1.EQ.10)MODEL(I)='1'
        IF(KM1.EQ.10)I=I+1
        IF(J.EQ.10)MODEL(I)='0'
C
        IF(KM1.LE.0)GOTO4250
C
        I=I+1
        MODEL(I)='*'
C
        IWD=IHARG(ILOCIV)
        CALL DPXH1H(IWD,ICH,IEND,IBUGA3)
        IF(IEND.GT.0)THEN
          DO4260J=1,IEND
            I=I+1
            MODEL(I)=ICH(J)
 4260     CONTINUE
        ENDIF
C
        IWD=IHARG2(ILOCIV)
        CALL DPXH1H(IWD,ICH,IEND,IBUGA3)
        IF(IEND.GT.0)THEN
          DO4270J=1,IEND
            I=I+1
            MODEL(I)=ICH(J)
 4270     CONTINUE
        ENDIF
C
        IF(KM1.LE.1)GOTO4250
C
        I=I+1
        MODEL(I)='*'
        I=I+1
        MODEL(I)='*'
C
        IF(0.LE.KM1.AND.KM1.LE.10)I=I+1
        IF(KM1.EQ.0)MODEL(I)='0'
        IF(KM1.EQ.1)MODEL(I)='1'
        IF(KM1.EQ.2)MODEL(I)='2'
        IF(KM1.EQ.3)MODEL(I)='3'
        IF(KM1.EQ.4)MODEL(I)='4'
        IF(KM1.EQ.5)MODEL(I)='5'
        IF(KM1.EQ.6)MODEL(I)='6'
        IF(KM1.EQ.7)MODEL(I)='7'
        IF(KM1.EQ.8)MODEL(I)='8'
        IF(KM1.EQ.9)MODEL(I)='9'
        IF(KM1.EQ.10)MODEL(I)='1'
        IF(KM1.EQ.10)I=I+1
        IF(J.EQ.10)MODEL(I)='0'
C
 4250 CONTINUE
 4290 CONTINUE
      IWIDMO=I
      NUMCHA=IWIDMO
C
C               **********************************************
C               **  STEP 6.3--                              **
C               **  FOR ALL VARIATIONS OF THE FIT COMMAND,  **
C               **  CHECK TO SEE THE TYPE CASE--            **
C               **    1) UNQUALIFIED (THAT IS, FULL);       **
C               **    2) SUBSET/EXCEPT; OR                  **
C               **    3) FOR.                               **
C               **********************************************
C
      ISTEPN='6.3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO490
      DO400J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GOTO410
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GOTO410
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GOTO420
  400 CONTINUE
      GOTO490
  410 CONTINUE
      ICASEQ='SUBS'
      IKEY='SUBS'
      IF(IHARG(J1).EQ.'EXCE')IKEY='EXCE'
      ILOCQ=J1
      GOTO490
  420 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO490
  490 CONTINUE
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        WRITE(ICOUT,491)NUMARG,ILOCQ
  491   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************************
C               **  STEP 6.4--                              **
C               **  FOR SOME VARIATIONS OF THE FIT COMMAND, **
C               **  EXTRACT THE UNDERLYING FUNCTION         **
C               **  FROM FUNCTION DEFINITIONS.              **
C               **********************************************
C
C
      ISTEPN='6.4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        DO5170I=1,NUMCHA
          I2=I
          IF(MODEL(I).EQ.'=')GOTO5175
 5170   CONTINUE
        IBRAN=5170
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5171)IBRAN
 5171   FORMAT('      IMPOSSIBLE CONDITION AT BRANCH POINT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5172)
 5172   FORMAT('      NO EQUAL SIGN FOUND FOR MODEL EXTRACTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2007)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2008)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
 5175   CONTINUE
        ILOCEQ=I2
C
        IWD1='=   '
        IWD12='    '
        IF(ICASEQ.EQ.'FULL')THEN
          IWD2='    '
          IWD22='    '
        ELSEIF(ICASEQ.EQ.'SUBS'.AND.IKEY.EQ.'SUBS')THEN
          IWD2='SUBS'
          IWD22='ET  '
        ELSEIF(ICASEQ.EQ.'SUBS'.AND.IKEY.EQ.'EXCE')THEN
          IWD2='EXCE'
          IWD22='PT  '
        ELSEIF(ICASEQ.EQ.'FOR')THEN
          IWD2='FOR '
          IWD22='    '
        ENDIF
C
        IF(ICASFI.EQ.'FIT'.OR.ICASFI.EQ.'RFIT')THEN
          CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1                IFUNC2,N2,IBUGA3,IFOUND,IERROR)
        ELSEIF(ICASFI.NE.'FIT'.AND.ICASFI.NE.'RFIT')THEN
          CALL DPEXST(MODEL,IWIDMO,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1                IFUNC2,N2,IBUGA3,IFOUND,IERROR)
        ENDIF
        IF(IERROR.EQ.'YES')GOTO9000
        IF(IFOUND.EQ.'NO')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3372)
 3372     FORMAT('      INVALID COMMAND FORM FOR FITTING.  GENERAL ',
     1           'FORM--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3374)
 3374     FORMAT('      FIT ... = ...  SUBSET ... ... ...')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2007)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,2008)(IANS(I),I=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1              NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,
     1              IFUNC3,N3,MAXN3,
     1              IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        J=ILOCEQ
        DO5180I=1,N3
          J=J+1
          MODEL(J)=IFUNC3(I)
 5180   CONTINUE
        NUMCHA=J
C
      ENDIF
C
C               ******************************************************
C               **  STEP 7--                                        **
C               **  MAKE A NON-CALCULATING PASS AT THE MODEL        **
C               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.
C               ******************************************************
C
      ISTEPN='7'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=1
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1              IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1              IBUGCO,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ELSEIF(ICASFI.EQ.'MFIT')THEN
C
CCCCC   APRIL 2002.  IF SET FIT ADDITIVE CONSTANT OFF ENTERED, THEN DO NOT
CCCCC   FIT A CONSTANT TERM.  UPDATE CODE BELOW ACCORDINGLY.
C
        JMIN=2
        JMAX=ILOCQ-1
        MAXIND=MAXCMF-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXIND,
     1              IHNAME,IHNAM2,IUSE,NUMNAM,
     1              IVARN1,IVARN2,NUMIND,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO8000
C
        IF(IFITAC.EQ.'OFF')THEN
          NUMPAR=NUMIND
          ISTRT=2
          ISTOP=NUMPAR+1
        ELSE
          NUMPAR=NUMIND+1
          ISTRT=1
          ISTOP=NUMPAR
        ENDIF
C
        ICOUNT=0
        DO6411I5=ISTRT,ISTOP
          ICOUNT=ICOUNT+1
          I5M1=I5-1
          IH='    '
          IH2='    '
          CALL DPCOIH(I5M1,IHOUT,NOUT,IVALID,IBUGA3,ISUBRO,IERROR)
          IHOUT1=IHOUT(1)
          IHOUT2=IHOUT(2)
          IHOUT3=IHOUT(3)
          IH(1:1)='A'
          IF(NOUT.GE.1)IH(2:2)=IHOUT1(1:1)
          IF(NOUT.GE.2)IH(3:3)=IHOUT2(1:1)
          IF(NOUT.GE.3)IH(4:4)=IHOUT3(1:1)
          IPARN(ICOUNT)=IH
          IPARN2(ICOUNT)=IH2
 6411   CONTINUE
C
CCCCC   THE FOLLOWING LINE WAS COMMENTED OUT MAY 1989
CCCCC   NUMIND=ILOCQ-2
CCCCC   THE FOLLOWING LINE WAS FIXED MARCH 1989
CCCCC   NUMPV=NUMIND
        NUMPV=NUMPAR
        ILOCQM=ILOCQ-1
CCCCC   THE FOLLOWING LINE WAS FIXED MAY 1989
CCCCC   DO6412I5=2,ILOCQM
        DO6412I5=1,NUMIND
          NUMPV=NUMPV+1
CCCCC     THE FOLLOWING LINE WAS FIXED MARCH 1989
CCCCC     J5=NUMIND+(I5-1)
CCCCC     J5=NUMIND+1+(I5-1)
          J5=NUMPAR+I5
          IPARN(J5)=IVARN1(I5)
          IPARN2(J5)=IVARN2(I5)
 6412   CONTINUE
      ELSE
CCCCC   THE FOLLOWING LINE WAS ADDED AUGUST 1989
        NUMPAR=IDEGRE+1
        IF(IFITAC.EQ.'OFF')NUMPAR=IDEGRE
        DO6421I5=1,NUMPAR
          I5M1=I5-1
          IF(IFITAC.EQ.'OFF')I5M1=I5
          IH='    '
          IH2='    '
          CALL DPCOIH(I5M1,IHOUT,NOUT,IVALID,IBUGA3,ISUBRO,IERROR)
          IHOUT1=IHOUT(1)
          IHOUT2=IHOUT(2)
          IHOUT3=IHOUT(3)
          IH(1:1)='A'
          IF(NOUT.GE.1)IH(2:2)=IHOUT1(1:1)
          IF(NOUT.GE.2)IH(3:3)=IHOUT2(1:1)
          IF(NOUT.GE.3)IH(4:4)=IHOUT3(1:1)
          IPARN(I5)=IH
          IPARN2(I5)=IH2
 6421   CONTINUE
C
        IDEGRE=0
        IF(ICASFI.EQ.'0FIT')IDEGRE=0
        IF(ICASFI.EQ.'1FIT')IDEGRE=1
        IF(ICASFI.EQ.'2FIT')IDEGRE=2
        IF(ICASFI.EQ.'3FIT')IDEGRE=3
        IF(ICASFI.EQ.'4FIT')IDEGRE=4
        IF(ICASFI.EQ.'5FIT')IDEGRE=5
        IF(ICASFI.EQ.'6FIT')IDEGRE=6
        IF(ICASFI.EQ.'7FIT')IDEGRE=7
        IF(ICASFI.EQ.'8FIT')IDEGRE=8
        IF(ICASFI.EQ.'9FIT')IDEGRE=9
        IF(ICASFI.EQ.'10FI')IDEGRE=10
        NUMPV=IDEGRE+2
        IF(IFITAC.EQ.'OFF')NUMPV=IDEGRE+1
        IPARN(NUMPV)=IHARG(2)
        IPARN2(NUMPV)=IHARG2(2)
      ENDIF
C
C               ********************************************
C               **  STEP 8--                              **
C               **  CHECK TO MAKE SURE THAT THE COMBINED  **
C               **  NUMBER OF PARAMETERS AND VARIABLES    **
C               **  IN THE MODEL IS AT LEAST 1.           **
C               ********************************************
C
      ISTEPN='8'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMPV.LT.1)THEN
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4402)
 4402   FORMAT('      COMBINED NUMBER OF PARAMETERS AND VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4403)NUMPV
 4403   FORMAT('      DETECTED IN THE MODEL IS 0.   NUMPV = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4407)NUMCHA
 4407  FORMAT('      NUMBER OF CHARACTERS IN MODEL = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHA.GE.1)THEN
          WRITE(ICOUT,4408)(MODEL(J),J=1,MIN(100,NUMCHA))
 4408     FORMAT('      MODEL--',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************************************
C               **  STEP 9--                                        **
C               **  CHECK THAT ALL VARIABLES                        **
C               **  IN THE MODEL ARE ALREADY PRESENT                **
C               **  IN THE AVAILABLE NAME LIST IHNAME(.) AND IHNAM2(.).
C               **  CHECK THAT ALL PARAMETERS                       **
C               **  IN THE MODEL ARE ALREADY PRESENT                **
C               **  IN THE AVAILABLE NAME LIST IHNAME(.) AND IHNAM2(.).
C               **  ALL NAMES IN THE MODEL THAT ARE NOT             **
C               **  IN THE NAME LIST AT ALL WILL BE ADDED           **
C               **  TO THE LIST, DEFINED AS PARAMETERS,             **
C               **  AND GIVEN A VALUE OF 1.0.                       **
C               **  THIS ALLOWS US TO MAKE AN INITIAL FIT           **
C               **  WITHOUT HAVING TO DEFINE STARTING VALUES AT ALL **
C               **  (THEY WILL BE AUTOMATICALLY SET TO 1.0).  ALSO, **
C               **  FORM A NEW VECTOR WHICH HAS ONLY PARAMETER NAMES**
C               **  AND ANOTHER VECTOR WHICH HAS ONLY VARIABLE NAMES.*
C               ******************************************************
C
      ISTEPN='9'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IP=0
      IV=0
      DO4165J=1,NUMPV
        IHPARN=IPARN(J)
        IHPAR2=IPARN2(J)
        DO4166I=1,NUMNAM
          I2=I
          IF(IHPARN.EQ.IHNAME(I).AND.IHPAR2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'V')THEN
            IV=IV+1
            IVARN3(IV)=IPARN(J)
            IVARN4(IV)=IPARN2(J)
            ICOLV3(IV)=IVALUE(I2)
            NIV(IV)=IN(I2)
            GOTO4165
          ELSEIF(IHPARN.EQ.IHNAME(I).AND.IHPAR2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'P')THEN
            IP=IP+1
            IPARN3(IP)=IPARN(J)
            IPARN4(IP)=IPARN2(J)
            PARAM3(IP)=VALUE(I2)
            GOTO4165
          ENDIF
 4166   CONTINUE
        IP=IP+1
        IPARN3(IP)=IPARN(J)
        IPARN4(IP)=IPARN2(J)
        PARAM3(IP)=1.0
C
        IF(NUMNAM.GE.MAXNAM)THEN
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7752)
 7752     FORMAT('      THE TOTAL NUMBER OF (VARIABLE + PARAMETER) ',
     1           'NAMES MUST')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7754)MAXNAM
 7754     FORMAT('      BE AT MOST ',I8,'.  SUCH WAS NOT THE CASE ',
     1           'HERE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7755)
 7755     FORMAT('      THE MAXIMUM ALLOWABLE NUMBER OF NAMES WAS JUST')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7757)
 7757     FORMAT('      EXCEEDED.  SUGGESTED ACTION--ENTER    STAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7758)
 7758     FORMAT('      TO DETERMINE THE IMPORTANT (VERSUS ',
     1           'UNIMPORTANT)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7760)
 7760     FORMAT('      VARIABLES AND PARAMETERS, AND THEN REUSE SOME')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7761)
 7761     FORMAT('      OF THE NAMES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2007)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,2008)(IANS(I),I=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        I2=NUMNAM+1
        IHNAME(I2)=IPARN(J)
        IHNAM2(I2)=IPARN2(J)
        IUSE(I2)='P'
        IVALUE(I2)=1
        VALUE(I2)=1.0
        IN(I2)=1
        NUMNAM=I2
        IF(ICASFI.EQ.'MFIT')GOTO4259
        IF(ICASFI.EQ.'0FIT')GOTO4259
        IF(ICASFI.EQ.'1FIT')GOTO4259
        IF(ICASFI.EQ.'2FIT')GOTO4259
        IF(ICASFI.EQ.'3FIT')GOTO4259
        IF(ICASFI.EQ.'4FIT')GOTO4259
        IF(ICASFI.EQ.'5FIT')GOTO4259
        IF(ICASFI.EQ.'6FIT')GOTO4259
        IF(ICASFI.EQ.'7FIT')GOTO4259
        IF(ICASFI.EQ.'8FIT')GOTO4259
        IF(ICASFI.EQ.'9FIT')GOTO4259
        IF(ICASFI.EQ.'10FI')GOTO4259
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4252)
 4252     FORMAT('      NOTE--A NAME USED IN AN EXPRESSION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4253)IPARN(J),IPARN2(J)
 4253     FORMAT('      HAS NOT YET BEEN DEFINED.  NAME = ',2A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4255)
 4255     FORMAT('      THIS NAME HAS BEEN ADDED TO THE LIST, ',
     1           'SPECIFIED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4257)
 4257     FORMAT('      AS A PARAMETER, AND GIVEN THE VALUE 1.0 .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4258)(MODEL(I),I=1,MIN(100,NUMCHA))
 4258     FORMAT('      FUNCTION EXPRESSION--',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 4259   CONTINUE
        GOTO4165
 4165 CONTINUE
      NUMPAR=IP
      NUMVAR=IV
C
C               *******************************************
C               **  STEP 10--                            **
C               **  CHECK FOR A VALID NUMBER             **
C               **  OF INDEPENDENT VARIABLES (1 TO 5).   **
C               **  CHECK THE VALIDITY OF EACH           **
C               **  OF THE INDEPENDENT VARIABLES.        **
C               **  DOES THE NAME EXIST IN THE TABLE?    **
C               **  DOES THE NUMBER OF ELEMENTS          **
C               **  AGREE WITH THE NUMBER OF ELEMENTS    **
C               **  IN THE RESPONSE VARIABLE?            **
C               *******************************************
C
      ISTEPN='10'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC THE FOLLOWING LINE WAS INSERTED MAY 1989
      IF(ICASFI.NE.'FIT')GOTO520
C
      IF(NUMVAR.LT.1 .OR. NUMVAR.GT.MAXV2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,552)
  552   FORMAT('      FOR A LEAST SQUARES FIT, THE NUMBER OF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,553)
  553   FORMAT('      INDEPENDENT VARIABLES MUST BE AT LEAST 1 AND AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,555)MAXV2
  555   FORMAT('      MOST ',I8,'.  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,557)NUMVAR
  557   FORMAT('      THE SPECIFIED NUMBER OF INDEPENDENT VARIABLES ',
     1         'WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2007)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2008)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4507)NUMCHA
 4507   FORMAT('      NUMBER OF CHARACTERS IN MODEL = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4508)(MODEL(J),J=1,MIN(100,NUMCHA))
 4508   FORMAT('      MODEL--',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4504)
 4504   FORMAT('      VARIABLES EXTRACTED FROM MODEL--')
        CALL DPWRST('XXX','BUG ')
        DO4505J=1,NUMVAR
          WRITE(ICOUT,4506)J,IVARN3(J),IVARN4(J),ICOLV3(J)
 4506     FORMAT('I,IVARN3(I),IVARN4(I),ICOLV3(I) = ',I8,2X,2A4,2X,I8)
          CALL DPWRST('XXX','BUG ')
 4505   CONTINUE
        IERROR='YES'
        GOTO9000
      ENDIF
C
  520 CONTINUE
      DO540J=1,NUMVAR
        IF(NIV(J).NE.NLEFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,562)
  562     FORMAT('      FOR A LEAST SQUARES FIT, THE NUMBER OF ',
     1           'ELEMENTS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,564)
  564     FORMAT('      IN EACH INDEPENDENT VARIABLE SHOULD BE THE ',
     1           'SAME')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,565)
  565     FORMAT('      AS THE NUMBER OF ELEMENTS IN THE DEPENDENT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,567)
  567     FORMAT('      VARIABLE (RESPONSE); SUCH WAS NOT THE CASE ',
     1           'HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,571)
  571     FORMAT('      DEPENDENT   VARIABLE  (RESPONSE)--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,572)IHLEFT,IHLEF2,NLEFT
  572     FORMAT('                  ',2A4,'  HAS ',I8,' ELEMENTS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,576)
  576     FORMAT('      INDEPENDENT VARIABLES           --')
          CALL DPWRST('XXX','BUG ')
          DO580JJ=1,NUMVAR
            WRITE(ICOUT,578)IVARN3(JJ),IVARN4(JJ),NIV(JJ)
  578       FORMAT('                  ',2A4,'  HAS ',I8,' ELEMENTS')
            CALL DPWRST('XXX','BUG ')
  580     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2007)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,588)(IANS(I),I=1,MIN(100,IWIDTH))
  588       FORMAT(100A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
  540 CONTINUE
C
C               ******************************************************
C               **  STEP 11--
C               **  DUMP THE COMMON VECTOR V(.) OUT ONTO MASS STORAGE
C               **  SO AS TO PRESERVE THEIR CONTENTS FOR LATER USE
C               **  (AFTER DPFIT2).  THE ABOVE DUMP TO MASS
C               **  STORAGE IS UNNECESSARY AND IS NOT DONE FOR
C               **  THE SPECIAL CASE WHEN THE NUMBER OF PARAMETERS IS
C               **  0 (A NO-FIT CASE WHEREBY WE ARE REALLY INTERESTED
C               **  IN GENERATING PREDICTED VALUES AND RESIDUALS
C               **  FOR A GIVEN FULLY-SPECIFIED MODEL).
C               ******************************************************
C
      ISTEPN='11'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC MAY 2009: NO LONGER NEED TO DO THIS
      IOP='WRIT'
CCCCC CALL DPSWAP(IOP,V,NUMNAM,IHNAME,IHNAM2,IUSE,IN,
CCCCC1IVALUE,MAXN,MAXCOL,MAXN2,MAXCO2,MAXIJ2,IBUGA3,ISUBRO,IERROR)
CCCCC CALL DPSWAP(IOP,NUMNAM,IHNAME,IHNAM2,IUSE,IN,
CCCCC1IVALUE,MAXN2,MAXCO2,MAXIJ2,IBUGA3,ISUBRO,IERROR)
C
C               *******************************************************
C               **  STEP 12--                                        **
C               **  BRANCH TO THE APPROPRIATE SUBCASE; THEN COPY     **
C               **  OVER THE RESPONSE VECTOR TO BE USED IN THE MODEL **
C               **  INTO THE VECTOR Y; AND                           **
C               **  COPY OVER THE WEIGHTS INTO THE VECTOR W;         **
C               **  COPY OVER THE VECTORS THAT WERE USED IN THE MODEL**
C               **  INTO THE VECTORS X1, X2, X3,X4, AND X5.          **
C               **  (MAX NUMBER OF ALLOWABLE VECTORS = 5.)           **
C               *******************************************************
C
      ISTEPN='12'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)N,NUMVAR
  601   FORMAT('N,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASEQ.EQ.'FULL')THEN
        DO615I=1,NLEFT
          ISUB(I)=1
  615   CONTINUE
        NQ=NLEFT
      ELSEIF(ICASEQ.EQ.'SUBS')THEN
        NIOLD=NLEFT
        CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
        NQ=NIOLD
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        NIOLD=NLEFT
        CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        NQ=NFOR
      ELSE
        DO618I=1,NLEFT
          ISUB(I)=1
  618   CONTINUE
        NQ=NLEFT
      ENDIF
C
      K=ICOLL
      J=0
      DO4500I=1,NLEFT
        IF(ISUB(I).EQ.0)GOTO4500
        J=J+1
        IJ=MAXN*(K-1)+I
        IF(K.LE.MAXCOL)Y(J)=V(IJ)
        IF(K.EQ.MAXCP1)Y(J)=PRED(I)
        IF(K.EQ.MAXCP2)Y(J)=RES(I)
        IF(K.EQ.MAXCP3)Y(J)=YPLOT(I)
        IF(K.EQ.MAXCP4)Y(J)=XPLOT(I)
        IF(K.EQ.MAXCP5)Y(J)=X2PLOT(I)
        IF(K.EQ.MAXCP6)Y(J)=TAGPLO(I)
 4500 CONTINUE
C
      K=ICOLW
      J=0
      DO380I=1,NLEFT
        W(I)=1.0
CCCCC   THE FOLLOWING LINE WAS MOVED    MARCH 1992
CCCCC   IF(IWEIGH.EQ.'OFF')GOTO380
        IF(ISUB(I).EQ.0)GOTO380
        J=J+1
CCCCC   THE FOLLOWING LINE WAS ADDED     MARCH 1992
        IF(IWEIGH.EQ.'OFF')GOTO380
        IJ=MAXN*(K-1)+I
        IF(K.LE.MAXCOL)W(J)=V(IJ)
        IF(K.EQ.MAXCP1)W(J)=PRED(I)
        IF(K.EQ.MAXCP2)W(J)=RES(I)
        IF(K.EQ.MAXCP3)W(J)=YPLOT(I)
        IF(K.EQ.MAXCP4)W(J)=XPLOT(I)
        IF(K.EQ.MAXCP5)W(J)=X2PLOT(I)
        IF(K.EQ.MAXCP6)W(J)=TAGPLO(I)
  380 CONTINUE
C
CCCCC THE FOLLOWING SECTION (TO 389 CONTINUE) WAS ADDED MAY 1989
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
C
CCCCC   THE FOLLOWING SECTION WAS EXPANDED FROM DEFINING SEPTEMBER 1991
CCCCC   X1 TO X5   TO   X1 TO X15                        SEPTEMBER 1991
CCCCC   (BUT ALSO CONSIDERABLY SHORTENED                 SEPTEMBER 1991
CCCCC   DUE TO USE OF SUBROUTINE).                       SEPTEMBER 1991
C
        K=ICOLV3(1)
        CALL DPCOVA(K,NLEFT,X1)
        IF(NUMVAR.LE.1)GOTO4590
C
        K=ICOLV3(2)
        CALL DPCOVA(K,NLEFT,X2)
        IF(NUMVAR.LE.2)GOTO4590
C
        K=ICOLV3(3)
        CALL DPCOVA(K,NLEFT,X3)
        IF(NUMVAR.LE.3)GOTO4590
C
        K=ICOLV3(4)
        CALL DPCOVA(K,NLEFT,X4)
        IF(NUMVAR.LE.4)GOTO4590
C
        K=ICOLV3(5)
        CALL DPCOVA(K,NLEFT,X5)
        IF(NUMVAR.LE.5)GOTO4590
C
        K=ICOLV3(6)
        CALL DPCOVA(K,NLEFT,X6)
        IF(NUMVAR.LE.6)GOTO4590
C
        K=ICOLV3(7)
        CALL DPCOVA(K,NLEFT,X7)
        IF(NUMVAR.LE.7)GOTO4590
C
        K=ICOLV3(8)
        CALL DPCOVA(K,NLEFT,X8)
        IF(NUMVAR.LE.8)GOTO4590
C
        K=ICOLV3(9)
        CALL DPCOVA(K,NLEFT,X9)
        IF(NUMVAR.LE.9)GOTO4590
C
        K=ICOLV3(10)
        CALL DPCOVA(K,NLEFT,X10)
        IF(NUMVAR.LE.10)GOTO4590
C
        K=ICOLV3(11)
        CALL DPCOVA(K,NLEFT,X11)
        IF(NUMVAR.LE.11)GOTO4590
C
        K=ICOLV3(12)
        CALL DPCOVA(K,NLEFT,X12)
        IF(NUMVAR.LE.12)GOTO4590
C
        K=ICOLV3(13)
        CALL DPCOVA(K,NLEFT,X13)
        IF(NUMVAR.LE.13)GOTO4590
C
        K=ICOLV3(14)
        CALL DPCOVA(K,NLEFT,X14)
        IF(NUMVAR.LE.14)GOTO4590
C
        K=ICOLV3(15)
        CALL DPCOVA(K,NLEFT,X15)
        IF(NUMVAR.LE.15)GOTO4590
      ELSEIF(ICASFI.EQ.'MFIT')THEN
CCCCC   APRIL 2002.  IF A SET FIT CONSTANT OFF COMMAND ENTERED,
CCCCC   THEN NO CONSTANT TERM.
C
        J=0
        IF(IFITAC.EQ.'ON')THEN
          DO383I=1,NLEFT
            IF(ISUB(I).EQ.0)GOTO383
            J=J+1
            XMAT(J)=1.0
  383     CONTINUE
        ENDIF
C
        DO385L=1,NUMVAR
          IF(IFITAC.EQ.'ON')THEN
            LP1=L+1
          ELSE
            LP1=L
          ENDIF
          K=ICOLV3(L)
          J=0
          DO386I=1,NLEFT
            IF(ISUB(I).EQ.0)GOTO386
            J=J+1
            IJ=MAXN*(K-1)+I
            IF(K.LE.MAXCOL)XMAT((LP1-1)*NLEFT + J)=V(IJ)
            IF(K.EQ.MAXCP1)XMAT((LP1-1)*NLEFT + J)=PRED(I)
            IF(K.EQ.MAXCP2)XMAT((LP1-1)*NLEFT + J)=RES(I)
            IF(K.EQ.MAXCP3)XMAT((LP1-1)*NLEFT + J)=YPLOT(I)
            IF(K.EQ.MAXCP4)XMAT((LP1-1)*NLEFT + J)=XPLOT(I)
            IF(K.EQ.MAXCP5)XMAT((LP1-1)*NLEFT + J)=X2PLOT(I)
            IF(K.EQ.MAXCP6)XMAT((LP1-1)*NLEFT + J)=TAGPLO(I)
  386     CONTINUE
  385   CONTINUE
      ELSE
        K=ICOLV3(1)
        J=0
        DO381I=1,NLEFT
          IF(ISUB(I).EQ.0)GOTO381
          J=J+1
          IJ=MAXN*(K-1)+I
          IF(K.LE.MAXCOL)XMAT(J)=V(IJ)
          IF(K.EQ.MAXCP1)XMAT(J)=PRED(I)
          IF(K.EQ.MAXCP2)XMAT(J)=RES(I)
          IF(K.EQ.MAXCP3)XMAT(J)=YPLOT(I)
          IF(K.EQ.MAXCP4)XMAT(J)=XPLOT(I)
          IF(K.EQ.MAXCP5)XMAT(J)=X2PLOT(I)
          IF(K.EQ.MAXCP6)XMAT(J)=TAGPLO(I)
  381   CONTINUE
      ENDIF
C
 4590 CONTINUE
      NS=J
C
CCCCC THE FOLLOWING SECTION WAS ADDED    MAY 1991   JJF
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        CONTINUE
      ELSEIF(ICASFI.EQ.'MFIT')THEN
CCCCC   APRIL 2002: HANDLE THE FIT CONSTANT OFF CASE
        IJUNK=0
        IF(IFITAC.EQ.'OFF')IJUNK=1
        LMAX=NUMVAR
        IF(LMAX.GT.5)LMAX=5
        DO4596L=1,LMAX
          DO4597I=1,NS
            IF(L.EQ.1)X1(I)=XMAT(I + (2-IJUNK-1)*NLEFT)
            IF(L.EQ.2)X2(I)=XMAT(I + (3-IJUNK-1)*NLEFT)
            IF(L.EQ.3)X3(I)=XMAT(I + (4-IJUNK-1)*NLEFT)
            IF(L.EQ.4)X4(I)=XMAT(I + (5-IJUNK-1)*NLEFT)
            IF(L.EQ.5)X5(I)=XMAT(I + (6-IJUNK-1)*NLEFT)
 4597     CONTINUE
 4596   CONTINUE
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,4598)X1(1),X2(1),X1(2),X2(2)
 4598     FORMAT('X1(1),X2(1),X1(2),X2(2) = ',4E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ELSEIF(ICASFI.EQ.'0FIT')THEN
        DO4594I=1,NS
          X1(I)=I
 4594   CONTINUE
      ELSE
        DO4591I=1,NS
          X1(I)=XMAT(I)
 4591   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 13--
C               **  PREPARE FOR ENTRANCE INTO DPFIT2/DPFIT3--
C               **  SET THE ICON3 VECTOR
C               **  (WHICH INDICATES WHICH PARAMETERS ARE TO BE HELD
C               **  CONSTANT EQUAL TO 0 THROUGHOUT.
C               **  DEFINE CONSTRAINTS AND LIMITS.
C               ******************************************************
C
      ISTEPN='13'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO4195I=1,NUMPAR
        ICON3(I)=0
 4195 CONTINUE
C
      IF(NUMCON.GT.0)THEN
        DO4700I=1,NUMPAR
          DO4800J=1,NUMCON
            J2=J
            IF(IPARN3(I).EQ.IPARNC(J).AND.IPARN4(I).EQ.IPANC2(J))THEN
              IPARO3(I)=IPAROC(J2)
              PARLI3(I)=PARLIM(J2)
              GOTO4700
            ENDIF
 4800     CONTINUE
          IPARO3(I)='NONE'
 4700   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 14--                                       **
C               **  CARRY OUT THE ACTUAL FIT                        **
C               **  VIA CALLING                                     **
C               **  DPFIT2 (FOR GENERAL MODELS), OR                 **
C               **  DPFIT3 (FOR POLYNOMIAL AND MULTILINEAR MODELS)  **
C               ******************************************************
C
      ISTEPN='14'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6081)
 6081   FORMAT('***** FROM DPFIT, AS ABOUT TO CALL DPFIT2/DPFIT3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6082)NUMCHA,NLEFT,MAXN,NS,NUMPV,NUMPAR,NUMVAR
 6082   FORMAT('NUMCHA,NLEFT,MAXN,NS,NUMPV,NUMPAR,NUMVAR = ',7I8)
        CALL DPWRST('XXX','BUG ')
        DO6083I=1,NS
          WRITE(ICOUT,6084)I,Y(I),X1(I),X2(I),X3(I),XMAT(I),
     1                     XMAT(I+NLEFT),W(I)
 6084     FORMAT('I,Y(I),X1(I),X2(I),X3(I),XMAT(I,1),XMAT(I,2),W(I) = ',
     1           I6,2X,7F10.5)
          CALL DPWRST('XXX','BUG ')
 6083   CONTINUE
        WRITE(ICOUT,6085)(MODEL(I),I=1,MIN(120,NUMCHA))
 6085   FORMAT('MODEL(.)--',120A1)
        CALL DPWRST('XXX','BUG ')
        DO6086J=1,NUMPAR
          WRITE(ICOUT,6087)J,IPARN3(J),IPARN4(J),PARAM3(J),ICON3(J)
 6087     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I),ICON3(I) = ',
     1           I8,2X,2A4,E15.7,A4)
          CALL DPWRST('XXX','BUG ')
 6086   CONTINUE
        DO6088J=1,NUMVAR
          WRITE(ICOUT,6089)J,IVARN3(J),IVARN4(J),ICOLV3(J)
 6089     FORMAT('I,IVARN3(I),IVARN4(I),ICOLV3(I) = ',I8,2X,2A4,2X,I8)
          CALL DPWRST('XXX','BUG ')
 6088   CONTINUE
        WRITE(ICOUT,6091)IBUGA3,IBUGCO,IBUGEV,NUMIND
 6091   FORMAT('IBUGA3,IBUGCO,IBUGEV,NUMIND = ',2(A4,2X),A4,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASFI.EQ.'FIT')THEN
CCCCC   JUNE, 1990.  ADD "DUMMY1,...,DUMMY5"  ARGUMENTS
CCCCC   SEPTEMBER, 1991.  ADD "X6 TO X15" ARGUMENTS
        CALL DPFIT2(Y,X1,X2,X3,X4,X5,
     1              X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,
     1              NUMVAR,IVARN3,IVARN4,W,NS,
     1              MODEL,NUMCHA,PARAM3,IPARN3,IPARN4,NUMPAR,ICON3,
     1              IANGLU,IPARO3,
     1              PARLI3,VSCRT,MAXITS,FITSD,FITPOW,CPUEPS,
     1              ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,
     1              IREP,REPSD,REPDF,RESSD,RESDF,PRED2,RES2,ALFCDF,
     1              DUMMY1,DUMMY2,DUMMY3,DUMMY4,DUMMY5,
     1              ICAPSW,ICAPTY,IFORSW,IFITAU,IAUXDP,
     1              IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO8000
      ELSE
CCCCC   JUNE 2002: CHECK TO SEE IF ALPHA PARAMETER DEFINED.
C
        ALPHA=0.95
        IHP='ALPH'
        IHP2='A   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          ALPHA=0.95
        ELSE
          ALPHA=VALUE(ILOCP)
        ENDIF
        IF(ALPHA.LE.0.0)THEN
          ALPHA=0.95
        ELSEIF(ALPHA.GE.1.0.AND.ALPHA.LT.100.0)THEN
          ALPHA=ALPHA/100.0
        ELSEIF(ALPHA.GE.100.0)THEN
          ALPHA=0.95
        ENDIF
        IF(ALPHA.LT.0.5)ALPHA=1.0-ALPHA
C
        CALL DPFIT3(Y,X1,X2,X3,X4,X5,XMAT,NLEFT,PARCOV,MAXPAR,
     1              NUMVAR,IVARN3,IVARN4,W,NS,
     1              MODEL,NUMCHA,PARAM3,IPARN3,IPARN4,NUMPAR,ICON3,
     1              IANGLU,IPARO3,
     1              PARLI3,VSDPRD,VSCRT,FITSD,FITPOW,ICASFI,
     1              ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,
     1              IREP,REPSD,REPDF,RESSD,RESDF,PRED2,RES2,ALFCDF,BIC,
     1              DUMMY1,DUMMY2,DUMMY3,DUMMY4,DUMMY5,
     1              IFITAC,ALPHA,
     1              RSQUAR,ADJRSQ,APRESS,
     1              ICAPSW,ICAPTY,IFORSW,IFITAU,IAUXDP,
     1              IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO8000
      ENDIF
C
C               ***************************************
C               **  STEP 15--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
 7000 CONTINUE
C
      ISTEPN='15'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
      IREPU='ON'
      IRESU='ON'
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NLEFT,
     1IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
C
CCCCC JUNE 2002.  ADD FOLLOWING PARAMETERS FOR MULTI-LINEAR FIT
      IF(ICASFI.EQ.'MFIT')THEN
        IH='RSQU'
        IH2='ARE '
        VALUE0=RSQUAR
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1  IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='ADJR'
        IH2='SQUA'
        VALUE0=ADJRSQ
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1  IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='PRES'
        IH2='SP  '
        VALUE0=APRESS
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1  IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='BIC '
        IH2='    '
        VALUE0=BIC
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1  IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
C
      IF(ICASFI.EQ.'FIT')GOTO7900
      IF(ICASFI.EQ.'RFIT')GOTO7900
C
CCCCC THE FOLLOWING SECTION (DOWN TO 7640 CONTINUE) WAS REWRITTEN MAY 1989
      IF(ICASFI.EQ.'MFIT')K1=NUMPAR
      L=0
      DO7600J=1,K1
        JM1=J-1
        L=L+1
        IH='    '
        IH2='    '
        CALL DPCOIH(JM1,IHOUT,NOUT,IVALID,IBUGA3,ISUBRO,IERROR)
        IHOUT1=IHOUT(1)
        IHOUT2=IHOUT(2)
        IHOUT3=IHOUT(3)
        IH(1:1)='A'
        IF(NOUT.GE.1)IH(2:2)=IHOUT1(1:1)
        IF(NOUT.GE.2)IH(3:3)=IHOUT2(1:1)
        IF(NOUT.GE.3)IH(4:4)=IHOUT3(1:1)
C
 7640   CONTINUE
        DO7650I=1,NUMNAM
          I2=I
          IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'P')THEN
            VALUE(I2)=PARAM3(L)
            VAL=VALUE(I2)
            IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=VAL+0.5
            IF(VAL.GT.CUTOFF)IVAL=CUTOFF
            IF(VAL.LT.(-CUTOFF))IVAL=(-CUTOFF)
            IVALUE(I2)=IVAL
            GOTO7600
          ENDIF
 7650   CONTINUE
        IF(NUMNAM.GE.MAXNAM)THEN
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7652)
 7652     FORMAT('      THE TOTAL NUMBER OF (VARIABLE + PARAMETER)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7653)MAXNAM
 7653     FORMAT('      NAMES MUST BE AT MOST ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7654)
 7654     FORMAT('      SUCH WAS NOT THE CASE HERE--THE MAXIMUM ',
     1           'ALLOWABLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7656)
 7656     FORMAT('      NUMBER OF NAMES WAS JUST EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7657)
 7657     FORMAT('      SUGGESTED ACTION--ENTER   STAT  TO DETERMINE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7659)
 7659     FORMAT('      THE IMPORTANT (VERSUS UNIMPORTANT) VARIABLES ',
     1           'AND')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7660)
 7660     FORMAT('      PARAMETERS, AND THEN REUSE SOME OF THE NAMES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2007)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,2008)(IANS(I),I=1,MIN(100,IWIDTH))
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
        VALUE(ILOC)=PARAM3(L)
        VAL=VALUE(ILOC)
        IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=VAL+0.5
        IF(VAL.GT.CUTOFF)IVAL=CUTOFF
        IF(VAL.LT.(-CUTOFF))IVAL=(-CUTOFF)
        IVALUE(ILOC)=IVAL
C
 7600 CONTINUE
 7900 CONTINUE
C
C               ******************************************************
C               **  STEP 16--
C               **  READ BACK IN FROM MASS STORAGE
C               **  THE CONTENTS OF THE V(.) VECTOR.  THE ABOVE
C               **  RETRIEVAL FROM MASS STORAGE IS UNNECESSARY AND IS
C               **  FOR THE SPECIAL CASE WHEN THE NUMBER OF PARAMETERS
C               **  IS 0 (A NO-FIT CASE WHEREBY WE ARE REALLY
C               **  INTERESTED IN GENERATING PREDICTED VALUES
C               **  AND RESIDUALS FOR A GIVEN FULLY-SPECIFIED MODEL).
C               ******************************************************
C
 8000 CONTINUE
C
      ISTEPN='16'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *************************************************
C               **  STEP 17--                                  **
C               **  COPY THE FINAL ESTIMATES FROM THE FIT      **
C               **  BACK INTO THE PARAMETERS.                  **
C               **  THESE FINAL ESTIMATES WILL THUS OVERWRITE  **
C               **  THE STARTING VALUES THAT WERE              **
C               **  ORIGINALLY ASSIGNED TO THE PARAMETERS.     **
C               *************************************************
C
 6000 CONTINUE
C
      ISTEPN='17'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMPAR.GT.0)THEN
        DO6100J=1,NUMPAR
          IH=IPARN3(J)
          IH2=IPARN4(J)
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          VALUE(ILOCP)=PARAM3(J)
          VAL=VALUE(ILOCP)
          IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=VAL+0.5
          IF(VAL.GT.CUTOFF)IVAL=CUTOFF
          IF(VAL.LT.(-CUTOFF))IVAL=(-CUTOFF)
          IVALUE(ILOCP)=IVAL
 6100   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NS,NUMNAM,ICASFI,ICASEQ
 9015   FORMAT('NS,NUMNAM,ICASFI,ICASEQ = ',2I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9017I=1,NUMNAM
          WRITE(ICOUT,9018)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),
     1                     IVALUE(I),VALUE(I)
 9018     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I)',
     1           'VALUE(I) = ',I8,2X,2A4,2X,A4,2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9017   CONTINUE
        WRITE(ICOUT,9021)NUMIND,NUMPV,NUMVAR,IP,IV
 9021   FORMAT('NUMIND,NUMPV,NUMVAR,IP,IV = ',5I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMPV.GT.0)THEN
          DO9022I=1,NUMPV
            WRITE(ICOUT,9023)I,IPARN(I),IPARN2(I)
 9023       FORMAT('I,IPARN(I),IPARN2(I) = ',I8,2X,2A4)
            CALL DPWRST('XXX','BUG ')
 9022     CONTINUE
        ENDIF
        IF(IP.GT.0)THEN
          DO9032I=1,IP
            WRITE(ICOUT,9033)I,IPARN3(I),IPARN4(I)
 9033       FORMAT('I,IPARN3(I),IPARN4(I) = ',I8,2X,2A4)
            CALL DPWRST('XXX','BUG ')
 9032     CONTINUE
        ENDIF
        IF(IV.GT.0)THEN
          DO9042I=1,IV
            WRITE(ICOUT,9043)I,IVARN3(I),IVARN4(I)
 9043       FORMAT('I,IVARN3(I),IVARN4(I) = ',I8,2X,2A4)
            CALL DPWRST('XXX','BUG ')
 9042     CONTINUE
        ENDIF
        WRITE(ICOUT,9051)MAXN2,NLEFT,NS,V(1),PRED(1),RES(1)
 9051   FORMAT('MAXN2,NLEFT,NS,V(1),PRED(1),RES(1) = ',3I8,3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9053)IWIDTH,ICOLW,NWEIGH,IWIDMO,IWEIGH
 9053   FORMAT('IWIDTH,ICOLW,NWEIGH,IWIDMO,IWEIGH = ',4I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2008)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IWIDMO.GE.1)THEN
          WRITE(ICOUT,9064)(MODEL(I),I=1,MIN(IWIDMO,100))
 9064     FORMAT('(MODEL(I),I=1,IWIDMO) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,9069)IFOUND,IERROR
 9069   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFIT2(Y,X1,X2,X3,X4,X5,
     1X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,
     1NUMVAR,IVARN3,IVARN4,W,N,
     1MODEL,NUMCHA,PARAM3,IPARN3,IPARN4,NUMPAR,ICON3,IANGLU,IPARO3,
     1PARLI3,V,MAXITS,FITSD,FITPOW,CPUEPS,
     1ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,
     1IREP,REPSD,REPDF,RESSD,RESDF,PRED2,RES2,ALFCDF,
     1DUM1,DUM2,Y2,WSQRT,G,
     1ICAPSW,ICAPTY,IFORSW,IFITAU,IAUXDP,
CCCCC THE FOLLOWING LINE AUGMENTED WITH ISUBRO       MARCH 1992
CCCCC1IBUGA3,IBUGCO,IBUGEV,IERROR)
     1IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
CCCCC JUNE 1990.  ADD DUM1 - G ARGUMENTS (DIMENSIONED IN DPFIT)
CCCCC SEPT. 1991. ARGS X6 TO X15 ABOVE ARE NEW.
C
C     LEVENBERG, MARQUARDT, MORRISON ALGORITHM IMPLEMENTED FOLLOWING
C     SUGGESTION OF GOLUB (SEE OSBORNE 'SOME ASPECTS OF NONLINEAR LEAST
C     SQUARES CALCULATION' EDITOR F.A. LOOTSMA ACADEMIC PRESS).  MAIN
C     FEATURE OF THIS ROUTINE IS AN IMPROVED TEST FOR ACCEPTING
C     PREDICTED CORRECTION AND ADJUSTING LEVENBERG PARAMETER ALAMBA
C
C     VARIABLES
C
C     PARAM3(1)   VECTOR OF INDEPENDENT VARIABLES
C            INPUT. CONTAINS ESTIMATE OF SOLUTION
C            OUTPUT. CONTAINS SOLUTION VECTOR OR LAST ATTEMPT
C
C     V(1)   STORAGE OF GRAD F BY COLUMNS
C            I.E., THE DERIVATIVES EVALUATED AT EACH OF THE N DATA POINTS
C            OF THE N RESIDUALS RES2(I) WITH RESPECT TO
C            THE FIRST PARAMETER FOLLOWED BY ALL THE DERIVATIVES
C            WITH RESPECT TO THE SECOND PARAMETER, ETC.
C
C     RES2(1)   STORAGE FOR F VECTOR OF TERMS IN SUM OF SQUARES
C            OUTPUT. VECTOR OF TERMS (USALLY RESIDUALS) IN SUM
C            OF SQUARES
C
C     SUMSQ   OUTPUT. CONTAINS SUM OF SQUARES
C
C     N      INPUT. NO. OF TERMS IN SUM OF SQUARES = NUMBER OF OBSERVATIONS.
C
C     NP     INPUT. NO. OF PARAMETERS INCLUDING ANY TO BE HELD CONSTANT
C
C     TOL    INPUT. TOLERANCE ON CALCULATION OF SUM OF SQUARES
C
C     EXPND  OUTPUT. FACTOR BY WHICH ALAMBA INCREASED IF TEST ON SUM OF
C            SQUARES FAILS, SUGGESTED VALUE 1.5
C
C     COMPR   INPUT. FACTOR BY WHICH ALAMBA COMPREASED IF TEST ON SUM OF
C            SQUARES SUCCEEDS ON FIRST ATTEMPT, SUGGESTED VALUE 0.5
C
C     ITS    INPUT. MAX NUMBER OF ITERATIONS
C            OUTPUT. ACTUAL NUMBER OF ITERATIONS
C
C     IER    INPUT.=-1+(100*NCONST)  NO PRINTING
C                  =0+(100*NCONST)  PRINTING AFTER CONVERGENCE ONLY
C                  =1+(100*NCONST)  PRINT DIAGNOSTIC INFORMATION
C                  =2+(100*NCONST)  AS ABOVE PLUS GRADIENT CHECK
C            WHERE NCONST = NO. OF PARAMETERS TO BE HELD CONSTANT
C            OUTPUT.=1 SUCCESSUL TERMINATION
C            =2 MAX ITS EXCEEDED
C            =3 ALAMBA EXCEEDS 1.D6
C            =4 ALL GRADIENTS ZERO FOR ONE OR MORE PARAMETERS
C            =5 NO. OF PARAMETERS LESS THAN ONE
C
C     C(1)   OUTPUT. CONTAINS APPROXIMATE
C            STANDARD ERRORS OF PARAMETER ESTIMATES
C
C     G(1)   OUTPUT. CONTAINS A VECTOR OF UNCORRELATED RESIDUALS
C
C     WS(1)   WORKING SPACE, MUST BE ALLOTTED AT LEAST
C            NPR*(NPR+5) + NCONST     IN CALLING PROGRAM,
C            WHERE NCONST IS THE NUMBER OF PARAMETERS TO BE HELD
C            CONSTANT AND     NPR = NP - NCONST.
C
C     ICON3(1) INPUT. ICON3(1)=1  IF THE I-TH PARAMETER IS TO BE HELD
C                               CONSTANT
C                           =0  OTHERWISE
C
C
C     USER SUPPLIED SUBROUTINE F REQUIRED TO SET VALUES OF SUMSQ,
C     F,A DECLARATION MUST BE
C             SUBROUTINE F (X,N,PARAM3,NUMPAR,F,A,SUMSQ,IFL)
C             IF IFL=1 SETS ALL VALUES
C             IF IFL=2 SETS SUMSQ ONLY MUST NOT ALTER A,F
C
C     N.B. THE VALUE OF ILF IS SUPPLIED BY DPFIT2 AND MUST NOT BE CHANGED
C
C     EPS IS A MACHINE-DEPENDENT CONSTANT.
C
C     NOTE--MAX NUMBER OF OBSERVATIONS N IS 1000 (NOT CHECKED FOR)
C     NOTE--MAX NUMBER OF PARAMETERS K IS 30 (NOT CHECKED FOR)
C     NOTE--DIMENSION OF G IS N (MAX IS 1000)
C     NOTE--DIMENSION OF C IS K (MAX IS 30)
C     NOTE--DIMENSION OF A IS N X K (BUT N X K MAX IS 10000)
C
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER 26, 1977.
C     UPDATED         --JULY      1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JUNE      1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --MARCH     1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --AUGUST    1987. WEIGHTED FIT
C     UPDATED         --JANUARY   1988. FIX WEIGHTED FIT PRED & RES
C     UPDATED         --MARCH     1988. ADD LOFCDF
C     UPDATED         --JUNE      1990. MOVE SOME DIMENSIONS TO DPFIT
C     UPDATED         --JULY      1990. FIX OVERFLOW
C     UPDATED         --SEPT      1991. EXPAND IND. VAR. 5 TO 15
C     UPDATED         --MARCH     1992. FIX FORMAT MESSAGE
C     UPDATED         --MARCH     1992. WRITE COEF SDCOEF TCDF TO FILE
C     UPDATED         --MARCH     1992. ISUBRO ADDED TO INPUT ARG LIST
C     UPDATED         --FEBRUARY  1994. ACTIVATE FITSD TEST
C     UPDATED         --MAY       1994. FIX (= SPLIT) FORMAT 1122
C     UPDATED         --MAY       1994. CORRECT AN OVERFLOW DIVISION
C     UPDATED         --MAY       1995. FIX SOME I/O
C     UPDATED         --APRIL     1996. IPRINT SWITCH
C     UPDATED         --JULY      1997. PRINT SUMMARY INFORMATION IF
C                                       MAXIMUM ITERATIONS REACHED
C     UPDATED         --FEBRUARY  1998. CALL DPFLSH (FOR GUI)
C     UPDATED         --APRIL     2001. PRINT OUT VAR-COV MATRIX
C     UPDATED         --NOVEMBER  2002. CAPTURE HTML, LATEX
C     UPDATED         --MAY       2011. USE DPAUFI TO OPEN/CLOSE
C                                       DPST?F.DAT FILES
C     UPDATED         --MAY       2011. USE DPDTA1 AND DPDT5B TO
C                                       PRINT OUTPUT
C     UPDATED         --JUNE      2014. USER OPTION TO SUPPRESS
C                                       WRITING TO AUXILLARY FILES
C     UPDATED         --APRIL     2019. USER CAN SPECIFY NUMBER OF
C                                       DECIMAL POINTS FOR AUXILLARY
C                                       FILES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IFITAU
C
      CHARACTER*4 IVARN3
      CHARACTER*4 IVARN4
      CHARACTER*4 IPARN3
      CHARACTER*4 IPARN4
      CHARACTER*4 IANGLU
      CHARACTER*4 IPARO3
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW2HOL
      CHARACTER*4 IW22HO
      CHARACTER*4 IREP
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
CCCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUND
C
      CHARACTER*4 IPARN5
      CHARACTER*4 IPARN6
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 MODEL
      CHARACTER*4 IOP
      CHARACTER*20 IFORMT
C
      PARAMETER(NUMCLI=10)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=60)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*50 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
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
C---------------------------------------------------------------------
C
      DOUBLE PRECISION SUM,SSS,SSINIT,SSR,WW,SSN,SUMSQ
      DOUBLE PRECISION S
      DOUBLE PRECISION DS1,DS2,DTOL
      DOUBLE PRECISION DRAT1,DRAT2
      DOUBLE PRECISION DEPS,DTOL2,DRAT
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC THE FOLLOWING INCLUDE STATEMENT WAS ADDED MARCH 1992
      INCLUDE 'DPCOF2.INC'
C
      DIMENSION Y(*)
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION X4(*)
      DIMENSION X5(*)
CCCCC THE FOLLOWING 10 LINES WERE ADDED SEPTEMBER 1991
      DIMENSION X6(*)
      DIMENSION X7(*)
      DIMENSION X8(*)
      DIMENSION X9(*)
      DIMENSION X10(*)
      DIMENSION X11(*)
      DIMENSION X12(*)
      DIMENSION X13(*)
      DIMENSION X14(*)
      DIMENSION X15(*)
C
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
C
      DIMENSION W(*)
C
      DIMENSION V(*)
C
      DIMENSION MODEL(*)
C
      DIMENSION IVARN3(*)
      DIMENSION IVARN4(*)
      DIMENSION PARAM3(*)
      DIMENSION IPARN3(*)
      DIMENSION IPARN4(*)
      DIMENSION ICON3(*)
      DIMENSION IPARO3(*)
      DIMENSION PARLI3(*)
C
      DIMENSION ITYPEH(*)
      DIMENSION IW2HOL(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
      DIMENSION IPARN5(30)
      DIMENSION IPARN6(30)
      DIMENSION PARAM5(30)
C
CCCCC JUNE, 1990.  DIMENSIONS MOVED TO DPFIT
CCCCC DIMENSION DUM1(MAXOBV)
CCCCC DIMENSION DUM2(MAXOBV)
CCCCC DIMENSION Y2(MAXOBV)
CCCCC DIMENSION WSQRT(MAXOBV)
C
CCCCC DIMENSION G(MAXOBV)
      DIMENSION DUM1(*)
      DIMENSION DUM2(*)
      DIMENSION Y2(*)
      DIMENSION WSQRT(*)
C
      DIMENSION G(*)
CCCCC END CHANGE
      DIMENSION WS(1100)
CCCCC DIMENSION Y0(MAXOBV)
C
      DIMENSION DUM(30)
      DIMENSION C(15)
      DIMENSION TVALU2(15)
      DIMENSION PARAM7(30)
      DIMENSION PARAM9(30)
      DIMENSION VARCOV(30,30)
      DIMENSION CORR(30,30)
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
      ISUBN1='DPFI'
      ISUBN2='T2  '
C
      IERROR='NO'
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
CCCCC THE FOLLOWING LINE WAS ADDED TO FIX OVERFLOW JULY 1990
      CPUMA2=CPUMAX/1000.0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N,NUMVAR,NUMPAR,NUMCHA
   52   FORMAT('N,NUMVAR,NUMPAR,NUMCHA = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,IBUGCO,IBUGEV,ISUBRO
   53   FORMAT('IBUGA3,IBUGCO,IBUGEV,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
         WRITE(ICOUT,56)I,Y(I),X1(I),W(I)
   56    FORMAT('I,Y(I),X1(I),W(I) = ',I5,3F20.10)
         CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO61J=1,NUMVAR
        WRITE(ICOUT,62)J,IVARN3(J),IVARN4(J)
   62   FORMAT('I,IVARN3(I),IVARN4(I) = ',I8,2X,A4,A4)
        CALL DPWRST('XXX','BUG ')
   61   CONTINUE
        DO66J=1,NUMPAR
          WRITE(ICOUT,67)J,IPARN3(J),IPARN4(J),PARAM3(J),ICON3(J)
   67     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I),ICON3(I) = ',
     1           I8,2X,A4,A4,E15.7,I8)
          CALL DPWRST('XXX','BUG ')
   66   CONTINUE
CCCCC   MAY 1995.  FIX SOME I/O
CCCCC   WRITE(ICOUT,71)(MODEL(J),J=1,NUMCHA)
        NTEMP=MIN(NUMCHA,100)
        WRITE(ICOUT,71)(MODEL(J),J=1,NTEMP)
   71   FORMAT('FUNCTIONAL EXPRESSION--',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
C               **************************************************
C               **  STEP 0.5--                                  **
C               **   OPEN THE STORAGE FILES                     **
C               **************************************************
C
      ISTEPN='0.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFITAU.EQ.'ON')THEN
        IOP='OPEN'
        IFLAG1=1
        IFLAG2=1
        IFLAG3=1
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************************
C               **  STEP 1--                                    **
C               **  DETERMINE THE PARAMETER NAMES IN THE MODEL  **
C               **  AND THE NUMBER NUMPAR OF PARAMETERS.        **
C               **************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=2
C
      IF(NUMPAR.GT.0)THEN
        DO7100I=1,NUMPAR
          IPARN5(I)=IPARN3(I)
          IPARN6(I)=IPARN4(I)
          PARAM5(I)=PARAM3(I)
 7100   CONTINUE
      ENDIF
C
      IF(NUMVAR.GT.0)THEN
        DO7300I=1,NUMVAR
          IPARN5(NUMPAR+I)=IVARN3(I)
          IPARN6(NUMPAR+I)=IVARN4(I)
 7300   CONTINUE
      ENDIF
C
      NUMPV=NUMPAR+NUMVAR
C
C               ******************************************************
C               **  STEP 2--                                        **
C               **  DEFINE VARIOUS CONSTANTS.                       **
C               **  DEFINE EPS = MACHINE EPSILON.                   **
C               **  DEFINE TOL = CUTOFF TOLERANCE FOR SUCCESSIVE    **
C               **               ESTIMATES.                         **
C               **  DEFINE MAXITS = MAX NUMBER OF ITERATIONS.       **
C               **  DEFINE EXPND = EXPANSION FACTOR                 **
C               **  DEFINE COMPR  = COMPRESSION FACTOR              **
C               **  DEFINE NCONST = NUMBER OF PARAMETERS HELD       **
C               **                  CONSTANT.                       **
C               **  DEFINE NP = NUMBER OF NON-CONSTNAT PARAMETERS.  **
C               **  DEFINE DF = DEGREES OF FREEDOM.                 **
C               **  DEFINE SOME WORKING STORAGE START POINTS IN WS. **
C               ******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IREP='NO'
      REPSD=0.0
      REPDF=0.0
      IREPDF=REPDF+0.5
      RESSD=0.0
      RESDF=0.0
      ALFCDF=(-999.99)
      IF(NUMPAR.GT.0)THEN
        EPS = 1.E-8
        DEPS=EPS
        TOL=0.00001
        DTOL=TOL
        ALAMBA=0.01
        EXPND=1.5
        COMPR=0.5
        NPST=NUMPAR
        NCONST=0
        DO501I=1,NUMPAR
          IF(ICON3(I).EQ.1)NCONST=NCONST+1
  501   CONTINUE
        NP=NUMPAR-NCONST
        IF(NP.LE.0) THEN
          WRITE(ICOUT,117) NP
117       FORMAT(10X,'NUMBER OF PARAMETERS TO BE VARIED = ',I8,
     *           ' (LESS THAN ONE)')
          CALL DPWRST('XXX','BUG ')
          IER = 5
          IERROR='YES'
          GOTO9000
        ENDIF
        DF=N-NP
        RESDF=DF
        IRESDF=DF+0.5
        IC=0
        IER=2
        IDA=NP*NP
        IDU=IDA+NP
        ID =IDU+NP
        IDX=ID +NP
        IY =IDX+NP
      ENDIF
C
C               **********************************************
C               **  STEP 2.2--                              **
C               **  COMPUTE THE SQUARE ROOT OF THE WEIGHTS  **
C               **********************************************
C
      ISTEPN='2.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO550I=1,N
        IF(W(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,556)
  556     FORMAT('***** ERROR IN DPFIT2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,557)
  557     FORMAT('      NEGATIVE WEIGHT ENCOUNTERED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,558)
  558     FORMAT('      FITTING WITH NEGATIVE WEIGHTS NOT PERMITTED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(W(I).EQ.0.0)THEN
          WSQRT(I)=W(I)
        ELSE
          WSQRT(I)=SQRT(W(I))
        ENDIF
  550 CONTINUE
C
C          ***************************************************
C          *  STEP 2.3--                                    **
C          *  FORM A NEW RESPONSE VECTOR  ( =               **
C          *  THE OLD RESPONSE * SQUARE ROOT OF WEIGHTS  (  **
C          ***************************************************
C
      ISTEPN='2.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO560I=1,N
        Y2(I)=Y(I)*WSQRT(I)
  560 CONTINUE
C
C               ******************************************************
C               **  STEP 2.5--                                      **
C               **  CHECK FOR REPLICATION AND IF EXISTENT           **
C               **  COMPUTE A (MODEL-FREE) REPLICATION STANDARD     **
C               **  DEVIATION.                                      **
C               ******************************************************
C
      ISTEPN='2.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPREPS(Y,X1,X2,X3,X4,X5,N,NUMVAR,DUM1,DUM2,
     1IREP,REPSS,REPMS,REPSD,REPDF,NUMSET,IBUGA3,IERROR)
      IREPDF=REPDF+0.5
C
C     PRINT INTIAL INFORMATION (BEFORE ANY FIT ITERATIONS)
C
      IF(IPRINT.EQ.'ON')THEN
        IF(NUMPAR.GE.1)THEN
          ITITLE='Least Squares Non-Linear Fit'
          NCTITL=28
        ELSE
          ITITLE='Fully-Specified Model'
          NCTITL=21
        ENDIF
        ITITLZ=' '
        NCTITZ=0
C
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Sample Size:'
        NCTEXT(ICNT)=12
        AVALUE(ICNT)=REAL(N)
        IDIGIT(ICNT)=0
C
        IMIN=1
        IF(MODEL(1).EQ.' ')IMIN=2
        IMAX=NUMCHA
        IDEL=IMAX-IMIN+1
        NUMLIN=((IDEL-1)/43)+1
        IF(NUMLIN.GE.1)THEN
          DO47240KLINE=1,NUMLIN
            IF(KLINE.EQ.1)THEN
              KMIN=IMIN
              KMAX=KMIN+43-1
              IF(KMAX.GT.IMAX)KMAX=IMAX
              ICNT=ICNT+1
              ITEXT(ICNT)(1:7)='Model: '
            ELSEIF(KLINE.GE.2)THEN
              ICNT=ICNT+1
              KMIN=KMAX+1
              KMAX=KMIN+100-1
              IF(KMAX.GT.IMAX)KMAX=IMAX
              ITEXT(ICNT)(1:7)='       '
            ENDIF
            ICNT2=7
            DO47245K=KMIN,KMAX
              ICNT2=ICNT2+1
              ITEXT(ICNT)(ICNT2:ICNT2)=MODEL(K)(1:1)
47245       CONTINUE
            NCTEXT(ICNT)=ICNT2
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
47240     CONTINUE
        ENDIF
C
        IF(IREP.EQ.'NO')THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='No Replication Case:'
          NCTEXT(ICNT)=20
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ELSE
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Case:'
          NCTEXT(ICNT)=17
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Standard Deviation:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REPSD
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Degrees of Freedom:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REAL(IREPDF)
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Distinct Subsets:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REAL(NUMSET)
          IDIGIT(ICNT)=0
        ENDIF
C
        NUMROW=ICNT
        DO2310I=1,NUMROW
          NTOT(I)=15
 2310   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1              NCTEXT,AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGA3,IERROR)
C
C       DEFINE HEADERS FOR THE INTERMEDIATE ITERATIONS
C
        ITITLE=' '
        NCTITL=-99
        ITITL9=' '
        NCTIT9=0
C
        IWHTML(1)=75
        IWHTML(2)=125
        IWHTML(3)=125
        IWHTML(4)=50
        IWHTML(5)=125
        IWHTML(6)=125
        IWHTML(7)=125
        IINC=1600
        IINC2=200
        IINC3=1200
        IWRTF(1)=IINC3
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC
        IWRTF(4)=IWRTF(3)+IINC2
        IWRTF(5)=IWRTF(4)+IINC
        IWRTF(6)=IWRTF(5)+IINC
        IWRTF(7)=IWRTF(6)+IINC
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
C
C       RESTRICT THE NUMBER OF PARAMETERS PER LINE DEPENDING
C       ON OUTPUT FORMAT
C
        IF(ICAPTY.EQ.'HTML')THEN
          NTEMP=3
        ELSEIF(ICAPTY.EQ.'LATE')THEN
          NTEMP=4
        ELSEIF(ICAPTY.EQ.'RTF')THEN
          NTEMP=3
        ELSE
          NTEMP=6
        ENDIF
        IF(NUMPAR.LE.NTEMP)THEN
          NUMCOL=4+NUMPAR
        ELSE
          NUMCOL=4+NTEMP
        ENDIF
        NUMLIN=3
C
        DO3101J=1,NUMCLI
          DO3102I=1,MAXLIN
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
 3102     CONTINUE
          DO3103I=1,MAXROW
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
            IDIGI2(I,J)=-6
 3103     CONTINUE
 3101   CONTINUE
C
        ITITL2(1,1)=' '
        NCTIT2(1,1)=0
        ITITL2(2,1)='Iteration'
        NCTIT2(2,1)=9
        ITITL2(3,1)='Number'
        NCTIT2(3,1)=6
C
        ITITL2(1,2)=' '
        NCTIT2(1,2)=0
        ITITL2(2,2)='Convergence'
        NCTIT2(2,2)=11
        ITITL2(3,2)='Measure'
        NCTIT2(3,2)=7
C
        ITITL2(1,3)='Residual'
        NCTIT2(1,3)=8
        ITITL2(2,3)='Standard'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Deviation'
        NCTIT2(3,3)=9
C
        ITITL2(1,4)=' * '
        NCTIT2(1,4)=3
        ITITL2(2,4)=' * '
        NCTIT2(2,4)=3
        ITITL2(3,4)=' * '
        NCTIT2(3,4)=3
C
        ITITL2(1,5)=' '
        NCTIT2(1,5)=0
        ITITL2(2,5)='Parameter'
        NCTIT2(2,5)=10
        ITITL2(3,5)='Estimates'
        NCTIT2(3,5)=10
C
        NMAX=0
        DO3110I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.1)NTOT(I)=10
          IF(I.EQ.4)NTOT(I)=3
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IF(I.EQ.4)ITYPCO(I)='ALPH'
          IDIGIT(I)=-7
          IF(I.EQ.1 .OR. I.EQ.4)THEN
            IDIGIT(I)=0
          ENDIF
 3110   CONTINUE
C
        ICNT=0
C
      ENDIF
C
C               *******************************************************
C               **  STEP 2.6--                                       **
C               **  TREAT THE SPECIAL CASE WHERE NO PARAMETERS       **
C               **  EXIST IN THE MODEL--                             **
C               **  THAT IS, WE ARE REALLY INTERESTED                **
C               **  IN GENERATING PREDICTED VALUES AND RESIDUALS     **
C               **  FROM A FULLY-SPECIFIED MODEL.                    **
C               **  (THIS IS USEFUL FOR MANUALLY ARRIVING AT         **
C               **  REASONABLE STARTING VALUES FOR A MORE            **
C               **  COMPLICATED FIT;                                 **
C               **  AND ALSO FOR TESTING THE GOODNESS OF AN          **
C               **  ALREADY-DERIVED                                  **
C               **  FIT FOR ONE DOMAIN OVER A SECOND DOMAIN.)        **
C               *******************************************************
C
      ISTEPN='2.6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMPAR.LE.0)THEN
        DO3000I=1,N
          IF(NUMVAR.LE.0)GOTO3090
          PARAM5(NUMPAR+1)=X1(I)
          IF(NUMVAR.LE.1)GOTO3090
          PARAM5(NUMPAR+2)=X2(I)
          IF(NUMVAR.LE.2)GOTO3090
          PARAM5(NUMPAR+3)=X3(I)
          IF(NUMVAR.LE.3)GOTO3090
          PARAM5(NUMPAR+4)=X4(I)
          IF(NUMVAR.LE.4)GOTO3090
          PARAM5(NUMPAR+5)=X5(I)
          IF(NUMVAR.LE.5)GOTO3090
          PARAM5(NUMPAR+6)=X6(I)
          IF(NUMVAR.LE.6)GOTO3090
          PARAM5(NUMPAR+7)=X7(I)
          IF(NUMVAR.LE.7)GOTO3090
          PARAM5(NUMPAR+8)=X8(I)
          IF(NUMVAR.LE.8)GOTO3090
          PARAM5(NUMPAR+9)=X9(I)
          IF(NUMVAR.LE.9)GOTO3090
          PARAM5(NUMPAR+10)=X10(I)
          IF(NUMVAR.LE.10)GOTO3090
          PARAM5(NUMPAR+11)=X11(I)
          IF(NUMVAR.LE.11)GOTO3090
          PARAM5(NUMPAR+12)=X12(I)
          IF(NUMVAR.LE.12)GOTO3090
          PARAM5(NUMPAR+13)=X13(I)
          IF(NUMVAR.LE.13)GOTO3090
          PARAM5(NUMPAR+14)=X14(I)
          IF(NUMVAR.LE.14)GOTO3090
          PARAM5(NUMPAR+15)=X15(I)
C
 3090     CONTINUE
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM5,IPARN5,IPARN6,NUMPV,
     1    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,PRED2(I),
     1    IBUGCO,IBUGEV,IERROR)
          PRED2(I)=PRED2(I)*WSQRT(I)
          IF(IERROR.EQ.'YES')GOTO9000
 3000   CONTINUE
C
        DO3100I=1,N
          RES2(I)=Y2(I)-PRED2(I)
 3100   CONTINUE
C
        SUM=0.0
        DO3200I=1,N
          SUM=SUM+RES2(I)**2
 3200   CONTINUE
        RESSS=SUM
C
        IRESDF=N
        RESDF=N
        RESMS=0.0
        IF(RESDF.GT.0.0)RESMS=RESSS/RESDF
        RESSD=0.0
        IF(RESMS.GT.0.0)RESSD=SQRT(RESMS)
        GOTO5000
      ENDIF
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  USING THE GIVEN STARTING VALUES FOR THE         **
C               **  PARAMETERS,                                     **
C               **  COMPUTE PREDICTED VALUES AND EXACT DERIVATIVES; **
C               **  THEN CHECK THE CORRECTNESS OF THE DERIVATIVES   **
C               **  FORMULAE                                        **
C               **  BY APPROXIMATING THE DERIVATIVES WITH DIFFERENCES*
C               **  AND COMPARING THE EXACT DERIVATIVES WITH THE    **
C               **  DIFFERENCES.                                    **
C               ******************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,425)
  425   FORMAT('    GRADIENTS FROM DIFFERENCES')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO1201J=1,NUMPAR
        PARAM5(J)=PARAM3(J)
 1201 CONTINUE
      DO1200I=1,N
        IF(NUMVAR.LE.0)GOTO1205
        PARAM5(NUMPAR+1)=X1(I)
        IF(NUMVAR.LE.1)GOTO1205
        PARAM5(NUMPAR+2)=X2(I)
        IF(NUMVAR.LE.2)GOTO1205
        PARAM5(NUMPAR+3)=X3(I)
        IF(NUMVAR.LE.3)GOTO1205
        PARAM5(NUMPAR+4)=X4(I)
        IF(NUMVAR.LE.4)GOTO1205
        PARAM5(NUMPAR+5)=X5(I)
        IF(NUMVAR.LE.5)GOTO1205
        PARAM5(NUMPAR+6)=X6(I)
        IF(NUMVAR.LE.6)GOTO1205
        PARAM5(NUMPAR+7)=X7(I)
        IF(NUMVAR.LE.7)GOTO1205
        PARAM5(NUMPAR+8)=X8(I)
        IF(NUMVAR.LE.8)GOTO1205
        PARAM5(NUMPAR+9)=X9(I)
        IF(NUMVAR.LE.9)GOTO1205
        PARAM5(NUMPAR+10)=X10(I)
        IF(NUMVAR.LE.10)GOTO1205
        PARAM5(NUMPAR+11)=X11(I)
        IF(NUMVAR.LE.11)GOTO1205
        PARAM5(NUMPAR+12)=X12(I)
        IF(NUMVAR.LE.12)GOTO1205
        PARAM5(NUMPAR+13)=X13(I)
        IF(NUMVAR.LE.13)GOTO1205
        PARAM5(NUMPAR+14)=X14(I)
        IF(NUMVAR.LE.14)GOTO1205
        PARAM5(NUMPAR+15)=X15(I)
C
 1205   CONTINUE
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM5,IPARN5,IPARN6,NUMPV,
     1  IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,DUM1(I),
     1  IBUGCO,IBUGEV,IERROR)
        DUM1(I)=DUM1(I)*WSQRT(I)
        IF(IERROR.EQ.'YES')GOTO9000
 1200 CONTINUE
C
      SUM=0.0
      DO1140I=1,N
        G(I)=Y2(I)-DUM1(I)
        SUM=SUM+G(I)**2
 1140 CONTINUE
      SSN=SUM
C
      DO1210J=1,NUMPAR
        PARAM7(J)=PARAM3(J)
 1210 CONTINUE
C
      DO1220J=1,NP
        IF(ICON3(J).EQ.1)GOTO1220
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,119)J
  119     FORMAT('PARAMETER NUMBER ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        PARAM7(J)=PARAM3(J)
        IF(PARAM7(J).EQ.0.0)H=0.001
        IF(PARAM7(J).NE.0.0)H=PARAM3(J)*0.01
        PARAM7(J)=PARAM3(J)+H
        DO1230I=1,N
          IF(NUMVAR.LE.0)GOTO1235
          PARAM7(NUMPAR+1)=X1(I)
          IF(NUMVAR.LE.1)GOTO1235
          PARAM7(NUMPAR+2)=X2(I)
          IF(NUMVAR.LE.2)GOTO1235
          PARAM7(NUMPAR+3)=X3(I)
          IF(NUMVAR.LE.3)GOTO1235
          PARAM7(NUMPAR+4)=X4(I)
          IF(NUMVAR.LE.4)GOTO1235
          PARAM7(NUMPAR+5)=X5(I)
          IF(NUMVAR.LE.5)GOTO1235
          PARAM7(NUMPAR+6)=X6(I)
          IF(NUMVAR.LE.6)GOTO1235
          PARAM7(NUMPAR+7)=X7(I)
          IF(NUMVAR.LE.7)GOTO1235
          PARAM7(NUMPAR+8)=X8(I)
          IF(NUMVAR.LE.8)GOTO1235
          PARAM7(NUMPAR+9)=X9(I)
          IF(NUMVAR.LE.9)GOTO1235
          PARAM7(NUMPAR+10)=X10(I)
          IF(NUMVAR.LE.10)GOTO1235
          PARAM7(NUMPAR+11)=X11(I)
          IF(NUMVAR.LE.11)GOTO1235
          PARAM7(NUMPAR+12)=X12(I)
          IF(NUMVAR.LE.12)GOTO1235
          PARAM7(NUMPAR+13)=X13(I)
          IF(NUMVAR.LE.13)GOTO1235
          PARAM7(NUMPAR+14)=X14(I)
          IF(NUMVAR.LE.14)GOTO1235
          PARAM7(NUMPAR+15)=X15(I)
C
 1235     CONTINUE
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM7,IPARN5,IPARN6,NUMPV,
     1    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,PRED2(I),
     1    IBUGCO,IBUGEV,IERROR)
          PRED2(I)=PRED2(I)*WSQRT(I)
          IF(IERROR.EQ.'YES')GOTO9000
          K=I+(J-1)*N
          V(K)=(PRED2(I)-DUM1(I))/H
          V(K)=-V(K)
 1230   CONTINUE
C
        SUM=0.0
        DO1250I=1,N
          RES2(I)=Y2(I)-PRED2(I)
          SUM=SUM+RES2(I)**2
 1250   CONTINUE
        S=SUM
C
        DO 1260 I=1,N
          RES2(I)=(RES2(I)-G(I))/H
 1260   CONTINUE
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
          DO1261I=1,N
            WRITE(ICOUT,120)RES2(I)
  120       FORMAT(G15.7)
            CALL DPWRST('XXX','BUG ')
 1261     CONTINUE
        ENDIF
C
        PARAM7(J)=PARAM3(J)
 1220 CONTINUE
C
C
C
C               ************************************************
C               **  STEP 4--                                  **
C               **  START THE ITERATIVE CYCLE.                **
C               **          ITS = THE ITERATION NUMBER.       **
C               **          NITS = THE NUMBER OF ITERATIONS.  **
C               ************************************************
C
   50 CONTINUE
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITS=0
   40 CONTINUE
      ITS=ITS+1
      NITS=0
C
C               *****************************************************
C               **  STEP 5--                                       **
C               **  FILL THE VECTOR V(.) WITH EVALUATED DERIVATIVES**
C               **  BASED ON THE STARTING VALUES FOR THE PARAMETERS.*
C               **  ALL THE DERIVATIVES WITH RESPECT TO PARAMETER 1**
C               **  GO IN THE FIRST N LOCATIONS.                   **
C               **  ALL THE DERIVATIVES WITH RESPECT TO PARAMETER 2**
C               **  GO IN THE NEXT N LOCATIONS.                    **
C               **  ALL THE DERIVATIVES WITH RESPECT TO PARAMETER 3**
C               **  GO IN THE FOLLOWING N LOCATIONS, ETC.          **
C               **  ALSO COMPUTE A SUM OF SQUARED DEVIATIONS       **
C               **  BASED ON THE CURRENT VALUES FOR THE PARAMETERS **
C               **  (THIS WILL BE USED FOR COMPARATIVE PURPOSES    **
C               **  WITHIN THE ITERATION).                         **
C               *****************************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1301J=1,NUMPAR
        PARAM5(J)=PARAM3(J)
 1301 CONTINUE
      DO1300I=1,N
        IF(NUMVAR.LE.0)GOTO1305
        PARAM5(NUMPAR+1)=X1(I)
        IF(NUMVAR.LE.1)GOTO1305
        PARAM5(NUMPAR+2)=X2(I)
        IF(NUMVAR.LE.2)GOTO1305
        PARAM5(NUMPAR+3)=X3(I)
        IF(NUMVAR.LE.3)GOTO1305
        PARAM5(NUMPAR+4)=X4(I)
        IF(NUMVAR.LE.4)GOTO1305
        PARAM5(NUMPAR+5)=X5(I)
        IF(NUMVAR.LE.5)GOTO1305
        PARAM5(NUMPAR+6)=X6(I)
        IF(NUMVAR.LE.6)GOTO1305
        PARAM5(NUMPAR+7)=X7(I)
        IF(NUMVAR.LE.7)GOTO1305
        PARAM5(NUMPAR+8)=X8(I)
        IF(NUMVAR.LE.8)GOTO1305
        PARAM5(NUMPAR+9)=X9(I)
        IF(NUMVAR.LE.9)GOTO1305
        PARAM5(NUMPAR+10)=X10(I)
        IF(NUMVAR.LE.10)GOTO1305
        PARAM5(NUMPAR+11)=X11(I)
        IF(NUMVAR.LE.11)GOTO1305
        PARAM5(NUMPAR+12)=X12(I)
        IF(NUMVAR.LE.12)GOTO1305
        PARAM5(NUMPAR+13)=X13(I)
        IF(NUMVAR.LE.13)GOTO1305
        PARAM5(NUMPAR+14)=X14(I)
        IF(NUMVAR.LE.14)GOTO1305
        PARAM5(NUMPAR+15)=X15(I)
C
 1305   CONTINUE
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM5,IPARN5,IPARN6,NUMPV,
     1  IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,PRED2(I),
     1  IBUGCO,IBUGEV,IERROR)
        PRED2(I)=PRED2(I)*WSQRT(I)
        IF(IERROR.EQ.'YES')GOTO9000
 1300 CONTINUE
C
      DO1310J=1,NUMPAR
        PARAM7(J)=PARAM3(J)
 1310 CONTINUE
      DO1320J=1,NUMPAR
        IF(PARAM3(J).EQ.0.0)H=0.001
        IF(PARAM3(J).NE.0.0)H=PARAM3(J)*0.01
        PARAM7(J)=PARAM3(J)+H
        DO1330I=1,N
          IF(NUMVAR.LE.0)GOTO1335
          PARAM7(NUMPAR+1)=X1(I)
          IF(NUMVAR.LE.1)GOTO1335
          PARAM7(NUMPAR+2)=X2(I)
          IF(NUMVAR.LE.2)GOTO1335
          PARAM7(NUMPAR+3)=X3(I)
          IF(NUMVAR.LE.3)GOTO1335
          PARAM7(NUMPAR+4)=X4(I)
          IF(NUMVAR.LE.4)GOTO1335
          PARAM7(NUMPAR+5)=X5(I)
          IF(NUMVAR.LE.5)GOTO1335
          PARAM7(NUMPAR+6)=X6(I)
          IF(NUMVAR.LE.6)GOTO1335
          PARAM7(NUMPAR+7)=X7(I)
          IF(NUMVAR.LE.7)GOTO1335
          PARAM7(NUMPAR+8)=X8(I)
          IF(NUMVAR.LE.8)GOTO1335
          PARAM7(NUMPAR+9)=X9(I)
          IF(NUMVAR.LE.9)GOTO1335
          PARAM7(NUMPAR+10)=X10(I)
          IF(NUMVAR.LE.10)GOTO1335
          PARAM7(NUMPAR+11)=X11(I)
          IF(NUMVAR.LE.11)GOTO1335
          PARAM7(NUMPAR+12)=X12(I)
          IF(NUMVAR.LE.12)GOTO1335
          PARAM7(NUMPAR+13)=X13(I)
          IF(NUMVAR.LE.13)GOTO1335
          PARAM7(NUMPAR+14)=X14(I)
          IF(NUMVAR.LE.14)GOTO1335
          PARAM7(NUMPAR+15)=X15(I)
C
 1335     CONTINUE
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM7,IPARN5,IPARN6,NUMPV,
     1    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,Y1,
     1    IBUGCO,IBUGEV,IERROR)
          Y1=Y1*WSQRT(I)
          IF(IERROR.EQ.'YES')GOTO9000
          K=I+(J-1)*N
          V(K)=(Y1-PRED2(I))/H
          V(K)=-V(K)
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
            WRITE(ICOUT,1333)J,I,PARAM3(J),PARAM7(J),H,
     1                       Y1,PRED2(I),V(K)
 1333       FORMAT(I2,I4,3F10.5,3D14.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
 1330   CONTINUE
        PARAM7(J)=PARAM3(J)
 1320 CONTINUE
C
      SUM=0.0
      DO1340I=1,N
        RES2(I)=Y2(I)-PRED2(I)
        SUM=SUM+RES2(I)**2
 1340 CONTINUE
      SSINIT=SUM
      SSINMS=0.0
      IF(DF.GT.0.0)SSINMS=SSINIT/DF
      SDINIT=0.0
      IF(SSINMS.GT.0.0)SDINIT=SQRT(SSINMS)
      IF(NCONST.EQ.0) GO TO 38
        J = 0
        DO 58 I=1,NPST
          K = ICON3(I)
          J = J + K
          IF(J.EQ.0.OR.K.EQ.1) GO TO 58
          II = (I-1)*N
          KK = (I-J-1)*N
          DO 54 K=1,N
            V(KK+K) = V(II+K)
   54     CONTINUE
   58   CONTINUE
   38 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2401)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2402)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2403)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2404)ITS
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2405)(PARAM3(J),J=1,NUMPAR)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2406)SDINIT
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2411)
        CALL DPWRST('XXX','BUG ')
        IMAX=N
        JMAX=NUMPAR
        WRITE(ICOUT,2412)IMAX,JMAX
        CALL DPWRST('XXX','BUG ')
 2401   FORMAT('---------- AFTER STEP 5 OF DPFIT2 ----------')
 2402   FORMAT('(THAT IS, AFTER FILLING V(.) WITH DERIVATIVES')
 2403   FORMAT('BASED ON CURRENT VALUES OF PARAMETERS)')
 2404   FORMAT('ITERATION = ',I5)
 2405   FORMAT('CURRENT PARAMETERS = ',8F13.6)
 2406   FORMAT('CURRENT RESIDUAL STANDARD DEVIATION = ',F20.10)
 2411   FORMAT('THE "MATRIX" V(.) AND THE VECTOR RES--')
 2412   FORMAT(I5,' ROWS BY ',I5,' COLUMNS (PLUS AN EXTRA ',
     1         'COLUMN FOR RES)')
        DO2420I=1,IMAX
          L=0
          DO2430J=1,JMAX
            L=L+1
            K=(J-1)*IMAX+I
            DUM(L)=V(K)
 2430     CONTINUE
          LMAX=L
          WRITE(ICOUT,2431)(DUM(L),L=1,LMAX),RES2(I)
 2431     FORMAT(10F13.7)
          CALL DPWRST('XXX','BUG ')
 2420   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2441)
        CALL DPWRST('XXX','BUG ')
        IMAX=NUMPAR
        JMAX=NUMPAR+4
        WRITE(ICOUT,2442)IMAX,JMAX
 2441   FORMAT('THE    MATRIX    WS--')
        CALL DPWRST('XXX','BUG ')
 2442   FORMAT(I5,' ROWS BY ',I5,' COLUMNS')
        DO2450I=1,IMAX
          L=0
          DO2460J=1,JMAX
            L=L+1
            K=(J-1)*IMAX+I
            DUM(L)=WS(K)
 2460     CONTINUE
          LMAX=L
          WRITE(ICOUT,2461)(DUM(L),L=1,LMAX)
 2461     FORMAT(10F13.7)
          CALL DPWRST('XXX','BUG ')
 2450   CONTINUE
      ENDIF
C
C     PRINT RESULTS FOR CURRENT ITERATION
C
      IF(IPRINT.EQ.'ON')THEN
        IF(ICNT.GT.55)THEN
          CALL DPDTA5(ITITLE,NCTITL,
     1                ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                IFLAGS,IFLAGE,
     1                ISUBRO,IBUGA3,IERROR)
          CALL DPFLSH(IPR,IBUGA3,ISUBRO,IFOUND,IERROR)
          ICNT=0
        ELSE
          NLINE=((NUMPAR-1)/NTEMP) + 1
          DO3910KK=1,NLINE
            ICNT=ICNT+1
            IVALUE(ICNT,4)=' * '
            NCVALU(ICNT,4)=3
            AMAT(ICNT,1)=REAL(ITS)
            AMAT(ICNT,2)=ALAMBA
            AMAT(ICNT,3)=SDINIT
            INDX1=(KK-1)*NTEMP+1
            INDX2=KK*NTEMP
            IF(INDX2.GT.NUMPAR)INDX2=NUMPAR
            ICNT3=0
            DO3920JJ=INDX1,INDX2
              ICNT3=ICNT3+1
              AMAT(ICNT,4+ICNT3)=PARAM3(JJ)
 3920       CONTINUE
 3910     CONTINUE
        ENDIF
      ENDIF
C
C               ******************************************************
C               **  STEP 6--                                        **
C               **  TO ENHANCE COMPUTATIONAL ACCURACY,              **
C               **  SCALE THE "MATRIX" V(.) OF DERIVATIVES          **
C               **  SO THAT COLUMNS HAVE LENGTH 1.                  **
C               **  STORE THE SCALE FACTOR FOR COLUMN (PARAMETER) 1 **
C               **  IN WS(ID+1).                                    **
C               **  STORE THE SCALE FACTOR FOR COLUMN (PARAMETER) 2 **
C               **  IN WS(ID+2).                                    **
C               **  STORE THE SCALE FACTOR FOR COLUMN (PARAMETER) 3 **
C               **  IN WS(ID+3),                                    **
C               ******************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO 1 I=1,NP
        II=(I-1)*N
        SUM=0.D0
        DO 2 J=1,N
          SUM=SUM+V(II+J)**2
    2   CONTINUE
        IF(SUM.EQ.0.0D0) THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,121)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,123)IPARN3(I),IPARN4(I)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,124)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,125)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,126)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,127)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,128)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,129)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,130)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,131)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,132)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,133)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,134)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,135)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,136)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,137)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,138)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,139)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,140)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,142)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,143)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,144)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,145)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,146)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,147)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,148)
          CALL DPWRST('XXX','BUG ')
  121     FORMAT('      *** COMPUTATIONAL INSTABILITY ENCOUNTERED ***')
  122     FORMAT('      IN COMPUTING THE NUMERICAL DERIVIATIVE')
  123     FORMAT('      FOR PARAMETER ',A4,A4,', IT WAS FOUND THAT')
  124     FORMAT('      THE CALCULATED DERIVATIVE WAS IDENTICALLY ZERO')
  125     FORMAT('      FOR EVERY VALUE OF THE INDEPENDENT')
  126     FORMAT('      VARIABLE(S).  ')
  127     FORMAT('      THIS IS USUALLY DUE TO INTERNAL DIFFERENCING')
  128     FORMAT('      ON A FINITE WORD LENGTH COMPUTER')
  129     FORMAT('      OF 2 VERY LARGE NUMBERS WHICH ARE')
  130     FORMAT('      NEARLY IDENTICAL.')
  131     FORMAT('      PROBABLE CAUSE 1--RAISING A LARGE')
  132     FORMAT('      VARIABLE VALUE TO A MODERATE OR LARGE POWER.')
  133     FORMAT('      THIS FREQUENTLY OCCURS FOR THE')
  134     FORMAT('      ADDITIVE CONSTANT PARAMETER IN A MODEL')
  135     FORMAT('      WHICH HAS LARGE INDEPENDENT VARIABLE VALUES')
  136     FORMAT('      BEING RAISED TO SOME POWER.')
  137     FORMAT('      SUGGESTED SOLUTION--SCALE DOWN')
  138     FORMAT('      THE INDEPENDENT VARIABLE VALUES ')
  139     FORMAT('      (IF POSSIBLE) TO A RANGE NEAR 1 TO 10,')
  140     FORMAT('      REFIT THE NEW MODEL, AND APPROPRIATELY')
  141     FORMAT('      CONVERT THE COEFFICENTS OF THE NEW MODEL')
  142     FORMAT('      BACK INTO COEFFICIENTS OF THE ORIGINAL MODEL')
  143     FORMAT('      PROBABLE CAUSE 2--RAISING A MODERATE ')
  144     FORMAT('      VARIABLE VALUE TO A LARGE POWER.')
  145     FORMAT('      THE DIFFERENT STARTING VALUES USUALLY')
  146     FORMAT('      RANGE OVER 10 OR MORE ORDERS OF MAGNITUDE.')
  147     FORMAT('      SUGGESTED SOLUTION--USE MORE MODERATE')
  148     FORMAT('      VALUES OF THE STARTING VALUES.')
          IER = 4
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(SUM.GT.0.0)DS3=DSQRT(SUM)
        IF(SUM.LE.0.0)DS3=0.0
        IF(DS3.LE.0.0)THEN
          WRITE(ICOUT,76)
   76     FORMAT('ERROR IN DPFIT2--DENOMINATOR DS3 = 0.0 AT FORMAT 76')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        SUM=1.0D0/DS3
        DO 3 J=1,N
          V(II+J)=V(II+J)*SUM
    3   CONTINUE
        WS(ID+I)=SUM
    1 CONTINUE
      WS(ID+I)=SUM
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,100)ITS,ALAMBA,SSINIT
  100   FORMAT (7H   ITS=,I3,8H ALAMBA=,G14.6,7H SUMSQ=,D14.6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************************
C               **  STEP 7--                                         **
C               **  OPERATE ON THE "MATRIX" V(.) AND THE VECTOR RES. **
C               **  PERFORM HOUSEHOLDER TRANSFORMATION ON            **
C               **  SCALED DERIVATIVE MATRIX AND COLUMN OF RESIDUALS,**
C               **  AND TEST FOR SINGULARITIES.                      **
C               *******************************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO 4 I=1,NP
        II=(I-1)*N
        SUM=0.D0
        DO 5 J=I,N
          SUM=SUM+V(II+J)**2
    5   CONTINUE
        IF(SUM.GT.0.0D0)SUM=DSQRT(SUM)
        IF(SUM.LE.0.0D0)SUM=0.0D0
        IF(SUM.GT.100.*EPS) GO TO 24
        IF(ITS.EQ.1) THEN
          SUM = SUM + EPS
          GO TO 24
        ENDIF
        II = I
        J = 1
27      CONTINUE
        IF(ICON3(J).NE.0) II = II + 1
        J = J + 1
        IF (J.LE.II) GO TO 27
C
C       (RANK DEFICIENCY DETECTED--
C       CONTINUE ITERATING WITH PARAMETER II FIXED.
C       GO BACK TO BEGINNING OF CYCLE
C       FOR A NEW ITERATION.
C       NOTE THAT THE INPUT VECTOR ICON3(.) IS HERE
C       BEING ALTERED DUE TO THIS RANK DEFICIENCY.)
C
        ICON3(II) = 1
        WRITE(ICOUT,1122)II
 1122   FORMAT(2X,'PARAMETER',I8,' IS LINEARLY DEPENDENT ON PREVIOUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1123)
 1123   FORMAT(2X,'PARAMETERS, AND WILL THEREFORE BE HELD CONSTANT')
        CALL DPWRST('XXX','BUG ')
        NP = NP - 1
        NCONST = NCONST + 1
        GO TO 40
C
   24   CONTINUE
        IF(V(II+I).GT.0.)SUM=-SUM
        WS(IDA+I)=SUM
        V(II+I)=V(II+I)-SUM
        IF(I.NE.NP) THEN
          IP1 = I+1
          KK=I*N
          DO 7 K=IP1,NP
            SUM=0.D0
            DO 8 J=I,N
              SUM=SUM+V(II+J)*V(KK+J)
    8       CONTINUE
            SUM=-SUM/(WS(IDA+I)*V(II+I))
            DO 9 J=I,N
              V(KK+J)=V(KK+J)-SUM*V(II+J)
    9       CONTINUE
            KK=KK+N
    7     CONTINUE
        ENDIF
        SUM=0.D0
        DO 20 J=I,N
          SUM=SUM+V(II+J)*RES2(J)
   20   CONTINUE
        SUM=-SUM/(WS(IDA+I)*V(II+I))
        DO 21 J=I,N
          RES2(J)=RES2(J)-SUM*V(II+J)
21      CONTINUE
4     CONTINUE
C
C               ******************************************************
C               **  STEP 8--                                        **
C               **  COMPUTE SSR = PARTIAL SUM OF SQUARED RESIDUALS  **
C               **  (NOTE THAT THE RESIDUALS HAVE JUST BEEN ALTERED).*
C               ******************************************************
C
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NP1=NP+1
      SSR=0.D0
      DO 22 I=NP1,N
        SSR=SSR+RES2(I)**2
   22 CONTINUE
C
C               ******************************************************
C               **  STEP 9--                                        **
C               **  ADD ON THE LAMBDA TO THE                        **
C               **  DIAGONAL ELEMENTS OF R'R                        **
C               **  FOR THE LEFT-HAND SIDE OF THE EQUATION.         **
C               **  TRANSFORM THE RIGHT-HAND SIDE OF THE EQUATION.  **
C               **  THE UPPER TRIANGLE OF THE TRANSFORMED MATRIX IS **
C               **  STORED IN WS                                    **
C               **  ELEMENT (I,J) OF THE TRANSFORMED MATRIX STORED IN*
C               **  ELEMENT   (I-1)*NP + J    OF WS.                **
C               ******************************************************
C
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
19    CONTINUE
      IP = 0
      DO 30 I=1,NP
        DO 31 J=1,I
          WS(IP+J)=0.
31      CONTINUE
        WS(IP+I)=ALAMBA
        IP = IP + NP
30    CONTINUE
      IP = 0
      DO 10 I=1,NP
        C(I)=0.
        S=WS(IDA+I)**2
        IP1=I+1
        IL1=I-1
        DO 12 J=1,I
          S=S+WS(IP+J)**2
12      CONTINUE
        IF(S.GT.0.0D0)S=DSQRT(S)
        IF(S.LE.0.0D0)S=0.0D0
        IF(WS(IDA+I).GT.0.)S=-S
        WS(IDU+I)=S
        WW=WS(IDA+I)-S
        IF(I.NE.NP) THEN
          KP = IP + NP
          DO 13 K=IP1,NP
            KK=(K-1)*N+I
            S=V(KK)*WW
            IF(I.NE.1) THEN
              DO 14 J=1,IL1
                S=S+WS(IP+J)*WS(KP+J)
14            CONTINUE
            ENDIF
            S=-S/(WS(IDU+I)*WW)
            WS(IP+K)=V(KK)-S*WW
            DO 15 J=1,I
              WS(KP+J)=WS(KP+J)-S*WS(IP+J)
15          CONTINUE
            KP = KP + NP
13        CONTINUE
        ENDIF
        S=RES2(I)*WW
        DO 16 J=1,I
          S=S+WS(IP+J)*C(J)
16      CONTINUE
        S=-S/(WS(IDU+I)*WW)
        WS(IDX+I)=RES2(I)-S*WW
        DO 17 J=1,I
          C(J)=C(J)-S*WS(IP+J)
17      CONTINUE
        IP = IP + NP
10    CONTINUE
C
C               ******************************************************
C               **  STEP 10--                                       **
C               **  BACK SUBSTITUTE.                                **
C               **  COEFFICIENTS OF THE DERIVATIVE FIT WILL END UP  **
C               **  IN ELEMENTS IDX+1, IDX+2, ... OF WS.            **
C               **  UPDATED VALUES OF THE PARAMETERS WILL END UP    **
C               **  IN ELEMENTS IY+1, IY+2, ... OF WS.              **
C               ******************************************************
C
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC THE FOLLOWING LINE WAS FIXED TO AVOID OVERFLOWS    MAY 1994
CCCCC WS(IY)=WS(IY)/WS(ID)
      IF(ABS(WS(IY)).LE.CPUMAX/10000)THEN
         WS(IY)=WS(IY)/WS(ID)
      ENDIF
C
      KP=(NP-1)*NP
      DO 25 I=2,NP
        K=NP-I+1
        KP1=K+1
        KP = KP - NP
        S=0.D0
        DO 26 J=KP1,NP
          S = S + WS(KP+J)*WS(IDX+J)
26      CONTINUE
        WS(IDX+K)=(WS(IDX+K)-S)/WS(IDU+K)
25    CONTINUE
      SSS=SSR
      J = 0
      DO 32 II=1,NPST
        IF(ICON3(II).NE.0) THEN
          J = J + 1
          WS(IY+II) = PARAM3(II)
          PARAM9(II)=WS(IY+II)
          GO TO 32
        ENDIF
        I = II - J
        SSS=SSS+C(I)**2
        WS(IDX+I) = WS(IDX+I)*WS(ID+I)
        WS(IY+II) = PARAM3(II) - WS(IDX+I)
C
C       TEST FOR CONSTRAINTS
C
        IOP=IPARO3(II)
        IF(IOP.EQ.'NONE')GOTO790
        PLIM=PARLI3(II)
        PUP=WS(IY+II)
        IF(IOP.EQ.'GT')GOTO710
        IF(IOP.EQ.'GE')GOTO720
        IF(IOP.EQ.'EQ')GOTO730
        IF(IOP.EQ.'LE')GOTO740
        IF(IOP.EQ.'LT')GOTO750
        GOTO790
  710   CONTINUE
        IF(PUP.LE.PLIM)PUP=PLIM
        GOTO780
  720   CONTINUE
        IF(PUP.LT.PLIM)PUP=PLIM
        GOTO780
  730   CONTINUE
        IF(PUP.NE.PLIM)PUP=PLIM
        GOTO780
  740   CONTINUE
        IF(PUP.GT.PLIM)PUP=PLIM
        GOTO780
  750   CONTINUE
        IF(PUP.GE.PLIM)PUP=PLIM
        GOTO780
  780   CONTINUE
        WS(IY+II)=PUP
  790   CONTINUE
C
32    CONTINUE
      NITS=NITS+1
C
C               *******************************************************
C               **  STEP 11--                                        **
C               **  BASED ON THE UPDATED PARAMETERS,                 **
C               **  COMPUTE THE LATEST RESIDUAL STANDARD DEVIATION.  **
C               **  TEST FOR CONVERGENCE.                            **
C               *******************************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1350II=1,NUMPAR
        PARAM9(II)=WS(IY+II)
 1350 CONTINUE
      DO1400IZ=1,N
        IF(NUMVAR.LE.0)GOTO1405
        PARAM9(NUMPAR+1)=X1(IZ)
        IF(NUMVAR.LE.1)GOTO1405
        PARAM9(NUMPAR+2)=X2(IZ)
        IF(NUMVAR.LE.2)GOTO1405
        PARAM9(NUMPAR+3)=X3(IZ)
        IF(NUMVAR.LE.3)GOTO1405
        PARAM9(NUMPAR+4)=X4(IZ)
        IF(NUMVAR.LE.4)GOTO1405
        PARAM9(NUMPAR+5)=X5(IZ)
        IF(NUMVAR.LE.5)GOTO1405
        PARAM9(NUMPAR+6)=X6(IZ)
        IF(NUMVAR.LE.6)GOTO1405
        PARAM9(NUMPAR+7)=X7(IZ)
        IF(NUMVAR.LE.7)GOTO1405
        PARAM9(NUMPAR+8)=X8(IZ)
        IF(NUMVAR.LE.8)GOTO1405
        PARAM9(NUMPAR+9)=X9(IZ)
        IF(NUMVAR.LE.9)GOTO1405
        PARAM9(NUMPAR+10)=X10(IZ)
        IF(NUMVAR.LE.10)GOTO1405
        PARAM9(NUMPAR+11)=X11(IZ)
        IF(NUMVAR.LE.11)GOTO1405
        PARAM9(NUMPAR+12)=X12(IZ)
        IF(NUMVAR.LE.12)GOTO1405
        PARAM9(NUMPAR+13)=X13(IZ)
        IF(NUMVAR.LE.13)GOTO1405
        PARAM9(NUMPAR+14)=X14(IZ)
        IF(NUMVAR.LE.14)GOTO1405
        PARAM9(NUMPAR+15)=X15(IZ)
C
 1405   CONTINUE
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM9,IPARN5,IPARN6,NUMPV,
     1  IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,PRED2(IZ),
     1  IBUGCO,IBUGEV,IERROR)
        PRED2(IZ)=PRED2(IZ)*WSQRT(IZ)
        IF(IERROR.EQ.'YES')GOTO9000
 1400 CONTINUE
C
      SUM=0.0
      DO1420IZ=1,N
        DEL=Y2(IZ)-PRED2(IZ)
        SUM=SUM+DEL**2
        IF(SUM.GT.CPUMA2)SUM=CPUMA2
 1420 CONTINUE
      SSN=SUM
      RESSS=SSN
      RESMS=0.0
      IF(DF.GT.0.0)RESMS=RESSS/DF
      RESSD=0.0
      IF(RESMS.GT.0.0)RESSD=SQRT(RESMS)
      IF(RESSD.LT.FITSD)GOTO1440
CCCCC IF(DEL.LT.0.0000001)GOTO1440
      GOTO1460
 1440 CONTINUE
      IC=1
      DO1450I=1,NPST
        PARAM3(I)=WS(IY+I)
 1450 CONTINUE
      GOTO220
C
 1460 CONTINUE
      DPSI=0.5D0*(SSINIT-SSN)/(SSINIT-SSS)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,203)NITS,ALAMBA,SSN,SSS,DPSI,RESSD
  203   FORMAT(1H ,'NITS=',I8,' ALAMBA=',E15.7,' SUMSQ=',D15.7,
     1         ' RES SUMSQ=',D15.7,' PSI =',E15.7,' RESSD = ',D15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,221)SSINIT,SSS,SSN
  221   FORMAT('SSINIT,SSS,SSN = ',3D15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,227)N,NUMPAR,NCONST,NP,DF,RESDF,IRESDF
  227   FORMAT('N,NUMPAR,NCONST,NP,DF,RESDF,IRESDF = ',4I8,2E15.7,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DRAT=0.0
      IF(SSINIT.GT.0.0)DRAT=SSS/SSINIT
      DTOL2=1.0D0-DEPS*50.0D0
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,224)SSINIT,SSS,DRAT,DTOL2
  224   FORMAT('SSINIT,SSS,DRAT,DTOL2= ',4D20.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(DTOL2.LE.DRAT.AND.DRAT.LE.1.0D0)GOTO28
      IF(DPSI.GE.1.0D-04) GO TO 28
      IF(DPSI.GE.0.0D0.AND.RESSD.LT.0.000001)GOTO28
      ALAMBA=ALAMBA*EXPND
      IC=0
      IER=3
      IF(ALAMBA.LT.1.0E6) GO TO 19
      WRITE(ICOUT,45)
   45 FORMAT('*****ERROR--ALAMBA HAS REACHED 1 MILLION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3046)ALAMBA,EXPND
 3046 FORMAT('ALAMBA = ',F20.10,' EXPANSION FACTOR EXPND = ',F20.10)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3047)
 3047 FORMAT('POSSIBLE FIX--RESCALE Y (OR X) DOWN (OR UP)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3049)
 3049 FORMAT('              E.G., DIVIDING OR MULTIPLYING BY, SAY, ',
     1       '1000')
      CALL DPWRST('XXX','BUG ')
      GO TO 910
C
   28 CONTINUE
      DO 29 I=1,NPST
        PARAM3(I)=WS(IY+I)
   29 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,201)(I,PARAM3(I),I=1,NPST)
  201   FORMAT (4(8H PARAM3(,I2,1H),G14.6))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IER=2
      IF(ITS.GE.MAXITS)GO TO 220
      IER=1
      IF(SSINIT.GT.0.0D0)DS1=DSQRT(SSINIT)
      IF(SSINIT.LE.0.0D0)DS1=0.0D0
      IF(SSS.GT.0.0D0)DS2=DSQRT(SSS)
      IF(SSS.LE.0.0D0)DS2=0.0D0
      DRAT1=DS2/DS1
      DRAT2=(DS1-DS2)/(1.0D0+DS1)
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,222)SSINIT,SSS,DS1,DS2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,223)DRAT1,DRAT2,DTOL
  222   FORMAT('SSINIT,SSS,DS1,DS2= ',4D16.9)
        CALL DPWRST('XXX','BUG ')
  223   FORMAT('DRAT1,DRAT2,DTOL = ',3D16.9)
      ENDIF
      IF(DRAT2.LE.DTOL)GOTO220
      IF(NITS.EQ.1) ALAMBA=ALAMBA*COMPR
      IC=0
      GO TO 40
C
C     THE ABOVE 'GO TO 40' MARKS THE USUAL END OF AN ITERATION.
C
C**** CONVERGENCE TEST SATISFIED OR MAXITS REACHED
C
220   CONTINUE
      SUMSQ=SSN
      IF(IC.EQ.1) GOTO78
      IF(SSINIT-SSN.LE.SSINIT*1000.*EPS) GOTO78
      IF(ITS.LT.MAXITS) GO TO 82
      WRITE(ICOUT,204)ITS
  204 FORMAT (1H ,20X,'FAILED TO CONVERGE IN ',I6,' ITERATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9204)
 9204 FORMAT (1H ,20X,'NOTE THAT THE FOLLOWING SUMMARY STATISTICS ARE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9205)
 9205 FORMAT (1H ,20X,'NOT THE BEST THAT CAN BE OBTAINED.')
      CALL DPWRST('XXX','BUG ')
CCCCC JULY 1997.  PRINT SUMMARY INFORMATION EVEN IF MAX ITERATIONS
CCCCC REACHED.  CHANGE FOLLOWING LINE.
CCCCC GO TO 910
      GO TO 2999
   82 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,205)
  205   FORMAT (1H ,20X,'EVIDENCE OF CONVERGENCE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,100)ITS,ALAMBA,SSN
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,201)(I,PARAM3(I),I=1,NPST)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IC=1
      ALAMBA=ALAMBA*COMPR
      GO TO 40
C
78    CONTINUE
      DO 91 I=1,N
        G(I)=RES2(I)
91    CONTINUE
      X0=0.
      ANMNP=N-NP
      IF(N.GT.NP)X0=SUMSQ/ANMNP
      II=0
      DO 33 I=1,NP
        V(II+I)=WS(IDA+I)
        IF(WS(IDA+I).NE.0.0) S=1.0/WS(ID+I)
        DO 34 J=1,I
           V(II+J)=V(II+J)*S
34      CONTINUE
        II=II+N
33    CONTINUE
C
C**** INVERT UPPER TRIANGULAR MATRIX
C
      II=0
      DO 70 I=1,NP
        IF(V(II+I).NE.0.0) V(II+I)=1.0/V(II+I)
        IF(I.NE.1) THEN
          IL1=I-1
          DO 65 J=1,IL1
            S=0.D0
            DO 60 K=J,IL1
              KJ=(K-1)*N+J
              S=S-V(II+K)*V(KJ)
60          CONTINUE
            V(II+J)=S*V(II+I)
65        CONTINUE
        ENDIF
        II=II+N
70    CONTINUE
C
C**** MULTIPLY INVERSE BY ITS TRANSPOSE
C
      L=0
      II=0
      DO 80 I=1,NP
        DO 79 J=1,I
          L=L+1
          S=0.D0
          KK=II
          DO 75 K=I,NP
            S=S+V(KK+I)*V(KK+J)
            KK=KK+N
75        CONTINUE
          WS(L)=S*X0
79      CONTINUE
        II=II+N
80    CONTINUE
C
C               *******************************************************
C               **  STEP 12.2--                                      **
C               **  PRINT OUT FINAL PARAMETER ESTIMATES              **
C               **  AND THEIR STANDARD DEVIATIONS.                   **
C               **  ALSO PRINT OUT THE RESIDUAL STANDARD DEVIATION.  **
C               *******************************************************
C
CCCCC JULY 1997.  PRINT SUMMARY INFORMATION IF MAX ITERATIONS REACHED.
CCCCC ADD FOLLOWING LINE.
CCCCC NOVEMBER 2016.  NEED TO DO SOME COMPUTATIONS IN THIS SECITON,
CCCCC SO JUST SKIP THE CALL TO PRINTING THE TABLE.
 2999 CONTINUE
CCCCC IF(IPRINT.EQ.'ON')THEN
C
C       PRINT REST OF ITERATIONS TABLE
C
        IF(ICNT.GE.1)THEN
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
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
C
        NUMCOL=6
        NUMLIN=2
C
        DO4101J=1,NUMCLI
          DO4102I=1,MAXLIN
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
            NCOLSP(I,J)=0
 4102     CONTINUE
          DO4103I=1,MAXROW
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
            ROWSEP(I)=0
 4103     CONTINUE
 4101   CONTINUE
C
        ITITL2(1,1)=' '
        NCTIT2(1,1)=0
        NCOLSP(1,1)=1
        ITITL2(2,1)=' '
        NCTIT2(2,1)=0
        NCOLSP(2,1)=1
C
        ITITL2(1,2)=' '
        NCTIT2(1,2)=0
        NCOLSP(1,2)=3
        ITITL2(2,2)='Final Parameter Estimates'
        NCTIT2(2,2)=25
        NCOLSP(2,2)=3
C
        ITITL2(1,5)='Approximate'
        NCTIT2(1,5)=11
        NCOLSP(1,5)=1
        ITITL2(2,5)='Standard Deviation'
        NCTIT2(2,5)=18
        NCOLSP(2,5)=1
C
        ITITL2(1,6)=' '
        NCTIT2(1,6)=0
        NCOLSP(1,6)=1
        ITITL2(2,6)='t-Value'
        NCTIT2(2,6)=7
        NCOLSP(2,6)=1
C
        NMAX=0
        DO4110I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.1)NTOT(I)=3
          IF(I.EQ.2)NTOT(I)=10
          IF(I.EQ.3)NTOT(I)=10
          IF(I.EQ.5)NTOT(I)=20
          IF(I.EQ.6)NTOT(I)=10
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IF(I.EQ.2 .OR. I.EQ.3)ITYPCO(I)='ALPH'
          DO4113J=1,MAXROW
            IDIGI2(J,I)=NUMDIG
            IF(I.EQ.1)THEN
              IDIGI2(J,I)=0
            ELSEIF(I.EQ.6)THEN
              IDIGI2(J,I)=4
            ENDIF
 4113     CONTINUE
 4110   CONTINUE
C
        KK=1
        J=0
        ICNT=0
        DO4120I=1,NP
C
 4188     CONTINUE
          II=I+J
          K=ICON3(II)
          J=J+K
C
          IF(K.EQ.1)THEN
            ICNT=ICNT+1
            AMAT(I,1)=REAL(I)
            IVALUE(I,2)(1:4)=IPARN3(I)
            IVALUE(I,2)(5:8)=IPARN4(I)
            NCVALU(I,2)=8
            IVALUE(I,3)(1:4)=' '
            IVALUE(I,3)(5:8)=' '
            NCVALU(I,3)=0
            AMAT(I,4)=PARAM3(II)
            AMAT(I,5)=0.0
            IDIGI2(I,5)=-1
            AMAT(I,6)=0.0
            IDIGI2(I,6)=-1
            GOTO4188
          ENDIF
          IF(WS(KK).GT.0.0)C(I)=SQRT(WS(KK))
          IF(WS(KK).LE.0.0)C(I)=0.0
          KK=KK+I+1
C
          TVALUE=(-999.9)
          IF(C(I).NE.0.0)THEN
            TVALUE=PARAM3(II)/C(I)
          ENDIF
          TVALU2(I)=TVALUE
          ICNT=ICNT+1
          AMAT(I,1)=REAL(II)
          IVALUE(I,2)(1:4)=IPARN3(I)
          IVALUE(I,2)(5:8)=IPARN4(I)
          NCVALU(I,2)=8
          IVALUE(I,3)(1:4)=' '
          IVALUE(I,3)(5:8)=' '
          NCVALU(I,3)=0
          AMAT(I,4)=PARAM3(II)
          AMAT(I,5)=C(I)
          IDIGI2(I,5)=NUMDIG
          IF(C(I).GT.0.0)THEN
            AMAT(I,6)=TVALUE
            IDIGI2(I,6)=4
          ELSE
            AMAT(I,6)=0.0
            IDIGI2(I,6)=-1
          ENDIF
 4120   CONTINUE
C
        IWHTML(1)=50
        IWHTML(2)=100
        IWHTML(3)=100
        IWHTML(4)=150
        IWHTML(5)=200
        IWHTML(6)=150
        IINC=1800
        IINC2=200
        IINC3=1200
        IINC4=2500
        IWRTF(1)=IINC2
        IWRTF(2)=IWRTF(1)+IINC3
        IWRTF(3)=IWRTF(2)+IINC3
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC4
        IWRTF(6)=IWRTF(5)+IINC
C
      IF(IPRINT.EQ.'ON')THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
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
C               *********************************************
C               **  STEP 13--                              **
C               **  PRINT OUT GOODNESS OF FIT INFORMATION  **
C               *********************************************
C
 5000 CONTINUE
C
      IF(IREP.EQ.'YES')THEN
        IFITDF=IRESDF-IREPDF
        FITDF=IFITDF
        FITSS=RESSS-REPSS
        FITMS=100000.0
        IF(FITDF.GT.0.0)FITMS=FITSS/FITDF
        FSTAT=100000.0
        IF(REPMS.GT.0.0)FSTAT=FITMS/REPMS
        CALL FCDF(FSTAT,IFITDF,IREPDF,CDF)
        CDF2=100.0*CDF
        ALFCDF=CDF
      ENDIF
C
      IF(IPRINT.EQ.'ON')THEN
        ITITLE=' '
        NCTITL=0
        ITITLZ=' '
        NCTITZ=0
C
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Residual Standard Deviation:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=RESSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Residual Degrees of Freedom:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=REAL(IRESDF)
        IDIGIT(ICNT)=0
C
        IF(IREP.EQ.'YES')THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Standard Deviation:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REPSD
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Degrees of Freedom:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REAL(IREPDF)
          IDIGIT(ICNT)=0
          IF(IFITDF.LT.1)THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='The Lack of Fit F Test cannot be done'
            NCTEXT(ICNT)=37
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='because the numerator of the F ratio'
            NCTEXT(ICNT)=36
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='has 0 degrees of freedom.  This happens'
            NCTEXT(ICNT)=39
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='when the number of parameters fitted is'
            NCTEXT(ICNT)=39
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='equal to the number of distinct subsets.'
            NCTEXT(ICNT)=40
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ELSE
            ICNT=ICNT+1
            ITEXT(ICNT)='Lack of Fit F Ratio:'
            NCTEXT(ICNT)=20
            AVALUE(ICNT)=FSTAT
            IDIGIT(ICNT)=NUMDIG
            ICNT=ICNT+1
            ITEXT(ICNT)='Lack of Fit F CDF (%):'
            NCTEXT(ICNT)=22
            AVALUE(ICNT)=CDF2
            IDIGIT(ICNT)=NUMDIG
            ICNT=ICNT+1
            ITEXT(ICNT)='Lack of Fit Degrees of Freedom 1:'
            NCTEXT(ICNT)=33
            AVALUE(ICNT)=REAL(IFITDF)
            IDIGIT(ICNT)=0
            ICNT=ICNT+1
            ITEXT(ICNT)='Lack of Fit Degrees of Freedom 2:'
            NCTEXT(ICNT)=33
            AVALUE(ICNT)=REAL(IREPDF)
            IDIGIT(ICNT)=0
          ENDIF
        ENDIF
C
        NUMROW=ICNT
        DO2410I=1,NUMROW
          NTOT(I)=15
 2410   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1              NCTEXT,AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
CCCCC JULY 1997.  MAX ITERATIONS FIX
      IF(ITS.GE.MAXITS) GO TO 910
      IF(NUMPAR.LE.0)GOTO9000
C
C               ********************************************
C               **  PRINT OUT CORRELATIONS OF REGRESSION  **
C               **  COEFFICIENT ESTIMATES                 **
C               **  (IF CALLED FOR)                       **
C               ********************************************
C
      IF(NP.GE.N) GO TO 910
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
         WRITE(ICOUT,108)
108      FORMAT(20X,'CORRELATIONS OF PARAMETER ESTIMATES')
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
      L=0
      KJ = 0
      DO 95 I=1,NP
89      CONTINUE
        II = I + KJ
        K = ICON3(II)
        KJ = KJ + K
        IF(K.EQ.1) GO TO 89
        IF(C(I).NE.0.0) GO TO 83
        C(I) = EPS
        GO TO 95
83      CONTINUE
        DO 94 J=1,I
          L=L+1
          WS(IY+J)=WS(L)/(C(I)*C(J))
          VARCOV(I,J)=WS(L)
          VARCOV(J,I)=WS(L)
          CORR(I,J)=WS(L)/(C(I)*C(J))
          CORR(J,I)=WS(L)/(C(I)*C(J))
  94    CONTINUE
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
          WRITE(ICOUT,209) II,(WS(IY+J),J=1,I)
  209     FORMAT(I6,(10F12.5))
          CALL DPWRST('XXX','BUG ')
        ENDIF
95    CONTINUE
      IF(X0.GT.0.0)X0=SQRT(X0)
      IF(X0.LE.0.0)X0=0.0
      CALL DPWRST('XXX','BUG ')
      DO1501J=1,NUMPAR
        PARAM5(J)=PARAM3(J)
 1501 CONTINUE
      DO1500I=1,N
CCCCC   CALL F(X(I),PARAM3,NUMPAR,PRED2(I))
        IF(NUMVAR.LE.0)GOTO1505
        PARAM5(NUMPAR+1)=X1(I)
        IF(NUMVAR.LE.1)GOTO1505
        PARAM5(NUMPAR+2)=X2(I)
        IF(NUMVAR.LE.2)GOTO1505
        PARAM5(NUMPAR+3)=X3(I)
        IF(NUMVAR.LE.3)GOTO1505
        PARAM5(NUMPAR+4)=X4(I)
        IF(NUMVAR.LE.4)GOTO1505
        PARAM5(NUMPAR+5)=X5(I)
CCCCC   THE FOLLOWING 20 LINES WERE ADDED SEPTEMBER 1991
        IF(NUMVAR.LE.5)GOTO1505
        PARAM5(NUMPAR+6)=X6(I)
        IF(NUMVAR.LE.6)GOTO1505
        PARAM5(NUMPAR+7)=X7(I)
        IF(NUMVAR.LE.7)GOTO1505
        PARAM5(NUMPAR+8)=X8(I)
        IF(NUMVAR.LE.8)GOTO1505
        PARAM5(NUMPAR+9)=X9(I)
        IF(NUMVAR.LE.9)GOTO1505
        PARAM5(NUMPAR+10)=X10(I)
        IF(NUMVAR.LE.10)GOTO1505
        PARAM5(NUMPAR+11)=X11(I)
        IF(NUMVAR.LE.11)GOTO1505
        PARAM5(NUMPAR+12)=X12(I)
        IF(NUMVAR.LE.12)GOTO1505
        PARAM5(NUMPAR+13)=X13(I)
        IF(NUMVAR.LE.13)GOTO1505
        PARAM5(NUMPAR+14)=X14(I)
        IF(NUMVAR.LE.14)GOTO1505
        PARAM5(NUMPAR+15)=X15(I)
C
 1505   CONTINUE
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM5,IPARN5,IPARN6,NUMPV,
     1  IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,PRED2(I),
     1  IBUGCO,IBUGEV,IERROR)
        PRED2(I)=PRED2(I)*WSQRT(I)
        IF(IERROR.EQ.'YES')GOTO9000
 1500 CONTINUE
      DO1510J=1,NUMPAR
        PARAM7(J)=PARAM3(J)
 1510 CONTINUE
      DO1520J=1,NUMPAR
        IF(PARAM3(J).EQ.0.0)H=0.001
        IF(PARAM3(J).NE.0.0)H=PARAM3(J)*0.01
        PARAM7(J)=PARAM3(J)+H
        DO1530I=1,N
CCCCC     CALL F(X(I),PARAM7,NUMPAR,Y1)
          IF(NUMVAR.LE.0)GOTO1535
          PARAM7(NUMPAR+1)=X1(I)
          IF(NUMVAR.LE.1)GOTO1535
          PARAM7(NUMPAR+2)=X2(I)
          IF(NUMVAR.LE.2)GOTO1535
          PARAM7(NUMPAR+3)=X3(I)
          IF(NUMVAR.LE.3)GOTO1535
          PARAM7(NUMPAR+4)=X4(I)
          IF(NUMVAR.LE.4)GOTO1535
          PARAM7(NUMPAR+5)=X5(I)
CCCCC     THE FOLLOWING 20 LINES WERE ADDED SEPTEMBER 1991
          IF(NUMVAR.LE.5)GOTO1535
          PARAM7(NUMPAR+6)=X6(I)
          IF(NUMVAR.LE.6)GOTO1535
          PARAM7(NUMPAR+7)=X7(I)
          IF(NUMVAR.LE.7)GOTO1535
          PARAM7(NUMPAR+8)=X8(I)
          IF(NUMVAR.LE.8)GOTO1535
          PARAM7(NUMPAR+9)=X9(I)
          IF(NUMVAR.LE.9)GOTO1535
          PARAM7(NUMPAR+10)=X10(I)
          IF(NUMVAR.LE.10)GOTO1535
          PARAM7(NUMPAR+11)=X11(I)
          IF(NUMVAR.LE.11)GOTO1535
          PARAM7(NUMPAR+12)=X12(I)
          IF(NUMVAR.LE.12)GOTO1535
          PARAM7(NUMPAR+13)=X13(I)
          IF(NUMVAR.LE.13)GOTO1535
          PARAM7(NUMPAR+14)=X14(I)
          IF(NUMVAR.LE.14)GOTO1535
          PARAM7(NUMPAR+15)=X15(I)
C
 1535     CONTINUE
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM7,IPARN5,IPARN6,NUMPV,
     1    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,Y1,
     1    IBUGCO,IBUGEV,IERROR)
          Y1=Y1*WSQRT(I)
          IF(IERROR.EQ.'YES')GOTO9000
          K=I+(J-1)*N
          V(K)=(Y1-PRED2(I))/H
          V(K)=-V(K)
 1530   CONTINUE
        PARAM7(J)=PARAM3(J)
 1520 CONTINUE
C
      SUM=0.0
      DO1540I=1,N
        RES2(I)=Y2(I)-PRED2(I)
        SUM=SUM+RES2(I)**2
 1540 CONTINUE
      SUMSQ=SUM
C
C**** FORM UNWEIGHTED (RAW) PREDICTED VALUES AND RESIDUALS
C
      DO1550I=1,N
        IF(WSQRT(I).LE.0.0)GOTO1550
        RES2(I)=Y2(I)-PRED2(I)
        RES2(I)=RES2(I)/WSQRT(I)
        PRED2(I)=Y(I)-RES2(I)
 1550 CONTINUE
C
C**** RELOCATE VAR-COV. MATRIX AND STANDARD ERRORS IF NCONST.NE.0.
C
CCCCC THE FOLLOWING LINE WAS CHANGED MARCH 1992
CC900 IF(NCONST.EQ.0) GOTO9000
  900 IF(NCONST.EQ.0) GOTO919
      L = NP*(NP+1)/2
      L2 = NP
      I = NPST
904   K = ICON3(I)
      IF(K.EQ.1) GO TO 903
      C(I) = C(L2)
      L2 = L2 - 1
      J = I
901   K = I*(I-1)/2 + J
      WS(K) = WS(L)
      L = L - 1
902   J = J - 1
      IF(J.LE.0) GO TO 903
      K = ICON3(J)
CCCCC IF(K) 902,901
      IF(K.LT.0)GOTO902
      IF(K.EQ.0)GOTO901
903   I = I - 1
      IF(I.GT.0) GO TO 904
910   NP = NPST
CCCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
  919 CONTINUE
C
CCCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
C               **************************************************
C               **  STEP 81--                                   **
C               **  WRITE INFO OUT TO FILES--                   **
C               **     1) DPST1F.DAT--COEF SDCOEF TCDF          **
C               **     2) DPST2F.DAT--PRED AND SDPRED           **
C               **     3) DPST3F.DAT--PARAMETER VAR-COV MATRIX  **
C               **************************************************
C
 8600 CONTINUE
C
      ISTEPN='86'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFITAU.EQ.'OFF')GOTO9000
C
      IFORMT='(3E15.7,10X,2A4)'
      IF(IAUXDP.NE.7)THEN
        IFORMT=' '
        IF(IAUXDP.LE.9)THEN
          IFORMT='(3Exx.x,10X,2A4)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(4:5),'(I2)')ITOT
          WRITE(IFORMT(7:7),'(I1)')IAUXDP
        ELSE
          IFORMT='(3Exx.xx,10X,2A4)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(4:5),'(I2)')ITOT
          WRITE(IFORMT(7:8),'(I2)')IAUXDP
        ENDIF
      ENDIF
C
      WRITE(IOUNI1,8613)
 8613 FORMAT(1X,
     1         'COEFFICIENT     ',
     2         'COEF SD         ',
     3         'T-VALUE         ')
      DO8610I=1,NUMPAR
        WRITE(IOUNI1,IFORMT)PARAM3(I),C(I),TVALU2(I),
     1               IPARN3(I),IPARN4(I)
 8610 CONTINUE
C
      IFORMT='(30(E15.7,1X))'
      IF(IAUXDP.NE.7)THEN
        IFORMT=' '
        IF(IAUXDP.LE.9)THEN
          IFORMT='(30(Exx.x,1X))'
          ITOT=IAUXDP+8
          WRITE(IFORMT(6:7),'(I2)')ITOT
          WRITE(IFORMT(9:9),'(I1)')IAUXDP
        ELSE
          IFORMT='(30(Exx.xx,1X))'
          ITOT=IAUXDP+8
          WRITE(IFORMT(6:7),'(I2)')ITOT
          WRITE(IFORMT(9:10),'(I2)')IAUXDP
        ENDIF
      ENDIF
C
      WRITE(IOUNI2,8624)
 8624 FORMAT(1X,
     1         'PARAMETER CORR  ',
     2         'PARAMETER COV   ')
      DO8623I=1,NP
        WRITE(IOUNI2,IFORMT) (CORR(I,J),J=1,NP)
        WRITE(IOUNI3,IFORMT) (VARCOV(I,J),J=1,NP)
 8623 CONTINUE
C8625 FORMAT(30(E15.7,1X))
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8612)
 8612   FORMAT('DPST1F.DAT: COEF AND SD(COEF)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8628)
 8628   FORMAT('DPST2F.DAT: PARAMETER CORRELATION MATRIX')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8627)
 8627   FORMAT('DPST3F.DAT: PARAMETER VARIANCE-COVARIANCE MATRIX')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
C               **************************************
C               **  STEP 82--                       **
C               **  CLOSE       THE STORAGE FILES.  **
C               **************************************
C
 8700 CONTINUE
C
      ISTEPN='82'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFITAU.EQ.'ON')THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,NUMVAR,NUMPAR,NUMCHA
 9013   FORMAT('N,NUMVAR,NUMPAR,NUMCHA = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMPAR
          WRITE(ICOUT,9016)I,IPARN3(I),IPARN4(I),PARAM3(I)
 9016     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I) = ',I8,2X,A4,A4,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO9020I=1,N
          WRITE(ICOUT,9021)I,Y(I),X1(I),X2(I),W(I),PRED2(I),RES2(I)
 9021     FORMAT('I,Y(I),X1(I),X2(I),W(I),PRED2(I),RES2(I) = ',
     1           I8,6G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        DO9025I=1,N
          WRITE(ICOUT,9026)I,Y(I),Y2(I),W(I),WSQRT(I)
 9026     FORMAT('I,Y(I),Y2(I),W(I),WSQRT(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9025   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFIT3(Y,X1,X2,X3,X4,X5,X,NLEFT,PARCOV,MAXPAR,
     1                  NUMVAR,IVARN3,IVARN4,W,N,
     1                  MODEL,NUMCHA,PARAM3,IPARN3,IPARN4,NUMPAR,
     1                  ICON3,IANGLU,IPARO3,
     1                  PARLI3,V,SCR,FITSD,FITPOW,ICASFI,
     1                  ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,
     1                  IREP,REPSD,REPDF,RESSD,RESDF,PRED2,RES2,ALFCDF,
     1                  BIC,DUM1,DUM2,G,Z,VSDPRE,
     1                  IFITAC,ALPHA,RSQUAR,ADJRSQ,APRESS,
     1                  ICAPSW,ICAPTY,IFORSW,IFITAU,IAUXDP,
     1                  IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
C
C     NOTE--MAX NUMBER OF OBSERVATIONS N IS 1000 (NOT CHECKED FOR)
C     NOTE--MAX NUMBER OF PARAMETERS K IS 30 (NOT CHECKED FOR)
C     NOTE--DIMENSION OF G IS N (MAX IS 1000)
C     NOTE--DIMENSION OF C IS K (MAX IS 30)
C     NOTE--DIMENSION OF A IS N X K (BUT N X K MAX IS 10000)
C
C     MORE DIMENSION INFO (FROM LSQRT)--
C           B     VECTOR OF COEFFICIENTS (M+1 BY 1).
C           Z     VECTOR OF RESIDUALS (N BY 1).
C           T     VECTOR OF STANDARD DEVIATIONS OF COEFFICIENTS (M+1 BY 1).
C           V     VECTOR OF STANDARD DEVIATIONS OF PREDICTED VALUES
C                    (N BY 1).
C           S     VECTOR OF SQUARED FOURIER COEFFICIENTS (M+3 BY 1).  THE
C                    FIRST M ELEMENTS OF THIS ARRAY ARE SUMS OF SQUARES
C                    WHICH CAN BE USED IN AN ANALYSIS OF VARIANCE.  THE
C                    LAST TWO ELEMENTS OF S ARE NOT COMPUTED IN THIS SUB-
C                    ROUTINE BUT ARE RESERVED FOR QUANTITIES TO BE COMPUTED
C                    IN THE CALLING PROGRAM.
C           E     RESIDUAL SUM OF SQUARES.
C           D     AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN INITIAL
C                    SOLUTION AND THE FIRST ITERATION (IN SUBROUTINE SLVE).
C           SD    RESIDUAL STANDARD DEVIATION.
C           NDF   NO. OF DEGREES OF FREEDOM.
C           SCR   A SCRATCH VECTOR USED FOR INTERNAL CALCULATIONS
C           ID    ID = 0  EVERYTHING IS OK.
C                 ID = 1  AUGMENTED MATRIX IS SINGULAR.
C                 ID = 2  ITERATION PROCEDURE FAILED TO CONVERGE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C     UPDATED         --FEBRUARY  1988.   (MAKE LINE NUMBERS ORDERLY)
C     UPDATED         --MARCH     1988.  (INCLUDE B0 IN MULTILINEAR FIT)
C     UPDATED         --MARCH     1988.  LOFCDF
C     UPDATED         --MARCH     1988.  ERROR ARG. TO CALL TO LSQRT + BRANC
C     UPDATED         --SEPTEMBER 1988.  ERROR BRANCH AFTER CALL TO DPREPS IF EM
C     UPDATED         --SEPTEMBER 1988.  CONSTANT FIT
C     UPDATED         --NOVEMBER  1988.  PROPER TITLE FOR MULTILINEAR
C     UPDATED         --MAY       1989.  MATRIX X ADDED TO INPUT ARG LIST
C     UPDATED         --MAY       1989.  ISUBRO ADDED TO INPUT ARG LIST
C     UPDATED         --NOVEMBER  1989.  S(.) DOUB. PREC. TO SING. PREC.
C     UPDATED         --NOVEMBER  1989.  OMITTED UNNEEDED DOUB. PREC.
C     UPDATED         --JUNE      1990.  SOME DIMENSIONS MOVED TO DPFIT
C     UPDATED         --MARCH     1992.  WRITE COEF SDCOEF TCDF TO FILE
C     UPDATED         --JULY      1993.  WRITE DIAGONAL OF HAT MATRIX,
C                                        PARAMETER COVARIANCE MATRIX TO
C                                        FILE.
C     UPDATED         --SEPTEMBER 1993.  ADD ISUBRO ARG TO LSQRT
C     UPDATED         --JANUARY   1994. WRITE SDPRED & LIMITS TO FILE
C     UPDATED         --FEBRUARY  1994. MERGE JIM AND ALAN UPDATES
C                                       ADD DPST4F.DAT
C     UPDATED         --FEBRUARY  1994. DPWRST: 'BUG ' => 'WRIT'
C     UPDATED         --JUNE      1994. BUG IN DPST4F.DAT OUTPUT FOR
C                                       POLYNOMIAL MODELS.
C     UPDATED         --MAY       1995. FIX SOME I/O
C     UPDATED         --SEPTEMBER 1995. ADD BLANK LINE FOR OUTPUT
C     UPDATED         --JANUARY   1996. FIX BOMB WITH CONSTANT FIT
C     UPDATED         --APRIL     1996. IPRINT SWITCH
C     UPDATED         --APRIL     2002. SUPPORT FOR NO CONSTANT TERM
C     UPDATED         --APRIL     2002. PRINT ERROR MESSAGE IF
C                                       SINGULARITY DETECTED
C     UPDATED         --JUNE      2002. AUGMENT DPST2F.DAT OUTPUT
C     UPDATED         --JUNE      2002. AUGMENT DPST3F.DAT OUTPUT
C     UPDATED         --JUNE      2002. WRITE ANOVA TABLE TO
C                                       DPST5F.DAT
C     UPDATED         --JULY      2003. MODIFY DIMENSIONING OF X TO
C                                       ALLOW MORE FLEXIBILITY BETWEEN
C                                       NUMBER OF ROWS AND COLUMNS.
C     UPDATED         --OCTOBER   2003. SUPPORT HTML, LATEX OUTPUT
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --MAY       2011. USE DPAUFI TO OPEN/CLOSE
C                                       DPST?F.DAT FILES
C     UPDATED         --MAY       2011. USE DPDTA1 AND DPDT5B TO PRINT
C                                       OUTPUT
C     UPDATED         --OCTOBER   2013. COMPUTE BIC STATISTIC
C     UPDATED         --JUNE      2014. USER OPTION TO SUPPRESS
C                                       WRITING TO AUXILLARY FILES
C     UPDATED         --APRIL     2019. USER CAN SPECIFY NUMBER OF
C                                       DECIMAL POINTS FOR AUXILLARY
C                                       FILES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVARN3
      CHARACTER*4 IVARN4
      CHARACTER*4 IPARN3
      CHARACTER*4 IPARN4
      CHARACTER*4 IANGLU
      CHARACTER*4 IPARO3
      CHARACTER*4 ICASFI
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW2HOL
      CHARACTER*4 IW22HO
      CHARACTER*4 IREP
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
CCCCC THE FOLLOWING LINE WAS INSERTED MAY 1989
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IPARN5
      CHARACTER*4 IPARN6
C
      CHARACTER*4 IHOLD3
      CHARACTER*4 IHOLD4
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 MODEL
      CHARACTER*4 IFITAC
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IFITAU
      CHARACTER*20 IFORMT
C
      CHARACTER*4 IOP
C
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=40)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
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
C---------------------------------------------------------------------
C
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT    NOVEMBER 1989
CCCCC BECAUSE THE VARIABLES WERE NEVER USED
CCCCC DOUBLE PRECISION SUM,SSS,SSINIT,SSR,WW,SSN,SUMSQ
C
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT NOVEMBER 1989
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC DOUBLE PRECISION S
C
CCCCC THE FOLLOWING 3 LINES WERE COMMENTED OUT    NOVEMBER 1989
CCCCC BECAUSE THE VARIABLES WERE NEVER USED
CCCCC DOUBLE PRECISION DS1,DS2
CCCCC DOUBLE PRECISION DRAT1,DRAT2
CCCCC DOUBLE PRECISION DRAT
C
      DOUBLE PRECISION DSUM1
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC THE FOLLOWING INCLUDE STATEMENT WAS ADDED MARCH 1992
      INCLUDE 'DPCOF2.INC'
      DIMENSION Y(*)
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION X4(*)
      DIMENSION X5(*)
C
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
C
      DIMENSION W(*)
C
      DIMENSION V(*)
C
      DIMENSION MODEL(*)
C
      DIMENSION IVARN3(*)
      DIMENSION IVARN4(*)
      DIMENSION PARAM3(*)
      DIMENSION IPARN3(*)
      DIMENSION IPARN4(*)
      DIMENSION ICON3(*)
      DIMENSION IPARO3(*)
      DIMENSION PARLI3(*)
C
      DIMENSION ITYPEH(*)
      DIMENSION IW2HOL(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
      DIMENSION IPARN5(80)
      DIMENSION IPARN6(80)
      DIMENSION PARAM5(80)
C
CCCCC JUNE, 1990.  SOME DIMENSIONS MOVED TO DPFIT (FOR STORAGE CONSIDERATIONS)
CCCCC DIMENSION DUM1(MAXOBV)
CCCCC DIMENSION DUM2(MAXOBV)
C
CCCCC DIMENSION G(MAXOBV)
C
      DIMENSION DUM1(*)
      DIMENSION DUM2(*)
C
      DIMENSION G(*)
CCCCC DIMENSION WS(1100)
CCCCC DIMENSION Y0(MAXOBV)
C
CCCCC DIMENSION DUM(80)
CCCCC DIMENSION C(10)  MARCH 1988
      DIMENSION C(80)
CCCCC DIMENSION PARAM7(80)
CCCCC DIMENSION PARAM9(80)
CCCCC JULY 1993.  ADD FOLLOWING LINE
CCCCC DIMENSION PARCOVM(MAXCMF+1,MAXCMF+1)
      DIMENSION PARCOV(MAXPAR+1,MAXPAR+1)
C
CCCCC DIMENSION X(NR,M)
CCCCC DIMENSION X(MAXOBV,MAXCMF)
      DIMENSION X(NLEFT,*)
CCCCC DIMENSION B(M)
      DIMENSION B(100)
CCCCC DIMENSION Z(N)
CCCCC DIMENSION Z(1000)  MARCH 1988
CCCCC DIMENSION Z(MAXOBV)
      DIMENSION Z(*)
CCCCC DIMENSION T(M+1)
      DIMENSION T(101)
CCCCC DIMENSION V(N)
CCCCC DIMENSION VSDPRE(1000)  MARCH 1988
CCCCC DIMENSION VSDPRE(MAXOBV)
      DIMENSION VSDPRE(*)
CCCCC END OF JUNE 1990 CHANGES
CCCCC DIMENSION S(M+2)
      DIMENSION S(102)
CCCCC DIMENSION SCR(10000)
CCCCC DIMENSION SCR(MAXOBW)
      DIMENSION SCR(*)
C ****  THE ABOVE DIMENSION IS PROBABLY WRONG FOR LARGE DATA SETS    JULY 1987
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
      ISUBN1='DPFI'
      ISUBN2='T3  '
      IERROR='NO'
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
CCCCC IF(IBUGA3.EQ.'OFF')GOTO90  MAY 1989
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIT3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)N,NUMVAR,NUMPAR,NUMCHA,ICASFI
   52   FORMAT('N,NUMVAR,NUMPAR,NUMCHA,ICASFI = ',4I8,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IBUGA3,IBUGCO,IBUGEV,ISUBRO
   53   FORMAT('IBUGA3,IBUGCO,IBUGEV,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        DO55I=1,N
          WRITE(ICOUT,56)I,Y(I),X(I,1),X(I,2),X(I,3),X(I,5),W(I)
   56     FORMAT('I,Y(I),X(I,1),X(I,2),X(I,3),X(I,4),W(I) = ',I5,6E13.6)
          CALL DPWRST('XXX','WRIT')
   55   CONTINUE
        DO61J=1,NUMVAR
          WRITE(ICOUT,62)J,IVARN3(J),IVARN4(J)
   62     FORMAT('I,IVARN3(I),IVARN4(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','WRIT')
   61   CONTINUE
        DO66J=1,NUMPAR
          WRITE(ICOUT,67)J,IPARN3(J),IPARN4(J),PARAM3(J),ICON3(J)
   67     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I),ICON3(I) = ',
     1           I8,2X,A4,A4,G15.7,I8)
          CALL DPWRST('XXX','WRIT')
   66   CONTINUE
        WRITE(ICOUT,71)(MODEL(J),J=1,MAX(100,NUMCHA))
   71   FORMAT('FUNCTIONAL EXPRESSION--',100A1)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
CCCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
C               **************************************************
C               **  STEP 0.5--                                  **
C               **   OPEN THE STORAGE FILES                     **
C               **************************************************
C
      ISTEPN='0.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFITAU.EQ.'ON')THEN
        IOP='OPEN'
        IFLAG1=1
        IFLAG2=1
        IFLAG3=1
        IFLAG4=1
        IFLAG5=1
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               **************************************************
C               **  STEP 11--                                   **
C               **  DETERMINE THE PARAMETER NAMES IN THE MODEL  **
C               **  AND THE NUMBER NUMPAR OF PARAMETERS.        **
C               **************************************************
C
      ISTEPN='11'
CCCCC IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)   MAY 1989
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMPAR.GE.1)THEN
        DO1110I=1,NUMPAR
          IPARN5(I)=IPARN3(I)
          IPARN6(I)=IPARN4(I)
          PARAM5(I)=PARAM3(I)
 1110   CONTINUE
      ENDIF
C
      IF(NUMVAR.GE.1)THEN
        DO1120I=1,NUMVAR
          IPARN5(NUMPAR+I)=IVARN3(I)
          IPARN6(NUMPAR+I)=IVARN4(I)
 1120   CONTINUE
      ENDIF
C
      NUMPV=NUMPAR+NUMVAR
C
C               ***************************************************************
C               **  STEP 12--                                                **
C               **  DEFINE VARIOUS CONSTANTS.                                **
C               **  DEFINE NCONST = NUMBER OF PARAMETERS HELD CONSTANT.      **
C               **  DEFINE NP = NUMBER OF NON-CONSTNAT PARAMETERS.           **
C               **  DEFINE DF = DEGREES OF FREEDOM.                          **
C               **  DEFINE SOME WORKING STORAGE START POINTS IN WS.          **
C               ***************************************************************
C
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IREP='NO'
      REPSD=0.0
      REPDF=0.0
      IREPDF=REPDF+0.5
      RESSD=0.0
      RESDF=0.0
      ALFCDF=(-999.99)
C
      IF(NUMPAR.LE.0)GOTO1239
      NPST=NUMPAR
      NCONST=0
C
      DO1210I=1,NUMPAR
        IF(ICON3(I).EQ.1)NCONST=NCONST+1
 1210 CONTINUE
      NP=NUMPAR-NCONST
C
      IF(NP.LE.0)THEN
        WRITE(ICOUT,1220)
 1220   FORMAT('***** ERROR IN FIT--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1221)NP
 1221   FORMAT('      THE NUMBER  OF PARAMETERS TO BE VARIED = ',I8,
     1         ' (LESS THAN ONE)')
        CALL DPWRST('XXX','WRIT')
        IER = 5
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DF=N-NP
      RESDF=DF
      IRESDF=DF+0.5
C
      IC=0
      IER=2
      IDA=NP*NP
      IDU=IDA+NP
      ID =IDU+NP
      IDX=ID +NP
      IY =IDX+NP
C
 1239 CONTINUE
C
      IDEGRE=NUMPAR-1
      IF(IFITAC.EQ.'OFF')IDEGRE=NUMPAR
C
C
C               **********************************************
C               **  STEP 13--                               **
C               **  CHANGE THE WEIGHTS VECTOR W(.)          **
C               **  SO THAT THE SUM OF SQUARED WEIGHTS = 1  **
C               **********************************************
C
      ISTEPN='13'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC SUMWSQ=0.0
CCCCC DO1310I=1,N
CCCCC SUMWSQ=SUMWSQ+W(I)**2
C1310 CONTINUE
CCCCC DO1320I=1,N
CCCCC W(I)=W(I)/SUMWSQ
C1320 CONTINUE
C
C               **************************************************************
C               **  STEP 21--                                               **
C               **  CHECK FOR REPLICATION AND IF EXISTENT                   **
C               **  COMPUTE A (MODEL-FREE) REPLICATION STANDARD DEVIATION.  **
C               **************************************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPREPS(Y,X1,X2,X3,X4,X5,N,NUMVAR,DUM1,DUM2,
     1IREP,REPSS,REPMS,REPSD,REPDF,NUMSET,IBUGA3,IERROR)
      IREPDF=REPDF+0.5
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *******************************************************
C               **  STEP 31--                                        **
C               **  CARRY OUT THE LEAST SQUARES FIT                  **
C               **  NOTE--IT = 1 IMPLIES POLYNOMIAL                  **
C               **        IT = 2 IMPLIES MULTILINEAR                 **
C               **  NOTE--M = DEGREE (IF POLYNOMIAL)                 **
C               **        M = NUMBER OF PARAMETERS (IF MULTILINEAR)  **
C               *******************************************************
C
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASFI.EQ.'MFIT')THEN
        IT=2
        M=NUMPAR
CCCCC   NR=MAXOBV
        NR=NLEFT
CCCCC   THE FOLLOWING 12 LINES WERE COMMENTED OUT MAY 1989
CCCCC   DO3121J=1,NUMPAR
CCCCC     DO3122I=1,N
CCCCC       IF(J.EQ.1)X(I,1)=1.0
CCCCC       IF(J.EQ.2)X(I,2)=X1(I)
CCCCC       IF(J.EQ.3)X(I,3)=X2(I)
CCCCC       IF(J.EQ.4)X(I,4)=X3(I)
CCCCC       IF(J.EQ.5)X(I,5)=X4(I)
CCCCC       IF(J.EQ.6)X(I,6)=X5(I)
CCCCC       IF(IBUGA3.EQ.'ON')WRITE(ICOUT,3123)I,J,X(I,J)
C3123       FORMAT('I,J,X(I,J) = ',I8,I8,E15.7)
CCCCC       IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','WRIT')
C3122     CONTINUE
C3121   CONTINUE
      ELSE
        IT=1
        M=NUMPAR-1
        IF(IFITAC.EQ.'OFF')M=NUMPAR
CCCCC   NR=MAXOBV
        NR=NLEFT
CCCCC   THE FOLLOWING 3 LINES WERE COMMENTED OUT MAY 1989
CCCCC   DO3111I=1,N
CCCCC     X(I,1)=X1(I)
C3111   CONTINUE
      ENDIF
C
C     THE FOLLOWING CHUNK OF CODE WAS ADDED SEPTEMBER 1988
C     TO HANDLE THE CONSTANT FIT (Y = CONSTANT + ERROR) CASE.
C
      IF(IT.EQ.1.AND.M.EQ.0)THEN
        SUMWY=0.0
        SUMW=0.0
        DO3172I=1,N
          SUMWY=SUMWY+W(I)*Y(I)
          SUMW=SUMW+W(I)
 3172   CONTINUE
        AMEAN=SUMWY/SUMW
        B(1)=AMEAN
        DO3173I=1,N
          Z(I)=Y(I)-AMEAN
 3173   CONTINUE
        NDF=N-1
        ANDF=NDF
        AN=N
        SUMWY=0.0
        DO3174I=1,N
          SUMWY=SUMWY+W(I)*Z(I)**2
 3174   CONTINUE
        SD=0.0
        IF(NDF.GT.0)SD=SUMWY/ANDF
        IF(SD.LE.0.0)SD=0.0
        IF(SD.GT.0.0)SD=SQRT(SD)
        T(1)=SD/SQRT(AN)
        GOTO3190
      ELSE
C
CCCCC   APRIL 2002.  CHECK FOR CERTAIN KINDS OF SINGULARITIES IN
CCCCC                MULTI-LINEAR FITS:
CCCCC                1) ANY COLUMNS ARE CONSTANTS.
CCCCC                2) ANY COLUMNS ARE EQUAL.
        IF(ICASFI.EQ.'MFIT')THEN
          IF(IFITAC.EQ.'ON')THEN
            ISTRT=2
            ISTOP=NUMPAR
          ELSE
            ISTRT=1
            ISTOP=NUMPAR
          ENDIF
          DO3176J=ISTRT,ISTOP
            AHOLD=X(1,J)
            DO3178I=1,N
              IF(AHOLD.NE.X(I,J))GOTO3176
 3178       CONTINUE
            WRITE(ICOUT,3181)
 3181       FORMAT('***** FROM DPFIT3, MULTI-LINEAR FIT CASE--')
            CALL DPWRST('XXX','WRIT')
            INDX=J
            IF(IFITAC.EQ.'ON')INDX=J-1
            WRITE(ICOUT,3183)IVARN3(INDX),IVARN4(INDX),AHOLD
 3183       FORMAT('      VARIABLE ',A4,A4,' HAS ALL VALUES = ',E15.7)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3185)
 3185       FORMAT('      THIS RESULTS IN A SINGULAR MATRIX.  NO FIT ',
     1             'PERFORMED.')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GOTO9000
 3176     CONTINUE
C
          DO13176J=ISTRT,ISTOP
            DO13179K=ISTRT,ISTOP
              IF(J.EQ.K)GOTO13179
              DO13181I=1,N
                IF(X(I,J).NE.X(I,K))GOTO13179
13181         CONTINUE
              WRITE(ICOUT,3181)
              CALL DPWRST('XXX','WRIT')
              INDX=J
              INDX2=K
              IF(IFITAC.EQ.'ON')THEN
                INDX=J-1
                INDX2=K-1
              ENDIF
              WRITE(ICOUT,13183)IVARN3(INDX),IVARN4(INDX),IVARN3(INDX2),
     1                          IVARN4(INDX2)
13183         FORMAT('      VARIABLE ',A4,A4,' HAS ALL VALUES = TO ',
     1               'VARIABLE ',A4,A4)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,13185)
13185         FORMAT('      THIS RESULTS IN A SINGULAR MATRIX.  NO ',
     1               'FIT PERFORMED.')
              CALL DPWRST('XXX','WRIT')
              IERROR='YES'
              GOTO9000
13179       CONTINUE
13176     CONTINUE
        ENDIF
C
      ENDIF
C
      CALL LSQRTX(Y,W,N,X,NR,M,IT,
     1            B,Z,T,VSDPRE,S,E,D,SD,NDF,SCR,ID,IFITAC,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
 3190 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
        WRITE(ICOUT,3191)N,M,NUMPAR
 3191   FORMAT('N,M,NUMPAR = ',3I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C               *******************************************************
C               **  STEP 32--                                        **
C               **  IF NEEDED, COMPUTE PREDICTED VALUES              **
C               **  AND RESIDUALS.                                   **
C               **  COPY OVER PARAMETERS, ETC.                       **
C               *******************************************************
C
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC JUNE 2002.  ADD SOME COMPUTATIONS USED FOR THE ANOVA TABLE
C
      IWRITE='OFF'
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
C
      DSUM1=0.0D0
      DO3210I=1,N
        RES2(I)=Z(I)
        PRED2(I)=Y(I)-RES2(I)
        DSUM1=DSUM1 + DBLE(PRED2(I) - YMEAN)**2
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
          WRITE(ICOUT,3211)I,Y(I),PRED2(I),RES2(I)
 3211     FORMAT('I,Y(I),PRED2(I),RES2(I) = ',I8,3E15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
 3210 CONTINUE
C
      SSR=REAL(DSUM1)
C
      DO3220I=1,NUMPAR
        PARAM3(I)=B(I)
        C(I)=T(I)
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
          WRITE(ICOUT,3221)I,PARAM3(I),C(I)
 3221     FORMAT('I,PARAM3(I),C(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
 3220 CONTINUE
C
      RESSD=SD
      RESDF=NDF
      RESMS=RESSD*RESSD
      RESSS=RESMS*RESDF
C
C     COMPUTE BIC VALUE:
C
C     BIC = N*LOG(RESVAR) + P*LOG(N)
C
C     NOTE THAT RESVAR FOR BIC USES DENOMINATOR OF N RATHER THAN
C     (N - P).  SO ADJUST FOR BIC.
C
      RESVAR=RESSD**2
      SSQTMP=REAL(N-NP)*RESVAR
      RESVA2=SSQTMP/REAL(N)
      BIC=REAL(N)*LOG(RESVA2) + REAL(NP)*LOG(REAL(N))
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
        WRITE(ICOUT,3231)RESSD,RESDF,RESMS,RESSS
 3231   FORMAT('RESSD,RESDF,RESMS,RESSS = ',4E15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C               *********************************************
C               **  STEP 42--                              **
C               **  PRINT OUT FIT TABLES                   **
C               *********************************************
C
      IF(IREP.EQ.'YES')THEN
        IFITDF=IRESDF-IREPDF
        FITDF=IFITDF
        FITSS=RESSS-REPSS
        FITMS=100000.0
        IF(FITDF.GT.0.0)FITMS=FITSS/FITDF
        FSTAT=100000.0
        IF(REPMS.GT.0.0)FSTAT=FITMS/REPMS
        CALL FCDF(FSTAT,IFITDF,IREPDF,CDF)
        CDF2=100.0*CDF
        ALFCDF=CDF
      ENDIF
C
      IF(IPRINT.EQ.'ON')THEN
        IF(NUMPAR.GE.1 .AND. ICASFI.NE.'MFIT')THEN
          ITITLE='Least Squares Polynomial Fit'
          NCTITL=28
        ELSEIF(NUMPAR.GE.1 .AND. ICASFI.EQ.'MFIT')THEN
          ITITLE='Least Squares Multilinear Fit'
          NCTITL=29
        ELSEIF(NUMPAR.LE.0)THEN
          ITITLE='Fully-Specified Model'
          NCTITL=21
        ENDIF
        ITITLZ=' '
        NCTITZ=0
C
        DO2301I=1,MAXROW
          ITEXT(I)=' '
          NCTEXT(I)=0
          AVALUE(I)=0.0
          IDIGIT(I)=NUMDIG
 2301   CONTINUE
        ICNT=1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Sample Size:'
        NCTEXT(ICNT)=12
        AVALUE(ICNT)=REAL(N)
        IDIGIT(ICNT)=0
        IDEGRE=NUMPAR-1
        IF(ICASFI.NE.'MFIT')THEN
          IF(IFITAC.EQ.'OFF')IDEGRE=NUMPAR
          ICNT=ICNT+1
          ITEXT(ICNT)='Degree:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=REAL(IDEGRE)
          IDIGIT(ICNT)=0
        ELSE
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Variables:'
          NCTEXT(ICNT)=20
          AVALUE(ICNT)=REAL(IDEGRE)
          IDIGIT(ICNT)=0
        ENDIF
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Residual Standard Deviation:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=RESSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Residual Degrees of Freedom:'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=REAL(IRESDF)
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='BIC:'
        NCTEXT(ICNT)=4
        AVALUE(ICNT)=BIC
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        IF(IREP.EQ.'NO')THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='No Replication Case:'
          NCTEXT(ICNT)=20
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ELSE
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Case:'
          NCTEXT(ICNT)=17
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Standard Deviation:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REPSD
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Degrees of Freedom:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REAL(IREPDF)
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Distinct Subsets:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REAL(NUMSET)
          IDIGIT(ICNT)=0
          IF(IFITDF.LT.1)THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='The Lack of Fit F Test cannot be done'
            NCTEXT(ICNT)=37
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='because the numerator of the F ratio'
            NCTEXT(ICNT)=36
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='has 0 degrees of freedom.  This happens'
            NCTEXT(ICNT)=39
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='when the number of parameters fitted is'
            NCTEXT(ICNT)=39
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='equal to the number of distinct subsets.'
            NCTEXT(ICNT)=40
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ELSE
            ICNT=ICNT+1
            ITEXT(ICNT)='Lack of Fit F Ratio:'
            NCTEXT(ICNT)=20
            AVALUE(ICNT)=FSTAT
            IDIGIT(ICNT)=NUMDIG
            ICNT=ICNT+1
            ITEXT(ICNT)='Lack of Fit F CDF (%):'
            NCTEXT(ICNT)=22
            AVALUE(ICNT)=CDF2
            IDIGIT(ICNT)=NUMDIG
            ICNT=ICNT+1
            ITEXT(ICNT)='Lack of Fit Degrees of Freedom 1:'
            NCTEXT(ICNT)=33
            AVALUE(ICNT)=REAL(IFITDF)
            IDIGIT(ICNT)=0
            ICNT=ICNT+1
            ITEXT(ICNT)='Lack of Fit Degrees of Freedom 2:'
            NCTEXT(ICNT)=33
            AVALUE(ICNT)=REAL(IREPDF)
            IDIGIT(ICNT)=0
          ENDIF
        ENDIF
C
        NUMROW=ICNT
        DO2310I=1,NUMROW
          NTOT(I)=15
 2310   CONTINUE
C
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1              NCTEXT,AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGA3,IERROR)
        ITITLE=' '
        NCTITL=-99
        ITITL9=' '
        NCTIT9=0
C
        NUMCOL=6
        NUMLIN=2
C
        DO4101J=1,NUMCLI
          DO4102I=1,MAXLIN
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
            NCOLSP(I,J)=0
 4102     CONTINUE
          DO4103I=1,MAXROW
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
            ROWSEP(I)=0
 4103     CONTINUE
 4101   CONTINUE
C
        ITITL2(1,1)=' '
        NCTIT2(1,1)=0
        NCOLSP(1,1)=1
        ITITL2(2,1)=' '
        NCTIT2(2,1)=0
        NCOLSP(2,1)=1
C
        ITITL2(1,2)=' '
        NCTIT2(1,2)=0
        NCOLSP(1,2)=3
        ITITL2(2,2)='Parameter Estimates'
        NCTIT2(2,2)=19
        NCOLSP(2,2)=3
C
        ITITL2(1,5)='Approximate'
        NCTIT2(1,5)=11
        NCOLSP(1,5)=1
        ITITL2(2,5)='Standard Deviation'
        NCTIT2(2,5)=18
        NCOLSP(2,5)=1
C
        ITITL2(1,6)=' '
        NCTIT2(1,6)=0
        NCOLSP(1,6)=1
        ITITL2(2,6)='t-Value'
        NCTIT2(2,6)=7
        NCOLSP(2,6)=1
C
        NMAX=0
        DO4110I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.1)NTOT(I)=3
          IF(I.EQ.2)NTOT(I)=10
          IF(I.EQ.3)NTOT(I)=10
          IF(I.EQ.5)NTOT(I)=20
          IF(I.EQ.6)NTOT(I)=10
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IF(I.EQ.2 .OR. I.EQ.3)ITYPCO(I)='ALPH'
          DO4113J=1,MAXROW
            IDIGI2(J,I)=NUMDIG
            IF(I.EQ.1)THEN
              IDIGI2(J,I)=0
            ELSEIF(I.EQ.6)THEN
              IDIGI2(J,I)=4
            ENDIF
 4113     CONTINUE
 4110   CONTINUE
C
        DO4120I=1,NUMPAR
C
          IF(IFITAC.EQ.'OFF')THEN
            IM1=I
            IHOLD3=IVARN3(IM1)
            IHOLD4=IVARN4(IM1)
          ELSE
            IF(I.LE.1)IHOLD3='    '
            IF(I.LE.1)IHOLD4='    '
            IM1=I-1
            IF(I.GE.2)IHOLD3=IVARN3(IM1)
            IF(I.GE.2)IHOLD4=IVARN4(IM1)
          ENDIF
          TVALUE=(-999.9)
          IF(C(I).GT.0.0)TVALUE=PARAM3(I)/C(I)
C
          AMAT(I,1)=REAL(I)
          IVALUE(I,2)(1:4)=IPARN3(I)
          IVALUE(I,2)(5:8)=IPARN4(I)
          NCVALU(I,2)=8
C
          IF(ICASFI.EQ.'MFIT'.AND.C(I).GT.0.0)THEN
            IVALUE(I,3)(1:4)=IHOLD3
            IVALUE(I,3)(5:8)=IHOLD4
            NCVALU(I,3)=8
            AMAT(I,4)=PARAM3(I)
            AMAT(I,5)=C(I)
            AMAT(I,6)=TVALUE
          ELSEIF(ICASFI.EQ.'MFIT'.AND.C(I).EQ.0.0)THEN
            IVALUE(I,3)(1:4)=IHOLD3
            IVALUE(I,3)(5:8)=IHOLD4
            NCVALU(I,3)=8
            AMAT(I,4)=PARAM3(I)
            AMAT(I,5)=C(I)
            AMAT(I,6)=0.0
            IDIGI2(I,6)=-1
          ELSEIF(ICASFI.NE.'MFIT'.AND.C(I).GT.0.0)THEN
            IVALUE(I,3)=' '
            NCVALU(I,3)=0
            AMAT(I,4)=PARAM3(I)
            AMAT(I,5)=C(I)
            AMAT(I,6)=TVALUE
          ELSEIF(ICASFI.NE.'MFIT'.AND.C(I).EQ.0.0)THEN
            IVALUE(I,3)=' '
            NCVALU(I,3)=0
            AMAT(I,4)=PARAM3(I)
            AMAT(I,5)=C(I)
            AMAT(I,6)=0.0
            IDIGI2(I,6)=-1
          ENDIF
 4120   CONTINUE
C
        IWHTML(1)=50
        IWHTML(2)=100
        IWHTML(3)=100
        IWHTML(4)=150
        IWHTML(5)=200
        IWHTML(6)=150
        IINC=1800
        IINC2=200
        IINC3=1200
        IINC4=2500
        IWRTF(1)=IINC2
        IWRTF(2)=IWRTF(1)+IINC3
        IWRTF(3)=IWRTF(2)+IINC3
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC4
        IWRTF(6)=IWRTF(5)+IINC
C
        ICNT=NUMPAR
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
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
CCCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
C               ************************************************
C               **  STEP 81--                                 **
C               **  WRITE INFO OUT TO FILES--                 **
C               **     1) DPST1F.DAT--COEF SDCOEF TCDF        **
C               **        JUNE 2002: ADD JOINT BONFERRNI      **
C               **        CONFIDENCE INTERVAL FOR PARAMETERS  **
C               **     2) DPST2F.DAT--SDPRED, CONFIDENCE      **
C               **        INTERVAL FOR PREDICTED VALUES       **
C               **     3) DPST3F.DAT--REGRESSION DIAGNOSTICS  **
C               **     4) DPST4F.DAT--CORR MATRIX             **
C               **     5) DPST5F.DAT--ADD ANOVA TABLE (AND    **
C               **        R-SQUARE, ADJUSTED R-SQUARE, MALLOWS**
C               **        CP, PRESS P STATISTICS              **
C               **        ADDED JUNE 2002                     **
C               ************************************************
C
 8600 CONTINUE
C
      ISTEPN='86'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFITAU.EQ.'OFF')GOTO8619
C
CCCCC JUNE 2002.  ADD T-VALUE AND JOINT BONFERONI CONFIDENCE
CCCCC LIMITS TO OUTPUT
C
      AJUNK=1.0 - ALPHA
      AJUNK2=1.0 - (AJUNK/(2.0*REAL(NUMPAR)))
      NP=N-NUMPAR
      TBONF=0.0
      IF(NP.GE.1.AND.(AJUNK2.GE.0.0.AND.AJUNK2.LE.1.0))
     1CALL TPPF(AJUNK2,REAL(NP),TBONF)
C
      IFORMT='(5(E15.7,2X),2A4)'
      IF(IAUXDP.NE.7)THEN
        IFORMT=' '
        IF(IAUXDP.LE.9)THEN
          IFORMT='(5(Exx.x,2X),2A4)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(5:6),'(I2)')ITOT
          WRITE(IFORMT(8:8),'(I1)')IAUXDP
        ELSE
          IFORMT='(5(Exx.xx,2X),2A4)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(5:6),'(I2)')ITOT
          WRITE(IFORMT(8:9),'(I2)')IAUXDP
        ENDIF
      ENDIF
C
      IF(IFITAU.EQ.'ON')THEN
        WRITE(IOUNI1,8613)
 8613   FORMAT(1X,
     1         'COEFFICIENT     ',
     2         'COEF SD         ',
     3         'T-VALUE         ',
     4         'BONF LOWER CONF ',
     5         'BONF UPPER CONF ')
        DO8610I=1,NUMPAR
          TVALUE=(-999.9)
          IF(C(I).GT.0.0)TVALUE=PARAM3(I)/C(I)
          TBONL=PARAM3(I) - TBONF*C(I)
          TBONU=PARAM3(I) + TBONF*C(I)
          WRITE(IOUNI1,IFORMT)PARAM3(I),C(I),TVALUE,TBONL,TBONU,
     1                        IPARN3(I),IPARN4(I)
 8610   CONTINUE
C8611   FORMAT(5E15.7,2X,A4,A4)
C
CCCCC   THE FOLLOWING 2 LINES WERE ADDED     SEPTEMBER 1995
CCCCC   APRIL 1996.  SUPPRESS PRINTING IF IPRINT OFF
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8612)
 8612     FORMAT('DPST1F.DAT: COEF, SD(COEF), T-VALUE, LOWER ,',
     1           'BONFERRONI UPPER BONFERRONI')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
 8619 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ACTIVATED     JANUARY 1994
CCCCC JUNE 2002: ADD SUPPORT FOR JOINT BONFERRONI AND JOINT
CCCCC HOTELLING CONFIDENCE INTERVALS.
      T975=0.0
      T995=0.0
      IF(IRESDF.GE.1)CALL TPPF(.975,REAL(IRESDF),T975)
      IF(IRESDF.GE.1)CALL TPPF(.995,REAL(IRESDF),T995)
C
      TBONF=0.0
      THOT=0.0
      IF(AJUNK.LE.0.0 .OR. AJUNK.GE.1.0)AJUNK=0.95
      IF(ALPHA.GE.0.5)THEN
        AJUNK=1.0 - ALPHA
      ELSE
        AJUNK=ALPHA
      ENDIF
      AJUNK2=1.0 - (AJUNK/(2.0*REAL(N)))
      NP=N-NUMPAR
      IF(NP.GE.1.AND.(AJUNK2.GE.0.0.AND.AJUNK2.LE.1.0))
     1CALL TPPF(AJUNK2,REAL(NP),TBONF)
      IF(NP.GE.1.AND.NUMPAR.GE.1.AND.(ALPHA.GE.0.0.AND.ALPHA.LE.1.0))
     1CALL FPPF(ALPHA,NUMPAR,NP,THOT)
      THOT=REAL(NUMPAR)*THOT
      IF(THOT.GT.0.0)THOT=SQRT(THOT)
C
      IF(IFITAU.EQ.'OFF')GOTO8629
C
      WRITE(IOUNI2,8623)
 8623 FORMAT(1X,
     1       'SD PRED VALUES  ',
     2       '95% LOW PRED CL ',
     3       '95% UPP PRED CL ',
     4       '99% LOW PRED CL ',
     5       '99% UPP PRED CL ',
     6       'BONF LOW PRED CL',
     7       'BONF UPP PRED CL',
     8       'HOTE LOW PRED CL',
     9       'HOTE UPP PRED CL')
      DO8620I=1,N
        PR=PRED2(I)
        SDPR=VSDPRE(I)
        ALOW2=PR-T975*SDPR
        AUPP2=PR+T975*SDPR
        ALOW3=PR-T995*SDPR
        AUPP3=PR+T995*SDPR
        ALOW4=PR-TBONF*SDPR
        AUPP4=PR+TBONF*SDPR
        ALOW5=PR-THOT*SDPR
        AUPP5=PR+THOT*SDPR
C
        IFORMT='(9(E15.7))'
        IF(IAUXDP.NE.7)THEN
          IFORMT=' '
          IF(IAUXDP.LE.9)THEN
            IFORMT='(9(Exx.x))'
            ITOT=IAUXDP+8
            WRITE(IFORMT(5:6),'(I2)')ITOT
            WRITE(IFORMT(8:8),'(I1)')IAUXDP
          ELSE
            IFORMT='(9(Exx.xx))'
            ITOT=IAUXDP+8
            WRITE(IFORMT(5:6),'(I2)')ITOT
            WRITE(IFORMT(8:9),'(I2)')IAUXDP
          ENDIF
        ENDIF
C
        WRITE(IOUNI2,IFORMT)SDPR,ALOW2,AUPP2,ALOW3,AUPP3,ALOW4,AUPP4,
     1                      ALOW5,AUPP5
C8621   FORMAT(9E15.7)
 8620 CONTINUE
CCCCC APRIL 1996.  SUPPRESS PRINTING IF IPRINT OFF
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8622)
 8622   FORMAT('DPST2F.DAT: SD(PRED),95LOWER,95UPPER,99LOWER,99UPPER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8624)
 8624   FORMAT('            LOWER BONFERRONI,UPPER BONFERRONI,',
     1         'LOWER HOTELLING,UPPER HOTELLING')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 8629 CONTINUE
C
CCCC  JULY 1993.  UNCOMMENT FOLLOWING BLOCK. COPUTE AND PRINT:
CCCCC 1) DIAGONALS OF HAT MATRIX (HII = VAR(PRED VALUE)/RESIDUAL VAR)
CCCCC 2) VARIANCE OF RESIDUALS   (VAR(RES) = MSE*(1-HII))
CCCCC 3) STANDARDIZED RESIDUALS  (STRES = RES/SQRT(MSE))
CCCCC 4) INTERNALLY STUDENTIZED RESIDUALS  ( = RES/SD(RES))
CCCCC 5) DELETED RESIDUALS       ( = RES/(1-HII))
CCCCC 6) EXTERNALLY STUDENTIZED RESIDUALS (=RES*SQRT((N-P-1)/(SSE*
CCCCC                                       (1-HII)-RES**2))
CCCCC 7) COOK'S DISTANCE         (COOK=(RES**2/(P*MSE))*HII/(1-HII)**2
CCCCC 8) DFFITS                  (DFFITS=EXTSRES*SQRT(HII(1-HII))
CCCCC                              WHERE EXTSRES=EXTERNAL STUDENT RES
CCCCC IF HAVE PERFECT FIT, RESSD IS ZERO.  DON'T PRINT DIAGNOSTIC
CCCCC STATISTICS IN THIS CASE.
C
      IF(IFITAU.EQ.'OFF')GOTO8649
C
      IF(RESSD.EQ.0.0)THEN
        WRITE(IOUNI3,8631)
 8631   FORMAT(1X,'PERFECT FIT, NO DIAGNOSTICS GENERATED.')
        GOTO8659
      ENDIF
C
      AJUNK=RESSD**2
      DSUM1=0.0D0
      DO8635I=1,N
        AJUNK2=VSDPRE(I)**2
        CALL SPDIV(AJUNK2,AJUNK,IND,Z(I))
        IF(W(I).EQ.0.0)Z(I)=0.0
 8635 CONTINUE
      WRITE(IOUNI3,8639)
 8639 FORMAT(1X,
     1'DIAGONAL OF HAT ',
     2'RESIDUAL VAR    ',
     3'STANDARD RES    ',
     4'INT. STUD. RES  ',
     5'DELETED RES     ',
     6'EXT. STUD. RES  ',
     7'COOKS DISTANCE  ',
     8'DFFITS          ')
      DO8640I=1,N
      AJUNK3=RESMS*(1.0-Z(I))
      IF(AJUNK3.LE.0.0)AJUNK3=0.0
      IF(SQRT(RESMS).GT.0.0)THEN
        AJUNK4=RES2(I)/SQRT(RESMS)
      ELSE
        AJUNK4=0.0
      ENDIF
      IF(AJUNK3.GT.0.0)THEN
        AJUNK5=RES2(I)/SQRT(AJUNK3)
      ELSE
        AJUNK5=0.0
      ENDIF
      IF(Z(I).NE.1.0)THEN
        AJUNK6=RES2(I)/(1.0-Z(I))
        DSUM1=DSUM1 + DBLE(AJUNK6)**2
      ELSE
        AJUNK6=CPUMAX
      ENDIF
      ACONST=(RESDF-1.0)
CCCCC SEPTEMBER 1993.  FIX TYPO IN FOLLOWING LINE
CCCCC IF(RESS*(1.0-Z(I))-RES2(I)**2.NE.0.0)THEN
      IF(RESSS*(1.0-Z(I))-RES2(I)**2.NE.0.0)THEN
        AJUNK2=ACONST/(RESSS*(1.0-Z(I))-RES2(I)**2)
      ELSE
        AJUNK2=0.0
      ENDIF
      AJUNK7=0.0
      IF(AJUNK2.GE.0.0)AJUNK7=RES2(I)*SQRT(AJUNK2)
CCCCC THE FOLLOWING LINE WAS FIXED        JANUARY 1996
CCCCC TO FIX BOMB WITH   CONSTANT FIT     JANUARY 1996
CCCCC AJUNK=RES2(I)**2/(REAL(M)*RESMS)
CCCCC USE NUMPAR INSTEAD OF M.
      AJUNK=0.0
CCCCC IF(M.GT.0)AJUNK=RES2(I)**2/(REAL(M)*RESMS)
      IF(NUMPAR.GT.0)AJUNK=RES2(I)**2/(REAL(NUMPAR)*RESMS)
      AJUNK2=0.0
      IF(Z(I)-1.0.NE.0.0)AJUNK2=Z(I)/((1.0-Z(I))**2)
      AJUNK8=AJUNK*AJUNK2
      AJUNK2=0.0
      IF(Z(I)-1.0.NE.0.0)AJUNK2=SQRT(Z(I)/(1.0-Z(I)))
      AJUNK9=AJUNK7*AJUNK2
C
      IFORMT='(8(E15.7,1X))'
      IF(IAUXDP.NE.7)THEN
        IFORMT=' '
        IF(IAUXDP.LE.9)THEN
          IFORMT='(8(Exx.x,1X))'
          ITOT=IAUXDP+8
          WRITE(IFORMT(5:6),'(I2)')ITOT
          WRITE(IFORMT(8:8),'(I1)')IAUXDP
        ELSE
          IFORMT='(8(Exx.xx,1X))'
          ITOT=IAUXDP+8
          WRITE(IFORMT(5:6),'(I2)')ITOT
          WRITE(IFORMT(8:9),'(I2)')IAUXDP
        ENDIF
      ENDIF
C
      WRITE(IOUNI3,IFORMT)Z(I),AJUNK3,AJUNK4,AJUNK5,AJUNK6,
     1AJUNK7,AJUNK8,AJUNK9
C8641 FORMAT(8(E15.7,1X))
 8640 CONTINUE
C
      APRESS=REAL(DSUM1)
C
CCCCC APRIL 1996.  SUPPRESS PRINTING IF IPRINT OFF
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8652)
 8652   FORMAT('DPST3F.DAT: REGRESSION DIAGNOSTICS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 8649 CONTINUE
C
CCCCC JULY 1993.  WRITE OUT VARIANCE-COVARIANCE PARAMETER OF
CCCCC PARAMETERS.  NOTE THAT IT IS STORED IN SCRATCH SCR, STARTING
CCCCC AT ELEMENT 1 AND (M+1)*(M+2)/2 ELEMENTS LONG
CCCCC ACTUALLY, THIS IS THE (X-TRANSPOSE X) INVERSE MATRIX, MULTIPLY
CCCCC BY MSE TO GET VARIANCE-COVARIANCE MATRIX.
CCCCC JUNE 1994.  BUG: FOR POLYNOMIAL, M=NUMPAR-1, SO ADD 1 BACK IN
C
      IF(IFITAU.EQ.'OFF')GOTO8689
C
 8659 CONTINUE
      NTEMP=M
      IF(ICASFI.NE.'MFIT')NTEMP=M+1
      ICOUNT=0
      DO8660I=1,NTEMP
        DO8662J=I,NTEMP
          ICOUNT=ICOUNT+1
          PARCOV(I,J)=SCR(ICOUNT)
          PARCOV(J,I)=PARCOV(I,J)
 8662   CONTINUE
 8660 CONTINUE
C
      IFORMT='(8(E15.7,1X))'
      IF(IAUXDP.NE.7)THEN
        IFORMT=' '
        IF(IAUXDP.LE.9)THEN
          IFORMT='(2(Exx.x,1X))'
          ITOT=IAUXDP+8
          WRITE(IFORMT(5:6),'(I2)')ITOT
          WRITE(IFORMT(8:8),'(I1)')IAUXDP
        ELSE
          IFORMT='(2(Exx.xx,1X))'
          ITOT=IAUXDP+8
          WRITE(IFORMT(5:6),'(I2)')ITOT
          WRITE(IFORMT(8:9),'(I2)')IAUXDP
        ENDIF
      ENDIF
C
      WRITE(IOUNI4,8673)
 8673 FORMAT(1X,
     1       'PARAMETER COV   ',
     2       'INVERSE X-TRANSPOSE*X')
      DO8670J=1,NTEMP
        DO8672I=1,NTEMP
          AJUNK=RESMS*PARCOV(I,J)
          WRITE(IOUNI4,IFORMT)AJUNK,PARCOV(I,J)
C8679     FORMAT(E15.7,1X,E15.7)
 8672   CONTINUE
        WRITE(IOUNI4,8678)
 8678   FORMAT(1X)
 8670 CONTINUE
C
CCCCC APRIL 1996.  SUPPRESS PRINTING IF IPRINT OFF
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8682)
 8682   FORMAT('DPST4F.DAT: PARAMETER VARIANCE-COVARIANCE MATRIX AND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8683)
 8683   FORMAT('            INVERSE OF X-TRANSPOSE X MATRIX')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 8689 CONTINUE
C
CCCCC WRITE REGRESSION ANOVA TABLE TO DPST5F.DAT
C
      RESSD=SD
      RESDF=NDF
      RESMS=RESSD*RESSD
      RESSS=RESMS*RESDF
C
      IREGDF=NUMPAR-1
      AMSR=SSR/REAL(IREGDF)
C
      ITOTDF=INT(RESDF) + IREGDF
      SSTO=SSR + RESSS
C
      RSQUAR=1.0 - RESSS/SSTO
      ADJRSQ=1.0 - (REAL(N-1)/REAL(N-NUMPAR))*RESSS/SSTO
C
      FSTAT=100000.0
      IF(RESMS.GT.0.0)FSTAT=AMSR/RESMS
      NP=N-NUMPAR
      CALL FCDF(FSTAT,IREGDF,NP,CDF)
C
      IF(IFITAU.EQ.'OFF')GOTO8729
C
      WRITE(IOUNI5,8710)
 8710 FORMAT('------------------------------------------------------',
     1       '-----------------------')
      WRITE(IOUNI5,8712)
 8712 FORMAT('SOURCE               DF    SUM OF SQUARES    ',
     1       ' MEAN SQUARE              F')
      WRITE(IOUNI5,8710)
C
      WRITE(IOUNI5,8714)IREGDF,SSR,AMSR,FSTAT
 8714 FORMAT('REGRESSION     ',I8,3X,E15.7,3X,E15.7,3X,E15.7)
      WRITE(IOUNI5,8716)INT(RESDF),RESSS,RESMS
 8716 FORMAT('RESIDUAL       ',I8,3X,E15.7,3X,E15.7)
      WRITE(IOUNI5,8718)ITOTDF,SSTO
 8718 FORMAT('TOTAL          ',I8,3X,E15.7)
C
      WRITE(IOUNI5,8710)
      WRITE(IOUNI5,999)
      WRITE(IOUNI5,999)
      WRITE(IOUNI5,8722)RSQUAR
 8722 FORMAT('R-SQUARE           = ',F10.7)
      WRITE(IOUNI5,8724)ADJRSQ
 8724 FORMAT('ADJUSTED R-SQUARE  = ',F10.7)
      WRITE(IOUNI5,8726)APRESS
 8726 FORMAT('PRESS-P STATISTIC  = ',G15.7)
      WRITE(IOUNI5,8727)BIC
 8727 FORMAT('BIC                = ',G15.7)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8782)
 8782   FORMAT('DPST5F.DAT: REGRESSION ANOVA TABLE')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 8729 CONTINUE
C
CCCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
C               **************************************
C               **  STEP 88--                       **
C               **  CLOSE       THE STORAGE FILES.  **
C               **************************************
C
 8800 CONTINUE
C
      ISTEPN='87'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFITAU.EQ.'ON')THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
CCCCC IF(IBUGA3.EQ.'OFF')GOTO9090   MAY 1989
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIT3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR,ICASFI,IT
 9012   FORMAT('IERROR,ICASFI,IT = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)N,NUMVAR,NUMPAR,NUMCHA
 9013   FORMAT('N,NUMVAR,NUMPAR,NUMCHA = ',4I8)
        CALL DPWRST('XXX','WRIT')
        DO9015I=1,NUMPAR
          WRITE(ICOUT,9016)I,IPARN3(I),IPARN4(I),PARAM3(I)
 9016     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I) = ',I8,2X,A4,A4,G15.7)
          CALL DPWRST('XXX','WRIT')
 9015   CONTINUE
        DO9020I=1,N
          WRITE(ICOUT,9021)I,Y(I),X1(I),X2(I),W(I),PRED2(I),RES2(I)
 9021     FORMAT('I,Y(I),X1(I),X2(I),W(I),PRED2(I),RES2(I) = ',
     1           I8,6E13.6)
          CALL DPWRST('XXX','WRIT')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
CCCCC-----LSQRT--------------------------------------
      SUBROUTINE LSQRTX (Y,W,N,X,NR,M,IT,
     1                   B,Z,T,V,S,E,D,SD,NDF,SCR,ID,IFITAC,
     1                  IBUGA3,ISUBRO,IERROR)
CCCCC THE ABOVE LINE WAS AUGMENTED     SEPTEMBER 1993
C
C     PURPOSE--PERFORM LEAST SQUARES FIT
C              OF MULTILINEAR MODEL OR POLYNOMIAL MODEL
C              USING A MODIFIED GRAM-SCHMIDT ALGORITHM
C              WITH ITERATIVE REFINEMENT OF THE SOLUTION.
C
C     INPUT ARGUMENTS--
C           Y     VECTOR OF OBSERVATIONS (N BY 1).
C           W     VECTOR OF WEIGHTS (N BY 1).
C           N     NUMBER OF OBSERVATIONS.
C           X     MATRIX OF INDEPENDENT VARIABLES WHICH ARE TO BE FITTED.
C           NR    MAXIMUM NUMBER OF ROWS IN X.
C           M     NUMBER OF UNKNOWN COEFFICIENTS OR DEGREE OF POLYNOMIAL
C                    (M LESS THAN OR EQUAL TO N).
C           IT    PARAMETER WHICH SPECIFIES WHETHER OR NOT A POLYNOMIAL TYPE
C                    FIT IS TO BE PERFORMED.
C                      IT = 1 INDICATES POLYNOMIAL FIT.
C                      IT = 2 INDICATES MULTILINEAR FIT.
C
C
C                 IF IT = 1, THE FUNCTION TO BE FITTED IS A POLYNOMIAL
C                    HAVING THE FORM
C
C                    Y(I) = B(1) + B(2)*Z(I) + B(3)*Z(I)**2 + ...
C                                + B(M)*Z(I)**(M-1) + ERROR, I=1,2,...,N.
C
C                 IF IT = 2, THE FUNCTION TO BE FITTED HAS THE FORM
C
C                    Y(I) = B(1)*X1(I) + B(2)*X2(I) + ... + B(M)*XM(I) +
C                                                     ERROR, I=1,2,...,N.
C     OUTPUT ARGUMENTS--
C           B     VECTOR OF COEFFICIENTS (M+1 BY 1).
C           Z     VECTOR OF RESIDUALS (N BY 1).
C           T     VECTOR OF STANDARD DEVIATIONS OF COEFFICIENTS (M+1 BY 1).
C           V     VECTOR OF STANDARD DEVIATIONS OF PREDICTED VALUES
C                    (N BY 1).
C           S     VECTOR OF SQUARED FOURIER COEFFICIENTS (M+3 BY 1).  THE
C                    FIRST M ELEMENTS OF THIS ARRAY ARE SUMS OF SQUARES
C                    WHICH CAN BE USED IN AN ANALYSIS OF VARIANCE.  THE
C                    LAST TWO ELEMENTS OF S ARE NOT COMPUTED IN THIS SUB-
C                    ROUTINE BUT ARE RESERVED FOR QUANTITIES TO BE COMPUTED
C                    IN THE CALLING PROGRAM.
C           E     RESIDUAL SUM OF SQUARES.
C           D     AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN INITIAL
C                    SOLUTION AND THE FIRST ITERATION (IN SUBROUTINE SLVE).
C           SD    RESIDUAL STANDARD DEVIATION.
C           NDF   NO. OF DEGREES OF FREEDOM.
C           SCR   A SCRATCH VECTOR USED FOR INTERNAL CALCULATIONS
C           ID    ID = 0  EVERYTHING IS OK.
C                 ID = 1  AUGMENTED MATRIX IS SINGULAR.
C                 ID = 2  ITERATION PROCEDURE FAILED TO CONVERGE.
C
C     NOTE--THE INPUT ARRAYS X, Y AND W ARE LEFT UNCHANGED
C           BY THIS SUBROUTINE.
C     NOTE--THE SCR VECTOR MUST HAVE SIZE EQUAL TO OR GREATER THAN
C           ((M + 1) (M + 2) / 2) + N*M + 2*N + 2*M +1
C     PRIMARY CALLING SEQUENCE--
C           LSQRT
C                 LSQ
C                       SCALE
C                       PDECOM
C                       SLVE
C                       DSUMAL
C                       SDPRED
C                       PINVRT
C     ADDITIONAL SUBROUTINES THAT HAVE BEEN CONVERTED FROM FUNCTIONS--
C           DPDIV
C           SPDIV
C           DPCON
C           DPSQRT
C           SPSQRT
C           SPLO10
C           IDIV
C
C     SUBROUTINE LSQ COMPUTES SOLUTIONS TO LINEAR LEAST SQUARES
C        PROBLEMS USING A MODIFIED GRAM-SCHMIDT ALGORITHM WITH
C        ITERATIVE REFINEMENT OF THE SOLUTION.
C
C     SUBROUTINES PDECOM, SLVE AND PINVRT ARE BASED ON ...
C        (1) ITERATIVE REFINEMENT OF LINEAR LEAST SQUARES SOLUTIONS II,
C            BY AKE BJORCK, BIT, VOL. 8 (1968), PP. 8-30.
C        (2) SOLUTIONS TO WEIGHTED LEAST SQUARES PROBLEMS BY MODIFIED
C            GRAM-SCHMIDT WITH ITERATIVE REFINEMENT, BY ROY H. WAMPLER,
C            ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE, VOL. 5 (1979),
C            TO APPEAR.
C
C     PRECISION--
C        SINGLE PRECISION ARITHMETIC IS USED FOR ALL CALCULATIONS EXCEPT
C        THE DOUBLE PRECISION ACCUMULATION OF INNER PRODUCTS.  (THE
C        VARIABLE SUM (OR DSUM) IS DECLARED TO BE DOUBLE PRECISION IN
C        SUBROUTINE LSQ, SCALE, PDECOM, SLVE, SDPRED AND PINVRT.)  IT
C        IS ESSENTIAL FOR THE SUCCESS OF THE ITERATIVE REFINEMENT
C        PROCEDURE IN SUBROUTINE SLVE THAT INNER PRODUCTS BE ACCUMULATED
C        IN DOUBLE PRECISION.
C
C *   CONVERSION OF THE PROGRAM TO STRICTLY DOUBLE PRECISION, AND      *
C *   CONVERSION OF THE PROGRAM TO STRICTLY SINGLE PRECISION.          *
C *      ON COMPUTERS HAVING SHORT WORD LENGTH (AS THE IBM 360/370)    *
C *      IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN DOUBLE     *
C *      PRECISION.  ON COMPUTERS HAVING LONG WORD LENGTH (AS THE CDC  *
C *      6600) IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN      *
C *      SINGLE PRECISION.  IN SUCH CASES, THE ITERATIVE REFINEMENT    *
C *      PRESENTLY INCLUDED IN SUBROUTINE SLVE SHOULD BE OMITTED.      *
C *      ADDITIONAL REMARKS ON HOW TO OMIT THE ITERATIVE REFINEMENT    *
C *      ARE GIVEN IN SUBROUTINE SLVE.                                 *
C *      IF ALL COMPUTING IS DONE IN DOUBLE PRECISION, THE VALUE OF    *
C *      ETA, A MACHINE DEPENDENT PARAMETER, SHOULD BE CHANGED SO THAT *
C *      ETA IS THE SMALLEST DOUBLE PRECISION NUMBER SUCH THAT         *
C *      1.0 + ETA IS GREATER THAN 1.0 IN DOUBLE PRECISION ARITHMETIC. *
C
C     TEST PROBLEM--
C           SAMPLE INPUT FOR A MULTILINEAR FIT
C           (4 INDEPENDENT VARIABLES EQUIVALENT TO A CUBIC FIT
C           AND UNIT WEIGHTING)--
C           FIRST LINE GIVES SAMPLE SIZE, DEGREE, POLYNOMIAL TYPE
C
C            7 4 2
C            10. 1. 3.4 11.56 39.304 1.
C            20. 1. 11.7 136.89 1601.613 1.
C            30. 1. 37.2 1383.84 51478.848 1.
C            40. 1. 80.1 6416.01 513922.401 1.
C            50. 1. 151.4 22921.96 3470384.744 1.
C            60. 1. 253.2 64110.24 16232712.768 1.
C            70. 1. 392.6 154134.76 60513306.776 1.
C
C           SAMPLE INPUT FOR A CUBIC POLYNOMIAL FIT
C           (SAME EXAMPLE AS ABOVE)--
C           FIRST LINE GIVES SAMPLE SIZE, NUMBER OF VAR., MULTILINEAR TYPE
C
C            7 3 1
C            10.   3.4 1.
C            20.  11.7 1.
C            30.  37.2 1.
C            40.  80.1 1.
C            50. 151.4 1.
C            60. 253.2 1.
C            70. 392.6 1.
C
C     OUTPUT (FROM EITHER OF THE ABOVE 2 TEST PROBLEMS)--
C
C       COEFFICIENTS
C          .12212494E+02    .46908681E+00   -.16867931E-02    .22115341E-05
C       RESIDUALS
C         -.37879763E+01    .25265538E+01    .25578816E+01   -.10042261E+00
C         -.22425069E+01    .12562386E+01   -.20976813E+00
C       S D OF COEFFICIENTS
C          .26445864E+01    .86317750E-01    .57921800E-03    .98128429E-06
C       S D OF PREDICATED VALUES
C          .24379267E+01    .20369802E+01    .17428904E+01    .23363574E+01
C          .23017371E+01    .31747709E+01    .33588546E+01
C       SQUARED FOURIER COEFFICIENTS
C          .11200000E+05    .24784422E+04    .23016542E+03    .57456310E+02
C       RESIDUAL SUM OF SQUARES =    .33936057E+02
C       AVERAGE NO. DIGITS IN AGREEMENT =    .78267799E+01
C       RESIDUAL STANDARD DEVIATION =    .33633345E+01
C       DEGREES OF FREEDOM =   3
C
C     NOTE--IN THE ABOVE TEST PROBLEMS, N = 7 AND M = 4
C           AND THUS THE DIMENSION OF SCR MUST BE AT LEAST
C           ((M + 1) (M + 2) / 2) + N*M + 2*N + 2*M +1 =
C           ((4 + 1) (4 + 2) / 2) + 7*4 + 2*7 + 2*4 +1 = 66
C
C     NOTE--MAXOBV = MAXIMUM NUMBER OF OBSERVATIONS PER VARIABLE
C                    (= 2048 (JULY 1987))
C           MAXCMF = MAXIMUM NUMBER OF COEFFICIENTS THAT MAY
C                    BE ESTIMATED IN A MULTILINEAR FIT
C                    (= 30 (JULY 1987))
C     WRITTEN BY--ROY H. WAMPLER
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 A337 ADMINISTRATION BUILDING
C                 NATIONAL BUREAU OF STANDARDS
C                 GAITHERSBURG, MD. 20899
C                 301-975-2844
C
C     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C     UPDATED         --MARCH     1988.  CHECK THAT SCRATCH AREA NOT EXCEEDED
C     UPDATED         --NOVEMBER  1989.  DIMENSION SCR(1) TO SCR(*)
C     UPDATED         --SEPTEMBER 1993.  ADD ISUBRO TO INPUT ARGS
C     UPDATED         --JULY      1995.  ADJUST DEBUG FORMATS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFITAC
      CHARACTER*4 IBUGA3
CCCCC THE FOLLOWING LINE WAS ADDED    SEPTEMBER 1993
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C-----DIMENSION-------------------------------------------------------
 
      INCLUDE 'DPCOPA.INC'
C
CCCCC DIMENSION X(NR,M),Y(N),W(N),B(M),Z(N),T(M+1),V(N),S(M+2),SCR(1)
CCCCC DIMENSION X(NR,M)
CCCCC DIMENSION X(MAXOBV,MAXCMF)
      DIMENSION X(NR,*)
      DIMENSION Y(N)
      DIMENSION W(N)
      DIMENSION B(M)
      DIMENSION Z(N)
      DIMENSION T(M+1)
      DIMENSION V(N)
      DIMENSION S(M+2)
CCCCC THE FOLLOWING LINE WAS CORRECTED NOVEMBER 1989
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC DIMENSION SCR(1)
      DIMENSION SCR(*)
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
      IERROR='NO'
C
CCCCC THE FOLLOWING LINE WAS CHANGED      SEPTEBMER 1993
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SQRT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF LSQRT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)N,M,IT,IBUGA3
   55   FORMAT('N,M,IT,IBUGA3 = ',3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO56J=1,M
          DO57I=1,N
            WRITE(ICOUT,58)I,J,Y(I),X(I,J),W(I)
   58       FORMAT('I,J,Y(I),X(I,J),W(I) = ',2I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
   57     CONTINUE
   56   CONTINUE
      ENDIF
C
CCCCC THE FOLLOWING SECTION OF CODE WAS INSERTED MARCH 1988.
C     CHECK THAT THE SCRATCH AREA WILL NOT OVERFLOW
C
      INEED=(((M+1)*(M+2))/2)+2*M+1+N*(M+2)+2
      IAVAIL=MAXOBW
      IF(INEED.GT.IAVAIL)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN LSQRT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      INTERNAL REGRESSION SCRATCH AREA EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)INEED
  113   FORMAT('      NEEDED    SCRATCH AREA SIZE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,114)IAVAIL
  114   FORMAT('      AVAILABLE SCRATCH AREA SIZE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)
  115   FORMAT('      RECOMMENDATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('         1. FIT TO A SUBSET; OR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)
  117   FORMAT('         2. SIMPLIFY THE MODEL.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C     DEFINE STARTING POINT FOR THE R MATRIX
C
      ISUBR = 1
      MZ = M
      IF (IT.EQ.1 .AND. IFITAC.EQ.'ON') MZ = MZ+1
      MIN2 = (MZ+1) * (MZ+2) / 2
C
C     DEFINE STARTING POINT FOR THE Q VECTOR
C
      ISUBQ = ISUBR + MIN2
      MM1 = N * (MZ+1)
C
C     DEFINE STARTING POINT FOR THE F VECTOR
C
      ISUBF = ISUBQ + MM1
C
C     DEFINE STARTING POINT FOR THE P VECTOR
C
      ISUBP = ISUBF + MZ + 1
C
C     DEFINE STARTING POINT FOR THE A VECTOR
C
      ISUBA = ISUBP + N
      C = 0.0
      H = 0.0
C
CCCCC THE FOLLOWING ARGUMENT LIST WAS AUGMENTED     SEPTEMBER 1995
      CALL LSQ (N,MZ,NR,X,Y,W,H,C,IT,B,Z,SCR(ISUBR),T,V,S,E,SCR(ISUBQ),
     1          SCR(ISUBF),SCR(ISUBP),SCR(ISUBA),ID,D,IFITAC,
     1          IBUGA3,ISUBRO,IERROR)
C
      NDF = 0
      DO 1100 I = 1,N
         IF (W(I) .GT. 0.0) NDF = NDF + 1
 1100 CONTINUE
      NDF = NDF-MZ
CCCCC SD = SPDIV(E,FLOAT(NDF),IND)
      CALL SPDIV(E,FLOAT(NDF),IND,RESULT)
      SD = RESULT
CCCCC SD = SPSQRT(SD)
      CALL SPSQRT(SD,RESULT)
      SD=RESULT
C
 9000 CONTINUE
CCCCC THE FOLLOWING SECTION WAS ADDED      SEPTEBMER 1993
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SQRT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF LSQRT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)SD,RESULT,M,NDF
 9015   FORMAT('SD,RESULT,M,NDF = ',2G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
        DO9016I=1,M
          WRITE(ICOUT,9017)I,B(I),T(I)
 9017     FORMAT('I,B(I),T(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
      RETURN
      END
CCCCC-----LSQ--------------------------------------
      SUBROUTINE LSQ (N,M,NR,X,Y,W,H,C,IT,B,Z,R,T,V,S,E,Q,F,P,A,ID,D,
     1                IFITAC,IBUGA3,ISUBRO,IERROR)
CCCCC SUBROUTINE LSQ (N,M,NR,X,Y,W,H,C,IT,B,Z,R,T,V,S,E,Q,F,P,A,ID,D)
CCCCC THE ABOVE ARGUMENT LIST WAS AUGMENTED    SEPTEMBER 1995
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C               WRITTEN BY -
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL BUREAU OF STANDARDS,
C                      GAITHERSBURG,MD. 20899
C                          TELEPHONE 301-975-2844
C
C     UPDATED--NOVEMBER  1989--DIMENSION (1) TO (*) (AND MOVED)
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --SEPTEMBER 1995. ADD BUGS TO ARGUMENT LIST
C
C     ==================================================================
C
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
CCCCC THE FOLLOWING 3 LINES WERE ADDED    SEPTEMBER 1995
      CHARACTER*4 IFITAC
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
CCCCC THE FOLLOWING 6 LINES WERE MOVED        NOVEMBER 1989
CCCCC AND CHANGED DIMENSION (1) TO (*)
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC REAL             A(1), B(1), F(1), P(1), Q(1), R(1), S(1)
CCCCC REALCCCCC        T(1), V(1), W(1), X(NR,M), Y(1), Z(1)
CCCCC REAL             T(1), V(1), W(1), X, Y(1), Z(1)
CCCCC REAL             C, D, E, H
CCCCC REAL             ETA, RESDF, RMS, RSS, SD, TOL, U, WC, WW, YINC
CCCCC REALCCCCC        SPDIV, DPCON, SPSQRT
C
CCCCC THE FOLLOWING LINE WAS CORRECTED      NOVEMBER 1989
CCCCC SPLIT INTO 2 LINES
CCCCC AND CHANGED DIMENSION (1) TO (MAXOBV) (SEE BELOW)
CCCCC (BUG UNCOVERED BY NELSON HSU)
      DOUBLE PRECISION DX(1)
C
      DOUBLE PRECISION SUM
CCCCC THE FOLLOWING 2 LINES WERE ADDED    SEPTEMBER 1995
      DOUBLE PRECISION SNEG
      DOUBLE PRECISION SPOS
C
      REAL             A(*), B(*), F(*), P(*), Q(*), R(*), S(*)
CCCCC REAL             T(*), V(*), W(*), X(NR,M), Y(*), Z(*)
      REAL             T(*), V(*), W(*), X, Y(*), Z(*)
      REAL             C, D, E, H
      REAL             ETA, RESDF, RMS, RSS, SD, TOL, U, WC, WW, YINC
CCCCC REAL             SPDIV, DPCON, SPSQRT
C
      INCLUDE 'DPCOPA.INC'
CCCCC DIMENSION X(MAXOBV,MAXCMF)
      DIMENSION X(NR,*)
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
C-----DATA STATEMENTS-----------------------------------------------------
C
      DATA RMXINT / 134217727. /
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,2001)
 2001   FORMAT('AT START OF LSQ ROUTINE')
        CALL DPWRST('XXX','BUG ')
        DO2000I=1,N
          WRITE(ICOUT,2011)I,J,(X(I,J),J=1,MAX(M,5))
 2011     FORMAT('I,J,(X(I,J),J=1,MAX(M,5)) = ',2I5,5G15.7)
          CALL DPWRST('XXX','BUG ')
 2000   CONTINUE
      ENDIF
      ID = 0
      NN  = N
      MM  = M
      WC = H
      U   = 0.0
C
C     SET VALUE OF ETA, A MACHINE-DEPENDENT PARAMETER.
C        ETA IS THE SMALLEST POSITIVE REAL NUMBER FOR WHICH 1.0 + ETA IS
C        GREATER THAN 1.0 IN FLOATING-POINT ARITHMETIC.
C        THE VALUE ETA = 2.**(-26) IS APPROPRIATE FOR THE UNIVAC 1108.
C
CCCCC ETA = SPDIV (RMXINT,2.0,IRR) + 1.0
      CALL  SPDIV (RMXINT,2.0,IRR,RESULT)
      ETA = RESULT + 1.0
CCCCC ETA = SPDIV (1.0,ETA,IND)
      CALL  SPDIV (1.0,ETA,IND,ETA)
C
C     SET VALUE OF TOL, A TOLERANCE USED IN DETERMINING THE RANK OF THE
C        SYSTEM OF EQUATIONS.
C
C     EMPIRICAL EVIDENCE SUGGESTS THAT TOL SHOULD BE CHOSEN NO SMALLER
C        THAN N*ETA.
C
      TOL = FLOAT (NN) * ETA
C
C     SET SCALE PARAMETER, ISCALE, EQUAL TO ZERO.
C        ISCALE = 0 INDICATES THAT A SOLUTION IS SOUGHT WITHOUT SCALING
C        THE INPUT DATA.
C
C     IN THE EVENT THAT THE ALGORITHM FAILS TO OBTAIN A SOLUTION WITH
C        UNSCALED DATA, ISCALE IS THEN SET EQUAL TO 1 AND ANOTHER
C        ATTEMPT IS C        ATTEMPT IS MADE TO OBTAIN A SOLUTION WITH THE DATA
C
      ISCALE = 0
      MP1 = MM + 1
C
C     SET UP MATRIX Q, INPUT FOR SUBROUTINES SCALE AND PDECOM.
C
  10  IF (IT.EQ.2) GO TO 50
C
C     CALL SUBROUTINE SCALE TO COMPUTE MEAN OF X-VECTOR (DENOTED BY U)
C        FOR POLYNOMIAL TYPE PROBLEMS, IF DATA ARE TO BE SCALED.
C
      IF (ISCALE.EQ.1) THEN
        CALL SCALDP (ISCALE,2,NN,MM,IT,NR,W,WC,X,U,Q,S,B,A,Z,R,F,IFAULT)
        IF (IFAULT.EQ.1) ID = 1
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2101)
 2101     FORMAT('AFTER FIRST CALL TO SCALE')
          CALL DPWRST('XXX','BUG ')
          DO2100I=1,N
            WRITE(ICOUT,2111)(X(I,J),J=1,MAX(M,5))
 2111       FORMAT('I,J,(X(I,J),J=1,MAX(M,5)) = ',2I5,5G15.7)
            CALL DPWRST('XXX','BUG ')
 2100     CONTINUE
        ENDIF
C
      ENDIF
C
      MM1 = MM - 1
      DO 40 I=1,NN
        K = MM * NN + I
        Q(K) = Y(I)
        Q(I) = 1.0
        IF (MM.EQ.1) GO TO 40
        DO 30 J=1,MM1
          K = (J) * NN + I
          Q(K) = (X(I,1) - U) ** (J)
  30    CONTINUE
  40  CONTINUE
C
      GO TO 80
C
  50  IF(ISCALE.EQ.1) GO TO 80
      DO 70 I=1,NN
        K = MM * NN + I
        Q(K) = Y(I)
        DO 60 J=1,MM
          K = (J-1) * NN + I
          Q(K) = X(I,J)
  60    CONTINUE
  70  CONTINUE
C
C     CALL SUBROUTINE SCALE TO COMPUTE VECTOR NORMS AND TO SET VALUES OF
C        SCALE FACTORS (F).
C
  80  CONTINUE
      CALL SCALDP (ISCALE,1,NN,MM,IT,NR,W,WC,X,U,Q,S,B,A,Z,R,F,
     1            IFAULT)
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,2201)
 2201   FORMAT('AT START OF LSQ ROUTINE')
        CALL DPWRST('XXX','BUG ')
        DO2200I=1,N
          WRITE(ICOUT,2211)I,J,(X(I,J),J=1,MAX(M,5))
 2211     FORMAT('I,J,(X(I,J),J=1,MAX(M,5)) = ',2I5,5G15.7)
          CALL DPWRST('XXX','BUG ')
 2200   CONTINUE
      ENDIF
C
C     IFAULT IS SET EQUAL TO ONE IN SUBROUTINE SCALE WHEN A COLUMN OF
C        MATRIX X IS FOUND TO EQUAL ZERO.
C
      IF (IFAULT.EQ.1) GO TO 240
C
C     CALL SUBROUTINE PDECOM TO OBTAIN AN ORTHOGONAL QR-DECOMPOSITION OF
C        THE MATRIX CONTAINED IN Q ON ENTRY TO PDECOM.  ON RETURN FROM
C        PDECOM, M1 IS THE COMPUTED RANK OF THE SYSTEM OF EQUATIONS.
C        IF MATRIX Q IS FOUND TO BE SINGULAR, IS = 0 ON RETURN FROM
C        PDECOM.  OTHERWISE, IS = 1.
C
      CALL PDECOM (NN,MP1,TOL,W,WC,IS,M1,Q,T,R)
CCCCC APRIL 2002: PRINT WARNING MESSAGE FOR POTENTIAL SINGULARITY
C
      IF(IS.EQ.1)THEN
        WRITE(ICOUT,99)
   99   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1001)
 1001   FORMAT('***** WARNING: POTENTIAL SINGULARITY FROM (LINEAR) ',
     1         'FIT DETECTED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1003)
 1003   FORMAT('      POTENTIAL CAUSES OF SINGULARITY INCLUDE:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1005)
 1005   FORMAT('      1. A COLUMN IN THE X MATRIX CONTAINS ALL THE ',
     1         'SAME VALUES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1007)
 1007   FORMAT('      2. TWO COLUMNS IN THE X MATRIX ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1009)
 1009   FORMAT('      3. A MORE COMPLICATED LINEAR DEPENDENCY EXISTS ',
     1         'BETWEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1010)
 1010   FORMAT('         BETWEEN THE COLUMNS IN THE X MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1011)
 1011   FORMAT('      FOR MULTI-LINEAR FITS, DATAPLOT CHECKS FOR THE ',
     1         'FIRST TWO CAUSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1012)
 1012   FORMAT('      FOR SINGULARITY.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1013)
 1013   FORMAT('      RECOMMENDED FIX: PERFORM THE FIT AFTER REMOVING ',
     1         'ONE OR MORE OF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1014)
 1014   FORMAT('      ONE OR MORE OF THE INDEPENDENT VARIABLES.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF (IS.EQ.0) GO TO 100
      IF (M1.GT.0) GO TO 90
      GO TO 240
C
C     ..................................................................
C
  90  IF (M1.EQ.MM) GO TO 100
      IF (ISCALE.EQ.1) GO TO 240
      ISCALE = 1
      GO TO 10
 100  IR = ISCALE
C
C     TRANSFER T(J) TO ARRAY R SO THAT T IS AVAILABLE FOR WORK AREA.
C
      DO 110 I=1,MP1
CCCCC   LD = IDIV (2*(I-1)*MP1-I*(I-3),2,IRR)
        CALL IDIV (2*(I-1)*MP1-I*(I-3),2,IRR,LD)
        R(LD) = T(I)
 110  CONTINUE
C
C     CALL SUBROUTINE SLVE TO OBTAIN THE SOLUTION (COEFFICIENTS AND
C        RESIDUALS) OF THE LEAST SQUARES PROBLEM.  ITERATIVE REFINEMENT
C        IS USED TO IMPROVE (IF POSSIBLE) THE ACCURACY OF THE
C        INITIAL SOLUTION.  ON RETURN FROM SLVE, PARAMETER IR = 0 IF THE
C        ITERATIVE REFINEMENT PROCEDURE CONVERGED TO A SOLUTION.
C        OTHERWISE, IR = 1.
C
      CALL SLVE (NN,MM,NR,X,Y,W,WC,IT,ETA,F,U,Q,T,R,IR,B,P,Z,V,S,NI)
CCCCC THE FOLLOWING WRITE SECTION WAS ACTIVATED   SEPTEMBER 1995
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,771)
  771    FORMAT(1H ,'*****FROM LSQ, AFTER 1ST CALL TO SLVE--')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,772)E
  772    FORMAT('AFTER 120--E = ',E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
      D = V(1)
C
      IF (IR.EQ.0) GO TO 130
      IF (ISCALE.EQ.1) GO TO 120
      ISCALE = 1
      GO TO 10
 120  CONTINUE
CCCCC THE FOLLOWING LINE WAS ACTIVATED   SEPTEMBER 1995
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,773)ISCALE
  773    FORMAT('FROM LSQ, AFTER 120--ISCALE = ',I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
C     GO TO 240
      ID =2
      RETURN
C
C     COMPUTATIONS NEEDED FOR COMPUTING ACCURATE DIGITS.
C        SUBROUTINE SLVE IS NOW CALLED TO OBTAIN A VECTOR OF
C        COEFFICIENTS (A) BY FITTING PREDICTED VALUES (Y - Z) INSTEAD OF
C        THE ORIGINAL OBSERVATIONS (Y).  A COMPARISON OF VECTOR B WITH
C        VECTOR A IS USED TO ASSESS THE ACCURACY OF VECTOR B.
C        THIS CALL TO SLVE IS OMITTED WHENEVER --
C           L1 = 24  (TWOWAY)
C           L2 =  2  (SPOLYFIT)
C           L2 =  4  (SFIT)
C
C130  IF (L1.EQ.24) GO TO 140
C     IF (L2.EQ.2.OR. L2.EQ.4) GO TO 140
C
 130  IZ  = ISCALE
      ITT = IT + 2
C
      CALL SLVE (NN,MM,NR,X,Y,W,WC,ITT,ETA,F,U,Q,T,R,IZ,A,Z,P,V,S,NJ)
CCCCC THE FOLLOWING WRITE SECTION WAS ACTIVATED   SEPTEMBER 1995
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,775)
  775    FORMAT(1H ,'*****FROM LSQ, AFTER 2ND CALL TO SLVE--')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,776)IZ,ID,E
  776    FORMAT('AFTER 120--IZ,ID,E = ',2I8,E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (IZ.EQ.0) GO TO 140
      ID = 2
      RETURN
C
C     ..................................................................
C
C     COMPUTE SQUARED FOURIER COEFFICIENTS (S) NEEDED FOR ANALYSIS OF
C        VARIANCE.
C
 140  L = MP1
      DO 150 J=1,MM
CCCCC   LD = IDIV (2*(J-1)*(MM+1)-J*J+3*J,2,IRR)
       CALL IDIV  (2*(J-1)*(MM+1)-J*J+3*J,2,IRR,LD)
        S(J) = R(LD) * R(L)**2
        L = L + MP1 - J
 150  CONTINUE
C
C     CALL SUBROUTINE SCALE TO ADJUST RESIDUALS (Z) AND SQUARED
C        FOURIER COEFFICIENTS (S) FOR SCALING, IF DATA WERE SCALED.
C
      IF (ISCALE.EQ.1) THEN
      CALL SCALDP (ISCALE,3,NN,MM,IT,NR,W,WC,X,U,Q,S,B,A,Z,R,F,IFAULT)
      IF (IFAULT.EQ.1) GO TO 420
      ENDIF
C     ADJUST THE FIRST SQUARED FOURIER COEFFICIENT IF Y MID-RANGE WAS
C        SUBTRACTED FROM Y-VECTOR.  IN THIS CASE C IS NONZERO.
C
      YINC = C
CCCCC IF (YINC.NE.0.0) S(1) = R(1) * ( SPDIV(R(MP1),F(MP1),IND) +
CCCCC1  SPDIV(YINC,F(1),IRR) )**2
      IF(YINC.NE.0.0)CALL SPDIV(R(MP1),F(MP1),IND,RESUL1)
      IF(YINC.NE.0.0)CALL SPDIV(YINC,F(1),IRR,RESUL2)
      IF(YINC.NE.0.0)S(1)=R(1)*(RESUL1+RESUL2)**2
C
C     COMPUTE RESIDUAL SUM OF SQUARES (E) AND RESIDUAL STANDARD
C        DEVIATION (SD).
C
      CALL DSUMAL (DX,0,SNEG,SPOS,SUM)
      WW = WC
      DO 160 I=1,NN
        IF (WC.LE.0.0) WW = W(I)
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
           WRITE(ICOUT,871)I,WC,WW
  871      FORMAT('FROM LSQ,160--I,WC,WW = ',I8,2E15.7)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,872)I,Z(I),SUM
  872      FORMAT('FROM LSQ,160--I,Z(I),SUM = ',I8,E15.7,D15.7)
           CALL DPWRST('XXX','BUG ')
        ENDIF
        DX(1) = DBLE (Z(I)**2) * DBLE (WW)
        CALL DSUMAL (DX,-1,SNEG,SPOS,SUM)
 160  CONTINUE
      CALL DSUMAL (DX,1,SNEG,SPOS,SUM)
CCCCC RSS = DPCON (SUM)
      CALL  DPCON (SUM,RSS)
C
      IF (NN.EQ.MM) GO TO 170
      GO TO 180
C
 170  RMS = 0.0
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,873)NN,MM,RSS,WC
  873    FORMAT('FROM LSQ,170--NN,MM,RSS,WC = ',2I8,2E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 210
C
 180  NOZWTS = 0
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,874)NN,MM,RSS,WC
  874    FORMAT('FROM LSQ,180--NN,MM,RSS,WC = ',2I8,2E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (WC.GT.0.0) GO TO 200
      DO 190 I=1,NN
        IF (W(I).NE.0.0) GO TO 190
        NOZWTS = NOZWTS + 1
 190  CONTINUE
 200  RESDF = NN - MM - NOZWTS
CCCCC RMS = SPDIV (RSS,RESDF,IRR)
      CALL  SPDIV (RSS,RESDF,IRR,RMS)
C210  SD = SPSQRT (RMS)
 210  CONTINUE
      CALL SPSQRT (RMS,RESULT)
      SD=RESULT
      E = RSS
C
C     CALL SUBROUTINE SDPRED TO COMPUTE STANDARD DEVIATION OF PREDICTED
C        VALUES (V).
C
      CALL SDPRED (NN,MM,R,Q,T,SD,V)
C
C     CALL SUBROUTINE PINVRT TO OBTAIN THE INVERSE OF (X-TRANSPOSE)*W*X
C        USING RESULTS FROM PDECOM (MATRIX R) AS INPUT.
C
C     MATRIX R IS OVERWRITTEN AND WILL EQUAL THE DESIRED INVERSE UPON
C        RETURN TO SUBROUTINE LSQ.
C
C     SINCE THE INVERSE MATRIX IS SYMMETRIC, ONLY THE PORTION ON OR
C        ABOVE THE PRINCIPAL DIAGONAL IS STORED.  COMMENTS AT THE
C        BEGINNING OF SUBROUTINE PINVRT GIVE FURTHER DETAILS.
C
      CALL PINVRT (MM,R,T)
C
C     CALL SUBROUTINE SCALE TO ADJUST COEFFICIENTS (B AND A) AND
C        COVARIANCE MATRIX (R) FOR SCALING, IF DATA WERE SCALED.
C
      IF (ISCALE.EQ.1) THEN
      CALL SCALDP (ISCALE,4,NN,MM,IT,NR,W,WC,X,U,Q,S,B,A,Z,R,F,IFAULT)
      IF (IFAULT.EQ.1) GO TO 420
      ENDIF
C
C     COMPUTE STANDARD DEVIATIONS OF COEFFICIENTS (T).
C
      DO 230 I=1,MM
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,777)I,R(I),RMS,RESDF,RSS
  777    FORMAT('FROM LSQ,230--I,R(I),RMS,RESDF,RSS = ',I8,4E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
CCCCC   L = IDIV  (2*(I-1)*MM-I*I+3*I,2,IRR)
        CALL IDIV (2*(I-1)*MM-I*I+3*I,2,IRR,L)
        IF (R(L).GE.0.0) GO TO 220
        R(L) = 0.0
C220    T(I) = SPSQRT (R(L)*RMS)
 220    CONTINUE
        CALL   SPSQRT (R(L)*RMS,RESULT)
        T(I) = RESULT
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
          WRITE(ICOUT,778)I,T(I)
  778     FORMAT('FROM LSQ,230--I,T(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 230  CONTINUE
C
C     SET VALUE OF ID.
 240  ID=NI
      RETURN
C
 420  ID = 1
C     IF (ISCALE.EQ.0) ID = - ID
      RETURN
C
C     ==================================================================
C
      END
CCCCC-----SCALE--------------------------------------
      SUBROUTINE SCALDP (IS,NC,N,M,IT,NR,W,WC,X,U,Q,SS,B,A,Z,R,SF,IFT)
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE SCALE SCALES THE MATRIX Q IN ORDER TO MITIGATE THE
C        ROUNDING ERROR PROBLEMS WHICH CAN OCCUR IN CONNECTION WITH
C        SOLVING ILL-CONDITIONED SYSTEMS OF EQUATIONS.  THIS IS DONE BY
C        MULTIPLYING EACH COLUMN OF Q BY ITS APPROPRIATE SCALE FACTOR SO
C        THAT THE COLUMNS OF THE SCALED MATRIX ALL HAVE UNIT LENGTH.  IN
C        THE CASE OF POLYNOMIAL TYPE PROBLEMS, THE MEAN OF THE X-VECTOR
C        IS COMPUTED SO THAT IT CAN BE SUBTRACTED FROM EACH ELEMENT OF
C        X WHENEVER POWERS OF X ARE GENERATED (IN SUBROUTINES LSQ AND
C        SLVE).  AFTER A SOLUTION IS OBTAINED FOR A SCALED PROBLEM, THE
C        COEFFICIENTS, RESIDUALS, SQUARED FOURIER COEFFICIENTS AND
C        COVARIANCE MATRIX MUST BE ADJUSTED TO ACCOUNT FOR SCALING.
C
C     REFERENCE --
C        A. BJORCK, COMMENT ON THE ITERATIVE REFINEMENT OF LEAST-SQUARES
C        SOLUTIONS, JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C        VOL. 73 (1978), PP. 161-166.
C
C               WRITTEN BY -
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL BUREAU OF STANDARDS,
C                      GAITHERSBURG, MD. 20899
C                          TELEPHONE 301-975-2844
C
C     UPDATED--NOVEMBER  1989--DIMENSION (1) TO (*) (AND MOVE)
C     UPDATED         --NOVEMBER  2009. RENAME "SCALE" TO "SCALDP".  THIS
C                                       IS SIMPLY TO AVOID COMPILATION
C                                       ISSUES WITH VERSION 11 OF THE
C                                       INTEL COMPILER ON WINDOWS
C                                       (CONFLICTS WITH INTRINSIC
C                                       SCALE FUNCTION EVEN IF AN
C                                       EXTERNAL STATEMENT IS USED)
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
C
C
CCCCC THE FOLLOWING 5 LINES WERE MOVED       NOVEMBER 1989
CCCCC AND CHANGED DIMENSION (1) TO (*)
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC REAL             A(1), B(1), Q(1), R(1), SF(1), SS(1)
CCCCC REALCCCCC        W(1), X(NR,1), Z(1)
CCCCC REAL             W(1), X, Z(1)
CCCCC REAL             U, WC
CCCCC REAL             VNORM2, WW
C
CCCCC REAL             SPDIV, DPCON
C
      DOUBLE PRECISION DSUM
CCCCC DOUBLE PRECISION DPDIV, DPSQRT
      DOUBLE PRECISION DRESUL
C
      REAL             A(*), B(*), Q(*), R(*), SF(*), SS(*)
CCCCC REAL             W(1), X(NR,1), Z(1)
      REAL             W(*), X, Z(*)
      REAL             U, WC
      REAL             VNORM2, WW
C
CCCCC INCLUDE 'DPCOPA.INC'
CCCCC DIMENSION X(MAXOBV,MAXCMF)
      DIMENSION X(NR,*)
C
C     ==================================================================
C
      MP1 = M + 1
      IFT = 0
CCCCC TEMPORARY CHANGE OF NCC TO NC AS SUGGESTED BY RUTH VARNER MAY 1989
CCCCC GO TO (10,80,100,130), NCC
      GO TO (10,80,100,130), NC
  10  IF (IS.EQ.1) GO TO 30
C
C     IS = 0.  SET SF(I) = 1.0 FOR I=1,...,M+1.
C
      DO 20 I=1,MP1
        SF(I) = 1.0
  20  CONTINUE
      RETURN
C
C     ..................................................................
C
C     IS = 1.  COMPUTE VECTOR NORMS.
C                  COMPUTE SCALE FACTORS (SF).
C                  SCALE MATRIX Q.
C
  30  WW = WC
      DO 70 J=1,MP1
        DSUM = 0.0D0
        K = (J-1) * N + 1
        DO 40 I=1,N
          IF (WC.LE.0.0) WW = W(I)
          DSUM = DSUM + DBLE (Q(K)) * DBLE (Q(K)) * DBLE (WW)
          K = K + 1
  40    CONTINUE
CCCCC   DSUM   = DPSQRT (DSUM)
        CALL     DPSQRT (DSUM,DRESUL)
        DSUM   = DRESUL
CCCCC   VNORM2 = DPCON (DSUM)
        CALL     DPCON (DSUM,VNORM2)
C
C       VECTOR NORMS COULD BE SAVED HERE, IF DESIRED.
C
        IF (VNORM2.GT.0.0) GO TO 50
        IFT = 1
C
C       IFT = 1 INDICATES ERROR RETURN.
C
        RETURN
C
C     ..................................................................
C
CC50    SF(J) = SPDIV (1.0,VNORM2,IRR)
   50 CONTINUE
        CALL    SPDIV (1.0,VNORM2,IRR,SF(J))
C
C       SCALE MATRIX Q.
C
        K = (J-1) * N + 1
        DO 60 I=1,N
          Q(K) = Q(K) * SF(J)
          K    = K + 1
  60    CONTINUE
  70  CONTINUE
      RETURN
C
C     ..................................................................
C
C     COMPUTE MEAN OF X VECTOR (DENOTED BY U) FOR POLYNOMIAL TYPE
C        PROBLEMS.
C
  80  DSUM = 0.0D0
      NW   = 0
      DO 90 I=1,N
        L    = L + 1
        IF (WC.LE.0.0 .AND. W(I).EQ.0.0) GO TO 90
        NW   = NW + 1
        DSUM = DSUM + DBLE (X(I,1))
  90  CONTINUE
CCCCC U = DPCON (DPDIV (DSUM,DBLE (FLOAT (NW)),IRR))
      CALL        DPDIV (DSUM,DBLE (FLOAT (NW)),IRR,DRESUL)
CCCCC U = DPCON (DRESUL)
      CALL DPCON (DRESUL,U)
      RETURN
C
C     ..................................................................
C
C     ADJUST SQUARED FOURIER COEFFICIENTS (SS) AND RESIDUALS (Z) FOR
C        SCALING.
C
 100   DO 110 J=1,M
CCCCC   SS(J) = SPDIV (SS(J),SF(MP1)*SF(MP1),IRR)
        CALL    SPDIV (SS(J),SF(MP1)*SF(MP1),IRR,SS(J))
 110  CONTINUE
C
      DO 120 I=1,N
CCCCC   Z(I) = SPDIV (Z(I),SF(MP1),IRR)
        CALL   SPDIV (Z(I),SF(MP1),IRR,Z(I))
 120  CONTINUE
      RETURN
C
C     ..................................................................
C
C     ADJUST COEFFICIENTS (B AND A) AND COVARIANCE MATRIX (R) FOR
C        SCALING.
C
 130  DO 140 J=1,M
CCCCC   B(J) = SPDIV (B(J) * SF(J),SF(MP1),IRR)
        CALL   SPDIV (B(J) * SF(J),SF(MP1),IRR,B(J))
CCCCC   A(J) = SPDIV (A(J) * SF(J),SF(MP1),IRR)
        CALL   SPDIV (A(J) * SF(J),SF(MP1),IRR,A(J))
 140  CONTINUE
      L = 0
      DO 160 I=1,M
        DO 150 J=I,M
          L    = L + 1
          R(L) = R(L) * SF(I) * SF(J)
 150    CONTINUE
 160  CONTINUE
      IF (IT.EQ.2) RETURN
C
C     ..................................................................
C
C     COMPLETE ADJUSTMENTS OF B, A AND R FOR SCALING IN POLYNOMIAL TYPE
C        PROBLEMS.
C     REFERENCE --
C        G. A. F. SEBER, LINEAR REGRESSION ANALYSIS (1977), THEOREM
C        1.4 AND COROLLARIES, PAGES 10-11.
C
      K = 0
      DO 180 I=1,M
        DO 170 J=I,M
          K = K + 1
          L = (I - 1) * M + J
          Q(L) = R(K)
          IF (I.EQ.J) GO TO 170
          L = (J - 1) * M + I
          Q(L) = R(K)
 170    CONTINUE
 180  CONTINUE
      DO 250 I=1,M
        SF(I) = 1.0
        IP1   = I + 1
        IF (IP1.GT.M) GO TO 200
        DO 190 J=IP1,M
CCCCC     SF(J) = DPCON (-DPDIV (DBLE(FLOAT(J-1)),DBLE(FLOAT(J-I)),IND)
CCCCC1    * DBLE (SF(J-1)) * DBLE (U) )
          CALL   DPDIV (DBLE(FLOAT(J-1)),DBLE(FLOAT(J-I)),IND,DRESUL)
CCCCC     SF(J) = DPCON (-DRESUL)
CCCCC1    * DBLE (SF(J-1)) * DBLE (U)
          CALL    DPCON (-DRESUL,RESULT)
          SF(J) = RESULT
     1    * DBLE (SF(J-1)) * DBLE (U)
 190    CONTINUE
 200    DSUM = 0.0D0
        DO 210 J=I,M
          DSUM = DSUM + DBLE (SF(J)) * DBLE (B(J))
 210    CONTINUE
        B(I) = DSUM
        DSUM = 0.0D0
        DO 220 J=I,M
          DSUM = DSUM + DBLE (SF(J)) * DBLE (A(J))
 220    CONTINUE
        A(I) = DSUM
        DO 240 J=I,M
          DSUM = 0.0D0
          DO 230 K=I,M
            L = (K-1)*M + J
            DSUM = DSUM + DBLE (SF(K)) * DBLE (Q(L))
 230      CONTINUE
          L    = (I - 1) * M + J
          Q(L) = DSUM
 240    CONTINUE
 250  CONTINUE
      DO 300 J=1,M
        SF(J) = 1.0
        IP1   = J + 1
        IF (IP1.GT.M) GO TO 270
        DO 260 I=IP1,M
CCCCC     SF(I) = DPCON (-DPDIV (DBLE(FLOAT(I-1)),DBLE(FLOAT(I-J)),IND)
CCCCC1    * DBLE (SF(I-1)) * DBLE (U) )
          CALL   DPDIV (DBLE(FLOAT(I-1)),DBLE(FLOAT(I-J)),IND,DRESUL)
CCCCC     SF(I) = DPCON (-DRESUL)
CCCCC1    * DBLE (SF(I-1)) * DBLE (U)
          CALL    DPCON (-DRESUL,RESULT)
          SF(I) = RESULT
     1    * DBLE (SF(I-1)) * DBLE (U)
 260    CONTINUE
 270    DO 290 I=1,J
          DSUM = 0.0D0
          DO 280 K=J,M
            L    = (I - 1) * M + K
            DSUM = DSUM + DBLE (Q(L)) * DBLE (SF(K))
 280      CONTINUE
          L    = (I - 1) * M + J
          Q(L) = DSUM
 290    CONTINUE
 300  CONTINUE
      K = 0
      DO 320 I=1,M
        DO 310 J=I,M
          K    = K + 1
          L    = (I - 1) * M + J
          R(K) = Q(L)
 310    CONTINUE
 320  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
CCCCC-----PDECOM--------------------------------------
      SUBROUTINE PDECOM (KN,KM,TOL,W,WCC,ISING,M1,Q,D,R)
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE PDECOM USES A MODIFIED GRAM-SCHMIDT ALGORITHM TO OBTAIN
C        AN ORTHOGONAL QR-DECOMPOSITION OF THE INPUT MATRIX GIVEN IN Q.
C
C               WRITTEN BY -
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL BUREAU OF STANDARDS,
C                      GSITHERSBURG, MD. 20899
C                          TELEPHONE 301-975-2844
C
C     UPDATED--NOVEMBER  1989--DIMENSION (1) TO (*) (AND MOVE)
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
C
CCCCC THE FOLLOWING 3 LINES WERE MOVED        NOVEMBER 1989
CCCCC AND DIMENSION (1) CHANGED TO DIMENSION (*)
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC REAL             D(1), Q(1), R(1), W(1)
CCCCC REAL             TOL, WCC
CCCCC REAL             DMAX, DS, RSJ, TOL2, WW
C
CCCCC REAL             SPDIV, DPCON
C
      DOUBLE PRECISION DSUM
C
      REAL             D(*), Q(*), R(*), W(*)
      REAL             TOL, WCC
      REAL             DMAX, DS, RSJ, TOL2, WW
C
C     ==================================================================
C
      WW    = WCC
      ISING = 1
      M     = KM
      N     = KN
      M1    = 0
CCCCC M2 = IDIV (M*(M+1),2,IRR)
      CALL IDIV (M*(M+1),2,IRR,M2)
      DO 10 J=1,M
        D(J) = 0.0
  10  CONTINUE
C
      DO 20 L=1,M2
        R(L) = 0.0
  20  CONTINUE
C
      TOL2 = TOL * TOL
      DMAX = 0.0
      DO 110 I=1,M
C
C     STEP NUMBER I IN THE DECOMPOSITION.
C
        DSUM = 0.0D0
        DO 30 L=1,N
          IF (WCC.LE.0.0) WW = W(L)
          J = (I-1) * N + L
          DSUM = DSUM + DBLE (Q(J)) * DBLE (Q(J)) * DBLE (WW)
  30    CONTINUE
C
CCCCC   D(I) = DPCON (DSUM)
        CALL   DPCON (DSUM,D(I))
        DS = D(I)
        IF (I.GT.1) GO TO 40
        DMAX = D(1)
        GO TO 50
C
  40    IF (DS.GT.DMAX) DMAX = D(I)
  50    DO 60 J=1,I
          IF (D(J).LE.TOL2*DMAX) RETURN
  60    CONTINUE
C
        IF (DS.EQ.0.0) RETURN
        IPLUS1 = I + 1
        IF (IPLUS1.GT.M) GO TO 100
C
C     BEGIN ORTHOGONALIZATION.
C
CCCCC   LD = IDIV (2*(I-1)*M-I*I+3*I,2,IRR)
        CALL IDIV (2*(I-1)*M-I*I+3*I,2,IRR,LD)
        K = 1
        DO 90 J=IPLUS1,M
          DSUM = 0.0D0
          DO 70 L=1,N
            IF (WCC.LE.0.0) WW = W(L)
            LS = (I-1) * N + L
            LJ = (J-1) * N + L
            DSUM = DSUM + DBLE(Q(LS)) * DBLE(Q(LJ)) * DBLE (WW)
  70      CONTINUE
C
          L = LD + K
CCCCC     R(L) = DPCON (DSUM)
          CALL   DPCON (DSUM,R(L))
CCCCC     R(L) = SPDIV (R(L),DS,IRR)
          CALL   SPDIV (R(L),DS,IRR,R(L))
          RSJ  = R(L)
          K    = K + 1
          JJ   = (J-1) * N + 1
          JS   = (I-1) * N + 1
          DO 80 L=1,N
            Q(JJ) = Q(JJ) - RSJ * Q(JS)
            JJ    = JJ + 1
            JS    = JS + 1
  80      CONTINUE
C
  90    CONTINUE
C
C     END ORTHOGONALIZATION.
C
 100    M1 = I
        IF (I.EQ.M-1) ISING = 0
 110  CONTINUE
C
C     END STEP NUMBER I.
C
      RETURN
C
C     ==================================================================
C
      END
CCCCC-----SLVE--------------------------------------
      SUBROUTINE SLVE (N,M,NR,X,Y,W,WA,IT,E,S,U,Q,D,A,K,B,R,Z,F,G,NI)
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE SLVE COMPUTES THE SOLUTION (COEFFICIENTS AND RESIDUALS)
C        OF THE LEAST SQUARES PROBLEM.  ITERATIVE REFINEMENT IS USED TO
C        IMPROVE (IF POSSIBLE) THE ACCURACY OF THE INITIAL SOLUTION.
C
C     SUBROUTINE SLVE IS GENERALLY CALLED TWICE FROM SUBROUTINE LSQ.
C        IN THE FIRST CALL, THE OBSERVATIONS (Y) ARE FITTED.  LET R
C           DENOTE THE RESIDUALS FROM THIS FIT.
C        IN THE SECOND CALL, THE PREDICTED VALUES (Y - R) ARE FITTED.
C           THE COEFFICIENTS OBTAINED FROM THIS FIT WILL BE USED IN
C           ASSESSING THE ACCURACY OF THE COEFFICIENTS FROM THE FIRST FIT.
C
C *   CONVERSION OF THE PROGRAM TO STRICTLY DOUBLE PRECISION, AND      *
C *   CONVERSION OF THE PROGRAM TO STRICTLY SINGLE PRECISION.          *
C *      ON COMPUTERS HAVING SHORT WORD LENGTH (AS THE IBM 360/370)    *
C *      IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN DOUBLE     *
C *      PRECISION.  ON COMPUTERS HAVING LONG WORD LENGTH (AS THE CDC  *
C *      6600) IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN      *
C *      SINGLE PRECISION.  IN SUCH CASES, THE ITERATIVE REFINEMENT    *
C *      PRESENTLY INCLUDED IN SUBROUTINE SLVE SHOULD BE OMITTED.      *
C *                                                                    *
C *      THE SIMPLEST WAY TO OBTAIN THE EFFECT OF OMITTING THE         *
C *      ITERATIVE REFINEMENT (WITHOUT ACTUALLY DOING SO) IS TO CHANGE *
C *      THE ONE STATEMENT WHICH PRESENTLY READS                       *
C *        310  K = 1 (USE THIS FOR 64-BIT MACHINES)                *
C *      TO READ                                                       *
C *        310  K = 0 (USE THIS FOR 32-BIT MACHINES)               *
C *                                                                    *
C *      TO ACTUALLY OMIT THE ITERATIVE REFINEMENT THE FOLLOWING       *
C *      APPROACH MAY BE USED.                                         *
C *      1. OMIT USAGE OF E, ETA2, RNB, RNDB1, RNDB2, RNDR1, RNDR2,    *
C *         RNR, AND SPCA FROM SUBROUTINE, REAL, AND DATA STATEMENTS.  *
C *      2. ATTACH LABEL  30  TO THE STATEMENT WHICH PRESENTLY READS   *
C *               DO 50 I=1,KN                                         *
C *      3. INSERT A STATEMENT READING                                 *
C *               GO TO 320                                            *
C *         IMMEDIATELY BEFORE THE STATEMENT WHICH PRESENTLY READS     *
C *          160  DO 210 ISX=1,KM                                      *
C *      4. OMIT THE FOUR BLOCKS OF STATEMENTS WHICH ARE SET OFF IN    *
C *         THE FOLLOWING MANNER --                                    *
C *                                                                    *
C BLOCK I ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C               (STATEMENTS TO BE OMITTED)
C
C BLOCK I (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C *                                                                    *
C *         BLOCK 1 CONTAINS  3 STATEMENTS (EXCLUDING COMMENTS).       *
C *         BLOCK 2 CONTAINS 10 STATEMENTS (EXCLUDING COMMENTS).       *
C *         BLOCK 3 CONTAINS 22 STATEMENTS (EXCLUDING COMMENTS).       *
C *         BLOCK 4 CONTAINS  4 STATEMENTS (EXCLUDING COMMENTS).       *
C *                                                                    *
C               WRITTEN BY -
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL BUREAU OF STANDARDS,
C                      GAITHERSBURG, MD. 20899
C                          TELEPHONE 301-975-2844
C
C     UPDATED--NOVEMBER  1989--DIMENSION (1) TO (*) (AND MOVED)
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
CCCCC THE FOLLOWING 9 LINES WERE MOVED      NOVEMBER 1989
CCCCC AND CHANGED DIMENSION (1) TO (*)
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC REAL             A(1), B(1), D(1), F(1), G(1), Q(1)
CCCCC REALCCCCC        R(1), S(1), W(1), X(NR,M), Y(1), Z(1)
CCCCC REAL             R(1), S(1), W(1), X, Y(1), Z(1)
CCCCC REAL             E, U, WA
CCCCC REAL             C, ETA2, DIGITS, DXNORM
CCCCC REAL             RNB, RNDB1, RNDB2, RNDR1, RNDR2
CCCCC REAL             RNR, WC, WW, XNORM
CCCCC REALCCCCC        SPDIV, DPCON, SPLO10, SPSQRT
CCCCC REAL             SPCA
C
      DOUBLE PRECISION DX, DSUM, DY
C
      REAL             A(*), B(*), D(*), F(*), G(*), Q(*)
CCCCC REAL             R(*), S(*), W(*), X(NR,M), Y(*), Z(*)
      REAL             R(*), S(*), W(*), X, Y(*), Z(*)
      REAL             E, U, WA
      REAL             C, ETA2, DIGITS, DXNORM
      REAL             RNB, RNDB1, RNDB2, RNDR1, RNDR2
      REAL             RNR, WC, WW, XNORM
CCCCC REAL             SPDIV, DPCON, SPLO10, SPSQRT
      REAL             SPCA
C
CCCCC INCLUDE 'DPCOPA.INC'
      DIMENSION X(NR,*)
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA SPCA / 64.0 /
C
C     ==================================================================
C
C     SET ISWAD = 0 IF COEFFICIENTS FOR ACCURATE DIGITS ARE NOT BEING
C                   COMPUTED.
C     SET ISWAD = 1 IF COEFFICIENTS FOR ACCURATE DIGITS ARE BEING
C                   COMPUTED.
C
      ISWAD = 0
      IF (IT.GT.2) ISWAD = 1
      KN = N
      KM = M
      MN = KM * KN
      WC = WA
      ITYP   = IT
      IF (ITYP.GT.2) ITYP = ITYP - 2
      MPLUS1 = KM + 1
      DIGITS = 0.0
C
C BLOCK 1 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
CCCCC ITMAX = INT (-SPLO10(E)) - 2   JUNE 1987
      CALL SPLO10(E,RESULT)
      ITMAX = INT (-RESULT)    - 2
      IF (K.EQ.1) ITMAX = ITMAX + 3
      ETA2 = E * E
C
C BLOCK 1 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     USE ELEMENTS M*N+1, M*N+2, ..., M*N+N OF ARRAY Q AS WORK AREA.
C
CCCCC IF (WC.GT.0.0) WW = SPSQRT(WC)
      IF (WC.GT.0.0) CALL SPSQRT(WC,RESULT)
      IF (WC.GT.0.0) WW = RESULT
      DO 10 I=1,KN
CCCCC   IF (WC.LE.0.0) WW = SPSQRT(W(I))
        IF (WC.LE.0.0) CALL SPSQRT(W(I),RESULT)
        IF (WC.LE.0.0) WW = RESULT
        IF (ISWAD.EQ.0) F(I) = Y(I) * WW * S(MPLUS1)
CCCCC   IF (ISWAD.EQ.1 ) F(I) = (Y(I)-SPDIV(R(I),S(MPLUS1),IND)) * WW
CCCCC1                            * S(MPLUS1)
        IF (ISWAD.EQ.1 ) CALL         SPDIV(R(I),S(MPLUS1),IND,RESULT)
        IF (ISWAD.EQ.1 ) F(I) = (Y(I)-RESULT)                   * WW
     1                            * S(MPLUS1)
        J = MN + I
        Q(J) = 0.0
        Z(I) = 0.0
  10  CONTINUE
C
      DO 20 J=1,KM
        B(J) = 0.0
        G(J) = 0.0
  20  CONTINUE
C
      KI    = 0
      RNR   = 0.0
      RNB   = 0.0
      RNDB1 = 0.0
      RNDR1 = 0.0
C
C BLOCK 2 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      RNDB2 = 0.0
      RNDR2 = 0.0
C
C     BEGIN KI-TH ITERATION STEP.
C
  30  IF (KI.LT.2) GO TO 40
      IF (SPCA*RNDB2.LT.RNDB1 .AND. RNDB2.GT.ETA2*RNB .OR.
     1    SPCA*RNDR2.LT.RNDR1 .AND. RNDR2.GT.ETA2*RNR) GO TO 40
      GO TO 300
C
  40  RNDB1 = RNDB2
      RNDR1 = RNDR2
      RNDB2 = 0.0
      RNDR2 = 0.0
C
C BLOCK 2 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF (KI.EQ.0) GO TO 160
C
C     NEW RESIDUALS.
C
      DO 50 I=1,KN
CCCCC   IF (WC.LE.0.0) WW = SPSQRT(W(I))
        IF (WC.LE.0.0) CALL SPSQRT(W(I),RESULT)
        IF (WC.LE.0.0) WW = RESULT
               J = MN + I
        Q(J) = Q(J) + F(I) * WW
CCCCC   Z(I) = Z(I) + SPDIV (F(I),WW,IRR)
        CALL          SPDIV (F(I),WW,IRR,RESULT)
        Z(I) = Z(I) + RESULT
  50  CONTINUE
C
      DO 100 ISX=1,KM
        B(ISX) = B(ISX) + G(ISX)
        DSUM = 0.0D0
        IF (ITYP.EQ.2) GO TO 70
        DO 60 L=1,KN
          J  = MN + L
          DX = DBLE (Q(J)) * DBLE (S(ISX))
          IF (ISX.GT.1) DX = DX * DBLE(X(L,1)-U) ** (ISX-1)
          DSUM = DSUM + DX
  60    CONTINUE
        GO TO 90
C
  70    DO 80 L=1,KN
          J    = MN + L
          DSUM = DSUM + DBLE (Q(J)) * DBLE (X(L,ISX) * S(ISX))
  80    CONTINUE
C
CC90    G(ISX) = -DPCON (DSUM)
  90    CONTINUE
        CALL      DPCON (DSUM,RESULT)
        G(ISX) = -RESULT
 100  CONTINUE
C
      DO 150 I=1,KN
        DSUM = DBLE ( Z(I) )
        IF (ITYP.EQ.2) GO TO 120
        DSUM = DSUM + DBLE (B(1)) * DBLE (S(1))
        IF (KM.EQ.1) GO TO 140
        DO 110 L=2,KM
          DSUM = DSUM + DBLE(B(L))*DBLE(X(I,1)-U)**(L-1)*DBLE(S(L))
 110    CONTINUE
        GO TO 140
C
 120    DO 130 L=1,KM
          DSUM = DSUM + DBLE(B(L)) * DBLE(X(I,L) * S(L))
 130    CONTINUE
C
 140    DY = DBLE ( Y(I) )
CCCCC   IF (ISWAD.EQ.1) DY = DBLE (Y(I) - SPDIV (R(I),S(MPLUS1),IND) )
        IF (ISWAD.EQ.1) CALL         SPDIV (R(I),S(MPLUS1),IND,RESULT)
        IF (ISWAD.EQ.1) DY = DBLE (Y(I) - RESULT                    )
        DSUM = DSUM - DY * DBLE (S(MPLUS1))
CCCCC   F(I) = -DPCON (DSUM)
        CALL    DPCON (DSUM,RESULT)
        F(I) = -RESULT
CCCCC   IF (WC.LE.0.0) WW = SPSQRT(W(I))
        IF (WC.LE.0.0) CALL SPSQRT(W(I),RESULT)
        IF (WC.LE.0.0) WW = RESULT
        F(I) = F(I) * WW
CCCCC   IF (WW.EQ.0.0) Z(I) = DPCON (DBLE (Z(I)) - DSUM)
        IF (WW.EQ.0.0) CALL   DPCON (DBLE (Z(I)) - DSUM,Z(I))
 150  CONTINUE
C
C     END NEW RESIDUALS.
C
 160  DO 210 ISX=1,KM
        LESS1 = ISX - 1
        DSUM  = - DBLE (G(ISX))
        IF (1.GT.LESS1) GO TO 180
        J    = ISX
        DO 170 L=1,LESS1
          DSUM = DSUM + DBLE (D(L)) * DBLE (A(J))
          J = J + MPLUS1 - L
 170    CONTINUE
C
C180    D(ISX) = - DPCON (DSUM)
 180    CONTINUE
        CALL       DPCON (DSUM,RESULT)
        D(ISX) = - RESULT
        DO 190 L=1,KN
CCCCC     IF (WC.LE.0.0) WW = SPSQRT (W(L))
          IF (WC.LE.0.0) CALL SPSQRT (W(L),RESULT)
          IF (WC.LE.0.0) WW = RESULT
          JJ   = (ISX-1) * KN + L
          DSUM = DSUM + DBLE (F(L)) * DBLE (Q(JJ)) * DBLE (WW)
 190    CONTINUE
C
CCCCC   C  = DPCON (DSUM)
        CALL DPCON (DSUM,C)
CCCCC   LD = IDIV (2*(ISX-1)*(MPLUS1)-ISX*ISX+3*ISX,2,IRR)
        CALL IDIV (2*(ISX-1)*(MPLUS1)-ISX*ISX+3*ISX,2,IRR,LD)
CCCCC   C  = SPDIV (C,A(LD),IRR)
        CALL SPDIV (C,A(LD),IRR,C)
        G(ISX) = C
        DO 200 I=1,KN
CCCCC     IF (WC.LE.0.0) WW = SPSQRT (W(I))
          IF (WC.LE.0.0) CALL SPSQRT (W(I),RESULT)
          IF (WC.LE.0.0) WW = RESULT
          JJ   = (ISX-1) * KN + I
          F(I) = F(I) - C * Q(JJ) * WW
 200    CONTINUE
C
 210  CONTINUE
      DO 240 IS=1,KM
        ISX    = MPLUS1 - IS
        IPLUS1 = ISX + 1
        DSUM   = DBLE (-G(ISX))
        IF (IPLUS1.GT.KM) GO TO 230
CCCCC   LD     = IDIV (2*(ISX-1)*(MPLUS1)-ISX*ISX+3*ISX,2,IRR)
        CALL     IDIV (2*(ISX-1)*(MPLUS1)-ISX*ISX+3*ISX,2,IRR,LD)
        J      = 0
        DO 220 L=IPLUS1,KM
          J    = J + 1
          LJ   = LD + J
          DSUM = DSUM + DBLE (G(L)) * DBLE (A(LJ))
 220    CONTINUE
C230    G(ISX) = - DPCON (DSUM)
 230    CONTINUE
        CALL       DPCON (DSUM,RESULT)
        G(ISX) = - RESULT
 240  CONTINUE
C
C BLOCK 3 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DSUM = RNDB2
      DO 250 ISX=1,KM
        DSUM = DSUM + DBLE (G(ISX) * G(ISX) )
 250  CONTINUE
C
CCCCC RNDB2 = DPCON (DSUM)
      CALL    DPCON (DSUM,RNDB2)
      DSUM  = RNDR2
      DO 260 I=1,KN
        DSUM = DSUM + DBLE (F(I) * F(I) )
 260  CONTINUE
C
CCCCC RNDR2 = DPCON (DSUM)
      CALL    DPCON (DSUM,RNDR2)
      IF (KI.NE.0) GO TO 270
      RNB = RNDB2
      RNR = RNDR2
C
C     COMPUTE DIGITS = AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN
C                         INITIAL SOLUTION AND FIRST ITERATION.
C
 270  IF (KI.NE.1) GO TO 290
CCCCC XNORM  = SPSQRT (RNB)
      CALL     SPSQRT (RNB,RESULT)
      XNORM  = RESULT
CCCCC DXNORM = SPSQRT (RNDB2)
      CALL     SPSQRT (RNDB2,RESULT)
      DXNORM = RESULT
      IF (XNORM.NE.0.0) GO TO 280
CCCCC DIGITS = - SPLO10 (E)  JUNE 1987
      CALL SPLO10(E,RESULT)
      DIGITS = - RESULT
      GO TO 290
C
C280  DIGITS = - SPLO10 (AMAX1(SPDIV(DXNORM,XNORM,IND),E))
  280 CONTINUE
CCCCC CALL       SPLO10 (AMAX1(SPDIV(DXNORM,XNORM,IND),E),RESULT)
      CALL                     SPDIV(DXNORM,XNORM,IND,RESUL2)
      CALL       SPLO10 (AMAX1(RESUL2,E),RESULT)
      DIGITS = - RESULT
C
C     END KI-TH ITERATION STEP.
C
 290  KI = KI + 1
      IF (KI.GT.ITMAX) GO TO 310
C
C BLOCK 3 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      GO TO 30
C
C BLOCK 4 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
 300  IF (RNDR2.GT.4.0*ETA2*RNR .AND. RNDB2.GT.4.0*ETA2*RNB) GO TO 310
      K = 0
      GO TO 320
C
C     NOTE: IF SINGLE PRECISION = DOUBLE PRECISION, THEN YOU WANT TO
C           EFFECTIVELY OMIT ITERATIVE REFINEMENT.
C310  K = 1    COMMENTED OUT (JUNE 1987) TO GIVE CORRECT ANSWERS ON THE VAX.
C310  K = 0
 310  CONTINUE
CCCCC print *,'k = ',k
      K = 0
C
C BLOCK 4 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
 320  NI   = KI - 1
      F(1) = DIGITS
      RETURN
C
C     ==================================================================
C
      END
CCCCC-----DSUMAL--------------------------------------
      SUBROUTINE DSUMAL (DX,NN,SNEG,SPOS,SUM)
CCCCC SUBROUTINE DSUMAL (DX,NN,SUM)
CCCCC THE ARGUMENTS SNEG AND SPOS WERE ADDED     SEPTEMBER 1995
CCCCC UPDATED--SEPTEMBER 1995 HAVE SNEG & SPOS AS  INPUT/OUTPUT ARGUMENTS
CCCCC                         TO AVOID FAILURE-TO-SAVE ON SOME COMPUTERS
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     ALGORITHM DESCRIBED BY MALCOLM IN COM. OF ACM VOL. 14, NO. 11
C
C     SPECIAL ALGORITHM FOR SUMMING DOUBLE PRECISION NUMBERS.
C        (USE SUMMAL, IF NUMBERS ARE REAL.)
C
C     NN EQUALS       ZERO, CLEAR AREA TO PREPARE FOR NEW SUM.
C     NN EQUALS        ONE, OBTAIN FINAL SUM.
C     NN GREATER THAN ZERO, CLEAR, DO SUM ON NN TERMS AND GET FINAL SUM.
C     NN LESS THAN    ZERO, CONTINUE SUM FOR NEXT ABS(NN) TERMS,
C                              DO NOT GET FINAL SUM.
C
C               WRITTEN BY -
C                      SALLY T. PEAVY,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL BUREAU OF STANDARDS,
C                      GAITHERSBURG, MD. 20899
C                          TELEPHONE 301-975-2844
C
C     UPDATED--NOVEMBER  1989--DIMENSION (1) TO DIMENSION (*)
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
CCCCC THE FOLLOWING LINE WAS MOVED AND       NOVEMBER 1989
CCCCC CONVERTED (1) TO (*)
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC DIMENSION DX(1)
C
      DOUBLE PRECISION             DX, SUM, SNEG, SPOS
C
      DIMENSION DX(*)
C
C     ==================================================================
C
CCCCC IF(NN) 30,10,20
      IF(NN.LT.0)THEN
        GOTO30
      ELSEIF(NN.EQ.0)THEN
        GOTO10
      ELSEIF(NN.GT.0)THEN
        GOTO20
      ENDIF
  10  SPOS = 0.0
      SNEG = 0.0
      RETURN
C
C     ..................................................................
C
  20  IF (NN.EQ.1) GO TO 50
      SPOS = 0.0
      SNEG = 0.0
C
  30  N = IABS (NN)
      DO 40 I=1,N
        IF (DX(I).LT.0.0) SNEG = SNEG + DX(I)
        IF (DX(I).GE.0.0) SPOS = SPOS + DX(I)
  40  CONTINUE
C
      IF (NN.LT.0) RETURN
C
  50  SUM = SPOS + SNEG
      RETURN
C
C     ==================================================================
C
      END
      SUBROUTINE SDPRED (N,M,R,Q,SB,SD,SDYHAT)
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE SDPRED COMPUTES STANDARD DEVIATIONS OF PREDICTED
C        VALUES.
C
C               WRITTEN BY -
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL BUREAU OF STANDARDS,
C                      GAITHERSBURG, MD. 20899
C                          TELEPHONE 301-975-2844
C
C     UPDATED--NOVEMBER  1989--DIMENSION (1) TO DIMENSION (*)
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
CCCCC THE FOLLOWING LINE WAS TRANSLATED TO    NOVEMBER 1989
CCCCC 4 DIMENSION STATEMENTS (SEE BELOW)
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC REAL             Q(1), R(1), SB(1), SDYHAT(1)
      REAL             SD
CCCCC REAL             SPDIV, DPCON, SPSQRT
C
      DOUBLE PRECISION DSUM
C
      DIMENSION Q(*)
      DIMENSION R(*)
      DIMENSION SB(*)
      DIMENSION SDYHAT(*)
C
C     ==================================================================
C
      DO 10 J=1,M
CCCCC   L =  IDIV (2*(J-1)*(M+1)-J*J+3*J,2,IND)
        CALL IDIV (2*(J-1)*(M+1)-J*J+3*J,2,IND,L)
CCCCC   SB(J) = SPDIV (1.0,SPSQRT (R(L)),IND)
        CALL SPSQRT(R(L),RESULT)
CCCCC   SB(J) = SPDIV (1.0,RESULT,IND)
        CALL    SPDIV (1.0,RESULT,IND,SB(J))
  10  CONTINUE
C
      DO 30 I=1,N
        DSUM = 0.0D0
        DO 20 J=1,M
          L = (J-1) * N + I
          DSUM = DSUM + (DBLE (Q(L)) * DBLE (SB(J))) ** 2
  20    CONTINUE
C
CCCCC   SDYHAT(I) = DPCON (DSUM)
        CALL        DPCON (DSUM,SDYHAT(I))
        IF (SDYHAT(I).LT.0.0) SDYHAT(I) = 0.0
CCCCC   SDYHAT(I) = SD * SPSQRT (SDYHAT(I))
        CALL SPSQRT(SDYHAT(I),RESULT)
        SDYHAT(I) = SD * RESULT
  30  CONTINUE
      RETURN
C
C     ==================================================================
C
      END
CCCCC-----PINVRT--------------------------------------
      SUBROUTINE PINVRT (M,R,D)
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     SUBROUTINE PINVRT OBTAINS THE UNSCALED COVARIANCE MATRIX OF THE
C        COEFFICIENTS, EQUAL TO THE INVERSE OF (X-TRANSPOSE)*W*X.
C        MATRIX R OBTAINED FROM SUBROUTINE PDECOM IS USED AS INPUT.
C        THIS MATRIX IS OVERWRITTEN AND ON EXIT WILL EQUAL THE DESIRED
C        INVERSE.
C
C     SINCE THE INVERSE MATRIX IS SYMMETRIC, ONLY THE PORTION ON OR
C        ABOVE THE PRINCIPAL DIAGONAL IS STORED.
C
C               WRITTEN BY -
C                      ROY H. WAMPLER,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL BUREAU OF STANDARDS,
C                      GAITHERSBURG,MD. 20899
C                          TELEPHONE 301-975-2844
C
C      UPDATED--NOVEMBER  1989--DIMENSION (1) TO DIMENSION (*)
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
CCCCC THE FOLLOWING LINE WAS TRANSLATED INTO     NOVEMBER 1989
CCCCC 2 DIMENSION STATEMENTS (SEE BELOW)
CCCCC (BUG UNCOVERED BY NELSON HSU)
CCCCC REAL             D(1), R(1)
C
CCCCC REAL             SPDIV, DPCON
C
      DOUBLE PRECISION DSUM
C
      DIMENSION D(*)
      DIMENSION R(*)
C
C     ==================================================================
C
      DO 10 L=1,M
CCCCC   LL = IDIV (2*(L-1)*(M+1)-L*L+3*L,2,IRR)
        CALL IDIV (2*(L-1)*(M+1)-L*L+3*L,2,IRR,LL)
CCCCC   R(LL) = SPDIV (1.0,R(LL),IRR)
        CALL    SPDIV (1.0,R(LL),IRR,R(LL))
  10  CONTINUE
C
      IF (M.EQ.1) RETURN
      L = M
  20  J = L - 1
CCCCC LJ = IDIV (2*(J-1)*(M+1)-J*J+3*J,2,IRR)
      CALL IDIV (2*(J-1)*(M+1)-J*J+3*J,2,IRR,LJ)
      INC = 0
      DO 30 K=L,M
        INC  = INC + 1
        JK   = LJ + INC
        D(K) = R(JK)
  30  CONTINUE
C
      I = M
      DO 50 KA=J,M
        DSUM = 0.0D0
        IF (I.EQ.J) DSUM = DBLE (R(LJ))
        DO 40 K=L,M
          JK    = MIN0 (K,I)
CCCCC     LL    = IDIV (2*(JK-1)*(M+1)-JK*JK+3*JK,2,IRR)
          CALL    IDIV (2*(JK-1)*(M+1)-JK*JK+3*JK,2,IRR,LL)
          INC   = IABS (K-I)
          JK    = LL + INC
          DSUM = DSUM -DBLE (D(K)) * DBLE (R(JK))
  40    CONTINUE
        INC = I - J
        JK = LJ + INC
CCCCC   R(JK) = DPCON (DSUM)
        CALL    DPCON (DSUM,R(JK))
        I = I - 1
  50  CONTINUE
      L = L - 1
      IF (L.GT.1) GO TO 20
C
C    C
C     PACK VECTOR R.
C
      DO 70 I=2,M
CCCCC   L =  IDIV (2*(I-1)*M-I*I+3*I,2,IRR)
        CALL IDIV (2*(I-1)*M-I*I+3*I,2,IRR,L)
        DO 60 J=I,M
          K = L + I - 1
          R(L) = R(K)
          L = L + 1
  60    CONTINUE
  70  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
CCCCC-----DPDIV--------------------------------------
      SUBROUTINE DPDIV(FN,FD,IND,DRESUL)
C
C     PURPOSE--PERFORM DOUBLE PRECISION DIVISION FN/FD,
C              IF THE DENOMINATOR EQUALS ZERO,
C              THE RESULT IS SET TO ZERO,
C              AND THE INDICATOR, IND, IS SET EQUAL TO ONE.
C              OTHERWISE, IND IS SET TO 0.
C     INPUT  ARGUMENTS--FN
C                     --FD
C     OUTPUT ARGUMENTS--IND
C                     --DRESUL
C     WRITTEN BY--ROY WAMPLER
C                 DAVE HOGBEN
C                 SALLY PEAVY
C     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION FN
      DOUBLE PRECISION FD
      DOUBLE PRECISION DRESUL
C
C-----DIMENSION-------------------------------------------------------
 
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
      IND = 0
      IF(FD.EQ.0.0D0)GOTO1010
      DRESUL=FN/FD
      GOTO9000
C
 1010 CONTINUE
      DRESUL=0.0D0
      IND=1
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
CCCCC-----SPDIV--------------------------------------
      SUBROUTINE SPDIV(FN,FD,IND,RESULT)
C
C     PURPOSE--PERFORM SINGLE PRECISION DIVISION FN/FD,
C              IF THE DENOMINATOR EQUALS ZERO,
C              THE RESULT IS SET TO ZERO,
C              AND THE INDICATOR, IND, IS SET EQUAL TO ONE.
C              OTHERWISE, IND IS SET TO 0.
C     INPUT  ARGUMENTS--FN
C                     --FD
C     OUTPUT ARGUMENTS--IND
C                     --RESULT
C     WRITTEN BY--ROY WAMPLER
C                 DAVE HOGBEN
C                 SALLY PEAVY
C     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C-----DIMENSION-------------------------------------------------------
 
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
      IND = 0
      IF(FD.EQ.0.0D0)GOTO1010
      RESULT=FN/FD
      GOTO9000
C
 1010 CONTINUE
      RESULT=0.0D0
      IND=1
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
CCCCC-----DPCON--------------------------------------
      SUBROUTINE DPCON(DX,RESULT)
C
C     PURPOSE--CONVERT DOUBLE PRECISION NUMBER
C              TO SINGLE PRECISION NUMBER BY OCTAL ROUNDING
C              INSTEAD OF TRUNCATION.
C     INPUT  ARGUMENTS--DX          (DOUBLE PRECISION)
C     OUTPUT ARGUMENTS--RESULT      (SINGLE PRECISION)
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL BUREAU OF STANDARDS,
C                      WASHINGTON, DC 20234
C                          TELEPHONE 301-975-2855
C                  ORIGINAL VERSION -   AUGUST, 1969.
C                   CURRENT VERSION - NOVEMBER, 1978.
C     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL             Y
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DXX
      DOUBLE PRECISION  D
C
C-----DIMENSION-------------------------------------------------------
 
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
C-----DATA STATEMETNS-------------------------------------------------
C
      DATA RMIFY / -1.0E37 /
      DATA RPIFY /  1.0E38 /
C
C-----START POINT-----------------------------------------------------
C
      DXX = DX
      IF (DXX.GT.DBLE(RPIFY)) DXX = RPIFY
      IF (DXX.LT.DBLE(RMIFY)) DXX = RMIFY
C
      Y = DXX
      D = Y
      RESULT = DXX + (DXX-D)
C
      RETURN
      END
CCCCC-----DPSQRT--------------------------------------
      SUBROUTINE DPSQRT(DX,DRESUL)
C
C     PURPOSE--PERFORM DOUBLE PRECISION SQUARE ROOT OF DX,
C              IF THE DENOMINATOR IS LESS THAN 0,
C              THE OUTPUT RESULT IS SET TO 0,
C              AND AN ARITHMETIC FAULT MESSAGE IS PRINTED.
C     INPUT  ARGUMENTS--X
C     OUTPUT ARGUMENTS--DRESUL
C     WRITTEN BY--ROY WAMPLER
C                 DAVE HOGBEN
C                 SALLY PEAVY
C     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DRESUL
C
C-----DIMENSION-------------------------------------------------------
 
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
      IF(DX.LE.0.0D0)GOTO1010
      DRESUL=DSQRT(DX)
      GOTO9000
C
 1010 CONTINUE
      DRESUL=0.0D0
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
CCCCC-----SPSQRT--------------------------------------
      SUBROUTINE SPSQRT(X,RESULT)
C
C     PURPOSE--PERFORM SINGLE PRECISION SQUARE ROOT OF X,
C              IF THE DENOMINATOR IS LESS THAN 0,
C              THE OUTPUT RESULT IS SET TO 0,
C              CALLS ERROR(101) IS DONE.
C     INPUT  ARGUMENTS--X
C     OUTPUT ARGUMENTS--RESULT
C     WRITTEN BY--ROY WAMPLER
C                 DAVE HOGBEN
C                 SALLY PEAVY
C     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--NOVEMBER  1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
C-----DIMENSION-------------------------------------------------------
 
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
      IF(X.LE.0.0)GOTO1010
      RESULT=SQRT(X)
      GOTO9000
C
 1010 CONTINUE
      RESULT=0.0
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
CCCCC-----SPLO10--------------------------------------
      SUBROUTINE SPLO10(X,RESULT)
C
C     PURPOSE--COMPUTER LOG TO BASE 10 OF X
C              USING LIBRARY FUNCTION OF X IS POSITIVE, OR
C              CALLS ERROR(101) AND SETS FUNCTION VALUE
C              EQUAL TO 0 IF X IS NONPOSITIVE.
C
C     INPUT  ARGUMENTS--X
C     OUTPUT ARGUMENTS--RESULT
C     WRITTEN BY--ROY WAMPLER
C                 DAVE HOGBEN
C                 SALLY PEAVY
C     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C-----DIMENSION-------------------------------------------------------
 
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
      IF(X.GT.0.0)GOTO1020
      RESULT=0.0
      GOTO9000
C
 1020 CONTINUE
      RESULT=LOG10(X)
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
CCCCC-----IDIV--------------------------------------
      SUBROUTINE IDIV(IN,ID,IND,IRESUL)
C
C     PURPOSE--THIS INTEGER FUNCTION PERFORMS THE DIVISION IN/ID, WHEN
C              THE NUMERATOR, IN, AND THE DENOMINATOR, ID, ARE INTEGERS.
C              IF ID = 0, THE FUNCTION VALUE IS SET EQUAL TO ZERO.
C
C     INPUT  ARGUMENTS--IN
C                     --ID
C     OUTPUT ARGUMENTS--IND
C                     --IRESUL
C     WRITTEN BY--ROY WAMPLER
C                 DAVE HOGBEN
C                 SALLY PEAVY
C     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C-----DIMENSION-------------------------------------------------------
 
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
      IND = 0
      IF(ID.EQ.0)GOTO1010
      IRESUL=IN/ID
      GOTO9000
C
 1010 CONTINUE
      IRESUL=0
      IND=1
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
*BACK
      SUBROUTINE BACK (NC,LB,L,K,MV,RS,A,I,JC,ID,XI,MD,II,NI,ND,KZ,NL,N)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   BACK V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                         LOOK BACK COMPUTATION OF RSS
C
C     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR
C                   REGRESSIONS BY LEAPS AND BOUNDS
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS
C                     G.M.FURNIVAL AND R.W.WILSON
C               YALE UNIVERSITY AND U.S. FOREST SERVICE
C                           VERSION 11/11/74
C
C               ADAPTED TO OMNITAB BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION I(ND,ND), ID(ND), K(ND), NC(ND,ND), NI(ND), MD(ND,ND)
C
      REAL             XI(NL)
      REAL             A, RS
      REAL             B
      REAL             FDIV
C
      DATA ITHRE  /3/
      DATA IONE   /1/
      DATA IZERO  /0/
C
C     ==================================================================
C
C                               FIND SOURCE MATRIX.
C
  10  ISUB1 = K(JC)
      IF (LB.LE.NI(ISUB1)) GO TO 20
      JC = JC - IONE
      GO TO 10
C
C                            ADJUST FOR PREVIOUS PIVOTS.
C
  20  ISUB2 = IONE
      ISUB3 = IONE
      DO 50 J=JC,MV
        IN    = K(J)
        L     = I(IN,LB)
        MM    = ID(IN)
        ISUB2 = MM + MD(L,KZ)
        ISUB3 = MM + MD(L,L)
        IF (J.EQ.MV) GO TO 60
        IS    = K(J+1)
        ISUB4 = ID(IS) + MD(LB,KZ)
        IP    = I(IN,IS-1)
        ISUB5 = MM + MD(IP,L)
        ISUB6 = MM + MD(IP,IP)
        ISUB7 = MM + MD(IP,KZ)
        B     = FDIV (XI(ISUB5),XI(ISUB6),IND)
        KA    = IS
  30    IF (KA.GT.LB) GO TO 40
        KN    = I(IN,KA)
        ISUB8 = ID(IS) + MD(KA,LB)
        ISUB9 = MM + MD(KN,L)
        ISUB0 = MM + MD(KN,IP)
        XI(ISUB8) = XI(ISUB9) - B * XI(ISUB0)
        KA    = KA + IONE
        GO TO 30
  40    XI(ISUB4) = XI(ISUB2) - B * XI(ISUB7)
        NI(IS) = LB
        I(IS,LB) = LB
        N = N + ITHRE + LB - IS
        IF (II.EQ.IZERO) NC(IS,LB) = NC(IN,L)
  50  CONTINUE
C
C                                 CURRENT PIVOT.
C
  60  RS = A - FDIV (XI(ISUB2)*XI(ISUB2),XI(ISUB3),IND)
      RETURN
C
C     ================================================================
C
      END
*CODEXY
      SUBROUTINE CODEXY (X,N,SUMX,AVEX,XCODE,SQRTCT,U,L)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CODEXY V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROCEDURE FOR CODING X FOR ACCURATELY COMPUTING
C        SUM OF SQUARED DEVIATIONS FROM THE MEAN.
C
C     INPUT PARAMETERS ARE -
C
C            X = VECTOR OF MEASUREMENTS
C            N = LENGTH OF X
C
C     OUPUT PARAMETERS ARE -
C
C         SUMX = DOUBLE PRECISION SUM OF X MEASUREMENTS
C         AVEX = SINGLE PRECISION AVERAGE OF THE X MEASUREMENTS
C        XCODE = CODED VALUE TO BE USED INSTEAD OF AVERAGE FOR
C                   CUMPUTING DEVIATIONS ABOUT THE MEAN.
C                   XCODE IS THE VALUE OF X(I) CLOSEST TO AVEX.
C       SQRTCT = SQUARE ROOT OF CORRECTION TERM FOR COMPUTING
C                   SUM OF SQUARED DEVIATIONS ABOUT THE MEAN.
C
C                   SUM (X-AVEX)**2 = SUM(X-CODEX)**2 - SQRTCT**2,
C
C                   WHERE SQRTCT = (SUMX-N*XCODE)/SQRT(N)
C
C         U(I) = X(I) -XCODE, = CODED VALUES OF X
C            L = VALUE OF I FOR WHICH XCODE = X(I).
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      REAL             X(*), U(*)
      REAL             AVEX, DELTA, XCODE
      REAL             FDPCON
C
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION DN, SQRTCT, SUMX
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DX(1)
      DOUBLE PRECISION SNEG
      DOUBLE PRECISION SPOS
C
      DATA DZERO  /0.0D0/
      DATA IONE   /1/
      DATA IZERO  /0/
C     ==================================================================
C
      SNEG=0.0D0
      SPOS=0.0D0
C     COMPUTE AVEX.
C
CCCCC CALL DSUMAL (DX,IZERO,SUMX)
      CALL DSUMAL (DX,IZERO,SNEG,SPOS,SUMX)
      DO 10 I=1,N
        DX(1) = DBLE ( X(I) )
CCCCC   CALL DSUMAL (DX,-IONE,SUMX)
        CALL DSUMAL (DX,-IONE,SNEG,SPOS,SUMX)
  10  CONTINUE
CCCCC CALL DSUMAL (DX,IONE,SUMX)
      CALL DSUMAL (DX,IONE,SNEG,SPOS,SUMX)
C
      DN = N
C
      AVEX = FDPCON ( FDDIV (SUMX,DN,IND) )
C
C     COMPUTE XCODE AND L.
C
      L = IONE
      DELTA = ABS (X(1)-AVEX)
      DO 30 I=2,N
CCCCC   IF (ABS(X(I)-AVEX)-DELTA) 20,30,30
        IF (ABS(X(I)-AVEX)-DELTA.LT.0.0) THEN
           L = I
           DELTA = ABS (X(I)-AVEX)
        ENDIF
   30 CONTINUE
C
      XCODE = X(L)
C
C     COMPUTE CODED X = (X-XCODE).
C
      DO 40 I=1,N
        U(I) = X(I) - XCODE
  40  CONTINUE
C
C     COMPUTE CORRECTION TERM
C        FOR COMPUTING SUMX OF DEVIATIONS ABOUT THE MEAN.
C
      SQRTCT = FDDIV (SUMX-DN*DBLE(XCODE),FDSQRT(DN),IND)
C
      RETURN
C
C     ==================================================================
C
      END
*COEF
      SUBROUTINE COEF (R2,BIC,MP,KZ,XI,RR,MAXC,IND,NDEF,M,
     1                 ND,MD,NL,IB,ZC,
     1                 AMAT,IVALUE,NCVALU,MAXROW,NUMCLI,ITITL9,NCTIT9)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   COEF V 7.00  8/27/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                     COMPUTES REGRESSION STATISTICS
C
C ******************************************************************** *
C                                                                      *
C     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR     *
C                   REGRESSIONS BY LEAPS AND BOUNDS                    *
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
C                     G.M.FURNIVAL AND R.W.WILSON                      *
C               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
C                           VERSION 11/11/74                           *
C                                                                      *
C ******************************************************************** *
C
C               MODIFIED TO PFORT BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - SEPTEMBER, 1976.
C                   CURRENT VERSION -    AUGUST, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION IND(ND), MD(ND,ND), NALPHA(15), NOUT(12)
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
CCCCC REAL             RR(29,29), XI(NL), ZC(ND)
      REAL             RR(MAXC,MAXC), XI(NL), ZC(ND)
      REAL             DBET, F, R2, VAR
      REAL             FDIV
C 
      REAL AMAT(MAXROW,NUMCLI)
      INTEGER NCVALU(MAXROW,NUMCLI)
      CHARACTER*8 IVALUE(MAXROW,NUMCLI)
      CHARACTER*(*) ITITL9
C
C     ..................................................................
C
      CHARACTER NALPHA*1, NOUT*1
C
      PARAMETER (MAXV=38)
      CHARACTER*1 ICOD(MAXV)
      CHARACTER*38 IOUT
      CHARACTER*8 IVLIST
      COMMON/BESTC1/IOUNI1,IOUNI2
      COMMON/BESTC2/IVLIST(MAXV)
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA NOUT( 1), NOUT( 2), NOUT( 3), NOUT( 4), NOUT( 5), NOUT( 6) /
     1          'R',      '*',      '*',      '2',      'R',      '*' /
      DATA NOUT( 7), NOUT( 8), NOUT( 9), NOUT(10), NOUT(11), NOUT(12) /
     1          '*',      '2',      'C',      '(',      'P',      ')' /
      DATA ICOD(1) /'1'/
      DATA ICOD(2) /'2'/
      DATA ICOD(3) /'3'/
      DATA ICOD(4) /'4'/
      DATA ICOD(5) /'5'/
      DATA ICOD(6) /'6'/
      DATA ICOD(7) /'7'/
      DATA ICOD(8) /'8'/
      DATA ICOD(9) /'9'/
      DATA ICOD(10) /'0'/
      DATA ICOD(11) /'A'/
      DATA ICOD(12) /'B'/
      DATA ICOD(13) /'C'/
      DATA ICOD(14) /'D'/
      DATA ICOD(15) /'E'/
      DATA ICOD(16) /'F'/
      DATA ICOD(17) /'G'/
      DATA ICOD(18) /'H'/
      DATA ICOD(19) /'I'/
      DATA ICOD(20) /'J'/
      DATA ICOD(21) /'K'/
      DATA ICOD(22) /'L'/
      DATA ICOD(23) /'M'/
      DATA ICOD(24) /'N'/
      DATA ICOD(25) /'O'/
      DATA ICOD(26) /'P'/
      DATA ICOD(27) /'Q'/
      DATA ICOD(28) /'R'/
      DATA ICOD(29) /'S'/
      DATA ICOD(30) /'T'/
      DATA ICOD(31) /'U'/
      DATA ICOD(32) /'V'/
      DATA ICOD(33) /'W'/
      DATA ICOD(34) /'X'/
      DATA ICOD(35) /'Y'/
      DATA ICOD(36) /'Z'/
      DATA ICOD(37) /'a'/
      DATA ICOD(38) /'b'/
C
C     IF THE FOLLOWING VALUE IS CHANGED,
C        THE DIMENSION OF NALPHA MUST BE CHANGED AND
C        15A1 MUST BE CHANGED IN FORMAT 70.
C
      DATA NX / 15 /
C
      DATA IFOUR  /4/
      DATA ITHRE  /3/
C
CCCCC NOTE: ISIGD = 7 CAUSES PROBLEMS ON MICROSOFT COMPILER, SGI
CCCCC       COMPILER.  JUST SET TO 6 TO BE SAFE.
CCCCC DATA ISIGD  /7/
      DATA ISIGD  /6/
C
C     ==================================================================
C
      IEND = IFOUR * IB
      IBEG = IEND - ITHRE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,60) (NOUT(I),I=IBEG,IEND), R2
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,61)
CCCCC CALL DPWRST('XXX','BUG ')
CC60  FORMAT(19X,4A1,' = ',F7.3)
CC61  FORMAT(4X,'VARIABLE',9X,'COEFFICIENT',7X,'F RATIO')
C
      ITITL9='C(p) = '
      WRITE(ITITL9(8:19),'(F12.3)')R2
      ITITL9(20:27)=', BIC = '
      WRITE(ITITL9(28:39),'(F12.3)')BIC
      NCTIT9=39
C
C                             FORM SUBMATRIX
C
      IND(MP) = KZ
      DO 20 I=1,MP
        DO 10 J=I,MP
          ISUB1 = MD(I,J)
          ISUB2 = IND(I)
          ISUB3 = IND(J)
          XI(ISUB1) = RR(ISUB2,ISUB3)
  10    CONTINUE
  20  CONTINUE
C
C                            INVERT SUBMATRIX
C
      DO 30 N=1,M
        NN = N
        CALL PIVOT (XI,MP,NN,MD,ND,NL)
  30  CONTINUE
C
      ISUB4 = MD(MP,MP)
      VAR = FDIV (XI(ISUB4),FLOAT(NDEF-M),IF)
C
      DO 40 I=1,M
        ISUB5 = MD(I,MP)
        ZC(I) = -XI(ISUB5)
 40   CONTINUE
C
CCCCC NOTE: HAD PROBLEMS WITH RFORMT ON SOME PLATFORMS (MICROSOFT
CCCCC FORTRAN, SGI), SO JUST USE E FORMAT FOR NOW.
CCCCC CALL RFORMT (0,ISIGD,ZC,XI(1), M,NX,LW,LD,NALPHA(1),IRF)
CCCCC LB = NX - LW
C
      DO 50 I=1,M
        DBET = ZC(I)
        ISUB6 = MD(I,I)
CCCCC   CALL RFORMT (1,ISIGD,XI,ZC(I),LB, 1,LW,LD,NALPHA(1),IRF)
        F = -DBET*FDIV (DBET,XI(ISUB6)*VAR,IF)
CCCCC   WRITE(ICOUT,70) IND(I), (NALPHA(J),J=1,NX), F
CCCCC   WRITE(ICOUT,70) IVLIST(IND(I)), ZC(I), F
CCCCC   CALL DPWRST('XXX','BUG ')
        IVALUE(I,1)=IVLIST(IND(I))
        NCVALU(I,1)=8
        AMAT(I,2)=ZC(I)
        AMAT(I,3)=F
  50  CONTINUE
CC70  FORMAT (10X,I2,7X,15A1,5X,F7.3)
  70  FORMAT (4X,A8,7X,E15.7,5X,F7.3)
C
      WRITE(IOUNI1,71)M,R2,BIC,(IVLIST(IND(J)),J=1,M)
  71  FORMAT(I3,1X,2F15.3,' :',38(1X,A8))
C
      IOUT=' '
      DO80I=1,M
        IOUT(I:I)=ICOD(IND(I))
  80  CONTINUE
      WRITE(IOUNI2,'(38A1)')(IOUT(I:I),I=1,M)
 999  FORMAT(1X)
C
      RETURN
      END
*CPSTRE
      SUBROUTINE CPSTRE (RSS,CAB,KO,CL,RM,N,NS,ND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CPSTRE V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                  SAVES RSS:S AND LABELS FOR BEST REGRESSIONS
C ******************************************************************** *
C                                                                      *
C     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR     *
C                   REGRESSIONS BY LEAPS AND BOUNDS                    *
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
C                     G.M.FURNIVAL AND R.W.WILSON                      *
C               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
C                           VERSION 11/11/74                           *
C                                                                      *
C ******************************************************************** *
C
C               MODIFIED TO PFORT BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      REAL             CL(11,ND), RM(11,ND)
      REAL             CAB, RSS
C
      DATA IONE   /1/
      DATA IZERO  /0/
C
C     ==================================================================
C
      DO 10 L=1,KO
        IF (CAB.EQ.CL(L,N)) RETURN
  10  CONTINUE
C
      L = IZERO
  20  L = L + IONE
        IF (RSS.GT.RM(L+1,N)) GO TO 30
        RM(L,N) = RM(L+1,N)
        CL(L,N) = CL(L+1,N)
        IF (L.EQ.NS) GO TO 30
      GO TO 20
C
  30  RM(L,N) = RSS
      CL(L,N) = CAB
      RETURN
C
C     ==================================================================
C
      END
*CRSPRD
      SUBROUTINE CRSPRD (X,N,M,INTCPT,CTERM,CP,MAXC)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CRSPRD V 7.00  2/14/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT FOR COMPUTING A CROSS PRODUCT OF DEVIATIONS ABOUT
C        MEAN MATRIX, CP().
C
C        INPUT X(N,M)
C              N = NUMBER OF MEASUREMENTS
C              M = NUMBER OF VARIABLES.
C         INTCPT = 0, CROSS PRODUCTS ABOUT ORIGIN ARE COMPUTED
C                = 1, CROSS PRODUCTS ABOUT MEAN   ARE COMPUTED.
C
C        STORAGE CONST(M).
C
C        OUTPUT CP(M,M) = CROSS PRODUCT MATRIX.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      REAL             X(N,*)
CCCCC REAL             CP(29,29)
      REAL             CP(MAXC,MAXC)
      REAL             AVEX, XCODE
      REAL             FDPCON
C
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION CTERM(*)
      DOUBLE PRECISION F, SUMNEG, SUMPOS, SUMX
C
C     ==================================================================
C
      DATA IONE   /1/
      DATA DZERO  /0.0D0/
C
C     BEGIN COMPUTING.
C
C     COMPUTE CORRECTION TERM, CTERM(I), AND CODE X(I,J).
C
      IF (INTCPT.EQ.IONE) GO TO 20
      DO 10 I= 1,M
        CTERM(I) = DZERO
  10  CONTINUE
      GO TO 40
C
  20  DO 30 I=1,M
        CALL CODEXY (X(1,I),N,SUMX,AVEX,XCODE,CTERM(I),X(1,I),L)
  30  CONTINUE
C
C     COMPUTE (N-1)*VARIANCES.
C
  40  DO 60 I=1,M
        SUMPOS = DZERO
        SUMNEG = DZERO
        DO 50 J=1,N
          F = X(J,I)
          F = F**2
          SUMPOS = SUMPOS + DMAX1 (DZERO, F)
          SUMNEG = SUMNEG + DMAX1 (DZERO,-F)
  50    CONTINUE
        CP(I,I) = FDPCON ( (SUMPOS - SUMNEG) - CTERM(I)**2 )
  60  CONTINUE
C
C     COMPUTE CROSS PRODUCT MATRIX.
C
      IEND = M-IONE
      DO 90 I=1,IEND
        JBEG = I + IONE
        DO 80 J=JBEG,M
          SUMPOS = DZERO
          SUMNEG = DZERO
          DO 70 K=1,N
            F = DBLE(X(K,I))*DBLE(X(K,J))
            SUMPOS = SUMPOS + DMAX1 (DZERO, F)
            SUMNEG = SUMNEG + DMAX1 (DZERO,-F)
  70      CONTINUE
          CP(I,J) = FDPCON ( (SUMPOS - SUMNEG) - CTERM(I)*CTERM(J) )
          CP(J,I) = CP(I,J)
  80    CONTINUE
  90  CONTINUE
C
      RETURN
C
C     ==================================================================
C
      END
*FDDIV
      DOUBLE PRECISION FUNCTION FDDIV (FN,FD,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FDDIV V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION PERFORMS DOUBLE PRECISION DIVISION.
C
C     IF THE DENOMINATOR EQUALS ZERO, THE RESULT IS SET EQUAL TO ZERO
C        AND THE INDICATOR, IND, IS SET EQUAL TO ONE.  OTHERWISE
C           IND EQUALS ZERO.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION FN, FD
C
C     ==================================================================
C
      DATA IZERO  /0/
      DATA IONE   /1/
      DATA DZERO  /0.0D0/
C
      IND = IZERO
      IF(FD-DZERO.EQ.0.0D0)THEN
        FDDIV = DZERO
        IND = IONE
      ELSE
        FDDIV = FN/FD
      ENDIF
      RETURN
C
C     ==================================================================
C
      END
*FDIV
      REAL             FUNCTION FDIV (FN,FD,IND)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FDIV V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        DIVIDES FN BY FD USING FORTRAN OPERATOR /,
C           IF X IS NOT EQUAL TO ZERO, OR
C        SETS FAULT INDICATOR EQUAL TO ONE,
C           IF X IS EQUAL TO ZERO.
C
C     FAULT INDICATOR, IND = 0, IF FN IS NOT EQUAL TO ZERO, AND
C                          = 1, IF FN IS     EQUAL TO ZERO.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
C
      REAL             FN, FD
C
C     ==================================================================
C
      DATA IONE   /1/
      DATA IZERO  /0/
      DATA RZERO  /0.0/
C
      IND = IZERO
      IF (FD.EQ.RZERO) GO TO 10
      FDIV = FN / FD
      RETURN
C
C     ..................................................................
C
  10  FDIV = RZERO
      IND = IONE
      RETURN
C
C     ==================================================================
C
      END
*FDPCON
      REAL             FUNCTION FDPCON (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FDPCON V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     FUNCTION TO CONVERT DOUBLE PRECISION NUMBER TO REAL NUMBER BY
C        OCTAL ROUNDING INSTEAD OF TRUNCATION.
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -   AUGUST, 1969.
C                   CURRENT VERSION - FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      REAL             Y
C
      DOUBLE PRECISION X
      DOUBLE PRECISION XX, D
C
C     ==================================================================
C
      DATA RPIFY /1.0E38/
      DATA RMIFY /-1.0E37/
C
      XX = X
      IF (XX.GT.DBLE(RPIFY)) XX = RPIFY
      IF (XX.LT.DBLE(RMIFY)) XX = RMIFY
C
      Y = XX
      D = Y
      FDPCON = XX + (XX-D)
C
      RETURN
C
C     ==================================================================
C
      END
*FDSQRT
      DOUBLE PRECISION FUNCTION FDSQRT (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FDSQRT V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     THIS FUNCTION COMPUTES THE DOUBLE PRECISION SQUARE ROOT OF X.
C
C     IF THE ARGUMENT, X, IS LESS THAN ZERO, THE FUNCTION VALUE IS SET
C        EQUAL TO ZERO AND AN ARITHMETIC FAULT MESSAGE IS PRINTED.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION X, DSQRT
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA DZERO /0.0D0/
C
C     ==================================================================
C
CCCCC IF (X-DZERO) 20,30,10
      FDSQRT = DZERO
      IF (X-DZERO.LT.0.0D0)THEN
CCCCC    CALL ERROR (101)
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)
         CALL DPWRST('XXX','BUG ')
      ELSEIF (X-DZERO.GT.0.0D0)THEN
         FDSQRT = DSQRT (X)
      ENDIF
  101 FORMAT('***** ERROR FROM FDSQRT: ATTEMPT TO TAKE SQUARE ROOT OF ',
     1       'NEGATIVE NUMBER.')
C
      RETURN
C
C     ==================================================================
C
      END
*FLOG10
      REAL             FUNCTION FLOG10 (X)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FLOG10 V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     PROGRAM UNIT ...
C        COMPUTES LOG TO BASE 10 OF X USING LIBRARY FUNCTION LOG10,
C           IF X IS POSITIVE, OR
C        CALLS ERROR (101) AND SETS FUNCTION VALUE EQUAL TO ZERO,
C           IF X IS NONPOSITIVE.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      REAL             X
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA RZERO  /0.0/
C     ==================================================================
C
      IF (X.GT.RZERO) THEN
         FLOG10 = LOG10 (X)
      ELSE
CCCCC    CALL ERROR (101)
         WRITE(ICOUT,51)
   51    FORMAT('***** ERROR FROM FLOG10: ATTEMPT TO TAKE THE LOG OF ',
     1          'A NON-POSITIVE NUMBER')
         CALL DPWRST('XXX','BUG ')
         FLOG10 = RZERO
      ENDIF
C
C     ..................................................................
C
      RETURN
C
C     ==================================================================
C
      END
*PIVOT
      SUBROUTINE PIVOT (XI,KP,N,MD,ND,NL)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  PIVOT V 7.00  2/21/90. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C              SYMETRIC PIVOT-RETURNS NEGATIVE INVERSE
C     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR
C                   REGRESSIONS BY LEAPS AND BOUNDS
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS
C                     G.M.FURNIVAL AND R.W.WILSON 
C               YALE UNIVERSITY AND U.S. FOREST SERVICE
C                           VERSION 11/11/74
C
C               MODIFIED TO PFORT BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845 
C                  ORIGINAL VERSION - SEPTEMBER, 1976.
C                   CURRENT VERSION -  FEBRUARY, 1990.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION MD(ND,ND)
C
      REAL             XI(NL) 
      REAL             B
      REAL             FDIV
C
      DATA RONE /1.0/
C
C     ==================================================================
C
      ISUB1 = MD(N,N)
      XI(ISUB1) = FDIV (-RONE,XI(ISUB1),IND)
      DO 20 I=1,KP
        IF (I.EQ.N) GO TO 20
        ISUB2 = MD(I,N)
        ISUB3 = MD(N,N)
        B = XI(ISUB2) * XI(ISUB3)
        DO 10 J=I,KP
          ISUB4 = MD(I,J)
          ISUB5 = MD(J,N)
          IF (J.NE.N) XI(ISUB4) = XI(ISUB4) + B*XI(ISUB5)
  10    CONTINUE
        XI(ISUB2) = B
  20  CONTINUE
      RETURN
C
C     ==================================================================
C
      END 
*RFORMT
      SUBROUTINE RFORMT (KTYPE,KDIGIT,X,XVALUE,K1,K2,KW,KD,NALPHA,KE)
C
C **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. RFORMT V 7.00  2/19/91. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C                            *** DESCRIPTION ***
C
C     RFORMT IS A GENERAL-PURPOSE PORTABLE FORTRAN SUBROUTINE FOR USE IN
C        PRINTING REAL NUMBERS.
C
C     IT IS PRIMARILY INTENDED FOR PREPARING REAL NUMBERS TO BE PRINTED
C        IN READABLE FORM, I.E., WITH A CONSTANT NUMBER OF SIGNIFICANT
C        DIGITS AND THE DECIMAL POINT IN A CONSTANT POSITION.  THIS IS
C        IS CALLED R FORMAT.  IT CAN ALSO BE USED TO PRINT REAL NUMBERS
C        IN E, F, OR I FORMATS.
C
C     TO USE THE R FORMAT, IT IS NORMALLY NECESSARY TO USE RFORMT IN TWO
C        STAGES.  IN THE FIRST STAGE, WITH ITYPE = 0, NWIDTH AND NDECS
C        ARE CALCULATED.  IN THE SECOND STAGE, NWIDTH AND NDECS ARE USED
C        TO OBTAIN THE HOLLERITH CHARACTER STRING IN THE VECTOR NALPHA.
C
C     IN STAGE 2, REAL NUMBERS ARE CONVERTED INTO A HOLLERITH STRING AND
C        STORED IN THE VECTOR NALPHA FOR PRINTING WITH AN NA1 FORMAT.
C        THE HOLLERITH STRING IS PACKED ONE CHARACTER PER WORD.
C
C     ..................................................................
C
C                       *** STAGE 1 ARGUMENTS ***
C                       COMPUTE NWIDTH AND NDECS
C
C     INPUT ARGUMENTS -
C
C        (1)    ITYPE = 0
C        (2)   NDIGIT = NUMBER OF SIGNIFICANT DIGITS TO BE USED
C        (3)        X = VECTOR OF REAL NUMBERS DIMENSIONED AT LEAST N1
C                          IN CALLING PROGRAM UNIT
C        (4)   XVALUE = DUMMY ARGUMENT
C        (5)       N1 = LENGTH OF VECTOR X
C        (6)       N2 = MAXIMUM VALUE OF NWIDTH ALLOWED
C
C     OUTPUT ARGUMENTS -
C
C        (7)   NWIDTH = WIDTH OF FIELD NEEDED TO PRINT EVERY REAL NUMBER
C                          IN X IN R FORMAT
C        (8)    NDECS = NUMBER OF PLACES AFTER THE DECIMAL POINT NEEDED
C                          TO PRINT NUMBERS IN X IN R FORMAT
C        (9)   NALPHA = DUMMY ARRAY ARGUMENT, WHICH MUST BE
C                                 DIMENSIONED IN CALLING PROGRAM UNIT
C       (10)   IFAULT = FAULT INDICATOR,
C                     = 0, IF EVERYTHING IS OK
C                     = 1, IF ITYPE IS NEGATIVE
C                     = 2, IF VALUE OF NDIGIT INVALID
C                     = 3, IF N1 IS NON-POSITIVE
C                     = 4, IF N2 IS LESS THAN NDIGIT+2
C                     = 5, IF CALCULATED VALUE OF NWIDTH EXCEEDS N2.
C                             NWIDTH IS RESET TO N2.
C                     = 6, IF CALCULATED NWIDTH EXCEEDS N2 AND NDIGIT+5
C                             EXCEEDS N2
C
C     ..................................................................
C
C                         *** STAGE 2 ARGUMENTS ***
C                      PUT HOLLERITH STRING IN NALPHA
C
C     INPUT ARGUMENTS -
C
C        (1)    ITYPE = TYPE OF FORMAT DESIRED,
C                     =  1, R FORMAT, NUMBER ZERO HAS BLANKS AFTER DEC.
C                             POINT, 1PEW.(D-1) FORMAT USED IF NECESSARY
C                     =  2, R FORMAT, ZERO CONVERTED NORMALLY
C                             1PEW.(D-1) FORMAT USED IF NECESSARY
C                     =  3, R FORMAT, ZERO HAS BLANKS AFTER DEC. POINT,
C                             0PEW.D FORMAT USED IF NECESSARY
C                     =  4, R FORMAT, ZEROS CONVERTED NORMALLY
C                             0PEW.D JORMAT USED IF NECESSARY
C                     =  5, 1PEW.D FORMAT
C                     =  6, 0PEW.D FORMAT
C                     =  7, FW.D FORMAT, WITH ROUNDING
C                     =  8, FW.D FORMAT, WITH TRUNCATION
C                     =  9, IW FORMAT, WITH ROUNDING
C                     = 10, IW FORMAT, WITH TRUNCATION
C                     = 11, NWIDTH+N1 BLANKS STORED IN NALPHA
C        (2)   NDIGIT = NUMBER OF SIGNIFICANT DIGITS USED
C        (3)        X = DUMMY ARRAY ARGUMENT, WHICH MUST BE
C                           DIMENSIONED IN CALLING PROGRAM UNIT
C        (4)   XVALUE = REAL NUMBER TO BE CONVERTED
C        (5)       N1 = NUMBER OF BLANKS ADDED TO FIELD IN NALPHA
C        (6)       N2 = 0, NA BLANKS INSERTED ON LEFT (BEGINNING)
C                     = 1, N1 BLANKS ARE CENTERED
C        (7)   NWIDTH = LENGTH OF FIELD (HOLLERITH STRING) EXCLUDING N2
C                          BLANKS
C        (8)    NDECS = NUMBER OF PLACES AFTER THE DECIMAL POINT
C
C     OUTPUT ARGUMENTS -
C
C        (9)   NALPHA = HOLLERITH STRING REPRESENTATION OF XVALUE,
C                          OF LENGTH NWIDTH+N1
C       (10)   IFAULT = FAULT INDICATOR,
C                     =  0, IF EVERYTHING IS OK
C                     =  1, IF VALUE OF ITYPE IS NOT VALID
C                     =  2, IF VALUE OF NDIGIT IS NOT VALID
C                     =  3, IF N1 IS NON-POSITIVE
C                     =  7, IF VALUE OF N2 IS NOT ZERO OR ONE
C                     =  8, IF VALUE OF NWIDTH IS NOT VALID
C                     =  9, IF VALUE OF NDECS IS NOT VALID
C                     = 10, IF OVERFLOW OCCURS WITH F OR I FORMATS
C                     = 11, IF R FORMAT FORCED INTO E FORMAT
C                     = 12, IF R FORMAT REQUIRES E FORMAT AND
C                              NWIDTH IS TOO SMALL
C                     = 13, IF R FORMAT REQUIRES E FORMAT AND
C                              NDECS IS TOO SMALL
C                     = 14, IF ITYPE EQUALS 9 OR 10 AND NDECS DOES NOT
C                              EQUAL ZERO. ZERO IS USED FOR IDECS.
C
C     ..................................................................
C
C                           *** NOTES ***
C
C      1.   CAUTION.  IN STAGE 1 ITYPE MUST EQUAL ZERO OR RFORMT WILL
C              EXECUTE STAGE 2.
C      2.   IFAULT = 5, 10, 11 OR 14, INDICATES INFORMATIVE DIAGNOSTIC.
C              OTHERWISE NON-ZERO VALUES OF IFAULT INDICATE FATAL ERRORS
C              AND EXIT OCCURS WITHOUT ANY FURTHER CALCULATIONS OR ERROR
C              CHECKING.
C      3.   NDIGIT MUST BE GREATER THAN ZERO AND LESS THAN OR EQUAL TO
C              NSIGD.  SEE SECTION ON PORTABILITY BELOW FOR DEFINITION
C              OF NSIGD.
C      4.   X AND NALPHA MUST BE DIMENSIONED IN CALLING PROGRAM UNIT.
C      5.   RFORMT HANDLES REAL NUMBERS BETWEEN 10**(-100) AND 10**100,
C              EXCLUSIVELY.
C      6.   WHEN N2 = 1 IN STAGE 2, LARGEST NUMBER OF BLANKS IS ON RIGHT
C              IF N1 IS ODD.
C      7.   IN STAGE 1, NWIDTH INCLUDES POSITION FOR SIGN, EVEN
C              IF ALL NUMBERS ARE POSITIVE.  HOWEVER THERE ARE TWO
C              SPECIAL CASES ...
C                 (A) WHEN ALL X(I) = 0, IN WHICH CASE NWIDTH = 2
C                        AND NDECS = 0.
C                 (B) WHEN ALL X(I) ARE LESS THAN ONE IN ABSOLUTE VALUE
C                        AND AT LEAST ONE X(I) EQUALS ZERO. A POSITION
C                        FOR THE SIGN OF ZERO IS NOT INCLUDED IN NWIDTH.
C
C      8.   WITH R FORMAT, A DECIMAL POINT IS NOT STORED IN NALPHA IF
C              THE REAL NUMBER XVALUE EXCEEDS 10**NDIGIT.  IF NDIGIT=3,
C              1.23+03 IS STORED AS 1230 RATHER THAN 1230., TO EMPHASIZE
C              THAT THE ZERO IS NOT A SIGNIFICANT DIGIT.
C      9.   RFORMT DOES NO PRINTING.  PRINTING OF NALPHA WITH NA1 FORMAT
C              MUST BE DONE BY THE CALLING PROGRAM UNIT.
C     10.   WHEN ZERO IS PRINTED WITH R FORMAT, NDECS OVERRIDES NDIGIT.
C     11.   CAUTION.  IF IFAULT IS NOT EQUAL TO ZERO, NALPHA MAY NOT BE
C              BLANKED OUT.
C     12.   NALPHA IS UNCHANGED, IF ITYPE EQUALS ZERO.
C
C     ..................................................................
C
C                     *** USE OF E, F, AND I FORMATS ***
C
C     1.   1PEW.D FORMAT IS OBTAINED BY SETTING -
C              ITYPE =   5
C             NWIDTH =   W   = WIDTH OF FIELD
C             NDIGIT = (D+1) = NUMBER OF DIGITS
C
C          WITH D=6, 12.345678 IS WRITTEN AS 1.234568+01
C
C     2.   0PEW.D FORMAT IS OBTAINED BY SETTING -
C              ITYPE = 6
C             NWIDTH = W = WIDTH OF FIELD
C             NDIGIT = D = NUMBER OF DIGITS
C
C          WITH D=7, 12.345678 IS WRITTEN AS .1234568+02
C
C     3.   FW.D FORMAT IS OBTAINED BY SETTING -
C              ITYPE = 7 OR 8
C             NWIDTH = W = WIDTH OF FIELD
C              NDECS = D = NUMBER OF PLACES AFTER DECIMAL POINT
C
C     4.   IW FORMAT IS OBTAINED BY SETTING -
C              ITYPE = 9 OR 10
C             NWIDTH = W = WIDTH OF FIELD
C              NDECS = 0
C
C     NOTES -
C        A.   FOR E FORMAT, NDECS MUST BE GREATER THAN OR EQUAL TO ZERO.
C                NSIGDS=NDECS IS SET EQUAL TO NDIGIT+2 BY RFORMT.
C        B.   WITH EW.D FORMAT, THE LETTER E IS NOT USED AFTER THE
C                NUMBER AND BEFORE THE SIGNED CHARACTERISTIC.
C        C.   WITH 0PEW.D FORMAT, ZERO IS NOT PUT BEFORE THE DECIMAL
C                POINT.
C        D.   WITH FW.D FORMAT AND THE ABSOLUTE VALUE OF NUMBER IS LESS
C                THAN ONE, ZERO IS NOT PUT ON LEFT OF DECIMAL POINT,
C                UNLESS D = 0.
C
C     ..................................................................
C
C                            *** PORTABILITY ***
C
C     RFORMT IS COMPLETELY PORTABLE EXCEPT FOR ONE MACHINE DEPENDENT
C        CONSTANT, NSIGD, SET IN THE DATA STATEMENT ON LINE RF 320.
C
C     NSIGD IS THE NUMBER OF SIGNIFICANT DECIMAL DIGITS IN THE COMPUTER.
C        NSIGD =  7, FOR A 32 BIT WORD COMPUTER (IBM)
C              =  8, FOR A 36 BIT WORD COMPUTER (UNIVAC), VALUE SET
C              = 10, FOR A 48 BIT WORD COMPUTER (BURROUGHS)
C              = 13, FOR A 60 BIT WORD COMPUTER (CDC).
C
C     CAUTION.  NSIGD MUST BE SMALL ENOUGH SO THAT 10**(NSIGD+1) IS A
C        VALID MACHINE INTEGER.  (THIS EXPLAINS WHY NSIGD EQUALS 13 AND
C        NOT 14 FOR A 60 BIT WORD COMPUTER.)
C
C     SOURCE LANGUAGE IS PFORT (A PORTABLE SUBSET OF ANS FORTRAN).
C
C     FORTRAN LIBRARY FUNCTION USED IS LOG10,
C        WHICH APPEARS ON LINES RF 389, RF 391, AND RF 612.
C
C     STORAGE USED IS 1495 36 BIT WORDS WITH UNIVAC 1108 EXEC 8 COMPUTER
C
C     ..................................................................
C
C                           *** STATIC PROFILE ***
C
C     I/O STATEMENTS                 0
C     NONEXECUTABLE STATEMENTS      20
C     EXECUTABLE STATEMENTS        244
C        UNCONDITIONAL 160
C          CONDITIONAL  84
C     COMMENT STATEMENTS           532
C     --------------------------------
C     TOTAL NUMBER OF STATEMENTS   796
C     --------------------------------
C     CONTINUATION LINES             6
C     --------------------------------
C     NUMBER OF LINES OF CODE      802
C
C     ..................................................................
C
C                             *** REFERENCE ***
C
C     HOGBEN, DAVID (1977).  A FLEXIBLE PORTABLE FORTRAN PROGRAM UNIT
C        FOR READABLE PRINTING OF REAL NUMBERS.  IN PREPARATION.
C
C     ..................................................................
C
C               WRITTEN BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-975-2845
C                  ORIGINAL VERSION -    APRIL, 1969.
C                   CURRENT VERSION - FEBRUARY, 1991.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      DIMENSION NALPHA(*)
C
C     ==================================================================
C
C                    ***   TYPE STATEMENTS   ***
C
      REAL             X(*)
      REAL             XVALUE
      REAL             ABSMAX, ABSMIN, ABSX, ABSXVA, X1, X2
      REAL             FLOG10
C
C......................................................................
C
      DOUBLE PRECISION Z, ZLOWER, ZUPPER
      DOUBLE PRECISION DFIVE, DTEN
      DOUBLE PRECISION FDDIV
C
C     ..................................................................
C
      CHARACTER*1 LA(74)
      CHARACTER NALPHA*1
C
CCCCC INCLUDE 'DPCOHO.INC'
C
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DFIVE, DTEN / 5.0D0, 10.0D0 /
C
      DATA ITEN   /10/
      DATA IFIVE  /5/
      DATA IFOUR  /4/
      DATA ITHRE  /3/
      DATA ITWO   /2/
      DATA IONE   /1/
      DATA IZERO  /0/
C
      DATA RHALF   /0.5/
      DATA RONE    /1.0/
      DATA RZERO   /0.0/
C
CCCCC DATA ISIGD /7/
C
C   LA( 1) =  0  LA( 2) =  1  LA( 3) =  2  LA( 4) =  3  LA( 5) =  4
C   LA( 6) =  5  LA( 7) =  6  LA( 8) =  7  LA( 9) =  8  LA(10) =  9
C   LA(11) =  A  LA(12) =  B  LA(13) =  C  LA(14) =  D  LA(15) =  E
C   LA(16) =  F  LA(17) =  G  LA(18) =  H  LA(19) =  I  LA(20) =  J
C   LA(21) =  K  LA(22) =  L  LA(23) =  M  LA(24) =  N  LA(25) =  O
C   LA(26) =  P  LA(27) =  Q  LA(28) =  R  LA(29) =  S  LA(30) =  T
C   LA(31) =  U  LA(32) =  V  LA(33) =  W  LA(34) =  X  LA(35) =  Y
C   LA(36) =  Z  LA(37) =  /  LA(38) =  .  LA(39) =  -  LA(40) =  +
C   LA(41) =  *  LA(42) =  (  LA(43) =  )  LA(44) =  ,  LA(45) =  
C   LA(46) =  =  LA(47) =  $  LA(48) =  '  LA(49) =  a  LA(50) =  b
C   LA(51) =  c  LA(52) =  d  LA(53) =  e  LA(54) =  f  LA(55) =  g
C   LA(56) =  h  LA(57) =  i  LA(58) =  j  LA(59) =  k  LA(60) =  l
C   LA(61) =  m  LA(62) =  n  LA(63) =  o  LA(64) =  p  LA(65) =  q
C   LA(66) =  r  LA(67) =  s  LA(68) =  t  LA(69) =  u  LA(70) =  v
C   LA(71) =  w  LA(72) =  x  LA(73) =  y  LA(74) =  z
C
      DATA LA( 1), LA( 2), LA( 3), LA( 4), LA( 5),
     1     LA( 6), LA( 7), LA( 8), LA( 9), LA(10)/
     2        '0',    '1',    '2',    '3',    '4',
     3        '5',    '6',    '7',    '8',    '9'/
C
      DATA LA(11), LA(12), LA(13), LA(14), LA(15),
     1     LA(16), LA(17), LA(18), LA(19), LA(20)/
     2        'A',    'B',    'C',    'D',    'E',
     3        'F',    'G',    'H',    'I',    'J'/
C
      DATA LA(21), LA(22), LA(23), LA(24), LA(25),
     1     LA(26), LA(27), LA(28), LA(29), LA(30)/
     2        'K',    'L',    'M',    'N',    'O',
     3        'P',    'Q',    'R',    'S',    'T'/
C
      DATA LA(31), LA(32), LA(33), LA(34), LA(35),
     1     LA(36), LA(37), LA(38), LA(39), LA(40)/
     2        'U',    'V',    'W',    'X',    'Y',
     3        'Z',    '/',    '.',    '-',    '+'/
C
      DATA LA(41), LA(42), LA(43), LA(44), LA(45),
     1     LA(46), LA(47), LA(48), LA(49), LA(50)/
     2        '*',    '(',    ')',    ',',    ' ',
     3        '=',    '$',   '''',    'a',    'b'/
C
      DATA LA(51), LA(52), LA(53), LA(54), LA(55),
     1     LA(56), LA(57), LA(58), LA(59), LA(60)/
     2        'c',    'd',    'e',    'f',    'g',
     3        'h',    'i',    'j',    'k',    'l'/
C
      DATA LA(61), LA(62), LA(63), LA(64), LA(65),
     1     LA(66), LA(67), LA(68), LA(69), LA(70)/
     2        'm',    'n',    'o',    'p',    'q',
     3        'r',    's',    't',    'u',    'v'/
C
      DATA LA(71), LA(72), LA(73), LA(74)/
     2        'w',    'x',    'y',    'z'/
C
C     ==================================================================
C
CCCCC ISIGD NEEDS TO BE 6 ON MICROSOFT/COMPAQ PC COMPILER.
CCCCC ALSO NEDS TO BE 6 ON SGI.
CCCCC TO BE SAFE, JUST SET TO 6, WHICH SHOULD WORK ON ALL 32-BIT
CCCCC HOSTS.
C
      ISIGD = 6
CCCCC IF(ICOMPI.EQ.'MS-F')ISIGD = 6
CCCCC IF(ICOMPI.EQ.'LAHE')ISIGD = 6
C
C     ADAPTIONS FOR OMNITAB.
C
C     NW IS USED INSTEAD OF NWIDTH
C     ND IS USED INSTEAD OF NDECS
C     IE IS USED INSTEAD OF IFAULT
C
      ITYPE  = KTYPE
      NDIGIT = KDIGIT
          N1 = K1
          N2 = K2
          NW = KW
          ND = KD
          IE = KE
C
C     GENERAL ERROR CHECKING.
C
      ZLOWER = ITEN ** NDIGIT
      ZUPPER = DTEN * ZLOWER
      IE = IZERO
      IF (ITYPE.GE.IZERO) GO TO 10
        IE = IONE
        GO TO 390
C
C     ..................................................................
C
  10  IF (NDIGIT.GT.IZERO .AND. NDIGIT.LE.ISIGD) GO TO 20
        IE = ITWO
        GO TO 390
C
C     ..................................................................
C
  20  IF (ITYPE.GT.IZERO) GO TO 80
C
C     ==================================================================
C
C                           *** STAGE 1 ***
C                       COMPUTE NWIDTH AND NDECS
C
C     STAGE 1 ERROR CHECKING
C
      IF (N1.GT.IZERO) GO TO 30
        IE = ITHRE
        GO TO 390
C
C     ..................................................................
C
C     N2 MUST BE LARGE ENOUGH FOR NDIGIT, DECIMAL POINT, AND SIGN.
C
  30  IF (N2.GE.NDIGIT+ITWO) GO TO 40
        IE = IFOUR
        GO TO 390
C
C     ..................................................................
C
C     (1)   COMPUTE MMIN, CHARACTERISTIC OF ABSMIN = MIN ABS VALUE X(I)
C             AND COMPUTE MMAX, CHARACTERISTIC OF ABSMAX = MAX ABS X(I).
C
  40  ABSX = ABS (X(1))
      IF (ABSX.LE.RZERO) ABSX = RONE
      ABSMIN = ABSX
      ABSMAX = ABSX
C
      K = IZERO
C
C     K IS USED IN TWO SPECIAL CASES ... WHEN
C        (A)  ALL X(I) EQUAL ZERO, AND
C        (B)  ABS (X(I)) IS LESS THAN 1.0, FOR ALL I, AND SOME X(I)=0.0.
C
      DO 50 I=1,N1
        ABSX = ABS (X(I))
        IF (ABSX.GE.RONE) K = IONE
        IF (ABSX.LE.RZERO) ABSX = RONE
        IF (ABSX.LT.ABSMIN) ABSMIN = ABSX
        IF (ABSX.GT.ABSMAX) ABSMAX = ABSX
  50  CONTINUE
C
      MMIN = FLOG10 (ABSMIN)
      IF (ABSMIN.LT.RONE) MMIN = MMIN - IONE
      MMAX = FLOG10 (ABSMAX)
      IF (ABSMAX.LT.RONE) MMAX = MMAX - IONE
C
C     ADJUST FOR POSSIBLE INCORRECT VALUES OF MMIN AND MMAX DUE TO
C        ERROR IN LOG10 CALCULATION.
C
      Z = ABSMIN
      Z = Z * DTEN ** (NDIGIT-MMIN) + DFIVE
C
      IF (Z.LT.ZLOWER) MMIN = MMIN - IONE
      IF (Z.GE.ZUPPER) MMIN = MMIN + IONE
C
      Z = ABSMAX
      Z = Z * DTEN ** (NDIGIT-MMAX) + DFIVE
C
      IF (Z.LT.ZLOWER) MMAX = MMAX - IONE
      IF (Z.GE.ZUPPER) MMAX = MMAX + IONE
C
C     ..................................................................
C
C     (2)   USE MMIN AND MMAX TO COMPUTE NWIDTH AND NDECS.
C
      ND = NDIGIT - MMIN - IONE
      ND = MAX0 (IZERO,ND)
      NW = MMAX + ITHRE + ND
      IF (MMAX.LT.IZERO) NW = ND + ITWO
      IF (K.EQ.IONE) GO TO 60
C
C     ADJUST FOR SPECIAL CASE (B) DESCRIBED ON LINE RF 368
C
      IF (ABSMIN.LT.RONE .AND. ABSMAX.GE.RONE) NW = NW - IONE
C
C     ADJUST FOR SPECIAL CASE (A) DESCRIBED ON LINE RF 367
C
      IF (ABSMIN.LT.RONE .OR. ABSMAX.LT.RONE) GO TO 60
      NW = ITWO
      ND  = IZERO
C
  60  IF (NW.LE.N2) GO TO 390
C
C     NWIDTH IS TOO LARGE AND HAS TO BE ADJUSTED.
C
        IE = IFIVE
      IF (NDIGIT+IFIVE.LE.N2) GO TO 70
        IE = 6
        GO TO 390
C
C     ..................................................................
C
C
C     NDIGIT+2 = (NDIGIT-1) + (+XX), FOR EXPONENT OF FLOATING-POINT NO.
C
  70  ND = MAX0 (ND,NDIGIT+ITWO)
C
C     N2-3 = N2 - (SIGN+DIGIT+DECIMAL POINT).
C
      ND = MIN0 (ND,N2-ITHRE)
      NW = N2
      GO TO 390
C
C     ==================================================================
C
C                          ***** STAGE 2 *****
C                     PUT HOLLERITH STRING IN NALPHA
C
  80  ABSXVA = ABS (XVALUE)
C
C     STAGE 2 ERROR CHECKING
C
      IF (ITYPE.LT.12) GO TO 90
        IE = IONE
        GO TO 390
C
C     ..................................................................
C
  90  IF (N1.GE.IZERO) GO TO 100
        IE = ITHRE
        GO TO 390
C
C     ..................................................................
C
 100  IF (N2.EQ.IZERO .OR. N2.EQ.IONE) GO TO 110
        IE = 7
        GO TO 390
C
C     ..................................................................
C
 110  IF (ITYPE.LT.9 .AND. NW.LT.ND+ITWO) GO TO 120
      IF (NW.LE.IZERO) GO TO 120
      IF (ITYPE.GT.6) GO TO 130
      IF (ABSXVA.LE.RZERO .AND. NW.GE.ITWO .AND. ITYPE.LE.IFOUR)
     1     GO TO 130
C
C     CHECK WHETHER NWIDTH IS VALID.
C
      IF (NW.LT.NDIGIT+ITWO) GO TO 120
      IF (ITYPE.LT.IFIVE) GO TO 130
      IF (NW.GE.NDIGIT+IFIVE) GO TO 130
 120    IE = 8
        GO TO 390
C
C     ..................................................................
C
 130  IF (ND.GE.IZERO) GO TO 140
        IE = 9
        GO TO 390
C
C     ..................................................................
C
C         VARIABLES USED TO DEFINE FIELD WIDTH FOR R FORMAT
C
C                     -----------------------------
C                     I        NWIDTH             I
C          ----------------------------------------------
C          I  NBLANK  I     NDIFF     I   NDECS   I     I
C          ----------------------------------------------
C          I       NPONE              I
C          ----------------------------------------
C          I             LTOTAL                   I
C          ----------------------------------------------
C          I        NTOTAL = NWIDTH + N1                I
C          ----------------------------------------------
C
C     ..................................................................
C
C     (1)   INITIALIZATION.
C
C     CLEAR OUT NALPHA WITH BLANKS.
C
 140  NTOTAL = NW + N1
      DO 150 I=1,NTOTAL
        NALPHA(I) = LA(45)
 150  CONTINUE
C
      IF (ITYPE.EQ.11) GO TO 390
C
C     IF NECESSARY, CENTER BLANKS WITH LARGEST NUMBER ON RIGHT IF N1 ODD
C
      CALL IDIV (N1+IONE,ITWO,IND,NJUNK)
      NBLANK = N1 - NJUNK * N2
C
      MF    = IZERO
      MREAL = IZERO
      IDECS = ND
      IF (ITYPE.LT.9 .OR. IDECS.EQ.IZERO) GO TO 160
      IDECS = IZERO
      IE    = 14
 160  IF (ITYPE.EQ.IFIVE .OR. ITYPE.EQ.6) IDECS = NDIGIT + ITWO
C
C     THE NEXT THREE STATEMENTS ARE USED TO SWITCH FROM F TO I FORMAT
C
      NSIGDS = NDIGIT
      IWIDTH = NW
      IF (ITYPE.EQ.9 .OR. ITYPE.EQ.ITEN) IWIDTH = IWIDTH + IONE
      NDIFF = IWIDTH - IDECS
      LTOTAL = IWIDTH + NBLANK
      NPONE = NDIFF + NBLANK
C
      IF (ABSXVA.GE.RONE) GO TO 200
      IF (ITYPE.LT.9 .AND. ABSXVA.GT.RZERO) GO TO 200
C
C     ..................................................................
C
C     (2)   XVALUE = 0. IS SPECIAL CASE.
C
      IF (ITYPE.LT.9) GO TO 180
C
C     INTEGER FORMAT
C
      IF (ABSXVA.LE.RHALF .OR. ITYPE.EQ.ITEN) GO TO 170
      NALPHA(LTOTAL-1) = LA(2)
        IF (XVALUE.LT.RZERO) NALPHA(LTOTAL-2) = LA(39)
      GO TO 390
C
C     ..................................................................
C
 170  NALPHA(LTOTAL-1) = LA(1)
      GO TO 390
C
C     ..................................................................
C
C     R FORMAT WITH ZERO STORED AS 0.
C
 180  NALPHA(NPONE  ) = LA(38)
      NALPHA(NPONE-1) = LA(1)
      IF (ITYPE.EQ.IONE .OR. ITYPE.EQ.ITHRE) GO TO 390
      IF (ITYPE.EQ.ITWO .AND. IDECS.EQ.IZERO) GO TO 390
      IF (ITYPE.EQ.IFOUR .AND. IDECS.EQ.IZERO) GO TO 390
C
C     FIXED 0
C
      IF (ITYPE.EQ.7 .AND. ND.EQ.IZERO) GO TO 390
      IF (ITYPE.EQ.8 .AND. ND.EQ.IZERO) GO TO 390
C
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.8) NALPHA(NPONE-1) = LA(45)
C
C     ALL OTHER CASES
C
      IBEG = NPONE + IONE
      IEND = NPONE + IDECS
      DO 190 I=IBEG,IEND
        NALPHA(I) = LA(1)
 190  CONTINUE
C
C     ..................................................................
C
      IF (ITYPE.NE.IFIVE .AND. ITYPE.NE.6) GO TO 390
C
C     FLOATING
C
      NALPHA(LTOTAL-2) = LA(40)
      IF (ITYPE.EQ.IFIVE) GO TO 390
      NALPHA(NPONE  ) = LA(1)
      NALPHA(NPONE-1) = LA(38)
      GO TO 390
C
C     ..................................................................
C
C     (3)   COMPUTE M = CHARACTERISTIC OF ABSXVA = ABS(XVALUE) AND
C                  LL = (NSIGDS+1) INTEGER REPRESENTATION OF ABSXVA.
C              FOR XVALUE = -12.345678, M=1 AND LL=123456784, AN
C              ADDITIONAL DIGIT IN LL IS USED TO AVOID ROUNDOFF ERROR.
C
 200  M = FLOG10 (ABSXVA)
      IF (ABSXVA.LT.RONE) M = M - IONE
      Z = ABSXVA
      Z = Z * DTEN**(NSIGDS-M)
C
C     IF M IS COMPUTED ACCURATELY, ZLOWER .LE. Z .LT. ZUPPER
C
      IF (Z.GE.ZLOWER) GO TO 210
C
C     Z IS LESS THAN ZLOWER BECAUSE M IS ONE TOO LARGE.
C       ADJUST BY SUBTRACTING 1 FROM M AND MULTIPLYING Z BY 10.
C
      M = M - IONE
      Z = DTEN * Z
      GO TO 220
C
 210  IF (Z.LT.ZUPPER) GO TO 220
C
C     Z IS GREATER THAN OR EQUAL TO ZUPPER BECAUSE M IS ONE TOO SMALL.
C       ADJUST BY ADDING 1 TO M AND DIVIDING Z BY 10.
C
      M = M + IONE
      Z = FDDIV (Z,DTEN,IND)
C
 220  X1 = Z
      LL1 = X1
      X2 = Z - DBLE (X1)
      LL2 = X2
      LL = LL1 + LL2 + IFIVE
      IF (LL.LT.ITEN**(NSIGDS+IONE)) GO TO 230
C
C     MAKE ADJUSTMENT WHEN LL IS TOO LARGE.
C
      M = M + IONE
      CALL IDIV (LL,ITEN,IND,LL)
      GO TO 240
 230  IF (LL.GE.ITEN**NSIGDS) GO TO 240
C
C     MAKE ADJUSTMENT WHEN LL IS TOO SMALL.
C
      M = M - IONE
      LL = ITEN * LL
 240  IF (ITYPE.EQ.8 .OR. ITYPE.EQ.ITEN) LL = LL - IFIVE
      IF (ITYPE.LT.IFIVE) GO TO 290
      IF (ITYPE.EQ.IFIVE .OR. ITYPE.EQ.6) GO TO 300
C
C     ..................................................................
C
C     (4)   FIXED AND INTEGER.
C
C     CHECK FOR OVERFLOW.
C
      IF (M.GT.NDIFF-ITWO) GO TO 270
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.LT.RZERO) GO TO 270
C
C     ADJUST NUMBER OF DIGITS (NSIGDS) AND LL.
C
      NSIGDS = MIN0 (NDIGIT,IDECS+M+IONE)
      NSIGDS = MAX0 (IZERO,NSIGDS)
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.9) LL = LL - IFIVE
      CALL IDIV (LL,ITEN**(NDIGIT-NSIGDS),IND,LLTEMP)
      LTEMP=LL
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.9) LL = LL + IFIVE
      IF (LL.LT.ITEN**(NSIGDS+IONE)) GO TO 250
C
C     ADJUST FOR XVALUE ROUNDED TO ONE MORE DIGIT.
C
      M = M + IONE
      NSIGDS = MIN0 (NDIGIT,IDECS+M+IONE)
      NSIGDS = MAX0 (IZERO,NSIGDS)
C
C     CHECK FOR OVERFLOW CAUSED BY ROUNDING TO ONE MORE DIGIT.
C
      IF (M.GT.NDIFF-ITWO) GO TO 270
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.LT.RZERO) GO TO 270
C
C     CHECK FOR UNDERFLOW.
C
 250  IF (NSIGDS.GT.IZERO) GO TO 310
C
C     ADJUST FOR UNDERFLOW.  XVALUE ROUNDED TO IDECS EQUALS ZERO.
C
      IF (IDECS.EQ.IZERO) NALPHA(NPONE-1) = LA(1)
C
      DO 260 I=NPONE,LTOTAL
        NALPHA(I) = LA(1)
 260  CONTINUE
C
      NALPHA(NPONE) = LA(38)
      GO TO 390
C
C     ..................................................................
C
C     PUT IN ASTERISKS WHEN OVERFLOW OCCURS.
C
 270  IE = ITEN
      DO 280 I=1,NW
        ISUBSC = I + NBLANK
        NALPHA(ISUBSC) = LA(41)
 280  CONTINUE
      GO TO 390
C
C     ..................................................................
C
C     (5)   CHECK WHETHER R FORMAT IS FORCED INTO E FORMAT.
C
 290  IF (M.GE.NSIGDS-IONE-IDECS .AND. M.LT.NDIFF-ITWO) GO TO 310
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.GT.RZERO) GO TO 310
        IE = 11
      IF (NW.GE.NDIGIT+IFIVE .AND. ND.GE.NDIGIT+ITWO) GO TO 300
        IE = 13
      IF (NW.GE.NDIGIT+IFIVE) GO TO 390
        IE = 12
        GO TO 390
C
C     ..................................................................
C
C     (6)   FLOATING.
C
 300  MREAL = M
      M = IZERO
      MF = IONE
C
C     ..................................................................
C
C     (7)   STORE REPRESENTATION IN NALPHA.
C
 310  IF (M.LT.NSIGDS .AND. ITYPE.LT.9) NALPHA(NPONE) = LA(38)
      NINT = NPONE - IONE - M
      IF (M.LT.IZERO) NINT = NINT + IONE
      NEND = NINT + NSIGDS - IONE
      IF (M.GE.IZERO .AND. M.LT.NSIGDS-IONE) NEND = NEND + IONE
      DO 320 J=NINT,NEND
        I = NEND + NINT - J
        IF (I.EQ.NPONE) GO TO 320
        CALL IDIV (LL,ITEN,IND,LLTEMP)
        LL = LTEMP
        NN = MOD (LL,ITEN)
        NALPHA(I) = LA(NN+1)
 320  CONTINUE
C
      IF (MF.EQ.IZERO) GO TO 340
C
C     ..................................................................
C
C     (8)   PUT IN EXPONENT FOR FLOATING POINT NUMBER.
C
      IF (ITYPE.EQ.IONE .OR. ITYPE.EQ.ITWO .OR. ITYPE.EQ.IFIVE) GOTO 330
C
C     CHANGE FROM 1PE TO 0PE
C
      NALPHA(NINT+1) = NALPHA(NINT)
      NALPHA(NINT  ) = LA(38)
      MREAL = MREAL + IONE
C
 330  IF (MREAL.LT.IZERO) NALPHA(NEND+1) = LA(39)
      IF (MREAL.GE.IZERO) NALPHA(NEND+1) = LA(40)
      MREALA = IABS(MREAL)
      CALL IDIV (MREALA,ITEN,IND,M1)
      M2 = MOD (MREALA,ITEN)
      NALPHA(NEND+2) = LA(M1+1)
      NALPHA(NEND+3) = LA(M2+1)
C
C     ..................................................................
C
C     (9)   PUT IN MINUS SIGN IF XVALUE LESS THAN ZERO.
C
 340  IF (XVALUE.GE.RZERO) GO TO 350
        IF (M.GE.IZERO) NALPHA(NINT-1) = LA(39)
        IF (M.LT.IZERO) NALPHA(NPONE-1) = LA(39)
 350  IF (M.GE.(-IONE)) GO TO 370
C
C     PUT ZEROS AFTER DECIMAL POINT FOR ABSXVA LESS THAN 0.1
C
      IBEG = NPONE + IONE
      IEND = NINT - IONE
      DO 360 I=IBEG,IEND
        NALPHA(I) = LA(1)
 360  CONTINUE
      GO TO 390
C
C     ..................................................................
C
C     (10)   PUT IN NON-SIGNIFICANT ZEROS FOR LARGE INTEGERS.
C
 370  IF (M.LT.NSIGDS .OR. MF.NE.IZERO) GO TO 390
      IBEG = NINT + NSIGDS
      IEND = NPONE - IONE
      DO 380 I=IBEG,IEND
        NALPHA(I) = LA(1)
 380  CONTINUE
C
C     ..................................................................
C
 390  KW = NW
      KD = ND
      KE = IE
      IF (IE.EQ.IZERO .OR. IE.EQ.IFIVE .OR. IE.EQ.6 .OR. IE.EQ.ITEN
     1                .OR. IE.EQ.11    .OR. IE.GE.14) RETURN
CCCCC   CALL ERROR (259)
        RETURN
C
C     ==================================================================
C
      END
*SCREEN
      SUBROUTINE SCREEN(RR,KX,NR,NDEF,IBIT,MBST,INTCPT,A,NS,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  IBUGA3,ISUBRO,IERROR)
C
C **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SCREEN V 7.00  4/21/92. **
C
C     ==================================================================
C
C                        ***   GENERAL COMMENTS   ***
C
C     **************************************************************** *
C                                                                      *
C                   REGRESSIONS BY LEAPS AND BOUNDS                    *
C          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
C                     G.M.FURNIVAL AND R.W.WILSON                      *
C               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
C                           VERSION 11/11/74                           *
C                                                                      *
C                 CALL SCREEN(RR,KX,NR,NDEF,IBIT,MBST)                 *
C                                                                      *
C     RR   = UPPER TRIANGULAR PORTION OF (KX+1)*(KX+1) CORRELATION OR  *
C            PRODUCT MATRIX. VARIABLE KX+1 IS THE DEPENDENT VARIABLE.  *
C     KX   = NUMBER OF INDEPENDENT VARIABLES (3.LE.KX.LE.28)           *
C     NR   = DIMENSION OF RR (NR.GT.KX)                                *
C     NDEF = DEGREES OF FREEDOM FOR RR (NDEF.GT.KX)                    *
C     IBIT = SELECTION CRITERION CODE (1=R**2,2=ADJUSTED R**2,3=CP)    *
C     MBST = NUMBER OF BEST REGRESSIONS DESIRED (1.LE.MBST.LE.10)      *
C                                                                      *
C       MBST BEST REGRESSIONS FOR EACH SIZE SUBSET WHEN IBIT.EQ.1      *
C             MBST BEST REGRESSIONS IN TOTAL WHEN IBIT.GT.1            *
C                                                                      *
C     **************************************************************** *
C
C     ARRAY STORAGE REQUIRED FOR K=KX INDPENDENT VARIABLES AND M = K+1.
C         2*NL FOR XI AND XN, WHERE NL = M(M+1)(M+2)/6
C        4M**2 FOR ILI, ILM, MD AND NC
C      2*(11M) FOR CL AND RM
C          12M FOR CI, CN, CO, ID, IPI, IPN, NI, NN, TOLL, YI, YN AND ZC
C
C     TOTAL STORAGE EQUALS 2M(M+1)(M+2)/6 + 4M**2 +22M + 12M
C                   = (M**3 + 15*M**2 + 104*M)/3
C
C              ***   ARRAY STORAGE EQUIVALENCE TO A(.)  ***
C
C                 ARRAY             SIZE                  START
C
C                   XI               NL                       1
C                   XN               NL                    NL+1
C                 .............................................
C                  ILI             M**2           2*NL+       1
C                  ILN             M**2           2*NL+  M**2+1
C                   MD             M**2           2*NL+2*M**2+1
C                   NC             M**2           2*NL+3*M**2+1
C                 .............................................
C                   CL             11*M      2*NL+4*M**2+     1
C                   RM             11*M      2*NL+4*M**2+11*M+1
C                 .............................................
C                   CI                M      2*NL+4*M**2+22*M+1
C                   CN                M      2*NL+4*M**2+23*M+1
C                   CO                M      2*NL+4*M**2+24*M+1
C                   ID                M      2*NL+4*M**2+25*M+1
C                  IPI                M      2*NL+4*M**2+26*M+1
C                  IPN                M      2*NL+4*M**2+27*M+1
C                   NI                M      2*NL+4*M**2+28*M+1
C                   NN                M      2*NL+4*M**2+29*M+1
C                 TOLL                M      2*NL+4*M**2+30*M+1
C                   YI                M      2*NL+4*M**2+31*M+1
C                   YN                M      2*NL+4*M**2+32*M+1
C                   ZC                M      2*NL+4*M**2+33*M+1
C                 .............................................
C
C               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
C                      DAVID HOGBEN,
C                      STATISTICAL ENGINEERING DIVISION,
C                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
C                      A337 ADMINISTRATION BUILDING,
C                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
C                      GAITHERSBURG, MD 20899
C                          TELEPHONE 301-921-3651
C                  ORIGINAL VERSION - FEBRUARY, 1977.
C                   CURRENT VERSION -    APRIL, 1992.
C
C     ==================================================================
C
C                    ***   SPECIFICATION STATEMENTS   ***
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      PARAMETER (MAXC=40)
C
CCCCC DIMENSION     ID(29),    IPI(29),   IPN(29),    NI(29),    NN(29)
      DIMENSION  ID(MAXC),  IPI(MAXC),  IPN(MAXC),  NI(MAXC),  NN(MAXC)
      DIMENSION ILI(845), ILN(845), MD(845), NC(845)
C
CCCCC INCLUDE 'WRKSCR.H'
      REAL A(NS)
C
C     ==================================================================
C
C                         ***   TYPE STATEMENTS   ***
C
CCCCC REAL             RR(29,29)
      REAL             RR(MAXC,MAXC)
      REAL             BOUND, CAB, RS, R2
      REAL             SIG, SS, TEMP, TOL, TWO
      REAL             FDIV
      REAL             SPCA, SPCB
C
C     ..................................................................
C
      DOUBLE PRECISION DTWO
C
      PARAMETER (MAXV=38)
      CHARACTER*1 ICOD(MAXV)
      CHARACTER*38 IOUT
      CHARACTER*8 IVLIST
      COMMON/BESTC1/IOUNI1,IOUNI2
      COMMON/BESTC2/IVLIST(MAXV)
C
      PARAMETER(NUMCLI=3)
CCCCC PARAMETER(NUMCLI=17)
      PARAMETER(MAXLIN=1)
      PARAMETER (MAXROW=38)
      CHARACTER*40 ITITLE
      CHARACTER*40 ITITLZ
      CHARACTER*40 ITITL9
      CHARACTER*40 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      IDIGI2(NUMCLI)
      INTEGER      NTOT(MAXROW)
      CHARACTER*20 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*8  IVALUE(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRSTZ
      LOGICAL ILASTZ
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
C     ==================================================================
C
C                 ***   DATA INITIALIZATION STATEMENTS   ***
C
      DATA DTWO  / 2.0D0 /
C
      DATA RTWO  / 2.0 /
      DATA RONE  / 1.0 /
      DATA RZERO / 0.0 /
      DATA RER   / 1.0E-8 /
C
      DATA IFOUR  /4/
      DATA ITHRE  /3/
      DATA ITWO   /2/
      DATA IONE   /1/
      DATA IZERO  /0/
      DATA LWIDE  /80/
C
      DATA KO, NV / 10, 11 /
C
      DATA SPCA /   100.0 /
      DATA SPCB / 10000.0 /
C
      DATA ICOD(1) /'1'/
      DATA ICOD(2) /'2'/
      DATA ICOD(3) /'3'/
      DATA ICOD(4) /'4'/
      DATA ICOD(5) /'5'/
      DATA ICOD(6) /'6'/
      DATA ICOD(7) /'7'/
      DATA ICOD(8) /'8'/
      DATA ICOD(9) /'9'/
      DATA ICOD(10) /'0'/
      DATA ICOD(11) /'A'/
      DATA ICOD(12) /'B'/
      DATA ICOD(13) /'C'/
      DATA ICOD(14) /'D'/
      DATA ICOD(15) /'E'/
      DATA ICOD(16) /'F'/
      DATA ICOD(17) /'G'/
      DATA ICOD(18) /'H'/
      DATA ICOD(19) /'I'/
      DATA ICOD(20) /'J'/
      DATA ICOD(21) /'K'/
      DATA ICOD(22) /'L'/
      DATA ICOD(23) /'M'/
      DATA ICOD(24) /'N'/
      DATA ICOD(25) /'O'/
      DATA ICOD(26) /'P'/
      DATA ICOD(27) /'Q'/
      DATA ICOD(28) /'R'/
      DATA ICOD(29) /'S'/
      DATA ICOD(30) /'T'/
      DATA ICOD(31) /'U'/
      DATA ICOD(32) /'V'/
      DATA ICOD(33) /'W'/
      DATA ICOD(34) /'X'/
      DATA ICOD(35) /'Y'/
      DATA ICOD(36) /'Z'/
      DATA ICOD(37) /'a'/
      DATA ICOD(38) /'b'/
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
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      AINC=1800
      IWRTF(1)=AINC
      IWRTF(2)=IWRTF(1) + AINC
      IWRTF(3)=IWRTF(2) + AINC
C
C     ==================================================================
C
C     10=KO=NV-1     NL=(KX+1)*(KX+2)*(KX+3)/6      ND-1=NR-1
C                          NX=(KX+1)*(KX+2)/2
C
C                                 SET UP SIZE OF KZ, ND, NL AND NX.
C
      KZ = KX + IONE
      ND = KZ
      CALL IDIV (ND * (ND + IONE) * (ND + ITWO),6,IND,NL)
      CALL IDIV (ND * (ND + IONE),ITWO,IND,NX)
C
C                                 TEST INPUT.
C
      KZSIZE = ITWO * NL + IFOUR * ND ** 2 + 34 * ND
      IF (KZSIZE.GT.NS) THEN
         WRITE(ICOUT,23)
         CALL DPWRST('XXX','BUG ')
CCCCC    CALL ERROR (23)
         RETURN
      ENDIF
   23 FORMAT(1X,'***** ERROR FROM SCREEN (BEST CP): INSUFFICIENT ',
     1      'SCRATCH SPACE.')
CCCCC IF (NERROR.NE.IZERO) RETURN
C
C     ..................................................................
C
      IF (KX.GE.ITHRE .AND. KX.LT.ND .AND. NDEF.GT.KX .AND.
     1     MBST.GT.IZERO .AND. MBST.LE.KO .AND. KO.LE.NV .AND. NR.GT.KX
     2     .AND. IBIT.GE.IONE .AND. IBIT.LE.ITHRE) GO TO 10
CCCCC CALL ERROR (3)
      WRITE(ICOUT,3)
      CALL DPWRST('XXX','BUG ')
    3 FORMAT(1X,'***** ERROR FROM SCREEN (BEST CP): INVALID OPTIONS')
      RETURN
C
C     ..................................................................
C
  10  SS = FDIV (RR(KZ,KZ),SPCA,IND)
      IF (IBIT.EQ.ITWO) SS = FDIV (SS,FLOAT(NDEF),IND)
      IF (SS.GT.RZERO) GO TO 30
  20  CONTINUE
CCCCC CALL ERROR (22)
      WRITE(ICOUT,22)
      CALL DPWRST('XXX','BUG ')
   22 FORMAT(1X,'***** ERROR FROM SCREEN (BEST CP): NON-POSITIVE SUM ',
     1      'OF SQUARES')
      RETURN
C
C     ..................................................................
C
C                                 INITIALIZE.
C
  30  LSUBXI = IONE
      LSUBXC = IONE
      LSUBXN = NL + IONE
      LSUBLI = ITWO * NL + IONE
      LSUBLN = LSUBLI + KZ ** 2
      LSUBMD = LSUBLN + KZ ** 2
      LSUBNC = LSUBMD + KZ ** 2
      LSUBCL = LSUBNC + KZ ** 2
      LSUBRM = LSUBCL + 11 * KZ
      LSUBCI = LSUBRM + 11 * KZ
      LSUBCN = LSUBCI + KZ
      LSUBCO = LSUBCN + KZ
      LSUBID = LSUBCO + KZ
      LSUBPI = LSUBID + KZ
      LSUBPN = LSUBPI + KZ
      LSUBNI = LSUBPN + KZ
      LSUBNN = LSUBNI + KZ
      LSUBTL = LSUBNN + KZ
      LSUBYI = LSUBTL + KZ
      LSUBYN = LSUBYI + KZ
      LSUBZC = LSUBYN + KZ
      A(LSUBCN)  = RZERO
      A(LSUBCI)  = RZERO
      TOL    = FDIV (RER,SPCB,IND)
      TWO    = RTWO * RR(KZ,KZ) * FLOAT(NDEF)
      LOW    = KO - MBST + IONE
      LISUBL = IONE
      LNSUBL = IONE
      MDSUBL = IONE
      NCSUBL = IONE
      IDSUBL = IONE
      NPSUBL = IONE
      IPSUBL = IONE
      NISUBL = IONE
      NNSUBL = IONE
      ISUBLI = LISUBL
      ISUBNC = NCSUBL
      ISUBCL = LSUBCL
      ISUBRM = LSUBRM
      ISUBCO = LSUBCO
      KSUBRM = LSUBRM + KO
      ISUBID = IDSUBL
      ISUBPN = NPSUBL
      ISUBTL = LSUBTL
C
C  FOR DATAPLOT, SET NTLINE HIGH.  THAT IS, WE ARE NOT USING A PAGE
C  BASED OUTPUT.
C
      NTLINE = 500
C
CCCCC IF (NCRT.NE.IZERO) NTLINE = LENGTH + ITHRE
      DO 50 L=1,KZ
        CALL IDIV ((KZ-IONE)*KZ*(KZ+IONE)-(KZ-L)*(KZ-L+IONE)*
     1                (KZ-L+ITWO),6,IND,ID(ISUBID))
        IPN(ISUBPN)  = IONE
        ILI(ISUBLI)  = L
        A(KSUBRM)   = -TWO
        KSUBRM       = KSUBRM + 11
        A(ISUBCO)   = DTWO**(KX-L)
        NC(ISUBNC)   = L
        A(ISUBTL) = TOL * RR(L,L)
        IF (A(ISUBTL).LE.RZERO) GO TO 20
        JSUBCL = ISUBCL
        JSUBRM = ISUBRM
        DO 40 M=1,KO
          A(JSUBCL) = RZERO
          A(JSUBRM) = TWO
          JSUBCL     = JSUBCL + IONE
          JSUBRM     = JSUBRM + IONE
  40    CONTINUE
        ISUBCL = ISUBCL + 11
        ISUBRM = ISUBRM + 11
        ISUBCO = ISUBCO + IONE
        ISUBLI = ISUBLI + KZ
        ISUBNC = ISUBNC + KZ
        ISUBID = ISUBID + IONE
        ISUBPN = ISUBPN + IONE
        ISUBTL = ISUBTL + IONE
  50  CONTINUE
C
C                           STORE MATRICES AS VECTORS.
C
      LS     = IZERO
      ISUBXC = LSUBXC - IONE
      ISUBXN = LSUBXN
      ISUBMD = MDSUBL
      MSUBMD = MDSUBL - IONE
      DO 70 L=1,KZ
        KSUBMD = ISUBMD
        JSUBMD = MSUBMD + KZ * (L - IONE) + L
        DO 60 M=L,KZ
          LS         = LS + IONE
          ISUBXC     = ISUBXC + IONE
          MD(KSUBMD) = LS
          MD(JSUBMD) = LS
          A(ISUBXC) = RR(L,M)
          A(ISUBXN) = A(ISUBXC)
          RR(M,L)    = RR(L,M)
          ISUBXN     = ISUBXN + IONE
          KSUBMD     = KSUBMD + KZ
          JSUBMD     = JSUBMD + IONE
  60    CONTINUE
        ISUBMD = ISUBMD + IONE + KZ
  70  CONTINUE
C
C                             INVERT MATRIX STEPWISE.
C
      ISUBMD = MDSUBL + KZ ** 2 - IONE
      ISUB2  = MD(ISUBMD) + LSUBXC - IONE
      NSUBLI = LISUBL
      NSUBLN = LNSUBL
      NSUBMD = MDSUBL + KZ * (KZ - IONE) - IONE
      ISUBRM = LSUBRM - IONE + KO
      MSUBRM = LSUBRM
      ISUBCO = LSUBCO - IONE
      DO 90 N=1,KX
        J      = IZERO
        N1     = N
        ISUBLI = NSUBLI
        DO 80 LA=N,KX
          L      = ILI(ISUBLI)
          ISUBLI = ISUBLI + KZ
          ISUBMD = MDSUBL + KZ * (L -IONE) - IONE
          MSUBMD = NSUBMD + L
          ISUBMD = ISUBMD + L
          ISUBTL = LSUBTL + L - IONE
          ISUB1  = MD(ISUBMD) + LSUBXC - IONE
          IF (A(ISUB1).LT.A(ISUBTL)) GO TO 80
          ISUB3 = MD(MSUBMD) + LSUBXC - IONE
          RS = A(ISUB2) - FDIV (A(ISUB3)*A(ISUB3),A(ISUB1),IND)
          IF (RS.LT.A(ISUBRM)) J = LA
          MSUBCO = ISUBCO + L
          IF (RS.LT.A(MSUBRM)) CALL CPSTRE (RS,A(LSUBCI)+A(MSUBCO),
     1                                KO,A(LSUBCL),A(LSUBRM),N1,NV,ND)
  80    CONTINUE
        IF (J.EQ.IZERO) GO TO 100
        JSUBLI      = LISUBL + KZ * (J -IONE)
        M           = ILI(JSUBLI)
        ILI(JSUBLI) = ILI(NSUBLI)
        ILI(NSUBLI) = M
        ILN(NSUBLN) = M
        MSUBCO      = ISUBCO + M
        A(LSUBCI)  = A(LSUBCI) + A(MSUBCO)
        NSUBLI      = NSUBLI + KZ
        NSUBLN      = NSUBLN + KZ
        ISUBRM      = ISUBRM + 11
        MSUBRM      = MSUBRM + 11
        CALL PIVOT (A(LSUBXC),KZ,M,MD(MDSUBL),ND,NX)
  90  CONTINUE
C
      N      = KZ
 100  K      = N - IONE
      KP     = KZ * K + LISUBL
      KXSUBL = KZ * (KX - IONE) + LISUBL
      IF (K.NE.KX) THEN
         ICNT=0
         DO102I=KP,KXSUBL,KZ
           ICNT=ICNT+1
           IF(ICNT.EQ.22)ILAST=I
           IF(ICNT.EQ.23)IFRST=I
  102    CONTINUE
CCCCC    WRITE (ICOUT,330) (ILI(I),I=KP,KXSUBL,KZ)
         WRITE (ICOUT,330)
 330     FORMAT(2X,
     1          'SCREEN-MATRIX IS SINGULAR.  VARIABLES DELETED ARE ...')
         CALL DPWRST('XXX','BUG ')
         IF(ICNT.LE.22)THEN
           WRITE (ICOUT,331) (ILI(I),I=KP,KXSUBL,KZ)
 331       FORMAT(5X,22I3)
           CALL DPWRST('XXX','BUG ')
         ELSE
           WRITE (ICOUT,331) (ILI(I),I=KP,KXSUBL,ILAST)
           CALL DPWRST('XXX','BUG ')
           WRITE (ICOUT,331) (ILI(I),I=IFRST,KXSUBL,KZ)
           CALL DPWRST('XXX','BUG ')
         ENDIF
      ENDIF
      IF (K.LT.ITHRE) RETURN
      KM = K - IONE
C
C     INTCPT - IONE = ADJUSTMENT FOR USING WITH NO CONSTANT TERM.
C
      SIG    = FDIV (RTWO*A(ISUBXC),FLOAT(NDEF-K+IONE-INTCPT),IND)
      A(LSUBYI)  = A(ISUBXC)
      A(LSUBYN)  = RR(KZ,KZ)
C
      NI(NISUBL) = K
      NN(NNSUBL) = K
      ISUBCL     = LSUBCL - IONE
      ISUBRM     = LSUBRM
      KSUBRM     = LSUBRM + 11 * (KZ - IONE)
      IF (IBIT.EQ.IONE) GO TO 130
      DO 120 M=1,K
        MSUBCL = ISUBCL
        MSUBRM = ISUBRM
        DO 110 L=1,KO
          IF (IBIT.EQ.ITWO)  RS = FDIV (A(MSUBRM),FLOAT(NDEF-M),IND)
          IF (IBIT.EQ.ITHRE) RS = A(MSUBRM) + SIG * FLOAT (M)
          MSUBCL = MSUBCL + IONE
          MSUBRM = MSUBRM + IONE
          IF (RS.GE.A(KSUBRM)) GO TO 110
          TEMP   = A(MSUBCL)
          CALL CPSTRE (RS,TEMP,KO,A(LSUBCL),A(LSUBRM),KZ,NV,ND)
 110    CONTINUE
        ISUBCL = ISUBCL + 11
        ISUBRM = ISUBRM + 11
 120  CONTINUE
C
 130  NREG =  IZERO
      NCAL =  ITWO
      MN   =  ITWO
      MV   = -IONE
C
C                                 STAGE  LOOP.
C
 140  CONTINUE
      JSUBRM = KSUBRM
      IF (MN.EQ.IONE) GO TO 240
      ISUBPN      = NPSUBL + MN - IONE
      IP          = IPN(ISUBPN)
      IPN(ISUBPN) = IP + IONE
      MV          = MV - IPN(ISUBPN+1) + IP + ITWO
      ISUBPI      = IPSUBL + MV - IONE
      IPI(ISUBPI) = IP
      MN          = MN - IONE
      ISUBPN      = ISUBPN - IONE
      IN          = IPN(ISUBPN)
      JC          = MV
      ISUBYI      = LSUBYI + IP - IONE
      BOUND       = A(ISUBYI)
      A(ISUBYI)  = TWO
C
C                              FIND LEAP FROM BOUNDS.
C
      ISUBRM = LSUBRM + LOW - IONE
      KSUBRM = LSUBRM + 11 * (KZ - IONE) + LOW - IONE
      DO 150 LB=IP,KM
        MT     = MN + KM - LB
        MSUBRM = ISUBRM + 11 * (MT - IONE)
        IF (IBIT.EQ.IONE .AND. A(MSUBRM).GT.BOUND) GO TO 160
        IF (IBIT.EQ.ITWO .AND. A(KSUBRM).GT.FDIV(BOUND,FLOAT(NDEF-MT),
     1     IND)) GO TO 160
        IF (IBIT.EQ.ITHRE .AND. A(KSUBRM).GT.BOUND+SIG*FLOAT(MT))
     1           GO TO 160
 150  CONTINUE
      GO TO 140
C
 160  LC = KM + IP - LB
      NREG = NREG + ITWO * (LC-IP+IONE)
      IF (IP.EQ.IONE) LC = K
C
C                         REGRESSIONS FROM INVERSE MATRIX.
C
      ISUBNI = NISUBL + IP
      ISUBNN = NNSUBL + IP
      KSUBLI = LISUBL + IP - IONE
      KSUBLN = LNSUBL + IN - IONE
      KSUBNN = NNSUBL + IN - IONE
      DO 200 LB=IP,LC
        LBB = LB
        CALL BACK (NC(NCSUBL),LBB,LI,IPI(IPSUBL),MV,RS,BOUND,ILI(LISUBL)
     1            ,JC,ID(IDSUBL),A(LSUBXI),MD(MDSUBL),
     2             IONE,NI(NISUBL),ND,KZ,NL,NCAL)
C
C                               RE-ORDER VARIABLES.
C
        M      = LB
        MSUBLN = KSUBLN + KZ * (M - IONE)
        MSUBLI = KSUBLI + KZ * (M - IONE)
        ISUBYI = LSUBYI + M - IONE
        IF (LB.GT.NN(KSUBNN)) GO TO 190
        LN = ILN(MSUBLN)
 170    IF (RS.LE.A(ISUBYI)) GO TO 180
        A(ISUBYI+1) = A(ISUBYI)
        NSUBLI       = MSUBLI - KZ
        NSUBLN       = MSUBLN - KZ
        ILI(MSUBLI)  = ILI(NSUBLI)
        ILN(MSUBLN)  = ILN(NSUBLN)
        M            = M - IONE
        MSUBLI       = MSUBLI - KZ
        MSUBLN       = MSUBLN - KZ
        ISUBYI       = ISUBYI - IONE
        GO TO 170
 180    ILI(MSUBLI)  = LI
        ILN(MSUBLN)  = LN
 190    A(ISUBYI+1) = RS
        NI(ISUBNI)   = LB
        NN(ISUBNN)   = LB
        ISUBNI       = ISUBNI + IONE
        ISUBNN       = ISUBNN + IONE
 200  CONTINUE
      IF (LC.EQ.K) LC = KM
      MI = K - MV
      JC = MN
C
C                         REGRESSIONS FROM PRODUCT MATRIX.
C
      ISUBRM = LSUBRM + 11 * (MI - IONE)
      KSUBRM = LSUBRM + 11 * (KZ - IONE)
      ISUBCI = LSUBCI + IP - IONE
      ISUBYI = LSUBYI + IP - IONE
      ISUBYN = LSUBYN + IP - IONE
      ISUBCO = LSUBCO - IONE
      DO 230 LB=IP,LC
        LBB        = LB
        ISUBCN     = LSUBCN + IN - IONE
        ISUBNC     = NCSUBL + IN - IONE
        KSUBYN     = LSUBYN + IN - IONE
        ISUBYI     = ISUBYI + IONE
        ISUBYN     = ISUBYN + IONE
        IS         = LB + IONE
        MSUBCN     = LSUBCN + LB
        A(MSUBCN) = A(KSUBYN)
        CALL BACK (NC(NCSUBL),LBB,L,IPN(NPSUBL),MN,A(ISUBYN),A(MSUBCN)
     1            ,ILN(LNSUBL),JC,ID(IDSUBL),A(LSUBXN),MD(MDSUBL),
     2             IZERO,NN(NNSUBL),ND,KZ,NL,NCAL)
        MSUBNC     = ISUBNC + KZ * (L - IONE)
        ISUB4      = NC(MSUBNC)
        MSUBCI     = LSUBCI + LB
        MSUBCO     = ISUBCO + ISUB4
        A(MSUBCI) = A(ISUBCI) - A(MSUBCO)
        A(MSUBCN) = A(ISUBCN) + A(MSUBCO)
        IF (A(ISUBYI).GE.A(ISUBRM)) GO TO 210
        CALL CPSTRE (A(ISUBYI),A(MSUBCI),KO,A(LSUBCL),A(LSUBRM),MI,
     1               NV,ND)
        IF (IBIT.EQ.IONE) GO TO 210
        IF (IBIT.EQ.ITWO) RS = FDIV (A(ISUBYI),FLOAT(NDEF-MI),IND)
        IF (IBIT.EQ.ITHRE) RS = A(ISUBYI) + FLOAT(MI) * SIG
        IF (RS.LT.A(KSUBRM)) CALL CPSTRE (RS,A(MSUBCI),KO,A(LSUBCL),
     1      A(LSUBRM),KZ,NV,ND)
 210    MSUBRM = LSUBRM + 11 * (MN - IONE)
        IF (A(ISUBYN).GE.A(MSUBRM)) GO TO 220
        CALL CPSTRE (A(ISUBYN),A(MSUBCN),KO,A(LSUBCL),A(LSUBRM),MN,
     1               NV,ND)
        IF (IBIT.EQ.IONE) GO TO 220
        IF (IBIT.EQ.ITWO) RS = FDIV (A(ISUBYN),FLOAT(NDEF-MN),IND)
        IF (IBIT.EQ.ITHRE) RS = A(ISUBYN) + FLOAT(MN) * SIG
        IF (RS.LT.A(KSUBRM)) CALL CPSTRE (RS,A(MSUBCN),KO,A(LSUBCL),
     1      A(LSUBRM),KZ,NV,ND)
 220    MN            = MN + IONE
        ISUBPN        = NPSUBL + MN - IONE
        IPN(ISUBPN+1) = IPN(ISUBPN) + IONE
        IN            = IS
 230  CONTINUE
      IF (LC.EQ.KM) MN = MN - IONE
      GO TO 140
C
C                                    OUTPUT.
C
 240  CONTINUE
      CALL IDIV (KX-IONE,ITWO,IND,NJUNK)
      NLINES = 8 + NJUNK
      ISUBCL = LSUBCL - 12
      ISUBRM = LSUBRM - 12
C
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
C
      DO 320 M=1,K
        MM     = M
        ISUBCL = ISUBCL + 11
        ISUBRM = ISUBRM + 11
CCCCC   IF (NLINES+ITHRE.LE.NTLINE) GO TO 250
CCCCC   CALL PAGE (IFOUR)
CCCCC   NLINES = ITHRE
C250    CONTINUE
        IF (KO.GT.IONE .AND. M.EQ.IONE) THEN
CCCCC      WRITE (ICOUT,390)
C390       FORMAT(4X,'REGRESSION WITH 1 VARIABLE')
CCCCC      CALL DPWRST('XXX','BUG ')
           ITITLE='Regression with One Variable'
           NCTITL=28
        ELSEIF (KO.GT.IONE .AND. M.GT.IONE) THEN
CCCCC      WRITE(ICOUT,999)
CCCCC      CALL DPWRST('XXX','BUG ')
CCCCC      WRITE (ICOUT,340) M
C340       FORMAT(4X,'REGRESSIONS WITH',I3,' VARIABLES')
CCCCC      CALL DPWRST('XXX','BUG ')
           ITITLE='Regressions with     Variables'
           WRITE(ITITLE(18:20),'(I3)')M
           NCTITL=30
        ENDIF
C
        NLINES = NLINES + ITWO
        IPRTSW = IZERO
        DO 310 LA=1,KO
          NCOF   = IONE
          L      = KO - LA + IONE
          MSUBRM = ISUBRM + L
CCCCC     IF (A(MSUBRM).EQ.TWO) GO TO 320
          IF (A(MSUBRM).EQ.TWO) GO TO 329
          IF (IBIT.EQ.IONE)  R2 = SPCA - FDIV (A(MSUBRM),SS,IND)
          IF (IBIT.EQ.ITWO)  RS = FDIV (A(MSUBRM),FLOAT(NDEF-M),IND)
          IF (IBIT.EQ.ITHRE) RS = A(MSUBRM) + SIG * FLOAT(M)
          IF (IBIT.EQ.IONE .AND. LA.LE.MBST .OR. IBIT.GT.IONE
     1         .AND. RS.LE.A(JSUBRM)) NCOF = IZERO
          IF (IBIT.EQ.ITWO)  R2 = SPCA - FDIV (RS,SS,IND)
          IF (IBIT.EQ.ITHRE) R2 = RTWO * FDIV (RS,SIG,IND) - FLOAT(NDEF)
C
C           ADJUSTMENT TO ALLOW USE OF MODEL WHICH DOES NOT HAVE
C              A CONSTANT TERM FOR THE FIRST TERM.
C                 CHANGE SUGGESTED BY JAMES W. FRANE.
C
          IF  (IBIT.EQ.ITHRE .AND. INTCPT.EQ.IZERO) R2 = R2 - RONE
          IF  (IBIT.EQ.ITHRE .AND. INTCPT.EQ.IONE ) R2 = R2 + RONE
          ANTEMP=REAL(NDEF+1)
          RSSTMP=A(MSUBRM)
          RSSTM2=RSSTMP/ANTEMP
          BIC=ANTEMP*LOG(RSSTM2) + REAL(M+1)*LOG(ANTEMP)
C
C                               DECODE LABELS.
C
          MSUBCL = ISUBCL + L
          CAB    = A(MSUBCL)
          MP     = IONE
          ISUBCO = LSUBCO - IONE
          ISUBPN = NPSUBL
          DO 260 I=1,KX
            ISUBCO      = ISUBCO + IONE
            IF (CAB.LT.A(ISUBCO)) GO TO 260
            IPN(ISUBPN) = I
            MP          = MP + IONE
            CAB         = CAB - A(ISUBCO)
            ISUBPN      = ISUBPN + IONE
 260      CONTINUE
C
          IF (NCOF.NE.IZERO) THEN
             ICNT9=ICNT9+1
             IF (IPRTSW.GT.IZERO) GO TO 300
             NLINES = NLINES + M + IONE
             IF (M.GT.15 .AND. LWIDE.LT.110) NLINES = NLINES + M
             IF (NLINES.LE.NTLINE) GO TO 290
CCCCC        CALL PAGE (IFOUR)
             NLINES = M + IFOUR
             IF (M.GT.15 .AND. LWIDE.LT.110) NLINES = NLINES + M
 290         CONTINUE
C
CCCCC        WRITE (ICOUT,350)
C350         FORMAT(10X,'C(P) STATISTIC',2X,'VARIABLES')
CCCCC        CALL DPWRST('XXX','BUG ')
C
             NUMCOL=2
             NUMLIN=1
C
             DO1183I=1,MAXLIN
               DO1185J=1,NUMCLI
                 ITITL2(I,J)=' '
                 NCTIT2(I,J)=0
 1185          CONTINUE
 1183        CONTINUE
C
             ITITL2(1,1)='C(p) Statistic'
             NCTIT2(1,1)=14
             ITITL2(1,2)='BIC'
             NCTIT2(1,2)=3
             ITITL2(1,3)='Variable'
             NCTIT2(1,3)=8
C
             NMAX=0
             NUMCOL=3
             DO1193I=1,NUMCOL
               VALIGN(I)='b'
               ALIGN(I)='r'
               NTOT(I)=15
               NMAX=NMAX+NTOT(I)
               ITYPCO(I)='NUME'
               IDIGI2(I)=NUMDIG
               IF(I.EQ.3)THEN
                 ITYPCO(I)='ALPH'
                 IDIGI2(I)=-1
               ENDIF
               DO1195J=1,MAXROW
                 IVALUE(J,I)=' '
                 NCVALU(J,I)=0
                 AMAT(J,I)=0.0
 1195          CONTINUE
 1193        CONTINUE
             ICNT=0
C
             IPRTSW = IONE
C
 300         CONTINUE
             ISTPPN = NPSUBL + M - IONE
             IJUNK=1
             IF(M.EQ.IONE)THEN
C
C              FOLLOWING CODE ONLY IMPLEMENTED FOR THE REGRESSIONS
C              WITH ONE VARIABLE, SO CAN SIMPLIFY CODE A BIT.
C
               WRITE(IOUNI1,71)IJUNK,R2,BIC,IVLIST(IPN(NPSUBL))
  71           FORMAT(I3,1X,2F15.3,' : ',A8)
               WRITE(IOUNI2,'(A1)')ICOD(IPN(NPSUBL))
             ENDIF
CCCCC        IF (LWIDE.GE.110) THEN
CCCCC           WRITE (ICOUT,360) R2, (IPN(I),I=NPSUBL,ISTPPN)
C360            FORMAT(13X,F8.3,5X,28I3)
CCCCC           CALL DPWRST('XXX','BUG ')
CCCCC        ELSEIF (LWIDE.LT.110) THEN
                INUMB=ISTPPN-NPSUBL+1
                IF(INUMB.LE.15)THEN
CCCCC             WRITE (ICOUT,370) R2, (IPN(I),I=NPSUBL,ISTPPN)
C370              FORMAT(14X,F8.3,3X,15I3)
CCCCC             CALL DPWRST('XXX','BUG ')
                  ICNT=ICNT+1
                  AMAT(ICNT,1)=R2
                  AMAT(ICNT,2)=BIC
C
C                 FOLLOWING ASSUMES ONLY ONE VARIABLE
C
CCCCC             WRITE(IVALUE(ICNT,3),'(15I3)')(IPN(I),I=NPSUBL,ISTPPN,15)
CCCCC             NCVALU(ICNT,3)=3*INUMB
                  IVALUE(ICNT,3)=IVLIST(IPN(NPSUBL))
                  NCVALU(ICNT,3)=8
                ELSE
C
C                 NOTE: SINCE THIS FORMATTING ONLY USED FOR THE
C                       ONE VARIABLE CASE, CAN COMMENT OUT THIS
C                       SECTION.
C
CONE              ITEMP1=NPSUBL+14
CCCCC             WRITE (ICOUT,370) R2, (IPN(I),I=NPSUBL,ITEMP1)
CCCCC             CALL DPWRST('XXX','BUG ')
CCCCC             WRITE (ICOUT,371) R2, (IPN(I),I=ITEMP1+1,ISTPPN)
C371              FORMAT(26X,15I3)
CCCCC             CALL DPWRST('XXX','BUG ')
CONE              ICNT=ICNT+1
CONE              AMAT(ICNT,1)=R2
CONE              AMAT(ICNT,2)=BIC
CONE              WRITE(IVALUE(ICNT,3),'(15I3)')
CONE 1                  (IPN(I),I=NPSUBL,ITEMP1)
CONE              NCVALU(ICNT,3)=45
CONE              ICNT=ICNT+1
CONE              AMAT(ICNT,1)=R2
CONE              AMAT(ICNT,2)=BIC
CONE              WRITE(IVALUE(ICNT,3),'(15I3)')
CONE 1                  (IPN(I),I=ITEMP1+1,ISTPPN)
CONE              ITEMP2=ISTPPN-ITEMP1
CONE              NCVALU(ICNT,3)=3*ITEMP2
                ENDIF
          ELSE
             NUMCOL=3
             NUMLIN=1
C
             DO183I=1,MAXLIN
               DO185J=1,NUMCLI
                 ITITL2(I,J)=' '
                 NCTIT2(I,J)=0
  185          CONTINUE
  183        CONTINUE
C
             ITITL2(1,1)='Variable'
             NCTIT2(1,1)=8
             ITITL2(1,2)='Coefficient'
             NCTIT2(1,2)=11
             ITITL2(1,3)='F Ratio'
             NCTIT2(1,3)=7
C
             NMAX=0
             NUMCOL=3
             DO193I=1,NUMCOL
               VALIGN(I)='b'
               ALIGN(I)='r'
               NTOT(I)=15
               NMAX=NMAX+NTOT(I)
               ITYPCO(I)='NUME'
               IF(I.EQ.1)ITYPCO(I)='ALPH'
               IDIGI2(I)=NUMDIG
               IF(I.EQ.1)THEN
                 IDIGI2(I)=-1
               ELSEIF(I.EQ.3)THEN
                 IDIGI2(I)=3
               ENDIF
               DO195J=1,MAXROW
                 IVALUE(J,I)=' '
                 NCVALU(J,I)=0
                 AMAT(J,I)=0.0
  195          CONTINUE
  193        CONTINUE
C
CCCCC        NLINES = NLINES + M + ITHRE
CCCCC        IF (NLINES.LE.NTLINE) GO TO 270
CCCCC        CALL PAGE (IFOUR)
CCCCC        NLINES = M + 6
C270         CONTINUE
             CALL COEF (R2,BIC,MP,KZ,A(LSUBXC),RR,MAXC,IPN(NPSUBL),
     1                  NDEF,MM,ND,
     1                  MD(MDSUBL),NX,IBIT,A(LSUBZC),
     1                  AMAT,IVALUE,NCVALU,MAXROW,NUMCLI,ITITL9,NCTIT9)
C
             NUMLIN=1
             ICNT=MM
             IFRSTZ=.TRUE.
             ILASTZ=.TRUE.
             IFLAGS=.TRUE.
             IFLAGE=.TRUE.
             IF(IPRINT.EQ.'ON')THEN
               CALL DPDTA5(ITITLE,NCTITL,
     1                     ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                     MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                     IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                     IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                     ICAPSW,ICAPTY,IFRSTZ,ILASTZ,
     1                     IFLAGS,IFLAGE,
     1                     ISUBRO,IBUGA3,IERROR)
             ENDIF
             ITITLE=' '
             NCTITL=0
             ICNT9=0
C
CCCCC        GO TO 310
          ENDIF
C
 310    CONTINUE
C
 329    CONTINUE
        NUMLIN=1
        ITITL9=' '
        NCTIT9=0
        IFRSTZ=.TRUE.
        ILASTZ=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        IF(IPRINT.EQ.'ON' .AND. ICNT9.GT.0)THEN
          CALL DPDTA5(ITITLE,NCTITL,
     1                ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRSTZ,ILASTZ,
     1                IFLAGS,IFLAGE,
     1                ISUBRO,IBUGA3,IERROR)
        ENDIF
C
 320  CONTINUE
      NCAL = NCAL + ITWO * NREG
      IF(IFEEDB.EQ.'ON')THEN
        WRITE (ICOUT,380) NREG, NCAL
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
C
C     ==================================================================
C
C                       ***   FORMAT STATEMENTS   ***
C
 380  FORMAT(2X,I9,' REGRESSIONS',2X,I10,' OPERATIONS')
 999  FORMAT(1X)
C
C     ==================================================================
C
      END
      SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA,
     1                 XDATA,NOBS)
C***BEGIN PROLOGUE  SNSQE
C***DATE WRITTEN   800301   (YYMMDD)
C***REVISION DATE  880222   (YYMMDD)
C***CATEGORY NO.  F2A
C***KEYWORDS  EASY-TO-USE,NONLINEAR SQUARE SYSTEM,POWELL HYBRID METHOD,
C             ZERO
C***AUTHOR  HIEBERT, K. L., (SNLA)
C***PURPOSE  SNSQE is the easy-to-use version of SNSQ which finds a zero
C            of a system of N nonlinear functions in N variables by a
C            modification of Powell hybrid method.  This code is the
C            combination of the MINPACK codes(Argonne) HYBRD1 and HYBRJ1
C***DESCRIPTION
C
C 1. Purpose. 
C
C
C       The purpose of SNSQE is to find a zero of a system of N non-
C       linear functions in N variables by a modification of the Powell
C       hybrid method.  This is done by using the more general nonlinear
C       equation solver SNSQ.  The user must provide a subroutine which
C       calculates the functions.  The user has the option of either to
C       provide a subroutine which calculates the Jacobian or to let the
C       code calculate it by a forward-difference approximation.  This
C       code is the combination of the MINPACK codes (Argonne) HYBRD1
C       and HYBRJ1.
C
C
C 2. Subroutine and Type Statements.
C
C       SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,
C      *                  WA,LWA)
C       INTEGER IOPT,N,NPRINT,INFO,LWA
C       REAL TOL
C       REAL X(N),FVEC(N),WA(LWA)
C       EXTERNAL FCN,JAC
C
C
C 3. Parameters.
C
C       Parameters designated as input parameters must be specified on
C       entry to SNSQE and are not changed on exit, while parameters
C       designated as output parameters need not be specified on entry
C       and are set to appropriate values on exit from SNSQE. 
C
C       FCN is the name of the user-supplied subroutine which calculates
C         the functions.  FCN must be declared in an EXTERNAL statement
C         in the user calling program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         REAL X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of SNSQE.  In this case, set
C         IFLAG to a negative integer.
C
C       JAC is the name of the user-supplied subroutine which calculates
C         the Jacobian.  If IOPT=1, then JAC must be declared in an
C         EXTERNAL statement in the user calling program, and should be
C         written as follows.
C
C         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C         INTEGER N,LDFJAC,IFLAG
C         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
C         ----------
C         Calculate the Jacobian at X and return this
C         matrix in FJAC.  FVEC contains the function
C         values at X and should not be altered.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by JAC unless the
C         user wants to terminate execution of SNSQE.  In this case, set
C         IFLAG to a negative integer.
C
C         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
C
C       IOPT is an input variable which specifies how the Jacobian will 
C         be calculated.  If IOPT=1, then the user must supply the
C         Jacobian through the subroutine JAC.  If IOPT=2, then the
C         code will approximate the Jacobian by forward-differencing. 
C
C       N is a positive integer input variable set to the number of
C         functions and variables.
C
C       X is an array of length N.  On input, X must contain an initial
C         estimate of the solution vector.  On output, X contains the
C         final estimate of the solution vector.
C
C       FVEC is an output array of length N which contains the functions
C         evaluated at the output X.
C
C       TOL is a non-negative input variable.  Termination occurs when
C         the algorithm estimates that the relative error between X and
C         the solution is at most TOL.  Section 4 contains more details
C         about TOL.
C
C       NPRINT is an integer input variable that enables controlled
C         printing of iterates if it is positive.  In this case, FCN is
C         called with IFLAG = 0 at the beginning of the first iteration
C         and every NPRINT iteration thereafter and immediately prior
C         to return, with X and FVEC available for printing. Appropriate
C         print statements must be added to FCN (see example). If NPRINT
C         is not positive, no special calls of FCN with IFLAG = 0 are
C         made. 
C
C       INFO is an integer output variable.  If the user has terminated
C         execution, INFO is set to the (negative) value of IFLAG.  See
C         description of FCN and JAC. Otherwise, INFO is set as follows.
C
C         INFO = 0  improper input parameters. 
C
C         INFO = 1  algorithm estimates that the relative error between
C                   X and the solution is at most TOL.
C
C         INFO = 2  number of calls to FCN has reached or exceeded
C                   100*(N+1) for IOPT=1 or 200*(N+1) for IOPT=2.
C
C         INFO = 3  TOL is too small.  No further improvement in the
C                   approximate solution X is possible.
C
C         INFO = 4  iteration is not making good progress.
C
C         Sections 4 and 5 contain more details about INFO.
C
C       WA is a work array of length LWA.
C
C       LWA is a positive integer input variable not less than
C         (3*N**2+13*N))/2.
C
C
C 4. Successful Completion.
C
C       The accuracy of SNSQE is controlled by the convergence parame-
C       ter TOL.  This parameter is used in a test which makes a compar-
C       ison between the approximation X and a solution XSOL.  SNSQE
C       terminates when the test is satisfied.  If TOL is less than the
C       machine precision (as defined by the function R1MACH(4)), then
C       SNSQE attemps only to satisfy the test defined by the machine
C       precision.  Further progress is not usually possible.  Unless
C       high precision solutions are required, the recommended value
C       for TOL is the square root of the machine precision. 
C
C       The test assumes that the functions are reasonably well behaved,
C       and, if the Jacobian is supplied by the user, that the functions
C       and the Jacobian  coded consistently.  If these conditions
C       are not satisfied, SNSQE may incorrectly indicate convergence.
C       The coding of the Jacobian can be checked by the subroutine
C       CHKDER.  If the Jacobian is coded correctly or IOPT=2, then
C       the validity of the answer can be checked, for example, by
C       rerunning SNSQE with a tighter tolerance. 
C
C       Convergence Test.  If SNRM2(Z) denotes the Euclidean norm of a 
C         vector Z, then this test attempts to guarantee that
C
C               SNRM2(X-XSOL) .LE.  TOL*SNRM2(XSOL).
C
C         If this condition is satisfied with TOL = 10**(-K), then the
C         larger components of X have K significant decimal digits and 
C         INFO is set to 1.  There is a danger that the smaller compo- 
C         nents of X may have large relative errors, but the fast rate
C         of convergence of SNSQE usually avoids this possibility. 
C
C
C 5. Unsuccessful Completion. 
C
C       Unsuccessful termination of SNSQE can be due to improper input
C       parameters, arithmetic interrupts, an excessive number of func-
C       tion evaluations, errors in the functions, or lack of good prog-
C       ress.
C
C       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1, or
C         IOPT .GT. 2, or N .LE. 0, or TOL .LT. 0.E0, or
C         LWA .LT. (3*N**2+13*N)/2.
C
C       Arithmetic Interrupts.  If these interrupts occur in the FCN
C         subroutine during an early stage of the computation, they may
C         be caused by an unacceptable choice of X by SNSQE.  In this
C         case, it may be possible to remedy the situation by not evalu-
C         ating the functions here, but instead setting the components
C         of FVEC to numbers that exceed those in the initial FVEC.
C
C       Excessive Number of Function Evaluations.  If the number of
C         calls to FCN reaches 100*(N+1) for IOPT=1 or 200*(N+1) for
C         IOPT=2, then this indicates that the routine is converging
C         very slowly as measured by the progress of FVEC, and INFO is
C         set to 2.  This situation should be unusual because, as
C         indicated below, lack of good progress is usually diagnosed 
C         earlier by SNSQE, causing termination with INFO = 4.
C
C       Errors in the Functions.  When IOPT=2, the choice of step length
C         in the forward-difference approximation to the Jacobian
C         assumes that the relative errors in the functions are of the
C         order of the machine precision.  If this is not the case,
C         SNSQE may fail (usually with INFO = 4).  The user should
C         then either use SNSQ and set the step length or use IOPT=1
C         and supply the Jacobian.
C
C       Lack of Good Progress.  SNSQE searches for a zero of the system
C         by minimizing the sum of the squares of the functions.  In so
C         doing, it can become trapped in a region where the minimum
C         does not correspond to a zero of the system and, in this situ-
C         ation, the iteration eventually fails to make good progress.
C         In particular, this will happen if the system does not have a 
C         zero.  If the system has a zero, rerunning SNSQE from a dif- 
C         ferent starting point may be helpful.
C
C
C 6. Characteristics of the Algorithm.
C
C       SNSQE is a modification of the Powell hybrid method.  Two of 
C       its main characteristics involve the choice of the correction as
C       a convex combination of the Newton and scaled gradient direc- 
C       tions, and the updating of the Jacobian by the rank-1 method of 
C       Broyden.  The choice of the correction guarantees (under reason-
C       able conditions) global convergence for starting points far from
C       the solution and a fast rate of convergence.  The Jacobian is
C       calculated at the starting point by either the user-supplied 
C       subroutine or a forward-difference approximation, but it is not
C       recalculated until the rank-1 method fails to produce satis-
C       factory progress.
C
C       Timing.  The time required by SNSQE to solve a given problem 
C         depends on N, the behavior of the functions, the accuracy
C         requested, and the starting point.  The number of arithmetic
C         operations needed by SNSQE is about 11.5*(N**2) to process
C         each evaluation of the functions (call to FCN) and 1.3*(N**3)
C         to process each evaluation of the Jacobian (call to JAC,
C         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
C         the timing of SNSQE will be strongly influenced by the time
C         spent in FCN and JAC.
C
C       Storage.  SNSQE requires (3*N**2 + 17*N)/2 single precision
C         storage locations, in addition to the storage required by the
C         program.  There are no internally declared storage arrays.
C
C
C 7. Example. 
C
C       The problem is to determine the values of X(1), X(2), ..., X(9),
C       which solve the system of tridiagonal equations
C
C       (3-2*X(1))*X(1)           -2*X(2)                   = -1
C               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
C                                   -X(8) + (3-2*X(9))*X(9) = -1
C
C       **********
C
C       PROGRAM TEST(INPUT,OUTPUT,TAPE6=OUTPUT)
C C
C C     Driver for SNSQE example.
C C
C       INTEGER J,N,IOPT,NPRINT,INFO,LWA,NWRITE
C       REAL TOL,FNORM
C       REAL X(9),FVEC(9),WA(180)
C       REAL SNRM2,R1MACH
C       EXTERNAL FCN
C       DATA NWRITE /6/
C C
C       IOPT = 2
C       N = 9
C C
C C     The following starting values provide a rough solution. 
C C
C       DO 10 J = 1, 9
C          X(J) = -1.E0
C    10    CONTINUE 
C
C       LWA = 180
C       NPRINT = 0
C C
C C     Set TOL to the square root of the machine precision.
C C     Unless high precision solutions are required,
C C     this is the recommended setting.
C C
C       TOL = SQRT(R1MACH(4)) 
C C
C       CALL SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
C       FNORM = SNRM2(N,FVEC) 
C       WRITE (NWRITE,1000) FNORM,INFO,(X(J),J=1,N)
C       STOP
C  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
C      *        5X,' EXIT PARAMETER',16X,I10 //
C      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
C       END
C       SUBROUTINE FCN(N,X,FVEC,IFLAG)
C       INTEGER N,IFLAG
C       REAL X(N),FVEC(N)
C       INTEGER K
C       REAL ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
C       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
C C
C       DO 10 K = 1, N
C          TEMP = (THREE - TWO*X(K))*X(K)
C          TEMP1 = ZERO
C          IF (K .NE. 1) TEMP1 = X(K-1) 
C          TEMP2 = ZERO
C          IF (K .NE. N) TEMP2 = X(K+1) 
C          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
C    10    CONTINUE 
C       RETURN
C       END
C
C       Results obtained with different compilers or machines
C       may be slightly different.
C
C       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
C
C       EXIT PARAMETER                         1
C
C       FINAL APPROXIMATE SOLUTION
C
C       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
C       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
C       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
C***REFERENCES  POWELL, M. J. D.
C                 A HYBRID METHOD FOR NONLINEAR EQUATIONS.
C                 NUMERICAL METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS,
C                 P. RABINOWITZ, EDITOR.  GORDON AND BREACH, 1970.
C***ROUTINES CALLED  SNSQ,XERROR
C***END PROLOGUE  SNSQE
      INTEGER IOPT,N,NPRINT,INFO,LWA
      REAL TOL
      REAL X(N),FVEC(N),WA(LWA),XDATA(NOBS)
C
C     NOTE 12/2009: NEW INTEL 11 COMPILER BALKS ON DECLARING JAC
CCCCC EXTERNAL FCN,JAC
      EXTERNAL FCN
      INTEGER INDEX,J,LR,MAXFEV,ML,MODE,MU,NFEV,NJEV
      REAL EPSFCN,FACTOR,ONE,XTOL,ZERO
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
C
      DATA FACTOR,ONE,ZERO /1.0E2,1.0E0,0.0E0/
C***FIRST EXECUTABLE STATEMENT  SNSQE
      INFO = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0
     1    .OR. TOL .LT. ZERO .OR. LWA .LT. (3*N**2 +13*N)/2)
     2   GO TO 20
C
C     CALL SNSQ.
C
      MAXFEV = 100*(N + 1)
      IF (IOPT .EQ. 2) MAXFEV = 2*MAXFEV
      XTOL = TOL
      ML = N - 1
      MU = N - 1
      EPSFCN = ZERO 
      MODE = 2
      DO 10 J = 1, N
         WA(J) = ONE
   10    CONTINUE
      LR = (N*(N + 1))/2
      INDEX=6*N+LR
      CALL SNSQ(FCN,JAC,IOPT,N,X,FVEC,WA(INDEX+1),N,XTOL,MAXFEV,ML,MU,
     1           EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
     2           WA(6*N+1),LR,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),
     3           WA(5*N+1),
     4           XDATA,NOBS)
      IF (INFO .EQ. 5) INFO = 4
   20 CONTINUE
      IF (INFO .EQ. 0) THEN
CCCCC    CALL XERROR( 'SNSQE  -- INVALID INPUT PARAMETER.'
CCCCC1,34,2,1)
        WRITE(ICOUT,11)
 11     FORMAT('***** ERROR IN SNSQE NON-LINEAR SIMULTANEOUS EQUATION ',
     1         'SOLVER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
 13     FORMAT('      INVALID INPUT PARAMETER.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
C
C     LAST CARD OF SUBROUTINE SNSQE.
C
      END 
      SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
      INTEGER N,LR
      REAL DELTA
      REAL R(LR),DIAG(N),QTB(N),X(N),WA1(N),WA2(N)
      INTEGER I,J,JJ,JP1,K,L
      REAL ALPHA,BNORM,EPSMCH,GNORM,ONE,QNORM,SGNORM,SUM,TEMP,ZERO
      REAL SNRM2
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
C
      DATA ONE,ZERO /1.0E0,0.0E0/
      EPSMCH = R1MACH(4)
      JJ = (N*(N + 1))/2 + 1
      DO 50 K = 1, N
         J = N - K + 1
         JP1 = J + 1
         JJ = JJ - K
         L = JJ + 1 
         SUM = ZERO 
         IF (N .LT. JP1) GO TO 20
         DO 10 I = JP1, N
            SUM = SUM + R(L)*X(I)
            L = L + 1
   10       CONTINUE
   20    CONTINUE
         TEMP = R(JJ)
         IF (TEMP .NE. ZERO) GO TO 40
         L = J
         DO 30 I = 1, J
            TEMP = AMAX1(TEMP,ABS(R(L)))
            L = L + N - I
   30       CONTINUE
         TEMP = EPSMCH*TEMP
         IF (TEMP .EQ. ZERO) TEMP = EPSMCH
   40    CONTINUE
         X(J) = (QTB(J) - SUM)/TEMP
   50    CONTINUE
      DO 60 J = 1, N
         WA1(J) = ZERO
         WA2(J) = DIAG(J)*X(J)
   60    CONTINUE
      QNORM = SNRM2(N,WA2,1)
      IF (QNORM .LE. DELTA) GO TO 140
      L = 1
      DO 80 J = 1, N
         TEMP = QTB(J)
         DO 70 I = J, N
            WA1(I) = WA1(I) + R(L)*TEMP 
            L = L + 1
   70       CONTINUE
         WA1(J) = WA1(J)/DIAG(J)
   80    CONTINUE
      GNORM = SNRM2(N,WA1,1)
      SGNORM = ZERO 
      ALPHA = DELTA/QNORM
      IF (GNORM .EQ. ZERO) GO TO 120
      DO 90 J = 1, N
         WA1(J) = (WA1(J)/GNORM)/DIAG(J)
   90    CONTINUE
      L = 1
      DO 110 J = 1, N
         SUM = ZERO 
         DO 100 I = J, N
            SUM = SUM + R(L)*WA1(I)
            L = L + 1
  100       CONTINUE
         WA2(J) = SUM
  110    CONTINUE
      TEMP = SNRM2(N,WA2,1)
      SGNORM = (GNORM/TEMP)/TEMP
      ALPHA = ZERO
      IF (SGNORM .GE. DELTA) GO TO 120
      BNORM = SNRM2(N,QTB,1)
      TEMP = (BNORM/GNORM)*(BNORM/QNORM)*(SGNORM/DELTA)
      TEMP = TEMP - (DELTA/QNORM)*(SGNORM/DELTA)**2
     1       + SQRT((TEMP-(DELTA/QNORM))**2
     2              +(ONE-(DELTA/QNORM)**2)*(ONE-(SGNORM/DELTA)**2))
      ALPHA = ((DELTA/QNORM)*(ONE - (SGNORM/DELTA)**2))/TEMP
  120 CONTINUE
      TEMP = (ONE - ALPHA)*AMIN1(SGNORM,DELTA)
      DO 130 J = 1, N
         X(J) = TEMP*WA1(J) + ALPHA*X(J)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END 
      SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,
     1   WA1,WA2,
     1   XDATA,NOBS)
      INTEGER N,LDFJAC,IFLAG,ML,MU
      REAL EPSFCN
      REAL X(N),FVEC(N),FJAC(LDFJAC,N),WA1(N),WA2(N)
      REAL XDATA(NOBS)
      INTEGER I,J,K,MSUM
      REAL EPS,EPSMCH,H,TEMP,ZERO
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA ZERO /0.0E0/
      EPSMCH = R1MACH(4)
      EPS = SQRT(AMAX1(EPSFCN,EPSMCH))
      MSUM = ML + MU + 1
      IF (MSUM .LT. N) GO TO 40
         DO 20 J = 1, N
            TEMP = X(J)
            H = EPS*ABS(TEMP) 
            IF (H .EQ. ZERO) H = EPS
            X(J) = TEMP + H
            CALL FCN(N,X,WA1,IFLAG,XDATA,NOBS)
            IF (IFLAG .LT. 0) GO TO 30
            X(J) = TEMP
            DO 10 I = 1, N
               FJAC(I,J) = (WA1(I) - FVEC(I))/H
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
         GO TO 110
   40 CONTINUE
         DO 90 K = 1, MSUM
            DO 60 J = K, N, MSUM
               WA2(J) = X(J)
               H = EPS*ABS(WA2(J))
               IF (H .EQ. ZERO) H = EPS 
               X(J) = WA2(J) + H
   60          CONTINUE
            CALL FCN(N,X,WA1,IFLAG,XDATA,NOBS)
            IF (IFLAG .LT. 0) GO TO 100 
            DO 80 J = K, N, MSUM
               X(J) = WA2(J)
               H = EPS*ABS(WA2(J))
               IF (H .EQ. ZERO) H = EPS 
               DO 70 I = 1, N 
                  FJAC(I,J) = ZERO
                  IF (I .GE. J - MU .AND. I .LE. J + ML)
     1               FJAC(I,J) = (WA1(I) - FVEC(I))/H
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
      RETURN
      END 
      SUBROUTINE QFORM(M,N,Q,LDQ,WA)
      INTEGER M,N,LDQ
      REAL Q(LDQ,M),WA(M)
      INTEGER I,J,JM1,K,L,MINMN,NP1
      REAL ONE,SUM,TEMP,ZERO
      DATA ONE,ZERO /1.0E0,0.0E0/
      MINMN = MIN0(M,N)
      IF (MINMN .LT. 2) GO TO 30
      DO 20 J = 2, MINMN
         JM1 = J - 1
         DO 10 I = 1, JM1
            Q(I,J) = ZERO
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
      NP1 = N + 1
      IF (M .LT. NP1) GO TO 60
      DO 50 J = NP1, M
         DO 40 I = 1, M
            Q(I,J) = ZERO
   40       CONTINUE
         Q(J,J) = ONE
   50    CONTINUE
   60 CONTINUE
      DO 120 L = 1, MINMN
         K = MINMN - L + 1
         DO 70 I = K, M
            WA(I) = Q(I,K)
            Q(I,K) = ZERO
   70       CONTINUE
         Q(K,K) = ONE
         IF (WA(K) .EQ. ZERO) GO TO 110 
         DO 100 J = K, M
            SUM = ZERO
            DO 80 I = K, M
               SUM = SUM + Q(I,J)*WA(I) 
   80          CONTINUE
            TEMP = SUM/WA(K)
            DO 90 I = K, M
               Q(I,J) = Q(I,J) - TEMP*WA(I)
   90          CONTINUE
  100       CONTINUE
  110    CONTINUE
  120    CONTINUE
      RETURN
      END 
      SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
      INTEGER M,N,LDA,LIPVT
      INTEGER IPVT(LIPVT)
      LOGICAL PIVOT 
      REAL A(LDA,N),SIGMA(N),ACNORM(N),WA(N)
      INTEGER I,J,JP1,K,KMAX,MINMN
      REAL AJNORM,EPSMCH,ONE,P05,SUM,TEMP,ZERO
      REAL SNRM2
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA ONE,P05,ZERO /1.0E0,5.0E-2,0.0E0/
      EPSMCH = R1MACH(4)
      DO 10 J = 1, N
         ACNORM(J) = SNRM2(M,A(1,J),1)
         SIGMA(J) = ACNORM(J) 
         WA(J) = SIGMA(J)
         IF (PIVOT) IPVT(J) = J
   10    CONTINUE
      MINMN = MIN0(M,N)
      DO 110 J = 1, MINMN
         IF (.NOT.PIVOT) GO TO 40
         KMAX = J
         DO 20 K = J, N
            IF (SIGMA(K) .GT. SIGMA(KMAX)) KMAX = K
   20       CONTINUE
         IF (KMAX .EQ. J) GO TO 40
         DO 30 I = 1, M
            TEMP = A(I,J)
            A(I,J) = A(I,KMAX)
            A(I,KMAX) = TEMP
   30       CONTINUE
         SIGMA(KMAX) = SIGMA(J)
         WA(KMAX) = WA(J)
         K = IPVT(J)
         IPVT(J) = IPVT(KMAX) 
         IPVT(KMAX) = K
   40    CONTINUE
         AJNORM = SNRM2(M-J+1,A(J,J),1)
         IF (AJNORM .EQ. ZERO) GO TO 100
         IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM
         DO 50 I = J, M
            A(I,J) = A(I,J)/AJNORM
   50       CONTINUE
         A(J,J) = A(J,J) + ONE
         JP1 = J + 1
         IF (N .LT. JP1) GO TO 100
         DO 90 K = JP1, N
            SUM = ZERO
            DO 60 I = J, M
               SUM = SUM + A(I,J)*A(I,K)
   60          CONTINUE
            TEMP = SUM/A(J,J) 
            DO 70 I = J, M
               A(I,K) = A(I,K) - TEMP*A(I,J)
   70          CONTINUE
            IF (.NOT.PIVOT .OR. SIGMA(K) .EQ. ZERO) GO TO 80
            TEMP = A(J,K)/SIGMA(K)
            SIGMA(K) = SIGMA(K)*SQRT(AMAX1(ZERO,ONE-TEMP**2))
            IF (P05*(SIGMA(K)/WA(K))**2 .GT. EPSMCH) GO TO 80
            SIGMA(K) = SNRM2(M-J,A(JP1,K),1)
            WA(K) = SIGMA(K)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
         SIGMA(J) = -AJNORM
  110    CONTINUE
      RETURN
      END 
      SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)
      INTEGER M,N,LDA
      REAL A(LDA,N),V(N),W(N) 
      INTEGER I,J,NMJ,NM1
      REAL COS,ONE,SIN,TEMP
      DATA ONE /1.0E0/
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 50
      DO 20 NMJ = 1, NM1
         J = N - NMJ
         IF (ABS(V(J)) .GT. ONE) COS = ONE/V(J)
         IF (ABS(V(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
         IF (ABS(V(J)) .LE. ONE) SIN = V(J)
         IF (ABS(V(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
         DO 10 I = 1, M
            TEMP = COS*A(I,J) - SIN*A(I,N)
            A(I,N) = SIN*A(I,J) + COS*A(I,N)
            A(I,J) = TEMP
   10       CONTINUE
   20    CONTINUE
      DO 40 J = 1, NM1
         IF (ABS(W(J)) .GT. ONE) COS = ONE/W(J)
         IF (ABS(W(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
         IF (ABS(W(J)) .LE. ONE) SIN = W(J)
         IF (ABS(W(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
         DO 30 I = 1, M
            TEMP = COS*A(I,J) + SIN*A(I,N)
            A(I,N) = -SIN*A(I,J) + COS*A(I,N)
            A(I,J) = TEMP
   30       CONTINUE
   40    CONTINUE
   50 CONTINUE
      RETURN
      END 
      SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
      INTEGER M,N,LS
      LOGICAL SING
      REAL S(LS),U(M),V(N),W(M)
      INTEGER I,J,JJ,L,NMJ,NM1
      REAL COS,COTAN,GIANT,ONE,P5,P25,SIN,TAN,TAU,TEMP,ZERO 
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
      DATA ONE,P5,P25,ZERO /1.0E0,5.0E-1,2.5E-1,0.0E0/
      GIANT = R1MACH(2)
      JJ = (N*(2*M - N + 1))/2 - (M - N)
      L = JJ
      DO 10 I = N, M
         W(I) = S(L)
         L = L + 1
   10    CONTINUE
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 NMJ = 1, NM1
         J = N - NMJ
         JJ = JJ - (M - J + 1)
         W(J) = ZERO
         IF (V(J) .EQ. ZERO) GO TO 50
         IF (ABS(V(N)) .GE. ABS(V(J))) GO TO 20
            COTAN = V(N)/V(J) 
            SIN = P5/SQRT(P25+P25*COTAN**2)
            COS = SIN*COTAN
            TAU = ONE
            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
            GO TO 30
   20    CONTINUE
            TAN = V(J)/V(N)
            COS = P5/SQRT(P25+P25*TAN**2)
            SIN = COS*TAN
            TAU = SIN
   30    CONTINUE
         V(N) = SIN*V(J) + COS*V(N)
         V(J) = TAU 
         L = JJ
         DO 40 I = J, M
            TEMP = COS*S(L) - SIN*W(I)
            W(I) = SIN*S(L) + COS*W(I)
            S(L) = TEMP
            L = L + 1
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
      DO 80 I = 1, M
         W(I) = W(I) + V(N)*U(I)
   80    CONTINUE
      SING = .FALSE.
      IF (NM1 .LT. 1) GO TO 140
      DO 130 J = 1, NM1
         IF (W(J) .EQ. ZERO) GO TO 120
         IF (ABS(S(JJ)) .GE. ABS(W(J))) GO TO 90
            COTAN = S(JJ)/W(J)
            SIN = P5/SQRT(P25+P25*COTAN**2)
            COS = SIN*COTAN
            TAU = ONE
            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
            GO TO 100
   90    CONTINUE
            TAN = W(J)/S(JJ)
            COS = P5/SQRT(P25+P25*TAN**2)
            SIN = COS*TAN
            TAU = SIN
  100    CONTINUE
         L = JJ
         DO 110 I = J, M
            TEMP = COS*S(L) + SIN*W(I)
            W(I) = -SIN*S(L) + COS*W(I) 
            S(L) = TEMP
            L = L + 1
  110       CONTINUE
         W(J) = TAU 
  120    CONTINUE
         IF (S(JJ) .EQ. ZERO) SING = .TRUE.
         JJ = JJ + (M - J + 1)
  130    CONTINUE
  140 CONTINUE
      L = JJ
      DO 150 I = N, M
         S(L) = W(I)
         L = L + 1
  150    CONTINUE
      IF (S(JJ) .EQ. ZERO) SING = .TRUE.
      RETURN
      END 
      SUBROUTINE SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,
     1   MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,R,LR,QTF,WA1,
     2   WA2,WA3,WA4,
     3   XDATA,NOBS)
      INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,NJEV
      REAL XTOL,EPSFCN,FACTOR 
      REAL X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),WA1(N),
     1     WA2(N),WA3(N),WA4(N)
      REAL XDATA(NOBS)
      EXTERNAL FCN
      INTEGER I,IFLAG,ITER,J,JM1,L,NCFAIL,NCSUC,NSLOW1,NSLOW2
      INTEGER IWA(1)
      LOGICAL JEVAL,SING
      REAL ACTRED,DELTA,EPSMCH,FNORM,FNORM1,ONE,PNORM,PRERED,P1,P5,
     1     P001,P0001,RATIO,SUM,TEMP,XNORM,ZERO
      REAL SNRM2
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
      DATA ONE,P1,P5,P001,P0001,ZERO
     1     /1.0E0,1.0E-1,5.0E-1,1.0E-3,1.0E-4,0.0E0/
      EPSMCH = R1MACH(4)
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
      IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR.
     1    N .LE. 0 .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0
     2    .OR. ML .LT. 0 .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO
     3    .OR. LDFJAC .LT. N .OR. LR .LT. (N*(N + 1))/2) GO TO 300
      IF (MODE .NE. 2) GO TO 20
      DO 10 J = 1, N
         IF (DIAG(J) .LE. ZERO) GO TO 300
   10    CONTINUE
   20 CONTINUE
      IFLAG = 1
      CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
      NFEV = 1
      IF (IFLAG .LT. 0) GO TO 300
      FNORM = SNRM2(N,FVEC,1)
      ITER = 1
      NCSUC = 0
      NCFAIL = 0
      NSLOW1 = 0
      NSLOW2 = 0
   30 CONTINUE
         JEVAL = .TRUE.
         IF (IOPT .EQ. 2) GO TO 31
CCCCC       CALL JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
            NJEV = NJEV+1
            GO TO 32
   31       IFLAG = 2
            CALL FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,WA1,
     1               WA2,
     2               XDATA,NOBS)
            NFEV = NFEV + MIN0(ML+MU+1,N)
   32    IF (IFLAG .LT. 0) GO TO 300
         CALL QRFAC(N,N,FJAC,LDFJAC,.FALSE.,IWA,1,WA1,WA2,WA3)
         IF (ITER .NE. 1) GO TO 70
         IF (MODE .EQ. 2) GO TO 50
         DO 40 J = 1, N
            DIAG(J) = WA2(J)
            IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
   40       CONTINUE
   50    CONTINUE
         DO 60 J = 1, N
            WA3(J) = DIAG(J)*X(J)
   60       CONTINUE
         XNORM = SNRM2(N,WA3,1) 
         DELTA = FACTOR*XNORM 
         IF (DELTA .EQ. ZERO) DELTA = FACTOR
   70    CONTINUE
         DO 80 I = 1, N
            QTF(I) = FVEC(I)
   80       CONTINUE
         DO 120 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) GO TO 110
            SUM = ZERO
            DO 90 I = J, N
               SUM = SUM + FJAC(I,J)*QTF(I)
   90          CONTINUE
            TEMP = -SUM/FJAC(J,J)
            DO 100 I = J, N
               QTF(I) = QTF(I) + FJAC(I,J)*TEMP
  100          CONTINUE
  110       CONTINUE
  120       CONTINUE
         SING = .FALSE.
         DO 150 J = 1, N
            L = J
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 140
            DO 130 I = 1, JM1 
               R(L) = FJAC(I,J)
               L = L + N - I
  130          CONTINUE
  140       CONTINUE
            R(L) = WA1(J)
            IF (WA1(J) .EQ. ZERO) SING = .TRUE.
  150       CONTINUE
         CALL QFORM(N,N,FJAC,LDFJAC,WA1)
         IF (MODE .EQ. 2) GO TO 170
         DO 160 J = 1, N
            DIAG(J) = AMAX1(DIAG(J),WA2(J))
  160       CONTINUE
  170    CONTINUE
  180    CONTINUE
            IF (NPRINT .LE. 0) GO TO 190
            IFLAG = 0
            IF (MOD(ITER-1,NPRINT) .EQ. 0)
     1         CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
            IF (IFLAG .LT. 0) GO TO 300 
  190       CONTINUE
            CALL DOGLEG(N,R,LR,DIAG,QTF,DELTA,WA1,WA2,WA3)
            DO 200 J = 1, N
               WA1(J) = -WA1(J)
               WA2(J) = X(J) + WA1(J)
               WA3(J) = DIAG(J)*WA1(J)
  200          CONTINUE
            PNORM = SNRM2(N,WA3,1)
            IF (ITER .EQ. 1) DELTA = AMIN1(DELTA,PNORM)
            IFLAG = 1
            CALL FCN(N,WA2,WA4,IFLAG,XDATA,NOBS)
            NFEV = NFEV + 1
            IF (IFLAG .LT. 0) GO TO 300 
            FNORM1 = SNRM2(N,WA4,1)
            ACTRED = -ONE
            IF (FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
            L = 1
            DO 220 I = 1, N
               SUM = ZERO
               DO 210 J = I, N
                  SUM = SUM + R(L)*WA1(J)
                  L = L + 1
  210             CONTINUE
               WA3(I) = QTF(I) + SUM
  220          CONTINUE
            TEMP = SNRM2(N,WA3,1)
            PRERED = ZERO
            IF (TEMP .LT. FNORM) PRERED = ONE - (TEMP/FNORM)**2
            RATIO = ZERO
            IF (PRERED .GT. ZERO) RATIO = ACTRED/PRERED
            IF (RATIO .GE. P1) GO TO 230
               NCSUC = 0
               NCFAIL = NCFAIL + 1
               DELTA = P5*DELTA
               GO TO 240
  230       CONTINUE
               NCFAIL = 0
               NCSUC = NCSUC + 1
               IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)
     1            DELTA = AMAX1(DELTA,PNORM/P5)
               IF (ABS(RATIO-ONE) .LE. P1) DELTA = PNORM/P5 
  240       CONTINUE
            IF (RATIO .LT. P0001) GO TO 260
            DO 250 J = 1, N
               X(J) = WA2(J)
               WA2(J) = DIAG(J)*X(J)
               FVEC(J) = WA4(J)
  250          CONTINUE
            XNORM = SNRM2(N,WA2,1)
            FNORM = FNORM1
            ITER = ITER + 1
  260       CONTINUE
            NSLOW1 = NSLOW1 + 1
            IF (ACTRED .GE. P001) NSLOW1 = 0
            IF (JEVAL) NSLOW2 = NSLOW2 + 1
            IF (ACTRED .GE. P1) NSLOW2 = 0
            IF (DELTA .LE. XTOL*XNORM .OR. FNORM .EQ. ZERO) INFO = 1
            IF (INFO .NE. 0) GO TO 300
            IF (NFEV .GE. MAXFEV) INFO = 2
            IF (P1*AMAX1(P1*DELTA,PNORM) .LE. EPSMCH*XNORM) INFO = 3
            IF (NSLOW2 .EQ. 5) INFO = 4 
            IF (NSLOW1 .EQ. 10) INFO = 5
            IF (INFO .NE. 0) GO TO 300
            IF (NCFAIL .EQ. 2) GO TO 290
            DO 280 J = 1, N
               SUM = ZERO
               DO 270 I = 1, N
                  SUM = SUM + FJAC(I,J)*WA4(I)
  270             CONTINUE
               WA2(J) = (SUM - WA3(J))/PNORM
               WA1(J) = DIAG(J)*((DIAG(J)*WA1(J))/PNORM)
               IF (RATIO .GE. P0001) QTF(J) = SUM 
  280          CONTINUE
            CALL R1UPDT(N,N,R,LR,WA1,WA2,WA3,SING)
            CALL R1MPYQ(N,N,FJAC,LDFJAC,WA2,WA3)
            CALL R1MPYQ(1,N,QTF,1,WA2,WA3)
            JEVAL = .FALSE.
            GO TO 180
  290    CONTINUE
         GO TO 30
  300 CONTINUE
      IF (IFLAG .LT. 0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT .GT. 0) CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS) 
C
C  ERROR SECTION
C
      IF (INFO .LT. 0) THEN
CCCCC   CALL XERROR( 'SNSQ   -- EXECUTION TERMINATED BECA
CCCCC1USE USER SET IFLAG NEGATIVE.',63,1,1)
        WRITE(ICOUT,1001)
 1001   FORMAT('***** ERROR IN SNSQE NON-LINEAR SIMULTANEOUS EQUATION ',
     1         'SOLVER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1003)
 1003   FORMAT('      TERMINATION HALTED BECAUSE IFLAG IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 0) THEN
CCCCC   CALL XERROR( 'SNSQ   -- INVALID INPUT PARAMETER.',34,2,1)
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1004)
 1004   FORMAT('      INVALID INPUT PARAMETER.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 2) THEN
CCCCC   CALL XERROR( 'SNSQ   -- TOO MANY FUNCTION EVALUATIONS.',40,9,1)
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1005)
 1005   FORMAT('      TOO MANY FUNCTION EVALUATIONS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 3) THEN
CCCCC   CALL XERROR( 'SNSQ   -- XTOL TOO SMALL. NO FURTHE
CCCCC1R IMPROVEMENT POSSIBLE.',58,3,1)
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1006)
 1006   FORMAT('      XTOL TOO SMALL.  NO FURTHER IMPROVEMENT ',
     1         'POSSIBLE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .GT. 4) THEN
CCCCC   CALL XERROR( 'SNSQ   -- ITERATION NOT MAKING GOOD
CCCCC1 PROGRESS.',45,1,1)
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1007)
 1007   FORMAT('      ITERATION NOT MAKING GOOD PROGRESS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END 
*DECK DNSQE
      SUBROUTINE DNSQE (FCN, JAC, IOPT, N, X, FVEC, TOL, NPRINT, INFO,
     +   WA, LWA,
     +   XDATA,NOBS)
C***BEGIN PROLOGUE  DNSQE
C***PURPOSE  An easy-to-use code to find a zero of a system of N
C            nonlinear functions in N variables by a modification of
C            the Powell hybrid method.
C***LIBRARY   SLATEC
C***CATEGORY  F2A
C***TYPE      DOUBLE PRECISION (SNSQE-S, DNSQE-D)
C***KEYWORDS  EASY-TO-USE, NONLINEAR SQUARE SYSTEM,
C             POWELL HYBRID METHOD, ZEROS
C***AUTHOR  Hiebert, K. L. (SNLA)
C***DESCRIPTION
C
C 1. Purpose.
C
C       The purpose of DNSQE is to find a zero of a system of N
C       nonlinear functions in N variables by a modification of the
C       Powell hybrid method.  This is done by using the more general
C       nonlinear equation solver DNSQ.  The user must provide a
C       subroutine which calculates the functions.  The user has the
C       option of either to provide a subroutine which calculates the
C       Jacobian or to let the code calculate it by a forward-difference
C       approximation.  This code is the combination of the MINPACK
C       codes (Argonne) HYBRD1 and HYBRJ1.
C
C 2. Subroutine and Type Statements.
C
C       SUBROUTINE DNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,
C      *                  WA,LWA)
C       INTEGER IOPT,N,NPRINT,INFO,LWA
C       DOUBLE PRECISION TOL
C       DOUBLE PRECISION X(N),FVEC(N),WA(LWA)
C       EXTERNAL FCN,JAC
C
C 3. Parameters.
C
C       Parameters designated as input parameters must be specified on
C       entry to DNSQE and are not changed on exit, while parameters
C       designated as output parameters need not be specified on entry
C       and are set to appropriate values on exit from DNSQE.
C
C       FCN is the name of the user-supplied subroutine which calculates
C         the functions.  FCN must be declared in an external statement
C         in the user calling program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         DOUBLE PRECISION X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of DNSQE.  In this case set
C         IFLAG to a negative integer.
C
C       JAC is the name of the user-supplied subroutine which calculates
C         the Jacobian.  If IOPT=1, then JAC must be declared in an
C         external statement in the user calling program, and should be
C         written as follows.
C
C         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C         INTEGER N,LDFJAC,IFLAG
C         DOUBLE PRECISION X(N),FVEC(N),FJAC(LDFJAC,N)
C         ----------
C         Calculate the Jacobian at X and return this
C         matrix in FJAC.  FVEC contains the function
C         values at X and should not be altered.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by JAC unless the
C         user wants to terminate execution of DNSQE. In this case set
C         IFLAG to a negative integer.
C
C         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
C
C       IOPT is an input variable which specifies how the Jacobian will
C         be calculated.  If IOPT=1, then the user must supply the
C         Jacobian through the subroutine JAC.  If IOPT=2, then the
C         code will approximate the Jacobian by forward-differencing.
C
C       N is a positive integer input variable set to the number of
C         functions and variables.
C
C       X is an array of length N.  On input X must contain an initial
C         estimate of the solution vector.  On output X contains the
C         final estimate of the solution vector.
C
C       FVEC is an output array of length N which contains the functions
C         evaluated at the output X.
C
C       TOL is a nonnegative input variable.  Termination occurs when
C         the algorithm estimates that the relative error between X and
C         the solution is at most TOL.  Section 4 contains more details
C         about TOL.
C
C       NPRINT is an integer input variable that enables controlled
C         printing of iterates if it is positive.  In this case, FCN is
C         called with IFLAG = 0 at the beginning of the first iteration
C         and every NPRINT iterations thereafter and immediately prior
C         to return, with X and FVEC available for printing. Appropriate
C         print statements must be added to FCN(see example).  If NPRINT
C         is not positive, no special calls of FCN with IFLAG = 0 are
C         made.
C
C       INFO is an integer output variable.  If the user has terminated
C         execution, INFO is set to the (negative) value of IFLAG.  See
C         description of FCN and JAC. Otherwise, INFO is set as follows.
C
C         INFO = 0  Improper input parameters.
C
C         INFO = 1  Algorithm estimates that the relative error between
C                   X and the solution is at most TOL.
C
C         INFO = 2  Number of calls to FCN has reached or exceeded
C                   100*(N+1) for IOPT=1 or 200*(N+1) for IOPT=2.
C
C         INFO = 3  TOL is too small.  No further improvement in the
C                   approximate solution X is possible.
C
C         INFO = 4  Iteration is not making good progress.
C
C         Sections 4 and 5 contain more details about INFO.
C
C       WA is a work array of length LWA.
C
C       LWA is a positive integer input variable not less than
C         (3*N**2+13*N))/2.
C
C 4. Successful Completion.
C
C       The accuracy of DNSQE is controlled by the convergence parameter
C       TOL.  This parameter is used in a test which makes a comparison
C       between the approximation X and a solution XSOL.  DNSQE
C       terminates when the test is satisfied.  If TOL is less than the
C       machine precision (as defined by the  function D1MACH(4)), then
C       DNSQE only attempts to satisfy the test defined by the machine
C       precision.  Further progress is not usually possible.  Unless
C       high precision solutions are required, the recommended value
C       for TOL is the square root of the machine precision.
C
C       The test assumes that the functions are reasonably well behaved,
C       and, if the Jacobian is supplied by the user, that the functions
C       and the Jacobian are coded consistently. If these conditions are
C       not satisfied, then DNSQE may incorrectly indicate convergence.
C       The coding of the Jacobian can be checked by the subroutine
C       DCKDER.  If the Jacobian is coded correctly or IOPT=2, then
C       the validity of the answer can be checked, for example, by
C       rerunning DNSQE with a tighter tolerance.
C
C       Convergence Test.  If DENORM(Z) denotes the Euclidean norm of a
C         vector Z, then this test attempts to guarantee that
C
C               DENORM(X-XSOL) .LE. TOL*DENORM(XSOL).
C
C         If this condition is satisfied with TOL = 10**(-K), then the
C         larger components of X have K significant decimal digits and
C         INFO is set to 1.  There is a danger that the smaller
C         components of X may have large relative errors, but the fast
C         rate of convergence of DNSQE usually avoids this possibility.
C
C 5. Unsuccessful Completion.
C
C       Unsuccessful termination of DNSQE can be due to improper input
C       parameters, arithmetic interrupts, an excessive number of
C       function evaluations, errors in the functions, or lack of good
C       progress.
C
C       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1, or
C         IOPT .GT. 2, or N .LE. 0, or TOL .LT. 0.E0, or
C         LWA .LT. (3*N**2+13*N)/2.
C
C       Arithmetic Interrupts.  If these interrupts occur in the FCN
C         subroutine during an early stage of the computation, they may
C         be caused by an unacceptable choice of X by DNSQE.  In this
C         case, it may be possible to remedy the situation by not
C         evaluating the functions here, but instead setting the
C         components of FVEC to numbers that exceed those in the initial
C         FVEC.
C
C       Excessive Number of Function Evaluations.  If the number of
C         calls to FCN reaches 100*(N+1) for IOPT=1 or 200*(N+1) for
C         IOPT=2, then this indicates that the routine is converging
C         very slowly as measured by the progress of FVEC, and INFO is
C         set to 2.  This situation should be unusual because, as
C         indicated below, lack of good progress is usually diagnosed
C         earlier by DNSQE, causing termination with INFO = 4.
C
C       Errors In the Functions.  When IOPT=2, the choice of step length
C         in the forward-difference approximation to the Jacobian
C         assumes that the relative errors in the functions are of the
C         order of the machine precision.  If this is not the case,
C         DNSQE may fail (usually with INFO = 4).  The user should
C         then either use DNSQ and set the step length or use IOPT=1
C         and supply the Jacobian.
C
C       Lack of Good Progress.  DNSQE searches for a zero of the system
C         by minimizing the sum of the squares of the functions.  In so
C         doing, it can become trapped in a region where the minimum
C         does not correspond to a zero of the system and, in this
C         situation, the iteration eventually fails to make good
C         progress.  In particular, this will happen if the system does
C         not have a zero.  If the system has a zero, rerunning DNSQE
C         from a different starting point may be helpful.
C
C 6. Characteristics of The Algorithm.
C
C       DNSQE is a modification of the Powell Hybrid method.  Two of
C       its main characteristics involve the choice of the correction as
C       a convex combination of the Newton and scaled gradient
C       directions, and the updating of the Jacobian by the rank-1
C       method of Broyden.  The choice of the correction guarantees
C       (under reasonable conditions) global convergence for starting
C       points far from the solution and a fast rate of convergence.
C       The Jacobian is calculated at the starting point by either the
C       user-supplied subroutine or a forward-difference approximation,
C       but it is not recalculated until the rank-1 method fails to
C       produce satisfactory progress.
C
C       Timing.  The time required by DNSQE to solve a given problem
C         depends on N, the behavior of the functions, the accuracy
C         requested, and the starting point.  The number of arithmetic
C         operations needed by DNSQE is about 11.5*(N**2) to process
C         each evaluation of the functions (call to FCN) and 1.3*(N**3)
C         to process each evaluation of the Jacobian (call to JAC,
C         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
C         the timing of DNSQE will be strongly influenced by the time
C         spent in FCN and JAC.
C
C       Storage.  DNSQE requires (3*N**2 + 17*N)/2 single precision
C         storage locations, in addition to the storage required by the
C         program.  There are no internally declared storage arrays.
C
C *Long Description:
C
C 7. Example.
C
C       The problem is to determine the values of X(1), X(2), ..., X(9),
C       which solve the system of tridiagonal equations
C
C       (3-2*X(1))*X(1)           -2*X(2)                   = -1
C               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
C                                   -X(8) + (3-2*X(9))*X(9) = -1
C
C       **********
C
C       PROGRAM TEST
C C
C C     DRIVER FOR DNSQE EXAMPLE.
C C
C       INTEGER J,N,IOPT,NPRINT,INFO,LWA,NWRITE
C       DOUBLE PRECISION TOL,FNORM
C       DOUBLE PRECISION X(9),FVEC(9),WA(180)
C       DOUBLE PRECISION DENORM,D1MACH
C       EXTERNAL FCN
C       DATA NWRITE /6/
C C
C       IOPT = 2
C       N = 9
C C
C C     THE FOLLOWING STARTING VALUES PROVIDE A ROUGH SOLUTION.
C C
C       DO 10 J = 1, 9
C          X(J) = -1.E0
C    10    CONTINUE
C
C       LWA = 180
C       NPRINT = 0
C C
C C     SET TOL TO THE SQUARE ROOT OF THE MACHINE PRECISION.
C C     UNLESS HIGH PRECISION SOLUTIONS ARE REQUIRED,
C C     THIS IS THE RECOMMENDED SETTING.
C C
C       TOL = SQRT(D1MACH(4))
C C
C       CALL DNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
C       FNORM = DENORM(N,FVEC)
C       WRITE (NWRITE,1000) FNORM,INFO,(X(J),J=1,N)
C       STOP
C  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
C      *        5X,' EXIT PARAMETER',16X,I10 //
C      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
C       END
C       SUBROUTINE FCN(N,X,FVEC,IFLAG)
C       INTEGER N,IFLAG
C       DOUBLE PRECISION X(N),FVEC(N)
C       INTEGER K
C       DOUBLE PRECISION ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
C       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
C C
C       DO 10 K = 1, N
C          TEMP = (THREE - TWO*X(K))*X(K)
C          TEMP1 = ZERO
C          IF (K .NE. 1) TEMP1 = X(K-1)
C          TEMP2 = ZERO
C          IF (K .NE. N) TEMP2 = X(K+1)
C          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
C    10    CONTINUE
C       RETURN
C       END
C
C       RESULTS OBTAINED WITH DIFFERENT COMPILERS OR MACHINES
C       MAY BE SLIGHTLY DIFFERENT.
C
C       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
C
C       EXIT PARAMETER                         1
C
C       FINAL APPROXIMATE SOLUTION
C
C       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
C       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
C       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
C
C***REFERENCES  M. J. D. Powell, A hybrid method for nonlinear equa-
C                 tions. In Numerical Methods for Nonlinear Algebraic
C                 Equations, P. Rabinowitz, Editor.  Gordon and Breach,
C                 1988.
C***ROUTINES CALLED  DNSQ, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DNSQE
      INTEGER INDEX, INFO, IOPT, J, LR, LWA, MAXFEV, ML, MODE, MU, N,
     1     NFEV, NJEV, NPRINT
      DOUBLE PRECISION EPSFCN, FACTOR, FVEC(*), ONE, TOL, WA(*),
     1     X(*), XTOL, ZERO
      REAL XDATA(NOBS)
CCCCC EXTERNAL FCN, JAC
      EXTERNAL FCN
      SAVE FACTOR, ONE, ZERO
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
      DATA FACTOR,ONE,ZERO /1.0D2,1.0D0,0.0D0/
C     BEGIN BLOCK PERMITTING ...EXITS TO 20
C***FIRST EXECUTABLE STATEMENT  DNSQE
         INFO = 0
C
C        CHECK THE INPUT PARAMETERS FOR ERRORS.
C
C     ...EXIT
         IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0
     1       .OR. TOL .LT. ZERO .OR. LWA .LT. (3*N**2 + 13*N)/2)
     2      GO TO 20
C
C        CALL DNSQ.
C
         MAXFEV = 100*(N + 1)
         IF (IOPT .EQ. 2) MAXFEV = 2*MAXFEV
         XTOL = TOL
         ML = N - 1
         MU = N - 1
         EPSFCN = ZERO
         MODE = 2
         DO 10 J = 1, N
            WA(J) = ONE
   10    CONTINUE
         LR = (N*(N + 1))/2
         INDEX = 6*N + LR
         CALL DNSQ(FCN,JAC,IOPT,N,X,FVEC,WA(INDEX+1),N,XTOL,MAXFEV,ML,
     1             MU,EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
     2             WA(6*N+1),LR,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),
     3             WA(5*N+1),
     4             XDATA,NOBS)
         IF (INFO .EQ. 5) INFO = 4
   20 CONTINUE
CCCCC IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNSQE',
CCCCC+   'INVALID INPUT PARAMETER.', 2, 1)
      IF (INFO .EQ. 0) THEN
        WRITE(ICOUT,11)
 11     FORMAT('***** ERROR IN DNSQE NON-LINEAR SIMULTANEOUS EQUATION ',
     1         'SOLVER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
 13     FORMAT('      INVALID INPUT PARAMETER.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
C
C     LAST CARD OF SUBROUTINE DNSQE.
C
      END
*DECK DNSQ
      SUBROUTINE DNSQ (FCN, JAC, IOPT, N, X, FVEC, FJAC, LDFJAC, XTOL,
     +   MAXFEV, ML, MU, EPSFCN, DIAG, MODE, FACTOR, NPRINT, INFO, NFEV,
     +   NJEV, R, LR, QTF, WA1, WA2, WA3, WA4,
     +   XDATA,NOBS)
C***BEGIN PROLOGUE  DNSQ
C***PURPOSE  Find a zero of a system of a N nonlinear functions in N
C            variables by a modification of the Powell hybrid method.
C***LIBRARY   SLATEC
C***CATEGORY  F2A
C***TYPE      DOUBLE PRECISION (SNSQ-S, DNSQ-D)
C***KEYWORDS  NONLINEAR SQUARE SYSTEM, POWELL HYBRID METHOD, ZEROS
C***AUTHOR  Hiebert, K. L. (SNLA)
C***DESCRIPTION
C
C 1. Purpose.
C
C       The purpose of DNSQ is to find a zero of a system of N nonlinear
C       functions in N variables by a modification of the Powell
C       hybrid method.  The user must provide a subroutine which
C       calculates the functions.  The user has the option of either to
C       provide a subroutine which calculates the Jacobian or to let the
C       code calculate it by a forward-difference approximation.
C       This code is the combination of the MINPACK codes (Argonne)
C       HYBRD and HYBRDJ.
C
C 2. Subroutine and Type Statements.
C
C       SUBROUTINE DNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,
C      *                 ML,MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,
C      *                 NJEV,R,LR,QTF,WA1,WA2,WA3,WA4)
C       INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,NJEV,LR
C       DOUBLE PRECISION XTOL,EPSFCN,FACTOR
C       DOUBLE PRECISION
C       X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),
C      *     WA1(N),WA2(N),WA3(N),WA4(N)
C       EXTERNAL FCN,JAC
C
C 3. Parameters.
C
C       Parameters designated as input parameters must be specified on
C       entry to DNSQ and are not changed on exit, while parameters
C       designated as output parameters need not be specified on entry
C       and are set to appropriate values on exit from DNSQ.
C
C       FCN is the name of the user-supplied subroutine which calculates
C         the functions.  FCN must be declared in an EXTERNAL statement
C         in the user calling program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         DOUBLE PRECISION X(N),FVEC(N)
C         ----------
C         CALCULATE THE FUNCTIONS AT X AND
C         RETURN THIS VECTOR IN FVEC.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by FCN unless the
C         user wants to terminate execution of DNSQ.  In this case set
C         IFLAG to a negative integer.
C
C       JAC is the name of the user-supplied subroutine which calculates
C         the Jacobian.  If IOPT=1, then JAC must be declared in an
C         EXTERNAL statement in the user calling program, and should be
C         written as follows.
C
C         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C         INTEGER N,LDFJAC,IFLAG
C         DOUBLE PRECISION X(N),FVEC(N),FJAC(LDFJAC,N)
C         ----------
C         Calculate the Jacobian at X and return this
C         matrix in FJAC.  FVEC contains the function
C         values at X and should not be altered.
C         ----------
C         RETURN
C         END
C
C         The value of IFLAG should not be changed by JAC unless the
C         user wants to terminate execution of DNSQ.  In this case set
C         IFLAG to a negative integer.
C
C         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
C
C       IOPT is an input variable which specifies how the Jacobian will
C         be calculated.  If IOPT=1, then the user must supply the
C         Jacobian through the subroutine JAC.  If IOPT=2, then the
C         code will approximate the Jacobian by forward-differencing.
C
C       N is a positive integer input variable set to the number of
C         functions and variables.
C
C       X is an array of length N.  On input X must contain an initial
C         estimate of the solution vector.  On output X contains the
C         final estimate of the solution vector.
C
C       FVEC is an output array of length N which contains the functions
C         evaluated at the output X.
C
C       FJAC is an output N by N array which contains the orthogonal
C         matrix Q produced by the QR factorization of the final
C         approximate Jacobian.
C
C       LDFJAC is a positive integer input variable not less than N
C         which specifies the leading dimension of the array FJAC.
C
C       XTOL is a nonnegative input variable.  Termination occurs when
C         the relative error between two consecutive iterates is at most
C         XTOL.  Therefore, XTOL measures the relative error desired in
C         the approximate solution.  Section 4 contains more details
C         about XTOL.
C
C       MAXFEV is a positive integer input variable.  Termination occurs
C         when the number of calls to FCN is at least MAXFEV by the end
C         of an iteration.
C
C       ML is a nonnegative integer input variable which specifies the
C         number of subdiagonals within the band of the Jacobian matrix.
C         If the Jacobian is not banded or IOPT=1, set ML to at
C         least N - 1.
C
C       MU is a nonnegative integer input variable which specifies the
C         number of superdiagonals within the band of the Jacobian
C         matrix.  If the Jacobian is not banded or IOPT=1, set MU to at
C         least N - 1.
C
C       EPSFCN is an input variable used in determining a suitable step
C         for the forward-difference approximation.  This approximation
C         assumes that the relative errors in the functions are of the
C         order of EPSFCN.  If EPSFCN is less than the machine
C         precision, it is assumed that the relative errors in the
C         functions are of the order of the machine precision.  If
C         IOPT=1, then EPSFCN can be ignored (treat it as a dummy
C         argument).
C
C       DIAG is an array of length N.  If MODE = 1 (see below), DIAG is
C         internally set.  If MODE = 2, DIAG must contain positive
C         entries that serve as implicit (multiplicative) scale factors
C         for the variables.
C
C       MODE is an integer input variable.  If MODE = 1, the variables
C         will be scaled internally.  If MODE = 2, the scaling is
C         specified by the input DIAG.  Other values of MODE are
C         equivalent to MODE = 1.
C
C       FACTOR is a positive input variable used in determining the
C         initial step bound.  This bound is set to the product of
C         FACTOR and the Euclidean norm of DIAG*X if nonzero, or else to
C         FACTOR itself.  In most cases FACTOR should lie in the
C         interval (.1,100.).  100. is a generally recommended value.
C
C       NPRINT is an integer input variable that enables controlled
C         printing of iterates if it is positive.  In this case, FCN is
C         called with IFLAG = 0 at the beginning of the first iteration
C         and every NPRINT iterations thereafter and immediately prior
C         to return, with X and FVEC available for printing. appropriate
C         print statements must be added to FCN(see example).  If NPRINT
C         is not positive, no special calls of FCN with IFLAG = 0 are
C         made.
C
C       INFO is an integer output variable.  If the user has terminated
C         execution, INFO is set to the (negative) value of IFLAG.  See
C         description of FCN and JAC. Otherwise, INFO is set as follows.
C
C         INFO = 0  Improper input parameters.
C
C         INFO = 1  Relative error between two consecutive iterates is
C                   at most XTOL.
C
C         INFO = 2  Number of calls to FCN has reached or exceeded
C                   MAXFEV.
C
C         INFO = 3  XTOL is too small.  No further improvement in the
C                   approximate solution X is possible.
C
C         INFO = 4  Iteration is not making good progress, as measured
C                   by the improvement from the last five Jacobian
C                   evaluations.
C
C         INFO = 5  Iteration is not making good progress, as measured
C                   by the improvement from the last ten iterations.
C
C         Sections 4 and 5 contain more details about INFO.
C
C       NFEV is an integer output variable set to the number of calls to
C         FCN.
C
C       NJEV is an integer output variable set to the number of calls to
C         JAC. (If IOPT=2, then NJEV is set to zero.)
C
C       R is an output array of length LR which contains the upper
C         triangular matrix produced by the QR factorization of the
C         final approximate Jacobian, stored rowwise.
C
C       LR is a positive integer input variable not less than
C         (N*(N+1))/2.
C
C       QTF is an output array of length N which contains the vector
C         (Q transpose)*FVEC.
C
C       WA1, WA2, WA3, and WA4 are work arrays of length N.
C
C
C 4. Successful completion.
C
C       The accuracy of DNSQ is controlled by the convergence parameter
C       XTOL.  This parameter is used in a test which makes a comparison
C       between the approximation X and a solution XSOL.  DNSQ
C       terminates when the test is satisfied.  If the convergence
C       parameter is less than the machine precision (as defined by the
C       function D1MACH(4)), then DNSQ only attempts to satisfy the test
C       defined by the machine precision.  Further progress is not
C       usually possible.
C
C       The test assumes that the functions are reasonably well behaved,
C       and, if the Jacobian is supplied by the user, that the functions
C       and the Jacobian are coded consistently.  If these conditions
C       are not satisfied, then DNSQ may incorrectly indicate
C       convergence.  The coding of the Jacobian can be checked by the
C       subroutine DCKDER. If the Jacobian is coded correctly or IOPT=2,
C       then the validity of the answer can be checked, for example, by
C       rerunning DNSQ with a tighter tolerance.
C
C       Convergence Test.  If DENORM(Z) denotes the Euclidean norm of a
C         vector Z and D is the diagonal matrix whose entries are
C         defined by the array DIAG, then this test attempts to
C         guarantee that
C
C               DENORM(D*(X-XSOL)) .LE. XTOL*DENORM(D*XSOL).
C
C         If this condition is satisfied with XTOL = 10**(-K), then the
C         larger components of D*X have K significant decimal digits and
C         INFO is set to 1.  There is a danger that the smaller
C         components of D*X may have large relative errors, but the fast
C         rate of convergence of DNSQ usually avoids this possibility.
C         Unless high precision solutions are required, the recommended
C         value for XTOL is the square root of the machine precision.
C
C
C 5. Unsuccessful Completion.
C
C       Unsuccessful termination of DNSQ can be due to improper input
C       parameters, arithmetic interrupts, an excessive number of
C       function evaluations, or lack of good progress.
C
C       Improper Input Parameters.  INFO is set to 0 if IOPT .LT .1,
C         or IOPT .GT. 2, or N .LE. 0, or LDFJAC .LT. N, or
C         XTOL .LT. 0.E0, or MAXFEV .LE. 0, or ML .LT. 0, or MU .LT. 0,
C         or FACTOR .LE. 0.E0, or LR .LT. (N*(N+1))/2.
C
C       Arithmetic Interrupts.  If these interrupts occur in the FCN
C         subroutine during an early stage of the computation, they may
C         be caused by an unacceptable choice of X by DNSQ.  In this
C         case, it may be possible to remedy the situation by rerunning
C         DNSQ with a smaller value of FACTOR.
C
C       Excessive Number of Function Evaluations.  A reasonable value
C         for MAXFEV is 100*(N+1) for IOPT=1 and 200*(N+1) for IOPT=2.
C         If the number of calls to FCN reaches MAXFEV, then this
C         indicates that the routine is converging very slowly as
C         measured by the progress of FVEC, and INFO is set to 2. This
C         situation should be unusual because, as indicated below, lack
C         of good progress is usually diagnosed earlier by DNSQ,
C         causing termination with info = 4 or INFO = 5.
C
C       Lack of Good Progress.  DNSQ searches for a zero of the system
C         by minimizing the sum of the squares of the functions.  In so
C         doing, it can become trapped in a region where the minimum
C         does not correspond to a zero of the system and, in this
C         situation, the iteration eventually fails to make good
C         progress.  In particular, this will happen if the system does
C         not have a zero.  If the system has a zero, rerunning DNSQ
C         from a different starting point may be helpful.
C
C
C 6. Characteristics of The Algorithm.
C
C       DNSQ is a modification of the Powell Hybrid method.  Two of its
C       main characteristics involve the choice of the correction as a
C       convex combination of the Newton and scaled gradient directions,
C       and the updating of the Jacobian by the rank-1 method of
C       Broyden.  The choice of the correction guarantees (under
C       reasonable conditions) global convergence for starting points
C       far from the solution and a fast rate of convergence.  The
C       Jacobian is calculated at the starting point by either the
C       user-supplied subroutine or a forward-difference approximation,
C       but it is not recalculated until the rank-1 method fails to
C       produce satisfactory progress.
C
C       Timing.  The time required by DNSQ to solve a given problem
C         depends on N, the behavior of the functions, the accuracy
C         requested, and the starting point.  The number of arithmetic
C         operations needed by DNSQ is about 11.5*(N**2) to process
C         each evaluation of the functions (call to FCN) and 1.3*(N**3)
C         to process each evaluation of the Jacobian (call to JAC,
C         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
C         the timing of DNSQ will be strongly influenced by the time
C         spent in FCN and JAC.
C
C       Storage.  DNSQ requires (3*N**2 + 17*N)/2 single precision
C         storage locations, in addition to the storage required by the
C         program.  There are no internally declared storage arrays.
C
C *Long Description:
C
C 7. Example.
C
C       The problem is to determine the values of X(1), X(2), ..., X(9),
C       which solve the system of tridiagonal equations
C
C       (3-2*X(1))*X(1)           -2*X(2)                   = -1
C               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
C                                   -X(8) + (3-2*X(9))*X(9) = -1
C C     **********
C
C       PROGRAM TEST
C C
C C     Driver for DNSQ example.
C C
C       INTEGER J,IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,
C      *        NWRITE
C       DOUBLE PRECISION XTOL,EPSFCN,FACTOR,FNORM
C       DOUBLE PRECISION X(9),FVEC(9),DIAG(9),FJAC(9,9),R(45),QTF(9),
C      *     WA1(9),WA2(9),WA3(9),WA4(9)
C       DOUBLE PRECISION DENORM,D1MACH
C       EXTERNAL FCN
C       DATA NWRITE /6/
C C
C       IOPT = 2
C       N = 9
C C
C C     THE FOLLOWING STARTING VALUES PROVIDE A ROUGH SOLUTION.
C C
C       DO 10 J = 1, 9
C          X(J) = -1.E0
C    10    CONTINUE
C C
C       LDFJAC = 9
C       LR = 45
C C
C C     SET XTOL TO THE SQUARE ROOT OF THE MACHINE PRECISION.
C C     UNLESS HIGH PRECISION SOLUTIONS ARE REQUIRED,
C C     THIS IS THE RECOMMENDED SETTING.
C C
C       XTOL = SQRT(D1MACH(4))
C C
C       MAXFEV = 2000
C       ML = 1
C       MU = 1
C       EPSFCN = 0.E0
C       MODE = 2
C       DO 20 J = 1, 9
C          DIAG(J) = 1.E0
C    20    CONTINUE
C       FACTOR = 1.E2
C       NPRINT = 0
C C
C       CALL DNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,MU,
C      *           EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
C      *           R,LR,QTF,WA1,WA2,WA3,WA4)
C       FNORM = DENORM(N,FVEC)
C       WRITE (NWRITE,1000) FNORM,NFEV,INFO,(X(J),J=1,N)
C       STOP
C  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
C      *        5X,' NUMBER OF FUNCTION EVALUATIONS',I10 //
C      *        5X,' EXIT PARAMETER',16X,I10 //
C      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
C       END
C       SUBROUTINE FCN(N,X,FVEC,IFLAG)
C       INTEGER N,IFLAG
C       DOUBLE PRECISION X(N),FVEC(N)
C       INTEGER K
C       DOUBLE PRECISION ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
C       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
C C
C       IF (IFLAG .NE. 0) GO TO 5
C C
C C     INSERT PRINT STATEMENTS HERE WHEN NPRINT IS POSITIVE.
C C
C       RETURN
C     5 CONTINUE
C       DO 10 K = 1, N
C          TEMP = (THREE - TWO*X(K))*X(K)
C          TEMP1 = ZERO
C          IF (K .NE. 1) TEMP1 = X(K-1)
C          TEMP2 = ZERO
C          IF (K .NE. N) TEMP2 = X(K+1)
C          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
C    10    CONTINUE
C       RETURN
C       END
C
C       Results obtained with different compilers or machines
C       may be slightly different.
C
C       Final L2 norm of the residuals  0.1192636E-07
C
C       Number of function evaluations        14
C
C       Exit parameter                         1
C
C       Final approximate solution
C
C       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
C       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
C       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
C
C***REFERENCES  M. J. D. Powell, A hybrid method for nonlinear equa-
C                 tions. In Numerical Methods for Nonlinear Algebraic
C                 Equations, P. Rabinowitz, Editor.  Gordon and Breach,
C                 1988.
C***ROUTINES CALLED  D1MACH, D1MPYQ, D1UPDT, DDOGLG, DENORM, DFDJC1,
C                    DQFORM, DQRFAC, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DNSQ
CCCCC DOUBLE PRECISION D1MACH,DENORM
      DOUBLE PRECISION DENORM
      INTEGER I, IFLAG, INFO, IOPT, ITER, IWA(1), J, JM1, L, LDFJAC,
     1     LR, MAXFEV, ML, MODE, MU, N, NCFAIL, NCSUC, NFEV, NJEV,
     2     NPRINT, NSLOW1, NSLOW2
      DOUBLE PRECISION ACTRED, DELTA, DIAG(*), EPSFCN, EPSMCH, FACTOR,
     1     FJAC(LDFJAC,*), FNORM, FNORM1, FVEC(*), ONE, P0001, P001,
     2     P1, P5, PNORM, PRERED, QTF(*), R(*), RATIO, SUM, TEMP,
     3     WA1(*), WA2(*), WA3(*), WA4(*), X(*), XNORM, XTOL, ZERO
      REAL XDATA(NOBS)
      EXTERNAL FCN
      LOGICAL JEVAL,SING
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
      SAVE ONE, P1, P5, P001, P0001, ZERO
      DATA ONE,P1,P5,P001,P0001,ZERO
     1     /1.0D0,1.0D-1,5.0D-1,1.0D-3,1.0D-4,0.0D0/
C
C     BEGIN BLOCK PERMITTING ...EXITS TO 320
C***FIRST EXECUTABLE STATEMENT  DNSQ
         EPSMCH = D1MACH(4)
C
         INFO = 0
         IFLAG = 0
         NFEV = 0
         NJEV = 0
C
C        CHECK THE INPUT PARAMETERS FOR ERRORS.
C
C     ...EXIT
         IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0
     1       .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0 .OR. ML .LT. 0
     2       .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO .OR. LDFJAC .LT. N
     3       .OR. LR .LT. (N*(N + 1))/2) GO TO 320
         IF (MODE .NE. 2) GO TO 20
            DO 10 J = 1, N
C     .........EXIT
               IF (DIAG(J) .LE. ZERO) GO TO 320
   10       CONTINUE
   20    CONTINUE
C
C        EVALUATE THE FUNCTION AT THE STARTING POINT
C        AND CALCULATE ITS NORM.
C
         IFLAG = 1
         CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
         NFEV = 1
C     ...EXIT
         IF (IFLAG .LT. 0) GO TO 320
         FNORM = DENORM(N,FVEC)
C
C        INITIALIZE ITERATION COUNTER AND MONITORS.
C
         ITER = 1
         NCSUC = 0
         NCFAIL = 0
         NSLOW1 = 0
         NSLOW2 = 0
C
C        BEGINNING OF THE OUTER LOOP.
C
   30    CONTINUE
C           BEGIN BLOCK PERMITTING ...EXITS TO 90
               JEVAL = .TRUE.
C
C              CALCULATE THE JACOBIAN MATRIX.
C
               IF (IOPT .EQ. 2) GO TO 40
C
C                 USER SUPPLIES JACOBIAN
C
CCCCC             CALL JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
                  NJEV = NJEV + 1
               GO TO 50
   40          CONTINUE
C
C                 CODE APPROXIMATES THE JACOBIAN
C
                  IFLAG = 2
                  CALL DFDJC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,
     1                        EPSFCN,WA1,WA2,XDATA,NOBS)
                  NFEV = NFEV + MIN(ML+MU+1,N)
   50          CONTINUE
C
C     .........EXIT
               IF (IFLAG .LT. 0) GO TO 320
C
C              COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
C
               CALL DQRFAC(N,N,FJAC,LDFJAC,.FALSE.,IWA,1,WA1,WA2,WA3)
C
C              ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
C              TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
C
C           ...EXIT
               IF (ITER .NE. 1) GO TO 90
               IF (MODE .EQ. 2) GO TO 70
                  DO 60 J = 1, N
                     DIAG(J) = WA2(J)
                     IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
   60             CONTINUE
   70          CONTINUE
C
C              ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED
C              X AND INITIALIZE THE STEP BOUND DELTA.
C
               DO 80 J = 1, N
                  WA3(J) = DIAG(J)*X(J)
   80          CONTINUE
               XNORM = DENORM(N,WA3)
               DELTA = FACTOR*XNORM
               IF (DELTA .EQ. ZERO) DELTA = FACTOR
   90       CONTINUE
C
C           FORM (Q TRANSPOSE)*FVEC AND STORE IN QTF.
C
            DO 100 I = 1, N
               QTF(I) = FVEC(I)
  100       CONTINUE
            DO 140 J = 1, N
               IF (FJAC(J,J) .EQ. ZERO) GO TO 130
                  SUM = ZERO
                  DO 110 I = J, N
                     SUM = SUM + FJAC(I,J)*QTF(I)
  110             CONTINUE
                  TEMP = -SUM/FJAC(J,J)
                  DO 120 I = J, N
                     QTF(I) = QTF(I) + FJAC(I,J)*TEMP
  120             CONTINUE
  130          CONTINUE
  140       CONTINUE
C
C           COPY THE TRIANGULAR FACTOR OF THE QR FACTORIZATION INTO R.
C
            SING = .FALSE.
            DO 170 J = 1, N
               L = J
               JM1 = J - 1
               IF (JM1 .LT. 1) GO TO 160
               DO 150 I = 1, JM1
                  R(L) = FJAC(I,J)
                  L = L + N - I
  150          CONTINUE
  160          CONTINUE
               R(L) = WA1(J)
               IF (WA1(J) .EQ. ZERO) SING = .TRUE.
  170       CONTINUE
C
C           ACCUMULATE THE ORTHOGONAL FACTOR IN FJAC.
C
            CALL DQFORM(N,N,FJAC,LDFJAC,WA1)
C
C           RESCALE IF NECESSARY.
C
            IF (MODE .EQ. 2) GO TO 190
               DO 180 J = 1, N
                  DIAG(J) = MAX(DIAG(J),WA2(J))
  180          CONTINUE
  190       CONTINUE
C
C           BEGINNING OF THE INNER LOOP.
C
  200       CONTINUE
C
C              IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
C
               IF (NPRINT .LE. 0) GO TO 210
                  IFLAG = 0
                  IF (MOD(ITER-1,NPRINT) .EQ. 0)
     1               CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
C     ............EXIT
                  IF (IFLAG .LT. 0) GO TO 320
  210          CONTINUE
C
C              DETERMINE THE DIRECTION P.
C
               CALL DDOGLG(N,R,LR,DIAG,QTF,DELTA,WA1,WA2,WA3)
C
C              STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
C
               DO 220 J = 1, N
                  WA1(J) = -WA1(J)
                  WA2(J) = X(J) + WA1(J)
                  WA3(J) = DIAG(J)*WA1(J)
  220          CONTINUE
               PNORM = DENORM(N,WA3)
C
C              ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
C
               IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)
C
C              EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.
C
               IFLAG = 1
               CALL FCN(N,WA2,WA4,IFLAG,XDATA,NOBS)
               NFEV = NFEV + 1
C     .........EXIT
               IF (IFLAG .LT. 0) GO TO 320
               FNORM1 = DENORM(N,WA4)
C
C              COMPUTE THE SCALED ACTUAL REDUCTION.
C
               ACTRED = -ONE
               IF (FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C
C              COMPUTE THE SCALED PREDICTED REDUCTION.
C
               L = 1
               DO 240 I = 1, N
                  SUM = ZERO
                  DO 230 J = I, N
                     SUM = SUM + R(L)*WA1(J)
                     L = L + 1
  230             CONTINUE
                  WA3(I) = QTF(I) + SUM
  240          CONTINUE
               TEMP = DENORM(N,WA3)
               PRERED = ZERO
               IF (TEMP .LT. FNORM) PRERED = ONE - (TEMP/FNORM)**2
C
C              COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
C              REDUCTION.
C
               RATIO = ZERO
               IF (PRERED .GT. ZERO) RATIO = ACTRED/PRERED
C
C              UPDATE THE STEP BOUND.
C
               IF (RATIO .GE. P1) GO TO 250
                  NCSUC = 0
                  NCFAIL = NCFAIL + 1
                  DELTA = P5*DELTA
               GO TO 260
  250          CONTINUE
                  NCFAIL = 0
                  NCSUC = NCSUC + 1
                  IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)
     1               DELTA = MAX(DELTA,PNORM/P5)
                  IF (ABS(RATIO-ONE) .LE. P1) DELTA = PNORM/P5
  260          CONTINUE
C
C              TEST FOR SUCCESSFUL ITERATION.
C
               IF (RATIO .LT. P0001) GO TO 280
C
C                 SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
C
                  DO 270 J = 1, N
                     X(J) = WA2(J)
                     WA2(J) = DIAG(J)*X(J)
                     FVEC(J) = WA4(J)
  270             CONTINUE
                  XNORM = DENORM(N,WA2)
                  FNORM = FNORM1
                  ITER = ITER + 1
  280          CONTINUE
C
C              DETERMINE THE PROGRESS OF THE ITERATION.
C
               NSLOW1 = NSLOW1 + 1
               IF (ACTRED .GE. P001) NSLOW1 = 0
               IF (JEVAL) NSLOW2 = NSLOW2 + 1
               IF (ACTRED .GE. P1) NSLOW2 = 0
C
C              TEST FOR CONVERGENCE.
C
               IF (DELTA .LE. XTOL*XNORM .OR. FNORM .EQ. ZERO) INFO = 1
C     .........EXIT
               IF (INFO .NE. 0) GO TO 320
C
C              TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
C
               IF (NFEV .GE. MAXFEV) INFO = 2
               IF (P1*MAX(P1*DELTA,PNORM) .LE. EPSMCH*XNORM) INFO = 3
               IF (NSLOW2 .EQ. 5) INFO = 4
               IF (NSLOW1 .EQ. 10) INFO = 5
C     .........EXIT
               IF (INFO .NE. 0) GO TO 320
C
C              CRITERION FOR RECALCULATING JACOBIAN
C
C           ...EXIT
               IF (NCFAIL .EQ. 2) GO TO 310
C
C              CALCULATE THE RANK ONE MODIFICATION TO THE JACOBIAN
C              AND UPDATE QTF IF NECESSARY.
C
               DO 300 J = 1, N
                  SUM = ZERO
                  DO 290 I = 1, N
                     SUM = SUM + FJAC(I,J)*WA4(I)
  290             CONTINUE
                  WA2(J) = (SUM - WA3(J))/PNORM
                  WA1(J) = DIAG(J)*((DIAG(J)*WA1(J))/PNORM)
                  IF (RATIO .GE. P0001) QTF(J) = SUM
  300          CONTINUE
C
C              COMPUTE THE QR FACTORIZATION OF THE UPDATED JACOBIAN.
C
               CALL D1UPDT(N,N,R,LR,WA1,WA2,WA3,SING)
               CALL D1MPYQ(N,N,FJAC,LDFJAC,WA2,WA3)
               CALL D1MPYQ(1,N,QTF,1,WA2,WA3)
C
C              END OF THE INNER LOOP.
C
               JEVAL = .FALSE.
            GO TO 200
  310       CONTINUE
C
C           END OF THE OUTER LOOP.
C
         GO TO 30
  320 CONTINUE
C
C     TERMINATION, EITHER NORMAL OR USER IMPOSED.
C
      IF (IFLAG .LT. 0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT .GT. 0) CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
CCCCC IF (INFO .LT. 0) CALL XERMSG ('SLATEC', 'DNSQ',
CCCCC+   'EXECUTION TERMINATED BECAUSE USER SET IFLAG NEGATIVE.', 1, 1)
CCCCC IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNSQ',
CCCCC+   'INVALID INPUT PARAMETER.', 2, 1)
CCCCC IF (INFO .EQ. 2) CALL XERMSG ('SLATEC', 'DNSQ',
CCCCC+   'TOO MANY FUNCTION EVALUATIONS.', 9, 1)
CCCCC IF (INFO .EQ. 3) CALL XERMSG ('SLATEC', 'DNSQ',
CCCCC+   'XTOL TOO SMALL. NO FURTHER IMPROVEMENT POSSIBLE.', 3, 1)
CCCCC IF (INFO .GT. 4) CALL XERMSG ('SLATEC', 'DNSQ',
CCCCC+   'ITERATION NOT MAKING GOOD PROGRESS.', 1, 1)
      IF (INFO .LT. 0) THEN
        WRITE(ICOUT,1001)
 1001   FORMAT('***** ERROR IN DNSQE NON-LINEAR SIMULTANEOUS EQUATION ',
     1         'SOLVER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1003)
 1003   FORMAT('      TERMINATION HALTED BECAUSE IFLAG IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 0) THEN
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1004)
 1004   FORMAT('      INVALID INPUT PARAMETER.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 2) THEN
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1005)
 1005   FORMAT('      TOO MANY FUNCTION EVALUATIONS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 3) THEN
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1006)
 1006   FORMAT('      XTOL TOO SMALL.  NO FURTHER IMPROVEMENT ',
     1         'POSSIBLE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .GT. 4) THEN
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1007)
 1007   FORMAT('      ITERATION NOT MAKING GOOD PROGRESS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
C
C     LAST CARD OF SUBROUTINE DNSQ.
C
      END
*DECK DFDJC1
      SUBROUTINE DFDJC1 (FCN, N, X, FVEC, FJAC, LDFJAC, IFLAG, ML, MU,
     +   EPSFCN, WA1, WA2,
     +   XDATA,NOBS)
C***BEGIN PROLOGUE  DFDJC1
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DNSQ and DNSQE
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (FDJAC1-S, DFDJC1-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     This subroutine computes a forward-difference approximation
C     to the N by N Jacobian matrix associated with a specified
C     problem of N functions in N variables. If the Jacobian has
C     a banded form, then function evaluations are saved by only
C     approximating the nonzero terms.
C
C     The subroutine statement is
C
C       SUBROUTINE DFDJC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,
C                         WA1,WA2)
C
C     where
C
C       FCN is the name of the user-supplied subroutine which
C         calculates the functions. FCN must be declared
C         in an EXTERNAL statement in the user calling
C         program, and should be written as follows.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         DOUBLE PRECISION X(N),FVEC(N)
C         ----------
C         Calculate the functions at X and
C         return this vector in FVEC.
C         ----------
C         RETURN
C
C         The value of IFLAG should not be changed by FCN unless
C         the user wants to terminate execution of DFDJC1.
C         In this case set IFLAG to a negative integer.
C
C       N is a positive integer input variable set to the number
C         of functions and variables.
C
C       X is an input array of length N.
C
C       FVEC is an input array of length N which must contain the
C         functions evaluated at X.
C
C       FJAC is an output N by N array which contains the
C         approximation to the Jacobian matrix evaluated at X.
C
C       LDFJAC is a positive integer input variable not less than N
C         which specifies the leading dimension of the array FJAC.
C
C       IFLAG is an integer variable which can be used to terminate
C         the execution of DFDJC1. See description of FCN.
C
C       ML is a nonnegative integer input variable which specifies
C         the number of subdiagonals within the band of the
C         Jacobian matrix. If the Jacobian is not banded, set
C         ML to at least N - 1.
C
C       EPSFCN is an input variable used in determining a suitable
C         step length for the forward-difference approximation. This
C         approximation assumes that the relative errors in the
C         functions are of the order of EPSFCN. If EPSFCN is less
C         than the machine precision, it is assumed that the relative
C         errors in the functions are of the order of the machine
C         precision.
C
C       MU is a nonnegative integer input variable which specifies
C         the number of superdiagonals within the band of the
C         Jacobian matrix. If the Jacobian is not banded, set
C         MU to at least N - 1.
C
C       WA1 and WA2 are work arrays of length N. If ML + MU + 1 is at
C         least N, then the Jacobian is considered dense, and WA2 is
C         not referenced.
C
C***SEE ALSO  DNSQ, DNSQE
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  DFDJC1
CCCCC DOUBLE PRECISION D1MACH
      INTEGER I, IFLAG, J, K, LDFJAC, ML, MSUM, MU, N
      DOUBLE PRECISION EPS, EPSFCN, EPSMCH, FJAC(LDFJAC,*),
     1     FVEC(*), H, TEMP, WA1(*), WA2(*), X(*), ZERO
      SAVE ZERO
C
      REAL XDATA(NOBS)
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA ZERO /0.0D0/
C
C     EPSMCH IS THE MACHINE PRECISION.
C
C***FIRST EXECUTABLE STATEMENT  DFDJC1
      EPSMCH = D1MACH(4)
C
      EPS = SQRT(MAX(EPSFCN,EPSMCH))
      MSUM = ML + MU + 1
      IF (MSUM .LT. N) GO TO 40
C
C        COMPUTATION OF DENSE APPROXIMATE JACOBIAN.
C
         DO 20 J = 1, N
            TEMP = X(J)
            H = EPS*ABS(TEMP)
            IF (H .EQ. ZERO) H = EPS
            X(J) = TEMP + H
            CALL FCN(N,X,WA1,IFLAG,XDATA,NOBS)
            IF (IFLAG .LT. 0) GO TO 30
            X(J) = TEMP
            DO 10 I = 1, N
               FJAC(I,J) = (WA1(I) - FVEC(I))/H
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
         GO TO 110
   40 CONTINUE
C
C        COMPUTATION OF BANDED APPROXIMATE JACOBIAN.
C
         DO 90 K = 1, MSUM
            DO 60 J = K, N, MSUM
               WA2(J) = X(J)
               H = EPS*ABS(WA2(J))
               IF (H .EQ. ZERO) H = EPS
               X(J) = WA2(J) + H
   60          CONTINUE
            CALL FCN(N,X,WA1,IFLAG,XDATA,NOBS)
            IF (IFLAG .LT. 0) GO TO 100
            DO 80 J = K, N, MSUM
               X(J) = WA2(J)
               H = EPS*ABS(WA2(J))
               IF (H .EQ. ZERO) H = EPS
               DO 70 I = 1, N
                  FJAC(I,J) = ZERO
                  IF (I .GE. J - MU .AND. I .LE. J + ML)
     1               FJAC(I,J) = (WA1(I) - FVEC(I))/H
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DFDJC1.
C
      END
*DECK DQRFAC
      SUBROUTINE DQRFAC (M, N, A, LDA, PIVOT, IPVT, LIPVT, SIGMA,
     +   ACNORM, WA)
C***BEGIN PROLOGUE  DQRFAC
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DNLS1, DNLS1E, DNSQ and DNSQE
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (QRFAC-S, DQRFAC-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C   **** Double Precision version of QRFAC ****
C
C     This subroutine uses Householder transformations with column
C     pivoting (optional) to compute a QR factorization of the
C     M by N matrix A. That is, DQRFAC determines an orthogonal
C     matrix Q, a permutation matrix P, and an upper trapezoidal
C     matrix R with diagonal elements of nonincreasing magnitude,
C     such that A*P = Q*R. The Householder transformation for
C     column K, K = 1,2,...,MIN(M,N), is of the form
C
C                           T
C           I - (1/U(K))*U*U
C
C     where U has zeros in the first K-1 positions. The form of
C     this transformation and the method of pivoting first
C     appeared in the corresponding LINPACK subroutine.
C
C     The subroutine statement is
C
C       SUBROUTINE DQRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A.
C
C       N is a positive integer input variable set to the number
C         of columns of A.
C
C       A is an M by N array. On input A contains the matrix for
C         which the QR factorization is to be computed. On output
C         the strict upper trapezoidal part of A contains the strict
C         upper trapezoidal part of R, and the lower trapezoidal
C         part of A contains a factored form of Q (the non-trivial
C         elements of the U vectors described above).
C
C       LDA is a positive integer input variable not less than M
C         which specifies the leading dimension of the array A.
C
C       PIVOT is a logical input variable. If pivot is set .TRUE.,
C         then column pivoting is enforced. If pivot is set .FALSE.,
C         then no column pivoting is done.
C
C       IPVT is an integer output array of length LIPVT. IPVT
C         defines the permutation matrix P such that A*P = Q*R.
C         Column J of P is column IPVT(J) of the identity matrix.
C         If pivot is .FALSE., IPVT is not referenced.
C
C       LIPVT is a positive integer input variable. If PIVOT is
C             .FALSE., then LIPVT may be as small as 1. If PIVOT is
C             .TRUE., then LIPVT must be at least N.
C
C       SIGMA is an output array of length N which contains the
C         diagonal elements of R.
C
C       ACNORM is an output array of length N which contains the
C         norms of the corresponding columns of the input matrix A.
C         If this information is not needed, then ACNORM can coincide
C         with SIGMA.
C
C       WA is a work array of length N. If pivot is .FALSE., then WA
C         can coincide with SIGMA.
C
C***SEE ALSO  DNLS1, DNLS1E, DNSQ, DNSQE
C***ROUTINES CALLED  D1MACH, DENORM
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  DQRFAC
      INTEGER M,N,LDA,LIPVT
      INTEGER IPVT(*)
      LOGICAL PIVOT
      SAVE ONE, P05, ZERO
      DOUBLE PRECISION A(LDA,*),SIGMA(*),ACNORM(*),WA(*)
      INTEGER I,J,JP1,K,KMAX,MINMN
      DOUBLE PRECISION AJNORM,EPSMCH,ONE,P05,SUM,TEMP,ZERO
CCCCC DOUBLE PRECISION D1MACH,DENORM
      DOUBLE PRECISION DENORM
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
      DATA ONE,P05,ZERO /1.0D0,5.0D-2,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DQRFAC
      EPSMCH = D1MACH(4)
C
C     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.
C
      DO 10 J = 1, N
         ACNORM(J) = DENORM(M,A(1,J))
         SIGMA(J) = ACNORM(J)
         WA(J) = SIGMA(J)
         IF (PIVOT) IPVT(J) = J
   10    CONTINUE
C
C     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.
C
      MINMN = MIN(M,N)
      DO 110 J = 1, MINMN
         IF (.NOT.PIVOT) GO TO 40
C
C        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.
C
         KMAX = J
         DO 20 K = J, N
            IF (SIGMA(K) .GT. SIGMA(KMAX)) KMAX = K
   20       CONTINUE
         IF (KMAX .EQ. J) GO TO 40
         DO 30 I = 1, M
            TEMP = A(I,J)
            A(I,J) = A(I,KMAX)
            A(I,KMAX) = TEMP
   30       CONTINUE
         SIGMA(KMAX) = SIGMA(J)
         WA(KMAX) = WA(J)
         K = IPVT(J)
         IPVT(J) = IPVT(KMAX)
         IPVT(KMAX) = K
   40    CONTINUE
C
C        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE
C        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.
C
         AJNORM = DENORM(M-J+1,A(J,J))
         IF (AJNORM .EQ. ZERO) GO TO 100
         IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM
         DO 50 I = J, M
            A(I,J) = A(I,J)/AJNORM
   50       CONTINUE
         A(J,J) = A(J,J) + ONE
C
C        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS
C        AND UPDATE THE NORMS.
C
         JP1 = J + 1
         IF (N .LT. JP1) GO TO 100
         DO 90 K = JP1, N
            SUM = ZERO
            DO 60 I = J, M
               SUM = SUM + A(I,J)*A(I,K)
   60          CONTINUE
            TEMP = SUM/A(J,J)
            DO 70 I = J, M
               A(I,K) = A(I,K) - TEMP*A(I,J)
   70          CONTINUE
            IF (.NOT.PIVOT .OR. SIGMA(K) .EQ. ZERO) GO TO 80
            TEMP = A(J,K)/SIGMA(K)
            SIGMA(K) = SIGMA(K)*SQRT(MAX(ZERO,ONE-TEMP**2))
            IF (P05*(SIGMA(K)/WA(K))**2 .GT. EPSMCH) GO TO 80
            SIGMA(K) = DENORM(M-J,A(JP1,K))
            WA(K) = SIGMA(K)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
         SIGMA(J) = -AJNORM
  110    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DQRFAC.
C
      END
*DECK DENORM
      DOUBLE PRECISION FUNCTION DENORM (N, X)
C***BEGIN PROLOGUE  DENORM
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DNSQ and DNSQE
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (ENORM-S, DENORM-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     Given an N-vector X, this function calculates the
C     Euclidean norm of X.
C
C     The Euclidean norm is computed by accumulating the sum of
C     squares in three different sums. The sums of squares for the
C     small and large components are scaled so that no overflows
C     occur. Non-destructive underflows are permitted. Underflows
C     and overflows do not occur in the computation of the unscaled
C     sum of squares for the intermediate components.
C     The definitions of small, intermediate and large components
C     depend on two constants, RDWARF and RGIANT. The main
C     restrictions on these constants are that RDWARF**2 not
C     underflow and RGIANT**2 not overflow. The constants
C     given here are suitable for every known computer.
C
C     The function statement is
C
C       DOUBLE PRECISION FUNCTION DENORM(N,X)
C
C     where
C
C       N is a positive integer input variable.
C
C       X is an input array of length N.
C
C***SEE ALSO  DNSQ, DNSQE
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  DENORM
      INTEGER I, N
      DOUBLE PRECISION AGIANT, FLOATN, ONE, RDWARF, RGIANT, S1, S2, S3,
     1     X(*), X1MAX, X3MAX, XABS, ZERO
      SAVE ONE, ZERO, RDWARF, RGIANT
      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
C***FIRST EXECUTABLE STATEMENT  DENORM
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT/FLOATN
      DO 90 I = 1, N
         XABS = ABS(X(I))
         IF (XABS .GT. RDWARF .AND. XABS .LT. AGIANT) GO TO 70
            IF (XABS .LE. RDWARF) GO TO 30
C
C              SUM FOR LARGE COMPONENTS.
C
               IF (XABS .LE. X1MAX) GO TO 10
                  S1 = ONE + S1*(X1MAX/XABS)**2
                  X1MAX = XABS
                  GO TO 20
   10          CONTINUE
                  S1 = S1 + (XABS/X1MAX)**2
   20          CONTINUE
               GO TO 60
   30       CONTINUE
C
C              SUM FOR SMALL COMPONENTS.
C
               IF (XABS .LE. X3MAX) GO TO 40
                  S3 = ONE + S3*(X3MAX/XABS)**2
                  X3MAX = XABS
                  GO TO 50
   40          CONTINUE
                  IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2
   50          CONTINUE
   60       CONTINUE
            GO TO 80
   70    CONTINUE
C
C           SUM FOR INTERMEDIATE COMPONENTS.
C
            S2 = S2 + XABS**2
   80    CONTINUE
   90    CONTINUE
C
C     CALCULATION OF NORM.
C
      IF (S1 .EQ. ZERO) GO TO 100
         DENORM = X1MAX*SQRT(S1+(S2/X1MAX)/X1MAX)
         GO TO 130
  100 CONTINUE
         IF (S2 .EQ. ZERO) GO TO 110
            IF (S2 .GE. X3MAX)
     1         DENORM = SQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
            IF (S2 .LT. X3MAX)
     1         DENORM = SQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
            GO TO 120
  110    CONTINUE
            DENORM = X3MAX*SQRT(S3)
  120    CONTINUE
  130 CONTINUE
      RETURN
C
C     LAST CARD OF FUNCTION DENORM.
C
      END
*DECK DQFORM
      SUBROUTINE DQFORM (M, N, Q, LDQ, WA)
C***BEGIN PROLOGUE  DQFORM
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DNSQ and DNSQE
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (QFORM-S, DQFORM-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     This subroutine proceeds from the computed QR factorization of
C     an M by N matrix A to accumulate the M by M orthogonal matrix
C     Q from its factored form.
C
C     The subroutine statement is
C
C       SUBROUTINE DQFORM(M,N,Q,LDQ,WA)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A and the order of Q.
C
C       N is a positive integer input variable set to the number
C         of columns of A.
C
C       Q is an M by M array. On input the full lower trapezoid in
C         the first MIN(M,N) columns of Q contains the factored form.
C         On output Q has been accumulated into a square matrix.
C
C       LDQ is a positive integer input variable not less than M
C         which specifies the leading dimension of the array Q.
C
C       WA is a work array of length M.
C
C***SEE ALSO  DNSQ, DNSQE
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  DQFORM
      INTEGER I, J, JM1, K, L, LDQ, M, MINMN, N, NP1
      DOUBLE PRECISION ONE, Q(LDQ,*), SUM, TEMP, WA(*), ZERO
      SAVE ONE, ZERO
      DATA ONE,ZERO /1.0D0,0.0D0/
C
C     ZERO OUT UPPER TRIANGLE OF Q IN THE FIRST MIN(M,N) COLUMNS.
C
C***FIRST EXECUTABLE STATEMENT  DQFORM
      MINMN = MIN(M,N)
      IF (MINMN .LT. 2) GO TO 30
      DO 20 J = 2, MINMN
         JM1 = J - 1
         DO 10 I = 1, JM1
            Q(I,J) = ZERO
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
C
C     INITIALIZE REMAINING COLUMNS TO THOSE OF THE IDENTITY MATRIX.
C
      NP1 = N + 1
      IF (M .LT. NP1) GO TO 60
      DO 50 J = NP1, M
         DO 40 I = 1, M
            Q(I,J) = ZERO
   40       CONTINUE
         Q(J,J) = ONE
   50    CONTINUE
   60 CONTINUE
C
C     ACCUMULATE Q FROM ITS FACTORED FORM.
C
      DO 120 L = 1, MINMN
         K = MINMN - L + 1
         DO 70 I = K, M
            WA(I) = Q(I,K)
            Q(I,K) = ZERO
   70       CONTINUE
         Q(K,K) = ONE
         IF (WA(K) .EQ. ZERO) GO TO 110
         DO 100 J = K, M
            SUM = ZERO
            DO 80 I = K, M
               SUM = SUM + Q(I,J)*WA(I)
   80          CONTINUE
            TEMP = SUM/WA(K)
            DO 90 I = K, M
               Q(I,J) = Q(I,J) - TEMP*WA(I)
   90          CONTINUE
  100       CONTINUE
  110    CONTINUE
  120    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DQFORM.
C
      END
*DECK DDOGLG
      SUBROUTINE DDOGLG (N, R, LR, DIAG, QTB, DELTA, X, WA1, WA2)
C***BEGIN PROLOGUE  DDOGLG
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DNSQ and DNSQE
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (DOGLEG-S, DDOGLG-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     Given an M by N matrix A, an N by N nonsingular diagonal
C     matrix D, an M-vector B, and a positive number DELTA, the
C     problem is to determine the convex combination X of the
C     Gauss-Newton and scaled gradient directions that minimizes
C     (A*X - B) in the least squares sense, subject to the
C     restriction that the Euclidean norm of D*X be at most DELTA.
C
C     This subroutine completes the solution of the problem
C     if it is provided with the necessary information from the
C     QR factorization of A. That is, if A = Q*R, where Q has
C     orthogonal columns and R is an upper triangular matrix,
C     then DDOGLG expects the full upper triangle of R and
C     the first N components of (Q transpose)*B.
C
C     The subroutine statement is
C
C       SUBROUTINE DDOGLG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
C
C     where
C
C       N is a positive integer input variable set to the order of R.
C
C       R is an input array of length LR which must contain the upper
C         triangular matrix R stored by rows.
C
C       LR is a positive integer input variable not less than
C         (N*(N+1))/2.
C
C       DIAG is an input array of length N which must contain the
C         diagonal elements of the matrix D.
C
C       QTB is an input array of length N which must contain the first
C         N elements of the vector (Q transpose)*B.
C
C       DELTA is a positive input variable which specifies an upper
C         bound on the Euclidean norm of D*X.
C
C       X is an output array of length N which contains the desired
C         convex combination of the Gauss-Newton direction and the
C         scaled gradient direction.
C
C       WA1 and WA2 are work arrays of length N.
C
C***SEE ALSO  DNSQ, DNSQE
C***ROUTINES CALLED  D1MACH, DENORM
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  DDOGLG
CCCCC DOUBLE PRECISION D1MACH,DENORM
      DOUBLE PRECISION DENORM
      INTEGER I, J, JJ, JP1, K, L, LR, N
      DOUBLE PRECISION ALPHA, BNORM, DELTA, DIAG(*), EPSMCH, GNORM,
     1     ONE, QNORM, QTB(*), R(*), SGNORM, SUM, TEMP, WA1(*),
     2     WA2(*), X(*), ZERO
      SAVE ONE, ZERO
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
      DATA ONE,ZERO /1.0D0,0.0D0/
C
C     EPSMCH IS THE MACHINE PRECISION.
C
C***FIRST EXECUTABLE STATEMENT  DDOGLG
      EPSMCH = D1MACH(4)
C
C     FIRST, CALCULATE THE GAUSS-NEWTON DIRECTION.
C
      JJ = (N*(N + 1))/2 + 1
      DO 50 K = 1, N
         J = N - K + 1
         JP1 = J + 1
         JJ = JJ - K
         L = JJ + 1
         SUM = ZERO
         IF (N .LT. JP1) GO TO 20
         DO 10 I = JP1, N
            SUM = SUM + R(L)*X(I)
            L = L + 1
   10       CONTINUE
   20    CONTINUE
         TEMP = R(JJ)
         IF (TEMP .NE. ZERO) GO TO 40
         L = J
         DO 30 I = 1, J
            TEMP = MAX(TEMP,ABS(R(L)))
            L = L + N - I
   30       CONTINUE
         TEMP = EPSMCH*TEMP
         IF (TEMP .EQ. ZERO) TEMP = EPSMCH
   40    CONTINUE
         X(J) = (QTB(J) - SUM)/TEMP
   50    CONTINUE
C
C     TEST WHETHER THE GAUSS-NEWTON DIRECTION IS ACCEPTABLE.
C
      DO 60 J = 1, N
         WA1(J) = ZERO
         WA2(J) = DIAG(J)*X(J)
   60    CONTINUE
      QNORM = DENORM(N,WA2)
      IF (QNORM .LE. DELTA) GO TO 140
C
C     THE GAUSS-NEWTON DIRECTION IS NOT ACCEPTABLE.
C     NEXT, CALCULATE THE SCALED GRADIENT DIRECTION.
C
      L = 1
      DO 80 J = 1, N
         TEMP = QTB(J)
         DO 70 I = J, N
            WA1(I) = WA1(I) + R(L)*TEMP
            L = L + 1
   70       CONTINUE
         WA1(J) = WA1(J)/DIAG(J)
   80    CONTINUE
C
C     CALCULATE THE NORM OF THE SCALED GRADIENT AND TEST FOR
C     THE SPECIAL CASE IN WHICH THE SCALED GRADIENT IS ZERO.
C
      GNORM = DENORM(N,WA1)
      SGNORM = ZERO
      ALPHA = DELTA/QNORM
      IF (GNORM .EQ. ZERO) GO TO 120
C
C     CALCULATE THE POINT ALONG THE SCALED GRADIENT
C     AT WHICH THE QUADRATIC IS MINIMIZED.
C
      DO 90 J = 1, N
         WA1(J) = (WA1(J)/GNORM)/DIAG(J)
   90    CONTINUE
      L = 1
      DO 110 J = 1, N
         SUM = ZERO
         DO 100 I = J, N
            SUM = SUM + R(L)*WA1(I)
            L = L + 1
  100       CONTINUE
         WA2(J) = SUM
  110    CONTINUE
      TEMP = DENORM(N,WA2)
      SGNORM = (GNORM/TEMP)/TEMP
C
C     TEST WHETHER THE SCALED GRADIENT DIRECTION IS ACCEPTABLE.
C
      ALPHA = ZERO
      IF (SGNORM .GE. DELTA) GO TO 120
C
C     THE SCALED GRADIENT DIRECTION IS NOT ACCEPTABLE.
C     FINALLY, CALCULATE THE POINT ALONG THE DOGLEG
C     AT WHICH THE QUADRATIC IS MINIMIZED.
C
      BNORM = DENORM(N,QTB)
      TEMP = (BNORM/GNORM)*(BNORM/QNORM)*(SGNORM/DELTA)
      TEMP = TEMP - (DELTA/QNORM)*(SGNORM/DELTA)**2
     1       + SQRT((TEMP-(DELTA/QNORM))**2
     2               +(ONE-(DELTA/QNORM)**2)*(ONE-(SGNORM/DELTA)**2))
      ALPHA = ((DELTA/QNORM)*(ONE - (SGNORM/DELTA)**2))/TEMP
  120 CONTINUE
C
C     FORM APPROPRIATE CONVEX COMBINATION OF THE GAUSS-NEWTON
C     DIRECTION AND THE SCALED GRADIENT DIRECTION.
C
      TEMP = (ONE - ALPHA)*MIN(SGNORM,DELTA)
      DO 130 J = 1, N
         X(J) = TEMP*WA1(J) + ALPHA*X(J)
  130    CONTINUE
  140 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DDOGLG.
C
      END
*DECK D1UPDT
      SUBROUTINE D1UPDT (M, N, S, LS, U, V, W, SING)
C***BEGIN PROLOGUE  D1UPDT
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DNSQ and DNSQE
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (R1UPDT-S, D1UPDT-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     Given an M by N lower trapezoidal matrix S, an M-vector U,
C     and an N-vector V, the problem is to determine an
C     orthogonal matrix Q such that
C
C                   t
C           (S + U*V )*Q
C
C     is again lower trapezoidal.
C
C     This subroutine determines Q as the product of 2*(N - 1)
C     transformations
C
C           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
C
C     where GV(I), GW(I) are Givens rotations in the (I,N) plane
C     which eliminate elements in the I-th and N-th planes,
C     respectively. Q itself is not accumulated, rather the
C     information to recover the GV, GW rotations is returned.
C
C     The SUBROUTINE statement is
C
C       SUBROUTINE D1UPDT(M,N,S,LS,U,V,W,SING)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of S.
C
C       N is a positive integer input variable set to the number
C         of columns of S. N must not exceed M.
C
C       S is an array of length LS. On input S must contain the lower
C         trapezoidal matrix S stored by columns. On output S contains
C         the lower trapezoidal matrix produced as described above.
C
C       LS is a positive integer input variable not less than
C         (N*(2*M-N+1))/2.
C
C       U is an input array of length M which must contain the
C         vector U.
C
C       V is an array of length N. On input V must contain the vector
C         V. On output V(I) contains the information necessary to
C         recover the Givens rotation GV(I) described above.
C
C       W is an output array of length M. W(I) contains information
C         necessary to recover the Givens rotation GW(I) described
C         above.
C
C       SING is a LOGICAL output variable. SING is set TRUE if any
C         of the diagonal elements of the output S are zero. Otherwise
C         SING is set FALSE.
C
C***SEE ALSO  DNSQ, DNSQE
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  D1UPDT
CCCCC DOUBLE PRECISION D1MACH
      INTEGER I, J, JJ, L, LS, M, N, NM1, NMJ
      DOUBLE PRECISION COS, COTAN, GIANT, ONE, P25, P5, S(*),
     1     SIN, TAN, TAU, TEMP, U(*), V(*), W(*), ZERO
      LOGICAL SING
      SAVE ONE, P5, P25, ZERO
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
      DATA ONE,P5,P25,ZERO /1.0D0,5.0D-1,2.5D-1,0.0D0/
C
C     GIANT IS THE LARGEST MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  D1UPDT
      GIANT = D1MACH(2)
C
C     INITIALIZE THE DIAGONAL ELEMENT POINTER.
C
      JJ = (N*(2*M - N + 1))/2 - (M - N)
C
C     MOVE THE NONTRIVIAL PART OF THE LAST COLUMN OF S INTO W.
C
      L = JJ
      DO 10 I = N, M
         W(I) = S(L)
         L = L + 1
   10    CONTINUE
C
C     ROTATE THE VECTOR V INTO A MULTIPLE OF THE N-TH UNIT VECTOR
C     IN SUCH A WAY THAT A SPIKE IS INTRODUCED INTO W.
C
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 NMJ = 1, NM1
         J = N - NMJ
         JJ = JJ - (M - J + 1)
         W(J) = ZERO
         IF (V(J) .EQ. ZERO) GO TO 50
C
C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
C        J-TH ELEMENT OF V.
C
         IF (ABS(V(N)) .GE. ABS(V(J))) GO TO 20
            COTAN = V(N)/V(J)
            SIN = P5/SQRT(P25+P25*COTAN**2)
            COS = SIN*COTAN
            TAU = ONE
            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
            GO TO 30
   20    CONTINUE
            TAN = V(J)/V(N)
            COS = P5/SQRT(P25+P25*TAN**2)
            SIN = COS*TAN
            TAU = SIN
   30    CONTINUE
C
C        APPLY THE TRANSFORMATION TO V AND STORE THE INFORMATION
C        NECESSARY TO RECOVER THE GIVENS ROTATION.
C
         V(N) = SIN*V(J) + COS*V(N)
         V(J) = TAU
C
C        APPLY THE TRANSFORMATION TO S AND EXTEND THE SPIKE IN W.
C
         L = JJ
         DO 40 I = J, M
            TEMP = COS*S(L) - SIN*W(I)
            W(I) = SIN*S(L) + COS*W(I)
            S(L) = TEMP
            L = L + 1
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     ADD THE SPIKE FROM THE RANK 1 UPDATE TO W.
C
      DO 80 I = 1, M
         W(I) = W(I) + V(N)*U(I)
   80    CONTINUE
C
C     ELIMINATE THE SPIKE.
C
      SING = .FALSE.
      IF (NM1 .LT. 1) GO TO 140
      DO 130 J = 1, NM1
         IF (W(J) .EQ. ZERO) GO TO 120
C
C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
C        J-TH ELEMENT OF THE SPIKE.
C
         IF (ABS(S(JJ)) .GE. ABS(W(J))) GO TO 90
            COTAN = S(JJ)/W(J)
            SIN = P5/SQRT(P25+P25*COTAN**2)
            COS = SIN*COTAN
            TAU = ONE
            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS
            GO TO 100
   90    CONTINUE
            TAN = W(J)/S(JJ)
            COS = P5/SQRT(P25+P25*TAN**2)
            SIN = COS*TAN
            TAU = SIN
  100    CONTINUE
C
C        APPLY THE TRANSFORMATION TO S AND REDUCE THE SPIKE IN W.
C
         L = JJ
         DO 110 I = J, M
            TEMP = COS*S(L) + SIN*W(I)
            W(I) = -SIN*S(L) + COS*W(I)
            S(L) = TEMP
            L = L + 1
  110       CONTINUE
C
C        STORE THE INFORMATION NECESSARY TO RECOVER THE
C        GIVENS ROTATION.
C
         W(J) = TAU
  120    CONTINUE
C
C        TEST FOR ZERO DIAGONAL ELEMENTS IN THE OUTPUT S.
C
         IF (S(JJ) .EQ. ZERO) SING = .TRUE.
         JJ = JJ + (M - J + 1)
  130    CONTINUE
  140 CONTINUE
C
C     MOVE W BACK INTO THE LAST COLUMN OF THE OUTPUT S.
C
      L = JJ
      DO 150 I = N, M
         S(L) = W(I)
         L = L + 1
  150    CONTINUE
      IF (S(JJ) .EQ. ZERO) SING = .TRUE.
      RETURN
C
C     LAST CARD OF SUBROUTINE D1UPDT.
C
      END
*DECK D1MPYQ
      SUBROUTINE D1MPYQ (M, N, A, LDA, V, W)
C***BEGIN PROLOGUE  D1MPYQ
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DNSQ and DNSQE
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (R1MPYQ-S, D1MPYQ-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     Given an M by N matrix A, this subroutine computes A*Q where
C     Q is the product of 2*(N - 1) transformations
C
C           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
C
C     and GV(I), GW(I) are Givens rotations in the (I,N) plane which
C     eliminate elements in the I-th and N-th planes, respectively.
C     Q itself is not given, rather the information to recover the
C     GV, GW rotations is supplied.
C
C     The SUBROUTINE statement is
C
C       SUBROUTINE D1MPYQ(M,N,A,LDA,V,W)
C
C     where
C
C       M is a positive integer input variable set to the number
C         of rows of A.
C
C       N IS a positive integer input variable set to the number
C         of columns of A.
C
C       A is an M by N array. On input A must contain the matrix
C         to be postmultiplied by the orthogonal matrix Q
C         described above. On output A*Q has replaced A.
C
C       LDA is a positive integer input variable not less than M
C         which specifies the leading dimension of the array A.
C
C       V is an input array of length N. V(I) must contain the
C         information necessary to recover the Givens rotation GV(I)
C         described above.
C
C       W is an input array of length N. W(I) must contain the
C         information necessary to recover the Givens rotation GW(I)
C         described above.
C
C***SEE ALSO  DNSQ, DNSQE
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  D1MPYQ
      INTEGER I, J, LDA, M, N, NM1, NMJ
      DOUBLE PRECISION A(LDA,*), COS, ONE, SIN, TEMP, V(*), W(*)
      SAVE ONE
      DATA ONE /1.0D0/
C
C     APPLY THE FIRST SET OF GIVENS ROTATIONS TO A.
C
C***FIRST EXECUTABLE STATEMENT  D1MPYQ
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 50
      DO 20 NMJ = 1, NM1
         J = N - NMJ
         IF (ABS(V(J)) .GT. ONE) COS = ONE/V(J)
         IF (ABS(V(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
         IF (ABS(V(J)) .LE. ONE) SIN = V(J)
         IF (ABS(V(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
         DO 10 I = 1, M
            TEMP = COS*A(I,J) - SIN*A(I,N)
            A(I,N) = SIN*A(I,J) + COS*A(I,N)
            A(I,J) = TEMP
   10       CONTINUE
   20    CONTINUE
C
C     APPLY THE SECOND SET OF GIVENS ROTATIONS TO A.
C
      DO 40 J = 1, NM1
         IF (ABS(W(J)) .GT. ONE) COS = ONE/W(J)
         IF (ABS(W(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)
         IF (ABS(W(J)) .LE. ONE) SIN = W(J)
         IF (ABS(W(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)
         DO 30 I = 1, M
            TEMP = COS*A(I,J) + SIN*A(I,N)
            A(I,N) = -SIN*A(I,J) + COS*A(I,N)
            A(I,J) = TEMP
   30       CONTINUE
   40    CONTINUE
   50 CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE D1MPYQ.
C
      END
      SUBROUTINE DECOMP(IND, LOCA, IOUT, NW, W, M, LSTFI, N, LS, LV,
     * LLIM, LP)
C  PART OF ACM 591 FOR ANOVA
C  ***************************** DECOMP *****************************   DEC   10
C                                                                       DEC   20
C  OBTAINS A FACTORIAL DECOMPOSITION OF THE VECTOR T WHERE T CONSISTS   DEC   30
C  OF THE FIRST NCELLS LOCATIONS OF THE VECTOR A (IN ARRAY W); THE      DEC   40
C  FACTORIAL DECOMPOSITION IS FORMED IN VECTOR A AND OCCUPIES ALL THE   DEC   50
C  LOCATIONS OF THIS VECTOR.  ALTERNATIVELY COMPUTES CLASSIFICATION     DEC   60
C  SUMS/MEANS IN VECTOR A FOR RESTRUCTURING DATA OR FOR THE C OPTION.   DEC   70
C  FOLLOWS THE ALGORITHM DESCRIBED IN HEMMERLE, STATISTICAL COMPUTA-    DEC   80
C  TIONS ON A DIGITAL COMPUTER 1967.                                    DEC   90
C                                                                       DEC  100
C  IND = 0 (FACTORIAL DECOMPOSITION); IND = 1 (CLASSIFICATION SUMS);    DEC  110
C  IND = 2 (CLASSIFICATION MEANS)                                       DEC  120
C                                                                       DEC  130
C  LOCA = BASE ADDRESS OF VECTOR A IN ARRAY W; IOUT = OUTPUT UNIT FOR   DEC  140
C  CLASSIFICATIONS MEANS.                                               DEC  150
C                                                                       DEC  160
C  (SEE MAIN PROGRAM COMMENTS) FOR DESCRIPTION OF OTHER ARGUMENTS       DEC  170
C                                                                       DEC  180
C  ******************************************************************   DEC  190
C NOTE: THE ARGUMENTS LS,LV,LP, AND IOUT ARE USED ONLY FOR C MEANS
      DOUBLE PRECISION W, TEMP, DNPM, CMEAN
      DIMENSION W(NW), LSTFI(M), LS(N), LV(N), LLIM(N), LP(10)
C
      CHARACTER*1 IDOT
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA IDOT /'.'/
      LL = 1
      MM = 1
      NN = 1
      LOCTWO = LOCA + 1
   10 LOCONE = LOCA + 1
      KK = LL
C FIND NUMBER OF ELEMENTS IN THIS MEAN
C
      K1 = N + 1 - NN
      NPM = LLIM(K1)
      DNPM = NPM
   20 LOCTWO = LOCTWO + LSTFI(MM)
C FIND NUMBER OF MEANS FOR EACH RESIDUAL
      MEANST = LSTFI(MM+1)
C FIND INCREMENT
      K1 = M + 1 - KK
      INC = LSTFI(K1)
C FORM THE ARRAY OF MEANS
      MD = 1
      NO = M - MM
CNIST IF (IND.EQ.2) CALL LABEL(NO, IDOT, LS, IOUT, N, LV, LP)
      DO 90 I=1,MEANST,INC
        JTWO = I + INC - 1
        DO 80 J=I,JTWO
          L = MD
          LD = MD
          I1 = LOCTWO + J - 1
          TEMP = 0.D0
          DO 30 K=1,NPM
            I2 = LOCONE + L - 1
            TEMP = TEMP + W(I2)
            L = L + INC
   30     CONTINUE
C DEVIATES (IND=0); SUMS (IND=1); CLASSIFICATION MEANS (IND=2)
          IF (IND.EQ.0) GO TO 50
          IF (IND.EQ.1) GO TO 40
          IF (TEMP.EQ.0.0) THEN
             WRITE (ICOUT,99999) J
             CALL DPWRST('XXX','BUG ')
          ENDIF
          IF (TEMP.GT.0.0) CMEAN = W(I1)/TEMP
          IF (TEMP.GT.0.0) THEN
             WRITE (ICOUT,99998) J, W(I1), TEMP, CMEAN
             CALL DPWRST('XXX','BUG ')
          ENDIF
99999     FORMAT (1H , I6, 4X, 29H(MISSING CLASSIFICATION CELL))
99998     FORMAT (1H , I6, 1X, E16.8, F5.0, 1X, E16.8)
   40     W(I1) = TEMP
          GO TO 70
   50     W(I1) = TEMP/DNPM
C FORM DEVIATES
          DO 60 K=1,NPM
            I2 = LOCONE + LD - 1
            W(I2) = W(I2) - W(I1)
            LD = LD + INC
   60     CONTINUE
   70     MD = MD + 1
   80   CONTINUE
        MD = L - INC + 1
   90 CONTINUE
      IF (KK.EQ.1) GO TO 100
      KK = KK - 1
      MM = MM + 1
      K1 = LL - KK
      LOCONE = LOCONE + LSTFI(K1)
      GO TO 20
  100 IF (NN.EQ.N) RETURN
      LL = LL + LL
      NN = NN + 1
      MM = MM + 1
      GO TO 10
      END
      SUBROUTINE SCAN(IPT, M, LER, N, LE, LS, LV, LLIM, LP, L, IA,
     * IBATCH)
C  PART OF ACM 591 FOR ANOVA
C  ****************************** SCAN ******************************   SCA   10
C                                                                       SCA   20
C  PROCESSES THE MODEL/HYPOTHESIS STATEMENT TO CONSTRUCT/MODIFY THE     SCA   30
C  E/R LIST (ARRAY LER); TURNS SWITCH ISST ON FOR AN INVALID STATE-     SCA   40
C  MENT.  DETERMINES THE EFFECTIVE NUMBER OF FACTORS (NSUBS); TURNS     SCA   50
C  SWITCH IXST ON WHEN THE EFFECTIVE X MATRIX IS SQUARE; COMPUTES THE   SCA   60
C  PARAMETERS NEEDED IN RESTRUCTURING DATA (LPOUT AND NO1).  COMPUTES   SCA   70
C  THE DEGREES OF FREEDOM APPLICABLE TO DATA WITH NO MISSING CELLS      SCA   80
C  (IDFM AND IDFR).                                                     SCA   90
C                                                                       SCA  100
C  IPT = POINTER TO BEGINNING OF MODEL/HYPOTHESIS STATEMENT IN INPUT    SCA  110
C  BUFFER; IBATCH = 1 (BATCH PROCESSING) OR IBATCH = 0 (INTERACTIVE)    SCA  120
C                                                                       SCA  130
C  (SEE MAIN PROGRAM COMMENTS FOR DESCRIPTION OF OTHER ARGUMENTS)       SCA  140
C                                                                       SCA  150
C  ******************************************************************   SCA  160
      COMMON /C1/ YPY, SSRM, SSEM, IIN, IOUT, IROPT, IVOPT, IGOPT,
     * IPOPT, IOFLAG, IBST, IHST, IRST, ISST, IXST, ICD, NSUBS, LPOUT,
     * NO1, IDF, IDFM, IDFR
      DIMENSION LER(M), LE(N), LS(N), LV(N), LLIM(N), LP(10), IA(L)
      DOUBLE PRECISION YPY, SSRM, SSEM
C
      CHARACTER*1 ILP, IRP, IM, IH, ISTAR, ISLASH, IBLANK, IC
      CHARACTER*4 ICD
C
CNIST CHARACTER*1 FUNCTION IGET
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA ILP   /'('/
      DATA IRP   /')'/
      DATA IM    /'M'/
      DATA IH    /'H'/
      DATA ISTAR /'*'/
      DATA ISLASH /'/'/
      DATA IBLANK /' '/
C
      ISST = 0
      IXST = 0
      M1 = M - 1
      II = IPT
      IF (II.GT.L) GO TO 350
CNIST IC = IGET(II,IA,L)
      IF (ICD(1:1).EQ.IH) GO TO 20
      IF (IC.EQ.ISTAR) GO TO 270
C INITIALIZE E/R LIST TO ZEROES FOR M AND ABSOLUTE VALUES FOR H
      DO 10 I=1,M1
        LER(I) = 0
   10 CONTINUE
      LER(M) = 1
   20 IF (LER(M).EQ.0) GO TO 350
      DO 30 I=1,M1
        LER(I) = IABS(LER(I))
   30 CONTINUE
      M2 = 2*M
C SCAN TERM TO CONSTRUCT E/R LIST; ENTER NEGATIVES FOR H TERM
   40 DO 50 I=1,N
        LP(I) = M2
   50 CONTINUE
C SUM VALUES OF FACTOR SYMBOLS FOR E/R ENTRY; ZERO LP POSITIONS
      NE = 0
      NVS = 0
   60 IFLAG = 0
      DO 70 I=1,N
CNIST   IF (IC.NE.LE(I)) GO TO 70
        LP(I) = 0
        IFLAG = 1
        NE = NE + 1
        NVS = NVS + LV(I)
   70 CONTINUE
      IF (IFLAG.NE.1) GO TO 80
      IF (II.GT.L) GO TO 350
CNIST IC = IGET(II,IA,L)
      GO TO 60
   80 IF (NE.EQ.0) GO TO 350
CNIST IF (IC.NE.ILP) GO TO 350
C SCAN SUBSCRIPTS; SET NONZERO LP ENTRIES TO NUMERICAL VALUES
      NS = 0
      NAS = 0
   90 IF (II.GT.L) GO TO 350
CNIST IC = IGET(II,IA,L)
CNIST SET FOLLOWING LINE JUST TO AVOID COMPILATION WARNING.
CNIST REMOVE IF WE ACTIVATE THIS CODE
      IC=' '
      IFLAG = 0
      DO 120 I=1,N
CNIST   IF (IC.NE.LS(I)) GO TO 120
        IF (LP(I).NE.0) LP(I) = LV(I)
        IF (LP(I).EQ.0) NAS = NAS + 1
C CHECK FOR INVALID NESTED TERM
        DO 100 J=I,N
          IF (LP(J).EQ.0) GO TO 110
  100   CONTINUE
        GO TO 350
  110   IFLAG = 1
        NS = NS + 1
  120 CONTINUE
      IF (IFLAG.NE.1) GO TO 130
      GO TO 90
  130 IF (NAS.NE.NE) GO TO 350
      IF (IC.NE.IRP) GO TO 350
      IF (NS.NE.NE) GO TO 150
C CHECK FOR INVALID CROSSED TERM
      DO 140 I=1,N
        IF (LP(I).EQ.M2) GO TO 140
        IF (LP(I).NE.0) GO TO 350
  140 CONTINUE
      I = M - NVS
      ITEMP = 0
      IF (ICD(1:1).EQ.IH) ITEMP = NVS + 1
      IF (LER(I).NE.ITEMP) GO TO 350
      LER(I) = NVS + 1
      IF (ICD(1:1).EQ.IH) LER(I) = -LER(I)
      GO TO 190
C ENTER SUM FOR NESTED TERM INTO E/R POSITIONS TO POOL
  150 DO 180 I=1,M1
        NUM = I - NVS
        DO 160 J=1,N
          NUM = NUM - LP(J)
          IF (NUM.GT.0) GO TO 160
          IF (NUM.EQ.0) GO TO 170
          NUM = NUM + LP(J)
  160   CONTINUE
        GO TO 180
  170   K = M - I
        ITEMP = 0
        IF (ICD(1:1).EQ.IH) ITEMP = NVS + 1
        IF (LER(K).NE.ITEMP) GO TO 350
        LER(K) = NVS + 1
        IF (ICD(1:1).EQ.IH) LER(K) = -LER(K)
  180 CONTINUE
  190 IF (II.GT.L) GO TO 200
CNIST IC = IGET(II,IA,L)
      IF (IC.EQ.IBLANK .AND. II.GT.L) GO TO 200
      IF (IC.NE.ISLASH) GO TO 40
C READ MODEL OR HYPOTHESIS CONTINUATION CARD (SLASH FOLLOWS TERM)
      READ (IIN,99999) (IA(I),I=1,L)
99999 FORMAT (80A1)
      IF (IBATCH.EQ.1) THEN
         WRITE (ICOUT,99998) (IA(I),I=1,L)
         CALL DPWRST('XXX','BUG ')
      ENDIF
99998 FORMAT (1H , 80A1)
      II = 1
CNIST IC = IGET(II,IA,L)
      GO TO 40
C CHECK FOR INVALID HYPOTHESIS TERM
  200 DO 220 I=1,M1
        DO 210 J=I,M1
          IF (LER(I).EQ.0) GO TO 210
          IF (LER(I).EQ.(-LER(J))) GO TO 350
  210   CONTINUE
  220 CONTINUE
C CONSTRUCT LP FROM E/R; DETERMINE EFFECTIVE FACTORS
      NSUBS = N
      DO 250 I=1,N
        LP(I) = 0
        INC1 = LV(I)
        INC2 = LV(1)/INC1
        LOC = 1
        DO 240 J=1,INC2
          DO 230 K=1,INC1
            IF (LER(LOC).GT.0) LP(I) = LP(I) + 1
            LOC = LOC + 1
  230     CONTINUE
          LOC = LOC + INC1
  240   CONTINUE
        IF (LP(I).EQ.0) NSUBS = NSUBS - 1
  250 CONTINUE
C DETERMINE IF THE EFFECTIVE X MATRIX IS SQUARE
      IV = N - NSUBS + 1
      DO 260 I=1,N
        IF (LP(I).EQ.0) GO TO 260
        IF (LP(I).NE.LV(IV)) GO TO 310
  260 CONTINUE
      GO TO 300
C CONSTRUCT E/R LIST FOR COMPLETELY CROSSED MODEL
  270 DO 280 I=1,M1
        LER(I) = M - I + 1
  280 CONTINUE
      NSUBS = N
      DO 290 I=1,N
        LP(I) = LV(1)
  290 CONTINUE
  300 IXST = 1
  310 IF (IOFLAG.EQ.1) THEN
        WRITE (ICOUT,99997) (LER(I),I=1,M)
        CALL DPWRST('XXX','BUG ')
      ENDIF
99997 FORMAT (10H E/R LIST-/(1H , 16I5))
C COMPUTE PARAMETERS REQUIRED TO RESTRUCTURE CELL FREQUENCY ARRAY
      LPOUT = 1
      NO1 = 1
      DO 320 I=1,N
        IF (LP(I).EQ.0) LPOUT = LPOUT*LLIM(I)
        IF (LP(I).NE.0) NO1 = NO1 + LV(I)
  320 CONTINUE
C COMPUTE DEGREES OF FREEDOM FOR FULL OR REDUCED MODEL
      IDF = 0
      DO 340 I=1,M
        IF (LER(I).LE.0) GO TO 340
        NO2 = M - I + 1
        CALL LABEL(NO2, 0, LLIM, IOUT, N, LV, LP)
        K = 1
        DO 330 J=1,N
          IF (LP(J).NE.0) K = K*(LLIM(J)-1)
  330   CONTINUE
        IDF = IDF + K
  340 CONTINUE
      IDFR = 0
      IF (ICD(1:1).EQ.IH) IDFR = IDF
      IF (ICD(1:1).EQ.IM) IDFM = IDF
      RETURN
  350 ISST = 1
      RETURN
      END
      SUBROUTINE STEP(IND, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM,
     *                LT, LP)
C  PART OF ACM 591 FOR ANOVA
C  ****************************** STEP ******************************   STE   10
C                                                                       STE   20
C  PERFORMS THE FOLLOWING SUB-STEPS OPERATING UPON THE VECTORS IN THE   STE   30
C  W ARRAY                                                              STE   40
C                                                                       STE   50
C                           1) T = (Y-D*V)/C                            STE   60
C                           2) V = V+T                                  STE   70
C                           3) B = B+T                                  STE   80
C                           4) T = R(T)                                 STE   90
C                           5) V = V-T                                  STE  100
C                           6) S = 2*Y*V-V*D*V                          STE  110
C                                                                       STE  120
C  VECTOR T CONSISTS OF THE FIRST NCELLS LOCATIONS IN VECTOR A OF W;    STE  130
C  HOWEVER, ALL LOCATIONS IN VECTOR A ARE NEEDED IN SUB-STEP 4.  R(T)   STE  140
C  IS THE RESIDUAL OPERATOR APPLIED TO VECTOR T; IT IS IMPLEMENTED      STE  150
C  USING SUBROUTINES DECOMP, POOL, AND LABEL.                           STE  160
C                                                                       STE  170
C  SUB-STEPS 1 AND 6 ARE MODIFIED IN COMPUTING RANK WITH THE R OPTION   STE  180
C  AND SUB-STEP 1 IS ALSO MODIFIED WHEN SWITCH IBST IS ON; ARGUMENT     STE  190
C  IND CONTROLS THESE MODIFICATIONS.                                    STE  200
C                                                                       STE  210
C  IND = 1 (ITERATION FOR SSR); IND = 2 (NON-ITERATIVE, IBST IS ON);    STE  220
C  IND = 3 (ITERATION FOR RANK)                                         STE  230
C                                                                       STE  240
C  S IS EITHER SSR (IND=2), AN APPROXIMATION TO SSR, (IND=1), OR PART   STE  250
C  OF THE RANK APPROXIMATION (IND=3).  C IS A SCALAR CONSTANT SELECT-   STE  260
C  ED FOR MONOTONICITY OF THE APPROXIMATION TO SSR OR FOR FASTER, BUT   STE  270
C  NOT MONOTONE, CONVERGENCE.                                           STE  280
C                                                                       STE  290
C  (SEE MAIN PROGRAM COMMENTS FOR DESCRIPTION OF OTHER ARGUMENTS)       STE  300
C                                                                       STE  310
C  ******************************************************************   STE  320
      DIMENSION W(NW), LSTFI(M), LER(M), LV(N), LLIM(N), LT(N), LP(10)
      DOUBLE PRECISION W, C, S, T1, T2
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      S = 0
      NCELLS = LSTFI(1)
      DO 40 I=1,NCELLS
C INCREMENT BASE ADDRESSES OF ARRAYS
        ID1 = NCELLS + I
        ID2 = ID1 + NCELLS
        IV = ID2 + NCELLS
        IB = IV + NCELLS
        IA = IB + NCELLS
C GENERAL ITERATION (IND=1); NON-ITERATIVE (IND=2); RANK (IND=3)
        IF (IND.EQ.1) GO TO 20
        IF (IND.EQ.2) GO TO 10
        W(IA) = W(I) - W(IV)
        IF (W(ID1).EQ.0.0) W(IA) = W(I)
        GO TO 30
   10   W(IA) = -W(IV)
        IF (W(ID2).GT.0.0) W(IA) = W(IA) + W(I)/W(ID2)
        GO TO 30
   20   W(IA) = (W(I)-W(ID1)*W(IV))/C
C V=V+A; B=B+A
   30   W(IV) = W(IV) + W(IA)
        W(IB) = W(IB) + W(IA)
   40 CONTINUE
C RESIDUAL OPERATOR
      IA = IB
      CALL DECOMP(0, IB, IOUT, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
      IFLAG = 0
      DO 70 I=1,M
        IF (LER(I).GT.0) GO TO 60
        IF (I.EQ.1) GO TO 50
        NO = M - I + 1
        CALL LABEL(NO, 0, LLIM, IOUT, N, LV, LP)
        CALL POOL(IFLAG, IA, IB, NW, W, N, LLIM, LT, LP)
   50   IFLAG = 1
   60   IB = IB + LSTFI(I)
   70 CONTINUE
C V=V-T; S=2*Y*V-V*D*V
      DO 90 I=1,NCELLS
        ID1 = NCELLS + I
        IV = ID2 + I
        IA = IA + 1
        IF (IFLAG.EQ.1) W(IV) = W(IV) - W(IA)
        T1 = 2.0D0*W(I)
        T2 = W(ID1)
        IF (T2.EQ.0.0) GO TO 80
        IF (IND.EQ.3) T2 = 1.0D0
        T1 = T1 - W(IV)*T2
   80   S = S + T1*W(IV)
   90 CONTINUE
      RETURN
      END
      SUBROUTINE PART1(NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP,
     * MAXMC, Q, QT)
C  PART OF ACM 591 FOR ANOVA
C  ****************************** PART1 *****************************   PAR   10
C                                                                       PAR   20
C  RESTRUCTURES THE DATA (CELL FREQUENCIES) WHEN APPROPRIATE; CHECKS    PAR   30
C  FOR BALANCE AND ALTERNATIVE NON-ITERATIVE COMPUTATIONS; TURNS IBST   PAR   40
C  ON WHEN THE EFFECTIVE X MATRIX IS SQUARE OR THE EFFECTIVE D MATRIX   PAR   50
C  IS A SCALAR MULTIPLE OF THE IDENTITY. COMPUTES RANK WITHOUT ITERA-   PAR   60
C  TION IF POSSIBLE OR ITERATIVELY OTHERWISE WHEN THE RANK (R) OPTION   PAR   70
C  IS SPECIFIED; TURNS SWITCH IRST ON IF THE MAXIMUM NUMBER OF ITERA-   PAR   80
C  TIONS IS EXCEEDED IN COMPUTING RANK.                                 PAR   90
C                                                                       PAR  100
C  ******************************************************************   PAR  110
      COMMON /C1/ YPY, SSRM, SSEM, IIN, IOUT, IROPT, IVOPT, IGOPT,
     * IPOPT, IOFLAG, IBST, IHST, IRST, ISST, IXST, ICD, NSUBS, LPOUT,
     * NO1, IDF, IDFM, IDFR
      COMMON /C2/ NCELLS, LOCD1, LOCD2, LOCV, LOCB, LOCA, IRANKM,
     * IRANKR, MAXIT
      DIMENSION W(NW), LSTFI(M), LER(M), LV(N), LLIM(N), LT(N), LP(10)
      DIMENSION Q(MAXMC,MAXMC), QT(MAXMC)
      DOUBLE PRECISION W, C, S, TRACE, TEMP, Q, QT, YPY, SSRM, SSEM
C
      CHARACTER*4 IH, IM, ICD
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
CCCCC DATA IH /1HH/, IM /1HM/
      DATA IH /'H'/, IM /'M'/
C
      IHST = 0
      IRST = 0
      IBST = 0
      IRANK = 0
      IF (NSUBS.EQ.N) GO TO 100
C FORM RESTRUCTURED CELL FREQUENCY ARRAY (EFFECTIVE D MATRIX)
      DO 10 I=1,NCELLS
        ID1 = LOCD1 + I
        IA = LOCA + I
        W(IA) = W(ID1)
   10 CONTINUE
      CALL DECOMP(1, LOCA, IOUT, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
      NS = LOCA
      NN = M - NO1
      DO 20 I=1,NN
        NS = NS + LSTFI(I)
   20 CONTINUE
CNIST CALL LABEL(NO1, 0, LLIM, IOUT, N, LV, LP)
      CALL POOL(0, LOCD2, NS, NW, W, N, LLIM, LT, LP)
C CHECK FOR A SQUARE EFFECTIVE X MATRIX
   30 IF (IXST.EQ.1) GO TO 80
      K = LOCD2 + 1
      IFLAG = 0
      DO 40 I=1,NCELLS
        ID2 = LOCD2 + I
        IF (W(ID2).EQ.0.0) GO TO 130
        IF (W(ID2).NE.W(K)) IFLAG = 1
   40 CONTINUE
      IF (IFLAG.EQ.1) GO TO 70
C THE EFFECTIVE D MATRIX IS A SCALAR TIMES THE IDENTITY
      IRANK = IDF
   50 DO 60 I=1,NCELLS
        ID2 = LOCD2 + I
        W(ID2) = W(ID2)/FLOAT(LPOUT)
   60 CONTINUE
      C = 1.0D0
      IBST = 1
      GO TO 120
C ALL ELEMENTS OF THE EFFECTIVE D MATRIX ARE NONZERO
   70 IRANK = IDF
      GO TO 120
C THE EFFECTIVE X MATRIX IS SQUARE
   80 DO 90 I=1,NCELLS
        ID2 = LOCD2 + I
        IF (W(ID2).NE.0.0) IRANK = IRANK + 1
   90 CONTINUE
      IRANK = IRANK/LPOUT
      GO TO 50
  100 DO 110 I=1,NCELLS
        ID1 = LOCD1 + I
        ID2 = LOCD2 + I
        W(ID2) = W(ID1)
  110 CONTINUE
      GO TO 30
C RANK HAS BEEN DETERMINED (NONITERATIVELY OR ITERATIVELY)
  120 IF (ICD.EQ.IH) IRANKR = IRANK
      IF (ICD.EQ.IM) IRANKM = IRANK
      GO TO 370
  130 IF (ICD.EQ.IM) GO TO 140
      IRANKR = 0
      IF (IRANKM.NE.IDFM) GO TO 150
      IRANKR = IDFR
      IRANK = IDFR
      GO TO 370
  140 IRANKM = 0
  150 IF (IROPT.EQ.0) GO TO 380
C ITERATIVELY COMPUTE RANK OF FULL OR REDUCED MODEL
      C = 1.0D0
      RTOL = 0.1
      NMC = 0
      DO 160 I=1,NCELLS
        ID1 = LOCD1 + I
        ID2 = LOCD2 + I
        IF (W(ID1).EQ.0.0) NMC = NMC + 1
        W(ID2) = W(I)
  160 CONTINUE
      IF (NMC.GT.MAXMC) GO TO 310
C COMPUTE Q, POWERS OF Q, AND RELATED TRACES (FEW EMPTY CELLS)
      K = 1
      IVEC = 0
      DO 190 I=1,NCELLS
        ID1 = LOCD1 + I
        IF (W(ID1).NE.0.0) GO TO 190
        DO 170 J=1,NCELLS
          IV = LOCV + J
          IB = LOCB + J
          W(IV) = 0
          W(IB) = 0
          W(J) = 0
          IF (J.EQ.I) W(J) = 1.0D0
  170   CONTINUE
        CALL STEP(3, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP)
        LL = 1
        DO 180 J=1,NCELLS
          ID1 = LOCD1 + J
          IV = LOCV + J
          IF (W(ID1).NE.0.0) GO TO 180
          Q(K,LL) = W(IV)
          LL = LL + 1
  180   CONTINUE
        K = K + 1
  190 CONTINUE
C POWER Q AND COMPUTE TR(I-Q**(2*K))
      TEMP = IDF
      DO 200 I=1,NMC
        TEMP = TEMP - Q(I,I)
  200 CONTINUE
      IT = 0
  210 IF (IOFLAG.EQ.1) THEN
        WRITE (ICOUT,99999) IT, TEMP
        CALL DPWRST('XXX','BUG ')
      ENDIF
99999 FORMAT (10H ITERATION, I3, 8H, TRACE=, F16.9)
      DO 250 J=1,NMC
        DO 230 I=J,NMC
          QT(I) = 0
          DO 220 K=1,NMC
            QT(I) = QT(I) + Q(K,J)*Q(K,I)
  220     CONTINUE
  230   CONTINUE
        DO 240 K=J,NMC
          Q(K,J) = QT(K)
  240   CONTINUE
  250 CONTINUE
      TRACE = IDF
      DO 270 I=1,NMC
        TRACE = TRACE - Q(I,I)
        DO 260 J=I,NMC
          Q(I,J) = Q(J,I)
  260   CONTINUE
  270 CONTINUE
      IT = IT + 1
      TEMP = TRACE - TEMP
C TRACE IS MONOTONICALLY INCREASING
      IF (TEMP.LE.RTOL) GO TO 280
      IF (IT.GE.MAXIT) GO TO 360
      TEMP = TRACE
      GO TO 210
  280 DO 290 I=1,NCELLS
        ID2 = LOCD2 + I
        W(I) = W(ID2)
  290 CONTINUE
C ADD ONE (BASED ON MONOTONICITY) TO OBTAIN INTEGER RANK
  300 IRANK = TRACE + 1.0D0
      GO TO 120
C COMPUTE S FOR UNIT VECTORS (MANY EMPTY CELLS)
  310 TRACE = 0
      RTOL = RTOL/(FLOAT(NCELLS)-FLOAT(NMC))
      DO 350 I=1,NCELLS
        ID1 = LOCD1 + I
        IF (W(ID1).EQ.0.0) GO TO 350
        DO 320 J=1,NCELLS
          IV = LOCV + J
          IB = LOCB + J
          W(IV) = 0
          W(IB) = 0
          W(J) = 0
          IF (J.EQ.I) W(J) = 1.0D0
  320   CONTINUE
        IT = 0
        TEMP = 0
  330   CALL STEP(3, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP)
        IT = IT + 1
        TEMP = S - TEMP
C THE VALUE OF S IS MONOTONICALLY INCREASING
        IF (TEMP.LE.RTOL) GO TO 340
        IVEC = I
        IF (IT.GE.MAXIT) GO TO 360
        TEMP = S
        GO TO 330
  340   TRACE = TRACE + S
        IF (IOFLAG.EQ.1) THEN
          WRITE (ICOUT,99998) I, IT, TRACE
          CALL DPWRST('XXX','BUG ')
        ENDIF
99998   FORMAT (7H VECTOR, I4, 12H, ITERATIONS, I4, 8H, TRACE=, F16.9)
  350 CONTINUE
      GO TO 280
  360 CONTINUE
      WRITE (ICOUT,99997) MAXIT
      CALL DPWRST('XXX','BUG ')
99997 FORMAT (11H MAXIMUM OF, I4, 34H ITERATIONS EXCEEDED IN COMPUTING ,
     * 4HRANK)
      WRITE (ICOUT,89997) TEMP, RTOL, IVEC
      CALL DPWRST('XXX','BUG ')
89997 FORMAT (7H DELTA=, F22.9, 10X, 8HEPSILON=, F22.9, 10X, 7HVECTOR=,
     * I10)
      IF (NMC.GT.MAXMC) TRACE = TRACE + S
      IRST = 1
      GO TO 300
  370 IF (IROPT.EQ.1) THEN
        WRITE (ICOUT,99996) ICD, IRANK
        CALL DPWRST('XXX','BUG ')
      ENDIF
99996 FORMAT (17H THE RANK OF THE , A1, 17H DESIGN MATRIX IS, I5)
  380 RETURN
      END
      SUBROUTINE PART2(NW, W, M, LSTFI, LER, N, LE, LV, LLIM, LT, LP)
C  PART OF ACM 591 FOR ANOVA
C  ****************************** PART2 *****************************   PAR   10
C                                                                       PAR   20
C  COMPUTES SSE AND SSR FOR THE FULL MODEL (ICD = M); OUTPUTS ESTI-     PAR   30
C  MATES OF EXPECTED CELL MEANS (THE VECTOR V) WHEN THE V OPTION IS     PAR   40
C  SPECIFIED; COMPUTES A G-INVERSE SOLUTION TO THE NORMAL EQUATIONS     PAR   50
C  WHEN THE G OPTION IS SPECIFIED.  COMPUTES SSR FOR THE REDUCED MOD-   PAR   60
C  EL (ICD = H) AND AN F STATISTIC; COMPUTES PROBABILITY VALUES WHEN    PAR   70
C  THE P OPTION IS SPECIFIED.  ALL COMPUTATIONS ARE NON-ITERATIVE IF    PAR   80
C  SWITCH IBST IS ON (IBST = 1)                                         PAR   90
C                                                                       PAR  100
C  (SEE MAIN PROGRAM COMMENTS FOR A DESCRIPTION OF ARGUMENTS)           PAR  110
C                                                                       PAR  120
C  ******************************************************************   PAR  130
      COMMON /C1/ YPY, SSRM, SSEM, IIN, IOUT, IROPT, IVOPT, IGOPT,
     * IPOPT, IOFLAG, IBST, IHST, IRST, ISST, IXST, ICD, NSUBS, LPOUT,
     * NO1, IDF, IDFM, IDFR
      COMMON /C2/ NCELLS, LOCD1, LOCD2, LOCV, LOCB, LOCA, IRANKM,
     * IRANKR, MAXIT
      COMMON /C3/ MAXDI, MINDI, FLEVEL, NOSIGD, NOBS
      DIMENSION W(NW), LSTFI(M), LER(M), LE(N), LV(N), LLIM(N), LT(N),
     * LP(10)
      DOUBLE PRECISION W, C, S, TEMP, YPY, SSRM, SSEM, DABS, F
C
      CHARACTER*1 IBLANK, ISTAR, IM, IH, ISIG
      CHARACTER*4 ICD
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
CCCCC DATA IBLANK /1H /, ISTAR /1H*/, IM /1HM/, IH /1HH/
      DATA IBLANK /' '/, ISTAR /'*'/, IM /'M'/, IH /'H'/
C
      FTOL = .005
      STOL = (.05*YPY)/(10.0**NOSIGD)
C ZERO THE VECTORS B AND V TO INITIALIZE THE ITERATIVE ALGORITHM
      DO 10 I=1,NCELLS
        IB = LOCB + I
        IV = LOCV + I
        W(IB) = 0
        W(IV) = 0
   10 CONTINUE
      IT = 0
      TEMP = 0
      IF (IBST.EQ.1) GO TO 260
      IF (ICD(1:1).EQ.IH) GO TO 170
C COMPUTE SSR FOR THE FULL MODEL USING OPTIMUM C FOR CONVERGENCE
      C = (FLOAT(MAXDI)+FLOAT(MINDI))/2.0
      IF (MINDI.EQ.0) C = MAXDI
   20 CALL STEP(1, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP)
      IT = IT + 1
      TEMP = S - TEMP
      IF (IOFLAG.EQ.1) THEN
        WRITE (ICOUT,99999) IT, ICD(1:1), S
        CALL DPWRST('XXX','BUG ')
      ENDIF
99999 FORMAT (10H ITERATION, I4, 5H, SSR, A1, 1H=, E16.8)
      IF (DABS(TEMP).LE.STOL) GO TO 30
      IF (IT.GE.MAXIT) GO TO 160
      TEMP = S
      GO TO 20
C APPLY THE E OPERATOR TO THE VECTOR B
   30 DO 40 I=1,NCELLS
        IB = LOCB + I
        IA = LOCA + I
        W(IA) = W(IB)
   40 CONTINUE
      CALL DECOMP(0, LOCA, IOUT, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
C COMPUTE SSR AND SSE FOR THE FULL MODEL
   50 SSRM = S
      SSEM = YPY - S
      WRITE (ICOUT,99998) IT, SSRM
99998 FORMAT (10H ITERATION, I4, 18H, SSR(FULL MODEL)=, E16.8, 1H,)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,89998) SSEM
89998 FORMAT (14X,18H  SSE(FULL MODEL)=, E16.8)
      CALL DPWRST('XXX','BUG ')
      IF (IVOPT.EQ.0) GO TO 70
      WRITE (ICOUT,99997)
99997 FORMAT (' ESTIMATES OF EXPECTED CELL MEANS-')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,89997)
89997 FORMAT ('    CELL  ESTIMATED MEAN')
      CALL DPWRST('XXX','BUG ')
      DO 60 I=1,NCELLS
        ID1 = LOCD1 + I
        IV = LOCV + I
        IF (W(ID1).EQ.0.0) THEN
          WRITE (ICOUT,99996) I, W(IV)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF (W(ID1).GT.0.0) THEN
          WRITE (ICOUT,99995) I, W(IV)
          CALL DPWRST('XXX','BUG ')
        ENDIF
   60 CONTINUE
99996 FORMAT (1H , I6, 1X, E16.8, 15H (MISSING CELL))
99995 FORMAT (1H , I6, 1X, E16.8)
   70 IF (IGOPT.EQ.0) GO TO 150
C COMPUTE THE G-INVERSE SOLUTION TO THE NORMAL EQUATIONS
      WRITE (ICOUT,99994)
99994 FORMAT (20H G-INVERSE SOLUTION-)
      CALL DPWRST('XXX','BUG ')
C POOL ARRAYS OF "ESTIMATES" WITH EQUAL E/R LIST VALUES
      NP = LOCA
      DO 140 I=1,M
        NO = LER(I)
        IF (NO.LE.0) GO TO 130
        NS = NP
        NOP = M - I + 1
CNIST   CALL LABEL(NOP, 0, LLIM, IOUT, N, LV, LP)
C POSITIVE VALUES IN LLIM WILL CORRESPOND TO SUBSCRIPTS IN PRIMARY
        DO 80 K=1,N
          IF (LP(K).EQ.0) LLIM(K) = -LLIM(K)
   80   CONTINUE
        DO 100 J=I,M
          IF (J.EQ.I) GO TO 90
          IF (LER(J).NE.NO) GO TO 90
          LER(J) = -NO
          NOS = M - J + 1
C OBTAIN MAP COEFFICIENTS FOR SECONDARY ARRAY AND POOL INTO PRIMARY
CNIST     CALL LABEL(NOS, 0, LLIM, IOUT, N, LV, LP)
          CALL POOL(1, NP, NS, NW, W, N, LLIM, LT, LP)
   90     NS = NS + LSTFI(J)
  100   CONTINUE
        DO 110 K=1,N
          LLIM(K) = IABS(LLIM(K))
  110   CONTINUE
C LABEL AND OUTPUT "ESTIMATES" FOR MODEL TERM
CNIST   CALL LABEL(NO, IBLANK, LE, IOUT, N, LV, LP)
        MST = LSTFI(I)
        DO 120 K=1,MST
          IA = NP + K
          WRITE (ICOUT,99995) K, W(IA)
          CALL DPWRST('XXX','BUG ')
  120   CONTINUE
  130   NP = NP + LSTFI(I)
  140 CONTINUE
  150 RETURN
  160 CONTINUE
      WRITE (ICOUT,99993) MAXIT, ICD(1:1)
99993 FORMAT (11H MAXIMUM OF, I4, 34H ITERATIONS EXCEEDED IN COMPUTING ,
     * 3HSSR, A1)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,89993) TEMP, STOL
89993 FORMAT (7H DELTA=, E16.8, 10X, 8HEPSILON=, E16.8)
      CALL DPWRST('XXX','BUG ')
      GO TO 30
C SELECT C FOR MONOTONICITY OF SSR AND F
  170 C = MAXDI
C COMPUTE DEGREES OF FREEDOM TO USE FOR F STATISTIC
  180 IF (IRANKM.EQ.0) GO TO 190
      IF (IRANKR.EQ.0) GO TO 190
      IDFD = NOBS - IRANKM
      IDFN = IRANKM - IRANKR
      WRITE (ICOUT,99992) IDFN, IDFD
99992 FORMAT (33H FROM RANK COMPUTATIONS- DF(NUM)=, I4, 10H, DF(DEN)=,
     * I5)
      CALL DPWRST('XXX','BUG ')
      GO TO 200
  190 IDFD = NOBS - IDFM
      IDFN = IDFM - IDFR
      WRITE (ICOUT,99991) IDFN, IDFD
99991 FORMAT (50H ASSUMES FULL RANK AND EQUAL LEVELS WITH- DF(NUM)=,
     * I4, 10H, DF(DEN)=, I5)
      CALL DPWRST('XXX','BUG ')
  200 IF (IDFD*IDFN.LE.0) GO TO 150
      IF (IBST.EQ.1) GO TO 220
C COMPUTE MONOTONICALLY DECREASING APPROXIMATION TO F
  210 CALL STEP(1, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP)
      IT = IT + 1
  220 F = ((SSRM-S)/FLOAT(IDFN))/(SSEM/FLOAT(IDFD))
      IF (IOFLAG.EQ.1) THEN
         WRITE (ICOUT,99999) IT, ICD(1:1), S
         CALL DPWRST('XXX','BUG ')
      ENDIF
C APPROXIMATION TO F PROBABILITY (SMILLIE AND ANSTEY)
      U1 = 2.0/(9.0*FLOAT(IDFN))
      U2 = 2.0/(9.0*FLOAT(IDFD))
      F1 = F**(1.0/3.0)
      U3 = ((1.0-U2)*F1-1.0+U1)/SQRT(2.0*(U2*F1*F1+U1))
      U = ABS(U3)
      PROB = 0.5/(1.0+(((.078108*U+.000972)*U+.230389)*U+.278393)*U)**4
      IF (U3.LT.0.0) PROB = 1.0 - PROB
      IF (IBST.EQ.1) GO TO 250
      IF (IPOPT.EQ.1) GO TO 230
      IF (PROB.GE.FLEVEL) GO TO 250
  230 TEMP = TEMP - F
      IF (DABS(TEMP).LE.FTOL) GO TO 250
      IF (IT.GE.MAXIT) GO TO 240
      TEMP = F
      GO TO 210
  240 CONTINUE
      WRITE (ICOUT,99993) MAXIT, ICD(1:1)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,89993) TEMP, FTOL
      CALL DPWRST('XXX','BUG ')
  250 ISIG = ISTAR
      IF (PROB.GE.FLEVEL) ISIG = IBLANK
      WRITE (IOUT,99990) IT, F, ISIG, PROB, FLEVEL
99990 FORMAT (10H ITERATION, I4, 4H, F=, F12.3, A1, 15H, PROB(F) .GT. ,
     * F7.4, 16H VS. F LEVEL OF , F7.4)
      CALL DPWRST('XXX','BUG ')
      WRITE (IOUT,89990) S
89990 FORMAT (20H SSR(REDUCED MODEL)=, E16.8)
      CALL DPWRST('XXX','BUG ')
      GO TO 150
C BALANCED CASE; ONE ITERATION
  260 CALL STEP(2, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP)
      IT = IT + 1
      IF (ICD(1:1).EQ.IM) GO TO 50
      GO TO 180
      END
      SUBROUTINE POOL(IND, NP, NS, NW, W, N, LLIM, LT, LP)
C  PART OF ACM 591 FOR ANOVA
C  ****************************** POOL ******************************   POO   10
C                                                                       POO   20
C  OPERATES UPON THE VECTORS IN ARRAY W, PRINCIPALLY THE ARRAYS OF A    POO   30
C  FACTORIAL DECOMPOSITION WITHIN VECTOR A OF W.  EITHER MOVES THE      POO   40
C  SECONDARY ARRAY INTO THE PRIMARY ARRAY, DUPLICATING ENTRIES WHERE    POO   50
C  NEEDED, OR POOLS THE SECONDARY ARRAY AND THE PRIMARY ARRAY BY AD-    POO   60
C  DITION INTO THE PRIMARY ARRAY (FOR DESCRIPTION OF MAPPING FUNCTION   POO   70
C  SEE SCHLATER AND HEMMERLE, CACM 1966)                                POO   80
C                                                                       POO   90
C  IND = 0 (REPLACEMENT); IND = 1 (POOLING)                             POO  100
C                                                                       POO  110
C  NP = BASE ADDRESS OF PRIMARY ARRAY (WITHIN ARRAY W)                  POO  120
C  NS = BASE ADDRESS OF SECONDARY ARRAY (WITHIN ARRAY W)                POO  130
C                                                                       POO  140
C  WHEN THE PRIMARY ARRAY HAS LESS THAN N SUBSCRIPTS, THE ENTRIES IN    POO  150
C  LLIM CORRESPONDING TO THE MISSING SUBSCRIPTS MUST BE MADE NEGATIVE   POO  160
C  PRIOR TO ENTRY AND THEN SET POSITIVE AGAIN AFTER RETURN; ARRAY LP    POO  170
C  MUST CONTAIN THE COEFFICIENTS OF THE MAPPING FUNCTION UPON ENTRY.    POO  180
C                                                                       POO  190
C  (SEE MAIN PROGRAM COMMENTS FOR DESCRIPTION OF OTHER ARGUMENTS)       POO  200
C                                                                       POO  210
C  ******************************************************************   POO  220
      DIMENSION W(NW), LLIM(N), LT(N), LP(10)
      DOUBLE PRECISION W, TEMP
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C NP=LOCATION OF PRIMARY ARRAY; NS=LOCATION OF SECONDARY ARRAY;
C MAP COEFFICIENTS OBTAINED FROM LP; REPLACE (IND=0); ADD (IND .NE. 0)
      LOC1 = NP
      I = 1
   10 DO 20 J=I,N
        LT(J) = 1
   20 CONTINUE
   30 LOC1 = LOC1 + 1
      LOC2 = NS + 1
      DO 40 J=1,N
        LOC2 = LOC2 + (LT(J)-1)*LP(J)
   40 CONTINUE
      TEMP = W(LOC2)
      IF (IND.NE.0) TEMP = TEMP + W(LOC1)
      W(LOC1) = TEMP
      DO 50 J=1,N
        K = N - J + 1
        IF (LLIM(K).LT.0) GO TO 50
        IF (LT(K).EQ.LLIM(K)) GO TO 50
        LT(K) = LT(K) + 1
        IF (K.EQ.N) GO TO 30
        I = K + 1
        GO TO 10
   50 CONTINUE
      RETURN
      END
      CHARACTER*1 FUNCTION IGET(ICURS, ISTRNG, LNGTH)
C  PART OF ACM 591 FOR ANOVA
C  ****************************** IGET ******************************   IGE   10
C                                                                       IGE   20
C  USED BY THE MAIN PROGRAM AND SCAN TO SEQUENTIALLY RETRIEVE CHARAC-   IGE   30
C  TERS FROM THE INPUT BUFFER.                                          IGE   40
C                                                                       IGE   50
C  ARGUMENTS - ICURS = POSITION IN CHARACTER STRING; ISTRNG = CHARAC-   IGE   60
C              TER STRING (INPUT BUFFER); LNGTH = LENGTH OF STRING.     IGE   70
C                                                                       IGE   80
C  ******************************************************************   IGE   90
      DIMENSION ISTRNG(LNGTH)
      CHARACTER*1 IBLANK, IPLUS, ICOMMA, ISTRNG
      DATA IBLANK /' '/, IPLUS /'+'/, ICOMMA /','/
   10 IGET = ISTRNG(ICURS)
      ICURS = ICURS + 1
      IF (ICURS.GT.LNGTH) RETURN
      IF (IGET.EQ.IBLANK .OR. IGET.EQ.IPLUS) GO TO 10
      IF (IGET.EQ.ICOMMA) GO TO 10
      RETURN
      END
      SUBROUTINE LABEL(NO, ICHAR, LIST, IOUT, N, LV, LOA)
C  ROUTINE FROM ACM 591 FOR ANOVA
C  ****************************** LABEL *****************************   LAB   10
C                                                                       LAB   20
C  DETERMINES THE SUBSCRIPTS OF THE PRIMARY ARRAY; CALCULATES COEFFI-   LAB   30
C  CIENTS FOR MAPPING THE SECONDARY ARRAY INTO THE PRIMARY ARRAY.       LAB   40
C  ALSO PREPARES LABELS FOR THE G-INVERSE SOLUTION AND CLASSIFICATION   LAB   50
C  MEANS; EACH LABEL IS AN ALPHANUMERIC ARRAY OF SIZE 10.               LAB   60
C                                                                       LAB   70
C                                                                       LAB   80
C                      (OUT)    ARGUMENTS      (IN)                     LAB   90
C                                                                       LAB  100
C                       LOA              NO   ICHAR  LIST               LAB  110
C                                                                       LAB  120
C               PRIMARY SUBSCRIPTS     M-I+1    0    LLIM               LAB  130
C               MAP COEFFICIENTS       M-I+1    0    LLIM               LAB  140
C               MODEL TERM LABEL       LER(I) BLANK   LE                LAB  150
C               SUBSCRIPTS LABEL       M-I+1    .     LS                LAB  160
C                                                                       LAB  170
C               IN COMPUTING NO, I IS THE POSITION OF THE               LAB  180
C               ARRAY WITHIN THE M ARRAYS (IN VECTOR A OF               LAB  190
C               W) OR, FOR MODEL TERM LABELS, THE VALUE                 LAB  200
C               OF THE E/R LIST (ARRAY LER) FOR THAT TERM               LAB  210
C                                                                       LAB  220
C  (SEE MAIN PROGRAM COMMENTS FOR DESCRIPTION OF OTHER ARGUMENTS)       LAB  230
C                                                                       LAB  240
C  ******************************************************************   LAB  250
      DIMENSION LIST(N), LV(N), LOA(10)
      CHARACTER*1 IBLANK
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA IBLANK /' '/
C
C MAP COEFFICIENTS: (NO=2**N-I+1,ICHAR=0,LIST=LLIM)
C LABELS: MODEL TERM (NO=LER(I),ICHAR= ,LIST=LE)
C         SUBSCRIPTS (NO=2**N-I+1,ICHAR=.,LIST=LS)
C
      NUM = NO - 1
      DO 10 I=N,10
CNIST   LOA(I) = IBLANK
        LOA(I) = -1
   10 CONTINUE
      DO 20 I=1,N
        LOA(I) = ICHAR
   20 CONTINUE
      IF (NUM.EQ.0) GO TO 60
      I = 0
      J = 0
   30 I = I + 1
   40 J = J + 1
      NUM = NUM - LV(J)
      IF (NUM.GE.0) GO TO 50
      NUM = NUM + LV(J)
CNIST IF (ICHAR.NE.IBLANK) GO TO 30
      IF (ICHAR.NE.-1) GO TO 30
      GO TO 40
   50 LOA(I) = LIST(J)
      IF (NUM.NE.0) GO TO 30
   60 IF (ICHAR.EQ.0) GO TO 70
CNIST WRITE (ICOUT,99999) (LOA(K),K=1,10)
99999 FORMAT (1H , 10A1)
      CALL DPWRST('XXX','BUG ')
      RETURN
   70 DO 90 I=1,N
        IF (LOA(I).EQ.0) GO TO 90
        LOA(I) = 1
        DO 80 J=I,N
          IF (LOA(J).EQ.0) GO TO 80
          LOA(I) = IABS(LOA(I)*LOA(J))
   80   CONTINUE
   90 CONTINUE
      RETURN
      END

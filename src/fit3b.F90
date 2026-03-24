!  THIS FILE CONTAINS THE FOLLOWING:
!
!     1. DPFIT   - DRIVER FOR FIT COMMAND
!
!        DPFIT2  - NON-LINEAR FIT
!        DPFIT3  - LINEAR FIT (ROUTINES FROM OMNITAB)
!        LSQRT
!        LSQ
!        SCALDP
!        PDECOM
!        SLVE
!        DSUMAL
!        SDPRED
!        PINVRT
!        DPDIV
!        SPDIV
!        DPCON
!        DPSQRT
!        SPSQRT
!        SPLO10
!        IDIV
!
!     2. BACK    - BEST SUBSETS FOR LINEAR FITS (ROUTINES FROM OMNITAB)
!        CODEXY
!        COEF
!        CPSTRE
!        CRSPRD
!        FDDIV
!        FDIV
!        FDPCON
!        FDSQRT
!        FLOG10
!        PIVOT
!        RFORMT
!        SCREEN
!
!     3. ACM591  - ANOVA ROUTINES FROM ACM 591.  NOTE THAT THESE
!        DECOMP    ARE NOT CURRENTLY IMPLEMENTED BY DATAPLOT'S
!        SCAN      ANOVA COMMAND.  INCLUDED FOR FUTURE IMPLEMENTATION.
!        STEP
!        PART1
!        PART2
!        POOL
!        IGET
!        LABEL
!
!     4. SNSQE   - NON-LINEAR EQUATIONS, SINGLE PRECISION (FROM CMLIB)
!        SNSQ
!        FDJAC1
!        QRFAC
!        QFORM
!        DOGLEG
!        R1UPDT
!        R1MPYQ
!
!     5. DNSQE   - NON-LINEAR EQUATIONS, DOUBLE PRECISION (FROM CMLIB)
!        DNSQ
!        DFDJC1
!        DQRFAC
!        DENORM
!        DQFORM
!        DDOGLG
!        D1UPDT
!        D1MPYQ
!
      SUBROUTINE DPFIT(ICAPSW,IFORSW,   &
                       IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,   &
                       IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A LEAST SQUARES FIT
!              FOR LINEAR AND NON-LINEAR MODELS.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--FEBRUARY 1988.
!     UPDATED         --FEBRUARY 1988. (SIMPLIFY THE CALL TO DPFIT3)
!     UPDATED         --MARCH    1988. (ALLOW B0 IN MULTILINEAR FIT)
!     UPDATED         --MARCH    1988. ADD LOFCDF
!     UPDATED         --MAY      1989. ALLOW OMNITAB FIT BEYOND 5 VAR.
!     UPDATED         --MAY      1989. ADDED ISUBRO IN CALL TO DPFIT3
!     UPDATED         --MAY      1989. AUTO COEF--A11, A12, A13, ...
!     UPDATED         --AUGUST   1989. NUMPAR FIXED FOR POLY FIT
!     UPDATED         --JUNE     1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!                                      ALSO, MOVE SOME DIMENSIONS FROM DPFIT2
!                                      AND DPFIT3 TO DPFIT
!     UPDATED         --JUNE     1991. REPLICATION BUG FOR POLY FIT
!     UPDATED         --SEPT     1991. EXPAND IND. VAR. 5 TO 15
!     UPDATED         --MARCH    1992. FIX INSTAB. MESSAGE (WEIGHTS)
!     UPDATED         --MARCH    1992. ISUBRO ADDED TO DPFIT2 ARG LIST
!     UPDATED         --MAY      1995. FIX SOME I/O
!     UPDATED         --MAY      1995. ADDITIONAL EQUIVALENCE
!     UPDATED         --APRIL    2002. OPTION TO OMIT CONSTANT TERM
!                                      FOR MULTILINEAR FIT
!     UPDATED         --JULY     2003. MODIFY STORAGE FOR LINEAR FIT
!                                      SO THAT > MAXCMF DEPENDENT
!                                      VARIABLES CAN BE USED (I.E.,
!                                      ADD VARIABLES AT EXPENSE OF
!                                      FEWER ROWS)
!     UPDATED         --NOVEMBER 2003. CAPTURE HTML AND LATEX FORMATS
!     UPDATED         --MAY      2009. WITH THE INCREASED DATA SET
!                                      SIZE ALLOWED, THE DPSWAP ROUTINE
!                                      WAS BECOMING A SERIOUS BOTTLE
!                                      NECK IN SOME CASES.  USE
!                                      DPCOZD.INC IN PLACE OF DPSWAP
!     UPDATED         --NOVEMBER 2016. SET FIT ADDITIVE CONSTANT FOR
!                                      POLYNOMIAL FITS
!     UPDATED         --JULY     2019. TWEAK SCRATCH SPACE
!     UPDATED         --JULY     2019. FOR DPFIT3, USE XMAT INSTEAD OF
!                                      X1 ... X15 TO REDUCE MEMORY
!                                      REQUIREMENTS
!     UPDATED         --JULY     2021. FIX BUG WITH MULTIPLE SUBSET/FOR
!                                      CLAUSES
!     UPDATED         --AUGUST   2021. ADD: RESSS, SSR, SSTO, RESMS,
!                                      AMSR, FSTAT, FCV95, FCV99 TO THE
!                                      CALL LIST TO DPFIT3 AND SAVE
!                                      THEM AS INTERNAL PARAMETERS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
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
!
!CCCC THE FOLLOWING 5 LINES WERE ADDED MAY 1989
      CHARACTER*4 IHOUT
      CHARACTER*4 IVALID
      CHARACTER*4 IHOUT1
      CHARACTER*4 IHOUT2
      CHARACTER*4 IHOUT3
!
!CCCC THE FOLLOWING 2 LINES WERE ADDED MAY 1989
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
!
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN0
!
!---------------------------------------------------------------------
!
!CCCC JULY 2003: MAKE MAXIMUM NUMBER OF PARAMETERS SETTABLE VIA
!CCCC SINGLE PARAMETER STATEMENT.
!
      PARAMETER(MAXPAR=300)
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
!
      DIMENSION IPAROC(MAXPAR)
!
      DIMENSION ITYPEH(1000)
      DIMENSION IW2HOL(1000)
      DIMENSION IW22HO(1000)
      DIMENSION W2HOLD(1000)
!
      DIMENSION PARAM(MAXPAR)
      DIMENSION IPARN(MAXPAR)
      DIMENSION IPARN2(MAXPAR)
      DIMENSION PARCOV(MAXPAR+1,MAXPAR+1)
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
      DIMENSION IVARN1(MAXPAR)
      DIMENSION IVARN2(MAXPAR)
!
      DIMENSION ICH(10)
      DIMENSION IHOUT(10)
!
      DIMENSION W(MAXOBV)
      DIMENSION VSDPRD(MAXOBV)
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
      DIMENSION DUMMY1(MAXOBV)
      DIMENSION DUMMY2(MAXOBV)
      DIMENSION DUMMY3(MAXOBV)
      DIMENSION DUMMY4(MAXOBV)
      DIMENSION DUMMY5(MAXOBV)
      DIMENSION VSCRT(10*MAXOBV)
      DIMENSION XMAT(MAXOBV*MAXCMF)
!
!-----COMMON----------------------------------------------------------
!
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      EQUIVALENCE (W(1),D(1))
      EQUIVALENCE (VSDPRD(1),D(MAXOBV+1))
      EQUIVALENCE (PRED2(1),DSIZE(1))
      EQUIVALENCE (RES2(1),DSIZE(MAXOBV+1))
      EQUIVALENCE (DUMMY1(1),DSYMB(1))
      EQUIVALENCE (DUMMY2(1),DSYMB(MAXOBV+1))
      EQUIVALENCE (DUMMY3(1),DCOLOR(1))
      EQUIVALENCE (DUMMY4(1),DCOLOR(MAXOBV+1))
      EQUIVALENCE (DUMMY5(1),DFILL(1))
      EQUIVALENCE (PARCOV(1,1),DFILL(MAXOBV+1))
      EQUIVALENCE (GARBAG(IGARB1),XMAT(1))
      EQUIVALENCE (DGARBG(IDGAR1),VSCRT(1))
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPFI'
      ISUBN2='T   '
      IERROR='NO'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IPAROC(1)='NONE'
      MAXV2=15
      MINN2=2
      MAXITS=IFITIT
      CPUEPS=R1MACH(3)
      MAXN2=MAXCHF
      MAXN3=MAXCHF
      MAXN4=MAXCHF
      NUMPV=(-999)
      IP=(-999)
      IV=(-999)
      IWIDMO=(-999)
      NUMIND=(-999)
      ICUTMX=NUMBPW
      IF(IHOST1.EQ.'CDC '.OR.IHOST1.EQ.'CYBE')ICUTMX=48
      IF(IHOST1.EQ.'205 ')ICUTMX=48
      CUTOFF=2**(ICUTMX-3)
      IVAL=0
      IDEGRE=0
      K1=0
!
!               **************************
!               **  TREAT THE FIT CASE  **
!               **************************
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IFITAC,IBUGA2,IBUGA3,NUMNAM
   53   FORMAT('IFITAC,IBUGA2,IBUGA3,NUMNAM = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGCO,IBUGEV,IBUGQ
   54   FORMAT('IBUGCO,IBUGEV,IBUGQ = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO 57 I=1,NUMNAM
          WRITE(ICOUT,58)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I),   &
                         VALUE(I)
   58     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I)',   &
                 'VALUE(I) = ',I8,2X,2A4,2X,A4,2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL CKFIT(ICASFI,ILOCFI,IBUGA3,IFOUND,IERROR)
      IF(ICASFI.EQ.'    '.OR.IFOUND.EQ.'NO')GO TO 9000
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
                  IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  STEP 3--                                         *
!               **  FOR THE CASES WHEN HAVE FIT Y = SOME EXPRESSION  *
!               **                   ROBUST FIT Y = SOME EXPRESSION, *
!               **  DETERMINE IF WE HAVE A VALID FUNCTIONAL          *
!               **  EXPRESSION--IN PARTICULAR, CHECK THAT THE NUMBER *
!               **  OF ARGUMENTS IS AT LEAST 1, AND ALSO CHECK THAT  *
!               **  THERE IS EXACTLY 1 EQUAL SIGN AND THAT THIS      *
!               **  EQUAL SIGN OCCURS AS THE SECOND ARGUMENT.        *
!               ******************************************************
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
      DO 2100 J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GO TO 2110
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GO TO 2110
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GO TO 2110
 2100 CONTINUE
      ILOCQ=NUMARG+1
      GO TO 2120
 2110 CONTINUE
      ILOCQ=J1
      GO TO 2120
 2120 CONTINUE
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        WRITE(ICOUT,2121)NUMARG,ILOCQ
 2121   FORMAT('AT 2120: NUMARG,ILOCQ = ',2I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        NUMEQ=0
        IMAX=ILOCQ-1
        DO 2130 I=1,IMAX
          IF(IHARG(I).EQ.'=   '.AND.IHARG2(I).EQ.'    ')NUMEQ=NUMEQ+1
 2130   CONTINUE
        IF(NUMEQ.NE.1)THEN
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2132)
 2132     FORMAT('      THE NUMBER OF EQUAL SIGNS DETECTED, ',I6,   &
                 ', IN MODEL NOT EQUAL 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2134)NUMARG,IMAX
 2134     FORMAT('      NUMARG, IMAX = ',2I10)
          CALL DPWRST('XXX','BUG ')
          DO 2135 I=1,NUMARG
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
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(ICASFI.EQ.'FIT'.AND.IHARG(2).NE.'=')GO TO 2200
      IF(ICASFI.EQ.'RFIT'.AND.IHARG(3).NE.'=')GO TO 2200
      GO TO 2290
!
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
      GO TO 9000
!
 2290 CONTINUE
!
!               ******************************************************
!               **  STEP 4--                                        **
!               **  FOR ALL VARIATIONS OF THE FIT COMMAND,          **
!               **  THE WORD AFTER     FIT     SHOULD BE THE RESPONSE*
!               **  VARIABLE (= THE DEPENDENT VARIABLE).            **
!               **  EXTRACT THE RESPONSE VARIABLE AND DETERMINE     **
!               **  IF IT IS ALREADY IN THE NAME LIST AND IS, IN FACT,*
!               **  A VARIABLE (AS OPPOSED TO A PARAMETER).         **
!               ******************************************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I2=0
!
      IF(ICASFI.EQ.'RFIT')THEN
        IMAX=ILOCQ-1
        DO 2330 I=1,IMAX
          I2=I
          IF(IHARG(I).EQ.'FIT')GO TO 2349
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
        DO 2336 I=1,NUMARG
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
        GO TO 9000
      ENDIF
 2349 CONTINUE
      ILOCFI=I2
!
      ILOCF1=ILOCFI+1
      IHLEFT=IHARG(ILOCF1)
      IHLEF2=IHARG2(ILOCF1)
      DO 2350 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2).AND.   &
           IUSE(I2).EQ.'V')THEN
          ILOCV=I2
          ICOLL=IVALUE(ILOCV)
          NLEFT=IN(ILOCV)
          GO TO 2390
        ENDIF
 2350 CONTINUE
!
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2362)
 2362 FORMAT('      THE NAME FOLLOWING THE WORD     FIT    (WHICH ',   &
             'SHOULD BE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2363)
 2363 FORMAT('      THE RESPONSE VARIABLE) DOES NOT EXIST IN THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2366)
 2366 FORMAT('      CURRENT NAME TABLE AS A VARIABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2369)IHLEFT,IHLEF2
 2369 FORMAT('      NAME AFTER THE WORD      FIT = ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2007)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,2008)(IANS(J),J=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
!
 2390 CONTINUE
!
!               *******************************************************
!               **  STEP 5--                                         **
!               **  FOR ALL VARIATIONS OF THE FIT COMMAND,           **
!               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)
!               **  FOR THE RESPONSE VARIABLE IS 2 OR LARGER.        **
!               *******************************************************
!
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NLEFT.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)IHLEFT,IHLEF2
  312   FORMAT('      THE NUMBER OF OBSERVATIONS IN VARIABLE ',2A4)
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
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 5.1--                                **
!               **  CHECK TO SEE IF HAVE A WEIGHTS VARIABLE.  **
!               **  IF DO HAVE, CHECK TO SEE IF A VARIABLE    **
!               **  (AS OPPOSED TO A PARAMETER).              **
!               ************************************************
!
      ISTEPN='5.1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILOCW=-99
      ICOLW=-99
      NWEIGH=-99
      IF(IWEIGH.EQ.'ON')THEN
        DO 2450 I=1,NUMNAM
          I2=I
          IF(IWEIG1.EQ.IHNAME(I2).AND.IWEIG2.EQ.IHNAM2(I2).AND.   &
             IUSE(I2).EQ.'V')THEN
            ILOCW=I2
            ICOLW=IVALUE(ILOCW)
            NWEIGH=IN(ILOCW)
            GO TO 2490
          ENDIF
 2450   CONTINUE
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2463)
 2463   FORMAT('      THE WEIGHTS VARIABLE (AS SPECIFIED VIA THE ',   &
               'WEIGHTS COMMAND)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2466)
 2466   FORMAT('      DOES NOT EXIST AS A VARIABLE IN THE CURRENT ',   &
               'NAME TABLE.')
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
        GO TO 9000
      ENDIF
!
 2490 CONTINUE
!
!               ********************************************************
!               **  STEP 6.1--                                        **
!               **  FOR THE CASES WHEN HAVE FIT Y = SOME EXPRESSION   **
!               **                   ROBUST FIT Y = SOME EXPRESSION   **
!               **  EXTRACT THE ENTIRE (LEFT AND RIGHT SIDE) FUNCTIONAL*
!               **  EXPRESSION FROM THE INPUT COMMAND LINE.  COPY     **
!               **  OUT TO IWIDTH, OR OUT TO 'SUBS' (EXCLUSIVE),      **
!               **  OR OUT THE 'EXCE' (EXCLUSIVE)                     **
!               **  OR OUT THE 'FOR' (EXCLUSIVE).                     **
!               ********************************************************
!
      ISTEPN='6.1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        IF(NUMARG.EQ.0)GO TO 4160
        IF(IHARG(1).EQ.'SUBS'.AND.IHARG2(1).EQ.'ET  ')GO TO 4160
        IF(IHARG(1).EQ.'EXCE'.AND.IHARG2(1).EQ.'PT  ')GO TO 4160
        IF(IHARG(1).EQ.'FOR '.AND.IHARG2(1).EQ.'    ')GO TO 4160
        ISTART=-99
        ISTOP=-99
        DO 4110 I=1,IWIDTH
          IP1=I+1
          IP2=I+2
          IP3=I+3
          IP4=I+4
          IP5=I+5
          IP6=I+6
          IP7=I+7
!
          IF(IP2.GT.IWIDTH)GO TO 4120
          IF(IANS(I).EQ.'F'.AND.IANS(IP1).EQ.'I'.AND.   &
             IANS(IP2).EQ.'T')ISTART=IP3
!
          IF(IP4.GT.IWIDTH)GO TO 4120
          IF(IANS(I).EQ.' '.AND.IANS(IP1).EQ.'F'.AND.   &
             IANS(IP2).EQ.'O'.AND.IANS(IP3).EQ.'R'.AND.   &
             IANS(IP4).EQ.' ')THEN
               IF(ISTOP.LE.0)THEN
                 ISTOP=I
               ELSEIF(I.LT.ISTOP)THEN
                 ISTOP=I
               ENDIF
               GO TO 4120
          ENDIF
!
          IF(IP7.GT.IWIDTH)GO TO 4120
          IF(IANS(I).EQ.' '.AND.IANS(IP1).EQ.'S'.AND.   &
             IANS(IP2).EQ.'U'.AND.IANS(IP3).EQ.'B'.AND.   &
             IANS(IP4).EQ.'S'.AND.IANS(IP5).EQ.'E'.AND.   &
             IANS(IP6).EQ.'T'.AND.IANS(IP7).EQ.' ')THEN
               IF(ISTOP.LE.0)THEN
                 ISTOP=I
               ELSEIF(I.LT.ISTOP)THEN
                 ISTOP=I
               ENDIF
               GO TO 4120
          ENDIF
!
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
          GO TO 9000
        ENDIF
!
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
          GO TO 9000
        ENDIF
!
        J=0
        DO 4150 I=ISTART,ISTOP
          J=J+1
          MODEL(J)=IANS(I)
 4150   CONTINUE
        NUMCHA=ISTOP-ISTART+1
 4160   CONTINUE
      ENDIF
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        WRITE(ICOUT,4161)ISTART,ISTOP
 4161   FORMAT('AT 4160: ISTART,ISTOP = ',2I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************************************
!               **  STEP 6.2--                                   **
!               **  FOR THE CASES WHEN HAVE ... FIT Y X       ,  **
!               **  EXTRACT THE INDEPENDENT VARIABLE,            **
!               **  AND FORM THE 1 CHARACTER PER WORD            **
!               **  REPRESENTATION OF THE MODEL.                 **
!               ***************************************************
!
      ISTEPN='6.2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASFI.EQ.'FIT')GO TO 4290
      IF(ICASFI.EQ.'RFIT')GO TO 4290
      IF(ICASFI.EQ.'MFIT')GO TO 4290
!
      ILOCRV=ILOCFI+1
      ILOCIV=ILOCFI+2
!
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
!
      I=0
!
      IWD=IHARG(ILOCRV)
      CALL DPXH1H(IWD,ICH,IEND,IBUGA3)
      IF(IEND.LE.0)GO TO 4219
      DO 4210 J=1,IEND
        I=I+1
        MODEL(I)=ICH(J)
 4210 CONTINUE
 4219 CONTINUE
!
      IWD=IHARG2(ILOCRV)
      CALL DPXH1H(IWD,ICH,IEND,IBUGA3)
      IF(IEND.GT.0)THEN
        DO 4220 J=1,IEND
          I=I+1
          MODEL(I)=ICH(J)
 4220   CONTINUE
      ENDIF
!
      KMAX=IDEGRE+1
      I=I+1
      MODEL(I)='='
!
      KMAX=IDEGRE+1
!
!     IF SET FIT ADDITIVE COMMAND ENTERED, THEN DO NOT INCLUDE
!     CONSTANT TERM.
!
      DO 4250 K=1,KMAX
        KTEMP=0
        IF(IFITAC.EQ.'OFF')THEN
          IF(K.EQ.1)GO TO 4250
          KTEMP=1
        ENDIF
        KM1=K-1
!
        IF(KM1.GT.KTEMP)THEN
          I=I+1
          MODEL(I)='+'
        ENDIF
!
        I=I+1
        MODEL(I)='A'
!
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
!
        IF(KM1.LE.0)GO TO 4250
!
        I=I+1
        MODEL(I)='*'
!
        IWD=IHARG(ILOCIV)
        CALL DPXH1H(IWD,ICH,IEND,IBUGA3)
        IF(IEND.GT.0)THEN
          DO 4260 J=1,IEND
            I=I+1
            MODEL(I)=ICH(J)
 4260     CONTINUE
        ENDIF
!
        IWD=IHARG2(ILOCIV)
        CALL DPXH1H(IWD,ICH,IEND,IBUGA3)
        IF(IEND.GT.0)THEN
          DO 4270 J=1,IEND
            I=I+1
            MODEL(I)=ICH(J)
 4270     CONTINUE
        ENDIF
!
        IF(KM1.LE.1)GO TO 4250
!
        I=I+1
        MODEL(I)='*'
        I=I+1
        MODEL(I)='*'
!
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
!
 4250 CONTINUE
 4290 CONTINUE
      IWIDMO=I
      NUMCHA=IWIDMO
!
!               **********************************************
!               **  STEP 6.3--                              **
!               **  FOR ALL VARIATIONS OF THE FIT COMMAND,  **
!               **  CHECK TO SEE THE TYPE CASE--            **
!               **    1) UNQUALIFIED (THAT IS, FULL);       **
!               **    2) SUBSET/EXCEPT; OR                  **
!               **    3) FOR.                               **
!               **********************************************
!
      ISTEPN='6.3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.GE.1)THEN
        DO 400 J=1,NUMARG
          J1=J
          IF((IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') .OR.   &
             (IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  '))THEN
            ICASEQ='SUBS'
            IKEY='SUBS'
            IF(IHARG(J1).EQ.'EXCE')IKEY='EXCE'
            IF(ILOCQ.EQ.NUMARG+1)ILOCQ=J1
            GO TO 409
          ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
            ICASEQ='FOR'
            IF(ILOCQ.EQ.NUMARG+1)ILOCQ=J1
            GO TO 409
          ENDIF
  400   CONTINUE
  409   CONTINUE
      ENDIF
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        WRITE(ICOUT,491)NUMARG,ILOCQ
  491   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************************
!               **  STEP 6.4--                              **
!               **  FOR SOME VARIATIONS OF THE FIT COMMAND, **
!               **  EXTRACT THE UNDERLYING FUNCTION         **
!               **  FROM FUNCTION DEFINITIONS.              **
!               **********************************************
!
!
      ISTEPN='6.4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        DO 5170 I=1,NUMCHA
          I2=I
          IF(MODEL(I).EQ.'=')GO TO 5175
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
        GO TO 9000
 5175   CONTINUE
        ILOCEQ=I2
!
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
!
        IF(ICASFI.EQ.'FIT'.OR.ICASFI.EQ.'RFIT')THEN
          CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,   &
                      IFUNC2,N2,IBUGA3,IFOUND,IERROR)
        ELSEIF(ICASFI.NE.'FIT'.AND.ICASFI.NE.'RFIT')THEN
          CALL DPEXST(MODEL,IWIDMO,IWD1,IWD12,IWD2,IWD22,MAXN2,   &
                      IFUNC2,N2,IBUGA3,IFOUND,IERROR)
        ENDIF
        IF(IERROR.EQ.'YES')GO TO 9000
        IF(IFOUND.EQ.'NO')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3372)
 3372     FORMAT('      INVALID COMMAND FORM FOR FITTING.  GENERAL ',   &
                 'FORM--')
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
          GO TO 9000
        ENDIF
!
        CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,   &
                    NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,   &
                    IFUNC3,N3,MAXN3,   &
                    IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        J=ILOCEQ
        DO 5180 I=1,N3
          J=J+1
          MODEL(J)=IFUNC3(I)
 5180   CONTINUE
        NUMCHA=J
!
      ENDIF
!
!               ******************************************************
!               **  STEP 7--                                        **
!               **  MAKE A NON-CALCULATING PASS AT THE MODEL        **
!               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.
!               ******************************************************
!
      ISTEPN='7'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IPASS=1
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT')THEN
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
                    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,AJUNK,   &
                    IBUGCO,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ELSEIF(ICASFI.EQ.'MFIT')THEN
!
!CCCC   APRIL 2002.  IF SET FIT ADDITIVE CONSTANT OFF ENTERED, THEN DO
!CCCC                NOT FIT A CONSTANT TERM.  UPDATE CODE BELOW
!CCCC                ACCORDINGLY.
!
        JMIN=2
        JMAX=ILOCQ-1
        MAXIND=MAXCMF-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXIND,   &
                    IHNAME,IHNAM2,IUSE,NUMNAM,   &
                    IVARN1,IVARN2,NUMIND,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 8000
!
        IF(IFITAC.EQ.'OFF')THEN
          NUMPAR=NUMIND
          ISTRT=2
          ISTOP=NUMPAR+1
        ELSE
          NUMPAR=NUMIND+1
          ISTRT=1
          ISTOP=NUMPAR
        ENDIF
!
        ICOUNT=0
        DO 6411 I5=ISTRT,ISTOP
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
!
!CCCC   THE FOLLOWING LINE WAS COMMENTED OUT MAY 1989
!CCCC   NUMIND=ILOCQ-2
!CCCC   THE FOLLOWING LINE WAS FIXED MARCH 1989
!CCCC   NUMPV=NUMIND
        NUMPV=NUMPAR
        ILOCQM=ILOCQ-1
!CCCC   THE FOLLOWING LINE WAS FIXED MAY 1989
!CCCC   DO6412I5=2,ILOCQM
        DO 6412 I5=1,NUMIND
          NUMPV=NUMPV+1
!CCCC     THE FOLLOWING LINE WAS FIXED MARCH 1989
!CCCC     J5=NUMIND+(I5-1)
!CCCC     J5=NUMIND+1+(I5-1)
          J5=NUMPAR+I5
          IPARN(J5)=IVARN1(I5)
          IPARN2(J5)=IVARN2(I5)
 6412   CONTINUE
      ELSE
!CCCC   THE FOLLOWING LINE WAS ADDED AUGUST 1989
        NUMPAR=IDEGRE+1
        IF(IFITAC.EQ.'OFF')NUMPAR=IDEGRE
        DO 6421 I5=1,NUMPAR
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
!
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
!
!               ********************************************
!               **  STEP 8--                              **
!               **  CHECK TO MAKE SURE THAT THE COMBINED  **
!               **  NUMBER OF PARAMETERS AND VARIABLES    **
!               **  IN THE MODEL IS AT LEAST 1.           **
!               ********************************************
!
      ISTEPN='8'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 9--                                        **
!               **  CHECK THAT ALL VARIABLES                        **
!               **  IN THE MODEL ARE ALREADY PRESENT                **
!               **  IN THE AVAILABLE NAME LIST IHNAME(.) AND IHNAM2(.).
!               **  CHECK THAT ALL PARAMETERS                       **
!               **  IN THE MODEL ARE ALREADY PRESENT                **
!               **  IN THE AVAILABLE NAME LIST IHNAME(.) AND IHNAM2(.).
!               **  ALL NAMES IN THE MODEL THAT ARE NOT             **
!               **  IN THE NAME LIST AT ALL WILL BE ADDED           **
!               **  TO THE LIST, DEFINED AS PARAMETERS,             **
!               **  AND GIVEN A VALUE OF 1.0.                       **
!               **  THIS ALLOWS US TO MAKE AN INITIAL FIT           **
!               **  WITHOUT HAVING TO DEFINE STARTING VALUES AT ALL **
!               **  (THEY WILL BE AUTOMATICALLY SET TO 1.0).  ALSO, **
!               **  FORM A NEW VECTOR WHICH HAS ONLY PARAMETER NAMES**
!               **  AND ANOTHER VECTOR WHICH HAS ONLY VARIABLE NAMES.*
!               ******************************************************
!
      ISTEPN='9'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IP=0
      IV=0
      DO 4165 J=1,NUMPV
        IHPARN=IPARN(J)
        IHPAR2=IPARN2(J)
        DO 4166 I=1,NUMNAM
          I2=I
          IF(IHPARN.EQ.IHNAME(I).AND.IHPAR2.EQ.IHNAM2(I).AND.   &
             IUSE(I).EQ.'V')THEN
            IV=IV+1
            IVARN3(IV)=IPARN(J)
            IVARN4(IV)=IPARN2(J)
            ICOLV3(IV)=IVALUE(I2)
            NIV(IV)=IN(I2)
            GO TO 4165
          ELSEIF(IHPARN.EQ.IHNAME(I).AND.IHPAR2.EQ.IHNAM2(I).AND.   &
             IUSE(I).EQ.'P')THEN
            IP=IP+1
            IPARN3(IP)=IPARN(J)
            IPARN4(IP)=IPARN2(J)
            PARAM3(IP)=VALUE(I2)
            GO TO 4165
          ENDIF
 4166   CONTINUE
        IP=IP+1
        IPARN3(IP)=IPARN(J)
        IPARN4(IP)=IPARN2(J)
        PARAM3(IP)=1.0
!
        IF(NUMNAM.GE.MAXNAM)THEN
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7752)
 7752     FORMAT('      THE TOTAL NUMBER OF (VARIABLE + PARAMETER) ',   &
                 'NAMES MUST')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7754)MAXNAM
 7754     FORMAT('      BE AT MOST ',I8,'.  SUCH WAS NOT THE CASE ',   &
                 'HERE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7755)
 7755     FORMAT('      THE MAXIMUM ALLOWABLE NUMBER OF NAMES WAS JUST')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7757)
 7757     FORMAT('      EXCEEDED.  SUGGESTED ACTION--ENTER    STAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7758)
 7758     FORMAT('      TO DETERMINE THE IMPORTANT (VERSUS ',   &
                 'UNIMPORTANT)')
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
          GO TO 9000
        ENDIF
!
        I2=NUMNAM+1
        IHNAME(I2)=IPARN(J)
        IHNAM2(I2)=IPARN2(J)
        IUSE(I2)='P'
        IVALUE(I2)=1
        VALUE(I2)=1.0
        IN(I2)=1
        NUMNAM=I2
        IF(ICASFI.EQ.'MFIT')GO TO 4259
        IF(ICASFI.EQ.'0FIT')GO TO 4259
        IF(ICASFI.EQ.'1FIT')GO TO 4259
        IF(ICASFI.EQ.'2FIT')GO TO 4259
        IF(ICASFI.EQ.'3FIT')GO TO 4259
        IF(ICASFI.EQ.'4FIT')GO TO 4259
        IF(ICASFI.EQ.'5FIT')GO TO 4259
        IF(ICASFI.EQ.'6FIT')GO TO 4259
        IF(ICASFI.EQ.'7FIT')GO TO 4259
        IF(ICASFI.EQ.'8FIT')GO TO 4259
        IF(ICASFI.EQ.'9FIT')GO TO 4259
        IF(ICASFI.EQ.'10FI')GO TO 4259
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
 4255     FORMAT('      THIS NAME HAS BEEN ADDED TO THE LIST, ',   &
                 'SPECIFIED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4257)
 4257     FORMAT('      AS A PARAMETER, AND GIVEN THE VALUE 1.0 .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4258)(MODEL(I),I=1,MIN(100,NUMCHA))
 4258     FORMAT('      FUNCTION EXPRESSION--',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 4259   CONTINUE
        GO TO 4165
 4165 CONTINUE
      NUMPAR=IP
      NUMVAR=IV
!
!               *******************************************
!               **  STEP 10--                            **
!               **  CHECK FOR A VALID NUMBER             **
!               **  OF INDEPENDENT VARIABLES (1 TO 5).   **
!               **  CHECK THE VALIDITY OF EACH           **
!               **  OF THE INDEPENDENT VARIABLES.        **
!               **  DOES THE NAME EXIST IN THE TABLE?    **
!               **  DOES THE NUMBER OF ELEMENTS          **
!               **  AGREE WITH THE NUMBER OF ELEMENTS    **
!               **  IN THE RESPONSE VARIABLE?            **
!               *******************************************
!
      ISTEPN='10'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC THE FOLLOWING LINE WAS INSERTED MAY 1989
      IF(ICASFI.NE.'FIT')GO TO 520
!
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
  557   FORMAT('      THE SPECIFIED NUMBER OF INDEPENDENT VARIABLES ',   &
               'WAS ',I8)
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
        DO 4505 J=1,NUMVAR
          WRITE(ICOUT,4506)J,IVARN3(J),IVARN4(J),ICOLV3(J)
 4506     FORMAT('I,IVARN3(I),IVARN4(I),ICOLV3(I) = ',I8,2X,2A4,2X,I8)
          CALL DPWRST('XXX','BUG ')
 4505   CONTINUE
        IERROR='YES'
        GO TO 9000
      ENDIF
!
  520 CONTINUE
      DO 540 J=1,NUMVAR
        IF(NIV(J).NE.NLEFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,562)
  562     FORMAT('      FOR A LEAST SQUARES FIT, THE NUMBER OF ',   &
                 'ELEMENTS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,564)
  564     FORMAT('      IN EACH INDEPENDENT VARIABLE SHOULD BE THE ',   &
                 'SAME')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,565)
  565     FORMAT('      AS THE NUMBER OF ELEMENTS IN THE DEPENDENT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,567)
  567     FORMAT('      VARIABLE (RESPONSE); SUCH WAS NOT THE CASE ',   &
                 'HERE.')
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
          DO 580 JJ=1,NUMVAR
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
          GO TO 9000
        ENDIF
  540 CONTINUE
!
!               ******************************************************
!               **  STEP 11--
!               **  DUMP THE COMMON VECTOR V(.) OUT ONTO MASS STORAGE
!               **  SO AS TO PRESERVE THEIR CONTENTS FOR LATER USE
!               **  (AFTER DPFIT2).  THE ABOVE DUMP TO MASS
!               **  STORAGE IS UNNECESSARY AND IS NOT DONE FOR
!               **  THE SPECIAL CASE WHEN THE NUMBER OF PARAMETERS IS
!               **  0 (A NO-FIT CASE WHEREBY WE ARE REALLY INTERESTED
!               **  IN GENERATING PREDICTED VALUES AND RESIDUALS
!               **  FOR A GIVEN FULLY-SPECIFIED MODEL).
!               ******************************************************
!
      ISTEPN='11'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC MAY 2009: NO LONGER NEED TO DO THIS
      IOP='WRIT'
!CCCC CALL DPSWAP(IOP,V,NUMNAM,IHNAME,IHNAM2,IUSE,IN,
!CCCC1IVALUE,MAXN,MAXCOL,MAXN2,MAXCO2,MAXIJ2,IBUGA3,ISUBRO,IERROR)
!CCCC CALL DPSWAP(IOP,NUMNAM,IHNAME,IHNAM2,IUSE,IN,
!CCCC1IVALUE,MAXN2,MAXCO2,MAXIJ2,IBUGA3,ISUBRO,IERROR)
!
!               *******************************************************
!               **  STEP 12--                                        **
!               **  BRANCH TO THE APPROPRIATE SUBCASE; THEN COPY     **
!               **  OVER THE RESPONSE VECTOR TO BE USED IN THE MODEL **
!               **  INTO THE VECTOR Y; AND                           **
!               **  COPY OVER THE WEIGHTS INTO THE VECTOR W;         **
!               **  COPY OVER THE VECTORS THAT WERE USED IN THE MODEL**
!               **  INTO THE VECTORS X1, X2, X3,X4, AND X5.          **
!               **  (MAX NUMBER OF ALLOWABLE VECTORS = 5.)           **
!               *******************************************************
!
      ISTEPN='12'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)N,NUMVAR
  601   FORMAT('N,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ICASEQ.EQ.'FULL')THEN
        DO 615 I=1,NLEFT
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
        DO 618 I=1,NLEFT
          ISUB(I)=1
  618   CONTINUE
        NQ=NLEFT
      ENDIF
!
      IROW=0
      DO 4501 I=1,NLEFT
        IF(ISUB(I).EQ.0)GO TO 4501
        IROW=IROW+1
 4501 CONTINUE
!
      K=ICOLL
      J=0
      DO 4500 I=1,NLEFT
        IF(ISUB(I).EQ.0)GO TO 4500
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
!
      K=ICOLW
      J=0
      DO 380 I=1,NLEFT
        W(I)=1.0
!CCCC   THE FOLLOWING LINE WAS MOVED    MARCH 1992
!CCCC   IF(IWEIGH.EQ.'OFF')GO TO 380
        IF(ISUB(I).EQ.0)GO TO 380
        J=J+1
!CCCC   THE FOLLOWING LINE WAS ADDED     MARCH 1992
        IF(IWEIGH.EQ.'OFF')GO TO 380
        IJ=MAXN*(K-1)+I
        IF(K.LE.MAXCOL)W(J)=V(IJ)
        IF(K.EQ.MAXCP1)W(J)=PRED(I)
        IF(K.EQ.MAXCP2)W(J)=RES(I)
        IF(K.EQ.MAXCP3)W(J)=YPLOT(I)
        IF(K.EQ.MAXCP4)W(J)=XPLOT(I)
        IF(K.EQ.MAXCP5)W(J)=X2PLOT(I)
        IF(K.EQ.MAXCP6)W(J)=TAGPLO(I)
  380 CONTINUE
!
      IF(ICASFI.EQ.'FIT' .OR. ICASFI.EQ.'RFIT' .OR.   &
         ICASFI.EQ.'MFIT')THEN
        J=0
!
        IADJ=0
        IF(IFITAC.EQ.'ON' .AND. ICASFI.EQ.'MFIT')THEN
          DO 383 I=1,NLEFT
            IF(ISUB(I).EQ.0)GO TO 383
            J=J+1
            XMAT(J)=1.0
  383     CONTINUE
          IADJ=1
        ENDIF
!
        DO 385 L=1,NUMVAR
          LP1=L+IADJ
          K=ICOLV3(L)
          J=0
          DO 386 I=1,NLEFT
            IF(ISUB(I).EQ.0)GO TO 386
            J=J+1
            IJ=MAXN*(K-1)+I
            IF(K.LE.MAXCOL)XMAT((LP1-1)*IROW + J)=V(IJ)
            IF(K.EQ.MAXCP1)XMAT((LP1-1)*IROW + J)=PRED(I)
            IF(K.EQ.MAXCP2)XMAT((LP1-1)*IROW + J)=RES(I)
            IF(K.EQ.MAXCP3)XMAT((LP1-1)*IROW + J)=YPLOT(I)
            IF(K.EQ.MAXCP4)XMAT((LP1-1)*IROW + J)=XPLOT(I)
            IF(K.EQ.MAXCP5)XMAT((LP1-1)*IROW + J)=X2PLOT(I)
            IF(K.EQ.MAXCP6)XMAT((LP1-1)*IROW + J)=TAGPLO(I)
  386     CONTINUE
  385   CONTINUE
      ELSE
        K=ICOLV3(1)
        J=0
        DO 381 I=1,NLEFT
          IF(ISUB(I).EQ.0)GO TO 381
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
!
      NS=J
!
!               ******************************************************
!               **  STEP 13--                                       **
!               **  PREPARE FOR ENTRANCE INTO DPFIT2/DPFIT3--       **
!               **  SET THE ICON3 VECTOR (WHICH INDICATES WHICH     **
!               **  PARAMETERS ARE TO BE HELD CONSTANT EQUAL TO 0   **
!               **  THROUGHOUT.  DEFINE CONSTRAINTS AND LIMITS.     **
!               ******************************************************
!
      ISTEPN='13'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 4195 I=1,NUMPAR
        ICON3(I)=0
 4195 CONTINUE
!
      IF(NUMCON.GT.0)THEN
        DO 4700 I=1,NUMPAR
          DO 4800 J=1,NUMCON
            J2=J
            IF(IPARN3(I).EQ.IPARNC(J).AND.IPARN4(I).EQ.IPANC2(J))THEN
              IPARO3(I)=IPAROC(J2)
              PARLI3(I)=PARLIM(J2)
              GO TO 4700
            ENDIF
 4800     CONTINUE
          IPARO3(I)='NONE'
 4700   CONTINUE
      ENDIF
!
!               ******************************************************
!               **  STEP 14--                                       **
!               **  CARRY OUT THE ACTUAL FIT                        **
!               **  VIA CALLING                                     **
!               **  DPFIT2 (FOR GENERAL MODELS), OR                 **
!               **  DPFIT3 (FOR POLYNOMIAL AND MULTILINEAR MODELS)  **
!               ******************************************************
!
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
        DO 6083 I=1,NS
          WRITE(ICOUT,6084)I,Y(I),XMAT(I),XMAT(I+IROW),W(I)
 6084     FORMAT('I,Y(I),XMAT(I,1),XMAT(I+IROW),W(I) = ',   &
                 I6,2X,7F10.5)
          CALL DPWRST('XXX','BUG ')
 6083   CONTINUE
        WRITE(ICOUT,6085)(MODEL(I),I=1,MIN(120,NUMCHA))
 6085   FORMAT('MODEL(.)--',120A1)
        CALL DPWRST('XXX','BUG ')
        DO 6086 J=1,NUMPAR
          WRITE(ICOUT,6087)J,IPARN3(J),IPARN4(J),PARAM3(J),ICON3(J)
 6087     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I),ICON3(I) = ',   &
                 I8,2X,2A4,E15.7,A4)
          CALL DPWRST('XXX','BUG ')
 6086   CONTINUE
        DO 6088 J=1,NUMVAR
          WRITE(ICOUT,6089)J,IVARN3(J),IVARN4(J),ICOLV3(J)
 6089     FORMAT('I,IVARN3(I),IVARN4(I),ICOLV3(I) = ',I8,2X,2A4,2X,I8)
          CALL DPWRST('XXX','BUG ')
 6088   CONTINUE
        WRITE(ICOUT,6091)IBUGA3,IBUGCO,IBUGEV,NUMIND
 6091   FORMAT('IBUGA3,IBUGCO,IBUGEV,NUMIND = ',2(A4,2X),A4,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ICASFI.EQ.'FIT')THEN
        CALL DPFIT2(Y,XMAT,IROW,   &
                    NUMVAR,IVARN3,IVARN4,W,NS,   &
                    MODEL,NUMCHA,PARAM3,IPARN3,IPARN4,NUMPAR,ICON3,   &
                    IANGLU,IPARO3,   &
                    PARLI3,VSCRT,MAXITS,FITSD,FITPOW,CPUEPS,   &
                    ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,   &
                    IREP,REPSD,REPDF,RESSD,RESDF,PRED2,RES2,ALFCDF,   &
                    DUMMY1,DUMMY2,DUMMY3,DUMMY4,DUMMY5,   &
                    ICAPSW,ICAPTY,IFORSW,IFITAU,IAUXDP,   &
                    IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 8000
      ELSE
!CCCC   JUNE 2002: CHECK TO SEE IF ALPHA PARAMETER DEFINED.
!
        ALPHA=0.95
        IHP='ALPH'
        IHP2='A   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
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
!
        CALL DPFIT3(Y,XMAT,IROW,PARCOV,MAXPAR,   &
                    NUMVAR,IVARN3,IVARN4,W,NS,   &
                    MODEL,NUMCHA,PARAM3,IPARN3,IPARN4,NUMPAR,ICON3,   &
                    VSCRT,FITSD,FITPOW,ICASFI,   &
                    IREP,REPSD,REPDF,RESSD,RESDF,PRED2,RES2,ALFCDF,BIC,   &
                    DUMMY1,DUMMY2,DUMMY4,DUMMY5,   &
                    IFITAC,ALPHA,   &
                    RSQUAR,ADJRSQ,APRESS,   &
                    RESSS,SSR,SSTO,RESMS,AMSR,FSTAT,FCV95,FCV99,   &
                    ICAPSW,ICAPTY,IFORSW,IFITAU,IAUXDP,   &
                    IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 8000
      ENDIF
!
!               ***************************************
!               **  STEP 15--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='15'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
      IREPU='ON'
      IRESU='ON'
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NLEFT,   &
      IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
!
!CCCC JUNE 2002.  ADD FOLLOWING PARAMETERS FOR MULTI-LINEAR FIT
      IF(ICASFI.EQ.'MFIT')THEN
        IH='RSQU'
        IH2='ARE '
        VALUE0=RSQUAR
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='ADJR'
        IH2='SQUA'
        VALUE0=ADJRSQ
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='PRES'
        IH2='SP  '
        VALUE0=APRESS
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='BIC '
        IH2='    '
        VALUE0=BIC
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='RESS'
        IH2='S   '
        VALUE0=RESSS
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='SSRE'
        IH2='G   '
        VALUE0=SSR
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='SSTO'
        IH2='TAL '
        VALUE0=SSTO
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='MSR '
        IH2='    '
        VALUE0=AMSR
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='MSE '
        IH2='    '
        VALUE0=RESMS
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='FSTA'
        IH2='T   '
        VALUE0=FSTAT
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='FCV9'
        IH2='5   '
        VALUE0=FCV95
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='FCV9'
        IH2='9   '
        VALUE0=FCV99
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
      ENDIF
!
      IF(ICASFI.EQ.'FIT')GO TO 7900
      IF(ICASFI.EQ.'RFIT')GO TO 7900
!
!CCCC THE FOLLOWING SECTION (DOWN TO 7640 CONTINUE) WAS REWRITTEN MAY 1989
      IF(ICASFI.EQ.'MFIT')K1=NUMPAR
      L=0
      DO 7600 J=1,K1
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
!
        DO 7650 I=1,NUMNAM
          I2=I
          IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.   &
             IUSE(I).EQ.'P')THEN
            VALUE(I2)=PARAM3(L)
            VAL=VALUE(I2)
            IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
            IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
            IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
            IVALUE(I2)=IVAL
            GO TO 7600
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
 7654     FORMAT('      SUCH WAS NOT THE CASE HERE--THE MAXIMUM ',   &
                 'ALLOWABLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7656)
 7656     FORMAT('      NUMBER OF NAMES WAS JUST EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7657)
 7657     FORMAT('      SUGGESTED ACTION--ENTER   STAT  TO DETERMINE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7659)
 7659     FORMAT('      THE IMPORTANT (VERSUS UNIMPORTANT) VARIABLES ',   &
                 'AND')
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
          GO TO 9000
        ENDIF
!
        NUMNAM=NUMNAM+1
        ILOC=NUMNAM
        IHNAME(ILOC)=IH
        IHNAM2(ILOC)=IH2
        IUSE(ILOC)='P'
        VALUE(ILOC)=PARAM3(L)
        VAL=VALUE(ILOC)
        IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
        IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
        IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
        IVALUE(ILOC)=IVAL
!
 7600 CONTINUE
 7900 CONTINUE
!
!               ******************************************************
!               **  STEP 16--
!               **  READ BACK IN FROM MASS STORAGE
!               **  THE CONTENTS OF THE V(.) VECTOR.  THE ABOVE
!               **  RETRIEVAL FROM MASS STORAGE IS UNNECESSARY AND IS
!               **  FOR THE SPECIAL CASE WHEN THE NUMBER OF PARAMETERS
!               **  IS 0 (A NO-FIT CASE WHEREBY WE ARE REALLY
!               **  INTERESTED IN GENERATING PREDICTED VALUES
!               **  AND RESIDUALS FOR A GIVEN FULLY-SPECIFIED MODEL).
!               ******************************************************
!
 8000 CONTINUE
!
      ISTEPN='16'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *************************************************
!               **  STEP 17--                                  **
!               **  COPY THE FINAL ESTIMATES FROM THE FIT      **
!               **  BACK INTO THE PARAMETERS.                  **
!               **  THESE FINAL ESTIMATES WILL THUS OVERWRITE  **
!               **  THE STARTING VALUES THAT WERE              **
!               **  ORIGINALLY ASSIGNED TO THE PARAMETERS.     **
!               *************************************************
!
      ISTEPN='17'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PFIT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMPAR.GT.0)THEN
        DO 6100 J=1,NUMPAR
          IH=IPARN3(J)
          IH2=IPARN4(J)
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          VALUE(ILOCP)=PARAM3(J)
          VAL=VALUE(ILOCP)
          IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
          IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
          IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
          IVALUE(ILOCP)=IVAL
 6100   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9017 I=1,NUMNAM
          WRITE(ICOUT,9018)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),   &
                           IVALUE(I),VALUE(I)
 9018     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I)',   &
                 'VALUE(I) = ',I8,2X,2A4,2X,A4,2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9017   CONTINUE
        WRITE(ICOUT,9021)NUMIND,NUMPV,NUMVAR,IP,IV
 9021   FORMAT('NUMIND,NUMPV,NUMVAR,IP,IV = ',5I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMPV.GT.0)THEN
          DO 9022 I=1,NUMPV
            WRITE(ICOUT,9023)I,IPARN(I),IPARN2(I)
 9023       FORMAT('I,IPARN(I),IPARN2(I) = ',I8,2X,2A4)
            CALL DPWRST('XXX','BUG ')
 9022     CONTINUE
        ENDIF
        IF(IP.GT.0)THEN
          DO 9032 I=1,IP
            WRITE(ICOUT,9033)I,IPARN3(I),IPARN4(I)
 9033       FORMAT('I,IPARN3(I),IPARN4(I) = ',I8,2X,2A4)
            CALL DPWRST('XXX','BUG ')
 9032     CONTINUE
        ENDIF
        IF(IV.GT.0)THEN
          DO 9042 I=1,IV
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
!
      RETURN
      END SUBROUTINE DPFIT
      SUBROUTINE DPFIT2(Y,XMAT,IROW,   &
                        NUMVAR,IVARN3,IVARN4,W,N,   &
                        MODEL,NUMCHA,PARAM3,IPARN3,IPARN4,NUMPAR,   &
                        ICON3,IANGLU,IPARO3,   &
                        PARLI3,V,MAXITS,FITSD,FITPOW,CPUEPS,   &
                        ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,   &
                        IREP,REPSD,REPDF,RESSD,RESDF,PRED2,RES2,ALFCDF,   &
                        DUM1,DUM2,Y2,WSQRT,G,   &
                        ICAPSW,ICAPTY,IFORSW,IFITAU,IAUXDP,   &
                        IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
!
!CCCC JUNE 1990.  ADD DUM1 - G ARGUMENTS (DIMENSIONED IN DPFIT)
!CCCC SEPT. 1991. ARGS X6 TO X15 ABOVE ARE NEW.
!CCCC JULY  2019. REPLACE X1 ... X15 WITH XMAT
!
!     LEVENBERG, MARQUARDT, MORRISON ALGORITHM IMPLEMENTED FOLLOWING
!     SUGGESTION OF GOLUB (SEE OSBORNE 'SOME ASPECTS OF NONLINEAR LEAST
!     SQUARES CALCULATION' EDITOR F.A. LOOTSMA ACADEMIC PRESS).  MAIN
!     FEATURE OF THIS ROUTINE IS AN IMPROVED TEST FOR ACCEPTING
!     PREDICTED CORRECTION AND ADJUSTING LEVENBERG PARAMETER ALAMBA
!
!     VARIABLES
!
!     PARAM3(1)   VECTOR OF INDEPENDENT VARIABLES
!            INPUT. CONTAINS ESTIMATE OF SOLUTION
!            OUTPUT. CONTAINS SOLUTION VECTOR OR LAST ATTEMPT
!
!     V(1)   STORAGE OF GRAD F BY COLUMNS
!            I.E., THE DERIVATIVES EVALUATED AT EACH OF THE N DATA POINTS
!            OF THE N RESIDUALS RES2(I) WITH RESPECT TO
!            THE FIRST PARAMETER FOLLOWED BY ALL THE DERIVATIVES
!            WITH RESPECT TO THE SECOND PARAMETER, ETC.
!
!     RES2(1)   STORAGE FOR F VECTOR OF TERMS IN SUM OF SQUARES
!            OUTPUT. VECTOR OF TERMS (USALLY RESIDUALS) IN SUM
!            OF SQUARES
!
!     SUMSQ   OUTPUT. CONTAINS SUM OF SQUARES
!
!     N      INPUT. NO. OF TERMS IN SUM OF SQUARES = NUMBER OF OBSERVATIONS.
!
!     NP     INPUT. NO. OF PARAMETERS INCLUDING ANY TO BE HELD CONSTANT
!
!     TOL    INPUT. TOLERANCE ON CALCULATION OF SUM OF SQUARES
!
!     EXPND  OUTPUT. FACTOR BY WHICH ALAMBA INCREASED IF TEST ON SUM OF
!            SQUARES FAILS, SUGGESTED VALUE 1.5
!
!     COMPR   INPUT. FACTOR BY WHICH ALAMBA COMPREASED IF TEST ON SUM OF
!            SQUARES SUCCEEDS ON FIRST ATTEMPT, SUGGESTED VALUE 0.5
!
!     ITS    INPUT. MAX NUMBER OF ITERATIONS
!            OUTPUT. ACTUAL NUMBER OF ITERATIONS
!
!     IER    INPUT.=-1+(100*NCONST)  NO PRINTING
!                  =0+(100*NCONST)  PRINTING AFTER CONVERGENCE ONLY
!                  =1+(100*NCONST)  PRINT DIAGNOSTIC INFORMATION
!                  =2+(100*NCONST)  AS ABOVE PLUS GRADIENT CHECK
!            WHERE NCONST = NO. OF PARAMETERS TO BE HELD CONSTANT
!            OUTPUT.=1 SUCCESSUL TERMINATION
!            =2 MAX ITS EXCEEDED
!            =3 ALAMBA EXCEEDS 1.D6
!            =4 ALL GRADIENTS ZERO FOR ONE OR MORE PARAMETERS
!            =5 NO. OF PARAMETERS LESS THAN ONE
!
!     C(1)   OUTPUT. CONTAINS APPROXIMATE
!            STANDARD ERRORS OF PARAMETER ESTIMATES
!
!     G(1)   OUTPUT. CONTAINS A VECTOR OF UNCORRELATED RESIDUALS
!
!     WS(1)   WORKING SPACE, MUST BE ALLOTTED AT LEAST
!            NPR*(NPR+5) + NCONST     IN CALLING PROGRAM,
!            WHERE NCONST IS THE NUMBER OF PARAMETERS TO BE HELD
!            CONSTANT AND     NPR = NP - NCONST.
!
!     ICON3(1) INPUT. ICON3(1)=1  IF THE I-TH PARAMETER IS TO BE HELD
!                               CONSTANT
!                           =0  OTHERWISE
!
!
!     USER SUPPLIED SUBROUTINE F REQUIRED TO SET VALUES OF SUMSQ,
!     F,A DECLARATION MUST BE
!             SUBROUTINE F (X,N,PARAM3,NUMPAR,F,A,SUMSQ,IFL)
!             IF IFL=1 SETS ALL VALUES
!             IF IFL=2 SETS SUMSQ ONLY MUST NOT ALTER A,F
!
!     N.B. THE VALUE OF ILF IS SUPPLIED BY DPFIT2 AND MUST NOT BE CHANGED
!
!     EPS IS A MACHINE-DEPENDENT CONSTANT.
!
!     NOTE--MAX NUMBER OF OBSERVATIONS N IS 1000 (NOT CHECKED FOR)
!     NOTE--MAX NUMBER OF PARAMETERS K IS 30 (NOT CHECKED FOR)
!     NOTE--DIMENSION OF G IS N (MAX IS 1000)
!     NOTE--DIMENSION OF C IS K (MAX IS 30)
!     NOTE--DIMENSION OF A IS N X K (BUT N X K MAX IS 10000)
!
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER 26, 1977.
!     UPDATED         --JULY      1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --OCTOBER   1978.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JUNE      1979.
!     UPDATED         --JULY      1979.
!     UPDATED         --MARCH     1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --AUGUST    1987. WEIGHTED FIT
!     UPDATED         --JANUARY   1988. FIX WEIGHTED FIT PRED & RES
!     UPDATED         --MARCH     1988. ADD LOFCDF
!     UPDATED         --JUNE      1990. MOVE SOME DIMENSIONS TO DPFIT
!     UPDATED         --JULY      1990. FIX OVERFLOW
!     UPDATED         --SEPT      1991. EXPAND IND. VAR. 5 TO 15
!     UPDATED         --MARCH     1992. FIX FORMAT MESSAGE
!     UPDATED         --MARCH     1992. WRITE COEF SDCOEF TCDF TO FILE
!     UPDATED         --MARCH     1992. ISUBRO ADDED TO INPUT ARG LIST
!     UPDATED         --FEBRUARY  1994. ACTIVATE FITSD TEST
!     UPDATED         --MAY       1994. FIX (= SPLIT) FORMAT 1122
!     UPDATED         --MAY       1994. CORRECT AN OVERFLOW DIVISION
!     UPDATED         --MAY       1995. FIX SOME I/O
!     UPDATED         --APRIL     1996. IPRINT SWITCH
!     UPDATED         --JULY      1997. PRINT SUMMARY INFORMATION IF
!                                       MAXIMUM ITERATIONS REACHED
!     UPDATED         --FEBRUARY  1998. CALL DPFLSH (FOR GUI)
!     UPDATED         --APRIL     2001. PRINT OUT VAR-COV MATRIX
!     UPDATED         --NOVEMBER  2002. CAPTURE HTML, LATEX
!     UPDATED         --MAY       2011. USE DPAUFI TO OPEN/CLOSE
!                                       DPST?F.DAT FILES
!     UPDATED         --MAY       2011. USE DPDTA1 AND DPDT5B TO
!                                       PRINT OUTPUT
!     UPDATED         --JUNE      2014. USER OPTION TO SUPPRESS
!                                       WRITING TO AUXILLARY FILES
!     UPDATED         --APRIL     2019. USER CAN SPECIFY NUMBER OF
!                                       DECIMAL POINTS FOR AUXILLARY
!                                       FILES
!     UPDATED         --JULY      2019. REPLACE X1 ... X15 WITH XMAT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IFITAU
!
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
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUND
!
      CHARACTER*4 IPARN5
      CHARACTER*4 IPARN6
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 MODEL
      CHARACTER*4 IOP
      CHARACTER*20 IFORMT
!
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
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION SUM,SSS,SSINIT,SSR,WW,SSN,SUMSQ
      DOUBLE PRECISION S
      DOUBLE PRECISION DS1,DS2,DTOL
      DOUBLE PRECISION DRAT1,DRAT2
      DOUBLE PRECISION DEPS,DTOL2,DRAT
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
!CCCC THE FOLLOWING INCLUDE STATEMENT WAS ADDED MARCH 1992
      INCLUDE 'DPCOF2.INC'
!
      DIMENSION Y(*)
      DIMENSION XMAT(IROW,*)
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
      DIMENSION W(*)
      DIMENSION V(*)
      DIMENSION DUM1(*)
      DIMENSION DUM2(*)
      DIMENSION Y2(*)
      DIMENSION WSQRT(*)
      DIMENSION G(*)
!
      DIMENSION MODEL(*)
      DIMENSION IVARN3(*)
      DIMENSION IVARN4(*)
      DIMENSION PARAM3(*)
      DIMENSION IPARN3(*)
      DIMENSION IPARN4(*)
      DIMENSION ICON3(*)
      DIMENSION IPARO3(*)
      DIMENSION PARLI3(*)
!
      DIMENSION ITYPEH(*)
      DIMENSION IW2HOL(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
      DIMENSION IPARN5(30)
      DIMENSION IPARN6(30)
      DIMENSION PARAM5(30)
!
      DIMENSION WS(1100)
      DIMENSION DUM(30)
      DIMENSION C(15)
      DIMENSION TVALU2(15)
      DIMENSION PARAM7(30)
      DIMENSION PARAM9(30)
      DIMENSION VARCOV(30,30)
      DIMENSION CORR(30,30)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPFI'
      ISUBN2='T2  '
      IERROR='NO'
!
      KMIN=0
      KMAX=0
      IY=0
      IDX=0
      IDU=0
      IDA=0
      ID=0
      NTEMP=0
      NPST=0
      CDF2=0.0
      S=0.0
      DS3=0.0
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
!CCCC THE FOLLOWING LINE WAS ADDED TO FIX OVERFLOW JULY 1990
      CPUMA2=CPUMAX/1000.0
!
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
        WRITE(ICOUT,53)IBUGA3,IBUGCO,IBUGEV,ISUBRO,IFITAC
   53   FORMAT('IBUGA3,IBUGCO,IBUGEV,ISUBRO,IFITAC = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)CPUEPS,FITPOW,FITSD
   59   FORMAT('CPUEPS,FITPOW,FITSD = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
         WRITE(ICOUT,56)I,Y(I),XMAT(I,1),W(I)
   56    FORMAT('I,Y(I),XMAT(I,1),W(I) = ',I5,3F20.10)
         CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO 61 J=1,NUMVAR
        WRITE(ICOUT,62)J,IVARN3(J),IVARN4(J)
   62   FORMAT('I,IVARN3(I),IVARN4(I) = ',I8,2X,A4,A4)
        CALL DPWRST('XXX','BUG ')
   61   CONTINUE
        DO 66 J=1,NUMPAR
          WRITE(ICOUT,67)J,IPARN3(J),IPARN4(J),PARAM3(J),ICON3(J)
   67     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I),ICON3(I) = ',   &
                 I8,2X,2A4,G15.7,I8)
          CALL DPWRST('XXX','BUG ')
   66   CONTINUE
        NTEMP=MIN(NUMCHA,100)
        WRITE(ICOUT,71)(MODEL(J),J=1,NTEMP)
   71   FORMAT('FUNCTIONAL EXPRESSION--',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!CCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
!               **************************************************
!               **  STEP 0.5--                                  **
!               **   OPEN THE STORAGE FILES                     **
!               **************************************************
!
      ISTEPN='0.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFITAU.EQ.'ON')THEN
        IOP='OPEN'
        IFLAG1=1
        IFLAG2=1
        IFLAG3=1
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  DETERMINE THE PARAMETER NAMES IN THE MODEL  **
!               **  AND THE NUMBER NUMPAR OF PARAMETERS.        **
!               **************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IPASS=2
!
      IF(NUMPAR.GT.0)THEN
        DO 7100 I=1,NUMPAR
          IPARN5(I)=IPARN3(I)
          IPARN6(I)=IPARN4(I)
          PARAM5(I)=PARAM3(I)
 7100   CONTINUE
      ENDIF
!
      IF(NUMVAR.GT.0)THEN
        DO 7300 I=1,NUMVAR
          IPARN5(NUMPAR+I)=IVARN3(I)
          IPARN6(NUMPAR+I)=IVARN4(I)
 7300   CONTINUE
      ENDIF
!
      NUMPV=NUMPAR+NUMVAR
!
!               ******************************************************
!               **  STEP 2--                                        **
!               **  DEFINE VARIOUS CONSTANTS.                       **
!               **  DEFINE EPS = MACHINE EPSILON.                   **
!               **  DEFINE TOL = CUTOFF TOLERANCE FOR SUCCESSIVE    **
!               **               ESTIMATES.                         **
!               **  DEFINE MAXITS = MAX NUMBER OF ITERATIONS.       **
!               **  DEFINE EXPND = EXPANSION FACTOR                 **
!               **  DEFINE COMPR  = COMPRESSION FACTOR              **
!               **  DEFINE NCONST = NUMBER OF PARAMETERS HELD       **
!               **                  CONSTANT.                       **
!               **  DEFINE NP = NUMBER OF NON-CONSTNAT PARAMETERS.  **
!               **  DEFINE DF = DEGREES OF FREEDOM.                 **
!               **  DEFINE SOME WORKING STORAGE START POINTS IN WS. **
!               ******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IREP='NO'
      REPSD=0.0
      REPDF=0.0
      IREPDF=INT(REPDF+0.5)
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
        DO 501 I=1,NUMPAR
          IF(ICON3(I).EQ.1)NCONST=NCONST+1
  501   CONTINUE
        NP=NUMPAR-NCONST
        IF(NP.LE.0) THEN
          WRITE(ICOUT,117) NP
117       FORMAT(10X,'NUMBER OF PARAMETERS TO BE VARIED = ',I8,   &
                 ' (LESS THAN ONE)')
          CALL DPWRST('XXX','BUG ')
          IER = 5
          IERROR='YES'
          GO TO 9000
        ENDIF
        DF=N-NP
        RESDF=DF
        IRESDF=INT(DF+0.5)
        IC=0
        IER=2
        IDA=NP*NP
        IDU=IDA+NP
        ID =IDU+NP
        IDX=ID +NP
        IY =IDX+NP
      ENDIF
!
!               **********************************************
!               **  STEP 2.2--                              **
!               **  COMPUTE THE SQUARE ROOT OF THE WEIGHTS  **
!               **********************************************
!
      ISTEPN='2.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 550 I=1,N
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
          GO TO 9000
        ELSEIF(W(I).EQ.0.0)THEN
          WSQRT(I)=W(I)
        ELSE
          WSQRT(I)=SQRT(W(I))
        ENDIF
  550 CONTINUE
!
!          ***************************************************
!          *  STEP 2.3--                                    **
!          *  FORM A NEW RESPONSE VECTOR  ( =               **
!          *  THE OLD RESPONSE * SQUARE ROOT OF WEIGHTS  (  **
!          ***************************************************
!
      ISTEPN='2.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 560 I=1,N
        Y2(I)=Y(I)*WSQRT(I)
  560 CONTINUE
!
!               ******************************************************
!               **  STEP 2.5--                                      **
!               **  CHECK FOR REPLICATION AND IF EXISTENT           **
!               **  COMPUTE A (MODEL-FREE) REPLICATION STANDARD     **
!               **  DEVIATION.                                      **
!               ******************************************************
!
      ISTEPN='2.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPREPS(Y,XMAT,IROW,N,NUMVAR,DUM1,DUM2,   &
                  IREP,REPSS,REPMS,REPSD,REPDF,NUMSET,   &
                  IBUGA3,IERROR)
      IREPDF=INT(REPDF+0.5)
!
!     PRINT INTIAL INFORMATION (BEFORE ANY FIT ITERATIONS)
!
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
!
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
!
        IMIN=1
        IF(MODEL(1).EQ.' ')IMIN=2
        IMAX=NUMCHA
        IDEL=IMAX-IMIN+1
        NUMLIN=((IDEL-1)/43)+1
        IF(NUMLIN.GE.1)THEN
          DO 47240 KLINE=1,NUMLIN
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
            DO 47245 K=KMIN,KMAX
              ICNT2=ICNT2+1
              ITEXT(ICNT)(ICNT2:ICNT2)=MODEL(K)(1:1)
47245       CONTINUE
            NCTEXT(ICNT)=ICNT2
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
47240     CONTINUE
        ENDIF
!
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
!
        NUMROW=ICNT
        DO 2310 I=1,NUMROW
          NTOT(I)=15
 2310   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                    NCTEXT,AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGA3,IERROR)
!
!       DEFINE HEADERS FOR THE INTERMEDIATE ITERATIONS
!
        ITITLE=' '
        NCTITL=-99
        ITITL9=' '
        NCTIT9=0
!
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
!
!       RESTRICT THE NUMBER OF PARAMETERS PER LINE DEPENDING
!       ON OUTPUT FORMAT
!
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
!
        DO 3101 J=1,NUMCLI
          DO 3102 I=1,MAXLIN
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
 3102     CONTINUE
          DO 3103 I=1,MAXROW
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
            IDIGI2(I,J)=-6
 3103     CONTINUE
 3101   CONTINUE
!
        ITITL2(1,1)=' '
        NCTIT2(1,1)=0
        ITITL2(2,1)='Iteration'
        NCTIT2(2,1)=9
        ITITL2(3,1)='Number'
        NCTIT2(3,1)=6
!
        ITITL2(1,2)=' '
        NCTIT2(1,2)=0
        ITITL2(2,2)='Convergence'
        NCTIT2(2,2)=11
        ITITL2(3,2)='Measure'
        NCTIT2(3,2)=7
!
        ITITL2(1,3)='Residual'
        NCTIT2(1,3)=8
        ITITL2(2,3)='Standard'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Deviation'
        NCTIT2(3,3)=9
!
        ITITL2(1,4)=' * '
        NCTIT2(1,4)=3
        ITITL2(2,4)=' * '
        NCTIT2(2,4)=3
        ITITL2(3,4)=' * '
        NCTIT2(3,4)=3
!
        ITITL2(1,5)=' '
        NCTIT2(1,5)=0
        ITITL2(2,5)='Parameter'
        NCTIT2(2,5)=10
        ITITL2(3,5)='Estimates'
        NCTIT2(3,5)=10
!
        NMAX=0
        DO 3110 I=1,NUMCOL
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
!
        ICNT=0
!
      ENDIF
!
!               *******************************************************
!               **  STEP 2.6--                                       **
!               **  TREAT THE SPECIAL CASE WHERE NO PARAMETERS       **
!               **  EXIST IN THE MODEL--                             **
!               **  THAT IS, WE ARE REALLY INTERESTED                **
!               **  IN GENERATING PREDICTED VALUES AND RESIDUALS     **
!               **  FROM A FULLY-SPECIFIED MODEL.                    **
!               **  (THIS IS USEFUL FOR MANUALLY ARRIVING AT         **
!               **  REASONABLE STARTING VALUES FOR A MORE            **
!               **  COMPLICATED FIT;                                 **
!               **  AND ALSO FOR TESTING THE GOODNESS OF AN          **
!               **  ALREADY-DERIVED                                  **
!               **  FIT FOR ONE DOMAIN OVER A SECOND DOMAIN.)        **
!               *******************************************************
!
      ISTEPN='2.6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMPAR.LE.0)THEN
        DO 3000 I=1,N
          IF(NUMVAR.GT.0)THEN
            DO 3005 J=1,NUMVAR
              PARAM5(NUMPAR+J)=XMAT(I,J)
 3005       CONTINUE
            CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM5,IPARN5,IPARN6,NUMPV,   &
                        IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,   &
                        PRED2(I),   &
                        IBUGCO,IBUGEV,IERROR)
            PRED2(I)=PRED2(I)*WSQRT(I)
            IF(IERROR.EQ.'YES')GO TO 9000
          ENDIF
 3000   CONTINUE
!
        DO 3100 I=1,N
          RES2(I)=Y2(I)-PRED2(I)
 3100   CONTINUE
!
        SUM=0.0
        DO 3200 I=1,N
          SUM=SUM+RES2(I)**2
 3200     CONTINUE
        RESSS=SUM
!
        IRESDF=N
        RESDF=N
        RESMS=0.0
        IF(RESDF.GT.0.0)RESMS=RESSS/RESDF
        RESSD=0.0
        IF(RESMS.GT.0.0)RESSD=SQRT(RESMS)
        GO TO 5000
      ENDIF
!
!               ******************************************************
!               **  STEP 3--                                        **
!               **  USING THE GIVEN STARTING VALUES FOR THE         **
!               **  PARAMETERS,                                     **
!               **  COMPUTE PREDICTED VALUES AND EXACT DERIVATIVES; **
!               **  THEN CHECK THE CORRECTNESS OF THE DERIVATIVES   **
!               **  FORMULAE                                        **
!               **  BY APPROXIMATING THE DERIVATIVES WITH DIFFERENCES*
!               **  AND COMPARING THE EXACT DERIVATIVES WITH THE    **
!               **  DIFFERENCES.                                    **
!               ******************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,425)
  425   FORMAT('    GRADIENTS FROM DIFFERENCES')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 1201 J=1,NUMPAR
        PARAM5(J)=PARAM3(J)
 1201 CONTINUE
!
      DO 1200 I=1,N
        IF(NUMVAR.GE.1)THEN
          DO 1205 J=1,NUMVAR
            PARAM5(NUMPAR+J)=XMAT(I,J)
 1205     CONTINUE
        ENDIF
!
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM5,IPARN5,IPARN6,NUMPV,   &
                    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,DUM1(I),   &
                    IBUGCO,IBUGEV,IERROR)
        DUM1(I)=DUM1(I)*WSQRT(I)
        IF(IERROR.EQ.'YES')GO TO 9000
 1200 CONTINUE
!
      SUM=0.0
      DO 1140 I=1,N
        G(I)=Y2(I)-DUM1(I)
        SUM=SUM+G(I)**2
 1140 CONTINUE
      SSN=SUM
!
      DO 1210 J=1,NUMPAR
        PARAM7(J)=PARAM3(J)
 1210 CONTINUE
!
      DO 1220 J=1,NP
        IF(ICON3(J).EQ.1)GO TO 1220
!
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,119)J
  119     FORMAT('PARAMETER NUMBER ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        PARAM7(J)=PARAM3(J)
        IF(PARAM7(J).EQ.0.0)H=0.001
        IF(PARAM7(J).NE.0.0)H=PARAM3(J)*0.01
        PARAM7(J)=PARAM3(J)+H
        DO 1230 I=1,N
          IF(NUMVAR.GE.1)THEN
            DO 1235 JJ=1,NUMVAR
              PARAM7(NUMPAR+JJ)=XMAT(I,JJ)
 1235       CONTINUE
          ENDIF
!
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM7,IPARN5,IPARN6,NUMPV,   &
                      IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,   &
                      PRED2(I),   &
                      IBUGCO,IBUGEV,IERROR)
          PRED2(I)=PRED2(I)*WSQRT(I)
          IF(IERROR.EQ.'YES')GO TO 9000
          K=I+(J-1)*N
          V(K)=(PRED2(I)-DUM1(I))/H
          V(K)=-V(K)
 1230   CONTINUE
!
        SUM=0.0
        DO 1250 I=1,N
          RES2(I)=Y2(I)-PRED2(I)
          SUM=SUM+RES2(I)**2
 1250   CONTINUE
        S=SUM
!
        DO 1260 I=1,N
          RES2(I)=(RES2(I)-G(I))/H
 1260   CONTINUE
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
          DO 1261 I=1,N
            WRITE(ICOUT,120)RES2(I)
  120       FORMAT(G15.7)
            CALL DPWRST('XXX','BUG ')
 1261     CONTINUE
        ENDIF
!
        PARAM7(J)=PARAM3(J)
 1220 CONTINUE
!
!
!
!               ************************************************
!               **  STEP 4--                                  **
!               **  START THE ITERATIVE CYCLE.                **
!               **          ITS = THE ITERATION NUMBER.       **
!               **          NITS = THE NUMBER OF ITERATIONS.  **
!               ************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITS=0
   40 CONTINUE
      ITS=ITS+1
      NITS=0
!
!               *****************************************************
!               **  STEP 5--                                       **
!               **  FILL THE VECTOR V(.) WITH EVALUATED DERIVATIVES**
!               **  BASED ON THE STARTING VALUES FOR THE PARAMETERS.*
!               **  ALL THE DERIVATIVES WITH RESPECT TO PARAMETER 1**
!               **  GO IN THE FIRST N LOCATIONS.                   **
!               **  ALL THE DERIVATIVES WITH RESPECT TO PARAMETER 2**
!               **  GO IN THE NEXT N LOCATIONS.                    **
!               **  ALL THE DERIVATIVES WITH RESPECT TO PARAMETER 3**
!               **  GO IN THE FOLLOWING N LOCATIONS, ETC.          **
!               **  ALSO COMPUTE A SUM OF SQUARED DEVIATIONS       **
!               **  BASED ON THE CURRENT VALUES FOR THE PARAMETERS **
!               **  (THIS WILL BE USED FOR COMPARATIVE PURPOSES    **
!               **  WITHIN THE ITERATION).                         **
!               *****************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1301 J=1,NUMPAR
        PARAM5(J)=PARAM3(J)
 1301 CONTINUE
      DO 1300 I=1,N
        IF(NUMVAR.GE.1)THEN
          DO 1305 J=1,NUMVAR
            PARAM5(NUMPAR+J)=XMAT(I,J)
 1305     CONTINUE
        ENDIF
!
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM5,IPARN5,IPARN6,NUMPV,   &
                    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,PRED2(I),   &
                    IBUGCO,IBUGEV,IERROR)
        PRED2(I)=PRED2(I)*WSQRT(I)
        IF(IERROR.EQ.'YES')GO TO 9000
 1300 CONTINUE
!
      DO 1310 J=1,NUMPAR
        PARAM7(J)=PARAM3(J)
 1310 CONTINUE
      DO 1320 J=1,NUMPAR
        IF(PARAM3(J).EQ.0.0)H=0.001
        IF(PARAM3(J).NE.0.0)H=PARAM3(J)*0.01
        PARAM7(J)=PARAM3(J)+H
        DO 1330 I=1,N
          IF(NUMVAR.GE.1)THEN
            DO 1335 JJ=1,NUMVAR
              PARAM7(NUMPAR+JJ)=XMAT(I,JJ)
 1335       CONTINUE
          ENDIF
!
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM7,IPARN5,IPARN6,NUMPV,   &
                      IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,Y1,   &
                      IBUGCO,IBUGEV,IERROR)
          Y1=Y1*WSQRT(I)
          IF(IERROR.EQ.'YES')GO TO 9000
          K=I+(J-1)*N
          V(K)=(Y1-PRED2(I))/H
          V(K)=-V(K)
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
            WRITE(ICOUT,1333)J,I,PARAM3(J),PARAM7(J),H,   &
                             Y1,PRED2(I),V(K)
 1333       FORMAT(I2,I4,3F10.5,3D14.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
 1330   CONTINUE
        PARAM7(J)=PARAM3(J)
 1320 CONTINUE
!
      SUM=0.0
      DO 1340 I=1,N
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
!
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
 2412   FORMAT(I5,' ROWS BY ',I5,' COLUMNS (PLUS AN EXTRA ',   &
               'COLUMN FOR RES)')
        DO 2420 I=1,IMAX
          L=0
          DO 2430 J=1,JMAX
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
        DO 2450 I=1,IMAX
          L=0
          DO 2460 J=1,JMAX
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
!
!     PRINT RESULTS FOR CURRENT ITERATION
!
      IF(IPRINT.EQ.'ON')THEN
        IF(ICNT.GT.55)THEN
          CALL DPDTA5(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
          CALL DPFLSH(IPR,IBUGA3,ISUBRO,IFOUND,IERROR)
          ICNT=0
        ELSE
          NLINE=((NUMPAR-1)/NTEMP) + 1
          DO 3910 KK=1,NLINE
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
            DO 3920 JJ=INDX1,INDX2
              ICNT3=ICNT3+1
              AMAT(ICNT,4+ICNT3)=PARAM3(JJ)
 3920       CONTINUE
 3910     CONTINUE
        ENDIF
      ENDIF
!
!               ******************************************************
!               **  STEP 6--                                        **
!               **  TO ENHANCE COMPUTATIONAL ACCURACY,              **
!               **  SCALE THE "MATRIX" V(.) OF DERIVATIVES          **
!               **  SO THAT COLUMNS HAVE LENGTH 1.                  **
!               **  STORE THE SCALE FACTOR FOR COLUMN (PARAMETER) 1 **
!               **  IN WS(ID+1).                                    **
!               **  STORE THE SCALE FACTOR FOR COLUMN (PARAMETER) 2 **
!               **  IN WS(ID+2).                                    **
!               **  STORE THE SCALE FACTOR FOR COLUMN (PARAMETER) 3 **
!               **  IN WS(ID+3),                                    **
!               ******************************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          GO TO 9000
        ENDIF
!
        IF(SUM.GT.0.0)DS3=DSQRT(SUM)
        IF(SUM.LE.0.0)DS3=0.0
        IF(DS3.LE.0.0)THEN
          WRITE(ICOUT,76)
   76     FORMAT('ERROR IN DPFIT2--DENOMINATOR DS3 = 0.0 AT FORMAT 76')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        SUM=1.0D0/DS3
        DO 3 J=1,N
          V(II+J)=V(II+J)*SUM
    3   CONTINUE
        WS(ID+I)=SUM
    1 CONTINUE
      WS(ID+I)=SUM
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,100)ITS,ALAMBA,SSINIT
  100   FORMAT (7H   ITS=,I3,8H ALAMBA=,G14.6,7H SUMSQ=,D14.6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **  STEP 7--                                         **
!               **  OPERATE ON THE "MATRIX" V(.) AND THE VECTOR RES. **
!               **  PERFORM HOUSEHOLDER TRANSFORMATION ON            **
!               **  SCALED DERIVATIVE MATRIX AND COLUMN OF RESIDUALS,**
!               **  AND TEST FOR SINGULARITIES.                      **
!               *******************************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
!       (RANK DEFICIENCY DETECTED--
!       CONTINUE ITERATING WITH PARAMETER II FIXED.
!       GO BACK TO BEGINNING OF CYCLE
!       FOR A NEW ITERATION.
!       NOTE THAT THE INPUT VECTOR ICON3(.) IS HERE
!       BEING ALTERED DUE TO THIS RANK DEFICIENCY.)
!
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
!
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
!
!               ******************************************************
!               **  STEP 8--                                        **
!               **  COMPUTE SSR = PARTIAL SUM OF SQUARED RESIDUALS  **
!               **  (NOTE THAT THE RESIDUALS HAVE JUST BEEN ALTERED).*
!               ******************************************************
!
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NP1=NP+1
      SSR=0.D0
      DO 22 I=NP1,N
        SSR=SSR+RES2(I)**2
   22 CONTINUE
!
!               ******************************************************
!               **  STEP 9--                                        **
!               **  ADD ON THE LAMBDA TO THE                        **
!               **  DIAGONAL ELEMENTS OF R'R                        **
!               **  FOR THE LEFT-HAND SIDE OF THE EQUATION.         **
!               **  TRANSFORM THE RIGHT-HAND SIDE OF THE EQUATION.  **
!               **  THE UPPER TRIANGLE OF THE TRANSFORMED MATRIX IS **
!               **  STORED IN WS                                    **
!               **  ELEMENT (I,J) OF THE TRANSFORMED MATRIX STORED IN*
!               **  ELEMENT   (I-1)*NP + J    OF WS.                **
!               ******************************************************
!
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
!               ******************************************************
!               **  STEP 10--                                       **
!               **  BACK SUBSTITUTE.                                **
!               **  COEFFICIENTS OF THE DERIVATIVE FIT WILL END UP  **
!               **  IN ELEMENTS IDX+1, IDX+2, ... OF WS.            **
!               **  UPDATED VALUES OF THE PARAMETERS WILL END UP    **
!               **  IN ELEMENTS IY+1, IY+2, ... OF WS.              **
!               ******************************************************
!
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC THE FOLLOWING LINE WAS FIXED TO AVOID OVERFLOWS    MAY 1994
!CCCC WS(IY)=WS(IY)/WS(ID)
      IF(ABS(WS(IY)).LE.CPUMAX/10000)THEN
         WS(IY)=WS(IY)/WS(ID)
      ENDIF
!
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
!
!       TEST FOR CONSTRAINTS
!
        IOP=IPARO3(II)
        IF(IOP.NE.'NONE')THEN
          PLIM=PARLI3(II)
          PUP=WS(IY+II)
          IF(IOP.EQ.'GT')THEN
            IF(PUP.LE.PLIM)PUP=PLIM
          ELSEIF(IOP.EQ.'GE')THEN
            IF(PUP.LT.PLIM)PUP=PLIM
          ELSEIF(IOP.EQ.'EQ')THEN
            IF(PUP.NE.PLIM)PUP=PLIM
          ELSEIF(IOP.EQ.'LE')THEN
            IF(PUP.GT.PLIM)PUP=PLIM
          ELSEIF(IOP.EQ.'LT')THEN
            IF(PUP.GE.PLIM)PUP=PLIM
          ENDIF
          WS(IY+II)=PUP
        ENDIF
!
32    CONTINUE
      NITS=NITS+1
!
!               *******************************************************
!               **  STEP 11--                                        **
!               **  BASED ON THE UPDATED PARAMETERS,                 **
!               **  COMPUTE THE LATEST RESIDUAL STANDARD DEVIATION.  **
!               **  TEST FOR CONVERGENCE.                            **
!               *******************************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1350 II=1,NUMPAR
        PARAM9(II)=WS(IY+II)
 1350 CONTINUE
      DO 1400 IZ=1,N
        IF(NUMVAR.GE.1)THEN
          DO 1405 J=1,NUMVAR
            PARAM9(NUMPAR+J)=XMAT(IZ,J)
 1405     CONTINUE
        ENDIF
!
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM9,IPARN5,IPARN6,NUMPV,   &
                    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,PRED2(IZ),   &
                    IBUGCO,IBUGEV,IERROR)
        PRED2(IZ)=PRED2(IZ)*WSQRT(IZ)
        IF(IERROR.EQ.'YES')GO TO 9000
 1400 CONTINUE
!
      SUM=0.0
      DO 1420 IZ=1,N
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
      IF(RESSD.LT.FITSD)GO TO 1440
      GO TO 1460
 1440 CONTINUE
      IC=1
      DO 1450 I=1,NPST
        PARAM3(I)=WS(IY+I)
 1450 CONTINUE
      GO TO 220
!
 1460 CONTINUE
      DPSI=0.5D0*(SSINIT-SSN)/(SSINIT-SSS)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,203)NITS,ALAMBA,SSN,SSS,DPSI,RESSD
  203   FORMAT(1H ,'NITS=',I8,' ALAMBA=',E15.7,' SUMSQ=',D15.7,   &
               ' RES SUMSQ=',D15.7,' PSI =',E15.7,' RESSD = ',D15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,221)SSINIT,SSS,SSN
  221   FORMAT('SSINIT,SSS,SSN = ',3D15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,227)N,NUMPAR,NCONST,NP,DF,RESDF,IRESDF
  227   FORMAT('N,NUMPAR,NCONST,NP,DF,RESDF,IRESDF = ',4I8,2E15.7,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DRAT=0.0
      IF(SSINIT.GT.0.0)DRAT=SSS/SSINIT
      DTOL2=1.0D0-DEPS*50.0D0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,224)SSINIT,SSS,DRAT,DTOL2
  224   FORMAT('SSINIT,SSS,DRAT,DTOL2= ',4D20.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(DTOL2.LE.DRAT.AND.DRAT.LE.1.0D0)GO TO 28
      IF(DPSI.GE.1.0D-04) GO TO 28
      IF(DPSI.GE.0.0D0.AND.RESSD.LT.0.000001)GO TO 28
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
 3049 FORMAT('              E.G., DIVIDING OR MULTIPLYING BY, SAY, ',   &
             '1000')
      CALL DPWRST('XXX','BUG ')
      GO TO 910
!
   28 CONTINUE
      DO 29 I=1,NPST
        PARAM3(I)=WS(IY+I)
   29 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,201)(I,PARAM3(I),I=1,NPST)
  201   FORMAT (4(8H PARAM3(,I2,1H),G14.6))
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IER=2
      IF(ITS.GE.MAXITS)GO TO 220
      IER=1
      IF(SSINIT.GT.0.0D0)DS1=DSQRT(SSINIT)
      IF(SSINIT.LE.0.0D0)DS1=0.0D0
      IF(SSS.GT.0.0D0)DS2=DSQRT(SSS)
      IF(SSS.LE.0.0D0)DS2=0.0D0
      DRAT1=DS2/DS1
      DRAT2=(DS1-DS2)/(1.0D0+DS1)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,222)SSINIT,SSS,DS1,DS2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,223)DRAT1,DRAT2,DTOL
  222   FORMAT('SSINIT,SSS,DS1,DS2= ',4D16.9)
        CALL DPWRST('XXX','BUG ')
  223   FORMAT('DRAT1,DRAT2,DTOL = ',3D16.9)
      ENDIF
!
      IF(DRAT2.LE.DTOL)GO TO 220
      IF(NITS.EQ.1) ALAMBA=ALAMBA*COMPR
      IC=0
      GO TO 40
!
!     THE ABOVE 'GO TO 40' MARKS THE USUAL END OF AN ITERATION.
!
!**** CONVERGENCE TEST SATISFIED OR MAXITS REACHED
!
220   CONTINUE
      SUMSQ=SSN
      IF(IC.EQ.1) GO TO 78
      IF(SSINIT-SSN.LE.SSINIT*1000.*EPS) GO TO 78
      IF(ITS.GE.MAXITS)THEN
        WRITE(ICOUT,204)ITS
  204   FORMAT(21X,'FAILED TO CONVERGE IN ',I6,' ITERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9204)
 9204   FORMAT(21X,'NOTE THAT THE FOLLOWING SUMMARY STATISTICS ARE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9205)
 9205   FORMAT (21X,'NOT THE BEST THAT CAN BE OBTAINED.')
        CALL DPWRST('XXX','BUG ')
!CCCC   JULY 1997.  PRINT SUMMARY INFORMATION EVEN IF MAX ITERATIONS
!CCCC   REACHED.  CHANGE FOLLOWING LINE.
!CCCC   GO TO 910
        GO TO 2999
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,205)
  205   FORMAT (1H ,20X,'EVIDENCE OF CONVERGENCE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,100)ITS,ALAMBA,SSN
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,201)(I,PARAM3(I),I=1,NPST)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IC=1
      ALAMBA=ALAMBA*COMPR
      GO TO 40
!
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
!
!**** INVERT UPPER TRIANGULAR MATRIX
!
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
!
!**** MULTIPLY INVERSE BY ITS TRANSPOSE
!
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
!
!               *******************************************************
!               **  STEP 12.2--                                      **
!               **  PRINT OUT FINAL PARAMETER ESTIMATES              **
!               **  AND THEIR STANDARD DEVIATIONS.                   **
!               **  ALSO PRINT OUT THE RESIDUAL STANDARD DEVIATION.  **
!               *******************************************************
!
!CCCC JULY 1997.  PRINT SUMMARY INFORMATION IF MAX ITERATIONS REACHED.
!CCCC ADD FOLLOWING LINE.
!CCCC NOVEMBER 2016.  NEED TO DO SOME COMPUTATIONS IN THIS SECITON,
!CCCC SO JUST SKIP THE CALL TO PRINTING THE TABLE.
 2999 CONTINUE
!CCCC IF(IPRINT.EQ.'ON')THEN
!
!       PRINT REST OF ITERATIONS TABLE
!
        IF(ICNT.GE.1)THEN
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
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
!
        NUMCOL=6
        NUMLIN=2
!
        DO 4101 J=1,NUMCLI
          DO 4102 I=1,MAXLIN
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
            NCOLSP(I,J)=0
 4102     CONTINUE
          DO 4103 I=1,MAXROW
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
            ROWSEP(I)=0
 4103     CONTINUE
 4101   CONTINUE
!
        ITITL2(1,1)=' '
        NCTIT2(1,1)=0
        NCOLSP(1,1)=1
        ITITL2(2,1)=' '
        NCTIT2(2,1)=0
        NCOLSP(2,1)=1
!
        ITITL2(1,2)=' '
        NCTIT2(1,2)=0
        NCOLSP(1,2)=3
        ITITL2(2,2)='Final Parameter Estimates'
        NCTIT2(2,2)=25
        NCOLSP(2,2)=3
!
        ITITL2(1,5)='Approximate'
        NCTIT2(1,5)=11
        NCOLSP(1,5)=1
        ITITL2(2,5)='Standard Deviation'
        NCTIT2(2,5)=18
        NCOLSP(2,5)=1
!
        ITITL2(1,6)=' '
        NCTIT2(1,6)=0
        NCOLSP(1,6)=1
        ITITL2(2,6)='t-Value'
        NCTIT2(2,6)=7
        NCOLSP(2,6)=1
!
        NMAX=0
        DO 4110 I=1,NUMCOL
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
          DO 4113 J=1,MAXROW
            IDIGI2(J,I)=NUMDIG
            IF(I.EQ.1)THEN
              IDIGI2(J,I)=0
            ELSEIF(I.EQ.6)THEN
              IDIGI2(J,I)=4
            ENDIF
 4113     CONTINUE
 4110   CONTINUE
!
        KK=1
        J=0
        ICNT=0
        DO 4120 I=1,NP
!
 4188     CONTINUE
          II=I+J
          K=ICON3(II)
          J=J+K
!
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
            GO TO 4188
          ENDIF
          IF(WS(KK).GT.0.0)C(I)=SQRT(WS(KK))
          IF(WS(KK).LE.0.0)C(I)=0.0
          KK=KK+I+1
!
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
!
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
!
      IF(IPRINT.EQ.'ON')THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
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
!               *********************************************
!               **  STEP 13--                              **
!               **  PRINT OUT GOODNESS OF FIT INFORMATION  **
!               *********************************************
!
 5000 CONTINUE
!
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
!
      IF(IPRINT.EQ.'ON')THEN
        ITITLE=' '
        NCTITL=0
        ITITLZ=' '
        NCTITZ=0
!
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
!
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
!
        NUMROW=ICNT
        DO 2410 I=1,NUMROW
          NTOT(I)=15
 2410   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                    NCTEXT,AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
!CCCC JULY 1997.  MAX ITERATIONS FIX
      IF(ITS.GE.MAXITS) GO TO 910
      IF(NUMPAR.LE.0)GO TO 9000
!
!               ********************************************
!               **  PRINT OUT CORRELATIONS OF REGRESSION  **
!               **  COEFFICIENT ESTIMATES                 **
!               **  (IF CALLED FOR)                       **
!               ********************************************
!
      IF(NP.GE.N) GO TO 910
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
         WRITE(ICOUT,108)
108      FORMAT(20X,'CORRELATIONS OF PARAMETER ESTIMATES')
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
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
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIT2')THEN
          WRITE(ICOUT,209) II,(WS(IY+J),J=1,I)
  209     FORMAT(I6,(10F12.5))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
95    CONTINUE
      IF(X0.GT.0.0)X0=SQRT(X0)
      IF(X0.LE.0.0)X0=0.0
      DO 1501 J=1,NUMPAR
        PARAM5(J)=PARAM3(J)
 1501 CONTINUE
      DO 1500 I=1,N
        IF(NUMVAR.GE.1)THEN
          DO 1505 J=1,NUMVAR
            PARAM5(NUMPAR+J)=XMAT(I,J)
 1505     CONTINUE
        ENDIF
!
        CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM5,IPARN5,IPARN6,NUMPV,   &
                    IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,PRED2(I),   &
                    IBUGCO,IBUGEV,IERROR)
        PRED2(I)=PRED2(I)*WSQRT(I)
        IF(IERROR.EQ.'YES')GO TO 9000
 1500 CONTINUE
      DO 1510 J=1,NUMPAR
        PARAM7(J)=PARAM3(J)
 1510 CONTINUE
      DO 1520 J=1,NUMPAR
        IF(PARAM3(J).EQ.0.0)H=0.001
        IF(PARAM3(J).NE.0.0)H=PARAM3(J)*0.01
        PARAM7(J)=PARAM3(J)+H
        DO 1530 I=1,N
          IF(NUMVAR.GE.1)THEN
            DO 1535 JJ=1,NUMVAR
              PARAM7(NUMPAR+JJ)=XMAT(I,JJ)
 1535       CONTINUE
          ENDIF
!
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM7,IPARN5,IPARN6,NUMPV,   &
                      IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,Y1,   &
                      IBUGCO,IBUGEV,IERROR)
          Y1=Y1*WSQRT(I)
          IF(IERROR.EQ.'YES')GO TO 9000
          K=I+(J-1)*N
          V(K)=(Y1-PRED2(I))/H
          V(K)=-V(K)
 1530   CONTINUE
        PARAM7(J)=PARAM3(J)
 1520 CONTINUE
!
      SUM=0.0
      DO 1540 I=1,N
        RES2(I)=Y2(I)-PRED2(I)
        SUM=SUM+RES2(I)**2
 1540 CONTINUE
      SUMSQ=SUM
!
!**** FORM UNWEIGHTED (RAW) PREDICTED VALUES AND RESIDUALS
!
      DO 1550 I=1,N
        IF(WSQRT(I).LE.0.0)GO TO 1550
        RES2(I)=Y2(I)-PRED2(I)
        RES2(I)=RES2(I)/WSQRT(I)
        PRED2(I)=Y(I)-RES2(I)
 1550 CONTINUE
!
!**** RELOCATE VAR-COV. MATRIX AND STANDARD ERRORS IF NCONST.NE.0.
!
!CCCC THE FOLLOWING LINE WAS CHANGED MARCH 1992
!C900 IF(NCONST.EQ.0) GO TO 9000
      IF(NCONST.EQ.0) GO TO 919
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
!CCCC IF(K) 902,901
      IF(K.LT.0)GO TO 902
      IF(K.EQ.0)GO TO 901
903   I = I - 1
      IF(I.GT.0) GO TO 904
910   NP = NPST
!CCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
  919 CONTINUE
!
!CCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
!               **************************************************
!               **  STEP 81--                                   **
!               **  WRITE INFO OUT TO FILES--                   **
!               **     1) DPST1F.DAT--COEF SDCOEF TCDF          **
!               **     2) DPST2F.DAT--PRED AND SDPRED           **
!               **     3) DPST3F.DAT--PARAMETER VAR-COV MATRIX  **
!               **************************************************
!
      ISTEPN='86'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFITAU.EQ.'OFF')GO TO 9000
!
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
!
      WRITE(IOUNI1,8613)
 8613 FORMAT(1X,   &
               'COEFFICIENT     ',   &
               'COEF SD         ',   &
               'T-VALUE         ')
      DO 8610 I=1,NUMPAR
        WRITE(IOUNI1,IFORMT)PARAM3(I),C(I),TVALU2(I),   &
                     IPARN3(I),IPARN4(I)
 8610 CONTINUE
!
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
!
      WRITE(IOUNI2,8624)
 8624 FORMAT(1X,   &
               'PARAMETER CORR  ',   &
               'PARAMETER COV   ')
      DO 8623 I=1,NP
        WRITE(IOUNI2,IFORMT) (CORR(I,J),J=1,NP)
        WRITE(IOUNI3,IFORMT) (VARCOV(I,J),J=1,NP)
 8623 CONTINUE
!8625 FORMAT(30(E15.7,1X))
!
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
!
!CCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
!               **************************************
!               **  STEP 82--                       **
!               **  CLOSE       THE STORAGE FILES.  **
!               **************************************
!
      ISTEPN='82'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFITAU.EQ.'ON')THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,N,NUMVAR,NUMPAR,NUMCHA
 9013   FORMAT('IERROR,N,NUMVAR,NUMPAR,NUMCHA = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NUMPAR
          WRITE(ICOUT,9016)I,IPARN3(I),IPARN4(I),PARAM3(I)
 9016     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I) = ',I8,2X,A4,A4,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO 9020 I=1,N
          WRITE(ICOUT,9021)I,Y(I),XMAT(I,1),XMAT(I,2),W(I),   &
                           PRED2(I),RES2(I)
 9021     FORMAT('I,Y(I),XMAT(I,1),XMAT(I,2),W(I),PRED2(I),RES2(I) = ',   &
                 I8,6G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        DO 9025 I=1,N
          WRITE(ICOUT,9026)I,Y(I),Y2(I),W(I),WSQRT(I)
 9026     FORMAT('I,Y(I),Y2(I),W(I),WSQRT(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9025   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPFIT2
      SUBROUTINE DPFIT3(Y,X,NLEFT,PARCOV,MAXPAR,   &
                        NUMVAR,IVARN3,IVARN4,W,N,   &
                        MODEL,NUMCHA,PARAM3,IPARN3,IPARN4,NUMPAR,ICON3,   &
                        SCR,FITSD,FITPOW,ICASFI,   &
                        IREP,REPSD,REPDF,RESSD,RESDF,PRED2,RES2,ALFCDF,   &
                        BIC,DUM1,DUM2,Z,VSDPRE,   &
                        IFITAC,ALPHA,RSQUAR,ADJRSQ,APRESS,   &
                        RESSS,SSR,SSTO,RESMS,AMSR,FSTAT,FCV95,FCV99,   &
                        ICAPSW,ICAPTY,IFORSW,IFITAU,IAUXDP,   &
                        IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
!
!     NOTE--MAX NUMBER OF OBSERVATIONS N IS 1000 (NOT CHECKED FOR)
!     NOTE--MAX NUMBER OF PARAMETERS K IS 30 (NOT CHECKED FOR)
!     NOTE--DIMENSION OF G IS N (MAX IS 1000)
!     NOTE--DIMENSION OF C IS K (MAX IS 30)
!     NOTE--DIMENSION OF A IS N X K (BUT N X K MAX IS 10000)
!
!     MORE DIMENSION INFO (FROM LSQRT)--
!           B     VECTOR OF COEFFICIENTS (M+1 BY 1).
!           Z     VECTOR OF RESIDUALS (N BY 1).
!           T     VECTOR OF STANDARD DEVIATIONS OF COEFFICIENTS (M+1 BY 1).
!           V     VECTOR OF STANDARD DEVIATIONS OF PREDICTED VALUES
!                    (N BY 1).
!           S     VECTOR OF SQUARED FOURIER COEFFICIENTS (M+3 BY 1).  THE
!                    FIRST M ELEMENTS OF THIS ARRAY ARE SUMS OF SQUARES
!                    WHICH CAN BE USED IN AN ANALYSIS OF VARIANCE.  THE
!                    LAST TWO ELEMENTS OF S ARE NOT COMPUTED IN THIS SUB-
!                    ROUTINE BUT ARE RESERVED FOR QUANTITIES TO BE COMPUTED
!                    IN THE CALLING PROGRAM.
!           E     RESIDUAL SUM OF SQUARES.
!           D     AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN INITIAL
!                    SOLUTION AND THE FIRST ITERATION (IN SUBROUTINE SLVE).
!           SD    RESIDUAL STANDARD DEVIATION.
!           NDF   NO. OF DEGREES OF FREEDOM.
!           SCR   A SCRATCH VECTOR USED FOR INTERNAL CALCULATIONS
!           ID    ID = 0  EVERYTHING IS OK.
!                 ID = 1  AUGMENTED MATRIX IS SINGULAR.
!                 ID = 2  ITERATION PROCEDURE FAILED TO CONVERGE.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--JUNE      1987.
!     UPDATED         --FEBRUARY  1988.   (MAKE LINE NUMBERS ORDERLY)
!     UPDATED         --MARCH     1988.  (INCLUDE B0 IN MULTILINEAR FIT)
!     UPDATED         --MARCH     1988.  LOFCDF
!     UPDATED         --MARCH     1988.  ERROR ARG. TO CALL TO LSQRT + BRANC
!     UPDATED         --SEPTEMBER 1988.  ERROR BRANCH AFTER CALL TO DPREPS IF EM
!     UPDATED         --SEPTEMBER 1988.  CONSTANT FIT
!     UPDATED         --NOVEMBER  1988.  PROPER TITLE FOR MULTILINEAR
!     UPDATED         --MAY       1989.  MATRIX X ADDED TO INPUT ARG LIST
!     UPDATED         --MAY       1989.  ISUBRO ADDED TO INPUT ARG LIST
!     UPDATED         --NOVEMBER  1989.  S(.) DOUB. PREC. TO SING. PREC.
!     UPDATED         --NOVEMBER  1989.  OMITTED UNNEEDED DOUB. PREC.
!     UPDATED         --JUNE      1990.  SOME DIMENSIONS MOVED TO DPFIT
!     UPDATED         --MARCH     1992.  WRITE COEF SDCOEF TCDF TO FILE
!     UPDATED         --JULY      1993.  WRITE DIAGONAL OF HAT MATRIX,
!                                        PARAMETER COVARIANCE MATRIX TO
!                                        FILE.
!     UPDATED         --SEPTEMBER 1993.  ADD ISUBRO ARG TO LSQRT
!     UPDATED         --JANUARY   1994. WRITE SDPRED & LIMITS TO FILE
!     UPDATED         --FEBRUARY  1994. MERGE JIM AND ALAN UPDATES
!                                       ADD DPST4F.DAT
!     UPDATED         --FEBRUARY  1994. DPWRST: 'BUG ' => 'WRIT'
!     UPDATED         --JUNE      1994. BUG IN DPST4F.DAT OUTPUT FOR
!                                       POLYNOMIAL MODELS.
!     UPDATED         --MAY       1995. FIX SOME I/O
!     UPDATED         --SEPTEMBER 1995. ADD BLANK LINE FOR OUTPUT
!     UPDATED         --JANUARY   1996. FIX BOMB WITH CONSTANT FIT
!     UPDATED         --APRIL     1996. IPRINT SWITCH
!     UPDATED         --APRIL     2002. SUPPORT FOR NO CONSTANT TERM
!     UPDATED         --APRIL     2002. PRINT ERROR MESSAGE IF
!                                       SINGULARITY DETECTED
!     UPDATED         --JUNE      2002. AUGMENT DPST2F.DAT OUTPUT
!     UPDATED         --JUNE      2002. AUGMENT DPST3F.DAT OUTPUT
!     UPDATED         --JUNE      2002. WRITE ANOVA TABLE TO
!                                       DPST5F.DAT
!     UPDATED         --JULY      2003. MODIFY DIMENSIONING OF X TO
!                                       ALLOW MORE FLEXIBILITY BETWEEN
!                                       NUMBER OF ROWS AND COLUMNS.
!     UPDATED         --OCTOBER   2003. SUPPORT HTML, LATEX OUTPUT
!     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
!     UPDATED         --MAY       2011. USE DPAUFI TO OPEN/CLOSE
!                                       DPST?F.DAT FILES
!     UPDATED         --MAY       2011. USE DPDTA1 AND DPDT5B TO PRINT
!                                       OUTPUT
!     UPDATED         --OCTOBER   2013. COMPUTE BIC STATISTIC
!     UPDATED         --JUNE      2014. USER OPTION TO SUPPRESS
!                                       WRITING TO AUXILLARY FILES
!     UPDATED         --APRIL     2019. USER CAN SPECIFY NUMBER OF
!                                       DECIMAL POINTS FOR AUXILLARY
!                                       FILES
!     UPDATED         --AUGUST    2021. ADD: RESSS, SSR, SSTO, RESMS,
!                                       AMSR, FSTAT, FCV95, FCV99 TO
!                                       CALL LIST
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IVARN3
      CHARACTER*4 IVARN4
      CHARACTER*4 IPARN3
      CHARACTER*4 IPARN4
      CHARACTER*4 ICASFI
      CHARACTER*4 IREP
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
!CCCC THE FOLLOWING LINE WAS INSERTED MAY 1989
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IPARN5
      CHARACTER*4 IPARN6
!
      CHARACTER*4 IHOLD3
      CHARACTER*4 IHOLD4
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 MODEL
      CHARACTER*4 IFITAC
      CHARACTER*4 IOP
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IFITAU
      CHARACTER*20 IFORMT
!
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
!
!---------------------------------------------------------------------
!
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT    NOVEMBER 1989
!CCCC BECAUSE THE VARIABLES WERE NEVER USED
!CCCC DOUBLE PRECISION SUM,SSS,SSINIT,SSR,WW,SSN,SUMSQ
!
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT NOVEMBER 1989
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC DOUBLE PRECISION S
!
!CCCC THE FOLLOWING 3 LINES WERE COMMENTED OUT    NOVEMBER 1989
!CCCC BECAUSE THE VARIABLES WERE NEVER USED
!CCCC DOUBLE PRECISION DS1,DS2
!CCCC DOUBLE PRECISION DRAT1,DRAT2
!CCCC DOUBLE PRECISION DRAT
!
      DOUBLE PRECISION DSUM1
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
!CCCC THE FOLLOWING INCLUDE STATEMENT WAS ADDED MARCH 1992
      INCLUDE 'DPCOF2.INC'
      DIMENSION Y(*)
      DIMENSION X(NLEFT,*)
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
      DIMENSION W(*)
      DIMENSION DUM1(*)
      DIMENSION DUM2(*)
      DIMENSION Z(*)
      DIMENSION VSDPRE(*)
      DIMENSION SCR(*)
!
      DIMENSION MODEL(*)
!
      DIMENSION IVARN3(*)
      DIMENSION IVARN4(*)
      DIMENSION PARAM3(*)
      DIMENSION IPARN3(*)
      DIMENSION IPARN4(*)
      DIMENSION ICON3(*)
!
      DIMENSION IPARN5(80)
      DIMENSION IPARN6(80)
      DIMENSION PARAM5(80)
!
      DIMENSION C(80)
      DIMENSION PARCOV(MAXPAR+1,MAXPAR+1)
!
      DIMENSION B(100)
      DIMENSION T(101)
      DIMENSION S(102)
!
! ****  THE ABOVE DIMENSION IS PROBABLY WRONG FOR LARGE DATA SETS    JULY 1987
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPFI'
      ISUBN2='T3  '
      IERROR='NO'
!
      CDF2=0.0
      S=0.0
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIT3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)N,NLEFT,NUMVAR,NUMPAR,NUMCHA,ICASFI
   52   FORMAT('N,NLEFT,NUMVAR,NUMPAR,NUMCHA,ICASFI = ',5I8,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IBUGA3,IBUGCO,IBUGEV,ISUBRO
   53   FORMAT('IBUGA3,IBUGCO,IBUGEV,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)FITPOW,FITSD
   54   FORMAT('FITPOW,FITSD = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y(I),X(I,1),X(I,2),X(I,3),X(I,5),W(I)
   56     FORMAT('I,Y(I),X(I,1),X(I,2),X(I,3),X(I,4),W(I) = ',I5,6E13.6)
          CALL DPWRST('XXX','WRIT')
   55   CONTINUE
        DO 61 J=1,NUMVAR
          WRITE(ICOUT,62)J,IVARN3(J),IVARN4(J)
   62     FORMAT('I,IVARN3(I),IVARN4(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','WRIT')
   61   CONTINUE
        DO 66 J=1,NUMPAR
          WRITE(ICOUT,67)J,IPARN3(J),IPARN4(J),PARAM3(J),ICON3(J)
   67     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I),ICON3(I) = ',   &
                 I8,2X,A4,A4,G15.7,I8)
          CALL DPWRST('XXX','WRIT')
   66   CONTINUE
        WRITE(ICOUT,71)(MODEL(J),J=1,MAX(100,NUMCHA))
   71   FORMAT('FUNCTIONAL EXPRESSION--',100A1)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!CCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
!               **************************************************
!               **  STEP 0.5--                                  **
!               **   OPEN THE STORAGE FILES                     **
!               **************************************************
!
      ISTEPN='0.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFITAU.EQ.'ON')THEN
        IOP='OPEN'
        IFLAG1=1
        IFLAG2=1
        IFLAG3=1
        IFLAG4=1
        IFLAG5=1
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               **************************************************
!               **  STEP 11--                                   **
!               **  DETERMINE THE PARAMETER NAMES IN THE MODEL  **
!               **  AND THE NUMBER NUMPAR OF PARAMETERS.        **
!               **************************************************
!
      ISTEPN='11'
!CCCC IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)   MAY 1989
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMPAR.GE.1)THEN
        DO 1110 I=1,NUMPAR
          IPARN5(I)=IPARN3(I)
          IPARN6(I)=IPARN4(I)
          PARAM5(I)=PARAM3(I)
 1110   CONTINUE
      ENDIF
!
      IF(NUMVAR.GE.1)THEN
        DO 1120 I=1,NUMVAR
          IPARN5(NUMPAR+I)=IVARN3(I)
          IPARN6(NUMPAR+I)=IVARN4(I)
 1120   CONTINUE
      ENDIF
!
      NUMPV=NUMPAR+NUMVAR
!
!               ********************************************************
!               **  STEP 12--                                         **
!               **  DEFINE VARIOUS CONSTANTS.                         **
!               **  DEFINE NCONST = NUMBER OF PARAMETERS HELD CONSTANT.*
!               **  DEFINE NP = NUMBER OF NON-CONSTNAT PARAMETERS.    **
!               **  DEFINE DF = DEGREES OF FREEDOM.                   **
!               **  DEFINE SOME WORKING STORAGE START POINTS IN WS.   **
!               ********************************************************
!
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IREP='NO'
      REPSD=0.0
      REPDF=0.0
      IREPDF=INT(REPDF+0.5)
      RESSD=0.0
      RESDF=0.0
      IRESDF=0
      ALFCDF=(-999.99)
!
      IF(NUMPAR.LE.0)GO TO 1239
      NPST=NUMPAR
      NCONST=0
!
      DO 1210 I=1,NUMPAR
        IF(ICON3(I).EQ.1)NCONST=NCONST+1
 1210 CONTINUE
      NP=NUMPAR-NCONST
!
      IF(NP.LE.0)THEN
        WRITE(ICOUT,1220)
 1220   FORMAT('***** ERROR IN FIT--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1221)NP
 1221   FORMAT('      THE NUMBER  OF PARAMETERS TO BE VARIED = ',I8,   &
               ' (LESS THAN ONE)')
        CALL DPWRST('XXX','WRIT')
        IER = 5
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DF=N-NP
      RESDF=DF
      IRESDF=INT(DF+0.5)
!
      IC=0
      IER=2
      IDA=NP*NP
      IDU=IDA+NP
      ID =IDU+NP
      IDX=ID +NP
      IY =IDX+NP
!
 1239 CONTINUE
!
      IDEGRE=NUMPAR-1
      IF(IFITAC.EQ.'OFF')IDEGRE=NUMPAR
!
!
!               **********************************************
!               **  STEP 13--                               **
!               **  CHANGE THE WEIGHTS VECTOR W(.)          **
!               **  SO THAT THE SUM OF SQUARED WEIGHTS = 1  **
!               **********************************************
!
      ISTEPN='13'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ********************************************************
!               **  STEP 21--                                         **
!               **  CHECK FOR REPLICATION AND IF EXISTENT COMPUTE     **
!               **  A (MODEL-FREE) REPLICATION STANDARD DEVIATION.    **
!               ********************************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     COLUMN 1 CONTAINS THE CONSTANT TERM, SO START IN COLUMN 2
!     FOR REPLICATION TEST.  IF THE FIT CONSTANT HAS BEEN TURNED
!     OFF, THEN START IN COLUMN 1.
!
      IF(IFITAC.EQ.'OFF')THEN
        CALL DPREPS(Y,X,NLEFT,N,NUMVAR,DUM1,DUM2,   &
                    IREP,REPSS,REPMS,REPSD,REPDF,NUMSET,IBUGA3,IERROR)
      ELSE
        CALL DPREPS(Y,X(1,2),NLEFT,N,NUMVAR,DUM1,DUM2,   &
                    IREP,REPSS,REPMS,REPSD,REPDF,NUMSET,IBUGA3,IERROR)
      ENDIF
      IREPDF=INT(REPDF+0.5)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *******************************************************
!               **  STEP 31--                                        **
!               **  CARRY OUT THE LEAST SQUARES FIT                  **
!               **  NOTE--IT = 1 IMPLIES POLYNOMIAL                  **
!               **        IT = 2 IMPLIES MULTILINEAR                 **
!               **  NOTE--M = DEGREE (IF POLYNOMIAL)                 **
!               **        M = NUMBER OF PARAMETERS (IF MULTILINEAR)  **
!               *******************************************************
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASFI.EQ.'MFIT')THEN
        IT=2
        M=NUMPAR
        NR=NLEFT
      ELSE
        IT=1
        M=NUMPAR-1
        IF(IFITAC.EQ.'OFF')M=NUMPAR
        NR=NLEFT
      ENDIF
!
!     THE FOLLOWING CHUNK OF CODE WAS ADDED SEPTEMBER 1988
!     TO HANDLE THE CONSTANT FIT (Y = CONSTANT + ERROR) CASE.
!
      IF(IT.EQ.1.AND.M.EQ.0)THEN
        SUMWY=0.0
        SUMW=0.0
        DO 3172 I=1,N
          SUMWY=SUMWY+W(I)*Y(I)
          SUMW=SUMW+W(I)
 3172   CONTINUE
        AMEAN=SUMWY/SUMW
        B(1)=AMEAN
        DO 3173 I=1,N
          Z(I)=Y(I)-AMEAN
 3173   CONTINUE
        NDF=N-1
        ANDF=NDF
        AN=N
        SUMWY=0.0
        DO 3174 I=1,N
          SUMWY=SUMWY+W(I)*Z(I)**2
 3174   CONTINUE
        SD=0.0
        IF(NDF.GT.0)SD=SUMWY/ANDF
        IF(SD.LE.0.0)SD=0.0
        IF(SD.GT.0.0)SD=SQRT(SD)
        T(1)=SD/SQRT(AN)
        GO TO 3190
      ELSE
!
!CCCC   APRIL 2002.  CHECK FOR CERTAIN KINDS OF SINGULARITIES IN
!CCCC                MULTI-LINEAR FITS:
!CCCC                1) ANY COLUMNS ARE CONSTANTS.
!CCCC                2) ANY COLUMNS ARE EQUAL.
        IF(ICASFI.EQ.'MFIT')THEN
          IF(IFITAC.EQ.'ON')THEN
            ISTRT=2
            ISTOP=NUMPAR
          ELSE
            ISTRT=1
            ISTOP=NUMPAR
          ENDIF
          DO 3176 J=ISTRT,ISTOP
            AHOLD=X(1,J)
            DO 3178 I=1,N
              IF(AHOLD.NE.X(I,J))GO TO 3176
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
 3185       FORMAT('      THIS RESULTS IN A SINGULAR MATRIX.  NO FIT ',   &
                   'PERFORMED.')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
 3176     CONTINUE
!
          DO 13176 J=ISTRT,ISTOP
            DO 13179 K=ISTRT,ISTOP
              IF(J.EQ.K)GO TO 13179
              DO 13181 I=1,N
                IF(X(I,J).NE.X(I,K))GO TO 13179
13181         CONTINUE
              WRITE(ICOUT,3181)
              CALL DPWRST('XXX','WRIT')
              INDX=J
              INDX2=K
              IF(IFITAC.EQ.'ON')THEN
                INDX=J-1
                INDX2=K-1
              ENDIF
              WRITE(ICOUT,13183)IVARN3(INDX),IVARN4(INDX),IVARN3(INDX2),   &
                                IVARN4(INDX2)
13183         FORMAT('      VARIABLE ',2A4,' HAS ALL VALUES = TO ',   &
                     'VARIABLE ',2A4)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,13185)
13185         FORMAT('      THIS RESULTS IN A SINGULAR MATRIX.  NO ',   &
                     'FIT PERFORMED.')
              CALL DPWRST('XXX','WRIT')
              IERROR='YES'
              GO TO 9000
13179       CONTINUE
13176     CONTINUE
        ENDIF
!
      ENDIF
!
      CALL LSQRTX(Y,W,N,X,NR,M,IT,   &
                  B,Z,T,VSDPRE,S,E,D,SD,NDF,SCR,ID,IFITAC,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
 3190 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
        WRITE(ICOUT,3191)N,M,NUMPAR
 3191   FORMAT('N,M,NUMPAR = ',3I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!               *******************************************************
!               **  STEP 32--                                        **
!               **  IF NEEDED, COMPUTE PREDICTED VALUES              **
!               **  AND RESIDUALS.                                   **
!               **  COPY OVER PARAMETERS, ETC.                       **
!               *******************************************************
!
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC JUNE 2002.  ADD SOME COMPUTATIONS USED FOR THE ANOVA TABLE
!
      IWRITE='OFF'
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
!
      DSUM1=0.0D0
      DO 3210 I=1,N
        RES2(I)=Z(I)
        PRED2(I)=Y(I)-RES2(I)
        DSUM1=DSUM1 + DBLE(PRED2(I) - YMEAN)**2
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
          WRITE(ICOUT,3211)I,Y(I),PRED2(I),RES2(I)
 3211     FORMAT('I,Y(I),PRED2(I),RES2(I) = ',I8,3E15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 3210 CONTINUE
!
      SSR=REAL(DSUM1)
!
      DO 3220 I=1,NUMPAR
        PARAM3(I)=B(I)
        C(I)=T(I)
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
          WRITE(ICOUT,3221)I,PARAM3(I),C(I)
 3221     FORMAT('I,PARAM3(I),C(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 3220 CONTINUE
!
      RESSD=SD
      RESDF=NDF
      RESMS=RESSD*RESSD
      RESSS=RESMS*RESDF
!
!     COMPUTE BIC VALUE:
!
!     BIC = N*LOG(RESVAR) + P*LOG(N)
!
!     NOTE THAT RESVAR FOR BIC USES DENOMINATOR OF N RATHER THAN
!     (N - P).  SO ADJUST FOR BIC.
!
      RESVAR=RESSD**2
      SSQTMP=REAL(N-NP)*RESVAR
      RESVA2=SSQTMP/REAL(N)
      BIC=REAL(N)*LOG(RESVA2) + REAL(NP)*LOG(REAL(N))
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')THEN
        WRITE(ICOUT,3231)RESSD,RESDF,RESMS,RESSS
 3231   FORMAT('RESSD,RESDF,RESMS,RESSS = ',4E15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!               *********************************************
!               **  STEP 42--                              **
!               **  PRINT OUT FIT TABLES                   **
!               *********************************************
!
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
!
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
!
        DO 2301 I=1,MAXROW
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
!
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
!
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
!
        NUMROW=ICNT
        DO 2310 I=1,NUMROW
          NTOT(I)=15
 2310   CONTINUE
!
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                    NCTEXT,AVALUE,IDIGIT,   &
                    NTOT,NUMROW,   &
                    ICAPSW,ICAPTY,ILAST,IFRST,   &
                    ISUBRO,IBUGA3,IERROR)
        ITITLE=' '
        NCTITL=-99
        ITITL9=' '
        NCTIT9=0
!
        NUMCOL=6
        NUMLIN=2
!
        DO 4101 J=1,NUMCLI
          DO 4102 I=1,MAXLIN
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
            NCOLSP(I,J)=0
 4102     CONTINUE
          DO 4103 I=1,MAXROW
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
            ROWSEP(I)=0
 4103     CONTINUE
 4101   CONTINUE
!
        ITITL2(1,1)=' '
        NCTIT2(1,1)=0
        NCOLSP(1,1)=1
        ITITL2(2,1)=' '
        NCTIT2(2,1)=0
        NCOLSP(2,1)=1
!
        ITITL2(1,2)=' '
        NCTIT2(1,2)=0
        NCOLSP(1,2)=3
        ITITL2(2,2)='Parameter Estimates'
        NCTIT2(2,2)=19
        NCOLSP(2,2)=3
!
        ITITL2(1,5)='Approximate'
        NCTIT2(1,5)=11
        NCOLSP(1,5)=1
        ITITL2(2,5)='Standard Deviation'
        NCTIT2(2,5)=18
        NCOLSP(2,5)=1
!
        ITITL2(1,6)=' '
        NCTIT2(1,6)=0
        NCOLSP(1,6)=1
        ITITL2(2,6)='t-Value'
        NCTIT2(2,6)=7
        NCOLSP(2,6)=1
!
        NMAX=0
        DO 4110 I=1,NUMCOL
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
          DO 4113 J=1,MAXROW
            IDIGI2(J,I)=NUMDIG
            IF(I.EQ.1)THEN
              IDIGI2(J,I)=0
            ELSEIF(I.EQ.6)THEN
              IDIGI2(J,I)=4
            ENDIF
 4113     CONTINUE
 4110   CONTINUE
!
        DO 4120 I=1,NUMPAR
!
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
!
          AMAT(I,1)=REAL(I)
          IVALUE(I,2)(1:4)=IPARN3(I)
          IVALUE(I,2)(5:8)=IPARN4(I)
          NCVALU(I,2)=8
!
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
!
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
!
        ICNT=NUMPAR
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
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
!CCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
!               ************************************************
!               **  STEP 81--                                 **
!               **  WRITE INFO OUT TO FILES--                 **
!               **     1) DPST1F.DAT--COEF SDCOEF TCDF        **
!               **        JUNE 2002: ADD JOINT BONFERRNI      **
!               **        CONFIDENCE INTERVAL FOR PARAMETERS  **
!               **     2) DPST2F.DAT--SDPRED, CONFIDENCE      **
!               **        INTERVAL FOR PREDICTED VALUES       **
!               **     3) DPST3F.DAT--REGRESSION DIAGNOSTICS  **
!               **     4) DPST4F.DAT--CORR MATRIX             **
!               **     5) DPST5F.DAT--ADD ANOVA TABLE (AND    **
!               **        R-SQUARE, ADJUSTED R-SQUARE, MALLOWS**
!               **        CP, PRESS P STATISTICS              **
!               **        ADDED JUNE 2002                     **
!               ************************************************
!
      ISTEPN='86'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFITAU.EQ.'OFF')GO TO 8619
!
!CCCC JUNE 2002.  ADD T-VALUE AND JOINT BONFERONI CONFIDENCE
!CCCC LIMITS TO OUTPUT
!
      AJUNK=1.0 - ALPHA
      AJUNK2=1.0 - (AJUNK/(2.0*REAL(NUMPAR)))
      NP=N-NUMPAR
      TBONF=0.0
      IF(NP.GE.1.AND.(AJUNK2.GE.0.0.AND.AJUNK2.LE.1.0))   &
      CALL TPPF(AJUNK2,REAL(NP),TBONF)
!
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
!
      IF(IFITAU.EQ.'ON')THEN
        WRITE(IOUNI1,8613)
 8613   FORMAT(1X,   &
               'COEFFICIENT     ',   &
               'COEF SD         ',   &
               'T-VALUE         ',   &
               'BONF LOWER CONF ',   &
               'BONF UPPER CONF ')
        DO 8610 I=1,NUMPAR
          TVALUE=(-999.9)
          IF(C(I).GT.0.0)TVALUE=PARAM3(I)/C(I)
          TBONL=PARAM3(I) - TBONF*C(I)
          TBONU=PARAM3(I) + TBONF*C(I)
          WRITE(IOUNI1,IFORMT)PARAM3(I),C(I),TVALUE,TBONL,TBONU,   &
                              IPARN3(I),IPARN4(I)
 8610   CONTINUE
!8611   FORMAT(5E15.7,2X,A4,A4)
!
!CCCC   THE FOLLOWING 2 LINES WERE ADDED     SEPTEMBER 1995
!CCCC   APRIL 1996.  SUPPRESS PRINTING IF IPRINT OFF
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8612)
 8612     FORMAT('DPST1F.DAT: COEF, SD(COEF), T-VALUE, LOWER ,',   &
                 'BONFERRONI UPPER BONFERRONI')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
 8619 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ACTIVATED     JANUARY 1994
!CCCC JUNE 2002: ADD SUPPORT FOR JOINT BONFERRONI AND JOINT
!CCCC HOTELLING CONFIDENCE INTERVALS.
      T975=0.0
      T995=0.0
      IF(IRESDF.GE.1)CALL TPPF(.975,REAL(IRESDF),T975)
      IF(IRESDF.GE.1)CALL TPPF(.995,REAL(IRESDF),T995)
!
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
      IF(NP.GE.1.AND.(AJUNK2.GE.0.0.AND.AJUNK2.LE.1.0))   &
      CALL TPPF(AJUNK2,REAL(NP),TBONF)
      IF(NP.GE.1.AND.NUMPAR.GE.1.AND.(ALPHA.GE.0.0.AND.ALPHA.LE.1.0))   &
      CALL FPPF(ALPHA,NUMPAR,NP,THOT)
      THOT=REAL(NUMPAR)*THOT
      IF(THOT.GT.0.0)THOT=SQRT(THOT)
!
      IF(IFITAU.EQ.'OFF')GO TO 8629
!
      WRITE(IOUNI2,8623)
 8623 FORMAT(1X,   &
             'SD PRED VALUES  ',   &
             '95% LOW PRED CL ',   &
             '95% UPP PRED CL ',   &
             '99% LOW PRED CL ',   &
             '99% UPP PRED CL ',   &
             'BONF LOW PRED CL',   &
             'BONF UPP PRED CL',   &
             'HOTE LOW PRED CL',   &
             'HOTE UPP PRED CL')
      DO 8620 I=1,N
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
!
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
!
        WRITE(IOUNI2,IFORMT)SDPR,ALOW2,AUPP2,ALOW3,AUPP3,ALOW4,AUPP4,   &
                            ALOW5,AUPP5
!8621   FORMAT(9E15.7)
 8620 CONTINUE
!CCCC APRIL 1996.  SUPPRESS PRINTING IF IPRINT OFF
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8622)
 8622   FORMAT('DPST2F.DAT: SD(PRED),95LOWER,95UPPER,99LOWER,99UPPER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8624)
 8624   FORMAT('            LOWER BONFERRONI,UPPER BONFERRONI,',   &
               'LOWER HOTELLING,UPPER HOTELLING')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
 8629 CONTINUE
!
!CCC  JULY 1993.  UNCOMMENT FOLLOWING BLOCK. COPUTE AND PRINT:
!CCCC 1) DIAGONALS OF HAT MATRIX (HII = VAR(PRED VALUE)/RESIDUAL VAR)
!CCCC 2) VARIANCE OF RESIDUALS   (VAR(RES) = MSE*(1-HII))
!CCCC 3) STANDARDIZED RESIDUALS  (STRES = RES/SQRT(MSE))
!CCCC 4) INTERNALLY STUDENTIZED RESIDUALS  ( = RES/SD(RES))
!CCCC 5) DELETED RESIDUALS       ( = RES/(1-HII))
!CCCC 6) EXTERNALLY STUDENTIZED RESIDUALS (=RES*SQRT((N-P-1)/(SSE*
!CCCC                                       (1-HII)-RES**2))
!CCCC 7) COOK'S DISTANCE         (COOK=(RES**2/(P*MSE))*HII/(1-HII)**2
!CCCC 8) DFFITS                  (DFFITS=EXTSRES*SQRT(HII(1-HII))
!CCCC                              WHERE EXTSRES=EXTERNAL STUDENT RES
!CCCC IF HAVE PERFECT FIT, RESSD IS ZERO.  DON'T PRINT DIAGNOSTIC
!CCCC STATISTICS IN THIS CASE.
!
      IF(IFITAU.EQ.'OFF')GO TO 8649
!
      IF(RESSD.EQ.0.0)THEN
        WRITE(IOUNI3,8631)
 8631   FORMAT(1X,'PERFECT FIT, NO DIAGNOSTICS GENERATED.')
        GO TO 8659
      ENDIF
!
      AJUNK=RESSD**2
      DSUM1=0.0D0
      DO 8635 I=1,N
        AJUNK2=VSDPRE(I)**2
        CALL SPDIV(AJUNK2,AJUNK,IND,Z(I))
        IF(W(I).EQ.0.0)Z(I)=0.0
 8635 CONTINUE
      WRITE(IOUNI3,8639)
 8639 FORMAT(1X,   &
      'DIAGONAL OF HAT ',   &
      'RESIDUAL VAR    ',   &
      'STANDARD RES    ',   &
      'INT. STUD. RES  ',   &
      'DELETED RES     ',   &
      'EXT. STUD. RES  ',   &
      'COOKS DISTANCE  ',   &
      'DFFITS          ')
      DO 8640 I=1,N
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
!CCCC SEPTEMBER 1993.  FIX TYPO IN FOLLOWING LINE
!CCCC IF(RESS*(1.0-Z(I))-RES2(I)**2.NE.0.0)THEN
      IF(RESSS*(1.0-Z(I))-RES2(I)**2.NE.0.0)THEN
        AJUNK2=ACONST/(RESSS*(1.0-Z(I))-RES2(I)**2)
      ELSE
        AJUNK2=0.0
      ENDIF
      AJUNK7=0.0
      IF(AJUNK2.GE.0.0)AJUNK7=RES2(I)*SQRT(AJUNK2)
!CCCC THE FOLLOWING LINE WAS FIXED        JANUARY 1996
!CCCC TO FIX BOMB WITH   CONSTANT FIT     JANUARY 1996
!CCCC AJUNK=RES2(I)**2/(REAL(M)*RESMS)
!CCCC USE NUMPAR INSTEAD OF M.
      AJUNK=0.0
!CCCC IF(M.GT.0)AJUNK=RES2(I)**2/(REAL(M)*RESMS)
      IF(NUMPAR.GT.0)AJUNK=RES2(I)**2/(REAL(NUMPAR)*RESMS)
      AJUNK2=0.0
      IF(Z(I)-1.0.NE.0.0)AJUNK2=Z(I)/((1.0-Z(I))**2)
      AJUNK8=AJUNK*AJUNK2
      AJUNK2=0.0
      IF(Z(I)-1.0.NE.0.0)AJUNK2=SQRT(Z(I)/(1.0-Z(I)))
      AJUNK9=AJUNK7*AJUNK2
!
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
!
      WRITE(IOUNI3,IFORMT)Z(I),AJUNK3,AJUNK4,AJUNK5,AJUNK6,   &
      AJUNK7,AJUNK8,AJUNK9
!8641 FORMAT(8(E15.7,1X))
 8640 CONTINUE
!
      APRESS=REAL(DSUM1)
!
!CCCC APRIL 1996.  SUPPRESS PRINTING IF IPRINT OFF
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8652)
 8652   FORMAT('DPST3F.DAT: REGRESSION DIAGNOSTICS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
 8649 CONTINUE
!
!CCCC JULY 1993.  WRITE OUT VARIANCE-COVARIANCE PARAMETER OF
!CCCC PARAMETERS.  NOTE THAT IT IS STORED IN SCRATCH SCR, STARTING
!CCCC AT ELEMENT 1 AND (M+1)*(M+2)/2 ELEMENTS LONG
!CCCC ACTUALLY, THIS IS THE (X-TRANSPOSE X) INVERSE MATRIX, MULTIPLY
!CCCC BY MSE TO GET VARIANCE-COVARIANCE MATRIX.
!CCCC JUNE 1994.  BUG: FOR POLYNOMIAL, M=NUMPAR-1, SO ADD 1 BACK IN
!
      IF(IFITAU.EQ.'OFF')GO TO 8689
!
 8659 CONTINUE
      NTEMP=M
      IF(ICASFI.NE.'MFIT')NTEMP=M+1
      ICOUNT=0
      DO 8660 I=1,NTEMP
        DO 8662 J=I,NTEMP
          ICOUNT=ICOUNT+1
          PARCOV(I,J)=SCR(ICOUNT)
          PARCOV(J,I)=PARCOV(I,J)
 8662   CONTINUE
 8660 CONTINUE
!
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
!
      WRITE(IOUNI4,8673)
 8673 FORMAT(1X,   &
             'PARAMETER COV   ',   &
             'INVERSE X-TRANSPOSE*X')
      DO 8670 J=1,NTEMP
        DO 8672 I=1,NTEMP
          AJUNK=RESMS*PARCOV(I,J)
          WRITE(IOUNI4,IFORMT)AJUNK,PARCOV(I,J)
!8679     FORMAT(E15.7,1X,E15.7)
 8672   CONTINUE
        WRITE(IOUNI4,8678)
 8678   FORMAT(1X)
 8670 CONTINUE
!
!CCCC APRIL 1996.  SUPPRESS PRINTING IF IPRINT OFF
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8682)
 8682   FORMAT('DPST4F.DAT: PARAMETER VARIANCE-COVARIANCE MATRIX AND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8683)
 8683   FORMAT('            INVERSE OF X-TRANSPOSE X MATRIX')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
 8689 CONTINUE
!
!CCCC WRITE REGRESSION ANOVA TABLE TO DPST5F.DAT
!
      RESSD=SD
      RESDF=NDF
      RESMS=RESSD*RESSD
      RESSS=RESMS*RESDF
!
      IREGDF=NUMPAR-1
      AMSR=SSR/REAL(IREGDF)
!
      ITOTDF=INT(RESDF) + IREGDF
      SSTO=SSR + RESSS
!
      RSQUAR=1.0 - RESSS/SSTO
      ADJRSQ=1.0 - (REAL(N-1)/REAL(N-NUMPAR))*RESSS/SSTO
!
      FSTAT=100000.0
      IF(RESMS.GT.0.0)FSTAT=AMSR/RESMS
      NP=N-NUMPAR
      CALL FCDF(FSTAT,IREGDF,NP,CDF)
      CALL FPPF(0.95,IREGDF,NP,FCV95)
      CALL FPPF(0.99,IREGDF,NP,FCV99)
!
      IF(IFITAU.EQ.'OFF')GO TO 8729
!
      WRITE(IOUNI5,8710)
 8710 FORMAT('------------------------------------------------------',   &
             '-----------------------')
      WRITE(IOUNI5,8712)
 8712 FORMAT('SOURCE               DF    SUM OF SQUARES    ',   &
             ' MEAN SQUARE              F')
      WRITE(IOUNI5,8710)
!
      WRITE(IOUNI5,8714)IREGDF,SSR,AMSR,FSTAT
 8714 FORMAT('REGRESSION     ',I8,3X,E15.7,3X,E15.7,3X,E15.7)
      WRITE(IOUNI5,8716)INT(RESDF),RESSS,RESMS
 8716 FORMAT('RESIDUAL       ',I8,3X,E15.7,3X,E15.7)
      WRITE(IOUNI5,8718)ITOTDF,SSTO
 8718 FORMAT('TOTAL          ',I8,3X,E15.7)
!
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
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,8782)
 8782   FORMAT('DPST5F.DAT: REGRESSION ANOVA TABLE')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
 8729 CONTINUE
!
!CCCCC THE FOLLOWING SECTION WAS ADDED MARCH 1992
!               **************************************
!               **  STEP 88--                       **
!               **  CLOSE       THE STORAGE FILES.  **
!               **************************************
!
      ISTEPN='87'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFITAU.EQ.'ON')THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!CCCC IF(IBUGA3.EQ.'OFF')GO TO 9090   MAY 1989
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
        DO 9015 I=1,NUMPAR
          WRITE(ICOUT,9016)I,IPARN3(I),IPARN4(I),PARAM3(I)
 9016     FORMAT('I,IPARN3(I),IPARN4(I),PARAM3(I) = ',I8,2X,A4,A4,G15.7)
          CALL DPWRST('XXX','WRIT')
 9015   CONTINUE
        DO 9020 I=1,N
          WRITE(ICOUT,9021)I,Y(I),W(I),PRED2(I),RES2(I)
 9021     FORMAT('I,Y(I),W(I),PRED2(I),RES2(I) = ',   &
                 I8,4G15.7)
          CALL DPWRST('XXX','WRIT')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPFIT3
!CCCC-----LSQRT--------------------------------------
      SUBROUTINE LSQRTX (Y,W,N,X,NR,M,IT,   &
                         B,Z,T,V,S,E,D,SD,NDF,SCR,ID,IFITAC,   &
                        IBUGA3,ISUBRO,IERROR)
!CCCC THE ABOVE LINE WAS AUGMENTED     SEPTEMBER 1993
!
!     PURPOSE--PERFORM LEAST SQUARES FIT
!              OF MULTILINEAR MODEL OR POLYNOMIAL MODEL
!              USING A MODIFIED GRAM-SCHMIDT ALGORITHM
!              WITH ITERATIVE REFINEMENT OF THE SOLUTION.
!
!     INPUT ARGUMENTS--
!           Y     VECTOR OF OBSERVATIONS (N BY 1).
!           W     VECTOR OF WEIGHTS (N BY 1).
!           N     NUMBER OF OBSERVATIONS.
!           X     MATRIX OF INDEPENDENT VARIABLES WHICH ARE TO BE FITTED.
!           NR    MAXIMUM NUMBER OF ROWS IN X.
!           M     NUMBER OF UNKNOWN COEFFICIENTS OR DEGREE OF POLYNOMIAL
!                    (M LESS THAN OR EQUAL TO N).
!           IT    PARAMETER WHICH SPECIFIES WHETHER OR NOT A POLYNOMIAL TYPE
!                    FIT IS TO BE PERFORMED.
!                      IT = 1 INDICATES POLYNOMIAL FIT.
!                      IT = 2 INDICATES MULTILINEAR FIT.
!
!
!                 IF IT = 1, THE FUNCTION TO BE FITTED IS A POLYNOMIAL
!                    HAVING THE FORM
!
!                    Y(I) = B(1) + B(2)*Z(I) + B(3)*Z(I)**2 + ...
!                                + B(M)*Z(I)**(M-1) + ERROR, I=1,2,...,N.
!
!                 IF IT = 2, THE FUNCTION TO BE FITTED HAS THE FORM
!
!                    Y(I) = B(1)*X1(I) + B(2)*X2(I) + ... + B(M)*XM(I) +
!                                                     ERROR, I=1,2,...,N.
!     OUTPUT ARGUMENTS--
!           B     VECTOR OF COEFFICIENTS (M+1 BY 1).
!           Z     VECTOR OF RESIDUALS (N BY 1).
!           T     VECTOR OF STANDARD DEVIATIONS OF COEFFICIENTS (M+1 BY 1).
!           V     VECTOR OF STANDARD DEVIATIONS OF PREDICTED VALUES
!                    (N BY 1).
!           S     VECTOR OF SQUARED FOURIER COEFFICIENTS (M+3 BY 1).  THE
!                    FIRST M ELEMENTS OF THIS ARRAY ARE SUMS OF SQUARES
!                    WHICH CAN BE USED IN AN ANALYSIS OF VARIANCE.  THE
!                    LAST TWO ELEMENTS OF S ARE NOT COMPUTED IN THIS SUB-
!                    ROUTINE BUT ARE RESERVED FOR QUANTITIES TO BE COMPUTED
!                    IN THE CALLING PROGRAM.
!           E     RESIDUAL SUM OF SQUARES.
!           D     AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN INITIAL
!                    SOLUTION AND THE FIRST ITERATION (IN SUBROUTINE SLVE).
!           SD    RESIDUAL STANDARD DEVIATION.
!           NDF   NO. OF DEGREES OF FREEDOM.
!           SCR   A SCRATCH VECTOR USED FOR INTERNAL CALCULATIONS
!           ID    ID = 0  EVERYTHING IS OK.
!                 ID = 1  AUGMENTED MATRIX IS SINGULAR.
!                 ID = 2  ITERATION PROCEDURE FAILED TO CONVERGE.
!
!     NOTE--THE INPUT ARRAYS X, Y AND W ARE LEFT UNCHANGED
!           BY THIS SUBROUTINE.
!     NOTE--THE SCR VECTOR MUST HAVE SIZE EQUAL TO OR GREATER THAN
!           ((M + 1) (M + 2) / 2) + N*M + 2*N + 2*M +1
!     PRIMARY CALLING SEQUENCE--
!           LSQRT
!                 LSQ
!                       SCALE
!                       PDECOM
!                       SLVE
!                       DSUMAL
!                       SDPRED
!                       PINVRT
!     ADDITIONAL SUBROUTINES THAT HAVE BEEN CONVERTED FROM FUNCTIONS--
!           DPDIV
!           SPDIV
!           DPCON
!           DPSQRT
!           SPSQRT
!           SPLO10
!           IDIV
!
!     SUBROUTINE LSQ COMPUTES SOLUTIONS TO LINEAR LEAST SQUARES
!        PROBLEMS USING A MODIFIED GRAM-SCHMIDT ALGORITHM WITH
!        ITERATIVE REFINEMENT OF THE SOLUTION.
!
!     SUBROUTINES PDECOM, SLVE AND PINVRT ARE BASED ON ...
!        (1) ITERATIVE REFINEMENT OF LINEAR LEAST SQUARES SOLUTIONS II,
!            BY AKE BJORCK, BIT, VOL. 8 (1968), PP. 8-30.
!        (2) SOLUTIONS TO WEIGHTED LEAST SQUARES PROBLEMS BY MODIFIED
!            GRAM-SCHMIDT WITH ITERATIVE REFINEMENT, BY ROY H. WAMPLER,
!            ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE, VOL. 5 (1979),
!            TO APPEAR.
!
!     PRECISION--
!        SINGLE PRECISION ARITHMETIC IS USED FOR ALL CALCULATIONS EXCEPT
!        THE DOUBLE PRECISION ACCUMULATION OF INNER PRODUCTS.  (THE
!        VARIABLE SUM (OR DSUM) IS DECLARED TO BE DOUBLE PRECISION IN
!        SUBROUTINE LSQ, SCALE, PDECOM, SLVE, SDPRED AND PINVRT.)  IT
!        IS ESSENTIAL FOR THE SUCCESS OF THE ITERATIVE REFINEMENT
!        PROCEDURE IN SUBROUTINE SLVE THAT INNER PRODUCTS BE ACCUMULATED
!        IN DOUBLE PRECISION.
!
! *   CONVERSION OF THE PROGRAM TO STRICTLY DOUBLE PRECISION, AND      *
! *   CONVERSION OF THE PROGRAM TO STRICTLY SINGLE PRECISION.          *
! *      ON COMPUTERS HAVING SHORT WORD LENGTH (AS THE IBM 360/370)    *
! *      IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN DOUBLE     *
! *      PRECISION.  ON COMPUTERS HAVING LONG WORD LENGTH (AS THE CDC  *
! *      6600) IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN      *
! *      SINGLE PRECISION.  IN SUCH CASES, THE ITERATIVE REFINEMENT    *
! *      PRESENTLY INCLUDED IN SUBROUTINE SLVE SHOULD BE OMITTED.      *
! *      ADDITIONAL REMARKS ON HOW TO OMIT THE ITERATIVE REFINEMENT    *
! *      ARE GIVEN IN SUBROUTINE SLVE.                                 *
! *      IF ALL COMPUTING IS DONE IN DOUBLE PRECISION, THE VALUE OF    *
! *      ETA, A MACHINE DEPENDENT PARAMETER, SHOULD BE CHANGED SO THAT *
! *      ETA IS THE SMALLEST DOUBLE PRECISION NUMBER SUCH THAT         *
! *      1.0 + ETA IS GREATER THAN 1.0 IN DOUBLE PRECISION ARITHMETIC. *
!
!     TEST PROBLEM--
!           SAMPLE INPUT FOR A MULTILINEAR FIT
!           (4 INDEPENDENT VARIABLES EQUIVALENT TO A CUBIC FIT
!           AND UNIT WEIGHTING)--
!           FIRST LINE GIVES SAMPLE SIZE, DEGREE, POLYNOMIAL TYPE
!
!            7 4 2
!            10. 1. 3.4 11.56 39.304 1.
!            20. 1. 11.7 136.89 1601.613 1.
!            30. 1. 37.2 1383.84 51478.848 1.
!            40. 1. 80.1 6416.01 513922.401 1.
!            50. 1. 151.4 22921.96 3470384.744 1.
!            60. 1. 253.2 64110.24 16232712.768 1.
!            70. 1. 392.6 154134.76 60513306.776 1.
!
!           SAMPLE INPUT FOR A CUBIC POLYNOMIAL FIT
!           (SAME EXAMPLE AS ABOVE)--
!           FIRST LINE GIVES SAMPLE SIZE, NUMBER OF VAR., MULTILINEAR TYPE
!
!            7 3 1
!            10.   3.4 1.
!            20.  11.7 1.
!            30.  37.2 1.
!            40.  80.1 1.
!            50. 151.4 1.
!            60. 253.2 1.
!            70. 392.6 1.
!
!     OUTPUT (FROM EITHER OF THE ABOVE 2 TEST PROBLEMS)--
!
!       COEFFICIENTS
!          .12212494E+02    .46908681E+00   -.16867931E-02    .22115341E-05
!       RESIDUALS
!         -.37879763E+01    .25265538E+01    .25578816E+01   -.10042261E+00
!         -.22425069E+01    .12562386E+01   -.20976813E+00
!       S D OF COEFFICIENTS
!          .26445864E+01    .86317750E-01    .57921800E-03    .98128429E-06
!       S D OF PREDICATED VALUES
!          .24379267E+01    .20369802E+01    .17428904E+01    .23363574E+01
!          .23017371E+01    .31747709E+01    .33588546E+01
!       SQUARED FOURIER COEFFICIENTS
!          .11200000E+05    .24784422E+04    .23016542E+03    .57456310E+02
!       RESIDUAL SUM OF SQUARES =    .33936057E+02
!       AVERAGE NO. DIGITS IN AGREEMENT =    .78267799E+01
!       RESIDUAL STANDARD DEVIATION =    .33633345E+01
!       DEGREES OF FREEDOM =   3
!
!     NOTE--IN THE ABOVE TEST PROBLEMS, N = 7 AND M = 4
!           AND THUS THE DIMENSION OF SCR MUST BE AT LEAST
!           ((M + 1) (M + 2) / 2) + N*M + 2*N + 2*M +1 =
!           ((4 + 1) (4 + 2) / 2) + 7*4 + 2*7 + 2*4 +1 = 66
!
!     NOTE--MAXOBV = MAXIMUM NUMBER OF OBSERVATIONS PER VARIABLE
!                    (= 2048 (JULY 1987))
!           MAXCMF = MAXIMUM NUMBER OF COEFFICIENTS THAT MAY
!                    BE ESTIMATED IN A MULTILINEAR FIT
!                    (= 30 (JULY 1987))
!     WRITTEN BY--ROY H. WAMPLER
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 A337 ADMINISTRATION BUILDING
!                 NATIONAL BUREAU OF STANDARDS
!                 GAITHERSBURG, MD. 20899
!                 301-975-2844
!
!     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--JUNE      1987.
!     UPDATED         --MARCH     1988.  CHECK THAT SCRATCH AREA NOT EXCEEDED
!     UPDATED         --NOVEMBER  1989.  DIMENSION SCR(1) TO SCR(*)
!     UPDATED         --SEPTEMBER 1993.  ADD ISUBRO TO INPUT ARGS
!     UPDATED         --JULY      1995.  ADJUST DEBUG FORMATS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IFITAC
      CHARACTER*4 IBUGA3
!CCCC THE FOLLOWING LINE WAS ADDED    SEPTEMBER 1993
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!-----DIMENSION-------------------------------------------------------
                                                                                                                                  
      INCLUDE 'DPCOPA.INC'
!
!CCCC DIMENSION X(NR,M),Y(N),W(N),B(M),Z(N),T(M+1),V(N),S(M+2),SCR(1)
!CCCC DIMENSION X(NR,M)
!CCCC DIMENSION X(MAXOBV,MAXCMF)
      DIMENSION X(NR,*)
      DIMENSION Y(N)
      DIMENSION W(N)
      DIMENSION B(M)
      DIMENSION Z(N)
      DIMENSION T(M+1)
      DIMENSION V(N)
      DIMENSION S(M+2)
!CCCC THE FOLLOWING LINE WAS CORRECTED NOVEMBER 1989
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC DIMENSION SCR(1)
      DIMENSION SCR(*)
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
!CCCC THE FOLLOWING LINE WAS CHANGED      SEPTEBMER 1993
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
        DO 56 J=1,M
          DO 57 I=1,N
            WRITE(ICOUT,58)I,J,Y(I),X(I,J),W(I)
   58       FORMAT('I,J,Y(I),X(I,J),W(I) = ',2I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
   57     CONTINUE
   56   CONTINUE
      ENDIF
!
!CCCC THE FOLLOWING SECTION OF CODE WAS INSERTED MARCH 1988.
!     CHECK THAT THE SCRATCH AREA WILL NOT OVERFLOW
!
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
        GO TO 9000
      ENDIF
!
!     DEFINE STARTING POINT FOR THE R MATRIX
!
      ISUBR = 1
      MZ = M
      IF (IT.EQ.1 .AND. IFITAC.EQ.'ON') MZ = MZ+1
      MIN2 = (MZ+1) * (MZ+2) / 2
!
!     DEFINE STARTING POINT FOR THE Q VECTOR
!
      ISUBQ = ISUBR + MIN2
      MM1 = N * (MZ+1)
!
!     DEFINE STARTING POINT FOR THE F VECTOR
!
      ISUBF = ISUBQ + MM1
!
!     DEFINE STARTING POINT FOR THE P VECTOR
!
      ISUBP = ISUBF + MZ + 1
!
!     DEFINE STARTING POINT FOR THE A VECTOR
!
      ISUBA = ISUBP + N
      C = 0.0
      H = 0.0
!
!CCCC THE FOLLOWING ARGUMENT LIST WAS AUGMENTED     SEPTEMBER 1995
      CALL LSQ (N,MZ,NR,X,Y,W,H,C,IT,B,Z,SCR(ISUBR),T,V,S,E,SCR(ISUBQ),   &
                SCR(ISUBF),SCR(ISUBP),SCR(ISUBA),ID,D,IFITAC,   &
                IBUGA3,ISUBRO,IERROR)
!
      NDF = 0
      DO 1100 I = 1,N
         IF (W(I) .GT. 0.0) NDF = NDF + 1
 1100 CONTINUE
      NDF = NDF-MZ
!CCCC SD = SPDIV(E,FLOAT(NDF),IND)
      CALL SPDIV(E,FLOAT(NDF),IND,RESULT)
      SD = RESULT
!CCCC SD = SPSQRT(SD)
      CALL SPSQRT(SD,RESULT)
      SD=RESULT
!
 9000 CONTINUE
!CCCC THE FOLLOWING SECTION WAS ADDED      SEPTEBMER 1993
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SQRT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF LSQRT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)SD,RESULT,M,NDF
 9015   FORMAT('SD,RESULT,M,NDF = ',2G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9016 I=1,M
          WRITE(ICOUT,9017)I,B(I),T(I)
 9017     FORMAT('I,B(I),T(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
      RETURN
      END SUBROUTINE LSQRTX 
!CCCC-----LSQ--------------------------------------
      SUBROUTINE LSQ (N,M,NR,X,Y,W,H,C,IT,B,Z,R,T,V,S,E,Q,F,P,A,ID,D,   &
                      IFITAC,IBUGA3,ISUBRO,IERROR)
!CCCC SUBROUTINE LSQ (N,M,NR,X,Y,W,H,C,IT,B,Z,R,T,V,S,E,Q,F,P,A,ID,D)
!CCCC THE ABOVE ARGUMENT LIST WAS AUGMENTED    SEPTEMBER 1995
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!               WRITTEN BY -
!                      ROY H. WAMPLER,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL BUREAU OF STANDARDS,
!                      GAITHERSBURG,MD. 20899
!                          TELEPHONE 301-975-2844
!
!     UPDATED--NOVEMBER  1989--DIMENSION (1) TO (*) (AND MOVED)
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --SEPTEMBER 1995. ADD BUGS TO ARGUMENT LIST
!
!     ==================================================================
!
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!CCCC THE FOLLOWING 3 LINES WERE ADDED    SEPTEMBER 1995
      CHARACTER*4 IFITAC
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!CCCC THE FOLLOWING 6 LINES WERE MOVED        NOVEMBER 1989
!CCCC AND CHANGED DIMENSION (1) TO (*)
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC REAL             A(1), B(1), F(1), P(1), Q(1), R(1), S(1)
!CCCC REALCCCCC        T(1), V(1), W(1), X(NR,M), Y(1), Z(1)
!CCCC REAL             T(1), V(1), W(1), X, Y(1), Z(1)
!CCCC REAL             C, D, E, H
!CCCC REAL             ETA, RESDF, RMS, RSS, SD, TOL, U, WC, WW, YINC
!CCCC REALCCCCC        SPDIV, DPCON, SPSQRT
!
!CCCC THE FOLLOWING LINE WAS CORRECTED      NOVEMBER 1989
!CCCC SPLIT INTO 2 LINES
!CCCC AND CHANGED DIMENSION (1) TO (MAXOBV) (SEE BELOW)
!CCCC (BUG UNCOVERED BY NELSON HSU)
      DOUBLE PRECISION DX(1)
!
      DOUBLE PRECISION SUM
!CCCC THE FOLLOWING 2 LINES WERE ADDED    SEPTEMBER 1995
      DOUBLE PRECISION SNEG
      DOUBLE PRECISION SPOS
!
      REAL             A(*), B(*), F(*), P(*), Q(*), R(*), S(*)
!CCCC REAL             T(*), V(*), W(*), X(NR,M), Y(*), Z(*)
      REAL             T(*), V(*), W(*), X, Y(*), Z(*)
      REAL             C, D, E, H
      REAL             ETA, RESDF, RMS, RSS, SD, TOL, U, WC, WW, YINC
!CCCC REAL             SPDIV, DPCON, SPSQRT
!
      INCLUDE 'DPCOPA.INC'
!CCCC DIMENSION X(MAXOBV,MAXCMF)
      DIMENSION X(NR,*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-----------------------------------------------------
!
      DATA RMXINT / 134217727. /
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,2001)
 2001   FORMAT('AT START OF LSQ ROUTINE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2003)IFITAC
 2003   FORMAT('IFITAC = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO 2000 I=1,N
          WRITE(ICOUT,2011)I,J,(X(I,J),J=1,MAX(M,5))
 2011     FORMAT('I,J,(X(I,J),J=1,MAX(M,5)) = ',2I5,5G15.7)
          CALL DPWRST('XXX','BUG ')
 2000   CONTINUE
      ENDIF
!
      IERROR='NO'
      ID = 0
      NN  = N
      MM  = M
      WC = H
      U   = 0.0
      WW = 0.0
!
!     SET VALUE OF ETA, A MACHINE-DEPENDENT PARAMETER.
!        ETA IS THE SMALLEST POSITIVE REAL NUMBER FOR WHICH 1.0 + ETA IS
!        GREATER THAN 1.0 IN FLOATING-POINT ARITHMETIC.
!        THE VALUE ETA = 2.**(-26) IS APPROPRIATE FOR THE UNIVAC 1108.
!
!CCCC ETA = SPDIV (RMXINT,2.0,IRR) + 1.0
      CALL  SPDIV (RMXINT,2.0,IRR,RESULT)
      ETA = RESULT + 1.0
!CCCC ETA = SPDIV (1.0,ETA,IND)
      CALL  SPDIV (1.0,ETA,IND,ETA)
!
!     SET VALUE OF TOL, A TOLERANCE USED IN DETERMINING THE RANK OF THE
!        SYSTEM OF EQUATIONS.
!
!     EMPIRICAL EVIDENCE SUGGESTS THAT TOL SHOULD BE CHOSEN NO SMALLER
!        THAN N*ETA.
!
      TOL = FLOAT (NN) * ETA
!
!     SET SCALE PARAMETER, ISCALE, EQUAL TO ZERO.
!        ISCALE = 0 INDICATES THAT A SOLUTION IS SOUGHT WITHOUT SCALING
!        THE INPUT DATA.
!
!     IN THE EVENT THAT THE ALGORITHM FAILS TO OBTAIN A SOLUTION WITH
!        UNSCALED DATA, ISCALE IS THEN SET EQUAL TO 1 AND ANOTHER
!        ATTEMPT IS C        ATTEMPT IS MADE TO OBTAIN A SOLUTION WITH THE DATA
!
      ISCALE = 0
      MP1 = MM + 1
!
!     SET UP MATRIX Q, INPUT FOR SUBROUTINES SCALE AND PDECOM.
!
  10  IF (IT.EQ.2) GO TO 50
!
!     CALL SUBROUTINE SCALE TO COMPUTE MEAN OF X-VECTOR (DENOTED BY U)
!        FOR POLYNOMIAL TYPE PROBLEMS, IF DATA ARE TO BE SCALED.
!
      IF (ISCALE.EQ.1) THEN
        CALL SCALDP (ISCALE,2,NN,MM,IT,NR,W,WC,X,U,Q,S,B,A,Z,R,F,IFAULT)
        IF (IFAULT.EQ.1) ID = 1
!
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,2101)
 2101     FORMAT('AFTER FIRST CALL TO SCALE')
          CALL DPWRST('XXX','BUG ')
          DO 2100 I=1,N
            WRITE(ICOUT,2111)(X(I,J),J=1,MAX(M,5))
 2111       FORMAT('I,J,(X(I,J),J=1,MAX(M,5)) = ',2I5,5G15.7)
            CALL DPWRST('XXX','BUG ')
 2100     CONTINUE
        ENDIF
!
      ENDIF
!
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
!
      GO TO 80
!
  50  IF(ISCALE.EQ.1) GO TO 80
      DO 70 I=1,NN
        K = MM * NN + I
        Q(K) = Y(I)
        DO 60 J=1,MM
          K = (J-1) * NN + I
          Q(K) = X(I,J)
  60    CONTINUE
  70  CONTINUE
!
!     CALL SUBROUTINE SCALE TO COMPUTE VECTOR NORMS AND TO SET VALUES OF
!        SCALE FACTORS (F).
!
  80  CONTINUE
      CALL SCALDP (ISCALE,1,NN,MM,IT,NR,W,WC,X,U,Q,S,B,A,Z,R,F,   &
                  IFAULT)
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,2201)
 2201   FORMAT('AT START OF LSQ ROUTINE')
        CALL DPWRST('XXX','BUG ')
        DO 2200 I=1,N
          WRITE(ICOUT,2211)I,J,(X(I,J),J=1,MAX(M,5))
 2211     FORMAT('I,J,(X(I,J),J=1,MAX(M,5)) = ',2I5,5G15.7)
          CALL DPWRST('XXX','BUG ')
 2200   CONTINUE
      ENDIF
!
!     IFAULT IS SET EQUAL TO ONE IN SUBROUTINE SCALE WHEN A COLUMN OF
!        MATRIX X IS FOUND TO EQUAL ZERO.
!
      IF (IFAULT.EQ.1) GO TO 240
!
!     CALL SUBROUTINE PDECOM TO OBTAIN AN ORTHOGONAL QR-DECOMPOSITION OF
!        THE MATRIX CONTAINED IN Q ON ENTRY TO PDECOM.  ON RETURN FROM
!        PDECOM, M1 IS THE COMPUTED RANK OF THE SYSTEM OF EQUATIONS.
!        IF MATRIX Q IS FOUND TO BE SINGULAR, IS = 0 ON RETURN FROM
!        PDECOM.  OTHERWISE, IS = 1.
!
      CALL PDECOM (NN,MP1,TOL,W,WC,IS,M1,Q,T,R)
!CCCC APRIL 2002: PRINT WARNING MESSAGE FOR POTENTIAL SINGULARITY
!
      IF(IS.EQ.1)THEN
        WRITE(ICOUT,99)
   99   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1001)
 1001   FORMAT('***** WARNING: POTENTIAL SINGULARITY FROM (LINEAR) ',   &
               'FIT DETECTED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1003)
 1003   FORMAT('      POTENTIAL CAUSES OF SINGULARITY INCLUDE:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1005)
 1005   FORMAT('      1. A COLUMN IN THE X MATRIX CONTAINS ALL THE ',   &
               'SAME VALUES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1007)
 1007   FORMAT('      2. TWO COLUMNS IN THE X MATRIX ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1009)
 1009   FORMAT('      3. A MORE COMPLICATED LINEAR DEPENDENCY EXISTS ',   &
               'BETWEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1010)
 1010   FORMAT('         BETWEEN THE COLUMNS IN THE X MATRIX.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1011)
 1011   FORMAT('      FOR MULTI-LINEAR FITS, DATAPLOT CHECKS FOR THE ',   &
               'FIRST TWO CAUSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1012)
 1012   FORMAT('      FOR SINGULARITY.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1013)
 1013   FORMAT('      RECOMMENDED FIX: PERFORM THE FIT AFTER REMOVING ',   &
               'ONE OR MORE OF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1014)
 1014   FORMAT('      ONE OR MORE OF THE INDEPENDENT VARIABLES.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF (IS.EQ.0) GO TO 100
      IF (M1.GT.0) GO TO 90
      GO TO 240
!
!     ..................................................................
!
  90  IF (M1.EQ.MM) GO TO 100
      IF (ISCALE.EQ.1) GO TO 240
      ISCALE = 1
      GO TO 10
 100  IR = ISCALE
!
!     TRANSFER T(J) TO ARRAY R SO THAT T IS AVAILABLE FOR WORK AREA.
!
      DO 110 I=1,MP1
!CCCC   LD = IDIV (2*(I-1)*MP1-I*(I-3),2,IRR)
        CALL IDIV (2*(I-1)*MP1-I*(I-3),2,IRR,LD)
        R(LD) = T(I)
 110  CONTINUE
!
!     CALL SUBROUTINE SLVE TO OBTAIN THE SOLUTION (COEFFICIENTS AND
!        RESIDUALS) OF THE LEAST SQUARES PROBLEM.  ITERATIVE REFINEMENT
!        IS USED TO IMPROVE (IF POSSIBLE) THE ACCURACY OF THE
!        INITIAL SOLUTION.  ON RETURN FROM SLVE, PARAMETER IR = 0 IF THE
!        ITERATIVE REFINEMENT PROCEDURE CONVERGED TO A SOLUTION.
!        OTHERWISE, IR = 1.
!
      CALL SLVE (NN,MM,NR,X,Y,W,WC,IT,ETA,F,U,Q,T,R,IR,B,P,Z,V,S,NI)
!CCCC THE FOLLOWING WRITE SECTION WAS ACTIVATED   SEPTEMBER 1995
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,771)
  771    FORMAT(1H ,'*****FROM LSQ, AFTER 1ST CALL TO SLVE--')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,772)E
  772    FORMAT('AFTER 120--E = ',E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
      D = V(1)
!
      IF (IR.EQ.0) GO TO 130
      IF (ISCALE.EQ.1) GO TO 120
      ISCALE = 1
      GO TO 10
 120  CONTINUE
!CCCC THE FOLLOWING LINE WAS ACTIVATED   SEPTEMBER 1995
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,773)ISCALE
  773    FORMAT('FROM LSQ, AFTER 120--ISCALE = ',I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
!     GO TO 240
      ID =2
      RETURN
!
!     COMPUTATIONS NEEDED FOR COMPUTING ACCURATE DIGITS.
!        SUBROUTINE SLVE IS NOW CALLED TO OBTAIN A VECTOR OF
!        COEFFICIENTS (A) BY FITTING PREDICTED VALUES (Y - Z) INSTEAD OF
!        THE ORIGINAL OBSERVATIONS (Y).  A COMPARISON OF VECTOR B WITH
!        VECTOR A IS USED TO ASSESS THE ACCURACY OF VECTOR B.
!        THIS CALL TO SLVE IS OMITTED WHENEVER --
!           L1 = 24  (TWOWAY)
!           L2 =  2  (SPOLYFIT)
!           L2 =  4  (SFIT)
!
!130  IF (L1.EQ.24) GO TO 140
!     IF (L2.EQ.2.OR. L2.EQ.4) GO TO 140
!
 130  IZ  = ISCALE
      ITT = IT + 2
!
      CALL SLVE (NN,MM,NR,X,Y,W,WC,ITT,ETA,F,U,Q,T,R,IZ,A,Z,P,V,S,NJ)
!CCCC THE FOLLOWING WRITE SECTION WAS ACTIVATED   SEPTEMBER 1995
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
!
!     ..................................................................
!
!     COMPUTE SQUARED FOURIER COEFFICIENTS (S) NEEDED FOR ANALYSIS OF
!        VARIANCE.
!
 140  L = MP1
      DO 150 J=1,MM
!CCCC   LD = IDIV (2*(J-1)*(MM+1)-J*J+3*J,2,IRR)
       CALL IDIV  (2*(J-1)*(MM+1)-J*J+3*J,2,IRR,LD)
        S(J) = R(LD) * R(L)**2
        L = L + MP1 - J
 150  CONTINUE
!
!     CALL SUBROUTINE SCALE TO ADJUST RESIDUALS (Z) AND SQUARED
!        FOURIER COEFFICIENTS (S) FOR SCALING, IF DATA WERE SCALED.
!
      IF (ISCALE.EQ.1) THEN
      CALL SCALDP (ISCALE,3,NN,MM,IT,NR,W,WC,X,U,Q,S,B,A,Z,R,F,IFAULT)
      IF (IFAULT.EQ.1) GO TO 420
      ENDIF
!     ADJUST THE FIRST SQUARED FOURIER COEFFICIENT IF Y MID-RANGE WAS
!        SUBTRACTED FROM Y-VECTOR.  IN THIS CASE C IS NONZERO.
!
      YINC = C
!CCCC IF (YINC.NE.0.0) S(1) = R(1) * ( SPDIV(R(MP1),F(MP1),IND) +
!CCCC1  SPDIV(YINC,F(1),IRR) )**2
      IF(YINC.NE.0.0)CALL SPDIV(R(MP1),F(MP1),IND,RESUL1)
      IF(YINC.NE.0.0)CALL SPDIV(YINC,F(1),IRR,RESUL2)
      IF(YINC.NE.0.0)S(1)=R(1)*(RESUL1+RESUL2)**2
!
!     COMPUTE RESIDUAL SUM OF SQUARES (E) AND RESIDUAL STANDARD
!        DEVIATION (SD).
!
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
!CCCC RSS = DPCON (SUM)
      CALL  DPCON (SUM,RSS)
!
      IF (NN.EQ.MM) GO TO 170
      GO TO 180
!
 170  RMS = 0.0
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,873)NN,MM,RSS,WC
  873    FORMAT('FROM LSQ,170--NN,MM,RSS,WC = ',2I8,2E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 210
!
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
!CCCC RMS = SPDIV (RSS,RESDF,IRR)
      CALL  SPDIV (RSS,RESDF,IRR,RMS)
!210  SD = SPSQRT (RMS)
 210  CONTINUE
      CALL SPSQRT (RMS,RESULT)
      SD=RESULT
      E = RSS
!
!     CALL SUBROUTINE SDPRED TO COMPUTE STANDARD DEVIATION OF PREDICTED
!        VALUES (V).
!
      CALL SDPRED (NN,MM,R,Q,T,SD,V)
!
!     CALL SUBROUTINE PINVRT TO OBTAIN THE INVERSE OF (X-TRANSPOSE)*W*X
!        USING RESULTS FROM PDECOM (MATRIX R) AS INPUT.
!
!     MATRIX R IS OVERWRITTEN AND WILL EQUAL THE DESIRED INVERSE UPON
!        RETURN TO SUBROUTINE LSQ.
!
!     SINCE THE INVERSE MATRIX IS SYMMETRIC, ONLY THE PORTION ON OR
!        ABOVE THE PRINCIPAL DIAGONAL IS STORED.  COMMENTS AT THE
!        BEGINNING OF SUBROUTINE PINVRT GIVE FURTHER DETAILS.
!
      CALL PINVRT (MM,R,T)
!
!     CALL SUBROUTINE SCALE TO ADJUST COEFFICIENTS (B AND A) AND
!        COVARIANCE MATRIX (R) FOR SCALING, IF DATA WERE SCALED.
!
      IF (ISCALE.EQ.1) THEN
      CALL SCALDP (ISCALE,4,NN,MM,IT,NR,W,WC,X,U,Q,S,B,A,Z,R,F,IFAULT)
      IF (IFAULT.EQ.1) GO TO 420
      ENDIF
!
!     COMPUTE STANDARD DEVIATIONS OF COEFFICIENTS (T).
!
      DO 230 I=1,MM
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
         WRITE(ICOUT,777)I,R(I),RMS,RESDF,RSS
  777    FORMAT('FROM LSQ,230--I,R(I),RMS,RESDF,RSS = ',I8,4E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
!CCCC   L = IDIV  (2*(I-1)*MM-I*I+3*I,2,IRR)
        CALL IDIV (2*(I-1)*MM-I*I+3*I,2,IRR,L)
        IF (R(L).GE.0.0) GO TO 220
        R(L) = 0.0
!220    T(I) = SPSQRT (R(L)*RMS)
 220    CONTINUE
        CALL   SPSQRT (R(L)*RMS,RESULT)
        T(I) = RESULT
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'LSQ')THEN
          WRITE(ICOUT,778)I,T(I)
  778     FORMAT('FROM LSQ,230--I,T(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 230  CONTINUE
!
!     SET VALUE OF ID.
 240  ID=NI
      RETURN
!
 420  ID = 1
!     IF (ISCALE.EQ.0) ID = - ID
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE LSQ 
!CCCC-----SCALE--------------------------------------
      SUBROUTINE SCALDP (IS,NC,N,M,IT,NR,W,WC,X,U,Q,SS,B,A,Z,R,SF,IFT)
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     SUBROUTINE SCALE SCALES THE MATRIX Q IN ORDER TO MITIGATE THE
!        ROUNDING ERROR PROBLEMS WHICH CAN OCCUR IN CONNECTION WITH
!        SOLVING ILL-CONDITIONED SYSTEMS OF EQUATIONS.  THIS IS DONE BY
!        MULTIPLYING EACH COLUMN OF Q BY ITS APPROPRIATE SCALE FACTOR SO
!        THAT THE COLUMNS OF THE SCALED MATRIX ALL HAVE UNIT LENGTH.  IN
!        THE CASE OF POLYNOMIAL TYPE PROBLEMS, THE MEAN OF THE X-VECTOR
!        IS COMPUTED SO THAT IT CAN BE SUBTRACTED FROM EACH ELEMENT OF
!        X WHENEVER POWERS OF X ARE GENERATED (IN SUBROUTINES LSQ AND
!        SLVE).  AFTER A SOLUTION IS OBTAINED FOR A SCALED PROBLEM, THE
!        COEFFICIENTS, RESIDUALS, SQUARED FOURIER COEFFICIENTS AND
!        COVARIANCE MATRIX MUST BE ADJUSTED TO ACCOUNT FOR SCALING.
!
!     REFERENCE --
!        A. BJORCK, COMMENT ON THE ITERATIVE REFINEMENT OF LEAST-SQUARES
!        SOLUTIONS, JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
!        VOL. 73 (1978), PP. 161-166.
!
!               WRITTEN BY -
!                      ROY H. WAMPLER,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL BUREAU OF STANDARDS,
!                      GAITHERSBURG, MD. 20899
!                          TELEPHONE 301-975-2844
!
!     UPDATED--NOVEMBER  1989--DIMENSION (1) TO (*) (AND MOVE)
!     UPDATED         --NOVEMBER  2009. RENAME "SCALE" TO "SCALDP".  THIS
!                                       IS SIMPLY TO AVOID COMPILATION
!                                       ISSUES WITH VERSION 11 OF THE
!                                       INTEL COMPILER ON WINDOWS
!                                       (CONFLICTS WITH INTRINSIC
!                                       SCALE FUNCTION EVEN IF AN
!                                       EXTERNAL STATEMENT IS USED)
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!
!
!CCCC THE FOLLOWING 5 LINES WERE MOVED       NOVEMBER 1989
!CCCC AND CHANGED DIMENSION (1) TO (*)
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC REAL             A(1), B(1), Q(1), R(1), SF(1), SS(1)
!CCCC REALCCCCC        W(1), X(NR,1), Z(1)
!CCCC REAL             W(1), X, Z(1)
!CCCC REAL             U, WC
!CCCC REAL             VNORM2, WW
!
!CCCC REAL             SPDIV, DPCON
!
      DOUBLE PRECISION DSUM
!CCCC DOUBLE PRECISION DPDIV, DPSQRT
      DOUBLE PRECISION DRESUL
!
      REAL             A(*), B(*), Q(*), R(*), SF(*), SS(*)
!CCCC REAL             W(1), X(NR,1), Z(1)
      REAL             W(*), X, Z(*)
      REAL             U, WC
      REAL             VNORM2, WW
!
!CCCC INCLUDE 'DPCOPA.INC'
!CCCC DIMENSION X(MAXOBV,MAXCMF)
      DIMENSION X(NR,*)
!
!     ==================================================================
!
      MP1 = M + 1
      IFT = 0
!CCCC TEMPORARY CHANGE OF NCC TO NC AS SUGGESTED BY RUTH VARNER MAY 1989
!CCCC GO TO (10,80,100,130), NCC
      GO TO (10,80,100,130), NC
  10  IF (IS.EQ.1) GO TO 30
!
!     IS = 0.  SET SF(I) = 1.0 FOR I=1,...,M+1.
!
      DO 20 I=1,MP1
        SF(I) = 1.0
  20  CONTINUE
      RETURN
!
!     ..................................................................
!
!     IS = 1.  COMPUTE VECTOR NORMS.
!                  COMPUTE SCALE FACTORS (SF).
!                  SCALE MATRIX Q.
!
  30  WW = WC
      DO 70 J=1,MP1
        DSUM = 0.0D0
        K = (J-1) * N + 1
        DO 40 I=1,N
          IF (WC.LE.0.0) WW = W(I)
          DSUM = DSUM + DBLE (Q(K)) * DBLE (Q(K)) * DBLE (WW)
          K = K + 1
  40    CONTINUE
!CCCC   DSUM   = DPSQRT (DSUM)
        CALL     DPSQRT (DSUM,DRESUL)
        DSUM   = DRESUL
!CCCC   VNORM2 = DPCON (DSUM)
        CALL     DPCON (DSUM,VNORM2)
!
!       VECTOR NORMS COULD BE SAVED HERE, IF DESIRED.
!
        IF (VNORM2.GT.0.0) GO TO 50
        IFT = 1
!
!       IFT = 1 INDICATES ERROR RETURN.
!
        RETURN
!
!     ..................................................................
!
!C50    SF(J) = SPDIV (1.0,VNORM2,IRR)
   50 CONTINUE
        CALL    SPDIV (1.0,VNORM2,IRR,SF(J))
!
!       SCALE MATRIX Q.
!
        K = (J-1) * N + 1
        DO 60 I=1,N
          Q(K) = Q(K) * SF(J)
          K    = K + 1
  60    CONTINUE
  70  CONTINUE
      RETURN
!
!     ..................................................................
!
!     COMPUTE MEAN OF X VECTOR (DENOTED BY U) FOR POLYNOMIAL TYPE
!        PROBLEMS.
!
  80  DSUM = 0.0D0
      NW   = 0
      DO 90 I=1,N
        L    = L + 1
        IF (WC.LE.0.0 .AND. W(I).EQ.0.0) GO TO 90
        NW   = NW + 1
        DSUM = DSUM + DBLE (X(I,1))
  90  CONTINUE
!CCCC U = DPCON (DPDIV (DSUM,DBLE (FLOAT (NW)),IRR))
      CALL        DPDIV (DSUM,DBLE (FLOAT (NW)),IRR,DRESUL)
!CCCC U = DPCON (DRESUL)
      CALL DPCON (DRESUL,U)
      RETURN
!
!     ..................................................................
!
!     ADJUST SQUARED FOURIER COEFFICIENTS (SS) AND RESIDUALS (Z) FOR
!        SCALING.
!
 100   DO 110 J=1,M
!CCCC   SS(J) = SPDIV (SS(J),SF(MP1)*SF(MP1),IRR)
        CALL    SPDIV (SS(J),SF(MP1)*SF(MP1),IRR,SS(J))
 110  CONTINUE
!
      DO 120 I=1,N
!CCCC   Z(I) = SPDIV (Z(I),SF(MP1),IRR)
        CALL   SPDIV (Z(I),SF(MP1),IRR,Z(I))
 120  CONTINUE
      RETURN
!
!     ..................................................................
!
!     ADJUST COEFFICIENTS (B AND A) AND COVARIANCE MATRIX (R) FOR
!        SCALING.
!
 130  DO 140 J=1,M
!CCCC   B(J) = SPDIV (B(J) * SF(J),SF(MP1),IRR)
        CALL   SPDIV (B(J) * SF(J),SF(MP1),IRR,B(J))
!CCCC   A(J) = SPDIV (A(J) * SF(J),SF(MP1),IRR)
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
!
!     ..................................................................
!
!     COMPLETE ADJUSTMENTS OF B, A AND R FOR SCALING IN POLYNOMIAL TYPE
!        PROBLEMS.
!     REFERENCE --
!        G. A. F. SEBER, LINEAR REGRESSION ANALYSIS (1977), THEOREM
!        1.4 AND COROLLARIES, PAGES 10-11.
!
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
!CCCC     SF(J) = DPCON (-DPDIV (DBLE(FLOAT(J-1)),DBLE(FLOAT(J-I)),IND)
!CCCC1    * DBLE (SF(J-1)) * DBLE (U) )
          CALL   DPDIV (DBLE(FLOAT(J-1)),DBLE(FLOAT(J-I)),IND,DRESUL)
!CCCC     SF(J) = DPCON (-DRESUL)
!CCCC1    * DBLE (SF(J-1)) * DBLE (U)
          CALL    DPCON (-DRESUL,RESULT)
          SF(J) = RESULT   &
          * DBLE (SF(J-1)) * DBLE (U)
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
!CCCC     SF(I) = DPCON (-DPDIV (DBLE(FLOAT(I-1)),DBLE(FLOAT(I-J)),IND)
!CCCC1    * DBLE (SF(I-1)) * DBLE (U) )
          CALL   DPDIV (DBLE(FLOAT(I-1)),DBLE(FLOAT(I-J)),IND,DRESUL)
!CCCC     SF(I) = DPCON (-DRESUL)
!CCCC1    * DBLE (SF(I-1)) * DBLE (U)
          CALL    DPCON (-DRESUL,RESULT)
          SF(I) = RESULT   &
          * DBLE (SF(I-1)) * DBLE (U)
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
!
!     ==================================================================
!
      END SUBROUTINE SCALDP 
!CCCC-----PDECOM--------------------------------------
      SUBROUTINE PDECOM (KN,KM,TOL,W,WCC,ISING,M1,Q,D,R)
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     SUBROUTINE PDECOM USES A MODIFIED GRAM-SCHMIDT ALGORITHM TO OBTAIN
!        AN ORTHOGONAL QR-DECOMPOSITION OF THE INPUT MATRIX GIVEN IN Q.
!
!               WRITTEN BY -
!                      ROY H. WAMPLER,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL BUREAU OF STANDARDS,
!                      GSITHERSBURG, MD. 20899
!                          TELEPHONE 301-975-2844
!
!     UPDATED--NOVEMBER  1989--DIMENSION (1) TO (*) (AND MOVE)
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!
!CCCC THE FOLLOWING 3 LINES WERE MOVED        NOVEMBER 1989
!CCCC AND DIMENSION (1) CHANGED TO DIMENSION (*)
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC REAL             D(1), Q(1), R(1), W(1)
!CCCC REAL             TOL, WCC
!CCCC REAL             DMAX, DS, RSJ, TOL2, WW
!
!CCCC REAL             SPDIV, DPCON
!
      DOUBLE PRECISION DSUM
!
      REAL             D(*), Q(*), R(*), W(*)
      REAL             TOL, WCC
      REAL             DMAX, DS, RSJ, TOL2, WW
!
!     ==================================================================
!
      WW    = WCC
      ISING = 1
      M     = KM
      N     = KN
      M1    = 0
!CCCC M2 = IDIV (M*(M+1),2,IRR)
      CALL IDIV (M*(M+1),2,IRR,M2)
      DO 10 J=1,M
        D(J) = 0.0
  10  CONTINUE
!
      DO 20 L=1,M2
        R(L) = 0.0
  20  CONTINUE
!
      TOL2 = TOL * TOL
      DMAX = 0.0
      DO 110 I=1,M
!
!     STEP NUMBER I IN THE DECOMPOSITION.
!
        DSUM = 0.0D0
        DO 30 L=1,N
          IF (WCC.LE.0.0) WW = W(L)
          J = (I-1) * N + L
          DSUM = DSUM + DBLE (Q(J)) * DBLE (Q(J)) * DBLE (WW)
  30    CONTINUE
!
!CCCC   D(I) = DPCON (DSUM)
        CALL   DPCON (DSUM,D(I))
        DS = D(I)
        IF (I.GT.1) GO TO 40
        DMAX = D(1)
        GO TO 50
!
  40    IF (DS.GT.DMAX) DMAX = D(I)
  50    DO 60 J=1,I
          IF (D(J).LE.TOL2*DMAX) RETURN
  60    CONTINUE
!
        IF (DS.EQ.0.0) RETURN
        IPLUS1 = I + 1
        IF (IPLUS1.GT.M) GO TO 100
!
!     BEGIN ORTHOGONALIZATION.
!
!CCCC   LD = IDIV (2*(I-1)*M-I*I+3*I,2,IRR)
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
!
          L = LD + K
!CCCC     R(L) = DPCON (DSUM)
          CALL   DPCON (DSUM,R(L))
!CCCC     R(L) = SPDIV (R(L),DS,IRR)
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
!
  90    CONTINUE
!
!     END ORTHOGONALIZATION.
!
 100    M1 = I
        IF (I.EQ.M-1) ISING = 0
 110  CONTINUE
!
!     END STEP NUMBER I.
!
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE PDECOM 
!CCCC-----SLVE--------------------------------------
      SUBROUTINE SLVE (N,M,NR,X,Y,W,WA,IT,E,S,U,Q,D,A,K,B,R,Z,F,G,NI)
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     SUBROUTINE SLVE COMPUTES THE SOLUTION (COEFFICIENTS AND RESIDUALS)
!        OF THE LEAST SQUARES PROBLEM.  ITERATIVE REFINEMENT IS USED TO
!        IMPROVE (IF POSSIBLE) THE ACCURACY OF THE INITIAL SOLUTION.
!
!     SUBROUTINE SLVE IS GENERALLY CALLED TWICE FROM SUBROUTINE LSQ.
!        IN THE FIRST CALL, THE OBSERVATIONS (Y) ARE FITTED.  LET R
!           DENOTE THE RESIDUALS FROM THIS FIT.
!        IN THE SECOND CALL, THE PREDICTED VALUES (Y - R) ARE FITTED.
!           THE COEFFICIENTS OBTAINED FROM THIS FIT WILL BE USED IN
!           ASSESSING THE ACCURACY OF THE COEFFICIENTS FROM THE FIRST FIT.
!
! *   CONVERSION OF THE PROGRAM TO STRICTLY DOUBLE PRECISION, AND      *
! *   CONVERSION OF THE PROGRAM TO STRICTLY SINGLE PRECISION.          *
! *      ON COMPUTERS HAVING SHORT WORD LENGTH (AS THE IBM 360/370)    *
! *      IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN DOUBLE     *
! *      PRECISION.  ON COMPUTERS HAVING LONG WORD LENGTH (AS THE CDC  *
! *      6600) IT MAY BE DESIRABLE TO PERFORM ALL CALCULATIONS IN      *
! *      SINGLE PRECISION.  IN SUCH CASES, THE ITERATIVE REFINEMENT    *
! *      PRESENTLY INCLUDED IN SUBROUTINE SLVE SHOULD BE OMITTED.      *
! *                                                                    *
! *      THE SIMPLEST WAY TO OBTAIN THE EFFECT OF OMITTING THE         *
! *      ITERATIVE REFINEMENT (WITHOUT ACTUALLY DOING SO) IS TO CHANGE *
! *      THE ONE STATEMENT WHICH PRESENTLY READS                       *
! *        310  K = 1 (USE THIS FOR 64-BIT MACHINES)                *
! *      TO READ                                                       *
! *        310  K = 0 (USE THIS FOR 32-BIT MACHINES)               *
! *                                                                    *
! *      TO ACTUALLY OMIT THE ITERATIVE REFINEMENT THE FOLLOWING       *
! *      APPROACH MAY BE USED.                                         *
! *      1. OMIT USAGE OF E, ETA2, RNB, RNDB1, RNDB2, RNDR1, RNDR2,    *
! *         RNR, AND SPCA FROM SUBROUTINE, REAL, AND DATA STATEMENTS.  *
! *      2. ATTACH LABEL  30  TO THE STATEMENT WHICH PRESENTLY READS   *
! *               DO 50 I=1,KN                                         *
! *      3. INSERT A STATEMENT READING                                 *
! *               GO TO 320                                            *
! *         IMMEDIATELY BEFORE THE STATEMENT WHICH PRESENTLY READS     *
! *          160  DO 210 ISX=1,KM                                      *
! *      4. OMIT THE FOUR BLOCKS OF STATEMENTS WHICH ARE SET OFF IN    *
! *         THE FOLLOWING MANNER --                                    *
! *                                                                    *
! BLOCK I ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!               (STATEMENTS TO BE OMITTED)
!
! BLOCK I (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! *                                                                    *
! *         BLOCK 1 CONTAINS  3 STATEMENTS (EXCLUDING COMMENTS).       *
! *         BLOCK 2 CONTAINS 10 STATEMENTS (EXCLUDING COMMENTS).       *
! *         BLOCK 3 CONTAINS 22 STATEMENTS (EXCLUDING COMMENTS).       *
! *         BLOCK 4 CONTAINS  4 STATEMENTS (EXCLUDING COMMENTS).       *
! *                                                                    *
!               WRITTEN BY -
!                      ROY H. WAMPLER,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL BUREAU OF STANDARDS,
!                      GAITHERSBURG, MD. 20899
!                          TELEPHONE 301-975-2844
!
!     UPDATED--NOVEMBER  1989--DIMENSION (1) TO (*) (AND MOVED)
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!CCCC THE FOLLOWING 9 LINES WERE MOVED      NOVEMBER 1989
!CCCC AND CHANGED DIMENSION (1) TO (*)
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC REAL             A(1), B(1), D(1), F(1), G(1), Q(1)
!CCCC REALCCCCC        R(1), S(1), W(1), X(NR,M), Y(1), Z(1)
!CCCC REAL             R(1), S(1), W(1), X, Y(1), Z(1)
!CCCC REAL             E, U, WA
!CCCC REAL             C, ETA2, DIGITS, DXNORM
!CCCC REAL             RNB, RNDB1, RNDB2, RNDR1, RNDR2
!CCCC REAL             RNR, WC, WW, XNORM
!CCCC REALCCCCC        SPDIV, DPCON, SPLO10, SPSQRT
!CCCC REAL             SPCA
!
      DOUBLE PRECISION DX, DSUM, DY
!
      REAL             A(*), B(*), D(*), F(*), G(*), Q(*)
!CCCC REAL             R(*), S(*), W(*), X(NR,M), Y(*), Z(*)
      REAL             R(*), S(*), W(*), X, Y(*), Z(*)
      REAL             E, U, WA
      REAL             C, ETA2, DIGITS, DXNORM
      REAL             RNB, RNDB1, RNDB2, RNDR1, RNDR2
      REAL             RNR, WC, WW, XNORM
!CCCC REAL             SPDIV, DPCON, SPLO10, SPSQRT
      REAL             SPCA
!
!CCCC INCLUDE 'DPCOPA.INC'
      DIMENSION X(NR,*)
!
!     ==================================================================
!
!                 ***   DATA INITIALIZATION STATEMENTS   ***
!
      DATA SPCA / 64.0 /
!
!     ==================================================================
!
!     SET ISWAD = 0 IF COEFFICIENTS FOR ACCURATE DIGITS ARE NOT BEING
!                   COMPUTED.
!     SET ISWAD = 1 IF COEFFICIENTS FOR ACCURATE DIGITS ARE BEING
!                   COMPUTED.
!
      ISWAD = 0
      IF (IT.GT.2) ISWAD = 1
      KN = N
      KM = M
      MN = KM * KN
      WC = WA
      WW = 0.0
      ITYP   = IT
      IF (ITYP.GT.2) ITYP = ITYP - 2
      MPLUS1 = KM + 1
      DIGITS = 0.0
!
! BLOCK 1 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!CCCC ITMAX = INT (-SPLO10(E)) - 2   JUNE 1987
      CALL SPLO10(E,RESULT)
      ITMAX = INT (-RESULT)    - 2
      IF (K.EQ.1) ITMAX = ITMAX + 3
      ETA2 = E * E
!
! BLOCK 1 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     USE ELEMENTS M*N+1, M*N+2, ..., M*N+N OF ARRAY Q AS WORK AREA.
!
!CCCC IF (WC.GT.0.0) WW = SPSQRT(WC)
      IF (WC.GT.0.0) CALL SPSQRT(WC,RESULT)
      IF (WC.GT.0.0) WW = RESULT
      DO 10 I=1,KN
!CCCC   IF (WC.LE.0.0) WW = SPSQRT(W(I))
        IF (WC.LE.0.0) CALL SPSQRT(W(I),RESULT)
        IF (WC.LE.0.0) WW = RESULT
        IF (ISWAD.EQ.0) F(I) = Y(I) * WW * S(MPLUS1)
!CCCC   IF (ISWAD.EQ.1 ) F(I) = (Y(I)-SPDIV(R(I),S(MPLUS1),IND)) * WW
!CCCC1                            * S(MPLUS1)
        IF (ISWAD.EQ.1 ) CALL         SPDIV(R(I),S(MPLUS1),IND,RESULT)
        IF (ISWAD.EQ.1 ) F(I) = (Y(I)-RESULT)                   * WW   &
                                  * S(MPLUS1)
        J = MN + I
        Q(J) = 0.0
        Z(I) = 0.0
  10  CONTINUE
!
      DO 20 J=1,KM
        B(J) = 0.0
        G(J) = 0.0
  20  CONTINUE
!
      KI    = 0
      RNR   = 0.0
      RNB   = 0.0
      RNDB1 = 0.0
      RNDR1 = 0.0
!
! BLOCK 2 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      RNDB2 = 0.0
      RNDR2 = 0.0
!
!     BEGIN KI-TH ITERATION STEP.
!
  30  IF (KI.LT.2) GO TO 40
      IF (SPCA*RNDB2.LT.RNDB1 .AND. RNDB2.GT.ETA2*RNB .OR.   &
          SPCA*RNDR2.LT.RNDR1 .AND. RNDR2.GT.ETA2*RNR) GO TO 40
      GO TO 300
!
  40  RNDB1 = RNDB2
      RNDR1 = RNDR2
      RNDB2 = 0.0
      RNDR2 = 0.0
!
! BLOCK 2 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      IF (KI.EQ.0) GO TO 160
!
!     NEW RESIDUALS.
!
      DO 50 I=1,KN
!CCCC   IF (WC.LE.0.0) WW = SPSQRT(W(I))
        IF (WC.LE.0.0) CALL SPSQRT(W(I),RESULT)
        IF (WC.LE.0.0) WW = RESULT
               J = MN + I
        Q(J) = Q(J) + F(I) * WW
!CCCC   Z(I) = Z(I) + SPDIV (F(I),WW,IRR)
        CALL          SPDIV (F(I),WW,IRR,RESULT)
        Z(I) = Z(I) + RESULT
  50  CONTINUE
!
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
!
  70    DO 80 L=1,KN
          J    = MN + L
          DSUM = DSUM + DBLE (Q(J)) * DBLE (X(L,ISX) * S(ISX))
  80    CONTINUE
!
!C90    G(ISX) = -DPCON (DSUM)
  90    CONTINUE
        CALL      DPCON (DSUM,RESULT)
        G(ISX) = -RESULT
 100  CONTINUE
!
      DO 150 I=1,KN
        DSUM = DBLE ( Z(I) )
        IF (ITYP.EQ.2) GO TO 120
        DSUM = DSUM + DBLE (B(1)) * DBLE (S(1))
        IF (KM.EQ.1) GO TO 140
        DO 110 L=2,KM
          DSUM = DSUM + DBLE(B(L))*DBLE(X(I,1)-U)**(L-1)*DBLE(S(L))
 110    CONTINUE
        GO TO 140
!
 120    DO 130 L=1,KM
          DSUM = DSUM + DBLE(B(L)) * DBLE(X(I,L) * S(L))
 130    CONTINUE
!
 140    DY = DBLE ( Y(I) )
!CCCC   IF (ISWAD.EQ.1) DY = DBLE (Y(I) - SPDIV (R(I),S(MPLUS1),IND) )
        IF (ISWAD.EQ.1) CALL         SPDIV (R(I),S(MPLUS1),IND,RESULT)
        IF (ISWAD.EQ.1) DY = DBLE (Y(I) - RESULT                    )
        DSUM = DSUM - DY * DBLE (S(MPLUS1))
!CCCC   F(I) = -DPCON (DSUM)
        CALL    DPCON (DSUM,RESULT)
        F(I) = -RESULT
!CCCC   IF (WC.LE.0.0) WW = SPSQRT(W(I))
        IF (WC.LE.0.0) CALL SPSQRT(W(I),RESULT)
        IF (WC.LE.0.0) WW = RESULT
        F(I) = F(I) * WW
!CCCC   IF (WW.EQ.0.0) Z(I) = DPCON (DBLE (Z(I)) - DSUM)
        IF (WW.EQ.0.0) CALL   DPCON (DBLE (Z(I)) - DSUM,Z(I))
 150  CONTINUE
!
!     END NEW RESIDUALS.
!
 160  DO 210 ISX=1,KM
        LESS1 = ISX - 1
        DSUM  = - DBLE (G(ISX))
        IF (1.GT.LESS1) GO TO 180
        J    = ISX
        DO 170 L=1,LESS1
          DSUM = DSUM + DBLE (D(L)) * DBLE (A(J))
          J = J + MPLUS1 - L
 170    CONTINUE
!
!180    D(ISX) = - DPCON (DSUM)
 180    CONTINUE
        CALL       DPCON (DSUM,RESULT)
        D(ISX) = - RESULT
        DO 190 L=1,KN
!CCCC     IF (WC.LE.0.0) WW = SPSQRT (W(L))
          IF (WC.LE.0.0) CALL SPSQRT (W(L),RESULT)
          IF (WC.LE.0.0) WW = RESULT
          JJ   = (ISX-1) * KN + L
          DSUM = DSUM + DBLE (F(L)) * DBLE (Q(JJ)) * DBLE (WW)
 190    CONTINUE
!
!CCCC   C  = DPCON (DSUM)
        CALL DPCON (DSUM,C)
!CCCC   LD = IDIV (2*(ISX-1)*(MPLUS1)-ISX*ISX+3*ISX,2,IRR)
        CALL IDIV (2*(ISX-1)*(MPLUS1)-ISX*ISX+3*ISX,2,IRR,LD)
!CCCC   C  = SPDIV (C,A(LD),IRR)
        CALL SPDIV (C,A(LD),IRR,C)
        G(ISX) = C
        DO 200 I=1,KN
!CCCC     IF (WC.LE.0.0) WW = SPSQRT (W(I))
          IF (WC.LE.0.0) CALL SPSQRT (W(I),RESULT)
          IF (WC.LE.0.0) WW = RESULT
          JJ   = (ISX-1) * KN + I
          F(I) = F(I) - C * Q(JJ) * WW
 200    CONTINUE
!
 210  CONTINUE
      DO 240 IS=1,KM
        ISX    = MPLUS1 - IS
        IPLUS1 = ISX + 1
        DSUM   = DBLE (-G(ISX))
        IF (IPLUS1.GT.KM) GO TO 230
!CCCC   LD     = IDIV (2*(ISX-1)*(MPLUS1)-ISX*ISX+3*ISX,2,IRR)
        CALL     IDIV (2*(ISX-1)*(MPLUS1)-ISX*ISX+3*ISX,2,IRR,LD)
        J      = 0
        DO 220 L=IPLUS1,KM
          J    = J + 1
          LJ   = LD + J
          DSUM = DSUM + DBLE (G(L)) * DBLE (A(LJ))
 220    CONTINUE
!230    G(ISX) = - DPCON (DSUM)
 230    CONTINUE
        CALL       DPCON (DSUM,RESULT)
        G(ISX) = - RESULT
 240  CONTINUE
!
! BLOCK 3 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      DSUM = RNDB2
      DO 250 ISX=1,KM
        DSUM = DSUM + DBLE (G(ISX) * G(ISX) )
 250  CONTINUE
!
!CCCC RNDB2 = DPCON (DSUM)
      CALL    DPCON (DSUM,RNDB2)
      DSUM  = RNDR2
      DO 260 I=1,KN
        DSUM = DSUM + DBLE (F(I) * F(I) )
 260  CONTINUE
!
!CCCC RNDR2 = DPCON (DSUM)
      CALL    DPCON (DSUM,RNDR2)
      IF (KI.NE.0) GO TO 270
      RNB = RNDB2
      RNR = RNDR2
!
!     COMPUTE DIGITS = AVERAGE NUMBER OF DIGITS IN AGREEMENT BETWEEN
!                         INITIAL SOLUTION AND FIRST ITERATION.
!
 270  IF (KI.NE.1) GO TO 290
!CCCC XNORM  = SPSQRT (RNB)
      CALL     SPSQRT (RNB,RESULT)
      XNORM  = RESULT
!CCCC DXNORM = SPSQRT (RNDB2)
      CALL     SPSQRT (RNDB2,RESULT)
      DXNORM = RESULT
      IF (XNORM.NE.0.0) GO TO 280
!CCCC DIGITS = - SPLO10 (E)  JUNE 1987
      CALL SPLO10(E,RESULT)
      DIGITS = - RESULT
      GO TO 290
!
!280  DIGITS = - SPLO10 (AMAX1(SPDIV(DXNORM,XNORM,IND),E))
  280 CONTINUE
!CCCC CALL       SPLO10 (AMAX1(SPDIV(DXNORM,XNORM,IND),E),RESULT)
      CALL                     SPDIV(DXNORM,XNORM,IND,RESUL2)
      CALL       SPLO10 (AMAX1(RESUL2,E),RESULT)
      DIGITS = - RESULT
!
!     END KI-TH ITERATION STEP.
!
 290  KI = KI + 1
      IF (KI.GT.ITMAX) GO TO 310
!
! BLOCK 3 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      GO TO 30
!
! BLOCK 4 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
 300  IF (RNDR2.GT.4.0*ETA2*RNR .AND. RNDB2.GT.4.0*ETA2*RNB) GO TO 310
      K = 0
      GO TO 320
!
!     NOTE: IF SINGLE PRECISION = DOUBLE PRECISION, THEN YOU WANT TO
!           EFFECTIVELY OMIT ITERATIVE REFINEMENT.
!310  K = 1    COMMENTED OUT (JUNE 1987) TO GIVE CORRECT ANSWERS ON THE VAX.
!310  K = 0
 310  CONTINUE
!CCCC print *,'k = ',k
      K = 0
!
! BLOCK 4 (END) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
 320  NI   = KI - 1
      F(1) = DIGITS
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE SLVE 
!CCCC-----DSUMAL--------------------------------------
      SUBROUTINE DSUMAL (DX,NN,SNEG,SPOS,SUM)
!CCCC SUBROUTINE DSUMAL (DX,NN,SUM)
!CCCC THE ARGUMENTS SNEG AND SPOS WERE ADDED     SEPTEMBER 1995
!CCCC UPDATED--SEPTEMBER 1995 HAVE SNEG & SPOS AS  INPUT/OUTPUT ARGUMENTS
!CCCC                         TO AVOID FAILURE-TO-SAVE ON SOME COMPUTERS
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     ALGORITHM DESCRIBED BY MALCOLM IN COM. OF ACM VOL. 14, NO. 11
!
!     SPECIAL ALGORITHM FOR SUMMING DOUBLE PRECISION NUMBERS.
!        (USE SUMMAL, IF NUMBERS ARE REAL.)
!
!     NN EQUALS       ZERO, CLEAR AREA TO PREPARE FOR NEW SUM.
!     NN EQUALS        ONE, OBTAIN FINAL SUM.
!     NN GREATER THAN ZERO, CLEAR, DO SUM ON NN TERMS AND GET FINAL SUM.
!     NN LESS THAN    ZERO, CONTINUE SUM FOR NEXT ABS(NN) TERMS,
!                              DO NOT GET FINAL SUM.
!
!               WRITTEN BY -
!                      SALLY T. PEAVY,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL BUREAU OF STANDARDS,
!                      GAITHERSBURG, MD. 20899
!                          TELEPHONE 301-975-2844
!
!     UPDATED--NOVEMBER  1989--DIMENSION (1) TO DIMENSION (*)
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!CCCC THE FOLLOWING LINE WAS MOVED AND       NOVEMBER 1989
!CCCC CONVERTED (1) TO (*)
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC DIMENSION DX(1)
!
      DOUBLE PRECISION             DX, SUM, SNEG, SPOS
!
      DIMENSION DX(*)
!
!     ==================================================================
!
!CCCC IF(NN) 30,10,20
      IF(NN.LT.0)THEN
        GO TO 30
      ELSEIF(NN.EQ.0)THEN
        GO TO 10
      ELSEIF(NN.GT.0)THEN
        GO TO 20
      ENDIF
  10  SPOS = 0.0
      SNEG = 0.0
      RETURN
!
!     ..................................................................
!
  20  IF (NN.EQ.1) GO TO 50
      SPOS = 0.0
      SNEG = 0.0
!
  30  N = IABS (NN)
      DO 40 I=1,N
        IF (DX(I).LT.0.0) SNEG = SNEG + DX(I)
        IF (DX(I).GE.0.0) SPOS = SPOS + DX(I)
  40  CONTINUE
!
      IF (NN.LT.0) RETURN
!
  50  SUM = SPOS + SNEG
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE DSUMAL 
      SUBROUTINE SDPRED (N,M,R,Q,SB,SD,SDYHAT)
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     SUBROUTINE SDPRED COMPUTES STANDARD DEVIATIONS OF PREDICTED
!        VALUES.
!
!               WRITTEN BY -
!                      ROY H. WAMPLER,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL BUREAU OF STANDARDS,
!                      GAITHERSBURG, MD. 20899
!                          TELEPHONE 301-975-2844
!
!     UPDATED--NOVEMBER  1989--DIMENSION (1) TO DIMENSION (*)
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!CCCC THE FOLLOWING LINE WAS TRANSLATED TO    NOVEMBER 1989
!CCCC 4 DIMENSION STATEMENTS (SEE BELOW)
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC REAL             Q(1), R(1), SB(1), SDYHAT(1)
      REAL             SD
!CCCC REAL             SPDIV, DPCON, SPSQRT
!
      DOUBLE PRECISION DSUM
!
      DIMENSION Q(*)
      DIMENSION R(*)
      DIMENSION SB(*)
      DIMENSION SDYHAT(*)
!
!     ==================================================================
!
      DO 10 J=1,M
!CCCC   L =  IDIV (2*(J-1)*(M+1)-J*J+3*J,2,IND)
        CALL IDIV (2*(J-1)*(M+1)-J*J+3*J,2,IND,L)
!CCCC   SB(J) = SPDIV (1.0,SPSQRT (R(L)),IND)
        CALL SPSQRT(R(L),RESULT)
!CCCC   SB(J) = SPDIV (1.0,RESULT,IND)
        CALL    SPDIV (1.0,RESULT,IND,SB(J))
  10  CONTINUE
!
      DO 30 I=1,N
        DSUM = 0.0D0
        DO 20 J=1,M
          L = (J-1) * N + I
          DSUM = DSUM + (DBLE (Q(L)) * DBLE (SB(J))) ** 2
  20    CONTINUE
!
!CCCC   SDYHAT(I) = DPCON (DSUM)
        CALL        DPCON (DSUM,SDYHAT(I))
        IF (SDYHAT(I).LT.0.0) SDYHAT(I) = 0.0
!CCCC   SDYHAT(I) = SD * SPSQRT (SDYHAT(I))
        CALL SPSQRT(SDYHAT(I),RESULT)
        SDYHAT(I) = SD * RESULT
  30  CONTINUE
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE SDPRED 
!CCCC-----PINVRT--------------------------------------
      SUBROUTINE PINVRT (M,R,D)
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     SUBROUTINE PINVRT OBTAINS THE UNSCALED COVARIANCE MATRIX OF THE
!        COEFFICIENTS, EQUAL TO THE INVERSE OF (X-TRANSPOSE)*W*X.
!        MATRIX R OBTAINED FROM SUBROUTINE PDECOM IS USED AS INPUT.
!        THIS MATRIX IS OVERWRITTEN AND ON EXIT WILL EQUAL THE DESIRED
!        INVERSE.
!
!     SINCE THE INVERSE MATRIX IS SYMMETRIC, ONLY THE PORTION ON OR
!        ABOVE THE PRINCIPAL DIAGONAL IS STORED.
!
!               WRITTEN BY -
!                      ROY H. WAMPLER,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL BUREAU OF STANDARDS,
!                      GAITHERSBURG,MD. 20899
!                          TELEPHONE 301-975-2844
!
!      UPDATED--NOVEMBER  1989--DIMENSION (1) TO DIMENSION (*)
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!CCCC THE FOLLOWING LINE WAS TRANSLATED INTO     NOVEMBER 1989
!CCCC 2 DIMENSION STATEMENTS (SEE BELOW)
!CCCC (BUG UNCOVERED BY NELSON HSU)
!CCCC REAL             D(1), R(1)
!
!CCCC REAL             SPDIV, DPCON
!
      DOUBLE PRECISION DSUM
!
      DIMENSION D(*)
      DIMENSION R(*)
!
!     ==================================================================
!
      DO 10 L=1,M
!CCCC   LL = IDIV (2*(L-1)*(M+1)-L*L+3*L,2,IRR)
        CALL IDIV (2*(L-1)*(M+1)-L*L+3*L,2,IRR,LL)
!CCCC   R(LL) = SPDIV (1.0,R(LL),IRR)
        CALL    SPDIV (1.0,R(LL),IRR,R(LL))
  10  CONTINUE
!
      IF (M.EQ.1) RETURN
      L = M
  20  J = L - 1
!CCCC LJ = IDIV (2*(J-1)*(M+1)-J*J+3*J,2,IRR)
      CALL IDIV (2*(J-1)*(M+1)-J*J+3*J,2,IRR,LJ)
      INC = 0
      DO 30 K=L,M
        INC  = INC + 1
        JK   = LJ + INC
        D(K) = R(JK)
  30  CONTINUE
!
      I = M
      DO 50 KA=J,M
        DSUM = 0.0D0
        IF (I.EQ.J) DSUM = DBLE (R(LJ))
        DO 40 K=L,M
          JK    = MIN0 (K,I)
!CCCC     LL    = IDIV (2*(JK-1)*(M+1)-JK*JK+3*JK,2,IRR)
          CALL    IDIV (2*(JK-1)*(M+1)-JK*JK+3*JK,2,IRR,LL)
          INC   = IABS (K-I)
          JK    = LL + INC
          DSUM = DSUM -DBLE (D(K)) * DBLE (R(JK))
  40    CONTINUE
        INC = I - J
        JK = LJ + INC
!CCCC   R(JK) = DPCON (DSUM)
        CALL    DPCON (DSUM,R(JK))
        I = I - 1
  50  CONTINUE
      L = L - 1
      IF (L.GT.1) GO TO 20
!
!    C
!     PACK VECTOR R.
!
      DO 70 I=2,M
!CCCC   L =  IDIV (2*(I-1)*M-I*I+3*I,2,IRR)
        CALL IDIV (2*(I-1)*M-I*I+3*I,2,IRR,L)
        DO 60 J=I,M
          K = L + I - 1
          R(L) = R(K)
          L = L + 1
  60    CONTINUE
  70  CONTINUE
!
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE PINVRT 
!CCCC-----DPDIV--------------------------------------
      SUBROUTINE DPDIV(FN,FD,IND,DRESUL)
!
!     PURPOSE--PERFORM DOUBLE PRECISION DIVISION FN/FD,
!              IF THE DENOMINATOR EQUALS ZERO,
!              THE RESULT IS SET TO ZERO,
!              AND THE INDICATOR, IND, IS SET EQUAL TO ONE.
!              OTHERWISE, IND IS SET TO 0.
!     INPUT  ARGUMENTS--FN
!                     --FD
!     OUTPUT ARGUMENTS--IND
!                     --DRESUL
!     WRITTEN BY--ROY WAMPLER
!                 DAVE HOGBEN
!                 SALLY PEAVY
!     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--JUNE      1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION FN
      DOUBLE PRECISION FD
      DOUBLE PRECISION DRESUL
!
!-----DIMENSION-------------------------------------------------------
                                                                                                                                  
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IND = 0
      IF(FD.EQ.0.0D0)GO TO 1010
      DRESUL=FN/FD
      GO TO 9000
!
 1010 CONTINUE
      DRESUL=0.0D0
      IND=1
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPDIV
!CCCC-----SPDIV--------------------------------------
      SUBROUTINE SPDIV(FN,FD,IND,RESULT)
!
!     PURPOSE--PERFORM SINGLE PRECISION DIVISION FN/FD,
!              IF THE DENOMINATOR EQUALS ZERO,
!              THE RESULT IS SET TO ZERO,
!              AND THE INDICATOR, IND, IS SET EQUAL TO ONE.
!              OTHERWISE, IND IS SET TO 0.
!     INPUT  ARGUMENTS--FN
!                     --FD
!     OUTPUT ARGUMENTS--IND
!                     --RESULT
!     WRITTEN BY--ROY WAMPLER
!                 DAVE HOGBEN
!                 SALLY PEAVY
!     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--JUNE      1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----DIMENSION-------------------------------------------------------
                                                                                                                                  
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IND = 0
      IF(FD.EQ.0.0D0)GO TO 1010
      RESULT=FN/FD
      GO TO 9000
!
 1010 CONTINUE
      RESULT=0.0D0
      IND=1
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE SPDIV
!CCCC-----DPCON--------------------------------------
      SUBROUTINE DPCON(DX,RESULT)
!
!     PURPOSE--CONVERT DOUBLE PRECISION NUMBER
!              TO SINGLE PRECISION NUMBER BY OCTAL ROUNDING
!              INSTEAD OF TRUNCATION.
!     INPUT  ARGUMENTS--DX          (DOUBLE PRECISION)
!     OUTPUT ARGUMENTS--RESULT      (SINGLE PRECISION)
!               WRITTEN BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL BUREAU OF STANDARDS,
!                      WASHINGTON, DC 20234
!                          TELEPHONE 301-975-2855
!                  ORIGINAL VERSION -   AUGUST, 1969.
!                   CURRENT VERSION - NOVEMBER, 1978.
!     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--JUNE      1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL             Y
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DXX
      DOUBLE PRECISION  D
!
!-----DIMENSION-------------------------------------------------------
                                                                                                                                  
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMETNS-------------------------------------------------
!
      DATA RMIFY / -1.0E37 /
      DATA RPIFY /  1.0E38 /
!
!-----START POINT-----------------------------------------------------
!
      DXX = DX
      IF (DXX.GT.DBLE(RPIFY)) DXX = RPIFY
      IF (DXX.LT.DBLE(RMIFY)) DXX = RMIFY
!
      Y = DXX
      D = Y
      RESULT = DXX + (DXX-D)
!
      RETURN
      END SUBROUTINE DPCON
!CCCC-----DPSQRT--------------------------------------
      SUBROUTINE DPSQRT(DX,DRESUL)
!
!     PURPOSE--PERFORM DOUBLE PRECISION SQUARE ROOT OF DX,
!              IF THE DENOMINATOR IS LESS THAN 0,
!              THE OUTPUT RESULT IS SET TO 0,
!              AND AN ARITHMETIC FAULT MESSAGE IS PRINTED.
!     INPUT  ARGUMENTS--X
!     OUTPUT ARGUMENTS--DRESUL
!     WRITTEN BY--ROY WAMPLER
!                 DAVE HOGBEN
!                 SALLY PEAVY
!     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--JUNE      1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DRESUL
!
!-----DIMENSION-------------------------------------------------------
                                                                                                                                  
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(DX.LE.0.0D0)GO TO 1010
      DRESUL=DSQRT(DX)
      GO TO 9000
!
 1010 CONTINUE
      DRESUL=0.0D0
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPSQRT
!CCCC-----SPSQRT--------------------------------------
      SUBROUTINE SPSQRT(X,RESULT)
!
!     PURPOSE--PERFORM SINGLE PRECISION SQUARE ROOT OF X,
!              IF THE DENOMINATOR IS LESS THAN 0,
!              THE OUTPUT RESULT IS SET TO 0,
!              CALLS ERROR(101) IS DONE.
!     INPUT  ARGUMENTS--X
!     OUTPUT ARGUMENTS--RESULT
!     WRITTEN BY--ROY WAMPLER
!                 DAVE HOGBEN
!                 SALLY PEAVY
!     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--NOVEMBER  1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!
!-----DIMENSION-------------------------------------------------------
                                                                                                                                  
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(X.LE.0.0)GO TO 1010
      RESULT=SQRT(X)
      GO TO 9000
!
 1010 CONTINUE
      RESULT=0.0
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE SPSQRT
!CCCC-----SPLO10--------------------------------------
      SUBROUTINE SPLO10(X,RESULT)
!
!     PURPOSE--COMPUTER LOG TO BASE 10 OF X
!              USING LIBRARY FUNCTION OF X IS POSITIVE, OR
!              CALLS ERROR(101) AND SETS FUNCTION VALUE
!              EQUAL TO 0 IF X IS NONPOSITIVE.
!
!     INPUT  ARGUMENTS--X
!     OUTPUT ARGUMENTS--RESULT
!     WRITTEN BY--ROY WAMPLER
!                 DAVE HOGBEN
!                 SALLY PEAVY
!     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--JUNE      1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----DIMENSION-------------------------------------------------------
                                                                                                                                  
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(X.GT.0.0)GO TO 1020
      RESULT=0.0
      GO TO 9000
!
 1020 CONTINUE
      RESULT=LOG10(X)
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE SPLO10
!CCCC-----IDIV--------------------------------------
      SUBROUTINE IDIV(IN,ID,IND,IRESUL)
!
!     PURPOSE--THIS INTEGER FUNCTION PERFORMS THE DIVISION IN/ID, WHEN
!              THE NUMERATOR, IN, AND THE DENOMINATOR, ID, ARE INTEGERS.
!              IF ID = 0, THE FUNCTION VALUE IS SET EQUAL TO ZERO.
!
!     INPUT  ARGUMENTS--IN
!                     --ID
!     OUTPUT ARGUMENTS--IND
!                     --IRESUL
!     WRITTEN BY--ROY WAMPLER
!                 DAVE HOGBEN
!                 SALLY PEAVY
!     CONVERTED TO DATAPLOT BY--JAMES J. FILLIBEN (JUNE 1987)
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/7
!     ORIGINAL VERSION--JUNE      1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----DIMENSION-------------------------------------------------------
                                                                                                                                  
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IND = 0
      IF(ID.EQ.0)GO TO 1010
      IRESUL=IN/ID
      GO TO 9000
!
 1010 CONTINUE
      IRESUL=0
      IND=1
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE IDIV
!BACK
      SUBROUTINE BACK (NC,LB,L,K,MV,RS,A,I,JC,ID,XI,MD,II,NI,ND,KZ,NL,N)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   BACK V 7.00  2/14/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!                         LOOK BACK COMPUTATION OF RSS
!
!     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR
!                   REGRESSIONS BY LEAPS AND BOUNDS
!          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS
!                     G.M.FURNIVAL AND R.W.WILSON
!               YALE UNIVERSITY AND U.S. FOREST SERVICE
!                           VERSION 11/11/74
!
!               ADAPTED TO OMNITAB BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-975-2845
!                  ORIGINAL VERSION - FEBRUARY, 1977.
!                   CURRENT VERSION - FEBRUARY, 1990.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      DIMENSION I(ND,ND), ID(ND), K(ND), NC(ND,ND), NI(ND), MD(ND,ND)
!
      REAL             XI(NL)
      REAL             A, RS
      REAL             B
      REAL             FDIV
!
      DATA ITHRE  /3/
      DATA IONE   /1/
      DATA IZERO  /0/
!
!     ==================================================================
!
!                               FIND SOURCE MATRIX.
!
  10  ISUB1 = K(JC)
      IF (LB.LE.NI(ISUB1)) GO TO 20
      JC = JC - IONE
      GO TO 10
!
!                            ADJUST FOR PREVIOUS PIVOTS.
!
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
!
!                                 CURRENT PIVOT.
!
  60  RS = A - FDIV (XI(ISUB2)*XI(ISUB2),XI(ISUB3),IND)
      RETURN
!
!     ================================================================
!
      END SUBROUTINE BACK 
!CODEXY
      SUBROUTINE CODEXY (X,N,SUMX,AVEX,XCODE,SQRTCT,U,L)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CODEXY V 7.00  2/14/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     PROCEDURE FOR CODING X FOR ACCURATELY COMPUTING
!        SUM OF SQUARED DEVIATIONS FROM THE MEAN.
!
!     INPUT PARAMETERS ARE -
!
!            X = VECTOR OF MEASUREMENTS
!            N = LENGTH OF X
!
!     OUPUT PARAMETERS ARE -
!
!         SUMX = DOUBLE PRECISION SUM OF X MEASUREMENTS
!         AVEX = SINGLE PRECISION AVERAGE OF THE X MEASUREMENTS
!        XCODE = CODED VALUE TO BE USED INSTEAD OF AVERAGE FOR
!                   CUMPUTING DEVIATIONS ABOUT THE MEAN.
!                   XCODE IS THE VALUE OF X(I) CLOSEST TO AVEX.
!       SQRTCT = SQUARE ROOT OF CORRECTION TERM FOR COMPUTING
!                   SUM OF SQUARED DEVIATIONS ABOUT THE MEAN.
!
!                   SUM (X-AVEX)**2 = SUM(X-CODEX)**2 - SQRTCT**2,
!
!                   WHERE SQRTCT = (SUMX-N*XCODE)/SQRT(N)
!
!         U(I) = X(I) -XCODE, = CODED VALUES OF X
!            L = VALUE OF I FOR WHICH XCODE = X(I).
!
!               WRITTEN BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-975-2845
!                  ORIGINAL VERSION - FEBRUARY, 1977.
!                   CURRENT VERSION - FEBRUARY, 1990.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      REAL             X(*), U(*)
      REAL             AVEX, DELTA, XCODE
      REAL             FDPCON
!
!CCCC DOUBLE PRECISION DZERO
      DOUBLE PRECISION DN, SQRTCT, SUMX
      DOUBLE PRECISION FDDIV, FDSQRT
      DOUBLE PRECISION DX(1)
      DOUBLE PRECISION SNEG
      DOUBLE PRECISION SPOS
!
!CCCC DATA DZERO  /0.0D0/
      DATA IONE   /1/
      DATA IZERO  /0/
!     ==================================================================
!
      SNEG=0.0D0
      SPOS=0.0D0
!     COMPUTE AVEX.
!
!CCCC CALL DSUMAL (DX,IZERO,SUMX)
      CALL DSUMAL (DX,IZERO,SNEG,SPOS,SUMX)
      DO 10 I=1,N
        DX(1) = DBLE ( X(I) )
!CCCC   CALL DSUMAL (DX,-IONE,SUMX)
        CALL DSUMAL (DX,-IONE,SNEG,SPOS,SUMX)
  10  CONTINUE
!CCCC CALL DSUMAL (DX,IONE,SUMX)
      CALL DSUMAL (DX,IONE,SNEG,SPOS,SUMX)
!
      DN = N
!
      AVEX = FDPCON ( FDDIV (SUMX,DN,IND) )
!
!     COMPUTE XCODE AND L.
!
      L = IONE
      DELTA = ABS (X(1)-AVEX)
      DO 30 I=2,N
!CCCC   IF (ABS(X(I)-AVEX)-DELTA) 20,30,30
        IF (ABS(X(I)-AVEX)-DELTA.LT.0.0) THEN
           L = I
           DELTA = ABS (X(I)-AVEX)
        ENDIF
   30 CONTINUE
!
      XCODE = X(L)
!
!     COMPUTE CODED X = (X-XCODE).
!
      DO 40 I=1,N
        U(I) = X(I) - XCODE
  40  CONTINUE
!
!     COMPUTE CORRECTION TERM
!        FOR COMPUTING SUMX OF DEVIATIONS ABOUT THE MEAN.
!
      SQRTCT = FDDIV (SUMX-DN*DBLE(XCODE),FDSQRT(DN),IND)
!
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE CODEXY 
!COEF
      SUBROUTINE COEF (R2,BIC,MP,KZ,XI,RR,MAXC,IND,NDEF,M,   &
                       ND,MD,NL,IB,ZC,   &
                       AMAT,IVALUE,NCVALU,MAXROW,NUMCLI,ITITL9,NCTIT9)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   COEF V 7.00  8/27/91. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!                     COMPUTES REGRESSION STATISTICS
!
! ******************************************************************** *
!                                                                      *
!     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR     *
!                   REGRESSIONS BY LEAPS AND BOUNDS                    *
!          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
!                     G.M.FURNIVAL AND R.W.WILSON                      *
!               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
!                           VERSION 11/11/74                           *
!                                                                      *
! ******************************************************************** *
!
!               MODIFIED TO PFORT BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-975-2845
!                  ORIGINAL VERSION - SEPTEMBER, 1976.
!                   CURRENT VERSION -    AUGUST, 1991.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!CCCC DIMENSION IND(ND), MD(ND,ND), NALPHA(15), NOUT(12)
!CCCC DIMENSION IND(ND), MD(ND,ND), NOUT(12)
      DIMENSION IND(ND), MD(ND,ND)
!
!     ==================================================================
!
!                         ***   TYPE STATEMENTS   ***
!
!CCCC REAL             RR(29,29), XI(NL), ZC(ND)
      REAL             RR(MAXC,MAXC), XI(NL), ZC(ND)
      REAL             DBET, F, R2, VAR
      REAL             FDIV
!
      REAL AMAT(MAXROW,NUMCLI)
      INTEGER NCVALU(MAXROW,NUMCLI)
      CHARACTER*8 IVALUE(MAXROW,NUMCLI)
      CHARACTER*(*) ITITL9
!
!     ..................................................................
!
!CCCC CHARACTER NALPHA*1, NOUT*1
!CCCC CHARACTER NOUT*1
!
      PARAMETER (MAXV=98)
      CHARACTER*1 ICOD(MAXV)
      CHARACTER*38 IOUT
      CHARACTER*8 IVLIST
      COMMON/BESTC1/IOUNI1,IOUNI2
      COMMON/BESTC2/IVLIST(MAXV)
!
      INCLUDE 'DPCOP2.INC'
!
!     ==================================================================
!
!                 ***   DATA INITIALIZATION STATEMENTS   ***
!
!CCCC DATA NOUT( 1), NOUT( 2), NOUT( 3), NOUT( 4), NOUT( 5), NOUT( 6) /
!CCCC1          'R',      '*',      '*',      '2',      'R',      '*' /
!CCCC DATA NOUT( 7), NOUT( 8), NOUT( 9), NOUT(10), NOUT(11), NOUT(12) /
!CCCC1          '*',      '2',      'C',      '(',      'P',      ')' /
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
!
!     IF THE FOLLOWING VALUE IS CHANGED,
!        THE DIMENSION OF NALPHA MUST BE CHANGED AND
!        15A1 MUST BE CHANGED IN FORMAT 70.
!
!CCCC DATA NX / 15 /
!
      DATA IFOUR  /4/
      DATA ITHRE  /3/
!
!CCCC NOTE: ISIGD = 7 CAUSES PROBLEMS ON MICROSOFT COMPILER, SGI
!CCCC       COMPILER.  JUST SET TO 6 TO BE SAFE.
!CCCC DATA ISIGD  /7/
!CCCC DATA ISIGD  /6/
!
!     ==================================================================
!
      IEND = IFOUR * IB
      IBEG = IEND - ITHRE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,60) (NOUT(I),I=IBEG,IEND), R2
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,61)
!CCCC CALL DPWRST('XXX','BUG ')
!C60  FORMAT(19X,4A1,' = ',F7.3)
!C61  FORMAT(4X,'VARIABLE',9X,'COEFFICIENT',7X,'F RATIO')
!
      ITITL9='C(p) = '
      WRITE(ITITL9(8:19),'(F12.3)')R2
      ITITL9(20:27)=', BIC = '
      WRITE(ITITL9(28:39),'(F12.3)')BIC
      NCTIT9=39
!
!                             FORM SUBMATRIX
!
      IND(MP) = KZ
      DO 20 I=1,MP
        DO 10 J=I,MP
          ISUB1 = MD(I,J)
          ISUB2 = IND(I)
          ISUB3 = IND(J)
          XI(ISUB1) = RR(ISUB2,ISUB3)
  10    CONTINUE
  20  CONTINUE
!
!                            INVERT SUBMATRIX
!
      DO 30 N=1,M
        NN = N
        CALL PIVOT (XI,MP,NN,MD,ND,NL)
  30  CONTINUE
!
      ISUB4 = MD(MP,MP)
      VAR = FDIV (XI(ISUB4),FLOAT(NDEF-M),IF)
!
      DO 40 I=1,M
        ISUB5 = MD(I,MP)
        ZC(I) = -XI(ISUB5)
 40   CONTINUE
!
!CCCC NOTE: HAD PROBLEMS WITH RFORMT ON SOME PLATFORMS (MICROSOFT
!CCCC FORTRAN, SGI), SO JUST USE E FORMAT FOR NOW.
!CCCC CALL RFORMT (0,ISIGD,ZC,XI(1), M,NX,LW,LD,NALPHA(1),IRF)
!CCCC LB = NX - LW
!
      DO 50 I=1,M
        DBET = ZC(I)
        ISUB6 = MD(I,I)
!CCCC   CALL RFORMT (1,ISIGD,XI,ZC(I),LB, 1,LW,LD,NALPHA(1),IRF)
        F = -DBET*FDIV (DBET,XI(ISUB6)*VAR,IF)
!CCCC   WRITE(ICOUT,70) IND(I), (NALPHA(J),J=1,NX), F
!CCCC   WRITE(ICOUT,70) IVLIST(IND(I)), ZC(I), F
!CCCC   CALL DPWRST('XXX','BUG ')
        IVALUE(I,1)=IVLIST(IND(I))
        NCVALU(I,1)=8
        AMAT(I,2)=ZC(I)
        AMAT(I,3)=F
  50  CONTINUE
!C70  FORMAT (10X,I2,7X,15A1,5X,F7.3)
!C70  FORMAT (4X,A8,7X,E15.7,5X,F7.3)
!
      WRITE(IOUNI1,71)M,R2,BIC,(IVLIST(IND(J)),J=1,M)
  71  FORMAT(I3,1X,2F15.3,' :',38(1X,A8))
!
      IOUT=' '
      DO 80 I=1,M
        IOUT(I:I)=ICOD(IND(I))
  80  CONTINUE
      WRITE(IOUNI2,'(38A1)')(IOUT(I:I),I=1,M)
!999  FORMAT(1X)
!
      RETURN
      END SUBROUTINE COEF 
!CPSTRE
      SUBROUTINE CPSTRE (RSS,CAB,KO,CL,RM,N,NS,ND)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CPSTRE V 7.00  2/14/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!                  SAVES RSS:S AND LABELS FOR BEST REGRESSIONS
! ******************************************************************** *
!                                                                      *
!     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR     *
!                   REGRESSIONS BY LEAPS AND BOUNDS                    *
!          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
!                     G.M.FURNIVAL AND R.W.WILSON                      *
!               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
!                           VERSION 11/11/74                           *
!                                                                      *
! ******************************************************************** *
!
!               MODIFIED TO PFORT BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-975-2845
!                  ORIGINAL VERSION - FEBRUARY, 1977.
!                   CURRENT VERSION - FEBRUARY, 1990.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      REAL             CL(11,ND), RM(11,ND)
      REAL             CAB, RSS
!
      DATA IONE   /1/
      DATA IZERO  /0/
!
!     ==================================================================
!
      DO 10 L=1,KO
        IF (CAB.EQ.CL(L,N)) RETURN
  10  CONTINUE
!
      L = IZERO
  20  L = L + IONE
        IF (RSS.GT.RM(L+1,N)) GO TO 30
        RM(L,N) = RM(L+1,N)
        CL(L,N) = CL(L+1,N)
        IF (L.EQ.NS) GO TO 30
      GO TO 20
!
  30  RM(L,N) = RSS
      CL(L,N) = CAB
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE CPSTRE 
!CRSPRD
      SUBROUTINE CRSPRD (X,N,M,INTCPT,CTERM,CP,MAXC)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. CRSPRD V 7.00  2/14/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     PROGRAM UNIT FOR COMPUTING A CROSS PRODUCT OF DEVIATIONS ABOUT
!        MEAN MATRIX, CP().
!
!        INPUT X(N,M)
!              N = NUMBER OF MEASUREMENTS
!              M = NUMBER OF VARIABLES.
!         INTCPT = 0, CROSS PRODUCTS ABOUT ORIGIN ARE COMPUTED
!                = 1, CROSS PRODUCTS ABOUT MEAN   ARE COMPUTED.
!
!        STORAGE CONST(M).
!
!        OUTPUT CP(M,M) = CROSS PRODUCT MATRIX.
!
!               WRITTEN BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-975-2845
!                  ORIGINAL VERSION - FEBRUARY, 1977.
!                   CURRENT VERSION - FEBRUARY, 1990.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      REAL             X(N,*)
!CCCC REAL             CP(29,29)
      REAL             CP(MAXC,MAXC)
      REAL             AVEX, XCODE
      REAL             FDPCON
!
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION CTERM(*)
      DOUBLE PRECISION F, SUMNEG, SUMPOS, SUMX
!
!     ==================================================================
!
      DATA IONE   /1/
      DATA DZERO  /0.0D0/
!
!     BEGIN COMPUTING.
!
!     COMPUTE CORRECTION TERM, CTERM(I), AND CODE X(I,J).
!
      IF (INTCPT.EQ.IONE) GO TO 20
      DO 10 I= 1,M
        CTERM(I) = DZERO
  10  CONTINUE
      GO TO 40
!
  20  DO 30 I=1,M
        CALL CODEXY (X(1,I),N,SUMX,AVEX,XCODE,CTERM(I),X(1,I),L)
  30  CONTINUE
!
!     COMPUTE (N-1)*VARIANCES.
!
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
!
!     COMPUTE CROSS PRODUCT MATRIX.
!
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
!
      RETURN
!
!     ==================================================================
!
      END SUBROUTINE CRSPRD 
!FDDIV
      DOUBLE PRECISION FUNCTION FDDIV (FN,FD,IND)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  FDDIV V 7.00  2/21/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     THIS FUNCTION PERFORMS DOUBLE PRECISION DIVISION.
!
!     IF THE DENOMINATOR EQUALS ZERO, THE RESULT IS SET EQUAL TO ZERO
!        AND THE INDICATOR, IND, IS SET EQUAL TO ONE.  OTHERWISE
!           IND EQUALS ZERO.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION FN, FD
!
!     ==================================================================
!
      DATA IZERO  /0/
      DATA IONE   /1/
      DATA DZERO  /0.0D0/
!
      IND = IZERO
      IF(FD-DZERO.EQ.0.0D0)THEN
        FDDIV = DZERO
        IND = IONE
      ELSE
        FDDIV = FN/FD
      ENDIF
      RETURN
!
!     ==================================================================
!
      END FUNCTION FDDIV 
!FDIV
      REAL FUNCTION FDIV (FN,FD,IND)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.   FDIV V 7.00  2/21/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     PROGRAM UNIT ...
!        DIVIDES FN BY FD USING FORTRAN OPERATOR /,
!           IF X IS NOT EQUAL TO ZERO, OR
!        SETS FAULT INDICATOR EQUAL TO ONE,
!           IF X IS EQUAL TO ZERO.
!
!     FAULT INDICATOR, IND = 0, IF FN IS NOT EQUAL TO ZERO, AND
!                          = 1, IF FN IS     EQUAL TO ZERO.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
!
      REAL             FN, FD
!
!     ==================================================================
!
      DATA IONE   /1/
      DATA IZERO  /0/
      DATA RZERO  /0.0/
!
      IND = IZERO
      IF (FD.EQ.RZERO) GO TO 10
      FDIV = FN / FD
      RETURN
!
!     ..................................................................
!
  10  FDIV = RZERO
      IND = IONE
      RETURN
!
!     ==================================================================
!
      END FUNCTION FDIV 
!FDPCON
      REAL FUNCTION FDPCON (X)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FDPCON V 7.00  2/21/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     FUNCTION TO CONVERT DOUBLE PRECISION NUMBER TO REAL NUMBER BY
!        OCTAL ROUNDING INSTEAD OF TRUNCATION.
!
!               WRITTEN BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-975-2845
!                  ORIGINAL VERSION -   AUGUST, 1969.
!                   CURRENT VERSION - FEBRUARY, 1990.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      REAL             Y
!
      DOUBLE PRECISION X
      DOUBLE PRECISION XX, D
!
!     ==================================================================
!
      DATA RPIFY /1.0E38/
      DATA RMIFY /-1.0E37/
!
      XX = X
      IF (XX.GT.DBLE(RPIFY)) XX = RPIFY
      IF (XX.LT.DBLE(RMIFY)) XX = RMIFY
!
      Y = XX
      D = Y
      FDPCON = XX + (XX-D)
!
      RETURN
!
!     ==================================================================
!
      END FUNCTION FDPCON 
!FDSQRT
      DOUBLE PRECISION FUNCTION FDSQRT (X)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FDSQRT V 7.00  2/21/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     THIS FUNCTION COMPUTES THE DOUBLE PRECISION SQUARE ROOT OF X.
!
!     IF THE ARGUMENT, X, IS LESS THAN ZERO, THE FUNCTION VALUE IS SET
!        EQUAL TO ZERO AND AN ARITHMETIC FAULT MESSAGE IS PRINTED.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      DOUBLE PRECISION DZERO
      DOUBLE PRECISION X, DSQRT
!
      INCLUDE 'DPCOP2.INC'
!
      DATA DZERO /0.0D0/
!
!     ==================================================================
!
!CCCC IF (X-DZERO) 20,30,10
      FDSQRT = DZERO
      IF (X-DZERO.LT.0.0D0)THEN
!CCCC    CALL ERROR (101)
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)
         CALL DPWRST('XXX','BUG ')
      ELSEIF (X-DZERO.GT.0.0D0)THEN
         FDSQRT = DSQRT (X)
      ENDIF
  101 FORMAT('***** ERROR FROM FDSQRT: ATTEMPT TO TAKE SQUARE ROOT OF ',   &
             'NEGATIVE NUMBER.')
!
      RETURN
!
!     ==================================================================
!
      END FUNCTION FDSQRT 
!FLOG10
      REAL FUNCTION FLOG10 (X)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. FLOG10 V 7.00  2/21/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     PROGRAM UNIT ...
!        COMPUTES LOG TO BASE 10 OF X USING LIBRARY FUNCTION LOG10,
!           IF X IS POSITIVE, OR
!        CALLS ERROR (101) AND SETS FUNCTION VALUE EQUAL TO ZERO,
!           IF X IS NONPOSITIVE.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      REAL             X
!
      INCLUDE 'DPCOP2.INC'
!
      DATA RZERO  /0.0/
!     ==================================================================
!
      IF (X.GT.RZERO) THEN
         FLOG10 = LOG10 (X)
      ELSE
!CCCC    CALL ERROR (101)
         WRITE(ICOUT,51)
   51    FORMAT('***** ERROR FROM FLOG10: ATTEMPT TO TAKE THE LOG OF ',   &
                'A NON-POSITIVE NUMBER')
         CALL DPWRST('XXX','BUG ')
         FLOG10 = RZERO
      ENDIF
!
!     ..................................................................
!
      RETURN
!
!     ==================================================================
!
      END FUNCTION FLOG10 
!PIVOT
      SUBROUTINE PIVOT (XI,KP,N,MD,ND,NL)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81.  PIVOT V 7.00  2/21/90. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!              SYMETRIC PIVOT-RETURNS NEGATIVE INVERSE
!     ONE OF FOUR SUBROUTINES CALLED BY MAIN SUBROUTINE SCREEN FOR
!                   REGRESSIONS BY LEAPS AND BOUNDS
!          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS
!                     G.M.FURNIVAL AND R.W.WILSON
!               YALE UNIVERSITY AND U.S. FOREST SERVICE
!                           VERSION 11/11/74
!
!               MODIFIED TO PFORT BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-975-2845
!                  ORIGINAL VERSION - SEPTEMBER, 1976.
!                   CURRENT VERSION -  FEBRUARY, 1990.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      DIMENSION MD(ND,ND)
!
      REAL             XI(NL)
      REAL             B
      REAL             FDIV
!
      DATA RONE /1.0/
!
!     ==================================================================
!
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
!
!     ==================================================================
!
      END SUBROUTINE PIVOT 
!RFORMT
      SUBROUTINE RFORMT (KTYPE,KDIGIT,X,XVALUE,K1,K2,KW,KD,NALPHA,KE)
!
! **  NBS OMNITAB 1980 VERSION 6.01  2/25/81. RFORMT V 7.00  2/19/91. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!                            *** DESCRIPTION ***
!
!     RFORMT IS A GENERAL-PURPOSE PORTABLE FORTRAN SUBROUTINE FOR USE IN
!        PRINTING REAL NUMBERS.
!
!     IT IS PRIMARILY INTENDED FOR PREPARING REAL NUMBERS TO BE PRINTED
!        IN READABLE FORM, I.E., WITH A CONSTANT NUMBER OF SIGNIFICANT
!        DIGITS AND THE DECIMAL POINT IN A CONSTANT POSITION.  THIS IS
!        IS CALLED R FORMAT.  IT CAN ALSO BE USED TO PRINT REAL NUMBERS
!        IN E, F, OR I FORMATS.
!
!     TO USE THE R FORMAT, IT IS NORMALLY NECESSARY TO USE RFORMT IN TWO
!        STAGES.  IN THE FIRST STAGE, WITH ITYPE = 0, NWIDTH AND NDECS
!        ARE CALCULATED.  IN THE SECOND STAGE, NWIDTH AND NDECS ARE USED
!        TO OBTAIN THE HOLLERITH CHARACTER STRING IN THE VECTOR NALPHA.
!
!     IN STAGE 2, REAL NUMBERS ARE CONVERTED INTO A HOLLERITH STRING AND
!        STORED IN THE VECTOR NALPHA FOR PRINTING WITH AN NA1 FORMAT.
!        THE HOLLERITH STRING IS PACKED ONE CHARACTER PER WORD.
!
!     ..................................................................
!
!                       *** STAGE 1 ARGUMENTS ***
!                       COMPUTE NWIDTH AND NDECS
!
!     INPUT ARGUMENTS -
!
!        (1)    ITYPE = 0
!        (2)   NDIGIT = NUMBER OF SIGNIFICANT DIGITS TO BE USED
!        (3)        X = VECTOR OF REAL NUMBERS DIMENSIONED AT LEAST N1
!                          IN CALLING PROGRAM UNIT
!        (4)   XVALUE = DUMMY ARGUMENT
!        (5)       N1 = LENGTH OF VECTOR X
!        (6)       N2 = MAXIMUM VALUE OF NWIDTH ALLOWED
!
!     OUTPUT ARGUMENTS -
!
!        (7)   NWIDTH = WIDTH OF FIELD NEEDED TO PRINT EVERY REAL NUMBER
!                          IN X IN R FORMAT
!        (8)    NDECS = NUMBER OF PLACES AFTER THE DECIMAL POINT NEEDED
!                          TO PRINT NUMBERS IN X IN R FORMAT
!        (9)   NALPHA = DUMMY ARRAY ARGUMENT, WHICH MUST BE
!                                 DIMENSIONED IN CALLING PROGRAM UNIT
!       (10)   IFAULT = FAULT INDICATOR,
!                     = 0, IF EVERYTHING IS OK
!                     = 1, IF ITYPE IS NEGATIVE
!                     = 2, IF VALUE OF NDIGIT INVALID
!                     = 3, IF N1 IS NON-POSITIVE
!                     = 4, IF N2 IS LESS THAN NDIGIT+2
!                     = 5, IF CALCULATED VALUE OF NWIDTH EXCEEDS N2.
!                             NWIDTH IS RESET TO N2.
!                     = 6, IF CALCULATED NWIDTH EXCEEDS N2 AND NDIGIT+5
!                             EXCEEDS N2
!
!     ..................................................................
!
!                         *** STAGE 2 ARGUMENTS ***
!                      PUT HOLLERITH STRING IN NALPHA
!
!     INPUT ARGUMENTS -
!
!        (1)    ITYPE = TYPE OF FORMAT DESIRED,
!                     =  1, R FORMAT, NUMBER ZERO HAS BLANKS AFTER DEC.
!                             POINT, 1PEW.(D-1) FORMAT USED IF NECESSARY
!                     =  2, R FORMAT, ZERO CONVERTED NORMALLY
!                             1PEW.(D-1) FORMAT USED IF NECESSARY
!                     =  3, R FORMAT, ZERO HAS BLANKS AFTER DEC. POINT,
!                             0PEW.D FORMAT USED IF NECESSARY
!                     =  4, R FORMAT, ZEROS CONVERTED NORMALLY
!                             0PEW.D JORMAT USED IF NECESSARY
!                     =  5, 1PEW.D FORMAT
!                     =  6, 0PEW.D FORMAT
!                     =  7, FW.D FORMAT, WITH ROUNDING
!                     =  8, FW.D FORMAT, WITH TRUNCATION
!                     =  9, IW FORMAT, WITH ROUNDING
!                     = 10, IW FORMAT, WITH TRUNCATION
!                     = 11, NWIDTH+N1 BLANKS STORED IN NALPHA
!        (2)   NDIGIT = NUMBER OF SIGNIFICANT DIGITS USED
!        (3)        X = DUMMY ARRAY ARGUMENT, WHICH MUST BE
!                           DIMENSIONED IN CALLING PROGRAM UNIT
!        (4)   XVALUE = REAL NUMBER TO BE CONVERTED
!        (5)       N1 = NUMBER OF BLANKS ADDED TO FIELD IN NALPHA
!        (6)       N2 = 0, NA BLANKS INSERTED ON LEFT (BEGINNING)
!                     = 1, N1 BLANKS ARE CENTERED
!        (7)   NWIDTH = LENGTH OF FIELD (HOLLERITH STRING) EXCLUDING N2
!                          BLANKS
!        (8)    NDECS = NUMBER OF PLACES AFTER THE DECIMAL POINT
!
!     OUTPUT ARGUMENTS -
!
!        (9)   NALPHA = HOLLERITH STRING REPRESENTATION OF XVALUE,
!                          OF LENGTH NWIDTH+N1
!       (10)   IFAULT = FAULT INDICATOR,
!                     =  0, IF EVERYTHING IS OK
!                     =  1, IF VALUE OF ITYPE IS NOT VALID
!                     =  2, IF VALUE OF NDIGIT IS NOT VALID
!                     =  3, IF N1 IS NON-POSITIVE
!                     =  7, IF VALUE OF N2 IS NOT ZERO OR ONE
!                     =  8, IF VALUE OF NWIDTH IS NOT VALID
!                     =  9, IF VALUE OF NDECS IS NOT VALID
!                     = 10, IF OVERFLOW OCCURS WITH F OR I FORMATS
!                     = 11, IF R FORMAT FORCED INTO E FORMAT
!                     = 12, IF R FORMAT REQUIRES E FORMAT AND
!                              NWIDTH IS TOO SMALL
!                     = 13, IF R FORMAT REQUIRES E FORMAT AND
!                              NDECS IS TOO SMALL
!                     = 14, IF ITYPE EQUALS 9 OR 10 AND NDECS DOES NOT
!                              EQUAL ZERO. ZERO IS USED FOR IDECS.
!
!     ..................................................................
!
!                           *** NOTES ***
!
!      1.   CAUTION.  IN STAGE 1 ITYPE MUST EQUAL ZERO OR RFORMT WILL
!              EXECUTE STAGE 2.
!      2.   IFAULT = 5, 10, 11 OR 14, INDICATES INFORMATIVE DIAGNOSTIC.
!              OTHERWISE NON-ZERO VALUES OF IFAULT INDICATE FATAL ERRORS
!              AND EXIT OCCURS WITHOUT ANY FURTHER CALCULATIONS OR ERROR
!              CHECKING.
!      3.   NDIGIT MUST BE GREATER THAN ZERO AND LESS THAN OR EQUAL TO
!              NSIGD.  SEE SECTION ON PORTABILITY BELOW FOR DEFINITION
!              OF NSIGD.
!      4.   X AND NALPHA MUST BE DIMENSIONED IN CALLING PROGRAM UNIT.
!      5.   RFORMT HANDLES REAL NUMBERS BETWEEN 10**(-100) AND 10**100,
!              EXCLUSIVELY.
!      6.   WHEN N2 = 1 IN STAGE 2, LARGEST NUMBER OF BLANKS IS ON RIGHT
!              IF N1 IS ODD.
!      7.   IN STAGE 1, NWIDTH INCLUDES POSITION FOR SIGN, EVEN
!              IF ALL NUMBERS ARE POSITIVE.  HOWEVER THERE ARE TWO
!              SPECIAL CASES ...
!                 (A) WHEN ALL X(I) = 0, IN WHICH CASE NWIDTH = 2
!                        AND NDECS = 0.
!                 (B) WHEN ALL X(I) ARE LESS THAN ONE IN ABSOLUTE VALUE
!                        AND AT LEAST ONE X(I) EQUALS ZERO. A POSITION
!                        FOR THE SIGN OF ZERO IS NOT INCLUDED IN NWIDTH.
!
!      8.   WITH R FORMAT, A DECIMAL POINT IS NOT STORED IN NALPHA IF
!              THE REAL NUMBER XVALUE EXCEEDS 10**NDIGIT.  IF NDIGIT=3,
!              1.23+03 IS STORED AS 1230 RATHER THAN 1230., TO EMPHASIZE
!              THAT THE ZERO IS NOT A SIGNIFICANT DIGIT.
!      9.   RFORMT DOES NO PRINTING.  PRINTING OF NALPHA WITH NA1 FORMAT
!              MUST BE DONE BY THE CALLING PROGRAM UNIT.
!     10.   WHEN ZERO IS PRINTED WITH R FORMAT, NDECS OVERRIDES NDIGIT.
!     11.   CAUTION.  IF IFAULT IS NOT EQUAL TO ZERO, NALPHA MAY NOT BE
!              BLANKED OUT.
!     12.   NALPHA IS UNCHANGED, IF ITYPE EQUALS ZERO.
!
!     ..................................................................
!
!                     *** USE OF E, F, AND I FORMATS ***
!
!     1.   1PEW.D FORMAT IS OBTAINED BY SETTING -
!              ITYPE =   5
!             NWIDTH =   W   = WIDTH OF FIELD
!             NDIGIT = (D+1) = NUMBER OF DIGITS
!
!          WITH D=6, 12.345678 IS WRITTEN AS 1.234568+01
!
!     2.   0PEW.D FORMAT IS OBTAINED BY SETTING -
!              ITYPE = 6
!             NWIDTH = W = WIDTH OF FIELD
!             NDIGIT = D = NUMBER OF DIGITS
!
!          WITH D=7, 12.345678 IS WRITTEN AS .1234568+02
!
!     3.   FW.D FORMAT IS OBTAINED BY SETTING -
!              ITYPE = 7 OR 8
!             NWIDTH = W = WIDTH OF FIELD
!              NDECS = D = NUMBER OF PLACES AFTER DECIMAL POINT
!
!     4.   IW FORMAT IS OBTAINED BY SETTING -
!              ITYPE = 9 OR 10
!             NWIDTH = W = WIDTH OF FIELD
!              NDECS = 0
!
!     NOTES -
!        A.   FOR E FORMAT, NDECS MUST BE GREATER THAN OR EQUAL TO ZERO.
!                NSIGDS=NDECS IS SET EQUAL TO NDIGIT+2 BY RFORMT.
!        B.   WITH EW.D FORMAT, THE LETTER E IS NOT USED AFTER THE
!                NUMBER AND BEFORE THE SIGNED CHARACTERISTIC.
!        C.   WITH 0PEW.D FORMAT, ZERO IS NOT PUT BEFORE THE DECIMAL
!                POINT.
!        D.   WITH FW.D FORMAT AND THE ABSOLUTE VALUE OF NUMBER IS LESS
!                THAN ONE, ZERO IS NOT PUT ON LEFT OF DECIMAL POINT,
!                UNLESS D = 0.
!
!     ..................................................................
!
!                            *** PORTABILITY ***
!
!     RFORMT IS COMPLETELY PORTABLE EXCEPT FOR ONE MACHINE DEPENDENT
!        CONSTANT, NSIGD, SET IN THE DATA STATEMENT ON LINE RF 320.
!
!     NSIGD IS THE NUMBER OF SIGNIFICANT DECIMAL DIGITS IN THE COMPUTER.
!        NSIGD =  7, FOR A 32 BIT WORD COMPUTER (IBM)
!              =  8, FOR A 36 BIT WORD COMPUTER (UNIVAC), VALUE SET
!              = 10, FOR A 48 BIT WORD COMPUTER (BURROUGHS)
!              = 13, FOR A 60 BIT WORD COMPUTER (CDC).
!
!     CAUTION.  NSIGD MUST BE SMALL ENOUGH SO THAT 10**(NSIGD+1) IS A
!        VALID MACHINE INTEGER.  (THIS EXPLAINS WHY NSIGD EQUALS 13 AND
!        NOT 14 FOR A 60 BIT WORD COMPUTER.)
!
!     SOURCE LANGUAGE IS PFORT (A PORTABLE SUBSET OF ANS FORTRAN).
!
!     FORTRAN LIBRARY FUNCTION USED IS LOG10,
!        WHICH APPEARS ON LINES RF 389, RF 391, AND RF 612.
!
!     STORAGE USED IS 1495 36 BIT WORDS WITH UNIVAC 1108 EXEC 8 COMPUTER
!
!     ..................................................................
!
!                           *** STATIC PROFILE ***
!
!     I/O STATEMENTS                 0
!     NONEXECUTABLE STATEMENTS      20
!     EXECUTABLE STATEMENTS        244
!        UNCONDITIONAL 160
!          CONDITIONAL  84
!     COMMENT STATEMENTS           532
!     --------------------------------
!     TOTAL NUMBER OF STATEMENTS   796
!     --------------------------------
!     CONTINUATION LINES             6
!     --------------------------------
!     NUMBER OF LINES OF CODE      802
!
!     ..................................................................
!
!                             *** REFERENCE ***
!
!     HOGBEN, DAVID (1977).  A FLEXIBLE PORTABLE FORTRAN PROGRAM UNIT
!        FOR READABLE PRINTING OF REAL NUMBERS.  IN PREPARATION.
!
!     ..................................................................
!
!               WRITTEN BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-975-2845
!                  ORIGINAL VERSION -    APRIL, 1969.
!                   CURRENT VERSION - FEBRUARY, 1991.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      DIMENSION NALPHA(*)
!
!     ==================================================================
!
!                    ***   TYPE STATEMENTS   ***
!
      REAL             X(*)
      REAL             XVALUE
      REAL             ABSMAX, ABSMIN, ABSX, ABSXVA, X1, X2
      REAL             FLOG10
!
!......................................................................
!
      DOUBLE PRECISION Z, ZLOWER, ZUPPER
      DOUBLE PRECISION DFIVE, DTEN
      DOUBLE PRECISION FDDIV
!
!     ..................................................................
!
      CHARACTER*1 LA(74)
      CHARACTER NALPHA*1
!
!CCCC INCLUDE 'DPCOHO.INC'
!
!     ==================================================================
!
!                 ***   DATA INITIALIZATION STATEMENTS   ***
!
      DATA DFIVE, DTEN / 5.0D0, 10.0D0 /
!
      DATA ITEN   /10/
      DATA IFIVE  /5/
      DATA IFOUR  /4/
      DATA ITHRE  /3/
      DATA ITWO   /2/
      DATA IONE   /1/
      DATA IZERO  /0/
!
      DATA RHALF   /0.5/
      DATA RONE    /1.0/
      DATA RZERO   /0.0/
!
!CCCC DATA ISIGD /7/
!
!   LA( 1) =  0  LA( 2) =  1  LA( 3) =  2  LA( 4) =  3  LA( 5) =  4
!   LA( 6) =  5  LA( 7) =  6  LA( 8) =  7  LA( 9) =  8  LA(10) =  9
!   LA(11) =  A  LA(12) =  B  LA(13) =  C  LA(14) =  D  LA(15) =  E
!   LA(16) =  F  LA(17) =  G  LA(18) =  H  LA(19) =  I  LA(20) =  J
!   LA(21) =  K  LA(22) =  L  LA(23) =  M  LA(24) =  N  LA(25) =  O
!   LA(26) =  P  LA(27) =  Q  LA(28) =  R  LA(29) =  S  LA(30) =  T
!   LA(31) =  U  LA(32) =  V  LA(33) =  W  LA(34) =  X  LA(35) =  Y
!   LA(36) =  Z  LA(37) =  /  LA(38) =  .  LA(39) =  -  LA(40) =  +
!   LA(41) =  *  LA(42) =  (  LA(43) =  )  LA(44) =  ,  LA(45) =
!   LA(46) =  =  LA(47) =  $  LA(48) =  '  LA(49) =  a  LA(50) =  b
!   LA(51) =  c  LA(52) =  d  LA(53) =  e  LA(54) =  f  LA(55) =  g
!   LA(56) =  h  LA(57) =  i  LA(58) =  j  LA(59) =  k  LA(60) =  l
!   LA(61) =  m  LA(62) =  n  LA(63) =  o  LA(64) =  p  LA(65) =  q
!   LA(66) =  r  LA(67) =  s  LA(68) =  t  LA(69) =  u  LA(70) =  v
!   LA(71) =  w  LA(72) =  x  LA(73) =  y  LA(74) =  z
!
      DATA LA( 1), LA( 2), LA( 3), LA( 4), LA( 5),   &
           LA( 6), LA( 7), LA( 8), LA( 9), LA(10)/   &
              '0',    '1',    '2',    '3',    '4',   &
              '5',    '6',    '7',    '8',    '9'/
!
      DATA LA(11), LA(12), LA(13), LA(14), LA(15),   &
           LA(16), LA(17), LA(18), LA(19), LA(20)/   &
              'A',    'B',    'C',    'D',    'E',   &
              'F',    'G',    'H',    'I',    'J'/
!
      DATA LA(21), LA(22), LA(23), LA(24), LA(25),   &
           LA(26), LA(27), LA(28), LA(29), LA(30)/   &
              'K',    'L',    'M',    'N',    'O',   &
              'P',    'Q',    'R',    'S',    'T'/
!
      DATA LA(31), LA(32), LA(33), LA(34), LA(35),   &
           LA(36), LA(37), LA(38), LA(39), LA(40)/   &
              'U',    'V',    'W',    'X',    'Y',   &
              'Z',    '/',    '.',    '-',    '+'/
!
      DATA LA(41), LA(42), LA(43), LA(44), LA(45),   &
           LA(46), LA(47), LA(48), LA(49), LA(50)/   &
              '*',    '(',    ')',    ',',    ' ',   &
              '=',    '$',   '''',    'a',    'b'/
!
      DATA LA(51), LA(52), LA(53), LA(54), LA(55),   &
           LA(56), LA(57), LA(58), LA(59), LA(60)/   &
              'c',    'd',    'e',    'f',    'g',   &
              'h',    'i',    'j',    'k',    'l'/
!
      DATA LA(61), LA(62), LA(63), LA(64), LA(65),   &
           LA(66), LA(67), LA(68), LA(69), LA(70)/   &
              'm',    'n',    'o',    'p',    'q',   &
              'r',    's',    't',    'u',    'v'/
!
      DATA LA(71), LA(72), LA(73), LA(74)/   &
              'w',    'x',    'y',    'z'/
!
!     ==================================================================
!
!CCCC ISIGD NEEDS TO BE 6 ON MICROSOFT/COMPAQ PC COMPILER.
!CCCC ALSO NEDS TO BE 6 ON SGI.
!CCCC TO BE SAFE, JUST SET TO 6, WHICH SHOULD WORK ON ALL 32-BIT
!CCCC HOSTS.
!
      LTEMP=0
      ISIGD = 6
!CCCC IF(ICOMPI.EQ.'MS-F')ISIGD = 6
!CCCC IF(ICOMPI.EQ.'LAHE')ISIGD = 6
!
!     ADAPTIONS FOR OMNITAB.
!
!     NW IS USED INSTEAD OF NWIDTH
!     ND IS USED INSTEAD OF NDECS
!     IE IS USED INSTEAD OF IFAULT
!
      ITYPE  = KTYPE
      NDIGIT = KDIGIT
          N1 = K1
          N2 = K2
          NW = KW
          ND = KD
          IE = KE
!
!     GENERAL ERROR CHECKING.
!
      ZLOWER = ITEN ** NDIGIT
      ZUPPER = DTEN * ZLOWER
      IE = IZERO
      IF (ITYPE.GE.IZERO) GO TO 10
        IE = IONE
        GO TO 390
!
!     ..................................................................
!
  10  IF (NDIGIT.GT.IZERO .AND. NDIGIT.LE.ISIGD) GO TO 20
        IE = ITWO
        GO TO 390
!
!     ..................................................................
!
  20  IF (ITYPE.GT.IZERO) GO TO 80
!
!     ==================================================================
!
!                           *** STAGE 1 ***
!                       COMPUTE NWIDTH AND NDECS
!
!     STAGE 1 ERROR CHECKING
!
      IF (N1.GT.IZERO) GO TO 30
        IE = ITHRE
        GO TO 390
!
!     ..................................................................
!
!     N2 MUST BE LARGE ENOUGH FOR NDIGIT, DECIMAL POINT, AND SIGN.
!
  30  IF (N2.GE.NDIGIT+ITWO) GO TO 40
        IE = IFOUR
        GO TO 390
!
!     ..................................................................
!
!     (1)   COMPUTE MMIN, CHARACTERISTIC OF ABSMIN = MIN ABS VALUE X(I)
!             AND COMPUTE MMAX, CHARACTERISTIC OF ABSMAX = MAX ABS X(I).
!
  40  ABSX = ABS (X(1))
      IF (ABSX.LE.RZERO) ABSX = RONE
      ABSMIN = ABSX
      ABSMAX = ABSX
!
      K = IZERO
!
!     K IS USED IN TWO SPECIAL CASES ... WHEN
!        (A)  ALL X(I) EQUAL ZERO, AND
!        (B)  ABS (X(I)) IS LESS THAN 1.0, FOR ALL I, AND SOME X(I)=0.0.
!
      DO 50 I=1,N1
        ABSX = ABS (X(I))
        IF (ABSX.GE.RONE) K = IONE
        IF (ABSX.LE.RZERO) ABSX = RONE
        IF (ABSX.LT.ABSMIN) ABSMIN = ABSX
        IF (ABSX.GT.ABSMAX) ABSMAX = ABSX
  50  CONTINUE
!
      MMIN = INT(FLOG10 (ABSMIN))
      IF (ABSMIN.LT.RONE) MMIN = MMIN - IONE
      MMAX = INT(FLOG10 (ABSMAX))
      IF (ABSMAX.LT.RONE) MMAX = MMAX - IONE
!
!     ADJUST FOR POSSIBLE INCORRECT VALUES OF MMIN AND MMAX DUE TO
!        ERROR IN LOG10 CALCULATION.
!
      Z = ABSMIN
      Z = Z * DTEN ** (NDIGIT-MMIN) + DFIVE
!
      IF (Z.LT.ZLOWER) MMIN = MMIN - IONE
      IF (Z.GE.ZUPPER) MMIN = MMIN + IONE
!
      Z = ABSMAX
      Z = Z * DTEN ** (NDIGIT-MMAX) + DFIVE
!
      IF (Z.LT.ZLOWER) MMAX = MMAX - IONE
      IF (Z.GE.ZUPPER) MMAX = MMAX + IONE
!
!     ..................................................................
!
!     (2)   USE MMIN AND MMAX TO COMPUTE NWIDTH AND NDECS.
!
      ND = NDIGIT - MMIN - IONE
      ND = MAX0 (IZERO,ND)
      NW = MMAX + ITHRE + ND
      IF (MMAX.LT.IZERO) NW = ND + ITWO
      IF (K.EQ.IONE) GO TO 60
!
!     ADJUST FOR SPECIAL CASE (B) DESCRIBED ON LINE RF 368
!
      IF (ABSMIN.LT.RONE .AND. ABSMAX.GE.RONE) NW = NW - IONE
!
!     ADJUST FOR SPECIAL CASE (A) DESCRIBED ON LINE RF 367
!
      IF (ABSMIN.LT.RONE .OR. ABSMAX.LT.RONE) GO TO 60
      NW = ITWO
      ND  = IZERO
!
  60  IF (NW.LE.N2) GO TO 390
!
!     NWIDTH IS TOO LARGE AND HAS TO BE ADJUSTED.
!
        IE = IFIVE
      IF (NDIGIT+IFIVE.LE.N2) GO TO 70
        IE = 6
        GO TO 390
!
!     ..................................................................
!
!
!     NDIGIT+2 = (NDIGIT-1) + (+XX), FOR EXPONENT OF FLOATING-POINT NO.
!
  70  ND = MAX0 (ND,NDIGIT+ITWO)
!
!     N2-3 = N2 - (SIGN+DIGIT+DECIMAL POINT).
!
      ND = MIN0 (ND,N2-ITHRE)
      NW = N2
      GO TO 390
!
!     ==================================================================
!
!                          ***** STAGE 2 *****
!                     PUT HOLLERITH STRING IN NALPHA
!
  80  ABSXVA = ABS (XVALUE)
!
!     STAGE 2 ERROR CHECKING
!
      IF (ITYPE.LT.12) GO TO 90
        IE = IONE
        GO TO 390
!
!     ..................................................................
!
  90  IF (N1.GE.IZERO) GO TO 100
        IE = ITHRE
        GO TO 390
!
!     ..................................................................
!
 100  IF (N2.EQ.IZERO .OR. N2.EQ.IONE) GO TO 110
        IE = 7
        GO TO 390
!
!     ..................................................................
!
 110  IF (ITYPE.LT.9 .AND. NW.LT.ND+ITWO) GO TO 120
      IF (NW.LE.IZERO) GO TO 120
      IF (ITYPE.GT.6) GO TO 130
      IF (ABSXVA.LE.RZERO .AND. NW.GE.ITWO .AND. ITYPE.LE.IFOUR)   &
           GO TO 130
!
!     CHECK WHETHER NWIDTH IS VALID.
!
      IF (NW.LT.NDIGIT+ITWO) GO TO 120
      IF (ITYPE.LT.IFIVE) GO TO 130
      IF (NW.GE.NDIGIT+IFIVE) GO TO 130
 120    IE = 8
        GO TO 390
!
!     ..................................................................
!
 130  IF (ND.GE.IZERO) GO TO 140
        IE = 9
        GO TO 390
!
!     ..................................................................
!
!         VARIABLES USED TO DEFINE FIELD WIDTH FOR R FORMAT
!
!                     -----------------------------
!                     I        NWIDTH             I
!          ----------------------------------------------
!          I  NBLANK  I     NDIFF     I   NDECS   I     I
!          ----------------------------------------------
!          I       NPONE              I
!          ----------------------------------------
!          I             LTOTAL                   I
!          ----------------------------------------------
!          I        NTOTAL = NWIDTH + N1                I
!          ----------------------------------------------
!
!     ..................................................................
!
!     (1)   INITIALIZATION.
!
!     CLEAR OUT NALPHA WITH BLANKS.
!
 140  NTOTAL = NW + N1
      DO 150 I=1,NTOTAL
        NALPHA(I) = LA(45)
 150  CONTINUE
!
      IF (ITYPE.EQ.11) GO TO 390
!
!     IF NECESSARY, CENTER BLANKS WITH LARGEST NUMBER ON RIGHT IF N1 ODD
!
      CALL IDIV (N1+IONE,ITWO,IND,NJUNK)
      NBLANK = N1 - NJUNK * N2
!
      MF    = IZERO
      MREAL = IZERO
      IDECS = ND
      IF (ITYPE.LT.9 .OR. IDECS.EQ.IZERO) GO TO 160
      IDECS = IZERO
      IE    = 14
 160  IF (ITYPE.EQ.IFIVE .OR. ITYPE.EQ.6) IDECS = NDIGIT + ITWO
!
!     THE NEXT THREE STATEMENTS ARE USED TO SWITCH FROM F TO I FORMAT
!
      NSIGDS = NDIGIT
      IWIDTH = NW
      IF (ITYPE.EQ.9 .OR. ITYPE.EQ.ITEN) IWIDTH = IWIDTH + IONE
      NDIFF = IWIDTH - IDECS
      LTOTAL = IWIDTH + NBLANK
      NPONE = NDIFF + NBLANK
!
      IF (ABSXVA.GE.RONE) GO TO 200
      IF (ITYPE.LT.9 .AND. ABSXVA.GT.RZERO) GO TO 200
!
!     ..................................................................
!
!     (2)   XVALUE = 0. IS SPECIAL CASE.
!
      IF (ITYPE.LT.9) GO TO 180
!
!     INTEGER FORMAT
!
      IF (ABSXVA.LE.RHALF .OR. ITYPE.EQ.ITEN) GO TO 170
      NALPHA(LTOTAL-1) = LA(2)
        IF (XVALUE.LT.RZERO) NALPHA(LTOTAL-2) = LA(39)
      GO TO 390
!
!     ..................................................................
!
 170  NALPHA(LTOTAL-1) = LA(1)
      GO TO 390
!
!     ..................................................................
!
!     R FORMAT WITH ZERO STORED AS 0.
!
 180  NALPHA(NPONE  ) = LA(38)
      NALPHA(NPONE-1) = LA(1)
      IF (ITYPE.EQ.IONE .OR. ITYPE.EQ.ITHRE) GO TO 390
      IF (ITYPE.EQ.ITWO .AND. IDECS.EQ.IZERO) GO TO 390
      IF (ITYPE.EQ.IFOUR .AND. IDECS.EQ.IZERO) GO TO 390
!
!     FIXED 0
!
      IF (ITYPE.EQ.7 .AND. ND.EQ.IZERO) GO TO 390
      IF (ITYPE.EQ.8 .AND. ND.EQ.IZERO) GO TO 390
!
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.8) NALPHA(NPONE-1) = LA(45)
!
!     ALL OTHER CASES
!
      IBEG = NPONE + IONE
      IEND = NPONE + IDECS
      DO 190 I=IBEG,IEND
        NALPHA(I) = LA(1)
 190  CONTINUE
!
!     ..................................................................
!
      IF (ITYPE.NE.IFIVE .AND. ITYPE.NE.6) GO TO 390
!
!     FLOATING
!
      NALPHA(LTOTAL-2) = LA(40)
      IF (ITYPE.EQ.IFIVE) GO TO 390
      NALPHA(NPONE  ) = LA(1)
      NALPHA(NPONE-1) = LA(38)
      GO TO 390
!
!     ..................................................................
!
!     (3)   COMPUTE M = CHARACTERISTIC OF ABSXVA = ABS(XVALUE) AND
!                  LL = (NSIGDS+1) INTEGER REPRESENTATION OF ABSXVA.
!              FOR XVALUE = -12.345678, M=1 AND LL=123456784, AN
!              ADDITIONAL DIGIT IN LL IS USED TO AVOID ROUNDOFF ERROR.
!
 200  M = INT(FLOG10 (ABSXVA))
      IF (ABSXVA.LT.RONE) M = M - IONE
      Z = ABSXVA
      Z = Z * DTEN**(NSIGDS-M)
!
!     IF M IS COMPUTED ACCURATELY, ZLOWER .LE. Z .LT. ZUPPER
!
      IF (Z.GE.ZLOWER) GO TO 210
!
!     Z IS LESS THAN ZLOWER BECAUSE M IS ONE TOO LARGE.
!       ADJUST BY SUBTRACTING 1 FROM M AND MULTIPLYING Z BY 10.
!
      M = M - IONE
      Z = DTEN * Z
      GO TO 220
!
 210  IF (Z.LT.ZUPPER) GO TO 220
!
!     Z IS GREATER THAN OR EQUAL TO ZUPPER BECAUSE M IS ONE TOO SMALL.
!       ADJUST BY ADDING 1 TO M AND DIVIDING Z BY 10.
!
      M = M + IONE
      Z = FDDIV (Z,DTEN,IND)
!
 220  X1 = Z
      LL1 = INT(X1)
      X2 = Z - DBLE (X1)
      LL2 = INT(X2)
      LL = LL1 + LL2 + IFIVE
      IF (LL.LT.ITEN**(NSIGDS+IONE)) GO TO 230
!
!     MAKE ADJUSTMENT WHEN LL IS TOO LARGE.
!
      M = M + IONE
      CALL IDIV (LL,ITEN,IND,LL)
      GO TO 240
 230  IF (LL.GE.ITEN**NSIGDS) GO TO 240
!
!     MAKE ADJUSTMENT WHEN LL IS TOO SMALL.
!
      M = M - IONE
      LL = ITEN * LL
 240  IF (ITYPE.EQ.8 .OR. ITYPE.EQ.ITEN) LL = LL - IFIVE
      IF (ITYPE.LT.IFIVE) GO TO 290
      IF (ITYPE.EQ.IFIVE .OR. ITYPE.EQ.6) GO TO 300
!
!     ..................................................................
!
!     (4)   FIXED AND INTEGER.
!
!     CHECK FOR OVERFLOW.
!
      IF (M.GT.NDIFF-ITWO) GO TO 270
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.LT.RZERO) GO TO 270
!
!     ADJUST NUMBER OF DIGITS (NSIGDS) AND LL.
!
      NSIGDS = MIN0 (NDIGIT,IDECS+M+IONE)
      NSIGDS = MAX0 (IZERO,NSIGDS)
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.9) LL = LL - IFIVE
      CALL IDIV (LL,ITEN**(NDIGIT-NSIGDS),IND,LLTEMP)
      LTEMP=LL
      IF (ITYPE.EQ.7 .OR. ITYPE.EQ.9) LL = LL + IFIVE
      IF (LL.LT.ITEN**(NSIGDS+IONE)) GO TO 250
!
!     ADJUST FOR XVALUE ROUNDED TO ONE MORE DIGIT.
!
      M = M + IONE
      NSIGDS = MIN0 (NDIGIT,IDECS+M+IONE)
      NSIGDS = MAX0 (IZERO,NSIGDS)
!
!     CHECK FOR OVERFLOW CAUSED BY ROUNDING TO ONE MORE DIGIT.
!
      IF (M.GT.NDIFF-ITWO) GO TO 270
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.LT.RZERO) GO TO 270
!
!     CHECK FOR UNDERFLOW.
!
 250  IF (NSIGDS.GT.IZERO) GO TO 310
!
!     ADJUST FOR UNDERFLOW.  XVALUE ROUNDED TO IDECS EQUALS ZERO.
!
      IF (IDECS.EQ.IZERO) NALPHA(NPONE-1) = LA(1)
!
      DO 260 I=NPONE,LTOTAL
        NALPHA(I) = LA(1)
 260  CONTINUE
!
      NALPHA(NPONE) = LA(38)
      GO TO 390
!
!     ..................................................................
!
!     PUT IN ASTERISKS WHEN OVERFLOW OCCURS.
!
 270  IE = ITEN
      DO 280 I=1,NW
        ISUBSC = I + NBLANK
        NALPHA(ISUBSC) = LA(41)
 280  CONTINUE
      GO TO 390
!
!     ..................................................................
!
!     (5)   CHECK WHETHER R FORMAT IS FORCED INTO E FORMAT.
!
 290  IF (M.GE.NSIGDS-IONE-IDECS .AND. M.LT.NDIFF-ITWO) GO TO 310
      IF (M.EQ.NDIFF-ITWO .AND. XVALUE.GT.RZERO) GO TO 310
        IE = 11
      IF (NW.GE.NDIGIT+IFIVE .AND. ND.GE.NDIGIT+ITWO) GO TO 300
        IE = 13
      IF (NW.GE.NDIGIT+IFIVE) GO TO 390
        IE = 12
        GO TO 390
!
!     ..................................................................
!
!     (6)   FLOATING.
!
 300  MREAL = M
      M = IZERO
      MF = IONE
!
!     ..................................................................
!
!     (7)   STORE REPRESENTATION IN NALPHA.
!
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
!
      IF (MF.EQ.IZERO) GO TO 340
!
!     ..................................................................
!
!     (8)   PUT IN EXPONENT FOR FLOATING POINT NUMBER.
!
      IF (ITYPE.EQ.IONE .OR. ITYPE.EQ.ITWO .OR. ITYPE.EQ.IFIVE) GO TO  330
!
!     CHANGE FROM 1PE TO 0PE
!
      NALPHA(NINT+1) = NALPHA(NINT)
      NALPHA(NINT  ) = LA(38)
      MREAL = MREAL + IONE
!
 330  IF (MREAL.LT.IZERO) NALPHA(NEND+1) = LA(39)
      IF (MREAL.GE.IZERO) NALPHA(NEND+1) = LA(40)
      MREALA = IABS(MREAL)
      CALL IDIV (MREALA,ITEN,IND,M1)
      M2 = MOD (MREALA,ITEN)
      NALPHA(NEND+2) = LA(M1+1)
      NALPHA(NEND+3) = LA(M2+1)
!
!     ..................................................................
!
!     (9)   PUT IN MINUS SIGN IF XVALUE LESS THAN ZERO.
!
 340  IF (XVALUE.GE.RZERO) GO TO 350
        IF (M.GE.IZERO) NALPHA(NINT-1) = LA(39)
        IF (M.LT.IZERO) NALPHA(NPONE-1) = LA(39)
 350  IF (M.GE.(-IONE)) GO TO 370
!
!     PUT ZEROS AFTER DECIMAL POINT FOR ABSXVA LESS THAN 0.1
!
      IBEG = NPONE + IONE
      IEND = NINT - IONE
      DO 360 I=IBEG,IEND
        NALPHA(I) = LA(1)
 360  CONTINUE
      GO TO 390
!
!     ..................................................................
!
!     (10)   PUT IN NON-SIGNIFICANT ZEROS FOR LARGE INTEGERS.
!
 370  IF (M.LT.NSIGDS .OR. MF.NE.IZERO) GO TO 390
      IBEG = NINT + NSIGDS
      IEND = NPONE - IONE
      DO 380 I=IBEG,IEND
        NALPHA(I) = LA(1)
 380  CONTINUE
!
!     ..................................................................
!
 390  KW = NW
      KD = ND
      KE = IE
      IF (IE.EQ.IZERO .OR. IE.EQ.IFIVE .OR. IE.EQ.6 .OR. IE.EQ.ITEN   &
                      .OR. IE.EQ.11    .OR. IE.GE.14) RETURN
!CCCC   CALL ERROR (259)
        RETURN
!
!     ==================================================================
!
      END SUBROUTINE RFORMT 
!SCREEN
      SUBROUTINE SCREEN(RR,KX,NR,NDEF,IBIT,MBST,INTCPT,A,NS,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        IBUGA3,ISUBRO,IERROR)
!
! **  NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. SCREEN V 7.00  4/21/92. **
!
!     ==================================================================
!
!                        ***   GENERAL COMMENTS   ***
!
!     **************************************************************** *
!                                                                      *
!                   REGRESSIONS BY LEAPS AND BOUNDS                    *
!          A PROGRAM FOR FINDING THE BEST SUBSET REGRESSIONS           *
!                     G.M.FURNIVAL AND R.W.WILSON                      *
!               YALE UNIVERSITY AND U.S. FOREST SERVICE                *
!                           VERSION 11/11/74                           *
!                                                                      *
!                 CALL SCREEN(RR,KX,NR,NDEF,IBIT,MBST)                 *
!                                                                      *
!     RR   = UPPER TRIANGULAR PORTION OF (KX+1)*(KX+1) CORRELATION OR  *
!            PRODUCT MATRIX. VARIABLE KX+1 IS THE DEPENDENT VARIABLE.  *
!     KX   = NUMBER OF INDEPENDENT VARIABLES (3.LE.KX.LE.28)           *
!     NR   = DIMENSION OF RR (NR.GT.KX)                                *
!     NDEF = DEGREES OF FREEDOM FOR RR (NDEF.GT.KX)                    *
!     IBIT = SELECTION CRITERION CODE (1=R**2,2=ADJUSTED R**2,3=CP)    *
!     MBST = NUMBER OF BEST REGRESSIONS DESIRED (1.LE.MBST.LE.10)      *
!                                                                      *
!       MBST BEST REGRESSIONS FOR EACH SIZE SUBSET WHEN IBIT.EQ.1      *
!             MBST BEST REGRESSIONS IN TOTAL WHEN IBIT.GT.1            *
!                                                                      *
!     **************************************************************** *
!
!     ARRAY STORAGE REQUIRED FOR K=KX INDPENDENT VARIABLES AND M = K+1.
!         2*NL FOR XI AND XN, WHERE NL = M(M+1)(M+2)/6
!        4M**2 FOR ILI, ILM, MD AND NC
!      2*(11M) FOR CL AND RM
!          12M FOR CI, CN, CO, ID, IPI, IPN, NI, NN, TOLL, YI, YN AND ZC
!
!     TOTAL STORAGE EQUALS 2M(M+1)(M+2)/6 + 4M**2 +22M + 12M
!                   = (M**3 + 15*M**2 + 104*M)/3
!
!              ***   ARRAY STORAGE EQUIVALENCE TO A(.)  ***
!
!                 ARRAY             SIZE                  START
!
!                   XI               NL                       1
!                   XN               NL                    NL+1
!                 .............................................
!                  ILI             M**2           2*NL+       1
!                  ILN             M**2           2*NL+  M**2+1
!                   MD             M**2           2*NL+2*M**2+1
!                   NC             M**2           2*NL+3*M**2+1
!                 .............................................
!                   CL             11*M      2*NL+4*M**2+     1
!                   RM             11*M      2*NL+4*M**2+11*M+1
!                 .............................................
!                   CI                M      2*NL+4*M**2+22*M+1
!                   CN                M      2*NL+4*M**2+23*M+1
!                   CO                M      2*NL+4*M**2+24*M+1
!                   ID                M      2*NL+4*M**2+25*M+1
!                  IPI                M      2*NL+4*M**2+26*M+1
!                  IPN                M      2*NL+4*M**2+27*M+1
!                   NI                M      2*NL+4*M**2+28*M+1
!                   NN                M      2*NL+4*M**2+29*M+1
!                 TOLL                M      2*NL+4*M**2+30*M+1
!                   YI                M      2*NL+4*M**2+31*M+1
!                   YN                M      2*NL+4*M**2+32*M+1
!                   ZC                M      2*NL+4*M**2+33*M+1
!                 .............................................
!
!               ADAPTED TO OMNITAB COMPUTING SYSTEM BY -
!                      DAVID HOGBEN,
!                      STATISTICAL ENGINEERING DIVISION,
!                      COMPUTING AND APPLIED MATHEMATICS LABORATORY,
!                      A337 ADMINISTRATION BUILDING,
!                      NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!                      GAITHERSBURG, MD 20899
!                          TELEPHONE 301-921-3651
!                  ORIGINAL VERSION - FEBRUARY, 1977.
!                   CURRENT VERSION -    APRIL, 1992.
!
!     ==================================================================
!
!                    ***   SPECIFICATION STATEMENTS   ***
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      PARAMETER (MAXC=100)
!
!CCCC DIMENSION     ID(29),    IPI(29),   IPN(29),    NI(29),    NN(29)
      DIMENSION  ID(MAXC),  IPI(MAXC),  IPN(MAXC),  NI(MAXC),  NN(MAXC)
      DIMENSION ILI(845), ILN(845), MD(845), NC(845)
!
!CCCC INCLUDE 'WRKSCR.H'
      REAL A(NS)
!
!     ==================================================================
!
!                         ***   TYPE STATEMENTS   ***
!
!CCCC REAL             RR(29,29)
      REAL             RR(MAXC,MAXC)
      REAL             BOUND, CAB, RS, R2
      REAL             SIG, SS, TEMP, TOL, TWO
      REAL             FDIV
      REAL             SPCA, SPCB
!
!     ..................................................................
!
      DOUBLE PRECISION DTWO
!
      PARAMETER (MAXV=98)
      CHARACTER*1 ICOD(MAXV)
      CHARACTER*8 IVLIST
      COMMON/BESTC1/IOUNI1,IOUNI2
      COMMON/BESTC2/IVLIST(MAXV)
!
      PARAMETER(NUMCLI=3)
!CCCC PARAMETER(NUMCLI=17)
      PARAMETER(MAXLIN=1)
      PARAMETER (MAXROW=38)
      CHARACTER*40 ITITLE
      CHARACTER*40 ITITLZ
      CHARACTER*40 ITITL9
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
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
!
      INCLUDE 'DPCOP2.INC'
!
!     ==================================================================
!
!                 ***   DATA INITIALIZATION STATEMENTS   ***
!
      DATA DTWO  / 2.0D0 /
!
      DATA RTWO  / 2.0 /
      DATA RONE  / 1.0 /
      DATA RZERO / 0.0 /
      DATA RER   / 1.0E-8 /
!
      DATA IFOUR  /4/
      DATA ITHRE  /3/
      DATA ITWO   /2/
      DATA IONE   /1/
      DATA IZERO  /0/
      DATA LWIDE  /80/
!
      DATA KO, NV / 10, 11 /
!
      DATA SPCA /   100.0 /
      DATA SPCB / 10000.0 /
!
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
!
      IFRST=0
      ILAST=0
      ICNT9=0
      DO 1 II=1,845
        MD(II)=0
   1  CONTINUE
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
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      IINC=1800
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1) + IINC
      IWRTF(3)=IWRTF(2) + IINC
!
!     ==================================================================
!
!     10=KO=NV-1     NL=(KX+1)*(KX+2)*(KX+3)/6      ND-1=NR-1
!                          NX=(KX+1)*(KX+2)/2
!
!                                 SET UP SIZE OF KZ, ND, NL AND NX.
!
      KZ = KX + IONE
      ND = KZ
      CALL IDIV (ND * (ND + IONE) * (ND + ITWO),6,IND,NL)
      CALL IDIV (ND * (ND + IONE),ITWO,IND,NX)
!
!                                 TEST INPUT.
!
      KZSIZE = ITWO * NL + IFOUR * ND ** 2 + 34 * ND
      IF (KZSIZE.GT.NS) THEN
         WRITE(ICOUT,23)
         CALL DPWRST('XXX','BUG ')
!CCCC    CALL ERROR (23)
         RETURN
      ENDIF
   23 FORMAT(1X,'***** ERROR FROM SCREEN (BEST CP): INSUFFICIENT ',   &
            'SCRATCH SPACE.')
!CCCC IF (NERROR.NE.IZERO) RETURN
!
!     ..................................................................
!
      IF (KX.GE.ITHRE .AND. KX.LT.ND .AND. NDEF.GT.KX .AND.   &
           MBST.GT.IZERO .AND. MBST.LE.KO .AND. KO.LE.NV .AND. NR.GT.KX   &
           .AND. IBIT.GE.IONE .AND. IBIT.LE.ITHRE) GO TO 10
!CCCC CALL ERROR (3)
      WRITE(ICOUT,3)
      CALL DPWRST('XXX','BUG ')
    3 FORMAT(1X,'***** ERROR FROM SCREEN (BEST CP): INVALID OPTIONS')
      RETURN
!
!     ..................................................................
!
  10  SS = FDIV (RR(KZ,KZ),SPCA,IND)
      IF (IBIT.EQ.ITWO) SS = FDIV (SS,FLOAT(NDEF),IND)
      IF (SS.GT.RZERO) GO TO 30
  20  CONTINUE
!CCCC CALL ERROR (22)
      WRITE(ICOUT,22)
      CALL DPWRST('XXX','BUG ')
   22 FORMAT(1X,'***** ERROR FROM SCREEN (BEST CP): NON-POSITIVE SUM ',   &
            'OF SQUARES')
      RETURN
!
!     ..................................................................
!
!                                 INITIALIZE.
!
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
!
!  FOR DATAPLOT, SET NTLINE HIGH.  THAT IS, WE ARE NOT USING A PAGE
!  BASED OUTPUT.
!
      NTLINE = 500
!
!CCCC IF (NCRT.NE.IZERO) NTLINE = LENGTH + ITHRE
      DO 50 L=1,KZ
        CALL IDIV ((KZ-IONE)*KZ*(KZ+IONE)-(KZ-L)*(KZ-L+IONE)*   &
                      (KZ-L+ITWO),6,IND,ID(ISUBID))
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
!
!                           STORE MATRICES AS VECTORS.
!
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
!
!                             INVERT MATRIX STEPWISE.
!
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
          IF (RS.LT.A(MSUBRM)) CALL CPSTRE (RS,A(LSUBCI)+A(MSUBCO),   &
                                      KO,A(LSUBCL),A(LSUBRM),N1,NV,ND)
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
!
      N      = KZ
 100  K      = N - IONE
      KP     = KZ * K + LISUBL
      KXSUBL = KZ * (KX - IONE) + LISUBL
      IF (K.NE.KX) THEN
         ICNT=0
         DO 102 I=KP,KXSUBL,KZ
           ICNT=ICNT+1
           IF(ICNT.EQ.22)ILAST=I
           IF(ICNT.EQ.23)IFRST=I
  102    CONTINUE
!CCCC    WRITE (ICOUT,330) (ILI(I),I=KP,KXSUBL,KZ)
         WRITE (ICOUT,330)
 330     FORMAT(2X,   &
                'SCREEN-MATRIX IS SINGULAR.  VARIABLES DELETED ARE ...')
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
!
!     INTCPT - IONE = ADJUSTMENT FOR USING WITH NO CONSTANT TERM.
!
      SIG    = FDIV (RTWO*A(ISUBXC),FLOAT(NDEF-K+IONE-INTCPT),IND)
      A(LSUBYI)  = A(ISUBXC)
      A(LSUBYN)  = RR(KZ,KZ)
!
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
!
 130  NREG =  IZERO
      NCAL =  ITWO
      MN   =  ITWO
      MV   = -IONE
!
!                                 STAGE  LOOP.
!
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
!
!                              FIND LEAP FROM BOUNDS.
!
      ISUBRM = LSUBRM + LOW - IONE
      KSUBRM = LSUBRM + 11 * (KZ - IONE) + LOW - IONE
      DO 150 LB=IP,KM
        MT     = MN + KM - LB
        MSUBRM = ISUBRM + 11 * (MT - IONE)
        IF (IBIT.EQ.IONE .AND. A(MSUBRM).GT.BOUND) GO TO 160
        IF (IBIT.EQ.ITWO .AND. A(KSUBRM).GT.FDIV(BOUND,FLOAT(NDEF-MT),   &
           IND)) GO TO 160
        IF (IBIT.EQ.ITHRE .AND. A(KSUBRM).GT.BOUND+SIG*FLOAT(MT))   &
                 GO TO 160
 150  CONTINUE
      GO TO 140
!
 160  LC = KM + IP - LB
      NREG = NREG + ITWO * (LC-IP+IONE)
      IF (IP.EQ.IONE) LC = K
!
!                         REGRESSIONS FROM INVERSE MATRIX.
!
      ISUBNI = NISUBL + IP
      ISUBNN = NNSUBL + IP
      KSUBLI = LISUBL + IP - IONE
      KSUBLN = LNSUBL + IN - IONE
      KSUBNN = NNSUBL + IN - IONE
      DO 200 LB=IP,LC
        LBB = LB
        CALL BACK (NC(NCSUBL),LBB,LI,IPI(IPSUBL),MV,RS,BOUND,ILI(LISUBL)   &
                  ,JC,ID(IDSUBL),A(LSUBXI),MD(MDSUBL),   &
                   IONE,NI(NISUBL),ND,KZ,NL,NCAL)
!
!                               RE-ORDER VARIABLES.
!
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
!
!                         REGRESSIONS FROM PRODUCT MATRIX.
!
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
        CALL BACK (NC(NCSUBL),LBB,L,IPN(NPSUBL),MN,A(ISUBYN),A(MSUBCN)   &
                  ,ILN(LNSUBL),JC,ID(IDSUBL),A(LSUBXN),MD(MDSUBL),   &
                   IZERO,NN(NNSUBL),ND,KZ,NL,NCAL)
        MSUBNC     = ISUBNC + KZ * (L - IONE)
        ISUB4      = NC(MSUBNC)
        MSUBCI     = LSUBCI + LB
        MSUBCO     = ISUBCO + ISUB4
        A(MSUBCI) = A(ISUBCI) - A(MSUBCO)
        A(MSUBCN) = A(ISUBCN) + A(MSUBCO)
        IF (A(ISUBYI).GE.A(ISUBRM)) GO TO 210
        CALL CPSTRE (A(ISUBYI),A(MSUBCI),KO,A(LSUBCL),A(LSUBRM),MI,   &
                     NV,ND)
        IF (IBIT.EQ.IONE) GO TO 210
        IF (IBIT.EQ.ITWO) RS = FDIV (A(ISUBYI),FLOAT(NDEF-MI),IND)
        IF (IBIT.EQ.ITHRE) RS = A(ISUBYI) + FLOAT(MI) * SIG
        IF (RS.LT.A(KSUBRM)) CALL CPSTRE (RS,A(MSUBCI),KO,A(LSUBCL),   &
            A(LSUBRM),KZ,NV,ND)
 210    MSUBRM = LSUBRM + 11 * (MN - IONE)
        IF (A(ISUBYN).GE.A(MSUBRM)) GO TO 220
        CALL CPSTRE (A(ISUBYN),A(MSUBCN),KO,A(LSUBCL),A(LSUBRM),MN,   &
                     NV,ND)
        IF (IBIT.EQ.IONE) GO TO 220
        IF (IBIT.EQ.ITWO) RS = FDIV (A(ISUBYN),FLOAT(NDEF-MN),IND)
        IF (IBIT.EQ.ITHRE) RS = A(ISUBYN) + FLOAT(MN) * SIG
        IF (RS.LT.A(KSUBRM)) CALL CPSTRE (RS,A(MSUBCN),KO,A(LSUBCL),   &
            A(LSUBRM),KZ,NV,ND)
 220    MN            = MN + IONE
        ISUBPN        = NPSUBL + MN - IONE
        IPN(ISUBPN+1) = IPN(ISUBPN) + IONE
        IN            = IS
 230  CONTINUE
      IF (LC.EQ.KM) MN = MN - IONE
      GO TO 140
!
!                                    OUTPUT.
!
 240  CONTINUE
      CALL IDIV (KX-IONE,ITWO,IND,NJUNK)
      NLINES = 8 + NJUNK
      ISUBCL = LSUBCL - 12
      ISUBRM = LSUBRM - 12
!
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
!
      DO 320 M=1,K
        MM     = M
        ISUBCL = ISUBCL + 11
        ISUBRM = ISUBRM + 11
!CCCC   IF (NLINES+ITHRE.LE.NTLINE) GO TO 250
!CCCC   CALL PAGE (IFOUR)
!CCCC   NLINES = ITHRE
!250    CONTINUE
        IF (KO.GT.IONE .AND. M.EQ.IONE) THEN
!CCCC      WRITE (ICOUT,390)
!390       FORMAT(4X,'REGRESSION WITH 1 VARIABLE')
!CCCC      CALL DPWRST('XXX','BUG ')
           ITITLE='Regression with One Variable'
           NCTITL=28
        ELSEIF (KO.GT.IONE .AND. M.GT.IONE) THEN
!CCCC      WRITE(ICOUT,999)
!CCCC      CALL DPWRST('XXX','BUG ')
!CCCC      WRITE (ICOUT,340) M
!340       FORMAT(4X,'REGRESSIONS WITH',I3,' VARIABLES')
!CCCC      CALL DPWRST('XXX','BUG ')
           ITITLE='Regressions with     Variables'
           WRITE(ITITLE(18:20),'(I3)')M
           NCTITL=30
        ENDIF
!
        NLINES = NLINES + ITWO
        IPRTSW = IZERO
        DO 310 LA=1,KO
          NCOF   = IONE
          L      = KO - LA + IONE
          MSUBRM = ISUBRM + L
!CCCC     IF (A(MSUBRM).EQ.TWO) GO TO 320
          IF (A(MSUBRM).EQ.TWO) GO TO 329
          IF (IBIT.EQ.IONE)  R2 = SPCA - FDIV (A(MSUBRM),SS,IND)
          IF (IBIT.EQ.ITWO)  RS = FDIV (A(MSUBRM),FLOAT(NDEF-M),IND)
          IF (IBIT.EQ.ITHRE) RS = A(MSUBRM) + SIG * FLOAT(M)
          IF (IBIT.EQ.IONE .AND. LA.LE.MBST .OR. IBIT.GT.IONE   &
               .AND. RS.LE.A(JSUBRM)) NCOF = IZERO
          IF (IBIT.EQ.ITWO)  R2 = SPCA - FDIV (RS,SS,IND)
          IF (IBIT.EQ.ITHRE) R2 = RTWO * FDIV (RS,SIG,IND) - FLOAT(NDEF)
!
!           ADJUSTMENT TO ALLOW USE OF MODEL WHICH DOES NOT HAVE
!              A CONSTANT TERM FOR THE FIRST TERM.
!                 CHANGE SUGGESTED BY JAMES W. FRANE.
!
          IF  (IBIT.EQ.ITHRE .AND. INTCPT.EQ.IZERO) R2 = R2 - RONE
          IF  (IBIT.EQ.ITHRE .AND. INTCPT.EQ.IONE ) R2 = R2 + RONE
          ANTEMP=REAL(NDEF+1)
          RSSTMP=A(MSUBRM)
          RSSTM2=RSSTMP/ANTEMP
          BIC=ANTEMP*LOG(RSSTM2) + REAL(M+1)*LOG(ANTEMP)
!
!                               DECODE LABELS.
!
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
!
          IF (NCOF.NE.IZERO) THEN
             ICNT9=ICNT9+1
             IF (IPRTSW.GT.IZERO) GO TO 300
             NLINES = NLINES + M + IONE
             IF (M.GT.15 .AND. LWIDE.LT.110) NLINES = NLINES + M
             IF (NLINES.LE.NTLINE) GO TO 290
!CCCC        CALL PAGE (IFOUR)
             NLINES = M + IFOUR
             IF (M.GT.15 .AND. LWIDE.LT.110) NLINES = NLINES + M
 290         CONTINUE
!
!CCCC        WRITE (ICOUT,350)
!350         FORMAT(10X,'C(P) STATISTIC',2X,'VARIABLES')
!CCCC        CALL DPWRST('XXX','BUG ')
!
             NUMCOL=2
             NUMLIN=1
!
             DO 1183 I=1,MAXLIN
               DO 1185 J=1,NUMCLI
                 ITITL2(I,J)=' '
                 NCTIT2(I,J)=0
 1185          CONTINUE
 1183        CONTINUE
!
             ITITL2(1,1)='C(p) Statistic'
             NCTIT2(1,1)=14
             ITITL2(1,2)='BIC'
             NCTIT2(1,2)=3
             ITITL2(1,3)='Variable'
             NCTIT2(1,3)=8
!
             NMAX=0
             NUMCOL=3
             DO 1193 I=1,NUMCOL
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
               DO 1195 J=1,MAXROW
                 IVALUE(J,I)=' '
                 NCVALU(J,I)=0
                 AMAT(J,I)=0.0
 1195          CONTINUE
 1193        CONTINUE
             ICNT=0
!
             IPRTSW = IONE
!
 300         CONTINUE
             ISTPPN = NPSUBL + M - IONE
             IJUNK=1
             IF(M.EQ.IONE)THEN
!
!              FOLLOWING CODE ONLY IMPLEMENTED FOR THE REGRESSIONS
!              WITH ONE VARIABLE, SO CAN SIMPLIFY CODE A BIT.
!
               WRITE(IOUNI1,71)IJUNK,R2,BIC,IVLIST(IPN(NPSUBL))
  71           FORMAT(I3,1X,2F15.3,' : ',A8)
               WRITE(IOUNI2,'(A1)')ICOD(IPN(NPSUBL))
             ENDIF
!CCCC        IF (LWIDE.GE.110) THEN
!CCCC           WRITE (ICOUT,360) R2, (IPN(I),I=NPSUBL,ISTPPN)
!360            FORMAT(13X,F8.3,5X,28I3)
!CCCC           CALL DPWRST('XXX','BUG ')
!CCCC        ELSEIF (LWIDE.LT.110) THEN
                INUMB=ISTPPN-NPSUBL+1
                IF(INUMB.LE.15)THEN
!CCCC             WRITE (ICOUT,370) R2, (IPN(I),I=NPSUBL,ISTPPN)
!370              FORMAT(14X,F8.3,3X,15I3)
!CCCC             CALL DPWRST('XXX','BUG ')
                  ICNT=ICNT+1
                  AMAT(ICNT,1)=R2
                  AMAT(ICNT,2)=BIC
!
!                 FOLLOWING ASSUMES ONLY ONE VARIABLE
!
!CCCC             WRITE(IVALUE(ICNT,3),'(15I3)')(IPN(I),I=NPSUBL,ISTPPN,15)
!CCCC             NCVALU(ICNT,3)=3*INUMB
                  IVALUE(ICNT,3)=IVLIST(IPN(NPSUBL))
                  NCVALU(ICNT,3)=8
                ELSE
!
!                 NOTE: SINCE THIS FORMATTING ONLY USED FOR THE
!                       ONE VARIABLE CASE, CAN COMMENT OUT THIS
!                       SECTION.
!
!ONE              ITEMP1=NPSUBL+14
!CCCC             WRITE (ICOUT,370) R2, (IPN(I),I=NPSUBL,ITEMP1)
!CCCC             CALL DPWRST('XXX','BUG ')
!CCCC             WRITE (ICOUT,371) R2, (IPN(I),I=ITEMP1+1,ISTPPN)
!371              FORMAT(26X,15I3)
!CCCC             CALL DPWRST('XXX','BUG ')
!ONE              ICNT=ICNT+1
!ONE              AMAT(ICNT,1)=R2
!ONE              AMAT(ICNT,2)=BIC
!ONE              WRITE(IVALUE(ICNT,3),'(15I3)')
!ONE 1                  (IPN(I),I=NPSUBL,ITEMP1)
!ONE              NCVALU(ICNT,3)=45
!ONE              ICNT=ICNT+1
!ONE              AMAT(ICNT,1)=R2
!ONE              AMAT(ICNT,2)=BIC
!ONE              WRITE(IVALUE(ICNT,3),'(15I3)')
!ONE 1                  (IPN(I),I=ITEMP1+1,ISTPPN)
!ONE              ITEMP2=ISTPPN-ITEMP1
!ONE              NCVALU(ICNT,3)=3*ITEMP2
                ENDIF
          ELSE
             NUMCOL=3
             NUMLIN=1
!
             DO 183 I=1,MAXLIN
               DO 185 J=1,NUMCLI
                 ITITL2(I,J)=' '
                 NCTIT2(I,J)=0
  185          CONTINUE
  183        CONTINUE
!
             ITITL2(1,1)='Variable'
             NCTIT2(1,1)=8
             ITITL2(1,2)='Coefficient'
             NCTIT2(1,2)=11
             ITITL2(1,3)='F Ratio'
             NCTIT2(1,3)=7
!
             NMAX=0
             NUMCOL=3
             DO 193 I=1,NUMCOL
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
               DO 195 J=1,MAXROW
                 IVALUE(J,I)=' '
                 NCVALU(J,I)=0
                 AMAT(J,I)=0.0
  195          CONTINUE
  193        CONTINUE
!
!CCCC        NLINES = NLINES + M + ITHRE
!CCCC        IF (NLINES.LE.NTLINE) GO TO 270
!CCCC        CALL PAGE (IFOUR)
!CCCC        NLINES = M + 6
!270         CONTINUE
             CALL COEF (R2,BIC,MP,KZ,A(LSUBXC),RR,MAXC,IPN(NPSUBL),   &
                        NDEF,MM,ND,   &
                        MD(MDSUBL),NX,IBIT,A(LSUBZC),   &
                        AMAT,IVALUE,NCVALU,MAXROW,NUMCLI,ITITL9,NCTIT9)
!
             NUMLIN=1
             ICNT=MM
             IFRSTZ=.TRUE.
             ILASTZ=.TRUE.
             IFLAGS=.TRUE.
             IFLAGE=.TRUE.
             IF(IPRINT.EQ.'ON')THEN
               CALL DPDTA5(ITITLE,NCTITL,   &
                           ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                           MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                           IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                           IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                           ICAPSW,ICAPTY,IFRSTZ,ILASTZ,   &
                           IFLAGS,IFLAGE,   &
                           ISUBRO,IBUGA3,IERROR)
             ENDIF
             ITITLE=' '
             NCTITL=0
             ICNT9=0
!
!CCCC        GO TO 310
          ENDIF
!
 310    CONTINUE
!
 329    CONTINUE
        NUMLIN=1
        ITITL9=' '
        NCTIT9=0
        IFRSTZ=.TRUE.
        ILASTZ=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        IF(IPRINT.EQ.'ON' .AND. ICNT9.GT.0)THEN
          CALL DPDTA5(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRSTZ,ILASTZ,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
        ENDIF
!
 320  CONTINUE
      NCAL = NCAL + ITWO * NREG
      IF(IFEEDB.EQ.'ON')THEN
        WRITE (ICOUT,380) NREG, NCAL
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
!
!     ==================================================================
!
!                       ***   FORMAT STATEMENTS   ***
!
 380  FORMAT(2X,I9,' REGRESSIONS',2X,I10,' OPERATIONS')
!999  FORMAT(1X)
!
!     ==================================================================
!
      END SUBROUTINE SCREEN
      SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA,   &
                       XDATA,NOBS)
!***BEGIN PROLOGUE  SNSQE
!***DATE WRITTEN   800301   (YYMMDD)
!***REVISION DATE  880222   (YYMMDD)
!***CATEGORY NO.  F2A
!***KEYWORDS  EASY-TO-USE,NONLINEAR SQUARE SYSTEM,POWELL HYBRID METHOD,
!             ZERO
!***AUTHOR  HIEBERT, K. L., (SNLA)
!***PURPOSE  SNSQE is the easy-to-use version of SNSQ which finds a zero
!            of a system of N nonlinear functions in N variables by a
!            modification of Powell hybrid method.  This code is the
!            combination of the MINPACK codes(Argonne) HYBRD1 and HYBRJ1
!***DESCRIPTION
!
! 1. Purpose.
!
!
!       The purpose of SNSQE is to find a zero of a system of N non-
!       linear functions in N variables by a modification of the Powell
!       hybrid method.  This is done by using the more general nonlinear
!       equation solver SNSQ.  The user must provide a subroutine which
!       calculates the functions.  The user has the option of either to
!       provide a subroutine which calculates the Jacobian or to let the
!       code calculate it by a forward-difference approximation.  This
!       code is the combination of the MINPACK codes (Argonne) HYBRD1
!       and HYBRJ1.
!
!
! 2. Subroutine and Type Statements.
!
!       SUBROUTINE SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,
!      *                  WA,LWA)
!       INTEGER IOPT,N,NPRINT,INFO,LWA
!       REAL TOL
!       REAL X(N),FVEC(N),WA(LWA)
!       EXTERNAL FCN,JAC
!
!
! 3. Parameters.
!
!       Parameters designated as input parameters must be specified on
!       entry to SNSQE and are not changed on exit, while parameters
!       designated as output parameters need not be specified on entry
!       and are set to appropriate values on exit from SNSQE.
!
!       FCN is the name of the user-supplied subroutine which calculates
!         the functions.  FCN must be declared in an EXTERNAL statement
!         in the user calling program, and should be written as follows.
!
!         SUBROUTINE FCN(N,X,FVEC,IFLAG)
!         INTEGER N,IFLAG
!         REAL X(N),FVEC(N)
!         ----------
!         Calculate the functions at X and
!         return this vector in FVEC.
!         ----------
!         RETURN
!         END
!
!         The value of IFLAG should not be changed by FCN unless the
!         user wants to terminate execution of SNSQE.  In this case, set
!         IFLAG to a negative integer.
!
!       JAC is the name of the user-supplied subroutine which calculates
!         the Jacobian.  If IOPT=1, then JAC must be declared in an
!         EXTERNAL statement in the user calling program, and should be
!         written as follows.
!
!         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
!         INTEGER N,LDFJAC,IFLAG
!         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
!         ----------
!         Calculate the Jacobian at X and return this
!         matrix in FJAC.  FVEC contains the function
!         values at X and should not be altered.
!         ----------
!         RETURN
!         END
!
!         The value of IFLAG should not be changed by JAC unless the
!         user wants to terminate execution of SNSQE.  In this case, set
!         IFLAG to a negative integer.
!
!         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
!
!       IOPT is an input variable which specifies how the Jacobian will
!         be calculated.  If IOPT=1, then the user must supply the
!         Jacobian through the subroutine JAC.  If IOPT=2, then the
!         code will approximate the Jacobian by forward-differencing.
!
!       N is a positive integer input variable set to the number of
!         functions and variables.
!
!       X is an array of length N.  On input, X must contain an initial
!         estimate of the solution vector.  On output, X contains the
!         final estimate of the solution vector.
!
!       FVEC is an output array of length N which contains the functions
!         evaluated at the output X.
!
!       TOL is a non-negative input variable.  Termination occurs when
!         the algorithm estimates that the relative error between X and
!         the solution is at most TOL.  Section 4 contains more details
!         about TOL.
!
!       NPRINT is an integer input variable that enables controlled
!         printing of iterates if it is positive.  In this case, FCN is
!         called with IFLAG = 0 at the beginning of the first iteration
!         and every NPRINT iteration thereafter and immediately prior
!         to return, with X and FVEC available for printing. Appropriate
!         print statements must be added to FCN (see example). If NPRINT
!         is not positive, no special calls of FCN with IFLAG = 0 are
!         made.
!
!       INFO is an integer output variable.  If the user has terminated
!         execution, INFO is set to the (negative) value of IFLAG.  See
!         description of FCN and JAC. Otherwise, INFO is set as follows.
!
!         INFO = 0  improper input parameters.
!
!         INFO = 1  algorithm estimates that the relative error between
!                   X and the solution is at most TOL.
!
!         INFO = 2  number of calls to FCN has reached or exceeded
!                   100*(N+1) for IOPT=1 or 200*(N+1) for IOPT=2.
!
!         INFO = 3  TOL is too small.  No further improvement in the
!                   approximate solution X is possible.
!
!         INFO = 4  iteration is not making good progress.
!
!         Sections 4 and 5 contain more details about INFO.
!
!       WA is a work array of length LWA.
!
!       LWA is a positive integer input variable not less than
!         (3*N**2+13*N))/2.
!
!
! 4. Successful Completion.
!
!       The accuracy of SNSQE is controlled by the convergence parame-
!       ter TOL.  This parameter is used in a test which makes a compar-
!       ison between the approximation X and a solution XSOL.  SNSQE
!       terminates when the test is satisfied.  If TOL is less than the
!       machine precision (as defined by the function R1MACH(4)), then
!       SNSQE attemps only to satisfy the test defined by the machine
!       precision.  Further progress is not usually possible.  Unless
!       high precision solutions are required, the recommended value
!       for TOL is the square root of the machine precision.
!
!       The test assumes that the functions are reasonably well behaved,
!       and, if the Jacobian is supplied by the user, that the functions
!       and the Jacobian  coded consistently.  If these conditions
!       are not satisfied, SNSQE may incorrectly indicate convergence.
!       The coding of the Jacobian can be checked by the subroutine
!       CHKDER.  If the Jacobian is coded correctly or IOPT=2, then
!       the validity of the answer can be checked, for example, by
!       rerunning SNSQE with a tighter tolerance.
!
!       Convergence Test.  If SNRM2(Z) denotes the Euclidean norm of a
!         vector Z, then this test attempts to guarantee that
!
!               SNRM2(X-XSOL) .LE.  TOL*SNRM2(XSOL).
!
!         If this condition is satisfied with TOL = 10**(-K), then the
!         larger components of X have K significant decimal digits and
!         INFO is set to 1.  There is a danger that the smaller compo-
!         nents of X may have large relative errors, but the fast rate
!         of convergence of SNSQE usually avoids this possibility.
!
!
! 5. Unsuccessful Completion.
!
!       Unsuccessful termination of SNSQE can be due to improper input
!       parameters, arithmetic interrupts, an excessive number of func-
!       tion evaluations, errors in the functions, or lack of good prog-
!       ress.
!
!       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1, or
!         IOPT .GT. 2, or N .LE. 0, or TOL .LT. 0.E0, or
!         LWA .LT. (3*N**2+13*N)/2.
!
!       Arithmetic Interrupts.  If these interrupts occur in the FCN
!         subroutine during an early stage of the computation, they may
!         be caused by an unacceptable choice of X by SNSQE.  In this
!         case, it may be possible to remedy the situation by not evalu-
!         ating the functions here, but instead setting the components
!         of FVEC to numbers that exceed those in the initial FVEC.
!
!       Excessive Number of Function Evaluations.  If the number of
!         calls to FCN reaches 100*(N+1) for IOPT=1 or 200*(N+1) for
!         IOPT=2, then this indicates that the routine is converging
!         very slowly as measured by the progress of FVEC, and INFO is
!         set to 2.  This situation should be unusual because, as
!         indicated below, lack of good progress is usually diagnosed
!         earlier by SNSQE, causing termination with INFO = 4.
!
!       Errors in the Functions.  When IOPT=2, the choice of step length
!         in the forward-difference approximation to the Jacobian
!         assumes that the relative errors in the functions are of the
!         order of the machine precision.  If this is not the case,
!         SNSQE may fail (usually with INFO = 4).  The user should
!         then either use SNSQ and set the step length or use IOPT=1
!         and supply the Jacobian.
!
!       Lack of Good Progress.  SNSQE searches for a zero of the system
!         by minimizing the sum of the squares of the functions.  In so
!         doing, it can become trapped in a region where the minimum
!         does not correspond to a zero of the system and, in this situ-
!         ation, the iteration eventually fails to make good progress.
!         In particular, this will happen if the system does not have a
!         zero.  If the system has a zero, rerunning SNSQE from a dif-
!         ferent starting point may be helpful.
!
!
! 6. Characteristics of the Algorithm.
!
!       SNSQE is a modification of the Powell hybrid method.  Two of
!       its main characteristics involve the choice of the correction as
!       a convex combination of the Newton and scaled gradient direc-
!       tions, and the updating of the Jacobian by the rank-1 method of
!       Broyden.  The choice of the correction guarantees (under reason-
!       able conditions) global convergence for starting points far from
!       the solution and a fast rate of convergence.  The Jacobian is
!       calculated at the starting point by either the user-supplied
!       subroutine or a forward-difference approximation, but it is not
!       recalculated until the rank-1 method fails to produce satis-
!       factory progress.
!
!       Timing.  The time required by SNSQE to solve a given problem
!         depends on N, the behavior of the functions, the accuracy
!         requested, and the starting point.  The number of arithmetic
!         operations needed by SNSQE is about 11.5*(N**2) to process
!         each evaluation of the functions (call to FCN) and 1.3*(N**3)
!         to process each evaluation of the Jacobian (call to JAC,
!         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
!         the timing of SNSQE will be strongly influenced by the time
!         spent in FCN and JAC.
!
!       Storage.  SNSQE requires (3*N**2 + 17*N)/2 single precision
!         storage locations, in addition to the storage required by the
!         program.  There are no internally declared storage arrays.
!
!
! 7. Example.
!
!       The problem is to determine the values of X(1), X(2), ..., X(9),
!       which solve the system of tridiagonal equations
!
!       (3-2*X(1))*X(1)           -2*X(2)                   = -1
!               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
!                                   -X(8) + (3-2*X(9))*X(9) = -1
!
!       **********
!
!       PROGRAM TEST(INPUT,OUTPUT,TAPE6=OUTPUT)
! C
! C     Driver for SNSQE example.
! C
!       INTEGER J,N,IOPT,NPRINT,INFO,LWA,NWRITE
!       REAL TOL,FNORM
!       REAL X(9),FVEC(9),WA(180)
!       REAL SNRM2,R1MACH
!       EXTERNAL FCN
!       DATA NWRITE /6/
! C
!       IOPT = 2
!       N = 9
! C
! C     The following starting values provide a rough solution.
! C
!       DO 10 J = 1, 9
!          X(J) = -1.E0
!    10    CONTINUE
!
!       LWA = 180
!       NPRINT = 0
! C
! C     Set TOL to the square root of the machine precision.
! C     Unless high precision solutions are required,
! C     this is the recommended setting.
! C
!       TOL = SQRT(R1MACH(4))
! C
!       CALL SNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
!       FNORM = SNRM2(N,FVEC)
!       WRITE (NWRITE,1000) FNORM,INFO,(X(J),J=1,N)
!       STOP
!  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
!      *        5X,' EXIT PARAMETER',16X,I10 //
!      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
!       END
!       SUBROUTINE FCN(N,X,FVEC,IFLAG)
!       INTEGER N,IFLAG
!       REAL X(N),FVEC(N)
!       INTEGER K
!       REAL ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
!       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
! C
!       DO 10 K = 1, N
!          TEMP = (THREE - TWO*X(K))*X(K)
!          TEMP1 = ZERO
!          IF (K .NE. 1) TEMP1 = X(K-1)
!          TEMP2 = ZERO
!          IF (K .NE. N) TEMP2 = X(K+1)
!          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
!    10    CONTINUE
!       RETURN
!       END
!
!       Results obtained with different compilers or machines
!       may be slightly different.
!
!       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
!
!       EXIT PARAMETER                         1
!
!       FINAL APPROXIMATE SOLUTION
!
!       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
!       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
!       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
!***REFERENCES  POWELL, M. J. D.
!                 A HYBRID METHOD FOR NONLINEAR EQUATIONS.
!                 NUMERICAL METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS,
!                 P. RABINOWITZ, EDITOR.  GORDON AND BREACH, 1970.
!***ROUTINES CALLED  SNSQ,XERROR
!***END PROLOGUE  SNSQE
      INTEGER IOPT,N,NPRINT,INFO,LWA
      REAL TOL
      REAL X(N),FVEC(N),WA(LWA),XDATA(NOBS)
!
!     NOTE 12/2009: NEW INTEL 11 COMPILER BALKS ON DECLARING JAC
!CCCC EXTERNAL FCN,JAC
      EXTERNAL FCN
      INTEGER INDEX,J,LR,MAXFEV,ML,MODE,MU,NFEV,NJEV
      REAL EPSFCN,FACTOR,ONE,XTOL,ZERO
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FACTOR,ONE,ZERO /1.0E2,1.0E0,0.0E0/
!***FIRST EXECUTABLE STATEMENT  SNSQE
      INFO = 0
!
!     CHECK THE INPUT PARAMETERS FOR ERRORS.
!
      IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0   &
          .OR. TOL .LT. ZERO .OR. LWA .LT. (3*N**2 +13*N)/2)   &
         GO TO 20
!
!     CALL SNSQ.
!
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
      CALL SNSQ(FCN,JAC,IOPT,N,X,FVEC,WA(INDEX+1),N,XTOL,MAXFEV,ML,MU,   &
                 EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,   &
                 WA(6*N+1),LR,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),   &
                 WA(5*N+1),   &
                 XDATA,NOBS)
      IF (INFO .EQ. 5) INFO = 4
   20 CONTINUE
      IF (INFO .EQ. 0) THEN
!CCCC    CALL XERROR( 'SNSQE  -- INVALID INPUT PARAMETER.'
!CCCC1,34,2,1)
        WRITE(ICOUT,11)
 11     FORMAT('***** ERROR IN SNSQE NON-LINEAR SIMULTANEOUS EQUATION ',   &
               'SOLVER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
 13     FORMAT('      INVALID INPUT PARAMETER.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
!
!     LAST CARD OF SUBROUTINE SNSQE.
!
      END SUBROUTINE SNSQE
      SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
      INTEGER N,LR
      REAL DELTA
      REAL R(LR),DIAG(N),QTB(N),X(N),WA1(N),WA2(N)
      INTEGER I,J,JJ,JP1,K,L
      REAL ALPHA,BNORM,EPSMCH,GNORM,ONE,QNORM,SGNORM,SUM,TEMP,ZERO
      REAL SNRM2
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!
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
      TEMP = TEMP - (DELTA/QNORM)*(SGNORM/DELTA)**2   &
             + SQRT((TEMP-(DELTA/QNORM))**2   &
                    +(ONE-(DELTA/QNORM)**2)*(ONE-(SGNORM/DELTA)**2))
      ALPHA = ((DELTA/QNORM)*(ONE - (SGNORM/DELTA)**2))/TEMP
  120 CONTINUE
      TEMP = (ONE - ALPHA)*AMIN1(SGNORM,DELTA)
      DO 130 J = 1, N
         X(J) = TEMP*WA1(J) + ALPHA*X(J)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END SUBROUTINE DOGLEG
      SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,   &
         WA1,WA2,   &
         XDATA,NOBS)
      INTEGER N,LDFJAC,IFLAG,ML,MU
      REAL EPSFCN
      REAL X(N),FVEC(N),FJAC(LDFJAC,N),WA1(N),WA2(N)
      REAL XDATA(NOBS)
      INTEGER I,J,K,MSUM
      REAL EPS,EPSMCH,H,TEMP,ZERO
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
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
                  IF (I .GE. J - MU .AND. I .LE. J + ML)   &
                     FJAC(I,J) = (WA1(I) - FVEC(I))/H
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
      RETURN
      END SUBROUTINE FDJAC1
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
      END SUBROUTINE QFORM
      SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
      INTEGER M,N,LDA,LIPVT
      INTEGER IPVT(LIPVT)
      LOGICAL PIVOT
      REAL A(LDA,N),SIGMA(N),ACNORM(N),WA(N)
      INTEGER I,J,JP1,K,KMAX,MINMN
      REAL AJNORM,EPSMCH,ONE,P05,SUM,TEMP,ZERO
      REAL SNRM2
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
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
      END SUBROUTINE QRFAC
      SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)
      INTEGER M,N,LDA
      REAL A(LDA,N),V(N),W(N)
      INTEGER I,J,NMJ,NM1
      REAL COS,ONE,SIN,TEMP
      DATA ONE /1.0E0/
!
      COS=0.0
      SIN=0.0
!
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
      END SUBROUTINE R1MPYQ
      SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
      INTEGER M,N,LS
      LOGICAL SING
      REAL S(LS),U(M),V(N),W(M)
      INTEGER I,J,JJ,L,NMJ,NM1
      REAL COS,COTAN,GIANT,ONE,P5,P25,SIN,TAN,TAU,TEMP,ZERO
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
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
      END SUBROUTINE R1UPDT
      SUBROUTINE SNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,   &
         MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,R,LR,QTF,WA1,   &
         WA2,WA3,WA4,   &
         XDATA,NOBS)
      INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,NJEV
      REAL XTOL,EPSFCN,FACTOR
      REAL X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),WA1(N),   &
           WA2(N),WA3(N),WA4(N)
      REAL XDATA(NOBS)
      EXTERNAL FCN
      INTEGER I,IFLAG,ITER,J,JM1,L,NCFAIL,NCSUC,NSLOW1,NSLOW2
      INTEGER IWA(1)
      LOGICAL JEVAL,SING
      REAL ACTRED,DELTA,EPSMCH,FNORM,FNORM1,ONE,PNORM,PRERED,P1,P5,   &
           P001,P0001,RATIO,SUM,TEMP,XNORM,ZERO
      REAL SNRM2
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ONE,P1,P5,P001,P0001,ZERO   &
           /1.0E0,1.0E-1,5.0E-1,1.0E-3,1.0E-4,0.0E0/
      XNORM = 0.0
      EPSMCH = R1MACH(4)
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
      IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR.   &
          N .LE. 0 .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0   &
          .OR. ML .LT. 0 .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO   &
          .OR. LDFJAC .LT. N .OR. LR .LT. (N*(N + 1))/2) GO TO 300
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
!CCCC       CALL JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
            NJEV = NJEV+1
            GO TO 32
   31       IFLAG = 2
            CALL FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,WA1,   &
                     WA2,   &
                     XDATA,NOBS)
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
            IF (MOD(ITER-1,NPRINT) .EQ. 0)   &
               CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
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
               IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)   &
                  DELTA = AMAX1(DELTA,PNORM/P5)
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
!
!  ERROR SECTION
!
      IF (INFO .LT. 0) THEN
!CCCC   CALL XERROR( 'SNSQ   -- EXECUTION TERMINATED BECA
!CCCC1USE USER SET IFLAG NEGATIVE.',63,1,1)
        WRITE(ICOUT,1001)
 1001   FORMAT('***** ERROR IN SNSQE NON-LINEAR SIMULTANEOUS EQUATION ',   &
               'SOLVER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1003)
 1003   FORMAT('      TERMINATION HALTED BECAUSE IFLAG IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 0) THEN
!CCCC   CALL XERROR( 'SNSQ   -- INVALID INPUT PARAMETER.',34,2,1)
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1004)
 1004   FORMAT('      INVALID INPUT PARAMETER.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 2) THEN
!CCCC   CALL XERROR( 'SNSQ   -- TOO MANY FUNCTION EVALUATIONS.',40,9,1)
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1005)
 1005   FORMAT('      TOO MANY FUNCTION EVALUATIONS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .EQ. 3) THEN
!CCCC   CALL XERROR( 'SNSQ   -- XTOL TOO SMALL. NO FURTHE
!CCCC1R IMPROVEMENT POSSIBLE.',58,3,1)
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1006)
 1006   FORMAT('      XTOL TOO SMALL.  NO FURTHER IMPROVEMENT ',   &
               'POSSIBLE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .GT. 4) THEN
!CCCC   CALL XERROR( 'SNSQ   -- ITERATION NOT MAKING GOOD
!CCCC1 PROGRESS.',45,1,1)
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1007)
 1007   FORMAT('      ITERATION NOT MAKING GOOD PROGRESS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE SNSQ
!DECK DNSQE
      SUBROUTINE DNSQE (FCN, JAC, IOPT, N, X, FVEC, TOL, NPRINT, INFO,   &
         WA, LWA,   &
         XDATA,NOBS)
!***BEGIN PROLOGUE  DNSQE
!***PURPOSE  An easy-to-use code to find a zero of a system of N
!            nonlinear functions in N variables by a modification of
!            the Powell hybrid method.
!***LIBRARY   SLATEC
!***CATEGORY  F2A
!***TYPE      DOUBLE PRECISION (SNSQE-S, DNSQE-D)
!***KEYWORDS  EASY-TO-USE, NONLINEAR SQUARE SYSTEM,
!             POWELL HYBRID METHOD, ZEROS
!***AUTHOR  Hiebert, K. L. (SNLA)
!***DESCRIPTION
!
! 1. Purpose.
!
!       The purpose of DNSQE is to find a zero of a system of N
!       nonlinear functions in N variables by a modification of the
!       Powell hybrid method.  This is done by using the more general
!       nonlinear equation solver DNSQ.  The user must provide a
!       subroutine which calculates the functions.  The user has the
!       option of either to provide a subroutine which calculates the
!       Jacobian or to let the code calculate it by a forward-difference
!       approximation.  This code is the combination of the MINPACK
!       codes (Argonne) HYBRD1 and HYBRJ1.
!
! 2. Subroutine and Type Statements.
!
!       SUBROUTINE DNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,
!      *                  WA,LWA)
!       INTEGER IOPT,N,NPRINT,INFO,LWA
!       DOUBLE PRECISION TOL
!       DOUBLE PRECISION X(N),FVEC(N),WA(LWA)
!       EXTERNAL FCN,JAC
!
! 3. Parameters.
!
!       Parameters designated as input parameters must be specified on
!       entry to DNSQE and are not changed on exit, while parameters
!       designated as output parameters need not be specified on entry
!       and are set to appropriate values on exit from DNSQE.
!
!       FCN is the name of the user-supplied subroutine which calculates
!         the functions.  FCN must be declared in an external statement
!         in the user calling program, and should be written as follows.
!
!         SUBROUTINE FCN(N,X,FVEC,IFLAG)
!         INTEGER N,IFLAG
!         DOUBLE PRECISION X(N),FVEC(N)
!         ----------
!         Calculate the functions at X and
!         return this vector in FVEC.
!         ----------
!         RETURN
!         END
!
!         The value of IFLAG should not be changed by FCN unless the
!         user wants to terminate execution of DNSQE.  In this case set
!         IFLAG to a negative integer.
!
!       JAC is the name of the user-supplied subroutine which calculates
!         the Jacobian.  If IOPT=1, then JAC must be declared in an
!         external statement in the user calling program, and should be
!         written as follows.
!
!         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
!         INTEGER N,LDFJAC,IFLAG
!         DOUBLE PRECISION X(N),FVEC(N),FJAC(LDFJAC,N)
!         ----------
!         Calculate the Jacobian at X and return this
!         matrix in FJAC.  FVEC contains the function
!         values at X and should not be altered.
!         ----------
!         RETURN
!         END
!
!         The value of IFLAG should not be changed by JAC unless the
!         user wants to terminate execution of DNSQE. In this case set
!         IFLAG to a negative integer.
!
!         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
!
!       IOPT is an input variable which specifies how the Jacobian will
!         be calculated.  If IOPT=1, then the user must supply the
!         Jacobian through the subroutine JAC.  If IOPT=2, then the
!         code will approximate the Jacobian by forward-differencing.
!
!       N is a positive integer input variable set to the number of
!         functions and variables.
!
!       X is an array of length N.  On input X must contain an initial
!         estimate of the solution vector.  On output X contains the
!         final estimate of the solution vector.
!
!       FVEC is an output array of length N which contains the functions
!         evaluated at the output X.
!
!       TOL is a nonnegative input variable.  Termination occurs when
!         the algorithm estimates that the relative error between X and
!         the solution is at most TOL.  Section 4 contains more details
!         about TOL.
!
!       NPRINT is an integer input variable that enables controlled
!         printing of iterates if it is positive.  In this case, FCN is
!         called with IFLAG = 0 at the beginning of the first iteration
!         and every NPRINT iterations thereafter and immediately prior
!         to return, with X and FVEC available for printing. Appropriate
!         print statements must be added to FCN(see example).  If NPRINT
!         is not positive, no special calls of FCN with IFLAG = 0 are
!         made.
!
!       INFO is an integer output variable.  If the user has terminated
!         execution, INFO is set to the (negative) value of IFLAG.  See
!         description of FCN and JAC. Otherwise, INFO is set as follows.
!
!         INFO = 0  Improper input parameters.
!
!         INFO = 1  Algorithm estimates that the relative error between
!                   X and the solution is at most TOL.
!
!         INFO = 2  Number of calls to FCN has reached or exceeded
!                   100*(N+1) for IOPT=1 or 200*(N+1) for IOPT=2.
!
!         INFO = 3  TOL is too small.  No further improvement in the
!                   approximate solution X is possible.
!
!         INFO = 4  Iteration is not making good progress.
!
!         Sections 4 and 5 contain more details about INFO.
!
!       WA is a work array of length LWA.
!
!       LWA is a positive integer input variable not less than
!         (3*N**2+13*N))/2.
!
! 4. Successful Completion.
!
!       The accuracy of DNSQE is controlled by the convergence parameter
!       TOL.  This parameter is used in a test which makes a comparison
!       between the approximation X and a solution XSOL.  DNSQE
!       terminates when the test is satisfied.  If TOL is less than the
!       machine precision (as defined by the  function D1MACH(4)), then
!       DNSQE only attempts to satisfy the test defined by the machine
!       precision.  Further progress is not usually possible.  Unless
!       high precision solutions are required, the recommended value
!       for TOL is the square root of the machine precision.
!
!       The test assumes that the functions are reasonably well behaved,
!       and, if the Jacobian is supplied by the user, that the functions
!       and the Jacobian are coded consistently. If these conditions are
!       not satisfied, then DNSQE may incorrectly indicate convergence.
!       The coding of the Jacobian can be checked by the subroutine
!       DCKDER.  If the Jacobian is coded correctly or IOPT=2, then
!       the validity of the answer can be checked, for example, by
!       rerunning DNSQE with a tighter tolerance.
!
!       Convergence Test.  If DENORM(Z) denotes the Euclidean norm of a
!         vector Z, then this test attempts to guarantee that
!
!               DENORM(X-XSOL) .LE. TOL*DENORM(XSOL).
!
!         If this condition is satisfied with TOL = 10**(-K), then the
!         larger components of X have K significant decimal digits and
!         INFO is set to 1.  There is a danger that the smaller
!         components of X may have large relative errors, but the fast
!         rate of convergence of DNSQE usually avoids this possibility.
!
! 5. Unsuccessful Completion.
!
!       Unsuccessful termination of DNSQE can be due to improper input
!       parameters, arithmetic interrupts, an excessive number of
!       function evaluations, errors in the functions, or lack of good
!       progress.
!
!       Improper Input Parameters.  INFO is set to 0 if IOPT .LT. 1, or
!         IOPT .GT. 2, or N .LE. 0, or TOL .LT. 0.E0, or
!         LWA .LT. (3*N**2+13*N)/2.
!
!       Arithmetic Interrupts.  If these interrupts occur in the FCN
!         subroutine during an early stage of the computation, they may
!         be caused by an unacceptable choice of X by DNSQE.  In this
!         case, it may be possible to remedy the situation by not
!         evaluating the functions here, but instead setting the
!         components of FVEC to numbers that exceed those in the initial
!         FVEC.
!
!       Excessive Number of Function Evaluations.  If the number of
!         calls to FCN reaches 100*(N+1) for IOPT=1 or 200*(N+1) for
!         IOPT=2, then this indicates that the routine is converging
!         very slowly as measured by the progress of FVEC, and INFO is
!         set to 2.  This situation should be unusual because, as
!         indicated below, lack of good progress is usually diagnosed
!         earlier by DNSQE, causing termination with INFO = 4.
!
!       Errors In the Functions.  When IOPT=2, the choice of step length
!         in the forward-difference approximation to the Jacobian
!         assumes that the relative errors in the functions are of the
!         order of the machine precision.  If this is not the case,
!         DNSQE may fail (usually with INFO = 4).  The user should
!         then either use DNSQ and set the step length or use IOPT=1
!         and supply the Jacobian.
!
!       Lack of Good Progress.  DNSQE searches for a zero of the system
!         by minimizing the sum of the squares of the functions.  In so
!         doing, it can become trapped in a region where the minimum
!         does not correspond to a zero of the system and, in this
!         situation, the iteration eventually fails to make good
!         progress.  In particular, this will happen if the system does
!         not have a zero.  If the system has a zero, rerunning DNSQE
!         from a different starting point may be helpful.
!
! 6. Characteristics of The Algorithm.
!
!       DNSQE is a modification of the Powell Hybrid method.  Two of
!       its main characteristics involve the choice of the correction as
!       a convex combination of the Newton and scaled gradient
!       directions, and the updating of the Jacobian by the rank-1
!       method of Broyden.  The choice of the correction guarantees
!       (under reasonable conditions) global convergence for starting
!       points far from the solution and a fast rate of convergence.
!       The Jacobian is calculated at the starting point by either the
!       user-supplied subroutine or a forward-difference approximation,
!       but it is not recalculated until the rank-1 method fails to
!       produce satisfactory progress.
!
!       Timing.  The time required by DNSQE to solve a given problem
!         depends on N, the behavior of the functions, the accuracy
!         requested, and the starting point.  The number of arithmetic
!         operations needed by DNSQE is about 11.5*(N**2) to process
!         each evaluation of the functions (call to FCN) and 1.3*(N**3)
!         to process each evaluation of the Jacobian (call to JAC,
!         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
!         the timing of DNSQE will be strongly influenced by the time
!         spent in FCN and JAC.
!
!       Storage.  DNSQE requires (3*N**2 + 17*N)/2 single precision
!         storage locations, in addition to the storage required by the
!         program.  There are no internally declared storage arrays.
!
! *Long Description:
!
! 7. Example.
!
!       The problem is to determine the values of X(1), X(2), ..., X(9),
!       which solve the system of tridiagonal equations
!
!       (3-2*X(1))*X(1)           -2*X(2)                   = -1
!               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
!                                   -X(8) + (3-2*X(9))*X(9) = -1
!
!       **********
!
!       PROGRAM TEST
! C
! C     DRIVER FOR DNSQE EXAMPLE.
! C
!       INTEGER J,N,IOPT,NPRINT,INFO,LWA,NWRITE
!       DOUBLE PRECISION TOL,FNORM
!       DOUBLE PRECISION X(9),FVEC(9),WA(180)
!       DOUBLE PRECISION DENORM,D1MACH
!       EXTERNAL FCN
!       DATA NWRITE /6/
! C
!       IOPT = 2
!       N = 9
! C
! C     THE FOLLOWING STARTING VALUES PROVIDE A ROUGH SOLUTION.
! C
!       DO 10 J = 1, 9
!          X(J) = -1.E0
!    10    CONTINUE
!
!       LWA = 180
!       NPRINT = 0
! C
! C     SET TOL TO THE SQUARE ROOT OF THE MACHINE PRECISION.
! C     UNLESS HIGH PRECISION SOLUTIONS ARE REQUIRED,
! C     THIS IS THE RECOMMENDED SETTING.
! C
!       TOL = SQRT(D1MACH(4))
! C
!       CALL DNSQE(FCN,JAC,IOPT,N,X,FVEC,TOL,NPRINT,INFO,WA,LWA)
!       FNORM = DENORM(N,FVEC)
!       WRITE (NWRITE,1000) FNORM,INFO,(X(J),J=1,N)
!       STOP
!  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
!      *        5X,' EXIT PARAMETER',16X,I10 //
!      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
!       END
!       SUBROUTINE FCN(N,X,FVEC,IFLAG)
!       INTEGER N,IFLAG
!       DOUBLE PRECISION X(N),FVEC(N)
!       INTEGER K
!       DOUBLE PRECISION ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
!       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
! C
!       DO 10 K = 1, N
!          TEMP = (THREE - TWO*X(K))*X(K)
!          TEMP1 = ZERO
!          IF (K .NE. 1) TEMP1 = X(K-1)
!          TEMP2 = ZERO
!          IF (K .NE. N) TEMP2 = X(K+1)
!          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
!    10    CONTINUE
!       RETURN
!       END
!
!       RESULTS OBTAINED WITH DIFFERENT COMPILERS OR MACHINES
!       MAY BE SLIGHTLY DIFFERENT.
!
!       FINAL L2 NORM OF THE RESIDUALS  0.1192636E-07
!
!       EXIT PARAMETER                         1
!
!       FINAL APPROXIMATE SOLUTION
!
!       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
!       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
!       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
!
!***REFERENCES  M. J. D. Powell, A hybrid method for nonlinear equa-
!                 tions. In Numerical Methods for Nonlinear Algebraic
!                 Equations, P. Rabinowitz, Editor.  Gordon and Breach,
!                 1988.
!***ROUTINES CALLED  DNSQ, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   890831  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DNSQE
      INTEGER INDEX, INFO, IOPT, J, LR, LWA, MAXFEV, ML, MODE, MU, N,   &
           NFEV, NJEV, NPRINT
      DOUBLE PRECISION EPSFCN, FACTOR, FVEC(*), ONE, TOL, WA(*),   &
           X(*), XTOL, ZERO
      REAL XDATA(NOBS)
!CCCC EXTERNAL FCN, JAC
      EXTERNAL FCN
      SAVE FACTOR, ONE, ZERO
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FACTOR,ONE,ZERO /1.0D2,1.0D0,0.0D0/
!     BEGIN BLOCK PERMITTING ...EXITS TO 20
!***FIRST EXECUTABLE STATEMENT  DNSQE
         INFO = 0
!
!        CHECK THE INPUT PARAMETERS FOR ERRORS.
!
!     ...EXIT
         IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0   &
             .OR. TOL .LT. ZERO .OR. LWA .LT. (3*N**2 + 13*N)/2)   &
            GO TO 20
!
!        CALL DNSQ.
!
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
         CALL DNSQ(FCN,JAC,IOPT,N,X,FVEC,WA(INDEX+1),N,XTOL,MAXFEV,ML,   &
                   MU,EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,   &
                   WA(6*N+1),LR,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),   &
                   WA(5*N+1),   &
                   XDATA,NOBS)
         IF (INFO .EQ. 5) INFO = 4
   20 CONTINUE
!CCCC IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNSQE',
!CCCC+   'INVALID INPUT PARAMETER.', 2, 1)
      IF (INFO .EQ. 0) THEN
        WRITE(ICOUT,11)
 11     FORMAT('***** ERROR IN DNSQE NON-LINEAR SIMULTANEOUS EQUATION ',   &
               'SOLVER--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
 13     FORMAT('      INVALID INPUT PARAMETER.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
!
!     LAST CARD OF SUBROUTINE DNSQE.
!
      END SUBROUTINE DNSQE 
!DECK DNSQ
      SUBROUTINE DNSQ (FCN, JAC, IOPT, N, X, FVEC, FJAC, LDFJAC, XTOL,   &
         MAXFEV, ML, MU, EPSFCN, DIAG, MODE, FACTOR, NPRINT, INFO, NFEV,   &
         NJEV, R, LR, QTF, WA1, WA2, WA3, WA4,   &
         XDATA,NOBS)
!***BEGIN PROLOGUE  DNSQ
!***PURPOSE  Find a zero of a system of a N nonlinear functions in N
!            variables by a modification of the Powell hybrid method.
!***LIBRARY   SLATEC
!***CATEGORY  F2A
!***TYPE      DOUBLE PRECISION (SNSQ-S, DNSQ-D)
!***KEYWORDS  NONLINEAR SQUARE SYSTEM, POWELL HYBRID METHOD, ZEROS
!***AUTHOR  Hiebert, K. L. (SNLA)
!***DESCRIPTION
!
! 1. Purpose.
!
!       The purpose of DNSQ is to find a zero of a system of N nonlinear
!       functions in N variables by a modification of the Powell
!       hybrid method.  The user must provide a subroutine which
!       calculates the functions.  The user has the option of either to
!       provide a subroutine which calculates the Jacobian or to let the
!       code calculate it by a forward-difference approximation.
!       This code is the combination of the MINPACK codes (Argonne)
!       HYBRD and HYBRDJ.
!
! 2. Subroutine and Type Statements.
!
!       SUBROUTINE DNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,
!      *                 ML,MU,EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,
!      *                 NJEV,R,LR,QTF,WA1,WA2,WA3,WA4)
!       INTEGER IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,NJEV,LR
!       DOUBLE PRECISION XTOL,EPSFCN,FACTOR
!       DOUBLE PRECISION
!       X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),
!      *     WA1(N),WA2(N),WA3(N),WA4(N)
!       EXTERNAL FCN,JAC
!
! 3. Parameters.
!
!       Parameters designated as input parameters must be specified on
!       entry to DNSQ and are not changed on exit, while parameters
!       designated as output parameters need not be specified on entry
!       and are set to appropriate values on exit from DNSQ.
!
!       FCN is the name of the user-supplied subroutine which calculates
!         the functions.  FCN must be declared in an EXTERNAL statement
!         in the user calling program, and should be written as follows.
!
!         SUBROUTINE FCN(N,X,FVEC,IFLAG)
!         INTEGER N,IFLAG
!         DOUBLE PRECISION X(N),FVEC(N)
!         ----------
!         CALCULATE THE FUNCTIONS AT X AND
!         RETURN THIS VECTOR IN FVEC.
!         ----------
!         RETURN
!         END
!
!         The value of IFLAG should not be changed by FCN unless the
!         user wants to terminate execution of DNSQ.  In this case set
!         IFLAG to a negative integer.
!
!       JAC is the name of the user-supplied subroutine which calculates
!         the Jacobian.  If IOPT=1, then JAC must be declared in an
!         EXTERNAL statement in the user calling program, and should be
!         written as follows.
!
!         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
!         INTEGER N,LDFJAC,IFLAG
!         DOUBLE PRECISION X(N),FVEC(N),FJAC(LDFJAC,N)
!         ----------
!         Calculate the Jacobian at X and return this
!         matrix in FJAC.  FVEC contains the function
!         values at X and should not be altered.
!         ----------
!         RETURN
!         END
!
!         The value of IFLAG should not be changed by JAC unless the
!         user wants to terminate execution of DNSQ.  In this case set
!         IFLAG to a negative integer.
!
!         If IOPT=2, JAC can be ignored (treat it as a dummy argument).
!
!       IOPT is an input variable which specifies how the Jacobian will
!         be calculated.  If IOPT=1, then the user must supply the
!         Jacobian through the subroutine JAC.  If IOPT=2, then the
!         code will approximate the Jacobian by forward-differencing.
!
!       N is a positive integer input variable set to the number of
!         functions and variables.
!
!       X is an array of length N.  On input X must contain an initial
!         estimate of the solution vector.  On output X contains the
!         final estimate of the solution vector.
!
!       FVEC is an output array of length N which contains the functions
!         evaluated at the output X.
!
!       FJAC is an output N by N array which contains the orthogonal
!         matrix Q produced by the QR factorization of the final
!         approximate Jacobian.
!
!       LDFJAC is a positive integer input variable not less than N
!         which specifies the leading dimension of the array FJAC.
!
!       XTOL is a nonnegative input variable.  Termination occurs when
!         the relative error between two consecutive iterates is at most
!         XTOL.  Therefore, XTOL measures the relative error desired in
!         the approximate solution.  Section 4 contains more details
!         about XTOL.
!
!       MAXFEV is a positive integer input variable.  Termination occurs
!         when the number of calls to FCN is at least MAXFEV by the end
!         of an iteration.
!
!       ML is a nonnegative integer input variable which specifies the
!         number of subdiagonals within the band of the Jacobian matrix.
!         If the Jacobian is not banded or IOPT=1, set ML to at
!         least N - 1.
!
!       MU is a nonnegative integer input variable which specifies the
!         number of superdiagonals within the band of the Jacobian
!         matrix.  If the Jacobian is not banded or IOPT=1, set MU to at
!         least N - 1.
!
!       EPSFCN is an input variable used in determining a suitable step
!         for the forward-difference approximation.  This approximation
!         assumes that the relative errors in the functions are of the
!         order of EPSFCN.  If EPSFCN is less than the machine
!         precision, it is assumed that the relative errors in the
!         functions are of the order of the machine precision.  If
!         IOPT=1, then EPSFCN can be ignored (treat it as a dummy
!         argument).
!
!       DIAG is an array of length N.  If MODE = 1 (see below), DIAG is
!         internally set.  If MODE = 2, DIAG must contain positive
!         entries that serve as implicit (multiplicative) scale factors
!         for the variables.
!
!       MODE is an integer input variable.  If MODE = 1, the variables
!         will be scaled internally.  If MODE = 2, the scaling is
!         specified by the input DIAG.  Other values of MODE are
!         equivalent to MODE = 1.
!
!       FACTOR is a positive input variable used in determining the
!         initial step bound.  This bound is set to the product of
!         FACTOR and the Euclidean norm of DIAG*X if nonzero, or else to
!         FACTOR itself.  In most cases FACTOR should lie in the
!         interval (.1,100.).  100. is a generally recommended value.
!
!       NPRINT is an integer input variable that enables controlled
!         printing of iterates if it is positive.  In this case, FCN is
!         called with IFLAG = 0 at the beginning of the first iteration
!         and every NPRINT iterations thereafter and immediately prior
!         to return, with X and FVEC available for printing. appropriate
!         print statements must be added to FCN(see example).  If NPRINT
!         is not positive, no special calls of FCN with IFLAG = 0 are
!         made.
!
!       INFO is an integer output variable.  If the user has terminated
!         execution, INFO is set to the (negative) value of IFLAG.  See
!         description of FCN and JAC. Otherwise, INFO is set as follows.
!
!         INFO = 0  Improper input parameters.
!
!         INFO = 1  Relative error between two consecutive iterates is
!                   at most XTOL.
!
!         INFO = 2  Number of calls to FCN has reached or exceeded
!                   MAXFEV.
!
!         INFO = 3  XTOL is too small.  No further improvement in the
!                   approximate solution X is possible.
!
!         INFO = 4  Iteration is not making good progress, as measured
!                   by the improvement from the last five Jacobian
!                   evaluations.
!
!         INFO = 5  Iteration is not making good progress, as measured
!                   by the improvement from the last ten iterations.
!
!         Sections 4 and 5 contain more details about INFO.
!
!       NFEV is an integer output variable set to the number of calls to
!         FCN.
!
!       NJEV is an integer output variable set to the number of calls to
!         JAC. (If IOPT=2, then NJEV is set to zero.)
!
!       R is an output array of length LR which contains the upper
!         triangular matrix produced by the QR factorization of the
!         final approximate Jacobian, stored rowwise.
!
!       LR is a positive integer input variable not less than
!         (N*(N+1))/2.
!
!       QTF is an output array of length N which contains the vector
!         (Q transpose)*FVEC.
!
!       WA1, WA2, WA3, and WA4 are work arrays of length N.
!
!
! 4. Successful completion.
!
!       The accuracy of DNSQ is controlled by the convergence parameter
!       XTOL.  This parameter is used in a test which makes a comparison
!       between the approximation X and a solution XSOL.  DNSQ
!       terminates when the test is satisfied.  If the convergence
!       parameter is less than the machine precision (as defined by the
!       function D1MACH(4)), then DNSQ only attempts to satisfy the test
!       defined by the machine precision.  Further progress is not
!       usually possible.
!
!       The test assumes that the functions are reasonably well behaved,
!       and, if the Jacobian is supplied by the user, that the functions
!       and the Jacobian are coded consistently.  If these conditions
!       are not satisfied, then DNSQ may incorrectly indicate
!       convergence.  The coding of the Jacobian can be checked by the
!       subroutine DCKDER. If the Jacobian is coded correctly or IOPT=2,
!       then the validity of the answer can be checked, for example, by
!       rerunning DNSQ with a tighter tolerance.
!
!       Convergence Test.  If DENORM(Z) denotes the Euclidean norm of a
!         vector Z and D is the diagonal matrix whose entries are
!         defined by the array DIAG, then this test attempts to
!         guarantee that
!
!               DENORM(D*(X-XSOL)) .LE. XTOL*DENORM(D*XSOL).
!
!         If this condition is satisfied with XTOL = 10**(-K), then the
!         larger components of D*X have K significant decimal digits and
!         INFO is set to 1.  There is a danger that the smaller
!         components of D*X may have large relative errors, but the fast
!         rate of convergence of DNSQ usually avoids this possibility.
!         Unless high precision solutions are required, the recommended
!         value for XTOL is the square root of the machine precision.
!
!
! 5. Unsuccessful Completion.
!
!       Unsuccessful termination of DNSQ can be due to improper input
!       parameters, arithmetic interrupts, an excessive number of
!       function evaluations, or lack of good progress.
!
!       Improper Input Parameters.  INFO is set to 0 if IOPT .LT .1,
!         or IOPT .GT. 2, or N .LE. 0, or LDFJAC .LT. N, or
!         XTOL .LT. 0.E0, or MAXFEV .LE. 0, or ML .LT. 0, or MU .LT. 0,
!         or FACTOR .LE. 0.E0, or LR .LT. (N*(N+1))/2.
!
!       Arithmetic Interrupts.  If these interrupts occur in the FCN
!         subroutine during an early stage of the computation, they may
!         be caused by an unacceptable choice of X by DNSQ.  In this
!         case, it may be possible to remedy the situation by rerunning
!         DNSQ with a smaller value of FACTOR.
!
!       Excessive Number of Function Evaluations.  A reasonable value
!         for MAXFEV is 100*(N+1) for IOPT=1 and 200*(N+1) for IOPT=2.
!         If the number of calls to FCN reaches MAXFEV, then this
!         indicates that the routine is converging very slowly as
!         measured by the progress of FVEC, and INFO is set to 2. This
!         situation should be unusual because, as indicated below, lack
!         of good progress is usually diagnosed earlier by DNSQ,
!         causing termination with info = 4 or INFO = 5.
!
!       Lack of Good Progress.  DNSQ searches for a zero of the system
!         by minimizing the sum of the squares of the functions.  In so
!         doing, it can become trapped in a region where the minimum
!         does not correspond to a zero of the system and, in this
!         situation, the iteration eventually fails to make good
!         progress.  In particular, this will happen if the system does
!         not have a zero.  If the system has a zero, rerunning DNSQ
!         from a different starting point may be helpful.
!
!
! 6. Characteristics of The Algorithm.
!
!       DNSQ is a modification of the Powell Hybrid method.  Two of its
!       main characteristics involve the choice of the correction as a
!       convex combination of the Newton and scaled gradient directions,
!       and the updating of the Jacobian by the rank-1 method of
!       Broyden.  The choice of the correction guarantees (under
!       reasonable conditions) global convergence for starting points
!       far from the solution and a fast rate of convergence.  The
!       Jacobian is calculated at the starting point by either the
!       user-supplied subroutine or a forward-difference approximation,
!       but it is not recalculated until the rank-1 method fails to
!       produce satisfactory progress.
!
!       Timing.  The time required by DNSQ to solve a given problem
!         depends on N, the behavior of the functions, the accuracy
!         requested, and the starting point.  The number of arithmetic
!         operations needed by DNSQ is about 11.5*(N**2) to process
!         each evaluation of the functions (call to FCN) and 1.3*(N**3)
!         to process each evaluation of the Jacobian (call to JAC,
!         if IOPT = 1).  Unless FCN and JAC can be evaluated quickly,
!         the timing of DNSQ will be strongly influenced by the time
!         spent in FCN and JAC.
!
!       Storage.  DNSQ requires (3*N**2 + 17*N)/2 single precision
!         storage locations, in addition to the storage required by the
!         program.  There are no internally declared storage arrays.
!
! *Long Description:
!
! 7. Example.
!
!       The problem is to determine the values of X(1), X(2), ..., X(9),
!       which solve the system of tridiagonal equations
!
!       (3-2*X(1))*X(1)           -2*X(2)                   = -1
!               -X(I-1) + (3-2*X(I))*X(I)         -2*X(I+1) = -1, I=2-8
!                                   -X(8) + (3-2*X(9))*X(9) = -1
! C     **********
!
!       PROGRAM TEST
! C
! C     Driver for DNSQ example.
! C
!       INTEGER J,IOPT,N,MAXFEV,ML,MU,MODE,NPRINT,INFO,NFEV,LDFJAC,LR,
!      *        NWRITE
!       DOUBLE PRECISION XTOL,EPSFCN,FACTOR,FNORM
!       DOUBLE PRECISION X(9),FVEC(9),DIAG(9),FJAC(9,9),R(45),QTF(9),
!      *     WA1(9),WA2(9),WA3(9),WA4(9)
!       DOUBLE PRECISION DENORM,D1MACH
!       EXTERNAL FCN
!       DATA NWRITE /6/
! C
!       IOPT = 2
!       N = 9
! C
! C     THE FOLLOWING STARTING VALUES PROVIDE A ROUGH SOLUTION.
! C
!       DO 10 J = 1, 9
!          X(J) = -1.E0
!    10    CONTINUE
! C
!       LDFJAC = 9
!       LR = 45
! C
! C     SET XTOL TO THE SQUARE ROOT OF THE MACHINE PRECISION.
! C     UNLESS HIGH PRECISION SOLUTIONS ARE REQUIRED,
! C     THIS IS THE RECOMMENDED SETTING.
! C
!       XTOL = SQRT(D1MACH(4))
! C
!       MAXFEV = 2000
!       ML = 1
!       MU = 1
!       EPSFCN = 0.E0
!       MODE = 2
!       DO 20 J = 1, 9
!          DIAG(J) = 1.E0
!    20    CONTINUE
!       FACTOR = 1.E2
!       NPRINT = 0
! C
!       CALL DNSQ(FCN,JAC,IOPT,N,X,FVEC,FJAC,LDFJAC,XTOL,MAXFEV,ML,MU,
!      *           EPSFCN,DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
!      *           R,LR,QTF,WA1,WA2,WA3,WA4)
!       FNORM = DENORM(N,FVEC)
!       WRITE (NWRITE,1000) FNORM,NFEV,INFO,(X(J),J=1,N)
!       STOP
!  1000 FORMAT (5X,' FINAL L2 NORM OF THE RESIDUALS',E15.7 //
!      *        5X,' NUMBER OF FUNCTION EVALUATIONS',I10 //
!      *        5X,' EXIT PARAMETER',16X,I10 //
!      *        5X,' FINAL APPROXIMATE SOLUTION' // (5X,3E15.7))
!       END
!       SUBROUTINE FCN(N,X,FVEC,IFLAG)
!       INTEGER N,IFLAG
!       DOUBLE PRECISION X(N),FVEC(N)
!       INTEGER K
!       DOUBLE PRECISION ONE,TEMP,TEMP1,TEMP2,THREE,TWO,ZERO
!       DATA ZERO,ONE,TWO,THREE /0.E0,1.E0,2.E0,3.E0/
! C
!       IF (IFLAG .NE. 0) GO TO 5
! C
! C     INSERT PRINT STATEMENTS HERE WHEN NPRINT IS POSITIVE.
! C
!       RETURN
!     5 CONTINUE
!       DO 10 K = 1, N
!          TEMP = (THREE - TWO*X(K))*X(K)
!          TEMP1 = ZERO
!          IF (K .NE. 1) TEMP1 = X(K-1)
!          TEMP2 = ZERO
!          IF (K .NE. N) TEMP2 = X(K+1)
!          FVEC(K) = TEMP - TEMP1 - TWO*TEMP2 + ONE
!    10    CONTINUE
!       RETURN
!       END
!
!       Results obtained with different compilers or machines
!       may be slightly different.
!
!       Final L2 norm of the residuals  0.1192636E-07
!
!       Number of function evaluations        14
!
!       Exit parameter                         1
!
!       Final approximate solution
!
!       -0.5706545E+00 -0.6816283E+00 -0.7017325E+00
!       -0.7042129E+00 -0.7013690E+00 -0.6918656E+00
!       -0.6657920E+00 -0.5960342E+00 -0.4164121E+00
!
!***REFERENCES  M. J. D. Powell, A hybrid method for nonlinear equa-
!                 tions. In Numerical Methods for Nonlinear Algebraic
!                 Equations, P. Rabinowitz, Editor.  Gordon and Breach,
!                 1988.
!***ROUTINES CALLED  D1MACH, D1MPYQ, D1UPDT, DDOGLG, DENORM, DFDJC1,
!                    DQFORM, DQRFAC, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   890831  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DNSQ
!CCCC DOUBLE PRECISION D1MACH,DENORM
      DOUBLE PRECISION DENORM
      INTEGER I, IFLAG, INFO, IOPT, ITER, IWA(1), J, JM1, L, LDFJAC,   &
           LR, MAXFEV, ML, MODE, MU, N, NCFAIL, NCSUC, NFEV, NJEV,   &
           NPRINT, NSLOW1, NSLOW2
      DOUBLE PRECISION ACTRED, DELTA, DIAG(*), EPSFCN, EPSMCH, FACTOR,   &
           FJAC(LDFJAC,*), FNORM, FNORM1, FVEC(*), ONE, P0001, P001,   &
           P1, P5, PNORM, PRERED, QTF(*), R(*), RATIO, SUM, TEMP,   &
           WA1(*), WA2(*), WA3(*), WA4(*), X(*), XNORM, XTOL, ZERO
      REAL XDATA(NOBS)
      EXTERNAL FCN
      LOGICAL JEVAL,SING
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      SAVE ONE, P1, P5, P001, P0001, ZERO
      DATA ONE,P1,P5,P001,P0001,ZERO   &
           /1.0D0,1.0D-1,5.0D-1,1.0D-3,1.0D-4,0.0D0/
!
!     BEGIN BLOCK PERMITTING ...EXITS TO 320
!***FIRST EXECUTABLE STATEMENT  DNSQ
         XNORM = 0.0D0
         EPSMCH = D1MACH(4)
!
         INFO = 0
         IFLAG = 0
         NFEV = 0
         NJEV = 0
!
!        CHECK THE INPUT PARAMETERS FOR ERRORS.
!
!     ...EXIT
         IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0   &
             .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0 .OR. ML .LT. 0   &
             .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO .OR. LDFJAC .LT. N   &
             .OR. LR .LT. (N*(N + 1))/2) GO TO 320
         IF (MODE .NE. 2) GO TO 20
            DO 10 J = 1, N
!     .........EXIT
               IF (DIAG(J) .LE. ZERO) GO TO 320
   10       CONTINUE
   20    CONTINUE
!
!        EVALUATE THE FUNCTION AT THE STARTING POINT
!        AND CALCULATE ITS NORM.
!
         IFLAG = 1
         CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
         NFEV = 1
!     ...EXIT
         IF (IFLAG .LT. 0) GO TO 320
         FNORM = DENORM(N,FVEC)
!
!        INITIALIZE ITERATION COUNTER AND MONITORS.
!
         ITER = 1
         NCSUC = 0
         NCFAIL = 0
         NSLOW1 = 0
         NSLOW2 = 0
!
!        BEGINNING OF THE OUTER LOOP.
!
   30    CONTINUE
!           BEGIN BLOCK PERMITTING ...EXITS TO 90
               JEVAL = .TRUE.
!
!              CALCULATE THE JACOBIAN MATRIX.
!
               IF (IOPT .EQ. 2) GO TO 40
!
!                 USER SUPPLIES JACOBIAN
!
!CCCC             CALL JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
                  NJEV = NJEV + 1
               GO TO 50
   40          CONTINUE
!
!                 CODE APPROXIMATES THE JACOBIAN
!
                  IFLAG = 2
                  CALL DFDJC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,   &
                              EPSFCN,WA1,WA2,XDATA,NOBS)
                  NFEV = NFEV + MIN(ML+MU+1,N)
   50          CONTINUE
!
!     .........EXIT
               IF (IFLAG .LT. 0) GO TO 320
!
!              COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
!
               CALL DQRFAC(N,N,FJAC,LDFJAC,.FALSE.,IWA,1,WA1,WA2,WA3)
!
!              ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
!              TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
!
!           ...EXIT
               IF (ITER .NE. 1) GO TO 90
               IF (MODE .EQ. 2) GO TO 70
                  DO 60 J = 1, N
                     DIAG(J) = WA2(J)
                     IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
   60             CONTINUE
   70          CONTINUE
!
!              ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED
!              X AND INITIALIZE THE STEP BOUND DELTA.
!
               DO 80 J = 1, N
                  WA3(J) = DIAG(J)*X(J)
   80          CONTINUE
               XNORM = DENORM(N,WA3)
               DELTA = FACTOR*XNORM
               IF (DELTA .EQ. ZERO) DELTA = FACTOR
   90       CONTINUE
!
!           FORM (Q TRANSPOSE)*FVEC AND STORE IN QTF.
!
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
!
!           COPY THE TRIANGULAR FACTOR OF THE QR FACTORIZATION INTO R.
!
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
!
!           ACCUMULATE THE ORTHOGONAL FACTOR IN FJAC.
!
            CALL DQFORM(N,N,FJAC,LDFJAC,WA1)
!
!           RESCALE IF NECESSARY.
!
            IF (MODE .EQ. 2) GO TO 190
               DO 180 J = 1, N
                  DIAG(J) = MAX(DIAG(J),WA2(J))
  180          CONTINUE
  190       CONTINUE
!
!           BEGINNING OF THE INNER LOOP.
!
  200       CONTINUE
!
!              IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
!
               IF (NPRINT .LE. 0) GO TO 210
                  IFLAG = 0
                  IF (MOD(ITER-1,NPRINT) .EQ. 0)   &
                     CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
!     ............EXIT
                  IF (IFLAG .LT. 0) GO TO 320
  210          CONTINUE
!
!              DETERMINE THE DIRECTION P.
!
               CALL DDOGLG(N,R,LR,DIAG,QTF,DELTA,WA1,WA2,WA3)
!
!              STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
!
               DO 220 J = 1, N
                  WA1(J) = -WA1(J)
                  WA2(J) = X(J) + WA1(J)
                  WA3(J) = DIAG(J)*WA1(J)
  220          CONTINUE
               PNORM = DENORM(N,WA3)
!
!              ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
!
               IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)
!
!              EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.
!
               IFLAG = 1
               CALL FCN(N,WA2,WA4,IFLAG,XDATA,NOBS)
               NFEV = NFEV + 1
!     .........EXIT
               IF (IFLAG .LT. 0) GO TO 320
               FNORM1 = DENORM(N,WA4)
!
!              COMPUTE THE SCALED ACTUAL REDUCTION.
!
               ACTRED = -ONE
               IF (FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
!
!              COMPUTE THE SCALED PREDICTED REDUCTION.
!
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
!
!              COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
!              REDUCTION.
!
               RATIO = ZERO
               IF (PRERED .GT. ZERO) RATIO = ACTRED/PRERED
!
!              UPDATE THE STEP BOUND.
!
               IF (RATIO .GE. P1) GO TO 250
                  NCSUC = 0
                  NCFAIL = NCFAIL + 1
                  DELTA = P5*DELTA
               GO TO 260
  250          CONTINUE
                  NCFAIL = 0
                  NCSUC = NCSUC + 1
                  IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)   &
                     DELTA = MAX(DELTA,PNORM/P5)
                  IF (ABS(RATIO-ONE) .LE. P1) DELTA = PNORM/P5
  260          CONTINUE
!
!              TEST FOR SUCCESSFUL ITERATION.
!
               IF (RATIO .LT. P0001) GO TO 280
!
!                 SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
!
                  DO 270 J = 1, N
                     X(J) = WA2(J)
                     WA2(J) = DIAG(J)*X(J)
                     FVEC(J) = WA4(J)
  270             CONTINUE
                  XNORM = DENORM(N,WA2)
                  FNORM = FNORM1
                  ITER = ITER + 1
  280          CONTINUE
!
!              DETERMINE THE PROGRESS OF THE ITERATION.
!
               NSLOW1 = NSLOW1 + 1
               IF (ACTRED .GE. P001) NSLOW1 = 0
               IF (JEVAL) NSLOW2 = NSLOW2 + 1
               IF (ACTRED .GE. P1) NSLOW2 = 0
!
!              TEST FOR CONVERGENCE.
!
               IF (DELTA .LE. XTOL*XNORM .OR. FNORM .EQ. ZERO) INFO = 1
!     .........EXIT
               IF (INFO .NE. 0) GO TO 320
!
!              TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
!
               IF (NFEV .GE. MAXFEV) INFO = 2
               IF (P1*MAX(P1*DELTA,PNORM) .LE. EPSMCH*XNORM) INFO = 3
               IF (NSLOW2 .EQ. 5) INFO = 4
               IF (NSLOW1 .EQ. 10) INFO = 5
!     .........EXIT
               IF (INFO .NE. 0) GO TO 320
!
!              CRITERION FOR RECALCULATING JACOBIAN
!
!           ...EXIT
               IF (NCFAIL .EQ. 2) GO TO 310
!
!              CALCULATE THE RANK ONE MODIFICATION TO THE JACOBIAN
!              AND UPDATE QTF IF NECESSARY.
!
               DO 300 J = 1, N
                  SUM = ZERO
                  DO 290 I = 1, N
                     SUM = SUM + FJAC(I,J)*WA4(I)
  290             CONTINUE
                  WA2(J) = (SUM - WA3(J))/PNORM
                  WA1(J) = DIAG(J)*((DIAG(J)*WA1(J))/PNORM)
                  IF (RATIO .GE. P0001) QTF(J) = SUM
  300          CONTINUE
!
!              COMPUTE THE QR FACTORIZATION OF THE UPDATED JACOBIAN.
!
               CALL D1UPDT(N,N,R,LR,WA1,WA2,WA3,SING)
               CALL D1MPYQ(N,N,FJAC,LDFJAC,WA2,WA3)
               CALL D1MPYQ(1,N,QTF,1,WA2,WA3)
!
!              END OF THE INNER LOOP.
!
               JEVAL = .FALSE.
            GO TO 200
  310       CONTINUE
!
!           END OF THE OUTER LOOP.
!
         GO TO 30
  320 CONTINUE
!
!     TERMINATION, EITHER NORMAL OR USER IMPOSED.
!
      IF (IFLAG .LT. 0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT .GT. 0) CALL FCN(N,X,FVEC,IFLAG,XDATA,NOBS)
!CCCC IF (INFO .LT. 0) CALL XERMSG ('SLATEC', 'DNSQ',
!CCCC+   'EXECUTION TERMINATED BECAUSE USER SET IFLAG NEGATIVE.', 1, 1)
!CCCC IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNSQ',
!CCCC+   'INVALID INPUT PARAMETER.', 2, 1)
!CCCC IF (INFO .EQ. 2) CALL XERMSG ('SLATEC', 'DNSQ',
!CCCC+   'TOO MANY FUNCTION EVALUATIONS.', 9, 1)
!CCCC IF (INFO .EQ. 3) CALL XERMSG ('SLATEC', 'DNSQ',
!CCCC+   'XTOL TOO SMALL. NO FURTHER IMPROVEMENT POSSIBLE.', 3, 1)
!CCCC IF (INFO .GT. 4) CALL XERMSG ('SLATEC', 'DNSQ',
!CCCC+   'ITERATION NOT MAKING GOOD PROGRESS.', 1, 1)
      IF (INFO .LT. 0) THEN
        WRITE(ICOUT,1001)
 1001   FORMAT('***** ERROR IN DNSQE NON-LINEAR SIMULTANEOUS EQUATION ',   &
               'SOLVER--')
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
 1006   FORMAT('      XTOL TOO SMALL.  NO FURTHER IMPROVEMENT ',   &
               'POSSIBLE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF (INFO .GT. 4) THEN
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1007)
 1007   FORMAT('      ITERATION NOT MAKING GOOD PROGRESS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
!
!     LAST CARD OF SUBROUTINE DNSQ.
!
      END SUBROUTINE DNSQ 
!DECK DFDJC1
      SUBROUTINE DFDJC1 (FCN, N, X, FVEC, FJAC, LDFJAC, IFLAG, ML, MU,   &
         EPSFCN, WA1, WA2,   &
         XDATA,NOBS)
!***BEGIN PROLOGUE  DFDJC1
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DNSQ and DNSQE
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (FDJAC1-S, DFDJC1-D)
!***AUTHOR  (UNKNOWN)
!***DESCRIPTION
!
!     This subroutine computes a forward-difference approximation
!     to the N by N Jacobian matrix associated with a specified
!     problem of N functions in N variables. If the Jacobian has
!     a banded form, then function evaluations are saved by only
!     approximating the nonzero terms.
!
!     The subroutine statement is
!
!       SUBROUTINE DFDJC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,
!                         WA1,WA2)
!
!     where
!
!       FCN is the name of the user-supplied subroutine which
!         calculates the functions. FCN must be declared
!         in an EXTERNAL statement in the user calling
!         program, and should be written as follows.
!
!         SUBROUTINE FCN(N,X,FVEC,IFLAG)
!         INTEGER N,IFLAG
!         DOUBLE PRECISION X(N),FVEC(N)
!         ----------
!         Calculate the functions at X and
!         return this vector in FVEC.
!         ----------
!         RETURN
!
!         The value of IFLAG should not be changed by FCN unless
!         the user wants to terminate execution of DFDJC1.
!         In this case set IFLAG to a negative integer.
!
!       N is a positive integer input variable set to the number
!         of functions and variables.
!
!       X is an input array of length N.
!
!       FVEC is an input array of length N which must contain the
!         functions evaluated at X.
!
!       FJAC is an output N by N array which contains the
!         approximation to the Jacobian matrix evaluated at X.
!
!       LDFJAC is a positive integer input variable not less than N
!         which specifies the leading dimension of the array FJAC.
!
!       IFLAG is an integer variable which can be used to terminate
!         the execution of DFDJC1. See description of FCN.
!
!       ML is a nonnegative integer input variable which specifies
!         the number of subdiagonals within the band of the
!         Jacobian matrix. If the Jacobian is not banded, set
!         ML to at least N - 1.
!
!       EPSFCN is an input variable used in determining a suitable
!         step length for the forward-difference approximation. This
!         approximation assumes that the relative errors in the
!         functions are of the order of EPSFCN. If EPSFCN is less
!         than the machine precision, it is assumed that the relative
!         errors in the functions are of the order of the machine
!         precision.
!
!       MU is a nonnegative integer input variable which specifies
!         the number of superdiagonals within the band of the
!         Jacobian matrix. If the Jacobian is not banded, set
!         MU to at least N - 1.
!
!       WA1 and WA2 are work arrays of length N. If ML + MU + 1 is at
!         least N, then the Jacobian is considered dense, and WA2 is
!         not referenced.
!
!***SEE ALSO  DNSQ, DNSQE
!***ROUTINES CALLED  D1MACH
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   900328  Added TYPE section.  (WRB)
!***END PROLOGUE  DFDJC1
!CCCC DOUBLE PRECISION D1MACH
      INTEGER I, IFLAG, J, K, LDFJAC, ML, MSUM, MU, N
      DOUBLE PRECISION EPS, EPSFCN, EPSMCH, FJAC(LDFJAC,*),   &
           FVEC(*), H, TEMP, WA1(*), WA2(*), X(*), ZERO
      SAVE ZERO
!
      REAL XDATA(NOBS)
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ZERO /0.0D0/
!
!     EPSMCH IS THE MACHINE PRECISION.
!
!***FIRST EXECUTABLE STATEMENT  DFDJC1
      EPSMCH = D1MACH(4)
!
      EPS = SQRT(MAX(EPSFCN,EPSMCH))
      MSUM = ML + MU + 1
      IF (MSUM .LT. N) GO TO 40
!
!        COMPUTATION OF DENSE APPROXIMATE JACOBIAN.
!
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
!
!        COMPUTATION OF BANDED APPROXIMATE JACOBIAN.
!
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
                  IF (I .GE. J - MU .AND. I .LE. J + ML)   &
                     FJAC(I,J) = (WA1(I) - FVEC(I))/H
   70             CONTINUE
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
      RETURN
!
!     LAST CARD OF SUBROUTINE DFDJC1.
!
      END SUBROUTINE DFDJC1 
!DECK DQRFAC
      SUBROUTINE DQRFAC (M, N, A, LDA, PIVOT, IPVT, LIPVT, SIGMA,   &
         ACNORM, WA)
!***BEGIN PROLOGUE  DQRFAC
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DNLS1, DNLS1E, DNSQ and DNSQE
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (QRFAC-S, DQRFAC-D)
!***AUTHOR  (UNKNOWN)
!***DESCRIPTION
!
!   **** Double Precision version of QRFAC ****
!
!     This subroutine uses Householder transformations with column
!     pivoting (optional) to compute a QR factorization of the
!     M by N matrix A. That is, DQRFAC determines an orthogonal
!     matrix Q, a permutation matrix P, and an upper trapezoidal
!     matrix R with diagonal elements of nonincreasing magnitude,
!     such that A*P = Q*R. The Householder transformation for
!     column K, K = 1,2,...,MIN(M,N), is of the form
!
!                           T
!           I - (1/U(K))*U*U
!
!     where U has zeros in the first K-1 positions. The form of
!     this transformation and the method of pivoting first
!     appeared in the corresponding LINPACK subroutine.
!
!     The subroutine statement is
!
!       SUBROUTINE DQRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,SIGMA,ACNORM,WA)
!
!     where
!
!       M is a positive integer input variable set to the number
!         of rows of A.
!
!       N is a positive integer input variable set to the number
!         of columns of A.
!
!       A is an M by N array. On input A contains the matrix for
!         which the QR factorization is to be computed. On output
!         the strict upper trapezoidal part of A contains the strict
!         upper trapezoidal part of R, and the lower trapezoidal
!         part of A contains a factored form of Q (the non-trivial
!         elements of the U vectors described above).
!
!       LDA is a positive integer input variable not less than M
!         which specifies the leading dimension of the array A.
!
!       PIVOT is a logical input variable. If pivot is set .TRUE.,
!         then column pivoting is enforced. If pivot is set .FALSE.,
!         then no column pivoting is done.
!
!       IPVT is an integer output array of length LIPVT. IPVT
!         defines the permutation matrix P such that A*P = Q*R.
!         Column J of P is column IPVT(J) of the identity matrix.
!         If pivot is .FALSE., IPVT is not referenced.
!
!       LIPVT is a positive integer input variable. If PIVOT is
!             .FALSE., then LIPVT may be as small as 1. If PIVOT is
!             .TRUE., then LIPVT must be at least N.
!
!       SIGMA is an output array of length N which contains the
!         diagonal elements of R.
!
!       ACNORM is an output array of length N which contains the
!         norms of the corresponding columns of the input matrix A.
!         If this information is not needed, then ACNORM can coincide
!         with SIGMA.
!
!       WA is a work array of length N. If pivot is .FALSE., then WA
!         can coincide with SIGMA.
!
!***SEE ALSO  DNLS1, DNLS1E, DNSQ, DNSQE
!***ROUTINES CALLED  D1MACH, DENORM
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   900328  Added TYPE section.  (WRB)
!***END PROLOGUE  DQRFAC
      INTEGER M,N,LDA,LIPVT
      INTEGER IPVT(*)
      LOGICAL PIVOT
      SAVE ONE, P05, ZERO
      DOUBLE PRECISION A(LDA,*),SIGMA(*),ACNORM(*),WA(*)
      INTEGER I,J,JP1,K,KMAX,MINMN
      DOUBLE PRECISION AJNORM,EPSMCH,ONE,P05,SUM,TEMP,ZERO
!CCCC DOUBLE PRECISION D1MACH,DENORM
      DOUBLE PRECISION DENORM
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ONE,P05,ZERO /1.0D0,5.0D-2,0.0D0/
!***FIRST EXECUTABLE STATEMENT  DQRFAC
!
      IF(ISUBG4.EQ.'RFAC')THEN
        WRITE(ICOUT,9052)LIPVT
 9052   FORMAT('LIPVT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      EPSMCH = D1MACH(4)
!
!     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.
!
      DO 10 J = 1, N
         ACNORM(J) = DENORM(M,A(1,J))
         SIGMA(J) = ACNORM(J)
         WA(J) = SIGMA(J)
         IF (PIVOT) IPVT(J) = J
   10    CONTINUE
!
!     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.
!
      MINMN = MIN(M,N)
      DO 110 J = 1, MINMN
         IF (.NOT.PIVOT) GO TO 40
!
!        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.
!
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
!
!        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE
!        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.
!
         AJNORM = DENORM(M-J+1,A(J,J))
         IF (AJNORM .EQ. ZERO) GO TO 100
         IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM
         DO 50 I = J, M
            A(I,J) = A(I,J)/AJNORM
   50       CONTINUE
         A(J,J) = A(J,J) + ONE
!
!        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS
!        AND UPDATE THE NORMS.
!
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
!
!     LAST CARD OF SUBROUTINE DQRFAC.
!
      END SUBROUTINE DQRFAC 
!DECK DENORM
      DOUBLE PRECISION FUNCTION DENORM (N, X)
!***BEGIN PROLOGUE  DENORM
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DNSQ and DNSQE
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (ENORM-S, DENORM-D)
!***AUTHOR  (UNKNOWN)
!***DESCRIPTION
!
!     Given an N-vector X, this function calculates the
!     Euclidean norm of X.
!
!     The Euclidean norm is computed by accumulating the sum of
!     squares in three different sums. The sums of squares for the
!     small and large components are scaled so that no overflows
!     occur. Non-destructive underflows are permitted. Underflows
!     and overflows do not occur in the computation of the unscaled
!     sum of squares for the intermediate components.
!     The definitions of small, intermediate and large components
!     depend on two constants, RDWARF and RGIANT. The main
!     restrictions on these constants are that RDWARF**2 not
!     underflow and RGIANT**2 not overflow. The constants
!     given here are suitable for every known computer.
!
!     The function statement is
!
!       DOUBLE PRECISION FUNCTION DENORM(N,X)
!
!     where
!
!       N is a positive integer input variable.
!
!       X is an input array of length N.
!
!***SEE ALSO  DNSQ, DNSQE
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   900328  Added TYPE section.  (WRB)
!***END PROLOGUE  DENORM
      INTEGER I, N
      DOUBLE PRECISION AGIANT, FLOATN, ONE, RDWARF, RGIANT, S1, S2, S3,   &
           X(*), X1MAX, X3MAX, XABS, ZERO
      SAVE ONE, ZERO, RDWARF, RGIANT
      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
!
      DENORM = ZERO
!
!***FIRST EXECUTABLE STATEMENT  DENORM
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
!
!              SUM FOR LARGE COMPONENTS.
!
               IF (XABS .LE. X1MAX) GO TO 10
                  S1 = ONE + S1*(X1MAX/XABS)**2
                  X1MAX = XABS
                  GO TO 20
   10          CONTINUE
                  S1 = S1 + (XABS/X1MAX)**2
   20          CONTINUE
               GO TO 60
   30       CONTINUE
!
!              SUM FOR SMALL COMPONENTS.
!
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
!
!           SUM FOR INTERMEDIATE COMPONENTS.
!
            S2 = S2 + XABS**2
   80    CONTINUE
   90    CONTINUE
!
!     CALCULATION OF NORM.
!
      IF (S1 .EQ. ZERO) GO TO 100
         DENORM = X1MAX*SQRT(S1+(S2/X1MAX)/X1MAX)
         GO TO 130
  100 CONTINUE
         IF (S2 .EQ. ZERO) GO TO 110
            IF (S2 .GE. X3MAX)   &
               DENORM = SQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
            IF (S2 .LT. X3MAX)   &
               DENORM = SQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
            GO TO 120
  110    CONTINUE
            DENORM = X3MAX*SQRT(S3)
  120    CONTINUE
  130 CONTINUE
      RETURN
!
!     LAST CARD OF FUNCTION DENORM.
!
      END FUNCTION DENORM 
!DECK DQFORM
      SUBROUTINE DQFORM (M, N, Q, LDQ, WA)
!***BEGIN PROLOGUE  DQFORM
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DNSQ and DNSQE
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (QFORM-S, DQFORM-D)
!***AUTHOR  (UNKNOWN)
!***DESCRIPTION
!
!     This subroutine proceeds from the computed QR factorization of
!     an M by N matrix A to accumulate the M by M orthogonal matrix
!     Q from its factored form.
!
!     The subroutine statement is
!
!       SUBROUTINE DQFORM(M,N,Q,LDQ,WA)
!
!     where
!
!       M is a positive integer input variable set to the number
!         of rows of A and the order of Q.
!
!       N is a positive integer input variable set to the number
!         of columns of A.
!
!       Q is an M by M array. On input the full lower trapezoid in
!         the first MIN(M,N) columns of Q contains the factored form.
!         On output Q has been accumulated into a square matrix.
!
!       LDQ is a positive integer input variable not less than M
!         which specifies the leading dimension of the array Q.
!
!       WA is a work array of length M.
!
!***SEE ALSO  DNSQ, DNSQE
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   900328  Added TYPE section.  (WRB)
!***END PROLOGUE  DQFORM
      INTEGER I, J, JM1, K, L, LDQ, M, MINMN, N, NP1
      DOUBLE PRECISION ONE, Q(LDQ,*), SUM, TEMP, WA(*), ZERO
      SAVE ONE, ZERO
      DATA ONE,ZERO /1.0D0,0.0D0/
!
!     ZERO OUT UPPER TRIANGLE OF Q IN THE FIRST MIN(M,N) COLUMNS.
!
!***FIRST EXECUTABLE STATEMENT  DQFORM
      MINMN = MIN(M,N)
      IF (MINMN .LT. 2) GO TO 30
      DO 20 J = 2, MINMN
         JM1 = J - 1
         DO 10 I = 1, JM1
            Q(I,J) = ZERO
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
!
!     INITIALIZE REMAINING COLUMNS TO THOSE OF THE IDENTITY MATRIX.
!
      NP1 = N + 1
      IF (M .LT. NP1) GO TO 60
      DO 50 J = NP1, M
         DO 40 I = 1, M
            Q(I,J) = ZERO
   40       CONTINUE
         Q(J,J) = ONE
   50    CONTINUE
   60 CONTINUE
!
!     ACCUMULATE Q FROM ITS FACTORED FORM.
!
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
!
!     LAST CARD OF SUBROUTINE DQFORM.
!
      END SUBROUTINE DQFORM 
!DECK DDOGLG
      SUBROUTINE DDOGLG (N, R, LR, DIAG, QTB, DELTA, X, WA1, WA2)
!***BEGIN PROLOGUE  DDOGLG
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DNSQ and DNSQE
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (DOGLEG-S, DDOGLG-D)
!***AUTHOR  (UNKNOWN)
!***DESCRIPTION
!
!     Given an M by N matrix A, an N by N nonsingular diagonal
!     matrix D, an M-vector B, and a positive number DELTA, the
!     problem is to determine the convex combination X of the
!     Gauss-Newton and scaled gradient directions that minimizes
!     (A*X - B) in the least squares sense, subject to the
!     restriction that the Euclidean norm of D*X be at most DELTA.
!
!     This subroutine completes the solution of the problem
!     if it is provided with the necessary information from the
!     QR factorization of A. That is, if A = Q*R, where Q has
!     orthogonal columns and R is an upper triangular matrix,
!     then DDOGLG expects the full upper triangle of R and
!     the first N components of (Q transpose)*B.
!
!     The subroutine statement is
!
!       SUBROUTINE DDOGLG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
!
!     where
!
!       N is a positive integer input variable set to the order of R.
!
!       R is an input array of length LR which must contain the upper
!         triangular matrix R stored by rows.
!
!       LR is a positive integer input variable not less than
!         (N*(N+1))/2.
!
!       DIAG is an input array of length N which must contain the
!         diagonal elements of the matrix D.
!
!       QTB is an input array of length N which must contain the first
!         N elements of the vector (Q transpose)*B.
!
!       DELTA is a positive input variable which specifies an upper
!         bound on the Euclidean norm of D*X.
!
!       X is an output array of length N which contains the desired
!         convex combination of the Gauss-Newton direction and the
!         scaled gradient direction.
!
!       WA1 and WA2 are work arrays of length N.
!
!***SEE ALSO  DNSQ, DNSQE
!***ROUTINES CALLED  D1MACH, DENORM
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   900328  Added TYPE section.  (WRB)
!***END PROLOGUE  DDOGLG
!CCCC DOUBLE PRECISION D1MACH,DENORM
      DOUBLE PRECISION DENORM
      INTEGER I, J, JJ, JP1, K, L, LR, N
      DOUBLE PRECISION ALPHA, BNORM, DELTA, DIAG(*), EPSMCH, GNORM,   &
           ONE, QNORM, QTB(*), R(*), SGNORM, SUM, TEMP, WA1(*),   &
           WA2(*), X(*), ZERO
      SAVE ONE, ZERO
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ONE,ZERO /1.0D0,0.0D0/
!
      IF(ISUBG4.EQ.'OGLG')THEN
        WRITE(ICOUT,9052)LR
 9052   FORMAT('LR = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     EPSMCH IS THE MACHINE PRECISION.
!
!***FIRST EXECUTABLE STATEMENT  DDOGLG
      EPSMCH = D1MACH(4)
!
!     FIRST, CALCULATE THE GAUSS-NEWTON DIRECTION.
!
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
!
!     TEST WHETHER THE GAUSS-NEWTON DIRECTION IS ACCEPTABLE.
!
      DO 60 J = 1, N
         WA1(J) = ZERO
         WA2(J) = DIAG(J)*X(J)
   60    CONTINUE
      QNORM = DENORM(N,WA2)
      IF (QNORM .LE. DELTA) GO TO 140
!
!     THE GAUSS-NEWTON DIRECTION IS NOT ACCEPTABLE.
!     NEXT, CALCULATE THE SCALED GRADIENT DIRECTION.
!
      L = 1
      DO 80 J = 1, N
         TEMP = QTB(J)
         DO 70 I = J, N
            WA1(I) = WA1(I) + R(L)*TEMP
            L = L + 1
   70       CONTINUE
         WA1(J) = WA1(J)/DIAG(J)
   80    CONTINUE
!
!     CALCULATE THE NORM OF THE SCALED GRADIENT AND TEST FOR
!     THE SPECIAL CASE IN WHICH THE SCALED GRADIENT IS ZERO.
!
      GNORM = DENORM(N,WA1)
      SGNORM = ZERO
      ALPHA = DELTA/QNORM
      IF (GNORM .EQ. ZERO) GO TO 120
!
!     CALCULATE THE POINT ALONG THE SCALED GRADIENT
!     AT WHICH THE QUADRATIC IS MINIMIZED.
!
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
!
!     TEST WHETHER THE SCALED GRADIENT DIRECTION IS ACCEPTABLE.
!
      ALPHA = ZERO
      IF (SGNORM .GE. DELTA) GO TO 120
!
!     THE SCALED GRADIENT DIRECTION IS NOT ACCEPTABLE.
!     FINALLY, CALCULATE THE POINT ALONG THE DOGLEG
!     AT WHICH THE QUADRATIC IS MINIMIZED.
!
      BNORM = DENORM(N,QTB)
      TEMP = (BNORM/GNORM)*(BNORM/QNORM)*(SGNORM/DELTA)
      TEMP = TEMP - (DELTA/QNORM)*(SGNORM/DELTA)**2   &
             + SQRT((TEMP-(DELTA/QNORM))**2   &
                     +(ONE-(DELTA/QNORM)**2)*(ONE-(SGNORM/DELTA)**2))
      ALPHA = ((DELTA/QNORM)*(ONE - (SGNORM/DELTA)**2))/TEMP
  120 CONTINUE
!
!     FORM APPROPRIATE CONVEX COMBINATION OF THE GAUSS-NEWTON
!     DIRECTION AND THE SCALED GRADIENT DIRECTION.
!
      TEMP = (ONE - ALPHA)*MIN(SGNORM,DELTA)
      DO 130 J = 1, N
         X(J) = TEMP*WA1(J) + ALPHA*X(J)
  130    CONTINUE
  140 CONTINUE
      RETURN
!
!     LAST CARD OF SUBROUTINE DDOGLG.
!
      END SUBROUTINE DDOGLG 
!DECK D1UPDT
      SUBROUTINE D1UPDT (M, N, S, LS, U, V, W, SING)
!***BEGIN PROLOGUE  D1UPDT
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DNSQ and DNSQE
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (R1UPDT-S, D1UPDT-D)
!***AUTHOR  (UNKNOWN)
!***DESCRIPTION
!
!     Given an M by N lower trapezoidal matrix S, an M-vector U,
!     and an N-vector V, the problem is to determine an
!     orthogonal matrix Q such that
!
!                   t
!           (S + U*V )*Q
!
!     is again lower trapezoidal.
!
!     This subroutine determines Q as the product of 2*(N - 1)
!     transformations
!
!           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
!
!     where GV(I), GW(I) are Givens rotations in the (I,N) plane
!     which eliminate elements in the I-th and N-th planes,
!     respectively. Q itself is not accumulated, rather the
!     information to recover the GV, GW rotations is returned.
!
!     The SUBROUTINE statement is
!
!       SUBROUTINE D1UPDT(M,N,S,LS,U,V,W,SING)
!
!     where
!
!       M is a positive integer input variable set to the number
!         of rows of S.
!
!       N is a positive integer input variable set to the number
!         of columns of S. N must not exceed M.
!
!       S is an array of length LS. On input S must contain the lower
!         trapezoidal matrix S stored by columns. On output S contains
!         the lower trapezoidal matrix produced as described above.
!
!       LS is a positive integer input variable not less than
!         (N*(2*M-N+1))/2.
!
!       U is an input array of length M which must contain the
!         vector U.
!
!       V is an array of length N. On input V must contain the vector
!         V. On output V(I) contains the information necessary to
!         recover the Givens rotation GV(I) described above.
!
!       W is an output array of length M. W(I) contains information
!         necessary to recover the Givens rotation GW(I) described
!         above.
!
!       SING is a LOGICAL output variable. SING is set TRUE if any
!         of the diagonal elements of the output S are zero. Otherwise
!         SING is set FALSE.
!
!***SEE ALSO  DNSQ, DNSQE
!***ROUTINES CALLED  D1MACH
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   900328  Added TYPE section.  (WRB)
!***END PROLOGUE  D1UPDT
!CCCC DOUBLE PRECISION D1MACH
      INTEGER I, J, JJ, L, LS, M, N, NM1, NMJ
      DOUBLE PRECISION COS, COTAN, GIANT, ONE, P25, P5, S(*),   &
           SIN, TAN, TAU, TEMP, U(*), V(*), W(*), ZERO
      LOGICAL SING
      SAVE ONE, P5, P25, ZERO
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ONE,P5,P25,ZERO /1.0D0,5.0D-1,2.5D-1,0.0D0/
!
      IF(ISUBG4.EQ.'DNSQ')THEN
        WRITE(ICOUT,9052)LS
 9052   FORMAT('LS = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     GIANT IS THE LARGEST MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  D1UPDT
      GIANT = D1MACH(2)
!
!     INITIALIZE THE DIAGONAL ELEMENT POINTER.
!
      JJ = (N*(2*M - N + 1))/2 - (M - N)
!
!     MOVE THE NONTRIVIAL PART OF THE LAST COLUMN OF S INTO W.
!
      L = JJ
      DO 10 I = N, M
         W(I) = S(L)
         L = L + 1
   10    CONTINUE
!
!     ROTATE THE VECTOR V INTO A MULTIPLE OF THE N-TH UNIT VECTOR
!     IN SUCH A WAY THAT A SPIKE IS INTRODUCED INTO W.
!
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 NMJ = 1, NM1
         J = N - NMJ
         JJ = JJ - (M - J + 1)
         W(J) = ZERO
         IF (V(J) .EQ. ZERO) GO TO 50
!
!        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
!        J-TH ELEMENT OF V.
!
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
!
!        APPLY THE TRANSFORMATION TO V AND STORE THE INFORMATION
!        NECESSARY TO RECOVER THE GIVENS ROTATION.
!
         V(N) = SIN*V(J) + COS*V(N)
         V(J) = TAU
!
!        APPLY THE TRANSFORMATION TO S AND EXTEND THE SPIKE IN W.
!
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
!
!     ADD THE SPIKE FROM THE RANK 1 UPDATE TO W.
!
      DO 80 I = 1, M
         W(I) = W(I) + V(N)*U(I)
   80    CONTINUE
!
!     ELIMINATE THE SPIKE.
!
      SING = .FALSE.
      IF (NM1 .LT. 1) GO TO 140
      DO 130 J = 1, NM1
         IF (W(J) .EQ. ZERO) GO TO 120
!
!        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE
!        J-TH ELEMENT OF THE SPIKE.
!
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
!
!        APPLY THE TRANSFORMATION TO S AND REDUCE THE SPIKE IN W.
!
         L = JJ
         DO 110 I = J, M
            TEMP = COS*S(L) + SIN*W(I)
            W(I) = -SIN*S(L) + COS*W(I)
            S(L) = TEMP
            L = L + 1
  110       CONTINUE
!
!        STORE THE INFORMATION NECESSARY TO RECOVER THE
!        GIVENS ROTATION.
!
         W(J) = TAU
  120    CONTINUE
!
!        TEST FOR ZERO DIAGONAL ELEMENTS IN THE OUTPUT S.
!
         IF (S(JJ) .EQ. ZERO) SING = .TRUE.
         JJ = JJ + (M - J + 1)
  130    CONTINUE
  140 CONTINUE
!
!     MOVE W BACK INTO THE LAST COLUMN OF THE OUTPUT S.
!
      L = JJ
      DO 150 I = N, M
         S(L) = W(I)
         L = L + 1
  150    CONTINUE
      IF (S(JJ) .EQ. ZERO) SING = .TRUE.
      RETURN
!
!     LAST CARD OF SUBROUTINE D1UPDT.
!
      END SUBROUTINE D1UPDT 
!DECK D1MPYQ
      SUBROUTINE D1MPYQ (M, N, A, LDA, V, W)
!***BEGIN PROLOGUE  D1MPYQ
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DNSQ and DNSQE
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (R1MPYQ-S, D1MPYQ-D)
!***AUTHOR  (UNKNOWN)
!***DESCRIPTION
!
!     Given an M by N matrix A, this subroutine computes A*Q where
!     Q is the product of 2*(N - 1) transformations
!
!           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)
!
!     and GV(I), GW(I) are Givens rotations in the (I,N) plane which
!     eliminate elements in the I-th and N-th planes, respectively.
!     Q itself is not given, rather the information to recover the
!     GV, GW rotations is supplied.
!
!     The SUBROUTINE statement is
!
!       SUBROUTINE D1MPYQ(M,N,A,LDA,V,W)
!
!     where
!
!       M is a positive integer input variable set to the number
!         of rows of A.
!
!       N IS a positive integer input variable set to the number
!         of columns of A.
!
!       A is an M by N array. On input A must contain the matrix
!         to be postmultiplied by the orthogonal matrix Q
!         described above. On output A*Q has replaced A.
!
!       LDA is a positive integer input variable not less than M
!         which specifies the leading dimension of the array A.
!
!       V is an input array of length N. V(I) must contain the
!         information necessary to recover the Givens rotation GV(I)
!         described above.
!
!       W is an input array of length N. W(I) must contain the
!         information necessary to recover the Givens rotation GW(I)
!         described above.
!
!***SEE ALSO  DNSQ, DNSQE
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   800301  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   900328  Added TYPE section.  (WRB)
!***END PROLOGUE  D1MPYQ
      INTEGER I, J, LDA, M, N, NM1, NMJ
      DOUBLE PRECISION A(LDA,*), COS, ONE, SIN, TEMP, V(*), W(*)
      SAVE ONE
      DATA ONE /1.0D0/
!
!     APPLY THE FIRST SET OF GIVENS ROTATIONS TO A.
!
!***FIRST EXECUTABLE STATEMENT  D1MPYQ
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 50
      COS = 0.0
      SIN = 0.0
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
!
!     APPLY THE SECOND SET OF GIVENS ROTATIONS TO A.
!
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
!
!     LAST CARD OF SUBROUTINE D1MPYQ.
!
      END SUBROUTINE D1MPYQ 
      SUBROUTINE DECOMP(IND, LOCA, NW, W, M, LSTFI, N, LS, LV,   &
                        LLIM, LP)
!  PART OF ACM 591 FOR ANOVA
!  ***************************** DECOMP *****************************   DEC   10
!                                                                       DEC   20
!  OBTAINS A FACTORIAL DECOMPOSITION OF THE VECTOR T WHERE T CONSISTS   DEC   30
!  OF THE FIRST NCELLS LOCATIONS OF THE VECTOR A (IN ARRAY W); THE      DEC   40
!  FACTORIAL DECOMPOSITION IS FORMED IN VECTOR A AND OCCUPIES ALL THE   DEC   50
!  LOCATIONS OF THIS VECTOR.  ALTERNATIVELY COMPUTES CLASSIFICATION     DEC   60
!  SUMS/MEANS IN VECTOR A FOR RESTRUCTURING DATA OR FOR THE C OPTION.   DEC   70
!  FOLLOWS THE ALGORITHM DESCRIBED IN HEMMERLE, STATISTICAL COMPUTA-    DEC   80
!  TIONS ON A DIGITAL COMPUTER 1967.                                    DEC   90
!                                                                       DEC  100
!  IND = 0 (FACTORIAL DECOMPOSITION); IND = 1 (CLASSIFICATION SUMS);    DEC  110
!  IND = 2 (CLASSIFICATION MEANS)                                       DEC  120
!                                                                       DEC  130
!  LOCA = BASE ADDRESS OF VECTOR A IN ARRAY W; IOUT = OUTPUT UNIT FOR   DEC  140
!  CLASSIFICATIONS MEANS.                                               DEC  150
!                                                                       DEC  160
!  (SEE MAIN PROGRAM COMMENTS) FOR DESCRIPTION OF OTHER ARGUMENTS       DEC  170
!                                                                       DEC  180
!  ******************************************************************   DEC  190
! NOTE: THE ARGUMENTS LS,LV,LP, AND IOUT ARE USED ONLY FOR C MEANS
      DOUBLE PRECISION W, TEMP, DNPM, CMEAN
      DIMENSION W(NW), LSTFI(M), LS(N), LV(N), LLIM(N), LP(10)
!
!CCCC CHARACTER*1 IDOT
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA IDOT /'.'/
!
      IF(ISUBG4.EQ.'COMP')THEN
        WRITE(ICOUT,9051)LS,LV
 9051   FORMAT('LS,LV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9050 I=1,10
          WRITE(ICOUT,9052)I,LP(1)
 9052     FORMAT('I,LP(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 9050   CONTINUE
      ENDIF
!
      L = 0
      LL = 1
      MM = 1
      NN = 1
      LOCTWO = LOCA + 1
   10 LOCONE = LOCA + 1
      KK = LL
! FIND NUMBER OF ELEMENTS IN THIS MEAN
!
      K1 = N + 1 - NN
      NPM = LLIM(K1)
      DNPM = NPM
   20 LOCTWO = LOCTWO + LSTFI(MM)
! FIND NUMBER OF MEANS FOR EACH RESIDUAL
      MEANST = LSTFI(MM+1)
! FIND INCREMENT
      K1 = M + 1 - KK
      INC = LSTFI(K1)
! FORM THE ARRAY OF MEANS
      MD = 1
      NO = M - MM
!NIST IF (IND.EQ.2) CALL LABEL(NO, IDOT, LS, IOUT, N, LV, LP)
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
! DEVIATES (IND=0); SUMS (IND=1); CLASSIFICATION MEANS (IND=2)
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
! FORM DEVIATES
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
      END SUBROUTINE DECOMP
      SUBROUTINE SCAN(IPT, M, LER, N, LE, LS, LV, LLIM, LP, L, IA,   &
                      IBATCH)
!  PART OF ACM 591 FOR ANOVA
!  ****************************** SCAN ******************************   SCA   10
!                                                                       SCA   20
!  PROCESSES THE MODEL/HYPOTHESIS STATEMENT TO CONSTRUCT/MODIFY THE     SCA   30
!  E/R LIST (ARRAY LER); TURNS SWITCH ISST ON FOR AN INVALID STATE-     SCA   40
!  MENT.  DETERMINES THE EFFECTIVE NUMBER OF FACTORS (NSUBS); TURNS     SCA   50
!  SWITCH IXST ON WHEN THE EFFECTIVE X MATRIX IS SQUARE; COMPUTES THE   SCA   60
!  PARAMETERS NEEDED IN RESTRUCTURING DATA (LPOUT AND NO1).  COMPUTES   SCA   70
!  THE DEGREES OF FREEDOM APPLICABLE TO DATA WITH NO MISSING CELLS      SCA   80
!  (IDFM AND IDFR).                                                     SCA   90
!                                                                       SCA  100
!  IPT = POINTER TO BEGINNING OF MODEL/HYPOTHESIS STATEMENT IN INPUT    SCA  110
!  BUFFER; IBATCH = 1 (BATCH PROCESSING) OR IBATCH = 0 (INTERACTIVE)    SCA  120
!                                                                       SCA  130
!  (SEE MAIN PROGRAM COMMENTS FOR DESCRIPTION OF OTHER ARGUMENTS)       SCA  140
!                                                                       SCA  150
!  ******************************************************************   SCA  160
      COMMON /C1/ YPY, SSRM, SSEM, IIN, IOUT, IROPT, IVOPT, IGOPT,   &
       IPOPT, IOFLAG, IBST, IHST, IRST, ISST, IXST, ICD, NSUBS, LPOUT,   &
       NO1, IDF, IDFM, IDFR
      DIMENSION LER(M), LE(N), LS(N), LV(N), LLIM(N), LP(10), IA(L)
      DOUBLE PRECISION YPY, SSRM, SSEM
!
!NIST CHARACTER*1 ILP, IRP, IM, IH, ISTAR, ISLASH, IBLANK, IC
      CHARACTER*1 IRP, IM, IH, ISTAR, ISLASH, IBLANK, IC
      CHARACTER*4 ICD
!
!NIST CHARACTER*1 FUNCTION IGET
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA ILP   /'('/
      DATA IRP   /')'/
      DATA IM    /'M'/
      DATA IH    /'H'/
      DATA ISTAR /'*'/
      DATA ISLASH /'/'/
      DATA IBLANK /' '/
!
      IF(ISUBG4.EQ.'SCAN')THEN
        WRITE(ICOUT,9052)LE,LS,LV
 9052   FORMAT('LE,LS,LV = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      ISST = 0
      IXST = 0
      M1 = M - 1
      II = IPT
      IF (II.GT.L) GO TO 350
!NIST IC = IGET(II,IA,L)
      IC = ' '
      IF (ICD(1:1).EQ.IH) GO TO 20
      IF (IC.EQ.ISTAR) GO TO 270
! INITIALIZE E/R LIST TO ZEROES FOR M AND ABSOLUTE VALUES FOR H
      DO 10 I=1,M1
        LER(I) = 0
   10 CONTINUE
      LER(M) = 1
   20 IF (LER(M).EQ.0) GO TO 350
      DO 30 I=1,M1
        LER(I) = IABS(LER(I))
   30 CONTINUE
      M2 = 2*M
! SCAN TERM TO CONSTRUCT E/R LIST; ENTER NEGATIVES FOR H TERM
   40 DO 50 I=1,N
        LP(I) = M2
   50 CONTINUE
! SUM VALUES OF FACTOR SYMBOLS FOR E/R ENTRY; ZERO LP POSITIONS
      NE = 0
      NVS = 0
   60 IFLAG = 0
      DO 70 I=1,N
!NIST   IF (IC.NE.LE(I)) GO TO 70
        LP(I) = 0
        IFLAG = 1
        NE = NE + 1
        NVS = NVS + LV(I)
   70 CONTINUE
      IF (IFLAG.NE.1) GO TO 80
      IF (II.GT.L) GO TO 350
!NIST IC = IGET(II,IA,L)
      GO TO 60
   80 IF (NE.EQ.0) GO TO 350
!NIST IF (IC.NE.ILP) GO TO 350
! SCAN SUBSCRIPTS; SET NONZERO LP ENTRIES TO NUMERICAL VALUES
      NS = 0
      NAS = 0
   90 IF (II.GT.L) GO TO 350
!NIST IC = IGET(II,IA,L)
!NIST SET FOLLOWING LINE JUST TO AVOID COMPILATION WARNING.
!NIST REMOVE IF WE ACTIVATE THIS CODE
      IC=' '
      IFLAG = 0
      DO 120 I=1,N
!NIST   IF (IC.NE.LS(I)) GO TO 120
        IF (LP(I).NE.0) LP(I) = LV(I)
        IF (LP(I).EQ.0) NAS = NAS + 1
! CHECK FOR INVALID NESTED TERM
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
! CHECK FOR INVALID CROSSED TERM
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
! ENTER SUM FOR NESTED TERM INTO E/R POSITIONS TO POOL
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
!NIST IC = IGET(II,IA,L)
      IF (IC.EQ.IBLANK .AND. II.GT.L) GO TO 200
      IF (IC.NE.ISLASH) GO TO 40
! READ MODEL OR HYPOTHESIS CONTINUATION CARD (SLASH FOLLOWS TERM)
      READ (IIN,99999) (IA(I),I=1,L)
99999 FORMAT (80A1)
      IF (IBATCH.EQ.1) THEN
         WRITE (ICOUT,99998) (IA(I),I=1,L)
         CALL DPWRST('XXX','BUG ')
      ENDIF
99998 FORMAT (1H , 80A1)
      II = 1
!NIST IC = IGET(II,IA,L)
      GO TO 40
! CHECK FOR INVALID HYPOTHESIS TERM
  200 DO 220 I=1,M1
        DO 210 J=I,M1
          IF (LER(I).EQ.0) GO TO 210
          IF (LER(I).EQ.(-LER(J))) GO TO 350
  210   CONTINUE
  220 CONTINUE
! CONSTRUCT LP FROM E/R; DETERMINE EFFECTIVE FACTORS
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
! DETERMINE IF THE EFFECTIVE X MATRIX IS SQUARE
      IV = N - NSUBS + 1
      DO 260 I=1,N
        IF (LP(I).EQ.0) GO TO 260
        IF (LP(I).NE.LV(IV)) GO TO 310
  260 CONTINUE
      GO TO 300
! CONSTRUCT E/R LIST FOR COMPLETELY CROSSED MODEL
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
! COMPUTE PARAMETERS REQUIRED TO RESTRUCTURE CELL FREQUENCY ARRAY
      LPOUT = 1
      NO1 = 1
      DO 320 I=1,N
        IF (LP(I).EQ.0) LPOUT = LPOUT*LLIM(I)
        IF (LP(I).NE.0) NO1 = NO1 + LV(I)
  320 CONTINUE
! COMPUTE DEGREES OF FREEDOM FOR FULL OR REDUCED MODEL
      IDF = 0
      DO 340 I=1,M
        IF (LER(I).LE.0) GO TO 340
        NO2 = M - I + 1
        CALL LABEL(NO2, 0, LLIM, N, LV, LP)
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
      END SUBROUTINE SCAN
      SUBROUTINE STEP(IND, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM,   &
                      LT, LP)
!  PART OF ACM 591 FOR ANOVA
!  ****************************** STEP ******************************   STE   10
!                                                                       STE   20
!  PERFORMS THE FOLLOWING SUB-STEPS OPERATING UPON THE VECTORS IN THE   STE   30
!  W ARRAY                                                              STE   40
!                                                                       STE   50
!                           1) T = (Y-D*V)/C                            STE   60
!                           2) V = V+T                                  STE   70
!                           3) B = B+T                                  STE   80
!                           4) T = R(T)                                 STE   90
!                           5) V = V-T                                  STE  100
!                           6) S = 2*Y*V-V*D*V                          STE  110
!                                                                       STE  120
!  VECTOR T CONSISTS OF THE FIRST NCELLS LOCATIONS IN VECTOR A OF W;    STE  130
!  HOWEVER, ALL LOCATIONS IN VECTOR A ARE NEEDED IN SUB-STEP 4.  R(T)   STE  140
!  IS THE RESIDUAL OPERATOR APPLIED TO VECTOR T; IT IS IMPLEMENTED      STE  150
!  USING SUBROUTINES DECOMP, POOL, AND LABEL.                           STE  160
!                                                                       STE  170
!  SUB-STEPS 1 AND 6 ARE MODIFIED IN COMPUTING RANK WITH THE R OPTION   STE  180
!  AND SUB-STEP 1 IS ALSO MODIFIED WHEN SWITCH IBST IS ON; ARGUMENT     STE  190
!  IND CONTROLS THESE MODIFICATIONS.                                    STE  200
!                                                                       STE  210
!  IND = 1 (ITERATION FOR SSR); IND = 2 (NON-ITERATIVE, IBST IS ON);    STE  220
!  IND = 3 (ITERATION FOR RANK)                                         STE  230
!                                                                       STE  240
!  S IS EITHER SSR (IND=2), AN APPROXIMATION TO SSR, (IND=1), OR PART   STE  250
!  OF THE RANK APPROXIMATION (IND=3).  C IS A SCALAR CONSTANT SELECT-   STE  260
!  ED FOR MONOTONICITY OF THE APPROXIMATION TO SSR OR FOR FASTER, BUT   STE  270
!  NOT MONOTONE, CONVERGENCE.                                           STE  280
!                                                                       STE  290
!  (SEE MAIN PROGRAM COMMENTS FOR DESCRIPTION OF OTHER ARGUMENTS)       STE  300
!                                                                       STE  310
!  ******************************************************************   STE  320
      DIMENSION W(NW), LSTFI(M), LER(M), LV(N), LLIM(N), LT(N), LP(10)
      DOUBLE PRECISION W, C, S, T1, T2
!
      INCLUDE 'DPCOP2.INC'
!
      ID1=0
      ID2=0
      IB=0
      S = 0
      NCELLS = LSTFI(1)
      DO 40 I=1,NCELLS
! INCREMENT BASE ADDRESSES OF ARRAYS
        ID1 = NCELLS + I
        ID2 = ID1 + NCELLS
        IV = ID2 + NCELLS
        IB = IV + NCELLS
        IA = IB + NCELLS
! GENERAL ITERATION (IND=1); NON-ITERATIVE (IND=2); RANK (IND=3)
        IF (IND.EQ.1) GO TO 20
        IF (IND.EQ.2) GO TO 10
        W(IA) = W(I) - W(IV)
        IF (W(ID1).EQ.0.0) W(IA) = W(I)
        GO TO 30
   10   W(IA) = -W(IV)
        IF (W(ID2).GT.0.0) W(IA) = W(IA) + W(I)/W(ID2)
        GO TO 30
   20   W(IA) = (W(I)-W(ID1)*W(IV))/C
! V=V+A; B=B+A
   30   W(IV) = W(IV) + W(IA)
        W(IB) = W(IB) + W(IA)
   40 CONTINUE
! RESIDUAL OPERATOR
      IA = IB
!CCCC CALL DECOMP(0, IB, IOUT, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
      CALL DECOMP(0, IB, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
      IFLAG = 0
      DO 70 I=1,M
        IF (LER(I).GT.0) GO TO 60
        IF (I.EQ.1) GO TO 50
        NO = M - I + 1
        CALL LABEL(NO, 0, LLIM, N, LV, LP)
        CALL POOL(IFLAG, IA, IB, NW, W, N, LLIM, LT, LP)
   50   IFLAG = 1
   60   IB = IB + LSTFI(I)
   70 CONTINUE
! V=V-T; S=2*Y*V-V*D*V
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
      END SUBROUTINE STEP
      SUBROUTINE PART1(NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP,   &
       MAXMC, Q, QT)
!  PART OF ACM 591 FOR ANOVA
!  ****************************** PART1 *****************************   PAR   10
!                                                                       PAR   20
!  RESTRUCTURES THE DATA (CELL FREQUENCIES) WHEN APPROPRIATE; CHECKS    PAR   30
!  FOR BALANCE AND ALTERNATIVE NON-ITERATIVE COMPUTATIONS; TURNS IBST   PAR   40
!  ON WHEN THE EFFECTIVE X MATRIX IS SQUARE OR THE EFFECTIVE D MATRIX   PAR   50
!  IS A SCALAR MULTIPLE OF THE IDENTITY. COMPUTES RANK WITHOUT ITERA-   PAR   60
!  TION IF POSSIBLE OR ITERATIVELY OTHERWISE WHEN THE RANK (R) OPTION   PAR   70
!  IS SPECIFIED; TURNS SWITCH IRST ON IF THE MAXIMUM NUMBER OF ITERA-   PAR   80
!  TIONS IS EXCEEDED IN COMPUTING RANK.                                 PAR   90
!                                                                       PAR  100
!  ******************************************************************   PAR  110
      COMMON /C1/ YPY, SSRM, SSEM, IIN, IOUT, IROPT, IVOPT, IGOPT,   &
       IPOPT, IOFLAG, IBST, IHST, IRST, ISST, IXST, ICD, NSUBS, LPOUT,   &
       NO1, IDF, IDFM, IDFR
      COMMON /C2/ NCELLS, LOCD1, LOCD2, LOCV, LOCB, LOCA, IRANKM,   &
       IRANKR, MAXIT
      DIMENSION W(NW), LSTFI(M), LER(M), LV(N), LLIM(N), LT(N), LP(10)
      DIMENSION Q(MAXMC,MAXMC), QT(MAXMC)
      DOUBLE PRECISION W, C, S, TRACE, TEMP, Q, QT, YPY, SSRM, SSEM
!
      CHARACTER*4 IH, IM, ICD
!
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA IH /1HH/, IM /1HM/
      DATA IH /'H'/, IM /'M'/
!
      IHST = 0
      IRST = 0
      IBST = 0
      IRANK = 0
      IF (NSUBS.EQ.N) GO TO 100
! FORM RESTRUCTURED CELL FREQUENCY ARRAY (EFFECTIVE D MATRIX)
      DO 10 I=1,NCELLS
        ID1 = LOCD1 + I
        IA = LOCA + I
        W(IA) = W(ID1)
   10 CONTINUE
!CCCC CALL DECOMP(1, LOCA, IOUT, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
      CALL DECOMP(1, LOCA, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
      NS = LOCA
      NN = M - NO1
      DO 20 I=1,NN
        NS = NS + LSTFI(I)
   20 CONTINUE
!NIST CALL LABEL(NO1, 0, LLIM, IOUT, N, LV, LP)
      CALL POOL(0, LOCD2, NS, NW, W, N, LLIM, LT, LP)
! CHECK FOR A SQUARE EFFECTIVE X MATRIX
   30 IF (IXST.EQ.1) GO TO 80
      K = LOCD2 + 1
      IFLAG = 0
      DO 40 I=1,NCELLS
        ID2 = LOCD2 + I
        IF (W(ID2).EQ.0.0) GO TO 130
        IF (W(ID2).NE.W(K)) IFLAG = 1
   40 CONTINUE
      IF (IFLAG.EQ.1) GO TO 70
! THE EFFECTIVE D MATRIX IS A SCALAR TIMES THE IDENTITY
      IRANK = IDF
   50 DO 60 I=1,NCELLS
        ID2 = LOCD2 + I
        W(ID2) = W(ID2)/FLOAT(LPOUT)
   60 CONTINUE
      C = 1.0D0
      IBST = 1
      GO TO 120
! ALL ELEMENTS OF THE EFFECTIVE D MATRIX ARE NONZERO
   70 IRANK = IDF
      GO TO 120
! THE EFFECTIVE X MATRIX IS SQUARE
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
! RANK HAS BEEN DETERMINED (NONITERATIVELY OR ITERATIVELY)
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
! ITERATIVELY COMPUTE RANK OF FULL OR REDUCED MODEL
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
! COMPUTE Q, POWERS OF Q, AND RELATED TRACES (FEW EMPTY CELLS)
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
! POWER Q AND COMPUTE TR(I-Q**(2*K))
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
! TRACE IS MONOTONICALLY INCREASING
      IF (TEMP.LE.RTOL) GO TO 280
      IF (IT.GE.MAXIT) GO TO 360
      TEMP = TRACE
      GO TO 210
  280 DO 290 I=1,NCELLS
        ID2 = LOCD2 + I
        W(I) = W(ID2)
  290 CONTINUE
! ADD ONE (BASED ON MONOTONICITY) TO OBTAIN INTEGER RANK
  300 IRANK = INT(TRACE + 1.0D0)
      GO TO 120
! COMPUTE S FOR UNIT VECTORS (MANY EMPTY CELLS)
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
! THE VALUE OF S IS MONOTONICALLY INCREASING
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
99997 FORMAT (11H MAXIMUM OF, I4, 34H ITERATIONS EXCEEDED IN COMPUTING ,   &
       4HRANK)
      WRITE (ICOUT,89997) TEMP, RTOL, IVEC
      CALL DPWRST('XXX','BUG ')
89997 FORMAT (7H DELTA=, F22.9, 10X, 8HEPSILON=, F22.9, 10X, 7HVECTOR=,   &
       I10)
      IF (NMC.GT.MAXMC) TRACE = TRACE + S
      IRST = 1
      GO TO 300
  370 IF (IROPT.EQ.1) THEN
        WRITE (ICOUT,99996) ICD, IRANK
        CALL DPWRST('XXX','BUG ')
      ENDIF
99996 FORMAT (17H THE RANK OF THE , A1, 17H DESIGN MATRIX IS, I5)
  380 RETURN
      END SUBROUTINE PART1
      SUBROUTINE PART2(NW, W, M, LSTFI, LER, N, LE, LV, LLIM, LT, LP)
!  PART OF ACM 591 FOR ANOVA
!  ****************************** PART2 *****************************   PAR   10
!                                                                       PAR   20
!  COMPUTES SSE AND SSR FOR THE FULL MODEL (ICD = M); OUTPUTS ESTI-     PAR   30
!  MATES OF EXPECTED CELL MEANS (THE VECTOR V) WHEN THE V OPTION IS     PAR   40
!  SPECIFIED; COMPUTES A G-INVERSE SOLUTION TO THE NORMAL EQUATIONS     PAR   50
!  WHEN THE G OPTION IS SPECIFIED.  COMPUTES SSR FOR THE REDUCED MOD-   PAR   60
!  EL (ICD = H) AND AN F STATISTIC; COMPUTES PROBABILITY VALUES WHEN    PAR   70
!  THE P OPTION IS SPECIFIED.  ALL COMPUTATIONS ARE NON-ITERATIVE IF    PAR   80
!  SWITCH IBST IS ON (IBST = 1)                                         PAR   90
!                                                                       PAR  100
!  (SEE MAIN PROGRAM COMMENTS FOR A DESCRIPTION OF ARGUMENTS)           PAR  110
!                                                                       PAR  120
!  ******************************************************************   PAR  130
      COMMON /C1/ YPY, SSRM, SSEM, IIN, IOUT, IROPT, IVOPT, IGOPT,   &
       IPOPT, IOFLAG, IBST, IHST, IRST, ISST, IXST, ICD, NSUBS, LPOUT,   &
       NO1, IDF, IDFM, IDFR
      COMMON /C2/ NCELLS, LOCD1, LOCD2, LOCV, LOCB, LOCA, IRANKM,   &
       IRANKR, MAXIT
      COMMON /C3/ MAXDI, MINDI, FLEVEL, NOSIGD, NOBS
      DIMENSION W(NW), LSTFI(M), LER(M), LE(N), LV(N), LLIM(N), LT(N),   &
       LP(10)
      DOUBLE PRECISION W, C, S, TEMP, YPY, SSRM, SSEM, DABS, F
!
      CHARACTER*1 IBLANK, ISTAR, IM, IH, ISIG
      CHARACTER*4 ICD
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA IBLANK /1H /, ISTAR /1H*/, IM /1HM/, IH /1HH/
      DATA IBLANK /' '/, ISTAR /'*'/, IM /'M'/, IH /'H'/
!
      IF(ISUBG4.EQ.'ART2')THEN
        WRITE(ICOUT,9052)N,LE,LV,LER,LLIM
 9052   FORMAT('N,LE,LV,LER,LLIM = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      FTOL = .005
      STOL = (.05*YPY)/(10.0**NOSIGD)
! ZERO THE VECTORS B AND V TO INITIALIZE THE ITERATIVE ALGORITHM
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
! COMPUTE SSR FOR THE FULL MODEL USING OPTIMUM C FOR CONVERGENCE
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
! APPLY THE E OPERATOR TO THE VECTOR B
   30 DO 40 I=1,NCELLS
        IB = LOCB + I
        IA = LOCA + I
        W(IA) = W(IB)
   40 CONTINUE
!CCCC CALL DECOMP(0, LOCA, IOUT, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
      CALL DECOMP(0, LOCA, NW, W, M, LSTFI, N, LT, LV, LLIM, LP)
! COMPUTE SSR AND SSE FOR THE FULL MODEL
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
! COMPUTE THE G-INVERSE SOLUTION TO THE NORMAL EQUATIONS
      WRITE (ICOUT,99994)
99994 FORMAT (20H G-INVERSE SOLUTION-)
      CALL DPWRST('XXX','BUG ')
! POOL ARRAYS OF "ESTIMATES" WITH EQUAL E/R LIST VALUES
      NP = LOCA
      DO 140 I=1,M
        NO = LER(I)
        IF (NO.LE.0) GO TO 130
        NS = NP
        NOP = M - I + 1
!NIST   CALL LABEL(NOP, 0, LLIM, IOUT, N, LV, LP)
! POSITIVE VALUES IN LLIM WILL CORRESPOND TO SUBSCRIPTS IN PRIMARY
        DO 80 K=1,N
          IF (LP(K).EQ.0) LLIM(K) = -LLIM(K)
   80   CONTINUE
        DO 100 J=I,M
          IF (J.EQ.I) GO TO 90
          IF (LER(J).NE.NO) GO TO 90
          LER(J) = -NO
          NOS = M - J + 1
! OBTAIN MAP COEFFICIENTS FOR SECONDARY ARRAY AND POOL INTO PRIMARY
!NIST     CALL LABEL(NOS, 0, LLIM, IOUT, N, LV, LP)
          CALL POOL(1, NP, NS, NW, W, N, LLIM, LT, LP)
   90     NS = NS + LSTFI(J)
  100   CONTINUE
        DO 110 K=1,N
          LLIM(K) = IABS(LLIM(K))
  110   CONTINUE
! LABEL AND OUTPUT "ESTIMATES" FOR MODEL TERM
!NIST   CALL LABEL(NO, IBLANK, LE, IOUT, N, LV, LP)
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
99993 FORMAT (11H MAXIMUM OF, I4, 34H ITERATIONS EXCEEDED IN COMPUTING ,   &
       3HSSR, A1)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,89993) TEMP, STOL
89993 FORMAT (7H DELTA=, E16.8, 10X, 8HEPSILON=, E16.8)
      CALL DPWRST('XXX','BUG ')
      GO TO 30
! SELECT C FOR MONOTONICITY OF SSR AND F
  170 C = MAXDI
! COMPUTE DEGREES OF FREEDOM TO USE FOR F STATISTIC
  180 IF (IRANKM.EQ.0) GO TO 190
      IF (IRANKR.EQ.0) GO TO 190
      IDFD = NOBS - IRANKM
      IDFN = IRANKM - IRANKR
      WRITE (ICOUT,99992) IDFN, IDFD
99992 FORMAT (33H FROM RANK COMPUTATIONS- DF(NUM)=, I4, 10H, DF(DEN)=,   &
       I5)
      CALL DPWRST('XXX','BUG ')
      GO TO 200
  190 IDFD = NOBS - IDFM
      IDFN = IDFM - IDFR
      WRITE (ICOUT,99991) IDFN, IDFD
99991 FORMAT (50H ASSUMES FULL RANK AND EQUAL LEVELS WITH- DF(NUM)=,   &
       I4, 10H, DF(DEN)=, I5)
      CALL DPWRST('XXX','BUG ')
  200 IF (IDFD*IDFN.LE.0) GO TO 150
      IF (IBST.EQ.1) GO TO 220
! COMPUTE MONOTONICALLY DECREASING APPROXIMATION TO F
  210 CALL STEP(1, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP)
      IT = IT + 1
  220 F = ((SSRM-S)/FLOAT(IDFN))/(SSEM/FLOAT(IDFD))
      IF (IOFLAG.EQ.1) THEN
         WRITE (ICOUT,99999) IT, ICD(1:1), S
         CALL DPWRST('XXX','BUG ')
      ENDIF
! APPROXIMATION TO F PROBABILITY (SMILLIE AND ANSTEY)
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
99990 FORMAT (10H ITERATION, I4, 4H, F=, F12.3, A1, 15H, PROB(F) .GT. ,   &
       F7.4, 16H VS. F LEVEL OF , F7.4)
      CALL DPWRST('XXX','BUG ')
      WRITE (IOUT,89990) S
89990 FORMAT (20H SSR(REDUCED MODEL)=, E16.8)
      CALL DPWRST('XXX','BUG ')
      GO TO 150
! BALANCED CASE; ONE ITERATION
  260 CALL STEP(2, C, S, NW, W, M, LSTFI, LER, N, LV, LLIM, LT, LP)
      IT = IT + 1
      IF (ICD(1:1).EQ.IM) GO TO 50
      GO TO 180
      END SUBROUTINE PART2
      SUBROUTINE POOL(IND, NP, NS, NW, W, N, LLIM, LT, LP)
!  PART OF ACM 591 FOR ANOVA
!  ****************************** POOL ******************************   POO   10
!                                                                       POO   20
!  OPERATES UPON THE VECTORS IN ARRAY W, PRINCIPALLY THE ARRAYS OF A    POO   30
!  FACTORIAL DECOMPOSITION WITHIN VECTOR A OF W.  EITHER MOVES THE      POO   40
!  SECONDARY ARRAY INTO THE PRIMARY ARRAY, DUPLICATING ENTRIES WHERE    POO   50
!  NEEDED, OR POOLS THE SECONDARY ARRAY AND THE PRIMARY ARRAY BY AD-    POO   60
!  DITION INTO THE PRIMARY ARRAY (FOR DESCRIPTION OF MAPPING FUNCTION   POO   70
!  SEE SCHLATER AND HEMMERLE, CACM 1966)                                POO   80
!                                                                       POO   90
!  IND = 0 (REPLACEMENT); IND = 1 (POOLING)                             POO  100
!                                                                       POO  110
!  NP = BASE ADDRESS OF PRIMARY ARRAY (WITHIN ARRAY W)                  POO  120
!  NS = BASE ADDRESS OF SECONDARY ARRAY (WITHIN ARRAY W)                POO  130
!                                                                       POO  140
!  WHEN THE PRIMARY ARRAY HAS LESS THAN N SUBSCRIPTS, THE ENTRIES IN    POO  150
!  LLIM CORRESPONDING TO THE MISSING SUBSCRIPTS MUST BE MADE NEGATIVE   POO  160
!  PRIOR TO ENTRY AND THEN SET POSITIVE AGAIN AFTER RETURN; ARRAY LP    POO  170
!  MUST CONTAIN THE COEFFICIENTS OF THE MAPPING FUNCTION UPON ENTRY.    POO  180
!                                                                       POO  190
!  (SEE MAIN PROGRAM COMMENTS FOR DESCRIPTION OF OTHER ARGUMENTS)       POO  200
!                                                                       POO  210
!  ******************************************************************   POO  220
      DIMENSION W(NW), LLIM(N), LT(N), LP(10)
      DOUBLE PRECISION W, TEMP
!
      INCLUDE 'DPCOP2.INC'
!
! NP=LOCATION OF PRIMARY ARRAY; NS=LOCATION OF SECONDARY ARRAY;
! MAP COEFFICIENTS OBTAINED FROM LP; REPLACE (IND=0); ADD (IND .NE. 0)
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
      END SUBROUTINE POOL
      CHARACTER*1 FUNCTION IGET(ICURS, ISTRNG, LNGTH)
!  PART OF ACM 591 FOR ANOVA
!  ****************************** IGET ******************************   IGE   10
!                                                                       IGE   20
!  USED BY THE MAIN PROGRAM AND SCAN TO SEQUENTIALLY RETRIEVE CHARAC-   IGE   30
!  TERS FROM THE INPUT BUFFER.                                          IGE   40
!                                                                       IGE   50
!  ARGUMENTS - ICURS = POSITION IN CHARACTER STRING; ISTRNG = CHARAC-   IGE   60
!              TER STRING (INPUT BUFFER); LNGTH = LENGTH OF STRING.     IGE   70
!                                                                       IGE   80
!  ******************************************************************   IGE   90
      DIMENSION ISTRNG(LNGTH)
      CHARACTER*1 IBLANK, IPLUS, ICOMMA, ISTRNG
      DATA IBLANK /' '/, IPLUS /'+'/, ICOMMA /','/
   10 IGET = ISTRNG(ICURS)
      ICURS = ICURS + 1
      IF (ICURS.GT.LNGTH) RETURN
      IF (IGET.EQ.IBLANK .OR. IGET.EQ.IPLUS) GO TO 10
      IF (IGET.EQ.ICOMMA) GO TO 10
      RETURN
      END FUNCTION IGET
      SUBROUTINE LABEL(NO, ICHAR, LIST, N, LV, LOA)
!CCCC SUBROUTINE LABEL(NO, ICHAR, LIST, IOUT, N, LV, LOA)
!  ROUTINE FROM ACM 591 FOR ANOVA
!  ****************************** LABEL *****************************   LAB   10
!                                                                       LAB   20
!  DETERMINES THE SUBSCRIPTS OF THE PRIMARY ARRAY; CALCULATES COEFFI-   LAB   30
!  CIENTS FOR MAPPING THE SECONDARY ARRAY INTO THE PRIMARY ARRAY.       LAB   40
!  ALSO PREPARES LABELS FOR THE G-INVERSE SOLUTION AND CLASSIFICATION   LAB   50
!  MEANS; EACH LABEL IS AN ALPHANUMERIC ARRAY OF SIZE 10.               LAB   60
!                                                                       LAB   70
!                                                                       LAB   80
!                      (OUT)    ARGUMENTS      (IN)                     LAB   90
!                                                                       LAB  100
!                       LOA              NO   ICHAR  LIST               LAB  110
!                                                                       LAB  120
!               PRIMARY SUBSCRIPTS     M-I+1    0    LLIM               LAB  130
!               MAP COEFFICIENTS       M-I+1    0    LLIM               LAB  140
!               MODEL TERM LABEL       LER(I) BLANK   LE                LAB  150
!               SUBSCRIPTS LABEL       M-I+1    .     LS                LAB  160
!                                                                       LAB  170
!               IN COMPUTING NO, I IS THE POSITION OF THE               LAB  180
!               ARRAY WITHIN THE M ARRAYS (IN VECTOR A OF               LAB  190
!               W) OR, FOR MODEL TERM LABELS, THE VALUE                 LAB  200
!               OF THE E/R LIST (ARRAY LER) FOR THAT TERM               LAB  210
!                                                                       LAB  220
!  (SEE MAIN PROGRAM COMMENTS FOR DESCRIPTION OF OTHER ARGUMENTS)       LAB  230
!                                                                       LAB  240
!  ******************************************************************   LAB  250
      DIMENSION LIST(N), LV(N), LOA(10)
!CCCC CHARACTER*1 IBLANK
!
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA IBLANK /' '/
!
! MAP COEFFICIENTS: (NO=2**N-I+1,ICHAR=0,LIST=LLIM)
! LABELS: MODEL TERM (NO=LER(I),ICHAR= ,LIST=LE)
!         SUBSCRIPTS (NO=2**N-I+1,ICHAR=.,LIST=LS)
!
      NUM = NO - 1
      DO 10 I=N,10
!NIST   LOA(I) = IBLANK
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
!NIST IF (ICHAR.NE.IBLANK) GO TO 30
      IF (ICHAR.NE.-1) GO TO 30
      GO TO 40
   50 LOA(I) = LIST(J)
      IF (NUM.NE.0) GO TO 30
   60 IF (ICHAR.EQ.0) GO TO 70
!NIST WRITE (ICOUT,99999) (LOA(K),K=1,10)
!NIST99999 FORMAT (1H , 10A1)
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
      END SUBROUTINE LABEL

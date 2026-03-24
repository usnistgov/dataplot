      SUBROUTINE DPC4HI(IHVAL,IVAL,IBUGA3,IERROR)
!
!     PURPOSE--CONVERT A CHARACTER VARIABLE
!              INTO THE CORRESPONDING INTEGER VALUE.
!     NOTE--INASMUCH AS THE ASSUMED INPUT WORD HAS 4 CHARACTERS AT MOST,
!           THEN THE VALID RANGE OF THE OUTPUT INTEGER VARIABLE
!           IS -999 TO 9999   .
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JANUARY  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHVAL
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHTEMP
      CHARACTER*4 ISIGN
!
!---------------------------------------------------------------------
!
      DIMENSION IHTEMP(4)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      NUMASC=4
      IVAL=0
!
      ITERM=0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPC4HI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IHVAL,IBUGA3,IERROR
   52 FORMAT('IHVAL,IBUGA3,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               *******************************************
!               **  STEP 1--                             **
!               **  DECOMPOSE THE 4-CHARACTERS IN IHVAL  **
!               **  INTO 4 1-CHARACTER WORDS.            **
!               *******************************************
!
      DO 200 J=1,NUMASC
      IHTEMP(J)='    '
      ISTAR1=NUMBPC*(J-1)
      CALL DPCHEX(ISTAR1,NUMBPC,IHVAL,0,NUMBPC,IHTEMP(J))
  200 CONTINUE
!
!               ******************************************************
!               **  STEP 2--                                        **
!               **  CARRY OUT THE HOLLERITH TO INTEGER CONVERSION.  **
!               ******************************************************
!
      ISIGN='+'
      NUMSIG=0
      IDIGI=0
      ISUM=0
      DO 400 I=1,NUMASC
      IREV=NUMASC-I+1
      IF(IHTEMP(IREV).EQ.' ')GO TO 400
      IF(IHTEMP(IREV).EQ.'0')GO TO 410
      IF(IHTEMP(IREV).EQ.'1')GO TO 411
      IF(IHTEMP(IREV).EQ.'2')GO TO 412
      IF(IHTEMP(IREV).EQ.'3')GO TO 413
      IF(IHTEMP(IREV).EQ.'4')GO TO 414
      IF(IHTEMP(IREV).EQ.'5')GO TO 415
      IF(IHTEMP(IREV).EQ.'6')GO TO 416
      IF(IHTEMP(IREV).EQ.'7')GO TO 417
      IF(IHTEMP(IREV).EQ.'8')GO TO 418
      IF(IHTEMP(IREV).EQ.'9')GO TO 419
      IF(IHTEMP(IREV).EQ.'+')GO TO 420
      IF(IHTEMP(IREV).EQ.'-')GO TO 421
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,431)
  431 FORMAT('***** ERROR IN DPC4HI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,432)
  432 FORMAT('      CHARACTER ENCOUNTERED IN THE CONVERSION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,433)
  433 FORMAT('      WHICH WAS NOT 0 THROUGH 9, +, - OR SPACE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,434)IHTEMP(IREV)
  434 FORMAT('      CHARACTER IN QUESTION IHTEMP(IREV) = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,435)IHVAL
  435 FORMAT('      IHVAL = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  410 ITERM=0
      GO TO 425
  411 ITERM=1
      GO TO 425
  412 ITERM=2
      GO TO 425
  413 ITERM=3
      GO TO 425
  414 ITERM=4
      GO TO 425
  415 ITERM=5
      GO TO 425
  416 ITERM=6
      GO TO 425
  417 ITERM=7
      GO TO 425
  418 ITERM=8
      GO TO 425
  419 ITERM=9
      GO TO 425
  420 NUMSIG=NUMSIG+1
      GO TO 400
  421 NUMSIG=NUMSIG+1
      ISIGN='-'
      GO TO 400
  425 IDIGI=IDIGI+1
      IEXP=IDIGI-1
!CCCC ISUM=ISUM+ITERM*(10**IEXP)
      IJUNK=INT(10.0**IEXP + 0.01)
      ISUM=ISUM+ITERM*IJUNK
  400 CONTINUE
!
      IF(NUMSIG.LE.1)GO TO 459
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,451)
  451 FORMAT('***** ERROR IN DPC4HI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,452)
  452 FORMAT('      MULTIPLE SIGNS (+/-) ENCOUNTERED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,453)
  453 FORMAT('      IN THE CONVERSION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,454)NUMSIG
  454 FORMAT('      NUMBER OF SIGNS NUMSIG = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,456)(IHTEMP(J),J=1,NUMASC)
  456 FORMAT('      (IHTEMP(J),J=1,NUMASC) = ',4A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,457)IHVAL
  457 FORMAT('      IHVAL = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  459 CONTINUE
      IF(ISIGN.EQ.'-')ISUM=-ISUM
      IVAL=ISUM
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END OF DPC4HI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IHVAL
 9012 FORMAT('IHVAL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)(IHTEMP(J),J=1,NUMASC)
 9014 FORMAT('(IHTEMP(J),J=1,NUMASC) = ',4A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NUMASC,ISIGN,NUMSIG,ISUM,ITERM
 9015 FORMAT('NUMASC,ISIGN,NUMSIG,ISUM,ITERM = ',I8,2X,A4,3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IBUGA3,IERROR
 9016 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IVAL
 9017 FORMAT('IVAL = ',I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPC4HI
      SUBROUTINE DPC4IH(IVAL,IHVAL,IBUGA3,IERROR)
!
!     PURPOSE--CONVERT AN INTEGER VARIABLE
!              TO A 4-CHARACTER-PER-WORD HOLLERITH STRING.
!     NOTE--CONVERT ONLY THE FIRST 4 CHARACTERS OF THE
!           INTEGER VARIABLE (INCLUDING THE NEGATIVE
!           SIGN, IF EXISTENT).
!     NOTE--INCORRECT VALUERS WILL RESULT IF THE INPUT INTEGER
!           IS LARGER THAN 9999 OR SMALLER THAN -999   .
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JANUARY  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHVAL
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHTEMP
      CHARACTER*4 ISIGN
      CHARACTER*4 IHDIG
!
!---------------------------------------------------------------------
!
      DIMENSION IHTEMP(4)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      NUMASC=4
      IVAL2=IVAL
      IHVAL='    '
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPC4IH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IVAL,IBUGA3,IERROR
   52 FORMAT('IVAL,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               ***********************
!               **  STEP 2--         **
!               **  DETERMINE SIGN.  **
!               ***********************
!
      ISIGN='+'
      IF(IVAL2.LT.0)ISIGN='-'
      IVAL2=IABS(IVAL2)
!
!               ***********************************
!               **  STEP 3--                     **
!               **  DETERMINE NUMBER OF DIGITS.  **
!               ***********************************
!
      IMIN=1
      IMAX=NUMASC
      DO 300 I=IMIN,IMAX
      IREV=IMAX-I+IMIN
      IDIV=INT(10.0**(IREV-1) + 0.01)
      IDIG=IVAL2/IDIV
      IF(IDIG.NE.0)GO TO 350
  300 CONTINUE
      NUMDIG=1
      GO TO 390
  350 CONTINUE
      NUMDIG=IREV
  390 CONTINUE
!
!               ***************************************
!               **  STEP 4--                         **
!               **  IF NEGATIVE,                     **
!               **  INSERT SIGN INTO OUTPUT VECTOR.  **
!               ***************************************
!
      J=0
      IF(ISIGN.EQ.'-')J=J+1
      IF(ISIGN.EQ.'-')IHTEMP(J)='-'
!
!               **************************
!               **  STEP 5--            **
!               **  INSERT DIGITS INTO  **
!               **  OUTPUT VECTOR.      **
!               **************************
!
      IMIN=1
      IMAX=NUMDIG
      IF(IMAX.GE.NUMASC.AND.ISIGN.EQ.'-')IMAX=NUMASC-1
      IF(IMAX.GE.NUMASC.AND.ISIGN.EQ.'+')IMAX=NUMASC
      DO 500 I=IMIN,IMAX
      IREV=IMAX-I+IMIN
      IDIV=INT(10.0**(IREV-1) + 0.01)
      IDIG=IVAL2/IDIV
!
      IF(IDIG.EQ.0)GO TO 510
      IF(IDIG.EQ.1)GO TO 511
      IF(IDIG.EQ.2)GO TO 512
      IF(IDIG.EQ.3)GO TO 513
      IF(IDIG.EQ.4)GO TO 514
      IF(IDIG.EQ.5)GO TO 515
      IF(IDIG.EQ.6)GO TO 516
      IF(IDIG.EQ.7)GO TO 517
      IF(IDIG.EQ.8)GO TO 518
      IF(IDIG.EQ.9)GO TO 519
  510 CONTINUE
      IHDIG='0'
      GO TO 529
  511 CONTINUE
      IHDIG='1'
      GO TO 529
  512 CONTINUE
      IHDIG='2'
      GO TO 529
  513 CONTINUE
      IHDIG='3'
      GO TO 529
  514 CONTINUE
      IHDIG='4'
      GO TO 529
  515 CONTINUE
      IHDIG='5'
      GO TO 529
  516 CONTINUE
      IHDIG='6'
      GO TO 529
  517 CONTINUE
      IHDIG='7'
      GO TO 529
  518 CONTINUE
      IHDIG='8'
      GO TO 529
  519 CONTINUE
      IHDIG='9'
      GO TO 529
  529 CONTINUE
!
      J=J+1
      IF(J.GT.NUMASC)GO TO 550
      IHTEMP(J)=IHDIG
      IVAL2=IVAL2-IDIG*IDIV
  500 CONTINUE
!
      NTEMP=J
      GO TO 590
!
  550 CONTINUE
      NTEMP=J-1
      GO TO 590
!
  590 CONTINUE
!
!               ***************************************
!               **  STEP 6--                         **
!               **  PACK THE CHARACTERS INTO 1 WORD  **
!               ***************************************
!
      IHVAL='    '
      IMAX=NUMASC
      IF(NTEMP.LE.IMAX)IMAX=NTEMP
      IF(IMAX.LE.0)GO TO 690
      DO 600 J=1,IMAX
      ISTAR2=NUMBPC*(J-1)
      CALL DPCHEX(0,NUMBPC,IHTEMP(J),ISTAR2,NUMBPC,IHVAL)
  600 CONTINUE
  690 CONTINUE
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPC4IH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ISIGN,NUMDIG,NUMASC,IMAX,IVAL,NTEMP
 9013   FORMAT('ISIGN,NUMDIG,NUMASC,IMAX,IVAL,NTEMP = ',A4,5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)(IHTEMP(I),I=1,NTEMP)
 9015   FORMAT('IHTEMP(.) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ISTAR2,IHVAL
 9016   FORMAT('ISTAR2,IHVAL = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IBUGA3,IERROR
 9017   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPC4IH
      SUBROUTINE DPCAAN(XTEMP1,MAXNXT,   &
                        ICASAN,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A TABLE OF CAPABILITY ANALYSIS STATISTICS
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--90/9
!     ORIGINAL VERSION--SEPTEMBER 1990.
!     UPDATED         --APRIL     2001. 1) ARGUMENT LIST TO DPCAA2
!                                       2) SAVE RESULTS FROM DPCAA2
!                                          AS INTERNAL PARAMETERS
!     UPDATED         --MAY       2011. USE DPPARS
!     UPDATED         --MAY       2011. SUPPORT FOR "MULTIPLE" AND
!                                       "REPLICATION" OPTIONS
!     UPDATED         --JUNE      2019. TWEAK TO SCRATCH ARRAYS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
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
      CHARACTER*4 IH
      CHARACTER*4 IH2
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
!
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
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
      DIMENSION W(MAXOBV)
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
      DIMENSION TEMP2(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
!
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE4(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE5(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE6(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB9),W(1))
      EQUIVALENCE (GARBAG(IGAR10),XDESGN(1,1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      ICASAN='CAAN'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPCA'
      ISUBN2='AN  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ***********************************************
!               **  TREAT THE CAPABILITY ANALYSIS    CASE    **
!               ***********************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAAN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCAAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  EXTRACT THE COMMAND                               **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:           **
!               **    1) CAPABILITY ANALYSIS Y                        **
!               **    2) MULTIPLE CAPABILITY ANALYSIS  Y1 ... YK      **
!               **    3) REPLICATED CAPABILITY ANALYSIS  Y X1 ... XK  **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTC=9999
      ILASTZ=9999
      ICASAN='CAAN'
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
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'CAPA' .AND. ICTMP2.EQ.'ANAL')THEN
          IFOUND='YES'
          ICASAN='CAAN'
          ILASTC=I+1
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'CAPA' .OR. ICTMP1.EQ.'CP' .OR.   &
               ICTMP1.EQ.'CPK')THEN
          IFOUND='YES'
          ICASAN='CAAN'
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
        ENDIF
  100 CONTINUE
!
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')THEN
        WRITE(ICOUT,91)ICASAN,IMULT,IREPL,ISHIFT
   91   FORMAT('DPCAAN: ICASAN,IMULT,IREPL,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN CAPABILITY ANALYSIS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE CAPABILITY ANALYSIS COMMAND.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
!               *********************************
!               **  STEP 4--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='CAPABILITY ANALYSIS'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NREPL=0
      NRESP=0
      IF(IREPL.EQ.'ON')THEN
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
      DO 519 I=1,MAXOBV
        W(I)=1.0
  519 CONTINUE
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************
!               **  STEP 7--                               **
!               **  DETERMINE IF THE ANALYST               **
!               **  HAS SPECIFIED                          **
!               **      LSL (LOWER SPEC LIMIT)             **
!               **      USL (UPPER SPEC LIMIT)             **
!               **      USLCOST (UPPER SPEC LIMIT COST)    **
!               **      TARGET                             **
!               *********************************************
!
      ISTEPN='7'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CCLSL=CPUMIN
      IH='LSL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')CCLSL=VALUE(ILOCP)
!
      CCUSL=CPUMIN
      IH='USL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')CCUSL=VALUE(ILOCP)
!
      CCTARG=CPUMIN
      IH='TARG'
      IH2='ET  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')CCTARG=VALUE(ILOCP)
!
      CCUSLC=CPUMIN
      IH='USLC'
      IH2='OST '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')CCUSLC=VALUE(ILOCP)
!
!               *********************************************************
!               **  STEP 6--                                           **
!               **  GENERATE THE CAPABILITY ANALYSIS FOR VARIOUS CASES **
!               *********************************************************
!
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ******************************************
!               **  STEP 8A--                           **
!               **  CASE 1: NO REPLICATION VARIABLES    **
!               ******************************************
!
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')   &
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')THEN
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
                      Y,XTEMP1,XTEMP1,NS1,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!         *****************************************************
!         **  STEP 8B--                                      **
!         *****************************************************
!
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAAN')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPCAAN--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,NS1
  823       FORMAT('ICASAN,NUMVAR,NQ = ',A4,2I8)
            CALL DPWRST('XXX','BUG ')
            IF(NS1.GE.1)THEN
              DO 825 I=1,NS1
                WRITE(ICOUT,826)I,Y(I)
  826           FORMAT('I,Y(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
!
          CALL DPCAA2(Y,W,NS1,XTEMP1,MAXNXT,   &
                      CCLSL,CCUSL,CCTARG,CCUSLC,   &
                      YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                      YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                      YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                      YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                      ICAPSW,ICAPTY,IFORSW,ICNPKD,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      IBUGA3,ISUBRO,IERROR)
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IF(NRESP.GT.1)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,   &
                      YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                      YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                      YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                      YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                      IFLAGU,IFRST,ILAST,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO 910 I=1,IMAX
          IF(ISUB(I).EQ.0)GO TO 910
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
!       **  CALL DPSUM2 TO PERFORM SUMMARY.                **
!       *****************************************************
!
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAAN')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPCAAN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,942)ICASAN,NUMVAR,NLOCAL,NREPL
  942     FORMAT('ICASAN,NUMVAR,NLOCAL,NREPL = ',   &
                 A4,3I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 945 I=1,NLOCAL
              WRITE(ICOUT,946)I,Y(I),XDESGN(I,1),XDESGN(I,2)
  946         FORMAT('I,Y(I),XDESGN(I,1),XDESGN(I,2) = ',   &
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
                   XTEMP1,TEMP2,   &
                   NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,   &
                   IBUGA3,ISUBRO,IERROR)
!
!       *****************************************************
!       **  STEP 9D--                                      **
!       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
!       *****************************************************
!
        NCURVE=0
        IADD=1
!
        IF(NREPL.EQ.1)THEN
          J=0
          NTOT=NUMSE1
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,   &
                          CCLSL,CCUSL,CCTARG,CCUSLC,   &
                          YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                          YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                          YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                          YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                          ICAPSW,ICAPTY,IFORSW,ICNPKD,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,   &
                        YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                        YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                        YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                        YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                        IFLAGU,IFRST,ILAST,   &
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
            IF(NTEMP.GT.0)THEN
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,   &
                          CCLSL,CCUSL,CCTARG,CCUSLC,   &
                          YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                          YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                          YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                          YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                          ICAPSW,ICAPTY,IFORSW,ICNPKD,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,   &
                        YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                        YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                        YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                        YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                        IFLAGU,IFRST,ILAST,   &
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,   &
                          CCLSL,CCUSL,CCTARG,CCUSLC,   &
                          YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                          YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                          YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                          YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                          ICAPSW,ICAPTY,IFORSW,ICNPKD,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,   &
                        YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                        YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                        YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                        YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                        IFLAGU,IFRST,ILAST,   &
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,   &
                          CCLSL,CCUSL,CCTARG,CCUSLC,   &
                          YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                          YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                          YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                          YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                          ICAPSW,ICAPTY,IFORSW,ICNPKD,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,   &
                        YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                        YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                        YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                        YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                        IFLAGU,IFRST,ILAST,   &
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,   &
                          CCLSL,CCUSL,CCTARG,CCUSLC,   &
                          YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                          YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                          YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                          YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                          ICAPSW,ICAPTY,IFORSW,ICNPKD,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,   &
                        YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                        YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                        YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                        YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                        IFLAGU,IFRST,ILAST,   &
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,   &
                          CCLSL,CCUSL,CCTARG,CCUSLC,   &
                          YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                          YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                          YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                          YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                          ICAPSW,ICAPTY,IFORSW,ICNPKD,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,   &
                        YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                        YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                        YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                        YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                        IFLAGU,IFRST,ILAST,   &
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
!
      IF(IERROR.EQ.'YES')THEN
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9001)(IANS(I),I=1,MIN(100,IWIDTH))
 9001     FORMAT(100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAAN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCAAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASAN
 9012   FORMAT('IFOUND,IERROR,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCAAN
      SUBROUTINE DPCAA2(Y,W,N,XTEMP1,MAXNXT,   &
                        CCLSL,CCUSL,CCTARG,CCUSLC,   &
                        YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                        YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                        YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                        YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                        ICAPSW,ICAPTY,IFORSW,ICNPKD,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE GENERATES A CAPABILITY ANALYSIS
!              TABULATION THE DATA IN THE INPUT VECTOR Y.
!     NOTE--NORMALITY IS ASSUMED
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                OF EQUALLY-SPACED OBSERVATIONS
!                                TO BE SMOOTHED.
!                       N      = THE INTEGER NUMBER OF
!                                OBSERVATIONS IN THE VECTOR Y.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--90/9
!     ORIGINAL VERSION--SEPTEMBER 1990.
!     UPDATED         --APRIL     2001.  EXPAND TABLE:
!                                        1) ADD CC, CPM, CPL, CPU,
!                                               CNPK
!                                        2) 95% CONFIDENCE INTERVAL
!                                           FOR CP, CPK, CPL, CPU, CPM
!                                        3) ADD COMPUTED STATS TO
!                                           CALL LIST SO THEY CAN BE
!                                           SAVED AS INTERNAL
!                                           PARAMETERS
!     UPDATED         --MAY       2011. USE DPDTA1 AND DPDTA5 TO PRINT
!                                       TABLES
!     UPDATED         --APRIL     2015. ADD "ICNPKD" TO CNPK CALL LIST
!     UPDATED         --APRIL     2015. ADDITIONAL CAPABILITY STATISTICS
!     UPDATED         --APRIL     2015. ADD CONFIDENCE LIMITS TABLES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICNPKD
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IFLAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION W(*)
      DIMENSION XTEMP1(*)
      DIMENSION PID(*)
!
      PARAMETER (NUMALP=5)
      PARAMETER (MAXROW=60)
      PARAMETER (NUMCLI=60)
      PARAMETER (MAXLIN=2)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*40 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      REAL         ALPHA(NUMALP)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*15 IVALUE(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA /50.0, 80.0, 90.0, 95.0, 99.0/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCA'
      ISUBN2='A2  '
!
      IERROR='NO'
      IWRITE='OFF'
!
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CAA2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPCAA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)CCLSL,CCUSL,CCTARG,CCUSLC
   54   FORMAT('CCLSL,CCUSL,CCTARG,CCUSLC = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),W(I)
   57     FORMAT('I,Y(I),W(I) = ',I8,2G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CAPABILITY ANALYSIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE IS LESS THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)N
  113   FORMAT('SAMPLE SIZE = ',I8)
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
!               **********************************************
!               **  STEP 3--                                **
!               **  COMPUTE VARIOUS CAPABILITY STATISTICS-- **
!               **     1) CP                                **
!               **     2) CPK                               **
!               **     3) PERCENT DEFECTIVE                 **
!               **     4) EXPECTED LOSS                     **
!               **********************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFLAG='BOTH'
!
      CALL MEAN(Y,N,IWRITE,XMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,XSD,IBUGA3,IERROR)
!
      CALL MEDIAN(Y,N,IWRITE,XTEMP1,MAXNXT,XMED,IBUGA3,IERROR)
      IF(ICNPKD.EQ.'PEAR')THEN
        P=99.865
        CALL PERCEN(P,Y,N,IWRITE,XTEMP1,MAXNXT,P995,IBUGA3,IERROR)
        P=0.135
        CALL PERCEN(P,Y,N,IWRITE,XTEMP1,MAXNXT,P005,IBUGA3,IERROR)
      ELSE
        P=99.5
        CALL PERCEN(P,Y,N,IWRITE,XTEMP1,MAXNXT,P995,IBUGA3,IERROR)
        P=0.5
        CALL PERCEN(P,Y,N,IWRITE,XTEMP1,MAXNXT,P005,IBUGA3,IERROR)
      ENDIF
!
      YCP=CPUMIN
      YCPLL=CPUMIN
      YCPUL=CPUMIN
      YCPK=CPUMIN
      YCPKLL=CPUMIN
      YCPKUL=CPUMIN
      YCNPK=CPUMIN
      YCPL=CPUMIN
      YCPLLL=CPUMIN
      YCPLUL=CPUMIN
      YCPU=CPUMIN
      YCPULL=CPUMIN
      YCPUUL=CPUMIN
      YCC=CPUMIN
      YCPM=CPUMIN
      YCPMLL=CPUMIN
      YCPMUL=CPUMIN
      YTHEPD=CPUMIN
      YTHEL=CPUMIN
      YTHEU=CPUMIN
      YACTPD=CPUMIN
      YACTL=CPUMIN
      YACTU=CPUMIN
      YEXPLO=CPUMIN
!
      IF(CCLSL.NE.CPUMIN.AND.CCUSL.NE.CPUMIN)THEN
        CALL CP(Y,N,CCLSL,CCUSL,IWRITE,YCP,YCPLL,YCPUL,   &
                IBUGA3,IERROR)
        CALL CPL(Y,N,CCLSL,CCUSL,IWRITE,YCPL,YCPLLL,YCPLUL,   &
                 IBUGA3,IERROR)
        CALL CPU(Y,N,CCLSL,CCUSL,IWRITE,YCPU,YCPULL,YCPUUL,   &
                 IBUGA3,IERROR)
        CALL CPK(Y,N,CCLSL,CCUSL,IWRITE,YCPK,YCPKLL,YCPKUL,   &
                 IBUGA3,IERROR)
        CALL CPM(Y,N,CCLSL,CCUSL,CCTARG,IWRITE,YCPM,YCPMLL,YCPMUL,   &
                 IBUGA3,IERROR)
        CALL CPMK(Y,N,CCLSL,CCUSL,CCTARG,IWRITE,YCPMK,YCPMLL,YCPMUL,   &
                  IBUGA3,IERROR)
        CALL CC(Y,N,CCLSL,CCUSL,CCTARG,IWRITE,YCC,   &
                IBUGA3,IERROR)
        CALL CNP(Y,N,XTEMP1,MAXNXT,CCLSL,CCUSL,IWRITE,ICNPKD,   &
                 YCNP,IBUGA3,IERROR)
        CALL CNPK(Y,N,XTEMP1,MAXNXT,CCLSL,CCUSL,IWRITE,ICNPKD,   &
                  YCNPK,IBUGA3,IERROR)
        CALL CNPM(Y,N,XTEMP1,MAXNXT,CCLSL,CCUSL,CCTARG,IWRITE,ICNPKD,   &
                  YCNPM,IBUGA3,IERROR)
        CALL CNPMK(Y,N,XTEMP1,MAXNXT,CCLSL,CCUSL,CCTARG,IWRITE,ICNPKD,   &
                   YCNPMK,IBUGA3,IERROR)
        CALL PERDEF(Y,N,CCLSL,CCUSL,IWRITE,YACTPD,YTHEPD,   &
                    YACTL,YTHEL,YACTU,YTHEU,   &
                    IFLAG,IBUGA3,IERROR)
        CALL EXPLOS(Y,N,CCLSL,CCUSL,CCUSLC,IWRITE,YEXPLO,   &
                    IBUGA3,IERROR)
      ENDIF
!
!               ****************************
!               **  STEP 7--              **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')   &
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
      ITITLE='Capability Analysis'
      NCTITL=19
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
        DO 2101 I=1,NREPL
          ICNT=ICNT+1
          ITEMP=I+IADD
          ITEXT(ICNT)='Factor Variable  : '
          WRITE(ITEXT(ICNT)(17:17),'(I1)')I
          WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(ITEMP)(1:4)
          WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(ITEMP)(1:4)
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=PID(ITEMP)
          IDIGIT(ICNT)=NUMDIG
 2101   CONTINUE
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
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
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Mean:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=XMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Standard Deviation:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=XSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Median:'
      NCTEXT(ICNT)=7
      AVALUE(ICNT)=XMED
      IDIGIT(ICNT)=NUMDIG
      IF(ICNPKD.EQ.'PEAR')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='0.135 Percentile:'
        NCTEXT(ICNT)=17
        AVALUE(ICNT)=P005
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='99.865 Percentile:'
        NCTEXT(ICNT)=18
        AVALUE(ICNT)=P995
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='0.5 Percentile:'
        NCTEXT(ICNT)=15
        AVALUE(ICNT)=P005
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='99.5 Percentile:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=P995
        IDIGIT(ICNT)=NUMDIG
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='User Specified Parameters:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Lower Specification Limit (LSL):'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=CCLSL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Upper Specification Limit (USL):'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=CCUSL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Target (Target):'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=CCTARG
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='USL Cost (USLCOST):'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=CCUSLC
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Normal-Based Capability Statistics:'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='CP:'
      NCTEXT(ICNT)=3
      AVALUE(ICNT)=YCP
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CPL:'
      NCTEXT(ICNT)=4
      AVALUE(ICNT)=YCPL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CPU:'
      NCTEXT(ICNT)=4
      AVALUE(ICNT)=YCPU
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CPK:'
      NCTEXT(ICNT)=4
      AVALUE(ICNT)=YCPK
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CPM:'
      NCTEXT(ICNT)=4
      AVALUE(ICNT)=YCPM
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CPMK:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=YCPMK
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CC:'
      NCTEXT(ICNT)=3
      AVALUE(ICNT)=YCC
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Actual Percent Defective:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=YACTPD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Theoretical Percent Defective:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=YTHEPD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Actual (Below) Percent Defective:'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=YACTL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Theoretical (Below) Percent Defective:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=YTHEL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Actual (Above) Percent Defective:'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=YACTU
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Theoretical (Above) Percent Defective:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=YTHEU
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Expected Loss:'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=YEXPLO
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Nonparametric Capability Statistics:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='CNP:'
      NCTEXT(ICNT)=4
      AVALUE(ICNT)=YCNP
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CNPK:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=YCNPK
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CNPM:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=YCNPM
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CNPMK:'
      NCTEXT(ICNT)=6
      AVALUE(ICNT)=YCNPMK
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
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
!     CONFIDENCE LIMITS TABLE FOR CP
!
      ITITLE=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
      NUMLIN=2
      NUMROW=NUMALP
      NUMCOL=4
!
      ITITL9='Confidence Limits for Cp Statistic'
      NCTIT9=34
!
      ITITL2(1,1)='Confidence'
      NCTIT2(1,1)=10
      ITITL2(2,1)='Value (%)'
      NCTIT2(2,1)=9
      ITITL2(1,2)='Value'
      NCTIT2(1,2)=5
      ITITL2(2,2)='of Cp'
      NCTIT2(2,2)=5
      ITITL2(1,3)='Lower'
      NCTIT2(1,3)=5
      ITITL2(2,3)='Limit'
      NCTIT2(2,3)=5
      ITITL2(1,4)='Lower'
      NCTIT2(1,4)=5
      ITITL2(2,4)='Limit'
      NCTIT2(2,4)=5
!
      NMAX=0
      DO 4221 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
        IWHTML(I)=150
        IF(I.EQ.1)THEN
          NTOT(I)=12
          IDIGIT(I)=3
          IWHTML(1)=75
        ENDIF
        NMAX=NMAX+NTOT(I)
 4221 CONTINUE
!
      AN=REAL(N)
      NV=N-1
      AV=REAL(NV)
!
      DO 4223 I=1,NUMROW
        DO 4225 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4225   CONTINUE
!
        PTEMP=ALPHA(I)/100.0
        PTEMPL=(1.0 - PTEMP)/2.0
        PTEMPU=1.0 - PTEMPL
        CALL CHSPPF(PTEMPL,NV,PPFL)
        ALOWER=0.0
        IF((PPFL/AV).GT.0.0)ALOWER=YCP*SQRT(PPFL/AV)
        CALL CHSPPF(PTEMPU,NV,PPFU)
        AUPPER=0.0
        IF((PPFU/AV).GT.0.0)AUPPER=YCP*SQRT(PPFU/AV)
        AMAT(I,1)=ALPHA(I)
        AMAT(I,2)=YCP
        AMAT(I,3)=ALOWER
        AMAT(I,4)=AUPPER
 4223 CONTINUE
!
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')   &
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
!     CONFIDENCE LIMITS TABLE FOR CPL
!
      ITITL9='Confidence Limits for Cpl Statistic'
      NCTIT9=35
      ITITL2(2,2)='of Cpl'
      NCTIT2(2,2)=6
!
      DO 4323 I=1,NUMROW
        DO 4325 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4325   CONTINUE
!
        PTEMP=ALPHA(I)/100.0
        PTEMPL=(1.0 - PTEMP)/2.0
        PTEMPU=1.0 - PTEMPL
        CALL NORPPF(PTEMPU,PPFU)
        ALOWER=0.0
        AUPPER=0.0
        IF(N.GT.1)THEN
          ALOWER=YCPL - PPFU*SQRT((1.0/(9.0*AN)) + YCPL/(2.0*(AN-1.0)))
          AUPPER=YCPL + PPFU*SQRT((1.0/(9.0*AN)) + YCPL/(2.0*(AN-1.0)))
        ENDIF
        AMAT(I,1)=ALPHA(I)
        AMAT(I,2)=YCPL
        AMAT(I,3)=ALOWER
        AMAT(I,4)=AUPPER
 4323 CONTINUE
!
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')   &
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
!     CONFIDENCE LIMITS TABLE FOR CPU
!
      ITITL9='Confidence Limits for Cpu Statistic'
      NCTIT9=35
      ITITL2(2,2)='of Cpu'
      NCTIT2(2,2)=6
!
      DO 4423 I=1,NUMROW
        DO 4425 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4425   CONTINUE
!
        PTEMP=ALPHA(I)/100.0
        PTEMPL=(1.0 - PTEMP)/2.0
        PTEMPU=1.0 - PTEMPL
        CALL NORPPF(PTEMPU,PPFU)
        ALOWER=0.0
        AUPPER=0.0
        IF(N.GT.1)THEN
          ALOWER=YCPU - PPFU*SQRT((1.0/(9.0*AN)) + YCPU/(2.0*(AN-1.0)))
          AUPPER=YCPU + PPFU*SQRT((1.0/(9.0*AN)) + YCPU/(2.0*(AN-1.0)))
        ENDIF
        AMAT(I,1)=ALPHA(I)
        AMAT(I,2)=YCPU
        AMAT(I,3)=ALOWER
        AMAT(I,4)=AUPPER
 4423 CONTINUE
!
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')   &
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
!     CONFIDENCE LIMITS TABLE FOR CPM
!
      ITITL9='Confidence Limits for Cpm Statistic'
      NCTIT9=35
      ITITL2(2,2)='of Cpm'
      NCTIT2(2,2)=6
!
      DO 4523 I=1,NUMROW
        DO 4525 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4525   CONTINUE
!
        PTEMP=ALPHA(I)/100.0
        PTEMPL=(1.0 - PTEMP)/2.0
        PTEMPU=1.0 - PTEMPL
        CALL CHSPPF(PTEMPL,NV,PPFL)
        ALOWER=0.0
        IF((PPFL/AV).GT.0.0)ALOWER=YCPM*SQRT(PPFL/AV)
        CALL CHSPPF(PTEMPU,NV,PPFU)
        AUPPER=0.0
        IF((PPFU/AV).GT.0.0)AUPPER=YCPM*SQRT(PPFU/AV)
        AMAT(I,1)=ALPHA(I)
        AMAT(I,2)=YCPM
        AMAT(I,3)=ALOWER
        AMAT(I,4)=AUPPER
 4523 CONTINUE
!
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')   &
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
!     CONFIDENCE LIMITS TABLE FOR CPK
!
      ITITL9='Confidence Limits for Cpk Statistic'
      NCTIT9=35
      ITITL2(2,2)='of Cpk'
      NCTIT2(2,2)=6
!
      DO 4623 I=1,NUMROW
        DO 4625 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4625   CONTINUE
!
        PTEMP=ALPHA(I)/100.0
        PTEMPL=(1.0 - PTEMP)/2.0
        PTEMPU=1.0 - PTEMPL
        ALOWER=0.0
        AUPPER=0.0
        CALL NORPPF(PTEMPU,PPFU)
        TERM1=1.0/(9.0*AN)
        TERM2=YCPK*YCPK/(2.0*(AN-1.0))
        ALOWER=YCPK - PPFU*SQRT(TERM1 + TERM2)
        AUPPER=YCPK + PPFU*SQRT(TERM1 + TERM2)
        AMAT(I,1)=ALPHA(I)
        AMAT(I,2)=YCPK
        AMAT(I,3)=ALOWER
        AMAT(I,4)=AUPPER
 4623 CONTINUE
!
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CAA2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCAA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IFLAG
 9014   FORMAT('IFLAG = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCAA2
      SUBROUTINE DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,   &
                        YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                        YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                        YCNPK,YCPM,YCPMLL,YCPMUL,YCC,   &
                        YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,   &
                        IFLAGU,IFRST,ILAST,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--UTILITY ROUTINE USED BY DPCAAN.  THIS ROUTINE
!              UPDATES VARIOUS PARAMETERS.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORAOTRY
!                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/5
!     ORIGINAL VERSION--MAY       2011.
!     UPDATED         --APRIL     2019. USER CAN SPECIFY NUMBER OF
!                                       DECIMAL POINTS FOR AUXILLARY
!                                       FILES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*20 IFORMT
!
!---------------------------------------------------------------------
!
      SAVE IOUNI1
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAA5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCAA5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)CCLSL,CCUSL,CCTARG,CCUSLC
   53   FORMAT('CCLSL,CCUSL,CCTARG,CCUSLC = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
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
  295     FORMAT(9X,'CPSTAT',11X,'CPLL',11X,'CPUL',   &
                 8X,'CPKSTAT',10X,'CPKLL',10X,'CPKUL',   &
                 8X,'CPLSTAT',10X,'CPLLL',10X,'CPLUL',   &
                 8X,'CPUSTAT',10X,'CPULL',10X,'CPUUL',   &
                 7X,'CNPKSTAT',   &
                 8X,'CPMSTAT',10X,'CPMLL',10X,'CPMUL',   &
                 7X,'ACTUALPD',7X,'ACTUALLL',7X,'ACTUALUL',   &
                 9X,'CCSTAT',8X,'THEORPD',8X,'THEORLL',   &
                 8X,'EXPLOSS')
        ENDIF
!
        IFORMT='(23E15.7)'
        IF(IAUXDP.NE.7)THEN
          IFORMT=' '
          IF(IAUXDP.LE.9)THEN
            IFORMT='(23Exx.x)'
            ITOT=IAUXDP+8
            WRITE(IFORMT(5:6),'(I2)')ITOT
            WRITE(IFORMT(8:8),'(I1)')IAUXDP
          ELSE
            IFORMT='(23Exx.xx)'
            ITOT=IAUXDP+8
            WRITE(IFORMT(5:6),'(I2)')ITOT
            WRITE(IFORMT(8:9),'(I2)')IAUXDP
          ENDIF
        ENDIF
!
        WRITE(IOUNI1,IFORMT)YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,   &
                         YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,   &
                         YCNPK,YCPM,YCPMLL,YCPMUL,YACTPD,YACTLL,YACTUL,   &
                         YCC,YTHERPD,YTHEL,YEXPLO
!C299   FORMAT(23E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IH='CPST'
        IH2='AT  '
        VALUE0=YCP
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPLL'
        IH2='    '
        VALUE0=YCPLL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPUL'
        IH2='    '
        VALUE0=YCPUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPKS'
        IH2='TAT '
        VALUE0=YCPK
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPKL'
        IH2='L   '
        VALUE0=YCPKLL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPKU'
        IH2='L   '
        VALUE0=YCPKUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPLS'
        IH2='TAT '
        VALUE0=YCPL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPLL'
        IH2='L   '
        VALUE0=YCPLLL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPLU'
        IH2='L   '
        VALUE0=YCPLUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPUS'
        IH2='TAT '
        VALUE0=YCPU
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPUL'
        IH2='L   '
        VALUE0=YCPULL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPUU'
        IH2='L   '
        VALUE0=YCPUUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CNPK'
        IH2='STAT'
        VALUE0=YCNPK
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPMS'
        IH2='TAT '
        VALUE0=YCPM
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPML'
        IH2='L   '
        VALUE0=YCPMLL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CPMU'
        IH2='L   '
        VALUE0=YCPMUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='CCST'
        IH2='AT  '
        VALUE0=YCC
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='ACTU'
        IH2='ALPD'
        VALUE0=YACTPD
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='THEO'
        IH2='RPD '
        VALUE0=YTHEPD
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='ACTU'
        IH2='ALLL'
        VALUE0=YACTL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='THEO'
        IH2='RLL '
        VALUE0=YTHEL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='ACTU'
        IH2='ALUL'
        VALUE0=YACTU
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='THEO'
        IH2='RUL '
        VALUE0=YTHEU
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='EXPL'
        IH2='OSS '
        VALUE0=YEXPLO
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAA5')THEN
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAA5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPCAA5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCAA5
      SUBROUTINE DPCAPA(IHARG,IARGT,ARG,NUMARG,   &
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
!     PURPOSE--DRAW ONE OR MORE CAPACITORS (DEPENDING ON HOW MANY NUMBERS ARE
!              PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
!              OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE THE BACK CENTER AND THE FRONT CENTER
!           OF THE CAPACITOR.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2
!          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
!     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN CAPACITOR WILL GO FROM
!           THE LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER ABSOLUTE OR
!           RELATIVE) AS DEFINED BY THE 2 NUMBERS.
!     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN CAPACITOR WILL GO FROM
!           THE ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST 2 NUMBERS TO
!           THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
!           THIRD AND FOURTH NUMBERS.
!     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN CAPACITOR WILL GO FROM
!           THE (X,Y) POSITION AS RESULTING FROM THE THIRD AND FOURTH NUMBERS
!           TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY
!           THE FIFTH AND SIXTH NUMBERS.
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
!                 PHONE--301-975-2899
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
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
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
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'CAPA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCAPA--')
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
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG = ',4G15.7)
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
        WRITE(ICOUT,88)IBUGG4,ISUBG4,IERRG4,IBUGD2,IFOUND,IERROR
   88   FORMAT('IBUGG4,ISUBG4,IERRG4,IBUGD2,IFOUND,IERROR = ',   &
               5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IFIG='CAPA'
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
 1131 FORMAT('***** ERROR IN DPCAPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR CAPICATOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW A CAPACITOR ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      WITH BACK CENTER AT 20 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      AND FRONT CENTER AT 40 60')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN THE ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      CAPACITOR 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      CAPACITOR ABSOLUTE 20 20 40 60 ')
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
      CALL DPCAP2(X1,Y1,X2,Y2,   &
                  IFIG,   &
                  ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                  AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                  IREFSW,IREFCO,IREFC2,   &
                  IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                  PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
      X1=X2
      Y1=Y2
!
      GO TO 1160
 1190 CONTINUE
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'CAPA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCAPA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,IFIG,ILOCFN,NUMNUM
 9012   FORMAT('IFOUND,IERROR,IFIG,ILOCFN,NUMNUM = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)X1,Y1,X2,Y2
 9013   FORMAT('X1,Y1,X2,Y2 = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)PXSTAR,PYSTAR,PXEND,PYEND
 9015   FORMAT('PXSTAR,PYSTAR,PXEND,PYEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCAPA
      SUBROUTINE DPCAP2(X1,Y1,X2,Y2,   &
                        IFIG,   &
                        ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!     PURPOSE--DRAW AN CAPACITOR WITH THE BACK CENTER AT (X1,Y1)
!              AND THE FRONT CENTER AT (X2,Y2).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      CHARACTER*4 IFIG
      CHARACTER*4 ILINPA
      CHARACTER*4 ILINCO
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
      DIMENSION PX(10)
      DIMENSION PY(10)
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
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'CAP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCAP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)X1,Y1,X2,Y2
   53   FORMAT('X1,Y1,X2,Y2 = ',4G15.7)
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
        WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),   &
                       PREPSP(1)
   65   FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',   &
               3(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)IREBC2(1,1),IREFC2(1,1),IREPC2(1,1)
   67   FORMAT('IREBC2(1,1),IREFC2(1,1),IREPC2(1,1) = ',3I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PTEXHE,PTEXWI,PTEXVG,PTEXHG
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IFIG,IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IFIG,IBUGG4,ISUBG4,IERRG4 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE CAPACITOR          **
!               *********************************
!
      DELX=X2-X1
      DELY=Y2-Y1
      LEN=INT(SQRT((X2-X1)**2+(Y2-Y1)**2))
      ALEN=LEN
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
!
      AJXMIN=PTEXWI
      AJXDEL=PTEXWI
      AJYDEL=PTEXHE
      AJXMAX=ALEN-AJXDEL
!
      XMIN=AJXMIN
      XDEL=AJXDEL
      YDEL=AJYDEL
      XMAX=AJXMAX
!
      K=0
!
      X=0
!CCCC Y=-ALEN/2.0
      Y=(-YDEL/2.0)
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=0
!CCCC Y=ALEN/2.0
      Y=YDEL/2.0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      NP=K
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
      K=0
!
      X=ALEN
!CCCC Y=-ALEN/2.0
      Y=(-YDEL/2.0)
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=ALEN
!CCCC Y=ALEN/2.0
      Y=YDEL/2.0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      NP=K
!
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
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'CAP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCAP2--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9039)IERRG4,NP
 9039   FORMAT('IERRG4,NP = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCAP2
      SUBROUTINE DPCAPT(ICOM,ICOM2,   &
                       ICAPSW,ICAPTY,ICAPSC,IPRDEF,   &
                       IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                       IANSLC,IANS,IWIDTH,   &
                       IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                       IOFILE,IBACCO,IBACC2,IGRASW,IDIASW,   &
                       PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,   &
                       PDIAHE,PDIAWI,PDIAVG,PDIAHG,   &
                       NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                       IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                       IDNVOF,IDNHOF,IDFONT,PDSCAL,   &
                       IREPCH,IMPSW,   &
                       IBUGS2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--INITIATE/TERMINATE A CAPTURE FILE FOR CAPTURING/REDIRECTING
!              ALPHANUMERIC OUTPUT (ONLY)--NOT EFFECT GRAPHICS OUTPUT.
!              THERE ARE 2 CAPABILITITES IN THIS REGARD--
!                 1) TURN THE CAPTURE SWITCH 'ON' WHICH WILL
!                    ALLOW A CAPTURE FILE TO BE OPENED.
!                 2) TURN THE CAPTURE SWITCH 'OFF' WHICH WILL TERMINATE
!                    THE ENTRY OF TEXT OUTPUT INTO THE CAPTURE FILE.
!     NOTE--THESE CAPABILITITIES WILL ALLOW THE ALPHANUMERIC OUTPUT (NOT
!           GRAPHICS OUTPUT) FROM ANY DATAPLOT COMMAND TO BE CAPTURED
!           (OR REDIRECTED) TO ANY FILE.  ALL SUBSEQUENT DATAPLOT
!           ALPHANUMERIC OUTPUT ARE AUTOMATICALLY DIVERTED FROM THE SCREEN
!           TO THE SPECIFIED SYSTEM FILE OR SUBFILE.  WHEN THE CAPTURE
!           SWITCH IS OFF, NO SUCH DIVERSION IS DONE.  THE SPECIFIED
!           STATUS (ON/OFF) OF THE CAPTURE WILL BE PLACED IN THE VARIABLE
!           ICAPSW.
!     INPUT  ARGUMENTS--ICOM
!                     --ICOM2
!                     --ICAPSW
!                     --ICAPTY
!                     --IANSLC (A  HOLLERITH VECTOR WHOSE
!                              I-TH ELEMENT CONTAINS THE
!                              I-TH CHARACTER OF THE
!                              ORIGINAL INPUT COMMAND LINE.
!                     --IWIDTH (AN INTEGER VARIABLE WHICH
!                              CONTAINS THE NUMBER OF CHARACTERS
!                              IN THE ORIGINAL COMMAND LINE.
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IBUG   (A HOLLERITH VARIABLE
!                               FOR DEBUGGING
!     PRIMARY CHANGED VARIABLE--IPR (IN COMMON)
!     OUTPUT ARGUMENTS--ICAPSW (AN INTEGER VARIABLE
!                              WHICH IF 'ON' INDICATES THAT
!                              CURRENT COMMANDS ARE
!                              BEING DIVERTED
!                              TO A CAPTURE TEXT; AND
!                              IF OFF INDICATES THAT
!                              A CAPTURE FILE IS NOT BEING CONSTRUCTED.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/6
!     ORIGINAL VERSION--JUNE      1989.
!     UPDATED         --JUNE      2002.  ADD SUPPORT FOR:
!                                        CAPTURE FLUSH
!                                        CAPTURE HTML FILE.
!                                        CAPTURE LATEX FILE.
!     UPDATED         --JANUARY   2003.  FOR CAPTURE HTML, OPTIONALLY
!                                        READ HEADER AND FOOTER FILES
!     UPDATED         --JULY      2003.  BUG: FILE NAME < 80
!                                        CHARACTERS, BUT COMMAND LINE
!                                        > 80 CHARACTERS
!     UPDATED         --SEPTEMBER 2003.  START IMPLEMENTING THE LATEX
!                                        CODE
!     UPDATED         --FEBRUARY  2005.  START IMPLEMENTING THE RTF
!                                        CODE
!     UPDATED         --DECEMBER  2005.  SUSPEND/RESUME CASES
!     UPDATED         --JANUARY   2006.  CAPTURE SCREEN <ON/OFF>
!     UPDATED         --FEBRUARY  2006.  ADD EPIC, EEPIC, GRAPHICS
!                                        PACKAGES TO LATEX PRE-AMBLE
!     UPDATED         --NOVEMBER  2008.  INITIALIZE HTML44 COMMON BLOCK
!     UPDATED         --APRIL     2012.  CAPTURE SCRIPT
!     UPDATED         --APRIL     2012.  CAPTURE FLUSH ERASE <ON/OFF>
!     UPDATED         --AUGUST    2015.  CAPTURE FUNCTION BLOCK
!     UPDATED         --DECEMBER  2015.  "NONE" OPTION FOR LATEX/HTML
!                                        HEADERS AND FOOTERS
!     UPDATED         --DECEMBER  2015.  SET CAPTURE SPLIT ON OPTION
!     UPDATED         --AUGUST    2016.  CAPTURE STATISTIC BLOCK
!     UPDATED         --DECEMBER  2018.  ADD PDSCAL TO CALL LIST TO DPERAS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 ICOM2
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICAPSC
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANSLC
      CHARACTER*4 IANS
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
      CHARACTER*4 IOFILE
!
      CHARACTER*240 IATEMP
      CHARACTER*1   ITEMP
!
      CHARACTER*1 IREPCH
      CHARACTER*4 IMPSW
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOPA.INC'
!
!CCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
!
!CCCC CHARACTER*80 IFILE2
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER*12 ISTAT2
      CHARACTER*12 IFORM2
      CHARACTER*12 IACCE2
      CHARACTER*12 IPROT2
      CHARACTER*12 ICURS2
      CHARACTER*4 IERRF2
      CHARACTER*4 IENDF2
      CHARACTER*4 IREWI2
!
      CHARACTER*4 IANSI
!CCCC CHARACTER*80 ICANS
      CHARACTER*200 ICANS
!
! ---------------------------------------------------------------------
!
      DIMENSION IANSLC(*)
      DIMENSION IANS(*)
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
      CHARACTER*4 IBACCO
      CHARACTER*4 IGRASW
      CHARACTER*4 IDIASW
!
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
!
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDCOLO
      CHARACTER*4 IDFONT
!
      CHARACTER*4 IFLAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFILQ2
      CHARACTER*1 IBASLC
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
      DIMENSION PDSCAL(*)
      DIMENSION IBACC2(*)
!
!-----COMMON----------------------------------------------------------
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      COMMON/HTML44/IFNTSZ
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOFB.INC'
      INCLUDE 'DPCOSB.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCA'
      ISUBN2='PT  '
      IFOUND='YES'
      IERROR='NO'
!
      IFILQ2=IFILQU
      IFILQU='ON'
      IH='UNKN'
      IH2='UNKN'
!
      KMIN=0
      KDEL=0
      KMAX=0
      JP3=0
      JP4=0
      JP5=0
      J12=0
      J22=0
      J32=0
      J42=0
      J52=0
      J62=0
      J72=0
      J82=0
      J92=0
      J102=0
      IPAR2=0
      IPAR3=0
      IPAR4=0
      IPAR5=0
      IPAR6=0
      IPAR7=0
      IPAR8=0
      IPAR9=0
      IPAR10=0
!
      P2=0.0
!
      CALL DPCONA(92,IBASLC)
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCAPT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICAPSW,ICAPTY,ICAPNU,ICAPCS,IPR,IPRDEF,NUMARG
   52   FORMAT('ICAPSW,ICAPTY,ICAPNU,ICAPCS,IPR,IPRDEF,NUMARG = ',   &
               2(A4,2X),I8,2X,A12,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGS2,IERROR,ICOM,ICOM2,IWIDTH
   54   FORMAT('IBUGS2,IERROR,ICOM,ICOM2,IWIDTH = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)(IANSLC(I),I=1,MIN(120,IWIDTH))
   55   FORMAT('IANSLC(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
        IF(NUMARG.GT.0)THEN
          DO 57 I=1,NUMARG
            WRITE(ICOUT,58)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   58       FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',   &
                   I8,3(2X,A4),I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   57     CONTINUE
        ENDIF
        WRITE(ICOUT,62)NUMNAM,MAXNAM,NUMCHA,ICAPNU
   62   FORMAT('NUMNAM,MAXNAM,NUMCHA,ICAPNU = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO 65 I=1,NUMNAM
          WRITE(ICOUT,66)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                         IVALUE(I),VALUE(I)
   66     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
                 I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        WRITE(ICOUT,73)(IA(I),I=1,MIN(100,NUMCHA))
   73   FORMAT('(IA(I),I=1,NUMCHA) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)ICAPNA
   82   FORMAT('ICAPNA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,83)ICAPST,ICAPFO,ICAPAC,ICAPFO
   83   FORMAT('ICAPST,ICAPFO,ICAPAC,ICAPCO = ',3(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,85)(IANS(I),I=1,MIN(100,IWIDTH))
   85   FORMAT('IANS(.) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************************
!               **  STEP 11--                                     **
!               **  FOR THE SPECIAL CASE WHEN THE CAPTURING       **
!               **  OF ALPHA TEXT HAS JUST BEEN FINISHED, JUMP    **
!               **  TO CLOSING THE FILE                           **
!               ****************************************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICAPCS.EQ.'CLO2        ')GO TO 5000
!
!               ***********************************************
!               **  STEP 12--                                **
!               **  FOR THE SPECIAL CASE WHEN HAVE THE       **
!               **  END CAPTURE     COMMAND, OR THE          **
!               **  END REDIRECT      COMMAND, OR THE        **
!               **  END OF CAPTURE      COMMAND,             **
!               **  END OF REDIRECT       COMMAND,           **
!               **  JUMP IMMEDIATELY TO THE SECTION OF CODE  **
!               **  WHICH PUTS ON AN END OF FILE AND         **
!               **  CLOSES THE FILE/SUBFILE.                 **
!               ***********************************************
!
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'END ')THEN
        IF(NUMARG.LE.0)GO TO 9000
        IF(IHARG(1).EQ.'CAPT')GO TO 4000
        IF(IHARG(1).EQ.'REDI')GO TO 4000
        IF(IHARG(1).EQ.'DIVE')GO TO 4000
        IF(IHARG(1).EQ.'PIPE')GO TO 4000
        IF(NUMARG.LE.1)GO TO 9000
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'CAPT')GO TO 4000
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'REDI')GO TO 4000
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'DIVE')GO TO 4000
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'PIPE')GO TO 4000
        GO TO 9000
      ELSEIF(ICOM.EQ.'FLUS')THEN
        IF(NUMARG.LE.0)GO TO 1290
        IF(IHARG(1).EQ.'CAPT')GO TO 6000
      ELSEIF(ICOM.EQ.'CAPT')THEN
        IF(NUMARG.LE.0)GO TO 1290
        IF(IHARG(1).EQ.'FLUS')GO TO 6000
      ENDIF
!
 1290 CONTINUE
!
!               ********************************************************
!               **  STEP 13--                                         **
!               **  DETERMINE THE TYPE CASE--                         **
!               **       1) CREATE AN EXPLICIT CAPTURE FILE;          **
!               **       2) OMIT THE FILE NAME;                       **
!               **  NOTE--IOFILE  WILL EQUAL 'YES' ONLY IN FILE CASE. **
!               **  IN OTHER WORDS, THIS STEP MAKES SURE              **
!               **  THAT A FILE NAME IS EXISTENT AFTER THE            **
!               **  CAPTURE   AND   REDIRECT   COMMANDS.              **
!               ********************************************************
!
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'SUSP')GO TO 2000
      IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'OFF ')GO TO 2000
      IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'RESU')GO TO 2000
      IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'ON  ')GO TO 2000
      IF(NUMARG.GE.1 .AND. IHARG(1).EQ.'SCRE')GO TO 2000
      IF(NUMARG.GE.2 .AND. IHARG(1).EQ.'FUNC' .AND.   &
         IHARG(2).EQ.'BLOC')GO TO 2000
      IF(NUMARG.GE.2 .AND. IHARG(1).EQ.'STAT' .AND.   &
         IHARG(2).EQ.'BLOC')GO TO 2000
!
!     2015/11: CHECK IF CAPTURE SWITCH IS ALREADY ON
!
      IF(ICAPSW.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,1211)
 1211     FORMAT('***** WARNING IN CAPTURE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1212)
 1212     FORMAT('      THE CAPTURE SWITCH IS ALREADY ON.  NOTHING ',   &
                 'DONE.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='WARN'
        GO TO 9000
      ENDIF
!
      IWORD=2
      IF(IHARG(1).EQ.'HTML'.OR.IHARG(1).EQ.'LATE'.OR.   &
         IHARG(1).EQ.'RTF '.OR.IHARG(1).EQ.'SCRI')IWORD=3
      CALL DPFILE(IANSLC,IWIDTH,IWORD,   &
                  IOFILE,IBUGS2,ISUBRO,IERROR)
!
!               **********************************************
!               **  STEP 14--                               **
!               **  IF NO FILE NAME GIVEN,                  **
!               **  THEN GENERATE AN ERROR MESSAGE.         **
!               **********************************************
!
      ISTEPN='14'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.NE.'YES')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1411)
 1411   FORMAT('***** ERROR IN CAPTURE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1412)
 1412   FORMAT('      THE DESIRED CAPTURE OPERATION CANNOT BE ',   &
               'PERFORMED BECAUSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1414)
 1414   FORMAT('      NO FILE NAME WAS GIVEN.  ILLUSTRATIVE EXAMPLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1416)
 1416   FORMAT('      TO DEMONSTRATE THE PROPER FORM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1417)
 1417   FORMAT('      SUPPOSE THE ANALYST WISHES TO CAPTURE TEXT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1419)
 1419   FORMAT('      OUTPUT TO THE FILE    TEMP1.  ;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1420)
 1420   FORMAT('      THEN THE FOLLOWING COMMAND LINE IS ENTERED--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1421)
 1421   FORMAT('         CAPTURE TEMP1.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               *************************************
!               **  STEP 15--                      **
!               **  IF HAVE THE FILE INPUT CASE    **
!               **  (WHICH WE MUST HAVE)--         **
!               **  COPY OVER VARIABLES            **
!               *************************************
!
      ISTEPN='15'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOUNIT=ICAPNU
      IFILE=ICAPNA
      ISTAT=ICAPST
      IF(IFILE.EQ.ISYSNA)ISTAT=ISYSST
      IF(IFILE.EQ.ILOGNA)ISTAT=ILOGST
      IFORM=ICAPFO
      IACCES=ICAPAC
      IPROT=ICAPPR
!     (SEE ADDITIONAL RESETTING OF   IPROT   BELOW
!     IF HAVE THE SYSTEM LOGIN AND/OR THE LOCAL LOGIN CAPTURE FILES)
      ICURST=ICAPCS
!
      ISUBN0='CAPT'
      IERRFI='NO'
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')THEN
        WRITE(ICOUT,1513)IOUNIT,ISUBN0,IERRFI
 1513   FORMAT('IOUNIT,ISUBN0,IERRFI = ',I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1514)IFILE
 1514   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1515)ISTAT,IFORM,IACCES,IPROT,ICURST
 1515   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***********************************************
!               **  STEP 16--                                **
!               **  IF HAVE THE FILE CASE (WHICH WE MUST     **
!               **  HAVE)--CHECK TO SEE IF THE CAPTURE FILE  **
!               **  MAY EXIST                                **
!               ***********************************************
!
      ISTEPN='16'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1412)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1614)
 1614   FORMAT('      THE INTERNAL VARIABLE   ICAPST   WHICH ALLOWS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1616)
 1616   FORMAT('      SUCH CAPTURE OPERATIONS HAS BEEN SET TO   NONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1617)ISTAT,ICAPST
 1617   FORMAT('ISTAT,ICAPST = ',A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1618)
 1618   FORMAT('      PLEASE CONTACT YOUR DATAPLOT IMPLEMENTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1619)
 1619   FORMAT('      TO CORRECT THE SETTING IN SUBROUTINE INITFO.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ********************************
!               **  STEP 17--                 **
!               **  EXTRACT THE FILE NAME.    **
!               **  THIS IS NEEDED FOR MOST   **
!               **  (BUT NOT ALL) VARIATIONS  **
!               **  OF THE CAPTURE COMMAND.   **
!               ********************************
!
      ISTEPN='17'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC JUNE 2002.  CHECK TO SEE IF FIRST ARGUMENT IS:
!CCCC             HTML
!CCCC             LATEX
!CCCC             RTF            (FEBRUARY 2005)
!CCCC             SCRIPT         (APRIL    2012)
!
      NSTRT=1
!
      IF(IHARG(1).EQ.'HTML')THEN
        ICAPTY='HTML'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1771)
 1771   FORMAT('THE CAPTURE OUTPUT WILL BE WRITTEN IN HTML FORMAT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IHARG(1).EQ.'LATE')THEN
        ICAPTY='LATE'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1791)
 1791   FORMAT('THE CAPTURE OUTPUT WILL BE WRITTEN IN LATEX FORMAT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IHARG(1).EQ.'RTF ')THEN
        ICAPTY='RTF '
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1793)
 1793   FORMAT('THE CAPTURE OUTPUT WILL BE WRITTEN IN ',   &
               'RTF (RICH TEXT FORMAT) FORMAT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IHARG(1).EQ.'SCRI')THEN
        ICAPTY='SCRI'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1795)
 1795   FORMAT('SCRIPT MODE TURNED ON FOR CAPTURE.  ALL ENTERED ',   &
               'COMMANDS WILL BE ECHOED, BUT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1797)
 1797   FORMAT('NOT EXECUTED, TO THE CAPTURE FILE UNTIL AN  ',   &
               'END OF CAPTURE  COMMAND IS ENTERED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 1710 I=1,200
        IANSI=IANSLC(I)
        ICANS(I:I)=IANSI(1:1)
 1710 CONTINUE
!
      ISTART=1
      ISTOP=IWIDTH
      IWORD=2
      IF(ICAPTY.EQ.'HTML')IWORD=3
      IF(ICAPTY.EQ.'LATE')IWORD=3
      IF(ICAPTY.EQ.'RTF ')IWORD=3
      IF(ICAPTY.EQ.'SCRI')IWORD=3
      CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,   &
                  ICOL1,ICOL2,IFILE,NCFILE,   &
                  IBUGS2,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(NCFILE.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1742)
 1742   FORMAT('      A USER FILE NAME IS REQUIRED IN THE ',   &
               'CAPTURE/REDIRECT COMMANDS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1744)
 1744   FORMAT('      (FOR EXAMPLE,    CAPTURE TEMP1.)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1745)
 1745   FORMAT('      BUT NONE WAS GIVEN HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1746)
 1746   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1747)(IANSLC(I),I=1,MIN(IWIDTH,100))
 1747     FORMAT('      ',100A1)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 9000
      ENDIF
!
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(IFILE.EQ.ISYSNA)IPROT=ISYSPR
      IF(IFILE.EQ.ILOGNA)IPROT=ILOGPR
!
!               *******************************************
!               **  STEP 20--                            **
!               **  CHECK THE DESIRED CAPTURE OPERATION  **
!               **  (ON, OFF, OR EXECUTE).               **
!               *******************************************
!
 2000 CONTINUE
!
      ISTEPN='20'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'CAPT' .OR. ICOM.EQ.'REDI' .OR. ICOM.EQ.'DIVE' .OR.   &
         ICOM.EQ.'PIPE')THEN
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'SUSP')GO TO 3800
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'OFF ')GO TO 3800
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'RESU')GO TO 3900
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'ON  ')GO TO 3900
!
!       2015/08: FUNCTION BLOCK CASE
!
        IF(NUMARG.GE.2 .AND. IHARG(1).EQ.'FUNC' .AND.   &
          IHARG(2).EQ.'BLOC')THEN
          ICAPTY='FUNB'
          IF(IHARG(3).EQ.'1' .OR. IHARG(3).EQ.'ONE')THEN
            IFBLSW='1'
          ELSEIF(IHARG(3).EQ.'2' .OR. IHARG(3).EQ.'TWO')THEN
            IFBLSW='2'
          ELSEIF(IHARG(3).EQ.'3' .OR. IHARG(3).EQ.'THREE')THEN
            IFBLSW='3'
          ELSE
            IFBLSW='OFF'
            IERROR='YES'
            WRITE(ICOUT,2116)
 2116       FORMAT('FOR   CAPTURE FUNCTION BLOCK  COMMAND, THE NEXT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2117)
 2117       FORMAT('ARGUMENT MUST BE ONE OF: ONE (OR 1), TWO (OR 2),',   &
                   ' OR THREE (OR 3).')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2118)IHARG(4)
 2118       FORMAT('THE ENTERED ARGUMENT WAS: ',A4)
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
!         NOW PARSE THE ARGUMENTS
!
!         FIRST, RETRIEVE THE NAME OF THE FUNCTION BLOCK.
!
          IF(NUMARG.GE.4)THEN
            IF(IFBLSW.EQ.'1')THEN
              IFBNA1(1:4)=IHARG(4)
              IFBNA1(5:8)=IHARG2(4)
            ELSEIF(IFBLSW.EQ.'2')THEN
              IFBNA2(1:4)=IHARG(4)
              IFBNA2(5:8)=IHARG2(4)
            ELSEIF(IFBLSW.EQ.'3')THEN
              IFBNA3(1:4)=IHARG(4)
              IFBNA3(5:8)=IHARG2(4)
            ENDIF
          ELSE
            IERROR='YES'
            IFBLSW='OFF'
            WRITE(ICOUT,22116)
22116       FORMAT('FOR THE  CAPTURE FUNCTION BLOCK  COMMAND, NO ',   &
                  'NAME WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
!         NEXT, RETRIEVE THE NAME OF THE PARAMETER/VARIABLE THAT WILL
!         CONTAIN THE RESPONSE (I.E., THE CALLING ROUTINE WILL EXTRACT
!         THE VALUE OF THIS PARAMETER/VARIABLE AFTER EXECUTING THE
!         FUNCTION BLOCK).
!
          IF(NUMARG.GE.5)THEN
            IF(IFBLSW.EQ.'1')THEN
              IFBAN1(1:4)=IHARG(5)
              IFBAN1(5:8)=IHARG2(5)
            ELSEIF(IFBLSW.EQ.'2')THEN
              IFBAN2(1:4)=IHARG(5)
              IFBAN2(5:8)=IHARG2(5)
            ELSEIF(IFBLSW.EQ.'3')THEN
              IFBAN3(1:4)=IHARG(5)
              IFBAN3(5:8)=IHARG2(5)
            ENDIF
          ELSE
            IERROR='YES'
            IFBLSW='OFF'
            WRITE(ICOUT,23126)
23126       FORMAT('FOR THE  CAPTURE FUNCTION BLOCK  COMMAND, NO ',   &
                  'RESPONSE PARAMETER/VARIABLE WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
!         THE REMAINING ARGUMENTS ARE THE PARAMETERS NEEDED BY THE
!         FUNCTION BLOCK,
!
          IF(NUMARG.GE.6)THEN
            ICNT=0
            DO 2150 II=6,NUMARG
              ICNT=ICNT+1
              IF(ICNT.LE.20)THEN
                IF(IFBLSW.EQ.'1')THEN
                  IFBPL1(ICNT)(1:4)=IHARG(II)
                  IFBPL1(ICNT)(5:8)=IHARG2(II)
                  IF(II.EQ.NUMARG)IFBCP1=ICNT
                ELSEIF(IFBLSW.EQ.'2')THEN
                  IFBPL2(ICNT)(1:4)=IHARG(II)
                  IFBPL2(ICNT)(5:8)=IHARG2(II)
                  IF(II.EQ.NUMARG)IFBCP2=ICNT
                ELSEIF(IFBLSW.EQ.'3')THEN
                  IFBPL3(ICNT)(1:4)=IHARG(II)
                  IFBPL3(ICNT)(5:8)=IHARG2(II)
                  IF(II.EQ.NUMARG)IFBCP3=ICNT
                ENDIF
              ENDIF
 2150       CONTINUE
            IFBCP1=MIN(ICNT,MAXFBP)
            IFBCP2=MIN(ICNT,MAXFBP)
            IFBCP3=MIN(ICNT,MAXFBP)
          ELSE
            IERROR='YES'
            IFBLSW='OFF'
            WRITE(ICOUT,22136)
22136       FORMAT('FOR THE  CAPTURE FUNCTION BLOCK  COMMAND, NO ',   &
                  'PARAMETER LIST WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
          GO TO 9000
!
!         2016/08: STATISTIC BLOCK CASE
!
        ELSEIF(NUMARG.GE.2 .AND. IHARG(1).EQ.'STAT' .AND.   &
          IHARG(2).EQ.'BLOC')THEN
          ICAPTY='STAB'
          IF(IHARG(3).EQ.'1' .OR. IHARG(3).EQ.'ONE')THEN
            ISBLSW='1'
          ELSEIF(IHARG(3).EQ.'2' .OR. IHARG(3).EQ.'TWO')THEN
            ISBLSW='2'
          ELSEIF(IHARG(3).EQ.'3' .OR. IHARG(3).EQ.'THREE')THEN
            ISBLSW='3'
          ELSE
            ISBLSW='OFF'
            IERROR='YES'
            WRITE(ICOUT,2126)
 2126       FORMAT('FOR   CAPTURE STATISTIC BLOCK  COMMAND, THE NEXT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2127)
 2127       FORMAT('ARGUMENT MUST BE ONE OF: ONE (OR 1), TWO (OR 2),',   &
                   ' OR THREE (OR 3).')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2128)IHARG(4)
 2128       FORMAT('THE ENTERED ARGUMENT WAS: ',A4)
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
!         NOW PARSE THE ARGUMENTS
!
!         FIRST, RETRIEVE THE NAME OF THE STATISTIC BLOCK.
!
          IF(NUMARG.GE.4)THEN
            IF(ISBLSW.EQ.'1')THEN
              ISBNA1(1:4)=IHARG(4)
              ISBNA1(5:8)=IHARG2(4)
            ELSEIF(ISBLSW.EQ.'2')THEN
              ISBNA2(1:4)=IHARG(4)
              ISBNA2(5:8)=IHARG2(4)
            ELSEIF(ISBLSW.EQ.'3')THEN
              ISBNA3(1:4)=IHARG(4)
              ISBNA3(5:8)=IHARG2(4)
            ENDIF
          ELSE
            IERROR='YES'
            ISBLSW='OFF'
            WRITE(ICOUT,22126)
22126       FORMAT('FOR THE  CAPTURE STATISTIC BLOCK  COMMAND, NO ',   &
                  'NAME WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
!         NEXT, RETRIEVE THE NAME OF THE PARAMETER THAT WILL
!         CONTAIN THE RESPONSE (I.E., THE CALLING ROUTINE WILL EXTRACT
!         THE VALUE OF THIS PARAMETER AFTER EXECUTING THE
!         STATISTIC BLOCK).
!
          IF(NUMARG.GE.5)THEN
            IF(ISBLSW.EQ.'1')THEN
              ISBAN1(1:4)=IHARG(5)
              ISBAN1(5:8)=IHARG2(5)
            ELSEIF(ISBLSW.EQ.'2')THEN
              ISBAN2(1:4)=IHARG(5)
              ISBAN2(5:8)=IHARG2(5)
            ELSEIF(ISBLSW.EQ.'3')THEN
              ISBAN3(1:4)=IHARG(5)
              ISBAN3(5:8)=IHARG2(5)
            ENDIF
          ELSE
            IERROR='YES'
            ISBLSW='OFF'
            WRITE(ICOUT,22127)
22127       FORMAT('FOR THE  CAPTURE STATISTIC BLOCK  COMMAND, NO ',   &
                  'RESPONSE PARAMETER WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
!         THE REMAINING ARGUMENTS ARE THE PARAMETERS NEEDED BY THE
!         STATISTIC BLOCK,
!
          IF(NUMARG.GE.6)THEN
            ICNT=0
            DO 2180 II=6,NUMARG
              ICNT=ICNT+1
              IF(ICNT.LE.20)THEN
                IF(ISBLSW.EQ.'1')THEN
                  ISBPL1(ICNT)(1:4)=IHARG(II)
                  ISBPL1(ICNT)(5:8)=IHARG2(II)
                  ISBCP1=ICNT
                ELSEIF(ISBLSW.EQ.'2')THEN
                  ISBPL2(ICNT)(1:4)=IHARG(II)
                  ISBPL2(ICNT)(5:8)=IHARG2(II)
                  ISBCP2=ICNT
                ELSEIF(ISBLSW.EQ.'3')THEN
                  ISBPL3(ICNT)(1:4)=IHARG(II)
                  ISBPL3(ICNT)(5:8)=IHARG2(II)
                  ISBCP3=ICNT
                ENDIF
              ENDIF
 2180       CONTINUE
            ISBCP1=MIN(ICNT,MAXSBP)
            ISBCP2=MIN(ICNT,MAXSBP)
            ISBCP3=MIN(ICNT,MAXSBP)
          ELSE
            IERROR='YES'
            ISBLSW='OFF'
            WRITE(ICOUT,22236)
22236       FORMAT('FOR THE  CAPTURE STATISTIC BLOCK  COMMAND, NO ',   &
                  'PARAMETER LIST WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
          GO TO 9000
        ENDIF
!
        IF(NUMARG.GE.1 .AND. IHARG(1).EQ.'SCRE')THEN
          ICAPSC='ON'
          IF(NUMARG.GE.2 .AND.   &
            (IHARG(2).EQ.'OFF ' .OR. IHARG(2).EQ.'END ' .OR.   &
             IHARG(2).EQ.'NO  ' .OR. IHARG(2).EQ.'NONE' .OR.   &
             IHARG(2).EQ.'CLOS'))ICAPSC='OFF '
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IF(IFEEDB.EQ.'ON')THEN
            IF(ICAPSC.EQ.'ON')THEN
              WRITE(ICOUT,2111)
 2111         FORMAT('CAPTURE OUTPUT WILL BE WRITTEN TO BOTH THE ',   &
                     'CAPTURE FILE AND THE SCREEN.')
            ELSE
              WRITE(ICOUT,2113)
 2113         FORMAT('CAPTURE OUTPUT WILL BE WRITTEN TO THE ',   &
                    'CAPTURE FILE ONLY.')
            ENDIF
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 9000
        ENDIF
        GO TO 3000
      ELSEIF(ICOM.EQ.'END '.AND.ICOM2.EQ.'    ')THEN
        IF(NUMARG.GE.1 .AND.   &
          (IHARG(1).EQ.'CAPT' .OR. IHARG(1).EQ.'REDI' .OR.   &
           IHARG(1).EQ.'DIVE' .OR. IHARG(1).EQ.'PIPE'))GO TO 4000
        IF(NUMARG.GE.1 .AND. IHARG(1).EQ.'OF  ' .AND.   &
          (IHARG(1).EQ.'CAPT' .OR. IHARG(1).EQ.'REDI' .OR.   &
           IHARG(1).EQ.'DIVE' .OR. IHARG(1).EQ.'PIPE'))GO TO 4000
      ENDIF
!
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1411)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1412)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2914)
 2914 FORMAT('      SPECIFIED OPERATION WAS ILLEGAL.  ILLUSTRATIVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2915)
 2915 FORMAT('      EXAMPLE TO DEMONSTRATE THE PROPER FORMS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2917)
 2917 FORMAT('         CAPTURE TEMP1.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2918)
 2918 FORMAT('         END OF CAPTURE')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               ********************************************************
!               **  STEP 30--                                         **
!               **  TREAT THE CAPTURE CASE.                           **
!               **  CARRY OUT WHATEVER SYSTEM OPERATIONS ARE NEEDED   **
!               **  IN ORDER TO OPERATE ON THE FILE OR SUBFILE.       **
!               **  FOR MOST INSTALLATIONS, THIS REQUIRES             **
!               **      1) AN OPENING OF THE FILE OR SUBFILE;         **
!               **      2) AN EQUIVALENCING OF THE FILE OR SUBFILE;   **
!               **      3) A  REWINDING OF THE FILE OR SUBFILE.       **
!               **  THE CODE BELOW OPENS THE FILE OR SUBFILE          **
!               **  (VIA @ASG,AX ON THE UNIVAC 1108).  THE CODE ALSO  **
!               **  EQUIVALENCES THE FILES OR SUBFILES (VIA @USE O    **
!               **  UNIVAC 1108) TO THE FORTRAN LOGICAL UNIT NUMBER   **
!               **  DESIGNATION IN THE VARIABLE ICAPNU (IN THE        **
!               **  SUBROUINTE INITFO);  THE CODE ALSO REWINDS THE    **
!               **  FILE OR SUBFILE. (VIA @REWIND ON THE UNIVAC 1108).**
!               ********************************************************
!
 3000 CONTINUE
      ISTEPN='30'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICAPSW='ON'
      IOUNIT=ICAPNU
!
      ICAPNA=IFILE
!
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
      ICAPCS=ICURST
!
!     2015/12: IF "SET CAPTURE SPLIT ON" IS GIVEN, OPEN A SECONDARY
!              CAPTURE FILE WITH "_1" APPENDED TO FILE NAME (BEFORE THE
!              ".").
!
      IF(ICAPSP.EQ.'ON')THEN
        ICAPCN=1
        ICAPN2=' '
        ICAPN2(1:80)=ICAPNA(1:80)
        ILAST=80
        IPEROD=-1
        DO 3001 JJ=80,1,-1
          IF(ICAPN2(JJ:JJ).NE.' ')THEN
            ILAST=JJ
            GO TO 3002
          ENDIF
 3001   CONTINUE
 3002   CONTINUE
!
        DO 3006 JJ=80,1,-1
          IF(ICAPN2(JJ:JJ).EQ.'.')THEN
            IPEROD=JJ
            GO TO 3007
          ENDIF
 3006   CONTINUE
 3007   CONTINUE
!
        IF(IPEROD.LE.0)THEN
          ILAST=ILAST+1
          ICAPN2(ILAST:ILAST+1)='.1'
        ELSE
          DO 3008 JJ=ILAST,IPEROD,-1
            ICAPN2(JJ+2:JJ+2)=ICAPN2(JJ:JJ)
 3008     CONTINUE
          ICAPN2(IPEROD:IPEROD+1)='_1'
        ENDIF
        IREWIN='ON'
        OPEN(ICPNU2,FILE=ICAPN2,STATUS="UNKNOWN",ACTION="WRITE")
        IF(IERRFI.EQ.'YES')GO TO 9000
      ENDIF
!
      IF(IFEEDB.EQ.'ON')THEN
        IF(ICAPTY.EQ.'RTF ')IRTFMD='OFF'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
 3011   FORMAT('THE CAPTURE SWITCH HAS JUST BEEN TURNED ON.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3012)ICAPNA
 3012   FORMAT('NAME OF CAPTURE FILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3013)
 3013   FORMAT('ALL SUBSEQUENT TEXT OUTPUT FROM ANY DATAPLOT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3014)
 3014   FORMAT('COMMAND WILL BE CAPTURED/REDIRECTED INTO THIS FILE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3015)
 3015   FORMAT('ONLY TEXT OUTPUT IS CAPTURED--NOT GRAPHICS OUTPUT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3016)
 3016   FORMAT('THE CAPTURED INFO WILL OVERWRITE THE PREVIOUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3017)
 3017   FORMAT('CONTENTS OF THE SPECIFIED FILE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3018)
 3018   FORMAT('THE TEXT CAPTURING WILL CONTINUE UNTIL YOU ENTER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3019)
 3019   FORMAT('THE COMMAND        END OF CAPTURE')
        CALL DPWRST('XXX','BUG ')
        IF(ICAPTY.EQ.'RTF ')IRTFMD='VERB'
      ENDIF
!
      IPR=ICAPNU
!
!CCCC JUNE 2002.  SPECIAL CASE OF GRAPHICS, LATEK, HTML, RTF OR SCRIPT.
!CCCC ADD ANY SPECIAL NEEDED INITIALIZATION CODE HERE.
!
!CCCC JANUARY 2003.  SET HTML HEADER FILE CAN BE USED TO SPECIFY A
!CCCC A FILE TO INCORPORATE THE HEADER FILE.
!
!CCCC DECEMBER 2015.  ADD OPTION "NONE" THAT SPECIFIES THAT NO HEADER
!CCCC IS GENERATED.  THIS IS SLIGHTLY DISTINCT FROM "NULL" WHICH
!CCCC GENERATES A MINIMAL HEADER.  THE "NONE" OPTION IS INTENDED FOR
!CCCC THE CASE WHERE YOU WANT TO INCORPORATE THE HTML CODE INTO A
!CCCC LARGER DISTINCT DOCUMENT.
!
      IF(ICAPTY.EQ.'HTML')THEN
        IFNTSZ=0
        IF(IHTMHE.EQ.'NONE')THEN
          CONTINUE
        ELSEIF(IHTMHE.EQ.'NULL')THEN
          WRITE(ICOUT,3071)
 3071     FORMAT('<HTML>')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3073)
 3073     FORMAT('<HEAD>')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3075)
 3075     FORMAT('<TITLE>')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3077)
 3077     FORMAT('Dataplot Output')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3079)
 3079     FORMAT('</TITLE>')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3081)
 3081     FORMAT('<META HTTP-EQUIV="Content-Type" CONTENT="text/html;',   &
                 ' charset=iso-8859-1">')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3083)
 3083     FORMAT('</HEAD>')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3085)
 3085     FORMAT('<BODY BGCOLOR=#FFFFFF>')
          CALL DPWRST('XXX','WRIT')
        ELSE
          IOUNI2=IST1NU
          IFILE2=IHTMHE
          ISTAT2='OLD'
          IFORM2='FORMATTED'
          IACCE2='SEQUENTIAL'
          IPROT2='READONLY'
          ICURS2='CLOSED'
          ISUBN0='CAPT'
          IERRF2='NO'
!
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
!
!  NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
!
          DO 3091 I=1,1000
            IATEMP=' '
            READ(IOUNI2,3092,END=3099,ERR=3099)IATEMP
 3092       FORMAT(A240)
            ILAST=1
            DO 3096 J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GO TO 3098
              ENDIF
 3096       CONTINUE
 3098       CONTINUE
            WRITE(ICOUT,3094)(IATEMP(J:J),J=1,ILAST)
            NCOUT=ILAST
 3094       FORMAT(240A1)
            CALL DPWRST('XXX','WRIT')
 3091     CONTINUE
 3099     CONTINUE
          IENDF2='OFF'
          IREWI2='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
        ENDIF
        WRITE(ICOUT,3087)
 3087   FORMAT('<PRE>')
        CALL DPWRST('XXX','WRIT')
      ELSEIF(ICAPTY.EQ.'LATE')THEN
!
!CCCC DECEMBER 2015.  ADD OPTION "NONE" THAT SPECIFIES THAT NO HEADER
!CCCC IS GENERATED.  THIS IS SLIGHTLY DISTINCT FROM "NULL" WHICH
!CCCC GENERATES A MINIMAL HEADER.  THE "NONE" OPTION IS INTENDED FOR
!CCCC THE CASE WHERE YOU WANT TO INCORPORATE THE LATEX CODE INTO A
!CCCC LARGER DISTINCT DOCUMENT.
!
        IF(ILATHE.EQ.'NONE')THEN
          CONTINUE
        ELSEIF(ILATHE.EQ.'NULL')THEN
          IF(ILATPS.EQ.12)THEN
            WRITE(ICOUT,3171)IBASLC
 3171       FORMAT(A1,'documentclass[12pt]{article}')
            CALL DPWRST('XXX','WRIT')
          ELSE
            IF(ILATPS.GE.10)THEN
              WRITE(ICOUT,3172)IBASLC,ILATPS
 3172         FORMAT(A1,'documentclass[',I2,'pt]{article}')
              CALL DPWRST('XXX','WRIT')
            ELSE
              WRITE(ICOUT,33172)IBASLC,ILATPS
33172         FORMAT(A1,'documentclass[',I1,'pt]{article}')
              CALL DPWRST('XXX','WRIT')
            ENDIF
          ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3173)IBASLC
 3173     FORMAT(A1,'usepackage{epsfig}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3174)IBASLC
 3174     FORMAT(A1,'usepackage{epic,eepic}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3175)IBASLC
 3175     FORMAT(A1,'usepackage{graphics,color}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13171)IBASLC,IBASLC
13171     FORMAT(A1,'setlength{',A1,'textwidth}{6.25in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13172)IBASLC,IBASLC
13172     FORMAT(A1,'setlength{',A1,'textheight}{9in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13173)IBASLC,IBASLC
13173     FORMAT(A1,'setlength{',A1,'oddsidemargin}{0.25in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13174)IBASLC,IBASLC
13174     FORMAT(A1,'setlength{',A1,'evensidemargin}{0in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13175)IBASLC,IBASLC
13175     FORMAT(A1,'setlength{',A1,'headheight}{0.5in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13176)IBASLC,IBASLC
13176     FORMAT(A1,'setlength{',A1,'headsep}{0.5in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13177)IBASLC,IBASLC
13177     FORMAT(A1,'setlength{',A1,'topmargin}{-1in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13178)IBASLC,IBASLC
13178     FORMAT(A1,'setlength{',A1,'parindent}{0in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13179)IBASLC,IBASLC
13179     FORMAT(A1,'setlength{',A1,'parskip}{10pt}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13180)IBASLC,IBASLC
13180     FORMAT(A1,'setlength{',A1,'textfloatsep}{4ex}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13181)IBASLC,IBASLC
13181     FORMAT(A1,'addtolength{',A1,'footskip}{0.25in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13182)IBASLC
13182     FORMAT(A1,'overfullrule=0pt')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,13183)IBASLC
13183     FORMAT(A1,'baselineskip=12pt')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3181)IBASLC,IBASLC,IBASLC
 3181     FORMAT(A1,'newcommand{',A1,'PGRAPHIC}[1]{',A1,'begin{figure}',   &
                 '[h]')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3182)IBASLC
 3182     FORMAT(23X,A1,'epsfig{file=#1,width=6.0in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3183)IBASLC
 3183     FORMAT(23X,A1,'end{figure}}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3186)IBASLC,IBASLC,IBASLC
 3186     FORMAT(A1,'newcommand{',A1,'LGRAPHIC}[1]{',A1,'begin{figure}',   &
                 '[h]')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3187)IBASLC
 3187     FORMAT(23X,A1,'epsfig{file=#1,angle=-90,width=6.0in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3188)IBASLC
 3188     FORMAT(23X,A1,'end{figure}}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3191)IBASLC
 3191     FORMAT(A1,'begin{document}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3197)IBASLC
 3197     FORMAT(A1,'begin{verbatim}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
        ELSE
          IOUNI2=IST1NU
          IFILE2=ILATHE
          ISTAT2='OLD'
          IFORM2='FORMATTED'
          IACCE2='SEQUENTIAL'
          IPROT2='READONLY'
          ICURS2='CLOSED'
          ISUBN0='CAPT'
          IERRF2='NO'
!
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
!
!  NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
!
          DO 3291 I=1,1000
            IATEMP=' '
            READ(IOUNI2,3292,END=3299,ERR=3299)IATEMP
 3292       FORMAT(A240)
            ILAST=1
            DO 3296 J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GO TO 3298
              ENDIF
 3296       CONTINUE
 3298       CONTINUE
            WRITE(ICOUT,3294)(IATEMP(J:J),J=1,ILAST)
            NCOUT=ILAST
 3294       FORMAT(240A1)
            CALL DPWRST('WRIT','BUG ')
 3291     CONTINUE
 3299     CONTINUE
          IENDF2='OFF'
          IREWI2='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
          WRITE(ICOUT,3197)IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
      ELSEIF(ICAPTY.EQ.'RTF ')THEN
        IRTFMD='OFF'
!CCCC   IF(IRTFHE.EQ.'NULL')THEN
          WRITE(ICOUT,3351)IBASLC,IBASLC,IBASLC
 3351     FORMAT('{',A1,'rtf1',A1,'ansi',A1,'deff0')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3361)IBASLC
 3361     FORMAT('{',A1,'fonttbl')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3363)IBASLC,IBASLC
 3363     FORMAT('{',A1,'f0',A1,'froman Times New Roman;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3367)IBASLC,IBASLC
 3367     FORMAT('{',A1,'f1',A1,'fmodern Courier New;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3369)IBASLC,IBASLC
 3369     FORMAT('{',A1,'f2',A1,'froman Arial;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3371)IBASLC,IBASLC
 3371     FORMAT('{',A1,'f3',A1,'froman Bookman;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3373)IBASLC,IBASLC
 3373     FORMAT('{',A1,'f4',A1,'froman Georgia;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3375)IBASLC,IBASLC
 3375     FORMAT('{',A1,'f5',A1,'fswiss Tahoma;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3376)IBASLC,IBASLC
 3376     FORMAT('{',A1,'f6',A1,'fswiss Lucida Sans;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3377)IBASLC,IBASLC
 3377     FORMAT('{',A1,'f7',A1,'fswiss Verdana;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3378)IBASLC,IBASLC
 3378     FORMAT('{',A1,'f8',A1,'fmodern Lucida Console;}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3379)
 3379     FORMAT('}')
          CALL DPWRST('XXX','WRIT')
!
          WRITE(ICOUT,3384)IBASLC
 3384     FORMAT('{',A1,'info')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3385)IBASLC
 3385     FORMAT('{',A1,'title Dataplot RTF Document}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3386)IBASLC
 3386     FORMAT('{',A1,'author Alan Heckert}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3387)IBASLC
 3387     FORMAT('{',A1,'company Statistical Engineering Division, ',   &
                 'NIST}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3379)
          CALL DPWRST('XXX','WRIT')
!
!CCCC     IPTSZ=2*IRTFPS
          IPTSZ=IRTFPS
          IF(IPTSZ.LT.0 .OR. IPTSZ.GT.99)IPTSZ=20
          ITEMP='0'
          IF(IRTFFP.EQ.'Arial')ITEMP='2'
          IF(IRTFFP.EQ.'Bookman')ITEMP='3'
          IF(IRTFFP.EQ.'Georgia')ITEMP='4'
          IF(IRTFFP.EQ.'Tahoma')ITEMP='5'
          IF(IRTFFP.EQ.'Lucida Sans')ITEMP='6'
          IF(IRTFFP.EQ.'Verdana')ITEMP='7'
          IF(IPTSZ.LE.9)THEN
            WRITE(ICOUT,3381)IBASLC,IBASLC,IBASLC,IBASLC,ITEMP,   &
                             IBASLC,IPTSZ
 3381       FORMAT(A1,'delang1033',A1,'widowctrl',A1,'plain',   &
                   A1,'f',A1,A1,'fs',I1)
          ELSE
            WRITE(ICOUT,3382)IBASLC,IBASLC,IBASLC,IBASLC,ITEMP,   &
                             IBASLC,IPTSZ
 3382       FORMAT(A1,'delang1033',A1,'widowctrl',A1,'plain',   &
                   A1,'f',A1,A1,'fs',I2)
          ENDIF
          CALL DPWRST('XXX','WRIT')
!
          WRITE(ICOUT,3389)IBASLC
 3389     FORMAT('{',A1,'pard')
          CALL DPWRST('XXX','WRIT')
          IRTFMD='VERB'
!CCCC   ELSE
!CCCC   ENDIF
      ELSEIF(ICAPTY.EQ.'SCRI')THEN
        CONTINUE
      ENDIF
!
      GO TO 9000
!
!               ******************************************************
!               **  STEP 38--                                       **
!               **  TREAT THE CAPTURE SUSPEND CASE.                 **
!               **  RESET OUTPUT UNIT TO IPR, BUT DO NOT CLOSE      **
!               **  THE CAPTURE FILE.                               **
!               ******************************************************
!
 3800 CONTINUE
      ISTEPN='38'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICAPSW.EQ.'OFF')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3811)
 3811   FORMAT('****** WARNING: THE CAPTURE SWITCH IS CURRENTLY OFF.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3813)
 3813   FORMAT('       CAPTURE SUSPEND COMMAND IGNORED.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      ICAPSW='OFF'
      IOUNIT=ICAPNU
      IPR=IPRDEF
!
      GO TO 9000
!
!               ******************************************************
!               **  STEP 39--                                       **
!               **  TREAT THE CAPTURE RESUME  CASE.                 **
!               **  RESET OUTPUT UNIT TO CAPTURE UNIT, BUT DO NOT   **
!               **  REOPEN THE CAPTURE FILE.                        **
!               ******************************************************
!
 3900 CONTINUE
      ISTEPN='39'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICAPSW.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3911)
 3911   FORMAT('****** WARNING: THE CAPTURE SWITCH IS CURRENTLY ON.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3913)
 3913   FORMAT('       CAPTURE RESUME COMMAND IGNORED.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      ICAPSW='ON'
      IPR=ICAPNU
!
      GO TO 9000
!
!               **********************************************************
!               **  STEP 40--                                           **
!               **  TREAT THE END OF CAPTURE CASE.  CARRY OUT WHATEVER  **
!               **  SYSTEM OPERATIONS ARE NEEDED IN ORDER TO OPERATE    **
!               **  ON THE FILE OR SUBFILE. FOR MOST INSTALLATIONS,     **
!               **  THIS REQUIRES                                       **
!               **      1) A PLACING OF AN END MARK OF THE FILE OR      **
!               **         SUBFILE;                                     **
!               **      2) A FREEING (DEASSIGNING) OF THE FILE OR       **
!               **         SUBFILE;                                     **
!               **********************************************************
!
 4000 CONTINUE
      ISTEPN='40'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICAPSW='OFF'
!
      IF(IFBLSW.NE.'OFF')THEN
        ICAPTY='TEXT'
        IFBLSW='OFF'
        GO TO 4090
      ENDIF
!
      IF(ISBLSW.NE.'OFF')THEN
        ICAPTY='TEXT'
        ISBLSW='OFF'
        GO TO 4090
      ENDIF
!
!CCCC JUNE 2002.  SPECIAL CASE OF GRAPHICS, LATEK, OR HTML.  ADD
!CCCC ANY SPECIAL NEED TERMINATION CODE HERE.
!
!CCCC DECEMBER 2015.  FOR HTML AND LATEX, ADD "NONE" OPTION FOR FOOTER.
!CCCC THIS IS DISTINCT FROM "NULL" WHICH ADDS A MINIMAL FOOTER.  THE
!CCCC "NONE" OPTION IS INTENDED FOR THE CASE WHERE THE HTML OR LATEX
!CCCC CODE IS TO BE INCORPORATED INTO A LARGER HTML OR LATEX DOCUMENT.
!
      IF(ICAPTY.EQ.'HTML')THEN
        WRITE(ICOUT,4110)
 4110   FORMAT('</PRE>')
        CALL DPWRST('XXX','WRIT')
        IF(IHTMFO.EQ.'NONE')THEN
          CONTINUE
        ELSEIF(IHTMFO.EQ.'NULL')THEN
          WRITE(ICOUT,4112)
 4112     FORMAT('</BODY>')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,4114)
 4114     FORMAT('</HTML>')
          CALL DPWRST('XXX','WRIT')
        ELSE
          IOUNI2=IST1NU
          IFILE2=IHTMFO
          ISTAT2='OLD'
          IFORM2='FORMATTED'
          IACCE2='SEQUENTIAL'
          IPROT2='READONLY'
          ICURS2='CLOSED'
          ISUBN0='CAPT'
          IERRF2='NO'
!
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
!
!  NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
!
          DO 4121 I=1,1000
            IATEMP=' '
            READ(IOUNI2,4122,END=4129,ERR=4129)IATEMP
 4122       FORMAT(A240)
            ILAST=1
            DO 4126 J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GO TO 4128
              ENDIF
 4126       CONTINUE
 4128       CONTINUE
            WRITE(ICOUT,4124)(IATEMP(J:J),J=1,ILAST)
            NCOUT=ILAST
 4124       FORMAT(240A1)
            CALL DPWRST('XXX','WRIT')
 4121     CONTINUE
 4129     CONTINUE
          IENDF2='OFF'
          IREWI2='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
        ENDIF
      ELSEIF(ICAPTY.EQ.'LATE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,4208)IBASLC
 4208   FORMAT(A1,'end{verbatim}')
        CALL DPWRST('XXX','WRIT')
        IF(ILATFO.EQ.'NONE')THEN
          CONTINUE
        ELSEIF(ILATFO.EQ.'NULL')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,4210)IBASLC
 4210     FORMAT(A1,'end{document}')
          CALL DPWRST('XXX','WRIT')
        ELSE
          IOUNI2=IST1NU
          IFILE2=ILATFO
          ISTAT2='OLD'
          IFORM2='FORMATTED'
          IACCE2='SEQUENTIAL'
          IPROT2='READONLY'
          ICURS2='CLOSED'
          ISUBN0='CAPT'
          IERRF2='NO'
!
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
!
!  NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
!
          DO 4221 I=1,1000
            IATEMP=' '
            READ(IOUNI2,4222,END=4229,ERR=4229)IATEMP
 4222       FORMAT(A240)
            ILAST=1
            DO 4226 J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GO TO 4228
              ENDIF
 4226       CONTINUE
 4228       CONTINUE
            WRITE(ICOUT,4224)(IATEMP(J:J),J=1,ILAST)
            NCOUT=ILAST
 4224       FORMAT(240A1)
            CALL DPWRST('XXX','WRIT')
 4221     CONTINUE
 4229     CONTINUE
          IENDF2='OFF'
          IREWI2='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
        ENDIF
      ELSEIF(ICAPTY.EQ.'RTF ')THEN
        IRTFMD='OFF'
        WRITE(ICOUT,4301)IBASLC
 4301   FORMAT(A1,'par}')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,4303)
 4303   FORMAT('}')
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      ICAPTY='TEXT'
      IOUNIT=ICAPNU
      IPR=IPRDEF
!
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
!
 4090 CONTINUE
      IF(IFEEDB.EQ.'ON')THEN
        IF(ICAPTY.EQ.'RTF ')IRTFMD='OFF'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4011)
 4011   FORMAT('THE CAPTURE SWITCH HAS JUST BEEN TURNED OFF.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4012)ICAPNA
 4012   FORMAT('NAME OF (JUST-CLOSED) CAPTURE FILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4013)
 4013   FORMAT('ALL FUTURE TEXT OUTPUT WILL NOW REVERT TO ',   &
               'THE SCREEN.')
        CALL DPWRST('XXX','BUG ')
        IF(ICAPTY.EQ.'RTF ')IRTFMD='VERB'
      ENDIF
      GO TO 9000
!
!               ****************************************************************
!               **  STEP 50--
!               **  TREAT THE CAPTURE FILE CLOSE CASE.
!               ****************************************************************
!
 5000 CONTINUE
      ISTEPN='50'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC ICAPSW='OFF'
!CCCC JUNE 2002. SUPPORT FOR SPECIAL CAPTURE OPERATIONS.
!CCCC IF(ICAPTY.EQ.'GRAP')THEN
!CCCC   IPR=IPRDEF
      IF(ICAPTY.EQ.'HTML')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,5111)
 5111   FORMAT('</PRE>')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,5113)
 5113   FORMAT('</BODY>')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,5115)
 5115   FORMAT('</HTML>')
        CALL DPWRST('XXX','WRIT')
      ELSEIF(ICAPTY.EQ.'LATE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,5208)IBASLC
 5208   FORMAT(A1,'end{verbatim}')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,5210)IBASLC
 5210   FORMAT(A1,'end{document}')
        CALL DPWRST('XXX','WRIT')
      ELSEIF(ICAPTY.EQ.'RTF ')THEN
        IRTFMD='OFF'
        WRITE(ICOUT,5301)IBASLC
 5301   FORMAT(A1,'par}')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,5303)
 5303   FORMAT('}')
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      ICAPTY='TEXT'
      IOUNIT=ICAPNU
!
      IENDFI='OFF'
!     ***** DO WE NEED THE FOLLOWING REWIND ????? *****
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
!
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'CAPT')GO TO 5019
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5011)ICAPNU
 5011 FORMAT('THE CAPTURE FILE NUMBER ',I8,' HAS JUST BEEN CLOSED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5012)ICAPNA
 5012 FORMAT('NAME OF (JUST-CLOSED) CAPTURE FILE = ',A80)
      CALL DPWRST('XXX','BUG ')
 5019 CONTINUE
      GO TO 9000
!
!     **********************************************************
!     **  STEP 60--                                           **
!     **  TREAT THE FLUSH  CAPTURE CASE.                      **
!     **      1) CLEAR GRAPHICS SCREEN (DPERAS)               **
!     **      2) CLOSE CAPTURE FILE (IF CURRENTLY OPEN)       **
!     **      3) OPEN THE CAPTURE FILE                        **
!     **      4) LOOP THROUGH THE FILE AND CALL DPWRSG        **
!     **      5) CLOSE THE CAPTURE FILE                       **
!     **      6) RE-OPEN THE CAPTURE FILE                     **
!     **********************************************************
!
 6000 CONTINUE
      ISTEPN='40'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!
!  STEP 2: CLEAR THE GRAPHICS SCREEN
!          (SKIP IF MULTIPLOTTING ON)
!
      IF(IMPSW.NE.'ON' .AND. ICAPFE.EQ.'ON')THEN
        CALL DPERAS(IHARG,IARGT,IARG,NUMARG,   &
                    IBACCO,IBACC2,IGRASW,IDIASW,   &
                    PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,   &
                    PDIAHE,PDIAWI,PDIAVG,PDIAHG,   &
                    NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                    IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                    IDNVOF,IDNHOF,IDFONT,PDSCAL,   &
                    ICAPSW,IBUGS2,ISUBRO,IFOUND,IERROR)
      ENDIF
!
!  STEP 2: CLOSE THE FILE
!
      IOUNIT=ICAPNU
      IFILE=ICAPNA
      ISTAT=ICAPST
      IFORM=ICAPFO
      IACCES=ICAPAC
      IPROT=ICAPPR
      ICURST=ICAPCS
      ICURST=ICAPCS
      IF(ICAPCS.EQ.'CLOSED')GO TO 6090
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
!
 6090 CONTINUE
!
!  STEP 3: RE-OPEN THE FILE
!
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
      ICAPCS=ICURST
!
!  STEP 4: LOOP THROUGH THE FILE
!
      ILINE=0
      ICOUNT=1
      DO 6110 I=1,10000
        ICOUT=' '
        READ(ICAPNU,'(A120)',END=6129,ERR=6119)ICOUT
        ILINE=ILINE+1
        IF(ILINE.GT.ICAPLI(ICOUNT).AND.IMPSW.NE.'ON')THEN
          CALL DPERAS(IHARG,IARGT,IARG,NUMARG,   &
                      IBACCO,IBACC2,IGRASW,IDIASW,   &
                      PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,   &
                      PDIAHE,PDIAWI,PDIAVG,PDIAHG,   &
                      NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                      IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                      IDNVOF,IDNHOF,IDFONT,PDSCAL,   &
                      ICAPSW,IBUGS2,ISUBRO,IFOUND,IERROR)
          ILINE=1
          ICOUNT=ICOUNT+1
          IF(ICOUNT.GT.MAXCLI)ICOUNT=1
        ENDIF
        IF(I.EQ.1)THEN
          IFLAG='INIT'
        ELSEIF(ILINE.EQ.1)THEN
          IFLAG='NEW'
        ELSE
          IFLAG='OLD'
        ENDIF
        CALL DPWRSG('XXXX','BUG ',IREPCH,IMPSW,IFLAG,ICAPNM,ICAPBX,   &
                    ILINE)
 6110 CONTINUE
 6119 CONTINUE
 6129 CONTINUE
!
!  STEP 5: CLOSE THE FILE
!
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      ICAPCS=ICURST
      IF(IERRFI.EQ.'YES')GO TO 9000
!
!  STEP 6: RE-OPEN THE FILE
!
      IFILE=ICAPNA
      IOUNIT=ICAPNU
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
      ICAPCS=ICURST
!
      GO TO 9000
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
!
      IFILQU=IFILQ2
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCAPT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IBUGS2,IFOUND,IERROR
 9013   FORMAT('IBUGS2,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ICOM,ICOM2,IOFILE,IWIDTH,IOUNIT
 9015   FORMAT('ICOM,ICOM2,IOFILE,IWIDTH,IOUNIT = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)(IANSLC(I),I=1,MIN(120,IWIDTH))
 9017   FORMAT('IANSLC(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)JP3,JP4,JP5,KMIN,KDEL,KMAX
 9031   FORMAT('JP2,JP3,JP4,KMIN,KDEL,KMAX = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9052)IFILE
 9052   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9053)ISTAT,IFORM,IACCES,IPROT,ICURST
 9053   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST  = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9058)IENDFI,IREWIN,ISUBN0,IERRFI
 9058   FORMAT('IENDFI,IREWIN,ISUBN0,IERRFI = ',2(A4,2X),A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCAPT
      SUBROUTINE DPCASE(ICOM,IHARG,NUMARG,IDEFCA,ITEXCA,   &
                        IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE CASE (UPPER OR LOWER) TYPE FOR
!              TITLE, LABEL, AND LEGEND SCRIPT
!              ON A PLOT.
!              THE CASE (UPPER OR LOWER) FOR THE SCRIPT WILL BE PLACED
!              IN THE CHARACTER VARIABLE ITEXCA.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDEFCA
!                     --IBUGD2
!     OUTPUT ARGUMENTS--ITEXCA
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   1993.  ACCEPT "ASIS" AS ARGUMENT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCA
      CHARACTER*4 ITEXCA
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
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'CASE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCASE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICOM,IDEFCA,IBUGD2,ISUBRO,NUMARG
   53   FORMAT('ICOM,IDEFCA,IBUGD2,ISUBRO,NUMARG = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO I=1,NUMARG
           WRITE(ICOUT,56)I,IHARG(I)
   56      FORMAT('I,IHARG(I) = ',I8,2X,A4)
           CALL DPWRST('XXX','BUG ')
        ENDDO
      ENDIF
!
!               ************************************************
!               **  TREAT THE CASE (UPPER VERSUS LOWER) CASE  **
!               ************************************************
!
      IF(ICOM.EQ.'CASE')THEN
        IF(NUMARG.LE.0 .OR. IHARG(NUMARG).EQ.'ON' .OR.      &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'UPPE')GO TO 1161
        IF(IHARG(NUMARG).EQ.'OFF' .OR. IHARG(NUMARG).EQ.'LOWE')GO TO 1162
        IF(IHARG(NUMARG).EQ.'ASIS')GO TO 1163
        IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1165
        IF(IHARG(NUMARG).EQ.'?')THEN
!
!               ********************************************
!               **  STEP 81--                             **
!               **  TREAT THE    ?    CASE--              **
!               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
!               ********************************************
!
          IFOUND='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8111)ITEXCA
 8111     FORMAT('THE CURRENT CASE IS ',A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8112)IDEFCA
 8112     FORMAT('THE DEFAULT CASE IS ',A4)
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
        IERROR='YES'
        WRITE(ICOUT,1171)
 1171   FORMAT('***** ERROR IN CASE (DPCASE)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1172)
 1172   FORMAT('      ILLEGAL ENTRY FOR CASE COMMAND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1173)
 1173   FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1174)
 1174   FORMAT('      SUPPOSE THE THE ANALYST WISHES TO HAVE CASE ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1175)
 1175   FORMAT('      FOR ALL PLOT TITLES, LABELS, AND LEGENDS,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1176)
 1176   FORMAT('      THEN ALLOWABLE FORMS ARE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1177)
 1177   FORMAT('           CASE UPPER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1178)
 1178   FORMAT('           UPPER CASE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1179)
 1179   FORMAT('           CASE')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
!
      ELSEIF(ICOM.EQ.'UPPE')THEN
        IF(NUMARG.LE.0 .OR. IHARG(1).NE.'CASE')GO TO 9000
        IF(NUMARG.LE.1 .OR. IHARG(NUMARG).EQ.'ON' .OR.       &
           IHARG(NUMARG).EQ.'AUTO')GO TO 1161
        IF(IHARG(NUMARG).EQ.'OFF')GO TO 1162
        IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1165
        GO TO 9000
      ELSEIF(ICOM.EQ.'LOWE')THEN
        IF(NUMARG.LE.0 .OR. IHARG(1).NE.'CASE')GO TO 9000
        IF(NUMARG.LE.1 .OR. IHARG(NUMARG).EQ.'ON' .OR.        &
           IHARG(NUMARG).EQ.'AUTO')GO TO 1162
        IF(IHARG(NUMARG).EQ.'OFF')GO TO 1161
        IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1165
        GO TO 9000
      ELSEIF(ICOM.EQ.'ASIS')THEN
        IF(NUMARG.LE.0 .OR. IHARG(1).NE.'CASE')GO TO 9000
        IF(NUMARG.LE.1)GO TO 1163
        IF(IHARG(NUMARG).EQ.'ON' .OR. IHARG(NUMARG).EQ.'AUTO')GO TO 1162
        IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1165
        IF(IHARG(NUMARG).EQ.'OFF')GO TO 1161
        GO TO 9000
      ENDIF
      GO TO 9000
!
 1161 CONTINUE
      ITEXCA='UPPE'
      GO TO 1180
!
 1162 CONTINUE
      ITEXCA='LOWE'
      GO TO 1180
!
 1163 CONTINUE
      ITEXCA='ASIS'
      GO TO 1180
!
 1165 CONTINUE
      ITEXCA=IDEFCA
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
 1181   FORMAT('THE CASE (FOR PLOT SCRIPT AND TEXT) ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)ITEXCA
 1182   FORMAT('HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'CASE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCASE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ITEXCA,IDEFCA
 9012   FORMAT('IFOUND,IERROR,ITEXCA,IDEFCA = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCASE
      SUBROUTINE DPCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE ONE OF THE FOLLOWING 12 CONTROL CHARTS--
!              1) MEAN
!              2) RANGE
!              3) STANDARD DEVIATION
!              4) CUSUM
!              5) P
!              6) PN
!              7) C
!              8) U
!              9) EWMA (EXPONENTIALLY WEIGHTED MOVING AVERAGE)
!             10) MOVING AVERAGE
!             11) MOVING RANGE
!             12) MOVING STANDARD DEVIATION
!             13) ISO 13528
!             14) ISO 13528 CUSUM
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JUNE      1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1988. (P, PN, C, AND U CHARTS)
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --JULY      1990. ADD    R CHART    CHECK
!     UPDATED         --JULY      1990. FIX P, NP, C, & U CHARTS
!     UPDATED         --SEPTEMBER 1990. LSL, USL, TARGET
!     UPDATED         --AUGUST    1991. TURN OFF MESS.--LSL/USL/TARGET
!     UPDATED         --MARCH     1997. EWMA, ACTIVATE CUSUM
!     UPDATED         --MARCH     1997. MOVING AVERAGE
!     UPDATED         --MARCH     1997. MOVING RANGE
!     UPDATED         --MARCH     1997. MOVING STANDARD DEVIATION
!     UPDATED         --SEPTEMBER 1998. ACTIVATED CUSUM MEAN CHART
!     UPDATED         --AUGUST    2010. USE DPPARS
!     UPDATED         --JANUARY   2012. SUPPORT HIGHLIGHTED OPTION
!     UPDATED         --JANUARY   2012. "MAXSET" OPTION
!     UPDATED         --FEBRUARY  2012. ISO 13528
!     UPDATED         --FEBRUARY  2012. ISO 13528 CUSUM
!     UPDATED         --FEBRUARY  2018. CONFLICT WITH
!                                       "MEAN CHARACTER PLOT"
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP2
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IERRO2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 CARG0
      CHARACTER*4 CARG1
      CHARACTER*4 CARG2
      CHARACTER*4 CARG3
      CHARACTER*4 CARG4
      CHARACTER*4 CARG11
!
      CHARACTER*4 IHIGH
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
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
      DIMENSION Y2(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION YPREV(MAXOBV)
!
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y1(1))
      EQUIVALENCE (GARBAG(IGARB3),Y2(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),XHIGH(1))
      EQUIVALENCE (GARBAG(IGARB8),YPREV(1))
!
!-----COMMON----------------------------------------------------------
!
!CCCC ADD FOLLOWING LINE APRIL 1997
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
!
      ISUBN1='DPCC'
      ISUBN2='    '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPCC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
!               *************************************************
!               **  TREAT THE CONTROL CHART CASE:              **
!               **     1) MEAN             CONTROL CHART       **
!               **     2) SD               CONTROL CHART       **
!               **     3) RANGE            CONTROL CHART       **
!               **     4) CUSUM            CONTROL CHART       **
!               **     5) P                CONTROL CHART       **
!               **     6) PN               CONTROL CHART       **
!               **     7) C                CONTROL CHART       **
!               **     8) U                CONTROL CHART       **
!               **     9) EWMA             CONTROL CHART       **
!               **    10) MOVING AVERAGE   CONTROL CHART       **
!               **    11) MOVING RANGE     CONTROL CHART       **
!               **    12) MOVING SD        CONTROL CHART       **
!               **    13) ISO 13528        CONTROL CHART       **
!               **    14) ISO 13528 CUSUM  CONTROL CHART       **
!               *************************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     CHECK FOR NAME CONFLICTS
!
      IF(ICOM.EQ.'FLUC')GO TO 9000
      IF(ICOM.EQ.'TABU')GO TO 9000
      IF(ICOM.EQ.'JACK')GO TO 9000
      IF(ICOM.EQ.'BOOT')GO TO 9000
      IF(ICOM.EQ.'DEX ')GO TO 9000
      IF(ICOM.EQ.'DEXP')GO TO 9000
      IF(ICOM.EQ.'DOE ')GO TO 9000
      IF(ICOM.EQ.'DOX ')GO TO 9000
      IF(ICOM.EQ.'CROS' .AND. IHARG(1).EQ.'TABU')GO TO 9000
!
      IHIGH='OFF'
      IFOUN1='OFF'
      IFOUN2='OFF'
      IF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')IHIGH='ON'
      ILASTC=-9999
!
      ISTOP=NUMARG-1
      DO 90 I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT' .OR. IHARG(I).EQ.'CHAR')THEN
          IF(IHARG2(I).EQ.'ACTE')GO TO 9000
          ISTOP=I
          GO TO 99
        ENDIF
   90 CONTINUE
   99 CONTINUE
!
      ICASP2='NONE'
      DO 100 I=0,ISTOP
!
        IF(I.EQ.0)THEN
          CARG0='    '
          CARG1=ICOM
          CARG2=IHARG(I+1)
          CARG3=IHARG(I+2)
          CARG4=IHARG(I+3)
        ELSE
          IF(I.EQ.1)THEN
            CARG0=ICOM
          ELSE
            CARG0=IHARG(I-1)
          ENDIF
          CARG1=IHARG(I)
          CARG11=IHARG2(I)
          CARG2=IHARG(I+1)
          CARG3=IHARG(I+2)
          CARG4=IHARG(I+3)
        ENDIF
!
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF((CARG1.EQ.'X   ' .OR. CARG1.EQ.'XBAR' .OR.   &
                CARG1.EQ.'MEAN' .OR. CARG1.EQ.'AVER') .AND.   &
                CARG2.NE.'CUSU' .AND. CARG2.NE.'CUMU' .AND.   &
                CARG0.NE.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='MECC'
        ELSEIF((CARG1.EQ.'SD  ' .OR. CARG1.EQ.'S   ') .AND.   &
                CARG2.NE.'CUSU' .AND. CARG2.NE.'CUMU' .AND.   &
                CARG0.NE.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='SDCC'
        ELSEIF(CARG1.EQ.'STAN' .AND. CARG2.EQ.'DEVI' .AND.   &
               CARG3.NE.'CUSU' .AND. CARG3.NE.'CUMU' .AND.   &
               CARG0.NE.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='SDCC'
        ELSEIF((CARG1.EQ.'RANG' .OR. CARG1.EQ.'R   ') .AND.   &
                CARG2.NE.'CUSU' .AND. CARG2.NE.'CUMU' .AND.   &
                CARG0.NE.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='RACC'
        ELSEIF((CARG1.EQ.'MEAN' .OR. CARG1.EQ.'AVER' .OR.   &
                CARG1.EQ.'X   ') .AND.   &
               (CARG2.EQ.'CUSU' .OR.   &
               (CARG2.EQ.'CUMU' .AND. CARG3.EQ.'SUM ')))THEN
          IFOUN1='YES'
          ICASPL='CUCC'
          ICASP2='MEAN'
        ELSEIF((CARG1.EQ.'SD  ' .OR. CARG1.EQ.'S   ') .AND.   &
               (CARG2.EQ.'CUSU' .OR.   &
               (CARG2.EQ.'CUMU' .AND. CARG3.EQ.'SUM ')))THEN
          IFOUN1='YES'
          ICASPL='CUCC'
          ICASP2='SD  '
        ELSEIF(CARG1.EQ.'STAN' .AND. CARG2.EQ.'DEVI' .AND.   &
               (CARG3.EQ.'CUSU' .OR.   &
               (CARG3.EQ.'CUMU' .AND. CARG4.EQ.'SUM ')))THEN
          IFOUN1='YES'
          ICASPL='CUCC'
          ICASP2='SD  '
        ELSEIF((CARG1.EQ.'RANG' .OR. CARG1.EQ.'R   ') .AND.   &
               (CARG2.EQ.'CUSU' .OR.   &
               (CARG2.EQ.'CUMU' .AND. CARG3.EQ.'SUM ')))THEN
          IFOUN1='YES'
          ICASPL='CUCC'
          ICASP2='RANG'
        ELSEIF(CARG1.EQ.'CUSU' .AND. ICASPL.NE.'1CUS')THEN
          IFOUN1='YES'
          ICASPL='CUCC'
        ELSEIF(CARG1.EQ.'CUMU' .AND. CARG2.EQ.'SUM ')THEN
          IFOUN1='YES'
          ICASPL='CUCC'
        ELSEIF(CARG1.EQ.'P   ')THEN
          IFOUN1='YES'
          ICASPL='PCC'
        ELSEIF(CARG1.EQ.'PN  ' .OR. CARG1.EQ.'NP  ')THEN
          IFOUN1='YES'
          ICASPL='PNCC'
        ELSEIF(CARG1.EQ.'C   ')THEN
          IFOUN1='YES'
          ICASPL='CCC'
        ELSEIF(CARG1.EQ.'U   ')THEN
          IFOUN1='YES'
          ICASPL='UCC'
        ELSEIF(CARG1.EQ.'EXPO' .AND. CARG2.EQ.'WEIG' .AND.   &
               CARG3.EQ.'MOVI' .AND. CARG4.EQ.'AVER')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'EWMA')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'EXPO' .AND. CARG2.EQ.'MOVI' .AND.   &
               CARG3.EQ.'AVER')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'EXPO' .AND. CARG2.EQ.'WEIG' .AND.   &
               CARG3.EQ.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'EXPO' .AND. CARG2.EQ.'WEIG')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'MOVI' .AND.   &
              (CARG2.EQ.'AVER' .OR. CARG2.EQ.'MEAN') .AND.   &
               CARG0.NE.'EXPO' .AND. CARG0.NE.'WEIG')THEN
          IFOUN1='YES'
          ICASPL='MACC'
        ELSEIF(CARG1.EQ.'MOVI' .AND. CARG2.EQ.'RANG')THEN
          IFOUN1='YES'
          ICASPL='MRCC'
        ELSEIF(CARG1.EQ.'MOVI' .AND.   &
              (CARG2.EQ.'SD  ' .OR. CARG2.EQ.'MSD' .OR.   &
               CARG2.EQ.'S   '))THEN
          IFOUN1='YES'
          ICASPL='MSCC'
        ELSEIF(CARG1.EQ.'MOVI' .AND. CARG2.EQ.'STAN' .AND.   &
               CARG3.EQ.'DEVI')THEN
          IFOUN1='YES'
          ICASPL='MSCC'
        ELSEIF(CARG1.EQ.'ISO ' .AND. CARG2.EQ.'1352')THEN
          IF(CARG3.EQ.'CUSU')THEN
            IFOUN1='YES'
            ICASPL='1CUS'
          ELSE
            IFOUN1='YES'
            ICASPL='1352'
          ENDIF
        ELSEIF(CARG1.EQ.'CONT' .AND. CARG2.EQ.'CHAR')THEN
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+1)
        ELSEIF(CARG1.EQ.'CONT' .AND. CARG2.EQ.'PLOT')THEN
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+1)
        ELSEIF(CARG1.EQ.'CHAR' .AND. CARG0.NE.'CONT')THEN
          IF(CARG11.EQ.'ACTE')GO TO 9000
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
        ENDIF
!
  100 CONTINUE
!
      IF(IFOUN1.EQ.'NO' .AND. IFOUN2.EQ.'YES')THEN
        ICASPL='MECC'
        IFOUN1='YES'
      ENDIF
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')IFOUND='YES'
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
!
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='CONTROL CHART'
      IF(ICASPL.EQ.'MECC')INAME='MEAN CONTROL CHART'
      IF(ICASPL.EQ.'SDCC')INAME='SD CONTROL CHART'
      IF(ICASPL.EQ.'RACC')INAME='RANGE CONTROL CHART'
      IF(ICASPL.EQ.'CUCC')INAME='CUSUM CONTROL CHART'
      IF(ICASPL.EQ.'PCC')INAME='P CONTROL CHART'
      IF(ICASPL.EQ.'PNCC')INAME='NP CONTROL CHART'
      IF(ICASPL.EQ.'CCC')INAME='C CONTROL CHART'
      IF(ICASPL.EQ.'UCC')INAME='U CONTROL CHART'
      IF(ICASPL.EQ.'EWCC')INAME='EWMA CONTROL CHART'
      IF(ICASPL.EQ.'MACC')INAME='MOVING AVERAGE CONTROL CHART'
      IF(ICASPL.EQ.'MRCC')INAME='MOVING RANGE CONTROL CHART'
      IF(ICASPL.EQ.'MSCC')INAME='MOVING SD CONTROL CHART'
      IF(ICASPL.EQ.'1352')INAME='ISO 13528 CONTROL CHART'
      IF(ICASPL.EQ.'1CUS')INAME='ISO 13528 CUSUM CONTROL CHART'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IF(ICASPL.EQ.'MACC')IFLAGM=1
      IF(ICASPL.EQ.'MRCC')IFLAGM=1
      IF(ICASPL.EQ.'MSCC')IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=3
      IF(IHIGH.EQ.'ON')MAXNVA=MAXNVA+1
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR,IHIGH,ICASPL
  282   FORMAT('NQ,NUMVAR,IHIGH,ICASPL = ',2I8,2(2X,A4))
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
      ICOL=1
      IF(IHIGH.EQ.'OFF')THEN
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,Y2,X1,TEMP,TEMP2,TEMP2,TEMP2,NLOCAL,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(NUMVAR.EQ.2)THEN
          DO 292 II=1,NLOCAL
            X1(II)=Y2(II)
  292     CONTINUE
        ENDIF
      ELSE
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,Y2,X1,XHIGH,TEMP2,TEMP2,TEMP2,NLOCAL,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(NUMVAR.EQ.3)THEN
          DO 294 II=1,NLOCAL
            XHIGH(II)=X1(II)
            X1(II)=Y2(II)
  294     CONTINUE
        ELSEIF(NUMVAR.EQ.2)THEN
          DO 296 II=1,NLOCAL
            XHIGH(II)=Y2(II)
  296     CONTINUE
        ENDIF
      ENDIF
!
!               *******************************************************
!               **  STEP 7--                                         **
!               **  FOR THE 1-VARIABLE CASE ONLY,                    **
!               **  DETERMINE IF THE ANALYST                         **
!               **  HAS SPECIFIED    THE GROUP SIZE,                 **
!               **  FOR THE CONTROL CHART ANALYSIS.                  **
!               **  THE GROUP SIZE SETTING IS DEFINED BY SEARCHING   **
!               **  THE INTERNAL TABLE FOR THE PARAMETER NAME  NI ;  **
!               **  IF FOUND, USE THE SPECIFIED VALUE.               **
!               **  IF NOT FOUND, GENERATE AN ERROR MESSAGE.         **
!               *******************************************************
!
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISIZE=1
      IF((IHIGH.EQ.'OFF'.AND.NUMVAR.LE.1) .OR.   &
         (IHIGH.EQ.'ON'.AND.NUMVAR.LE.2))THEN
        IH='NI  '
        IH2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'YES')THEN
          ISIZE=1
        ELSE
          ISIZE=INT(VALUE(ILOCP)+0.5)
        ENDIF
      ENDIF
!
!CCCC THE FOLLOWING SECTION WAS ADDED SEPTEBMER 1990
!               ********************************************************
!               **  STEP 8--                                          **
!               **  DETERMINE IF THE ANALYST                          **
!               **  HAS SPECIFIED                                     **
!               **      LSL (LOWER SPEC LIMIT)                        **
!               **      USL (UPPER SPEC LIMIT)                        **
!               **      USLCOST (UPPER SPEC LIMIT COST)               **
!               **      TARGET                                        **
!               **      P (FOR EWMA CHARTS)                           **
!               **      K (FOR UNGROUPED DATA, FILTER WIDTH)          **
!               **      WIDTH AS ALTERNATIVE TO K                     **
!               **      WEIGHT AS ALTERNATIVE TO P                    **
!               **  FOR THE CONTROL CHART ANALYSIS.                   **
!               ********************************************************
!
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CCLSL=CPUMIN
      IH='LSL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCLSL=VALUE(ILOCP)
!
      CCUSL=CPUMIN
      IH='USL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCUSL=VALUE(ILOCP)
!
      CCTARG=CPUMIN
      IH='TARG'
      IH2='ET  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCTARG=VALUE(ILOCP)
!
      P=CPUMIN
      IH='P   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')THEN
        P=VALUE(ILOCP)
      ELSE
        IH='WEIG'
        IH2='HT  '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
        IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
        ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'NO')P=VALUE(ILOCP)
      ENDIF
!
      KWIDTH=3
      IH='K   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')THEN
        KWIDTH=INT(VALUE(ILOCP)+0.5)
      ELSE
        IH='WIDT'
        IH2='H   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
        IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
        ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'NO')KWIDTH=INT(VALUE(ILOCP)+0.5)
      ENDIF
!
      USRSIG=CPUMIN
      IH='SIGM'
      IH2='AE  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')USRSIG=VALUE(ILOCP)
!
      AK=0.5
      IH='K   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')AK=VALUE(ILOCP)
!
      H=5.0
      IH='H   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')H=VALUE(ILOCP)
!
      H=5.0
      IH='H   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')H=VALUE(ILOCP)
!
      SHI=CPUMIN
      IH='SHI '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')SHI=VALUE(ILOCP)
!
      SLI=CPUMIN
      IH='SLI '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')SLI=VALUE(ILOCP)
!
      MAXSET=-99
      IH='MAXS'
      IH2='ET  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')MAXSET=INT(VALUE(ILOCP)+0.5)
!
!               *******************************************************
!               **  STEP 9--                                         **
!               **  COMPUTE THE APPROPRIATE CONTROL CHART STATISTIC--**
!               **  MEAN, STANDARD DEVIATION, RANGE, CUSUM,          **
!               **  P, NP, C, U.                                     **
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
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPCC2(Y1,Y2,X1,XHIGH,NLOCAL,NUMVAR,ICASPL,IHIGH,ISIZE,ICONT,   &
                 XIDTEM,TEMP,TEMP2,YPREV,   &
                 CCLSL,CCUSL,CCTARG,P,KWIDTH,   &
                 ICCHPR,ICCHWT,ICONWC,USRSIG,   &
                 AK,H,SHI,SLI,MAXSET,   &
                 Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPCC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ISIZE
 9012   FORMAT('IFOUND,IERROR,ISIZE = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        IF(IFOUND.EQ.'YES')THEN
          WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013     FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
          CALL DPWRST('XXX','BUG ')
          IF(NPLOTP.GE.1)THEN
            DO 9015 I=1,NPLOTP
              WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016         FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
 9015       CONTINUE
          ENDIF
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPCC
      SUBROUTINE DPCC2(Y,YN,X,XHIGH,N,NUMV2,ICASPL,IHIGH,ISIZE,ICONT,   &
                       XIDTEM,TEMP,TEMP2,YPREV,   &
                       CCLSL,CCUSL,CCTARG,P,KWIDTH,   &
                       ICCHPR,ICCHWT,ICONWC,USRSIG,   &
                       AK,H,SHI,SLI,MAXSET,   &
                       Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A CONTROL CHART
!              OF THE FOLLOWING TYPES--
!                 1) MEAN CONTROL CHART    Y X
!                 2) STANDARD DEVIATION CONTROL CHART    Y X
!                 3) RANGE CONTROL CHART    Y X
!                 4) CUSUM CONTROL CHART    Y X
!                 5) P CONTROL CHART    NUMDEF NUMTOT X
!                 6) PN CONTROL CHART    NUMDEF NUMTOT X
!                 7) U CONTROL CHART    NUMDEF SIZE X
!                 8) P CONTROL CHART    NUMDEF SIZE X
!                 9) EWMA CONTROL CHART Y X
!                10) MOVING AVERAGE CONTROL CHART Y X
!                11) MOVING RANGE CONTROL CHART Y X
!                12) MOVING STANDARD DEVIATION CONTROL CHART Y X
!                13) ISO 13528 CONTROL CHART Y X
!                14) ISO 13528 CUSUM CONTROL CHART Y X
!     NOTE--USE P AND PN CHARTS IF KNOW HOW MANY ITEMS HAVE DEFECTS
!         --USE U AND C CHARTS IF KNOW HOW MANY DEFECTS
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     REFERENCE--ISHIKAWA, GUIDE TO QUALITY CONTROL
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JUNE      1978.
!     UPDATED         --OCTOBER   1978.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --APRIL     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY  1988. P, PN, U, AND C CHARTS
!     UPDATED         --JULY     1990. FIX P, PN, U, & C CHARTS
!     UPDATED         --SEPTEMBER 1990. LSL, USL, TARGET
!     UPDATED         --MARCH     1997. EWMA CHART, ACTIVATE CUSUM
!     UPDATED         --MARCH     1997. MOVING AVERAGE CHART
!     UPDATED         --MARCH     1997. MOVING RANGE CHART
!     UPDATED         --MARCH     1997. MOVING STANDARD DEVIATION CHART
!     UPDATED         --JANUARY   2012. SUPPORT FOR HIGHLIGHTING OPTION
!     UPDATED         --JANUARY   2012. SUPPORT FOR WECO AND ISO 13528
!                                       CONTROL LIMITS
!     UPDATED         --JANUARY   2012. SUPPORT FOR "MAXSET" OPTION
!     UPDATED         --FEBRUARY  2012. ISO 13528 CONTROL CHART
!     UPDATED         --FEBRUARY  2012. ISO 13528 CUSUM CONTROL CHART
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IHIGH
      CHARACTER*4 ICONT
      CHARACTER*4 ICCHPR
      CHARACTER*4 ICCHWT
      CHARACTER*4 ICONWC
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION YN(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION XHIGH(*)
      DIMENSION YPREV(*)
!
      DIMENSION XIDTEM(*)
      DIMENSION TEMP(*)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      DIMENSION TEMP2(*)
!
      DIMENSION A3(30)
      DIMENSION C4(30)
      DIMENSION B3(30)
      DIMENSION B4(30)
      DIMENSION E2(30)
      DIMENSION D22(30)
      DIMENSION D3(30)
      DIMENSION D4(30)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!CCCC DATA(A(I),I=    1,   25)
!CCCC1/9.999,2.121,1.732,1.500,1.342,1.225,1.134,1.061,1.000,0.945,
!CCCC1 0.905,0.866,0.832,0.802,0.775,0.750,0.723,0.707,0.688,0.671,
!CCCC1 0.655,0.640,0.626,0.612,0.600/
!CCCC DATA(A0(I),I=    1,   25)
!CCCC1/9.999,3.760,3.070,2.914,2.884,2.899,2.935,2.980,3.030,3.085,
!CCCC1 3.136,3.189,3.242,3.295,3.347,3.398,3.448,3.497,3.545,3.592,
!CCCC1 3.639,3.684,3.729,3.773,3.816/
!CCCC DATA(A1(I),I=    1,   25)
!CCCC1/9.999,3.760,2.394,1.880,1.596,1.410,1.277,1.175,1.094,1.028,
!CCCC1 0.973,0.925,0.884,0.848,0.816,0.788,0.762,0.738,0.717,0.697,
!CCCC1 0.679,0.662,0.647,0.632,0.619/
!CCCC DATA(A2(I),I=    1,   25)
!CCCC1/9.999,1.880,1.023,0.729,0.577,0.483,0.419,0.373,0.337,0.308,
!CCCC1 0.285,0.266,0.249,0.235,0.223,0.212,0.203,0.194,0.187,0.180,
!CCCC1 0.173,0.167,0.162,0.157,0.153/
!CCCC DATA(C2(I),I=    1,   25)
!CCCC1/9.9999,0.5642,0.7236,0.7979,0.8407,
!CCCC1 0.8686,0.8882,0.9027,0.9139,0.9227,
!CCCC1 0.9300,0.9359,0.9410,0.9453,0.9490,
!CCCC1 0.9523,0.9551,0.9576,0.9599,0.9619,
!CCCC1 0.9638,0.9655,0.9670,0.9684,0.9696/
!CCCC DATA(B1(I),I=    1,   25)
!CCCC1/0.000,0.000,0.000,0.000,0.000,0.026,0.105,0.167,0.219,0.262,
!CCCC1 0.299,0.331,0.359,0.384,0.406,0.427,0.445,0.461,0.477,0.491,
!CCCC1 0.504,0.516,0.527,0.538,0.548/
!CCCC DATA(B2(I),I=    1,   25)
!CCCC1/9.999,1.843,1.858,1.808,1.756,1.711,1.672,1.638,1.609,1.584,
!CCCC1 1.561,1.541,1.523,1.507,1.492,1.478,1.465,1.454,1.443,1.433,
!CCCC1 1.424,1.415,1.407,1.399,1.392/
!CCCC DATA(D1(I),I=    1,   25)
!CCCC1/0.000,0.000,0.000,0.000,0.000,0.000,0.205,0.387,0.546,0.687,
!CCCC1 0.812,0.924,1.026,1.121,1.207,1.285,1.359,1.426,1.490,1.548,
!CCCC1 1.606,1.659,1.710,1.759,1.804/
!
      DATA(A3(I),I=    1,   25)   &
      /9.999,2.659,1.954,1.628,1.427,   &
       1.287,1.182,1.099,1.032,0.975,   &
       0.927,0.886,0.850,0.817,0.789,   &
       0.763,0.739,0.718,0.698,0.680,   &
       0.663,0.647,0.633,0.619,0.606/
      DATA(C4(I),I=    1,   25)   &
      /9.9999,0.7979,0.8862,0.9213,0.9400,   &
       0.9515,0.9594,0.9650,0.9693,0.9727,   &
       0.9754,0.9776,0.9794,0.9810,0.9823,   &
       0.9835,0.9845,0.9854,0.9862,0.9869,   &
       0.9876,0.9882,0.9887,0.9892,0.9896/
      DATA(B3(I),I=    1,   25)   &
      /0.000,0.000,0.000,0.000,0.000,0.030,0.118,0.185,0.239,0.284,   &
       0.321,0.354,0.382,0.406,0.428,0.448,0.466,0.482,0.497,0.510,   &
       0.523,0.534,0.545,0.555,0.565/
      DATA(B4(I),I=    1,   25)   &
      /9.999,3.267,2.568,2.266,2.089,1.970,1.882,1.815,1.761,1.716,   &
       1.679,1.646,1.618,1.594,1.572,1.552,1.534,1.518,1.503,1.490,   &
       1.477,1.466,1.455,1.445,1.435/
      DATA(E2(I),I=    1,   25)   &
      /9.999,1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078,   &
       3.173,3.258,3.336,3.407,3.472,3.532,3.588,3.640,3.689,3.735,   &
       3.778,3.819,3.858,3.895,3.931/
      DATA(D22(I),I=    1,   25)   &
      /9.999,3.686,4.358,4.698,4.918,5.078,5.203,5.307,5.394,5.469,   &
       5.534,5.592,5.646,5.693,5.737,5.779,5.817,5.854,5.888,5.922,   &
       5.950,5.979,6.006,6.031,6.058/
      DATA(D3(I),I=    1,   25)   &
      /0.000,0.000,0.000,0.000,0.000,0.000,0.076,0.136,0.184,0.223,   &
       0.256,0.284,0.308,0.329,0.348,0.364,0.379,0.392,0.404,0.414,   &
       0.425,0.434,0.443,0.452,0.459/
      DATA(D4(I),I=    1,   25)   &
      /9.999,3.267,2.575,2.282,2.115,2.004,1.924,1.864,1.816,1.777,   &
       1.744,1.716,1.692,1.671,1.652,1.636,1.621,1.608,1.596,1.586,   &
       1.575,1.566,1.557,1.548,1.541/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCC'
      ISUBN2='2   '
      IWRITE='OFF'
!
      XTMAX=0.0
      XTMIN=0.0
      D3FACT=0.0D0
      D4FACT=0.0D0
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPCC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)N,NUMV2,ISIZE,MAXSET,ICASPL,ICONT
   71   FORMAT('N,NUMV2,ISIZE,MAXSET,ICASPL,ICONT = ',4I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,75)ICCHPR,ICCHWT,ICONWC,USRSIG
   75   FORMAT('ICCHPR,ICCHWT,ICONWC,USRSIG = ',3(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 72 I=1,N
          WRITE(ICOUT,73)I,Y(I),YN(I),X(I),XHIGH(I)
   73     FORMAT('I,Y(I),YN(I),X(I),XHIGH(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
      ENDIF
!
      I2=0
      ISIZE2=0
!
      AN=0.0
      XBARG=0.0
      SDG=0.0
      RANGEG=0.0
      YUPPER=0.0
      YLOWER=0.0
!
      ANUMSE=0.0
      SDI=0.0
      SIGMAE=0.0
      RANGEE=0.0
      SADJ=0.0
      RADJ=0.0
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN CONTROL CHART--')
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
      HOLD=Y(1)
      DO 60 I=1,N
      IF(Y(I).NE.HOLD)GO TO 69
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)HOLD
   62 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS ARE IDENTICALLY ',   &
             'EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
   69 CONTINUE
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
!               **  FOR VARIABLE 2 (THE GROUP VARIABLE).              **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
!               **  WHICH IS AN ERROR CONDITION FOR A CONTROL CHART.  **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF((IHIGH.EQ.'OFF'.AND.NUMV2.EQ.1) .OR.   &
         (IHIGH.EQ.'ON'.AND.NUMV2.EQ.2))THEN
!
!       WHEN THERE IS NO GROUP-ID VARIABLE, CREATE ONE (BASED ON
!       ISIZE).
!
        NUMSET=0
        IF(ISIZE.EQ.1)THEN
          DO 120 I=1,N
            XIDTEM(I)=REAL(I)
            X(I)=XIDTEM(I)
  120     CONTINUE
        ELSE
          NUMSET=0
          ILOOP=N/ISIZE
          DO 145 I=1,ILOOP
            NUMSET=NUMSET+1
            XIDTEM(NUMSET)=REAL(NUMSET)
            ISTART=(I-1)*ISIZE+1
            ISTOP=I*ISIZE
            DO 147 J=ISTART,ISTOP
              X(J)=XIDTEM(NUMSET)
  147       CONTINUE
  145     CONTINUE
          ILEFT=MOD(N,ISIZE)
          IF(ILEFT.NE.0)THEN
            ISTART=ILOOP*ISIZE+1
            NUMSET=NUMSET+1
            XIDTEM(NUMSET)=REAL(NUMSET)
            DO 148 J=ISTART,N
              X(J)=XIDTEM(NUMSET)
  148       CONTINUE
          ENDIF
        ENDIF
      ENDIF
!
!     WHEN THERE IS A GROUP-ID VARIABLE, EXTRACT UNIQUE VALUES
!
      CALL DISTIN(X,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
!
      IF(NUMSET.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      THE NUMBER OF SETS IS EQUAL TO ZERO.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      CALL SORT(XIDTEM,NUMSET,XIDTEM)
!
      IF((ICASPL.EQ.'MECC' .OR. ICASPL.EQ.'SDCC' .OR.   &
         ICASPL.EQ.'RACC') .AND.NUMSET.EQ.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,196)
  196   FORMAT('      THE NUMBER OF SETS IS IDENTICAL TO THE NUMBER ',   &
               'OF OBSERVATIONS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,198)NUMSET
  198   FORMAT('      THEN  NUMBER OF SETS/OBSERVATIONS  = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      AN=N
      ANUMSE=NUMSET
!
!               *******************************************
!               **  STEP 3.0--                           **
!               **  DETERMINE STATISTICS FOR THE ENTIRE  **
!               **  DATA SET                             **
!               *******************************************
!
!     NOTE 2012/1: IN SOME CASES, WE MAY WANT TO BASE CONTROL
!                  LIMITS ON PORTION OF PLOT THAT IS KNOWN TO
!                  BE IN CONTROL (E.G., HISTORICAL DATA).  IF
!                  USER HAS SPECIFIED "MAXSET", ONLY USE SETS
!                  FROM 1 TO MAXSET IN COMPUTING THESE STATISTICS.
!
!                  FOR NOW, LIMIT THIS OPTION TO THE SHEWHART
!                  CHARTS (MEAN, SD, RANGE).
!
      ISTEPN='3.0'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUMXBG=0.0
      SUMSDG=0.0
      SUMRAG=0.0
      SUMSIE=0.0
      SUMRIE=0.0
!
      NUMTMP=NUMSET
      IF(MAXSET.GE.1 .AND. MAXSET.LT.NUMSET)THEN
        IF(ICASPL.EQ.'MECC' .OR. ICASPL.EQ.'RACC' .OR.   &
           ICASPL.EQ.'SDCC' .OR. ICASPL.EQ.'MACC' .OR.   &
           ICASPL.EQ.'MSCC' .OR. ICASPL.EQ.'MRCC')THEN
                                                                                                                                  
          NUMTMP=MAXSET
        ENDIF
      ENDIF
!
      J=0
      ANTMP=0.0
      DO 1010 ISET=1,NUMTMP
        J=J+1
!
        K=0
        DO 1020 I=1,N
          IF(X(I).EQ.XIDTEM(ISET))K=K+1
          IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 1020   CONTINUE
        NI=K
        ANI=NI
!
        SUM=0.0
!
        IF(NI.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1042)
 1042     FORMAT('NI FOR SOME CLASS = 0')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1043)ISET,XIDTEM(ISET),NI
 1043     FORMAT('ISET,XIDTEM(ISET),NI = ',I8,G15.7,I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        ANTMP=ANTMP+REAL(NI)
        CALL MEAN(TEMP,NI,IWRITE,XBARI,IBUGG3,IERROR)
        VARI=0.0
        IF(NI.GE.2)THEN
          CALL VAR(TEMP,NI,IWRITE,VARI,IBUGG3,IERROR)
        ENDIF
        SDI=0.0
        IF(VARI.GT.0.0)SDI=SQRT(VARI)
        XTMIN=TEMP(1)
        XTMAX=TEMP(1)
        DO 1034 I=1,NI
          IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
          IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 1034   CONTINUE
        RANGEI=XTMAX-XTMIN
        SUMXBG=SUMXBG+ANI*XBARI
        SUMSDG=SUMSDG+ANI*SDI
        SUMRAG=SUMRAG+ANI*RANGEI
!
        IF(NI.LE.25)THEN
          SUMSIE=SUMSIE+SDI/C4(NI)
          SUMRIE=SUMRIE+RANGEI/D22(NI)
          AJUNK1=C4(NI)
          AJUNK2=D22(NI)
        ELSE
          C4LARG=1.0
          D22LAR=2.0*SQRT(2.0*LOG(2.0*ANI))
          SUMSIE=SUMSIE+SDI/C4LARG
          SUMRIE=SUMRIE+RANGEI/D22LAR
          AJUNK1=C4LARG
          AJUNK2=D22LAR
        ENDIF
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
          WRITE(ICOUT,1061)ISET,NI,ANI,XBARI
 1061     FORMAT('ISET,NI,ANI,XBARI = ',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1063)SDI,AJUNK1,SUMSIE
 1063     FORMAT('SDI,C4,SUMSIE = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1064)RANGEI,AJUNK2,SUMRIE
 1064     FORMAT('RANGEI,D22,SUMRIE = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 1010 CONTINUE
!
      XBARG=SUMXBG/ANTMP
      SDG=SUMSDG/ANTMP
      RANGEG=SUMRAG/ANTMP
!CCCC SIGMAE=SUMSIE/REAL(MAXSET)
!CCCC RANGEE=SUMRIE/REAL(MAXSET)
      SIGMAE=SUMSIE/REAL(NUMTMP)
      RANGEE=SUMRIE/REAL(NUMTMP)
!
!     FOR UNGROUPED DATA, USE THE MOVING RANGE OR THE MOVING STANDARD
!     DEVIATION TO COMPUTE AN ESTIMATE FOR SIGMAE.  MARCH 1997.
!
      RANGEM=0.0
      SDM=0.0
      IF(N.EQ.NUMSET .AND. ICASPL.NE.'1352' .AND. ICASPL.NE.'1CUS')THEN
        IF(KWIDTH.LT.2)KWIDTH=2
        IF(KWIDTH.GT.N-1)KWIDTH=N-1
        NBEF=KWIDTH/2
        NAFT=NBEF
        IF(MOD(KWIDTH,2).EQ.0)NAFT=NBEF-1
        IF(1+NBEF.GT.NUMSET-NAFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1071)
 1071     FORMAT('      THERE ARE NOT ENOUGH DATA POINTS TO FORM THE ',   &
                 'MOVING RANGE ESTIMATE')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1072)
 1072     FORMAT('      OF THE ERROR STANDARD DEVIATION FOR UNGROUPED ',   &
                 'DATA.  YOU PROBABLY')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1073)
 1073     FORMAT('      NEED TO SET A SMALLER VALUE FOR THE FILTER ',   &
                 'WIDTH.  FOR EXAMPLE,')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1074)
 1074     FORMAT('         LET K = 3')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1075)
 1075     FORMAT('      THE PARAMETER K DEFINES HOW MANY VALUES ARE ',   &
                 'USED TO COMUTE THE')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1076)
 1076     FORMAT('      MOVING RANGE (3 IS THE TYPICAL VALUE).  THE ',   &
                 'CURRENT VALUE')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1077)KWIDTH
 1077     FORMAT('      OF K IS ',I5,'.')
          CALL DPWRST('XXXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        SUM=0.0
        SUM2=0.0
        ICOUNT=0
!CCCC   DO1083I=1+NBEF,MAXSET-NAFT
        DO 1083 I=1+NBEF,NUMTMP-NAFT
          ICOUNT=ICOUNT+1
          SUM1=0.0
          XTMIN=Y(I-NBEF)
          XTMAX=Y(I+NAFT)
          DO 1086 II=I-NBEF,I+NAFT
            IF(Y(II).LT.XTMIN)XTMIN=Y(II)
            IF(Y(II).GT.XTMAX)XTMAX=Y(II)
            SUM1=SUM1+Y(II)
 1086     CONTINUE
          SUM=SUM+(XTMAX-XTMIN)
          XMEAN=SUM1/REAL(KWIDTH)
          SUM1=0.0
          DO 1087 II=I-NBEF,I+NAFT
            SUM1=SUM1+(Y(II)-XMEAN)**2
 1087     CONTINUE
          SUM2=SUM2+SQRT(SUM1/REAL(KWIDTH-1))
 1083   CONTINUE
        RANGEM=SUM/REAL(ICOUNT)
        SDM=SUM2/REAL(ICOUNT)
      ENDIF
!
!           *********************************************************
!           **  STEP 4--                                           **
!           **  IN ORDER TO DETERMINE THE PROPER PLOT COOORDINATES **
!           **  FOR THE DESIRED PLOT,                              **
!           **  BRANCH TO THE PROPER SUBCASE--                     **
!           **         1) MEAN CONTROL CHART                       **
!           **         2) STANDARD DEVIATION CONTROL CHART         **
!           **         3) RANGE CONTROL CHART                      **
!           **         4) CUSUM CONTROL CHART                      **
!           **         5) P CONTROL CHART                          **
!           **         6) PN CONTROL CHART                         **
!           **         7) C CONTROL CHART                          **
!           **         8) U CONTROL CHART                          **
!           **         9) EWMA CONTROL CHART                       **
!           **        10) MOVING AVERAGE  CONTROL CHART            **
!           **        11) MOVING RANGE    CONTROL CHART            **
!           **        12) MOVING SD       CONTROL CHART            **
!           **        13) ISO 13528       CONTROL CHART            **
!           **        14) ISO 13528 CUSUM CONTROL CHART            **
!           *********************************************************
!
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NPREV=0
      IF(ICASPL.EQ.'MECC')THEN
!
!       *****************************************
!       **  STEP 5.1--                         **
!       **  TREAT THE MEAN CONTROL CHART CASE  **
!       *****************************************
!
        ISTEPN='5.1'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        DO 1110 ISET=1,NUMSET
!
          XTAG=0.0
          K=0
          DO 1120 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              IF(XHIGH(I).GE.0.5)XTAG=1.0
            ENDIF
 1120     CONTINUE
          NI=K
          ANI=NI
!
          IF(NI.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1132)
 1132       FORMAT('FOR SOME CLASS NI= 0')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
 1133       FORMAT('ISET,XIDTEM(ISET),NI = ',I8,G15.7,I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          SUM=0.0
          DO 1140 I=1,NI
            SUM=SUM+TEMP(I)
 1140     CONTINUE
          XBARI=SUM/ANI
          YMID=XBARG
!
          IF(NI.GE.26)THEN
            C4LARG=1.0
            SADJ=C4LARG*SIGMAE
            A3LARG=3.0/SQRT(ANI)
            YUPPER=XBARG+A3LARG*SADJ
            YLOWER=XBARG-A3LARG*SADJ
            AJUNK1=C4LARG
            AJUNK2=A3LARG
          ELSE
            SADJ=C4(NI)*SIGMAE
            YUPPER=XBARG+A3(NI)*SADJ
            YLOWER=XBARG-A3(NI)*SADJ
            AJUNK1=C4(NI)
            AJUNK2=A3(NI)
          ENDIF
!
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
            WRITE(ICOUT,1161)ISET,NI,ANI,XBARI,XBARG
 1161       FORMAT('ISET,NI,ANI,XBARI,XBARG = ',2I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1163)SDI,AJUNK1,SIGMAE,SADJ
 1163       FORMAT('SDI,AJUNK1,SIGMAE,SADJ = ',4G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1165)YMID,AJUNK2,YUPPER,YLOWER
 1165       FORMAT('YMID,A3,YUPPER,YLOWER = ',4G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          CALL DPCC3(ICASPL,J,XBARI,YMID,YLOWER,YUPPER,   &
                     Y2,X2,D2,XIDTEM(ISET),   &
                     YPREV,NPREV,IHIGH,XTAG,SIGMAE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 1110   CONTINUE
      ELSEIF(ICASPL.EQ.'SDCC')THEN
!
!       ********************************************************
!       **  STEP 5.2--                                        **
!       **  TREAT THE  STANDARD DEVIATION CONTROL CHART CASE  **
!       ********************************************************
!
        ISTEPN='5.2'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        DO 1210 ISET=1,NUMSET
!
          XTAG=0.0
          K=0
          DO 1220 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              IF(XHIGH(I).GE.0.5)XTAG=1.0
            ENDIF
 1220     CONTINUE
          NI=K
          ANI=NI
!
          IF(NI.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1132)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          SUM=0.0
          DO 1240 I=1,NI
            SUM=SUM+TEMP(I)
 1240     CONTINUE
          XBARI=SUM/ANI
!
          IF(NI.LE.1)GO TO 1210
!
          SUM=0.0
          DO 1250 I=1,NI
            SUM=SUM+(TEMP(I)-XBARI)**2
 1250     CONTINUE
          DENOM=ANI-1.0
          VARI=0.0
          IF(NI.GE.2)VARI=SUM/DENOM
          SDI=0.0
          IF(VARI.GT.0.0)SDI=SQRT(VARI)
!
          IF(NI.GE.26)THEN
            C4LARG=1.0
            SADJ=C4LARG*SIGMAE
            B4LARG=1.0+3.0/SQRT(2.0*(ANI-1.0))
            B3LARG=1.0-3.0/SQRT(2.0*(ANI-1.0))
            YUPPER=B4LARG*SADJ
            YLOWER=B3LARG*SADJ
            AJUNK1=C4LARG
            AJUNK2=B4LARG
            AJUNK3=B3LARG
          ELSE
            SADJ=C4(NI)*SIGMAE
            YUPPER=B4(NI)*SADJ
            YLOWER=B3(NI)*SADJ
            AJUNK1=C4(NI)
            AJUNK2=B4(NI)
            AJUNK3=B3(NI)
          ENDIF
!
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
            WRITE(ICOUT,1261)ISET,NI,ANI,XBARI
 1261       FORMAT('ISET,NI,ANI,XBARI = ',2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1263)SDI,AJUNK1,SIGMAE,SADJ,YMID
 1263       FORMAT('SDI,C4,SIGMAE,SADJ,YMID = ',5G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1265)YMID,AJUNK2,AJUNK3,YUPPER,YLOWER
 1265       FORMAT('YMID,B4,YUPPER,B3,YLOWER = ',4G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          CALL DPCC3(ICASPL,J,SDI,SADJ,YLOWER,YUPPER,   &
                     Y2,X2,D2,XIDTEM(ISET),   &
                     YPREV,NPREV,IHIGH,XTAG,SIGMAE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 1210   CONTINUE
      ELSEIF(ICASPL.EQ.'RACC')THEN
!
!       ******************************************
!       **  STEP 5.3--                          **
!       **  TREAT THE RANGE CONTROL CHART CASE  **
!       ******************************************
!
        ISTEPN='5.3'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        D4FACT=1.25
        D3FACT=1.0/1.25
!
        J=0
        DO 1310 ISET=1,NUMSET
!
          XTAG=0.0
          K=0
          DO 1320 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              IF(XHIGH(I).GE.0.5)XTAG=1.0
            ENDIF
 1320     CONTINUE
          NI=K
          ANI=NI
!
          IF(NI.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1132)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          IF(NI.LE.1)GO TO 1310
!
          XTMIN=TEMP(1)
          XTMAX=TEMP(1)
          DO 1340 I=1,NI
            IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
            IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 1340     CONTINUE
          RANGEI=XTMAX-XTMIN
!
          IF(NI.GE.26)THEN
            D22LAR=2.0*SQRT(2.0*LOG(2.0*ANI))
            RADJ=D22LAR*RANGEE
            D4LARG=1.0+3.0*D4FACT/SQRT(2.0*(ANI-1.0))
            D3LARG=1.0-3.0*D3FACT/SQRT(2.0*(ANI-1.0))
            YUPPER=D4LARG*RADJ
            YLOWER=D3LARG*RADJ
            AJUNK1=D22LAR
            AJUNK2=D4LARG
            AJUNK3=D3LARG
          ELSE
            RADJ=D22(NI)*RANGEE
            YUPPER=D4(NI)*RADJ
            YLOWER=D3(NI)*RADJ
            AJUNK1=D22(NI)
            AJUNK2=D4(NI)
            AJUNK3=D3(NI)
          ENDIF
          YMID=RADJ
!
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
            WRITE(ICOUT,1361)ISET,NI,ANI,RANGEI,YMID
 1361       FORMAT('ISET,NI,ANI,YMID = ',2I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1363)RANGEI,AJUNK1,RANGEE,SADJ,RADJ
 1363       FORMAT('RANGEI,D22,RANGEE,SADJ,RADJ = ',5G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1365)NI,ANI,AJUNK2,YUPPER,AJUNK3,YLOWER
 1365       FORMAT('NI,ANI,D4,YUPPER,D3,YLOWER = ',I8,5G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          CALL DPCC3(ICASPL,J,RANGEI,YMID,YLOWER,YUPPER,   &
                     Y2,X2,D2,XIDTEM(ISET),   &
                     YPREV,NPREV,IHIGH,XTAG,RANGEE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 1310   CONTINUE
      ELSEIF(ICASPL.EQ.'CUCC')THEN
!
!       ******************************************************
!       **  STEP 5.4--                                      **
!       **  DETERMINE PLOT COORDINATES                      **
!       **  FOR THE CUSUM CONTROL CHART PLOT SUBCASE.       **
!       ******************************************************
!
        ISTEPN='5.4'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
!
        SUMH=0.0
        SUML=0.0
        IF(SHI.NE.CPUMIN)SUMH=SHI
        IF(SLI.NE.CPUMIN)SUML=SLI
        ZHIGH=3.5
        IF(CCUSL.NE.CPUMIN)ZHIGH=CCUSL
!
        DO 1410 ISET=1,NUMSET
!
          K=0
          XTAG=0.0
          DO 1420 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))K=K+1
            IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
            IF(XHIGH(I).GE.0.5)XTAG=1.0
 1420     CONTINUE
          NI=K
          ANI=NI
!
          IF(NI.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1132)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          IF(NI.EQ.1)THEN
            ZI=(TEMP(1)-XBARG)/RANGEM
          ELSE
            SUM=0.0
            DO 1441 I=1,NI
              SUM=SUM+TEMP(I)
 1441       CONTINUE
            XBARI=SUM/ANI
            ZI=(XBARI-XBARG)/SIGMAE
          ENDIF
!
          SUMH=MAX(0.0,SUMH+(ZI-AK))
          SUML=MAX(0.0,SUML+(-ZI-AK))
!
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
            WRITE(ICOUT,1461)ISET,NI,ANI,XBARI
 1461       FORMAT('ISET,NI,ANI,XBARI = ',2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1463)ZI,SUMH,SUML
 1463       FORMAT('ZI,SUMH,SUML = ',3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          YUPPER=H
          YLOWER=-H
!
          J=J+1
          Y2(J)=SUMH
          X2(J)=XIDTEM(ISET)
          D2(J)=1.0
!
          J=J+1
          Y2(J)=-SUML
          X2(J)=XIDTEM(ISET)
          D2(J)=2.0
!
          J=J+1
          Y2(J)=0.0
          X2(J)=XIDTEM(ISET)
          D2(J)=3.0
!
          J=J+1
          Y2(J)=YUPPER
          X2(J)=XIDTEM(ISET)
          D2(J)=4.0
!
          J=J+1
          Y2(J)=YLOWER
          X2(J)=XIDTEM(ISET)
          D2(J)=5.0
!
          IF(ZI.LE.ZHIGH)GO TO 1472
          J=J+1
          Y2(J)=SUMH
          X2(J)=XIDTEM(ISET)
          D2(J)=6.0
          J=J+1
          Y2(J)=SUML
          X2(J)=XIDTEM(ISET)
          D2(J)=7.0
 1472     CONTINUE
!
 1410   CONTINUE
      ELSEIF(ICASPL.EQ.'PCC')THEN
!
!       ********************************************************
!       **  STEP 5.5--                                        **
!       **  TREAT THE  P CONTROL CHART CASE                   **
!       **  PROPORTION DEFECTIVE PER BATCH (SUBSAMPLE)        **
!       **  NUMBER DEFECTIVE PER BATCH / TOTAL NUMBER IN BATCH**
!       **  THE INPUT IS A DUAL SERIES--                      **
!       **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE  **
!       **     2) TOTAL NUMBER OF ITEMS IN THE SAMPLE         **
!       **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING BINOMIAL**
!       ********************************************************
!
        ISTEPN='5.5'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        SUM1=0.0
        SUM2=0.0
        DO 1510 ISET=1,NUMSET
          SUM1=SUM1+Y(ISET)
          SUM2=SUM2+YN(ISET)
 1510   CONTINUE
        CTOTAL=SUM1
        ANTOT=SUM2
        PBARG=CTOTAL/ANTOT
        PRBARG=100.0*PBARG
!
        J=0
        XTAG=0.0
        DO 1550 ISET=1,NUMSET
!
          CI=Y(ISET)
          ANI=YN(ISET)
          NI=INT(ANI+0.5)
          IF(NI.LE.0)GO TO 1550
!
          PI=CI/ANI
          PROPI=100.0*PI
          YMID=PRBARG
          VARPI=0.0
          IF(ANI.GT.0.0)VARPI=PBARG*(1.0-PBARG)/ANI
          SDPI=0.0
          IF(VARPI.GT.0.0)SDPI=SQRT(VARPI)
          SDPRI=100.0*SDPI
          YUPPER=YMID+3.0*SDPRI
          IF(YUPPER.GT.100.0)YUPPER=100.0
          YLOWER=YMID-3.0*SDPRI
          IF(YLOWER.LT.0.0)YLOWER=0.0
!
          CALL DPCC3(ICASPL,J,PROPI,YMID,YLOWER,YUPPER,   &
                     Y2,X2,D2,XIDTEM(ISET),   &
                     YPREV,NPREV,IHIGH,XTAG,SIGMAE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 1550   CONTINUE
!
      ELSEIF(ICASPL.EQ.'PNCC')THEN
!
!       *************************************************************
!       **  STEP 5.6--                                             **
!       **  TREAT THE PN CONTROL CHART CASE                        **
!       **  TOTAL NUMBER DEFECTIVE IN A BATCH (SUBSAMPLE)          **
!       **  SUM UP THE NUMBER OF DEFECTIVES PER BATCH (SUBSAMPLE)  **
!       **  THE NUMBER WILL BE  A NON-NEGATIVE INTEGER             **
!       **  THE INPUT IS A DUAL SERIES--                           **
!       **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE       **
!       **     2) TOTAL NUMBER OF ITEMS IN THE SAMPLE              **
!       **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING BINOMIAL     **
!       **  NOTE--THE PN CHART SHOULD BE USED ONLY WHEN            **
!       **        THE SUBSAMPLE SIZE IS CONSTANT.                  **
!       **        FOR VARYING SUBSAMPLE SIZE, USE THE P CHART      **
!       **        (ISHIKAWA, GUIDE TO QUALITY CONTROL, PAGE 77)    **
!       *************************************************************
!
        ISTEPN='5.6'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        XTAG=0.0
        SUM1=0.0
        SUM2=0.0
        ANUMSE=NUMSET
        DO 1610 ISET=1,NUMSET
          SUM1=SUM1+Y(ISET)
          SUM2=SUM2+YN(ISET)
 1610   CONTINUE
        CTOTAL=SUM1
        ANTOT=SUM2
        PBARG=CTOTAL/ANTOT
        ANBARG=ANTOT/ANUMSE
        CBARG=PBARG*ANBARG
!
        J=0
        DO 1650 ISET=1,NUMSET
!
          CI=Y(ISET)
          ANI=YN(ISET)
          NI=INT(ANI+0.5)
          IF(NI.LE.0)GO TO 1650
!
          PI=CI/ANI
          TAGI=XIDTEM(ISET)
          YMID=CBARG
          VARCI=0.0
          IF(ANBARG.GT.0.0)VARCI=ANBARG*PBARG*(1.0-PBARG)
          SDCI=0.0
          IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
          YUPPER=YMID+3.0*SDCI
          YLOWER=YMID-3.0*SDCI
          IF(YLOWER.LT.0.0)YLOWER=0.0
!
          CALL DPCC3(ICASPL,J,CI,YMID,YLOWER,YUPPER,   &
                     Y2,X2,D2,XIDTEM(ISET),   &
                     YPREV,NPREV,IHIGH,XTAG,SIGMAE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 1650   CONTINUE
      ELSEIF(ICASPL.EQ.'UCC')THEN
!
!       *********************************************************
!       **  STEP 5.7--                                         **
!       **  TREAT THE U CONTROL CHART CASE (POISSON)           **
!       **  DEFECTIVE PER UNIT                                 **
!       **  DEFECTIVE PER UNIT AREA                            **
!       **  NUMBER DEFECTIVE PER SUB-BATCH / LENGTH OR AREA    **
!       **  THE INPUT IS A DUAL SERIES--                       **
!       **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE   **
!       **     2) LENGTH OR AREA OF THE ITEM                   **
!       **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING POISSON  **
!       *********************************************************
!
        ISTEPN='5.7'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        XTAG=0.0
        SUM1=0.0
        SUM2=0.0
        DO 1710 ISET=1,NUMSET
          SUM1=SUM1+Y(ISET)
          SUM2=SUM2+YN(ISET)
 1710   CONTINUE
        CTOTAL=SUM1
        SIZTOT=SUM2
        CBARG=CTOTAL/SIZTOT
!
        J=0
        DO 1750 ISET=1,NUMSET
!
          CI=Y(ISET)
          SIZEI=YN(ISET)
          NSIZEI=INT(SIZEI+0.5)
          IF(NSIZEI.LE.0)GO TO 1750
          STAT=-1.0
          IF(SIZEI.NE.0.0)STAT=CI/SIZEI
          YMID=CBARG
          VARCI=0.0
          IF(ANI.GT.0.0)VARCI=CBARG/SIZEI
          SDCI=0.0
          IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
          YUPPER=YMID+3.0*SDCI
          YLOWER=YMID-3.0*SDCI
          IF(YLOWER.LT.0.0)YLOWER=0.0
!
          CALL DPCC3(ICASPL,J,STAT,YMID,YLOWER,YUPPER,   &
                     Y2,X2,D2,XIDTEM(ISET),   &
                     YPREV,NPREV,IHIGH,XTAG,SIGMAE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 1750   CONTINUE
      ELSEIF(ICASPL.EQ.'CCC')THEN
!
!       ********************************************************
!       **  STEP 5.8--                                        **
!       **  TREAT THE C CONTROL CHART CASE (POISSON)          **
!       **  TOTAL NUMBER DEFECTIVE IN A BATCH (SUBSAMPLE)     **
!       **  SUM OF DEFECTIVES IN A BATCH (SUBSAMPLE)          **
!       **  THE INPUT IS USUALLY A SERIES OF INTEGERS         **
!       **  THE VALUE WILL BE A NON-NEGATIVE INTEGER          **
!       **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING POISSON **
!       **  NOTE--THE C CHART SHOULD BE USED ONLY WHEN        **
!       **        THE SUBSAMPLE SIZE IS CONSTANT.             **
!       **        FOR VARYING SUBSAMPLE SIZE, USE THE U CHART **
!       **        (ISHIKAWA, GUIDE TO QUALITY CONTROL, PAGE 77)*
!       ********************************************************
!
        ISTEPN='5.8'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        XTAG=0.0
        SUM1=0.0
        SUM2=0.0
        ANUMSE=NUMSET
        DO 1810 ISET=1,NUMSET
          SUM1=SUM1+Y(ISET)
          IF(NUMV2.LE.2)SUM2=SUM2+1
          IF(NUMV2.GE.3)SUM2=SUM2+YN(ISET)
 1810   CONTINUE
        CTOTAL=SUM1
        CBARG=CTOTAL/ANUMSE
!
        J=0
        DO 1850 ISET=1,NUMSET
!
          CI=Y(ISET)
          SIZEI=YN(ISET)
          NSIZEI=INT(SIZEI+0.5)
          IF(NSIZEI.LE.0)GO TO 1850
          YMID=CBARG
          VARCI=0.0
          IF(ANI.GT.0.0)VARCI=CBARG
          SDCI=0.0
          IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
          YUPPER=YMID+3.0*SDCI
          YLOWER=YMID-3.0*SDCI
          IF(YLOWER.LT.0.0)YLOWER=0.0
!
          CALL DPCC3(ICASPL,J,CI,YMID,YLOWER,YUPPER,   &
                     Y2,X2,D2,XIDTEM(ISET),   &
                     YPREV,NPREV,IHIGH,XTAG,SIGMAE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 1850   CONTINUE
      ELSEIF(ICASPL.EQ.'EWCC')THEN
!
!       *****************************************
!       **  STEP 5.9--                         **
!       **  TREAT THE EXPONETIALLY WEIGHTED    **
!       **  CONTROL CHART CASE                 **
!       *****************************************
!
        ISTEPN='5.9'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(P.GE.1.0 .AND. P.LE.100.)P=P/100.
        IF(P.LE.0.0 .OR. P.GE.1.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1901)
 1901     FORMAT('      FOR THE EWMA CONTROL CHARTS, THE WEIGHTING',   &
                 ' PARAMETER P MUST BE SPECIFIED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1902)
 1902     FORMAT('     AND IN THE RANGE (0,1).  IT IS TYPICALLY ',   &
                 ' BETWEEN 0.1 AND 0.5 .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1903)
 1903     FORMAT('     FOR EXAMPLE: LET P = 0.2 ')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        J=0
        IF(CCTARG.NE.CPUMIN)THEN
          AK0=CCTARG
        ELSE
          AK0=XBARG
        ENDIF
        YMID=AK0
!
        DO 1910 ISET=1,NUMSET
!
          K=0
          XTAG=0.0
          DO 1920 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))K=K+1
            IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
            IF(XHIGH(I).GE.0.5)XTAG=1.0
 1920     CONTINUE
          NI=K
          ANI=NI
!
          IF(NI.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1132)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          SUM=0.0
          DO 1940 I=1,NI
            SUM=SUM+TEMP(I)
 1940     CONTINUE
          XBARI=SUM/ANI
!
          AK1=P*XBARI + (1.0-P)*AK0
          IF(N.NE.NUMSET)THEN
            SADJ=SIGMAE*3.0902*SQRT(P/(ANI*(2.0-P)))
          ELSE
            IF(KWIDTH.LE.25)THEN
              SADJ=(RANGEM/E2(KWIDTH))*3.0902*SQRT(P/(ANI*(2.0-P)))
            ELSE
              SADJ=(RANGEM/E2(25))*3.0902*SQRT(P/(ANI*(2.0-P)))
            ENDIF
          ENDIF
          YUPPER=XBARG+SADJ
          YLOWER=XBARG-SADJ
!
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
            WRITE(ICOUT,1961)ISET,NI,ANI,XBARI
 1961       FORMAT('ISET,NI,ANI,XBARI = ',2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1963)SDI,SIGMAE,SADJ,XBARG
 1963       FORMAT('SDI,SIGMAE,SADJ,XBARG = ',4G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1964)AK0,AK1,YLOWER,YUPPER
 1964       FORMAT('AK0,AK1,YLOWER,YUPPER = ',4G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          CALL DPCC3(ICASPL,J,AK1,XBARG,YLOWER,YUPPER,   &
                     Y2,X2,D2,XIDTEM(ISET),   &
                     YPREV,NPREV,IHIGH,XTAG,SIGMAE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
          AK0=AK1
!
 1910   CONTINUE
      ELSEIF(ICASPL.EQ.'MACC')THEN
!
!       *****************************************
!       **  STEP 5.10--                        **
!       **  TREAT THE MOVING AVERAGE           **
!       **  CONTROL CHART CASE                 **
!       *****************************************
!
        ISTEPN='5.10'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(KWIDTH.LT.2)KWIDTH=2
        IF(KWIDTH.GT.N-1)KWIDTH=N-1
        AK=REAL(KWIDTH)
        NBEF=KWIDTH/2
        NAFT=NBEF
        IF(MOD(KWIDTH,2).EQ.0)NAFT=NBEF-1
!
        J=0
        XTAG=0.0
!
!       2 CASES:
!         1) UNGROUPED DATA (N=NUMSET)
!         2) GROUPED DATA (N> NUMSET).  FOR GROUPED DATA, EACH GROUP
!            SHOULD HAVE AT LEAST 2 VALUES.
!
!       UNGROUPED CASE
!
        IF(N.EQ.NUMSET)THEN
          DO 2002 ISET=1,N
            TEMP2(ISET)=Y(ISET)
 2002     CONTINUE
        ELSE
!
!         GROUPED CASE
!
          DO 2010 ISET=1,NUMSET
!
            K=0
            DO 2020 I=1,N
              IF(X(I).EQ.XIDTEM(ISET))THEN
                K=K+1
                TEMP(K)=Y(I)
              ENDIF
 2020       CONTINUE
            NI=K
            ANI=NI
!
            IF(NI.LT.1)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,31)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2032)
 2032         FORMAT('FOR MOVING AVERAGE, FOR SOME CLASS NI < 1')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
!
            IF(NI.EQ.1)THEN
              TEMP2(ISET)=TEMP(1)
            ELSE
              SUM=0.0
              DO 2040 I=1,NI
                SUM=SUM+TEMP(I)
 2040         CONTINUE
              TEMP2(ISET)=SUM/ANI
            ENDIF
!
 2010     CONTINUE
        ENDIF
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
          WRITE(ICOUT,2061)ISET,NI,ANI,XBARI,XBARG
 2061     FORMAT('ISET,NI,ANI,XBARI,XBARG = ',2I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2063)SDI,SIGMAE,SADJ
 2063     FORMAT('SDI,SIGMAE,SADJ = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2064)AK0,AK1,YLOWER,YUPPER
 2064     FORMAT('AK0,AK1,YLOWER,YUPPER = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(1+NBEF.GT.NUMSET-NAFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2065)
 2065     FORMAT('      THERE ARE NOT ENOUGH GROUPS TO FORM THE ',   &
                 'MOVING AVERAGE PLOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2268)KWIDTH,NUMSET
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DO 2090 ISET=1,NUMSET
!
          IF(N.EQ.NUMSET)THEN
            XTAG=0.0
            IF(XHIGH(ISET).GE.0.5)XTAG=1.0
          ENDIF
!
          SUM=0.0
          ISTRT=ISET-NBEF
          ISTOP=ISET+NAFT
          DENOM=AK
          IF(ISET.LT.1+NBEF)THEN
            ISTRT=1
            DENOM=REAL(ISET+NAFT)
          ELSEIF(ISET.GT.NUMSET-NAFT)THEN
            ISTOP=NUMSET
            DENOM=REAL(NUMSET-(ISET-NBEF)+1)
          ENDIF
          DO 2092 II=ISTRT,ISTOP
            SUM=SUM+TEMP2(II)
 2092     CONTINUE
          YVAL=SUM/DENOM
          XVAL=XIDTEM(ISET)
          IF(NBEF.NE.NAFT)THEN
            IF(ISET.GT.1)THEN
              XVAL=(XIDTEM(ISET)+XIDTEM(ISET-1))/2.0
            ELSE
              XVAL=XIDTEM(1)
            ENDIF
          ENDIF
!
          IF(N.NE.NUMSET)THEN
            YUPPER=XBARG+3.09*SIGMAE/SQRT(AK)
            YLOWER=XBARG-3.09*SIGMAE/SQRT(AK)
          ELSE
            IF(KWIDTH.LE.25)THEN
              YUPPER=XBARG+3.09*RANGEM/(E2(KWIDTH)*SQRT(AK))
              YLOWER=XBARG-3.09*RANGEM/(E2(KWIDTH)*SQRT(AK))
            ELSE
              YUPPER=XBARG+3.09*RANGEM/(E2(25)*SQRT(AK))
              YLOWER=XBARG-3.09*RANGEM/(E2(25)*SQRT(AK))
            ENDIF
          ENDIF
!
          CALL DPCC3(ICASPL,J,YVAL,XBARG,YLOWER,YUPPER,   &
                     Y2,X2,D2,XVAL,   &
                     YPREV,NPREV,IHIGH,XTAG,SIGMAE,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 2090   CONTINUE
      ELSEIF(ICASPL.EQ.'MRCC')THEN
!
!       *****************************************
!       **  STEP 5.11--                        **
!       **  TREAT THE MOVING RANGE             **
!       **  CONTROL CHART CASE                 **
!       *****************************************
!
        ISTEPN='5.11'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(KWIDTH.LT.2)KWIDTH=2
        IF(KWIDTH.GT.N-1)KWIDTH=N-1
        AK=REAL(KWIDTH)
        NBEF=KWIDTH/2
        NAFT=NBEF
        IF(MOD(KWIDTH,2).EQ.0)NAFT=NBEF-1
!
        J=0
        XTAG=0.0
!
!       2 CASES:
!         1) UNGROUPED DATA (N=NUMSET)
!         2) GROUPED DATA (N> NUMSET).  FOR GROUPED DATA, EACH GROUP
!            SHOULD HAVE AT LEAST 2 VALUES.
!
!       UNGROUPED CASE
!
        IF(N.EQ.NUMSET)THEN
          DO 2102 ISET=1,N
            TEMP2(ISET)=Y(ISET)
 2102     CONTINUE
        ELSE
!
!         GROUPED CASE
!
          DO 2110 ISET=1,NUMSET
!
            K=0
            DO 2120 I=1,N
              IF(X(I).EQ.XIDTEM(ISET))THEN
                K=K+1
                TEMP(K)=Y(I)
              ENDIF
 2120       CONTINUE
            NI=K
            ANI=NI
!
            IF(NI.LT.2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,31)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2132)
 2132         FORMAT('FOR MOVING RANGE, FOR SOME CLASS NI < 2')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
!
            XTMIN=TEMP(1)
            XTMAX=TEMP(1)
            DO 2140 I=1,NI
              IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
              IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 2140       CONTINUE
            TEMP2(ISET)=XTMAX-XTMIN
 2110     CONTINUE
        ENDIF
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
          WRITE(ICOUT,2161)ISET,NI,ANI,XBARI,XBARG
 2161     FORMAT('ISET,NI,ANI,XBARI,XBARG = ',2I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2163)SDI,SIGMAE,SADJ
 2163     FORMAT('SDI,SIGMAE,SADJ = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2164)AK0,AK1,YLOWER,YUPPER
 2164     FORMAT('AK0,AK1,YLOWER,YUPPER = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(1+NBEF.GT.NUMSET-NAFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2165)
 2165     FORMAT('      THERE ARE NOT ENOUGH GROUPS TO FORM THE ',   &
                 'MOVING RANGE PLOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2268)KWIDTH,NUMSET
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        SUM2=0.0
        NUMRAN=0
        DO 2190 ISET=1,NUMSET
!
!         GROUPED DATA
!
          IF(N.NE.NUMSET)THEN
            SUM=0.0
            ISTRT=ISET-NBEF
            ISTOP=ISET+NAFT
            DENOM=AK
            IF(ISET.LT.1+NBEF)THEN
              ISTRT=1
              DENOM=REAL(ISET+NAFT)
            ELSEIF(ISET.GT.NUMSET-NAFT)THEN
              ISTOP=NUMSET
              DENOM=REAL(NUMSET-(ISET-NBEF)+1)
            ENDIF
            DO 2192 II=ISTRT,ISTOP
              SUM=SUM+TEMP2(II)
 2192       CONTINUE
            YVAL=SUM/DENOM
!
!           UNGROUPED DATA
!
          ELSE
            ISTRT=ISET-NBEF
            ISTOP=ISET+NAFT
            IF(ISET.LT.1+NBEF)THEN
              ISTRT=1
            ELSEIF(ISET.GT.NUMSET-NAFT)THEN
              ISTOP=NUMSET
            ENDIF
            XTMIN=TEMP2(ISTRT)
            XTMMAX=TEMP2(ISTRT)
            DO 2182 II=ISTRT,ISTOP
              IF(TEMP2(II).LT.XTMIN)XTMIN=TEMP2(II)
              IF(TEMP2(II).GT.XTMAX)XTMAX=TEMP2(II)
 2182       CONTINUE
            YVAL=XTMAX-XTMIN
            XTAG=0.0
            IF(XHIGH(ISET).GE.0.5)XTAG=1.0
          ENDIF
          XVAL=XIDTEM(ISET)
          IF(NBEF.NE.NAFT)XVAL=(XIDTEM(ISET)+XIDTEM(ISET-1))/2.0
          IF(KWIDTH.LE.25)THEN
            YUPPER=D4(KWIDTH)*RANGEM
            YLOWER=D3(KWIDTH)*RANGEM
          ELSE
            YUPPER=(1.0+3.0*D4FACT/SQRT(2.0*(REAL(KWIDTH)-1.0)))*RANGEM   &
                   /E2(25)
            YLOWER=(1.0-3.0*D3FACT/SQRT(2.0*(REAL(KWIDTH)-1.0)))*RANGEM   &
                   /E2(25)
          ENDIF
          IF(YLOWER.LT.0.0)YLOWER=0.0
!
          CALL DPCC3(ICASPL,J,YVAL,RANGEM,YLOWER,YUPPER,   &
                     Y2,X2,D2,XVAL,   &
                     YPREV,NPREV,IHIGH,XTAG,RANGEM,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 2190   CONTINUE
      ELSEIF(ICASPL.EQ.'MSCC')THEN
!
!       *****************************************
!       **  STEP 5.12--                        **
!       **  TREAT THE MOVING STANDARD DEVIATION**
!       **  CONTROL CHART CASE                 **
!       *****************************************
!
        ISTEPN='5.12'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(KWIDTH.LT.2)KWIDTH=2
        IF(KWIDTH.GT.N-1)KWIDTH=N-1
        AK=REAL(KWIDTH)
        NBEF=KWIDTH/2
        NAFT=NBEF
        IF(MOD(KWIDTH,2).EQ.0)NAFT=NBEF-1
!
        J=0
        XTAG=0.0
!
!       2 CASES:
!         1) UNGROUPED DATA (N=NUMSET)
!         2) GROUPED DATA (N> NUMSET).  FOR GROUPED DATA, EACH GROUP
!            SHOULD HAVE AT LEAST 2 VALUES.
!
!       UNGROUPED CASE
!
        IF(N.EQ.NUMSET)THEN
          DO 2202 ISET=1,N
            TEMP2(ISET)=Y(ISET)
 2202     CONTINUE
        ELSE
!
!       GROUPED CASE
!
          DO 2210 ISET=1,NUMSET
!
            K=0
            XTAG=0.0
            DO 2220 I=1,N
              IF(X(I).EQ.XIDTEM(ISET))K=K+1
              IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 2220       CONTINUE
            NI=K
            ANI=NI
!
            IF(NI.LT.2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,31)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2232)
 2232         FORMAT('FOR MOVING STANDARD DEVIATION, FOR SOME CLASS ',   &
                     'NI < 2')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
!
            SUM1=0.0
            DO 2240 I=1,NI
              SUM1=SUM1+TEMP(I)
 2240       CONTINUE
            XMEAN=SUM1/ANI
            SUM1=0.0
            DO 2242 I=1,NI
              SUM1=SUM1+(TEMP(I)-XMEAN)**2
 2242       CONTINUE
            SD=SQRT(SUM1/(ANI-1.0))
            TEMP2(ISET)=SD
 2210     CONTINUE
        ENDIF
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
          WRITE(ICOUT,2261)ISET,NI,ANI,XBARI,XBARG
 2261     FORMAT('ISET,NI,ANI,XBARI,XBARG = ',2I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2263)SD,SIGMAE,SADJ
 2263     FORMAT('SD,SIGMAE,SADJ = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2264)AK0,AK1,YLOWER,YUPPER
 2264     FORMAT('AK0,AK1,YLOWER,YUPPER = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(1+NBEF.GT.NUMSET-NAFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2265)
 2265     FORMAT('      THERE ARE NOT ENOUGH GROUPS TO FORM THE ',   &
                 'MOVING STANDARD DEVAITION PLOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2268)KWIDTH,NUMSET
 2268     FORMAT('      THE FILTER WIDTH IS ',I5,' AND THE NUMBER OF ',   &
                 'GROUPS IS ',I5,'.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        SUM2=0.0
        NUMSD=0
        DO 2290 ISET=1,NUMSET
!
!         GROUPED DATA
!
          IF(N.NE.NUMSET)THEN
            SUM=0.0
            ISTRT=ISET-NBEF
            ISTOP=ISET+NAFT
            DENOM=AK
            IF(ISET.LT.1+NBEF)THEN
              ISTRT=1
              DENOM=REAL(ISET+NAFT)
            ELSEIF(ISET.GT.NUMSET-NAFT)THEN
              ISTOP=NUMSET
              DENOM=REAL(NUMSET-(ISET-NBEF)+1)
            ENDIF
            DO 2292 II=ISTRT,ISTOP
              SUM=SUM+TEMP2(II)
 2292       CONTINUE
            YVAL=SUM/DENOM
!
!         UNGROUPED DATA
!
          ELSE
            ISTRT=ISET-NBEF
            ISTOP=ISET+NAFT
            IF(ISET.LT.1+NBEF)THEN
              ISTRT=1
            ELSEIF(ISET.GT.NUMSET-NAFT)THEN
              ISTOP=NUMSET
            ENDIF
            SUM1=0.0
            ICOUNT=0
            DO 2282 II=ISTRT,ISTOP
              ICOUNT=ICOUNT+1
              SUM1=SUM1+TEMP2(II)
 2282       CONTINUE
            XMEAN=SUM1/REAL(ICOUNT)
            SUM1=0.0
            DO 2283 II=ISTRT,ISTOP
              SUM1=SUM1+(TEMP2(II)-XMEAN)**2
 2283       CONTINUE
            IF(ICOUNT.LT.2)GO TO 2290
            YVAL=SQRT(SUM1/REAL(ICOUNT-1))
            XTAG=0.0
            IF(XHIGH(ISET).GE.0.5)XTAG=1.0
          ENDIF
!
          XVAL=XIDTEM(ISET)
          IF(NBEF.NE.NAFT)XVAL=(XIDTEM(ISET)+XIDTEM(ISET-1))/2.0
          IF(KWIDTH.LE.25)THEN
            YUPPER=B4(KWIDTH)*SDM
            YLOWER=B3(KWIDTH)*SDM
          ELSE
            YUPPER=(1.0+3.0/SQRT(2.0*(REAL(KWIDTH)-1.0)))*SDM
            YLOWER=(1.0-3.0/SQRT(2.0*(REAL(KWIDTH)-1.0)))*SDM
          ENDIF
          IF(YLOWER.LT.0.0)YLOWER=0.0
!
          CALL DPCC3(ICASPL,J,YVAL,SDM,YLOWER,YUPPER,   &
                     Y2,X2,D2,XVAL,   &
                     YPREV,NPREV,IHIGH,XTAG,SDM,   &
                     CCLSL,CCUSL,CCTARG,ICONWC,   &
                     IBUGG3,ISUBRO,IERROR)
!
 2290   CONTINUE
!
      ELSEIF(ICASPL.EQ.'1352')THEN
!
!       **********************************************
!       **  STEP 5.13--                             **
!       **  TREAT THE ISO 13528 CONTROL CHART CASE  **
!       **********************************************
!
! THE ISO 13528 CONTROL CHART IS BASED ON THE FOLLOWING:
!
!    1) USE A Z-SCORE AS THE RESPONSE.  SINCE THE STANDARD
!       PROVIDES FOR VARIOUS WAYS TO COMPUTE THE Z-SCORE,
!       ASSUME THAT THE RESPONSE IS ALREADY IN Z-SCORE FORMAT.
!
!    2) IF THERE IS REPLICATION, COMPUTE A MEAN FOR EACH
!       GROUP.  IF THERE IS NO REPLICATION, THEN JUST USE
!       THE DATA VALUE.  UNLIKE THE STANDARD MEAN CONTROL
!       CHART, WE DO NOT AVERAGE OVER SEVERAL VALUES FOR
!       INDIVIDUAL OBSERVATIONS.
!
!    3) CONTROL LIMITS ARE AT +/-2 AND +/-3.
!
!    4) ONE VERSION OF THIS PLOT ALSO PLOTS THE RAW DATA
!       VALUES.
!
!    5) THE MATERIAL-ID CAN BE TREATED AS A "HIGHLIGHTING"
!       VARIABLE.  THEREFORE, LET THE HIGHLIGHT VARIABLE
!       SPECIFY THE MATERIAL ID RATHER THAN JUST 0/1.
!
        ISTEPN='5.13'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        ICNT=0
        DO 2310 ISET=1,NUMSET
!
          K=0
          DO 2320 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              TEMP2(K)=XHIGH(I)
            ENDIF
 2320     CONTINUE
          NI=K
          ANI=NI
!
          IF(NI.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1132)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          SUM=0.0
          DO 2340 I=1,NI
            SUM=SUM+TEMP(I)
 2340     CONTINUE
          STAT=SUM/ANI
!
          ICNT=1
          J=J+1
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
!
          ICNT=ICNT+1
          J=J+1
          Y2(J)=0.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
!
          ICNT=ICNT+1
          J=J+1
          Y2(J)=2.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
!
          ICNT=ICNT+1
          J=J+1
          Y2(J)=-2.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
!
          ICNT=ICNT+1
          J=J+1
          Y2(J)=3.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
!
          ICNT=ICNT+1
          J=J+1
          Y2(J)=-3.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
!
          ICNT=ICNT+1
          DO 2350 II=1,NI
            J=J+1
            Y2(J)=TEMP(II)
            X2(J)=XIDTEM(ISET)
            IF(IHIGH.EQ.'ON')THEN
              D2(J)=REAL(ICNT) + TEMP2(II) - 1.0
            ELSE
              D2(J)=REAL(ICNT)
            ENDIF
 2350     CONTINUE
!
 2310   CONTINUE
      ELSEIF(ICASPL.EQ.'1CUS')THEN
!
!       ****************************************************
!       **  STEP 5.14--                                   **
!       **  TREAT THE ISO 13528 CUSUM CONTROL CHART CASE  **
!       ****************************************************
!
! THE ISO 13528 CUSUM CONTROL CHART IS BASED ON THE FOLLOWING:
!
!    1) USE A Z-SCORE AS THE RESPONSE.  SINCE THE STANDARD
!       PROVIDES FOR VARIOUS WAYS TO COMPUTE THE Z-SCORE,
!       ASSUME THAT THE RESPONSE IS ALREADY IN Z-SCORE FORMAT.
!
!    2) IF THERE IS REPLICATION, COMPUTE A MEAN FOR EACH
!       GROUP.  IF THERE IS NO REPLICATION, THEN JUST USE
!       THE DATA VALUE.
!
!    3) SIMPLY PLOT THE CUMULATIVE SUM OF THE Z-SCORES.
!       THE TARGET VALUE IS ZERO.
!
!    4) THERE ARE NO CONTROL LIMITS FOR THIS PLOT.
!
!    5) THE MATERIAL-ID CAN BE TREATED AS A "HIGHLIGHTING"
!       VARIABLE.  THEREFORE, LET THE HIGHLIGHT VARIABLE
!       SPECIFY THE MATERIAL ID RATHER THAN JUST 0/1.
!
        ISTEPN='5.13'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        ICNT=0
        CUSUM=0.0
        DO 2410 ISET=1,NUMSET
!
          K=0
          DO 2420 I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              TEMP2(K)=XHIGH(I)
            ENDIF
 2420     CONTINUE
          NI=K
          ANI=NI
!
          IF(NI.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1132)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          SUM=0.0
          DO 2440 I=1,NI
            SUM=SUM+TEMP(I)
 2440     CONTINUE
          CUSUM=CUSUM + (SUM/ANI)
!
          ICNT=1
          J=J+1
          Y2(J)=CUSUM
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
!
          ICNT=ICNT+1
          J=J+1
          Y2(J)=0.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
!
 2410   CONTINUE
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1053)
 1053   FORMAT('      ICASPL NOT EQUAL ONE OF THE ALLOWABLE 12--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1054)
 1054   FORMAT('      MECC, SDCC, RACC, CSCC, PCC, PNCC, UCC, CCC, ',   &
               'EWMA, MACC, MSCC, OR MRCC.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1056)ICASPL
 1056   FORMAT('      ICASPL = ',A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      N2=J
      NPLOTV=3
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,ICASPL,N,NUMSET,N2
 9012   FORMAT('IERROR,ICASPL,N,NUMSET,N2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMV2,ISIZE
 9013   FORMAT('NUMV2,ISIZE = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)AN,XBARG,SDG,RANGEG
 9014   FORMAT('AN,XBARG,SDG,RANGEG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ANUMSE,SIGMAE,RANGEE
 9015   FORMAT('ANUMSE,SIGMAE,RANGEE = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPCC2
      SUBROUTINE DPCC3(ICASPL,J,STAT,YMID,YLOWER,YUPPER,   &
                       Y2,X2,D2,XVAL,   &
                       YPREV,NPREV,IHIGH,XHIGH,SIGMA,   &
                       CCLSL,CCUSL,CCTARG,ICONWC,   &
                       IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE-UTIITY ROUTINE USED BY DPCC.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2012/1
!     ORIGINAL VERSION--JANUARY   2012. EXTRACTED FROM DPCC2
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONWC
      CHARACTER*4 IHIGH
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION YPREV(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPCC3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)J,STAT,XVAL,ICASPL,ICONWC,ISUBRO
   71   FORMAT('J,STAT,XVAL,ICASPL,ICONWC,ISUBRO = ',I8,2G15.7,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)IHIGH,XHIGH,SIGMA
   74   FORMAT('IHIGH,XHIGH,SIGMA = ',A4,2X,2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IERROR='NO'
!
      ICNT=1
      J=J+1
      Y2(J)=STAT
      X2(J)=XVAL
      D2(J)=REAL(ICNT)
!
!     IF "ISO 13528" CONTROL LIMITS REQUESTED, SPECIFY LIMITS
!     AT +/-2 AND +/-3.  THESE ONLY APPLY TO "MEAN CONTROL"
!     CHART.
!
      IF(ICONWC.EQ.'ISO' .AND.   &
        (ICASPL.EQ.'MECC' .OR. ICASPL.EQ.'MACC'))THEN
!
        ICNT=ICNT+1
        J=J+1
        Y2(J)=0.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
!
        ICNT=ICNT+1
        J=J+1
        Y2(J)=2.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
!
        ICNT=ICNT+1
        J=J+1
        Y2(J)=-2.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
!
        ICNT=ICNT+1
        J=J+1
        Y2(J)=3.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
!
        ICNT=ICNT+1
        J=J+1
        Y2(J)=-3.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
!
      ELSE
        ICNT=ICNT+1
        J=J+1
        Y2(J)=YMID
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
!
        ICNT=ICNT+1
        J=J+1
        Y2(J)=YUPPER
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
!
        ICNT=ICNT+1
        J=J+1
        Y2(J)=YLOWER
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
!
!       IMPLEMENT WECO (WESTERN ELECTRIC) RULES FOR MEAN, SD,
!       AND RANGE CONTROL CHARTS.  THESE ARE TYPICALLY USED IN
!       ADDITION TO THE STANDARD CONTROL LIMITS.  ONE DRAWBACK TO
!       THESE RULES IS THAT THEY CAN LEAD TO AN EXCESSIVE NUMBER
!       OF FALSE POSITIVES.
!
!       THESE RULES FLAG THE FOLLOWING (THESE ARE LISTED FOR
!       POINTS ABOVE THE CENTER LINE (I.E., YMID).  THERE ARE
!       SIMILAR RULES FOR POINTS BELOW THE CENTER LINE.
!
!          1) ANY POINT > 3*SIGMA
!          2) 2 OUT OF LAST 3 POINTS > 2*SIGMA
!          3) 4 OUT OF LAST 5 POINTS > 1*SIGMA
!          4) 8 CONSECUTIVE POINTS ABOVE CENTER LINE
!
!       FOR RULE 1, WE DO NOT NEED ANY PAST DATA.  FOR THE OTHERS,
!       PASS IN AN ARRAY THAT CONTAINS THE PREVIOUS DATA.
!
        IF(ICONWC.EQ.'WECO' .AND.   &
          (ICASPL.EQ.'MECC' .OR. ICASPL.EQ.'MACC' .OR.   &
           ICASPL.EQ.'RACC' .OR. ICASPL.EQ.'MRCC' .OR.   &
           ICASPL.EQ.'SDCC' .OR. ICASPL.EQ.'MSCC'))THEN
!
          ITAG=0
          NPREV=NPREV+1
          YPREV(NPREV)=STAT
!
          IF(STAT.GT.YMID + 3.0*SIGMA)THEN
            ITAG=1
          ELSEIF(STAT.LT.YMID - 3.0*SIGMA)THEN
            ITAG=1
          ENDIF
!
          IF(NPREV.GE.3)THEN
            ISTRT=NPREV-2
            ICNT1=0
            ICNT2=0
            DO 1020 I=ISTRT,NPREV
              IF(YPREV(I).GT.YMID + 2.0*SIGMA)ICNT1=ICNT1+1
              IF(YPREV(I).LT.YMID - 2.0*SIGMA)ICNT2=ICNT2+1
 1020       CONTINUE
            IF(ICNT1.GE.2 .OR. ICNT2.GE.2)ITAG=1
          ENDIF
!
          IF(NPREV.GE.5)THEN
            ISTRT=NPREV-4
            ICNT1=0
            ICNT2=0
            DO 1030 I=ISTRT,NPREV
              IF(YPREV(I).GT.YMID + SIGMA)ICNT1=ICNT1+1
              IF(YPREV(I).LT.YMID - SIGMA)ICNT2=ICNT2+1
 1030       CONTINUE
            IF(ICNT1.GE.2 .OR. ICNT2.GE.2)ITAG=1
          ENDIF
!
          IF(NPREV.GE.8)THEN
            ISTRT=NPREV-7
            IFLAG=1
            IF(STAT.GT.YMID)THEN
              DO 1040 I=ISTRT,NPREV-1
                IF(YPREV(I).LT.YMID)IFLAG=0
 1040         CONTINUE
            ELSEIF(STAT.LT.YMID)THEN
              DO 1045 I=ISTRT,NPREV-1
                IF(YPREV(I).GT.YMID)IFLAG=0
 1045         CONTINUE
            ENDIF
            IF(IFLAG.EQ.1)ITAG=1
          ENDIF
!
          IF(ITAG.EQ.1)THEN
            ICNT=ICNT+1
            J=J+1
            Y2(J)=STAT
            X2(J)=XVAL
            D2(J)=REAL(ICNT)
          ENDIF
        ENDIF
!
      ENDIF
!
      IF(CCTARG.NE.CPUMIN)THEN
        ICNT=ICNT+1
        J=J+1
        Y2(J)=CCTARG
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
      ENDIF
!
      IF(CCUSL.NE.CPUMIN)THEN
        ICNT=ICNT+1
        J=J+1
        Y2(J)=CCUSL
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
      ENDIF
!
      IF(CCLSL.NE.CPUMIN)THEN
        ICNT=ICNT+1
        J=J+1
        Y2(J)=CCLSL
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
      ENDIF
!
      IF(IHIGH.EQ.'ON' .AND. XHIGH.GE.0.5)THEN
        ICNT=ICNT+1
        J=J+1
        Y2(J)=STAT
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
      ENDIF
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCC3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCC3
      SUBROUTINE DPCD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                      IANGLU,DEMOFR,DEMODF,   &
                      IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE ONE OF THE FOLLOWING 2
!              COMPLEX DEMODULATION PLOTS--
!                   1) AMPLITUDE;
!                   2) PHASE;
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JUNE      1978.
!     UPDATED         --JULY      1981.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --MARCH     2011. USE DPPARS AND DPPAR3
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
!
      CHARACTER*4 ICASE
      PARAMETER (MAXSPN=10)
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
!CCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
!CCCC END CHANGE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA PI/3.141592653/
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      ISUBN1='DPCD'
      ISUBN2='    '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ***********************************************
!               **  TREAT THE COMPLEX DEMODULATION CASE      **
!               ***********************************************
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPCD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,IANGLU,DEMODF
   52   FORMAT('ICASPL,IAND1,IAND2,IANGLU,DEMODF = ',4(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.3 .AND. ICOM.EQ.'COMP' .AND.   &
         IHARG(1).EQ.'DEMO' .AND. IHARG(2).EQ.'AMPL' .AND.   &
         IHARG(3).EQ.'PLOT')THEN
        ICASPL='CDAM'
        ILASTC=3
      ELSEIF(NUMARG.GE.3 .AND. ICOM.EQ.'COMP' .AND.   &
             IHARG(1).EQ.'DEMO' .AND. IHARG(2).EQ.'PHAS' .AND.   &
             IHARG(3).EQ.'PLOT')THEN
        ICASPL='CDPH'
        ILASTC=3
      ELSE
        IFOUND='NO'
        GO TO 9000
      ENDIF
!
      IFOUND='YES'
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='COMPLEX DEMODULATION PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR,ICASPL
  282   FORMAT('NQ,NUMVAR,ICASPL = ',2I8,2X,A4)
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
!     EXTRACT THE VARIABLE.
!
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,Y1,Y1,NLEFT,NLOCAL,NLOCAL,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  STEP 7--                                        **
!               **  DETERMINE IF THE ANALYST                        **
!               **  HAS SPECIFIED    THE DEMODULATION FREQUENCY     **
!               **  FOR THE COMPLEX DEMODULATION ANALYSIS.          **
!               **  THE FREQUENCY SETTING IS DEFINED BY PRE-USE     **
!               **  OF THE DEMODULATION FREQUENCY     COMMAND.      **
!               **  IF FOUND, USE THE SPECIFIED VALUE.              **
!               **  IF NOT FOUND, GENERATE AN ERROR MESSAGE.        **
!               ******************************************************
!
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DEMOF2=DEMOFR
      IF(IANGLU.EQ.'DEGR')DEMOF2=DEMOF2*PI/180.0
      IF(IANGLU.EQ.'GRAD')DEMOF2=DEMOF2*PI/200.0
!CCCC IF(0.0.LT.DEMOF2.AND.DEMOF2.LT.0.5)GO TO 790
!
      IF(DEMOF2.LE.0.0 .OR. DEMOF2.GE.0.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,741)
  741   FORMAT('****** ERROR IN COMPLEX DEMODULATION PLOT--')
        CALL DPWRST('XXX','BUG ')
        IF(ICASPL.EQ.'CDAM')THEN
          WRITE(ICOUT,742)
  742     FORMAT('       FOR A COMPLEX DEMODULATION AMPLITUDE PLOT,')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASPL.EQ.'CDPH')THEN
          WRITE(ICOUT,743)
  743     FORMAT('       FOR A COMPLEX DEMODULATION PHASE PLOT,')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,744)
  744   FORMAT('       THE FREQUENCY AT WHICH THE DEMODULATION IS TO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,746)
  746   FORMAT('       PERFORMED MUST BE PRE-SPECIFIED BY THE ANALYST,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,747)
  747   FORMAT('       AND MUST BE BETWEEN 0 AND 0.5 RADIANS;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,748)
  748   FORMAT('       SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,749)DEMOFR,IANGLU
  749   FORMAT('       THE DEMODULATION FREQUENCY = ',G15.7,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(IANGLU.NE.'RADI')THEN
          WRITE(ICOUT,750)DEMOF2
  750     FORMAT('       THE DEMODULATION FREQUENCY = ',G15.7,2X,   &
                 'RADIANS')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,751)
  751   FORMAT('       TO DEFINE THE DEMODULATION FREQUENCY, USE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,753)
  753   FORMAT('       DEMODULATION FREQUENCY     COMMAND, AS IN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,754)
  754   FORMAT('            DEMODULATION FREQUENCY 0.3')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,755)
  755   FORMAT('            DEMODULATION FREQUENCY 0.155')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************************
!               **  STEP 8--                                           *
!               **  COMPUTE THE APPROPRIATE COMPLEX DEMODULATION       *
!               **  PLOT  (AMPLITUDE OR PHASE).                        *
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS              *
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.                 *
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).      *
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).      *
!               ********************************************************
!
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPCD2(Y1,NLEFT,ICASPL,DEMOF2,DEMODF,   &
                 Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DPCD')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPHIST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DEMOFR,IANGLU,DEMOF2
 9014   FORMAT('DEMOFR,IANGLU,DEMOF2 = ',G15.7,2X,A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO 9015 I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPCD
      SUBROUTINE DPCD2(Y,N,ICASPL,F,DEMODF,   &
                       Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE PERFORMS A COMPLEX DEMODULATION
!              ON THE DATA IN THE INPUT VECTOR X
!              AT THE INPUT DEMODULATION FREQUENCY = F.
!              THE COMPLEX DEMODULATION CONSISTS OF THE FOLLOWING--
!              1) AN AMPLITUDE VERSUS TIME PLOT;
!              2) A PHASE VERSUS TIME PLOT;
!              3) AN UPDATED DEMODULATION FREQUENCY ESTIMATE
!                 TO ASSIST THE ANALYST IN DETERMINING A
!                 MORE APPROPRIATE FREQUENCY AT WHICH
!                 TO DEMODULATE IN CASE THE SPECIFIED
!                 INPUT DEMODULATION FREQUENCY F
!                 DOES NOT FLATTEN SUFFICIENTLY THE
!                 PHASE PLOT.
!
!              THE ALLOWABLE RANGE OF THE INPUT DEMODULATION
!              FREQUENCY F IS 0.0 TO 0.5 (EXCLUSIVELY).
!              THE INPUT DEMODULATION FREQUENCY F IS MEASURED  OF
!              IN UNITS OF CYCLES PER 'DATA POINT' OR,
!              MORE PRECISELY, IN CYCLES PER UNIT TIME WHERE
!              'UNIT TIME' IS DEFINED AS THE
!              ELAPSED TIME BETWEEN ADJACENT OBSERVATIONS.
!
!     INPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED) OBSERVATIONS.
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!                      FREQ   = THE SINGLE PRECISION
!                               DEMODULATION FREQUENCY.
!                               F IS IN UNITS OF CYCLES PER DATA POINT.
!                               F IS BETWEEN 0.0 AND 0.5 (EXCLUSIVELY).
!     OUTPUT--2 PAGES OF AUTOMATIC PRINTOUT--
!             1) AN AMPLITUDE PLOT;
!             2) A PHASE PLOT; AND
!             3) AN UPDATED DEMODULATION FREQUENCY ESTIMATE.
!     PRINTING--YES.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS 5000.
!                 --THE SAMPLE SIZE N MUST BE GREATER
!                   THAN OR EQUAL TO 3.
!                 --THE INPUT FREQUENCY F MUST BE
!                   GREATER THAN OR EQUAL TO 2/(N-2).
!                 --THE INPUT FREQUENCY F MUST BE
!                   SMALLER THAN 0.5.
!     OTHER DATAPAC   SUBROUTINES NEEDED--PLOTX.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, SIN, COS, ATAN.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--IN ORDER THAT THE RESULTS OF THE COMPLEX DEMODULATION
!              BE VALID AND PROPERLY INTERPRETED, THE INPUT DATA
!              IN X SHOULD BE EQUI-SPACED IN TIME
!              (OR WHATEVER VARIABLE CORRESPONDS TO TIME).
!            --IF THE INPUT OBSERVATIONS IN X ARE CONSIDERED
!              TO HAVE BEEN COLLECTED 1 SECOND APART IN TIME,
!              THEN THE DEMODULATION FREQUENCY F
!              WOULD BE IN UNITS OF HERTZ
!              (= CYCLES PER SECOND).
!            --A FREQUENCY OF 0.0 CORRESPONDS TO A CYCLE
!              IN THE DATA OF INFINITE (= 1/(0.0))
!              LENGTH OR PERIOD.
!              A FREQUENCY OF 0.5 CORRESPONDS TO A CYCLE
!              IN THE DATA OF LENGTH = 1/(0.5) = 2 DATA POINTS.
!            --IN EXAMINING THE AMPLITUDE AND PHASE PLOTS,
!              ATTENTION SHOULD BE PAID NOT ONLY TO THE
!              STRUCTURE OF THE PHASE PLOT
!              (NEAR-ZERO SLOPE VERSUS NON-ZERO SLOPE)
!              BUT ALSO TO THE RANGE
!              OF VALUES ON THE VERTICAL AXIS.
!              A PLOT WITH MUCH STRUCTURE BUT
!              WITH A SMALL RANGE ON THE VERTICAL AXIS
!              IS USUALLY MORE INDICATIVE OF A
!              DEFINITE CYCLIC COMPONENT AT THE
!              SPECIFIED INPUT DEMODULATION FREQUENCY,
!              THAN IS A PLOT WITH LESS STRUCTURE BUT
!              A WIDER RANGE ON THE VERTICAL AXIS.
!            --INTERNAL TO THIS SUBROUTINE, 2 MOVING
!              AVERAGES ARE APPLIED, EACH OF LENGTH 1/F.
!              HENCE THE AMPLITUDE AND PHASE PLOTS
!              HAVE N - 2/F VALUES
!              (RATHER THAN N VALUES) ALONG THE
!              HORIZONTAL (TIME) AXIS.
!              IN ORDER THAT THE AMPLITUDE AND PHASE
!              PLOTS BE NON-EMPTY, AN INPUT
!              REQUIREMENT ON F FOR THIS SUBROUTINE
!              IS THAT THE SAMPLE SIZE N
!              AND THE DEMODULATION FREQUENCY F
!              MUST BE SUCH THAT
!              N - 2/F BE GREATER THAN ZERO.
!              FURTHER, SINCE A PLOT WITH BUT
!              1 POINT IS MEANINGLESS
!              AND OUGHT ALSO BE EXCLUDED,
!              THE REQUIREMENT IS EXTENDED
!              SO THAT N - 2/F MUST BE GREATER THAN 1.
!     REFERENCES--GRANGER AND HATANAKA, PAGES 170 TO 189,
!                 ESPECIALLY PAGES 173, 177, AND 182.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1972.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --FEBRUARY  1976.
!     UPDATED         --JUNE      1978.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --MAY       1982.
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y(*)
!
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA PI/3.141592653/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCD'
      ISUBN2='2   '
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCD2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCD2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N,ICASPL
   52   FORMAT('N,ICASPL = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      ILOWER=3
      IUPPER=MAXOBV
      AN=N
      FMIN=2.0/(AN-2.0)
!
!               ********************************************
!               **  STEP 0--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.ILOWER.OR.N.GT.IUPPER)GO TO 50
      IF(F.LE.FMIN.OR.F.GE.0.5)GO TO 60
      HOLD=Y(1)
      DO 65 I=2,N
      IF(Y(I).NE.HOLD)GO TO 95
   65 CONTINUE
      WRITE(ICOUT, 9)HOLD
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
   50 WRITE(ICOUT,17)ILOWER,IUPPER
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
   60 WRITE(ICOUT,27)FMIN
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)F
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,28)FMIN,N
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
   95 CONTINUE
    9 FORMAT('***** WARNING--THE FIRST ARGUMENT ',   &
      '(A VECTOR) TO THE DPCD2  SUBROUTINE HAS ALL ELEMENTS = ',   &
      G15.7)
   17 FORMAT('***** ERROR--THE SECOND ARGUMENT TO THE ',   &
      'DPCD2  SUBROUTINE IS OUTSIDE THE ALLOWABLE (',I6,',',I6,') ',   &
      'INTERVAL')
   27 FORMAT('***** ERROR--THE THIRD ARGUMENT TO THE ',   &
      'DPCD2  SUBROUTINE IS OUTSIDE THE ALLOWABLE (',I6,'0.5) ',   &
      'INTERVAL')
   28 FORMAT('                   THE ABOVE LOWER LIMIT (',F11.8,   &
      ') = 2/(N-2) WHERE N = THE INPUT SAMPLE SIZE = ',I8)
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!               ******************************
!               **  STEP 1--                **
!               **  FORM THE COSINE SERIES  **
!               ******************************
!
      DO 100 I=1,N
        AI=I
        Y2(I)=Y(I)*COS(2.0*PI*F*AI)
  100 CONTINUE
!
!     DEFINE THE LENGTH OF THE 2 MOVING AVERAGES
!
      LENMA1=INT(1.0/F)
      LENMA2=INT(1.0/F)
      ALEN1=REAL(LENMA1)
      ALEN2=REAL(LENMA2)
      IMAX1=N-LENMA1
      IMAX2=IMAX1-LENMA2
!
!               *********************************************************
!               **  STEP 2--                                           **
!               **  FORM THE FIRST MOVING AVERAGE FOR THE COSINE SERIES**
!               *********************************************************
!
      DO 200 I=1,IMAX1
        ISTART=I+1
        IEND=I+LENMA1-1
        IENDP1=I+LENMA1
        SUM=0.0
        DO 300 J=ISTART,IEND
          SUM=SUM+Y2(J)
  300   CONTINUE
        SUM=SUM+Y2(I)/2.0+Y2(IENDP1)/2.0
        D2(I)=SUM/ALEN1
  200 CONTINUE
!
!               ************************************************************
!               **  STEP 3--                                              **
!               **  FORM THE SECOND MOVING AVERAGE FOR THE COSINE SERIES  **
!               ************************************************************
!
      DO 400 I=1,IMAX2
        ISTART=I+1
        IEND=I+LENMA2-1
        IENDP1=I+LENMA2
        SUM=0.0
        DO 500 J=ISTART,IEND
          SUM=SUM+D2(J)
  500   CONTINUE
        SUM=SUM+D2(I)/2.0+D2(IENDP1)/2.0
        Y2(I)=SUM/ALEN2
  400 CONTINUE
!
!               ****************************
!               **  STEP 4--              **
!               **  FORM THE SINE SERIES  **
!               ****************************
!
      DO 700 I=1,N
        AI=I
        X2(I)=Y(I)*SIN(2.0*PI*F*AI)
  700 CONTINUE
!
!               *********************************************************
!               **  STEP 5--                                           **
!               **  FORM THE FIRST MOVING AVERAGE FOR THE SINE SERIES  **
!               *********************************************************
!
      DO 800 I=1,IMAX1
        ISTART=I+1
        IEND=I+LENMA1-1
        IENDP1=I+LENMA1
        SUM=0.0
        DO 900 J=ISTART,IEND
          SUM=SUM+X2(J)
  900   CONTINUE
        SUM=SUM+X2(I)/2.0+X2(IENDP1)/2.0
        D2(I)=SUM/ALEN1
  800 CONTINUE
!
!               **********************************************************
!               **  STEP 6--                                            **
!               **  FORM THE SECOND MOVING AVERAGE FOR THE SINE SERIES  **
!               **********************************************************
!
      DO 1000 I=1,IMAX2
        ISTART=I+1
        IEND=I+LENMA1-1
        IENDP1=I+LENMA1
        SUM=0.0
        DO 1100 J=ISTART,IEND
          SUM=SUM+D2(J)
 1100   CONTINUE
        SUM=SUM+D2(I)/2.0+D2(IENDP1)/2.0
        X2(I)=SUM/ALEN2
 1000 CONTINUE
!
!     CHECK FOR DESIRED CASE
!     AND BRANCH ACCORDINGLY.
!
      IF(ICASPL.EQ.'CDAM')GO TO 1400
      IF(ICASPL.EQ.'CDPH')GO TO 1700
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1311)
 1311 FORMAT('***** INTERNAL ERROR IN DPCD2 ',   &
      'AT BRANCH POINT 1311--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1312)
 1312 FORMAT('      ICASPL SHOULD BE EITHER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1313)
 1313 FORMAT('      CDAM   OR    CDPH, BUT IS NEITHER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1314)ICASPL
 1314 FORMAT('      ICASPL = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
!               *****************************************
!               **  STEP 7--                           **
!               **  FORM THE AMPLITUDES AND PLOT THEM  **
!               *****************************************
!
 1400 CONTINUE
      DO 1450 I=1,IMAX2
      Y2(I)=2.0*SQRT(Y2(I)*Y2(I)+X2(I)*X2(I))
      X2(I)=I
      D2(I)=1.0
 1450 CONTINUE
      N2=IMAX2
      NPLOTV=2
!CCCC WRITE(ICOUT,1451)F
!1451 FORMAT(30X, 48HAMPLITUDE PLOT FOR THE DEMODULATION FREQUENCY =
!CCCC1 ,F8.6,21H CYCLES PER UNIT TIME)
!CCCC CALL DPWRST('XXX','BUG ')
!
!     COMPUTE THE DIFFERENCE BETWEEN THE MAX AND MIN AMPLITUDES AND WRITE IT OUT
!
      Y2MIN=Y2(1)
      Y2MAX=Y2(1)
      DO 1600 I=1,IMAX2
      IF(Y2(I).LT.Y2MIN)Y2MIN=Y2(I)
      IF(Y2(I).GT.Y2MAX)Y2MAX=Y2(I)
 1600 CONTINUE
      RANGE=Y2MAX-Y2MIN
!CCCC WRITE(ICOUT,1651)Y2MIN,Y2MAX,RANGE
!1651 FORMAT(9X,20HMINIMUM AMPLITUDE = ,E15.8,5X,20HMAXIMUM AMPLITUD
!CCCC1E = ,E15.8,5X,22HRANGE OF AMPLITUDES = ,E15.8)
!CCCC CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               *************************************
!               **  STEP 8--                       **
!               **  FORM THE PHASES AND PLOT THEM  **
!               *************************************
!
 1700 CONTINUE
      DO 1750 I=1,IMAX2
        Y2(I)=ATAN(Y2(I)/X2(I))
        X2(I)=I
        D2(I)=1.0
 1750 CONTINUE
      N2=IMAX2
      NPLOTV=2
!
!CCCC WRITE(ICOUT,1751)F
!1751 FORMAT(32X, 44HPHASE PLOT FOR THE DEMODULATION FREQUENCY = ,F8
!CCCC1.6,21H CYCLES PER UNIT TIME)
!CCCC CALL DPWRST('XXX','BUG ')
!
!     COMPUTE A NEW ESTIMATE FOR THE DEMODULATION FREQUENCY AND WRITE IT OUT
!
      AIMAX2=IMAX2
      IMAX2M=IMAX2-1
      IFLAG=0
      Y2MIN=Y2(1)
      Y2MAX=Y2(1)
      DO 1800 I=1,IMAX2M
        IP1=I+1
        DEL=Y2(IP1)-Y2(I)
        IF(DEL.GT.2.5)IFLAG=IFLAG-1
        IF(DEL.LT.-2.5)IFLAG=IFLAG+1
        AIFLAG=REAL(IFLAG)
        Y2NEW=Y2(IP1)+AIFLAG*PI
        IF(Y2NEW.LT.Y2MIN)Y2MIN=Y2NEW
        IF(Y2NEW.GT.Y2MAX)Y2MAX=Y2NEW
 1800 CONTINUE
      RANGE=Y2MAX-Y2MIN
      SLOPER=RANGE/AIMAX2
      SLOPEH=SLOPER/(2.0*PI)
      FEST=F+SLOPEH
      DEMODF=FEST
!CCCC WRITE(ICOUT,2025)Y2MIN,Y2MAX,RANGE
!2025 FORMAT(3X,16HMINIMUM PHASE = ,E15.8,11H RADIANS   ,16HMAXIMUM
!CCCC1PHASE = ,E15.8,11H RADIANS   ,18HRANGE OF PHASES = ,E15.8,8H RADIA
!CCCC1NS)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,2030)SLOPER,SLOPEH,FEST
!2030 FORMAT(8HSLOPE = ,E14.8,11H RADIANS = ,E14.6,52H CYCLES PER UN
!CCCC1IT TIME    EST. OF NEW DEMOD. FREQ. = ,E15.8,15H CYC./UNIT TIME)
!CCCC CALL DPWRST('XXX','BUG ')
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPCD2
      SUBROUTINE DPCDC3(Y,N,ICASA2,ICASA4,ISEED,MAXNXT,   &
                        TEMP1,ALPHA,NALPHA,ALOWLM,AUPPLM,   &
                        CD,YMED,YAAD,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES CONFIDENCE LIMITS FOR THE
!              COEFFIENT OF DISPERSION.  THE COEFFICIENT OF DISPERSION
!              IS AN ALTERNATIVE TO THE COEFFICIENT OF VARIATION FOR
!              NON-NORMAL DATA.
!
!              THE FOLLOWING CASES ARE SUPPORTED:
!
!                 LET A = LOWER COEFFICIENT OF DISPERSION CONFIDENCE LIMIT Y
!                 LET A = UPPER COEFFICIENT OF DISPERSION CONFIDENCE LIMIT Y
!                 LET A = ONE SIDED LOWER COEFFICIENT OF DISPERSION CONFIDENCE LIMIT Y
!                 LET A = ONE SIDED UPPER COEFFICIENT OF DISPERSION CONFIDENCE LIMIT Y
!
!              THE DATA CONSISTS OF N OBSERVATIONS IN Y.
!
!              THIS ALGORITHM IS FROM THE BONETT AND SEIER PAPER.
!
!              THE COEFFICIENT OF DISPERSION IS DEFINED AS:
!
!                  CD = MEAN ABSOLUTE DEVIATION/MEDIAN
!
!     INPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED OR SORTED) OBSERVATIONS.
!                    --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR Y.
!                    --ALPHA  = THE SINGLE PRECISION VECTOR OF CONFIDENCE
!                               LEVELS
!                      NALPHA = THE INTEGER NUMBER OF ALPHA VALUES
!     OUTPUT ARGUMENTS-ALOWLM = THE SINGLE PRECISION VECTOR OF LOWER
!                               CONFIDENCE LIMIT VALUES
!                     -AUPPLM = THE SINGLE PRECISION VECTOR OF UPPER
!                               CONFIDENCE LIMIT VALUES
!     OTHER DATAPAC   SUBROUTINES NEEDED--MEDIAN, MEAN, VAR, AAD, SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--BONETT AND SEIER (2006), "CONFIDENCE INTERVAL FOR A
!                 COEFFICIENT OF DISPERSION", BIOMETRICAL JOURNAL,
!                 VOL. 48, NO. 1, PP. 144-148.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     ORIGINAL VERSION--NOVEMBER  2017.
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION ALOWLM(*)
      DIMENSION AUPPLM(*)
      DIMENSION ALPHA(*)
!
      INTEGER ASTAR
      INTEGER BSTAR
!
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA4
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='CDC3'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPCDC3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASA2,ICASA4
   52   FORMAT('IBUGA3,ISUBRO,ICASA2,ICASA4 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)N,NALPHA,ISEED,ALPHA(1)
   53   FORMAT('N,NALPHA,ISEED,ALPHA(1) = ',3I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        DO 76 I=1,NALPHA
          WRITE(ICOUT,77)I,ALPHA(I)
   77     FORMAT('I,ALPHA(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   76   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 110 I=1,NALPHA
        ALOWLM(I)=CPUMIN
        AUPPLM(I)=CPUMIN
  110 CONTINUE
!
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: COEFFICIENT OF DISPERSION CONFIDENCE ',   &
               'LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF ORIGINAL OBSERVATIONS  IS LESS ',   &
               'THAN THREE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CARRY OUT CALCULATIONS FOR CONFIDENCE **
!               **  LIMITS.                               **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'CDC3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     ICASA2:  LOWE     => LOWER LIMIT
!              UPPE     => UPPER LIMIT
!     ICASA4:  ONES     => ONE-SIDED LIMIT
!              TWOS     => TWO-SIDED LIMIT
!
!     COMPUTE MEDIAN AND MEAN ABSOLUTE DEVIATION FROM MEDIAN
!
      CALL MEDIAN(Y,N,IWRITE,TEMP1,MAXNXT,YMED,IBUGA3,IERROR)
      CALL AAD(Y,N,IWRITE,TEMP1,MAXNXT,YAAD,'MEDI',IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,201)YMED,YAAD
  201   FORMAT('YMED,YAAD = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(YAAD.EQ.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,207)
  207   FORMAT('      THE MEAN ABSOLUTE DEVIATION FROM THE MEDIAN IS ',   &
               'ZERO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,209)
  209   FORMAT('      THE COEFFICIENT OF DISPERSION CONFIDENCE LIMIT ',   &
               'IS NOT COMPUTED IN THIS CASE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ELSEIF(YMED.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,217)
  217   FORMAT('      THE MEDIAN OF THE OBSERVATIONS IS NON-POSITIVE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,209)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      CD=YAAD/YMED
!
      CALL MEAN(Y,N,IWRITE,U,IBUGA3,IERROR)
      CALL VAR(Y,N,IWRITE,V,IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,220)CD,U,V
  220   FORMAT('CD,U,V = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      AN=REAL(N)
      DEL=(U-YMED)/YAAD
      GAM=V/(YAAD**2)
      CALL SORT(Y,N,Y)
      C=AN/(AN-1.0)
      TERM1=(AN+1.0)/2.0 - SQRT(AN)
      ASTAR=INT(TERM1+0.5)
      BSTAR=N-ASTAR+1
      TERM1=LOG(Y(ASTAR)) - LOG(Y(BSTAR))
      VRLETA=(TERM1/4.0)**2
      SE1=SQRT(VRLETA)
      VRLTAU=(GAM + (DEL**2) - 1.0)/AN
      SE2=SQRT(VRLTAU)
      CVLTLE=(DEL*SQRT(VRLETA))/SQRT(AN)
      AK=SQRT(VRLETA + VRLTAU - 2.0*CVLTLE)/(SE1 + SE2)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,223)DEL,GAMC,VRLETA,SE1,VRLTAU,SE2
  223   FORMAT('DEL,GAMC,VRLETA,SE1,VRLTAU,SE2 = ',6G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,225)ASTAR,BSTAR,CVLTLE,AK
  225   FORMAT('ASTAR,BSTAR,CVLTLE,AK = ',2I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      DO 300 I=1,NALPHA
!
!       GET NORMAL CRITICAL VALUE
!
        ALP=ALPHA(I)
        IF(ALP.GE.1.0 .AND. ALP.LE.100.)ALP=ALP/100.
        IF(ALP.LE.0.0 .OR. ALP.GE.1.0)THEN
          IF(ICASA4.EQ.'ONES')THEN
            Z=1.645
          ELSE
            Z=1.96
          ENDIF
        ELSE
          IF(ALP.LT.0.5)THEN
            ALP=1.0-ALP
          ENDIF
          ALP=1.0 - ALP
          IF(ICASA4.EQ.'ONES')THEN
            P1=ALP
            P2=1.0-ALP
            CALL NORPPF(P2,Z)
          ELSE
            P1=ALP/2.0
            P2=1.0-(ALP/2.0)
            CALL NORPPF(P2,Z)
          ENDIF
        ENDIF
!
        A=(AN+1)/2.0 - AK*Z*SQRT(AN/4.0)
        IA=INT(A+0.5)
        IF(IA.LE.0)THEN
          ALOWLM(I)=EXP(AL1-AU2STR)
          AUPPLM(I)=EXP(AU1 -AL2STR)
        ELSE
          IB=N - IA + 1
          AL2STR=LOG(Y(IA))
          AU2STR=LOG(Y(IB))
          AL1=LOG(C*YAAD) - AK*Z*SE2
          AU1=LOG(C*YAAD) + AK*Z*SE2
!
!         COMPUTE BOTH UPPER AND LOWER LIMIT.  LET CALLING ROUTINE
!         DETERMINE WHICH TO USE.
!
          ALOWLM(I)=EXP(AL1-AU2STR)
          AUPPLM(I)=EXP(AU1 -AL2STR)
        ENDIF
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CDC3')THEN
          WRITE(ICOUT,311)I,IA,IB,Z,AL2STR,AU2STR
  311     FORMAT('I,IA,IB,Z,AL2STR,AU2STR = ',3I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,313)AL1,AU1,ALOWLM(I),AUPPLM(I)
  313     FORMAT('AL1,AU1,ALOWLM(I),AUPPLM(I) = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
  300 CONTINUE
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9051)
 9051   FORMAT('**** AT THE END OF DPCDC3--')
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCDC3
      SUBROUTINE DPCDF1(Y,Y2,N,ICASPL,IFLAGD,   &
                        SHAPE1,SHAPE2,SHAPE3,SHAPE4,   &
                        SHAPE5,SHAPE6,SHAPE7,   &
                        YLOWLM,YUPPLM,A,B,MINMAX,   &
                        ICAPSW,ICAPTY,   &
                        IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                        ILGADF,ISKNDF,IGLDDF,IBGEDF,   &
                        IGETDF,ICONDF,IGOMDF,IKATDF,   &
                        IGIGDF,IGEODF,   &
                        KSLOC,KSSCAL,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--COMPUTE THE CDF VALUE AT GIVEN SET OF POINTS.  THIS
!              WILL BE USED BY VARIOUS K-S AND ANDERSON DARLING
!              ROUTINES.  THIS ROUTINE SIMPLY RETURNS THE ARRAY
!              OF COMPUTED CDF VALUES.  THE CALLING ROUTINE IS
!              RESPONSIBLE FOR CONVERTING THAT INTO A K-S,
!              ANDERSON-DARLING, OR SOME OTHER RELEVANT STATISTIC.
!
!              THIS ROUTINE HANDLES THE UNGROUPED, UNCENSORED CASE.
!              IF IFLAGD = 1, THEN DISCRETE DISTRIBUTIONS WILL
!              BE SKIPPED.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --JULY      2010. END EFFECTS WEIBULL
!     UPDATED         --AUGUST    2010. BRITTLE FIBER WEIBULL
!     UPDATED         --MARCH     2013. COSINE
!     UPDATED         --MAY       2014. 3-PARAMETER INVERSE GAUSSIAN
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      LOGICAL HYPPNT
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IADEDF
      CHARACTER*4 IGEPDF
      CHARACTER*4 IMAKDF
      CHARACTER*4 IBEIDF
      CHARACTER*4 ILGADF
      CHARACTER*4 ISKNDF
      CHARACTER*4 IGLDDF
      CHARACTER*4 IBGEDF
      CHARACTER*4 IGETDF
      CHARACTER*4 ICONDF
      CHARACTER*4 IGOMDF
      CHARACTER*4 IKATDF
      CHARACTER*4 IGIGDF
      CHARACTER*4 IGEODF
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      REAL KSLOC
      REAL KSSCAL
!
      DOUBLE PRECISION DXOUT
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION CDFGLO
      DOUBLE PRECISION CDFWAK
      DOUBLE PRECISION LANCDF
      DOUBLE PRECISION XPAR(5)
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION Y2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      ISUBN1='DPCD'
      ISUBN2='F1  '
      IERROR='NO'
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!CCCC 2013/07: ALLOW ONE VALUE (FOR CALL FROM DPBEF2).
!
!CCCC IF(N.LT.2)THEN
      NMIN=1
      IF(N.LT.NMIN)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN DPCDF1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)NMIN
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST ',   &
               I1,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N.GT.1)THEN
        HOLD=Y(1)
        DO 60 I=1,N
          IF(Y(I).NE.HOLD)GO TO 69
   60   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)
   62   FORMAT('      ALL ELEMENTS IN THE RESPONSE VARIABLE ARE ',   &
               'IDENTICALLY EQUAL TO ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
   69   CONTINUE
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CDF1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPCDF1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ICASPL,ICAPSW,ICAPTY,N,MINMAX
   72   FORMAT('ICASPL,ICAPSW,ICAPTY,N,MINMAX = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)KSLOC,KSSCAL,SHAPE1,SHAPE2
   74   FORMAT('KSLOC,KSSCAL,SHAPE1,SHAPE2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 85 I=1,N
          WRITE(ICOUT,86)I,Y(I)
   86     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   85   CONTINUE
      ENDIF
!
!               ************************************************
!               **  STEP 2.1--                                **
!               **  COMPUTE CDF VALUE AT GIVEN POINTS         **
!               ************************************************
!
      ZSCALE=B - A
      ZLOC=A
      IWRITE='OFF'
      CALL MINIM(Y,N,IWRITE,XMIN,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,XMAX,IBUGA3,IERROR)
!
      IF(ICASPL.EQ.'UNIF')THEN
        DO 1010 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL UNICDF(XL,Y2(I))
 1010   CONTINUE
!
      ELSEIF(ICASPL.EQ.'NORM')THEN
        DO 1020 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL NODCDF(DBLE(XL),DXOUT)
          Y2(I)=REAL(DXOUT)
 1020   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LOGI')THEN
        DO 1030 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LOGCDF(XL,Y2(I))
 1030   CONTINUE
!
      ELSEIF(ICASPL.EQ.'DEXP')THEN
        DO 1040 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DEXCDF(XL,Y2(I))
 1040   CONTINUE
!
      ELSEIF(ICASPL.EQ.'CAUC')THEN
        DO 1050 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL CAUCDF(XL,Y2(I))
 1050   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TULA')THEN
        DO 1060 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LAMCDF(XL,SHAPE1,Y2(I))
 1060   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LOGN' .OR. ICASPL.EQ.'3LGN')THEN
        DO 1070 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LGNCDF(XL,SHAPE1,Y2(I))
 1070   CONTINUE
!
      ELSEIF(ICASPL.EQ.'HNOR' .OR. ICASPL.EQ.'1HNO')THEN
        DO 1080 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HFNCDF(XL,Y2(I))
 1080   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TPP')THEN
        DO 1090 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TCDF(XL,SHAPE1,Y2(I))
 1090   CONTINUE
!
      ELSEIF(ICASPL.EQ.'CHIS')THEN
        DO 1100 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL CHSCDF(XL,INT(SHAPE1+0.1),Y2(I))
 1100   CONTINUE
!
      ELSEIF(ICASPL.EQ.'FPP')THEN
        DO 1110 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL FCDF(XL,INT(SHAPE1+0.1),INT(SHAPE2+0.1),Y2(I))
 1110   CONTINUE
!
      ELSEIF(ICASPL.EQ.'EXPO')THEN
        DO 1120 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL EXPCDF(XL,Y2(I))
 1120   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GAMM' .OR. ICASPL.EQ.'3GAM')THEN
        DO 1130 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GAMCDF(XL,SHAPE1,Y2(I))
 1130   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BETA' .OR. ICASPL.EQ.'4BET')THEN
        DO 1140 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL BETCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1140   CONTINUE
!
      ELSEIF(ICASPL.EQ.'WEIB' .OR. ICASPL.EQ.'3WEI')THEN
        DO 1150 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL WEICDF(XL,SHAPE1,MINMAX,Y2(I))
 1150   CONTINUE
!
      ELSEIF(ICASPL.EQ.'EV1 ')THEN
        DO 1160 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL EV1CDF(XL,MINMAX,Y2(I))
 1160   CONTINUE
!
      ELSEIF(ICASPL.EQ.'EV2 ' .OR. ICASPL.EQ.'3EV2')THEN
        DO 1170 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL EV2CDF(XL,SHAPE1,MINMAX,Y2(I))
 1170   CONTINUE
!
      ELSEIF(ICASPL.EQ.'PARE')THEN
        ZLOC=SHAPE2
        IF(ZLOC.GT.XMIN)ZLOC=XMIN
        DO 1180 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PARCDF(XL,SHAPE1,ZLOC,Y2(I))
 1180   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BINO')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1190 I=1,N
          XL=Y(I)
          CALL BINCDF(DBLE(XL),DBLE(SHAPE1),INT(SHAPE2+0.1),DXOUT)
          Y2(I)=REAL(DXOUT)
 1190   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GEOM')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        IF(IGEODF.EQ.'DLMF')THEN
          DO 1200 I=1,N
            XL=Y(I)
            CALL GE2CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
            Y2(I)=REAL(DXOUT)
 1200     CONTINUE
        ELSE
          DO 1205 I=1,N
            XL=Y(I)
            CALL GEOCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
            Y2(I)=REAL(DXOUT)
 1205     CONTINUE
        ENDIF
!
      ELSEIF(ICASPL.EQ.'POIS')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1210 I=1,N
          XL=Y(I)
          CALL POICDF(XL,SHAPE1,Y2(I))
 1210   CONTINUE
!
      ELSEIF(ICASPL.EQ.'NEBI')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1220 I=1,N
          XL=Y(I)
          CALL NBCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 1220   CONTINUE
!
      ELSEIF(ICASPL.EQ.'SEMI')THEN
        DO 1230 I=1,N
          XL=Y(I) - KSLOC
          CALL SEMCDF(XL,KSSCAL,Y2(I))
 1230   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TRIA')THEN
        IF(A.EQ.CPUMIN .OR. B.EQ.CPUMAX)THEN
          ZLOWLM=-1.0
          ZUPPLM=1.0
        ELSE
          ZLOWLM=MIN(A,B)
          ZUPPLM=MAX(A,B)
        ENDIF
        IF(ZLOWLM.GT.XMIN)ZLOWLM=XMIN
        IF(ZUPPLM.LT.XMAX)ZUPPLM=XMAX
        IF(SHAPE1.LT.ZLOWLM .OR. SHAPE1.GT.ZUPPLM)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1343)
 1343     FORMAT('       FOR THE TRIANGULAR DISTRIBUTION, THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1344)
 1344     FORMAT('       OF THE SHAPE PARAMETER IS OUTSIDE THE ',   &
                 'INTERVAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1345)
 1345     FORMAT('       OF THE LOWER AND UPPER LIMIT PARAMETERS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1346)SHAPE1
 1346     FORMAT('       THE VALUE OF THE SHAPE PARAMETER       = ',   &
               G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1347)ZLOWLM
 1347     FORMAT('       THE VALUE OF THE LOWER LIMIT PARAMETER = ',   &
                 G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1348)ZUPPLM
 1348     FORMAT('       THE VALUE OF THE LOWER LIMIT PARAMETER = ',   &
                 G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DO 1240 I=1,N
          XL=Y(I)
          CALL TRICDF(XL,SHAPE1,ZLOWLM,ZUPPLM,Y2(I))
 1240   CONTINUE
!
      ELSEIF(ICASPL.EQ.'INGA' .OR. ICASPL.EQ.'3IGA')THEN
        DO 1250 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IGCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DCDF)
          Y2(I)=REAL(DCDF)
 1250   CONTINUE
!
      ELSEIF(ICASPL.EQ.'WALD')THEN
        AMU=1.0
        DO 1260 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IGCDF(DBLE(XL),DBLE(SHAPE1),DBLE(AMU),DCDF)
          Y2(I)=REAL(DCDF)
 1260   CONTINUE
!
      ELSEIF(ICASPL.EQ.'RIGA')THEN
        DO 1270 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL RIGCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DCDF)
          Y2(I)=REAL(DCDF)
 1270   CONTINUE
!
      ELSEIF(ICASPL.EQ.'FATL')THEN
        DO 1280 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL FLCDF(XL,SHAPE1,Y2(I))
 1280   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GPAR')THEN
        DO 1290 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GEPCDF(XL,SHAPE1,MINMAX,IGEPDF,Y2(I))
 1290   CONTINUE
!
      ELSEIF(ICASPL.EQ.'DUNI')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1300 I=1,N
          XL=Y(I)
          IXL=INT(XL+0.1)
          CALL DISCDF(IXL,INT(SHAPE1+0.1),Y2(I))
 1300   CONTINUE
!
      ELSEIF(ICASPL.EQ.'NCT ')THEN
        DO 1310 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL NCTCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1310   CONTINUE
!
      ELSEIF(ICASPL.EQ.'NCF ')THEN
        DO 1320 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL NCFCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1320   CONTINUE
!
      ELSEIF(ICASPL.EQ.'NCCS')THEN
        DO 1330 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL NCCCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1330   CONTINUE
!
      ELSEIF(ICASPL.EQ.'NCBE')THEN
        DO 1340 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL NCBCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1340   CONTINUE
!
      ELSEIF(ICASPL.EQ.'DNCT')THEN
        DO 1350 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DNTCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1350   CONTINUE
!
      ELSEIF(ICASPL.EQ.'DNCF')THEN
        DO 1360 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DNFCDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,Y2(I))
 1360   CONTINUE
!
      ELSEIF(ICASPL.EQ.'HYPG')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        HYPPNT=.FALSE.
        DO 1365 I=1,N
          XL=Y(I)
          CALL HYPCDF(INT(XL+0.1),INT(SHAPE1+0.1),INT(SHAPE2+0.1),   &
                      INT(SHAPE3+0.1),HYPPNT,Y2(I))
 1365   CONTINUE
!
      ELSEIF(ICASPL.EQ.'VONM')THEN
        DO 1370 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL VONCDF(XL,SHAPE1,Y2(I))
 1370   CONTINUE
!
      ELSEIF(ICASPL.EQ.'POWN')THEN
        DO 1380 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PNRCDF(XL,SHAPE1,Y2(I))
 1380   CONTINUE
!
      ELSEIF(ICASPL.EQ.'PLGN')THEN
        DO 1390 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PLNCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1390   CONTINUE
!
      ELSEIF(ICASPL.EQ.'ALPH')THEN
        DO 1400 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ALPCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 1400   CONTINUE
!
      ELSEIF(ICASPL.EQ.'COSI')THEN
        DO 1410 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL COSCDF(XL,Y2(I))
 1410   CONTINUE
!
      ELSEIF(ICASPL.EQ.'SINE')THEN
        DO 1415 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL SINCDF(XL,Y2(I))
 1415   CONTINUE
!
      ELSEIF(ICASPL.EQ.'POWF')THEN
        DO 1420 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL POWCDF(XL,SHAPE1,Y2(I))
 1420   CONTINUE
!
      ELSEIF(ICASPL.EQ.'CHI ')THEN
        DO 1430 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL CHCDF(XL,SHAPE1,Y2(I))
 1430   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LOGS')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1435 I=1,N
          XL=Y(I)
          CALL DLGCDF(XL,SHAPE1,Y2(I))
 1435   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LOGL')THEN
        DO 1440 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LLGCDF(XL,SHAPE1,Y2(I))
 1440   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GGAM')THEN
        DO 1450 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GGDCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1450   CONTINUE
!
      ELSEIF(ICASPL.EQ.'WARI')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1460 I=1,N
          XL=Y(I)
!CCCC     CALL WARCDF(XL,SHAPE1,SHAPE2,Y2(I),'NOTR')
          CALL WARCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1460   CONTINUE
!
      ELSEIF(ICASPL.EQ.'YULE')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1470 I=1,N
          XL=Y(I)
          CALL YULCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 1470   CONTINUE
!
      ELSEIF(ICASPL.EQ.'ANGL')THEN
        DO 1480 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ANGCDF(XL,Y2(I))
 1480   CONTINUE
!
      ELSEIF(ICASPL.EQ.'ARSI')THEN
        DO 1490 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ARSCDF(XL,Y2(I))
 1490   CONTINUE
!
      ELSEIF(ICASPL.EQ.'FNOR')THEN
!
!       FOR FOLDED NORMAL, ARE PARAMETERS GIVEN AS LOCATION/SCALE
!       OR SHAPE1 AND SHAPE2?
!
        IF(SHAPE1.NE.CPUMIN .AND. SHAPE2.NE.CPUMIN)THEN
          AVAL1=SHAPE1
          AVAL2=SHAPE2
        ELSEIF(KSLOC.NE.CPUMIN .AND. KSSCAL.NE.CPUMIN)THEN
          AVAL1=KSLOC
          AVAL2=KSSCAL
        ELSE
          AVAL1=0.0
          AVAL2=1.0
        ENDIF
!
        DO 1500 I=1,N
!CCCC     XL=(Y(I) - KSLOC)/KSSCAL
!CCCC     CALL FNRCDF(XL,KSLOC,KSSCAL,Y2(I))
          XL=Y(I)
          CALL FNRCDF(XL,AVAL1,AVAL2,Y2(I))
 1500   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TNOR')THEN
        DO 1510 I=1,N
          XL=Y(I)
          CALL TNRCDF(DBLE(XL),DBLE(A),DBLE(B),   &
                      DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 1510   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LGAM')THEN
        DO 1520 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LGACDF(XL,SHAPE1,ILGADF,Y2(I))
 1520   CONTINUE
!
      ELSEIF(ICASPL.EQ.'HSEC')THEN
        DO 1530 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HSECDF(XL,Y2(I))
 1530   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GOMP')THEN
        DO 1540 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GOMCDF(XL,SHAPE1,SHAPE2,IGOMDF,Y2(I))
 1540   CONTINUE
!
      ELSEIF(ICASPL.EQ.'HCAU')THEN
        DO 1550 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HFCCDF(XL,Y2(I))
 1550   CONTINUE
!
      ELSEIF(ICASPL.EQ.'HALO')THEN
        SHAPE1=-1.0
        DO 1560 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HFLCDF(XL,SHAPE1,Y2(I))
 1560   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GHLO')THEN
        DO 1570 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HFLCDF(XL,SHAPE1,Y2(I))
 1570   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GEV ')THEN
        DO 1580 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GEVCDF(XL,SHAPE1,MINMAX,Y2(I))
 1580   CONTINUE
!
      ELSEIF(ICASPL.EQ.'PAR2')THEN
        ZLOC=SHAPE2
        IF(ZLOC.GT.XMIN)ZLOC=XMIN
        DO 1590 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PA2CDF(XL,SHAPE1,ZLOC,Y2(I))
 1590   CONTINUE
!
      ELSEIF(ICASPL.EQ.'DWEI')THEN
        DO 1600 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DWECDF(XL,SHAPE1,Y2(I))
 1600   CONTINUE
!
      ELSEIF(ICASPL.EQ.'WCAU')THEN
        DO 1610 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL WCACDF(XL,SHAPE1,Y2(I))
 1610   CONTINUE
!
      ELSEIF(ICASPL.EQ.'EWEI')THEN
        IARG1=1
        DO 1620 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL EWECDF(XL,SHAPE1,SHAPE2,IARG1,Y2(I))
 1620   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TEXP')THEN
        DO 1630 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TNECDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1630   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GLOG')THEN
        DO 1640 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GLOCDF(XL,SHAPE1,Y2(I))
 1640   CONTINUE
!
      ELSEIF(ICASPL.EQ.'PEXP')THEN
        DO 1650 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PEXCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 1650   CONTINUE
!
      ELSEIF(ICASPL.EQ.'DGAM')THEN
        DO 1660 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DGACDF(XL,SHAPE1,Y2(I))
 1660   CONTINUE
!
      ELSEIF(ICASPL.EQ.'MBKA')THEN
        DO 1670 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL MIECDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 1670   CONTINUE
!
      ELSEIF(ICASPL.EQ.'FCAU')THEN
        DO 1680 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL FCACDF(XL,SHAPE1,SHAPE2,Y2(I))
 1680   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BBIN')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1690 I=1,N
          XL=Y(I)
          CALL BBNCDF(XL,SHAPE1,SHAPE2,INT(SHAPE3+0.1),Y2(I))
 1690   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BRAD')THEN
        DO 1700 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BRACDF(XL,SHAPE1,Y2(I))
 1700   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GEXP')THEN
        DO 1710 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GEXCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1710   CONTINUE
!
      ELSEIF(ICASPL.EQ.'RECI')THEN
        DO 1715 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL RECCDF(XL,SHAPE1,Y2(I))
 1715   CONTINUE
!
      ELSEIF(ICASPL.EQ.'NORX')THEN
        DO 1720 I=1,N
          XL=Y(I)
          CALL NMXCDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,   &
                      Y2(I))
 1720   CONTINUE
!
      ELSEIF(ICASPL.EQ.'IGAM')THEN
        DO 1730 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IGACDF(XL,SHAPE1,Y2(I))
 1730   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GTLA')THEN
        DO 1740 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GLDCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT,   &
                      IGLDDF,IWRITE)
          Y2(I)=REAL(DXOUT)
 1740   CONTINUE
!
      ELSEIF(ICASPL.EQ.'JOSB')THEN
        DO 1750 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL JSBCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1750   CONTINUE
!
      ELSEIF(ICASPL.EQ.'JOSU')THEN
        DO 1760 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL JSUCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1760   CONTINUE
!
      ELSEIF(ICASPL.EQ.'IWEI')THEN
        DO 1770 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IWECDF(XL,SHAPE1,Y2(I))
 1770   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LDEX')THEN
        DO 1780 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LDECDF(XL,SHAPE1,Y2(I))
 1780   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GEEX')THEN
        DO 1790 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GEECDF(XL,SHAPE1,Y2(I))
 1790   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TSPO')THEN
        IF(A.EQ.CPUMIN .OR. B.EQ.CPUMAX)THEN
          ZLOWLM=0.0
          ZUPPLM=1.0
        ELSE
          ZLOWLM=MIN(A,B)
          ZUPPLM=MAX(A,B)
        ENDIF
        IF(ZLOWLM.GT.XMIN)ZLOWLM=XMIN
        IF(ZUPPLM.LT.XMAX)ZUPPLM=XMAX
        IF(SHAPE1.LT.ZLOWLM .OR. SHAPE1.GT.ZUPPLM)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1943)
 1943     FORMAT('       FOR THE TWO-SIDED POWER DISTRIBUTION, THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1944)
 1944     FORMAT('       VALUE OF THE THETA SHAPE PARAMETER IS ',   &
                 'OUTSIDE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1945)
 1945     FORMAT('       INTERVAL OF THE LOWER AND UPPER LIMIT ',   &
                 'PARAMETERS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1946)SHAPE1
 1946     FORMAT('       THE VALUE OF THE THETA SHAPE PARAMETER = ',   &
                 G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1947)ZLOWLM
 1947     FORMAT('       THE VALUE OF THE LOWER LIMIT PARAMETER = ',   &
                 G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1948)ZUPPLM
 1948     FORMAT('       THE VALUE OF THE LOWER LIMIT PARAMETER = ',   &
                 G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DO 1800 I=1,N
          XL=Y(I)
          CALL TSPCDF(XL,SHAPE1,SHAPE2,A,B,Y2(I))
 1800   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BWEI')THEN
        DO 1810 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BWECDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,   &
                      Y2(I),DXOUT)
 1810   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GHPP')THEN
        DO 1820 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GHCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1820   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GPP')THEN
        HTEMP=0.0
        DO 1821 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GHCDF(XL,SHAPE1,HTEMP,Y2(I))
 1821   CONTINUE
!
      ELSEIF(ICASPL.EQ.'HPP')THEN
        GTEMP=0.0
        DO 1823 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GHCDF(XL,GTEMP,SHAPE1,Y2(I))
 1823   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LAND')THEN
        DO 1830 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          DXOUT=LANCDF(DBLE(XL))
          Y2(I)=REAL(DXOUT)
 1830   CONTINUE
!
      ELSEIF(ICASPL.EQ.'ERRO')THEN
        DO 1840 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ERRCDF(XL,SHAPE1,Y2(I))
 1840   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TRAP')THEN
        DO 1850 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TRACDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,Y2(I))
 1850   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GTRA')THEN
        DO 1860 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GTRCDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,   &
                      SHAPE6,SHAPE7,Y2(I))
 1860   CONTINUE
!
      ELSEIF(ICASPL.EQ.'FT  ')THEN
        DO 1870 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL FTCDF(XL,INT(SHAPE1+0.1),Y2(I))
 1870   CONTINUE
!
      ELSEIF(ICASPL.EQ.'SLAS')THEN
        DO 1880 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL SLACDF(XL,Y2(I))
 1880   CONTINUE
!
      ELSEIF(ICASPL.EQ.'SNOR')THEN
        DO 1890 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL SNCDF(XL,SHAPE1,ISKNDF,Y2(I))
 1890   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TSKE')THEN
        DO 1900 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL STCDF(XL,INT(SHAPE1+0.1),SHAPE2,Y2(I))
 1900   CONTINUE
!
      ELSEIF(ICASPL.EQ.'IBET')THEN
        DO 1910 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IBCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1910   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GOMM')THEN
        IF(IMAKDF.EQ.'DLMF')THEN
          DO 1930 I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL MAKCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1930     CONTINUE
        ELSEIF(IMAKDF.EQ.'MEEK')THEN
          XI=SHAPE1/SHAPE3
          THETA=SHAPE2/SHAPE1
          ALAMB=SHAPE3
          DO 1935 I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL MAKCDF(XL,XI,ALAMBA,THETA,Y2(I))
 1935     CONTINUE
        ELSEIF(IMAKDF.EQ.'REPA')THEN
          DO 1938 I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL MA2CDF(XL,SHAPE1,SHAPE2,Y2(I))
 1938     CONTINUE
        ENDIF
!
      ELSEIF(ICASPL.EQ.'LSNO')THEN
        DO 1940 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LSNCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1940   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LSKT')THEN
        DO 1950 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LSTCDF(XL,INT(SHAPE1+0.1),SHAPE2,SHAPE3,Y2(I))
 1950   CONTINUE
!
      ELSEIF(ICASPL.EQ.'POLY')THEN
        DO 1960 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL POLCDF(XL,SHAPE1,SHAPE2,INT(SHAPE3+0.1),Y2(I))
 1960   CONTINUE
!
      ELSEIF(ICASPL.EQ.'HERM')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 1970 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HERCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1970   CONTINUE
!
      ELSEIF(ICASPL.EQ.'SDEX')THEN
        DO 1980 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL SDECDF(XL,SHAPE1,Y2(I))
 1980   CONTINUE
!
      ELSEIF(ICASPL.EQ.'ADEX')THEN
        DO 1990 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ADECDF(XL,SHAPE1,IADEDF,Y2(I))
 1990   CONTINUE
!
      ELSEIF(ICASPL.EQ.'MAXW' .OR. ICASPL.EQ.'1MAX')THEN
        AVAL1=KSLOC
        IF(ICASPL.EQ.'1MAX')AVAL1=0.0
        DO 2000 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL MAXCDF(XL,Y2(I))
 2000   CONTINUE
!
      ELSEIF(ICASPL.EQ.'RAYL')THEN
        DO 2010 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL RAYCDF(XL,Y2(I))
 2010   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GIGA')THEN
        IF(IGIGDF.EQ.'2PAR')THEN
          DO 2020 I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL GI2CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
            Y2(I)=REAL(DXOUT)
 2020     CONTINUE
        ELSE
          DO 2030 I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL GIGCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                        DBLE(SHAPE3),DXOUT)
            Y2(I)=REAL(DXOUT)
 2030     CONTINUE
        ENDIF
!
      ELSEIF(ICASPL.EQ.'GALP')THEN
        DO 2040 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GALCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),IADEDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2040   CONTINUE
!
      ELSEIF(ICASPL.EQ.'MCLE')THEN
        DO 2050 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL MCLCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2050   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BEIP')THEN
        DO 2060 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BEICDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DBLE(SHAPE3),   &
                      IBEIDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2060   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BEIK')THEN
        DO 2070 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
!CCCC     CALL BEKCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DBLE(SHAPE3),
!CCCC1                IBEIDF,DXOUT)
!CCCC     Y2(I)=REAL(DXOUT)
          Y2(I)=0.0
 2070   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GMCL')THEN
        DO 2080 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GMCCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2080   CONTINUE
!
      ELSEIF(ICASPL.EQ.'G5LO')THEN
        XPAR(1)=DBLE(KSLOC)
        XPAR(2)=DBLE(KSSCAL)
        XPAR(3)=DBLE(SHAPE1)
        DO 2090 I=1,N
          XL=Y(I)
          DXOUT=CDFGLO(DBLE(XL),XPAR)
          Y2(I)=REAL(DXOUT)
 2090   CONTINUE
!
      ELSEIF(ICASPL.EQ.'WAKE')THEN
        XPAR(1)=DBLE(KSLOC)
        XPAR(2)=DBLE(KSSCAL)
        XPAR(3)=DBLE(SHAPE1)
        XPAR(4)=DBLE(SHAPE2)
        XPAR(5)=DBLE(SHAPE3)
        DO 2100 I=1,N
          XL=Y(I)
          DXOUT=CDFWAK(DBLE(XL),XPAR)
          Y2(I)=REAL(DXOUT)
 2100   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BNOR')THEN
        DO 2110 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BNOCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2110   CONTINUE
!
      ELSEIF(ICASPL.EQ.'G2LO')THEN
        DO 2120 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GL2CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2120   CONTINUE
!
      ELSEIF(ICASPL.EQ.'G3LO')THEN
        DO 2130 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GL3CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2130   CONTINUE
!
      ELSEIF(ICASPL.EQ.'G4LO')THEN
        DO 2140 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GL4CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2140   CONTINUE
!
      ELSEIF(ICASPL.EQ.'ALDE')THEN
        DO 2150 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ALDCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2150   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BGEO')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        IF(IBGEDF.EQ.'UNSH')THEN
          DO 2160 I=1,N
            XL=Y(I)
            CALL BGECDF(XL,SHAPE1,SHAPE2,Y2(I))
 2160     CONTINUE
        ELSE
          DO 2165 I=1,N
            XL=Y(I)
            CALL BG2CDF(XL,SHAPE1,SHAPE2,Y2(I))
 2165     CONTINUE
        ENDIF
!
      ELSEIF(ICASPL.EQ.'ZETA')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2170 I=1,N
          XL=Y(I)
          CALL ZETCDF(XL,SHAPE1,Y2(I))
 2170   CONTINUE
!
      ELSEIF(ICASPL.EQ.'ZIPF')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2180 I=1,N
          XL=Y(I)
          CALL ZIPCDF(XL,SHAPE1,INT(SHAPE2+0.1),Y2(I))
 2180   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BTAN')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2190 I=1,N
          XL=Y(I)
          CALL BTACDF(XL,SHAPE1,SHAPE2,Y2(I))
 2190   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BNBI')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2200 I=1,N
          XL=Y(I)
          CALL GWACDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2200   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LPOI')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2210 I=1,N
          XL=Y(I)
          CALL LPOCDF(XL,SHAPE1,SHAPE2,Y2(I))
 2210   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LICT')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2220 I=1,N
          XL=Y(I)
          CALL LCTCDF(XL,INT(SHAPE1+0.1),Y2(I))
 2220   CONTINUE
!
      ELSEIF(ICASPL.EQ.'MATC')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2230 I=1,N
          XL=Y(I)
          CALL MATCDF(XL,INT(SHAPE1+0.1),Y2(I))
 2230   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LBET')THEN
        YLOWLM=SHAPE3
        YUPPLM=SHAPE4
        EPS=(XMAX-XMIN)*0.01
        IF(YLOWLM.GT.XMIN)YLOWLM=XMIN-EPS
        IF(YUPPLM.LT.XMAX)YUPPLM=XMAX+EPS
        DO 2240 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LBECDF(XL,SHAPE1,SHAPE2,YLOWLM,YUPPLM,Y2(I))
 2240   CONTINUE
!
      ELSEIF(ICASPL.EQ.'AEPP')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2250 I=1,N
          XL=Y(I)
          CALL PAPCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2250   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GLOS')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2270 I=1,N
          XL=Y(I)
          CALL GLSCDF(XL,SHAPE1,SHAPE2,Y2(I))
 2270   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GNBI')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2280 I=1,N
          XL=Y(I)
          CALL GNBCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 2280   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GEET')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2290 I=1,N
          XL=Y(I)
          CALL GETCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      IGETDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2290   CONTINUE
!
      ELSEIF(ICASPL.EQ.'QBIN')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2300 I=1,N
          XL=Y(I)
          CALL QBICDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 2300   CONTINUE
!
      ELSEIF(ICASPL.EQ.'CONS')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2310 I=1,N
          XL=Y(I)
          CALL CONCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      ICONDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2310   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LKAT')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2320 I=1,N
          XL=Y(I)
          CALL LKCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2320   CONTINUE
!
      ELSEIF(ICASPL.EQ.'KATZ')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2330 I=1,N
          XL=Y(I)
          CALL KATCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),IKATDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2330   CONTINUE
!
      ELSEIF(ICASPL.EQ.'DISW')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2340 I=1,N
          XL=Y(I)
          CALL DIWCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2340   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GLGP')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2350 I=1,N
          XL=Y(I)
          CALL GLGCDF(XL,SHAPE1,INT(SHAPE2+0.1),SHAPE3,Y2(I))
 2350   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TGNB')THEN
        IF(IFLAGD.EQ.1)GO TO 8000
        DO 2360 I=1,N
          XL=Y(I)
          CALL GNTCDF(XL,SHAPE1,SHAPE2,SHAPE3,INT(SHAPE4+0.1),Y2(I))
 2360   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TOPL')THEN
        DO 2370 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TOPCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2370   CONTINUE
!
      ELSEIF(ICASPL.EQ.'GTOL')THEN
        DO 2380 I=1,N
          XL=Y(I)
          CALL GTLCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(A),DBLE(B),DXOUT)
          Y2(I)=REAL(DXOUT)
 2380   CONTINUE
!
      ELSEIF(ICASPL.EQ.'RGTL')THEN
        DO 2390 I=1,N
          XL=Y(I)
          CALL RGTCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(A),DBLE(B),DXOUT)
          Y2(I)=REAL(DXOUT)
 2390   CONTINUE
!
      ELSEIF(ICASPL.EQ.'SLOP')THEN
        DO 2400 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL SLOCDF(XL,SHAPE1,Y2(I))
 2400   CONTINUE
!
      ELSEIF(ICASPL.EQ.'OGIV')THEN
        DO 2410 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL OGICDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2410   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TSSL')THEN
        DO 2420 I=1,N
          XL=Y(I)
          CALL TSSCDF(XL,SHAPE1,SHAPE2,   &
                      A,B,Y2(I))
 2420   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TSOG')THEN
        DO 2430 I=1,N
          XL=Y(I)
          CALL TSOCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(A),DBLE(B),DXOUT)
          Y2(I)=REAL(DXOUT)
 2430   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BUR2')THEN
        DO 2450 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU2CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2450   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BUR3')THEN
        DO 2460 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU3CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2460   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BUR4')THEN
        DO 2470 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU4CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2470   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BUR5')THEN
        DO 2480 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU5CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2480   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BUR6')THEN
        DO 2490 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU6CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2490   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BUR7')THEN
        DO 2500 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU7CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2500   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BUR8')THEN
        DO 2510 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU8CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2510   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BUR9')THEN
        DO 2520 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU9CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2520   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BU10' .OR. ICASPL.EQ.'B10' .OR.   &
             ICASPL.EQ.'3B10')THEN
        DO 2530 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL B10CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2530   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BU11')THEN
        DO 2540 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL B11CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2540   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BU12')THEN
        DO 2550 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL B12CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2550   CONTINUE
!
      ELSEIF(ICASPL.EQ.'DPUN')THEN
        DO 2560 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DPUCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(SHAPE3),DBLE(SHAPE4),DXOUT)
          Y2(I)=REAL(DXOUT)
 2560   CONTINUE
!
      ELSEIF(ICASPL.EQ.'KUMA')THEN
        DO 2570 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL KUMCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2570   CONTINUE
!
      ELSEIF(ICASPL.EQ.'RPOW')THEN
        DO 2580 I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL RPOCDF(XL,SHAPE1,Y2(I))
 2580   CONTINUE
!
      ELSEIF(ICASPL.EQ.'UTSP')THEN
        DO 2590 I=1,N
          XL=Y(I)
          CALL UTSCDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,   &
                      SHAPE6,Y2(I))
 2590   CONTINUE
!
      ELSEIF(ICASPL.EQ.'MUTH')THEN
        DO 2600 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL MUTCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2600   CONTINUE
!
      ELSEIF(ICASPL.EQ.'LEXP')THEN
        DO 2610 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LEXCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2610   CONTINUE
!
      ELSEIF(ICASPL.EQ.'TPAR')THEN
        DO 2620 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TNPCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2620   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BFRA')THEN
        DO 2630 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BFRCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2630   CONTINUE
!
      ELSEIF(ICASPL.EQ.'L3EX')THEN
        DO 2640 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LE3CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2640   CONTINUE
!
      ELSEIF(ICASPL.EQ.'KAPP')THEN
        DO 2650 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL KAPCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),   &
                      DBLE(KSLOC),DBLE(KSSCAL),DXOUT)
          Y2(I)=REAL(DXOUT)
 2650   CONTINUE
!
      ELSEIF(ICASPL.EQ.'PEA3')THEN
        DO 2660 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PE3CDF(DBLE(XL),DBLE(SHAPE1),   &
                      DBLE(KSLOC),DBLE(KSSCAL),DXOUT)
          Y2(I)=REAL(DXOUT)
 2660   CONTINUE
!
      ELSEIF(ICASPL.EQ.'EEWE')THEN
        DO 2670 I=1,N
          XL=Y(I)
          CALL EEWCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DBLE(SHAPE3),   &
                      DBLE(SHAPE4),DBLE(SHAPE5),DXOUT)
          Y2(I)=REAL(DXOUT)
 2670   CONTINUE
!
      ELSEIF(ICASPL.EQ.'BFWE')THEN
        DO 2680 I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BFWCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2680   CONTINUE
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)ICASPL
 8011   FORMAT('      UNKNOWN DISTRIBUTION -- ',A40)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      GO TO 9000
!
!     SET AN ERROR FLAG TO INDICATE A DISCRETE DISTRIBUTION
!     IS NOT TO BE PROCESSED.
!
 8000 CONTINUE
      IFLAGD=99
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CDF1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCDF1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,N,MINMAX,IERROR
 9012   FORMAT('ICASPL,N,MINMAX,IERROR = ',A4,2X,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,N
          WRITE(ICOUT,9021)I,Y(I),Y2(I)
 9021     FORMAT('I,Y(I),Y2(I), = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPCDF1
      SUBROUTINE DPCHAL(ICHAR2,ICHARN,IBUGXX,IFOUND)
!
!     PURPOSE--CONVERT AN ALPHABETIC CHARACTER
!              (A TO Z) INTO A NUMERIC VALUE
!              (1 TO 26).
!     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
!                              CONTAINING THE HOLLERITH
!                              CHARACTER(S) OF INTEREST.
!     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
!                              CONTAINING THE NUMERIC
!                              DESIGNATION FOR THE
!                              ALPHABETIC CHARACTER.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MARCH     1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUGXX
      CHARACTER*4 IFOUND
!
      CHARACTER*1 ICH1
      CHARACTER*1 ICH2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      ICH1='-'
      ICH2='-'
!
      ICH1N=(-999)
      ICH2N=(-999)
!
      IF(IBUGXX.EQ.'ON' .OR. ISUBG4.EQ.'CHAL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCHAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IBUGXX,IBUGG4,ISUBG4,ICHAR2
   59   FORMAT('IBUGXX,IBUGXX,ISUBG4,ICHAR2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  CONVERT THE CHARACTER       **
!               **********************************
!
      ICH2(1:1)=ICHAR2(2:2)
!CCCC ICH2N=ICHAR(ICH2)
      CALL DPCOAN(ICH2,ICH2N)
      IF(ICH2N.EQ.32)GO TO 1100
      GO TO 7900
!
 1100 CONTINUE
      ICH1(1:1)=ICHAR2(1:1)
!CCCC ICH1N=ICHAR(ICH1)
      CALL DPCOAN(ICH1,ICH1N)
      ICHARN=ICH1N-64
      IF(1.LE.ICHARN.AND.ICHARN.LE.26)GO TO 8000
      GO TO 7900
!
 7900 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7911)
!7911 FORMAT('***** ERROR IN DPCHAL--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7912)
!7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7913)ICHAR
!7913 FORMAT('      INPUT CHARACTER = ',A4)
!CCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GO TO 9000
!
 8000 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGXX.EQ.'ON' .OR. ISUBG4.EQ.'CHAL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICH1,ICH1N,ICH2,ICH2N
 9012   FORMAT('ICH1,ICH1N,ICH2,ICH2N = ',A1,2X,I8,2X,A1,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IFOUND,ICHAR2,ICHARN
 9014   FORMAT('IFOUND,ICHAR2,ICHARN = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHAL
      SUBROUTINE DPCHAN(MAXCHA,ACHAAN,   &
      IBUGP2,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER ANGLES FOR USE IN MULTI-TRACE PLOTS.
!              THE ANGLE FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE FLOATING POINT
!              VECTOR ACHAAN(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --MAXCHA
!     OUTPUT ARGUMENTS--ACHAAN  (A  FLOATING POINT VECTOR
!                       WHOSE I-TH ELEMENT IS THE ANGLE
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --ACHAAN = CHARACTER ANGLE
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/11
!     ORIGINAL VERSION--NOVEMBER  1986.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC CHARACTER*4 IHARG        DECEMBER 1986
!CCCC CHARACTER*4 IARGT        DECEMBER 1986
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEQ
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
!CCCC DIMENSION IHARG(*)       DECEMBER 1986
!CCCC DIMENSION IARGT(*)       DECEMBER 1986
!CCCC DIMENSION IARG(*)        DECEMBER 1986
!CCCC DIMENSION ARG(*)         DECEMBER 1986
!
      DIMENSION ACHAAN(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCH'
      ISUBN2='AN  '
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'ANGL')GO TO 1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'ANGL')GO TO 1105
      GO TO 9000
!
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1110
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
!
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GO TO 1160
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GO TO 1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GO TO 1140
!
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'AUTO')GO TO 3000
!
      GO TO 1150
!
 1110 CONTINUE
      DO 1115 I=1,MAXCHA
      ACHAAN(I)=0.0
 1115 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ACHAAN(I)
 1116 FORMAT('ALL CHARACTER ANGLES HAVE JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 8000
!
 1120 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GO TO 1180
      ACHAAN(1)=ARG(2)
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,ACHAAN(I)
 1126 FORMAT('THE ANGLE FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 8000
!
 1130 CONTINUE
      I=1
      IF(IARGT(3).NE.'NUMB')GO TO 1180
      DO 1135 I=1,MAXCHA
      ACHAAN(I)=ARG(3)
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ACHAAN(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 8000
!
 1140 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GO TO 1180
      DO 1145 I=1,MAXCHA
      ACHAAN(I)=ARG(2)
 1145 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ACHAAN(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 8000
!
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO 1155 I=1,IMAX
      IP1=I+1
      IF(IARGT(IP1).NE.'NUMB')GO TO 1180
      ACHAAN(I)=ARG(IP1)
 1155 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1156 I=1,IMAX
      WRITE(ICOUT,1126)I,ACHAAN(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GO TO 8000
!
 1160 CONTINUE
      DO 1165 I=1,MAXCHA
      ACHAAN(I)=0.0
 1165 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ACHAAN(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GO TO 8000
!
 1180 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DPCHAN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('CHARACTER ANGLES MUST BE NUMERIC;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('HOWEVER, THE SPECIFIED CHARACTER ANGLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)I
 1184 FORMAT('FOR CHARACTER ',I6,' WAS NON-NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               ***********************************************************
!               **  STEP 30--                                            **
!               **  TREAT THE   CHARACTER ANGLE AUTOMATIC <VARIABLE>  CASE **
!               ***********************************************************
!
 3000 CONTINUE
!
!               ********************************************
!               **  STEP 31--                             **
!               **  CHECK THE VALIDITY OF ARGUMENT 3      **
!               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
!               ********************************************
!
      ISTEPN='31'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(3)
      IHLEF2=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
!
!               *****************************************
!               **  STEP 32--                          **
!               **  CHECK TO SEE THE TYPE CASE--       **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='32'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 3290
      DO 3200 J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GO TO 3210
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GO TO 3210
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GO TO 3220
 3200 CONTINUE
      GO TO 3290
 3210 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GO TO 3290
 3220 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 3290
 3290 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 3295
      WRITE(ICOUT,3291)NUMARG,ILOCQ
 3291 FORMAT('NUMARG,ILOCQ = ',2I8)
      CALL DPWRST('XXX','BUG ')
 3295 CONTINUE
!
!               *********************************************
!               **  STEP 33--                              **
!               **  TEMPORARILY FORM THE VARIABLE Y(.)     **
!               **  WHICH WILL HOLD THE RESPONSE VARIABLE. **
!               **  FORM THIS VARIABLE BY                  **
!               **  BRANCHING TO THE APPROPRIATE SUBCASE   **
!               **  (FULL, SUBSET, OR FOR).                **
!               *********************************************
!
      ISTEPN='33'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL')GO TO 3310
      IF(ICASEQ.EQ.'SUBS')GO TO 3320
      IF(ICASEQ.EQ.'FOR')GO TO 3330
!
 3310 CONTINUE
      DO 3315 I=1,NLEFT
      ISUB(I)=1
 3315 CONTINUE
      NQ=NLEFT
      GO TO 3350
!
 3320 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GO TO 3350
!
 3330 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GO TO 3350
!
 3350 CONTINUE
      MINN2=1
      IF(NQ.GE.MINN2)GO TO 3360
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3351)
 3351 FORMAT('***** ERROR IN DPCHAN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3352)
 3352 FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',   &
      'EXTRACTED,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3353)IHLEFT,IHLEF2
 3353 FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING',   &
      'FROM VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3354)
 3354 FORMAT('      (FOR WHICH CHARACTER ANGLES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3355)
 3355 FORMAT('      ARE TO BE GENERATED)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3356)MINN2
 3356 FORMAT('      MUST BE ',I8,' OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3357)
 3357 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3358)
 3358 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3359)(IANS(I),I=1,IWIDTH)
 3359 FORMAT('      ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
 3360 CONTINUE
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      J=0
      IMAX=NLEFT
      IF(NQ.LT.NLEFT)IMAX=NQ
      DO 3370 I=1,IMAX
      IF(ISUB(I).EQ.0)GO TO 3370
      J=J+1
!
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)Y(J)=V(IJ)
      IF(ICOLL.EQ.MAXCP1)Y(J)=PRED(I)
      IF(ICOLL.EQ.MAXCP2)Y(J)=RES(I)
      IF(ICOLL.EQ.MAXCP3)Y(J)=YPLOT(I)
      IF(ICOLL.EQ.MAXCP4)Y(J)=XPLOT(I)
      IF(ICOLL.EQ.MAXCP5)Y(J)=X2PLOT(I)
      IF(ICOLL.EQ.MAXCP6)Y(J)=TAGPLO(I)
!
 3370 CONTINUE
      NS=J
      NY=J
!
!               *****************************************
!               **  STEP 34--                          **
!               **  EXTRACT THE DISTINCT VALUES        **
!               **  FROM THE TARGET VARIABLE Y(.)   .  **
!               **  STORE THEM IN X(.)   .             **
!               *****************************************
!
      IWRITE='OFF'
      CALL DISTIN(Y,NY,IWRITE,X,NX,IBUGP2,IERROR)
!
!               ***********************************
!               **  STEP 35--                    **
!               **  SORT THESE DISTINCT VALUES   **
!               **  (IN PLACE).                  **
!               ***********************************
!
      CALL SORT(X,NX,X)
!
!               ******************************************
!               **  STEP 36--                           **
!               **  COPY    THE NUMERIC VALUES IN X(.)  **
!               **  INTO INDIVIDUAL ELEMENTS            **
!               **  OF ACHAAN(.)                        **
!               **  NOTE--MAX NUMBER OF VALUES  = 100   **
!               ******************************************
!
      IMAX=NX
      IF(IMAX.GT.MAXCHA)IMAX=MAXCHA
      DO 3650 I=1,IMAX
      ACHAAN(I)=X(I)
 3650 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 3679
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 3675 I=1,IMAX
      WRITE(ICOUT,3676)I,ACHAAN(I)
 3676 FORMAT('CHARACTER ANGLE ',I6,' HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 3675 CONTINUE
 3679 CONTINUE
      GO TO 8000
!
 8000 CONTINUE
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
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPCHAR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2
 9012 FORMAT('IBUGP2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IFOUND,IERROR
 9013 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IMAX
 9014 FORMAT('IMAX = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)NY
 9021 FORMAT('NY = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NY.LE.0)GO TO 9022
      DO 9023 I=1,NY
      WRITE(ICOUT,9024)I,Y(I)
 9024 FORMAT('I,Y(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9023 CONTINUE
 9022 CONTINUE
      WRITE(ICOUT,9031)NX
 9031 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NX.LE.0)GO TO 9032
      DO 9033 I=1,NX
      WRITE(ICOUT,9034)I,X(I)
 9034 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9033 CONTINUE
 9032 CONTINUE
      WRITE(ICOUT,9041)MAXCHA
 9041 FORMAT('MAXCHA = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NX.LE.0)GO TO 9042
      DO 9043 I=1,NX
      WRITE(ICOUT,9044)I,ACHAAN(I)
 9044 FORMAT('I,ACHAAN(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9043 CONTINUE
 9042 CONTINUE
 9090 CONTINUE
      RETURN
      END SUBROUTINE DPCHAN
      SUBROUTINE DPCHAR(MAXCHA,ICHAPA,ICHAPO,   &
                        IBUGP2,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTERS FOR USE IN MULTI-TRACE PLOTS.
!              THE CHARACTER FOR THE I-TH TRACE WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR ICHAPA(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --MAXCHA
!     OUTPUT ARGUMENTS--ICHAPA  (A  HOLLERITH VECTOR
!                       WHOSE I-TH ELEMENT IS THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977
!     UPDATED         --SEPTEMBER 1980
!     UPDATED         --MARCH     1982
!     UPDATED         --MAY       1982
!     UPDATED         --JULY      1983
!     UPDATED         --NOVEMBER  1986
!     UPDATED         --JANAURY   1988 (OMIT SORTING FOR CHAR AUTOMATIC)
!     UPDATED         --AUGUST    1987 TUFTE BOX PLOT
!     UPDATED         --NOVEMBER  1988 ERROR BAR PLOT
!     UPDATED         --JUNE      1989 CHAR AUTOMATIC DISTINCT
!     UPDATED         --SEPTEMBER 1990 AUGMENT CONTROL CHART
!     UPDATED         --NOVEMBER  1995 SUPPORT CASE ASIS
!     UPDATED         --FEBRUARY  1998 CHAR <SAVE/RESTORE>
!     UPDATED         --JANUARY   2001 CHAR AUTOMATIC SIGN
!     UPDATED         --FEBRUARY  2003 CHAR VIOLIN PLOT
!     UPDATED         --JUNE      2010 ALLOW 16 CHARACTERS FOR CHARACTER
!                                      PATTERN
!     UPDATED         --DECEMBER  2011 CHARACTER AUTOMATIC OFFSET
!     UPDATED         --JULY      2012 CHARACTER AUTOMATIC DYNAMIC
!     UPDATED         --APRIL     2018 ALLOW 24 CHARACTERS FOR CHARACTER
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC CHARACTER*4 IHARG       DECEMBER 1986
!CCCC CHARACTER*4 ICHAPA
!CCCC CHARACTER*4 ICHAPO
      CHARACTER*24 ICHAPA
      CHARACTER*24 ICHAPO
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEQ
      CHARACTER*4 IWRITE
      CHARACTER*4 ICTEXT
!CCCC FOLLOWING LINE JANAURY 2001
      CHARACTER*4 ISIGNF
      CHARACTER*4 IHYPSV
!
      CHARACTER*80 ISTRIN
      CHARACTER*80 ISTRCH
!
!---------------------------------------------------------------------
!
!CCCC DIMENSION IHARG(*)      DECEMBER 1986
      DIMENSION ICHAPA(*)
!CCCC ADD FOLLOWING LINE FEBRUARY 1998.
      DIMENSION ICHAPO(*)
      DIMENSION ICTEXT(100)
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCH'
      ISUBN2='AR  '
      IFOUND='NO'
      IERROR='NO'
      ICHAVN='NULL'
      IHYPSV=IHYPSW
      IHYPSW='OFF'
!
      NCCHAR=0
!
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHAR')THEN
        WRITE(ICOUT,11)ICOM,IHARG(1),IHARG(2),IHARG(3),NUMARG
   11   FORMAT('IN DPCHAR: ICOM,IHARG(1),IHARG(2),IHARG(3),NUMARG = ',   &
               4(2X,A4),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIZE')GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FILL')GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TYPE')GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TABU')GO TO 9000
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SAVE')THEN
        DO 2163 I=1,MAXCHA
          ICHAPO(I)=ICHAPA(I)
 2163   CONTINUE
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2164)
 2164     FORMAT('THE CURRENT CHARACTER SETTINGS HAVE BEEN SAVED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IFOUND='YES'
        GO TO 9000
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'REST')THEN
        DO 2168 I=1,MAXCHA
          ICHAPA(I)=ICHAPO(I)
 2168   CONTINUE
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2169)
 2169     FORMAT('THE SAVED CHARACTER SETTINGS HAVE BEEN RESTORED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IFOUND='YES'
        GO TO 9000
      ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'BOX'.AND.   &
              IHARG(2).EQ.'PLOT') .OR.   &
             (NUMARG.GE.3.AND.IHARG(2).EQ.'BOX'.AND.   &
              IHARG(3).EQ.'PLOT'))THEN
        IMAX=24
        ICHAPA(1)='X'
        ICHAPA(2)=' '
        ICHAPA(3)=' '
        ICHAPA(4)='X'
        ICHAPA(5)=' '
        ICHAPA(6)=' '
        ICHAPA(7)='X'
        ICHAPA(8)=' '
        ICHAPA(9)=' '
        ICHAPA(10)=' '
        ICHAPA(11)=' '
        ICHAPA(12)=' '
        ICHAPA(13)=' '
        ICHAPA(14)=' '
        ICHAPA(15)=' '
        ICHAPA(16)=' '
        ICHAPA(17)=' '
        ICHAPA(18)=' '
        ICHAPA(19)=' '
        ICHAPA(20)=' '
        ICHAPA(21)='CIRC'
        ICHAPA(22)='CIRC'
        ICHAPA(23)='CIRC'
        ICHAPA(24)='CIRC'
        GO TO 2170
      ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'TUFT'.AND.   &
             IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')THEN
        IMAX=24
        ICHAPA(1)=' '
        ICHAPA(2)=' '
        ICHAPA(3)=' '
        ICHAPA(4)='X'
        ICHAPA(5)=' '
        ICHAPA(6)=' '
        ICHAPA(7)=' '
        ICHAPA(8)=' '
        ICHAPA(9)=' '
        ICHAPA(10)=' '
        ICHAPA(11)=' '
        ICHAPA(12)=' '
        ICHAPA(13)=' '
        ICHAPA(14)=' '
        ICHAPA(15)=' '
        ICHAPA(16)=' '
        ICHAPA(17)=' '
        ICHAPA(18)=' '
        ICHAPA(19)=' '
        ICHAPA(20)=' '
        ICHAPA(21)='CIRC'
        ICHAPA(22)='CIRC'
        ICHAPA(23)='CIRC'
        ICHAPA(24)='CIRC'
        GO TO 2170
      ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'ERRO'.AND.   &
             IHARG(2).EQ.'BAR'.AND.IHARG(3).EQ.'PLOT')THEN
        IMAX=7
        ICHAPA(1)='CIRC'
        ICHAPA(2)='-'
        ICHAPA(3)='-'
        ICHAPA(4)='|'
        ICHAPA(5)='|'
        ICHAPA(6)=' '
        ICHAPA(7)=' '
        GO TO 2170
      ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'I'.AND.   &
             IHARG(2).EQ.'PLOT') .OR.   &
            (NUMARG.GE.3.AND.IHARG(2).EQ.'I'.AND.   &
             IHARG(3).EQ.'PLOT'))THEN
        IMAX=5
        ICHAPA(1)='-'
        ICHAPA(2)='X'
        ICHAPA(3)='-'
        ICHAPA(4)=' '
        ICHAPA(5)=' '
        GO TO 2170
      ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'CONT'.AND.   &
             IHARG(2).EQ.'CHAR') .OR.   &
            (NUMARG.GE.3.AND.IHARG(2).EQ.'CONT'.AND.   &
             IHARG(3).EQ.'CHAR'))THEN
        IMAX=7
        ICHAPA(1)='CIRC'
        ICHAPA(2)=' '
        ICHAPA(3)=' '
        ICHAPA(4)=' '
        ICHAPA(5)=' '
        ICHAPA(6)=' '
        ICHAPA(7)=' '
        GO TO 2170
      ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'VIOL'.AND.   &
             IHARG(2).EQ.'PLOT') .OR.   &
            (NUMARG.GE.4.AND.IHARG(1).EQ.'VIOL'.AND.   &
             IHARG(2).EQ.'TUFT'.AND.IHARG(3).EQ.'BOX'.AND.   &
             IHARG(4).EQ.'PLOT'))THEN
        IMAX=25
        ICHAPA(1)=' '
        ICHAPA(2)=' '
        ICHAPA(3)=' '
        ICHAPA(4)=' '
        ICHAPA(5)='X'
        ICHAPA(6)=' '
        ICHAPA(7)=' '
        ICHAPA(8)=' '
        ICHAPA(9)=' '
        ICHAPA(10)=' '
        ICHAPA(11)=' '
        ICHAPA(12)=' '
        ICHAPA(13)=' '
        ICHAPA(14)=' '
        ICHAPA(15)=' '
        ICHAPA(16)=' '
        ICHAPA(17)=' '
        ICHAPA(18)=' '
        ICHAPA(19)=' '
        ICHAPA(20)=' '
        ICHAPA(21)=' '
        ICHAPA(22)='CIRC'
        ICHAPA(23)='CIRC'
        ICHAPA(24)='CIRC'
        ICHAPA(25)='CIRC'
        GO TO 2170
      ELSEIF(NUMARG.GE.4.AND.IHARG(1).EQ.'VIOL'.AND.   &
             IHARG(2).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')THEN
        IMAX=25
        ICHAPA(1)=' '
        ICHAPA(2)='X'
        ICHAPA(3)=' '
        ICHAPA(4)=' '
        ICHAPA(5)='X'
        ICHAPA(6)=' '
        ICHAPA(7)=' '
        ICHAPA(8)='X'
        ICHAPA(9)=' '
        ICHAPA(10)=' '
        ICHAPA(11)=' '
        ICHAPA(12)=' '
        ICHAPA(13)=' '
        ICHAPA(14)=' '
        ICHAPA(15)=' '
        ICHAPA(16)=' '
        ICHAPA(17)=' '
        ICHAPA(18)=' '
        ICHAPA(19)=' '
        ICHAPA(20)=' '
        ICHAPA(21)=' '
        ICHAPA(22)='CIRC'
        ICHAPA(23)='CIRC'
        ICHAPA(24)='CIRC'
        ICHAPA(25)='CIRC'
        GO TO 2170
      ENDIF
      GO TO 1101
!
 2170 CONTINUE
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 2175 I=1,IMAX
          WRITE(ICOUT,2176)I,ICHAPA(I)(1:8)
 2176     FORMAT('CHARACTER ',I6,' HAS JUST BEEN SET TO ',A8)
          CALL DPWRST('XXX','BUG ')
 2175   CONTINUE
      ENDIF
      GO TO 8000
!
 1101 CONTINUE
!
      IF(NUMARG.LE.0 .OR.   &
         (NUMARG.EQ.1.AND.IHARG(1).EQ.'ALL') .OR.   &
         (IHARG(NUMARG).EQ.'OFF') .OR.   &
         (IHARG(NUMARG).EQ.'DEFA'))THEN
        DO 1165 I=1,MAXCHA
          ICHAPA(I)='    '
 1165   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAPA(I)(1:8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 8000
      ELSEIF(IHARG(NUMARG).EQ.'ON' .OR. IHARG(NUMARG).EQ.'AUTO')THEN
        DO 1115 I=1,MAXCHA
          ICHAPA(I)='X'
 1115   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAPA(I)(1:24)
 1116     FORMAT('ALL CHARACTERS HAVE JUST BEEN SET TO ',A24)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 8000
!
      ELSEIF(NUMARG.EQ.1)THEN
        IF(NUMARG.EQ.0)ICHAPA(1)='    '
        IF(NUMARG.GE.1)THEN
          IF(IHARG(1).EQ.'BOX')THEN
            ICHAPA(1)='SQUA'
          ELSE
            ICHAPA(1)=' '
!CCCC       ICHAPA(1)(1:4)=IHARLC(1)
!CCCC       ICHAPA(1)(5:8)=IHARL2(1)
            ISTART=1
            ISTOP=IWIDTH
            IWORD=2
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO 6001 II=1,IWIDTH
              ISTRIN(II:II)=IANSLC(II)(1:1)
 6001       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                        ICOL1,ICOL2,ISTRCH,NCCHAR,   &
                        IBUGP2,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(1)=' '
            ICHAPA(1)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          ENDIF
        ENDIF
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1126)I,ICHAPA(I)
 1126     FORMAT('CHARACTER ',I6,' HAS JUST BEEN SET TO ',A24)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 8000
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'ALL')THEN
        DO 1135 I=1,MAXCHA
          ICHAPA(I)=' '
          IF(IHARG(2).EQ.'BOX')THEN
            ICHAPA(I)='SQUA'
          ELSE
!CCCC       ICHAPA(I)(1:4)=IHARLC(2)
!CCCC       ICHAPA(I)(5:8)=IHARL2(2)
            ISTART=1
            ISTOP=IWIDTH
            IWORD=3
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO 6003 II=1,IWIDTH
              ISTRIN(II:II)=IANSLC(II)(1:1)
 6003       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                        ICOL1,ICOL2,ISTRCH,NCCHAR,   &
                        IBUGP2,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(I)=' '
            ICHAPA(I)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          ENDIF
 1135   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAPA(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 8000
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'ALL')THEN
        DO 1145 I=1,MAXCHA
          ICHAPA(I)=' '
          IF(IHARG(1).EQ.'BOX')THEN
            ICHAPA(I)='SQUA'
          ELSE
!CCCC       ICHAPA(I)(1:4)=IHARLC(1)
!CCCC       ICHAPA(I)(5:8)=IHARL2(1)
            ISTART=1
            ISTOP=IWIDTH
            IWORD=2
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO 6005 II=1,IWIDTH
              ISTRIN(II:II)=IANSLC(II)(1:1)
 6005       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                        ICOL1,ICOL2,ISTRCH,NCCHAR,   &
                        IBUGP2,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(I)=' '
            ICHAPA(I)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          ENDIF
 1145   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAPA(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 8000
      ELSEIF((NUMARG.GE.2.AND.IHARG(2).EQ.'SUBS'.AND.   &
              IHARG2(2).EQ.'ET  ') .OR.   &
             (NUMARG.GE.2.AND.IHARG(2).EQ.'EXCE'.AND.   &
              IHARG2(2).EQ.'PT  '))THEN
        ICASEQ='SUBS'
        GO TO 4190
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'FOR '.AND.   &
             IHARG2(2).EQ.'    ')THEN
        ICASEQ='FOR'
        GO TO 4190
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'AUTO')THEN
!
!               ***********************************************************
!               **  STEP 30--                                            **
!               **  TREAT THE    CHARACTERS AUTOMATIC <VARIABLE>   CASE  **
!               ***********************************************************
!
!       NOTE 2012/07: IF A "SET CHARACTER AUTOMATIC DYNAMIC ON" HAS BEEN
!                     ENTERED, JUST STORE THE VARIABLE NAME.
!
!               ********************************************
!               **  STEP 31--                             **
!               **  CHECK THE VALIDITY OF ARGUMENT 2 (OR 3)**
!               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
!               ********************************************
!
        ISTEPN='31'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IHLEFT=IHARG(2)
        IHLEF2=IHARG2(2)
        IF(IHARG(2).EQ.'DIST'.AND.IHARG2(2).EQ.'INCT')IHLEFT=IHARG(3)
        IF(IHARG(2).EQ.'DIST'.AND.IHARG2(2).EQ.'INCT')IHLEF2=IHARG2(3)
        ISIGNF='OFF'
        IF(IHARG(2).EQ.'SIGN'.AND.IHARG2(2).EQ.'    ')ISIGNF='ON'
        IF(IHARG(2).EQ.'SIGN'.AND.IHARG2(2).EQ.'    ')IHLEFT=IHARG(3)
        IF(IHARG(2).EQ.'SIGN'.AND.IHARG2(2).EQ.'    ')IHLEF2=IHARG2(3)
        IHWUSE='V'
        MESSAG='YES'
        CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(ICHADY.EQ.'ON')THEN
          WRITE(ICOUT,3010)ICOLL,NLEFT
 3010     FORMAT('CHARACTER AUTOMATIC: ICOLL,NLEFT = ',2I8)
          CALL DPWRST('XXX','BUG ')
          ICHAVN(1:4)=IHLEFT
          ICHAVN(5:8)=IHLEF2
          IF(IFEEDB.EQ.'OFF')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3003)
 3003       FORMAT('CHARACTER SETTINGS WILL BE EXTRACTED FROM ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3005)ICHAVN
 3005       FORMAT('VARIABLE ',A8,' WHEN THE PLOT IS GENERATED.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 9000
        ENDIF
!
        ICOLL=IVALUE(ILOCV)
        NLEFT=IN(ILOCV)
!
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')THEN
          WRITE(ICOUT,3090)ICOLL,NLEFT
 3090     FORMAT('CHARACTER AUTOMATIC: ICOLL,NLEFT = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *****************************************
!               **  STEP 32--                          **
!               **  CHECK TO SEE THE TYPE CASE--       **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
        ISTEPN='32'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICASEQ='FULL'
        ILOCQ=NUMARG+1
        IF(NUMARG.LT.1)GO TO 3290
        DO 3200 J=1,NUMARG
          J1=J
          IF((IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') .OR.   &
             (IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  '))THEN
            ICASEQ='SUBS'
            ILOCQ=J1
            GO TO 3290
          ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
            ICASEQ='FOR'
            ILOCQ=J1
            GO TO 3290
          ENDIF
 3200   CONTINUE
!
 3290   CONTINUE
        IF(IBUGP2.EQ.'OFF')THEN
          WRITE(ICOUT,3291)NUMARG,ILOCQ
 3291     FORMAT('NUMARG,ILOCQ = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *********************************************
!               **  STEP 33--                              **
!               **  TEMPORARILY FORM THE VARIABLE Y(.)     **
!               **  WHICH WILL HOLD THE RESPONSE VARIABLE. **
!               **  FORM THIS VARIABLE BY                  **
!               **  BRANCHING TO THE APPROPRIATE SUBCASE   **
!               **  (FULL, SUBSET, OR FOR).                **
!               *********************************************
!
        ISTEPN='33'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(ICASEQ.EQ.'SUBS')THEN
          NIOLD=NLEFT
          CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
          NQ=NIOLD
        ELSEIF(ICASEQ.EQ.'FOR')THEN
          NIOLD=NLEFT
          CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
                     NLOCAL,ILOCS,NS,IBUGQ,IERROR)
          NQ=NFOR
        ELSE
          DO 3315 I=1,NLEFT
            ISUB(I)=1
 3315     CONTINUE
          NQ=NLEFT
        ENDIF
!
        MINN2=1
        IF(NQ.LT.MINN2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3351)
 3351     FORMAT('***** ERROR IN DPCHAR--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3352)
 3352     FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',   &
                 'EXTRACTED,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3353)IHLEFT,IHLEF2
 3353     FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING',   &
                 'FROM VARIABLE ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3354)
 3354     FORMAT('      (FOR WHICH CHARACTER DEFINITIONS ARE TO BE ',   &
                 'GENERATED)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3356)MINN2
 3356     FORMAT('      MUST BE ',I8,' OR LARGER;  SUCH WAS NOT THE ',   &
                 'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3358)
 3358     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,3359)(IANS(I),I=1,MIN(IWIDTH,80))
 3359       FORMAT('      ',80A1)
           CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        MAXCP1=MAXCOL+1
        MAXCP2=MAXCOL+2
        MAXCP3=MAXCOL+3
        MAXCP4=MAXCOL+4
        MAXCP5=MAXCOL+5
        MAXCP6=MAXCOL+6
        J=0
        IMAX=NLEFT
        IF(NQ.LT.NLEFT)IMAX=NQ
        DO 3370 I=1,IMAX
          IF(ISUB(I).EQ.0)GO TO 3370
          J=J+1
!
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)Y(J)=V(IJ)
          IF(ICOLL.EQ.MAXCP1)Y(J)=PRED(I)
          IF(ICOLL.EQ.MAXCP2)Y(J)=RES(I)
          IF(ICOLL.EQ.MAXCP3)Y(J)=YPLOT(I)
          IF(ICOLL.EQ.MAXCP4)Y(J)=XPLOT(I)
          IF(ICOLL.EQ.MAXCP5)Y(J)=X2PLOT(I)
          IF(ICOLL.EQ.MAXCP6)Y(J)=TAGPLO(I)
          IF(ISIGNF.EQ.'ON')THEN
            IF(Y(J).GT.0.0)THEN
              ICHAPA(J)='+   '
            ELSEIF(Y(J).LT.0.0)THEN
              ICHAPA(J)='-   '
            ELSEIF(Y(J).EQ.0.0)THEN
              ICHAPA(J)='0   '
            ELSE
              ICHAPA(J)='0   '
            ENDIF
          ENDIF
!
          IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')THEN
            WRITE(ICOUT,3365)ISIGNF,J,Y(J),ICHAPA(J)
 3365       FORMAT('ISIGNF,J,Y(J),ICHAPA(J) = ',A4,2X,I5,G15.7,2X,A24)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
 3370   CONTINUE
        NS=J
        NY=J
        IF(ISIGNF.EQ.'ON')GO TO 8000
!
!               *****************************************
!               **  STEP 34--                          **
!               **  IF HAVE THE FORM--                 **
!               **  CHARACTERS AUTOMATIC DISTINCT X    **
!               **  EXTRACT THE DISTINCT VALUES        **
!               **  FROM THE TARGET VARIABLE Y(.)   .  **
!               **  STORE THEM IN X(.)   .             **
!               **  IF HAVE THE FORM--                 **
!               **  CHARACTERS AUTOMATIC X             **
!               **  DO NOTHING                         **
!               *****************************************
!
        ISTEPN='34'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(IHARG(2).EQ.'DIST'.AND.IHARG2(2).EQ.'INCT')THEN
          IWRITE='OFF'
          CALL DISTIN(Y,NY,IWRITE,X,NX,IBUGP2,IERROR)
        ELSE
          DO 3411 I=1,NY
            X(I)=Y(I)
 3411     CONTINUE
          NX=NY
        ENDIF
!
!               ***********************************
!               **  STEP 35--                    **
!               **  SORT THESE DISTINCT VALUES   **
!               **  (IN PLACE).                  **
!               ***********************************
!
!CCCC   CALL SORT(X,NX,X)
!
!               ******************************************
!               **  STEP 36--                           **
!               **  CONVERT THE NUMERIC VALUES IN X(.)  **
!               **  TO CHARACTER STRINGS.               **
!               **  THEN LOAD THESE STRINGS             **
!               **  INTO INDIVIDUAL ELEMENTS            **
!               **  OF ICHAPA(.)                        **
!               **  NOTE--MAX CHARACTERS/STRING = 4     **
!               **        MAX NUMBER OF STRINGS = 100   **
!               ******************************************
!
        ISTEPN='36'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IMAX=NX
        IF(IMAX+ICHAOF.GT.MAXCHA)IMAX=MAXCHA-ICHAOF
        DO 3650 I=1,IMAX
          ICHAPA(I+ICHAOF)=' '
          VAL=X(I)
          IVAL=INT(VAL+0.5)
          IF(VAL.LT.0.0)IVAL=INT(VAL-0.5)
          NUMDID=(-1)
          CALL DPCON2(IVAL,VAL,ICTEXT,NCTEXT,NUMDID,IBUGP2,IERROR)
          JMAX=NCTEXT
          IF(JMAX.GT.24)JMAX=24
          DO 3660 J=1,JMAX
            ICHAPA(I+ICHAOF)(J:J)=ICTEXT(J)(1:1)
 3660     CONTINUE
!
          IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')THEN
            WRITE(ICOUT,3665)I,ICHAOF,ICHAPA(I+ICHAOF)
 3665       FORMAT('I,ICHAOF,ICHAPA(I+ICHAOF) = ',2I6,2X,A24)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
 3650   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          DO 3675 I=1,IMAX
            WRITE(ICOUT,3676)I+ICHAOF,ICHAPA(I+ICHAOF)
 3676       FORMAT('CHARACTER ',I6,' HAS JUST BEEN SET TO ',A24)
            CALL DPWRST('XXX','BUG ')
 3675     CONTINUE
        ENDIF
        GO TO 8000
      ELSE
        IMAX=NUMARG
        IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
        DO 1155 I=1,IMAX
          ICHAPA(I)=' '
          IF(IHARG(I).EQ.'BOX')THEN
            ICHAPA(I)='SQUA'
          ELSE
!CCCC       ICHAPA(I)(1:4)=IHARLC(I)
!CCCC       ICHAPA(I)(5:8)=IHARL2(I)
            ISTART=1
            ISTOP=IWIDTH
            IWORD=I+1
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO 6007 II=1,IWIDTH
              ISTRIN(II:II)=IANSLC(II)(1:1)
 6007       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                        ICOL1,ICOL2,ISTRCH,NCCHAR,   &
                        IBUGP2,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(I)=' '
            ICHAPA(I)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          ENDIF
 1155   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          DO 1156 I=1,IMAX
            WRITE(ICOUT,1126)I,ICHAPA(I)
            CALL DPWRST('XXX','BUG ')
 1156     CONTINUE
        ENDIF
        GO TO 8000
      ENDIF
!
!               ***********************************************************
!               **  STEP 40--                                            **
!               **  TREAT THE CHARACTERS ... SUBSET/EXCEPT/FOR CASE      **
!               **  FOR REDEFINING SPECIFIED CHARACTERS                  **
!               ***********************************************************
!
!               *****************************************
!               **  STEP 41--                          **
!               **  DEFINE THE TYPE CASE--             **
!               **    1) SUBSET/EXCEPT                 **
!               **    2) FOR.                          **
!               *****************************************
!
 4190 CONTINUE
!
      ISTEPN='41'
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILOCQ=2
!
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')THEN
        WRITE(ICOUT,4191)ICASEQ,ILOCQ,NUMARG
 4191   FORMAT('ICASEQ,ILOCQ,NUMARG = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************
!               **  STEP 42--                              **
!               **  DETERMINE WHICH ELEMENTS ARE           **
!               **  TO BE REDEFINED.                       **
!               *********************************************
!
      ISTEPN='42'
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NQ=0
      IF(ICASEQ.EQ.'SUBS')THEN
        NIOLD=MAXCHA
        CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
        NQ=NIOLD
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        NIOLD=MAXCHA
        CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
                   NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        NQ=NFOR
      ENDIF
!
      IF(NQ.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3351)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4252)
 4252   FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',   &
               'EXTRACTED,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4253)IHLEFT,IHLEF2
 4253   FORMAT('      EXTRACTED, NO CHARACTER ELEMENTS  ',   &
               'FROM VARIABLE ',A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4254)
 4254   FORMAT('      REMAINED TO BE REDEFINED. ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4255)ICASEQ
 4255   FORMAT('ICASEQ = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4258)
 4258   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,4259)(IANS(I),I=1,MIN(80,IWIDTH))
 4259     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *********************************************
!               **  STEP 43--                              **
!               **  REDEFINE THE DESIGNATED CHARACTERS.    **
!               *********************************************
!
      ISTEPN='43'
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IMAX=MAXCHA
      IF(NQ.LT.MAXCHA)IMAX=NQ
      DO 4310 I=1,IMAX
        IF(ISUB(I).EQ.0)GO TO 4310
        ICHAPA(I)=' '
!CCCC   ICHAPA(I)(1:4)=IHARLC(1)
!CCCC   ICHAPA(I)(5:8)=IHARL2(1)
        ISTART=1
        ISTOP=IWIDTH
        IWORD=2
        NCCHAR=0
        ISTRIN=' '
        ISTRCH=' '
        DO 6008 II=1,IWIDTH
          ISTRIN(II:II)=IANSLC(II)(1:1)
 6008   CONTINUE
        CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                    ICOL1,ICOL2,ISTRCH,NCCHAR,   &
                    IBUGP2,ISUBRO,IERROR)
        IF(NCCHAR.GT.24)NCCHAR=24
        ICHAPA(I)=' '
        ICHAPA(I)(1:NCCHAR)=ISTRCH(1:NCCHAR)
 4310 CONTINUE
!
!               *********************************************
!               **  STEP 44--                              **
!               **  IF CALLED FOR,                         **
!               **  PRINT OUT A MESSAGE.                   **
!               *********************************************
!
      ISTEPN='44'
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFEEDB.EQ.'OFF')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 4410 I=1,IMAX
          IF(ISUB(I).EQ.0)GO TO 4410
          WRITE(ICOUT,4411)I,ICHAPA(I)
 4411     FORMAT('CHARACTER ',I6,' HAS JUST BEEN SET TO ',A24)
          CALL DPWRST('XXX','BUG ')
 4410   CONTINUE
      ENDIF
      GO TO 8000
!
 8000 CONTINUE
      IFOUND='YES'
      DO 8010 I=1,MAXCHA
        IF(ICHAPA(I)(1:4).EQ.'BLAN')ICHAPA(I)='BLAN'
        IF(ICHAPA(I)(1:4).EQ.'blan')ICHAPA(I)='BLAN'
        IF(ICHAPA(I)(1:4).EQ.'NONE')ICHAPA(I)='BLAN'
        IF(ICHAPA(I).EQ.'BL')ICHAPA(I)='BLAN'
        IF(ICHAPA(I).EQ.'bl')ICHAPA(I)='BLAN'
        IF(ICHAPA(I).EQ.'NO')ICHAPA(I)='BLAN'
 8010 CONTINUE
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IHYPSW=IHYPSV
!
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHAR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IBUGP2,IFOUND,IERROR
 9013   FORMAT('IBUGP2,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IMAX,NY,NX,MAXCHA,ICHAOF
 9014   FORMAT('IMAX,NY,NX,MAXCHA,ICHAOF = ',5I8)
        CALL DPWRST('XXX','BUG ')
        IF(NY.GT.0)THEN
          DO 9023 I=1,NY
            WRITE(ICOUT,9024)I,Y(I)
 9024       FORMAT('I,Y(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
 9023     CONTINUE
        ENDIF
        IF(NX.GT.0)THEN
          DO 9033 I=1,NX
            WRITE(ICOUT,9034)I,X(I),ICHAPA(I)
 9034       FORMAT('I,X(I),ICHAPA(I) = ',I8,G15.7,2X,A24)
            CALL DPWRST('XXX','BUG ')
 9033     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHAR
      SUBROUTINE DPCHCA(IHARG,NUMARG,IDEFCA,MAXCHA,ICHACA,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER CASES FOR USE IN MULTI-TRACE PLOTS.
!              THE CASE FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR ICHACA(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFCA
!                     --MAXCHA
!     OUTPUT ARGUMENTS--ICHACA  (A  HOLLERITH VECTOR
!                       WHOSE I-TH ELEMENT IS THE CASE
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCA
      CHARACTER*4 ICHACA
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ICHACA(*)
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
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'CASE')GO TO 1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CASE')GO TO 1105
      GO TO 1199
!
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1110
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
!
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GO TO 1160
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GO TO 1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GO TO 1140
!
      GO TO 1150
!
 1110 CONTINUE
      DO 1115 I=1,MAXCHA
      ICHACA(I)=IDEFCA
 1115 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHACA(I)
 1116 FORMAT('ALL CHARACTER CASES HAVE JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 1190
!
 1120 CONTINUE
      ICHACA(1)=IHARG(2)
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,ICHACA(I)
 1126 FORMAT('THE CASE FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 1190
!
 1130 CONTINUE
      DO 1135 I=1,MAXCHA
      ICHACA(I)=IHARG(3)
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHACA(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 1190
!
 1140 CONTINUE
      DO 1145 I=1,MAXCHA
      ICHACA(I)=IHARG(2)
 1145 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHACA(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 1190
!
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO 1155 I=1,IMAX
      IP1=I+1
      ICHACA(I)=IHARG(IP1)
 1155 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1156 I=1,IMAX
      WRITE(ICOUT,1126)I,ICHACA(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GO TO 1190
!
 1160 CONTINUE
      DO 1165 I=1,MAXCHA
      ICHACA(I)=IDEFCA
 1165 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHACA(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GO TO 1190
!
 1190 CONTINUE
      IFOUND='YES'
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPCHCA
      SUBROUTINE DPCHCL(IHARG,IARG,NUMARG,IDEFCO,MAXCHA,ICHACO,   &
                        ICHAC2,IRGBMX,ICASCL,   &
                        IBUGP2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER COLORS FOR USE IN MULTI-TRACE PLOTS.
!              THE COLOR FOR THE CHARACTER FOR THE I-TH TRACE WILL BE
!              PLACED IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR ICHACO(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFCO
!                     --MAXCHA
!     OUTPUT ARGUMENTS--ICHACO  (A  HOLLERITH VECTOR WHOSE I-TH ELEMENT
!                       IS THE COLOR FOR THE CHARACTER ASSIGNED TO THE
!                       I-TH  TRACE IN A MULTI-TRACE PLOT.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCO
      CHARACTER*4 ICHACO
      CHARACTER*4 ICASCL
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARG(*)
      DIMENSION ICHACO(*)
      DIMENSION ICHAC2(MAXCHA,3)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPCH'
      ISUBN2='CL  '
      IHOLD1='-999'
      IHOLD2='-999'
      NUMCHA=MAXCHA
!
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHCL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCHCL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGP2,ISUBRO,IFOUND,IERROR
   52   FORMAT('IBUGP2,ISUBRO,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IDEBCO,MAXCHA,NUMCHA,NUMARG,IRGBMX
   53   FORMAT('IDEBCO,MAXCHA,NUMCHA,NUMARG,IRGBMX = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        DO 65 I=1,NUMARG
          WRITE(ICOUT,66)I,IARG(I),IHARG(I)
   66     FORMAT('I,IARG(I),IHARG(I) = ',2I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO 75 I=1,10
          WRITE(ICOUT,76)I,ICHACO(I),ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3)
   76     FORMAT('I,ICHACO(I),ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3) = ',   &
                 I8,2X,A4,2X,3I5)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
!
      IF(NUMARG.EQ.0)GO TO 9000
      IF(ICASCL.EQ.'RGB ')GO TO 2000
!
!     THIS IS THE "STANDARD" CASE
!
      IF((NUMARG.EQ.1.AND.IHARG(1).EQ.'COLO') .OR.   &
         (NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL'))THEN
        DO 1165 I=1,MAXCHA
          ICHACO(I)=IDEFCO
 1165   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHACO(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'COLO')THEN
        IF(IHARG(NUMARG).EQ.'ON' .OR. IHARG(NUMARG).EQ.'OFF' .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
          DO 1115 I=1,MAXCHA
            ICHACO(I)=IDEFCO
 1115     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHACO(I)
 1116       FORMAT('ALL CHARACTER COLORS HAVE JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.EQ.2)THEN
          ICHACO(1)=IHARG(2)
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I,ICHACO(I)
 1126       FORMAT('THE COLOR FOR CHARACTER ',I6,' HAS JUST BEEN SET ',   &
                   'TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')THEN
          DO 1135 I=1,MAXCHA
            ICHACO(I)=IHARG(3)
 1135     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHACO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSEIF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')THEN
          DO 1145 I=1,MAXCHA
            ICHACO(I)=IHARG(2)
 1145     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHACO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IMAX=NUMARG-1
          IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
          DO 1155 I=1,IMAX
            IP1=I+1
            ICHACO(I)=IHARG(IP1)
 1155     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO 1156 I=1,IMAX
              WRITE(ICOUT,1126)I,ICHACO(I)
              CALL DPWRST('XXX','BUG ')
 1156       CONTINUE
          ENDIF
        ENDIF
      ELSE
        GO TO 9000
      ENDIF
!
      IFOUND='YES'
      GO TO 9000
!
!     RGB COLORS CASE: 3 COLORS SHOULD BE GIVEN
!
!                      CHARACTER COLOR
!                      CHARACTER COLOR IRED IBLUE IGREEN
!                      CHARACTER COLOR IRED IBLUE IGREEN ALL
!                      CHARACTER COLOR ALL IRED IBLUE IGREEN
!                      CHARACTER COLOR IRED1 IBLUE1 IGREEN1 IRED2 ...
!
!                      THE "RGB" KEYWORD HAS ALREADY BEEN STRIPPED
!                      OUT.  NOTE THAT THE DEFAULT COLOR IS -1
!                      (I.E., NO RGB COLOR VALUES SPECIFIED).
!
 2000 CONTINUE
!
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHCL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      JHOLD1=-1
      JHOLD2=-1
      JHOLD3=-1
      NUMCHA=MAXCHA
!
      IF(NUMARG.EQ.2 .AND. IHARG(2).EQ.'ALL')THEN
        GO TO 2300
      ELSEIF(NUMARG.EQ.5)THEN
        IF(IHARG(2).EQ.'ALL')THEN
          JHOLD1=IARG(3)
          JHOLD2=IARG(4)
          JHOLD3=IARG(5)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD1=-1
          GO TO 2300
        ELSEIF(IHARG(5).EQ.'ALL')THEN
          JHOLD1=IARG(2)
          JHOLD2=IARG(3)
          JHOLD3=IARG(4)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD1=-1
          GO TO 2300
        ENDIF
      ENDIF
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
!               *************************************************
!
      ISTEPN='2A'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHCL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)THEN
        NUMCHA=1
        ICHAC2(1,1)=-1
        ICHAC2(1,2)=-1
        ICHAC2(1,3)=-1
      ELSE
        NTEMP=NUMARG-1
        NUMCHA=NTEMP/3
        IF(NUMCHA.LT.1)THEN
          NUMCHA=1
          ICHAC2(1,1)=-1
          ICHAC2(1,2)=-1
          ICHAC2(1,3)=-1
        ELSEIF(NUMCHA.GT.MAXCHA)THEN
          NUMCHA=MAXCHA
        ENDIF
        DO 2225 I=1,NUMCHA
          J1=(I-1)*3+2
          J2=J1+1
          J3=J1+2
          JHOLD1=IARG(J1)
          JHOLD2=IARG(J2)
          JHOLD3=IARG(J3)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-999
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-999
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-999
          ICHAC2(I,1)=JHOLD1
          ICHAC2(I,2)=JHOLD2
          ICHAC2(I,3)=JHOLD3
 2225   CONTINUE
      ENDIF
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 2278 I=1,NUMCHA
          WRITE(ICOUT,2276)I,ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3)
 2276     FORMAT('THE RGB COLORS OF CHARACTER ',I6,   &
                 ' HAVE JUST BEEN SET TO ',3I8)
          CALL DPWRST('XXX','BUG ')
 2278   CONTINUE
      ENDIF
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 2300 CONTINUE
      ISTEPN='2B'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHCL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 2315 I=1,NUMCHA
        ICHAC2(I,1)=JHOLD1
        ICHAC2(I,2)=JHOLD2
        ICHAC2(I,3)=JHOLD3
 2315 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,2316)ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3)
 2316   FORMAT('THE RGB COLORS OF ALL CHARACTERS HAVE JUST ',   &
               'BEEN SET TO ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHCL')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHCL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,NUMCHA
 9012   FORMAT('IFOUND,IERROR,NUMCHA = ',2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IHOLD1,IHOLD2
 9014   FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 9075 I=1,10
          WRITE(ICOUT,76)I,ICHACO(I),ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3)
          CALL DPWRST('XXX','BUG ')
 9075   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHCL
      SUBROUTINE DPCHEC(K,IHOL,IHOL2,   &
      IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
      INT1,FLOAT1,IBUGA3,IERROR)
!
!     PURPOSE--EXAMINE COMPONENT K OF IHOL(.) AND IHOL2(.).
!     IF IT IS A PARAMETER NAME, DETERMINE THE VALUE
!     OF THE PARAMETER AND PLACE THIS VALUE
!     IN INT1(K) AND FLOAT1(K).
!     IF OTHERWISE, DO NOTHING.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JANUARY   1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
!
!---------------------------------------------------------------------
!
      DIMENSION IHOL(*)
      DIMENSION IHOL2(*)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
!
      DIMENSION INT1(*)
      DIMENSION FLOAT1(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('****** AT THE BEGINNING OF DPCHEC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)K,IHOL(K),IHOL2(K)
   52 FORMAT('K,IHOL(K),IHOL2(K) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NUMNAM,IBUGA3,IERROR
   53 FORMAT('NUMNAM,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NUMNAM
      WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)
   56 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
      I8,2X,A4,2X,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,57)K,INT1(K),FLOAT1(K)
   57 FORMAT('K,INT1(K),FLOAT1(K) = ',I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
      IH=IHOL(K)
      IH2=IHOL2(K)
      IF(NUMNAM.LE.0)GO TO 2799
      DO 2795 I=1,NUMNAM
      IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.   &
      IUSE(I).EQ.'P')GO TO 2796
      GO TO 2795
 2796 CONTINUE
      INT1(K)=IVALUE(I)
      FLOAT1(K)=VALUE(I)
      GO TO 2799
 2795 CONTINUE
 2799 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('****** AT THE END       OF DPCHEC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)K,IHOL(K),IHOL2(K)
 9012 FORMAT('K,IHOL(K),IHOL2(K) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NUMNAM,IBUGA3,IERROR
 9013 FORMAT('NUMNAM,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NUMNAM
      WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)
 9016 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
      I8,2X,A4,2X,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9017)K,INT1(K),FLOAT1(K)
 9017 FORMAT('K,INT1(K),FLOAT1(K) = ',I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPCHEC
      SUBROUTINE DPCHEX(ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2)
!
!     PURPOSE--CHARACTER EXTRACTION--
!              GIVEN A CHARACTER STRING IN A WORD (IX1),
!              MOVE THE BIT STRING WHICH STARTS IN BIT ISTAR1
!              (ISTAR1 RANGES FROM 0 TO 35 IN A UNIVAC 1108,
!                                  0 TO 31 IN AN IBM 3033,
!                                  0 TO 59 IN A CDC 7600, ETC.
!              AND IS OF LENGTH ILEN1 BITS)
!              INTO BITS STARTING AT ISTAR2 OF LENGTH ILEN2
!              (HERE ILEN2 USUALLY = ILEN1) IN THE WORD IX2.
!              OUTPUT THE NEW CHARACTER VARIABLE (IX2).
!     NOTE--0 DENOTES THE LEFT-MOST (THAT IS, THE HIGH-ORDER) BIT.
!     NOTE--ISTAR1 AND ISTAR2 RANGE FROM 0 TO NUMBPW-1
!           THAT IS, FROM 0 TO ONE LESS THAN THE TOTLA NUMBER OF BITS PER WORD.
!           (FOR EXAMPLE, ON UNIVAC 1100/82--FROM 0 TO 35
!                         ON VAX    11/780 --FROM 0 TO 31)
!     NOTE--IX1 AND IX2 ARE CHARACTER*4 VARIABLES.
!     NOTE--THIS SUBROUTINE HAS BEEN CONSTRAINED SO THAT
!           NEITHER ILEN1 NOR ILEN2 ARE EXPLICITELY USED.
!           THIS SUBROUTINE, AS CODED, OPERATES ON THE ASSUMPTIONS THAT
!              1) ILEN1 = NUMBPC (THAT IS, THE LENGTH
!                 OF THE BIT STRING BEING MOVED IS IDENTICAL
!                 TO THE NUMBER OF BITS PER CHARACTER ON
!                 YOUR COMPUTER).
!              2) ILEN2 = ILEN1 (THAT IS, THE LENGTH OF THE OUTPUT STRING =
!                 THE LENGTH OF THE INPUT STRING),
!              3) ISTAR1 IS SUCH THAT THE START OF THE BIT STRING
!                 IS ALWAYS AT THE BEGINNING OF A CHARACTER
!           THE NET RESULT IS THAT THIS SUBROUTINE, AS CODED,
!           EXTRACTS EXACTLY 1 CHARACTER AND
!           MOVES IT TO THE POSITION OF ANOTHER CHARACTER.
!           THESE CONSTRAINTS WILL BE ACCEPTABLE FOR ALL USES
!           OF THIS SUBROUTINE BY ANY OTHER DATAPLOT SUBROUTINE.
!     NOTE--THE VALUES FOR NUMBPC (NUMBER OF BITS PER CHARACTER)
!           AND NUMBPW (NUMBER OF BITS PER WORD) ARE SET
!           FOR YOUR COMPUTER IN DATAPLOT SUBROUTINE INITMC.
!     NOTE--ALGORITHM PROVIDED BY MICHAEL VOGT
!                                 INFORMATION TECHNOLOGY LABORATORY
!                                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--OCTOBER  1978.
!     UPDATED         --JUNE      1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IX1
      CHARACTER*4 IX2
!
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               ********************************************************
!               **  THE FOLLOWING CODE WILL CARRY OUT                 **
!               **  THE CHARACTER EXTRACTION FOR ALL COMPUTERS        **
!               **  WITH AN ANSI 77 FORTRAN COMPILER--IT MAKES        **
!               **  USE OF THE ANSI FORTRAN 77 CONSTRUCT--            **
!               **  IY(IC:ID)=IX(IA:IB)                               **
!               **  WHERE IX AND IY ARE CHARACTER*4 VARIABLES,        **
!               **  WHERE IA, IB, IC, AND ID ARE INTEGER VARIABLES,   **
!               **  AND WHERE IY(IC:ID)=IX(IA:IB) MEANS               **
!               **  TO COPY CHARACTERS IA THROUGH IB OF VARIABLE IX AND
!               **  PLACE THEM INTO CHARACTERS IC THROUGH ID OF VARIABLE IY.
!               **  WITH ALL OTHER CHARACTERS IN IY BEING UNAFFECTED. **
!               **  USUALLY IA, IB, IC, AND ID RANGE FROM 1 TO 4.     **
!               ********************************************************
!
      IF(ISUBG4.EQ.'CHEX')THEN
        WRITE(ICOUT,51)ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2
   51   FORMAT('FROM DPCHEX: ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2 = ',6I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IBYTE1=(ISTAR1+NUMBPC)/NUMBPC
      IBYTE2=(ISTAR2+NUMBPC)/NUMBPC
      IX2(IBYTE2:IBYTE2)=IX1(IBYTE1:IBYTE1)
      GO TO 9000
!
!               ****************************************************************
!               **  CHARACTER EXTRACTION FOR THE UNIVAC 1100 SERIES. FOR COMPILE
!               **  (FORTRAN 1966 COMPILER)
!               ****************************************************************
!
!CCCC ISTAR1=IABS(ISTAR1)
!CCCC ISTAR2=IABS(ISTAR2)
!
!CCCC FLD(ISTAR2,ILEN2,IX2)=FLD(ISTAR1,ILEN1,IX1)
!
!               ****************************************************************
!               **  CHARACTER EXTRACTION FOR THE UNIVAC 1100 SERIES. FTN COMPILE
!               **  (FORTRAN 1977 COMPILER)
!               ****************************************************************
!
!CCCC ISTR1P=ISTAR1+1
!CCCC ISTR2P=ISTAR2+1
!
!CCCC BITS(IX2,ISTR2P,ILEN2)=BITS(IX1,ISTR1P,ILEN1)
!
!               ***********************************************
!               **  CHARACTER EXTRACTION FOR THE VAX-11/780  **
!               **  (FORTRAN 1966 COMPILER)
!               ***********************************************
!
!CCCC LOGICAL*1 IX1(4)
!CCCC LOGICAL*1 IX2(4)
!
!CCCC I1=(ISTAR1+8)/8
!CCCC I2=(ISTAR2+8)/8
!CCCC IX2(I2)=IX1(I1)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPCHEX
      SUBROUTINE DPCHFI(IHARG,NUMARG,IDEFFI,MAXCHA,ICHAFI,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER FILL SWITCH FOR USE IN MULTI-TRACE PLOTS.
!              THE FILL SWITCH FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR ICHAFI(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFFI
!                     --MAXCHA
!     OUTPUT ARGUMENTS--ICHAFI  (A  HOLLERITH VECTOR
!                       WHOSE I-TH ELEMENT IS THE FILL SWITCH
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1998. CHECK FOR CHARCTER FILL COLOR
!                                       (SKIP IF ABOVE FOUND)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFI
      CHARACTER*4 ICHAFI
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ICHAFI(*)
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
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'FILL')GO TO 1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'FILL')GO TO 1105
      GO TO 1199
!
 1105 CONTINUE
!CCCC IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
!CCCC IF(IHARG(NUMARG).EQ.'OFF')GO TO 1110
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
!CCCC ADD FOLLOWING LINE  JUNE 1998
      IF(IHARG(NUMARG).EQ.'COLO')GO TO 1199
!
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GO TO 1160
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GO TO 1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GO TO 1140
!
      GO TO 1150
!
 1110 CONTINUE
      DO 1115 I=1,MAXCHA
      ICHAFI(I)=IDEFFI
      IF(IHARG(NUMARG).EQ.'ON')ICHAFI(I)='ON'
      IF(IHARG(NUMARG).EQ.'OFF')ICHAFI(I)='OFF'
      IF(IHARG(NUMARG).EQ.'AUTO')ICHAFI(I)='ON'
 1115 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAFI(I)
 1116 FORMAT('ALL CHARACTER FILL SWITCHES HAVE JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 1190
!
 1120 CONTINUE
      ICHAFI(1)=IHARG(2)
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,ICHAFI(I)
 1126 FORMAT('THE FILL SWITCH FOR CHARACTER ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 1190
!
 1130 CONTINUE
      DO 1135 I=1,MAXCHA
      ICHAFI(I)=IHARG(3)
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAFI(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 1190
!
 1140 CONTINUE
      DO 1145 I=1,MAXCHA
      ICHAFI(I)=IHARG(2)
 1145 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAFI(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 1190
!
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO 1155 I=1,IMAX
      IP1=I+1
      ICHAFI(I)=IHARG(IP1)
 1155 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1156 I=1,IMAX
      WRITE(ICOUT,1126)I,ICHAFI(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GO TO 1190
!
 1160 CONTINUE
      DO 1165 I=1,MAXCHA
      ICHAFI(I)=IDEFFI
      IF(IHARG(1).EQ.'ON')ICHAFI(I)='ON'
      IF(IHARG(1).EQ.'OFF')ICHAFI(I)='OFF'
      IF(IHARG(1).EQ.'AUTO')ICHAFI(I)='ON'
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'FILL')ICHAFI(I)='ON'
 1165 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAFI(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GO TO 1190
!
 1190 CONTINUE
      IFOUND='YES'
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPCHFI
      SUBROUTINE DPCHFO(IHARG,NUMARG,IDEFFO,MAXCHA,ICHAFO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER FONTS FOR USE IN MULTI-TRACE PLOTS.
!              THE FONT FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR ICHAFO(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFFO
!                     --MAXCHA
!     OUTPUT ARGUMENTS--ICHAFO  (A  HOLLERITH VECTOR
!                       WHOSE I-TH ELEMENT IS THE FONT
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JULY      2021. "HARDWARE" AS SYNONYM FOR
!                                       "TEKTRONIX
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFO
      CHARACTER*4 ICHAFO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ICHAFO(*)
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
      IF((NUMARG.EQ.1.AND.IHARG(1).EQ.'FONT') .OR.   &
         (NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL'))THEN
        DO 1165 I=1,MAXCHA
          ICHAFO(I)=IDEFFO
 1165   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAFO(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'FONT')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
          DO 1115 I=1,MAXCHA
            ICHAFO(I)=IDEFFO
 1115     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHAFO(I)
 1116       FORMAT('ALL CHARACTER FONTS HAVE JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.EQ.2)THEN
          ICHAFO(1)=IHARG(2)
          IF(ICHAFO(1).EQ.'HARD')ICHAFO(1)='TEKT'
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I,ICHAFO(I)
 1126       FORMAT('THE FONT FOR CHARACTER ',I6,   &
                   ' HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')THEN
          DO 1135 I=1,MAXCHA
            ICHAFO(I)=IHARG(3)
            IF(ICHAFO(I).EQ.'HARD')ICHAFO(I)='TEKT'
 1135     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHAFO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')THEN
          DO 1145 I=1,MAXCHA
            ICHAFO(I)=IHARG(2)
            IF(ICHAFO(I).EQ.'HARD')ICHAFO(I)='TEKT'
 1145     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHAFO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IMAX=NUMARG-1
          IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
          DO 1155 I=1,IMAX
            IP1=I+1
            ICHAFO(I)=IHARG(IP1)
            IF(ICHAFO(I).EQ.'HARD')ICHAFO(I)='TEKT'
 1155     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO 1156 I=1,IMAX
              WRITE(ICOUT,1126)I,ICHAFO(I)
              CALL DPWRST('XXX','BUG ')
 1156       CONTINUE
          ENDIF
        ENDIF
      ENDIF
!
      IFOUND='YES'
!
      RETURN
      END SUBROUTINE DPCHFO
      SUBROUTINE DPCHGR(ICHAR2,ICHARN,IBUG,IFOUND)
!
!     PURPOSE--NUMERICALLY CONVERT A GREEK ALPHABETIC CHARACTER.
!              CONVERT A PACKED ALPHABETIC STRING
!              (PACKED INTO 1 COMPUTER WORD
!              WITH ONLY THE FIRST 4 CHARACTERS BEING SIGNIFICANT)
!              (ALPH... TO OMEG...) INTO A NUMERIC VALUE
!              (1 TO 24).
!     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
!                              CONTAINING THE HOLLERITH
!                              CHARACTER(S) OF INTEREST.
!     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
!                              CONTAINING THE NUMERIC
!                              DESIGNATION FOR THE
!                              ALPHABETIC CHARACTER.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MARCH     1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUG
      CHARACTER*4 IFOUND
!
!-----COMMON VARIABLES (BUGS & ERROR)---------------------------------
!
      CHARACTER*4 IBUGG4
      CHARACTER*4 ISUBG4
      CHARACTER*4 IERRG4
!
      COMMON /ICOMBE/IBUGG4,ISUBG4,IERRG4
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
!
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHGR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCHGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)ICHAR2,IBUG,ISUBG4
   59   FORMAT('ICHAR2,IBUG,ISUBG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  CONVERT THE CHARACTER       **
!               **********************************
!
      IF(ICHAR2.EQ.'ALPH')GO TO 100
      IF(ICHAR2.EQ.'BETA')GO TO 200
      IF(ICHAR2.EQ.'GAMM')GO TO 300
      IF(ICHAR2.EQ.'DELT')GO TO 400
      IF(ICHAR2.EQ.'EPSI')GO TO 500
      IF(ICHAR2.EQ.'ZETA')GO TO 600
      IF(ICHAR2.EQ.'ETA')GO TO 700
      IF(ICHAR2.EQ.'THET')GO TO 800
      IF(ICHAR2.EQ.'IOTA')GO TO 900
      IF(ICHAR2.EQ.'KAPP')GO TO 1000
      IF(ICHAR2.EQ.'LAMB')GO TO 1100
      IF(ICHAR2.EQ.'MU')GO TO 1200
      IF(ICHAR2.EQ.'NU')GO TO 1300
      IF(ICHAR2.EQ.'XI')GO TO 1400
      IF(ICHAR2.EQ.'OMIC')GO TO 1500
      IF(ICHAR2.EQ.'PI')GO TO 1600
      IF(ICHAR2.EQ.'RHO')GO TO 1700
      IF(ICHAR2.EQ.'SIGM')GO TO 1800
      IF(ICHAR2.EQ.'TAU')GO TO 1900
      IF(ICHAR2.EQ.'UPSI')GO TO 2000
      IF(ICHAR2.EQ.'PHI')GO TO 2100
      IF(ICHAR2.EQ.'CHI')GO TO 2200
      IF(ICHAR2.EQ.'PSI')GO TO 2300
      IF(ICHAR2.EQ.'OMEG')GO TO 2400
      GO TO 7900
!
  100 CONTINUE
      ICHARN=1
      GO TO 8000
!
  200 CONTINUE
      ICHARN=2
      GO TO 8000
!
  300 CONTINUE
      ICHARN=3
      GO TO 8000
!
  400 CONTINUE
      ICHARN=4
      GO TO 8000
!
  500 CONTINUE
      ICHARN=5
      GO TO 8000
!
  600 CONTINUE
      ICHARN=6
      GO TO 8000
!
  700 CONTINUE
      ICHARN=7
      GO TO 8000
!
  800 CONTINUE
      ICHARN=8
      GO TO 8000
!
  900 CONTINUE
      ICHARN=9
      GO TO 8000
!
 1000 CONTINUE
      ICHARN=10
      GO TO 8000
!
 1100 CONTINUE
      ICHARN=11
      GO TO 8000
!
 1200 CONTINUE
      ICHARN=12
      GO TO 8000
!
 1300 CONTINUE
      ICHARN=13
      GO TO 8000
!
 1400 CONTINUE
      ICHARN=14
      GO TO 8000
!
 1500 CONTINUE
      ICHARN=15
      GO TO 8000
!
 1600 CONTINUE
      ICHARN=16
      GO TO 8000
!
 1700 CONTINUE
      ICHARN=17
      GO TO 8000
!
 1800 CONTINUE
      ICHARN=18
      GO TO 8000
!
 1900 CONTINUE
      ICHARN=19
      GO TO 8000
!
 2000 CONTINUE
      ICHARN=20
      GO TO 8000
!
 2100 CONTINUE
      ICHARN=21
      GO TO 8000
!
 2200 CONTINUE
      ICHARN=22
      GO TO 8000
!
 2300 CONTINUE
      ICHARN=23
      GO TO 8000
!
 2400 CONTINUE
      ICHARN=24
      GO TO 8000
!
 7900 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7911)
!7911 FORMAT('***** ERROR IN DPCHNU--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7912)
!7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7913)ICHAR2
!7913 FORMAT('      INPUT CHAR2ACTER = ',A4)
!CCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GO TO 9000
!
 8000 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHGR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,ICHAR2,ICHARN
 9013   FORMAT('IFOUND,ICHAR2,ICHARN = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHGR
      SUBROUTINE DPCHHW(MAXCHA,PCHAHE,PCHAWI,PDEFHE,PDEFWI,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER HEIGHT AND WIDTH
!              FOR USE IN MULTI-TRACE PLOTS.
!              THE HEIGHT AND WIDTH FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE FLOATING POINT
!              VECTORS PCHAHE(.) AND PCHAWI(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --MAXCHA
!     OUTPUT ARGUMENTS--PCHAHE  (A  FLOATING POINT VECTOR
!                       WHOSE I-TH ELEMENT IS THE HEIGHT
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --PCHAWI  (A  FLOATING POINT VECTOR
!                       WHOSE I-TH ELEMENT IS THE WIDTH
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --PDEFHE  = DEFAULT CHARACTER HEIGHT
!                     --PDEFWI  = DEFAULT CHARACTER WIDTH
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/8
!     ORIGINAL VERSION--AUGUST    1988.
!     UPDATED         --JANUARY   1995. ALLOW ? AS ARGUMENT (FOR HELP)
!     UPDATED         --MARCH     2021. FOR ?, SAVE CURRENT SETTING TO
!                                       PARAMETER
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC CHARACTER*4 IHARG
!CCCC CHARACTER*4 IARGT
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
!CCCC DIMENSION IHARG(*)
!CCCC DIMENSION IARGT(*)
!CCCC DIMENSION ARG(*)
!
      DIMENSION PCHAHE(*)
      DIMENSION PCHAWI(*)
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 IBUGS3
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN0='CHHW'
      IBUGS3='XXXX'
!
      IF((NUMARG.EQ.1 .AND. IHARG(1).EQ.'HW') .OR.   &
         (NUMARG.EQ.2 .AND. IHARG(2).EQ.'ALL'))THEN
        DO 1165 I=1,MAXCHA
          PCHAHE(I)=PDEFHE
          PCHAWI(I)=PDEFWI
 1165   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1117)PCHAHE(I),PCHAWI(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 2190
!
      ELSEIF(NUMARG.GE.2 .AND. IHARG(1).EQ.'HW')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
          DO 1115 I=1,MAXCHA
            PCHAHE(I)=PDEFHE
            PCHAWI(I)=PDEFWI
 1115     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)
 1116       FORMAT('THE HEIGHTS AND WIDTHS OF ALL CHARACTERS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1117)PCHAHE(I),PCHAWI(I)
 1117       FORMAT('    HAVE JUST BEEN SET TO ',2E15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSEIF(IHARG(NUMARG).EQ.'?')THEN
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1221)I,PCHAHE(I)
 1221       FORMAT('THE CURRENT HEIGHT FOR CHARACTER ',I6,' IS ',E15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1222)I,PCHAWI(I)
 1222       FORMAT('THE CURRENT WIDTH  FOR CHARACTER ',I6,' IS ',E15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1223)I,PDEFHE
 1223       FORMAT('THE DEFAULT HEIGHT FOR CHARACTER ',I6,' IS ',E15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1224)I,PDEFWI
 1224       FORMAT('THE DEFAULT WIDTH  FOR CHARACTER ',I6,' IS ',E15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IH='CHAR'
          IH2='HEIG'
          CALL DPADDP(IH,IH2,PCHAHE(1),IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGS3,IERROR)
          IH='CHAR'
          IH2='WIDT'
          CALL DPADDP(IH,IH2,PCHAWI(1),IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGS3,IERROR)
          GO TO 2190
        ELSEIF(NUMARG.EQ.3)THEN
          I=1
          IF(IARGT(2).NE.'NUMB'.OR.IARGT(3).NE.'NUMB')GO TO 1180
          PCHAHE(1)=ARG(2)
          PCHAWI(1)=ARG(3)
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I
 1126       FORMAT('THE HEIGHT AND WIDTH OF CHARACTER ',I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1127)PCHAHE(I),PCHAWI(I)
 1127       FORMAT('    HAS JUST BEEN SET TO ',2E15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSEIF(NUMARG.GE.4.AND.IHARG(2).EQ.'ALL')THEN
          I=1
          IF(IARGT(3).NE.'NUMB'.OR.IARGT(4).NE.'NUMB')GO TO 1180
          DO 1135 I=1,MAXCHA
            PCHAHE(I)=ARG(3)
            PCHAWI(I)=ARG(4)
 1135     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1117)PCHAHE(I),PCHAWI(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSEIF(NUMARG.GE.4.AND.IHARG(4).EQ.'ALL')THEN
          I=1
          IF(IARGT(2).NE.'NUMB'.OR.IARGT(3).NE.'NUMB')GO TO 1180
          DO 1145 I=1,MAXCHA
            PCHAHE(I)=ARG(2)
            PCHAWI(I)=ARG(3)
 1145     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1117)PCHAHE(I),PCHAWI(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSE
          IMAX=NUMARG-1
          IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
          J=0
          DO 1155 I=1,IMAX,2
            IP1=I+1
            IP2=I+2
            IF(IARGT(IP1).NE.'NUMB')GO TO 1180
            IF(IARGT(IP2).NE.'NUMB')GO TO 1180
            J=J+1
            PCHAHE(J)=ARG(IP1)
            PCHAWI(J)=ARG(IP2)
 1155     CONTINUE
          JMAX=J
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO 1156 I=1,JMAX
              WRITE(ICOUT,1126)I
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1127)PCHAHE(I),PCHAWI(I)
              CALL DPWRST('XXX','BUG ')
 1156       CONTINUE
          ENDIF
          GO TO 2190
        ENDIF
      ELSE
        GO TO 9000
      ENDIF
!
 1180 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN CHARACTER HW (DPCHHW)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('THE HEIGHTS AND WIDTHS OF CHARACTERS MUST BE NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('HOWEVER, THE SPECIFIED CHARACTER HEIGHT AND WIDTH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)I
 1184 FORMAT('FOR CHARACTER ',I6,' WAS NON-NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 2190 CONTINUE
      IFOUND='YES'
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPCHHW
      SUBROUTINE DPCHJU(IHARG,NUMARG,MAXCHA,ICHAJU,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER JUSTIFICATION FOR USE IN MULTI-TRACE PLOTS.
!              THE JUSTIFICATION FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR ICHAJU(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --MAXCHA
!     OUTPUT ARGUMENTS--ICHAJU  (A  HOLLERITH VECTOR
!                       WHOSE I-TH ELEMENT IS THE JUSTIFICATION
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1986.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 ICHAJU
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ICHAJU(*)
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
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'JUST')GO TO 1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'JUST')GO TO 1105
      GO TO 1199
!
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1110
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
!
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GO TO 1160
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GO TO 1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GO TO 1140
!
      GO TO 1150
!
 1110 CONTINUE
      DO 1115 I=1,MAXCHA
      ICHAJU(I)='CENT'
 1115 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAJU(I)
 1116 FORMAT('ALL CHARACTER JUSTIFICATIONS HAVE JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 1190
!
 1120 CONTINUE
      ICHAJU(1)=IHARG(2)
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,ICHAJU(I)
 1126 FORMAT('THE JUSTIFICATION FOR CHARACTER ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 1190
!
 1130 CONTINUE
      DO 1135 I=1,MAXCHA
      ICHAJU(I)=IHARG(3)
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAJU(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 1190
!
 1140 CONTINUE
      DO 1145 I=1,MAXCHA
      ICHAJU(I)=IHARG(2)
 1145 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAJU(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 1190
!
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO 1155 I=1,IMAX
      IP1=I+1
      ICHAJU(I)=IHARG(IP1)
 1155 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1156 I=1,IMAX
      WRITE(ICOUT,1126)I,ICHAJU(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GO TO 1190
!
 1160 CONTINUE
      DO 1165 I=1,MAXCHA
      ICHAJU(I)='CENT'
 1165 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAJU(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GO TO 1190
!
 1190 CONTINUE
      IFOUND='YES'
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPCHJU
      SUBROUTINE DPCHLI(ICONT,NUMCPL,YSTART,YSTOP,XSTART,XSTOP,   &
      J,JD,Y2,X2,D2,IERROR)
!
!     PURPOSE--GENERATE PLOT COORDINATES FOR A POINT
!              OR FOR A LINE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JANUARY   1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICONT
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
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
      IERROR='NO'
!
      NUMCP2=NUMCPL
      IF(ICONT.EQ.'ON')NUMCP2=2
      ANUMC2=NUMCP2
!
      IF(YSTART.EQ.YSTOP)GO TO 200
      IF(XSTART.EQ.XSTOP)GO TO 1300
      GO TO 1400
!
  200 CONTINUE
      IF(XSTART.EQ.XSTOP)GO TO 1100
      GO TO 1200
!
!               ***************************
!               **  STEP 2.1--           **
!               **  TREAT THE CASE WHEN  **
!               **  Y HAS NO CHANGE      **
!               **  X HAS NO CHANGE      **
!               ***************************
!
 1100 CONTINUE
      J=J+1
      JD=JD+1
      Y2(J)=YSTART
      X2(J)=XSTART
      D2(J)=JD
      GO TO 9000
!
!               ***************************
!               **  STEP 2.2--           **
!               **  TREAT THE CASE WHEN  **
!               **  Y HAS NO CHANGE      **
!               **  X HAS    CHANGE      **
!               ***************************
!
 1200 CONTINUE
      JD=JD+1
      XDEL=XSTOP-XSTART
      DO 1210 I=1,NUMCP2
      J=J+1
      AI=I
      P=(AI-1.0)/(ANUMC2-1.0)
      XP=XSTART+P*XDEL
      Y2(J)=YSTART
      X2(J)=XP
      D2(J)=JD
 1210 CONTINUE
      GO TO 9000
!
!               ***************************
!               **  STEP 2.3--           **
!               **  TREAT THE CASE WHEN  **
!               **  Y HAS    CHANGE      **
!               **  X HAS NO CHANGE      **
!               ***************************
!
 1300 CONTINUE
      JD=JD+1
      YDEL=YSTOP-YSTART
      DO 1310 I=1,NUMCP2
      J=J+1
      AI=I
      P=(AI-1.0)/(ANUMC2-1.0)
      YP=YSTART+P*YDEL
      Y2(J)=YP
      X2(J)=XSTART
      D2(J)=JD
 1310 CONTINUE
      GO TO 9000
!
!               ***************************
!               **  STEP 2.4--           **
!               **  TREAT THE CASE WHEN  **
!               **  Y HAS    CHANGE      **
!               **  X HAS    CHANGE      **
!               ***************************
!
 1400 CONTINUE
      JD=JD+1
      XDEL=XSTOP-XSTART
      YDEL=YSTOP-YSTART
      DO 1410 I=1,NUMCP2
      J=J+1
      AI=I
      P=(AI-1.0)/(ANUMC2-1.0)
      XP=XSTART+P*XDEL
      YP=YSTART+P*YDEL
      Y2(J)=YP
      X2(J)=XP
      D2(J)=JD
 1410 CONTINUE
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPCHLI
      SUBROUTINE DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUG,IERROR)
!
!     PURPOSE--CHECK FOR A LEFT AND RIGHT PARENTHESIS.
!              CHECK FOR A LEFT  PARENTHESIS IN LOCATION ILOCLP.
!              CHECK FOR A RIGHT PARENTHESIS IN LOCATION ILOCRP.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --FEBRUARY  1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISTRIN
      CHARACTER*4 IFOULR
      CHARACTER*4 IBUG
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION ISTRIN(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOULR='NO'
      IERROR='NO'
!
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHLR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCHLR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NUMCHS,ILOCLP,ILOCRP
   52   FORMAT('NUMCHS,ILOCLP,ILOCRP = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)(ISTRIN(I),I=1,MIN(100,NUMCHS))
   53   FORMAT('(ISTRIN(I),I=1,NUMCHS) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IBUG,ISUBG4,IERRG4
   59   FORMAT('IBUG,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ILOCLP.LT.1)GO TO 1200
      IF(ILOCLP.GT.NUMCHS)GO TO 1200
!
      IF(ILOCRP.LT.1)GO TO 1200
      IF(ILOCRP.GT.NUMCHS)GO TO 1200
!
      IF(ISTRIN(ILOCLP).NE.'(')GO TO 1200
      IF(ISTRIN(ILOCRP).NE.')')GO TO 1200
!
      IFOULR='YES'
      GO TO 9000
!
 1200 CONTINUE
      IFOULR='NO'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHLR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END      OF DPCHLR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOULR,IERRG4
 9012   FORMAT('IFOULR,IERRG4 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHLR
      SUBROUTINE DPCHMA(ICHAR2,ICHARN,IBUG,IFOUND)
!
!     PURPOSE--CONVERT A MATHEMATICAL SYMBOL
!              INTO A NUMERIC VALUE
!              (1 TO 66).
!     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
!                              CONTAINING THE HOLLERITH
!                              CHARACTER(S) OF INTEREST.
!     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
!                              CONTAINING THE NUMERIC
!                              DESIGNATION FOR THE
!                              ALPHABETIC CHARACTER.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MARCH     1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --APRIL     1987.
!     UPDATED         --AUGUST    1992.  ADD SYNONYMS FOR REVERSE
!                                        TRIANGLE (TO AGREE WITH
!                                        DOCUMENTATION), ADD ARROW CASE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUG
      CHARACTER*4 IFOUND
!
      CHARACTER*1 IBASLC
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
!
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHMA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCHMA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)ICHAR2,IBUGG4,ISUBG4
   59   FORMAT('ICHAR2,IBUGG4,ISUBG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  CONVERT THE CHARACTER       **
!               **********************************
!
      IF(ICHAR2.EQ.'/   ')GO TO 100
      IF(ICHAR2.EQ.'(   ')GO TO 200
      IF(ICHAR2.EQ.')   ')GO TO 300
      IF(ICHAR2.EQ.'[   ')GO TO 400
      IF(ICHAR2.EQ.'LBRA')GO TO 400
      IF(ICHAR2.EQ.']   ')GO TO 500
      IF(ICHAR2.EQ.'RBRA')GO TO 500
      IF(ICHAR2.EQ.'{   ')GO TO 600
      IF(ICHAR2.EQ.'LCBR')GO TO 600
      IF(ICHAR2.EQ.'}   ')GO TO 700
      IF(ICHAR2.EQ.'RCBR')GO TO 700
      IF(ICHAR2.EQ.'LELB')GO TO 800
      IF(ICHAR2.EQ.'RELB')GO TO 900
      IF(ICHAR2.EQ.'|   ')GO TO 1000
      IF(ICHAR2.EQ.'VBAR')GO TO 1000
      IF(ICHAR2.EQ.':   ')GO TO 1100
      IF(ICHAR2.EQ.'DVBA')GO TO 1100
      IF(ICHAR2.EQ.'COLO')GO TO 1100
      IF(ICHAR2.EQ.'-   ')GO TO 1200
      IF(ICHAR2.EQ.'MINU')GO TO 1200
      IF(ICHAR2.EQ.'+   ')GO TO 1300
      IF(ICHAR2.EQ.'PLUS')GO TO 1300
      IF(ICHAR2.EQ.'CROS')GO TO 1300
      IF(ICHAR2.EQ.'+-  ')GO TO 1400
      IF(ICHAR2.EQ.'-+  ')GO TO 1500
      IF(ICHAR2.EQ.'TIME')GO TO 1600
      IF(ICHAR2.EQ.'DOTP')GO TO 1700
      IF(ICHAR2.EQ.'/   ')GO TO 1800
      IF(ICHAR2.EQ.'DIVI')GO TO 1800
      IF(ICHAR2.EQ.'SLAS')GO TO 1800
      IF(ICHAR2.EQ.'=   ')GO TO 1900
      IF(ICHAR2.EQ.'EQUA')GO TO 1900
      IF(ICHAR2.EQ.'NOT=')GO TO 2000
      IF(ICHAR2.EQ.'<>')GO TO 2000
      IF(ICHAR2.EQ.'><')GO TO 2000
      IF(ICHAR2.EQ.'EQUI')GO TO 2100
      IF(ICHAR2.EQ.'<   ')GO TO 2200
      IF(ICHAR2.EQ.'LT  ')GO TO 2200
      IF(ICHAR2.EQ.'>   ')GO TO 2300
      IF(ICHAR2.EQ.'GT  ')GO TO 2300
      IF(ICHAR2.EQ.'<=  ')GO TO 2400
      IF(ICHAR2.EQ.'=<  ')GO TO 2400
      IF(ICHAR2.EQ.'LTEQ')GO TO 2400
      IF(ICHAR2.EQ.'>=  ')GO TO 2500
      IF(ICHAR2.EQ.'=>  ')GO TO 2500
      IF(ICHAR2.EQ.'GTEQ')GO TO 2500
      IF(ICHAR2.EQ.'VARI')GO TO 2600
      IF(ICHAR2.EQ.'APPR')GO TO 2700
      IF(ICHAR2.EQ.'~   ')GO TO 2700
      IF(ICHAR2.EQ.'TILD')GO TO 2700
      IF(ICHAR2.EQ.'CARA')GO TO 2800
      IF(ICHAR2.EQ.'PRIM')GO TO 2900
      IF(ICHAR2.EQ.'`   ')GO TO 3000
      IF(ICHAR2.EQ.'LACC')GO TO 3000
      IF(ICHAR2.EQ.'BREV')GO TO 3100
      IF(ICHAR2.EQ.'RQUO')GO TO 3200
      IF(ICHAR2.EQ.'LQUO')GO TO 3300
      IF(ICHAR2.EQ.'NASP')GO TO 3400
      IF(ICHAR2.EQ.'IASP')GO TO 3500
      IF(ICHAR2.EQ.'RADI')GO TO 3600
      IF(ICHAR2.EQ.'SUBS')GO TO 3700
      IF(ICHAR2.EQ.'UNIO')GO TO 3800
      IF(ICHAR2.EQ.'SUPE')GO TO 3900
      IF(ICHAR2.EQ.'INTR')GO TO 4000
      IF(ICHAR2.EQ.'ELEM')GO TO 4100
      IF(ICHAR2.EQ.'RARR')GO TO 4200
      IF(ICHAR2.EQ.'^   ')GO TO 4300
      IF(ICHAR2.EQ.'UARR')GO TO 4300
      IF(ICHAR2.EQ.'LARR')GO TO 4400
      IF(ICHAR2.EQ.'DARR')GO TO 4500
      IF(ICHAR2.EQ.'PART')GO TO 4600
      IF(ICHAR2.EQ.'DEL ')GO TO 4700
      IF(ICHAR2.EQ.'LRAD')GO TO 4800
      IF(ICHAR2.EQ.'INTE')GO TO 4900
      IF(ICHAR2.EQ.'CINT')GO TO 5000
      IF(ICHAR2.EQ.'INFI')GO TO 5100
      IF(ICHAR2.EQ.'%   ')GO TO 5200
      IF(ICHAR2.EQ.'&   ')GO TO 5300
      IF(ICHAR2.EQ.'@   ')GO TO 5400
      IF(ICHAR2.EQ.'$   ')GO TO 5500
      IF(ICHAR2.EQ.'#   ')GO TO 5600
      IF(ICHAR2.EQ.'PARA')GO TO 5700
      IF(ICHAR2.EQ.'DAGG')GO TO 5800
      IF(ICHAR2.EQ.'DDAG')GO TO 5900
      IF(ICHAR2.EQ.'THEX')GO TO 6000
      IF(ICHAR2.EQ.'PROD')GO TO 6100
      IF(ICHAR2.EQ.'SUMM')GO TO 6200
      IF(ICHAR2.EQ.'THFO')GO TO 6300
      IF(ICHAR2.EQ.'LVBA')GO TO 6400
      IF(ICHAR2.EQ.'HBAR')GO TO 6500
      IF(ICHAR2.EQ.'LHBA')GO TO 6600
!
      IF(ICHAR2.EQ.'.   ')GO TO 10100
      IF(ICHAR2.EQ.'POIN')GO TO 10100
      IF(ICHAR2.EQ.'PO  ')GO TO 10100
      IF(ICHAR2.EQ.'PT  ')GO TO 10100
      IF(ICHAR2.EQ.'CIRC')GO TO 10200
      IF(ICHAR2.EQ.'CI  ')GO TO 10200
      IF(ICHAR2.EQ.'SQUA')GO TO 10300
      IF(ICHAR2.EQ.'SQ  ')GO TO 10300
      IF(ICHAR2.EQ.'TRIA')GO TO 10400
      IF(ICHAR2.EQ.'TR  ')GO TO 10400
      IF(ICHAR2.EQ.'DIAM')GO TO 10500
      IF(ICHAR2.EQ.'DI  ')GO TO 10500
      IF(ICHAR2.EQ.'STAR')GO TO 10600
      IF(ICHAR2.EQ.'ST  ')GO TO 10600
      IF(ICHAR2.EQ.'*   ')GO TO 10700
      IF(ICHAR2.EQ.'ASTE')GO TO 10700
      IF(ICHAR2.EQ.'AS  ')GO TO 10700
      IF(ICHAR2.EQ.'TRIR')GO TO 10800
      IF(ICHAR2.EQ.'TRII')GO TO 10800
!  AUGUST 1992.  ADD FOLLOWING 2 LINES (TO MAKE DOCUMENTATION CORRECT)
      IF(ICHAR2.EQ.'REVT')GO TO 10800
      IF(ICHAR2.EQ.'RT  ')GO TO 10800
!
      IF(ICHAR2.EQ.'BARU')GO TO 10900
      IF(ICHAR2.EQ.'BU  ')GO TO 10900
      IF(ICHAR2.EQ.'BARV')GO TO 10900
      IF(ICHAR2.EQ.'BV  ')GO TO 10900
      IF(ICHAR2.EQ.'BARH')GO TO 11000
      IF(ICHAR2.EQ.'BH  ')GO TO 11000
      IF(ICHAR2.EQ.'ARRU')GO TO 11100
      IF(ICHAR2.EQ.'AU  ')GO TO 11100
      IF(ICHAR2.EQ.'ARRD')GO TO 11200
      IF(ICHAR2.EQ.'AD  ')GO TO 11200
      IF(ICHAR2.EQ.'ARRL')GO TO 11300
      IF(ICHAR2.EQ.'AL  ')GO TO 11300
      IF(ICHAR2.EQ.'ARRR')GO TO 11400
      IF(ICHAR2.EQ.'AR  ')GO TO 11400
      CALL DPCONA(92,IBASLC)
      IF(ICHAR2.EQ.IBASLC)GO TO 11500
      IF(ICHAR2.EQ.'BASL')GO TO 11500
      IF(ICHAR2.EQ.'BACK')GO TO 11500
      IF(ICHAR2.EQ.'BS  ')GO TO 11500
      IF(ICHAR2.EQ.'_   ')GO TO 11600
      IF(ICHAR2.EQ.'UNDE')GO TO 11600
      IF(ICHAR2.EQ.'CUBE')GO TO 11700
      IF(ICHAR2.EQ.'PYRA')GO TO 11800
!  AUGUST 1992.  ADD AN ARROW OPTION
      IF(ICHAR2.EQ.'ARRO')GO TO 11900
      IF(ICHAR2.EQ.'ARRH')GO TO 11900
      IF(ICHAR2.EQ.'VECT')GO TO 11900
!
      GO TO 17900
!
  100 CONTINUE
      ICHARN=1
      GO TO 18000
!
  200 CONTINUE
      ICHARN=2
      GO TO 18000
!
  300 CONTINUE
      ICHARN=3
      GO TO 18000
!
  400 CONTINUE
      ICHARN=4
      GO TO 18000
!
  500 CONTINUE
      ICHARN=5
      GO TO 18000
!
  600 CONTINUE
      ICHARN=6
      GO TO 18000
!
  700 CONTINUE
      ICHARN=7
      GO TO 18000
!
  800 CONTINUE
      ICHARN=8
      GO TO 18000
!
  900 CONTINUE
      ICHARN=9
      GO TO 18000
!
 1000 CONTINUE
      ICHARN=10
      GO TO 18000
!
 1100 CONTINUE
      ICHARN=11
      GO TO 18000
!
 1200 CONTINUE
      ICHARN=12
      GO TO 18000
!
 1300 CONTINUE
      ICHARN=13
      GO TO 18000
!
 1400 CONTINUE
      ICHARN=14
      GO TO 18000
!
 1500 CONTINUE
      ICHARN=15
      GO TO 18000
!
 1600 CONTINUE
      ICHARN=16
      GO TO 18000
!
 1700 CONTINUE
      ICHARN=17
      GO TO 18000
!
 1800 CONTINUE
      ICHARN=18
      GO TO 18000
!
 1900 CONTINUE
      ICHARN=19
      GO TO 18000
!
 2000 CONTINUE
      ICHARN=20
      GO TO 18000
!
 2100 CONTINUE
      ICHARN=21
      GO TO 18000
!
 2200 CONTINUE
      ICHARN=22
      GO TO 18000
!
 2300 CONTINUE
      ICHARN=23
      GO TO 18000
!
 2400 CONTINUE
      ICHARN=24
      GO TO 18000
!
 2500 CONTINUE
      ICHARN=25
      GO TO 18000
!
 2600 CONTINUE
      ICHARN=26
      GO TO 18000
!
 2700 CONTINUE
      ICHARN=27
      GO TO 18000
!
 2800 CONTINUE
      ICHARN=28
      GO TO 18000
!
 2900 CONTINUE
      ICHARN=29
      GO TO 18000
!
 3000 CONTINUE
      ICHARN=30
      GO TO 18000
!
 3100 CONTINUE
      ICHARN=31
      GO TO 18000
!
 3200 CONTINUE
      ICHARN=32
      GO TO 18000
!
 3300 CONTINUE
      ICHARN=33
      GO TO 18000
!
 3400 CONTINUE
      ICHARN=34
      GO TO 18000
!
 3500 CONTINUE
      ICHARN=35
      GO TO 18000
!
 3600 CONTINUE
      ICHARN=36
      GO TO 18000
!
 3700 CONTINUE
      ICHARN=37
      GO TO 18000
!
 3800 CONTINUE
      ICHARN=38
      GO TO 18000
!
 3900 CONTINUE
      ICHARN=39
      GO TO 18000
!
 4000 CONTINUE
      ICHARN=40
      GO TO 18000
!
 4100 CONTINUE
      ICHARN=41
      GO TO 18000
!
 4200 CONTINUE
      ICHARN=42
      GO TO 18000
!
 4300 CONTINUE
      ICHARN=43
      GO TO 18000
!
 4400 CONTINUE
      ICHARN=44
      GO TO 18000
!
 4500 CONTINUE
      ICHARN=45
      GO TO 18000
!
 4600 CONTINUE
      ICHARN=46
      GO TO 18000
!
 4700 CONTINUE
      ICHARN=47
      GO TO 18000
!
 4800 CONTINUE
      ICHARN=48
      GO TO 18000
!
 4900 CONTINUE
      ICHARN=49
      GO TO 18000
!
 5000 CONTINUE
      ICHARN=50
      GO TO 18000
!
 5100 CONTINUE
      ICHARN=51
      GO TO 18000
!
 5200 CONTINUE
      ICHARN=52
      GO TO 18000
!
 5300 CONTINUE
      ICHARN=53
      GO TO 18000
!
 5400 CONTINUE
      ICHARN=54
      GO TO 18000
!
 5500 CONTINUE
      ICHARN=55
      GO TO 18000
!
 5600 CONTINUE
      ICHARN=56
      GO TO 18000
!
 5700 CONTINUE
      ICHARN=57
      GO TO 18000
!
 5800 CONTINUE
      ICHARN=58
      GO TO 18000
!
 5900 CONTINUE
      ICHARN=59
      GO TO 18000
!
 6000 CONTINUE
      ICHARN=60
      GO TO 18000
!
 6100 CONTINUE
      ICHARN=61
      GO TO 18000
!
 6200 CONTINUE
      ICHARN=62
      GO TO 18000
!
 6300 CONTINUE
      ICHARN=63
      GO TO 18000
!
 6400 CONTINUE
      ICHARN=64
      GO TO 18000
!
 6500 CONTINUE
      ICHARN=65
      GO TO 18000
!
 6600 CONTINUE
      ICHARN=66
      GO TO 18000
!
10100 CONTINUE
      ICHARN=101
      GO TO 18000
!
10200 CONTINUE
      ICHARN=102
      GO TO 18000
!
10300 CONTINUE
      ICHARN=103
      GO TO 18000
!
10400 CONTINUE
      ICHARN=104
      GO TO 18000
!
10500 CONTINUE
      ICHARN=105
      GO TO 18000
!
10600 CONTINUE
      ICHARN=106
      GO TO 18000
!
10700 CONTINUE
      ICHARN=107
      GO TO 18000
!
10800 CONTINUE
      ICHARN=108
      GO TO 18000
!
10900 CONTINUE
      ICHARN=109
      GO TO 18000
!
11000 CONTINUE
      ICHARN=110
      GO TO 18000
!
11100 CONTINUE
      ICHARN=111
      GO TO 18000
!
11200 CONTINUE
      ICHARN=112
      GO TO 18000
!
11300 CONTINUE
      ICHARN=113
      GO TO 18000
!
11400 CONTINUE
      ICHARN=114
      GO TO 18000
!
11500 CONTINUE
      ICHARN=115
      GO TO 18000
!
11600 CONTINUE
      ICHARN=116
      GO TO 18000
!
11700 CONTINUE
      ICHARN=117
      GO TO 18000
!
11800 CONTINUE
      ICHARN=118
      GO TO 18000
!  AUGUST 1992.  ADDED FOLLOWING 3 LINES
11900 CONTINUE
      ICHARN=119
      GO TO 18000
!
17900 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7911)
!7911 FORMAT('***** ERROR IN DPCHMA--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7912)
!7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7913)ICHAR2
!7913 FORMAT('      INPUT CHARACTER = ',A4)
!CCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GO TO 19000
!
18000 CONTINUE
      IFOUND='YES'
      GO TO 19000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
19000 CONTINUE
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHMA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19011)
19011   FORMAT('***** AT THE END       OF DPCHMA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19013)IFOUND,ICHAR2,ICHARN
19013   FORMAT('IFOUND,ICHAR2,ICHARN = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHMA
      SUBROUTINE DPCHNU(ICHAR2,ICHARN,IBUG,IFOUND)
!
!     PURPOSE--CONVERT AN ALPHABETIC CHARACTER
!              (0 TO 9) INTO A NUMERIC VALUE
!              (1 TO 10).
!     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
!                              CONTAINING THE HOLLERITH
!                              CHARACTER(S) OF INTEREST.
!     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
!                              CONTAINING THE NUMERIC
!                              DESIGNATION FOR THE
!                              ALPHABETIC CHARACTER.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MARCH     1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUG
      CHARACTER*4 IFOUND
!
      CHARACTER*1 ICH1
      CHARACTER*1 ICH2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
!
      ICH1='-'
      ICH2='-'
!
      ICH1N=(-999)
      ICH2N=(-999)
!
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHNU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCHNU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)ICHAR2,IBUGG4,ISUBG4
   59   FORMAT('ICHAR2,IBUGG4,ISUBG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  CONVERT THE CHARACTER       **
!               **********************************
!
      ICH2(1:1)=ICHAR2(2:2)
!CCCC ICH2N=ICHAR(ICH2)
      CALL DPCOAN(ICH2,ICH2N)
      IF(ICH2N.EQ.32)GO TO 1100
      GO TO 7900
!
 1100 CONTINUE
      ICH1(1:1)=ICHAR2(1:1)
!CCCC ICH1N=ICHAR(ICH1)
      CALL DPCOAN(ICH1,ICH1N)
      ICHARN=ICH1N-47
      IF(1.LE.ICHARN.AND.ICHARN.LE.10)GO TO 8000
      GO TO 7900
!
 7900 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7911)
!7911 FORMAT('***** ERROR IN DPCHNU--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7912)
!7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7913)ICHAR
!7913 FORMAT('      INPUT CHARACTER = ',A4)
!CCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GO TO 9000
!
 8000 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHNU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICH1,ICH1N,ICH2,ICH2N
 9012   FORMAT('ICH1,ICH1N,ICH2,ICH2N = ',A1,2X,I8,2X,A1,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IFOUND,ICHAR2,ICHARN
 9014   FORMAT('IFOUND,ICHAR2,ICHARN = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHNU
      SUBROUTINE DPCHOF(IHARG,IARGT,ARG,IARG,NUMARG,   &
                        MAXCHA,   &
                        PCHAHO,PCHAVO,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER (HORIZONTAL AND VERTICAL) OFFSET
!              FOR USE IN MULTI-TRACE PLOTS.  THE OFFSET FOR THE
!              CHARACTER FOR THE I-TH TRACE WILL BE PLACED IN THE I-TH
!              ELEMENT OF THE FLOATING POINT VECTORS PCHAHO(.) AND
!              PCHAVO(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --MAXCHA
!     OUTPUT ARGUMENTS--PCHAHO  (A  FLOATING POINT VECTOR
!                       WHOSE I-TH ELEMENT IS THE HORIZONTAL OFFSET
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --PCHAVO  (A  FLOATING POINT VECTOR
!                       WHOSE I-TH ELEMENT IS THE VERTICAL OFFSET
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --PCHAHO = CHARACTER WIDTH
!                     --PCHAVG = VERTICAL GAP BETWEEN CHARACTERS
!                     --PCHAHG = HORIZONTAL GAP BETWEEN CHARACTERS
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1986.
!     UPDATED         --AUGUST    1988. CORRECTED FORMAT STATEMENT
!     UPDATED         --AUGUST    1988. CORRECTED LOOP LOGIC
!     UPDATED         --FEBRUARY  2022. ADD "?" TO EXTRACT CURRENT
!                                       SETTING
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
      DIMENSION IARG(*)
!
      DIMENSION PCHAHO(*)
      DIMENSION PCHAVO(*)
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
      IF(NUMARG.EQ.1.AND.   &
         (IHARG(1).EQ.'OFFS' .OR. IHARG(1).EQ.'DISP'))THEN
        GO TO 1160
      ELSEIF(NUMARG.GE.2.AND.   &
         (IHARG(1).EQ.'OFFS' .OR. IHARG(1).EQ.'DISP'))THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
          DO 1115 I=1,MAXCHA
            PCHAVO(I)=0.0
            PCHAHO(I)=0.0
 1115     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)
 1116       FORMAT('ALL CHARACTER (HORIZONTAL AND VERTICAL OFFSETS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1117)PCHAHO(I),PCHAVO(I)
 1117       FORMAT('    HAVE JUST BEEN SET TO ',2E15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSEIF((IHARG(2).EQ.'?' .AND. IHARG(3).EQ.'ALL') .OR.   &
               (IHARG(2).EQ.'ALL' .AND. IHARG(3).EQ.'?'))THEN
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO 3115 I=1,MAXCHA
              WRITE(ICOUT,1126)I
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1119)PCHAHO(I),PCHAVO(I)
              CALL DPWRST('XXX','BUG ')
 3115       CONTINUE
          ENDIF
          GO TO 2190
        ELSEIF(IARGT(2).EQ.'NUMB' .AND. IHARG(3).EQ.'?')THEN
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=IARG(2)
            IF(I.GE.1 .AND. I.LE.100)THEN
              WRITE(ICOUT,1126)I
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1119)PCHAHO(I),PCHAVO(I)
              CALL DPWRST('XXX','BUG ')
            ELSE
              IERROR='YES'
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1181)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3182)
 3182         FORMAT('      FOR   CHARACTER OFFSET <INDEX> ?')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3183)MAXCHA
 3183         FORMAT('      <INDEX> SHOULD BE BETWEEN 1 AND ',I3,'.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3184)I
 3184         FORMAT('      <INDEX> = ',I6)
              CALL DPWRST('XXX','BUG ')
              GO TO 2199
            ENDIF
          ENDIF
          GO TO 2190
        ELSEIF(IHARG(NUMARG).EQ.'?')THEN
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1119)PCHAHO(I),PCHAVO(I)
 1119       FORMAT('    IS CURRENTLY SET TO ',2E15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSEIF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')THEN
          GO TO 1160
        ELSEIF(NUMARG.EQ.3)THEN
          I=1
          IF(IARGT(2).NE.'NUMB'.OR.IARGT(3).NE.'NUMB')GO TO 1180
          PCHAHO(1)=ARG(2)
          PCHAVO(1)=ARG(3)
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I
 1126       FORMAT('THE HORIZONTAL AND VERTICAL OFFSET FOR ',   &
                   'CHARACTER ',I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1127)PCHAHO(I),PCHAVO(I)
 1127       FORMAT('    HAS JUST BEEN SET TO ',2E15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSEIF(NUMARG.GE.4.AND.IHARG(2).EQ.'ALL')THEN
          I=1
          IF(IARGT(3).NE.'NUMB'.OR.IARGT(4).NE.'NUMB')GO TO 1180
          DO 1135 I=1,MAXCHA
            PCHAHO(I)=ARG(3)
            PCHAVO(I)=ARG(4)
 1135     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1117)PCHAHO(I),PCHAVO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSEIF(NUMARG.GE.4.AND.IHARG(4).EQ.'ALL')THEN
          I=1
          IF(IARGT(2).NE.'NUMB'.OR.IARGT(3).NE.'NUMB')GO TO 1180
          DO 1145 I=1,MAXCHA
            PCHAHO(I)=ARG(2)
            PCHAVO(I)=ARG(3)
 1145     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1117)PCHAHO(I),PCHAVO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2190
        ELSE
          IMAX=NUMARG-1
          IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
          J=0
          DO 1155 I=1,IMAX,2
            IP1=I+1
            IP2=I+2
            IF(IARGT(IP1).NE.'NUMB' .OR. IARGT(IP2).NE.'NUMB')GO TO 1180
            J=J+1
            PCHAHO(J)=ARG(IP1)
            PCHAVO(J)=ARG(IP2)
 1155     CONTINUE
          JMAX=J
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO 1156 I=1,JMAX
              WRITE(ICOUT,1126)I
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1127)PCHAHO(I),PCHAVO(I)
              CALL DPWRST('XXX','BUG ')
 1156       CONTINUE
          ENDIF
          GO TO 2190
        ENDIF
      ELSE
        GO TO 2199
      ENDIF
!
 1160 CONTINUE
      DO 1165 I=1,MAXCHA
        PCHAHO(I)=0.0
        PCHAVO(I)=0.0
 1165 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,1116)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1117)PCHAHO(I),PCHAVO(I)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 2190
!
 1180 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DPCHOF--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      CHARACTER (HORIZONTAL AND VERTICAL) OFFSETS MUST ',   &
             'BE NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('      HOWEVER, THE SPECIFIED CHARACTER OFFSET')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)I
 1184 FORMAT('FOR CHARACTER ',I6,' WAS NON-NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      GO TO 2199
!
 2190 CONTINUE
      IFOUND='YES'
!
 2199 CONTINUE
      RETURN
      END SUBROUTINE DPCHOF
      SUBROUTINE DPCHS3(ICASPL,IDIST,NUMSHA,IFORSW,ICASP3,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        N,XMEAN,XSD,XMIN,XMAX,   &
                        A,B,MINMAX,   &
                        SHAPE1,SHAPE2,SHAPE3,SHAPE4,   &
                        SHAPE5,SHAPE6,SHAPE7,   &
                        KSLOC,KSSCAL,ICAPSW,ICAPTY,   &
                        STATVA,STATCD,PVAL,NCELLS,IDF,IDISFL,MINSZ,   &
                        CDF1,CDF2,CDF3,CDF4,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--PRINT THE OUTPUT FOR THE CHI-SQUARE TEST (GROUPED,
!              UNCENSORED CASE) IN ASCII, HTML, LATEX, OR RTF FORMAT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!         --DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/12
!     ORIGINAL VERSION--DECEMBER  2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL PID(*)
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP3
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IDISFL
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IERROR
!
      CHARACTER*60 IDIST
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      REAL KSLOC
      REAL KSSCAL
!
!---------------------------------------------------------------------
!
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
!
!CCCC INCLUDE 'DPCOST.INC'
!
      CHARACTER*1 IBASLC
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=50)
      CHARACTER*60 ITITLE
      CHARACTER*60  ITITLZ
      CHARACTER*60  ITITL9
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
      DATA ALPHA/   &
       0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.5/
!
!-----START POINT-----------------------------------------------------
!
!
      ISUBN1='DPCH'
      ISUBN2='SQ  '
      IERROR='NO'
      IWRITE='OFF'
      CALL DPCONA(92,IBASLC)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CHS3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPCHS3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ICASPL,IDIST
   72   FORMAT('ICASPL,IDIST = ',A4,2X,A60)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)N,MINMAX,XMIN,XMAX,XMEAN,XSD
   73   FORMAT('N,MINMAX,XMIN,XMAX,XMEAN,XSD = ',2I8,4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,75)STATVA,STATCD,PVAL
   75   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************
!               **   STEP 41--                           **
!               **   WRITE OUT INITIAL HEADER TABLE      **
!               *******************************************
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CHS3')   &
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
      ITITLE='Chi-Square Goodness of Fit Test'
      NCTITL=31
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(ICASP3.EQ.'RAW')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSEIF(ICASP3.EQ.'FREQ')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Bin Frequency Variable: '
        WRITE(ITEXT(ICNT)(25:28),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(29:32),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=32
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Bin Midpoint Variable:  '
        WRITE(ITEXT(ICNT)(25:28),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(29:32),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=32
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSEIF(ICASP3.EQ.'FRE2')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Bin Frequency Variable:       '
        WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(35:38),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=38
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Bin Lower Boundary Variable: '
        WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(35:38),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=38
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Bin Upper Boundary Variable: '
        WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARID(3)(1:4)
        WRITE(ITEXT(ICNT)(35:38),'(A4)')IVARI2(3)(1:4)
        NCTEXT(ICNT)=38
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
      DO 4101 I=1,NREPL
        ICNT=ICNT+1
        ITEXT(ICNT)='Factor Variable  : '
        WRITE(ITEXT(ICNT)(17:17),'(I1)')I
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(I+1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(I+1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=PID(I+1)
        IDIGIT(ICNT)=NUMDIG
 4101 CONTINUE
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The distribution fits the data'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The distribution does not fit the data'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IEND=46
      DO 4111 I=46,1,-1
        IF(IDIST(I:I).NE.' ')THEN
          IEND=I
          GO TO 4119
        ENDIF
 4111 CONTINUE
      IEND=1
 4119 CONTINUE
      CALL EXTBOU(ICASPL,IBOUND)
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)(1:14)='Distribution: '
      ISTRT=15
      ISTOP=15+IEND-1
      ITEXT(ICNT)(ISTRT:ISTOP)=IDIST(1:IEND)
      NCTEXT(ICNT)=ISTOP
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IF(IDISFL.EQ.'CONT')THEN
        IF(IBOUND.EQ.0)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Location Parameter:'
          NCTEXT(ICNT)=19
          AVALUE(ICNT)=KSLOC
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Scale Parameter:'
          NCTEXT(ICNT)=16
          AVALUE(ICNT)=KSSCAL
          IDIGIT(ICNT)=NUMDIG
        ELSE
          ICNT=ICNT+1
          ITEXT(ICNT)='Lower Limit Parameter:'
          NCTEXT(ICNT)=22
          AVALUE(ICNT)=A
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Upper Limit Parameter:'
          NCTEXT(ICNT)=22
          AVALUE(ICNT)=B
          IDIGIT(ICNT)=NUMDIG
        ENDIF
      ENDIF
!
      IF(NUMSHA.GE.1)THEN
        DO 4140 I=1,NUMSHA
          ICNT=ICNT+1
          ITEXT(ICNT)='Shape Parameter  :'
          WRITE(ITEXT(ICNT)(17:17),'(I1)')I
          NCTEXT(ICNT)=18
          IF(I.EQ.1)THEN
            AVALUE(ICNT)=SHAPE1
          ELSEIF(I.EQ.2)THEN
            AVALUE(ICNT)=SHAPE2
          ELSEIF(I.EQ.3)THEN
            AVALUE(ICNT)=SHAPE3
          ELSEIF(I.EQ.4)THEN
            AVALUE(ICNT)=SHAPE4
          ELSEIF(I.EQ.5)THEN
            AVALUE(ICNT)=SHAPE5
          ELSEIF(I.EQ.6)THEN
            AVALUE(ICNT)=SHAPE6
          ELSEIF(I.EQ.7)THEN
            AVALUE(ICNT)=SHAPE7
          ENDIF
          IDIGIT(ICNT)=NUMDIG
 4140   CONTINUE
      ENDIF
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
      ITEXT(ICNT)='Minimum Class Frequency'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=REAL(MINSZ)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Non-Empty Cells'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=REAL(NCELLS)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Degress of Freedom'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=REAL(IDF)
      IDIGIT(ICNT)=0
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
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=XMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample SD:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=XSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Chi-Square Test Statistic Value:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
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
!
      NUMROW=ICNT
      DO 2310 I=1,NUMROW
        NTOT(I)=15
 2310 CONTINUE
!
      ITITLZ=' '
      NCTITZ=0
      IFRST=.TRUE.
      ILAST=.TRUE.
      NCTITZ=0
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
      ITITLE=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
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
      DO 2521 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.2)NTOT(I)=5
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
 2521 CONTINUE
      ITYPCO(2)='ALPH'
      IDIGIT(1)=1
      IDIGIT(3)=3
      DO 2523 I=1,NUMROW
        DO 2525 J=1,NUMCOL
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
            IF(I.GE.2)THEN
              P100=ALPHA(I)/100.0
              CALL CHSPPF(P100,IDF,XPERC)
              XPERC2=RND(XPERC,3)
              AMAT(I,J)=XPERC2
            ELSE
              XPERC=0.0
              XPERC2=RND(XPERC,3)
              AMAT(I,J)=XPERC2
            ENDIF
          ENDIF
 2525   CONTINUE
 2523 CONTINUE
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
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
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
      DO 2821 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
        IF(I.EQ.3)NTOT(I)=17
        NMAX=NMAX+NTOT(I)
!CCCC   IDIGIT(I)=NUMDIG
        IDIGIT(I)=3
        ITYPCO(I)='ALPH'
 2821 CONTINUE
      ITYPCO(3)='NUME'
      IDIGIT(1)=0
      IDIGIT(2)=0
      DO 2823 I=1,NUMROW
        DO 2825 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
 2825   CONTINUE
 2823 CONTINUE
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
      CALL CHSPPF(0.90,IDF,CDF1)
      CALL CHSPPF(0.95,IDF,CDF2)
      CALL CHSPPF(0.975,IDF,CDF3)
      CALL CHSPPF(0.99,IDF,CDF4)
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
!
!     FOR LATEX, WE WANT TO ENSURE THAT TRAILING LINE IS PART
!     OF THE TABLE SO THAT IT WILL BE PRINTED IN THE PROPER PLACE.
!
      IF(ICAPTY.EQ.'LATE')THEN
        ILAST=.FALSE.
      ELSE
        ILAST=.TRUE.
      ENDIF
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CHS3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHS3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHS3
      SUBROUTINE DPCHSY(ICHAR2,ICHARN,IBUG,IFOUND)
!
!     PURPOSE--CONVERT A KEYBOARD SYMBOL
!              (. , ; : ETC.) INTO A NUMERIC VALUE
!              (1 TO 23).
!              (1 TO 24).
!     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
!                              CONTAINING THE HOLLERITH
!                              CHARACTER(S) OF INTEREST.
!     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
!                              CONTAINING THE NUMERIC
!                              DESIGNATION FOR THE
!                              ALPHABETIC CHARACTER.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MARCH     1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MAY       1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUG
      CHARACTER*4 IFOUND
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
!
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHSY')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCHSY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)ICHAR2,IBUGG4,ISUBG4
   59   FORMAT('ICHAR2,IBUGG4,ISUBG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  CONVERT THE CHARACTER       **
!               **********************************
!
      IF(ICHAR2.EQ.'.')GO TO 100
      IF(ICHAR2.EQ.',')GO TO 200
      IF(ICHAR2.EQ.':')GO TO 300
      IF(ICHAR2.EQ.';')GO TO 400
      IF(ICHAR2.EQ.'!')GO TO 500
      IF(ICHAR2.EQ.'?')GO TO 600
      IF(ICHAR2.EQ.'&')GO TO 700
      IF(ICHAR2.EQ.'$')GO TO 800
      IF(ICHAR2.EQ.'/')GO TO 900
      IF(ICHAR2.EQ.'(')GO TO 1000
      IF(ICHAR2.EQ.')')GO TO 1100
      IF(ICHAR2.EQ.'*')GO TO 1200
      IF(ICHAR2.EQ.'-')GO TO 1300
      IF(ICHAR2.EQ.'+')GO TO 1400
      IF(ICHAR2.EQ.'=')GO TO 1500
      IF(ICHAR2.EQ.'''')GO TO 1600
      IF(ICHAR2.EQ.'"')GO TO 1700
      IF(ICHAR2.EQ.'DEGR')GO TO 1800
      IF(ICHAR2.EQ.'NOSP')GO TO 1900
      IF(ICHAR2.EQ.'HASP')GO TO 2000
      IF(ICHAR2.EQ.' ')GO TO 2100
      IF(ICHAR2.EQ.'LAPO')GO TO 2200
      IF(ICHAR2.EQ.'RAPO')GO TO 2300
      IF(ICHAR2.EQ.'|')GO TO 2400
      GO TO 7900
!
  100 CONTINUE
      ICHARN=1
      GO TO 8000
!
  200 CONTINUE
      ICHARN=2
      GO TO 8000
!
  300 CONTINUE
      ICHARN=3
      GO TO 8000
!
  400 CONTINUE
      ICHARN=4
      GO TO 8000
!
  500 CONTINUE
      ICHARN=5
      GO TO 8000
!
  600 CONTINUE
      ICHARN=6
      GO TO 8000
!
  700 CONTINUE
      ICHARN=7
      GO TO 8000
!
  800 CONTINUE
      ICHARN=8
      GO TO 8000
!
  900 CONTINUE
      ICHARN=9
      GO TO 8000
!
 1000 CONTINUE
      ICHARN=10
      GO TO 8000
!
 1100 CONTINUE
      ICHARN=11
      GO TO 8000
!
 1200 CONTINUE
      ICHARN=12
      GO TO 8000
!
 1300 CONTINUE
      ICHARN=13
      GO TO 8000
!
 1400 CONTINUE
      ICHARN=14
      GO TO 8000
!
 1500 CONTINUE
      ICHARN=15
      GO TO 8000
!
 1600 CONTINUE
      ICHARN=16
      GO TO 8000
!
 1700 CONTINUE
      ICHARN=17
      GO TO 8000
!
 1800 CONTINUE
      ICHARN=18
      GO TO 8000
!
 1900 CONTINUE
      ICHARN=19
      GO TO 8000
!
 2000 CONTINUE
      ICHARN=20
      GO TO 8000
!
 2100 CONTINUE
      ICHARN=21
      GO TO 8000
!
 2200 CONTINUE
      ICHARN=22
      GO TO 8000
!
 2300 CONTINUE
      ICHARN=23
      GO TO 8000
!
 2400 CONTINUE
      ICHARN=24
      GO TO 8000
!
 7900 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7911)
!7911 FORMAT('***** ERROR IN DPCHSY--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7912)
!7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,7913)ICHAR2
!7913 FORMAT('      INPUT CHARACTER = ',A4)
!CCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GO TO 9000
!
 8000 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUG.EQ.'ON' .OR. ISUBG4.EQ.'CHSY')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHSY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,ICHAR2,ICHARN
 9013   FORMAT('IFOUND,ICHAR2,ICHARN = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHSY
      SUBROUTINE DPCHSZ(PDEFHE,MAXCHA,   &
      PCHAHE,PCHAWI,PCHAVG,PCHAHG,   &
      IBUGP2,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER SIZES FOR USE IN MULTI-TRACE PLOTS.
!              THE SIZE FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE FLOATING POINT
!              VECTOR PCHAHE(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --PDEFHE
!                     --MAXCHA
!     OUTPUT ARGUMENTS--PCHAHE  (A  FLOATING POINT VECTOR
!                       WHOSE I-TH ELEMENT IS THE SIZE (= HEIGHT)
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --PCHAWI = CHARACTER WIDTH
!                     --PCHAVG = VERTICAL GAP BETWEEN CHARACTERS
!                     --PCHAHG = HORIZONTAL GAP BETWEEN CHARACTERS
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1982.
!     UPDATED         --JANUARY   1995. ALLOW ? AS ARGUMENT (FOR HELP)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC CHARACTER*4 IHARG          DECEMBER 1986
!CCCC CHARACTER*4 IARGT          DECEMBER 1986
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEQ
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
!CCCC DIMENSION IHARG(*)          DECEMBER 1986
!CCCC DIMENSION IARGT(*)          DECEMBER 1986
!CCCC DIMENSION IARG(*)          DECEMBER 1986
!CCCC DIMENSION ARG(*)          DECEMBER 1986
!
      DIMENSION PCHAHE(*)
      DIMENSION PCHAWI(*)
      DIMENSION PCHAVG(*)
      DIMENSION PCHAHG(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCH'
      ISUBN2='SZ  '
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'SIZE'.AND.   &
      IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')GO TO 2110
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'HEIG'.AND.   &
      IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')GO TO 2110
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'SIZE'.AND.   &
      IHARG(3).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')GO TO 2110
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'HEIG'.AND.   &
      IHARG(3).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')GO TO 2110
!
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'SIZE')GO TO 1160
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'HEIG')GO TO 1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SIZE')GO TO 1105
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'HEIG')GO TO 1105
      GO TO 9000
!
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1110
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
!CCCC THE FOLLOWING LINE WAS ADDED    JANUARY 1995
      IF(IHARG(NUMARG).EQ.'?')GO TO 1200
!
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GO TO 1160
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GO TO 1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GO TO 1140
!
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'AUTO')GO TO 3000
!
      GO TO 1150
!
 1110 CONTINUE
      DO 1115 I=1,MAXCHA
      PCHAHE(I)=PDEFHE
 1115 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAHE(I)
 1116 FORMAT('ALL CHARACTER SIZES HAVE JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 8000
!
 1120 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GO TO 1180
      PCHAHE(1)=ARG(2)
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,PCHAHE(I)
 1126 FORMAT('THE SIZE FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 8000
!
 1130 CONTINUE
      I=1
      IF(IARGT(3).NE.'NUMB')GO TO 1180
      DO 1135 I=1,MAXCHA
      PCHAHE(I)=ARG(3)
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAHE(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 8000
!
 1140 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GO TO 1180
      DO 1145 I=1,MAXCHA
      PCHAHE(I)=ARG(2)
 1145 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAHE(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 8000
!
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO 1155 I=1,IMAX
      IP1=I+1
      IF(IARGT(IP1).NE.'NUMB')GO TO 1180
      PCHAHE(I)=ARG(IP1)
 1155 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1156 I=1,IMAX
      WRITE(ICOUT,1126)I,PCHAHE(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GO TO 8000
!
 1160 CONTINUE
      DO 1165 I=1,MAXCHA
      PCHAHE(I)=PDEFHE
 1165 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAHE(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GO TO 8000
!
 1180 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DPCHSZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('CHARACTER SIZES MUST BE NUMERIC;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('HOWEVER, THE SPECIFIED CHARACTER SIZE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)I
 1184 FORMAT('FOR CHARACTER ',I6,' WAS NON-NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!CCCC THE FOLLOWING SECTION WAS ADDED    JANUARY 1995
 1200 CONTINUE
      IFOUND='YES'
      IF(IFEEDB.EQ.'OFF')GO TO 1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1226)I,PCHAHE(I)
 1226 FORMAT('THE CURRENT SIZE FOR CHARACTER ',I6,' IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1227)I,PDEFHE
 1227 FORMAT('THE DEFAULT SIZE FOR CHARACTER ',I6,' IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GO TO 9000
!
 2110 CONTINUE
      IMAX=24
      PCHAHE(1)=2.0
      PCHAHE(2)=2.0
      PCHAHE(3)=2.0
      PCHAHE(4)=2.0
      PCHAHE(5)=2.0
      PCHAHE(6)=2.0
      PCHAHE(7)=2.0
      PCHAHE(8)=2.0
      PCHAHE(9)=2.0
      PCHAHE(10)=2.0
      PCHAHE(11)=2.0
      PCHAHE(12)=2.0
      PCHAHE(13)=2.0
      PCHAHE(14)=2.0
      PCHAHE(15)=2.0
      PCHAHE(16)=2.0
      PCHAHE(17)=2.0
      PCHAHE(18)=2.0
      PCHAHE(19)=2.0
      PCHAHE(20)=2.0
      PCHAHE(21)=3.0
      PCHAHE(22)=2.0
      PCHAHE(23)=2.0
      PCHAHE(24)=3.0
      GO TO 2170
!
 2170 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 2179
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 2175 I=1,IMAX
      WRITE(ICOUT,2176)I,PCHAHE(I)
 2176 FORMAT('THE SIZE FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 2175 CONTINUE
 2179 CONTINUE
      GO TO 8000
!
!               ***********************************************************
!               **  STEP 30--                                            **
!               **  TREAT THE   CHARACTER SIZE AUTOMATIC <VARIABLE>  CASE **
!               ***********************************************************
!
 3000 CONTINUE
!
!               ********************************************
!               **  STEP 31--                             **
!               **  CHECK THE VALIDITY OF ARGUMENT 3      **
!               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
!               ********************************************
!
      ISTEPN='31'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(3)
      IHLEF2=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
!
!               *****************************************
!               **  STEP 32--                          **
!               **  CHECK TO SEE THE TYPE CASE--       **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='32'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 3290
      DO 3200 J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GO TO 3210
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GO TO 3210
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GO TO 3220
 3200 CONTINUE
      GO TO 3290
 3210 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GO TO 3290
 3220 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 3290
 3290 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 3295
      WRITE(ICOUT,3291)NUMARG,ILOCQ
 3291 FORMAT('NUMARG,ILOCQ = ',2I8)
      CALL DPWRST('XXX','BUG ')
 3295 CONTINUE
!
!               *********************************************
!               **  STEP 33--                              **
!               **  TEMPORARILY FORM THE VARIABLE Y(.)     **
!               **  WHICH WILL HOLD THE RESPONSE VARIABLE. **
!               **  FORM THIS VARIABLE BY                  **
!               **  BRANCHING TO THE APPROPRIATE SUBCASE   **
!               **  (FULL, SUBSET, OR FOR).                **
!               *********************************************
!
      ISTEPN='33'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL')GO TO 3310
      IF(ICASEQ.EQ.'SUBS')GO TO 3320
      IF(ICASEQ.EQ.'FOR')GO TO 3330
!
 3310 CONTINUE
      DO 3315 I=1,NLEFT
      ISUB(I)=1
 3315 CONTINUE
      NQ=NLEFT
      GO TO 3350
!
 3320 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GO TO 3350
!
 3330 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GO TO 3350
!
 3350 CONTINUE
      MINN2=1
      IF(NQ.GE.MINN2)GO TO 3360
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3351)
 3351 FORMAT('***** ERROR IN DPCHSZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3352)
 3352 FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',   &
      'EXTRACTED,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3353)IHLEFT,IHLEF2
 3353 FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING',   &
      'FROM VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3354)
 3354 FORMAT('      (FOR WHICH CHARACTER SIZES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3355)
 3355 FORMAT('      ARE TO BE GENERATED)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3356)MINN2
 3356 FORMAT('      MUST BE ',I8,' OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3357)
 3357 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3358)
 3358 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3359)(IANS(I),I=1,IWIDTH)
 3359 FORMAT('      ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
 3360 CONTINUE
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      J=0
      IMAX=NLEFT
      IF(NQ.LT.NLEFT)IMAX=NQ
      DO 3370 I=1,IMAX
      IF(ISUB(I).EQ.0)GO TO 3370
      J=J+1
!
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)Y(J)=V(IJ)
      IF(ICOLL.EQ.MAXCP1)Y(J)=PRED(I)
      IF(ICOLL.EQ.MAXCP2)Y(J)=RES(I)
      IF(ICOLL.EQ.MAXCP3)Y(J)=YPLOT(I)
      IF(ICOLL.EQ.MAXCP4)Y(J)=XPLOT(I)
      IF(ICOLL.EQ.MAXCP5)Y(J)=X2PLOT(I)
      IF(ICOLL.EQ.MAXCP6)Y(J)=TAGPLO(I)
!
 3370 CONTINUE
      NS=J
      NY=J
!
!               *****************************************
!               **  STEP 34--                          **
!               **  EXTRACT THE DISTINCT VALUES        **
!               **  FROM THE TARGET VARIABLE Y(.)   .  **
!               **  STORE THEM IN X(.)   .             **
!               *****************************************
!
      IWRITE='OFF'
      CALL DISTIN(Y,NY,IWRITE,X,NX,IBUGP2,IERROR)
!
!               ***********************************
!               **  STEP 35--                    **
!               **  SORT THESE DISTINCT VALUES   **
!               **  (IN PLACE).                  **
!               ***********************************
!
      CALL SORT(X,NX,X)
!
!               ******************************************
!               **  STEP 36--                           **
!               **  COPY    THE NUMERIC VALUES IN X(.)  **
!               **  INTO INDIVIDUAL ELEMENTS            **
!               **  OF PCHAHE(.)                        **
!               **  NOTE--MAX NUMBER OF VALUES  = 100   **
!               ******************************************
!
      IMAX=NX
      IF(IMAX.GT.MAXCHA)IMAX=MAXCHA
      DO 3650 I=1,IMAX
      PCHAHE(I)=X(I)
 3650 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 3679
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 3675 I=1,IMAX
      WRITE(ICOUT,3676)I,PCHAHE(I)
 3676 FORMAT('CHARACTER SIZE ',I6,' HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 3675 CONTINUE
 3679 CONTINUE
      GO TO 8000
!
 8000 CONTINUE
      IFOUND='YES'
      DO 8010 I=1,MAXCHA
      PCHAWI(I)=PCHAHE(I)*0.5
      PCHAVG(I)=PCHAHE(I)*0.5
      PCHAHG(I)=PCHAWI(I)*0.5
 8010 CONTINUE
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPCHAR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2
 9012 FORMAT('IBUGP2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IFOUND,IERROR
 9013 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)PDEFHE,IMAX
 9014 FORMAT('PDEFHE,IMAX = ',E15.7,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)NY
 9021 FORMAT('NY = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NY.LE.0)GO TO 9022
      DO 9023 I=1,NY
      WRITE(ICOUT,9024)I,Y(I)
 9024 FORMAT('I,Y(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9023 CONTINUE
 9022 CONTINUE
      WRITE(ICOUT,9031)NX
 9031 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NX.LE.0)GO TO 9032
      DO 9033 I=1,NX
      WRITE(ICOUT,9034)I,X(I)
 9034 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9033 CONTINUE
 9032 CONTINUE
      WRITE(ICOUT,9041)MAXCHA
 9041 FORMAT('MAXCHA = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NX.LE.0)GO TO 9042
      DO 9043 I=1,NX
      WRITE(ICOUT,9044)I,PCHAHE(I),PCHAWI(I),PCHAVG(I),PCHAHG(I)
 9044 FORMAT('I,PCHAHE(I),PCHAWI(I),PCHAVG(I),PCHAHG(I) = ',I8,2X,   &
      4E15.7)
      CALL DPWRST('XXX','BUG ')
 9043 CONTINUE
 9042 CONTINUE
 9090 CONTINUE
      RETURN
      END SUBROUTINE DPCHSZ
      SUBROUTINE DPCHTH(IHARG,ARG,NUMARG,PDEFTH,MAXCHA,PCHATH,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER THICKNESSS FOR USE IN MULTI-TRACE PLOTS.
!              THE THICKNESS FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR PCHATH(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --ARG    (A REAL VECTOR)
!                     --NUMARG
!                     --PDEFTH
!                     --MAXCHA
!     OUTPUT ARGUMENTS--PCHATH  (A  REAL VECTOR
!                       WHOSE I-TH ELEMENT IS THE THICKNESS
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
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
      DIMENSION PCHATH(*)
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
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'THIC')GO TO 1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'THIC')GO TO 1105
      GO TO 1199
!
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1110
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
!
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GO TO 1160
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GO TO 1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GO TO 1140
!
      GO TO 1150
!
 1110 CONTINUE
      DO 1115 I=1,MAXCHA
      PCHATH(I)=PDEFTH
 1115 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHATH(I)
 1116 FORMAT('ALL CHARACTER THICKNESSS HAVE JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 1190
!
 1120 CONTINUE
      PCHATH(1)=ARG(2)
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,PCHATH(I)
 1126 FORMAT('THE THICKNESS FOR CHARACTER ',I6,' HAS JUST BEEN ',   &
      'SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 1190
!
 1130 CONTINUE
      DO 1135 I=1,MAXCHA
      PCHATH(I)=ARG(3)
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHATH(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 1190
!
 1140 CONTINUE
      DO 1145 I=1,MAXCHA
      PCHATH(I)=ARG(2)
 1145 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHATH(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 1190
!
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO 1155 I=1,IMAX
      IP1=I+1
      PCHATH(I)=ARG(IP1)
 1155 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1156 I=1,IMAX
      WRITE(ICOUT,1126)I,PCHATH(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GO TO 1190
!
 1160 CONTINUE
      DO 1165 I=1,MAXCHA
      PCHATH(I)=PDEFTH
 1165 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHATH(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GO TO 1190
!
 1190 CONTINUE
      IFOUND='YES'
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPCHTH
      SUBROUTINE DPCHUN(IHARG,NUMARG,MAXCHA,ICHATY,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER UNITS (DATA OR SCREEN) FOR USE IN
!              MULTI-TRACE PLOTS.  THE UNITS FOR THE CHARACTER FOR THE
!              I-TH TRACE WILL BE PLACED IN THE I-TH ELEMENT OF THE
!              HOLLERITH VECTOR ICHATY(.).
!
!              THE UNITS ARE SPECIFIED AS:
!
!                 DD     => X AXIS = DATA UNITS,   Y AXIS = DATA UNITS
!                 DS     => X AXIS = DATA UNITS,   Y AXIS = SCREEN UNITS
!                 SD     => X AXIS = SCREEN UNITS, Y AXIS = DATA UNITS
!                 SS     => X AXIS = SCREEN UNITS, Y AXIS = SCREEN UNITS
!                 DATA   => X AXIS = DATA UNITS,   Y AXIS = DATA UNITS
!                 SCREEN => X AXIS = SCREEN UNITS, Y AXIS = SCREEN UNITS
!
!              THE DEFAULT IS DATA UNITS FOR BOTH AXES.
!
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --MAXCHA
!     OUTPUT ARGUMENTS--ICHATY  (A  HOLLERITH VECTOR WHOSE I-TH ELEMENT
!                       IS THE UNITS FOR THE CHARACTER ASSIGNED TO THE
!                       I-TH TRACE IN A MULTI-TRACE PLOT.
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
!     VERSION NUMBER--2018/01
!     ORIGINAL VERSION--JANUARY   2018.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 ICHATY
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IDEFTY
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ICHATY(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
      IERROR='NO'
      IDEFTY='DD'
!
      IF((NUMARG.EQ.1.AND.IHARG(1).EQ.'UNIT') .OR.   &
         (NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL'))THEN
        DO 1165 I=1,MAXCHA
          ICHATY(I)=IDEFTY
 1165   CONTINUE
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHATY(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'UNIT')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
          DO 1115 I=1,MAXCHA
            ICHATY(I)=IDEFTY
 1115     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHATY(I)
 1116       FORMAT('ALL CHARACTER UNITS HAVE JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.EQ.2)THEN
          ICHATY(1)=IHARG(2)
          IF(ICHATY(1).EQ.'SCRE' .OR. ICHATY(1).EQ.'SS  ')THEN
            ICHATY(1)='SS  '
          ELSEIF(ICHATY(1).EQ.'DATA' .OR. ICHATY(1).EQ.'DD  ')THEN
            ICHATY(1)='DD  '
          ELSE
            IF(ICHATY(1).NE.'DS  ' .AND. ICHATY(1).NE.'SD  ')THEN
              ICHATY(1)='DD'
            ENDIF
          ENDIF
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I,ICHATY(I)
 1126       FORMAT('THE UNITS FOR CHARACTER ',I6,   &
                   ' HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')THEN
          DO 1135 I=1,MAXCHA
            ICHATY(I)=IHARG(3)
            IF(ICHATY(I).EQ.'SCRE' .OR. ICHATY(I).EQ.'SS  ')THEN
              ICHATY(I)='SS  '
            ELSEIF(ICHATY(I).EQ.'DATA' .OR. ICHATY(I).EQ.'DD  ')THEN
              ICHATY(I)='DD  '
            ELSE
              IF(ICHATY(I).NE.'DS  ' .AND. ICHATY(I).NE.'SD  ')THEN
                ICHATY(I)='DD'
              ENDIF
            ENDIF
 1135     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHATY(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')THEN
          DO 1145 I=1,MAXCHA
            ICHATY(I)=IHARG(2)
            IF(ICHATY(I).EQ.'SCRE' .OR. ICHATY(I).EQ.'SS  ')THEN
              ICHATY(I)='SS  '
            ELSEIF(ICHATY(I).EQ.'DATA' .OR. ICHATY(I).EQ.'DD  ')THEN
              ICHATY(I)='DD  '
            ELSE
              IF(ICHATY(I).NE.'DS  ' .AND. ICHATY(I).NE.'SD  ')THEN
                ICHATY(I)='DD'
              ENDIF
            ENDIF
 1145     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHATY(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ELSE
          IMAX=NUMARG-1
          IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
          DO 1155 I=1,IMAX
            IP1=I+1
            ICHATY(I)=IHARG(IP1)
            IF(ICHATY(I).EQ.'SCRE' .OR. ICHATY(I).EQ.'SS  ')THEN
              ICHATY(I)='SS  '
            ELSEIF(ICHATY(I).EQ.'DATA' .OR. ICHATY(I).EQ.'DD  ')THEN
              ICHATY(I)='DD  '
            ELSE
              IF(ICHATY(I).NE.'DS  ' .AND. ICHATY(I).NE.'SD  ')THEN
                ICHATY(I)='DD'
              ENDIF
            ENDIF
 1155     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO 1156 I=1,IMAX
              WRITE(ICOUT,1126)I,ICHATY(I)
              CALL DPWRST('XXX','BUG ')
 1156       CONTINUE
          ENDIF
        ENDIF
      ELSE
        IFOUND='NO'
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHUN
      SUBROUTINE DPCHIS(MAXNXT,   &
                        ICASAN,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--COMPUTE THE CHI-SQUARE TEST FOR INDEPENDENCE
!     EXAMPLE--CHI-SQUARE INDEPENDENCE TEST Y1 Y2
!            --CHI-SQUARE INDEPENDENCE TEST N11 N21 N12 N22
!            --CHI-SQUARE INDEPENDENCE TEST M
!     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC
!                STATISTICS", THIRD EDITION, WILEY, PP. 204-216.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/3
!     ORIGINAL VERSION--MARCH     2007.
!     UPDATED         --JANUARY   2011. USE DPPARS, DPPAR3, DPPAR6
!     UPDATED         --JUNE      2019. TWEAK TO SCRATCH STORAGE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
!
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASEQ
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
      CHARACTER*4 ICASE
!
      CHARACTER*40 INAME
!
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
      PARAMETER(MAXLEV=1000)
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
!
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
      REAL TEMP3(MAXOBV)
      REAL XIDTEM(MAXOBV)
      REAL XIDTE2(MAXOBV)
      REAL XMAT(MAXLEV,MAXLEV)
!
      DOUBLE PRECISION ROWTOT(MAXOBV)
      DOUBLE PRECISION COLTOT(MAXOBV)
!
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),XMAT(1,1))
!
      EQUIVALENCE (DGARBG(IDGAR1),ROWTOT(1))
      EQUIVALENCE (DGARBG(IDGAR2),COLTOT(1))
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCH'
      ISUBN2='IS  '
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
      N11=(-999)
      N21=(-999)
      N12=(-999)
      N22=(-999)
      AN11=0.0
      AN12=0.0
      AN21=0.0
      AN22=0.0
!
      NS1=(-999)
      NS2=(-999)
      NS3=(-999)
      NS4=(-999)
!
      ICASE='PARA'
      MINN2=2
!
      IFOUND='YES'
      ICASEQ='UNKN'
!
!               ***************************************************
!               **  TREAT THE CHI-SQUARE INDEPENDENCE TEST CASE  **
!               ***************************************************
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCHIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ICASAN
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ICASAN = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)MAXNXT,NUMARG
   55   FORMAT('MAXNXT,NUMARG = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 59 I=1,NUMARG
          WRITE(ICOUT,57)I,IHARG(I),IHARG2(I),ARG(I)
   57     FORMAT('I,IHARG(I),IHARG2(I),ARG(I) = ',I5,A4,A4,G15.7)
   59   CONTINUE
      ENDIF
!
!               *********************************
!               **  STEP 4--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='CHI-SQUARE INDEPENDENCE TEST'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=9
      IFLAGP=9
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
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
                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')THEN
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
!               ***********************************
!               **  STEP 22--                    **
!               **  CHECK FOR PROPER VALUES FOR  **
!               **  INPUT PARAMETERS             **
!               ***********************************
!
      IF(IVARTY(1).EQ.'PARA' .OR. IVARTY(1).EQ.'NUMB')THEN
        N11=INT(PVAR(1)+0.5)
        N21=INT(PVAR(2)+0.5)
        N12=INT(PVAR(3)+0.5)
        N22=INT(PVAR(4)+0.5)
        AN11=REAL(N11)
        AN21=REAL(N21)
        AN12=REAL(N12)
        AN22=REAL(N22)
        ICASE='PARA'
!
        ISTEPN='22'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(N11.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
 2201     FORMAT('***** ERROR FROM CHI-SQUARE INDEPENDENCE TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2203)
 2203     FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = THE ',   &
                 'NUMBER OF SUCCESSES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2204)
 2204     FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2205)N11
 2205     FORMAT('      N11 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
!
        ELSEIF(N21.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2303)
 2303     FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = THE ',   &
                 'NUMBER OF FAILURES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2304)
 2304     FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2305)N21
 2305     FORMAT('      N21 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
!
        ELSEIF(N12.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2403)
 2403     FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = THE ',   &
                 'NUMBER OF SUCCESSES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2404)
 2404     FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2405)N12
 2405     FORMAT('      N12 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
!
        ELSEIF(N22.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2503)
 2503     FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = THE ',   &
                 'NUMBER OF FAILURES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2504)
 2504     FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2505)N22
 2505     FORMAT('      N22 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
      ELSEIF(IVARTY(1).EQ.'VARI')THEN
!
        ICASE='VARI'
        ICOL=1
        IF(NUMVAR.GT.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2603)
 2603     FORMAT('      MORE THAN TWO VARIABLES GIVEN.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2605)NUMVAR
 2605     FORMAT('      THE NUMBER OF VARIABLES GIVEN  = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y,X,X,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NS1=NLOCAL
        NS2=NLOCA2
!
      ELSEIF(IVARTY(1).EQ.'MATR')THEN
        ICASE='MATR'
        ICOL=1
        NUMVAR=1
        CALL DPPAR6(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    XMAT,MAXLEV,NROW,NCOL,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        ICASE='TABL'
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***********************************
!               **  STEP 61--                    **
!               **  COMPUTE THE CHI-SQUARE TEST  **
!               ***********************************
!
      ISTEPN='61'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CHIS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6111)
 6111   FORMAT('***** FROM DPCHIS--READY TO COMPUTE TEST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6112)AN11,AN21,AN12,AN22
 6112   FORMAT('AN11,AN21,AN12,AN22 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL DPCHI2(Y,NS1,X,NS2,   &
                  AN11,AN21,AN12,AN22,   &
                  XMAT,MAXLEV,NROW,NCOL,   &
                  XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,MAXOBW,   &
                  ROWTOT,COLTOT,   &
                  ICASE,   &
                  ICAPSW,ICAPTY,IFORSW,   &
                  STATVA,CDF,STATV2,CDF2,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 62--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='62'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUBN0='CHIS'
!
      IH='STAT'
      IH2='VAL '
      VALUE0=STATVA
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='STAT'
      IH2='CDF '
      VALUE0=CDF
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='STAT'
      IH2='VAL2'
      VALUE0=STATV2
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='STAT'
      IH2='CDF2'
      VALUE0=CDF2
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA2,IBUGA3
 9012   FORMAT('IBUGA2,IBUGA3 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IERROR
 9016   FORMAT('IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHIS
      SUBROUTINE DPCHI2(Y1,N1,Y2,N2,   &
                        AN11,AN21,AN12,AN22,   &
                        XMAT,MAXLEV,NROW,NCOL,   &
                        XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,MAXNXT,   &
                        ROWTOT,COLTOT,   &
                        ICASE,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        STATVA,CDF,STATV2,CDF2,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--PERFORM A CHI-SQUARE TEST FOR INDEPENDENCE.
!              THE INPUT CAN BE ENTERED IN THE FOLLOWING WAYS:
!
!              1) THE COMMON CASE OF A 2X2 TABLE CAN BE
!                 ENTERED AS 4 PARAMETERS:
!
!                    N11 = NUMBER OF SUCCESSES FOR VARIABLE 1
!                    N21 = NUMBER OF FAILURES  FOR VARIABLE 1
!                    N12 = NUMBER OF SUCCESSES FOR VARIABLE 2
!                    N22 = NUMBER OF SUCCESSES FOR VARIABLE 2
!
!              2) AS RAW DATA, THAT IS TWO VARIABLES.  A
!                 CROSS-TABULATION IS PERFORMED TO GENERATE
!                 AN RXC TABLE OF COUNTS.
!
!              3) AS A MATRIX, I.E., THE RXC TABLE HAS ALREADY
!                 BEEN GENERATED.
!
!              THE CHI-SQUARE TEST CAN THEN BE COMPUTED AS:
!
!                 CHI-SQUARE = SUM[(f - F)**2/F
!
!              WHERE THE SUMMATION IS OVER ALL CELLS IN THE
!              TABLE AND WHERE
!
!                 f   = OBSERVED FFEQUENCY OF THE CELL
!                 F   = EXPECTED FREQUENCY OF THE CELL
!                     = (ROW TOTAL)*(COLUMN TOTAL)/(GRAND TOTAL)
!
!              SOME ANALYSTS PREFER TO USE THE YATES CONTINUITY
!              CORRECTION.  IN THIS CORRECTON, 0.5 IS ADDED TO
!              EACH CELL.  DATAPLOT WILL GENERATE THE TEST STATISTIC
!              FOR BOTH THE UNCORRECTED AND CORRECTED CASES.
!
!     EXAMPLE--CHI-SQUARE INDEPENDENCE TEST Y1 Y2
!            --CHI-SQUARE INDEPENDENCE TEST N11 N21 N12 N22
!            --CHI-SQUARE INDEPENDENCE TEST M
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGYU LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/3
!     ORIGINAL VERSION--MARCH     2007.
!     UPDATED         --JANUARY   2011. USE DPAUFI TO OPEN/CLOSE
!                                       AUXILLARY FILES
!     UPDATED         --JANUARY   2011. USE DPDTA1, DPDT5B TO PRINT
!                                       TABLES
!     UPDATED         --APRIL     2019. USER CAN SPECIFY NUMBER OF
!                                       DECIMAL POINTS FOR AUXILLARY
!                                       FILES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 ICASE
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
!
      CHARACTER*4 IWRITE
      CHARACTER*6 ICONC1
      CHARACTER*6 ICONC2
      CHARACTER*6 ICONC3
      CHARACTER*6 ICONC4
      CHARACTER*6 ICONC5
      CHARACTER*6 ICONC6
      CHARACTER*6 KCONC1
      CHARACTER*6 KCONC2
      CHARACTER*6 KCONC3
      CHARACTER*6 KCONC4
      CHARACTER*6 KCONC5
      CHARACTER*6 KCONC6
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 IOP
      CHARACTER*20 IFORMT
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
!
      DIMENSION XMAT(MAXLEV,MAXLEV)
!
      DOUBLE PRECISION ROWTOT(*)
      DOUBLE PRECISION COLTOT(*)
!
      PARAMETER (NUMALP=6)
!CCCC DIMENSION SIGVAL(NUMALP)
!CCCC DIMENSION ALOWCL(NUMALP)
!CCCC DIMENSION AUPPCL(NUMALP)
!CCCC DIMENSION ALOWC2(NUMALP)
!CCCC DIMENSION AUPPC2(NUMALP)
!
      DOUBLE PRECISION GTOTAL
      DOUBLE PRECISION VALTMP
      DOUBLE PRECISION EXP
      DOUBLE PRECISION CHISQ1
      DOUBLE PRECISION CHISQ2
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
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
      INCLUDE 'DPCOST.INC'
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA SIGVAL /0.50, 0.80, 0.90, 0.95, 0.975, 0.99/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCH'
      ISUBN2='I2  '
      IERROR='NO'
      IWRITE='NO'
!
      ICONC1='ACCEPT'
      ICONC2='ACCEPT'
      ICONC3='ACCEPT'
      ICONC4='ACCEPT'
      ICONC5='ACCEPT'
      ICONC6='ACCEPT'
!
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
      WRITE(IOUNI1,41)
   41 FORMAT(5X,'ROW  COLUMN',9X,'ROWTOT',9X,'COLTOT',6X,'EXPECTED',   &
            8X,'OBSERVED')
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CHI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPCHI2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASE,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,ICASE,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        IF(ICASE.EQ.'VARI')THEN
          WRITE(ICOUT,55)N1
   55     FORMAT('N1 = ',I8)
          CALL DPWRST('XXX','WRIT')
          DO 56 I=1,N1
            WRITE(ICOUT,57)I,Y1(I)
   57       FORMAT('I,Y1(I) = ',I8,E15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
          WRITE(ICOUT,65)N2
   65     FORMAT('N2 = ',I8)
          CALL DPWRST('XXX','WRIT')
          DO 66 I=1,N2
            WRITE(ICOUT,67)I,Y2(I)
   67       FORMAT('I,Y2(I) = ',I8,E15.7)
            CALL DPWRST('XXX','WRIT')
   66     CONTINUE
        ELSE
          WRITE(ICOUT,75)AN11,AN21,AN12,AN22
   75     FORMAT('AN11,AN21,AN12,AN22 = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
      ENDIF
                                                                                                                                  
!               ********************************************
!               **  STEP 0--                              **
!               **  BRANCH TO APPROPRIATE CASE (PARAMETER **
!               **  OR VARIABLE)                          **
!               ********************************************
!
      ISTEPN='00'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASE.EQ.'PARA')GO TO 1000
      IF(ICASE.EQ.'VARI')GO TO 2000
      IF(ICASE.EQ.'TABL')GO TO 3000
!
!               ********************************************
!               **  STEP 11--                             **
!               **  PARAMETER CASE                        **
!               ********************************************
!
 1000 CONTINUE
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ********************************************
!               **  STEP 12--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      N11=INT(AN11+0.5)
      N21=INT(AN21+0.5)
      N12=INT(AN12+0.5)
      N22=INT(AN22+0.5)
!
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N11.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
 1201   FORMAT('***** ERROR FROM THE CHI-SQUARE INDEPENDENCE TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1203)
 1203   FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = THE ',   &
               'NUMBER OF SUCCESSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1204)
 1204   FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1205)N11
 1205   FORMAT('      N11 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N21.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1303)
 1303   FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = THE ',   &
               'NUMBER OF FAILURES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1304)
 1304   FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1305)N21
 1305   FORMAT('      N21 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N12.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1403)
 1403   FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = THE ',   &
               'NUMBER OF SUCCESSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1404)
 1404   FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1405)N12
 1405   FORMAT('      N12 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N22.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1503)
 1503   FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = THE ',   &
               'NUMBER OF FAILURES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1504)
 1504   FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1505)N22
 1505   FORMAT('      N22 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************
!               **  STEP 12--                             **
!               **  COMPUTE THE CHI-SQUARE TEST           **
!               ********************************************
!
!
      ROWTOT(1)=DBLE(AN11 + AN12)
      ROWTOT(2)=DBLE(AN21 + AN22)
      COLTOT(1)=DBLE(AN11 + AN21)
      COLTOT(2)=DBLE(AN12 + AN22)
      GTOTAL=ROWTOT(1) + ROWTOT(2)
      TEMP1(1)=AN11
      TEMP1(2)=AN21
      TEMP1(3)=AN12
      TEMP1(4)=AN22
      N1=N11 + N21
      N2=N12 + N22
      AN1=REAL(N1)
      AN2=REAL(N2)
!
      IFORMT='(2I8,4E15.7)'
      IF(IAUXDP.NE.7)THEN
        IFORMT=' '
        IF(IAUXDP.LE.9)THEN
          IFORMT='(2I8,4Exx.x)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(8:9),'(I2)')ITOT
          WRITE(IFORMT(11:11),'(I1)')IAUXDP
        ELSE
          IFORMT='(2I8,4Exx.xx)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(8:9),'(I2)')ITOT
          WRITE(IFORMT(11:12),'(I2)')IAUXDP
        ENDIF
      ENDIF
!
      IINDX=0
      CHISQ1=0.0D0
      CHISQ2=0.0D0
      DO 1600 J=1,2
        DO 1610 I=1,2
          IINDX=IINDX+1
          EXP=ROWTOT(I)*COLTOT(J)/GTOTAL
          VALTMP=DBLE(TEMP1(IINDX))
          CHISQ1=CHISQ1 + (VALTMP - EXP)**2/EXP
          VALTMP=DABS(DBLE(TEMP1(IINDX)) - EXP)
          VALTMP=(VALTMP - 0.5D0)**2/EXP
          CHISQ2=CHISQ2 + VALTMP
!
          WRITE(IOUNI1,IFORMT)I,J,ROWTOT(I),COLTOT(J),EXP,TEMP1(IINDX)
!1605     FORMAT(I8,I8,4E15.7)
!
 1610   CONTINUE
 1600 CONTINUE
      NROW=2
      NCOL=2
!
      GO TO 4000
!
!               ********************************************
!               **  STEP 20--                             **
!               **  VARIABLE  CASE                        **
!               ********************************************
!
 2000 CONTINUE
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2101)
 2101   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 1 ',   &
               'IS NON-POSITIVE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N1
 2103   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2106)
 2106   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 2 ',   &
               'IS NON-POSITIVE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 2.2--                                      **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
!               **  FOR THE GROUP VARIABLES (Y1, Y2).               **
!               ******************************************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(Y1,N1,IWRITE,XIDTEM,NUMSE1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(Y2,N2,IWRITE,XIDTE2,NUMSE2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
!
      IF(NUMSE1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN CHI-SQUARE INDEPENDENCE TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      NUMBER OF SETS    NUMSE1 = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NUMSE2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)
 2204   FORMAT('      NUMBER OF SETS    NUMSE2 = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      AN1=N1
      AN2=N2
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
!
!               ***********************************************
!               **  STEP 2.3--                               **
!               **  COMPUTE THE CHI-SQUARE STATISTIC         **
!               ***********************************************
!
      ISTEPN='23'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
!
!     COMPUTE COUNTS FOR EACH CELL
!
      J=0
      DO 2310 ISET1=1,NUMSE1
        DO 2320 ISET2=1,NUMSE2
!
          K=0
          DO 2330 I=1,N1
            IF(XIDTEM(ISET1).EQ.Y1(I).AND.XIDTE2(ISET2).EQ.Y2(I))THEN
!
              K=K+1
            ENDIF
 2330     CONTINUE
          NTEMP=K
          J=J+1
          TEMP1(J)=REAL(K)
          TEMP2(J)=XIDTEM(ISET1)
          TEMP3(J)=XIDTE2(ISET2)
!
 2320   CONTINUE
 2310 CONTINUE
      NTEMP2=J
!
!     COMPUTE ROW AND COLUMN TOTALS AND GRAND TOTAL.
!
      J=0
      GTOTAL=0.0D0
!
      DO 2340 ISET1=1,NUMSE1
        ROWTOT(ISET1)=0.0D0
        DO 2350 ISET2=1,NUMSE2
          J=J+1
          ROWTOT(ISET1)=ROWTOT(ISET1) + DBLE(TEMP1(J))
          GTOTAL=GTOTAL + DBLE(TEMP1(J))
 2350   CONTINUE
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')THEN
          WRITE(ICOUT,2352)ISET1,ROWTOT(ISET1)
 2352     FORMAT('ISET1,ROWTOT(ISET1)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 2340 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')THEN
        WRITE(ICOUT,2355)GTOTAL
 2355   FORMAT('GTOTAL=',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 2360 ISET2=1,NUMSE2
        COLTOT(ISET2)=0.0D0
        VALTMP=XIDTE2(ISET2)
        DO 2370 J=1,NTEMP2
          IF(TEMP3(J).EQ.XIDTE2(ISET2))THEN
            COLTOT(ISET2)=COLTOT(ISET2) + DBLE(TEMP1(J))
          ENDIF
 2370   CONTINUE
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')THEN
          WRITE(ICOUT,2372)ISET2,COLTOT(ISET2)
 2372     FORMAT('ISET2,COLTOT(ISET2)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 2360 CONTINUE
!
!     NOW COMPUTE THE CHI-SQUARE TEST STATISTIC
!
      CHISQ1=0.0D0
      CHISQ2=0.0D0
      J=0
!
      IFORMT='(2I8,4E15.7)'
      IF(IAUXDP.NE.7)THEN
        IFORMT=' '
        IF(IAUXDP.LE.9)THEN
          IFORMT='(2I8,4Exx.x)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(8:9),'(I2)')ITOT
          WRITE(IFORMT(11:11),'(I1)')IAUXDP
        ELSE
          IFORMT='(2I8,4Exx.xx)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(8:9),'(I2)')ITOT
          WRITE(IFORMT(11:12),'(I2)')IAUXDP
        ENDIF
      ENDIF
!
      DO 2380 ISET1=1,NUMSE1
        DO 2390 ISET2=1,NUMSE2
          J=J+1
          EXP=ROWTOT(ISET1)*COLTOT(ISET2)/GTOTAL
          VALTMP=(DBLE(TEMP1(J)) - EXP)**2/EXP
          CHISQ1=CHISQ1 + VALTMP
          VALTMP=DABS(DBLE(TEMP1(J)) - EXP)
          VALTMP=(VALTMP - 0.5D0)**2/EXP
          CHISQ2=CHISQ2 + VALTMP
          WRITE(IOUNI1,IFORMT)ISET1,ISET2,ROWTOT(ISET1),COLTOT(ISET2),   &
                              EXP,TEMP1(J)
!2385     FORMAT(I8,I8,E15.7,E15.7,E15.7,E15.7)
 2390   CONTINUE
 2380 CONTINUE
      NROW=NUMSE1
      NCOL=NUMSE2
!
      GO TO 4000
!
 3000 CONTINUE
!
!               ********************************************
!               **  STEP 31--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               **  ALL TABLE ENTRIES SHOULD BE           **
!               **  NON-NEGATIVE INTEGERS.  NEGATIVE      **
!               **  VALUES WILL BE FLAGGED AS ERRORS      **
!               **  WHILE NON-INTEGER VALUES WILL BE      **
!               **  ROUNDED TO NEAREST INTEGER.           **
!               **  SINCE WE ARE SCANNING TABLE, COMPUTE  **
!               **  ROW AND COLUMN TOTALS.                **
!               ********************************************
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERROR='NO'
      NUMERR=0
      MAXERR=10
!
      DO 3001 I=1,NROW
        ROWTOT(I)=0.0D0
 3001 CONTINUE
      GTOTAL=0.0D0
!
      DO 3010 J=1,NCOL
        COLTOT(J)=0.0D0
        DO 3020 I=1,NROW
          IF(XMAT(I,J).LT.0.0)THEN
            NUMERR=NUMERR+1
            IF(NUMERR.GT.MAXERR)GO TO 9000
            IERROR='YES'
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3021)I,J
 3021       FORMAT('      ROW ',I8,' AND COLUMN ',I8,   &
                   ' OF THE INPUT TABLE')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3023)XMAT(I,J)
 3023       FORMAT('      IS NEGATIVE.  THE VALIE IS ',G15.7)
            CALL DPWRST('XXX','WRIT')
          ELSE
            ITEMP=INT(XMAT(I,J)+0.5)
            XMAT(I,J)=REAL(ITEMP)
            COLTOT(J)=COLTOT(J) + DBLE(XMAT(I,J))
            ROWTOT(I)=ROWTOT(I) + DBLE(XMAT(I,J))
            GTOTAL=GTOTAL + DBLE(XMAT(I,J))
          ENDIF
 3020   CONTINUE
 3010 CONTINUE
!
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************************
!               **  STEP 32--                             **
!               **  COMPUTE THE CHI-SQUARE TEST STATISTIC **
!               ********************************************
!
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CHISQ1=0.0D0
      CHISQ2=0.0D0
      ICNT=0
!
      IFORMT='(2I8,4E15.7)'
      IF(IAUXDP.NE.7)THEN
        IFORMT=' '
        IF(IAUXDP.LE.9)THEN
          IFORMT='(2I8,4Exx.x)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(8:9),'(I2)')ITOT
          WRITE(IFORMT(11:11),'(I1)')IAUXDP
        ELSE
          IFORMT='(2I8,4Exx.xx)'
          ITOT=IAUXDP+8
          WRITE(IFORMT(8:9),'(I2)')ITOT
          WRITE(IFORMT(11:12),'(I2)')IAUXDP
        ENDIF
      ENDIF
!
      DO 3110 J=1,NCOL
        DO 3120 I=1,NROW
          ICNT=ICNT+1
          EXP=ROWTOT(I)*COLTOT(J)/GTOTAL
          VALTMP=(DBLE(XMAT(I,J)) - EXP)**2/EXP
          CHISQ1=CHISQ1 + VALTMP
          VALTMP=DABS(DBLE(XMAT(I,J)) - EXP)
          VALTMP=(VALTMP - 0.5D0)**2/EXP
          CHISQ2=CHISQ2 + VALTMP
          WRITE(IOUNI1,IFORMT)I,J,ROWTOT(I),COLTOT(J),EXP,XMAT(I,J)
!3115     FORMAT(2I8,4E15.7)
 3120   CONTINUE
 3110 CONTINUE
!
      AN1=REAL(GTOTAL)
      AN2=REAL(GTOTAL)
!
      GO TO 4000
!
!               ********************************************
!               **  STEP 41--                             **
!               **  FOR ALL INPUT METHODS (SCALAR,        **
!               **  TWO VARIABLES, TABLE), COMPUTE THE    **
!               **  CRITIVAL VALUES AND PRINT THE RESULTS.**
!               ********************************************
!
 4000 CONTINUE
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      STATVA=CHISQ1
      STATV2=CHISQ2
!
      IDF=(NROW-1)*(NCOL-1)
      CALL CHSCDF(STATVA,IDF,CDF)
      CALL CHSCDF(STATV2,IDF,CDF2)
!
      IWRITE='OFF'
!
      ICONC1='REJECT'
      ICONC2='REJECT'
      ICONC3='REJECT'
      ICONC4='REJECT'
      ICONC5='REJECT'
      ICONC6='REJECT'
      KCONC1='REJECT'
      KCONC2='REJECT'
      KCONC3='REJECT'
      KCONC4='REJECT'
      KCONC5='REJECT'
      KCONC6='REJECT'
!
      ALPHA=0.50
      CALL CHSPPF(ALPHA,IDF,CV1)
      ALPHA=0.80
      CALL CHSPPF(ALPHA,IDF,CV2)
      ALPHA=0.90
      CALL CHSPPF(ALPHA,IDF,CV3)
      ALPHA=0.95
      CALL CHSPPF(ALPHA,IDF,CV4)
      ALPHA=0.975
      CALL CHSPPF(ALPHA,IDF,CV5)
      ALPHA=0.99
      CALL CHSPPF(ALPHA,IDF,CV6)
!
      IF(0.000.LE.CDF.AND.CDF.LE.0.50)ICONC1='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.80)ICONC2='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.90)ICONC3='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.95)ICONC4='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.975)ICONC5='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.99)ICONC6='ACCEPT'
!
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.50)KCONC1='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.80)KCONC2='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.90)KCONC3='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.95)KCONC4='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.975)KCONC5='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.99)KCONC6='ACCEPT'
!
!               ******************************
!               **   STEP 42--              **
!               **   WRITE OUT EVERYTHING   **
!               **   FOR CHI-SQUARE   TEST  **
!               ******************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
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
      ITITLE='Chi-Square Test for Independence (RxC Table)'
      NCTITL=44
      ITITLZ=' '
      NCTITZ=0
!
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Two Variables Are Independent'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Two Variables Are Not Independent'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample 1:'
      NCTEXT(ICNT)=9
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=AN1
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Levels (Rows):'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=REAL(NROW)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample 2:'
      NCTEXT(ICNT)=9
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=AN2
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Levels (Columns):'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=REAL(NCOL)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Without Yates Continuity Correction:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Chi-Square Test Statistic:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Degrees of Freedom:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=REAL(IDF)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value of Test Statistic:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=CDF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='With Yates Continuity Correction:'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Chi-Square Test Statistic:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=STATV2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Degrees of Freedom:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=REAL(IDF)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value of Test Statistic:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=CDF2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
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
      ITITLE(1:25)='Without Yates Correction:'
      NCTITL=25
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(1,1)=' '
      NCTIT2(1,1)=0
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
      ITITL2(1,2)=' '
      NCTIT2(1,2)=0
      ITITL2(2,2)='Confidence'
      NCTIT2(2,2)=10
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
      ITITL2(1,3)=' '
      NCTIT2(1,3)=0
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value'
      NCTIT2(3,3)=5
      ITITL2(1,4)='Null Hypothesis'
      NCTIT2(1,4)=15
      ITITL2(2,4)='Acceptance'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Interval'
      NCTIT2(3,4)=8
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
!
      NMAX=0
      NUMCOL=5
      DO 7410 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        IF(I.EQ.3)THEN
          ITYPCO(I)='NUME'
        ELSE
          ITYPCO(I)='ALPH'
        ENDIF
        IF(I.EQ.2)THEN
          IDIGIT(I)=1
        ELSEIF(I.EQ.3)THEN
          IDIGIT(I)=2
        ELSE
          IDIGIT(I)=NUMDIG
        ENDIF
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=125
        IWHTML(4)=150
        IWHTML(5)=150
        IINC=1600
        IINC2=1400
        IINC3=2200
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC3
        IWRTF(5)=IWRTF(4)+IINC2
!
        DO 7489 J=1,NUMALP
          NCVALU(J,1)=0
          NCVALU(J,2)=0
          NCVALU(J,3)=0
          NCVALU(J,4)=0
          NCVALU(J,5)=0
          IVALUE(J,1)=' '
          IVALUE(J,2)=' '
          IVALUE(J,3)=' '
          IVALUE(J,4)=' '
          IVALUE(J,5)=' '
          IF(J.EQ.1)THEN
            IVALUE(J,2)='50.0%'
            NCVALU(J,2)=5
            AMAT(J,3)=CV1
            IVALUE(J,5)(1:6)=ICONC1(1:6)
            NCVALU(J,5)=6
            IVALUE(J,4)='(0,0.500)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='80.0%'
            NCVALU(J,2)=5
            AMAT(J,3)=CV2
            IVALUE(J,5)(1:6)=ICONC2(1:6)
            NCVALU(J,5)=6
            IVALUE(J,4)='(0,0.800)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='90.0%'
            NCVALU(J,2)=5
            AMAT(J,3)=CV3
            IVALUE(J,5)(1:6)=ICONC3(1:6)
            NCVALU(J,5)=6
            IVALUE(J,4)='(0,0.900)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,2)='95.0%'
            NCVALU(J,2)=5
            AMAT(J,3)=CV4
            IVALUE(J,5)(1:6)=ICONC4(1:6)
            NCVALU(J,5)=6
            IVALUE(J,4)='(0,0.950)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.5)THEN
            IVALUE(J,2)='97.5%'
            NCVALU(J,2)=5
            AMAT(J,3)=CV5
            IVALUE(J,5)(1:6)=ICONC5(1:6)
            NCVALU(J,5)=6
            IVALUE(J,4)='(0,0.975)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.6)THEN
            IVALUE(J,2)='99.0%'
            NCVALU(J,2)=5
            AMAT(J,3)=CV6
            IVALUE(J,5)(1:6)=ICONC6(1:6)
            NCVALU(J,5)=6
            IVALUE(J,4)='(0,0.990)'
            NCVALU(J,4)=9
          ENDIF
          AMAT(J,1)=0.0
          AMAT(J,2)=0.0
          AMAT(J,4)=0.0
          AMAT(J,5)=0.0
          IVALUE(J,1)='Independent'
          NCVALU(J,1)=11
 7489   CONTINUE
!
 7410 CONTINUE
!
      ICNT=NUMALP
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
      ITITLE(1:30)='With Yates Bias Correction:'
      NCTITL=30
!
      NUMCOL=5
      DO 7510 I=1,NUMCOL
!
        DO 7589 J=1,NUMALP
          IF(J.EQ.1)THEN
            IVALUE(J,5)(1:6)=KCONC1(1:6)
            NCVALU(J,5)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,5)(1:6)=KCONC2(1:6)
            NCVALU(J,5)=6
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,5)(1:6)=KCONC3(1:6)
            NCVALU(J,5)=6
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,5)(1:6)=KCONC4(1:6)
            NCVALU(J,5)=6
          ELSEIF(J.EQ.5)THEN
            IVALUE(J,5)(1:6)=KCONC5(1:6)
            NCVALU(J,5)=6
          ELSEIF(J.EQ.6)THEN
            IVALUE(J,5)(1:6)=KCONC6(1:6)
            NCVALU(J,5)=6
          ENDIF
 7589   CONTINUE
!
 7510 CONTINUE
!
      ICNT=NUMALP
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
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CHI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHI2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)AN11,AN21,AN12,AN22
 9013   FORMAT('AN11,AN21,AN12,AN22=',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9015)AN1,AN2
 9015   FORMAT('AN1,AN2=',2G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)N11,N21,N12,N22
 9017   FORMAT('N11,N21,N12,N22=',4I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCHI2
      SUBROUTINE DPCHWI(IHARG,IARGT,ARG,NUMARG,   &
                        PDEFWI,   &
                        MAXCHA,   &
                        PCHAWI,PCHAHG,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER WIDTHS FOR USE IN MULTI-TRACE PLOTS.
!              THE WIDTH FOR THE CHARACTER FOR THE I-TH TRACE
!              WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE FLOATING POINT
!              VECTOR PCHAWI(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --PDEFWI
!                     --MAXCHA
!     OUTPUT ARGUMENTS--PCHAWI  (A  FLOATING POINT VECTOR
!                       WHOSE I-TH ELEMENT IS THE WIDTH (= WIDTHT)
!                       FOR THE CHARACTER
!                       ASSIGNED TO THE I-TH    TRACE    IN
!                       A MULTI-TRACE PLOT.
!                     --PCHAWI = CHARACTER WIDTH
!                     --PCHAHG = HORIZONTAL GAP BETWEEN CHARACTERS
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
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
      DIMENSION PCHAWI(*)
      DIMENSION PCHAHG(*)
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
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'WIDTH'.AND.   &
      IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')GO TO 2110
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'WIDT'.AND.   &
      IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')GO TO 2110
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'WIDTH'.AND.   &
      IHARG(3).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')GO TO 2110
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'WIDT'.AND.   &
      IHARG(3).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')GO TO 2110
!
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'WIDTH')GO TO 1160
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'WIDT')GO TO 1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'WIDTH')GO TO 1105
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'WIDT')GO TO 1105
      GO TO 2199
!
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1110
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1110
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1110
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1110
!
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GO TO 1160
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GO TO 1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GO TO 1140
!
      GO TO 1150
!
 1110 CONTINUE
      DO 1115 I=1,MAXCHA
      PCHAWI(I)=PDEFWI
 1115 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAWI(I)
 1116 FORMAT('ALL CHARACTER WIDTHS HAVE JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GO TO 2190
!
 1120 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GO TO 1180
      PCHAWI(1)=ARG(2)
!
      IF(IFEEDB.EQ.'OFF')GO TO 1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,PCHAWI(I)
 1126 FORMAT('THE WIDTH FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GO TO 2190
!
 1130 CONTINUE
      I=1
      IF(IARGT(3).NE.'NUMB')GO TO 1180
      DO 1135 I=1,MAXCHA
      PCHAWI(I)=ARG(3)
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 2190
!
 1140 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GO TO 1180
      DO 1145 I=1,MAXCHA
      PCHAWI(I)=ARG(2)
 1145 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 2190
!
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO 1155 I=1,IMAX
      IP1=I+1
      IF(IARGT(IP1).NE.'NUMB')GO TO 1180
      PCHAWI(I)=ARG(IP1)
 1155 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1156 I=1,IMAX
      WRITE(ICOUT,1126)I,PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GO TO 2190
!
 1160 CONTINUE
      DO 1165 I=1,MAXCHA
      PCHAWI(I)=PDEFWI
 1165 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GO TO 2190
!
 1180 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DPCHWI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('CHARACTER WIDTHS MUST BE NUMERIC;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('HOWEVER, THE SPECIFIED CHARACTER WIDTH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)I
 1184 FORMAT('FOR CHARACTER ',I6,' WAS NON-NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      GO TO 2199
!
 2110 CONTINUE
      IMAX=24
      PCHAWI(1)=1.0
      PCHAWI(2)=1.0
      PCHAWI(3)=1.0
      PCHAWI(4)=1.0
      PCHAWI(5)=1.0
      PCHAWI(6)=1.0
      PCHAWI(7)=1.0
      PCHAWI(8)=1.0
      PCHAWI(9)=1.0
      PCHAWI(10)=1.0
      PCHAWI(11)=1.0
      PCHAWI(12)=1.0
      PCHAWI(13)=1.0
      PCHAWI(14)=1.0
      PCHAWI(15)=1.0
      PCHAWI(16)=1.0
      PCHAWI(17)=1.0
      PCHAWI(18)=1.0
      PCHAWI(19)=1.0
      PCHAWI(20)=1.0
      PCHAWI(21)=1.5
      PCHAWI(22)=1.0
      PCHAWI(23)=1.0
      PCHAWI(24)=1.5
      GO TO 2170
!
 2170 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 2179
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 2175 I=1,IMAX
      WRITE(ICOUT,2176)I,PCHAWI(I)
 2176 FORMAT('THE WIDTH FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 2175 CONTINUE
 2179 CONTINUE
      GO TO 2180
!
 2180 CONTINUE
      IFOUND='YES'
      GO TO 2190
!
 2190 CONTINUE
      IFOUND='YES'
      DO 2191 I=1,MAXCHA
      PCHAHG(I)=PCHAWI(I)*0.25
 2191 CONTINUE
!
 2199 CONTINUE
      RETURN
      END SUBROUTINE DPCHWI
      SUBROUTINE DPCMAP(IHARG,NUMARG,IDCMAP,ICHMAP,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PLOT CHARACTER MAPPING
!              (BY RANK    OR    BY EXACT)
!              WHICH LINKS TRACE ID AND CHARACTER
!              (THE CURRENT DEFAULT IS BY RANK).
!     EXAMPLE--IF HAVE DATA: X: 1 1 2 2 3 3
!                            Y: 1 2 3 4 5 6
!                          TAG: 1 1 3 3 5 5
!              AND CHARACTERS 1 2 3 4 5
!              AND DESIRE TO HAVE THE TRACES SHOW 1 3 AND 5
!              THEN CURRENTLY BY DEFAULT WOULD GET
!              TRACES SHOWING 1 2 3 (SINCE MAP VIA RANK)
!              BUT IF ENTER      CHARACTER MAP EXACT
!              THEN WOULD GET TRACES SHOWING 1 3 5 (AS DESIRED)
!     COMMAND EXAMPLE--CHARACTER MAP RANK (= DEFAULT)
!                      CHARACTER MAP EXACT
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDCMAP
!     OUTPUT ARGUMENTS--ICHMAP  (A  CHARACTER VARIABLE
!                       WHICH DEFINES THE MAP
!                       (RANK OR EXAC)
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
!     VERSION NUMBER--94/12
!     ORIGINAL VERSION--DECEMBER  1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDCMAP
      CHARACTER*4 ICHMAP
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
      IF(NUMARG.EQ.1)THEN
         ICHMAP=IDCMAP
         GO TO 1150
      ENDIF
!
      IF(NUMARG.GE.2)THEN
         IF(IHARG(NUMARG).EQ.'ON'.OR.   &
         IHARG(NUMARG).EQ.'OFF'.OR.   &
         IHARG(NUMARG).EQ.'AUTO'.OR.   &
         IHARG(NUMARG).EQ.'DEFA')THEN
            ICHMAP=IDCMAP
            GO TO 1150
         ELSE IF(IHARG(NUMARG).EQ.'EXAC'.OR.   &
         IHARG(NUMARG).EQ.'1TO1')THEN
            ICHMAP='EXAC'
            GO TO 1150
         ELSE IF(IHARG(NUMARG).EQ.'?')THEN
            GO TO 1160
         ELSE
            ICHMAP=IHARG(2)
            GO TO 1150
         ENDIF
      ENDIF
!
 1150 CONTINUE
      IF(IFEEDB.EQ.'ON')THEN
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1151)ICHMAP
 1151    FORMAT('THE CHARACTER MAPPING HAS JUST BEEN SET TO ',   &
         A4)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IFOUND='YES'
      GO TO 9000
!
 1160 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1161)
 1161 FORMAT('CHARACTER MAPPING HAS 2 POSSIBLE SETTINGS:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1162)
 1162 FORMAT('   RANK   AND   EXACT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1163)ICHMAP
 1163 FORMAT('THE CURRENT CHARACTER MAPPING IS    ',A4)
      CALL DPWRST('XXX','BUG ')
      IFOUND='YES'
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPCMAP
      SUBROUTINE DPCONC(IHARG,NUMARG,IDEFCC,ICONCH,   &
                        IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE CONTINUE CHARACTOR WHICH MAY
!              BE USED TO CONTINUE A COMMAND TO A SECOND
!              LINE (NO MORE THAN 2 LINES ALLOWED)
!              ABOUT THE ONLY PLACE THIS IS NECCESSARY
!              IN DATAPLOT IS IN ENTERING TITLES, ESPECIALLY
!              IF MANY SHIFTS ARE INCLUDED FOR UPPER, LOWER CASE
!              AND SPECIAL SYMBOLS
!
!              THE CONTINUE CHARACTER CAN BE UP TO 4 CHARACTERS LONG
!
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFCC (A  CHARACTER VARIABLE)
!                     --IBUGS2 (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--ICONCH (A CHARACTER VARIABLE)
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER 1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCC
      CHARACTER*4 ICONCH
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
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
      IF(IBUGS2.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPCONC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFCC
   53 FORMAT('IDEFCC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMARG
   54 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1150
      GO TO 1110
!
 1110 CONTINUE
      IF(NUMARG.LE.1)GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD=IDEFCC
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      ICONCH=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ICONCH
 1181 FORMAT('THE CONTINUE CHARACTER HAS JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 9000
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPCONC-')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR
 9012 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFCC,ICONCH
 9013 FORMAT('IDEFCC,ICONCH = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPCONC

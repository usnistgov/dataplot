      SUBROUTINE DPC4HI(IHVAL,IVAL,IBUGA3,IERROR)
C
C     PURPOSE--CONVERT A CHARACTER VARIABLE
C              INTO THE CORRESPONDING INTEGER VALUE.
C     NOTE--INASMUCH AS THE ASSUMED INPUT WORD HAS 4 CHARACTERS AT MOST,
C           THEN THE VALID RANGE OF THE OUTPUT INTEGER VARIABLE
C           IS -999 TO 9999   .
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHVAL
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHTEMP
      CHARACTER*4 ISIGN
C
C---------------------------------------------------------------------
C
      DIMENSION IHTEMP(4)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      NUMASC=4
      IVAL=0
C
      ITERM=0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPC4HI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IHVAL,IBUGA3,IERROR
   52 FORMAT('IHVAL,IBUGA3,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DECOMPOSE THE 4-CHARACTERS IN IHVAL  **
C               **  INTO 4 1-CHARACTER WORDS.            **
C               *******************************************
C
      DO200J=1,NUMASC
      IHTEMP(J)='    '
      ISTAR1=NUMBPC*(J-1)
      CALL DPCHEX(ISTAR1,NUMBPC,IHVAL,0,NUMBPC,IHTEMP(J))
  200 CONTINUE
C
C               ******************************************************
C               **  STEP 2--                                        **
C               **  CARRY OUT THE HOLLERITH TO INTEGER CONVERSION.  **
C               ******************************************************
C
      ISIGN='+'
      NUMSIG=0
      IDIGI=0
      ISUM=0
      DO400I=1,NUMASC
      IREV=NUMASC-I+1
      IF(IHTEMP(IREV).EQ.' ')GOTO400
      IF(IHTEMP(IREV).EQ.'0')GOTO410
      IF(IHTEMP(IREV).EQ.'1')GOTO411
      IF(IHTEMP(IREV).EQ.'2')GOTO412
      IF(IHTEMP(IREV).EQ.'3')GOTO413
      IF(IHTEMP(IREV).EQ.'4')GOTO414
      IF(IHTEMP(IREV).EQ.'5')GOTO415
      IF(IHTEMP(IREV).EQ.'6')GOTO416
      IF(IHTEMP(IREV).EQ.'7')GOTO417
      IF(IHTEMP(IREV).EQ.'8')GOTO418
      IF(IHTEMP(IREV).EQ.'9')GOTO419
      IF(IHTEMP(IREV).EQ.'+')GOTO420
      IF(IHTEMP(IREV).EQ.'-')GOTO421
C
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
      GOTO9000
C
  410 ITERM=0
      GOTO425
  411 ITERM=1
      GOTO425
  412 ITERM=2
      GOTO425
  413 ITERM=3
      GOTO425
  414 ITERM=4
      GOTO425
  415 ITERM=5
      GOTO425
  416 ITERM=6
      GOTO425
  417 ITERM=7
      GOTO425
  418 ITERM=8
      GOTO425
  419 ITERM=9
      GOTO425
  420 NUMSIG=NUMSIG+1
      GOTO400
  421 NUMSIG=NUMSIG+1
      ISIGN='-'
      GOTO400
  425 IDIGI=IDIGI+1
      IEXP=IDIGI-1
CCCCC ISUM=ISUM+ITERM*(10**IEXP)
      IJUNK=INT(10.0**IEXP + 0.01)
      ISUM=ISUM+ITERM*IJUNK
  400 CONTINUE
C
      IF(NUMSIG.LE.1)GOTO459
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
      GOTO9000
  459 CONTINUE
      IF(ISIGN.EQ.'-')ISUM=-ISUM
      IVAL=ISUM
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
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
C
      RETURN
      END
      SUBROUTINE DPC4IH(IVAL,IHVAL,IBUGA3,IERROR)
C
C     PURPOSE--CONVERT AN INTEGER VARIABLE
C              TO A 4-CHARACTER-PER-WORD HOLLERITH STRING.
C     NOTE--CONVERT ONLY THE FIRST 4 CHARACTERS OF THE
C           INTEGER VARIABLE (INCLUDING THE NEGATIVE
C           SIGN, IF EXISTENT).
C     NOTE--INCORRECT VALUERS WILL RESULT IF THE INPUT INTEGER
C           IS LARGER THAN 9999 OR SMALLER THAN -999   .
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHVAL
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHTEMP
      CHARACTER*4 ISIGN
      CHARACTER*4 IHDIG
C
C---------------------------------------------------------------------
C
      DIMENSION IHTEMP(4)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      NUMASC=4
      IVAL2=IVAL
      IHVAL='    '
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPC4IH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IVAL,IBUGA3,IERROR
   52 FORMAT('IVAL,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               ***********************
C               **  STEP 2--         **
C               **  DETERMINE SIGN.  **
C               ***********************
C
      ISIGN='+'
      IF(IVAL2.LT.0)ISIGN='-'
      IVAL2=IABS(IVAL2)
C
C               ***********************************
C               **  STEP 3--                     **
C               **  DETERMINE NUMBER OF DIGITS.  **
C               ***********************************
C
      IMIN=1
      IMAX=NUMASC
      DO300I=IMIN,IMAX
      IREV=IMAX-I+IMIN
      IDIV=INT(10.0**(IREV-1) + 0.01)
      IDIG=IVAL2/IDIV
      IF(IDIG.NE.0)GOTO350
  300 CONTINUE
      NUMDIG=1
      GOTO390
  350 CONTINUE
      NUMDIG=IREV
  390 CONTINUE
C
C               ***************************************
C               **  STEP 4--                         **
C               **  IF NEGATIVE,                     **
C               **  INSERT SIGN INTO OUTPUT VECTOR.  **
C               ***************************************
C
      J=0
      IF(ISIGN.EQ.'-')J=J+1
      IF(ISIGN.EQ.'-')IHTEMP(J)='-'
C
C               **************************
C               **  STEP 5--            **
C               **  INSERT DIGITS INTO  **
C               **  OUTPUT VECTOR.      **
C               **************************
C
      IMIN=1
      IMAX=NUMDIG
      IF(IMAX.GE.NUMASC.AND.ISIGN.EQ.'-')IMAX=NUMASC-1
      IF(IMAX.GE.NUMASC.AND.ISIGN.EQ.'+')IMAX=NUMASC
      DO500I=IMIN,IMAX
      IREV=IMAX-I+IMIN
      IDIV=INT(10.0**(IREV-1) + 0.01)
      IDIG=IVAL2/IDIV
C
      IF(IDIG.EQ.0)GOTO510
      IF(IDIG.EQ.1)GOTO511
      IF(IDIG.EQ.2)GOTO512
      IF(IDIG.EQ.3)GOTO513
      IF(IDIG.EQ.4)GOTO514
      IF(IDIG.EQ.5)GOTO515
      IF(IDIG.EQ.6)GOTO516
      IF(IDIG.EQ.7)GOTO517
      IF(IDIG.EQ.8)GOTO518
      IF(IDIG.EQ.9)GOTO519
  510 CONTINUE
      IHDIG='0'
      GOTO529
  511 CONTINUE
      IHDIG='1'
      GOTO529
  512 CONTINUE
      IHDIG='2'
      GOTO529
  513 CONTINUE
      IHDIG='3'
      GOTO529
  514 CONTINUE
      IHDIG='4'
      GOTO529
  515 CONTINUE
      IHDIG='5'
      GOTO529
  516 CONTINUE
      IHDIG='6'
      GOTO529
  517 CONTINUE
      IHDIG='7'
      GOTO529
  518 CONTINUE
      IHDIG='8'
      GOTO529
  519 CONTINUE
      IHDIG='9'
      GOTO529
  529 CONTINUE
C
      J=J+1
      IF(J.GT.NUMASC)GOTO550
      IHTEMP(J)=IHDIG
      IVAL2=IVAL2-IDIG*IDIV
  500 CONTINUE
C
      NTEMP=J
      GOTO590
C
  550 CONTINUE
      NTEMP=J-1
      GOTO590
C
  590 CONTINUE
C
C               ***************************************
C               **  STEP 6--                         **
C               **  PACK THE CHARACTERS INTO 1 WORD  **
C               ***************************************
C
      IHVAL='    '
      IMAX=NUMASC
      IF(NTEMP.LE.IMAX)IMAX=NTEMP
      IF(IMAX.LE.0)GOTO690
      DO600J=1,IMAX
      ISTAR2=NUMBPC*(J-1)
      CALL DPCHEX(0,NUMBPC,IHTEMP(J),ISTAR2,NUMBPC,IHVAL)
  600 CONTINUE
  690 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCAAN(XTEMP1,MAXNXT,
     1                  ICASAN,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A TABLE OF CAPABILITY ANALYSIS STATISTICS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--90/9
C     ORIGINAL VERSION--SEPTEMBER 1990.
C     UPDATED         --APRIL     2001. 1) ARGUMENT LIST TO DPCAA2
C                                       2) SAVE RESULTS FROM DPCAA2
C                                          AS INTERNAL PARAMETERS
C     UPDATED         --MAY       2011. USE DPPARS
C     UPDATED         --MAY       2011. SUPPORT FOR "MULTIPLE" AND
C                                       "REPLICATION" OPTIONS
C     UPDATED         --JUNE      2019. TWEAK TO SCRATCH ARRAYS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
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
C
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
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
      DIMENSION W(MAXOBV)
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
      DIMENSION TEMP2(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
C
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
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
      ICASAN='CAAN'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPCA'
      ISUBN2='AN  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***********************************************
C               **  TREAT THE CAPABILITY ANALYSIS    CASE    **
C               ***********************************************
C
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
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  EXTRACT THE COMMAND                               **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:           **
C               **    1) CAPABILITY ANALYSIS Y                        **
C               **    2) MULTIPLE CAPABILITY ANALYSIS  Y1 ... YK      **
C               **    3) REPLICATED CAPABILITY ANALYSIS  Y X1 ... XK  **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTC=9999
      ILASTZ=9999
      ICASAN='CAAN'
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
        ELSEIF(ICTMP1.EQ.'CAPA' .AND. ICTMP2.EQ.'ANAL')THEN
          IFOUND='YES'
          ICASAN='CAAN'
          ILASTC=I+1
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'CAPA' .OR. ICTMP1.EQ.'CP' .OR.
     1         ICTMP1.EQ.'CPK')THEN
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
C
      IF(IFOUND.EQ.'NO')GOTO9000
C
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGA2,IERROR)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')THEN
        WRITE(ICOUT,91)ICASAN,IMULT,IREPL,ISHIFT
   91   FORMAT('DPCAAN: ICASAN,IMULT,IREPL,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN CAPABILITY ANALYSIS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE CAPABILITY ANALYSIS COMMAND.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               *********************************
C               **  STEP 4--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
      DO519I=1,MAXOBV
        W(I)=1.0
  519 CONTINUE
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************
C               **  STEP 7--                               **
C               **  DETERMINE IF THE ANALYST               **
C               **  HAS SPECIFIED                          **
C               **      LSL (LOWER SPEC LIMIT)             **
C               **      USL (UPPER SPEC LIMIT)             **
C               **      USLCOST (UPPER SPEC LIMIT COST)    **
C               **      TARGET                             **
C               *********************************************
C
      ISTEPN='7'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CCLSL=CPUMIN
      IH='LSL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')CCLSL=VALUE(ILOCP)
C
      CCUSL=CPUMIN
      IH='USL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')CCUSL=VALUE(ILOCP)
C
      CCTARG=CPUMIN
      IH='TARG'
      IH2='ET  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')CCTARG=VALUE(ILOCP)
C
      CCUSLC=CPUMIN
      IH='USLC'
      IH2='OST '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')CCUSLC=VALUE(ILOCP)
C
C               *********************************************************
C               **  STEP 6--                                           **
C               **  GENERATE THE CAPABILITY ANALYSIS FOR VARIOUS CASES **
C               *********************************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 1: NO REPLICATION VARIABLES    **
C               ******************************************
C
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')THEN
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
     1                Y,XTEMP1,XTEMP1,NS1,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C         *****************************************************
C         **  STEP 8B--                                      **
C         *****************************************************
C
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
              DO825I=1,NS1
                WRITE(ICOUT,826)I,Y(I)
  826           FORMAT('I,Y(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
C
          CALL DPCAA2(Y,W,NS1,XTEMP1,MAXNXT,
     1                CCLSL,CCUSL,CCTARG,CCUSLC,
     1                YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                ICAPSW,ICAPTY,IFORSW,ICNPKD,
     1                PID,IVARID,IVARI2,NREPL,
     1                IBUGA3,ISUBRO,IERROR)
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(NRESP.GT.1)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,
     1                YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CAAN')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO910I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO910
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
C       **  CALL DPSUM2 TO PERFORM SUMMARY.                **
C       *****************************************************
C
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAAN')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPCAAN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,942)ICASAN,NUMVAR,NLOCAL,NREPL
  942     FORMAT('ICASAN,NUMVAR,NLOCAL,NREPL = ',
     1           A4,3I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO945I=1,NLOCAL
              WRITE(ICOUT,946)I,Y(I),XDESGN(I,1),XDESGN(I,2)
  946         FORMAT('I,Y(I),XDESGN(I,1),XDESGN(I,2) = ',
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
     1             XTEMP1,TEMP2,
     1             NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,
     1             IBUGA3,ISUBRO,IERROR)
C
C       *****************************************************
C       **  STEP 9D--                                      **
C       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
C       *****************************************************
C
        NCURVE=0
        IADD=1
C
        IF(NREPL.EQ.1)THEN
          J=0
          NTOT=NUMSE1
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
     1                    CCLSL,CCUSL,CCTARG,CCUSLC,
     1                    YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                    YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                    YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                    YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                    ICAPSW,ICAPTY,IFORSW,ICNPKD,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,
     1                  YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                  YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                  YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                  YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                  IFLAGU,IFRST,ILAST,
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
            IF(NTEMP.GT.0)THEN
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
     1                    CCLSL,CCUSL,CCTARG,CCUSLC,
     1                    YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                    YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                    YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                    YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                    ICAPSW,ICAPTY,IFORSW,ICNPKD,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,
     1                  YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                  YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                  YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                  YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                  IFLAGU,IFRST,ILAST,
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
     1                    CCLSL,CCUSL,CCTARG,CCUSLC,
     1                    YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                    YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                    YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                    YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                    ICAPSW,ICAPTY,IFORSW,ICNPKD,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,
     1                  YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                  YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                  YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                  YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                  IFLAGU,IFRST,ILAST,
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
     1                    CCLSL,CCUSL,CCTARG,CCUSLC,
     1                    YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                    YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                    YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                    YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                    ICAPSW,ICAPTY,IFORSW,ICNPKD,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,
     1                  YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                  YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                  YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                  YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                  IFLAGU,IFRST,ILAST,
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
     1                    CCLSL,CCUSL,CCTARG,CCUSLC,
     1                    YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                    YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                    YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                    YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                    ICAPSW,ICAPTY,IFORSW,ICNPKD,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,
     1                  YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                  YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                  YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                  YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                  IFLAGU,IFRST,ILAST,
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
              CALL DPCAA2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
     1                    CCLSL,CCUSL,CCTARG,CCUSLC,
     1                    YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                    YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                    YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                    YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                    ICAPSW,ICAPTY,IFORSW,ICNPKD,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    IBUGA3,ISUBRO,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,
     1                  YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                  YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                  YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                  YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                  IFLAGU,IFRST,ILAST,
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
C
      IF(IERROR.EQ.'YES')THEN
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9001)(IANS(I),I=1,MIN(100,IWIDTH))
 9001     FORMAT(100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
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
C
      RETURN
      END
      SUBROUTINE DPCAA2(Y,W,N,XTEMP1,MAXNXT,
     1                  CCLSL,CCUSL,CCTARG,CCUSLC,
     1                  YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                  YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                  YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                  YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                  ICAPSW,ICAPTY,IFORSW,ICNPKD,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE GENERATES A CAPABILITY ANALYSIS
C              TABULATION THE DATA IN THE INPUT VECTOR Y.
C     NOTE--NORMALITY IS ASSUMED
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                OF EQUALLY-SPACED OBSERVATIONS
C                                TO BE SMOOTHED.
C                       N      = THE INTEGER NUMBER OF
C                                OBSERVATIONS IN THE VECTOR Y.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--90/9
C     ORIGINAL VERSION--SEPTEMBER 1990.
C     UPDATED         --APRIL     2001.  EXPAND TABLE:
C                                        1) ADD CC, CPM, CPL, CPU,
C                                               CNPK
C                                        2) 95% CONFIDENCE INTERVAL
C                                           FOR CP, CPK, CPL, CPU, CPM
C                                        3) ADD COMPUTED STATS TO
C                                           CALL LIST SO THEY CAN BE
C                                           SAVED AS INTERNAL
C                                           PARAMETERS
C     UPDATED         --MAY       2011. USE DPDTA1 AND DPDTA5 TO PRINT
C                                       TABLES
C     UPDATED         --APRIL     2015. ADD "ICNPKD" TO CNPK CALL LIST
C     UPDATED         --APRIL     2015. ADDITIONAL CAPABILITY STATISTICS
C     UPDATED         --APRIL     2015. ADD CONFIDENCE LIMITS TABLES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICNPKD
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IFLAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION W(*)
      DIMENSION XTEMP1(*)
      DIMENSION PID(*)
C
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA ALPHA /50.0, 80.0, 90.0, 95.0, 99.0/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCA'
      ISUBN2='A2  '
C
      IERROR='NO'
      IWRITE='OFF'
C
C
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
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),W(I)
   57     FORMAT('I,Y(I),W(I) = ',I8,2G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CAPABILITY ANALYSIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE IS LESS THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)N
  113   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO135I=2,N
        IF(Y(I).NE.HOLD)GOTO139
  135 CONTINUE
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
C               **********************************************
C               **  STEP 3--                                **
C               **  COMPUTE VARIOUS CAPABILITY STATISTICS-- **
C               **     1) CP                                **
C               **     2) CPK                               **
C               **     3) PERCENT DEFECTIVE                 **
C               **     4) EXPECTED LOSS                     **
C               **********************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG='BOTH'
C
      CALL MEAN(Y,N,IWRITE,XMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,XSD,IBUGA3,IERROR)
C
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
C
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
C
      IF(CCLSL.NE.CPUMIN.AND.CCUSL.NE.CPUMIN)THEN
        CALL CP(Y,N,CCLSL,CCUSL,IWRITE,YCP,YCPLL,YCPUL,
     1          IBUGA3,IERROR)
        CALL CPL(Y,N,CCLSL,CCUSL,IWRITE,YCPL,YCPLLL,YCPLUL,
     1           IBUGA3,IERROR)
        CALL CPU(Y,N,CCLSL,CCUSL,IWRITE,YCPU,YCPULL,YCPUUL,
     1           IBUGA3,IERROR)
        CALL CPK(Y,N,CCLSL,CCUSL,IWRITE,YCPK,YCPKLL,YCPKUL,
     1           IBUGA3,IERROR)
        CALL CPM(Y,N,CCLSL,CCUSL,CCTARG,IWRITE,YCPM,YCPMLL,YCPMUL,
     1           IBUGA3,IERROR)
        CALL CPMK(Y,N,CCLSL,CCUSL,CCTARG,IWRITE,YCPMK,YCPMLL,YCPMUL,
     1            IBUGA3,IERROR)
        CALL CC(Y,N,CCLSL,CCUSL,CCTARG,IWRITE,YCC,
     1          IBUGA3,IERROR)
        CALL CNP(Y,N,XTEMP1,MAXNXT,CCLSL,CCUSL,IWRITE,ICNPKD,
     1           YCNP,IBUGA3,IERROR)
        CALL CNPK(Y,N,XTEMP1,MAXNXT,CCLSL,CCUSL,IWRITE,ICNPKD,
     1            YCNPK,IBUGA3,IERROR)
        CALL CNPM(Y,N,XTEMP1,MAXNXT,CCLSL,CCUSL,CCTARG,IWRITE,ICNPKD,
     1            YCNPM,IBUGA3,IERROR)
        CALL CNPMK(Y,N,XTEMP1,MAXNXT,CCLSL,CCUSL,CCTARG,IWRITE,ICNPKD,
     1             YCNPMK,IBUGA3,IERROR)
        CALL PERDEF(Y,N,CCLSL,CCUSL,IWRITE,YACTPD,YTHEPD,
     1              YACTL,YTHEL,YACTU,YTHEU,
     1              IFLAG,IBUGA3,IERROR)
        CALL EXPLOS(Y,N,CCLSL,CCUSL,CCUSLC,IWRITE,YEXPLO,
     1              IBUGA3,IERROR)
      ENDIF
C
C               ****************************
C               **  STEP 7--              **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')
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
      ITITLE='Capability Analysis'
      NCTITL=19
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
        DO2101I=1,NREPL
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
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
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
C
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
C
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
C
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
C
      NUMROW=ICNT
      DO2110I=1,NUMROW
        NTOT(I)=15
 2110 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
C     CONFIDENCE LIMITS TABLE FOR CP
C
      ITITLE=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
      NUMLIN=2
      NUMROW=NUMALP
      NUMCOL=4
C
      ITITL9='Confidence Limits for Cp Statistic'
      NCTIT9=34
C
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
C
      NMAX=0
      DO4221I=1,NUMCOL
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
C
      AN=REAL(N)
      NV=N-1
      AV=REAL(NV)
C
      DO4223I=1,NUMROW
        DO4225J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4225   CONTINUE
C
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
C
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')
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
C     CONFIDENCE LIMITS TABLE FOR CPL
C
      ITITL9='Confidence Limits for Cpl Statistic'
      NCTIT9=35
      ITITL2(2,2)='of Cpl'
      NCTIT2(2,2)=6
C
      DO4323I=1,NUMROW
        DO4325J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4325   CONTINUE
C
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
C
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')
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
C     CONFIDENCE LIMITS TABLE FOR CPU
C
      ITITL9='Confidence Limits for Cpu Statistic'
      NCTIT9=35
      ITITL2(2,2)='of Cpu'
      NCTIT2(2,2)=6
C
      DO4423I=1,NUMROW
        DO4425J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4425   CONTINUE
C
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
C
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')
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
C     CONFIDENCE LIMITS TABLE FOR CPM
C
      ITITL9='Confidence Limits for Cpm Statistic'
      NCTIT9=35
      ITITL2(2,2)='of Cpm'
      NCTIT2(2,2)=6
C
      DO4523I=1,NUMROW
        DO4525J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4525   CONTINUE
C
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
C
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')
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
C     CONFIDENCE LIMITS TABLE FOR CPK
C
      ITITL9='Confidence Limits for Cpk Statistic'
      NCTIT9=35
      ITITL2(2,2)='of Cpk'
      NCTIT2(2,2)=6
C
      DO4623I=1,NUMROW
        DO4625J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4625   CONTINUE
C
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
C
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+2000
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='5C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CAA2')
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
C
      RETURN
      END
      SUBROUTINE DPCAA5(CCLSL,CCUSL,CCTARG,CCUSLC,
     1                  YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                  YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                  YCNPK,YCPM,YCPMLL,YCPMUL,YCC,
     1                  YACTPD,YTHEPD,YACTL,YTHEL,YACTU,YTHEU,YEXPLO,
     1                  IFLAGU,IFRST,ILAST,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPCAAN.  THIS ROUTINE
C              UPDATES VARIOUS PARAMETERS.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/5
C     ORIGINAL VERSION--MAY       2011.
C     UPDATED         --APRIL     2019. USER CAN SPECIFY NUMBER OF
C                                       DECIMAL POINTS FOR AUXILLARY
C                                       FILES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*20 IFORMT
C
C---------------------------------------------------------------------
C
      SAVE IOUNI1
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
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
  295     FORMAT(9X,'CPSTAT',11X,'CPLL',11X,'CPUL',
     1           8X,'CPKSTAT',10X,'CPKLL',10X,'CPKUL',
     1           8X,'CPLSTAT',10X,'CPLLL',10X,'CPLUL',
     1           8X,'CPUSTAT',10X,'CPULL',10X,'CPUUL',
     1           7X,'CNPKSTAT',
     1           8X,'CPMSTAT',10X,'CPMLL',10X,'CPMUL',
     1           7X,'ACTUALPD',7X,'ACTUALLL',7X,'ACTUALUL',
     1           9X,'CCSTAT',8X,'THEORPD',8X,'THEORLL',
     1           8X,'EXPLOSS')
        ENDIF
C
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
C
        WRITE(IOUNI1,IFORMT)YCP,YCPLL,YCPUL,YCPK,YCPKLL,YCPKUL,
     1                   YCPL,YCPLLL,YCPLUL,YCPU,YCPULL,YCPUUL,
     1                   YCNPK,YCPM,YCPMLL,YCPMUL,YACTPD,YACTLL,YACTUL,
     1                   YCC,YTHERPD,YTHEL,YEXPLO
CC299   FORMAT(23E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IH='CPST'
        IH2='AT  '
        VALUE0=YCP
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPLL'
        IH2='    '
        VALUE0=YCPLL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPUL'
        IH2='    '
        VALUE0=YCPUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPKS'
        IH2='TAT '
        VALUE0=YCPK
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPKL'
        IH2='L   '
        VALUE0=YCPKLL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPKU'
        IH2='L   '
        VALUE0=YCPKUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPLS'
        IH2='TAT '
        VALUE0=YCPL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPLL'
        IH2='L   '
        VALUE0=YCPLLL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPLU'
        IH2='L   '
        VALUE0=YCPLUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPUS'
        IH2='TAT '
        VALUE0=YCPU
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPUL'
        IH2='L   '
        VALUE0=YCPULL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPUU'
        IH2='L   '
        VALUE0=YCPUUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CNPK'
        IH2='STAT'
        VALUE0=YCNPK
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPMS'
        IH2='TAT '
        VALUE0=YCPM
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPML'
        IH2='L   '
        VALUE0=YCPMLL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CPMU'
        IH2='L   '
        VALUE0=YCPMUL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CCST'
        IH2='AT  '
        VALUE0=YCC
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='ACTU'
        IH2='ALPD'
        VALUE0=YACTPD
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='THEO'
        IH2='RPD '
        VALUE0=YTHEPD
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='ACTU'
        IH2='ALLL'
        VALUE0=YACTL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='THEO'
        IH2='RLL '
        VALUE0=YTHEL
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='ACTU'
        IH2='ALUL'
        VALUE0=YACTU
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='THEO'
        IH2='RUL '
        VALUE0=YTHEU
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='EXPL'
        IH2='OSS '
        VALUE0=YEXPLO
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAA5')THEN
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'CAA5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPCAA5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCAPA(IHARG,IARGT,ARG,NUMARG,
     1                  PXSTAR,PYSTAR,PXEND,PYEND,
     1                  ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,
     1                  IREFSW,IREFCO,IREFC2,
     1                  IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG,
     1                  IGRASW,IDIASW,
     1                  PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1                  PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1                  NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IDNVOF,IDNHOF,IDFONT,UNITSW,PDSCAL,
     1                  IBACCO,IBACC2,
     1                  IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DRAW ONE OR MORE CAPACITORS (DEPENDING ON HOW MANY NUMBERS ARE
C              PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
C              OF 0 TO 100.
C     NOTE--THE INPUT COORDINATES DEFINE THE BACK CENTER AND THE FRONT CENTER
C           OF THE CAPACITOR.
C     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2
C          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
C     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN CAPACITOR WILL GO FROM
C           THE LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER ABSOLUTE OR
C           RELATIVE) AS DEFINED BY THE 2 NUMBERS.
C     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN CAPACITOR WILL GO FROM
C           THE ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST 2 NUMBERS TO
C           THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
C           THIRD AND FOURTH NUMBERS.
C     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN CAPACITOR WILL GO FROM
C           THE (X,Y) POSITION AS RESULTING FROM THE THIRD AND FOURTH NUMBERS
C           TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY
C           THE FIFTH AND SIXTH NUMBERS.
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
C                 PHONE--301-975-2899
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
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
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
      DIMENSION ILINC2(MAXLNZ,3)
      DIMENSION PLINTH(*)
C
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
      DIMENSION IBACC2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
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
        WRITE(ICOUT,88)IBUGG4,ISUBG4,IERRG4,IBUGD2,IFOUND,IERROR
   88   FORMAT('IBUGG4,ISUBG4,IERRG4,IBUGD2,IFOUND,IERROR = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFIG='CAPA'
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
C
      CALL DPOPPL(IGRASW,IBELSW,NUMRIN,IERASW,IBACCO,IBACC2)
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
 1131 FORMAT('***** ERROR IN DPCAPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR CAPICATOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
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
      CALL DPCAP2(X1,Y1,X2,Y2,
     1            IFIG,
     1            ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,
     1            AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,
     1            IREFSW,IREFCO,IREFC2,
     1            IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,
     1            PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
      X1=X2
      Y1=Y2
C
      GOTO1160
 1190 CONTINUE
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
     1            PGRAXF,PGRAYF,
     1            IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1            PDIAHE,PDIAWI,PDIAVG,PDIAHG)
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
C
      RETURN
      END
      SUBROUTINE DPCAP2(X1,Y1,X2,Y2,
     1                  IFIG,
     1                  ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,
     1                  IREFSW,IREFCO,IREFC2,
     1                  IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
C     PURPOSE--DRAW AN CAPACITOR WITH THE BACK CENTER AT (X1,Y1)
C              AND THE FRONT CENTER AT (X2,Y2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----NON-COMMON VARIABLES-------------------------------------
C
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
C
      CHARACTER*4 IPATT
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
C
      DIMENSION PX(10)
      DIMENSION PY(10)
C
      DIMENSION ILINPA(*)
      DIMENSION ILINCO(*)
      DIMENSION ILINC2(MAXLN,3)
      DIMENSION PLINTH(*)
C
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
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
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
   63   FORMAT('IREBLI(1),IREBCO(1),PREBTH(1),AREGBA(1) = ',
     1         2(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64   FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),
     1                 PREPSP(1)
   65   FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',
     1         3(A4,2X),2G15.7)
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
C
C               *********************************
C               **  STEP 1--                   **
C               **  DETERMINE THE COORDINATES  **
C               **  FOR THE CAPACITOR          **
C               *********************************
C
      DELX=X2-X1
      DELY=Y2-Y1
      LEN=INT(SQRT((X2-X1)**2+(Y2-Y1)**2))
      ALEN=LEN
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
C
      AJXMIN=PTEXWI
      AJXDEL=PTEXWI
      AJYDEL=PTEXHE
      AJXMAX=ALEN-AJXDEL
C
      XMIN=AJXMIN
      XDEL=AJXDEL
      YDEL=AJYDEL
      XMAX=AJXMAX
C
      K=0
C
      X=0
CCCCC Y=-ALEN/2.0
      Y=(-YDEL/2.0)
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      X=0
CCCCC Y=ALEN/2.0
      Y=YDEL/2.0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      NP=K
C
      IPATT=ILINPA(1)
      PTHICK=PLINTH(1)
      ICOL=ILINCO(1)
      ICOLR=ILINC2(1,1)
      ICOLG=ILINC2(1,2)
      ICOLB=ILINC2(1,3)
      IFLAG='ON'
      CALL DPDRPL(PX,PY,NP,
     1            IFIG,IPATT,PTHICK,
     1            ICOL,ICOLR,ICOLG,ICOLB,
     1            JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
      K=0
C
      X=ALEN
CCCCC Y=-ALEN/2.0
      Y=(-YDEL/2.0)
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      X=ALEN
CCCCC Y=ALEN/2.0
      Y=YDEL/2.0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      NP=K
C
      IFLAG='ON'
      CALL DPDRPL(PX,PY,NP,
     1            IFIG,IPATT,PTHICK,
     1            ICOL,ICOLR,ICOLG,ICOLB,
     1            JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'CAP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCAP2--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9039)IERRG4,NP
 9039   FORMAT('IERRG4,NP = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCAPT(ICOM,ICOM2,
     1                 ICAPSW,ICAPTY,ICAPSC,IPRDEF,
     1                 IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                 IANSLC,IANS,IWIDTH,
     1                 IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1                 IOFILE,IBACCO,IBACC2,IGRASW,IDIASW,
     1                 PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1                 PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1                 NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                 IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                 IDNVOF,IDNHOF,IDFONT,PDSCAL,
     1                 IREPCH,IMPSW,
     1                 IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--INITIATE/TERMINATE A CAPTURE FILE FOR CAPTURING/REDIRECTING
C              ALPHANUMERIC OUTPUT (ONLY)--NOT EFFECT GRAPHICS OUTPUT.
C              THERE ARE 2 CAPABILITITES IN THIS REGARD--
C                 1) TURN THE CAPTURE SWITCH 'ON' WHICH WILL
C                    ALLOW A CAPTURE FILE TO BE OPENED.
C                 2) TURN THE CAPTURE SWITCH 'OFF' WHICH WILL TERMINATE
C                    THE ENTRY OF TEXT OUTPUT INTO THE CAPTURE FILE.
C     NOTE--THESE CAPABILITITIES WILL ALLOW THE ALPHANUMERIC OUTPUT (NOT
C           GRAPHICS OUTPUT) FROM ANY DATAPLOT COMMAND TO BE CAPTURED
C           (OR REDIRECTED) TO ANY FILE.  ALL SUBSEQUENT DATAPLOT
C           ALPHANUMERIC OUTPUT ARE AUTOMATICALLY DIVERTED FROM THE SCREEN
C           TO THE SPECIFIED SYSTEM FILE OR SUBFILE.  WHEN THE CAPTURE
C           SWITCH IS OFF, NO SUCH DIVERSION IS DONE.  THE SPECIFIED
C           STATUS (ON/OFF) OF THE CAPTURE WILL BE PLACED IN THE VARIABLE
C           ICAPSW.
C     INPUT  ARGUMENTS--ICOM
C                     --ICOM2
C                     --ICAPSW
C                     --ICAPTY
C                     --IANSLC (A  HOLLERITH VECTOR WHOSE
C                              I-TH ELEMENT CONTAINS THE
C                              I-TH CHARACTER OF THE
C                              ORIGINAL INPUT COMMAND LINE.
C                     --IWIDTH (AN INTEGER VARIABLE WHICH
C                              CONTAINS THE NUMBER OF CHARACTERS
C                              IN THE ORIGINAL COMMAND LINE.
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IBUG   (A HOLLERITH VARIABLE
C                               FOR DEBUGGING
C     PRIMARY CHANGED VARIABLE--IPR (IN COMMON)
C     OUTPUT ARGUMENTS--ICAPSW (AN INTEGER VARIABLE
C                              WHICH IF 'ON' INDICATES THAT
C                              CURRENT COMMANDS ARE
C                              BEING DIVERTED
C                              TO A CAPTURE TEXT; AND
C                              IF OFF INDICATES THAT
C                              A CAPTURE FILE IS NOT BEING CONSTRUCTED.
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/6
C     ORIGINAL VERSION--JUNE      1989.
C     UPDATED         --JUNE      2002.  ADD SUPPORT FOR:
C                                        CAPTURE FLUSH
C                                        CAPTURE HTML FILE.
C                                        CAPTURE LATEX FILE.
C     UPDATED         --JANUARY   2003.  FOR CAPTURE HTML, OPTIONALLY
C                                        READ HEADER AND FOOTER FILES
C     UPDATED         --JULY      2003.  BUG: FILE NAME < 80
C                                        CHARACTERS, BUT COMMAND LINE
C                                        > 80 CHARACTERS
C     UPDATED         --SEPTEMBER 2003.  START IMPLEMENTING THE LATEX
C                                        CODE
C     UPDATED         --FEBRUARY  2005.  START IMPLEMENTING THE RTF
C                                        CODE
C     UPDATED         --DECEMBER  2005.  SUSPEND/RESUME CASES
C     UPDATED         --JANUARY   2006.  CAPTURE SCREEN <ON/OFF>
C     UPDATED         --FEBRUARY  2006.  ADD EPIC, EEPIC, GRAPHICS
C                                        PACKAGES TO LATEX PRE-AMBLE
C     UPDATED         --NOVEMBER  2008.  INITIALIZE HTML44 COMMON BLOCK
C     UPDATED         --APRIL     2012.  CAPTURE SCRIPT
C     UPDATED         --APRIL     2012.  CAPTURE FLUSH ERASE <ON/OFF>
C     UPDATED         --AUGUST    2015.  CAPTURE FUNCTION BLOCK
C     UPDATED         --DECEMBER  2015.  "NONE" OPTION FOR LATEX/HTML
C                                        HEADERS AND FOOTERS
C     UPDATED         --DECEMBER  2015.  SET CAPTURE SPLIT ON OPTION
C     UPDATED         --AUGUST    2016.  CAPTURE STATISTIC BLOCK
C     UPDATED         --DECEMBER  2018.  ADD PDSCAL TO CALL LIST TO DPERAS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
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
C
      CHARACTER*240 IATEMP
      CHARACTER*1   ITEMP
C
      CHARACTER*1 IREPCH
      CHARACTER*4 IMPSW
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC CHARACTER*80 IFILE
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
C
CCCCC CHARACTER*80 IFILE2
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER*12 ISTAT2
      CHARACTER*12 IFORM2
      CHARACTER*12 IACCE2
      CHARACTER*12 IPROT2
      CHARACTER*12 ICURS2
      CHARACTER*4 IERRF2
      CHARACTER*4 IENDF2
      CHARACTER*4 IREWI2
C
      CHARACTER*4 IANSI
CCCCC CHARACTER*80 ICANS
      CHARACTER*200 ICANS
C
C ---------------------------------------------------------------------
C
      DIMENSION IANSLC(*)
      DIMENSION IANS(*)
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
      CHARACTER*4 IBACCO
      CHARACTER*4 IGRASW
      CHARACTER*4 IDIASW
C
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
C
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDCOLO
      CHARACTER*4 IDFONT
C
      CHARACTER*4 IFLAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFILQ2
      CHARACTER*1 IBASLC
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
      DIMENSION PDSCAL(*)
      DIMENSION IBACC2(*)
C
C-----COMMON----------------------------------------------------------
C
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
C
      COMMON/HTML44/IFNTSZ
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOFB.INC'
      INCLUDE 'DPCOSB.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCA'
      ISUBN2='PT  '
      IFOUND='YES'
      IERROR='NO'
C
      IFILQ2=IFILQU
      IFILQU='ON'
      IH='UNKN'
      IH2='UNKN'
C
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
C
      P2=0.0
C
      CALL DPCONA(92,IBASLC)
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCAPT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICAPSW,ICAPTY,ICAPNU,ICAPCS,IPR,IPRDEF,NUMARG
   52   FORMAT('ICAPSW,ICAPTY,ICAPNU,ICAPCS,IPR,IPRDEF,NUMARG = ',
     1         2(A4,2X),I8,2X,A12,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGS2,IERROR,ICOM,ICOM2,IWIDTH
   54   FORMAT('IBUGS2,IERROR,ICOM,ICOM2,IWIDTH = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)(IANSLC(I),I=1,MIN(120,IWIDTH))
   55   FORMAT('IANSLC(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
        IF(NUMARG.GT.0)THEN
          DO57I=1,NUMARG
            WRITE(ICOUT,58)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   58       FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1             I8,3(2X,A4),I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   57     CONTINUE
        ENDIF
        WRITE(ICOUT,62)NUMNAM,MAXNAM,NUMCHA,ICAPNU
   62   FORMAT('NUMNAM,MAXNAM,NUMCHA,ICAPNU = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO65I=1,NUMNAM
          WRITE(ICOUT,66)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                   IVALUE(I),VALUE(I)
   66     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1           I8,3(2X,A4),I8,G15.7)
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
C
C               ****************************************************
C               **  STEP 11--                                     **
C               **  FOR THE SPECIAL CASE WHEN THE CAPTURING       **
C               **  OF ALPHA TEXT HAS JUST BEEN FINISHED, JUMP    **
C               **  TO CLOSING THE FILE                           **
C               ****************************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICAPCS.EQ.'CLO2        ')GOTO5000
C
C               ***********************************************
C               **  STEP 12--                                **
C               **  FOR THE SPECIAL CASE WHEN HAVE THE       **
C               **  END CAPTURE     COMMAND, OR THE          **
C               **  END REDIRECT      COMMAND, OR THE        **
C               **  END OF CAPTURE      COMMAND,             **
C               **  END OF REDIRECT       COMMAND,           **
C               **  JUMP IMMEDIATELY TO THE SECTION OF CODE  **
C               **  WHICH PUTS ON AN END OF FILE AND         **
C               **  CLOSES THE FILE/SUBFILE.                 **
C               ***********************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'END ')THEN
        IF(NUMARG.LE.0)GOTO9000
        IF(IHARG(1).EQ.'CAPT')GOTO4000
        IF(IHARG(1).EQ.'REDI')GOTO4000
        IF(IHARG(1).EQ.'DIVE')GOTO4000
        IF(IHARG(1).EQ.'PIPE')GOTO4000
        IF(NUMARG.LE.1)GOTO9000
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'CAPT')GOTO4000
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'REDI')GOTO4000
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'DIVE')GOTO4000
        IF(IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'PIPE')GOTO4000
        GOTO9000
      ELSEIF(ICOM.EQ.'FLUS')THEN
        IF(NUMARG.LE.0)GOTO1290
        IF(IHARG(1).EQ.'CAPT')GOTO6000
      ELSEIF(ICOM.EQ.'CAPT')THEN
        IF(NUMARG.LE.0)GOTO1290
        IF(IHARG(1).EQ.'FLUS')GOTO6000
      ENDIF
C
 1290 CONTINUE
C
C               ********************************************************
C               **  STEP 13--                                         **
C               **  DETERMINE THE TYPE CASE--                         **
C               **       1) CREATE AN EXPLICIT CAPTURE FILE;          **
C               **       2) OMIT THE FILE NAME;                       **
C               **  NOTE--IOFILE  WILL EQUAL 'YES' ONLY IN FILE CASE. **
C               **  IN OTHER WORDS, THIS STEP MAKES SURE              **
C               **  THAT A FILE NAME IS EXISTENT AFTER THE            **
C               **  CAPTURE   AND   REDIRECT   COMMANDS.              **
C               ********************************************************
C
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'SUSP')GOTO2000
      IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'OFF ')GOTO2000
      IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'RESU')GOTO2000
      IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'ON  ')GOTO2000
      IF(NUMARG.GE.1 .AND. IHARG(1).EQ.'SCRE')GOTO2000
      IF(NUMARG.GE.2 .AND. IHARG(1).EQ.'FUNC' .AND.
     1   IHARG(2).EQ.'BLOC')GOTO2000
      IF(NUMARG.GE.2 .AND. IHARG(1).EQ.'STAT' .AND.
     1   IHARG(2).EQ.'BLOC')GOTO2000
C
C     2015/11: CHECK IF CAPTURE SWITCH IS ALREADY ON
C
      IF(ICAPSW.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,1211)
 1211     FORMAT('***** WARNING IN CAPTURE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1212)
 1212     FORMAT('      THE CAPTURE SWITCH IS ALREADY ON.  NOTHING ',
     1           'DONE.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='WARN'
        GOTO9000
      ENDIF
C
      IWORD=2
      IF(IHARG(1).EQ.'HTML'.OR.IHARG(1).EQ.'LATE'.OR.
     1   IHARG(1).EQ.'RTF '.OR.IHARG(1).EQ.'SCRI')IWORD=3
      CALL DPFILE(IANSLC,IWIDTH,IWORD,
     1            IOFILE,IBUGS2,ISUBRO,IERROR)
C
C               **********************************************
C               **  STEP 14--                               **
C               **  IF NO FILE NAME GIVEN,                  **
C               **  THEN GENERATE AN ERROR MESSAGE.         **
C               **********************************************
C
      ISTEPN='14'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOFILE.NE.'YES')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1411)
 1411   FORMAT('***** ERROR IN CAPTURE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1412)
 1412   FORMAT('      THE DESIRED CAPTURE OPERATION CANNOT BE ',
     1         'PERFORMED BECAUSE')
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
        GOTO9000
      ENDIF
C
C               *************************************
C               **  STEP 15--                      **
C               **  IF HAVE THE FILE INPUT CASE    **
C               **  (WHICH WE MUST HAVE)--         **
C               **  COPY OVER VARIABLES            **
C               *************************************
C
      ISTEPN='15'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=ICAPNU
      IFILE=ICAPNA
      ISTAT=ICAPST
      IF(IFILE.EQ.ISYSNA)ISTAT=ISYSST
      IF(IFILE.EQ.ILOGNA)ISTAT=ILOGST
      IFORM=ICAPFO
      IACCES=ICAPAC
      IPROT=ICAPPR
C     (SEE ADDITIONAL RESETTING OF   IPROT   BELOW
C     IF HAVE THE SYSTEM LOGIN AND/OR THE LOCAL LOGIN CAPTURE FILES)
      ICURST=ICAPCS
C
      ISUBN0='CAPT'
      IERRFI='NO'
C
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
C
C               ***********************************************
C               **  STEP 16--                                **
C               **  IF HAVE THE FILE CASE (WHICH WE MUST     **
C               **  HAVE)--CHECK TO SEE IF THE CAPTURE FILE  **
C               **  MAY EXIST                                **
C               ***********************************************
C
      ISTEPN='16'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
        GOTO9000
      ENDIF
C
C               ********************************
C               **  STEP 17--                 **
C               **  EXTRACT THE FILE NAME.    **
C               **  THIS IS NEEDED FOR MOST   **
C               **  (BUT NOT ALL) VARIATIONS  **
C               **  OF THE CAPTURE COMMAND.   **
C               ********************************
C
      ISTEPN='17'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC JUNE 2002.  CHECK TO SEE IF FIRST ARGUMENT IS:
CCCCC             HTML
CCCCC             LATEX
CCCCC             RTF            (FEBRUARY 2005)
CCCCC             SCRIPT         (APRIL    2012)
C
      NSTRT=1
C
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
 1793   FORMAT('THE CAPTURE OUTPUT WILL BE WRITTEN IN ',
     1         'RTF (RICH TEXT FORMAT) FORMAT.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IHARG(1).EQ.'SCRI')THEN
        ICAPTY='SCRI'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1795)
 1795   FORMAT('SCRIPT MODE TURNED ON FOR CAPTURE.  ALL ENTERED ',
     1         'COMMANDS WILL BE ECHOED, BUT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1797)
 1797   FORMAT('NOT EXECUTED, TO THE CAPTURE FILE UNTIL AN  ',
     1         'END OF CAPTURE  COMMAND IS ENTERED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO1710I=1,200
        IANSI=IANSLC(I)
        ICANS(I:I)=IANSI(1:1)
 1710 CONTINUE
C
      ISTART=1
      ISTOP=IWIDTH
      IWORD=2
      IF(ICAPTY.EQ.'HTML')IWORD=3
      IF(ICAPTY.EQ.'LATE')IWORD=3
      IF(ICAPTY.EQ.'RTF ')IWORD=3
      IF(ICAPTY.EQ.'SCRI')IWORD=3
      CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1            ICOL1,ICOL2,IFILE,NCFILE,
     1            IBUGS2,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(NCFILE.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1742)
 1742   FORMAT('      A USER FILE NAME IS REQUIRED IN THE ',
     1         'CAPTURE/REDIRECT COMMANDS')
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
        GOTO9000
      ENDIF
C
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFILE.EQ.ISYSNA)IPROT=ISYSPR
      IF(IFILE.EQ.ILOGNA)IPROT=ILOGPR
C
C               *******************************************
C               **  STEP 20--                            **
C               **  CHECK THE DESIRED CAPTURE OPERATION  **
C               **  (ON, OFF, OR EXECUTE).               **
C               *******************************************
C
 2000 CONTINUE
C
      ISTEPN='20'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'CAPT' .OR. ICOM.EQ.'REDI' .OR. ICOM.EQ.'DIVE' .OR.
     1   ICOM.EQ.'PIPE')THEN
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'SUSP')GOTO3800
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'OFF ')GOTO3800
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'RESU')GOTO3900
        IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'ON  ')GOTO3900
C
C       2015/08: FUNCTION BLOCK CASE
C
        IF(NUMARG.GE.2 .AND. IHARG(1).EQ.'FUNC' .AND.
     1    IHARG(2).EQ.'BLOC')THEN
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
 2117       FORMAT('ARGUMENT MUST BE ONE OF: ONE (OR 1), TWO (OR 2),',
     1             ' OR THREE (OR 3).')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2118)IHARG(4)
 2118       FORMAT('THE ENTERED ARGUMENT WAS: ',A4)
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
C         NOW PARSE THE ARGUMENTS
C
C         FIRST, RETRIEVE THE NAME OF THE FUNCTION BLOCK.
C
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
22116       FORMAT('FOR THE  CAPTURE FUNCTION BLOCK  COMMAND, NO ',
     1            'NAME WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
C         NEXT, RETRIEVE THE NAME OF THE PARAMETER/VARIABLE THAT WILL
C         CONTAIN THE RESPONSE (I.E., THE CALLING ROUTINE WILL EXTRACT
C         THE VALUE OF THIS PARAMETER/VARIABLE AFTER EXECUTING THE
C         FUNCTION BLOCK).
C
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
23126       FORMAT('FOR THE  CAPTURE FUNCTION BLOCK  COMMAND, NO ',
     1            'RESPONSE PARAMETER/VARIABLE WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
C         THE REMAINING ARGUMENTS ARE THE PARAMETERS NEEDED BY THE
C         FUNCTION BLOCK,
C
          IF(NUMARG.GE.6)THEN
            ICNT=0
            DO2150II=6,NUMARG
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
22136       FORMAT('FOR THE  CAPTURE FUNCTION BLOCK  COMMAND, NO ',
     1            'PARAMETER LIST WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
          GOTO9000
C
C         2016/08: STATISTIC BLOCK CASE
C
        ELSEIF(NUMARG.GE.2 .AND. IHARG(1).EQ.'STAT' .AND.
     1    IHARG(2).EQ.'BLOC')THEN
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
 2127       FORMAT('ARGUMENT MUST BE ONE OF: ONE (OR 1), TWO (OR 2),',
     1             ' OR THREE (OR 3).')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2128)IHARG(4)
 2128       FORMAT('THE ENTERED ARGUMENT WAS: ',A4)
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
C         NOW PARSE THE ARGUMENTS
C
C         FIRST, RETRIEVE THE NAME OF THE STATISTIC BLOCK.
C
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
22126       FORMAT('FOR THE  CAPTURE STATISTIC BLOCK  COMMAND, NO ',
     1            'NAME WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
C         NEXT, RETRIEVE THE NAME OF THE PARAMETER THAT WILL
C         CONTAIN THE RESPONSE (I.E., THE CALLING ROUTINE WILL EXTRACT
C         THE VALUE OF THIS PARAMETER AFTER EXECUTING THE
C         STATISTIC BLOCK).
C
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
22127       FORMAT('FOR THE  CAPTURE STATISTIC BLOCK  COMMAND, NO ',
     1            'RESPONSE PARAMETER WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
C         THE REMAINING ARGUMENTS ARE THE PARAMETERS NEEDED BY THE
C         STATISTIC BLOCK,
C
          IF(NUMARG.GE.6)THEN
            ICNT=0
            DO2180II=6,NUMARG
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
22236       FORMAT('FOR THE  CAPTURE STATISTIC BLOCK  COMMAND, NO ',
     1            'PARAMETER LIST WAS SPECIFIED.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
          GOTO9000
        ENDIF
C
        IF(NUMARG.GE.1 .AND. IHARG(1).EQ.'SCRE')THEN
          ICAPSC='ON'
          IF(NUMARG.GE.2 .AND. 
     1      (IHARG(2).EQ.'OFF ' .OR. IHARG(2).EQ.'END ' .OR.
     1       IHARG(2).EQ.'NO  ' .OR. IHARG(2).EQ.'NONE' .OR.
     1       IHARG(2).EQ.'CLOS'))ICAPSC='OFF '
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IF(IFEEDB.EQ.'ON')THEN
            IF(ICAPSC.EQ.'ON')THEN
              WRITE(ICOUT,2111)
 2111         FORMAT('CAPTURE OUTPUT WILL BE WRITTEN TO BOTH THE ',
     1               'CAPTURE FILE AND THE SCREEN.')
            ELSE
              WRITE(ICOUT,2113)
 2113         FORMAT('CAPTURE OUTPUT WILL BE WRITTEN TO THE ',
     1              'CAPTURE FILE ONLY.')
            ENDIF
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GOTO9000
        ENDIF
        GOTO3000
      ELSEIF(ICOM.EQ.'END '.AND.ICOM2.EQ.'    ')THEN
        IF(NUMARG.GE.1 .AND.
     1    (IHARG(1).EQ.'CAPT' .OR. IHARG(1).EQ.'REDI' .OR.
     1     IHARG(1).EQ.'DIVE' .OR. IHARG(1).EQ.'PIPE'))GOTO4000
        IF(NUMARG.GE.1 .AND. IHARG(1).EQ.'OF  ' .AND.
     1    (IHARG(1).EQ.'CAPT' .OR. IHARG(1).EQ.'REDI' .OR.
     1     IHARG(1).EQ.'DIVE' .OR. IHARG(1).EQ.'PIPE'))GOTO4000
      ENDIF
C
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
      GOTO9000
C
C               ********************************************************
C               **  STEP 30--                                         **
C               **  TREAT THE CAPTURE CASE.                           **
C               **  CARRY OUT WHATEVER SYSTEM OPERATIONS ARE NEEDED   **
C               **  IN ORDER TO OPERATE ON THE FILE OR SUBFILE.       **
C               **  FOR MOST INSTALLATIONS, THIS REQUIRES             **
C               **      1) AN OPENING OF THE FILE OR SUBFILE;         **
C               **      2) AN EQUIVALENCING OF THE FILE OR SUBFILE;   **
C               **      3) A  REWINDING OF THE FILE OR SUBFILE.       **
C               **  THE CODE BELOW OPENS THE FILE OR SUBFILE          **
C               **  (VIA @ASG,AX ON THE UNIVAC 1108).  THE CODE ALSO  **
C               **  EQUIVALENCES THE FILES OR SUBFILES (VIA @USE O    **
C               **  UNIVAC 1108) TO THE FORTRAN LOGICAL UNIT NUMBER   **
C               **  DESIGNATION IN THE VARIABLE ICAPNU (IN THE        **
C               **  SUBROUINTE INITFO);  THE CODE ALSO REWINDS THE    **
C               **  FILE OR SUBFILE. (VIA @REWIND ON THE UNIVAC 1108).**
C               ********************************************************
C
 3000 CONTINUE
      ISTEPN='30'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICAPSW='ON'
      IOUNIT=ICAPNU
C
      ICAPNA=IFILE
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
      ICAPCS=ICURST
C
C     2015/12: IF "SET CAPTURE SPLIT ON" IS GIVEN, OPEN A SECONDARY
C              CAPTURE FILE WITH "_1" APPENDED TO FILE NAME (BEFORE THE
C              ".").
C
      IF(ICAPSP.EQ.'ON')THEN
        ICAPCN=1
        ICAPN2=' '
        ICAPN2(1:80)=ICAPNA(1:80)
        ILAST=80
        IPEROD=-1
        DO3001JJ=80,1,-1
          IF(ICAPN2(JJ:JJ).NE.' ')THEN
            ILAST=JJ
            GOTO3002
          ENDIF
 3001   CONTINUE
 3002   CONTINUE
C
        DO3006JJ=80,1,-1
          IF(ICAPN2(JJ:JJ).EQ.'.')THEN
            IPEROD=JJ
            GOTO3007
          ENDIF
 3006   CONTINUE
 3007   CONTINUE
C
        IF(IPEROD.LE.0)THEN
          ILAST=ILAST+1
          ICAPN2(ILAST:ILAST+1)='.1'
        ELSE
          DO3008JJ=ILAST,IPEROD,-1
            ICAPN2(JJ+2:JJ+2)=ICAPN2(JJ:JJ)
 3008     CONTINUE
          ICAPN2(IPEROD:IPEROD+1)='_1'
        ENDIF
        IREWIN='ON'
        OPEN(ICPNU2,FILE=ICAPN2,STATUS="UNKNOWN",ACTION="WRITE")
        IF(IERRFI.EQ.'YES')GOTO9000
      ENDIF
C
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
C
      IPR=ICAPNU
C
CCCCC JUNE 2002.  SPECIAL CASE OF GRAPHICS, LATEK, HTML, RTF OR SCRIPT.
CCCCC ADD ANY SPECIAL NEEDED INITIALIZATION CODE HERE.
C
CCCCC JANUARY 2003.  SET HTML HEADER FILE CAN BE USED TO SPECIFY A
CCCCC A FILE TO INCORPORATE THE HEADER FILE.
C
CCCCC DECEMBER 2015.  ADD OPTION "NONE" THAT SPECIFIES THAT NO HEADER
CCCCC IS GENERATED.  THIS IS SLIGHTLY DISTINCT FROM "NULL" WHICH
CCCCC GENERATES A MINIMAL HEADER.  THE "NONE" OPTION IS INTENDED FOR
CCCCC THE CASE WHERE YOU WANT TO INCORPORATE THE HTML CODE INTO A
CCCCC LARGER DISTINCT DOCUMENT.
C
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
 3081     FORMAT('<META HTTP-EQUIV="Content-Type" CONTENT="text/html;',
     1           ' charset=iso-8859-1">')
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
C
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO9000
C
C  NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
C
          DO3091I=1,1000
            IATEMP=' '
            READ(IOUNI2,3092,END=3099,ERR=3099)IATEMP
 3092       FORMAT(A240)
            ILAST=1
            DO3096J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GOTO3098
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
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO9000
        ENDIF
        WRITE(ICOUT,3087)
 3087   FORMAT('<PRE>')
        CALL DPWRST('XXX','WRIT')
      ELSEIF(ICAPTY.EQ.'LATE')THEN
C
CCCCC DECEMBER 2015.  ADD OPTION "NONE" THAT SPECIFIES THAT NO HEADER
CCCCC IS GENERATED.  THIS IS SLIGHTLY DISTINCT FROM "NULL" WHICH
CCCCC GENERATES A MINIMAL HEADER.  THE "NONE" OPTION IS INTENDED FOR
CCCCC THE CASE WHERE YOU WANT TO INCORPORATE THE LATEX CODE INTO A
CCCCC LARGER DISTINCT DOCUMENT.
C
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
 3181     FORMAT(A1,'newcommand{',A1,'PGRAPHIC}[1]{',A1,'begin{figure}',
     1           '[h]')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3182)IBASLC
 3182     FORMAT(23X,A1,'epsfig{file=#1,width=6.0in}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3183)IBASLC
 3183     FORMAT(23X,A1,'end{figure}}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3186)IBASLC,IBASLC,IBASLC
 3186     FORMAT(A1,'newcommand{',A1,'LGRAPHIC}[1]{',A1,'begin{figure}',
     1           '[h]')
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
C
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO9000
C
C  NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
C
          DO3291I=1,1000
            IATEMP=' '
            READ(IOUNI2,3292,END=3299,ERR=3299)IATEMP
 3292       FORMAT(A240)
            ILAST=1
            DO3296J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GOTO3298
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
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO9000
          WRITE(ICOUT,3197)IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
      ELSEIF(ICAPTY.EQ.'RTF ')THEN
        IRTFMD='OFF'
CCCCC   IF(IRTFHE.EQ.'NULL')THEN
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
C
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
 3387     FORMAT('{',A1,'company Statistical Engineering Division, ',
     1           'NIST}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3379)
          CALL DPWRST('XXX','WRIT')
C
CCCCC     IPTSZ=2*IRTFPS
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
            WRITE(ICOUT,3381)IBASLC,IBASLC,IBASLC,IBASLC,ITEMP,
     1                       IBASLC,IPTSZ
 3381       FORMAT(A1,'delang1033',A1,'widowctrl',A1,'plain',
     1             A1,'f',A1,A1,'fs',I1)
          ELSE
            WRITE(ICOUT,3382)IBASLC,IBASLC,IBASLC,IBASLC,ITEMP,
     1                       IBASLC,IPTSZ
 3382       FORMAT(A1,'delang1033',A1,'widowctrl',A1,'plain',
     1             A1,'f',A1,A1,'fs',I2)
          ENDIF
          CALL DPWRST('XXX','WRIT')
C
          WRITE(ICOUT,3389)IBASLC
 3389     FORMAT('{',A1,'pard')
          CALL DPWRST('XXX','WRIT')
          IRTFMD='VERB'
CCCCC   ELSE
CCCCC   ENDIF
      ELSEIF(ICAPTY.EQ.'SCRI')THEN
        CONTINUE
      ENDIF
C
      GOTO9000
C
C               ******************************************************
C               **  STEP 38--                                       **
C               **  TREAT THE CAPTURE SUSPEND CASE.                 **
C               **  RESET OUTPUT UNIT TO IPR, BUT DO NOT CLOSE      **
C               **  THE CAPTURE FILE.                               **
C               ******************************************************
C
 3800 CONTINUE
      ISTEPN='38'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICAPSW.EQ.'OFF')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3811)
 3811   FORMAT('****** WARNING: THE CAPTURE SWITCH IS CURRENTLY OFF.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3813)
 3813   FORMAT('       CAPTURE SUSPEND COMMAND IGNORED.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      ICAPSW='OFF'
      IOUNIT=ICAPNU
      IPR=IPRDEF
C
      GOTO9000
C
C               ******************************************************
C               **  STEP 39--                                       **
C               **  TREAT THE CAPTURE RESUME  CASE.                 **
C               **  RESET OUTPUT UNIT TO CAPTURE UNIT, BUT DO NOT   **
C               **  REOPEN THE CAPTURE FILE.                        **
C               ******************************************************
C
 3900 CONTINUE
      ISTEPN='39'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICAPSW.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3911)
 3911   FORMAT('****** WARNING: THE CAPTURE SWITCH IS CURRENTLY ON.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3913)
 3913   FORMAT('       CAPTURE RESUME COMMAND IGNORED.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      ICAPSW='ON'
      IPR=ICAPNU
C
      GOTO9000
C
C               **********************************************************
C               **  STEP 40--                                           **
C               **  TREAT THE END OF CAPTURE CASE.  CARRY OUT WHATEVER  **
C               **  SYSTEM OPERATIONS ARE NEEDED IN ORDER TO OPERATE    **
C               **  ON THE FILE OR SUBFILE. FOR MOST INSTALLATIONS,     **
C               **  THIS REQUIRES                                       **
C               **      1) A PLACING OF AN END MARK OF THE FILE OR      **
C               **         SUBFILE;                                     **
C               **      2) A FREEING (DEASSIGNING) OF THE FILE OR       **
C               **         SUBFILE;                                     **
C               **********************************************************
C
 4000 CONTINUE
      ISTEPN='40'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICAPSW='OFF'
C
      IF(IFBLSW.NE.'OFF')THEN
        ICAPTY='TEXT'
        IFBLSW='OFF'
        GOTO4090
      ENDIF
C
      IF(ISBLSW.NE.'OFF')THEN
        ICAPTY='TEXT'
        ISBLSW='OFF'
        GOTO4090
      ENDIF
C
CCCCC JUNE 2002.  SPECIAL CASE OF GRAPHICS, LATEK, OR HTML.  ADD
CCCCC ANY SPECIAL NEED TERMINATION CODE HERE.
C
CCCCC DECEMBER 2015.  FOR HTML AND LATEX, ADD "NONE" OPTION FOR FOOTER.
CCCCC THIS IS DISTINCT FROM "NULL" WHICH ADDS A MINIMAL FOOTER.  THE
CCCCC "NONE" OPTION IS INTENDED FOR THE CASE WHERE THE HTML OR LATEX
CCCCC CODE IS TO BE INCORPORATED INTO A LARGER HTML OR LATEX DOCUMENT.
C
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
C
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO9000
C
C  NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
C
          DO4121I=1,1000
            IATEMP=' '
            READ(IOUNI2,4122,END=4129,ERR=4129)IATEMP
 4122       FORMAT(A240)
            ILAST=1
            DO4126J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GOTO4128
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
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO9000
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
C
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO9000
C
C  NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
C
          DO4221I=1,1000
            IATEMP=' '
            READ(IOUNI2,4222,END=4229,ERR=4229)IATEMP
 4222       FORMAT(A240)
            ILAST=1
            DO4226J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GOTO4228
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
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO9000
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
C
      ICAPTY='TEXT'
      IOUNIT=ICAPNU
      IPR=IPRDEF
C
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
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
 4013   FORMAT('ALL FUTURE TEXT OUTPUT WILL NOW REVERT TO ',
     1         'THE SCREEN.')
        CALL DPWRST('XXX','BUG ')
        IF(ICAPTY.EQ.'RTF ')IRTFMD='VERB'
      ENDIF
      GOTO9000
C
C               ****************************************************************
C               **  STEP 50--
C               **  TREAT THE CAPTURE FILE CLOSE CASE.
C               ****************************************************************
C
 5000 CONTINUE
      ISTEPN='50'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC ICAPSW='OFF'
CCCCC JUNE 2002. SUPPORT FOR SPECIAL CAPTURE OPERATIONS.
CCCCC IF(ICAPTY.EQ.'GRAP')THEN
CCCCC   IPR=IPRDEF
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
C
      ICAPTY='TEXT'
      IOUNIT=ICAPNU
C
      IENDFI='OFF'
C     ***** DO WE NEED THE FOLLOWING REWIND ????? *****
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'CAPT')GOTO5019
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5011)ICAPNU
 5011 FORMAT('THE CAPTURE FILE NUMBER ',I8,' HAS JUST BEEN CLOSED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5012)ICAPNA
 5012 FORMAT('NAME OF (JUST-CLOSED) CAPTURE FILE = ',A80)
      CALL DPWRST('XXX','BUG ')
 5019 CONTINUE
      GOTO9000
C
C     **********************************************************
C     **  STEP 60--                                           **
C     **  TREAT THE FLUSH  CAPTURE CASE.                      **
C     **      1) CLEAR GRAPHICS SCREEN (DPERAS)               **
C     **      2) CLOSE CAPTURE FILE (IF CURRENTLY OPEN)       **
C     **      3) OPEN THE CAPTURE FILE                        **
C     **      4) LOOP THROUGH THE FILE AND CALL DPWRSG        **
C     **      5) CLOSE THE CAPTURE FILE                       **
C     **      6) RE-OPEN THE CAPTURE FILE                     **
C     **********************************************************
C
 6000 CONTINUE
      ISTEPN='40'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CAPT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C
C  STEP 2: CLEAR THE GRAPHICS SCREEN
C          (SKIP IF MULTIPLOTTING ON)
C
      IF(IMPSW.NE.'ON' .AND. ICAPFE.EQ.'ON')THEN
        CALL DPERAS(IHARG,IARGT,IARG,NUMARG,
     1              IBACCO,IBACC2,IGRASW,IDIASW,
     1              PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1              PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1              NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1              IDNVOF,IDNHOF,IDFONT,PDSCAL,
     1              ICAPSW,IBUGS2,ISUBRO,IFOUND,IERROR)
      ENDIF
C
C  STEP 2: CLOSE THE FILE
C
      IOUNIT=ICAPNU
      IFILE=ICAPNA
      ISTAT=ICAPST
      IFORM=ICAPFO
      IACCES=ICAPAC
      IPROT=ICAPPR
      ICURST=ICAPCS
      ICURST=ICAPCS
      IF(ICAPCS.EQ.'CLOSED')GOTO6090
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
 6090 CONTINUE
C
C  STEP 3: RE-OPEN THE FILE
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
      ICAPCS=ICURST
C
C  STEP 4: LOOP THROUGH THE FILE
C
      ILINE=0
      ICOUNT=1
      DO6110I=1,10000
        ICOUT=' '
        READ(ICAPNU,'(A120)',END=6129,ERR=6119)ICOUT
        ILINE=ILINE+1
        IF(ILINE.GT.ICAPLI(ICOUNT).AND.IMPSW.NE.'ON')THEN
          CALL DPERAS(IHARG,IARGT,IARG,NUMARG,
     1                IBACCO,IBACC2,IGRASW,IDIASW,
     1                PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1                PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1                NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                IDNVOF,IDNHOF,IDFONT,PDSCAL,
     1                ICAPSW,IBUGS2,ISUBRO,IFOUND,IERROR)
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
        CALL DPWRSG('XXXX','BUG ',IREPCH,IMPSW,IFLAG,ICAPNM,ICAPBX,
     1              ILINE)
 6110 CONTINUE
 6119 CONTINUE
 6129 CONTINUE
C
C  STEP 5: CLOSE THE FILE
C
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      ICAPCS=ICURST
      IF(IERRFI.EQ.'YES')GOTO9000
C
C  STEP 6: RE-OPEN THE FILE
C
      IFILE=ICAPNA
      IOUNIT=ICAPNU
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
      ICAPCS=ICURST
C
      GOTO9000
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
C
      IFILQU=IFILQ2
C
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
C
      RETURN
      END
      SUBROUTINE DPCASE(ICOM,IHARG,NUMARG,
     1IDEFCA,
     1ITEXCA,
     1IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE CASE (UPPER OR LOWER) TYPE FOR
C              TITLE, LABEL, AND LEGEND SCRIPT
C              ON A PLOT.
C              THE CASE (UPPER OR LOWER) FOR THE SCRIPT WILL BE PLACED
C              IN THE CHARACTER VARIABLE ITEXCA.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFCA
C                     --IBUGD2
C     OUTPUT ARGUMENTS--ITEXCA
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   1993.  ACCEPT "ASIS" AS ARGUMENT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCA
      CHARACTER*4 ITEXCA
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
      INCLUDE 'DPCOP2.INC'
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
   51 FORMAT('***** AT THE BEGINNING OF DPCASE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICOM,NUMARG,IDEFCA
   53 FORMAT('ICOM,NUMARG,IDEFCA = ',A4,2X,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ************************************************
C               **  TREAT THE CASE (UPPER VERSUS LOWER) CASE  **
C               ************************************************
C
      IF(ICOM.EQ.'CASE')GOTO1120
      IF(ICOM.EQ.'UPPE')GOTO1130
      IF(ICOM.EQ.'LOWE')GOTO1140
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE
      IF(ICOM.EQ.'ASIS')GOTO1150
      GOTO9000
C
 1120 CONTINUE
      IF(NUMARG.LE.0)GOTO1161
      IF(IHARG(NUMARG).EQ.'ON')GOTO1161
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1162
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1161
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      IF(IHARG(NUMARG).EQ.'UPPE')GOTO1161
      IF(IHARG(NUMARG).EQ.'LOWE')GOTO1162
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE
      IF(IHARG(NUMARG).EQ.'ASIS')GOTO1163
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      GOTO1170
C
 1130 CONTINUE
      IF(NUMARG.LE.0)GOTO9000
      IF(IHARG(1).NE.'CASE')GOTO9000
      IF(NUMARG.LE.1)GOTO1161
      IF(IHARG(NUMARG).EQ.'ON')GOTO1161
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1162
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1161
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      GOTO9000
C
 1140 CONTINUE
      IF(NUMARG.LE.0)GOTO9000
      IF(IHARG(1).NE.'CASE')GOTO9000
      IF(NUMARG.LE.1)GOTO1162
      IF(IHARG(NUMARG).EQ.'ON')GOTO1162
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1161
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1162
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      GOTO9000
CCCCC OCTOBER 1993.  ADD FOLLOWING SECTION
C
 1150 CONTINUE
      IF(NUMARG.LE.0)GOTO9000
      IF(IHARG(1).NE.'CASE')GOTO9000
      IF(NUMARG.LE.1)GOTO1163
      IF(IHARG(NUMARG).EQ.'ON')GOTO1162
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1161
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1162
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      GOTO9000
C
 1161 CONTINUE
      ITEXCA='UPPE'
      GOTO1180
C
 1162 CONTINUE
      ITEXCA='LOWE'
      GOTO1180
CCCCC OCTOBER 1993.  ADD FOLLOWING SECTION
C
 1163 CONTINUE
      ITEXCA='ASIS'
      GOTO1180
C
 1165 CONTINUE
      ITEXCA=IDEFCA
      GOTO1180
C
 1170 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1171)
 1171 FORMAT('***** ERROR IN DPCASE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1172)
 1172 FORMAT('      ILLEGAL ENTRY FOR CASE ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1173)
 1173 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1174)
 1174 FORMAT('      SUPPOSE THE THE ANALYST WISHES TO HAVE CASE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1175)
 1175 FORMAT('      FOR ALL PLOT TITLES, LABELS, AND LEGENDS,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1176)
 1176 FORMAT('      THEN ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1177)
 1177 FORMAT('           CASE UPPER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1178)
 1178 FORMAT('           UPPER CASE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1179)
 1179 FORMAT('           CASE')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE CASE (FOR PLOT SCRIPT AND TEXT) ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)ITEXCA
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
      WRITE(ICOUT,8111)ITEXCA
 8111 FORMAT('THE CURRENT CASE IS ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)IDEFCA
 8112 FORMAT('THE DEFAULT CASE IS ',A4)
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
 9011 FORMAT('***** AT THE END       OF DPCASE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ITEXCA,IDEFCA
 9013 FORMAT('ITEXCA,IDEFCA = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE ONE OF THE FOLLOWING 12 CONTROL CHARTS--
C              1) MEAN
C              2) RANGE
C              3) STANDARD DEVIATION
C              4) CUSUM
C              5) P
C              6) PN
C              7) C
C              8) U
C              9) EWMA (EXPONENTIALLY WEIGHTED MOVING AVERAGE)
C             10) MOVING AVERAGE 
C             11) MOVING RANGE 
C             12) MOVING STANDARD DEVIATION 
C             13) ISO 13528
C             14) ISO 13528 CUSUM
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE      1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1988. (P, PN, C, AND U CHARTS)
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --JULY      1990. ADD    R CHART    CHECK
C     UPDATED         --JULY      1990. FIX P, NP, C, & U CHARTS
C     UPDATED         --SEPTEMBER 1990. LSL, USL, TARGET
C     UPDATED         --AUGUST    1991. TURN OFF MESS.--LSL/USL/TARGET
C     UPDATED         --MARCH     1997. EWMA, ACTIVATE CUSUM
C     UPDATED         --MARCH     1997. MOVING AVERAGE
C     UPDATED         --MARCH     1997. MOVING RANGE
C     UPDATED         --MARCH     1997. MOVING STANDARD DEVIATION
C     UPDATED         --SEPTEMBER 1998. ACTIVATED CUSUM MEAN CHART
C     UPDATED         --AUGUST    2010. USE DPPARS
C     UPDATED         --JANUARY   2012. SUPPORT HIGHLIGHTED OPTION
C     UPDATED         --JANUARY   2012. "MAXSET" OPTION
C     UPDATED         --FEBRUARY  2012. ISO 13528
C     UPDATED         --FEBRUARY  2012. ISO 13528 CUSUM
C     UPDATED         --FEBRUARY  2018. CONFLICT WITH
C                                       "MEAN CHARACTER PLOT"
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
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
C
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
C
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION YPREV(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y1(1))
      EQUIVALENCE (GARBAG(IGARB3),Y2(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),XHIGH(1))
      EQUIVALENCE (GARBAG(IGARB8),YPREV(1))
C
C-----COMMON----------------------------------------------------------
C
CCCCC ADD FOLLOWING LINE APRIL 1997
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
C
      ISUBN1='DPCC'
      ISUBN2='    '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
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
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
C               *************************************************
C               **  TREAT THE CONTROL CHART CASE:              **
C               **     1) MEAN             CONTROL CHART       **
C               **     2) SD               CONTROL CHART       **
C               **     3) RANGE            CONTROL CHART       **
C               **     4) CUSUM            CONTROL CHART       **
C               **     5) P                CONTROL CHART       **
C               **     6) PN               CONTROL CHART       **
C               **     7) C                CONTROL CHART       **
C               **     8) U                CONTROL CHART       **
C               **     9) EWMA             CONTROL CHART       **
C               **    10) MOVING AVERAGE   CONTROL CHART       **
C               **    11) MOVING RANGE     CONTROL CHART       **
C               **    12) MOVING SD        CONTROL CHART       **
C               **    13) ISO 13528        CONTROL CHART       **
C               **    14) ISO 13528 CUSUM  CONTROL CHART       **
C               *************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CHECK FOR NAME CONFLICTS
C
      IF(ICOM.EQ.'FLUC')GOTO9000
      IF(ICOM.EQ.'TABU')GOTO9000
      IF(ICOM.EQ.'JACK')GOTO9000
      IF(ICOM.EQ.'BOOT')GOTO9000
      IF(ICOM.EQ.'DEX ')GOTO9000
      IF(ICOM.EQ.'DEXP')GOTO9000
      IF(ICOM.EQ.'DOE ')GOTO9000
      IF(ICOM.EQ.'DOX ')GOTO9000
      IF(ICOM.EQ.'CROS' .AND. IHARG(1).EQ.'TABU')GOTO9000
C
      IHIGH='OFF'
      IFOUN1='OFF'
      IFOUN2='OFF'
      IF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')IHIGH='ON'
      ILASTC=-9999
C
      ISTOP=NUMARG-1
      DO90I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT' .OR. IHARG(I).EQ.'CHAR')THEN
          IF(IHARG2(I).EQ.'ACTE')GOTO9000
          ISTOP=I
          GOTO99
        ENDIF
   90 CONTINUE
   99 CONTINUE
C
      ICASP2='NONE'
      DO100I=0,ISTOP
C
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
C
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF((CARG1.EQ.'X   ' .OR. CARG1.EQ.'XBAR' .OR.
     1          CARG1.EQ.'MEAN' .OR. CARG1.EQ.'AVER') .AND.
     1          CARG2.NE.'CUSU' .AND. CARG2.NE.'CUMU' .AND.
     1          CARG0.NE.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='MECC'
        ELSEIF((CARG1.EQ.'SD  ' .OR. CARG1.EQ.'S   ') .AND.
     1          CARG2.NE.'CUSU' .AND. CARG2.NE.'CUMU' .AND.
     1          CARG0.NE.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='SDCC'
        ELSEIF(CARG1.EQ.'STAN' .AND. CARG2.EQ.'DEVI' .AND.
     1         CARG3.NE.'CUSU' .AND. CARG3.NE.'CUMU' .AND.
     1         CARG0.NE.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='SDCC'
        ELSEIF((CARG1.EQ.'RANG' .OR. CARG1.EQ.'R   ') .AND.
     1          CARG2.NE.'CUSU' .AND. CARG2.NE.'CUMU' .AND.
     1          CARG0.NE.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='RACC'
        ELSEIF((CARG1.EQ.'MEAN' .OR. CARG1.EQ.'AVER' .OR.
     1          CARG1.EQ.'X   ') .AND.
     1         (CARG2.EQ.'CUSU' .OR.
     1         (CARG2.EQ.'CUMU' .AND. CARG3.EQ.'SUM ')))THEN
          IFOUN1='YES'
          ICASPL='CUCC'
          ICASP2='MEAN'
        ELSEIF((CARG1.EQ.'SD  ' .OR. CARG1.EQ.'S   ') .AND.
     1         (CARG2.EQ.'CUSU' .OR.
     1         (CARG2.EQ.'CUMU' .AND. CARG3.EQ.'SUM ')))THEN
          IFOUN1='YES'
          ICASPL='CUCC'
          ICASP2='SD  '
        ELSEIF(CARG1.EQ.'STAN' .AND. CARG2.EQ.'DEVI' .AND.
     1         (CARG3.EQ.'CUSU' .OR.
     1         (CARG3.EQ.'CUMU' .AND. CARG4.EQ.'SUM ')))THEN
          IFOUN1='YES'
          ICASPL='CUCC'
          ICASP2='SD  '
        ELSEIF((CARG1.EQ.'RANG' .OR. CARG1.EQ.'R   ') .AND.
     1         (CARG2.EQ.'CUSU' .OR.
     1         (CARG2.EQ.'CUMU' .AND. CARG3.EQ.'SUM ')))THEN
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
        ELSEIF(CARG1.EQ.'EXPO' .AND. CARG2.EQ.'WEIG' .AND.
     1         CARG3.EQ.'MOVI' .AND. CARG4.EQ.'AVER')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'EWMA')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'EXPO' .AND. CARG2.EQ.'MOVI' .AND.
     1         CARG3.EQ.'AVER')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'EXPO' .AND. CARG2.EQ.'WEIG' .AND.
     1         CARG3.EQ.'MOVI')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'EXPO' .AND. CARG2.EQ.'WEIG')THEN
          IFOUN1='YES'
          ICASPL='EWCC'
        ELSEIF(CARG1.EQ.'MOVI' .AND.
     1        (CARG2.EQ.'AVER' .OR. CARG2.EQ.'MEAN') .AND.
     1         CARG0.NE.'EXPO' .AND. CARG0.NE.'WEIG')THEN
          IFOUN1='YES'
          ICASPL='MACC'
        ELSEIF(CARG1.EQ.'MOVI' .AND. CARG2.EQ.'RANG')THEN
          IFOUN1='YES'
          ICASPL='MRCC'
        ELSEIF(CARG1.EQ.'MOVI' .AND.
     1        (CARG2.EQ.'SD  ' .OR. CARG2.EQ.'MSD' .OR.
     1         CARG2.EQ.'S   '))THEN
          IFOUN1='YES'
          ICASPL='MSCC'
        ELSEIF(CARG1.EQ.'MOVI' .AND. CARG2.EQ.'STAN' .AND.
     1         CARG3.EQ.'DEVI')THEN
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
          IF(CARG11.EQ.'ACTE')GOTO9000
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
        ENDIF
C
  100 CONTINUE
C
      IF(IFOUN1.EQ.'NO' .AND. IFOUN2.EQ.'YES')THEN
        ICASPL='MECC'
        IFOUN1='YES'
      ENDIF
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')IFOUND='YES'
      IF(IFOUND.EQ.'NO')GOTO9000
C
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
C
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
      ICOL=1
      IF(IHIGH.EQ.'OFF')THEN
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,Y2,X1,TEMP,TEMP2,TEMP2,TEMP2,NLOCAL,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(NUMVAR.EQ.2)THEN
          DO292II=1,NLOCAL
            X1(II)=Y2(II)
  292     CONTINUE
        ENDIF
      ELSE
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,Y2,X1,XHIGH,TEMP2,TEMP2,TEMP2,NLOCAL,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(NUMVAR.EQ.3)THEN
          DO294II=1,NLOCAL
            XHIGH(II)=X1(II)
            X1(II)=Y2(II)
  294     CONTINUE
        ELSEIF(NUMVAR.EQ.2)THEN
          DO296II=1,NLOCAL
            XHIGH(II)=Y2(II)
  296     CONTINUE
        ENDIF
      ENDIF
C
C               *******************************************************
C               **  STEP 7--                                         **
C               **  FOR THE 1-VARIABLE CASE ONLY,                    **
C               **  DETERMINE IF THE ANALYST                         **
C               **  HAS SPECIFIED    THE GROUP SIZE,                 **
C               **  FOR THE CONTROL CHART ANALYSIS.                  **
C               **  THE GROUP SIZE SETTING IS DEFINED BY SEARCHING   **
C               **  THE INTERNAL TABLE FOR THE PARAMETER NAME  NI ;  **
C               **  IF FOUND, USE THE SPECIFIED VALUE.               **
C               **  IF NOT FOUND, GENERATE AN ERROR MESSAGE.         **
C               *******************************************************
C
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISIZE=1
      IF((IHIGH.EQ.'OFF'.AND.NUMVAR.LE.1) .OR.
     1   (IHIGH.EQ.'ON'.AND.NUMVAR.LE.2))THEN
        IH='NI  '
        IH2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'YES')THEN
          ISIZE=1
        ELSE
          ISIZE=INT(VALUE(ILOCP)+0.5)
        ENDIF
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED SEPTEBMER 1990
C               ********************************************************
C               **  STEP 8--                                          **
C               **  DETERMINE IF THE ANALYST                          **
C               **  HAS SPECIFIED                                     **
C               **      LSL (LOWER SPEC LIMIT)                        **
C               **      USL (UPPER SPEC LIMIT)                        **
C               **      USLCOST (UPPER SPEC LIMIT COST)               **
C               **      TARGET                                        **
C               **      P (FOR EWMA CHARTS)                           **
C               **      K (FOR UNGROUPED DATA, FILTER WIDTH)          **
C               **      WIDTH AS ALTERNATIVE TO K                     **
C               **      WEIGHT AS ALTERNATIVE TO P                    **
C               **  FOR THE CONTROL CHART ANALYSIS.                   **
C               ********************************************************
C
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CCLSL=CPUMIN
      IH='LSL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCLSL=VALUE(ILOCP)
C
      CCUSL=CPUMIN
      IH='USL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCUSL=VALUE(ILOCP)
C
      CCTARG=CPUMIN
      IH='TARG'
      IH2='ET  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCTARG=VALUE(ILOCP)
C
      P=CPUMIN
      IH='P   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')THEN
        P=VALUE(ILOCP)
      ELSE
        IH='WEIG'
        IH2='HT  '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'NO')P=VALUE(ILOCP)
      ENDIF
C
      KWIDTH=3
      IH='K   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')THEN
        KWIDTH=INT(VALUE(ILOCP)+0.5)
      ELSE
        IH='WIDT'
        IH2='H   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'NO')KWIDTH=INT(VALUE(ILOCP)+0.5)
      ENDIF
C
      USRSIG=CPUMIN
      IH='SIGM'
      IH2='AE  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')USRSIG=VALUE(ILOCP)
C
      AK=0.5
      IH='K   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')AK=VALUE(ILOCP)
C
      H=5.0
      IH='H   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')H=VALUE(ILOCP)
C
      H=5.0
      IH='H   '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')H=VALUE(ILOCP)
C
      SHI=CPUMIN
      IH='SHI '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')SHI=VALUE(ILOCP)
C
      SLI=CPUMIN
      IH='SLI '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')SLI=VALUE(ILOCP)
C
      MAXSET=-99
      IH='MAXS'
      IH2='ET  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')MAXSET=INT(VALUE(ILOCP)+0.5)
C
C               *******************************************************
C               **  STEP 9--                                         **
C               **  COMPUTE THE APPROPRIATE CONTROL CHART STATISTIC--**
C               **  MEAN, STANDARD DEVIATION, RANGE, CUSUM,          **
C               **  P, NP, C, U.                                     **
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
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPCC2(Y1,Y2,X1,XHIGH,NLOCAL,NUMVAR,ICASPL,IHIGH,ISIZE,ICONT,
     1           XIDTEM,TEMP,TEMP2,YPREV,
     1           CCLSL,CCUSL,CCTARG,P,KWIDTH,
     1           ICCHPR,ICCHWT,ICONWC,USRSIG,
     1           AK,H,SHI,SLI,MAXSET,
     1           Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
            DO9015I=1,NPLOTP
              WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016         FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
 9015       CONTINUE
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCC2(Y,YN,X,XHIGH,N,NUMV2,ICASPL,IHIGH,ISIZE,ICONT,
     1                 XIDTEM,TEMP,TEMP2,YPREV,
     1                 CCLSL,CCUSL,CCTARG,P,KWIDTH,
     1                 ICCHPR,ICCHWT,ICONWC,USRSIG,
     1                 AK,H,SHI,SLI,MAXSET,
     1                 Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A CONTROL CHART
C              OF THE FOLLOWING TYPES--
C                 1) MEAN CONTROL CHART    Y X
C                 2) STANDARD DEVIATION CONTROL CHART    Y X
C                 3) RANGE CONTROL CHART    Y X
C                 4) CUSUM CONTROL CHART    Y X
C                 5) P CONTROL CHART    NUMDEF NUMTOT X
C                 6) PN CONTROL CHART    NUMDEF NUMTOT X
C                 7) U CONTROL CHART    NUMDEF SIZE X
C                 8) P CONTROL CHART    NUMDEF SIZE X
C                 9) EWMA CONTROL CHART Y X
C                10) MOVING AVERAGE CONTROL CHART Y X
C                11) MOVING RANGE CONTROL CHART Y X
C                12) MOVING STANDARD DEVIATION CONTROL CHART Y X
C                13) ISO 13528 CONTROL CHART Y X
C                14) ISO 13528 CUSUM CONTROL CHART Y X
C     NOTE--USE P AND PN CHARTS IF KNOW HOW MANY ITEMS HAVE DEFECTS
C         --USE U AND C CHARTS IF KNOW HOW MANY DEFECTS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     REFERENCE--ISHIKAWA, GUIDE TO QUALITY CONTROL
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE      1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY  1988. P, PN, U, AND C CHARTS
C     UPDATED         --JULY     1990. FIX P, PN, U, & C CHARTS
C     UPDATED         --SEPTEMBER 1990. LSL, USL, TARGET
C     UPDATED         --MARCH     1997. EWMA CHART, ACTIVATE CUSUM
C     UPDATED         --MARCH     1997. MOVING AVERAGE CHART
C     UPDATED         --MARCH     1997. MOVING RANGE CHART
C     UPDATED         --MARCH     1997. MOVING STANDARD DEVIATION CHART
C     UPDATED         --JANUARY   2012. SUPPORT FOR HIGHLIGHTING OPTION
C     UPDATED         --JANUARY   2012. SUPPORT FOR WECO AND ISO 13528
C                                       CONTROL LIMITS
C     UPDATED         --JANUARY   2012. SUPPORT FOR "MAXSET" OPTION
C     UPDATED         --FEBRUARY  2012. ISO 13528 CONTROL CHART
C     UPDATED         --FEBRUARY  2012. ISO 13528 CUSUM CONTROL CHART
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IHIGH
      CHARACTER*4 ICONT
      CHARACTER*4 ICCHPR
      CHARACTER*4 ICCHWT
      CHARACTER*4 ICONWC
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION YN(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION XHIGH(*)
      DIMENSION YPREV(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION TEMP(*)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      DIMENSION TEMP2(*)
C
      DIMENSION A3(30)
      DIMENSION C4(30)
      DIMENSION B3(30)
      DIMENSION B4(30)
      DIMENSION E2(30)
      DIMENSION D22(30)
      DIMENSION D3(30)
      DIMENSION D4(30)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
CCCCC DATA(A(I),I=    1,   25)
CCCCC1/9.999,2.121,1.732,1.500,1.342,1.225,1.134,1.061,1.000,0.945,
CCCCC1 0.905,0.866,0.832,0.802,0.775,0.750,0.723,0.707,0.688,0.671,
CCCCC1 0.655,0.640,0.626,0.612,0.600/
CCCCC DATA(A0(I),I=    1,   25)
CCCCC1/9.999,3.760,3.070,2.914,2.884,2.899,2.935,2.980,3.030,3.085,
CCCCC1 3.136,3.189,3.242,3.295,3.347,3.398,3.448,3.497,3.545,3.592,
CCCCC1 3.639,3.684,3.729,3.773,3.816/
CCCCC DATA(A1(I),I=    1,   25)
CCCCC1/9.999,3.760,2.394,1.880,1.596,1.410,1.277,1.175,1.094,1.028,
CCCCC1 0.973,0.925,0.884,0.848,0.816,0.788,0.762,0.738,0.717,0.697,
CCCCC1 0.679,0.662,0.647,0.632,0.619/
CCCCC DATA(A2(I),I=    1,   25)
CCCCC1/9.999,1.880,1.023,0.729,0.577,0.483,0.419,0.373,0.337,0.308,
CCCCC1 0.285,0.266,0.249,0.235,0.223,0.212,0.203,0.194,0.187,0.180,
CCCCC1 0.173,0.167,0.162,0.157,0.153/
CCCCC DATA(C2(I),I=    1,   25)
CCCCC1/9.9999,0.5642,0.7236,0.7979,0.8407,
CCCCC1 0.8686,0.8882,0.9027,0.9139,0.9227,
CCCCC1 0.9300,0.9359,0.9410,0.9453,0.9490,
CCCCC1 0.9523,0.9551,0.9576,0.9599,0.9619,
CCCCC1 0.9638,0.9655,0.9670,0.9684,0.9696/
CCCCC DATA(B1(I),I=    1,   25)
CCCCC1/0.000,0.000,0.000,0.000,0.000,0.026,0.105,0.167,0.219,0.262,
CCCCC1 0.299,0.331,0.359,0.384,0.406,0.427,0.445,0.461,0.477,0.491,
CCCCC1 0.504,0.516,0.527,0.538,0.548/
CCCCC DATA(B2(I),I=    1,   25)
CCCCC1/9.999,1.843,1.858,1.808,1.756,1.711,1.672,1.638,1.609,1.584,
CCCCC1 1.561,1.541,1.523,1.507,1.492,1.478,1.465,1.454,1.443,1.433,
CCCCC1 1.424,1.415,1.407,1.399,1.392/
CCCCC DATA(D1(I),I=    1,   25)
CCCCC1/0.000,0.000,0.000,0.000,0.000,0.000,0.205,0.387,0.546,0.687,
CCCCC1 0.812,0.924,1.026,1.121,1.207,1.285,1.359,1.426,1.490,1.548,
CCCCC1 1.606,1.659,1.710,1.759,1.804/
C
      DATA(A3(I),I=    1,   25)
     1/9.999,2.659,1.954,1.628,1.427,
     1 1.287,1.182,1.099,1.032,0.975,
     1 0.927,0.886,0.850,0.817,0.789,
     1 0.763,0.739,0.718,0.698,0.680,
     1 0.663,0.647,0.633,0.619,0.606/
      DATA(C4(I),I=    1,   25)
     1/9.9999,0.7979,0.8862,0.9213,0.9400,
     1 0.9515,0.9594,0.9650,0.9693,0.9727,
     1 0.9754,0.9776,0.9794,0.9810,0.9823,
     1 0.9835,0.9845,0.9854,0.9862,0.9869,
     1 0.9876,0.9882,0.9887,0.9892,0.9896/
      DATA(B3(I),I=    1,   25)
     1/0.000,0.000,0.000,0.000,0.000,0.030,0.118,0.185,0.239,0.284,
     1 0.321,0.354,0.382,0.406,0.428,0.448,0.466,0.482,0.497,0.510,
     1 0.523,0.534,0.545,0.555,0.565/
      DATA(B4(I),I=    1,   25)
     1/9.999,3.267,2.568,2.266,2.089,1.970,1.882,1.815,1.761,1.716,
     1 1.679,1.646,1.618,1.594,1.572,1.552,1.534,1.518,1.503,1.490,
     1 1.477,1.466,1.455,1.445,1.435/
      DATA(E2(I),I=    1,   25)
     1/9.999,1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078,
     1 3.173,3.258,3.336,3.407,3.472,3.532,3.588,3.640,3.689,3.735,
     1 3.778,3.819,3.858,3.895,3.931/
      DATA(D22(I),I=    1,   25)
     1/9.999,3.686,4.358,4.698,4.918,5.078,5.203,5.307,5.394,5.469,
     1 5.534,5.592,5.646,5.693,5.737,5.779,5.817,5.854,5.888,5.922,
     1 5.950,5.979,6.006,6.031,6.058/
      DATA(D3(I),I=    1,   25)
     1/0.000,0.000,0.000,0.000,0.000,0.000,0.076,0.136,0.184,0.223,
     1 0.256,0.284,0.308,0.329,0.348,0.364,0.379,0.392,0.404,0.414,
     1 0.425,0.434,0.443,0.452,0.459/
      DATA(D4(I),I=    1,   25)
     1/9.999,3.267,2.575,2.282,2.115,2.004,1.924,1.864,1.816,1.777,
     1 1.744,1.716,1.692,1.671,1.652,1.636,1.621,1.608,1.596,1.586,
     1 1.575,1.566,1.557,1.548,1.541/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCC'
      ISUBN2='2   '
      IWRITE='OFF'
C
      XTMAX=0.0
      XTMIN=0.0
      D3FACT=0.0D0
      D4FACT=0.0D0
C
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
        DO72I=1,N
          WRITE(ICOUT,73)I,Y(I),YN(I),X(I),XHIGH(I)
   73     FORMAT('I,Y(I),YN(I),X(I),XHIGH(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
      ENDIF
C
      I2=0
      ISIZE2=0
C
      AN=0.0
      XBARG=0.0
      SDG=0.0
      RANGEG=0.0
      YUPPER=0.0
      YLOWER=0.0
C
      ANUMSE=0.0
      SDI=0.0
      SIGMAE=0.0
      RANGEE=0.0
      SADJ=0.0
      RADJ=0.0
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
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
      WRITE(ICOUT,62)HOLD
   62 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS ARE IDENTICALLY ',
     1       'EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
   69 CONTINUE
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
C               **  FOR VARIABLE 2 (THE GROUP VARIABLE).              **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
C               **  WHICH IS AN ERROR CONDITION FOR A CONTROL CHART.  **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF((IHIGH.EQ.'OFF'.AND.NUMV2.EQ.1) .OR.
     1   (IHIGH.EQ.'ON'.AND.NUMV2.EQ.2))THEN
C
C       WHEN THERE IS NO GROUP-ID VARIABLE, CREATE ONE (BASED ON
C       ISIZE).
C
        NUMSET=0
        IF(ISIZE.EQ.1)THEN
          DO120I=1,N
            XIDTEM(I)=REAL(I)
            X(I)=XIDTEM(I)
  120     CONTINUE
        ELSE
          NUMSET=0
          ILOOP=N/ISIZE
          DO145I=1,ILOOP
            NUMSET=NUMSET+1
            XIDTEM(NUMSET)=REAL(NUMSET)
            ISTART=(I-1)*ISIZE+1
            ISTOP=I*ISIZE
            DO147J=ISTART,ISTOP
              X(J)=XIDTEM(NUMSET)
  147       CONTINUE
  145     CONTINUE
          ILEFT=MOD(N,ISIZE)
          IF(ILEFT.NE.0)THEN
            ISTART=ILOOP*ISIZE+1
            NUMSET=NUMSET+1
            XIDTEM(NUMSET)=REAL(NUMSET)
            DO148J=ISTART,N
              X(J)=XIDTEM(NUMSET)
  148       CONTINUE
          ENDIF
        ENDIF
      ENDIF
C
C     WHEN THERE IS A GROUP-ID VARIABLE, EXTRACT UNIQUE VALUES
C
      CALL DISTIN(X,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
C
      IF(NUMSET.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      THE NUMBER OF SETS IS EQUAL TO ZERO.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
      IF((ICASPL.EQ.'MECC' .OR. ICASPL.EQ.'SDCC' .OR.
     1   ICASPL.EQ.'RACC') .AND.NUMSET.EQ.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,196)
  196   FORMAT('      THE NUMBER OF SETS IS IDENTICAL TO THE NUMBER ',
     1         'OF OBSERVATIONS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,198)NUMSET
  198   FORMAT('      THEN  NUMBER OF SETS/OBSERVATIONS  = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      AN=N
      ANUMSE=NUMSET
C
C               *******************************************
C               **  STEP 3.0--                           **
C               **  DETERMINE STATISTICS FOR THE ENTIRE  **
C               **  DATA SET                             **
C               *******************************************
C
C     NOTE 2012/1: IN SOME CASES, WE MAY WANT TO BASE CONTROL
C                  LIMITS ON PORTION OF PLOT THAT IS KNOWN TO
C                  BE IN CONTROL (E.G., HISTORICAL DATA).  IF
C                  USER HAS SPECIFIED "MAXSET", ONLY USE SETS
C                  FROM 1 TO MAXSET IN COMPUTING THESE STATISTICS.
C
C                  FOR NOW, LIMIT THIS OPTION TO THE SHEWHART
C                  CHARTS (MEAN, SD, RANGE).
C
      ISTEPN='3.0'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SUMXBG=0.0
      SUMSDG=0.0
      SUMRAG=0.0
      SUMSIE=0.0
      SUMRIE=0.0
C
      NUMTMP=NUMSET
      IF(MAXSET.GE.1 .AND. MAXSET.LT.NUMSET)THEN
        IF(ICASPL.EQ.'MECC' .OR. ICASPL.EQ.'RACC' .OR.
     1     ICASPL.EQ.'SDCC' .OR. ICASPL.EQ.'MACC' .OR.
     1     ICASPL.EQ.'MSCC' .OR. ICASPL.EQ.'MRCC')THEN
           
          NUMTMP=MAXSET
        ENDIF
      ENDIF
C
      J=0
      ANTMP=0.0
      DO1010ISET=1,NUMTMP
        J=J+1
C
        K=0
        DO1020I=1,N
          IF(X(I).EQ.XIDTEM(ISET))K=K+1
          IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 1020   CONTINUE
        NI=K
        ANI=NI
C
        SUM=0.0
C
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
          GOTO9000
        ENDIF
C
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
        DO1034I=1,NI
          IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
          IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 1034   CONTINUE
        RANGEI=XTMAX-XTMIN
        SUMXBG=SUMXBG+ANI*XBARI
        SUMSDG=SUMSDG+ANI*SDI
        SUMRAG=SUMRAG+ANI*RANGEI
C
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
C
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
C
 1010 CONTINUE
C
      XBARG=SUMXBG/ANTMP
      SDG=SUMSDG/ANTMP
      RANGEG=SUMRAG/ANTMP
CCCCC SIGMAE=SUMSIE/REAL(MAXSET)
CCCCC RANGEE=SUMRIE/REAL(MAXSET)
      SIGMAE=SUMSIE/REAL(NUMTMP)
      RANGEE=SUMRIE/REAL(NUMTMP)
C
C     FOR UNGROUPED DATA, USE THE MOVING RANGE OR THE MOVING STANDARD
C     DEVIATION TO COMPUTE AN ESTIMATE FOR SIGMAE.  MARCH 1997.
C
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
 1071     FORMAT('      THERE ARE NOT ENOUGH DATA POINTS TO FORM THE ',
     1           'MOVING RANGE ESTIMATE')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1072)
 1072     FORMAT('      OF THE ERROR STANDARD DEVIATION FOR UNGROUPED ',
     1           'DATA.  YOU PROBABLY')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1073)
 1073     FORMAT('      NEED TO SET A SMALLER VALUE FOR THE FILTER ',
     1           'WIDTH.  FOR EXAMPLE,')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1074)
 1074     FORMAT('         LET K = 3')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1075)
 1075     FORMAT('      THE PARAMETER K DEFINES HOW MANY VALUES ARE ',
     1           'USED TO COMUTE THE')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1076)
 1076     FORMAT('      MOVING RANGE (3 IS THE TYPICAL VALUE).  THE ',
     1           'CURRENT VALUE')
          CALL DPWRST('XXXX','BUG ')
          WRITE(ICOUT,1077)KWIDTH
 1077     FORMAT('      OF K IS ',I5,'.')
          CALL DPWRST('XXXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        SUM=0.0
        SUM2=0.0
        ICOUNT=0
CCCCC   DO1083I=1+NBEF,MAXSET-NAFT
        DO1083I=1+NBEF,NUMTMP-NAFT
          ICOUNT=ICOUNT+1
          SUM1=0.0
          XTMIN=Y(I-NBEF)
          XTMAX=Y(I+NAFT)
          DO1086II=I-NBEF,I+NAFT
            IF(Y(II).LT.XTMIN)XTMIN=Y(II)
            IF(Y(II).GT.XTMAX)XTMAX=Y(II)
            SUM1=SUM1+Y(II)
 1086     CONTINUE
          SUM=SUM+(XTMAX-XTMIN)
          XMEAN=SUM1/REAL(KWIDTH)
          SUM1=0.0
          DO1087II=I-NBEF,I+NAFT
            SUM1=SUM1+(Y(II)-XMEAN)**2
 1087     CONTINUE
          SUM2=SUM2+SQRT(SUM1/REAL(KWIDTH-1))
 1083   CONTINUE
        RANGEM=SUM/REAL(ICOUNT)
        SDM=SUM2/REAL(ICOUNT)
      ENDIF
C
C           *********************************************************
C           **  STEP 4--                                           **
C           **  IN ORDER TO DETERMINE THE PROPER PLOT COOORDINATES **
C           **  FOR THE DESIRED PLOT,                              **
C           **  BRANCH TO THE PROPER SUBCASE--                     **
C           **         1) MEAN CONTROL CHART                       **
C           **         2) STANDARD DEVIATION CONTROL CHART         **
C           **         3) RANGE CONTROL CHART                      **
C           **         4) CUSUM CONTROL CHART                      **
C           **         5) P CONTROL CHART                          **
C           **         6) PN CONTROL CHART                         **
C           **         7) C CONTROL CHART                          **
C           **         8) U CONTROL CHART                          **
C           **         9) EWMA CONTROL CHART                       **
C           **        10) MOVING AVERAGE  CONTROL CHART            **
C           **        11) MOVING RANGE    CONTROL CHART            **
C           **        12) MOVING SD       CONTROL CHART            **
C           **        13) ISO 13528       CONTROL CHART            **
C           **        14) ISO 13528 CUSUM CONTROL CHART            **
C           *********************************************************
C
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NPREV=0
      IF(ICASPL.EQ.'MECC')THEN
C
C       *****************************************
C       **  STEP 5.1--                         **
C       **  TREAT THE MEAN CONTROL CHART CASE  **
C       *****************************************
C
        ISTEPN='5.1'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        DO1110ISET=1,NUMSET
C
          XTAG=0.0
          K=0
          DO1120I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              IF(XHIGH(I).GE.0.5)XTAG=1.0
            ENDIF
 1120     CONTINUE
          NI=K
          ANI=NI
C
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
            GOTO9000
          ENDIF
C
          SUM=0.0
          DO1140I=1,NI
            SUM=SUM+TEMP(I)
 1140     CONTINUE
          XBARI=SUM/ANI
          YMID=XBARG
C
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
C
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
C
          CALL DPCC3(ICASPL,J,XBARI,YMID,YLOWER,YUPPER,
     1               Y2,X2,D2,XIDTEM(ISET),
     1               YPREV,NPREV,IHIGH,XTAG,SIGMAE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 1110   CONTINUE
      ELSEIF(ICASPL.EQ.'SDCC')THEN
C
C       ********************************************************
C       **  STEP 5.2--                                        **
C       **  TREAT THE  STANDARD DEVIATION CONTROL CHART CASE  **
C       ********************************************************
C
        ISTEPN='5.2'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        DO1210ISET=1,NUMSET
C
          XTAG=0.0
          K=0
          DO1220I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              IF(XHIGH(I).GE.0.5)XTAG=1.0
            ENDIF
 1220     CONTINUE
          NI=K
          ANI=NI
C
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
            GOTO9000
          ENDIF
C
          SUM=0.0
          DO1240I=1,NI
            SUM=SUM+TEMP(I)
 1240     CONTINUE
          XBARI=SUM/ANI
C
          IF(NI.LE.1)GOTO1210
C
          SUM=0.0
          DO1250I=1,NI
            SUM=SUM+(TEMP(I)-XBARI)**2
 1250     CONTINUE
          DENOM=ANI-1.0
          VARI=0.0
          IF(NI.GE.2)VARI=SUM/DENOM
          SDI=0.0
          IF(VARI.GT.0.0)SDI=SQRT(VARI)
C
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
C
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
C
          CALL DPCC3(ICASPL,J,SDI,SADJ,YLOWER,YUPPER,
     1               Y2,X2,D2,XIDTEM(ISET),
     1               YPREV,NPREV,IHIGH,XTAG,SIGMAE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 1210   CONTINUE
      ELSEIF(ICASPL.EQ.'RACC')THEN
C
C       ******************************************
C       **  STEP 5.3--                          **
C       **  TREAT THE RANGE CONTROL CHART CASE  **
C       ******************************************
C
        ISTEPN='5.3'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        D4FACT=1.25
        D3FACT=1.0/1.25
C
        J=0
        DO1310ISET=1,NUMSET
C
          XTAG=0.0
          K=0
          DO1320I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              IF(XHIGH(I).GE.0.5)XTAG=1.0
            ENDIF
 1320     CONTINUE
          NI=K
          ANI=NI
C
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
            GOTO9000
          ENDIF
C
          IF(NI.LE.1)GOTO1310
C
          XTMIN=TEMP(1)
          XTMAX=TEMP(1)
          DO1340I=1,NI
            IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
            IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 1340     CONTINUE
          RANGEI=XTMAX-XTMIN
C
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
C
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
C
          CALL DPCC3(ICASPL,J,RANGEI,YMID,YLOWER,YUPPER,
     1               Y2,X2,D2,XIDTEM(ISET),
     1               YPREV,NPREV,IHIGH,XTAG,RANGEE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 1310   CONTINUE
      ELSEIF(ICASPL.EQ.'CUCC')THEN
C
C       ******************************************************
C       **  STEP 5.4--                                      **
C       **  DETERMINE PLOT COORDINATES                      **
C       **  FOR THE CUSUM CONTROL CHART PLOT SUBCASE.       **
C       ******************************************************
C
        ISTEPN='5.4'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
C
        SUMH=0.0
        SUML=0.0
        IF(SHI.NE.CPUMIN)SUMH=SHI
        IF(SLI.NE.CPUMIN)SUML=SLI
        ZHIGH=3.5
        IF(CCUSL.NE.CPUMIN)ZHIGH=CCUSL
C
        DO1410ISET=1,NUMSET
C
          K=0
          XTAG=0.0
          DO1420I=1,N
            IF(X(I).EQ.XIDTEM(ISET))K=K+1
            IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
            IF(XHIGH(I).GE.0.5)XTAG=1.0
 1420     CONTINUE
          NI=K
          ANI=NI
C
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
            GOTO9000
          ENDIF
C
          IF(NI.EQ.1)THEN
            ZI=(TEMP(1)-XBARG)/RANGEM
          ELSE
            SUM=0.0
            DO1441I=1,NI
              SUM=SUM+TEMP(I)
 1441       CONTINUE
            XBARI=SUM/ANI
            ZI=(XBARI-XBARG)/SIGMAE
          ENDIF
C
          SUMH=MAX(0.0,SUMH+(ZI-AK))
          SUML=MAX(0.0,SUML+(-ZI-AK))
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')THEN
            WRITE(ICOUT,1461)ISET,NI,ANI,XBARI
 1461       FORMAT('ISET,NI,ANI,XBARI = ',2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1463)ZI,SUMH,SUML
 1463       FORMAT('ZI,SUMH,SUML = ',3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          YUPPER=H
          YLOWER=-H
C
          J=J+1
          Y2(J)=SUMH
          X2(J)=XIDTEM(ISET)
          D2(J)=1.0
C
          J=J+1
          Y2(J)=-SUML
          X2(J)=XIDTEM(ISET)
          D2(J)=2.0
C
          J=J+1
          Y2(J)=0.0
          X2(J)=XIDTEM(ISET)
          D2(J)=3.0
C
          J=J+1
          Y2(J)=YUPPER
          X2(J)=XIDTEM(ISET)
          D2(J)=4.0
C
          J=J+1
          Y2(J)=YLOWER
          X2(J)=XIDTEM(ISET)
          D2(J)=5.0
C
          IF(ZI.LE.ZHIGH)GOTO1472
          J=J+1
          Y2(J)=SUMH
          X2(J)=XIDTEM(ISET)
          D2(J)=6.0
          J=J+1
          Y2(J)=SUML
          X2(J)=XIDTEM(ISET)
          D2(J)=7.0
 1472     CONTINUE
C
 1410   CONTINUE
      ELSEIF(ICASPL.EQ.'PCC')THEN
C
C       ********************************************************
C       **  STEP 5.5--                                        **
C       **  TREAT THE  P CONTROL CHART CASE                   **
C       **  PROPORTION DEFECTIVE PER BATCH (SUBSAMPLE)        **
C       **  NUMBER DEFECTIVE PER BATCH / TOTAL NUMBER IN BATCH**
C       **  THE INPUT IS A DUAL SERIES--                      **
C       **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE  **
C       **     2) TOTAL NUMBER OF ITEMS IN THE SAMPLE         **
C       **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING BINOMIAL**
C       ********************************************************
C
        ISTEPN='5.5'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        SUM1=0.0
        SUM2=0.0
        DO1510ISET=1,NUMSET
          SUM1=SUM1+Y(ISET)
          SUM2=SUM2+YN(ISET)
 1510   CONTINUE
        CTOTAL=SUM1
        ANTOT=SUM2
        PBARG=CTOTAL/ANTOT
        PRBARG=100.0*PBARG
C
        J=0
        XTAG=0.0
        DO1550ISET=1,NUMSET
C
          CI=Y(ISET)
          ANI=YN(ISET)
          NI=INT(ANI+0.5)
          IF(NI.LE.0)GOTO1550
C
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
C
          CALL DPCC3(ICASPL,J,PROPI,YMID,YLOWER,YUPPER,
     1               Y2,X2,D2,XIDTEM(ISET),
     1               YPREV,NPREV,IHIGH,XTAG,SIGMAE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 1550   CONTINUE
C
      ELSEIF(ICASPL.EQ.'PNCC')THEN
C
C       *************************************************************
C       **  STEP 5.6--                                             **
C       **  TREAT THE PN CONTROL CHART CASE                        **
C       **  TOTAL NUMBER DEFECTIVE IN A BATCH (SUBSAMPLE)          **
C       **  SUM UP THE NUMBER OF DEFECTIVES PER BATCH (SUBSAMPLE)  **
C       **  THE NUMBER WILL BE  A NON-NEGATIVE INTEGER             **
C       **  THE INPUT IS A DUAL SERIES--                           **
C       **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE       **
C       **     2) TOTAL NUMBER OF ITEMS IN THE SAMPLE              **
C       **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING BINOMIAL     **
C       **  NOTE--THE PN CHART SHOULD BE USED ONLY WHEN            **
C       **        THE SUBSAMPLE SIZE IS CONSTANT.                  **
C       **        FOR VARYING SUBSAMPLE SIZE, USE THE P CHART      **
C       **        (ISHIKAWA, GUIDE TO QUALITY CONTROL, PAGE 77)    **
C       *************************************************************
C
        ISTEPN='5.6'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        XTAG=0.0
        SUM1=0.0
        SUM2=0.0
        ANUMSE=NUMSET
        DO1610ISET=1,NUMSET
          SUM1=SUM1+Y(ISET)
          SUM2=SUM2+YN(ISET)
 1610   CONTINUE
        CTOTAL=SUM1
        ANTOT=SUM2
        PBARG=CTOTAL/ANTOT
        ANBARG=ANTOT/ANUMSE
        CBARG=PBARG*ANBARG
C
        J=0
        DO1650ISET=1,NUMSET
C
          CI=Y(ISET)
          ANI=YN(ISET)
          NI=INT(ANI+0.5)
          IF(NI.LE.0)GOTO1650
C
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
C
          CALL DPCC3(ICASPL,J,CI,YMID,YLOWER,YUPPER,
     1               Y2,X2,D2,XIDTEM(ISET),
     1               YPREV,NPREV,IHIGH,XTAG,SIGMAE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 1650   CONTINUE
      ELSEIF(ICASPL.EQ.'UCC')THEN
C
C       *********************************************************
C       **  STEP 5.7--                                         **
C       **  TREAT THE U CONTROL CHART CASE (POISSON)           **
C       **  DEFECTIVE PER UNIT                                 **
C       **  DEFECTIVE PER UNIT AREA                            **
C       **  NUMBER DEFECTIVE PER SUB-BATCH / LENGTH OR AREA    **
C       **  THE INPUT IS A DUAL SERIES--                       **
C       **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE   **
C       **     2) LENGTH OR AREA OF THE ITEM                   **
C       **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING POISSON  **
C       *********************************************************
C
        ISTEPN='5.7'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        XTAG=0.0
        SUM1=0.0
        SUM2=0.0
        DO1710ISET=1,NUMSET
          SUM1=SUM1+Y(ISET)
          SUM2=SUM2+YN(ISET)
 1710   CONTINUE
        CTOTAL=SUM1
        SIZTOT=SUM2
        CBARG=CTOTAL/SIZTOT
C
        J=0
        DO1750ISET=1,NUMSET
C
          CI=Y(ISET)
          SIZEI=YN(ISET)
          NSIZEI=INT(SIZEI+0.5)
          IF(NSIZEI.LE.0)GOTO1750
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
C
          CALL DPCC3(ICASPL,J,STAT,YMID,YLOWER,YUPPER,
     1               Y2,X2,D2,XIDTEM(ISET),
     1               YPREV,NPREV,IHIGH,XTAG,SIGMAE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 1750   CONTINUE
      ELSEIF(ICASPL.EQ.'CCC')THEN
C
C       ********************************************************
C       **  STEP 5.8--                                        **
C       **  TREAT THE C CONTROL CHART CASE (POISSON)          **
C       **  TOTAL NUMBER DEFECTIVE IN A BATCH (SUBSAMPLE)     **
C       **  SUM OF DEFECTIVES IN A BATCH (SUBSAMPLE)          **
C       **  THE INPUT IS USUALLY A SERIES OF INTEGERS         **
C       **  THE VALUE WILL BE A NON-NEGATIVE INTEGER          **
C       **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING POISSON **
C       **  NOTE--THE C CHART SHOULD BE USED ONLY WHEN        **
C       **        THE SUBSAMPLE SIZE IS CONSTANT.             **
C       **        FOR VARYING SUBSAMPLE SIZE, USE THE U CHART **
C       **        (ISHIKAWA, GUIDE TO QUALITY CONTROL, PAGE 77)*
C       ********************************************************
C
        ISTEPN='5.8'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        XTAG=0.0
        SUM1=0.0
        SUM2=0.0
        ANUMSE=NUMSET
        DO1810ISET=1,NUMSET
          SUM1=SUM1+Y(ISET)
          IF(NUMV2.LE.2)SUM2=SUM2+1
          IF(NUMV2.GE.3)SUM2=SUM2+YN(ISET)
 1810   CONTINUE
        CTOTAL=SUM1
        CBARG=CTOTAL/ANUMSE
C
        J=0
        DO1850ISET=1,NUMSET
C
          CI=Y(ISET)
          SIZEI=YN(ISET)
          NSIZEI=INT(SIZEI+0.5)
          IF(NSIZEI.LE.0)GOTO1850
          YMID=CBARG
          VARCI=0.0
          IF(ANI.GT.0.0)VARCI=CBARG
          SDCI=0.0
          IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
          YUPPER=YMID+3.0*SDCI
          YLOWER=YMID-3.0*SDCI
          IF(YLOWER.LT.0.0)YLOWER=0.0
C
          CALL DPCC3(ICASPL,J,CI,YMID,YLOWER,YUPPER,
     1               Y2,X2,D2,XIDTEM(ISET),
     1               YPREV,NPREV,IHIGH,XTAG,SIGMAE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 1850   CONTINUE
      ELSEIF(ICASPL.EQ.'EWCC')THEN
C
C       *****************************************
C       **  STEP 5.9--                         **
C       **  TREAT THE EXPONETIALLY WEIGHTED    **
C       **  CONTROL CHART CASE                 **
C       *****************************************
C
        ISTEPN='5.9'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(P.GE.1.0 .AND. P.LE.100.)P=P/100.
        IF(P.LE.0.0 .OR. P.GE.1.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1901)
 1901     FORMAT('      FOR THE EWMA CONTROL CHARTS, THE WEIGHTING',
     1           ' PARAMETER P MUST BE SPECIFIED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1902)
 1902     FORMAT('     AND IN THE RANGE (0,1).  IT IS TYPICALLY ',
     1           ' BETWEEN 0.1 AND 0.5 .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1903)
 1903     FORMAT('     FOR EXAMPLE: LET P = 0.2 ')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        J=0
        IF(CCTARG.NE.CPUMIN)THEN
          AK0=CCTARG
        ELSE
          AK0=XBARG
        ENDIF
        YMID=AK0
C
        DO1910ISET=1,NUMSET
C
          K=0
          XTAG=0.0
          DO1920I=1,N
            IF(X(I).EQ.XIDTEM(ISET))K=K+1
            IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
            IF(XHIGH(I).GE.0.5)XTAG=1.0
 1920     CONTINUE
          NI=K
          ANI=NI
C
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
            GOTO9000
          ENDIF
C
          SUM=0.0
          DO1940I=1,NI
            SUM=SUM+TEMP(I)
 1940     CONTINUE
          XBARI=SUM/ANI
C
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
C
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
C
          CALL DPCC3(ICASPL,J,AK1,XBARG,YLOWER,YUPPER,
     1               Y2,X2,D2,XIDTEM(ISET),
     1               YPREV,NPREV,IHIGH,XTAG,SIGMAE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
          AK0=AK1
C
 1910   CONTINUE
      ELSEIF(ICASPL.EQ.'MACC')THEN
C
C       *****************************************
C       **  STEP 5.10--                        **
C       **  TREAT THE MOVING AVERAGE           **
C       **  CONTROL CHART CASE                 **
C       *****************************************
C
        ISTEPN='5.10'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(KWIDTH.LT.2)KWIDTH=2
        IF(KWIDTH.GT.N-1)KWIDTH=N-1
        AK=REAL(KWIDTH)
        NBEF=KWIDTH/2
        NAFT=NBEF
        IF(MOD(KWIDTH,2).EQ.0)NAFT=NBEF-1
C
        J=0
        XTAG=0.0
C
C       2 CASES:
C         1) UNGROUPED DATA (N=NUMSET)
C         2) GROUPED DATA (N> NUMSET).  FOR GROUPED DATA, EACH GROUP
C            SHOULD HAVE AT LEAST 2 VALUES.
C
C       UNGROUPED CASE
C
        IF(N.EQ.NUMSET)THEN
          DO2002ISET=1,N
            TEMP2(ISET)=Y(ISET)
 2002     CONTINUE
        ELSE
C
C         GROUPED CASE
C
          DO2010ISET=1,NUMSET
C
            K=0
            DO2020I=1,N
              IF(X(I).EQ.XIDTEM(ISET))THEN
                K=K+1
                TEMP(K)=Y(I)
              ENDIF
 2020       CONTINUE
            NI=K
            ANI=NI
C
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
              GOTO9000
            ENDIF
C
            IF(NI.EQ.1)THEN
              TEMP2(ISET)=TEMP(1)
            ELSE
              SUM=0.0
              DO2040I=1,NI
                SUM=SUM+TEMP(I)
 2040         CONTINUE
              TEMP2(ISET)=SUM/ANI
            ENDIF
C
 2010     CONTINUE
        ENDIF
C
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
C
        IF(1+NBEF.GT.NUMSET-NAFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2065)
 2065     FORMAT('      THERE ARE NOT ENOUGH GROUPS TO FORM THE ',
     1           'MOVING AVERAGE PLOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2268)KWIDTH,NUMSET
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        DO2090ISET=1,NUMSET
C
          IF(N.EQ.NUMSET)THEN
            XTAG=0.0
            IF(XHIGH(ISET).GE.0.5)XTAG=1.0
          ENDIF
C
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
          DO2092II=ISTRT,ISTOP
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
C
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
C
          CALL DPCC3(ICASPL,J,YVAL,XBARG,YLOWER,YUPPER,
     1               Y2,X2,D2,XVAL,
     1               YPREV,NPREV,IHIGH,XTAG,SIGMAE,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 2090   CONTINUE
      ELSEIF(ICASPL.EQ.'MRCC')THEN
C
C       *****************************************
C       **  STEP 5.11--                        **
C       **  TREAT THE MOVING RANGE             **
C       **  CONTROL CHART CASE                 **
C       *****************************************
C
        ISTEPN='5.11'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(KWIDTH.LT.2)KWIDTH=2
        IF(KWIDTH.GT.N-1)KWIDTH=N-1
        AK=REAL(KWIDTH)
        NBEF=KWIDTH/2
        NAFT=NBEF
        IF(MOD(KWIDTH,2).EQ.0)NAFT=NBEF-1
C
        J=0
        XTAG=0.0
C
C       2 CASES:
C         1) UNGROUPED DATA (N=NUMSET)
C         2) GROUPED DATA (N> NUMSET).  FOR GROUPED DATA, EACH GROUP
C            SHOULD HAVE AT LEAST 2 VALUES.
C
C       UNGROUPED CASE
C
        IF(N.EQ.NUMSET)THEN
          DO2102ISET=1,N
            TEMP2(ISET)=Y(ISET)
 2102     CONTINUE
        ELSE
C
C         GROUPED CASE
C
          DO2110ISET=1,NUMSET
C
            K=0
            DO2120I=1,N
              IF(X(I).EQ.XIDTEM(ISET))THEN
                K=K+1
                TEMP(K)=Y(I)
              ENDIF
 2120       CONTINUE
            NI=K
            ANI=NI
C
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
              GOTO9000
            ENDIF
C
            XTMIN=TEMP(1)
            XTMAX=TEMP(1)
            DO2140I=1,NI
              IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
              IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 2140       CONTINUE
            TEMP2(ISET)=XTMAX-XTMIN
 2110     CONTINUE
        ENDIF
C
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
C
        IF(1+NBEF.GT.NUMSET-NAFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2165)
 2165     FORMAT('      THERE ARE NOT ENOUGH GROUPS TO FORM THE ',
     1           'MOVING RANGE PLOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2268)KWIDTH,NUMSET
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        SUM2=0.0
        NUMRAN=0
        DO2190ISET=1,NUMSET
C
C         GROUPED DATA
C
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
            DO2192II=ISTRT,ISTOP
              SUM=SUM+TEMP2(II)
 2192       CONTINUE
            YVAL=SUM/DENOM
C
C           UNGROUPED DATA
C
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
            DO2182II=ISTRT,ISTOP
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
            YUPPER=(1.0+3.0*D4FACT/SQRT(2.0*(REAL(KWIDTH)-1.0)))*RANGEM
     1             /E2(25)
            YLOWER=(1.0-3.0*D3FACT/SQRT(2.0*(REAL(KWIDTH)-1.0)))*RANGEM
     1             /E2(25)
          ENDIF
          IF(YLOWER.LT.0.0)YLOWER=0.0
C
          CALL DPCC3(ICASPL,J,YVAL,RANGEM,YLOWER,YUPPER,
     1               Y2,X2,D2,XVAL,
     1               YPREV,NPREV,IHIGH,XTAG,RANGEM,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 2190   CONTINUE
      ELSEIF(ICASPL.EQ.'MSCC')THEN
C
C       *****************************************
C       **  STEP 5.12--                        **
C       **  TREAT THE MOVING STANDARD DEVIATION**
C       **  CONTROL CHART CASE                 **
C       *****************************************
C
        ISTEPN='5.12'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(KWIDTH.LT.2)KWIDTH=2
        IF(KWIDTH.GT.N-1)KWIDTH=N-1
        AK=REAL(KWIDTH)
        NBEF=KWIDTH/2
        NAFT=NBEF
        IF(MOD(KWIDTH,2).EQ.0)NAFT=NBEF-1
C
        J=0
        XTAG=0.0
C
C       2 CASES:
C         1) UNGROUPED DATA (N=NUMSET)
C         2) GROUPED DATA (N> NUMSET).  FOR GROUPED DATA, EACH GROUP
C            SHOULD HAVE AT LEAST 2 VALUES.
C
C       UNGROUPED CASE
C
        IF(N.EQ.NUMSET)THEN
          DO2202ISET=1,N
            TEMP2(ISET)=Y(ISET)
 2202     CONTINUE
        ELSE
C
C       GROUPED CASE
C
          DO2210ISET=1,NUMSET
C
            K=0
            XTAG=0.0
            DO2220I=1,N
              IF(X(I).EQ.XIDTEM(ISET))K=K+1
              IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 2220       CONTINUE
            NI=K
            ANI=NI
C
            IF(NI.LT.2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,31)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2232)
 2232         FORMAT('FOR MOVING STANDARD DEVIATION, FOR SOME CLASS ',
     1               'NI < 2')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1133)ISET,XIDTEM(ISET),NI
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
C
            SUM1=0.0
            DO2240I=1,NI
              SUM1=SUM1+TEMP(I)
 2240       CONTINUE
            XMEAN=SUM1/ANI
            SUM1=0.0
            DO2242I=1,NI
              SUM1=SUM1+(TEMP(I)-XMEAN)**2
 2242       CONTINUE
            SD=SQRT(SUM1/(ANI-1.0))
            TEMP2(ISET)=SD
 2210     CONTINUE
        ENDIF
C
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
C
        IF(1+NBEF.GT.NUMSET-NAFT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2265)
 2265     FORMAT('      THERE ARE NOT ENOUGH GROUPS TO FORM THE ',
     1           'MOVING STANDARD DEVAITION PLOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2268)KWIDTH,NUMSET
 2268     FORMAT('      THE FILTER WIDTH IS ',I5,' AND THE NUMBER OF ',
     1           'GROUPS IS ',I5,'.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        SUM2=0.0
        NUMSD=0
        DO2290ISET=1,NUMSET
C
C         GROUPED DATA
C
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
            DO2292II=ISTRT,ISTOP
              SUM=SUM+TEMP2(II)
 2292       CONTINUE
            YVAL=SUM/DENOM
C
C         UNGROUPED DATA
C
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
            DO2282II=ISTRT,ISTOP
              ICOUNT=ICOUNT+1
              SUM1=SUM1+TEMP2(II)
 2282       CONTINUE
            XMEAN=SUM1/REAL(ICOUNT)
            SUM1=0.0
            DO2283II=ISTRT,ISTOP
              SUM1=SUM1+(TEMP2(II)-XMEAN)**2
 2283       CONTINUE
            IF(ICOUNT.LT.2)GOTO2290
            YVAL=SQRT(SUM1/REAL(ICOUNT-1))
            XTAG=0.0
            IF(XHIGH(ISET).GE.0.5)XTAG=1.0
          ENDIF
C
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
C
          CALL DPCC3(ICASPL,J,YVAL,SDM,YLOWER,YUPPER,
     1               Y2,X2,D2,XVAL,
     1               YPREV,NPREV,IHIGH,XTAG,SDM,
     1               CCLSL,CCUSL,CCTARG,ICONWC,
     1               IBUGG3,ISUBRO,IERROR)
C
 2290   CONTINUE
C
      ELSEIF(ICASPL.EQ.'1352')THEN
C
C       **********************************************
C       **  STEP 5.13--                             **
C       **  TREAT THE ISO 13528 CONTROL CHART CASE  **
C       **********************************************
C
C THE ISO 13528 CONTROL CHART IS BASED ON THE FOLLOWING:
C
C    1) USE A Z-SCORE AS THE RESPONSE.  SINCE THE STANDARD
C       PROVIDES FOR VARIOUS WAYS TO COMPUTE THE Z-SCORE,
C       ASSUME THAT THE RESPONSE IS ALREADY IN Z-SCORE FORMAT.
C
C    2) IF THERE IS REPLICATION, COMPUTE A MEAN FOR EACH
C       GROUP.  IF THERE IS NO REPLICATION, THEN JUST USE
C       THE DATA VALUE.  UNLIKE THE STANDARD MEAN CONTROL
C       CHART, WE DO NOT AVERAGE OVER SEVERAL VALUES FOR
C       INDIVIDUAL OBSERVATIONS.
C
C    3) CONTROL LIMITS ARE AT +/-2 AND +/-3.
C
C    4) ONE VERSION OF THIS PLOT ALSO PLOTS THE RAW DATA
C       VALUES.
C
C    5) THE MATERIAL-ID CAN BE TREATED AS A "HIGHLIGHTING"
C       VARIABLE.  THEREFORE, LET THE HIGHLIGHT VARIABLE
C       SPECIFY THE MATERIAL ID RATHER THAN JUST 0/1.
C
        ISTEPN='5.13'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        ICNT=0
        DO2310ISET=1,NUMSET
C
          K=0
          DO2320I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              TEMP2(K)=XHIGH(I)
            ENDIF
 2320     CONTINUE
          NI=K
          ANI=NI
C
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
            GOTO9000
          ENDIF
C
          SUM=0.0
          DO2340I=1,NI
            SUM=SUM+TEMP(I)
 2340     CONTINUE
          STAT=SUM/ANI
C
          ICNT=1
          J=J+1
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
C
          ICNT=ICNT+1
          J=J+1
          Y2(J)=0.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
C
          ICNT=ICNT+1
          J=J+1
          Y2(J)=2.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
C
          ICNT=ICNT+1
          J=J+1
          Y2(J)=-2.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
C
          ICNT=ICNT+1
          J=J+1
          Y2(J)=3.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
C
          ICNT=ICNT+1
          J=J+1
          Y2(J)=-3.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
C
          ICNT=ICNT+1
          DO2350II=1,NI
            J=J+1
            Y2(J)=TEMP(II)
            X2(J)=XIDTEM(ISET)
            IF(IHIGH.EQ.'ON')THEN
              D2(J)=REAL(ICNT) + TEMP2(II) - 1.0
            ELSE
              D2(J)=REAL(ICNT)
            ENDIF
 2350     CONTINUE
C
 2310   CONTINUE
      ELSEIF(ICASPL.EQ.'1CUS')THEN
C
C       ****************************************************
C       **  STEP 5.14--                                   **
C       **  TREAT THE ISO 13528 CUSUM CONTROL CHART CASE  **
C       ****************************************************
C
C THE ISO 13528 CUSUM CONTROL CHART IS BASED ON THE FOLLOWING:
C
C    1) USE A Z-SCORE AS THE RESPONSE.  SINCE THE STANDARD
C       PROVIDES FOR VARIOUS WAYS TO COMPUTE THE Z-SCORE,
C       ASSUME THAT THE RESPONSE IS ALREADY IN Z-SCORE FORMAT.
C
C    2) IF THERE IS REPLICATION, COMPUTE A MEAN FOR EACH
C       GROUP.  IF THERE IS NO REPLICATION, THEN JUST USE
C       THE DATA VALUE.
C
C    3) SIMPLY PLOT THE CUMULATIVE SUM OF THE Z-SCORES.
C       THE TARGET VALUE IS ZERO.
C
C    4) THERE ARE NO CONTROL LIMITS FOR THIS PLOT.
C
C    5) THE MATERIAL-ID CAN BE TREATED AS A "HIGHLIGHTING"
C       VARIABLE.  THEREFORE, LET THE HIGHLIGHT VARIABLE
C       SPECIFY THE MATERIAL ID RATHER THAN JUST 0/1.
C
        ISTEPN='5.13'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PCC2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        ICNT=0
        CUSUM=0.0
        DO2410ISET=1,NUMSET
C
          K=0
          DO2420I=1,N
            IF(X(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP(K)=Y(I)
              TEMP2(K)=XHIGH(I)
            ENDIF
 2420     CONTINUE
          NI=K
          ANI=NI
C
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
            GOTO9000
          ENDIF
C
          SUM=0.0
          DO2440I=1,NI
            SUM=SUM+TEMP(I)
 2440     CONTINUE
          CUSUM=CUSUM + (SUM/ANI)
C
          ICNT=1
          J=J+1
          Y2(J)=CUSUM
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
C
          ICNT=ICNT+1
          J=J+1
          Y2(J)=0.0
          X2(J)=XIDTEM(ISET)
          D2(J)=REAL(ICNT)
C
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
 1054   FORMAT('      MECC, SDCC, RACC, CSCC, PCC, PNCC, UCC, CCC, ',
     1         'EWMA, MACC, MSCC, OR MRCC.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1056)ICASPL
 1056   FORMAT('      ICASPL = ',A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      N2=J
      NPLOTV=3
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
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
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCC3(ICASPL,J,STAT,YMID,YLOWER,YUPPER,
     1                 Y2,X2,D2,XVAL,
     1                 YPREV,NPREV,IHIGH,XHIGH,SIGMA,
     1                 CCLSL,CCUSL,CCTARG,ICONWC,
     1                 IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE-UTIITY ROUTINE USED BY DPCC.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/1
C     ORIGINAL VERSION--JANUARY   2012. EXTRACTED FROM DPCC2
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONWC
      CHARACTER*4 IHIGH
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION YPREV(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
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
C
      IERROR='NO'
C
      ICNT=1
      J=J+1
      Y2(J)=STAT
      X2(J)=XVAL
      D2(J)=REAL(ICNT)
C
C     IF "ISO 13528" CONTROL LIMITS REQUESTED, SPECIFY LIMITS
C     AT +/-2 AND +/-3.  THESE ONLY APPLY TO "MEAN CONTROL"
C     CHART.
C
      IF(ICONWC.EQ.'ISO' .AND.
     1  (ICASPL.EQ.'MECC' .OR. ICASPL.EQ.'MACC'))THEN
C
        ICNT=ICNT+1
        J=J+1
        Y2(J)=0.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
C
        ICNT=ICNT+1
        J=J+1
        Y2(J)=2.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
C
        ICNT=ICNT+1
        J=J+1
        Y2(J)=-2.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
C
        ICNT=ICNT+1
        J=J+1
        Y2(J)=3.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
C
        ICNT=ICNT+1
        J=J+1
        Y2(J)=-3.0
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
C
      ELSE
        ICNT=ICNT+1
        J=J+1
        Y2(J)=YMID
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
C
        ICNT=ICNT+1
        J=J+1
        Y2(J)=YUPPER
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
C
        ICNT=ICNT+1
        J=J+1
        Y2(J)=YLOWER
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
C
C       IMPLEMENT WECO (WESTERN ELECTRIC) RULES FOR MEAN, SD,
C       AND RANGE CONTROL CHARTS.  THESE ARE TYPICALLY USED IN
C       ADDITION TO THE STANDARD CONTROL LIMITS.  ONE DRAWBACK TO
C       THESE RULES IS THAT THEY CAN LEAD TO AN EXCESSIVE NUMBER
C       OF FALSE POSITIVES.
C
C       THESE RULES FLAG THE FOLLOWING (THESE ARE LISTED FOR
C       POINTS ABOVE THE CENTER LINE (I.E., YMID).  THERE ARE
C       SIMILAR RULES FOR POINTS BELOW THE CENTER LINE.
C
C          1) ANY POINT > 3*SIGMA
C          2) 2 OUT OF LAST 3 POINTS > 2*SIGMA
C          3) 4 OUT OF LAST 5 POINTS > 1*SIGMA
C          4) 8 CONSECUTIVE POINTS ABOVE CENTER LINE
C
C       FOR RULE 1, WE DO NOT NEED ANY PAST DATA.  FOR THE OTHERS,
C       PASS IN AN ARRAY THAT CONTAINS THE PREVIOUS DATA.
C
        IF(ICONWC.EQ.'WECO' .AND.
     1    (ICASPL.EQ.'MECC' .OR. ICASPL.EQ.'MACC' .OR.
     1     ICASPL.EQ.'RACC' .OR. ICASPL.EQ.'MRCC' .OR.
     1     ICASPL.EQ.'SDCC' .OR. ICASPL.EQ.'MSCC'))THEN
C
          ITAG=0
          NPREV=NPREV+1
          YPREV(NPREV)=STAT
C
          IF(STAT.GT.YMID + 3.0*SIGMA)THEN
            ITAG=1
          ELSEIF(STAT.LT.YMID - 3.0*SIGMA)THEN
            ITAG=1
          ENDIF
C
          IF(NPREV.GE.3)THEN
            ISTRT=NPREV-2
            ICNT1=0
            ICNT2=0
            DO1020I=ISTRT,NPREV
              IF(YPREV(I).GT.YMID + 2.0*SIGMA)ICNT1=ICNT1+1
              IF(YPREV(I).LT.YMID - 2.0*SIGMA)ICNT2=ICNT2+1
 1020       CONTINUE
            IF(ICNT1.GE.2 .OR. ICNT2.GE.2)ITAG=1
          ENDIF
C
          IF(NPREV.GE.5)THEN
            ISTRT=NPREV-4
            ICNT1=0
            ICNT2=0
            DO1030I=ISTRT,NPREV
              IF(YPREV(I).GT.YMID + SIGMA)ICNT1=ICNT1+1
              IF(YPREV(I).LT.YMID - SIGMA)ICNT2=ICNT2+1
 1030       CONTINUE
            IF(ICNT1.GE.2 .OR. ICNT2.GE.2)ITAG=1
          ENDIF
C
          IF(NPREV.GE.8)THEN
            ISTRT=NPREV-7
            IFLAG=1
            IF(STAT.GT.YMID)THEN
              DO1040I=ISTRT,NPREV-1
                IF(YPREV(I).LT.YMID)IFLAG=0
 1040         CONTINUE
            ELSEIF(STAT.LT.YMID)THEN
              DO1045I=ISTRT,NPREV-1
                IF(YPREV(I).GT.YMID)IFLAG=0
 1045         CONTINUE
            ENDIF
            IF(IFLAG.EQ.1)ITAG=1
          ENDIF
C
          IF(ITAG.EQ.1)THEN
            ICNT=ICNT+1
            J=J+1
            Y2(J)=STAT
            X2(J)=XVAL
            D2(J)=REAL(ICNT)
          ENDIF
        ENDIF
C
      ENDIF
C
      IF(CCTARG.NE.CPUMIN)THEN
        ICNT=ICNT+1
        J=J+1
        Y2(J)=CCTARG
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
      ENDIF
C
      IF(CCUSL.NE.CPUMIN)THEN
        ICNT=ICNT+1
        J=J+1
        Y2(J)=CCUSL
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
      ENDIF
C
      IF(CCLSL.NE.CPUMIN)THEN
        ICNT=ICNT+1
        J=J+1
        Y2(J)=CCLSL
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
      ENDIF
C
      IF(IHIGH.EQ.'ON' .AND. XHIGH.GE.0.5)THEN
        ICNT=ICNT+1
        J=J+1
        Y2(J)=STAT
        X2(J)=XVAL
        D2(J)=REAL(ICNT)
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'PCC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCC3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCD(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                IANGLU,DEMOFR,DEMODF,
     1                IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE ONE OF THE FOLLOWING 2
C              COMPLEX DEMODULATION PLOTS--
C                   1) AMPLITUDE;
C                   2) PHASE;
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE      1978.
C     UPDATED         --JULY      1981.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
CCCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
CCCCC END CHANGE
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA PI/3.141592653/
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      ISUBN1='DPCD'
      ISUBN2='    '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***********************************************
C               **  TREAT THE COMPLEX DEMODULATION CASE      **
C               ***********************************************
C
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
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.3 .AND. ICOM.EQ.'COMP' .AND.
     1   IHARG(1).EQ.'DEMO' .AND. IHARG(2).EQ.'AMPL' .AND.
     1   IHARG(3).EQ.'PLOT')THEN
        ICASPL='CDAM'
        ILASTC=3
      ELSEIF(NUMARG.GE.3 .AND. ICOM.EQ.'COMP' .AND.
     1       IHARG(1).EQ.'DEMO' .AND. IHARG(2).EQ.'PHAS' .AND.
     1       IHARG(3).EQ.'PLOT')THEN
        ICASPL='CDPH'
        ILASTC=3
      ELSE
        IFOUND='NO'
        GOTO9000
      ENDIF
C
      IFOUND='YES'
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
C     EXTRACT THE VARIABLE.
C
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y1,Y1,NLEFT,NLOCAL,NLOCAL,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 7--                                        **
C               **  DETERMINE IF THE ANALYST                        **
C               **  HAS SPECIFIED    THE DEMODULATION FREQUENCY     **
C               **  FOR THE COMPLEX DEMODULATION ANALYSIS.          **
C               **  THE FREQUENCY SETTING IS DEFINED BY PRE-USE     **
C               **  OF THE DEMODULATION FREQUENCY     COMMAND.      **
C               **  IF FOUND, USE THE SPECIFIED VALUE.              **
C               **  IF NOT FOUND, GENERATE AN ERROR MESSAGE.        **
C               ******************************************************
C
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DEMOF2=DEMOFR
      IF(IANGLU.EQ.'DEGR')DEMOF2=DEMOF2*PI/180.0
      IF(IANGLU.EQ.'GRAD')DEMOF2=DEMOF2*PI/200.0
CCCCC IF(0.0.LT.DEMOF2.AND.DEMOF2.LT.0.5)GOTO790
C
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
  750     FORMAT('       THE DEMODULATION FREQUENCY = ',G15.7,2X,
     1           'RADIANS')
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
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 8--                                           *
C               **  COMPUTE THE APPROPRIATE COMPLEX DEMODULATION       *
C               **  PLOT  (AMPLITUDE OR PHASE).                        *
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS              *
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.                 *
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).      *
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).      *
C               ********************************************************
C
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DPCD')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPCD2(Y1,NLEFT,ICASPL,DEMOF2,DEMODF,
     1           Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DEMOFR,IANGLU,DEMOF2
 9014   FORMAT('DEMOFR,IANGLU,DEMOF2 = ',G15.7,2X,A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
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
      SUBROUTINE DPCD2(Y,N,ICASPL,F,DEMODF,
     1                 Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE PERFORMS A COMPLEX DEMODULATION
C              ON THE DATA IN THE INPUT VECTOR X
C              AT THE INPUT DEMODULATION FREQUENCY = F.
C              THE COMPLEX DEMODULATION CONSISTS OF THE FOLLOWING--
C              1) AN AMPLITUDE VERSUS TIME PLOT;
C              2) A PHASE VERSUS TIME PLOT;
C              3) AN UPDATED DEMODULATION FREQUENCY ESTIMATE
C                 TO ASSIST THE ANALYST IN DETERMINING A
C                 MORE APPROPRIATE FREQUENCY AT WHICH
C                 TO DEMODULATE IN CASE THE SPECIFIED
C                 INPUT DEMODULATION FREQUENCY F
C                 DOES NOT FLATTEN SUFFICIENTLY THE
C                 PHASE PLOT.
C
C              THE ALLOWABLE RANGE OF THE INPUT DEMODULATION
C              FREQUENCY F IS 0.0 TO 0.5 (EXCLUSIVELY).
C              THE INPUT DEMODULATION FREQUENCY F IS MEASURED  OF
C              IN UNITS OF CYCLES PER 'DATA POINT' OR,
C              MORE PRECISELY, IN CYCLES PER UNIT TIME WHERE
C              'UNIT TIME' IS DEFINED AS THE
C              ELAPSED TIME BETWEEN ADJACENT OBSERVATIONS.
C
C     INPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED) OBSERVATIONS.
C                      N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X.
C                      FREQ   = THE SINGLE PRECISION
C                               DEMODULATION FREQUENCY.
C                               F IS IN UNITS OF CYCLES PER DATA POINT.
C                               F IS BETWEEN 0.0 AND 0.5 (EXCLUSIVELY).
C     OUTPUT--2 PAGES OF AUTOMATIC PRINTOUT--
C             1) AN AMPLITUDE PLOT;
C             2) A PHASE PLOT; AND
C             3) AN UPDATED DEMODULATION FREQUENCY ESTIMATE.
C     PRINTING--YES.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 5000.
C                 --THE SAMPLE SIZE N MUST BE GREATER
C                   THAN OR EQUAL TO 3.
C                 --THE INPUT FREQUENCY F MUST BE
C                   GREATER THAN OR EQUAL TO 2/(N-2).
C                 --THE INPUT FREQUENCY F MUST BE
C                   SMALLER THAN 0.5.
C     OTHER DATAPAC   SUBROUTINES NEEDED--PLOTX.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, SIN, COS, ATAN.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--IN ORDER THAT THE RESULTS OF THE COMPLEX DEMODULATION
C              BE VALID AND PROPERLY INTERPRETED, THE INPUT DATA
C              IN X SHOULD BE EQUI-SPACED IN TIME
C              (OR WHATEVER VARIABLE CORRESPONDS TO TIME).
C            --IF THE INPUT OBSERVATIONS IN X ARE CONSIDERED
C              TO HAVE BEEN COLLECTED 1 SECOND APART IN TIME,
C              THEN THE DEMODULATION FREQUENCY F
C              WOULD BE IN UNITS OF HERTZ
C              (= CYCLES PER SECOND).
C            --A FREQUENCY OF 0.0 CORRESPONDS TO A CYCLE
C              IN THE DATA OF INFINITE (= 1/(0.0))
C              LENGTH OR PERIOD.
C              A FREQUENCY OF 0.5 CORRESPONDS TO A CYCLE
C              IN THE DATA OF LENGTH = 1/(0.5) = 2 DATA POINTS.
C            --IN EXAMINING THE AMPLITUDE AND PHASE PLOTS,
C              ATTENTION SHOULD BE PAID NOT ONLY TO THE
C              STRUCTURE OF THE PHASE PLOT
C              (NEAR-ZERO SLOPE VERSUS NON-ZERO SLOPE)
C              BUT ALSO TO THE RANGE
C              OF VALUES ON THE VERTICAL AXIS.
C              A PLOT WITH MUCH STRUCTURE BUT
C              WITH A SMALL RANGE ON THE VERTICAL AXIS
C              IS USUALLY MORE INDICATIVE OF A
C              DEFINITE CYCLIC COMPONENT AT THE
C              SPECIFIED INPUT DEMODULATION FREQUENCY,
C              THAN IS A PLOT WITH LESS STRUCTURE BUT
C              A WIDER RANGE ON THE VERTICAL AXIS.
C            --INTERNAL TO THIS SUBROUTINE, 2 MOVING
C              AVERAGES ARE APPLIED, EACH OF LENGTH 1/F.
C              HENCE THE AMPLITUDE AND PHASE PLOTS
C              HAVE N - 2/F VALUES
C              (RATHER THAN N VALUES) ALONG THE
C              HORIZONTAL (TIME) AXIS.
C              IN ORDER THAT THE AMPLITUDE AND PHASE
C              PLOTS BE NON-EMPTY, AN INPUT
C              REQUIREMENT ON F FOR THIS SUBROUTINE
C              IS THAT THE SAMPLE SIZE N
C              AND THE DEMODULATION FREQUENCY F
C              MUST BE SUCH THAT
C              N - 2/F BE GREATER THAN ZERO.
C              FURTHER, SINCE A PLOT WITH BUT
C              1 POINT IS MEANINGLESS
C              AND OUGHT ALSO BE EXCLUDED,
C              THE REQUIREMENT IS EXTENDED
C              SO THAT N - 2/F MUST BE GREATER THAN 1.
C     REFERENCES--GRANGER AND HATANAKA, PAGES 170 TO 189,
C                 ESPECIALLY PAGES 173, 177, AND 182.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1972.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --FEBRUARY  1976.
C     UPDATED         --JUNE      1978.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --MAY       1982.
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y(*)
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA PI/3.141592653/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCD'
      ISUBN2='2   '
C
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
C
      ILOWER=3
      IUPPER=MAXOBV
      AN=N
      FMIN=2.0/(AN-2.0)
C
C               ********************************************
C               **  STEP 0--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.ILOWER.OR.N.GT.IUPPER)GOTO50
      IF(F.LE.FMIN.OR.F.GE.0.5)GOTO60
      HOLD=Y(1)
      DO65I=2,N
      IF(Y(I).NE.HOLD)GOTO95
   65 CONTINUE
      WRITE(ICOUT, 9)HOLD
      CALL DPWRST('XXX','BUG ')
      GOTO9000
   50 WRITE(ICOUT,17)ILOWER,IUPPER
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
      CALL DPWRST('XXX','BUG ')
      GOTO9000
   60 WRITE(ICOUT,27)FMIN
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)F
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,28)FMIN,N
      CALL DPWRST('XXX','BUG ')
      GOTO9000
   95 CONTINUE
    9 FORMAT('***** WARNING--THE FIRST ARGUMENT ',
     1'(A VECTOR) TO THE DPCD2  SUBROUTINE HAS ALL ELEMENTS = ',
     1G15.7)
   17 FORMAT('***** ERROR--THE SECOND ARGUMENT TO THE ',
     1'DPCD2  SUBROUTINE IS OUTSIDE THE ALLOWABLE (',I6,',',I6,') ',
     1'INTERVAL')
   27 FORMAT('***** ERROR--THE THIRD ARGUMENT TO THE ',
     1'DPCD2  SUBROUTINE IS OUTSIDE THE ALLOWABLE (',I6,'0.5) ',
     1'INTERVAL')
   28 FORMAT('                   THE ABOVE LOWER LIMIT (',F11.8,
     1') = 2/(N-2) WHERE N = THE INPUT SAMPLE SIZE = ',I8)
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C               ******************************
C               **  STEP 1--                **
C               **  FORM THE COSINE SERIES  **
C               ******************************
C
      DO100I=1,N
        AI=I
        Y2(I)=Y(I)*COS(2.0*PI*F*AI)
  100 CONTINUE
C
C     DEFINE THE LENGTH OF THE 2 MOVING AVERAGES
C
      LENMA1=INT(1.0/F)
      LENMA2=INT(1.0/F)
      ALEN1=REAL(LENMA1)
      ALEN2=REAL(LENMA2)
      IMAX1=N-LENMA1
      IMAX2=IMAX1-LENMA2
C
C               *********************************************************
C               **  STEP 2--                                           **
C               **  FORM THE FIRST MOVING AVERAGE FOR THE COSINE SERIES**
C               *********************************************************
C
      DO200I=1,IMAX1
        ISTART=I+1
        IEND=I+LENMA1-1
        IENDP1=I+LENMA1
        SUM=0.0
        DO300J=ISTART,IEND
          SUM=SUM+Y2(J)
  300   CONTINUE
        SUM=SUM+Y2(I)/2.0+Y2(IENDP1)/2.0
        D2(I)=SUM/ALEN1
  200 CONTINUE
C
C               ************************************************************
C               **  STEP 3--                                              **
C               **  FORM THE SECOND MOVING AVERAGE FOR THE COSINE SERIES  **
C               ************************************************************
C
      DO400I=1,IMAX2
        ISTART=I+1
        IEND=I+LENMA2-1
        IENDP1=I+LENMA2
        SUM=0.0
        DO500J=ISTART,IEND
          SUM=SUM+D2(J)
  500   CONTINUE
        SUM=SUM+D2(I)/2.0+D2(IENDP1)/2.0
        Y2(I)=SUM/ALEN2
  400 CONTINUE
C
C               ****************************
C               **  STEP 4--              **
C               **  FORM THE SINE SERIES  **
C               ****************************
C
      DO700I=1,N
        AI=I
        X2(I)=Y(I)*SIN(2.0*PI*F*AI)
  700 CONTINUE
C
C               *********************************************************
C               **  STEP 5--                                           **
C               **  FORM THE FIRST MOVING AVERAGE FOR THE SINE SERIES  **
C               *********************************************************
C
      DO800I=1,IMAX1
        ISTART=I+1
        IEND=I+LENMA1-1
        IENDP1=I+LENMA1
        SUM=0.0
        DO900J=ISTART,IEND
          SUM=SUM+X2(J)
  900   CONTINUE
        SUM=SUM+X2(I)/2.0+X2(IENDP1)/2.0
        D2(I)=SUM/ALEN1
  800 CONTINUE
C
C               **********************************************************
C               **  STEP 6--                                            **
C               **  FORM THE SECOND MOVING AVERAGE FOR THE SINE SERIES  **
C               **********************************************************
C
      DO1000I=1,IMAX2
        ISTART=I+1
        IEND=I+LENMA1-1
        IENDP1=I+LENMA1
        SUM=0.0
        DO1100J=ISTART,IEND
          SUM=SUM+D2(J)
 1100   CONTINUE
        SUM=SUM+D2(I)/2.0+D2(IENDP1)/2.0
        X2(I)=SUM/ALEN2
 1000 CONTINUE
C
C     CHECK FOR DESIRED CASE
C     AND BRANCH ACCORDINGLY.
C
      IF(ICASPL.EQ.'CDAM')GOTO1400
      IF(ICASPL.EQ.'CDPH')GOTO1700
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1311)
 1311 FORMAT('***** INTERNAL ERROR IN DPCD2 ',
     1'AT BRANCH POINT 1311--')
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
      GOTO9000
C
C               *****************************************
C               **  STEP 7--                           **
C               **  FORM THE AMPLITUDES AND PLOT THEM  **
C               *****************************************
C
 1400 CONTINUE
      DO1450I=1,IMAX2
      Y2(I)=2.0*SQRT(Y2(I)*Y2(I)+X2(I)*X2(I))
      X2(I)=I
      D2(I)=1.0
 1450 CONTINUE
      N2=IMAX2
      NPLOTV=2
CCCCC WRITE(ICOUT,1451)F
C1451 FORMAT(30X, 48HAMPLITUDE PLOT FOR THE DEMODULATION FREQUENCY =
CCCCC1 ,F8.6,21H CYCLES PER UNIT TIME)
CCCCC CALL DPWRST('XXX','BUG ')
C
C     COMPUTE THE DIFFERENCE BETWEEN THE MAX AND MIN AMPLITUDES AND WRITE IT OUT
C
      Y2MIN=Y2(1)
      Y2MAX=Y2(1)
      DO1600I=1,IMAX2
      IF(Y2(I).LT.Y2MIN)Y2MIN=Y2(I)
      IF(Y2(I).GT.Y2MAX)Y2MAX=Y2(I)
 1600 CONTINUE
      RANGE=Y2MAX-Y2MIN
CCCCC WRITE(ICOUT,1651)Y2MIN,Y2MAX,RANGE
C1651 FORMAT(9X,20HMINIMUM AMPLITUDE = ,E15.8,5X,20HMAXIMUM AMPLITUD
CCCCC1E = ,E15.8,5X,22HRANGE OF AMPLITUDES = ,E15.8)
CCCCC CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C               *************************************
C               **  STEP 8--                       **
C               **  FORM THE PHASES AND PLOT THEM  **
C               *************************************
C
 1700 CONTINUE
      DO1750I=1,IMAX2
        Y2(I)=ATAN(Y2(I)/X2(I))
        X2(I)=I
        D2(I)=1.0
 1750 CONTINUE
      N2=IMAX2
      NPLOTV=2
C
CCCCC WRITE(ICOUT,1751)F
C1751 FORMAT(32X, 44HPHASE PLOT FOR THE DEMODULATION FREQUENCY = ,F8
CCCCC1.6,21H CYCLES PER UNIT TIME)
CCCCC CALL DPWRST('XXX','BUG ')
C
C     COMPUTE A NEW ESTIMATE FOR THE DEMODULATION FREQUENCY AND WRITE IT OUT
C
      AIMAX2=IMAX2
      IMAX2M=IMAX2-1
      IFLAG=0
      Y2MIN=Y2(1)
      Y2MAX=Y2(1)
      DO1800I=1,IMAX2M
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
CCCCC WRITE(ICOUT,2025)Y2MIN,Y2MAX,RANGE
C2025 FORMAT(3X,16HMINIMUM PHASE = ,E15.8,11H RADIANS   ,16HMAXIMUM
CCCCC1PHASE = ,E15.8,11H RADIANS   ,18HRANGE OF PHASES = ,E15.8,8H RADIA
CCCCC1NS)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,2030)SLOPER,SLOPEH,FEST
C2030 FORMAT(8HSLOPE = ,E14.8,11H RADIANS = ,E14.6,52H CYCLES PER UN
CCCCC1IT TIME    EST. OF NEW DEMOD. FREQ. = ,E15.8,15H CYC./UNIT TIME)
CCCCC CALL DPWRST('XXX','BUG ')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPCDC3(Y,N,ICASA2,ICASA4,ISEED,MAXNXT,
     1                  TEMP1,ALPHA,NALPHA,ALOWLM,AUPPLM,
     1                  CD,YMED,YAAD,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES CONFIDENCE LIMITS FOR THE
C              COEFFIENT OF DISPERSION.  THE COEFFICIENT OF DISPERSION
C              IS AN ALTERNATIVE TO THE COEFFICIENT OF VARIATION FOR
C              NON-NORMAL DATA.
C
C              THE FOLLOWING CASES ARE SUPPORTED:
C
C                 LET A = LOWER COEFFICIENT OF DISPERSION CONFIDENCE LIMIT Y
C                 LET A = UPPER COEFFICIENT OF DISPERSION CONFIDENCE LIMIT Y
C                 LET A = ONE SIDED LOWER COEFFICIENT OF DISPERSION CONFIDENCE LIMIT Y
C                 LET A = ONE SIDED UPPER COEFFICIENT OF DISPERSION CONFIDENCE LIMIT Y
C
C              THE DATA CONSISTS OF N OBSERVATIONS IN Y.
C
C              THIS ALGORITHM IS FROM THE BONETT AND SEIER PAPER.
C
C              THE COEFFICIENT OF DISPERSION IS DEFINED AS:
C
C                  CD = MEAN ABSOLUTE DEVIATION/MEDIAN
C
C     INPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED OR SORTED) OBSERVATIONS.
C                    --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR Y.
C                    --ALPHA  = THE SINGLE PRECISION VECTOR OF CONFIDENCE
C                               LEVELS
C                      NALPHA = THE INTEGER NUMBER OF ALPHA VALUES
C     OUTPUT ARGUMENTS-ALOWLM = THE SINGLE PRECISION VECTOR OF LOWER
C                               CONFIDENCE LIMIT VALUES
C                     -AUPPLM = THE SINGLE PRECISION VECTOR OF UPPER
C                               CONFIDENCE LIMIT VALUES
C     OTHER DATAPAC   SUBROUTINES NEEDED--MEDIAN, MEAN, VAR, AAD, SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--BONETT AND SEIER (2006), "CONFIDENCE INTERVAL FOR A
C                 COEFFICIENT OF DISPERSION", BIOMETRICAL JOURNAL,
C                 VOL. 48, NO. 1, PP. 144-148.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     ORIGINAL VERSION--NOVEMBER  2017. 
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION ALOWLM(*)
      DIMENSION AUPPLM(*)
      DIMENSION ALPHA(*)
C
      INTEGER ASTAR
      INTEGER BSTAR
C
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA4
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='CDC3'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
C
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
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        DO76I=1,NALPHA
          WRITE(ICOUT,77)I,ALPHA(I)
   77     FORMAT('I,ALPHA(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   76   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO110I=1,NALPHA
        ALOWLM(I)=CPUMIN
        AUPPLM(I)=CPUMIN
  110 CONTINUE
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: COEFFICIENT OF DISPERSION CONFIDENCE ',
     1         'LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF ORIGINAL OBSERVATIONS  IS LESS ',
     1         'THAN THREE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CARRY OUT CALCULATIONS FOR CONFIDENCE **
C               **  LIMITS.                               **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'CDC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     ICASA2:  LOWE     => LOWER LIMIT
C              UPPE     => UPPER LIMIT
C     ICASA4:  ONES     => ONE-SIDED LIMIT
C              TWOS     => TWO-SIDED LIMIT
C
C     COMPUTE MEDIAN AND MEAN ABSOLUTE DEVIATION FROM MEDIAN
C
      CALL MEDIAN(Y,N,IWRITE,TEMP1,MAXNXT,YMED,IBUGA3,IERROR)
      CALL AAD(Y,N,IWRITE,TEMP1,MAXNXT,YAAD,'MEDI',IBUGA3,IERROR)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,201)YMED,YAAD
  201   FORMAT('YMED,YAAD = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IF(YAAD.EQ.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,207)
  207   FORMAT('      THE MEAN ABSOLUTE DEVIATION FROM THE MEDIAN IS ',
     1         'ZERO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,209)
  209   FORMAT('      THE COEFFICIENT OF DISPERSION CONFIDENCE LIMIT ',
     1         'IS NOT COMPUTED IN THIS CASE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
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
        GOTO9000
      ENDIF
C
      CD=YAAD/YMED
C
      CALL MEAN(Y,N,IWRITE,U,IBUGA3,IERROR)
      CALL VAR(Y,N,IWRITE,V,IBUGA3,IERROR)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,220)CD,U,V
  220   FORMAT('CD,U,V = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
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
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,223)DEL,GAMC,VRLETA,SE1,VRLTAU,SE2
  223   FORMAT('DEL,GAMC,VRLETA,SE1,VRLTAU,SE2 = ',6G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,225)ASTAR,BSTAR,CVLTLE,AK
  225   FORMAT('ASTAR,BSTAR,CVLTLE,AK = ',2I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      DO300I=1,NALPHA
C
C       GET NORMAL CRITICAL VALUE
C
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
C
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
C
C         COMPUTE BOTH UPPER AND LOWER LIMIT.  LET CALLING ROUTINE
C         DETERMINE WHICH TO USE.
C
          ALOWLM(I)=EXP(AL1-AU2STR)
          AUPPLM(I)=EXP(AU1 -AL2STR)
        ENDIF
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CDC3')THEN
          WRITE(ICOUT,311)I,IA,IB,Z,AL2STR,AU2STR
  311     FORMAT('I,IA,IB,Z,AL2STR,AU2STR = ',3I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,313)AL1,AU1,ALOWLM(I),AUPPLM(I)
  313     FORMAT('AL1,AU1,ALOWLM(I),AUPPLM(I) = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
  300 CONTINUE
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CDC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9051)
 9051   FORMAT('**** AT THE END OF DPCDC3--')
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END 
      SUBROUTINE DPCDF1(Y,Y2,N,ICASPL,IFLAGD,
     1                  SHAPE1,SHAPE2,SHAPE3,SHAPE4,
     1                  SHAPE5,SHAPE6,SHAPE7,
     1                  YLOWLM,YUPPLM,A,B,MINMAX,
     1                  ICAPSW,ICAPTY,
     1                  IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1                  ILGADF,ISKNDF,IGLDDF,IBGEDF,
     1                  IGETDF,ICONDF,IGOMDF,IKATDF,
     1                  IGIGDF,IGEODF,
     1                  KSLOC,KSSCAL,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--COMPUTE THE CDF VALUE AT GIVEN SET OF POINTS.  THIS
C              WILL BE USED BY VARIOUS K-S AND ANDERSON DARLING
C              ROUTINES.  THIS ROUTINE SIMPLY RETURNS THE ARRAY
C              OF COMPUTED CDF VALUES.  THE CALLING ROUTINE IS
C              RESPONSIBLE FOR CONVERTING THAT INTO A K-S,
C              ANDERSON-DARLING, OR SOME OTHER RELEVANT STATISTIC.
C
C              THIS ROUTINE HANDLES THE UNGROUPED, UNCENSORED CASE.
C              IF IFLAGD = 1, THEN DISCRETE DISTRIBUTIONS WILL
C              BE SKIPPED.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009.
C     UPDATED         --JULY      2010. END EFFECTS WEIBULL
C     UPDATED         --AUGUST    2010. BRITTLE FIBER WEIBULL
C     UPDATED         --MARCH     2013. COSINE
C     UPDATED         --MAY       2014. 3-PARAMETER INVERSE GAUSSIAN
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      LOGICAL HYPPNT
C
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
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      REAL KSLOC
      REAL KSSCAL
C
      DOUBLE PRECISION DXOUT
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION CDFGLO
      DOUBLE PRECISION CDFWAK
      DOUBLE PRECISION LANCDF
      DOUBLE PRECISION XPAR(5)
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Y2(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      ISUBN1='DPCD'
      ISUBN2='F1  '
      IERROR='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
CCCCC 2013/07: ALLOW ONE VALUE (FOR CALL FROM DPBEF2).
C
CCCCC IF(N.LT.2)THEN
      NMIN=1
      IF(N.LT.NMIN)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN DPCDF1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)NMIN
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST ',
     1         I1,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N.GT.1)THEN
        HOLD=Y(1)
        DO60I=1,N
          IF(Y(I).NE.HOLD)GOTO69
   60   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)
   62   FORMAT('      ALL ELEMENTS IN THE RESPONSE VARIABLE ARE ',
     1         'IDENTICALLY EQUAL TO ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
   69   CONTINUE
      ENDIF
C
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
        DO85I=1,N
          WRITE(ICOUT,86)I,Y(I)
   86     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   85   CONTINUE
      ENDIF
C
C               ************************************************
C               **  STEP 2.1--                                **
C               **  COMPUTE CDF VALUE AT GIVEN POINTS         **
C               ************************************************
C
      ZSCALE=B - A
      ZLOC=A
      IWRITE='OFF'
      CALL MINIM(Y,N,IWRITE,XMIN,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,XMAX,IBUGA3,IERROR)
C
      IF(ICASPL.EQ.'UNIF')THEN
        DO1010I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL UNICDF(XL,Y2(I))
 1010   CONTINUE
C
      ELSEIF(ICASPL.EQ.'NORM')THEN
        DO1020I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL NODCDF(DBLE(XL),DXOUT)
          Y2(I)=REAL(DXOUT)
 1020   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LOGI')THEN
        DO1030I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LOGCDF(XL,Y2(I))
 1030   CONTINUE
C
      ELSEIF(ICASPL.EQ.'DEXP')THEN
        DO1040I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DEXCDF(XL,Y2(I))
 1040   CONTINUE
C
      ELSEIF(ICASPL.EQ.'CAUC')THEN
        DO1050I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL CAUCDF(XL,Y2(I))
 1050   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TULA')THEN
        DO1060I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LAMCDF(XL,SHAPE1,Y2(I))
 1060   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LOGN' .OR. ICASPL.EQ.'3LGN')THEN
        DO1070I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LGNCDF(XL,SHAPE1,Y2(I))
 1070   CONTINUE
C
      ELSEIF(ICASPL.EQ.'HNOR' .OR. ICASPL.EQ.'1HNO')THEN
        DO1080I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HFNCDF(XL,Y2(I))
 1080   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TPP')THEN
        DO1090I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TCDF(XL,SHAPE1,Y2(I))
 1090   CONTINUE
C
      ELSEIF(ICASPL.EQ.'CHIS')THEN
        DO1100I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL CHSCDF(XL,INT(SHAPE1+0.1),Y2(I))
 1100   CONTINUE
C
      ELSEIF(ICASPL.EQ.'FPP')THEN
        DO1110I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL FCDF(XL,INT(SHAPE1+0.1),INT(SHAPE2+0.1),Y2(I))
 1110   CONTINUE
C
      ELSEIF(ICASPL.EQ.'EXPO')THEN
        DO1120I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL EXPCDF(XL,Y2(I))
 1120   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GAMM' .OR. ICASPL.EQ.'3GAM')THEN
        DO1130I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GAMCDF(XL,SHAPE1,Y2(I))
 1130   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BETA' .OR. ICASPL.EQ.'4BET')THEN
        DO1140I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL BETCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1140   CONTINUE
C
      ELSEIF(ICASPL.EQ.'WEIB' .OR. ICASPL.EQ.'3WEI')THEN
        DO1150I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL WEICDF(XL,SHAPE1,MINMAX,Y2(I))
 1150   CONTINUE
C
      ELSEIF(ICASPL.EQ.'EV1 ')THEN
        DO1160I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL EV1CDF(XL,MINMAX,Y2(I))
 1160   CONTINUE
C
      ELSEIF(ICASPL.EQ.'EV2 ' .OR. ICASPL.EQ.'3EV2')THEN
        DO1170I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL EV2CDF(XL,SHAPE1,MINMAX,Y2(I))
 1170   CONTINUE
C
      ELSEIF(ICASPL.EQ.'PARE')THEN
        ZLOC=SHAPE2
        IF(ZLOC.GT.XMIN)ZLOC=XMIN
        DO1180I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PARCDF(XL,SHAPE1,ZLOC,Y2(I))
 1180   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BINO')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1190I=1,N
          XL=Y(I)
          CALL BINCDF(DBLE(XL),DBLE(SHAPE1),INT(SHAPE2+0.1),DXOUT)
          Y2(I)=REAL(DXOUT)
 1190   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GEOM')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        IF(IGEODF.EQ.'DLMF')THEN
          DO1200I=1,N
            XL=Y(I)
            CALL GE2CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
            Y2(I)=REAL(DXOUT)
 1200     CONTINUE
        ELSE
          DO1205I=1,N
            XL=Y(I)
            CALL GEOCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
            Y2(I)=REAL(DXOUT)
 1205     CONTINUE
        ENDIF
C
      ELSEIF(ICASPL.EQ.'POIS')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1210I=1,N
          XL=Y(I)
          CALL POICDF(XL,SHAPE1,Y2(I))
 1210   CONTINUE
C
      ELSEIF(ICASPL.EQ.'NEBI')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1220I=1,N
          XL=Y(I)
          CALL NBCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 1220   CONTINUE
C
      ELSEIF(ICASPL.EQ.'SEMI')THEN
        DO1230I=1,N
          XL=Y(I) - KSLOC
          CALL SEMCDF(XL,KSSCAL,Y2(I))
 1230   CONTINUE
C
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
 1344     FORMAT('       OF THE SHAPE PARAMETER IS OUTSIDE THE ',
     1           'INTERVAL')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1345)
 1345     FORMAT('       OF THE LOWER AND UPPER LIMIT PARAMETERS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1346)SHAPE1
 1346     FORMAT('       THE VALUE OF THE SHAPE PARAMETER       = ',
     1         G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1347)ZLOWLM
 1347     FORMAT('       THE VALUE OF THE LOWER LIMIT PARAMETER = ',
     1           G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1348)ZUPPLM
 1348     FORMAT('       THE VALUE OF THE LOWER LIMIT PARAMETER = ',
     1           G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        DO1240I=1,N
          XL=Y(I)
          CALL TRICDF(XL,SHAPE1,ZLOWLM,ZUPPLM,Y2(I))
 1240   CONTINUE
C
      ELSEIF(ICASPL.EQ.'INGA' .OR. ICASPL.EQ.'3IGA')THEN
        DO1250I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IGCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DCDF)
          Y2(I)=REAL(DCDF)
 1250   CONTINUE
C
      ELSEIF(ICASPL.EQ.'WALD')THEN
        AMU=1.0
        DO1260I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IGCDF(DBLE(XL),DBLE(SHAPE1),DBLE(AMU),DCDF)
          Y2(I)=REAL(DCDF)
 1260   CONTINUE
C
      ELSEIF(ICASPL.EQ.'RIGA')THEN
        DO1270I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL RIGCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DCDF)
          Y2(I)=REAL(DCDF)
 1270   CONTINUE
C
      ELSEIF(ICASPL.EQ.'FATL')THEN
        DO1280I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL FLCDF(XL,SHAPE1,Y2(I))
 1280   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GPAR')THEN
        DO1290I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GEPCDF(XL,SHAPE1,MINMAX,IGEPDF,Y2(I))
 1290   CONTINUE
C
      ELSEIF(ICASPL.EQ.'DUNI')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1300I=1,N
          XL=Y(I)
          IXL=INT(XL+0.1)
          CALL DISCDF(IXL,INT(SHAPE1+0.1),Y2(I))
 1300   CONTINUE
C
      ELSEIF(ICASPL.EQ.'NCT ')THEN
        DO1310I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL NCTCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1310   CONTINUE
C
      ELSEIF(ICASPL.EQ.'NCF ')THEN
        DO1320I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL NCFCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1320   CONTINUE
C
      ELSEIF(ICASPL.EQ.'NCCS')THEN
        DO1330I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL NCCCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1330   CONTINUE
C
      ELSEIF(ICASPL.EQ.'NCBE')THEN
        DO1340I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL NCBCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1340   CONTINUE
C
      ELSEIF(ICASPL.EQ.'DNCT')THEN
        DO1350I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DNTCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1350   CONTINUE
C
      ELSEIF(ICASPL.EQ.'DNCF')THEN
        DO1360I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DNFCDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,Y2(I))
 1360   CONTINUE
C
      ELSEIF(ICASPL.EQ.'HYPG')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        HYPPNT=.FALSE.
        DO1365I=1,N
          XL=Y(I)
          CALL HYPCDF(INT(XL+0.1),INT(SHAPE1+0.1),INT(SHAPE2+0.1),
     1                INT(SHAPE3+0.1),HYPPNT,Y2(I))
 1365   CONTINUE
C
      ELSEIF(ICASPL.EQ.'VONM')THEN
        DO1370I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL VONCDF(XL,SHAPE1,Y2(I))
 1370   CONTINUE
C
      ELSEIF(ICASPL.EQ.'POWN')THEN
        DO1380I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PNRCDF(XL,SHAPE1,Y2(I))
 1380   CONTINUE
C
      ELSEIF(ICASPL.EQ.'PLGN')THEN
        DO1390I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PLNCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1390   CONTINUE
C
      ELSEIF(ICASPL.EQ.'ALPH')THEN
        DO1400I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ALPCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 1400   CONTINUE
C
      ELSEIF(ICASPL.EQ.'COSI')THEN
        DO1410I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL COSCDF(XL,Y2(I))
 1410   CONTINUE
C
      ELSEIF(ICASPL.EQ.'SINE')THEN
        DO1415I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL SINCDF(XL,Y2(I))
 1415   CONTINUE
C
      ELSEIF(ICASPL.EQ.'POWF')THEN
        DO1420I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL POWCDF(XL,SHAPE1,Y2(I))
 1420   CONTINUE
C
      ELSEIF(ICASPL.EQ.'CHI ')THEN
        DO1430I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL CHCDF(XL,SHAPE1,Y2(I))
 1430   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LOGS')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1435I=1,N
          XL=Y(I)
          CALL DLGCDF(XL,SHAPE1,Y2(I))
 1435   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LOGL')THEN
        DO1440I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LLGCDF(XL,SHAPE1,Y2(I))
 1440   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GGAM')THEN
        DO1450I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GGDCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1450   CONTINUE
C
      ELSEIF(ICASPL.EQ.'WARI')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1460I=1,N
          XL=Y(I)
CCCCC     CALL WARCDF(XL,SHAPE1,SHAPE2,Y2(I),'NOTR')
          CALL WARCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1460   CONTINUE
C
      ELSEIF(ICASPL.EQ.'YULE')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1470I=1,N
          XL=Y(I)
          CALL YULCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 1470   CONTINUE
C
      ELSEIF(ICASPL.EQ.'ANGL')THEN
        DO1480I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ANGCDF(XL,Y2(I))
 1480   CONTINUE
C
      ELSEIF(ICASPL.EQ.'ARSI')THEN
        DO1490I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ARSCDF(XL,Y2(I))
 1490   CONTINUE
C
      ELSEIF(ICASPL.EQ.'FNOR')THEN
C
C       FOR FOLDED NORMAL, ARE PARAMETERS GIVEN AS LOCATION/SCALE
C       OR SHAPE1 AND SHAPE2?
C
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
C
        DO1500I=1,N
CCCCC     XL=(Y(I) - KSLOC)/KSSCAL
CCCCC     CALL FNRCDF(XL,KSLOC,KSSCAL,Y2(I))
          XL=Y(I)
          CALL FNRCDF(XL,AVAL1,AVAL2,Y2(I))
 1500   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TNOR')THEN
        DO1510I=1,N
          XL=Y(I)
          CALL TNRCDF(DBLE(XL),DBLE(A),DBLE(B),
     1                DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 1510   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LGAM')THEN
        DO1520I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LGACDF(XL,SHAPE1,ILGADF,Y2(I))
 1520   CONTINUE
C
      ELSEIF(ICASPL.EQ.'HSEC')THEN
        DO1530I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HSECDF(XL,Y2(I))
 1530   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GOMP')THEN
        DO1540I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GOMCDF(XL,SHAPE1,SHAPE2,IGOMDF,Y2(I))
 1540   CONTINUE
C
      ELSEIF(ICASPL.EQ.'HCAU')THEN
        DO1550I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HFCCDF(XL,Y2(I))
 1550   CONTINUE
C
      ELSEIF(ICASPL.EQ.'HALO')THEN
        SHAPE1=-1.0
        DO1560I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HFLCDF(XL,SHAPE1,Y2(I))
 1560   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GHLO')THEN
        DO1570I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HFLCDF(XL,SHAPE1,Y2(I))
 1570   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GEV ')THEN
        DO1580I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GEVCDF(XL,SHAPE1,MINMAX,Y2(I))
 1580   CONTINUE
C
      ELSEIF(ICASPL.EQ.'PAR2')THEN
        ZLOC=SHAPE2
        IF(ZLOC.GT.XMIN)ZLOC=XMIN
        DO1590I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PA2CDF(XL,SHAPE1,ZLOC,Y2(I))
 1590   CONTINUE
C
      ELSEIF(ICASPL.EQ.'DWEI')THEN
        DO1600I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DWECDF(XL,SHAPE1,Y2(I))
 1600   CONTINUE
C
      ELSEIF(ICASPL.EQ.'WCAU')THEN
        DO1610I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL WCACDF(XL,SHAPE1,Y2(I))
 1610   CONTINUE
C
      ELSEIF(ICASPL.EQ.'EWEI')THEN
        IARG1=1
        DO1620I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL EWECDF(XL,SHAPE1,SHAPE2,IARG1,Y2(I))
 1620   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TEXP')THEN
        DO1630I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TNECDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1630   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GLOG')THEN
        DO1640I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GLOCDF(XL,SHAPE1,Y2(I))
 1640   CONTINUE
C
      ELSEIF(ICASPL.EQ.'PEXP')THEN
        DO1650I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PEXCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 1650   CONTINUE
C
      ELSEIF(ICASPL.EQ.'DGAM')THEN
        DO1660I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DGACDF(XL,SHAPE1,Y2(I))
 1660   CONTINUE
C
      ELSEIF(ICASPL.EQ.'MBKA')THEN
        DO1670I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL MIECDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 1670   CONTINUE
C
      ELSEIF(ICASPL.EQ.'FCAU')THEN
        DO1680I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL FCACDF(XL,SHAPE1,SHAPE2,Y2(I))
 1680   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BBIN')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1690I=1,N
          XL=Y(I)
          CALL BBNCDF(XL,SHAPE1,SHAPE2,INT(SHAPE3+0.1),Y2(I))
 1690   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BRAD')THEN
        DO1700I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BRACDF(XL,SHAPE1,Y2(I))
 1700   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GEXP')THEN
        DO1710I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GEXCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1710   CONTINUE
C
      ELSEIF(ICASPL.EQ.'RECI')THEN
        DO1715I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL RECCDF(XL,SHAPE1,Y2(I))
 1715   CONTINUE
C
      ELSEIF(ICASPL.EQ.'NORX')THEN
        DO1720I=1,N
          XL=Y(I)
          CALL NMXCDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,
     1                Y2(I))
 1720   CONTINUE
C
      ELSEIF(ICASPL.EQ.'IGAM')THEN
        DO1730I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IGACDF(XL,SHAPE1,Y2(I))
 1730   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GTLA')THEN
        DO1740I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GLDCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT,
     1                IGLDDF,IWRITE)
          Y2(I)=REAL(DXOUT)
 1740   CONTINUE
C
      ELSEIF(ICASPL.EQ.'JOSB')THEN
        DO1750I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL JSBCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1750   CONTINUE
C
      ELSEIF(ICASPL.EQ.'JOSU')THEN
        DO1760I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL JSUCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1760   CONTINUE
C
      ELSEIF(ICASPL.EQ.'IWEI')THEN
        DO1770I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IWECDF(XL,SHAPE1,Y2(I))
 1770   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LDEX')THEN
        DO1780I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LDECDF(XL,SHAPE1,Y2(I))
 1780   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GEEX')THEN
        DO1790I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GEECDF(XL,SHAPE1,Y2(I))
 1790   CONTINUE
C
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
 1944     FORMAT('       VALUE OF THE THETA SHAPE PARAMETER IS ',
     1           'OUTSIDE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1945)
 1945     FORMAT('       INTERVAL OF THE LOWER AND UPPER LIMIT ',
     1           'PARAMETERS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1946)SHAPE1
 1946     FORMAT('       THE VALUE OF THE THETA SHAPE PARAMETER = ',
     1           G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1947)ZLOWLM
 1947     FORMAT('       THE VALUE OF THE LOWER LIMIT PARAMETER = ',
     1           G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1948)ZUPPLM
 1948     FORMAT('       THE VALUE OF THE LOWER LIMIT PARAMETER = ',
     1           G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        DO1800I=1,N
          XL=Y(I)
          CALL TSPCDF(XL,SHAPE1,SHAPE2,A,B,Y2(I))
 1800   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BWEI')THEN
        DO1810I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BWECDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,
     1                Y2(I),DXOUT)
 1810   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GHPP')THEN
        DO1820I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GHCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1820   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GPP')THEN
        HTEMP=0.0
        DO1821I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GHCDF(XL,SHAPE1,HTEMP,Y2(I))
 1821   CONTINUE
C
      ELSEIF(ICASPL.EQ.'HPP')THEN
        GTEMP=0.0
        DO1823I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GHCDF(XL,GTEMP,SHAPE1,Y2(I))
 1823   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LAND')THEN
        DO1830I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          DXOUT=LANCDF(DBLE(XL))
          Y2(I)=REAL(DXOUT)
 1830   CONTINUE
C
      ELSEIF(ICASPL.EQ.'ERRO')THEN
        DO1840I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ERRCDF(XL,SHAPE1,Y2(I))
 1840   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TRAP')THEN
        DO1850I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TRACDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,Y2(I))
 1850   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GTRA')THEN
        DO1860I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GTRCDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,
     1                SHAPE6,SHAPE7,Y2(I))
 1860   CONTINUE
C
      ELSEIF(ICASPL.EQ.'FT  ')THEN
        DO1870I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL FTCDF(XL,INT(SHAPE1+0.1),Y2(I))
 1870   CONTINUE
C
      ELSEIF(ICASPL.EQ.'SLAS')THEN
        DO1880I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL SLACDF(XL,Y2(I))
 1880   CONTINUE
C
      ELSEIF(ICASPL.EQ.'SNOR')THEN
        DO1890I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL SNCDF(XL,SHAPE1,ISKNDF,Y2(I))
 1890   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TSKE')THEN
        DO1900I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL STCDF(XL,INT(SHAPE1+0.1),SHAPE2,Y2(I))
 1900   CONTINUE
C
      ELSEIF(ICASPL.EQ.'IBET')THEN
        DO1910I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL IBCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1910   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GOMM')THEN
        IF(IMAKDF.EQ.'DLMF')THEN
          DO1930I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL MAKCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 1930     CONTINUE
        ELSEIF(IMAKDF.EQ.'MEEK')THEN
          XI=SHAPE1/SHAPE3
          THETA=SHAPE2/SHAPE1
          ALAMB=SHAPE3
          DO1935I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL MAKCDF(XL,XI,ALAMBA,THETA,Y2(I))
 1935     CONTINUE
        ELSEIF(IMAKDF.EQ.'REPA')THEN
          DO1938I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL MA2CDF(XL,SHAPE1,SHAPE2,Y2(I))
 1938     CONTINUE
        ENDIF
C
      ELSEIF(ICASPL.EQ.'LSNO')THEN
        DO1940I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LSNCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1940   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LSKT')THEN
        DO1950I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LSTCDF(XL,INT(SHAPE1+0.1),SHAPE2,SHAPE3,Y2(I))
 1950   CONTINUE
C
      ELSEIF(ICASPL.EQ.'POLY')THEN
        DO1960I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL POLCDF(XL,SHAPE1,SHAPE2,INT(SHAPE3+0.1),Y2(I))
 1960   CONTINUE
C
      ELSEIF(ICASPL.EQ.'HERM')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO1970I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL HERCDF(XL,SHAPE1,SHAPE2,Y2(I))
 1970   CONTINUE
C
      ELSEIF(ICASPL.EQ.'SDEX')THEN
        DO1980I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL SDECDF(XL,SHAPE1,Y2(I))
 1980   CONTINUE
C
      ELSEIF(ICASPL.EQ.'ADEX')THEN
        DO1990I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ADECDF(XL,SHAPE1,IADEDF,Y2(I))
 1990   CONTINUE
C
      ELSEIF(ICASPL.EQ.'MAXW' .OR. ICASPL.EQ.'1MAX')THEN
        AVAL1=KSLOC
        IF(ICASPL.EQ.'1MAX')AVAL1=0.0
        DO2000I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL MAXCDF(XL,Y2(I))
 2000   CONTINUE
C
      ELSEIF(ICASPL.EQ.'RAYL')THEN
        DO2010I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL RAYCDF(XL,Y2(I))
 2010   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GIGA')THEN
        IF(IGIGDF.EQ.'2PAR')THEN
          DO2020I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL GI2CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
            Y2(I)=REAL(DXOUT)
 2020     CONTINUE
        ELSE
          DO2030I=1,N
            XL=(Y(I) - KSLOC)/KSSCAL
            CALL GIGCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                  DBLE(SHAPE3),DXOUT)
            Y2(I)=REAL(DXOUT)
 2030     CONTINUE
        ENDIF
C
      ELSEIF(ICASPL.EQ.'GALP')THEN
        DO2040I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GALCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),IADEDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2040   CONTINUE
C
      ELSEIF(ICASPL.EQ.'MCLE')THEN
        DO2050I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL MCLCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2050   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BEIP')THEN
        DO2060I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BEICDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DBLE(SHAPE3),
     1                IBEIDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2060   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BEIK')THEN
        DO2070I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
CCCCC     CALL BEKCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DBLE(SHAPE3),
CCCCC1                IBEIDF,DXOUT)
CCCCC     Y2(I)=REAL(DXOUT)
          Y2(I)=0.0
 2070   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GMCL')THEN
        DO2080I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GMCCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2080   CONTINUE
C
      ELSEIF(ICASPL.EQ.'G5LO')THEN
        XPAR(1)=DBLE(KSLOC)
        XPAR(2)=DBLE(KSSCAL)
        XPAR(3)=DBLE(SHAPE1)
        DO2090I=1,N
          XL=Y(I)
          DXOUT=CDFGLO(DBLE(XL),XPAR)
          Y2(I)=REAL(DXOUT)
 2090   CONTINUE
C
      ELSEIF(ICASPL.EQ.'WAKE')THEN
        XPAR(1)=DBLE(KSLOC)
        XPAR(2)=DBLE(KSSCAL)
        XPAR(3)=DBLE(SHAPE1)
        XPAR(4)=DBLE(SHAPE2)
        XPAR(5)=DBLE(SHAPE3)
        DO2100I=1,N
          XL=Y(I)
          DXOUT=CDFWAK(DBLE(XL),XPAR)
          Y2(I)=REAL(DXOUT)
 2100   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BNOR')THEN
        DO2110I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BNOCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2110   CONTINUE
C
      ELSEIF(ICASPL.EQ.'G2LO')THEN
        DO2120I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GL2CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2120   CONTINUE
C
      ELSEIF(ICASPL.EQ.'G3LO')THEN
        DO2130I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GL3CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2130   CONTINUE
C
      ELSEIF(ICASPL.EQ.'G4LO')THEN
        DO2140I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL GL4CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2140   CONTINUE
C
      ELSEIF(ICASPL.EQ.'ALDE')THEN
        DO2150I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL ALDCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2150   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BGEO')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        IF(IBGEDF.EQ.'UNSH')THEN
          DO2160I=1,N
            XL=Y(I)
            CALL BGECDF(XL,SHAPE1,SHAPE2,Y2(I))
 2160     CONTINUE
        ELSE
          DO2165I=1,N
            XL=Y(I)
            CALL BG2CDF(XL,SHAPE1,SHAPE2,Y2(I))
 2165     CONTINUE
        ENDIF
C
      ELSEIF(ICASPL.EQ.'ZETA')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2170I=1,N
          XL=Y(I)
          CALL ZETCDF(XL,SHAPE1,Y2(I))
 2170   CONTINUE
C
      ELSEIF(ICASPL.EQ.'ZIPF')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2180I=1,N
          XL=Y(I)
          CALL ZIPCDF(XL,SHAPE1,INT(SHAPE2+0.1),Y2(I))
 2180   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BTAN')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2190I=1,N
          XL=Y(I)
          CALL BTACDF(XL,SHAPE1,SHAPE2,Y2(I))
 2190   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BNBI')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2200I=1,N
          XL=Y(I)
          CALL GWACDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2200   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LPOI')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2210I=1,N
          XL=Y(I)
          CALL LPOCDF(XL,SHAPE1,SHAPE2,Y2(I))
 2210   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LICT')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2220I=1,N
          XL=Y(I)
          CALL LCTCDF(XL,INT(SHAPE1+0.1),Y2(I))
 2220   CONTINUE
C
      ELSEIF(ICASPL.EQ.'MATC')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2230I=1,N
          XL=Y(I)
          CALL MATCDF(XL,INT(SHAPE1+0.1),Y2(I))
 2230   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LBET')THEN
        YLOWLM=SHAPE3
        YUPPLM=SHAPE4
        EPS=(XMAX-XMIN)*0.01
        IF(YLOWLM.GT.XMIN)YLOWLM=XMIN-EPS
        IF(YUPPLM.LT.XMAX)YUPPLM=XMAX+EPS
        DO2240I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LBECDF(XL,SHAPE1,SHAPE2,YLOWLM,YUPPLM,Y2(I))
 2240   CONTINUE
C
      ELSEIF(ICASPL.EQ.'AEPP')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2250I=1,N
          XL=Y(I)
          CALL PAPCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2250   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GLOS')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2270I=1,N
          XL=Y(I)
          CALL GLSCDF(XL,SHAPE1,SHAPE2,Y2(I))
 2270   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GNBI')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2280I=1,N
          XL=Y(I)
          CALL GNBCDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 2280   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GEET')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2290I=1,N
          XL=Y(I)
          CALL GETCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                IGETDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2290   CONTINUE
C
      ELSEIF(ICASPL.EQ.'QBIN')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2300I=1,N
          XL=Y(I)
          CALL QBICDF(XL,SHAPE1,SHAPE2,SHAPE3,Y2(I))
 2300   CONTINUE
C
      ELSEIF(ICASPL.EQ.'CONS')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2310I=1,N
          XL=Y(I)
          CALL CONCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                ICONDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2310   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LKAT')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2320I=1,N
          XL=Y(I)
          CALL LKCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2320   CONTINUE
C
      ELSEIF(ICASPL.EQ.'KATZ')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2330I=1,N
          XL=Y(I)
          CALL KATCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),IKATDF,DXOUT)
          Y2(I)=REAL(DXOUT)
 2330   CONTINUE
C
      ELSEIF(ICASPL.EQ.'DISW')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2340I=1,N
          XL=Y(I)
          CALL DIWCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2340   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GLGP')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2350I=1,N
          XL=Y(I)
          CALL GLGCDF(XL,SHAPE1,INT(SHAPE2+0.1),SHAPE3,Y2(I))
 2350   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TGNB')THEN
        IF(IFLAGD.EQ.1)GOTO8000
        DO2360I=1,N
          XL=Y(I)
          CALL GNTCDF(XL,SHAPE1,SHAPE2,SHAPE3,INT(SHAPE4+0.1),Y2(I))
 2360   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TOPL')THEN
        DO2370I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TOPCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2370   CONTINUE
C
      ELSEIF(ICASPL.EQ.'GTOL')THEN
        DO2380I=1,N
          XL=Y(I)
          CALL GTLCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(A),DBLE(B),DXOUT)
          Y2(I)=REAL(DXOUT)
 2380   CONTINUE
C
      ELSEIF(ICASPL.EQ.'RGTL')THEN
        DO2390I=1,N
          XL=Y(I)
          CALL RGTCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(A),DBLE(B),DXOUT)
          Y2(I)=REAL(DXOUT)
 2390   CONTINUE
C
      ELSEIF(ICASPL.EQ.'SLOP')THEN
        DO2400I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL SLOCDF(XL,SHAPE1,Y2(I))
 2400   CONTINUE
C
      ELSEIF(ICASPL.EQ.'OGIV')THEN
        DO2410I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL OGICDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2410   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TSSL')THEN
        DO2420I=1,N
          XL=Y(I)
          CALL TSSCDF(XL,SHAPE1,SHAPE2,
     1                A,B,Y2(I))
 2420   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TSOG')THEN
        DO2430I=1,N
          XL=Y(I)
          CALL TSOCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(A),DBLE(B),DXOUT)
          Y2(I)=REAL(DXOUT)
 2430   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BUR2')THEN
        DO2450I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU2CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2450   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BUR3')THEN
        DO2460I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU3CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2460   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BUR4')THEN
        DO2470I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU4CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2470   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BUR5')THEN
        DO2480I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU5CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2480   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BUR6')THEN
        DO2490I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU6CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2490   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BUR7')THEN
        DO2500I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU7CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2500   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BUR8')THEN
        DO2510I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU8CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2510   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BUR9')THEN
        DO2520I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BU9CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2520   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BU10' .OR. ICASPL.EQ.'B10' .OR.
     1       ICASPL.EQ.'3B10')THEN
        DO2530I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL B10CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2530   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BU11')THEN
        DO2540I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL B11CDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2540   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BU12')THEN
        DO2550I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL B12CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2550   CONTINUE
C
      ELSEIF(ICASPL.EQ.'DPUN')THEN
        DO2560I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL DPUCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(SHAPE3),DBLE(SHAPE4),DXOUT)
          Y2(I)=REAL(DXOUT)
 2560   CONTINUE
C
      ELSEIF(ICASPL.EQ.'KUMA')THEN
        DO2570I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL KUMCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DXOUT)
          Y2(I)=REAL(DXOUT)
 2570   CONTINUE
C
      ELSEIF(ICASPL.EQ.'RPOW')THEN
        DO2580I=1,N
          XL=(Y(I) - ZLOC)/ZSCALE
          CALL RPOCDF(XL,SHAPE1,Y2(I))
 2580   CONTINUE
C
      ELSEIF(ICASPL.EQ.'UTSP')THEN
        DO2590I=1,N
          XL=Y(I)
          CALL UTSCDF(XL,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,
     1                SHAPE6,Y2(I))
 2590   CONTINUE
C
      ELSEIF(ICASPL.EQ.'MUTH')THEN
        DO2600I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL MUTCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2600   CONTINUE
C
      ELSEIF(ICASPL.EQ.'LEXP')THEN
        DO2610I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LEXCDF(DBLE(XL),DBLE(SHAPE1),DXOUT)
          Y2(I)=REAL(DXOUT)
 2610   CONTINUE
C
      ELSEIF(ICASPL.EQ.'TPAR')THEN
        DO2620I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL TNPCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2620   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BFRA')THEN
        DO2630I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL BFRCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2630   CONTINUE
C
      ELSEIF(ICASPL.EQ.'L3EX')THEN
        DO2640I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL LE3CDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(SHAPE3),DXOUT)
          Y2(I)=REAL(DXOUT)
 2640   CONTINUE
C
      ELSEIF(ICASPL.EQ.'KAPP')THEN
        DO2650I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL KAPCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),
     1                DBLE(KSLOC),DBLE(KSSCAL),DXOUT)
          Y2(I)=REAL(DXOUT)
 2650   CONTINUE
C
      ELSEIF(ICASPL.EQ.'PEA3')THEN
        DO2660I=1,N
          XL=(Y(I) - KSLOC)/KSSCAL
          CALL PE3CDF(DBLE(XL),DBLE(SHAPE1),
     1                DBLE(KSLOC),DBLE(KSSCAL),DXOUT)
          Y2(I)=REAL(DXOUT)
 2660   CONTINUE
C
      ELSEIF(ICASPL.EQ.'EEWE')THEN
        DO2670I=1,N
          XL=Y(I)
          CALL EEWCDF(DBLE(XL),DBLE(SHAPE1),DBLE(SHAPE2),DBLE(SHAPE3),
     1                DBLE(SHAPE4),DBLE(SHAPE5),DXOUT)
          Y2(I)=REAL(DXOUT)
 2670   CONTINUE
C
      ELSEIF(ICASPL.EQ.'BFWE')THEN
        DO2680I=1,N
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
        GOTO9000
      ENDIF
C
      GOTO9000
C
C     SET AN ERROR FLAG TO INDICATE A DISCRETE DISTRIBUTION
C     IS NOT TO BE PROCESSED.
C
 8000 CONTINUE
      IFLAGD=99
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
        DO9020I=1,N
          WRITE(ICOUT,9021)I,Y(I),Y2(I)
 9021     FORMAT('I,Y(I),Y2(I), = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCHAL(ICHAR2,ICHARN,IBUGXX,IFOUND)
C
C     PURPOSE--CONVERT AN ALPHABETIC CHARACTER
C              (A TO Z) INTO A NUMERIC VALUE
C              (1 TO 26).
C     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
C                              CONTAINING THE HOLLERITH
C                              CHARACTER(S) OF INTEREST.
C     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
C                              CONTAINING THE NUMERIC
C                              DESIGNATION FOR THE
C                              ALPHABETIC CHARACTER.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH     1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUGXX
      CHARACTER*4 IFOUND
C
      CHARACTER*1 ICH1
      CHARACTER*1 ICH2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      ICH1='-'
      ICH2='-'
C
      ICH1N=(-999)
      ICH2N=(-999)
C
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
C
C               **********************************
C               **  STEP 1--                    **
C               **  CONVERT THE CHARACTER       **
C               **********************************
C
      ICH2(1:1)=ICHAR2(2:2)
CCCCC ICH2N=ICHAR(ICH2)
      CALL DPCOAN(ICH2,ICH2N)
      IF(ICH2N.EQ.32)GOTO1100
      GOTO7900
C
 1100 CONTINUE
      ICH1(1:1)=ICHAR2(1:1)
CCCCC ICH1N=ICHAR(ICH1)
      CALL DPCOAN(ICH1,ICH1N)
      ICHARN=ICH1N-64
      IF(1.LE.ICHARN.AND.ICHARN.LE.26)GOTO8000
      GOTO7900
C
 7900 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7911)
C7911 FORMAT('***** ERROR IN DPCHAL--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7912)
C7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7913)ICHAR
C7913 FORMAT('      INPUT CHARACTER = ',A4)
CCCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GOTO9000
C
 8000 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCHAN(MAXCHA,ACHAAN,
     1IBUGP2,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER ANGLES FOR USE IN MULTI-TRACE PLOTS.
C              THE ANGLE FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE FLOATING POINT
C              VECTOR ACHAAN(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --MAXCHA
C     OUTPUT ARGUMENTS--ACHAAN  (A  FLOATING POINT VECTOR
C                       WHOSE I-TH ELEMENT IS THE ANGLE
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --ACHAAN = CHARACTER ANGLE
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/11
C     ORIGINAL VERSION--NOVEMBER  1986.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC CHARACTER*4 IHARG        DECEMBER 1986
CCCCC CHARACTER*4 IARGT        DECEMBER 1986
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEQ
      CHARACTER*4 IWRITE
C
C---------------------------------------------------------------------
C
CCCCC DIMENSION IHARG(*)       DECEMBER 1986
CCCCC DIMENSION IARGT(*)       DECEMBER 1986
CCCCC DIMENSION IARG(*)        DECEMBER 1986
CCCCC DIMENSION ARG(*)         DECEMBER 1986
C
      DIMENSION ACHAAN(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCH'
      ISUBN2='AN  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'ANGL')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'ANGL')GOTO1105
      GOTO9000
C
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GOTO1140
C
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'AUTO')GOTO3000
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      ACHAAN(I)=0.0
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ACHAAN(I)
 1116 FORMAT('ALL CHARACTER ANGLES HAVE JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO8000
C
 1120 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GOTO1180
      ACHAAN(1)=ARG(2)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,ACHAAN(I)
 1126 FORMAT('THE ANGLE FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO8000
C
 1130 CONTINUE
      I=1
      IF(IARGT(3).NE.'NUMB')GOTO1180
      DO1135I=1,MAXCHA
      ACHAAN(I)=ARG(3)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ACHAAN(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO8000
C
 1140 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GOTO1180
      DO1145I=1,MAXCHA
      ACHAAN(I)=ARG(2)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ACHAAN(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO8000
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO1155I=1,IMAX
      IP1=I+1
      IF(IARGT(IP1).NE.'NUMB')GOTO1180
      ACHAAN(I)=ARG(IP1)
 1155 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1156I=1,IMAX
      WRITE(ICOUT,1126)I,ACHAAN(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO8000
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      ACHAAN(I)=0.0
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ACHAAN(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO8000
C
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
      GOTO9000
C
C               ***********************************************************
C               **  STEP 30--                                            **
C               **  TREAT THE   CHARACTER ANGLE AUTOMATIC <VARIABLE>  CASE **
C               ***********************************************************
C
 3000 CONTINUE
C
C               ********************************************
C               **  STEP 31--                             **
C               **  CHECK THE VALIDITY OF ARGUMENT 3      **
C               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
C               ********************************************
C
      ISTEPN='31'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(3)
      IHLEF2=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
C
C               *****************************************
C               **  STEP 32--                          **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='32'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO3290
      DO3200J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO3210
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO3210
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO3220
 3200 CONTINUE
      GOTO3290
 3210 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO3290
 3220 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO3290
 3290 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO3295
      WRITE(ICOUT,3291)NUMARG,ILOCQ
 3291 FORMAT('NUMARG,ILOCQ = ',2I8)
      CALL DPWRST('XXX','BUG ')
 3295 CONTINUE
C
C               *********************************************
C               **  STEP 33--                              **
C               **  TEMPORARILY FORM THE VARIABLE Y(.)     **
C               **  WHICH WILL HOLD THE RESPONSE VARIABLE. **
C               **  FORM THIS VARIABLE BY                  **
C               **  BRANCHING TO THE APPROPRIATE SUBCASE   **
C               **  (FULL, SUBSET, OR FOR).                **
C               *********************************************
C
      ISTEPN='33'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO3310
      IF(ICASEQ.EQ.'SUBS')GOTO3320
      IF(ICASEQ.EQ.'FOR')GOTO3330
C
 3310 CONTINUE
      DO3315I=1,NLEFT
      ISUB(I)=1
 3315 CONTINUE
      NQ=NLEFT
      GOTO3350
C
 3320 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GOTO3350
C
 3330 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GOTO3350
C
 3350 CONTINUE
      MINN2=1
      IF(NQ.GE.MINN2)GOTO3360
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3351)
 3351 FORMAT('***** ERROR IN DPCHAN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3352)
 3352 FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',
     1'EXTRACTED,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3353)IHLEFT,IHLEF2
 3353 FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING',
     1'FROM VARIABLE ',A4,A4)
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
      GOTO9000
C
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
      DO3370I=1,IMAX
      IF(ISUB(I).EQ.0)GOTO3370
      J=J+1
C
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)Y(J)=V(IJ)
      IF(ICOLL.EQ.MAXCP1)Y(J)=PRED(I)
      IF(ICOLL.EQ.MAXCP2)Y(J)=RES(I)
      IF(ICOLL.EQ.MAXCP3)Y(J)=YPLOT(I)
      IF(ICOLL.EQ.MAXCP4)Y(J)=XPLOT(I)
      IF(ICOLL.EQ.MAXCP5)Y(J)=X2PLOT(I)
      IF(ICOLL.EQ.MAXCP6)Y(J)=TAGPLO(I)
C
 3370 CONTINUE
      NS=J
      NY=J
C
C               *****************************************
C               **  STEP 34--                          **
C               **  EXTRACT THE DISTINCT VALUES        **
C               **  FROM THE TARGET VARIABLE Y(.)   .  **
C               **  STORE THEM IN X(.)   .             **
C               *****************************************
C
      IWRITE='OFF'
      CALL DISTIN(Y,NY,IWRITE,X,NX,IBUGP2,IERROR)
C
C               ***********************************
C               **  STEP 35--                    **
C               **  SORT THESE DISTINCT VALUES   **
C               **  (IN PLACE).                  **
C               ***********************************
C
      CALL SORT(X,NX,X)
C
C               ******************************************
C               **  STEP 36--                           **
C               **  COPY    THE NUMERIC VALUES IN X(.)  **
C               **  INTO INDIVIDUAL ELEMENTS            **
C               **  OF ACHAAN(.)                        **
C               **  NOTE--MAX NUMBER OF VALUES  = 100   **
C               ******************************************
C
      IMAX=NX
      IF(IMAX.GT.MAXCHA)IMAX=MAXCHA
      DO3650I=1,IMAX
      ACHAAN(I)=X(I)
 3650 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO3679
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO3675I=1,IMAX
      WRITE(ICOUT,3676)I,ACHAAN(I)
 3676 FORMAT('CHARACTER ANGLE ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 3675 CONTINUE
 3679 CONTINUE
      GOTO8000
C
 8000 CONTINUE
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
      IF(NY.LE.0)GOTO9022
      DO9023I=1,NY
      WRITE(ICOUT,9024)I,Y(I)
 9024 FORMAT('I,Y(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9023 CONTINUE
 9022 CONTINUE
      WRITE(ICOUT,9031)NX
 9031 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NX.LE.0)GOTO9032
      DO9033I=1,NX
      WRITE(ICOUT,9034)I,X(I)
 9034 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9033 CONTINUE
 9032 CONTINUE
      WRITE(ICOUT,9041)MAXCHA
 9041 FORMAT('MAXCHA = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NX.LE.0)GOTO9042
      DO9043I=1,NX
      WRITE(ICOUT,9044)I,ACHAAN(I)
 9044 FORMAT('I,ACHAAN(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9043 CONTINUE
 9042 CONTINUE
 9090 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHAR(MAXCHA,ICHAPA,ICHAPO,
     1                  IBUGP2,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTERS FOR USE IN MULTI-TRACE PLOTS.
C              THE CHARACTER FOR THE I-TH TRACE WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE HOLLERITH
C              VECTOR ICHAPA(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --MAXCHA
C     OUTPUT ARGUMENTS--ICHAPA  (A  HOLLERITH VECTOR
C                       WHOSE I-TH ELEMENT IS THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977 
C     UPDATED         --SEPTEMBER 1980 
C     UPDATED         --MARCH     1982 
C     UPDATED         --MAY       1982 
C     UPDATED         --JULY      1983 
C     UPDATED         --NOVEMBER  1986 
C     UPDATED         --JANAURY   1988 (OMIT SORTING FOR CHAR AUTOMATIC)
C     UPDATED         --AUGUST    1987 TUFTE BOX PLOT
C     UPDATED         --NOVEMBER  1988 ERROR BAR PLOT
C     UPDATED         --JUNE      1989 CHAR AUTOMATIC DISTINCT
C     UPDATED         --SEPTEMBER 1990 AUGMENT CONTROL CHART
C     UPDATED         --NOVEMBER  1995 SUPPORT CASE ASIS
C     UPDATED         --FEBRUARY  1998 CHAR <SAVE/RESTORE>
C     UPDATED         --JANUARY   2001 CHAR AUTOMATIC SIGN
C     UPDATED         --FEBRUARY  2003 CHAR VIOLIN PLOT
C     UPDATED         --JUNE      2010 ALLOW 16 CHARACTERS FOR CHARACTER
C                                      PATTERN
C     UPDATED         --DECEMBER  2011 CHARACTER AUTOMATIC OFFSET
C     UPDATED         --JULY      2012 CHARACTER AUTOMATIC DYNAMIC
C     UPDATED         --APRIL     2018 ALLOW 24 CHARACTERS FOR CHARACTER
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC CHARACTER*4 IHARG       DECEMBER 1986
CCCCC CHARACTER*4 ICHAPA
CCCCC CHARACTER*4 ICHAPO
      CHARACTER*24 ICHAPA
      CHARACTER*24 ICHAPO
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
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
CCCCC FOLLOWING LINE JANAURY 2001
      CHARACTER*4 ISIGNF
      CHARACTER*4 IHYPSV
C
      CHARACTER*80 ISTRIN
      CHARACTER*80 ISTRCH
C
C---------------------------------------------------------------------
C
CCCCC DIMENSION IHARG(*)      DECEMBER 1986
      DIMENSION ICHAPA(*)
CCCCC ADD FOLLOWING LINE FEBRUARY 1998.
      DIMENSION ICHAPO(*)
      DIMENSION ICTEXT(100)
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCH'
      ISUBN2='AR  '
      IFOUND='NO'
      IERROR='NO'
      ICHAVN='NULL'
      IHYPSV=IHYPSW
      IHYPSW='OFF'
C
      NCCHAR=0
C
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHAR')THEN
        WRITE(ICOUT,11)ICOM,IHARG(1),IHARG(2),IHARG(3),NUMARG
   11   FORMAT('IN DPCHAR: ICOM,IHARG(1),IHARG(2),IHARG(3),NUMARG = ',
     1         4(2X,A4),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIZE')GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FILL')GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TYPE')GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TABU')GOTO9000
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SAVE')THEN
        DO2163I=1,MAXCHA 
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
        GOTO9000
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'REST')THEN
        DO2168I=1,MAXCHA 
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
        GOTO9000
      ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'BOX'.AND.
     1        IHARG(2).EQ.'PLOT') .OR.
     1       (NUMARG.GE.3.AND.IHARG(2).EQ.'BOX'.AND.
     1        IHARG(3).EQ.'PLOT'))THEN
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
        GOTO2170
      ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'TUFT'.AND.
     1       IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')THEN
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
        GOTO2170
      ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'ERRO'.AND.
     1       IHARG(2).EQ.'BAR'.AND.IHARG(3).EQ.'PLOT')THEN
        IMAX=7
        ICHAPA(1)='CIRC'
        ICHAPA(2)='-'
        ICHAPA(3)='-'
        ICHAPA(4)='|'
        ICHAPA(5)='|'
        ICHAPA(6)=' '
        ICHAPA(7)=' '
        GOTO2170
      ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'I'.AND.
     1       IHARG(2).EQ.'PLOT') .OR.
     1      (NUMARG.GE.3.AND.IHARG(2).EQ.'I'.AND.
     1       IHARG(3).EQ.'PLOT'))THEN
        IMAX=5
        ICHAPA(1)='-'
        ICHAPA(2)='X'
        ICHAPA(3)='-'
        ICHAPA(4)=' '
        ICHAPA(5)=' '
        GOTO2170
      ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'CONT'.AND.
     1       IHARG(2).EQ.'CHAR') .OR.
     1      (NUMARG.GE.3.AND.IHARG(2).EQ.'CONT'.AND.
     1       IHARG(3).EQ.'CHAR'))THEN
        IMAX=7
        ICHAPA(1)='CIRC'
        ICHAPA(2)=' '
        ICHAPA(3)=' '
        ICHAPA(4)=' '
        ICHAPA(5)=' '
        ICHAPA(6)=' '
        ICHAPA(7)=' '
        GOTO2170
      ELSEIF((NUMARG.GE.2.AND.IHARG(1).EQ.'VIOL'.AND.
     1       IHARG(2).EQ.'PLOT') .OR.
     1      (NUMARG.GE.4.AND.IHARG(1).EQ.'VIOL'.AND.
     1       IHARG(2).EQ.'TUFT'.AND.IHARG(3).EQ.'BOX'.AND.
     1       IHARG(4).EQ.'PLOT'))THEN
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
        GOTO2170
      ELSEIF(NUMARG.GE.4.AND.IHARG(1).EQ.'VIOL'.AND.
     1       IHARG(2).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')THEN
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
        GOTO2170
      ENDIF
      GOTO1101
C
 2170 CONTINUE
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO2175I=1,IMAX
          WRITE(ICOUT,2176)I,ICHAPA(I)(1:8)
 2176     FORMAT('CHARACTER ',I6,' HAS JUST BEEN SET TO ',A8)
          CALL DPWRST('XXX','BUG ')
 2175   CONTINUE
      ENDIF
      GOTO8000
C
 1101 CONTINUE
C
      IF(NUMARG.LE.0 .OR.
     1   (NUMARG.EQ.1.AND.IHARG(1).EQ.'ALL') .OR.
     1   (IHARG(NUMARG).EQ.'OFF') .OR.
     1   (IHARG(NUMARG).EQ.'DEFA'))THEN
        DO1165I=1,MAXCHA
          ICHAPA(I)='    '
 1165   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAPA(I)(1:8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO8000
      ELSEIF(IHARG(NUMARG).EQ.'ON' .OR. IHARG(NUMARG).EQ.'AUTO')THEN
        DO1115I=1,MAXCHA
          ICHAPA(I)='X'
 1115   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAPA(I)(1:24)
 1116     FORMAT('ALL CHARACTERS HAVE JUST BEEN SET TO ',A24)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO8000
C
      ELSEIF(NUMARG.EQ.1)THEN
        IF(NUMARG.EQ.0)ICHAPA(1)='    '
        IF(NUMARG.GE.1)THEN
          IF(IHARG(1).EQ.'BOX')THEN
            ICHAPA(1)='SQUA'
          ELSE
            ICHAPA(1)=' '
CCCCC       ICHAPA(1)(1:4)=IHARLC(1)
CCCCC       ICHAPA(1)(5:8)=IHARL2(1)
            ISTART=1
            ISTOP=IWIDTH
            IWORD=2
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO6001II=1,IWIDTH
              ISTRIN(II:II)=IANSLC(II)(1:1)
 6001       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1                  ICOL1,ICOL2,ISTRCH,NCCHAR,
     1                  IBUGP2,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(1)=' '
            ICHAPA(1)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          ENDIF
        ENDIF
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1126)I,ICHAPA(I)
 1126     FORMAT('CHARACTER ',I6,' HAS JUST BEEN SET TO ',A24)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO8000
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'ALL')THEN
        DO1135I=1,MAXCHA
          ICHAPA(I)=' '
          IF(IHARG(2).EQ.'BOX')THEN
            ICHAPA(I)='SQUA'
          ELSE
CCCCC       ICHAPA(I)(1:4)=IHARLC(2)
CCCCC       ICHAPA(I)(5:8)=IHARL2(2)
            ISTART=1
            ISTOP=IWIDTH
            IWORD=3
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO6003II=1,IWIDTH
              ISTRIN(II:II)=IANSLC(II)(1:1)
 6003       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1                  ICOL1,ICOL2,ISTRCH,NCCHAR,
     1                  IBUGP2,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(I)=' '
            ICHAPA(I)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          ENDIF
 1135   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAPA(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO8000
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'ALL')THEN
        DO1145I=1,MAXCHA
          ICHAPA(I)=' '
          IF(IHARG(1).EQ.'BOX')THEN
            ICHAPA(I)='SQUA'
          ELSE
CCCCC       ICHAPA(I)(1:4)=IHARLC(1)
CCCCC       ICHAPA(I)(5:8)=IHARL2(1)
            ISTART=1
            ISTOP=IWIDTH
            IWORD=2
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO6005II=1,IWIDTH
              ISTRIN(II:II)=IANSLC(II)(1:1)
 6005       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1                  ICOL1,ICOL2,ISTRCH,NCCHAR,
     1                  IBUGP2,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(I)=' '
            ICHAPA(I)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          ENDIF
 1145   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAPA(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO8000
      ELSEIF((NUMARG.GE.2.AND.IHARG(2).EQ.'SUBS'.AND.
     1        IHARG2(2).EQ.'ET  ') .OR.
     1       (NUMARG.GE.2.AND.IHARG(2).EQ.'EXCE'.AND.
     1        IHARG2(2).EQ.'PT  '))THEN
        ICASEQ='SUBS'
        GOTO4190
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'FOR '.AND.
     1       IHARG2(2).EQ.'    ')THEN
        ICASEQ='FOR'
        GOTO4190
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'AUTO')THEN
C
C               ***********************************************************
C               **  STEP 30--                                            **
C               **  TREAT THE    CHARACTERS AUTOMATIC <VARIABLE>   CASE  **
C               ***********************************************************
C
C       NOTE 2012/07: IF A "SET CHARACTER AUTOMATIC DYNAMIC ON" HAS BEEN
C                     ENTERED, JUST STORE THE VARIABLE NAME.
C
C               ********************************************
C               **  STEP 31--                             **
C               **  CHECK THE VALIDITY OF ARGUMENT 2 (OR 3)**
C               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
C               ********************************************
C
        ISTEPN='31'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
        CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
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
          GOTO9000
        ENDIF
C
        ICOLL=IVALUE(ILOCV)
        NLEFT=IN(ILOCV)
C
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')THEN
          WRITE(ICOUT,3090)ICOLL,NLEFT
 3090     FORMAT('CHARACTER AUTOMATIC: ICOLL,NLEFT = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               *****************************************
C               **  STEP 32--                          **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
        ISTEPN='32'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICASEQ='FULL'
        ILOCQ=NUMARG+1
        IF(NUMARG.LT.1)GOTO3290
        DO3200J=1,NUMARG
          J1=J
          IF((IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') .OR.
     1       (IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  '))THEN
            ICASEQ='SUBS'
            ILOCQ=J1
            GOTO3290
          ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
            ICASEQ='FOR'
            ILOCQ=J1
            GOTO3290
          ENDIF
 3200   CONTINUE
C
 3290   CONTINUE
        IF(IBUGP2.EQ.'OFF')THEN
          WRITE(ICOUT,3291)NUMARG,ILOCQ
 3291     FORMAT('NUMARG,ILOCQ = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               *********************************************
C               **  STEP 33--                              **
C               **  TEMPORARILY FORM THE VARIABLE Y(.)     **
C               **  WHICH WILL HOLD THE RESPONSE VARIABLE. **
C               **  FORM THIS VARIABLE BY                  **
C               **  BRANCHING TO THE APPROPRIATE SUBCASE   **
C               **  (FULL, SUBSET, OR FOR).                **
C               *********************************************
C
        ISTEPN='33'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(ICASEQ.EQ.'SUBS')THEN
          NIOLD=NLEFT
          CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
          NQ=NIOLD
        ELSEIF(ICASEQ.EQ.'FOR')THEN
          NIOLD=NLEFT
          CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1               NLOCAL,ILOCS,NS,IBUGQ,IERROR)
          NQ=NFOR
        ELSE
          DO3315I=1,NLEFT
            ISUB(I)=1
 3315     CONTINUE
          NQ=NLEFT
        ENDIF
C
        MINN2=1
        IF(NQ.LT.MINN2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3351)
 3351     FORMAT('***** ERROR IN DPCHAR--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3352)
 3352     FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',
     1           'EXTRACTED,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3353)IHLEFT,IHLEF2
 3353     FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING',
     1           'FROM VARIABLE ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3354)
 3354     FORMAT('      (FOR WHICH CHARACTER DEFINITIONS ARE TO BE ',
     1           'GENERATED)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3356)MINN2
 3356     FORMAT('      MUST BE ',I8,' OR LARGER;  SUCH WAS NOT THE ',
     1           'CASE HERE.')
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
          GOTO9000
        ENDIF
C
        MAXCP1=MAXCOL+1
        MAXCP2=MAXCOL+2
        MAXCP3=MAXCOL+3
        MAXCP4=MAXCOL+4
        MAXCP5=MAXCOL+5
        MAXCP6=MAXCOL+6
        J=0
        IMAX=NLEFT
        IF(NQ.LT.NLEFT)IMAX=NQ
        DO3370I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO3370
          J=J+1
C
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
C
          IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')THEN
            WRITE(ICOUT,3365)ISIGNF,J,Y(J),ICHAPA(J)
 3365       FORMAT('ISIGNF,J,Y(J),ICHAPA(J) = ',A4,2X,I5,G15.7,2X,A24)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 3370   CONTINUE
        NS=J
        NY=J
        IF(ISIGNF.EQ.'ON')GOTO8000
C
C               *****************************************
C               **  STEP 34--                          **
C               **  IF HAVE THE FORM--                 **
C               **  CHARACTERS AUTOMATIC DISTINCT X    **
C               **  EXTRACT THE DISTINCT VALUES        **
C               **  FROM THE TARGET VARIABLE Y(.)   .  **
C               **  STORE THEM IN X(.)   .             **
C               **  IF HAVE THE FORM--                 **
C               **  CHARACTERS AUTOMATIC X             **
C               **  DO NOTHING                         **
C               *****************************************
C
        ISTEPN='34'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IHARG(2).EQ.'DIST'.AND.IHARG2(2).EQ.'INCT')THEN
          IWRITE='OFF'
          CALL DISTIN(Y,NY,IWRITE,X,NX,IBUGP2,IERROR)
        ELSE
          DO3411I=1,NY
            X(I)=Y(I)
 3411     CONTINUE
          NX=NY
        ENDIF
C
C               ***********************************
C               **  STEP 35--                    **
C               **  SORT THESE DISTINCT VALUES   **
C               **  (IN PLACE).                  **
C               ***********************************
C
CCCCC   CALL SORT(X,NX,X)
C
C               ******************************************
C               **  STEP 36--                           **
C               **  CONVERT THE NUMERIC VALUES IN X(.)  **
C               **  TO CHARACTER STRINGS.               **
C               **  THEN LOAD THESE STRINGS             **
C               **  INTO INDIVIDUAL ELEMENTS            **
C               **  OF ICHAPA(.)                        **
C               **  NOTE--MAX CHARACTERS/STRING = 4     **
C               **        MAX NUMBER OF STRINGS = 100   **
C               ******************************************
C
        ISTEPN='36'
        IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IMAX=NX
        IF(IMAX+ICHAOF.GT.MAXCHA)IMAX=MAXCHA-ICHAOF
        DO3650I=1,IMAX
          ICHAPA(I+ICHAOF)=' '
          VAL=X(I)
          IVAL=INT(VAL+0.5)
          IF(VAL.LT.0.0)IVAL=INT(VAL-0.5)
          NUMDID=(-1)
          CALL DPCON2(IVAL,VAL,ICTEXT,NCTEXT,NUMDID,IBUGP2,IERROR)
          JMAX=NCTEXT
          IF(JMAX.GT.24)JMAX=24
          DO3660J=1,JMAX
            ICHAPA(I+ICHAOF)(J:J)=ICTEXT(J)(1:1)
 3660     CONTINUE
C
          IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')THEN
            WRITE(ICOUT,3665)I,ICHAOF,ICHAPA(I+ICHAOF)
 3665       FORMAT('I,ICHAOF,ICHAPA(I+ICHAOF) = ',2I6,2X,A24)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 3650   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          DO3675I=1,IMAX
            WRITE(ICOUT,3676)I+ICHAOF,ICHAPA(I+ICHAOF)
 3676       FORMAT('CHARACTER ',I6,' HAS JUST BEEN SET TO ',A24)
            CALL DPWRST('XXX','BUG ')
 3675     CONTINUE
        ENDIF
        GOTO8000
      ELSE
        IMAX=NUMARG
        IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
        DO1155I=1,IMAX
          ICHAPA(I)=' '
          IF(IHARG(I).EQ.'BOX')THEN
            ICHAPA(I)='SQUA'
          ELSE
CCCCC       ICHAPA(I)(1:4)=IHARLC(I)
CCCCC       ICHAPA(I)(5:8)=IHARL2(I)
            ISTART=1
            ISTOP=IWIDTH
            IWORD=I+1
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO6007II=1,IWIDTH
              ISTRIN(II:II)=IANSLC(II)(1:1)
 6007       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1                  ICOL1,ICOL2,ISTRCH,NCCHAR,
     1                  IBUGP2,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(I)=' '
            ICHAPA(I)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          ENDIF
 1155   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          DO1156I=1,IMAX
            WRITE(ICOUT,1126)I,ICHAPA(I)
            CALL DPWRST('XXX','BUG ')
 1156     CONTINUE
        ENDIF
        GOTO8000
      ENDIF
C
C               ***********************************************************
C               **  STEP 40--                                            **
C               **  TREAT THE CHARACTERS ... SUBSET/EXCEPT/FOR CASE      **
C               **  FOR REDEFINING SPECIFIED CHARACTERS                  **
C               ***********************************************************
C
C               *****************************************
C               **  STEP 41--                          **
C               **  DEFINE THE TYPE CASE--             **
C               **    1) SUBSET/EXCEPT                 **
C               **    2) FOR.                          **
C               *****************************************
C
 4190 CONTINUE
C
      ISTEPN='41'
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCQ=2
C
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')THEN
        WRITE(ICOUT,4191)ICASEQ,ILOCQ,NUMARG
 4191   FORMAT('ICASEQ,ILOCQ,NUMARG = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************
C               **  STEP 42--                              **
C               **  DETERMINE WHICH ELEMENTS ARE           **
C               **  TO BE REDEFINED.                       **
C               *********************************************
C
      ISTEPN='42'
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NQ=0
      IF(ICASEQ.EQ.'SUBS')THEN
        NIOLD=MAXCHA
        CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
        NQ=NIOLD
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        NIOLD=MAXCHA
        CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1             NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        NQ=NFOR
      ENDIF
C
      IF(NQ.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3351)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4252)
 4252   FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',
     1         'EXTRACTED,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4253)IHLEFT,IHLEF2
 4253   FORMAT('      EXTRACTED, NO CHARACTER ELEMENTS  ',
     1         'FROM VARIABLE ',A4,A4)
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
        GOTO9000
      ENDIF
C
C               *********************************************
C               **  STEP 43--                              **
C               **  REDEFINE THE DESIGNATED CHARACTERS.    **
C               *********************************************
C
      ISTEPN='43'
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IMAX=MAXCHA
      IF(NQ.LT.MAXCHA)IMAX=NQ
      DO4310I=1,IMAX
        IF(ISUB(I).EQ.0)GOTO4310
        ICHAPA(I)=' '
CCCCC   ICHAPA(I)(1:4)=IHARLC(1)
CCCCC   ICHAPA(I)(5:8)=IHARL2(1)
        ISTART=1
        ISTOP=IWIDTH
        IWORD=2
        NCCHAR=0
        ISTRIN=' '
        ISTRCH=' '
        DO6008II=1,IWIDTH
          ISTRIN(II:II)=IANSLC(II)(1:1)
 6008   CONTINUE
        CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1              ICOL1,ICOL2,ISTRCH,NCCHAR,
     1              IBUGP2,ISUBRO,IERROR)
        IF(NCCHAR.GT.24)NCCHAR=24
        ICHAPA(I)=' '
        ICHAPA(I)(1:NCCHAR)=ISTRCH(1:NCCHAR)
 4310 CONTINUE
C
C               *********************************************
C               **  STEP 44--                              **
C               **  IF CALLED FOR,                         **
C               **  PRINT OUT A MESSAGE.                   **
C               *********************************************
C
      ISTEPN='44'
      IF(IBUGP2.EQ.'ON'.OR.ISUBRO.EQ.'CHAR')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEEDB.EQ.'OFF')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO4410I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO4410
          WRITE(ICOUT,4411)I,ICHAPA(I)
 4411     FORMAT('CHARACTER ',I6,' HAS JUST BEEN SET TO ',A24)
          CALL DPWRST('XXX','BUG ')
 4410   CONTINUE
      ENDIF
      GOTO8000
C
 8000 CONTINUE
      IFOUND='YES'
      DO8010I=1,MAXCHA
        IF(ICHAPA(I)(1:4).EQ.'BLAN')ICHAPA(I)='BLAN'
        IF(ICHAPA(I)(1:4).EQ.'blan')ICHAPA(I)='BLAN'
        IF(ICHAPA(I)(1:4).EQ.'NONE')ICHAPA(I)='BLAN'
        IF(ICHAPA(I).EQ.'BL')ICHAPA(I)='BLAN'
        IF(ICHAPA(I).EQ.'bl')ICHAPA(I)='BLAN'
        IF(ICHAPA(I).EQ.'NO')ICHAPA(I)='BLAN'
 8010 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IHYPSW=IHYPSV
C
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
          DO9023I=1,NY
            WRITE(ICOUT,9024)I,Y(I)
 9024       FORMAT('I,Y(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
 9023     CONTINUE
        ENDIF
        IF(NX.GT.0)THEN
          DO9033I=1,NX
            WRITE(ICOUT,9034)I,X(I),ICHAPA(I)
 9034       FORMAT('I,X(I),ICHAPA(I) = ',I8,G15.7,2X,A24)
            CALL DPWRST('XXX','BUG ')
 9033     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCHCA(IHARG,NUMARG,IDEFCA,MAXCHA,ICHACA,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER CASES FOR USE IN MULTI-TRACE PLOTS.
C              THE CASE FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE HOLLERITH
C              VECTOR ICHACA(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFCA
C                     --MAXCHA
C     OUTPUT ARGUMENTS--ICHACA  (A  HOLLERITH VECTOR
C                       WHOSE I-TH ELEMENT IS THE CASE
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
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
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCA
      CHARACTER*4 ICHACA
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION ICHACA(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'CASE')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CASE')GOTO1105
      GOTO1199
C
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GOTO1140
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      ICHACA(I)=IDEFCA
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHACA(I)
 1116 FORMAT('ALL CHARACTER CASES HAVE JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO1190
C
 1120 CONTINUE
      ICHACA(1)=IHARG(2)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,ICHACA(I)
 1126 FORMAT('THE CASE FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO1190
C
 1130 CONTINUE
      DO1135I=1,MAXCHA
      ICHACA(I)=IHARG(3)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHACA(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO1190
C
 1140 CONTINUE
      DO1145I=1,MAXCHA
      ICHACA(I)=IHARG(2)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHACA(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO1190
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO1155I=1,IMAX
      IP1=I+1
      ICHACA(I)=IHARG(IP1)
 1155 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1156I=1,IMAX
      WRITE(ICOUT,1126)I,ICHACA(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      ICHACA(I)=IDEFCA
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHACA(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO1190
C
 1190 CONTINUE
      IFOUND='YES'
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHCL(IHARG,IARG,NUMARG,IDEFCO,MAXCHA,ICHACO,
     1                  ICHAC2,IRGBMX,ICASCL,
     1                  IBUGP2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER COLORS FOR USE IN MULTI-TRACE PLOTS.
C              THE COLOR FOR THE CHARACTER FOR THE I-TH TRACE WILL BE
C              PLACED IN THE I-TH ELEMENT OF THE HOLLERITH
C              VECTOR ICHACO(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFCO
C                     --MAXCHA
C     OUTPUT ARGUMENTS--ICHACO  (A  HOLLERITH VECTOR WHOSE I-TH ELEMENT
C                       IS THE COLOR FOR THE CHARACTER ASSIGNED TO THE
C                       I-TH  TRACE IN A MULTI-TRACE PLOT.
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCO
      CHARACTER*4 ICHACO
      CHARACTER*4 ICASCL
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARG(*)
      DIMENSION ICHACO(*)
      DIMENSION ICHAC2(MAXCHA,3)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPCH'
      ISUBN2='CL  '
      IHOLD1='-999'
      IHOLD2='-999'
      NUMCHA=MAXCHA
C
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
        DO65I=1,NUMARG
          WRITE(ICOUT,66)I,IARG(I),IHARG(I)
   66     FORMAT('I,IARG(I),IHARG(I) = ',2I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO75I=1,10
          WRITE(ICOUT,76)I,ICHACO(I),ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3)
   76     FORMAT('I,ICHACO(I),ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3) = ',
     1           I8,2X,A4,2X,3I5)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
C
      IF(NUMARG.EQ.0)GOTO9000
      IF(ICASCL.EQ.'RGB ')GOTO2000
C
C     THIS IS THE "STANDARD" CASE
C
      IF((NUMARG.EQ.1.AND.IHARG(1).EQ.'COLO') .OR.
     1   (NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL'))THEN
        DO1165I=1,MAXCHA
          ICHACO(I)=IDEFCO
 1165   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHACO(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'COLO')THEN
        IF(IHARG(NUMARG).EQ.'ON' .OR. IHARG(NUMARG).EQ.'OFF' .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
          DO1115I=1,MAXCHA
            ICHACO(I)=IDEFCO
 1115     CONTINUE
C
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
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I,ICHACO(I)
 1126       FORMAT('THE COLOR FOR CHARACTER ',I6,' HAS JUST BEEN SET ',
     1             'TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')THEN
          DO1135I=1,MAXCHA
            ICHACO(I)=IHARG(3)
 1135     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHACO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ELSEIF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')THEN
          DO1145I=1,MAXCHA
            ICHACO(I)=IHARG(2)
 1145     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHACO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ELSE
          IMAX=NUMARG-1
          IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
          DO1155I=1,IMAX
            IP1=I+1
            ICHACO(I)=IHARG(IP1)
 1155     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO1156I=1,IMAX
              WRITE(ICOUT,1126)I,ICHACO(I)
              CALL DPWRST('XXX','BUG ')
 1156       CONTINUE
          ENDIF
        ENDIF
      ELSE
        GOTO9000
      ENDIF
C
      IFOUND='YES'
      GOTO9000
C
C     RGB COLORS CASE: 3 COLORS SHOULD BE GIVEN
C
C                      CHARACTER COLOR
C                      CHARACTER COLOR IRED IBLUE IGREEN
C                      CHARACTER COLOR IRED IBLUE IGREEN ALL
C                      CHARACTER COLOR ALL IRED IBLUE IGREEN
C                      CHARACTER COLOR IRED1 IBLUE1 IGREEN1 IRED2 ...
C
C                      THE "RGB" KEYWORD HAS ALREADY BEEN STRIPPED
C                      OUT.  NOTE THAT THE DEFAULT COLOR IS -1
C                      (I.E., NO RGB COLOR VALUES SPECIFIED).
C
 2000 CONTINUE
C
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHCL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JHOLD1=-1
      JHOLD2=-1
      JHOLD3=-1
      NUMCHA=MAXCHA
C
      IF(NUMARG.EQ.2 .AND. IHARG(2).EQ.'ALL')THEN
        GOTO2300
      ELSEIF(NUMARG.EQ.5)THEN
        IF(IHARG(2).EQ.'ALL')THEN
          JHOLD1=IARG(3)
          JHOLD2=IARG(4)
          JHOLD3=IARG(5)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD1=-1
          GOTO2300
        ELSEIF(IHARG(5).EQ.'ALL')THEN
          JHOLD1=IARG(2)
          JHOLD2=IARG(3)
          JHOLD3=IARG(4)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD1=-1
          GOTO2300
        ENDIF
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
C               *************************************************
C
      ISTEPN='2A'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHCL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
        DO2225I=1,NUMCHA
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
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO2278I=1,NUMCHA
          WRITE(ICOUT,2276)I,ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3)
 2276     FORMAT('THE RGB COLORS OF CHARACTER ',I6,
     1           ' HAVE JUST BEEN SET TO ',3I8)
          CALL DPWRST('XXX','BUG ')
 2278   CONTINUE
      ENDIF
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 2300 CONTINUE
      ISTEPN='2B'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'CHCL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2315I=1,NUMCHA
        ICHAC2(I,1)=JHOLD1
        ICHAC2(I,2)=JHOLD2
        ICHAC2(I,3)=JHOLD3
 2315 CONTINUE
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,2316)ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3)
 2316   FORMAT('THE RGB COLORS OF ALL CHARACTERS HAVE JUST ',
     1         'BEEN SET TO ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
        DO9075I=1,10
          WRITE(ICOUT,76)I,ICHACO(I),ICHAC2(I,1),ICHAC2(I,2),ICHAC2(I,3)
          CALL DPWRST('XXX','BUG ')
 9075   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCHEC(K,IHOL,IHOL2,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1INT1,FLOAT1,IBUGA3,IERROR)
C
C     PURPOSE--EXAMINE COMPONENT K OF IHOL(.) AND IHOL2(.).
C     IF IT IS A PARAMETER NAME, DETERMINE THE VALUE
C     OF THE PARAMETER AND PLACE THIS VALUE
C     IN INT1(K) AND FLOAT1(K).
C     IF OTHERWISE, DO NOTHING.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY   1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
C---------------------------------------------------------------------
C
      DIMENSION IHOL(*)
      DIMENSION IHOL2(*)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
      DIMENSION INT1(*)
      DIMENSION FLOAT1(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF')GOTO90
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
      DO55I=1,NUMNAM
      WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)
   56 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,57)K,INT1(K),FLOAT1(K)
   57 FORMAT('K,INT1(K),FLOAT1(K) = ',I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      IH=IHOL(K)
      IH2=IHOL2(K)
      IF(NUMNAM.LE.0)GOTO2799
      DO2795I=1,NUMNAM
      IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1IUSE(I).EQ.'P')GOTO2796
      GOTO2795
 2796 CONTINUE
      INT1(K)=IVALUE(I)
      FLOAT1(K)=VALUE(I)
      GOTO2799
 2795 CONTINUE
 2799 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
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
      DO9015I=1,NUMNAM
      WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)
 9016 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9017)K,INT1(K),FLOAT1(K)
 9017 FORMAT('K,INT1(K),FLOAT1(K) = ',I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPCHEX(ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2)
C
C     PURPOSE--CHARACTER EXTRACTION--
C              GIVEN A CHARACTER STRING IN A WORD (IX1),
C              MOVE THE BIT STRING WHICH STARTS IN BIT ISTAR1
C              (ISTAR1 RANGES FROM 0 TO 35 IN A UNIVAC 1108,
C                                  0 TO 31 IN AN IBM 3033,
C                                  0 TO 59 IN A CDC 7600, ETC.
C              AND IS OF LENGTH ILEN1 BITS)
C              INTO BITS STARTING AT ISTAR2 OF LENGTH ILEN2
C              (HERE ILEN2 USUALLY = ILEN1) IN THE WORD IX2.
C              OUTPUT THE NEW CHARACTER VARIABLE (IX2).
C     NOTE--0 DENOTES THE LEFT-MOST (THAT IS, THE HIGH-ORDER) BIT.
C     NOTE--ISTAR1 AND ISTAR2 RANGE FROM 0 TO NUMBPW-1
C           THAT IS, FROM 0 TO ONE LESS THAN THE TOTLA NUMBER OF BITS PER WORD.
C           (FOR EXAMPLE, ON UNIVAC 1100/82--FROM 0 TO 35
C                         ON VAX    11/780 --FROM 0 TO 31)
C     NOTE--IX1 AND IX2 ARE CHARACTER*4 VARIABLES.
C     NOTE--THIS SUBROUTINE HAS BEEN CONSTRAINED SO THAT
C           NEITHER ILEN1 NOR ILEN2 ARE EXPLICITELY USED.
C           THIS SUBROUTINE, AS CODED, OPERATES ON THE ASSUMPTIONS THAT
C              1) ILEN1 = NUMBPC (THAT IS, THE LENGTH
C                 OF THE BIT STRING BEING MOVED IS IDENTICAL
C                 TO THE NUMBER OF BITS PER CHARACTER ON
C                 YOUR COMPUTER).
C              2) ILEN2 = ILEN1 (THAT IS, THE LENGTH OF THE OUTPUT STRING =
C                 THE LENGTH OF THE INPUT STRING),
C              3) ISTAR1 IS SUCH THAT THE START OF THE BIT STRING
C                 IS ALWAYS AT THE BEGINNING OF A CHARACTER
C           THE NET RESULT IS THAT THIS SUBROUTINE, AS CODED,
C           EXTRACTS EXACTLY 1 CHARACTER AND
C           MOVES IT TO THE POSITION OF ANOTHER CHARACTER.
C           THESE CONSTRAINTS WILL BE ACCEPTABLE FOR ALL USES
C           OF THIS SUBROUTINE BY ANY OTHER DATAPLOT SUBROUTINE.
C     NOTE--THE VALUES FOR NUMBPC (NUMBER OF BITS PER CHARACTER)
C           AND NUMBPW (NUMBER OF BITS PER WORD) ARE SET
C           FOR YOUR COMPUTER IN DATAPLOT SUBROUTINE INITMC.
C     NOTE--ALGORITHM PROVIDED BY MICHAEL VOGT
C                                 INFORMATION TECHNOLOGY LABORATORY
C                                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--OCTOBER  1978.
C     UPDATED         --JUNE      1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IX1
      CHARACTER*4 IX2
C
C---------------------------------------------------------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C               ********************************************************
C               **  THE FOLLOWING CODE WILL CARRY OUT                 **
C               **  THE CHARACTER EXTRACTION FOR ALL COMPUTERS        **
C               **  WITH AN ANSI 77 FORTRAN COMPILER--IT MAKES        **
C               **  USE OF THE ANSI FORTRAN 77 CONSTRUCT--            **
C               **  IY(IC:ID)=IX(IA:IB)                               **
C               **  WHERE IX AND IY ARE CHARACTER*4 VARIABLES,        **
C               **  WHERE IA, IB, IC, AND ID ARE INTEGER VARIABLES,   **
C               **  AND WHERE IY(IC:ID)=IX(IA:IB) MEANS               **
C               **  TO COPY CHARACTERS IA THROUGH IB OF VARIABLE IX AND
C               **  PLACE THEM INTO CHARACTERS IC THROUGH ID OF VARIABLE IY.
C               **  WITH ALL OTHER CHARACTERS IN IY BEING UNAFFECTED. **
C               **  USUALLY IA, IB, IC, AND ID RANGE FROM 1 TO 4.     **
C               ********************************************************
C
      IF(ISUBG4.EQ.'CHEX')THEN
        WRITE(ICOUT,51)ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2
   51   FORMAT('FROM DPCHEX: ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2 = ',6I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IBYTE1=(ISTAR1+NUMBPC)/NUMBPC
      IBYTE2=(ISTAR2+NUMBPC)/NUMBPC
      IX2(IBYTE2:IBYTE2)=IX1(IBYTE1:IBYTE1)
      GOTO9000
C
C               ****************************************************************
C               **  CHARACTER EXTRACTION FOR THE UNIVAC 1100 SERIES. FOR COMPILE
C               **  (FORTRAN 1966 COMPILER)
C               ****************************************************************
C
CCCCC ISTAR1=IABS(ISTAR1)
CCCCC ISTAR2=IABS(ISTAR2)
C
CCCCC FLD(ISTAR2,ILEN2,IX2)=FLD(ISTAR1,ILEN1,IX1)
C
C               ****************************************************************
C               **  CHARACTER EXTRACTION FOR THE UNIVAC 1100 SERIES. FTN COMPILE
C               **  (FORTRAN 1977 COMPILER)
C               ****************************************************************
C
CCCCC ISTR1P=ISTAR1+1
CCCCC ISTR2P=ISTAR2+1
C
CCCCC BITS(IX2,ISTR2P,ILEN2)=BITS(IX1,ISTR1P,ILEN1)
C
C               ***********************************************
C               **  CHARACTER EXTRACTION FOR THE VAX-11/780  **
C               **  (FORTRAN 1966 COMPILER)
C               ***********************************************
C
CCCCC LOGICAL*1 IX1(4)
CCCCC LOGICAL*1 IX2(4)
C
CCCCC I1=(ISTAR1+8)/8
CCCCC I2=(ISTAR2+8)/8
CCCCC IX2(I2)=IX1(I1)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHFI(IHARG,NUMARG,IDEFFI,MAXCHA,ICHAFI,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER FILL SWITCH FOR USE IN MULTI-TRACE PLOTS.
C              THE FILL SWITCH FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE HOLLERITH
C              VECTOR ICHAFI(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFFI
C                     --MAXCHA
C     OUTPUT ARGUMENTS--ICHAFI  (A  HOLLERITH VECTOR
C                       WHOSE I-TH ELEMENT IS THE FILL SWITCH
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1998. CHECK FOR CHARCTER FILL COLOR
C                                       (SKIP IF ABOVE FOUND)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFI
      CHARACTER*4 ICHAFI
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION ICHAFI(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'FILL')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'FILL')GOTO1105
      GOTO1199
C
 1105 CONTINUE
CCCCC IF(IHARG(NUMARG).EQ.'ON')GOTO1110
CCCCC IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
CCCCC ADD FOLLOWING LINE  JUNE 1998
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1199
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GOTO1140
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      ICHAFI(I)=IDEFFI
      IF(IHARG(NUMARG).EQ.'ON')ICHAFI(I)='ON'
      IF(IHARG(NUMARG).EQ.'OFF')ICHAFI(I)='OFF'
      IF(IHARG(NUMARG).EQ.'AUTO')ICHAFI(I)='ON'
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAFI(I)
 1116 FORMAT('ALL CHARACTER FILL SWITCHES HAVE JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO1190
C
 1120 CONTINUE
      ICHAFI(1)=IHARG(2)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,ICHAFI(I)
 1126 FORMAT('THE FILL SWITCH FOR CHARACTER ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO1190
C
 1130 CONTINUE
      DO1135I=1,MAXCHA
      ICHAFI(I)=IHARG(3)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAFI(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO1190
C
 1140 CONTINUE
      DO1145I=1,MAXCHA
      ICHAFI(I)=IHARG(2)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAFI(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO1190
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO1155I=1,IMAX
      IP1=I+1
      ICHAFI(I)=IHARG(IP1)
 1155 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1156I=1,IMAX
      WRITE(ICOUT,1126)I,ICHAFI(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      ICHAFI(I)=IDEFFI
      IF(IHARG(1).EQ.'ON')ICHAFI(I)='ON'
      IF(IHARG(1).EQ.'OFF')ICHAFI(I)='OFF'
      IF(IHARG(1).EQ.'AUTO')ICHAFI(I)='ON'
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'FILL')ICHAFI(I)='ON'
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAFI(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO1190
C
 1190 CONTINUE
      IFOUND='YES'
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHFO(IHARG,NUMARG,IDEFFO,MAXCHA,ICHAFO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER FONTS FOR USE IN MULTI-TRACE PLOTS.
C              THE FONT FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE HOLLERITH
C              VECTOR ICHAFO(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFFO
C                     --MAXCHA
C     OUTPUT ARGUMENTS--ICHAFO  (A  HOLLERITH VECTOR
C                       WHOSE I-TH ELEMENT IS THE FONT
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
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
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFO
      CHARACTER*4 ICHAFO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION ICHAFO(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF((NUMARG.EQ.1.AND.IHARG(1).EQ.'FONT') .OR.
     1   (NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL'))THEN
        DO1165I=1,MAXCHA
          ICHAFO(I)=IDEFFO
 1165   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHAFO(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'FONT')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
          DO1115I=1,MAXCHA
            ICHAFO(I)=IDEFFO
 1115     CONTINUE
C
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
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I,ICHAFO(I)
 1126       FORMAT('THE FONT FOR CHARACTER ',I6,
     1             ' HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')THEN
          DO1135I=1,MAXCHA
            ICHAFO(I)=IHARG(3)
 1135     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHAFO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')THEN
          DO1145I=1,MAXCHA
            ICHAFO(I)=IHARG(2)
 1145     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHAFO(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ELSE
          IMAX=NUMARG-1
          IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
          DO1155I=1,IMAX
            IP1=I+1
            ICHAFO(I)=IHARG(IP1)
 1155     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO1156I=1,IMAX
              WRITE(ICOUT,1126)I,ICHAFO(I)
              CALL DPWRST('XXX','BUG ')
 1156       CONTINUE
          ENDIF
        ENDIF
      ENDIF
C
      IFOUND='YES'
C
      RETURN
      END
      SUBROUTINE DPCHGR(ICHAR2,ICHARN,IBUG,IFOUND)
C
C     PURPOSE--NUMERICALLY CONVERT A GREEK ALPHABETIC CHARACTER.
C              CONVERT A PACKED ALPHABETIC STRING
C              (PACKED INTO 1 COMPUTER WORD
C              WITH ONLY THE FIRST 4 CHARACTERS BEING SIGNIFICANT)
C              (ALPH... TO OMEG...) INTO A NUMERIC VALUE
C              (1 TO 24).
C     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
C                              CONTAINING THE HOLLERITH
C                              CHARACTER(S) OF INTEREST.
C     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
C                              CONTAINING THE NUMERIC
C                              DESIGNATION FOR THE
C                              ALPHABETIC CHARACTER.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH     1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUG
      CHARACTER*4 IFOUND
C
C-----COMMON VARIABLES (BUGS & ERROR)---------------------------------
C
      CHARACTER*4 IBUGG4
      CHARACTER*4 ISUBG4
      CHARACTER*4 IERRG4
C
      COMMON /ICOMBE/IBUGG4,ISUBG4,IERRG4
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
C
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
C
C               **********************************
C               **  STEP 1--                    **
C               **  CONVERT THE CHARACTER       **
C               **********************************
C
      IF(ICHAR2.EQ.'ALPH')GOTO100
      IF(ICHAR2.EQ.'BETA')GOTO200
      IF(ICHAR2.EQ.'GAMM')GOTO300
      IF(ICHAR2.EQ.'DELT')GOTO400
      IF(ICHAR2.EQ.'EPSI')GOTO500
      IF(ICHAR2.EQ.'ZETA')GOTO600
      IF(ICHAR2.EQ.'ETA')GOTO700
      IF(ICHAR2.EQ.'THET')GOTO800
      IF(ICHAR2.EQ.'IOTA')GOTO900
      IF(ICHAR2.EQ.'KAPP')GOTO1000
      IF(ICHAR2.EQ.'LAMB')GOTO1100
      IF(ICHAR2.EQ.'MU')GOTO1200
      IF(ICHAR2.EQ.'NU')GOTO1300
      IF(ICHAR2.EQ.'XI')GOTO1400
      IF(ICHAR2.EQ.'OMIC')GOTO1500
      IF(ICHAR2.EQ.'PI')GOTO1600
      IF(ICHAR2.EQ.'RHO')GOTO1700
      IF(ICHAR2.EQ.'SIGM')GOTO1800
      IF(ICHAR2.EQ.'TAU')GOTO1900
      IF(ICHAR2.EQ.'UPSI')GOTO2000
      IF(ICHAR2.EQ.'PHI')GOTO2100
      IF(ICHAR2.EQ.'CHI')GOTO2200
      IF(ICHAR2.EQ.'PSI')GOTO2300
      IF(ICHAR2.EQ.'OMEG')GOTO2400
      GOTO7900
C
  100 CONTINUE
      ICHARN=1
      GOTO8000
C
  200 CONTINUE
      ICHARN=2
      GOTO8000
C
  300 CONTINUE
      ICHARN=3
      GOTO8000
C
  400 CONTINUE
      ICHARN=4
      GOTO8000
C
  500 CONTINUE
      ICHARN=5
      GOTO8000
C
  600 CONTINUE
      ICHARN=6
      GOTO8000
C
  700 CONTINUE
      ICHARN=7
      GOTO8000
C
  800 CONTINUE
      ICHARN=8
      GOTO8000
C
  900 CONTINUE
      ICHARN=9
      GOTO8000
C
 1000 CONTINUE
      ICHARN=10
      GOTO8000
C
 1100 CONTINUE
      ICHARN=11
      GOTO8000
C
 1200 CONTINUE
      ICHARN=12
      GOTO8000
C
 1300 CONTINUE
      ICHARN=13
      GOTO8000
C
 1400 CONTINUE
      ICHARN=14
      GOTO8000
C
 1500 CONTINUE
      ICHARN=15
      GOTO8000
C
 1600 CONTINUE
      ICHARN=16
      GOTO8000
C
 1700 CONTINUE
      ICHARN=17
      GOTO8000
C
 1800 CONTINUE
      ICHARN=18
      GOTO8000
C
 1900 CONTINUE
      ICHARN=19
      GOTO8000
C
 2000 CONTINUE
      ICHARN=20
      GOTO8000
C
 2100 CONTINUE
      ICHARN=21
      GOTO8000
C
 2200 CONTINUE
      ICHARN=22
      GOTO8000
C
 2300 CONTINUE
      ICHARN=23
      GOTO8000
C
 2400 CONTINUE
      ICHARN=24
      GOTO8000
C
 7900 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7911)
C7911 FORMAT('***** ERROR IN DPCHNU--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7912)
C7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7913)ICHAR2
C7913 FORMAT('      INPUT CHAR2ACTER = ',A4)
CCCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GOTO9000
C
 8000 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCHHW(IHARG,IARGT,ARG,NUMARG,
     1                  MAXCHA,
     1                  PCHAHE,PCHAWI,PDEFHE,PDEFWI,
     1                  IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER HEIGHT AND WIDTH
C              FOR USE IN MULTI-TRACE PLOTS.
C              THE HEIGHT AND WIDTH FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE FLOATING POINT
C              VECTORS PCHAHE(.) AND PCHAWI(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --MAXCHA
C     OUTPUT ARGUMENTS--PCHAHE  (A  FLOATING POINT VECTOR
C                       WHOSE I-TH ELEMENT IS THE HEIGHT
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --PCHAWI  (A  FLOATING POINT VECTOR
C                       WHOSE I-TH ELEMENT IS THE WIDTH
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --PDEFHE  = DEFAULT CHARACTER HEIGHT
C                     --PDEFWI  = DEFAULT CHARACTER WIDTH
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/8
C     ORIGINAL VERSION--AUGUST    1988.
C     UPDATED         --JANUARY   1995. ALLOW ? AS ARGUMENT (FOR HELP)
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
      DIMENSION PCHAHE(*)
      DIMENSION PCHAWI(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'HW')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'HW')GOTO1105
      GOTO9000
C
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
      IF(IHARG(NUMARG).EQ.'?')GOTO1200
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.3)GOTO1120
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'ALL')GOTO1140
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      PCHAWI(I)=PDEFHE
      PCHAHE(I)=PDEFWI
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)
 1116 FORMAT('THE HEIGHTS AND WIDTHS OF ALL CHARACTERS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1117)PCHAHE(I),PCHAWI(I)
 1117 FORMAT('    HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO2190
C
 1120 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB'.OR.IARGT(3).NE.'NUMB')GOTO1180
      PCHAHE(1)=ARG(2)
      PCHAWI(1)=ARG(3)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I
 1126 FORMAT('THE HEIGHT AND WIDTH OF CHARACTER ',I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)PCHAHE(I),PCHAWI(I)
 1127 FORMAT('    HAS JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO2190
C
 1130 CONTINUE
      I=1
      IF(IARGT(3).NE.'NUMB'.OR.IARGT(4).NE.'NUMB')GOTO1180
      DO1135I=1,MAXCHA
      PCHAHE(I)=ARG(3)
      PCHAWI(I)=ARG(4)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1117)PCHAHE(I),PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO2190
C
 1140 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB'.OR.IARGT(3).NE.'NUMB')GOTO1180
      DO1145I=1,MAXCHA
      PCHAHE(I)=ARG(2)
      PCHAWI(I)=ARG(3)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1117)PCHAHE(I),PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO2190
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      J=0
      DO1155I=1,IMAX,2
      IP1=I+1
      IP2=I+2
      IF(IARGT(IP1).NE.'NUMB')GOTO1180
      IF(IARGT(IP2).NE.'NUMB')GOTO1180
      J=J+1
      PCHAHE(J)=ARG(IP1)
      PCHAWI(J)=ARG(IP2)
 1155 CONTINUE
      JMAX=J
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1156I=1,JMAX
      WRITE(ICOUT,1126)I
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)PCHAHE(I),PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO2190
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      PCHAHE(I)=PDEFHE
      PCHAWI(I)=PDEFWI
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1117)PCHAHE(I),PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO2190
C
 1180 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DPCHHW--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('THE HEIGHTS AND WIDTHS OF CHARACTERS MUST BE NUMERIC')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('HOWEVER, THE SPECIFIED CHARACTER HEIGHT AND WIDTH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)I
 1184 FORMAT('FOR CHARACTER ',I6,' WAS NON-NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
CCCCC THE FOLLOWING SECTION WAS ADDED    JANUARY 1995
 1200 CONTINUE
      IFOUND='YES'
      IF(IFEEDB.EQ.'OFF')GOTO1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1221)I,PCHAHE(I)
 1221 FORMAT('THE CURRENT HEIGHT FOR CHARACTER ',I6,' IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1222)I,PCHAWI(I)
 1222 FORMAT('THE CURRENT WIDTH  FOR CHARACTER ',I6,' IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1223)I,PDEFHE
 1223 FORMAT('THE DEFAULT HEIGHT FOR CHARACTER ',I6,' IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1224)I,PDEFWI
 1224 FORMAT('THE DEFAULT WIDTH  FOR CHARACTER ',I6,' IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GOTO9000
C
 2190 CONTINUE
      IFOUND='YES'
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHJU(IHARG,NUMARG,MAXCHA,ICHAJU,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER JUSTIFICATION FOR USE IN MULTI-TRACE PLOTS.
C              THE JUSTIFICATION FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE HOLLERITH
C              VECTOR ICHAJU(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --MAXCHA
C     OUTPUT ARGUMENTS--ICHAJU  (A  HOLLERITH VECTOR
C                       WHOSE I-TH ELEMENT IS THE JUSTIFICATION
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1986.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 ICHAJU
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION ICHAJU(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'JUST')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'JUST')GOTO1105
      GOTO1199
C
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GOTO1140
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      ICHAJU(I)='CENT'
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAJU(I)
 1116 FORMAT('ALL CHARACTER JUSTIFICATIONS HAVE JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO1190
C
 1120 CONTINUE
      ICHAJU(1)=IHARG(2)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,ICHAJU(I)
 1126 FORMAT('THE JUSTIFICATION FOR CHARACTER ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO1190
C
 1130 CONTINUE
      DO1135I=1,MAXCHA
      ICHAJU(I)=IHARG(3)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAJU(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO1190
C
 1140 CONTINUE
      DO1145I=1,MAXCHA
      ICHAJU(I)=IHARG(2)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAJU(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO1190
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO1155I=1,IMAX
      IP1=I+1
      ICHAJU(I)=IHARG(IP1)
 1155 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1156I=1,IMAX
      WRITE(ICOUT,1126)I,ICHAJU(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      ICHAJU(I)='CENT'
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)ICHAJU(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO1190
C
 1190 CONTINUE
      IFOUND='YES'
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHLI(ICONT,NUMCPL,YSTART,YSTOP,XSTART,XSTOP,
     1J,JD,Y2,X2,D2,IERROR)
C
C     PURPOSE--GENERATE PLOT COORDINATES FOR A POINT
C              OR FOR A LINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY   1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICONT
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      NUMCP2=NUMCPL
      IF(ICONT.EQ.'ON')NUMCP2=2
      ANUMC2=NUMCP2
C
      IF(YSTART.EQ.YSTOP)GOTO200
      IF(XSTART.EQ.XSTOP)GOTO1300
      GOTO1400
C
  200 CONTINUE
      IF(XSTART.EQ.XSTOP)GOTO1100
      GOTO1200
C
C               ***************************
C               **  STEP 2.1--           **
C               **  TREAT THE CASE WHEN  **
C               **  Y HAS NO CHANGE      **
C               **  X HAS NO CHANGE      **
C               ***************************
C
 1100 CONTINUE
      J=J+1
      JD=JD+1
      Y2(J)=YSTART
      X2(J)=XSTART
      D2(J)=JD
      GOTO9000
C
C               ***************************
C               **  STEP 2.2--           **
C               **  TREAT THE CASE WHEN  **
C               **  Y HAS NO CHANGE      **
C               **  X HAS    CHANGE      **
C               ***************************
C
 1200 CONTINUE
      JD=JD+1
      XDEL=XSTOP-XSTART
      DO1210I=1,NUMCP2
      J=J+1
      AI=I
      P=(AI-1.0)/(ANUMC2-1.0)
      XP=XSTART+P*XDEL
      Y2(J)=YSTART
      X2(J)=XP
      D2(J)=JD
 1210 CONTINUE
      GOTO9000
C
C               ***************************
C               **  STEP 2.3--           **
C               **  TREAT THE CASE WHEN  **
C               **  Y HAS    CHANGE      **
C               **  X HAS NO CHANGE      **
C               ***************************
C
 1300 CONTINUE
      JD=JD+1
      YDEL=YSTOP-YSTART
      DO1310I=1,NUMCP2
      J=J+1
      AI=I
      P=(AI-1.0)/(ANUMC2-1.0)
      YP=YSTART+P*YDEL
      Y2(J)=YP
      X2(J)=XSTART
      D2(J)=JD
 1310 CONTINUE
      GOTO9000
C
C               ***************************
C               **  STEP 2.4--           **
C               **  TREAT THE CASE WHEN  **
C               **  Y HAS    CHANGE      **
C               **  X HAS    CHANGE      **
C               ***************************
C
 1400 CONTINUE
      JD=JD+1
      XDEL=XSTOP-XSTART
      YDEL=YSTOP-YSTART
      DO1410I=1,NUMCP2
      J=J+1
      AI=I
      P=(AI-1.0)/(ANUMC2-1.0)
      XP=XSTART+P*XDEL
      YP=YSTART+P*YDEL
      Y2(J)=YP
      X2(J)=XP
      D2(J)=JD
 1410 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUG,IERROR)
C
C     PURPOSE--CHECK FOR A LEFT AND RIGHT PARENTHESIS.
C              CHECK FOR A LEFT  PARENTHESIS IN LOCATION ILOCLP.
C              CHECK FOR A RIGHT PARENTHESIS IN LOCATION ILOCRP.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISTRIN
      CHARACTER*4 IFOULR
      CHARACTER*4 IBUG
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION ISTRIN(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOULR='NO'
      IERROR='NO'
C
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
C
      IF(ILOCLP.LT.1)GOTO1200
      IF(ILOCLP.GT.NUMCHS)GOTO1200
C
      IF(ILOCRP.LT.1)GOTO1200
      IF(ILOCRP.GT.NUMCHS)GOTO1200
C
      IF(ISTRIN(ILOCLP).NE.'(')GOTO1200
      IF(ISTRIN(ILOCRP).NE.')')GOTO1200
C
      IFOULR='YES'
      GOTO9000
C
 1200 CONTINUE
      IFOULR='NO'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCHMA(ICHAR2,ICHARN,IBUG,IFOUND)
C
C     PURPOSE--CONVERT A MATHEMATICAL SYMBOL
C              INTO A NUMERIC VALUE
C              (1 TO 66).
C     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
C                              CONTAINING THE HOLLERITH
C                              CHARACTER(S) OF INTEREST.
C     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
C                              CONTAINING THE NUMERIC
C                              DESIGNATION FOR THE
C                              ALPHABETIC CHARACTER.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH     1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --APRIL     1987.
C     UPDATED         --AUGUST    1992.  ADD SYNONYMS FOR REVERSE
C                                        TRIANGLE (TO AGREE WITH
C                                        DOCUMENTATION), ADD ARROW CASE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUG
      CHARACTER*4 IFOUND
C
      CHARACTER*1 IBASLC
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
C
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
C
C               **********************************
C               **  STEP 1--                    **
C               **  CONVERT THE CHARACTER       **
C               **********************************
C
      IF(ICHAR2.EQ.'/   ')GOTO100
      IF(ICHAR2.EQ.'(   ')GOTO200
      IF(ICHAR2.EQ.')   ')GOTO300
      IF(ICHAR2.EQ.'[   ')GOTO400
      IF(ICHAR2.EQ.'LBRA')GOTO400
      IF(ICHAR2.EQ.']   ')GOTO500
      IF(ICHAR2.EQ.'RBRA')GOTO500
      IF(ICHAR2.EQ.'{   ')GOTO600
      IF(ICHAR2.EQ.'LCBR')GOTO600
      IF(ICHAR2.EQ.'}   ')GOTO700
      IF(ICHAR2.EQ.'RCBR')GOTO700
      IF(ICHAR2.EQ.'LELB')GOTO800
      IF(ICHAR2.EQ.'RELB')GOTO900
      IF(ICHAR2.EQ.'|   ')GOTO1000
      IF(ICHAR2.EQ.'VBAR')GOTO1000
      IF(ICHAR2.EQ.':   ')GOTO1100
      IF(ICHAR2.EQ.'DVBA')GOTO1100
      IF(ICHAR2.EQ.'COLO')GOTO1100
      IF(ICHAR2.EQ.'-   ')GOTO1200
      IF(ICHAR2.EQ.'MINU')GOTO1200
      IF(ICHAR2.EQ.'+   ')GOTO1300
      IF(ICHAR2.EQ.'PLUS')GOTO1300
      IF(ICHAR2.EQ.'CROS')GOTO1300
      IF(ICHAR2.EQ.'+-  ')GOTO1400
      IF(ICHAR2.EQ.'-+  ')GOTO1500
      IF(ICHAR2.EQ.'TIME')GOTO1600
      IF(ICHAR2.EQ.'DOTP')GOTO1700
      IF(ICHAR2.EQ.'/   ')GOTO1800
      IF(ICHAR2.EQ.'DIVI')GOTO1800
      IF(ICHAR2.EQ.'SLAS')GOTO1800
      IF(ICHAR2.EQ.'=   ')GOTO1900
      IF(ICHAR2.EQ.'EQUA')GOTO1900
      IF(ICHAR2.EQ.'NOT=')GOTO2000
      IF(ICHAR2.EQ.'<>')GOTO2000
      IF(ICHAR2.EQ.'><')GOTO2000
      IF(ICHAR2.EQ.'EQUI')GOTO2100
      IF(ICHAR2.EQ.'<   ')GOTO2200
      IF(ICHAR2.EQ.'LT  ')GOTO2200
      IF(ICHAR2.EQ.'>   ')GOTO2300
      IF(ICHAR2.EQ.'GT  ')GOTO2300
      IF(ICHAR2.EQ.'<=  ')GOTO2400
      IF(ICHAR2.EQ.'=<  ')GOTO2400
      IF(ICHAR2.EQ.'LTEQ')GOTO2400
      IF(ICHAR2.EQ.'>=  ')GOTO2500
      IF(ICHAR2.EQ.'=>  ')GOTO2500
      IF(ICHAR2.EQ.'GTEQ')GOTO2500
      IF(ICHAR2.EQ.'VARI')GOTO2600
      IF(ICHAR2.EQ.'APPR')GOTO2700
      IF(ICHAR2.EQ.'~   ')GOTO2700
      IF(ICHAR2.EQ.'TILD')GOTO2700
      IF(ICHAR2.EQ.'CARA')GOTO2800
      IF(ICHAR2.EQ.'PRIM')GOTO2900
      IF(ICHAR2.EQ.'`   ')GOTO3000
      IF(ICHAR2.EQ.'LACC')GOTO3000
      IF(ICHAR2.EQ.'BREV')GOTO3100
      IF(ICHAR2.EQ.'RQUO')GOTO3200
      IF(ICHAR2.EQ.'LQUO')GOTO3300
      IF(ICHAR2.EQ.'NASP')GOTO3400
      IF(ICHAR2.EQ.'IASP')GOTO3500
      IF(ICHAR2.EQ.'RADI')GOTO3600
      IF(ICHAR2.EQ.'SUBS')GOTO3700
      IF(ICHAR2.EQ.'UNIO')GOTO3800
      IF(ICHAR2.EQ.'SUPE')GOTO3900
      IF(ICHAR2.EQ.'INTR')GOTO4000
      IF(ICHAR2.EQ.'ELEM')GOTO4100
      IF(ICHAR2.EQ.'RARR')GOTO4200
      IF(ICHAR2.EQ.'^   ')GOTO4300
      IF(ICHAR2.EQ.'UARR')GOTO4300
      IF(ICHAR2.EQ.'LARR')GOTO4400
      IF(ICHAR2.EQ.'DARR')GOTO4500
      IF(ICHAR2.EQ.'PART')GOTO4600
      IF(ICHAR2.EQ.'DEL ')GOTO4700
      IF(ICHAR2.EQ.'LRAD')GOTO4800
      IF(ICHAR2.EQ.'INTE')GOTO4900
      IF(ICHAR2.EQ.'CINT')GOTO5000
      IF(ICHAR2.EQ.'INFI')GOTO5100
      IF(ICHAR2.EQ.'%   ')GOTO5200
      IF(ICHAR2.EQ.'&   ')GOTO5300
      IF(ICHAR2.EQ.'@   ')GOTO5400
      IF(ICHAR2.EQ.'$   ')GOTO5500
      IF(ICHAR2.EQ.'#   ')GOTO5600
      IF(ICHAR2.EQ.'PARA')GOTO5700
      IF(ICHAR2.EQ.'DAGG')GOTO5800
      IF(ICHAR2.EQ.'DDAG')GOTO5900
      IF(ICHAR2.EQ.'THEX')GOTO6000
      IF(ICHAR2.EQ.'PROD')GOTO6100
      IF(ICHAR2.EQ.'SUMM')GOTO6200
      IF(ICHAR2.EQ.'THFO')GOTO6300
      IF(ICHAR2.EQ.'LVBA')GOTO6400
      IF(ICHAR2.EQ.'HBAR')GOTO6500
      IF(ICHAR2.EQ.'LHBA')GOTO6600
C
      IF(ICHAR2.EQ.'.   ')GOTO10100
      IF(ICHAR2.EQ.'POIN')GOTO10100
      IF(ICHAR2.EQ.'PO  ')GOTO10100
      IF(ICHAR2.EQ.'PT  ')GOTO10100
      IF(ICHAR2.EQ.'CIRC')GOTO10200
      IF(ICHAR2.EQ.'CI  ')GOTO10200
      IF(ICHAR2.EQ.'SQUA')GOTO10300
      IF(ICHAR2.EQ.'SQ  ')GOTO10300
      IF(ICHAR2.EQ.'TRIA')GOTO10400
      IF(ICHAR2.EQ.'TR  ')GOTO10400
      IF(ICHAR2.EQ.'DIAM')GOTO10500
      IF(ICHAR2.EQ.'DI  ')GOTO10500
      IF(ICHAR2.EQ.'STAR')GOTO10600
      IF(ICHAR2.EQ.'ST  ')GOTO10600
      IF(ICHAR2.EQ.'*   ')GOTO10700
      IF(ICHAR2.EQ.'ASTE')GOTO10700
      IF(ICHAR2.EQ.'AS  ')GOTO10700
      IF(ICHAR2.EQ.'TRIR')GOTO10800
      IF(ICHAR2.EQ.'TRII')GOTO10800
C  AUGUST 1992.  ADD FOLLOWING 2 LINES (TO MAKE DOCUMENTATION CORRECT)
      IF(ICHAR2.EQ.'REVT')GOTO10800
      IF(ICHAR2.EQ.'RT  ')GOTO10800
C
      IF(ICHAR2.EQ.'BARU')GOTO10900
      IF(ICHAR2.EQ.'BU  ')GOTO10900
      IF(ICHAR2.EQ.'BARV')GOTO10900
      IF(ICHAR2.EQ.'BV  ')GOTO10900
      IF(ICHAR2.EQ.'BARH')GOTO11000
      IF(ICHAR2.EQ.'BH  ')GOTO11000
      IF(ICHAR2.EQ.'ARRU')GOTO11100
      IF(ICHAR2.EQ.'AU  ')GOTO11100
      IF(ICHAR2.EQ.'ARRD')GOTO11200
      IF(ICHAR2.EQ.'AD  ')GOTO11200
      IF(ICHAR2.EQ.'ARRL')GOTO11300
      IF(ICHAR2.EQ.'AL  ')GOTO11300
      IF(ICHAR2.EQ.'ARRR')GOTO11400
      IF(ICHAR2.EQ.'AR  ')GOTO11400
      CALL DPCONA(92,IBASLC)
      IF(ICHAR2.EQ.IBASLC)GOTO11500
      IF(ICHAR2.EQ.'BASL')GOTO11500
      IF(ICHAR2.EQ.'BACK')GOTO11500
      IF(ICHAR2.EQ.'BS  ')GOTO11500
      IF(ICHAR2.EQ.'_   ')GOTO11600
      IF(ICHAR2.EQ.'UNDE')GOTO11600
      IF(ICHAR2.EQ.'CUBE')GOTO11700
      IF(ICHAR2.EQ.'PYRA')GOTO11800
C  AUGUST 1992.  ADD AN ARROW OPTION
      IF(ICHAR2.EQ.'ARRO')GOTO11900
      IF(ICHAR2.EQ.'ARRH')GOTO11900
      IF(ICHAR2.EQ.'VECT')GOTO11900
C
      GOTO17900
C
  100 CONTINUE
      ICHARN=1
      GOTO18000
C
  200 CONTINUE
      ICHARN=2
      GOTO18000
C
  300 CONTINUE
      ICHARN=3
      GOTO18000
C
  400 CONTINUE
      ICHARN=4
      GOTO18000
C
  500 CONTINUE
      ICHARN=5
      GOTO18000
C
  600 CONTINUE
      ICHARN=6
      GOTO18000
C
  700 CONTINUE
      ICHARN=7
      GOTO18000
C
  800 CONTINUE
      ICHARN=8
      GOTO18000
C
  900 CONTINUE
      ICHARN=9
      GOTO18000
C
 1000 CONTINUE
      ICHARN=10
      GOTO18000
C
 1100 CONTINUE
      ICHARN=11
      GOTO18000
C
 1200 CONTINUE
      ICHARN=12
      GOTO18000
C
 1300 CONTINUE
      ICHARN=13
      GOTO18000
C
 1400 CONTINUE
      ICHARN=14
      GOTO18000
C
 1500 CONTINUE
      ICHARN=15
      GOTO18000
C
 1600 CONTINUE
      ICHARN=16
      GOTO18000
C
 1700 CONTINUE
      ICHARN=17
      GOTO18000
C
 1800 CONTINUE
      ICHARN=18
      GOTO18000
C
 1900 CONTINUE
      ICHARN=19
      GOTO18000
C
 2000 CONTINUE
      ICHARN=20
      GOTO18000
C
 2100 CONTINUE
      ICHARN=21
      GOTO18000
C
 2200 CONTINUE
      ICHARN=22
      GOTO18000
C
 2300 CONTINUE
      ICHARN=23
      GOTO18000
C
 2400 CONTINUE
      ICHARN=24
      GOTO18000
C
 2500 CONTINUE
      ICHARN=25
      GOTO18000
C
 2600 CONTINUE
      ICHARN=26
      GOTO18000
C
 2700 CONTINUE
      ICHARN=27
      GOTO18000
C
 2800 CONTINUE
      ICHARN=28
      GOTO18000
C
 2900 CONTINUE
      ICHARN=29
      GOTO18000
C
 3000 CONTINUE
      ICHARN=30
      GOTO18000
C
 3100 CONTINUE
      ICHARN=31
      GOTO18000
C
 3200 CONTINUE
      ICHARN=32
      GOTO18000
C
 3300 CONTINUE
      ICHARN=33
      GOTO18000
C
 3400 CONTINUE
      ICHARN=34
      GOTO18000
C
 3500 CONTINUE
      ICHARN=35
      GOTO18000
C
 3600 CONTINUE
      ICHARN=36
      GOTO18000
C
 3700 CONTINUE
      ICHARN=37
      GOTO18000
C
 3800 CONTINUE
      ICHARN=38
      GOTO18000
C
 3900 CONTINUE
      ICHARN=39
      GOTO18000
C
 4000 CONTINUE
      ICHARN=40
      GOTO18000
C
 4100 CONTINUE
      ICHARN=41
      GOTO18000
C
 4200 CONTINUE
      ICHARN=42
      GOTO18000
C
 4300 CONTINUE
      ICHARN=43
      GOTO18000
C
 4400 CONTINUE
      ICHARN=44
      GOTO18000
C
 4500 CONTINUE
      ICHARN=45
      GOTO18000
C
 4600 CONTINUE
      ICHARN=46
      GOTO18000
C
 4700 CONTINUE
      ICHARN=47
      GOTO18000
C
 4800 CONTINUE
      ICHARN=48
      GOTO18000
C
 4900 CONTINUE
      ICHARN=49
      GOTO18000
C
 5000 CONTINUE
      ICHARN=50
      GOTO18000
C
 5100 CONTINUE
      ICHARN=51
      GOTO18000
C
 5200 CONTINUE
      ICHARN=52
      GOTO18000
C
 5300 CONTINUE
      ICHARN=53
      GOTO18000
C
 5400 CONTINUE
      ICHARN=54
      GOTO18000
C
 5500 CONTINUE
      ICHARN=55
      GOTO18000
C
 5600 CONTINUE
      ICHARN=56
      GOTO18000
C
 5700 CONTINUE
      ICHARN=57
      GOTO18000
C
 5800 CONTINUE
      ICHARN=58
      GOTO18000
C
 5900 CONTINUE
      ICHARN=59
      GOTO18000
C
 6000 CONTINUE
      ICHARN=60
      GOTO18000
C
 6100 CONTINUE
      ICHARN=61
      GOTO18000
C
 6200 CONTINUE
      ICHARN=62
      GOTO18000
C
 6300 CONTINUE
      ICHARN=63
      GOTO18000
C
 6400 CONTINUE
      ICHARN=64
      GOTO18000
C
 6500 CONTINUE
      ICHARN=65
      GOTO18000
C
 6600 CONTINUE
      ICHARN=66
      GOTO18000
C
10100 CONTINUE
      ICHARN=101
      GOTO18000
C
10200 CONTINUE
      ICHARN=102
      GOTO18000
C
10300 CONTINUE
      ICHARN=103
      GOTO18000
C
10400 CONTINUE
      ICHARN=104
      GOTO18000
C
10500 CONTINUE
      ICHARN=105
      GOTO18000
C
10600 CONTINUE
      ICHARN=106
      GOTO18000
C
10700 CONTINUE
      ICHARN=107
      GOTO18000
C
10800 CONTINUE
      ICHARN=108
      GOTO18000
C
10900 CONTINUE
      ICHARN=109
      GOTO18000
C
11000 CONTINUE
      ICHARN=110
      GOTO18000
C
11100 CONTINUE
      ICHARN=111
      GOTO18000
C
11200 CONTINUE
      ICHARN=112
      GOTO18000
C
11300 CONTINUE
      ICHARN=113
      GOTO18000
C
11400 CONTINUE
      ICHARN=114
      GOTO18000
C
11500 CONTINUE
      ICHARN=115
      GOTO18000
C
11600 CONTINUE
      ICHARN=116
      GOTO18000
C
11700 CONTINUE
      ICHARN=117
      GOTO18000
C
11800 CONTINUE
      ICHARN=118
      GOTO18000
C  AUGUST 1992.  ADDED FOLLOWING 3 LINES
11900 CONTINUE
      ICHARN=119
      GOTO18000
C
17900 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7911)
C7911 FORMAT('***** ERROR IN DPCHMA--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7912)
C7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7913)ICHAR2
C7913 FORMAT('      INPUT CHARACTER = ',A4)
CCCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GOTO19000
C
18000 CONTINUE
      IFOUND='YES'
      GOTO19000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCHNU(ICHAR2,ICHARN,IBUG,IFOUND)
C
C     PURPOSE--CONVERT AN ALPHABETIC CHARACTER
C              (0 TO 9) INTO A NUMERIC VALUE
C              (1 TO 10).
C     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
C                              CONTAINING THE HOLLERITH
C                              CHARACTER(S) OF INTEREST.
C     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
C                              CONTAINING THE NUMERIC
C                              DESIGNATION FOR THE
C                              ALPHABETIC CHARACTER.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH     1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUG
      CHARACTER*4 IFOUND
C
      CHARACTER*1 ICH1
      CHARACTER*1 ICH2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
C
      ICH1='-'
      ICH2='-'
C
      ICH1N=(-999)
      ICH2N=(-999)
C
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
C
C               **********************************
C               **  STEP 1--                    **
C               **  CONVERT THE CHARACTER       **
C               **********************************
C
      ICH2(1:1)=ICHAR2(2:2)
CCCCC ICH2N=ICHAR(ICH2)
      CALL DPCOAN(ICH2,ICH2N)
      IF(ICH2N.EQ.32)GOTO1100
      GOTO7900
C
 1100 CONTINUE
      ICH1(1:1)=ICHAR2(1:1)
CCCCC ICH1N=ICHAR(ICH1)
      CALL DPCOAN(ICH1,ICH1N)
      ICHARN=ICH1N-47
      IF(1.LE.ICHARN.AND.ICHARN.LE.10)GOTO8000
      GOTO7900
C
 7900 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7911)
C7911 FORMAT('***** ERROR IN DPCHNU--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7912)
C7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7913)ICHAR
C7913 FORMAT('      INPUT CHARACTER = ',A4)
CCCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GOTO9000
C
 8000 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCHOF(IHARG,IARGT,ARG,NUMARG,
     1                  MAXCHA,
     1                  PCHAHO,PCHAVO,
     1                  IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER (HORIZONTAL AND VERTICAL) OFFSET
C              FOR USE IN MULTI-TRACE PLOTS.
C              THE OFFSET FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE FLOATING POINT
C              VECTORS PCHAHO(.) AND PCHAVO(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --MAXCHA
C     OUTPUT ARGUMENTS--PCHAHO  (A  FLOATING POINT VECTOR
C                       WHOSE I-TH ELEMENT IS THE HORIZONTAL OFFSET
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --PCHAVO  (A  FLOATING POINT VECTOR
C                       WHOSE I-TH ELEMENT IS THE VERTICAL OFFSET
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --PCHAHO = CHARACTER WIDTH
C                     --PCHAVG = VERTICAL GAP BETWEEN CHARACTERS
C                     --PCHAHG = HORIZONTAL GAP BETWEEN CHARACTERS
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1986.
C     UPDATED         --AUGUST    1988.  CORRECTED FORMAT STATEMENT
C     UPDATED         --AUGUST    1988.  CORRECTED LOOP LOGIC
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
      DIMENSION PCHAHO(*)
      DIMENSION PCHAVO(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'OFFS')GOTO1160
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'DISP')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'OFFS')GOTO1105
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DISP')GOTO1105
      GOTO2199
C
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.3)GOTO1120
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.4.AND.IHARG(4).EQ.'ALL')GOTO1140
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      PCHAVO(I)=0.0
      PCHAHO(I)=0.0
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)
 1116 FORMAT('ALL CHARACTER (HORIZ. AND VERT.) OFFSETS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1117)PCHAHO(I),PCHAVO(I)
 1117 FORMAT('    HAVE JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO2190
C
 1120 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB'.OR.IARGT(3).NE.'NUMB')GOTO1180
      PCHAHO(1)=ARG(2)
      PCHAVO(1)=ARG(3)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I
 1126 FORMAT('THE (HORIZ. AND VERT.) OFFSET FOR CHARACTER ',I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)PCHAHO(I),PCHAVO(I)
 1127 FORMAT('    HAS JUST BEEN SET TO ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO2190
C
 1130 CONTINUE
      I=1
      IF(IARGT(3).NE.'NUMB'.OR.IARGT(4).NE.'NUMB')GOTO1180
      DO1135I=1,MAXCHA
      PCHAHO(I)=ARG(3)
      PCHAVO(I)=ARG(4)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1117)PCHAHO(I),PCHAVO(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO2190
C
 1140 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB'.OR.IARGT(3).NE.'NUMB')GOTO1180
      DO1145I=1,MAXCHA
      PCHAHO(I)=ARG(2)
      PCHAVO(I)=ARG(3)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1117)PCHAHO(I),PCHAVO(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO2190
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
CCCCC THE FOLLOWING 1 LINE WAS INSERTED IN AUGUST 1988
      J=0
      DO1155I=1,IMAX,2
      IP1=I+1
      IP2=I+2
      IF(IARGT(IP1).NE.'NUMB')GOTO1180
      IF(IARGT(IP2).NE.'NUMB')GOTO1180
CCCCC PCHAHO(I)=ARG(IP1)                  AUGUST 1988
CCCCC PCHAVO(I)=ARG(IP2)                  AUGUST 1988
CCCCC THE FOLLOWING 3 LINES WERE INSERTED IN AUGUST 1988
      J=J+1
      PCHAHO(J)=ARG(IP1)
      PCHAVO(J)=ARG(IP2)
 1155 CONTINUE
CCCCC THE FOLLOWING 1 LINE WAS INSERTED IN AUGUST 1988
      JMAX=J
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
CCCCC DO1156I=1,IMAX                      AUGUST 1988
CCCCC THE FOLLOWING 1 LINE WAS INSERTED IN AUGUST 1988
      DO1156I=1,JMAX
      WRITE(ICOUT,1126)I
      CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1127)I,PCHAHO(I),PCHAVO(I)             AUGUST 1988
CCCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)PCHAHO(I),PCHAVO(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO2190
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      PCHAHO(I)=0.0
      PCHAVO(I)=0.0
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1117)PCHAHO(I),PCHAVO(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO2190
C
 1180 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DPCHOF--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('CHARACTER (HORIZ. AND VERT.) OFFSETS MUST BE NUMERIC')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('HOWEVER, THE SPECIFIED CHARACTER OFFSET')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)I
 1184 FORMAT('FOR CHARACTER ',I6,' WAS NON-NUMERIC.')
      CALL DPWRST('XXX','BUG ')
      GOTO2199
C
 2190 CONTINUE
      IFOUND='YES'
C
 2199 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHS3(ICASPL,IDIST,NUMSHA,IFORSW,ICASP3,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  N,XMEAN,XSD,XMIN,XMAX,
     1                  A,B,MINMAX,
     1                  SHAPE1,SHAPE2,SHAPE3,SHAPE4,
     1                  SHAPE5,SHAPE6,SHAPE7,
     1                  KSLOC,KSSCAL,ICAPSW,ICAPTY,
     1                  STATVA,STATCD,PVAL,NCELLS,IDF,IDISFL,MINSZ,
     1                  CDF1,CDF2,CDF3,CDF4,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--PRINT THE OUTPUT FOR THE CHI-SQUARE TEST (GROUPED,
C              UNCENSORED CASE) IN ASCII, HTML, LATEX, OR RTF FORMAT
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C         --DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/12
C     ORIGINAL VERSION--DECEMBER  2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL PID(*)
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
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
C
      CHARACTER*60 IDIST
C
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      REAL KSLOC
      REAL KSSCAL
C
C---------------------------------------------------------------------
C
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
C
CCCCC INCLUDE 'DPCOST.INC'
C
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA ALPHA/
     1 0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.5/
C
C-----START POINT-----------------------------------------------------
C
C
      ISUBN1='DPCH'
      ISUBN2='SQ  '
      IERROR='NO'
      IWRITE='OFF'
      CALL DPCONA(92,IBASLC)
C
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
C
C               *******************************************
C               **   STEP 41--                           **
C               **   WRITE OUT INITIAL HEADER TABLE      **
C               *******************************************
C
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CHS3')
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
      ITITLE='Chi-Square Goodness of Fit Test'
      NCTITL=31
C
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
C
      DO4101I=1,NREPL
        ICNT=ICNT+1
        ITEXT(ICNT)='Factor Variable  : '
        WRITE(ITEXT(ICNT)(17:17),'(I1)')I
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(I+1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(I+1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=PID(I+1)
        IDIGIT(ICNT)=NUMDIG
 4101 CONTINUE
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
C
      IEND=46
      DO4111I=46,1,-1
        IF(IDIST(I:I).NE.' ')THEN
          IEND=I
          GOTO4119
        ENDIF
 4111 CONTINUE
      IEND=1
 4119 CONTINUE
      CALL EXTBOU(ICASPL,IBOUND)
C
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
C
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
C
      IF(NUMSHA.GE.1)THEN
        DO4140I=1,NUMSHA
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
C
      NUMROW=ICNT
      DO2310I=1,NUMROW
        NTOT(I)=15
 2310 CONTINUE
C
      ITITLZ=' '
      NCTITZ=0
      IFRST=.TRUE.
      ILAST=.TRUE.
      NCTITZ=0
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
      ITITLE=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
C
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
      DO2521I=1,NUMCOL
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
      DO2523I=1,NUMROW
        DO2525J=1,NUMCOL
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
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
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
      DO2821I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
        IF(I.EQ.3)NTOT(I)=17
        NMAX=NMAX+NTOT(I)
CCCCC   IDIGIT(I)=NUMDIG
        IDIGIT(I)=3
        ITYPCO(I)='ALPH'
 2821 CONTINUE
      ITYPCO(3)='NUME'
      IDIGIT(1)=0
      IDIGIT(2)=0
      DO2823I=1,NUMROW
        DO2825J=1,NUMCOL
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
C
C     FOR LATEX, WE WANT TO ENSURE THAT TRAILING LINE IS PART
C     OF THE TABLE SO THAT IT WILL BE PRINTED IN THE PROPER PLACE.
C
      IF(ICAPTY.EQ.'LATE')THEN
        ILAST=.FALSE.
      ELSE
        ILAST=.TRUE.
      ENDIF
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CHS3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCHS3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCHSY(ICHAR2,ICHARN,IBUG,IFOUND)
C
C     PURPOSE--CONVERT A KEYBOARD SYMBOL
C              (. , ; : ETC.) INTO A NUMERIC VALUE
C              (1 TO 23).
C              (1 TO 24).
C     INPUT  ARGUMENTS--ICHAR2 (A HOLLERITH VARIABLE
C                              CONTAINING THE HOLLERITH
C                              CHARACTER(S) OF INTEREST.
C     OUTPUT ARGUMENTS--ICHARN (AN INTEGER VARIABLE
C                              CONTAINING THE NUMERIC
C                              DESIGNATION FOR THE
C                              ALPHABETIC CHARACTER.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH     1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MAY       1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IBUG
      CHARACTER*4 IFOUND
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
C
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
C
C               **********************************
C               **  STEP 1--                    **
C               **  CONVERT THE CHARACTER       **
C               **********************************
C
      IF(ICHAR2.EQ.'.')GOTO100
      IF(ICHAR2.EQ.',')GOTO200
      IF(ICHAR2.EQ.':')GOTO300
      IF(ICHAR2.EQ.';')GOTO400
      IF(ICHAR2.EQ.'!')GOTO500
      IF(ICHAR2.EQ.'?')GOTO600
      IF(ICHAR2.EQ.'&')GOTO700
      IF(ICHAR2.EQ.'$')GOTO800
      IF(ICHAR2.EQ.'/')GOTO900
      IF(ICHAR2.EQ.'(')GOTO1000
      IF(ICHAR2.EQ.')')GOTO1100
      IF(ICHAR2.EQ.'*')GOTO1200
      IF(ICHAR2.EQ.'-')GOTO1300
      IF(ICHAR2.EQ.'+')GOTO1400
      IF(ICHAR2.EQ.'=')GOTO1500
      IF(ICHAR2.EQ.'''')GOTO1600
      IF(ICHAR2.EQ.'"')GOTO1700
      IF(ICHAR2.EQ.'DEGR')GOTO1800
      IF(ICHAR2.EQ.'NOSP')GOTO1900
      IF(ICHAR2.EQ.'HASP')GOTO2000
      IF(ICHAR2.EQ.' ')GOTO2100
      IF(ICHAR2.EQ.'LAPO')GOTO2200
      IF(ICHAR2.EQ.'RAPO')GOTO2300
      IF(ICHAR2.EQ.'|')GOTO2400
      GOTO7900
C
  100 CONTINUE
      ICHARN=1
      GOTO8000
C
  200 CONTINUE
      ICHARN=2
      GOTO8000
C
  300 CONTINUE
      ICHARN=3
      GOTO8000
C
  400 CONTINUE
      ICHARN=4
      GOTO8000
C
  500 CONTINUE
      ICHARN=5
      GOTO8000
C
  600 CONTINUE
      ICHARN=6
      GOTO8000
C
  700 CONTINUE
      ICHARN=7
      GOTO8000
C
  800 CONTINUE
      ICHARN=8
      GOTO8000
C
  900 CONTINUE
      ICHARN=9
      GOTO8000
C
 1000 CONTINUE
      ICHARN=10
      GOTO8000
C
 1100 CONTINUE
      ICHARN=11
      GOTO8000
C
 1200 CONTINUE
      ICHARN=12
      GOTO8000
C
 1300 CONTINUE
      ICHARN=13
      GOTO8000
C
 1400 CONTINUE
      ICHARN=14
      GOTO8000
C
 1500 CONTINUE
      ICHARN=15
      GOTO8000
C
 1600 CONTINUE
      ICHARN=16
      GOTO8000
C
 1700 CONTINUE
      ICHARN=17
      GOTO8000
C
 1800 CONTINUE
      ICHARN=18
      GOTO8000
C
 1900 CONTINUE
      ICHARN=19
      GOTO8000
C
 2000 CONTINUE
      ICHARN=20
      GOTO8000
C
 2100 CONTINUE
      ICHARN=21
      GOTO8000
C
 2200 CONTINUE
      ICHARN=22
      GOTO8000
C
 2300 CONTINUE
      ICHARN=23
      GOTO8000
C
 2400 CONTINUE
      ICHARN=24
      GOTO8000
C
 7900 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7911)
C7911 FORMAT('***** ERROR IN DPCHSY--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7912)
C7912 FORMAT('      NO MATCH FOUND FOR INPUT CHARACTER.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,7913)ICHAR2
C7913 FORMAT('      INPUT CHARACTER = ',A4)
CCCCC CALL DPWRST('XXX','BUG ')
      IFOUND='NO'
      GOTO9000
C
 8000 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCHSZ(PDEFHE,MAXCHA,
     1PCHAHE,PCHAWI,PCHAVG,PCHAHG,
     1IBUGP2,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER SIZES FOR USE IN MULTI-TRACE PLOTS.
C              THE SIZE FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE FLOATING POINT
C              VECTOR PCHAHE(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --PDEFHE
C                     --MAXCHA
C     OUTPUT ARGUMENTS--PCHAHE  (A  FLOATING POINT VECTOR
C                       WHOSE I-TH ELEMENT IS THE SIZE (= HEIGHT)
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --PCHAWI = CHARACTER WIDTH
C                     --PCHAVG = VERTICAL GAP BETWEEN CHARACTERS
C                     --PCHAHG = HORIZONTAL GAP BETWEEN CHARACTERS
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1982.
C     UPDATED         --JANUARY   1995. ALLOW ? AS ARGUMENT (FOR HELP)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC CHARACTER*4 IHARG          DECEMBER 1986
CCCCC CHARACTER*4 IARGT          DECEMBER 1986
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASEQ
      CHARACTER*4 IWRITE
C
C---------------------------------------------------------------------
C
CCCCC DIMENSION IHARG(*)          DECEMBER 1986
CCCCC DIMENSION IARGT(*)          DECEMBER 1986
CCCCC DIMENSION IARG(*)          DECEMBER 1986
CCCCC DIMENSION ARG(*)          DECEMBER 1986
C
      DIMENSION PCHAHE(*)
      DIMENSION PCHAWI(*)
      DIMENSION PCHAVG(*)
      DIMENSION PCHAHG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCH'
      ISUBN2='SZ  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'SIZE'.AND.
     1IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')GOTO2110
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'HEIG'.AND.
     1IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')GOTO2110
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'SIZE'.AND.
     1IHARG(3).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')GOTO2110
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'HEIG'.AND.
     1IHARG(3).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')GOTO2110
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'SIZE')GOTO1160
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'HEIG')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SIZE')GOTO1105
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'HEIG')GOTO1105
      GOTO9000
C
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
CCCCC THE FOLLOWING LINE WAS ADDED    JANUARY 1995
      IF(IHARG(NUMARG).EQ.'?')GOTO1200
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GOTO1140
C
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'AUTO')GOTO3000
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      PCHAHE(I)=PDEFHE
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAHE(I)
 1116 FORMAT('ALL CHARACTER SIZES HAVE JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO8000
C
 1120 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GOTO1180
      PCHAHE(1)=ARG(2)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,PCHAHE(I)
 1126 FORMAT('THE SIZE FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO8000
C
 1130 CONTINUE
      I=1
      IF(IARGT(3).NE.'NUMB')GOTO1180
      DO1135I=1,MAXCHA
      PCHAHE(I)=ARG(3)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAHE(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO8000
C
 1140 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GOTO1180
      DO1145I=1,MAXCHA
      PCHAHE(I)=ARG(2)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAHE(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO8000
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO1155I=1,IMAX
      IP1=I+1
      IF(IARGT(IP1).NE.'NUMB')GOTO1180
      PCHAHE(I)=ARG(IP1)
 1155 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1156I=1,IMAX
      WRITE(ICOUT,1126)I,PCHAHE(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO8000
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      PCHAHE(I)=PDEFHE
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAHE(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO8000
C
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
      GOTO9000
C
CCCCC THE FOLLOWING SECTION WAS ADDED    JANUARY 1995
 1200 CONTINUE
      IFOUND='YES'
      IF(IFEEDB.EQ.'OFF')GOTO1229
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
      GOTO9000
C
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
      GOTO2170
C
 2170 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO2179
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO2175I=1,IMAX
      WRITE(ICOUT,2176)I,PCHAHE(I)
 2176 FORMAT('THE SIZE FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 2175 CONTINUE
 2179 CONTINUE
      GOTO8000
C
C               ***********************************************************
C               **  STEP 30--                                            **
C               **  TREAT THE   CHARACTER SIZE AUTOMATIC <VARIABLE>  CASE **
C               ***********************************************************
C
 3000 CONTINUE
C
C               ********************************************
C               **  STEP 31--                             **
C               **  CHECK THE VALIDITY OF ARGUMENT 3      **
C               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
C               ********************************************
C
      ISTEPN='31'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(3)
      IHLEF2=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
C
C               *****************************************
C               **  STEP 32--                          **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='32'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO3290
      DO3200J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO3210
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO3210
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO3220
 3200 CONTINUE
      GOTO3290
 3210 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO3290
 3220 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO3290
 3290 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO3295
      WRITE(ICOUT,3291)NUMARG,ILOCQ
 3291 FORMAT('NUMARG,ILOCQ = ',2I8)
      CALL DPWRST('XXX','BUG ')
 3295 CONTINUE
C
C               *********************************************
C               **  STEP 33--                              **
C               **  TEMPORARILY FORM THE VARIABLE Y(.)     **
C               **  WHICH WILL HOLD THE RESPONSE VARIABLE. **
C               **  FORM THIS VARIABLE BY                  **
C               **  BRANCHING TO THE APPROPRIATE SUBCASE   **
C               **  (FULL, SUBSET, OR FOR).                **
C               *********************************************
C
      ISTEPN='33'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO3310
      IF(ICASEQ.EQ.'SUBS')GOTO3320
      IF(ICASEQ.EQ.'FOR')GOTO3330
C
 3310 CONTINUE
      DO3315I=1,NLEFT
      ISUB(I)=1
 3315 CONTINUE
      NQ=NLEFT
      GOTO3350
C
 3320 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GOTO3350
C
 3330 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GOTO3350
C
 3350 CONTINUE
      MINN2=1
      IF(NQ.GE.MINN2)GOTO3360
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3351)
 3351 FORMAT('***** ERROR IN DPCHSZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3352)
 3352 FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',
     1'EXTRACTED,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3353)IHLEFT,IHLEF2
 3353 FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING',
     1'FROM VARIABLE ',A4,A4)
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
      GOTO9000
C
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
      DO3370I=1,IMAX
      IF(ISUB(I).EQ.0)GOTO3370
      J=J+1
C
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)Y(J)=V(IJ)
      IF(ICOLL.EQ.MAXCP1)Y(J)=PRED(I)
      IF(ICOLL.EQ.MAXCP2)Y(J)=RES(I)
      IF(ICOLL.EQ.MAXCP3)Y(J)=YPLOT(I)
      IF(ICOLL.EQ.MAXCP4)Y(J)=XPLOT(I)
      IF(ICOLL.EQ.MAXCP5)Y(J)=X2PLOT(I)
      IF(ICOLL.EQ.MAXCP6)Y(J)=TAGPLO(I)
C
 3370 CONTINUE
      NS=J
      NY=J
C
C               *****************************************
C               **  STEP 34--                          **
C               **  EXTRACT THE DISTINCT VALUES        **
C               **  FROM THE TARGET VARIABLE Y(.)   .  **
C               **  STORE THEM IN X(.)   .             **
C               *****************************************
C
      IWRITE='OFF'
      CALL DISTIN(Y,NY,IWRITE,X,NX,IBUGP2,IERROR)
C
C               ***********************************
C               **  STEP 35--                    **
C               **  SORT THESE DISTINCT VALUES   **
C               **  (IN PLACE).                  **
C               ***********************************
C
      CALL SORT(X,NX,X)
C
C               ******************************************
C               **  STEP 36--                           **
C               **  COPY    THE NUMERIC VALUES IN X(.)  **
C               **  INTO INDIVIDUAL ELEMENTS            **
C               **  OF PCHAHE(.)                        **
C               **  NOTE--MAX NUMBER OF VALUES  = 100   **
C               ******************************************
C
      IMAX=NX
      IF(IMAX.GT.MAXCHA)IMAX=MAXCHA
      DO3650I=1,IMAX
      PCHAHE(I)=X(I)
 3650 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO3679
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO3675I=1,IMAX
      WRITE(ICOUT,3676)I,PCHAHE(I)
 3676 FORMAT('CHARACTER SIZE ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 3675 CONTINUE
 3679 CONTINUE
      GOTO8000
C
 8000 CONTINUE
      IFOUND='YES'
      DO8010I=1,MAXCHA
      PCHAWI(I)=PCHAHE(I)*0.5
      PCHAVG(I)=PCHAHE(I)*0.5
      PCHAHG(I)=PCHAWI(I)*0.5
 8010 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO9090
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
      IF(NY.LE.0)GOTO9022
      DO9023I=1,NY
      WRITE(ICOUT,9024)I,Y(I)
 9024 FORMAT('I,Y(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9023 CONTINUE
 9022 CONTINUE
      WRITE(ICOUT,9031)NX
 9031 FORMAT('NX = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NX.LE.0)GOTO9032
      DO9033I=1,NX
      WRITE(ICOUT,9034)I,X(I)
 9034 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9033 CONTINUE
 9032 CONTINUE
      WRITE(ICOUT,9041)MAXCHA
 9041 FORMAT('MAXCHA = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NX.LE.0)GOTO9042
      DO9043I=1,NX
      WRITE(ICOUT,9044)I,PCHAHE(I),PCHAWI(I),PCHAVG(I),PCHAHG(I)
 9044 FORMAT('I,PCHAHE(I),PCHAWI(I),PCHAVG(I),PCHAHG(I) = ',I8,2X,
     14E15.7)
      CALL DPWRST('XXX','BUG ')
 9043 CONTINUE
 9042 CONTINUE
 9090 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHTH(IHARG,ARG,NUMARG,PDEFTH,MAXCHA,PCHATH,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER THICKNESSS FOR USE IN MULTI-TRACE PLOTS.
C              THE THICKNESS FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE HOLLERITH
C              VECTOR PCHATH(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --ARG    (A REAL VECTOR)
C                     --NUMARG
C                     --PDEFTH
C                     --MAXCHA
C     OUTPUT ARGUMENTS--PCHATH  (A  REAL VECTOR
C                       WHOSE I-TH ELEMENT IS THE THICKNESS
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
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
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
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
      DIMENSION PCHATH(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'THIC')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'THIC')GOTO1105
      GOTO1199
C
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GOTO1140
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      PCHATH(I)=PDEFTH
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHATH(I)
 1116 FORMAT('ALL CHARACTER THICKNESSS HAVE JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO1190
C
 1120 CONTINUE
      PCHATH(1)=ARG(2)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,PCHATH(I)
 1126 FORMAT('THE THICKNESS FOR CHARACTER ',I6,' HAS JUST BEEN ',
     1'SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO1190
C
 1130 CONTINUE
      DO1135I=1,MAXCHA
      PCHATH(I)=ARG(3)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHATH(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO1190
C
 1140 CONTINUE
      DO1145I=1,MAXCHA
      PCHATH(I)=ARG(2)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHATH(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO1190
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO1155I=1,IMAX
      IP1=I+1
      PCHATH(I)=ARG(IP1)
 1155 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1156I=1,IMAX
      WRITE(ICOUT,1126)I,PCHATH(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      PCHATH(I)=PDEFTH
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHATH(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO1190
C
 1190 CONTINUE
      IFOUND='YES'
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPCHUN(IHARG,NUMARG,MAXCHA,ICHATY,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER UNITS (DATA OR SCREEN) FOR USE IN
C              MULTI-TRACE PLOTS.  THE UNITS FOR THE CHARACTER FOR THE
C              I-TH TRACE WILL BE PLACED IN THE I-TH ELEMENT OF THE
C              HOLLERITH VECTOR ICHATY(.).
C
C              THE UNITS ARE SPECIFIED AS:
C
C                 DD     => X AXIS = DATA UNITS,   Y AXIS = DATA UNITS
C                 DS     => X AXIS = DATA UNITS,   Y AXIS = SCREEN UNITS
C                 SD     => X AXIS = SCREEN UNITS, Y AXIS = DATA UNITS
C                 SS     => X AXIS = SCREEN UNITS, Y AXIS = SCREEN UNITS
C                 DATA   => X AXIS = DATA UNITS,   Y AXIS = DATA UNITS
C                 SCREEN => X AXIS = SCREEN UNITS, Y AXIS = SCREEN UNITS
C
C              THE DEFAULT IS DATA UNITS FOR BOTH AXES.
C
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --MAXCHA
C     OUTPUT ARGUMENTS--ICHATY  (A  HOLLERITH VECTOR WHOSE I-TH ELEMENT
C                       IS THE UNITS FOR THE CHARACTER ASSIGNED TO THE
C                       I-TH TRACE IN A MULTI-TRACE PLOT.
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
C     VERSION NUMBER--2018/01
C     ORIGINAL VERSION--JANUARY   2018.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 ICHATY
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IDEFTY
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION ICHATY(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
      IDEFTY='DD'
C
      IF((NUMARG.EQ.1.AND.IHARG(1).EQ.'UNIT') .OR.
     1   (NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL'))THEN
        DO1165I=1,MAXCHA
          ICHATY(I)=IDEFTY
 1165   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          I=1
          WRITE(ICOUT,1116)ICHATY(I)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'UNIT')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
          DO1115I=1,MAXCHA
            ICHATY(I)=IDEFTY
 1115     CONTINUE
C
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
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1126)I,ICHATY(I)
 1126       FORMAT('THE UNITS FOR CHARACTER ',I6,
     1             ' HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')THEN
          DO1135I=1,MAXCHA
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
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHATY(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')THEN
          DO1145I=1,MAXCHA
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
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1116)ICHATY(I)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ELSE
          IMAX=NUMARG-1
          IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
          DO1155I=1,IMAX
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
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO1156I=1,IMAX
              WRITE(ICOUT,1126)I,ICHATY(I)
              CALL DPWRST('XXX','BUG ')
 1156       CONTINUE
          ENDIF
        ENDIF
      ELSE
        IFOUND='NO'
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCHIS(MAXNXT,
     1                  ICASAN,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--COMPUTE THE CHI-SQUARE TEST FOR INDEPENDENCE
C     EXAMPLE--CHI-SQUARE INDEPENDENCE TEST Y1 Y2
C            --CHI-SQUARE INDEPENDENCE TEST N11 N21 N12 N22
C            --CHI-SQUARE INDEPENDENCE TEST M
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC
C                STATISTICS", THIRD EDITION, WILEY, PP. 204-216.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/3
C     ORIGINAL VERSION--MARCH     2007.
C     UPDATED         --JANUARY   2011. USE DPPARS, DPPAR3, DPPAR6
C     UPDATED         --JUNE      2019. TWEAK TO SCRATCH STORAGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
C
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASEQ
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
      CHARACTER*4 ICASE
C
      CHARACTER*40 INAME
C
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
      PARAMETER(MAXLEV=1000)
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
C
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
      REAL TEMP3(MAXOBV)
      REAL XIDTEM(MAXOBV)
      REAL XIDTE2(MAXOBV)
      REAL XMAT(MAXLEV,MAXLEV)
C
      DOUBLE PRECISION ROWTOT(MAXOBV)
      DOUBLE PRECISION COLTOT(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),XMAT(1,1))
C
      EQUIVALENCE (DGARBG(IDGAR1),ROWTOT(1))
      EQUIVALENCE (DGARBG(IDGAR2),COLTOT(1))
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCH'
      ISUBN2='IS  '
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
      N11=(-999)
      N21=(-999)
      N12=(-999)
      N22=(-999)
      AN11=0.0
      AN12=0.0
      AN21=0.0
      AN22=0.0
C
      NS1=(-999)
      NS2=(-999)
      NS3=(-999)
      NS4=(-999)
C
      ICASE='PARA'
      MINN2=2
C
      IFOUND='YES'
      ICASEQ='UNKN'
C
C               ***************************************************
C               **  TREAT THE CHI-SQUARE INDEPENDENCE TEST CASE  **
C               ***************************************************
C
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
        DO59I=1,NUMARG
          WRITE(ICOUT,57)I,IHARG(I),IHARG2(I),ARG(I)
   57     FORMAT('I,IHARG(I),IHARG2(I),ARG(I) = ',I5,A4,A4,G15.7)
   59   CONTINUE
      ENDIF
C
C               *********************************
C               **  STEP 4--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
C               ***********************************
C               **  STEP 22--                    **
C               **  CHECK FOR PROPER VALUES FOR  **
C               **  INPUT PARAMETERS             **
C               ***********************************
C
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
C
        ISTEPN='22'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(N11.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
 2201     FORMAT('***** ERROR FROM CHI-SQUARE INDEPENDENCE TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2203)
 2203     FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = THE ',
     1           'NUMBER OF SUCCESSES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2204)
 2204     FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2205)N11
 2205     FORMAT('      N11 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
        ELSEIF(N21.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2303)
 2303     FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = THE ',
     1           'NUMBER OF FAILURES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2304)
 2304     FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2305)N21
 2305     FORMAT('      N21 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
        ELSEIF(N12.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2403)
 2403     FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = THE ',
     1           'NUMBER OF SUCCESSES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2404)
 2404     FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2405)N12
 2405     FORMAT('      N12 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
        ELSEIF(N22.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2503)
 2503     FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = THE ',
     1           'NUMBER OF FAILURES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2504)
 2504     FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2505)N22
 2505     FORMAT('      N22 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
      ELSEIF(IVARTY(1).EQ.'VARI')THEN
C
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
          GOTO9000
        ENDIF
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,X,X,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NS1=NLOCAL
        NS2=NLOCA2
C
      ELSEIF(IVARTY(1).EQ.'MATR')THEN
        ICASE='MATR'
        ICOL=1
        NUMVAR=1
        CALL DPPAR6(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              XMAT,MAXLEV,NROW,NCOL,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        ICASE='TABL'
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***********************************
C               **  STEP 61--                    **
C               **  COMPUTE THE CHI-SQUARE TEST  **
C               ***********************************
C
      ISTEPN='61'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
C
      CALL DPCHI2(Y,NS1,X,NS2,
     1            AN11,AN21,AN12,AN22,
     1            XMAT,MAXLEV,NROW,NCOL,
     1            XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,MAXOBW,
     1            ROWTOT,COLTOT,
     1            ICASE,
     1            ICAPSW,ICAPTY,IFORSW,
     1            STATVA,CDF,STATV2,CDF2,
     1            ISUBRO,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 62--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='62'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'CHIS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISUBN0='CHIS'
C
      IH='STAT'
      IH2='VAL '
      VALUE0=STATVA
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='STAT'
      IH2='CDF '
      VALUE0=CDF
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='STAT'
      IH2='VAL2'
      VALUE0=STATV2
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='STAT'
      IH2='CDF2'
      VALUE0=CDF2
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCHI2(Y1,N1,Y2,N2,
     1                  AN11,AN21,AN12,AN22,
     1                  XMAT,MAXLEV,NROW,NCOL,
     1                  XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,MAXNXT,
     1                  ROWTOT,COLTOT,
     1                  ICASE,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  STATVA,CDF,STATV2,CDF2,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--PERFORM A CHI-SQUARE TEST FOR INDEPENDENCE.
C              THE INPUT CAN BE ENTERED IN THE FOLLOWING WAYS:
C
C              1) THE COMMON CASE OF A 2X2 TABLE CAN BE
C                 ENTERED AS 4 PARAMETERS:
C
C                    N11 = NUMBER OF SUCCESSES FOR VARIABLE 1
C                    N21 = NUMBER OF FAILURES  FOR VARIABLE 1
C                    N12 = NUMBER OF SUCCESSES FOR VARIABLE 2
C                    N22 = NUMBER OF SUCCESSES FOR VARIABLE 2
C
C              2) AS RAW DATA, THAT IS TWO VARIABLES.  A
C                 CROSS-TABULATION IS PERFORMED TO GENERATE
C                 AN RXC TABLE OF COUNTS.
C
C              3) AS A MATRIX, I.E., THE RXC TABLE HAS ALREADY
C                 BEEN GENERATED.
C
C              THE CHI-SQUARE TEST CAN THEN BE COMPUTED AS:
C
C                 CHI-SQUARE = SUM[(f - F)**2/F
C
C              WHERE THE SUMMATION IS OVER ALL CELLS IN THE
C              TABLE AND WHERE
C
C                 f   = OBSERVED FFEQUENCY OF THE CELL
C                 F   = EXPECTED FREQUENCY OF THE CELL
C                     = (ROW TOTAL)*(COLUMN TOTAL)/(GRAND TOTAL)
C
C              SOME ANALYSTS PREFER TO USE THE YATES CONTINUITY
C              CORRECTION.  IN THIS CORRECTON, 0.5 IS ADDED TO
C              EACH CELL.  DATAPLOT WILL GENERATE THE TEST STATISTIC
C              FOR BOTH THE UNCORRECTED AND CORRECTED CASES.
C
C     EXAMPLE--CHI-SQUARE INDEPENDENCE TEST Y1 Y2
C            --CHI-SQUARE INDEPENDENCE TEST N11 N21 N12 N22
C            --CHI-SQUARE INDEPENDENCE TEST M
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGYU LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/3
C     ORIGINAL VERSION--MARCH     2007.
C     UPDATED         --JANUARY   2011. USE DPAUFI TO OPEN/CLOSE
C                                       AUXILLARY FILES
C     UPDATED         --JANUARY   2011. USE DPDTA1, DPDT5B TO PRINT
C                                       TABLES
C     UPDATED         --APRIL     2019. USER CAN SPECIFY NUMBER OF
C                                       DECIMAL POINTS FOR AUXILLARY
C                                       FILES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 ICASE
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
C
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
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IOP
      CHARACTER*20 IFORMT
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
C
      DIMENSION XMAT(MAXLEV,MAXLEV)
C
      DOUBLE PRECISION ROWTOT(*)
      DOUBLE PRECISION COLTOT(*)
C
      PARAMETER (NUMALP=6)
CCCCC DIMENSION SIGVAL(NUMALP)
CCCCC DIMENSION ALOWCL(NUMALP)
CCCCC DIMENSION AUPPCL(NUMALP)
CCCCC DIMENSION ALOWC2(NUMALP)
CCCCC DIMENSION AUPPC2(NUMALP)
C
      DOUBLE PRECISION GTOTAL
      DOUBLE PRECISION VALTMP
      DOUBLE PRECISION EXP
      DOUBLE PRECISION CHISQ1
      DOUBLE PRECISION CHISQ2
C
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
C
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA SIGVAL /0.50, 0.80, 0.90, 0.95, 0.975, 0.99/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCH'
      ISUBN2='I2  '
      IERROR='NO'
      IWRITE='NO'
C
      ICONC1='ACCEPT'
      ICONC2='ACCEPT'
      ICONC3='ACCEPT'
      ICONC4='ACCEPT'
      ICONC5='ACCEPT'
      ICONC6='ACCEPT'
C
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=0
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      WRITE(IOUNI1,41)
   41 FORMAT(5X,'ROW  COLUMN',9X,'ROWTOT',9X,'COLTOT',6X,'EXPECTED',
     1      8X,'OBSERVED')
C
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
          DO56I=1,N1
            WRITE(ICOUT,57)I,Y1(I)
   57       FORMAT('I,Y1(I) = ',I8,E15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
          WRITE(ICOUT,65)N2
   65     FORMAT('N2 = ',I8)
          CALL DPWRST('XXX','WRIT')
          DO66I=1,N2
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
 
C               ********************************************
C               **  STEP 0--                              **
C               **  BRANCH TO APPROPRIATE CASE (PARAMETER **
C               **  OR VARIABLE)                          **
C               ********************************************
C
      ISTEPN='00'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASE.EQ.'PARA')GOTO1000
      IF(ICASE.EQ.'VARI')GOTO2000
      IF(ICASE.EQ.'TABL')GOTO3000
C
C               ********************************************
C               **  STEP 11--                             **
C               **  PARAMETER CASE                        **
C               ********************************************
C
 1000 CONTINUE
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ********************************************
C               **  STEP 12--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      N11=INT(AN11+0.5)
      N21=INT(AN21+0.5)
      N12=INT(AN12+0.5)
      N22=INT(AN22+0.5)
C
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N11.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
 1201   FORMAT('***** ERROR FROM THE CHI-SQUARE INDEPENDENCE TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1203)
 1203   FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = THE ',
     1         'NUMBER OF SUCCESSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1204)
 1204   FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1205)N11
 1205   FORMAT('      N11 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N21.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1303)
 1303   FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = THE ',
     1         'NUMBER OF FAILURES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1304)
 1304   FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1305)N21
 1305   FORMAT('      N21 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N12.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1403)
 1403   FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = THE ',
     1         'NUMBER OF SUCCESSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1404)
 1404   FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1405)N12
 1405   FORMAT('      N12 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N22.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1503)
 1503   FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = THE ',
     1         'NUMBER OF FAILURES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1504)
 1504   FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1505)N22
 1505   FORMAT('      N22 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************
C               **  STEP 12--                             **
C               **  COMPUTE THE CHI-SQUARE TEST           **
C               ********************************************
C
C
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
C
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
C
      IINDX=0
      CHISQ1=0.0D0
      CHISQ2=0.0D0
      DO1600J=1,2
        DO1610I=1,2
          IINDX=IINDX+1
          EXP=ROWTOT(I)*COLTOT(J)/GTOTAL
          VALTMP=DBLE(TEMP1(IINDX))
          CHISQ1=CHISQ1 + (VALTMP - EXP)**2/EXP
          VALTMP=DABS(DBLE(TEMP1(IINDX)) - EXP)
          VALTMP=(VALTMP - 0.5D0)**2/EXP
          CHISQ2=CHISQ2 + VALTMP
C
          WRITE(IOUNI1,IFORMT)I,J,ROWTOT(I),COLTOT(J),EXP,TEMP1(IINDX)
C1605     FORMAT(I8,I8,4E15.7)
C
 1610   CONTINUE
 1600 CONTINUE
      NROW=2
      NCOL=2
C
      GOTO4000
C
C               ********************************************
C               **  STEP 20--                             **
C               **  VARIABLE  CASE                        **
C               ********************************************
C
 2000 CONTINUE
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2101)
 2101   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 1 ',
     1         'IS NON-POSITIVE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N1
 2103   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2106)
 2106   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 2 ',
     1         'IS NON-POSITIVE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************************************
C               **  STEP 2.2--                                      **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (Y1, Y2).               **
C               ******************************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DISTIN(Y1,N1,IWRITE,XIDTEM,NUMSE1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(Y2,N2,IWRITE,XIDTE2,NUMSE2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
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
        GOTO9000
      ENDIF
C
      IF(NUMSE2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)
 2204   FORMAT('      NUMBER OF SETS    NUMSE2 = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      AN1=N1
      AN2=N2
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
C
C               ***********************************************
C               **  STEP 2.3--                               **
C               **  COMPUTE THE CHI-SQUARE STATISTIC         **
C               ***********************************************
C
      ISTEPN='23'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
C     COMPUTE COUNTS FOR EACH CELL
C
      J=0
      DO2310ISET1=1,NUMSE1
        DO2320ISET2=1,NUMSE2
C
          K=0
          DO2330I=1,N1
            IF(XIDTEM(ISET1).EQ.Y1(I).AND.XIDTE2(ISET2).EQ.Y2(I))THEN
C
              K=K+1
            ENDIF
 2330     CONTINUE
          NTEMP=K
          J=J+1
          TEMP1(J)=REAL(K)
          TEMP2(J)=XIDTEM(ISET1)
          TEMP3(J)=XIDTE2(ISET2)
C
 2320   CONTINUE
 2310 CONTINUE
      NTEMP2=J
C
C     COMPUTE ROW AND COLUMN TOTALS AND GRAND TOTAL.
C
      J=0
      GTOTAL=0.0D0
C
      DO2340ISET1=1,NUMSE1
        ROWTOT(ISET1)=0.0D0
        DO2350ISET2=1,NUMSE2
          J=J+1
          ROWTOT(ISET1)=ROWTOT(ISET1) + DBLE(TEMP1(J))
          GTOTAL=GTOTAL + DBLE(TEMP1(J))
 2350   CONTINUE
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')THEN
          WRITE(ICOUT,2352)ISET1,ROWTOT(ISET1)
 2352     FORMAT('ISET1,ROWTOT(ISET1)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 2340 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')THEN
        WRITE(ICOUT,2355)GTOTAL
 2355   FORMAT('GTOTAL=',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO2360ISET2=1,NUMSE2
        COLTOT(ISET2)=0.0D0
        VALTMP=XIDTE2(ISET2)
        DO2370J=1,NTEMP2
          IF(TEMP3(J).EQ.XIDTE2(ISET2))THEN
            COLTOT(ISET2)=COLTOT(ISET2) + DBLE(TEMP1(J))
          ENDIF
 2370   CONTINUE
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')THEN
          WRITE(ICOUT,2372)ISET2,COLTOT(ISET2)
 2372     FORMAT('ISET2,COLTOT(ISET2)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 2360 CONTINUE
C
C     NOW COMPUTE THE CHI-SQUARE TEST STATISTIC
C
      CHISQ1=0.0D0
      CHISQ2=0.0D0
      J=0
C
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
C
      DO2380ISET1=1,NUMSE1
        DO2390ISET2=1,NUMSE2
          J=J+1
          EXP=ROWTOT(ISET1)*COLTOT(ISET2)/GTOTAL
          VALTMP=(DBLE(TEMP1(J)) - EXP)**2/EXP
          CHISQ1=CHISQ1 + VALTMP
          VALTMP=DABS(DBLE(TEMP1(J)) - EXP)
          VALTMP=(VALTMP - 0.5D0)**2/EXP
          CHISQ2=CHISQ2 + VALTMP
          WRITE(IOUNI1,IFORMT)ISET1,ISET2,ROWTOT(ISET1),COLTOT(ISET2),
     1                        EXP,TEMP1(J)
C2385     FORMAT(I8,I8,E15.7,E15.7,E15.7,E15.7)
 2390   CONTINUE
 2380 CONTINUE
      NROW=NUMSE1
      NCOL=NUMSE2
C
      GOTO4000
C
 3000 CONTINUE
C
C               ********************************************
C               **  STEP 31--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               **  ALL TABLE ENTRIES SHOULD BE           **
C               **  NON-NEGATIVE INTEGERS.  NEGATIVE      **
C               **  VALUES WILL BE FLAGGED AS ERRORS      **
C               **  WHILE NON-INTEGER VALUES WILL BE      **
C               **  ROUNDED TO NEAREST INTEGER.           **
C               **  SINCE WE ARE SCANNING TABLE, COMPUTE  **
C               **  ROW AND COLUMN TOTALS.                **
C               ********************************************
C
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IERROR='NO'
      NUMERR=0
      MAXERR=10
C
      DO3001I=1,NROW
        ROWTOT(I)=0.0D0
 3001 CONTINUE
      GTOTAL=0.0D0
C
      DO3010J=1,NCOL
        COLTOT(J)=0.0D0
        DO3020I=1,NROW
          IF(XMAT(I,J).LT.0.0)THEN
            NUMERR=NUMERR+1
            IF(NUMERR.GT.MAXERR)GOTO9000
            IERROR='YES'
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3021)I,J
 3021       FORMAT('      ROW ',I8,' AND COLUMN ',I8,
     1             ' OF THE INPUT TABLE')
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
C
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ********************************************
C               **  STEP 32--                             **
C               **  COMPUTE THE CHI-SQUARE TEST STATISTIC **
C               ********************************************
C
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CHISQ1=0.0D0
      CHISQ2=0.0D0
      ICNT=0
C
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
C
      DO3110J=1,NCOL
        DO3120I=1,NROW
          ICNT=ICNT+1
          EXP=ROWTOT(I)*COLTOT(J)/GTOTAL
          VALTMP=(DBLE(XMAT(I,J)) - EXP)**2/EXP
          CHISQ1=CHISQ1 + VALTMP
          VALTMP=DABS(DBLE(XMAT(I,J)) - EXP)
          VALTMP=(VALTMP - 0.5D0)**2/EXP
          CHISQ2=CHISQ2 + VALTMP
          WRITE(IOUNI1,IFORMT)I,J,ROWTOT(I),COLTOT(J),EXP,XMAT(I,J)
C3115     FORMAT(2I8,4E15.7)
 3120   CONTINUE
 3110 CONTINUE
C
      AN1=REAL(GTOTAL)
      AN2=REAL(GTOTAL)
C
      GOTO4000
C
C               ********************************************
C               **  STEP 41--                             **
C               **  FOR ALL INPUT METHODS (SCALAR,        **
C               **  TWO VARIABLES, TABLE), COMPUTE THE    **
C               **  CRITIVAL VALUES AND PRINT THE RESULTS.**
C               ********************************************
C
 4000 CONTINUE
C
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      STATVA=CHISQ1
      STATV2=CHISQ2
C
      IDF=(NROW-1)*(NCOL-1)
      CALL CHSCDF(STATVA,IDF,CDF)
      CALL CHSCDF(STATV2,IDF,CDF2)
C
      IWRITE='OFF'
C
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
C
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
C
      IF(0.000.LE.CDF.AND.CDF.LE.0.50)ICONC1='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.80)ICONC2='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.90)ICONC3='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.95)ICONC4='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.975)ICONC5='ACCEPT'
      IF(0.000.LE.CDF.AND.CDF.LE.0.99)ICONC6='ACCEPT'
C
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.50)KCONC1='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.80)KCONC2='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.90)KCONC3='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.95)KCONC4='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.975)KCONC5='ACCEPT'
      IF(0.000.LE.CDF2.AND.CDF2.LE.0.99)KCONC6='ACCEPT'
C
C               ******************************
C               **   STEP 42--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR CHI-SQUARE   TEST  **
C               ******************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')
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
      ITITLE='Chi-Square Test for Independence (RxC Table)'
      NCTITL=44
      ITITLZ=' '
      NCTITZ=0
C
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
C
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
C
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
C
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
C
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
      ITITLE(1:25)='Without Yates Correction:'
      NCTITL=25
      ITITL9=' '
      NCTIT9=0
C
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
C
      NMAX=0
      NUMCOL=5
      DO7410I=1,NUMCOL
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
C
        DO7489J=1,NUMALP
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
C
 7410 CONTINUE
C
      ICNT=NUMALP
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
      ITITLE(1:30)='With Yates Bias Correction:'
      NCTITL=30
C
      NUMCOL=5
      DO7510I=1,NUMCOL
C
        DO7589J=1,NUMALP
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
C
 7510 CONTINUE
C
      ICNT=NUMALP
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
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DPCHWI(IHARG,IARGT,ARG,NUMARG,
     1                  PDEFWI,
     1                  MAXCHA,
     1                  PCHAWI,PCHAHG,
     1                  IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER WIDTHS FOR USE IN MULTI-TRACE PLOTS.
C              THE WIDTH FOR THE CHARACTER FOR THE I-TH TRACE
C              WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE FLOATING POINT
C              VECTOR PCHAWI(.).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --PDEFWI
C                     --MAXCHA
C     OUTPUT ARGUMENTS--PCHAWI  (A  FLOATING POINT VECTOR
C                       WHOSE I-TH ELEMENT IS THE WIDTH (= WIDTHT)
C                       FOR THE CHARACTER
C                       ASSIGNED TO THE I-TH    TRACE    IN
C                       A MULTI-TRACE PLOT.
C                     --PCHAWI = CHARACTER WIDTH
C                     --PCHAHG = HORIZONTAL GAP BETWEEN CHARACTERS
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
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
      DIMENSION PCHAWI(*)
      DIMENSION PCHAHG(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'WIDTH'.AND.
     1IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')GOTO2110
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'WIDT'.AND.
     1IHARG(2).EQ.'BOX'.AND.IHARG(3).EQ.'PLOT')GOTO2110
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'WIDTH'.AND.
     1IHARG(3).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')GOTO2110
      IF(NUMARG.GE.4.AND.IHARG(2).EQ.'WIDT'.AND.
     1IHARG(3).EQ.'BOX'.AND.IHARG(4).EQ.'PLOT')GOTO2110
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'WIDTH')GOTO1160
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'WIDT')GOTO1160
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'WIDTH')GOTO1105
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'WIDT')GOTO1105
      GOTO2199
C
 1105 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1110
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
C
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ALL')GOTO1160
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'ALL')GOTO1130
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'ALL')GOTO1140
C
      GOTO1150
C
 1110 CONTINUE
      DO1115I=1,MAXCHA
      PCHAWI(I)=PDEFWI
 1115 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAWI(I)
 1116 FORMAT('ALL CHARACTER WIDTHS HAVE JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO2190
C
 1120 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GOTO1180
      PCHAWI(1)=ARG(2)
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1126)I,PCHAWI(I)
 1126 FORMAT('THE WIDTH FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO2190
C
 1130 CONTINUE
      I=1
      IF(IARGT(3).NE.'NUMB')GOTO1180
      DO1135I=1,MAXCHA
      PCHAWI(I)=ARG(3)
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO2190
C
 1140 CONTINUE
      I=1
      IF(IARGT(2).NE.'NUMB')GOTO1180
      DO1145I=1,MAXCHA
      PCHAWI(I)=ARG(2)
 1145 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO2190
C
 1150 CONTINUE
      IMAX=NUMARG-1
      IF(MAXCHA.LT.IMAX)IMAX=MAXCHA
      DO1155I=1,IMAX
      IP1=I+1
      IF(IARGT(IP1).NE.'NUMB')GOTO1180
      PCHAWI(I)=ARG(IP1)
 1155 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1156I=1,IMAX
      WRITE(ICOUT,1126)I,PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1156 CONTINUE
 1159 CONTINUE
      GOTO2190
C
 1160 CONTINUE
      DO1165I=1,MAXCHA
      PCHAWI(I)=PDEFWI
 1165 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1116)PCHAWI(I)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GOTO2190
C
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
      GOTO2199
C
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
      GOTO2170
C
 2170 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO2179
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO2175I=1,IMAX
      WRITE(ICOUT,2176)I,PCHAWI(I)
 2176 FORMAT('THE WIDTH FOR CHARACTER ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 2175 CONTINUE
 2179 CONTINUE
      GOTO2180
C
 2180 CONTINUE
      IFOUND='YES'
      GOTO2190
C
 2190 CONTINUE
      IFOUND='YES'
      DO2191I=1,MAXCHA
      PCHAHG(I)=PCHAWI(I)*0.25
 2191 CONTINUE
C
 2199 CONTINUE
      RETURN
      END
      SUBROUTINE DPCMAP(IHARG,NUMARG,IDCMAP,ICHMAP,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PLOT CHARACTER MAPPING
C              (BY RANK    OR    BY EXACT)
C              WHICH LINKS TRACE ID AND CHARACTER
C              (THE CURRENT DEFAULT IS BY RANK).
C     EXAMPLE--IF HAVE DATA: X: 1 1 2 2 3 3
C                            Y: 1 2 3 4 5 6
C                          TAG: 1 1 3 3 5 5
C              AND CHARACTERS 1 2 3 4 5
C              AND DESIRE TO HAVE THE TRACES SHOW 1 3 AND 5
C              THEN CURRENTLY BY DEFAULT WOULD GET
C              TRACES SHOWING 1 2 3 (SINCE MAP VIA RANK)
C              BUT IF ENTER      CHARACTER MAP EXACT
C              THEN WOULD GET TRACES SHOWING 1 3 5 (AS DESIRED)
C     COMMAND EXAMPLE--CHARACTER MAP RANK (= DEFAULT)
C                      CHARACTER MAP EXACT
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDCMAP
C     OUTPUT ARGUMENTS--ICHMAP  (A  CHARACTER VARIABLE
C                       WHICH DEFINES THE MAP
C                       (RANK OR EXAC)
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
C     VERSION NUMBER--94/12
C     ORIGINAL VERSION--DECEMBER  1994.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDCMAP
      CHARACTER*4 ICHMAP
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.1)THEN
         ICHMAP=IDCMAP
         GOTO1150
      ENDIF
C
      IF(NUMARG.GE.2)THEN
         IF(IHARG(NUMARG).EQ.'ON'.OR.
     1   IHARG(NUMARG).EQ.'OFF'.OR.
     1   IHARG(NUMARG).EQ.'AUTO'.OR.
     1   IHARG(NUMARG).EQ.'DEFA')THEN
            ICHMAP=IDCMAP
            GOTO1150
         ELSE IF(IHARG(NUMARG).EQ.'EXAC'.OR.
     1   IHARG(NUMARG).EQ.'1TO1')THEN
            ICHMAP='EXAC'
            GOTO1150
         ELSE IF(IHARG(NUMARG).EQ.'?')THEN
            GOTO1160
         ELSE
            ICHMAP=IHARG(2)
            GOTO1150
         ENDIF
      ENDIF
C
 1150 CONTINUE
      IF(IFEEDB.EQ.'ON')THEN
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1151)ICHMAP
 1151    FORMAT('THE CHARACTER MAPPING HAS JUST BEEN SET TO ',
     1   A4)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IFOUND='YES'
      GOTO9000
C
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
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPCONC(IHARG,NUMARG,IDEFCC,ICONCH,
     1                  IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE CONTINUE CHARACTOR WHICH MAY
C              BE USED TO CONTINUE A COMMAND TO A SECOND
C              LINE (NO MORE THAN 2 LINES ALLOWED)
C              ABOUT THE ONLY PLACE THIS IS NECCESSARY
C              IN DATAPLOT IS IN ENTERING TITLES, ESPECIALLY
C              IF MANY SHIFTS ARE INCLUDED FOR UPPER, LOWER CASE
C              AND SPECIAL SYMBOLS
C
C              THE CONTINUE CHARACTER CAN BE UP TO 4 CHARACTERS LONG
C
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFCC (A  CHARACTER VARIABLE)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--ICONCH (A CHARACTER VARIABLE)
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
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER 1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCC
      CHARACTER*4 ICONCH
      CHARACTER*4 IBUGS2
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGS2.EQ.'OFF')GOTO90
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
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1150
      GOTO1110
C
 1110 CONTINUE
      IF(NUMARG.LE.1)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFCC
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      ICONCH=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ICONCH
 1181 FORMAT('THE CONTINUE CHARACTER HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
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
C
      RETURN
      END

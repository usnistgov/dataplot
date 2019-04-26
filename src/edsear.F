       SUBROUTINE EDSEAR(IMARK,ICOLL1,ICOLL2,IBLASW,ISHIFN,IEXEIM,
     1ISEQNU,ICSEQN,NCSEQN,IPPLIN,IPPOFF,IERASW,IMANUF,IMODEL,
     1ILPOFF,
CCCCC THE FOLLOWING LINE WAS AUGMENTED JULY 1992
CCCCC1IHARLC,IHARL2)
     1IHARLC,IHARL2,STOPSW)
C
C     PURPOSE--THIS SUBROUTINES DETERMINES IF THE ENTERED COMMAND
C              IS IN THE EDIT DICTIONARY, AND BRANCHES ACCORDINGLY
C              SO AS TO CARRY OUT THE COMMAND.
C
C     THE EDIT COMMANDS SEARCHED FOR ARE AS FOLLOWS--
C
C
C           UNDO = UNDO FROM PREVIOUS TOP
C           .
C
C       NUMBERS ON/OFF
C     DC
C     DX
C     X
C     CALL
C     EA = EXTEND ALL
C
C     NEAT
C
C     VW
C     HW
C     LET
C     ON BRIEF & OFF BRIEF   TO FEEDBACK ON/OFF
C     ON TRACE & OFF TRACE
C     TIME
C LD = LOCATE AND DELETE
C LR = LOCATE AND RETAIN
C MARGIN
C FILE (TO BE EDITED)
C NEW FILE OR EXISTING FILE
C SPACE (SINGLE OR DOUBLE)
C JUSTIFICATION LEFT, RIGHT CENTER, BOTH
C     CENTER
C     INDENT
C HYPENATION
C FORMAT
C MATCH
C DOUBLE
C CALL
C A
C Z
C     MOVE
C     COPY WITH 4 ARGS
C      MATCH
C      LET
C           NA = PRINT NAME OF FILE BEING EDITED
C     DISPLAY LI = PRINT CURRENT LINE NUMBER
C     DISPLAY LE = PRINT CURRENT LINE LENGTH
C     DISPLAY " " = PRINT BLANK LINE
C     STAT
C     ALLOW
C     PROMPT
C STATUS
C  PRINT SCREENFUL (PS)
C     TRACE
C
C SET IBUG..
C SET ISUBRO ..
C PROBE
C DUMP
C     ASCII
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--DECEMBER  1985.
C     UPDATED         --DECEMBER  1988.   LOCATE (NEXT) PERIOD (ONLY)    LPER
C     UPDATED         --APRIL     1990.
C     UPDATED         --JULY      1992. ADD STOP SWITCH (STOPSW)
C     UPDATED         --MAY       1993. LOOP
C     UPDATED         --MAY       1993. IF
C     UPDATED         --DECEMBER  1994. PRINT PREVIOUS PAGE
C     UPDATED         --DECEMBER  1994. PRINT PAGE VIA / ? " '
C     UPDATED         --DECEMBER  1994. GENERAL UPDATING
C     UPDATED         --FEBRUARY  1995. SYNONYMS FOR DELETE
C     UPDATED         --FEBRUARY  1995. NAME CONFLICT BETWEEN FIRST
C                                       AND FIND
C
C---------------------------------------------------------------------
C
C
      CHARACTER*4 IGLOBA
C
      CHARACTER*4 IFEESV
C
      CHARACTER*1 IANS0
C
      CHARACTER*4 IBLASW
C
      CHARACTER*4 IEXEIM
C
CCCCC CHARACTER*4 IJUNSW
C
      CHARACTER*240 ICTEMP
C
      CHARACTER*10 ICSEQN
C
      CHARACTER*4 IERASW
C
      CHARACTER*4 IMANUF
      CHARACTER*4 IMODEL
C
      CHARACTER*4 IVALID
C
      CHARACTER*4 IHARLC
      CHARACTER*4 IHARL2
C
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1992
      CHARACTER*4 STOPSW
C
      CHARACTER*11 ICJU11
C
      CHARACTER*1 ICJUNK
C
C----------------------------------------------------------------------------
C
      DIMENSION IHARLC(*)
      DIMENSION IHARL2(*)
C
      DIMENSION IANS0(132)
C
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
CCCCC SEPTEMBER 1993.  ADD FOLLOWING LINE
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
   50 CONTINUE
      IF(IBUGED.EQ.'OFF'.AND.ISUBRO.NE.'SEAR')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF EDSEAR--')
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,52)ICURLN
   52 FORMAT('ICURLN = ',I8)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,53)IWIDTH
   53 FORMAT('IWIDTH = ',I8)
      CALL EDWRST('EDSEAR')
      IF(IWIDTH.GE.0)THEN
         WRITE(ICOUT,54)(IANS(I),I=1,IWIDTH)
   54    FORMAT('(IANS(I),I=1,IWIDTH) = ',100A1)
         CALL EDWRST('EDSEAR')
      ENDIF
      IF(IWIDTH.GE.0)CALL EDWRST('EDSEAR')
      WRITE(ICOUT,55)IBUGED,IBUGE2
   55 FORMAT('IBUGED,IBUGE2 = ',A4,2X,A4)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,61)IMARK
   61 FORMAT('IMARK = ',I8)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,62)ICOLL1,ICOLL2,IJUST
   62 FORMAT('ICOLL1,ICOLL2,IJUST = ',2I8,2X,A4)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,63)IEXEIM
   63 FORMAT('IEXEIM = ',A4)
      CALL EDWRST('EDSEAR')
CCCCC WRITE(ICOUT,70)INEGAT,IFOUND,IERROR
CCC70 FORMAT('INEGSW,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      WRITE(ICOUT,76)IDELSW,IDELLN
   76 FORMAT('IDELSW,IDELLN = ',A4,2X,I8)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,77)ICOM,ICOM2
   77 FORMAT('ICOM,ICOM2 = ',A4,2X,A4)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,81)NUMARG
   81 FORMAT('NUMARG = ',I8)
      CALL EDWRST('EDSEAR')
      DO82I=1,NUMARG
      WRITE(ICOUT,83)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   83 FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,2X,I8,E15.7)
      CALL EDWRST('EDSEAR')
   82 CONTINUE
   90 CONTINUE
C
      IFOUND='NO'
      IERROR='NO'
C
C
C               **********************************************
C               **  TREAT THE SPECIAL CASES UNDER A PRIOR DELETE
C               **********************************************
C
      IF(IDELSW.EQ.'OFF')GOTO190
C
      IF(ICOM.EQ.'PRINT'.AND.NUMARG.EQ.0)GOTO110
      IF(ICOM.EQ.'P'.AND.NUMARG.EQ.0)GOTO110
      IF(ICOM.EQ.'P2'.AND.NUMARG.EQ.0)GOTO110
C
      IF(ICOM.EQ.'PRINT'.AND.NUMARG.GE.1)GOTO120
      IF(ICOM.EQ.'P'.AND.NUMARG.GE.1)GOTO120
      IF(ICOM.EQ.'P2'.AND.NUMARG.GE.1)GOTO120
CCCCC IF(ICOM(1:2).EQ.'DE')GOTO120
      IF(ICOM.EQ.'D')GOTO120
CCCCC FEBRUARY 1995.  ADD FOLLOWING LINE BACK FOR SYNONYMS
      IF(ICOM(1:2).EQ.'DE')GOTO120
      IF(ICOM(1:2).EQ.'DH')GOTO120
      IF(ICOM.EQ.'HD')GOTO120
      IF(ICOM(1:2).EQ.'CH')GOTO120
      IF(ICOM.EQ.'C')GOTO120
      IF(ICOM(1:3).EQ.'EXT')GOTO120
      IF(ICOM.EQ.'DTL')GOTO120
      IF(ICOM.EQ.'HD')GOTO120
      IF(ICOM.EQ.'DH')GOTO120
      IF(ICOM.EQ.'HD')GOTO120
C
      IF(ICOM(1:3).EQ.'INS')GOTO130
      IF(ICOM.EQ.'IN')GOTO130
      IF(ICOM.EQ.'I')GOTO130
CCCCC IF(ICOM(1:2).EQ.'LO')GOTO130
      IF(ICOM.EQ.'L')GOTO130
      IF(ICOM.EQ.'LB')GOTO130
      IF(ICOM.EQ.'LBA')GOTO130
      IF(ICOM.EQ.'LO')GOTO130
      IF(ICOM.EQ.'LA')GOTO130
      IF(ICOM.EQ.'LBL')GOTO130
      IF(ICOM.EQ.'LOBL')GOTO130
      IF(ICOM.EQ.'LABL')GOTO130
      IF(ICOM.EQ.'LPER')GOTO130
      IF(ICOM.EQ.'LKIL')GOTO130
      IF(ICOM.EQ.'LR')GOTO130
      IF(ICOM.EQ.'LDEL')GOTO130
      IF(ICOM.EQ.'LLD')GOTO130
      IF(ICOM(1:2).EQ.'FI')GOTO130
      IF(ICOM.EQ.'F')GOTO130
      IF(ICOM(1:1).EQ.'N')GOTO130
      IF(ICOM.EQ.'    ')GOTO130
      IF(ICOM.EQ.'LI')GOTO130
      IF(ICOM.EQ.'LIB')GOTO130
      IF(ICOM.EQ.'LP')GOTO130
CCCC? IF(ICOM.EQ.'LC')GOTO130
C
      GOTO140
C
  110 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO119
CCCCC THE FOLLOWING 9 LINES WERE CHANGED MAY 1993
CCCCC IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
CCCCC    WRITE(ICOUT,113)
CC113    FORMAT('[DELETED LINE]')
CCCCC    CALL EDWRST('EDSEAR')
CCCCC ENDIF
CCCCC IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
CCCCC    WRITE(ICOUT,114)ICURLN
CC114    FORMAT(I6,':',3X,'[DELETED LINE]')
CCCCC    CALL EDWRST('EDSEAR')
CCCCC ENDIF
CCCCC THE FOLLOWING 4 LINES WERE ADDED MAY 1993
CCCCC ICURLN=ICURLN-1
      IDELSW='OFF'
      IDELLN=(-999)
      GOTO190
  119 CONTINUE
      IFOUND='YES'
      IERROR='NO'
      GOTO9000
C
  120 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO129
CCCCC THE FOLLOWING 9 LINES WERE CHANGED MAY 1993
CCCCC IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
CCCCC    WRITE(ICOUT,123)
CC123    FORMAT('[ERROR--OPERATING ON A DELETED LINE]')
CCCCC    CALL EDWRST('EDSEAR')
CCCCC ENDIF
CCCCC IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
CCCCC    WRITE(ICOUT,124)ICURLN
CC124    FORMAT(I6,':',3X,'[ERROR--OPERATING ON A DELETED LINE]')
CCCCC    CALL EDWRST('EDSEAR')
CCCCC ENDIF
CCCCC THE FOLLOWING 4 LINES WERE ADDED MAY 1993
CCCCC ICURLN=ICURLN-1
      IDELSW='OFF'
      IDELLN=(-999)
      GOTO190
  129 CONTINUE
      IFOUND='YES'
      IERROR='NO'
      GOTO9000
C
  130 CONTINUE
      ICURLN=ICURLN-1
      IDELSW='OFF'
      IDELLN=(-999)
      GOTO190
C
  140 CONTINUE
      IDELSW='OFF'
      IDELLN=(-999)
      GOTO190
C
  190 CONTINUE
C
C               **********************************************
C               **  TREAT THE EDIT CASE                     **
C               **  CHANGE FROM INPUT MODE TO EDIT  MODE    **
C               **********************************************
C
      IF(IWIDTH.NE.4)GOTO299
      IF(IANS(1).NE.'E'.AND.IANS(1).NE.'e')GOTO299
      IF(IANS(2).NE.'D'.AND.IANS(2).NE.'d')GOTO299
      IF(IANS(3).NE.'I'.AND.IANS(3).NE.'i')GOTO299
      IF(IANS(4).NE.'T'.AND.IANS(4).NE.'t')GOTO299
      GOTO220
C
  220 CONTINUE
      CALL EDEDIT
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
  299 CONTINUE
C
C               **********************************************
C               **  TREAT THE INPUT CASE                    **
C               **  CHANGE FROM EDIT  MODE TO INPUT MODE    **
C               **********************************************
C
      IF(ICOM(1:3).EQ.'INP')GOTO320
      GOTO399
C
  320 CONTINUE
      CALL EDINPU
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
  399 CONTINUE
C
C               **************************************************************
C               **  TREAT THE INPUT MODE INSERT CASE--                      **
C               **  INSERT A LINE WHILE IN INPUT MODE                       **
C               **  (OR ERASE THE SCREEN [IF ENTER CONTROL E = ASCII NUMERIC 5]
C               **  (THE ONLY 2 PRACTICAL ACTIVITIES YOU CAN DO IN INPUT MODE)
C               **************************************************************
C
      IF(IMODE.EQ.'INPU')GOTO420
      GOTO499
C
  420 CONTINUE
      IF(IWIDTH.GE.1)IASC=ICHAR(IANS(1))
      IF(IWIDTH.EQ.1.AND.IASC.EQ.5)CALL EDERAS(IMANUF,IMODEL)
      IF(IWIDTH.NE.1.OR.IASC.NE.5)CALL EDININ
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
  499 CONTINUE
C
C               ****************************
C               **  TREAT THE DEFINE CASE **
C               ****************************
C
      IF(ICOM.EQ.'DEFI')GOTO1020
      GOTO1099
C
 1020 CONTINUE
      CALL EDDEFI(IHARLC,IHARL2)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1099 CONTINUE
C
C               ****************************
C               **  TREAT THE PRINT CASE  **
C               ****************************
C
      IF(ICOM(1:2).EQ.'PR')GOTO1120
      IF(ICOM.EQ.'P')GOTO1120
      IF(ICOM.EQ.'P2')GOTO1120
      GOTO1199
C
 1120 CONTINUE
      IF(ICOM.EQ.'P2')IPRISW='ON'
      CALL EDPRIN
      IF(ICOM.EQ.'P2')IPRISW='OFF'
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1199 CONTINUE
C
C               ***********************************************************
C               **  TREAT THE PRINT (STARTING WITH NEXT LINE) CASE CASE  **
C               ***********************************************************
C
C
      IF(ICOM.EQ.'PN')GOTO1210
      GOTO1299
C
 1210 CONTINUE
      IPRISW='ON'
      CALL EDPRNE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1299 CONTINUE
C
C               *****************************
C               **  TREAT THE LOCATE CASE  **
C               *****************************
C
CCCCC IF(ICOM(1:2).EQ.'LO')GOTO1320
      IF(ICOM.EQ.'L')GOTO1320
      IF(ICOM.EQ.'LO')GOTO1320
CCCCC FEBRUARY 1995.  ADD LOC, LOCA AS SYNONYMS
      IF(ICOM(1:3).EQ.'LOC')GOTO1320
      IF(ICOM.EQ.'LA')GOTO1320
      GOTO1399
C
 1320 CONTINUE
      IALL='OFF'
      IF(ICOM.EQ.'LA')IALL='ON'
      CALL EDLOCA
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1399 CONTINUE
CCCCC FEBRUARY 1995.  NAME CONFLICT WITH FIND.  MOVE THIS COMMAND
CCCCC TO BEFORE FIND (SO CAN STILL USE F AND FI AS SYNONYMS FOR FIND).
C
C               ****************************
C               **  TREAT THE FIRST CASE  **
C               ****************************
C
      IF(ICOM.EQ.'FIRS')GOTO4220
      IF(ICOM.EQ.'FIR')GOTO4220
CCCCC IF(ICOM.EQ.'FI')GOTO4220
      GOTO4299
C
 4220 CONTINUE
CCCCC IF(ICURLN.GE.2)CALL EDWRWF(ID)
      CALL EDFIRS
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4299 CONTINUE
C
C               ****************************
C               **  TREAT THE FIND  CASE  **
C               ****************************
C
      IF(ICOM(1:2).EQ.'FI')GOTO1420
      IF(ICOM.EQ.'F')GOTO1420
      GOTO1499
C
 1420 CONTINUE
      CALL EDFIND
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1499 CONTINUE
C
C               ****************************************
C               **  TREAT THE PRINT TILL LOCATE CASE  **
C               ****************************************
C
      IF(ICOM.EQ.'PL')GOTO1520
      IF(ICOM.EQ.'PTL')GOTO1520
      GOTO1599
C
 1520 CONTINUE
      IL1=ICURLN
      ICOM='LOCA'
      ICOM2='TE  '
      IFEESV=IFEESW
      IFEESW='OFF'
      CALL EDLOCA
      IL2=ICURLN
C
      ICOM='PRIN'
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1990
CCCCC ICOM2='T    '
      ICOM2='T   '
      NUMARG=2
      IARG(1)=IL1
      IARG(2)=IL2
      IARGT(1)='NUMB'
      IARGT(2)='NUMB'
      IFEESW=IFEESV
      CALL EDPRIN
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1599 CONTINUE
C
C               *********************************
C               **  TREAT THE PRINT PAGE CASE  **
C               *********************************
C
      IF(ICOM.EQ.'PP')GOTO1620
      IF(ICOM.EQ.'PP2')GOTO1620
      GOTO1699
C
 1620 CONTINUE
      IF(ICOM.EQ.'PP2')IPRISW='ON'
      CALL EDPP(IPPLIN,IPPOFF)
      IF(ICOM.EQ.'PP2')IPRISW='OFF'
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 1699 CONTINUE
C
C               *****************************
C               **  TREAT THE CHANGE CASE  **
C               *****************************
C
      IF(ICOM.EQ.'CA')THEN
         IGLOBA='ON'
         CALL EDCHAN(IGLOBA)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IF(ICOM(1:2).EQ.'CH'.OR.ICOM.EQ.'C')THEN
         IGLOBA='OFF'
         CALL EDCHAN(IGLOBA)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ************************************
C               **  TREAT THE CHANGE ALL CASE     **
C               ************************************
C
      IF(ICOM.EQ.'CG')THEN
         IGLOBA='ON'
         CALL EDCHAN(IGLOBA)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE DELETE CASE  **
C               *****************************
C
CCCCC IF(ICOM(1:2).EQ.'DE')GOTO2220
CCCCC FEBRUARY 1995.  ACCEPT SYNONYMS.
CCCCC IF(ICOM.EQ.'D')THEN
      IF(ICOM.EQ.'D'.OR.ICOM(1:2).EQ.'DE')THEN
         CALL EDDELE
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE INSERT CASE  **
C               *****************************
C
      IF(ICOM(1:3).EQ.'INS'.OR.
     1ICOM.EQ.'IN'.OR.
     1ICOM.EQ.'I')THEN
         CALL EDINSE
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC           ******************************
CCCCC           **  TREAT THE TRANSLATE CASE  **
CCCCC           ******************************
CCCCC
CCCCC IF(ICOM(1:3).EQ.'TRA')GOTO2420
CCCCC GOTO2499
CCCCC
C2420 CONTINUE
CCCCC CALL EDTRAN
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C2499 CONTINUE
CCCCC
C
CCCC FEBRUARY 1995.  MOVE TO BEFORE EXTEND CASE TO AVOID
CCCC NAME CONFLICT WITH EXTEND COMMAND.
C               *****************************************
C               **  TREAT THE EXTEND ALL CASE          **
C               **  EA     EXTALL                      **
C               *****************************************
C
      IF(ICOM.EQ.'EA'.OR.ICOM.EQ.'EXTA')THEN
         ICOM='EA  '
         ICOM2='    '
         CALL EDEA
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE EXTEND CASE  **
C               *****************************
C
      IF(ICOM(1:3).EQ.'EXT')THEN
         CALL EDEXTE
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE CARRIAGE RETURN CASE  **
C               ** = NEXT                           **
C               *****************************
C
      IF(ICOM.EQ.'    ')THEN
         ICOM='N   '
         NUMARG=0
         CALL EDNEXT
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION IS NEW      DECEMBER 1994
C               **********************************
C               **  TREAT THE DELETE WORD CASE  **
C               **********************************
C
      IF(ICOM.EQ.'DW')THEN
CCCCC    CALL EDDW
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************************
C               **  TREAT THE DELETE TILL LOCATE CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'DTL')GOTO2620
      GOTO2699
C
 2620 CONTINUE
      IFEESV=IFEESW
C
      IL1=ICURLN
      ICOM='LOCA'
      ICOM2='TE  '
      IFEESW='OFF'
      CALL EDLOCA
CCCCC IL2=ICURLN
      IL2=ICURLN-1
C
      ICOM='DELE'
      ICOM2='TE  '
      NUMARG=2
      IARG(1)=IL1
      IARG(2)=IL2
      IARGT(1)='NUMB'
      IARGT(2)='NUMB'
      IFEESW=IFEESV
      CALL EDDELE
C
      ICOM='PRIN'
      ICOM2='T   '
      NUMARG=0
      IFEESW=IFEESV
      CALL EDPRIN
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 2699 CONTINUE
C
C               *****************************************
C               **  TREAT THE LOCATE AND INSERT CASE   **
C               *****************************************
C
      IF(ICOM.EQ.'LI')GOTO2720
      GOTO2799
C
 2720 CONTINUE
C
      IWIDT0=IWIDTH
      DO2730I=1,IWIDTH
      IANS0(I)=IANS(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,2731)I,IANS(I),IANS0(I)
 2731    FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
 2730 CONTINUE
C
      DO2740IDUMMY=1,100000
C
      IANS(1)='L'
      IANS(2)='O'
      IANS(3)=' '
      IWIDTH=2
      J=3
      IF(IWIDT0.LT.4)GOTO2758
      DO2750I=4,IWIDT0
      IF(IANS0(I).EQ.' ')IWIDTH=J
      IF(IANS0(I).EQ.' ')GOTO2758
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,2751)I,J,IANS0(I),IANS(J)
 2751 FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
 2750 CONTINUE
      IWIDTH=J
 2758 CONTINUE
      ICOM='LO'
      CALL EDLOCA
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,2759)ICURLN,NUMLIN
 2759    FORMAT('ICURLN,NUMLIN = ',2I8)
         CALL EDWRST('EDSEAR')
      ENDIF
      IF(ICURLN.GT.NUMLIN)GOTO2779
C
      IANS(1)='I'
      IANS(2)='N'
      IANS(3)=' '
      IARGUM=1
      J=3
      IF(IWIDT0.LT.4)GOTO2765
      DO2760I=4,IWIDT0
      IF(IANS0(I).EQ.' ')IARGUM=IARGUM+1
      IF(IARGUM.EQ.1)GOTO2760
      IF(IARGUM.EQ.2.AND.IANS0(I).EQ.' ')GOTO2760
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,2761)I,J,IANS0(I),IANS(J)
 2761    FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
 2760 CONTINUE
 2765 CONTINUE
      IF(IARGUM.EQ.1)GOTO2766
      GOTO2769
 2766 CONTINUE
      J=J+1
      IANS(J)=' '
 2769 CONTINUE
      NUMARG=IARGUM-1
      IWIDTH=J
      ICOM='IN'
      CALL EDINSE
C
 2740 CONTINUE
C
 2779 CONTINUE
C
      IWIDTH=IWIDT0
      DO2780I=1,IWIDT0
      IANS(I)=IANS0(I)
 2780 CONTINUE
      ICOM='LI'
C
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 2799 CONTINUE
C
C               *****************************************
C               **  TREAT THE IB (INSERT BEFORE) CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'IB')GOTO2820
      IF(ICOM.EQ.'UI')GOTO2820
      GOTO2899
C
 2820 CONTINUE
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,2821)ICURLN,NUMLIN
 2821    FORMAT('***** FROM EDSEAR--ICURLN,NUMLIN = ',2I8)
         CALL EDWRST('EDSEAR')
      ENDIF
C
      NUMASV=NUMARG
      IFEESV=IFEESW
C
      ICOM='UP  '
      ICOM2='    '
      NUMARG=0
      IFEESW='OFF'
      CALL EDUP
      IF(IERROR.EQ.'YES')GOTO9000
C
      ICOM='IN  '
      ICOM2='    '
      NUMARG=1
      IFEESW=IFEESV
      CALL EDINSE
      IF(IERROR.EQ.'YES')GOTO9000
C
      ICOM='IB  '
      ICOM2='    '
      NUMARG=NUMASV
      IFEESW=IFEESV
CCCCC ICURLN=ICURLN-1
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 2899 CONTINUE
C
C               *****************************
C               **  TREAT THE EXIT   CASE  **
C               *****************************
C
      IF(ICOM.EQ.'EXIT'.OR.
     1ICOM.EQ.'EXI'.OR.
     1ICOM.EQ.'EX'.OR.
     1ICOM.EQ.'E'.OR.
     1ICOM.EQ.'END'.OR.
     1ICOM.EQ.'STOP'.OR.
     1ICOM.EQ.'HALT'.OR.
     1ICOM.EQ.'QUIT'.OR.
     1ICOM.EQ.'BYE'.OR.
     1ICOM.EQ.'EXRR')THEN
CCCCC    THE FOLLOWING LINE WAS AUGMENTED JULY 1992
CCCCC    CALL EDEXIT
         CALL EDEXIT(STOPSW)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE ABORT  CASE  **
C               *****************************
C
      IF(ICOM.EQ.'AB'.OR.
     1ICOM(1:3).EQ.'ABO'.OR.
     1ICOM.EQ.'ABRR')THEN
CCCCC    THE FOLLOWING LINE WAS AUGMENTED JULY 1992
CCCCC    CALL EDABOR
         CALL EDABOR(STOPSW)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE UNDO   CASE  **
C               *****************************
C
      IF(ICOM.EQ.'UNDO')THEN
         CALL EDUNDO
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE TOP   CASE  **
C               ****************************
C
      IF(ICOM.EQ.'TOP')GOTO4120
      IF(ICOM.EQ.'TO')GOTO4120
      IF(ICOM.EQ.'T')GOTO4120
      GOTO4199
C
 4120 CONTINUE
CCCCC IF(ICURLN.GE.1)CALL EDWRWF(ID)
      CALL EDTOP
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4199 CONTINUE
CCCCC FEBRUARY 1995.  NAME CONFLICT WITH FIRST.  MOVE THIS COMMAND
CCCCC TO BEFORE FIND (SO CAN STILL USE F AND FI AS SYNONYMS FOR FIND).
C
C               ****************************
C               **  TREAT THE FIRST CASE  **
C               ****************************
C
CCCCC IF(ICOM.EQ.'FIRS')GOTO4220
CCCCC IF(ICOM.EQ.'FIR')GOTO4220
CCCCC IF(ICOM.EQ.'FI')GOTO4220
CCCCC GOTO4299
C
C4220 CONTINUE
CCCCC IF(ICURLN.GE.2)CALL EDWRWF(ID)
CCCCC CALL EDFIRS
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
C4299 CONTINUE
C
C               ****************************
C               **  TREAT THE GO    CASE  **
C               ****************************
C
      IF(ICOM.EQ.'GO'.OR.ICOM.EQ.'G')THEN
CCCCC    IF(NUMARG.GE.1.AND.IARGT(NUMARG).EQ.'NUMB'.AND.
CCCCC1   IARG(NUMARG).LT.ICURLN)CALL EDWRWF(ID)
         CALL EDGO(IMARK)
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
 
C               *******************************
C               **  TREAT THE <NUMBER> CASE  **
C               *******************************
C
      IF(ICOM.EQ.'.')GOTO4499
      IF(ICOMT.EQ.'NUMB'.AND.ICOM.NE.'    ')GOTO4420
      GOTO4499
C
 4420 CONTINUE
CCCCC IF(ICOMI.LT.ICURLN)CALL EDWRWF(ID)
      CALL EDGONU
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 4499 CONTINUE
C
C               ****************************
C               **  TREAT THE LAST  CASE  **
C               ****************************
C
      IF(ICOM(1:3).EQ.'LAS')THEN
CCCCC    IF(ICURLN.GT.NUMLIN)CALL EDWRWF(ID)
         CALL EDLAST
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ******************************
C               **  TREAT THE BOTTOM  CASE  **
C               ******************************
C
      IF(ICOM(1:1).EQ.'B')THEN
         CALL EDBOTT
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ****************************
C               **  TREAT THE UP    CASE  **
C               ****************************
C
      IF(ICOM.EQ.'UP'.OR.ICOM.EQ.'U')THEN
CCCCC    IF(NUMARG.GE.1)CALL EDWRWF(ID)
         CALL EDUP
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C               ****************************
C               **  TREAT THE NEXT  CASE  **
C               ****************************
C
      IF(ICOM.EQ.'N'.OR.ICOM.EQ.'NEXT'.OR.ICOM(1:2).EQ.'DO')THEN
         CALL EDNEXT
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE HOLD   CASE  **
C               *****************************
C
      IF(ICOM(1:2).EQ.'HO'.OR.ICOM.EQ.'H')THEN
         CALL EDHOLD
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ***************************************
C               **  TREAT THE HOLD AND DELETE  CASE  **
C               ***************************************
C
      IF(ICOM.EQ.'HD'.OR.ICOM(1:2).EQ.'DH')THEN
         CALL EDHOLD
         NUMARG=0
         CALL EDDELE
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               ********************************
C               **  TREAT THE DUPLICATE CASE  **
C               ********************************
C
      IF(ICOM(1:2).EQ.'DU')THEN
         CALL EDDUPL
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *****************************
C               **  TREAT THE COPY    CASE **
C               **  COPY                   **
C               **  COPY  JUNK.            **
C               **  COPY  5 7              **
C               **  COPY  JUNK. 5 7        **
C               **  COPY  5 7 10           **
C               *****************************
C
      IF(ICOM.EQ.'COPY')GOTO5720
      IF(ICOM.EQ.'COP')GOTO5720
      IF(ICOM.EQ.'CO')GOTO5720
      GOTO5799
C
 5720 CONTINUE
      IF(NUMARG.LE.1)GOTO5721
      IF(NUMARG.EQ.2)GOTO5722
      IF(NUMARG.EQ.3.AND.IARGT(1).EQ.'WORD')GOTO5723
      GOTO5724
C
 5721 CONTINUE
      CALL EDCOP1
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5799
C
 5722 CONTINUE
      CALL EDCOP2
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5799
C
 5723 CONTINUE
      CALL EDCOP2
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5799
C
 5724 CONTINUE
      CALL EDCOP3
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5799
C
 5799 CONTINUE
C
C               *****************************
C               **  TREAT THE DCOPY   CASE **
C               **  (IDENTICAL TO MOVE)    **
C               **  DCOPY                  **
C               **  DCOPY JUNK.            **
C               **  DCOPY 5 7              **
C               **  DCOPY JUNK. 5 7        **
C               **  DCOPY 5 7 10           **
C               **  MOVE                   **
C               **  MOVE  JUNK.            **
C               **  MOVE  5 7              **
C               **  MOVE  JUNK. 5 7        **
C               **  MOVE  5 7 10           **
C               *****************************
C
      IF(ICOM.EQ.'DCOP')GOTO5820
      IF(ICOM.EQ.'MOVE')GOTO5820
      IF(ICOM.EQ.'MOV')GOTO5820
      IF(ICOM.EQ.'MO')GOTO5820
      IF(ICOM.EQ.'M')GOTO5820
      GOTO5899
C
 5820 CONTINUE
      IF(NUMARG.LE.1)GOTO5821
      IF(NUMARG.EQ.2)GOTO5822
      IF(NUMARG.EQ.3.AND.IARGT(1).EQ.'WORD')GOTO5823
      GOTO5824
C
 5821 CONTINUE
      CALL EDCOP1
      IF(IFOUND.EQ.'NO')GOTO5899
      IF(IERROR.EQ.'YES')GOTO9000
      ITEMP1=IBLIM1
      ITEMP2=IBLIM2
      GOTO5830
C
 5822 CONTINUE
      CALL EDCOP2
      IF(IFOUND.EQ.'NO')GOTO5899
      IF(IERROR.EQ.'YES')GOTO9000
      ITEMP1=IARG(1)
      ITEMP2=IARG(2)
      GOTO5830
C
 5823 CONTINUE
      CALL EDCOP2
      IF(IFOUND.EQ.'NO')GOTO5899
      IF(IERROR.EQ.'YES')GOTO9000
      ITEMP1=IARG(2)
      ITEMP2=IARG(3)
      GOTO5830
C
 5824 CONTINUE
      CALL EDCOP3
      IF(IFOUND.EQ.'NO')GOTO5899
      IF(IERROR.EQ.'YES')GOTO9000
      ITEMP1=IARG(1)
      ITEMP2=IARG(2)
      ITEMP3=IARG(3)
      IFUDGE=0
      IF(ITEMP3.LT.ITEMP1)IFUDGE=ITEMP2-ITEMP1+1
      ITEMP1=ITEMP1+IFUDGE
      ITEMP2=ITEMP2+IFUDGE
      GOTO5830
C
 5830 CONTINUE
      ICOM='DELE'
      ICOM2='TE  '
      NUMARG=2
      IARG(1)=ITEMP1
      IARG(2)=ITEMP2
      IARGT(1)='NUMB'
      IARGT(2)='NUMB'
      CALL EDDELE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
      GOTO5899
C
 5899 CONTINUE
C
C               *****************************
C               **  TREAT THE ADD    CASE  **
C               *****************************
C
      IF(ICOM(1:2).EQ.'AD')GOTO5920
      GOTO5999
C
 5920 CONTINUE
      CALL EDADD
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 5999 CONTINUE
C
C               ****************************
C               **  TREAT THE SCALE CASE  **
C               ****************************
C
      IF(ICOM.EQ.'SCAL')GOTO7020
      IF(ICOM.EQ.'SCA')GOTO7020
CCCCC IF(ICOM.EQ.'SC')GOTO7020
      GOTO7099
C
 7020 CONTINUE
      CALL EDSCAL
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7099 CONTINUE
C
C               ***************************************
C               **  TREAT THE . CASE                 **
C               **  (NULL COMMAND, E.G., A COMMENT)  **
C               ***************************************
C
      IF(ICOM.EQ.'.')GOTO7120
CCCCC IF(ICOM.EQ.'    ')GOTO7120
      GOTO7199
C
 7120 CONTINUE
      CALL EDPERI
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7199 CONTINUE
C
C               *****************************
C               **  TREAT THE HELP   CASE  **
C               *****************************
C
      IF(ICOM(1:2).EQ.'HE')GOTO7220
      GOTO7299
C
 7220 CONTINUE
      CALL EDHELP
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7299 CONTINUE
C
C               *****************************
C               **  TREAT THE SET    CASE  **
C               *****************************
C
      IF(ICOM.EQ.'SET')GOTO7350
      IF(ICOM.EQ.'SE')GOTO7350
      IF(ICOM.EQ.'S')GOTO7350
      IF(ICOM.EQ.'SC1')GOTO7310
      IF(ICOM.EQ.'SC2')GOTO7320
      IF(ICOM.EQ.'SC')GOTO7330
      IF(ICOM.EQ.'MARK')GOTO7330
      IF(ICOM.EQ.'SM')GOTO7340
      GOTO7399
C
 7310 CONTINUE
      IF(NUMARG.LE.0)GOTO7311
      IARG(2)=IARG(1)
 7311 CONTINUE
      ICOM='SET '
      ICOM2='    '
      IHARG(1)='BEGI'
      IHARG2(1)='    '
      NUMARG=NUMARG+1
      GOTO7350
C
 7320 CONTINUE
      IF(NUMARG.LE.0)GOTO7321
      IARG(2)=IARG(1)
 7321 CONTINUE
      ICOM='SET '
      ICOM2='    '
      IHARG(1)='END '
      IHARG2(1)='    '
      NUMARG=NUMARG+1
      GOTO7350
C
 7330 CONTINUE
      IF(NUMARG.LE.0)GOTO7331
      IARG(3)=IARG(2)
      IARG(2)=IARG(1)
 7331 CONTINUE
      ICOM='SET '
      ICOM2='    '
      IHARG(1)='COPY'
      IHARG2(1)='    '
      NUMARG=NUMARG+1
      GOTO7350
C
 7340 CONTINUE
      IF(NUMARG.LE.0)GOTO7341
      IARG(2)=IARG(1)
 7341 CONTINUE
      ICOM='SET '
      ICOM2='    '
      IHARG(1)='MARK'
      IHARG2(1)='    '
      NUMARG=NUMARG+1
      GOTO7350
C
 7350 CONTINUE
      CALL EDSET(IMARK,ICOLL1,ICOLL2,ISHIFN,IPPLIN,IPPOFF,IERASW,
     1ISEQNU,ILPOFF)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7399 CONTINUE
C
C               *****************************
C               **  TREAT THE SHOW   CASE  **
C               *****************************
C
      IF(ICOM.EQ.'SHOW')GOTO7420
      IF(ICOM.EQ.'SHO')GOTO7420
      IF(ICOM.EQ.'SH')GOTO7420
      GOTO7499
C
 7420 CONTINUE
      CALL EDSHOW
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7499 CONTINUE
C
C               *****************************
C               **  TREAT THE STATUS CASE  **
C               *****************************
C
      IF(ICOM.EQ.'STAT')GOTO7520
      IF(ICOM.EQ.'STA')GOTO7520
      IF(ICOM.EQ.'ST')GOTO7520
      GOTO7599
C
 7520 CONTINUE
      CALL EDSTAT
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 7599 CONTINUE
C
C               ******************************
C               **  TREAT THE TRUNCATE CASE **
C               ******************************
C
      IF(ICOM(1:3).EQ.'TRU')GOTO8120
      IF(ICOM.EQ.'TR')GOTO8120
      GOTO8199
C
 8120 CONTINUE
      CALL EDTRUN
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 8199 CONTINUE
C
C               ******************************
C               **  TREAT THE CENTER   CASE **
C               ******************************
C
      IF(ICOM(1:2).EQ.'CE')GOTO8420
      GOTO8499
C
 8420 CONTINUE
      CALL EDCENT
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 8499 CONTINUE
C
C               ******************************
C               **  TREAT THE INDENT   CASE **
C               ******************************
C
      IF(ICOM(1:3).EQ.'IND')GOTO8520
      GOTO8599
C
 8520 CONTINUE
      CALL EDINDE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 8599 CONTINUE
C
C               ******************************
C               **  TREAT THE ERASE    CASE **
C               ******************************
C
      IF(ICOM.EQ.'ERAS')GOTO8820
      IF(ICOM.EQ.'ERA')GOTO8820
      IF(ICOM.EQ.'ER')GOTO8820
      GOTO8899
C
 8820 CONTINUE
      CALL EDERAS(IMANUF,IMODEL)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 8899 CONTINUE
C               ******************************
C               **  TREAT THE LIST   CASE   **
C               ******************************
C
      IF(ICOM(1:3).EQ.'LIS')GOTO9120
      GOTO9199
C
 9120 CONTINUE
      CALL EDLIST
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 9199 CONTINUE
C
C               ******************************
C               **  TREAT THE CALL   CASE   **
C               ******************************
C
      IF(ICOM.EQ.'CALL')GOTO9220
      IF(ICOM(1:3).EQ.'CAL')GOTO9220
      GOTO9299
C
 9220 CONTINUE
      CALL EDCALL
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 9299 CONTINUE
C               ******************************
C               **  TREAT THE EXECUTE CASE  **
C               ******************************
C
      IF(ICOM.EQ.'EXEC')GOTO9320
      IF(ICOM.EQ.'EXE')GOTO9320
      IF(ICOM.EQ.'X')GOTO9320
      GOTO9399
C
 9320 CONTINUE
      CALL EDX
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 9399 CONTINUE
C
C               ******************************
C               **  TREAT THE SAVE     CASE **
C               ******************************
C
      IF(ICOM.EQ.'SAVE')GOTO9420
      IF(ICOM.EQ.'SAV')GOTO9420
      IF(ICOM.EQ.'SA')GOTO9420
      GOTO9499
C
 9420 CONTINUE
      CALL EDSAVE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 9499 CONTINUE
C
C               ******************************
C               **  TREAT THE RESTORE  CASE **
C               ******************************
C
      IF(ICOM.EQ.'REST')GOTO9520
      IF(ICOM.EQ.'RES')GOTO9520
      GOTO9599
C
 9520 CONTINUE
      CALL EDREST
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
 9599 CONTINUE
C
C               *****************************
C               **  TREAT THE DUMP   CASE  **
C               *****************************
C
      IF(ICOM.EQ.'DUMP')GOTO11320
      GOTO11399
C
11320 CONTINUE
      CALL EDDUMP
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11399 CONTINUE
C
C               *****************************
C               **  TREAT THE CUT    CASE  **
C               *****************************
C
      IF(ICOM.EQ.'CUT')GOTO11420
      IF(ICOM.EQ.'CU')GOTO11420
      GOTO11499
C
11420 CONTINUE
      CALL EDCUT
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11499 CONTINUE
C
C               *****************************
C               **  TREAT THE DELETE AND INSERT CASE  **
C               *****************************
C
      IF(ICOM.EQ.'DI')GOTO11520
      GOTO11599
C
11520 CONTINUE
      CALL EDDI
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11599 CONTINUE
C
CCCCC          *****************************
CCCCC          **  TREAT THE FORMAT CASE  **
CCCCC          *****************************
CCCCC
CCCCC IF(ICOM(1:2).EQ.'FO')GOTO11620
CCCCC GOTO11699
CCCCC
C1620 CONTINUE
CCCCC CALL EDFORM
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C1699 CONTINUE
CCCCC
CCCCC          *********************************
CCCCC          **  TREAT THE PRINT PARAGRAPH CASE  **
CCCCC          *********************************
CCCCC
CCCCC IF(ICOM.EQ.'PPAR')GOTO11720
CCCCC GOTO11799
CCCCC
C1720 CONTINUE
CCCCC CALL EDPPAR
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C1799 CONTINUE
CCCCC
C               *********************************
C               **  TREAT THE PRINT ALL CASE  **
C               *********************************
C
      IF(ICOM.EQ.'PA')GOTO11820
      GOTO11899
C
11820 CONTINUE
      CALL EDPA
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11899 CONTINUE
C
C               *********************************
C               **  TREAT THE SHIFT CASE  **
C               *********************************
C
      IF(ICOM(1:3).EQ.'SHI')GOTO11920
      GOTO11999
C
11920 CONTINUE
      CALL EDSHIF(ISHIFN)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
11999 CONTINUE
C
CCCCC           *********************************
CCCCC           **  TREAT THE SPLIT CASE  **
CCCCC           *********************************
CCCCC
CCCCC IF(ICOM(1:2).EQ.'SP')GOTO12020
CCCCC GOTO12099
CCCCC
C2020 CONTINUE
CCCCC CALL EDSPLI
CCCCC IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC
C2099 CONTINUE
CCCCC
C               ********************************************
C               **  TREAT THE LOCATE AND KEEP (ALL) CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'LKIL')GOTO12120
      IF(ICOM.EQ.'LR')GOTO12120
      GOTO12199
C
12120 CONTINUE
      IFEESV=IFEESW
      DO12130IDUMMY=1,1000000
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,12121)ICURLN,NUMLIN
12121 FORMAT('***** FROM EDSEAR--ICURLN,NUMLIN = ',2I8)
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDSEAR')
C
      ICURL2=ICURLN
CCCCC IF(ICURLN.EQ.NUMLIN)GOTO12149
      IF(ICURLN.GE.NUMLIN)GOTO12149
C
      ICOM='LOCA'
      ICOM2='TE  '
      IFEESW=IFEESV
      CALL EDLOCA
      IF(IERROR.EQ.'YES')GOTO9000
CCCCC IF(ICURLN.GT.NUMLIN)GOTO12149
C
      ICOM='DELE'
      ICOM2='TE  '
      IL1=ICURL2+1
      IL2=ICURLN-1
      IF(ICURLN.GT.NUMLIN)IL2=ICURLN
      IF(IL1.GT.IL2)GOTO12129
      NUMARG=2
      IARG(1)=IL1
      IARG(2)=IL2
      IARGT(1)='NUMB'
      IARGT(2)='NUMB'
      IFEESW='OFF'
      CALL EDDELE
      IF(IERROR.EQ.'YES')GOTO9000
      ICURLN=ICURLN-1
C
      ICOM='NEXT'
      ICOM2='    '
      NUMARG=0
      IFEESW='OFF'
      CALL EDNEXT
      IF(IERROR.EQ.'YES')GOTO9000
C
12129 CONTINUE
12130 CONTINUE
C
12149 CONTINUE
      IFEESW=IFEESV
      ICOM='LKIL'
      ICOM2='    '
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
12199 CONTINUE
C
C               ********************************************
C               **  TREAT THE LOCATE AND DELETE (ALL) CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'LDEL')GOTO12220
      GOTO12299
C
12220 CONTINUE
      IFEESV=IFEESW
      DO12230IDUMMY=1,1000000
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,12221)ICURLN,NUMLIN
12221 FORMAT('***** FROM EDSEAR--ICURLN,NUMLIN = ',2I8)
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDSEAR')
      ICOM='LOCA'
      ICOM2='TE  '
      IFEESW=IFEESV
      CALL EDLOCA
      IF(IERROR.EQ.'YES')GOTO9000
      IF(ICURLN.GT.NUMLIN)GOTO12249
C
      ICOM='DELE'
      ICOM2='TE  '
      NUMARG=0
      IFEESW='OFF'
      CALL EDDELE
      IF(IERROR.EQ.'YES')GOTO9000
      ICURLN=ICURLN-1
C
12230 CONTINUE
C
12249 CONTINUE
      IFEESW=IFEESV
      ICOM='LDEL'
      ICOM2='    '
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
12299 CONTINUE
C
C               ***********************************
C               **  TREAT THE LOCATE BLANK CASE  **
C               ***********************************
C
      IF(ICOM(1:3).EQ.'LBL')GOTO12320
      IF(ICOM.EQ.'LOBL')GOTO12320
      IF(ICOM.EQ.'LABL')GOTO12320
      IF(ICOM.EQ.'LPER')GOTO12330
      GOTO12399
C
12320 CONTINUE
      IALL='OFF'
      IF(ICOM.EQ.'LABL')IALL='ON'
      CALL EDLOBL
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
12330 CONTINUE
      IALL='OFF'
      CALL EDLOPE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
12399 CONTINUE
C
C               ********************************************
C               **  TREAT THE LOCATE (ALL) BACK-TO-BACK
C               **  AND DELETE THE SECOND LINE CASE
C               ********************************************
C
      IF(ICOM.EQ.'LLD')GOTO12420
      GOTO12499
C
12420 CONTINUE
      IFEESV=IFEESW
CCCCC ICL1=0
      ICLN1=0
      DO12430IDUMMY=1,1000000
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,12421)ICURLN,NUMLIN
12421 FORMAT('***** FROM EDSEAR--ICURLN,NUMLIN = ',2I8)
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDSEAR')
      ICOM='LOCA'
      ICOM2='TE  '
      IFEESW=IFEESV
      CALL EDLOCA
      IF(IERROR.EQ.'YES')GOTO9000
      IF(ICURLN.GT.NUMLIN)GOTO12449
      IF(ICURLN.EQ.ICLN1+1)GOTO12441
      ICLN1=ICURLN
      GOTO12430
C
12441 CONTINUE
      ICOM='DELE'
      ICOM2='TE  '
      NUMARG=0
      IFEESW='OFF'
      CALL EDDELE
      IF(IERROR.EQ.'YES')GOTO9000
      ICURLN=ICURLN-1
      ICLN1=0
C
12430 CONTINUE
C
12449 CONTINUE
      IFEESW=IFEESV
      ICOM='LDEL'
      ICOM2='    '
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
12499 CONTINUE
C
C               *****************************************
C               **  TREAT THE LOCATE ALL AND INSERT BEFORE CASE   **
C               *****************************************
C
      IF(ICOM.EQ.'LIB')GOTO12520
      GOTO12599
C
12520 CONTINUE
C
      IWIDT0=IWIDTH
      DO12530I=1,IWIDTH
      IANS0(I)=IANS(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,12531)I,IANS(I),IANS0(I)
12531    FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
12530 CONTINUE
C
      DO12540IDUMMY=1,100000
C
      IANS(1)='L'
      IANS(2)='O'
      IANS(3)=' '
      IWIDTH=2
      J=3
      IF(IWIDT0.LT.5)GOTO12558
      DO12550I=5,IWIDT0
      IF(IANS0(I).EQ.' ')IWIDTH=J
      IF(IANS0(I).EQ.' ')GOTO12558
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,12551)I,J,IANS0(I),IANS(J)
12551    FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
12550 CONTINUE
      IWIDTH=J
12558 CONTINUE
      ICOM='LO'
      CALL EDLOCA
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,12559)ICURLN,NUMLIN
12559    FORMAT('ICURLN,NUMLIN = ',2I8)
         CALL EDWRST('EDSEAR')
      ENDIF
      IF(ICURLN.GT.NUMLIN)GOTO12579
C
      ICOM='UP  '
      ICOM2='    '
      NUMARG=0
      CALL EDUP
C
      IANS(1)='I'
      IANS(2)='N'
      IANS(3)=' '
      IARGUM=1
      J=3
      IF(IWIDT0.LT.5)GOTO12565
      DO12560I=5,IWIDT0
      IF(IANS0(I).EQ.' ')IARGUM=IARGUM+1
      IF(IARGUM.EQ.1)GOTO12560
      IF(IARGUM.EQ.2.AND.IANS0(I).EQ.' ')GOTO12560
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,12561)I,J,IANS0(I),IANS(J)
12561    FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
12560 CONTINUE
12565 CONTINUE
      IF(IARGUM.EQ.1)GOTO12566
      GOTO12569
12566 CONTINUE
      J=J+1
      IANS(J)=' '
12569 CONTINUE
      NUMARG=IARGUM-1
      IWIDTH=J
      ICOM='IN'
      CALL EDINSE
C
      ICOM='N   '
      ICOM2='    '
      NUMARG=0
      CALL EDNEXT
C
12540 CONTINUE
C
12579 CONTINUE
C
      IWIDTH=IWIDT0
      DO12580I=1,IWIDT0
      IANS(I)=IANS0(I)
12580 CONTINUE
      ICOM='LIB'
C
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
12599 CONTINUE
C
CCCC FEBRUARY 1995.  MOVE TO BEFORE EXTEND CASE TO AVOID
CCCC NAME CONFLICT WITH EXTEND COMMAND.
C               *****************************************
C               **  TREAT THE EXTEND ALL CASE          **
C               **  EA     EXTALL                      **
C               *****************************************
C
CCCCC IF(ICOM.EQ.'EA'.OR.ICOM.EQ.'EXTA')THEN
CCCCC    ICOM='EA  '
CCCCC    ICOM2='    '
CCCCC    CALL EDEA
CCCCC    IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
CCCCC ENDIF
C
C               *****************************
C               **  TREAT THE NEAT   CASE  **
C               *****************************
C
      IF(ICOM(1:3).EQ.'NEA')GOTO12720
      GOTO12799
C
12720 CONTINUE
      CALL EDNEAT(ICOLL1,ICOLL2,IBLASW)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
12799 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS UPDATED   DECEMBER 1994
C               ********************************************
C               **  TREAT THE / = PRINT 20 LINES (A PSEUDO-SCREEN) CASE  **
C               ** SYNONYMS: / ? ' "                      **
C               ********************************************
C
      IF(ICOM.EQ.'/'.OR.ICOM.EQ.'?'.OR.
     1ICOM(1:1).EQ.CHAR(39).OR.ICOM.EQ.'"')THEN
         ICOM='P   '
         ICOM2='    '
         NUMARG=1
         IARG(1)=20
         IARGT(1)='NUMB'
         CALL EDERAS(IMANUF,IMODEL)
         CALL EDPRIN
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
         ICOM='/ '
         ICOM2='    '
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED   DECEMBER 1994
C               ********************************************
C               **  TREAT THE \ OR | COMMAND:    **
C               ** GO UP 20 LINES AND PRINT THIS CURRENT PAGE
C               ********************************************
C
CUNIX IF(ICOM.EQ.'\'.OR.ICOM.EQ.'|'.OR.
      IF(ICOM.EQ.'\\'.OR.ICOM.EQ.'|'.OR.
     1   ICOM.EQ.']'.OR.ICOM.EQ.'}')THEN
C
         ICOM='UP  '
         ICOM2='    '
         NUMARG=1
         IARG(1)=18
         IARGT(1)='NUMB'
CCCCC FEBRUARY 1995.  ONLY WANT ONE UP COMMAND, NOT TWO.
CCCCC    CALL EDUP
         CALL EDUP
         CALL EDERAS(IMANUF,IMODEL)
         IF(IERROR.EQ.'YES')GOTO9000
C
         ICOM='P   '
         ICOM2='    '
         NUMARG=1
         IARG(1)=20
         IARGT(1)='NUMB'
         CALL EDPRIN
         IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C
C               *****************************************
C               **  TREAT THE LOCATE BACKWARDS CASE    **
C               *****************************************
C
      IF(ICOM.EQ.'LB')GOTO12920
      IF(ICOM.EQ.'LBA')GOTO12920
      GOTO12999
C
12920 CONTINUE
      IALL='OFF'
      IF(ICOM.EQ.'LBA')IALL='ON'
      CALL EDLB
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
12999 CONTINUE
C
C               **********************************
C               **  TREAT THE MAKE FORMAT CASE  **
C               **  (A MACRO)                   **
C               **********************************
C
      IF(ICOM.EQ.'MF')GOTO13120
      GOTO13199
C
13120 CONTINUE
      NCTEMP=16
      ICTEMP(1:16)='CALL FED$:MF.FED'
      CALL EDCOST(ICTEMP,NCTEMP)
      IEXEIM='YES'
      IFOUND='YES'
      GOTO9000
C
13199 CONTINUE
C
C               *****************************************
C               **  TREAT THE LOCATE (ALL) AND SEQUENCE CASE   **
C               *****************************************
C
      IF(ICOM.EQ.'LS')GOTO13220
      GOTO13299
C
13220 CONTINUE
C
      IWIDT0=IWIDTH
      DO13230I=1,IWIDTH
      IANS0(I)=IANS(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,13231)I,IANS(I),IANS0(I)
13231    FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
13230 CONTINUE
C
      DO13240IDUMMY=1,100000
C
      IANS(1)='L'
      IANS(2)='O'
      IANS(3)=' '
      IWIDTH=2
      J=3
      IF(IWIDT0.LT.4)GOTO13258
      DO13250I=4,IWIDT0
      IF(IANS0(I).EQ.' ')IWIDTH=J
      IF(IANS0(I).EQ.' ')GOTO13258
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,13251)I,J,IANS0(I),IANS(J)
13251    FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
13250 CONTINUE
      IWIDTH=J
13258 CONTINUE
      ICOM='LO'
      CALL EDLOCA
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,13259)ICURLN,NUMLIN
13259    FORMAT('ICURLN,NUMLIN = ',2I8)
         CALL EDWRST('EDSEAR')
      ENDIF
      IF(ICURLN.GT.NUMLIN)GOTO13279
C
      IANS(1)='C'
      IANS(2)=' '
      IANS(3)='/'
      IARGUM=1
      J=3
      IF(IWIDT0.LT.4)GOTO13265
      DO13260I=4,IWIDT0
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,13261)I,J,IANS0(I),IANS(J)
13261    FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
13260 CONTINUE
13265 CONTINUE
      J=J+1
      IANS(J)='/'
      IF(NUMARG.GE.2)ISEQNU=IARG(2)
      CALL EDCOIH(ISEQNU,ICSEQN,NCSEQN,IVALID)
      IF(NCSEQN.LE.0)GOTO13271
      DO13270I=1,NCSEQN
      J=J+1
      IANS(J)=ICSEQN(I:I)
13270 CONTINUE
      ISEQNU=ISEQNU+1
13271 CONTINUE
      NUMARG=1
      J=J+1
      IANS(J)='/'
      IWIDTH=J
      ICOM='C'
      IGLOBA='OFF'
      CALL EDCHAN(IGLOBA)
C
13240 CONTINUE
C
13279 CONTINUE
      ISEQNU=1
      ICSEQN='1         '
      NCSEQN=1
C
      IWIDTH=IWIDT0
      DO13280I=1,IWIDT0
      IANS(I)=IANS0(I)
13280 CONTINUE
      ICOM='LS'
C
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
13299 CONTINUE
C
C               *******************************************
C               **  TREAT THE DEVICE MANUFACTURER CASE   **
C               **  (= TERMINAL DEFINITION CASE)         **
C               *******************************************
C
      IF(NUMARG.GE.2)THEN
         IF(ICOM.EQ.'DEVI'.AND.IHARG(1).EQ.'1')THEN
            IF(IHARG(2).EQ.'TEKT')GOTO13320
            IF(IHARG(2).EQ.'IBM')GOTO13320
            IF(IHARG(2).EQ.'IBM-')GOTO13320
            IF(IHARG(2).EQ.'HDS')GOTO13320
            IF(IHARG(2).EQ.'QUME')GOTO13320
            IF(IHARG(2).EQ.'VT')GOTO13320
            IF(IHARG(2).EQ.'VT-1')GOTO13320
            IF(IHARG(2).EQ.'VT-2')GOTO13320
C
            IF(IHARG(2).EQ.'TURB')GOTO13320
            IF(IHARG(2).EQ.'DOS')GOTO13320
            IF(IHARG(2).EQ.'UNIX')GOTO13320
            IF(IHARG(2).EQ.'X11')GOTO13320
            IF(IHARG(2).EQ.'X')GOTO13320
            IF(IHARG(2).EQ.'MOTI')GOTO13320
            IF(IHARG(2).EQ.'MAC')GOTO13320
            IF(IHARG(2).EQ.'MACI')GOTO13320
         ENDIF
      ENDIF
      GOTO13399
C
13320 CONTINUE
      ICOM=IHARG(2)
      CALL EDDEMN(IMANUF,IMODEL)
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
13399 CONTINUE
C
C               ******************************
C               **  TREAT THE NEWS   CASE   **
C               ******************************
C
      IF(ICOM(1:4).EQ.'NEWS')GOTO13420
      GOTO13499
C
13420 CONTINUE
      CALL EDLINE
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
13499 CONTINUE
C
C               *****************************************
C               **  TREAT THE LOCATE (ALL) AND PRINT (SOME OFFSET) CASE   **
C               *****************************************
C
      IF(ICOM.EQ.'LP')GOTO13520
      GOTO13599
C
13520 CONTINUE
C
      IWIDT0=IWIDTH
      DO13530I=1,IWIDTH
      IANS0(I)=IANS(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,13531)I,IANS(I),IANS0(I)
13531    FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
13530 CONTINUE
C
      IFEESV=IFEESW
      DO13540IDUMMY=1,100000
C
      IANS(1)='L'
      IANS(2)='O'
      IANS(3)=' '
      IWIDTH=2
      J=3
      IF(IWIDT0.LT.5)GOTO13558
      DO13550I=5,IWIDT0
      IF(IANS0(I).EQ.' ')IWIDTH=J
      IF(IANS0(I).EQ.' ')GOTO13558
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,13551)I,J,IANS0(I),IANS(J)
13551    FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
13550 CONTINUE
      IWIDTH=J
13558 CONTINUE
      ICOM='LO'
      IFEESW='OFF'
      CALL EDLOCA
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,13559)ICURLN,NUMLIN
13559    FORMAT('ICURLN,NUMLIN = ',2I8)
         CALL EDWRST('EDSEAR')
      ENDIF
      IF(ICURLN.GT.NUMLIN)GOTO13579
C
13560 CONTINUE
      ICOM='NEXT'
      ICOM2='    '
      NUMARG=1
      IARG(1)=ILPOFF
      IARGT(1)='NUMB'
      IFEESW=IFEESV
      CALL EDNEXT
C
      ICOM='NEXT'
      ICOM2='    '
      NUMARG=1
      IARG(1)=(-ILPOFF)
      IARGT(1)='NUMB'
      IFEESW='OFF'
      CALL EDNEXT
C
13540 CONTINUE
C
13579 CONTINUE
      IFEESW=IFEESV
C
      IWIDTH=IWIDT0
      DO13580I=1,IWIDT0
      IANS(I)=IANS0(I)
13580 CONTINUE
      ICOM='LP'
C
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
13599 CONTINUE
C
C               ******************************
C               **  TREAT THE OPEN   CASE   **
C               ******************************
C
      IF(ICOM(1:4).EQ.'OPEN')GOTO14120
      GOTO14199
C
14120 CONTINUE
      CALL EDOPEN
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
14199 CONTINUE
C
C               ******************************
C               **  TREAT THE CLOSE  CASE   **
C               ******************************
C
      IF(ICOM(1:4).EQ.'CLOS')GOTO14220
      GOTO14299
C
14220 CONTINUE
      CALL EDCLOS
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
14299 CONTINUE
C
C               *****************************************
C               **  TREAT THE LOCATE AND JUMP IF NOT FOUND   CASE   **
C               *****************************************
C
      IF(ICOM(1:4).EQ.'LJNF')GOTO14320
      GOTO14399
C
14320 CONTINUE
      ICOM='LOCA'
      ICOM2='TE  '
      CALL EDLOCA
      IF(IFOUND.EQ.'NO')GOTO14399
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'YES'.AND.ICURLN.LE.NUMLIN)GOTO9000
      DO14330I=1,1000
      READ(ICALNU,14335)ICJU11
14335 FORMAT(A11)
      IF(ICJU11.EQ.'END OF LJNF')GOTO9000
14330 CONTINUE
C
14399 CONTINUE
C
C               ********************************************
C               **  TREAT THE LOCATE AND DELETE (ALL) BLANK (LINES) CASE  **
C               ********************************************
C
      IF(ICOM.EQ.'LDBL')GOTO14420
      GOTO14499
C
14420 CONTINUE
      IFEESV=IFEESW
      DO14430IDUMMY=1,1000000
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,14421)ICURLN,NUMLIN
14421 FORMAT('***** FROM EDSEAR--ICURLN,NUMLIN = ',2I8)
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDSEAR')
      ICOM='LBL '
      ICOM2='    '
      IFEESW=IFEESV
      IALL='OFF'
      CALL EDLOBL
      IF(IERROR.EQ.'YES')GOTO9000
      IF(ICURLN.GT.NUMLIN)GOTO14449
C
      ICOM='DELE'
      ICOM2='TE  '
      NUMARG=0
      IFEESW='OFF'
      CALL EDDELE
      IF(IERROR.EQ.'YES')GOTO9000
      ICURLN=ICURLN-1
C
14430 CONTINUE
C
14449 CONTINUE
      IFEESW=IFEESV
      ICOM='LDBL'
      ICOM2='    '
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
14499 CONTINUE
C
C               *****************************************
C               **  TREAT THE LOCATE (ALL) AND SEQUENCE--RIGHT JUSTIFIED CASE   **
C               *****************************************
C
      IF(ICOM.EQ.'LSRJ')GOTO14520
      GOTO14599
C
14520 CONTINUE
C
      IWIDT0=IWIDTH
      IWIDST=IWIDT0-6+1
      DO14530I=1,IWIDTH
      IANS0(I)=IANS(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,14531)I,IANS(I),IANS0(I)
14531    FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
14530 CONTINUE
C
      DO14540IDUMMY=1,100000
C
      IANS(1)='L'
      IANS(2)='O'
      IANS(3)=' '
      IWIDTH=2
      J=3
      IF(IWIDT0.LT.6)GOTO14558
      DO14550I=6,IWIDT0
      IF(IANS0(I).EQ.' ')IWIDTH=J
      IF(IANS0(I).EQ.' ')GOTO14558
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
          WRITE(ICOUT,14551)I,J,IANS0(I),IANS(J)
14551    FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
14550 CONTINUE
      IWIDTH=J
14558 CONTINUE
      ICOM='LO'
      CALL EDLOCA
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,14559)ICURLN,NUMLIN
14559    FORMAT('ICURLN,NUMLIN = ',2I8)
         CALL EDWRST('EDSEAR')
      ENDIF
      IF(ICURLN.GT.NUMLIN)GOTO14579
C
      IANS(1)='C'
      IANS(2)='A'
      IANS(3)=' '
      IANS(4)='/'
      IARGUM=1
      J=4
      IF(IWIDT0.LT.6)GOTO14565
      DO14560I=6,IWIDT0
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,14561)I,J,IANS0(I),IANS(J)
14561    FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
14560 CONTINUE
14565 CONTINUE
      J=J+1
      IANS(J)='/'
      IF(NUMARG.GE.2)ISEQNU=IARG(2)
      CALL EDCOIH(ISEQNU,ICSEQN,NCSEQN,IVALID)
      IF(NCSEQN.LE.0)GOTO14578
CCCCC DO14570I=1,NCSEQN
      JSAVE=J
      DO14570I=1,IWIDST
      J=J+1
      IANS(J)=' '
14570 CONTINUE
      J=JSAVE
      IBLANK=IWIDST-NCSEQN
      IF(IBLANK.LE.0)GOTO14572
      DO14573I=1,IBLANK
      J=J+1
14573 CONTINUE
14572 CONTINUE
      DO14575I=1,NCSEQN
      J=J+1
      IANS(J)=ICSEQN(I:I)
14575 CONTINUE
      ISEQNU=ISEQNU+1
14578 CONTINUE
      NUMARG=1
      J=J+1
      IANS(J)='/'
      IWIDTH=J
      ICOM='C'
      IGLOBA='ON'
      CALL EDCHAN(IGLOBA)
C
14540 CONTINUE
C
14579 CONTINUE
      ISEQNU=1
      ICSEQN='1         '
      NCSEQN=1
C
      IWIDTH=IWIDT0
      DO14580I=1,IWIDT0
      IANS(I)=IANS0(I)
14580 CONTINUE
      ICOM='LSRJ'
C
      IF(IFOUND.EQ.'YES'.OR.IERROR.EQ.'YES')GOTO9000
C
14599 CONTINUE
C
C               *****************************************
C               **  TREAT THE CHANGE TILL LOCATE CASE  **
C               *****************************************
C
      IF(ICOM.EQ.'CTL')GOTO14620
      GOTO14699
C
14620 CONTINUE
      IFEESV=IFEESW
C
      IWIDT0=IWIDTH
      DO14630I=1,IWIDTH
      IANS0(I)=IANS(I)
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,14631)I,IANS(I),IANS0(I)
14631    FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
14630 CONTINUE
C
      IANS(1)='L'
      IANS(2)=' '
      IANS(3)=' '
      IANS(4)=' '
      ICJUNK=IANS(5)
      ICOUNT=0
      DO14640I=5,IWIDTH
      IF(IANS(I).EQ.ICJUNK)ICOUNT=ICOUNT+1
      IANS(I)=' '
      IF(ICOUNT.GE.3)GOTO14649
14640 CONTINUE
14649 CONTINUE
C
      IL1=ICURLN
      ICOM='LOCA'
      ICOM2='TE  '
      IFEESW='OFF'
      IALL='OFF'
      CALL EDLOCA
      IF(IERROR.EQ.'YES')GOTO9000
CCCCC IL2=ICURLN
      IL2=ICURLN-1
C
      IANS(1)='C'
      IANS(2)=' '
      IANS(3)=' '
      IANS(4)=' '
      ICJUNK=IANS0(5)
      ICOUNT=0
      IWIDJU=IWIDTH
      DO14650I=5,IWIDTH
      IANS(I)=IANS0(I)
      IF(ICOUNT.GE.3)IANS(I)=' '
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,14651)I,IANS(I),IANS0(I)
14651    FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
         CALL EDWRST('EDSEAR')
      ENDIF
      IF(ICOUNT.GE.3)GOTO14650
      IF(IANS(I).EQ.ICJUNK)ICOUNT=ICOUNT+1
      IF(IANS(I).EQ.ICJUNK.AND.ICOUNT.EQ.3)IWIDJU=I
14650 CONTINUE
14659 CONTINUE
      IWIDTH=IWIDJU
      ICOM='C   '
      ICOM2='    '
      NUMARG=NUMARG-1
C
      IF(IBUGED.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
         WRITE(ICOUT,999)
         CALL EDWRST('EDSEAR')
         DO14660I=1,IWIDTH
            WRITE(ICOUT,14661)I,IANS(I),IANS0(I)
14661       FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
            CALL EDWRST('EDSEAR')
14660    CONTINUE
      ENDIF
C
      IF(IL2.LT.IL1)GOTO14679
      DO14670IDUMMY=IL1,IL2
C
      ICURLN=IDUMMY
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,14671)ICURLN,NUMLIN
14671 FORMAT('***** FROM EDSEAR--ICURLN,NUMLIN,IL1,IL2 = ',4I8)
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDSEAR')
C
      IFEESW=IFEESV
      IF(ICURLN.GT.NUMLIN)GOTO14679
      IGLOBA='ON'
      CALL EDCHAN(IGLOBA)
      IF(IERROR.EQ.'YES')GOTO9000
C
14670 CONTINUE
14679 CONTINUE
C
      ICURLN=IL2
      ICOM='PRIN'
      ICOM2='T   '
      NUMARG=0
      IFEESW=IFEESV
      CALL EDPRIN
      IF(IERROR.EQ.'YES')GOTO9000
C
      ICURLN=IL2+1
      ICOM='PRIN'
      ICOM2='T   '
      NUMARG=0
      IFEESW=IFEESV
      CALL EDPRIN
      IF(IERROR.EQ.'YES')GOTO9000
C
      ICOM='CTL '
      ICOM2='    '
      NUMARG=NUMARG+1
      IWIDTH=IWIDT0
      DO14680I=1,IWIDTH
      IANS(I)=IANS0(I)
14680 CONTINUE
C
      IFOUND='YES'
      GOTO9000
C
14699 CONTINUE
C
C               *********************************************
C               **  TREAT THE    PRINT BLANK LINE   CASE   **
C               *********************************************
C
      IF(ICOM.EQ.'PBL')THEN
         WRITE(ICOUT,999)
         CALL EDWRST('EDSEAR')
         IFOUND='YES'
         GOTO9000
      ENDIF
C
C               **********************************************
C               **  TREAT THE    PRINT DOTTED LINE   CASE   **
C               **********************************************
C
      IF(ICOM.EQ.'PDL')THEN
         WRITE(ICOUT,14710)
14710    FORMAT('--------------------------------------------------')
         CALL EDWRST('EDSEAR')
         IFOUND='YES'
         GOTO9000
      ENDIF
C
C               *********************************************
C               **  TREAT THE    PRINT ERROR LINE   CASE   **
C               *********************************************
C
      IF(ICOM.EQ.'PEL')THEN
         WRITE(ICOUT,14711)IBELC,IBELC,IBELC,IBELC,IBELC
14711    FORMAT(5A1)
         CALL EDWRST('EDSEAR')
         WRITE(ICOUT,14711)IBELC,IBELC,IBELC,IBELC,IBELC
         CALL EDWRST('EDSEAR')
         WRITE(ICOUT,14711)IBELC,IBELC,IBELC,IBELC,IBELC
         CALL EDWRST('EDSEAR')
         WRITE(ICOUT,14711)IBELC,IBELC,IBELC,IBELC,IBELC
         CALL EDWRST('EDSEAR')
         WRITE(ICOUT,14711)IBELC,IBELC,IBELC,IBELC,IBELC
         CALL EDWRST('EDSEAR')
         WRITE(ICOUT,14712)
14712    FORMAT('***** ERROR ENCOUNTERED *****')
         CALL EDWRST('EDSEAR')
         IFOUND='YES'
         GOTO9000
      ENDIF
C
C               **************************************************
C               **            TREAT THE LOOP CASE               **
C               **************************************************
C
      IF(ICOM.EQ.'LOOP')THEN
         LOOPCT=IARG(1)
         LOOPST=' '
         ILOC=IWIDTH+1
         DO14721I=1,IWIDTH
            I2=I
            IF(IANS(I).EQ.' ')GOTO14722
14721    CONTINUE
14722    CONTINUE
         IMIN=I2+1
         DO14723I=IMIN,IWIDTH
            I2=I
            IF(IANS(I).EQ.' ')GOTO14724
14723    CONTINUE
14724    CONTINUE
         IMIN=I2+1
         J=0
         DO14725I=IMIN,IWIDTH
            J=J+1
            LOOPST(J:J)=IANS(I)
14725    CONTINUE
         LOOPIW=J
         WRITE(ICOUT,999)
         CALL EDWRST('EDSEAR')
         WRITE(ICOUT,14726)LOOPCT
14726    FORMAT('LOOP COUNT  = ',I8)
         CALL EDWRST('EDMAI2')
         WRITE(ICOUT,14727)(LOOPST(I:I),I=1,LOOPIW)
14727    FORMAT('LOOP STRING = ',240A1)
         CALL EDWRST('EDMAI2')
         WRITE(ICOUT,999)
         CALL EDWRST('EDSEAR')
         IFOUND='YES'
         GOTO9000
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED MAY 1993
C               **************************************************
C               **            TREAT THE IF CASE                 **
C               **      EXAMPLE--IF CHANGE YES UP        ZZ
C               **      EXAMPLE--IF CHANGE NO UP        ZZ
C               **      EXAMPLE--IF LOCATE YES UP        ZZ
C               **      EXAMPLE--IF LOCATE NO UP        ZZ
C               **************************************************
C
      IF(ICOM.EQ.'IF')THEN
         IF((IHARG(1).EQ.'CHAN'.AND.IHARG(2).EQ.CHANGE).OR.
     1      (IHARG(1).EQ.'LOCA'.AND.IHARG(2).EQ.LOCATE))THEN
            J=0
            IF(IHARG(2).EQ.'YES')IMIN=15
            IF(IHARG(2).EQ.'NO')IMIN=14
            DO14730I=IMIN,IWIDTH
               J=J+1
               IANS(J)=IANS(I)
14730       CONTINUE
            IWIDTH=J
            ICOM=IHARG(3)
            ICOM2=IHARG2(3)
            CALL EDSHLE(3)
            GOTO50
         ELSE
            IANS(1)='.'
            IWIDTH=1
            ICOM='.    '
            ICOM2='    '
            NUMARG=0
            GOTO50
         ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGED.EQ.'OFF'.AND.ISUBRO.NE.'SEAR')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF EDSEAR--')
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9012)ICURLN
 9012 FORMAT('ICURLN = ',I8)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9013)IWIDTH
 9013 FORMAT('IWIDTH = ',I8)
      CALL EDWRST('EDSEAR')
      IF(IWIDTH.GE.0)THEN
         WRITE(ICOUT,9014)(IANS(I),I=1,IWIDTH)
 9014    FORMAT('(IANS(I),I=1,IWIDTH) = ',100A1)
         CALL EDWRST('EDSEAR')
      ENDIF
      WRITE(ICOUT,9015)IBUGED,IBUGE2
 9015 FORMAT('IBUGED,IBUGE2 = ',A4,2X,A4)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9021)IMARK
 9021 FORMAT('IMARK = ',I8)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9022)ICOLL1,ICOLL2,IJUST
 9022 FORMAT('ICOLL1,ICOLL2,IJUST = ',2I8,2X,A4)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9023)IEXEIM
 9023 FORMAT('IEXEIM = ',A4)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9024)IFOUND,IERROR
 9024 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9031)ICOM,ICOM2
 9031 FORMAT('ICOM,ICOM2 = ',A4,2X,A4)
      CALL EDWRST('EDSEAR')
      WRITE(ICOUT,9041)NUMARG
 9041 FORMAT('NUMARG = ',I8)
      CALL EDWRST('EDSEAR')
      DO9042I=1,NUMARG
      WRITE(ICOUT,9043)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
 9043 FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,2X,I8,E15.7)
      CALL EDWRST('EDSEAR')
 9042 CONTINUE
 9090 CONTINUE
      RETURN
      END

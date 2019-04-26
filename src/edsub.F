CCCCC THE FOLLOWING LINE WAS AUGMENTED JULY 1992
CCCCC SUBROUTINE EDABOR
      SUBROUTINE EDABOR(STOPSW)
C
C     PURPOSE--ABORT THE EDITING SESSION.
C              IGNORE ALL CHANGES MADE DURING THE EDITING SESSION.
C              LEAVE THE ORIGINAL FILE AS IT WAS UPON ENTRY BY THE EDITOR.
C     NOTE--NO ARGUMENTS ARE EXPECTED.
C     SYNTAX--AB
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED--JULY   1992 ADD STOP SWITCH (STOPSW)
C
C---------------------------------------------------------------------
C
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1992
      CHARACTER*4 STOPSW
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDAB'
      ISUBN2='OR  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ABOR')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDABOR')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDABOR')
   51 FORMAT('*****AT THE BEGINNING OF EDABOR--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ******************************
C               **  STEP 1--                **
C               **  WRITE OUT A MESSAGE     **
C               ******************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ABOR')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,1205)
      CALL EDWRST('EDABOR')
 1205 FORMAT('THIS IS AN ABORT FROM THE EDITOR')
      WRITE(ICOUT,1206)
      CALL EDWRST('EDABOR')
 1206 FORMAT('(THE FILE HAS BEEN LEFT UNCHANGED)')
      WRITE(ICOUT,1207)IORINA
      CALL EDWRST('EDABOR')
 1207 FORMAT('FILE              = ',A80)
      WRITE(ICOUT,1208)NUMLOR
      CALL EDWRST('EDABOR')
 1208 FORMAT('NUMBER OF LINES   = ',I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDABOR')
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ABOR')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDABOR')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDABOR')
 9011 FORMAT('*****AT THE END       OF EDABOR--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      IF(ICOM.EQ.'ABR'.OR.ICOM.EQ.'ABRR')RETURN
CCCCC THE FOLLOWING LINE WAS FIXED JULY 1992
CCCCC IF(ICOM.NE.'ABR'.AND.ICOM.NE.'ABRR')STOP
      IF(ICOM.NE.'ABR'.AND.ICOM.NE.'ABRR')STOPSW='YES'
      END
      SUBROUTINE EDACPR
C
C     PURPOSE--ACTIVATE THE PRINTER
C              (FOR THE IBM-PC PRINTER,
C              SEND DOWN A CONTROL LOWER CASE P
C              = ASCII DLE = CHAR(16)  )
C     NOTE--NO ARGUMENTS ARE EXPECTED OR PROCESSED
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDAC'
      ISUBN2='PR  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ACPR')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDACPR')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDACPR')
   51 FORMAT('***** AT THE BEGINNING OF EDACPR--')
   90 CONTINUE
C
      NCOUT=1
      ICOUT(1:1)=IDLEC
      ILOUT=(-1)
      CALL EDWRST('EDACPR')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDACPR')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDACPR')
 9011 FORMAT('***** AT THE END       OF EDACPR--')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDADD
C
C     PURPOSE--ADD (= INSERT) A TEXT BLOCK FROM FILE
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED         --APRIL   1993. FORMAT CHANGED FROM 80 TO 130
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1993
CCCCC CHARACTER*80 IREC
      CHARACTER*240 IREC
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUEX
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDAD'
      ISUBN2='D   '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ADD')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDADD ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDADD ')
   51 FORMAT('***** AT THE BEGINNING OF EDADD--')
      WRITE(ICOUT,52)ICURLN,NUMLIN
      CALL EDWRST('EDADD ')
   52 FORMAT('ICURLN,NUMLIN = ',2I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE NAME OF THE FILE       **
C               **  THAT THE BLOCK IS TO BE COPIED FROM  **
C               **  (IF NO NAME IS GIVEN,                **
C               **  THEN USE A DEFAULT FILE NAME).       **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUEX)
      IF(IFOUEX.EQ.'YES')GOTO1110
      GOTO1120
C
 1110 CONTINUE
      IFILE=' '
      IF(NUMCST.LE.0)GOTO1119
      DO1111I=1,NUMCST
      IFILE(I:I)=ISTRIN(I)
 1111 CONTINUE
 1119 CONTINUE
CCCCC ISTAT='UNKNOWN'
      ISTAT='OLD'
      GOTO1190
C
 1120 CONTINUE
      IFILE=ICOPNA
CCCCC ISTAT=ICOPST
      ISTAT='OLD'
      GOTO1190
C
 1190 CONTINUE
      IOUNIT=ICOPNU
      IFORM=ICOPFO
      IACCES=ICOPAC
      IREWR=ICOPRW
      ISUBN0='ADD'
      IERRFI='NO'
C
C               *********************
C               **  STEP 12--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO1210
      GOTO1290
C
 1210 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL EDWRST('EDADD ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** ERROR IN EDADD--')
      CALL EDWRST('EDADD ')
      WRITE(ICOUT,1212)
      CALL EDWRST('EDADD ')
 1212 FORMAT('      THE ADD FILE COULD NOT BE OPENED')
      WRITE(ICOUT,1213)
      CALL EDWRST('EDADD ')
 1213 FORMAT('      (FILE NOT EXIST OR FILE PROTECTION VIOLATION)')
      WRITE(ICOUT,1214)
      CALL EDWRST('EDADD ')
 1214 FORMAT('      NO ADDING WAS CARRIED OUT.')
      GOTO9000
C
 1290 CONTINUE
C
C               *****************************************
C               **  STEP 21--                          **
C               **  SET UP A LOOP                      **
C               **  THAT WILL READ ONE LINE AT A TIME  **
C               **  FROM THE FILE                      **
C               **  AND INSERT IT AT THE PROPER POINT  **
C               **  IN THE WORKSPACE                   **
C               *****************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ITARG=ICURLN
      DO2100IDUMMY=1,100000
C
C               ************************************
C               **  STEP 22--                     **
C               **  READ IN A LINE FROM THE FILE  **
C               ************************************
C
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
CCCCC NUMCRE=80
      NUMCRE=238
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1993
      IREC=' '
      READ(IOUNIT,2205,END=2190)IREC
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
C2205 FORMAT(A80)
 2205 FORMAT(A238)
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2206)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDADD ')
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1993
C2206 FORMAT('IREC = ',A80)
 2206 FORMAT('IREC = ',A238)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2207)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDADD ')
 2207 FORMAT('NUMCRE = ',I8)
C
      DO2200I=1,NUMCRE
      IREV=NUMCRE-I+1
      IF(IREC(IREV:IREV).NE.' ')GOTO2290
 2200 CONTINUE
      IREV=0
 2290 CONTINUE
      NUMCRE=IREV
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2295)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDADD ')
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1993
C2295 FORMAT('IREC = ',A80)
 2295 FORMAT('IREC = ',A238)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2296)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDADD ')
 2296 FORMAT('NUMCRE = ',I8)
C
C               ***************************************************
C               **  STEP 23--                                    **
C               **  DETERMINE IF THERE IS ROOM IN THE WORKSPACE  **
C               **  FOR THE NEW LINE                             **
C               ***************************************************
C
      NUMCH2=NUMCHA+NUMCRE
      CALL EDCHSI(NUMCH2)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               **********************************************
C               **  STEP 24--                               **
C               **  UPDATE THE WORKSPACE TABLES             **
C               **  AND INSERT THE LINE INTO THE WORKSPACE  **
C               **********************************************
C
      IF(ICURLN.GT.NUMLIN)ICURLN=NUMLIN
      ICURLP=ICURLN+1
      NUMLP1=NUMLIN+1
      NUMRP1=NUMROW+1
C
      IHOLD1=IPOINT(ICURLP)
      IPOINT(ICURLP)=NUMRP1
C
      IF(ICURLP.LE.0)GOTO2419
      IF(NUMLIN.LE.0)GOTO2419
      IF(ICURLP.GT.NUMLIN)GOTO2419
      DO2410ILINE=ICURLP,NUMLIN
      ILINEP=ILINE+1
      IHOLD2=IPOINT(ILINEP)
      IPOINT(ILINEP)=IHOLD1
      IHOLD1=IHOLD2
 2410 CONTINUE
 2419 CONTINUE
C
      J1=NUMCHA+1
      N1=NUMCRE
      J2=J1+(N1-1)
C
      IROW=NUMRP1
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO2420J=J1,J2
      I=I+1
      ICHA(J)=IREC(I:I)
 2420 CONTINUE
 2429 CONTINUE
      ICURLN=ICURLP
      NUMLIN=NUMLP1
      NUMROW=NUMRP1
      NUMCHA=J2
C
 2100 CONTINUE
 2190 CONTINUE
C
C               ***************************
C               **  STEP 25--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      ISTEPN='25'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEESW.EQ.'OFF')GOTO2529
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDADD ')
      IMIN=ITARG+1
      IMAX=ICURLN
      IDEL=IMAX-IMIN+1
      ILINE=IMIN-1
      DO2520IPASS=1,10000
      ILINE=ILINE+1
      IF(IPASS.GE.2.AND.IDEL.GE.11)ILINE=IMAX
      IF(ILINE.LT.1)GOTO2525
      IF(ILINE.GT.IMAX)GOTO2525
      IF(ILINE.GT.NUMLIN)GOTO2525
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDADD ')
      ENDIF
 2521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2522)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDADD ')
      ENDIF
 2522 FORMAT(I6,':',3X,230A1)
 2525 CONTINUE
      IF(IPASS.GE.2.AND.IDEL.GE.11)GOTO2528
 2520 CONTINUE
 2528 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDADD ')
 2529 CONTINUE
C
C               **********************
C               **  STEP 31--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ADD')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDADD ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDADD ')
 9011 FORMAT('***** AT THE END       OF EDADD--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDADD ')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDADD ')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDADD ')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)NUMLIN,NUMCHA
      CALL EDWRST('EDADD ')
 9016 FORMAT('NUMLIN,NUMCHA = ',2I8)
      WRITE(ICOUT,9017)ICURLN,NUMLIN,IFOUEX
      CALL EDWRST('EDADD ')
 9017 FORMAT('ICURLN,NUMLIN,IFOUEX = ',2I8,2X,A4)
      WRITE(ICOUT,9018)IBLIM1,IBLIM2,IMIN,IMAX
      CALL EDWRST('EDADD ')
 9018 FORMAT('IMIN,IMAX,IBLIM1,IBLIM2 = ',4I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDADD ')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDADWO(ICNEWL,NCNEWL,ICWORD,NCWORD,
     1ICOLL1,ICOLL2,IBLASW,IADDED)
C
C     PURPOSE--ADD THE NCWORD CHARACTERS IN ICWORD
C              TO THE END OF ICNEWL.
C               START THE ADDITION AT COLUMN    NCNEWL+1    OF ICNEWL.
C              IF IBLASW = YES, THEN THIS SUBROUTINE
C                 SHOULD ADD THE WORD AND THEN APPEND THE APPROPRIATE
C                 TRAILING SPACES (USUALLY 1, BUT IT WILL BE 2
C                 IF THE LAST CHARACTER IN ICWORD IS A PERIOD,
C                 QUESTION MARK, OR EXCLAMATION POINT).
C              DURING EXECUTION OF THIS SUBROUTINE
C                 (AS ICWORD IS BEING APPENDED TO ICLINE)
C                 IF NO OVERFLOW WAS ENCOUNTERED
C                 AND SO THE WORD WAS ACTUALLY ADDED,
C                 THEN IADDED WILL BE SET TO    NO
C                 IF AN OVERFLOW IS ENCOUNTERED,
C                 (AND SO THE WORD WILL NOT BE ADDED)
C                 THEN IADDED WILL BE SET TO    NO.
C     INPUT  ARGUMENTS--ICNEWL = THE CHARACTER VARIABLE
C                                TO WHICH THE APPENDING
C                                IS BEING DONE.
C                       NCNEWL = THE CURRENT TOTAL NUMBER OF
C                                CHARACTERS IN ICNEWL.
C                                THE NCNEWL-TH CHARACTER IN ICNEWL
C                                MAY BE EITHER A BLANK OR A NON-BLANK
C                                DEPENDING ON PRIOR CALLS TO THIS SUBROUTINE
C                                AND DEPENDING ON PRIOR SETTINS OF  IBLASW.
C                                NEW CHARACTERS BEING ADDED TO
C                                ICNEWL WILL START AT COLUMN NCNEWL+1
C                       ICWORD = THE CHARACTER VARIABLE
C                                CONTAINING THE WORD BEING ADDED.
C                       NCWORD = THE NUMBER OF CHARACTERS IN ICWORD
C                                TO BE ADDED (STARTING WITH CHARACTER 1).
C                       ICOLL1 = THE LOWER BOUND ON THE COLUMN NUMBER
C                                IN ICLINE; THAT IS, UNDER NO CIRCUMSTANCES
C                                MAY CHARACTERS BE INSERTED INTO ICLINE
C                                BEFORE COLUMN ICOLL1.  IF ICOLL1 IS NOT 1,
C                                THEN THIS ALLOWS US TO FORCE LEAD BLANKS
C                                INTO ICLINE.
C                       ICOLL2 = THE UPPER BOUND ON THE COLUMN NUMBER
C                                IN ICLINE; THAT IS, UNDER NO CIRCUMSTANCES
C                                MAY CHARACTERS BE INSERTED INTO ICLINE
C                                AFTER COLUMN ICOLL1.
C                       IBLASW = THE CHARACTER VARIBLE WHICH SPECIFIES
C                                WHETHER A TRAILING BLANK (OR BLANKS)
C                                IS TO BE APPENDED AS A SEPARATOR
C                                AFTER THE ADDED WORD.
C                                IF IBLASW = YES, THEN APPEND A BLANK
C                                (AND INCREMENT NCNEWL BY 1)
C                                IF IBLASW = YES AND THE LAST CHARACTER
C                                OF ICWORD IS A PERIOD, QUESTION MARK,
C                                OR EXCLAMATION POINT, THEN APPEND
C                                2 SPACES (AND INCREMENT NCNEWL BY 2).
C                                IF IBLASW = NO, THEN APPEND NO BLANKS
C     OUTPUT ARGUMENTS--ICNEWL = THE CHARACTER VARIABLE
C                                TO WHICH THE APPENDING
C                                IS BEING DONE.
C                       NCNEWL = THE CURRENT TOTAL NUMBER OF
C                                CHARACTERS IN ICNEWL.
C                                NEW CHARACTERS BEING ADDED TO
C                                ICNEWL WILL START AT COLUMN NCNEWL+1
C                       IADDED = THE CHARACTER VARIABLE WHICH
C                                INDICATES WHETHER OR NOT THE
C                                SPECIFIED WORD WAS IN FACT ADDED OR NOT.
C                                IF NO OVERFLOW WAS ENCOUNTERED,
C                                (AND SO THE WORD WAS ADDED),
C                                THEN IADDED WILL BE SET TO    YES   .
C                                IF AN OVERFLOW WAS ENCOUNTERED,
C                                (AND SO THE WORD WAS NOT ADDED),
C                                THEN IADDED WILL BE SET TO    NO   .
C     CAUTION--THE INPUT VARIABLES ICNEWL AND NCNEWL
C              MAY BE CHANGED INSIDE THIS SUBROUTINE.
C     NOTE--ICNEWL AND ICWORD ARE CHARACTER*240
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.6
C     ORIGINAL VERSION--MAY 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ICNEWL
      CHARACTER*240 ICWORD
      CHARACTER*4 IBLASW
      CHARACTER*4 IADDED
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDEX'
      ISUBN2='WO  '
C
      IF(IBUGE3.EQ.'OFF'.AND.ISUBRO.NE.'ADWO')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDADWO')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDADWO')
   51 FORMAT('***** AT THE BEGINNING OF EDADWO--')
      WRITE(ICOUT,54)NCNEWL
      CALL EDWRST('EDADWO')
   54 FORMAT('NCNEWL = ',I8)
      IF(NCNEWL.GE.1)THEN
         WRITE(ICOUT,55)(ICNEWL(I:I),I=1,100)
         CALL EDWRST('EDADWO')
      ENDIF
   55 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,61)NCWORD
      CALL EDWRST('EDADWO')
   61 FORMAT('NCWORD = ',I8)
      IF(NCWORD.GE.1)THEN
         WRITE(ICOUT,62)(ICWORD(I:I),I=1,100)
         CALL EDWRST('EDADWO')
      ENDIF
   62 FORMAT('(ICWORD(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,71)ICOLL1,ICOLL2,IBLASW,IADDED
      CALL EDWRST('EDADWO')
   71 FORMAT('ICOLL1,ICOLL2,IBLASW,IADDED = ',2I8,2X,A4,2X,A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDADWO')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 11--                            **
C               **  IF POSSIBLE (THAT IS, NO OVERFLOW),  **
C               **  ADD THE SPECIFIED WORD.              **
C               *******************************************
C
      ISTART=NCNEWL+1
      ISTOP=ISTART+NCWORD-1
      IF(ISTOP.GT.ICOLL2)GOTO1150
      IF(ISTART.GT.ISTOP)GOTO1150
C
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      ICNEWL(I:I)=ICWORD(J:J)
 1100 CONTINUE
      IADDED='YES'
      NCNEWL=ISTOP
      GOTO1190
C
 1150 CONTINUE
      IADDED='NO'
      GOTO9000
C
 1190 CONTINUE
C
C               *****************************
C               **  STEP 12--              **
C               **  IF CALLED FOR,         **
C               **  ADD TRAILING BLANK(S)  **
C               *****************************
C
      IF(IBLASW.EQ.'NO')GOTO1290
C
      I=NCNEWL+1
      IF(I.GT.ICOLL2)GOTO1290
      ICNEWL(I:I)=' '
      NCNEWL=I
C
      IF(NCWORD.LE.2)GOTO1290
      IF(ICWORD(NCWORD:NCWORD).EQ.'.')GOTO1220
      IF(ICWORD(NCWORD:NCWORD).EQ.'?')GOTO1220
      IF(ICWORD(NCWORD:NCWORD).EQ.'!')GOTO1220
      GOTO1290
 1220 CONTINUE
      I=NCNEWL+1
      IF(I.GT.ICOLL2)GOTO1290
      ICNEWL(I:I)=' '
      NCNEWL=I
C
 1290 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE3.EQ.'OFF'.AND.ISUBRO.NE.'ADWO')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDADWO')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDADWO')
 9011 FORMAT('***** AT THE END       OF EDADWO--')
      WRITE(ICOUT,9014)NCNEWL
      CALL EDWRST('EDADWO')
 9014 FORMAT('NCNEWL = ',I8)
      IF(NCNEWL.GE.1)THEN
         WRITE(ICOUT,9015)(ICNEWL(I:I),I=1,100)
         CALL EDWRST('EDADWO')
      ENDIF
 9015 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,9021)NCWORD
      CALL EDWRST('EDADWO')
 9021 FORMAT('NCWORD = ',I8)
      IF(NCWORD.GE.1)THEN
         WRITE(ICOUT,9022)(ICWORD(I:I),I=1,100)
         CALL EDWRST('EDADWO')
      ENDIF
 9022 FORMAT('(ICWORD(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,9031)ICOLL1,ICOLL2,IBLASW,IADDED
      CALL EDWRST('EDADWO')
 9031 FORMAT('ICOLL1,ICOLL2,IBLASW,IADDED = ',2I8,2X,A4,2X,A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDADWO')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDBOTT
C
C     PURPOSE--GO TO BOTTOM OF FILE
C              (= LINE N+1 = "LINE" AFTER LAST REAL LINE).
C
C     NOTE--NO ARGUMENTS ARE EXPECTED OR PROCESSED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDBO'
      ISUBN2='TT  '
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'BOTT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDBOTT')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDBOTT')
   51 FORMAT('***** AT THE BEGINNING OF EDBOTT--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************
C               **  STEP 1--           **
C               **  GO TO LINE N+1     **
C               *************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'BOTT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=NUMLIN+1
      IF(IFEESW.EQ.'OFF')GOTO1159
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1151)
         CALL EDWRST('EDBOTT')
      ENDIF
 1151 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1152)
         CALL EDWRST('EDBOTT')
      ENDIF
 1152 FORMAT(10X,'[BOTTOM]')
 1159 CONTINUE
      ICURLN=ILINE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'BOTT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDBOTT')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDBOTT')
 9011 FORMAT('***** AT THE END       OF EDBOTT--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCALL
C
C     PURPOSE--DEFINE THE CALL FILE, OPEN IT, AND
C              SET THE CALL SWITCH TO ON.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUEX
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCA'
      ISUBN2='LL  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'CALL')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCALL')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCALL')
   51 FORMAT('***** AT THE BEGINNING OF EDCALL--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE NAME OF THE FILE       **
C                **  THAT IS TO BE CALLED                **
C               **  (IF NO NAME IS GIVEN,                **
C               **  THEN USE A DEFAULT FILE NAME).       **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CALL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUEX)
      IF(IFOUEX.EQ.'YES')GOTO1110
      GOTO1120
C
 1110 CONTINUE
      IFILE=' '
      IF(NUMCST.LE.0)GOTO1119
      DO1111I=1,NUMCST
      IFILE(I:I)=ISTRIN(I)
 1111 CONTINUE
 1119 CONTINUE
      ISTAT='UNKNOWN'
      GOTO1190
C
 1120 CONTINUE
      IFILE=ICALNA
      ISTAT=ICALST
      GOTO1190
C
 1190 CONTINUE
      IOUNIT=ICALNU
      IFORM=ICALFO
      IACCES=ICALAC
      IREWR=ICALRW
      ISUBN0='CALP'
      IERRFI='NO'
C
      ICALNA=IFILE
      ICALST=ISTAT
C
C               *********************
C               **  STEP 2--       **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='2'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'CALL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')IERROR=IERRFI
      IF(IERRFI.EQ.'YES')GOTO9000
C
C               ***********************************
C               **  STEP 3--                     **
C               **  SET THE CALL SWITCH TO   ON  **
C               ***********************************
C
C
      ISTEPN='3'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'CALL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICCALL='ON'
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'CALL')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCALL')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCALL')
 9011 FORMAT('***** AT THE END       OF EDCALL--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDCALL')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDCALL')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDCALL')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCALL')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCEN2(IOLDLI,NUMCOL,INEWLI,NUMCNL)
C
C     PURPOSE--CENTER THE LINE IN IOLDLI(.).
C              THE CENTERED LINE WILL BE PLACED IN INEWLI(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      DIMENSION IOLDLI(*)
      DIMENSION INEWLI(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCE'
      ISUBN2='N2  '
C
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CEN2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCEN2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCEN2')
   51 FORMAT('***** AT THE BEGINNING OF EDCEN2--')
      WRITE(ICOUT,71)NUMCOL
      CALL EDWRST('EDCEN2')
   71 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,72)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDCEN2')
   72 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,73)NUMCNL
      CALL EDWRST('EDCEN2')
   73 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,74)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDCEN2')
   74 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,81)ICENTC
      CALL EDWRST('EDCEN2')
   81 FORMAT('ICENTC = ',I8)
   90 CONTINUE
C
C               ************************************************************
C               **  STEP 1--                                              **
C               **  DETERMINE THE FIRST NON-BLANK CHARACTER IN IOLDLI(.)  **
C               ************************************************************
C
      I2=0
      IF(NUMCOL.LE.0)GOTO1190
      DO1100I=1,NUMCOL
      I2=I
      IF(IOLDLI(I2).NE.' ')GOTO1190
 1100 CONTINUE
      I2=NUMCOL+1
 1190 CONTINUE
      IFIRST=I2
C
C               ************************************************************
C               **  STEP 2--                                              **
C               **  DETERMINE THE LAST  NON-BLANK CHARACTER IN IOLDLI(.)  **
C               ************************************************************
C
      IREV=0
      IF(NUMCOL.LE.0)GOTO1290
      DO1200I=1,NUMCOL
      IREV=NUMCOL-I+1
      IF(IOLDLI(IREV).NE.' ')GOTO1290
 1200 CONTINUE
      IREV=0
 1290 CONTINUE
      ILAST=IREV
C
C               ***************************************************************
C               **  STEP 3--
C               **  DETERMINE THE CENTER OF THE NON-BLANK STRING IN IOLDLI(.)
C               ***************************************************************
C
      ICENT1=(IFIRST+ILAST)/2
C
C               ******************************************************
C               **  STEP 4--                                        **
C               **  DETERMINE THE DESIRED CENTER OF THE NEW STRING  **
C               ******************************************************
C
      ICENT2=ICENTC
C
C               ********************************
C               **  STEP 5--                  **
C               **  BLANK OUT THE NEW STRING  **
C               ********************************
C
      NUMCNL=1
      DO1500I=1,240
      INEWLI(I)=' '
 1500 CONTINUE
C
C               **************************************
C               **  STEP 6--                        **
C               **  FORM THE NEW (CENTERED) STRING  **
C               **************************************
C
      ISHIFT=ICENT2-ICENT1
C
      INEW=1
      IF(IFIRST.LE.0)GOTO1690
      IF(IFIRST.GT.ILAST)GOTO1690
      DO1600IOLD=IFIRST,ILAST
      INEW=IOLD+ISHIFT
      IF(INEW.LT.IMLIM1)GOTO1600
      IF(INEW.GT.IMLIM2)GOTO1600
      INEWLI(INEW)=IOLDLI(IOLD)
 1600 CONTINUE
 1690 CONTINUE
      NUMCNL=INEW
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CEN2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCEN2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCEN2')
 9011 FORMAT('***** AT THE END       OF EDCEN2--')
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDCEN2')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDCEN2')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDCEN2')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDCEN2')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,9041)IFIRST,ILAST,ICENT1,ICENT2,ISHIFT
      CALL EDWRST('EDCEN2')
 9041 FORMAT('IFIRST,ILAST,ICENT1,ICENT2,ISHIFT = ',5I8)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCENT
C
C     PURPOSE--CENTER CURRENT LINE
C              (AND NEXT IARG1-1 LINES)
C              SO AS TO BE BETWEEN MARGIN LIMITS.
C     COMMAND SYNTAX--CENTER <N> WHERE <N> IS BLANK OR SOME NUMBER.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      DIMENSION IOLDLI(240)
      DIMENSION INEWLI(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCE'
      ISUBN2='NT  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CENT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCENT')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCENT')
   51 FORMAT('***** AT THE BEGINNING OF EDCENT--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 1--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE CENTERED.  **
C               ************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=ICURLN
C
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      GOTO1120
C
 1100 CONTINUE
      ISTART=ICURLN
      ISTOP=ICURLN
      GOTO1190
C
 1110 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      ISTART=ICURLN
      ISTOP=ISTART+IARG1-1
      GOTO1190
C
 1120 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      IARG2=NUMLIN+1
      IF(IARGT(2).EQ.'NUMB')IARG2=IARG(2)
      ISTART=IARG1
      ISTOP=IARG2
      GOTO1190
C
 1190 CONTINUE
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD2.LT.IHOLD1)ISTART=IHOLD2
      IF(IHOLD2.LT.IHOLD1)ISTOP=IHOLD1
C
C               ********************************************
C               **  STEP 21--                             **
C               **  LOOP THROUGH THE LINES TO BE CENTERED **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ISTART-1
 2100 CONTINUE
      ILINE=ILINE+1
      IF(ILINE.LE.ISTOP)GOTO2910
      ILINE=ISTOP
      GOTO2900
 2910 CONTINUE
      ICURLN=ILINE
      IF(ILINE.LT.1)GOTO2100
      IF(ILINE.GT.NUMLIN)GOTO2900
C
C               *****************************
C               **  STEP 22--              **
C               **  COPY THE OLD    LINE   **
C               *****************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      I=0
      DO2200J=J1,J2
      I=I+1
      IOLDLI(I)=ICHA(J)
 2200 CONTINUE
      NUMCOL=I
C
C               ***********************************
C               **  STEP 23--                    **
C               **  APPLY THE DESIRED CENTERING  **
C               **  TO THE TARGET LINE,          **
C               **  SO AS TO CREATE              **
C               **  A NEW LINE.                  **
C               ***********************************
C
      ISTEPN='23'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDCEN2(IOLDLI,NUMCOL,INEWLI,NUMCNL)
C
C               ***************************************
C               **  STEP 24--                        **
C               **  DETERMINE IF ROOM EXISTS IN THE  **
C               **  MAIN INTERNAL CHARACTER ARRAY    **
C               **  FOR THE NEW CENTERED LINE.       **
C               ***************************************
C
      ISTEPN='24'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH9=NUMCHA+NUMCNL
      CALL EDCHSI(NUMCH9)
      IF(IERROR.EQ.'YES')GOTO9000
C
 2450 CONTINUE
      IF(ILINE.LT.1)GOTO2460
      IF(ILINE.GT.NUMLIN)GOTO2470
      GOTO2490
C
 2460 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2461)
         CALL EDWRST('EDCENT')
      ENDIF
 2461 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2462)
         CALL EDWRST('EDCENT')
      ENDIF
 2462 FORMAT(10X,'[TOP]')
 2469 CONTINUE
      GOTO2100
C
 2470 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2471)
         CALL EDWRST('EDCENT')
      ENDIF
 2471 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2472)
         CALL EDWRST('EDCENT')
      ENDIF
 2472 FORMAT(10X,'[BOTTOM]')
 2479 CONTINUE
      GOTO9000
C
 2490 CONTINUE
C
C               ***************************************
C               **  STEP 25--                        **
C               **  UPDATE THE MAIN CHARACTER ARRAY  **
C               **  WITH THE NEW     LINE.           **
C               ***************************************
C
      ISTEPN='25'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J1=NUMCHA+1
      N1=NUMCNL
      J2=J1+(N1-1)
C
      NUMRP1=NUMROW+1
      IROW=NUMRP1
      IPOINT(ILINE)=IROW
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO2500J=J1,J2
      I=I+1
      ICHA(J)=INEWLI(I)
 2500 CONTINUE
      NUMROW=NUMRP1
      NUMCHA=J2
C
      IF(IFEESW.EQ.'OFF')GOTO2629
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2621)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCENT')
      ENDIF
 2621 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2622)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCENT')
      ENDIF
 2622 FORMAT(I6,':',3X,230A1)
 2629 CONTINUE
C
      GOTO2100
C
 2900 CONTINUE
      IF(ILINE.LE.NUMLIN)GOTO2919
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2911)
         CALL EDWRST('EDCENT')
      ENDIF
 2911 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2912)
         CALL EDWRST('EDCENT')
      ENDIF
 2912 FORMAT(10X,'[BOTTOM]')
 2919 CONTINUE
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CENT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCENT')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCENT')
 9011 FORMAT('***** AT THE END       OF EDCENT--')
      WRITE(ICOUT,999)
      CALL EDWRST('EDCENT')
      WRITE(ICOUT,9012)IWIDTH
      CALL EDWRST('EDCENT')
 9012 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9013)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDCENT')
 9013 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDCENT')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDCENT')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDCENT')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDCENT')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,9041)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDCENT')
 9041 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9042)J1,N1,J2
      CALL EDWRST('EDCENT')
 9042 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCENT')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCHA2(IGLOBA,ISTRI1,NUMCS1,ISTRI2,NUMCS2,
     1IOLDLI,NUMCOL,INEWLI,NUMCNL,IFOUST)
C
C     PURPOSE--SCAN IOLDLI(.) FOR THE STRING IN ISTRI1(.).
C              IF FOUND, THEN FORM THE NEW LINE INEWLI(.)
C              WHICH IS THE SAME AS IOLDLI(.) EXCEPT
C              ISTRI1(.) HAS BEEN CHANGED TO ISTRI2(.).
C              IF NOT FOUND, THEN INEWLI(.) = IOLDLI(.)
C     NOTE--IF IGLOBA='ON', THEN DO SO FOR ALL STRINGS IN IOLDLI(.),
C           OTHERWISE DO ONLY FOR THE FIRST STRING IN IOLDLI(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED--APRIL 1990 FORCE IMASK TO BE NON-BLANK
C     UPDATED--APRIL 1990 ALLOW FOR (EXACTLY 1) WORD CHANGE VIA $ MASK CHAR
C     UPDATED--MARCH 1992 UNDO APRIL 1990 SINCE TRUNCATING A LINE
C     UPDATED--APRIL 1992 CHANGE MASK CHARACTER FROM $ TO ~
C     UPDATED--SEPTEMBER 1994 CHANGE MASK CHARACTER FROM ~ TO `
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IGLOBA
C
      CHARACTER*1 ISTRI1
      CHARACTER*1 ISTRI2
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      CHARACTER*4 IFOUST
C
      CHARACTER*1 IMASKV
CCCCC THE FOLLOWING 3 LINES WERE ADDED APRIL 1990
CCCCC TO ALLOW FOR WORD MASK       APRIL 1990
      CHARACTER*4 IMWSW
      CHARACTER*1 IMASKW
      CHARACTER*1 IMASWV
C
      DIMENSION ISTRI1(*)
      DIMENSION ISTRI2(*)
      DIMENSION IOLDLI(*)
      DIMENSION INEWLI(*)
C
      DIMENSION IMASKV(240)
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1990
CCCCC TO ALLOW FOR WORD MASK       APRIL 1990
      DIMENSION IMASWV(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCH'
      ISUBN2='A2  '
C
      JPIM1=(-999)
C
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1990
CCCCC TO ALLOW FOR WORD MASK       APRIL 1990
      IMWSW='OFF'
CCCCC THE MASK CHARACTER WAS CHANGED FROM ~ TO `
CCCCC THE FOLLOWING LINE WAS CHANGED   SEPTEMBER 1994
CCCCC TO CHANGE THE MASK CHARACTER FROM ~ TO `  SEPTEMBER 1994
CCCCC IMASKW='$'
      IMASKW='`'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CHA2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHA2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCHA2')
   51 FORMAT('***** AT THE BEGINNING OF EDCHA2--')
      WRITE(ICOUT,52)IGLOBA
      CALL EDWRST('EDCHA2')
   52 FORMAT('IGLOBA = ',A4)
      WRITE(ICOUT,61)NUMCS1
      CALL EDWRST('EDCHA2')
   61 FORMAT('NUMCS1 = ',I8)
      WRITE(ICOUT,62)(ISTRI1(I),I=1,NUMCS1)
      CALL EDWRST('EDCHA2')
   62 FORMAT('(ISTRI1(I),I=1,NUMCS1) = ',110A1)
      WRITE(ICOUT,63)NUMCS2
      CALL EDWRST('EDCHA2')
   63 FORMAT('NUMCS2 = ',I8)
      WRITE(ICOUT,64)(ISTRI2(I),I=1,NUMCS2)
      CALL EDWRST('EDCHA2')
   64 FORMAT('(ISTRI2(I),I=1,NUMCS2) = ',110A1)
      WRITE(ICOUT,71)NUMCOL
      CALL EDWRST('EDCHA2')
   71 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,72)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDCHA2')
   72 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,73)NUMCNL
      CALL EDWRST('EDCHA2')
   73 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,74)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDCHA2')
   74 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,81)IFOUST
      CALL EDWRST('EDCHA2')
   81 FORMAT('IFOUST = ',A4)
      WRITE(ICOUT,82)ICLIM1,ICLIM2
      CALL EDWRST('EDCHA2')
   82 FORMAT('ICLIM1,ICLIM2 = ',2I8)
   90 CONTINUE
C
      IFOUST='NO'
      NUMCNL=NUMCOL
      IF(NUMCOL.LE.0)GOTO9000
      DO1100I=1,NUMCOL
      INEWLI(I)=IOLDLI(I)
 1100 CONTINUE
C
C               *******************************
C               **  STEP 11--                **
C               **  TREAT THE CASE WHEN      **
C               **  STRING 1 IS NULL         **
C               **  (THAT IS, NUMCS1 <=  0)  **
C               *******************************
C
      IF(NUMCS1.LE.0)GOTO1200
      GOTO1900
C
 1200 CONTINUE
      IFOUST='YES'
C
      K=0
C
C     STEP 11.1--COPY OUT TO THE COLUMN LIMITS
C
      IMIN=1
      IMAX=ICLIM1-1
      IF(IMIN.GT.IMAX)GOTO1219
      DO1210I=IMIN,IMAX
      K=K+1
      INEWLI(K)=' '
      IF(I.LE.NUMCOL)INEWLI(K)=IOLDLI(I)
CCCCC WRITE(ICOUT,777)I,K,INEWLI(K),IMIN,IMAX
CC777 FORMAT('I,K,INEWLI(I),IMIN,IMAX = ',2I8,2X,A1,2I8)
 1210 CONTINUE
 1219 CONTINUE
C
C     STEP 11.2--COPY STRING 2 AT THE COLUMN LIMIT
C
      IMIN=ICLIM1
      IMAX=ICLIM1+NUMCS2-1
      IF(IMIN.GT.IMAX)GOTO1229
      L=0
      DO1220I=IMIN,IMAX
      K=K+1
      L=L+1
      INEWLI(K)=ISTRI2(L)
CCCCC WRITE(ICOUT,777)I,K,INEWLI(K),IMIN,IMAX
 1220 CONTINUE
 1229 CONTINUE
C
C     STEP 11.3--PUSH (COPY) THE OLD LINE TO THE RIGHT
C
      IMIN=ICLIM1+NUMCS2
      IMAX=ICLIM2
      IF(IMIN.GT.IMAX)GOTO1239
      DO1230I=IMIN,IMAX
      I2=I-NUMCS2
      IF(I2.GT.NUMCOL)GOTO1239
      K=K+1
      INEWLI(K)=IOLDLI(I2)
CCCCC WRITE(ICOUT,777)I,K,INEWLI(K),IMIN,IMAX
 1230 CONTINUE
 1239 CONTINUE
C
C     STEP 11.4--COPY BEYOND THE COLUMN LIMITS
C
      IMIN=ICLIM2+1
      IMAX=240
      IF(IMIN.GT.IMAX)GOTO1249
      DO1240I=IMIN,IMAX
      IF(I.GT.NUMCOL)GOTO1249
      K=K+1
      INEWLI(K)=IOLDLI(I)
CCCCC WRITE(ICOUT,777)I,K,INEWLI(K),IMIN,IMAX
 1240 CONTINUE
 1249 CONTINUE
C
      NUMCNL=K
      GOTO9000
C
 1900 CONTINUE
C
C               **********************************
C               **  STEP 21--                   **
C               **  TREAT THE CASE WHEN         **
C               **  THE OLD STRING IS NON-NULL  **
C               **  (THAT IS, NUMCS1 >= 1)      **
C               **********************************
C
C    I IS THE INDEX OF STRING 1
C     J IS THE INDEX OF THE OLD LINE
C     K IS THE INDEX OF THE NEW LINE
C
      J=0
      K=0
CCCCC THE FOLLOWING 2 LINES WERE ADDED APRIL 1990
CCCCC TO ALLOW FOR WORD MASK       APRIL 1990
      IMW=0
      IMWSW='OFF'
 2100 CONTINUE
      J=J+1
      IF(J.GT.NUMCOL)GOTO2900
      IF(J.GT.ICLIM2)GOTO2900
C
      IF(J.LT.ICLIM1)GOTO2210
      IM=0
      DO2200I=1,NUMCS1
      JPIM1=J+(I-1)
CCCCC THE FOLLOWING 1 LINE WAS ADDED APRIL 1990
CCCCC TO ALLOW FOR WORD MASK       APRIL 1990
      IF(ISTRI1(I).EQ.IMASKW.AND.IOLDLI(JPIM1).NE.' ')GOTO2207
      IF(ISTRI1(I).EQ.IMASK)GOTO2200
CCCCC THE FOLLOWING LINE WAS FIXED      APRIL 1990
CCCCC TO FORCE IMASK TO BE NON-BLANK    APRIL 1990
CCCCC IF(ISTRI1(I).EQ.IMASK)GOTO2205
      IF(ISTRI1(I).EQ.IMASK.AND.IOLDLI(JPIM1).NE.' ')GOTO2205
      IF(IOLDLI(JPIM1).EQ.ISTRI1(I))GOTO2200
      GOTO2210
 2205 CONTINUE
      IM=IM+1
      IMASKV(IM)=IOLDLI(JPIM1)
      GOTO2200
CCCCC THE FOLLOWING 4 LINES WERE ADDED APRIL 1990
CCCCC TO ALLOW FOR WORD MASK       APRIL 1990
 2207 CONTINUE
      IMWSW='ON'
      IMW=IMW+1
      NMW=IMW
      IMASWV(IMW)=IOLDLI(JPIM1)
CCCCC WRITE(ICOUT,888)J,K,I,IMW
CC888 FORMAT('J,K,I,IMW = ',4I8)
      J=J+1
      JPIM1=J+(I-1)
      IF(JPIM1.GT.NUMCOL)GOTO2290
      IF(JPIM1.GT.ICLIM2)GOTO2290
      IF(IOLDLI(JPIM1).NE.' ')GOTO2207
      GOTO2290
 2200 CONTINUE
      NM=IM
      NMW=IMW
      GOTO2290
C
 2210 CONTINUE
      K=K+1
      INEWLI(K)=IOLDLI(J)
      GOTO2100
C
 2290 CONTINUE
C
      IFOUST='YES'
      IF(NUMCS2.LE.0)GOTO2350
      IM=0
      DO2300I=1,NUMCS2
CCCCC THE FOLLOWING 9 LINES WERE ADDED APRIL 1990
CCCCC TO ALLOW FOR WORD MASK       APRIL 1990
      IF(ISTRI2(I).EQ.IMASKW)GOTO2310
      GOTO2319
 2310 CONTINUE
      DO2311L=1,NMW
      K=K+1
      INEWLI(K)=IMASWV(L)
 2311 CONTINUE
      J=J-1
      GOTO2300
 2319 CONTINUE
      K=K+1
      IF(IM.GT.NM)IM=0
      IF(ISTRI2(I).EQ.IMASK)IM=IM+1
      IF(ISTRI2(I).EQ.IMASK)INEWLI(K)=IMASKV(IM)
      IF(ISTRI2(I).EQ.IMASK)GOTO2300
      INEWLI(K)=ISTRI2(I)
 2300 CONTINUE
 2350 CONTINUE
      J=JPIM1
      IF(IGLOBA.EQ.'ON')GOTO2100
C
 2400 CONTINUE
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1990
CCCCC J=J+1
      IF(IMWSW.EQ.'ON')J=J-1
 2410 CONTINUE
      J=J+1
      IF(J.GT.NUMCOL)GOTO2900
      K=K+1
      INEWLI(K)=IOLDLI(J)
      GOTO2410
C
 2900 CONTINUE
      NUMCNL=K
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CHA2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHA2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCHA2')
 9011 FORMAT('***** AT THE END       OF EDCHA2--')
      WRITE(ICOUT,9012)IGLOBA
      CALL EDWRST('EDCHA2')
 9012 FORMAT('IGLOBA = ',A4)
      WRITE(ICOUT,9021)NUMCS1
      CALL EDWRST('EDCHA2')
 9021 FORMAT('NUMCS1 = ',I8)
      WRITE(ICOUT,9022)(ISTRI1(I),I=1,NUMCS1)
      CALL EDWRST('EDCHA2')
 9022 FORMAT('(ISTRI1(I),I=1,NUMCS1) = ',100A1)
      WRITE(ICOUT,9023)NUMCS2
      CALL EDWRST('EDCHA2')
 9023 FORMAT('NUMCS2 = ',I8)
      WRITE(ICOUT,9024)(ISTRI2(I),I=1,NUMCS2)
      CALL EDWRST('EDCHA2')
 9024 FORMAT('(ISTRI2(I),I=1,NUMCS2) = ',100A1)
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDCHA2')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDCHA2')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',100A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDCHA2')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDCHA2')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',100A1)
      WRITE(ICOUT,9041)IFOUST
      CALL EDWRST('EDCHA2')
 9041 FORMAT('IFOUST = ',A4)
      WRITE(ICOUT,9042)ICLIM1,ICLIM2
      CALL EDWRST('EDCHA2')
 9042 FORMAT('ICLIM1,ICLIM2 = ',2I8)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCHAN(IGLOBA)
C
C     PURPOSE--CHANGE A LINE OF TEXT
C              THE FIRST STRING WILL BE REPLACED BY THE SECOND STRING
C     COMMAND SYNTAX--C /<STRING 1>/<STRING 2>/
C     NOTE--IF THE FINAL DELIMITER IS OMITTED,
C           IT WILL AUTOMATICALLY BE SUPPLIED BY THE EDITOR.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED         --APRIL 1990 FIX BOMB FOR C COMMAND WITH NO ARGS
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IGLOBA
C
      CHARACTER*1 ISTRIN
      CHARACTER*1 ISTRI1
      CHARACTER*1 ISTRI2
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      CHARACTER*1 IDELCH
      CHARACTER*4 IFOUEX
      CHARACTER*4 IFOUST
      CHARACTER*4 IFOUSA
C
      DIMENSION ISTRIN(240)
      DIMENSION ISTRI1(240)
      DIMENSION ISTRI2(240)
      DIMENSION IOLDLI(240)
      DIMENSION INEWLI(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCH'
      ISUBN2='AN  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IFOUEX='NO'
      IFOUST='NO'
      IFOUSA='NO'
      CHANGE='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CHAN')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHAN')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCHAN')
   51 FORMAT('***** AT THE BEGINNING OF EDCHAN--')
      WRITE(ICOUT,52)IGLOBA
      CALL EDWRST('EDCHAN')
   52 FORMAT('IGLOBA = ',A4)
      WRITE(ICOUT,61)NUMCCS
      CALL EDWRST('EDCHAN')
   61 FORMAT('NUMCCS = ',I8)
      IF(NUMCCS.GE.1)WRITE(ICOUT,62)(ICHAST(I),I=1,NUMCCS)
      IF(NUMCCS.GE.1)CALL EDWRST('EDCHAN')
   62 FORMAT('(ICHAST(I),I=1,NUMCCS) = ',80A1)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **********************************
C               **  STEP 10--                   **
C               **  SAVE THE CURRENT POINTERS   **
C               **  (FOR POSSIBLE LATER USE     **
C               **  BY THE   UNDO     COMMAND)  **
C               **********************************
C
      ISTEPN='10'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDSACP
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE FULL CHANGE STRING     **
C               **  ON THE COMMAND LINE                  **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUEX)
      IF(IFOUEX.EQ.'YES')GOTO1190
C
      NUMCST=NUMCCS
      IF(NUMCCS.LE.0)GOTO1159
      DO1155I=1,NUMCCS
      ISTRIN(I)=ICHAST(I)
 1155 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1190 CONTINUE
C
C               **************************************
C               **  STEP 12--                       **
C               **  IF THE STRING IS NEW,           **
C               **  COPY THE STRING INTO ILOCST(.)  **
C               **  FOR FUTURE USE BY SUBSEQUENT    **
C               **  LOCATE COMMANDS.                **
C               **************************************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFOUEX.EQ.'NO')GOTO1290
      NUMCCS=NUMCST
      IF(NUMCST.LE.0)GOTO1290
      DO1210I=1,NUMCST
      ICHAST(I)=ISTRIN(I)
 1210 CONTINUE
 1290 CONTINUE
C
C               *****************************
C               **  STEP 13--              **
C               **  EXTRACT THE DELIMITER  **
C               *****************************
C
      ISTEPN='13'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IDELCH=ISTRIN(1)
      ILOCD1=1
C
C               ***********************************
C               **  STEP 14--                    **
C               **  EXTRACT THE "OLD" SUBSTRING  **
C               ***********************************
C
      ISTEPN='14'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ILOCD2=ILOCD1
      IMIN=ILOCD1+1
      IF(NUMCST.LT.IMIN)GOTO1490
      DO1400I=IMIN,NUMCST
      I2=I
      J=J+1
      IF(ISTRIN(I).EQ.IDELCH)GOTO1450
      ISTRI1(J)=ISTRIN(I)
 1400 CONTINUE
      ILOCD2=NUMCST+1
      GOTO1490
 1450 CONTINUE
      J=J-1
      ILOCD2=I2
 1490 CONTINUE
      NUMCS1=J
C
C               ***********************************
C               **  STEP 15--                    **
C               **  EXTRACT THE "NEW" SUBSTRING  **
C               ***********************************
C
      ISTEPN='15'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ILOCD3=ILOCD2
      IMIN=ILOCD2+1
      IF(NUMCST.LT.IMIN)GOTO1590
      DO1500I=IMIN,NUMCST
      I2=I
      J=J+1
      IF(ISTRIN(I).EQ.IDELCH)GOTO1550
      ISTRI2(J)=ISTRIN(I)
 1500 CONTINUE
      ILOCD3=NUMCST+1
      GOTO1590
 1550 CONTINUE
      J=J-1
      ILOCD3=I2
 1590 CONTINUE
      NUMCS2=J
C
C               *****************************************************
C               **  STEP 16--                                      **
C               **  DETERMINE IF THERE IS A NUMBER DESIGNATION     ** AT
C               **  THE END OF THE COMMAND LINE                 **
C               **  (WHICH WOULD SPECIFY THAT                      **
C               **  MULTIPLE LINES SHOULD BE CHANGED).             **
C               **  THUS DETERMINE THE START LINE FOR THE CHANGE,  **
C               **  AND THE STOP LINE FOR THE CHANGE.              **
C               *****************************************************
C
      ISTEPN='16'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      NUMLTC=1
      IF(ILOCD3.EQ.NUMCST)GOTO1690
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1990 FOR MY 386
      IF(NUMARG.LE.0)GOTO1690
      IF(IARGT(NUMARG).EQ.'WORD'.AND.IHARG(NUMARG).EQ.IINFIN)
     1NUMLTC=100000
      IF(IARGT(NUMARG).EQ.'NUMB')NUMLTC=IARG(NUMARG)
 1690 CONTINUE
      ISTOP=ISTART+(NUMLTC-1)
C
C               ********************************************
C               **  STEP 21--                             **
C               **  LOOP THROUGH THE LINES TO BE CHANGED  **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ISTART-1
 2100 CONTINUE
      ILINE=ILINE+1
      IF(ILINE.LE.ISTOP)GOTO2910
      ILINE=ISTOP
      GOTO2900
 2910 CONTINUE
      ICURLN=ILINE
      IF(ILINE.LT.1)GOTO2100
      IF(ILINE.GT.NUMLIN)GOTO2900
C
C               *****************************
C               **  STEP 22--              **
C               **  COPY THE OLD    LINE   **
C               *****************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      I=0
      DO2200J=J1,J2
      I=I+1
      IOLDLI(I)=ICHA(J)
 2200 CONTINUE
      NUMCOL=I
C
C               ********************************
C               **  STEP 23--                 **
C               **  APPLY THE DESIRED CHANGE  **
C               **  TO THE TARGET LINE,       **
C               **  SO AS TO CREATE           **
C               **  A NEW LINE.               **
C               ********************************
C
      CALL EDCHA2(IGLOBA,ISTRI1,NUMCS1,ISTRI2,NUMCS2,
     1IOLDLI,NUMCOL,INEWLI,NUMCNL,IFOUST)
C
      IF(IFOUST.EQ.'YES')IFOUSA='YES'
      IF(IFOUST.EQ.'YES')CHANGE='YES'
      IF(IFOUST.EQ.'NO')GOTO2100
C
C               ***************************************
C               **  STEP 24--                        **
C               **  DETERMINE IF ROOM EXISTS IN THE  **
C               **  MAIN INTERNAL CHARACTER ARRAY    **
C               **  FOR THE NEW CHANGED LINE.        **
C               ***************************************
C
      ISTEPN='24'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH2=NUMCHA+NUMCNL
      CALL EDCHSI(NUMCH2)
      IF(IERROR.EQ.'YES')GOTO9000
C
 2450 CONTINUE
      IF(ILINE.LT.1)GOTO2460
      IF(ILINE.GT.NUMLIN)GOTO2470
      GOTO2490
C
 2460 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2461)
         CALL EDWRST('EDCHAN')
      ENDIF
 2461 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2462)
         CALL EDWRST('EDCHAN')
      ENDIF
 2462 FORMAT(10X,'[TOP]')
 2469 CONTINUE
      GOTO2100
C
 2470 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2471)
         CALL EDWRST('EDCHAN')
      ENDIF
 2471 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2472)
         CALL EDWRST('EDCHAN')
      ENDIF
 2472 FORMAT(10X,'[BOTTOM]')
 2479 CONTINUE
      GOTO9000
C
 2490 CONTINUE
C
C               ***************************************
C               **  STEP 25--                        **
C               **  UPDATE THE MAIN CHARACTER ARRAY  **
C               **  WITH THE NEW     LINE.           **
C               ***************************************
C
      ISTEPN='25'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CHAN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J1=NUMCHA+1
      N1=NUMCNL
      J2=J1+(N1-1)
C
      NUMRP1=NUMROW+1
      IROW=NUMRP1
      IPOINT(ILINE)=IROW
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO2500J=J1,J2
      I=I+1
      ICHA(J)=INEWLI(I)
 2500 CONTINUE
      NUMROW=NUMRP1
      NUMCHA=J2
C
      IF(IFOUST.EQ.'NO')GOTO2629
      IF(IFEESW.EQ.'OFF')GOTO2629
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2621)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCHAN')
      ENDIF
 2621 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2622)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCHAN')
      ENDIF
 2622 FORMAT(I6,':',3X,230A1)
 2629 CONTINUE
C
      GOTO2100
C
 2900 CONTINUE
      IF(ILINE.LE.NUMLIN)GOTO2919
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2911)
         CALL EDWRST('EDCHAN')
      ENDIF
 2911 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2912)
         CALL EDWRST('EDCHAN')
      ENDIF
 2912 FORMAT(10X,'[BOTTOM]')
 2919 CONTINUE
C
      IF(IFOUSA.EQ.'YES')GOTO2929
      IF(IFEESW.EQ.'OFF')GOTO2929
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2921)
         CALL EDWRST('EDCHAN')
      ENDIF
 2921 FORMAT('STRING NOT FOUND')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2922)
         CALL EDWRST('EDCHAN')
      ENDIF
 2922 FORMAT(10X,'STRING NOT FOUND')
 2929 CONTINUE
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CHAN')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHAN')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCHAN')
 9011 FORMAT('***** AT THE END       OF EDCHAN--')
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHAN')
      WRITE(ICOUT,9012)IWIDTH
      CALL EDWRST('EDCHAN')
 9012 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9013)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDCHAN')
 9013 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9015)IGLOBA
      CALL EDWRST('EDCHAN')
 9015 FORMAT('IGLOBA = ',A4)
      WRITE(ICOUT,9016)NUMCST
      CALL EDWRST('EDCHAN')
 9016 FORMAT('NUMCST = ',I8)
      WRITE(ICOUT,9017)(ISTRIN(I),I=1,NUMCST)
      CALL EDWRST('EDCHAN')
 9017 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,9021)NUMCS1
      CALL EDWRST('EDCHAN')
 9021 FORMAT('NUMCS1 = ',I8)
      WRITE(ICOUT,9022)(ISTRI1(I),I=1,NUMCS1)
      CALL EDWRST('EDCHAN')
 9022 FORMAT('(ISTRI1(I),I=1,NUMCS1) = ',110A1)
      WRITE(ICOUT,9023)NUMCS2
      CALL EDWRST('EDCHAN')
 9023 FORMAT('NUMCS2 = ',I8)
      WRITE(ICOUT,9024)(ISTRI2(I),I=1,NUMCS2)
      CALL EDWRST('EDCHAN')
 9024 FORMAT('(ISTRI2(I),I=1,NUMCS2) = ',110A1)
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDCHAN')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDCHAN')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDCHAN')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDCHAN')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,9035)IFOUEX,IFOUST,IFOUSA,CHANGE
      CALL EDWRST('EDCHAN')
 9035 FORMAT('IFOUEX,IFOUST,IFOUSA = ',A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,9041)J1,N1,J2
      CALL EDWRST('EDCHAN')
 9041 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9042)ICURLN
      CALL EDWRST('EDCHAN')
 9042 FORMAT('ICURLN = ',I8)
      WRITE(ICOUT,9051)NUMCCS
      CALL EDWRST('EDCHAN')
 9051 FORMAT('NUMCCS = ',I8)
      IF(NUMCCS.GE.1)WRITE(ICOUT,9052)(ICHAST(I),I=1,NUMCCS)
      IF(NUMCCS.GE.1)CALL EDWRST('EDCHAN')
 9052 FORMAT('(ICHAST(I),I=1,NUMCCS) = ',80A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHAN')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCHSI(NUMCHX)
C
C     PURPOSE--CHECK TO SEE IF A NEW LINE WILL FIT
C              IN THE INTERNAL WORKSPACE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      ISUBN1='EDCH'
      ISUBN2='SI  '
C
      IFOUND='YES'
      IERROR='NO'
C
C
      IF(IBUGE3.EQ.'OFF'.AND.ISUBRO.NE.'CHSI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHSI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCHSI')
   51 FORMAT('***** AT THE BEGINNING OF EDCHSI--')
      WRITE(ICOUT,52)NUMLIN,MAXEDL
      CALL EDWRST('EDCHSI')
   52 FORMAT('NUMLIN,MAXEDL = ',2I8)
      WRITE(ICOUT,53)NUMROW,MAXROW
      CALL EDWRST('EDCHSI')
   53 FORMAT('NUMROW,MAXROW = ',2I8)
      WRITE(ICOUT,54)NUMCHA,MAXEDC,NUMCHX
      CALL EDWRST('EDCHSI')
   54 FORMAT('NUMCHA,MAXEDC,NUMCHX = ',3I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************************
C               **  STEP 1--                       **
C               **  CHECK TO SEE IF THE WORKSPACE  **
C               **  WILL OVERFLOW                  **
C               *************************************
C
      IF(NUMLIN.GE.MAXEDL)GOTO2410
      IF(NUMROW.GE.MAXROW)GOTO2420
      IF(NUMCHA.GE.MAXEDC)GOTO2430
      IF(NUMCHX.GT.MAXEDC)GOTO2440
      GOTO2490
C
 2410 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHSI')
      WRITE(ICOUT,2411)
      CALL EDWRST('EDCHSI')
 2411 FORMAT('***** ERROR IN EDCHSI--')
      WRITE(ICOUT,2412)
      CALL EDWRST('EDCHSI')
 2412 FORMAT('      NO ROOM LEFT FOR MORE LINES.')
      WRITE(ICOUT,2413)NUMLIN,MAXEDL
      CALL EDWRST('EDCHSI')
 2413 FORMAT('      NUMLIN (',I8,') >= MAXEDL (',I8,')')
      IERROR='YES'
      GOTO9000
C
 2420 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHSI')
      WRITE(ICOUT,2421)
      CALL EDWRST('EDCHSI')
 2421 FORMAT('***** ERROR IN EDCHSI--')
      WRITE(ICOUT,2422)
      CALL EDWRST('EDCHSI')
 2422 FORMAT('      NO ROOM LEFT FOR MORE ROWS.')
      WRITE(ICOUT,2423)NUMROW,MAXROW
      CALL EDWRST('EDCHSI')
 2423 FORMAT('      NUMROW (',I8,') >= MAXROW (',I8,')')
      IERROR='YES'
      GOTO9000
C
 2430 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHSI')
      WRITE(ICOUT,2431)
      CALL EDWRST('EDCHSI')
 2431 FORMAT('***** ERROR IN EDCHSI--')
      WRITE(ICOUT,2432)
      CALL EDWRST('EDCHSI')
 2432 FORMAT('      NO ROOM LEFT FOR MORE CHARACTERS.')
      WRITE(ICOUT,2433)NUMCHA,MAXEDC
      CALL EDWRST('EDCHSI')
 2433 FORMAT('      NUMCHA (',I8,') >= MAXEDC (',I8,')')
      IERROR='YES'
      GOTO9000
C
 2440 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHSI')
      WRITE(ICOUT,2441)
      CALL EDWRST('EDCHSI')
 2441 FORMAT('***** ERROR IN EDCHSI--')
      WRITE(ICOUT,2442)
      CALL EDWRST('EDCHSI')
 2442 FORMAT('      NEW STRING WOULD EXCEED ICHA(.) WORKSPACE.')
      WRITE(ICOUT,2443)NUMCHA,MAXEDC,NUMCHX
      CALL EDWRST('EDCHSI')
 2443 FORMAT('      NUMCHA,MAXEDC,NUMCHX = ',3I8)
      IERROR='YES'
      GOTO9000
C
 2490 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE3.EQ.'OFF'.AND.ISUBRO.NE.'CHSI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHSI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCHSI')
 9011 FORMAT('***** AT THE END       OF EDCHSI--')
      WRITE(ICOUT,9012)NUMLIN,MAXEDL
      CALL EDWRST('EDCHSI')
 9012 FORMAT('NUMLIN,MAXEDL = ',2I8)
      WRITE(ICOUT,9013)NUMROW,MAXROW
      CALL EDWRST('EDCHSI')
 9013 FORMAT('NUMROW,MAXROW = ',2I8)
      WRITE(ICOUT,9014)NUMCHA,MAXEDC,NUMCHX
      CALL EDWRST('EDCHSI')
 9014 FORMAT('NUMCHA,MAXEDC,NUMCHX = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCHSI')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C     PURPOSE--CLOSE A FILE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--NOVEMBER 1985.
C
C-----------------------------------------------------------------------------
C
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCL'
      ISUBN2='FI  '
C
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'CLFI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCLFI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCLFI')
   51 FORMAT('***** AT THE BEGINNING OF EDCLFI--')
      WRITE(ICOUT,52)IOUNIT
      CALL EDWRST('EDCLFI')
   52 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,53)IENDFI
      CALL EDWRST('EDCLFI')
   53 FORMAT('IENDFI = ',A4)
      WRITE(ICOUT,54)IREWIN
      CALL EDWRST('EDCLFI')
   54 FORMAT('IREWIN = ',A4)
   90 CONTINUE
C
C               *********************
C               **  STEP 1--       **
C               **  CLOSE THE FILE **
C               *********************
C
      ISTEPN='1'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'CLFI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IENDFI.EQ.'ON'.AND.IHOST1.EQ.'UNIV')GOTO1100
      GOTO1190
 1100 CONTINUE
      ENDFILE IOUNIT
 1190 CONTINUE
C
      IF(IREWIN.EQ.'ON')REWIND IOUNIT
C
      CLOSE(IOUNIT)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'CLFI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCLFI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCLFI')
 9011 FORMAT('***** AT THE END       OF EDCLFI--')
      WRITE(ICOUT,9012)IOUNIT
      CALL EDWRST('EDCLFI')
 9012 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9013)IENDFI
      CALL EDWRST('EDCLFI')
 9013 FORMAT('IENDFI = ',A4)
      WRITE(ICOUT,9014)IENDFI
      CALL EDWRST('EDCLFI')
 9014 FORMAT('IENDFI = ',A4)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCLOS
C
C     PURPOSE--CLOSE A FILE
C     NOTE--IF NO ARGUMENTS GIVEN,
C           THE THE DEFAULT PRINTER FILE WILL BE CLOSED.
C         --IF 1 ARGUMENT GIVEN (PRESUMEDLY A FILE NAME),
C           THEN THAT FILE WILL BE CLOSED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
CCCCC CHARACTER*80 IREC
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUEX
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLI'
      ISUBN2='ST  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ADD')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCLOS')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCLOS')
   51 FORMAT('***** AT THE BEGINNING OF EDCLOS--')
      WRITE(ICOUT,53)IOPENU
      CALL EDWRST('EDCLOS')
   53 FORMAT('IOPENU = ',I8)
      WRITE(ICOUT,54)IOPENA
      CALL EDWRST('EDCLOS')
   54 FORMAT('IOPENA = ',A80)
      WRITE(ICOUT,55)IOPEST
      CALL EDWRST('EDCLOS')
   55 FORMAT('IOPEST = ',A12)
      WRITE(ICOUT,56)IOPEFO
      CALL EDWRST('EDCLOS')
   56 FORMAT('IOPEFO = ',A12)
      WRITE(ICOUT,57)IOPEAC
      CALL EDWRST('EDCLOS')
   57 FORMAT('IOPEAC = ',A12)
      WRITE(ICOUT,58)IOPERW
      CALL EDWRST('EDCLOS')
   58 FORMAT('IOPERW = ',A12)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCLOS')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE NAME OF THE FILE       **
C               **  THAT IS TO BE CLOSED                 **
C               **  (IF NO NAME IS GIVEN,                **
C               **  THEN USE A DEFAULT FILE NAME).       **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUEX)
      IF(IFOUEX.EQ.'YES')GOTO1110
      GOTO1120
C
 1110 CONTINUE
      IFILE=' '
      IF(NUMCST.LE.0)GOTO1119
      DO1111I=1,NUMCST
      IFILE(I:I)=ISTRIN(I)
 1111 CONTINUE
 1119 CONTINUE
      ISTAT='UNKNOWN'
      GOTO1190
C
 1120 CONTINUE
      IFILE=IOPENA
      ISTAT=IOPEST
      GOTO1190
C
 1190 CONTINUE
      IOUNIT=IOPENU
      IFORM=IOPEFO
      IACCES=IOPEAC
      IREWR=IOPERW
      ISUBN0='OPEN'
      IERRFI='NO'
C
C               **********************
C               **  STEP 12--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               ****************************
C               **  STEP 13--             **
C               **  WRITE OUT A MESSAGE   **
C               **  IDENTIFYING THE FILE  **
C               ****************************
C
      ISTEPN='13'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDCLOS')
      WRITE(ICOUT,1305)IFILE
      CALL EDWRST('EDCLOS')
 1305 FORMAT(10X,'FILE = ',A80)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCLOS')
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ADD')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCLOS')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCLOS')
 9011 FORMAT('***** AT THE END       OF EDCLOS--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDCLOS')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDCLOS')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDCLOS')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCLOS')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCOIH(IVAL,IHOUT,NOUT,IVALID)
C
C     PURPOSE--CONVERT AN INTEGER VARIABLE
C              TO A LEFT-JUSTIFIED CHARACTER*10 CHARACTER STRING
C              THIS SUBROUTINE IS VALID FOR (POSITIVE OR
C              NEGATIVE) INTEGERS UP TO 9 DIGITS.
C
C     ORIGINAL VERSION--JULY  1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*10 IHOUT
      CHARACTER*4 IVALID
C
      CHARACTER*4 ISIGN
      CHARACTER*4 IHDIG
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
C     MAXDIG IS THE MAXIMUM NUMBER OF DIGITS
C     FOR AN INTEGER VARIABLE.
C     THIS WILL VARY FROM ONE COMPUTER TO THE NEXT
C     DEPENDING ON THE NUMBER OF BITS FOR A WORD.
C     THE FOLLOWING DEFINED VALUE (= 10)
C     HAS BEEN SET FOR THE VAX 11/780.
C
CCCCC MAXDIG=11
      MAXDIG=9
      NUMDIG=(-999)
      IHOUT='          '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COIH')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOIH')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCOIH')
   51 FORMAT('***** AT THE BEGINNING OF EDCOIH--')
      WRITE(ICOUT,52)IBUGE2,ISUBRO,IERROR
      CALL EDWRST('EDCOIH')
   52 FORMAT('IBUGE2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      WRITE(ICOUT,53)IVAL
      CALL EDWRST('EDCOIH')
   53 FORMAT('IVAL = ',I11)
      WRITE(ICOUT,54)NUMDIG
      CALL EDWRST('EDCOIH')
   54 FORMAT('NUMDIG = ',I8)
      WRITE(ICOUT,55)(IHOUT(I:I),I=1,NOUT)
      CALL EDWRST('EDCOIH')
   55 FORMAT('IHOUT(.) = ',80A1)
   90 CONTINUE
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      IERROR='NO'
      IVALID='YES'
      IVAL2=IVAL
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
      IMAX=MAXDIG
      DO300I=IMIN,IMAX
      IREV=IMAX-I+IMIN
      IDIV=10**(IREV-1)
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
      IF(ISIGN.EQ.'-')IHOUT(J:J)='-'
C
C               **************************
C               **  STEP 5--            **
C               **  INSERT DIGITS INTO  **
C               **  OUTPUT VECTOR.      **
C               **************************
C
      IMIN=1
      IMAX=NUMDIG
      DO500I=IMIN,IMAX
      IREV=IMAX-I+IMIN
      IDIV=10**(IREV-1)
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
      IHOUT(J:J)=IHDIG
      IVAL2=IVAL2-IDIG*IDIV
  500 CONTINUE
      NOUT=J
C
C               ****************
C               **  STEP 6--  **
C               **  EXIT.     **
C               ****************
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COIH')GOTO9090
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCOIH')
 9011 FORMAT('***** AT THE END       OF EDCOIH--')
      WRITE(ICOUT,9012)IBUGE2,ISUBRO,IERROR
      CALL EDWRST('EDCOIH')
 9012 FORMAT('IBUGE2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      WRITE(ICOUT,9013)IVAL
      CALL EDWRST('EDCOIH')
 9013 FORMAT('IVAL = ',I11)
      WRITE(ICOUT,9014)NOUT
      CALL EDWRST('EDCOIH')
 9014 FORMAT('NOUT = ',I11)
      WRITE(ICOUT,9015)(IHOUT(I:I),I=1,NOUT)
      CALL EDWRST('EDCOIH')
 9015 FORMAT('IHOUT(.) = ',80A1)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCOP1
C
C     PURPOSE--COPY THE TEXT BLOCK OUT TO FILE
C              TREAT THE 2 CASES--
C                 COPY
C                 COPY JUNK.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
CCCCC UPDATED         --APRIL   1993.  FORMAT CHANGED FROM 80 TO 130
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUEX
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCO'
      ISUBN2='P1  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COP1')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP1')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCOP1')
   51 FORMAT('***** AT THE BEGINNING OF EDCOP1--')
      WRITE(ICOUT,52)IBLIM1,IBLIM2
      CALL EDWRST('EDCOP1')
   52 FORMAT('IBLIM1,IBLIM2 = ',2I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *****************************************
C               **  STEP 11--                          **
C               **  DETERMINE THE NAME OF THE FILE     **
C               **  THAT THE BLOCK IS TO BE COPIED TO  **
C               **  (IF NO NAME IS GIVEN,              **
C               **  THEN USE A DEFAULT FILE NAME).     **
C               *****************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP1')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUEX)
      IF(IFOUEX.EQ.'YES')GOTO1110
      GOTO1120
C
 1110 CONTINUE
      IFILE=' '
      IF(NUMCST.LE.0)GOTO1119
      DO1111I=1,NUMCST
      IFILE(I:I)=ISTRIN(I)
 1111 CONTINUE
 1119 CONTINUE
      ISTAT='UNKNOWN'
      GOTO1190
C
 1120 CONTINUE
      IFILE=ICOPNA
      ISTAT=ICOPST
      GOTO1190
C
 1190 CONTINUE
      IOUNIT=ICOPNU
      IFORM=ICOPFO
      IACCES=ICOPAC
      IREWR=ICOPRW
      ISUBN0='COP1'
      IERRFI='NO'
C
C               *********************
C               **  STEP 12--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP1')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
C
C               **********************************
C               **  STEP 21--                   **
C               **  SPECIFY THE END       LINE  **
C               **  OF THE BLOCK OF TEXT        **
C               **  TO BE OPERATED ON           **
C               **  (COPIED, MOVED, ETC.)       **
C               **********************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP1')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC IBLIM2=ICURLN
C
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP1')THEN
         WRITE(ICOUT,2111)IBLIM1,IBLIM2
         CALL EDWRST('EDCOP1')
      ENDIF
 2111 FORMAT('IBLIM1,IBLIM2 = ',2I8)
C
C               ****************************************************
C               **  STEP 22--                                     **
C               **  WRITE TEXT BLOCK OUT TO FILE                  **
C               ****************************************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP1')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IMIN=IBLIM1
      IMAX=IBLIM2
      IF(IMIN.LT.1)IMIN=1
      IF(IMAX.GT.NUMLIN)IMAX=NUMLIN
C
      IF(IMIN.GT.IMAX)GOTO2290
      DO2200ILINE=IMIN,IMAX
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      WRITE(IOUNIT,2205)(ICHA(J),J=J1,J2)
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
C2205 FORMAT(80A1)
 2205 FORMAT(238A1)
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP1')THEN
         WRITE(ICOUT,2206)J1,J2,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCOP1')
      ENDIF
 2206 FORMAT('J1,J2,(ICHA(J),J=J1,J2) = ',2I8,2X,80A1)
 2200 CONTINUE
C
 2290 CONTINUE
      ICURLN=IMAX
C
C               ***************************
C               **  STEP 23--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      ISTEPN='23'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP1')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEESW.EQ.'OFF')GOTO2329
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP1')
      IDEL=IMAX-IMIN+1
      ILINE=IMIN-1
      DO2320IPASS=1,10000
      ILINE=ILINE+1
      IF(IPASS.GE.2.AND.IDEL.GE.11)ILINE=IMAX
      IF(ILINE.LT.1)GOTO2325
      IF(ILINE.GT.IMAX)GOTO2325
      IF(ILINE.GT.NUMLIN)GOTO2325
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2321)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCOP1')
      ENDIF
 2321 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2322)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCOP1')
      ENDIF
 2322 FORMAT(I6,':',3X,230A1)
 2325 CONTINUE
      IF(IPASS.GE.2.AND.IDEL.GE.11)GOTO2328
 2320 CONTINUE
 2328 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP1')
 2329 CONTINUE
C
C               **********************
C               **  STEP 31--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP1')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC ENDFILE IOUNIT
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='ON'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COP1')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP1')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCOP1')
 9011 FORMAT('***** AT THE END       OF EDCOP1--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDCOP1')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDCOP1')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDCOP1')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)NUMLIN,NUMCHA
      CALL EDWRST('EDCOP1')
 9016 FORMAT('NUMLIN,NUMCHA = ',2I8)
      WRITE(ICOUT,9017)ICURLN,IFOUEX
      CALL EDWRST('EDCOP1')
 9017 FORMAT('ICURLN,IFOUEX = ',I8,2X,A4)
      WRITE(ICOUT,9018)IBLIM1,IBLIM2,IMIN,IMAX
      CALL EDWRST('EDCOP1')
 9018 FORMAT('IBLIM1,IBLIM2,IMIN,IMAX = ',4I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP1')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCOP2
C
C     PURPOSE--COPY AN INTERNALLY SPECIFIED TEXT BLOCK
C              OUT TO THE DEFAULT FILE (EDCOPY.TEX) OR
C              OUT TO A USER-SPECIFIED FILE.
C              TREAT THE 2 CASES--
C                 COPY 5 7
C                 COPY JUNK. 5 7
C     NOTE--THE CASE    COPY 5 7 JUNK.   IS ONLY PARTIALLY SUPPORTED.
C           STEP 21 (EXTRACTING THE FILE NAME) WOULD NEED TO BE UPDATED
C           IN ORDER TO MAKE THIS FORM WORK.)
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
CCCCC UPDATED         --APRIL   1993.  FORMAT CHANGED FROM 80 TO 130
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUEX
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCO'
      ISUBN2='P2  '
C
      IFOUND='YES'
      IERROR='NO'
C
      ISTART=(-999)
      ISTOP=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COP2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCOP2')
   51 FORMAT('***** AT THE BEGINNING OF EDCOP2--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 11--                     **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS OF THE BLOCK.    **
C               ************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP2')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1120
      GOTO1140
C
 1120 CONTINUE
      IF(IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')GOTO1130
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
      WRITE(ICOUT,1121)
      CALL EDWRST('EDCOP2')
 1121 FORMAT('***** ERROR IN COPY COMMAND--')
      WRITE(ICOUT,1122)
      CALL EDWRST('EDCOP2')
 1122 FORMAT('      FOR THIS FORM OF THE COPY COMMAND,')
      WRITE(ICOUT,1123)
      CALL EDWRST('EDCOP2')
 1123 FORMAT('      BOTH ARGUMENTS SHOULD BE NUMERIC,')
      WRITE(ICOUT,1124)
      CALL EDWRST('EDCOP2')
 1124 FORMAT('      BUT WERE NOT.')
      WRITE(ICOUT,1125)IHARG(1),IHARG(2)
      CALL EDWRST('EDCOP2')
 1125 FORMAT('      THE 2 ARGUMENTS ARE--',A4,2X,A4)
      WRITE(ICOUT,1126)
      CALL EDWRST('EDCOP2')
 1126 FORMAT('      EXAMPLE OF PROPER FORM--')
      WRITE(ICOUT,1127)
      CALL EDWRST('EDCOP2')
 1127 FORMAT('         COPY 5 7')
      IERROR='YES'
      GOTO9000
C
 1130 CONTINUE
      ISTART=IARG(1)
      ISTOP=IARG(2)
      GOTO1180
C
 1140 CONTINUE
      IF(IARGT(1).EQ.'WORD')GOTO1150
      IF(IARGT(3).EQ.'WORD')GOTO1160
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
      WRITE(ICOUT,1141)
      CALL EDWRST('EDCOP2')
 1141 FORMAT('***** ERROR IN COPY COMMAND--')
      WRITE(ICOUT,1142)
      CALL EDWRST('EDCOP2')
 1142 FORMAT('      FOR THIS FORM OF THE COPY COMMAND,')
      WRITE(ICOUT,1143)
      CALL EDWRST('EDCOP2')
 1143 FORMAT('      ARGUMENT 1 OR 3 MUST BE A FILE NAME,')
      WRITE(ICOUT,1144)
      CALL EDWRST('EDCOP2')
 1144 FORMAT('      BUT WERE NOT.')
      WRITE(ICOUT,1145)IHARG(1),IHARG(2),IHARG(3)
      CALL EDWRST('EDCOP2')
 1145 FORMAT('      THE 3 ARGUMENTS ARE--',A4,2X,A4,2X,A4)
      WRITE(ICOUT,1146)
      CALL EDWRST('EDCOP2')
 1146 FORMAT('      EXAMPLES OF PROPER FORM--')
      WRITE(ICOUT,1147)
      CALL EDWRST('EDCOP2')
 1147 FORMAT('         COPY JUNK. 5 7')
      WRITE(ICOUT,1148)
      CALL EDWRST('EDCOP2')
 1148 FORMAT('         COPY 5 7 JUNK.')
      IERROR='YES'
      GOTO9000
C
 1150 CONTINUE
      IF(IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')GOTO1159
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
      WRITE(ICOUT,1151)
      CALL EDWRST('EDCOP2')
 1151 FORMAT('***** ERROR IN COPY COMMAND--')
      WRITE(ICOUT,1152)
      CALL EDWRST('EDCOP2')
 1152 FORMAT('      FOR THIS FORM OF THE COPY COMMAND,')
      WRITE(ICOUT,1153)
      CALL EDWRST('EDCOP2')
 1153 FORMAT('      ARGUMENTS 2 AND 3 MUST BE NUMERIC,')
      WRITE(ICOUT,1154)
      CALL EDWRST('EDCOP2')
 1154 FORMAT('      BUT WERE NOT.')
      WRITE(ICOUT,1155)IHARG(2),IHARG(3)
      CALL EDWRST('EDCOP2')
 1155 FORMAT('      THE 2 ARGUMENTS ARE--',A4,2X,A4)
      WRITE(ICOUT,1156)
      CALL EDWRST('EDCOP2')
 1156 FORMAT('      EXAMPLE OF PROPER FORM--')
      WRITE(ICOUT,1157)
      CALL EDWRST('EDCOP2')
 1157 FORMAT('         COPY JUNK. 5 7')
      IERROR='YES'
      GOTO9000
 1159 CONTINUE
      ISTART=IARG(2)
      ISTOP=IARG(3)
      GOTO1180
C
 1160 CONTINUE
      IF(IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')GOTO1169
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
      WRITE(ICOUT,1161)
      CALL EDWRST('EDCOP2')
 1161 FORMAT('***** ERROR IN COPY COMMAND--')
      WRITE(ICOUT,1162)
      CALL EDWRST('EDCOP2')
 1162 FORMAT('      FOR THIS FORM OF THE COPY COMMAND,')
      WRITE(ICOUT,1163)
      CALL EDWRST('EDCOP2')
 1163 FORMAT('      ARGUMENTS 1 AND 2 MUST BE NUMERIC,')
      WRITE(ICOUT,1164)
      CALL EDWRST('EDCOP2')
 1164 FORMAT('      BUT WERE NOT.')
      WRITE(ICOUT,1165)IHARG(1),IHARG(2)
      CALL EDWRST('EDCOP2')
 1165 FORMAT('      THE 2 ARGUMENTS ARE--',A4,2X,A4)
      WRITE(ICOUT,1166)
      CALL EDWRST('EDCOP2')
 1166 FORMAT('      EXAMPLE OF PROPER FORM--')
      WRITE(ICOUT,1167)
      CALL EDWRST('EDCOP2')
 1167 FORMAT('         COPY 5 7 JUNK.')
      IERROR='YES'
      GOTO9000
 1169 CONTINUE
      ISTART=IARG(1)
      ISTOP=IARG(2)
      GOTO1180
C
 1180 CONTINUE
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD2.LT.IHOLD1)ISTART=IHOLD2
      IF(IHOLD2.LT.IHOLD1)ISTOP=IHOLD1
C
C               *****************************************
C               **  STEP 21--                          **
C               **  DETERMINE THE NAME OF THE FILE     **
C               **  THAT THE BLOCK IS TO BE COPIED TO  **
C               **  (IF NO NAME IS GIVEN,              **
C               **  THEN USE A DEFAULT FILE NAME).     **
C               *****************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP2')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')GOTO2120
      CALL EDEXST(ISTRIN,NUMCST,IFOUEX)
      IF(IFOUEX.EQ.'YES')GOTO2110
      GOTO2120
C
 2110 CONTINUE
      IFILE=' '
      IF(NUMCST.LE.0)GOTO2119
      DO2111I=1,NUMCST
      IF(ISTRIN(I).EQ.' ')GOTO2119
      IFILE(I:I)=ISTRIN(I)
 2111 CONTINUE
 2119 CONTINUE
      ISTAT='UNKNOWN'
      GOTO2190
C
 2120 CONTINUE
      IFILE=ICOPNA
      ISTAT=ICOPST
      GOTO2190
C
 2190 CONTINUE
      IOUNIT=ICOPNU
      IFORM=ICOPFO
      IACCES=ICOPAC
      IREWR=ICOPRW
      ISUBN0='COP2'
      IERRFI='NO'
C
C               *********************
C               **  STEP 22--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP2')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
C
C               ****************************************************
C               **  STEP 31--                                     **
C               **  WRITE TEXT BLOCK OUT TO FILE                  **
C               ****************************************************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP2')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IMIN=ISTART
      IMAX=ISTOP
      IF(IMIN.LT.1)IMIN=1
      IF(IMAX.GT.NUMLIN)IMAX=NUMLIN
C
      IF(IMIN.GT.IMAX)GOTO3190
      DO3100ILINE=IMIN,IMAX
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      WRITE(IOUNIT,3105)(ICHA(J),J=J1,J2)
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
C3105 FORMAT(80A1)
 3105 FORMAT(238A1)
 3100 CONTINUE
C
 3190 CONTINUE
      ENDFILE IOUNIT
      REWIND IOUNIT
C
C               ***************************
C               **  STEP 32--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      ISTEPN='32'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP2')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEESW.EQ.'OFF')GOTO3229
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
      IDEL=IMAX-IMIN+1
      ILINE=IMIN-1
      DO3220IPASS=1,10000
      ILINE=ILINE+1
      IF(IPASS.GE.2.AND.IDEL.GE.11)ILINE=IMAX
      IF(ILINE.LT.1)GOTO3225
      IF(ILINE.GT.IMAX)GOTO3225
      IF(ILINE.GT.NUMLIN)GOTO3225
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,3221)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCOP2')
      ENDIF
 3221 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,3222)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCOP2')
      ENDIF
 3222 FORMAT(I6,':',3X,230A1)
 3225 CONTINUE
      IF(IPASS.GE.2.AND.IDEL.GE.11)GOTO3228
 3220 CONTINUE
 3228 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
 3229 CONTINUE
C
C               **********************
C               **  STEP 41--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='41'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP2')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC ENDFILE IOUNIT
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='ON'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COP2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCOP2')
 9011 FORMAT('***** AT THE END       OF EDCOP2--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDCOP2')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDCOP2')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDCOP2')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)NUMLIN,NUMCHA
      CALL EDWRST('EDCOP2')
 9016 FORMAT('NUMLIN,NUMCHA = ',2I8)
      WRITE(ICOUT,9017)ICURLN,IFOUND
      CALL EDWRST('EDCOP2')
 9017 FORMAT('ICURLN,IFOUND = ',I8,2X,A4)
      WRITE(ICOUT,9018)ISTART,ISTOP,IMIN,IMAX,IDEL
      CALL EDWRST('EDCOP2')
 9018 FORMAT('ISTART,ISTOP,IMIN,IMAX,IDEL = ',5I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP2')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCOP3
C
C     PURPOSE--COPY THE TEXT BLOCK IN LINES ARG1 TO ARG2
C              OUT TO EDCOPY.TEX   AND THEN
C              INSERT THE BLOCK AFTER LINE ARG3.
C              TREAT THE 1 CASE--
C                 COPY 5 7 10
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
CCCCC UPDATED         --APRIL   1993.  FORMAT CHANGED FROM 80 TO 130
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
CCCCC CHARACTER*80 IREC
      CHARACTER*240 IREC
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCO'
      ISUBN2='P3  '
C
      IFOUND='YES'
      IERROR='NO'
C
      ISTART=(-999)
      ISTOP=(-999)
      ITARG=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COP3')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP3')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCOP3')
   51 FORMAT('*****AT THE BEGINNING OF EDCOP3--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 11--                     **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS OF THE BLOCK.    **
C               **  DETERMINE THE LINE NUMBER     **
C               **  AFTER WHICH THE BLOCK IS      **
C               **  TO BE MOVED.                  **
C               ************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP3')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB'.AND.
     1   IARGT(3).EQ.'NUMB')GOTO1120
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP3')
      WRITE(ICOUT,1111)
      CALL EDWRST('EDCOP3')
 1111 FORMAT('***** ERROR IN COPY COMMAND--')
      WRITE(ICOUT,1112)
      CALL EDWRST('EDCOP3')
 1112 FORMAT('      ONE OF THE 3 ARGUMENTS IS NON-NUMERIC')
      IERROR='YES'
      GOTO9000
C
 1120 CONTINUE
      ISTART=IARG(1)
      ISTOP=IARG(2)
      ITARG=IARG(3)
C
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD2.LT.IHOLD1)ISTART=IHOLD2
      IF(IHOLD2.LT.IHOLD1)ISTOP=IHOLD1
C
C               *****************************************
C               **  STEP 12--                          **
C               **  DEFINE    THE NAME OF THE FILE     **
C               **  THAT THE BLOCK IS TO BE COPIED TO  **
C               **  (USE THE DEFAULT FILE NAME).       **
C               *****************************************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP3')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IFILE=ICOPNA
      ISTAT=ICOPST
      IOUNIT=ICOPNU
      IFORM=ICOPFO
      IACCES=ICOPAC
      IREWR=ICOPRW
      ISUBN0='COP3'
      IERRFI='NO'
C
C               *********************
C               *********************
C               **  STEP 13--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='13'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP3')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
C
C               ****************************************************
C               **  STEP 21--                                     **
C               **  WRITE TEXT BLOCK OUT TO FILE                  **
C               ****************************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP3')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IMIN=ISTART
      IMAX=ISTOP
      IF(IMIN.LT.1)IMIN=1
      IF(IMAX.GT.NUMLIN)IMAX=NUMLIN
C
      IF(IMIN.GT.IMAX)GOTO2190
      DO2100ILINE=IMIN,IMAX
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      WRITE(IOUNIT,2105)(ICHA(J),J=J1,J2)
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
C2105 FORMAT(80A1)
 2105 FORMAT(238A1)
 2100 CONTINUE
C
 2190 CONTINUE
      ENDFILE IOUNIT
      REWIND IOUNIT
C
C               *****************************************
C               **  STEP 31--                          **
C               **  SET UP A LOOP                      **
C               **  THAT WILL READ ONE LINE AT A TIME  **
C               **  FROM THE FILE                      **
C               **  AND INSERT IT AT THE PROPER POINT  **
C               **  IN THE WORKSPACE                   **
C               *****************************************
C
      ISTEPN='31'
      IF(IBUGE3.EQ.'ON'.OR.ISUBRO.EQ.'COP3')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLN=ITARG
C
      DO3100IDUMMY=1,100000
C
C               ************************************
C               **  STEP 32--                     **
C               **  READ IN A LINE FROM THE FILE  **
C               ************************************
C
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
CCCCC NUMCRE=80
      NUMCRE=238
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1993
      IREC=' '
      READ(IOUNIT,3205,END=3190)IREC
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
C3205 FORMAT(A80)
 3205 FORMAT(A238)
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,3206)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDCOP3')
 3206 FORMAT('IREC = ',A80)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,3207)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDCOP3')
 3207 FORMAT('NUMCRE = ',I8)
C
      DO3200I=1,NUMCRE
      IREV=NUMCRE-I+1
      IF(IREC(IREV:IREV).NE.' ')GOTO3290
 3200 CONTINUE
      IREV=0
 3290 CONTINUE
      NUMCRE=IREV
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,3295)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDCOP3')
 3295 FORMAT('IREC = ',A80)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,3296)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDCOP3')
 3296 FORMAT('NUMCRE = ',I8)
C
C               ***************************************************
C               **  STEP 33--                                    **
C               **  DETERMINE IF THERE IS ROOM IN THE WORKSPACE  **
C               **  FOR THE NEW LINE                             **
C               ***************************************************
C
      NUMCH2=NUMCHA+NUMCRE
      CALL EDCHSI(NUMCH2)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               **********************************************
C               **  STEP 34--                               **
C               **  UPDATE THE WORKSPACE TABLES             **
C               **  AND INSERT THE LINE INTO THE WORKSPACE  **
C               **********************************************
C
      IF(ICURLN.GT.NUMLIN)ICURLN=NUMLIN
      ICURLP=ICURLN+1
      NUMLP1=NUMLIN+1
      NUMRP1=NUMROW+1
C
      IHOLD1=IPOINT(ICURLP)
      IPOINT(ICURLP)=NUMRP1
C
      IF(ICURLP.LE.0)GOTO3419
      IF(NUMLIN.LE.0)GOTO3419
      IF(ICURLP.GT.NUMLIN)GOTO3419
      DO3410ILINE=ICURLP,NUMLIN
      ILINEP=ILINE+1
      IHOLD2=IPOINT(ILINEP)
      IPOINT(ILINEP)=IHOLD1
      IHOLD1=IHOLD2
 3410 CONTINUE
 3419 CONTINUE
C
      J1=NUMCHA+1
      N1=NUMCRE
      J2=J1+(N1-1)
C
      IROW=NUMRP1
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO3420J=J1,J2
      I=I+1
      ICHA(J)=IREC(I:I)
 3420 CONTINUE
 3429 CONTINUE
      ICURLN=ICURLP
      NUMLIN=NUMLP1
      NUMROW=NUMRP1
      NUMCHA=J2
C
 3100 CONTINUE
 3190 CONTINUE
C
C               ***************************
C               **  STEP 35--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      ISTEPN='35'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP3')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEESW.EQ.'OFF')GOTO2529
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP3')
      IMIN=ITARG+1
      IMAX=ICURLN
      IDEL=IMAX-IMIN+1
      ILINE=IMIN-1
      DO2520IPASS=1,10000
      ILINE=ILINE+1
      IF(IPASS.GE.2.AND.IDEL.GE.11)ILINE=IMAX
      IF(ILINE.LT.1)GOTO2525
      IF(ILINE.GT.IMAX)GOTO2525
      IF(ILINE.GT.NUMLIN)GOTO2525
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCOP3')
      ENDIF
 2521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2522)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDCOP3')
      ENDIF
 2522 FORMAT(I6,':',3X,230A1)
 2525 CONTINUE
      IF(IPASS.GE.2.AND.IDEL.GE.11)GOTO2528
 2520 CONTINUE
 2528 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP3')
 2529 CONTINUE
C
C               **********************
C               **  STEP 41--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='41'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COP3')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC ENDFILE IOUNIT
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='ON'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COP3')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP3')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCOP3')
 9011 FORMAT('*****AT THE END       OF EDCOP3--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDCOP3')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDCOP3')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDCOP3')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)NUMLIN,NUMCHA
      CALL EDWRST('EDCOP3')
 9016 FORMAT('NUMLIN,NUMCHA = ',2I8)
      WRITE(ICOUT,9017)ICURLN,IFOUND
      CALL EDWRST('EDCOP3')
 9017 FORMAT('ICURLN,IFOUND = ',I8,2X,A4)
      WRITE(ICOUT,9018)IBLIM1,IBLIM2,IMIN,IMAX
      CALL EDWRST('EDCOP3')
 9018 FORMAT('IBLIM1,IBLIM2,IMIN,IMAX = ',4I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOP3')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCOST(ICTEMP,NCTEMP)
C
C     PURPOSE--CONVERT THE STRING IN THE CHARACTER*240
C              STRING VARIABLE INTO THE DIMENSIONED (240)
C              CHARACTER*1 VARIABLE IANS(.).
C              ADJUST IWIDTH ALSO.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.7
C     ORIGINAL VERSION--JUNE 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ICTEMP
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCO'
      ISUBN2='ST  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COST')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOST')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCOST')
   51 FORMAT('***** AT THE BEGINNING OF EDCOST--')
      WRITE(ICOUT,52)NCTEMP
      CALL EDWRST('EDCOST')
   52 FORMAT('NCTEMP = ',I8)
      IF(NCTEMP.GE.1)THEN
         WRITE(ICOUT,53)(ICTEMP(I:I),I=1,NCTEMP)
         CALL EDWRST('EDCOST')
      ENDIF
   53 FORMAT('(ICTEMP(I:I),I=1,NCTEMP) = ',110A1)
      WRITE(ICOUT,62)IWIDTH
      CALL EDWRST('EDCOST')
   62 FORMAT('IWIDTH = ',I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,63)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDCOST')
      ENDIF
   63 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOST')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **************************
C               **  STEP 1--            **
C               **  CONVERT THE STRING  **
C               **************************
C
      DO1100I=1,240
      IANS(I)=' '
 1100 CONTINUE
C
      IMAX=NCTEMP
      IF(IMAX.LT.0)IMAX=0
      IF(IMAX.GT.240)IMAX=240
      IWIDTH=IMAX
      IF(IMAX.LE.0)GOTO1290
      DO1200I=1,IMAX
      IANS(I)=ICTEMP(I:I)
 1200 CONTINUE
 1290 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'COST')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOST')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCOST')
 9011 FORMAT('***** AT THE END       OF EDCOST--')
      WRITE(ICOUT,9012)NCTEMP
      CALL EDWRST('EDCOST')
 9012 FORMAT('NCTEMP = ',I8)
      IF(NCTEMP.GE.1)THEN
         WRITE(ICOUT,9013)(ICTEMP(I:I),I=1,NCTEMP)
         CALL EDWRST('EDCOST')
      ENDIF
 9013 FORMAT('(ICTEMP(I:I),I=1,NCTEMP) = ',110A1)
      WRITE(ICOUT,9022)IWIDTH
      CALL EDWRST('EDCOST')
 9022 FORMAT('IWIDTH = ',I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,9023)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDCOST')
      ENDIF
 9023 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9032)IMAX
      CALL EDWRST('EDCOST')
 9032 FORMAT('IMAX = ',I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDCOST')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDCUT
C
C     PURPOSE--CUT CURRENT FROM     STRING    TO END OF LINE.
C              THE STRING WHICH FOLLOWS THE SPLIT COMMAND
C              WILL BE INCLUDED IN THE CUT.
C     NOTE--IN THE EVENT THAT THERE IS MORE THAN
C           ONE OCCURRANCE OF THE STRING ON THE LINE,
C           THE CUTTING WILL BE DONE FROM THE
C           LAST SUCH OCCURRANCE.
C     COMMAND SYNTAX--CUT <STRING>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.6
C     ORIGINAL VERSION--JUNE 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ICLINE
      CHARACTER*240 ICNEWL
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUST
      CHARACTER*4 IFEES2
C
      CHARACTER*4 IEOF
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCU'
      ISUBN2='T   '
C
      IFOUND='YES'
      IERROR='NO'
C
      IEOF='NO'
      IARG1=(-999)
      IARG2=(-999)
      J=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CUT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDCUT ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDCUT ')
   51 FORMAT('*****AT THE BEGINNING OF EDCUT--')
      WRITE(ICOUT,52)IPRISW,ICURLN
      CALL EDWRST('EDCUT ')
   52 FORMAT('IPRISW,ICURLN = ',A4,I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **********************************************
C               **  STEP 11--                               **
C               **  SET THE LINE NUMBER                     **
C               **  TO BE OPERATED ON.                      **
C               **********************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=ISTART
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
C               ***************************
C               **  STEP 21--            **
C               **  EXTRACT  THE LINE.   **
C               ***************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ISTART
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(N1.LE.0)GOTO9000
      J=J1-1
      DO2100I=1,N1
      J=J+1
      ICLINE(I:I)=ICHA(J)
 2100 CONTINUE
      NCLINE=N1
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CUT')GOTO2199
      WRITE(ICOUT,2191)NUMCST
      CALL EDWRST('EDCUT ')
 2191 FORMAT('NUMCST = ',I8)
      IF(NUMCST.GE.1)THEN
         WRITE(ICOUT,2192)(ISTRIN(I),I=1,100)
         CALL EDWRST('EDCUT ')
      ENDIF
 2192 FORMAT('(ISTRIN(I),I=1,100) = ',100A1)
 2199 CONTINUE
C
C               ************************************
C               **  STEP 22--                     **
C               **  EXTRACT  THE TARGET STRING    **
C               **  FROM THE COMMAND LINE.        **
C               ************************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUST)
      IF(IFOUST.EQ.'YES')GOTO2290
      NUMCST=0
C
 2290 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CUT')GOTO2299
      WRITE(ICOUT,2291)NUMCST
      CALL EDWRST('EDCUT ')
 2291 FORMAT('NUMCST = ',I8)
      IF(NUMCST.GE.1)THEN
         WRITE(ICOUT,2292)(ISTRIN(I),I=1,100)
         CALL EDWRST('EDCUT ')
      ENDIF
 2292 FORMAT('(ISTRIN(I),I=1,100) = ',100A1)
 2299 CONTINUE
 
C               *********************************************
C               **  STEP 23--                              **
C               **  DETERMINE THE LAST OCCURRANCE OF       **
C               **  THE TARGET STRING ON THE CURRENT LINE. **
C               *********************************************
C
      ISTEPN='23'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IC1=(-999)
      DO2310I=1,NCLINE
      I2=I
      DO2320J=1,NUMCST
      IJ=I+J-1
      IF(ICLINE(IJ:IJ).EQ.ISTRIN(J))GOTO2320
      GOTO2310
 2320 CONTINUE
      IC1=I2-1
 2310 CONTINUE
      IF(IC1.LE.(-1))GOTO9000
C
 2390 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CUT')GOTO2399
      WRITE(ICOUT,2391)IC1
      CALL EDWRST('EDCUT ')
 2391 FORMAT('IC1 = ',I8)
 2399 CONTINUE
C
C               *******************************************
C               **  STEP 24--                            **
C               **  FORM THE NEW LINE                    **
C               *******************************************
C
      ISTEPN='24'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDZERO(ICNEWL,NCNEWL)
      IF(IC1.LE.0)NCNEWL=1
      IF(IC1.LE.0)GOTO2490
C
      DO2410I=1,IC1
      IP=I
      ICNEWL(IP:IP)=ICLINE(I:I)
 2410 CONTINUE
      NCNEWL=IP
C
 2490 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CUT')GOTO2499
      WRITE(ICOUT,2491)NCNEWL
      CALL EDWRST('EDCUT ')
 2491 FORMAT('NCNEWL = ',I8)
      IF(NCNEWL.GE.1)THEN
         WRITE(ICOUT,2492)(ICNEWL(I:I),I=1,100)
         CALL EDWRST('EDCUT ')
      ENDIF
 2492 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
 2499 CONTINUE
      IF(NCNEWL.LE.0)GOTO9000
C
C               *********************************************
C               **  STEP 31--                              **
C               **  INSERT THE CORRECT (CUT) LINE          **
C               **  IMMEDIATELY AFTER THE ORIGINAL LINE.   **
C               *********************************************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURL2=ISTOP
      CALL EDINLI(ICNEWL,NCNEWL,ICURL2)
      ISTAR2=ICURL2
      ISTOP2=ICURL2
C
C               ***********************************************
C               **  STEP 32--                                **
C               **  DELETE THE OLD (ORIGINAL) LINE.          **
C               ***********************************************
C
      ISTEPN='32'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IFEES2='OFF'
      CALL EDDELI(ISTART,IFEES2)
CCCCC IDELSW='OFF'
CCCCC IDELLN='-999'
C
C               ***********************************************
C               **  STEP 33--                                **
C               **  PRINT THE NEW (CUT) LINE.                **
C               ***********************************************
C
      ISTEPN='33'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IDEL=ISTOP-ISTART+1
      ISTAR3=ISTAR2-IDEL
      ISTOP3=ISTOP2-IDEL
      DO3300ILINE=ISTAR3,ISTOP3
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(N1.LE.0)GOTO3300
      J=J1-1
      DO3310I=1,N1
      J=J+1
      ICNEWL(I:I)=ICHA(J)
 3310 CONTINUE
      NCNEWL=N1
      IF(IFEESW.EQ.'ON')CALL EDWRLI(ICNEWL,NCNEWL,ILINE)
 3300 CONTINUE
C
C               ***********************************************
C               **  STEP 34--                                **
C               **  SET THE CURRENT LINE POSITION.           **
C               ***********************************************
C
      ISTEPN='33'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CUT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLN=ISTOP3
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CUT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDCUT ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDCUT ')
 9011 FORMAT('*****AT THE END       OF EDCUT--')
      WRITE(ICOUT,9012)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDCUT ')
 9012 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9013)IPRISW
      CALL EDWRST('EDCUT ')
 9013 FORMAT('IPRISW = ',A4)
      WRITE(ICOUT,9014)ISTART,ISTOP,ISTAR2,ISTOP2
      CALL EDWRST('EDCUT ')
 9014 FORMAT('ISTART,ISTOP,ISTAR2,ISTOP2 = ',4I8)
      WRITE(ICOUT,9015)IDEL,ISTAR3,ISTOP3,ICURLN
      CALL EDWRST('EDCUT ')
 9015 FORMAT('IDEL,ISTAR3,ISTOP3,ICURLN = ',4I8)
      WRITE(ICOUT,9031)NCLINE
      CALL EDWRST('EDCUT ')
 9031 FORMAT('NCLINE = ',I8)
      IF(NCLINE.GE.1)THEN
         WRITE(ICOUT,9032)(ICLINE(I:I),I=1,100)
         CALL EDWRST('EDCUT ')
      ENDIF
 9032 FORMAT('(ICLINE(I:I),I=1,100) = ',100A1)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDDEBL(ISTRIN,NUMCST)
C
C     PURPOSE--"DEBLANK" THE TRAILING BLANKS FROM A CHARACTER STRING.
C     DANGER--THE INPUT ARGUMENTS ISTRIN AND NUMCST
C             ARE BOTH CHANGED BY THIS SUBROUTINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      DIMENSION ISTRIN(*)
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGMA.EQ.'OFF'.AND.ISUBRO.NE.'DEBL')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEBL')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDDEBL')
   51 FORMAT('***** AT THE BEGINNING OF EDDEBL--')
      WRITE(ICOUT,52)IBUGMA,IERROR
      CALL EDWRST('EDDEBL')
   52 FORMAT('IBUGMA,IERROR = ',A4,2X,A4)
      WRITE(ICOUT,53)NUMCST
      CALL EDWRST('EDDEBL')
   53 FORMAT('NUMCST = ',I8)
      IF(NUMCST.GE.1)WRITE(ICOUT,54)(ISTRIN(I),I=1,NUMCST)
      IF(NUMCST.GE.1)CALL EDWRST('EDDEBL')
   54 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',80A1)
   90 CONTINUE
C
      DO1100I=1,NUMCST
      IREV=NUMCST-I+1
      IF(ISTRIN(IREV).NE.' ')GOTO1190
 1100 CONTINUE
      IREV=0
 1190 CONTINUE
      NUMCST=IREV
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
C
 9000 CONTINUE
      IF(IBUGMA.EQ.'OFF'.AND.ISUBRO.NE.'DEBL')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEBL')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDDEBL')
 9011 FORMAT('***** AT THE END       OF EDDEBL--')
      WRITE(ICOUT,9012)IBUGMA,IERROR
      CALL EDWRST('EDDEBL')
 9012 FORMAT('IBUGMA,IERROR = ',A4,2X,A4)
      WRITE(ICOUT,9013)NUMCST
      CALL EDWRST('EDDEBL')
 9013 FORMAT('NUMCST = ',I8)
      IF(NUMCST.GE.1)WRITE(ICOUT,9014)(ISTRIN(I),I=1,NUMCST)
      IF(NUMCST.GE.1)CALL EDWRST('EDDEBL')
 9014 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',80A1)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDDEFI(IHARLC,IHARL2)
C
C     PURPOSE--CREATE USER-DEFINED COMMANDS.
C     USED INPUT  ARGUMENTS--IHARLC  (A CHARACTER VECTOR)
C                          --IHARL2  (A CHARACTER VECTOR)
C                          --NUMARG
C     USED OUTPUT ARGUMENTS--ICOM3
C                            ICOM4
C                            ICOM5
C                            NUMCOM
C                            NCOM5
C                            IFOUND
C                            IERROR
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/6
C     ORIGINAL VERSION--JUNE  1986.
C
C-----NON-COMMON VARIABLES----------------------------------------
C
      CHARACTER*4 IHARLC
      CHARACTER*4 IHARL2
C
      CHARACTER*30 ICOM5J
      CHARACTER*4 IC4
      CHARACTER*1 IC1
      CHARACTER*4 IC4LC
      CHARACTER*1 IC1LC
      CHARACTER*30 ICTE30
C
      DIMENSION IHARLC(*)
      DIMENSION IHARL2(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDDE'
      ISUBN2='FI  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DEFI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEFI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDDEFI')
   51 FORMAT('AT THE BEGINNING OF EDDEFI--')
      WRITE(ICOUT,53)IBUGE2,ISUBRO
      CALL EDWRST('EDDEFI')
   53 FORMAT('IBUGE2,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,55)NUMARG
      CALL EDWRST('EDDEFI')
   55 FORMAT('NUMARG = ',I8)
      IF(NUMARG.LE.0)GOTO59
      DO56I=1,NUMARG
      WRITE(ICOUT,57)I,IHARLC(I)
      CALL EDWRST('EDDEFI')
   57 FORMAT('I,IHARLC(I) = ',I8,2X,A4)
   56 CONTINUE
   59 CONTINUE
      WRITE(ICOUT,61)NUMCOM
      CALL EDWRST('EDDEFI')
   61 FORMAT('NUMCOM = ',I8)
      IF(NUMCOM.LE.0)GOTO69
      DO62I=1,NUMCOM
      WRITE(ICOUT,63)I,ICOM3(I),ICOM4(I),NCOM5(I),ICOM5(I)
      CALL EDWRST('EDDEFI')
   63 FORMAT('I,ICOM3(I),ICOM4(I),NCOM5(I),ICOM5(I) = ',
     1I8,2X,A4,2X,A4,I8,A60)
   62 CONTINUE
   69 CONTINUE
      WRITE(ICOUT,81)IFOUND,IERROR
      CALL EDWRST('EDDEFI')
   81 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
   90 CONTINUE
C
C               ***************************************************
C               **  STEP 11--                                    **
C               **  DETERMINE THE ELEMENT NUMBER FOR THE COMMAND.**
C               **  IS IT AN EXISTING USER-DEFINED COMMAND?      **
C               **  IS IT A NEW USER-DEFINED COMMAND?            **
C               ***************************************************
C
      IF(NUMARG.LE.0)GOTO1180
C
      I2=1
      IF(NUMCOM.LE.0)GOTO1190
      DO1100I=1,NUMCOM
      I2=I
      IF(IHARG(1).EQ.ICOM3(I).AND.IHARG2(1).EQ.ICOM4(I))GOTO1190
 1100 CONTINUE
      I2=NUMCOM+1
      GOTO1190
C
 1180 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEFI')
      WRITE(ICOUT,1181)
      CALL EDWRST('EDDEFI')
 1181 FORMAT('***** ERROR IN SUBROUTINE EDDEFI--')
      WRITE(ICOUT,1182)
      CALL EDWRST('EDDEFI')
 1182 FORMAT('      WHEN USING THE DEFINE COMMAND,')
      WRITE(ICOUT,1183)
      CALL EDWRST('EDDEFI')
 1183 FORMAT('      YOU MUST HAVE SOME ENTRY')
      WRITE(ICOUT,1184)
      CALL EDWRST('EDDEFI')
 1184 FORMAT('      AFTER THE WORD    DEFINE   ;')
      WRITE(ICOUT,1185)
      CALL EDWRST('EDDEFI')
 1185 FORMAT('      BUT NONE WAS GIVEN HERE.')
      WRITE(ICOUT,1186)NUMARG
      CALL EDWRST('EDDEFI')
 1186 FORMAT('      NUMARG = ',I8)
      IERROR='YES'
      GOTO9000
C
 1190 CONTINUE
C
C               *************************************************
C               **  STEP 12--                                  **
C               **  EXTRACT THE NAME OF THE COMMAND.           **
C               *************************************************
C
      ICOM3(I2)=IHARG(1)
      ICOM4(I2)=IHARG2(1)
C
C               ***************************************************
C               **  STEP 12--                                    **
C               **  EXTRACT THE ASCII SEQUENCE.                  **
C               ***************************************************
C
      ICTE30(1:30)='                              '
      ICOM5(I2)=ICTE30(1:30)
      NCOM5(I2)=0
C
      J=0
      IF(NUMARG.LE.1)GOTO1290
      DO1200I=2,NUMARG
      J=J+1
      IC4=IHARG(I)
      IC1=IC4(1:1)
C
      IC4LC=IHARLC(I)
      IC1LC=IC4LC(1:1)
C
      IF(IC4(1:3).EQ.'NUL')IC1LC=INULC
      IF(IC4(1:3).EQ.'SOH')IC1LC=ISOHC
      IF(IC4(1:3).EQ.'STX')IC1LC=ISTXC
      IF(IC4(1:3).EQ.'ETX')IC1LC=IETXC
      IF(IC4(1:3).EQ.'EOT')IC1LC=IEOTC
      IF(IC4(1:3).EQ.'ENQ')IC1LC=IENQC
      IF(IC4(1:3).EQ.'ACK')IC1LC=IACKC
      IF(IC4(1:3).EQ.'BEL')IC1LC=IBELC
      IF(IC4(1:2).EQ.'BS')IC1LC=IBSC
      IF(IC4(1:3).EQ.'HTX')IC1LC=IHTC
      IF(IC4(1:2).EQ.'LF')IC1LC=ILFC
      IF(IC4(1:2).EQ.'VT')IC1LC=IVTC
      IF(IC4(1:2).EQ.'FF')IC1LC=IFFC
      IF(IC4(1:2).EQ.'CR')IC1LC=ICRC
      IF(IC4(1:2).EQ.'SO')IC1LC=ISOC
      IF(IC4(1:2).EQ.'SI')IC1LC=ISIC
      IF(IC4(1:3).EQ.'DLE')IC1LC=IDLEC
      IF(IC4(1:3).EQ.'DC1')IC1LC=IDC1C
      IF(IC4(1:3).EQ.'DC2')IC1LC=IDC2C
      IF(IC4(1:3).EQ.'DC3')IC1LC=IDC3C
      IF(IC4(1:3).EQ.'DC4')IC1LC=IDC4C
      IF(IC4(1:3).EQ.'NAK')IC1LC=INAKC
      IF(IC4(1:3).EQ.'SYN')IC1LC=ISYNC
      IF(IC4(1:3).EQ.'ETB')IC1LC=IETBC
      IF(IC4(1:3).EQ.'CAN')IC1LC=ICANC
      IF(IC4(1:2).EQ.'EM')IC1LC=IEMC
      IF(IC4(1:3).EQ.'SUB')IC1LC=ISUBC
      IF(IC4(1:3).EQ.'ESC')IC1LC=IESCC
      IF(IC4(1:2).EQ.'FS')IC1LC=IFSC
      IF(IC4(1:2).EQ.'GS')IC1LC=IGSC
      IF(IC4(1:2).EQ.'RS')IC1LC=IRSC
      IF(IC4(1:2).EQ.'US')IC1LC=IUSC
      IF(IC4(1:3).EQ.'BLA')IC1LC=' '
      IF(IC4(1:2).EQ.'SP')IC1LC=' '
      IF(IC4(1:3).EQ.'BLA')IC1LC=' '
      IF(IC4(1:2).EQ.'BL')IC1LC=' '
C
      ICTE30(J:J)=IC1LC
 1200 CONTINUE
C
 1290 CONTINUE
      ICOM5(I2)=ICTE30(1:30)
      NCOM5(I2)=J
      IF(I2.GT.NUMCOM)NUMCOM=I2
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DEFI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEFI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDDEFI')
 9011 FORMAT('AT THE END       OF EDDEFI--')
      WRITE(ICOUT,9013)IBUGE2,ISUBRO
      CALL EDWRST('EDDEFI')
 9013 FORMAT('IBUGE2,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,9015)NUMARG
      CALL EDWRST('EDDEFI')
 9015 FORMAT('NUMARG = ',I8)
      IF(NUMARG.LE.0)GOTO9019
      DO9016I=1,NUMARG
      WRITE(ICOUT,9017)I,IHARG(I)
      CALL EDWRST('EDDEFI')
 9017 FORMAT('I,IHARG(I) = ',I8,2X,A4)
 9016 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)I2,NUMCOM
      CALL EDWRST('EDDEFI')
 9021 FORMAT('I2,NUMCOM = ',2I8)
      WRITE(ICOUT,9022)I2,ICOM3(I2),ICOM4(I2),NCOM5(I2)
      CALL EDWRST('EDDEFI')
 9022 FORMAT('I2,ICOM3(I2),ICOM4(I2),NCOM5(I2)  = ',
     1I8,2X,A4,2X,A4,I8)
      IMAX=NCOM5(I)
      IF(IMAX.LE.0)GOTO9033
      ICOM5J=ICOM5(I)
      DO9031I=1,IMAX
      WRITE(ICOUT,9032)I,ICOM5J(I:I)
      CALL EDWRST('EDDEFI')
 9032 FORMAT('I,ICOM5J(I:I) = ',I8,2X,A1)
 9031 CONTINUE
 9033 CONTINUE
      IF(NUMCOM.LE.0)GOTO9043
      DO9041I=1,NUMCOM
      WRITE(ICOUT,9042)I,ICOM3(I),ICOM4(I),NCOM5(I),ICOM5(I)
      CALL EDWRST('EDDEFI')
 9042 FORMAT('I,ICOM3(I),ICOM4(I),NCOM5(I),ICOM5(I) = ',
     1I8,2X,A4,2X,A4,I8,A30)
 9041 CONTINUE
 9043 CONTINUE
      WRITE(ICOUT,9051)IFOUND,IERROR
      CALL EDWRST('EDDEFI')
 9051 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDDELE
C
C     PURPOSE--DELETE LINES OF TEXT.
C              IF NO ARGUMENTS, THEN DELETE CURRENT LINE.
C              IF 1  ARGUMENT , THEN DELETE CURRENT LINE
C                               + NEXT (N-1) LINES;
C                               THAT IS, DELETE RELATIVE.
C              IF 2  ARGUMENTS, THEN DELETE LINES N1 TO N2,
C                               THAT IS, DELETE ABSOLUTE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ITOP
      CHARACTER*4 IEOF
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDDE'
      ISUBN2='LE  '
C
      IFOUND='YES'
      IERROR='NO'
C
      ITOP='NO'
      IEOF='NO'
      IARG1=(-999)
      IARG2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DELE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDDELE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDDELE')
   51 FORMAT('*****AT THE BEGINNING OF EDDELE--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
      WRITE(ICOUT,52)ICURLN,IDELSW,IDELLN
      CALL EDWRST('EDDELE')
   52 FORMAT('ICURLN,IDELSW,IDELLN = ',I8,2X,A4,I8)
   90 CONTINUE
C
C               **********************************
C               **  STEP 10--                   **
C               **  SAVE THE CURRENT POINTERS   **
C               **  (FOR POSSIBLE LATER USE     **
C               **  BY THE   UNDO     COMMAND)  **
C               **********************************
C
      ISTEPN='10'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDSACP
C
C               ************************************
C               **  STEP 11--                     **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE DELETED.   **
C               ************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=ICURLN
C
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      GOTO1120
C
 1100 CONTINUE
      ISTART=ICURLN
      ISTOP=ICURLN
      GOTO1190
C
 1110 CONTINUE
CCCCC IARG1=NUMLIN+1
      IARG1=1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      ISTART=ICURLN
      ISTOP=ISTART+IARG1-1
      GOTO1190
C
 1120 CONTINUE
CCCCC IARG1=NUMLIN+1
      IARG1=1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
CCCCC IARG2=NUMLIN+1
      IARG2=1
      IF(IARGT(2).EQ.'NUMB')IARG2=IARG(2)
      ISTART=IARG1
      ISTOP=IARG2
      GOTO1190
C
 1190 CONTINUE
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD2.LT.IHOLD1)ISTART=IHOLD2
      IF(IHOLD2.LT.IHOLD1)ISTOP=IHOLD1
C
C               ************************************************
C               **  STEP 21--                                 **
C               **  WRITE OUT A MESSAGE                       **
C               **  IF ATTEMPT TO DELETE BEFORE LINE 1, OR    **
C               **  IF ATTEMPT TO DELETE BEYOND LINE NUMLIN.  **
C               ************************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.GE.1)GOTO1219
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1211)
         CALL EDWRST('EDDELE')
      ENDIF
 1211 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1212)
         CALL EDWRST('EDDELE')
      ENDIF
 1212 FORMAT(10X,'[TOP]')
 1219 CONTINUE
C
      IF(ISTOP.LE.NUMLIN)GOTO1229
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1221)
         CALL EDWRST('EDDELE')
      ENDIF
 1221 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1222)
         CALL EDWRST('EDDELE')
      ENDIF
 1222 FORMAT(10X,'[BOTTOM]')
 1229 CONTINUE
C
C               ****************************
C               **  STEP 22--             **
C               **  DELETE     THE LINES. **
C               ****************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LT.1)ITOP='YES'
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
C
      IF(ISTART.LT.1.AND.ISTOP.LT.1)GOTO9000
      IF(ISTART.GT.NUMLIN.AND.ISTOP.GT.NUMLIN)GOTO9000
C
      IF(ISTART.LT.1)ISTART=1
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      IDEL=ISTOP-ISTART+1
      ISTOPP=ISTOP+1
      IF(ISTOPP.LE.0)GOTO1519
      IF(ISTOPP.GT.NUMLIN)GOTO1519
      DO1510ILINE=ISTOPP,NUMLIN
      ILINEM=ILINE-IDEL
      IF(ILINEM.GE.1)IPOINT(ILINEM)=IPOINT(ILINE)
      IPOINT(ILINE)=(-999)
 1510 CONTINUE
 1519 CONTINUE
      NUMLIN=NUMLIN-IDEL
C
      IF(1.LE.ISTART.AND.ISTART.LE.NUMLIN)GOTO1520
      GOTO1529
C
 1520 CONTINUE
CCCCC ILINE=ISTART-1
CCCCC IF(ILINE.LE.0)GOTO1529
CCCCC IROW=IPOINT(ILINE)
CCCCC J1=ILOCC1(IROW)
CCCCC N1=NUMCPL(IROW)
CCCCC J2=J1+(N1-1)
      ILINE=ISTART
 1529 CONTINUE
C
      IF(IFEESW.EQ.'OFF')GOTO1539
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1533)
 1533    FORMAT('[DELETED LINE]')
         CALL EDWRST('EDDELE')
      ENDIF
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1534)ILINE
 1534    FORMAT(I6,':',3X,'[DELETED LINE]')
         CALL EDWRST('EDDELE')
      ENDIF
 1539 CONTINUE
      ICURLN=ISTART
      IDELSW='ON'
      IDELLN=ICURLN
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DELE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDDELE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDDELE')
 9011 FORMAT('*****AT THE END       OF EDDELE--')
      WRITE(ICOUT,9012)ITOP,IEOF,ISTART,ISTOP
      CALL EDWRST('EDDELE')
 9012 FORMAT('ITOP,IEOF,ISTART,ISTOP = ',A4,2X,A4,2I8)
      WRITE(ICOUT,9013)ICURLN,IDELSW,IDELLN
      CALL EDWRST('EDDELE')
 9013 FORMAT('ICURLN,IDELSW,IDELLN = ',I8,2X,A4,I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDDELE')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDDELI(ICURL2,IFEES2)
C
C     PURPOSE--DELETE LINE ICURL2.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.1
C     ORIGINAL VERSION--MAY     1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEES2
C
      CHARACTER*4 ITOP
      CHARACTER*4 IEOF
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDDE'
      ISUBN2='LI  '
C
      IFOUND='YES'
      IERROR='NO'
C
      ITOP='NO'
      IEOF='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DELI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDDELI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDDELI')
   51 FORMAT('*****AT THE BEGINNING OF EDDELI--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
      WRITE(ICOUT,52)ICURL2,IDELSW,IDELLN
      CALL EDWRST('EDDELI')
   52 FORMAT('ICURL2,IDELSW,IDELLN = ',I8,2X,A4,I8)
      WRITE(ICOUT,53)IFEES2
      CALL EDWRST('EDDELI')
   53 FORMAT('IFEES2 = ',A4)
   90 CONTINUE
C
C               **********************************
C               **  STEP 10--                   **
C               **  SAVE THE CURRENT POINTERS   **
C               **  (FOR POSSIBLE LATER USE     **
C               **  BY THE   UNDO     COMMAND)  **
C               **********************************
C
      ISTEPN='10'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDSACP
C
C               ************************************
C               **  STEP 11--                     **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE DELETED.   **
C               ************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURL2
      ISTOP=ICURL2
C
C               ************************************************
C               **  STEP 21--                                 **
C               **  WRITE OUT A MESSAGE                       **
C               **  IF ATTEMPT TO DELETE BEFORE LINE 1, OR    **
C               **  IF ATTEMPT TO DELETE BEYOND LINE NUMLIN.  **
C               ************************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.GE.1)GOTO1219
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1211)
         CALL EDWRST('EDDELI')
      ENDIF
 1211 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1212)
         CALL EDWRST('EDDELI')
      ENDIF
 1212 FORMAT(10X,'[TOP]')
 1219 CONTINUE
C
      IF(ISTOP.LE.NUMLIN)GOTO1229
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1221)
         CALL EDWRST('EDDELI')
      ENDIF
 1221 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1222)
         CALL EDWRST('EDDELI')
      ENDIF
 1222 FORMAT(10X,'[BOTTOM]')
 1229 CONTINUE
C
C               ****************************
C               **  STEP 22--             **
C               **  DELETE     THE LINES. **
C               ****************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LT.1)ITOP='YES'
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
C
      IF(ISTART.LT.1.AND.ISTOP.LT.1)GOTO9000
      IF(ISTART.GT.NUMLIN.AND.ISTOP.GT.NUMLIN)GOTO9000
C
      IF(ISTART.LT.1)ISTART=1
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      IDEL=ISTOP-ISTART+1
      ISTOPP=ISTOP+1
      IF(ISTOPP.LE.0)GOTO1519
      IF(ISTOPP.GT.NUMLIN)GOTO1519
      DO1510ILINE=ISTOPP,NUMLIN
      ILINEM=ILINE-IDEL
      IF(ILINEM.GE.1)IPOINT(ILINEM)=IPOINT(ILINE)
      IPOINT(ILINE)=(-999)
 1510 CONTINUE
 1519 CONTINUE
      NUMLIN=NUMLIN-IDEL
C
      IF(1.LE.ISTART.AND.ISTART.LE.NUMLIN)GOTO1520
      GOTO1529
C
 1520 CONTINUE
CCCCC ILINE=ISTART-1
CCCCC IF(ILINE.LE.0)GOTO1529
CCCCC IROW=IPOINT(ILINE)
CCCCC J1=ILOCC1(IROW)
CCCCC N1=NUMCPL(IROW)
CCCCC J2=J1+(N1-1)
      ILINE=ISTART
 1529 CONTINUE
C
      IF(IFEES2.EQ.'OFF')GOTO1539
      IF(IFEESW.EQ.'OFF')GOTO1539
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1533)
         CALL EDWRST('EDDELI')
      ENDIF
CCCCC    WRITE(ICOUT,1533)(ICHA(J),J=J1,J2)
C1533 FORMAT(238A1)
 1533 FORMAT('[DELETED LINE]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1534)ILINE
 1534    FORMAT(I6,':',3X,'[DELETED LINE]')
         CALL EDWRST('EDDELI')
      ENDIF
 1539 CONTINUE
      ICURLN=ISTART
CCCCC IDELSW='ON'
CCCCC IDELLN=ICURLN
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DELI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDDELI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDDELI')
 9011 FORMAT('*****AT THE END       OF EDDELI--')
      WRITE(ICOUT,9012)ITOP,IEOF,ISTART,ISTOP
      CALL EDWRST('EDDELI')
 9012 FORMAT('ITOP,IEOF,ISTART,ISTOP = ',A4,2X,A4,2I8)
      WRITE(ICOUT,9013)ICURL2,IDELSW,IDELLN
      CALL EDWRST('EDDELI')
 9013 FORMAT('ICURL2,IDELSW,IDELLN = ',I8,2X,A4,I8)
      WRITE(ICOUT,9014)IFEES2
      CALL EDWRST('EDDELI')
 9014 FORMAT('IFEES2 = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDDELI')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDDEMN(IMANUF,IMODEL)
C
C     PURPOSE--SPECIFY DEVICE MANUFACTURER
C              (THAT IS, TERMINAL MANUFACTURER AND MODEL)
C
C     DATE--JULY 24, 1986
C
C-------------------------------------------------------------------------------
C
      CHARACTER*4 IMANUF
      CHARACTER*4 IMODEL
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
CCCCC THE FOLLOWING LINE WAS ADDED (FOR MENU/GUI)   MAY 1993
      INCLUDE 'DPCODV.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      ISUBN1='EDDE'
      ISUBN2='MN  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DEMN')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEMN')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDDEMN')
   51 FORMAT('***** AT THE BEGINNING OF EDDEMN--')
      WRITE(ICOUT,52)ICOM,ICOM2,NUMARG,IHARG(1)
      CALL EDWRST('EDDEMN')
   52 FORMAT('ICOM,ICOM2,NUMARG,IHARG(1) = ',A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,53)IHARG(2),IHARG(3)
      CALL EDWRST('EDDEMN')
   53 FORMAT('IHARG(2),IHARG(3) = ',A4,2X,A4)
      WRITE(ICOUT,54)IMANUF,IMODEL
      CALL EDWRST('EDDEMN')
   54 FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
      WRITE(ICOUT,55)IFOUND,IERROR
      CALL EDWRST('EDDEMN')
   55 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
   90 CONTINUE
C
      IF(IHARG(2).EQ.'TEKT')GOTO1010
      IF(IHARG(2).EQ.'IBM')GOTO1020
      IF(IHARG(2).EQ.'IBM-')GOTO1020
      IF(IHARG(2).EQ.'HDS')GOTO1030
      IF(IHARG(2).EQ.'QUME')GOTO1040
      IF(IHARG(2).EQ.'VT')GOTO1050
      IF(IHARG(2).EQ.'VT-1')GOTO1050
      IF(IHARG(2).EQ.'VT-2')GOTO1050
C
      IF(IHARG(2).EQ.'DOS')GOTO2100
      IF(IHARG(2).EQ.'UNIX')GOTO2200
C
      IF(IHARG(2).EQ.'TURB')GOTO3100
      IF(IHARG(2).EQ.'X11')GOTO3200
      IF(IHARG(2).EQ.'X')GOTO3200
      IF(IHARG(2).EQ.'MOTI')GOTO3300
      IF(IHARG(2).EQ.'MAC')GOTO3400
      IF(IHARG(2).EQ.'MACI')GOTO3400
C
      IFOUND='NO'
      GOTO9000
C
 1010 CONTINUE
      IMANUF='TEKT'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 1020 CONTINUE
      IMANUF='IBM'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 1030 CONTINUE
      IMANUF='HDS'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 1040 CONTINUE
      IMANUF='QUME'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 1050 CONTINUE
      IMANUF='VT'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 2100 CONTINUE
      IMANUF='DOS'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 2200 CONTINUE
      IMANUF='UNIX'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 3100 CONTINUE
      IMANUF='TURB'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
CCCCC THE FOLLOWING 3 LINES WERE ADDED (FOR MENU/GUI)   MAY 1993
CTURB CALL TCINCO(ISUBRO)
CTURB CALL TCLOTC(ISUBRO)
CTURB CALL TCSHME(ISUBRO)
      GOTO8000
C
 3200 CONTINUE
      IMANUF='X11'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 3300 CONTINUE
      IMANUF='MOTI'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 3400 CONTINUE
      IMANUF='MAC'
      IMODEL='    '
      IF(NUMARG.GE.3)IMODEL=IHARG(3)
      GOTO8000
C
 8000 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1919
      WRITE(ICOUT,1911)IMANUF,IMODEL
      CALL EDWRST('EDDEMN')
 1911 FORMAT('TERMINAL = ',A4,2X,A4)
 1919 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DEMN')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEMN')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDDEMN')
 9011 FORMAT('***** AT THE END       OF EDDEMN--')
      WRITE(ICOUT,9012)ICOM,ICOM2,NUMARG,IHARG(1)
      CALL EDWRST('EDDEMN')
 9012 FORMAT('ICOM,ICOM2,NUMARG,IHARG(1) = ',A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,9013)IHARG(2),IHARG(3)
      CALL EDWRST('EDDEMN')
 9013 FORMAT('IHARG(2),IHARG(3) = ',A4,2X,A4)
      WRITE(ICOUT,9014)IMANUF,IMODEL
      CALL EDWRST('EDDEMN')
 9014 FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
      WRITE(ICOUT,9015)IFOUND,IERROR
      CALL EDWRST('EDDEMN')
 9015 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDDEPR
C
C     PURPOSE--DEACTIVATE THE PRINTER
C              (FOR THE IBM-PC PRINTER,
C              SEND DOWN A CONTROL LOWER CASE P
C              = ASCII DLE = CHAR(16)  )
C     NOTE--NO ARGUMENTS ARE EXPECTED OR PROCESSED
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDDE'
      ISUBN2='PR  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DEPR')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEPR')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDDEPR')
   51 FORMAT('***** AT THE BEGINNING OF EDDEPR--')
   90 CONTINUE
C
      NCOUT=1
      ICOUT(1:1)=IDLEC
      ILOUT=(-1)
      CALL EDWRST('EDDEPR')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDDEPR')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDDEPR')
 9011 FORMAT('***** AT THE END       OF EDDEPR--')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDDI
C
C     PURPOSE--DELETE AND REWRIE CURRENT LINE OF TEXT
C              THE STRING WHICH FOLLOWS THE INSERT COMMAND
C              WILL REPLACE THE CURRENT LINE.
C              IF NO STRING FOLLOWS THE REWRITE COMMAND,
C              THEN A BLANK LINE (CONSISTING OF 1 BLANK CHARACTER)
C              WILL BE INSERTED.
C     COMMAND SYNTAX--REWRITE <STRING>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
CCCCC ADD FOLLOWING LINE NOVEMBER 1994.
      CHARACTER*4 IFOUST
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDDI'
      ISUBN2='    '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDDI  ')
   51 FORMAT('***** AT THE BEGINNING OF EDDI--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  DETERMINE THE STRING THAT WILL  **
C               **  REPLACE THE CURRENT LINE.       **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUST)
C
C               ****************************
C               **  STEP 2--              **
C               **  REWRITE    THE LINE.  **
C               ****************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMLIN.GE.MAXEDL)GOTO1510
      IF(NUMROW.GE.MAXROW)GOTO1520
      IF(NUMCHA.GE.MAXEDC)GOTO1530
      NUMCH2=NUMCHA+NUMCST
      IF(NUMCH2.GT.MAXEDC)GOTO1540
      GOTO1550
C
 1510 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      WRITE(ICOUT,1511)
      CALL EDWRST('EDDI  ')
 1511 FORMAT('***** ERROR IN EDDI--')
      WRITE(ICOUT,1512)
      CALL EDWRST('EDDI  ')
 1512 FORMAT('      NO ROOM LEFT FOR MORE LINES.')
      WRITE(ICOUT,1513)NUMLIN,MAXEDL
      CALL EDWRST('EDDI  ')
 1513 FORMAT('      NUMLIN (',I8,') >= MAXEDL (',I8,')')
      IERROR='YES'
      GOTO9000
C
 1520 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      WRITE(ICOUT,1521)
      CALL EDWRST('EDDI  ')
 1521 FORMAT('***** ERROR IN EDDI--')
      WRITE(ICOUT,1522)
      CALL EDWRST('EDDI  ')
 1522 FORMAT('      NO ROOM LEFT FOR MORE ROWS.')
      WRITE(ICOUT,1523)NUMROW,MAXROW
      CALL EDWRST('EDDI  ')
 1523 FORMAT('      NUMROW (',I8,') >= MAXROW (',I8,')')
      IERROR='YES'
      GOTO9000
C
 1530 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      WRITE(ICOUT,1531)
      CALL EDWRST('EDDI  ')
 1531 FORMAT('***** ERROR IN EDDI--')
      WRITE(ICOUT,1532)
      CALL EDWRST('EDDI  ')
 1532 FORMAT('      NO ROOM LEFT FOR MORE CHARACTERS.')
      WRITE(ICOUT,1533)NUMCHA,MAXEDC
      CALL EDWRST('EDDI  ')
 1533 FORMAT('      NUMCHA (',I8,') >= MAXEDC (',I8,')')
      IERROR='YES'
      GOTO9000
C
 1540 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      WRITE(ICOUT,1541)
      CALL EDWRST('EDDI  ')
 1541 FORMAT('***** ERROR IN EDDI--')
      WRITE(ICOUT,1542)
      CALL EDWRST('EDDI  ')
 1542 FORMAT('      NEW STRING WOULD EXCEED ICHA(.) WORKSPACE.')
      WRITE(ICOUT,1543)NUMCHA,NUMCST,MAXEDC
      CALL EDWRST('EDDI  ')
 1543 FORMAT('      NUMCHA,NUMCST,MAXEDC = ',3I8)
      IERROR='YES'
      GOTO9000
C
 1550 CONTINUE
      IF(ICURLN.LT.1)GOTO1560
      IF(ICURLN.GT.NUMLIN)GOTO1570
      GOTO1580
C
 1560 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      WRITE(ICOUT,1561)
      CALL EDWRST('EDDI  ')
 1561 FORMAT('***** ERROR IN EDDI--')
      WRITE(ICOUT,1562)
      CALL EDWRST('EDDI  ')
 1562 FORMAT('      ATTEMPT TO REWRITE THE NON-EXISTENT LINE.')
      WRITE(ICOUT,1563)
      CALL EDWRST('EDDI  ')
 1563 FORMAT('      AT THE TOP OF THE FILE ("LINE" 0)')
      IERROR='YES'
      GOTO9000
C
 1570 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      WRITE(ICOUT,1571)
      CALL EDWRST('EDDI  ')
 1571 FORMAT('***** ERROR IN EDDI--')
      WRITE(ICOUT,1572)
      CALL EDWRST('EDDI  ')
 1572 FORMAT('      ATTEMPT TO REWRITE THE NON-EXISTENT LINE.')
      WRITE(ICOUT,1573)
      CALL EDWRST('EDDI  ')
 1573 FORMAT('      AT THE BOTTOM OF THE FILE ("LINE" NUMLIN+1 )')
      IERROR='YES'
      GOTO9000
C
 1580 CONTINUE
      J1=NUMCHA+1
      N1=NUMCST
      J2=J1+(N1-1)
C
      NUMRP1=NUMROW+1
      IROW=NUMRP1
      IPOINT(ICURLN)=IROW
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO1585J=J1,J2
      I=I+1
      ICHA(J)=ISTRIN(I)
 1585 CONTINUE
 1589 CONTINUE
      NUMROW=NUMRP1
      NUMCHA=J2
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDDI  ')
 9011 FORMAT('***** AT THE END       OF EDDI--')
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      WRITE(ICOUT,9014)NUMCST
      CALL EDWRST('EDDI  ')
 9014 FORMAT('NUMCST = ',I8)
      WRITE(ICOUT,9015)(ISTRIN(I),I=1,NUMCST)
      CALL EDWRST('EDDI  ')
 9015 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDDI  ')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDDI  ')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDDUMP
C
C     PURPOSE--DUMP TRACE/DEBUG INFORMATION
C
C     DATE--JANUARY 24,1985
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      IFOUND='YES'
C
      CALL EDTRA1
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE EDDUPL
C
C     PURPOSE--INSERT 1 LINE OF TEXT (AFTER CURRENT LINE).
C              THE STRING WHICH FOLLOWS THE INSERT COMMAND
C              WILL BE INSERTED.
C              IF NO STRING FOLLOWS THE INSERT COMMAND,
C              THEN A BLANK LINE (CONSISTING OF 1 BLANK CHARACTER)
C              WILL BE INSERTED.
C     COMMAND SYNTAX--INSERT <STRING>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ISTRIN
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDDU'
      ISUBN2='PL  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      NUMCST=(-999)
      ISTRIN(1:1)=' '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DUPL')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDDUPL')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDDUPL')
   51 FORMAT('***** AT THE BEGINNING OF EDDUPL--')
      WRITE(ICOUT,52)NCHH1,NCHH2,NCHH3
      CALL EDWRST('EDDUPL')
   52 FORMAT('NCHH1,NCHH2,NCHH3 = ',3I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE STRING TO BE INSERTED  **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DUPL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      DO1110I=1,240
      ISTRIN(I:I)=' '
 1110 CONTINUE
C
      IARG1=1
      IF(NUMARG.GE.1)IARG1=IARG(1)
C
      IF(IARG1.EQ.1)NUMCST=NCHH1
      IF(IARG1.EQ.2)NUMCST=NCHH2
      IF(IARG1.EQ.3)NUMCST=NCHH3
      IF(IARG1.EQ.4)NUMCST=NCHH4
      IF(IARG1.EQ.5)NUMCST=NCHH5
      IF(IARG1.EQ.6)NUMCST=NCHH6
      IF(IARG1.EQ.7)NUMCST=NCHH7
      IF(IARG1.EQ.8)NUMCST=NCHH8
      IF(IARG1.EQ.9)NUMCST=NCHH9
      IF(IARG1.EQ.10)NUMCST=NCHH10
C
      IF(NUMCST.LE.0)GOTO1190
      DO1120I=1,NUMCST
      IF(IARG1.EQ.1)ISTRIN(I:I)=IHOLS1(I:I)
      IF(IARG1.EQ.2)ISTRIN(I:I)=IHOLS2(I:I)
      IF(IARG1.EQ.3)ISTRIN(I:I)=IHOLS3(I:I)
      IF(IARG1.EQ.4)ISTRIN(I:I)=IHOLS4(I:I)
      IF(IARG1.EQ.5)ISTRIN(I:I)=IHOLS5(I:I)
      IF(IARG1.EQ.6)ISTRIN(I:I)=IHOLS6(I:I)
      IF(IARG1.EQ.7)ISTRIN(I:I)=IHOLS7(I:I)
      IF(IARG1.EQ.8)ISTRIN(I:I)=IHOLS8(I:I)
      IF(IARG1.EQ.9)ISTRIN(I:I)=IHOLS9(I:I)
      IF(IARG1.EQ.10)ISTRIN(I:I)=IHOL10(I:I)
 1120 CONTINUE
 1190 CONTINUE
C
      IF(NUMCST.LE.0)ISTRIN(1:1)=' '
      IF(NUMCST.LE.0)NUMCST=1
C
C               *****************************************************
C               **  STEP 2--                                       **
C               **  DETERMINE THE NUMBER OF TIMES                  **
C               **  THAT THE STRING WILL BE INSERTED (DUPLICATED)  **
C               *****************************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DUPL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IARG2=1
      IF(NUMARG.GE.2)IARG2=IARG(2)
      NUMDUP=IARG2
C
C               ****************************
C               **  STEP 3--              **
C               **  INSERT     THE LINE.  **
C               ****************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DUPL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCHP=NUMCHA+NUMCST*NUMDUP
      CALL EDCHSI(NUMCHP)
      IF(IERROR.EQ.'YES')GOTO9000
C
 1550 CONTINUE
      DO1555IDUP=1,NUMDUP
C
      IF(ICURLN.GT.NUMLIN)ICURLN=NUMLIN
      ICURLP=ICURLN+1
      NUMLP1=NUMLIN+1
      NUMRP1=NUMROW+1
C
      IHOLD1=IPOINT(ICURLP)
      IPOINT(ICURLP)=NUMRP1
C
      IF(ICURLP.LE.0)GOTO1569
      IF(NUMLIN.LE.0)GOTO1569
      IF(ICURLP.GT.NUMLIN)GOTO1569
      DO1560ILINE=ICURLP,NUMLIN
      ILINEP=ILINE+1
      IHOLD2=IPOINT(ILINEP)
      IPOINT(ILINEP)=IHOLD1
      IHOLD1=IHOLD2
 1560 CONTINUE
 1569 CONTINUE
C
      J1=NUMCHA+1
      N1=NUMCST
      J2=J1+(N1-1)
C
      IROW=NUMRP1
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO1570J=J1,J2
      I=I+1
      ICHA(J)=ISTRIN(I:I)
 1570 CONTINUE
 1579 CONTINUE
      ICURLN=ICURLP
      IF(IDUP.EQ.1)IMIN=ICURLN
      NUMLIN=NUMLP1
      NUMROW=NUMRP1
      NUMCHA=J2
C
 1555 CONTINUE
      IMAX=ICURLN
C
C               ***************************
C               **  STEP 23--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      ISTEPN='23'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'COPY')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
C
      IF(IFEESW.EQ.'OFF')GOTO2329
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDDUPL')
      IDEL=IMAX-IMIN+1
      ILINE=IMIN-1
      DO2320IPASS=1,10000
      ILINE=ILINE+1
      IF(IPASS.GE.2.AND.IDEL.GE.11)ILINE=IMAX
      IF(ILINE.LT.1)GOTO2325
      IF(ILINE.GT.IMAX)GOTO2325
      IF(ILINE.GT.NUMLIN)GOTO2325
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2321)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDDUPL')
      ENDIF
 2321 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2322)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDDUPL')
      ENDIF
 2322 FORMAT(I6,':',3X,230A1)
 2325 CONTINUE
      IF(IPASS.GE.2.AND.IDEL.GE.11)GOTO2328
 2320 CONTINUE
 2328 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDDUPL')
 2329 CONTINUE
C
C               **********************
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DUPL')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDDUPL')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDDUPL')
 9011 FORMAT('***** AT THE END       OF EDDUPL--')
      WRITE(ICOUT,9012)IWIDTH
      CALL EDWRST('EDDUPL')
 9012 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9014)NUMCST
      CALL EDWRST('EDDUPL')
 9014 FORMAT('NUMCST = ',I8)
      WRITE(ICOUT,9015)(ISTRIN(I:I),I=1,NUMCST)
      CALL EDWRST('EDDUPL')
 9015 FORMAT('(ISTRIN(I:I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDDUPL')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)NUMARG,IARG(1),IARG(2)
      CALL EDWRST('EDDUPL')
 9018 FORMAT('NUMARG,IARG(1),IARG(2) = ',3I8)
      WRITE(ICOUT,9019)IARG1,IARG2,NUMDUP
      CALL EDWRST('EDDUPL')
 9019 FORMAT('IARG1,IARG2,NUMDUP = ',3I8)
      WRITE(ICOUT,9021)ICURLN,NUMLIN,NUMROW
      CALL EDWRST('EDDUPL')
 9021 FORMAT('ICURLN,NUMLIN,NUMROW = ',3I8)
      WRITE(ICOUT,9022)IMIN,IMAX
      CALL EDWRST('EDDUPL')
 9022 FORMAT('IMIN,IMAX = ',2I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDDUPL')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDEA
C
C     PURPOSE--EXTEND ALL LINES BY     STRING    .
C              START WITH THE CURRENT LINE.
C              THE STRING WHICH FOLLOWS THE EXTEND COMMAND
C              WILL BE APPENDED.
C     COMMAND SYNTAX--EA <STRING>
C     METHOD--LOOPING WITH THE EXTEND COMMAND.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.6
C     ORIGINAL VERSION--MAY 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IANS0
C
      DIMENSION IANS0(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDEA'
      ISUBN2='    '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'EA')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDEA  ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDEA  ')
   51 FORMAT('***** AT THE BEGINNING OF EDEA--')
      WRITE(ICOUT,52)NUMARG,IWIDTH
      CALL EDWRST('EDEA  ')
   52 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 11--                            **
C               **  COPY THE OPIGINAL COMMAND LINE       **
C               *******************************************
C
      NUMASV=NUMARG
      IWIDT0=IWIDTH
      DO1100I=1,IWIDTH
      IANS0(I)=IANS(I)
      IF(IBUGED.EQ.'OFF'.AND.ISUBRO.EQ.'EA')THEN
         WRITE(ICOUT,1111)I,IANS(I),IANS0(I)
         CALL EDWRST('EDEA  ')
      ENDIF
 1111 FORMAT('I,IANS(I),IANS0(I) = ',I8,2X,A1,2X,A1)
 1100 CONTINUE
C
C               *******************************************
C               **  STEP 21--                             **
C               **  LOOP THROUGH (CURRENT +)              **
C               **  REMAINING LINES OF TEXT               **
C               **  VIA MULTIPLE CALLS                    **
C               **  TO THE EXTEND COMMAND                 **
C               *******************************************
C
      DO2100IDUMMY=1,100000
C
      IF(IBUGED.EQ.'OFF'.AND.ISUBRO.EQ.'EA')THEN
         WRITE(ICOUT,2119)IDUMMY,ICURLN,NUMLIN
         CALL EDWRST('EDEA  ')
      ENDIF
 2119 FORMAT('IDUMMY,ICURLN,NUMLIN = ',3I8)
      IF(ICURLN.GT.NUMLIN)GOTO2170
C
      IANS(1)='E'
      IANS(2)='X'
      IANS(3)='T'
      IANS(4)=' '
      J=4
      IWIDTH=2
      IMIN=IWIDTH+2
      IF(IWIDT0.LT.IMIN)GOTO2118
      DO2110I=IMIN,IWIDT0
      IF(IANS0(I).EQ.' ')IWIDTH=J
      IF(IANS0(I).EQ.' ')GOTO2118
      J=J+1
      IANS(J)=IANS0(I)
      IF(IBUGED.EQ.'OFF'.AND.ISUBRO.EQ.'EA')THEN
         WRITE(ICOUT,2111)I,J,IANS0(I),IANS(J)
         CALL EDWRST('EDEA  ')
      ENDIF
 2111 FORMAT('I,J,IANS0(I),IANS(J) = ',2I8,2X,A1,2X,A1)
 2110 CONTINUE
      IWIDTH=J
 2118 CONTINUE
      NUMARG=1
      ICOM='EXT'
      CALL EDEXTE
C
      ICOM='N   '
      ICOM2='    '
      NUMARG=0
      CALL EDNEXT
C
 2100 CONTINUE
C
 2170 CONTINUE
      IWIDTH=IWIDT0
      DO2175I=1,IWIDT0
      IANS(I)=IANS0(I)
 2175 CONTINUE
      ICOM='EA  '
      ICOM2='    '
      NUMARG=NUMASV
      IFOUND='YES'
      IERROR='NO'
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'EA')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDEA  ')
      WRITE(ICOUT,9021)
      CALL EDWRST('EDEA  ')
 9021 FORMAT('***** AT THE END       OF EDEA--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
      WRITE(ICOUT,999)
      CALL EDWRST('EDEA  ')
      WRITE(ICOUT,9012)NUMARG,IWIDTH
      CALL EDWRST('EDEA  ')
 9012 FORMAT('NUMARG,IWIDTH = ',2I8)
      WRITE(ICOUT,9013)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDEA  ')
 9013 FORMAT('(IANS(I),I=1,IWIDTH) = ',210A1)
      WRITE(ICOUT,9014)IWIDT0
      CALL EDWRST('EDEA  ')
 9014 FORMAT('IWIDT0 = ',I8)
      WRITE(ICOUT,9015)(IANS0(I),I=1,IWIDT0)
      CALL EDWRST('EDEA  ')
 9015 FORMAT('(IANS0(I),I=1,IWIDT0) = ',80A1)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDECCO
C
C     PURPOSE--ECHO A COMMAND LINE BACK
C              (THAT IS, PUT IT IN A BOX FOR NOTICEABILITY)
C     NOTE--NO ARGUMENTS ARE EXPECTED OR PROCESSED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IAST
      CHARACTER*1 IBL
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDEC'
      ISUBN2='CO  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ECCO')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDECCO')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDECCO')
   51 FORMAT('*****AT THE BEGINNING OF EDECCO--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************************
C               **  STEP 1--                       **
C               **  ECHO THE COMMAND LINE          **
C               *************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ECCO')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IAST='*'
      IBL=' '
C
      IMAX=IWIDTH+8
      WRITE(ICOUT,999)
      CALL EDWRST('EDECCO')
      WRITE(ICOUT,1211)(IAST,I=1,IMAX)
      CALL EDWRST('EDECCO')
 1211 FORMAT(10X,230A1)
      WRITE(ICOUT,1212)IAST,IAST,IBL,IBL,(IANS(I),I=1,IWIDTH),
     1IBL,IBL,IAST,IAST
      CALL EDWRST('EDECCO')
 1212 FORMAT(10X,230A1)
      WRITE(ICOUT,1213)(IAST,I=1,IMAX)
      CALL EDWRST('EDECCO')
 1213 FORMAT(10X,230A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDECCO')
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ECCO')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDECCO')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDECCO')
 9011 FORMAT('*****AT THE END       OF EDECCO--')
      WRITE(ICOUT,9012)IWIDTH,IMAX
      CALL EDWRST('EDECCO')
 9012 FORMAT('IWIDTH,IMAX = ',2I8)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDEDIT
C
C     PURPOSE--CHANGE TO EDIT MODE (FROM INPUT MODE)
C
C     DATE--JANUARY 24,1985
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      ISUBN1='EDED'
      ISUBN2='IT  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'EDIT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDEDIT')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDEDIT')
   51 FORMAT('*****AT THE BEGINNING OF EDEDIT--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IMODE='EDIT'
C
CCCCC IF(IFEESW.EQ.'OFF')GOTO1159
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1151)
         CALL EDWRST('EDEDIT')
      ENDIF
 1151 FORMAT('YOU HAVE JUST ENTERED EDIT MODE')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1152)
         CALL EDWRST('EDEDIT')
      ENDIF
 1152 FORMAT(10X,'YOU HAVE JUST ENTERED EDIT MODE')
 1159 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'EDIT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDEDIT')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDEDIT')
 9011 FORMAT('*****AT THE END       OF EDEDIT--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDERAS(IMANUF,IMODEL)
C
C     PURPOSE--WRITE OUT THE CONTENTS OF THE ICERAS STRING
C              WHICH (IF PROPERLY SET) WILL ERASE THE TERMINAL SCREEN.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED         --APRIL   1990.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IMANUF
      CHARACTER*4 IMODEL
C
      CHARACTER*238 ICSTR
      CHARACTER*4 ISUBN0
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDER'
      ISUBN2='AS  '
C
      IFOUND='YES'
      IERROR='NO'
C
      ISUBN0='ERAS'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ERAS')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDERAS')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDERAS')
   51 FORMAT('***** AT THE BEGINNING OF EDERAS--')
      WRITE(ICOUT,52)IMANUF,IMODEL
      CALL EDWRST('EDERAS')
   52 FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ***************************
C               **  STEP 11--            **
C               **  ERASE THE SCREEN     **
C               **  AND POSITION CURSOR  **
C               **  AT TOP OF SCREEN     **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ERAS')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC WRITE(ICOUT,1111)ICERAS
C1111 FORMAT(A4)
C
C     THE FOLLOWING 2 ASSEMBLY CALLS
C     ARE COMMENTED OUT IN THE VAX VERSION--
C
CVVVV CALL ASERSC
CVVVV CALL ASPOCT
C
C               ********************************************
C               **  STEP 1--                              **
C               **  BRANCH ACCORDING TO THE MANUFACTURER  **
C               **  AND THE MODEL                         **
C               ********************************************
C
      IF(IMANUF.EQ.'TEKT')GOTO1010
      IF(IMANUF.EQ.'HP')GOTO1020
      IF(IMANUF.EQ.'GENE')GOTO1030
      IF(IMANUF.EQ.'IBM')GOTO1040
      IF(IMANUF.EQ.'HDS')GOTO1050
      IF(IMANUF.EQ.'QUME')GOTO1060
      IF(IMANUF.EQ.'VT')GOTO1070
      IF(IMANUF.EQ.'MAC')GOTO1080
      IF(IMANUF.EQ.'FORT')GOTO1090
      GOTO9000
C
 1010 CONTINUE
      IF(IMODEL.EQ.'4662')GOTO9000
C
      IF(IMODEL.EQ.'4020')GOTO1200
      IF(IMODEL.EQ.'4022')GOTO1200
      IF(IMODEL.EQ.'4025')GOTO1200
      IF(IMODEL.EQ.'4027')GOTO1200
C
      IF(IMODEL.EQ.'4105')GOTO1300
      IF(IMODEL.EQ.'4107')GOTO1300
      IF(IMODEL.EQ.'4109')GOTO1300
      IF(IMODEL.EQ.'4115')GOTO1300
      IF(IMODEL.EQ.'4107')GOTO1300
      IF(IMODEL.EQ.'4109')GOTO1300
      IF(IMODEL.EQ.'4115')GOTO1300
C
      GOTO1100
C
 1020 CONTINUE
      IF(IMODEL.EQ.'7221')GOTO2100
      IF(IMODEL.EQ.'2622')GOTO2300
      IF(IMODEL.EQ.'2623')GOTO2300
      IF(IMODEL.EQ.'2627')GOTO2300
      IF(IMODEL.EQ.'2647')GOTO2300
      GOTO2200
C
 1030 CONTINUE
      IF(IMODEL.EQ.'CODE')GOTO3200
      GOTO3100
C
 1040 CONTINUE
      GOTO4100
C
 1050 CONTINUE
      GOTO5100
C
 1060 CONTINUE
      GOTO6100
C
 1070 CONTINUE
      GOTO7100
C
 1080 CONTINUE
      GOTO8100
C
 1090 CONTINUE
      GOTO9100
 
C               ************************************************************
C               **  STEP 11--                                             **
C               **  TREAT THE TEKTRONIX 400X, 401X, 405X, AND 4114 CASES  **
C               **  (THESE ARE ALL NON-COLOR (= MONOCHROME) DEVICES)      **
C               **  TO ERASE THE SCREEN,                                  **
C               **  WRITE OUT AN ESCAPE FORM-FEED                         **
C               ************************************************************
C
 1100 CONTINUE
      ICSTR(1:1)=IESCC
      ICSTR(2:2)=IFFC
      NCSTR=2
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
C
CCCCC DO1130J=1,10
CCCCC ICSTR(J:J)=INULC
C1130 CONTINUE
CCCCC NCSTR=10
CCCCC
CCCCC AGERDE=10.0
CCCCC INULLI=AGERDE+0.5
CCCCC IF(INULLI.LE.0)GOTO1139
CCCCC DO1135I=1,INULLI
CCCCC CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
C1135 CONTINUE
C1139 CONTINUE
C
      GOTO9000
C
C               ******************************************************
C               **  STEP 12--                                       **
C               **  TREAT THE TEKTRONIX 4020, 4022, AND 4025 CASES  **
C               **  (NON-COLOR RASTER DEVICES).                     **
C               **  TO ERASE THE SCREEN,                            **
C               **  USE THE !ERA COMMAND                            **
C               **  REFERENCE--4027 OPERATOR'S MANUAL, PAGE 5-6.    **
C               ******************************************************
C
 1200 CONTINUE
CCCCC WRITE(IGUNIT,1211)JCOL
C1211 FORMAT('!ERA G C',I1,';')
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1990
CCCCC ICSTR(1:8)='!ERA G C;'
      ICSTR(1:9)='!ERA G C;'
      IX=JCOL+48
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1990
CCCCC ICSTR(9:9)=CHAR(IX)
      ICSTR(10:10)=CHAR(IX)
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1990
CCCCC ICSTR(10:10)=';'
      ICSTR(11:11)=';'
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1990
CCCCC NCSTR=10
      NCSTR=11
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 13--                                       **
C               **  TREAT THE 4105 CASE                             **
C               **  (COLOR DEVICE)                                  **
C               **  TO ERASE THE SCREEN,                            **
C               **  SET THE BACKGROUND COLOR AND                    **
C               **  THEN CARRY OUT THE ERASE                        **
C               **  REFERENCE--PAGE 5-51                            **
C               ******************************************************
C
 1300 CONTINUE
      JCOL=1
CCCCC WRITE(IGUNIT,1311)IESCC,JCOL
C1311 FORMAT(A1,'RA1',I1,'0')
      ICSTR(1:1)=IESCC
      ICSTR(2:4)='RA1'
      IX=JCOL+48
      ICSTR(5:5)=CHAR(IX)
      ICSTR(6:6)='0'
      NCSTR=6
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
CCCCC WRITE(IGUNIT,1312)IESCC,IFFC
C1312 FORMAT(A1,A1)
      ICSTR(1:1)=IESCC
      ICSTR(2:2)=IFFC
      NCSTR=2
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 21--                                       **
C               **  TREAT THE HEWLETT-PACKARD 7221 CASE             **
C               **  (MULTI-COLOR PENPLOTTER)                        **
C               **  THERE IS NO    ERASE      INSTRUCTION PER SE.   **
C               **  REFERENCE--HP 7221A GRAPHICS PLOTTER            **
C               **             OPERATING AND PROGRAMMING MANUAL,    **
C               **             PAGE XX.                             **
C               ******************************************************
C
 2100 CONTINUE
      GOTO9000
C
C               ******************************************************
C               **  STEP 22--                                       **
C               **  TREAT THE HEWLETT-PACKARD HP-GL CASES           **
C               **  (ALL PLOTTERS THAT TAKE HP-GL INSTRUCTIONS)     **
C               **  (MULTI-COLOR PENPLOTTERS)                       **
C               **  THERE IS NO    ERASE      INSTRUCTION PER SE.   **
C               **  REFERENCE--HP 9872C GRAPHICS PLOTTER            **
C               **             OPERATING AND PROGRAMMING MANUAL,    **
C               **             PAGE XX, EDERAS.                        **
C               ******************************************************
C
 2200 CONTINUE
      GOTO9000
C
C               **********************************************************
C               **  STEP 23--                                           **
C               **  TREAT THE HEWLETT-PACKARD HP-2622 CASES             **
C               **  (ALL PLOTTERS THAT TAKE HP-2622-LIKE INSTRUCTIONS)  **
C               **  (MONOCHROME DISPLAY TERMINALS)                      **
C               **  REFERENCE--HP 2322C GRAPHICS PLOTTER                **
C               **             REFERENCE MANUAL,                        **
C               **             PAGE 10-4, 10-3.                         **
C               **********************************************************
C
 2300 CONTINUE
      ICSTR(1:1)=IESCC
      ICSTR(2:5)='*daZ'
      NCSTR=5
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 31--                                       **
C               **  TREAT THE GENERAL (DEVICE-INDEPENDENT) CASE     **
C               ******************************************************
C
 3100 CONTINUE
CCCCC WRITE(IGUNIT,3111)
C3111 FORMAT('ERASE SCREEN')
      ICSTR(1:12)='ERASE SCREEN'
      NCSTR=12
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ***************************************************************
C               **  STEP 32--                                                **
C               **  TREAT THE CODED GENERAL (DEVICE-INDEPENDENT) CASE        **
C               ***************************************************************
C
 3200 CONTINUE
      ICSTR(1:4)='ERSC'
      NCSTR=4
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 41--                                       **
C               **  TREAT THE IBM-PC CASE                           **
C               **  TO ERASE SCREEN---                              **
C               **  WRITE OUT AN ESCAPE [ 2 J                       **
C               **  REFERENCE--XX                                   **
C               **             XX                                   **
C               **             PAGES XX AND XX                      **
C               ******************************************************
C
 4100 CONTINUE
      ICSTR(1:1)=IESCC
      ICSTR(2:2)='['
      ICSTR(3:3)='2'
      ICSTR(4:4)='J'
      ICSTR(5:5)=IESCC
      ICSTR(6:6)='['
      ICSTR(7:7)='H'
      NCSTR=7
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 51--                                       **
C               **  TREAT THE HDS CASE                           **
C               **  TO ERASE SCREEN---                              **
C               **  WRITE OUT AN ESCAPE [ 2 J                       **
C               **  REFERENCE--XX                                   **
C               **             XX                                   **
C               **             PAGES XX AND XX                      **
C               ******************************************************
C
 5100 CONTINUE
      ICSTR(1:1)=IESCC
      ICSTR(2:2)='['
      ICSTR(3:3)='2'
      ICSTR(4:4)='J'
      ICSTR(5:5)=IESCC
      ICSTR(6:6)='['
      ICSTR(7:7)='H'
      NCSTR=7
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 61--                                       **
C               **  TREAT THE QUME CASE                           **
C               **  TO ERASE SCREEN---                              **
C               **  WRITE OUT AN ESCAPE [ 2 J                       **
C               **  REFERENCE--XX                                   **
C               **             XX                                   **
C               **             PAGES XX AND XX                      **
C               ******************************************************
C
 6100 CONTINUE
      ICSTR(1:1)=IESCC
      ICSTR(2:2)='['
      ICSTR(3:3)='2'
      ICSTR(4:4)='J'
      ICSTR(5:5)=IESCC
      ICSTR(6:6)='['
      ICSTR(7:7)='H'
      NCSTR=7
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 71--                                       **
C               **  TREAT THE VT CASE                           **
C               **  TO ERASE SCREEN---                              **
C               **  WRITE OUT AN ESCAPE [ 2 J                       **
C               **  REFERENCE--XX                                   **
C               **             XX                                   **
C               **             PAGES XX AND XX                      **
C               ******************************************************
C
 7100 CONTINUE
      ICSTR(1:1)=IESCC
      ICSTR(2:2)='['
      ICSTR(3:3)='2'
      ICSTR(4:4)='J'
      ICSTR(5:5)=IESCC
      ICSTR(6:6)='['
      ICSTR(7:7)='H'
      NCSTR=7
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 81--                                       **
C               **  TREAT THE MAC CASE                           **
C               **  TO ERASE SCREEN---                              **
C               **  WRITE OUT AN ESCAPE [ 2 J                       **
C               **  REFERENCE--XX                                   **
C               **             XX                                   **
C               **             PAGES XX AND XX                      **
C               ******************************************************
C
 8100 CONTINUE
      ICSTR(1:1)=IESCC
      ICSTR(2:2)='['
      ICSTR(3:3)='2'
      ICSTR(4:4)='J'
      ICSTR(5:5)=IESCC
      ICSTR(6:6)='['
      ICSTR(7:7)='H'
      NCSTR=7
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               ******************************************************
C               **  STEP 91--                                       **
C               **  TREAT THE FORTUNE CASE                           **
C               **  TO ERASE SCREEN---                              **
C               **  WRITE OUT AN ESCAPE [ 2 J                       **
C               **  REFERENCE--XX                                   **
C               **             XX                                   **
C               **             PAGES XX AND XX                      **
C               ******************************************************
C
 9100 CONTINUE
      ICSTR(1:1)=IESCC
      ICSTR(2:2)='['
      ICSTR(3:3)='2'
      ICSTR(4:4)='J'
      ICSTR(5:5)=IESCC
      ICSTR(6:6)='['
      ICSTR(7:7)='H'
      NCSTR=7
      CALL EDWRGS(ICSTR,NCSTR,ISUBN0)
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ERAS')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDERAS')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDERAS')
 9011 FORMAT('***** AT THE END       OF EDERAS--')
      WRITE(ICOUT,9015)IESCC,IFFC,ISYNC
      CALL EDWRST('EDERAS')
 9015 FORMAT('IESCC,IFFC,ISYNC = ',A1,2X,A1,2X,A1)
      WRITE(ICOUT,9018)IMANUF,IMODEL
      CALL EDWRST('EDERAS')
 9018 FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
      WRITE(ICOUT,9019)IX
      CALL EDWRST('EDERAS')
 9019 FORMAT('IX = ',I8)
      WRITE(ICOUT,9023)NCSTR
      CALL EDWRST('EDERAS')
 9023 FORMAT('NCSTR = ',I8)
      IF(NCSTR.LE.0)GOTO9027
      DO9025I=1,NCSTR
      IASCNE=ICHAR(ICSTR(I:I))
      WRITE(ICOUT,9026)I,ICSTR(I:I),IASCNE
      CALL EDWRST('EDERAS')
 9026 FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
 9025 CONTINUE
 9027 CONTINUE
      WRITE(ICOUT,9029)IBUGE2,ISUBRO,IERROR
      CALL EDWRST('EDERAS')
 9029 FORMAT('IBUGE2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
CCCCC THE FOLLOWING LINE WAS AUGMENTED JULY 1992
CCCCC SUBROUTINE EDEXIT
      SUBROUTINE EDEXIT(STOPSW)
C
C     PURPOSE--CARRY OUT A NORMAL EXIT FROM THE EDITOR.
C              BEFORE DOING SO, FIRST COPY THE CURRENT WORKSPACE
C              CONTENTS OUT TO THE ORIGINAL FILE.
C     NOTE--NO ARGUMENTS ARE EXPECTED.
C     SYNTAX--E
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED--JULY   1992  ADD STOP SWITCH (STOPSW)
C
C---------------------------------------------------------------------
C
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1992
      CHARACTER*4 STOPSW
C
      CHARACTER*4 ID
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDEX'
      ISUBN2='IT  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'EXIT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXIT')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDEXIT')
   51 FORMAT('*****AT THE BEGINNING OF EDEXIT--')
      WRITE(ICOUT,52)NUMLIN
      CALL EDWRST('EDEXIT')
   52 FORMAT('NUMLIN = ',I8)
      WRITE(ICOUT,53)IORINA
      CALL EDWRST('EDEXIT')
   53 FORMAT('IORINA = ',A80)
      WRITE(ICOUT,54)IORIST
      CALL EDWRST('EDEXIT')
   54 FORMAT('IORIST = ',A12)
      WRITE(ICOUT,55)IORINU
      CALL EDWRST('EDEXIT')
   55 FORMAT('IORINU = ',I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ***************************************
C               **  STEP 1--                         **
C               **  WRITE WORKSPACE OUT TO THE FILE  **
C               ***************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ID='ORIG'
      IF(IORINA.NE.ICOMNA.AND.IORINA.NE.ISAVNA)
     1CALL EDWRWF(ID)
C
C               ******************************
C               **  STEP 2--                **
C               **  WRITE OUT A MESSAGE     **
C               ******************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,1205)
      CALL EDWRST('EDEXIT')
 1205 FORMAT('THIS IS A NORMAL EDITOR EXIT')
      WRITE(ICOUT,1206)IORINA
      CALL EDWRST('EDEXIT')
 1206 FORMAT('FILE              = ',A80)
      WRITE(ICOUT,1207)NUMLIN
      CALL EDWRST('EDEXIT')
 1207 FORMAT('NUMBER OF LINES   = ',I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXIT')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'EXIT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXIT')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDEXIT')
 9011 FORMAT('*****AT THE END       OF EDEXIT--')
      WRITE(ICOUT,9012)NUMLIN
      CALL EDWRST('EDEXIT')
 9012 FORMAT('NUMLIN = ',I8)
      WRITE(ICOUT,9013)IORINA
      CALL EDWRST('EDEXIT')
 9013 FORMAT('IORINA = ',A80)
      WRITE(ICOUT,9014)IORIST
      CALL EDWRST('EDEXIT')
 9014 FORMAT('IORIST = ',A12)
      WRITE(ICOUT,9015)IORINU
      CALL EDWRST('EDEXIT')
 9015 FORMAT('IORINU = ',I8)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      IF(ICOM.EQ.'ER'.OR.ICOM.EQ.'ERR'.OR.ICOM.EQ.'EXRR')RETURN
      IF(ICOM.NE.'ER'.AND.ICOM.NE.'ERR'.AND.ICOM.NE.'EXRR'
     1.AND.IHOST1.EQ.'VAX ')CALL EXIT(1)
CCCCC THE FOLLOWING LINE WAS CHANGED JULY 1992
CCCCC STOP
      STOPSW='YES'
      END
      SUBROUTINE EDEXST(ISTRIN,NUMCST,IFOUST)
C
C     PURPOSE--SCAN IANS(.) AND EXTRACT THE
C              STRING AFTER THE COMMAND.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUST
      CHARACTER*4 ICASE
      CHARACTER*5 IC5
C
      DIMENSION ISTRIN(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDEX'
      ISUBN2='ST  '
C
      IF(IBUGE3.EQ.'OFF'.AND.ISUBRO.NE.'EXST')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXST')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDEXST')
   51 FORMAT('***** AT THE BEGINNING OF EDEXST--')
      WRITE(ICOUT,52)IBUGE3,ISUBRO
      CALL EDWRST('EDEXST')
   52 FORMAT('IBUGE3,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,54)NUMARG,IWIDTH
      CALL EDWRST('EDEXST')
   54 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,55)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDEXST')
      ENDIF
   55 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXST')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **************************
C               **  STEP 1--            **
C               **  EXTRACT THE STRING  **
C               **************************
C
CCCCC ISTRIN(1)=' '
CCCCC NUMCST=1
      NUMCST=0
      IFOUST='NO'
C
      IF(NUMARG.LE.0)GOTO1190
      IF(IWIDTH.LE.1)GOTO1190
C
      DO1120I=1,IWIDTH
      I2=I
      IF(IANS(I).EQ.' ')GOTO1125
 1120 CONTINUE
      I2=IWIDTH+1
 1125 CONTINUE
      IFIRBL=I2
C
      IMIN=IFIRBL+1
      IF(IMIN.GT.IWIDTH)GOTO1190
C
      ICASE='PRIN'
      DO1130I=IMIN,IWIDTH
      IF(IANS(I).EQ.'('.AND.IANS(I+1).EQ.')')ICASE='NONP'
 1130 CONTINUE
C
      IF(ICASE.EQ.'PRIN')GOTO1140
      GOTO1150
C
 1140 CONTINUE
      J=0
      DO1141I=IMIN,IWIDTH
      J=J+1
      ISTRIN(J)=IANS(I)
 1141 CONTINUE
      NUMCST=J
      IFOUST='YES'
      GOTO1190
C
 1150 CONTINUE
      J=0
      I=IMIN
 1152 CONTINUE
      IF(I.GT.IWIDTH)GOTO1159
C
      IVALUE=-1
C
      IC5='     '
      IC5(1:1)=IANS(I)
      IF(I+1.GT.IWIDTH)GOTO1155
      IC5(2:2)=IANS(I+1)
      IF(I+2.GT.IWIDTH)GOTO1155
      IC5(3:3)=IANS(I+2)
      IF(I+3.GT.IWIDTH)GOTO1155
      IC5(4:4)=IANS(I+3)
      IF(I+4.GT.IWIDTH)GOTO1151
      IC5(5:5)=IANS(I+4)
 1151 CONTINUE
C
      IF(IC5.EQ.'NUL()')IVALUE=0
      IF(IC5.EQ.'nul()')IVALUE=0
      IF(IC5.EQ.'SOH()')IVALUE=1
      IF(IC5.EQ.'soh()')IVALUE=1
      IF(IC5.EQ.'STX()')IVALUE=2
      IF(IC5.EQ.'stx()')IVALUE=2
      IF(IC5.EQ.'ETX()')IVALUE=3
      IF(IC5.EQ.'etx()')IVALUE=3
      IF(IC5.EQ.'EOT()')IVALUE=4
      IF(IC5.EQ.'eot()')IVALUE=4
      IF(IC5.EQ.'ENQ()')IVALUE=5
      IF(IC5.EQ.'enq()')IVALUE=5
      IF(IC5.EQ.'ACK()')IVALUE=6
      IF(IC5.EQ.'ack()')IVALUE=6
      IF(IC5.EQ.'BEL()')IVALUE=7
      IF(IC5.EQ.'bel()')IVALUE=7
      IF(IC5.EQ.'BS()')IVALUE=8
      IF(IC5.EQ.'bs()')IVALUE=8
      IF(IC5.EQ.'HT()')IVALUE=9
      IF(IC5.EQ.'ht()')IVALUE=9
      IF(IC5.EQ.'LF()')IVALUE=10
      IF(IC5.EQ.'lf()')IVALUE=10
      IF(IC5.EQ.'VT()')IVALUE=11
      IF(IC5.EQ.'vt()')IVALUE=11
      IF(IC5.EQ.'FF()')IVALUE=12
      IF(IC5.EQ.'ff()')IVALUE=12
      IF(IC5.EQ.'CR()')IVALUE=130
      IF(IC5.EQ.'cr()')IVALUE=130
      IF(IC5.EQ.'SO()')IVALUE=14
      IF(IC5.EQ.'so()')IVALUE=14
      IF(IC5.EQ.'SI()')IVALUE=15
      IF(IC5.EQ.'si()')IVALUE=15
      IF(IC5.EQ.'DLE()')IVALUE=16
      IF(IC5.EQ.'dle()')IVALUE=16
      IF(IC5.EQ.'DC1()')IVALUE=17
      IF(IC5.EQ.'dc1()')IVALUE=17
      IF(IC5.EQ.'DC2()')IVALUE=18
      IF(IC5.EQ.'dc2()')IVALUE=18
      IF(IC5.EQ.'DC3()')IVALUE=19
      IF(IC5.EQ.'dc3()')IVALUE=19
      IF(IC5.EQ.'DC4()')IVALUE=20
      IF(IC5.EQ.'dc4()')IVALUE=20
      IF(IC5.EQ.'NAK()')IVALUE=21
      IF(IC5.EQ.'nak()')IVALUE=21
      IF(IC5.EQ.'SYN()')IVALUE=22
      IF(IC5.EQ.'syn()')IVALUE=22
      IF(IC5.EQ.'ETB()')IVALUE=23
      IF(IC5.EQ.'etb()')IVALUE=23
      IF(IC5.EQ.'CAN()')IVALUE=24
      IF(IC5.EQ.'can()')IVALUE=24
      IF(IC5.EQ.'EM()')IVALUE=25
      IF(IC5.EQ.'em()')IVALUE=25
      IF(IC5.EQ.'SUB()')IVALUE=26
      IF(IC5.EQ.'sub()')IVALUE=26
      IF(IC5.EQ.'ESC()')IVALUE=27
      IF(IC5.EQ.'esc()')IVALUE=27
      IF(IC5.EQ.'FS()')IVALUE=28
      IF(IC5.EQ.'fs()')IVALUE=28
      IF(IC5.EQ.'GS()')IVALUE=29
      IF(IC5.EQ.'gs()')IVALUE=29
      IF(IC5.EQ.'RS()')IVALUE=30
      IF(IC5.EQ.'rs()')IVALUE=30
      IF(IC5.EQ.'US()')IVALUE=31
      IF(IC5.EQ.'us()')IVALUE=31
 1155 CONTINUE
C
      IF(IVALUE.GE.0.AND.IVALUE.LE.31) THEN
         J=J+1
         ISTRIN(J)=CHAR(IVALUE)
         IW=1
         IF(0.LE.IVALUE.AND.IVALUE.LE.7)IW=5
         IF(8.LE.IVALUE.AND.IVALUE.LE.15)IW=4
         IF(16.LE.IVALUE.AND.IVALUE.LE.24)IW=5
         IF(IVALUE.EQ.25)IW=4
         IF(IVALUE.EQ.26)IW=5
         IF(IVALUE.EQ.27)IW=5
         IF(IVALUE.EQ.28)IW=4
         IF(IVALUE.EQ.29)IW=4
         IF(IVALUE.EQ.30)IW=4
         IF(IVALUE.EQ.31)IW=4
         I=I+IW
         GOTO1152
      ENDIF
C
      IF(IVALUE.LE.-1) THEN
         J=J+1
         ISTRIN(J)=IANS(I)
         I=I+1
         GOTO1152
      ENDIF
C
 1159 CONTINUE
      NUMCST=J
      IFOUST='YES'
      GOTO1190
C
 1190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE3.EQ.'OFF'.AND.ISUBRO.NE.'EXST')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXST')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDEXST')
 9011 FORMAT('***** AT THE END       OF EDEXST--')
      WRITE(ICOUT,9012)IBUGE3,ISUBRO
      CALL EDWRST('EDEXST')
 9012 FORMAT('IBUGE3,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,9014)NUMCST,IFOUST
      CALL EDWRST('EDEXST')
 9014 FORMAT('NUMCST,IFOUST = ',I8,2X,A4)
C
      IF(NUMCST.LE.0)GOTO9019
      DO9016I=1,NUMCST
      WRITE(ICOUT,9017)I,ISTRIN(I)
      CALL EDWRST('EDEXST')
 9017 FORMAT('I,ISTRIN(I) = ',I8,A4)
 9016 CONTINUE
 9019 CONTINUE
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXST')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDEXTE
C
C     PURPOSE--EXTEND CURRENT LINE BY     STRING    .
C              THE STRING WHICH FOLLOWS THE EXTEND COMMAND
C              WILL BE APPENDED.
C     COMMAND SYNTAX--EXTEND <STRING>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUST
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDEX'
      ISUBN2='TE  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IFIRBL=(-999)
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'EXTE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXTE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDEXTE')
   51 FORMAT('***** AT THE BEGINNING OF EDEXTE--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE STRING TO BE EXTENDED  **
C               *******************************************
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUST)
      IF(IFOUST.EQ.'YES')GOTO1190
      NUMCST=0
 1190 CONTINUE
C
C               ****************************
C               **  STEP 3--              **
C               **  EXTEND     THE LINE.  **
C               ****************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'EXTE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMLIN.GE.MAXEDL)GOTO1510
      IF(NUMROW.GE.MAXROW)GOTO1520
      IF(NUMCHA.GE.MAXEDC)GOTO1530
CCCCC THE FOLLOWING LINE WAS ADDED MAY 1990
      IF(ICURLN.GT.NUMLIN)GOTO1610
      ICURRO=IPOINT(ICURLN)
CCCCC THE FOLLOWING LINE WAS ADDED MAY 1990
      IF(ICURRO.LE.0)GOTO1610
      NUMCH2=NUMCHA+NUMCPL(ICURRO)+NUMCST
      IF(NUMCH2.GT.MAXEDC)GOTO1540
      GOTO1550
C
 1510 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXTE')
      WRITE(ICOUT,1511)
      CALL EDWRST('EDEXTE')
 1511 FORMAT('***** ERROR IN EDEXTE--')
      WRITE(ICOUT,1512)
      CALL EDWRST('EDEXTE')
 1512 FORMAT('      NO ROOM LEFT FOR MORE LINES.')
      WRITE(ICOUT,1513)NUMLIN,MAXEDL
      CALL EDWRST('EDEXTE')
 1513 FORMAT('      NUMLIN (',I8,') >= MAXEDL (',I8,')')
      IERROR='YES'
      GOTO9000
C
 1520 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXTE')
      WRITE(ICOUT,1521)
      CALL EDWRST('EDEXTE')
 1521 FORMAT('***** ERROR IN EDEXTE--')
      WRITE(ICOUT,1522)
      CALL EDWRST('EDEXTE')
 1522 FORMAT('      NO ROOM LEFT FOR MORE ROWS.')
      WRITE(ICOUT,1523)NUMROW,MAXROW
      CALL EDWRST('EDEXTE')
 1523 FORMAT('      NUMROW (',I8,') >= MAXROW (',I8,')')
      IERROR='YES'
      GOTO9000
C
 1530 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXTE')
      WRITE(ICOUT,1531)
      CALL EDWRST('EDEXTE')
 1531 FORMAT('***** ERROR IN EDEXTE--')
      WRITE(ICOUT,1532)
      CALL EDWRST('EDEXTE')
 1532 FORMAT('      NO ROOM LEFT FOR MORE CHARACTERS.')
      WRITE(ICOUT,1533)NUMCHA,MAXEDC
      CALL EDWRST('EDEXTE')
 1533 FORMAT('      NUMCHA (',I8,') >= MAXEDC (',I8,')')
      IERROR='YES'
      GOTO9000
C
 1540 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXTE')
      WRITE(ICOUT,1541)
      CALL EDWRST('EDEXTE')
 1541 FORMAT('***** ERROR IN EDEXTE--')
      WRITE(ICOUT,1542)
      CALL EDWRST('EDEXTE')
 1542 FORMAT('      NO ROOM LEFT FOR MORE ROWS.')
      WRITE(ICOUT,1543)NUMROW,MAXROW
      CALL EDWRST('EDEXTE')
 1543 FORMAT('      NUMROW (',I8,') >= MAXROW (',I8,')')
      IERROR='YES'
      GOTO9000
C
CCCCC THE FOLLOWING SECTION WAS ADDED MAY 1990
 1610 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXTE')
      WRITE(ICOUT,1611)
      CALL EDWRST('EDEXTE')
 1611 FORMAT('***** ERROR IN EDEXTE--')
      WRITE(ICOUT,1612)
      CALL EDWRST('EDEXTE')
 1612 FORMAT('      [BOTTOM]')
      IERROR='YES'
      GOTO9000
C
 1550 CONTINUE
      IF(ICURLN.GT.NUMLIN)ICURLN=NUMLIN
      NUMRP1=NUMROW+1
C
      J1=NUMCHA+1
      N1=NUMCST
      J2=J1+(N1-1)
C
      IROW=NUMRP1
      ILOCC1(IROW)=J1
C
      IROW0=IPOINT(ICURLN)
      J10=ILOCC1(IROW0)
      N10=NUMCPL(IROW0)
      J20=J10+(N10-1)
C
      J=J1-1
      IF(J10.GT.J20)GOTO1572
      DO1571K=J10,J20
      J=J+1
      ICHA(J)=ICHA(K)
 1571 CONTINUE
 1572 CONTINUE
C
      IF(NUMCST.LE.0)GOTO1574
      DO1573K=1,NUMCST
      J=J+1
      ICHA(J)=ISTRIN(K)
 1573 CONTINUE
 1574 CONTINUE
C
      IPOINT(ICURLN)=IROW
      NUMCPL(IROW)=(J20-J10+1)+NUMCST
C
 1579 CONTINUE
      NUMROW=NUMRP1
      NUMCHA=J
C
C               **************************
C               **  STEP 4--            **
C               **  PRINT OUT THE LINE  **
C               **************************
C
      ISTEPN='4'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'EXTE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ICURLN
      J3=ILOCC1(IROW)
      N3=NUMCPL(IROW)
      J4=J3+(N3-1)
      IF(IFEESW.EQ.'OFF')GOTO1629
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1621)(ICHA(J),J=J3,J4)
         CALL EDWRST('EDEXTE')
      ENDIF
 1621 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1622)ILINE,(ICHA(J),J=J3,J4)
         CALL EDWRST('EDEXTE')
      ENDIF
C1622 FORMAT(I6,':',3X,230A1)
 1622 FORMAT(I6,':',3X,238A1)
 1629 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'EXTE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXTE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDEXTE')
 9011 FORMAT('***** AT THE END       OF EDEXTE--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXTE')
      WRITE(ICOUT,9012)IWIDTH
      CALL EDWRST('EDEXTE')
 9012 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9013)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDEXTE')
 9013 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9014)NUMCST
      CALL EDWRST('EDEXTE')
 9014 FORMAT('NUMCST = ',I8)
      WRITE(ICOUT,9015)(ISTRIN(I),I=1,NUMCST)
      CALL EDWRST('EDEXTE')
 9015 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,9016)IFIRBL
      CALL EDWRST('EDEXTE')
 9016 FORMAT('IFIRBL = ',I8)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDEXTE')
 9017 FORMAT('J1,N1,J2 = ',3I8)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDEXWO(ICLINE,NCLINE,IC1,IC2,IC3,IC4,
     1ICWORD,NCWORD,IFOUWO)
C
C     PURPOSE--SCAN ICLINE AND EXTRACT THE
C              FIRST NON-BLANK WORD (= STRING)
C              BETWEEN COLUMNS IC1 AND IC2.
C              PLACE THE COLUMN NUMBER OF THE
C              ACTUAL FIRST NON-BLANK CHARACTER INTO IC3.
C              PLACE THE COLUMN NUMBER OF THE
C              ACTUAL LAST NON-BLANK CHARACTER INTO IC4.
C              PLACE THE FOUND WORD INTO ICWORD.
C              PLACE THE NUMBER OF CHARACTERS INTO NCWORD.
C              IF A NON-BLANK WORD IS FOUND, SET IFOUWO TO    YES;
C              IF NOT FOUND (THAT IS, HAVE ALL BLANKS), SET IFOUWO TO   NO.
C     NOTE--ICLINE AND ICWORD ARE CHARACTER*240
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.6
C     ORIGINAL VERSION--MAY 1986.
C     UPDATED--OCTOBER 1991.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ICLINE
      CHARACTER*240 ICWORD
      CHARACTER*4 IFOUWO
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDEX'
      ISUBN2='WO  '
C
      IF(IBUGE3.EQ.'OFF'.AND.ISUBRO.NE.'EXWO')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXWO')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDEXWO')
   51 FORMAT('***** AT THE BEGINNING OF EDEXWO--')
      WRITE(ICOUT,54)NCLINE
      CALL EDWRST('EDEXWO')
   54 FORMAT('NCLINE = ',I8)
CCCCC IF(NCLINE.GE.1)THEN
CCCCC    WRITE(ICOUT,55)(ICLINE(I:I),I=1,100) non-fortran 77 on unix
CCC55 FORMAT('(ICLINE(I:I),I=1,NCLINE) = ',100A1)
      WRITE(ICOUT,61)IC1,IC2,IC3,IC4
      CALL EDWRST('EDEXWO')
   61 FORMAT('IC1,IC23,IC3,IC4 = ',4I8)
      WRITE(ICOUT,71)NCWORD
      CALL EDWRST('EDEXWO')
   71 FORMAT('NCWORD = ',I8)
      IF(NCWORD.GE.1)THEN
         WRITE(ICOUT,72)(ICWORD(I:I),I=1,100)
         CALL EDWRST('EDEXWO')
      ENDIF
   72 FORMAT('(ICWORD(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXWO')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **********************************
C               **  STEP 11--                   **
C               **  FIND THE FIRST NON-BLANK    **
C               **********************************
C
      DO1100I=IC1,IC2
      I2=I
      IF(ICLINE(I:I).NE.' ')GOTO1180
 1100 CONTINUE
      IC3=IC1
      IC4=IC2
      NCWORD=0
      IFOUWO='NO'
      GOTO9000
C
 1180 CONTINUE
      IC3=I2
      IFOUWO='YES'
      GOTO1190
C
 1190 CONTINUE
C
C               *********************************************
C               **  STEP 12--                              **
C               **  FIND THE LAST CONTIGUOUS NON-BLANK     **
C               *********************************************
C
      DO1200I=IC3,IC2
      I2=I
      IF(ICLINE(I:I).EQ.' ')GOTO1280
 1200 CONTINUE
      IC4=IC2
      GOTO1290
 1280 CONTINUE
      IC4=I2-1
      GOTO1290
C
 1290 CONTINUE
C
C               *********************************************
C               **  STEP 13--                              **
C               **  COPY OVER THE WORD.                    **
C               *********************************************
C
      J=0
      DO1300I=IC3,IC4
      J=J+1
      ICWORD(J:J)=ICLINE(I:I)
 1300 CONTINUE
      NCWORD=J
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE3.EQ.'OFF'.AND.ISUBRO.NE.'EXWO')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXWO')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDEXWO')
 9011 FORMAT('***** AT THE END       OF EDEXWO--')
      WRITE(ICOUT,9014)NCLINE
      CALL EDWRST('EDEXWO')
 9014 FORMAT('NCLINE = ',I8)
CCCCC IF(NCLINE.GE.1)THEN
CCCCC    WRITE(ICOUT,9015)(ICLINE(I:I),I=1,100) non-fortran 77 on unix
C9015 FORMAT('(ICLINE(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,9021)IC1,IC2,IC3,IC4
      CALL EDWRST('EDEXWO')
 9021 FORMAT('IC1,IC23,IC3,IC4 = ',4I8)
      WRITE(ICOUT,9031)NCWORD
      CALL EDWRST('EDEXWO')
 9031 FORMAT('NCWORD = ',I8)
      IF(NCWORD.GE.1)THEN
         WRITE(ICOUT,9032)(ICWORD(I:I),I=1,100)
         CALL EDWRST('EDEXWO')
      ENDIF
 9032 FORMAT('(ICWORD(I:I),I=1,NCWORD) = ',100A1)
      WRITE(ICOUT,9041)IFOUWO
      CALL EDWRST('EDEXWO')
 9041 FORMAT('IFOUWO = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDEXWO')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDFIN2(ISTRIN,NUMCST,J1,J2,IHIT)
C
C     PURPOSE--SCAN ICHA(.) BETWEEN
C              ELEMENTS J1 AND J2 (INCLUSIVE)
C              AND DETERMINE IF THE STRING IN ISTRIN(.)
C              IS CONTAINED THERIN IN COLUMNS 1, 2, 3, ...
C              IF SO, THEN SET IHIT TO 'YES'
C              IF NO, THEN SET IHIT TO 'NO'
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IHIT
C
      DIMENSION ISTRIN(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDFI'
      ISUBN2='N2  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'FIN2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIN2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDFIN2')
   51 FORMAT('***** AT THE BEGINNING OF EDFIN2--')
      WRITE(ICOUT,54)NUMCST
      CALL EDWRST('EDFIN2')
   54 FORMAT('NUMCST = ',I8)
      IF(NUMCST.GE.1)THEN
         WRITE(ICOUT,55)(ISTRIN(I),I=1,NUMCST)
         CALL EDWRST('EDFIN2')
      ENDIF
   55 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,56)J1,J2
      CALL EDWRST('EDFIN2')
   56 FORMAT('J1,J2 = ',2I8)
      WRITE(ICOUT,57)IHIT
      CALL EDWRST('EDFIN2')
   57 FORMAT('IHIT = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIN2')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IHIT='NO'
C
      IF(J1.GT.J2)GOTO9000
      IF(NUMCST.LE.0)IHIT='YES'
      IF(NUMCST.LE.0)GOTO9000
C
      J=J1-1
      DO1100I=1,NUMCST
      J=J+1
      IF(ISTRIN(I).EQ.IMASK)GOTO1100
      IF(ICHA(J).EQ.ISTRIN(I))GOTO1100
      GOTO1190
 1100 CONTINUE
      IHIT='YES'
      GOTO9000
 1190 CONTINUE
      IHIT='NO'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'FIN2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIN2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDFIN2')
 9011 FORMAT('***** AT THE END       OF EDFIN2--')
      WRITE(ICOUT,9014)NUMCST
      CALL EDWRST('EDFIN2')
 9014 FORMAT('NUMCST = ',I8)
      IF(NUMCST.GE.1)THEN
         WRITE(ICOUT,9015)(ISTRIN(I),I=1,NUMCST)
         CALL EDWRST('EDFIN2')
      ENDIF
 9015 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,9016)J1,J2
      CALL EDWRST('EDFIN2')
 9016 FORMAT('J1,J2 = ',2I8)
      WRITE(ICOUT,9017)IHIT
      CALL EDWRST('EDFIN2')
 9017 FORMAT('IHIT = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIN2')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDFIND
C
C     PURPOSE--FIND THE NEXT OCCURRANCE OF A STRING
C              (START THE SEARCH WITH THE NEXT LINE)
C     NOTE--
C          LOCATE ABC WILL LOCATE ABC IF IN ANY COLUMN
C          FIND ABC WILL LOCATE ABC ONLY IF IN COLUMNS 1, 2, AND 3
C     COMMAND SYNTAX--FIND <STRING>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUST
      CHARACTER*4 IHIT
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDFI'
      ISUBN2='ND  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IHIT='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'FIND')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIND')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDFIND')
   51 FORMAT('***** AT THE BEGINNING OF EDFIND--')
      WRITE(ICOUT,52)IALL
      CALL EDWRST('EDFIND')
   52 FORMAT('IALL = ',A4)
      WRITE(ICOUT,53)IWIDTH
      CALL EDWRST('EDFIND')
   53 FORMAT('IWIDTH = ',I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE STRING TO BE FOUND     **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'FIND')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUST)
      IF(IFOUST.EQ.'YES')GOTO1190
C
      NUMCST=NUMCFS
      IF(NUMCFS.LE.0)GOTO1159
      DO1155I=1,NUMCFS
      ISTRIN(I)=IFINST(I)
 1155 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1190 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  IF THE STRING IS NEW,           **
C               **  COPY THE STRING INTO IFINST(.)  **
C               **  FOR FUTURE USE BY SUBSEQUENT    **
C               **  FIND   COMMANDS.                **
C               **************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'FIND')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFOUST.EQ.'NO')GOTO1290
      NUMCFS=NUMCST
      IF(NUMCST.LE.0)GOTO1290
      DO1210I=1,NUMCST
      IFINST(I)=ISTRIN(I)
 1210 CONTINUE
 1290 CONTINUE
C
C               ************************************
C               **  STEP 3--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE SCANNED.   **
C               ************************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'FIND')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLP=ICURLN+1
C
      ISTART=ICURLP
      ISTOP=NUMLIN
C
C               ****************************
C               **  STEP 4--              **
C               **  SCAN      THE LINES.  **
C               ****************************
C
      ISTEPN='4'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'FIND')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTART.GT.ISTOP)GOTO1550
C
      DO1510ILINE=ISTART,ISTOP
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDFIN2(ISTRIN,NUMCST,J1,J2,IHIT)
      IF(IHIT.EQ.'YES')GOTO1520
      GOTO1510
C
 1520 CONTINUE
      ICURLN=ILINE
      IF(IFEESW.EQ.'OFF')GOTO1529
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDFIND')
      ENDIF
 1521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1522)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDFIND')
      ENDIF
 1522 FORMAT(I6,':',3X,230A1)
 1529 CONTINUE
      IF(IALL.EQ.'ON')GOTO1510
      GOTO1590
C
 1510 CONTINUE
C
 1550 CONTINUE
      ICURLN=NUMLIN+1
      IF(IFEESW.EQ.'OFF')GOTO1559
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1551)
         CALL EDWRST('EDFIND')
      ENDIF
 1551 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1552)
         CALL EDWRST('EDFIND')
      ENDIF
 1552 FORMAT(10X,'[BOTTOM]')
 1559 CONTINUE
C
 1590 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'FIND')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIND')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDFIND')
 9011 FORMAT('***** AT THE END       OF EDFIND--')
      WRITE(ICOUT,9016)IWIDTH
      CALL EDWRST('EDFIND')
 9016 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDFIND')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)J1,J2,IHIT,ICURLN
      CALL EDWRST('EDFIND')
 9018 FORMAT('J1,J2,IHIT,ICURLN = ',4I8)
      WRITE(ICOUT,9019)IALL,IFOUST
      CALL EDWRST('EDFIND')
 9019 FORMAT('IALL,IFOUST = ',A4,2X,A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIND')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDFIRS
C
C     PURPOSE--GO TO FIRST LINE OF FILE
C              (= LINE 1)
C
C     NOTE--NO ARGUMENTS ARE EXPECTED OR PROCESSED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDFI'
      ISUBN2='ST  '
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'FIRS')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIRS')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDFIRS')
   51 FORMAT('***** AT THE BEGINNING OF EDFIRS--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************
C               **  STEP 1--           **
C               **  GO TO LINE 1       **
C               **  AND PRINT IT OUT.  **
C               *************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'FIRS')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=1
      IF(ILINE.GT.NUMLIN)GOTO1150
C
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(IFEESW.EQ.'OFF')GOTO1129
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1121)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDFIRS')
      ENDIF
 1121 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1122)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDFIRS')
      ENDIF
 1122 FORMAT(I6,':',3X,230A1)
 1129 CONTINUE
      GOTO1190
C
 1150 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1159
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1151)
         CALL EDWRST('EDFIRS')
      ENDIF
 1151 FORMAT('THE FILE IS EMPTY--THERE IS NO LINE 1.')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1152)
         CALL EDWRST('EDFIRS')
      ENDIF
 1152 FORMAT(10X,'THE FILE IS EMPTY--THERE IS NO LINE 1.')
 1159 CONTINUE
      GOTO1190
C
 1190 CONTINUE
C
      ICURLN=ILINE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'FIRS')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDFIRS')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDFIRS')
 9011 FORMAT('***** AT THE END       OF EDFIRS--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDGETF(IFORIG)
C
C     PURPOSE--STRIP OFF THE FILE NAME
C              FROM THE RUN LINE ON A VAX
C              THEREFORE, CAN SAY    FED XXX
C              RATHER THAN           FED
C                                    XXX
C     NOTE--THIS SUBROUTINE IS VAX-DEPENDENT.
C     DATE--OCTOBER 15, 1985
C
C------------------------------------------------------------------------------
 
      CHARACTER*80 IFORIG
 
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
CCCCC IF(IHOST1.EQ.'VAX ')IJUNK=CLI$GET_VALUE('PARAM1',IFORIG)
CCCCC IF(IHOST1.EQ.'UNIX ')IJUNK=GETARG(etc.)
C
      RETURN
      END
      SUBROUTINE EDGO(IMARK)
C
C     PURPOSE--GO TO LINE K
C
C     NOTE--IF NO ARGUMENTS, THEN GO TO CURRENT LINE
C           IF 1 ARGUMENT, THEN GO TO LINE K.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDGO'
      ISUBN2='    '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'GO')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDGO  ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDGO  ')
   51 FORMAT('***** AT THE BEGINNING OF EDGO--')
      WRITE(ICOUT,52)IMARK
      CALL EDWRST('EDGO  ')
   52 FORMAT('IMARK = ',I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************
C               **  STEP 1--           **
C               **  GO TO LINE K       **
C               **  AND PRINT IT OUT.  **
C               *************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'GO')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      K=IMARK
      IF(NUMARG.GE.1.AND.IARGT(1).EQ.'NUMB')K=IARG(1)
C
      ILINE=K
C
      IF(ILINE.LT.1)GOTO1110
      IF(ILINE.GT.NUMLIN)GOTO1120
      GOTO1130
C
 1110 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1119
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1111)
         CALL EDWRST('EDGO  ')
      ENDIF
 1111 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1112)
         CALL EDWRST('EDGO  ')
      ENDIF
 1112 FORMAT(10X,'[TOP]')
 1119 CONTINUE
      ILINE=0
      GOTO1190
C
 1120 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1129
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1121)
         CALL EDWRST('EDGO  ')
      ENDIF
 1121 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1122)
         CALL EDWRST('EDGO  ')
      ENDIF
 1122 FORMAT(10X,'[BOTTOM]')
 1129 CONTINUE
      ILINE=NUMLIN+1
      GOTO1190
C
 1130 CONTINUE
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(IFEESW.EQ.'OFF')GOTO1139
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1131)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDGO  ')
      ENDIF
 1131 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1132)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDGO  ')
      ENDIF
 1132 FORMAT(I6,':',3X,230A1)
 1139 CONTINUE
      GOTO1190
C
 1190 CONTINUE
      ICURLN=ILINE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'GO')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDGO  ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDGO  ')
 9011 FORMAT('***** AT THE END       OF EDGO--')
      WRITE(ICOUT,9012)IMARK
      CALL EDWRST('EDGO  ')
 9012 FORMAT('IMARK = ',I8)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDGONU
C
C     PURPOSE--THE ANALYST HAS ENTERED A LINE NUMBER--
C              GO TO THAT LINE NUMBER.
C
C     NOTE--NO ARGUMENTS ARE EXPECTED
C           (THE COMMAND IS THE LINE NUMBER)
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDGO'
      ISUBN2='NU  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'GONU')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDGONU')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDGONU')
   51 FORMAT('***** AT THE BEGINNING OF EDGONU--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************
C               **  STEP 1--           **
C               **  GO TO LINE K       **
C               **  AND PRINT IT OUT.  **
C               *************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'GONU')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ICOMI
C
      IF(ILINE.LT.1)GOTO1110
      IF(ILINE.GT.NUMLIN)GOTO1120
      GOTO1130
C
 1110 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1119
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1111)
         CALL EDWRST('EDGONU')
      ENDIF
 1111 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1112)
         CALL EDWRST('EDGONU')
      ENDIF
 1112 FORMAT(10X,'[TOP]')
 1119 CONTINUE
      ILINE=0
      GOTO1190
C
 1120 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1129
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1121)
         CALL EDWRST('EDGONU')
      ENDIF
 1121 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1122)
         CALL EDWRST('EDGONU')
      ENDIF
 1122 FORMAT(10X,'[BOTTOM]')
 1129 CONTINUE
      ILINE=NUMLIN+1
      GOTO1190
C
 1130 CONTINUE
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(IFEESW.EQ.'OFF')GOTO1139
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1131)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDGONU')
      ENDIF
 1131 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1132)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDGONU')
      ENDIF
 1132 FORMAT(I6,':',3X,230A1)
 1139 CONTINUE
      GOTO1190
C
 1190 CONTINUE
      ICURLN=ILINE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'GONU')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDGONU')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDGONU')
 9011 FORMAT('***** AT THE END       OF EDGONU--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDHELP
C
C     PURPOSE--READ THE HELP FILE AND WRITE IT OUT
C              (1 LINE AT A TIME) TO THE SCREEN.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY  1985.
C     UPDATED         --JULY     1992.  PRINT ONE PAGE AT A TIME
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*4 ITABID
      CHARACTER*4 IWORD1
      CHARACTER*4 IWORD2
      CHARACTER*4 IWORD3
      CHARACTER*4 IWORD4
      CHARACTER*4 IWOR12
C
      CHARACTER*1 ICHAR1
C
      CHARACTER*4 ICTEST
C
      CHARACTER*4 IW1
      CHARACTER*4 IW2
      CHARACTER*4 IW3
      CHARACTER*4 IW4
      CHARACTER*4 IW5
C
      CHARACTER*4 IZ1
      CHARACTER*4 IZ2
      CHARACTER*4 IZ3
      CHARACTER*4 IZ4
C
      CHARACTER*4 ICTEXT
C
      CHARACTER*20 ISTRIN
C
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1992
      CHARACTER*1 IYESNO
C
      DIMENSION ITABID(100)
      DIMENSION ITABLN(100)
C
      DIMENSION ICTEXT(20)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDHE'
      ISUBN2='LP  '
C
      NUMLF=(-999)
      NUMSEC=(-999)
      ISECNA=(-999)
C
      NUMAR2=(-999)
C
      IWORD1='    '
      IWORD2='    '
      IWORD3='    '
      IWORD4='    '
      IWOR12='    '
C
      ICTEST='    '
C
      IW1='    '
      IW2='    '
      IW3='    '
      IW4='    '
      IW5='    '
C
      IZ1='    '
      IZ2='    '
      IZ3='    '
      IZ4='    '
C
      JCHAR1=(-999)
      JSEC=(-999)
      JSECP1=(-999)
C
      ISKIP=(-999)
      ISTART=(-999)
      ISTOP=(-999)
C
      ISTRIN='                    '
C
      NUMWOR=(-999)
C
      ILOC2=(-999)
      ILOC3=(-999)
      ILOC4=(-999)
C
      ILOC2P=(-999)
      ILOC3P=(-999)
      ILOC4P=(-999)
C
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1992
      IYESNO=' '
C
      IFOUND='YES'
      IERROR='NO'
C
      ISUBN1='EDHE'
      ISUBN2='LP  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'HELP')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDHELP')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDHELP')
   51 FORMAT('*****AT THE BEGINNING OF EDHELP--')
      WRITE(ICOUT,53)IHELNU
      CALL EDWRST('EDHELP')
   53 FORMAT('IHELNU = ',I8)
      WRITE(ICOUT,54)IHELNA
      CALL EDWRST('EDHELP')
   54 FORMAT('IHELNA = ',A80)
      WRITE(ICOUT,55)IHELST
      CALL EDWRST('EDHELP')
   55 FORMAT('IHELST = ',A12)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **************************
C               **  STEP 1--            **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=IHELNU
      IFILE=IHELNA
      ISTAT=IHELST
      IFORM=IHELFO
      IACCES=IHELAC
      IREWR=IHELRW
      ISUBN0='HELP'
      IERRFI='NO'
C
      IF(IBUGE2.EQ.'OFF')GOTO1199
      WRITE(ICOUT,1193)IOUNIT
      CALL EDWRST('EDHELP')
 1193 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,1194)IFILE
      CALL EDWRST('EDHELP')
 1194 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IREWR
      CALL EDWRST('EDHELP')
 1195 FORMAT('ISTAT,IFORM,IACCES,IREWR = ',
     1A12,2X,A12,2X,A12,2X,A12)
      WRITE(ICOUT,1196)ISUBN0,IERRFI
      CALL EDWRST('EDHELP')
 1196 FORMAT('ISUBN0,IERRFI = ',A4,2X,A4)
 1199 CONTINUE
C
C               *********************
C               **  STEP 2--       **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO9000
C
C               **********************************************************
C               **********************************************************
C               **  STEP 2--                                            **
C               **  COPY OVER THE FIRST 4 WORDS AFTER THE WORD   HELP.  **
C               **********************************************************
C               **********************************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IWORD1=IHARG(1)
      IWOR12=IHARG2(1)
      IWORD2=IHARG(2)
      IWORD3=IHARG(3)
      IWORD4=IHARG(4)
C
      NUMAR2=NUMARG
      IF(NUMARG.LE.0)NUMAR2=1
      IF(NUMARG.LE.0)IWORD1='OVER'
C
C               ********************************************************
C               ********************************************************
C               **  STEP 3--                                          **
C               **  STRIP OUT THE FIRST CHARACTER OF THE FIRST WORD.  **
C               ********************************************************
C               ********************************************************
C
      ISTEPN='3'
C
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICHAR1=IWORD1(1:1)
C
C               ************************************************************
C               ************************************************************
C               **  STEP 4--                                              **
C               **  READ IN FILE INFORMATION                              **
C               **  FROM THE BEGINNING LINES OF THE FILE.                 **
C               **  THE FIRST LINE CONTAINS THE                           **
C               **  NUMBER OF LINES IN THE FILE (ANUMLF) (F10.0 FORMAT).  **
C               **  THE SECOND LINE CONTAINS THE NUMBER OF                **
C               **  SECTIONS IN THE FILE (ANUMSF) (F10.0 FORMAT)          **
C               **  THE NEXT ANUMSF LINES CONTAIN                         **
C               **  THE THIRD  LINE CONTAINS THE SECTION NUMBER           **
C               **  OF THE     A    SECTION (ASECNA) (F10.0 FORMAT).      **
C               **  THE STARTING LINE NUMBER OF EACH SECTION              **
C               **  IN THE FILE (ATABLN)   (F10.0 FORMAT), AND            **
C               **  THE IDENTIFIER (IF ANY) FOR EACH SECTION              **
C               **  IN THE FILE (ITABID(.) (A4 FORMAT).                   **
C               ************************************************************
C               ************************************************************
C
CCCCC READ(IOUNIT,1411)ANUMLF
C1411 FORMAT(F10.0)
CCCCC NUMLF=ANUMLF+0.5
      NUMLF=1000000
      READ(IOUNIT,1412)ANUMSF
 1412 FORMAT(F10.0)
      NUMSEC=ANUMSF+0.5
      READ(IOUNIT,1413)ASECNA
 1413 FORMAT(F10.0)
      ISECNA=ASECNA+0.5
      IF(NUMSEC.LE.0)GOTO1490
      DO1420I=1,NUMSEC
      READ(IOUNIT,1421)ATABLN,ITABID(I)
 1421 FORMAT(F10.0,A4)
      ITABLN(I)=ATABLN+0.5
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')THEN
         WRITE(ICOUT,1422)I,ATABLN,ITABLN(I),ITABID(I)
         CALL EDWRST('EDHELP')
      ENDIF
 1422 FORMAT('I,ATABLN,ITABLN(I),ITABID(I) = ',I8,E15.7,I8,2X,A4)
 1420 CONTINUE
 1490 CONTINUE
C
C               *******************************************************
C               *******************************************************
C               **  STEP 5--                                         **
C               **  BASED ON THE FIRST CHARACTER OF THE FIRST WORD,  **
C               **  DO A TABLE LOOK-UP WHICH WILL SPECIFY            **
C               **  THE ABSOLUTE LINE NUMBER IN THE FILE             **
C               **  IN WHICH ALL COMMANDS STARTING OFF WITH THAT     **
C               **  CHARACTER WILL START.                            **
C               *******************************************************
C               *******************************************************
C
      ISTEPN='5'
C
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      JCHAR1=ICHAR(ICHAR1)
C
      JSEC=NUMSEC
      IF(65.LE.JCHAR1.AND.JCHAR1.LE.90)JSEC=JCHAR1-65+ISECNA
      IF(IWORD1.EQ.'OVER')JSEC=1
      IF(IWORD1.EQ.'ABBR')JSEC=2
      IF(IWORD1.EQ.'DEFA')JSEC=3
      IF(IWORD1.EQ.'DEF')JSEC=3
      IF(IWORD1.EQ.'EXAM')JSEC=4
      IF(IWORD1.EQ.'FEAT')JSEC=5
      IF(IWORD1.EQ.'HOST')JSEC=6
      IF(IWORD1.EQ.'IMPL')JSEC=7
      IF(IWORD1.EQ.'LANG')JSEC=8
      IF(IWORD1.EQ.'MODE')JSEC=9
      IF(IWORD1.EQ.'OUTP')JSEC=10
      IF(IWORD1.EQ.'PORT')JSEC=11
      IF(IWORD1.EQ.'SUMM')JSEC=12
      IF(IWORD1.EQ.'TERM')JSEC=13
      IF(IWORD1.EQ.'TUTO')JSEC=14
      ISTART=ITABLN(JSEC)
C
      IF(ISTART.LE.NUMLF)GOTO1509
      WRITE(ICOUT,999)
      CALL EDWRST('EDHELP')
      WRITE(ICOUT,1501)
      CALL EDWRST('EDHELP')
 1501 FORMAT('***** INTERNAL ERROR IN EDHELP--')
      WRITE(ICOUT,1502)
      CALL EDWRST('EDHELP')
 1502 FORMAT('THE START LINE EXCEEDS THE NUMBER OF LINES.')
      WRITE(ICOUT,1503)IWORD1,JCHAR1,JSEC,JSECP1,ISTART,NUMLF
      CALL EDWRST('EDHELP')
 1503 FORMAT('IWORD1,JCHAR1,JSEC,JSECP1,ISTART,NUMLF = ',6I8)
      IERROR='YES'
      GOTO8000
 1509 CONTINUE
C
      JSECP1=JSEC+1
      ISTOP=NUMLF
      IF(JSECP1.LE.NUMSEC)ISTOP=ITABLN(JSECP1)
      IF(ISTOP.LE.ISTART)ISTOP=NUMLF
 
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'HELP')
     1GOTO1590
 1511 FORMAT('***** FROM 1511 IN MIDDLE OF EDHELP--')
      WRITE(ICOUT,1512)ICHAR1,JCHAR1,IWORD1,IWOR12
      CALL EDWRST('EDHELP')
 1512 FORMAT('ICHAR1,JCHAR1,IWORD1,IWOR12 = ',A1,2X,I8,2X,A4,2X,A4)
      WRITE(ICOUT,1513)JSEC,JSECP1,ISTART,ISTOP
      CALL EDWRST('EDHELP')
 1513 FORMAT('JSEC,JSECP1,ISTART,ISTOP = ',4I8)
 1590 CONTINUE
C
C               *************************************************
C               *************************************************
C               **  STEP 6--                                   **
C               **  READ DOWN IN THE FILE TO                   **
C               **  THE LINE BEFORE WHERE THE CHARACTER RESIDES**
C               *************************************************
C               *************************************************
C
      ISTEPN='6'
C
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      REWIND(IOUNIT)
C
      ISKIP=ISTART-1
      IF(ISKIP.LE.0)GOTO1690
      DO1600I=1,ISKIP
      READ(IOUNIT,1605,END=1780)
 1605 FORMAT()
 1600 CONTINUE
 1690 CONTINUE
C
C               ******************************************************
C               ******************************************************
C               **  STEP 7--                                        **
C               **  LOOP THROUGH THE VARIOUS LINES OF THIS SECTION  **
C               **  OF THE FILE.                                    **
C               ******************************************************
C               ******************************************************
C
      ISTEPN='7'
C
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      DO1700I=ISTART,ISTOP
      I2=I
C
C               *****************************************
C               *****************************************
C               **  STEP 8--                           **
C               **  READ IN SUCCEEDING LINES UNTIL     **
C               **  GET A HIT BASED ON THE FIRST WORD  **
C               **  OF THE COMMAND.                    **
C               *****************************************
C               *****************************************
C
      READ(IOUNIT,1702,END=1780)IW1,IW2,IW3,IW4,IW5
 1702 FORMAT(5A4)
      IF(IW1.EQ.'    ')GOTO1700
      ICTEST=IW1
      IF(ICTEST(3:3).EQ.' ')ICTEST(3:4)='  '
      IF(ICTEST(2:2).EQ.' ')ICTEST(2:4)='   '
CCCCC WRITE(ICOUT,1703)I,IW1,ICTEST,IWORD1,IWOR12
C1703 FORMAT('I,IW1,ICTEST,IWORD1,IWOR12 = ',
CCCCC1I8,2X,A4,2X,A4,2X,A4,2X,A4)
      IF(ICTEST.EQ.IWORD1)GOTO1706
      GOTO1700
 1706 CONTINUE
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')THEN
         WRITE(ICOUT,1707)I,IW1,IW2,IW3,IW4,IW5,
     1   ICTEST,IWORD1,IWOR12
         CALL EDWRST('EDHELP')
      ENDIF
 1707 FORMAT('I,IW1,IW2,IW3,IW4,IW5,ICTEST,IWORD1,IWOR12 = ',
     1I8,2X,5A4,2X,A4,2X,A4,2X,A4)
C
C               ***************************************
C               ***************************************
C               **  STEP 9--                         **
C               **  IF GOT A HIT ON THE FIRST WORD,  **
C               **  CHECK FOR A HIT ON ALL WORDS     **
C               ***************************************
C               ***************************************
C
      DO1710J=1,4
      JP4=J+4
      JP8=J+8
      JP12=J+12
      JP16=J+16
      ISTRIN(J:J)=IW1(J:J)
      ISTRIN(JP4:JP4)=IW2(J:J)
      ISTRIN(JP8:JP8)=IW3(J:J)
      ISTRIN(JP12:JP12)=IW4(J:J)
      ISTRIN(JP16:JP16)=IW5(J:J)
 1710 CONTINUE
C
      NUMWOR=1
      ILOC2=1
      ILOC3=1
      ILOC4=1
      DO1720J=1,16
      IF(ISTRIN(J:J).EQ.' ')GOTO1725
      GOTO1720
 1725 CONTINUE
      NUMWOR=NUMWOR+1
      IF(NUMWOR.EQ.2)ILOC2=J+1
      IF(NUMWOR.EQ.3)ILOC3=J+1
      IF(NUMWOR.EQ.4)ILOC4=J+1
 1720 CONTINUE
C
      ILOC2P=ILOC2+3
      ILOC3P=ILOC3+3
      ILOC4P=ILOC4+3
      IZ1=IW1
      IZ2(1:4)='    '
      IF(NUMWOR.GE.2)IZ2(1:4)=ISTRIN(ILOC2:ILOC2P)
      IZ3(1:4)='    '
      IF(NUMWOR.GE.3)IZ3(1:4)=ISTRIN(ILOC3:ILOC3P)
      IZ4(1:4)='    '
      IF(NUMWOR.GE.4)IZ4(1:4)=ISTRIN(ILOC4:ILOC4P)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'HELP')
     1GOTO1739
      WRITE(ICOUT,1731)
      CALL EDWRST('EDHELP')
 1731 FORMAT('***** FROM 1731 IN MIDDLE OF EDHELP--')
      WRITE(ICOUT,1732)IWORD1,IWORD2,IWORD3,IWORD4,IWOR12
      CALL EDWRST('EDHELP')
 1732 FORMAT('IWORD1,IWORD2,IWORD3,IWORD4,IWOR12 = ',
     1A4,2X,A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,1733)IW1,IW2,IW3,IW4,IW5
      CALL EDWRST('EDHELP')
 1733 FORMAT('IW1,IW2,IW3,IW4,IW5 = ',A4,2X,A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,1734)IZ1,IZ2,IZ3,IZ4
      CALL EDWRST('EDHELP')
 1734 FORMAT('IZ1,IZ2,IZ3,IZ4 = ',A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,1735)ISTRIN
      CALL EDWRST('EDHELP')
 1735 FORMAT('ISTRIN = ',A20)
      WRITE(ICOUT,1736)NUMWOR
      CALL EDWRST('EDHELP')
 1736 FORMAT('NUMWOR = ',I8)
      WRITE(ICOUT,1737)ILOC2,ILOC3,ILOC4
      CALL EDWRST('EDHELP')
 1737 FORMAT('ILOC2,ILOC3,ILOC4 = ',3I8)
      WRITE(ICOUT,1738)ILOC2P,ILOC3P,ILOC4P
      CALL EDWRST('EDHELP')
 1738 FORMAT('ILOC2P,ILOC3P,ILOC4P = ',3I8)
 1739 CONTINUE
C
 1752 CONTINUE
      IF(NUMAR2.LE.1)GOTO1790
      IF(NUMWOR.LE.1)GOTO1790
      IF(IZ2.EQ.IWORD2)GOTO1753
      GOTO1700
C
 1753 CONTINUE
      IF(NUMAR2.LE.2)GOTO1790
      IF(NUMWOR.LE.2)GOTO1790
      IF(IZ3.EQ.IWORD3)GOTO1754
      GOTO1700
C
 1754 CONTINUE
      IF(NUMAR2.LE.3)GOTO1790
      IF(NUMWOR.LE.3)GOTO1790
      IF(IZ4.EQ.IWORD4)GOTO1790
      GOTO1700
C
 1700 CONTINUE
C
 1780 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL EDWRST('EDHELP')
      WRITE(ICOUT,1781)
      CALL EDWRST('EDHELP')
 1781 FORMAT('***** ERROR IN EDHELP--')
      WRITE(ICOUT,1782)
      CALL EDWRST('EDHELP')
 1782 FORMAT('      THE SPECIFIED COMMAND FOR WHICH')
      WRITE(ICOUT,1783)
      CALL EDWRST('EDHELP')
 1783 FORMAT('      HELP WAS DESIRED WAS NOT FOUND')
      WRITE(ICOUT,1784)
      CALL EDWRST('EDHELP')
 1784 FORMAT('      IN THE HELP FILE.')
      WRITE(ICOUT,1785)
      CALL EDWRST('EDHELP')
 1785 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      WRITE(ICOUT,1786)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDHELP')
 1786 FORMAT('      ',230A1)
      GOTO8000
C
 1790 CONTINUE
C
C               ****************************************************
C               ****************************************************
C               **  STEP 10--                                     **
C               **  IF HAVE A HIT ON ALL WORDS,                   **
C               **  THEN READ IN AND WRITE OUT                    **
C               **  THE ENTIRE TEXT DESCRIPTION ASSOCIATED WITH   **
C               **  THE DESIRED COMMAND.                          **
C               **  THIS DESCRIPTION WILL START ON THE NEXT LINE  **
C               **  AND WILL FINISH WITH A LINE OF HYPHENS.       **
C               ****************************************************
C               ****************************************************
C
      ISTEPN='10'
C
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1992
      K=0
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDHELP')
CCCCC DO2000I=1,2
      DO2000I=1,100000
      READ(IOUNIT,2005,END=2090)(ICTEXT(J),J=1,20)
 2005 FORMAT(20A4)
      IF(ICTEXT(1).EQ.'----')GOTO2090
C
      DO2010J=1,20
      JREV=20-J+1
      IF(ICTEXT(JREV).NE.'    ')GOTO2015
 2010 CONTINUE
 2015 CONTINUE
      JMAX=JREV
C
      WRITE(ICOUT,2006)(ICTEXT(J),J=1,JMAX)
      CALL EDWRST('EDHELP')
 2006 FORMAT(20A4)
C
CCCCC THE FOLLOWING SECTION WAS ADDED JULY 1992
CCCCC SO THAT HELP INFO WOULD COME OUT 1 PAGE AT A TIME   JULY 1992
      K=K+1
      IF(K.GE.20)THEN
         WRITE(ICOUT,2007)
      CALL EDWRST('EDHELP')
 2007    FORMAT('                                      More...?')
         READ(IRD,2008)IYESNO
 2008    FORMAT(A1)
         IF(IYESNO.EQ.'N')GOTO2090
         K=0
      ENDIF
C
 2000 CONTINUE
C
 2090 CONTINUE
C
C               ***********************************************
C               ***********************************************
C               **  STEP 11--                          **
C               **  CLOSE           THE HELP FILE.  **
C               ***********************************************
C               ***********************************************
C
      ISTEPN='11'
C
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
 8000 CONTINUE
C
      ISTEPN='4'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HELP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               ****************
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C               ****************
C
 9000 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDHELP')
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'HELP')GOTO9090
      WRITE(ICOUT,9011)
      CALL EDWRST('EDHELP')
 9011 FORMAT('***** AT THE END       OF EDHELP--')
      WRITE(ICOUT,9012)ISKIP,ISTART,ISTOP,JMAX
      CALL EDWRST('EDHELP')
 9012 FORMAT('ISKIP,ISTART,ISTOP,JMAX = ',4I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDHOLD
C
C     PURPOSE--HOLD (= SAVE) THE CURRENT LINE
C     COMMAND SYNTAX--HOLD <ID> WHERE <ID> IS BLANK, OR 1 TO 10
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ISTRIN
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDHO'
      ISUBN2='LD  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      NUMCST=(-999)
      ISTRIN(1:1)=' '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'HOLD')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDHOLD')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDHOLD')
   51 FORMAT('***** AT THE BEGINNING OF EDHOLD--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE STRING TO BE HELD      **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HOLD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ICURLN
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      I=0
      IF(J1.GT.J2)GOTO1190
      IF(N1.LE.0)GOTO1190
      DO1100J=J1,J2
      I=I+1
      ISTRIN(I:I)=ICHA(J)
 1100 CONTINUE
 1190 CONTINUE
      NUMCST=I
C
      IF(NUMCST.LE.0)ISTRIN(1:1)=' '
      IF(NUMCST.LE.0)NUMCST=1
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  COPY THE STRING                                  **
C               **  INTO IHOLS1, IHOLS2, IHOLS3, ..., OR IHOLS10.    **
C               *******************************************************
C
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'HOLD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IARG1=1
      IF(NUMARG.GE.1)IARG1=IARG(1)
C
      IF(IARG1.EQ.1)NCHH1=NUMCST
      IF(IARG1.EQ.2)NCHH2=NUMCST
      IF(IARG1.EQ.3)NCHH3=NUMCST
      IF(IARG1.EQ.4)NCHH4=NUMCST
      IF(IARG1.EQ.5)NCHH5=NUMCST
      IF(IARG1.EQ.6)NCHH6=NUMCST
      IF(IARG1.EQ.7)NCHH7=NUMCST
      IF(IARG1.EQ.8)NCHH8=NUMCST
      IF(IARG1.EQ.9)NCHH9=NUMCST
      IF(IARG1.EQ.10)NCHH10=NUMCST
 
C
      IF(NUMCST.LE.0)GOTO1290
      DO1200I=1,NUMCST
      IF(IARG1.EQ.1)IHOLS1(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.2)IHOLS2(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.3)IHOLS3(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.4)IHOLS4(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.5)IHOLS5(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.6)IHOLS6(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.7)IHOLS7(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.8)IHOLS8(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.9)IHOLS9(I:I)=ISTRIN(I:I)
      IF(IARG1.EQ.10)IHOL10(I:I)=ISTRIN(I:I)
 1200 CONTINUE
 1290 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'HOLD')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDHOLD')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDHOLD')
 9011 FORMAT('***** AT THE END       OF EDHOLD--')
      WRITE(ICOUT,9016)ICURLN,IARG1
      CALL EDWRST('EDHOLD')
 9016 FORMAT('ICURLN,IARG1 = ',2I8)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDHOLD')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)NUMCST
      CALL EDWRST('EDHOLD')
 9018 FORMAT('NUMCST = ',A4)
      WRITE(ICOUT,9019)(ISTRIN(I:I),I=1,NUMCST)
      CALL EDWRST('EDHOLD')
 9019 FORMAT('(ISTRIN(I:I),I=1,NUMCST) = ',80A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDHOLD')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDIND2(IOLDLI,NUMCOL,INEWLI,NUMCNL)
C
C     PURPOSE--INDENT THE LINE IN IOLDLI(.).
C              THE INDENTED LINE WILL BE PLACED IN INEWLI(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      DIMENSION IOLDLI(*)
      DIMENSION INEWLI(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDIN'
      ISUBN2='N2  '
C
      IINDE2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'IND2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDIND2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDIND2')
   51 FORMAT('***** AT THE BEGINNING OF EDIND2--')
      WRITE(ICOUT,71)NUMCOL
      CALL EDWRST('EDIND2')
   71 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,72)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDIND2')
   72 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,73)NUMCNL
      CALL EDWRST('EDIND2')
   73 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,74)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDIND2')
   74 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,81)IINDEC
      CALL EDWRST('EDIND2')
   81 FORMAT('IINDEC = ',I8)
   90 CONTINUE
C
C               ************************************************************
C               **  STEP 1--                                              **
C               **  DETERMINE THE FIRST NON-BLANK CHARACTER IN IOLDLI(.)  **
C               ************************************************************
C
      I2=0
      IF(NUMCOL.LE.0)GOTO1190
      DO1100I=1,NUMCOL
      I2=I
      IF(IOLDLI(I2).NE.' ')GOTO1190
 1100 CONTINUE
      I2=NUMCOL+1
 1190 CONTINUE
      IFIRST=I2
C
C               ********************************
C               **  STEP 2--                  **
C               **  BLANK OUT THE NEW STRING  **
C               ********************************
C
      NUMCNL=1
      DO1500I=1,240
      INEWLI(I)=' '
 1500 CONTINUE
C
C               **************************************
C               **  STEP 3--                        **
C               **  FORM THE NEW (INDENTED) STRING  **
C               **************************************
C
      INEW=1
      IF(IFIRST.LE.0)GOTO1690
      IF(IFIRST.GT.NUMCOL)GOTO1690
      IINDE2=IINDEC
      IF(IINDE2.LE.0)IINDE2=1
      INEW=IINDE2-1
      DO1600IOLD=IFIRST,NUMCOL
      INEW=INEW+1
      IF(INEW.LT.1)GOTO1600
      IF(INEW.GT.240)GOTO1600
      INEWLI(INEW)=IOLDLI(IOLD)
 1600 CONTINUE
 1690 CONTINUE
      NUMCNL=INEW
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'IND2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDIND2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDIND2')
 9011 FORMAT('***** AT THE END       OF EDIND2--')
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDIND2')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDIND2')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDIND2')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDIND2')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,9041)IINDEC,IINDE2
      CALL EDWRST('EDIND2')
 9041 FORMAT('IINDEC,IINDE2 = ',2I8)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDINDE
C
C     PURPOSE--INDENT CURRENT LINE
C              (AND NEXT IARG2-1 LINES)
C              IARG1 SPACES
C     SYNTAX--INDENT <S> <L>
C             WHERE <S> = NUMBER OF SPACES (DEFAULT = 5)
C             AND   <L> = NUMBER OF LINES  (DEFAULT = 1)
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      DIMENSION IOLDLI(240)
      DIMENSION INEWLI(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDIN'
      ISUBN2='DE  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INDE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDINDE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDINDE')
   51 FORMAT('***** AT THE BEGINNING OF EDINDE--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 1--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE INDENTED.  **
C               ************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INDE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=ICURLN
C
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      GOTO1120
C
 1100 CONTINUE
      ISTART=ICURLN
      ISTOP=ICURLN
      GOTO1190
C
 1110 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      ISTART=ICURLN
      ISTOP=ISTART+IARG1-1
      GOTO1190
C
 1120 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      IARG2=NUMLIN+1
      IF(IARGT(2).EQ.'NUMB')IARG2=IARG(2)
      ISTART=IARG1
      ISTOP=IARG2
      GOTO1190
C
 1190 CONTINUE
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD2.LT.IHOLD1)ISTART=IHOLD2
      IF(IHOLD2.LT.IHOLD1)ISTOP=IHOLD1
C
C               ********************************************
C               **  STEP 21--                             **
C               **  LOOP THROUGH THE LINES TO BE INDENTED **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INDE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ISTART-1
 2100 CONTINUE
      ILINE=ILINE+1
      IF(ILINE.LE.ISTOP)GOTO2910
      ILINE=ISTOP
      GOTO2900
 2910 CONTINUE
      ICURLN=ILINE
      IF(ILINE.LT.1)GOTO2100
      IF(ILINE.GT.NUMLIN)GOTO2900
C
C               *****************************
C               **  STEP 22--              **
C               **  COPY THE OLD    LINE   **
C               *****************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INDE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      I=0
      DO2200J=J1,J2
      I=I+1
      IOLDLI(I)=ICHA(J)
 2200 CONTINUE
      NUMCOL=I
C
C               ***********************************
C               **  STEP 23--                    **
C               **  APPLY THE DESIRED INDENTING  **
C               **  TO THE TARGET LINE,          **
C               **  SO AS TO CREATE              **
C               **  A NEW LINE.                  **
C               ***********************************
C
      ISTEPN='23'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INDE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDIND2(IOLDLI,NUMCOL,INEWLI,NUMCNL)
C
C               ***************************************
C               **  STEP 24--                        **
C               **  DETERMINE IF ROOM EXISTS IN THE  **
C               **  MAIN INTERNAL CHARACTER ARRAY    **
C               **  FOR THE NEW INDENTED LINE.       **
C               ***************************************
C
      ISTEPN='24'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INDE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH9=NUMCHA+NUMCNL
      CALL EDCHSI(NUMCH9)
      IF(IERROR.EQ.'YES')GOTO9000
C
 2450 CONTINUE
      IF(ILINE.LT.1)GOTO2460
      IF(ILINE.GT.NUMLIN)GOTO2470
      GOTO2490
C
 2460 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2461)
         CALL EDWRST('EDINDE')
      ENDIF
 2461 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2462)
         CALL EDWRST('EDINDE')
      ENDIF
 2462 FORMAT(10X,'[TOP]')
 2469 CONTINUE
      GOTO2100
C
 2470 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2471)
         CALL EDWRST('EDINDE')
      ENDIF
 2471 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2472)
         CALL EDWRST('EDINDE')
      ENDIF
 2472 FORMAT(10X,'[BOTTOM]')
 2479 CONTINUE
      GOTO9000
C
 2490 CONTINUE
C
C               ***************************************
C               **  STEP 25--                        **
C               **  UPDATE THE MAIN CHARACTER ARRAY  **
C               **  WITH THE NEW     LINE.           **
C               ***************************************
C
      ISTEPN='25'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INDE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J1=NUMCHA+1
      N1=NUMCNL
      J2=J1+(N1-1)
C
      NUMRP1=NUMROW+1
      IROW=NUMRP1
      IPOINT(ILINE)=IROW
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO2500J=J1,J2
      I=I+1
      ICHA(J)=INEWLI(I)
 2500 CONTINUE
      NUMROW=NUMRP1
      NUMCHA=J2
C
      IF(IFEESW.EQ.'OFF')GOTO2629
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2621)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDINDE')
      ENDIF
 2621 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2622)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDINDE')
      ENDIF
 2622 FORMAT(I6,':',3X,230A1)
 2629 CONTINUE
C
      GOTO2100
C
 2900 CONTINUE
      IF(ILINE.LE.NUMLIN)GOTO2919
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2911)
         CALL EDWRST('EDINDE')
      ENDIF
 2911 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2912)
         CALL EDWRST('EDINDE')
      ENDIF
 2912 FORMAT(10X,'[BOTTOM]')
 2919 CONTINUE
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INDE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDINDE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDINDE')
 9011 FORMAT('***** AT THE END       OF EDINDE--')
      WRITE(ICOUT,999)
      CALL EDWRST('EDINDE')
      WRITE(ICOUT,9012)IWIDTH
      CALL EDWRST('EDINDE')
 9012 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9013)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDINDE')
 9013 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDINDE')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDINDE')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDINDE')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDINDE')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,9041)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDINDE')
 9041 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9042)J1,N1,J2
      CALL EDWRST('EDINDE')
 9042 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDINDE')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDINFI(IFILE,IEXIST,ISUBN0,IERRFI)
C
C     PURPOSE--INQUIRE ABOUT THE EXISTENCE OF A FILE
C              (BUT THERE MAY BE SOME SMALL DIFFERENCES
C              IN HOW THAT IS DONE FOR DIFFERENT COMPUTERS).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JULY 1986.
C
C------------------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*4 IEXIST
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
      LOGICAL LEXIST
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDIN'
      ISUBN2='FI  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'INFI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDINFI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDINFI')
   51 FORMAT('*****AT THE BEGINNING OF EDINFI--')
      WRITE(ICOUT,52)IFILE
      CALL EDWRST('EDINFI')
   52 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,54)IEXIST
      CALL EDWRST('EDINFI')
   54 FORMAT('IEXIST = ',A4)
      WRITE(ICOUT,55)ISUBN0
      CALL EDWRST('EDINFI')
   55 FORMAT('ISUBN0 = ',A4)
      WRITE(ICOUT,56)IERRFI
      CALL EDWRST('EDINFI')
   56 FORMAT('IERRFI = ',A4)
      WRITE(ICOUT,61)IHOST1
      CALL EDWRST('EDINFI')
   61 FORMAT('IHOST1 = ',A4)
   90 CONTINUE
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  INQUIRE ABOUT THE EXISTENCE OF A FILE.            **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'INFI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHOST1.EQ.'VAX')GOTO1100
      GOTO1200
C
 1100 CONTINUE
      INQUIRE(FILE=IFILE,EXIST=LEXIST)
      IEXIST='NO'
      IF(LEXIST)IEXIST='YES'
      GOTO9000
C
 1200 CONTINUE
      INQUIRE(FILE=IFILE,EXIST=LEXIST)
      IEXIST='NO'
      IF(LEXIST)IEXIST='YES'
      GOTO9000
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'INFI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDINFI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDINFI')
 9011 FORMAT('*****AT THE END       OF EDINFI--')
      WRITE(ICOUT,9012)IFILE
      CALL EDWRST('EDINFI')
 9012 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IEXIST
      CALL EDWRST('EDINFI')
 9014 FORMAT('IEXIST = ',A4)
      WRITE(ICOUT,9015)ISUBN0
      CALL EDWRST('EDINFI')
 9015 FORMAT('ISUBN0 = ',A4)
      WRITE(ICOUT,9016)IERRFI
      CALL EDWRST('EDINFI')
 9016 FORMAT('IERRFI = ',A4)
      WRITE(ICOUT,9021)IHOST1
      CALL EDWRST('EDINFI')
 9021 FORMAT('IHOST1 = ',A4)
 9090 CONTINUE
      RETURN
      END
      SUBROUTINE EDININ
C
C     PURPOSE--WHILE IN INPUT MODE, INSERT A LINE OF TEXT
C              (IMMEDIATELY AFTER CURRENT LINE).
C              THE ENTIRE STRING IN IANS(.) WILL BE INSERTED.
C              IF NO STRING IS IN IANS(.),
C              THEN A BLANK LINE (CONSISTING OF 1 BLANK CHARACTER)
C              WILL BE INSERTED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDIN'
      ISUBN2='IN  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IFIRBL=(-999)
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ININ')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDININ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDININ')
   51 FORMAT('***** AT THE BEGINNING OF EDININ--')
      WRITE(ICOUT,52)IWIDTH
      CALL EDWRST('EDININ')
   52 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,53)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDININ')
   53 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 2--                             **
C               **  DETERMINE THE STRING TO BE INSERTED  **
C               *******************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ININ')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTRIN(1)=' '
      NUMCST=1
C
      IF(IWIDTH.LE.0)GOTO1190
      DO1130I=1,IWIDTH
      ISTRIN(I)=IANS(I)
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ININ')THEN
         WRITE(ICOUT,1131)I,IWIDTH,NUMCST,IANS(I),ISTRIN(I)
         CALL EDWRST('EDININ')
      ENDIF
 1131 FORMAT('I,IWIDTH,NUMCST,IANS(I),ISTRIN(I) = ',3I8,2X,A1,2X,A1)
 1130 CONTINUE
      NUMCST=IWIDTH
      GOTO1190
C
 1190 CONTINUE
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ININ')THEN
         WRITE(ICOUT,1191)NUMCST,(ISTRIN(I),I=1,40)
         CALL EDWRST('EDININ')
      ENDIF
 1191 FORMAT('NUMCST,(ISTRIN(I),I=1,20) = ',I8,2X,80A1)
C
C               ****************************
C               **  STEP 3--              **
C               **  INSERT     THE LINE.  **
C               ****************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ININ')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH2=NUMCHA+NUMCST
      CALL EDCHSI(NUMCH2)
      IF(IERROR.EQ.'YES')GOTO9000
C
 1550 CONTINUE
      IF(ICURLN.GT.NUMLIN)ICURLN=NUMLIN
      ICURLP=ICURLN+1
      NUMLP1=NUMLIN+1
      NUMRP1=NUMROW+1
C
      IHOLD1=IPOINT(ICURLP)
      IPOINT(ICURLP)=NUMRP1
C
      IF(ICURLP.LE.0)GOTO1569
      IF(NUMLIN.LE.0)GOTO1569
      IF(ICURLP.GT.NUMLIN)GOTO1569
      DO1560ILINE=ICURLP,NUMLIN
      ILINEP=ILINE+1
      IHOLD2=IPOINT(ILINEP)
      IPOINT(ILINEP)=IHOLD1
      IHOLD1=IHOLD2
 1560 CONTINUE
 1569 CONTINUE
C
      J1=NUMCHA+1
      N1=NUMCST
      J2=J1+(N1-1)
C
      IROW=NUMRP1
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO1570J=J1,J2
      I=I+1
      ICHA(J)=ISTRIN(I)
 1570 CONTINUE
 1579 CONTINUE
      ICURLN=ICURLP
      NUMLIN=NUMLP1
      NUMROW=NUMRP1
      NUMCHA=J2
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ININ')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDININ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDININ')
 9011 FORMAT('***** AT THE END       OF EDININ--')
      WRITE(ICOUT,9012)IWIDTH
      CALL EDWRST('EDININ')
 9012 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9013)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDININ')
 9013 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9014)NUMCST
      CALL EDWRST('EDININ')
 9014 FORMAT('NUMCST = ',I8)
      WRITE(ICOUT,9015)(ISTRIN(I),I=1,NUMCST)
      CALL EDWRST('EDININ')
 9015 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,9016)IFIRBL
      CALL EDWRST('EDININ')
 9016 FORMAT('IFIRBL = ',I8)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDININ')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDININ')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDINLI(ICNEWL,NCNEWL,ICURL2)
C
C     PURPOSE--INSERT THE LINE IN ICNEWL
C              (WHICH CONSISTS OF NCNEWL CHARACTERS)
C              IMMEDIATELY AFTER LINE ICURL2
C              (ICURL2 IS THE PSEUDO-CURRENT LINE NUMBER).
C     CAUTION--THE INPUT ARGUMENT ICURL2 MAY BE CHANGED
C              WITHIN THIS SUBROUTINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.1
C     ORIGINAL VERSION--MAY     1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ICNEWL
      CHARACTER*1 ISTRIN
CCCCC CHARACTER*4 IFOUST
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDIN'
      ISUBN2='LI  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INLI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDINLI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDINLI')
   51 FORMAT('***** AT THE BEGINNING OF EDINLI--')
      WRITE(ICOUT,52)NCNEWL
      CALL EDWRST('EDINLI')
   52 FORMAT('NCNEWL = ',I8)
      IF(NCNEWL.GE.1)THEN
         WRITE(ICOUT,53)(ICNEWL(I:I),I=1,100)
         CALL EDWRST('EDINLI')
      ENDIF
   53 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,54)ICURL2
      CALL EDWRST('EDINLI')
   54 FORMAT('ICURL2 = ',I8)
      WRITE(ICOUT,61)ITABC
      CALL EDWRST('EDINLI')
   61 FORMAT('ITABC = ',I8)
      WRITE(ICOUT,62)ICURL2,NUMLIN
      CALL EDWRST('EDINLI')
   62 FORMAT('ICURL2,NUMLIN = ',2I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **********************************
C               **  STEP 10--                  **
C               **  SAVE THE CURRENT POINTERS   **
C               **  (FOR POSSIBLE LATER USE     **
C               **  BY THE   UNDO     COMMAND)  **
C               **********************************
C
      ISTEPN='10'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INLI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDSACP
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE STRING TO BE INSERTED  **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INLI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC CALL EDEXST(ISTRIN,NUMCST,IFOUST)
CCCCC IFOUST='YES'
      IF(NCNEWL.LE.0)GOTO1111
      DO1110I=1,NCNEWL
      ISTRIN(I)=ICNEWL(I:I)
 1110 CONTINUE
 1111 CONTINUE
      NUMCST=NCNEWL
C
      ITABM1=ITABC-1
      IF(ITABM1.LE.0)GOTO1290
C
      IF(NUMCST.LE.0)GOTO1190
      DO1120I=1,NUMCST
      IREV=NUMCST-I+1
      IREV2=IREV+ITABM1
      ISTRIN(IREV2)=ISTRIN(IREV)
 1120 CONTINUE
 1190 CONTINUE
C
      DO1200I=1,ITABM1
      ISTRIN(I)=' '
 1200 CONTINUE
C
 1290 CONTINUE
      NUMCST=NUMCST+ITABM1
C
C               ****************************
C               **  STEP 12--             **
C               **  INSERT     THE LINE.  **
C               ****************************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INLI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH2=NUMCHA+NUMCST
      CALL EDCHSI(NUMCH2)
      IF(IERROR.EQ.'YES')GOTO9000
C
 1550 CONTINUE
      IF(ICURL2.GT.NUMLIN)ICURLN=NUMLIN
      ICURLP=ICURL2+1
      NUMLP1=NUMLIN+1
      NUMRP1=NUMROW+1
C
      IHOLD1=IPOINT(ICURLP)
      IPOINT(ICURLP)=NUMRP1
C
      IF(ICURLP.LE.0)GOTO1569
      IF(NUMLIN.LE.0)GOTO1569
      IF(ICURLP.GT.NUMLIN)GOTO1569
      DO1560ILINE=ICURLP,NUMLIN
      ILINEP=ILINE+1
      IHOLD2=IPOINT(ILINEP)
      IPOINT(ILINEP)=IHOLD1
      IHOLD1=IHOLD2
 1560 CONTINUE
 1569 CONTINUE
C
      J1=NUMCHA+1
      N1=NUMCST
      J2=J1+(N1-1)
C
      IROW=NUMRP1
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO1570J=J1,J2
      I=I+1
      ICHA(J)=ISTRIN(I)
 1570 CONTINUE
 1579 CONTINUE
      ICURL2=ICURLP
      NUMLIN=NUMLP1
      NUMROW=NUMRP1
      NUMCHA=J2
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INLI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDINLI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDINLI')
 9011 FORMAT('***** AT THE END       OF EDINLI--')
      WRITE(ICOUT,9012)NCNEWL
      CALL EDWRST('EDINLI')
 9012 FORMAT('NCNEWL = ',I8)
      IF(NCNEWL.GE.1)THEN
         WRITE(ICOUT,9013)(ICNEWL(I:I),I=1,100)
         CALL EDWRST('EDINLI')
      ENDIF
 9013 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,9014)ICURL2
      CALL EDWRST('EDINLI')
 9014 FORMAT('ICURL2 = ',I8)
      WRITE(ICOUT,9021)NUMCST
      CALL EDWRST('EDINLI')
 9021 FORMAT('NUMCST = ',I8)
      WRITE(ICOUT,9022)(ISTRIN(I),I=1,100)
      CALL EDWRST('EDINLI')
 9022 FORMAT('(ISTRIN(I),I=1,100) = ',100A1)
      WRITE(ICOUT,9031)J1,N1,J2
      CALL EDWRST('EDINLI')
 9031 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9032)NUMCST,ITABC,ITABM1
      CALL EDWRST('EDINLI')
 9032 FORMAT('NUMCST,ITABC,ITABM1 = ',3I8)
      WRITE(ICOUT,9033)ICURL2,NUMLIN
      CALL EDWRST('EDINLI')
 9033 FORMAT('ICURL2,NUMLIN = ',2I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDINLI')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDINPU
C
C     PURPOSE--CHANGE TO INPUT MODE (FROM EDIT MODE)
C
C     DATE--JANUARY 24,1985
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      ISUBN1='EDIN'
      ISUBN2='PU  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INPU')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDINPU')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDINPU')
   51 FORMAT('*****AT THE BEGINNING OF EDINPU--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IMODE='INPU'
C
CCCCC IF(IFEESW.EQ.'OFF')GOTO1159
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1151)
         CALL EDWRST('EDINPU')
      ENDIF
 1151 FORMAT('YOU HAVE JUST ENTERED INPUT MODE')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1152)
         CALL EDWRST('EDINPU')
      ENDIF
 1152 FORMAT(10X,'YOU HAVE JUST ENTERED INPUT MODE')
 1159 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INPU')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDINPU')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDINPU')
 9011 FORMAT('*****AT THE END       OF EDINPU--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDINSE
C
C     PURPOSE--INSERT 1 LINE OF TEXT (AFTER CURRENT LINE).
C              THE STRING WHICH FOLLOWS THE INSERT COMMAND
C              WILL BE INSERTED.
C              IF NO STRING FOLLOWS THE INSERT COMMAND,
C              THEN A BLANK LINE (CONSISTING OF 1 BLANK CHARACTER)
C              WILL BE INSERTED.
C     COMMAND SYNTAX--INSERT <STRING>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUST
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDIN'
      ISUBN2='SE  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INSE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDINSE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDINSE')
   51 FORMAT('***** AT THE BEGINNING OF EDINSE--')
      WRITE(ICOUT,52)ITABC
      CALL EDWRST('EDINSE')
   52 FORMAT('ITABC = ',I8)
      WRITE(ICOUT,53)ICURLN,NUMLIN
      CALL EDWRST('EDINSE')
   53 FORMAT('ICURLN,NUMLIN = ',2I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **********************************
C               **  STEP 10--                  **
C               **  SAVE THE CURRENT POINTERS   **
C               **  (FOR POSSIBLE LATER USE     **
C               **  BY THE   UNDO     COMMAND)  **
C               **********************************
C
      ISTEPN='10'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDSACP
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE STRING TO BE INSERTED  **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INSE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUST)
C
      ITABM1=ITABC-1
      IF(ITABM1.LE.0)GOTO1290
C
      IF(NUMCST.LE.0)GOTO1190
      DO1100I=1,NUMCST
      IREV=NUMCST-I+1
      IREV2=IREV+ITABM1
      ISTRIN(IREV2)=ISTRIN(IREV)
 1100 CONTINUE
 1190 CONTINUE
C
      DO1200I=1,ITABM1
      ISTRIN(I)=' '
 1200 CONTINUE
C
 1290 CONTINUE
      NUMCST=NUMCST+ITABM1
C
C               ****************************
C               **  STEP 12--             **
C               **  INSERT     THE LINE.  **
C               ****************************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'INSE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH2=NUMCHA+NUMCST
      CALL EDCHSI(NUMCH2)
      IF(IERROR.EQ.'YES')GOTO9000
C
 1550 CONTINUE
      IF(ICURLN.GT.NUMLIN)ICURLN=NUMLIN
      ICURLP=ICURLN+1
      NUMLP1=NUMLIN+1
      NUMRP1=NUMROW+1
C
      IHOLD1=IPOINT(ICURLP)
      IPOINT(ICURLP)=NUMRP1
C
      IF(ICURLP.LE.0)GOTO1569
      IF(NUMLIN.LE.0)GOTO1569
      IF(ICURLP.GT.NUMLIN)GOTO1569
      DO1560ILINE=ICURLP,NUMLIN
      ILINEP=ILINE+1
      IHOLD2=IPOINT(ILINEP)
      IPOINT(ILINEP)=IHOLD1
      IHOLD1=IHOLD2
 1560 CONTINUE
 1569 CONTINUE
C
      J1=NUMCHA+1
      N1=NUMCST
      J2=J1+(N1-1)
C
      IROW=NUMRP1
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO1570J=J1,J2
      I=I+1
      ICHA(J)=ISTRIN(I)
 1570 CONTINUE
 1579 CONTINUE
      ICURLN=ICURLP
      NUMLIN=NUMLP1
      NUMROW=NUMRP1
      NUMCHA=J2
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INSE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDINSE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDINSE')
 9011 FORMAT('***** AT THE END       OF EDINSE--')
      WRITE(ICOUT,9012)IWIDTH
      CALL EDWRST('EDINSE')
 9012 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9013)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDINSE')
 9013 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9014)NUMCST
      CALL EDWRST('EDINSE')
 9014 FORMAT('NUMCST = ',I8)
      WRITE(ICOUT,9015)(ISTRIN(I),I=1,NUMCST)
      CALL EDWRST('EDINSE')
 9015 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDINSE')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)NUMCST,ITABC,ITABM1
      CALL EDWRST('EDINSE')
 9018 FORMAT('NUMCST,ITABC,ITABM1 = ',3I8)
      WRITE(ICOUT,9019)ICURLN,NUMLIN
      CALL EDWRST('EDINSE')
 9019 FORMAT('ICURLN,NUMLIN = ',2I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDINSE')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDJUST(ICNEWL,NCNEWL,ICOLL1,ICOLL2)
C
C     PURPOSE--TAKE THE TEXT IN COLUMNS ICOLL1 TO ICOLL2
C              OF ICNEWL AND MAKE IT HAVE SPECIFIED JUSTIFICATION
C              (AS DEFINED BY THE COMMON VARIABLE   IJUST)--
C                 LEFT (THE DEFAULT)
C                 BOTH
C                 RIGHT
C                 FORTRAN
C              THE 240-CHARACTER VARIABLE ICNEWL
C              CONSISTS OF NCNEWL CHARACTERS.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.1
C     ORIGINAL VERSION--MAY     1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ICNEWL
      CHARACTER*240 ICNEW2
      CHARACTER*240 ICWORD
      CHARACTER*4 IFOUWO
C
      CHARACTER*4 ICASFO
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDJU'
      ISUBN2='ST  '
C
      CALL EDZERO(ICNEW2,NCNEW2)
      NUMTRB=(-999)
      NUMWOR=(-999)
      NUMGAP=(-999)
      IRATIO=(-999)
      IREM=(-999)
      IREM2=(-999)
C
      ICASFO='-999'
      NUMBEB=(-999)
      ICNB=(-999)
      NUMCLA=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'JUST')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDJUST')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDJUST')
   51 FORMAT('***** AT THE BEGINNING OF EDJUST--')
      WRITE(ICOUT,52)IJUST
      CALL EDWRST('EDJUST')
   52 FORMAT('IJUST = ',A4)
      WRITE(ICOUT,53)NCNEWL,ICOLL1,ICOLL2
      CALL EDWRST('EDJUST')
   53 FORMAT('NCNEWL,ICOLL1,ICOLL2 = ',3I8)
      WRITE(ICOUT,54)(ICNEWL(I:I),I=1,100)
      CALL EDWRST('EDJUST')
   54 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
   90 CONTINUE
C
C               ***********************************************
C               **  STEP 10--                                **
C               **  BRANCH TO THE SPECIFIED CASE--           **
C               ***********************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IJUST.EQ.'LEFT')GOTO9000
      IF(IJUST.EQ.'RIGH')GOTO2000
      IF(IJUST.EQ.'BOTH')GOTO3000
      IF(IJUST.EQ.'FORT')GOTO4000
      GOTO9000
C
C               ***********************************************
C               **  STEP 20--                                **
C               **  TREAT THE RIGHT JUSTIFICATION CASE       **
C               ***********************************************
C
 2000 CONTINUE
C
      ISTEPN='20'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  DETERMINE THE NUMBER OF TRAILING BLANKS  **
C               ***********************************************
C
 2100 CONTINUE
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO2110I=ICOLL1,ICOLL2
      J=J+1
      IREV=ICOLL2-I+ICOLL1
      IF(ICNEWL(IREV:IREV).NE.' ')GOTO2120
 2110 CONTINUE
      GOTO9000
 2120 CONTINUE
      NUMTRB=J-1
      NUMBEG=(ICOLL2-ICOLL1+1)-NUMTRB
      IF(NUMTRB.LE.0)GOTO9000
C
C               *******************************************
C               **  STEP 22--                            **
C               **  FORM THE NEW LINE                    **
C               *******************************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDZERO(ICNEW2,NCNEW2)
      DO2210I=1,NUMBEG
      IP=I+NUMTRB
      ICNEW2(IP:IP)=ICNEWL(I:I)
 2210 CONTINUE
      NCNEW2=IP
C
C               ****************************************************
C               **  STEP 23--                                     **
C               **  COPY THE SECOND NEW LINE BACK INTO THE FIRST  **
C               ****************************************************
C
      ISTEPN='23'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      DO2300I=ICOLL1,ICOLL2
      ICNEWL(I:I)=ICNEW2(I:I)
 2300 CONTINUE
      NCNEWL=NCNEW2
      GOTO9000
C
C               ***********************************************
C               **  STEP 30--                                **
C               **  TREAT THE BOTH JUSTIFICATION CASE        **
C               ***********************************************
C
 3000 CONTINUE
C
      ISTEPN='30'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
C               ***********************************************
C               **  STEP 31--                                **
C               **  DETERMINE THE NUMBER OF TRAILING BLANKS  **
C               ***********************************************
C
 3100 CONTINUE
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO3110I=ICOLL1,ICOLL2
      J=J+1
      IREV=ICOLL2-I+ICOLL1
      IF(ICNEWL(IREV:IREV).NE.' ')GOTO3120
 3110 CONTINUE
      GOTO9000
 3120 CONTINUE
      NUMTRB=J-1
      IF(NUMTRB.LE.0)GOTO9000
C
C               *******************************************
C               **  STEP 32--                            **
C               **  DETERMINE THE NUMBER OF WORDS        **
C               *******************************************
C
      ISTEPN='32'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IC1=ICOLL1
      IC2=ICOLL2
      IC3=0
      IC4=IC2
C
      NUMWOR=0
      DO3200IW=1,NCNEWL
C
      IF(IW.GE.2)IC1=IC4+1
      IF(IC1.GT.IC2)GOTO3290
      CALL EDZERO(ICWORD,NCWORD)
      CALL EDEXWO(ICNEWL,NCNEWL,IC1,IC2,IC3,IC4,
     1ICWORD,NCWORD,IFOUWO)
      IF(IFOUWO.EQ.'NO')GOTO3290
      NUMWOR=NUMWOR+1
C
 3200 CONTINUE
 3290 CONTINUE
C
C               *******************************************
C               **  STEP 33--                            **
C               **  DETERMINE THE NUMBER OF GAPS         **
C               **  AND THE NUMBER OF ADDITIONAL         **
C               **  BLANKS PER GAP.                      **
C               *******************************************
C
      ISTEPN='33'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMGAP=NUMWOR-1
      IRATIO=NUMTRB/NUMGAP
      IREM=NUMTRB-IRATIO*NUMGAP
C
C               *******************************************
C               **  STEP 34--                            **
C               **  FORM THE NEW LINE                    **
C               *******************************************
C
      ISTEPN='34'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IREM2=IREM
      J=ICOLL1-1
C
      IC1=ICOLL1
      IC2=ICOLL2
      IC3=0
      IC4=IC2
C
      DO3400IW=1,NUMWOR
C
      IF(IW.GE.2)IC1=IC4+1
      IF(IC1.GT.IC2)GOTO3490
      CALL EDZERO(ICWORD,NCWORD)
      CALL EDEXWO(ICNEWL,NCNEWL,IC1,IC2,IC3,IC4,
     1ICWORD,NCWORD,IFOUWO)
      IF(IFOUWO.EQ.'NO')GOTO3490
C
      DO3410I=IC3,IC4
      J=J+1
      ICNEW2(J:J)=ICNEWL(I:I)
 3410 CONTINUE
C
      IF(IW.EQ.NUMWOR)GOTO3490
      IC4P=IC4+1
      IF(IC4P.GT.ICOLL2)GOTO3425
      DO3420I=IC4P,ICOLL2
      IF(ICNEWL(I:I).NE.' ')GOTO3425
      J=J+1
      ICNEW2(J:J)=ICNEWL(I:I)
 3420 CONTINUE
 3425 CONTINUE
C
      IF(IRATIO.LE.0)GOTO3439
      DO3430I=1,IRATIO
      J=J+1
      ICNEW2(J:J)=' '
 3430 CONTINUE
 3439 CONTINUE
C
      IF(IREM2.LE.0)GOTO3449
      J=J+1
      ICNEW2(J:J)=' '
      IREM2=IREM2-1
 3449 CONTINUE
C
 3400 CONTINUE
 3490 CONTINUE
      NCNEW2=J
C
C               ****************************************************
C               **  STEP 35--                                     **
C               **  COPY THE SECOND NEW LINE BACK INTO THE FIRST  **
C               ****************************************************
C
      ISTEPN='35'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      DO3500I=ICOLL1,ICOLL2
      ICNEWL(I:I)=ICNEW2(I:I)
 3500 CONTINUE
      NCNEWL=NCNEW2
      GOTO9000
C
C               ***********************************************
C               **  STEP 40--                                **
C               **  TREAT THE FORTRAN JUSTIFICATION CASE     **
C               ***********************************************
C
 4000 CONTINUE
C
      ISTEPN='40'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
C               ***********************************************
C               **  STEP 41--                                **
C               **  DETERMINE THE NUMBER OF LEADING BLANKS   **
C               ***********************************************
C
 4100 CONTINUE
C
      ISTEPN='41'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO4110I=ICOLL1,ICOLL2
      J=J+1
      IF(ICNEWL(I:I).NE.' ')GOTO4120
 4110 CONTINUE
      GOTO9000
 4120 CONTINUE
      NUMBEB=J-1
C
C               *******************************************
C               **  STEP 42--                            **
C               **  DETERMINE THE NATURE OF THE LINE--   **
C               **     1) IS IT A USUAL FORTRAN LINE?
C               **     2) IS IT A COMMENT LINE?
C               **     3) IS IT A STATEMENT LABEL?
C               **     4) IS IT A CONTINUATION LINE?
C               *******************************************
C
      ISTEPN='42'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICNB=NUMBEB+1
      ICNBP1=ICNB+1
      ICNBP4=ICNB+4
C
      ICASFO='REGU'
C
      IF(NCNEWL.LE.ICNB.AND.ICNEWL(ICNB:ICNB).EQ.'C')ICASFO='COMM'
      IF(NCNEWL.GT.ICNB.AND.ICNEWL(ICNB:ICNB).EQ.'C'.AND.
     1   ICNEWL(ICNBP1:ICNBP1).EQ.' ')ICASFO='COMM'
      IF(NCNEWL.GT.ICNB.AND.ICNEWL(ICNB:ICNB).EQ.'C'.AND.
     1   ICNEWL(ICNBP1:ICNBP1).EQ.'-')ICASFO='COMM'
      IF(NCNEWL.GT.ICNB.AND.ICNEWL(ICNB:ICNB).EQ.'C'.AND.
     1   ICNEWL(ICNBP1:ICNBP4).EQ.'CCCC')ICASFO='COMM'
C
      IF(ICNEWL(ICNB:ICNB).EQ.'&')ICASFO='CONT'
C
      IF(ICNEWL(ICNB:ICNB).EQ.'1')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'2')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'3')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'4')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'5')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'6')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'7')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'8')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'9')ICASFO='LABE'
      IF(ICNEWL(ICNB:ICNB).EQ.'0')ICASFO='LABE'
C
      IF(ICASFO.EQ.'LABE')GOTO4210
      GOTO4290
 4210 CONTINUE
      J=0
      DO4211I=ICNB,ICOLL2
      J=J+1
      IF(ICNEWL(I:I).EQ.' ')GOTO4212
 4211 CONTINUE
      NUMCLA=J
      GOTO4290
 4212 CONTINUE
      NUMCLA=J-1
      GOTO4290
 4290 CONTINUE
C
C               *******************************************
C               **  STEP 43--                            **
C               **  FORM THE NEW LINE                    **
C               *******************************************
C
      ISTEPN='43'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDZERO(ICNEW2,NCNEW2)
      IMIN=NUMBEB+1
      IMAX=ICOLL2
      IF(IMIN.GT.IMAX)GOTO9000
      DO4310I=IMIN,IMAX
      IP=I+7-1
      IF(ICASFO.EQ.'COMM'.AND.I.EQ.1)IP=I+1-1
      IF(ICASFO.EQ.'COMM'.AND.I.GE.2)IP=I+5-1
      IF(ICASFO.EQ.'COMM'.AND.I.GE.2.AND.ICNEWL(2:5).EQ.'----')IP=I+1-1
      IF(ICASFO.EQ.'COMM'.AND.I.GE.2.AND.ICNEWL(2:5).EQ.'CCCC')IP=I+1-1
      IF(ICASFO.EQ.'CONT')IP=I+6-1
      IF(ICASFO.EQ.'LABE')IP=I+(6-NUMCLA)-1
      IF(IP.GT.ICOLL2)GOTO4320
      ICNEW2(IP:IP)=ICNEWL(I:I)
 4310 CONTINUE
      NCNEW2=IP
      GOTO4390
C
 4320 CONTINUE
      NCNEW2=IP-1
      GOTO4390
C
 4390 CONTINUE
C
C               ****************************************************
C               **  STEP 44--                                     **
C               **  COPY THE SECOND NEW LINE BACK INTO THE FIRST  **
C               ****************************************************
C
      ISTEPN='44'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'JUST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      DO4400I=ICOLL1,ICOLL2
      ICNEWL(I:I)=ICNEW2(I:I)
 4400 CONTINUE
      NCNEWL=NCNEW2
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'JUST')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDJUST')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDJUST')
 9011 FORMAT('***** AT THE END       OF EDJUST--')
      WRITE(ICOUT,9012)IJUST
      CALL EDWRST('EDJUST')
 9012 FORMAT('IJUST = ',A4)
      WRITE(ICOUT,9013)NCNEWL,ICOLL1,ICOLL2
      CALL EDWRST('EDJUST')
 9013 FORMAT('NCNEWL,ICOLL1,ICOLL2 = ',3I8)
      WRITE(ICOUT,9014)(ICNEWL(I:I),I=1,100)
      CALL EDWRST('EDJUST')
 9014 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
      WRITE(ICOUT,9015)NUMTRB,NUMWOR,NUMGAP,IRATIO,IREM,IREM2
      CALL EDWRST('EDJUST')
 9015 FORMAT('NUMTRB,NUMWOR,NUMGAP,IRATIO,IREM,IREM2 = ',6I8)
      WRITE(ICOUT,9016)ICASFO,NUMBEB,ICNB,NUMCLA
      CALL EDWRST('EDJUST')
 9016 FORMAT('ICASFO,NUMBEB,ICNB,NUMCLA = ',A4,3I8)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLAST
C
C     PURPOSE--GO TO THE LAST LINE OF THE FILE
C
C     NOTE--NO ARGUMENTS ARE EXPECTED OR PROCESSED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLA'
      ISUBN2='ST  '
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LAST')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLASTX')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLASTX')
   51 FORMAT('***** AT THE BEGINNING OF EDLAST--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************
C               **  STEP 1--           **
C               **  GO TO LINE 1       **
C               **  AND PRINT IT OUT.  **
C               *************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LAST')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=NUMLIN
      IF(ILINE.LE.0)GOTO1150
C
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(IFEESW.EQ.'OFF')GOTO1129
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1121)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLASTX')
      ENDIF
 1121 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1122)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLASTX')
      ENDIF
 1122 FORMAT(I6,':',3X,230A1)
 1129 CONTINUE
      GOTO1190
C
 1150 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1159
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1151)
         CALL EDWRST('EDLASTX')
      ENDIF
 1151 FORMAT('THE FILE IS EMPTY--THERE IS NO LAST LINE.')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1152)
         CALL EDWRST('EDLASTX')
      ENDIF
 1152 FORMAT(10X,'THE FILE IS EMPTY--THERE IS NO LAST LINE.')
 1159 CONTINUE
      GOTO1190
C
 1190 CONTINUE
C
      ICURLN=ILINE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LAST')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLASTX')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLASTX')
 9011 FORMAT('***** AT THE END       OF EDLAST--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLB
C
C     PURPOSE--LOCATE (BACKWARDS) THE PREVIOUS OCCURRANCE OF A STRING
C              (START THE SEARCH WITH THE PREVIOUS LINE)
C     NOTE--
C          LB ABC    WILL LOCATE BACKWARDS    ABC   IF IN ANY COLUMN
C     COMMAND SYNTAX--LB <STRING>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.6
C     ORIGINAL VERSION--JUNE 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUST
      CHARACTER*4 IHIT
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLB'
      ISUBN2='    '
C
      IFOUND='YES'
      IERROR='NO'
C
      IHIT='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOCA')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLB  ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLB  ')
   51 FORMAT('***** AT THE BEGINNING OF EDLB--')
      WRITE(ICOUT,52)IALL
      CALL EDWRST('EDLB  ')
   52 FORMAT('IALL = ',A4)
      WRITE(ICOUT,54)NUMARG,IWIDTH
      CALL EDWRST('EDLB  ')
   54 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,55)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDLB  ')
      ENDIF
   55 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,56)ICURLN,NUMLIN,NUMCLS
      CALL EDWRST('EDLB  ')
   56 FORMAT('ICURLN,NUMLIN,NUMCLS = ',3I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE STRING TO BE FOUND     **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOCA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUST)
      IF(IFOUST.EQ.'YES')GOTO1190
C
      NUMCST=NUMCLS
      IF(NUMCLS.LE.0)GOTO1159
      DO1155I=1,NUMCLS
      ISTRIN(I)=ILOCST(I)
 1155 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1190 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  IF THE STRING IS NEW,           **
C               **  COPY THE STRING INTO ILOCST(.)  **
C               **  FOR FUTURE USE BY SUBSEQUENT    **
C               **  LOCATE COMMANDS.                **
C               **************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOCA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFOUST.EQ.'NO')GOTO1290
      NUMCLS=NUMCST
      IF(NUMCST.LE.0)GOTO1290
      DO1210I=1,NUMCST
      ILOCST(I)=ISTRIN(I)
 1210 CONTINUE
 1290 CONTINUE
C
C               ************************************
C               **  STEP 3--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE SCANNED.   **
C               ************************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOCA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLM=ICURLN-1
C
      ISTART=1
      ISTOP=ICURLM
C
C               ****************************
C               **  STEP 4--              **
C               **  SCAN      THE LINES.  **
C               ****************************
C
      ISTEPN='4'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOCA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTOP.LE.0)ISTOP=1
      IF(ISTART.GT.ISTOP)GOTO1550
C
      DO1510ILINE=ISTART,ISTOP
      ILINRV=ISTOP-ILINE+ISTART
      IROW=IPOINT(ILINRV)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDLOC2(ISTRIN,NUMCST,J1,J2,IHIT)
      IF(IHIT.EQ.'YES')GOTO1520
      GOTO1510
C
 1520 CONTINUE
      ICURLN=ILINRV
      IF(IFEESW.EQ.'OFF')GOTO1529
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLB  ')
      ENDIF
 1521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1522)ILINRV,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLB  ')
      ENDIF
 1522 FORMAT(I6,':',3X,230A1)
 1529 CONTINUE
      IF(IALL.EQ.'ON')GOTO1510
      GOTO1590
C
 1510 CONTINUE
C
 1550 CONTINUE
      ICURLN=0
      IF(IFEESW.EQ.'OFF')GOTO1559
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1551)
         CALL EDWRST('EDLB  ')
      ENDIF
 1551 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1552)
         CALL EDWRST('EDLB  ')
      ENDIF
 1552 FORMAT(10X,'[TOP]')
 1559 CONTINUE
C
 1590 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOCA')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLB  ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLB  ')
 9011 FORMAT('***** AT THE END       OF EDLB--')
      WRITE(ICOUT,9015)NUMARG,IWIDTH
      CALL EDWRST('EDLB  ')
 9015 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,9016)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDLB  ')
      ENDIF
 9016 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDLB  ')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)J1,J2,IHIT,ICURLN
      CALL EDWRST('EDLB  ')
 9018 FORMAT('J1,J2,IHIT,ICURLN = ',2I8,2X,A4,I8)
      WRITE(ICOUT,9019)IALL,IFOUST
      CALL EDWRST('EDLB  ')
 9019 FORMAT('IALL,IFOUST = ',A4,2X,A4)
      WRITE(ICOUT,9020)ICURLN,NUMLIN,NUMCLS
      CALL EDWRST('EDLB  ')
 9020 FORMAT('ICURLN,NUMLIN,NUMCLS = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLB  ')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLIME
C
C     PURPOSE--LIST THE CONTENTS OF THE MESSAGE FILE
C              (THIS IS DONE AT SIGN-ON TO THE EDITOR).
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JUNE 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*80 IREC
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLI'
      ISUBN2='ME  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LIME')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIME')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLIME')
   51 FORMAT('***** AT THE BEGINNING OF EDLIME--')
      WRITE(ICOUT,52)IBUGE2,ISUBRO
      CALL EDWRST('EDLIME')
   52 FORMAT('IBUGE2,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,53)IMESNU
      CALL EDWRST('EDLIME')
   53 FORMAT('IMESNU = ',I8)
      WRITE(ICOUT,54)IMESNA
      CALL EDWRST('EDLIME')
   54 FORMAT('IMESNA = ',A80)
      WRITE(ICOUT,55)IMESST
      CALL EDWRST('EDLIME')
   55 FORMAT('IMESST = ',A12)
      WRITE(ICOUT,56)IMESFO
      CALL EDWRST('EDLIME')
   56 FORMAT('IMESFO = ',A12)
      WRITE(ICOUT,57)IMESAC
      CALL EDWRST('EDLIME')
   57 FORMAT('IMESAC = ',A12)
      WRITE(ICOUT,58)IMESRW
      CALL EDWRST('EDLIME')
   58 FORMAT('IMESRW = ',A12)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIME')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE NAME OF THE FILE       **
C               **  THAT THE IS TO BE LISTED             **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LIME')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IFILE=IMESNA
      ISTAT=IMESST
      IOUNIT=IMESNU
      IFORM=IMESFO
      IACCES=IMESAC
      IREWR=IMESRW
      ISUBN0='LIME'
      IERRFI='NO'
C
C               *********************
C               **  STEP 13--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='13'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LIME')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO9000
      REWIND IOUNIT
C
C               ******************************
C               **  STEP 21--               **
C               **  READ IN A FILE AND      **
C               **  WRITE IT OUT (LIST IT)  **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LIME')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICOUNT=0
      DO2100IDUMMY=1,100000
C
      NUMCRE=80
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1993
      IREC=' '
      READ(IOUNIT,2105,END=2190)IREC
 2105 FORMAT(A80)
      ICOUNT=ICOUNT+1
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2106)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLIME')
 2106 FORMAT('IREC = ',A80)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2107)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLIME')
 2107 FORMAT('NUMCRE = ',I8)
C
      DO2200I=1,NUMCRE
      IREV=NUMCRE-I+1
      IF(IREC(IREV:IREV).NE.' ')GOTO2290
 2200 CONTINUE
      IREV=0
 2290 CONTINUE
      NUMCRE=IREV
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2296)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLIME')
 2296 FORMAT('IREC = ',A80)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2297)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLIME')
 2297 FORMAT('NUMCRE = ',I8)
C
      WRITE(ICOUT,2311)(IREC(I:I),I=1,NUMCRE)
      CALL EDWRST('EDLIME')
 2311 FORMAT(230A1)
C
 2100 CONTINUE
 2190 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC IF(ICOUNT.LE.0)WRITE(ICOUT,2191)
CCCCC CALL EDWRST('EDLIME')
C2191 FORMAT('[THE FILE IS EMPTY]')
C
C               **********************
C               **  STEP 31--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LIME')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LIME')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIME')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLIME')
 9011 FORMAT('***** AT THE END       OF EDLIME--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDLIME')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDLIME')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDLIME')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)ICOUNT
      CALL EDWRST('EDLIME')
 9016 FORMAT('ICOUNT = ',I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIME')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLINE
C
C     PURPOSE--LIST THE CONTENTS OF THE NEWS FILE
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JUNE 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*80 IREC
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLI'
      ISUBN2='ME  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LIME')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLINE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLINE')
   51 FORMAT('***** AT THE BEGINNING OF EDLINE--')
      WRITE(ICOUT,52)IBUGE2,ISUBRO
      CALL EDWRST('EDLINE')
   52 FORMAT('IBUGE2,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,53)INEWNU
      CALL EDWRST('EDLINE')
   53 FORMAT('INEWNU = ',I8)
      WRITE(ICOUT,54)INEWNA
      CALL EDWRST('EDLINE')
   54 FORMAT('INEWNA = ',A80)
      WRITE(ICOUT,55)INEWST
      CALL EDWRST('EDLINE')
   55 FORMAT('INEWST = ',A12)
      WRITE(ICOUT,56)INEWFO
      CALL EDWRST('EDLINE')
   56 FORMAT('INEWFO = ',A12)
      WRITE(ICOUT,57)INEWAC
      CALL EDWRST('EDLINE')
   57 FORMAT('INEWAC = ',A12)
      WRITE(ICOUT,58)INEWRW
      CALL EDWRST('EDLINE')
   58 FORMAT('INEWRW = ',A12)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLINE')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE NAME OF THE FILE       **
C               **  THAT THE IS TO BE LISTED             **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LIME')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IFILE=INEWNA
      ISTAT=INEWST
      IOUNIT=INEWNU
      IFORM=INEWFO
      IACCES=INEWAC
      IREWR=INEWRW
      ISUBN0='LIME'
      IERRFI='NO'
C
C               *********************
C               **  STEP 13--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='13'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LIME')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO9000
      REWIND IOUNIT
C
C               ******************************
C               **  STEP 21--               **
C               **  READ IN A FILE AND      **
C               **  WRITE IT OUT (LIST IT)  **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LIME')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICOUNT=0
      DO2100IDUMMY=1,100000
C
      NUMCRE=80
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1993
      IREC=' '
      READ(IOUNIT,2105,END=2190)IREC
 2105 FORMAT(A80)
      ICOUNT=ICOUNT+1
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2106)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLINE')
 2106 FORMAT('IREC = ',A80)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2107)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLINE')
 2107 FORMAT('NUMCRE = ',I8)
C
      DO2200I=1,NUMCRE
      IREV=NUMCRE-I+1
      IF(IREC(IREV:IREV).NE.' ')GOTO2290
 2200 CONTINUE
      IREV=0
 2290 CONTINUE
      NUMCRE=IREV
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2296)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLINE')
 2296 FORMAT('IREC = ',A80)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2297)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLINE')
 2297 FORMAT('NUMCRE = ',I8)
C
      WRITE(ICOUT,2311)(IREC(I:I),I=1,NUMCRE)
      CALL EDWRST('EDLINE')
 2311 FORMAT(230A1)
C
 2100 CONTINUE
 2190 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC IF(ICOUNT.LE.0)WRITE(ICOUT,2191)
CCCCC CALL EDWRST('EDLINE')
C2191 FORMAT('[THE FILE IS EMPTY]')
C
C               **********************
C               **  STEP 31--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LIME')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LIME')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLINE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLINE')
 9011 FORMAT('***** AT THE END       OF EDLINE--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDLINE')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDLINE')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDLINE')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)ICOUNT
      CALL EDWRST('EDLINE')
 9016 FORMAT('ICOUNT = ',I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLINE')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLIST
C
C     PURPOSE--LIST (BUT DO NOT INSERT OR EXECUTE)
C              THE CONTENTS OF A FILE
C     NOTE--IF NO ARGUMENTS GIVEN,
C           THE THE DEFAULT LIST FILE WILL BE LISTED.
C         --IF 1 ARGUMENT GIVEN (PRESUMEDLY A FILE NAME),
C           THEN THAT FILE WILL BE LISTED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*80 IREC
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUEX
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLI'
      ISUBN2='ST  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ADD')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIST')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLIST')
   51 FORMAT('***** AT THE BEGINNING OF EDLIST--')
      WRITE(ICOUT,53)ILISNU
      CALL EDWRST('EDLIST')
   53 FORMAT('ILISNU = ',I8)
      WRITE(ICOUT,54)ILISNA
      CALL EDWRST('EDLIST')
   54 FORMAT('ILISNA = ',A80)
      WRITE(ICOUT,55)ILISST
      CALL EDWRST('EDLIST')
   55 FORMAT('ILISST = ',A12)
      WRITE(ICOUT,56)ILISFO
      CALL EDWRST('EDLIST')
   56 FORMAT('ILISFO = ',A12)
      WRITE(ICOUT,57)ILISAC
      CALL EDWRST('EDLIST')
   57 FORMAT('ILISAC = ',A12)
      WRITE(ICOUT,58)ILISRW
      CALL EDWRST('EDLIST')
   58 FORMAT('ILISRW = ',A12)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIST')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE NAME OF THE FILE       **
C               **  THAT THE IS TO BE LISTED             **
C               **  (IF NO NAME IS GIVEN,                **
C               **  THEN USE A DEFAULT FILE NAME).       **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUEX)
      IF(IFOUEX.EQ.'YES')GOTO1110
      GOTO1120
C
 1110 CONTINUE
      IFILE=' '
      IF(NUMCST.LE.0)GOTO1119
      DO1111I=1,NUMCST
      IFILE(I:I)=ISTRIN(I)
 1111 CONTINUE
 1119 CONTINUE
      ISTAT='UNKNOWN'
      GOTO1190
C
 1120 CONTINUE
      IFILE=ILISNA
      ISTAT=ILISST
      GOTO1190
C
 1190 CONTINUE
      IOUNIT=ILISNU
      IFORM=ILISFO
      IACCES=ILISAC
      IREWR=ILISRW
      ISUBN0='LIST'
      IERRFI='NO'
C
C               ****************************
C               **  STEP 12--             **
C               **  WRITE OUT A MESSAGE   **
C               **  IDENTIFYING THE FILE  **
C               ****************************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIST')
      WRITE(ICOUT,1205)IFILE
      CALL EDWRST('EDLIST')
 1205 FORMAT(10X,'FILE = ',A80)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIST')
C               *********************
C               **  STEP 13--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='13'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO9000
      REWIND IOUNIT
C
C               ******************************
C               **  STEP 21--               **
C               **  READ IN A FILE AND      **
C               **  WRITE IT OUT (LIST IT)  **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICOUNT=0
      DO2100IDUMMY=1,100000
C
      NUMCRE=80
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1993
      IREC=' '
      READ(IOUNIT,2105,END=2190)IREC
 2105 FORMAT(A80)
      ICOUNT=ICOUNT+1
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2106)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLIST')
 2106 FORMAT('IREC = ',A80)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2107)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLIST')
 2107 FORMAT('NUMCRE = ',I8)
C
      DO2200I=1,NUMCRE
      IREV=NUMCRE-I+1
      IF(IREC(IREV:IREV).NE.' ')GOTO2290
 2200 CONTINUE
      IREV=0
 2290 CONTINUE
      NUMCRE=IREV
C
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2296)IREC
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLIST')
 2296 FORMAT('IREC = ',A80)
      IF(IBUGE2.EQ.'ON')WRITE(ICOUT,2297)NUMCRE
      IF(IBUGE2.EQ.'ON')CALL EDWRST('EDLIST')
 2297 FORMAT('NUMCRE = ',I8)
C
      WRITE(ICOUT,2311)ICOUNT,(IREC(I:I),I=1,NUMCRE)
      CALL EDWRST('EDLIST')
 2311 FORMAT(I6,':',3X,230A1)
C
 2100 CONTINUE
 2190 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIST')
      IF(ICOUNT.LE.0)WRITE(ICOUT,2191)
      IF(ICOUNT.LE.0)CALL EDWRST('EDLIST')
 2191 FORMAT('[THE FILE IS EMPTY]')
C
C               **********************
C               **  STEP 31--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ADD')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIST')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLIST')
 9011 FORMAT('***** AT THE END       OF EDLIST--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDLIST')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDLIST')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDLIST')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)ICOUNT
      CALL EDWRST('EDLIST')
 9016 FORMAT('ICOUNT = ',I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLIST')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLOBL
C
C     PURPOSE--LOCATE THE NEXT OCCURRANCE OF A BLANK
C              (START THE SEARCH WITH THE NEXT LINE)
C     NOTE--
C     COMMAND SYNTAX--LBL
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.3
C     ORIGINAL VERSION--FEBRUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHIT
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLO'
      ISUBN2='BL  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IHIT='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOBL')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOBL')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLOBL')
   51 FORMAT('***** AT THE BEGINNING OF EDLOBL--')
      WRITE(ICOUT,52)IALL
      CALL EDWRST('EDLOBL')
   52 FORMAT('IALL = ',A4)
      WRITE(ICOUT,54)NUMARG,IWIDTH
      CALL EDWRST('EDLOBL')
   54 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,55)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDLOBL')
      ENDIF
   55 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,56)ICURLN,NUMLIN,NUMCLS
      CALL EDWRST('EDLOBL')
   56 FORMAT('ICURLN,NUMLIN,NUMCLS = ',3I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 3--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE SCANNED.   **
C               ************************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOBL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLP=ICURLN+1
C
      ISTART=ICURLP
      ISTOP=NUMLIN
C
C               ****************************
C               **  STEP 4--              **
C               **  SCAN      THE LINES.  **
C               ****************************
C
      ISTEPN='4'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOBL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTART.GT.ISTOP)GOTO1550
C
      DO1510ILINE=ISTART,ISTOP
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDLOB2(J1,J2,IHIT)
      IF(IHIT.EQ.'YES')GOTO1520
      GOTO1510
C
 1520 CONTINUE
      ICURLN=ILINE
      IF(IFEESW.EQ.'OFF')GOTO1529
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLOBL')
      ENDIF
 1521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1522)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLOBL')
      ENDIF
 1522 FORMAT(I6,':',3X,230A1)
 1529 CONTINUE
      IF(IALL.EQ.'ON')GOTO1510
      GOTO1590
C
 1510 CONTINUE
C
 1550 CONTINUE
      ICURLN=NUMLIN+1
      IF(IFEESW.EQ.'OFF')GOTO1559
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1551)
         CALL EDWRST('EDLOBL')
      ENDIF
 1551 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1552)
         CALL EDWRST('EDLOBL')
      ENDIF
 1552 FORMAT(10X,'[BOTTOM]')
 1559 CONTINUE
C
 1590 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOBL')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOBL')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLOBL')
 9011 FORMAT('***** AT THE END       OF EDLOBL--')
      WRITE(ICOUT,9015)NUMARG,IWIDTH
      CALL EDWRST('EDLOBL')
 9015 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,9016)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDLOBL')
      ENDIF
 9016 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDLOBL')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)J1,J2,IHIT,ICURLN
      CALL EDWRST('EDLOBL')
 9018 FORMAT('J1,J2,IHIT,ICURLN = ',2I8,2X,A4,I8)
      WRITE(ICOUT,9019)IALL,IFOUND
      CALL EDWRST('EDLOBL')
 9019 FORMAT('IALL,IFOUND = ',A4,2X,A4)
      WRITE(ICOUT,9020)ICURLN,NUMLIN,NUMCLS
      CALL EDWRST('EDLOBL')
 9020 FORMAT('ICURLN,NUMLIN,NUMCLS = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOBL')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLOB2(J1,J2,IHIT)
C
C     PURPOSE--SCAN ICHA(.) BETWEEN
C              ELEMENTS J1 AND J2 (INCLUSIVE)
C              AND DETERMINE IF ALL BLANK.
C              IF SO, THEN SET IHIT TO 'YES'
C              IF NO, THEN SET IHIT TO 'NO'
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHIT
C
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLO'
      ISUBN2='B2  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOB2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOB2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLOB2')
   51 FORMAT('***** AT THE BEGINNING OF EDLOB2--')
      WRITE(ICOUT,56)J1,J2
      CALL EDWRST('EDLOB2')
   56 FORMAT('J1,J2 = ',2I8)
      WRITE(ICOUT,57)(ICHA(J),J=J1,J2)
      CALL EDWRST('EDLOB2')
   57 FORMAT('(ICHA(J),J=J1,J2) = ',100A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOB2')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IHIT='NO'
C
      K1=(J1-1)+ILLIM1
      K2=(J1-1)+ILLIM2
      IF(K1.LT.J1.OR.K1.GT.J2)K1=J1
      IF(K2.LT.J1.OR.K2.GT.J2)K2=J2
      IF(K1.GT.K2)GOTO1110
      DO1100J=K1,K2
      IF(ICHA(J).EQ.' ')GOTO1100
      GOTO1120
 1100 CONTINUE
 1110 CONTINUE
      IHIT='YES'
      GOTO9000
 1120 CONTINUE
      IHIT='NO'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOB2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOB2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLOB2')
 9011 FORMAT('***** AT THE END       OF EDLOB2--')
      WRITE(ICOUT,9016)J1,J2,ILLIM1,ILLIM2,K1,K2
      CALL EDWRST('EDLOB2')
 9016 FORMAT('J1,J2,ILLIM1,ILLIM2,K1,K2 = ',6I8)
      WRITE(ICOUT,9017)(ICHA(J),J=J1,J2)
      CALL EDWRST('EDLOB2')
 9017 FORMAT('(ICHA(J),J=J1,J2) = ',100A1)
      WRITE(ICOUT,9018)IHIT
      CALL EDWRST('EDLOB2')
 9018 FORMAT('IHIT = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOB2')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLOCA
C
C     PURPOSE--LOCATE THE NEXT OCCURRANCE OF A STRING
C              (START THE SEARCH WITH THE NEXT LINE)
C     NOTE--
C          LOCATE ABC WILL LOCATE ABC IF IN ANY COLUMN
C          FIND ABC WILL LOCATE ABC ONLY IF IN COLUMNS 1, 2, AND 3
C     COMMAND SYNTAX--LOCATE <STRING>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUST
      CHARACTER*4 IHIT
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLO'
      ISUBN2='CA  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IHIT='NO'
      LOCATE='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOCA')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOCA')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLOCA')
   51 FORMAT('***** AT THE BEGINNING OF EDLOCA--')
      WRITE(ICOUT,52)IALL
      CALL EDWRST('EDLOCA')
   52 FORMAT('IALL = ',A4)
      WRITE(ICOUT,54)NUMARG,IWIDTH
      CALL EDWRST('EDLOCA')
   54 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,55)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDLOCA')
      ENDIF
   55 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,56)ICURLN,NUMLIN,NUMCLS
      CALL EDWRST('EDLOCA')
   56 FORMAT('ICURLN,NUMLIN,NUMCLS = ',3I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE STRING TO BE FOUND     **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOCA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUST)
      LOCATE=IFOUST
      IF(IFOUST.EQ.'YES')GOTO1190
C
      NUMCST=NUMCLS
      IF(NUMCLS.LE.0)GOTO1159
      DO1155I=1,NUMCLS
      ISTRIN(I)=ILOCST(I)
 1155 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1190 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  IF THE STRING IS NEW,           **
C               **  COPY THE STRING INTO ILOCST(.)  **
C               **  FOR FUTURE USE BY SUBSEQUENT    **
C               **  LOCATE COMMANDS.                **
C               **************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOCA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFOUST.EQ.'NO')GOTO1290
      NUMCLS=NUMCST
      IF(NUMCST.LE.0)GOTO1290
      DO1210I=1,NUMCST
      ILOCST(I)=ISTRIN(I)
 1210 CONTINUE
 1290 CONTINUE
C
C               ************************************
C               **  STEP 3--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE SCANNED.   **
C               ************************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOCA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLP=ICURLN+1
C
      ISTART=ICURLP
      ISTOP=NUMLIN
C
C               ****************************
C               **  STEP 4--              **
C               **  SCAN      THE LINES.  **
C               ****************************
C
      ISTEPN='4'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOCA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTART.GT.ISTOP)GOTO1550
C
      DO1510ILINE=ISTART,ISTOP
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDLOC2(ISTRIN,NUMCST,J1,J2,IHIT)
CCCCC THE FOLLOWING LINE WAS ADDED MAY 1993
      LOCATE=IHIT
      IF(IHIT.EQ.'YES')GOTO1520
      GOTO1510
C
 1520 CONTINUE
      ICURLN=ILINE
      IF(IFEESW.EQ.'OFF')GOTO1529
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLOCA')
      ENDIF
 1521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1522)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLOCA')
      ENDIF
 1522 FORMAT(I6,':',3X,230A1)
 1529 CONTINUE
      IF(IALL.EQ.'ON')GOTO1510
      GOTO1590
C
 1510 CONTINUE
C
 1550 CONTINUE
      ICURLN=NUMLIN+1
      IF(IFEESW.EQ.'OFF')GOTO1559
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1551)
         CALL EDWRST('EDLOCA')
      ENDIF
 1551 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1552)
         CALL EDWRST('EDLOCA')
      ENDIF
 1552 FORMAT(10X,'[BOTTOM]')
 1559 CONTINUE
C
 1590 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOCA')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOCA')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLOCA')
 9011 FORMAT('***** AT THE END       OF EDLOCA--')
      WRITE(ICOUT,9015)NUMARG,IWIDTH
      CALL EDWRST('EDLOCA')
 9015 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,9016)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDLOCA')
      ENDIF
 9016 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDLOCA')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)J1,J2,IHIT,LOCATE,ICURLN
      CALL EDWRST('EDLOCA')
 9018 FORMAT('J1,J2,IHIT,ICURLN = ',2I8,2X,A4,2X,A4,I8)
      WRITE(ICOUT,9019)IALL,IFOUST,LOCATE
      CALL EDWRST('EDLOCA')
 9019 FORMAT('IALL,IFOUST,LOCATE = ',A4,2X,A4,2X,A4)
      WRITE(ICOUT,9020)ICURLN,NUMLIN,NUMCLS
      CALL EDWRST('EDLOCA')
 9020 FORMAT('ICURLN,NUMLIN,NUMCLS = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOCA')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLOC2(ISTRIN,NUMCST,J1,J2,IHIT)
C
C     PURPOSE--SCAN ICHA(.) BETWEEN
C              ELEMENTS J1 AND J2 (INCLUSIVE)
C              AND DETERMINE IF THE STRING IN ISTRIN(.)
C              IS CONTAINED THERIN.
C              IF SO, THEN SET IHIT TO 'YES'
C              IF NO, THEN SET IHIT TO 'NO'
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IHIT
      CHARACTER*1 ICHAUC
C
      DIMENSION ISTRIN(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLO'
      ISUBN2='C2  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOC2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOC2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLOC2')
   51 FORMAT('***** AT THE BEGINNING OF EDLOC2--')
      WRITE(ICOUT,54)NUMCST
      CALL EDWRST('EDLOC2')
   54 FORMAT('NUMCST = ',I8)
      IF(NUMCST.GE.1)THEN
         WRITE(ICOUT,55)(ISTRIN(I),I=1,NUMCST)
         CALL EDWRST('EDLOC2')
      ENDIF
   55 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,56)J1,J2
      CALL EDWRST('EDLOC2')
   56 FORMAT('J1,J2 = ',2I8)
      WRITE(ICOUT,57)(ICHA(J),J=J1,J2)
      CALL EDWRST('EDLOC2')
   57 FORMAT('(ICHA(J),J=J1,J2) = ',100A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOC2')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IHIT='NO'
C
      IF(J1.GT.J2)GOTO9000
      IF(NUMCST.LE.0)IHIT='YES'
      IF(NUMCST.LE.0)GOTO9000
C
      K1=(J1-1)+ILLIM1
      K2=(J1-1)+ILLIM2
      IF(K1.LT.J1.OR.K1.GT.J2)K1=J1
      IF(K2.LT.J1.OR.K2.GT.J2)K2=J2
CCCCC DO1100J=J1,J2
      DO1100J=K1,K2
      DO1200I=1,NUMCST
      JPIM1=J+(I-1)
      IF(ISTRIN(I).EQ.IMASK)GOTO1200
      IF(ICHA(JPIM1).EQ.ISTRIN(I))GOTO1200
      CALL EDUPP1(ICHA(JPIM1),ICHAUC)
      IF(ICHAUC.EQ.ISTRIN(I))GOTO1200
      CALL EDLOW1(ICHA(JPIM1),ICHAUC)
      IF(ICHAUC.EQ.ISTRIN(I))GOTO1200
      GOTO1100
 1200 CONTINUE
      IHIT='YES'
      GOTO9000
 1100 CONTINUE
      IHIT='NO'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOC2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOC2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLOC2')
 9011 FORMAT('***** AT THE END       OF EDLOC2--')
      WRITE(ICOUT,9014)NUMCST
      CALL EDWRST('EDLOC2')
 9014 FORMAT('NUMCST = ',I8)
      IF(NUMCST.GE.1)THEN
         WRITE(ICOUT,9015)(ISTRIN(I),I=1,NUMCST)
         CALL EDWRST('EDLOC2')
      ENDIF
 9015 FORMAT('(ISTRIN(I),I=1,NUMCST) = ',110A1)
      WRITE(ICOUT,9016)J1,J2,ILLIM1,ILLIM2,K1,K2
      CALL EDWRST('EDLOC2')
 9016 FORMAT('J1,J2,ILLIM1,ILLIM2,K1,K2 = ',6I8)
      WRITE(ICOUT,9017)(ICHA(J),J=J1,J2)
      CALL EDWRST('EDLOC2')
 9017 FORMAT('(ICHA(J),J=J1,J2) = ',100A1)
      WRITE(ICOUT,9018)IHIT
      CALL EDWRST('EDLOC2')
 9018 FORMAT('IHIT = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOC2')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLOPE
C
C     PURPOSE--LOCATE THE NEXT OCCURRANCE OF A PERIOD (ONLY)
C              (START THE SEARCH WITH THE NEXT LINE)
C     NOTE--
C     COMMAND SYNTAX--LPER
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89.1
C     ORIGINAL VERSION--DECEMBER 1988.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHIT
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLO'
      ISUBN2='PE  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IHIT='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOPE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOPE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLOPE')
   51 FORMAT('***** AT THE BEGINNING OF EDLOPE--')
      WRITE(ICOUT,52)IALL
      CALL EDWRST('EDLOPE')
   52 FORMAT('IALL = ',A4)
      WRITE(ICOUT,54)NUMARG,IWIDTH
      CALL EDWRST('EDLOPE')
   54 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,55)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDLOPE')
      ENDIF
   55 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,56)ICURLN,NUMLIN,NUMCLS
      CALL EDWRST('EDLOPE')
   56 FORMAT('ICURLN,NUMLIN,NUMCLS = ',3I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 3--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE SCANNED.   **
C               ************************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOPE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLP=ICURLN+1
C
      ISTART=ICURLP
      ISTOP=NUMLIN
C
C               ****************************
C               **  STEP 4--              **
C               **  SCAN      THE LINES.  **
C               ****************************
C
      ISTEPN='4'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'LOPE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTART.GT.ISTOP)GOTO1550
C
      DO1510ILINE=ISTART,ISTOP
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDLOP2(J1,J2,IHIT)
      IF(IHIT.EQ.'YES')GOTO1520
      GOTO1510
C
 1520 CONTINUE
      ICURLN=ILINE
      IF(IFEESW.EQ.'OFF')GOTO1529
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLOPE')
      ENDIF
 1521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1522)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDLOPE')
      ENDIF
 1522 FORMAT(I6,':',3X,230A1)
 1529 CONTINUE
      IF(IALL.EQ.'ON')GOTO1510
      GOTO1590
C
 1510 CONTINUE
C
 1550 CONTINUE
      ICURLN=NUMLIN+1
      IF(IFEESW.EQ.'OFF')GOTO1559
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1551)
         CALL EDWRST('EDLOPE')
      ENDIF
 1551 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1552)
         CALL EDWRST('EDLOPE')
      ENDIF
 1552 FORMAT(10X,'[BOTTOM]')
 1559 CONTINUE
C
 1590 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOPE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOPE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLOPE')
 9011 FORMAT('***** AT THE END       OF EDLOPE--')
      WRITE(ICOUT,9015)NUMARG,IWIDTH
      CALL EDWRST('EDLOPE')
 9015 FORMAT('NUMARG,IWIDTH = ',2I8)
      IF(IWIDTH.GE.1)THEN
         WRITE(ICOUT,9016)(IANS(I),I=1,IWIDTH)
         CALL EDWRST('EDLOPE')
      ENDIF
 9016 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDLOPE')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)J1,J2,IHIT,ICURLN
      CALL EDWRST('EDLOPE')
 9018 FORMAT('J1,J2,IHIT,ICURLN = ',2I8,2X,A4,I8)
      WRITE(ICOUT,9019)IALL,IFOUND
      CALL EDWRST('EDLOPE')
 9019 FORMAT('IALL,IFOUND = ',A4,2X,A4)
      WRITE(ICOUT,9020)ICURLN,NUMLIN,NUMCLS
      CALL EDWRST('EDLOPE')
 9020 FORMAT('ICURLN,NUMLIN,NUMCLS = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOPE')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLOP2(J1,J2,IHIT)
C
C     PURPOSE--SCAN ICHA(.) BETWEEN
C              ELEMENTS J1 AND J2 (INCLUSIVE)
C              AND DETERMINE IF ONLY A LEAD PERIOD.
C              IF SO, THEN SET IHIT TO 'YES'
C              IF NO, THEN SET IHIT TO 'NO'
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89.1
C     ORIGINAL VERSION--DECEMBER 1988.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHIT
C
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLO'
      ISUBN2='P2  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOP2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOP2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLOP2')
   51 FORMAT('***** AT THE BEGINNING OF EDLOP2--')
      WRITE(ICOUT,56)J1,J2
      CALL EDWRST('EDLOP2')
   56 FORMAT('J1,J2 = ',2I8)
      WRITE(ICOUT,57)(ICHA(J),J=J1,J2)
      CALL EDWRST('EDLOP2')
   57 FORMAT('(ICHA(J),J=J1,J2) = ',100A1)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOP2')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IHIT='NO'
C
      K1=(J1-1)+ILLIM1
      K2=(J1-1)+ILLIM2
      IF(K1.LT.J1.OR.K1.GT.J2)K1=J1
      IF(K2.LT.J1.OR.K2.GT.J2)K2=J2
      IF(K1.GT.K2)GOTO1110
      IF(ICHA(K1).EQ.'.')GOTO1105
      GOTO1120
 1105 CONTINUE
      K1P1=K1+1
      IF(K1P1.GT.K2)GOTO1110
      DO1100J=K1P1,K2
      IF(ICHA(J).EQ.' ')GOTO1100
      GOTO1120
 1100 CONTINUE
 1110 CONTINUE
      IHIT='YES'
      GOTO9000
 1120 CONTINUE
      IHIT='NO'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'LOP2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOP2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLOP2')
 9011 FORMAT('***** AT THE END       OF EDLOP2--')
      WRITE(ICOUT,9016)J1,J2,ILLIM1,ILLIM2,K1,K2
      CALL EDWRST('EDLOP2')
 9016 FORMAT('J1,J2,ILLIM1,ILLIM2,K1,K2 = ',6I8)
      WRITE(ICOUT,9017)(ICHA(J),J=J1,J2)
      CALL EDWRST('EDLOP2')
 9017 FORMAT('(ICHA(J),J=J1,J2) = ',100A1)
      WRITE(ICOUT,9018)IHIT
      CALL EDWRST('EDLOP2')
 9018 FORMAT('IHIT = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOP2')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDLOW1(IA1,IB1)
C
C     PURPOSE--CONVERT 1-CHARACTER UPPER CASE ALPHABETIC (A TO Z)
C              ASCII WORD IA1
C              TO LOWER CASE ASCII WORD IB1.
C     NOTE--IA1 AND IB1 ARE ASSUMED TO BE CHARACTER*1
C     NOTE--IA1 AND IB1 MAY BE THE SAME VARIABLE IN THE CALLING ROUTINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY 1981.
C     UPDATED         --MAY       1982.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IA1
      CHARACTER*1 IB1
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)----------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGMA.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOW1')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDLOW1')
   51 FORMAT('***** AT THE BEGINNING OF EDLOW1--')
      WRITE(ICOUT,52)IA1
      CALL EDWRST('EDLOW1')
   52 FORMAT('IA1 = ',A1)
   90 CONTINUE
C
C               ********************************************************
C               **  STEP 11--
C               **  THE FOLLOWING CODE WILL CARRY OUT THE UPPER CASE
C               **  TO LOWER CASE CONVERSION FOR ALL COMPUTERS
C               **  WITH AN ANSI 77 FORTRAN COMPILER--IT MAKES
C               **  USE OF THE ANSI FORTRAN 77 INTRINSIC FUNCTIONS
C               **     1) ICHAR (FOR ASCII CHARACTER TO ASCII NUMERIC CO
C               **     2) CHAR  (FOR ASCII NUMERIC TO ASCII CHARACTER CO
C               ********************************************************
C
      IB1=IA1
C
      IVALUE=ICHAR(IA1)
      IF(65.LE.IVALUE.AND.IVALUE.LE.90)GOTO1110
      GOTO1190
C
 1110 CONTINUE
      IVAL32=IVALUE+32
      IB1=CHAR(IVAL32)
      GOTO1190
C
 1190 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGMA.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDLOW1')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDLOW1')
 9011 FORMAT('***** AT THE END       OF EDLOW1--')
      WRITE(ICOUT,9012)IA1,IVALUE,IVAL32,IB1
      CALL EDWRST('EDLOW1')
 9012 FORMAT('IA1,IVALUE,IVAL32,IB1 = ',A4,I8,I8,2X,A4)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDNEAT(ICOLL1,ICOLL2,IBLASW)
C
C     PURPOSE--MAKE NEAT A CHUNK OF TEXT.
C              IF NO ARGUMENTS, THEN MAKE NEAT THE CURRENT (OR NEXT) PARAGRAPH.
C              IF 1  ARGUMENT , THEN MAKE NEAT THE CURRENT LINE
C                               + NEXT (N-1) LINES;
C              IF 2  ARGUMENTS, THEN MAKE NEAT LINES N1 TO N2.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--MAY     1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHIT
      CHARACTER*4 IBLASW
      CHARACTER*4 IADDED
C
      CHARACTER*4 IFEES2
C
      CHARACTER*240 ICLINE
      CHARACTER*240 ICWORD
      CHARACTER*240 ICNEWL
C
      CHARACTER*4 IFOUWO
C
      CHARACTER*4 IEOF
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDNE'
      ISUBN2='AT  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IEOF='NO'
      IARG1=(-999)
      IARG2=(-999)
      J=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'NEAT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDNEAT')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDNEAT')
   51 FORMAT('*****AT THE BEGINNING OF EDNEAT--')
      WRITE(ICOUT,52)IPRISW,ICURLN
      CALL EDWRST('EDNEAT')
   52 FORMAT('IPRISW,ICURLN = ',A4,I8)
      WRITE(ICOUT,53)ICOLL1,ICOLL2,IBLASW,IJUST
      CALL EDWRST('EDNEAT')
   53 FORMAT('ICOLL1,ICOLL2,IBLASW,IJUST = ',2I8,2X,A4,2X,A4)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************************
C               **  STEP 11--                                        **
C               **  DETERMINE THE FIRST AND LAST                     **
C               **  LINE NUMBERS TO BE OPERATED ON.                  **
C               **  FOR THE NO-ARGUMENT CASE--                       **
C               **     IF CURRENT LINE IS NON-BLANK, START WITH IT,  **
C               **     ELSE START WITH NEXT NON-BLANK LINE.          **
C               **     AFTER FINDING THE START LINE,                 **
C               **     STOP ON THE LINE BEFORE THE NEXT BLANK LINE.  **
C               *******************************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'NEAT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=ISTART
C
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      GOTO1120
C
 1100 CONTINUE
      I2=1
      IMIN=ICURLN
      IF(IMIN.LE.0)IMIN=1
      IMAX=NUMLIN
      IF(IMAX.LE.0)IMAX=1
      IF(IMIN.GT.IMAX)GOTO1102
      DO1101I=IMIN,IMAX
      I2=I
      IROW=IPOINT(I)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDLOB2(J1,J2,IHIT)
      IF(IHIT.EQ.'YES')GOTO1101
      GOTO1102
 1101 CONTINUE
 1102 CONTINUE
      ISTART=I2
C
      I2=ISTART
      IMIN=ISTART
      IF(IMIN.LE.0)IMIN=1
      IMAX=NUMLIN
      IF(IMAX.LE.0)IMAX=1
      IF(IMIN.GT.IMAX)GOTO1107
      DO1106I=IMIN,IMAX
      I2=I
      IROW=IPOINT(I)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDLOB2(J1,J2,IHIT)
      IF(IHIT.EQ.'YES')GOTO1108
      GOTO1106
 1106 CONTINUE
 1107 CONTINUE
      ISTOP=I2
      GOTO1109
 1108 CONTINUE
      ISTOP=I2-1
      GOTO1109
 1109 CONTINUE
C
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'NEAT')THEN
         WRITE(ICOUT,1177)IMIN,IMAX,ISTART,ISTOP
         CALL EDWRST('EDNEAT')
      ENDIF
 1177 FORMAT('IMIN,IMAX,ISTART,ISTOP = ',4I8)
      GOTO1190
C
 1110 CONTINUE
      IARG1=1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
C
      ISTART=ICURLN
      ISTOP=ISTART+IARG1-1
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      GOTO1190
C
 1120 CONTINUE
      IARG1=1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
C
      IARG2=1
      IF(IARGT(2).EQ.'NUMB')IARG2=IARG(2)
C
      ISTART=IARG1
      IF(ISTART.LE.0)ISTART=1
      ISTOP=IARG2
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      GOTO1190
C
 1190 CONTINUE
      ICURL2=ISTOP
      ISTAR2=ICURL2
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  LOOP THROUGH THE LINES.                  **
C               ***********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'NEAT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLN=ISTART
      CALL EDZERO(ICNEWL,NCNEWL)
      NCNEWL=ICOLL1-1
C
      DO2100ILINE=ISTART,ISTOP
C
C               *******************************************************
C               **  STEP 22--                                        **
C               **  OPERATE ON A GIVEN LINE--                        **
C               **     1) EXTRACT THE LINE (PUT IT INTO ICLINE)      **
C               **     2) LOOP THOUGH THE VARIOUS WORDS              **
C               **     3) FOR EACH WORD--                            **
C               **     5) IF THE NEWLY-CREATED LINE IS NOT FULL,     **
C               **        THEN ADD THE WORD TO IT;                   **
C               **     6) IF THE NEWLY-CREATED LINE IS FULL,         **
C               **        THEN APPEND THE LINE AFTER THE PARAGRAPH.  **
C               **        AND "ZERO-OUT" THE LINE FOR ANOTHER LINE.  **
C               **  NOTE--TREAT THE FORTRAN JUSTIFICATION CASE       **
C               **        IN A SPECIAL FASHION.                      **
C               *******************************************************
C
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(N1.LE.0)GOTO2100
      J=J1-1
      DO2200I=1,N1
      J=J+1
      ICLINE(I:I)=ICHA(J)
 2200 CONTINUE
      NCLINE=N1
C
      IF(IJUST.EQ.'FORT')GOTO2210
      GOTO2219
 2210 CONTINUE
      DO2211I=1,NCLINE
      ICNEWL(I:I)=ICLINE(I:I)
 2211 CONTINUE
      NCNEWL=NCLINE
      CALL EDJUST(ICNEWL,NCNEWL,ICOLL1,ICOLL2)
      CALL EDINLI(ICNEWL,NCNEWL,ICURL2)
      CALL EDZERO(ICNEWL,NCNEWL)
      NCNEWL=0
      GOTO2100
 2219 CONTINUE
C
      IC1=ICOLL1
      IC2=N1
      IC3=0
      IC4=IC2
C
      IF(ILINE.EQ.ISTART)GOTO2220
      GOTO2250
 2220 CONTINUE
      NCNEWL=ICOLL1-1
      IF(NCNEWL.LE.0)GOTO2229
      DO2221K=1,NCNEWL
      ICNEWL(K:K)=ICLINE(K:K)
 2221 CONTINUE
 2229 CONTINUE
 2250 CONTINUE
C
      DO2300IW=1,N1
C
      IF(IW.GE.2)IC1=IC4+1
      IF(IC1.GT.IC2)GOTO2100
      CALL EDZERO(ICWORD,NCWORD)
      CALL EDEXWO(ICLINE,NCLINE,IC1,IC2,IC3,IC4,
     1ICWORD,NCWORD,IFOUWO)
      IF(IFOUWO.EQ.'YES')GOTO2310
      GOTO2300
C
 2310 CONTINUE
      CALL EDADWO(ICNEWL,NCNEWL,ICWORD,NCWORD,
     1ICOLL1,ICOLL2,IBLASW,IADDED)
C
      IF(IADDED.EQ.'YES')GOTO2300
      CALL EDJUST(ICNEWL,NCNEWL,ICOLL1,ICOLL2)
CCCCC WRITE(ICOUT,777)(ICNEWL(I:I),I=1,100)
CC777 FORMAT('***** ICNEWL = ',100A1)
      CALL EDINLI(ICNEWL,NCNEWL,ICURL2)
      CALL EDZERO(ICNEWL,NCNEWL)
      NCNEWL=ICOLL1-1
C
      CALL EDADWO(ICNEWL,NCNEWL,ICWORD,NCWORD,
     1ICOLL1,ICOLL2,IBLASW,IADDED)
      IF(IADDED.EQ.'YES')GOTO2300
      WRITE(ICOUT,2321)
      CALL EDWRST('EDNEAT')
 2321 FORMAT('***** ERROR IN EDNEAT--')
      WRITE(ICOUT,2322)
      CALL EDWRST('EDNEAT')
 2322 FORMAT('      WORD IS TOO LONG TO BE ADDED')
      WRITE(ICOUT,2323)
      CALL EDWRST('EDNEAT')
 2323 FORMAT('      (EVEN TO AN EMPTY NEW LINE)')
      WRITE(ICOUT,2324)NCNEWL
      CALL EDWRST('EDNEAT')
 2324 FORMAT('NCNEWL = ',I8)
      IF(NCNEWL.GE.1)THEN
         WRITE(ICOUT,2325)(ICNEWL(I:I),I=1,100)
         CALL EDWRST('EDNEAT')
      ENDIF
 2325 FORMAT('(ICNEWL(I:I),I=1,NCNEWL) = ',100A1)
      WRITE(ICOUT,2326)NCWORD
      CALL EDWRST('EDNEAT')
 2326 FORMAT('NCWORD = ',I8)
      IF(NCWORD.GE.1)THEN
         WRITE(ICOUT,2327)(ICWORD(I:I),I=1,100)
         CALL EDWRST('EDNEAT')
      ENDIF
 2327 FORMAT('(ICWORD(I:I),I=1,NCWORD) = ',100A1)
      WRITE(ICOUT,2328)ICOLL1,ICOLL2,IBLASW,IADDED
      CALL EDWRST('EDNEAT')
 2328 FORMAT('ICOLL1,ICOLL2,IBLASW,IADDED = ',2I8,2X,A4,2X,A4)
      GOTO2300
C
 2300 CONTINUE
 2100 CONTINUE
      IF(NCNEWL.GE.1)GOTO2450
      GOTO2490
 2450 CONTINUE
      IF(IJUST.EQ.'RIGH'.OR.IJUST.EQ.'FORT')
     1CALL EDJUST(ICNEWL,NCNEWL,ICOLL1,ICOLL2)
      CALL EDINLI(ICNEWL,NCNEWL,ICURL2)
 2490 CONTINUE
      CALL EDZERO(ICNEWL,NCNEWL)
      NCNEWL=ICOLL1-1
      ISTOP2=ICURL2
C
C               ***********************************************
C               **  STEP 31--                                **
C               **  DELETE THE OLD (NON-NEAT) LINES.         **
C               ***********************************************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'NEAT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IFEES2='OFF'
      DO3100ILINE=ISTART,ISTOP
      CALL EDDELI(ISTART,IFEES2)
 3100 CONTINUE
CCCCC IDELSW='OFF'
CCCCC IDELLN='-999'
C
C               ***********************************************
C               **  STEP 32--                                **
C               **  PRINT THE NEW (NEAT) LINES.              **
C               ***********************************************
C
      ISTEPN='32'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'NEAT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC IDEL=ISTOP-ISTART
      IDEL=ISTOP-ISTART+1
      ISTAR3=ISTAR2-IDEL
      ISTOP3=ISTOP2-IDEL
CCCCC THE FOLLOWING 2 LINES WERE ADDED     DECEMBER 1994
      IF(ISTAR3.LE.0)ISTAR3=1
      IF(ISTOP3.LT.ISTAR3)ISTOP3=ISTAR3
      DO3200ILINE=ISTAR3,ISTOP3
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(N1.LE.0)GOTO3200
      J=J1-1
      DO3210I=1,N1
      J=J+1
      ICNEWL(I:I)=ICHA(J)
 3210 CONTINUE
      NCNEWL=N1
      CALL EDWRLI(ICNEWL,NCNEWL,ILINE)
 3200 CONTINUE
      CALL EDZERO(ICNEWL,NCNEWL)
      NCNEWL=ICOLL1-1
C     ICURLN=ISTOP3   WHY NOT WORK???
CCCCC ICURLN=ISTOP3-1
      ICURLN=ISTOP3
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'NEAT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDNEAT')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDNEAT')
 9011 FORMAT('*****AT THE END       OF EDNEAT--')
      WRITE(ICOUT,9012)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDNEAT')
 9012 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9013)IPRISW
      CALL EDWRST('EDNEAT')
 9013 FORMAT('IPRISW = ',A4)
      WRITE(ICOUT,9014)ISTART,ISTOP,ISTAR2,ISTOP2
      CALL EDWRST('EDNEAT')
 9014 FORMAT('ISTART,ISTOP,ISTAR2,ISTOP2 = ',4I8)
      WRITE(ICOUT,9015)ICOLL1,ICOLL2,IBLASW,IJUST
      CALL EDWRST('EDNEAT')
 9015 FORMAT('ICOLL1,ICOLL2,IBLASW,IJUST = ',2I8,2X,A4,2X,A4)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDNEXT
C
C     PURPOSE--GO TO NEXT K-TH LINE
C
C     NOTE--IF NO ARGUMENTS, THEN GO TO NEXT LINE
C           IF 1 ARGUMENT, THEN GO TO NEXT K-TH LINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDNE'
      ISUBN2='XT  '
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'NEXT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDNEXT')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDNEXT')
   51 FORMAT('***** AT THE BEGINNING OF EDNEXT--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************
C               **  STEP 1--           **
C               **  GO TO NEXT K-TH LINE **
C               **  AND PRINT IT OUT.  **
C               *************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'NEXT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      K=1
      IF(NUMARG.GE.1.AND.IARGT(1).EQ.'NUMB')K=IARG(1)
C
      ILINE=ICURLN+K
C
      IF(ILINE.LT.1)GOTO1110
      IF(ILINE.GT.NUMLIN)GOTO1120
      GOTO1130
C
 1110 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1119
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1111)
         CALL EDWRST('EDNEXT')
      ENDIF
 1111 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1112)
         CALL EDWRST('EDNEXT')
      ENDIF
 1112 FORMAT(10X,'[TOP]')
 1119 CONTINUE
      ILINE=0
      GOTO1190
C
 1120 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1129
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1121)
         CALL EDWRST('EDNEXT')
      ENDIF
 1121 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1122)
         CALL EDWRST('EDNEXT')
      ENDIF
 1122 FORMAT(10X,'[BOTTOM]')
 1129 CONTINUE
      ILINE=NUMLIN+1
      GOTO1190
C
 1130 CONTINUE
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(IFEESW.EQ.'OFF')GOTO1139
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1131)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDNEXT')
      ENDIF
 1131 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1132)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDNEXT')
      ENDIF
 1132 FORMAT(I6,':',3X,230A1)
 1139 CONTINUE
      GOTO1190
C
 1190 CONTINUE
      ICURLN=ILINE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'NEXT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDNEXT')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDNEXT')
 9011 FORMAT('***** AT THE END       OF EDNEXT--')
      WRITE(ICOUT,9012)NUMARG,K,ICURLN,ILINE
      CALL EDWRST('EDNEXT')
 9012 FORMAT('NUMARG,K,ICURLN,ILINE = ',4I8)
      WRITE(ICOUT,9013)IROW,J1,N1,J2
      CALL EDWRST('EDNEXT')
 9013 FORMAT('IROW,J1,N1,J2 = ',4I8)
      WRITE(ICOUT,9014)IFEESW,IPRISW,INUMSW
      CALL EDWRST('EDNEXT')
 9014 FORMAT('IFEESW,IPRISW,INUMSW = ',A4,2X,A4,2X,A4)
      WRITE(ICOUT,9015)(ICHA(J),J=1,100)
      CALL EDWRST('EDNEXT')
 9015 FORMAT('ICHA(J),J=1,100 =',100A1)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDOPEN
C
C     PURPOSE--OPEN (BUT DO NOT WRITE TO PER SE) A FILE
C     NOTE--IF NO ARGUMENTS GIVEN,
C           THE THE DEFAULT PRINTER FILE WILL BE OPENED.
C         --IF 1 ARGUMENT GIVEN (PRESUMEDLY A FILE NAME),
C           THEN THAT FILE WILL BE OPENED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED         --AUGUST  1992.  USE DATAPLOT ROUTINE DPOPFI
C                                      (TO AVOID INSTALLERS HAVING TO
C                                      MODIFY ADDITIONAL FILES)
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
CCCCC CHARACTER*80 IREC
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUEX
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDLI'
      ISUBN2='ST  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ADD')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPEN')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDOPEN')
   51 FORMAT('***** AT THE BEGINNING OF EDOPEN--')
      WRITE(ICOUT,53)IOPENU
      CALL EDWRST('EDOPEN')
   53 FORMAT('IOPENU = ',I8)
      WRITE(ICOUT,54)IOPENA
      CALL EDWRST('EDOPEN')
   54 FORMAT('IOPENA = ',A80)
      WRITE(ICOUT,55)IOPEST
      CALL EDWRST('EDOPEN')
   55 FORMAT('IOPEST = ',A12)
      WRITE(ICOUT,56)IOPEFO
      CALL EDWRST('EDOPEN')
   56 FORMAT('IOPEFO = ',A12)
      WRITE(ICOUT,57)IOPEAC
      CALL EDWRST('EDOPEN')
   57 FORMAT('IOPEAC = ',A12)
      WRITE(ICOUT,58)IOPERW
      CALL EDWRST('EDOPEN')
   58 FORMAT('IOPERW = ',A12)
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPEN')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 11--                            **
C               **  DETERMINE THE NAME OF THE FILE       **
C               **  THAT IS TO BE OPENED                 **
C               **  (IF NO NAME IS GIVEN,                **
C               **  THEN USE A DEFAULT FILE NAME).       **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUEX)
      IF(IFOUEX.EQ.'YES')GOTO1110
      GOTO1120
C
 1110 CONTINUE
      IFILE=' '
      IF(NUMCST.LE.0)GOTO1119
      DO1111I=1,NUMCST
      IFILE(I:I)=ISTRIN(I)
 1111 CONTINUE
 1119 CONTINUE
      ISTAT='UNKNOWN'
      GOTO1190
C
 1120 CONTINUE
      IFILE=IOPENA
      ISTAT=IOPEST
      GOTO1190
C
 1190 CONTINUE
      IOUNIT=IOPENU
      IFORM=IOPEFO
      IACCES=IOPEAC
      IREWR=IOPERW
      ISUBN0='OPEN'
      IERRFI='NO'
C
C               ****************************
C               **  STEP 12--             **
C               **  WRITE OUT A MESSAGE   **
C               **  IDENTIFYING THE FILE  **
C               ****************************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPEN')
      WRITE(ICOUT,1205)IFILE
      CALL EDWRST('EDOPEN')
 1205 FORMAT(10X,'FILE = ',A80)
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPEN')
C
C               *********************
C               **  STEP 13--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='13'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'ADD')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO9000
      REWIND IOUNIT
C
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ADD')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPEN')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDOPEN')
 9011 FORMAT('***** AT THE END       OF EDOPEN--')
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDOPEN')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDOPEN')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDOPEN')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPEN')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,
     1ISUBN0,IERRFI)
C
C     PURPOSE--OPEN A FILE
C              (BUT THERE MAY BE SOME SMALL DIFFERENCES
C              IN HOW THAT IS DONE FOR DIFFERENT COMPUTERS).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--NOVEMBER 1985.
C
C------------------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
CCCCC CHARACTER*12 ICC
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDOP'
      ISUBN2='FI  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'OPFI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPFI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDOPFI')
   51 FORMAT('***** AT THE BEGINNING OF EDOPFI--')
      WRITE(ICOUT,52)IBUGFI,ISUBRO
      CALL EDWRST('EDOPFI')
   52 FORMAT('IBUGFI,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,53)IOUNIT
      CALL EDWRST('EDOPFI')
   53 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,54)IFILE
      CALL EDWRST('EDOPFI')
   54 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,55)ISTAT
      CALL EDWRST('EDOPFI')
   55 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,56)IFORM
      CALL EDWRST('EDOPFI')
   56 FORMAT('IFORM = ',A12)
      WRITE(ICOUT,57)IACCES
      CALL EDWRST('EDOPFI')
   57 FORMAT('IACCES = ',A12)
      WRITE(ICOUT,58)IREWR
      CALL EDWRST('EDOPFI')
   58 FORMAT('IREWR = ',A12)
      WRITE(ICOUT,59)ISUBN0
      CALL EDWRST('EDOPFI')
   59 FORMAT('ISUBN0 = ',A4)
      WRITE(ICOUT,60)IERRFI
      CALL EDWRST('EDOPFI')
   60 FORMAT('IERRFI = ',A4)
      WRITE(ICOUT,61)IHOST1
      CALL EDWRST('EDOPFI')
   61 FORMAT('IHOST1 = ',A4)
   90 CONTINUE
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  OPEN A FILE.                                      **
C               **  NOTE--ON THE VAX                                  **
C               **        (AND PROBABLY MOST OTHER COMPUTERS),        **
C               **        IF YOU HAVE THE READONLY ARGUMENT,          **
C               **        THEN THE ONLY ACCEPTABLE STATUS IS 'OLD'--  **
C               **        A STATUS OF 'UNKNOWN' OR 'NEW' WILL         **
C               **        RESULT IN THE FILE NOT BEING OPENED         **
C               **        EVEN THOUGH THE FILE MAY EXIST.             **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'OPFI')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHOST1.EQ.'VAX')GOTO1100
      IF(IHOST1.EQ.'HONE')GOTO1300
      IF(IHOST1.EQ.'IBM-')GOTO1400
      GOTO1200
C
 1100 CONTINUE
CCCCC IF(IREWR.EQ.'READONLY')
CCCCC1OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='OLD',FORM=IFORM,
CCCCC1ERR=5100,ACCESS=IACCES,CARRIAGE CONTROL='LIST',READONLY)
CCCCC IF(IREWR.NE.'READONLY')
CCCCC1OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM=IFORM,
CCCCC1ERR=5100,ACCESS=IACCES,CARRIAGE CONTROL='LIST')
      GOTO9000
C
 1200 CONTINUE
      IF(IREWR.EQ.'READONLY')
     1OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='OLD',FORM=IFORM,
     1ERR=5100)
      IF(IREWR.NE.'READONLY')
     1OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM=IFORM,
     1ERR=5100)
      GOTO9000
C
 1300 CONTINUE
CCCCC IF(IREWR.EQ.'READONLY')
CCCCC1OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='OLD',FORM=IFORM,
CCCCC1MODE="IN",ERR=5100)
CCCCC IF(IREWR.NE.'READONLY')
CCCCC1OPEN(UNIT=IOUNIT,FILE=IFILE,FORM=IFORM,
CCCCC1ERR=5100)
      GOTO9000
C
 1400 CONTINUE
      IF(IREWR.EQ.'READONLY')
     1OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='OLD',FORM=IFORM,
     1ERR=5100)
      IF(IREWR.NE.'READONLY')
     1OPEN(UNIT=IOUNIT,FILE=IFILE,FORM=IFORM,
     1ERR=5100)
      GOTO9000
C
 5100 CONTINUE
      IERRFI='YES'
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPFI')
      WRITE(ICOUT,5101)
      CALL EDWRST('EDOPFI')
 5101 FORMAT('***** ERROR IN EDOPFI--')
      WRITE(ICOUT,5102)
      CALL EDWRST('EDOPFI')
 5102 FORMAT('ERROR IN ATTEMPTING TO OPEN A FILE.')
      WRITE(ICOUT,5103)IOUNIT
      CALL EDWRST('EDOPFI')
 5103 FORMAT('I/O UNIT    = ',I8)
      WRITE(ICOUT,5104)IFILE
      CALL EDWRST('EDOPFI')
 5104 FORMAT('FILE NAME   = ',A80)
      WRITE(ICOUT,5105)ISTAT
      CALL EDWRST('EDOPFI')
 5105 FORMAT('FILE STATUS = ',A12)
      WRITE(ICOUT,5106)IFORM
      CALL EDWRST('EDOPFI')
 5106 FORMAT('FILE FORMAT = ',A12)
      WRITE(ICOUT,5107)IACCES
      CALL EDWRST('EDOPFI')
 5107 FORMAT('FILE ACCESS = ',A12)
      WRITE(ICOUT,5108)IREWR
      CALL EDWRST('EDOPFI')
 5108 FORMAT('FILE READ/WRITE SETTING         = ',A12)
      WRITE(ICOUT,5109)ISUBN0
      CALL EDWRST('EDOPFI')
 5109 FORMAT('PREVIOUS (= CALLING) SUBROUTINE = ',A4)
      WRITE(ICOUT,5110)IHOST1
      CALL EDWRST('EDOPFI')
 5110 FORMAT('HOST COMPUTER                   = ',A4)
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'OPFI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPFI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDOPFI')
 9011 FORMAT('***** AT THE END       OF EDOPFI--')
      WRITE(ICOUT,9012)IBUGFI,ISUBRO
      CALL EDWRST('EDOPFI')
 9012 FORMAT('IBUGFI,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,9013)IOUNIT
      CALL EDWRST('EDOPFI')
 9013 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9014)IFILE
      CALL EDWRST('EDOPFI')
 9014 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDOPFI')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)IFORM
      CALL EDWRST('EDOPFI')
 9016 FORMAT('IFORM = ',A12)
      WRITE(ICOUT,9017)IACCES
      CALL EDWRST('EDOPFI')
 9017 FORMAT('IACCES = ',A12)
      WRITE(ICOUT,9018)IREWR
      CALL EDWRST('EDOPFI')
 9018 FORMAT('IREWR = ',A12)
      WRITE(ICOUT,9019)ISUBN0
      CALL EDWRST('EDOPFI')
 9019 FORMAT('ISUBN0 = ',A4)
      WRITE(ICOUT,9020)IERRFI
      CALL EDWRST('EDOPFI')
 9020 FORMAT('IERRFI = ',A4)
      WRITE(ICOUT,9021)IHOST1
      CALL EDWRST('EDOPFI')
 9021 FORMAT('IHOST1 = ',A4)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDOPPR
C
C     PURPOSE--OPEN THE PRINTER
C              (FOR THE IBM-PC PRINTER,
C              OPEN THE PRINTER "FILE")
C     NOTE--NO ARGUMENTS ARE EXPECTED OR PROCESSED
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDAC'
      ISUBN2='PR  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'ACPR')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPPR')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDOPPR')
   51 FORMAT('***** AT THE BEGINNING OF EDOPPR--')
   90 CONTINUE
C
      NCOUT=1
      ICOUT(1:1)=IDLEC
      ILOUT=(-1)
      CALL EDWRST('EDOPPR')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDOPPR')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDOPPR')
 9011 FORMAT('***** AT THE END       OF EDOPPR--')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDPA
C
C     PURPOSE--PRINT OUT ALL REMAINING LINES OF TEXT.
C              IF NO ARGUMENTS, THEN PRINT OUT CURRENT LINE
C                               + ALL REMAINING LINES.
C              IF 1  ARGUMENT , THEN PRINT THAT LINE
C                               + ALL REMAINING LINES.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*4 IEOF
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDPR'
      ISUBN2='IN  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IEOF='NO'
      IARG1=(-999)
      J=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDPA  ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDPA  ')
   51 FORMAT('***** AT THE BEGINNING OF EDPA--')
      WRITE(ICOUT,52)IPRISW,ICURLN
      CALL EDWRST('EDPA  ')
   52 FORMAT('IPRISW,ICURLN = ',A4,I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 2--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE PRINTED.   **
C               ************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=NUMLIN
C
      IF(NUMARG.LE.0)GOTO1100
      GOTO1110
C
 1100 CONTINUE
      ISTART=ICURLN
      IF(ISTART.LE.0)ISTART=1
      ISTOP=NUMLIN
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
      GOTO1190
C
 1110 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
C
      ISTART=IARG1
      IF(ISTART.LE.0)ISTART=1
      ISTOP=NUMLIN
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      GOTO1190
C
 1190 CONTINUE
CCCCC IHOLD1=ISTART
CCCCC IHOLD2=ISTOP
CCCCC IF(IHOLD1.GT.IHOLD2)ISTART=ISTOP
CCCCC IF(IHOLD1.GT.IHOLD2)ISTOP=ISTART
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  COPY OVER THE PRINTER "FILE" VARIABLES)  **
C               **  (IF CALLED FOR)                          **
C               ***********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO2190
C
C
      IOUNIT=IPRINU
      IFILE=IPRINA
      ISTAT=IPRIST
      IFORM=IPRIFO
      IACCES=IPRIAC
      IREWR=IPRIRW
      ISUBN0='PRIN'
      IERRFI='NO'
      IF(IBUGE2.EQ.'OFF')GOTO2189
      WRITE(ICOUT,999)
      CALL EDWRST('EDPA  ')
      WRITE(ICOUT,2181)
      CALL EDWRST('EDPA  ')
 2181 FORMAT('***** FROM THE MIDDLE OF EDPA--')
      WRITE(ICOUT,2183)IFILE
      CALL EDWRST('EDPA  ')
 2183 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,2184)IOUNIT
      CALL EDWRST('EDPA  ')
 2184 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,2185)ISTAT
      CALL EDWRST('EDPA  ')
 2185 FORMAT('ISTAT = ',A12)
 2189 CONTINUE
C
 2190 CONTINUE
C
C               *************************
C               **  STEP 22--          **
C               **  OPEN  THE PRINTER  **
C               **  (IF CALLED FOR)    **
C               *************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO2290
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
C
 2290 CONTINUE
C
C               ****************************
C               **  STEP 31--             **
C               **  PRINT OUT THE LINES.  **
C               ****************************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTART.GT.ISTOP)GOTO3150
      DO3110ILINE=ISTART,ISTOP
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      NCOUT=0
      IF(J1.GT.J2)GOTO3129
      DO3120J=J1,J2
      NCOUT=NCOUT+1
      ICOUT(NCOUT:NCOUT)=ICHA(J)
 3120 CONTINUE
 3129 CONTINUE
      ILOUT=ILINE
      CALL EDWRST('EDPA  ')
 3110 CONTINUE
C
 3150 CONTINUE
      IF(IEOF.EQ.'NO')GOTO3159
      IF(IFEESW.EQ.'OFF')GOTO3159
      ICOUT(1:12)='[BOTTOM]'
      NCOUT=12
      ILOUT=(-999)
      CALL EDWRST('EDPA  ')
 3159 CONTINUE
C
      ICURLN=ISTOP
      IF(IEOF.EQ.'YES')ICURLN=NUMLIN+1
C
C               *************************
C               **  STEP 41--          **
C               **  CLOSE THE PRINTER  **
C               **  (IF CALLED FOR)    **
C               *************************
C
      ISTEPN='41'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO4190
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
 4190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDPA  ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDPA  ')
 9011 FORMAT('***** AT THE END       OF EDPA--')
      WRITE(ICOUT,9012)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDPA  ')
 9012 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9013)IPRISW
      CALL EDWRST('EDPA  ')
 9013 FORMAT('IPRISW = ',A4)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDPERI
C
C     PURPOSE--THIS IS A NULL SUBROUTINE
C              FOR THE    .   COMMAND
C              WHICH IS A NULL COMMENT COMMAND.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDPE'
      ISUBN2='RI  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PERI')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDPERI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDPERI')
   51 FORMAT('*****AT THE BEGINNING OF EDPERI--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PERI')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDPERI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDPERI')
 9011 FORMAT('*****AT THE END       OF EDPERI--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDPL
C
C     PURPOSE--PRINT TEXT UNTIL LOCATE    STRING
C              START WITH CURRENT LINE
C              STOP WITH NEXT LINE WITH SPECIFIED STRING
C
 
C              IF NO ARGUMENTS, THEN PRINT TO END OF FILE
C              IF 1  ARGUMENT , THEN PRINT UNTIL LOCATE LINE WITH    ARG1
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 ISTRIN
      CHARACTER*4 IFOUST
      CHARACTER*4 IHIT
      CHARACTER*4 IDONE
C
      DIMENSION ISTRIN(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDPL'
      ISUBN2='    '
C
      IFOUND='YES'
      IERROR='NO'
C
      IHIT='NO'
      IDONE='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
      IMIN=(-999)
      IFIRBL=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PL')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDPL  ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDPL  ')
   51 FORMAT('*****AT THE BEGINNING OF EDPL--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE STRING TO BE LOCATED   **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDEXST(ISTRIN,NUMCST,IFOUST)
      IF(IFOUST.EQ.'YES')GOTO1190
C
      NUMCST=NUMCLS
      IF(NUMCLS.LE.0)GOTO1159
      DO1155I=1,NUMCLS
      ISTRIN(I)=ILOCST(I)
 1155 CONTINUE
 1159 CONTINUE
      GOTO1190
C
 1190 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  IF THE STRING IS NEW,           **
C               **  COPY THE STRING INTO ILOCST(.)  **
C               **  FOR FUTURE USE BY SUBSEQUENT    **
C               **  LOCATE COMMANDS.                **
C               **************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFOUST.EQ.'NO')GOTO1290
      NUMCLS=NUMCST
      IF(NUMCST.LE.0)GOTO1290
      DO1210I=1,NUMCST
      ILOCST(I)=ISTRIN(I)
 1210 CONTINUE
 1290 CONTINUE
C
C               ************************************
C               **  STEP 3--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE SCANNED.   **
C               ************************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=NUMLIN
C
C               *********************************
C               **  STEP 4--                   **
C               **  SCAN AND PRINT THE LINES.  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IDONE='NO'
      IF(ISTART.LE.0)ISTART=1
      IF(ISTART.GT.ISTOP)GOTO1550
C
      DO1510ILINE=ISTART,ISTOP
C
      ICURLN=ILINE
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDLOC2(ISTRIN,NUMCST,J1,J2,IHIT)
      IF(IHIT.EQ.'NO')GOTO1520
      IF(ILINE.EQ.ISTART)GOTO1520
      IDONE='YES'
      GOTO1520
C
 1520 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDPL  ')
      ENDIF
 1521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1522)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDPL  ')
      ENDIF
 1522 FORMAT(I6,':',3X,230A1)
      IF(IDONE.EQ.'YES')GOTO1590
C
 1510 CONTINUE
C
 1550 CONTINUE
      ICURLN=NUMLIN+1
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1551)
         CALL EDWRST('EDPL  ')
      ENDIF
 1551 FORMAT('===BOTTOM===')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1552)
         CALL EDWRST('EDPL  ')
      ENDIF
 1552 FORMAT(10X,'===BOTTOM===')
 1559 CONTINUE
      GOTO1590
C
 1590 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PL')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDPL  ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDPL  ')
 9011 FORMAT('*****AT THE END       OF EDPL--')
      WRITE(ICOUT,9012)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDPL  ')
 9012 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9016)IFIRBL,IMIN,IWIDTH
      CALL EDWRST('EDPL  ')
 9016 FORMAT('IFIRBL,IMIN,IWIDTH = ',3I8)
      WRITE(ICOUT,9017)J1,N1,J2
      CALL EDWRST('EDPL  ')
 9017 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,9018)J1,J2,IHIT,ICURLN
      CALL EDWRST('EDPL  ')
 9018 FORMAT('J1,J2,IHIT,ICURLN = ',4I8)
      WRITE(ICOUT,9019)IFOUST
      CALL EDWRST('EDPL  ')
 9019 FORMAT('IFOUST = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDPL  ')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDPP(IPPLIN,IPPOFF)
C
C     PURPOSE--PRINT OUT A PAGEFUL OF TEXT--
C              THAT IS, PRINT OUT FROM (AND INCLUDING) CURRENT LINE - IPPOFF
C                 1) AND THE NEXT   IPPLIN   LINES; OR
C                 2) DOWN TO A LINE WHICH STARTS WITH **S OR **s
C              (WHICHEVER COMES FIRST).
C              IF NO ARGUMENTS, THEN PRINT OUT CURRENT PAGE
C              IF 1  ARGUMENT , THEN PRINT ARG1 PAGES
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED         --DECEMBER  1988.  ELIMINATE MULTI-PRINTING OF SAME PAGE
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*4 IHIT
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDPP'
      ISUBN2='    '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PP')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDPP  ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDPP  ')
   51 FORMAT('*****AT THE BEGINNING OF EDPP--')
      WRITE(ICOUT,52)IPRISW,IPRINU,IOUNIT,INUMSW
      CALL EDWRST('EDPP  ')
   52 FORMAT('IPRISW,IPRINU,IOUNIT,INUMSW = ',A4,I8,I8,2X,A4)
      WRITE(ICOUT,53)IPPLIN,IPPOFF
      CALL EDWRST('EDPP  ')
   53 FORMAT('IPPLIN,IPPOFF = ',2I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************************
C               **  STEP 11--                      **
C               **  DETERMINE THE NUMBER OF PAGES  **
C               **  TO BE PRINTED                  **
C               *************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.0)GOTO1100
      GOTO1110
C
 1100 CONTINUE
      NUMPAG=1
      GOTO1190
C
 1110 CONTINUE
      NUMPAG=100000
      IF(IARGT(1).EQ.'NUMB')NUMPAG=IARG(1)
      IF(NUMPAG.LE.0)NUMPAG=1
      GOTO1190
C
 1190 CONTINUE
C     THE FOLLOWING FIX WAS INSERTED DECEMBER 1988 TO AVOID PP 50
C     (FOR EXAMPLE) YIELDING 50 PRINTINGS OF THE SAME PAGE.
      NUMPAG=1
C
C               ************************************
C               **  STEP 12--                     **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE SCANNED.   **
C               **  NOTE--IPPOFF IS USUALLY A NEGATIVE NUMBER  **
C               ************************************
C
      ISTEPN='12'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN+IPPOFF
CCCCC ISTOP=NUMLIN
      ISTOP=ISTART+(IPPLIN-1)
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  COPY OVER THE PRINTER "FILE" VARIABLES)  **
C               **  (IF CALLED FOR)                          **
C               ***********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO2190
C
      IOUNIT=IPRINU
      IFILE=IPRINA
      ISTAT=IPRIST
      IFORM=IPRIFO
      IACCES=IPRIAC
      IREWR=IPRIRW
      ISUBN0='PP'
      IERRFI='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PP')GOTO2189
      WRITE(ICOUT,999)
      CALL EDWRST('EDPP  ')
      WRITE(ICOUT,2181)
      CALL EDWRST('EDPP  ')
 2181 FORMAT('***** FROM THE MIDDLE OF EDPP--')
      WRITE(ICOUT,2183)IFILE
      CALL EDWRST('EDPP  ')
 2183 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,2184)IOUNIT
      CALL EDWRST('EDPP  ')
 2184 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,2185)ISTAT
      CALL EDWRST('EDPP  ')
 2185 FORMAT('ISTAT = ',A12)
 2189 CONTINUE
C
 2190 CONTINUE
C
C               *************************
C               **  STEP 22--          **
C               **  OPEN  THE PRINTER  **
C               **  (IF CALLED FOR)    **
C               *************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO2290
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED',
CCCCC1ERR=2280)
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO2280
      GOTO2290
C
 2280 CONTINUE
      WRITE(ICOUT,2281)
      CALL EDWRST('EDPP  ')
 2281 FORMAT('***** ERROR IN EDPP--')
      WRITE(ICOUT,2282)
      CALL EDWRST('EDPP  ')
 2282 FORMAT('      FAILED IN ATTEMPTING TO OPEN THE PRINTER')
      IERROR='YES'
      GOTO9000
C
 2290 CONTINUE
C
C               ****************************
C               **  STEP 31--             **
C               **  SCAN      THE LINES.  **
C               ****************************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTART.GT.ISTOP)GOTO3150
C
      DO3100IPAGE=1,NUMPAG
      DO3110ILINE=ISTART,ISTOP
C
      ICURLN=ILINE
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      CALL EDPP2(J1,J2,IHIT)
      IF(IHIT.EQ.'NO')GOTO3120
      IF(ILINE.EQ.ISTART)GOTO3110
      GOTO3100
C
 3120 CONTINUE
      IF(INUMSW.EQ.'ON')GOTO3121
      GOTO3126
 3121 CONTINUE
      WRITE(ICOUT,3122)ILINE,(ICHA(J),J=J1,J2)
      CALL EDWRST('EDPP  ')
 3122 FORMAT(I6,':',3X,230A1)
      IF(IPRISW.EQ.'ON')THEN
         WRITE(IOUNIT,3123)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDPP  ')
 3123    FORMAT(I6,':',3X,230A1)
      ENDIF
      GOTO3129
 3126 CONTINUE
      WRITE(ICOUT,3127)(ICHA(J),J=J1,J2)
      CALL EDWRST('EDPP  ')
 3127 FORMAT(238A1)
      IF(IPRISW.EQ.'ON')THEN
         WRITE(IOUNIT,3128)(ICHA(J),J=J1,J2)
 3128    FORMAT(238A1)
      ENDIF
      GOTO3129
 3129 CONTINUE
C
 3110 CONTINUE
 3100 CONTINUE
      GOTO3190
C
 3150 CONTINUE
      ICURLN=NUMLIN+1
      GOTO3190
C
 3190 CONTINUE
C
C               *************************
C               **  STEP 41--          **
C               **  CLOSE THE PRINTER  **
C               **  (IF CALLED FOR)    **
C               *************************
C
      ISTEPN='41'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO4190
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
 4190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PP')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDPP  ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDPP  ')
 9011 FORMAT('*****AT THE END       OF EDPP--')
      WRITE(ICOUT,9012)IPPLIN,IPPOFF
      CALL EDWRST('EDPP  ')
 9012 FORMAT('IPPLIN,IPPOFF = ',2I8)
      WRITE(ICOUT,9013)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDPP  ')
 9013 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9014)IPRISW,IPRINU,IOUNIT,INUMSW
      CALL EDWRST('EDPP  ')
 9014 FORMAT('IPRISW,IPRINU,IOUNIT,INUMSW = ',A4,I8,I8,2X,A4)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDPP2(J1,J2,IHIT)
C
C     PURPOSE--SCAN ICHA(.) BETWEEN
C              ELEMENTS J1 AND J2 (INCLUSIVE)
C              AND DETERMINE IF THE STRINGS    **NEW    OR     **new
C              OR    **ERASE     OR     **erase
C              ARE CONTAINED THERIN IN COLUMNS 1 TO 5.
C              IF SO, THEN SET IHIT TO 'YES'
C              IF NO, THEN SET IHIT TO 'NO'
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*8 IC8
C
      CHARACTER*4 IHIT
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDPP'
      ISUBN2='2   '
C
      K1=(-999)
      K2=(-999)
      K3=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PP2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDPP2 ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDPP2 ')
   51 FORMAT('***** AT THE BEGINNING OF EDPP2--')
      WRITE(ICOUT,54)J1,J2
      CALL EDWRST('EDPP2 ')
   54 FORMAT('J1,J2 = ',2I8)
      WRITE(ICOUT,57)IHIT
      CALL EDWRST('EDPP2 ')
   57 FORMAT('IHIT = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDPP2 ')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IHIT='NO'
C
      JDEL=J2-J1+1
      IF(JDEL.LE.4)GOTO1110
C
      K1=J1
      K2=J1+1
      K3=J1+2
      K4=J1+3
      K5=J1+4
      IC8(1:1)=ICHA(K1)
      IC8(2:2)=ICHA(K2)
      IC8(3:3)=ICHA(K3)
      IC8(4:4)=ICHA(K4)
      IC8(5:5)=ICHA(K5)
      IF(IC8(1:5).EQ.'**NEW')GOTO1120
      IF(IC8(1:5).EQ.'**new')GOTO1120
      IF(IC8(1:5).EQ.'**ERA')GOTO1120
      IF(IC8(1:5).EQ.'**era')GOTO1120
      GOTO1110
C
 1110 CONTINUE
      IHIT='NO'
      GOTO1190
C
 1120 CONTINUE
      IHIT='YES'
      GOTO1190
C
 1190 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PP2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDPP2 ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDPP2 ')
 9011 FORMAT('***** AT THE END       OF EDPP2--')
      WRITE(ICOUT,9013)J1,J2
      CALL EDWRST('EDPP2 ')
 9013 FORMAT('J1,J2 = ',2I8)
      WRITE(ICOUT,9014)K1,K2,K3
      CALL EDWRST('EDPP2 ')
 9014 FORMAT('K1,K2,K3 = ',3I8)
      WRITE(ICOUT,9017)IHIT
      CALL EDWRST('EDPP2 ')
 9017 FORMAT('IHIT = ',A4)
      WRITE(ICOUT,9018)IC8
      CALL EDWRST('EDPP2 ')
 9018 FORMAT('IC8 = ',A8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDPP2 ')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDPRIN
C
C     PURPOSE--PRINT OUT LINES OF TEXT.
C              IF NO ARGUMENTS, THEN PRINT OUT CURRENT LINE.
C              IF 1  ARGUMENT , THEN PRINT CURRENT LINE
C                               + NEXT (N-1) LINES;
C                               THAT IS, PRINT RELATIVE.
C              IF 2  ARGUMENTS, THEN PRINT LINES N1 TO N2,
C                               THAT IS, PRINT ABSOLUTE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*4 IEOF
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDPR'
      ISUBN2='IN  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IEOF='NO'
      IARG1=(-999)
      IARG2=(-999)
      J=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDPRIN')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDPRIN')
   51 FORMAT('*****AT THE BEGINNING OF EDPRIN--')
      WRITE(ICOUT,52)IPRISW,ICURLN
      CALL EDWRST('EDPRIN')
   52 FORMAT('IPRISW,ICURLN = ',A4,I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 2--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE PRINTED.   **
C               ************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=ISTART
C
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      GOTO1120
C
 1100 CONTINUE
      ISTART=ICURLN
      ISTOP=ISTART
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
      GOTO1190
C
 1110 CONTINUE
CCCCC IARG1=NUMLIN+1
      IARG1=1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
C
      ISTART=ICURLN
      ISTOP=ISTART+IARG1-1
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      GOTO1190
C
 1120 CONTINUE
CCCCC IARG1=NUMLIN+1
      IARG1=1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
C
CCCCC IARG2=NUMLIN+1
      IARG2=1
      IF(IARGT(2).EQ.'NUMB')IARG2=IARG(2)
C
      ISTART=IARG1
      IF(ISTART.LE.0)ISTART=1
      ISTOP=IARG2
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      GOTO1190
C
 1190 CONTINUE
CCCCC IHOLD1=ISTART
CCCCC IHOLD2=ISTOP
CCCCC IF(IHOLD1.GT.IHOLD2)ISTART=ISTOP
CCCCC IF(IHOLD1.GT.IHOLD2)ISTOP=ISTART
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  COPY OVER THE PRINTER "FILE" VARIABLES)  **
C               **  (IF CALLED FOR)                          **
C               ***********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO2190
C
      IOUNIT=IPRINU
      IFILE=IPRINA
      ISTAT=IPRIST
      IFORM=IPRIFO
      IACCES=IPRIAC
      IREWR=IPRIRW
      ISUBN0='PRIN'
      IERRFI='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO2189
      WRITE(ICOUT,999)
      CALL EDWRST('EDPRIN')
      WRITE(ICOUT,2181)
      CALL EDWRST('EDPRIN')
 2181 FORMAT('***** FROM THE MIDDLE OF EDPRIN--')
      WRITE(ICOUT,2183)IFILE
      CALL EDWRST('EDPRIN')
 2183 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,2184)IOUNIT
      CALL EDWRST('EDPRIN')
 2184 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,2185)ISTAT
      CALL EDWRST('EDPRIN')
 2185 FORMAT('ISTAT = ',A12)
 2189 CONTINUE
C
 2190 CONTINUE
C
C               *************************
C               **  STEP 22--          **
C               **  OPEN  THE PRINTER  **
C               **  (IF CALLED FOR)    **
C               *************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO2290
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED',
CCCCC1ERR=2280)
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO2280
      GOTO2290
C
 2280 CONTINUE
      WRITE(ICOUT,2281)
      CALL EDWRST('EDPRIN')
 2281 FORMAT('***** ERROR IN EDPRIN--')
      WRITE(ICOUT,2282)
      CALL EDWRST('EDPRIN')
 2282 FORMAT('      FAILED IN ATTEMPTING TO OPEN THE PRINTER')
      IERROR='YES'
      GOTO9000
C
 2290 CONTINUE
C
C               ****************************
C               **  STEP 31--             **
C               **  PRINT OUT THE LINES.  **
C               ****************************
C
      ISTEPN='31'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTART.GT.ISTOP)GOTO3150
      DO3110ILINE=ISTART,ISTOP
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      NCOUT=0
      IF(J1.GT.J2)GOTO3129
      DO3120J=J1,J2
      NCOUT=NCOUT+1
      ICOUT(NCOUT:NCOUT)=ICHA(J)
 3120 CONTINUE
 3129 CONTINUE
      ILOUT=ILINE
      CALL EDWRST('EDPRIN')
 3110 CONTINUE
C
 3150 CONTINUE
      IF(IEOF.EQ.'NO')GOTO3159
      IF(IFEESW.EQ.'OFF')GOTO3159
      ICOUT(1:12)='[BOTTOM]'
      NCOUT=12
      ILOUT=(-999)
      CALL EDWRST('EDPRIN')
 3159 CONTINUE
C
      ICURLN=ISTOP
      IF(IEOF.EQ.'YES')ICURLN=NUMLIN+1
C
C               *************************
C               **  STEP 41--          **
C               **  CLOSE THE PRINTER  **
C               **  (IF CALLED FOR)    **
C               *************************
C
      ISTEPN='41'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO4190
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
 4190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDPRIN')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDPRIN')
 9011 FORMAT('*****AT THE END       OF EDPRIN--')
      WRITE(ICOUT,9012)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDPRIN')
 9012 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9013)IPRISW
      CALL EDWRST('EDPRIN')
 9013 FORMAT('IPRISW = ',A4)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDPRNE
C
C     PURPOSE--PRINT OUT LINES OF TEXT
C              (STARTING WITH THE NEXT LINE).
C              IF NO ARGUMENTS, THEN PRINT OUT NEXT LINE ONLY.
C              IF 1  ARGUMENT , THEN PRINT NEXT LINE
C                               + NEXT (N-1) LINES;
C                               THAT IS, PRINT RELATIVE.
C              IF 2  ARGUMENTS, THEN PRINT LINES N1 TO N2,
C                               THAT IS, PRINT ABSOLUTE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IEOF
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDPR'
      ISUBN2='NE  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IEOF='NO'
      IARG1=(-999)
      IARG2=(-999)
      J=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRNE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDPRNE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDPRNE')
   51 FORMAT('*****AT THE BEGINNING OF EDPRNE--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 2--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE PRINTED.   **
C               ************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRNE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLP=ICURLN+1
C
      ISTART=ICURLP
      ISTOP=ISTART
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      GOTO1120
C
 1100 CONTINUE
      ISTART=ICURLP
      ISTOP=ISTART
      GOTO1190
C
 1110 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
C
      ISTART=ICURLP
      ISTOP=ISTART+IARG1-1
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      GOTO1190
C
 1120 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
C
      IARG2=NUMLIN+1
      IF(IARGT(2).EQ.'NUMB')IARG2=IARG(2)
C
      ISTART=IARG1
      IF(ISTART.LE.0)ISTART=1
      ISTOP=IARG2
      IF(ISTOP.GT.NUMLIN)IEOF='YES'
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
C
      GOTO1190
C
 1190 CONTINUE
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD1.GT.IHOLD2)ISTART=ISTOP
      IF(IHOLD1.GT.IHOLD2)ISTOP=ISTART
C
C               ****************************
C               **  STEP 3--              **
C               **  PRINT OUT THE LINES.  **
C               ****************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRNE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LE.0)ISTART=1
      IF(ISTOP.GT.NUMLIN)ISTOP=NUMLIN
      IF(ISTART.GT.ISTOP)GOTO9000
      DO1510ILINE=ISTART,ISTOP
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1521)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDPRNE')
      ENDIF
 1521 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1522)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDPRNE')
      ENDIF
 1522 FORMAT(I6,':',3X,230A1)
 1510 CONTINUE
C
 1550 CONTINUE
      IF(IEOF.EQ.'NO')GOTO1559
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1551)
         CALL EDWRST('EDPRNE')
      ENDIF
 1551 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1552)
         CALL EDWRST('EDPRNE')
      ENDIF
 1552 FORMAT(10X,'[BOTTOM]')
 1559 CONTINUE
C
      ICURLN=ISTOP
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRNE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDPRNE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDPRNE')
 9011 FORMAT('*****AT THE END       OF EDPRNE--')
      WRITE(ICOUT,9012)ICURLN,ICURLP,ISTART,ISTOP
      CALL EDWRST('EDPRNE')
 9012 FORMAT('ICURLN,ICURLP,ISTART,ISTOP = ',4I8)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDREFW(ID)
C
C     PURPOSE--LOAD (= READ) A FILE INTO THE WORKSPACE
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C     UPDATED--JULY 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*4 ID
C
      CHARACTER*4 IEXIST
C
CCCCC CHARACTER*80 IREC
      CHARACTER*240 IREC
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDRE'
      ISUBN2='FW  '
C
CCCCC IBUGFI='ON'
      IFOUND='YES'
      IERROR='NO'
C
      IEXIST='-999'
C
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'REFW')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDREFW')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDREFW')
   51 FORMAT('*****AT THE BEGINNING OF EDREFW--')
      WRITE(ICOUT,52)ID,IBUGFI,ISUBRO
      CALL EDWRST('EDREFW')
   52 FORMAT('ID,IBUGFI,ISUBRO = ',A4,2X,A4,2X,A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDREFW')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'REFW')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ID.EQ.'ORIG')GOTO1110
      IF(ID.EQ.'SAVE')GOTO1120
      GOTO1120
C
 1110 CONTINUE
      IOUNIT=IORINU
      IFILE=IORINA
      ISTAT=IORIST
      IFORM=IORIFO
      IACCES=IORIAC
      IREWR=IORIRW
      ISUBN0='REFW'
      IERRFI='NO'
      GOTO1190
C
 1120 CONTINUE
      IOUNIT=ISAVNU
      IFILE=ISAVNA
      ISTAT=ISAVST
      IFORM=ISAVFO
      IACCES=ISAVAC
      IREWR=ISAVRW
      ISUBN0='REFW'
      IERRFI='NO'
      GOTO1190
C
 1190 CONTINUE
      IF(IBUGFI.EQ.'OFF')GOTO1199
      WRITE(ICOUT,1193)IFILE
      CALL EDWRST('EDREFW')
 1193 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,1194)IOUNIT
      CALL EDWRST('EDREFW')
 1194 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,1195)ISTAT
      CALL EDWRST('EDREFW')
 1195 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,1196)NUMLIN,NUMCHA
      CALL EDWRST('EDREFW')
 1196 FORMAT('NUMLIN,NUMCHA = ',2I8)
 1199 CONTINUE
C
C               *********************
C               **  STEP 12--      **
C               **  INQUIRE AS TO  **
C               **  WHETHER THE    **
C               **  FILE EXISTS.   **
C               *********************
C
      ISTEPN='12'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'REFW')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDINFI(IFILE,IEXIST,ISUBN0,IERRFI)
      NUMCHA=0
      NUMLIN=0
      NUMROW=0
      IF(IEXIST.EQ.'NO')GOTO9000
C
C               *********************
C               **  STEP 13--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='13'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'REFW')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
C
C               ****************************************************
C               **  STEP 14--                                     **
C               **  READ A FILE INTO THE WORKSPACE                **
C               ****************************************************
C
      ISTEPN='14'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'REFW')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCHA=0
      NUMLIN=0
      NUMROW=0
C
      ILINE=0
      J1=0
      J2=0
C
      DO1300IDUMMY=1,100000
CCCCC NUMCRE=80
      NUMCRE=240
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1993
      IREC=' '
      READ(IOUNIT,1305,END=1390)IREC
 1305 FORMAT(A238)
C
      IF(IBUGFI.EQ.'ON')WRITE(ICOUT,1306)IREC
      IF(IBUGFI.EQ.'ON')CALL EDWRST('EDREFW')
 1306 FORMAT('IREC = ',A230)
      IF(IBUGFI.EQ.'ON')WRITE(ICOUT,1307)NUMCRE
      IF(IBUGFI.EQ.'ON')CALL EDWRST('EDREFW')
 1307 FORMAT('NUMCRE = ',I8)
C
      DO1400I=1,NUMCRE
      IREV=NUMCRE-I+1
      IF(IREC(IREV:IREV).NE.' ')GOTO1490
 1400 CONTINUE
      IREV=0
 1490 CONTINUE
      NUMCRE=IREV
C
      IF(IBUGFI.EQ.'ON')WRITE(ICOUT,1495)IREC
      IF(IBUGFI.EQ.'ON')CALL EDWRST('EDREFW')
 1495 FORMAT('IREC = ',A230)
      IF(IBUGFI.EQ.'ON')WRITE(ICOUT,1496)NUMCRE
      IF(IBUGFI.EQ.'ON')CALL EDWRST('EDREFW')
 1496 FORMAT('NUMCRE = ',I8)
C
      ILINE=ILINE+1
      IF(ILINE.LE.MAXEDL)GOTO1519
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL EDWRST('EDREFW')
      WRITE(ICOUT,1511)
      CALL EDWRST('EDREFW')
 1511 FORMAT('***** ERROR IN EDREFW--')
      WRITE(ICOUT,1512)
      CALL EDWRST('EDREFW')
 1512 FORMAT('      THE FILE BEING EDITED HAS TOO MANY LINES.')
      WRITE(ICOUT,1513)ILINE,MAXEDL
      CALL EDWRST('EDREFW')
 1513 FORMAT('      ILINE,MAXEDL = ',2I8)
      WRITE(ICOUT,1514)
      CALL EDWRST('EDREFW')
 1514 FORMAT('      ONLY A PARTIAL READ WAS DONE.')
      ILINE=MAXEDL
      GOTO1390
 1519 CONTINUE
C
      IPOINT(ILINE)=ILINE
      J1=J2+1
      N1=NUMCRE
      J2=J1+(N1-1)
      ILOCC1(ILINE)=J1
      NUMCPL(ILINE)=N1
C
      K=0
      IF(J2.LE.MAXEDC)GOTO1529
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL EDWRST('EDREFW')
      WRITE(ICOUT,1521)
      CALL EDWRST('EDREFW')
 1521 FORMAT('***** ERROR IN EDREFW--')
      WRITE(ICOUT,1522)
      CALL EDWRST('EDREFW')
 1522 FORMAT('      THE FILE BEING EDITED HAS TOO MANY CHARACTERS.')
      WRITE(ICOUT,1523)J2,MAXEDC
      CALL EDWRST('EDREFW')
 1523 FORMAT('      J2,MAXEDC = ',2I8)
      WRITE(ICOUT,1524)
      CALL EDWRST('EDREFW')
 1524 FORMAT('      ONLY A PARTIAL READ WAS DONE.')
      J2=MAXEDC
      GOTO1390
 1529 CONTINUE
C
      IF(J1.GT.J2)GOTO1590
      DO1500J=J1,J2
      K=K+1
      ICHA(J)=IREC(K:K)
 1500 CONTINUE
 1590 CONTINUE
C
 1300 CONTINUE
 1390 CONTINUE
      NUMCHA=J2
      NUMLIN=ILINE
      NUMROW=ILINE
C
C               **********************
C               **  STEP 15--       **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='15'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'REFW')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'REFW')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDREFW')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDREFW')
 9011 FORMAT('*****AT THE END       OF EDREFW--')
      WRITE(ICOUT,9012)ID,IEXIST
      CALL EDWRST('EDREFW')
 9012 FORMAT('ID,IEXIST = ',A4,2X,A4)
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDREFW')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDREFW')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDREFW')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)NUMLIN,NUMCHA
      CALL EDWRST('EDREFW')
 9016 FORMAT('NUMLIN,NUMCHA = ',2I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDREFW')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDREST
C
C     PURPOSE--RESTORE CURRENT DOCUMENT FROM THE SAVE FILE.
C     NOTE--NO ARGUMENTS ARE EXPECTED.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ID
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDRE'
      ISUBN2='ST  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SAVE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDREST')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDREST')
   51 FORMAT('***** AT THE BEGINNING OF EDREST--')
      WRITE(ICOUT,52)NUMLIN
      CALL EDWRST('EDREST')
   52 FORMAT('NUMLIN = ',I8)
      WRITE(ICOUT,53)ISAVNA
      CALL EDWRST('EDREST')
   53 FORMAT('ISAVNA = ',A80)
      WRITE(ICOUT,54)ISAVST
      CALL EDWRST('EDREST')
   54 FORMAT('ISAVST = ',A12)
      WRITE(ICOUT,55)ISAVNU
      CALL EDWRST('EDREST')
   55 FORMAT('ISAVNU = ',I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ***************************************
C               **  STEP 1--                         **
C               **  READ  WORKSPACE FROM SAVE  FILE  **
C               ***************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ID='SAVE'
      CALL EDREFW(ID)
C
C               ******************************
C               **  STEP 2--                **
C               **  WRITE OUT A MESSAGE     **
C               ******************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,1205)
      CALL EDWRST('EDREST')
 1205 FORMAT('THE SAVED VERSION HAS BEEN RESTORED')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SAVE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDREST')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDREST')
 9011 FORMAT('***** AT THE END       OF EDREST--')
      WRITE(ICOUT,9012)NUMLIN
      CALL EDWRST('EDREST')
 9012 FORMAT('NUMLIN = ',I8)
      WRITE(ICOUT,9013)ISAVNA
      CALL EDWRST('EDREST')
 9013 FORMAT('ISAVNA = ',A80)
      WRITE(ICOUT,9014)ISAVST
      CALL EDWRST('EDREST')
 9014 FORMAT('ISAVST = ',A12)
      WRITE(ICOUT,9015)ISAVNU
      CALL EDWRST('EDREST')
 9015 FORMAT('ISAVNU = ',I8)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSACP
C
C     PURPOSE--SAVE CURRENT SETTINGS OF INTERNAL POINTERS
C              FOR POSSIBLE LATER USE
C              BY THE UNDO COMMAND
C     NOTE--NO ARGUMENTS ARE EXPECTED.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDSA'
      ISUBN2='CP  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SACP')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSACP')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSACP')
   51 FORMAT('*****AT THE BEGINNING OF EDSACP--')
      WRITE(ICOUT,52)IBUGE2,ISUBRO
      CALL EDWRST('EDSACP')
   52 FORMAT('IBUGE2,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,53)NUMLOL,NUMROL,NCHAOL,IOLDLN
      CALL EDWRST('EDSACP')
   53 FORMAT('NUMLOL,NUMROL,NCHAOL,IOLDLN = ',4I8)
      WRITE(ICOUT,54)NUMLIN,NUMROW,NUMCHA,ICURLN
      CALL EDWRST('EDSACP')
   54 FORMAT('NUMLIN,NUMROW,NUMCHA,ICURLN = ',4I8)
      IF(NUMLIN.LE.0)GOTO59
      DO55I=1,NUMLIN
CCCCC WRITE(ICOUT,56)I,IPOIOL(I),IPOINT(I)
CCC56 FORMAT('I,IPOIOL(I),IPOINT(I) = ',3I8)
   55 CONTINUE
   59 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDSACP')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *********************************
C               **  STEP 1--                   **
C               **  COPY THE CURRENT SETTINGS  **
C               **  ONTO THE OLD SETTINGS      **
C               *********************************
C
      NCHAOL=NUMCHA
      NUMLOL=NUMLIN
      NUMROL=NUMROW
      IOLDLN=ICURLN
C
      IF(NUMLIN.LE.0)GOTO1190
      DO1100I=1,NUMLIN
      IPOIOL(I)=IPOINT(I)
 1100 CONTINUE
 1190 CONTINUE
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SACP')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSACP')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSACP')
 9011 FORMAT('*****AT THE END       OF EDSACP--')
      WRITE(ICOUT,9012)NUMLOL,NUMROL,NCHAOL,IOLDLN
      CALL EDWRST('EDSACP')
 9012 FORMAT('NUMLOL,NUMROL,NCHAOL,IOLDLN = ',4I8)
      WRITE(ICOUT,9013)NUMLIN,NUMROW,NUMCHA,ICURLN
      CALL EDWRST('EDSACP')
 9013 FORMAT('NUMLIN,NUMROW,NUMCHA,ICURLN = ',4I8)
      IF(NUMLIN.LE.0)GOTO9019
      DO9015I=1,NUMLIN
CCCCC WRITE(ICOUT,9016)I,IPOIOL(I),IPOINT(I)
C9016 FORMAT('I,IPOIOL(I),IPOINT(I) = ',3I8)
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDSACP')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSAVE
C
C     PURPOSE--SAVE CURRENT DOCUMENT ONTO THE SAVE FILE.
C     NOTE--NO ARGUMENTS ARE EXPECTED.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ID
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDSA'
      ISUBN2='VE  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SAVE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSAVE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSAVE')
   51 FORMAT('***** AT THE BEGINNING OF EDSAVE--')
      WRITE(ICOUT,52)NUMLIN
      CALL EDWRST('EDSAVE')
   52 FORMAT('NUMLIN = ',I8)
      WRITE(ICOUT,53)ISAVNA
      CALL EDWRST('EDSAVE')
   53 FORMAT('ISAVNA = ',A80)
      WRITE(ICOUT,54)ISAVST
      CALL EDWRST('EDSAVE')
   54 FORMAT('ISAVST = ',A12)
      WRITE(ICOUT,55)ISAVNU
      CALL EDWRST('EDSAVE')
   55 FORMAT('ISAVNU = ',I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ***************************************
C               **  STEP 1--                         **
C               **  WRITE WORKSPACE OUT TO THE FILE  **
C               ***************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ID='SAVE'
      CALL EDWRWF(ID)
C
C               ******************************
C               **  STEP 2--                **
C               **  WRITE OUT A MESSAGE     **
C               ******************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,1205)
      CALL EDWRST('EDSAVE')
 1205 FORMAT('THE CURRENT DOCUMENT HAS BEEN SAVED')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SAVE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSAVE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSAVE')
 9011 FORMAT('***** AT THE END       OF EDSAVE--')
      WRITE(ICOUT,9012)NUMLIN
      CALL EDWRST('EDSAVE')
 9012 FORMAT('NUMLIN = ',I8)
      WRITE(ICOUT,9013)ISAVNA
      CALL EDWRST('EDSAVE')
 9013 FORMAT('ISAVNA = ',A80)
      WRITE(ICOUT,9014)ISAVST
      CALL EDWRST('EDSAVE')
 9014 FORMAT('ISAVST = ',A12)
      WRITE(ICOUT,9015)ISAVNU
      CALL EDWRST('EDSAVE')
 9015 FORMAT('ISAVNU = ',I8)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSCAL
C
C     PURPOSE--WRITE OUT A SCALE LINE WHICH CONSISTS
C              OF NUMBERS WHICH INDICATE THE COLUMNS 1 TO 80.
C              IF NO ARGUMENTS, THEN WRITE OUT 1 TO 80
C              IF 1 ARGUMENT, THEN WRITE OUT 1 TO N1
C              IF 2 ARGUMENTS, THEN WRITE OUT N1 TO N2
C     NOTE--ONLY THE 1 TO 80 SCALE WORKS NOW.
C           TO BE DONE--THE OTHER OPTIONS
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ISTRIN
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDSC'
      ISUBN2='AL  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SCAL')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSCAL')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSCAL')
   51 FORMAT('*****AT THE BEGINNING OF EDSCAL--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ***********************************************
C               **  STEP 2--                                 **
C               **  DETERMINE THE RIGHT-MOST COLUMN          **
C               **  THAT THE SCALE IS TO BE PRINTED OUT TO.  **
C               ***********************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'PRIN')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=1
      ISTOP=80
CCCCC IF(INUMSW.EQ.'ON')ISTOP=70   JANUARY 1989
      IF(INUMSW.EQ.'ON')ISTOP=80
C
      IF(NUMARG.LE.0)GOTO1100
      GOTO1110
C
 1100 CONTINUE
      GOTO1190
C
 1110 CONTINUE
      IF(IARGT(1).EQ.'NUMB')ISTOP=IARG(1)
      GOTO1190
C
 1190 CONTINUE
      IF(ISTART.LE.1)ISTART=1
      IF(ISTOP.GE.240)ISTOP=240
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD1.GT.IHOLD2)ISTART=IHOLD2
      IF(IHOLD1.GT.IHOLD2)ISTOP=IHOLD1
C
C               ***************************
C               **  STEP 3--             **
C               **  PRINT OUT THE SCALE  **
C               ***************************
C
      ISTRIN(1:40)='123456789.123456789.123456789.123456789.'
      ISTRIN(41:80)='123456789.123456789.123456789.123456789.'
      ISTRIN(81:120)='123456789.123456789.123456789.123456789.'
CCCCC THE FOLLOWING LINE WAS FIXED APRIL 1990
CCCCC ISTRIN(121:132)='123456789.123456789.123456789.123456789.'
      ISTRIN(121:240)='123456789.123456789.123456789.123456789.'
C
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1105)(ISTRIN(I:I),I=ISTART,ISTOP)
         CALL EDWRST('EDSCAL')
      ENDIF
 1105 FORMAT(131A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1106)(ISTRIN(I:I),I=ISTART,ISTOP)
         CALL EDWRST('EDSCAL')
      ENDIF
 1106 FORMAT(10X,121A1)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SCAL')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSCAL')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSCAL')
 9011 FORMAT('*****AT THE END       OF EDSCAL--')
      WRITE(ICOUT,9012)IHOLD1,IHOLD2,ISTART,ISTOP
      CALL EDWRST('EDSCAL')
 9012 FORMAT('IHOLD1,IHOLD2,ISTART,ISTOP = ',4I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDSCAL')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSELM(ILMARG)
C
C     PURPOSE--SET THE LEFT MARGIN ON THE PRINTER
C              (FOR THE IBM-PC PRINTER,
C              SPACE OUT TO THE DESIRED COLUMN,
C               AND THEN SEND DOWN AN ESCAPE 9     )
C               (ESCAPE = ASCII ESC = CHAR(27)  )
C     NOTE--THE INPUT ARGUMENT IS THE DESIRED COLUMN
C           NUMBER FOR THE LEFT MARGIN.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDSE'
      ISUBN2='LM  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SELM')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSELM')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSELM')
   51 FORMAT('***** AT THE BEGINNING OF EDSELM--')
      WRITE(ICOUT,52)ILMARG
      CALL EDWRST('EDSELM')
   52 FORMAT('ILMARG = ',I8)
   90 CONTINUE
C
      CALL EDACPR
C
      NSPACE=ILMARG-1
      IF(NSPACE.LT.0)NSPACE=0
      IF(NSPACE.GT.240)NSPACE=240
C
      NCOUT=0
      IF(NSPACE.LE.0)GOTO1190
      DO1100I=1,NSPACE
      NCOUT=NCOUT+1
      ICOUT(NCOUT:NCOUT)=' '
 1100 CONTINUE
 1190 CONTINUE
C
      NCOUT=NCOUT+1
      ICOUT(NCOUT:NCOUT)=IESCC
C
      NCOUT=NCOUT+1
      ICOUT(NCOUT:NCOUT)='9'
C
      ILOUT=(-1)
      CALL EDWRST('EDSELM')
C
      CALL EDDEPR
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'PRIN')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSELM')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSELM')
 9011 FORMAT('***** AT THE END       OF EDSELM--')
      WRITE(ICOUT,9012)ILMARG
      CALL EDWRST('EDSELM')
 9012 FORMAT('ILMARG = ',I8)
      WRITE(ICOUT,9013)NCOUT,ILOUT
      CALL EDWRST('EDSELM')
 9013 FORMAT('NCOUT,ILOUT = ',2I8)
      IF(NCOUT.LE.0)GOTO9019
      DO9015I=1,NCOUT
      WRITE(ICOUT,9016)I,ICOUT(I:I)
      CALL EDWRST('EDSELM')
 9016 FORMAT('I,ICOUT(I:I) = ',I8,2X,A1)
 9015 CONTINUE
 9019 CONTINUE
C
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSERA
C
C     PURPOSE--SET THE VARIABLE WHICH DEFINES
C              THE STRING WHICH WILL ERASE THE TERMINAL SCREEN
C              IF NO ARGUMENTS, THEN SET ICERAS TO DEFAULT (= '    ')
C              IF 1  ARGUMENT , THEN SET ICERAS TO IHARG1
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDSE'
      ISUBN2='RA  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SERA')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSERA')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSERA')
   51 FORMAT('*****AT THE BEGINNING OF EDSERA--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************************
C               **  STEP 11--                      **
C               **  SET THE ERASE  STRING          **
C               *************************************
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SERA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.0)GOTO1100
      GOTO1110
C
 1100 CONTINUE
      ICERAS='    '
      GOTO1190
C
 1110 CONTINUE
      ICERAS=IHARG(1)
      GOTO1190
C
 1190 CONTINUE
C
C               ***************************
C               **  STEP 2--             **
C               **  WRITE OUT A MESSAGE  **
C               **  (IF CALLED FOR)      **
C               ***************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SERA')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEESW.EQ.'OFF')GOTO1219
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1211)ICERAS
         CALL EDWRST('EDSERA')
      ENDIF
 1211 FORMAT('ERASE STRING = ',A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1212)ICERAS
         CALL EDWRST('EDSERA')
      ENDIF
 1212 FORMAT(10X,'ERASE STRING = ',A1)
 1219 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SERA')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSERA')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSERA')
 9011 FORMAT('*****AT THE END       OF EDSERA--')
      WRITE(ICOUT,9012)ICERAS
      CALL EDWRST('EDSERA')
 9012 FORMAT('ICERAS = ',A1)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSET(IMARK,ICOLL1,ICOLL2,ISHIFN,IPPLIN,IPPOFF,IERASW,
     1ISEQNU,ILPOFF)
C
C     PURPOSE--SET SYSTEM SWITCHES AND VARIABLES.
C
C     DATE--JANUARY 24,1985
C
C-------------------------------------------------------------------------------
C
      CHARACTER*4 IERASW
C
      CHARACTER*4 IHARGI
      CHARACTER*1 ICHAR1
      CHARACTER*4 IHIT
      CHARACTER*8 IHTEM8
      CHARACTER*20 IHTE20
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      ISUBN1='EDSE'
      ISUBN2='T   '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SET')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSET ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSET ')
   51 FORMAT('*****AT THE BEGINNING OF EDSET--')
      WRITE(ICOUT,53)ILPOFF
      CALL EDWRST('EDSET ')
   53 FORMAT('ILPOFF = ',I8)
      WRITE(ICOUT,61)NUMARG,IHARG(1),IHARG(2)
      CALL EDWRST('EDSET ')
   61 FORMAT('NUMARG,IHARG(1),IHARG(2) = ',I8,2X,A4,2X,A4)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
CCCCC IF(NUMARG.LE.1)IERROR='YES'
CCCCC IF(NUMARG.LE.1)GOTO1000
CCCCC IF(NUMARG.LE.1)GOTO1000
C
      IF(IHARG(1).EQ.'IBUG')GOTO1100
      IF(IHARG(1).EQ.'ISUB')GOTO2100
C
      IF(IHARG(1).EQ.'BEGI')GOTO3100
      IF(IHARG(1).EQ.'END')GOTO3200
      IF(IHARG(1).EQ.'COPY')GOTO3300
      IF(IHARG(1).EQ.'MARK')GOTO3400
C
      IF(IHARG(1).EQ.'CHAN')GOTO4110
      IF(IHARG(1).EQ.'FIND')GOTO4120
      IF(IHARG(1).EQ.'LOCA')GOTO4130
      IF(IHARG(1).EQ.'MARG')GOTO4140
      IF(IHARG(1).EQ.'PRIN'.AND.IHARG2(1).EQ.'T   ')GOTO4150
      IF(IHARG(1).EQ.'PAGE')GOTO4160
C
      IF(IHARG(1).EQ.'SCRE'.AND.IHARG(2).EQ.'ROWS')GOTO4310
      IF(IHARG(1).EQ.'SCRE'.AND.IHARG(2).EQ.'ROW')GOTO4310
      IF(IHARG(1).EQ.'SCRE'.AND.IHARG(2).EQ.'COLU')GOTO4320
      IF(IHARG(1).EQ.'SCRE'.AND.IHARG(2).EQ.'COL')GOTO4320
C
      IF(IHARG(1).EQ.'PRIN'.AND.IHARG(2).EQ.'ROWS')GOTO4410
      IF(IHARG(1).EQ.'PRIN'.AND.IHARG(2).EQ.'ROW')GOTO4410
      IF(IHARG(1).EQ.'PRIN'.AND.IHARG(2).EQ.'COLU')GOTO4420
      IF(IHARG(1).EQ.'PRIN'.AND.IHARG(2).EQ.'COL')GOTO4420
C
CCCCC THE FOLLOWING LINE WAS ADDED MAY 1993
      IF(IHARG(1).EQ.'LOOP'.AND.IHARG(2).EQ.'COUN')GOTO4510
C
      IF(IHARG(1).EQ.'INDE')GOTO5110
      IF(IHARG(1).EQ.'CENT')GOTO5120
      IF(IHARG(1).EQ.'TRUN')GOTO5130
      IF(IHARG(1).EQ.'TAB')GOTO5140
      IF(IHARG(1).EQ.'NEAT')GOTO5150
      IF(IHARG(1).EQ.'JUST')GOTO5160
      IF(IHARG(1).EQ.'SHIF')GOTO5170
      IF(IHARG(1).EQ.'PP'.AND.IHARG(2).EQ.'OFFS')GOTO5180
      IF(IHARG(1).EQ.'PP')GOTO5190
      IF(IHARG(1).EQ.'ERAS')GOTO5200
      IF(IHARG(1).EQ.'SEQU')GOTO5210
      IF(IHARG(1).EQ.'LP'.AND.IHARG(2).EQ.'OFFS')GOTO5220
C
      IF(IHARG(1).EQ.'FEED')GOTO6110
      IF(IHARG(1).EQ.'NUMB')GOTO6120
      IF(IHARG(1).EQ.'PRIN'.AND.IHARG2(1).EQ.'TER ')GOTO6130
      IF(IHARG(1).EQ.'PROM')GOTO6140
      IF(IHARG(1).EQ.'TRAC')GOTO6150
      IF(IHARG(1).EQ.'ECHO')GOTO6160
C
      IF(IHARG(1).EQ.'MASK')GOTO6510
CCCCC IF(IHARG(1).EQ.'JUST')GOTO6520
      IF(IHARG(1).EQ.'HORI')GOTO6530
      IF(IHARG(1).EQ.'VERT')GOTO6540
      IF(IHARG(1).EQ.'TRAN')GOTO6550
C
      IF(IHARG(1).EQ.'COMM')GOTO7100
C
      GOTO9000
C
C               *************************
C               **  STEP 11--          **
C               **  SET BUG VARIABLES  **
C               *************************
C
 1000 CONTINUE
      WRITE(ICOUT,1011)
      CALL EDWRST('EDSET ')
 1011 FORMAT(10X,'[AVAILABLE SETTINGS UNDER')
      WRITE(ICOUT,1012)
      CALL EDWRST('EDSET ')
 1012 FORMAT(10X,' THE   SET   COMMAND-')
      WRITE(ICOUT,999)
      CALL EDWRST('EDSET ')
      WRITE(ICOUT,1021)
      CALL EDWRST('EDSET ')
 1021 FORMAT(10X,'      COPY   (LIMITS)')
      WRITE(ICOUT,1022)
      CALL EDWRST('EDSET ')
 1022 FORMAT(10X,'      CHANGE (LIMITS)')
      WRITE(ICOUT,1023)
      CALL EDWRST('EDSET ')
 1023 FORMAT(10X,'      FIND   (LIMITS)')
      WRITE(ICOUT,1024)
      CALL EDWRST('EDSET ')
 1024 FORMAT(10X,'      LOCATE (LIMITS)')
      WRITE(ICOUT,1025)
      CALL EDWRST('EDSET ')
 1025 FORMAT(10X,'      MARGIN (LIMITS)')
      WRITE(ICOUT,1026)
      CALL EDWRST('EDSET ')
 1026 FORMAT(10X,'      PAGE   (LIMITS)')
      WRITE(ICOUT,1027)
      CALL EDWRST('EDSET ')
 1027 FORMAT(10X,'      PRINT  (LIMITS)')
      WRITE(ICOUT,999)
      CALL EDWRST('EDSET ')
      WRITE(ICOUT,1031)
      CALL EDWRST('EDSET ')
 1031 FORMAT(10X,'      SCREEN    ROWS')
      WRITE(ICOUT,1032)
      CALL EDWRST('EDSET ')
 1032 FORMAT(10X,'      SCREEN    COLUMNS')
      WRITE(ICOUT,1033)
      CALL EDWRST('EDSET ')
 1033 FORMAT(10X,'      PRINTER   ROWS')
      WRITE(ICOUT,1034)
      CALL EDWRST('EDSET ')
 1034 FORMAT(10X,'      PRINTER   COLUMNS')
      WRITE(ICOUT,999)
      CALL EDWRST('EDSET ')
      WRITE(ICOUT,1041)
      CALL EDWRST('EDSET ')
 1041 FORMAT(10X,'      INDENTING   (COLUMN)')
      WRITE(ICOUT,1042)
      CALL EDWRST('EDSET ')
 1042 FORMAT(10X,'      CENTERING   (COLUMN)')
      WRITE(ICOUT,1043)
      CALL EDWRST('EDSET ')
 1043 FORMAT(10X,'      TRUNCATING  (COLUMN)')
      WRITE(ICOUT,1044)
      CALL EDWRST('EDSET ')
 1044 FORMAT(10X,'      TABBING     (COLUMN)')
      WRITE(ICOUT,999)
      CALL EDWRST('EDSET ')
      WRITE(ICOUT,1051)
      CALL EDWRST('EDSET ')
 1051 FORMAT(10X,'      FEEDBACK                 (SWITCH)')
      WRITE(ICOUT,1052)
      CALL EDWRST('EDSET ')
 1052 FORMAT(10X,'      (LINE) NUMBER (PRINTING) (SWITCH)')
      WRITE(ICOUT,1053)
      CALL EDWRST('EDSET ')
 1053 FORMAT(10X,'      PRINTER                  (SWITCH)')
      WRITE(ICOUT,1054)
      CALL EDWRST('EDSET ')
 1054 FORMAT(10X,'      PROMPT (PRINTING)        (SWITCH)')
      WRITE(ICOUT,1055)
      CALL EDWRST('EDSET ')
 1055 FORMAT(10X,'      TRACE                    (SWITCH)')
      WRITE(ICOUT,1056)
      CALL EDWRST('EDSET ')
 1056 FORMAT(10X,'      ECHO                     (SWITCH)')
      WRITE(ICOUT,999)
      CALL EDWRST('EDSET ')
      WRITE(ICOUT,1061)
      CALL EDWRST('EDSET ')
 1061 FORMAT(10X,'      MASK     (CHARACTER)')
      WRITE(ICOUT,1062)
      CALL EDWRST('EDSET ')
 1062 FORMAT(10X,'      JUSTIFICATION       ')
      WRITE(ICOUT,1063)
      CALL EDWRST('EDSET ')
 1063 FORMAT(10X,'      HORIZONTAL SPACING  ')
      WRITE(ICOUT,1064)
      CALL EDWRST('EDSET ')
 1064 FORMAT(10X,'      VERTICAL   SPACING  ')
      WRITE(ICOUT,1065)
      CALL EDWRST('EDSET ')
 1065 FORMAT(10X,'      TRANSLATION (TYPE)  ')
      WRITE(ICOUT,999)
      CALL EDWRST('EDSET ')
      WRITE(ICOUT,1071)
      CALL EDWRST('EDSET ')
 1071 FORMAT(10X,'      (USER-DEFINED) COMMANDS')
      GOTO9000
 
 1100 CONTINUE
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SET')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHARG2(1).EQ.'ED')GOTO1110
      IF(IHARG2(1).EQ.'E2')GOTO1120
      IF(IHARG2(1).EQ.'E3')GOTO1130
      IF(IHARG2(1).EQ.'TY')GOTO1140
      IF(IHARG2(1).EQ.'FI')GOTO1150
      IF(IHARG2(1).EQ.'T1')GOTO1160
      IF(IHARG2(1).EQ.'T2')GOTO1170
      IF(IHARG2(1).EQ.'WR')GOTO1180
      IF(IHARG2(1).EQ.'MA')GOTO1190
      IERROR='YES'
      GOTO9000
C
 1110 CONTINUE
      IBUGED=IHARG(2)
      GOTO1900
C
 1120 CONTINUE
      IBUGE2=IHARG(2)
      GOTO1900
C
 1130 CONTINUE
      IBUGE3=IHARG(2)
      GOTO1900
C
 1140 CONTINUE
      IBUGTY=IHARG(2)
      GOTO1900
C
 1150 CONTINUE
      IBUGFI=IHARG(2)
      GOTO1900
C
 1160 CONTINUE
      IBUGT1=IHARG(2)
      GOTO1900
C
 1170 CONTINUE
      IBUGT2=IHARG(2)
      GOTO1900
C
 1180 CONTINUE
      IBUGWR=IHARG(2)
      GOTO1900
C
 1190 CONTINUE
      IBUGMA=IHARG(2)
      GOTO1900
C
 1900 CONTINUE
      GOTO9000
C
C               ********************************
C               **  STEP 21--                 **
C               **  SET SUBROUTINE VARIABLES  **
C               ********************************
C
 2100 CONTINUE
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SET')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISUBRO=IHARG(2)
      GOTO9000
C
C               ********************************
C               **  STEP 31--                 **
C               **  SET OTHER      VARIABLES  **
C               ********************************
C
 3100 CONTINUE
      IBLIM1=ICURLN
      IF(NUMARG.GE.2)IBLIM1=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,3111)IBLIM1
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 3111 FORMAT(10X,'[BEGINNING LINE NUMBER FOR COPY = ',I8,']')
      GOTO9000
C
 3200 CONTINUE
      IBLIM2=ICURLN
      IF(NUMARG.GE.2)IBLIM2=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,3211)IBLIM2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 3211 FORMAT(10X,'[END       LINE NUMBER FOR COPY = ',I8,']')
      GOTO9000
C
 3300 CONTINUE
      IBLIM1=ICURLN
      IBLIM2=ICURLN
      IF(NUMARG.GE.2)IBLIM1=IARG(2)
      IF(NUMARG.GE.3)IBLIM2=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,3311)IBLIM1,IBLIM2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 3311 FORMAT(10X,'[LINE   LIMITS FOR COPY   COMMAND = ',2I8,']')
      GOTO9000
C
 3400 CONTINUE
      IMARK=ICURLN
      IF(NUMARG.GE.2)IMARK=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,3411)IMARK
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 3411 FORMAT(10X,'[MARK SET AT LINE ',I8,']')
      GOTO9000
C
 4110 CONTINUE
      IF(NUMARG.LE.1)ICLIM1=1
      IF(NUMARG.LE.1)ICLIM2=240
      IF(NUMARG.EQ.2)ICLIM1=IARG(2)
      IF(NUMARG.EQ.2)ICLIM2=240
      IF(NUMARG.GE.3)ICLIM1=IARG(2)
      IF(NUMARG.GE.3)ICLIM2=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4111)ICLIM1,ICLIM2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4111 FORMAT(10X,'[COLUMN LIMITS FOR CHANGE COMMAND = ',2I8,']')
      GOTO9000
C
 4120 CONTINUE
      IFLIM1=IARG(2)
      IFLIM2=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4121)IFLIM1,IFLIM2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4121 FORMAT(10X,'[COLUMN LIMITS FOR FIND   COMMAND = ',2I8,']')
      GOTO9000
C
 4130 CONTINUE
      ILLIM1=IARG(2)
      ILLIM2=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4131)ILLIM1,ILLIM2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4131 FORMAT(10X,'[COLUMN LIMITS FOR LOCATE COMMAND = ',2I8,']')
      GOTO9000
C
 4140 CONTINUE
      IMLIM1=IARG(2)
      IMLIM2=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4141)IMLIM1,IMLIM2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4141 FORMAT(10X,'[COLUMN LIMITS FOR MARGIN COMMAND = ',2I8,']')
      GOTO9000
C
 4150 CONTINUE
      IPLIM1=IARG(2)
      IPLIM2=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4151)IPLIM1,IPLIM2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4151 FORMAT(10X,'[COLUMN LIMITS FOR PRINT  COMMAND = ',2I8,']')
      GOTO9000
C
 4160 CONTINUE
      IP2LI1=IARG(2)
      IP2LI2=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4161)IP2LI1,IP2LI2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4161 FORMAT(10X,'[COLUMN LIMITS FOR PAGE   COMMAND = ',2I8,']')
      GOTO9000
C
 4310 CONTINUE
      ISCRO1=IARG(3)
      ISCRO2=IARG(4)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4311)ISCRO1,ISCRO2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4311 FORMAT(10X,'[SCREEN  ROW    LIMITS            = ',2I8,']')
      GOTO9000
C
 4320 CONTINUE
      ISCCO1=IARG(3)
      ISCCO2=IARG(4)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4321)ISCCO1,ISCCO2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4321 FORMAT(10X,'[SCREEN  COLUMN LIMITS            = ',2I8,']')
      GOTO9000
C
 4410 CONTINUE
      IPRRO1=IARG(3)
      IPRRO2=IARG(4)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4411)IPRRO1,IPRRO2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4411 FORMAT(10X,'[(PRINT) ROW    LIMITS            = ',2I8,']')
      GOTO9000
C
 4420 CONTINUE
      IPRCO1=IARG(3)
      IPRCO2=IARG(4)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4421)IPRCO1,IPRCO2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4421 FORMAT(10X,'[PRINTER COLUMN LIMITS            = ',2I8,']')
      GOTO9000
C
CCCCC THE FOLLOWING SECTION WAS ADDED   MAY 1993
 4510 CONTINUE
      LOOPCT=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,4511)LOOPCT
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 4511 FORMAT(10X,'[THE LOOP COUNTER HAS JUST BEEN SET TO ',I8,']')
      GOTO9000
C
 5110 CONTINUE
      IINDEC=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5111)IINDEC
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5111 FORMAT(10X,'[COLUMN FOR INDENT   COMMAND = ',I8,']')
      GOTO9000
C
 5120 CONTINUE
      ICENTC=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5121)ICENTC
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5121 FORMAT(10X,'[COLUMN FOR CENTER   COMMAND = ',I8,']')
      GOTO9000
C
 5130 CONTINUE
      ITRUNC=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5131)ITRUNC
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5131 FORMAT(10X,'[COLUMN FOR TRUNCATE COMMAND = ',I8,']')
      GOTO9000
C
 5140 CONTINUE
      ITABC=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5141)ITABC
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5141 FORMAT(10X,'[COLUMN FOR AUTO-TABBING     = ',I8,']')
      GOTO9000
C
 5150 CONTINUE
      IF(NUMARG.LE.1)ICOLL1=1
      IF(NUMARG.LE.1)ICOLL2=50
      IF(NUMARG.EQ.2)ICOLL1=1
      IF(NUMARG.EQ.2)ICOLL2=IARG(2)
      IF(NUMARG.GE.3)ICOLL1=IARG(2)
      IF(NUMARG.GE.3)ICOLL2=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5151)ICOLL1,ICOLL2
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5151 FORMAT(10X,'[COLUMNS FOR NEAT   COMMAND = ',I8,I8,']')
      GOTO9000
C
 5160 CONTINUE
      IF(NUMARG.LE.1)IJUST='LEFT'
      IF(NUMARG.GE.2)IJUST=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5161)IJUST
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5161 FORMAT(10X,'[SWITCH FOR SPACING (LEFT/BOTH) = ',A4,']')
      GOTO9000
C
 5170 CONTINUE
      ISHIFN=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5171)ISHIFN
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5171 FORMAT(10X,'[NUMBER OF COLUMNS FOR SHIFT   COMMAND = ',I8,']')
      GOTO9000
C
 5180 CONTINUE
      IF(NUMARG.LE.1)IPPOFF=0
      IF(NUMARG.EQ.2)IPPOFF=IARG(2)
      IF(NUMARG.GE.3)IPPOFF=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5181)IPPOFF
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5181 FORMAT(10X,'[OFFSET FOR PP COMMAND = ',I8,']')
      GOTO9000
C
 5190 CONTINUE
      IF(NUMARG.LE.1)IPPLIN=50
      IF(NUMARG.EQ.2)IPPLIN=IARG(2)
      IF(NUMARG.GE.3)IPPLIN=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5191)IPPLIN
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5191 FORMAT(10X,'[LINES FOR PP   COMMAND = ',I8,']')
      GOTO9000
C
 5200 CONTINUE
      IERASW=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5201)IERASW
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5201 FORMAT(10X,'[PRE-ERASE SWITCH FOR   PP   COMMAND = ',A4,']')
      GOTO9000
C
 5210 CONTINUE
      ISEQNU=IARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5211)ISEQNU
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5211 FORMAT(10X,'[SEQUENCE NUMBER FOR LS   COMMAND = ',I8,']')
      GOTO9000
C
 5220 CONTINUE
      IF(NUMARG.LE.1)ILPOFF=0
      IF(NUMARG.EQ.2)ILPOFF=IARG(2)
      IF(NUMARG.GE.3)ILPOFF=IARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,5221)ILPOFF
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 5221 FORMAT(10X,'[OFFSET FOR LP COMMAND = ',I8,']')
      GOTO9000
C
 6110 CONTINUE
      IF(NUMARG.LE.1)IFEESW='ON'
      IF(NUMARG.GE.2)IFEESW=IHARG(2)
CCCCC IF(IFEESW.EQ.'ON')WRITE(ICOUT,6111)IFEESW
CCCCC IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
C6111 FORMAT(10X,'[SWITCH FOR PRINTING FEEDBACK     = ',A4,']')
      GOTO9000
C
 6120 CONTINUE
      IF(NUMARG.LE.1)INUMSW='ON'
      IF(NUMARG.GE.2)INUMSW=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6121)INUMSW
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6121 FORMAT(10X,'[SWITCH FOR PRINTING LINE NUMBERS = ',A4,']')
      GOTO9000
C
 6130 CONTINUE
      IF(NUMARG.LE.1)IPRISW='OFF'
      IF(NUMARG.GE.2)IPRISW=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6131)IPRISW
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6131 FORMAT(10X,'[SWITCH FOR PRINTER ON/OFF          = ',A4,']')
      GOTO9000
C
 6140 CONTINUE
      IF(NUMARG.LE.1)IPROSW='ON'
      IF(NUMARG.GE.2)IPROSW=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6141)IPROSW
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6141 FORMAT(10X,'[SWITCH FOR PRINTING PROMPT       = ',A4,']')
      GOTO9000
C
 6150 CONTINUE
      IF(NUMARG.LE.1)ITRASW='OFF'
      IF(NUMARG.GE.2)ITRASW=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6151)ITRASW
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6151 FORMAT(10X,'[SWITCH FOR TRACING               = ',A4,']')
      GOTO9000
C
 6160 CONTINUE
      IF(NUMARG.LE.1)IECHSW='OFF'
      IF(NUMARG.GE.2)IECHSW=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6161)IECHSW
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6161 FORMAT(10X,'[SWITCH FOR ECHOING COMMANDS      = ',A4,']')
      GOTO9000
C
 6510 CONTINUE
      IF(NUMARG.LE.1)IMASK=':'
      IF(NUMARG.GE.2)IMASK=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6511)IMASK
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6511 FORMAT(10X,'[MASK CHARACTER        = ',A4,']')
      GOTO9000
C
C6520 CONTINUE
CCCCC IF(NUMARG.LE.1)IJUST='LEFT'
CCCCC IF(NUMARG.GE.2)IJUST=IHARG(2)
CCCCC IF(IFEESW.EQ.'ON')WRITE(ICOUT,6521)IJUST
CCCCC IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
C6521 FORMAT(10X,'[JUSTIFICATION         = ',A4,']')
CCCCC GOTO9000
C
 6530 CONTINUE
      IF(NUMARG.LE.2)ISPACH='EQUA'
      IF(NUMARG.GE.3)ISPACH=IHARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6531)ISPACH
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6531 FORMAT(10X,'[HORIZONTAL SPACING    = ',A4,']')
      GOTO9000
C
 6540 CONTINUE
      IF(NUMARG.LE.2)ISPACV='SING'
      IF(NUMARG.GE.3)ISPACV=IHARG(3)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6541)ISPACV
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6541 FORMAT(10X,'[VERTICAL   SPACING    = ',A4,']')
      GOTO9000
C
 6550 CONTINUE
      IF(NUMARG.LE.1)ITRANS='NEAT'
      IF(NUMARG.GE.2)ITRANS=IHARG(2)
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,6551)ITRANS
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 6551 FORMAT(10X,'[TRANSLATION TYPE      = ',A4,']')
      GOTO9000
C
 7100 CONTINUE
      IF(NUMCOM.LE.0)GOTO7118
      DO7110I=1,NUMCOM
      IHTEM8=ICOM3(I)
      IF(IHARG(2).EQ.IHTEM8(1:4).AND.IHARG2(2).EQ.IHTEM8(5:8))GOTO7115
      GOTO7110
 7115 CONTINUE
      I2=I
      GOTO7119
 7110 CONTINUE
 7118 CONTINUE
      I2=NUMCOM+1
      IHTEM8(1:4)=IHARG(2)
      IHTEM8(5:8)=IHARG2(2)
      ICOM3(I2)=IHTEM8
      NUMCOM=I2
      GOTO7119
 7119 CONTINUE
C
      IF(I2.LE.MAXCMN)GOTO7129
      WRITE(ICOUT,7121)
      CALL EDWRST('EDSET ')
 7121 FORMAT('***** ERROR IN EDSET--')
      WRITE(ICOUT,7122)
      CALL EDWRST('EDSET ')
 7122 FORMAT('      TOO MANY USER-DEFINED COMMANDS.')
      WRITE(ICOUT,7123)I2
      CALL EDWRST('EDSET ')
 7123 FORMAT('      NUMBER OF COMMANDS         = ',I8)
      WRITE(ICOUT,7124)MAXCMN
      CALL EDWRST('EDSET ')
 7124 FORMAT('      MAXIMUM NUMBER OF COMMANDS = ',I8)
      IERROR='YES'
      GOTO9000
 7129 CONTINUE
C
      IHTE20='                    '
      J=0
      IF(NUMARG.LE.2)GOTO7135
      DO7130I=3,NUMARG
      IHARGI=IHARG(I)
      CALL EDTRNP(IHARGI,ICHAR1,IHIT)
      J=J+1
      IF(IHIT.EQ.'YES')IHTE20(J:J)=ICHAR1
      IF(IHIT.EQ.'NO')IHTE20(J:J)=IHARGI(1:1)
 7130 CONTINUE
 7135 CONTINUE
      ICOM4(I2)=IHTE20
      NCCOM4(I2)=J
 7139 CONTINUE
C
      IF(I2.LE.0)GOTO7149
      IF(NUMCOM.LE.0.OR.NUMCOM.GT.MAXCMN)GOTO7149
      IF(IFEESW.EQ.'ON')WRITE(ICOUT,7141)ICOM3(I2),ICOM4(I2)
      IF(IFEESW.EQ.'ON')CALL EDWRST('EDSET ')
 7141 FORMAT(10X,'[COMMAND ',A8,' = ',A20,' ]')
 7149 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SET')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSET ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSET ')
 9011 FORMAT('*****AT THE END       OF EDSET--')
      WRITE(ICOUT,9012)ICURLN
      CALL EDWRST('EDSET ')
 9012 FORMAT('ICURLN = ',I8)
      WRITE(ICOUT,9013)ICLIM1,ICLIM2
      CALL EDWRST('EDSET ')
 9013 FORMAT('ICLIM1,ICLIM2 = ',2I8)
      WRITE(ICOUT,9014)I2,NUMCOM,MAXCMN
      CALL EDWRST('EDSET ')
 9014 FORMAT('I2,NUMCOM,MAXCMN = ',3I8)
      WRITE(ICOUT,9015)IHARGI,ICHAR1,IHIT
      CALL EDWRST('EDSET ')
 9015 FORMAT('IHARGI,ICHAR1,IHIT = ',A4,2X,A1,2X,A4)
      WRITE(ICOUT,9018)ITRANS
      CALL EDWRST('EDSET ')
 9018 FORMAT('ITRANS = ',A4)
      WRITE(ICOUT,9019)ILPOFF
      CALL EDWRST('EDSET ')
 9019 FORMAT('ILPOFF = ',I8)
      WRITE(ICOUT,9021)IFEESW,LOOPCT
      CALL EDWRST('EDSET ')
 9021 FORMAT('IFEESW,LOOPCT = ',A4,I8)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSEUC
C
C     PURPOSE--SEARCH FOR USER-DEFINE COMMANDS
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JUNE 1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*240 ICTEMP
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDSE'
      ISUBN2='UC  '
C
      IFOUND='NO'
      IERROR='NO'
C
CCCCC IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SEUC')GOTO90
      IF(ISUBRO.NE.'SEUC')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSEUC')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSEUC')
   51 FORMAT('***** AT THE BEGINNING OF EDSEUC--')
      WRITE(ICOUT,54)ICOM,ICOM2,NUMCOM
      CALL EDWRST('EDSEUC')
   54 FORMAT('ICOM,ICOM2,NUMCOM = ',A4,2X,A4,I8)
      IF(NUMCOM.LE.0)GOTO69
      DO60I=1,NUMCOM
      WRITE(ICOUT,999)
      CALL EDWRST('EDSEUC')
      WRITE(ICOUT,61)ICOM3(I),ICOM4(I),NCOM5(I)
      CALL EDWRST('EDSEUC')
   61 FORMAT('ICOM3(I),ICOM4(I),NCOM5(I) = ',A4,2X,A4,I8)
      WRITE(ICOUT,62)ICOM5(I)
      CALL EDWRST('EDSEUC')
   62 FORMAT('ICOM5(I) = ',A30)
   60 CONTINUE
   69 CONTINUE
      WRITE(ICOUT,72)IBUGE2,ISUBRO,IFOUND,IERROR
      CALL EDWRST('EDSEUC')
   72 FORMAT('IBUGE2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,73)IPRISW,IPRINU
      CALL EDWRST('EDSEUC')
   73 FORMAT('IPRISW,IPRINU = ',A4,I8)
   90 CONTINUE
C
C               **********************************************
C               **     SEARCH FOR USER-DEFINED COMMANDS      *
C               **********************************************
C
      NCTEMP=(-999)
      IF(NUMCOM.LE.0)GOTO9000
C
      DO1110I=1,NUMCOM
      I2=I
      IF(ICOM.EQ.ICOM3(I).AND.ICOM2.EQ.ICOM4(I))GOTO1119
 1110 CONTINUE
      GOTO9000
 1119 CONTINUE
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  COPY OVER THE PRINTER "FILE" VARIABLES)  **
C               **  (IF CALLED FOR)                          **
C               ***********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SEUC')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO2190
C
      IOUNIT=IPRINU
      IFILE=IPRINA
      ISTAT=IPRIST
      IFORM=IPRIFO
      IACCES=IPRIAC
      IREWR=IPRIRW
      ISUBN0='SEUC'
      IERRFI='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SEUC')GOTO2189
      WRITE(ICOUT,999)
      CALL EDWRST('EDSEUC')
      WRITE(ICOUT,2181)
      CALL EDWRST('EDSEUC')
 2181 FORMAT('***** FROM THE MIDDLE OF EDSEUC--')
      WRITE(ICOUT,2183)IFILE
      CALL EDWRST('EDSEUC')
 2183 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,2184)IOUNIT
      CALL EDWRST('EDSEUC')
 2184 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,2185)ISTAT
      CALL EDWRST('EDSEUC')
 2185 FORMAT('ISTAT = ',A12)
 2189 CONTINUE
C
 2190 CONTINUE
C
C               *************************
C               **  STEP 22--          **
C               **  OPEN  THE PRINTER  **
C               **  (IF CALLED FOR)    **
C               *************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SEUC')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO2290
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED',
CCCCC1ERR=2280)
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
      IF(IERRFI.EQ.'YES')GOTO2280
      GOTO2290
C
 2280 CONTINUE
      WRITE(ICOUT,2281)
      CALL EDWRST('EDSEUC')
 2281 FORMAT('***** ERROR IN EDSEUC--')
      WRITE(ICOUT,2282)
      CALL EDWRST('EDSEUC')
 2282 FORMAT('      FAILED IN ATTEMPTING TO OPEN THE PRINTER')
      IERROR='YES'
      GOTO9000
C
 2290 CONTINUE
C
C               **********************************************
C               **  STEP 31--                               **
C               **   WRITE OUT THE USER-DEFINED COMMAND     **
C               **********************************************
C
      NCTEMP=NCOM5(I2)
      IF(NCTEMP.LE.0)GOTO3129
      ICTEMP(001:025)='                         '
      ICTEMP(026:050)='                         '
      ICTEMP(051:075)='                         '
      ICTEMP(076:100)='                         '
      ICTEMP(101:125)='                         '
      ICTEMP(126:240)='                                        '
      ICTEMP(1:30)=ICOM5(I2)
CCCCC IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SEUC')
      IF(ISUBRO.EQ.'SEUC')THEN
         WRITE(ICOUT,3121)I2,NCTEMP,ICTEMP(1:30)
         CALL EDWRST('EDSEUC')
      ENDIF
 3121 FORMAT('I2,NCTEMP,ICTEMP(1:30) = ',2I8,2X,A30)
      IMAX=NCTEMP
      IF(IMAX.GT.238)IMAX=238
      WRITE(ICOUT,3122)(ICTEMP(I:I),I=1,IMAX)
      CALL EDWRST('EDSEUC')
 3122 FORMAT(238A1)
      IF(IPRISW.EQ.'ON')THEN
         WRITE(IOUNIT,3123)(ICTEMP(I:I),I=1,IMAX)
         CALL EDWRST('EDSEUC')
 3123    FORMAT(238A1)
      ENDIF
 3129 CONTINUE
C
      IFOUND='YES'
      IERROR='NO'
C
C               *************************
C               **  STEP 41--          **
C               **  CLOSE THE PRINTER  **
C               **  (IF CALLED FOR)    **
C               *************************
C
      ISTEPN='41'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SEUC')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRISW.EQ.'OFF')GOTO4190
C
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='OFF'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
 4190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
CCCCC IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SEUC')GOTO9090
      IF(ISUBRO.NE.'SEUC')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSEUC')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSEUC')
 9011 FORMAT('***** AT THE END       OF EDSEUC--')
      WRITE(ICOUT,9014)ICOM,ICOM2,NUMCOM
      CALL EDWRST('EDSEUC')
 9014 FORMAT('ICOM,ICOM2,NUMCOM = ',A4,2X,A4,I8)
      IF(NUMCOM.LE.0)GOTO9029
      DO9020I=1,NUMCOM
      WRITE(ICOUT,999)
      CALL EDWRST('EDSEUC')
      WRITE(ICOUT,9021)ICOM3(I),ICOM4(I),NCOM5(I)
      CALL EDWRST('EDSEUC')
 9021 FORMAT('ICOM3(I),ICOM4(I),NCOM5(I) = ',A4,2X,A4,I8)
      WRITE(ICOUT,9022)ICOM5(I)
      CALL EDWRST('EDSEUC')
 9022 FORMAT('ICOM5(I) = ',A30)
 9020 CONTINUE
 9029 CONTINUE
      WRITE(ICOUT,9031)I2,NCTEMP,ICTEMP(1:30)
      CALL EDWRST('EDSEUC')
 9031 FORMAT('I2,NCTEMP,ICTEMP(1:30) = ',2I8,2X,A30)
      WRITE(ICOUT,9032)IMAX
      CALL EDWRST('EDSEUC')
 9032 FORMAT('IMAX = ',I8)
      WRITE(ICOUT,9042)IBUGE2,ISUBRO,IFOUND,IERROR
      CALL EDWRST('EDSEUC')
 9042 FORMAT('IBUGE2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,9043)IPRISW,IPRINU,IOUNIT
      CALL EDWRST('EDSEUC')
 9043 FORMAT('IPRISW,IPRINU,IOUNIT = ',A4,2I8)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSHIF(ISHIFN)
C
C     PURPOSE--SHIFT  CURRENT LINE
C              (AND NEXT IARG1-1 LINES)
C              (OR SHIFT LINES IARG1 THROUGH IARG2)
C      COMMAND SYNTAX--SHIFT
C                      SHIFT <N>
C                      SHIFT <N1> <N2>
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      DIMENSION IOLDLI(240)
      DIMENSION INEWLI(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDSH'
      ISUBN2='IF  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SHIF')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHIF')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSHIF')
   51 FORMAT('***** AT THE BEGINNING OF EDSHIF--')
      WRITE(ICOUT,52)ISHIFN
      CALL EDWRST('EDSHIF')
   52 FORMAT('ISHIFN = ',I8)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ************************************
C               **  STEP 1--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE SHIFTED .  **
C               ************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SHIF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=ICURLN
C
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      GOTO1120
C
 1100 CONTINUE
      ISTART=ICURLN
      ISTOP=ICURLN
      GOTO1190
C
 1110 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      ISTART=ICURLN
      ISTOP=ISTART+IARG1-1
      GOTO1190
C
 1120 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      IARG2=NUMLIN+1
      IF(IARGT(2).EQ.'NUMB')IARG2=IARG(2)
      ISTART=IARG1
      ISTOP=IARG2
      GOTO1190
C
 1190 CONTINUE
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD2.LT.IHOLD1)ISTART=IHOLD2
      IF(IHOLD2.LT.IHOLD1)ISTOP=IHOLD1
C
C               ********************************************
C               **  STEP 21--                             **
C               **  LOOP THROUGH THE LINES TO BE SHIFTED  **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SHIF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ISTART-1
 2100 CONTINUE
      ILINE=ILINE+1
      IF(ILINE.LE.ISTOP)GOTO2910
      ILINE=ISTOP
      GOTO2900
 2910 CONTINUE
      ICURLN=ILINE
      IF(ILINE.LT.1)GOTO2100
      IF(ILINE.GT.NUMLIN)GOTO2900
C
C               *****************************
C               **  STEP 22--              **
C               **  COPY THE OLD    LINE   **
C               *****************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SHIF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      I=0
      DO2200J=J1,J2
      I=I+1
      IOLDLI(I)=ICHA(J)
 2200 CONTINUE
      NUMCOL=I
C
C               ***********************************
C               **  STEP 23--                    **
C               **  APPLY THE DESIRED SHIFTING   **
C               **  TO THE TARGET LINE,          **
C               **  SO AS TO CREATE              **
C               **  A NEW LINE.                  **
C               ***********************************
C
      ISTEPN='23'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SHIF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDSHI2(ISHIFN,IOLDLI,NUMCOL,INEWLI,NUMCNL)
C
C               ***************************************
C               **  STEP 24--                        **
C               **  DETERMINE IF ROOM EXISTS IN THE  **
C               **  MAIN INTERNAL CHARACTER ARRAY    **
C               **  FOR THE NEW SHIFTED  LINE.       **
C               ***************************************
C
      ISTEPN='24'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SHIF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH9=NUMCHA+NUMCNL
      CALL EDCHSI(NUMCH9)
      IF(IERROR.EQ.'YES')GOTO9000
C
 2450 CONTINUE
      IF(ILINE.LT.1)GOTO2460
      IF(ILINE.GT.NUMLIN)GOTO2470
      GOTO2490
C
 2460 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2461)
         CALL EDWRST('EDSHIF')
      ENDIF
 2461 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2462)
         CALL EDWRST('EDSHIF')
      ENDIF
 2462 FORMAT(10X,'[TOP]')
 2469 CONTINUE
      GOTO2100
C
 2470 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2471)
         CALL EDWRST('EDSHIF')
      ENDIF
 2471 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2472)
         CALL EDWRST('EDSHIF')
      ENDIF
 2472 FORMAT(10X,'[BOTTOM]')
 2479 CONTINUE
      GOTO9000
C
 2490 CONTINUE
C
C               ***************************************
C               **  STEP 25--                        **
C               **  UPDATE THE MAIN CHARACTER ARRAY  **
C               **  WITH THE NEW     LINE.           **
C               ***************************************
C
      ISTEPN='25'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SHIF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J1=NUMCHA+1
      N1=NUMCNL
      J2=J1+(N1-1)
C
      NUMRP1=NUMROW+1
      IROW=NUMRP1
      IPOINT(ILINE)=IROW
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO2500J=J1,J2
      I=I+1
      ICHA(J)=INEWLI(I)
 2500 CONTINUE
      NUMROW=NUMRP1
      NUMCHA=J2
C
      IF(IFEESW.EQ.'OFF')GOTO2629
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2621)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDSHIF')
      ENDIF
 2621 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2622)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDSHIF')
      ENDIF
 2622 FORMAT(I6,':',3X,230A1)
 2629 CONTINUE
C
      GOTO2100
C
 2900 CONTINUE
      IF(ILINE.LE.NUMLIN)GOTO2919
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2911)
         CALL EDWRST('EDSHIF')
      ENDIF
 2911 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2912)
         CALL EDWRST('EDSHIF')
      ENDIF
 2912 FORMAT(10X,'[BOTTOM]')
 2919 CONTINUE
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SHIF')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHIF')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSHIF')
 9011 FORMAT('***** AT THE END       OF EDSHIF--')
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHIF')
      WRITE(ICOUT,9012)ISHIFN
      CALL EDWRST('EDSHIF')
 9012 FORMAT('ISHIFN = ',I8)
      WRITE(ICOUT,9013)IWIDTH
      CALL EDWRST('EDSHIF')
 9013 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9014)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDSHIF')
 9014 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDSHIF')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDSHIF')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDSHIF')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDSHIF')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,9041)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDSHIF')
 9041 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9042)J1,N1,J2
      CALL EDWRST('EDSHIF')
 9042 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHIF')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSHI2(ISHIFN,IOLDLI,NUMCOL,INEWLI,NUMCNL)
C
C     PURPOSE--SHIFT THE LINE IN IOLDLI(.)  OVER    ISHIFN   SPACES.
C              THE SHIFTED LINE WILL BE PLACED IN INEWLI(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      DIMENSION IOLDLI(*)
      DIMENSION INEWLI(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDSH'
      ISUBN2='I2  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SHI2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHI2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSHI2')
   51 FORMAT('***** AT THE BEGINNING OF EDSHI2--')
      WRITE(ICOUT,52)ISHIFN
      CALL EDWRST('EDSHI2')
   52 FORMAT('ISHIFN = ',I8)
      WRITE(ICOUT,71)NUMCOL
      CALL EDWRST('EDSHI2')
   71 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,72)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDSHI2')
   72 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,73)NUMCNL
      CALL EDWRST('EDSHI2')
   73 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,74)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDSHI2')
   74 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,81)ISHIFN
      CALL EDWRST('EDSHI2')
   81 FORMAT('ISHIFN = ',I8)
   90 CONTINUE
C
C               ************************************************************
C               **  STEP 1--                                              **
C               **  DETERMINE THE FIRST NON-BLANK CHARACTER IN IOLDLI(.)  **
C               ************************************************************
C
      I2=0
      IF(NUMCOL.LE.0)GOTO1190
      DO1100I=1,NUMCOL
      I2=I
      IF(IOLDLI(I2).NE.' ')GOTO1190
 1100 CONTINUE
      I2=NUMCOL+1
 1190 CONTINUE
      IFIRST=I2
C
C               ********************************
C               **  STEP 2--                  **
C               **  BLANK OUT THE NEW STRING  **
C               ********************************
C
      NUMCNL=1
      DO1500I=1,240
      INEWLI(I)=' '
 1500 CONTINUE
C
C               **************************************
C               **  STEP 3--                        **
C               **  FORM THE NEW (SHIFTED) STRING   **
C               **************************************
C
      IF(ISHIFN.LE.0)GOTO2100
      IF(ISHIFN.EQ.0)GOTO2200
      GOTO2300
C
 2100 CONTINUE
      INEW=ISHIFN
      IF(NUMCOL.LE.0)GOTO2190
      DO2110IOLD=1,NUMCOL
      INEW=INEW+1
      IF(INEW.LE.0)GOTO2110
      INEWLI(INEW)=IOLDLI(IOLD)
 2110 CONTINUE
 2190 CONTINUE
      IF(INEW.LE.0)INEW=0
      NUMCNL=INEW
      GOTO2900
C
 2200 CONTINUE
      INEW=0
      IF(NUMCOL.LE.0)GOTO2290
      DO2210IOLD=1,NUMCOL
      INEW=INEW+1
      INEWLI(INEW)=IOLDLI(IOLD)
 2210 CONTINUE
 2290 CONTINUE
      NUMCNL=INEW
      GOTO2900
C
 2300 CONTINUE
      INEW=ISHIFN
      IF(NUMCOL.LE.0)GOTO2390
      DO2310IOLD=1,NUMCOL
      INEW=INEW+1
      IF(INEW.GT.240)GOTO2390
      INEWLI(INEW)=IOLDLI(IOLD)
 2310 CONTINUE
 2390 CONTINUE
      IF(INEW.GT.240)INEW=240
      NUMCNL=INEW
      GOTO2900
C
 2900 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SHI2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHI2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSHI2')
 9011 FORMAT('***** AT THE END       OF EDSHI2--')
      WRITE(ICOUT,9012)ISHIFN
      CALL EDWRST('EDSHI2')
 9012 FORMAT('ISHIFN = ',I8)
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDSHI2')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDSHI2')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDSHI2')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDSHI2')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSHLE(ISHIFT)
C
C     PURPOSE--SHIFT TO THE LEFT (ONLY)
C              THE IHARG,IHARG2,IARG,ARG, AND IARGT VECTORS
C              AND ADJUST THE VALUE OF NUMARG ACCORDINGLY.
C              THE ADJUSTMENT RESULTS IN
C              ALL ELEMENTS BEING SHIFTED
C              ISHIFT STEPS TO THE LEFT.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY   1985.
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUGMA.EQ.'OFF'.AND.ISUBRO.NE.'SHLE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHLE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSHLE')
   51 FORMAT('***** AT THE BEGINNING OF EDSHLE--')
      WRITE(ICOUT,52)ISHIFT,NUMARG
      CALL EDWRST('EDSHLE')
   52 FORMAT('ISHIFT,NUMARG = ',2I8)
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I)
      CALL EDWRST('EDSHLE')
   56 FORMAT('I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I) = ',
     1I8,2X,A4,A4,I8,E15.7,2X,A4)
   55 CONTINUE
   90 CONTINUE
C
      IMIN=1
      IMAX=NUMARG-ISHIFT
      DO100I=IMIN,IMAX
      IPSHIF=I+ISHIFT
      IHARG(I)=IHARG(IPSHIF)
      IHARG2(I)=IHARG2(IPSHIF)
      IARG(I)=IARG(IPSHIF)
      ARG(I)=ARG(IPSHIF)
      IARGT(I)=IARGT(IPSHIF)
  100 CONTINUE
      NUMARG=IMAX
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGMA.EQ.'OFF'.AND.ISUBRO.NE.'SHLE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHLE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSHLE')
 9011 FORMAT('***** AT THE END       OF EDSHLE--')
      WRITE(ICOUT,9012)ISHIFT,NUMARG
      CALL EDWRST('EDSHLE')
 9012 FORMAT('ISHIFT,NUMARG = ',2I8)
      WRITE(ICOUT,9013)IMIN,IMAX
      CALL EDWRST('EDSHLE')
 9013 FORMAT('IMIN,IMAX = ',2I8)
      DO9015I=1,NUMARG
      WRITE(ICOUT,9016)I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I)
      CALL EDWRST('EDSHLE')
 9016 FORMAT('I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I) = ',
     1I8,2X,A4,A4,I8,E15.7,2X,A4)
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSHOW
C
C     PURPOSE--SHOW SYSTEM SWITCHES AND VARIABLES.
C
C     DATE--OCTOBER 24,1985
C
C-------------------------------------------------------------------------------
C
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      ISUBN1='EDSH'
      ISUBN2='OW  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SHOW')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHOW')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSHOW')
   51 FORMAT('*****AT THE BEGINNING OF EDSHOW--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IF(NUMARG.LE.0)IERROR='YES'
      IF(NUMARG.LE.0)GOTO9000
C
      IF(IHARG(1).EQ.'IBUG')GOTO1100
      IF(IHARG(1).EQ.'ISUB')GOTO2100
C
      IF(IHARG(1).EQ.'FILE')GOTO3000
C
      IF(IHARG(1).EQ.'BEGI')GOTO3100
      IF(IHARG(1).EQ.'END')GOTO3200
      IF(IHARG(1).EQ.'COPY')GOTO3300
C
      IF(IHARG(1).EQ.'CHAN')GOTO4100
      IF(IHARG(1).EQ.'FIND')GOTO4200
      IF(IHARG(1).EQ.'LOCA')GOTO4300
      IF(IHARG(1).EQ.'MARG')GOTO4400
      IF(IHARG(1).EQ.'PAGE')GOTO4500
      IF(IHARG(1).EQ.'PRIN'.AND.IHARG2(1).EQ.'T   ')GOTO4600
      IF(IHARG(1).EQ.'TAB')GOTO4700
C
      IF(IHARG(1).EQ.'INDE')GOTO5100
      IF(IHARG(1).EQ.'CENT')GOTO5200
      IF(IHARG(1).EQ.'TRUN')GOTO5300
C
      IF(IHARG(1).EQ.'FEED')GOTO6100
      IF(IHARG(1).EQ.'NUMB')GOTO6200
      IF(IHARG(1).EQ.'PRIN'.AND.IHARG2(1).EQ.'T   ')GOTO6300
      IF(IHARG(1).EQ.'PROM')GOTO6400
      IF(IHARG(1).EQ.'TRAN')GOTO6500
C
      IF(IHARG(1).EQ.'COMM')GOTO7100
      IF(IHARG(1).EQ.'INLI')GOTO7200
C
      IF(IHARG(1).EQ.'LIMI')GOTO8100
      IF(IHARG(1).EQ.'SWIT')GOTO8300
C
      GOTO9000
C
C               *************************
C               **  STEP 11--          **
C               **  SET BUG VARIABLES  **
C               *************************
C
 1100 CONTINUE
C
      ISTEPN='11'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SET')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHARG2(1).EQ.'ED')GOTO1110
      IF(IHARG2(1).EQ.'E2')GOTO1120
      IF(IHARG2(1).EQ.'E3')GOTO1130
      IF(IHARG2(1).EQ.'TY')GOTO1140
      IF(IHARG2(1).EQ.'FI')GOTO1150
      IF(IHARG2(1).EQ.'T1')GOTO1160
      IF(IHARG2(1).EQ.'T2')GOTO1170
      IF(IHARG2(1).EQ.'WR')GOTO1180
      IF(IHARG2(1).EQ.'MA')GOTO1190
      IERROR='YES'
      GOTO9000
C
 1110 CONTINUE
      IBUGED=IHARG(2)
      GOTO1900
C
 1120 CONTINUE
      IBUGE2=IHARG(2)
      GOTO1900
C
 1130 CONTINUE
      IBUGE3=IHARG(2)
      GOTO1900
C
 1140 CONTINUE
      IBUGTY=IHARG(2)
      GOTO1900
C
 1150 CONTINUE
      IBUGFI=IHARG(2)
      GOTO1900
C
 1160 CONTINUE
      IBUGT1=IHARG(2)
      GOTO1900
C
 1170 CONTINUE
      IBUGT2=IHARG(2)
      GOTO1900
C
 1180 CONTINUE
      IBUGWR=IHARG(2)
      GOTO1900
C
 1190 CONTINUE
      IBUGMA=IHARG(2)
      GOTO1900
C
 1900 CONTINUE
      GOTO9000
C
C               ********************************
C               **  STEP 21--                 **
C               **  SET SUBROUTINE VARIABLES  **
C               ********************************
C
 2100 CONTINUE
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'SET')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISUBRO=IHARG(2)
      GOTO9000
C
C               ********************************
C               **  STEP 31--                 **
C               **  SET OTHER      VARIABLES  **
C               ********************************
C
 3000 CONTINUE
      WRITE(ICOUT,3011)IORINA
      CALL EDWRST('EDSHOW')
 3011 FORMAT(10X,'[FILE BEING EDITED = ',A80,']')
      GOTO9000
C
 3100 CONTINUE
      WRITE(ICOUT,3111)IBLIM1
      CALL EDWRST('EDSHOW')
 3111 FORMAT(10X,'[BEGINNING LINE NUMBER FOR COPY = ',I8,']')
      GOTO9000
C
 3200 CONTINUE
      WRITE(ICOUT,3211)IBLIM2
      CALL EDWRST('EDSHOW')
      GOTO9000
 3211 FORMAT(10X,'[END       LINE NUMBER FOR COPY = ',I8,']')
C
 3300 CONTINUE
      WRITE(ICOUT,3311)IBLIM1,IBLIM2
      CALL EDWRST('EDSHOW')
 3311 FORMAT(10X,'[LINE   LIMITS FOR COPY   COMMAND   = ',2I8,']')
      GOTO9000
C
 4100 CONTINUE
      WRITE(ICOUT,4111)ICLIM1,ICLIM2
      CALL EDWRST('EDSHOW')
 4111 FORMAT(10X,'[COLUMN LIMITS FOR CHANGE COMMAND   = ',2I8,']')
      GOTO9000
C
 4200 CONTINUE
      WRITE(ICOUT,4211)IFLIM1,IFLIM2
      CALL EDWRST('EDSHOW')
 4211 FORMAT(10X,'[COLUMN LIMITS FOR FIND   COMMAND   = ',2I8,']')
      GOTO9000
C
 4300 CONTINUE
      WRITE(ICOUT,4311)ILLIM1,ILLIM2
      CALL EDWRST('EDSHOW')
 4311 FORMAT(10X,'[COLUMN LIMITS FOR LOCATE COMMAND   = ',2I8,']')
      GOTO9000
C
 4400 CONTINUE
      WRITE(ICOUT,4411)IMLIM1,IMLIM2
      CALL EDWRST('EDSHOW')
 4411 FORMAT(10X,'[COLUMN LIMITS FOR MARGIN COMMAND   = ',2I8,']')
      GOTO9000
C
 4500 CONTINUE
      WRITE(ICOUT,4511)IP2LI1,IP2LI2
      CALL EDWRST('EDSHOW')
 4511 FORMAT(10X,'[COLUMN LIMITS FOR PAGE   COMMAND   = ',2I8,']')
      GOTO9000
C
 4600 CONTINUE
      WRITE(ICOUT,4611)IPLIM1,IPLIM2
      CALL EDWRST('EDSHOW')
 4611 FORMAT(10X,'[COLUMN LIMITS FOR PRINT  COMMAND   = ',2I8,']')
      GOTO9000
C
 4700 CONTINUE
      WRITE(ICOUT,4711)ITABC
      CALL EDWRST('EDSHOW')
 4711 FORMAT(10X,'[COLUMN        FOR AUTO-TABBING     = ',I8,']')
      GOTO9000
C
 5100 CONTINUE
      WRITE(ICOUT,5111)IINDEC
      CALL EDWRST('EDSHOW')
 5111 FORMAT(10X,'[COLUMN        FOR INDENT   COMMAND = ',I8,']')
      GOTO9000
C
 5200 CONTINUE
      WRITE(ICOUT,5211)ICENTC
      CALL EDWRST('EDSHOW')
 5211 FORMAT(10X,'[COLUMN        FOR CENTER   COMMAND = ',I8,']')
      GOTO9000
C
 5300 CONTINUE
      WRITE(ICOUT,5311)ITRUNC
      CALL EDWRST('EDSHOW')
 5311 FORMAT(10X,'[COLUMN        FOR TRUNCATE COMMAND = ',I8,']')
      GOTO9000
C
 6100 CONTINUE
      WRITE(ICOUT,6111)IFEESW
      CALL EDWRST('EDSHOW')
 6111 FORMAT(10X,'[SWITCH FOR PRINTING FEEDBACK     = ',A4,']')
      GOTO9000
C
 6200 CONTINUE
      WRITE(ICOUT,6211)IPRISW
      CALL EDWRST('EDSHOW')
 6211 FORMAT(10X,'[SWITCH FOR PRINTING EDSHOWX         = ',A4,']')
      GOTO9000
C
 6300 CONTINUE
      WRITE(ICOUT,6311)INUMSW
      CALL EDWRST('EDSHOW')
 6311 FORMAT(10X,'[SWITCH FOR PRINTING LINE NUMBERS = ',A4,']')
      GOTO9000
C
 6400 CONTINUE
      WRITE(ICOUT,6411)IPROSW
      CALL EDWRST('EDSHOW')
 6411 FORMAT(10X,'[SWITCH FOR PRINTING PROMPT       = ',A4,']')
      GOTO9000
C
 6500 CONTINUE
      WRITE(ICOUT,6511)ITRANS
      CALL EDWRST('EDSHOW')
 6511 FORMAT(10X,'[SWITCH FOR TRANSLATION TYPE      = ',A4,']')
      GOTO9000
C
 7100 CONTINUE
      WRITE(ICOUT,7101)NUMCOM
      CALL EDWRST('EDSHOW')
 7101 FORMAT(10X,'[NUMBER OF USER-DEFINED COMMANDS = ',I8,']')
      IF(NUMCOM.LE.0)GOTO7119
      WRITE(ICOUT,7102)
      CALL EDWRST('EDSHOW')
 7102 FORMAT(10X,'[-----------------------------------------',']')
      WRITE(ICOUT,7103)
      CALL EDWRST('EDSHOW')
 7103 FORMAT(10X,'[COMMAND   TRANSLATED STRING   NUMB. OF CHAR.]')
      WRITE(ICOUT,7104)
      CALL EDWRST('EDSHOW')
 7104 FORMAT(10X,'[-----------------------------------------',']')
      DO7110I=1,NUMCOM
      WRITE(ICOUT,7111)ICOM3(I),ICOM4(I),NCCOM4(I)
      CALL EDWRST('EDSHOW')
 7111 FORMAT(10X,A8,5X,A20,5X,I8,']')
 7110 CONTINUE
      WRITE(ICOUT,7112)
      CALL EDWRST('EDSHOW')
 7112 FORMAT(10X,'[-----------------------------------------',']')
 7119 CONTINUE
      GOTO9000
C
 7200 CONTINUE
      GOTO9000
C
 8100 CONTINUE
      WRITE(ICOUT,8111)IBLIM1,IBLIM2
      CALL EDWRST('EDSHOW')
 8111 FORMAT(10X,'[LINE   LIMITS FOR COPY   COMMAND   = ',2I8,']')
      WRITE(ICOUT,8121)ICLIM1,ICLIM2
      CALL EDWRST('EDSHOW')
 8121 FORMAT(10X,'[COLUMN LIMITS FOR CHANGE COMMAND   = ',2I8,']')
      WRITE(ICOUT,8131)IFLIM1,IFLIM2
      CALL EDWRST('EDSHOW')
 8131 FORMAT(10X,'[COLUMN LIMITS FOR FIND   COMMAND   = ',2I8,']')
      WRITE(ICOUT,8141)ILLIM1,ILLIM2
      CALL EDWRST('EDSHOW')
 8141 FORMAT(10X,'[COLUMN LIMITS FOR LOCATE COMMAND   = ',2I8,']')
      WRITE(ICOUT,8151)IMLIM1,IMLIM2
      CALL EDWRST('EDSHOW')
 8151 FORMAT(10X,'[COLUMN LIMITS FOR MARGIN COMMAND   = ',2I8,']')
      WRITE(ICOUT,8161)IP2LI1,IP2LI2
      CALL EDWRST('EDSHOW')
 8161 FORMAT(10X,'[COLUMN LIMITS FOR PAGE   COMMAND   = ',2I8,']')
      WRITE(ICOUT,8171)IPLIM1,IPLIM2
      CALL EDWRST('EDSHOW')
 8171 FORMAT(10X,'[COLUMN LIMITS FOR PRINT  COMMAND   = ',2I8,']')
      WRITE(ICOUT,8181)ITABC
      CALL EDWRST('EDSHOW')
 8181 FORMAT(10X,'[COLUMN        FOR AUTO-TABBING     = ',I8,']')
      WRITE(ICOUT,8191)IINDEC
      CALL EDWRST('EDSHOW')
 8191 FORMAT(10X,'[COLUMN        FOR INDENT   COMMAND = ',I8,']')
      WRITE(ICOUT,8201)ICENTC
      CALL EDWRST('EDSHOW')
 8201 FORMAT(10X,'[COLUMN        FOR CENTER   COMMAND = ',I8,']')
      WRITE(ICOUT,8211)ITRUNC
      CALL EDWRST('EDSHOW')
 8211 FORMAT(10X,'[COLUMN        FOR TRUNCATE COMMAND = ',I8,']')
      GOTO9000
C
 8300 CONTINUE
      WRITE(ICOUT,8311)IFEESW
      CALL EDWRST('EDSHOW')
 8311 FORMAT(10X,'[SWITCH FOR PRINTING FEEDBACK     = ',A4,']')
      WRITE(ICOUT,8321)IPRISW
      CALL EDWRST('EDSHOW')
 8321 FORMAT(10X,'[SWITCH FOR PRINTING XXXX         = ',A4,']')
      WRITE(ICOUT,8331)INUMSW
      CALL EDWRST('EDSHOW')
 8331 FORMAT(10X,'[SWITCH FOR PRINTING LINE NUMBERS = ',A4,']')
      WRITE(ICOUT,8341)IPROSW
      CALL EDWRST('EDSHOW')
 8341 FORMAT(10X,'[SWITCH FOR PRINTING PROMPT       = ',A4,']')
      WRITE(ICOUT,8351)ITRANS
      CALL EDWRST('EDSHOW')
 8351 FORMAT(10X,'[SWITCH FOR TRANSLATION TYPE      = ',A4,']')
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'SET')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSHOW')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSHOW')
 9011 FORMAT('*****AT THE END       OF EDSET--')
      WRITE(ICOUT,9012)ICURLN
      CALL EDWRST('EDSHOW')
 9012 FORMAT('ICURLN = ',I8)
      WRITE(ICOUT,9013)ICLIM1,ICLIM2
      CALL EDWRST('EDSHOW')
 9013 FORMAT('ICLIM1,ICLIM2 = ',2I8)
      WRITE(ICOUT,9014)ITRANS
      CALL EDWRST('EDSHOW')
 9014 FORMAT('ITRANS = ',A4)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDSTAT
C
C     PURPOSE--DUMP (= PRINT) STATUS OF CERTAIN VARIABLES
C
C     DATE--JANUARY 24,1985
C
C---------------------------------------------------------------
C
      CHARACTER*1 ICJUNK
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      ISUBN1='EDST'
      ISUBN2='AT  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      J2=(-999)
      N1=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'STAT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDSTAT')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDSTAT')
   51 FORMAT('***** AT THE BEGINNING OF EDSTAT--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************
C               **  STEP 1--           **
C               **  DUMP STATUS        **
C               *************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'STAT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDSTAT')
      IF(ICURLN.LT.1)GOTO1110
      IF(ICURLN.GT.NUMLIN)GOTO1110
      IROW=IPOINT(ICURLN)
      J1=ILOCC1(ICURLN)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
 1110 CONTINUE
C
      IF(ICURLN.GE.1.AND.ICURLN.LE.NUMLIN)THEN
         WRITE(ICOUT,1121)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDSTAT')
      ENDIF
 1121 FORMAT('CURRENT LINE              = ',80A1)
      WRITE(ICOUT,1122)ICURLN
      CALL EDWRST('EDSTAT')
 1122 FORMAT('CURRENT LINE NUMBER       = ',I8)
      WRITE(ICOUT,1123)J1
      CALL EDWRST('EDSTAT')
 1123 FORMAT('CURRENT START BYTE NUBMER = ',I8)
      WRITE(ICOUT,1124)J2
      CALL EDWRST('EDSTAT')
 1124 FORMAT('CURRENT STOP  BYTE NUMBER = ',I8)
      IF(ICURLN.GE.1.AND.ICURLN.LE.NUMLIN)THEN
         WRITE(ICOUT,1125)N1
         CALL EDWRST('EDSTAT')
      ENDIF
 1125 FORMAT('LENGTH OF CURRENT LINE    = ',I8)
      WRITE(ICOUT,1126)NUMLIN
      CALL EDWRST('EDSTAT')
 1126 FORMAT('CURRENT TOTAL LINES       = ',I8)
      WRITE(ICOUT,1127)NUMROW
      CALL EDWRST('EDSTAT')
 1127 FORMAT('CURRENT WORKSPACE LINES   = ',I8)
      WRITE(ICOUT,1128)MAXROW
      CALL EDWRST('EDSTAT')
 1128 FORMAT('ALLOWABLE WORKSPACE LINES = ',I8)
      ANUMRO=NUMROW
      AMAXRO=MAXROW
      RATIO=ANUMRO/AMAXRO
      PRATIO=RATIO*100.0
      WRITE(ICOUT,1131)PRATIO
      CALL EDWRST('EDSTAT')
 1131 FORMAT('WORKSPACE LINES % FULL    = ',F8.2,' %')
C
      WRITE(ICOUT,1144)NUMCHA
      CALL EDWRST('EDSTAT')
 1144 FORMAT('CURRENT TOTAL CHARS       = ',I8)
CCCCC WRITE(ICOUT,1145)NUMCH9
C1145 FORMAT('CURRENT WORKSPACE CHARS   = ',I8)
      WRITE(ICOUT,1146)MAXEDC
      CALL EDWRST('EDSTAT')
 1146 FORMAT('ALLOWABLE WORKSPACE CHARS = ',I8)
      ANUMCH=NUMCHA
      AMAXCH=MAXEDC
      RATIO=ANUMCH/AMAXCH
      PRATIO=RATIO*100.0
      WRITE(ICOUT,1147)PRATIO
      CALL EDWRST('EDSTAT')
 1147 FORMAT('WORKSPACE CHARS % FULL    = ',F8.2,' %')
      WRITE(ICOUT,999)
      CALL EDWRST('EDSTAT')
      WRITE(ICOUT,1148)
      CALL EDWRST('EDSTAT')
 1148 FORMAT(40X,'More:  hit any key ...')
      READ(IRD,1149)ICJUNK
 1149 FORMAT(A1)
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDSTAT')
      WRITE(ICOUT,1151)IMASK
      CALL EDWRST('EDSTAT')
 1151 FORMAT('MASK     CHARACTER = ',A1)
      WRITE(ICOUT,1152)IINFIN
      CALL EDWRST('EDSTAT')
 1152 FORMAT('INFINITY CHARACTER = ',A1)
      WRITE(ICOUT,1153)IPROSW
      CALL EDWRST('EDSTAT')
 1153 FORMAT('PROMPT             = ',A4)
      WRITE(ICOUT,1154)IFEESW
      CALL EDWRST('EDSTAT')
 1154 FORMAT('FEEDBACK           = ',A4)
      WRITE(ICOUT,1155)INUMSW
      CALL EDWRST('EDSTAT')
 1155 FORMAT('AUTO-NUMBERING     = ',A4)
      WRITE(ICOUT,1156)ITRASW
      CALL EDWRST('EDSTAT')
 1156 FORMAT('TRACE              = ',A4)
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDSTAT')
      WRITE(ICOUT,1161)ICLIM1,ICLIM2
      CALL EDWRST('EDSTAT')
 1161 FORMAT('COLUMN LIMITS FOR CHANGE   = ',2I8)
      WRITE(ICOUT,1162)IPLIM1,IPLIM2
      CALL EDWRST('EDSTAT')
 1162 FORMAT('COLUMN LIMITS FOR PRINT    = ',2I8)
      WRITE(ICOUT,1163)IMLIM1,IMLIM2
      CALL EDWRST('EDSTAT')
 1163 FORMAT('COLUMN LIMITS FOR MARGIN   = ',2I8)
      WRITE(ICOUT,1164)ITABC
      CALL EDWRST('EDSTAT')
 1164 FORMAT('COLUMN FOR TAB             = ',I8)
      WRITE(ICOUT,1165)ISCRO1,ISCRO2
      CALL EDWRST('EDSTAT')
 1165 FORMAT('SCREEN ROWS                = ',2I8)
      WRITE(ICOUT,1166)ISCCO1,ISCCO2
      CALL EDWRST('EDSTAT')
 1166 FORMAT('SCREEN COLUMNS             = ',2I8)
      WRITE(ICOUT,1167)IJUST
      CALL EDWRST('EDSTAT')
 1167 FORMAT('JUSTIFICATION              = ',A4)
      WRITE(ICOUT,1168)ISPACH,ISPACV
      CALL EDWRST('EDSTAT')
 1168 FORMAT('SPACING                    = ',A4,2X,A4)
CCCCC WRITE(ICOUT,1169)IFORMF
C1169 FORMAT('FORM FEED CHARACTER STRING = ',A4)
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDSTAT')
      WRITE(ICOUT,1171)IBLIM1,IBLIM2
      CALL EDWRST('EDSTAT')
 1171 FORMAT('BLOCK LINE NUMBERS      = ',2I8)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'STAT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDSTAT')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDSTAT')
 9011 FORMAT('***** AT THE END       OF EDSTAT--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDTOP
C
C     PURPOSE--GO TO TOP OF FILE
C              (= LINE 0 = "LINE" BEFORE TOPT VISIBLE LINE).
C
C     NOTE--NO ARGUMENTS ARE EXPECTED OR PROCESSED.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------
C
      CHARACTER*4 ID
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDTO'
      ISUBN2='P   '
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'TOP')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDTOP ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDTOP ')
   51 FORMAT('***** AT THE BEGINNING OF EDTOP--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               ***************************************
C               **  STEP 1--                         **
C               **  WRITE WORKSPACE OUT TO THE FILE  **
C               ***************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'TOP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ID='SAVE'
      IF(IORINA.NE.ICOMNA.AND.IORINA.NE.ISAVNA)
     1CALL EDWRWF(ID)
C
C               ************************************************
C               **  STEP 2--                                  **
C               **  REWIND THE COMMAND-SAVE (= JOURNAL) FILE  **
C               ************************************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'TOP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IORINA.NE.ICOMNA.AND.IORINA.NE.ISAVNA)
     1REWIND ICOMNU
C
C               *************************
C               **  STEP 3--           **
C               **  GO TO LINE 0       **
C               *************************
C
      ISTEPN='3'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'TOP')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=0
      IF(IFEESW.EQ.'OFF')GOTO1159
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1151)
         CALL EDWRST('EDTOP ')
      ENDIF
 1151 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1152)
         CALL EDWRST('EDTOP ')
      ENDIF
 1152 FORMAT(10X,'[TOP]')
 1159 CONTINUE
      ICURLN=ILINE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'TOP')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDTOP ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDTOP ')
 9011 FORMAT('***** AT THE END       OF EDTOP--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDTRA1
C
C     PURPOSE--PRINT TRACE/DEBUG INFORMATION AT THE BEGINNING
C              OF VARIOUS SUBROUTINES.
C
C     DATE--JANUARY 19,1985
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      WRITE(ICOUT,52)IBUGE2,IFOUND,IERROR,IFEESW
      CALL EDWRST('EDTRA1')
   52 FORMAT('IBUGE2,IFOUND,IERROR,IFEESW = ',A4,2X,A4,2X,A4,2X,A4)
      WRITE(ICOUT,53)IWIDTH
      CALL EDWRST('EDTRA1')
   53 FORMAT('IWIDTH = ',I8)
      IF(IWIDTH.GE.1)WRITE(ICOUT,54)(IANS(I),I=1,IWIDTH)
      IF(IWIDTH.GE.1)CALL EDWRST('EDTRA1')
   54 FORMAT('(IANS(I),I=1,IWIDTH) = ',80A1)
      WRITE(ICOUT,55)ICOM,ICOM2,ICOMT,ACOM,ICOMI
      CALL EDWRST('EDTRA1')
   55 FORMAT('ICOM,ICOM2,ICOMT,ACOM,ICOMI = ',
     1A4,2X,A4,2X,A4,E15.7,I8)
      WRITE(ICOUT,56)NUMARG
      CALL EDWRST('EDTRA1')
   56 FORMAT('NUMARG = ',I8)
      IF(NUMARG.LE.0)GOTO59
      DO57I=1,NUMARG
      WRITE(ICOUT,58)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
      CALL EDWRST('EDTRA1')
   58 FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,I8,E15.7)
   57 CONTINUE
   59 CONTINUE
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRA1')
  999 FORMAT(1X)
      WRITE(ICOUT,61)ICURLN,IPRISW,IMODE
      CALL EDWRST('EDTRA1')
   61 FORMAT('ICURLN,IPRISW,IMODE = ',I8,2X,A4,2X,A4)
      WRITE(ICOUT,62)IBLIM1,IBLIM2
      CALL EDWRST('EDTRA1')
   62 FORMAT('IBLIM1,IBLIM2 = ',2I8)
      WRITE(ICOUT,63)NUMLIN,MAXEDL
      CALL EDWRST('EDTRA1')
   63 FORMAT('NUMLIN,MAXEDL = ',3I8)
      WRITE(ICOUT,64)NUMROW,MAXROW
      CALL EDWRST('EDTRA1')
   64 FORMAT('NUMROW,MAXROW = ',3I8)
      DO65I=1,NUMROW
      WRITE(ICOUT,66)I,IPOINT(I),ILOCC1(I),NUMCPL(I)
      CALL EDWRST('EDTRA1')
   66 FORMAT('I,IPOINT(I),ILOCC1(I),NUMCPL(I) = ',4I8)
   65 CONTINUE
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRA1')
      WRITE(ICOUT,71)NUMCHA,MAXEDC
      CALL EDWRST('EDTRA1')
   71 FORMAT('NUMCHA,MAXEDC        = ',2I8)
      WRITE(ICOUT,72)
      CALL EDWRST('EDTRA1')
   72 FORMAT('ICHA(.)--')
      IF(NUMCHA.GE.1)THEN
         WRITE(ICOUT,73)(ICHA(I),I=1,NUMCHA)
         CALL EDWRST('EDTRA1')
      ENDIF
   73 FORMAT(50A1)
C
      WRITE(ICOUT,81)IORINU,IORIST,IORINA
      CALL EDWRST('EDTRA1')
   81 FORMAT('IORINU,IORIST,IORINA = ',I8,A12,A80)
      WRITE(ICOUT,82)ISAVNU,ISAVST,ISAVNA
      CALL EDWRST('EDTRA1')
   82 FORMAT('ISAVNU,ISAVST,ISAVNA = ',I8,A12,A80)
      WRITE(ICOUT,83)IHELNU,IHELST,IHELNA
      CALL EDWRST('EDTRA1')
   83 FORMAT('IHELNU,IHELST,IHELNA = ',I8,A12,A80)
      WRITE(ICOUT,84)ICOPNU,ICOPST,ICOPNA
      CALL EDWRST('EDTRA1')
   84 FORMAT('ICOPNU,ICOPST,ICOPNA = ',I8,A12,A80)
C
      WRITE(ICOUT,91)NCHH1,NCHH2,NCHH3,NCHH4,NCHH5
      CALL EDWRST('EDTRA1')
   91 FORMAT('NCHH1,NCHH2,NCHH3,NCHH4,NCHH5 = ',5I8)
      WRITE(ICOUT,92)NCHH6,NCHH8,NCHH9,NCHH10
      CALL EDWRST('EDTRA1')
   92 FORMAT('NCHH6,NCHH8,NCHH9,NCHH10 = ',4I8)
      IF(NCHH1.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS1(I:I),I=1,NCHH1)
         CALL EDWRST('EDTRA1')
      ENDIF
   93 FORMAT(80A1)
      IF(NCHH2.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS2(I:I),I=1,NCHH2)
         CALL EDWRST('EDTRA1')
      ENDIF
      IF(NCHH3.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS3(I:I),I=1,NCHH3)
         CALL EDWRST('EDTRA1')
      ENDIF
      IF(NCHH4.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS4(I:I),I=1,NCHH4)
         CALL EDWRST('EDTRA1')
      ENDIF
      IF(NCHH5.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS5(I:I),I=1,NCHH5)
         CALL EDWRST('EDTRA1')
      ENDIF
      IF(NCHH6.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS6(I:I),I=1,NCHH6)
         CALL EDWRST('EDTRA1')
      ENDIF
      IF(NCHH7.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS7(I:I),I=1,NCHH7)
         CALL EDWRST('EDTRA1')
      ENDIF
      IF(NCHH8.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS8(I:I),I=1,NCHH8)
         CALL EDWRST('EDTRA1')
      ENDIF
      IF(NCHH9.GE.1)THEN
         WRITE(ICOUT,93)(IHOLS9(I:I),I=1,NCHH9)
         CALL EDWRST('EDTRA1')
      ENDIF
      IF(NCHH10.GE.1)THEN
         WRITE(ICOUT,93)(IHOL10(I:I),I=1,NCHH10)
         CALL EDWRST('EDTRA1')
      ENDIF
      CALL EDWRST('EDTRA1')
      WRITE(ICOUT,101)ICLIM1,ICLIM2
      CALL EDWRST('EDTRA1')
  101 FORMAT('ICLIM1,ICLIM2 = ',2I8)
      WRITE(ICOUT,102)IPLIM1,IPLIM2
      CALL EDWRST('EDTRA1')
  102 FORMAT('IPLIM1,IPLIM2 = ',2I8)
      WRITE(ICOUT,103)IMLIM1,IMLIM2
      CALL EDWRST('EDTRA1')
  103 FORMAT('IMLIM1,IMLIM2 = ',2I8)
      WRITE(ICOUT,104)ITABC
      CALL EDWRST('EDTRA1')
  104 FORMAT('ITABC = ',I8)
C
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRA1')
      WRITE(ICOUT,111)NCOUT,ILOUT
      CALL EDWRST('EDTRA1')
  111 FORMAT('NCOUT,ILOUT = ',2I8)
      WRITE(ICOUT,112)ICOUT
      CALL EDWRST('EDTRA1')
  112 FORMAT('ICOUT = ',A230)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE EDTRA2
C
C     PURPOSE--PRINT TRACE/DEBUG INFORMATION AT THE END
C              OF VARIOUS SUBROUTINES.
C
C     DATE--JANUARY 19,1985
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-------------------------------------------
C
      CALL EDTRA1
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
C     PURPOSE--PRINT OUT A TRACE LINE FOR DEBUGGING.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1984.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C---------------------------------------------------------------------
C
CCCCC CHARACTER*4 IFEESW
CCCCC CHARACTER*4 IPRINT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
CCCCC COMMON /PRINT/IFEESW,IPRINT
C
C-----START POINT-----------------------------------------------------
C
      WRITE(ICOUT,105)ISTEPN,ISUBN1,ISUBN2
      CALL EDWRST('EDTRA3')
  105 FORMAT('TRACE AT STEP ',A4,' OF ',A4,A4)
C
      RETURN
      END
      SUBROUTINE EDTRNP(IST4,IC1,IHIT)
C
C     PURPOSE--EXAMINE THE INPUT ASCI SCRING IN IST4
C              AND NOTE TO SEE IF IT IS ONE OF THE
C              MNEMONICS FOR THE FIRST 32 ASCII CHARACTERS.
C              IF IT IS    , SET    IHIT   TO   YES
C              AND OUTPUT (IN IC1) THE ACTUAL ASCII CHARACTER.
C              IF IT IS NOT, SET    IHIT   TO   NO
C              AND DO NOTHING ELSE.
C
C     NOTE--ACTUALLY, THE FIRST 33 CHARACTERS HAVE BEEN
C           INCLUDED (THE 33RD BEING    A BLANK = A SPACE).
C     DATE--OCTOBER 20,1985
C
C-------------------------------------------------------------------------------
C
      CHARACTER*4 IST4
      CHARACTER*1 IC1
      CHARACTER*4 IHIT
C
      CHARACTER*4 ITABNP
C
      DIMENSION ITABNP(33)
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
      INCLUDE 'DPCONP.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----DATA STATEMENTS-------------------------------------
C
      DATA ITABNP( 1)/'SOH'/
      DATA ITABNP( 2)/'STX'/
      DATA ITABNP( 3)/'ETX'/
      DATA ITABNP( 4)/'EOT'/
      DATA ITABNP( 5)/'ENQ'/
      DATA ITABNP( 6)/'ACK'/
      DATA ITABNP( 7)/'BEL'/
      DATA ITABNP( 8)/'BS'/
      DATA ITABNP( 9)/'NUL'/
      DATA ITABNP(10)/'LF'/
      DATA ITABNP(11)/'VT'/
      DATA ITABNP(12)/'FF'/
      DATA ITABNP(13)/'CR'/
      DATA ITABNP(14)/'SO'/
      DATA ITABNP(15)/'SI'/
      DATA ITABNP(16)/'DLE'/
      DATA ITABNP(17)/'DC1'/
      DATA ITABNP(18)/'DC2'/
      DATA ITABNP(19)/'DC3'/
      DATA ITABNP(20)/'DC4'/
      DATA ITABNP(21)/'NAK'/
      DATA ITABNP(22)/'SYN'/
      DATA ITABNP(23)/'ETB'/
      DATA ITABNP(24)/'CAN'/
      DATA ITABNP(25)/'EM'/
      DATA ITABNP(26)/'SUB'/
      DATA ITABNP(27)/'ESC'/
      DATA ITABNP(28)/'FS'/
      DATA ITABNP(29)/'GS'/
      DATA ITABNP(30)/'RS'/
      DATA ITABNP(31)/'US'/
      DATA ITABNP(32)/'BLAN'/
      DATA ITABNP(33)/'NUL'/
 
C-----START POINT-------------------------------------------
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'TRNP')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRNP')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDTRNP')
   51 FORMAT('***** AT THE BEGINNING OF EDTRNP--')
      WRITE(ICOUT,52)IST4
      CALL EDWRST('EDTRNP')
   52 FORMAT('IST4 = ',A4)
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
      IHIT='NO'
      IC1='-'
      DO1100I=1,33
      IF(IST4.EQ.ITABNP(I))GOTO1150
      GOTO1100
 1150 CONTINUE
      IHIT='YES'
      IC1=CHAR(I)
      IF(I.EQ.33)IC1=CHAR(0)
      GOTO9000
 1100 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'TRNP')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRNP')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDTRNP')
 9011 FORMAT('***** AT THE END       OF EDTRNP--')
      WRITE(ICOUT,9012)IST4,IHIT
      CALL EDWRST('EDTRNP')
 9012 FORMAT('IST4,IHIT = ',A4,2X,A4)
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDTRUN
C
C     PURPOSE--TRUNCATE CURRENT LINE
C              (AND NEXT IARG1-1 LINES)
C              SO AS TO BE BE TRUNCATED AT   ITRUNC.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      DIMENSION IOLDLI(240)
      DIMENSION INEWLI(240)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDCE'
      ISUBN2='NT  '
C
      IFOUND='YES'
      IERROR='NO'
C
      J1=(-999)
      N1=(-999)
      J2=(-999)
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CENT')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRUN')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDTRUN')
   51 FORMAT('***** AT THE BEGINNING OF EDTRUN--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C
C               ************************************
C               **  STEP 1--                      **
C               **  DETERMINE THE FIRST AND LAST  **
C               **  LINE NUMBERS TO BE TRUNCATED.  **
C               ************************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=ICURLN
      ISTOP=ICURLN
C
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      GOTO1120
C
 1100 CONTINUE
      ISTART=ICURLN
      ISTOP=ICURLN
      GOTO1190
C
 1110 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      ISTART=ICURLN
      ISTOP=ISTART+IARG1-1
      GOTO1190
C
 1120 CONTINUE
      IARG1=NUMLIN+1
      IF(IARGT(1).EQ.'NUMB')IARG1=IARG(1)
      IARG2=NUMLIN+1
      IF(IARGT(2).EQ.'NUMB')IARG2=IARG(2)
      ISTART=IARG1
      ISTOP=IARG2
      GOTO1190
C
 1190 CONTINUE
      IHOLD1=ISTART
      IHOLD2=ISTOP
      IF(IHOLD2.LT.IHOLD1)ISTART=IHOLD2
      IF(IHOLD2.LT.IHOLD1)ISTOP=IHOLD1
C
C               ********************************************
C               **  STEP 21--                             **
C               **  LOOP THROUGH THE LINES TO BE TRUNCATED **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ILINE=ISTART-1
 2100 CONTINUE
      ILINE=ILINE+1
      IF(ILINE.LE.ISTOP)GOTO2910
      ILINE=ISTOP
      GOTO2900
 2910 CONTINUE
      ICURLN=ILINE
      IF(ILINE.LT.1)GOTO2100
      IF(ILINE.GT.NUMLIN)GOTO2900
C
C               *****************************
C               **  STEP 22--              **
C               **  COPY THE OLD    LINE   **
C               *****************************
C
      ISTEPN='22'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      I=0
      DO2200J=J1,J2
      I=I+1
      IOLDLI(I)=ICHA(J)
 2200 CONTINUE
      NUMCOL=I
C
C               ***********************************
C               **  STEP 23--                    **
C               **  APPLY THE DESIRED CENTERING  **
C               **  TO THE TARGET LINE,          **
C               **  SO AS TO CREATE              **
C               **  A NEW LINE.                  **
C               ***********************************
C
      ISTEPN='23'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      CALL EDTRU2(IOLDLI,NUMCOL,INEWLI,NUMCNL)
C
C               ***************************************
C               **  STEP 24--                        **
C               **  DETERMINE IF ROOM EXISTS IN THE  **
C               **  MAIN INTERNAL CHARACTER ARRAY    **
C               **  FOR THE NEW TRUNCATED LINE.       **
C               ***************************************
C
      ISTEPN='24'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH9=NUMCHA+NUMCNL
      CALL EDCHSI(NUMCH9)
      IF(IERROR.EQ.'YES')GOTO9000
C
 2450 CONTINUE
      IF(ILINE.LT.1)GOTO2460
      IF(ILINE.GT.NUMLIN)GOTO2470
      GOTO2490
C
 2460 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2461)
         CALL EDWRST('EDTRUN')
      ENDIF
 2461 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2462)
         CALL EDWRST('EDTRUN')
      ENDIF
 2462 FORMAT(10X,'[TOP]')
 2469 CONTINUE
      GOTO2100
C
 2470 CONTINUE
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2471)
         CALL EDWRST('EDTRUN')
      ENDIF
 2471 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2472)
         CALL EDWRST('EDTRUN')
      ENDIF
 2472 FORMAT(10X,'[BOTTOM]')
 2479 CONTINUE
      GOTO9000
C
 2490 CONTINUE
C
C               ***************************************
C               **  STEP 25--                        **
C               **  UPDATE THE MAIN CHARACTER ARRAY  **
C               **  WITH THE NEW     LINE.           **
C               ***************************************
C
      ISTEPN='25'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'CENT')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      J1=NUMCHA+1
      N1=NUMCNL
      J2=J1+(N1-1)
C
      NUMRP1=NUMROW+1
      IROW=NUMRP1
      IPOINT(ILINE)=IROW
      ILOCC1(IROW)=J1
      NUMCPL(IROW)=N1
C
      I=0
      DO2500J=J1,J2
      I=I+1
      ICHA(J)=INEWLI(I)
 2500 CONTINUE
      NUMROW=NUMRP1
      NUMCHA=J2
C
      IF(IFEESW.EQ.'OFF')GOTO2629
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2621)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDTRUN')
      ENDIF
 2621 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2622)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDTRUN')
      ENDIF
 2622 FORMAT(I6,':',3X,230A1)
 2629 CONTINUE
C
      GOTO2100
C
 2900 CONTINUE
      IF(ILINE.LE.NUMLIN)GOTO2919
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,2911)
         CALL EDWRST('EDTRUN')
      ENDIF
 2911 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,2912)
         CALL EDWRST('EDTRUN')
      ENDIF
 2912 FORMAT(10X,'[BOTTOM]')
 2919 CONTINUE
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'CENT')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRUN')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDTRUN')
 9011 FORMAT('***** AT THE END       OF EDTRUN--')
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRUN')
      WRITE(ICOUT,9012)IWIDTH
      CALL EDWRST('EDTRUN')
 9012 FORMAT('IWIDTH = ',I8)
      WRITE(ICOUT,9013)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDTRUN')
 9013 FORMAT('(IANS(I),I=1,IWIDTH) = ',110A1)
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDTRUN')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDTRUN')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDTRUN')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDTRUN')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,9041)ICURLN,ISTART,ISTOP
      CALL EDWRST('EDTRUN')
 9041 FORMAT('ICURLN,ISTART,ISTOP = ',3I8)
      WRITE(ICOUT,9042)J1,N1,J2
      CALL EDWRST('EDTRUN')
 9042 FORMAT('J1,N1,J2 = ',3I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRUN')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDTRU2(IOLDLI,NUMCOL,INEWLI,NUMCNL)
C
C     PURPOSE--TRUNCATE THE LINE IN IOLDLI(.).
C              THE TRUNCATED LINE WILL BE PLACED IN INEWLI(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IOLDLI
      CHARACTER*1 INEWLI
C
      DIMENSION IOLDLI(*)
      DIMENSION INEWLI(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDTR'
      ISUBN2='U2  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'TRU2')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRU2')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDTRU2')
   51 FORMAT('***** AT THE BEGINNING OF EDTRU2--')
      WRITE(ICOUT,71)NUMCOL
      CALL EDWRST('EDTRU2')
   71 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,72)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDTRU2')
   72 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,73)NUMCNL
      CALL EDWRST('EDTRU2')
   73 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,74)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDTRU2')
   74 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,81)ITRUNC
      CALL EDWRST('EDTRU2')
   81 FORMAT('ITRUNC = ',I8)
   90 CONTINUE
C
C               ************************************************************
C               **  STEP 2--                                              **
C               **  DETERMINE THE LAST  NON-BLANK CHARACTER IN IOLDLI(.)  **
C               ************************************************************
C
      IREV=0
      IF(NUMCOL.LE.0)GOTO1290
      DO1200I=1,NUMCOL
      IREV=NUMCOL-I+1
      IF(IOLDLI(IREV).NE.' ')GOTO1290
 1200 CONTINUE
      IREV=0
 1290 CONTINUE
      ILAST=IREV
C
C               ******************************************************
C               **  STEP 4--                                        **
C               **  DETERMINE THE DESIRED TRUNCATION COLUMN OF THE NEW STRING  *
*
C               ******************************************************
C
      ITRUN2=ITRUNC
C
C               ********************************
C               **  STEP 5--                  **
C               **  BLANK OUT THE NEW STRING  **
C               ********************************
C
      NUMCNL=1
      DO1500I=1,240
      INEWLI(I)=' '
 1500 CONTINUE
C
C               **************************************
C               **  STEP 6--                        **
C               **  FORM THE NEW (TRUNCATED) STRING  **
C               **************************************
C
      INEW=0
      IMAX=ILAST
      IF(ITRUN2.LT.IMAX)IMAX=ITRUN2
      IF(IMAX.LE.0)GOTO1690
      DO1600IOLD=1,IMAX
      INEW=IOLD
      INEWLI(INEW)=IOLDLI(IOLD)
 1600 CONTINUE
 1690 CONTINUE
      NUMCNL=INEW
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'TRU2')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDTRU2')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDTRU2')
 9011 FORMAT('***** AT THE END       OF EDTRU2--')
      WRITE(ICOUT,9031)NUMCOL
      CALL EDWRST('EDTRU2')
 9031 FORMAT('NUMCOL = ',I8)
      WRITE(ICOUT,9032)(IOLDLI(I),I=1,NUMCOL)
      CALL EDWRST('EDTRU2')
 9032 FORMAT('(IOLDLI(I),I=1,NUMCOL) = ',110A1)
      WRITE(ICOUT,9033)NUMCNL
      CALL EDWRST('EDTRU2')
 9033 FORMAT('NUMCNL = ',I8)
      WRITE(ICOUT,9034)(INEWLI(I),I=1,NUMCNL)
      CALL EDWRST('EDTRU2')
 9034 FORMAT('(INEWLI(I),I=1,NUMCNL) = ',110A1)
      WRITE(ICOUT,9041)ILAST,ITRUNC,ITRUN2
      CALL EDWRST('EDTRU2')
 9041 FORMAT('ILAST,ITRUNC,ITRUN2 = ',3I8)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDTYPE(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM)
C
C     PURPOSE--TAKE THE COMPONENTS OF AN INPUT COMMAND LINE
C              AND COMPUTE HOLLERITH, INTEGER, AND FLOATING POINT
C              EQUIVALENTS FOR EACH COMPONENT.
C     INPUT  ARGUMENTS--IANS   (A HOLLERITH VECTOR)
C                     --IWIDTH (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--ICOM   (AN A4 HOLLERITH VALUE FOR COMMAND)
C                     --ICOM2  (AN A4 HOLLERITH VALUE FOR COMMAND)
C                     --IHARG  (AN A4 HOLLERITH VECTOR)
C                     --IHARG2 (AN A4 HOLLERITH VECTOR)
C                     --IARG   (AN INTEGER VECTOR)
C                     --ARG    (A FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C      NOTE--A GIVEN ARGUMENT MAY END UP WITH
C            3 DIFFERENT REPRESENTATIONS--
C            HOLLERITH, INTEGER, AND FLOATING POINT.
C     WRITTEN BY--JAMES J. FILLIBEN
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
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
C
      CHARACTER*4 IFLUNK
      CHARACTER*1 IB
      CHARACTER*4 IANS1
      CHARACTER*4 IANS2
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
C---------------------------------------------------------------------
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
      DIMENSION ISTART(160)
      DIMENSION ISTOP(160)
      DIMENSION IB(160)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDTY'
      ISUBN2='PE  '
C
      IF(IBUGTY.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDTYPE')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDTYPE')
   51 FORMAT('***** AT THE BEGINNING OF EDTYPE--')
      WRITE(ICOUT,52)IWIDTH
      CALL EDWRST('EDTYPE')
   52 FORMAT('IWIDTH = ',I6)
      WRITE(ICOUT,53)(IANS(I),I=1,IWIDTH)
      CALL EDWRST('EDTYPE')
   53 FORMAT('(IANS(.) = ',230A1)
   90 CONTINUE
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
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICOM='    '
      ICOM2='    '
      ICOMT='NUMB'
      ICOMI=(-1)
      ACOM=(-1.0)
      DO110I=1,100
      IHARG(I)='    '
      IHARG2(I)='    '
      IARGT(I)='NUMB'
      IARG(I)=(-1)
      ARG(I)=(-1.0)
  110 CONTINUE
      NUMARG=(-1)
C
C               ***************************************************************
C               **  STEP 2--
C               **  SEPARATE IANS(.) INTO COMPONENTS WHERE
C               **  A COMPONENT IS DEFINED AS THAT SEPARATED BY 1 OR MORE BLANK
C               **  IN ADDITION, AN EQUAL SIGN (=),
C               **  A GREATER-THAN SIGN (>), AND A LESS-THAN SIGN (<)
C               **  ARE ALSO CONSIDERED AS A COMPONENT UNTO ITSELF
C               **  REGARDLESS OF WHETHER OR NOT
C               **  IT HAS PRECEEDING AND SUCCEEDING BLANKS.
C               **  FINALLY, A HYPHEN WHEN IMMEDIATELY PRECEDED
C               **  AND SUCCEEDED BY A NON-BLANK CHARACTER
C               **  WILL ALSO BE CONSIDERED AS A SEPARATOR
C               **  AND SO WILL NOT BE COPIED AS A CHARACTER.
C               **  HOWEVER, IF THERE IS A BLANK BEFORE OR AFTER THE HYPHEN
C               **  (AS IN DEFINING THE    -    AS A PLOT CHARACTER TYPE),
C               **  THEN THE HYPHEN WILL BE TREATED AND COPIED AS A SEPARATE
C               **  COMPONENT.
C               **  TREAT THE CASE WHERE THE ORIGINAL LINE IANS(.) WAS NON-EMPT
C               **  LOCATE THE START AND STOP COLUMNS FOR EACH 'WORD'.
C               ***************************************************************
C
      ISTEPN='2'
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMWD=0
      DO300I=1,IWIDTH
      IM1=I-1
      IM2=I-2
      IP1=I+1
C
      IF(IANS(I).EQ.'=')GOTO350
      IF(IANS(I).EQ.'>')GOTO350
      IF(IANS(I).EQ.'<')GOTO350
      IF(IANS(I).NE.' '.AND.I.LE.1)GOTO350
C
      IF(I.LE.1)GOTO360
      IF(IANS(I).NE.' '.AND.IANS(IM1).EQ.' ')GOTO350
      IF(IANS(I).NE.' '.AND.IANS(IM1).EQ.'=')GOTO350
      IF(IANS(I).NE.' '.AND.IANS(IM1).EQ.'>')GOTO350
      IF(IANS(I).NE.' '.AND.IANS(IM1).EQ.'<')GOTO350
C
      IF(I.LE.2)GOTO360
      IF(IANS(I).NE.' '.AND.IANS(IM1).EQ.'-')GOTO340
      GOTO360
C
  340 CONTINUE
      IF(IANS(IM2).EQ.'=')GOTO360
      IF(IANS(IM2).EQ.'-')GOTO355
      IF(IANS(IM2).NE.' ')GOTO350
      GOTO360
C
  350 CONTINUE
      NUMWD=NUMWD+1
C
  355 CONTINUE
      ISTART(NUMWD)=I
C
  360 CONTINUE
      IF(IANS(I).EQ.'=')GOTO370
      IF(IANS(I).EQ.'>')GOTO370
      IF(IANS(I).EQ.'<')GOTO370
      IF(IANS(I).NE.' '.AND.I.GE.IWIDTH)GOTO370
C
      IF(I.GE.IWIDTH)GOTO390
      IF(IANS(I).NE.' '.AND.IANS(IP1).EQ.' ')GOTO370
      IF(IANS(I).NE.' '.AND.IANS(IP1).EQ.'=')GOTO370
      IF(IANS(I).NE.' '.AND.IANS(IP1).EQ.'>')GOTO370
      IF(IANS(I).NE.' '.AND.IANS(IP1).EQ.'<')GOTO370
      IF(IANS(I).NE.' '.AND.IANS(IP1).EQ.'-')GOTO370
C
      GOTO390
C
  370 CONTINUE
      ISTOP(NUMWD)=I
C
  390 CONTINUE
      IF(IBUGTY.EQ.'ON')THEN
         WRITE(ICOUT,391)NUMWD
         CALL EDWRST('EDTYPE')
      ENDIF
  391 FORMAT('NUMWD = ',I8)
      IF(IBUGTY.EQ.'ON'.AND.NUMWD.GE.1)THEN
         WRITE(ICOUT,392)I,NUMWD,ISTART(NUMWD),ISTOP(NUMWD)
         CALL EDWRST('EDTYPE')
      ENDIF
  392 FORMAT('I,NUMWD,ISTART(NUMWD),ISTOP(NUMWD) = ',4I8)
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
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IWORD=1
      IWID=ISTOP(IWORD)-ISTART(IWORD)+1
      JMIN=ISTART(IWORD)
      JMAX=ISTOP(IWORD)
      I=0
      DO800J=JMIN,JMAX
      I=I+1
      IB(I)=IANS(J)
  800 CONTINUE
C
      IANS1='    '
      IANS2='    '
      IMAX=2*NUMASC
      IF(IWID.LT.IMAX)IMAX=IWID
      IF(IBUGTY.EQ.'ON')THEN
         WRITE(ICOUT,901)IMAX
         CALL EDWRST('EDTYPE')
      ENDIF
  901 FORMAT('IMAX = ',I6)
      DO900I=1,IMAX
      IF(IB(I).EQ.' ')GOTO910
      IM4=I-4
      IF(I.LE.NUMASC)IANS1(I:I)=IB(I)
      IF(I.GT.NUMASC)IANS2(IM4:IM4)=IB(I)
  900 CONTINUE
  910 CONTINUE
      ICOM=IANS1
      ICOM2=IANS2
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
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
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
      IB(I)=IANS(J)
 1100 CONTINUE
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
 1200 CONTINUE
 1210 CONTINUE
      IWORM1=IWORD-1
      IHARG(IWORM1)=IANS1
      IHARG2(IWORM1)=IANS2
C
 1000 CONTINUE
 1999 CONTINUE
C
C               **********************************************************
C               **  STEP 5--                                            **
C               **  CONVERT STRINGS 1 THROUGH END TO INTEGER ARGUMENTS  **
C               **********************************************************
C
      ISTEPN='5'
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMWD.LE.0)GOTO2999
      DO2000IWORD=1,NUMWD
      IWORM1=IWORD-1
C
      IF(IWORD.LE.1)GOTO2005
      GOTO2006
C
 2005 CONTINUE
      IH=ICOM
      IH2=ICOM2
      GOTO2009
C
 2006 CONTINUE
      IH=IHARG(IWORM1)
      IH2=IHARG2(IWORM1)
      GOTO2009
C
 2009 CONTINUE
      IF(NUMNAM.LE.0)GOTO2040
      DO2010INAME=1,NUMNAM
      IF(IH.EQ.IHNAME(INAME).AND.IH2.EQ.IHNAM2(INAME))GOTO2020
      GOTO2010
 2020 CONTINUE
      IF(IUSE(INAME).EQ.'P')GOTO2030
      GOTO2040
 2030 CONTINUE
      IARGT(IWORM1)='NUMB'
      IARG(IWORM1)=IVALUE(INAME)
      GOTO2000
 2010 CONTINUE
 2040 CONTINUE
C
      IFLUNK='NO'
      IANS3=(-1)
      IWID=ISTOP(IWORD)-ISTART(IWORD)+1
      JMIN=ISTART(IWORD)
      JMAX=ISTOP(IWORD)
      I=0
      DO2100J=JMIN,JMAX
      I=I+1
      IB(I)=IANS(J)
 2100 CONTINUE
C
      IDIG=0
      ISIGN=0
      IDECPT=0
      ISUM=0
      DO2700I=1,IWID
      IREV=IWID-I+1
      IF(IB(IREV).EQ.' ')GOTO2700
      IF(IB(IREV).EQ.'0')GOTO2710
      IF(IB(IREV).EQ.'1')GOTO2711
      IF(IB(IREV).EQ.'2')GOTO2712
      IF(IB(IREV).EQ.'3')GOTO2713
      IF(IB(IREV).EQ.'4')GOTO2714
      IF(IB(IREV).EQ.'5')GOTO2715
      IF(IB(IREV).EQ.'6')GOTO2716
      IF(IB(IREV).EQ.'7')GOTO2717
      IF(IB(IREV).EQ.'8')GOTO2718
      IF(IB(IREV).EQ.'9')GOTO2719
      IF(IB(IREV).EQ.'+')GOTO2720
      IF(IB(IREV).EQ.'-')GOTO2721
      IF(IB(IREV).EQ.'.')GOTO2722
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
      ISUM=ISUM+ITERM*10          **(IDIG-1)
 2700 CONTINUE
      IF(IDIG.LE.0)GOTO2800
      IF(ISIGN.GE.2)GOTO2800
      IANS3=ISUM
 2800 CONTINUE
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
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
C               ************************************************************
C               **  STEP 6.1--                                            **
C               **  FIRST OF ALL, LOCATE THE DECIMAL POINT (IF EXISTEPNT)  **
C               ************************************************************
C
      ISTEPN='6.1'
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      AMIN=-1000000.
      AMAX=+1000000.
      NUMARG=NUMWD-1
CCCCC IF(NUMARG.LE.0)GOTO3999
      IF(NUMWD.LE.0)GOTO3999
      DO3000IWORD=1,NUMWD
C
      IWORM1=IWORD-1
C
      IF(IWORD.LE.1)GOTO3005
      GOTO3006
C
 3005 CONTINUE
      IH=ICOM
      IH2=ICOM2
      GOTO3009
C
 3006 CONTINUE
      IH=IHARG(IWORM1)
      IH2=IHARG2(IWORM1)
      GOTO3009
C
 3009 CONTINUE
C
      IF(NUMNAM.LE.0)GOTO3040
      DO3010INAME=1,NUMNAM
      IF(IH.EQ.IHNAME(INAME).AND.IH2.EQ.IHNAM2(INAME))GOTO3020
      GOTO3010
 3020 CONTINUE
      IF(IUSE(INAME).EQ.'P')GOTO3030
      GOTO3040
 3030 CONTINUE
      IF(IWORD.LE.1)ICOMT='NUMB'
      IF(IWORD.GE.2)IARGT(IWORM1)='NUMB'
      IF(IWORD.LE.1)ACOM=VALUE(INAME)
      IF(IWORD.GE.2)ARG(IWORM1)=VALUE(INAME)
      GOTO3000
 3010 CONTINUE
 3040 CONTINUE
C
      IFLUNK='NO'
      ANS2=(-1.0)
      IWID=ISTOP(IWORD)-ISTART(IWORD)+1
      JMIN=ISTART(IWORD)
      JMAX=ISTOP(IWORD)
      I=0
      DO3050J=JMIN,JMAX
      I=I+1
      IB(I)=IANS(J)
 3050 CONTINUE
C
      ILOC=0
      IDECPT=0
      DO3060I=1,IWID
      IF(IB(I).EQ.'.')ILOC=I
      IF(IB(I).EQ.'.')IDECPT=IDECPT+1
 3060 CONTINUE
      IF(IDECPT.GE.2)GOTO3900
      IF(IDECPT.EQ.1)GOTO3150
      DO3100I=1,IWID
      IREV=IWID-I+1
      IF(IB(IREV).EQ.' ')GOTO3100
      IF(IB(IREV).EQ.'0')GOTO3110
      IF(IB(IREV).EQ.'1')GOTO3110
      IF(IB(IREV).EQ.'2')GOTO3110
      IF(IB(IREV).EQ.'3')GOTO3110
      IF(IB(IREV).EQ.'4')GOTO3110
      IF(IB(IREV).EQ.'5')GOTO3110
      IF(IB(IREV).EQ.'6')GOTO3110
      IF(IB(IREV).EQ.'7')GOTO3110
      IF(IB(IREV).EQ.'8')GOTO3110
      IF(IB(IREV).EQ.'9')GOTO3110
      IFLUNK='YES'
      IF(IB(IREV).EQ.'+')GOTO3900
      IF(IB(IREV).EQ.'-')GOTO3900
      GOTO3900
 3100 CONTINUE
      IFLUNK='YES'
      GOTO3900
 3110 ILOC=IREV+1
 3150 CONTINUE
      IF(IBUGTY.NE.'OFF')THEN
         WRITE(ICOUT,3111)ILOC,IDECPT
         CALL EDWRST('EDTYPE')
      ENDIF
 3111 FORMAT('ILOC = ',I8,'    IDECPT = ',I8)
C
C               *******************************************************
C               **  STEP 6.2--                                       **
C               **  SECONDLY, COMPUTE THE INTEGER PART OF THE VALUE  **
C               *******************************************************
C
      ISTEPN='6.2'
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      SIGN=1.0
      IDIGI=0
      ISIGN=0
      SUMI=0
      ILOCM1=ILOC-1
      IF(ILOCM1.LT.1)GOTO3250
      DO3200I=1,ILOCM1
      IREV=ILOCM1-I+1
      IF(IB(IREV).EQ.' ')GOTO3200
      IF(IB(IREV).EQ.'0')GOTO3210
      IF(IB(IREV).EQ.'1')GOTO3211
      IF(IB(IREV).EQ.'2')GOTO3232
      IF(IB(IREV).EQ.'3')GOTO3213
      IF(IB(IREV).EQ.'4')GOTO3214
      IF(IB(IREV).EQ.'5')GOTO3215
      IF(IB(IREV).EQ.'6')GOTO3216
      IF(IB(IREV).EQ.'7')GOTO3217
      IF(IB(IREV).EQ.'8')GOTO3218
      IF(IB(IREV).EQ.'9')GOTO3219
      IF(IB(IREV).EQ.'+')GOTO3220
      IF(IB(IREV).EQ.'-')GOTO3221
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
      SUMI=SUMI+TERM*(10.0          **IEXP)
 3200 CONTINUE
 3250 CONTINUE
      IF(ISIGN.GE.2)GOTO3900
      IF(IBUGTY.NE.'OFF')THEN
         WRITE(ICOUT,3255)IDIGI,SUMI
         CALL EDWRST('EDTYPE')
      ENDIF
 3255 FORMAT('IDIGI = ',I8,'    SUMI = ',F20.10)
C
C               ******************************************************
C               **  STEP 6.3--                                      **
C               **  THIRDLY, COMPUTE THE DECIMAL PART OF THE VALUE  **
C               ******************************************************
C
      ISTEPN='6.3'
      IF(IBUGTY.EQ.'ON')CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IDIGD=0
      SUMD=0.0
      ILOCP1=ILOC+1
      IF(ILOCP1.GT.IWID)GOTO3350
      DO3300I=ILOCP1,IWID
      IF(IB(I).EQ.' ')GOTO3300
      IF(IB(I).EQ.'0')GOTO3310
      IF(IB(I).EQ.'1')GOTO3311
      IF(IB(I).EQ.'2')GOTO3312
      IF(IB(I).EQ.'3')GOTO3333
      IF(IB(I).EQ.'4')GOTO3314
      IF(IB(I).EQ.'5')GOTO3315
      IF(IB(I).EQ.'6')GOTO3316
      IF(IB(I).EQ.'7')GOTO3317
      IF(IB(I).EQ.'8')GOTO3318
      IF(IB(I).EQ.'9')GOTO3319
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
      SUMD=SUMD+TERM/(10.0          **IDIGD)
 3300 CONTINUE
 3350 CONTINUE
      IF(IBUGTY.NE.'OFF')THEN
         WRITE(ICOUT,3355)IDIGD,SUMD
         CALL EDWRST('EDTYPE')
      ENDIF
 3355 FORMAT('IDIGD = ',I8,'    SUMD = ',F20.10)
      IDIGT=IDIGI+IDIGD
      IF(IDIGT.LE.0)GOTO3900
      ANS2=SUMI+SUMD
      IF(SIGN.LT.0.0)ANS2=-ANS2
      IWORM1=IWORD-1
      IF(IWORD.LE.1)ACOM=ANS2
      IF(IWORD.GE.2)ARG(IWORM1)=ANS2
      IF(AMIN.LE.ANS2.AND.ANS2.LE.AMAX)GOTO3000
      GOTO3900
C
 3900 CONTINUE
      IWORM1=IWORD-1
CCCCC THE FOLLOWING 2 LINES WERE FIXED APRIL 1990
CCCCC BASED ON IBM-PC OTG COMPILER DIAGNOSTICS
CCCCC ARG(IWORM1)=ANS2
CCCCC IF(IFLUNK.EQ.'YES')IARGT(IWORM1)='WORD'
      IF(IWORD.GE.2)ARG(IWORM1)=ANS2
      IF(IWORD.GE.2.AND.IFLUNK.EQ.'YES')IARGT(IWORM1)='WORD'
 3000 CONTINUE
 3999 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGTY.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDTYPE')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDTYPE')
 9011 FORMAT('***** AT THE END       OF EDTYPE--')
      WRITE(ICOUT,9012)ICOM,ICOM2,ICOMT,ACOM,ICOMI
      CALL EDWRST('EDTYPE')
 9012 FORMAT('ICOM,ICOM2,ICOMT,ACOM,ICOMI = ',
     1A4,2X,A4,2X,A4,E15.7,I8)
      WRITE(ICOUT,9013)NUMARG
      CALL EDWRST('EDTYPE')
 9013 FORMAT('NUMARG = ',I6)
      DO9015I=1,NUMARG
      WRITE(ICOUT,9016)I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I)
      CALL EDWRST('EDTYPE')
 9016 FORMAT('I,IHARG(I),IHARG2(I),IARG(I),ARG(I),IARGT(I) = ',
     1I6,1X,A4,1X,A4,1X,I6,1X,E15.7,1X,A4)
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDUNDO
C
C     PURPOSE-UNDO THE LAST COMMAND.
C             RESTORE THE INTERNAL POINTERS TO THEIR SETTINGS
C             PRIOR TO THE LAST COMMAND.
C     NOTE--NO ARGUMENTS ARE EXPECTED.
C     SYNTAX--UNDO
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDUN'
      ISUBN2='DO  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'UNDO')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDUNDO')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDUNDO')
   51 FORMAT('*****AT THE BEGINNING OF EDUNDO--')
      WRITE(ICOUT,52)IBUGE2,ISUBRO
      CALL EDWRST('EDUNDO')
   52 FORMAT('IBUGE2,ISUBRO = ',A4,2X,A4)
      WRITE(ICOUT,53)NUMLOL,NUMROL,NCHAOL,IOLDLN
      CALL EDWRST('EDUNDO')
   53 FORMAT('NUMLOL,NUMROL,NCHAOL,IOLDLN = ',4I8)
      WRITE(ICOUT,54)NUMLIN,NUMROW,NUMCHA,ICURLN
      CALL EDWRST('EDUNDO')
   54 FORMAT('NUMLIN,NUMROW,NUMCHA,ICURLN = ',4I8)
      IF(NUMLIN.LE.0)GOTO59
      DO55I=1,NUMLIN
      WRITE(ICOUT,56)I,IPOIOL(I),IPOINT(I)
      CALL EDWRST('EDUNDO')
   56 FORMAT('I,IPOIOL(I),IPOINT(I) = ',3I8)
   55 CONTINUE
   59 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDUNDO')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *****************************
C               **  STEP 1--               **
C               **  COPY THE OLD POINTERS  **
C               *****************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'UNDO')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      NUMLIN=NUMLOL
      ICURLN=IOLDLN
      NUMROW=NUMROL
      NUMCHA=NCHAOL
C
      IF(NUMLIN.LE.0)GOTO1190
      DO1100I=1,NUMLIN
      IPOINT(I)=IPOIOL(I)
 1100 CONTINUE
 1190 CONTINUE
C
C               *************************
C               **  STEP 2--           **
C               **  GO TO TOP OF FILE  **
C               *************************
C
      ISTEPN='2'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'UNDO')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICURLN=0
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'UNDO')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDUNDO')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDUNDO')
 9011 FORMAT('*****AT THE END       OF EDUNDO--')
      WRITE(ICOUT,9012)NUMLOL,NUMROL,NCHAOL,IOLDLN
      CALL EDWRST('EDUNDO')
 9012 FORMAT('NUMLOL,NUMROL,NCHAOL,IOLDLN = ',4I8)
      WRITE(ICOUT,9013)NUMLIN,NUMROW,NUMCHA,ICURLN
      CALL EDWRST('EDUNDO')
 9013 FORMAT('NUMLIN,NUMROW,NUMCHA,ICURLN = ',4I8)
      IF(NUMLIN.LE.0)GOTO9019
      DO9015I=1,NUMLIN
      WRITE(ICOUT,9016)I,IPOIOL(I),IPOINT(I)
      CALL EDWRST('EDUNDO')
 9016 FORMAT('I,IPOIOL(I),IPOINT(I) = ',3I8)
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,999)
      CALL EDWRST('EDUNDO')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDUP
C
C     PURPOSE--GO UP K LINES
C
C     NOTE--IF NO ARGUMENTS, THEN GO UP 1 LINE
C           IF 1 ARGUMENT, THEN GO GO UP K LINES.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85.1
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDUP'
      ISUBN2='    '
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DO')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDUP  ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDUP  ')
   51 FORMAT('***** AT THE BEGINNING OF EDUP--')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *************************
C               **  STEP 1--           **
C               **  GO UP K LINES      **
C               **  AND PRINT IT OUT.  **
C               *************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'DO')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      K=1
      IF(NUMARG.GE.1.AND.IARGT(1).EQ.'NUMB')K=IARG(1)
C
      ILINE=ICURLN-K
C
      IF(ILINE.LT.1)GOTO1110
      IF(ILINE.GT.NUMLIN)GOTO1120
      GOTO1130
C
 1110 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1119
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1111)
         CALL EDWRST('EDUP  ')
      ENDIF
 1111 FORMAT('[TOP]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1112)
         CALL EDWRST('EDUP  ')
      ENDIF
 1112 FORMAT(10X,'[TOP]')
 1119 CONTINUE
      ILINE=0
      GOTO1190
C
 1120 CONTINUE
      IF(IFEESW.EQ.'OFF')GOTO1129
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1121)
         CALL EDWRST('EDUP  ')
      ENDIF
 1121 FORMAT('[BOTTOM]')
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1122)
         CALL EDWRST('EDUP  ')
      ENDIF
 1122 FORMAT(10X,'[BOTTOM]')
 1129 CONTINUE
      ILINE=NUMLIN+1
      GOTO1190
C
 1130 CONTINUE
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
C
      IF(IFEESW.EQ.'OFF')GOTO1139
      IF(IPRISW.EQ.'OFF'.AND.INUMSW.EQ.'OFF')THEN
         WRITE(ICOUT,1131)(ICHA(J),J=J1,J2)
         CALL EDWRST('EDUP  ')
      ENDIF
 1131 FORMAT(238A1)
      IF(IPRISW.EQ.'ON'.OR.INUMSW.EQ.'ON')THEN
         WRITE(ICOUT,1132)ILINE,(ICHA(J),J=J1,J2)
         CALL EDWRST('EDUP  ')
      ENDIF
 1132 FORMAT(I6,':',3X,230A1)
 1139 CONTINUE
      GOTO1190
C
 1190 CONTINUE
      ICURLN=ILINE
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'DO')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDUP  ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDUP  ')
 9011 FORMAT('***** AT THE END       OF EDUP--')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDUPP1(IA1,IB1)
C
C     PURPOSE--CONVERT 1-CHARACTER LOWER CASE ALPHABETIC (A TO Z)
C              ASCII WORD IA1
C              TO UPPER CASE ASCII WORD IB1.
C     NOTE--IA1 AND IB1 ARE ASSUMED TO BE CHARACTER*1
C     NOTE--IA1 AND IB1 MAY BE THE SAME VARIABLE IN THE CALLING ROUTINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY 1981.
C     UPDATED         --MAY       1982.
C
C---------------------------------------------------------------------
C
      CHARACTER*1 IA1
      CHARACTER*1 IB1
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)----------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGMA.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDUPP1')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDUPP1')
   51 FORMAT('***** AT THE BEGINNING OF EDUPP1--')
      WRITE(ICOUT,52)IA1
      CALL EDWRST('EDUPP1')
   52 FORMAT('IA1 = ',A1)
   90 CONTINUE
C
C               ********************************************************
C               **  STEP 11--
C               **  THE FOLLOWING CODE WILL CARRY OUT THE LOWER CASE
C               **  TO UPPER CASE CONVERSION FOR ALL COMPUTERS
C               **  WITH AN ANSI 77 FORTRAN COMPILER--IT MAKES
C               **  USE OF THE ANSI FORTRAN 77 INTRINSIC FUNCTIONS
C               **     1) ICHAR (FOR ASCII CHARACTER TO ASCII NUMERIC CO
C               **     2) CHAR  (FOR ASCII NUMERIC TO ASCII CHARACTER CO
C               ********************************************************
C
      IB1=IA1
C
      IVALUE=ICHAR(IA1)
      IF(97.LE.IVALUE.AND.IVALUE.LE.122)GOTO1110
      GOTO1190
C
 1110 CONTINUE
      IVAL32=IVALUE-32
      IB1=CHAR(IVAL32)
      GOTO1190
C
 1190 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGMA.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDUPP1')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDUPP1')
 9011 FORMAT('***** AT THE END       OF EDUPP1--')
      WRITE(ICOUT,9012)IA1,IVALUE,IVAL32,IB1
      CALL EDWRST('EDUPP1')
 9012 FORMAT('IA1,IVALUE,IVAL32,IB1 = ',A4,I8,I8,2X,A4)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDUPP4(IA4,IB4)
C
C     PURPOSE--CONVERT 4-CHARACTER LOWER CASE ALPHABETIC (A TO Z)
C              ASCII WORD IA4
C              TO UPPER CASE ASCII WORD IB4.
C     NOTE--IA4 AND IB4 ARE ASSUMED TO BE CHARACTER*4
C     NOTE--IA4 AND IB4 MAY BE THE SAME VARIABLE IN THE CALLING ROUTINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY 1981.
C     UPDATED         --MAY       1982.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IA4
      CHARACTER*4 IB4
      CHARACTER*1 IA1
      CHARACTER*1 IB1
C
CCCCC CHARACTER*4 ISUBN1
CCCCC CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)----------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGMA.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDUPP4')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDUPP4')
   51 FORMAT('***** AT THE BEGINNING OF EDUPP4--')
      WRITE(ICOUT,52)IA4
      CALL EDWRST('EDUPP4')
   52 FORMAT('IA4 = ',A4)
   90 CONTINUE
C
C               ********************************************************
C               **  STEP 11--
C               **  THE FOLLOWING CODE WILL CARRY OUT THE LOWER CASE
C               **  TO UPPER CASE CONVERSION FOR ALL COMPUTERS
C               **  WITH AN ANSI 77 FORTRAN COMPILER--IT MAKES
C               **  USE OF THE ANSI FORTRAN 77 INTRINSIC FUNCTIONS
C               **     1) ICHAR (FOR ASCII CHARACTER TO ASCII NUMERIC CO
C               **     2) CHAR  (FOR ASCII NUMERIC TO ASCII CHARACTER CO
C               ********************************************************
C
      IB4=IA4
C
      IA1=' '
      IB1=' '
C
      DO1100I=1,4
C
      IA1=IA4(I:I)
      IVALUE=ICHAR(IA1)
      IF(97.LE.IVALUE.AND.IVALUE.LE.122)GOTO1110
      GOTO1100
C
 1110 CONTINUE
      IVAM32=IVALUE-32
      IB1=CHAR(IVAM32)
      IB4(I:I)=IB1
      GOTO1100
C
 1100 CONTINUE
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGMA.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDUPP4')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDUPP4')
 9011 FORMAT('***** AT THE END       OF EDUPP4--')
      WRITE(ICOUT,9012)IA4,IA1,IB1,IB4
      CALL EDWRST('EDUPP4')
 9012 FORMAT('IA4,IA1,IB1,IB4 = ',A4,2X,A1,2X,A1,2X,A4)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDWRGS(ICSTR,NCSTR,ISUBN0)
C
C     PURPOSE--WRITE OUT THE NCSTR ELEMENTS OF THE
C              CHARACTER*238 STRING ICSTR(.:.)
C              OUT TO A GENERAL GRAPHICS DEVICE.
C              THE VALUE OF THE VARIABLE    NCSTR
C              IS THE NUMBER OF ELEMENTS IN ICSTR(.:.)
C              TO BE WRITTEN OUT.
C     NOTE--ISUBN0 = NAME OF SUBROUTINE WHICH CALLED EDWRGS.
C                    (AND THEREBY HAVE WALKBACK INFORMATION).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*238 ICSTR
C
      CHARACTER*4 IMANUF
C
      CHARACTER*4 ISUBN0
C
      CHARACTER*4 IBRANC
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IGUNIT=IPR
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'WRGS')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDWRGS')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDWRGS')
   51 FORMAT('***** AT THE BEGINNING OF EDWRGS--')
      WRITE(ICOUT,52)ISUBN0
      CALL EDWRST('EDWRGS')
   52 FORMAT('ISUBN0 (NAME OF THE CALLING SUBROUTINE) = ',A4)
      WRITE(ICOUT,53)IGUNIT,IMANUF
      CALL EDWRST('EDWRGS')
   53 FORMAT('IGUNIT,IMANUF = ',I8,2X,A4)
      WRITE(ICOUT,54)NCSTR
      CALL EDWRST('EDWRGS')
   54 FORMAT('NCSTR = ',I8)
      IF(NCSTR.LE.0)GOTO57
      DO55I=1,NCSTR
      IASCNE=ICHAR(ICSTR(I:I))
      WRITE(ICOUT,56)I,ICSTR(I:I),IASCNE
      CALL EDWRST('EDWRGS')
   56 FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
   55 CONTINUE
   57 CONTINUE
CCCCC WRITE(ICOUT,61)NUMTRA
CCC61 FORMAT('NUMTRA = ',I8)
CCCCC IF(NUMTRA.LE.0)GOTO69
CCCCC DO62I=1,NUMTRA
CCCCC WRITE(ICOUT,63)I,NCTRA1(I),ICTRA1(I),NCTRA2(I),ICTRA2(I)
CCC63 FORMAT('I,NCTRA1(I),ICTRA1(I),NCTRA2(I),ICTRA2(I) = ',
CCCCC1I8,I8,2X,A30,I8,2X,A30)
CCC62 CONTINUE
CCC69 CONTINUE
      WRITE(ICOUT,79)IBUGE2,ISUBRO,IERROR
      CALL EDWRST('EDWRGS')
   79 FORMAT('IBUGE2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
   90 CONTINUE
C
C               *********************************************
C               **  STEP 11--                              **
C               **  IF CALLED FOR,                         **
C               **  CARRY OUT ANY SUB-STRING TRANSLATIONS  **
C               *********************************************
C
CCCCC IF(NUMTRA.GE.1)
CCCCC1CALL GRTRST(ICSTR,NCSTR,
CCCCC1ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,
CCCCC1IBUGE2,ISUBRO,IERROR)
C
C               ****************************
C               **  STEP 21--             **
C               **  WRITE OUT THE STRING  **
C               ****************************
C
      IBRANC='NOWR'
      IF(1.LE.NCSTR.AND.NCSTR.LE.238)GOTO2100
      GOTO2190
C
 2100 CONTINUE
      IBRANC='WRIT'
      IF(IHOST1.EQ.'VAX'.AND.ICSTR(1:1).EQ.'$')GOTO2110
      GOTO2120
C
 2110 CONTINUE
      WRITE(IGUNIT,2111)(ICSTR(I:I),I=1,NCSTR)
 2111 FORMAT(238A1)
      GOTO2190
C
 2120 CONTINUE
      WRITE(IGUNIT,2121)(ICSTR(I:I),I=1,NCSTR)
 2121 FORMAT(238A1)
      GOTO2190
C
 2190 CONTINUE
C
C               *******************************************
C               **  STEP 31--                            **
C               **  IF CALLED FOR,                       **
C               **  CALL THE LINE TRANSLATOR SUBROUTINE  **
C               **  WHICH CONVERTS A TEKTRONIX LINE      **
C               **  INTO A SET OF CALLS FOR              **
C               **  ANOTHER GRAPHICS DEVICE              **
C               **  (SEE SUBROUTINE GRTRTK).             **
C               *******************************************
C
CCCCC IF(ITRANS.EQ.'ON')CALL GRTRTK(ICSTR,NCSTR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'WRGS')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDWRGS')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDWRGS')
 9011 FORMAT('***** AT THE END       OF EDWRGS--')
      WRITE(ICOUT,9012)IBRANC
      CALL EDWRST('EDWRGS')
 9012 FORMAT('IBRANC = ',A4)
      WRITE(ICOUT,9013)IGUNIT,IMANUF
      CALL EDWRST('EDWRGS')
 9013 FORMAT('IGUNIT,IMANUF = ',I8,2X,A4)
      WRITE(ICOUT,9014)NCSTR
      CALL EDWRST('EDWRGS')
 9014 FORMAT('NCSTR = ',I8)
      IF(NCSTR.LE.0)GOTO9017
      IF(NCSTR.LE.0)GOTO9017
      DO9015I=1,NCSTR
      IASCNE=ICHAR(ICSTR(I:I))
      WRITE(ICOUT,9016)I,ICSTR(I:I),IASCNE
      CALL EDWRST('EDWRGS')
 9016 FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
 9015 CONTINUE
 9017 CONTINUE
      WRITE(ICOUT,9019)IBUGE2,ISUBRO,IERROR
      CALL EDWRST('EDWRGS')
 9019 FORMAT('IBUGE2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
CCCCC WRITE(ICOUT,9021)NUMTRA
C9021 FORMAT('NUMTRA = ',I8)
CCCCC IF(NUMTRA.LE.0)GOTO9029
CCCCC DO9022I=1,NUMTRA
CCCCC WRITE(ICOUT,9023)I,ICTRA1(I),NCTRA1(I),NCTRA2(I),ICTRA2(I)
C9023 FORMAT('I,ICTRA1(I),NCTRA1(I),NCTRA2(I),ICTRA2(I) = ',
CCCCC1I8,2X,A30,I8,2X,A30,I8)
C9022 CONTINUE
C9029 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDWRLI(ICNEWL,NCNEWL,ILINE)
C
C     PURPOSE--WRITE OUT THE LINE CURRENTLY IN
C              THE 240-CHARACTER VARIABLE ICNEWL
C              (WHICH CONSISTS OF NCNEWL CHARACTERS,
C              AND HAS LINE NUMBER    ILINE   ).
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.1
C     ORIGINAL VERSION--MAY     1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ICNEWL
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDWR'
      ISUBN2='LI  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INSE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDWRLI')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDWRLI')
   51 FORMAT('***** AT THE BEGINNING OF EDNEAT--')
      WRITE(ICOUT,52)NCNEWL,ILINE
      CALL EDWRST('EDWRLI')
   52 FORMAT('NCNEWL,ILINE = ',2I8)
      WRITE(ICOUT,53)(ICNEWL(I:I),I=1,100)
      CALL EDWRST('EDWRLI')
   53 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
   90 CONTINUE
C
C               ****************************************
C               **  STEP 11--                         **
C               **  WRITE OUT THE CHARACTER VARIABLE  **
C               ****************************************
C
      DO1100I=1,240
      ICOUT(I:I)=ICNEWL(I:I)
 1100 CONTINUE
      NCOUT=NCNEWL
      ILOUT=ILINE
      CALL EDWRST('EDWRLI')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INSE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDWRLI')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDWRLI')
 9011 FORMAT('***** AT THE END       OF EDNEAT--')
      WRITE(ICOUT,9012)NCOUT,ILOUT
      CALL EDWRST('EDWRLI')
 9012 FORMAT('NCOUT,ILOUT = ',2I8)
      WRITE(ICOUT,9013)(ICOUT(I:I),I=1,100)
      CALL EDWRST('EDWRLI')
 9013 FORMAT('(ICOUT(I:I),I=1,100) = ',100A1)
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDWRWF(ID)
C
C     PURPOSE--DUMP (= WRITE) WORKSPACE TO A FILE
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
C---------------------------------------------------------------------
C
      CHARACTER*80 IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IREWR
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
C
      CHARACTER*4 ID
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDWR'
      ISUBN2='WF  '
C
      ISUBN0='WRWF'
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'WRWF')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDWRWF')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDWRWF')
   51 FORMAT('*****AT THE BEGINNING OF EDWRWF--')
      WRITE(ICOUT,52)ID
      CALL EDWRST('EDWRWF')
   52 FORMAT('ID = ',A4)
      WRITE(ICOUT,999)
      CALL EDWRST('EDWRWF')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               **************************
C               **  STEP 1--            **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='1'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'WRWF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ID.EQ.'ORIG')GOTO1110
      IF(ID.EQ.'SAVE')GOTO1120
      GOTO1120
C
 1110 CONTINUE
      IOUNIT=IORINU
      IFILE=IORINA
      ISTAT=IORIST
      IFORM=IORIFO
      IACCES=IORIAC
      IREWR=IORIRW
      ISUBN0='WRWF'
      IERRFI='NO'
      GOTO1190
C
 1120 CONTINUE
      IOUNIT=ISAVNU
      IFILE=ISAVNA
      ISTAT=ISAVST
      IFORM=ISAVFO
      IACCES=ISAVAC
      IREWR=ISAVRW
      ISUBN0='WRWF'
      IERRFI='NO'
      GOTO1190
C
 1190 CONTINUE
      IF(IBUGFI.EQ.'OFF')GOTO1199
      WRITE(ICOUT,1193)IFILE
      CALL EDWRST('EDWRWF')
 1193 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,1194)IOUNIT
      CALL EDWRST('EDWRWF')
 1194 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,1195)ISTAT
      CALL EDWRST('EDWRWF')
 1195 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,1196)NUMLIN,NUMCHA
      CALL EDWRST('EDWRWF')
 1196 FORMAT('NUMLIN,NUMCHA = ',2I8)
 1199 CONTINUE
C
C               *********************
C               **  STEP 2--       **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='2'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'WRWF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM='FORMATTED')
CCCCC OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='NEW',FORM='FORMATTED')
      CALL EDOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IREWR,ISUBN0,IERRFI)
C
C               ****************************************************
C               **  STEP 3--                                      **
C               **  WRITE WORKSPACE OUT TO THE              FILE  **
C               ****************************************************
C
      ISTEPN='3'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'WRWF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMLIN.LE.0)GOTO1390
C
      DO1300ILINE=1,NUMLIN
      IROW=IPOINT(ILINE)
      J1=ILOCC1(IROW)
      N1=NUMCPL(IROW)
      J2=J1+(N1-1)
      WRITE(IOUNIT,1305)(ICHA(J),J=J1,J2)
 1305 FORMAT(238A1)
 1300 CONTINUE
C
 1390 CONTINUE
C
C               **********************
C               **  STEP 4--        **
C               **  CLOSE THE FILE  **
C               **********************
C
      ISTEPN='4'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'WRWF')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC ENDFILE IOUNIT
CCCCC REWIND IOUNIT
CCCCC CLOSE(UNIT=IOUNIT)
      IENDFI='ON'
      IREWIN='ON'
      CALL EDCLFI(IOUNIT,IENDFI,IREWIN)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'WRWF')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDWRWF')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDWRWF')
 9011 FORMAT('*****AT THE END       OF EDWRWF--')
      WRITE(ICOUT,9012)ID
      CALL EDWRST('EDWRWF')
 9012 FORMAT('ID = ',A4)
      WRITE(ICOUT,9013)IFILE
      CALL EDWRST('EDWRWF')
 9013 FORMAT('IFILE = ',A80)
      WRITE(ICOUT,9014)IOUNIT
      CALL EDWRST('EDWRWF')
 9014 FORMAT('IOUNIT = ',I8)
      WRITE(ICOUT,9015)ISTAT
      CALL EDWRST('EDWRWF')
 9015 FORMAT('ISTAT = ',A12)
      WRITE(ICOUT,9016)NUMLIN,NUMCHA
      CALL EDWRST('EDWRWF')
 9016 FORMAT('NUMLIN,NUMCHA = ',2I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDWRWF')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDX
C
C     PURPOSE--DETERMINE WHICH HELD COMMAND
C              WILL BE EXECUTED AND SET A SWITCH
C              SO THAT IT WILL BE EXECUTED.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JANUARY 1985.
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDX '
      ISUBN2='    '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'CALL')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDX   ')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDX   ')
   51 FORMAT('***** AT THE BEGINNING OF EDX--')
      WRITE(ICOUT,52)ICXQT,IXQT
      CALL EDWRST('EDX   ')
   52 FORMAT('ICXQT,IXQT = ',A4,I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDX   ')
      IF(IBUGT1.EQ.'ON')CALL EDTRA1
   90 CONTINUE
C
C               *********************************
C               **  STEP 1--                   **
C               **  DETERMINE WHICH HELD LINE  **
C               **  IS TO BE EXECUTED.         **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGE2.EQ.'ON'.OR.ISUBRO.EQ.'X')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      IXQT=1
C
      IF(NUMARG.LE.0)GOTO1100
      GOTO1110
C
 1100 CONTINUE
      GOTO1190
C
 1110 CONTINUE
      IF(IARGT(1).EQ.'NUMB')IXQT=IARG(1)
      GOTO1190
C
 1190 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  SET THE EXECUTE SWITCH TO   ON  **
C               **************************************
C
      ISTEPN='2'
      IF(IBUGFI.EQ.'ON'.OR.ISUBRO.EQ.'CALL')
     1CALL EDTRA3(ISTEPN,ISUBN1,ISUBN2)
C
      ICXQT='ON'
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGFI.EQ.'OFF'.AND.ISUBRO.NE.'CALL')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDX   ')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDX   ')
 9011 FORMAT('***** AT THE END       OF EDX--')
      WRITE(ICOUT,9013)ICXQT,IXQT
      CALL EDWRST('EDX   ')
 9013 FORMAT('ICXQT,IXQT = ',A4,I8)
      WRITE(ICOUT,999)
      CALL EDWRST('EDX   ')
      IF(IBUGT2.EQ.'ON')CALL EDTRA2
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE EDZERO(ICNEWL,NCNEWL)
C
C     PURPOSE--BLANK-OUT THE 240-CHAREACTER VARIABLE ICNEWL
C              AND SET NCNEWL TO ZERO.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.1
C     ORIGINAL VERSION--MAY     1986.
C
C---------------------------------------------------------------------
C
      CHARACTER*240 ICNEWL
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (EDITING)-----------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'EDCOMM.INC'
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='EDZE'
      ISUBN2='RO  '
C
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INSE')GOTO90
      WRITE(ICOUT,999)
      CALL EDWRST('EDZERO')
  999 FORMAT(1X)
      WRITE(ICOUT,51)
      CALL EDWRST('EDZERO')
   51 FORMAT('***** AT THE BEGINNING OF EDZERO--')
      WRITE(ICOUT,52)NCNEWL
      CALL EDWRST('EDZERO')
   52 FORMAT('NCNEWL = ',I8)
      WRITE(ICOUT,53)(ICNEWL(I:I),I=1,100)
      CALL EDWRST('EDZERO')
   53 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
   90 CONTINUE
C
C               ****************************************
C               **  STEP 11--                         **
C               **  BLANK-OUT THE CHARACTER VARIABLE  **
C               **  ZERO -OUT THE INTEGER             **
C               ****************************************
C
      DO1100I=1,240
      ICNEWL(I:I)=' '
 1100 CONTINUE
      NCNEWL=0
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGE2.EQ.'OFF'.AND.ISUBRO.NE.'INSE')GOTO9090
      WRITE(ICOUT,999)
      CALL EDWRST('EDZERO')
      WRITE(ICOUT,9011)
      CALL EDWRST('EDZERO')
 9011 FORMAT('***** AT THE END       OF EDZERO--')
      WRITE(ICOUT,9012)NCNEWL
      CALL EDWRST('EDZERO')
 9012 FORMAT('NCNEWL = ',I8)
      WRITE(ICOUT,9013)(ICNEWL(I:I),I=1,100)
      CALL EDWRST('EDZERO')
 9013 FORMAT('(ICNEWL(I:I),I=1,100) = ',100A1)
 9090 CONTINUE
C
      RETURN
      END

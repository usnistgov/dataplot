      SUBROUTINE DPECHO(IANS,IWIDTH)
C
C     PURPOSE--ECHO THE CURRENT COMMAND LINE.
C              THIS IS ESPECIALLY USEFUL WHEN A SET OF
C              DATAPLOT COMMANDS ARE 'ADDED' IN BULK
C              FROM A MACRO ON MASS STORAGE.
C     INPUT  ARGUMENTS--IANS   (A  HOLLERITH VECTOR WHOSE
C                              I-TH ELEMENT CONTAINS THE
C                              I-TH CHARACTER OF THE
C                              ORIGINAL INPUT COMMAND LINE.
C                     --IWIDTH (AN INTEGER VARIABLE WHICH
C                              CONTAINS THE NUMBER OF CHARACTERS
C                              IN THE ORIGINAL COMMAND LINE.
C     OUTPUT ARGUMENTS--NONE
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
C     ORIGINAL VERSION--OCTOBER   1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --MAY       2009. HAVE 2 SEPARATE LIMITS:
C                                       1) MAX CHARS PER LINE
C                                       2) MAX CHARS PER DPWRST
C                                       BE SURE TO CHECK BOTH
C                                       LIMITS OK
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS
C
      CHARACTER*4 ISTAR
      CHARACTER*4 IBLANK
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISTAR='*'
      IBLANK=' '
C
      ISTART=1
      IF(IWIDTH.LE.0)GOTO240
      DO200I=1,IWIDTH
      IREV=IWIDTH-I+1
      IF(IANS(IREV).NE.' ')GOTO220
  200 CONTINUE
      ISTOP=ISTART
      GOTO260
  220 CONTINUE
      ISTOP=IREV
      GOTO260
  240 CONTINUE
      ISTOP=1
      GOTO260
  260 CONTINUE
C
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      ISTOP8=ISTOP+8
      WRITE(ICOUT,261)(ISTAR,I=1,MIN(124,ISTOP8))
  261 FORMAT(6X,124A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,262)ISTAR,ISTAR,IBLANK,IBLANK,
     1(IANS(I),I=ISTART,MIN(124,ISTOP)),IBLANK,IBLANK,ISTAR,ISTAR
  262 FORMAT(6X,4A1,124A1,4A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,261)(ISTAR,I=1,MIN(124,ISTOP8))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
      RETURN
      END
      SUBROUTINE DPECSW(IHARG,NUMARG,
     1IECHO,IFOUND,IERROR)
C
C     PURPOSE--SPECIFY THE ECHO SWITCH WHICH IN TURN
C              DETERMINES WHETHER ENTERED COMMANDS WILL BE
C                 ECHOED BACK    (IN A BOX FOR ACCENTUATION)
C              TO THE TERMINAL.
C              THIS CAPABILITY IS USEFUL FOR MONITORING THE
C              PROGRESS OF A MACRO WHICH HAS BEEN ADDED
C              FROM MASS STORAGE.
C              THE SPECIFIED ECHO SWITCH SPECIFICATION
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IECHO.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IECHO  (A HOLLERITH VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     ORIGINAL VERSION--OCTOBER   1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IECHO
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
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1160
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
      IECHO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IECHO
 1181 FORMAT('THE ECHO SWITCH HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPEINL(ICAPSW,IFORSW,ISUBRO,IBUGA2,IBUGA3,IBUGQ,
     1                  IFOUND,IERROR)
C
C     PURPOSE--PERFORM AN INTERLABORATORY STUDY ACCORDING TO
C              ASTM E 691 STANDARD.
C
C              THE DATA CONSISTS OF DATA GROUPED BY MATERIALS AND
C              LABS.  EACH COMINATION OF MATERIAL AND LAB IS REFERRED
C              TO AS A CELL AND IT IS ASSUMED THAT THE CELLS HAVE
C              THE SAME NUMBER OF REPLICATIONS.
C
C              THERE ARE 4 BASIC
C              QUANTITIES COMPUTED:
C              1) REPEATABILITY STANDARD DEVIATION
C              2) REPRODUCIBILITY STANDARD DEVIATION
C              3) H CONSISTENCY STATISTIC
C              4) K CONSISTENCY STATISTIC
C              THESE ARE ALL NOW AVAILABLE AS SEPARATE COMMANDS.
C              THE PRIMARY FUNCTION OF THIS
C
C                  E691 INTERLAB  Y  LABID  MATID
C
C              COMMAND IS TO GENERATE THE FOLLOWING 4 TABLES BASED
C              ON THESE BASIC QUANTITIES:
C              1) FOR EACH MATERIAL, PRINT
C                 A) LAB ID
C                 B) Cell Average
C                 C) Cell Standard Deviation
C                 D) Deviation of Cell Average from Overall Average
C                    (for that material)
C                 E) h Consistency Statistic for each cell
C                 F) k Consistency Statistic for each cell
C              2) A TWO-WAY TABLE (ROWS ARE LABS AND COLUMNS ARE
C                 MATERIALS) OF THE h CONSISTENCY STATISTIC
C              3) A TWO-WAY TABLE (ROWS ARE LABS AND COLUMNS ARE
C                 MATERIALS) OF THE k CONSISTENCY STATISTIC
C              4) A TABLE SUMMARIZING THE PRECISION STATISTICS
C                 FOR EACH MATERIAL.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C     UPDATED         --AUGUST    2010. ISSUE WITH CODED LABS
C     UPDATED         --AUGUST    2010. USE DPPARS TO PROCESS COMMAND
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASE
C
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
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      REAL KCV
C
C----------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
C
      DIMENSION Z1(MAXOBV)
      DIMENSION Z2(MAXOBV)
      DIMENSION Z3(MAXOBV)
      DIMENSION Z4(MAXOBV)
      DIMENSION Z5(MAXOBV)
      DIMENSION Z6(MAXOBV)
      DIMENSION Z7(MAXOBV)
      DIMENSION Z8(MAXOBV)
      DIMENSION Z9(MAXOBV)
      DIMENSION Z10(MAXOBV)
      DIMENSION Z11(MAXOBV)
      DIMENSION Z12(MAXOBV)
      DIMENSION Z13(MAXOBV)
      DIMENSION Z14(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),Y3(1))
      EQUIVALENCE (GARBAG(IGARB4),Z1(1))
      EQUIVALENCE (GARBAG(IGARB5),Z2(1))
      EQUIVALENCE (GARBAG(IGARB7),Z3(1))
      EQUIVALENCE (GARBAG(IGARB9),Z4(1))
      EQUIVALENCE (GARBAG(JGAR11),Z5(1))
      EQUIVALENCE (GARBAG(JGAR12),Z6(1))
      EQUIVALENCE (GARBAG(JGAR13),Z7(1))
      EQUIVALENCE (GARBAG(JGAR14),Z8(1))
      EQUIVALENCE (GARBAG(JGAR15),Z9(1))
      EQUIVALENCE (GARBAG(JGAR16),Z10(1))
      EQUIVALENCE (GARBAG(JGAR17),Z11(1))
      EQUIVALENCE (GARBAG(JGAR18),Z12(1))
      EQUIVALENCE (GARBAG(JGAR19),Z13(1))
      EQUIVALENCE (GARBAG(JGAR20),Z14(1))
C
C-----COMMON----------------------------------------------------
C
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT------------------------------------------------
C
      ISUBN1='DPEI'
      ISUBN2='NL  '
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      MAXV2=100
C
C               *********************************************
C               **  TREAT THE E691 INTERLAB ANALYSIS CASE  **
C               *********************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EINL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEINL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EINL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.2.AND.ICOM.EQ.'E691'.AND.
     1   IHARG(1).EQ.'INTE'.AND.IHARG(2).EQ.'ANAL')THEN
        ILASTC=2
      ELSEIF(NUMARG.GE.1.AND.ICOM.EQ.'E691'.AND.
     1   IHARG(1).EQ.'INTE')THEN
        ILASTC=1
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EINL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='E691 INTERLAB'
      MINNA=2
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=3
      MAXNVA=3
      DO210I=1,MAXSPN
        IVARTY(I)='XXXX'
  210 CONTINUE
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EINL')THEN
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
      ICASE='VARI'
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y2,Y3,NS,NJUNK2,NJUNK3,ICASE,
     1            IBUGA3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IH='ALPH'
      IH2='A   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ALPHA=0.005
      ELSE
        ALPHA=VALUE(ILOCP)
      ENDIF
      IF(ALPHA.LE.0.0 .OR. ALPHA.GE.1.0)ALPHA=0.005
      IF(ALPHA.GT.0.50)ALPHA=1.0 - ALPHA
C
C               ****************************************
C               **  STEP 3B--                         **
C               **  EXTRACT GROUP LABEL VARIABLE      **
C               ****************************************
      IGRPID=-1
      IF(I691GL.NE.'NONE')THEN
        DO310I=1,MAXGRP
          IF(I691GL(1:8).EQ.IGRPVN(I)(1:8))THEN
            IGRPID=I
            GOTO319
          ENDIF
  310   CONTINUE
  319   CONTINUE
      ENDIF
C
C               ***********************************************
C               **  STEP 9--                                 **
C               **  CARRY OUT THE E691 INTERLAB ANALYSIS     **
C               ***********************************************
C
      ISTEPN='9'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EINL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EINL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,711)
  711   FORMAT('***** FROM DPEINL, AS WE ARE ABOUT TO CALL DPEIN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,712)NRIGHT(1),MAXN,NS,NUMVAR
  712   FORMAT('NRIGHT(1),MAXN,NS,NUMVAR = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO715I=1,NS
          WRITE(ICOUT,716)I,Y1(I),Y2(I),Y3(I)
  716     FORMAT('I,Y1(I),Y2(I),Y3(I) = ',
     1           I6,2X,3G15.7)
          CALL DPWRST('XXX','BUG ')
  715   CONTINUE
      ENDIF
C
      IWRITE='OFF'
      CALL DPEIN2(Y1,Y2,Y3,NS,
     1            Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9,Z10,Z11,Z12,Z13,Z14,
     1            ALPHA,HCV,KCV,
     1            IGRPID,IGRPLA,MAXGLA,MAXGRP,
     1            IWRITE,IFORSW,ICAPSW,ICAPTY,
     1            ISUBRO,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 10--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='10'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EINL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH='HCV '
      IH2='    '
      VALUE0=HCV
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='KCV '
      IH2='    '
      VALUE0=KCV
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EINL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEINL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,NS,NUMVAR
 9012   FORMAT('IFOUND,IERROR,NS,NUMVAR = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEIN2(Y,X1,X2,N,XIDTEM,XIDTE2,
     1                  XBAR,XBARI,SDI,SDXBRI,DXBARI,H,AK,SRPT,SRPRD,
     1                  TEMP1,TEMP2,TAG,
     1                  ALPHA,HCV,KCV,
     1                  IGRPID,IGRPLA,MAXGLA,MAXGRP,
     1                  IWRITE,IFORSW,ICAPSW,ICAPTY,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--PERFORM AN INTERLABORATORY STUDY ACCORDING TO
C              ASTM E 691 STANDARD.
C
C              THE DATA CONSISTS OF DATA GROUPED BY MATERIALS AND
C              LABS.  EACH COMINATION OF MATERIAL AND LAB IS REFERRED
C              TO AS A CELL AND IT IS ASSUMED THAT THE CELLS HAVE
C              THE SAME NUMBER OF REPLICATIONS.
C
C              THERE ARE 4 BASIC
C              QUANTITIES COMPUTED:
C              1) REPEATABILITY STANDARD DEVIATION
C              2) REPRODUCIBILITY STANDARD DEVIATION
C              3) H CONSISTENCY STATISTIC
C              4) K CONSISTENCY STATISTIC
C              THESE ARE ALL NOW AVAILABLE AS SEPARATE COMMANDS.
C              THE PRIMARY FUNCTION OF THIS
C
C                  E691 INTERLAB  Y  LABID  MATID
C
C              COMMAND IS TO GENERATE THE FOLLOWING 4 TABLES BASED
C              ON THESE BASIC QUANTITIES:
C              1) FOR EACH MATERIAL, PRINT
C                 A) LAB ID
C                 B) Cell Average
C                 C) Cell Standard Deviation
C                 D) Deviation of Cell Average from Overall Average
C                    (for that material)
C                 E) h Consistency Statistic for each cell
C                 F) k Consistency Statistic for each cell
C              2) A TWO-WAY TABLE (ROWS ARE LABS AND COLUMNS ARE
C                 MATERIALS) OF THE h CONSISTENCY STATISTIC
C              3) A TWO-WAY TABLE (ROWS ARE LABS AND COLUMNS ARE
C                 MATERIALS) OF THE k CONSISTENCY STATISTIC
C              4) A TABLE SUMMARIZING THE PRECISION STATISTICS
C                 FOR EACH MATERIAL.
C     PRINTING--YES
C     SUBROUTINES NEEDED--FCDF
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --AUGUST    2010. BUG WHEN LAB/MATERIAL NOT
C                                       "CODED"
C     UPDATED         --MARCH     2015. USE DPDTxx ROUTINES TO PRINT
C                                       TABLES
C     UPDATED         --MARCH     2015. USER CAN SPECIFY WHICH TABLES
C                                       TO PRINT
C     UPDATED         --APRIL     2015. ADD TABLE TO PRINT RAW DATA
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
C
      CHARACTER*40 IGRPLA(MAXGLA,MAXGRP)
      CHARACTER*4 IFORSW
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*20 IFORMT
C
      REAL KCV
C
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DXREP
      DOUBLE PRECISION XREPRD
C
C----------------------------------------------------------------
C
      REAL Y(*)
      REAL X1(*)
      REAL X2(*)
      REAL XIDTEM(*)
      REAL XIDTE2(*)
      REAL XBAR(*)
      REAL XBARI(*)
      REAL SDI(*)
      REAL SDXBRI(*)
      REAL DXBARI(*)
      REAL H(*)
      REAL AK(*)
      REAL SRPT(*)
      REAL SRPRD(*)
      REAL TEMP1(*)
      REAL TEMP2(*)
      REAL TAG(*)
C
      INCLUDE 'DPCOST.INC'
C
      PARAMETER(MAXLIN=2)
      PARAMETER(MAXROW=50)
      PARAMETER(MAXCOL=7)
      CHARACTER*4  ALIGN(MAXCOL)
      CHARACTER*4  VALIGN(MAXCOL)
      INTEGER      IDIGIT(MAXCOL)
      INTEGER      IDIGI2(MAXROw,MAXCOL)
      INTEGER      NTOT(MAXCOL)
      INTEGER      NCTEXT(MAXROW)
      CHARACTER*4  ITEXT(MAXROW)
      CHARACTER*40 ITTEMP
      CHARACTER*60 ITITL9
      CHARACTER*60 ITITLE
      CHARACTER*40 ITITL2(MAXLIN,MAXCOL)
      CHARACTER*15 IVALUE(MAXROW,MAXCOL)
      CHARACTER*4  ITYPCO(MAXCOL)
      INTEGER      NCVALU(MAXROW,MAXCOL)
      INTEGER      NCOLSP(MAXLIN,MAXCOL)
      INTEGER      ROWSEP(MAXROW)
      INTEGER      NCTIT2(MAXLIN,MAXCOL)
      INTEGER      IWHTML(MAXCOL)
      INTEGER      IWRTF(MAXCOL)
      REAL         AMAT(MAXROW,MAXCOL)
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
C
      LOGICAL IFLAGA
      LOGICAL IFLAGB
      LOGICAL IFLAGS
      LOGICAL IFLAGE
      LOGICAL IFRSTL
      LOGICAL ILASTL
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPEI'
      ISUBN2='N2  '
C
      NUMDIG=4
      IF(IFORSW.EQ.'1')NUMDIG=2
      IF(IFORSW.EQ.'2')NUMDIG=2
      IF(IFORSW.EQ.'3')NUMDIG=3
      IF(IFORSW.EQ.'4')NUMDIG=4
      IF(IFORSW.EQ.'5')NUMDIG=5
      IF(IFORSW.EQ.'6')NUMDIG=6
      IF(IFORSW.EQ.'7')NUMDIG=7
      IF(IFORSW.EQ.'8')NUMDIG=7
      IF(IFORSW.EQ.'9')NUMDIG=7
      IF(IFORSW.EQ.'0')NUMDIG=7
      IF(IFORSW.EQ.'-2')NUMDIG=-2
      IF(IFORSW.EQ.'-3')NUMDIG=-3
      IF(IFORSW.EQ.'-4')NUMDIG=-4
      IF(IFORSW.EQ.'-5')NUMDIG=-5
      IF(IFORSW.EQ.'-6')NUMDIG=-6
      IF(IFORSW.EQ.'-7')NUMDIG=-7
      IF(IFORSW.EQ.'-8')NUMDIG=-8
      IF(IFORSW.EQ.'-9')NUMDIG=-9
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EIN2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEIN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N
   52   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,Y(I),X1(I),X2(I)
   56     FORMAT('I,Y(I),X1(I),X2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN E691 INTERLAB ANALYSIS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE ',
     1         'E691 INTERLAB ANALYSIS')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      MUST BE AT LEAST 4; THE NUMBER OF ',
     1         'OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               **************************************************
C               **  STEP 1.1--                                  **
C               **   OPEN THE STORAGE FILES                     **
C               **************************************************
C
      ISTEPN='1.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOP='OPEN'
      IFLG1=1
      IFLG2=1
      IFLG3=1
      IFLG4=1
      IFLG5=0
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DISTIN(X1,N,IWRITE,XIDTEM,NUMSE1,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
CCCCC CALL SORT(XIDTEM,NUMSE1,XIDTEM)
C
      CALL DISTIN(X2,N,IWRITE,XIDTE2,NUMSE2,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
CCCCC CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
      NLAB=NUMSE1
      NMAT=NUMSE2
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')THEN
        WRITE(ICOUT,122)NUMSE1,NUMSE2,NLAB,NMAT
  122   FORMAT('NUMSE1,NUMSE2,NLAB,NMAT=',4I8)
        CALL DPWRST('XXX','WRIT')
        DO124I=1,NUMSE1
          WRITE(ICOUT,125)I,XIDTEM(I)
  125     FORMAT('I,XIDTEM(I)=',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
  124   CONTINUE
        DO128I=1,NUMSE2
          WRITE(ICOUT,127)I,XIDTE2(I)
  127     FORMAT('I,XIDTE2(I)=',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
  128   CONTINUE
      ENDIF
C
      IF(NLAB.LT.2 .OR. NLAB.GE.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,212)
  212   FORMAT('      FOR THE E691 INTERLAB COMMAND, THE SECOND ',
     1         'VARIABLE IS THE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,214)
  214   FORMAT('      LAB ID VARIABLE.  THE NUMBER OF LABS SHOULD ',
     1         'BE AT LEAST 2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,216)
  216   FORMAT('      AND LESS THAN THE NUMBER OF POINTS.  SUCH WAS ',
     1         'NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,218)NLAB
  218   FORMAT('      THE NUMBER OF UNIQUE LAB IDS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,219)N
  219   FORMAT('      THE TOTAL NUMBER OF POINTS   = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NMAT.LT.2 .OR. NMAT.GE.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,222)
  222   FORMAT('      FOR THE E691 INTERLAB COMMAND, THE THIRD ',
     1         'VARIABLE IS THE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,224)
  224   FORMAT('      MATERIAL ID VARIABLE.  THE NUMBER OF MATERIALS ',
     1         'SHOULD BE AT LEAST 2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,226)
  226   FORMAT('      AND LESS THAN THE NUMBER OF POINTS.  SUCH WAS ',
     1         'NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,228)NMAT
  228   FORMAT('      THE NUMBER OF UNIQUE MATERIAL IDS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,229)N
  229   FORMAT('      THE TOTAL NUMBER OF POINTS        = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      ISTEPN='2.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      NOUT=0
      DO1110ISET2=1,NUMSE2
C
C  STEP 1: COMPUTE OVERALL MEAN FOR CURRENT MATERIAL
C
        K=0
        DO1120I=1,N
          IF(XIDTE2(ISET2).EQ.X2(I))THEN
            K=K+1
            TEMP1(K)=Y(I)
          ENDIF
 1120   CONTINUE
        NTEMP=K
        CALL MEAN(TEMP1,NTEMP,IWRITE,XMEAN,IBUGA3,IERROR)
        XBAR(ISET2)=XMEAN
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')THEN
          WRITE(ICOUT,1128)ISET2,XBAR(ISET2)
 1128     FORMAT('ISET2,XBAR(ISET2) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
        DSUM=0.0D0
        DO1130ISET1=1,NUMSE1
C
          NOUT=(ISET2-1)*NUMSE1 + ISET1
          TAG(NOUT)=REAL(ISET2)
C
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')THEN
            WRITE(ICOUT,1138)ISET1,NOUT
 1138       FORMAT('ISET1,NOUT = ',2I8)
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
          K=0
          DO1140I=1,N
            IF(XIDTEM(ISET1).EQ.X1(I).AND.XIDTE2(ISET2).EQ.X2(I))THEN
              K=K+1
              TEMP1(K)=Y(I)
            ENDIF
 1140     CONTINUE
          NTEMP=K
C
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')THEN
            WRITE(ICOUT,1141)XIDTEM(ISET1),XIDTE2(ISET2),NTEMP
 1141       FORMAT('XIDTEM(ISET1),XIDTE2(ISET2),NTEMP = ',2G15.7,I8)
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
          IF(ISET1.EQ.1 .AND. ISET2.EQ.1)THEN
            NHOLD=NTEMP
          ELSE
            IF(NTEMP.NE.NHOLD)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1146)ISET1,ISET2
 1146         FORMAT('      FOR LAB ',I8,' AND MATERIAL ',I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1148)NHOLD,NTEMP
 1148         FORMAT('      ',I8,' ELEMENTS EXPECTED BUT ',I8,
     1               ' ELEMENTS FOUND.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
          ENDIF
C
          CALL MEAN(TEMP1,NTEMP,IWRITE,XMEAN,IBUGA3,IERROR)
          CALL SD(TEMP1,NTEMP,IWRITE,XSD,IBUGA3,IERROR)
          DSUM=DSUM + DBLE(XSD)**2
          XBARI(NOUT)=XMEAN
          SDI(NOUT)=XSD
          AK(NOUT)=SDI(NOUT)
          DXBARI(NOUT)=XBARI(NOUT) - XBAR(ISET2)
          H(NOUT)=XBARI(NOUT) - XBAR(ISET2)
          TEMP2(ISET1)=XBARI(NOUT)
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ONS2')THEN
            WRITE(ICOUT,1149)ISET1,ISET2,XBAR(ISET2),XBARI(NOUT)
 1149       FORMAT('ISET1,ISET2,XBAR(ISET2),XBARI(NOUT) = ',
     1             2I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 1130   CONTINUE
C
        CALL SD(TEMP2,NUMSE1,IWRITE,XSD,IBUGA3,IERROR)
        SDXBRI(ISET2)=XSD
        DXREP=DSQRT(DSUM/DBLE(NUMSE1))
        SRPT(ISET2)=REAL(DXREP)
C
        DXREP=DSUM/DBLE(NUMSE1)
        XREPRD=DSQRT(DBLE(XSD**2) + DXREP*DBLE(NHOLD-1)/DBLE(NHOLD))
        SRPRD(ISET2)=REAL(MAX(DSQRT(DXREP),XREPRD))
C
        DO1150I=(ISET2-1)*NUMSE1+1,ISET2*NUMSE1
          H(I)=H(I)/SDXBRI(ISET2)
          AK(I)=AK(I)/SRPT(ISET2)
 1150   CONTINUE
C
 1110 CONTINUE
      NOUT=NUMSE1*NUMSE2
C
      ANLAB=REAL(NLAB)
      IDF=NLAB-2
      ALP2=1.0  - (ALPHA/2.0)
      CALL TPPF(ALP2,REAL(IDF),TVAL)
      HCV=(ANLAB - 1.0)*TVAL/SQRT(ANLAB*(TVAL**2 + ANLAB - 2.0))
      HCV=REAL(INT(HCV*100.0 + 0.5))
      HCV=HCV/100.0
      IDF1=NHOLD-1
      IDF2=(NHOLD-1)*(NLAB-1)
      ALP2=1.0 - ALPHA
      CALL FPPF(ALP2,IDF1,IDF2,FVAL)
      KCV=SQRT(ANLAB/(1.0 + (ANLAB-1.0)/FVAL))
      KCV=REAL(INT(KCV*100.0 + 0.5))
      KCV=KCV/100.0
C
C               ***********************************************
C               **  STEP 3.1--                               **
C               **  WRITE COMPUTED INFORMATION TO FILE       **
C               ***********************************************
C
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICNT=0
      DO3100ISET2=1,NUMSE2
        IMAT=INT(XIDTE2(ISET2)+0.5)
        DO3110ISET1=1,NUMSE1
          ILAB=INT(XIDTEM(ISET1)+0.5)
          ICNT=ICNT+1
          WRITE(IOUNI1,3119)IMAT,ILAB,XBARI(ICNT),SDI(ICNT),
     1                      DXBARI(ICNT),H(ICNT),AK(ICNT)
 3119     FORMAT(I8,1X,I8,3(1X,E15.7),2(1X,F10.2))
 3110   CONTINUE
 3100 CONTINUE
C
      ICNT=0
      IFORMT=' '
      IFORMT(1:15)='(I8,1X,  F10.2)'
      IF(NMAT.LE.9)THEN
        WRITE(IFORMT(9:9),'(I1)')NMAT
      ELSE
        WRITE(IFORMT(8:9),'(I2)')NMAT
      ENDIF
      DO3200ISET1=1,NUMSE1
        ILAB=INT(XIDTEM(ISET1)+0.5)
        WRITE(IOUNI2,IFORMT)ILAB,(H(II),II=ILAB,NMAT*NLAB,NUMSE1)
        WRITE(IOUNI3,IFORMT)ILAB,(AK(II),II=ILAB,NMAT*NLAB,NUMSE1)
 3200 CONTINUE
C
      AR=2.8
      DO3400ISET2=1,NUMSE2
        IMAT=INT(XIDTE2(ISET2)+0.5)
        AR1=2.8*SRPT(ISET2)
        AR2=2.8*SRPRD(ISET2)
        WRITE(IOUNI4,3419)IMAT,XBAR(ISET2),SDXBRI(ISET2),
     1                      SRPT(ISET2),SRPRD(ISET2),AR1,AR2
 3419   FORMAT(I8,4(1X,E15.7),2(1X,F7.2))
 3400 CONTINUE
C
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C
C               ***********************************************
C               **  STEP 2.1--                               **
C               **  PRINT THE RESULTS                        **
C               ***********************************************
C
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EIN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO4001I=1,MAXROW
        NCTEXT(I)=0
        ITEXT(I)=' '
        ROWSEP(I)=0
        DO4002J=1,MAXCOL
          AMAT(I,J)=-99.0
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
 4002   CONTINUE
 4001 CONTINUE
C
      IF(IPRINT.EQ.'ON')THEN
C
C  HEADER
C
        IF(NCTABT.LE.0)THEN
          ITITL9(1:30)='E 691 Interlaboratory Analysis'
          NCTIT9=30
        ELSE
          ITITL9(1:32)='E 691 Interlaboratory Analysis: '
          NCTEMP=NCTABT
          IF(NCTEMP.GT.28)NCTEMP=28
          ITITL9(33:33+NCTEMP-1)=ITABTI(1:NCTEMP)
          NCTIT9=32+NCTEMP
         ENDIF
C
C       PRINT TABLE 0 (RAW DATA TABLE)
C
        IF(I691DS.EQ.'OFF')GOTO4099
C
C       CREATE A "REPLICATION" VARIABLE
C
        DO4009II=1,N
          TEMP1(II)=0.0
 4009   CONTINUE
C
        DO4003II=1,NUMSE1
          ALABT=XIDTEM(II)
          DO4005JJ=1,NUMSE2
            AMATT=XIDTE2(JJ)
            ICNT=0
            DO4007KK=1,N
              IF(X1(KK).EQ.ALABT .AND. X2(KK).EQ.AMATT)THEN
                ICNT=ICNT+1
                TEMP1(KK)=REAL(ICNT)
              ENDIF
 4007       CONTINUE
 4005     CONTINUE
 4003   CONTINUE
        CALL MINIM(TEMP1,N,IWRITE,XMIN,IBUGA3,IERROR)
        CALL MAXIM(TEMP1,N,IWRITE,XMAX,IBUGA3,IERROR)
        MINREP=INT(XMIN+0.1)
        MAXREP=INT(XMAX+0.1)
        IF(MINREP.LE.0)THEN
          GOTO4099
CCCCC   ELSEIF(MINREP.NE.MAXREP)THEN
CCCCC     GOTO4099
        ENDIF
C
C       PRINT MATERIALS (LIMIT TO SIX ON A SINGLE TABLE)
C
        NTABLE=1
        NMAXMA=6
        IF(NUMSE2.GT.1)THEN
          NTABLE=NUMSE2/NMAXMA
          IF(MOD(NUMSE2,NMAXMA).GT.0)NTABLE=NTABLE+1
        ENDIF
C
        ICNT3=0
        DO4010ITABLE=1,NTABLE
C
          IMAT1=(ITABLE-1)*NMAXMA+1
          IMAT2=MIN(NUMSE2,IMAT1+NMAXMA-1)
C
          ITITLE(1:16)='Test Result Data'
          NCTITL=16
          NUMLIN=2
          NUMCOL=IMAT2-IMAT1+2
          ITITL2(1,1)=' '
          ITITL2(2,1)='Laboratory'
          NCTIT2(1,1)=0
          NCTIT2(2,1)=10
          NCOLSP(1,1)=1
          NCOLSP(2,1)=1
          ICNT=1
          NTOT(1)=10
C
          DO4020ISET2=IMAT1,IMAT2
            IMAT=INT(XIDTE2(ISET2)+0.5)
            ICNT=ICNT+1
            ICNT3=ICNT3+1
            IF(ICNT.EQ.2)THEN
              ITITL2(1,ICNT)='Material'
              NCTIT2(1,ICNT)=8
              NTEMP=IMAT2-IMAT1+1
              NCOLSP(1,ICNT)=NTEMP
            ELSE
              ITITL2(1,ICNT)=' '
              NCTIT2(1,ICNT)=0
              NCOLSP(1,ICNT)=1
            ENDIF
            IF(IGRPID.LE.0)THEN
              IF(IMAT.LE.9)THEN
                WRITE(ITITL2(2,ICNT)(1:1),'(I1)')IMAT
                NCTIT2(2,ICNT)=1
              ELSEIF(IMAT.LE.99)THEN
                WRITE(ITITL2(2,ICNT)(1:2),'(I2)')IMAT
                NCTIT2(2,ICNT)=2
              ELSE
                WRITE(ITITL2(2,ICNT)(1:3),'(I3)')IMAT
                NCTIT2(2,ICNT)=3
              ENDIF
            ELSE
              ILAST=1
              DO4022II=14,1,-1
                IF(IGRPLA(ICNT3,IGRPID)(II:II).NE.' ')THEN
                  ILAST=II
                  GOTO4023
                ENDIF
 4022         CONTINUE
 4023         CONTINUE
              ITITL2(2,ICNT)(1:ILAST)=IGRPLA(ISET2,IGRPID)(1:ILAST)
              NCTIT2(2,ICNT)=ILAST
            ENDIF
            NCOLSP(2,ICNT)=1
            NTOT(ICNT)=15
 4020     CONTINUE
          NUMCOL=ICNT
          NMAX=0
          IWRTF(1)=1300
          DO4030J=1,NUMCOL
            ITYPCO(J)='NUME'
            VALIGN(J)='b'
            ALIGN(J)='r'
            IF(J.EQ.1)ALIGN(J)='c'
            NMAX=NMAX+NTOT(J)
            DO4032JJ=1,MAXROW
              IDIGI2(JJ,J)=NUMDIG
              IF(J.EQ.1)IDIGI2(JJ,J)=0
              IDIGI2(JJ,J)=NUMDIG
 4032       CONTINUE
            IWHTML(J)=125
            IF(J.GT.1)THEN
              IWRTF(J)=IWRTF(J-1)+1600
            ENDIF
 4030     CONTINUE
          IDIGIT(1)=0
          IFRSTL=.TRUE.
          ILASTL=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
C
          ICNTRW=0
          DO4040II=1,NUMSE1
            ALABT=XIDTEM(II)
            IF(ICNTRW+MAXREP+1.GT.MAXROW)THEN
              CALL DPDT5B(ITITL9,NCTIT9,
     1                    ITITLE,NCTITL,ITITL2,NCTIT2,
     1                    MAXLIN,NUMLIN,MAXCOL,NUMCOL,
     1                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,
     1                    IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                    NCOLSP,ROWSEP,
     1                    ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                    IFLAGS,IFLAGE,
     1                    ISUBRO,IBUGA3,IERROR)
              ICNTRW=0
            ENDIF
C
C           LOOP THROUGH REPLICATIONS
C
            ICNT3=ICNTRW
            ICNT1=1
            DO4050KK=IMAT1,IMAT2
              AMATT=XIDTE2(KK)
              ICNT1=ICNT1+1
              DO4060JJ=1,MAXREP
                AREPT=REAL(JJ)
                DO4062IREPL=1,MAXREP
                  AMAT(ICNT3+IREPL,1)=ALABT
                  IDIGI2(ICNT3+IREPL,1)=0
 4062           CONTINUE
                IREPL=JJ
                DO4070LL=1,N
                  IF(X1(LL).EQ.ALABT .AND. X2(LL).EQ.AMATT .AND.
     1               TEMP1(LL).EQ.AREPT)THEN
                     AMAT(ICNT3+IREPL,ICNT1)=Y(LL)
                     IDIGI2(ICNT3+IREPL,ICNT1)=NUMDIG
                  ENDIF
 4070           CONTINUE
                DO4072IK=1,NUMCOL
                  AMAT(ICNT3+IREPL+1,ICNT1)=0.0
                  IDIGI2(ICNT3+IREPL+1,ICNT1)=-1
 4072           CONTINUE
                IF(II.LT.NUMSE1)THEN
                  AMAT(ICNT3+IREPL+1,1)=0.0
                  IDIGI2(ICNT3+IREPL+1,1)=-1
                ENDIF
 4060         CONTINUE
 4050       CONTINUE
            IF(II.LT.NUMSE1)THEN
              ICNTRW=ICNTRW+MAXREP+1
            ELSE
              ICNTRW=ICNTRW+MAXREP
            ENDIF
 4040     CONTINUE
C
          CALL DPDT5B(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,MAXCOL,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNTRW,
     1                IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                NCOLSP,ROWSEP,
     1                ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                IFLAGS,IFLAGE,
     1                ISUBRO,IBUGA3,IERROR)
C
 4010   CONTINUE
        NCTIT9=0
        ITITL9=' '
C
 4099   CONTINUE
C
C       PRINT TABLE 1
C
        IF(I691TR.EQ.'OFF')GOTO4199
C
        ICNT=0
        DO4110ISET2=1,NUMSE2
          IMAT=INT(XIDTE2(ISET2)+0.5)
          ITITLE(1:40)='Initial Preparation of Test Result Data '
          ITITLE(41:55)='for Material: '
C
          IF(IGRPID.GT.0)THEN
            ILAST=1
            DO4102II=14,1,-1
              IF(IGRPLA(ICNT3,IGRPID)(II:II).NE.' ')THEN
                ILAST=II
                GOTO4103
              ENDIF
 4102       CONTINUE
 4103       CONTINUE
            ITITLE(56:56+ILAST-1)=IGRPLA(ISET2,IGRPID)(1:ILAST)
            NCTITL=56+ILAST-1
          ELSE
            WRITE(ITITLE(56:60),'(I5)')IMAT
            NCTITL=60
          ENDIF
          NUMLIN=2
          NUMCOL=6
          ITITL2(1,1)='Laboratory'
          ITITL2(2,1)='Number'
          ITITL2(1,2)='Cell'
          ITITL2(2,2)='Mean'
          ITITL2(1,3)='Cell'
          ITITL2(2,3)='SD'
          ITITL2(1,4)=' '
          ITITL2(2,4)='d'
          ITITL2(1,5)=' '
          ITITL2(2,5)='h'
          ITITL2(1,6)=' '
          ITITL2(2,6)='k'
          NCTIT2(1,1)=10
          NCTIT2(2,1)=6
          NCTIT2(1,2)=4
          NCTIT2(2,2)=4
          NCTIT2(1,3)=4
          NCTIT2(2,3)=2
          NCTIT2(1,4)=0
          NCTIT2(2,4)=1
          NCTIT2(1,5)=0
          NCTIT2(2,5)=1
          NCTIT2(1,6)=0
          NCTIT2(2,6)=1
          NMAX=0
          NTOT(1)=10
          NTOT(2)=16
          NTOT(3)=16
          NTOT(4)=16
          NTOT(5)=11
          NTOT(6)=11
          DO4120J=1,NUMCOL
            VALIGN(J)='b'
            ALIGN(J)='r'
            IF(J.EQ.1)ALIGN(J)='c'
            NMAX=NMAX+NTOT(J)
            IDIGIT(J)=NUMDIG
            IWHTML(J)=125
 4120     CONTINUE
          IDIGIT(1)=0
          IDIGIT(5)=2
          IDIGIT(6)=2
          IWRTF(1)=1300
          IWRTF(2)=IWRTF(1)+1600
          IWRTF(3)=IWRTF(2)+1600
          IWRTF(4)=IWRTF(3)+1600
          IWRTF(5)=IWRTF(4)+1000
          IWRTF(6)=IWRTF(5)+1000
          IFRSTL=.TRUE.
          ILASTL=.TRUE.
C
          ICNT2=0
          DO4130ISET1=1,NUMSE1
            ICNT=ICNT+1
            ICNT2=ICNT2+1
            IF(ICNT2.GT.MAXROW)THEN
              ICNT2=MAXROW
              CALL DPDTA2(ITITL9,NCTIT9,
     1                    ITITLE,NCTITL,ITITL2,NCTIT2,
     1                    MAXLIN,NUMLIN,MAXCOL,NUMCOL,
     1                    ITEXT,NCTEXT,AMAT,MAXROW,ICNT2,
     1                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                    ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                    ISUBRO,IBUGA3,IERROR)
              ICNT2=1
            ENDIF
            AMAT(ICNT2,1)=XIDTEM(ISET1)
            AMAT(ICNT2,2)=XBARI(ICNT)
            AMAT(ICNT2,3)=SDI(ICNT)
            AMAT(ICNT2,4)=DXBARI(ICNT)
            AMAT(ICNT2,5)=H(ICNT)
            AMAT(ICNT2,6)=AK(ICNT)
 4130     CONTINUE
C
          IF(ICNT2.GT.0)THEN
            CALL DPDTA2(ITITL9,NCTIT9,
     1                  ITITLE,NCTITL,ITITL2,NCTIT2,
     1                  MAXLIN,NUMLIN,MAXCOL,NUMCOL,
     1                  ITEXT,NCTEXT,AMAT,MAXROW,ICNT2,
     1                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                  ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                  ISUBRO,IBUGA3,IERROR)
          ENDIF
C
          ISIZE=0
          IRTFMD='OFF'
          IFLAGA=.TRUE.
          IFLAGB=.FALSE.
          NTOTAL=40
          NBLNK1=0
          NBLNK2=0
          ITYPE=2
          ITTEMP='Average of cell averages: '
          NCTEMP=26
          CALL DPDTXT(ITTEMP,NCTEMP,XBAR(ISET2),NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
          IFLAGA=.FALSE.
          ITTEMP='Standard deviation of cell averages: '
          NCTEMP=37
          CALL DPDTXT(ITTEMP,NCTEMP,SDXBRI(ISET2),NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
          ITTEMP='Repeatability standard deviation: '
          NCTEMP=34
          CALL DPDTXT(ITTEMP,NCTEMP,SRPT(ISET2),NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
          ITTEMP='Reproducibility standard deviation: '
          NCTEMP=36
          NBLNK1=0
          NBLNK2=1
          IFLAGB=.TRUE.
          CALL DPDTXT(ITTEMP,NCTEMP,SRPRD(ISET2),NUMDIG,NTOTAL,
     1                NBLNK1,NBLNK2,
     1                IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
C
C         ONLY PUT MAIN TITLE ON FIRST TABLE
C
          ITITL9=' '
          NCTIT9=0
C
 4110   CONTINUE
C
 4199   CONTINUE
C
C       TABLE 2
C
        IF(I691HC.EQ.'OFF')GOTO4299
C
C       NOTE: DETERMINE HOW MANY MATERIALS CAN REASONABLY BE PRINTED FOR
C             THE TABLE (I.E., DOES THE TABLE NEED TO BE PRINTED IN
C             MORE THAN 1 ITERATION).
C
C             BASE ON MAXIMUM OF 6 MATERIALS PER TABLE.
C
CCCCC   NITEMS=(ILPRCO - 11)/10
CCCCC   NIT=(NMAT/NITEMS) + 1
        NITEMS=6
        NIT=(NMAT/NITEMS) + 1
        IF(MOD(NMAT,NITEMS).EQ.0)NIT=NIT-1
C
        DO4210ITER=1,MAX(NIT,1)
C
          IFRST=(ITER-1)*NITEMS + 1
          ILAST=MIN(ITER*NITEMS,NMAT)
          NTEMP=ILAST-IFRST+1
          NUMCOL=NTEMP+1
C
          ITITLE(1:1)='h'
          NCTITL=1
          NUMLIN=2
          ITITL2(1,1)='Laboratory'
          ITITL2(2,1)='Number'
          ITITL2(1,2)='Material'
          NCTIT2(1,1)=10
          NCTIT2(2,1)=6
          NCTIT2(1,2)=8
          JTEMP=INT(XIDTE2(IFRST)+0.5)
          IF(IGRPID.GT.0)THEN
            ILAST2=1
            DO4202II=10,1,-1
              IF(IGRPLA(IFRST,IGRPID)(II:II).NE.' ')THEN
                 LAST2=II
                 GOTO4203
              ENDIF
 4202       CONTINUE
 4203       CONTINUE
            ITITL2(2,2)(1:ILAST2)=IGRPLA(IFRST,IGRPID)(1:ILAST2)
            NCTIT2(2,2)=ILAST2
          ELSE
            IF(JTEMP.LE.9)THEN
              WRITE(ITITL2(2,2)(1:1),'(I1)')JTEMP
              NCTIT2(2,2)=1
            ELSEIF(JTEMP.LE.99)THEN
              WRITE(ITITL2(2,2)(1:2),'(I2)')JTEMP
              NCTIT2(2,2)=2
            ELSE
              WRITE(ITITL2(2,2)(1:3),'(I3)')JTEMP
              NCTIT2(2,2)=3
            ENDIF
          ENDIF
C
          IF(NUMCOL.GE.3)THEN
            ICNT2=2
            DO4215JJ=IFRST+1,ILAST
              ICNT2=ICNT2+1
              ITITL2(1,ICNT2)=' '
              NCTIT2(1,ICNT2)=0
              JTEMP=INT(XIDTE2(JJ)+0.5)
              IF(IGRPID.GT.0)THEN
                ILAST2=1
                DO4212II=10,1,-1
                  IF(IGRPLA(JJ,IGRPID)(II:II).NE.' ')THEN
                    LAST2=II
                    GOTO4213
                  ENDIF
 4212           CONTINUE
 4213           CONTINUE
                ITITL2(2,ICNT2)(1:ILAST2)=IGRPLA(JJ,IGRPID)(1:ILAST2)
                NCTIT2(2,ICNT2)=ILAST2
              ELSE
                IF(JTEMP.LE.9)THEN
                  WRITE(ITITL2(2,ICNT2)(1:1),'(I1)')JTEMP
                  NCTIT2(2,ICNT2)=1
                ELSEIF(JTEMP.LE.99)THEN
                  WRITE(ITITL2(2,ICNT2)(1:2),'(I2)')JTEMP
                  NCTIT2(2,ICNT2)=2
                ELSEIF(JTEMP.LE.999)THEN
                  WRITE(ITITL2(2,ICNT2)(1:3),'(I3)')JTEMP
                  NCTIT2(2,ICNT2)=3
                ELSE
                  WRITE(ITITL2(2,ICNT2)(1:4),'(I4)')JTEMP
                  NCTIT2(2,ICNT2)=4
                ENDIF
              ENDIF
 4215       CONTINUE
          ENDIF
C
          NMAX=0
          NTOT(1)=12
          NTOT(2)=10
          NTOT(3)=10
          NTOT(4)=10
          NTOT(5)=10
          NTOT(6)=10
          NTOT(7)=10
          DO4220J=1,NUMCOL
            VALIGN(J)='b'
            ALIGN(J)='r'
            IF(J.EQ.1)ALIGN(J)='c'
            NMAX=NMAX+NTOT(J)
            IDIGIT(J)=2
            IWHTML(J)=100
 4220     CONTINUE
          IDIGIT(1)=0
          IINC=1200
          IWRTF(1)=1400
          IWRTF(2)=IWRTF(1)+IINC
          IWRTF(3)=IWRTF(2)+IINC
          IWRTF(4)=IWRTF(3)+IINC
          IWRTF(5)=IWRTF(4)+IINC
          IWRTF(6)=IWRTF(5)+IINC
          IWRTF(7)=IWRTF(6)+IINC
          IFRSTL=.TRUE.
          ILASTL=.TRUE.
C
          ICNT3=0
          DO4230ISET1=1,NUMSE1
            ICNT3=ICNT3+1
            IF(ICNT3.GT.MAXROW)THEN
              ICNT3=MAXROW
              CALL DPDTA2(ITITL9,NCTIT9,
     1                    ITITLE,NCTITL,ITITL2,NCTIT2,
     1                    MAXLIN,NUMLIN,MAXCOL,NUMCOL,
     1                    ITEXT,NCTEXT,AMAT,MAXROW,ICNT3,
     1                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                    ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                    ISUBRO,IBUGA3,IERROR)
              ICNT3=1
            ENDIF
            ILAB=ISET1
            AMAT(ICNT3,1)=XIDTEM(ISET1)
            NSTRT=ILAB + (IFRST-1)*NUMSE1
            NSTOP=ILAST*NLAB
            ICNT2=1
            DO4235II=NSTRT,NSTOP,NUMSE1
              ICNT2=ICNT2+1
              AMAT(ICNT3,ICNT2)=H(II)
 4235       CONTINUE
 4230     CONTINUE
C
          IF(ICNT3.GT.0)THEN
            CALL DPDTA2(ITITL9,NCTIT9,
     1                  ITITLE,NCTITL,ITITL2,NCTIT2,
     1                  MAXLIN,NUMLIN,MAXCOL,NUMCOL,
     1                  ITEXT,NCTEXT,AMAT,MAXROW,ICNT3,
     1                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                  ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                  ISUBRO,IBUGA3,IERROR)
          ENDIF
C
          IRTFMD='OFF'
          IFLAGA=.TRUE.
          IFLAGB=.TRUE.
          NTOTAL=20
          NBLNK1=0
          NBLNK2=1
          ITYPE=2
          ITTEMP='Critical Value: '
          NCTEMP=16
          NUMDI2=2
          CALL DPDTXT(ITTEMP,NCTEMP,HCV,NUMDI2,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
C
 4210   CONTINUE
C
        ITITL9=' '
        NCTIT9=0
C
 4299   CONTINUE
C
C       TABLE 3
C
        IF(I691KC.EQ.'OFF')GOTO4399
C
C       NOTE: DETERMINE HOW MANY MATERIALS CAN REASONABLY BE PRINTED FOR
C             THE TABLE (I.E., DOES THE TABLE NEED TO BE PRINTED IN
C             MORE THAN 1 ITERATION).
C
C             BASE ON MAXIMUM OF 6 MATERIALS PER TABLE.
C
CCCCC   NITEMS=(ILPRCO - 11)/10
CCCCC   NIT=(NMAT/NITEMS) + 1
        NITEMS=6
        NIT=(NMAT/NITEMS) + 1
        IF(MOD(NMAT,NITEMS).EQ.0)NIT=NIT-1
C
        DO4310ITER=1,MAX(NIT,1)
C
          IFRST=(ITER-1)*NITEMS + 1
          ILAST=MIN(ITER*NITEMS,NMAT)
          NTEMP=ILAST-IFRST+1
          NUMCOL=NTEMP+1
C
          ITITLE(1:1)='k'
          NCTITL=1
          NUMLIN=2
          ITITL2(1,1)='Laboratory'
          ITITL2(2,1)='Number'
          ITITL2(1,2)='Material'
          NCTIT2(1,1)=10
          NCTIT2(2,1)=6
          NCTIT2(1,2)=8
          JTEMP=INT(XIDTE2(IFRST)+0.5)
          IF(IGRPID.GT.0)THEN
            ILAST2=1
            DO4302II=10,1,-1
              IF(IGRPLA(IFRST,IGRPID)(II:II).NE.' ')THEN
                 LAST2=II
                 GOTO4303
              ENDIF
 4302       CONTINUE
 4303       CONTINUE
            ITITL2(2,2)(1:ILAST2)=IGRPLA(IFRST,IGRPID)(1:ILAST2)
            NCTIT2(2,2)=ILAST2
          ELSE
            IF(JTEMP.LE.9)THEN
              WRITE(ITITL2(2,2)(1:1),'(I1)')JTEMP
              NCTIT2(2,2)=1
            ELSEIF(JTEMP.LE.99)THEN
              WRITE(ITITL2(2,2)(1:2),'(I2)')JTEMP
              NCTIT2(2,2)=2
            ELSE
              WRITE(ITITL2(2,2)(1:3),'(I3)')JTEMP
              NCTIT2(2,2)=3
            ENDIF
          ENDIF
C
          IF(NUMCOL.GE.3)THEN
            ICNT2=2
            DO4315JJ=IFRST+1,ILAST
              ICNT2=ICNT2+1
              ITITL2(1,ICNT2)=' '
              NCTIT2(1,ICNT2)=0
              JTEMP=INT(XIDTE2(JJ)+0.5)
              IF(IGRPID.GT.0)THEN
                ILAST2=1
                DO4312II=10,1,-1
                  IF(IGRPLA(JJ,IGRPID)(II:II).NE.' ')THEN
                    LAST2=II
                    GOTO4313
                  ENDIF
 4312           CONTINUE
 4313           CONTINUE
                ITITL2(2,ICNT2)(1:ILAST2)=IGRPLA(JJ,IGRPID)(1:ILAST2)
                NCTIT2(2,ICNT2)=ILAST2
              ELSE
                IF(JTEMP.LE.9)THEN
                  WRITE(ITITL2(2,ICNT2)(1:1),'(I1)')JTEMP
                  NCTIT2(2,ICNT2)=1
                ELSEIF(JTEMP.LE.99)THEN
                  WRITE(ITITL2(2,ICNT2)(1:2),'(I2)')JTEMP
                  NCTIT2(2,ICNT2)=2
                ELSEIF(JTEMP.LE.999)THEN
                  WRITE(ITITL2(2,ICNT2)(1:3),'(I3)')JTEMP
                  NCTIT2(2,ICNT2)=3
                ELSE
                  WRITE(ITITL2(2,ICNT2)(1:4),'(I4)')JTEMP
                  NCTIT2(2,ICNT2)=4
                ENDIF
              ENDIF
 4315       CONTINUE
          ENDIF
C
          NMAX=0
          NTOT(1)=12
          NTOT(2)=10
          NTOT(3)=10
          NTOT(4)=10
          NTOT(5)=10
          NTOT(6)=10
          NTOT(7)=10
          DO4320J=1,NUMCOL
            VALIGN(J)='b'
            ALIGN(J)='r'
            IF(J.EQ.1)ALIGN(J)='c'
            NMAX=NMAX+NTOT(J)
            IDIGIT(J)=2
            IWHTML(J)=100
 4320     CONTINUE
          IDIGIT(1)=0
          IINC=1200
          IWRTF(1)=1400
          IWRTF(2)=IWRTF(1)+IINC
          IWRTF(3)=IWRTF(2)+IINC
          IWRTF(4)=IWRTF(3)+IINC
          IWRTF(5)=IWRTF(4)+IINC
          IWRTF(6)=IWRTF(5)+IINC
          IWRTF(7)=IWRTF(6)+IINC
          IFRSTL=.TRUE.
          ILASTL=.TRUE.
C
          ICNT3=0
          DO4330ISET1=1,NUMSE1
            ICNT3=ICNT3+1
            IF(ICNT3.GT.MAXROW)THEN
              ICNT3=MAXROW
              CALL DPDTA2(ITITL9,NCTIT9,
     1                    ITITLE,NCTITL,ITITL2,NCTIT2,
     1                    MAXLIN,NUMLIN,MAXCOL,NUMCOL,
     1                    ITEXT,NCTEXT,AMAT,MAXROW,ICNT3,
     1                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                    ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                    ISUBRO,IBUGA3,IERROR)
              ICNT3=1
            ENDIF
            ILAB=ISET1
            AMAT(ICNT3,1)=XIDTEM(ISET1)
            NSTRT=ILAB + (IFRST-1)*NUMSE1
            NSTOP=ILAST*NLAB
            ICNT2=1
            DO4335II=NSTRT,NSTOP,NUMSE1
              ICNT2=ICNT2+1
              AMAT(ICNT3,ICNT2)=AK(II)
 4335       CONTINUE
 4330     CONTINUE
C
          IF(ICNT3.GT.0)THEN
            CALL DPDTA2(ITITL9,NCTIT9,
     1                  ITITLE,NCTITL,ITITL2,NCTIT2,
     1                  MAXLIN,NUMLIN,MAXCOL,NUMCOL,
     1                  ITEXT,NCTEXT,AMAT,MAXROW,ICNT3,
     1                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                  ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                  ISUBRO,IBUGA3,IERROR)
          ENDIF
C
          IRTFMD='OFF'
          IFLAGA=.TRUE.
          IFLAGB=.TRUE.
          NTOTAL=20
          NBLNK1=1
          NBLNK2=1
          ITYPE=2
          ITTEMP='Critical Value: '
          NCTEMP=16
          NUMDI2=2
          CALL DPDTXT(ITTEMP,NCTEMP,KCV,NUMDI2,NTOTAL,
     1                NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,
     1                ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
C
 4310   CONTINUE
C
        ITITL9=' '
        NCTIT9=0
C
 4399   CONTINUE
C
C       PRINT TABLE 4
C
        IF(I691PS.EQ.'OFF')GOTO4499
C
        ITITLE='Precision Statistics'
        NCTITL=20
        NUMLIN=1
        NUMCOL=7
        ITITL2(1,1)='Material'
        ITITL2(1,2)='Xbar'
        ITITL2(1,3)='s(x)'
        ITITL2(1,4)='s(r)'
        ITITL2(1,5)='s(R)'
        ITITL2(1,6)='r'
        ITITL2(1,7)='R'
        NCTIT2(1,1)=8
        NCTIT2(1,2)=4
        NCTIT2(1,3)=4
        NCTIT2(1,4)=4
        NCTIT2(1,5)=4
        NCTIT2(1,6)=1
        NCTIT2(1,7)=1
        NMAX=0
        NTOT(1)=10
        NTOT(2)=16
        NTOT(3)=16
        NTOT(4)=16
        NTOT(5)=16
        NTOT(6)=10
        NTOT(7)=10
        DO4420J=1,NUMCOL
          IF(IGRPID.GT.0)THEN
            IF(J.GT.1)IDIGIT(J-1)=NUMDIG
            ALIGN(J)='r'
          ELSE
            IDIGIT(J)=NUMDIG
            ALIGN(J)='r'
            IF(J.EQ.1)ALIGN(J)='c'
          ENDIF
          VALIGN(J)='b'
          IWHTML(J)=100
          NMAX=NMAX+NTOT(J)
 4420   CONTINUE
        IF(IGRPID.GT.0)THEN
          IDIGIT(5)=2
          IDIGIT(6)=2
        ELSE
          IDIGIT(1)=0
          IDIGIT(6)=2
          IDIGIT(7)=2
        ENDIF
        IWRTF(1)=1000
        IWRTF(2)=IWRTF(1)+1400
        IWRTF(3)=IWRTF(2)+1400
        IWRTF(4)=IWRTF(3)+1400
        IWRTF(5)=IWRTF(4)+1400
        IWRTF(6)=IWRTF(5)+1000
        IWRTF(7)=IWRTF(6)+1000
        IFRSTL=.TRUE.
        ILASTL=.TRUE.
        NUMCOT=NUMCOL
        IF(IGRPID.GT.0)NUMCOT=NUMCOL-1
C
        ICNT3=0
        DO4430ISET2=1,NUMSE2
          ICNT=ICNT+1
          ICNT3=ICNT3+1
          IF(ICNT3.GT.MAXROW)THEN
            ICNT3=MAXROW
            CALL DPDTA2(ITITL9,NCTIT9,
     1                  ITITLE,NCTITL,ITITL2,NCTIT2,
     1                  MAXLIN,NUMLIN,MAXCOL,NUMCOT,
     1                  ITEXT,NCTEXT,AMAT,MAXROW,ICNT3,
     1                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                  ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                  ISUBRO,IBUGA3,IERROR)
            ICNT3=1
          ENDIF
          AR1=2.8*SRPT(ISET2)
          AR2=2.8*SRPRD(ISET2)
          IF(IGRPID.GT.0)THEN
            ILAST=1
            DO4402II=10,1,-1
              IF(IGRPLA(ISET2,IGRPID)(II:II).NE.' ')THEN
                ILAST=II
                GOTO4403
              ENDIF
 4402       CONTINUE
 4403       CONTINUE
            ITEXT(ICNT3)(1:ILAST)=IGRPLA(ISET2,IGRPID)(1:ILAST)
            NCTEXT(ICNT3)=ILAST
            ICNT4=0
          ELSE
            ICNT4=1
            AMAT(ICNT3,ICNT4)=XIDTE2(ISET2)
          ENDIF
          ICNT4=ICNT4+1
          AMAT(ICNT3,ICNT4)=XBAR(ISET2)
          ICNT4=ICNT4+1
          AMAT(ICNT3,ICNT4)=SDXBRI(ISET2)
          ICNT4=ICNT4+1
          AMAT(ICNT3,ICNT4)=SRPT(ISET2)
          ICNT4=ICNT4+1
          AMAT(ICNT3,ICNT4)=SRPRD(ISET2)
          ICNT4=ICNT4+1
          AMAT(ICNT3,ICNT4)=AR1
          ICNT4=ICNT4+1
          AMAT(ICNT3,ICNT4)=AR2
 4430   CONTINUE
C
        IF(ICNT3.GT.0)THEN
          CALL DPDTA2(ITITL9,NCTIT9,
     1                ITITLE,NCTITL,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,MAXCOL,NUMCOT,
     1                ITEXT,NCTEXT,AMAT,MAXROW,ICNT3,
     1                IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                ICAPSW,ICAPTY,IFRSTL,ILASTL,
     1                ISUBRO,IBUGA3,IERROR)
        ENDIF
C
 4499   CONTINUE
C
      ENDIF
C
      IF(IFEEDB.EQ.'OFF')GOTO8099
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'HTML')GOTO8099
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'LATE')GOTO8099
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'RTF')GOTO8099
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8005)
 8005 FORMAT('THE FOLLOWING VARIABLES WERE WRITTEN TO THE FILE ',
     1       'dpst1f.dat:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8011)
 8011 FORMAT('   1. MATERIAL ID')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8013)
 8013 FORMAT('   2. LAB ID')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8021)
 8021 FORMAT('   3. CELL AVERAGE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8023)
 8023 FORMAT('   4. CELL STANDARD DEVIATION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8025)
 8025 FORMAT('   5. CELL AVERAGE - OVERALL AVERAGE FOR MATERIAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8027)
 8027 FORMAT('   6. H-CONSISTENCY STATISTIC')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8029)
 8029 FORMAT('   7. VARIANCE OF MEAN OF LAB')
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8041)
 8041 FORMAT('THE H-CONSISTECNY STATISTICS WERE WRITTEN TO THE FILE ',
     1       'dpst2f.dat:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8043)
 8043 FORMAT('   THE ROWS REPRESENT THE LAB AND THE COLUMNS REPRESENT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8045)
 8045 FORMAT('   THE MATERIALS.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8051)
 8051 FORMAT('THE K-CONSISTECNY STATISTICS WERE WRITTEN TO THE FILE ',
     1       'dpst3f.dat:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8053)
 8053 FORMAT('   THE ROWS REPRESENT THE LAB AND THE COLUMNS REPRESENT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8055)
 8055 FORMAT('   THE MATERIALS.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,8071)
 8071 FORMAT('THE FOLLOWING VARIABLES WERE WRITTEN TO THE FILE ',
     1       'dpst4f.dat:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8073)
 8073 FORMAT('   1. MATERIAL ID')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8075)
 8075 FORMAT('   2. MEAN OF THE CELL AVERAGES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8077)
 8077 FORMAT('   3. STANDARD DEVIATION OF THE CELL AVERAGES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8079)
 8079 FORMAT('   4. REPEATABILITY STANDARD DEVIATION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8081)
 8081 FORMAT('   5. REPRODUCIBILITY STANDARD DEVIATION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8083)
 8083 FORMAT('   6. 95% REPEATABILITY STANDARD DEVIATION LIMIT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8085)
 8085 FORMAT('   7. 95% REPRODUCIBILITY STANDARD DEVIATION LIMIT')
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8091)
 8091 FORMAT('   H- AND K-CONSISTENCY STATISTIC CRITICAL VALUES ',
     1       'SAVED AS INTERNAL PARAMETERS HCV AND KCV.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
 8099 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EIN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEIN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,IBUGA3
 9012   FORMAT('IERROR,IBUGA3 = ',A4,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPELL2(X1,Y1,X2,Y2,X3,Y3,PX,PY,
     1                  IFIG,ILINPA,ILINCO,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,PREBTH,
     1                  IREFSW,IREFCO,
     1                  IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
C     PURPOSE--DRAW A ELLIPSE WITH ONE END OF THE MAJOR AXIS AT (X1,Y1)
C              WITH ONE END OF THE MINOR AXIS AT (X2,Y2) AND THE OTHER
C              END OF MAJOR AXIS AT (X3,Y3).
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
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
C     UPDATED         --FEBRUARY  1994. ARRAY TO GARBAGE COMMON
C     UPDATED         --JULY      2019. MOVE CREATION OF SCRATCH
C                                       STORAGE FROM DPELL2 TO DPELLI
C
C-----NON-COMMON VARIABLES-------------------------------------
C
      DIMENSION PX(*)
      DIMENSION PY(*)
C
      CHARACTER*4 IPATT2
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
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ELL2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPELL2--')
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
        WRITE(ICOUT,69)PTEXHE,PTEXWI,PTEXVG,PTEXHG
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  DETERMINE THE COORDINATES  **
C               **  FOR THE ELLIPSE            **
C               *********************************
C
      DELX=X3-X1
      DELY=Y3-Y1
      ALEN=0.0
      TERM=DELX**2+DELY**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      A=ALEN/2.0
C
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
C
      XCENT=(X1+X3)/2.0
      YCENT=(Y1+Y3)/2.0
C
      DELX2=X2-XCENT
      DELY2=Y2-YCENT
      ALEN=0.0
      TERM=DELX2**2+DELY2**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      B=ALEN
C
      K=0
C
      X=0.0
      Y=0.0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      DO3010I=181,541,5
        IREV=541-I+181
        PHI2=IREV-1
        PHI2=PHI2*(2.0*3.1415926)/360.0
        X=A*COS(PHI2)+A
        Y=B*SIN(PHI2)
        CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
        K=K+1
        PX(K)=XP
        PY(K)=YP
 3010 CONTINUE
C
      NP=K
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
     1            IFIG,IPATT,PTHICK,PXGAP,PYGAP,ICOLF,ICOLP,IPATT2)
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
      CALL DPDRPL(PX,PY,NP,
     1            IFIG,IPATT,PTHICK,ICOL,
     1            JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ELL2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPELL2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)DELX,DELY,DELX2,DELY2
 9012   FORMAT('DELX,DELY,DELX2,DELY2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)XCENT,YCENT,A,B,THETA
 9013   FORMAT('XCENT,YCENT,A,B,THETA = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NP,IERRG4
 9014   FORMAT('NP,IERRG4 = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPELLI(IHARG,IARGT,ARG,NUMARG,
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
C     PURPOSE--DRAW ONE OR MORE ELLIPSES (DEPENDING ON HOW MANY NUMBERS
C              ARE PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
C              OF 0 TO 100.
C     NOTE--THE INPUT COORDINATES DEFINE 3 SUCCESSIVE POINTS
C           AROUND THE ELLIPSE--AT THE ENDS OF AXES.
C     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 3
C          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*3 = 6.
C     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN ELLIPSE WILL GOC
C           FROM THE LAST CURSOR POSITION (ASSUMED TO BE AT ONE END OF
C           MAJOR AXIS) THROUGH THE (X,Y) POINT (EITHER ABSOLUTE OR
C           RELATIVE) AS DEFINED BY THE FIRST AND SECOND NUMBERS
C           (ASSUMED TO BE AT ONE END OF MINOR AXIS), TO THE (X,Y) POINT
C           (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE THIRD AND
C           FOURTH NUMBERS (ASSUMED TO BE AT THE OTHER END OF MAJOR
C           AXIS), AND THEN BACK TO THE OTHER END OF THE MINOR AXIS,
C           AND CONTINUING BACK THE START POINT TO CLOSE THE ELLIPSE.
C     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN ELLIPSE WILL GO
C           FROM THE ABSOLUTE (X,Y) POSITION AS RESULTING FORM THE FIRST
C           AND SECOND NUMBERS (ASSUMED TO BE AT ONE END OF MAJOR AXIS),
C           THROUGH THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS
C           DEFINED BY THE THIRD AND FOURTH NUMBERS (ASSUMED TO BE AT
C           ONE END OF MINOR AXIS), TO THE (X,Y) POINT (EITHER ABSOLUTE
C           OR RELATIVE) AS DEFINED BY THE FIFTH AND SIXTH NUMBERS
C           (ASSUMED TO BE AT THE OTHER END OF MAJOR AXIS), AND THEN
C           BACK TO THE OTHER END OF THE MINOR AXIS, AND CONTINUING BACK
C           THE START POINT TO CLOSE THE ELLIPSE.
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
C                 PHONE--301-921-3651
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
C     UPDATED         --JULY      2019. MOVE CREATION OF SCRATCH STORAGE
C                                       TO DPELLI FROM DPELL2
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
      CHARACTER*4 IDFONT
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
      DIMENSION PX(MAXOBV)
      DIMENSION PY(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ELLI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPELLI--')
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
      IFIG='ELLI'
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
          IF(IARGT(I).EQ.'NUMB')GOTO1120
          GOTO1130
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
        J=J+1
        IF(J.GT.NUMARG)GOTO1190
        X3=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X3,X3,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')X3=X2+X3
        J=J+1
        IF(J.GT.NUMARG)GOTO1190
        Y3=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y3,Y3,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')Y3=Y2+Y3
C
        CALL DPELL2(X1,Y1,X2,Y2,X3,Y3,PX,PY,
     1              IFIG,ILINPA,ILINCO,PLINTH,
     1              AREGBA,IREBLI,IREBCO,PREBTH,
     1              IREFSW,IREFCO,
     1              IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1              PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
        X1=X3
        Y1=Y3
C
        GOTO1160
 1190   CONTINUE
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
 1131 FORMAT('***** ERROR IN ELLIPSE (DPELLI)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR THE ELLIPSE COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW AN ELLIPSE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      WITH ONE END OF MAJOR AXIS AT THE POINT 20 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      ONE END OF THE MINOR AXIS AT THE POINT 30 10')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1138)
 1138 FORMAT('      AND WITH THE OTHER END OF THE MAJOR AXIS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1139)
 1139 FORMAT('      AT THE POINT 40 20')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN THE ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      ELLIPSE 20 20 30 10 40 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      ELLIPSE ABSOLUTE 20 20 30 10 40 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('      ELLIPSE RELATIVE 20 20 30 10 40 20 ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ELLI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPELLI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ILOCFN,NUMNUM
 9012   FORMAT('ILOCFN,NUMNUM = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)X1,Y1,X2,Y2,X3,Y3
 9013   FORMAT('X1,Y1,X2,Y2,X3,Y3 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)PXSTAR,PYSTAR,PXEND,PYEND
 9015   FORMAT('PXSTAR,PYSTAR,PXEND,PYEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9027)IFOUND,IERRG4,IERROR
 9027   FORMAT('IFOUND,IERRG4,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEMBE(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,ICOM,IERASW,
     1                  IEMBSW,IEMCNT,PEMXC1,PEMXC2,PEMYC1,PEMYC2,
     1                  PWXMIN,PWXMAX,PWYMIN,PWYMAX,
     1                  IBUGP2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FOLLOWING EMBED COMMANDS:
C
C                  EMBED <ON/OFF>
C                  EMBED CORNER COORDINATES <xlow> <ylow> <xhigh> <yhigh>
C
C              ALTHOUGH EMBEDDED PLOTS ARE RELATED TO MULTI-PLOTS, THEY
C              ARE DIFFERENT.  WHERE THE MULTI-PLOT DEFINES ROWS/COLUMNS
C              FOR PLOTS, EMBEDED PLOTS WORK AS FOLLOWS:
C
C                  1) THE FIRST PLOT AFTER THE "EMBED ON" DEFINES A
C                     COORDINATE SYSTEM.
C
C                  2) FOR EACH SUBSEQUENT PLOT, THE
C                     "EMBED CORNER COORDINATES" COMMAND IS USED TO
C                     DEFINE A SUB-PLOT REGION RELATIVE TO THE FIRST
C                     COORDINATE SYSTEM (THE COORDINATES ARE
C                     SPECIFIED IN UNITS OF THIS FIRST COORDINATE
C                     SYSTEM).
C
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --IARG   (AN INTEGER VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS/-IEMNSW = ON-OFF EMBED SWITCH
C                     --PEMXC1 = LOWER X-COORDINATE FOR EMBEDDED REGION
C                     --PEMYC1 = LOWER Y-COORDINATE FOR EMBEDDED REGION
C                     --PEMXC2 = UPPER X-COORDINATE FOR EMBEDDED REGION
C                     --PEMYC2 = UPPER Y-COORDINATE FOR EMBEDDED REGION
C                     --PEMYC2 = UPPER Y-COORDINATE FOR EMBEDDED REGION
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     NOTE--EMBED IS USED IN DPGRAP
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/8
C     ORIGINAL VERSION--AUGUST    2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IEMBSW
      CHARACTER*4 IERASW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICOM
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IHARG2(*)
      CHARACTER*4 IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
C
      COMMON/IEMBZ/FX1MNE,FX1MXE,FY1MNE,FY1MXE,
     1             PXMINE,PXMAXE,PYMINE,PYMAXE
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEM'
      ISUBN2='BE  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(ISUBRO.EQ.'EMBE' .OR. IBUGP2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEMBE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGP2,ISUBRO,IFOUND,IERROR,IERASW,IEMBSW,NUMARG
   53   FORMAT('IBUGP2,ISUBRO,IFOUND,IERROR,IERASW,IEMBSW,NUMARG = ',
     1         6(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I),ARG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,84)PEMXC1,PEMXC2,PEMYC1,PEMYC2
   84   FORMAT('PEMXC1,PEMXC2,PEMYC1,PEMYC2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************
C               **  TREAT THE    EMBED               CASE  **
C               *********************************************
C
C               *************************************************
C               **  STEP 1--                                   **
C               **  BRANCH TO THE VARIOUS CASES                **
C               *************************************************
C
      IF(NUMARG.LE.0)GOTO9000
C
      IF(ICOM.EQ.'EMBE')THEN
        IF(NUMARG.GE.2 .AND. IHARG(1).EQ.'CORN' .AND.
     1         IHARG(2).EQ.'COOR')THEN
          IFOUND='YES'
          IF(IARGT(3).EQ.'NUMB')THEN
            PEMXC1=ARG(3)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1201)
 1201       FORMAT('THE ARGUMENT FOR THE LOWER HORIZONTAL CORNER ',
     1             'COORDINATE  OF THE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1202)
 1202       FORMAT('EMBED CORNER COORDINATES  COMMAND IS NOT NUMERIC.')
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(IARGT(4).EQ.'NUMB')THEN
            PEMYC1=ARG(4)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1211)
 1211       FORMAT('THE ARGUMENT FOR THE LOWER VERTICAL CORNER ',
     1             'COORDINATE  OF THE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1202)
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(IARGT(5).EQ.'NUMB')THEN
            PEMXC2=ARG(5)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1221)
 1221       FORMAT('THE ARGUMENT FOR THE UPPER HORIZONTAL CORNER ',
     1             'COORDINATE  OF THE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1202)
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(IARGT(6).EQ.'NUMB')THEN
            PEMYC2=ARG(6)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1231)
 1231       FORMAT('THE ARGUMENT FOR THE UPPER VERTICAL CORNER ',
     1             'COORDINATE  OF THE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1202)
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1241)PEMXC1
 1241       FORMAT('THE LOWER HORIZONTAL CORNER COORDINATE IS ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1242)PEMYC1
 1242       FORMAT('THE LOWER VERTICAL CORNER COORDINATE IS   ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1243)PEMXC2
 1243       FORMAT('THE UPPER HORIZONTAL CORNER COORDINATE IS ',G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1244)PEMYC2
 1244       FORMAT('THE UPPER VERTICAL CORNER COORDINATE IS   ',G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'ON  ' .OR.
     1       IHARG(1).EQ.'YES')THEN
            IFOUND='YES'
            IEMBSW='ON'
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1401)
 1401         FORMAT('THE EMBED PLOT SWITCH HAS BEEN TURNED ON.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSEIF(NUMARG.EQ.1 .AND. IHARG(1).EQ.'OFF ' .OR.
     1           IHARG(1).EQ.'NO  ' .OR. IHARG(1).EQ.'DEFA')THEN
            IFOUND='YES'
C
C           RESET SOME PARAMETERS TO THEIR DEFAULT SETTINGS
C
            IEMBSW='OFF'
            IERASW='ON'
            FX1MNE=CPUMIN
            FX1MXE=CPUMIN
            FY1MNE=CPUMIN
            FY1MXE=CPUMIN
            PXMINE=CPUMIN
            PXMAXE=CPUMIN
            PYMINE=CPUMIN
            PYMAXE=CPUMIN
            PWXMIN=0.0
            PWXMAX=100.0
            PWYMIN=0.0
            PWYMAX=100.0
C
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1403)
 1403         FORMAT('THE EMBED PLOT SWITCH HAS BEEN TURNED OFF.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(ISUBRO.EQ.'EMBE' .OR. IBUGP2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEMBE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR,IEMBSW,IEMCNT
 9013   FORMAT('IFOUND,IERROR,IEMBSW,IEMCNT = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)PEMXC1,PEMXC2,PEMYC1,PEMYC2
 9022   FORMAT('PEMXC1,PEMXC2,PEMYC1,PEMYC2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPENCB(Y,X,N,MAXNXT,
     1                  TEMP1,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,
     1                  AY2,AX2,NOUT,AREA,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--GIVEN A SET OF (X,Y) PAIRS, RETURN THE 4 POINTS
C              THAT DEFINE THE MINIMUM AREA ENCLOSING RECTANGLE.
C              ALSO RETURN THAT AREA.
C     INPUT  ARGUMENTS--X      = A REAL VECTOR CONTAINING THE X
C                                COORDINATES OF THE POINTS
C                     --Y      = A REAL VECTOR CONTAINING THE Y
C                                COORDINATES OF THE POINTS
C                     --N      = NUMBER OF POINTS IN X, Y
C     OUTPUT ARGUMENTS--X2     = A REAL VECTOR CONTAINING THE X
C                                COORDINATES OF THE ENCLOSING RECTANGLE
C                     --Y2     = A REAL VECTOR CONTAINING THE Y
C                                COORDINATES OF THE ENCLOSING RECTANGLE
C                     --NOUT   = NUMBER OF POINTS IN X2, Y2
C                     --AREA   = THE MINIMUM AREA
C     REFERENCE--ALLISON AND NOGA, "ON THE COMPUTATION OF MINIMUM ENCASING
C                RECTANGLES AND SET DIAMETERS", CS81017-R, DEPARTMENT OF
C                COMPUTER SCIENCE, VIRGINIA POLYTECHNIC INSTITUTE AND
C                STATE UNIVERSITY, BLACKSBURG, VA 24061.
C     REFERENCE--ARNON AND GIESELMANN (1983), "A LINEAR TIME ALGORITHM
C                FOR THE MINIMUM AREA RECTANGLE ENCLOSING A CONVEX
C                POLYGON", REPORT NUMBER 83-463, COMPUTER SCIENCE
C                TECHNICAL REPORTS, PURDUE UNIVERSITY.
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
      REAL AX2(*)
      REAL AY2(*)
      REAL TEMP1(*)
CCCCC REAL PI
      REAL AREA
CCCCC REAL DELTAT
CCCCC REAL AREAT
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER MAXNXT
      INTEGER N
      INTEGER NHULL
      INTEGER NOUT
      INTEGER I
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IWRITE
C
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA PI /3.1415926535/
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ENCB')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPENCB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
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
      IHIGHR=0
C
C     STEP 1: FIND THE CONVEX HULL
C
      IWRITE='OFF'
      CALL DP2DCH(Y,X,TEMP1,N,IWRITE,MAXNXT,
     1            AY2,AX2,NHULL,
     1            ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,
     1            IBUGA3,IERROR)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ENCB')THEN
        WRITE(ICOUT,91)NHULL
   91   FORMAT('AFTER CONVEX HULL, NHULL = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO93I=1,NHULL
          WRITE(ICOUT,95)I,AX2(I),AY2(I)
   95     FORMAT('I,AX2(I),AY2(I) = ',I8,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
   93   CONTINUE
      ENDIF
C
C     IMPLEMENT "HIGHPOINT" STRATEGY OF ALLISON AND NOGA.
C
C     STEP 1: START WITH VERTEX 1 AND THE EDGE BETWEEN VERTEX 1 AND
C             VERTEX 2 AND FIND THE HIGHPOINTS.
C
      XI=AX2(1)
      YI=AY2(1)
      XJ=AX2(2)
      YJ=AY2(2)
      DO310I=3,NHULL
        XA1=AX2(I-1)
        YA1=AY2(I-1)
        SA=XA1*(YI-YJ) + YA1*(XJ-XI) + YJ*XI - YI*XJ
        XA2=AX2(I)
        YA2=AY2(I)
        SB=XA2*(YI-YJ) + YA2*(XJ-XI) + YJ*XI - YI*XJ
        IF(SA.EQ.SB)THEN
          IHIGHL=I
          IHIGHR=I-1
          GOTO319
        ELSEIF(SB.LT.SA)THEN
          IHIGHL=-1
          IHIGHR=I-1
          GOTO319
        ENDIF
  310 CONTINUE
  319 CONTINUE
      IVA=IHIGHR
C
C     STEP 2: A) FIND LINE PERPINDICULAR TO EDGE AND CONTAINING THE
C                HIGHPOINT.
C             B) FIND THE POINT WHERE THAT LINE INTERSECTS THE EDGE.
C
      CALL DPPLIN(XI,YI,XJ,YJ,XA1,YA1,
     1            XA2,YA2,S1,S2,DIST1,
     1            ISUBRO,IBUGA3)
      CALL INTLI2(XI,YI,XJ,YJ,XA1,YA1,XA2,YA2,
     1             XC,YC,
     1             IBUGA3,ISUBRO,IERROR)
C
C     STEP 3: NOW FIND THE HIGHPOINT FROM (XA1,YA1), (XC,YC)
C             BETWEEN THE INITIAL EDGE AND IHIGHR.  WE CAN
C             THEN DETERMINE TWO OF THE VERTICES FOR THE
C             ENCLOSING RECTANGLE.
C
      X1=XC
      Y1=YC
      X2=XA1
      Y2=YA1
      DO410I=3,IVA
        XD1=AX2(I-1)
        YD1=AY2(I-1)
        SA=XD1*(Y1-Y2) + YD1*(X2-X1) + Y2*X1 - Y1*X2
        XD2=AX2(I)
        YD2=AY2(I)
        SB=XD2*(Y1-Y2) + YD2*(X2-X1) + Y2*X1 - Y1*X2
        IF(SA.EQ.SB)THEN
          IHIGHL=I
          IHIGHR=I-1
          GOTO419
        ELSEIF(SB.LT.SA)THEN
          IHIGHL=-1
          IHIGHR=I-1
          GOTO419
        ENDIF
  410 CONTINUE
  419 CONTINUE
      IVD=IHIGHR
C
C     FIND TWO POINTS FOR THE PERPINDICULAR LINE.
C
      CALL DPPLIN(XC,YC,XA1,YA1,XD1,YD1,
     1            XD2,YD2,S3,S4,DIST2,
     1            ISUBRO,IBUGA3)
C
C     NOW FIND LINE PARALLEL TO (XA1,YA1), (XC,YC) THAT
C     CONTAINS (XD1,YD1).  FOR CURRENT PURPOSE, JUST NEED
C     ANY ARBITRARY SECOND POINT ON THE LINE.
C
C     ALSO FIND SECOND POINT FOR LINE PARALLEL TO (XI,YI),
C     (XJ,YJ) THAT CONTAINS (XA1,YA1).
C
      CALL PARALI(XA1,YA1,XC,YC,XD1,YD1,
     1            XD3,YD3,
     1            IBUGA3,ISUBRO,IERROR)
      CALL PARALI(XI,YI,XJ,YJ,XA1,YA1,
     1            XA2,YA2,
     1            IBUGA3,ISUBRO,IERROR)
C
C     DETERMINE VERTICES OF ENCLOSING RECTANGLE BASED
C     LINE INTERSECTIONS.
C
      CALL INTLI2(XI,YI,XJ,YJ,XD1,YD1,XD3,YD3,
     1             ZX1,ZY1,
     1             IBUGA3,ISUBRO,IERROR)
      CALL INTLI2(XA1,YA1,XA2,YA2,XD1,YD1,XD3,YD3,
     1             ZX2,ZY2,
     1             IBUGA3,ISUBRO,IERROR)
C
C     STEP 4: NOW FIND THE HIGHPOINT FROM (XA1,YA1), (XC,YC)
C             BETWEEN (XA1,YA1) AND THE LAST POINT.  WE CAN
C             THEN DETERMINE THE OTHER TWO VERTICES FOR THE
C             ENCLOSING RECTANGLE.
C
      X1=XC
      Y1=YC
      X2=XA1
      Y2=YA1
      DO510I=IVA+1,NHULL
        XE1=AX2(I-1)
        YE1=AY2(I-1)
        SA=XE1*(Y1-Y2) + YE1*(X2-X1) + Y2*X1 - Y1*X2
        XE2=AX2(I)
        YE2=AY2(I)
        SB=XE2*(Y1-Y2) + YE2*(X2-X1) + Y2*X1 - Y1*X2
        IF(SA.EQ.SB)THEN
          IHIGHL=I
          IHIGHR=I-1
          GOTO519
        ELSEIF(SB.LT.SA)THEN
          IHIGHL=-1
          IHIGHR=I-1
          GOTO519
        ENDIF
  510 CONTINUE
  519 CONTINUE
      IVE=IHIGHR
C
C     FIND SECOND POINT FOR LINE PERPINDICULAR TO
C     (XC,YC), (XA1,YA1) THAT CONTAINS (XE1,YE1).
C
      CALL DPPLIN(XC,YC,XA1,YA1,XE1,YE1,
     1            XE2,YE2,S5,S6,DIST3,
     1            ISUBRO,IBUGA3)
C
C     FIND LINE PARALLEL TO (XC,YC), (XA1,YA1) THAT
C     CONTAINS (XE1,YE1).
C
      CALL PARALI(XC,YC,XA1,YA1,XE1,YE1,
     1            XE3,YE3,
     1            IBUGA3,ISUBRO,IERROR)
C
C     DETERMINE VERTICES OF ENCLOSING RECTANGLE BASED
C     LINE INTERSECTIONS.
C
      CALL INTLI2(XA1,YA1,XA2,YA3,XE1,YE1,XE3,YE3,
     1             ZX3,ZY3,
     1             IBUGA3,ISUBRO,IERROR)
      CALL INTLI2(XI,YI,XJ,YJ,XE1,YE1,XE3,YE3,
     1             ZX4,ZY4,
     1             IBUGA3,ISUBRO,IERROR)
C
      AY2(1)=ZY1
      AX2(1)=ZX1
      AY2(2)=ZY2
      AX2(2)=ZY2
      AY2(3)=ZY3
      AX2(3)=ZX3
      AY2(4)=ZY4
      AX2(4)=ZX4
      NOUT=4
C
C     HAVEN'T BEEN ABLE TO GET THIS ALGORITHM WORKING.
C     COMMENT OUT FOR NOW.
C
C     STEP 2: INITIALIZATION
C
CCCCC ALPHA=0.0
CCCCC J=2
CCCCC K=2
CCCCC M=2
CCCCC BETA=ANGRAD(X(1),Y(1),X(2),Y(2),X(3),Y(3),IBUGA3)
CCCCC print *,'beta = ',beta
CCCCC GAMMA=BETA
CCCCC DELTA=BETA
CCCCC AREA=CPUMAX
C
C     STEP 3: LOOP THROUGH VERTICES OF CONVEX POLYGON
C
CCCCC DO300I=1,NHULL
C
C       STEP 3A: FIND ANGLE OF ROTATION OF NEXT EDGE OF THE POLYGON
C
CCCCC   IM1=I-1
CCCCC   IP1=I+1
CCCCC   IF(I.GT.1)THEN
CCCCC     ALPHAT=ANGRAD(X(IM1),Y(IM1),X(I),Y(I),X(IP1),Y(IP1),IBUGA3)
CCCCC     ALPHA=ALPHA + ALPHAT
CCCCC   ENDIF
CCCCC   print *,'i,alphat,alpha = ',i,alphat,alpha
C
C       STEP 3B: FIND A VERTEX ON THE FIRST PERPINDICULAR LINE OF
C                SUPPORT
C
CCCCC   IF(BETA.GE.ALPHA + (PI/2.0))GOTO319
CC310   CONTINUE
CCCCC     J=J+1
CCCCC     JM1=J-1
CCCCC     JP1=J+1
CCCCC     BETAT=ANGRAD(X(JM1),Y(JM1),X(J),Y(J),X(JP1),Y(JP1),IBUGA3)
CCCCC     BETA=BETA + BETAT
CCCCC     print *,'j,betat,beta = ',j,betat,beta
CCCCC     IF(BETA.GE.ALPHA + (PI/2.0))GOTO319
CCCCC     GOTO310
CC319   CONTINUE
C
C       STEP 3C: FIND A VERTEX ON A PARALLEL LINE OF SUPPORT
C
CCCCC   IF(GAMMA.GE.ALPHA + PI)GOTO329
CC320   CONTINUE
CCCCC     K=K+1
CCCCC     KM1=K-1
CCCCC     KP1=K+1
CCCCC     GAMMAT=ANGRAD(X(KM1),Y(KM1),X(K),Y(K),X(KP1),Y(KP1),IBUGA3)
CCCCC     GAMMA=GAMMA + GAMMAT
CCCCC     print *,'k,gammat,gamma = ',k,gammat,gamma
CCCCC     IF(GAMMA.GE.ALPHA + PI)GOTO329
CCCCC     GOTO320
CC329   CONTINUE
C
C       STEP 3D: FIND A VERTEX ON THE SECOND PERPINDICULAR LINE OF
C                SUPPORT
C
CCCCC   IF(DELTA.GE.ALPHA + (3.0*PI/2.0))GOTO339
CC330   CONTINUE
CCCCC     M=M+1
CCCCC     MM1=M-1
CCCCC     MP1=M+1
CCCCC     DELTAT=ANGRAD(X(MM1),Y(MM1),X(M),Y(M),X(MP1),Y(MP1),IBUGA3)
CCCCC     DELTA=DELTA + DELTAT
CCCCC     print *,'m,deltat,delta = ',m,deltat,delta
CCCCC     IF(DELTA.GE.ALPHA + (3.0*PI/2.0))GOTO339
CCCCC     GOTO330
CC339   CONTINUE
C
C       STEP 3E: FIND DISTANCES BETWEEN PARALLEL AND PERPINDICULAR
C                LINES OF SUPPORT
C
CCCCC   IF(X(I+1).EQ.X(I))THEN
CCCCC     D1=ABS(X(K) - X(I))
CCCCC     D2=ABS(Y(M) - Y(J))
CCCCC   ELSEIF(Y(I+1).EQ.Y(I))THEN
CCCCC     D1=ABS(Y(K) - Y(I))
CCCCC     D2=ABS(X(M) - X(J))
CCCCC   ELSE
CCCCC     SLOPE=(Y(I+1) - Y(I))/(X(I+1) - X(I))
CCCCC     SLOPE2=-1.0/SLOPE
CCCCC     D1=DPNTLI(X(I),Y(I),X(K),Y(K),SLOPE,IBUGA3)
CCCCC     D2=DPNTLI(X(J),Y(J),X(M),Y(M),SLOPE2,IBUGA3)
CCCCC   ENDIF
C
C       STEP 3F: COMPUTE THE AREA
C
CCCCC   AREAT=D1*D2
CCCCC   IF(I.EQ.1 .OR. AREAT.LT.AREA)THEN
CCCCC     AREA=AREAT
CCCCC     IEDGE=I
CCCCC     ISAVE=I
CCCCC     JSAVE=J
CCCCC     KSAVE=K
CCCCC     MSAVE=M
CCCCC   ENDIF
C
CCCCC IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ENCB')THEN
CCCCC   WRITE(ICOUT,391)I,J,K,M
C 391   FORMAT('FROM MIDDLE OF DPENCB: I,J,K,M = ',4I8)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,392)ALPHA,BETA,GAMMA,DELTA
C 392   FORMAT('ALPHA,BETA,GAMMA,DELTA = ',4G15.7)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,393)SLOPE,D1,D2,AREA
C 393   FORMAT('SLOPE,D1,D2,AREA = ',4G15.7)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC ENDIF
C
C 300 CONTINUE
C
C     STEP 4: SAVE THE VERTICES OF THE ENCLOSING BOX
C
CCCCC Y2(1)=Y(I)
CCCCC X2(1)=X(I)
CCCCC Y2(2)=Y(J)
CCCCC X2(2)=X(J)
CCCCC Y2(3)=Y(K)
CCCCC X2(3)=X(K)
CCCCC Y2(4)=Y(M)
CCCCC X2(4)=X(M)
CCCCC NOUT=4
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ENCB')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DPENCB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9052)AREA
 9052   FORMAT('AREA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        DO9055I=1,NOUT
          WRITE(ICOUT,9056)I,AX2(I),AY2(I)
 9056     FORMAT('I,AX2(I),AY2(I) = ',I8,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9055   CONTINUE
      ENDIF
      RETURN
      END
      SUBROUTINE DPENC2(Y,X,N,MAXNXT,
     1                  TEMP1,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,
     1                  Y2,X2,NOUT,AREA,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--GIVEN A SET OF (X,Y) PAIRS, RETURN THE 4 POINTS
C              THAT DEFINE THE MINIMUM AREA ENCLOSING RECTANGLE.
C              ALSO RETURN THAT AREA.
C     INPUT  ARGUMENTS--X      = A REAL VECTOR CONTAINING THE X
C                                COORDINATES OF THE POINTS
C                     --Y      = A REAL VECTOR CONTAINING THE Y
C                                COORDINATES OF THE POINTS
C                     --N      = NUMBER OF POINTS IN X, Y
C     OUTPUT ARGUMENTS--X2     = A REAL VECTOR CONTAINING THE X
C                                COORDINATES OF THE ENCLOSING RECTANGLE
C                     --Y2     = A REAL VECTOR CONTAINING THE Y
C                                COORDINATES OF THE ENCLOSING RECTANGLE
C                     --NOUT   = NUMBER OF POINTS IN X2, Y2
C                     --AREA   = THE MINIMUM AREA
C     REFERENCE--ALLISON AND NOGA, "ON THE COMPUTATION OF MINIMUM ENCASING
C                RECTANGLES AND SET DIAMETERS", CS81017-R, DEPARTMENT OF
C                COMPUTER SCIENCE, VIRGINIA POLYTECHNIC INSTITUTE AND
C                STATE UNIVERSITY, BLACKSBURG, VA 24061.
C     REFERENCE--ARNON AND GIESELMANN (1983), "A LINEAR TIME ALGORITHM
C                FOR THE MINIMUM AREA RECTANGLE ENCLOSING A CONVEX
C                POLYGON", REPORT NUMBER 83-463, COMPUTER SCIENCE
C                TECHNICAL REPORTS, PURDUE UNIVERSITY.
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
      REAL TEMP1(*)
      REAL PI
      REAL ALPHA
      REAL BETA
      REAL GAMMA
      REAL DELTA
      REAL AREA
      REAL ALPHAT
      REAL BETAT
      REAL GAMMAT
      REAL DELTAT
      REAL AREAT
      REAL D1
      REAL D2
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER MAXNXT
      INTEGER N
      INTEGER NHULL
      INTEGER NOUT
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER M
      INTEGER ISAVE
      INTEGER JSAVE
      INTEGER KSAVE
      INTEGER MSAVE
      INTEGER IEDGE
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IWRITE
C
      INCLUDE 'DPCOP2.INC'
C
      DATA PI /3.1415926535/
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ENC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPENC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
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
C     STEP 1: FIND THE CONVEX HULL
C
      IWRITE='OFF'
      CALL DP2DCH(Y,X,TEMP1,N,IWRITE,MAXNXT,
     1            Y2,X2,NHULL,
     1            ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,
     1            IBUGA3,IERROR)
      DO80I=1,NHULL
        Y(I)=Y2(I)
        X(I)=X2(I)
   80 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ENC2')THEN
        WRITE(ICOUT,91)NHULL
   91   FORMAT('AFTER CONVEX HULL, NHULL = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO93I=1,NHULL
          WRITE(ICOUT,95)I,X2(I),Y2(I)
   95     FORMAT('I,X2(I),Y2(I) = ',I8,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
   93   CONTINUE
      ENDIF
C
C     HAVEN'T BEEN ABLE TO GET THIS ALGORITHM WORKING.
C     COMMENT OUT FOR NOW.
C
C     STEP 2: INITIALIZATION
C
      ALPHA=0.0
      J=2
      K=2
      M=2
      BETA=ANGRAD(X(3),Y(3),X(2),Y(2),X(1),Y(1),IBUGA3)
      GAMMA=BETA
      DELTA=BETA
      AREA=CPUMAX
C
C     STEP 3: LOOP THROUGH VERTICES OF CONVEX POLYGON
C
      DO300I=1,NHULL
C
C       STEP 3A: FIND ANGLE OF ROTATION OF NEXT EDGE OF THE POLYGON
C
        IM1=I-1
        IP1=MOD(I+1-1,NHULL)+1
        IF(I.GT.1)THEN
          ALPHAT=ANGRAD(X(IP1),Y(IP1),X(I),Y(I),X(IM1),Y(IM1),IBUGA3)
          ALPHA=ALPHA + ALPHAT
        ENDIF
C
C       STEP 3B: FIND A VERTEX ON THE FIRST PERPINDICULAR LINE OF
C                SUPPORT
C
        IF(BETA.GE.ALPHA + (PI/2.0))GOTO319
  310   CONTINUE
          J=MOD(J+1-1,NHULL)+1
          JM1=MOD(J-1-1,NHULL)+1
          JP1=MOD(J+1-1,NHULL)+1
          BETAT=ANGRAD(X(JP1),Y(JP1),X(J),Y(J),X(JM1),Y(JM1),IBUGA3)
          BETA=BETA + BETAT
          IF(BETA.GE.ALPHA + (PI/2.0))GOTO319
          GOTO310
  319   CONTINUE
C
C       STEP 3C: FIND A VERTEX ON A PARALLEL LINE OF SUPPORT
C
        IF(GAMMA.GE.ALPHA + PI)GOTO329
  320   CONTINUE
          K=MOD(K+1-1,NHULL)+1
          KM1=MOD(K-1-1,NHULL)+1
          KP1=MOD(K+1-1,NHULL)+1
          GAMMAT=ANGRAD(X(KP1),Y(KP1),X(K),Y(K),X(KM1),Y(KM1),IBUGA3)
          GAMMA=GAMMA + GAMMAT
          IF(GAMMA.GE.ALPHA + PI)GOTO329
          GOTO320
  329   CONTINUE
C
C       STEP 3D: FIND A VERTEX ON THE SECOND PERPINDICULAR LINE OF
C                SUPPORT
C
        IF(DELTA.GE.ALPHA + (3.0*PI/2.0))GOTO339
  330   CONTINUE
          M=MOD(M+1-1,NHULL)+1
          MM1=MOD(M-1-1,NHULL)+1
          MP1=MOD(M+1-1,NHULL)+1
          DELTAT=ANGRAD(X(MP1),Y(MP1),X(M),Y(M),X(MM1),Y(MM1),IBUGA3)
          DELTA=DELTA + DELTAT
          IF(DELTA.GE.ALPHA + (3.0*PI/2.0))GOTO339
          GOTO330
  339   CONTINUE
C
C       STEP 3E: FIND DISTANCES BETWEEN PARALLEL AND PERPINDICULAR
C                LINES OF SUPPORT
C
        IF(X(I+1).EQ.X(I))THEN
          D1=ABS(X(K) - X(I))
          D2=ABS(Y(M) - Y(J))
        ELSEIF(Y(I+1).EQ.Y(I))THEN
          D1=ABS(Y(K) - Y(I))
          D2=ABS(X(M) - X(J))
        ELSE
          SLOPE=(Y(I+1) - Y(I))/(X(I+1) - X(I))
          SLOPE2=-1.0/SLOPE
          D1=DPNTLI(X(I),Y(I),X(K),Y(K),SLOPE,IBUGA3)
          D2=DPNTLI(X(J),Y(J),X(M),Y(M),SLOPE2,IBUGA3)
        ENDIF
C
C       STEP 3F: COMPUTE THE AREA
C
        AREAT=D1*D2
        IF(I.EQ.1 .OR. AREAT.LT.AREA)THEN
          AREA=AREAT
          IEDGE=I
          ISAVE=I
          JSAVE=J
          KSAVE=K
          MSAVE=M
        ENDIF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ENC2')THEN
        WRITE(ICOUT,391)I,J,K,M
  391   FORMAT('FROM MIDDLE OF DPENCB: I,J,K,M = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,392)ALPHA,BETA,GAMMA,DELTA
  392   FORMAT('ALPHA,BETA,GAMMA,DELTA = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,393)SLOPE,D1,D2,AREAT,AREA
  393   FORMAT('SLOPE,D1,D2,AREAT,AREA = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,395)IEDGE,ISAVE,JSAVE,KSAVE,MSAVE
  395   FORMAT('IEDGE,ISAVE,JSAVE,KSAVE,MSAVE = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
  300 CONTINUE
C
C     STEP 4: SAVE THE VERTICES OF THE ENCLOSING BOX
C
      Y2(1)=Y(ISAVE)
      X2(1)=X(ISAVE)
      Y2(2)=Y(JSAVE)
      X2(2)=X(JSAVE)
      Y2(3)=Y(KSAVE)
      X2(3)=X(KSAVE)
      Y2(4)=Y(MSAVE)
      X2(4)=X(MSAVE)
      NOUT=4
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ENC2')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DPENC2--')
        CALL DPWRST('XXX','BUG ')
        DO9055I=1,NOUT
          WRITE(ICOUT,9056)I,X2(I),Y2(I)
 9056     FORMAT('I,X2(I),Y2(I) = ',I8,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9055   CONTINUE
      ENDIF
      RETURN
      END
      SUBROUTINE DPENMU(IMPSW,
     1IERASV,
     1PWXMIS,PWXMAS,PWYMIS,PWYMAS,
     1IERASW,
     1PWXMIN,PWXMAX,PWYMIN,PWYMAX,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--END (TERMINATE) THE MULTIPLOT PROCESS
C     INPUT  ARGUMENTS--
C                       IMPSW = MULTIPLOT SWITCH (OFF OR ON)
C                       IERASV
C                       PWXMIS
C                       PWXMAS
C                       PWYMIS
C                       PWYMAS
C                       IBUGP2
C     OUTPUT ARGUMENTS--
C                       IMPSW
C                       IERASW
C                       PWXMIN
C                       PWXMAX
C                       PWYMIN
C                       PWYMAX
C                       IFOUND ('YES' OR 'NO' )
C                       IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/7
C     ORIGINAL VERSION--MARCH     1986.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IMPSW
      CHARACTER*4 IERASV
      CHARACTER*4 IERASW
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
CCCCC CHARACTER*4 IHWUSE
CCCCC CHARACTER*4 MESSAG
CCCCC CHARACTER*4 IHWORD
CCCCC CHARACTER*4 IHWOR2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEN'
      ISUBN2='MU  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPENMU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGP2,IFOUND,IERROR
   53 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,81)IMPSW
   81 FORMAT('IMPSW = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,82)IERASV
   82 FORMAT('IERASV = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,83)PWXMIS,PWXMAS,PWYMIS,PWYMAS
   83 FORMAT('PWXMIS,PWXMAS,PWYMIS,PWYMAS = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,84)IERASW
   84 FORMAT('IERASW = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,85)PWXMIN,PWXMAX,PWYMIN,PWYMAX
   85 FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *********************************************
C               **  TREAT THE    END OF MULTIPLOT    CASE  **
C               *********************************************
C
      IMPSW='OFF'
      IERASW=IERASV
      PWXMIN=PWXMIS
      PWXMAX=PWXMAS
      PWYMIN=PWYMIS
      PWYMAX=PWYMAS
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE MULTIPLOT SWITCH HAS JUST BEEN SET ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('TO   OFF')
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
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
 9011 FORMAT('***** AT THE END       OF DPENMU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGP2,IFOUND,IERROR
 9013 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)IMPSW
 9041 FORMAT('IMPSW = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9042)IERASV
 9042 FORMAT('IERASV = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9043)PWXMIS,PWXMAS,PWYMIS,PWYMAS
 9043 FORMAT('PWXMIS,PWXMAS,PWYMIS,PWYMAS = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9044)IERASW
 9044 FORMAT('IERASW = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9045)PWXMIN,PWXMAX,PWYMIN,PWYMAX
 9045 FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPEPM2(Y,N,ICASPL,MAXNXT,MINMAX,IGEPDF,
     1                  ISEED,NSAMP,
     1                  P,GAMMSV,SCALSV,ALOCSV,TEMP1,
     1                  ALOC,SCALE,SHAPE,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--ESTIMATE THE PARAMETERS OF A DISTRIBUTION USING
C              THE ELEMENTAL PERCENTILE METHOD DESCRIBED BY
C              CASTILLO, ET. AL. (SEE REFERENCE).
C
C              SUPPORTED DISTRIBUTIONS ARE:
C
C              1) GENERALIZED PARETO
C              2) GENERALIZED EXTREME VALUE
C
C     REFERENCE--CASTILLO, HADI, BALAKRISHNAN, SARABIA, "EXTREME
C                VALUE AND RELATED MODELS WITH APPLICATIONS IN
C                ENGINEERING AND SCIENCE", WILEY, 2005.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/6
C     ORIGINAL VERSION--JUNE      2005.
C     UPDATED         --AUGUST    2005. DUNRAN WAS FIXED TO GO FROM
C                                       0 TO N.  THIS ROUTINE WAS
C                                       MODIFIED TO CALL A VERSION
C                                       THAT GOES FROM 1 TO N.
C     UPDATED         --JUNE      2008. CORRECT ESTIMATE OF LOCATION
C                                       FOR GENERALIZED EXTREME VALUE
C
C---------------------------------------------------------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IGEPDF
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      DIMENSION Y(*)
      DIMENSION P(*)
      DIMENSION GAMMSV(*)
      DIMENSION SCALSV(*)
      DIMENSION ALOCSV(*)
      DIMENSION TEMP1(*)
      DIMENSION XRAN(3)
C
      DOUBLE PRECISION XIN
      DOUBLE PRECISION XJN
      DOUBLE PRECISION XRN
      DOUBLE PRECISION XNN
      DOUBLE PRECISION PIN
      DOUBLE PRECISION PJN
      DOUBLE PRECISION PRN
      DOUBLE PRECISION PNN
      DOUBLE PRECISION DIJR
      DOUBLE PRECISION AIJ
      DOUBLE PRECISION AJI
      DOUBLE PRECISION AIR
      DOUBLE PRECISION AJR
      DOUBLE PRECISION CI
      DOUBLE PRECISION CR
      DOUBLE PRECISION PJNSV
      DOUBLE PRECISION DELTA0
      DOUBLE PRECISION EPS
      DOUBLE PRECISION EPS2
      DOUBLE PRECISION SIG
      DOUBLE PRECISION XLOWER
      DOUBLE PRECISION XUPPER
      DOUBLE PRECISION XMID
      DOUBLE PRECISION FXLOW
      DOUBLE PRECISION FXUPP
      DOUBLE PRECISION FCS
      DOUBLE PRECISION XRML
C
      INTEGER R
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA EPS /0.00001D0/
      DATA SIG /1.0D-5/
      DATA MAXIT /300/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEP'
      ISUBN2='M2  '
      IWRITE='OFF'
C
C               ****************************************************
C               **  STEP 1--                                      **
C               **  A. SORT THE DATA                              **
C               **  B. COMPUTE THE P(I,N) = I/(N+1)               **
C               ****************************************************
C
      CALL SORT(Y,N,Y)
      DO110I=1,N
        P(I)=REAL(I)/REAL(N+1)
  110 CONTINUE
C
C               ****************************************************
C               **  STEP 2--                                      **
C               **  GENERATE EPM ESTIMATES FOR GIVEN DISTRIBUTION **
C               ****************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EPM2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASPL.EQ.'GPAR ')GOTO1000
      IF(ICASPL.EQ.'GPAR' .AND. N.GT.45)GOTO1000
      IF(ICASPL.EQ.'GPAR' .AND. N.LE.45)GOTO1500
      IF(ICASPL.EQ.'GEV ')GOTO2000
      GOTO9000
C
C  GENERAL ALGORITHM FOR GENERALIZED PARETO GIVEN ON PAGES 274-275
C  OF CASTILLO, ET.AL.
C
C  FOR N <= 45, COMPUTE FOR ALL SUBSETS ((45 2) = 990).  IF N > 45,
C  COMPUTE NSAMP RANDOM SUBSETS (GENERATE AT LEAST 1,000).
C
 1000 CONTINUE
C
      XNN=DBLE(Y(N))
      PNN=DBLE(P(N))
      PJNSV=DBLE(P(1))
      NCNT=0
      EPS2=1.0D-12
      DO1100II=1,NSAMP
C
        NTEMP=2
        CALL DUNRA2(NTEMP,N,ISEED,XRAN)
        I=INT(XRAN(1)+0.1)
        J=INT(XRAN(2)+0.1)
        IF(I.GT.J)THEN
          ITEMP=I
          I=J
          J=ITEMP
        ENDIF
C
        XIN=DBLE(Y(I))
        XJN=DBLE(Y(J))
        IF(XIN.EQ.XJN)GOTO1100
        PIN=DBLE(P(I))
        PJN=DBLE(P(J))
CCCCC   IF(PJN.EQ.PJNSV)GOTO1100
CCCCC   PJNSV=PJN
        DELTA0=(XIN/XJN) - (DLOG(1.0D0-PIN)/DLOG(1.0D0-PJN))
        IF(DELTA0.GT.0.0D0)THEN
          XLOWER=EPS2
          XUPPER=DLOG(1.0D0-XIN/XNN)/DLOG(1.0D0-PIN)
        ELSE
          XLOWER=DLOG(XIN/XNN)/DLOG((1.0D0-PIN)/(1.0D0-PNN))
          XUPPER=-EPS2
        ENDIF
        ICNT=0
        FXLOW=XIN*(1.0D0 - (1.0D0 - PJN)**XLOWER) -
     1        XJN*(1.0D0 - (1.0D0 - PIN)**XLOWER)
        FXUPP=XIN*(1.0D0 - (1.0D0 - PJN)**XUPPER) -
     1        XJN*(1.0D0 - (1.0D0 - PIN)**XUPPER)
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EPM2')THEN
          WRITE(ICOUT,1003)
 1003     FORMAT('DPEPM2: GENERALIZED PARETO')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1005)I,J,XIN,XJN,XNN
 1005     FORMAT('I,J,XIN,XJN,XNN = ',2I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1007)PIN,PJN,PNN
 1007     FORMAT('PIN,PJN,PNN = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1009)FXLOW,FXUPP
 1009     FORMAT('FXLOW,FXUPP = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 1110   CONTINUE
        XMID=(XLOWER+XUPPER)*0.5D0
        FCS=XIN*(1.0D0 - (1.0D0 - PJN)**XMID) -
     1      XJN*(1.0D0 - (1.0D0 - PIN)**XMID)
        IF(FCS*FXLOW.GT.0.0D0)THEN
          XLOWER=XMID
          FXLOW=FCS
        ELSE
          XUPPER=XMID
          FXUPP=FCS
        ENDIF
        XRML=XUPPER - XLOWER
        IF(XRML.LE.SIG .OR. ABS(FCS).LE.EPS)THEN
          NCNT=NCNT+1
          GAMMSV(NCNT)=REAL(XMID)
CCCCC     SCALSV(NCNT)=REAL(XMID*XJN/(1.0D0 - (1.0D0 - PJN)**XMID))
          SCALSV(NCNT)=REAL(XJN/(1.0D0 - (1.0D0 - PJN)**XMID))
          GOTO1100
        ELSE
          ICNT = ICNT + 1
          IF(ICNT.LE.MAXIT)GOTO1110
CCCCC       WRITE(ICOUT,1130)J
C1130       FORMAT('***** ITERATION ',I8,' OF GENERALIZED PARERO')
CCCCC       CALL DPWRST('XXX','BUG ')
CCCCC       WRITE(ICOUT,1133)
C1133       FORMAT('      ELEMENTAL PERCENTILE ESTIMATION DID NOT ',
CCCCC1             'DID NOT CONVERGE.')
CCCCC       CALL DPWRST('XXX','BUG ')
            GOTO1100
        ENDIF
C
 1100 CONTINUE
C
      CALL MEDIAN(GAMMSV,NCNT,IWRITE,TEMP1,MAXNXT,SHAPE,IBUGA3,IERROR)
      CALL MEDIAN(SCALSV,NCNT,IWRITE,TEMP1,MAXNXT,XMED,IBUGA3,IERROR)
      SCALE=SHAPE*XMED
      IF(IGEPDF.EQ.'SIMI')SHAPE=-SHAPE
      GOTO9000
C
 1500 CONTINUE
C
      XNN=DBLE(Y(N))
      PNN=DBLE(P(N))
      PJNSV=DBLE(P(1))
      NCNT=0
      EPS2=1.0D-12
      DO1600I=1,N-1
        DO1610J=I+1,N
C
          XIN=DBLE(Y(I))
          XJN=DBLE(Y(J))
          IF(XIN.EQ.XJN)GOTO1610
          PIN=DBLE(P(I))
          PJN=DBLE(P(J))
          IF(PJN.EQ.PJNSV)GOTO1610
          PJNSV=PJN
          DELTA0=(XIN/XJN) - (DLOG(1.0D0-PIN)/DLOG(1.0D0-PJN))
          IF(DELTA0.GT.0.0D0)THEN
            XLOWER=EPS2
            XUPPER=DLOG(1.0D0-XIN/XNN)/DLOG(1.0D0-PIN)
          ELSE
            XLOWER=DLOG(XIN/XNN)/DLOG((1.0D0-PIN)/(1.0D0-PNN))
            XUPPER=-EPS2
          ENDIF
          FXLOW=XIN*(1.0D0 - (1.0D0 - PJN)**XLOWER) -
     1          XJN*(1.0D0 - (1.0D0 - PIN)**XLOWER)
          FXUPP=XIN*(1.0D0 - (1.0D0 - PJN)**XUPPER) -
     1          XJN*(1.0D0 - (1.0D0 - PIN)**XUPPER)
          ICNT=0
        
C
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EPM2')THEN
            WRITE(ICOUT,1503)
 1503       FORMAT('DPEPM2: GENERALIZED PARETO')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1505)I,J,XIN,XJN,XNN
 1505       FORMAT('I,J,XIN,XJN,XNN = ',2I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1507)PIN,PJN,PNN
 1507       FORMAT('PIN,PJN,PNN = ',3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1509)FXLOW,FXUPP,MINMAX
 1509       FORMAT('FXLOW,FXUPP,MINMAX = ',2G15.7,I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 1630     CONTINUE
          XMID=(XLOWER+XUPPER)*0.5D0
          FCS=XIN*(1.0D0 - (1.0D0 - PJN)**XMID) -
     1        XJN*(1.0D0 - (1.0D0 - PIN)**XMID)
          IF(FCS*FXLOW.GT.0.0D0)THEN
            XLOWER=XMID
            FXLOW=FCS
          ELSE
            XUPPER=XMID
            FXUPP=FCS
          ENDIF
          XRML=XUPPER - XLOWER
          IF(XRML.LE.SIG .OR. ABS(FCS).LE.EPS)THEN
            NCNT=NCNT+1
            GAMMSV(NCNT)=REAL(XMID)
            SCALSV(NCNT)=REAL(XJN/(1.0D0 - (1.0D0 - PJN)**XMID))
            GOTO1630
          ELSE
            ICNT = ICNT + 1
            IF(ICNT.LE.MAXIT)GOTO1630
CCCCC       WRITE(ICOUT,1640)J
C1640       FORMAT('***** ITERATION ',I8,' OF GENERALIZED PARERO')
CCCCC       CALL DPWRST('XXX','BUG ')
CCCCC       WRITE(ICOUT,1643)
C1643       FORMAT('      ELEMENTAL PERCENTILE ESTIMATION DID NOT ',
CCCCC1             'DID NOT CONVERGE.')
CCCCC       CALL DPWRST('XXX','BUG ')
            GOTO1610
          ENDIF
C
 1610   CONTINUE
 1600 CONTINUE
C
      CALL MEDIAN(GAMMSV,NCNT,IWRITE,TEMP1,MAXNXT,SHAPE,IBUGA3,IERROR)
      CALL MEDIAN(SCALSV,NCNT,IWRITE,TEMP1,MAXNXT,XMED,IBUGA3,IERROR)
      SCALE=SHAPE*XMED
      IF(IGEPDF.EQ.'SIMI')SHAPE=-SHAPE
      GOTO9000
C
C  GENERAL ALGORITHM FOR GENERALIZED EXTREME VALUE GIVEN ON
C  PAGES 220-223  CASTILLO, ET.AL.
C
 2000 CONTINUE
C
      XNN=DBLE(Y(N))
      PNN=DBLE(P(N))
      NCNT=0
      EPS2=1.0D-12
      DO2100II=1,NSAMP
C
        NTEMP=3
CCCCC   CALL DUNRAN(NTEMP,N,ISEED,XRAN)
        CALL DUNRA2(NTEMP,N,ISEED,XRAN)
        CALL SORT(XRAN,NTEMP,XRAN)
        I=INT(XRAN(1)+0.1)
        J=INT(XRAN(2)+0.1)
        R=INT(XRAN(3)+0.1)
C
        XIN=DBLE(Y(I))
        XJN=DBLE(Y(J))
        XRN=DBLE(Y(R))
        XNN=DBLE(Y(N))
        IF(XIN.EQ.XJN)GOTO2100
        IF(XJN.EQ.XRN)GOTO2100
        PIN=DBLE(P(I))
        PJN=DBLE(P(J))
        PRN=DBLE(P(R))
        PNN=DBLE(P(N))
        AIR=DLOG(PIN)/DLOG(PRN)
        AJR=DLOG(PJN)/DLOG(PRN)
        AIJ=DLOG(PIN)/DLOG(PJN)
        AJI=DLOG(PJN)/DLOG(PIN)
        DIJR=(XJN-XRN)/(XIN-XRN)
        IF(DIJR.LT.DLOG(AJR)/DLOG(AIR))THEN
          XLOWER=EPS2
          XUPPER=DLOG(DIJR)/DLOG(AJI)
        ELSE
          XLOWER=DLOG(1.0D0-DIJR)/DLOG(AJR)
          XUPPER=-EPS2
        ENDIF
        ICNT=0
        FXLOW=(1.0D0 - AJR**XLOWER)/(1.0D0 - AIR**XLOWER) - DIJR
        FXUPP=(1.0D0 - AJR**XUPPER)/(1.0D0 - AIR**XUPPER) - DIJR
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EPM2')THEN
          WRITE(ICOUT,2003)
 2003     FORMAT('DPEPM2: GENERALIZED EXTREME VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2005)I,J,XIN,XJN,XRN
 2005     FORMAT('I,J,XIN,XJN,XRN = ',2I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2007)PIN,PJN,PRN
 2007     FORMAT('PIN,PJN,PRN = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2006)AIR,AJR,AIJ,AJI
 2006     FORMAT('AIR,AJR,AIJ,AJI = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2009)DIJR,FXLOW,FXUPP
 2009     FORMAT('DIJR,FXLOW,FXUPP = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 2110   CONTINUE
        XMID=(XLOWER+XUPPER)*0.5D0
        FCS=(1.0D0 - AJR**XMID)/(1.0D0 - AIR**XMID) - DIJR
        IF(FCS*FXLOW.GT.0.0D0)THEN
          XLOWER=XMID
          FXLOW=FCS
        ELSE
          XUPPER=XMID
          FXUPP=FCS
        ENDIF
        XRML=XUPPER - XLOWER
        IF(XRML.LE.SIG .OR. ABS(FCS).LE.EPS)THEN
          NCNT=NCNT+1
          GAMMSV(NCNT)=REAL(XMID)
          CR=-DLOG(PRN)
          CI=-DLOG(PIN)
          DSCALE=XMID*(XIN-XRN)/(CR**XMID - CI**XMID)
          SCALSV(NCNT)=REAL(XMID*(XIN-XRN)/(CR**XMID - CI**XMID))
CCCCC     JUNE 2008: FORMULA FOR LOCATION WRONG, SEE P. 221
CCCCC     DLOC=XIN - DSCALE*DLOG(CI)
          DLOC=XIN - DSCALE*(1.0D0 - CI**XMID)/XMID
          ALOCSV(NCNT)=REAL(DLOC)
          GOTO2100
        ELSE
          ICNT = ICNT + 1
          IF(ICNT.LE.MAXIT)GOTO2110
CCCCC       WRITE(ICOUT,2130)J
C2130       FORMAT('***** ITERATION ',I8,' OF GENERALIZED EXTREME ',
CCCCC1             'VALUE')
CCCCC       CALL DPWRST('XXX','BUG ')
CCCCC       WRITE(ICOUT,2133)
C2133       FORMAT('      ELEMENTAL PERCENTILE ESTIMATION DID NOT ',
CCCCC1             'CONVERGE.')
CCCCC       CALL DPWRST('XXX','BUG ')
            GOTO2100
        ENDIF
C
 2100 CONTINUE
C
      CALL MEDIAN(GAMMSV,NCNT,IWRITE,TEMP1,MAXNXT,SHAPE,IBUGA3,IERROR)
      CALL MEDIAN(SCALSV,NCNT,IWRITE,TEMP1,MAXNXT,SCALE,IBUGA3,IERROR)
      CALL MEDIAN(ALOCSV,NCNT,IWRITE,TEMP1,MAXNXT,ALOC,IBUGA3,IERROR)
      GOTO9000
C
 9000 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPEQFU(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS THAT WILL DEFINE
C              AN EMPIRICAL QUANTILE FUNCTION PLOT
C
C              OPTIONALLY OVERLAY A "QUANTILE BOX PLOT" (NOT SUPPORTED
C              FOR MULTIPLE AND REPLICATION CASES).  TO GENERATE THIS
C              DISPLAY WITH THE QUANTILE BOX PLOT, USE THE COMMAND
C
C                    QUANTILE BOX PLOT Y
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
C     VERSION NUMBER--2017/02
C     ORIGINAL VERSION--FEBRUARY  2017.
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
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION ZY(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XDESGN(MAXOBV,2)
C
      INCLUDE 'DPCOZZ.INC'
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB9),ZY(1))
      EQUIVALENCE (GARBAG(IGAR10),XDESGN(1,1))
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPEQ'
      ISUBN2='FU  '
      ICASE2='MULT'
      ICASPL='EQFU'
      IMULT='OFF'
      IREPL='OFF'
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
C               **  TREAT THE EMPIRICAL QUANTILE PLOT            **
C               ***************************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEQFU--')
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
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:         **
C               **    1) EMPIRICAL QUANTILE PLOT Y                  **
C               **    2) QUANTILE BOX PLOT Y                        **
C               **    2) MULTIPLE EMPIRICAL QUANTILE PLOT Y1 ... YK **
C               **    3) REPLICATED EMPIRCAL QUANTILE PLOT Y X1  X2 **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CHECK FOR "QUANTILE BOX PLOT" FIRST.  THIS DOES NOT SUPPORT
C     "MULTIPLE" OR "REPLICATION" OPTION, SO NO NEED TO SEARCH
C     FOR THOSE.
C
      IF(ICOM.EQ.'QUAN' .AND. IHARG(1).EQ.'BOX ' .AND.
     1   IHARG(2).EQ.'PLOT')THEN
        ICASE2='SING'
        ILASTC=2
        IFOUND='YES'
        GOTO110
      ENDIF
C
      IF(ICOM.EQ.'EMPI')THEN
        IFOUN1='YES'
      ELSEIF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
      ELSEIF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
      ELSE
        GOTO9000
      ENDIF
C
      ILASTC=-9999
C
      ISTOP=NUMARG-1
      DO90I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT')THEN
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
        ELSEIF(IHARG(I).EQ.'EMPI' .AND. IHARG(I+1).EQ.'QUAN')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'PLOT')THEN
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
  101     FORMAT('***** ERROR IN EMPIRICAL QUANTILE PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THE EMPIRICAL QUANTILE PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
  110 CONTINUE
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'EQFU')THEN
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='EMPIRICAL QUANTILE PLOT'
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')THEN
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 1: NO REPLICATION              **
C               ******************************************
C
      IF(NREPL.EQ.0)THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
        NPLOTP=0
        DO810IRESP=1,NRESP
          NCURVE=IRESP
C
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')THEN
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
     1                Y1,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 8B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               *****************************************************
C
          CALL DPEQF2(Y1,XTEMP1,XTEMP2,NCURVE,NLOCAL,ICASPL,ICASE2,
     1                TEMP1,MAXOBV,
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
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICASE2='REPL'
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
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')THEN
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
              CALL DPEQF2(ZY,XTEMP1,XTEMP2,NCURVE,NTEMP,ICASPL,ICASE2,
     1                    TEMP1,MAXOBV,
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
              CALL DPEQF2(ZY,XTEMP1,XTEMP2,NCURVE,NTEMP,ICASPL,ICASE2,
     1                    TEMP1,MAXOBV,
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'EQFU')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEQFU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASPL,IAND1,IAND2
 9012   FORMAT('IFOUND,IERROR,ICASPL,IAND1,IAND2 = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS
 9013   FORMAT('NPLOTV,NPLOTP,NS = ',3I8,I8,I8)
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
      SUBROUTINE DPEQF2(Y,TEMPY,TEMPU,NCURVE,N,ICASPL,ICASE,
     1                  TEMP1,MAXNXT,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN EMPIRICAL QUANTILE PLOT.
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
C     VERSION NUMBER--2017/02
C     ORIGINAL VERSION--FEBRUARY  2017.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASE
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
      DIMENSION TEMPY(*)
      DIMENSION TEMPU(*)
      DIMENSION TEMP1(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEQ'
      ISUBN2='F2  '
      IWRITE='OFF '
      IERROR='NO'
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
   31   FORMAT('***** ERROR IN EMPIRICAL QUANTILE PLOT--')
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
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'EQF2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPEQF2--')
        CALL DPWRST('XXX','BUG ')
        DO73I=1,N
          WRITE(ICOUT,74)I,REAL(Y(I))
   74     FORMAT('I, Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C               **********************************************
C               **  STEP 2--                                **
C               **  CALL EMPQUA ROUTINE TO COMPUTE THE      **
C               **  EMPIRICAL QUANTILE FUNCTION             **
C               **********************************************
C
      CALL EMPQUA(Y,N,IWRITE,TEMPY,TEMPU,NOUT,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      DO410I=1,NOUT
        N2=N2+1
        Y2(N2)=TEMPY(I)
        X2(N2)=TEMPU(I)
        D2(N2)=REAL(NCURVE)
  410 CONTINUE
C
      IF(ICASE.NE.'SING')GOTO9000
C
C     DRAW QUANTILE BOX PLOT FOR CASE WHERE THERE IS A SINGLE
C     RESPONSE VARIABLE AND NO REPLICATION.
C
      QNT=0.50
      CALL QUANT(QNT,TEMPY,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           QU50,IBUGG3,IERROR)
C
      QNT=0.75
      CALL QUANT(QNT,TEMPY,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           QU75,IBUGG3,IERROR)
C
      QNT=0.25
      CALL QUANT(QNT,TEMPY,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           QU25,IBUGG3,IERROR)
C
      QNT=0.875
      CALL QUANT(QNT,TEMPY,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           QU875,IBUGG3,IERROR)
C
      QNT=0.125
      CALL QUANT(QNT,TEMPY,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           QU125,IBUGG3,IERROR)
C
      QNT=0.9375
      CALL QUANT(QNT,TEMPY,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           QU9375,IBUGG3,IERROR)
C
      QNT=0.0625
      CALL QUANT(QNT,TEMPY,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           QU0625,IBUGG3,IERROR)
C
      QNT=0.50
      CALL QUANT(QNT,TEMPU,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           U50,IBUGG3,IERROR)
C
      QNT=0.75
      CALL QUANT(QNT,TEMPU,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           U75,IBUGG3,IERROR)
C
      QNT=0.25
      CALL QUANT(QNT,TEMPU,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           U25,IBUGG3,IERROR)
C
      QNT=0.875
      CALL QUANT(QNT,TEMPU,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           U875,IBUGG3,IERROR)
C
      QNT=0.125
      CALL QUANT(QNT,TEMPU,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           U125,IBUGG3,IERROR)
C
      QNT=0.9375
      CALL QUANT(QNT,TEMPU,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           U9375,IBUGG3,IERROR)
C
      QNT=0.0625
      CALL QUANT(QNT,TEMPU,NOUT,IWRITE,TEMP1,MAXNXT,
     1           IQUAME,
     1           U0625,IBUGG3,IERROR)
C
      N2=N2+1
      Y2(N2)=QU50
      X2(N2)=U25
      D2(N2)=2.0
      N2=N2+1
      Y2(N2)=QU50
      X2(N2)=U75
      D2(N2)=2.0
C
      N2=N2+1
      Y2(N2)=QU25
      X2(N2)=U25
      D2(N2)=3.0
      N2=N2+1
      Y2(N2)=QU25
      X2(N2)=U75
      D2(N2)=3.0
      N2=N2+1
      Y2(N2)=QU75
      X2(N2)=U75
      D2(N2)=3.0
      N2=N2+1
      Y2(N2)=QU75
      X2(N2)=U25
      D2(N2)=3.0
      N2=N2+1
      Y2(N2)=QU25
      X2(N2)=U25
      D2(N2)=3.0
C
      N2=N2+1
      Y2(N2)=QU125
      X2(N2)=U125
      D2(N2)=4.0
      N2=N2+1
      Y2(N2)=QU125
      X2(N2)=U875
      D2(N2)=4.0
      N2=N2+1
      Y2(N2)=QU875
      X2(N2)=U875
      D2(N2)=4.0
      N2=N2+1
      Y2(N2)=QU875
      X2(N2)=U125
      D2(N2)=4.0
      N2=N2+1
      Y2(N2)=QU125
      X2(N2)=U125
      D2(N2)=4.0
C
      N2=N2+1
      Y2(N2)=QU0625
      X2(N2)=U0625
      D2(N2)=5.0
      N2=N2+1
      Y2(N2)=QU0625
      X2(N2)=U9375
      D2(N2)=5.0
      N2=N2+1
      Y2(N2)=QU9375
      X2(N2)=U9375
      D2(N2)=5.0
      N2=N2+1
      Y2(N2)=QU9375
      X2(N2)=U0625
      D2(N2)=5.0
      N2=N2+1
      Y2(N2)=QU0625
      X2(N2)=U0625
      D2(N2)=5.0
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
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'EQF2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEQF2--')
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
      SUBROUTINE DPEQSL(TEMP1,TEMP2,MAXNXT,
     1                  ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT AN EQUAL SLOPES TEST (I.E., ARE TWO OR MORE
C              REGRESSION LINES PARALLEL?).
C     EXAMPLE--EQUAL SLOPES TEST Y X TAG
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/10
C     ORIGINAL VERSION--OCTOBER   2015.
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
      CHARACTER*12 ICASE2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ICASE
C
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IVARI5
      CHARACTER*4 IVARI6
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
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TAG(MAXOBV)
C
      DOUBLE PRECISION RESVAR(100)
      DOUBLE PRECISION SLOPE(100)
      DOUBLE PRECISION AINTER(100)
      DOUBLE PRECISION Q(100)
      DOUBLE PRECISION QYX(100)
      DOUBLE PRECISION QY(100)
      DOUBLE PRECISION QXY(100)
C
      DIMENSION NSIZE(MAXOBV)
      EQUIVALENCE(GARBAG(IGARB1),TEMP3(1))
      EQUIVALENCE(GARBAG(IGARB2),TAG(1))
C
      EQUIVALENCE(DGARBG(IDGAR1),RESVAR(1))
      EQUIVALENCE(DGARBG(IDGAR1+200),SLOPE(1))
      EQUIVALENCE(DGARBG(IDGAR1+400),AINTER(1))
      EQUIVALENCE(DGARBG(IDGAR1+600),Q(1))
      EQUIVALENCE(DGARBG(IDGAR1+800),QYX(1))
      EQUIVALENCE(DGARBG(IDGAR1+1000),QY(1))
      EQUIVALENCE(DGARBG(IDGAR1+1200),QXY(1))
C
      EQUIVALENCE(IGARBG(IIGAR1),NSIZE(1))
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEQ'
      ISUBN2='S3  '
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
C               ************************************************
C               **  TREAT THE EQUAL SLOPES TEST CASE          **
C               ************************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EQSL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2SIN--')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EQSL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASAN='NULL'
      IF(ICOM.EQ.'EQUA' .AND. IHARG(1).EQ.'SLOP' .AND.
     1   IHARG(2).EQ.'TEST')THEN
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGA2,IERROR)
        IFOUND='YES'
        ICASAN='EQSL'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EQSL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='EQUAL SLOPES TEST'
      MINNA=1
      MAXNA=100
      MINN2=6
      IFLAGE=1
      IFLAGM=0
      MINNVA=3
      MAXNVA=3
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EQSL')THEN
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
C               ************************************************
C               **  STEP 3--                                  **
C               **  THREE RESPONSE VARIABLES                  **
C               ************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EQSL')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVAR=3
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y,X,TAG,NS1,NS1,NS1,ICASE,
     1            IBUGA3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ****************************************
C               **  STEP 4--                          **
C               **  PERFORM THE EQUAL SLOPES TEST     **
C               ****************************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EQSL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5211)
 5211   FORMAT('***** FROM DPEQSL, BEFORE CALL DPEQS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5212)NS1
 5212   FORMAT('NS1 = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO5215II=1,NS1
          WRITE(ICOUT,5216)II,Y(II),X(II),TAG(II)
 5216     FORMAT('I,Y(I),X(I),TAG(II) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 5215   CONTINUE
      ENDIF
C
      IVARID=IVARN1(1)
      IVARI2=IVARN2(1)
      IVARI3=IVARN1(2)
      IVARI4=IVARN2(2)
      IVARI5=IVARN1(3)
      IVARI6=IVARN2(3)
      CALL DPEQS2(Y,X,TAG,NS1,ICASAN,MAXOBV,ICASE2,
     1            IVARID,IVARI2,IVARI3,IVARI4,IVARI5,IVARI6,
     1            TEMP1,TEMP2,TEMP3,
     1            SLOPE,NUMSLO,AINTER,RESVAR,Q,QYX,QY,QXY,NSIZE,
     1            ICAPSW,ICAPTY,IFORSW,
     1            STATVA,STATCD,PVALUE,
     1            STATV1,STATC1,PVAL1,
     1            STATV2,STATC2,PVAL2,
     1            STATV3,STATC3,PVAL3,
     1            CV80,CV90,CV95,CV99,
     1            CV180,CV190,CV195,CV199,
     1            CV280,CV290,CV295,CV299,
     1            CV380,CV390,CV395,CV399,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ***************************************
C               **  STEP 6--                         **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EQSL')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IH='NUMB'
        IH2='SLOP'
        VALUE0=REAL(NUMSLO)
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
      IF(ICASE2.NE.'3ORMORE')THEN
        IH='STAT'
        IH2='VAL '
        VALUE0=STATVA
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='STAT'
        IH2='CDF '
        VALUE0=STATCD
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='PVAL'
        IH2='UE  '
        VALUE0=PVALUE
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP80'
        VALUE0=CV80
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP90'
        VALUE0=CV90
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP95'
        VALUE0=CV95
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP99'
        VALUE0=CV99
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
      ELSE
        IH='STAT'
        IH2='VAL1'
        VALUE0=STATV1
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='STAT'
        IH2='CDF1'
        VALUE0=STATC1
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='PVAL'
        IH2='UE1 '
        VALUE0=PVAL1
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P180'
        VALUE0=CV180
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P190'
        VALUE0=CV190
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P195'
        VALUE0=CV195
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P199'
        VALUE0=CV199
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='STAT'
        IH2='VAL2'
        VALUE0=STATV2
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='STAT'
        IH2='CDF2'
        VALUE0=STATC2
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='PVAL'
        IH2='UE2 '
        VALUE0=PVAL2
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P280'
        VALUE0=CV280
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P290'
        VALUE0=CV290
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P295'
        VALUE0=CV295
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P299'
        VALUE0=CV299
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='STAT'
        IH2='VAL3'
        VALUE0=STATV3
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='STAT'
        IH2='CDF3'
        VALUE0=STATC3
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='PVAL'
        IH2='UE3 '
        VALUE0=PVAL3
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P380'
        VALUE0=CV380
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P390'
        VALUE0=CV390
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P395'
        VALUE0=CV395
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P399'
        VALUE0=CV399
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EQSL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEQSL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEQS2(Y,X,TAG,N,ICASAN,MAXNXT,ICASE,
     1                  IVARID,IVARI2,IVARI3,IVARI4,IVARI5,IVARI6,
     1                  TEMP1,TEMP2,TEMP3,
     1                  SLOPE,NUMSLO,AINTER,RESVAR,Q,QYX,QY,QXY,NSIZE,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  STATVA,STATCD,PVAL2T,
     1                  STATV1,STATC1,PVAL1,
     1                  STATV2,STATC2,PVAL2,
     1                  STATV3,STATC3,PVAL3,
     1                  CV80,CV90,CV95,CV99,
     1                  CV180,CV190,CV195,CV199,
     1                  CV280,CV290,CV295,CV299,
     1                  CV380,CV390,CV395,CV399,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--CARRY OUT AN EQUAL SLOPES TEST (I.E., ARE TWO OR MORE
C              REGRESSION LINES PARALLEL?).
C     EXAMPLE--EQUAL SLOPES TEST Y X TAG
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/10
C     ORIGINAL VERSION--OCTOBER   2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*12 ICASE
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICASAN
      CHARACTER*4 IFORSW
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IVARI5
      CHARACTER*4 IVARI6
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION TAG(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
C
      DOUBLE PRECISION SLOPE(*)
      DOUBLE PRECISION AINTER(*)
      DOUBLE PRECISION RESVAR(*)
      DOUBLE PRECISION Q(*)
      DOUBLE PRECISION QYX(*)
      DOUBLE PRECISION QY(*)
      DOUBLE PRECISION QXY(*)
      DIMENSION NSIZE(*)
C
      PARAMETER (NUMALP=4)
      REAL ALPHA(NUMALP)
C
      PARAMETER(NUMCLI=5)
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
      INCLUDE 'DPCOP2.INC'
C
      DATA ALPHA/0.80, 0.90, 0.95, 0.99/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEQ'
      ISUBN2='S2  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPEQS2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,N
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,N = ',3(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),X(I),TAG(I)
   57     FORMAT('I,Y(I),X(I),TAG(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ******************************
C               **  STEP 21--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR AN          F TEST  **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPEQS3(Y,X,TAG,N,MAXNXT,
     1            TEMP1,TEMP2,TEMP3,
     1            SLOPE,AINTER,RESVAR,Q,QYX,QY,QXY,NSIZE,
     1            NUMSLO,ICASE,
     1            STATVA,STATCD,PVAL2T,
     1            STATV1,STATC1,PVAL1,
     1            STATV2,STATC2,PVAL2,
     1            STATV3,STATC3,PVAL3,
     1            CV80,CV90,CV95,CV99,
     1            CV180,CV190,CV195,CV199,
     1            CV280,CV290,CV295,CV299,
     1            CV380,CV390,CV395,CV399,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************
C               **   STEP 42--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR EQUAL SLOPES TEST  **
C               ******************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
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
C     PRINT A SUMMARY TABLE
C
      ITITLE='Summary Table'
      NCTITL=13
      ITITL9=' '
      NCTIT9=0
C
      DO4030J=1,NUMCLI
        DO4040I=1,2
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 4040   CONTINUE
 4030 CONTINUE
C
      ITITL2(2,1)='Group-ID'
      NCTIT2(2,1)=8
C
      ITITL2(1,2)='Sample'
      NCTIT2(1,2)=6
      ITITL2(2,2)='Size'
      NCTIT2(2,2)=4
C
      ITITL2(2,3)='Intercept'
      NCTIT2(2,3)=9
C
      ITITL2(2,4)='Slope'
      NCTIT2(2,4)=5
C
      ITITL2(1,5)='Residual'
      NCTIT2(1,5)=8
      ITITL2(2,5)='Variance'
      NCTIT2(2,5)=8
C
      NMAX=0
      NUMCOL=5
      DO4050I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=10
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2)IDIGIT(I)=0
 4050 CONTINUE
C
      IWHTML(1)=100
      IWHTML(2)=100
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
C
      ICNT=NUMSLO
      DO4060J=1,NUMSLO
        AMAT(J,1)=REAL(J)
        AMAT(J,2)=REAL(NSIZE(J))
        AMAT(J,3)=AINTER(J)
        AMAT(J,4)=SLOPE(J)
        AMAT(J,5)=RESVAR(J)
        IVALUE(J,1)=' '
        NCVALU(J,1)=0
        IVALUE(J,2)=' '
        NCVALU(J,2)=0
        IVALUE(J,3)=' '
        NCVALU(J,3)=0
        IVALUE(J,4)=' '
        NCVALU(J,4)=0
        IVALUE(J,5)=' '
        NCVALU(J,5)=0
 4060 CONTINUE
C
      NUMLIN=2
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
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMSLO.EQ.3)GOTO6000
C
C     EQUAL SLOPES FOR TWO GROUPS CASE
C
      ITITLE='Equal Slopes Test for Two Groups'
      NCTITL=32
      IF(ICASE.EQ.'2EQUALVAR')THEN
        ITITLZ='(Equal Residual Variances Case)'
        NCTITZ=31
      ELSEIF(ICASE.EQ.'2NEVAR')THEN
        ITITLZ='(Unequal Residual Variances Case)'
        NCTITZ=33
      ELSEIF(ICASE.EQ.'2NEVARSS')THEN
        ITITLZ='(Small Samples Unequal Residual Variances Case)'
        NCTITZ=47
      ENDIF
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Dependent (Y) Variable:  '
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(30:33),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Independent (X) Variable:  '
      WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(32:35),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Group-ID Variable:  '
      WRITE(ITEXT(ICNT)(22:25),'(A4)')IVARI5(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI6(1:4)
      NCTEXT(ICNT)=29
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
      ITEXT(ICNT)='H0: The Regression Slopes are Equal'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Regression Slopes are not Equal'
      NCTEXT(ICNT)=39
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Number of Observations:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Groups with Ni > 3:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(NUMSLO)
      IDIGIT(ICNT)=0
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
      ITEXT(ICNT)='Equal Slopes Test Statistic Value:'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL2T
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO4010I=1,NUMROW
        NTOT(I)=15
 4010 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9='H0: Slopes Are Equal'
      NCTIT9=20
      IF(ICASE.EQ.'2EQUALVAR')THEN
        ITITLE='Conclusions (Two-Tailed t-Test)'
        NCTITL=31
      ELSEIF(ICASE.EQ.'2NEVAR')THEN
        ITITLE='Conclusions (Two-Tailed Normal Test)'
        NCTITL=36
      ELSEIF(ICASE.EQ.'2NEVARSS')THEN
        ITITLE='Conclusions (Two-Tailed t Test)'
        NCTITL=31
      ENDIF
C
      DO5030J=1,NUMCLI
        DO5040I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5040   CONTINUE
 5030 CONTINUE
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
      ITITL2(3,3)='Region (+/-)'
      NCTIT2(3,3)=12
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
      DO5050I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.4)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 5050 CONTINUE
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
      ICNT=NUMALP
      AMAT(1,3)=CV80
      AMAT(2,3)=CV90
      AMAT(3,3)=CV95
      AMAT(4,3)=CV99
C
      DO5060J=1,NUMALP
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
        AMAT(J,1)=0.0
        AMAT(J,2)=STATVA
        IVALUE(J,4)(1:6)='REJECT'
        NCVALU(J,4)=6
        IF(ICASE.EQ.'3ORMORE')THEN
          IF(STATVA.LT.AMAT(J,3))IVALUE(J,4)(1:6)='ACCEPT'
        ELSE
          IF(ABS(STATVA).LT.ABS(AMAT(J,3)))IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
 5060 CONTINUE
C
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
 6000 CONTINUE
C
C     THREE OR MORE GROUPS CASE
C
      ITITLE='Equal Regressions Test'
      NCTITL=22
      ITITLZ='(More Than Two Groups Case)'
      NCTITZ=27
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Dependent (Y) Variable:  '
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(30:33),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Independent (X) Variable:  '
      WRITE(ITEXT(ICNT)(28:31),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(32:35),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Group-ID Variable:  '
      WRITE(ITEXT(ICNT)(22:25),'(A4)')IVARI5(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI6(1:4)
      NCTEXT(ICNT)=29
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
      ITEXT(ICNT)='H0: The Regressions are Equal'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Regressions are not Equal'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Number of Observations:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Groups with Ni > 3:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(NUMSLO)
      IDIGIT(ICNT)=0
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
      ITEXT(ICNT)='Equal Regressions Test Statistic Value:'
      NCTEXT(ICNT)=39
      AVALUE(ICNT)=STATV1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=STATC1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL1
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO6010I=1,NUMROW
        NTOT(I)=15
 6010 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='6B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9='H0: Regressions Are Equal'
      NCTIT9=25
      ITITLE='Conclusions (Upper-Tailed F Test)'
      NCTITL=33
C
      DO6030J=1,NUMCLI
        DO6040I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 6040   CONTINUE
 6030 CONTINUE
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
      ITITL2(3,3)='Region (>)'
      NCTIT2(3,3)=10
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
      DO6050I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.4)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 6050 CONTINUE
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
      ICNT=NUMALP
      AMAT(1,3)=CV180
      AMAT(2,3)=CV190
      AMAT(3,3)=CV195
      AMAT(4,3)=CV199
C
      DO6060J=1,NUMALP
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
        AMAT(J,1)=0.0
        AMAT(J,2)=STATV1
        IVALUE(J,4)(1:6)='REJECT'
        NCVALU(J,4)=6
        IF(STATV1.LT.AMAT(J,3))IVALUE(J,4)(1:6)='ACCEPT'
 6060 CONTINUE
C
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
      ITITLE='Equal Slopes Test'
      NCTITL=17
      ITITLZ='(More Than Two Groups Case)'
      NCTITZ=27
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Slopes are Equal'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Slopes are not Equal'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
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
      ITEXT(ICNT)='Equal Slopes Test Statistic Value:'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=STATV2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=STATC2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL2
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO6110I=1,NUMROW
        NTOT(I)=15
 6110 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='61A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='61B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9='H0: Slopes Are Equal'
      NCTIT9=20
      ITITLE='Conclusions (Upper-Tailed F Test)'
      NCTITL=33
C
      DO6130J=1,NUMCLI
        DO6140I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 6140   CONTINUE
 6130 CONTINUE
C
      ICNT=NUMALP
      AMAT(1,3)=CV280
      AMAT(2,3)=CV290
      AMAT(3,3)=CV295
      AMAT(4,3)=CV299
C
      DO6160J=1,NUMALP
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
        AMAT(J,1)=0.0
        AMAT(J,2)=STATV2
        IVALUE(J,4)(1:6)='REJECT'
        NCVALU(J,4)=6
        IF(STATV2.LT.AMAT(J,3))IVALUE(J,4)(1:6)='ACCEPT'
 6160 CONTINUE
C
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
      ITITLE='Equal Intercepts Test'
      NCTITL=21
      ITITLZ='(More Than Two Groups Case)'
      NCTITZ=27
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Intercepts are Equal'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Intercepts are not Equal'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
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
      ITEXT(ICNT)='Equal Intercepts Test Statistic Value:'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=STATV3
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=STATC3
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL3
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO6210I=1,NUMROW
        NTOT(I)=15
 6210 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='62A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='62B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9='H0: Intercepts Are Equal'
      NCTIT9=24
      ITITLE='Conclusions (Upper-Tailed F Test)'
      NCTITL=33
C
      DO6230J=1,NUMCLI
        DO6240I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 6240   CONTINUE
 6230 CONTINUE
C
      ICNT=NUMALP
      AMAT(1,3)=CV380
      AMAT(2,3)=CV390
      AMAT(3,3)=CV395
      AMAT(4,3)=CV399
C
      DO6260J=1,NUMALP
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
        AMAT(J,1)=0.0
        AMAT(J,2)=STATV3
        IVALUE(J,4)(1:6)='REJECT'
        NCVALU(J,4)=6
        IF(STATV3.LT.AMAT(J,3))IVALUE(J,4)(1:6)='ACCEPT'
 6260 CONTINUE
C
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
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEQS2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEQS3(Y,X,TAG,N,MAXNXT,
     1                  TEMP1,XTEMP,YTEMP,
     1                  SLOPE,AINTER,RESVAR,Q,QYX,QY,QXY,NSIZE,
     1                  NUMSLO,ICASE,
     1                  STATVA,STATCD,PVAL2T,
     1                  STATV1,STATC1,PVAL1,
     1                  STATV2,STATC2,PVAL2,
     1                  STATV3,STATC3,PVAL3,
     1                  CV80,CV90,CV95,CV99,
     1                  CV180,CV190,CV195,CV199,
     1                  CV280,CV290,CV295,CV299,
     1                  CV380,CV390,CV395,CV399,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT A TEST FOR EQUAL SLOPES
C              OF LINEAR FITS FOR K GROUPS.
C     EXAMPLE--EQUAL SLOPES TEST Y X TAG
C              WHERE TAG IS A GROUP-ID VARIABLE (I.E., IF THERE
C              ARE K GROUPS, THEN THERE WILL BE K REGRESSIONS
C              FIT AND WE TEST THE EQUALITY OF THE SLOPES FOR
C              THE K REGRESSIONS.
C     REFERENCE--LOTHAR SACHS (1982), "APPLIED STATISTICS: A
C                HANDBOOK OF TECHNIQUES", SPRINGER-VERLAG, PP. 440-442.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/10
C     ORIGINAL VERSION--OCTOBER   2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*12 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
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
C
C---------------------------------------------------------------------
C
      PARAMETER (MAXGRP=40)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION TAG(*)
      DIMENSION TEMP1(*)
      DIMENSION XTEMP(*)
      DIMENSION YTEMP(*)
C
      DOUBLE PRECISION SLOPE(*)
      DOUBLE PRECISION AINTER(*)
      DOUBLE PRECISION RESVAR(*)
      DOUBLE PRECISION Q(*)
      DOUBLE PRECISION QYX(*)
      DOUBLE PRECISION QY(*)
      DOUBLE PRECISION QXY(*)
      INTEGER   NSIZE(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEQ'
      ISUBN2='S3  '
      IERROR='NO'
      IWRITE='OFF'
      ICASE='NULL'
C
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPEQS3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),X(I),TAG(I)
   57     FORMAT('I,Y(I),X(I),TAG(I) = ',I8,3G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      HOLD=Y(1)
      DO1135I=2,N
      IF(Y(I).NE.HOLD)GOTO1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR FROM EQUAL SLOPES TEST--')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
 1139 CONTINUE
C
      HOLD=X(1)
      DO1235I=2,N
      IF(X(I).NE.HOLD)GOTO1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)HOLD
 1231 FORMAT('      THE INDEPENDENT VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
 1239 CONTINUE
C
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DISTIN(TAG,N,IWRITE,TEMP1,NGROUP,IBUGA3,IERROR)
C
      IF(NGROUP.LT.2 .OR. NGROUP.GT.MAXGRP)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1131)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1241)
 1241   FORMAT('      THE NUMBER OF GROUPS IS LESS THAN TWO OR')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1243)MAXGRP
 1243   FORMAT('      GREATER THAN ',I5)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1245)NGROUP
 1245   FORMAT('      THE NUMBER OF GROUPS = ',I5)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************
C               **  STEP 2--                **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR EQUAL SLOPES TEST   **
C               ******************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EQS3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     COMPUTE LINEAR FITS FOR EACH GROUP
C
      NUMSLO=0
      DO2000K=1,NGROUP
        HOLD=TEMP1(K)
        ICNT=0
        DO2010J=1,N
          IF(TAG(J).EQ.HOLD)THEN
            ICNT=ICNT+1
            YTEMP(ICNT)=Y(J)
            XTEMP(ICNT)=X(J)
          ENDIF
 2010   CONTINUE
        NTEMP=ICNT
        IF(NTEMP.GE.3)THEN
          CALL LINFIT(YTEMP,XTEMP,NTEMP,
     1                ALPHA,BETA,XRESSD,XRESDF,CCXY,SDALPH,SDBETA,
     1                CCALBE,
     1                ISUBRO,IBUGA3,IERROR)
          NUMSLO=NUMSLO+1
          SLOPE(NUMSLO)=BETA
          AINTER(NUMSLO)=ALPHA
          RESVAR(NUMSLO)=XRESSD**2
          CALL SSQMEA(XTEMP,NTEMP,IWRITE,XSSQ,IBUGA3,ISUBRO,IERROR)
          Q(NUMSLO)=XSSQ
          CALL SSQMEA(YTEMP,NTEMP,IWRITE,YSSQ,IBUGA3,ISUBRO,IERROR)
          QY(NUMSLO)=YSSQ
          QXY(NUMSLO)=Q(NUMSLO)*SLOPE(NUMSLO)
          NSIZE(NUMSLO)=NTEMP
          DSUM1=0.0D0
          DO2020I=1,NTEMP
            YPRED=ALPHA + BETA*XTEMP(I)
            DSUM1=DSUM1 + DBLE(YTEMP(I)-YPRED)**2
 2020     CONTINUE
          QYX(NUMSLO)=DSUM1
        ELSE
          WRITE(ICOUT,2011)
 2011     FORMAT('***** TEST OF SLOPES WARNING--')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,2013)K
 2013     FORMAT('      GROUP ',I5,' HAS FEWER THAN THAN THREE ',
     1           'POINTS AND WILL BE EXCLUDED FROM THE TEST.')
          CALL DPWRST('XXX','WRIT')
        ENDIF
 2000 CONTINUE
C
C     3 CASES:
C
C       1) 2 GROUPS, EQUAL RESIDUAL VARIANCES
C       2) 2 GROUPS, UNEQUAL RESIDUAL VARIANCES
C       3) > 2 GROUPS
C
      IF(NUMSLO.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1131)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2031)
 2031   FORMAT('      FEWER THAN TWO GROUPS HAVE AT LEAST TWO POINTS.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ELSEIF(NUMSLO.EQ.2)THEN
C
C       STEP 1: CHECK RATIO OF RESIDUAL VARIANCES TO DETERMINE
C               WHETHER TO USE "EQUAL" OR "UNEQUAL" RESIDUAL
C               VARIANCE TEST.
C
C               REFERENCE TAKES ABSOLUTE VALUE OF DIFFERENCES
C               BETWEEEN SLOPES. THIS ESSENTIALLY MAKES IT AN
C               UPPER TAILED TEST SINCE THE TEST STATISTIC IS
C               ALWAYS POSITIVE.  WE LEAVE THE SIGN INTACT
C               (NO ABSOLUTE VALUE) IN ORDER TO MAKE IT A
C               TWO-TAILED TEST.
C
        AN1=REAL(NSIZE(1))
        AN2=REAL(NSIZE(2))
CCCCC   TERM1=ABS(SLOPE(1) - SLOPE(2))
        TERM1=SLOPE(1) - SLOPE(2)
C
        RATIO=RESVAR(1)/RESVAR(2)
        NUMDF1=NSIZE(1)-2
        NUMDF2=NSIZE(2)-2
        ALP=0.90
        CALL FPPF(ALP,NUMDF1,NUMDF2,FCV)
        IF(RATIO.LE.FCV)THEN
C
C         EQUAL VARIANCES CASE
C
          ICASE='2EQUALVAR'
          TERM2=RESVAR(1)*(AN1-2.0) + RESVAR(2)*(AN2-2.0)
          TERM3=AN1+AN2-4.0
          TERM4=(1.0/Q(1)) + (1.0/Q(2))
          STATVA=TERM1/SQRT((TERM2/TERM3)*TERM4)
          ADF=AN1 + AN2 - 4.0
C
          ALP2S=0.9
          CALL TPPF(ALP2S,ADF,CV80)
          ALP2S=0.95
          CALL TPPF(ALP2S,ADF,CV90)
          ALP2S=0.975
          CALL TPPF(ALP2S,ADF,CV95)
          ALP2S=0.995
          CALL TPPF(ALP2S,ADF,CV99)
C
          CALL TCDF(STATVA,ADF,STATCD)
          PVALLT=STATCD
          PVALUT=1.0 - STATCD
          IF(STATVA.LE.0.0)THEN
            PVAL2T=2.0*PVALLT
          ELSE
            PVAL2T=2.0*PVALUT
          ENDIF
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS3')THEN
            WRITE(ICOUT,2091)TERM1,TERM2,TERM3,TERM4
 2091       FORMAT('TERM1,TERM2,TERM3,TERM4 = ',4G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
        ELSE
C
C         UNEQUAL VARIANCES CASE
C
          ICASE='2NEVAR'
          TERM2=(RESVAR(1)/Q(1)) + (RESVAR(2)/Q(2))
          STATVA=TERM1/TERM2
          IF(NSIZE(1).GE.20 .AND. NSIZE(2).GE.20)THEN
            ALP2S=0.975
            CALL NORPPF(ALP2S,CV95)
C
            ALP2S=0.9
            CALL NORPPF(ALP2S,CV80)
            ALP2S=0.95
            CALL NORPPF(ALP2S,CV90)
            ALP2S=0.975
            CALL NORPPF(ALP2S,CV95)
            ALP2S=0.995
            CALL NORPPF(ALP2S,CV99)
C
            CALL NORCDF(STATVA,STATCD)
            PVALLT=STATCD
            PVALUT=1.0 - STATCD
            IF(STATVA.LE.0.0)THEN
              PVAL2T=2.0*PVALLT
            ELSE
              PVAL2T=2.0*PVALUT
            ENDIF
          ELSE
            ICASE='2NEVARSS'
C
C           DEGREES OF FREEDOM
C
            TERM3=(RESVAR(1)/Q(1)) + (RESVAR(2)/Q(2))
            IF(NSIZE(1).LE.NSIZE(2))THEN
              TERM2=RESVAR(1)/Q(1)
              C=TERM2/TERM3
              TERM4=C**2/(AN1-2.0)
              TERM5=(1.0 - C)**2/(AN2-2.0)
            ELSE
              TERM2=RESVAR(2)/Q(2)
              C=TERM2/TERM3
              TERM4=C**2/(AN2-2.0)
              TERM5=(1.0 - C)**2/(AN1-2.0)
            ENDIF
            ADF=1.0/(TERM4 + TERM5)
C
            ALP2S=0.9
            CALL TPPF(ALP2S,ADF,CV80)
            ALP2S=0.95
            CALL TPPF(ALP2S,ADF,CV90)
            ALP2S=0.975
            CALL TPPF(ALP2S,ADF,CV95)
            ALP2S=0.995
            CALL TPPF(ALP2S,ADF,CV99)
C
            CALL TCDF(STATVA,ADF,STATCD)
            PVALLT=STATCD
            PVALUT=1.0 - STATCD
            IF(STATVA.LE.0.0)THEN
              PVAL2T=2.0*PVALLT
            ELSE
              PVAL2T=2.0*PVALUT
            ENDIF
          ENDIF
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS3')THEN
            WRITE(ICOUT,2096)TERM1,TERM2,TERM3,TERM4,TERM5,C
 2096       FORMAT('TERM1,TERM2,TERM3,TERM4,TERM5,C = ',6G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
        ENDIF
      ELSE
C
C       MORE THAN TWO GROUPS.  THIS ACTUALLY INVOLVES 3 TESTS:
C
C          1. TEST THAT THE REGRESSIONS ARE EQUAL
C          2. IF THE REGRESSIONS ARE NOT EQUAL, THEN
C             TEST THAT THE SLOPES ARE EQUAL.
C          3. IF THE SLOPES ARE EQUAL, THEN TEST THAT THE
C             INTERCEPTS ARE EQUAL.
C
C       WE WILL SAVE 3 SETS OF TEST STATISTICS, CDF VALUES, AND
C       P-VALUES AND LET THE CALLING PROGRAM DETERMINE WHICH
C       IS OF INTEREST.
C
C       TEST 1: TEST FOR "EQUAL REGRESSIONS".
C
C       STEP 1 IS TO COMPUTE THE FIT FOR THE FULL DATA SET.  INCLUDE
C       ALL THE DATA, EVEN GROUPS THAT MIGHT HAVE LESS THAN 3 DATA
C       POINTS.
C
        ICASE='3ORMORE'
        CALL LINFIT(Y,X,N,
     1              ALPHA,BETA,XRESSD,XRESDF,CCXY,SDALPH,SDBETA,
     1              CCALBE,
     1              ISUBRO,IBUGA3,IERROR)
        DSUM1=0.0D0
        DO3020I=1,NTEMP
          YPRED=ALPHA + BETA*X(I)
          DSUM1=DSUM1 + DBLE(Y(I)-YPRED)**2
 3020   CONTINUE
        QYXT=DSUM1
C
        CALL SUMDP(QYX,NUMSLO,IWRITE,YXSUM,IBUGA3,IERROR)
        TERM1=QYXT - YXSUM
        AK=REAL(NUMSLO)
        AN=REAL(N)
        C1=1.0/(2.0*AK - 2.0)
        C2=1.0/(AN - 2.0*AK)
        STATV1=C1*TERM1/(C2*YXSUM)
        NUMDF1=2*NUMSLO - 2
        NUMDF2=N - 2*NUMSLO
C
        ALP=0.80
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV180)
        ALP=0.90
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV190)
        ALP=0.95
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV195)
        ALP=0.99
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV199)
C
        CALL FCDF(STATV1,NUMDF1,NUMDF2,STATC1)
        PVAL1=1.0 - STATC1
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS3')THEN
          WRITE(ICOUT,3091)QYXT,XSUM,TERM1,AK,AN
 3091     FORMAT('QYXT,XSUM,TERM1,AK,AN = ',5G15.7)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3093)STATV1,STATC1,PVAL1
 3093     FORMAT('STATV1,STATC1,PVAL1 = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
C       TEST 2: TEST FOR "EQUAL SLOPES".
C
CCCCC   CALL SUMDP(QY,NUMSLO,IWRITE,YSUM,IBUGA3,IERROR)
CCCCC   CALL SUMDP(Q,NUMSLO,IWRITE,XSUM,IBUGA3,IERROR)
CCCCC   CALL SUMDP(QXY,NUMSLO,IWRITE,XYSUM,IBUGA3,IERROR)
        DSUM1=0.0D0
        DSUM2=0.0D0
        DSUM3=0.0D0
        DO3101II=1,NUMSLO
          DSUM1=DSUM1 + QY(II)
          DSUM2=DSUM2 + Q(II)
          DSUM3=DSUM3 + QXY(II)
 3101   CONTINUE
        YSUM=DSUM1
        XSUM=DSUM2
        XYSUM=DSUM3
C
        A=YSUM - (XYSUM/XSUM)
        C1=1.0/(AK - 1.0)
        C2=1.0/(AN - 2.0*AK)
        ANUM=C1*(A - YXSUM)
        DENOM=C2*YXSUM
        STATV2=ANUM/DENOM
        NUMDF1=NUMSLO - 1
        NUMDF2=N - 2*NUMSLO
C
        ALP=0.80
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV280)
        ALP=0.90
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV290)
        ALP=0.95
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV295)
        ALP=0.99
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV299)
C
        CALL FCDF(STATV2,NUMDF1,NUMDF2,STATC2)
        PVAL2=1.0 - STATC2
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS3')THEN
          WRITE(ICOUT,3096)STATV2,STATC2,PVAL2
 3096     FORMAT('STATV2,STATC2,PVAL2 = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
C       TEST 3: TEST FOR "EQUAL INTERCEPTS".
C
        ANUM=C1*(QYXT - A)
        DENOM=C2*YXSUM
        STATV3=ANUM/DENOM
        NUMDF1=NUMSLO - 1
        NUMDF2=N - 2*NUMSLO
C
        ALP=0.80
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV380)
        ALP=0.90
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV390)
        ALP=0.95
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV395)
        ALP=0.99
        CALL FPPF(ALP,NUMDF1,NUMDF2,CV399)
C
        CALL FCDF(STATV3,NUMDF1,NUMDF2,STATC3)
        PVAL3=1.0 - STATC3
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS3')THEN
          WRITE(ICOUT,3097)STATV3,STATC3,PVAL3
 3097     FORMAT('STATV3,STATC3,PVAL3 = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
      ENDIF
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EQS3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEQS3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)STATVA,STATCD,CV95,PVAL2T,NUMSLO,N
 9012   FORMAT('STATVA,STATCD,CV95,PVAL2T,NUMSLO,N = ',4G15.7,2I8)
        CALL DPWRST('XXX','WRIT')
        IF(NUMSLO.GE.1)THEN
          DO9015I=1,NUMSLO
            WRITE(ICOUT,9014)I,SLOPE(I),RESVAR(I),Q(I),QYX(I),NSIZE(I)
 9014       FORMAT('I,SLOPE(I),RESVAR(I),Q(I),QYX(I),NSIZE(I) = ',
     1             I5,4G15.7,I5)
            CALL DPWRST('XXX','WRIT')
 9015     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPERAS(IHARG,IARGT,IARG,NUMARG,
     1                  IBACCO,IGRASW,IDIASW,
     1                  PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1                  PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1                  NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IDNVOF,IDNHOF,IDFONT,PDSCAL,
     1                  ICAPSW,IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT AN IMMEDIATE ERASE OF THE SCREEN FOR DISPLAY TERMINALS
C              (OR SKIP TO A NEW PAGE FOR PAPER-OUTPUT TERMINALS AND THE BATCH
C              HIGH-SPEED PRINTER)
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --IARG   (AN INTEGER VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--IFOUND ('YES' OR 'NO')
C                     --IERROR ('YES' OR 'NO' )
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
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --MAY       1989. IDNVOF/HOF ADDED TO INPUT ARGS (ALAN)
C                                       TO FIX POSTSCRIPT SCALING PROBLEM
C     UPDATED         --MARCH     1990. PATCH FOR X11 (CHECK PICTURE POINTS)
C     UPDATED         --MAY       1992. AUTO CLOSE/OPEN OF DEVICE 3 (JJF)
C     UPDATED         --MAY       1992. DEBUG STATEMENTS
C     UPDATED         --MAY       1992. IBUGXX, ISUBXX, IERRXX
C     UPDATED         --NOVEMBER  1996. QWIN, BUG FIX
C     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
C     UPDATED         --SEPTEMBER 2002. ICAPSW
C     UPDATED         --NOVEMBER  2015. OPTION AS TO WHETHER DEVICE 3 IS
C                                       HANDLED AUTOMATICALLY BY
C                                       DATAPLOT OR BY THE USER
C     UPDATED         --DECEMBER  2018. CHECK FOR DISCRETE, NULL, OR
C                                       NONE DEVICE
C     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
C                                       COMMAND
C
C-----NON-COMMON VARIABLES----------------------------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IBACCO
      CHARACTER*4 ICAPSW
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
CCCCC ADD FOLLOWING LINE MARCH 1997.
      CHARACTER*4 IDFONT
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICOPSJ
      CHARACTER*4 IBUGXX
      CHARACTER*4 ISUBXX
      CHARACTER*4 IERRXX
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
C
      DIMENSION IDMANU(*)
      DIMENSION IDMODE(*)
      DIMENSION IDMOD2(*)
      DIMENSION IDMOD3(*)
C
      DIMENSION IDPOWE(*)
      DIMENSION IDCONT(*)
      DIMENSION IDCOLO(*)
CCCCC ADD FOLLOWING LINE MARCH 1997.
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
C
CCCCC THE FOLLOWING 2 LINES WERE ADDED               MAY 1989
CCCCC TO FIX POSTSCRIPT TRANSLATION PROBLEM (ALAN)   MAY 1989
      DIMENSION IDNVOF(*)
      DIMENSION IDNHOF(*)
      DIMENSION PDSCAL(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
CCCCC THE FOLLOWING LINE WAS ADDED    MAY 1992 (JJF)
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPER'
      ISUBN2='SC  '
      IFOUND='NO'
      IERROR='NO'
C
      IBUGG4=IBUGD2
      ISUBG4=ISUBRO
      IERRG4=IERROR
C
      IF(IBUGD2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPERAS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,IBUGG4,IFOUND,IERROR,IPL2CS
   53   FORMAT('IBUGD2,IBUGG4,IFOUND,IERROR,IPL2CS = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)NUMARG,NUMDEV,IDEVO3,IDV2SP,IDIASW
   60   FORMAT('NUMARG,NUMDEV,IDEVO3,IDV2SP,IDIASW = ',2I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        DO61I=1,NUMARG
          WRITE(ICOUT,62)I,IHARG(I),IARGT(I),IARG(I)
   62     FORMAT('I,IHARG(I),IARGT(I),IARG(I) = ',I8,2(2X,A4),2X,I8)
          CALL DPWRST('XXX','BUG ')
   61   CONTINUE
        DO71I=1,NUMDEV
          WRITE(ICOUT,72)I,IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   72     FORMAT('I,IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',
     1           I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,73)I,IDPOWE(I),IDCONT(I),IDCOLO(I)
   73     FORMAT('I,IDPOWE(I),IDCONT(I),IDCOLO(I) = ',I8,3(2X,A4))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,74)I,IDNVPP(I),IDNHPP(I),IDUNIT(I),
     1                   IDNVOF(I),IDNHOF(I)
   74     FORMAT('I,IDNVPP(I),IDNHPP(I),IDUNIT(I),'
     1           'IDNVOP(I),IDNHOP(I) = ',6I8)
          CALL DPWRST('XXX','BUG ')
   71   CONTINUE
        WRITE(ICOUT,82)IMANUF,IMODEL,IMODE2,IMODE3,IGCONT,IGCOLO
   82   FORMAT('IMANUF,IMODEL,IMODE2,IMODE3,IGCONT,IGCOLO = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,84)NUMVPP,NUMHPP,ANUMVP,ANUMHP
   84   FORMAT('NUMVPP,NUMHPP,ANUMVP,ANUMHP = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  EXTRACT NEEDED INFORMATION FROM THE COMMAND LINE  **
C               ********************************************************
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DELA')GOTO9000
C
      IFOUND='YES'
C
CCCCC THE FOLLOWING 7 LINES WERE ADDED           MAY 1992 (JJF)
CCCCC TO AUTOMATICALLY CLOSE/OPEN DEVICE 3    MAY 1992
CCCCC WHENEVER AN INITIALIZATION/ERASE IS DONE   MAY 1992
CCCCC (SEE ALSO DPERAS AND MAINOD)               MAY 1992
C
C     2015/11: "SET DEVICE 3 <AUTOMATIC/USER>" COMMAND TO SPECIFY
C              WHETHER DEVICE 3 IS AUTOMATICALLY OPENED/CLOSED BY
C              DATAPLOT.
C
C     2015/12: "SET DEVICE 2 SPLIT <ON/OFF>" COMMAND TO SPECIFY
C              WHETHER DEVICE 2 IS AUTOMATICALLY SAVED TO ONE FILE
C              OR EACH GRAPH IS SAVED TO A SEPARATE FILE.
C
      IF(IDEVO3.EQ.'AUTO')THEN
        IBUGXX=IBUGG4
        ISUBXX=ISUBG4
        IERRXX=IERRG4
        IF(IPL2CS.EQ.'OPEN')THEN
          CALL DPDEV(3,'CLOS','POST',ICAPSW,IBUGXX,ISUBXX,IERRXX)
        ELSEIF(IPL2CS.EQ.'CLOSED')THEN
          CALL DPDEV(3,'OPEN','POST',ICAPSW,IBUGXX,ISUBXX,IERRXX)
        ENDIF
      ENDIF
C
      IF(IDV2SP.EQ.'ON' .AND. IDPOWE(2).EQ.'ON')THEN
        IBUGXX=IBUGG4
        ISUBXX=ISUBG4
        IERRXX=IERRG4
        IF(IPL1CS.EQ.'OPEN')THEN
          CALL DPDEV(2,'CLOS','POST',ICAPSW,IBUGXX,ISUBXX,IERRXX)
        ELSEIF(IPL1CS.EQ.'CLOSED')THEN
          CALL DPDEV(2,'OPEN','POST',ICAPSW,IBUGXX,ISUBXX,IERRXX)
        ENDIF
      ENDIF
C
C               ********************************
C               **  STEP 2--                  **
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
        IGUNIT=IDUNIT(IDEVIC)
        PCHSCA=PDSCAL(IDEVIC)
C
C               *****************************************
C               **  STEP 2.1--                         **
C               **  TREAT THE ERASE CASE FOR PRINTERS  **
C               **  AND DISCRETE TERMINALS             **
C               **  (SKIP TO NEXT PAGE)            ZZ  **
C               *****************************************
C
        ISTEPN='2.1'
        IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IGCONT.EQ.'OFF')THEN
          WRITE(ICOUT,998)
  998     FORMAT(1H1)
          CALL DPWRST('XXX','BUG ')
          GOTO8000
        ENDIF
C
C               ****************************************
C               **  STEP 2.2--                        **
C               **  TREAT THE ERASE CASE              **
C               **  FOR CONTINUOUS TERMINALS.         **
C               ****************************************
C
        ISTEPN='2.2'
        IF(IBUGD2.EQ.'ON')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,1205)
 1205     FORMAT('*** FROM DPERAS--AN ERASE SHOULD TAKE PLACE NOW ***')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        CALL DPERSC(IBACCO)
CCCCC   FOLLOWING LINES ADDED FOR X11 (PICTURE POINTS MAY BE DYNAMICALLY
CCCCC   CHANGED BY ERASE SCREEN ROUTINE, MAKE SURE HAVE UP-TO-DATE VALUES).
CCCCC   FIX BUG IN FOLLOWING   NOVEMBER 1996.
        IF(IMANUF.EQ.'X11')THEN
          NUMVPP=INT(ANUMVP+0.5)
          NUMHPP=INT(ANUMHP+0.5)
CCCCC     IDNVPP(I)=NUMVPP
CCCCC     IDNHPP(I)=NUMHPP
          IDNVPP(IDEVIC)=NUMVPP
          IDNHPP(IDEVIC)=NUMHPP
        ENDIF
CCCCC   END CHANGE
CCCCC   FOLLOWING LINES ADDED FOR QWIN (PICTURE POINTS MAY BE DYNAMICALLY
CCCCC   CHANGED BY ERASE SCREEN ROUTINE, MAKE SURE HAVE UP-TO-DATE
CCCCC   VALUES).  NOVEMBER 1996.
        IF(IMANUF.EQ.'QWIN')THEN
          NUMVPP=INT(ANUMVP+0.5)
          NUMHPP=INT(ANUMHP+0.5)
          IDNVPP(IDEVIC)=NUMVPP
          IDNHPP(IDEVIC)=NUMHPP
        ENDIF
C
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1206)
 1206     FORMAT('*** AN ERASE SHOULD HAVE JUST TAKEN PLACE ***')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ************************************
C               **  STEP 2.2B--                   **
C               **  CARRY OUT CLOSING OPERATIONS  **
C               **  ON THE GRAPHICS DEVICES       **
C               ************************************
C
        ICOPSJ='OFF'
        NUMCOJ=0
        CALL DPCLPL(ICOPSJ,NUMCOJ,
     1              PGRAXF,PGRAYF,
     1              IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1              PDIAHE,PDIAWI,PDIAVG,PDIAHG)
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
      IF(IBUGD2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DPERAS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IFOUND,IERROR,IPL1CS,IPL2CS
 9014   FORMAT('IFOUND,IERROR,IPL1CS,IPL2CS = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPERBA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICONT,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE AN ERROR BAR PLOT
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/11
C     ORIGINAL VERSION--OCTOBER   1988.
C     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR5
C     UPDATED         --JULY      2019. USE DPCOZZ INSTEAD OF DPCOZ2
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
      DIMENSION Z1(MAXOBV)
      DIMENSION Z2(MAXOBV)
      DIMENSION Z3(MAXOBV)
      DIMENSION Z4(MAXOBV)
      DIMENSION Z5(MAXOBV)
      DIMENSION Z6(MAXOBV)
CCCCC FOLLOWING LINES ADDED FEBRUARY, 1994
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Z1(1))
      EQUIVALENCE (GARBAG(IGARB2),Z2(1))
      EQUIVALENCE (GARBAG(IGARB3),Z3(1))
      EQUIVALENCE (GARBAG(IGARB4),Z4(1))
      EQUIVALENCE (GARBAG(IGARB5),Z5(1))
      EQUIVALENCE (GARBAG(IGARB6),Z6(1))
CCCCC END CHANGE
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPER'
      ISUBN2='BA  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               *************************************
C               **  TREAT THE ERROR BAR PLOT CASE  **
C               *************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ERBA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPERBA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,ICONT
   53   FORMAT('ICASPL,IAND1,IAND2,ICONT = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 11--            **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ERBA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASPL='ERBA'
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'BAR'.AND.
     1  IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ERBA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='ERROR BAR PLOT'
      MINNA=2
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ERBA')THEN
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
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Z1,Z2,Z3,Z4,Z5,Z6,Z6,NLOCAL,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 31--                                       **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS           **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.              **
C               **  DEFINE THE VECTOR D(.) TO 1'S, 2'S, AND 3'S     **
C               **  FOR THE PLOTTED VALUE, THE LOWER CONFIDENCE     **
C               **  LINE, AND THE UPPER CONFIDENCE LINE.            **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).   **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).   **
C               ******************************************************
C
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ERBA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPERB2(Z1,Z2,Z3,Z4,Z5,Z6,NLOCAL,NUMVAR,ICASPL,ICONT,
     1            Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ERBA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPERBA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)NLOCAL,NUMV2,NPLOTP
 9041   FORMAT('NLOCAL,NUMV2,NPLOTP = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NLOCAL.GE.1)THEN
          DO9042I=1,NLOCAL
            WRITE(ICOUT,9043)I,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I)
 9043       FORMAT('I,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I) = ',I8,6E10.3)
            CALL DPWRST('XXX','BUG ')
 9042     CONTINUE
        ENDIF
        IF(NPLOTP.GE.1)THEN
          DO9052I=1,NPLOTP
            WRITE(ICOUT,9053)I,Y(I),X(I),D(I)
 9053       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9052     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPERB2(Z1,Z2,Z3,Z4,Z5,Z6,N,NUMV2,ICASPL,ICONT,
     1Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN ERROR BAR PLOT
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/12
C     ORIGINAL VERSION--DECEMBER  1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONT
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION Z1(*)
      DIMENSION Z2(*)
      DIMENSION Z3(*)
      DIMENSION Z4(*)
      DIMENSION Z5(*)
      DIMENSION Z6(*)
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
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'ERB2')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPERB2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGG3,ISUBRO,IERROR
   52 FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICASPL,ICONT
   53 FORMAT('ICASPL,ICONT = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMV2
   54 FORMAT('NUMV2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)N
   61 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO62I=1,N
      WRITE(ICOUT,63)I,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I)
   63 FORMAT('I,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I) = ',I8,6E10.3)
      CALL DPWRST('XXX','BUG ')
   62 CONTINUE
   90 CONTINUE
C
      NUMCPL=11
      J=0
      JD=0
C
      DO1100I=1,N
C
      YMID=Z1(I)
C
      YMAX=YMID+Z2(I)
C
      IF(NUMV2.LE.3)YMIN=YMID-Z2(I)
      IF(NUMV2.GE.4)YMIN=YMID-Z3(I)
C
      IF(NUMV2.LE.2)XMID=I
      IF(NUMV2.EQ.3)XMID=Z3(I)
      IF(NUMV2.GE.4)XMID=Z4(I)
C
      IF(NUMV2.LE.4)XLEF=XMID
      IF(NUMV2.GE.5)XLEF=XMID-Z5(I)
C
      IF(NUMV2.LE.4)XRIG=XMID
      IF(NUMV2.EQ.5)XRIG=XMID+Z5(I)
      IF(NUMV2.EQ.6)XRIG=XMID+Z6(I)
C
      CALL DPCHLI(ICONT,NUMCPL,YMID,YMID,XMID,XMID,J,JD,Y2,X2,D2,IERROR)
      CALL DPCHLI(ICONT,NUMCPL,YMAX,YMAX,XMID,XMID,J,JD,Y2,X2,D2,IERROR)
      CALL DPCHLI(ICONT,NUMCPL,YMIN,YMIN,XMID,XMID,J,JD,Y2,X2,D2,IERROR)
      CALL DPCHLI(ICONT,NUMCPL,YMID,YMID,XLEF,XLEF,J,JD,Y2,X2,D2,IERROR)
      CALL DPCHLI(ICONT,NUMCPL,YMID,YMID,XRIG,XRIG,J,JD,Y2,X2,D2,IERROR)
      CALL DPCHLI(ICONT,NUMCPL,YMAX,YMIN,XMID,XMID,J,JD,Y2,X2,D2,IERROR)
      CALL DPCHLI(ICONT,NUMCPL,YMID,YMID,XLEF,XRIG,J,JD,Y2,X2,D2,IERROR)
C
 1100 CONTINUE
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
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'ERB2')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPERB2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG3,ISUBRO,IERROR
 9012 FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICASPL,ICONT
 9013 FORMAT('ICASPL,ICONT = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMV2
 9014 FORMAT('NUMV2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)N2,NPLOTV
 9021 FORMAT('N2,NPLOTV = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9022I=1,N2
      WRITE(ICOUT,9023)I,Y2(I),X2(I),D2(I)
 9023 FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3E10.3)
      CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPERDE(IHARG,IARGT,ARG,NUMARG,DEFERD,
     1ERASDE,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE ERASE DELAY FACTOR.
C              THE SPECIFIED ERASE DELAY FACTOR WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE ERASDE.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFERD (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--ERASDE (A  FLOATING POINT VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     ORIGINAL VERSION--NOVEMBER 1980.
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
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DELA')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'DELA')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPERDE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR ERASE DELAY ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE THE THE ANALYST WISHES TO DOUBLE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      THE DELAY TIME WHILE SCREEN ERASURES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      ARE BEING CARRIED OUT, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      ERASE DELAY 2 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFERD
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
CCCCC ERASDE=HOLD
      AIMAX=2**(NUMBPC*NUMCPW-2)
      IF(HOLD.LT.AIMAX)ERASDE=HOLD
      IF(HOLD.GE.AIMAX)ERASDE=AIMAX
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ERASDE
 1181 FORMAT('THE ERASE DELAY FACTOR HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPERRO(IERRFA,IANSLC,IWIDTH,IGUIFL,
     1                  ISUBN1,ISUBN2,ICASAN,
     1                  IBUGA2,ISUBRO,IERROR)
C
C     PURPOSE--IF ERROR RETURNED FROM A COMMAND, THEN DO ONE
C              OF THE FOLLOWING:
C
C                 1) NOTHING (I.E., JUST RESUME PROCESSING THE
C                    NEXT COMMAND).
C
C                 2) PROMPT WHETHER YOU WANT TO CONTINUE PROCESSING
C                    OR TO TERMINATE DATAPLOT SESSION.
C
C                 3) TERMINATE DATAPLOT SESSION IMMEDIATELY.
C               
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (2014)
C     VERSION NUMBER--2014/2
C     ORIGINAL VERSION--FEBRUARY  2014.
C     UPDATED         --OCTOBER   2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IERRFA
      CHARACTER*4 IGUIFL
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASAN
      CHARACTER*4 IBUGA2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*1 IAJUNK
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOF2.INC'
      CHARACTER (LEN=MAXFNC) :: IMANAM(10)
      COMMON/IMAC/IMACNU,IMALEV,IMANAM
C
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ERRO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPERRO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IERROR,IERRFA,IGUIFL,ISUBN1,ISUBN2,ICASAN
   53   FORMAT('IERROR,IERRFA,IGUIFL,ISUBN1,ISUBN2,ICASAN = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERRFA.EQ.'IGNO' .OR. IGUIFL.EQ.'ON')GOTO9009
      IF(IERRFA.EQ.'TERM' .AND. IERROR.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9001)ISUBN1,ISUBN2
 9001   FORMAT('***** ERROR ENCOUNTERED IN: ',2A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9002)ICASAN
 9002   FORMAT('      ICASE = ',A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9003)(IANSLC(II:II),II=1,MIN(IWIDTH,120))
 9003   FORMAT('      COMMAND = ',120A1)
        CALL DPWRST('XXX','WRIT')
C
        IF(ICRENA.NE.'-999')THEN
          WRITE(ICOUT,9012)ICRENA
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
        WRITE(ICOUT,9004)
 9004   FORMAT('      DATAPLOT EXITING.')
        CALL DPWRST('XXX','WRIT')
        STOP
      ELSEIF(IERRFA.EQ.'PROM')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9001)ISUBN1,ISUBN2
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9002)ICASAN
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9003)(IANSLC(II:II),II=1,MIN(IWIDTH,120))
        CALL DPWRST('XXX','WRIT')
C
        IF(IMACNU.NE.5)THEN
          DO8010I=1,IMALEV
CCCCC       WRITE(ICOUT,9012)ICRENA
            WRITE(ICOUT,9012)IMANAM(I)
 9012       FORMAT('      CALLED FROM: ',A80)
            CALL DPWRST('XXX','WRIT')
 8010     CONTINUE
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9005)
 9005   FORMAT('      ENTER THE FOLLOWING:')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9006)
 9006   FORMAT('      1 - EXIT DATAPLOT')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9007)
 9007   FORMAT('      2 - CONTINUE RUNNING DATAPLOT')
        CALL DPWRST('XXX','WRIT')
        READ(IRD,'(A1)')IAJUNK
        IF(IAJUNK.EQ.'1' .OR. IAJUNK.EQ.'E' .OR. IAJUNK.EQ.'X' .OR.
     1     IAJUNK.EQ.'e' .OR. IAJUNK.EQ.'x')THEN
          STOP
        ENDIF
      ENDIF
C
 9009 CONTINUE
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ERRO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('AT THE END OF DPERRO')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPERSC(IBACCO)
C
C     PURPOSE--ERASE THE SCREEN
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED         --JANUARY   1989.  SEND BKGD COLOR TO GRERSC
C                                        (FOR METAFILE) (ALAN)
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 IBACCO
C
      CHARACTER*4 ICASE
      CHARACTER*4 ICOL
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ICASE='9999'
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'ERSC')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPERSC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBACCO
   52 FORMAT('IBACCO = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IMANUF,IMODEL
   53 FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IGCOLO
   54 FORMAT('IGCOLO = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4
   59 FORMAT('IBUGG4 = ',A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **********************************************
C               **  STEP 1--                                **
C               **  BRANCH TO THE COLOR OR NON-COLOR CASES  **
C               **********************************************
C
      IF(IGCOLO.EQ.'OFF')GOTO1100
      GOTO1200
C
C               ********************************
C               **  STEP 1--                  **
C               **  TREAT THE NON-COLOR CASE  **
C               ********************************
C
 1100 CONTINUE
      JCOL=0
      CALL GRERSC(JCOL,IBACCO)
      GOTO9000
C
C               ****************************************************
C               **  STEP 2--                                      **
C               **  TREAT THE COLOR CASE                          **
C               **  STEP 2.1--                                    **
C               **        TRANSLATE THE CHARACTER REPRESENTATION  **
C               **        OF THE BACKGROUND COLOR                 **
C               **        INTO A NUMERIC REPRESENTATION           **
C               **        WHICH CAN BE UNDERSTOOD BY THE          **
C               **        GRAPHICS DEVICE.                        **
C               **  STEP 2.2--                                    **
C               **        SET THE BACKGROUND COLOR                **
C               **        ON THE GRAPHICS DEVICE.                 **
C               **  STEP 2.3--                                    **
C               **        ERASE THE SCREEN                        **
C               ****************************************************
C
 1200 CONTINUE
CCCCC ICASE='REGI'
      ICASE='BACK'
      ICOL=IBACCO
      CALL GRTRCO(ICASE,ICOL,JCOL)
      CALL GRSECO(ICASE,ICOL,JCOL)
      CALL GRERSC(JCOL,ICOL)
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'ERSC')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPERSC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBACCO,ICOL,JCOL
 9012 FORMAT('IBACCO,ICOL,JCOL = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICASE
 9013 FORMAT('ICASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IMANUF,IMODEL
 9014 FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IBUGG4,ISUBG4,IERRG4
 9015 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPEXA2(Y,X,N,ITYPE,B1,K1,K2,B2,
     1                  F3,F4,NLEFT,NUMFAC,KM1,RES2,PRED2,
     1                  ICAPTY,ICAPSW,IFORSW,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--SOLVE FOR THE COEFFICIENTS FOR AN
C              EXACT FIT OF A FUNCTION OF THE FORM
C              Y = F(X) = POLYNOMIAL/POLYNOMIAL
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
C     ORIGINAL VERSION--MAY       1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --FEBRUARY  2013. USE DPDTA1 AND DPDTxx TO PRINT
C                                       OUTPUT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ITYPE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION ITYPE(*)
      DIMENSION B1(*)
      DIMENSION B2(*)
      DIMENSION F3(*)
      DIMENSION F4(*)
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
C
      DIMENSION A(25,25)
      DIMENSION A2(25,25)
      DIMENSION A3(25,25)
      DIMENSION RIGHT(25)
      DIMENSION RIGHT2(25)
      DIMENSION B3(25)
C
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=1)
      PARAMETER (MAXROW=20)
      CHARACTER*40 ITITLE
      CHARACTER*70 ITITLZ
      CHARACTER*40 ITITL9
      CHARACTER*40 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*30 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*30 IVALUE(MAXROW,NUMCLI)
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='A2  '
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EXA2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,N,K1,K2
   53   FORMAT('IBUGA3,N,K1,K2 = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        KTEMP=K1+K2
        DO55I=1,KTEMP
          WRITE(ICOUT,56)I,Y(I),X(I),ITYPE(I),B1(I)
   56     FORMAT('I,Y(.),X(.),ITYPE(.),B1(.) = ',I8,2G15.7,2X,A4,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  FORM THE MATIX FOR THE LINEAR SYSTEM  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      K1P1=K1+1
      K1P5=K1+5
      K1P6=K1+6
      K1P10=K1+10
      K1P11=K1+11
      K1P15=K1+15
      K1P16=K1+16
      K1P20=K1+20
      K1P21=K1+21
      K1PK2=K1+K2
C
      DO100J=1,K1
      DO110I=1,N
        IF(J.EQ.1)A(I,J)=1.0
        IF(J.GT.1)A(I,J)=X(I)**(J-1)
  110 CONTINUE
  100 CONTINUE
C
      DO200J=1,K2
        K1PJ=K1+J
        DO210I=1,N
          IF(J.EQ.1)A(I,K1PJ)=1.0
          IF(J.GT.1)A(I,K1PJ)=X(I)**(J-1)
          A(I,K1PJ)=-Y(I)*A(I,K1PJ)
  210   CONTINUE
  200 CONTINUE
C
      K=K1+K2
      IF(IBUGA3.EQ.'ON')THEN
        DO250I=1,N
          WRITE(ICOUT,255)(A(I,J),J=1,K)
  255     FORMAT('A(.,.) = ',10G15.7)
          CALL DPWRST('XXX','BUG ')
  250   CONTINUE
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  FORM THE RIGHT SIDE FOR THE LINEAR SYSTEM  **
C               *************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO300I=1,N
        RIGHT(I)=0.0
  300 CONTINUE
C
      DO400J=1,K
        IF(ITYPE(J).EQ.'K')THEN
          DO500I=1,N
            RIGHT(I)=RIGHT(I)-A(I,J)
  500     CONTINUE
        ENDIF
  400 CONTINUE
C
C               ***********************************************
C               **  STEP 3--                                 **
C               **  ADJUST THE MATRIX FOR THE LINEAR SYSTEM  **
C               ***********************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J2=0
      DO600J=1,K
        IF(ITYPE(J).EQ.'K')GOTO600
        J2=J2+1
        DO700I=1,N
          A2(I,J2)=A(I,J)
  700   CONTINUE
  600 CONTINUE
C
C               *********************************
C               **  STEP 4--                   **
C               **  TRIANGULARIZE THE SYSTEM,  **
C               **  THEN BACKSOLVE.            **
C               *********************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EXA2')THEN
        ISTEPN='4'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,711)N
  711   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO720I=1,N
          WRITE(ICOUT,721)(A2(I,J),J=1,N),RIGHT(I)
  721     FORMAT('A2(I,.),RIGHT(I) = ',11E10.3)
          CALL DPWRST('XXX','BUG ')
  720   CONTINUE
      ENDIF
C
      CALL TRIA25(A2,N,N,RIGHT,A3,RIGHT2,IBUGA3)
      CALL BACK25(A3,N,N,RIGHT2,B3,IBUGA3)
C
C               *****************************
C               **  STEP 5--               **
C               **  COPY THE COEFFICIENTS  **
C               *****************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IU=0
      DO800J=1,K
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,811)J,IU,ITYPE(J),B1(J),B2(J),B3(J)
  811     FORMAT('J,IU,ITYPE(J),B1(J),B2(J),B3(J) = ',2I6,2X,A4,3F10.4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ITYPE(J).EQ.'K')THEN
          B2(J)=B1(J)
        ELSE
          IU=IU+1
          B2(J)=B3(IU)
        ENDIF
  800 CONTINUE
C
C               ***************************************************
C               **  STEP 6--                                     **
C               **  IF A SECOND SET OF POINTS EXISTS             **
C               **  (THAT IS, IF VARIABLES 3 AND 4               **
C               **  HAVE BEEN SPECIFIED),                        **
C               **  THEN COMPUTE PREDICTED VALUES AND RESIDUALS  **
C               **  FOR THIS SECOND SET OF POINTS                **
C               **  BASED ON THE EXACT-FIT COEFFICIENTS          **
C               **  DERIVED FROM  THE FIRST SET OF POINTS        **
C               **  (THAT IS, FROM VARIABLES 1 AND 2).           **
C               ***************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1000I=1,NLEFT
C
        J=1
        ANUM=B2(J)
        IF(K1.LE.1)GOTO1150
        IF(NUMFAC.EQ.2)THEN
          DO1125J=2,K1
            ANUM=ANUM+B2(J)*X(I)**(J-1)
 1125     CONTINUE
        ELSEIF(NUMFAC.EQ.4)THEN
          DO1145J=2,K1
            ANUM=ANUM+B2(J)*F4(I)**(J-1)
 1145     CONTINUE
        ENDIF
C
 1150   CONTINUE
C
        J=1
        K1PJ=K1+J
        ADEN=B2(K1PJ)
        IF(K2.LE.1)GOTO1250
        IF(NUMFAC.EQ.2)THEN
          DO1225J=2,K2
            K1PJ=K1+J
            ADEN=ADEN+B2(K1PJ)*X(I)**(J-1)
 1225     CONTINUE
        ELSEIF(NUMFAC.EQ.4)THEN
          DO1245J=2,K2
            K1PJ=K1+J
            ADEN=ADEN+B2(K1PJ)*F4(I)**(J-1)
 1245     CONTINUE
        ENDIF
C
 1250   CONTINUE
C
        PRED2(I)=ANUM/ADEN
        IF(NUMFAC.EQ.2)RES2(I)=Y(I)-PRED2(I)
        IF(NUMFAC.EQ.4)RES2(I)=F3(I)-PRED2(I)
 1000 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')THEN
        DO1380I=1,NLEFT
          WRITE(ICOUT,1381)I,Y(I),X(I),F3(I),F4(I),PRED2(I),RES2(I)
 1381     FORMAT('I,Y(I),X(I),F3(I),F4(I),PRED2(I),RES2(I) = ',
     1           I8,6E10.3)
          CALL DPWRST('XXX','BUG ')
 1380   CONTINUE
      ENDIF
C
      SUM=0.0
      DO1500I=1,NLEFT
        SUM=SUM+RES2(I)**2
 1500 CONTINUE
      RESSS=SUM
      RESSD=0.0
      IRESDF=NLEFT-KM1
      RESDF=IRESDF
      IF(IRESDF.LE.0)GOTO1510
      RESV=RESSS/RESDF
      IF(RESV.GT.0.0)RESSD=SQRT(RESV)
      IF(RESV.LE.0.0)RESSD=0.0
 1510 CONTINUE
C
      ANLEFT=NLEFT
      SUM=0.0
      DO1600I=1,NLEFT
        SUM=SUM+ABS(RES2(I))
 1600 CONTINUE
      RESMA=SUM/ANLEFT
C
      AMAXR=RES2(1)
      AMINR=RES2(1)
      DO1700I=1,NLEFT
        IF(RES2(I).GT.AMAXR)AMAXR=RES2(I)
        IF(RES2(I).LT.AMINR)AMINR=RES2(I)
 1700 CONTINUE
      ABSMAX=ABS(AMAXR)
      ABSMIN=ABS(AMINR)
      ABSMM=ABSMAX
      IF(ABSMIN.GT.ABSMAX)ABSMM=ABSMIN
C
C
C               ****************************
C               **  STEP 6--              **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IDEG1=K1-1
      IDEG2=K2-1
C
      IF(IPRINT.EQ.'ON')THEN
        ITITLE='Exact Rational Fit'
        NCTITL=18
        ITITLZ=' '
        NCTITZ=0
C
        ICNT=1
        ITEXT(ICNT)='Number of Points in First Set:'
        NCTEXT(ICNT)=30
        AVALUE(ICNT)=REAL(N)
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='Degree of Numerator:'
        NCTEXT(ICNT)=20
        AVALUE(ICNT)=REAL(IDEG1)
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='Degree of Denominator:'
        NCTEXT(ICNT)=22
        AVALUE(ICNT)=REAL(IDEG1)
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        NUMROW=ICNT
        DO1001I=1,NUMROW
          NTOT(I)=15
 1001   CONTINUE
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1              NCTEXT,AVALUE,IDIGIT,
     1              NTOT,NUMROW,
     1              ICAPSW,ICAPTY,ILAST,IFRST,
     1              ISUBRO,IBUGA3,IERROR)
C
        ITITLE=' '
        NCTITL=0
        ITITL9='Coefficients'
        NCTIT9=12
C
        NUMCOL=6
        NUMLIN=1
C
        DO2010II=1,NUMCLI
          ITITL2(1,II)=' '
          NCTIT2(1,II)=1
          NCOLSP(1,II)=1
 2010   CONTINUE
        ITITL2(1,1)='Terms'
        NCTIT2(1,1)=5
        ITITL2(1,2)='Values'
        NCTIT2(1,2)=6
        NUMLIN=1
C
        IWHTML(1)=300
        IWHTML(2)=150
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IWHTML(6)=150
        IINC1=3000
        IINC2=1200
        IWRTF(1)=IINC1
        IWRTF(2)=IWRTF(1)+IINC2
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC2
        IWRTF(5)=IWRTF(4)+IINC2
        IWRTF(6)=IWRTF(5)+IINC2
C
        NMAX=0
        DO1210I=1,NUMCLI
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          IF(I.EQ.1)THEN
            ALIGN(I)='l'
            NTOT(I)=30
            ITYPCO(I)='ALPH'
          ENDIF
 1210   CONTINUE
C
        DO1220J=1,NUMCLI
          DO1230I=1,MAXROW
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
            IDIGI2(I,J)=-1
            IF(J.EQ.1)IDIGI2(I,J)=0
 1230     CONTINUE
 1220   CONTINUE
C
        NTEMP=1
        IF(K1.GT.5)NTEMP=2
        IF(K1.GT.10)NTEMP=3
        IF(K1.GT.15)NTEMP=4
        IF(K1.GT.20)NTEMP=5
C
        NUMCOL=0
        ICNT=0
        DO1301J=1,NTEMP
C
          ICNT=ICNT+1
          IF(J.EQ.1)THEN
            IVALUE(ICNT,1)(1:13)='Numerator  --'
          ELSE
            IVALUE(ICNT,1)(1:13)='           --'
          ENDIF
          JTEMP1=(J-1)*5+1
          JTEMP2=J*5
          IF(JTEMP2.GT.K1)JTEMP2=K1
          ICNT2=13
          DO1310II=JTEMP1,JTEMP2
            ICNT2=ICNT2+1
            ITEMP1=II-1
            IF(ITEMP1.LE.9)THEN
              IVALUE(ICNT,1)(ICNT2:ICNT2+2)='A  '
              WRITE(IVALUE(ICNT,1)(ICNT2+1:ICNT2+1),'(I1)')ITEMP1
              ICNT2=ICNT2+2
            ELSE
              IVALUE(ICNT,1)(ICNT2:ICNT2+3)='A   '
              WRITE(IVALUE(ICNT,1)(ICNT2+1:ICNT2+2),'(I2)')ITEMP1
              ICNT2=ICNT2+3
            ENDIF
 1310     CONTINUE
          IVALUE(ICNT,1)(ICNT2:ICNT2)=':'
          NCVALU(ICNT,1)=30
C
          ICNT3=1
          ICNT4=0
          DO1315I=JTEMP1,JTEMP2
            ICNT3=ICNT3+1
            ICNT4=ICNT4+1
            AMAT(ICNT,ICNT3)=B2(JTEMP1+ICNT4-1)
            IDIGI2(ICNT,ICNT3)=NUMDIG
 1315     CONTINUE
          NUMCOL=MAX(NUMCOL,ICNT3)
          ROWSEP(ICNT)=0
 1301 CONTINUE
C
        NTEMP=1
        IF(K2.GT.5)NTEMP=2
        IF(K2.GT.10)NTEMP=3
        IF(K2.GT.15)NTEMP=4
        IF(K2.GT.20)NTEMP=5
C
        DO1401J=1,NTEMP
C
          ICNT=ICNT+1
          IF(J.EQ.1)THEN
            IVALUE(ICNT,1)(1:13)='Denominator--'
          ELSE
            IVALUE(ICNT,1)(1:13)='           --'
          ENDIF
          JTEMP1=(J-1)*5+1
          JTEMP2=J*5
          IF(JTEMP2.GT.K2)JTEMP2=K2
          ICNT2=13
          DO1410II=JTEMP1,JTEMP2
            ICNT2=ICNT2+1
            ITEMP1=II-1
            IF(ITEMP1.LE.9)THEN
              IVALUE(ICNT,1)(ICNT2:ICNT2+2)='B  '
              WRITE(IVALUE(ICNT,1)(ICNT2+1:ICNT2+1),'(I1)')ITEMP1
              ICNT2=ICNT2+2
            ELSE
              IVALUE(ICNT,1)(ICNT2:ICNT2+3)='B   '
              WRITE(IVALUE(ICNT,1)(ICNT2+1:ICNT2+2),'(I2)')ITEMP1
              ICNT2=ICNT2+3
            ENDIF
 1410     CONTINUE
          IVALUE(ICNT,1)(ICNT2:ICNT2)=':'
          NCVALU(ICNT,1)=30
C
          ITEMP1=K1
          ICNT3=1
          ICNT4=0
          DO1415I=JTEMP1,JTEMP2
            ICNT3=ICNT3+1
            ICNT4=ICNT4+1
            AMAT(ICNT,ICNT3)=B2(K1+JTEMP1+ICNT4-1)
            IDIGI2(ICNT,ICNT3)=NUMDIG
 1415     CONTINUE
          ROWSEP(ICNT)=0
          NUMCOL=MAX(NUMCOL,ICNT3)
 1401 CONTINUE
C
        ITEMP1=MAX(K1,K2)
        IF(ITEMP1.GT.5)ITEMP1=5
        NMAX=30 + 15*ITEMP1
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
CCCCC   MAXLIZ=0
        CALL DPDT5B(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              NCOLSP,ROWSEP,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
C
        IF(NUMFAC.GT.2)THEN
          ITITLE=' '
          NCTITL=0
          ITITLZ='Application of Exact-Fit Coefficients to Second '
          ITITLZ(49:66)='Pair of Variables'
          NCTITZ=66
C
          ICNT=1
          ITEXT(ICNT)='Number of Points in Second Set:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REAL(NLEFT)
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Estimated Coefficients:'
          NCTEXT(ICNT)=33
          AVALUE(ICNT)=REAL(KM1)
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Residual Degrees of Freedom:'
          NCTEXT(ICNT)=28
          AVALUE(ICNT)=REAL(IRESDF)
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='Residual Sum of Squares:'
          NCTEXT(ICNT)=24
          AVALUE(ICNT)=RESSS
          IDIGIT(ICNT)=NUMDIG
          IF(IRESDF.GE.1)THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='Residual Standard Deviation (Denom=N-P):'
            NCTEXT(ICNT)=40
            AVALUE(ICNT)=RESSD
            IDIGIT(ICNT)=NUMDIG
          ELSE
            ICNT=ICNT+1
            ITEXT(ICNT)='Residual Standard Deviation Undefined'
            NCTEXT(ICNT)=37
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
            ICNT=ICNT+1
            ITEXT(ICNT)='Since Non-Positive Degrees of Freedom'
            NCTEXT(ICNT)=37
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ENDIF
          ICNT=ICNT+1
          ITEXT(ICNT)='Average Absolute Residual (Denom=N):'
          NCTEXT(ICNT)=36
          AVALUE(ICNT)=RESMA
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Largest Positive Residual:'
          NCTEXT(ICNT)=26
          AVALUE(ICNT)=AMAXR
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Largest Negative Residual:'
          NCTEXT(ICNT)=26
          AVALUE(ICNT)=AMINR
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Largest Absolute Residual:'
          NCTEXT(ICNT)=26
          AVALUE(ICNT)=ABSMM
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=0
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          NUMROW=ICNT
          DO2901I=1,NUMROW
            NTOT(I)=15
 2901     CONTINUE
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1                NCTEXT,AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGA3,IERROR)
C
        ENDIF
C
      ENDIF
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EXA2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,N,K1,K2
 9013   FORMAT('IERROR,N,K1,K2 = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXAC(ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT AN EXACT RATIONAL FUNCTION FIT.
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
C     ORIGINAL VERSION--MAY       1978.
C     UPDATED         --JUNE      1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1988. ADD LOFCDF
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE
C                                       COMMON
C     UPDATED         --NOVEMBER  1993. ALLOW SPACES AROUND /
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFORSW
      CHARACTER*4 ICAPSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ITYPE
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 IH2
      CHARACTER*4 IH
      CHARACTER*4 ICH
      CHARACTER*4 ICH1A
      CHARACTER*4 ICH2A
      CHARACTER*4 ICH1B
      CHARACTER*4 ICH2B
      CHARACTER*4 IHFACT
      CHARACTER*4 IHFAC2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION F1(MAXOBV)
      DIMENSION F2(MAXOBV)
      DIMENSION F3(MAXOBV)
      DIMENSION F4(MAXOBV)
C
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
C
CCCCC DIMENSION W(MAXOBV)
C
      DIMENSION ITYPE(100)
      DIMENSION B1(100)
      DIMENSION B2(100)
C
CCCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),F1(1))
      EQUIVALENCE (GARBAG(IGARB2),F2(1))
      EQUIVALENCE (GARBAG(IGARB3),F3(1))
      EQUIVALENCE (GARBAG(IGARB4),F4(1))
      EQUIVALENCE (GARBAG(IGARB5),PRED2(1))
      EQUIVALENCE (GARBAG(IGARB6),RES2(1))
      EQUIVALENCE (GARBAG(IGARB7),B1(1))
      EQUIVALENCE (GARBAG(IGARB7+100),B2(1))
CCCCC END CHANGE
      DIMENSION ICOLIV(10)
      DIMENSION NIV(10)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='AC  '
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      MINN2=2
      MAXFAC=4
C
C               **************************************************
C               **  TREAT THE EXACT RATIONAL FUNCTION FIT CASE  **
C               **************************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EXAC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXAC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.2.AND.ICOM.EQ.'EXAC'.AND.
     1   IHARG(2).EQ.'FIT ')THEN
        ILASTC=2
      ELSEIF(NUMARG.GE.3.AND.ICOM.EQ.'EXAC'.AND.
     1       IHARG(3).EQ.'FIT ')THEN
        ILASTC=3
      ELSEIF(NUMARG.GE.4.AND.ICOM.EQ.'EXAC'.AND.
     1       IHARG(4).EQ.'FIT ')THEN
        ILASTC=4
      ELSEIF(NUMARG.GE.5.AND.ICOM.EQ.'EXAC'.AND.
     1       IHARG(5).EQ.'FIT ')THEN
        ILASTC=5
      ELSEIF(NUMARG.GE.6.AND.ICOM.EQ.'EXAC'.AND.
     1       IHARG(6).EQ.'FIT ')THEN
        ILASTC=6
      ELSE
        IFOUND='NO'
        GOTO9000
      ENDIF
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=2
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************
C               **  STEP 3--                           **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO290
      DO200J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO210
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO210
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO220
  200 CONTINUE
      GOTO290
  210 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO290
  220 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO290
  290 CONTINUE
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EXAC')THEN
        WRITE(ICOUT,291)NUMARG,ILOCQ
  291   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 5--                                          **
C               **  1) CHECK THE VALIDITY OF THE VARIABLES.           **
C               **  2) CHECK THAT THERE ARE EXACTLY 2 OR EXACTLY 4    **
C               **     VARIABLES.                                     **
C               **  3) CHECK THE VALIDITY OF EACH OF THE VARIABLES.   **
C               **     DOES THE VARIABLE NAME EXIST IN THE TABLE?     **
C               **     IS THE NUMBER OF ELEMENTS FOR EACH VARIABLE    **
C               **     POSITIVE?                                      **
C               **  DOES THE NUMBER OF ELEMENTS IN VARIABLE 2         **
C               **  AGREE WITH THE NUMBER OF ELEMENTS IN VARIABLE 1?  **
C               **  IF VARIABLES 3 AND 4 EXIST,                       **
C               **  DOES THE NUMBER OF ELEMENTS IN VARIABLE 4         **
C               **  AGREE WITH THE NUMBER OF ELEMENTS IN VARIABLE 3?  **
C               ********************************************************
C
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMFAC=ILOCQ-1
      IF(NUMFAC.LT.1 .OR. NUMFAC.GT.MAXFAC)THEN
        WRITE(ICOUT,501)
  501   FORMAT('***** ERROR IN EXACT RATIONAL FIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,502)
  502   FORMAT('      FOR AN EXACT RATIONAL FUNCTION FIT, THE NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,504)
  504   FORMAT('      OF VARIABLES MUST BE EXACTLY 2 OR EXACTLY 4;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,505)
  505   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,506)NUMFAC
  506   FORMAT('      THE SPECIFIED NUMBER OF VARIABLES WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
  507   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
  508     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO510IFAC=1,NUMFAC
        IHFACT=IHARG(IFAC)
        IHFAC2=IHARG2(IFAC)
        IHWUSE='V'
        MESSAG='YES'
        CALL CHECKN(IHFACT,IHFAC2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        ICOLIV(IFAC)=IVALUE(ILOCV)
        NIV(IFAC)=IN(ILOCV)
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EXAC')THEN
          WRITE(ICOUT,665)IFAC,IHFACT,IHFAC2,ILOCV,IVALUE(ILOCV)
  665     FORMAT('IFAC,IHFACT,IHFAC2,ILOCV,IVALUE(ILOCV) = ',
     1           I8,2(2X,A4),2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,666)IFAC,IHFACT,IHFAC2,ICOLIV(IFAC),NIV(IFAC)
  666     FORMAT('IFAC,IHFACT,IHFAC2,ICOLIV(IFAC),NIV(IFAC) = ',
     1           I8,2(2X,A4),2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  510 CONTINUE
C
      DO515IFAC=1,NUMFAC
        IF(NIV(IFAC).GE.1)GOTO515
C
        WRITE(ICOUT,501)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,522)
  522   FORMAT('      FOR AN EXACT RATIONAL FUNCTION FIT, ALL ',
     1         'VARIABLES MUST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,524)
  524   FORMAT('      HAVE AT LEAST ONE ELEMENT; SUCH WAS NOT THE ',
     1         'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO525J=1,NUMFAC
          WRITE(ICOUT,526)IHARG(J),IHARG2(J),NIV(J)
  526     FORMAT('      VARIABLE ',A4,A4,'  HAS ',I8,' ELEMENTS')
          CALL DPWRST('XXX','BUG ')
  525   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
  515 CONTINUE
      NEXACT=NIV(1)
C
      IF(NIV(1).NE.NIV(2) .OR.
     1  (NUMFAC.GE.3 .AND. NIV(3).NE.NIV(4)))THEN
        WRITE(ICOUT,501)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,532)
  532   FORMAT('      FOR AN EXACT RATIONAL FUNCTION FIT, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,533)
  533   FORMAT('      NUMBER OF ELEMENTS IN VARIABLE TWO MUST EQUAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,534)
  534   FORMAT('      THE NUMBER OF ELEMENTS IN VARIABLE ONE;')
        CALL DPWRST('XXX','BUG ')
        IF(NUMFAC.GE.3)THEN
          WRITE(ICOUT,536)
  536     FORMAT('      AND THE NUMBER OF ELEMENTS IN VARIABLE FOUR ',
     1           'MUST')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,537)
  537     FORMAT('      EQUAL THE NUMBER OF ELEMENTS IN VARIABLE ',
     1           'THREE;')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,538)
  538   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO545J=1,NUMFAC
          WRITE(ICOUT,546)IHARG(J),IHARG2(J),NIV(J)
  546     FORMAT('      VARIABLE ',A4,A4,'  HAS ',I8,' ELEMENTS')
          CALL DPWRST('XXX','BUG ')
  545   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ***************************************
C               **  STEP 6--                         **
C               **  EXTRACT THE EXACT-FIT VARIABLES  **
C               ***************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO560I=1,NEXACT
        J=J+1
C
        IFAC=1
        ICOLR=ICOLIV(IFAC)
        IJ=MAXN*(ICOLR-1)+I
        IF(ICOLR.LE.MAXCOL)F1(J)=V(IJ)
        IF(ICOLR.EQ.MAXCP1)F1(J)=PRED(I)
        IF(ICOLR.EQ.MAXCP2)F1(J)=RES(I)
        IF(ICOLR.EQ.MAXCP3)F1(J)=YPLOT(I)
        IF(ICOLR.EQ.MAXCP4)F1(J)=XPLOT(I)
        IF(ICOLR.EQ.MAXCP5)F1(J)=X2PLOT(I)
        IF(ICOLR.EQ.MAXCP6)F1(J)=TAGPLO(I)
C
        IFAC=2
        ICOLR=ICOLIV(IFAC)
        IJ=MAXN*(ICOLR-1)+I
        IF(ICOLR.LE.MAXCOL)F2(J)=V(IJ)
        IF(ICOLR.EQ.MAXCP1)F2(J)=PRED(I)
        IF(ICOLR.EQ.MAXCP2)F2(J)=RES(I)
        IF(ICOLR.EQ.MAXCP3)F2(J)=YPLOT(I)
        IF(ICOLR.EQ.MAXCP4)F2(J)=XPLOT(I)
        IF(ICOLR.EQ.MAXCP5)F2(J)=X2PLOT(I)
        IF(ICOLR.EQ.MAXCP6)F2(J)=TAGPLO(I)
C
  560 CONTINUE
C
C               *********************************************
C               **  STEP 7--                               **
C               **  BRANCH TO THE APPROPRIATE SUBCASE;     **
C               **  THEN FORM THE RESPONSE VARIABLE        **
C               **  AND THE FACTORS.                       **
C               *********************************************
C
      ISTEPN='7'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMFAC.GE.3)N34=NIV(3)
C
      IF(NUMFAC.LE.2)NLEFT=NEXACT
      IF(NUMFAC.GE.3)NLEFT=N34
C
      IF(ICASEQ.EQ.'FULL')GOTO610
      IF(ICASEQ.EQ.'SUBS')GOTO620
      IF(ICASEQ.EQ.'FOR')GOTO630
C
  610 CONTINUE
      DO615I=1,NLEFT
      ISUB(I)=1
  615 CONTINUE
      NQ=NLEFT
      GOTO650
C
  620 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GOTO650
C
  630 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GOTO650
C
  650 CONTINUE
C
      IF(NUMFAC.GE.3)THEN
        J=0
        DO670I=1,NLEFT
          J=J+1
C
          IFAC=3
          ICOLR=ICOLIV(IFAC)
          IJ=MAXN*(ICOLR-1)+I
          IF(ICOLR.LE.MAXCOL)F3(J)=V(IJ)
          IF(ICOLR.EQ.MAXCP1)F3(J)=PRED(I)
          IF(ICOLR.EQ.MAXCP2)F3(J)=RES(I)
          IF(ICOLR.EQ.MAXCP3)F3(J)=YPLOT(I)
          IF(ICOLR.EQ.MAXCP4)F3(J)=XPLOT(I)
          IF(ICOLR.EQ.MAXCP5)F3(J)=X2PLOT(I)
          IF(ICOLR.EQ.MAXCP6)F3(J)=TAGPLO(I)
C
          IFAC=4
          ICOLR=ICOLIV(IFAC)
          IJ=MAXN*(ICOLR-1)+I
          IF(ICOLR.LE.MAXCOL)F4(J)=V(IJ)
          IF(ICOLR.EQ.MAXCP1)F4(J)=PRED(I)
          IF(ICOLR.EQ.MAXCP2)F4(J)=RES(I)
          IF(ICOLR.EQ.MAXCP3)F4(J)=YPLOT(I)
          IF(ICOLR.EQ.MAXCP4)F4(J)=XPLOT(I)
          IF(ICOLR.EQ.MAXCP5)F4(J)=X2PLOT(I)
          IF(ICOLR.EQ.MAXCP6)F4(J)=TAGPLO(I)
C
  670   CONTINUE
C
      ENDIF
C
C               ****************************************
C               **  STEP 8--                          **
C               **  DETERMINE THE DEGREES             **
C               **  OF THE NUMERATOR AND DENOMINATOR  **
C               ****************************************
C
      ISTEPN='8'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO700J=1,IWIDTH
        J2=J
        IF(IANS(J).EQ.'/')GOTO710
  700 CONTINUE
C
      WRITE(ICOUT,501)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,702)
  702 FORMAT('      NO    /    FOUND ON ENTERED COMMAND LINE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,703)
  703 FORMAT('      THEREFORE, DEGREE OF NUMERATOR UNKNOWN, AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,704)
  704 FORMAT('      DEGREE OF DENOMINATOR UNKNOWN.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,507)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
  710 CONTINUE
      J2M1=J2-1
      J2M2=J2-2
      IF(J2M1.GE.1)GOTO720
C
      WRITE(ICOUT,501)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,712)
  712 FORMAT('      THE LOCATED    /    WAS FOUND AS THE FIRST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,713)
  713 FORMAT('      CHARACTER OF THE ENTERED COMMAND LINE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,714)
  714 FORMAT('      THEREFORE, DEGREE OF NUMERATOR UNKNOWN.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,507)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
  720 CONTINUE
      ICH1B=IANS(J2M1)
      IF(J2M1.GE.2)ICH2B=IANS(J2M2)
      IF(J2M1.EQ.1)ICH2B=' '
C
      IF(ICH2B.EQ.' '.AND.ICH1B.EQ.'0')THEN
        IDEGN=0
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'1')THEN
        IDEGN=1
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'2')THEN
        IDEGN=2
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'3')THEN
        IDEGN=3
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'4')THEN
        IDEGN=4
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'5')THEN
        IDEGN=5
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'6')THEN
        IDEGN=6
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'7')THEN
        IDEGN=7
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'8')THEN
        IDEGN=8
      ELSEIF(ICH2B.EQ.' '.AND.ICH1B.EQ.'9')THEN
        IDEGN=9
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'0')THEN
        IDEGN=10
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'1')THEN
        IDEGN=11
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'2')THEN
        IDEGN=12
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'3')THEN
        IDEGN=13
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'4')THEN
        IDEGN=14
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'5')THEN
        IDEGN=15
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'6')THEN
        IDEGN=16
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'7')THEN
        IDEGN=17
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'8')THEN
        IDEGN=18
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.'9')THEN
        IDEGN=19
      ELSEIF(ICH2B.EQ.'2'.AND.ICH1B.EQ.'0')THEN
        IDEGN=20
      ELSEIF(ICH2B.EQ.'0'.AND.ICH1B.EQ.' ')THEN
        IDEGN=0
      ELSEIF(ICH2B.EQ.'1'.AND.ICH1B.EQ.' ')THEN
        IDEGN=1
      ELSEIF(ICH2B.EQ.'2'.AND.ICH1B.EQ.' ')THEN
        IDEGN=2
      ELSEIF(ICH2B.EQ.'3'.AND.ICH1B.EQ.' ')THEN
        IDEGN=3
      ELSEIF(ICH2B.EQ.'4'.AND.ICH1B.EQ.' ')THEN
        IDEGN=4
      ELSEIF(ICH2B.EQ.'5'.AND.ICH1B.EQ.' ')THEN
        IDEGN=5
      ELSEIF(ICH2B.EQ.'6'.AND.ICH1B.EQ.' ')THEN
        IDEGN=6
      ELSEIF(ICH2B.EQ.'7'.AND.ICH1B.EQ.' ')THEN
        IDEGN=7
      ELSEIF(ICH2B.EQ.'8'.AND.ICH1B.EQ.' ')THEN
        IDEGN=8
      ELSEIF(ICH2B.EQ.'9'.AND.ICH1B.EQ.' ')THEN
        IDEGN=9
      ELSE
        WRITE(ICOUT,501)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,722)
  722   FORMAT('      FOR AN EXACT RATIONAL FUNCTION FIT,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,723)
  723   FORMAT('      THE DEGREE FOR THE NUMERATOR MUST BE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,724)
  724   FORMAT('      BETWEEN 0 AND 20 (INCLUSIVELY);')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,725)ICH1B,ICH2B
  725   FORMAT('      SUCH WAS NOT THE CASE HERE.',A1,',',A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,726)
  726   FORMAT('      (REMINDER--THERE SHOULD BE NO BLANK')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,727)
  727   FORMAT('      BETWEEN THE DEGREE NUMBER AND THE /).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      J2P1=J2+1
      J2P2=J2+2
      IF(J2P1.GT.IWIDTH)THEN
        WRITE(ICOUT,501)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)
  812   FORMAT('      THE LOCATED    /    WAS FOUND AS THE LAST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)
  813   FORMAT('      CHARACTER OF THE ENTERED COMMAND LINE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,814)
  814   FORMAT('      THEREFORE, DEGREE OF DENOMINATOR UNKNOWN.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      ICH1A=IANS(J2P1)
      IF(J2P1.GE.2)ICH2A=IANS(J2P2)
      IF(J2P1.EQ.1)ICH2A=' '
C
      IF(ICH2A.EQ.' '.AND.ICH1A.EQ.'0')THEN
        IDEGD=0
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'1')THEN
        IDEGD=1
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'2')THEN
        IDEGD=2
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'3')THEN
        IDEGD=3
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'4')THEN
        IDEGD=4
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'5')THEN
        IDEGD=5
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'6')THEN
        IDEGD=6
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'7')THEN
        IDEGD=7
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'8')THEN
        IDEGD=8
      ELSEIF(ICH2A.EQ.' '.AND.ICH1A.EQ.'9')THEN
        IDEGD=9
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'0')THEN
        IDEGD=10
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'1')THEN
        IDEGD=11
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'2')THEN
        IDEGD=12
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'3')THEN
        IDEGD=13
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'4')THEN
        IDEGD=14
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'5')THEN
        IDEGD=15
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'6')THEN
        IDEGD=16
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'7')THEN
        IDEGD=17
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'8')THEN
        IDEGD=18
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.'9')THEN
        IDEGD=19
      ELSEIF(ICH2A.EQ.'2'.AND.ICH1A.EQ.'0')THEN
        IDEGD=20
      ELSEIF(ICH2A.EQ.'0'.AND.ICH1A.EQ.' ')THEN
        IDEGD=0
      ELSEIF(ICH2A.EQ.'1'.AND.ICH1A.EQ.' ')THEN
        IDEGD=1
      ELSEIF(ICH2A.EQ.'2'.AND.ICH1A.EQ.' ')THEN
        IDEGD=2
      ELSEIF(ICH2A.EQ.'3'.AND.ICH1A.EQ.' ')THEN
        IDEGD=3
      ELSEIF(ICH2A.EQ.'4'.AND.ICH1A.EQ.' ')THEN
        IDEGD=4
      ELSEIF(ICH2A.EQ.'5'.AND.ICH1A.EQ.' ')THEN
        IDEGD=5
      ELSEIF(ICH2A.EQ.'6'.AND.ICH1A.EQ.' ')THEN
        IDEGD=6
      ELSEIF(ICH2A.EQ.'7'.AND.ICH1A.EQ.' ')THEN
        IDEGD=7
      ELSEIF(ICH2A.EQ.'8'.AND.ICH1A.EQ.' ')THEN
        IDEGD=8
      ELSEIF(ICH2A.EQ.'9'.AND.ICH1A.EQ.' ')THEN
        IDEGD=9
      ELSE
        WRITE(ICOUT,501)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,722)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,823)
  823   FORMAT('      THE DEGREE FOR THE DENOMINATOR MUST BE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,724)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,725)ICH1A,ICH2A
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,726)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,727)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      K1=IDEGN+1
      K2=IDEGD+1
      K=K1+K2
      KM1=K-1
C
      IF(NEXACT.NE.KM1)THEN
        WRITE(ICOUT,501)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,902)
  902   FORMAT('      FOR AN EXACT RATIONAL FUNCTION FIT, THE NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,903)
  903   FORMAT('      OF ELEMENTS IN THE FIRST VARIABLE (THAT IS, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,905)
  905   FORMAT('      NUMBER IF POINTS TO BE EXACTLY FITTED) MUST =')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,906)
  906   FORMAT('      THE NUMBER OF COEFFICIENTS TO BE ESTIMATED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,907)
  907   FORMAT('      (THAT IS, MUST = (DEGREE OF NUMERATOR + 1) +')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,908)
  908   FORMAT('      (DEGREE OF DENOMINATOR + 1) - 1   );')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,909)
  909   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,910)NEXACT
  910   FORMAT('      NUMBER OF FIT POINTS FROM FIRST VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,911)KM1
  911   FORMAT('      NUMBER OF ESTIMATED COEFFICIENTS         = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,912)IDEGN
  912   FORMAT('      DEGREE OF NUMERATOR                      = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,913)IDEGD
  913   FORMAT('      DEGREE OF DENOMINATOR                    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO930I=1,K
        ITYPE(I)='U'
        B1(I)=999.0
  930 CONTINUE
      K1P1=K1+1
      ITYPE(K1P1)='K'
      B1(K1P1)=1.0
C
C               ************************************
C               **  STEP 9--                      **
C               **  CARRY OUT THE EXACT RATIONAL  **
C               **  FUNCTION FIT.                 **
C               ************************************
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')THEN
        ISTEPN='9'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,681)
  681   FORMAT('***** FROM DPEXAC, AS WE ARE ABOUT TO CALL DPEXA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,682)NEXACT,MAXN,K1,K2,NUMFAC,N34,NLEFT
  682   FORMAT('NEXACT,MAXN,K1,K2,NUMFAC,N34,NLEFT = ',7I8)
        CALL DPWRST('XXX','BUG ')
        DO685I=1,NEXACT
          WRITE(ICOUT,686)I,F1(I),F2(I)
  686     FORMAT('I,F1(I),F2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
  685   CONTINUE
      ENDIF
C
      CALL DPEXA2(F1,F2,NEXACT,ITYPE,B1,K1,K2,B2,
     1            F3,F4,NLEFT,NUMFAC,KM1,RES2,PRED2,
     1            ICAPTY,ICAPSW,IFORSW,
     1            IBUGA3,ISUBRO,IERROR)
C
C               ***************************************
C               **  STEP 11--                        **
C               **  UPDATE DATAPLOT INTERNAL TABLES  **
C               ***************************************
C
      ISTEPN='11'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
C
CCCCC IREPU='ON'  MARCH 1988
CCCCC IRESU='ON'  MARCH 1988
C     THE FOLLOWING CORRECTION WAS BASED ON
C     COMMENTS FROM DAVE EVANS     MARCH 1988
CCCCC IREPU='ON'
      IREPU='OFF'
      REPSD=(-999.99)
      REPDF=(-999.99)
      ALFCDF=(-999.99)
C
      IRESU='ON'
C
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NLEFT,
     1            IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
C
      L=0
      DO7600J=1,K1
        L=L+1
        IH='A   '
        IH2='    '
        JTEMP=J-1
        IF(JTEMP.LE.9)THEN
          WRITE(IH(2:2),'(I1)')JTEMP
        ELSEIF(JTEMP.LE.20)THEN
          WRITE(IH(2:3),'(I2)')JTEMP
        ENDIF
C
        DO7650I=1,NUMNAM
          I2=I
          IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'P')THEN
            VALUE(I2)=B2(L)
            GOTO7600
          ENDIF
 7650   CONTINUE
C
        IF(NUMNAM.GE.MAXNAM)THEN
          WRITE(ICOUT,501)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7652)
 7652     FORMAT('      THE TOTAL NUMBER OF (VARIABLE + PARAMETER) ',
     1           'NAMES MUST BE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7654)MAXNAM
 7654     FORMAT('      AT MOST ',I8,';  SUCH WAS NOT THE CASE HERE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7655)
 7655     FORMAT('      THE MAXIMUM ALLOWABLE NUMBER OF NAMES WAS ',
     1           'JUST EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7657)
 7657     FORMAT('      SUGGESTED ACTION--ENTER     STAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7658)
 7658     FORMAT('      TO DETERMINE THE IMPORTANT VARIABLES AND')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7660)
 7660     FORMAT('      PARAMETERS, AND THEN REUSE SOME OF THE NAMES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,507)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ELSE
          NUMNAM=NUMNAM+1
          ILOC=NUMNAM
          IHNAME(ILOC)=IH
          IHNAM2(ILOC)=IH2
          IUSE(ILOC)='P'
          VALUE(ILOC)=B2(L)
        ENDIF
C
 7600 CONTINUE
C
      DO7700J=1,K2
        L=L+1
        IH='B   '
        IH2='    '
        JTEMP=J-1
        IF(JTEMP.LE.9)THEN
          WRITE(IH(2:2),'(I1)')JTEMP
        ELSEIF(JTEMP.LE.20)THEN
          WRITE(IH(2:3),'(I2)')JTEMP
        ENDIF
C
        DO7750I=1,NUMNAM
          I2=I
          IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'P')THEN
            VALUE(I2)=B2(L)
            GOTO7700
          ENDIF
 7750   CONTINUE
C
        IF(NUMNAM.GE.MAXNAM)THEN
          WRITE(ICOUT,501)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7652)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7654)MAXNAM
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7655)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7657)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7658)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7660)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,507)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ELSE
          NUMNAM=NUMNAM+1
          ILOC=NUMNAM
          IHNAME(ILOC)=IH
          IHNAM2(ILOC)=IH2
          IUSE(ILOC)='P'
          VALUE(ILOC)=B2(L)
        ENDIF
C
 7700 CONTINUE
C
C               ***************************************
C               **  STEP 12--                        **
C               **  ENTER THE FORTRAN EXPRESSION     **
C               **  FOR THE RATIONAL FUNCTION MODEL  **
C               **  INTO MODEL(.)                    **
C               **  FOR FURTHER USE                  **
C               **  VIA THE    FIT    COMMAND.       **
C               ***************************************
C
      ISTEPN='12'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I=0
      I=I+1
      MODEL(I)='Y'
      I=I+1
      MODEL(I)='='
      I=I+1
      MODEL(I)='('
C
      DO8100J=1,K1
        IF(J.GT.1)THEN
          I=I+1
          IF(I.GT.80)GOTO8150
          MODEL(I)='+'
        ENDIF
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='A'
        ICH=' '
        JTEMP=J-1
        IF(JTEMP.LE.9)THEN
          I=I+1
          IF(I.GT.80)GOTO8150
          WRITE(MODEL(I)(1:1),'(I1)')JTEMP
        ELSEIF(JTEMP.LE.20)THEN
          I=I+1
          IF(I+1.GT.80)GOTO8150
          WRITE(ICH(1:2),'(I2)')JTEMP
          MODEL(I)(1:1)=ICH(1:1)
          I=I+1
          MODEL(I)(1:1)=ICH(2:2)
        ENDIF
C
        IF(J.LE.1)GOTO8100
C
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='*'
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='X'
C
        IF(J.LE.2)GOTO8100
C
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='*'
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='*'
        IF(JTEMP.LE.9)THEN
          I=I+1
          IF(I.GT.80)GOTO8150
          WRITE(MODEL(I)(1:1),'(I1)')JTEMP
        ELSEIF(JTEMP.LE.20)THEN
          I=I+1
          IF(I+1.GT.80)GOTO8150
          WRITE(ICH(1:2),'(I2)')JTEMP
          MODEL(I)(1:1)=ICH(1:1)
          I=I+1
          MODEL(I)(1:1)=ICH(2:2)
        ENDIF
 8100 CONTINUE
C
      I=I+1
      IF(I.GT.80)GOTO8150
      MODEL(I)=')'
C
      I=I+1
      IF(I.GT.80)GOTO8150
      MODEL(I)='/'
      I=I+1
      IF(I.GT.80)GOTO8150
      MODEL(I)='('
C
      DO8200J=1,K2
        IF(J.GT.1)THEN
          I=I+1
          IF(I.GT.80)GOTO8150
          MODEL(I)='+'
        ENDIF
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='B'
        ICH=' '
        JTEMP=J-1
        IF(JTEMP.LE.9)THEN
          I=I+1
          IF(I.GT.80)GOTO8150
          WRITE(MODEL(I)(1:1),'(I1)')JTEMP
        ELSEIF(JTEMP.LE.20)THEN
          I=I+1
          IF(I+1.GT.80)GOTO8150
          WRITE(ICH(1:2),'(I2)')JTEMP
          MODEL(I)(1:1)=ICH(1:1)
          I=I+1
          MODEL(I)(1:1)=ICH(2:2)
        ENDIF
C
        IF(J.LE.1)GOTO8200
C
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='*'
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='X'
C
        IF(J.LE.2)GOTO8200
C
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='*'
        I=I+1
        IF(I.GT.80)GOTO8150
        MODEL(I)='*'
        IF(JTEMP.LE.9)THEN
          I=I+1
          IF(I.GT.80)GOTO8150
          WRITE(MODEL(I)(1:1),'(I1)')JTEMP
        ELSEIF(JTEMP.LE.20)THEN
          I=I+1
          IF(I+1.GT.80)GOTO8150
          WRITE(ICH(1:2),'(I2)')JTEMP
          MODEL(I)(1:1)=ICH(1:1)
          I=I+1
          MODEL(I)(1:1)=ICH(2:2)
        ENDIF
 8200 CONTINUE
      I=I+1
      IF(I.GT.80)GOTO8150
      MODEL(I)=')'
      NUMCHA=I
      GOTO8290
C
 8150 CONTINUE
      WRITE(ICOUT,8251)
 8251 FORMAT('***** NOTE FROM DPEXAC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8252)
 8252 FORMAT('      THE FORTRAN EXPRESSION FOR THE RATIONAL FUNCTION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8254)
 8254 FORMAT('      THAT WAS BEING AUTOMATICALLY ENTERED INTO AN')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8255)
 8255 FORMAT('      INTERNAL DATAPLOT ARRAY NAMED MODEL(.) WAS NOT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8257)
 8257 FORMAT('      COMPLETED DUE TO THE FACT THAT THE FORTRAN ',
     1       'EXPRESSION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8258)
 8258 FORMAT('      FOR THIS RATIONAL FUNCTION IS IN EXCESS OF THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8259)
 8259 FORMAT('      ARRAY LIMIT OF 80 CHARACTERS.  THIS DOES NOT ',
     1       'AFFECT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8261)
 8261 FORMAT('      THE VALIDITY OF THE PRECEEDING EXACT RATIONAL ',
     1       'FUNCTION FIT.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,507)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,508)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 8290 CONTINUE
C
      GOTO9000
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EXAC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXAC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NEXACT,N34,K1,K2,NLEFT
 9014   FORMAT('NEXACT,N34,K1,K2,NLEFT = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ICASEQ,IFOUND,IERROR
 9016   FORMAT('ICASEQ,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXFI(TEMP1,NPAR,IBUGA3,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--SUPPORTS THE FOLLOWING COMMAND:
C
C                 LET Y = EXECUTE <FILE> X
C
C              THIS COMMAND WILL WRITE THE INFORMATION IN "X" TO
C              THE FILE "dpst1f.dat" AND THEN EXECUTE THE COMMAND
C
C                 <FILE>  < dpst1f.dat > dpst2f.dat
C
C              THE OUTPUT WILL BE READ INTO THE VARIABLE Y.
C
C              THIS COMMAND WAS MOTIVATED PRIMARILY TO BE USED
C              WITHIN THE A "FUNCTION BLOCK".  HOWEVER, IT CAN IN
C              FACT BE USED INDEPDENTLY OF FUNCTION BLOCKS.
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
C     UPDATED         --FEBRUARY  2016. SET FILE NAME QUOTE ON
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DIMENSION TEMP1(*)
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOFILE
      CHARACTER*4 IANSI
      CHARACTER*4 IOP
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IOUTTY
      CHARACTER*4 NEWNAM
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFILSV
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC CHARACTER*255 IFILE
CCCCC CHARACTER*255 ICANS
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER (LEN=MAXSTR) :: ICANS
C
C ---------------------------------------------------------------------
C
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='FI  '
      IFOUND='YES'
      IERROR='NO'
      IH='UNKN'
      IH2='UNKN'
      IFILSV=IFILQU
      IFILQU='ON'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      ICOL=0
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXFI--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************************
C               **  STEP 1--                                      **
C               **  CHECK FOR VALID COMMAND.                      **
C               ****************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHARG(2).NE.'=' .OR. IHARG(3).NE.'EXEC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN EXECUTE FILE COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)
  103   FORMAT('      INVALID FORM FOR THE COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 2--                                          **
C               **  EXTRACT THE FILE NAME                             **
C               ********************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWORD=5
      CALL DPFILE(IANSLC,IWIDTH,IWORD,
     1            IOFILE,IBUGA3,ISUBRO,IERROR)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')THEN
        WRITE(ICOUT,201)IWORD,IWIDTH,IOFILE
  201   FORMAT('IWORD,IWIDTH,IOFILE = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************************
C               **  STEP 21--                               **
C               **  IF NO FILE NAME GIVEN,                  **
C               **  THEN GENERATE AN ERROR MESSAGE.         **
C               **********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOFILE.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,212)
  212   FORMAT('      NO FILE NAME WAS GIVEN.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *************************************
C               **  STEP 22--                      **
C               **  COPY THE FILE INTO THE STRING  **
C               **  "FILE"                         **
C               *************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO220I=1,MAXSTR
        IANSI=IANSLC(I)
        ICANS(I:I)=IANSI(1:1)
  220 CONTINUE
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')THEN
        WRITE(ICOUT,221)NCFILE,IFILE
  221   FORMAT('NCFILE,IFILE = ',I6,2X,A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(NCFILE.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,222)
  222   FORMAT('      A USER FILE NAME IS REQUIRED IN THE ',
     1         'LET ... = EXECUTE ...  COMMAND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,224)
  224   FORMAT('      (FOR EXAMPLE,    LET Y = EXECUTE ./a.out X   )')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,225)
  225   FORMAT('      BUT NONE WAS GIVEN HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,226)
  226   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,227)(IANSLC(I),I=1,MIN(IWIDTH,100))
  227     FORMAT('      ',100A1)
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
C               *******************************************
C               **  STEP 23--                           **
C               **  APPEND " < dpst1f.dat > dpst2f.dat" **
C               **  TO THE STRING                       **
C               ******************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NCFILE.LE.229)THEN
        IFILE(NCFILE+1:NCFILE+26)=' < dpst1f.dat > dpst2f.dat'
        NCFILE=NCFILE+26
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,231)
  231   FORMAT('      THE STRING TO EXECUTE EXCEEDS 229 CHARACTERS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,233)NCFILE
  233   FORMAT('      THE NUMBER OF CHARACTERS = ',I6)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')THEN
        WRITE(ICOUT,221)NCFILE,IFILE
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 3--                             **
C               **  EXTRACT THE VARIABLE NAME ON THE RHS **
C               *******************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      IH=IHARG(5)
      IH2=IHARG2(5)
      DO310II=1,NUMNAM
        IF(IH.EQ.IHNAME(II) .AND. IH2.EQ.IHNAM2(II))THEN
          IF(IUSE(II).EQ.'P')THEN
            IF(NPAR.LE.0)THEN
              NVAL=1
              WRITE(IOUNI1,'(I8)')NVAL
              WRITE(IOUNI1,'(E15.7)')VALUE(II)
            ELSE
              NTEMP=NPAR+NVAL
              WRITE(IOUNI1,'(I8)')NTEMP
              DO311JJ=1,NPAR
                WRITE(IOUNI1,'(E15.7)')TEMP1(JJ)
  311         CONTINUE
              WRITE(IOUNI1,'(E15.7)')VALUE(II)
            ENDIF
          ELSEIF(IUSE(II).EQ.'V')THEN
            ICOL=IVALUE(II)
            NVAL=IN(II)
            IF(NPAR.LE.0)THEN
              WRITE(IOUNI1,'(I8)')NVAL
            ELSE
              NTEMP=NPAR+NVAL
              WRITE(IOUNI1,'(I8)')NTEMP
              DO313JJ=1,NPAR
                WRITE(IOUNI1,'(E15.7)')TEMP1(JJ)
  313         CONTINUE
            ENDIF
            DO320JJ=1,NVAL
              IJ=MAXN*(ICOL-1)+JJ
              IF(ICOL.LE.MAXCOL)AVAL=V(IJ)
              IF(ICOL.EQ.MAXCP1)AVAL=PRED(JJ)
              IF(ICOL.EQ.MAXCP2)AVAL=RES(JJ)
              IF(ICOL.EQ.MAXCP3)AVAL=YPLOT(JJ)
              IF(ICOL.EQ.MAXCP4)AVAL=XPLOT(JJ)
              IF(ICOL.EQ.MAXCP5)AVAL=X2PLOT(JJ)
              IF(ICOL.EQ.MAXCP6)AVAL=TAGPLO(JJ)
              WRITE(IOUNI1,'(E15.7)')AVAL
  320       CONTINUE
          ELSE
            NVAL=0
            WRITE(IOUNI1,'(I8)')NVAL
          ENDIF
        ENDIF
  310 CONTINUE
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *******************************************
C               **  STEP 4--                             **
C               **  RUN THE COMMAND                      **
C               *******************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPSYS2(IFILE,NCFILE,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *******************************************
C               **  STEP 5--                             **
C               **  READ THE RESULTS BACK IN             **
C               *******************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CHECK THE VARIABLE NAME ON THE LEFT HAND SIDE (THIS
C     SHOULD BE A VARIABLE).
C
      IOUTTY='NULL'
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO510II=1,NUMNAM
        IF(IHLEFT.EQ.IHNAME(II) .AND. IHLEF2.EQ.IHNAM2(II))THEN
          IF(IUSE(II).EQ.'P')THEN
            ILISTL=II
            NEWNAM='NO'
            IOUTTY='PARA'
            GOTO519
          ELSEIF(IUSE(II).EQ.'V')THEN
            ILISTL=II
            NEWNAM='NO'
            IOUTTY='VARI'
            ICOL=IVALUE(ILISTL)
            NLEFT=IN(ILISTL)
            GOTO519
          ELSE
            WRITE(ICOUT,506)
  506       FORMAT('      THE NAME ON THE LEFT HAND SIDE OF THE = WAS ',
     1             'FOUND')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,507)
  507       FORMAT('      IN THE NAME TABLE, BUT NOT AS A PARAMETER ',
     1             'OR VARIABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
        ENDIF
  510 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IOUTTY='VARI'
      IF(NUMNAM.GE.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,512)
  512   FORMAT('      THE NUMBER OF VARIABLE AND/OR PARAMETER NAMES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,513)MAXNAM
  513   FORMAT('      HAS JUST EXCEEDED THE MAXIMUM ALLOWABLE ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,514)
  514   FORMAT('      NOTHING DONE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSE
        NLEFT=0
        ICOL=NUMCOL+1
        IF(ICOL.GT.MAXCOL)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,516)
  516     FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST EXCEEDED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,517)MAXCOL
  517     FORMAT('      THE MAXIMUM ALLOWABLE ',I8,'.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
  519 CONTINUE
C
      IOP='OPEN'
      IFLAG1=0
      IFLAG2=1
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      NINEW=0
      NLAST=MAXOBV
      IF(IOUTTY.EQ.'PARA')NLAST=1
      DO520JJ=1,NLAST
        READ(IOUNI2,*,END=590,ERR=580)AVAL
        NINEW=NINEW+1
        IJ=MAXN*(ICOL-1)+JJ
        IF(ICOL.LE.MAXCOL)V(IJ)=AVAL
        IF(ICOL.EQ.MAXCP1)PRED(JJ)=AVAL
        IF(ICOL.EQ.MAXCP2)RES(JJ)=AVAL
        IF(ICOL.EQ.MAXCP3)YPLOT(JJ)=AVAL
        IF(ICOL.EQ.MAXCP4)XPLOT(JJ)=AVAL
        IF(ICOL.EQ.MAXCP5)X2PLOT(JJ)=AVAL
        IF(ICOL.EQ.MAXCP6)TAGPLO(JJ)=AVAL
  520 CONTINUE
      GOTO590
C
C     ERROR READING FILE
C
 580  CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,582)
  582 FORMAT('      ERROR READING dpst2f.dat FILE.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
C
  590 CONTINUE
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IOUTTY.EQ.'VARI')THEN
        IF(NEWNAM.EQ.'YES')THEN
          NUMNAM=NUMNAM+1
          NUMCOL=NUMCOL+1
          IHNAME(ILISTL)=IHLEFT
          IHNAM2(ILISTL)=IHLEF2
          IUSE(ILISTL)='V'
          IVALUE(ILISTL)=ICOL
          VALUE(ILISTL)=ICOL
          IN(ILISTL)=NINEW
        ELSE
          IN(ILISTL)=NINEW
        ENDIF
      ELSEIF(IOUTTY.EQ.'PARA')THEN
        VALUE(ILISTL)=AVAL
      ENDIF
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,601)IHLEFT,IHLEF2,NINEW
  601   FORMAT('THE NUMBER OF VALUES GENERATED FOR ',
     1         'THE VARIABLE ',A4,A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXFI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXFI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IBUGA3,IFOUND,IERROR
 9013   FORMAT('IBUGA3,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXFN(IANS,IANSLC,ICANS,MAXTMP,IWIDTH,NUMARG,
     1                  ISTRIN,IWORD,ICMDTI,ITEMP,
     1                  ICASE,IFILEZ,NCHAR,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--FOR VARIOUS SET COMMANDS, EXTRACT A FILE NAME.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/9
C     ORIGINAL VERSION--SEPTEMBER   2011. EXTRACT FROM DPSET
C     UPDATED         --JANUARY     2019. PYTHON PATH
C     UPDATED         --MARCH       2019. CHANGE DEFAULT BROWSER FOR
C                                         LINUX TO "xdg-open"
C     UPDATED         --NOVEMBER    2019. R PATH
C     UPDATED         --NOVEMBER    2019. LIST VIEWER
C     UPDATED         --NOVEMBER    2019. EXCEL VIEWER
C     UPDATED         --NOVEMBER    2019. WORD VIEWER
C     UPDATED         --NOVEMBER    2019. POWER POINT VIEWER
C     UPDATED         --NOVEMBER    2019. LIST LAUNCHER
C     UPDATED         --NOVEMBER    2019. FOR MACOS, USE "open" RATHER
C                                         THAN "xdg-open"
C     UPDATED         --DECEMBER    2019. PDF VIEWER
C     UPDATED         --DECEMBER    2019. IMAGE VIEWER
C     UPDATED         --DECEMBER    2019. BACKUP VIEWER
C     UPDATED         --MARCH       2020. SEARCH2 DIRECTORY
C     UPDATED         --MARCH       2020. SEARCH3 DIRECTORY
C     UPDATED         --APRIL       2020. SEARCH4 DIRECTORY
C     UPDATED         --APRIL       2020. SEARCH5 DIRECTORY
C     UPDATED         --APRIL       2020. SEARCH6 DIRECTORY
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4   ICASE
      CHARACTER*4   IANS(*)
      CHARACTER*4   IANSLC(*)
C
      CHARACTER (LEN=*) ::  ICMDTI
      CHARACTER (LEN=*) ::  IFILEZ
      CHARACTER (LEN=*) ::  ITEMP
      CHARACTER (LEN=*) ::  ICANS
      CHARACTER (LEN=*) ::  ISTRIN
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*1   IBASLC
C
      INCLUDE 'DPCOHO.INC'
C
      CHARACTER*80 PROFIL
      CHARACTER*80 P86FIL
      CHARACTER*80 APPDAT
      CHARACTER*80 COMNAM
      CHARACTER*80 UPROFI
      CHARACTER*80 DEFPRI
      CHARACTER*20 USRNAM
      CHARACTER*20 ISHELL
      CHARACTER*4  WINBIT
      COMMON/SYSVAR/PROFIL,P86FIL,APPDAT,COMNAM,UPROFI,USRNAM,DEFPRI,
     1              WINBIT,ISHELL
      COMMON/SYSVA2/NCPROF,NCP86F,NCAPPD,NCCOMP,NCUPRO,NCUSER,NCPRIN,
     1              NCSHEL
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     PATH NAMES FOR UNIX ARE CASE SENSITIVE, SO PRESERVE CASE
C
      DO1011I=1,MAXTMP
        ICANS(I:I)=IANSLC(I)
 1011 CONTINUE
      IVAL=92
      CALL DPCONA(IVAL,IBASLC)
C
      IF(NUMARG+1.LT.IWORD)THEN
        NCSTRI=0
      ELSE
        ISTART=1
        ISTOP=IWIDTH
        CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1              ICOL1,ICOL4,ISTRIN,NCSTRI,
     1              IBUGS2,ISUBRO,IERROR)
        NCSTRI=ICOL4-ICOL1+1
      ENDIF
C
C     CHECK FOR KEYWORDS THAT IMPLY DEFAULT SHOULD BE USED.
C
      CALL DPCONA(92,IBASLC)
      IFILEZ=' '
      NCTEMP=0
      IF(ICOL1.LE.0 .OR. ICOL1.GE.MAXSTR)THEN
        GOTO9000
      ELSEIF(ICOL4.LE.0 .OR. ICOL4.GE.MAXSTR)THEN
        GOTO9000
      ENDIF
C
      IF(IANS(ICOL1)(1:1).EQ.'O'.AND.IANS(ICOL1+1)(1:1).EQ.'N')THEN
        NCSTRI=0
      ELSEIF(IANS(ICOL1)(1:1).EQ.'O' .AND.
     1       IANS(ICOL1+1)(1:1).EQ.'F'.AND.
     1       IANS(ICOL1+2)(2:2).EQ.'F')THEN
        NCSTRI=0
      ELSEIF(IANS(ICOL1)(1:1).EQ.'D' .AND.
     1       IANS(ICOL1+1)(1:1).EQ.'E'.AND.
     1       IANS(ICOL1+2)(1:1).EQ.'F'.AND.
     1       IANS(ICOL1+3)(1:1).EQ.'A')THEN
        NCSTRI=0
      ELSEIF(IANS(ICOL1)(1:1).EQ.'N' .AND.
     1       IANS(ICOL1+1)(1:1).EQ.'U'.AND.
     1       IANS(ICOL1+2)(1:1).EQ.'L'.AND.
     1       IANS(ICOL1+3)(1:1).EQ.'L')THEN
        NCSTRI=0
      ELSEIF(IANS(ICOL1)(1:1).EQ.'A' .AND.
     1       IANS(ICOL1+1)(1:1).EQ.'U'.AND.
     1       IANS(ICOL1+2)(1:1).EQ.'T'.AND.
     1       IANS(ICOL1+3)(1:1).EQ.'O')THEN
        NCSTRI=0
      ENDIF
C
      IF(NCSTRI.GE.1)THEN
        NCTEMP=NCSTRI
        IFILEZ=' '
        ITEMP=' '
        ITEMP(1:NCTEMP)=ICANS(ICOL1:ICOL4)
        CALL DEQUOT(ITEMP,NCSTRI,IFILEZ,NCTEMP,IBUGS2,ISUBRO)
        IFOUND='YES'
C
      ELSE
        IF(IOPSY1.EQ.'PC-D')THEN
          IF(ICASE.EQ.'PATH' .OR. ICASE.EQ.'MPAT')THEN
            IF(WINBIT.EQ.'32')THEN
              IFILEZ(1:NCPROF)=PROFIL(1:NCPROF)
              NCTEMP=NCPROF
              NCTEMP=NCTEMP+1
              IFILEZ(NCTEMP:NCTEMP)=IBASLC
              IFILEZ(NCTEMP+1:NCTEMP+4)='NIST'
              NCTEMP=NCTEMP+4
              NCTEMP=NCTEMP+1
              IFILEZ(NCTEMP:NCTEMP)=IBASLC
              IFILEZ(NCTEMP+1:NCTEMP+8)='DATAPLOT'
              NCTEMP=NCTEMP+8
              NCTEMP=NCTEMP+1
              IFILEZ(NCTEMP:NCTEMP)=IBASLC
              NCDIR=NCTEMP
            ELSE
              IFILEZ(1:NCP86F)=P86FIL(1:NCP86F)
              NCTEMP=NCP86F
              NCTEMP=NCTEMP+1
              IFILEZ(NCTEMP:NCTEMP)=IBASLC
              IFILEZ(NCTEMP+1:NCTEMP+4)='NIST'
              NCTEMP=NCTEMP+4
              NCTEMP=NCTEMP+1
              IFILEZ(NCTEMP:NCTEMP)=IBASLC
              IFILEZ(NCTEMP+1:NCTEMP+8)='DATAPLOT'
              NCTEMP=NCTEMP+8
              NCTEMP=NCTEMP+1
              IFILEZ(NCTEMP:NCTEMP)=IBASLC
              NCDIR=NCTEMP
            ENDIF
          ELSEIF(ICASE.EQ.'PSVW')THEN
CCCCC       IFILEZ='"C: Program Files GHOSTGUM GSVIEW GSVIEW32.EXE"' 
CCCCC       IFILEZ(4:4)=IBASLC
CCCCC       IFILEZ(18:18)=IBASLC
CCCCC       IFILEZ(27:27)=IBASLC
CCCCC       IFILEZ(34:34)=IBASLC
CCCCC       NCTEMP=47
            IF(WINBIT.EQ.'64')THEN
              IFILEZ(1:NCP86F)=P86FIL(1:NCP86F)
            ELSE
              IFILEZ(1:NCPROF)=PROFIL(1:NCPROF)
            ENDIF
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+8)='GHOSTGUM'
            NCTEMP=NCTEMP+8
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+6)='GSVIEW'
            NCTEMP=NCTEMP+6
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+12)='GSVIEW32.EXE'
            NCTEMP=NCTEMP+12
          ELSEIF(ICASE.EQ.'PDVW')THEN
            IFILEZ(1:23)='C: PROGRAM FILES (x86) '
            IFILEZ(3:3)=IBASLC
            IFILEZ(23:23)=IBASLC
            IFILEZ(24:66)='ADOBE ACROBAT READER DC READER AcroRd32.EXE'
            IFILEZ(29:29)=IBASLC
            IFILEZ(47:47)=IBASLC
            IFILEZ(47:47)=IBASLC
            IFILEZ(54:54)=IBASLC
            NCTEMP=66
            IF(WINBIT.EQ.'64')THEN
              IFILEZ(1:NCP86F)=P86FIL(1:NCP86F)
            ELSE
              IFILEZ(1:NCPROF)=PROFIL(1:NCPROF)
            ENDIF
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+5)='ADOBE'
            NCTEMP=NCTEMP+5
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+17)='Acrobat Reader DC'
            NCTEMP=NCTEMP+17
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+6)='Reader'
            NCTEMP=NCTEMP+6
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+12)='AcroRd32.exe'
            NCTEMP=NCTEMP+12
          ELSEIF(ICASE.EQ.'IMVW')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'LSVW')THEN
            IFILEZ='notepad'
            NCTEMP=7
          ELSEIF(ICASE.EQ.'EXVW')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'WOVW')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'PPVW')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'LSLA')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'BROW')THEN
            IFILEZ='"C: Program Files Internet Explorer iexplore.exe"'
            IFILEZ(4:4)=IBASLC
            IFILEZ(18:18)=IBASLC
            IFILEZ(36:36)=IBASLC
            NCTEMP=49
C
CCCCC       IFILEZ(1:NCPROF)=PROFIL(1:NCPROF)
CCCCC       NCTEMP=NCTEMP+1
CCCCC       IFILEZ(NCTEMP:NCTEMP)=IBASLC
CCCCC       IFILEZ(NCTEMP+1:NCTEMP+17)='Internet Explorer'
CCCCC       NCTEMP=NCTEMP+17
CCCCC       NCTEMP=NCTEMP+1
CCCCC       IFILEZ(NCTEMP:NCTEMP)=IBASLC
CCCCC       IFILEZ(NCTEMP+1:NCTEMP+12)='iexplore.exe'
CCCCC       NCTEMP=NCTEMP+12
C
          ELSEIF(ICASE.EQ.'DPUR')THEN
            IFILEZ='http://www.itl.nist.gov/div898/software/dataplot/'
            NCTEMP=49
          ELSEIF(ICASE.EQ.'IURL')THEN
            IFILEZ='http://www.nist.gov/'
            NCTEMP=20
          ELSEIF(ICASE.EQ.'HBUR')THEN
            IFILEZ='http://www.nist.gov/div898/handbook/'
            NCTEMP=36
          ELSEIF(ICASE.EQ.'GVPA')THEN
            IF(WINBIT.EQ.'64')THEN
              IFILEZ(1:NCP86F)=P86FIL(1:NCP86F)
            ELSE
              IFILEZ(1:NCPROF)=PROFIL(1:NCPROF)
            ENDIF
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+8)='GHOSTGUM'
            NCTEMP=NCTEMP+8
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+6)='GSVIEW'
            NCTEMP=NCTEMP+6
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
          ELSEIF(ICASE.EQ.'GSPA')THEN
            IF(WINBIT.EQ.'64')THEN
              IFILEZ(1:NCP86F)=P86FIL(1:NCP86F)
            ELSE
              IFILEZ(1:NCPROF)=PROFIL(1:NCPROF)
            ENDIF
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+2)='GS'
            NCTEMP=NCTEMP+2
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+6)='GS9.27'
            NCTEMP=NCTEMP+6
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
            IFILEZ(NCTEMP+1:NCTEMP+3)='BIN'
            NCTEMP=NCTEMP+3
            NCTEMP=NCTEMP+1
            IFILEZ(NCTEMP:NCTEMP)=IBASLC
          ELSEIF(ICASE.EQ.'HHTM')THEN
            IFILEZ='NULL'
            NCTEMP=4
          ELSEIF(ICASE.EQ.'FHTM')THEN
            IFILEZ='NULL'
            NCTEMP=4
          ELSEIF(ICASE.EQ.'HLAT')THEN
            IFILEZ='NULL'
            NCTEMP=4
          ELSEIF(ICASE.EQ.'FLAT')THEN
            IFILEZ='NULL'
            NCTEMP=4
          ELSEIF(ICASE.EQ.'PYPA')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'RPAT')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'EDIT')THEN
            IFILEZ='FED'
            NCTEMP=3
          ENDIF
        ELSEIF(IOPSY1.EQ.'UNIX')THEN
          IF(ICASE.EQ.'PATH')THEN
            IFILEZ='/usr/local/lib/dataplot/'
            NCTEMP=24
          ELSEIF(ICASE.EQ.'MPAT')THEN
            IFILEZ='/usr/local/lib/dataplot/'
            NCTEMP=24
          ELSEIF(ICASE.EQ.'PSVW')THEN
CCCCC       IFILEZ='ghostview'
CCCCC       NCTEMP=9
            IF(IOPSY2.EQ.'MAC ')THEN
              IFILEZ='open'
              NCTEMP=4
            ELSE
              IFILEZ='xdg-open'
              NCTEMP=8
            ENDIF
          ELSEIF(ICASE.EQ.'PDVW')THEN
            IF(IOPSY2.EQ.'MAC ')THEN
              IFILEZ='open'
              NCTEMP=4
            ELSE
              IFILEZ='xdg-open'
              NCTEMP=8
            ENDIF
          ELSEIF(ICASE.EQ.'IMVW')THEN
            IF(IOPSY2.EQ.'MAC ')THEN
              IFILEZ='open'
              NCTEMP=4
            ELSE
              IFILEZ='xdg-open'
              NCTEMP=8
            ENDIF
          ELSEIF(ICASE.EQ.'BKVW')THEN
            IF(IOPSY2.EQ.'MAC ')THEN
              IFILEZ='/Applications/Preview.app'
              NCTEMP=25
            ELSE
              IFILEZ='okular'
              NCTEMP=6
            ENDIF
          ELSEIF(ICASE.EQ.'LSVW')THEN
            IFILEZ='vi'
            NCTEMP=2
          ELSEIF(ICASE.EQ.'LSLA')THEN
            IFILEZ='gnome-terminal -e'
            NCTEMP=17
          ELSEIF(ICASE.EQ.'EXVW')THEN
            IF(IOPSY2.EQ.'MAC ')THEN
              IFILEZ='open'
              NCTEMP=4
            ELSE
              IFILEZ='xdg-open'
              NCTEMP=8
            ENDIF
          ELSEIF(ICASE.EQ.'WOVW')THEN
            IF(IOPSY2.EQ.'MAC ')THEN
              IFILEZ='open'
              NCTEMP=4
            ELSE
              IFILEZ='xdg-open'
              NCTEMP=8
            ENDIF
          ELSEIF(ICASE.EQ.'PPVW')THEN
            IF(IOPSY2.EQ.'MAC ')THEN
              IFILEZ='open'
              NCTEMP=4
            ELSE
              IFILEZ='xdg-open'
              NCTEMP=8
            ENDIF
          ELSEIF(ICASE.EQ.'BROW')THEN
            IF(IOPSY2.EQ.'MAC ')THEN
              IFILEZ='open'
              NCTEMP=4
            ELSE
              IFILEZ='xdg-open'
              NCTEMP=8
            ENDIF
CCCCC       IFILEZ='firefox'
CCCCC       NCTEMP=7
          ELSEIF(ICASE.EQ.'DPUR')THEN
            IFILEZ='https://www.itl.nist.gov/div898/software/dataplot/'
            NCTEMP=50
          ELSEIF(ICASE.EQ.'IURL')THEN
            IFILEZ='https://www.nist.gov/'
            NCTEMP=21
          ELSEIF(ICASE.EQ.'HBUR')THEN
            IFILEZ='https://www.nist.gov/div898/handbook/'
            NCTEMP=37
          ELSEIF(ICASE.EQ.'GVPA')THEN
            IFILEZ='/usr/bin/'
            NCTEMP=9
          ELSEIF(ICASE.EQ.'GSPA')THEN
            IFILEZ='/usr/bin/'
            NCTEMP=9
          ELSEIF(ICASE.EQ.'HHTM')THEN
            IFILEZ='NULL'
            NCTEMP=4
          ELSEIF(ICASE.EQ.'FHTM')THEN
            IFILEZ='NULL'
            NCTEMP=4
          ELSEIF(ICASE.EQ.'HLAT')THEN
            IFILEZ='NULL'
            NCTEMP=4
          ELSEIF(ICASE.EQ.'FLAT')THEN
            IFILEZ='NULL'
            NCTEMP=4
          ELSEIF(ICASE.EQ.'PYPA')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'RPAT')THEN
            IFILEZ=' '
            NCTEMP=0
          ELSEIF(ICASE.EQ.'EDIT')THEN
            IFILEZ='FED'
            NCTEMP=3
          ENDIF
        ENDIF
      ENDIF
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1021)ICMDTI
 1021   FORMAT(A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1021)IFILEZ
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 9000 CONTINUE
      IFOUND='YES'
      NCHAR=NCTEMP
      RETURN
      END
      SUBROUTINE DPEXIN(IHARG,IARGT,IARG,NUMARG,ISTART,ISTOP,
     1                  MININT,MAXINT,
     1                  ITAB,NTAB,MAXTAB,
     1                  IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--SCAN THE ARGUMENTS IN IHARG(.)
C              AND EXTRACT THE INTEGER SEQUENCES
C              ALLOWING FOR THE USE OF THE   TO   CONNECTOR
C              TO IMPLY ALL  INTERMEDIATE INTEGERS.
C     EXAMPLE--12 9 4 2 (IN THE INPUT VECTOR IHARG(.))
C              WOULD BECOME
C              12 9 4 2 (IN THE OUTPUT VECTOR ITAB(.))
C     EXAMPLE--12 TO 9 6 4 TO 2 (IN THE INPUT VECTOR IHARG(.))
C              WOULD BECOME
C              12 11 10 9 6 4 3 2 (IN THE OUTPUT VECTOR ITAB(.))
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/7
C     ORIGINAL VERSION--APRIL     1986.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ITAB(*)
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ITOSW
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='IN  '
      IERROR='NO'
C
      IV1=0
      IV2=0
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXIN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR
   53   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NUMARG,ISTART,ISTOP,MININT,MAXINT,MAXTAB
   54   FORMAT('NUMARG,ISTART,ISTOP,MININT,MAXINT,MAXTAB = ',6I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMARG.GE.1)THEN
          DO55I=1,NUMARG
            WRITE(ICOUT,56)I,IHARG(I),IARGT(I),IARG(I)
   56       FORMAT('I,IHARG(I),IARGT(I),IARG(I) = ',I8,2(2X,A4),I8)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
      ENDIF
C
C               **************************************
C               **  STEP 11--                       **
C               **  INITIALIZE THE OUTPUT VARIABLES **
C               **************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NTAB=0
      DO1100I=1,MAXTAB
        ITAB(I)=(-999)
 1100 CONTINUE
C
      IF(NUMARG.LE.0)GOTO9000
C
C               *******************************************
C               **  STEP 12--                            **
C               **  CHECK THE INPUT ARGUMENTS            **
C               *******************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.GE.1.AND.ISTOP.GE.1.AND.
     1   ISTART.LE.NUMARG.AND.ISTOP.LE.NUMARG)GOTO1219
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      ISTART OR ISTOP IS < 1 OR > NUMARG   .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1213)ISTART
 1213 FORMAT('      ISTART  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)ISTOP
 1214 FORMAT('      ISTOP   = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)NUMARG
 1215 FORMAT('      NUMARG  = ',I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1219 CONTINUE
C
      IF(ISTART.LE.ISTOP)GOTO1229
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1221)
 1221 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1222)
 1222 FORMAT('      ISTART IS GREATER THAN ISTOP')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1223)ISTART
 1223 FORMAT('      ISTART  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1224)ISTOP
 1224 FORMAT('      ISTOP   = ',I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1229 CONTINUE
C
      IF(MININT.GE.1.AND.MAXINT.GE.1.AND.
     1   MININT.LE.MAXTAB.AND.MAXINT.LE.MAXTAB)GOTO1239
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1231)
 1231 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1232)
 1232 FORMAT('      MININT OR MAXINT IS < 1 OR > MAXTAB   .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1233)MININT
 1233 FORMAT('      MININT  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1234)MAXINT
 1234 FORMAT('      MAXINT   = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1235)MAXTAB
 1235 FORMAT('      MAXTAB  = ',I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1239 CONTINUE
C
      IF(MININT.LE.MAXINT)GOTO1249
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1241)
 1241 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1242)
 1242 FORMAT('      MININT IS GREATER THAN MAXINT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1243)MININT
 1243 FORMAT('      MININT  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1244)MAXINT
 1244 FORMAT('      MAXINT   = ',I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1249 CONTINUE
C
      DO1250I=ISTART,ISTOP
      I2=I
      IF(IARGT(I).EQ.'NUMB')GOTO1250
      IF(IHARG(I).EQ.'TO  ')GOTO1250
      GOTO1260
 1250 CONTINUE
      GOTO1269
C
 1260 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1261)
 1261 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1262)
 1262 FORMAT('      AN ERROR OCCURRED IN PARSING A SEQUENCE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1263)
 1263 FORMAT('      IN SUCH A SEQUENCE, EVERY WORD')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1264)
 1264 FORMAT('      MUST BE A PRE-EXISTING PARAMETER, OR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1265)
 1265 FORMAT('      MUST BE THE WORD   TO    .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1266)
 1266 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1267)IHARG(I2)
 1267 FORMAT('      THE OFFENDING WORD WAS ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1268)I2,IARGT(I2),IARG(I2)
 1268 FORMAT('      I2,IARGT(I2),IARG(I2) = ',I8,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1269 CONTINUE
C
      DO1270I=ISTART,ISTOP
        I2=I
        IF(IHARG(I).EQ.'TO  ')GOTO1270
        IX=IARG(I2)
        IF(MININT.LE.IX.AND.IX.LE.MAXINT)GOTO1270
        GOTO1280
 1270 CONTINUE
      GOTO1299
C
 1280 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)
 1282 FORMAT('      AN ERROR OCCURRED IN PARSING A SEQUENCE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1283)
 1283 FORMAT('      IN SUCH A SEQUENCE, EVERY PARAMETER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1284)MININT
 1284 FORMAT('      MUST BE BETWEEN ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1285)MAXINT
 1285 FORMAT('      AND ',I8,' (INCLUSIVE).')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1286)
 1286 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1287)I2
 1287 FORMAT('      ARGUMENT ',I8,' WAS OUT-OF-BOUNDS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1288)IHARG(I2)
 1288 FORMAT('      THE ARGUMENT       = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1289)IX
 1289 FORMAT('      ITS VALUE          = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1290)MININT
 1290 FORMAT('      ALLOWABLE MINIMUM  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1291)MAXINT
 1291 FORMAT('      ALLOWABLE MAXIMUM  = ',I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1299 CONTINUE
C
      I=ISTART
      I2=I
      IF(IHARG(I).EQ.'TO  ')GOTO1310
      GOTO1319
 1310 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1311)
 1311 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1312)
 1312 FORMAT('      AN ERROR OCCURRED IN PARSING A SEQUENCE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1313)
 1313 FORMAT('      THE FIRST WORD IN THE SEQUENCE WAS   TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1314)I2,ISTART
 1314 FORMAT('I2,ISTART = ',2I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1319 CONTINUE
C
      I=ISTOP
      I2=I
      IF(IHARG(I).EQ.'TO  ')GOTO1320
      GOTO1329
 1320 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1321)
 1321 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1322)
 1322 FORMAT('      AN ERROR OCCURRED IN PARSING A SEQUENCE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1323)
 1323 FORMAT('      THE LAST WORD IN THE SEQUENCE WAS   TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1324)I2,ISTART
 1324 FORMAT('I2,ISTART = ',2I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1329 CONTINUE
C
C               ************************************
C               **  STEP 21--                     **
C               **  GENERATE THE SEQUENCE         **
C               ************************************
C
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITOSW='OFF'
      K=0
      NTAB=0
      DO2100I=ISTART,ISTOP
      IF(IHARG(I).EQ.'TO  ')GOTO2110
      IF(ITOSW.EQ.'ON')GOTO2120
      GOTO2130
C
 2110 CONTINUE
      ITOSW='ON'
      GOTO2100
C
 2120 CONTINUE
      IV2=IARG(I)
      IF(IV1.LT.IV2)GOTO2121
      GOTO2126
 2121 CONTINUE
      DO2122J=IV1,IV2
      IF(J.EQ.IV1)GOTO2122
      K=K+1
      IF(K.GT.MAXTAB)GOTO2180
      ITAB(K)=J
 2122 CONTINUE
      GOTO2129
 2126 CONTINUE
      DO2127J=IV2,IV1
      IF(J.EQ.IV2)GOTO2127
      JREV=IV1-J+IV2
      K=K+1
      IF(K.GT.MAXTAB)GOTO2180
      ITAB(K)=JREV
 2127 CONTINUE
      GOTO2129
 2129 CONTINUE
      ITOSW='OFF'
      GOTO2100
C
 2130 CONTINUE
      K=K+1
      IF(K.GT.MAXTAB)GOTO2180
      IV1=IARG(I)
      ITAB(K)=IV1
      GOTO2100
C
 2100 CONTINUE
      NTAB=K
      GOTO9000
C
 2180 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2181)
 2181 FORMAT('***** ERROR IN DPEXIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2182)
 2182 FORMAT('      AN ERROR OCCURRED IN FORMING A SEQUENCE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2183)
 2183 FORMAT('      THE NUMBER OF ELEMENTS RESULTING')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2184)
 2184 FORMAT('      FROM FORMING SUCH A SEQUENCE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2185)MAXTAB
 2185 FORMAT('      MUST NOT EXCEED ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2186)
 2186 FORMAT('      BUT JUST HAS.')
      CALL DPWRST('XXX','BUG ')
      KM1=K-1
      WRITE(ICOUT,2187)KM1,ITAB(KM1)
 2187 FORMAT('      KM1,ITAB(KM1) = ',2I8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXIN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IBUGS2,ISUBRO,IERROR
 9013   FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IERROR,ISTART,ISTOP,MININT,MAXINT
 9021   FORMAT('IERROR,ISTART,ISTOP,MININT,MAXINT = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)NTAB,MAXTAB
 9031   FORMAT('NTAB,MAXTAB = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NTAB.GE.1)THEN
          DO9032I=1,NTAB
            WRITE(ICOUT,9033)I,ITAB(I)
 9033       FORMAT('I,ITAB(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9032     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXIT(ICAPSW,IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--EXIT FROM DATAPLOT.
C     NOTE--IN THE PROCESS OF EXITING, CERTAIN FILE OPERATIONS MUST BE DONE TO
C           KEEP FILES TIDY.  IN PARTICULAR, CLOSE NO FILES PER SE, BUT DO PUT
C           END OF FILE MARKS ON SOME FILES.  FOR MOST FILES, NOTHING AT ALL
C           NEED BE DONE.
C     NOTE--FOR ANY FILES THAT ARE ALREADY CLOSED (E.G., THE DATAPLOT PERMANENT
C           FILES SUCH AS MESSAGE, NEWS, HELP, ETC., PLUS OTHER FILES SUCH AS
C           SAVE, LIST, ETC.)--
C           DO NOTHING.
C     NOTE--FOR ANY FILES THAT ARE OPEN AND MAY HAVE HAD WRITING GO INTO THE
C           FILE, (E.G., THE WRITE FILE, THE PLOT1-FILE, THE PLOT-2 FILE,
C           ETC.)--
C           PUT AN END OF FILE, BUT DO NOT CLOSE IT.
C     NOTE--ON SOME COMPUTER SYSTEMS, CLOSING A FILE--ESPECIALLY A 'TEMPORARY'
C           FILE--HAS THE EFFECT OF DELETING THE FILE FROM THE SYSTEM WHICH
C           MEANS THE USER HAS NO ACCESS TO IT AFTER EXITING OUT OF DATAPLOT.
C           THIS IS COUNTER-PRODUCTIVE FOR SOME OF THE DATAPLOT-CREATED FILES
C           SUCH AS THE PLOT-1 FILE, THE PLOT-2 FILE, AND THE CONCLUSIONS FILE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --APRIL     1979.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1986.
C     UPDATED         --FEBRUARY  1989. CALL GREXIT  (ALAN HECKERT))
C     UPDATED         --AUGUST    1986. CLOSE & EXIT WINDOW SYSTEM
C     UPDATED         --AUGUST    1986. WINDOW SYSTEM COMMON
C     UPDATED         --JULY      1991. COMMENT OUT WINDOW SYS.
C     UPDATED         --APRIL     1992. FIX PC DEVICE 2 CLOSE EXIT BOMB
C     UPDATED         --MAY       1992. ADD ----- TO OUTPUT
C     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
C     UPDATED         --FEBRUARY  2006. ONLY CALL GREXIT IF DEVICE
C                                       POWER IS ON
C     UPDATED         --AUGUST    2016. SUPPRESS THE "ENDFILE" OF THE
C                                       PLOT FILE.  IF THE PLOT FILE
C                                       CANNOT BE OPENED WITH WRITE
C                                       ACCESS, WE DO NOT WANT DATAPLOT
C                                       TO CRASH.
C     UPDATED         --DECEMBER  2018. CHECK FOR DISCRETE, NULL, OR
C                                       NONE DEVICE
C     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
C                                       COMMAND
C     UPDATED         --MARCH     2019. IF "SET POSTSCRIPT CONVERT" IS
C                                       SET, CALL DPDEP3 TO DO THE
C                                       REQUESTED CONVERSION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
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
      CHARACTER*4 IOPERA
      CHARACTER*4 IGENID
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IPOWER
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHK.INC'
CCCCC THE FOLLOWING WINDOW SYSTEM COMMON WAS ADDED AUGUST 1990
      INCLUDE 'DPCOWI.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
      ISUBN1='DPEX'
      ISUBN2='IT  '
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,IHOST1,IHOST2,ISITE
   52   FORMAT('IBUGS2,ISUBRO,IHOST1,IHOST2,ISITE = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)IWRINU,IWRIST,IWRICS
   61   FORMAT('IWRINU,IWRIST,IWRICS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)IWRINA
   62   FORMAT('IWRINA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ICRENU,ICREST,ICRECS
   63   FORMAT('ICRENU,ICREST,ICRECS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)ICRENA
   64   FORMAT('ICRENA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IPL1NU,IPL1ST,IPL1CS
   71   FORMAT('IPL1NU,IPL1ST,IPL1CS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IPL1NA
   72   FORMAT('IPL1NA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)IPL2NU,IPL2ST,IPL2CS
   73   FORMAT('IPL2NU,IPL2ST,IPL2CS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)IPL2NA
   74   FORMAT('IPL2NA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,75)ICONNU,ICONST,ICONCS
   75   FORMAT('ICONNU,ICONST,ICONCS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,76)ICONNA
   76   FORMAT('ICONNA = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC THE FOLLOWING ENTIRE SECTION (DECTION 20) WAS INSERTED (FEBRUARY 1989)
CCCCC BY ALAN BECAUSE SOME DEVICES, NAMELY LASER PRINTERS,   (FEBRUARY 1989)
CCCCC MAY NEED A "TERMINATE" ROUTINE                         (FEBRUARY 1989)
C
C               ********************************************
C               **  STEP 20--                             **
C               **  CALL GREXIT FOR EACH DEVICE           **
C               ********************************************
C
      ISTEPN='20'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1000IDEV=1,NUMDEV
CCCCC   THE FOLLOWING 2 LINES WERE ADDED    APRIL 1992 (ALAN)
CCCCC   TO PREVENT PC EXIT BOMB             APRIL 1992 (ALAN)
CCCCC   AFTER     DEVICE 2 CLOSE            APRIL 1992 (ALAN)
CCCCC   IF(IDEV.EQ.2.AND.IPL1CS.EQ.'CLOSED')GOTO1000
CCCCC   IF(IDEV.EQ.3.AND.IPL2CS.EQ.'CLOSED')GOTO1000
        IPOWER=IDPOWE(IDEV)
        IMANUF=IDMANU(IDEV)
        IMODEL=IDMODE(IDEV)
        IMODE2=IDMOD2(IDEV)
        IMODE3=IDMOD3(IDEV)
        IGCODE=IDCODE(IDEV)
        IGUNIT=IDUNIT(IDEV)
        NUMHPP=IDNHPP(IDEV)
        ANUMHP=NUMHPP
        NUMVPP=IDNVPP(IDEV)
        ANUMVP=NUMVPP
        IGCOLO=IDCOLO(IDEV)
        IGFONT=IDFONT(IDEV)
        IGBAUD=IDBAUD(IDEV)
        PCHSCA=PDSCAL(IDEV)
        ISOFT=IDSOFT(IDEV)
        ISOFT2=IDSOF2(IDEV)
        ISOFT3=IDSOF3(IDEV)
        IF(IPOWER.EQ.'ON')CALL GREXIT
 1000 CONTINUE
C
C               ********************************************
C               **  STEP 21--                             **
C               **  IF THE WRITE FILE IS STILL OPEN,      **
C               **  PUT AN    END OF FILE    ON IT.       **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=IWRINU
      IFILE=IWRINA
      ISTAT=IWRIST
      IFORM=IWRIFO
      IACCES=IWRIAC
      IPROT=IWRIPR
      ICURST=IWRICS
      ISUBN0='EXIT'
      IERRFI='NO'
C
      IF(ISTAT.EQ.'NONE')GOTO2190
      IF(ICURST.EQ.'CLOSED')GOTO2190
CCCCC ENDFILE IOUNIT
 2190 CONTINUE
C
C               ********************************************
C               **  STEP 22--                             **
C               **  IF THE MACRO FILE IS STILL OPEN,      **
C               **  PUT AN    END OF FILE    ON IT.       **
C               ********************************************
C
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=ICRENU
      IFILE=ICRENA
      ISTAT=ICREST
      IFORM=ICREFO
      IACCES=ICREAC
      IPROT=ICREPR
      ICURST=ICRECS
      ISUBN0='EXIT'
      IERRFI='NO'
C
      IF(ISTAT.EQ.'NONE')GOTO2290
      IF(ICURST.EQ.'CLOSED')GOTO2290
CCCCC ENDFILE IOUNIT
 2290 CONTINUE
C
C               ********************************************
C               **  STEP 23--                             **
C               **  IF THE PLOT-1 FILE IS STILL OPEN,     **
C               **  PUT AN    END OF FILE    ON IT.       **
C               ********************************************
C
      ISTEPN='23'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=IPL1NU
      IFILE=IPL1NA
      ISTAT=IPL1ST
      IFORM=IPL1FO
      IACCES=IPL1AC
      IPROT=IPL1PR
      ICURST=IPL1CS
      ISUBN0='EXIT'
      IERRFI='NO'
C
      IF(IPSTDV.NE.'NULL')THEN
        IF(IDMANU(2).NE.'GD  ' .AND. IDMANU(2).NE.'GDI ' .AND.
     1     IDMANU(2).NE.'CAIR')THEN
           IENDFI='ON'
           IREWIN='OFF'
           CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                 IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
           IOPERA='CLOS'
           IGENNU=IPL1NU
           IGENID='PLO1'
           CALL DPDEP3(IOPERA,IGENNU,IGENID,ICAPSW,IFILE,
     1                 IBUGS2,ISUBRO,IFOUND,IERROR)
C
        ENDIF
      ENDIF
      IF(ISTAT.EQ.'NONE')GOTO2390
      IF(ICURST.EQ.'CLOSED')GOTO2390
CCCCC ENDFILE IOUNIT
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
CCCCC THE FOLLOWING 2 LINES WERE ADDED MAY 1992 (JJF)
      WRITE(ICOUT,2310)
 2310 FORMAT('-----------------------------------------------')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2311)
 2311 FORMAT('NOTE--DEVICE 2 (A FILE CONTAINING PLOT IMAGES) ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2312)
 2312 FORMAT('      HAS JUST BEEN CLOSED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2313)IOUNIT
 2313 FORMAT('      FILE NUMBER = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2314)IFILE
 2314 FORMAT('      FILE NAME   = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2316)
 2316 FORMAT('NOTE--TO EXAMINE THE FILE, USE ANY EDITOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2317)
 2317 FORMAT('      AND SIMPLY PRINT THE FILE CONTENTS.')
      CALL DPWRST('XXX','BUG ')
CCCCC THE FOLLOWING LINE WAS ADDED MAY 1992 (JJF)
      WRITE(ICOUT,2310)
      CALL DPWRST('XXX','BUG ')
C
 2390 CONTINUE
C
C               ********************************************
C               **  STEP 24--                             **
C               **  IF THE PLOT-2 FILE IS STILL OPEN,     **
C               **  PUT AN    END OF FILE    ON IT.       **
C               ********************************************
C
      ISTEPN='24'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=IPL2NU
      IFILE=IPL2NA
      ISTAT=IPL2ST
      IFORM=IPL2FO
      IACCES=IPL2AC
      IPROT=IPL2PR
      ICURST=IPL2CS
      ISUBN0='EXIT'
      IERRFI='NO'
C
      IF(ISTAT.EQ.'NONE')GOTO2490
      IF(ICURST.EQ.'CLOSED')GOTO2490
CCCCC ENDFILE IOUNIT
C
      IF(IPSTDV.NE.'NULL')THEN
        IF(IDMANU(3).NE.'GD  ' .AND. IDMANU(3).NE.'GDI ' .AND.
     1     IDMANU(3).NE.'CAIR')THEN
           IENDFI='ON'
           IREWIN='OFF'
           CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                 IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
           IOPERA='CLOS'
           IGENNU=IPL2NU
           IGENID='PLO2'
           CALL DPDEP3(IOPERA,IGENNU,IGENID,ICAPSW,IFILE,
     1                 IBUGS2,ISUBRO,IFOUND,IERROR)
C
        ENDIF
      ENDIF
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
CCCCC THE FOLLOWING 2 LINES WERE ADDED MAY 1992 (JJF)
      WRITE(ICOUT,2410)
 2410 FORMAT('-----------------------------------------------')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2411)
 2411 FORMAT('NOTE--DEVICE 3 (A FILE CONTAINING PLOT IMAGES) ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2412)
 2412 FORMAT('      HAS JUST BEEN CLOSED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2413)IOUNIT
 2413 FORMAT('      FILE NUMBER = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2414)IFILE
 2414 FORMAT('      FILE NAME   = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2416)
 2416 FORMAT('NOTE--TO EXAMINE THE FILE, USE ANY EDITOR,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2417)
 2417 FORMAT('      AND SIMPLY PRINT THE FILE CONTENTS.')
      CALL DPWRST('XXX','BUG ')
CCCCC THE FOLLOWING LINE WAS ADDED MAY 1992 (JJF)
      WRITE(ICOUT,2410)
      CALL DPWRST('XXX','BUG ')
C
 2490 CONTINUE
C
C               ********************************************
C               **  STEP 25--                             **
C               **  IF THE CONCLUSIONS FILE IS STILL OPEN,**
C               **  PUT AN    END OF FILE    ON IT.       **
C               ********************************************
C
      ISTEPN='25'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=ICONNU
      IFILE=ICONNA
      ISTAT=ICONST
      IFORM=ICONFO
      IACCES=ICONAC
      IPROT=ICONPR
      ICURST=ICONCS
      ISUBN0='EXIT'
      IERRFI='NO'
C
      IF(ISTAT.EQ.'NONE')GOTO2590
      IF(ICURST.EQ.'CLOSED')GOTO2590
      ENDFILE IOUNIT
 2590 CONTINUE
C
C               ***************************
C               **  STEP 80--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8011)
 8011 FORMAT('THIS IS AN EXIT FROM DATAPLOT.')
      CALL DPWRST('XXX','BUG ')
C
CCCCC THE FOLLOWING SECTION WAS ADDED AUGUST 1990
C               ***********************************
C               **  IF IN A WINDOW SYSTEM,       **
C               **  CLOSE THE WINDOW, AND        **
C               **  EXIT FROM THE WINDOW SYSTEM  **
C               ***********************************
C
CCCCC THE FOLLOWING WAS COMMENTED OUT IN JULY 1991   JJF
C
CCCCC IF(IWINSY.EQ.'NONE')GOTO8190
CCCCC CALL WISEWI(1)
CCCCC CALL WICLWI('OFF ','OFF ')
CCCCC CALL WIEXWS('OFF ')
C8190 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXIT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IWRINU,IWRIST,IWRICS
 9021   FORMAT('IWRINU,IWRIST,IWRICS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ICRENU,ICREST,ICRECS
 9023   FORMAT('ICRENU,ICREST,ICRECS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)IPL1NU,IPL1ST,IPL1CS
 9031   FORMAT('IPL1NU,IPL1ST,IPL1CS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9033)IPL2NU,IPL2ST,IPL2CS
 9033   FORMAT('IPL2NU,IPL2ST,IPL2CS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9035)ICONNU,ICONST,ICONCS
 9035   FORMAT('ICONNU,ICONST,ICONCS = ',I8,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)IFOUND,IERROR
 9041   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      STOP
      END
      SUBROUTINE DPEXEC(IANSEX,IWIDEX,IBUGMA,IFOUND,IERROR)
C
C     PURPOSE--TRANSFORM THE STRING (WITH ALL SUBSTITUTIONS MADE)
C              FOR THE    EXECUTE STRING   COMMAND
C     INPUT  --A COMMAND LINE STARTING WITH    EXECUTE STRING
C              (THIS COMMAND LINE IS IN IANS(.)--IN COMMON)
C     OUTPUT --A TRANSFORMED COMMAND LINE IN WHICH THE 2 LEAD WORDS
C              EXECUTE STRING   HAVE BEEN DELETED,
C              AND THE TRAILING WORDS BECOME THE NEW COMMAND LINE.
C              NOTE ALSO THAT IF ANY OF THE TRAILING WORDS ARE FUNCTION
C              (= STRING) NAMES, THEN THE WORDS THEMSELVES WILL HAVE BEEN
C              REPLACED BY THE STRINGS.
C              (THE OUTPUT STRING IS IN IANSEX(.))
C     EXAMPLE--LET FUNCTION F = CALIBRATION ANALYSIS
C              EXECUTE STRING TITLE F
C                 WILL RESULT IN THE FOLLOWING COMMAND LINE--
C              TITLE CALIBRATION ANALYSIS
C                 BEING EXECUTED.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--85/7
C     ORIGINAL VERSION--JULY      1985.
C     UPDATED         --FEBRUARY  1994. CHECK FOR X CHART, X CONTROL CHART
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*1 IANSEX
C
      CHARACTER*4 IBUGMA
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASE
      CHARACTER*4 IF4
C
      CHARACTER*4 IWD1
      CHARACTER*4 IWD2
      CHARACTER*4 IWD12
      CHARACTER*4 IWD22
      CHARACTER*4 IFOUN1
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IANSEX(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='EC  '
      IFOUND='NO'
      IERROR='NO'
      ICASE='NONE'
      IFOUN1='NO'
C
C               ******************************************
C               **  TREAT THE    EXECUTE STRING   CASE  **
C               ******************************************
C
      IF(IBUGMA.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPEXEC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGMA,IFOUND,IERROR
   52 FORMAT('IBUGMA,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NUMNAM
   53 FORMAT('NUMNAM = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMNAM
      WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)
   56 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',
     1I8,2X,A4,A4,2X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,57)NUMCHF,MAXCHF
   57 FORMAT('NUMCHF,MAXCHF = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)(IFUNC(I),I=1,MAXCHF)
   60 FORMAT('IFUNC(.)  = ',120A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)IWIDTH,MAXWID
   61 FORMAT('IWIDTH,MAXWID = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)(IANS(I),I=1,IWIDTH)
   62 FORMAT('(IANS(.) = ',110A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **********************************
C               **  STEP 11--                   **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='11'
      IF(IBUGMA.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWIDEX=(-999)
      MAXN2=MAXCHF
      MAXN3=MAXCHF
C
      DO1100I=1,1000
      IANSEX(I)=' '
 1100 CONTINUE
C
C               ***********************************************************
C               **  STEP 12--                                            **
C               **  CHECK TO SEE IF HAVE THE   EXECUTE STRING   COMMAND  **
C               ***********************************************************
C
      ISTEPN='12'
      IF(IBUGMA.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IWIDTH.EQ.1.AND.
     1IANS(1).EQ.'X')GOTO1210
C
      IF(IWIDTH.GE.2.AND.
     1IANS(1).EQ.'X'.AND.IANS(2).EQ.' ')GOTO1210
C
      IF(IWIDTH.GE.14.AND.
     1IANS(1).EQ.'E'.AND.IANS(2).EQ.'X'.AND.
     1IANS(3).EQ.'E'.AND.IANS(4).EQ.'C'.AND.
     1IANS(5).EQ.'U'.AND.IANS(6).EQ.'T'.AND.
     1IANS(7).EQ.'E'.AND.IANS(8).EQ.' '.AND.
     1IANS(9).EQ.'S'.AND.IANS(10).EQ.'T'.AND.
     1IANS(11).EQ.'R'.AND.IANS(12).EQ.'I'.AND.
     1IANS(13).EQ.'N'.AND.IANS(14).EQ.'G')GOTO1220
C
      GOTO1230
C
 1210 CONTINUE
CCCCC CHECK FOR X CHART OR X CONTROL CHART.  FEBRUARY 1994.
      IF(IWIDTH.GE.7.AND.
     1IANS(1).EQ.'X'.AND.IANS(2).EQ.' '.AND.
     1IANS(3).EQ.'C'.AND.IANS(4).EQ.'H'.AND.
     1IANS(5).EQ.'A'.AND.IANS(6).EQ.'R'.AND.
     1IANS(7).EQ.'T')GOTO1230
      IF(IWIDTH.GE.15.AND.
     1IANS(1).EQ.'X'.AND.IANS(2).EQ.' '.AND.
     1IANS(3).EQ.'C'.AND.IANS(4).EQ.'O'.AND.
     1IANS(5).EQ.'N'.AND.IANS(6).EQ.'T'.AND.
     1IANS(7).EQ.'R'.AND.IANS(8).EQ.'O'.AND.
     1IANS(9).EQ.'L'.AND.IANS(10).EQ.' '.AND.
     1IANS(11).EQ.'C'.AND.IANS(12).EQ.'H'.AND.
     1IANS(13).EQ.'A'.AND.IANS(14).EQ.'R'.AND.
     1IANS(15).EQ.'T')GOTO1230
      IF(IWIDTH.GE.6.AND.
     1IANS(1).EQ.'X'.AND.IANS(2).EQ.' '.AND.
     1IANS(3).EQ.'C'.AND.IANS(4).EQ.'O'.AND.
     1IANS(5).EQ.'N'.AND.IANS(6).EQ.'T')GOTO1230
      IFOUND='YES'
      ICASE='X   '
      GOTO1290
C
 1220 CONTINUE
      IFOUND='YES'
      ICASE='EXEC'
      GOTO1290
C
 1230 CONTINUE
      IFOUND='NO'
      ICASE='NONE'
      GOTO9000
C
 1290 CONTINUE
C
C               ***************************************************************
C               **  STEP 13--                                                **
C               **  EXTRACT THE RIGHT-SIDE                                   **
C               **  EXPRESSION FROM THE INPUT COMMAND LINE                   **
C               **  (STARTING WITH THE FIRST NON-BLANK LOCATION AFTER THE    **
C               **  WORD   STRING   OF    EXECUTE STRING                     **
C               **  AND ENDING WITH THE END OF THE LINE                      **
C               ***************************************************************
C
      ISTEPN='13'
      IF(IBUGMA.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWD1='X   '
      IWD12='    '
      IF(ICASE.EQ.'EXEC')IWD1='STRI'
      IF(ICASE.EQ.'EXEC')IWD12='NG  '
      IWD2='    '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1IFUNC2,N2,IBUGMA,IFOUN1,IERROR)
      IF(IFOUN1.EQ.'NO')GOTO1310
      IF(IERROR.EQ.'YES')GOTO1310
      GOTO1390
C
 1310 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1311)
 1311 FORMAT('***** ERROR IN DPEXEC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1312)
 1312 FORMAT('      INTERNAL ERROR--AT 3101 AFTER CALL TO DPEXST.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1313)
 1313 FORMAT('      ERROR IN EXTRACTING TRAILING STRING.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1315)
 1315 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,1316)(IANS(I),I=1,IWIDTH)
 1316 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1317)IFOUN1,IERROR
 1317 FORMAT('IFOUN1,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 1390 CONTINUE
C
C               ***********************************************************
C               **  STEP 14--                                            **
C               **  DETERMINE IF THE EXPRESSION HAS ANY STRING   NAMES   **
C               **  INBEDDED.  IF SO, REPLACE THE STRING   NAMES         **
C               **  BY EACH STRING  'S DEFINITION.  DO SO REPEATEDLY     **
C               **  UNTIL ALL STRING   REFERENCES HAVE BEEN ANNIHILATED  **
C               **  AND THE EXPRESSION IS LEFT ONLY WITH                 **
C               **  CONSTANTS, PARAMETERS, AND VARIABLES--NO STRING  S.  **
C               **  PLACE THE RESULTING FUNCTIONAL EXPRESSION INTO IFUNC3(.) **
C               ***********************************************************
C
      ISTEPN='14'
      IF(IBUGMA.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,N3,MAXN3,
     1IBUGMA,IERROR)
      IF(IERROR.EQ.'YES')GOTO1410
      GOTO1490
C
 1410 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1411)
 1411 FORMAT('***** ERROR IN DPEXEC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1412)
 1412 FORMAT('      INTERNAL ERROR--AT 1401 AFTER CALL TO DPEXFU.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1413)
 1413 FORMAT('      ERROR IN TRANSFORMING TRAILING STRING.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1415)
 1415 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,1416)(IANS(I),I=1,IWIDTH)
 1416 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1417)IERROR
 1417 FORMAT('IERROR = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 1490 CONTINUE
C
C               *******************************************
C               **  STEP 15--                            **
C               **  FORM THE TRANSFORMED COMMAND STRING  **
C               *******************************************
C
      ISTEPN='15'
      IF(IBUGMA.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
 
      IWIDEX=N3
      IF(N3.LT.0)IWIDEX=0
      IF(N3.GT.MAXWID)IWIDEX=MAXWID
      IF(IWIDEX.LE.0)GOTO1590
      DO1500I=1,IWIDEX
      IF4=IFUNC3(I)
      IANSEX(I)=IF4(1:1)
 1500 CONTINUE
 1590 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGMA.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPEXEC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGMA,IFOUND,IERROR,ICASE
 9012 FORMAT('IBUGMA,IFOUND,IERROR,ICASE = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IFOUN1
 9013 FORMAT('IFOUN1 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMNAM
 9014 FORMAT('NUMNAM = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NUMNAM
      WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)
 9016 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',
     1I8,2X,A4,A4,2X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9017)N2,N3,NUMCHF,MAXN2,MAXN3,MAXCHF
 9017 FORMAT('N2,N3,NUMCHF,MAXN2,MAXN3,MAXCHF = ',6I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)(IFUNC2(I),I=1,N2)
 9018 FORMAT('IFUNC2(.) = ',120A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)(IFUNC3(I),I=1,N3)
 9019 FORMAT('IFUNC3(.) = ',120A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)(IFUNC(I),I=1,MAXCHF)
 9020 FORMAT('IFUNC(.)  = ',120A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)IWIDEX
 9021 FORMAT('IWIDEX = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)(IANSEX(I),I=1,IWIDEX)
 9022 FORMAT('(IANSEX(.) = ',110A1)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,N3,MAXN3,
     1IBUGA3,IERROR)
C
C     PURPOSE--SCAN A STRING FOR FUNCTION NAMES;
C              REPLACE FUNCTION NAMES BY FUNCTION EXPRESSIONS;
C              DO SO RECURSIVELY UNTIL ALL FUNCTION NAMES
C              HAVE BEEN ANNIHILATED AND THERE REMAINS ONLY
C              AN EXPRESSION IN CONSTANTS, PARAMETERS,
C              VARIABLES--NO FUNCTIONS.
C     NOTE--THE INPUT STRING IS IN IFUNC2(.).
C           THE OUTPUT EXPRESSION WILL BE IN IFUNC3(.).
C     NOTE--IF SO DESIRED, THE OUTPUT VECTOR IFUNC3(.)
C           MAY BE IDENTICAL TO THE INPUT VECTOR IFUNC2(.).
C
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
C     ORIGINAL VERSION--DECEMBER  1978.
C     UPDATED         --JANUARY   1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --APRIL     2005. SINCE FUNCTIONS NO LONGER
C                                       STORED IN UPPER CASE,
C                                       NEED TO CONVERT EXTRACTED
C                                       FUNCTION TO UPPER CASE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFUNC2
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANS
      CHARACTER*4 IFUNC
      CHARACTER*4 IFUNC3
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IFOUNN
      CHARACTER*4 ICH
      CHARACTER*4 IX1
      CHARACTER*4 IX2
      CHARACTER*4 IHOUT
      CHARACTER*4 IWD1
      CHARACTER*4 IWD12
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IFUNC2(*)
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IANS(*)
      DIMENSION IFUNC(*)
      DIMENSION IFUNC3(*)
C
      DIMENSION ICH(8)
C
      DIMENSION IHOUT(10)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='FU  '
C
      NUMASC=4
      NUMAS2=2*NUMASC
C
      IFOUNN='NO'
      IEND=0
      ISTART=0
      ISTOP=0
      J2=0
      ILENEX=0
      ILENFN=0
      IDEL=0
      N3PDEL=0
      ISTART=0
      ISTOP=0
      ISTAR2=0
      ISTOP2=0
      IPOINT=0
      IPOIN1=0
      IPOIN2=0
      DO10I=1,8
        ICH(I)=' '
   10 CONTINUE

C               ***************************
C               **  EXTRACT A FUNCTION.  **
C               ***************************
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPEXFU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N2,NUMCHF,NUMNAM
   72   FORMAT('N2,NUMCHF,NUMNAM = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)(IFUNC2(I),I=1,MIN(N2,115))
   73   FORMAT('IFUNC2(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,75)(IFUNC(I),I=1,MIN(NUMCHF,115))
   75   FORMAT('IFUNC(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,78)
   78   FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I) = ')
        CALL DPWRST('XXX','BUG ')
        IF(NUMNAM.GT.0)THEN
          DO80I=1,NUMNAM
            WRITE(ICOUT,81)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVSTAR(I),IVSTOP(I)
   81       FORMAT(I8,2X,2A4,2X,A4,2(2X,I8))
            CALL DPWRST('XXX','BUG ')
   80     CONTINUE
        ENDIF
      ENDIF
C
C               **********************************************
C               **  STEP 1--                                **
C               **  INITIALIZE SOME VARIABLES AND           **
C               **  COPY THE INITIAL CONTENTS OF IFUNC2(.)  **
C               **  INTO IFUNC3(.).                         **
C               **  SET N3 INITIALLY = N2.                  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IERROR='NO'
      NUMPAS=100
      DO120I=1,N2
        IFUNC3(I)=IFUNC2(I)
  120 CONTINUE
      N3=N2
C
C               *********************************************************
C               **  STEP 3--                                           **
C               **  MAKE A MAXIMUM OF 100 INDEPENDENT MULTI-NAME PASSES *
C               **  AT THE CONTINUOUSLY-UPDATED STRING IN IFUNC3(.).   **
C               **  EACH INDEPENDENT MULTI-NAME PASS CONSISTS OF GOING **
C               **  THROUGH ALL THE FUNCTION NAMES IN THE INTERNAL     **
C               **  DATAPLOT TABLE, SEEING IF EACH ONE OCCURS IN       **
C               **  IFUNC3(.), AND THEN REPLACING THE FUNCTION NAME IN **
C               **  IFUNC3(.) BY THE DEFINED FUNCTION EXPRESSION.      **
C               **  WHEN IFUNC3(.) NO LONGER CONTAINS ANY FUNCTION     **
C               **  NAMES, THEN TERMINATE THE PASSES.                  **
C               *********************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1100IPASS=1,NUMPAS
C
C               *****************************************************
C               **  STEP 3.1--                                     **
C               **  FOR A GIVEN INDEPENDENT MULTI-NAME PASS,       **
C               **  EXAMINE (SWEEP THROUGH) ALL THE FUNCTION       **
C               **  NAMES IN THE INTERNAL DATAPLOT TABLE.          **
C               **  FOR A GIVEN FUNCTION NAME, EXAMINE THE         **
C               **  CURRENT STRING IN IFUNC3(.) TO DETERMINE       **
C               **  IF THIS PARTICULAR FUNCTION NAME OCCURS        **
C               **  ANYWHERE IN THE STRING.                        **
C               *****************************************************
C
        ISTEPN='3.1'
        IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IFOUNN='NO'
        DO2100INAME=1,NUMNAM
          IF(IUSE(INAME).EQ.'F')GOTO2190
          GOTO2100
 2190     CONTINUE
C
C               **************************************************
C               **  STEP 3.2--                                  **
C               **  FOR A GIVEN NAME IN THE TABLE,              **
C               **  DECOMPOSE THE NAME INTO 1-CHARACTER WORDS.  **
C               **************************************************
C
          ISTEPN='3.2'
          IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IWD1=IHNAME(INAME)
          IWD12=IHNAM2(INAME)
          IF(IWD1.EQ.' ')THEN
            WRITE(ICOUT,3081)
 3081       FORMAT('***** ERROR IN DPEXFU--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3082)
 3082       FORMAT('      A FUNCTION NAME ENCOUNTERED IN THE INTERNAL')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3084)
 3084       FORMAT('      DATAPLOT TABLE CONSISTED OF ALL BLANKS.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3085)INAME
 3085       FORMAT('      IT WAS NAME NUMBER ',I8,' IN THE PARAMETER/')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3086)
 3086       FORMAT('      IN THE VARIABLE/FUNCTION TABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
CCCCC     CALL DPXH1H(IWD1,ICH,IEND,IBUGA3)
          DO3195I=1,NUMAS2
            ICH(I)=' '
 3195     CONTINUE
C
          J=0
          IF(IWD1.EQ.' ')THEN
            IEND=0
            GOTO3390
          ENDIF
          IX1=IWD1
          ISTR2=0
          ILEN1=NUMBPC
          ILEN2=ILEN1
          DO3200I=1,NUMASC
            J=J+1
            IX2=' '
            ISTR1=(I-1)*NUMBPC
            CALL DPCHEX(ISTR1,ILEN1,IX1,ISTR2,ILEN2,IX2)
            ICH(J)=IX2
 3200     CONTINUE
C
          IF(IWD12.NE.' ')THEN
            IX1=IWD12
            ISTR2=0
            ILEN1=NUMBPC
            ILEN2=ILEN1
            DO3250I=1,NUMASC
              J=J+1
              IX2=' '
              ISTR1=(I-1)*NUMBPC
              CALL DPCHEX(ISTR1,ILEN1,IX1,ISTR2,ILEN2,IX2)
              ICH(J)=IX2
 3250       CONTINUE
          ENDIF
C
          K=0
          DO3300I=1,J
            K=K+1
            IF(ICH(I).EQ.' ')THEN
              IEND=K-1
              GOTO3390
            ENDIF
 3300     CONTINUE
          IEND=K
 3390     CONTINUE
C
          IF(IBUGA3.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3391)
 3391       FORMAT('***** FROM THE MIDDLE OF DPEXFU--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3392)IPASS,INAME,IWD1,IWD12,IEND
 3392       FORMAT('IPASS,INAME,IWD1,IWD12,IEND = ',2I8,2(2X,A4),I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3393)(ICH(I),I=1,8)
 3393       FORMAT('ICH(.)--',120A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
C               *************************************************
C               **  STEP 3.3--                                 **
C               **  SEARCH THE CURRENT STRING TO SEE IF THIS   **
C               **  PARTICULAR FUNCTION NAME IS ANYWHERE       **
C               **  IN THE STRING.                             **
C               **  ALSO CHECK TO SEE IF A FOUND STRING        **
C               **  IS VALID UNTO ITSELF BY CHECKING IF IT     **
C               **  IS PRECEDED AND SUCCEEDED BY THE           **
C               **  USUAL TYPE OF SEPARATORS AS FOUND          **
C               **  IN MATHEMATICAL EXPRESSIONS                **
C               **  (+, -, *, /, PARENTHESIS, OR SPACE.        **
C               *************************************************
C
          ISTEPN='3.3'
          IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          ISTART=-99
          ISTOP=-99
          NUMTNF=0
          DO4100I=1,N3
            I2=I
            IF(IFUNC3(I).EQ.ICH(1))GOTO4190
            GOTO4100
 4190       CONTINUE
C
            DO4200J=1,IEND
              J2=I2+J-1
C     *****   THE FOLLOWING CORRECTIVE LINE INSERTED IN AUGUST 1983 *****
              IF(J2.GT.N3)GOTO4100
              IF(IFUNC3(J2).EQ.ICH(J))GOTO4200
              GOTO4100
 4200       CONTINUE
C
            ISTART=I2
            ISTOP=J2
C
            ISTAM1=ISTART-1
            IF(ISTAM1.LT.1.OR.ISTAM1.GT.N3)GOTO4390
            IF(IFUNC3(ISTAM1).EQ.' ')GOTO4390
            IF(IFUNC3(ISTAM1).EQ.'(')GOTO4390
            IF(IFUNC3(ISTAM1).EQ.'+')GOTO4390
            IF(IFUNC3(ISTAM1).EQ.'-')GOTO4390
            IF(IFUNC3(ISTAM1).EQ.'*')GOTO4390
            IF(IFUNC3(ISTAM1).EQ.'/')GOTO4390
            IF(IFUNC3(ISTAM1).EQ.'**')GOTO4390
            GOTO4100
 4390       CONTINUE
C
            ISTOP1=ISTOP+1
            IF(ISTOP1.LT.1.OR.ISTOP1.GT.N3)GOTO4490
            IF(IFUNC3(ISTOP1).EQ.' ')GOTO4490
            IF(IFUNC3(ISTOP1).EQ.')')GOTO4490
            IF(IFUNC3(ISTOP1).EQ.'+')GOTO4490
            IF(IFUNC3(ISTOP1).EQ.'-')GOTO4490
            IF(IFUNC3(ISTOP1).EQ.'*')GOTO4490
            IF(IFUNC3(ISTOP1).EQ.'/')GOTO4490
            IF(IFUNC3(ISTOP1).EQ.'**')GOTO4490
            GOTO4100
 4490       CONTINUE
C
            IFOUNN='YES'
C
C               *********************************************************
C               **  STEP 3.4--                                         **
C               **  HAVING FOUND AN OCCURRANCE OF A GIVEN FUNCTION NAME *
C               **  SOMEWHERE IN THE CURRENT STRING IFUNC3(.),         **
C               **  1) DETERMINE THE LENGTH OF THE FUNCTION EXPRESSION **
C               **     ABOUT TO BE SUBSTITUTED (INTO IFUNC3(.))        **
C               **     IN PLACE OF THE FUNCTION NAME.                  **
C               **  2) MOVE THE SEGMENT OF THE STRING IN IFUNC3(.)     **
C               **     WHICH IS BEYOND THE FOUND FUNCTION NAME OVER    **
C               **     AN APPROPRIATE NUMBER OF SPACES.                **
C               **  3) ACTUALLY INSERT THE FUNCTION EXPRESSION         **
C               **     INTO IFUNC3(.) IN PLACE OF THE FUNCTION NAME    **
C               **     (PRECEDED AND SUCCEEDED BY PARENTHESES).        **
C               **  4) UPDATE THE CURRENT LENGTH N3 OF IFUNC3(.).      **
C               **  5) LOOP BACK AND COMPLETELY REEXAMINE IFUNC3(.) FOR *
C               **     ADDITIONAL OCCURRANCES OF THIS FUNCTION NAME.   **
C               *********************************************************
C
C
            ISTEPN='3.4'
            IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
            ISTAR2=IVSTAR(INAME)
            ISTOP2=IVSTOP(INAME)
            ILENEX=ISTOP2-ISTAR2+1
            ILENFN=IEND
            IDEL=ILENEX-ILENFN
            IDEL=IDEL+2
C
            N3PDEL=N3+IDEL
            IF(N3PDEL.GT.MAXN3)THEN
              WRITE(ICOUT,3081)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,5002)
 5002         FORMAT('      ERROR CAUSED IN FORMATION OF FUNCTION--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,5005)
 5005         FORMAT('      THE TOTAL NUMBER OF CHARACTERS FOR THE')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,5006)MAXCHF
 5006         FORMAT('      FUNCTION MAY NOT EXCEED ',I8,'.  SUCH AN')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,5007)
 5007         FORMAT('      OVERFLOW CONDITION HAS JUST BEEN ',
     1               'ENCOUNTERED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,5018)
 5018         FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,5019)(IANS(K),K=1,MIN(IWIDTH,100))
 5019         FORMAT('      ',100A1)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
C
            IPOINT=ISTOP
            IHOUT(1)=')'
            NOUT=1
            CALL DPSIAS(IPOINT,IFUNC3,N3,IHOUT,NOUT,IBUGA3,IERROR)
C
            IPOINT=ISTART-1
            IHOUT(1)='('
            NOUT=1
            CALL DPSIAS(IPOINT,IFUNC3,N3,IHOUT,NOUT,IBUGA3,IERROR)
C
            IPOIN1=ISTART+1
            IPOIN2=ISTOP+1
            CALL DPSIRS(IFUNC3,N3,IPOIN1,IPOIN2,IFUNC,NUMCHF,
     1                  ISTAR2,ISTOP2,
     1                  IBUGA3,IERROR)
C
CCCCC       APRIL 2005.  CONVERT NEW FUNCTION TO UPPER CASE.
C
            DO5201II=1,N3
              IJUNK=ICHAR(IFUNC3(II)(1:1))
              IF(IJUNK.GE.97 .AND. IJUNK.LE.122)THEN
                IJUNK=IJUNK-32
                IFUNC3(II)(1:1)=CHAR(IJUNK)
              ENDIF
 5201       CONTINUE
C
            NUMTNF=NUMTNF+1
            IF(NUMTNF.LE.MAXN3)GOTO4100
C
C           WRITE(ICOUT,3081)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5302)
 5302       FORMAT('      FOR A GIVEN MULTI-NAME PASS,')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5303)(ICH(K),K=1,IEND)
 5303       FORMAT('      FOR A PARTICULAR FUNCTION NAME (= ',10A1,')')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5304)MAXN3
 5304       FORMAT('      THE NAME OCCURRED MORE THAN ',I8,' TIMES ON ',
     1             'THE LINE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5307)
 5307       FORMAT('      POSSIBLE CAUSE--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5308)
 5308       FORMAT('      AN IMPROPER INFINITELY-RECURSIVE ORIGINAL')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5309)
 5309       FORMAT('      FUNCTION DEFINITION THAT HAD BEEN')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5310)(ICH(K),K=1,IEND)
 5310       FORMAT('      PREVIOUSLY MADE FOR ',10A1)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5311)
 5311       FORMAT('      EXAMPLE--LET FUNCTION F1=F1*F1')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5312)
 5312       FORMAT('      SOLUTION--CORRECT THE ORIGINAL DEFINITION ',
     1             'FOR')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5313)(ICH(K),K=1,IEND)
 5313       FORMAT('      THE FUNCTION ',10A1,' SO THAT IT IS AN ',
     1             'EXPRESSION')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5315)
 5315       FORMAT('      IN TERMS OF CONSTANTS, PARAMETERS, AND ',
     1             'VARIABLES--NOT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5317)
 5317       FORMAT('      UNENDINGLY RECURSIVE IN ITS OWN FUNCTION ',
     1             'NAME.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
C
 4100     CONTINUE
C
 2100   CONTINUE
        IF(IFOUNN.EQ.'NO')GOTO9000
C
 1100 CONTINUE
C
      WRITE(ICOUT,3081)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5902)
 5902 FORMAT('      THE NUMBER OF INDEPENDENT, MULTI-NAME PASSES TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5904)
 5904 FORMAT('      DETERMINE THE EXPLICIT UNDERLYING FUNCTION HAS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5906)
 5906 FORMAT('      JUST EXCEEDED THE MAXIMUM ALLOWABLE NUMBER OF ',
     1       'NAMES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5907)
 5907 FORMAT('      POSSIBLE CAUSE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5908)
 5908 FORMAT('      AN IMPROPER INFINITELY-RECURSIVE ORIGINAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5909)
 5909 FORMAT('      FUNCTION DEFINITION THAT HAD BEEN PREVIOUSLY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5910)
 5910 FORMAT('      MADE FOR ONE OR MORE FUNCTIONS.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5911)
 5911 FORMAT('      EXAMPLE--LET FUNCTION F1=F1*F2')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5912)
 5912 FORMAT('      SOLUTION--CORRECT THE ORIGINAL DEFINITION FOR SOME')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5914)
 5914 FORMAT('      FUNCTION SO THAT IT IS AN EXPRESSION TERMS OF')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5915)
 5915 FORMAT('      OF CONSTANTS, PARAMETERS, AND VARIABLES--NOT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5917)
 5917 FORMAT('      UNENDINGLY RECURSIVE IN ITS OWN FUNCTION NAME.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               ****************
C               **  STEP 4--  **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXFU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ISTAR2,ISTOP2,ILENEX,ILENFN,IDEL,N3PDEL
 9012   FORMAT('ISTAR2,ISTOP2,ILENEX,ILENFN,IDEL,N3PDEL = ',6I4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ISTART,ISTOP,IFOUNN,IERROR
 9013   FORMAT('ISTART,ISTOP,IFOUNN,IERROR = ',2I8,3X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IPOINT,IPOIN1,IPOIN2,IEND,N3
 9014   FORMAT('IPOINT,IPOIN1,IPOIN2,IEND,N3 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)(ICH(I),I=1,8)
 9016   FORMAT('ICH(.) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(IFUNC3(I),I=1,MIN(N3,115))
 9018   FORMAT('IFUNC3(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXPY(ICASE,TEMP1,TEMP2,TEMP3,N1,N2,N3,
     1                  YOUT1,YOUT2,YOUT3,AVAL1,AVAL2,AVAL3,
     1                  IBUGA3,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--SUPPORTS THE FOLLOWING COMMAND:
C
C                 LET Y = PYTHON <COMMAND> <VAR-LIST>
C
C              THIS COMMAND EXECUTES PYTHON SCRIPTS.
C
C                  1. IT IS ASSUMED THAT PYTHON, AND ANY NEEDED
C                     PACKAGES SUCH AS NUMPY, ARE ALREADY INSTALLED
C                     ON THE LOCAL PLATFORM.
C
C                  2. THIS COMMAND EXECUTES SCRIPTS THAT ARE
C                     STORED IN THE "scripts" DIRECTORY.
C
C                     THE DESIRED SCRIPT WILL BE BASED ON THE
C                     COMMAND NAME.
C
C                  3. THE VARIABLES ON THE RIGHT HAND SIDE WILL
C                     BE WRITTEN TO THE FILE "dpst1f.dat" AND THE
C                     FOLLOWING COMMAND WILL BE EXECUTED:
C
C                        SYSTEM python <path>/scripts/<file.py>
C                               > dpst2f.dat
C
C                     THE OUTPUT NEEDED BY DATAPLOT WILL BE WRITTEN
C                     TO "dpst3f.dat" (AND "dpst4f.dat" AND "dpst5f.dat"
C                     IF NEEDED).
C
C              THE FOLLOWING PYTHON SCRIPTS ARE CURRENTLY SUPPORTED:
C
C                  1. LET A = PYTHON MEAN Y
C
C                     NOTE THAT THIS IS PRIMARILY A "PROOF OF
C                     CONCEPT" COMMAND AS MEANS ARE EASILY COMPUTED
C                     IN DATAPLOT.
C
C                  ONCE THE SIMPLE "MEAN.PY" IS WORKING, THEN HAVE
C                  BASIC STRUCTURE FOR NON-TRIVIAL SCRIPTS.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2019/01
C     ORIGINAL VERSION--JANUARY   2019.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION YOUT1(*)
      DIMENSION YOUT2(*)
      DIMENSION YOUT3(*)
C
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*4 IOP
      CHARACTER*4 IOFILE
CCCCC CHARACTER*255 IFILE
CCCCC CHARACTER*255 ITEXT
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER (LEN=MAXSTR) :: ITEXT
      CHARACTER*1 IQUOTE
C
      LOGICAL LEXIST
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='PY  '
      IFOUND='YES'
      IERROR='NO'
      IFILE=' '
      ITEXT=' '
      IQUOTE='"'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXPY')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXPY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASE,N1,N2,N3
   53   FORMAT('ICASE,N1,N2,N3 = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        DO61I=1,N1
          WRITE(ICOUT,63)I,TEMP1(I),TEMP2(I),TEMP3(I)
   63     FORMAT('I,TEMP1(I),TEMP2(I),TEMP3(I) = ',
     1           I8,2X,3G15.7)
          CALL DPWRST('XXX','BUG ')
   61   CONTINUE
      ENDIF
C
C               ****************************************************
C               **  STEP 1--                                      **
C               **  CHECK FOR VALID COMMAND.                      **
C               ****************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXPY')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASE.EQ.'MEAN')THEN
C
C       ********************************************************
C       **  STEP 2A-                                          **
C       **  LET <PAR> = PYTHON MEAN    CASE                   **
C       ********************************************************
C
        ISTEPN='2A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXPY')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       CHECK IF "mean.py" EXISTS IN "scripts" DIRECTORY.  USE
C       FORTRAN "INQUIRE" FUNCTION DIRECTLY RATHER THAN DPINFI
C       AS WE NEED TO INQUIRE THE FILE EXACTLY AS GIVEN.
C
        NC1=0
        IF(IOPSY1.EQ.'UNIX') THEN
          IFILE(1:IUNXNC)=UNIXPN(1:IUNXNC)
          IFILE(IUNXNC+1:IUNXNC+15)='scripts/mean.py'
          NC1=IUNXNC+15
        ELSE
          IFILE(1:NCPATH)=PATH(1:NCPATH)
          IFILE(NCPATH+1:NCPATH+15)='scripts\mean.py'
          NC1=NCPATH+15
        ENDIF
        INQUIRE(FILE=IFILE,EXIST=LEXIST)
        IF(.NOT.LEXIST)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,110)
  110     FORMAT('***** ERROR IN PYTHON COMMAND--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
  111     FORMAT('      THE mean.py FILE DOES NOT EXIST IN ',
     1           'THE scripts DIRECTORY.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
C       WRITE THE INPUT DATA TO "dpst1f.dat".
C
        IOP='OPEN'
        IFLG11=1
        IFLG21=0
        IFLG31=0
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        DO113I=1,N1
          WRITE(IOUNI1,'(E15.7)')TEMP1(I)
  113   CONTINUE
C
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
C
C       EXECUTE THE PYTHON COMMAND
C
        IFLGSP=0
        IF(NCPYTH.GT.0)THEN
          DO115KK=1,NCPYTH
            IF(IPYTPA(KK:KK).EQ.' ')THEN
              IFLSP=1
              GOTO117
            ENDIF
  115     CONTINUE
  117     CONTINUE
        ENDIF
C
        NC2=0
        IF(IFLGSP.EQ.1)THEN
          NC2=NC2+1
          ITEXT(NC2:NC2)=IQUOTE
        ENDIF
C
        IF(NCPYTH.GT.0)THEN
          NC2=NC2+1
          NLAST=NCPYTH+NC2-1
          ITEXT(NC2:NLAST)=IPYTPA(1:NCPYTH)
          NC2=NLAST
          IF(IOPSY1.EQ.'UNIX') THEN
            IF(ITEXT(NC2:NC2).NE.'/')THEN
              NC2=NC2+1
              ITEXT(NC2:NC2)='/'
            ENDIF
          ELSE
            IF(ITEXT(NC2:NC2).NE.'\')THEN
              NC2=NC2+1
              ITEXT(NC2:NC2)='\'
            ENDIF
          ENDIF
        ENDIF
C
        IF(IPYTVR.EQ.'DEFA')THEN
          ITEXT(NC2+1:NC2+6)='python'
          NC2=NC2+6
        ELSEIF(IPYTVR.EQ.'3')THEN
          ITEXT(NC2+1:NC2+7)='python3'
          NC2=NC2+7
        ELSEIF(IPYTVR.EQ.'2')THEN
          ITEXT(NC2+1:NC2+7)='python2'
          NC2=NC2+7
        ENDIF
C
        IF(IFLGSP.EQ.1)THEN
          NC2=NC2+1
          ITEXT(NC2:NC2)=IQUOTE
        ENDIF
        NC2=NC2+1
        ITEXT(NC2:NC2)=' '
C
        IFLGS2=0
        DO125KK=1,NC1
          IF(IFILE(KK:KK).EQ.' ')THEN
            IFLGS2=1
            GOTO127
          ENDIF
  125   CONTINUE
  127   CONTINUE
C
        IF(IFLGS2.EQ.1)THEN
          NC2=NC2+1
          ITEXT(NC2:NC2)=IQUOTE
        ENDIF
C
        ITEXT(NC2+1:NC2+NC1)=IFILE(1:NC1)
        NC2=NC2+NC1
        IF(IFLGS2.EQ.1)THEN
          NC2=NC2+1
          ITEXT(NC2:NC2)=IQUOTE
        ENDIF
C
        ISSAV1=ISYSPE
        ISSAV2=ISYSHI
        ISYSPE='OFF'
        ISYSHI='ON'
        IF(IFLGSP.EQ.1 .OR. IFLGS2.EQ.1)ISYSHI='OFF'
        CALL DPSYS2(ITEXT,NC2,ISUBRO,IERROR)
        ISYSPE=ISSAV1
        ISYSHI=ISSAV2
        IF(IERROR.EQ.'YES')GOTO9000
C
C       NOW READ THE MEAN VALUE FROM "dpst2f.dat"
C
        IOP='OPEN'
        IFLG11=0
        IFLG21=1
        IFLG31=0
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        REWIND(IOUNI2)
        READ(IOUNI2,*,END=121,ERR=131)AVAL1
C
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
        GOTO9000
C
  121   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,110)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      END OF FILE WHEN READING dpst2f.dat TO ',
     1         'OBTAIN THE MEAN VALUE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
  131   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,110)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      ERROR WHEN READING dpst2f.dat TO ',
     1         'OBTAIN THE MEAN VALUE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,110)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,97)
   97   FORMAT('      COMMAND NOT RECOGNIZED')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
      IWORD=5
      CALL DPFILE(IANSLC,IWIDTH,IWORD,
     1            IOFILE,IBUGA3,ISUBRO,IERROR)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXPY')THEN
        WRITE(ICOUT,201)IWORD,IWIDTH,IOFILE
  201   FORMAT('IWORD,IWIDTH,IOFILE = ',2I8,2X,A4)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'EXPY')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXPY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)AVAL1,AVAL2,AVAL3,NC2
 9015   FORMAT('AVAL1,AVAL2,AVAL3,NC2 = ',3G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)ITEXT(1:NC2)
 9018   FORMAT('ITEXT(1:NC2) = ',A)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)YOUT1(1),YOUT2(1),YOUT3(1)
 9020   FORMAT('YOUT1(1),YOUT2(1),YOUT3(1) = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1                  IHARG,IHARG2,NUMARG,
     1                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1                  IUSE,IN,NUMNAM,
     1                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILLOUT,
     1                  IVOUT,VOUT,IUOUT,
     1                  INOUT,IBUGA3,IERROR)
C
C     PURPOSE--SCAN THE ARGUMENTS OF THE COMMAND LINE
C              FOR A KEY WORD AND EXTRACT INFORMATION
C              ABOUT A SELECTED ARGUMENT AFTER THE KEY WORD.
C
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
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --MARCH     1979.
C     UPDATED         --JULY      1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      2013. CHECK FOR + OR - INFINITY
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IKEY
      CHARACTER*4 IKEY2
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 INCLUN
      CHARACTER*4 IANS
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IHOUT
      CHARACTER*4 IHOUT2
      CHARACTER*4 IUOUT
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IVALID
      CHARACTER*4 IANS2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IANS(*)
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
C
      DIMENSION IANS2(50)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='QU  '
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXQU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IKEY,IKEY2,ISHIFT,ILOCA,ILOCB
   52   FORMAT('IKEY,IKEY2,ISHIFT,ILOCA,ILOCB = ',2A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IWIDTH,NUMARG,INCLUN
   55   FORMAT('IWDITH,NUMARG,INCLUM = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANS(I),I=1,MIN(IWIDTH,120))
   54   FORMAT('IANS(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
        DO57I=1,NUMARG
          WRITE(ICOUT,58)I,IHARG(I),IHARG2(I)
   58     FORMAT('I,IHARG(I),IHARG2(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
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
      IFOUN1='NO'
      IFOUN2='NO'
      IHOUT='JUNK'
      IHOUT2='JUNK'
      ILLOUT=-99
      IVOUT=-99
      VOUT=-99.
      IUOUT='U'
      INOUT=-99
      IERROR='NO'
C
C               *****************************************
C               **  STEP 2--                           **
C               **  SEARCH THE COMMAND LINE ARGUMENTS  **
C               **  FOR THE WORD CONTAINED IN IKEY.    **
C               **  STORE THE LOCATION IN ILOC1.       **
C               *****************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO200I=ILOCA,ILOCB
        I2=I
        IF(IHARG(I).EQ.IKEY.AND.IHARG2(I).EQ.IKEY2)GOTO210
  200 CONTINUE
      IFOUN1='NO'
      IFOUN2='NO'
      GOTO9000
  210 CONTINUE
      IFOUN1='YES'
      ILOC1=I2
C
C               ***************************************************
C               **  STEP 3--                                     **
C               **  SEARCH FOR THE COMMAND LINE ARGUMENT         **
C               **  SHIFTED    ISHIFT    ARGUMENTS TO THE RIGHT  **
C               **  OF THE KEY WORD.                             **
C               **  STORE THE LOCATION IN ILOC2.                 **
C               **  STORE THE FOUND WORD IN IHOUT.               **
C               ***************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I2=I2+ISHIFT
      IF(ILOCA.LE.I2.AND.I2.LE.ILOCB)GOTO310
      IFOUN2='NO'
      GOTO9000
  310 CONTINUE
      IFOUN2='YES'
      ILOC2=I2
      IHOUT=IHARG(ILOC2)
      IHOUT2=IHARG2(ILOC2)
C
C     2013/06: CHECK FOR "INFINITY", "CPUMAX", "-INFINITY" AND "CPUMIN".
C              THIS IS USEFUL, FOR EXAMPLE, FOR IDENTIFYING THE CASE
C              OF AN INDEFINITE INTEGRAL.
C
      IF(IHOUT.EQ.'INFI' .AND. IHOUT2.EQ.'NITY')THEN
        VOUT=CPUMAX
        IUOUT='C'
        ILLOUT=0
        IVOUT=0
        GOTO9000
      ELSEIF(IHOUT.EQ.'CPUM' .AND. IHOUT2.EQ.'AX  ')THEN
        VOUT=CPUMAX
        IUOUT='C'
        ILLOUT=0
        IVOUT=0
        GOTO9000
      ELSEIF(IHOUT.EQ.'-INF' .AND. IHOUT2.EQ.'INIT')THEN
        VOUT=CPUMIN
        IUOUT='C'
        ILLOUT=0
        IVOUT=0
        GOTO9000
      ELSEIF(IHOUT.EQ.'CPUM' .AND. IHOUT2.EQ.'IN  ')THEN
        VOUT=CPUMIN
        IUOUT='C'
        ILLOUT=0
        IVOUT=0
        GOTO9000
      ENDIF
C
C               **************************************************************
C               **  STEP 4--                                                **
C               **  DETERMINE THE CHARACTERISTICS OF                        **
C               **  THIS SECOND ARGUMENT--                                  **
C               **       ILLOUT = LINE NUMBER IN IHNAME(.)LIST;              **
C               **       IVOUT = INTEGER VALUE ASSOCIATED WITH IT           **
C               **               (E.G., COLUMN NUMBER FOR A VARIABLE);      **
C               **       VOUT  = FLOATING POINT VALUE ASSOCIATED WITH IT    **
C               **               (E.G., VALUE OF A PARAMETER OR CONSTANT);  **
C               **       IUOUT = TYPE OF ARGUMENT                           **
C               **               (V = VARIABLE, P = PARAMETER,              **
C               **               C = CONSTANT, U = UNKNOWN);                **
C               **       INOUT = INTEGER VALUE DENOTING                     **
C               **               THE NUMBER OF OBSERVATIONS IN THE COLUMN   **
C               **               FOR A VARIABLE.                            **
C               **************************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 4.1--                         **
C               **  SEARCH FOR VARIABLE OR PARAMETER.  **
C               *****************************************
C
      IF(NUMNAM.GE.1)THEN
        DO400I=1,NUMNAM
          I2=I
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'1   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'2   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'3   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'4   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'5   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'6   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'7   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'8   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'9   '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
CCCCC     IF(INCLUN.EQ.'NO'.AND.IHNAME(I).EQ.'10  '.AND.
CCCCC1       IHNAM2(I).EQ.'    '.AND.IUSE(I).EQ.'V')GOTO400
          IF(IHOUT.EQ.IHNAME(I).AND.IHOUT2.EQ.IHNAM2(I).AND.
     1      (IUSE(I).EQ.'P' .OR. IUSE(I).EQ.'V'))THEN
            ILLOUT=I2
            IVOUT=IVALUE(I2)
            VOUT=VALUE(I2)
            IUOUT=IUSE(I2)
            INOUT=IN(I2)
            GOTO9000
          ENDIF
  400   CONTINUE
      ENDIF
C
C               **************************************************************
C               **  STEP 4.2--                                              **
C               **  EXTRACT THE 1H HOLLERITH REPRESENTATION                 **
C               **  OF IHOUT.                                               **
C               **  COPY ALSO THE 1H CONTINUATION OF THE WORD IF EXISTENT.  **
C               **************************************************************
C
      ISTEPN='4.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPWDST(IKEY,IKEY2,ISHIFT,IHOUT,IHOUT2,IANS,IWIDTH,
     1            IANS2,N2,IBUGA3,IERROR)
C
C               ********************************
C               **  STEP 4.3--                **
C               **  TREAT THE CONSTANT CASE.  **
C               ********************************
C
      ISTEPN='4.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPHOCO(IANS2,N2,IVALID,VALCON,IBUGA3,IERROR)
      IF(IVALID.EQ.'YES')GOTO460
      GOTO469
  460 CONTINUE
      IVOUT=INT(VALCON)
      VOUT=VALCON
      IUOUT='N'
      INOUT=0
      GOTO9000
  469 CONTINUE
C
C               *********************************************
C               **  STEP 4.4--                             **
C               **  TREAT THE ELEMENT OF A VARIABLE CASE.  **
C               *********************************************
C
      ISTEPN='4.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC CALL DPHOEV(IANS2,N2,IV,IHNAME,IHNAM2,IUSE,IVALUE,VALUE,
CCCCC1IUSE,NUMNAM,IFOUND,VALEV,IBUGA3,IERROR)
CCCCC IF(IFOUND.EQ.'YES')GOTO475
CCCCC GOTO479
CC475 CONTINUE
CCCCC IVOUT=VALEV
CCCCC VOUT=VALEV
CCCCC IVOUT='EV'
CCCCC INOUT=0
CCCCC GOTO9000
CC479 CONTINUE
CCCCCC
CCCCC IUOUT='U'
CC489 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXQU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ILOC1,ILOC2,IFOUN1,IFOUN2
 9012   FORMAT('ILOC1,ILOC2,IFOUN1,IFOUN2 = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IHOUT,IHOUT2,ILLOUT,IVOUT,VOUT,IUOUT,INOUT
 9013   FORMAT('IHOUT,IHOUT2,ILLOUT,IVOUT,VOUT,IUOUT,INOUT = ',
     1         2A4,2I8,G15.7,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR
 9014   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXRP(IANS,IANSLC,IWIDTH,ICOM,ICOM2,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--RUN EITHER AN R OR A PYTHON SCRIPT.  FOR EXAMPLE,
C
C                  RSCRIPT  file.r
C                  PYTHON   file.py
C
C              IN ORDER FOR THIS COMMAND TO WORK CORRECTLY, THE
C              FOLLOWING NEEDS TO BE DONE:
C
C                 1) R (OR PYTHON) NEEDS TO BE INSTALLED ON YOUR LOCAL
C                    PLATFORM.
C
C                 2) YOU MAY NEED TO SET THE PATH FOR R OR PYTHON WITH
C                    THE COMMANDS
C
C                       SET PYTHON PATH ...
C                       SET R      PATH ...
C
C                 3) FOR PYTHON, YOU MAY NEED TO SPECIFY THE VERSION
C                    WITH THE COMMAND
C
C                       SET PYTHON VERSION <2/3>
C
C               FOR BUILT-IN SCRIPTS (I.E., FROM THE "lib/scripts"
C               DIRECTORY), SET ITYPE TO "SYST".  FOR USER DEFINED
C               SCRIPTS, SET ITYPE TO "USER".
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK OF THE
C           NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2019/11
C     ORIGINAL VERSION--NOVEMBER   2019.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 ICOM2
      CHARACTER*4 IANS
      CHARACTER*4 IANSLC
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      DIMENSION IANS(*)
      DIMENSION IANSLC(*)
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC CHARACTER*255 ICANS
CCCCC CHARACTER*255 ITEXT3
      CHARACTER (LEN=MAXSTR) :: ICANS
      CHARACTER (LEN=MAXSTR) :: ITEXT3
C
      CHARACTER (LEN=MAXFNC) :: IFILE1
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER*12 ISTAT1
      CHARACTER*12 IFORM1
      CHARACTER*12 IACCE1
      CHARACTER*12 IPROT1
      CHARACTER*12 ICURS1
C
      CHARACTER*4 ICASE
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPEN
      CHARACTER*4 IFILQ2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*12 IACC
      CHARACTER*80 ARGLST
      CHARACTER*1 IBASLC
      CHARACTER*1 IATEMP
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPEX'
      ISUBN2='RP  '
      CALL DPCONA(92,IBASLC)
C
      J2=0
      IPROG=0
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXRP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IWIDTH
   53   FORMAT('IBUGS2,ISUBRO,IWIDTH = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANS(I),I=1,MIN(255,IWIDTH))
   54   FORMAT('(IANS(I),I=1,IWIDTH) = ',255A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
C               *****************************************************
C
C               ********************************************
C               **  STEP 1.1--                            **
C               **  DETERMINE THE COMMAND                 **
C               **  (RSCRIPT OR PYTHON) AND ITS LOCATION  **
C               **  ON THE LINE.                          **
C               ********************************************
C
C     CHECK FOR "RSCRIPT" OR "PYTHON" FIRST
C
      IF(ICOM.EQ.'RSCR' .AND. ICOM2.EQ.'IPT ')THEN
        ICASE='R   '
        IFOUND='YES'
      ELSEIF(ICOM.EQ.'PYTH' .AND. ICOM2.EQ.'ON  ')THEN
        ICASE='PYTH'
        IFOUND='YES'
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1001)
 1001   FORMAT('***** ERROR IN RSCRIPT/PYTHON COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1002)
 1002   FORMAT('      COMMAND NOT EQUAL TO:  RSCRIPT OR PYTHON.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNI1=ILISNU
      IFILE1=ILISNA
      ISTAT1=ILISST
      IFORM1=ILISFO
      IACCE1=ILISAC
      IPROT1=ILISPR
      ICURS1=ILISCS
C
C               ***************************************
C               **  STEP 13--                        **
C               **  EXTRACT THE INPUT  FILE NAME.    **
C               ***************************************
C
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1310I=1,MAXSTR
        ICANS(I:I)=IANSLC(I)(1:1)
 1310 CONTINUE
C
      IFILQ2=IFILQU
      IFILQU='ON'
      ISTART=1
      ISTOP=IWIDTH
      IWORD=2
      CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1            ICOL1,ICOL2,IFILE1,NCFIL1,
     1            IBUGS2,ISUBRO,IERROR)
      IFILQU=IFILQ2
      IF(IFILE1(1:1).EQ.'"' .AND. IFILE1(NCFIL1:NCFIL1).EQ.'"')THEN
        IFILE1(1:NCFIL1-2)=IFILE1(2:NCFIL1-1)
        NCFIL1=NCFIL1-2
        IFILE1(NCFIL1+1:NCFIL1+2)='  '
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')THEN
        WRITE(ICOUT,1351)ICOL1,ICOL2,NCFIL1
 1351   FORMAT('AFTER DPEXWO: ICOL1,ICOL2,NCFIL1 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1353)IFILE1
 1353   FORMAT('IFILE1 = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERROR.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1332)
 1332   FORMAT('      UNABLE TO EXTRACT FILE NAME')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C     CHECK FOR COMMAND LINE ARGUMENTS
C
      ARGLST=' '
      NCARG=0
      ISTRT=ICOL2+1
      DO1340II=MAXSTR,ISTRT,-1
        IF(ICANS(II:II).NE.' ')THEN
          ILAST=II
          GOTO1349
        ENDIF
 1340 CONTINUE
      ILAST=ICOL2
 1349 CONTINUE
      IF(ISTRT.LT.ILAST)THEN
        NCARG=ILAST-ISTRT+1
        IF(NCARG.GT.80)NCARG=80
        ARGLST(1:NCARG)=ICANS(ISTRT:ILAST)
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')THEN
        WRITE(ICOUT,1393)ISTRT,ILAST,NCARG,ARGLST
 1393   FORMAT('ISTRT,ILAST,NCARG,ARGLST = ',3I8,2X,A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C
C
C            ****************************************************
C            **  STEP 2--                                      **
C            **  SEE IF SPECIFIED FILE EXISTS                  **
C            ****************************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     FIRST INQUIRE IF FILE EXISTS AS GIVEN
C
      CALL DPINFI(IFILE1,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')GOTO290
C
C     CHECK ALL UPPER CASE
C
      IFILE2=' '
      IWIDTH=80
      CALL DPUP80(IFILE1,IFILE2,IWIDTH,IBUGS2,IERROR)
      CALL DPINFI(IFILE2,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')THEN
        IFILE1(1:IWIDTH)=IFILE2(1:IWIDTH)
        GOTO290
      ENDIF
C
C     CHECK ALL LOWER CASE
C
      IFILE2=' '
      IWIDTH=80
      CALL DPLO80(IFILE1,IFILE2,IWIDTH,IBUGS2,IERROR)
      CALL DPINFI(IFILE2,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')THEN
        IFILE1(1:IWIDTH)=IFILE2(1:IWIDTH)
        GOTO290
      ENDIF
C
C     CHECK FOR FILE IN "scripts" DIRECTORY
C
C
C     ADD DATAPLOT AUXILLARY PATH
C
      IF(IOPSY1.EQ.'UNIX') THEN
        IFILE2(1:IUNXNC)=UNIXPN(1:IUNXNC)
        NC1=IUNXNC
        IF(IFILE2(NC1:NC1).EQ.'/')THEN
          IFILE2(NC1+1:NC1+8)='scripts/'
          NC1=NC1+8
        ELSE
          IFILE2(NC1+1:NC1+9)='/scripts/'
          NC1=NC1+9
        ENDIF
      ELSE
        IFILE2(1:NCPATH)=PATH(1:NCPATH)
        NC1=NCPATH
        IF(IFILE2(NC1:NC1).EQ.'\')THEN
          IFILE2(NC1+1:NC1+8)='scripts\'
          NC1=NC1+8
        ELSE
          IFILE2(NC1+1:NC1+9)='\scripts\'
          NC1=NC1+9
        ENDIF
      ENDIF
C
      DO210II=IWIDTH,1,-1
        IF(IFILE1(II:II).NE.' ')THEN
          ILAST=II
          GOTO219
        ENDIF
  210 CONTINUE
      ILAST=1
  219 CONTINUE
C
      IF(NC1+ILAST.GT.IWIDTH)GOTO8000
C
C     FIRST, NAME AS IS
C
      NCNT=NC1
      DO220II=1,ILAST
        NCNT=NCNT+1
        IFILE2(NCNT:NCNT)=IFILE1(II:II)
  220 CONTINUE
      IF(NCNT.LT.IWIDTH)IFILE2(NCNT+1:IWIDTH)=' '
      CALL DPINFI(IFILE2,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')THEN
        IFILE1(1:IWIDTH)=IFILE2(1:IWIDTH)
        GOTO290
      ENDIF
C
C     NOW CONVERT TO UPPER CASE
C
      DO230II=NC1+1,NCNT
        IATEMP=IFILE2(II:II)
        CALL DPCOAN(IATEMP,IVAL)
        IF(IVAL.GE.97 .AND. IVAL.LE.122)THEN
          IVAL=IVAL-32
          CALL DPCONA(IVAL,IATEMP)
          IFILE2(II:II)=IATEMP
        ENDIF
  230 CONTINUE
      CALL DPINFI(IFILE2,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')THEN
        IFILE1(1:IWIDTH)=IFILE2(1:IWIDTH)
        GOTO290
      ENDIF
C
C     NOW CONVERT TO LOWER CASE
C
      DO240II=NC1+1,NCNT
        IATEMP=IFILE2(II:II)
        CALL DPCOAN(IATEMP,IVAL)
        IF(IVAL.GE.65 .AND. IVAL.LE.90)THEN
          IVAL=IVAL+32
          CALL DPCONA(IVAL,IATEMP)
          IFILE2(II:II)=IATEMP
        ENDIF
  240 CONTINUE
      CALL DPINFI(IFILE2,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')THEN
        IFILE1(1:IWIDTH)=IFILE2(1:IWIDTH)
        GOTO290
      ENDIF
C
      GOTO8000
C
 290  CONTINUE
      IFLAG1=0
      DO293II=80,1,-1
        IF(IFILE1(II:II).NE.' ')THEN
          NCFILE=II
          GOTO295
        ENDIF
  293 CONTINUE
      NCFILE=1
  295 CONTINUE
      DO297II=1,NCFIL1
        IF(IFILE1(II:II).EQ.' ')THEN
          IFLAG1=1
          GOTO299
        ENDIF
  297 CONTINUE
  299 CONTINUE
C
C               *****************************************
C               **  STEP 3--                           **
C               **  CALL DPSYS2 TO EXECUTE THE COMMAND **
C               *****************************************
C
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG2=0
      NCHAR=0
      IF(ICASE.EQ.'R   ')THEN
        IF(NCRPAT.GE.1)THEN
          DO311II=1,NCRPAT
            IF(IRRRPA(II:II).EQ.' ')THEN
              IFLAG2=1
              GOTO319
            ENDIF
  311     CONTINUE
  319     CONTINUE
          IF(IFLAG2.EQ.1 .AND. IRRRPA(1:1).NE.'"')THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)='"'
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+NCRPAT)=IRRRPA(1:NCRPAT)
          NCHAR=NCHAR+NCRPAT
        ENDIF
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')THEN
          WRITE(ICOUT,351)NCRPAT,NCHAR,IFLAG2,IRRRPA(1:1)
  351     FORMAT('DPEXRP STEP 3: NCRPAT,NCHAR,IFLAG2,IRRRPA(1:1) = ',
     1           3I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,353)IRRRPA
  353     FORMAT('IRRRPA: ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,355)(ITEXT3(JJ:JJ),JJ=1,MIN(100,NCHAR))
  355     FORMAT('ITEXT3: ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IHOST1.EQ.'IBM-')THEN
          IF(NCHAR.GT.0 .AND. ITEXT3(NCHAR:NCHAR).NE.IBASLC)THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)=IBASLC
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+11)='RSCRIPT.EXE'
          NCHAR=NCHAR+11
          IF(IFLAG2.EQ.1 .AND. IRRRPA(1:1).NE.'"')THEN
            ITEXT3(NCHAR+1:NCHAR+2)='" '
            NCHAR=NCHAR+2
          ELSE
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)=' '
          ENDIF
        ELSEIF(IOPSY1.EQ.'UNIX')THEN
          IF(NCHAR.GT.0 .AND. ITEXT3(NCHAR:NCHAR).NE.'/')THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)='/'
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+7)='Rscript'
          NCHAR=NCHAR+7
          IF(IFLAG2.EQ.1 .AND. IRRRPA(1:1).NE.'"')THEN
            ITEXT3(NCHAR+1:NCHAR+2)='" '
            NCHAR=NCHAR+2
          ELSE
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)=' '
          ENDIF
        ENDIF
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')THEN
          WRITE(ICOUT,356)NCHAR
  356     FORMAT('DPEXRP STEP 3A: NCHAR = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,355)(ITEXT3(JJ:JJ),JJ=1,MIN(100,NCHAR))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        NTEMP=NCHAR+NCFILE
        IF(IFLAG1.EQ.1)NTEMP=NTEMP+2
        IF(NTEMP.GT.MAXSTR)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)MAXSTR
  301     FORMAT('      SPECIFIED STRING EXCEEDS ',I5,' ',
     1           'CHARACTERS.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        IF(IFLAG1.EQ.1)THEN
          NCHAR=NCHAR+1
          ITEXT3(NCHAR:NCHAR)='"'
        ENDIF
        ITEXT3(NCHAR+1:NCHAR+NCFILE)=IFILE1(1:NCFILE)
        NCHAR=NCHAR+NCFILE
        IF(IFLAG1.EQ.1)THEN
          NCHAR=NCHAR+1
          ITEXT3(NCHAR:NCHAR)='"'
        ENDIF
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')THEN
          WRITE(ICOUT,366)NCHAR,IFLAG1
  366     FORMAT('DPEXRP STEP 3B: NCHAR,IFLAG1 = ',2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,355)(ITEXT3(JJ:JJ),JJ=1,MIN(100,NCHAR))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NCARG.GT.0)THEN
          IF(NCHAR+NCARG+1.GT.MAXSTR)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)MAXSTR
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          NCHAR=NCHAR+1
          ITEXT3(NCHAR:NCHAR)=' '
          ITEXT3(NCHAR+1:NCHAR+NCARG)=ARGLST(1:NCARG)
          NCHAR=NCHAR+NCARG
        ENDIF
      ELSEIF(ICASE.EQ.'PYTH')THEN
        IF(NCPYTH.GE.1)THEN
          DO361II=1,NCPYTH
            IF(IPYTPA(II:II).EQ.' ')THEN
              IFLAG2=1
              GOTO369
            ENDIF
  361     CONTINUE
  369     CONTINUE
          IF(IFLAG2.EQ.1 .AND. IPYTPA(1:1).NE.'"')THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)='"'
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+NCRPAT)=IRRRPA(1:NCRPAT)
          NCHAR=NCHAR+NCRPAT
        ENDIF
        IF(IHOST1.EQ.'IBM-')THEN
          IF(NCHAR.GT.0 .AND. ITEXT3(NCHAR:NCHAR).NE.IBASLC)THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)=IBASLC
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+6)='PYTHON'
          NCHAR=NCHAR+6
          IF(IFLAG2.EQ.1 .AND. IPYTPA(1:1).NE.'"')THEN
            ITEXT3(NCHAR+1:NCHAR+2)='" '
            NCHAR=NCHAR+2
          ELSE
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)=' '
          ENDIF
        ELSEIF(IOPSY1.EQ.'UNIX')THEN
          IF(NCHAR.GT.0 .AND. ITEXT3(NCHAR:NCHAR).NE.'/')THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)='/'
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+6)='python'
          NCHAR=NCHAR+6
          IF(IFLAG2.EQ.1 .AND. IPYTPA(1:1).NE.'"')THEN
            ITEXT3(NCHAR+1:NCHAR+2)='" '
            NCHAR=NCHAR+2
          ELSE
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)=' '
          ENDIF
        ENDIF
        NTEMP=NCHAR+NCFILE
        IF(IFLAG1.EQ.1)NTEMP=NTEMP+2
        IF(NTEMP.GT.MAXSTR)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)MAXSTR
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        IF(IFLAG1.EQ.1)THEN
          NCHAR=NCHAR+1
          ITEXT3(NCHAR:NCHAR)='"'
        ENDIF
        ITEXT3(NCHAR+1:NCHAR+NCFILE)=IFILE1(1:NCFILE)
        NCHAR=NCHAR+NCFILE
        IF(IFLAG1.EQ.1)THEN
          NCHAR=NCHAR+1
          ITEXT3(NCHAR:NCHAR)='"'
        ENDIF
        IF(NCARG.GT.0)THEN
          IF(NCHAR+NCARG+1.GT.MAXSTR)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)MAXSTR
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          NCHAR=NCHAR+1
          ITEXT3(NCHAR:NCHAR)=' '
          ITEXT3(NCHAR+1:NCHAR+NCARG)=ARGLST(1:NCARG)
          NCHAR=NCHAR+NCARG
        ENDIF
      ENDIF
C
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISYSPE='OFF'
      ISYSHI='ON'
      IF(IFLAG1.EQ.1 .OR. IFLAG2.EQ.1)ISYSHI='OFF'
      CALL DPSYS2(ITEXT3,NCHAR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
      GOTO9000
C
 8000 CONTINUE
      IERROR='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXRP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXRP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IFOUND,IERROR,NCHAR
 9017   FORMAT('IFOUND,IERROR,NCHAR = ',2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT3(J:J),J=1,MIN(NCHAR,220))
 9018   FORMAT('(ITEXT3(I),I=1,NCHAR) = ',220A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXR2(ICASE,IFILE1,NCFIL1,ARGLST,NCARG,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--RUN EITHER AN R OR A PYTHON SCRIPT THAT EXISTS IN THE
C              "/lib/scripts" DIRECTORY.  THESE SCRIPTS ARE CALLED BY
C              OTHER DATAPLOT SUBROUTINES TO PERFORM SPECIFIC TASKS.
C
C              IN ORDER FOR THIS COMMAND TO WORK CORRECTLY, THE
C              FOLLOWING NEEDS TO BE DONE:
C
C                 1) R (OR PYTHON) NEEDS TO BE INSTALLED ON YOUR LOCAL
C                    PLATFORM.
C
C                 2) YOU MAY NEED TO SET THE PATH FOR R OR PYTHON WITH
C                    THE COMMANDS
C
C                       SET PYTHON PATH ...
C                       SET R      PATH ...
C
C                 3) FOR PYTHON, YOU MAY NEED TO SPECIFY THE VERSION
C                    WITH THE COMMAND
C
C                       SET PYTHON VERSION <2/3>
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK OF THE
C           NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2020/02
C     ORIGINAL VERSION--FEBRUARY   2020.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER (LEN=*) :: IFILE1
      CHARACTER*80 ARGLST
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER (LEN=MAXSTR) :: ITEXT3
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER (LEN=MAXFNC) :: IFILE3
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPEN
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*12 IACC
      CHARACTER*1 IBASLC
      CHARACTER*1 IATEMP
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPEX'
      ISUBN2='R2  '
      CALL DPCONA(92,IBASLC)
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXR2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,ICASE,NCFIL1,NCARG
   53   FORMAT('IBUGS2,ISUBRO,ICASE,NCFIL1,NCARG = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IFILE1(1:80)
   54   FORMAT('IFILE1 = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ARGLST
   55   FORMAT('ARGLST = ',A80)
        CALL DPWRST('XXX','BUG ')
        IF(IOPSY1.EQ.'UNIX') THEN
          WRITE(ICOUT,61)IUNXNC,UNIXPN
   61     FORMAT('IUNXNC,UNIXPN = ',I5,2X,A80)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,63)NCPATH,PATH
   63     FORMAT('NCPATH,PATH = ',I5,2X,A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C            ****************************************************
C            **  STEP 2--                                      **
C            **  SEE IF SPECIFIED FILE EXISTS                  **
C            ****************************************************
C
C     NOTE: HAVING AN ISSUE GETTING QUOTES FOR THE SCRIPT FILE
C           TO WORK CORRECTLY UNDER WINDOWS.  SO AS A HACK, COPY
C           THE SCRIPT FILE TO THE CURRENT DIRECTORY BEFORE INVOKE
C           PYTHON.
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CHECK FOR FILE IN "scripts" DIRECTORY
C
C     ADD DATAPLOT AUXILLARY PATH
C
      IFILE2=' '
      NC1=0
      IFLAGQ=0
      IFILE3=' '
      IFILE3(1:NCFIL1)=IFILE1(1:NCFIL1)
C
      IF(IOPSY1.EQ.'UNIX') THEN
C
C       UNIX: REPLACE SPACE CHARACTER WITH "\ "
C
        ISTEPN='2B'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        DO105II=1,IUNXNC
          IF(UNIXPN(II:II).EQ.' ')THEN
            NC1=NC1+1
            IFILE2(NC1:NC1+1)='\ '
            NC=NC+1
          ELSE
            NC1=NC1+1
            IFILE2(NC1:NC1)=UNIXPN(II:II)
          ENDIF
  105   CONTINUE
        IF(IFILE2(NC1:NC1).EQ.'/')THEN
          IFILE2(NC1+1:NC1+8)='scripts/'
          NC1=NC1+8
        ELSE
          IFILE2(NC1+1:NC1+9)='/scripts/'
          NC1=NC1+9
        ENDIF
      ELSE
C
C       FOR WINDOWS, IF THERE ARE SPACE CHARACTERS NEED TO
C       QUOTE THE FILE NAME.  HOWEVER, DO NOT ADD QUOTES UNTIL
C       AFTER "DPINFI" CALLS.
C
        ISTEPN='2C'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        DO110II=1,NCPATH
          IF(PATH(II:II).EQ.' ')THEN
            IFLAGQ=1
            GOTO119
          ENDIF
  110   CONTINUE
  119   CONTINUE
        NC1=NCPATH
        IFILE2(1:NC1)=PATH(1:NC1)
C
        IF(IFILE2(NC1:NC1).EQ.'\')THEN
          IFILE2(NC1+1:NC1+8)='scripts\'
          NC1=NC1+8
        ELSE
          IFILE2(NC1+1:NC1+9)='\scripts\'
          NC1=NC1+9
        ENDIF
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)NC1,IFILE2
  151   FORMAT('AT 151: NC1,IFILE2 = ',I6,2X,A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ILAST=NCFIL1
C
      IF(NC1+ILAST.GT.MAXFNC)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
  211   FORMAT('      THE MAXIMUM NUMBER OF CHARACTERS FOR THE ',
     1         'SCRIPT FILE NAME EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
        GOTO8000
      ENDIF
C
C     FIRST, NAME AS IS
C
      IWIDTH=MAXFNC
      NCNT=NC1
      DO220II=1,ILAST
        NCNT=NCNT+1
        IFILE2(NCNT:NCNT)=IFILE1(II:II)
  220 CONTINUE
      IF(NCNT.LT.IWIDTH)IFILE2(NCNT+1:IWIDTH)=' '
      CALL DPINFI(IFILE2,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
C
      ISTEPN='2E'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')THEN
        IFILE1(1:IWIDTH)=IFILE2(1:IWIDTH)
        GOTO290
      ENDIF
C
C     NOW CONVERT TO UPPER CASE
C
      DO230II=NC1+1,NCNT
        IATEMP=IFILE2(II:II)
        CALL DPCOAN(IATEMP,IVAL)
        IF(IVAL.GE.97 .AND. IVAL.LE.122)THEN
          IVAL=IVAL-32
          CALL DPCONA(IVAL,IATEMP)
          IFILE2(II:II)=IATEMP
        ENDIF
  230 CONTINUE
      CALL DPINFI(IFILE2,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')THEN
        IFILE1(1:IWIDTH)=IFILE2(1:IWIDTH)
        GOTO290
      ENDIF
C
      ISTEPN='2F'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     NOW CONVERT TO LOWER CASE
C
      DO240II=NC1+1,NCNT
        IATEMP=IFILE2(II:II)
        CALL DPCOAN(IATEMP,IVAL)
        IF(IVAL.GE.65 .AND. IVAL.LE.90)THEN
          IVAL=IVAL+32
          CALL DPCONA(IVAL,IATEMP)
          IFILE2(II:II)=IATEMP
        ENDIF
  240 CONTINUE
      CALL DPINFI(IFILE2,IEXIST,IOPEN,IACC,
     1            ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.EQ.'YES' .OR. IEXIST.EQ.'ON')THEN
        IFILE1(1:IWIDTH)=IFILE2(1:IWIDTH)
        GOTO290
      ENDIF
C
      ISTEPN='2G'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,251)
  251 FORMAT('      THE SPECIFIED SCRIPT WAS NOT FOUND IN THE ',
     1       'lib/scripts DIRECTORY.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,253)IFILE1(1:80)
  253 FORMAT('      SCRIPT NAME: ',A80)
      CALL DPWRST('XXX','BUG ')
      GOTO8000
C
 290  CONTINUE
C
C     ADD QUOTES IF NEEDED
C
      IF(IFLAGQ.EQ.1)THEN
        IFILE2(2:NCNT+1)=IFILE1(1:NCNT)
        IFILE2(1:1)='"'
        IFILE2(NCNT+2:NCNT+2)='"'
        IFILE1(1:NCNT+2)=IFILE2(1:NCNT+2)
        NCNT=NCNT+2
C
C       FOR WINDOWS SYSTEMS, COPY SCRIPT FILE TO CURRENT
C       DIRECTORY
C
        IF(IHOST1.EQ.'IBM-')THEN
          CALL COPYFI(IFILE2,IFILE3,IBUGS2,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          IFILE1=' '
          IFILE1=IFILE3
          IFLAGQ=0
        ENDIF
      ENDIF
C
      DO293II=80,1,-1
        IF(IFILE1(II:II).NE.' ')THEN
          NCFILE=II
          GOTO295
        ENDIF
  293 CONTINUE
      NCFILE=1
  295 CONTINUE
C
C               *****************************************
C               **  STEP 3--                           **
C               **  CALL DPSYS2 TO EXECUTE THE COMMAND **
C               *****************************************
C
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NCHAR=0
      IFLAGQ=0
C
      IF(ICASE.EQ.'R   ')THEN
        IF(NCRPAT.GE.1)THEN
          DO311II=1,NCRPAT
            IF(IRRRPA(II:II).EQ.' ')THEN
              IF(IHOST1.EQ.'IBM-')THEN
                NCHAR=NCHAR+1
                ITEXT3(NCHAR:NCHAR)=IRRRPA(II:II)
                IFLAGQ=1
              ELSE
                NCHAR=NCHAR+1
                ITEXT3(NCHAR:NCHAR+1)='\ '
              ENDIF
            ELSE
              NCHAR=NCHAR+1
              ITEXT3(NCHAR:NCHAR)=IRRRPA(II:II)
            ENDIF
  311     CONTINUE
        ENDIF
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')THEN
          WRITE(ICOUT,351)NCRPAT,NCHAR,IFLAG2,IRRRPA(1:1)
  351     FORMAT('DPEXRP STEP 3: NCRPAT,NCHAR,IFLAG2,IRRRPA(1:1) = ',
     1           3I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,353)IRRRPA(1:80)
  353     FORMAT('IRRRPA: ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,355)(ITEXT3(JJ:JJ),JJ=1,MIN(100,NCHAR))
  355     FORMAT('ITEXT3: ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IHOST1.EQ.'IBM-')THEN
          IF(NCHAR.GT.0 .AND. ITEXT3(NCHAR:NCHAR).NE.IBASLC)THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)=IBASLC
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+11)='RSCRIPT.EXE'
          NCHAR=NCHAR+11
        ELSEIF(IOPSY1.EQ.'UNIX')THEN
          IF(NCHAR.GT.0 .AND. ITEXT3(NCHAR:NCHAR).NE.'/')THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)='/'
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+7)='Rscript'
          NCHAR=NCHAR+7
        ENDIF
C
        IF(IFLAGQ.EQ.1)THEN
          NCTEMP=0
          DO356II=NCHAR,1,-1
            ITEXT3(II+1:II+1)=ITEXT3(II:II)
  356     CONTINUE
          ITEXT3(1:1)='"'
          ITEXT3(NCHAR+2:NCHAR+2)='"'
          NCHAR=NCHAR+2
        ENDIF
        NCHAR=NCHAR+1
        ITEXT3(NCHAR:NCHAR)=' '
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')THEN
          WRITE(ICOUT,359)NCHAR
  359     FORMAT('DPEXRP STEP 3A: NCHAR = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,355)(ITEXT3(JJ:JJ),JJ=1,MIN(100,NCHAR))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        NTEMP=NCHAR+NCFILE
        IF(NTEMP.GT.MAXSTR)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1001)
 1001     FORMAT('***** ERROR IN RSCRIPT/PYTHON COMMAND--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)MAXSTR
  301     FORMAT('      SPECIFIED STRING EXCEEDS ',I5,' CHARACTERS.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        ITEXT3(NCHAR+1:NCHAR+NCFILE)=IFILE1(1:NCFILE)
        NCHAR=NCHAR+NCFILE
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')THEN
          WRITE(ICOUT,366)NCHAR,IFLAG1
  366     FORMAT('DPEXRP STEP 3B: NCHAR,IFLAG1 = ',2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,355)(ITEXT3(JJ:JJ),JJ=1,MIN(100,NCHAR))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NCARG.GT.0)THEN
          IF(NCHAR+NCARG+1.GT.MAXSTR)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)MAXSTR
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          NCHAR=NCHAR+1
          ITEXT3(NCHAR:NCHAR)=' '
          ITEXT3(NCHAR+1:NCHAR+NCARG)=ARGLST(1:NCARG)
          NCHAR=NCHAR+NCARG
        ENDIF
      ELSEIF(ICASE.EQ.'PYTH')THEN
        IF(NCPYTH.GE.1)THEN
          DO361II=1,NCPYTH
            IF(IPYTPA(II:II).EQ.' ')THEN
              IF(IHOST1.EQ.'IBM-')THEN
                NCHAR=NCHAR+1
                ITEXT3(NCHAR:NCHAR)=IPYTPA(II:II)
                IFLAGQ=1
              ELSE
                NCHAR=NCHAR+1
                ITEXT3(NCHAR:NCHAR)='\ '
                NCHAR=NCHAR+1
              ENDIF
            ELSE
              NCHAR=NCHAR+1
              ITEXT3(NCHAR:NCHAR)=IPYTPA(II:II)
            ENDIF
  361     CONTINUE
        ENDIF
C
        IF(IHOST1.EQ.'IBM-')THEN
          IF(NCHAR.GT.0 .AND. ITEXT3(NCHAR:NCHAR).NE.IBASLC)THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)=IBASLC
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+6)='PYTHON'
          NCHAR=NCHAR+6
        ELSEIF(IOPSY1.EQ.'UNIX')THEN
          IF(NCHAR.GT.0 .AND. ITEXT3(NCHAR:NCHAR).NE.'/')THEN
            NCHAR=NCHAR+1
            ITEXT3(NCHAR:NCHAR)='/'
          ENDIF
          ITEXT3(NCHAR+1:NCHAR+6)='python'
          NCHAR=NCHAR+6
        ENDIF
C
        IF(IFLAGQ.EQ.1)THEN
          NCTEMP=0
          DO386II=NCHAR,1,-1
            ITEXT3(II+1:II+1)=ITEXT3(II:II)
  386     CONTINUE
          ITEXT3(1:1)='"'
          ITEXT3(NCHAR+2:NCHAR+2)='"'
          NCHAR=NCHAR+2
        ENDIF
        NCHAR=NCHAR+1
        ITEXT3(NCHAR:NCHAR)=' '
C
        NTEMP=NCHAR+NCFILE
        IF(NTEMP.GT.MAXSTR)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)MAXSTR
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        ITEXT3(NCHAR+1:NCHAR+NCFILE)=IFILE1(1:NCFILE)
        NCHAR=NCHAR+NCFILE
        IF(NCARG.GT.0)THEN
          IF(NCHAR+NCARG+1.GT.MAXSTR)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)MAXSTR
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          NCHAR=NCHAR+1
          ITEXT3(NCHAR:NCHAR)=' '
          ITEXT3(NCHAR+1:NCHAR+NCARG)=ARGLST(1:NCARG)
          NCHAR=NCHAR+NCARG
        ENDIF
      ENDIF
C
C     FOR WINDOWS, ADD BEGINNING AND ENDING QUOTES IF
C     STRING CONTAINS ANY QUOTES.
C
      IF(IHOST1.EQ.'IBM-')THEN
        IF(IFLAGQ.EQ.0)THEN
          DO7010II=1,NCHAR
            IF(ITEXT3(II:II).EQ.'"')THEN
              ITEXT3(2:NCHAR+1)=ITEXT3(1:NCHAR)
              ITEXT3(1:1)='"'
              ITEXT3(NCHAR+2:NCHAR+2)='"'
              NCHAR=NCHAR+2
              GOTO7019
            ENDIF
 7010     CONTINUE
 7019     CONTINUE
        ENDIF
      ENDIF
C
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISYSPE='OFF'
      ISYSHI='ON'
      IF(IFLAG1.EQ.1 .OR. IFLAG2.EQ.1)ISYSHI='OFF'
      CALL DPSYS2(ITEXT3,NCHAR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
      GOTO9000
C
 8000 CONTINUE
      IERROR='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXR2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXR2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IFOUND,IERROR,NCHAR
 9017   FORMAT('IFOUND,IERROR,NCHAR = ',2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT3(J:J),J=1,MIN(NCHAR,220))
 9018   FORMAT('(ITEXT3(I),I=1,NCHAR) = ',220A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXS1(ISTRIN,NCOLMX,ISTART,ISTOP,K,MESSAG,
     1                  ICOL1,ICOL2,ISTRI2,NCSTR2,
     1                  IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--SCAN THE CHARACTER*130 STRING ISTRIN
C              BETWEEN COLUMNS ISTART TO ISTOP
C              AND EXTRACT THE K-TH STRING.
C              DEBLANK THIS STRING, PLACE IT INTO
C              THE CHARACTER*130 STRING ISTRI2,
C              AND PLACE THE LENGTH OF
C              THE STRING INTO NCSTR2.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--JANUARY   1988.
C     UPDATED         --SEPTEMBER 2014. DO NOT RESTRICT TO 130 COLUMNS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) ISTRIN
      CHARACTER*(*) ISTRI2
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 MESSAG
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='S1  '
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXS1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXS1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MESSAG,IBUGS2,ISUBRO,IERROR
   53   FORMAT('MESSAG,IBUGS2,ISUBRO,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ISTRIN(J:J),J=1,100)
   54   FORMAT('(ISTRIN(J:J),J=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ISTART,ISTOP,K,NCOLMX
   55   FORMAT('ISTART,ISTOP,K,NCOLMX = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************
C               **  STEP 11--                       **
C               **  INITIALIZE THE OUTPUT VARIABLES **
C               **************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXS1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1100I=1,NCOLMX
      ISTRI2(I:I)=' '
 1100 CONTINUE
      NCSTR2=0
C
C               *******************************************
C               **  STEP 12--                            **
C               **  CHECK THE INPUT ARGUMENTS            **
C               *******************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXS1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.GE.1.AND.ISTOP.GE.1.AND.
     1   ISTART.LE.NCOLMX.AND.ISTOP.LE.NCOLMX)GOTO1219
C
      IF(MESSAG.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPEXS1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)NCOLMX
 1212   FORMAT('      ISTART OR ISTOP IS < 1 OR > ',I6,'. ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1213)ISTART
 1213   FORMAT('      ISTART  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)ISTOP
 1214   FORMAT('      ISTOP   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,100)
 1216   FORMAT('      (ISTRIN(I:I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
 1219 CONTINUE
C
      IF(ISTART.GT.ISTOP)THEN
        IF(MESSAG.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1222)
 1222     FORMAT('      ISTART EXCEEDS ISTOP')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1223)ISTART
 1223     FORMAT('      ISTART  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1224)ISTOP
 1224     FORMAT('      ISTOP   = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,100)
 1226     FORMAT('      (ISTRIN(I:I),I=1,100) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(K.LT.1)THEN
        IF(MESSAG.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1232)
 1232     FORMAT('      K      IS LESS THAN 1 .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1233)K
 1233     FORMAT('      K       = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,100)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ************************************************
C               **  STEP 21--                                 **
C               **  IDENTIFY THE COLUMNS WHERE                **
C               **  THE K     -TH STRING RESIDES              **
C               **  ICOL1 = START COLUMN OF A STRING          **
C               **  ICOL2 = STOP  COLUMN OF A STRING          **
C               ************************************************
C
      ICOL2=ISTART-1
      DO2100ILOOP=1,K
C
        ICOL1=ISTOP+1
        IMIN=ICOL2+1
        IF(IMIN.LE.ISTOP)THEN
          DO2110I=IMIN,ISTOP
            I2=I
            IF(ISTRIN(I:I).NE.' ')THEN
              ICOL1=I2
              GOTO2119
            ENDIF
 2110     CONTINUE
          ICOL1=ISTOP+1
 2119     CONTINUE
        ENDIF
C
        ICOL2=ISTOP
        IMIN=ICOL1+1
        IF(IMIN.LE.ISTOP)THEN
          DO2120I=IMIN,ISTOP
            I2=I
            IF(ISTRIN(I:I).EQ.' ')THEN
              ICOL2=I2-1
              GOTO2129
            ENDIF
 2120     CONTINUE
          ICOL2=ISTOP
 2129     CONTINUE
        ENDIF
C
        IF(ICOL1.GE.ISTART.AND.ICOL2.GE.ISTART.AND.
     1     ICOL1.LE.ISTOP.AND.ICOL2.LE.ISTOP)GOTO2139
        IF(MESSAG.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2132)
 2132     FORMAT('      ICOL1 OR ICOL2 IS < ISTART OR > ISTOP. ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2133)ICOL1
 2133     FORMAT('      ICOL1  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2134)ICOL2
 2134     FORMAT('      ICOL2  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2135)ISTART
 2135     FORMAT('      ISTART = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2136)ISTOP
 2136     FORMAT('      ISTOP  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1226)(ISTRIN(I:I),I=1,100)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
 2139   CONTINUE
C
        IF(ICOL1.GT.ICOL2)THEN
          IF(MESSAG.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2142)
 2142       FORMAT('      ICOL1 EXCEEDS ICOL2')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2143)ICOL1
 2143       FORMAT('      ICOL1  = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2144)ICOL2
 2144       FORMAT('      ICOL2  = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,100)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
 2100 CONTINUE
C
C               *********************************************
C               **  STEP 22--                              **
C               **  COPY THE K     -TH STRING INTO ISTRI2  **
C               *********************************************
C
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXS1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOL1.LE.ICOL2)THEN
        J=0
        DO2200I=ICOL1,ICOL2
          J=J+1
          ISTRI2(J:J)=ISTRIN(I:I)
 2200   CONTINUE
        NCSTR2=J
      ELSE
        NCSTR2=0
        ISTRI2=' '
      ENDIF
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXS1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXS1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ISTART,ISTOP,ICOL1,ICOL2,K,NCSTR2
 9015   FORMAT('ISTART,ISTOP,ICOL1,ICOL2,K,NCSTR2 = ',6I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR2.GE.1)THEN
          WRITE(ICOUT,9023)(ISTRI2(I:I),I=1,MIN(100,NCSTR2))
 9023     FORMAT('(ISTRI2(I:I),I=1,NCSTR2) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXS2(IFOLOC,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1                  IANS,IWIDTH,IHPN,IHPN2,ASTART,AINC,ASTOP,
     1                  NUMINC,ILALOC,IBUGA3,IFOUND,IERROR)
C
C     NOTE--THIS SUBROUTINE IS IDENTICAL TO SUBROUTINE DPEXSE
C           AND HAS BEEN CREATED SO AS TO ACHIEVE
C           STORAGE ECONOMY IN THE MAPPING/LOADING.
C
C     NOTE--DPEXSE REMOVED 2012/09 AND ALL CALLS TO DPEXSE CHANGED
C           TO DPEXS2.
C
C     PURPOSE--EXTRACT THE SEQUENCE OF VALUES
C            AS DICTATED BY A FOR SPECIFICATION.
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
C     ORIGINAL VERSION--OCTOBER   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --SEPTEMBER 2012. RECODED A BIT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
      CHARACTER*4 IANS
      CHARACTER*4 IHPN
      CHARACTER*4 IHPN2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='S2  '
      IERROR='NO'
      IFOUND='YES'
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IFOLOC,NUMARG
   52   FORMAT('IFOLOC,NUMARG = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NUMARG
          WRITE(ICOUT,56)I,IHARG(I),IHARG2(I),IARG(I),ARG(I)
   56     FORMAT('I,IHARG(I),IHARG2(I),IARG(I),ARG(I) = ',
     1           I8,2A4,2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *******************************************************
C               **  STEP 3--                                        **
C               **  DETERMINE THE NAME OF THE  NEXT  DUMMY VARIABLE **
C               **  (IT NEVER GETS STORED PERMANENTLY)               **
C               **  IMMEDIATELY FOLLOWING THE        'FOR' KEYWORD   **
C               *******************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFOLOC.GT.NUMARG)THEN
        IBRAN=3161
        WRITE(ICOUT,3121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3162)
 3162   FORMAT('      THE        FOR    NOT FOUND,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3163)
 3163   FORMAT('      EVEN THOUGH THE STRING    =    WAS FOUND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3136)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3137)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSEIF(IFOLOC.EQ.NUMARG)THEN
        WRITE(ICOUT,3121)
 3121   FORMAT('***** ERROR IN DPEXS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3122)
 3122   FORMAT('      THE       FOR    WAS THE FINAL WORD ON THE ',
     1         'COMMAND LINE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3124)
 3124   FORMAT('      THE WORD    FOR    SHOULD HAVE BEEN FOLLOWED ',
     1         'BY 5 WORDS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3126)
 3126   FORMAT('      1) A DUMMY VARIABLE NAME;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3127)
 3127   FORMAT('      2) AN EQUAL SIGN;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3128)
 3128   FORMAT('      3) ONE LIMIT (LOWER OR UPPER) ',
     1         'FOR THE DUMMY VARIABLE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3129)
 3129   FORMAT('      4) THE INCREMENT FOR THE DUMMY VARIABLE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3130)
 3130   FORMAT('      5) THE OTHER LIMIT (UPPER OR LOWER) ',
     1         'FOR THE DUMMY VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3136)
 3136   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3137)(IANS(I),I=1,MIN(100,IWIDTH))
 3137     FORMAT('      ',100A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               *******************************************************
C               **  STEP 3.1--                                       **
C               **  DETERMINE THE NAME OF THE        DUMMY VARIABLE  **
C               **  (IT NEVER GETS STORED PERMANENTLY)               **
C               **  IMMEDIATELY FOLLOWING THE        'FOR' KEYWORD   **
C               *******************************************************
C
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFOLP1=IFOLOC+1
      IFOLP2=IFOLOC+2
      IFOLP3=IFOLOC+3
      IFOLP4=IFOLOC+4
      IFOLP5=IFOLOC+5
C
C     CHECK THAT THE START, INC, AND STOP VALUES WERE GIVEN
C     AND THAT THEY ARE IN FACT NUMERIC FIELDS
C
      IF(IFOLP3.GT.NUMARG .OR. IARGT(IFOLP3).NE.'NUMB')THEN
        WRITE(ICOUT,3121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3172)
 3172   FORMAT('      THE START VALUE FOR THE LOOP WAS EITHER NOT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3174)
 3174   FORMAT('      GIVEN OR IT IS NOT A NUMERIC VALUE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3136)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3137)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSEIF(IFOLP4.GT.NUMARG .OR. IARGT(IFOLP4).NE.'NUMB')THEN
        WRITE(ICOUT,3121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3182)
 3182   FORMAT('      THE INCREMENT VALUE FOR THE LOOP WAS EITHER NOT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3184)
 3184   FORMAT('      GIVEN OR IT IS NOT A NUMERIC VALUE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3136)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3137)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSEIF(IFOLP5.GT.NUMARG .OR. IARGT(IFOLP5).NE.'NUMB')THEN
        WRITE(ICOUT,3121)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3192)
 3192   FORMAT('      THE STOP VALUE FOR THE LOOP WAS EITHER NOT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3194)
 3194   FORMAT('      GIVEN OR IT IS NOT A NUMERIC VALUE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3136)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3137)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               *******************************************
C               **  STEP 4--                             **
C               *******************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHPN=IHARG(IFOLP1)
      IHPN2=IHARG2(IFOLP1)
C
      ASTART=ARG(IFOLP3)
C
      AINC=0.0
      IF(IFOLP4.GT.NUMARG)GOTO3240
      IF(IHARG(IFOLP4).EQ.'FOR '.AND.IHARG2(IFOLP4).EQ.'    ')GOTO3240
      IF(IHARG(IFOLP4).EQ.'SUBS'.AND.IHARG2(IFOLP4).EQ.'ET  ')GOTO3240
      IF(IHARG(IFOLP4).EQ.'EXCE'.AND.IHARG2(IFOLP4).EQ.'PT  ')GOTO3240
      AINC=ARG(IFOLP4)
 3240 CONTINUE
C
      ASTOP=ASTART
      IF(IFOLP4.GT.NUMARG)GOTO3250
      IF(IHARG(IFOLP4).EQ.'FOR '.AND.IHARG2(IFOLP4).EQ.'    ')GOTO3250
      IF(IHARG(IFOLP4).EQ.'SUBS'.AND.IHARG2(IFOLP4).EQ.'ET  ')GOTO3250
      IF(IHARG(IFOLP4).EQ.'EXCE'.AND.IHARG2(IFOLP4).EQ.'PT  ')GOTO3250
      ASTOP=ARG(IFOLP5)
 3250 CONTINUE
C
      NUMINC=0
      IF(AINC.NE.0.0)NUMINC=INT((ASTOP-ASTART)/AINC)
      IF(NUMINC.LT.0)NUMINC=-NUMINC
      NUMINC=NUMINC+1
C
      ILALOC=IFOLP3
      IF(IFOLP4.GT.NUMARG)GOTO3260
      IF(IHARG(IFOLP4).EQ.'FOR '.AND.IHARG2(IFOLP4).EQ.'    ')GOTO3260
      IF(IHARG(IFOLP4).EQ.'SUBS'.AND.IHARG2(IFOLP4).EQ.'ET  ')GOTO3260
      IF(IHARG(IFOLP4).EQ.'EXCE'.AND.IHARG2(IFOLP4).EQ.'PT  ')GOTO3260
      ILALOC=IFOLP5
 3260 CONTINUE
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IHPN,IHPN2,NUMINC,ILALOC
 9012   FORMAT('IHPN,IHPN2,NUMINC,ILALOC = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ASTART,AINC,ASTOP
 9013   FORMAT('ASTART,AINC,ASTOP = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXS3(ISEED,XMAT,N,NUMFAC,Y,
     1IBUGA3,ISUBRO,IERROR)
C
CCCCC SUBROUTINE DPEXS3(ISIMID,IAUTH,IBOOK,IPAGE,
CCCCC1GMEAN,INDEXB,B,NUMB,
CCCCC1GSD,INDEXS,S,NUMS,
CCCCC1DMINT,DMSLOP,
CCCCC1DSINT,DSSLOP,
C
C     PURPOSE--GENERATE SIMULATED RESPONSES
C              FROM AN EXPERIMENTAL MODEL.
C     INPUT  ARGUMENTS--IPAGE  = PAGE NUMBER
C                       ISEED   = CURRENT VALUE OF RANDOM NUMBER SEED
C                       X1      = FACTOR 1
C                       X2      = FACTOR 2
C                       X3      = FACTOR 3
C                       .
C                       .
C                       .
C                       N       = NUMBER OF ELEMENTS IN EACH FACTOR.
C                       NUMFAC   = NUMBER OF FACTORS PROVIDED.
C     OUTPUT ARGUMENTS--Y      = A SINGLE PRECISION VECTOR
C                                INTO WHICH THE GENERATED
C                                SIMULATED RESPONSE WILL BE PLACED.
C     OUTPUT--A SIMULATED SAMPLE OF SIZE N
C             FROM THE MODEL
C                Y = F(X1,X2,X3, ...) + RANDOM ERROR
C                  + DMINT + DMSLOP*TIME + RANDOM ERROR
C             WHERE (FOR EXAMPLE)
C                F(X1,X2,X3) =
C                B0 + 0.5 * [ B1*X1 + B2*X2 + B3*X3 +
C                B12*X1*X2 + B13*X1*X3 + B23*X2*X3 +
C                B123*X1*X2*X3) ]
C             WHERE
C                XK = TAKES ON 2 VALUES: +1 AND -1
C             AND WHERE
C                B0 = 71.25
C                B1 = 23
C                B2 = -5
C                B3 = 1.5
C                B12 = 1.5
C                B13 = 10
C                B23 = 0
C                B123 = 0.5
C             AND
C                SD(ERROR) = 0.1
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--89.1
C     ORIGINAL VERSION--JANUARY   1989.
C     UPDATED         --APRIL   1992.  NUMCOL TO NUMFAC
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
C
      DIMENSION XMAT(20,20)
C
      DIMENSION IDIGIT(10)
C
      DIMENSION DET(100)
C
      DIMENSION Z1(100)
      DIMENSION SRAND1(100)
      DIMENSION RAND1(100)
C
      DIMENSION TIME(100)
C
      DIMENSION Z2(100)
      DIMENSION SRAND2(100)
      DIMENSION RAND2(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DEX3')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPEXS3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ISIMID,N,NUMFAC
   53 FORMAT('ISIMID,N,NUMFAC = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO56I=1,N
CCCCC THE FOLLOWING 2 LINES WERE CHANGED   APRIL 1992  (ALAN)
CCCCC WRITE(ICOUT,57)I,(XMAT(I,J),J=1,NUMCOL)
CCC57 FORMAT('I,(XMAT(I,J),J=1,NUMCOL) = ',I8,10F7.1)
CCCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,57)I,(XMAT(I,J),J=1,NUMFAC)
   57 FORMAT('I,(XMAT(I,J),J=1,NUMFAC) = ',I8,10F7.1)
      CALL DPWRST('XXX','BUG ')
   56 CONTINUE
   90 CONTINUE
C
      HALF=0.5
C
C               **************************************************
C               **  STEP 11--                                   **
C               **  DEFINE THE DEFAULT MODEL                    **
C               **************************************************
C
      IF(ISIMID.LE.1)GOTO1100
      GOTO1190
C
 1100 CONTINUE
      GMEAN=71.25
      NUMB=7
      INDEXB(1)=1
      INDEXB(2)=2
      INDEXB(3)=3
      INDEXB(4)=12
      INDEXB(5)=13
      INDEXB(6)=23
      INDEXB(7)=123
      B(1)=23.0
      B(2)=(-5.0)
      B(3)=1.5
      B(4)=1.5
      B(5)=10.0
      B(6)=0.0
      B(7)=0.5
C
CCCCC GSD=0.1
      GSD=0.0
      NUMS=0
C
      DMINT=0.0
      DMSLOP=0.0
C
      DSINT=0.0
      DSSLOP=0.0
C
 1190 CONTINUE
C
C               **************************************************
C               **  STEP 21--                                   **
C               **  COMPUTE THE DETERMINISTIC COMPONENT         **
C               **************************************************
C
      IF(N.LE.0)GOTO2119
      DO2110I=1,N
      SUM=0.0
      SUM=SUM+GMEAN
C
      IF(NUMB.LE.0)GOTO2129
      DO2120J=1,NUMB
      TERM1=B(J)
      TERM2=1.0
      J2=INDEXB(J)
      CALL EXTDIG(J2,IDIGIT,NDIGIT,IBUGA3,IERROR)
C
      IF(NDIGIT.LE.0)GOTO2139
      DO2130K=1,NDIGIT
      L=IDIGIT(K)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEX3')
     1WRITE(ICOUT,2131)NDIGIT,K,L,I,TERM2,XMAT(I,L)
 2131 FORMAT('NDIGIT,K,L,I,TERM2,XMAT(I,L) = ',4I8,2E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEX3')
     1CALL DPWRST('XXX','BUG ')
      TERM2=TERM2*XMAT(I,L)
 2130 CONTINUE
 2139 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEX3')
     1WRITE(ICOUT,2132)I,SUM,TERM1,TERM2
 2132 FORMAT('I,SUM,TERM1,TERM2 = ',I8,3E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEX3')
     1CALL DPWRST('XXX','BUG ')
      SUM=SUM+HALF*TERM1*TERM2
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEX3')
     1WRITE(ICOUT,2133)I,SUM,TERM1,TERM2
 2133 FORMAT('I,SUM,TERM1,TERM2 = ',I8,3E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEX3')
     1CALL DPWRST('XXX','BUG ')
 2120 CONTINUE
 2129 CONTINUE
C
      DET(I)=SUM
CCCCC WRITE(ICOUT,778)I,DET(I)
CC778 FORMAT('I,DET(I) = ',I8,E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
 2110 CONTINUE
 2119 CONTINUE
C
C               **************************************************
C               **  STEP 22--                                   **
C               **  COMPUTE THE RANDOM COMPONENT                **
C               **************************************************
C
      CALL NORRAN(N,ISEED,Z1)
C
      IF(N.LE.0)GOTO2219
      DO2210I=1,N
      SUM=0.0
      SUM=SUM+GSD
C
      IF(NUMS.LE.0)GOTO2229
      DO2220J=1,NUMS
      TERM1=S(J)
      TERM2=1.0
      J2=INDEXS(J)
      CALL EXTDIG(J2,IDIGIT,NDIGIT,IBUGA3,IERROR)
C
      IF(NDIGIT.LE.0)GOTO2239
      DO2230K=1,NDIGIT
      L=IDIGIT(K)
      TERM2=TERM2*XMAT(I,L)
 2230 CONTINUE
 2239 CONTINUE
C
      SUM=SUM+HALF*TERM1*TERM2
 2220 CONTINUE
 2229 CONTINUE
C
      SRAND1(I)=SUM
      RAND1(I)=Z1(I)*SRAND1(I)
 2210 CONTINUE
 2219 CONTINUE
C
C               **************************************************
C               **  STEP 23--                                   **
C               **  COMPUTE THE CONTRIBUTION DUE TO A           **
C               **  CHANGE IN LOCATION WITH TIME (DRIFT)        **
C               **************************************************
C
      DO2300I=1,N
      AI=I
      TIME(I)=DMINT*DMSLOP*AI
 2300 CONTINUE
C
C               **************************************************
C               **  STEP 24--                                   **
C               **  COMPUTE THE CONTRIBUTION DUE TO A           **
C               **  CHANGE IN VARIATION WITH TIME               **
C               **************************************************
C
      CALL NORRAN(N,ISEED,Z2)
      DO2400I=1,N
      AI=I
      SRAND2(I)=DSINT+DSSLOP*AI
      RAND2(I)=Z2(I)*SRAND2(I)
 2400 CONTINUE
C
C               **************************************************
C               **  STEP 29--                                   **
C               **  COMPUTE THE FINAL RESPONSE =                **
C               **  SUM OF ALL CONTRIBUTIONS                    **
C               **************************************************
C
      DO2900I=1,N
      Y(I)=DET(I)+RAND1(I)+TIME(I)+RAND2(I)
CCCCC WRITE(ICOUT,779)I,DET(I),RAND1(I),Y(I)
CC779 FORMAT('I,DET(I),RAND1(I),Y(I) = ',I8,3E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2911)I,Y(I),(XMAT(I,J),J=1,NUMFAC)
 2911 FORMAT(I4,'--','RESULT = ',F10.5,5X,15F7.2)
      CALL DPWRST('XXX','BUG ')
 2900 CONTINUE
C
C               **************************************************
C               **   STEP 90--                                  **
C               **   EXIT                                       **
C               **************************************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DEX3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXS3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3
 9012   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ISIMID,N,NUMFAC,GMEAN
 9013   FORMAT('ISIMID,N,NUMFAC,GMEAN = ',3I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO9016I=1,N
          WRITE(ICOUT,9017)I,XMAT(I,1),XMAT(I,2),XMAT(I,3),Y(I)
 9017     FORMAT('I,XMAT(I,1),XMAT(I,2),XMAT(I,3),Y(I) = ',I8,6E11.4)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
        WRITE(ICOUT,9022)TERM1,TERM2,SUM
 9022   FORMAT('TERM1,TERM2,SUM = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXSI(ISEED,MAXNXT,ICASAN,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT AN EXPERIMENTAL SIMULATION
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/6
C     ORIGINAL VERSION--MAY       1989.
C     UPDATED         --JULY      1989. CHAR*4 STATEMENTS FOR IDEXEF & IEXSIA
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IDEXEF
      CHARACTER*4 IEXSIA
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION XMAT(20,20)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPEX'
      ISUBN2='SI  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      MAXV2=2
      MINN2=2
C
      ICOLL=0
      ICOLX=0
      ICOLXI=0
C
      NUMVAR=0
      NUMCOM=0
      NUMFAC=0
C
      IEXSIA='NONP'
      IDEXEF='STAT'
C
C               *********************************************
C               **  TREAT THE EXPERIMENTAL SIMULATION CASE **
C               *********************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'EXSI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXSI--')
       CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO  = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASAN,ISEED,MAXNXT
   53   FORMAT('ICASAN,ISEED,MAXNXT = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 11--                                      **
C               **  EXTRACT THE COMMAND                            **
C               **  DETERMINE THE LOCATION     (IN IHARG(.))       **
C               **  OF THE WORD      SIMULATION OR RUN             **
C               **  PLACE IT IN    ILASTC   .                      **
C               **  THEN SHIFT LEFT THE ENTIRE COMMAND LINE        **
C               **  SO THAT THE FIRST VARIABLE ARGUMENT            **
C               **  IS MOVED TO  IHARG(1)                          **
C               *****************************************************
C
      IF(ICOM.EQ.'RUN')GOTO590
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIMU')GOTO501
      IFOUND='NO'
      GOTO9000
C
  501 CONTINUE
      ILASTC=1
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
  590 CONTINUE
      IFOUND='YES'
C
C               ***********************************************************
C               **  STEP 21--                                            **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.      **
C               ***********************************************************
C
      ISTEPN='21'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'EXSI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               **************************************************
C               **  STEP 22-                                    **
C               **  EXTRACT EACH ARGUMENT                       **
C               **  TREAT THE ALL-NUMBER/PARAMETER CASE         **
C               **************************************************
C
      NUMROW=1
      NUMCOL=NUMARG
      DO2200J=1,NUMARG
      XMAT(1,J)=ARG(J)
 2200 CONTINUE
C
C               **************************************************
C               **  STEP 31--                                   **
C               **  GENERATE A SIMULATED VALUE(S)               **
C               **************************************************
C
      CALL DPEXS3(ISEED,XMAT,NUMROW,NUMCOL,Y,
     1IBUGA3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'OFF'.AND.ISUBRO.NE.'EXSI')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPEXSI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ISUBRO,IBUGA2,IBUGA3,IBUGQ
 9012 FORMAT('ISUBRO,IBUGA2,IBUGA3,IBUGQ  = ',
     1A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IFOUND,IERROR
 9013 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ICASAN
 9014 FORMAT('ICASAN = ',A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPEXSS(X1,Y1,D1,N1,DTARG,
     1X2,Y2,N2,DHIT,
     1IBUGU2,ISUBRO,IERROR)
C
C     PURPOSE--EXTRACT A SUBSET.
C              GIVEN THE VECTORS X1 AND Y1 CONTAINING N1
C              COORDINATES, AND THE VECTOR D1 CONTAINING
C              TAG INFORMATION, EXTRACT A SUBSET.
C              TWO CAPABILITITIES EXIST--
C                 CASE 1--IF DTARG IS SET AT CPUMIN,
C                         THEN SEARCH THE D1 VECTOR FOR EACH
C                         TAG VALUE.  THE FIRST SUCH TAG VALUE
C                         WHICH OCCURS MULTIPLE TIMES WILL
C                         HAVE ITS X AND Y VALUES EXTRACTED
C                         AND PLACED IN X2 AND Y2.
C                         THE EXTRACTED NUMBER OF OBSERVATIONS
C                         WILL BE PLACED IN N2.
C                         THE SUCCESSFUL TAG VALUE WILL BE PLACED
C                         IN DHIT.
C                         IF NO MULTIPLE VALUES ARE FOUND,
C                         THEN DHIT WILL BE SET AT THE LAST TAG
C                         VALUE EXAMINED, AND N2 WILL BE SET TO 1.
C                         IF MORE THAN ONE TAG VALUE HAS MULTIPLES,
C                         ONLY THE FIRST WILL BE USED; THE OTHERS
C                         WILL BE IGNORED.
C                 CASE 2--IF DTARG IS SET AT SOME FINITE VALUE,
C                         THEN THE SUBSET IS EXTRACTED AND PLACED
C                         IN X2 AND Y2, AND THE OUTPUT
C                         NUMBER OF OBSERVATIONS IS PLACED IN N2.
C                         DHIT IS SET TO DTARG.
C                         IF NO OCCURRANCES ARE FOUND,
C                         THEN N2 WILL BE SET TO 0.
C     CAUTION--THE OUTPUT VECTORS X2 AND Y2
C              MUST NOT BE THE SAME AS THE
C              INPUT VECTORS X AND Y IN THE CALLING SEQUENCE.
C     ORIGINAL VERSION--SEPTEMBER 1988
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGU2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION X1(*)
      DIMENSION Y1(*)
      DIMENSION D1(*)
      DIMENSION X2(*)
      DIMENSION Y2(*)
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='SS  '
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'EXSS')GOTO50
      GOTO90
   50 CONTINUE
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPEXSS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGU2,ISUBRO,IERROR
   52 FORMAT('IBUGU2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)CPUMIN
   53 FORMAT('CPUMIN = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)N1,DTARG
   61 FORMAT('N1,DTARG = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      DO62I=1,N1
      WRITE(ICOUT,63)I,X1(I),Y1(I),D1(I)
   63 FORMAT('I,X1(I),Y1(I),D1(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
   62 CONTINUE
   90 CONTINUE
C
C               **************************************************
C               **  STEP 10--                                   **
C               **  BRANCH TO THE APPROPRIATE CASE              **
C               **************************************************
C
      IF(DTARG.EQ.CPUMIN)GOTO1100
      GOTO1200
C
C               **************************************************
C               **  STEP 11--                                   **
C               **  TREAT THE FLOATING TAG CASE                 **
C               **************************************************
C
 1100 CONTINUE
C
      N2=0
      DHIT=DTARG
      IF(N1.LE.1)GOTO1190
C
      N1M1=N1-1
      DO1110I=1,N1M1
      DTARG2=D1(I)
      IP1=I+1
C
      ICOUNT=0
      ICOUNT=ICOUNT+1
      X2(ICOUNT)=X1(I)
      Y2(ICOUNT)=Y1(I)
C
      DO1120J=IP1,N1
      IF(D1(J).EQ.DTARG2)GOTO1130
      GOTO1120
 1130 CONTINUE
      ICOUNT=ICOUNT+1
      X2(ICOUNT)=X1(J)
      Y2(ICOUNT)=Y1(J)
 1120 CONTINUE
C
      IF(ICOUNT.GE.2)GOTO1140
      GOTO1110
C
 1140 CONTINUE
      N2=ICOUNT
      DHIT=DTARG2
      GOTO1190
C
 1110 CONTINUE
C
 1190 CONTINUE
      GOTO9000
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  TREAT THE PRE-SPECIFIED FINITE TAG CASE     **
C               **************************************************
C
 1200 CONTINUE
C
      DTARG2=DTARG
C
      ICOUNT=0
      DO1220J=1,N1
      IF(D1(J).EQ.DTARG2)GOTO1230
      GOTO1220
 1230 CONTINUE
      ICOUNT=ICOUNT+1
      X2(ICOUNT)=X1(J)
      Y2(ICOUNT)=Y1(J)
 1220 CONTINUE
      N2=ICOUNT
      DHIT=DTARG2
C
      GOTO9000
C
C               **************************************************
C               **  STEP 90--                                   **
C               **  EXIT.                                       **
C               **************************************************
C
 9000 CONTINUE
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'EXSS')GOTO9010
      GOTO9090
 9010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE BEGINNING OF DPEXSS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGU2,ISUBRO,IERROR
 9012 FORMAT('IBUGU2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)CPUMIN
 9013 FORMAT('CPUMIN = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)N1,DTARG
 9021 FORMAT('N1,DTARG = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9022I=1,N1
      WRITE(ICOUT,9023)I,X1(I),Y1(I),D1(I)
 9023 FORMAT('I,X1(I),Y1(I),D1(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
      WRITE(ICOUT,9031)N2,DHIT
 9031 FORMAT('N2,DHIT = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9032I=1,N2
      WRITE(ICOUT,9033)I,X2(I),Y2(I)
 9033 FORMAT('I,X2(I),Y2(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXCHF,
     1IFUNC2,N2,IBUGA3,IFOUND,IERROR)
C
C     PURPOSE--EXTRACT A STRING.
C     THE EXTRACTED STRING WILL BE DEFINED
C     BY THE FIRST NON-BLANK CHARACTER AFTER
C     THE WORD (OR THE COMPLETION OF THE WORD)
C     DEFINED BY IWD1 AND IWD12,
C     AND CONTINUE UNTIL THE LAST NON-BLANK CHARACTER
C     BEFORE THE WORD DEFINED BY IWD2 AND IWD22.
C     (E.G., IF IWD1 = 'DERI' AND IWD12 = 'VATI', THEN THE STRING
C     WILL BEGIN WITH THE FIRST NON-BLANK CHARACTER
C     AFTER 'DERIVATIVE', 'DERIVATIXXX', ETC.).
C     THE STRING WILL FINISH WITH THE LAST NON-BLANK CHARACTER BEFORE IWD2 AND
C     IWD22.  THE SCAN WILL COVER THE ENTIRE LINE.
C     NOTE THE FOLLOWING CONVENTIONS--
C          IF IWD1 = ' ', THE EXTRACTED STRING WILL START WITH THE
C          FIRST WORD OF THE LINE (INCLUSIVE).
C          IF IWD2 = ' ', THE EXTRACTED STRING WILL STOP WITH THE
C          LAST WORD OF THE LINE (INCLUSIVE).
C     NOTE--ONLY THE STRING EXTRACTION IS DONE--
C           NO FUNCTION REPLACEMENT IS DONE.
C     OUTPUT ARGUMENTS--IFUNC2 = THE HOLLERITH VARIABLE
C                                CONTAINING THE EXTRACTED
C                                STRING (1 CHARACTER PER WORD).
C                     --N2     = THE INTEGER NUMBER OF A1 CHARACTERS
C                                IN THE EXTRACTED STRING.
C
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
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --MAY       1982.
C     ORIGINAL VERSION--FEBRUARY  1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --JUNE      1989.  FIX MIS-PARSING OF LET S = ABCFORDEF
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS
      CHARACTER*4 IWD1
      CHARACTER*4 IWD12
      CHARACTER*4 IWD2
      CHARACTER*4 IWD22
      CHARACTER*4 IFUNC2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICH1
      CHARACTER*4 ICH2
      CHARACTER*4 IX1
      CHARACTER*4 IX2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
      DIMENSION IFUNC2(*)
C
      DIMENSION ICH1(8)
      DIMENSION ICH2(8)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='ST  '
C
      NUMASC=4
      NUMAS2=2*NUMASC
C
      IEND1=0
      IEND2=0
      ILOCST=0
      ILOC1=0
      I2=0
      J2=0
      ISTART=0
      ILOC2=0
      ISTOP=0
C
C               ***************************
C               **  EXTRACT A FUNCTION.  **
C               ***************************
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,91)
   91 FORMAT('***** AT THE BEGINNING OF DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,92)IWIDTH
   92 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,93)(IANS(I),I=1,IWIDTH)
   93 FORMAT('IANS(.) = ',115A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,94)IWD1,IWD12
   94 FORMAT('IWD1,IWD12 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,95)IWD2,IWD22
   95 FORMAT('IWD2,IWD22 = ',A4,2X,A4)
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
      IERROR='NO'
      IFOUND='NO'
C
      N2=0
      DO100I=1,MAXCHF
      IFUNC2(I)='    '
  100 CONTINUE
C
      DO110I=1,NUMAS2
      ICH1(I)=' '
      ICH2(I)=' '
  110 CONTINUE
C
C               **************************************************************
C               **  STEP 2--                                                **
C               **  DECOMPOSE THE 2 TARGET WORDS INTO INDIVIDUAL CHARACTERS.**
C               **************************************************************
C
      J=0
      IF(IWD1.EQ.' ')GOTO390
      IX1=IWD1
      ISTAR2=0
      ILEN1=NUMBPC
      ILEN2=ILEN1
      DO200I=1,NUMASC
      J=J+1
      IX2=' '
      ISTAR1=(I-1)*NUMBPC
      CALL DPCHEX(ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2)
      ICH1(J)=IX2
  200 CONTINUE
C
      IF(IWD12.EQ.' ')GOTO290
      IX1=IWD12
      ISTAR2=0
      ILEN1=NUMBPC
      ILEN2=ILEN1
      DO250I=1,NUMASC
      J=J+1
      IX2=' '
      ISTAR1=(I-1)*NUMBPC
      CALL DPCHEX(ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2)
      ICH1(J)=IX2
  250 CONTINUE
  290 CONTINUE
C
      K=0
      DO300I=1,J
      K=K+1
      IF(ICH1(I).EQ.' ')GOTO350
  300 CONTINUE
      IEND1=K
      GOTO390
  350 CONTINUE
      IEND1=K-1
  390 CONTINUE
C
      J=0
      IF(IWD2.EQ.' ')GOTO590
      IX1=IWD2
      ISTAR2=0
      ILEN1=NUMBPC
      ILEN2=ILEN1
      DO400I=1,NUMASC
      J=J+1
      IX2=' '
      ISTAR1=(I-1)*NUMBPC
      CALL DPCHEX(ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2)
      ICH2(J)=IX2
  400 CONTINUE
C
      IF(IWD22.EQ.' ')GOTO490
      IX1=IWD22
      ISTAR2=0
      ILEN1=NUMBPC
      ILEN2=ILEN1
      DO450I=1,NUMASC
      J=J+1
      IX2=' '
      ISTAR1=(I-1)*NUMBPC
      CALL DPCHEX(ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2)
      ICH2(J)=IX2
  450 CONTINUE
  490 CONTINUE
C
      K=0
      DO500I=1,J
      K=K+1
      IF(ICH2(I).EQ.' ')GOTO550
  500 CONTINUE
      IEND2=K
      GOTO590
  550 CONTINUE
      IEND2=K-1
  590 CONTINUE
C
C               ***************************************************************
C               **  STEP 3--                                                 **
C               **  EXTRACT THE                                              **
C               **  EXPRESSION FROM THE INPUT COMMAND LINE.                  **
C               **  START WITH THE FIRST NON-BLANK CHARACTER AFTER THE       **
C               **  WORD (OR THE CONTINUATION OF THE WORD)                   **
C               **  DEFINED IN IWD1 AND IWD12,                               **
C               **  AND END WITH THE FIRST NON-BLANK CHARACTER               **
C               **  BEFORE THE WORD DEFINED IN IWD2 AND IWD22.               **
C               **  NOTE THAT IF IWD1 = ' ', THEN THIS IS TO BE INTERPRETED  **
C               **  AS STARTING WITH THE FIRST NON-BLANK CHARACTER AFTER     **
C               **  (BUT NOT INCLUDING) THE EQUAL SIGN.                      **
C               **  NOTE THAT IF IWD2 = ' ', THEN THIS IS TO BE INTERPRETED  **
C               **  AS ENDING WITH THE FIRST NON-BLANK CHARACTER UP TO       **
C               **  (AND INCLUDING) THE END OF THE LINE.                     **
C               **  THE EXTRACTED FUNCTION WILL BE PUT INTO IFUNC2(.).       **
C               **  THE NUMBER OF CHARACTERS IN IFUNC2(.) WILL BE N2.        **
C               ***************************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IWIDTH.GE.1)GOTO1109
      IBRAN=1100
      WRITE(ICOUT,1101)
 1101 FORMAT('INTERNAL ERROR IN DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1102)IBRAN
 1102 FORMAT('AT BRANCH POINT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1103)
 1103 FORMAT('IWIDTH IS NON-POSITIVE FOR FUNCTION EXTRACTION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1104)IWIDTH
 1104 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1105)
 1105 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,1106)(IANS(I),I=1,IWIDTH)
 1106 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1109 CONTINUE
C
C               *************************************************
C               **  STEP 3.2--                                 **
C               **  SEARCH FOR THE STRING DEFINED BY THE       **
C               **  CHARACTERS IN IWD1 AND IWD12.              **
C               *************************************************
C
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCST=0
      ILOC1=(-99)
      IF(IWD1.EQ.' ')ILOC1=ILOCST
      IF(IWD1.EQ.' ')GOTO3290
      IMIN=ILOCST+1
      IF(IMIN.GT.IWIDTH)GOTO3290
      DO3210I=IMIN,IWIDTH
      I2=I
      IF(IANS(I).EQ.ICH1(1))GOTO3215
      GOTO3210
 3215 CONTINUE
      DO3212J=1,IEND1
      J2=I2+J-1
      IF(IANS(J2).EQ.ICH1(J))GOTO3212
      GOTO3210
 3212 CONTINUE
      ILOC1=J2
      GOTO3290
 3210 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO3259
      WRITE(ICOUT,3251)
 3251 FORMAT('***** BUG-MODE DIAGNOSTIC IN DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3252)IWD1,IWD12
 3252 FORMAT('      NO ',A4,A4,' FOUND AFTER EQUAL SIGN.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3253)
 3253 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3254)(IANS(I),I=1,IWIDTH)
 3254 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
 3259 CONTINUE
      IFOUND='NO'
      GOTO9000
 3290 CONTINUE
C
C               ********************************************
C               **  STEP 3.3--                            **
C               **  DETERMINE IF THERE IS A CONTINUATION  **
C               **  OF THE WORD DEFINED BY IWD1 AND IWD12.**
C               ********************************************
C
      ISTEPN='3.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IWD1.EQ.' ')GOTO3390
      IF(IEND1.NE.NUMAS2)GOTO3390
      IMIN=ILOC1+1
      IF(IMIN.GT.IWIDTH)GOTO3319
      DO3300I=IMIN,IWIDTH
      I2=I
      IF(IANS(I).EQ.' ')GOTO3310
 3300 CONTINUE
      ILOC1=I2+1
      GOTO3319
 3310 CONTINUE
      ILOC1=I2-1
 3319 CONTINUE
C
      IF(ILOC1.GE.1)GOTO3359
      WRITE(ICOUT,3351)
 3351 FORMAT('***** ERROR IN DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3352)IWD1,IWD12
 3352 FORMAT('      NO ',A4,A4,' FOUND AFTER EQUAL SIGN.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3353)
 3353 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3354)(IANS(I),I=1,IWIDTH)
 3354 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3359 CONTINUE
C
      IF(ILOC1.LT.IWIDTH)GOTO3369
      WRITE(ICOUT,3361)
 3361 FORMAT('***** ERROR IN DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3362)IWD1,IWD12
 3362 FORMAT('      ',A4,A4,' IS LAST WORD ON COMMAND LINE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3363)
 3363 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3364)(IANS(I),I=1,IWIDTH)
 3364 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3369 CONTINUE
C
 3390 CONTINUE
C
C               ********************************************
C               **  STEP 3.4--                            **
C               **  SEARCH FOR FIRST NON-BLANK CHARACTER  **
C               **  AFTER THE WORD (OR THE CONTINUATION   **
C               **  OF THE WORD) DEFINED BY IWD1 AND IWD12.**
C               ********************************************
C
      ISTEPN='3.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=(-99)
      IMIN=ILOC1+1
      IF(IMIN.GT.IWIDTH)GOTO3419
      DO3410I=IMIN,IWIDTH
      I2=I
      IF(IANS(I).NE.' ')GOTO3415
 3410 CONTINUE
      GOTO3419
 3415 CONTINUE
      ISTART=I2
 3419 CONTINUE
C
      IF(ISTART.GE.1)GOTO3459
      WRITE(ICOUT,3451)
 3451 FORMAT('***** ERROR IN DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3452)IWD1,IWD12
 3452 FORMAT('      ALL CHARACTERS AFTER ',A4,A4,' ARE BLANK.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3453)
 3453 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3454)(IANS(I),I=1,IWIDTH)
 3454 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3459 CONTINUE
C
C               *************************************************
C               **  STEP 3.5--                                 **
C               **  SEARCH FOR FIRST OCCURRANCE OF CHARACTER   **
C               **  DEFINED BY IWD2.                           **
C               *************************************************
C
      ISTEPN='3.5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOC2=(-99)
      IF(IWD2.EQ.' ')ILOC2=IWIDTH+1
      IF(IWD2.EQ.' ')GOTO3590
C     THE FOLLOWING LINE WAS ENTERED (SEPT 1987)
C     TO HANDLE THE PROBLEM THAT AROSE WHEN THE RIGHT HAND SIDE OF
C     THE EQUAL SIGN CONSISTED OF ONLY 1 CHARACTER, AS IN
C     LET FUNCTION ABC = F
      IF(IWD2.EQ.'FOR'.AND.ISTART.EQ.IWIDTH)GOTO3511
      IMIN=ISTART+1
      IF(IMIN.GT.IWIDTH)GOTO3590
      DO3510I=IMIN,IWIDTH
      I2=I
      IF(IANS(I).EQ.ICH2(1))GOTO3515
      GOTO3510
 3515 CONTINUE
      DO3512J=1,IEND2
      J2=I2+J-1
      IF(IANS(J2).EQ.ICH2(J))GOTO3512
      GOTO3510
 3512 CONTINUE
      ILOC2=I2-1
      GOTO3590
 3510 CONTINUE
 3511 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO3559
      WRITE(ICOUT,3551)
 3551 FORMAT('***** BUG-MODE DIAGNOSTIC IN DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3552)IWD2,IWD22
 3552 FORMAT('      NO ',A4,A4,' FOUND AFTER EQUAL SIGN.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3553)
 3553 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3554)(IANS(I),I=1,IWIDTH)
 3554 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
 3559 CONTINUE
      IFOUND='NO'
      GOTO9000
 3590 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED                         JUNE 1989
CCCCC TO CORRECT MIS-PARSING OF    LET STRING S = ABCFORDEF   JUNE 1989
      IF(IWD2.EQ.'FOR'.AND.ILOC2.GE.1)GOTO3591
      GOTO3599
 3591 CONTINUE
      DO3592I=ILOC2,IWIDTH
      IF(IANS(I).EQ.'=')GOTO3599
 3592 CONTINUE
      IFOUND='NO'
      GOTO9000
 3599 CONTINUE
C
C               ********************************************
C               **  STEP 3.6--                            **
C               **  SEARCH FOR FIRST NON-BLANK CHARACTER  **
C               **  BEFORE THE WORD DEFINED BY IWD2.      **
C               ********************************************
C
      ISTEPN='3.6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTOP=(-99)
      IMAX=ILOC2-1
      IF(IMAX.LT.ISTART)GOTO3619
      DO3610I=ISTART,IMAX
      IREV=IMAX-I+ISTART
      IF(IANS(IREV).NE.' ')GOTO3615
 3610 CONTINUE
      GOTO3619
 3615 CONTINUE
      ISTOP=IREV
 3619 CONTINUE
C
      IF(ISTOP.GE.1)GOTO3659
      WRITE(ICOUT,3651)
 3651 FORMAT('***** ERROR IN DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3652)IWD2,IWD22
 3652 FORMAT('      ALL CHARACTERS BEFORE ',A4,A4,' ARE BLANK.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3653)
 3653 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3654)(IANS(I),I=1,IWIDTH)
 3654 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3659 CONTINUE
C
      IF(ISTART.LE.ISTOP)GOTO3919
      IBRAN=3910
      WRITE(ICOUT,3911)
 3911 FORMAT('INTERNAL ERROR IN DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3912)IBRAN
 3912 FORMAT('AT BRANCH POINT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3913)
 3913 FORMAT('ISTART GREATER THAN ISTOP FOR FUNCTION EXTRACTION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3914)ISTART,ISTOP
 3914 FORMAT('ISTART, ISTOP = ',2I8)
      CALL DPWRST('XXX','BUG ')
       WRITE(ICOUT,3915)
 3915 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3916)(IANS(I),I=1,IWIDTH)
 3916 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 3919 CONTINUE
C
C               **********************************
C               **  STEP 4--                    **
C               **  COPY OUT THE STRING AS IS.  **
C               **  COPY IT INTO IFUNC2(.).     **
C               **********************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO4000I=ISTART,ISTOP
      J=J+1
      IFUNC2(J)=IANS(I)
 4000 CONTINUE
      N2=J
C
      IFOUND='YES'
C
C               ****************
C               **  STEP 5--  **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPEXST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IEND1,IEND2
 9012 FORMAT('IEND1,IEND2 = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)(ICH1(I),I=1,8)
 9013 FORMAT('ICH1(I),I=1,8)--',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)(ICH2(I),I=1,8)
 9014 FORMAT('ICH2(I),I=1,8)--',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)ILOCST,ILOC1,ISTART,ISTOP,ILOC2
 9015 FORMAT('ILOCST,ILOC1,ISTART,ISTOP,ILOC2 = ',5I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IFOUND,IERROR
 9016 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)N2
 9017 FORMAT('N2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)(IFUNC2(I),I=1,N2)
 9018 FORMAT('IFUNC2(.) = ',115A1)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPEXTE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--EXTEND A VARIABLE X BY APPENDING VARIABLE Y
C              TO THE END OF X.
C     EXAMPLE--EXTEND X Y
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
C     ORIGINAL VERSION (IN DPLET)--APRIL     1984.
C     UPDATED                    --JUNE      1990.  ADD ISUBRO ARGUMENT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
CCCCC FOLLOWING LINE ADDED JUNE, 1990.
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IVAR11
      CHARACTER*4 IVAR12
      CHARACTER*4 IVAR21
      CHARACTER*4 IVAR22
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='TE  '
      IFOUND='YES'
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      I2=0
      N1=0
      N2=0
      ICOL1=0
      ICOL2=0
C
      IVAR11='UNKN'
      IVAR12='UNKN'
      IVAR21='UNKN'
      IVAR22='UNKN'
      ILIST1=(-999)
      ILIST2=(-999)
      N1PN2=(-999)
      N1PI=(-999)
      IJ1=(-999)
      IJ2=(-999)
      N1NEW=(-999)
      IROW1=(-999)
      IROWN=(-99)
C
C               **********************************************
C               **  TREAT THE CASE OF EXTENDING A VARIABLE  **
C               **  WITH THE CONTENTS OF ANOTHER VARIABLE.  **
C               **********************************************
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,IBUGQ,ISUBRO
   52   FORMAT('IBUGS2,IBUGQ,ISUBRO = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTE')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=2
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ********************************************************
C               **  STEP 3--                                          **
C               **  EXAMINE THE FIRST  VARIABLE.                      **
C               **  IS IT IN THE TABLE?                               **
C               **  IS IT A VARIABLE?                                 **
C               **  IVAR11 AND IVAR12 = THE NAME OF THE FIRST VARIABLE.*
C               **  ILIST1 = THE LINE IN THE INTERNAL TABLE           **
C               **           WHERE THE FIRST  VARIABLE IS FOUND.      **
C               **  ICOL1  = THE DATA COLUMN FOR THE FIRST  VARIABLE. **
C               **  N1     = THE NUMBER OF OBSERVATIONS FOR THE FIRST **
C               **           VARIABLE.                                **
C               ********************************************************
C
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTE')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IVAR11=IHARG(1)
      IVAR12=IHARG2(1)
C
      DO310I=1,NUMNAM
      I2=I
      IF(IVAR11.EQ.IHNAME(I).AND.IVAR12.EQ.IHNAM2(I).AND.
     1IUSE(I).EQ.'V')GOTO380
      IF(IVAR11.EQ.IHNAME(I).AND.IVAR12.EQ.IHNAM2(I).AND.
     1IUSE(I).NE.'V')GOTO330
  310 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,321)
  321 FORMAT('***** ERROR IN DPEXTE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,322)
  322 FORMAT('      THE FIRST  VARIABLE NAME REFERENCED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,323)IVAR11,IVAR12
  323 FORMAT('      (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,324)
  324 FORMAT('      WAS NOT FOUND IN THE INTERNAL NAME TABLE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,325)
  325 FORMAT('      SUGGESTED ACTION--USE THE STATUS COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,326)
  326 FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  330 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,331)
  331 FORMAT('***** ERROR IN DPEXTE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,332)
  332 FORMAT('      THE FIRST  VARIABLE NAME REFERENCED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,333)IVAR11,IVAR12
  333 FORMAT('      (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,334)
  334 FORMAT('      SHOULD HAVE BEEN A VARIABLE, BUT WAS NOT.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  380 CONTINUE
      ILIST1=I2
      ICOL1=IVALUE(ILIST1)
      N1=IN(ILIST1)
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  EXAMINE THE SECOND VARIABLE.                      **
C               **  IS IT IN THE TABLE?                               **
C               **  IS IT A VARIABLE?                                 **
C               **  IVAR21 AND IVAR22 = THE NAME OF THE SECOND VARIABLE.
C               **  ILIST2 = THE LINE IN THE INTERNAL TABLE           **
C               **           WHERE THE SECOND VARIABLE IS FOUND.      **
C               **  ICOL2  = THE DATA COLUMN FOR THE SECOND VARIABLE. **
C               **  N2     = THE NUMBER OF OBSERVATIONS FOR THE       **
C               **           SECOND VARIABLE.                         **
C               ********************************************************
C
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTE')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IVAR21=IHARG(2)
      IVAR22=IHARG2(2)
C
      DO410I=1,NUMNAM
      I2=I
      IF(IVAR21.EQ.IHNAME(I).AND.IVAR22.EQ.IHNAM2(I).AND.
     1IUSE(I).EQ.'V')GOTO480
      IF(IVAR21.EQ.IHNAME(I).AND.IVAR22.EQ.IHNAM2(I).AND.
     1IUSE(I).NE.'V')GOTO430
  410 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,421)
  421 FORMAT('***** ERROR IN DPEXTE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,422)
  422 FORMAT('      THE SECOND VARIABLE NAME REFERENCED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,423)IVAR21,IVAR22
  423 FORMAT('      (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,424)
  424 FORMAT('      WAS NOT FOUND IN THE INTERNAL NAME TABLE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,425)
  425 FORMAT('      SUGGESTED ACTION--USE THE STATUS COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,426)
  426 FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  430 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,431)
  431 FORMAT('***** ERROR IN DPEXTE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,432)
  432 FORMAT('      THE SECOND VARIABLE NAME REFERENCED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,433)IVAR21,IVAR22
  433 FORMAT('      (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,434)
  434 FORMAT('      SHOULD HAVE BEEN A VARIABLE, BUT WAS NOT.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  480 CONTINUE
      ILIST2=I2
      ICOL2=IVALUE(ILIST2)
      N2=IN(ILIST2)
C
      ISTEPN='6'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTE')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ***********************************************
C               **  STEP 6--                                 **
C               **  DO A PRELIMINARY CHECK--                 **
C               **  WILL APPENDING VARIABLE 2 TO VARIABLE 1  **
C               **  MAKE VARIABLE 1 TOO LONG?                **
C               **  (THAT IS, WILL IT EXCEED MAXN)?          **
C               ***********************************************
C
      N1PN2=N1+N2
      IF(N1PN2.LE.MAXN)GOTO690
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,621)
  621 FORMAT('***** ERROR IN DPEXTE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,622)IVAR11,IVAR12
  622 FORMAT('      THE EXTENSION OF VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,623)IVAR21,IVAR22
  623 FORMAT('      BY VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,624)IVAR11,IVAR12
  624 FORMAT('      WILL MAKE VARIABLE ',A4,A4,' TOO LONG.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,625)IVAR11,IVAR12,N1
  625 FORMAT('      NUMBER OF OBSERVATIONS IN ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,626)IVAR21,IVAR22,N2
  626 FORMAT('      NUMBER OF OBSERVATIONS IN ',A4,A4,' = ' ,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,627)IVAR11,IVAR12,N1PN2
  627 FORMAT('      NEW NUMBER OF OBSERVATIONS IN ',A4,A4,
     1' WOULD = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,628)MAXN
  628 FORMAT('      ALLOWABLE NUMBER OF OBSERVATIONS    = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,629)
  629 FORMAT('      THEREFORE, NO EXTENSION CARRIED OUT.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  690 CONTINUE
C
C               ****************************************************
C               **  STEP 10--                                     **
C               **  APPEND VARIABLE 2 ONTO THE END OF VARIABLE 1  **
C               ****************************************************
C
      ISTEPN='10'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTE')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2100I=1,N2
      N1PI=N1+I
      IJ1=MAXN*(ICOL1-1)+N1PI
      IJ2=MAXN*(ICOL2-1)+I
      IF(ICOL1.LE.MAXCOL)V(IJ1)=V(IJ2)
      IF(ICOL1.EQ.MAXCP1)PRED(N1PI)=Y(IJ2)
      IF(ICOL1.EQ.MAXCP2)RES(N1PI)=Y(IJ2)
 2100 CONTINUE
      N1NEW=N1PI
C
C               *******************************************
C               **  STEP 11--                            **
C               **  CARRY OUT THE LIST UPDATING AND      **
C               **  GENERATE THE INFORMATIVE PRINTING.   **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTE')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHNAME(ILIST1)=IVAR11
      IHNAM2(ILIST1)=IVAR12
      IUSE(ILIST1)='V'
      IVALUE(ILIST1)=ICOL1
      VALUE(ILIST1)=ICOL1
      IN(ILIST1)=N1NEW
C
      DO2400J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOL1)GOTO2405
      GOTO2400
 2405 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOL1
      VALUE(J4)=ICOL1
      IN(J4)=N1NEW
 2400 CONTINUE
C
      IF(IPRINT.EQ.'OFF')GOTO2459
      IF(IFEEDB.EQ.'OFF')GOTO2459
      NNUM=N2
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2411)IVAR11,IVAR12,NNUM
 2411 FORMAT('THE NUMBER OF VALUES ADDED TO ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
C
      IROW1=N1+1
      IROWN=N1+N2
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IJ=MAXN*(ICOL1-1)+IROW1
      IF(ICOL1.LE.MAXCOL)WRITE(ICOUT,2421)IVAR11,IVAR12,V(IJ),
     1IROW1
      IF(ICOL1.LE.MAXCOL)CALL DPWRST('XXX','BUG ')
      IF(ICOL1.EQ.MAXCP1)WRITE(ICOUT,2421)IVAR11,IVAR12,PRED(IROW1),
     1IROW1
      IF(ICOL1.EQ.MAXCP1)CALL DPWRST('XXX','BUG ')
      IF(ICOL1.EQ.MAXCP2)WRITE(ICOUT,2421)IVAR11,IVAR12,RES(IROW1),
     1IROW1
 2421 FORMAT('THE FIRST           VALUE ADDED TO ',A4,A4,
     1' = ',E15.7,'   (ROW ',I6,')')
      IF(ICOL1.EQ.MAXCP2)CALL DPWRST('XXX','BUG ')
      IJ=MAXN*(ICOL1-1)+IROWN
      IF(ICOL1.LE.MAXCOL.AND.
     1NNUM.NE.1)WRITE(ICOUT,2431)NNUM,IVAR11,IVAR12,V(IJ),IROWN
      IF(ICOL1.LE.MAXCOL.AND.
     1NNUM.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOL1.EQ.MAXCP1.AND.
     1NNUM.NE.1)WRITE(ICOUT,2431)NNUM,IVAR11,IVAR12,PRED(IROWN),IROWN
      IF(ICOL1.EQ.MAXCP1.AND.
     1NNUM.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOL1.EQ.MAXCP2.AND.
     1NNUM.NE.1)WRITE(ICOUT,2431)NNUM,IVAR11,IVAR12,RES(IROWN),IROWN
 2431 FORMAT('THE LAST (',I5,'-TH) VALUE ADDED TO ',A4,A4,
     1' = ',E15.7,'   (ROW ',I6,')')
      IF(ICOL1.EQ.MAXCP2.AND.
     1NNUM.NE.1)CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2453)IVAR11,IVAR12,N1NEW
 2453 FORMAT('THE NEW     LENGTH OF  ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 2459 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IVAR11,IVAR12,ILIST1,ICOL1,N1
 9021   FORMAT('IVAR11,IVAR12,ILIST1,ICOL1,N1 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IVAR22,IVAR22,ILIST2,ICOL2,N2
 9022   FORMAT('IVAR22,IVAR22,ILIST2,ICOL2,N2 = ',A4,2X,A4,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)N1PI,N1PN2,N1NEW,IROW1,IROWN,IJ1,IJ2
 9023   FORMAT('N1PI,N1PN2,N1NEW,IROW1,IROWN,IJ1,IJ2 = ',6I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXTL(IHRE11,IHRE12,IHRE21,IHRE22,
     1                  KNUMB,IVAL1,IVAL2,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--EXTRACT "TO" LIMITS.
C              DO A CHARACTER-BY-CHARACTER COMPARISON
C              OF IHRE11/IHRE12 AND IHRE21/IHRE22,
C              AND NOTE (THIS WILL BECOME KNUMB)
C              WHERE THE TRAILING NUMBERS BEGIN.
C              THEN EXTRACT THE 2 TRAILING NUMERIC STRINGS
C              AND CONVERT
C              THESE 2 NUMBERS INTO INTEGERS (IVAL1 AND IVAL2)
C
C     ORIGINAL VERSION--DECEMBER   1986.
C     UPDATED         --JULY       2009. DO NOT TREAT "Y1 TO Y1" AS
C                                        AN ERROR.  THIS CONSTRUCT
C                                        MAY BE HELPFUL IN MACROS WHERE
C                                        THE NUMBER OF VARIABLES MAY
C                                        NOT BE KNOWN A PRIORI.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHRE11
      CHARACTER*4 IHRE12
      CHARACTER*4 IHRE21
      CHARACTER*4 IHRE22
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IVALI3
      CHARACTER*4 IVALI4
      CHARACTER*8 IS1
      CHARACTER*8 IS2
      CHARACTER*4 IS3
      CHARACTER*4 IS4
C
      CHARACTER*1 IC
C
      DIMENSION IS3(8)
      DIMENSION IS4(8)
C
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      KDIFF=(-999)
      KNUMB=(-999)
      IVAL1=(-999)
      IVAL2=(-999)
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXTL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXTL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,IERROR
   52   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IHRE11,IHRE12,IHRE21,IHRE22
   53   FORMAT('IHRE11,IHRE12,IHRE21,IHRE22 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************************
C               **  STEP 11--                                     **
C               **  FORM IS1 WHICH WILL BE A CHARACTER*8          **
C               **  COMBINATION OF IHRE11 AND IHRE12.             **
C               **  COPY IHRE11 INTO THE FIRST  HALF OF IS1.      **
C               **  COPY IHRE12 INTO THE SECOND HALF OF IS1.      **
C               **  FORM IS2 WHICH WILL BE A CHARACTER*8          **
C               **  COMBINATION OF IHRE21 AND IHRE22.             **
C               **  COPY IHRE21 INTO THE FIRST  HALF OF IS2.      **
C               **  COPY IHRE22 INTO THE SECOND HALF OF IS2.      **
C               **  FORM IS3 WHICH WILL BE A 8-TERM VECTOR        **
C               **  VERSION OF IS1.                               **
C               **  FORM IS4 WHICH WILL BE A 8-TERM VECTOR        **
C               **  VERSION OF IS2.                               **
C               ****************************************************
C
      IS1(1:8)='        '
      IS2(1:8)='        '
      IS1(1:4)=IHRE11
      IS1(5:8)=IHRE12
      IS2(1:4)=IHRE21
      IS2(5:8)=IHRE22
C
      DO 1100 K=1,8
        IS3(K)='    '
        IS4(K)='    '
        IS3(K)=IS1(K:K)
        IS4(K)=IS2(K:K)
 1100 CONTINUE
C
C               ****************************************************
C               **  STEP 12--                                     **
C               **  FORM IS3 WHICH WILL BE A 8-TERM VECTOR        **
C               **  DETERMINE THE LENGTH OF THE NON-BLANK         **
C               **  PART OF IS3.                                  **
C               **  DETERMINE THE LENGTH OF THE NON-BLANK         **
C               **  PART OF IS4.                                  **
C               ****************************************************
C
      DO1210K=1,8
        KREV=8-K+1
        IF(IS3(KREV).NE.'    ')GOTO1219
 1210 CONTINUE
      KREV=0
 1219 CONTINUE
      NS3=KREV
C
      DO1220K=1,8
        KREV=8-K+1
        IF(IS4(KREV).NE.'    ')GOTO1229
 1220 CONTINUE
      KREV=0
 1229 CONTINUE
      NS4=KREV
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXTL')THEN
        WRITE(ICOUT,1231)NS3,N
 1231   FORMAT('AT 1229: NS3,NS4 = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 13--                            **
C               **  DETERMINE THE POSITION (1 TO 8)      **
C               **  WHEREBY IS1 AND IS2                  **
C               **  (OR EQUIVALENTLY IS3 AND IS4)        **
C               **  FIRST DIFFER.                        **
C               *******************************************
C
C
C     JULY 2009: DO NOT TREAT "Y1 TO Y1" AS AN ERROR AS THIS
C                CONSTRUCT MAY BE USEFUL IN MACROS WHERE THE
C                NUMBER OF VARIABLES MAY NOT BE KNOWN.
C
C     MARCH 2020: IF NO DIFFERENCE FOUND, SET KDIFF TO LAST
C                 NON-BLANK CHARACTER
C
      DO1300K=1,8
        KDIFF=K
        IF(IS3(K).NE.IS4(K))GOTO1390
 1300 CONTINUE
C
      DO1310K=8,1,-1
        IF(IS3(K).NE.' ')THEN
          KDIFF=K
          GOTO1319
        ENDIF
 1310 CONTINUE
 1319 CONTINUE
C
CCCCC IERROR='YES'
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1311)
C1311 FORMAT('***** ERROR IN DPEXTL--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1312)
C1312 FORMAT('      NO DIFFERENCE FOUND IN ')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1313)
C1313 FORMAT('      THE 2 REFERENCE STRINGS')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1314)
C1314 FORMAT('      IN ATTEMPTING TO EXTRACT')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1315)
C1315 FORMAT('      LIMITS IN CONNECTION WITH THE   TO   KEYWORD.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1316)IS1
C1316 FORMAT('            IS1 = ',A8)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1317)IS2
C1317 FORMAT('            IS2 = ',A8)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC GOTO9000
C
 1390 CONTINUE
C
C               *************************************************
C               **  STEP 14--                                  **
C               **  STEP BACK TO SEE IF PREVIOUS CHARACTERS    **
C               **  ARE DIGITS                                 **
C               *************************************************
C
      KNUMB=KDIFF
      KDIFM1=KDIFF-1
      IF(KDIFM1.LE.0)GOTO1490
      DO1400K=1,KDIFM1
        KREV=KDIFM1-K+1
        IC=IS1(KREV:KREV)
        CALL DPCOAN(IC,IX)
        IF(IX.LE.47)GOTO1490
        IF(IX.GE.58)GOTO1490
        KNUMB=KREV
 1400 CONTINUE
 1490 CONTINUE
      K31=KNUMB
      K32=NS3
      K41=KNUMB
      K42=NS4
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXTL')THEN
        WRITE(ICOUT,1491)K31,K32,K41,K42
 1491   FORMAT('AT 1490: K31,K32,K41,K42 = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 15--                                  **
C               **  EXTRACT THE TRAILING DIFFERERING STRING    **
C               **  FOR IS1 AND CONVERT IT TO AN INTEGER.      **
C               **  EXTRACT THE TRAILING DIFFERERING STRING    **
C               **  FOR IS2 AND CONVERT IT TO AN INTEGER.      **
C               *************************************************
C
      CALL DPCOHI(K31,K32,IS3,NS3,IVALI3,VALCO3,IVALC3,
     1IBUGS2,IERROR)
      CALL DPCOHI(K41,K42,IS4,NS4,IVALI4,VALCO4,IVALC4,
     1IBUGS2,IERROR)
C
      IVAL1=IVALC3
      IVAL2=IVALC4
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'EXTL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXTL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGS2,ISUBRO,IERROR
 9012   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IHRE11,IHRE12,IHRE21,IHRE22
 9013   FORMAT('IHRE11,IHRE12,IHRE21,IHRE22 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)KDIFF,KNUMB,IVAL1,IVAL2
 9014   FORMAT('KDIFF,KNUMB,IVAL1,IVAL2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IS1,NS3,K31,K32
 9015   FORMAT('IS1,NS3,K31,K32 = ',A8,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IVALI3,VALCO3,IVALC3,IVAL1
 9016   FORMAT('IVALI3,VALCO3,IVALC3,IVAL1 = ',A4,E15.7,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IS2,NS4,K41,K42
 9017   FORMAT('IS2,NS4,K41,K42 = ',A8,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IVALI4,VALCO4,IVALC4,IVAL2
 9018   FORMAT('IVALI4,VALCO4,IVALC4,IVAL2 = ',A4,E15.7,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXTP(X,Y,N,
     1                  X2,Y2,NOUT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--GIVEN A SET OF (X,Y) PAIRS, RETURN THE 4 "EXTREME" POINTS:
C
C                  (X1,YMIN)
C                  (XMAX,Y2
C                  (X3,YMAX)
C                  (XMIN,Y4)
C
C     INPUT  ARGUMENTS--X      = A REAL VECTOR CONTAINING THE X
C                                COORDINATES OF THE POINTS
C                     --Y      = A REAL VECTOR CONTAINING THE Y
C                                COORDINATES OF THE POINTS
C                     --N      = NUMBER OF POINTS IN X, Y
C     OUTPUT ARGUMENTS--X2     = A REAL VECTOR CONTAINING THE X
C                                COORDINATES OF THE EXTREME POINTS
C                     --Y2     = A REAL VECTOR CONTAINING THE Y
C                                COORDINATES OF THE EXTREME POINTS
C                     --NOUT   = NUMBER OF POINTS IN X2, Y2
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
      INTEGER N
      INTEGER NOUT
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA PI / 3.1415926535 8979323846 2643383279 503 D0 /
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EXTP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXTP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
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
      INDX1=1
      INDX2=1
      INDX3=1
      INDX4=1
      XMIN=X(1)
      XMAX=X(1)
      YMIN=Y(1)
      YMAX=Y(1)
      DO100IROW=2,N
C
        IF(Y(IROW).LT.YMIN)THEN
          INDX1=IROW
          YMIN=Y(IROW)
        ENDIF
C
        IF(X(IROW).GT.XMAX)THEN
          INDX2=IROW
          XMAX=X(IROW)
        ENDIF
C
        IF(Y(IROW).GT.YMAX)THEN
          INDX3=IROW
          YMAX=Y(IROW)
        ENDIF
C
        IF(X(IROW).LT.XMIN)THEN
          INDX4=IROW
          XMIN=X(IROW)
        ENDIF
C
  100 CONTINUE
C
      Y2(1)=Y(INDX1)
      X2(1)=X(INDX1)
      Y2(2)=Y(INDX2)
      X2(2)=X(INDX2)
      Y2(3)=Y(INDX3)
      X2(3)=X(INDX3)
      Y2(4)=Y(INDX4)
      X2(4)=X(INDX4)
      NOUT=4
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'EXTP')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DPEXTP--')
        CALL DPWRST('XXX','BUG ')
        DO9055I=1,NOUT
          WRITE(ICOUT,9056)I,X2(I),Y2(I)
 9056     FORMAT('I,X2(I),Y2(I) = ',I8,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9055   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1                  ICOL1,ICOL2,ISTRI2,NCSTR2,
     1                  IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--SCAN THE CHARACTER*80 VARIABLE ISTRIN BETWEEN COLUMNS
C              ISTART TO ISTOP AND EXTRACT THE IWORD-TH WORD IN THAT
C              INTERVAL.  PLACE THE FIRST AND LAST COLUMNS OF THE
C              IWORD-TH WORD INTO ICOL1 AND ICOL2, PLACE THE IWORD-TH
C              WORD ITSELF INTO THE CHARACTER*80 VARIABLE ISTRI2; PLACE
C              THE NUMBER OF CHARACTERS IN THIS IWORD-TH WORD INTO
C              NCSTR2.  THE CHARACTER*80 STRING ISTRI2, AND PLACE THE
C              LENGTH OF THE STRING INTO NCSTR2.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--DECEMBER  1985.
C     UPDATED         --APRIL     1997. DO NOT RESTRICT TO 80 CHARACTERS
C     UPDATED         --JULY      2002. ALLOW WORD TO BE ENCLOSED IN
C                                       QUOTES (PC FILE NAMES CAN
C                                       HAVE SPACES)
C     UPDATED         --JANUARY   2017. IHYPSW TO SPECIFY WHETHER
C                                       LEADING "-" WILL BE TREATED AS
C                                       A WORD SEPARATOR OR NOT.
C     UPDATED         --APRIL     2018. ICOMCL TO SPECIFY WHETHER COMMA
C                                       WILL BE A WORD SEPARATOR
C     UPDATED         --APRIL     2018. IEQUCL TO SPECIFY WHETHER "="
C                                       WILL BE A WORD SEPARATOR
C     UPDATED         --MAY       2018. IF HAVE SOMETHING LIKE
C                                          CHAR="blank")
C                                       THEN TREAT THE CLOSING
C                                       PARENTHESIS AS PART OF THE WORD
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC USE DUMMY DIMENSIONING TO REMOVE ARTIFICIAL RESTRICTION ON
CCCCC 80 CHARACTERS.  APRIL 1997.
CCCCC CHARACTER*80 ISTRIN
CCCCC CHARACTER*80 ISTRI2
      CHARACTER (LEN=*) :: ISTRIN
      CHARACTER (LEN=*) :: ISTRI2
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='WO  '
      IERROR='NO'
C
      IQUOTE=0
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXWO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXWO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR,IFILQU
   53   FORMAT('IBUGS2,ISUBRO,IERROR,IFILQU = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ISTRIN(J:J),J=1,MIN(100,ISTOP))
   54   FORMAT('(ISTRIN(J:J),J=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ISTART,ISTOP,IWORD
   55   FORMAT('ISTART,ISTOP,IWORD = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************
C               **  STEP 11--                       **
C               **  INITIALIZE THE OUTPUT VARIABLES **
C               **************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXWO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL1=(-999)
      ICOL2=(-999)
      NCSTR2=(-999)
CCCCC FOLLOWING CHANGED APRIL 1997
CCCCC 2012/10: DON'T DO THE LOOP INITIALIZATION SINCE THE WORD BEING
CCCCC          EXTRACTED MAY BE OF SMALLER LENGTH THAN ISTOP.  A BETTER
CCCCC          FIX WOULD BE TO ADD A PARAMETER SPECIFYING THE MAXIMUM
CCCCC          SIZE FOR THE OUTPUT STRING.
C
      ISTRI2=' '
CCCCC DO1100I=1,80
CCCCC DO1100I=1,ISTOP
CCCCC   ISTRI2(I:I)=' '
C1100 CONTINUE
C
C               *******************************************
C               **  STEP 12--                            **
C               **  CHECK THE INPUT ARGUMENTS            **
C               *******************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXWO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC NO CHECK ON UPPER LIMIT.  APRIL 1997.
CCCCC IF(ISTART.GE.1.AND.ISTOP.GE.1.AND.
CCCCC1   ISTART.LE.80.AND.ISTOP.LE.80)GOTO1219
      IF(ISTART.LT.1.OR.ISTOP.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPEXWO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      ISTART OR ISTOP IS < 1. ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1213)ISTART
 1213   FORMAT('      ISTART  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)ISTOP
 1214   FORMAT('      ISTOP   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,MIN(100,ISTOP))
 1216   FORMAT('      (ISTRIN(I:I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
C
      ELSEIF(ISTART.GT.ISTOP)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1222)
 1222   FORMAT('      ISTART EXCEEDS ISTOP')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1213)ISTART
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)ISTOP
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,MIN(100,ISTOP))
        CALL DPWRST('XXX','BUG ')
        GOTO9000
C
      ELSEIF(IWORD.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1232)
 1232   FORMAT('      IWORD IS LESS THAN 1 .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1233)IWORD
 1233   FORMAT('      IWORD = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,MIN(ISTOP,100))
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               ************************************************
C               **  STEP 21--                                 **
C               **  IDENTIFY THE COLUMNS WHERE                **
C               **  THE IWORD-TH STRING RESIDES               **
C               **  ICOL1 = START COLUMN OF A STRING          **
C               **  ICOL2 = STOP  COLUMN OF A STRING          **
C               ************************************************
C
C     FIND LAST NON-BLANK CHARACTER AND RESET VALUE OF ISTOP
C
      DO2010I=ISTOP,1,-1
        IF(ISTRIN(I:I).NE.' ')THEN
          ISTOP2=I
          GOTO2019
        ENDIF
 2010 CONTINUE
      ISTOP2=ISTOP
 2019 CONTINUE
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXWO')THEN
        WRITE(ICOUT,2021)ISTOP2
 2021   FORMAT('AFTER 2019: ISTOP2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1232)
      ENDIF
C
      ICOL2=ISTART-1
      DO2100ILOOP=1,IWORD
C
        ICOL1=ISTOP2+1
        IMIN=ICOL2+1
        IF(IMIN.GT.ISTOP2)GOTO2119
        IQUOTE=0
        DO2110I=IMIN,ISTOP2
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXWO')THEN
            WRITE(ICOUT,2117)ILOOP,IMIN,ISTOP2,I,ICOL1,ICOL2,ISTRIN(I:I)
 2117       FORMAT('2110: ILOOP,IMIN,ISTOP2,I,ICOL1,ICOL2,',
     1             'ISTRIN(I:I) = ',6I6,2X,A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
C         CHECK FOR FIRST NON-BLANK CHARACTER AND THEN CHECK
C         IF IT IS QUOTE CHARACTER.
C
          I2=I
          IF(IHYPSW.EQ.'OFF')THEN
            IF(ISTRIN(I:I).NE.' ')THEN
              IF((ICOMCL.EQ.'ON' .AND. ISTRIN(I:I).NE.',') .OR.
     1            ICOMCL.EQ.'OFF')THEN
                IF(IFILQU.EQ.'ON' .AND. ISTRIN(I:I).EQ.'"')IQUOTE=1
                ICOL1=I2
                GOTO2119
              ENDIF
            ENDIF
          ELSE
            IF(ISTRIN(I:I).NE.' ' .AND. ISTRIN(I:I).NE.'-')THEN
              IF((ICOMCL.EQ.'ON' .AND. ISTRIN(I:I).NE.',') .OR.
     1            ICOMCL.EQ.'OFF')THEN
                IF(IFILQU.EQ.'ON' .AND. ISTRIN(I:I).EQ.'"')IQUOTE=1
                ICOL1=I2
                GOTO2119
              ENDIF
            ENDIF
          ENDIF
C
 2110   CONTINUE
C
C       2014/10: NO NON-BLANK CHARACTER FOUND.  SET TO NULL STRING
C       AND RETURN.
C
        ICOL1=ISTOP
        ICOL2=ISTOP
        NCSTR2=1
        GOTO9000
C
 2119   CONTINUE
C
C       2018/04: CHECK FOR QUOTE PRECEEDED BY AN EQUAL SIGN
C
C                ALSO CHECK FOR "...") (IN THIS CASE, TREAT
C                TRAILING PARENTHESIS AS PART OF THE WORD.
C
        ICOL2=ISTOP2
        IMIN=ICOL1+1
        IF(IMIN.GT.ISTOP2)GOTO2129
        DO2120I=IMIN,ISTOP2
          I2=I
          IF(IQUOTE.EQ.0)THEN
            IF(ISTRIN(I:I).EQ.'"' .AND. ISTRIN(I-1:I-1).EQ.'=')THEN
              IQUOTE=1
              GOTO2120
            ELSEIF(ISTRIN(I:I).EQ.' ' .OR. ISTRIN(I:I).EQ.'-' .OR.
     1        (ICOMCL.EQ.'ON' .AND. ISTRIN(I:I).EQ.','))THEN
              ICOL2=I2-1
              GOTO2129
            ENDIF
          ELSE
            IF(ISTRIN(I:I).EQ.'"')THEN
              ICOL2=I2
              IF(I2.LT.ISTOP2 .AND. ISTRIN(I+1:I+1).EQ.')')ICOL2=I2+1
              GOTO2129
            ENDIF
          ENDIF
 2120   CONTINUE
        ICOL2=ISTOP2
 2129   CONTINUE
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXWO')THEN
          WRITE(ICOUT,2125)ILOOP,ICOL1,ICOL2
 2125     FORMAT('AT 2129: ILOOP,ICOL1,ICOL2 = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ICOL1.GE.ISTART.AND.ICOL2.GE.ISTART.AND.
     1     ICOL1.LE.ISTOP.AND.ICOL2.LE.ISTOP)GOTO2139
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2132)
 2132     FORMAT('      ICOL1 OR ICOL2 IS < ISTART OR > ISTOP. ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2133)ICOL1
 2133     FORMAT('      ICOL1  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2134)ICOL2
 2134     FORMAT('      ICOL2  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2135)ISTART
 2135     FORMAT('      ISTART = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2136)ISTOP
 2136     FORMAT('      ISTOP  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2137)(ISTRIN(I:I),I=1,80)
 2137     FORMAT('      (ISTRIN(I:I),I=1,80) = ',80A1)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
 2139   CONTINUE
C
        IF(ICOL1.GT.ICOL2)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2142)
 2142     FORMAT('      ICOL1 EXCEEDS ICOL2')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2133)ICOL1
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2134)ICOL2
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,MIN(80,ISTOP))
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
 2100 CONTINUE
C
C               *********************************************
C               **  STEP 22--                              **
C               **  COPY THE IWORD-TH STRING INTO ISTRI2   **
C               *********************************************
C
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXWO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO2200I=ICOL1,ICOL2
        J=J+1
        ISTRI2(J:J)=ISTRIN(I:I)
 2200 CONTINUE
      NCSTR2=J
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXWO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXWO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IBUGS2,ISUBRO,IERROR,NCSTR2
 9013   FORMAT('IBUGS2,ISUBRO,IERROR,NCSTR2 = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ISTRIN(J:J),J=1,MIN(100,ISTOP))
 9014   FORMAT('(ISTRIN(J:J),J=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ISTART,ISTOP,IWORD,ICOL1,ICOL2
 9015   FORMAT('ISTART,ISTOP,IWORD,ICOL1,ICOL2 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR2.GE.1)THEN
          WRITE(ICOUT,9023)(ISTRI2(I:I),I=1,MIN(100,NCSTR2))
 9023     FORMAT('(ISTRI2(I:I),I=1,NCSTR2) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXW2(IFUNC2,N2,ISTART,ISTOP,IWORD,
     1                  ICOL1,ICOL2,IFUNC3,N3,
     1                  IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--SCAN THE CHARACTER*4 DIMENSIONED VARIABLE IFUNC2
C              BETWEEN COLUMNS ISTART TO ISTOP AND EXTRACT THE IWORD-TH
C              WORD IN THAT INTERVAL.  PLACE THE FIRST AND LAST COLUMNS
C              OF THE IWORD-TH WORD INTO ICOL1 AND ICOL2, PLACE THE
C              IWORD-TH WORD ITSELF INTO THE CHARACTER*4 DIMENSIONED
C              VARIABLE IFUNC3; PLACE THE NUMBER OF CHARACTERS IN THIS
C              IWORD-TH WORD INTO N3.
C
C     NOTE--THIS SUBROUTINE IS SIMILAR
C           (ALTHOUGH THE INPUT ARGUMENT STRUCTURE DIFFERS
C           BY 2 EXTRA ARGUMENTS), TO SUBROUTINE DPEXWO EXCEPT
C           DPEXWO OPERATES ON UNDIMENSIONED CHARACTER*80 VARIABLES
C           IFUNC2 AND IFUNC3, WHEREAS THIS SUBROUTINE DPEXW2
C           OPERATES ON CHARACTER*4 DIMENSIONED VARIABLES IFUNC2 AND
C           IFUNC3.  THE FACT THAT THE VARIABLES ARE HERE DIMENSIONED
C           BECAUSE THEY ARE THEN NOT RESTRICTED TO 80, OR 132, OR
C           WHATEVER.
C     NOTE--EVEN THOUGH VARIABLES IFUNC2 AND IFUNC3 ARE CHARACTER*4,
C           THERE IS NO ESSENTIAL USE BEING MADE OF POSITIONS 2, 3, AND
C           4, AND SO (IF CHANGES WERE MADE IN THE CALLING ROUTINE),
C           IFUNC2 AND IFUNC3 COULD JUST AS WELL HAVE BEEN CHARACTER*1.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/7
C     ORIGINAL VERSION--JULY  1986.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFUNC2
      CHARACTER*4 IFUNC3
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IFUNC2(*)
      DIMENSION IFUNC3(*)
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='W2  '
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXW2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,IERROR,N2
   52   FORMAT('IBUGS2,ISUBRO,IERROR,N2 = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IFUNC2(J),J=1,100)
   54   FORMAT('(IFUNC2(J),J=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ISTART,ISTOP,IWORD
   55   FORMAT('ISTART,ISTOP,IWORD = '32I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************
C               **  STEP 11--                       **
C               **  INITIALIZE THE OUTPUT VARIABLES **
C               **************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL1=(-999)
      ICOL2=(-999)
      N3=(-999)
      DO1100I=1,N2
        IFUNC3(I)='    '
 1100 CONTINUE
C
C               *******************************************
C               **  STEP 12--                            **
C               **  CHECK THE INPUT ARGUMENTS            **
C               *******************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.GT.ISTOP)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1222)
 1222   FORMAT('      ISTART EXCEEDS ISTOP')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1223)ISTART
 1223   FORMAT('      ISTART  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1224)ISTOP
 1224   FORMAT('      ISTOP   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1226)(IFUNC2(I),I=1,100)
 1226   FORMAT('      (IFUNC2(I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ISTART.LT.1.OR.ISTOP.GE.N2)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPEXW2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)N2
 1212   FORMAT('      ISTART IS < 1 OR ISTOP > ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1213)ISTART
 1213   FORMAT('      ISTART  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)ISTOP
 1214   FORMAT('      ISTOP   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)(IFUNC2(I),I=1,100)
 1216   FORMAT('      (IFUNC2(I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(IWORD.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1232)
 1232   FORMAT('      IWORD IS LESS THAN 1 .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1233)IWORD
 1233   FORMAT('      IWORD = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1236)(IFUNC2(I),I=1,100)
 1236   FORMAT('      (IFUNC2(I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               ************************************************
C               **  STEP 21--                                 **
C               **  IDENTIFY THE COLUMNS WHERE                **
C               **  THE IWORD-TH STRING RESIDES               **
C               **  ICOL1 = START COLUMN OF A STRING          **
C               **  ICOL2 = STOP  COLUMN OF A STRING          **
C               ************************************************
C
      ICOL2=ISTART-1
      DO2100ILOOP=1,IWORD
C
      ICOL1=ISTOP+1
      IMIN=ICOL2+1
      IF(IMIN.GT.ISTOP)GOTO2119
      DO2110I=IMIN,ISTOP
      I2=I
      IF(IFUNC2(I).NE.'    ')GOTO2115
 2110 CONTINUE
      ICOL1=ISTOP+1
      GOTO2119
 2115 CONTINUE
      ICOL1=I2
      GOTO2119
 2119 CONTINUE
C
      ICOL2=ISTOP
      IMIN=ICOL1+1
      IF(IMIN.GT.ISTOP)GOTO2129
      DO2120I=IMIN,ISTOP
      I2=I
      IF(IFUNC2(I).EQ.'    ')GOTO2125
 2120 CONTINUE
      ICOL2=ISTOP
      GOTO2129
 2125 CONTINUE
      ICOL2=I2-1
      GOTO2129
 2129 CONTINUE
 
      IF(ICOL1.GE.ISTART.AND.ICOL2.GE.ISTART.AND.
     1   ICOL1.LE.ISTOP.AND.ICOL2.LE.ISTOP)GOTO2139
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2131)
 2131 FORMAT('***** ERROR IN DPEXW2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2132)
 2132 FORMAT('      ICOL1 OR ICOL2 IS < ISTART OR > ISTOP. ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2133)ICOL1
 2133 FORMAT('      ICOL1  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2134)ICOL2
 2134 FORMAT('      ICOL2  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2135)ISTART
 2135 FORMAT('      ISTART = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2136)ISTOP
 2136 FORMAT('      ISTOP  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2137)(IFUNC2(I),I=1,100)
 2137 FORMAT('      (IFUNC2(I),I=1,100) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 2139 CONTINUE
C
      IF(ICOL1.LE.ICOL2)GOTO2149
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2141)
 2141 FORMAT('***** ERROR IN DPEXW2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2142)
 2142 FORMAT('      ICOL1 EXCEEDS ICOL2')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2143)ICOL1
 2143 FORMAT('      ICOL1  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2144)ICOL2
 2144 FORMAT('      ICOL2  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2146)(IFUNC2(I),I=1,100)
 2146 FORMAT('      (IFUNC2(I),I=1,100) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 2149 CONTINUE
C
 2100 CONTINUE
C
C               *********************************************
C               **  STEP 22--                              **
C               **  COPY THE IWORD-TH STRING INTO IFUNC3   **
C               *********************************************
C
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO2200I=ICOL1,ICOL2
      J=J+1
      IFUNC3(J)=IFUNC2(I)
 2200 CONTINUE
      N3=J
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXW2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)ICOL1,ICOL2,N3
 9021   FORMAT('ICOL1,ICOL2,N3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)(IFUNC3(I),I=1,100)
 9023   FORMAT('(IFUNC3(I),I=1,N3) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEXW3(ISTRIN,ISTART,ISTOP,IWORD,
     1                  ICOL1,ICOL2,ISTRI2,NCSTR2,
     1                  IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--SCAN THE CHARACTER VARIABLE ISTRIN BETWEEN COLUMNS
C              ISTART TO ISTOP AND EXTRACT THE STRING STARTING WITH THE
C              IWORD-TH WORD IN THAT INTERVAL UNTIL THE END OF THE
C              STRING.  PLACE THE FIRST AND LAST COLUMNS OF THE IWORD-TH
C              WORD INTO ICOL1 AND ICOL2, PLACE THE IWORD-TH WORD ITSELF
C              INTO ISTRI2; PLACE THE NUMBER OF CHARACTERS IN THIS
C              IWORD-TH WORD INTO NCSTR2.
C
C              THIS IS SIMILAR TO DPEXWO.  THE DISTINCTION IS THAT
C              DPEXWO ONLY EXTRACTS A SINGLE WORD WHILE DPEXW3 EXTRACTS
C              ALL CHARACTERS STARTING WITH THE I-TH WORD.
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
C     VERSION NUMBER--2017/07
C     UPDATED       --2018/04  ALLOW "," AS A WORD SEPARATER (AS LONG
C                              AS IT IS NOT WITHIN QUOTES)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) ISTRIN
      CHARACTER*(*) ISTRI2
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPEX'
      ISUBN2='W3  '
      IERROR='NO'
C
      IQUOTE=0
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEXW3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR
   53   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ISTRIN(J:J),J=1,MIN(100,ISTOP))
   54   FORMAT('(ISTRIN(J:J),J=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ISTART,ISTOP,IWORD
   55   FORMAT('ISTART,ISTOP,IWORD = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************
C               **  STEP 11--                       **
C               **  INITIALIZE THE OUTPUT VARIABLES **
C               **************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL1=(-999)
      ICOL2=(-999)
      NCSTR2=(-999)
      ISTRI2=' '
C
C               *******************************************
C               **  STEP 12--                            **
C               **  CHECK THE INPUT ARGUMENTS            **
C               *******************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LT.1.OR.ISTOP.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPEXW3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      ISTART OR ISTOP IS < 1. ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1213)ISTART
 1213   FORMAT('      ISTART  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)ISTOP
 1214   FORMAT('      ISTOP   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,MIN(100,ISTOP))
 1216   FORMAT('      (ISTRIN(I:I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
C
      ELSEIF(ISTART.GT.ISTOP)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1222)
 1222   FORMAT('      ISTART EXCEEDS ISTOP')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1213)ISTART
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)ISTOP
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,MIN(100,ISTOP))
        CALL DPWRST('XXX','BUG ')
        GOTO9000
C
      ELSEIF(IWORD.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1232)
 1232   FORMAT('      IWORD IS LESS THAN 1 .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1233)IWORD
 1233   FORMAT('      IWORD = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,MIN(ISTOP,100))
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               ************************************************
C               **  STEP 21--                                 **
C               **  IDENTIFY THE COLUMNS WHERE                **
C               **  THE IWORD-TH STRING RESIDES               **
C               **  ICOL1 = START COLUMN OF A STRING          **
C               **  ICOL2 = STOP  COLUMN OF A STRING          **
C               ************************************************
C
C     FIND LAST NON-BLANK CHARACTER AND RESET VALUE OF ISTOP
C
C     THIS CHARACTER WILL DEFINE THE VALUE OF ICOL2.
C
      DO2010I=ISTOP,1,-1
        IF(ISTRIN(I:I).NE.' ')THEN
          ISTOP2=I
          GOTO2019
        ENDIF
 2010 CONTINUE
      ISTOP2=ISTOP
 2019 CONTINUE
      ILAST=ISTOP2
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')THEN
        WRITE(ICOUT,2017)ILAST
 2017   FORMAT('ILAST = ',I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ICOL2=ISTART-1
      DO2100ILOOP=1,IWORD
C
        ICOL1=ISTOP2+1
        IMIN=ICOL2+1
        IF(IMIN.GT.ISTOP2)GOTO2119
        IQUOTE=0
        DO2110I=IMIN,ISTOP2
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')THEN
            WRITE(ICOUT,2117)ILOOP,IMIN,ISTOP2,I,ICOL1,ICOL2,ISTRIN(I:I)
 2117       FORMAT('2110: ILOOP,IMIN,ISTOP2,I,ICOL1,ICOL2,',
     1             'ISTRIN(I:I) = ',6I6,2X,A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
C         CHECK FOR FIRST NON-BLANK CHARACTER AND THEN CHECK
C         IF IT IS QUOTE CHARACTER.
C
          I2=I
          IF(IHYPSW.EQ.'OFF')THEN
            IF(ISTRIN(I:I).NE.' ' .AND. ISTRIN(I:I).NE.',')THEN
              IF(IFILQU.EQ.'ON' .AND. ISTRIN(I:I).EQ.'"')IQUOTE=1
              ICOL1=I2
              GOTO2119
            ENDIF
          ELSE
            IF(ISTRIN(I:I).NE.' ' .AND. ISTRIN(I:I).NE.'-' .AND.
     1         ISTRIN(I:I).NE.',')THEN
              IF(IFILQU.EQ.'ON' .AND. ISTRIN(I:I).EQ.'"')IQUOTE=1
              ICOL1=I2
              GOTO2119
            ENDIF
          ENDIF
C
 2110   CONTINUE
C
C       2014/10: NO NON-BLANK CHARACTER FOUND.  SET TO NULL STRING
C       AND RETURN.
C
        ICOL1=ISTOP
        ICOL2=ISTOP
        NCSTR2=1
        GOTO9000
C
 2119   CONTINUE
C
        ICOL2=ISTOP2
        IMIN=ICOL1+1
        IF(IMIN.GT.ISTOP2)GOTO2129
        DO2120I=IMIN,ISTOP2
          I2=I
          IF(IQUOTE.EQ.0)THEN
            IF(ISTRIN(I:I).EQ.'"' .AND. ISTRIN(I-1:I-1).EQ.'=')THEN
              IQUOTE=1
              GOTO2120
            ELSEIF(ISTRIN(I:I).EQ.' ' .OR. ISTRIN(I:I).EQ.'-' .OR.
     1        (ICOMCL.EQ.'ON' .AND. ISTRIN(I:I).EQ.','))THEN
              ICOL2=I2-1
              GOTO2129
            ENDIF
          ELSE
            IF(ISTRIN(I:I).EQ.'"')THEN
              ICOL2=I2
              GOTO2129
            ENDIF
          ENDIF
 2120   CONTINUE
        ICOL2=ISTOP2
 2129   CONTINUE
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')THEN
          WRITE(ICOUT,2125)ILOOP,ICOL1,ICOL2
 2125     FORMAT('AT 2129: ILOOP,ICOL1,ICOL2 = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ICOL1.GE.ISTART.AND.ICOL2.GE.ISTART.AND.
     1     ICOL1.LE.ISTOP.AND.ICOL2.LE.ISTOP)GOTO2139
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2132)
 2132     FORMAT('      ICOL1 OR ICOL2 IS < ISTART OR > ISTOP. ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2133)ICOL1
 2133     FORMAT('      ICOL1  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2134)ICOL2
 2134     FORMAT('      ICOL2  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2135)ISTART
 2135     FORMAT('      ISTART = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2136)ISTOP
 2136     FORMAT('      ISTOP  = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2137)(ISTRIN(I:I),I=1,80)
 2137     FORMAT('      (ISTRIN(I:I),I=1,80) = ',80A1)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
 2139   CONTINUE
C
        IF(ICOL1.GT.ICOL2)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2142)
 2142     FORMAT('      ICOL1 EXCEEDS ICOL2')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2133)ICOL1
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2134)ICOL2
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1216)(ISTRIN(I:I),I=1,MIN(80,ISTOP))
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
 2100 CONTINUE
C
C               *********************************************
C               **  STEP 22--                              **
C               **  COPY THE IWORD-TH STRING INTO ISTRI2   **
C               *********************************************
C
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL2=ILAST
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')THEN
        WRITE(ICOUT,2209)ICOL1,ICOL2
 2209   FORMAT('BEFORE 2200: ICOL1,ICOL2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      J=0
      DO2200I=ICOL1,ICOL2
        J=J+1
        ISTRI2(J:J)=ISTRIN(I:I)
 2200 CONTINUE
      NCSTR2=J
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'EXW3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPEXW3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IBUGS2,ISUBRO,IERROR,NCSTR2
 9013   FORMAT('IBUGS2,ISUBRO,IERROR,NCSTR2 = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ISTART,ISTOP,IWORD,ICOL1,ICOL2
 9015   FORMAT('ISTART,ISTOP,IWORD,ICOL1,ICOL2 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR2.GE.1)THEN
          WRITE(ICOUT,9023)(ISTRI2(I:I),I=1,MIN(100,NCSTR2))
 9023     FORMAT('(ISTRI2(I:I),I=1,NCSTR2) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEYCO(IHARG,IARGT,ARG,NUMARG,
     1AEYEXC,AEYEYC,AEYEZC,
     1X3DEYE,Y3DEYE,Z3DEYE,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE (X,Y,Z) EYE COORDINATES CONTAINED IN THE
C              3 VARAIBLES AEYEXC,AEYEYC,AEYEZC
C              SUCH EYE COORDINATES ARE USED IN 3-DIMENSIONAL PLOTS.
C     COMMAND = EYE (COORDINATES) ... ... ...
C     NOTE--LOGIC HEREIN ASSUMES THE WORD    COORDINATES   HAS BEEN
C           SHIFTED OUT (DONE IN MAIPC4).
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--AEYEXC  = X-COORDINATE OF EYE
C                     --AEYEYC  = Y-COORDINATE OF EYE
C                     --AEYEZC  = Z-COORDINATE OF EYE
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
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
C     ORIGINAL VERSION--NOVEMBER  1978.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --SEPTEMBER 1993.  REWRITE ALL
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  BRANCH ACCORDING TO THE CASE          **
C               ********************************************
C
      IF(NUMARG.EQ.0)GOTO1000
      IF(NUMARG.GE.1)THEN
         IF(IHARG(NUMARG).EQ.'ON')GOTO1000
         IF(IHARG(NUMARG).EQ.'OFF')GOTO1000
         IF(IHARG(NUMARG).EQ.'AUTO')GOTO1000
         IF(IHARG(NUMARG).EQ.'DEFA')GOTO1000
         IF(IHARG(NUMARG).EQ.'?')GOTO3000
         IF(IARGT(1).EQ.'NUMB'.OR.IARGT(2).EQ.'NUMB'.OR.
     1   IARGT(3).EQ.'NUMB')GOTO2000
         GOTO8000
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  TREAT THE    DEFAULT    CASE--        **
C               ********************************************
C
 1000 CONTINUE
      IFOUND='YES'
      AEYEXC=CPUMIN
      AEYEYC=CPUMIN
      AEYEZC=CPUMIN
      IF(IFEEDB.EQ.'ON')THEN
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1011)
 1011    FORMAT('THE (X,Y,Z) EYE COORDINATES HAVE JUST BEEN SET')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1012)
 1012    FORMAT('TO AUTOMATICALLY FLOAT WITH THE DATA.')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1013)
 1013    FORMAT('THE (X,Y,Z) EYE COORDINATES WILL BE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1021)
 1021    FORMAT('    X = XMIN + 3 * (XMAX - XMIN)')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1022)
 1022    FORMAT('    Y = YMIN + 3 * (YMAX - YMIN)')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1023)
 1023    FORMAT('    Z = ZMIN + 3 * (ZMAX - ZMIN)')
         CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
C               ********************************************
C               **  STEP 12--                             **
C               **  TREAT THE    USER-SPEC    CASE--      **
C               ********************************************
C
 2000 CONTINUE
      IFOUND='YES'
      IF(IARGT(1).EQ.'NUMB'.AND.IHARG(1).NE.'.')THEN
         AEYEXC=ARG(1)
         X3DEYE=ARG(1)
      ENDIF
      IF(IARGT(2).EQ.'NUMB'.AND.IHARG(2).NE.'.')THEN
         AEYEYC=ARG(2)
         Y3DEYE=ARG(2)
      ENDIF
      IF(IARGT(3).EQ.'NUMB'.AND.IHARG(3).NE.'.')THEN
         AEYEZC=ARG(3)
         Z3DEYE=ARG(3)
      ENDIF
      IF(IFEEDB.EQ.'ON')THEN
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,2011)
 2011    FORMAT('THE (X,Y,Z) EYE COORDINATES HAVE JUST BEEN SET TO')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,2021)AEYEXC
 2021    FORMAT('    X = ',E15.7)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,2022)AEYEYC
 2022    FORMAT('    Y = ',E15.7)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,2023)AEYEZC
 2023    FORMAT('    Z = ',E15.7)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
C               ********************************************
C               **  STEP 13--                             **
C               **  TREAT THE    ?    CASE--              **
C               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
C               ********************************************
C
 3000 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,3011)
 3011 FORMAT('THE CURRENT (X,Y,Z) EYE COORDINATES ARE')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,3021)X3DEYE
 3021 FORMAT('    X = ',E15.7)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,3022)Y3DEYE
 3022 FORMAT('    Y = ',E15.7)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,3023)Z3DEYE
 3023 FORMAT('    Z = ',E15.7)
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,3031)
 3031 FORMAT('THE DEFAULT (X,Y,Z) EYE COORDINATES ARE')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,3041)
 3041 FORMAT('    X = XMIN + 3 * (XMAX - XMIN)')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,3042)
 3042 FORMAT('    Y = YMIN + 3 * (YMAX - YMIN)')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,3043)
 3043 FORMAT('    Z = ZMIN + 3 * (ZMAX - ZMIN)')
      CALL DPWRST('XXX','WRIT')
      GOTO9000
C
C               ********************************************
C               **  STEP 14--                             **
C               **  TREAT THE    ERROR    CASE            **
C               ********************************************
C
 8000 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,8011)
 8011 FORMAT('***** ERROR IN DPEYCO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8012)
 8012 FORMAT('      ILLEGAL FORM FOR EYE COORDINATES ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8014)
 8014 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8015)
 8015 FORMAT('      SUPPOSE IT IS DESIRED TO POSITION ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8016)
 8016 FORMAT('      THE AXES EYE FOR A 3 DIMENSIONAL PLOT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8017)
 8017 FORMAT('      AT (IN UNITS OF THE PLOTTED DATA)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8018)
 8018 FORMAT('      (X=500, Y=25000, Z=.03)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8019)
 8019 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8020)
 8020 FORMAT('      EYE COORDINATES 500 2500 .03')
      CALL DPWRST('XXX','BUG ')
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

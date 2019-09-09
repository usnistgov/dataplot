      SUBROUTINE DPDB80(ISTRIN,JMAX,NMAX,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--DETERMINE THE LAST NON-BLANK
C              CHARACTER IN THE CHARACTER*80
C              VARIABLE   ISTRIN    .
C              (THIS IS USEFUL FOR   DEBLANKING   A STRING.)
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
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--DECEMBER  1985.
C     UPDATED         --APRIL     2018. ADD NMAX SO NOT RESTRICTED TO
C                                       80 CHARACTERS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) ISTRIN
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
      ISUBN1='DPDB'
      ISUBN2='80  '
C
      IERROR='NO'
      JMAX=0
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DB80')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDB80--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR,JMAX,NMAX
   53   FORMAT('IBUGS2,ISUBRO,IERROR,JMAX,NMAX = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ISTRIN(J:J),J=1,MIN(100,NMAX))
   54   FORMAT('(ISTRIN(J:J),J=1,NMAX) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************
C               **  STEP 1--            **
C               **  DETERMINE THE LAST  **
C               **  NON-BLANK CHARACTER **
C               **************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DB80')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1100I=1,NMAX
        IREV=NMAX-I+1
        IF(ISTRIN(IREV:IREV).EQ.' ')GOTO1100
        GOTO1150
 1100 CONTINUE
      JMAX=0
      GOTO1190
 1150 CONTINUE
      JMAX=IREV
      GOTO1190
 1190 CONTINUE
C
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DB80')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDB80--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,JMAX
 9013   FORMAT('IERROR,JMAX = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ISTRIN(J:J),J=1,MIN(100,NMAX))
 9014   FORMAT('(ISTRIN(J:J),J=1,NMAX) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDCF2(XMAT,NUMFAC,NINP,MAXK,MAXROW,
     1                  X2T,MAX2T,
     1                  ME,X2TROW,X2TCOL,CNTAG1,CNTAG2,
     1                  IROW,ICOL,
     1                  NCME,NCMEC,NCST2T,NCST2C,NCSTC,NCSTT,
     1                  STME,STMEC,ST2T,ST2TC,STC,STT,
     1                  NUMCNF,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE: THE DEX CONFOUND COMMAND GENERATES THE CONFOUNDING
C              STRUCTURE FOR A 2-LEVEL FACTORIAL DESIGN.  IT
C              IMPLEMENTS THE "CONF.DP" MACRO THAT IS PART OF THE
C              "10-STEP" ANALYSIS.  THIS ROUTINE IS BASICALLY TO
C              IMPROVE PERFORMANCE.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2018/01
C     ORIGINAL VERSION--JANUARY   2018.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IWRITE
      CHARACTER*24 STJ1
      CHARACTER*24 STJ2
      CHARACTER*24 STROWJ
      CHARACTER*24 STCOLJ
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 ICTEMP
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEL
      CHARACTER*1 STPLUS
      CHARACTER*20 IFORMT
      CHARACTER*4  ISTR(40)
C
C---------------------------------------------------------------------
C
      REAL XMAT(MAXROW,NUMFAC)
      REAL X2T(NINP,MAX2T)
      REAL X2TROW(*)
      REAL X2TCOL(*)
      REAL CNTAG1(*)
      REAL CNTAG2(*)
      REAL ME(*)
C
      INTEGER IROW(*)
      INTEGER ICOL(*)
      INTEGER NCME(*)
      INTEGER NCMEC(*)
      INTEGER NCST2T(*)
      INTEGER NCST2C(*)
      INTEGER NCSTC(*)
      INTEGER NCSTT(*)
C
      CHARACTER*40 STME(*)
      CHARACTER*40 STMEC(*)
      CHARACTER*40 ST2T(*)
      CHARACTER*40 ST2TC(*)
      CHARACTER*40 STC(*)
      CHARACTER*40 STT(*)
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
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
      ISUBN1='DPDC'
      ISUBN2='F2  '
      IERROR='NO'
      IFORMT=' '
      IWRITE='OFF'
C
      DO10J=1,MAX2T
        DO11I=1,NINP
          X2T(I,J)=-999.0
   11   CONTINUE
   10 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDCF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NINP,NUMFAC,MAXK,MAX2T
   52   FORMAT('NIMP,NUMFAC,MAXK,MAX2T = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO61I=1,NINP
          WRITE(ICOUT,63)I,(XMAT(I,J),J=1,MIN(NUMFAC,20))
   63     FORMAT('I,XMAT(I,J) = ',I8,20G15.7)
          CALL DPWRST('XXX','BUG ')
   61   CONTINUE
      ENDIF
C
C               **************************************************
C               **  STEP 1--                                    **
C               **   CHECK FOR VALID VALUES OF K                **
C               **   CHECK THAT ALL VALUES ARE -1 OR +1         **
C               **************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMFAC.LT.1 .OR. NUMFAC.GT.MAXK)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN DEX CONFOUND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)MAXK
  103   FORMAT('      THE NUMBER OF DESIGN FACTORS IS LESS THAN ',
     1         '1 OR GREATER THAN ',I3)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)NUMFAC
  105   FORMAT('      THE NUMBER OF DESIGN FACTORS IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     REMOVE CENTER POINTS (I.E., = 0) AND CHECK THAT ALL REMAINING
C     POINTS ARE EITHER -1 OR +1.
C
      DO110J=1,NUMFAC
        ICNT=0
        DO120I=1,NINP
          AVAL=XMAT(I,J)
          IF(AVAL.EQ.0.0)THEN
            GOTO120
          ELSEIF(AVAL.NE.-1.0 .AND. AVAL.NE.1.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,115)I,J
  115       FORMAT('     ROW ',I8,' OF FACTOR ',I5,' DOES NOT EQUAL ',
     1             '-1 OR +1')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,117)AVAL
  117       FORMAT('     THE VALUE IS ',G15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSE
            ICNT=ICNT+1
            XMAT(ICNT,J)=XMAT(I,J)
          ENDIF
  120   CONTINUE
  110 CONTINUE
      NINP=ICNT
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,121)NINP
  121   FORMAT('AFTER CHECK FOR CENTER POINTS, NINP = ',I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 2--                                       **
C               **  CHECK FOR BALANCE IN EACH OF THE INPUT FACTORS **
C               *****************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO210J=1,NUMFAC
        DO220I=1,NINP
          CNTAG1(I)=XMAT(I,J)
  220   CONTINUE
C
        CALL DISTIN(CNTAG1,NINP,IWRITE,CNTAG2,NDIST,IBUGA3,IERROR)
        DO230L=1,NDIST
          AVAL=CNTAG2(L)
          ICNT=0
          DO235I=1,NINP
            IF(CNTAG1(I).EQ.AVAL)ICNT=ICNT+1
  235     CONTINUE
          IF(L.EQ.1)THEN
            ICNTSV=ICNT
          ELSE
            IF(ICNT.NE.ICNTSV)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,237)J
  237         FORMAT('      FACTOR ',I5,' IS NOT BALANCED, THIS ',
     1               'DESIGN IS NOT ORTHOGONAL.')
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,238)L,ICNT,ICNTSV
  238         FORMAT('      LEVEL ',I5,' HAS COUNT ',I5,
     1               ' WHEN EXPECTED COUNT WAS ',I5)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','WRIT')
              IERROR='YES'
              GOTO9000
            ENDIF
          ENDIF
  230   CONTINUE
C
  210 CONTINUE
C
C               *****************************************************
C               **  STEP 3--                                       **
C               **  COMPUTE ALL K-CHOOSE-2 2-TERM INTERACTION      **
C               **  FACTORS.                                       **
C               *****************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICNT=0
      KM1=NUMFAC-1
      DO301J1=1,KM1
        J1P1=J1+1
        DO302J2=J1P1,NUMFAC
          ICNT=ICNT+1
C
C         CHECK FOR MAXIMUM NUMBER OF 2-TERM INTERACTIONS
C
          IF(ICNT.GT.MAX2T)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,303)MAX2T
  303       FORMAT('      MAXIMUM NUMBER OF 2-TERM INTERACTIONS (',
     1             I5,') EXCEEDED.')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IROW(ICNT)=J1
          ICOL(ICNT)=J2
          DO305I=1,NINP
            X2T(I,ICNT)=XMAT(I,J1)*XMAT(I,J2)
  305     CONTINUE
  302   CONTINUE
  301 CONTINUE
      NX2T=ICNT
C
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,306)NX2T
  306   FORMAT('NX2T = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,307)
  307   FORMAT('X2T MATRIX:')
        CALL DPWRST('XXX','BUG ')
        IFORMT='(   F5.1)'
        WRITE(IFORMT(2:4),'(I3)')NX2T
        DO309I=1,NINP
          WRITE(ICOUT,IFORMT)(X2T(I,J),J=1,MIN(45,NX2T))
          CALL DPWRST('XXX','BUG ')
  309   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 3.1--                                     **
C               **  DETERMINE THE CONFOUNDING FOR EACH OF THE      **
C               **  NUMFAC MAIN EFFECTS                            **
C               *****************************************************
C
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      STPLUS='+'
      ICNT=0
      DO310J1=1,NUMFAC
        ME(J1)=REAL(J1)
        STME(J1)=' '
        IF(J1.LE.9)THEN
          WRITE(STME(J1)(1:1),'(I1)')J1
          NCME(J1)=1
        ELSEIF(J1.LE.99)THEN
          STME(J1)='(  )'
          WRITE(STME(J1)(2:3),'(I2)')J1
          NCME(J1)=4
        ELSEIF(J1.LE.999)THEN
          STME(J1)='(   )'
          WRITE(STME(J1)(2:4),'(I3)')J1
          NCME(J1)=5
        ENDIF
        STMEC(J1)='-999'
        NCMEC(J1)=4
  310 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        DO315J=1,NUMFAC
          WRITE(ICOUT,317)J,ME(J),NCME(J),STME(J)
  317     FORMAT('J,ME(J),NCME(J),STME(J) = ',I5,F7.1,I5,2X,A8)
          CALL DPWRST('XXX','BUG ')
  315   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 3.2--                                     **
C               **  STEP THROUGH EACH MAIN EFFECT AND COMPARE TO   **
C               **  ALL OTHER MAIN EFFECTS                         **
C               *****************************************************
C
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICNT=0
      DO320J1=1,NUMFAC
        CALL RANGDP(XMAT(1,J1),NINP,IWRITE,RJ1,IBUGA3,IERROR)
        DO325J2=1,NUMFAC
          CALL RANGDP(XMAT(1,J2),NINP,IWRITE,RJ2,IBUGA3,IERROR)
          RJ1J2=RJ1*RJ2
          IF(J1.NE.J2 .AND. RJ1J2.NE.0.0)THEN
            CALL CORR(XMAT(1,J1),XMAT(1,J2),NINP,IWRITE,CJ1J2,
     1                 IBUGA3,IERROR)
            CJ1J2A=ABS(CJ1J2)
            IF(CJ1J2A.EQ.1.0)THEN
              ICNT=ICNT+1
              IROWJ=J1
              ICOLJ=J2
              STROWJ=' '
              STCOLJ=' '
              IF(IROWJ.LE.9)THEN
                WRITE(STROWJ(1:1),'(I1)')IROWJ
              ELSEIF(IROWJ.LE.99)THEN
                STROWJ='(  )'
                WRITE(STROWJ(2:3),'(I2)')IROWJ
              ELSEIF(IROWJ.LE.999)THEN
                STROWJ='(   )'
                WRITE(STROWJ(2:4),'(I3)')IROWJ
              ENDIF
              IF(ICOLJ.LE.9)THEN
                WRITE(STCOLJ(1:1),'(I1)')ICOLJ
                NC=1
              ELSEIF(ICOLJ.LE.99)THEN
                STCOLJ='(  )'
                WRITE(STCOLJ(2:3),'(I2)')ICOLJ
                NC=4
              ELSEIF(ICOLJ.LE.999)THEN
                STCOLJ='(   )'
                WRITE(STCOLJ(2:4),'(I3)')ICOLJ
                NC=5
              ENDIF
              IF(STMEC(J1).NE.'-999')THEN
                NC2=NCMEC(J1)+1
                STMEC(J1)(NC2:NC2)='+'
                NC2=NC2+1
                NC3=NC2+NC-1
                STMEC(J1)(NC2:NC3)=STCOLJ(1:NC)
                NCMEC(J1)=NC3
              ELSE
                STMEC(J1)=' '
                STMEC(J1)(1:NC)=STCOLJ(1:NC)
                NCMEC(J1)=NC
              ENDIF
            ENDIF
          ENDIF
  325   CONTINUE
  320 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,321)ICNT
  321   FORMAT('ICNT = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO322L=1,NUMFAC
          WRITE(ICOUT,323)L,NCMEC(L),STMEC(L)
  323     FORMAT('I,NCMEC(L),STMEC(L) = ',2I5,2X,A40)
          CALL DPWRST('XXX','BUG ')
  322   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 3.3--                                     **
C               **  STEP THROUGH EACH MAIN EFFECT AND COMPARE TO   **
C               **  ALL 2-TERM VECTORS                             **
C               *****************************************************
C
      ISTEPN='33'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      EPS=0.001
C
      DO330J1=1,NUMFAC
        CALL RANGDP(XMAT(1,J1),NINP,IWRITE,RJ1,IBUGA3,IERROR)
        DO335J2=1,NX2T
          CALL RANGDP(X2T(1,J2),NINP,IWRITE,RJ2,IBUGA3,IERROR)
          RJ1J2=RJ1*RJ2
C
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
            WRITE(ICOUT,338)J1,J2,RJ1,RJ2,RJ1J2
  338       FORMAT('J1,J2,RJ1,RJ2,RJ1J2 = ',2I8,3F10.3)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(RJ1J2.NE.0.0)THEN
            CALL CORR(XMAT(1,J1),X2T(1,J2),NINP,IWRITE,CJ1J2,
     1                 IBUGA3,IERROR)
            CJ1J2A=ABS(CJ1J2)
C
            IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
              WRITE(ICOUT,339)J1,J2,CJ1J2,CJ1J2A
  339         FORMAT('J1,J2,CJ1J2,CJ1J2A = ',2I8,2F10.3)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
CCCCC       IF(CJ1J2A.EQ.1.0)THEN
            IF(ABS(CJ1J2A - 1.0).LE.EPS)THEN
C
              ISTEPN='33A'
              IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              ICNT=ICNT+1
              IROWJ=IROW(J2)
              ICOLJ=ICOL(J2)
              STROWJ=' '
              STCOLJ=' '
              IF(IROWJ.LE.9)THEN
                WRITE(STROWJ(1:1),'(I1)')IROWJ
                NCR=1
              ELSEIF(IROWJ.LE.99)THEN
                STROWJ='(  )'
                WRITE(STROWJ(2:3),'(I2)')IROWJ
                NCR=4
              ELSEIF(IROWJ.LE.999)THEN
                STROWJ='(   )'
                WRITE(STROWJ(2:4),'(I3)')IROWJ
                NCR=5
              ENDIF
              IF(ICOLJ.LE.9)THEN
                WRITE(STCOLJ(1:1),'(I1)')ICOLJ
                NCC=1
              ELSEIF(ICOLJ.LE.99)THEN
                STCOLJ='(  )'
                WRITE(STCOLJ(2:3),'(I2)')ICOLJ
                NCC=4
              ELSEIF(ICOLJ.LE.999)THEN
                STCOLJ='(   )'
                WRITE(STCOLJ(2:4),'(I3)')ICOLJ
                NCC=5
              ENDIF
              IF(STMEC(J1).NE.'-999')THEN
                NC2=NCMEC(J1)+1
                STMEC(J1)(NC2:NC2)='+'
                NC2=NC2+1
                NC3=NC2+NCR-1
                STMEC(J1)(NC2:NC3)=STROWJ(1:NCR)
                NC2=NC3+1
                NC3=NC2+NCC-1
                STMEC(J1)(NC2:NC3)=STCOLJ(1:NCC)
                NCMEC(J1)=NC3
              ELSE
                STMEC(J1)=' '
                STMEC(J1)(1:NCR)=STROWJ(1:NCR)
                NC=NCR+1
                STMEC(J1)(NC:NC+NCC-1)=STCOLJ(1:NCC)
                NCMEC(J1)=NC+NCC-1
              ENDIF
C
              IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
                WRITE(ICOUT,332)IROWJ,ICOLJ,STROWJ,STCOLJ
  332           FORMAT('IROWJ,ICOLJ,STROWJ,STCOLJ = ',2I5,2A40)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
            ENDIF
          ENDIF
  335   CONTINUE
  330 CONTINUE
C
CCCCC DO439L=1,NUMFAC
CCCCC   IF(STMEC(J1).EQ.'-999')STMEC(J1)=' '
CCCCC   NCMEC(J1)=1
CC439 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,331)ICNT
  331   FORMAT('ICNT = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO337L=1,NUMFAC
          WRITE(ICOUT,323)L,NCMEC(L),STMEC(L)
          CALL DPWRST('XXX','BUG ')
  337   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 4--                                       **
C               **  DETERMINE THE CONFOUNDING FOR EACH OF THE      **
C               **  K-CHOOSE-2 2-TERM INTERACTIONS                 **
C               *****************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     INITIALIZE OUTPUT STRINGS
C
      ICNT=0
      KM1=NUMFAC-1
      DO401J1=1,KM1
        STJ1=' '
        IF(J1.LE.9)THEN
          WRITE(STJ1(1:1),'(I1)')J1
          NC1=1
        ELSEIF(J1.LE.99)THEN
          STJ1='(  )'
          WRITE(STJ1(2:3),'(I2)')J1
          NC1=4
        ELSEIF(J1.LE.999)THEN
          STJ1='(   )'
          WRITE(STJ1(2:4),'(I3)')J1
          NC1=5
        ENDIF
        J1P1=J1+1
        DO402J2=J1P1,NUMFAC
          STJ2=' '
          IF(J2.LE.9)THEN
            WRITE(STJ2(1:1),'(I1)')J2
            NC2=1
          ELSEIF(J2.LE.99)THEN
            STJ2='(  )'
            WRITE(STJ2(2:3),'(I2)')J2
            NC2=4
          ELSEIF(J2.LE.999)THEN
            STJ2='(   )'
            WRITE(STJ2(2:4),'(I3)')J2
            NC2=5
          ENDIF
          ICNT=ICNT+1
          X2TROW(ICNT)=REAL(J1)
          X2TCOL(ICNT)=REAL(J2)
          ST2T(ICNT)=' '
          ST2T(ICNT)(1:NC1)=STJ1(1:NC1)
          ST2T(ICNT)(NC1+1:NC1+NC2)=STJ2(1:NC2)
          NCST2T(ICNT)=NC1+NC2
          ST2TC(ICNT)='-999'
          NCST2C(ICNT)=4
  402   CONTINUE
  401 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,403)ICNT
  403   FORMAT('ICNT = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO404L=1,ICNT
          WRITE(ICOUT,405)L,X2TROW(L),X2TCOL(L),NCST2T(L),ST2T(L)
  405     FORMAT('L,X2TROW(L),X2TCOL(L),NCST2T(L),ST2T(L) = ',
     1           I5,2F6.1,I5,2X,A40)
          CALL DPWRST('XXX','BUG ')
  404   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 5.1--                                     **
C               **  STEP THROUGH EACH MAIN EFFECT AND COMPARE TO   **
C               **  ALL 2-TERM VECTORS                             **
C               *****************************************************
C
      ISTEPN='51'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO511J1=1,NX2T
        CALL RANGDP(X2T(1,J1),NINP,IWRITE,RJ1,IBUGA3,IERROR)
        DO512J2=1,NUMFAC
          CALL RANGDP(XMAT(1,J2),NINP,IWRITE,RJ2,IBUGA3,IERROR)
          RJ1J2=RJ1*RJ2
          IF(RJ1J2.NE.0.0)THEN
            CALL CORR(X2T(1,J1),XMAT(1,J2),NINP,IWRITE,CJ1J2,
     1                 IBUGA3,IERROR)
            CJ1J2A=ABS(CJ1J2)
            IF(ABS(CJ1J2A - 1.0).LE.EPS)THEN
              ICNT=ICNT+1
              STJ2=' '
              IF(J2.LE.9)THEN
                WRITE(STJ2(1:1),'(I1)')J2
                NC=1
              ELSEIF(J2.LE.99)THEN
                STJ2='(  )'
                WRITE(STJ2(2:3),'(I2)')J2
                NC=4
              ELSEIF(J2.LE.999)THEN
                STJ2='(   )'
                WRITE(STJ2(2:4),'(I3)')J2
                NC=5
              ENDIF
              IF(ST2TC(J1).NE.'-999')THEN
                NC2=NCST2C(J1)+1
                ST2TC(J1)(NC2:NC2)='+'
                NC2=NC2+1
                NC3=NC2+NC-1
                ST2TC(J1)(NC2:NC3)=STJ2(1:NC)
                NCST2C(J1)=NC3
              ELSE
                ST2TC(J1)=' '
                ST2TC(J1)(1:NC)=STJ2(1:NC)
                NCST2C(J1)=NC
              ENDIF
            ENDIF
          ENDIF
  512   CONTINUE
  511 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,513)ICNT
  513   FORMAT('ICNT = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO515L=1,NUMFAC
          WRITE(ICOUT,517)L,NCST2C(L),ST2TC(L)
  517     FORMAT('L,NCST2C(L),ST2TC(L) = ',2I5,A40)
          CALL DPWRST('XXX','BUG ')
  515   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 5.2--                                     **
C               **  STEP THROUGH EACH 2-TERM VECTOR AND COMPARE TO **
C               **  ALL OTHER 2-TERM VECTORS                       **
C               *****************************************************
C
      ISTEPN='52'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO521J1=1,NX2T
        CALL RANGDP(X2T(1,J1),NINP,IWRITE,RJ1,IBUGA3,IERROR)
        DO522J2=1,NX2T
          CALL RANGDP(X2T(1,J2),NINP,IWRITE,RJ2,IBUGA3,IERROR)
          RJ1J2=RJ1*RJ2
          IF(RJ1J2.NE.0.0 .AND. J1.NE.J2)THEN
            CALL CORR(X2T(1,J1),X2T(1,J2),NINP,IWRITE,CJ1J2,
     1                 IBUGA3,IERROR)
            CJ1J2A=ABS(CJ1J2)
            IF(ABS(CJ1J2A - 1.0).LE.EPS)THEN
              ICNT=ICNT+1
              IROWJ=IROW(J2)
              ICOLJ=ICOL(J2)
              STROWJ=' '
              STCOLJ=' '
              IF(IROWJ.LE.9)THEN
                WRITE(STROWJ(1:1),'(I1)')IROWJ
                NCR=1
              ELSEIF(IROWJ.LE.99)THEN
                STROWJ='(  )'
                WRITE(STROWJ(2:3),'(I2)')IROWJ
                NCR=4
              ELSEIF(IROWJ.LE.099)THEN
                STROWJ='(   )'
                WRITE(STROWJ(2:4),'(I3)')IROWJ
                NCR=5
              ENDIF
              IF(ICOLJ.LE.9)THEN
                WRITE(STCOLJ(1:1),'(I1)')ICOLJ
                NCC=1
              ELSEIF(ICOLJ.LE.99)THEN
                STCOLJ='(  )'
                WRITE(STCOLJ(2:3),'(I2)')ICOLJ
                NCC=4
              ELSEIF(ICOLJ.LE.099)THEN
                STCOLJ='(   )'
                WRITE(STCOLJ(2:4),'(I3)')ICOLJ
                NCC=5
              ENDIF
              IF(ST2TC(J1).NE.'-999')THEN
                NC2=NCST2C(J1)+1
                ST2TC(J1)(NC2:NC2)='+'
                NC2=NC2+1
                NC3=NC2+NCR-1
                ST2TC(J1)(NC2:NC3)=STROWJ(1:NCR)
                NC2=NC3+1
                NC3=NC2+NCC-1
                ST2TC(J1)(NC2:NC3)=STCOLJ(1:NCC)
                NCST2C(J1)=NC3
              ELSE
                ST2TC(J1)=' '
                ST2TC(J1)(1:NCR)=STROWJ(1:NCR)
                NC=NCR+1
                ST2TC(J1)(NC:NC+NCC-1)=STCOLJ(1:NCC)
                NCST2C(J1)=NC+NCC-1
              ENDIF
            ENDIF
          ENDIF
  522   CONTINUE
  521 CONTINUE
C
CCCCC DO529L=1,NUMFAC
CCCCC   IF(STMEC(J1).EQ.'-999')STMEC(J1)=' '
CCCCC   NCMEC(J1)=1
CC529 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,523)NX2T
  523   FORMAT('STEP 52: NX2T = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO525L=1,NX2T
          WRITE(ICOUT,527)L,NCST2C(L),ST2TC(L)
  527     FORMAT('L,NCST2C(L),ST2TC(L) = ',2I5,2X,A40)
          CALL DPWRST('XXX','BUG ')
  525   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 6--                                       **
C               **  MERGE THE ME AND X2T OUTPUT VECTORS INTO A     **
C               **  SINGLE PAIR OF OUTPUT VECTORS                  **
C               *****************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICNT=0
      DO605I=1,MAXOBV
        CNTAG1(I)=0.0
        CNTAG2(I)=0.0
        NCSTT(I)=-1
        NCSTC(I)=-1
  605 CONTINUE
C
      DO610J=1,NUMFAC
        ICNT=ICNT+1
        CNTAG1(ICNT)=ME(J)
        CNTAG2(ICNT)=-999.0
        STT(ICNT)=STME(J)
        NCSTT(ICNT)=NCME(J)
        STC(ICNT)=STMEC(J)
        NCSTC(ICNT)=NCMEC(J)
  610 CONTINUE
C
      DO620J=1,NX2T
        ICNT=ICNT+1
        CNTAG1(ICNT)=X2TROW(J)
        CNTAG2(ICNT)=X2TCOL(J)
        STT(ICNT)=ST2T(J)
        NCSTT(ICNT)=NCST2T(J)
        STC(ICNT)=ST2TC(J)
        NCSTC(ICNT)=NCST2C(J)
  620 CONTINUE
C
      NUMCNF=ICNT
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,623)NUMCNF
  623   FORMAT('NUMCNF = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO625L=1,NUMCNF
          WRITE(ICOUT,627)L,CNTAG1(L),CNTAG2(L)
  627     FORMAT('L,CNTAG1(L),CNTAG2(L) = ',I5,2G15.7)
          CALL DPWRST('XXX','BUG ')
  625   CONTINUE
        DO635L=1,NUMCNF
          WRITE(ICOUT,637)L,NCSTC(L),STC(L),NCSTT(L),STT(L)
  637     FORMAT('L,NCSTC(L),STC(L),NCSTT(L),STT(L) = ',
     1           I5,2(I5,2X,A40))
          CALL DPWRST('XXX','BUG ')
  635   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 7--                                        **
C               **  SAVE STC AND STT STRINGS IN INTERNAL NAME TABLE **
C               ******************************************************
C
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'STCN')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO710J=0,NUMCNF
        IHLEFT='STT '
        IHLEF2='    '
        ICTEMP=' '
        IF(J.LE.9)THEN
          WRITE(IHLEFT(4:4),'(I1)')J
        ELSEIF(J.LE.99)THEN
          WRITE(ICTEMP(1:2),'(I2)')J
          IHLEFT(4:4)=ICTEMP(1:1)
          IHLEF2(1:1)=ICTEMP(2:2)
        ELSEIF(J.LE.999)THEN
          WRITE(ICTEMP(1:3),'(I3)')J
          IHLEFT(4:4)=ICTEMP(1:1)
          IHLEF2(1:2)=ICTEMP(2:3)
        ENDIF
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2'.AND.J.GE.1)THEN
          WRITE(ICOUT,713)J,NCSTT(J),STT(J),NCSTC(J),STC(J),
     1                    IHLEFT,IHLEF2
  713     FORMAT('J,NCSTT(J),STT(J),NCSTC(J),STC(J),IHLEFT,IHLEF2 = ',
     1           I3,2(I5,2X,A40),2X,2A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        NEWNAM='NO'
        NEWCOL='NO'
        ICASEL='UNKN'
        NIOLD1=0
        ICOLL=0
C
        DO720I=1,NUMNAM
          I2=I
          IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
            IF(IUSE(I2).EQ.'F')THEN
              ICASEL='STRI'
              ILISTL=I2
              GOTO729
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,723)IHLEFT,IHLEF2
  723         FORMAT('      THE STRING ',A4,A4,' ALREADY EXISTS, ',
     1               'BUT NOT AS A STRING.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
          ENDIF
  720   CONTINUE
C
        NEWNAM='YES'
        IF(ICASEL.EQ.'UNKN')ICASEL='STRI'
C
        ILISTL=NUMNAM+1
        IF(ILISTL.GT.MAXNAM)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,727)
  727     FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND ',
     1           'FUNCTION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,728)MAXNAM
  728     FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
  729   CONTINUE
C
        IF(J.GE.1)THEN
          DO731L=1,NCSTT(J)
            ISTR(L)=' '
            ISTR(L)(1:1)=STT(J)(L:L)
  731     CONTINUE
          NCTEMP=NCSTT(J)
        ELSE
          ISTR(1)='N   '
          ISTR(2)='U   '
          ISTR(3)='L   '
          ISTR(4)='L   '
          NCTEMP=4
       ENDIF
C
        CALL DPINFU(ISTR,NCTEMP,IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1              NUMNAM,IANS,IWIDTH,IHLEFT,IHLEF2,ILISTL,
     1              NEWNAM,MAXNAM,
     1              IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IHLEFT='STC '
        IHLEF2='    '
        ICTEMP=' '
        IF(J.LE.9)THEN
          WRITE(IHLEFT(4:4),'(I1)')J
        ELSEIF(J.LE.99)THEN
          WRITE(ICTEMP(1:2),'(I2)')J
          IHLEFT(4:4)=ICTEMP(1:1)
          IHLEF2(1:1)=ICTEMP(2:2)
        ELSEIF(J.LE.999)THEN
          WRITE(ICTEMP(1:3),'(I3)')J
          IHLEFT(4:4)=ICTEMP(1:1)
          IHLEF2(1:2)=ICTEMP(2:3)
        ENDIF
C
        NEWNAM='NO'
        NEWCOL='NO'
        ICASEL='UNKN'
        NIOLD1=0
        ICOLL=0
C
        DO740I=1,NUMNAM
          I2=I
          IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
            IF(IUSE(I2).EQ.'F')THEN
              ICASEL='STRI'
              ILISTL=I2
              GOTO749
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,723)IHLEFT,IHLEF2
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
          ENDIF
  740   CONTINUE
C
        NEWNAM='YES'
        IF(ICASEL.EQ.'UNKN')ICASEL='STRI'
C
        ILISTL=NUMNAM+1
        IF(ILISTL.GT.MAXNAM)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,727)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,728)MAXNAM
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
  749   CONTINUE
C
        IF(J.GE.1)THEN
          DO741L=1,NCSTC(J)
            ISTR(L)=' '
            ISTR(L)(1:1)=STC(J)(L:L)
  741     CONTINUE
          NCTEMP=NCSTC(J)
        ELSE
          ISTR(1)='N   '
          ISTR(2)='U   '
          ISTR(3)='L   '
          ISTR(4)='L   '
          NCTEMP=4
        ENDIF
C
        CALL DPINFU(ISTR,NCTEMP,IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1              NUMNAM,IANS,IWIDTH,IHLEFT,IHLEF2,ILISTL,
     1              NEWNAM,MAXNAM,
     1              IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
  710 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DCF2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDCF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NUMCNF
 9012   FORMAT('IERROR,NUMCOR = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMCNF
          WRITE(ICOUT,9017)I,CNTAG1(I),CNTAG2(I)
 9017     FORMAT('I,CNTAG1(I),CNTAG2(I) = ',I8,2F10.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDCNT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A DEX CONTOUR PLOT--
C              THE COMMAND HAS THE FOLLOWING FORMAT:
C                  DEX CONTOUR PLOT Z X1 X2 YCONT
C              WHERE X1 AND X2 ARE RESTRICTED TO HAVING VALUES
C              IN THE (-1,1) INTERVAL.
C     EXAMPLE--DEX CONTOUR PLOT Z X1 X2
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/12
C     ORIGINAL VERSION--DECEMBER   1999.
C     UPDATED         --FEBRUARY   2011. USE DPPARS, DPPAR5
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
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 ICASEQ
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASE
      CHARACTER*4 ICASP2
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
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION Z(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION YCONT(MAXOBV)
      DIMENSION U1JUNK(MAXOBV)
      DIMENSION ZTEMP(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION X1TEMP(MAXOBV)
      DIMENSION X2TEMP(MAXOBV)
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Z(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB3),X2(1))
      EQUIVALENCE (GARBAG(IGARB4),YCONT(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),U1JUNK(1))
      EQUIVALENCE (GARBAG(IGARB7),PRED2(1))
      EQUIVALENCE (GARBAG(IGARB8),RES2(1))
      EQUIVALENCE (GARBAG(IGARB9),X1TEMP(1))
      EQUIVALENCE (GARBAG(IGAR10),X2TEMP(1))
      EQUIVALENCE (GARBAG(JGAR11),ZTEMP(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOPC.INC'
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
      ISUBN1='DPDC'
      ISUBN2='NT  '
      ICASPL='DCON'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ****************************************
C               **  TREAT THE DEX CONTOUR PLOT CASE   **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDCNT--')
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CONT'.AND.
     1   IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='DEX CONTOUR PLOT'
      MINNA=3
      MAXNA=100
      MINN2=2
      IFLAGE=99
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')THEN
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
C               **       Z(.)                               **
C               **       X1(.)                              **
C               **       X2(.)                              **
C               **  CONTAINING                              **
C               **       THE RESPONSE Z VARIABLE            **
C               **       THE HORIZONTAL AXIS VARIABLE       **
C               **       THE VERTICAL AXIS VARIABLE         **
C               **  RESPECTIVELY.                           **
C               **********************************************
C
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=1
      NUMVA2=3
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Z,X1,X2,X2,X2,X2,X2,NS,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               **********************************************
C               **  STEP 34--                               **
C               **  FORM THE FULL VARIABLE                  **
C               **       YCONT(.)                           **
C               **  CONTAINING THE VALUES                   **
C               **  OF THE RESPONSE VARIABLE                **
C               **  WHERE IT IS DESIRED THAT                **
C               **  CONTOUR CURVES BE DETERMINED.           **
C               **********************************************
C
      ISTEPN='34'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMVAR.EQ.4)THEN
        ICOL=4
        NUMVA2=1
        NQ=NRIGHT(4)
        DO3410I=1,NQ
          ISUB(I)=1.0
 3410   CONTINUE
C
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              YCONT,U1JUNK,U1JUNK,N4,NLOCA2,NLOCA3,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        AINC2=-1.0
      ELSE
C
C       IF USER DOES NOT SPECIFY CONTOUR LEVELS, DETERMINE SOME
C       VALUES TO BE USED FOR DATAPLOT GENERATED CONTOUR LEVELS.
C
        N4=0
        ICASP2='Y'
        DO301II=1,NS
          U1JUNK(II)=Z(II)
  301   CONTINUE
C
        ISTEPN='34A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        CALL DPFRLI(ICASP2,IFRALI,U1JUNK,NS,
     1              GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1              IX1TSC,IX1TSW,IY1TSC,IY1TSW,
     1              IX1JSW,IY1JSW,
     1              NMJX1T,NMNX1T,IX1NSW,NMJY1T,NMNY1T,IY1NSW,
     1              PX1COO,X1COOR,NX1COO,
     1              PY1COO,Y1COOR,NY1COO,
     1              PX1CMN,X1COMN,NX1CMN,PX1TOL,PX1TOR,
     1              PY1CMN,Y1COMN,NY1CMN,PY1TOB,PY1TOT,
     1              ITICUN,PXMIN,PXMAX,PYMIN,PYMAX,
     1              FMIN,FMAX,
     1              IBUGG3,ISUBRO,IERROR)
C
        ISTEPN='34B'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        RANTIC=FMAX - FMIN
        TERM=RANTIC
        DO305I=1,1000
          IF(TERM.GT.10.0)TERM=TERM/10.0
          IF(TERM.LT.1.0)TERM=TERM*10.0
          IF(1.0.LE.TERM.AND.TERM.LE.10.0)GOTO306
  305   CONTINUE
  306   CONTINUE
        SIGDIG=AINT(TERM)
        AINC=RANTIC/SIGDIG
        NUMINC=INT((RANTIC/AINC)+0.01)
        AINC2=AINC
        IF(NUMINC.EQ.1)THEN
          AINC2=AINC/10.
        ELSEIF(NUMINC.EQ.2)THEN
          AINC2=AINC/5.
        ELSEIF(NUMINC.EQ.3)THEN
          AINC2=AINC/3.
        ELSEIF(NUMINC.EQ.4)THEN
          AINC2=AINC/2.
        ELSEIF(NUMINC.EQ.5)THEN
          AINC2=AINC/2.
        ELSEIF(NUMINC.EQ.6)THEN
          AINC2=AINC/2.
        ENDIF
        ASTRT=FMIN - 3.0*AINC2
        ASTOP=FMAX + 3.0*AINC2
        N4=1
        AVAL=ASTRT
        YCONT(1)=AVAL
  307   CONTINUE
          AVAL=AVAL+AINC2
          IF(AVAL.GT.ASTOP .OR. NCONT.GE.MAXOBV)GOTO309
          N4=N4+1
          YCONT(N4)=AVAL
          GOTO307
  309   CONTINUE
C
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,321)
  321     FORMAT('***** AFTER AUTOMATIC GENERATION OF CONTOUR LEVELS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,322)FMIN,FMAX,RANTIC,SIGDIG
  322     FORMAT('FMIN,FMAX,RANTIC,SIGDIG = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,324)NUMINC,N4,AINC,AINC2,ASTRT,ASTOP
  324     FORMAT('NUMINC,N4,AINC,AINC2,ASTRT,ASTOP = ',2I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
          DO330I=1,NS
            WRITE(ICOUT,335)I,YCONT(I)
  335       FORMAT('I,YCONT(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  330     CONTINUE
        ENDIF
C
      ENDIF
C
C     CHECK FOR "PLOTLEFT", "PLOTRIGH", "PLOTBOTT", "PLOTTOP"
C
      PLOTLE=-2.0
      PLOTRI=2.0
      PLOTBO=-2.0
      PLOTTO=2.0
      YID=1.0
C
      IH1='PLOT'
      IH2='LEFT'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH1,IH2,IHWUSE,
     1IH1,IH2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'NO')THEN
        PLOTLE=VALUE(ILOC)
      ENDIF
C
      IH1='PLOT'
      IH2='RIGH'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH1,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'NO')THEN
        PLOTRI=VALUE(ILOC)
      ENDIF
C
      IH1='PLOT'
      IH2='BOTT'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH1,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'NO')THEN
        PLOTBO=VALUE(ILOC)
      ENDIF
C
      IH1='PLOT'
      IH2='TOP '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH1,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'NO')THEN
        PLOTTO=VALUE(ILOC)
      ENDIF
C
      IH1='YID '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH1,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'NO')THEN
        YID=VALUE(ILOC)
      ENDIF
C
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,5001)NS,N4,ICASPL
 5001   FORMAT('NS,N4,ICASPL=',2I8,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      MAXNXT=MAXOBV
      CALL DPDCN2(Z,X1,X2,YCONT,NS,N4,ICASPL,
     1            PRED2,RES2,ZTEMP,U1JUNK,TEMP1,X1TEMP,X2TEMP,
     1            Y,X,D,X3D,
     1            PLOTLE,PLOTRI,PLOTBO,PLOTTO,YID,AINC2,MAXNXT,
     1            B1,B2,B12,YCP,YSD,NCENT,
     1            STATVA,NCDF,CUTL95,CUTU95,
     1            IDCPDI,IDCPFI,
     1            N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
      NPLOTP=N2
C
C               ***************************************
C               **  STEP 9--                         **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
 7000 CONTINUE
C
      ISTEPN='9'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
      IREPU='OFF'
      IRESU='OFF'
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NS,
     1IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,ILOCN,IBUGG3,IERROR)
C
      ISUBN0='DCNT'
C
      IH='B1  '
      IH2='    '
      VALUE0=B1
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='B2  '
      IH2='    '
      VALUE0=B2
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='B12 '
      IH2='    '
      VALUE0=B12
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='STAT'
      IH2='VAL '
      VALUE0=STATVA
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='STAT'
      IH2='NU  '
      VALUE0=REAL(NCDF)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='CUTL'
      IH2='OW95'
      VALUE0=CUTL95
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='CUTU'
      IH2='PP95'
      VALUE0=CUTU95
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='NCEN'
      IH2='TER '
      VALUE0=REAL(NCENT)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='MCEN'
      IH2='TER '
      VALUE0=YCP
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
      IH='SDCE'
      IH2='NTER'
      VALUE0=YSD
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG2,IERROR)
C
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DCNT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDCNT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDCN2(Z,X1,X2,YCONT,NZ,NCONT,ICASPL,
     1                  PRED,RES,ZTEMP,U1JUNK,TEMP1,X1TEMP,X2TEMP,
     1                  Y,X,D,X3D,
     1                  PLOTLE,PLOTRI,PLOTBO,PLOTTO,YID,AINC2,MAXNXT,
     1                  B1,B2,B12,YCP,YSD,NCENT,
     1                  TESTST,NCDF,CUTOF1,CUTOF2,
     1                  IDCPDI,IDCPFI,
     1                  N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A DEX CONTOUR PLOT
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/12
C     ORIGINAL VERSION--DECEMBER  1999.
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IDCPDI
      CHARACTER*4 IDCPFI
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICONC
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
C
C---------------------------------------------------------------------
C
      DIMENSION Z(*)
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION YCONT(*)
C
      DIMENSION PRED(*)
      DIMENSION RES(*)
      DIMENSION ZTEMP(*)
      DIMENSION U1JUNK(*)
      DIMENSION TEMP1(*)
      DIMENSION X1TEMP(*)
      DIMENSION X2TEMP(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION X3D(*)
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
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDCN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,NZ,NCONT,NUMV2,MAXNXT
   52   FORMAT('ICASPL,NZ,NCONT,NUMV2,MAXNXT = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)PLOTLE,PLOTRI,PLOTBO,PLOTTO
   55   FORMAT('PLOTLE,PLOTRI,PLOTBO,PLOTTO = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO60I=1,NZ
          WRITE(ICOUT,65)I,Z(I),X1(I),X2(I)
   65     FORMAT('I,Z(I),X1(I),X2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
        DO70I=1,NCONT
          WRITE(ICOUT,75)I,YCONT(I)
   75     FORMAT('I,YCONT(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
C
      ISUBN1='DPDC'
      ISUBN2='N2  '
      IWRITE='OFF'
      IERROR='NO'
C
      NCENT=0
      YCP=0.0
      YSD=0.0
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NZ.LT.4)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN DEX CONTOUR PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 4.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)NZ
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  PLOT INNER SAMPLING SQUARE        **
C               **  AND A CENTER POINT (IF IT EXISTS) **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      EPS=0.001
      XLEFT=-1.1
      XRIGHT=1.1
      XMID=0.1
      YLOW0=-0.95
      YUPP0=1.05
      YMID0=0.0
      YDEL=0.0
      IF(ABS(YID-1.0).LT.EPS)YDEL=0.0
      IF(ABS(YID-2.0).LT.EPS)YDEL=0.15
      YLOW=YLOW0 - YDEL
      YUPP=YUPP0 - YDEL
      YMID=YMID0 - YDEL
      ITAG=0
      N2=0
C
C     THE X = 1 AND THE CENTER POINT ARE LEFT JUSTIFIED WHILE THE
C     X = -1 ARE RIGHT JUSTIFIED, SO GIVE THESE SEPARATE TAG VALUES.
C
      ITAG=ITAG+1
      ATOL=0.00001
      J=0
      DO510I=1,NZ
        X1VAL=ABS(X1(I)-(-1.0))
        X2VAL=ABS(X2(I)-(-1.0))
        IF(ABS(X1VAL).LE.ATOL.AND.ABS(X2VAL).LE.ATOL)THEN
          J=J+1
          X1TEMP(J)=X1(I)
          X2TEMP(J)=X2(I)
          ZTEMP(J)=Z(I)
        ENDIF
  510 CONTINUE
      NPTS=J
      IF(NPTS.GT.0)THEN
        CALL MEAN(ZTEMP,NPTS,IWRITE,YMM,IBUGG3,IERROR)
        N2=N2+1
        X(N2)=XLEFT
        Y(N2)=YLOW
        D(N2)=REAL(ITAG)
        X3D(N2)=YMM
      ENDIF
C
      J=0
      DO530I=1,NZ
        X1VAL=ABS(X1(I)-(-1.0))
        X2VAL=ABS(X2(I)-(1.0))
        IF(ABS(X1VAL).LE.ATOL.AND.ABS(X2VAL).LE.ATOL)THEN
          J=J+1
          X1TEMP(J)=X1(I)
          X2TEMP(J)=X2(I)
          ZTEMP(J)=Z(I)
        ENDIF
  530 CONTINUE
      NPTS=J
      IF(NPTS.GT.0)THEN
        CALL MEAN(ZTEMP,NPTS,IWRITE,YMP,IBUGG3,IERROR)
        N2=N2+1
        X(N2)=XLEFT
        Y(N2)=YUPP
        D(N2)=REAL(ITAG)
        X3D(N2)=YMP
      ENDIF
C
      ITAG=ITAG+1
      J=0
      DO520I=1,NZ
        X1VAL=ABS(X1(I)-(1.0))
        X2VAL=ABS(X2(I)-(-1.0))
        IF(ABS(X1VAL).LE.ATOL.AND.ABS(X2VAL).LE.ATOL)THEN
          J=J+1
          X1TEMP(J)=X1(I)
          X2TEMP(J)=X2(I)
          ZTEMP(J)=Z(I)
        ENDIF
  520 CONTINUE
      NPTS=J
      IF(NPTS.GT.0)THEN
        CALL MEAN(ZTEMP,NPTS,IWRITE,YPM,IBUGG3,IERROR)
        N2=N2+1
        X(N2)=XRIGHT
        Y(N2)=YLOW
        D(N2)=REAL(ITAG)
        X3D(N2)=YPM
      ENDIF
C
      J=0
      DO540I=1,NZ
        X1VAL=ABS(X1(I)-(1.0))
        X2VAL=ABS(X2(I)-(1.0))
        IF(ABS(X1VAL).LE.ATOL.AND.ABS(X2VAL).LE.ATOL)THEN
          J=J+1
          X1TEMP(J)=X1(I)
          X2TEMP(J)=X2(I)
          ZTEMP(J)=Z(I)
        ENDIF
  540 CONTINUE
      NPTS=J
      IF(NPTS.GT.0)THEN
        CALL MEAN(ZTEMP,NPTS,IWRITE,YPP,IBUGG3,IERROR)
        N2=N2+1
        X(N2)=XRIGHT
        Y(N2)=YUPP
        D(N2)=REAL(ITAG)
        X3D(N2)=YPP
      ENDIF
C
      ATOL=0.00001
      J=0
      DO400I=1,NZ
        X1VAL=ABS(X1(I))
        X2VAL=ABS(X2(I))
        IF(ABS(X1VAL).LE.ATOL.AND.ABS(X2VAL).LE.ATOL)THEN
          J=J+1
          X1TEMP(J)=X1(I)
          X2TEMP(J)=X2(I)
          ZTEMP(J)=Z(I)
        ENDIF
  400 CONTINUE
      NCENT=J
      IF(NCENT.GT.0)THEN
        CALL MEAN(ZTEMP,NCENT,IWRITE,YCP,IBUGG3,IERROR)
        CALL SD(ZTEMP,NCENT,IWRITE,YSD,IBUGG3,IERROR)
        N2=N2+1
        X(N2)=XMID
        Y(N2)=YMID
        D(N2)=REAL(ITAG)
        X3D(N2)=YCP
C
        N2=N2+1
        ITAG=ITAG+1
        X(N2)=0.0
        Y(N2)=0.0
        D(N2)=REAL(ITAG)
      ENDIF
C
      ITAG=ITAG+1
      N2=N2+1
      X(N2)=-1.0
      Y(N2)=-1.0
      D(N2)=REAL(ITAG)
      X3D(N2)=0.0
      N2=N2+1
      X(N2)=1.0
      Y(N2)=-1.0
      D(N2)=REAL(ITAG)
      X3D(N2)=0.0
      N2=N2+1
      X(N2)=1.0
      Y(N2)=1.0
      D(N2)=REAL(ITAG)
      X3D(N2)=0.0
      N2=N2+1
      X(N2)=-1.0
      Y(N2)=1.0
      D(N2)=REAL(ITAG)
      X3D(N2)=0.0
      N2=N2+1
      X(N2)=-1.0
      Y(N2)=-1.0
      D(N2)=REAL(ITAG)
      X3D(N2)=0.0
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')THEN
        WRITE(ICOUT,411)N2
  411   FORMAT('AFTER STEP 2: N2 = ', I5)
        CALL DPWRST('XXX','BUG ')
        DO420I=1,N2
          WRITE(ICOUT,421)I,X(I),Y(I),D(I)
  421     FORMAT('I,X(I),Y(I),D(I) = ',I5,3G15.7)
          CALL DPWRST('XXX','BUG ')
  420   CONTINUE
      ENDIF
C
C
C               ****************************************
C               **  STEP 3--                          **
C               **  EXTRACT POINTS WHERE X1, X2 ARE   **
C               **  EQUAL TO +/- 1.                   **
C               **  COMPUTE B1, B2, B12               **
C               ****************************************
C
      ISTEPN='3'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ATOL=0.00001
      J=0
      DO100I=1,NZ
        X1VAL=ABS(X1(I))
        X2VAL=ABS(X2(I))
        IF(ABS(X1VAL-1.0).LE.ATOL.AND.ABS(X2VAL-1.0).LE.ATOL)THEN
          J=J+1
          X1TEMP(J)=X1(I)
          X2TEMP(J)=X2(I)
          ZTEMP(J)=Z(I)
        ENDIF
  100 CONTINUE
      IF(J.LE.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        WRITE(ICOUT,113)
  113   FORMAT('      NONE OF THE X1, X2 PAIRS EQUAL TO +/- 1')
        IERROR='YES'
        GOTO9000
      ENDIF
      NEDGE=J
C
      CALL MEAN(ZTEMP,NEDGE,IWRITE,AMU,IBUGG3,IERROR)
      AMEDGE=AMU
      DO120I=1,NEDGE
        TEMP1(I)=Z(I)*X1TEMP(I)
  120 CONTINUE
      CALL MEAN(TEMP1,NEDGE,IWRITE,AMU2,IBUGG3,IERROR)
      B1=2.0*AMU2
C
      DO130I=1,NEDGE
        TEMP1(I)=Z(I)*X2TEMP(I)
  130 CONTINUE
      CALL MEAN(TEMP1,NEDGE,IWRITE,AMU2,IBUGG3,IERROR)
      B2=2.0*AMU2
C
      DO140I=1,NEDGE
        TEMP1(I)=Z(I)*X1TEMP(I)*X2TEMP(I)
  140 CONTINUE
      CALL MEAN(TEMP1,NEDGE,IWRITE,AMU2,IBUGG3,IERROR)
      B12=2.0*AMU2
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')THEN
        WRITE(ICOUT,151)B1,B2,B12
  151   FORMAT('AFTER STEP 3: B1,B2,B12 = ', 3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 4--                          **
C               **  COMPUTE RESIDUALS, PREDICTED      **
C               **  VALUES EVERYWHERE                 **
C               ****************************************
C
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO210I=1,NZ
        PRED(I)=AMU+0.5*(B1*X1(I)+B2*X2(I)+B12*X1(I)*X2(I))
        RES(I)=Z(I)-PRED(I)
  210 CONTINUE
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')THEN
        DO220I=1,NZ
          WRITE(ICOUT,221)I,PRED(I),RES(I)
  221     FORMAT('I,PRED(I),RES(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
  220   CONTINUE
      ENDIF
C
C               ****************************************
C               **  STEP 5--                          **
C               **  GENERATE THE CONTOUR VALUES       **
C               ****************************************
C
      ISTEPN='5'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     NOTE: ALTHOUGH THIS CODE EXISTS IN THE 10-STEP MACROS, IT
C           IS NOT ACTUALLY ACTIVATED (THE NEW CONTOUR LEVELS ARE
C           ONLY CREATED IF YCONT DOES NOT PREVIOUSLY EXIST, BUT
C           DEXCP.DP ALWAYS CREATES YCONT BEFORE THIS CODE IS
C           CALLED.  LEAVE IN, BUT COMMENTED OUT, IN CASE WE NEED
C           TO RE-ACTIVATE IT AT A LATER DATE).
C
CCCCC IF(AINC2.GT.0.0)THEN
C
CCCCC   AMXCOR=YMM
CCCCC   IF(YMP.GT.AMXCOR)AMXCOR=YMP
CCCCC   IF(YPM.GT.AMXCOR)AMXCOR=YPM
CCCCC   IF(YPP.GT.AMXCOR)AMXCOR=YPP
C
CCCCC   AMNCOR=YMM
CCCCC   IF(YMP.LT.AMNCOR)AMNCOR=YMP
CCCCC   IF(YPM.LT.AMNCOR)AMNCOR=YPM
CCCCC   IF(YPP.LT.AMNCOR)AMNCOR=YPP
C
CCCCC   DELCOR=AMXCOR - AMNCOR
CCCCC   AINCCR=DELCOR/5.0
C
CCCCC   IF(AINCCR.LT.AINC2)THEN
CCCCC     ALOW=AMNCOR - 2*DELCOR
CCCCC     AUPP=AMXCOR + 2*DELCOR
CCCCC     NCONT=1
CCCCC     AVAL=ALOW
CCCCC     YCONT(NCONT)=AVAL
CC250     CONTINUE
CCCCC       AVAL=AVAL+AINCCR
CCCCC       IF(AVAL.GT.AUPP .OR. NCONT.GE.MAXNXT)GOTO259
CCCCC       NCONT=NCONT+1
CCCCC       YCONT(NCONT)=AVAL
CCCCC       GOTO250
CC259     CONTINUE
CCCCC   ENDIF
C
CCCCC   IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')THEN
CCCCC     WRITE(ICOUT,261)AINC2,AMXCOR,AMNCOR,DELCOR,AINCCR
CC261     FORMAT('AINC2,AMXCOR,AMNCOR,DELCOR,AINCCR = ',5G15.7)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,263)ALOW,AUPP,NCONT
CC263     FORMAT('ALOW,AUPP,NCONT = ',2G15.7,I8)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     DO265I=1,NCONT
CCCCC       WRITE(ICOUT,268)I,YCONT(I)
CC268       FORMAT('I,YCONT(I) = ',I8,G15.7)
CCCCC       CALL DPWRST('XXX','BUG ')
CC265     CONTINUE
CCCCC   ENDIF
C
CCCCC ENDIF
C
      IF(PLOTLE.LT.PLOTRI)THEN
        ASTRT=PLOTLE
        ASTOP=PLOTRI
      ELSE
        ASTRT=-2.0
        ASTOP=2.0
      ENDIF
CCCCC AINC=0.025
      AINC=0.05
      AVAL=ASTRT
      ICNT=1
      U1JUNK(ICNT)=AVAL
  310 CONTINUE
        AVAL=AVAL+AINC
        IF(AVAL.GT.ASTOP .OR. ICNT.GE.MAXNXT)GOTO311
        ICNT=ICNT+1
        U1JUNK(ICNT)=AVAL
        GOTO310
  311 CONTINUE
      NTEMP=ICNT
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')THEN
        WRITE(ICOUT,321)NTEMP,ASTRT,ASTOP,AINC
  321   FORMAT('NTEMP,ASTRT,ASTOP,AINC = ', I8,3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO325I=1,NTEMP
          WRITE(ICOUT,328)I,U1JUNK(I)
  328     FORMAT('I,U1JUNK(I) = ',I5,G15.7)
          CALL DPWRST('XXX','BUG ')
  325   CONTINUE
      ENDIF
C
C     NOW GENERATE THE CONTOUR LINES
C
      ISTEPN='5A'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL SORT(YCONT,NCONT,YCONT)
      IF(IDCPDI.EQ.'MINI')THEN
        IFRST=1
        ILAST=NCONT
        INC=1
      ELSE
        IFRST=NCONT
        ILAST=1
        INC=-1
      ENDIF
C
C     WRITE CONTOUR VALUE TAG TO DPST1F.DAT
C
      IOP='OPEN'
      IFLG11=1
      IFLG21=0
      IFLG31=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
C
      DO330ICONT=IFRST,ILAST,INC
        Y0=YCONT(ICONT)
        ATEMP=2.0*(Y0-AMU)
        ITAG=ITAG+1
        ICNT=0
        DO340I=1,NTEMP
          ANUM=ATEMP-B1*U1JUNK(I)
          ADEN=B2+B12*U1JUNK(I)
          IF(ADEN.EQ.0.0)GOTO340
          AVAL=ANUM/ADEN
          IF(AVAL.GE.PLOTLE .AND. AVAL.LE.PLOTRI)ICNT=ICNT+1
CCCCC     IF(AVAL.GE.PLOTLE .AND. AVAL.LE.PLOTRI)THEN
            N2=N2+1
            Y(N2)=AVAL
            X(N2)=U1JUNK(I)
            D(N2)=REAL(ITAG)
            X3D(N2)=0.0
CCCCC     ENDIF
  340   CONTINUE
        IF(IERROR.EQ.'NO'.AND.ICNT.GT.0)
     1     WRITE(IOUNI1,'(E15.7,2F10.0)')Y0,REAL(ITAG),REAL(ICNT)
  330 CONTINUE
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
C
C               ****************************************
C               **  STEP 6--                          **
C               **  GENERATE THE T-TEST FOR CURVATURE **
C               ****************************************
C
      ISTEPN='6'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NCENT.GE.2.AND.NEDGE.GE.1)THEN
        STATNM=0.0
        STATDN=0.0
        TESTST=0.0
        NCDF=0
        STATNM=AMEDGE-YCP
        AJUNK1=1.0/REAL(NEDGE)
        AJUNK2=1.0/REAL(NCENT)
        STATDN=YSD*SQRT(AJUNK1+AJUNK2)
        IF(STATDN.EQ.0.0)GOTO699
        TESTST=STATNM/STATDN
        NCDF=NCENT-1
        AP=0.975
        CALL TPPF(AP,REAL(NCDF),CUTOF2)
        CUTOF1=-CUTOF2
        ICONC='NO'
        IF(TESTST.LT.CUTOF1 .OR. TESTST.GT.CUTOF2)ICONC='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,611)
  611   FORMAT('----- DEX CONTOUR PLOT TEST FOR CURVATURE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,613)NCENT
  613   FORMAT('      NUMBER OF CENTER POINTS             =  ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,615)YCP
  615   FORMAT('      MEAN OF CENTER POINTS                =  ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,617)YSD
  617   FORMAT('      STANDARD DEVIATION OF CENTER POINTS  =  ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,623)NEDGE
  623   FORMAT('      NUMBER OF EDGE POINTS                =  ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,625)AMEDGE
  625   FORMAT('      MEAN OF EDGE POINTS                  =  ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,631)TESTST
  631   FORMAT('      CURVATURE CHECK: T TEST STATISTIC    =  ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,633)NCDF
  633   FORMAT('      T DEGREES OF FREEDOM                 =  ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,635)CUTOF1
  635   FORMAT('      LOWER T CRITICAL VALUE               =  ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,637)CUTOF2
  637   FORMAT('      UPPER T CRITICAL VALUE               =  ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(ICONC.EQ.'NO')THEN
        WRITE(ICOUT,641)
  641   FORMAT('      CONCLUSION: THERE IS NO CURVATURE')
        CALL DPWRST('XXX','BUG ')
        ELSE
        WRITE(ICOUT,643)
  643   FORMAT('      CONCLUSION: THERE IS  CURVATURE')
        CALL DPWRST('XXX','BUG ')
        ENDIF
  699   CONTINUE
      ELSE
        NDF=0
        TESTST=0.0
        CUTOF1=0.0
        CUTOF2=0.0
      ENDIF
 8000 CONTINUE
      NPLOTV=3
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DCN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDCN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,N2,NPLOTV
 9012   FORMAT('ICASPL,N2,NPLOTV = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,N2
          WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036     FORMAT('I,Y(I),X(I),D(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDECL(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1                  IDEFDC,
     1                  NUMDEV,MAXDEV,
     1                  IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IBUGO2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE COLOR STATUS (ON/OFF) FOR AN OUTPUT DEVICE.
C              THE COLOR (ON/OFF) FOR DEVICE I
C              WILL BE PLACED IN THE I-TH ELEMENT OF THE CHARACTER
C              VECTOR IDCOLO(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFDC
C                     --MAXDEV
C     OUTPUT ARGUMENTS--IDCONT (A CHARACTER VECTOR
C                              WHOSE I-TH ELEMENT CONTAINS THE
C                              COLOR (ON/OFF) FOR DEVICE I.
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
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IDEFDC
C
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
C
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDCOLO
C
      CHARACTER*4 IBUGO2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
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
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
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
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'COLO')GOTO1140
      GOTO9000
C
 1110 CONTINUE
      IF(NUMARG.LE.1)GOTO1120
      IF(IHARG(2).EQ.'ON')GOTO1120
      IF(IHARG(2).EQ.'OFF')GOTO1125
      IF(IHARG(2).EQ.'AUTO')GOTO1120
      IF(IHARG(2).EQ.'DEFA')GOTO1127
      GOTO1120
C
 1120 CONTINUE
      IHOLD='ON'
      GOTO1130
C
 1125 CONTINUE
      IHOLD='OFF'
      GOTO1130
C
 1127 CONTINUE
      IHOLD=IDEFDC
      GOTO1130
C
 1130 CONTINUE
      IFOUND='YES'
      DO1135I=1,NUMDEV
      IDCOLO(I)=IHOLD
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)IHOLD
 1136 FORMAT('THE COLOR FOR ALL DEVICES HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO9000
C
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GOTO1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPDECL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE DEVICE ... COLOR COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE DEVICE IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      DEVICE 3 COLOR ON')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXDEV)GOTO1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPDECL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE DEVICE ... COLOR COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF DEVICES MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXDEV
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',
     1'DEVICE.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1160 CONTINUE
      IF(NUMARG.LE.2)GOTO1170
      IF(IHARG(3).EQ.'ON')GOTO1170
      IF(IHARG(3).EQ.'OFF')GOTO1175
      IF(IHARG(3).EQ.'AUTO')GOTO1170
      IF(IHARG(3).EQ.'DEFA')GOTO1177
      GOTO1170
C
 1170 CONTINUE
      IHOLD='ON'
      GOTO1180
C
 1175 CONTINUE
      IHOLD='OFF'
      GOTO1180
C
 1177 CONTINUE
      IHOLD=IDEFDC
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IDCOLO(I)=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1199
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)I
 1181 FORMAT('            DEVICE           --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IDUNIT(I)
 1182 FORMAT('            I/O UNIT         --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)IDMANU(I)
 1183 FORMAT('            MANUFACTURER     --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)IDMODE(I),IDMOD2(I),IDMOD3(I)
 1184 FORMAT('            MODEL            --',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1185)IDPOWE(I)
 1185 FORMAT('            POWER            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)IDCONT(I)
 1186 FORMAT('            CONTINUITY       --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1187)IDCOLO(I)
 1187 FORMAT('            COLOR            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IDNHPP(I)
 1188 FORMAT('            HORIZONTAL PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1189)IDNVPP(I)
 1189 FORMAT('            VERTICAL   PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
 1199 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPDECN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IDEFCN,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1IBUGO2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE CONTINUITY STATUS (ON/OFF) FOR AN OUTPUT DEVICE.
C              A DEVICE IS CONSIDERED CONTINUOUS IF IT IS CAPABLE
C              OF DRAWING A CONTINUOUS LINE SEGMENT.
C              FOR EXAMPLE, THE TEKTRONIX 4014 IS CONTINUOUS;
C              THE TEXAS INSTRUMENT SILENT 700 IS NOT CONTINUOUS.
C              THE CONTINUITY (ON/OFF) FOR DEVICE I
C              WILL BE PLACED IN THE I-TH ELEMENT OF THE CHARACTER
C              VECTOR IDCONT(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFCN
C                     --MAXDEV
C     OUTPUT ARGUMENTS--IDCONT (A CHARACTER VECTOR
C                              WHOSE I-TH ELEMENT CONTAINS THE
C                              CONTINUITY (ON/OFF) FOR DEVICE I.
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IDEFCN
C
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
C
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDCOLO
C
      CHARACTER*4 IBUGO2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
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
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
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
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONT')GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'CONT')GOTO1140
      GOTO9000
C
 1110 CONTINUE
      IF(NUMARG.LE.1)GOTO1120
      IF(IHARG(2).EQ.'ON')GOTO1120
      IF(IHARG(2).EQ.'OFF')GOTO1125
      IF(IHARG(2).EQ.'AUTO')GOTO1120
      IF(IHARG(2).EQ.'DEFA')GOTO1127
      GOTO1120
C
 1120 CONTINUE
      IHOLD='ON'
      GOTO1130
C
 1125 CONTINUE
      IHOLD='OFF'
      GOTO1130
C
 1127 CONTINUE
      IHOLD=IDEFCN
      GOTO1130
C
 1130 CONTINUE
      IFOUND='YES'
      DO1135I=1,NUMDEV
      IDCONT(I)=IHOLD
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)IHOLD
 1136 FORMAT('THE CONTINUITY FOR ALL DEVICES HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO9000
C
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GOTO1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPDECN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE DEVICE ... CONTINUITY COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE DEVICE IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      DEVICE 3 CONTINUITY ON')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXDEV)GOTO1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPDECN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE DEVICE ... CONTINUITY COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF DEVICES MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXDEV
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',
     1'DEVICE.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1160 CONTINUE
      IF(NUMARG.LE.2)GOTO1170
      IF(IHARG(3).EQ.'ON')GOTO1170
      IF(IHARG(3).EQ.'OFF')GOTO1175
      IF(IHARG(3).EQ.'AUTO')GOTO1170
      IF(IHARG(3).EQ.'DEFA')GOTO1177
      GOTO1170
C
 1170 CONTINUE
      IHOLD='ON'
      GOTO1180
C
 1175 CONTINUE
      IHOLD='OFF'
      GOTO1180
C
 1177 CONTINUE
      IHOLD=IDEFCN
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IDCONT(I)=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1199
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)I
 1181 FORMAT('            DEVICE           --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IDUNIT(I)
 1182 FORMAT('            I/O UNIT         --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)IDMANU(I)
 1183 FORMAT('            MANUFACTURER     --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)IDMODE(I),IDMOD2(I),IDMOD3(I)
 1184 FORMAT('            MODEL            --',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1185)IDPOWE(I)
 1185 FORMAT('            POWER            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)IDCONT(I)
 1186 FORMAT('            CONTINUITY       --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1187)IDCOLO(I)
 1187 FORMAT('            COLOR            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IDNHPP(I)
 1188 FORMAT('            HORIZONTAL PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1189)IDNVPP(I)
 1189 FORMAT('            VERTICAL   PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
 1199 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPDECO(IANS,IWIDTH,IHARG,NUMARG,
     1IDEFCM,IWIDDC,IDEFC,IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--EXTRACT THE STRING TO BE USED AS A DEFAULT COMMAND;
C              SAVE THIS STRING FOR USE IN MAIN
C              WHEN NO MATCH FORUND FOR A GIVEN COMMAND.
C     NOTE--A CHECK IS CONTAINED HEREIN WHICH RESTRICTS
C           THE MAXIMUM NUMBER OF CHARACTERS IN THE DEFAULT
C           COMMAND TO BE 40 CHARACTERS.
C     INPUT  ARGUMENTS--IANS   (A  HOLLERITH VECTOR)
C                     --IWIDTH
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--IDEFCM
C                     --IWIDDC
C                     --IDEFC
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE       1981.
C     UPDATED         --SEPTEMBER  1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCM
      CHARACTER*4 IDEFC
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
      DIMENSION IHARG(*)
C
      DIMENSION IDEFC(*)
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
      IF(IBUGS2.NE.'ON')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPDECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFCM,IWIDDC
   53 FORMAT('IDEFCM,IWIDDC = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)(IDEFC(I),I=1,IWIDDC)
   54 FORMAT('IDEFC(.)--',120A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE SECOND WORD (COMMAND)  **
C               **  IN THE ASSUMED COMMAND STRING        **
C               **  (DEFAULT COMMNAD)                    **
C               *******************************************
C
      DO100I=1,IWIDTH
      I2=I
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
      IP6=I+6
      IF(IANS(I).EQ.'C'.AND.IANS(IP1).EQ.'O'
     1.AND.IANS(IP2).EQ.'M'.AND.IANS(IP3).EQ.'M'
     1.AND.IANS(IP4).EQ.'A'.AND.IANS(IP5).EQ.'N'
     1.AND.IANS(IP6).EQ.'D')
     1GOTO190
C
  100 CONTINUE
      WRITE(ICOUT,101)
  101 FORMAT('***** ERROR IN DPDECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,102)
  102 FORMAT('      THE WORD   COMMAND   NOT FOUND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO800
  190 CONTINUE
C
C               **********************************************************
C               **  STEP 2--                                            **
C               **  DEFINE THE START POSITION (ISTART) FOR THE STRING.  **
C               **  DEFINE THE STOP POSITION (ISTOP) FOR THE STRING.  **
C               ********************************************************
C
      IFOUND='YES'
      ISTART=I2+8
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
C               *****************************************
C               **  STEP 4--                           **
C               **  COPY OVER THE STRING OF INTEREST.  **
C               *****************************************
C
      IF(NUMARG.LE.1)GOTO359
      IF(NUMARG.LE.2.AND.IHARG(NUMARG).EQ.'ON')GOTO359
      IF(NUMARG.LE.2.AND.IHARG(NUMARG).EQ.'OFF')GOTO359
      IF(NUMARG.LE.2.AND.IHARG(NUMARG).EQ.'AUTO')GOTO359
      IF(NUMARG.LE.2.AND.IHARG(NUMARG).EQ.'DEFA')GOTO359
C
      IF(ISTART.GT.ISTOP)GOTO359
      IF(ISTOP.EQ.0)GOTO359
      J=0
      DO350I=ISTART,ISTOP
      J=J+1
      IDEFC(J)=IANS(I)
      IF(J.GE.40)GOTO355
  350 CONTINUE
  355 CONTINUE
      IDEFCM='ON'
      IWIDDC=J
      GOTO800
  359 CONTINUE
C
C               ************************************
C               **  STEP 5--                      **
C               **  TREAT THE EMPTY-STRING CASE.  **
C               ************************************
C
      IDEFCM='OFF'
      IWIDDC=0
      DO410I=1,40
      IDEFC(I)='    '
  410 CONTINUE
      GOTO800
C
C               ***************************
C               **  STEP 6--             **
C               **  PRINT OUT A MESSAGE  **
C               ***************************
C
  800 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO819
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)
  811 FORMAT('THE DEFAULT COMMAND HAS JUST BEEN SET TO')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDDC.EQ.0)WRITE(ICOUT,999)
      IF(IWIDDC.EQ.0)CALL DPWRST('XXX','BUG ')
      IF(IWIDDC.GE.1)WRITE(ICOUT,812)(IDEFC(I),I=1,IWIDDC)
  812 FORMAT(10X,120A1)
      IF(IWIDDC.GE.1)CALL DPWRST('XXX','BUG ')
  819 CONTINUE
      GOTO9000
C
C               ****************
C               **  STEP 7--  **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.NE.'ON')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPDECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IDEFCM,IWIDDC
 9012 FORMAT('IDEFCM,IWIDDC(1) = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)(IDEFC(I),I=1,IWIDDC)
 9013 FORMAT('IDEFC(.) --',120A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IFOUND,IERROR
 9014 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDEDE(IHARG,IARG,NUMARG,IDEDED,
     1IDEXDE,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE DESIGN OF EXPERIMENT PLOT DEPTH
C              INTO THE INTERACTION TERMS
C              1 = MAIN EFFECTS ONLY
C              2 = UP TO 2-TERM INTERACTIONS
C              3 = UP TO 3-TERM INTERACTIONS
C              ETC.
C     INPUT  ARGUMENTS--IHARG  (A HOLLARITH VECTOR)
C                     --IARG    (AN INTEGER VECTOR)
C                     --NUMARG
C                     --IDEDED
C     OUTPUT ARGUMENTS--IDEXDE
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
C     VERSION NUMBER--89/5
C     ORIGINAL VERSION--MAY       1989.
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
      DIMENSION IARG(*)
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
 1100 CONTINUE
      IF(NUMARG.EQ.1)GOTO1150
      IF(IHARG(2).EQ.'ON')GOTO1150
      IF(IHARG(2).EQ.'OFF')GOTO1150
      IF(IHARG(2).EQ.'AUTO')GOTO1150
      IF(IHARG(2).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEDED
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IDEXDE=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE DESIGN OF EXPERIMENT DEPTH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('(INTO THE INTERACTION TERMS)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)IHOLD
 1183 FORMAT('HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPDEDL(Y,X,PX,NP,NUMSET,
     1                  ICASPL,ICAS3D,
     1                  ISPISW,ASPIBA,MAXSPI,
     1                  IBARSW,ABARBA,ABARWI,MAXBAR,XDELMN,
     1                  GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1                  GX2MIN,GX2MAX,GY2MIN,GY2MAX,
     1                  IX1MIN,IX1MAX,IY1MIN,IY1MAX,
     1                  IX2MIN,IX2MAX,IY2MIN,IY2MAX,
     1                  DX1MIN,DX1MAX,DY1MIN,DY1MAX,
     1                  DX2MIN,DX2MAX,DY2MIN,DY2MAX,
     1                  IHORSW,ICHATY,ILINTY,DTAG,XIDTEM,
     1                  PXMIN,PXMAX,PYMIN,PYMAX,
     1                  ITICUN,PX1TOL,PX1TOR,PY1TOB,PY1TOT)
C
C     PURPOSE--COMPUTE ACTUAL DATA LIMITS FOR POTENTIAL USE IN SETTING
C              LIMITS FOR ALL 4 FRAME LINES
C     NOTE--IN THE EVENT THAT THE FRAME LIMITS HAVE BEEN FIXED (AS OPPOSED
C           TO FLOATING), THEN COMPUTE THE ACTUAL DATA LIMITS ONLY FOR
C           THOSE DATA POINTS RESIDING WITHIN THE FIXED LIMITS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION--MAY         1983.
C     UPDATED         --SEPTEMBER   2011. BUG WITH HISTOGRAMS AND
C                                         HORIZONTAL SWITCH ON
C     UPDATED         --JANUARY     2018. SUPPORT FOR SCREEN UNITS FOR LINE
C                                         AND CHARACTER TRACES.  FOR THESE
C                                         CASES, DO NOT THIS TRACE TO
C                                         DETERMINE DATA LIMITS.
C     UPDATED         --APRIL       2018. TAKE TIC OFFSET INTO ACCOUNT
C
C-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAS3D
      CHARACTER*4 ITICUN
C
      CHARACTER*4 ICHATY(*)
      CHARACTER*4 ILINTY(*)
C
      CHARACTER*4 IX1MIN
      CHARACTER*4 IX1MAX
      CHARACTER*4 IY1MIN
      CHARACTER*4 IY1MAX
C
      CHARACTER*4 IX2MIN
      CHARACTER*4 IX2MAX
      CHARACTER*4 IY2MIN
      CHARACTER*4 IY2MAX
C
      CHARACTER*4 ISPISW
      CHARACTER*4 IBARSW
      CHARACTER*4 IBAR
      CHARACTER*4 ISAVE
      CHARACTER*4 IHORSW
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION PX(*)
      DIMENSION DTAG(*)
      DIMENSION XIDTEM(*)
C
      DIMENSION ISPISW(*)
      DIMENSION ASPIBA(*)
      DIMENSION IBARSW(*)
      DIMENSION ABARBA(*)
      DIMENSION ABARWI(*)
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
      DEL=-999.0
      AWIDTH=-999.0
      BAMIN=-999.0
      BAMAX=-999.0
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEDL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEDL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NP,NUMSET,MAXSPI,MAXBAR
   52   FORMAT('NP,NUMSET,MAXSPI,MAXBAR = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)ICASPL,ICAS3D,XDELMN
   54   FORMAT('ICASPL,ICAS3D,XDELMN = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NP.GE.1)THEN
          DO57I=1,NP
            WRITE(ICOUT,58)I,X(I),Y(I)
   58       FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   57     CONTINUE
        ENDIF
        WRITE(ICOUT,61)GX1MIN,GY1MIN,GX1MAX,GY1MAX
   61   FORMAT('GX1MIN,GY1MIN,GX1MAX,GY1MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)GX2MIN,GY2MIN,GX2MAX,GY2MAX
   62   FORMAT('GX2MIN,GY2MIN,GX2MAX,GY2MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IX1MIN,IY1MIN,IX1MAX,IY1MAX
   63   FORMAT('IX1MIN,IY1MIN,IX1MAX,IY1MAX = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IX2MIN,IY2MIN,IX2MAX,IY2MAX
   64   FORMAT('IX2MIN,IY2MIN,IX2MAX,IY2MAX = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)DX1MIN,DY1MIN,DX1MAX,DY1MAX
   65   FORMAT('DX1MIN,DY1MIN,DX1MAX,DY1MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,66)DX2MIN,DY2MIN,DX2MAX,DY2MAX
   66   FORMAT('DX2MIN,DY2MIN,DX2MAX,DY2MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        IMAX=NUMSET
        IF(IMAX.GT.MAXSPI)IMAX=MAXSPI
        DO71I=1,IMAX
          WRITE(ICOUT,72)I,ISPISW(I),ASPIBA(I)
   72     FORMAT('I,ISPISW(I),ASPIBA(I) = ',I8,2X,A4,2X,G15.7)
          CALL DPWRST('XXX','BUG ')
   71   CONTINUE
        IMAX=NUMSET
        IF(IMAX.GT.MAXBAR)IMAX=MAXBAR
        DO73I=1,IMAX
          WRITE(ICOUT,74)I,IBARSW(I),ABARBA(I),ABARWI(I)
   74     FORMAT('I,IBARSW(I),ABARBA(I),ABARWI(I)= ',I8,2X,A4,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
        WRITE(ICOUT,89)IBUGG4,ISUBG4,IERRG4,IHORSW
   89   FORMAT('IBUGG4,ISUBG4,IERRG4,IHORSW = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO90I=1,NP
          WRITE(ICOUT,92)I,DTAG(I),XIDTEM(I)
   92     FORMAT('I,DTAG(I),XIDTEM(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   90   CONTINUE
      ENDIF
C
C     2018/10: WHEN USING FIXED LIMITS FOR THE X-AXIS (GX1MIN,GX1MAX),
C              NEED TO INCLUDE THE OFFSET WHEN DETERMING WHAT POINTS
C              ARE INCLUDED IN DETERMING THE MIN AND MAX VALUES FOR THE
C              Y AXIS.  THIS IS AN ISSUE WITH THE DEX MEAN PLOT (AND
C              POSSIBLY OTHERS).  FOR EXAMPLE, WITH 3 FACTORS, WE MIGHT
C              USE "XLIMITS 1 3" AND THEN USE AN OFFSET TO CAPTURE THE
C              THE POINTS LESS THAN 1 OR GREATER THAN 3.  SO IF THE
C              FIRST OR LAST POINT CONTAINS THE MIN/MAX VALUE, THIS
C              MIGHT GET CLIPPED.
C
C              ONLY NEED TO ADJUST IF TIC OFFSETS ARE ACTUALLY GIVEN.
C              ALSO, IF MIN/MAX ARE INFINITY DO NOT APPLY CORRECTIONS.
C
      GY1MNT=GY1MIN
      GY1MXT=GY1MAX
      GX1MNT=GX1MIN
      GX1MXT=GX2MAX
C
      IF(IX1MIN.EQ.'FIXE' .AND. IX1MAX.EQ.'FIXE')THEN
        IF(GX1MIN.NE.CPUMIN .AND. GX1MAX.NE.CPUMAX)THEN
          IF(PX1TOL.GT.0.0 .OR. PX1TOR.GE.0.0)THEN
            IF(ITICUN.EQ.'ABSO')THEN
C
C             TIC OFFSET UNITS IN SCREEN UNITS
C
              AFACT=(GX1MAX - GX1MIN)/(PXMAX - PXMIN)
              IF(PX1TOL.GT.0.0 .AND. PX1TOL.LT.100.0)THEN
                GX1MNT=GX1MIN - AFACT*PX1TOL
              ENDIF
              IF(PX1TOR.GT.0.0 .AND. PX1TOR.LT.100.0)THEN
                GX1MXT=GX1MAX + AFACT*PX1TOR
              ENDIF
            ELSE
C
C             TIC OFFSET UNITS IN DATA UNITS
C
              IF(PX1TOL.GT.0.0 .AND. PX1TOL.LT.100.0)THEN
                GX1MNT=GX1MIN - PX1TOL
              ENDIF
              IF(PX1TOR.GT.0.0 .AND. PX1TOR.LT.100.0)THEN
                GX1MXT=GX1MAX + PX1TOR
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
      IF(IY1MIN.EQ.'FIXE' .AND. IY1MAX.EQ.'FIXE')THEN
        IF(GY1MIN.NE.CPUMIN .AND. GY1MAX.NE.CPUMAX)THEN
          IF(PY1TOB.GT.0.0 .OR. PY1TOT.GE.0.0)THEN
            IF(ITICUN.EQ.'ABSO')THEN
C
C             TIC OFFSET UNITS IN SCREEN UNITS
C
              AFACT=(GY1MAX - GY1MIN)/(PYMAX - PYMIN)
              IF(PY1TOB.GT.0.0 .AND. PY1TOB.LT.100.0)THEN
                GY1MNT=GY1MIN - AFACT*PY1TOB
              ENDIF
              IF(PY1TOT.GT.0.0 .AND. PY1TOT.LT.100.0)THEN
                GY1MXT=GY1MAX + AFACT*PY1TOT
              ENDIF
            ELSE
C
C             TIC OFFSET UNITS IN DATA UNITS
C
              IF(PY1TOL.GT.0.0 .AND. PY1TOL.LT.100.0)THEN
                GY1MNT=GY1MIN - PY1TOB
              ENDIF
              IF(PY1TOR.GT.0.0 .AND. PY1TOR.LT.100.0)THEN
                GY1MXT=GY1MAX + PY1TOR
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEDL')THEN
        WRITE(ICOUT,110)GX1MNT,GX1MXT,GY1MNT,GY1MXT
  110   FORMAT('GX1MNT,GX1MXT,GY1MINT,GY1MXT = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************************************
C               **  STEP 1--                                             **
C               **  IF ANY OF THE BAR SWITCHES ARE ON,                   **
C               **  DETERMINE THE MINIMUM NON-ZERO DIFFERENCE            **
C               **  (XDELMN) BETWEEN X-VARIABLE VALUES.                  **
C               **  THIS VALUE (XDELMN) IS USED TO DEFINE THE BAR WIDTH  **
C               **  IN BAR PLOTS WHEN THE WIDTH IS ALLOWED TO "FLOAT"    **
C               **  WITH THE DATA.                                       **
C               **  THIS VALUE IS USED IN THE DPDRBA SUBROUTINE.         **
C               ***********************************************************
C
      ISAVE=IBARSW(1)
      IBAR='OFF'
      IF(ICASPL.EQ.'HIST')IBARSW(1)='ON'
      IF(ICASPL.EQ.'CUMH')IBARSW(1)='ON'
      IF(ICASPL.EQ.'BARP')IBARSW(1)='ON'
      IF(ICASPL.EQ.'ROOT')IBARSW(1)='ON'
      IF(ICASPL.EQ.'CUMR')IBARSW(1)='ON'
      IF(ICASPL.EQ.'BIHI')IBARSW(1)='ON'
      XDELMN=CPUMAX
      IF(NUMSET.GT.0)THEN
        IMAX=NUMSET
        IF(IMAX.GT.MAXBAR)IMAX=MAXBAR
        DO1010I=1,IMAX
          IF(IBARSW(I).EQ.'ON')THEN
            IBAR='ON'
            GOTO1019
          ENDIF
 1010   CONTINUE
 1019   CONTINUE
C
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEDL')THEN
          WRITE(ICOUT,1011)IHORSW,NP,IBAR
 1011     FORMAT('IHORSW,NP,IBAR=',A4,2X,I4,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C       SEPTEMBER, 1987: IF HORIZONTAL SWITCH IS ON, REVERSE
        IF(IHORSW.EQ.'ON')THEN
          CALL SORT(Y,NP,PX)
        ELSE
          CALL SORT(X,NP,PX)
        ENDIF
        IF(NP.GT.1)THEN
          DO1020I=2,NP
            IM1=I-1
            DEL=PX(I)-PX(IM1)
            IF(DEL.LE.0.0)GOTO1020
            IF(DEL.LT.XDELMN)XDELMN=DEL
 1020     CONTINUE
        ENDIF
C
      ENDIF
C
C               **********************************************
C               **  STEP 2--                                **
C               **  DETERMINE ACTUAL LIMITS FOR X VARIABLE  **
C               **********************************************
C
C     2018/01: IF USER HAS REQUESTED "SCREEN" UNITS FOR EITHER
C              LINE OR CHARACTER TRACE, OMIT THAT DATA FROM
C              THE DETERMINATION OF THE DATA LIMITS.
C
C              THIS ASSUMES THAT XIDTEM IS IN SORTED ORDER.
C
      IFLAG=0
      DO1103I=1,NUMSET
        IF(ICHATY(I)(1:1).EQ.'S' .OR. ILINTY(I)(1:1).EQ.'S')THEN
          IFLAG=1
          GOTO1104
        ENDIF
 1103 CONTINUE
 1104 CONTINUE
C
      TXMIN=CPUMAX
      TXMAX=CPUMIN
C
      XMIN=CPUMAX
      XMAX=CPUMIN
      IF(NP.GE.1)THEN
        IF(IFLAG.EQ.0)THEN
          DO1110I=1,NP
            IF(IX1MIN.EQ.'FIXE'.AND.X(I).LT.GX1MNT)GOTO1110
            IF(IX1MAX.EQ.'FIXE'.AND.X(I).GT.GX1MXT)GOTO1110
CCCCC       JULY 1996.  FOLLOWING CODE EXCLUDES VALUES THAT ARE
CCCCC       "OUT OF RANGE" ON THE Y-AXIS.  SPECIAL CASE WHERE UPPER AND
CCCCC       LOWER LIMIT ARE EQUAL, INCLUDE ALL VALUES.
            IF(GY1MIN.NE.GY1MAX)THEN
              IF(IY1MIN.EQ.'FIXE'.AND.Y(I).LT.GY1MNT)GOTO1110
              IF(IY1MAX.EQ.'FIXE'.AND.Y(I).GT.GY1MXT)GOTO1110
            ENDIF
            IF(X(I).LT.XMIN)XMIN=X(I)
            IF(X(I).GT.XMAX)XMAX=X(I)
 1110     CONTINUE
          TXMIN=XMIN
          TXMAX=XMAX
        ELSE
          DO1120I=1,NP
            DO1125K=1,NUMSET
              AVAL=XIDTEM(K)
              IF(AVAL.EQ.DTAG(I))THEN
                IF(ICHATY(K)(1:1).EQ.'S')GOTO1120
                IF(ILINTY(K)(1:1).EQ.'S')GOTO1120
                IF(IX1MIN.EQ.'FIXE'.AND.X(I).LT.GX1MNT)GOTO1120
                IF(IX1MAX.EQ.'FIXE'.AND.X(I).GT.GX1MXT)GOTO1120
                IF(GY1MNT.NE.GY1MXT)THEN
                  IF(IY1MIN.EQ.'FIXE'.AND.Y(I).LT.GY1MNT)GOTO1120
                  IF(IY1MAX.EQ.'FIXE'.AND.Y(I).GT.GY1MXT)GOTO1120
                ENDIF
                IF(X(I).LT.XMIN)XMIN=X(I)
                IF(X(I).GT.XMAX)XMAX=X(I)
              ENDIF
 1125       CONTINUE
 1120     CONTINUE
          TXMIN=XMIN
          TXMAX=XMAX
        ENDIF
      ENDIF
C
CCCCC 2011/09: EXECUTE BLOCKS DEPENDING ON WHETHER HORIZONTAL SWITCH
CCCCC          ON OR OFF
C
      IF(IHORSW.EQ.'OFF')THEN
        BWMIN=CPUMAX
        BWMAX=CPUMIN
        IF(NUMSET.GE.1)THEN
          IMAX=NUMSET
          IF(IMAX.GT.MAXBAR)IMAX=MAXBAR
          DO1130I=1,IMAX
            IF(IBARSW(I).EQ.'OFF')GOTO1130
            AWIDTH=ABARWI(I)
            IF(ABARWI(I).EQ.CPUMIN)AWIDTH=XDELMN
            IF(AWIDTH.GT.BWMAX)BWMAX=AWIDTH
 1130     CONTINUE
          BAMIN=XMIN
          BAMAX=XMAX
          IF(XMIN.NE.CPUMAX.AND.BWMAX.NE.CPUMIN)BAMIN=XMIN-BWMAX/2.0
          IF(XMAX.NE.CPUMIN.AND.BWMAX.NE.CPUMIN)BAMAX=XMAX+BWMAX/2.0
          IF(BAMIN.LT.TXMIN)TXMIN=BAMIN
          IF(BAMAX.GT.TXMAX)TXMAX=BAMAX
        ENDIF
      ELSE
        SBMIN=CPUMAX
        SBMAX=CPUMIN
        IF(NUMSET.GE.1)THEN
          IMAX=NUMSET
          IF(IMAX.GT.MAXSPI)IMAX=MAXSPI
          DO1170I=1,IMAX
            IF(ISPISW(I).EQ.'OFF')GOTO1170
            IF(ASPIBA(I).LT.SBMIN)SBMIN=ASPIBA(I)
            IF(ASPIBA(I).GT.SBMAX)SBMAX=ASPIBA(I)
 1170     CONTINUE
          IF(SBMIN.LT.TXMIN)TXMIN=SBMIN
          IF(SBMAX.GT.TXMAX)TXMAX=SBMAX
        ENDIF
C
        BBMIN=CPUMAX
        BBMAX=CPUMIN
        IF(NUMSET.GE.1)THEN
          IMAX=NUMSET
          IF(IMAX.GT.MAXBAR)IMAX=MAXBAR
          DO1180I=1,IMAX
            IF(IBARSW(I).EQ.'OFF')GOTO1180
            IF(ABARBA(I).LT.BBMIN)BBMIN=ABARBA(I)
            IF(ABARBA(I).GT.BBMAX)BBMAX=ABARBA(I)
 1180     CONTINUE
          IF(BBMIN.LT.TXMIN)TXMIN=BBMIN
          IF(BBMAX.GT.TXMAX)TXMAX=BBMAX
        ENDIF
      ENDIF
C
      DX1MIN=TXMIN
      DX2MIN=TXMIN
      DX1MAX=TXMAX
      DX2MAX=TXMAX
      IF(DX1MIN.EQ.CPUMAX)DX1MIN=X(1)
      IF(DX2MIN.EQ.CPUMAX)DX2MIN=X(1)
      IF(DX1MAX.EQ.CPUMIN)DX1MAX=X(1)
      IF(DX2MAX.EQ.CPUMIN)DX2MAX=X(1)
C
C               **********************************************
C               **  STEP 3--                                **
C               **  DETERMINE ACTUAL LIMITS FOR Y VARIABLE  **
C               **********************************************
C
C     2018/01: IF USER HAS REQUESTED "SCREEN" UNITS FOR EITHER
C              LINE OR CHARACTER TRACE, OMIT THAT DATA FROM
C              THE DETERMINATION OF THE DATA LIMITS.
C
C              THIS ASSUMES THAT XIDTEM IS IN SORTED ORDER.
C
      IFLAG=0
      DO1203I=1,NUMSET
        IF(ICHATY(I)(2:2).EQ.'S' .OR. ILINTY(I)(2:2).EQ.'S')THEN
          IFLAG=1
          GOTO1204
        ENDIF
 1203 CONTINUE
 1204 CONTINUE
C
      TYMIN=CPUMAX
      TYMAX=CPUMIN
C
      YMIN=CPUMAX
      YMAX=CPUMIN
      IF(NP.GE.1)THEN
        IF(IFLAG.EQ.0)THEN
          DO1210I=1,NP
CCCCC       JULY 1996.  FOLLOWING CODE EXCLUDES VALUES THAT ARE
CCCCC       "OUT OF RANGE" ON THE Y-AXIS.  SPECIAL CASE WHERE UPPER AND
CCCCC       LOWER LIMIT ARE EQUAL, INCLUDE ALL VALUES.
            IF(GX1MNT.NE.GX1MXT)THEN
              IF(IX1MIN.EQ.'FIXE'.AND.X(I).LT.GX1MNT)GOTO1210
              IF(IX1MAX.EQ.'FIXE'.AND.X(I).GT.GX1MXT)GOTO1210
            ENDIF
            IF(IY1MIN.EQ.'FIXE'.AND.Y(I).LT.GY1MNT)GOTO1210
            IF(IY1MAX.EQ.'FIXE'.AND.Y(I).GT.GY1MXT)GOTO1210
            IF(Y(I).LT.YMIN)YMIN=Y(I)
            IF(Y(I).GT.YMAX)YMAX=Y(I)
 1210     CONTINUE
          TYMIN=YMIN
          TYMAX=YMAX
        ELSE
          DO1215I=1,NP
            DO1218K=1,NUMSET
              AVAL=XIDTEM(K)
              IF(AVAL.EQ.DTAG(I))THEN
                IF(ICHATY(K)(2:2).EQ.'S')GOTO1215
                IF(ILINTY(K)(2:2).EQ.'S')GOTO1215
                IF(GX1MNT.NE.GX1MXT)THEN
                  IF(IX1MIN.EQ.'FIXE'.AND.X(I).LT.GX1MNT)GOTO1215
                  IF(IX1MAX.EQ.'FIXE'.AND.X(I).GT.GX1MXT)GOTO1215
                ENDIF
                IF(IY1MIN.EQ.'FIXE'.AND.Y(I).LT.GY1MNT)GOTO1215
                IF(IY1MAX.EQ.'FIXE'.AND.Y(I).GT.GY1MXT)GOTO1215
                IF(Y(I).LT.YMIN)YMIN=Y(I)
                IF(Y(I).GT.YMAX)YMAX=Y(I)
              ENDIF
 1218       CONTINUE
 1215     CONTINUE
          TYMIN=YMIN
          TYMAX=YMAX
        ENDIF
      ENDIF
C
CCCCC 2011/09: EXECUTE DIFFERENT BLOCKS DEPENDING ON WHETHER HORIZONTAL
CCCCC          SWITCH IS OFF OR ON.
C
      IF(IHORSW.EQ.'OFF')THEN
        SBMIN=CPUMAX
        SBMAX=CPUMIN
        IF(NUMSET.GE.1)THEN
          IMAX=NUMSET
          IF(IMAX.GT.MAXSPI)IMAX=MAXSPI
          DO1220I=1,IMAX
            IF(ISPISW(I).EQ.'OFF')GOTO1220
            IF(ASPIBA(I).LT.SBMIN)SBMIN=ASPIBA(I)
            IF(ASPIBA(I).GT.SBMAX)SBMAX=ASPIBA(I)
 1220     CONTINUE
          IF(SBMIN.LT.TYMIN)TYMIN=SBMIN
          IF(SBMAX.GT.TYMAX)TYMAX=SBMAX
        ENDIF
C
        BBMIN=CPUMAX
        BBMAX=CPUMIN
        IF(NUMSET.GE.1)THEN
          IMAX=NUMSET
          IF(IMAX.GT.MAXBAR)IMAX=MAXBAR
          DO1230I=1,IMAX
            IF(IBARSW(I).EQ.'OFF')GOTO1230
            IF(ABARBA(I).LT.BBMIN)BBMIN=ABARBA(I)
            IF(ABARBA(I).GT.BBMAX)BBMAX=ABARBA(I)
 1230     CONTINUE
          IF(BBMIN.LT.TYMIN)TYMIN=BBMIN
          IF(BBMAX.GT.TYMAX)TYMAX=BBMAX
        ENDIF
      ELSE
        BWMIN=CPUMAX
        BWMAX=CPUMIN
        IF(NUMSET.GE.1)THEN
          IMAX=NUMSET
          IF(IMAX.GT.MAXBAR)IMAX=MAXBAR
          DO1280I=1,IMAX
            IF(IBARSW(I).EQ.'OFF')GOTO1280
            AWIDTH=ABARWI(I)
            IF(ABARWI(I).EQ.CPUMIN)AWIDTH=XDELMN
            IF(AWIDTH.GT.BWMAX)BWMAX=AWIDTH
 1280     CONTINUE
          BAMIN=YMIN
          BAMAX=YMAX
          IF(YMIN.NE.CPUMAX.AND.BWMAX.NE.CPUMIN)BAMIN=YMIN-BWMAX/2.0
          IF(YMAX.NE.CPUMIN.AND.BWMAX.NE.CPUMIN)BAMAX=YMAX+BWMAX/2.0
          IF(BAMIN.LT.TYMIN)TYMIN=BAMIN
          IF(BAMAX.GT.TYMAX)TYMAX=BAMAX
        ENDIF
      ENDIF
C
      DY1MIN=TYMIN
      DY2MIN=TYMIN
      DY1MAX=TYMAX
      DY2MAX=TYMAX
      IF(DY1MIN.EQ.CPUMAX)DY1MIN=Y(1)
      IF(DY2MIN.EQ.CPUMAX)DY2MIN=Y(1)
      IF(DY1MAX.EQ.CPUMIN)DY1MAX=Y(1)
      IF(DY2MAX.EQ.CPUMIN)DY2MAX=Y(1)
C
 8000 CONTINUE
      IBARSW(1)=ISAVE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEDL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEDL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASPL,ICAS3D,XDELMN,AWIDTH
 9014   FORMAT('ICASPL,ICAS3D,XDELMN,AWIDTH = ',2(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)XMIN,XMAX,BWMIN,BWMAX
 9041   FORMAT('XMIN,XMAX,BWMIN,BWMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9042)BAMIN,BAMAX,TXMIN,TXMAX
 9042   FORMAT('BAMIN,BAMAX,TXMIN,TXMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9043)YMIN,YMAX,SBMIN,SBMAX
 9043   FORMAT('YMIN,YMAX,SBMIN,SBMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9044)BBMIN,BBMAX,TYMIN,TYMAX
 9044   FORMAT('BBMIN,BBMAX,TYMIN,TYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)IBAR,DEL,XDELMN,ISAVE
 9051   FORMAT('IBAR,DEL,XDELMN,ISAVE = ',A4,2E15.7,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEFI(IHARG,IHARG2,IHARLC,IHARL2,NUMARG,
     1ICOM3,ICOM4,ICOM5,NUMCOM,NCOM5,
     1ICPREP,NCPREP,ICPOST,NCPOST,
     1ICPREH,NCPREH,ICPOSH,NCPOSH,
     1IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CREATE USER-DEFINED COMMANDS.
C     INPUT  ARGUMENTS--IHARG    (A CHARACTER VECTOR)
C                     --IHARG2  (A CHARACTER VECTOR)
C                     --IHARLC  (A CHARACTER VECTOR)
C                     --IHARL2  (A CHARACTER VECTOR)
C                     --NUMARG
C      OUTPUT ARGUMENTS--ICOM3
C                        ICOM4
C                        ICOM5
C                        NUMCOM
C                        NCOM5
C                        IFOUND
C                        IERROR
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     VERSION NUMBER--86/6
C     ORIGINAL VERSION--FEBRUARY  1986.
C     UPDATED         --AUGUST    1986.
C     UPDATED         --SEPTEMBER 1987. (PREHELP AND POSTHELP)
C
C-----NON-COMMON VARIABLES----------------------------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IHARLC
      CHARACTER*4 IHARL2
C
      CHARACTER*4 ICOM3
      CHARACTER*4 ICOM4
      CHARACTER*40 ICOM5
C
      CHARACTER*40 ICPREP
      CHARACTER*40 ICPOST
      CHARACTER*40 ICPREH
      CHARACTER*40 ICPOSH
C
      CHARACTER*40 ICOM5J
      CHARACTER*4 IC4
      CHARACTER*1 IC1
      CHARACTER*4 IC4LC
      CHARACTER*1 IC1LC
      CHARACTER*40 ISTRIN
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IHARLC(*)
      DIMENSION IHARL2(*)
      DIMENSION ICOM3(*)
      DIMENSION ICOM4(*)
      DIMENSION ICOM5(*)
      DIMENSION NCOM5(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCONP.INC'
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
      ISUBN1='DPDE'
      ISUBN2='FI  '
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'DEFI')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('AT THE BEGINNING OF DPDEFI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGS2,ISUBRO
   53 FORMAT('IBUGS2,ISUBRO = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)NUMARG
   55 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GOTO59
      DO56I=1,NUMARG
      WRITE(ICOUT,57)I,IHARG(I),IHARLC(I)
   57 FORMAT('I,IHARG(I),IHARLC(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   56 CONTINUE
   59 CONTINUE
      WRITE(ICOUT,61)NUMCOM
   61 FORMAT('NUMCOM = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCOM.LE.0)GOTO65
      DO62I=1,NUMCOM
CCCCC WRITE(ICOUT,63)I,ICOM3(I),ICOM4(I),NCOM5(I),ICOM5(I)
CCC63 FORMAT('I,ICOM3(I),ICOM4(I),NCOM5(I),ICOM5(I) = ',
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC1I8,2X,A4,2X,A4,I8,A40)
   62 CONTINUE
   65 CONTINUE
      WRITE(ICOUT,66)IFOUND,IERROR
   66 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)NCPREP
   71 FORMAT('NCPREP = ',A4)
      CALL DPWRST('XXX','BUG ')
      IF(NCPREP.LE.0)GOTO74
      DO72I=1,NCPREP
      WRITE(ICOUT,73)I,ICPREP(I:I)
   73 FORMAT('I,ICPREP(I:I) = ',I8,2X,A1,4X)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
   74 CONTINUE
      WRITE(ICOUT,76)NCPOST
   76 FORMAT('NCPOST = ',A4)
      CALL DPWRST('XXX','BUG ')
      IF(NCPOST.LE.0)GOTO79
      DO77I=1,NCPOST
      WRITE(ICOUT,78)I,ICPOST(I:I)
   78 FORMAT('I,ICPOST(I:I) = ',I8,2X,A1,4X)
      CALL DPWRST('XXX','BUG ')
   77 CONTINUE
   79 CONTINUE
      WRITE(ICOUT,81)NCPREH
   81 FORMAT('NCPREH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCPREH.LE.0)GOTO84
      DO82I=1,NCPREH
      WRITE(ICOUT,83)I,ICPREH(I:I)
   83 FORMAT('I,ICPREH(I:I) = ',I8,2X,A1,4X)
      CALL DPWRST('XXX','BUG ')
   82 CONTINUE
   84 CONTINUE
      WRITE(ICOUT,86)NCPOSH
   86 FORMAT('NCPOSH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCPOSH.LE.0)GOTO89
      DO87I=1,NCPOSH
      WRITE(ICOUT,88)I,ICPOSH(I:I)
   88 FORMAT('I,ICPOSH(I:I) = ',I8,2X,A1,4X)
      CALL DPWRST('XXX','BUG ')
   87 CONTINUE
   89 CONTINUE
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
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN SUBROUTINE DPDEFI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      WHEN USING THE DEFINE COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('      YOU MUST HAVE SOME ENTRY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)
 1184 FORMAT('      AFTER THE WORD    DEFINE   ;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1185)
 1185 FORMAT('      BUT NONE WAS GIVEN HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)NUMARG
 1186 FORMAT('      NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
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
      ISTRIN(1:40)='                                        '
      ICOM5(I2)=ISTRIN(1:40)
      NCOM5(I2)=0
C
      J=0
      IF(NUMARG.LE.1)GOTO1290
      DO1200I=2,NUMARG
      J=J+1
C
      IC4=IHARG(I)
      IC1=IC4(1:1)
C
      IC4LC=IHARLC(I)
      IC1LC=IC4LC(1:1)
C
      IF(IC4(2:4).EQ.'   ')GOTO1210
      IF(IC4(1:3).EQ.'ESC')IC1LC=IESCC
      IF(IC4(1:3).EQ.'ESC')GOTO1210
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
CCCCC IF(IC4(1:3).EQ.'ESC')IC1LC=IESCC
      IF(IC4(1:2).EQ.'FS')IC1LC=IFSC
      IF(IC4(1:2).EQ.'GS')IC1LC=IGSC
      IF(IC4(1:2).EQ.'RS')IC1LC=IRSC
      IF(IC4(1:2).EQ.'US')IC1LC=IUSC
      IF(IC4(1:3).EQ.'SPA')IC1LC=' '
      IF(IC4(1:2).EQ.'SP')IC1LC=' '
      IF(IC4(1:3).EQ.'BLA')IC1LC=' '
      IF(IC4(1:2).EQ.'BL')IC1LC=' '
C
 1210 CONTINUE
      ISTRIN(J:J)=IC1LC
 1200 CONTINUE
C
 1290 CONTINUE
      ICOM5(I2)=ISTRIN(1:40)
      NCOM5(I2)=J
      IF(I2.GT.NUMCOM)NUMCOM=I2
C
C               ***************************************************
C               **  STEP 13--                                    **
C               **  CHECK FOR THE USER-COMMAND   PREPLOT         **
C               **  IF FOUND, COPY IT.                           **
C               **  CHECK FOR THE USER-COMMAND   POSTPLOT        **
C               **  IF FOUND, COPY IT.                           **
C               ***************************************************
C
      IF(IHARG(1).EQ.'PREP'.AND.IHARG2(1).EQ.'LOT')GOTO1310
      IF(IHARG(1).EQ.'POST'.AND.IHARG2(1).EQ.'PLOT')GOTO1320
      GOTO1390
 1310 CONTINUE
      NCPREP=NCOM5(I2)
      ICPREP(1:40)=ICOM5(I2)
      GOTO1390
 1320 CONTINUE
      NCPOST=NCOM5(I2)
      ICPOST(1:40)=ICOM5(I2)
      GOTO1390
 1390 CONTINUE
C
C               ***************************************************
C               **  STEP 14--                                    **
C               **  CHECK FOR THE USER-COMMAND   PREHELP         **
C               **  IF FOUND, COPY IT.                           **
C               **  CHECK FOR THE USER-COMMAND   POSTHELP        **
C               **  IF FOUND, COPY IT.                           **
C               ***************************************************
C
      IF(IHARG(1).EQ.'PREH'.AND.IHARG2(1).EQ.'ELP')GOTO1410
      IF(IHARG(1).EQ.'POST'.AND.IHARG2(1).EQ.'HELP')GOTO1420
      GOTO1490
 1410 CONTINUE
      NCPREH=NCOM5(I2)
      ICPREH(1:40)=ICOM5(I2)
      GOTO1490
 1420 CONTINUE
      NCPOSH=NCOM5(I2)
      ICPOSH(1:40)=ICOM5(I2)
      GOTO1490
 1490 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'DEFI')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DPDEFI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGS2,ISUBRO
 9013 FORMAT('IBUGS2,ISUBRO = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NUMARG
 9015 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GOTO9019
      DO9016I=1,NUMARG
      WRITE(ICOUT,9017)I,IHARG(I),IHARLC(I)
 9017 FORMAT('I,IHARG(I),IHARLC(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9016 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)I2,NUMCOM
 9021 FORMAT('I2,NUMCOM = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)I2,ICOM3(I2),ICOM4(I2),NCOM5(I2)
 9022 FORMAT('I2,ICOM3(I2),ICOM4(I2),NCOM5(I2)  = ',
     1I8,2X,A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCOM5(I)
      IF(IMAX.LE.0)GOTO9033
      ICOM5J=ICOM5(I)
      DO9031I=1,IMAX
      WRITE(ICOUT,9032)I,ICOM5J(I:I)
 9032 FORMAT('I,ICOM5J(I:I) = ',I8,2X,A1,2X)
      CALL DPWRST('XXX','BUG ')
 9031 CONTINUE
 9033 CONTINUE
      IF(NUMCOM.LE.0)GOTO9043
      DO9041I=1,NUMCOM
CCCCC WRITE(ICOUT,9042)I,ICOM3(I),ICOM4(I),NCOM5(I),ICOM5(I)
C9042 FORMAT('I,ICOM3(I),ICOM4(I),NCOM5(I),ICOM5(I) = ',
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC1I8,2X,A4,2X,A4,I8,A40)
 9041 CONTINUE
 9043 CONTINUE
      WRITE(ICOUT,9051)IFOUND,IERROR
 9051 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9061)IC4,IC1
 9061 FORMAT('IC4,IC1 = ',A4,2X,A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9062)IC4LC,IC1LC
 9062 FORMAT('IC4LC,IC1LC = ',A4,2X,A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9071)NCPREP
 9071 FORMAT('NCPREP = ',A4)
      CALL DPWRST('XXX','BUG ')
      IF(NCPREP.LE.0)GOTO9074
      DO9072I=1,NCPREP
      WRITE(ICOUT,9073)I,ICPREP(I:I)
 9073 FORMAT('I,ICPREP(I:I) = ',I8,2X,A1,4X)
      CALL DPWRST('XXX','BUG ')
 9072 CONTINUE
 9074 CONTINUE
      WRITE(ICOUT,9076)NCPOST
 9076 FORMAT('NCPOST = ',A4)
      CALL DPWRST('XXX','BUG ')
      IF(NCPOST.LE.0)GOTO9079
      DO9077I=1,NCPOST
      WRITE(ICOUT,9078)I,ICPOST(I:I)
 9078 FORMAT('I,ICPOST(I:I) = ',I8,2X,A1,4X)
      CALL DPWRST('XXX','BUG ')
 9077 CONTINUE
 9079 CONTINUE
      WRITE(ICOUT,9081)NCPREH
 9081 FORMAT('NCPREH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCPREH.LE.0)GOTO9084
      DO9082I=1,NCPREH
      WRITE(ICOUT,9083)I,ICPREH(I:I)
 9083 FORMAT('I,ICPREH(I:I) = ',I8,2X,A1,4X)
      CALL DPWRST('XXX','BUG ')
 9082 CONTINUE
 9084 CONTINUE
      WRITE(ICOUT,9086)NCPOSH
 9086 FORMAT('NCPOSH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCPOSH.LE.0)GOTO9089
      DO9087I=1,NCPOSH
      WRITE(ICOUT,9088)I,ICPOSH(I:I)
 9088 FORMAT('I,ICPOSH(I:I) = ',I8,2X,A1,4X)
      CALL DPWRST('XXX','BUG ')
 9087 CONTINUE
 9089 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDEFL(ICASPL,ICAS3D,
     1                  DX1MIN,DX1MAX,DY1MIN,DY1MAX,
     1                  DX2MIN,DX2MAX,DY2MIN,DY2MAX,
     1                  GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1                  GX2MIN,GX2MAX,GY2MIN,GY2MAX,
     1                  IX1MIN,IX1MAX,IY1MIN,IY1MAX,
     1                  IX2MIN,IX2MAX,IY2MIN,IY2MAX,
     1                  IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1                  FX1MIN,FX1MAX,FY1MIN,FY1MAX,
     1                  FX2MIN,FX2MAX,FY2MIN,FY2MAX,
     1                  NMJX1T,NMJX2T,NMJY1T,NMJY2T)
C
C     PURPOSE--TRANSFORM OBSERVED DATA LIMITS OR GIVEN LIMITS INTO
C              INTO ACTUAL FRAME LIMITS (WHICH MAY OR MAY NOT BE
C              NEAT--DEPENDING ON THE SPECIFICATION) FOR ALL 4 FRAME
C              LINES.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAS3D
C
      CHARACTER*4 IX1MIN
      CHARACTER*4 IX1MAX
      CHARACTER*4 IY1MIN
      CHARACTER*4 IY1MAX
C
      CHARACTER*4 IX2MIN
      CHARACTER*4 IX2MAX
      CHARACTER*4 IY2MIN
      CHARACTER*4 IY2MAX
C
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEFL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEFL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)DX1MIN,DY1MIN,DX1MAX,DY1MAX
   52   FORMAT('DX1MIN,DY1MIN,DX1MAX,DY1MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)DX2MIN,DY2MIN,DX2MAX,DY2MAX
   53   FORMAT('DX2MIN,DY2MIN,DX2MAX,DY2MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)GX1MIN,DY1MIN,GX1MAX,DY1MAX
   54   FORMAT('GX1MIN,DY1MIN,GX1MAX,DY1MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)GX2MIN,DY2MIN,GX2MAX,DY2MAX
   55   FORMAT('GX2MIN,DY2MIN,GX2MAX,DY2MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IX1MIN,IY1MIN,IX1MAX,IY1MAX
   56   FORMAT('IX1MIN,IY1MIN,IX1MAX,IY1MAX = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IX2MIN,IY2MIN,IX2MAX,IY2MAX
   57   FORMAT('IX2MIN,IY2MIN,IX2MAX,IY2MAX = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IX1TSC,IX2TSC,IY1TSC,IY2TSC
   58   FORMAT('IX1TSC,IX2TSC,IY1TSC,IY2TSC = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)FX1MIN,FX1MAX,FY1MIN,FY1MAX
   61   FORMAT('FX1MIN,FX1MAX,FY1MIN,FY1MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)FX2MIN,FX2MAX,FY2MIN,FY2MAX
   62   FORMAT('FX2MIN,FX2MAX,FY2MIN,FY2MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ICASPL,ICAS3D
   63   FORMAT('ICASPL,ICAS3D = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)NMJX1T,NMJX2T,NMJY1T,NMJY2T
   64   FORMAT('NMJX1T,NMJX2T,NMJY1T,NMJY2T = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  DETERMINE FRAME LIMITS ON BOTTOM HORIZONTAL AXIS **
C               *******************************************************
C
      CALL DPDEF2(DX1MIN,DX1MAX,GX1MIN,GX1MAX,IX1MIN,IX1MAX,IX1TSC,
     1            FX1MIN,FX1MAX,NMJX1T)
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  DETERMINE FRAME LIMITS ON TOP  HORIZONTAL   AXIS **
C               *******************************************************
C
      CALL DPDEF2(DX2MIN,DX2MAX,GX2MIN,GX2MAX,IX2MIN,IX2MAX,IX2TSC,
     1            FX2MIN,FX2MAX,NMJX2T)
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               *******************************************************
C               **  STEP 3--                                         **
C               **  DETERMINE FRAME LIMITS ON LEFT    VERTICAL AXIS  **
C               *******************************************************
C
      CALL DPDEF2(DY1MIN,DY1MAX,GY1MIN,GY1MAX,IY1MIN,IY1MAX,IY1TSC,
     1            FY1MIN,FY1MAX,NMJY1T)
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  DETERMINE FRAME LIMITS ON RIGHT   VERTICAL   AXIS **
C               ********************************************************
C
      CALL DPDEF2(DY2MIN,DY2MAX,GY2MIN,GY2MAX,IY2MIN,IY2MAX,IY2TSC,
     1            FY2MIN,FY2MAX,NMJY2T)
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEFL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEFL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)DX1MIN,DY1MIN,DX1MAX,DY1MAX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)DX2MIN,DY2MIN,DX2MAX,DY2MAX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)GX1MIN,DY1MIN,GX1MAX,DY1MAX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)GX2MIN,DY2MIN,GX2MAX,DY2MAX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IX1MIN,IY1MIN,IX1MAX,IY1MAX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IX2MIN,IY2MIN,IX2MAX,IY2MAX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IX1TSC,IX2TSC,IY1TSC,IY2TSC
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)FX1MIN,FX1MAX,FY1MIN,FY1MAX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)FX2MIN,FX2MAX,FY2MIN,FY2MAX
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ICASPL,ICAS3D
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)NMJX1T,NMJX2T,NMJY1T,NMJY2T
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEF2(DMIN,DMAX,GMIN,GMAX,IMIN,IMAX,ITICSC,
     1                  FMIN,FMAX,NUMMAJ)
C
C     PURPOSE--TRANSFORM OBSERVED DATA LIMITS OR GIVEN LIMITS INTO
C              ACTUAL FRAME LIMITS (WHICH MAY OR MAY NOT BE NEAT--
C              DEPENDING ON THE SPECIFICATION) FOR A SINGLE FRAME LINE.
C     NOTE--ALGORITHM SUGGESTED BY WALTER LIGGETT
C           NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C     INTERESTING TEST PROBLEMS--156 AND 234
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
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED         --???       19??.  WEIBULL SCALE
C     UPDATED         --JUNE      1990.  NORMAL SCALE
C     UPDATED         --JULY      1996.  ALLOW UPPER AND LOWER BOUND
C                                        TO BE EQUAL
C
C-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
C
      CHARACTER*4 IMIN
      CHARACTER*4 IMAX
      CHARACTER*4 ITICSC
C
      DIMENSION WEIB21(25)
CCCCC THE FOLLOWING LINE WAS ADDED JUNE 1990
      DIMENSION ANORM(27)
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
C-----DATA STATEMENTS---------------------------------------------------
C
      DATA WEIB21( 1),WEIB21( 2),WEIB21( 3),WEIB21( 4),WEIB21( 5),
     1     WEIB21( 6),WEIB21( 7),WEIB21( 8),WEIB21( 9),WEIB21(10),
     1     WEIB21(11),WEIB21(12),WEIB21(13),WEIB21(14),WEIB21(15),
     1     WEIB21(16),WEIB21(17),WEIB21(18),WEIB21(19),WEIB21(20),
     1     WEIB21(21)
     1/0.000001,0.00001,0.0001,0.001,0.01,0.1,
     1 0.5,1.0,5.0,10.0,20.0,30.0,40.0,50.0,
     1 60.0,70.0,80.0,90.0,95.0,99.0,99.9/
C
CCCCC THE FOLLOWING DATA STATEMENT WAS ADDED JUNE 1990
      DATA ANORM( 1),ANORM( 2),ANORM( 3),ANORM( 4),ANORM( 5),
     1     ANORM( 6),ANORM( 7),ANORM( 8),ANORM( 9),ANORM(10),
     1     ANORM(11),ANORM(12),ANORM(13),ANORM(14),ANORM(15),
     1     ANORM(16),ANORM(17),ANORM(18),ANORM(19),ANORM(20),
     1     ANORM(21),ANORM(22),ANORM(23),ANORM(24),ANORM(25),
     1     ANORM(26),ANORM(27)
     1/0.000001,0.00001,0.0001,0.001,0.01,0.1,0.5,
     1 1.0,5.0,10.0,20.0,30.0,40.0,
     1 50.0,
     1 60.0,70.0,80.0,90.0,95.0,99.0,
     1 99.5,99.9,99.99,99.999,99.9999,99.99999,99.999999/
C
C-----START POINT-----------------------------------------------------
C
      RTMINP=(-999.0)
      RTMAXP=(-999.00)
      ANUM=0.0
      EXPMIN=0.0
      EXPMAX=0.0
      IEXMIN=0
      IEXMAX=0
      AEXMIN=0.0
      AEXMAX=0.0
      DELMIN=0.0
      DELMAX=0.0
      ATMIN=0.0
      ATMAX=0.0
      IEXP=0
      IRTMIN=0
      IRTMAX=0
      RTMIN=0.0
      RTMAX=0.0
      ANMIN=0.0
      ANMAX=0.0
C
      AHUNDR=100.0
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEF2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)DMIN,DMAX,GMIN,GMAX
   52   FORMAT('DMIN,DMAX,GMIN,GMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IMIN,IMAX,ITICSC,NUMMAJ
   54   FORMAT('IMIN,IMAX,ITICSC,NUMMAJ = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IBUGG4,ISUBG4,IERRG4
   59   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  AS A STARTER FOR THE LINEAR, LOG, WEIBULL, AND   **
C               **  NORMAL CASES, TREAT THE FIXED OR FLOATING CASE   **
C               *******************************************************
C
      FMINOL=FMIN
      FMAXOL=FMAX
C
      AMIN=DMIN
      AMAX=DMAX
      IF(IMIN.EQ.'FIXE')AMIN=GMIN
      IF(IMAX.EQ.'FIXE')AMAX=GMAX
C
      IF(AMIN.GT.AMAX)THEN
        HOLD=AMIN
        AMAX=AMIN
        AMIN=HOLD
      ENDIF
C
C               *****************************************
C               **  STEP 2--                           **
C               **  TREAT THE LOG SCALE CASE           **
C               **  (WHICH WILL AUTOMATICALLY BE NEAT  **
C               **  WITH FULL CYCLES RESULTING)        **
C               *****************************************
C
      IF(ITICSC.EQ.'LOG')THEN
        IF(AMIN.LE.0.0)THEN
           ANUM=AMIN
        ELSEIF(AMAX.LE.0.0)THEN
           ANUM=AMAX
        ELSE
          GOTO1219
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPDEF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      WHILE COMPUTING FRAME LIMITS FOR A LOG SCALE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)
 1214   FORMAT('      PLOT, THE LOG OF A NON-POSITIVE NUMBER WAS ',
     1         'ENCOUNTERED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)ANUM
 1216   FORMAT('      THE NUMBER = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)
 1217   FORMAT('      CORRECTIVE ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1218)
 1218   FORMAT('      CHANGE DATA OR CHANGE LIMITS.')
        CALL DPWRST('XXX','BUG ')
        IERRG4='YES'
        GOTO9000
C
 1219   CONTINUE
C
        IF(AMIN.EQ.AMAX)THEN
          EXPMIN=LOG10(AMIN)
          IF(EXPMIN.LT.0.0)EXPMIN=EXPMIN-1.0
          IEXMIN=EXPMIN
          FMIN=10.0**IEXMIN
          FMAX=FMIN
          GOTO9000
        ENDIF
C
        EXPMIN=LOG10(AMIN)
        IF(EXPMIN.LT.0.0)EXPMIN=EXPMIN-1.0
        EXPMAX=LOG10(AMAX)
        IEXMIN=EXPMIN
        IEXMAX=EXPMAX
        AEXMIN=IEXMIN
        AEXMAX=IEXMAX
        DELMIN=EXPMIN-AEXMIN
        DELMAX=EXPMAX-AEXMAX
        IF(DELMAX.GE.0.00001)IEXMAX=IEXMAX+1
        IF(IEXMAX.EQ.IEXMIN)IEXMAX=IEXMIN+1
C
        FMIN=10.0**IEXMIN
        FMAX=10.0**IEXMAX
        GOTO9000
C
      ENDIF
C
C               *****************************************
C               **  STEP 3--                           **
C               **  TREAT THE WEIBULL SCALE CASE       **
C               **  (WHICH WILL AUTOMATICALLY BE NEAT) **
C               *****************************************
C
      IF(ITICSC.EQ.'WEIB')THEN
C
        IF(AMIN.LE.0.0.OR.AMIN.GE.AHUNDR)THEN
          ANUM=AMIN
        ELSEIF(AMAX.LE.0.0.OR.AMAX.GE.AHUNDR)THEN
          ANUM=AMAX
        ELSE
          GOTO1319
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1312)
 1312   FORMAT('      WHILE COMPUTING FRAME LIMITS FOR A WEIBULL SCALE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1314)
 1314   FORMAT('      PLOT, THE LOG OF A NUMBER OUTSIDE OF THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1315)
 1315   FORMAT('      0 TO 100 RANGE    WAS ENCOUNTERED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)ANUM
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1218)
        CALL DPWRST('XXX','BUG ')
        IERRG4='YES'
        GOTO9000
C
 1319   CONTINUE
C
        NUMMA2=NUMMAJ
        IF(NUMMAJ.LE.16)NUMMA2=16
        IF(NUMMAJ.GE.21)NUMMA2=21
C
        I1=1+(21-NUMMA2)
        I2=21
C
        FMIN=WEIB21(I1)
        FMAX=WEIB21(I2)
C
        GOTO9000
C
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS ADDED JUNE 1990
C               *****************************************
C               **  STEP 4--                           **
C               **  TREAT THE NORMAL SCALE CASE        **
C               **  (WHICH WILL AUTOMATICALLY BE NEAT) **
C               *****************************************
C
      IF(ITICSC.EQ.'NORM')THEN
C
        GOTO1419
C
 1410   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1412)
 1412   FORMAT('      WHILE COMPUTING FRAME LIMITS FOR A NORMAL SCALE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1414)
 1414   FORMAT('      PLOT, A NUMBER OUTSIDE OF THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1415)
 1415   FORMAT('      0 TO 100 RANGE    WAS ENCOUNTERED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)ANUM
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1218)
        CALL DPWRST('XXX','BUG ')
        IERRG4='YES'
        GOTO9000
C
 1419   CONTINUE
C
        NUMMA2=NUMMAJ
        IF(NUMMAJ.LE.15)NUMMA2=15
        IF(NUMMAJ.GE.27)NUMMA2=27
        IHALF=NUMMA2/2
        I1=14-IHALF
        I2=14+IHALF
        IF(I1.LE.1)I1=1
        IF(I2.GE.27)I2=27
C
        FMIN=ANORM(I1)
        FMAX=ANORM(I2)
C
        GOTO9000
C
      ENDIF
C
C               ************************************
C               **  STEP 34--                     **
C               **  TREAT THE EQUALITY CASE       **
C               **  WHICH WILL AUTOMATICALLY GET  **
C               **  ARTIFICIAL, NON-EQUAL LIMITS  **
C               ************************************
C
      IF(AMIN.EQ.AMAX)THEN
        IF(AMIN.EQ.0.0)THEN
          ATMIN=-1.0
          ATMAX=1.0
        ELSEIF(AMIN.LT.0.0)THEN
          ATMIN=1.05*AMIN
          ATMAX=0.95*AMIN
        ELSEIF(AMIN.GT.0.0)THEN
          ATMIN=0.95*AMIN
          ATMAX=1.05*AMIN
        ENDIF
        FMIN=ATMIN
        FMAX=ATMAX
        GOTO9000
      ENDIF
C
C               ***************************************
C               **  STEP 35--                        **
C               **  COMPUTE NEAT, NON-LOG    LIMITS  **
C               ***************************************
C
      ATMIN=AMIN
      ATMAX=AMAX
      IEXP=0
 4500 CONTINUE
      ATDEL=ATMAX-ATMIN
C
      IF(IBUGG4.EQ.'ON')THEN
        WRITE(ICOUT,4505)ATMIN,ATMAX,ATDEL,IEXP
 4505   FORMAT('ATMIN,ATMAX,ATDEL,IEXP = ',3E15.7,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ATDEL.LT.1.0)THEN
        ATMIN=ATMIN*10.0
        ATMAX=ATMAX*10.0
        IEXP=IEXP+1
        GOTO4500
      ELSEIF(ATDEL.GT.10.0)THEN
        ATMIN=ATMIN/10.0
        ATMAX=ATMAX/10.0
        IEXP=IEXP-1
        GOTO4500
      ELSE
        CALL DPDEF3(ATMIN,ATMAX,RTMIN,RTMAX)
      ENDIF
C
 4590 CONTINUE
      DENOM=10.0**IEXP
      ANMIN=RTMIN/DENOM
      ANMAX=RTMAX/DENOM
C
      FMIN=AMIN
      FMAX=AMAX
      IF(IMIN.EQ.'FLOA')FMIN=ANMIN
      IF(IMAX.EQ.'FLOA')FMAX=ANMAX
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEF2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)FMIN,FMAX,NUMMAJ
 9016   FORMAT('FMIN,FMAX,NUMMAJ = ',2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)EXPMIN,EXPMAX,IEXMIN,IEXMAX
 9021   FORMAT('EXPMIN,EXPMAX,IEXMIN,IEXMAX = ',2G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)ATMIN,ATMAX,IEXP
 9032   FORMAT('ATMIN,ATMAX,IEXP  = ',2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9034)RTMIN,RTMAX,IRTMIN,IRTMAX
 9034   FORMAT('RTMIN,RTMAX,IRTMIN,IRTMAX = ',2G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9035)ANMIN,ANMAX,AMIN,AMAX
 9035   FORMAT('ANMIN,ANMAX,AMIN,AMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9037)AEXMIN,AEXMAX,DELMIN,DELMAX
 9037   FORMAT('AEXMIN,AEXMAX,DELMIN,DELMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9039)IBUGG4,ISUBG4,IERRG4
 9039   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEF3(ATMIN,ATMAX,RTMIN,RTMAX)
C
C     PURPOSE--TRANSFORM LIMITS (WHICH ARE SUCH THAT THERE DIFFERENCE
C              IS BETWEEN 1 AND 10) INTO NEAT LIMITS.
C     NOTE--ALGORITHM SUGGESTED BY WALTER LIGGETT
C           NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C     INTERESTING TEST PROBLEMS--156 AND 234
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
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
C
      CHARACTER*4 ICMIN
      CHARACTER*4 ICMAX
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
      IRTMIN=(-999)
      IRTMAX=(-999)
      RTMINP=(-999.0)
      RTMAXP=(-999.0)
C
      EPS=0.00001
      ONEMEP=1.0-EPS
      ONEPEP=1.0+EPS
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEF3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEF3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ATMIN,ATMAX,EPS
   52   FORMAT('ATMIN,ATMAX,EPS = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 1100 CONTINUE
      CALL CKINTE(ATMIN,EPS,ONEMEP,ONEPEP,ICMIN,IRTMIN)
      CALL CKINTE(ATMAX,EPS,ONEMEP,ONEPEP,ICMAX,IRTMAX)
      RTMIN=IRTMIN
      RTMAX=IRTMAX
      IF(ICMIN.EQ.'YES'.AND.ICMAX.EQ.'YES')GOTO9000
C
 1120 CONTINUE
      IF(ICMIN.EQ.'YES')GOTO1129
      IRTMIN=ATMIN
      IF(ATMIN.LT.0.0)IRTMIN=ATMIN-1.0
      RTMIN=IRTMIN
CCCCC RTMINP=RTMIN+0.5
CCCCC IF(RTMINP.LE.ATMIN)RTMIN=RTMINP
 1129 CONTINUE
C
 1130 CONTINUE
      IF(ICMAX.EQ.'YES')GOTO1139
      IRTMAX=ATMAX+1.0
      IF(ATMAX.LT.0.0)IRTMAX=ATMAX
      RTMAX=IRTMAX
CCCCC RTMAXP=RTMAX-0.5
CCCCC IF(RTMAXP.GE.ATMAX)RTMAX=RTMAXP
 1139 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DEF3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEF3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)ATMIN,IRTMIN,RTMINP,RTMIN
 9021   FORMAT('ATMIN,IRTMIN,RTMINP,RTMIN = ',G15.7,I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)ATMAX,IRTMAX,RTMAXP,RTMAX
 9022   FORMAT('ATMAX,IRTMAX,RTMAXP,RTMAX = ',G15.7,I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9025)EPS,ONEMEP,ONEPEP
 9025   FORMAT('EPS,ONEMEP,ONEPEP = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9026)ICMIN,ICMAX
 9026   FORMAT('ICMIN,ICMAX = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9029)IBUGG4,ISUBG4,IERRG4
 9029   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEFN(ICOM,IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IBUGO2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--SHOW DEVICE FONTS.  THIS COMMAND IS INTENDED
C              FOR DEVICES WHICH SUPPORT VARIOUS HARDWARE FONTS
C              (RIGHT NOW, LIMITED TO POSTSCRIPT).
C     INPUT  ARGUMENTS--ICOM   (A  CHARACTER VECTOR)
C                     --IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
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
C     ORIGINAL VERSION--OCTOBER   1991.
C     UPDATED         --SEPTEMBER 1993.  FIX MULTI-LINE FORMATS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGO2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*40 ICPREH
      CHARACTER*4 IRESP
      CHARACTER*40 IPSTAL(100)
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCODV.INC'
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
      DATA (IPSTAL(I),I=1,15)/
     1 'TIMES ROMAN',
     2 'TIMES ITALIC',
     3 'TIMES BOLD',
     4 'TIMES BOLD ITALIC',
     5 'HELVETICA',
     6 'HELVETICA OBLIQUE',
     7 'HELVETICA BOLD',
     8 'HELVETICA BOLD OBLIQUE',
     9 'COURIER',
     * 'COURIER OBLIQUE',
     1 'COURIER BOLD',
     2 'COURIER BOLD OBLIQUE',
     3 'AVANT GARDE',
     4 'AVANT GARDE BOOK OBLIQUE',
     5 'AVANT GARDE DEMI'/
      DATA (IPSTAL(I),I=16,30)/
     1 'AVANT GARDE DEMI OBLIQUE',
     2 'BOOK DEMI',
     3 'BOOK DEMI ITALIC',
     4 'BOOK LIGHT',
     5 'BOOK LIGHT ITALIC',
     6 'HELVETICA NARROW',
     7 'HELVETICA NARROW BOLD',
     8 'HELVETICA NARROW BOLD OBLIQUE',
     9 'HELVETICA NARROW OBLIQUE',
     * 'CENTURY',
     1 'CENTURY BOLD',
     2 'CENTURY ITALIC',
     3 'CENTURY BOLD ITALIC',
     4 'PALATINO',
     5 'PALATINO BOLD'/
      DATA (IPSTAL(I),I=31,45)/
     1 'PALATINO ITALIC',
     2 'PALATINO BOLD ITALIC',
     3 'ZAPF',
     4 'SYMBOL',
     5 ' ',
     6 ' ',
     7 ' ',
     8 ' ',
     9 ' ',
     * ' ',
     1 ' ',
     2 ' ',
     3 ' ',
     4 ' ',
     5 ' '/
      DATA (IPSTAL(I),I=46,60)/
     1 ' ',
     2 ' ',
     3 ' ',
     4 ' ',
     5 ' ',
     6 ' ',
     7 ' ',
     8 ' ',
     9 ' ',
     * ' ',
     1 ' ',
     2 ' ',
     3 ' ',
     4 ' ',
     5 ' '/
      DATA (IPSTAL(I),I=61,75)/
     1 ' ',
     2 ' ',
     3 ' ',
     4 ' ',
     5 ' ',
     6 ' ',
     7 ' ',
     8 ' ',
     9 ' ',
     * ' ',
     1 ' ',
     2 ' ',
     3 ' ',
     4 ' ',
     5 ' '/
      DATA (IPSTAL(I),I=76,90)/
     1 ' ',
     2 ' ',
     3 ' ',
     4 ' ',
     5 ' ',
     6 ' ',
     7 ' ',
     8 ' ',
     9 ' ',
     * ' ',
     1 ' ',
     2 ' ',
     3 ' ',
     4 ' ',
     5 ' '/
      DATA (IPSTAL(I),I=91,100)/
     1 ' ',
     2 ' ',
     3 ' ',
     4 ' ',
     5 ' ',
     6 ' ',
     7 ' ',
     8 ' ',
     9 ' ',
     * ' '/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      IBUGG4='OFF'
      ISUBG4='-999'
C
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEFN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPDEFN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)IBUGO2,IFOUND,IERROR,NUMARG
   60   FORMAT('IBUGO2,IFOUND,IERROR,NUMARG = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I) = ',
     1           I8,3(2X,A4),I8)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
C
      IF(ICOM.EQ.'POST')THEN
C
C       *****************************************
C       **  POSTSCRIPT CASE                    **
C       *****************************************
C
        IF(IHARG(1).EQ.'FONT'.AND. IHARG(2).EQ.'SHOW')GOTO4800
        IF(IHARG(1).EQ.'FONT'.AND. IHARG(2).EQ.'LIST')GOTO4800
        IF(IHARG(1).EQ.'FONT'.AND. IHARG(2).EQ.'PRIN')GOTO4800
        IF(IHARG(1).EQ.'FONT'.AND. IHARG(2).EQ.'DEFA')GOTO4500
        IF(IHARG(1).EQ.'FONT'.AND. IHARG(2).EQ.'    ')GOTO4800
        IF(IHARG(1).EQ.'FONT')GOTO4110
        IF(IHARG(1).EQ.'SHOW')GOTO4800
        IF(IHARG(1).EQ.'LIST')GOTO4800
        IF(IHARG(1).EQ.'PRIN')GOTO4800
        GOTO9000
      ENDIF
C
 4110 CONTINUE
      IARGFN=3
      GOTO4190
C
 4120 CONTINUE
      IARGFN=2
      GOTO4190
C
 4190 CONTINUE
      IFOUND='YES'
      IF(IHARG(IARGFN).EQ.'    ')GOTO4800
      IF(IHARG(IARGFN).EQ.'AUTO')GOTO4500
      IF(IHARG(IARGFN).EQ.'DEFA')GOTO4500
      IF(IHARG(IARGFN).EQ.'ON  ')GOTO4500
      IF(IHARG(IARGFN).EQ.'LIST')GOTO4800
      IF(IHARG(IARGFN).EQ.'SHOW')GOTO4800
      IF(IHARG(IARGFN).EQ.'PRIN')GOTO4800
      GOTO4600
C
C  SET DEFAULT POSTSCRIPT FONT
C
 4500 CONTINUE
      IFOUND='YES'
      IPSTFN='HELB'
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,4508)IPSTFN
 4508   FORMAT('THE POSTSCRIPT FONT HAS BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
C  SET POSTSCRIPT FONT.  THIS CODE HAS NOT BEEN WRITTEN YET.
C
 4600 CONTINUE
      GOTO9000
C
C  LIST AVAILABLE POSTSCRIPT FONTS
C
 4800 CONTINUE
      IFOUND='YES'
      IHELMX=24
      NCPREH=0
      ICPREH=' '
      IRESP='YES'
      IBUGO2='OFF'
      NUMLPR=4
      DO4810I=1,IPSTMF
         NUMLPR=NUMLPR+3
         IF(NUMLPR.GE.IHELMX)THEN
            CALL DPMORE(NUMLPR,NCPREH,ICPREH,IRESP,IBUGO2,IERROR)
            NUMLPR=0
            IF(IRESP.EQ.'NO')GOTO9000
         END IF
CCCCC THE FOLLOWING FORMAT STATEMENT WAS SPLIT   SEPTEMBER 1993
         WRITE(ICOUT,4811)IPSTT2(I)
 4811    FORMAT('POSTSCRIPT FONT: ',A40)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,4812)IPSTT1(I)
 4812    FORMAT('      SET POSTSCRIPT FONT ',A4,' OR')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,4813)IPSTAL(I)
 4813    FORMAT('      SET POSTSCRIPT FONT ',A40)
         CALL DPWRST('XXX','BUG ')
 4810 CONTINUE
      GOTO9000
C
 4910 CONTINUE
      IERROR='YES'
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEFN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DPDEFN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR
 9020   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEFR(IHARG,IARGT,ARG,NUMARG,DEFDMF,
     1DEMOFR,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FREQUENCY AS INPUT IN COMPLEX
C              DEMODULATION.
C              THE SPECIFIED FREQUENCY VALUE WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE DEMOFR.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFDMF (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--DEMOFR (A  FLOATING POINT VARIABLE)
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
 1110 CONTINUE
      IF(NUMARG.LE.1)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      GOTO1120
C
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPDEFR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR DEMODULATION FREQUENCY ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE THE ANALYST DESIRES THE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      FREQUENCY FOR DEMODULATION ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      TO BE .25 (OBSERVATIONS PER UNIT TIME)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      DEMODULATION FREQUENCY .25 ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      HOLD=DEFDMF
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      DEMOFR=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)DEMOFR
 1181 FORMAT('THE DEMODULATION FREQUENCY HAS JUST BEEN SET TO ',
     1E15.7)
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
      WRITE(ICOUT,8111)DEMOFR
 8111 FORMAT('THE CURRENT DEMODULATION FREQUENCY IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)DEFDMF
 8112 FORMAT('THE DEFAULT DEMODULATION FREQUENCY IS ',E15.7)
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
      SUBROUTINE DPDEFT(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1                  IDEFFN,NUMDEV,MAXDEV,
     1                  IDMANU,IDMODE,IDMOD2,IDMOD3,IDPOWE,
     1                  IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1                  IBUGO2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FONT (TEKT/SIMP/...) FOR AN OUTPUT DEVICE.
C              THE FONT FOR DEVICE I
C              WILL BE PLACED IN THE I-TH ELEMENT OF THE CHARACTER
C              VECTOR IDFONT(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFFN
C                     --MAXDEV
C     OUTPUT ARGUMENTS--IDFONT (A CHARACTER VECTOR
C                              WHOSE I-TH ELEMENT CONTAINS THE
C                              FONT (ON/OFF) FOR DEVICE I.
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
C     VERSION NUMBER--96/7
C     ORIGINAL VERSION--JULY      1996.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IDEFFN
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
      CHARACTER*4 IBUGO2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
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
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
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
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FONT')THEN
        IF(NUMARG.LE.1 .OR. IHARG(2).EQ.'ON')THEN
          IHOLD='SIMP'
        ELSEIF(IHARG(2).EQ.'OFF')THEN
          IHOLD='OFF'
        ELSEIF(IHARG(2).EQ.'AUTO' .OR. IHARG(2).EQ.'DEFA')THEN
          IHOLD=IDEFFN
        ELSE
          IHOLD=IHARG(NUMARG)
        ENDIF
        IFOUND='YES'
        DO1135I=1,NUMDEV
          IDFONT(I)=IHOLD
 1135   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1136)IHOLD
 1136     FORMAT('THE FONT FOR ALL DEVICES HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'FONT')THEN
        IF(IARGT(1).EQ.'NUMB')THEN
          I=IARG(1)
          IF(1.LT.I .OR. I.GT.MAXDEV)THEN
            IERROR='YES'
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1141)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1152)
 1152       FORMAT('      IN THE DEVICE ... FONT COMMAND,')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1153)
 1153       FORMAT('      THE NUMBER OF DEVICES MUST BE ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1154)MAXDEV
 1154       FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1155)
 1155       FORMAT('      SUCH WAS NOT THE CASE HERE--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1156)I
 1156       FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',
     1             'DEVICE.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ELSE
            IF(NUMARG.LE.2 .OR. IHARG(3).EQ.'ON')THEN
              IHOLD='SIMP'
            ELSEIF(IHARG(3).EQ.'OFF')THEN
              IHOLD='OFF'
            ELSEIF(IHARG(3).EQ.'AUTO' .OR. IHARG(3).EQ.'DEFA')THEN
              IHOLD=IDEFFN
            ELSE
              IHOLD=IHARG(NUMARG)
            ENDIF
            IFOUND='YES'
            IDFONT(I)=IHOLD
C
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1181)I
 1181         FORMAT('            DEVICE           --',I4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1182)IDUNIT(I)
 1182         FORMAT('            I/O UNIT         --',I4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1183)IDMANU(I)
 1183         FORMAT('            MANUFACTURER     --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1184)IDMODE(I),IDMOD2(I),IDMOD3(I)
 1184         FORMAT('            MODEL            --',2(A4,2X),A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1185)IDPOWE(I)
 1185         FORMAT('            POWER            --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1186)IDCONT(I)
 1186         FORMAT('            CONTINUITY       --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1187)IDCOLO(I)
 1187         FORMAT('            COLOR           --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1188)IDNHPP(I)
 1188         FORMAT('            HORIZONTAL PIXELS--',I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1189)IDNVPP(I)
 1189         FORMAT('            VERTICAL   PIXELS--',I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1191)IDFONT(I)
 1191         FORMAT('            FONT             --',A4)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            GOTO9000
          ENDIF
        ELSE
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
 1141     FORMAT('***** ERROR IN DPDEFT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
 1142     FORMAT('      IN THE DEVICE ... FONT COMMAND,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
 1143     FORMAT('      THE DEVICE IS IDENTIFIED BY A NUMBER, AS IN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1144)
 1144     FORMAT('      DEVICE 3 FONT TEKTRONIX')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
      ENDIF
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPDEGR(IHARG,IARGT,IARG,NUMARG,IDEFDG,
     1                  IDEG,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE INTEGER DEGREE OF THE POLYNOMIAL FOR USE
C              IN THE FIT, PRE-FIT, SPLINE FIT, AND SMOOTH COMMANDS.
C              THE SPECIFIED DEGREE WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE IDEG.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --IARG   (AN INTEGER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFDG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IDEG   (AN INTEGER VARIABLE)
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
C     ORIGINAL VERSION--NOVEMBER 1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --JULY      2019. CHECK FOR CONFLICT WITH
C                                       "DEGREE ... FIT" COMMAND
C     UPDATED         --JULY      2019. RECODE FOR BETTER READABILITY
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
      DIMENSION IARG(*)
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
 1110 CONTINUE
      IF(NUMARG.LE.0 .OR.
     1   (NUMARG.EQ.1.AND.IHARG(1).EQ.'DEGR') .OR.
     1   IHARG(NUMARG).EQ.'ON' .OR. IHARG(NUMARG).EQ.'OFF' .OR.
     1   IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
        IHOLD=IDEFDG
      ELSEIF(IARGT(NUMARG).EQ.'NUMB')THEN
        IHOLD=IARG(NUMARG)
      ELSE
        IERROR='YES'
        WRITE(ICOUT,1121)
 1121   FORMAT('***** ERROR IN DPDEGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1122)
 1122   FORMAT('      ILLEGAL FORM FOR DEGREE COMMAND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1124)
 1124   FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1125)
 1125   FORMAT('      SUPPOSE THE THE ANALYST WISHES TO SET THE ',
     1         'DEGREE = 3  ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1127)
 1127   FORMAT('      FOR SOME SMOOTHING OR FITTING OPERATION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1128)
 1128   FORMAT('      THEN AN ALLOWABLE FORM IS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1129)
 1129   FORMAT('      DEGREE 3 ')
        CALL DPWRST('XXX','BUG ')
        GOTO1199
      ENDIF
C
      IFOUND='YES'
      IDEG=IHOLD
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)IDEG
 1181   FORMAT('THE POLYNOMIAL DEGREE HAS JUST BEEN SET TO ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPDEGP(Y,N,
     1                  XTEMP,MAXNXT,
     1                  GAMMA,A,SDG,THRESH,
     1                  GAMMA2,ALOC,SCALE,
     1                  ALIKE,AIC,AICC,BIC,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE DEHAAN
C              ESTIMATES FOR THE GENERALIZED PARETO DISTRIBUTION.
C              THIS IS USED IN EXTREME VALUE APPLICATIONS.
C     EXAMPLE--DEHAAN Y
C     REFERENCE--DeHaan (1994). "Extreme Value Theory and
C                Applications", Edited by Galambos, Lechner, and
C                Simiu, Kluwer Academic Publishers, Boston,
C                pp. 93-122.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/5
C     ORIGINAL VERSION--MAY       1998.
C     UPDATED         --APRIL     2005. NUMEROUS CORRECTIONS AND
C                                       ENHANCEMENTS
C     UPDATED         --OCTOBER   2010. SLIGHT TWEAK TO ALGORITHM
C                                       IN REGARD TO THE THRESHOLD
C     UPDATED         --OCTOBER   2010. CALL GEPLI1 TO OBTAIN
C                                       LIKELIHOOD, AIC VALUES
C     UPDATED         --OCTOBER   2010. USE DPDTA1 TO PRINT
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
      CHARACTER*8 ISIGN1
      CHARACTER*8 ISIGN2
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DGAMMA
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      EXTERNAL DGAMMA
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
      DIMENSION QP(1)
C
      INCLUDE 'DPCOST.INC'
C
      PARAMETER (MAXROW=40)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITEXT(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL IFIRST
      LOGICAL ILAST
C
      CHARACTER*40 IDIST
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
      DATA PI / 3.1415926535/
      DATA MINSIZ / 5/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPDE'
      ISUBN2='GP  '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEGP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPDEGP--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)N,MINSIZ,THRESH,PPOTTO
   53   FORMAT('N,MINSIZ,THRESH,PPOTTO = ',2I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,MAX(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEGP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NPERC=0
      NTEMP=MINSIZ-1
      CALL CKDIST(Y,N,NTEMP,QP,NPERC,ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IDIST='GENERALIZED PARETO (DEHAAN)'
      IFLAG=0
C
      CALL SUMRAW(Y,N,IDIST,IFLAG,
     1            XMEAN,XVAR,XSD,XMIN,XMAX,
     1            ISUBRO,IBUGA3,IERROR)
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CARRY OUT CALCULATIONS                **
C               **  FOR DEHAAN ESTIMATE                   **
C               **  SORT THE DATA                         **
C               **  AND IDENTIFY POINTS ABOVE THE THRESHOLD*
C               ********************************************
C
 2100 CONTINUE
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEGP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     NOTE 10/2010: DEFINE THRESHOLD AS MINIMUM VALUE, NOT
C     MINIMUM MINUS EPSILON.
C
      CALL SORT(Y,N,Y)
      EPS=0.0001
CCCCC IF(THRESH.LE.0.0)THRESH=Y(1)-EPS
      IF(THRESH.LE.0.0)THRESH=Y(1)
      DO2110I=1,N
        IF(Y(I).GT.THRESH)THEN
          IFRST=I
          GOTO2119
        ENDIF
 2110 CONTINUE
      IFRST=N+1
 2119 CONTINUE
C
      NUSE=N-IFRST+1
      IF(NUSE.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN GENERALIZED PARETO DEHAAN ESTIMATION--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2121)
 2121   FORMAT('      NO POINTS ARE ABOVE THE THRESHOLD.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2123)THRESH
 2123   FORMAT('      THRESHOLD          = ',G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2125)Y(N)
 2125   FORMAT('      MAXIMUM DATA POINT = ',G15.7)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(Y(IFRST).LT.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2131)
 2131   FORMAT('      NEGATIVE VALUES ENCOUNTERED IN THE INPUT DATA.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      CALL DEHAAN(Y(IFRST),NUSE,THRESH,GAMMA,SDG,KK,ANM1)
C
CCCCC U=Y(IFRST)
      U=THRESH
      IF(GAMMA.GE.0.0)THEN
        RHO1=1.0
      ELSE
        RHO1=1.0/(1.0-GAMMA)
      ENDIF
      A=U*ANM1/RHO1
C
      IWRITE='OFF'
      CALL MEAN(Y(IFRST),NUSE,IWRITE,ZMEAN,IBUGA3,IERROR)
      CALL SD(Y(IFRST),NUSE,IWRITE,ZSD,IBUGA3,IERROR)
      IF(GAMMA.LT.-PPOTTO)THEN
        GAMMA2=-1.0/GAMMA
        DA=DGAMMA(DBLE((GAMMA2+1.0)/GAMMA2))
        DB=DGAMMA(DBLE((GAMMA2+2.0)/GAMMA2)) - DA*DA
        IF(DB.GT.0.0D0)THEN
          SCALE=XSD/REAL(DSQRT(DB))
          ALOC=XMEAN + SCALE*REAL(DA)
        ELSE
          SCALE=0.0
          ALOC=0.0
        ENDIF
      ELSEIF(ABS(GAMMA).LE.PPOTTO)THEN
        SCALE=XSD*SQRT(6.0)/PI
        ALOC=XMEAN - 0.57722*SCALE
      ELSE
      ENDIF
C
C  DEPENDING ON WHAT DEFINITION OF GENERALIZED PARETO PREFERRED,
C  REVERSE SIGN OF GAMMA.
C
      IF(IGEPDF.EQ.'SIMI')THEN
        GAMMSV=GAMMA
        ISIGN1='negative'
        ISIGN2='positive'
      ELSE
        GAMMSV=-GAMMA
        ISIGN1='positive'
        ISIGN2='negative'
      ENDIF
C
C     NOTE THAT LIKELIHOOD IS NOT ALWAYS DEFINED (CAN GET LOG OF
C     NEGATIVE NUMBER).  SO PRINTING IS CONDITIONAL ON THESE VALUES
C     ACTUALLY BEING COMPUTED.
C
      MINMXZ=2
      CALL GEPLI1(Y(IFRST),NUSE,MINMXZ,IGEPDF,
     1            U,A,GAMMSV,
     1            ALIKE,AIC,AICC,BIC,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *********************************
C               **   STEP 42--                 **
C               **   WRITE OUT EVERYTHING      **
C               **   FOR DEHAAN      ESTIMATE  **
C               *********************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEGP')
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
      ITITLE='Generalized Pareto Parameter Estimation (de Haan)'
      NCTITL=49
      ITITLZ='(Maximum Case)'
      NCTITZ=14
      ICNT=1
      ITEXT(ICNT)='Summary Statistics (Full Data Set):'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=XMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=XSD
      IDIGIT(ICNT)=NUMDIG
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
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics for'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Observations Above Threshold:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Threshold:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=THRESH
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations Above Threshold:'
      NCTEXT(ICNT)=39
      AVALUE(ICNT)=REAL(NUSE)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=ZMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=ZSD
      IDIGIT(ICNT)=NUMDIG
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='de Haan Parameter Estimates:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Location (Threshold) Parameter:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=THRESH
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Scale Parameter:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=A
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Shape Parameter (Gamma):'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=GAMMSV
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Standard Deviation of Gamma:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=SDG
      IDIGIT(ICNT)=NUMDIG
      IF(ALIKE.NE.CPUMIN)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Log-likelihood:'
        NCTEXT(ICNT)=15
        AVALUE(ICNT)=ALIKE
        IDIGIT(ICNT)=-7
        ICNT=ICNT+1
        ITEXT(ICNT)='AIC:'
        NCTEXT(ICNT)=4
        AVALUE(ICNT)=AIC
        IDIGIT(ICNT)=-7
        ICNT=ICNT+1
        ITEXT(ICNT)='AICc:'
        NCTEXT(ICNT)=5
        AVALUE(ICNT)=AICC
        IDIGIT(ICNT)=-7
        ICNT=ICNT+1
        ITEXT(ICNT)='BIC:'
        NCTEXT(ICNT)=4
        AVALUE(ICNT)=BIC
        IDIGIT(ICNT)=-7
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(GAMMA.LT.-PPOTTO)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)(1:4)='For '
        WRITE(ITEXT(ICNT)(5:12),'(A8)')ISIGN1
        ITEXT(ICNT)(13:42)=' Gamma, the generalized Pareto'
        NCTEXT(ICNT)=42
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)(1:34)='is equivalent to a reverse Weibull'
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)(1:22)='(SET MINMAX MAX) with:'
        NCTEXT(ICNT)=22
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Shape Parameter (Gamma):'
        NCTEXT(ICNT)=24
        AVALUE(ICNT)=GAMMA2
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Location Parameter:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=ALOC
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Scale Parameter:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=SCALE
        IDIGIT(ICNT)=NUMDIG
      ELSEIF(ABS(GAMMA).LE.PPOTTO)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)(1:40)='For Gamma = zero, the generalized Pareto'
        NCTEXT(ICNT)=40
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)(1:40)='is equivalent to an extreme value type I'
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)(1:14)='(Gumbel) with:'
        NCTEXT(ICNT)=14
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Location Parameter:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=ALOC
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Scale Parameter:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=SCALE
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)(1:4)='For '
        WRITE(ITEXT(ICNT)(5:12),'(A8)')ISIGN2
        ITEXT(ICNT)(13:42)=' Gamma, the generalized Pareto'
        NCTEXT(ICNT)=42
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)(1:28)='is equivalent to a (maximum)'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)(1:31)='extreme value type II (Frechet)'
        NCTEXT(ICNT)=31
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Shape Parameter (Gamma):'
        NCTEXT(ICNT)=24
        AVALUE(ICNT)=GAMMA2
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Location Parameter:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=ALOC
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Scale Parameter:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=SCALE
        IDIGIT(ICNT)=NUMDIG
      ENDIF
C
      NUMROW=ICNT
      DO2320I=1,NUMROW
        NTOT(I)=15
 2320 CONTINUE
C
      IFIRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFIRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      IF(ICAPSW.EQ.'OFF' .AND. ICAPTY.EQ.'TEXT')THEN
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4941)
 4941   FORMAT('GAMMA, SDGAMMA, AND A WILL BE SAVED AS INTERNAL ',
     1         'PARAMETERS.')
        CALL DPWRST('XXX','BUG ')
        IF(GAMMA.LT.-PPOTTO)THEN
          WRITE(ICOUT,4951)
 4951     FORMAT('THE REVERSE WEIBULL PARAMETERS WILL BE SAVED AS')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,4953)
 4953     FORMAT('THE INTERNAL PARAMETERS GAMMA2, LOC, AND SCALE, ',
     1           ' RESPECTIVELY.')
          CALL DPWRST('XXX','WRIT')
        ELSEIF(ABS(GAMMA).LE.PPOTTO)THEN
          WRITE(ICOUT,4961)
 4961     FORMAT('THE GUMBEL PARAMETERS WILL BE SAVED AS THE ',
     1           'INTERNAL PARAMETERS LOC AND SCALE, RESPECTIVELY.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DEGP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEGP--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEHA(IHARG,NUMARG,IDEFHA,
     1IDEXHA,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE DESIGN OF EXPERIMENT HORIZONTAL AXIS
C              (FACTORS OR TERMS)
C              (DEFAULT = FACTORS)
C                     --IHARG  (A  HOLLERITH VECTOR)
C     INPUT  ARGUMENTS--IHARG (A HOLLARITH VECTOR)
C                     --NUMARG
C                     --IDEFHA
C     OUTPUT ARGUMENTS--IDEXHA (A HOLLERITH VARIABLE
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
C     VERSION NUMBER--89/5
C     ORIGINAL VERSION--MAY       1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFHA
      CHARACTER*4 IDEXHA
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
      IF(NUMARG.GT.1)THEN
        IF(NUMARG.EQ.2 .OR. IHARG(3).EQ.'ON' .OR.
     1     IHARG(3).EQ.'OFF' .OR. IHARG(3).EQ.'AUTO' .OR.
     1     IHARG(4).EQ.'DEFA')THEN
          IHOLD=IDEFHA
        ELSE
          IHOLD=IHARG(NUMARG)
        ENDIF
C
        IFOUND='YES'
        IDEXHA=IHOLD
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
 1181     FORMAT('THE DESIGN OF EXPERIMENT HORIZONTAL AXIS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1182)IHOLD
 1182     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEHL(X,Y,Z,D,X2,Y2,NPLOTP,
     1                  XEYE,YEYE,ZEYE,
     1                  XOUT,YOUT,TAGOUT,NOUT,NTRACE,
     1                  ALOWER,AUPPER,XHORIZ,XD,YD,XS,YS,
     1                  XTEMP,YTEMP,DTEMP,IVIS,
     1                  IBUGU2,ISUBRO,IERROR)
C
C     PURPOSE--GIVEN THE VECTORS OF THE ALREADY-TRANSFORMED 3D-TO-2D
C              DATA, DETERMINE WHERE THE HIDDEN LINES ARE AND REMOVE
C              THEM--FORM UPDATED VECTORS CONTAINING ONLY THE VISIBLE
C              LINES.
C     METHOD USED = FLOATING HORIZON
C     REFERENCE--ROGERS, DAVID F. (1985).  PROCEDURAL
C                ELEMENTS FOR COMPUTER GRAPHICS.
C                MCGRAW-HILL, NEW YORK, PAGE 197-201.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/9
C     ORIGINAL VERSION--AUGUST    1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGU2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IWRITE
C
      CHARACTER*4 IVIS
C
CCCCC CHARACTER*4 ICASEF
C
      CHARACTER*4 ICNEAR
      CHARACTER*4 IXCASE
      CHARACTER*4 IYCASE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Z(*)
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D(*)
C
      DIMENSION XOUT(*)
      DIMENSION YOUT(*)
      DIMENSION TAGOUT(*)
C
CCCCC DIMENSION AUPPER(1000)
CCCCC DIMENSION ALOWER(1000)
CCCCC DIMENSION XHORIZ(1000)
C
CCCCC DIMENSION XD(1000)
CCCCC DIMENSION YD(1000)
C
CCCCC DIMENSION XS(1000)
CCCCC DIMENSION YS(1000)
CCCCC DIMENSION IVIS(1000)
C
CCCCC DIMENSION XTEMP(1000)
CCCCC DIMENSION YTEMP(1000)
CCCCC DIMENSION DTEMP(1000)
C
      DIMENSION AUPPER(*)
      DIMENSION ALOWER(*)
      DIMENSION XHORIZ(*)
C
      DIMENSION XD(*)
      DIMENSION YD(*)
C
      DIMENSION XS(*)
      DIMENSION YS(*)
      DIMENSION IVIS(*)
C
      DIMENSION XTEMP(*)
      DIMENSION YTEMP(*)
      DIMENSION DTEMP(*)
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
      ISUBN1='DPDE'
      ISUBN2='HL  '
C
      NHORP=300
CCCCC NHORP=1000
C
      IPASS=0
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEHL')GOTO50
      GOTO90
   50 CONTINUE
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPDEHL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IPASS
   52 FORMAT('IPASS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGU2,ISUBRO,IERROR
   53 FORMAT('IBUGU2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NPLOTP
   54 FORMAT('NPLOTP = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NPLOTP
      WRITE(ICOUT,56)I,X(I),Y(I),Z(I),D(I),X2(I),Y2(I)
   56 FORMAT('I,X(I),Y(I),Z(I),D(I),X2(I),Y2(I) = ',I8,6E13.4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,61)XEYE,YEYE,ZEYE
   61 FORMAT('XEYE,YEYE,ZEYE = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)NOUT,NTRACE
   71 FORMAT('NOUT,NTRACE = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NOUT.LE.0)GOTO79
      DO72I=1,NOUT
      WRITE(ICOUT,73)I,XOUT(I),YOUT(I),TAGOUT(I)
   73 FORMAT('I,XOUT(I),YOUT(I),TAGOUT(I) = ',I8)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
   79 CONTINUE
   90 CONTINUE
C
C           ******************************************
C           **  STEP 11--                           **
C           **  DETERMINE WHICH OF THE 4 CORNERS--  **
C           **     1. (XMIN,YMIN)                   **
C           **     2. (XMAX,YMIN)                   **
C           **     3. (XMIN,YMAX)                   **
C           **     4. (XMAX,YMAX)                   **
C           **  IS CLOSEST TO THE EYE POINT.        **
C           ******************************************
C
      NX=NPLOTP
      NY=NPLOTP
      ND=NPLOTP
C
      CALL DISTIN(X,NX,IWRITE,XD,NXD,IBUGU2,IERROR)
      CALL DISTIN(Y,NY,IWRITE,YD,NYD,IBUGU2,IERROR)
C
      XMIN=XD(1)
      XMAX=XD(1)
      DO1110I=1,NXD
      IF(XD(I).LT.XMIN)XMIN=XD(I)
      IF(XD(I).GT.XMAX)XMAX=XD(I)
 1110 CONTINUE
C
      YMIN=YD(1)
      YMAX=YD(1)
      DO1120I=1,NYD
      IF(YD(I).LT.YMIN)YMIN=YD(I)
      IF(YD(I).GT.YMAX)YMAX=YD(I)
 1120 CONTINUE
C
      XMIN2=(XMIN-XEYE)/(XMAX-XMIN)
      YMIN2=(YMIN-YEYE)/(YMAX-YMIN)
      XMAX2=(XMAX-XEYE)/(XMAX-XMIN)
      YMAX2=(YMAX-YEYE)/(YMAX-YMIN)
      ALEN1=XMIN2**2+YMIN2**2
      ALEN2=XMAX2**2+YMIN2**2
      ALEN3=XMIN2**2+YMAX2**2
      ALEN4=XMAX2**2+YMAX2**2
C
      IF(ALEN1.LE.ALEN2.AND.ALEN1.LE.ALEN3.AND.
     1ALEN1.LE.ALEN4)GOTO1210
      IF(ALEN2.LE.ALEN1.AND.ALEN2.LE.ALEN3.AND.
     1ALEN2.LE.ALEN4)GOTO1220
      IF(ALEN3.LE.ALEN1.AND.ALEN3.LE.ALEN2.AND.
     1ALEN3.LE.ALEN4)GOTO1230
      GOTO1240
C
 1210 CONTINUE
      ICNEAR='X1Y1'
      IXCASE='SL'
      IYCASE='SL'
      GOTO1290
 1220 CONTINUE
      ICNEAR='X2Y1'
      IXCASE='LS'
      IYCASE='SL'
      GOTO1290
 1230 CONTINUE
      ICNEAR='X1Y2'
      IXCASE='SL'
      IYCASE='LS'
      GOTO1290
 1240 CONTINUE
      ICNEAR='X2Y2'
      IXCASE='LS'
      IYCASE='LS'
      GOTO1290
 1290 CONTINUE
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  DETERMINE THE ORDER IN WHICH THE X VALUES   **
C               **  WILL BE PROCESSED.                          **
C               **  DETERMINE THE ORDER IN WHICH THE Y VALUES   **
C               **  WILL BE PROCESSED.                          **
C               **************************************************
C
      IF(IXCASE.EQ.'LS')CALL SORTDE(XD,NXD,XD)
      IF(IYCASE.EQ.'LS')CALL SORTDE(YD,NYD,YD)
C
C               **************************************************
C               **  STEP 13--                                   **
C               **  EXTRACT ALL DISTINCT SCREEN                 **
C               **  HORIZONTAL VALUES.                          **
C               **  SORT THEM LEFT TO RIGHT                     **
C               **  (SMALLEST TO LARGEST).
C               **************************************************
C
      X2MIN=X2(1)
      X2MAX=X2(1)
      DO1300I=1,NPLOTP
      IF(X2(I).LT.X2MIN)X2MIN=X2(I)
      IF(X2(I).GT.X2MAX)X2MAX=X2(I)
 1300 CONTINUE
C
C               **************************************************
C               **  STEP 21--                                   **
C               **  FORM THE VISIBLE TRACES FOR THE Y SLICES--  **
C               **     1. LOOP THROUGH EACH Y TRACE;            **
C               **     2. EXTRACT THE SCREEN HORIZONTAL AND     **
C               **        VERTICAL VALUES FOR A GIVEN SLICE;    **
C               **     3. FORM THE OUTPUT VISIBLE TRACE FOR     **
C               **        A GIVEN SLICE.                        **
C               **************************************************
C
      ILOOP=0
      DO2100IYD=1,NYD
      YTARG=YD(IYD)
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEHL')
     1WRITE(ICOUT,2101)IYD,NYD,YTARG
 2101 FORMAT('SEARCH FOR Y SLICE--IYD,NYD,YTARG = ',2I8,E15.7)
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEHL')
     1CALL DPWRST('XXX','BUG ')
C
      ICOUNT=0
      DO2110I=1,NPLOTP
      IF(Y(I).EQ.YTARG)GOTO2120
      GOTO2110
 2120 CONTINUE
      ICOUNT=ICOUNT+1
      XTEMP(ICOUNT)=X2(I)
      YTEMP(ICOUNT)=Y2(I)
      DTEMP(ICOUNT)=D(I)
 2110 CONTINUE
      NTEMP=ICOUNT
C
      DTARG=CPUMIN
      CALL DPEXSS(XTEMP,YTEMP,DTEMP,NTEMP,DTARG,
     1XS,YS,NS,DHIT,
     1IBUGU2,ISUBRO,IERROR)
C
      IF(NS.LE.1)GOTO2100
      ILOOP=ILOOP+1
C
      IF(IBUGU2.NE.'ON'.AND.ISUBRO.NE.'DEHL')GOTO2139
      WRITE(ICOUT,2131)NS,ILOOP
 2131 FORMAT('Y SLICE--NS,ILOOP = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO2132I=1,NS
      WRITE(ICOUT,2133)I,XS(I),YS(I)
 2133 FORMAT('Y SLICE--I,XS(I),YS(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 2132 CONTINUE
 2139 CONTINUE
C
      CALL DPDEH2(XS,YS,IVIS,NS,ILOOP,
     1XHORIZ,AUPPER,ALOWER,NHORP,X2MIN,X2MAX,
     1XOUT,YOUT,TAGOUT,NOUT,NTRACE,
     1IBUGU2,ISUBRO,IERROR)
C
 2100 CONTINUE
C
C               **************************************************
C               **  STEP 22--                                   **
C               **  FORM THE VISIBLE TRACES FOR THE X SLICES--  **
C               **     1. LOOP THROUGH EACH X TRACE;            **
C               **     2. EXTRACT THE SCREEN HORIZONTAL AND     **
C               **        VERTICAL VALUES FOR A GIVEN SLICE;    **
C               **     3. FORM THE OUTPUT VISIBLE TRACE FOR     **
C               **        A GIVEN SLICE.                        **
C               **************************************************
C
      ILOOP=0
      DO2200IXD=1,NXD
      XTARG=XD(IXD)
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEHL')
     1WRITE(ICOUT,2201)IXD,NXD,XTARG
 2201 FORMAT('SEARCH FOR X SLICE--IXD,NXD,XTARG = ',2I8,E15.7)
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEHL')
     1CALL DPWRST('XXX','BUG ')
C
      ICOUNT=0
      DO2210I=1,NPLOTP
      IF(X(I).EQ.XTARG)GOTO2220
      GOTO2210
 2220 CONTINUE
      ICOUNT=ICOUNT+1
      XTEMP(ICOUNT)=X2(I)
      YTEMP(ICOUNT)=Y2(I)
      DTEMP(ICOUNT)=D(I)
 2210 CONTINUE
      NTEMP=ICOUNT
C
      DTARG=CPUMIN
      CALL DPEXSS(XTEMP,YTEMP,DTEMP,NTEMP,DTARG,
     1XS,YS,NS,DHIT,
     1IBUGU2,ISUBRO,IERROR)
C
      IF(NS.LE.1)GOTO2200
      ILOOP=ILOOP+1
C
      IF(IBUGU2.NE.'ON'.AND.ISUBRO.NE.'DEHL')GOTO2239
      WRITE(ICOUT,2231)NS,ILOOP
 2231 FORMAT('X SLICE--NS,ILOOP = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO2232I=1,NS
      WRITE(ICOUT,2233)I,XS(I),YS(I)
 2233 FORMAT('X SLICE--I,XS(I),YS(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 2232 CONTINUE
 2239 CONTINUE
C
      CALL DPDEH2(XS,YS,IVIS,NS,ILOOP,
     1XHORIZ,AUPPER,ALOWER,NHORP,X2MIN,X2MAX,
     1XOUT,YOUT,TAGOUT,NOUT,NTRACE,
     1IBUGU2,ISUBRO,IERROR)
C
 2200 CONTINUE
C
C               **************************************************
C               **  STEP 90--                                   **
C               **  EXIT.                                       **
C               **************************************************
C
 9000 CONTINUE
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEHL')GOTO9010
      GOTO9090
 9010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPDEHL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IPASS
 9012 FORMAT('IPASS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGU2,ISUBRO,IERROR
 9013 FORMAT('IBUGU2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NPLOTP
 9014 FORMAT('NPLOTP = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NPLOTP
      WRITE(ICOUT,9016)I,X(I),Y(I),Z(I),D(I),X2(I),Y2(I)
 9016 FORMAT('I,X(I),Y(I),Z(I),D(I),X2(I),Y2(I) = ',I8,6E12.4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9021)XEYE,YEYE,ZEYE
 9021 FORMAT('XEYE,YEYE,ZEYE = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)XMIN,XMAX,YMIN,YMAX
 9022 FORMAT('XMIN,XMAX,YMIN,YMAX = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NOUT,NTRACE
 9031 FORMAT('NOUT,NTRACE = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(NOUT.LE.0)GOTO9039
      DO9032I=1,NOUT
      WRITE(ICOUT,9033)I,XOUT(I),YOUT(I),TAGOUT(I)
 9033 FORMAT('I,XOUT(I),YOUT(I),TAGOUT(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9039 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDEH2(XS,YS,IVIS,NS,ILOOP,
     1XHORIZ,AUPPER,ALOWER,NHORP,X2MIN,X2MAX,
     1XOUT,YOUT,TAGOUT,NOUT,NTRACE,
     1IBUGU2,ISUBRO,IERROR)
C
C     PURPOSE--GIVEN THE SCREEN COORDINATES OF A TRACE,
C              AND THE CURRENT HORIZON TABLE,
C              DETERMINE WHAT PART OF THE INPUT TRACE
C              IS VISIBLE, AND
C              FORM THE APPROPRIATE VISIBLE OUTPUT TRACE.
C     METHOD USED = FLOATING HORIZON
C     REFERENCE--ROGERS, DAVID F. (1985).  PROCEDURAL
C                ELEMENTS FOR COMPUTER GRAPHICS.
C                MCGRAW-HILL, NEW YORK, PAGE 197-201.
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     VERSION NUMBER--88/10
C     ORIGINAL VERSION--SEPTEMBER 1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVIS
C
      CHARACTER*4 IBUGU2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASEF
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION XS(1000)
      DIMENSION YS(1000)
      DIMENSION IVIS(1000)
C
      DIMENSION XHORIZ(1000)
      DIMENSION AUPPER(1000)
      DIMENSION ALOWER(1000)
C
      DIMENSION XOUT(*)
      DIMENSION YOUT(*)
      DIMENSION TAGOUT(*)
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
C
      ISUBN1='DPDE'
      ISUBN2='H2  '
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')GOTO50
      GOTO90
   50 CONTINUE
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPDEH2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGU2,ISUBRO,IERROR
   52 FORMAT('IBUGU2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)NS,ILOOP
   55 FORMAT('NS,ILOOP = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO56I=1,NS
      WRITE(ICOUT,57)I,XS(I),YS(I),IVIS(I)
   57 FORMAT('I,XS(I),YS(I),IVIS(I) = ',I8,2E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
   56 CONTINUE
      WRITE(ICOUT,61)NHORP,X2MIN,X2MAX
   61 FORMAT('NHORP,X2MIN,X2MAX = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
      DO62I=1,NHORP
      WRITE(ICOUT,63)I,XHORIZ(I),AUPPER(I),ALOWER(I)
   63 FORMAT('I,XHORIZ(I),AUPPER(I),ALOWER(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
   62 CONTINUE
      WRITE(ICOUT,71)NOUT,NTRACE
   71 FORMAT('NOUT,NTRACE = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO72I=1,NOUT
      WRITE(ICOUT,73)I,XOUT(I),YOUT(I),TAGOUT(I)
   73 FORMAT('I,XOUT(I),YOUT(I),TAGOUT(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
   90 CONTINUE
C
C               **************************************************
C               **  STEP 21--                                   **
C               **  SORT THE INPUT TRACE                        **
C               **  BY THE HORIZONTAL AXIS SCREEN VALUES, AND   **
C               **  CARRY ALONG THE VERTICAL AXIS SCREEN VALUES.**
C               **************************************************
C
      CALL SORTC(XS,YS,NS,XS,YS)
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')GOTO2180
      GOTO2189
 2180 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2181)
 2181 FORMAT('***** FROM THE MIDDLE OF DPDEH2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2182)ILOOP
 2182 FORMAT('      AFTER THE SORT OF NEW SLICE # ',I8)
      CALL DPWRST('XXX','BUG ')
      DO2185I=1,NS
      WRITE(ICOUT,2186)I,XS(I),YS(I)
 2186 FORMAT('I,XS(I),YS(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 2185 CONTINUE
 2189 CONTINUE
C
C               **************************************************
C               **  STEP 22--                                   **
C               **  BRANCH DEPENDING ON WHETHER THIS IS SLICE 1 **
C               **  (THE NEAREST SLICE), OR                     **
C               **  WHETHER IT IS A MORE DISTANT SLICE.         **
C               **************************************************
C
      IF(ILOOP.EQ.1)GOTO3000
      GOTO4000
C
C               **************************************************
C               **  STEP 30--                                   **
C     ----------**  TREAT THE FIRST (= NEAR) SLICE SUBCASE.     **----------
C               **************************************************
C
 3000 CONTINUE
      ISTEPN='30'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               **************************************************
C               **  STEP 31--                                   **
C               **  FOR THE FIRST (= NEAR) SLICE SUBCASE        **
C               **  INITIALIZE THE HORIZON ARRAYS.              **
C               **************************************************
C
      DO3110I=1,NHORP
      AUPPER(I)=CPUMIN
      ALOWER(I)=CPUMAX
 3110 CONTINUE
C
      ANHORP=NHORP
      DO3120I=1,NHORP
      AI=I
      P=(AI-1.0)/(ANHORP-1.0)
      XHORIZ(I)=X2MIN+P*(X2MAX-X2MIN)
 3120 CONTINUE
C
C               **************************************************
C               **  STEP 32--                                   **
C               **  FOR THE FIRST (= NEAR) SLICE SUBCASE        **
C               **  FORM THE OUTPUT DRAW VECTOR.                **
C               **************************************************
C
      NTRACE=NTRACE+1
      DO3200I=1,NS
      NOUT=NOUT+1
      XOUT(NOUT)=XS(I)
      YOUT(NOUT)=YS(I)
      TAGOUT(NOUT)=NTRACE
 3200 CONTINUE
C
C               **************************************************
C               **  STEP 33--                                   **
C               **  FOR THE FIRST (= NEAR) SLICE SUBCASE,       **
C               **  FILL THE UPPER HORIZON TABLES DIRECTLY      **
C               **************************************************
C
      DO3300I=1,NS
      CALL HORIND(XS(I),X2MIN,X2MAX,1,NHORP,I2,IBUGU2,ISUBRO,IERROR)
      IF(YS(I).GT.AUPPER(I2))AUPPER(I2)=YS(I)
CCCCC IF(YS(I).LT.ALOWER(I2))ALOWER(I2)=YS(I)
 3300 CONTINUE
C
C               **************************************************
C               **  STEP 34--                                   **
C               **  FOR THE FIRST (= NEAR) SLICE SUBCASE,       **
C               **  FILL THE LOWER HORIZON TABLES WITH THE      **
C               **  GLOBAL MIN FOR THIS FIRST SLICE.            **
C               **  "PAINTED WALL"                              **
C               **************************************************
C
      Y3MIN=YS(1)
      DO3410I=1,NS
      IF(YS(I).LT.Y3MIN)Y3MIN=YS(I)
 3410 CONTINUE
CCCCC DO3420I=1,NS
      DO3420I=1,NHORP
CCCCC CALL HORIND(XS(I),X2MIN,X2MAX,1,NHORP,I2,IBUGU2,ISUBRO,IERROR)
      I2=I
      IF(Y3MIN.LT.ALOWER(I2))ALOWER(I2)=Y3MIN
 3420 CONTINUE
C
C               **************************************************
C               **  STEP 35--                                   **
C               **  FOR THE FIRST (= NEAR) SLICE SUBCASE,       **
C               **  FILL IN (INTERPOLATE) THE HORIZON TABLES    **
C               **  (IF NEEDED).                                **
C               **************************************************
C
      I=1
      CALL HORIND(XS(I),X2MIN,X2MAX,1,NHORP,IPREV,IBUGU2,ISUBRO,IERROR)
      DO3500I=2,NS
      CALL HORIND(XS(I),X2MIN,X2MAX,1,NHORP,ICUR,IBUGU2,ISUBRO,IERROR)
      IDEL=ICUR-IPREV
      ICASEF='BOTH'
      IF(IDEL.GE.2)
     1CALL FILLHT(IPREV,ICUR,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      IPREV=ICUR
 3500 CONTINUE
C
      GOTO9000
C
C               ******************************************************
C               **  STEP 40--                                       **
C     ----------**  TREAT THE OTHER (= FARTHER AWAY) SLICE SUBCASE  **----------
C               ******************************************************
C
 4000 CONTINUE
      ISTEPN='40'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************************
C               **  STEP 41--                                      **
C               **  FOR THE OTHER (= FARTHER AWAY) SLICE SUBCASE,  **
C               **  DETERMINE VISIBILITY.                          **
C               *****************************************************
C
      DO4100I=1,NS
      CALL HORIND(XS(I),X2MIN,X2MAX,1,NHORP,I2,IBUGU2,ISUBRO,IERROR)
      IVIS(I)='YES'
      IF(YS(I).LT.AUPPER(I2).AND.YS(I).GT.ALOWER(I2))IVIS(I)='NO'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')
     1WRITE(ICOUT,4111)I,YS(I),IVIS(I),I2,ALOWER(I2),AUPPER(I2)
 4111 FORMAT('I,YS(I),IVIS(I),I2,ALOWER(I2),AUPPER(I2) = ',
     1I8,E15.7,2X,A4,I8,2E15.7)
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')
     1CALL DPWRST('XXX','BUG ')
 4100 CONTINUE
C
C               *****************************************************
C               **  STEP 42--                                      **
C               **  FOR THE OTHER (= FARTHER AWAY) SLICE SUBCASE,  **
C               **  FORM THE OUTPUT DRAW VECTOR,                   **
C               **  FILL THE HORIZON TABLES,                       **
C               **  FILL IN THE HORIZON TABLES (IF NEEDED)         **
C               *****************************************************
C
      DO4200I=1,NS
C
      IF(I.EQ.1)GOTO4210
      GOTO4220
C
 4210 CONTINUE
      CALL HORIND(XS(I),X2MIN,X2MAX,1,NHORP,ICHORI,IBUGU2,ISUBRO,IERROR)
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')
     1WRITE(ICOUT,777)I,YS(I),IVIS(I),I2,ALOWER(I2),AUPPER(I2)
  777 FORMAT('I,YS(I),IVIS(I),I2,ALOWER(I2),AUPPER(I2) = ',
     1I8,E15.7,2X,A4,I8,2E15.7)
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')
     1CALL DPWRST('XXX','BUG ')
      IF(IVIS(I).EQ.'YES')GOTO4211
      GOTO4219
 4211 CONTINUE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XS(I)
      YOUT(NOUT)=YS(I)
      TAGOUT(NOUT)=NTRACE
 4219 CONTINUE
      IPHORI=ICHORI
      GOTO4200
C
 4220 CONTINUE
      IP=I-1
      IC=I
      CALL HORIND(XS(I),X2MIN,X2MAX,1,NHORP,ICHORI,IBUGU2,ISUBRO,IERROR)
      IPASS=IPASS+1
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')WRITE(ICOUT,4221)
CCCCC1IPASS,ILOOP,I,IP,YTARG,XS(IP)
     1IPASS,ILOOP,I,IP,XS(IP)
 4221 FORMAT('FROM DPDEH2, BEFORE CALL TO DPDETR--',
CCCCC1'IPASS,ILOOP,I,IP,YTARG,XS(IP) = ',4I8,2E15.7)
     1'IPASS,ILOOP,I,IP,XS(IP) = ',4I8,2E15.7)
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')CALL DPWRST('XXX','BUG ')
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')WRITE(ICOUT,4222)
     1I,IP,IC,IVIS(IP),IVIS(IC)
 4222 FORMAT('I,IP,IC,IVIS(IP),IVIS(IC) = ',3I8,2X,A4,2X,A4)
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')CALL DPWRST('XXX','BUG ')
      CALL DPDETR(IP,IC,XS,YS,IVIS,NS,
     1IPHORI,ICHORI,AUPPER,ALOWER,XHORIZ,NHORP,
     1X2MIN,X2MAX,IPASS,
     1XOUT,YOUT,TAGOUT,NOUT,NTRACE,
     1IBUGU2,ISUBRO,IERROR)
      IPHORI=ICHORI
C
 4200 CONTINUE
C
      GOTO9000
C
C               **************************************************
C               **  STEP 90--                                   **
C               **  EXIT.                                       **
C               **************************************************
C
 9000 CONTINUE
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DEH2')GOTO9010
      GOTO9090
 9010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPDEH2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGU2,ISUBRO,IERROR
 9012 FORMAT('IBUGU2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NS,ILOOP
 9015 FORMAT('NS,ILOOP = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9016I=1,NS
      WRITE(ICOUT,9017)I,XS(I),YS(I),IVIS(I)
 9017 FORMAT('I,XS(I),YS(I),IVIS(I) = ',I8,2E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9016 CONTINUE
      WRITE(ICOUT,9021)NHORP,X2MIN,X2MAX
 9021 FORMAT('NHORP,X2MIN,X2MAX = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9022I=1,NHORP
      WRITE(ICOUT,9023)I,XHORIZ(I),AUPPER(I),ALOWER(I)
 9023 FORMAT('I,XHORIZ(I),AUPPER(I),ALOWER(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
      WRITE(ICOUT,9024)IPHORI,ICHORI
 9024 FORMAT('IPHORI,ICHORI = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NOUT,NTRACE
 9031 FORMAT('NOUT,NTRACE = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9032I=1,NOUT
      WRITE(ICOUT,9033)I,XOUT(I),YOUT(I),TAGOUT(I)
 9033 FORMAT('I,XOUT(I),YOUT(I),TAGOUT(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDELE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
CCCCC SUBROUTINE DPDELE(IBUGS2,IBUGQ,IFOUND,IERROR)
CCCCC THE THIRD ARGUMENT (ISUBRO) ABOVE WAS ADDED   SEPTEMBER 1995
C
C     PURPOSE--TREAT THE DELETE CASE--
C              DELETE SPECIFIED ELEMENTS OF A VARIABLE
C              AND PACK THE REMAINING ELEMENTS
C              INTO THE FIRST AVAILABLE LOCATIONS;
C              REDEFINE THE LENGTH OF THE PACKED VARIABLE.
C     INPUT --NECESSARILY A VARIABLE.
C     OUTPUT--NECESSARILY A VARIABLE.
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
C     ORIGINAL VERSION--MARCH     1978.
C     UPDATED         --MAY       1978.
C     UPDATED         --JUNE      1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   1993. ADD DELETE MATRIX.
C     UPDATED         --FEBRUARY  1994. EQUIVALENCE TO GARBAGE COMMON
C     UPDATED         --DECEMBER  1994. SUPP. ERROR MESS. IF NOT EXIST
C     UPDATED         --SEPTEMBER 1995. ALLOW   DELETE Y7 TO Y15
C     UPDATED         --OCTOBER   1997. REINITIALIZE DELETED VALUES TO
C                                       0 INSTEAD OF CPUMIN.
C     UPDATED         --JANUARY   2000. SUPPORT FOR VARIABLE LABELS
C     UPDATED         --JULY      2009. SUPPORT "Y1 TO Y1". THIS CAN
C                                       BE USEFUL IN MACROS WHERE THE
C                                       THE NUMBER OF VARIABLES IS NOT
C                                       KNOWN IN ADVANCE
C     UPDATED         --JANUARY   2012. SUPPORT DELETION OF STRINGS/
C                                       FUNCTIONS
C     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
C     UPDATED         --SEPTEMBER 2015. DELETE FUNCTION BLOCK
C     UPDATED         --AUGUST    2016. DELETE STATISTIC BLOCK
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
CCCCC THE FOLLOWING LINE WAS ADDED    SEPTEBMER 1995
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASEQ
      CHARACTER*4 ISTRIN
      CHARACTER*4 ISTRI2
      CHARACTER*4 INEX
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IFOUCO
      CHARACTER*4 IFOULP
      CHARACTER*4 IFOURP
      CHARACTER*4 IFOURN
      CHARACTER*4 IFOUVN
      CHARACTER*4 IHVARJ
      CHARACTER*4 IHVRJ2
      CHARACTER*4 IVN
      CHARACTER*4 IVN2
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
      CHARACTER*4 IERRO1
      CHARACTER*4 ITYPCO
      CHARACTER*4 IHOLCO
      CHARACTER*4 IHLCO2
      CHARACTER*4 ITYPLP
      CHARACTER*4 IHOLLP
      CHARACTER*4 IHLLP2
      CHARACTER*4 ITYPRP
      CHARACTER*4 IHOLRP
      CHARACTER*4 IHLRP2
      CHARACTER*4 ITYPRN
      CHARACTER*4 IHOLRN
      CHARACTER*4 IHLRN2
      CHARACTER*4 ITYPVN
      CHARACTER*4 IHOLVN
      CHARACTER*4 IHLVN2
C
CCCCC THE FOLLOWING 16 LINES WERE ADDED   SEPTEMBER 1995
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 ICASTO
      CHARACTER*4 IECASE
C
      CHARACTER*4 JVNAM1
      CHARACTER*4 JPNAM1
      CHARACTER*4 JMNAM1
      CHARACTER*4 JFNAM1
      CHARACTER*4 JUNAM1
      CHARACTER*4 JENAM1
C
      CHARACTER*4 JVNAM2
      CHARACTER*4 JPNAM2
      CHARACTER*4 JMNAM2
      CHARACTER*4 JFNAM2
      CHARACTER*4 JUNAM2
      CHARACTER*4 JENAM2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
CCCCC APRIL 1996.  ADD FOLLOWING LINE
      CHARACTER*4 ICASEA
      CHARACTER*4 NEWNAM
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
C
      PARAMETER (MAXDEL=100)
      DIMENSION ILISTV(MAXDEL)
      DIMENSION TEMP(MAXOBV)
C
      DIMENSION IVN(MAXDEL)
      DIMENSION IVN2(MAXDEL)
      DIMENSION IRN(MAXDEL)
C
      DIMENSION JVNAM1(100)
      DIMENSION JPNAM1(100)
      DIMENSION JMNAM1(100)
      DIMENSION JFNAM1(100)
      DIMENSION JUNAM1(100)
      DIMENSION JENAM1(100)
C
      DIMENSION JVNAM2(100)
      DIMENSION JPNAM2(100)
      DIMENSION JMNAM2(100)
      DIMENSION JFNAM2(100)
      DIMENSION JUNAM2(100)
      DIMENSION JENAM2(100)
C
      DIMENSION NIV(100)
C
      DIMENSION IECOL2(100)
      DIMENSION IECASE(100)
      DIMENSION PVAL(100)
      DIMENSION IFSTA2(100)
      DIMENSION IFSTO2(100)
C
      EQUIVALENCE (GARBAG(IGARB1),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB2),PVAL(1))
      EQUIVALENCE (IGARBG(IIGAR1),ILISTV(1))
      EQUIVALENCE (IGARBG(IIGAR1+MAXDEL),IVN(1))
      EQUIVALENCE (IGARBG(IIGAR1+2*MAXDEL),IVN2(1))
      EQUIVALENCE (IGARBG(IIGAR1+3*MAXDEL),IRN(1))
      EQUIVALENCE (IGARBG(IIGAR1+4*MAXDEL),NIV(1))
      EQUIVALENCE (IGARBG(IIGAR1+5*MAXDEL),IECOL2(1))
      EQUIVALENCE (IGARBG(IIGAR1+6*MAXDEL),IECASE(1))
      EQUIVALENCE (IGARBG(IIGAR1+7*MAXDEL),IFSTA2(1))
      EQUIVALENCE (IGARBG(IIGAR1+8*MAXDEL),IFSTO2(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOFB.INC'
      INCLUDE 'DPCOSB.INC'
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
      ISUBN1='DPDE'
      ISUBN2='LE  '
C
      IPASS=0
      NUMDEL=0
      ISAVE=0
      IROD1O=0
      IRODNO=0
      IROW1O=0
      IROWNO=0
      ILQP1=0
C
      TEMPD=0.0
      VALD1O=0.0
      VALDNO=0.0
      VAL1O=0.0
      VALNO=0.0
CCCCC FEBRUARY 1993.  ADD FOLLOWING INITIALIZATION
      DO30I=1,100
      ILISTV(I)=0
 30   CONTINUE
C
C               *************************************************
C               **  TREAT THE DELETE CASE                      **
C               **  DELETE SPECIFIC ELEMENTS OF A VECTOR       **
C               **  AND PACK REMAINING ELEMENTS                **
C               **  INTO THE FIRST AVAILABLE LOCATIONS.        **
C               *************************************************
C
      IFOUND='YES'
      IERROR='NO'
C
CCCCC MAXDEL=100
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
CCCCC THE FOLLOWING 6 LINES WERE ADDED   SEPTEMBER 1995
      MAXV2=MAXDEL
      MAXP2=MAXDEL
      MAXM2=MAXDEL
      MAXF2=MAXDEL
      MAXU2=MAXDEL
      MAXE2=MAXDEL
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'DELE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDELE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,IERROR,MAXDEL
   52   FORMAT('IBUGS2,IERROR,MAXDEL = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)MAXNAM,NUMNAM,MAXN,MAXCOL,NUMCOL
   54   FORMAT('MAXNAM,NUMNAM,MAXN,MAXCOL,NUMCOL = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO60I=1,NUMNAM
          WRITE(ICOUT,61)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                   IVALUE(I),VALUE(I)
   61     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1    I8,2X,A4,A4,2X,A4,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,62)I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)
   62     FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',
     1    I8,2X,A4,A4,6X,I8,I8,I8)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO70J=1,NUMCOL
          IJ=MAXN*(J-1)+1
          WRITE(ICOUT,71)J,MAXN,IJ,V(IJ)
   71     FORMAT('J,MAXN,IJ,V(IJ) = ',3I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
C               *******************************************************
C               **  STEP 1--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LT.1)THEN
        IERROR='YES'
        GOTO8900
      ENDIF
      IFOUND='YES'
C
      ISTEPN='1B'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2015/09: DELETE FUNCTION BLOCK CASE
C
      IF(NUMARG.GE.2)THEN
        IF(IHARG(1).EQ.'FUNC' .AND. IHARG(2).EQ.'BLOC')THEN
          IF(IHARG(3).EQ.'ONE' .OR. IHARG(3).EQ.'1')THEN
            IFBNA1=' '
            IFBAN1=' '
            IFBCN1=0
            IFBCP1=0
            DO110I=1,MAXFBP
              IFBPL1(I)=' '
  110       CONTINUE
            DO112I=1,MAXFBL
              IFBLI1(I)=' '
  112       CONTINUE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,114)
  114       FORMAT('FUNCTION BLOCK ONE CLEARED.')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(IHARG(3).EQ.'TWO' .OR. IHARG(3).EQ.'2')THEN
            IFBNA2=' '
            IFBAN2=' '
            IFBCN2=0
            IFBCP2=0
            DO120I=1,MAXFBP
              IFBPL2(I)=' '
  120       CONTINUE
            DO122I=1,MAXFBL
              IFBLI2(I)=' '
  122       CONTINUE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,124)
  124       FORMAT('FUNCTION BLOCK TWO CLEARED.')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(IHARG(3).EQ.'THRE' .OR. IHARG(3).EQ.'3')THEN
            IFBNA3=' '
            IFBAN3=' '
            IFBCN3=0
            IFBCP3=0
            DO130I=1,MAXFBP
              IFBPL3(I)=' '
  130       CONTINUE
            DO132I=1,MAXFBL
              IFBLI3(I)=' '
  132       CONTINUE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,134)
  134       FORMAT('FUNCTION BLOCK THREE CLEARED.')
            CALL DPWRST('XXX','BUG ')
          ELSE
            IFBNA1=' '
            IFBAN1=' '
            IFBNA2=' '
            IFBAN2=' '
            IFBNA3=' '
            IFBAN3=' '
            IFBCN1=0
            IFBCN2=0
            IFBCN3=0
            IFBCP1=0
            IFBCP2=0
            IFBCP3=0
            DO140I=1,MAXFBP
              IFBPL1(I)=' '
              IFBPL2(I)=' '
              IFBPL3(I)=' '
  140       CONTINUE
            DO142I=1,MAXFBL
              IFBLI1(I)=' '
              IFBLI2(I)=' '
              IFBLI3(I)=' '
  142       CONTINUE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,144)
  144       FORMAT('FUNCTION BLOCKS ONE, TWO AND THREE CLEARED.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IFOUND='YES'
          GOTO9000
        ELSEIF(IHARG(1).EQ.'STAT' .AND. IHARG(2).EQ.'BLOC')THEN
          IF(IHARG(3).EQ.'ONE' .OR. IHARG(3).EQ.'1')THEN
            ISBNA1=' '
            ISBAN1=' '
            ISBCN1=0
            ISBCP1=0
            DO160I=1,MAXSBP
              ISBPL1(I)=' '
  160       CONTINUE
            DO162I=1,MAXSBL
              ISBLI1(I)=' '
  162       CONTINUE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,164)
  164       FORMAT('STATISTIC BLOCK ONE CLEARED.')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(IHARG(3).EQ.'TWO' .OR. IHARG(3).EQ.'2')THEN
            ISBNA2=' '
            ISBAN2=' '
            ISBCN2=0
            ISBCP2=0
            DO170I=1,MAXSBP
              ISBPL2(I)=' '
  170       CONTINUE
            DO172I=1,MAXFBL
              ISBLI2(I)=' '
  172       CONTINUE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,174)
  174       FORMAT('STATISTIC BLOCK TWO CLEARED.')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(IHARG(3).EQ.'THRE' .OR. IHARG(3).EQ.'3')THEN
            ISBNA3=' '
            ISBAN3=' '
            ISBCN3=0
            ISBCP3=0
            DO180I=1,MAXSBP
              ISBPL3(I)=' '
  180       CONTINUE
            DO182I=1,MAXFBL
              ISBLI3(I)=' '
  182       CONTINUE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,184)
  184       FORMAT('STATISTIC BLOCK THREE CLEARED.')
            CALL DPWRST('XXX','BUG ')
          ELSE
            ISBNA1=' '
            ISBAN1=' '
            ISBNA2=' '
            ISBAN2=' '
            ISBNA3=' '
            ISBAN3=' '
            ISBCN1=0
            ISBCN2=0
            ISBCN3=0
            ISBCP1=0
            ISBCP2=0
            ISBCP3=0
            DO190I=1,MAXSBP
              ISBPL1(I)=' '
              ISBPL2(I)=' '
              ISBPL3(I)=' '
  190       CONTINUE
            DO192I=1,MAXSBL
              ISBLI1(I)=' '
              ISBLI2(I)=' '
              ISBLI3(I)=' '
  192       CONTINUE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,194)
  194       FORMAT('STATISTIC BLOCKS ONE, TWO AND THREE CLEARED.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IFOUND='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C           ***********************************************************
C           **  STEP 2--                                             **
C           **  DETERMINE THE SUBCASE BASED ON THE QUALIFIER.        **
C           **  SCAN TO CHECK IF 'SUBSET' OR 'FOR' IS PRESENT.       **
C           **  IF NOT PRESENT, THEN HAVE CASE 1--                   **
C           **  EXAMPLE--DELETE X(4) Y(1) Z(46)                      **
C           **  IF PRESENT, THEN HAVE CASE 2--                       **
C           **  EXAMPLE--DELETE X Y Z FOR I = 1 1 10                 **
C           **  DETERMINE THE LOCATION IN THE ARGUMENT LIST          **
C           **  OF 'SUBSET' OR 'FOR'.                                **
C           **  BRANCH TO THE APPROPRIATE SUBCASE                    **
C           **  FULL VERSUS SUBSET/FOR.                              **
C           ***********************************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCQ=1
      ICASEQ='UNKN'
      IF(NUMARG.LE.0)GOTO290
      DO210J=1,NUMARG
        J2=J
        IF((IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') .OR.
     1     (IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  '))THEN
          ICASEQ='SUBS'
          ILOCQ=J2
          GOTO290
        ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
          ICASEQ='FOR'
          ILOCQ=J2
          GOTO290
        ENDIF
  210 CONTINUE
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
C
  290 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED                 SEPTEMBER 1995
CCCCC TO ALLOW COMMANDS SUCH AS    DELETE Y1 TO Y10   SEPTEMBER 1995
C       ****************************************************************
C       **  STEP 2.5--
C       **  TREAT THE     TO    KEYWORD CASE
C       **  AS IN    DELETE Y1 TO Y10
C       **  EXPAND SUCH LINES LITERALLY.
C       **
C       **  DETERMINE THE TYPE AND NUMBER OF ITEMS
C       **  TO BE DELETED   .
C       **  NUMALL = TOTAL NUMBER OF DELETED  ITEMS
C       **           (AS DETERMINED BY INCLUDING ONLY ALL BEFORE
C       **           'SUBSET' OR 'EXCEPT' OR 'FOR')
C       **  NUMV   = NUMBER OF VARIABLES TO BE DELETED    ;
C       **  NUMP   = NUMBER OF PARAMETERS TO BE DELETED    ;
C       **  NUMM   = NUMBER OF MODELS TO BE DELETED     (SHOULD = 0 OR 1)
C       **  NUMF   = NUMBER OF FUNCTIONS TO BE DELETED
C       **  NUMU   = NUMBER OF UNKNOWNS TO BE DELETED    ;
C       **  NUME   = TOTAL NUMBER OF DELETED  ITEMS (SHOULD = NUMALL);
C       ****************************************************************
C
      ISTEPN='2B'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IV=0
      IP=0
      IM=0
      IF=0
      IU=0
      IE=0
C
      JMIN=1
      JMAX=ILOCQ-1
C
      IVALMA=0
      NUMALL=0
      NUMALL=JMAX-JMIN+1
      IF(JMIN.GT.JMAX)GOTO4290
C
      IISKIP=0
C
      DO4200J=JMIN,JMAX
C
        IF(IISKIP.EQ.1)THEN
          IISKIP=0
          GOTO4200
        ENDIF
C
        IH1=IHARG(J)
        IH2=IHARG2(J)
C
        ICASTO='OFF'
        IF(IH1.NE.'TO  ')GOTO4220
C
        ICASTO='ON'
        JM1=J-1
        JP1=J+1
        CALL DPEXTL(IHARG(JM1),IHARG2(JM1),IHARG(JP1),IHARG2(JP1),
     1              KNUMB,IVAL1,IVAL2,IBUGS2,ISUBRO,IERROR)
C
        IF(IVAL1.EQ.IVAL2)THEN
          IISKIP=1
          GOTO4200
        ENDIF
C
        IVA1P1=IVAL1+1
        IVA2M1=IVAL2-1
        IF(IVA1P1.GT.IVA2M1)GOTO4200
        IVAL=IVAL1
C
 4215   CONTINUE
        IVAL=IVAL+1
        IF(IVAL.GE.IVAL2)GOTO4200
C
        CALL DPAPNU(IHARG(JM1),IHARG2(JM1),KNUMB,IVAL,
     1              IH1,IH2,IBUGS2,ISUBRO,IERROR)
C
 4220   CONTINUE
        ICASEA='    '
        DO4300I=1,NUMNAM
          I2=I
          IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
            IF(IUSE(I).EQ.'V')THEN
              ICASEA='V'
              IV=IV+1
              IF(IV.GT.MAXV2)GOTO4370
              JVNAM1(IV)=IH1
              JVNAM2(IV)=IH2
              NIV(IV)=IN(I2)
              GOTO4370
            ELSEIF(IUSE(I).EQ.'P')THEN
              ICASEA='P'
              IP=IP+1
              IF(IP.GT.MAXP2)GOTO4370
              JPNAM1(IP)=IH1
              JPNAM2(IP)=IH2
              PVAL(IP)=VALUE(I2)
              GOTO4370
            ELSEIF(IUSE(I).EQ.'M')THEN
              ICASEA='M'
              IM=IM+1
              IF(IM.GT.MAXM2)GOTO4370
              JMNAM1(IM)=IH1
              JMNAM2(IM)=IH2
              GOTO4370
            ELSEIF(IUSE(I).EQ.'F')THEN
              ICASEA='F'
              IF=IF+1
              IF(IF.GT.MAXF2)GOTO4370
              JFNAM1(IF)=IH1
              JFNAM2(IF)=IH2
              IFSTA2(IF)=IVSTAR(I2)
              IFSTO2(IF)=IVSTOP(I2)
              GOTO4370
            ENDIF
          ENDIF
 4300   CONTINUE
        ICASEA='U'
        IU=IU+1
        IF(IU.LE.MAXU2)THEN
          JUNAM1(IU)=IH1
          JUNAM2(IU)=IH2
        ENDIF
C
 4370   CONTINUE
        IE=IE+1
C
        IF(IE.GT.MAXE2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4381)
 4381     FORMAT('***** ERROR IN DELETE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4382)
 4382     FORMAT('      THE NUMBER OF NAMES IN THE DELETE COMMAND HAS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4383)MAXE2
 4383     FORMAT('      JUST EXCEEDED THE ALLOWABLE MAXIMUM (',I5,')')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        JENAM1(IE)=IH1
        JENAM2(IE)=IH2
        IECASE(IE)='NEW'
        IF(ICASEA.EQ.'V')IECASE(IE)='OLD'
        IECOL2(IE)=-1
        IF(ICASEA.EQ.'V')IECOL2(IE)=IVALUE(I2)
        IF(ICASEA.EQ.'P')IECASE(IE)='OLD'
        IF(ICASEA.EQ.'F')IECASE(IE)='OLD'
        IF(ICASTO.EQ.'ON')GOTO4215
C
 4200 CONTINUE
 4290 CONTINUE
      NUMV=IV
      NUMP=IP
      NUMM=IM
      NUMF=IF
      NUMU=IU
      NUME=IE
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'DELE')THEN
        ISTEPN='2C'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,4411)NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME
 4411   FORMAT('NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME = ',7I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO4420II=1,IE
          WRITE(ICOUT,4421)II,JVNAM1(II),JVNAM2(II),
     1                        JPNAM1(II),JPNAM2(II),
     1                        JMNAM1(II),JMNAM2(II),
     1                        JFNAM1(II),JFNAM2(II),
     1                        JUNAM1(II),JUNAM2(II)
 4421     FORMAT(I5,5X,2A4,1X,2A4,1X,2A4,1X,2A4,1X,2A4)
          CALL DPWRST('XXX','BUG ')
 4420   CONTINUE
        WRITE(ICOUT,4499)ICASEQ
 4499   FORMAT('ICASEQ = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASEQ.EQ.'SUBS')GOTO7000
      IF(ICASEQ.EQ.'FOR')GOTO7000
C
C            ***********************************************************
C            **  STEP 3--                                             **
C            **  FOR THE FULL CASE,                                   **
C            **  EXTRACT EACH VARIABLE NAME AND EACH ARGUMENT VALUE.  **
C            ***********************************************************
C
  300 CONTINUE
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=0
      IPASS=IPASS+1
C
      IF(IPASS.LT.1 .OR. IPASS.GT.MAXDEL)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
  301   FORMAT('***** ERROR IN DELETE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)
  302   FORMAT('      THE DELETE COMMAND REQUIRES THAT THE NUMBER OF ',
     1         'ITEMS TO BE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,304)IPASS
  304   FORMAT('      DELETED BE BETWEEN 1 AND ',I8,' (INCLUSIVE);')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,305)NUMDEL
  305   FORMAT('      THE SPECIFIED NUMBER WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,306)
  306   FORMAT('      THE INPUT COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
  307     FORMAT('      ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO8900
      ENDIF
C
      IF(IPASS.GE.2)ISAVE=IENDRP
C
C           ************************************************************
C           **  STEP 3.1--                                            **
C           **  IF THIS IS THE FIRST PASS ON THIS LINE (AND ONLY FOR  **
C           **  PASS 1) SEARCH FOR DELETE (OTHERWISE SKIP THIS STEP)  **
C           **  SEARCH BETWEEN COLUMN 1 AND THE END OF THE LINE       **
C           **  (INCLUSIVE).                                          **
C           ************************************************************
C
      ISTEPN='3.1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPASS.EQ.1)THEN
C
        ISTAR1=1
        ISTOP1=IWIDTH
        ISTRIN='DELE'
        ISTRI2='TE  '
        INEX='II'
        CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1               IFOUCO,IBEGCO,IENDCO,
     1               ITYPCO,IHOLCO,IHLCO2,INT1CO,FLOACO,IERRO1)
        IF(IFOUCO.NE.'YES')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,312)
  312     FORMAT('      THE WORD      DELETE      NOT FOUND')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,313)
  313     FORMAT('      ON THE ENTERED INPUT COMMAND LINE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,306)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO8900
        ENDIF
      ENDIF
C
C            ***********************************************************
C            **  STEP 3.2--                                           **
C            **  SEARCH FOR LEFT PARENTHESIS;                         **
C            **  IF THIS IS THE FIRST PASS FOR THIS LINE,             **
C            **  SEARCH BETWEEN    DELETE     AND      END OF LINE    **
C            **  (IF NO LEFT PARENTHESIS FOUND AT ALL, JUMP TO 7000). **
C            **  IF THIS IS THE SECOND (OR HIGHER) PASS FOR THIS LINE,**
C            **  SEARCH BETWEEN    PREVIOUS RIGHT PARENTHESIS AND     **
C            **  END OF LINE.                                         **
C            ***********************************************************
C
      ISTEPN='3.2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPASS.LE.1)ISTAR1=IENDCO+1
      IF(IPASS.GE.2)ISTAR1=ISAVE+1
      ISTOP1=IWIDTH
      ISTRIN='('
      INEX='II'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOULP,IBEGLP,IENDLP,
     1             ITYPLP,IHOLLP,IHLLP2,INT1LP,FLOALP,IERRO1)
      IF(IFOULP.EQ.'NO')THEN
        IF(IPASS.GE.2)GOTO399
        GOTO7000
      ENDIF
C
C               ********************************************************
C               **  STEP 3.3--                                        **
C               **  SEARCH FOR RIGHT PARENTHESIS;  SEARCH BETWEEN     **
C               **  LEFT PARENTHESIS     AND    END OF LINE.          **
C               ********************************************************
C
      ISTEPN='3.3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTAR1=IENDLP+1
      ISTOP1=IWIDTH
      ISTRIN=')'
      INEX='II'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOURP,IBEGRP,IENDRP,
     1             ITYPRP,IHOLRP,IHLRP2,INT1RP,FLOARP,IERRO1)
      IF(IFOURP.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,342)
  342   FORMAT('      WHEN THE DELETE COMMAND IS USED WITHOUT A SUBSET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,344)
  344   FORMAT('      QUALIFICATION OR WITHOUT A FOR  QUALIFICATION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,345)
  345   FORMAT('      THEN ONLY INDIVIDUAL ELEMENTS OF A VARIABLE MAY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,347)
  347   FORMAT('      BE DELETED.  SUCH INDIVIDUAL ELEMENTS ARE ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,348)
  348   FORMAT('      SPECIFIED BY A VARIABLE NAME FOLLOWED BY A')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,349)
  349   FORMAT('      PAIR OF PARENTHSES WITH A ROW NUMBER WITHIN;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,350)
  350   FORMAT('      HOWEVER, A RIGHT PARENTHESIS IS MISSING HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,306)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO8900
      ENDIF
C
C               ********************************************************
C               **  STEP 3.4--                                        **
C               **  SEARCH FOR ROW NUMBER;  SEARCH BETWEEN            **
C               **  LEFT PARENTHESIS     AND     RIGHT PARENTH        **
C               ********************************************************
C
      ISTEPN='3.4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTAR1=IENDLP
      ISTOP1=IENDRP
      ISTRIN='(;)'
      INEX='EE'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOURN,IBEGRN,IENDRN,
     1             ITYPRN,IHOLRN,IHLRN2,INT1RN,FLOARN,IERRO1)
      IF(IFOURN.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,342)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,344)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,345)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,347)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,348)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,349)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,370)
  370   FORMAT('      HOWEVER, A ROW NUMBER IS MISSING HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,306)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO8900
      ENDIF
C
C            ***********************************************************
C            **  STEP 3.5--                                           **
C            **  SEARCH FOR VARIABLE NAME;                            **
C            **  IF THIS IS THE FIRST PASS FOR THIS LINE, SEARCH      **
C            **  BETWEEN    DELETE     AND      LEFT PARENTHESIS;  IF **
C            **  THIS IS THE SECOND (OR HIGHER) PASS FOR THIS LINE,   **
C            **  SEARCH BETWEEN    PREVIOUS RIGHT PARENTHESIS AND     **
C            **  THE NEXT LEFT PARENTHESIS.                           **
C            ***********************************************************
C
      ISTEPN='3.5'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPASS.LE.1)ISTAR1=IENDCO+1
      IF(IPASS.GE.2)ISTAR1=ISAVE+1
      ISTOP1=IENDLP
      ISTRIN='!;('
      INEX='IE'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOUVN,IBEGVN,IENDVN,
     1             ITYPVN,IHOLVN,IHLVN2,INT1VN,FLOAVN,IERRO1)
      IF(IFOUVN.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,342)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,344)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,345)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,347)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,348)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,349)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,390)
  390   FORMAT('      HOWEVER, A VARIABLE NAME IS MISSING HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,306)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO8900
      ENDIF
C
      IVN(IPASS)=IHOLVN
      IVN2(IPASS)=IHLVN2
      IRN(IPASS)=INT1RN
C
      GOTO300
C
  399 CONTINUE
      NUMDEL=IPASS-1
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  FOR THE FULL CASE, CHECK TO MAKE SURE ALL         **
C               **  VARIABLES WITH DELETIONS ARE, IN FACT, IN THE     **
C               **  INTERNAL LIST, AND ARE, IN FACT, VARIABLES        **
C               **  (AS OPPOSED TO PARAMETERS).                       **
C               ********************************************************
C
  400 CONTINUE
C
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO420J=1,NUMDEL
        J2=J
        IHVARJ=IVN(J)
        IHVRJ2=IVN2(J)
        DO430I=1,NUMNAM
          I2=I
          IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'V')THEN
            ILISTV(J2)=I2
            GOTO420
          ELSEIF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'M')THEN
            ILISTV(J2)=I2
            GOTO420
          ELSEIF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1      IUSE(I).EQ.'P')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,452)
  452       FORMAT('      A VARIABLE WITH ELEMENTS TO BE DELETED WAS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,454)
  454       FORMAT('      FOUND IN THE INTERNAL NAME LIST, BUT AS A ',
     1             'PARAMETER,')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,456)
  456       FORMAT('      AND NOT AS A VARIABLE AS IT SHOULD BE HERE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,457)IHVARJ,IHVRJ2
  457       FORMAT('      THE VARIABLE NAME WAS ',2A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,306)
            CALL DPWRST('XXX','BUG ')
            IF(IWIDTH.GE.1)THEN
              WRITE(ICOUT,307)(IANS(II),II=1,MIN(100,IWIDTH))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
            GOTO8900
          ELSEIF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1      IUSE(I).EQ.'F')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,452)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,464)
  464       FORMAT('      FOUND IN THE INTERNAL NAME LIST, BUT AS A ',
     1             'FUNCTION/STRING,')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,456)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,457)IHVARJ,IHVRJ2
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,306)
            CALL DPWRST('XXX','BUG ')
            IF(IWIDTH.GE.1)THEN
              WRITE(ICOUT,307)(IANS(II),II=1,MIN(100,IWIDTH))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
            GOTO8900
          ENDIF
  430   CONTINUE
        ILISTV(J2)=(-5)
C
  420 CONTINUE
C
C               *****************************************
C               **  STEP 5--                           **
C               **  TREAT THE FULL CASE.               **
C               **  CARRY OUT THE DELETING,            **
C               **  AND THE SUBSEQUENT PACKING,        **
C               **  DO THE LIST UPDATING, AND          **
C               **  PRODUCE SOME INFORMATIVE PRINTING. **
C               *****************************************
C
      ISTEPN='5'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO500J=1,NUMDEL
        IHVARJ=IVN(J)
        IHVRJ2=IVN2(J)
        IROWD=IRN(J)
        ILIST2=ILISTV(J)
CCCCC   THE FOLLOWING LINE WAS INSERTED           DECEMBER 1994
        IF(ILIST2.LE.0)GOTO500
        NIVARJ=IN(ILIST2)
        ICOLVJ=IVALUE(ILIST2)
CCCCC   OCTOBER 1993.  ADD FOLLOWING LINE
        INCLVJ=IVALU2(ILIST2)
        IMAX=NIVARJ
        IF(1.LE.IROWD.AND.IROWD.LE.IMAX)GOTO539
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,532)IROWD
  532   FORMAT('      THE SPECIFIED ROW ELEMENT (= ROW ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,533)IHVARJ,IHVRJ2
  533   FORMAT('      TO BE DELETED FROM VARIABLE ',2A4,' WAS SMALLER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,535)IMAX
  535   FORMAT('      THAN 1 OR LARGER THAN THE CURRENT (= ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,536)
  536   FORMAT('      NUMBER OF ELEMENTS IN THIS VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO8900
C
  539   CONTINUE
C
        NS2=0
        ND2=0
        DO550I=1,IMAX
          IF(I.EQ.IROWD)THEN
            ND2=ND2+1
            IJ=MAXN*(ICOLVJ-1)+I
            IF(ICOLVJ.LE.MAXCOL)TEMPD=V(IJ)
            IF(ICOLVJ.EQ.MAXCP1)TEMPD=PRED(I)
            IF(ICOLVJ.EQ.MAXCP2)TEMPD=RES(I)
            IF(ICOLVJ.EQ.MAXCP3)TEMPD=YPLOT(I)
            IF(ICOLVJ.EQ.MAXCP4)TEMPD=XPLOT(I)
            IF(ICOLVJ.EQ.MAXCP5)TEMPD=X2PLOT(I)
            IF(ICOLVJ.EQ.MAXCP6)TEMPD=TAGPLO(I)
            IF(ND2.EQ.1)IROD1O=I
            IRODNO=I
            IF(ND2.EQ.1)VALD1O=TEMPD
            VALDNO=TEMPD
          ELSE
C
            NS2=NS2+1
            IJ=MAXN*(ICOLVJ-1)+I
            IF(ICOLVJ.LE.MAXCOL)TEMP(NS2)=V(IJ)
            IF(ICOLVJ.EQ.MAXCP1)TEMP(NS2)=PRED(I)
            IF(ICOLVJ.EQ.MAXCP2)TEMP(NS2)=RES(I)
            IF(ICOLVJ.EQ.MAXCP3)TEMP(NS2)=YPLOT(I)
            IF(ICOLVJ.EQ.MAXCP4)TEMP(NS2)=XPLOT(I)
            IF(ICOLVJ.EQ.MAXCP5)TEMP(NS2)=X2PLOT(I)
            IF(ICOLVJ.EQ.MAXCP6)TEMP(NS2)=TAGPLO(I)
            IF(NS2.EQ.1)IROW1O=I
            IROWNO=I
            IF(NS2.EQ.1)VAL1O=TEMP(NS2)
            VALNO=TEMP(NS2)
          ENDIF
C
  550   CONTINUE
        NIOLD=NIVARJ
        NINEW=NS2
        IROW1N=1
        IROWNN=NS2
C
        IF(NS2.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,582)
  582     FORMAT('      FOR THE FULL (UNQUALIFIED) CASE, SINCE THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,583)
  583     FORMAT('      RESULTING NS2 = 0, THE NUMBER OF ELEMENTS ',
     1          'DELETED = 0.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,585)IHVARJ,IHVRJ2,IMAX,IROWD
  585     FORMAT('      IHVARJ, IHVRJ2, IMAX, IROWD = ',2A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,306)
  590     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO8900
        ENDIF
C
        DO600I=1,NS2
          IJ=MAXN*(ICOLVJ-1)+I
          IF(ICOLVJ.LE.MAXCOL)V(IJ)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP1)PRED(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP2)RES(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=TEMP(I)
  600   CONTINUE
C
        DO602J4=1,NUMNAM
          IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLVJ)GOTO605
          GOTO602
  605   CONTINUE
        IUSE(J4)='V'
        IVALUE(J4)=ICOLVJ
CCCCC   OCTOBER 1993.  ADD FOLLOWING LINE
        IVALU2(J4)=INCLVJ
        VALUE(J4)=ICOLVJ
        IN(J4)=NINEW
        IVSTAR(J4)=MAXN*(ICOLVJ-1)+1
        IVSTOP(J4)=MAXN*(ICOLVJ-1)+NINEW
  602   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,611)IHVARJ,IHVRJ2,NIOLD
  611     FORMAT('VARIABLE ',2A4,'--OLD NUMBER OF ELEMENTS = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,612)NINEW
  612     FORMAT('                   NEW NUMBER OF ELEMENTS = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,613)VALD1O
  613     FORMAT('                   FIRST VALUE DELETED    = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,614)IROD1O
  614     FORMAT('                         (DELETED FROM ROW ',I8,')')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,615)VALDNO
  615     FORMAT('                   LAST  VALUE DELETED    = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,616)IRODNO
  616     FORMAT('                         (DELETED FROM ROW ',I8,')')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,617)VAL1O
  617     FORMAT('                   FIRST VALUE RETAINED   = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,618)IROW1O,IROW1N
  618     FORMAT('                         (MOVED FROM ROW ',I8,
     1           ' TO ROW ',I8,')')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,619)VALNO
  619     FORMAT('                   LAST  VALUE RETAINED   = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,620)IROWNO,IROWNN
  620     FORMAT('                         (MOVED FROM ROW ',I8,
     1           ' TO ROW ',I8,')')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  500 CONTINUE
C
      GOTO8900
C
C               ********************************************************
C               **  STEP 7--                                          **
C               **  FOR THE SUBSET AND FOR CASES                      **
C               **  (AND WHEN DELETING ENTIRE VARIABLES),             **
C               **  CHECK TO MAKE SURE ALL VARIABLES WITH DELETIONS   **
C               **  ARE, IN FACT, IN THE INTERNAL LIST.               **
C               ********************************************************
C
 7000 CONTINUE
C
      ISTEPN='7'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUME.LE.0.OR.NUME.GT.MAXE2)THEN
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,301)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,7102)
 7102    FORMAT('      THE DELETE COMMAND REQUIRES THAT THE NUMBER OF')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,7103)
 7103    FORMAT('      VARIABLES WITH ELEMENTS TO BE DELETED BE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,7104)MAXE2
 7104    FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVE);')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,7105)NUME
 7105    FORMAT('      THE SPECIFIED NUMBER WAS ',I8)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,306)
         CALL DPWRST('XXX','BUG ')
         IF(IWIDTH.GE.1)THEN
           WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
           CALL DPWRST('XXX','BUG ')
         ENDIF
         IERROR='YES'
         GOTO8900
      ENDIF
C
      J2=0
      DO7200J=1,NUME
         IHVARJ=JENAM1(J)
         IHVRJ2=JENAM2(J)
         DO7300I=1,NUMNAM
           I2=I
           IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1        IUSE(I).EQ.'V')THEN
             J2=J2+1
             ILISTV(J2)=I2
             GOTO7200
           ELSEIF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1        IUSE(I).EQ.'P')THEN
CCCCC        OCTOBER 1993.  TO DELETE A PARAMETER, USE THE FACT THAT DPUPDV
CCCCC        REMOVES ANY NAME WHERE IN(.)=0.  SET IN(.) TO ZERO.
CCCCC        JUNE 1994.  SET TO -1 (SOME INTERNALLY SET PARAMETERS DO
CCCCC        NOT SET IN(.), SO BE MORE EXPLICIT.
             IN(I2)=-1
             GOTO7200
           ELSEIF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1        IUSE(I).EQ.'F')THEN
CCCCC        JANUARY 2012.  TO DELETE A FUNCTION/STRING, USE THE FACT
CCCCC                       THAT DPUPDV REMOVES ANY NAME WHERE
CCCCC                       IN(.) = -1.  ALSO CALL DPINFU TO DELETE
CCCCC                       THE TEXT FROM STRING SPACE FOR ALL STRINGS.
             IN(I2)=-1
             N3=-99
             NEWNAM='NO'
             ILISTL=I2
             CALL DPINFU(IFUNC3,N3,IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1                   NUMNAM,IANS,IWIDTH,IHVARJ,IHVRJ2,ILISTL,
     1                   NEWNAM,MAXNAM,
     1                   IFUNC,NUMCHF,MAXCHF,IBUGS2,IERROR)
             GOTO7200
           ELSEIF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1        IUSE(I).EQ.'M')THEN
CCCCC        OCTOBER 1993.  HANDLE MATRIX CASE.  NEED TO DELETE EACH OF
CCCCC                       THE VARIABLES AS WELL.
             IN(I2)=-1
             NCOL=IVALU2(I2) - IVALUE(I2) + 1
             ISTART=I2+1
             ISTOP=ISTART+NCOL-1
             DO7455II=ISTART,ISTOP
               IN(II)=-1
 7455        CONTINUE
             GOTO7200
           ENDIF
 7300    CONTINUE
C
 7200 CONTINUE
      NDONE=J2
C
C               *****************************************
C               **  STEP 8--                           **
C               **  TREAT THE SUBSET AND FOR CASES     **
C               **  AND CERTAIN FULL CASES.            **
C               **  CARRY OUT THE DELETING,            **
C               **  AND THE SUBSEQUENT PACKING,        **
C               **  DO THE LIST UPDATING, AND          **
C               **  PRODUCE SOME INFORMATIVE PRINTING. **
C               *****************************************
C
      ISTEPN='8'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO8100
      ILQP1=ILOCQ+1
      IF(ILQP1.LE.NUMARG)GOTO8100
      IF(ICASEQ.EQ.'FOR')GOTO8030
      GOTO8010
C
 8010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8012)
 8012 FORMAT('      THE WORD    SUBSET    WAS THE FINAL WORD ON THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8014)
 8014 FORMAT('      COMMAND LINE.  THE WORD    SUBSET   SHOULD HAVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8015)
 8015 FORMAT('      BEEN FOLLOWED BY EITHER 2 OR 3 ARGUMENTS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8016)
 8016 FORMAT('      THE FIRST ARGUMENT SPECIFIES THE SUBSET VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8017)
 8017 FORMAT('      THE SECOND AND (IF EXISTENT) THIRD ARGUMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8018)
 8018 FORMAT('      SPECIFY THE VALUE OR INTERVAL (OF THE SUBSET')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8019)
 8019 FORMAT('      VARIABLE) WHICH DEFINES THE SUBSET OF INTEREST.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,306)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO8900
C
 8030 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8032)
 8032 FORMAT('      THE WORD  FOR  WAS THE FINAL WORD ON THE COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8034)
 8034 FORMAT('      LINE.  THE WORD  FOR  SHOULD HAVE BEEN FOLLOWED BY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8035)
 8035 FORMAT('      EXACTLY 3 OR BY EXACTLY 5 WORDS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8036)
 8036 FORMAT('      1) A DUMMY VARIABLE NAME;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8037)
 8037 FORMAT('      2) AN EQUAL SIGN;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8038)
 8038 FORMAT('      3) ONE LIMIT (LOWER OR UPPER) FOR THE DUMMY ',
     1       'VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8039)
 8039 FORMAT('      4) THE INCREMENT FOR THE DUMMY VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9040)
 9040 FORMAT('      5) THE OTHER LIMIT (UPPER OR LOWER) FOR THE ',
     1       'DUMMY VARIABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,306)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO8900
C
 8100 CONTINUE
      IF(ICASEQ.EQ.'FULL')THEN
        DO8135I=1,MAXN
          ISUB(I)=1
 8135   CONTINUE
        NQ=MAXN
        GOTO8200
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        NIOLD=MAXN
        CALL DPFOR(NIOLD,NINEW,IROW1,IROWN,
     1             NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        NQ=NINEW
        GOTO8200
      ENDIF
C
      IHSET=IHARG(ILQP1)
      IHSET2=IHARG2(ILQP1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      NISET=IN(ILOC)
      CALL DPSUBS(NISET,ILOCS,NS,IBUGQ,IERROR)
      NQ=NISET
C
 8200 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO8300J=1,NUME
        IHVARJ=JENAM1(J)
        IHVRJ2=JENAM2(J)
        ILIST2=ILISTV(J)
        IF(ILIST2.LE.0)GOTO8300
        NIVARJ=IN(ILIST2)
CCCCC   OCTOBER 1993.  SKIP FOR PARAMETER
        IF(NIVARJ.LE.0)GOTO8300
        ICOLVJ=IVALUE(ILIST2)
CCCCC   OCTOBER 1993.  ADD FOLLOWING LINE
        INCLVJ=IVALU2(ILIST2)
        NS2=0
        ND2=0
        IMAX=NQ
        IF(NIVARJ.LT.NQ)IMAX=NIVARJ
        DO8400I=1,IMAX
          IF(ISUB(I).EQ.0)THEN
            NS2=NS2+1
            IJ=MAXN*(ICOLVJ-1)+I
            IF(ICOLVJ.LE.MAXCOL)TEMP(NS2)=V(IJ)
            IF(ICOLVJ.EQ.MAXCP1)TEMP(NS2)=PRED(I)
            IF(ICOLVJ.EQ.MAXCP2)TEMP(NS2)=RES(I)
            IF(ICOLVJ.EQ.MAXCP3)TEMP(NS2)=YPLOT(I)
            IF(ICOLVJ.EQ.MAXCP4)TEMP(NS2)=XPLOT(I)
            IF(ICOLVJ.EQ.MAXCP5)TEMP(NS2)=X2PLOT(I)
            IF(ICOLVJ.EQ.MAXCP6)TEMP(NS2)=TAGPLO(I)
            IF(NS2.EQ.1)IROW1O=I
            IROWNO=I
            IF(NS2.EQ.1)VAL1O=TEMP(NS2)
            VALNO=TEMP(NS2)
          ELSE
            ND2=ND2+1
            IJ=MAXN*(ICOLVJ-1)+I
            IF(ICOLVJ.LE.MAXCOL)TEMPD=V(IJ)
            IF(ICOLVJ.EQ.MAXCP1)TEMPD=PRED(I)
            IF(ICOLVJ.EQ.MAXCP2)TEMPD=RES(I)
            IF(ICOLVJ.EQ.MAXCP3)TEMPD=YPLOT(I)
            IF(ICOLVJ.EQ.MAXCP4)TEMPD=XPLOT(I)
            IF(ICOLVJ.EQ.MAXCP5)TEMPD=X2PLOT(I)
            IF(ICOLVJ.EQ.MAXCP6)TEMPD=TAGPLO(I)
            IF(ND2.EQ.1)IROD1O=I
            IRODNO=I
            IF(ND2.EQ.1)VALD1O=TEMPD
            VALDNO=TEMPD
          ENDIF
 8400   CONTINUE
        NIOLD=NIVARJ
        NINEW=NS2
        IROW1N=1
        IROWNN=NS2
C
        IF(ND2.LT.1)THEN
          IERROR='YES'
          GOTO8900
        ENDIF
C
        DO8500I=1,NS2
          IJ=MAXN*(ICOLVJ-1)+I
          IF(ICOLVJ.LE.MAXCOL)V(IJ)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP1)PRED(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP2)RES(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=TEMP(I)
          IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=TEMP(I)
 8500   CONTINUE
C
CCCCC   OCTOBER 1997.  REINIITIALIZE DELETED VALUES TO ZERO
CCCCC   INSTEAD OF CPUMIN.
        NS2P1=NS2+1
        IF(NS2P1.GT.IMAX)GOTO8569
        DO8560I=NS2P1,IMAX
          IJ=MAXN*(ICOLVJ-1)+I
CCCCC     IF(ICOLVJ.LE.MAXCOL)V(IJ)=CPUMIN
CCCCC     IF(ICOLVJ.EQ.MAXCP1)PRED(I)=CPUMIN
CCCCC     IF(ICOLVJ.EQ.MAXCP2)RES(I)=CPUMIN
CCCCC     IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=CPUMIN
CCCCC     IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=CPUMIN
CCCCC     IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=CPUMIN
CCCCC     IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=CPUMIN
          IF(ICOLVJ.LE.MAXCOL)V(IJ)=0.0
          IF(ICOLVJ.EQ.MAXCP1)PRED(I)=0.0
          IF(ICOLVJ.EQ.MAXCP2)RES(I)=0.0
          IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=0.0
          IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=0.0
          IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=0.0
          IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=0.0
 8560   CONTINUE
 8569   CONTINUE
C
        DO8600J4=1,NUMNAM
          IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLVJ)THEN
            IUSE(J4)='V'
            IVALUE(J4)=ICOLVJ
CCCCC       OCTOBER 1993.  ADD FOLLOWING LINE
            IVALU2(J4)=INCLVJ
            VALUE(J4)=ICOLVJ
            IN(J4)=NINEW
            IVSTAR(J4)=MAXN*(ICOLVJ-1)+1
            IVSTOP(J4)=MAXN*(ICOLVJ-1)+NINEW
          ENDIF
 8600   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8611)IHVARJ,IHVRJ2,NIOLD
 8611     FORMAT('VARIABLE ',2A4,'--OLD NUMBER OF ELEMENTS = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8612)NINEW
 8612     FORMAT('                   NEW NUMBER OF ELEMENTS = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8613)VALD1O
 8613     FORMAT('                   FIRST VALUE DELETED    = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8614)IROD1O
 8614     FORMAT('                         (DELETED FROM ROW ',I8,')')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8615)VALDNO
 8615     FORMAT('                   LAST  VALUE DELETED    = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8616)IRODNO
 8616     FORMAT('                         (DELETED FROM ROW ',I8,')')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8617)VAL1O
 8617     FORMAT('                   FIRST VALUE RETAINED   = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8618)IROW1O,IROW1N
 8618     FORMAT('                         (MOVED FROM ROW ',I8,
     1           ' TO ROW ',I8,'  )')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8619)VALNO
 8619     FORMAT('                   LAST  VALUE RETAINED   = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8620)IROWNO,IROWNN
 8620     FORMAT('                         (MOVED FROM ROW ',I8,
     1           ' TO ROW ',I8,')')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 8300 CONTINUE
C
      GOTO8900
C
C               **********************************
C               **  STEP 9--                    **
C               **  UPDATE INTERNAL DATA ARRAY  **
C               **  (IF NECESSARY)              **
C               **********************************
C
 8900 CONTINUE
C
      ISTEPN='9'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'DELE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OCTOBER 1993.  ADD ARGUMENT TO LIST
CCCCC CALL DPUPDV(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
      CALL DPUPDV(IHNAME,IHNAM2,IUSE,IVALUE,IVALU2,VALUE,IN,
     1            IVARLB,
     1            IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,
     1            IBUGS2,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'DELE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDELE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGS2,IERROR
 9012   FORMAT('IBUGS2,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)MAXNAM,NUMNAM,MAXN,MAXCOL,NUMCOL
 9014   FORMAT('MAXNAM,NUMNAM,MAXN,MAXCOL,NUMCOL = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,NUMNAM
          WRITE(ICOUT,9021)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVALUE(I),VALUE(I)
 9021     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1           I8,2X,A4,A4,2X,A4,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9022)I,IHNAME(I),IHNAM2(I),IN(I),
     1                     IVSTAR(I),IVSTOP(I)
 9022     FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',
     1           I8,2X,A4,A4,6X,I8,I8,I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO9030J=1,NUMCOL
          IJ=MAXN*(J-1)+1
          WRITE(ICOUT,9031)J,MAXN,IJ,V(IJ)
 9031     FORMAT('J,MAXN,IJ,V(IJ) = ',I8,I8,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9030   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDELI(Y,X,X3D,PX,NP,NUMSET,ICASPL,ICAS3D,XDELMN,
     1                  DTAG,XIDTEM)
C
C     PURPOSE--DETERMINE DATA LIMITS, FRAME LIMITS,
C              AND TIC COORDINATES FOR A PLOT.
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
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED--MAY        1990. ADD VARIABLES TO DPDETM CALL LIST
C     UPDATED--DECEMBER   2006. SUPPORT FOR TRILINEAR SCALES
C     UPDATED--JANUARY    2018. SUPPORT FOR SCREEN UNITS FOR LINE AND
C                               CHARACTER TRACES.  FOR THESE CASES, DO
C                               NOT THIS TRACE TO DETERMINE DATA LIMITS.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAS3D
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION X3D(*)
      DIMENSION PX(*)
      DIMENSION DTAG(*)
      DIMENSION XIDTEM(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOPC.INC'
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DELI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDELI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IMANUF,IMODEL,ICASPL,ICAS3D
   52   FORMAT('IMANUF,IMODEL,ICASPL,ICAS3D = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NP,NUMSET,XDELMN
   54   FORMAT('NP,NUMSET,XDELMN = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IBUGG4,ISUBG4,IERRG4,ITICUN
   59   FORMAT('IBUGG4,ISUBG4,IERRG4,ITICUN = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)PX1TOL,PX2TOL,PY1TOB,PY2TOB
   71   FORMAT('PX1TOL,PX2TOL,PY1TOB,PY2TOB = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)PX1TOR,PX2TOR,PY1TOT,PY2TOT
   72   FORMAT('PX1TOR,PX2TOR,PY1TOT,PY2TOT = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO80I=1,NP
          WRITE(ICOUT,82)I,DTAG(I),XIDTEM(I)
   82     FORMAT('I,DTAG(I),XIDTEM(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   80   CONTINUE
      ENDIF
C
C               ************************************
C               **  STEP 1--                      **
C               **  DETERMINE ACTUAL DATA LIMITS  **
C               **  (IN UNITS OF THE DATA)        **
C               ************************************
C
      CALL DPDEDL(Y,X,PX,NP,NUMSET,
     1            ICASPL,ICAS3D,
     1            ISPISW,ASPIBA,MAXSPI,
     1            IBARSW,ABARBA,ABARWI,MAXBAR,XDELMN,
     1            GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1            GX2MIN,GX2MAX,GY2MIN,GY2MAX,
     1            IX1MIN,IX1MAX,IY1MIN,IY1MAX,
     1            IX2MIN,IX2MAX,IY2MIN,IY2MAX,
     1            DX1MIN,DX1MAX,DY1MIN,DY1MAX,
     1            DX2MIN,DX2MAX,DY2MIN,DY2MAX,
     1            IHORSW,ICHATY,ILINTY,DTAG,XIDTEM,
     1            PXMIN,PXMAX,PYMIN,PYMAX,
     1            ITICUN,PX1TOL,PX1TOR,PY1TOB,PY1TOT)
C
C               *************************************
C               **  STEP 2--                       **
C               **  DETERMINE ACTUAL FRAME LIMITS  **
C               **  (IN UNITS OF THE DATA)         **
C               *************************************
C
      CALL DPDEFL(ICASPL,ICAS3D,
     1            DX1MIN,DX1MAX,DY1MIN,DY1MAX,
     1            DX2MIN,DX2MAX,DY2MIN,DY2MAX,
     1            GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1            GX2MIN,GX2MAX,GY2MIN,GY2MAX,
     1            IX1MIN,IX1MAX,IY1MIN,IY1MAX,
     1            IX2MIN,IX2MAX,IY2MIN,IY2MAX,
     1            IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1            FX1MIN,FX1MAX,FY1MIN,FY1MAX,
     1            FX2MIN,FX2MAX,FY2MIN,FY2MAX,
     1            NMJX1T,NMJX2T,NMJY1T,NMJY2T)
C
CCCCC NOVEMBER 1997.  SAVE FRAME LIMITS BEFORE AFFECTED BY
CCCCC TIC OFFSET VALUES.
C
      FX1MNZ=FX1MIN
      FX1MXZ=FX1MAX
      FX2MNZ=FX2MIN
      FX2MXZ=FX2MAX
      FY1MNZ=FY1MIN
      FY1MXZ=FY1MAX
      FY2MNZ=FY2MIN
      FY2MXZ=FY2MAX
C
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               *********************************************
C               **  STEP 3--                               **
C               **  DETERMINE ACTUAL TIC MARK COORDINATES  **
C               **  (IN BOTH STANDARDIZED 0 TO 100 UNITS,  **
C               **  AND IN DATA UNITS)                     **
C               *********************************************
C
CCCCC ADDED TIC OFFSET VARIABLES TO CALL LIST MAY, 1990.
C
      CALL DPDETM(PXMIN,PYMIN,PXMAX,PYMAX,
     1            ICASPL,ICAS3D,
     1            FX1MIN,FX1MAX,FY1MIN,FY1MAX,
     1            FX2MIN,FX2MAX,FY2MIN,FY2MAX,
     1            IX1TSW,IX2TSW,IY1TSW,IY2TSW,
     1            IX1JSW,IX2JSW,IY1JSW,IY2JSW,
     1            NMJX1T,NMJX2T,NMJY1T,NMJY2T,
     1            IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1            PX1COO,PX2COO,PY1COO,PY2COO,
     1            X1COOR,X2COOR,Y1COOR,Y2COOR,
     1            NX1COO,NX2COO,NY1COO,NY2COO,
     1            IX1NSW,IX2NSW,IY1NSW,IY2NSW,
     1            NMNX1T,NMNX2T,NMNY1T,NMNY2T,
     1            PX1CMN,PX2CMN,PY1CMN,PY2CMN,
     1            X1COMN,X2COMN,Y1COMN,Y2COMN,
     1            NX1CMN,NX2CMN,NY1CMN,NY2CMN,
     1            PX1TOL,PX2TOL,PY1TOB,PY2TOB,
     1            PX1TOR,PX2TOR,PY1TOT,PY2TOT,
     1            ITICUN)
      IF(IERRG4.EQ.'YES')GOTO9000
C
CCCCC FOR TRILINEAR SCALES, SET TO 0 AND SUM (WHICH SHOULD
CCCCC BE EITHER 1 OR 100).
C
      ASUM=Y(1) + X(1) + X3D(1)
      IF(ICASPL.EQ.'TRPL')THEN
        FX1MIN=0.0
        FX1MAX=ASUM
        FX2MIN=0.0
        FX2MAX=ASUM
        FY1MIN=0.0
        FY1MAX=ASUM
        FY2MIN=0.0
        FY2MAX=ASUM
        FX1MNZ=FX1MIN
        FX1MXZ=FX1MAX
        FX2MNZ=FX2MIN
        FX2MXZ=FX2MAX
        FY1MNZ=FY1MIN
        FY1MXZ=FY1MAX
        FY2MNZ=FY2MIN
        FY2MXZ=FY2MAX
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'DELI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDELI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NP,NUMSET,XDELMN
 9014   FORMAT('NP,NUMSET,XDELMN = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEMN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1                  IPL1NU,IPL1NA,
     1                  IPL2NU,IPL2NA,
     1                  IPL1CS,IPL2CS,
     1                  IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1                  IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1                  NUMDEV,MAXDEV,
     1                  IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,
     1                  IDUNIT,IDNVOF,IDNHOF,
     1                  ICAPSW,ICAPNU,
     1                  IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
C
C  JUNE 1992.  IPL1CS, IPL2CS ADDED TO ARGUMENT LIST
C
C     PURPOSE--DEFINE THE MANUFACTURER AND MODEL FOR AN OUTPUT DEVICE.
C              THE MANUFACTURER AND MODEL WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE VECTORS
C              IDMANU, IDMODE, IDMOD2, AND IDMOD3.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFMA
C                     --IDEFMO
C                     --IDEFM2
C                     --IDEFM3
C                     --MAXDEV
C     OUTPUT ARGUMENTS--
C                     --IDMANU
C                     --IDMODE
C                     --IDMOD2
C                     --IDMOD3
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
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --FEBRUARY  1989.  POSTSCRIPT, QUIC, PCL, ETC.  (ALAN)
C     UPDATED         --MAY       1989.  POSTSCRIPT TRANSLATION FIX (ALAN)
C     UPDATED         --MARCH     1990.  X11 FIX
C     UPDATED         --MAY       1990.  HP-GL MODEL NUMBERS
C     UPDATED         --MAY       1990.  DISTINCTION BETWEEN OFF AND CLOSE
C     UPDATED         --APRIL     1992.  CALL GREXIT IF CLOSE (ALAN)
C     UPDATED         --MAY       1992.  SKIP MESSAGE FOR DEVICE 3
C     UPDATED         --JUNE      1992.  DON'T CALL GRINDE FOR ON
C     UPDATED         --JUNE      1992.  IF DEVICE, CHECK IF STATUS IS OPEN
C     UPDATED         --AUGUST    1992.  FIX FOR HP-GL (LASER JET III)
C     UPDATED         --MARCH     1995.  SYNONYMS FOR POSTSCRIPT
C     UPDATED         --OCTOBER   1996.  QWIN PATCH
C     UPDATED         --FEBRUARY  2001.  GD AND GDI DEVICES SHOULD NOT OPEN OUTPUT
C                                        FILE (DONE BY UNDERLYING C ROUTINES)
C                                        PASS IGDFLG TO DPDEP2
C     UPDATED         --SEPTEMBER 2002.  ICAPSW
C     UPDATED         --AUGUST    2016.  IF WRITE ERROR DETECTED IN
C                                        CALL TO GRINDE, TURN POWER
C                                        SWITCH OFF
C     UPDATED         --APRIL     2018.  IF "ON" GIVEN, ONLY TURN
C                                        POWER SWITCH ON IF THE DEVICE
C                                        HAS BEEN PREVIOUSLY DEFINED
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
      CHARACTER*80 IPL1NA
      CHARACTER*80 IPL2NA
      CHARACTER*12 IPL1CS
      CHARACTER*12 IPL2CS
      CHARACTER*4 ICAPSW
      CHARACTER*4 IDEFMA
      CHARACTER*4 IDEFMO
      CHARACTER*4 IDEFM2
      CHARACTER*4 IDEFM3
      CHARACTER*4 IDEFPO
      CHARACTER*4 IDEFCN
      CHARACTER*4 IDEFDC
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDCOLO
      CHARACTER*4 IDFONT
      CHARACTER*4 IBUGO2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IANS
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOPERA
      CHARACTER*4 IGENID
      CHARACTER*4 IGDFLG
      CHARACTER*4 ICAFLG
      CHARACTER*4 IFOUN2
      CHARACTER*4 ISAVE
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
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
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
      DIMENSION IDNVOF(*)
      DIMENSION IDNHOF(*)
C
      DIMENSION IANS(*)
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
C
      COMMON/GRAERR/IGFLAG
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
      IBUGG4='OFF'
      ISUBG4='-999'
      ISAVE='-999'
C
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEMN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPDEMN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGO2,ISUBRO,IFOUND,IERROR,NUMARG
   53   FORMAT('IBUGO2,ISUBRO,IFOUND,IERROR,NUMARG = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
          WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I)
   71     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I) = ',
     1           I8,3(2X,A4),2X,I8)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
        WRITE(ICOUT,75)IPL1CS,IPL2CS
   75   FORMAT('IPL1CS,IPL2CS=',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC FEBRUARY, 1989.  CHECK IF MODEL ="ON" OR "OFF".  IF SO, DO NOT
CCCCC TREAT AS AS A DEVICE.  WILL BE HANDLED SEPARATELY IN MAINOD.
C
      IF(NUMARG.LE.0)GOTO1199
      IF((NUMARG.GE.1.AND.IHARG(1).EQ.'MANU') .OR.
     1   (NUMARG.GE.1.AND.IHARG(1).EQ.'MODE'))THEN
C
C       ****************************************************************
C       **  FEBRAURY, 1989: HANDLE "ON" AND "OFF" SEPARETELY.  THESE  **
C       **  WILL TURN THE DEVICE POWER "ON" AND "OFF", BUT WILL NOT   **
C       **  RESET THE DEVICE TYPE OR OTHER SETTINGS.  DO THIS WAY SO  **
C       **  CAN TOGGLE A DEVICE "ON" AND "OFF".                       **
C       ****************************************************************
C
C       MAY, 1990.  DISTINGUISH BETWEEN OFF AND CLOSE.  BOTH WILL TURN THE
C                   POWER SWITCH OFF, BUT CLOSE WILL ALSO CLOSE THE FILE.
C
        IF(IHARG(NUMARG).EQ.'ON' .OR. IHARG(NUMARG).EQ.'OPEN')THEN
          DO1114I=1,NUMDEV
            IF(IDMANU(I).NE.'    ')THEN
              IDPOWE(I)='ON'
            ENDIF
 1114     CONTINUE
        ELSEIF(IHARG(NUMARG).EQ.'OFF')THEN
          DO1117I=1,NUMDEV
            IDPOWE(I)='OFF'
 1117     CONTINUE
        ELSEIF(IHARG(NUMARG).EQ.'CLOS')THEN
          DO1119I=1,NUMDEV
            IDPOWE(I)='OFF'
 1119     CONTINUE
        ELSEIF(IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1         NUMARG.LE.1)THEN
          DO1121I=1,NUMDEV
            IDMANU(I)=IDEFMA
            IDMODE(I)=IDEFMO
            IDMOD2(I)=IDEFM2
            IDMOD3(I)=IDEFM3
            IDPOWE(I)=IDEFPO
            IDCONT(I)=IDEFCN
            IDCOLO(I)=IDEFDC
            IDNVPP(I)=IDEFVP
            IDNHPP(I)=IDEFHP
            IDUNIT(I)=IDEFUN
 1121     CONTINUE
        ELSE
C
C         FEBRUARY,1989.  "QMS" WILL BE SET TO "QUIC".  "LASER JET" TO PCL.
C         JUNE 1992.      FOLLOWING FOR DEBUGGING PURPOSES
C
          IF(IBUGO2.EQ.'ON' .OR. ISUBRO.EQ.'DEMN')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1123)
 1123       FORMAT('IN LOOP 1125')
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          DO1127I=1,NUMDEV
            K=2
            IF(K.LE.NUMARG)IDMANU(I)=IHARG(K)
            IF(K.GT.NUMARG)IDMANU(I)='    '
            ISAVE=IDMANU(I)
            IF(ISAVE.EQ.'HPGL')THEN
              IDMANU(I)='HP  '
            ELSEIF(ISAVE.EQ.'QMS')THEN
              IDMANU(I)='QUIC'
            ELSEIF(ISAVE.EQ.'TELA')THEN
              IDMANU(I)='QUIC'
            ELSEIF(ISAVE.EQ.'LASE')THEN
              IDMANU(I)='PCL '
            ELSEIF(ISAVE.EQ.'PS  ')THEN
              IDMANU(I)='POST'
            ENDIF
            K=K+1
            IDMODE(I)='    '
            IF(K.LE.NUMARG)IDMODE(I)=IHARG(K)
C
C           FOLLOWING LINE MOVED TO BELOW
CCCCC       IF(ISAVE.EQ.'HPGL')IDMODE(I)='GL  '
C
            IF(ISAVE.EQ.'LASE'.AND.IDMODE(I).EQ.'JET')IDMODE(I)='    '
            K=K+1
            IDMOD2(I)='    '
            IF(K.LE.NUMARG)IDMOD2(I)=IHARG(K)
            K=K+1
            IDMOD3(I)='    '
            IF(K.LE.NUMARG)IDMOD3(I)=IHARG(K)
C
C           FOLLOWING BLOCK ADDED TO CHECK FOR HPGL MODEL NUMBERS
C
            IF(ISAVE.EQ.'HPGL')THEN
              IDMOD3(I)=IDMOD2(I)
              IDMOD2(I)=IDMODE(I)
              IDMODE(I)='GL  '
C
CCCCC       MARCH 1995.  ADD FOLLOWING 4 LINES
C
            ELSEIF(ISAVE.EQ.'EPS ' .OR. ISAVE.EQ.'ENCA')THEN
              IDMANU(I)='POST'
              IDMODE(I)='ENCA'
            ENDIF
C
            CALL GRSEPP(I,IPL1NU,IPL2NU,
     1                  IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,
     1                  IDUNIT,IDNVOF,IDNHOF,
     1                  IBUGO2,IFOUN2,IERROR)
 1127     CONTINUE
        ENDIF
C
      ELSE
        IF(IARGT(1).NE.'NUMB')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
 1141     FORMAT('***** ERROR IN DPDEMN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
 1142     FORMAT('      IN THE DEVICE ... MANUFACTURER COMMAND,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
 1143     FORMAT('      THE DEVICE IS IDENTIFIED BY A NUMBER, AS IN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1144)
 1144     FORMAT('      DEVICE 2 MANUFACTURER FR-80')
          CALL DPWRST('XXX','BUG ')
          GOTO1199
        ELSE
          I=IARG(1)
          IF(1.LE.I.AND.I.LE.MAXDEV)THEN
            GOTO1160
          ELSE
            IERROR='YES'
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1141)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1152)
 1152       FORMAT('      IN THE DEVICE ... MANUFACTURER COMMAND,')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1153)
 1153       FORMAT('      THE NUMBER OF DEVICES MUST BE ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1154)MAXDEV
 1154       FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1155)
 1155       FORMAT('      SUCH WAS NOT THE CASE HERE--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1156)I
 1156       FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',
     1             'DEVICE.')
            CALL DPWRST('XXX','BUG ')
            GOTO1199
          ENDIF
        ENDIF
      ENDIF
C
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1136)
 1136   FORMAT('THE MANUFACTURER FOR ALL DEVICES HAS JUST BEEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1137)IDMANU(1),IDMODE(1),IDMOD2(1),IDMOD3(1)
 1137   FORMAT('SET TO ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1199
C
C
C
C     ***************************************************************
C     **  FEBRUARY, 1989: HANDLE "ON" AND "OFF" SEPARETELY.  THESE **
C     **  WILL TURN THE DEVICE POWER "ON" AND "OFF", BUT WILL NOT  **
C     **  RESET THE DEVICE TYPE OR OTHER SETTINGS.  DO THIS WAY SO **
C     **  CAN TOGGLE A DEVICE "ON" AND "OFF".                      **
C     ***************************************************************
C
C     MAY, 1990.  DISTINGUISH BETWEEN OFF AND CLOSE.  BOTH WILL TURN THE
C                 POWER SWITCH OFF, BUT CLOSE WILL ALSO CLOSE THE FILE.
C
 1160 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')THEN
        IF(I.GT.NUMDEV)NUMDEV=I
        IF(IDMANU(I).NE.'    ')IDPOWE(I)='ON'
        GOTO1180
      ELSEIF(IHARG(NUMARG).EQ.'OPEN')THEN
CCCCC   JUNE 1992.  FOLLOWING BLOCK ADDED.
        IF(I.EQ.2.AND.IPL1CS(1:4).EQ.'OPEN')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3164)
 3164     FORMAT('      DEVICE 2 IS ALREADY OPEN')
          CALL DPWRST('XXX','BUG ')
          GOTO1199
        ELSEIF(I.EQ.3.AND.IPL2CS(1:4).EQ.'OPEN')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3174)
 3174     FORMAT('      DEVICE 3 IS ALREADY OPEN')
          CALL DPWRST('XXX','BUG ')
          GOTO1199
        ENDIF
        IF(I.GT.NUMDEV)NUMDEV=I
        IF(IDMANU(I).NE.'    ')THEN
          IDPOWE(I)='ON'
          IOPERA='OPEN'
        ENDIF
        GOTO1179
      ELSEIF(IHARG(NUMARG).EQ.'OFF')THEN
        IF(I.GT.NUMDEV)NUMDEV=I
        IDPOWE(I)='OFF'
        GOTO1180
      ELSEIF(IHARG(NUMARG).EQ.'CLOS')THEN
        IF(I.GT.NUMDEV)NUMDEV=I
        IDPOWE(I)='OFF'
        IOPERA='CLOS'
        GOTO1179
      ELSEIF(IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
        IF(I.GT.NUMDEV)NUMDEV=I
        IDMANU(I)=IDEFMA
        IDMODE(I)=IDEFMO
        IDMOD2(I)=IDEFM2
        IDMOD3(I)=IDEFM3
        IDPOWE(I)=IDEFPO
        IDCONT(I)=IDEFCN
        IDCOLO(I)=IDEFDC
        IDNVPP(I)=IDEFVP
        IDNHPP(I)=IDEFHP
        IDUNIT(I)=IDEFUN
        GOTO1180
      ELSE
C
C       FEBRUARY,1989.  "QMS" WILL BE SET TO "QUIC".  "LASER JET" TO PCL.
CCCCC   JUNE 1992.      DON"T RE-OPEN DEVICE IF ALREADY OPEN
CCCCC                   FOLLOWING FOR DEBUGGING PURPOSES
C
        IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEMN')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1173)I,IPL1CS,IPL2CS
 1173     FORMAT('IN LOOP 1175, I,IPL1CS,IPL2CS=',I4,A4,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(I.EQ.2.AND.IPL1CS(1:4).EQ.'OPEN')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3164)
          CALL DPWRST('XXX','BUG ')
          GOTO1199
        ELSEIF(I.EQ.3.AND.IPL2CS(1:4).EQ.'OPEN')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3174)
          CALL DPWRST('XXX','BUG ')
          GOTO1199
        ENDIF
C
        IF(I.GT.NUMDEV)NUMDEV=I
        K=2
        IF(IHARG(2).EQ.'MANU')K=3
        IF(IHARG(2).EQ.'MODE')K=3
        IDMANU(I)='    '
        IF(K.LE.NUMARG)IDMANU(I)=IHARG(K)
        ISAVE=IDMANU(I)
        IF(ISAVE.EQ.'HPGL')THEN
          IDMANU(I)='HP  '
        ELSEIF(ISAVE.EQ.'QMS' .OR. ISAVE.EQ.'TELA')THEN
          IDMANU(I)='QUIC'
        ELSEIF(ISAVE.EQ.'LASE')THEN
          IDMANU(I)='PCL '
        ELSEIF(ISAVE.EQ.'PS  ')THEN
          IDMANU(I)='POST'
        ENDIF
        K=K+1
        IF(K.LE.NUMARG)IDMODE(I)=IHARG(K)
C
C       FOLLOWING LINE MOVED TO BELOW TO CHECK FOR HP-GL MODEL NUMBERS
CCCCC   IF(ISAVE.EQ.'HPGL')IDMODE(I)='GL  '
C
        IF(ISAVE.EQ.'LASE'.AND.IDMODE(I).EQ.'JET')IDMODE(I)='    '
        IF(K.GT.NUMARG)IDMODE(I)='    '
        K=K+1
        IDMOD2(I)='    '
        IF(K.LE.NUMARG)IDMOD2(I)=IHARG(K)
        K=K+1
        IDMOD3(I)='    '
        IF(K.LE.NUMARG)IDMOD3(I)=IHARG(K)
C
C       FOLLOWING BLOCK ADDED TO CHECK FOR HPGL MODEL NUMBERS
C
        IF(ISAVE.EQ.'HPGL')THEN
          IDMOD3(I)=IDMOD2(I)
          IDMOD2(I)=IDMODE(I)
          IDMODE(I)='GL  '
        ELSEIF(ISAVE.EQ.'EPS ' .OR. ISAVE.EQ.'ENCA')THEN
          IDMANU(I)='POST'
          IDMODE(I)='ENCA'
        ENDIF
C
        CALL GRSEPP(I,IPL1NU,IPL2NU,
     1              IDMANU,IDMODE,IDMOD2,IDMOD3,
     1              IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,
     1              IDUNIT,IDNVOF,IDNHOF,
     1              IBUGO2,IFOUN2,IERROR)
        IOPERA='OPEN'
        GOTO1179
      ENDIF
C
 1180 CONTINUE
C
      IOPERA=IDPOWE(I)
C
C     MAY, 1980: FOLLOWING LINE ADDED (DEVICE ... CLOSE)
C
 1179 CONTINUE
C
C     FEBRUARY,1989.  SEPARATE UNITS FOR GRAPHICS AND ALPHANUMERIC
C                     OUTPUT.  SAME ON MOST SYSTEMS, BUT CDC NOS/VE
C                     REQUIRES DIFFERENT ATTRIBUTES FOR GRAPHICS AND
C                     ALPANUMERIC OUTPUT.
CCCCC IGENNU=IPR
      IGENNU=IPRGR
C
      IF(I.EQ.1)THEN
        IGENID='SCRE'
        IF(IDMANU(1).EQ.'LATE'.AND.ICAPSW.EQ.'ON')THEN
          IGENNU=ICAPNU
          IPRGR=IGENNU
        ELSE
          IGENNU=IPRGR
        ENDIF
      ELSEIF(I.EQ.2)THEN
        IGENNU=IPL1NU
        IGENID='PLO1'
      ELSEIF(I.EQ.3)THEN
        IGENNU=IPL2NU
        IGENID='PLO2'
      ELSEIF(I.GE.4)THEN
        IGENNU=IDUNIT(I)
        IGENID='GENE'
      ENDIF
C
      IGDFLG='OFF'
      IF(IDMANU(I).EQ.'GD  '.OR.IDMANU(I).EQ.'GDI ')IGDFLG='ON'
      ICAFLG='OFF'
      IF(IDMANU(I).EQ.'CAIR')ICAFLG='ON'
C
CCCCC IF(IGENNU.NE.IPR)
CCCCC IF(IGENNU.NE.IPRGR)
      CALL DPDEP2(IOPERA,IGENNU,IGENID,IGDFLG,ICAFLG,
     1            ICAPSW,
     1            IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
C
      IF(IERROR.EQ.'YES')GOTO9000
C
CCCCC JUNE 1992.  FOLLOWINF LINE ADDED
      IF(IOPERA.EQ.'OPEN' .OR. IOPERA.EQ.'ON')THEN
        IMANUF=IDMANU(I)
        IMODEL=IDMODE(I)
C
CCCCC   AUGUST 1992.  FOLLOWING 2 LINES ADDED FOR HPGL LASERJET
C
        IMODE2=IDMOD2(I)
        IMODE3=IDMOD3(I)
        IGUNIT=IDUNIT(I)
C
C       FEBRUARY 2006: DEVICE 1 LATEX OUTPUT SHOULD GO TO CAPTURE
C                      FILE IF CAPTURE SWITCH IS ON.
C
        IF(IMANUF.EQ.'LATE' .AND. I.EQ.1 .AND. ICAPSW.EQ.'ON')THEN
          IGUNIT=ICAPNU
        ENDIF
C
        IBUGG4=IBUGO2
C
C       FOLLOWING LINE ADDED FEBRUARY, 1989 FOR POSTSCRIPT DEVICE
C
        ANUMVP=IDNVPP(I)
C
C       FOLLOWING LINE ADDED MARCH, 1990 FOR X11 DEVICE
C
        ANUMHP=IDNHPP(I)
C
CCCCC   MAY 1989: THE FOLLOWING 2 LINES WERE ADDED TO FIX POSTSCRIPT
C
        IOFFSV=IDNVOF(I)
        IOFFSH=IDNHOF(I)
C
CCCCC   JUNE 1992.  ONLY CALL FOR OPEN CASE (NOT FOR ON)
CCCCC   CALL GRINDE
C
        IF(IOPERA.EQ.'OPEN')THEN
          CALL GRINDE
          IF(IGFLAG.EQ.1)IDPOWE(I)='OFF'
        ENDIF
C
CCCCC   MARCH 1990: FOLLOWING THREE LINES FOR X11 LIBRARY WHICH CAN
CCCCC               DYNAMICALLY CHANGE THE NUMBER OF PICTURE POINTS.
C
        IF(IMANUF.EQ.'X11'.OR.IMANUF.EQ.'QWIN')THEN
          IDNVPP(I)=ANUMVP
          IDNHPP(I)=ANUMHP
        ENDIF
      ENDIF
C
      IFOUND='YES'
C
CCCCC MAY 1992: THE FOLLOWING 3 LINES WERE ADDED   MAY 1992 (JJF)
C
      IF(NUMARG.GE.1)THEN
         IF(IARGT(1).EQ.'NUMB'.AND.IARG(1).EQ.3)GOTO1199
      ENDIF
      IF(IFEEDB.EQ.'ON')THEN
        IF(IDMANU(I).EQ.'LATE' .AND. I.EQ.1 .AND. ICAPSW.EQ.'ON')
     1    GOTO1199
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)I
 1181   FORMAT('            DEVICE           --',I4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)IDUNIT(I)
 1182   FORMAT('            I/O UNIT         --',I4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)IDMANU(I)
 1183   FORMAT('            MANUFACTURER     --',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1184)IDMODE(I),IDMOD2(I),IDMOD3(I)
 1184   FORMAT('            MODEL            --',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1185)IDPOWE(I)
 1185   FORMAT('            POWER            --',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1186)IDCONT(I)
 1186   FORMAT('            CONTINUITY       --',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1187)IDCOLO(I)
 1187   FORMAT('            COLOR            --',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1188)IDNHPP(I)
 1188   FORMAT('            HORIZONTAL PIXELS--',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1189)IDNVPP(I)
 1189   FORMAT('            VERTICAL   PIXELS--',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(I.EQ.2)THEN
          WRITE(ICOUT,1192)IPL1NA
 1192     FORMAT('            FILE NAME (LOCAL)--',A80)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(I.EQ.3)THEN
          WRITE(ICOUT,1193)IPL2NA
 1193     FORMAT('            FILE NAME (LOCAL)--',A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
 1199 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEMN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DPDEMN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IOPERA,IMANUF,IMODEL
 9015   FORMAT('IOPERA,IMANUF,IMODEL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR,NUMARG
 9020   FORMAT('IFOUND,IERROR,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)IHARG(2),ISAVE,IDMANU(1),IDMODE(1)
 9041   FORMAT('IHARG(2),ISAVE,IDMANU(1),IDMODE(1) = ',
     1         3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEPM(ICOM,IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IBUGO2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE A PEN MAP (I.E., ASSOCIATE A COLOR WITH AN INDEX
C              NUMBER FOR A DEVICE).  THIS COMMAND IS INTENDED FOR PEN
C              PLOTTERS HAVE PEN SLOTS WHICH CAN BE LOADED WITH WHATEVER
C              COLOR PEN THE LOCAL OPERATOR DESIRES.  DATAPLOT WILL USE
C              A DEFAULT MAPPING.  THIS COMMAND ALLOWS A USER CONFIGURABLE
C              MAPPING TO OVERRIDE THE DEFAULT.  CURRENTLY, THE HPGL, ZETA,
C              AND CALCOMP DEVICES ARE SUPPORTED.  THE OTHER 2 PLOTTERS
C              CURRENTLY SUPPORTED (HP 7221 AND TEKTRONIX 4662) ARE
C              RATHER OBSOLETE, SO DATAPLOT CODE HAS NOT BEEN UPDATED
C              TO SUPPORT THEM THIS WAY (I.E., ONLY THE DEFAULT MAPPING
C              AVAILABLE).
C     INPUT  ARGUMENTS--ICOM   (A  CHARACTER VECTOR)
C                     --IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MAY       1990.
C     UPDATED         --JANUARY   1991.  REGIS SUPPORT
C     UPDATED         --JUNE      1991.  ADD SHOW X11 COLORS COMMAND
C     UPDATED         --APRIL     1992.  SHORTEN COLORS AFTER IZETPM(.)
C                                        TO 4 CHARACTERS (BUT NO NOTE NOTED)
C     UPDATED         --AUGUST    1992.  DATAPLOT COLORS HAVE BEEN
C                                        REDEFINED IN A CONSISTENT
C                                        MANNER FOR ALL DEVICES.
C                                        UPDATE SHOW COLORS COMMANDS TO
C                                        REFLECT THIS.
C     UPDATED         --SEPTEMBER 1993.  SPLIT MULTI-LINE FORMATS
C     UPDATED         --MARCH     1995.  ? AS SYNONYM FOR SHOW
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGO2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICOL
C
      CHARACTER*40 ICPREH
      CHARACTER*4 IRESP
C
      CHARACTER*4 IGRAY
      CHARACTER*4 IDEV
      CHARACTER*4 IDEV2
C
CCCCC CHARACTER*25 IRGCLR(64)
CCCCC CHARACTER*8 IRGNAM(64)
CCCCC CHARACTER*25 IXCLR(67)
CCCCC CHARACTER*8 IXNAM(67)
      CHARACTER*4 CJUNK
      PARAMETER(MAXCLR=89)
      CHARACTER*25 ICLR(MAXCLR)
      CHARACTER*8 INAM(MAXCLR)
      INTEGER J4027(MAXCLR)
      INTEGER J4105(MAXCLR)
      INTEGER JPLOT4(MAXCLR)
      INTEGER JPLOT8(MAXCLR)
      INTEGER J2622(MAXCLR)
      INTEGER JCGM(MAXCLR)
      INTEGER JSUN(MAXCLR)
      INTEGER JX11(MAXCLR)
      INTEGER JPC(MAXCLR)
      INTEGER JREGIS(MAXCLR)
      INTEGER JTEMP(MAXCLR)
      INTEGER JREG2(64)
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCODV.INC'
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
C  TEKTRONIX 4027
C
      DATA (J4027(I),I=1,MAXCLR)/
     1  0,  7,  1,  3,  2,  6,  5,  3,  4,  4,
     2  2,  3,  3,  1,  7,  0,  3,  5,  3,  1,
     3  3,  2,  6,  3,  6,  4,  2,  4,  4,  7,
     4  1,  4,  0,  3,  2,  1,  3,  3,  2,  4,
     5  6,  2,  3,  2,  6,  6,  3,  3,  5,  6,
     6  2,  1,  6,  6,  4,  2,  6,  3,  3,  2,
     7  3,  4,  4,  6,  6,  4,  2,  3,  3,  3,
     8  3,  3,  3,  3,  2,  2,  2,  4,  4,  4,
     9  5,  5,  5,  1,  1,  1,  6,  6,  6/
C  TEKTRONIX 4105, GENERAL, GENERAL CODED
C
      DATA (J4105(I),I=1,MAXCLR)/
     1  1,  0,  2,  4,  3,  6,  2,  5,  7,  7,
     2  3,  4,  4,  2,  0,  1,  4,  2,  4,  2,
     3  4,  3,  6,  4,  6,  7,  3,  7,  7,  0,
     4  2,  7,  1,  4,  3,  2,  4,  4,  3,  7,
     5  6,  3,  4,  3,  6,  6,  4,  4,  2,  6,
     6  3,  2,  6,  6,  7,  3,  6,  4,  4,  3,
     7  4,  7,  7,  6,  6,  7,  3,  5,  4,  4,
     8  4,  5,  5,  5,  3,  3,  3,  7,  7,  7,
     9  7,  7,  7,  2,  2,  2,  6,  6,  6/
C
C  PLOTTERS WITH 4 PENS (TEKTRONIX 4662, HP-7221, CALCOMP, ZETA, HP-GL)
C
      DATA (JPLOT4(I),I=1,MAXCLR)/
     1  1,  1,  2,  3,  4,  4,  2,  3,  2,  2,
     2  4,  3,  3,  2,  1,  1,  3,  2,  3,  2,
     3  3,  4,  2,  3,  2,  2,  4,  2,  2,  1,
     4  2,  2,  1,  3,  4,  2,  3,  3,  4,  2,
     5  2,  4,  3,  4,  2,  2,  3,  3,  2,  2,
     6  4,  2,  2,  2,  2,  4,  2,  3,  3,  4,
     7  3,  2,  2,  2,  2,  2,  4,  3,  3,  3,
     8  3,  3,  3,  3,  4,  4,  4,  2,  2,  2,
     9  2,  2,  2,  2,  2,  2,  4,  4,  4/
C
C  PLOTTERS WITH 8 PENS (HP-GL, CALCOMP, ZETA)
C
      DATA (JPLOT8(I),I=1,MAXCLR)/
     1  1,  1,  2,  3,  4,  5,  6,  7,  8,  8,
     2  4,  7,  3,  2,  1,  1,  3,  8,  3,  2,
     3  3,  4,  5,  3,  5,  8,  4,  8,  8,  1,
     4  2,  8,  1,  3,  4,  5,  7,  3,  4,  8,
     5  5,  4,  7,  4,  5,  5,  3,  3,  6,  5,
     6  4,  2,  5,  5,  8,  4,  5,  7,  3,  4,
     7  3,  8,  8,  5,  5,  8,  4,  7,  3,  3,
     8  3,  7,  7,  7,  4,  4,  4,  8,  8,  8,
     9  6,  6,  6,  2,  2,  2,  5,  5,  5/
C
C  HP-2622 AND RELATED TERMINALS
C
      DATA (J2622(I),I=1,MAXCLR)/
     1  7,  0,  1,  4,  2,  5,  3,  6,  3,  3,
     2  2,  6,  4,  1,  0,  7,  6,  3,  4,  1,
     3  4,  2,  5,  4,  5,  3,  2,  3,  3,  0,
     4  1,  3,  7,  6,  2,  5,  6,  4,  2,  3,
     5  5,  2,  6,  2,  5,  5,  4,  4,  3,  5,
     6  2,  1,  5,  5,  3,  2,  5,  6,  4,  2,
     7  4,  3,  3,  5,  5,  3,  2,  6,  4,  4,
     8  4,  6,  6,  6,  2,  2,  2,  3,  3,  3,
     9  3,  3,  3,  1,  1,  1,  5,  5,  5/
C
C  DIRECT RGB DEVICES (CGM, POSTSCRIPT)
C
      DATA (JCGM(I),I=1,MAXCLR)/
     1  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     2 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     3 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
     4 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
     5 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
     6 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
     7 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
     8 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
     9 81, 82, 83, 84, 85, 86, 87, 88, 89/
C
C  SUN
C
      DATA (JSUN(I),I=1,MAXCLR)/
     1  7,  5,  1,  3,  2,  6,  4,  3,  4,  4,
     2  2,  3,  3,  1,  0,  7,  3,  4,  3,  1,
     3  3,  2,  6,  3,  6,  4,  2,  4,  4,  0,
     4  1,  4,  7,  3,  2,  6,  3,  3,  2,  4,
     5  6,  2,  3,  2,  6,  6,  3,  3,  1,  6,
     6  2,  1,  6,  6,  4,  2,  6,  3,  3,  2,
     7  3,  4,  4,  6,  6,  4,  2,  3,  3,  3,
     8  3,  3,  3,  3,  2,  2,  2,  4,  4,  4,
     9  4,  4,  4,  1,  1,  1,  6,  6,  6/
C
C  X11
C
      DATA (JX11(I),I=1,MAXCLR)/
     1  1,  0,  4,  5,  2,  6,  8,  7,  3,  9,
     2 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
     3 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
     4 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
     5 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
     6 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
     7 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
     8 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
     9 80, 81, 82, 83, 84, 85, 86, 87, 88/
C
C  REGIS
C
      DATA (JREGIS(I),I=1,MAXCLR)/
     1 62,  3, 47,  4, 23, 39, 41, 18, 63, 64,
     2 24,  8, 60, 51, 35, 37,  1,  3,  5, 17,
     3  6, 25, 43,  7, 57, 19, 26, 20, 21, 35,
     4 48, 38, 36,  9, 27, 40,  2, 10, 28, 22,
     5 44, 29, 11, 30, 58, 49, 12, 13, 50, 42,
     6 31, 45, 46, 57, 52, 32, 53, 14, 15, 33,
     7 16, 54, 55, 56, 59, 61, 34, 18,  4,  4,
     8  4, 18, 18, 18, 23, 23, 23, 63, 63, 63,
     9 41, 41, 41, 47, 47, 47, 39, 39, 39/
      DATA (JREG2(I),I=1,64)/
     1 17, 37,  2,  4, 19, 21, 24, 12, 34, 38,
     2 43, 47, 48, 58, 59, 60, 20,  8, 26, 28,
     3 29, 40,  5, 11, 22, 27, 35, 39, 42, 44,
     4 51, 56, 60, 67, 15, 33, 16, 32,  6, 36,
     5  7, 50, 23, 41, 52, 53,  3, 31, 46, 49,
     6 14, 55, 57, 62, 63, 64, 25, 45, 65, 13,
     7 66,  1,  9, 10/
C
C  IBM-PC
C
      DATA (JPC(I),I=1,MAXCLR)/
     1 15,  0,  4,  1,  2,  5, 14,  3, 14, 14,
     2  2,  9,  1,  4,  8, 10,  3,  6,  9, 12,
     3  1,  2,  5,  1,  5, 14,  2, 14, 14,  7,
     4 12, 14, 10,  9,  2,  5, 11,  9,  2, 14,
     5  5,  2,  9,  2, 13,  5,  1,  1,  4, 13,
     6  2,  4,  5,  5, 14,  2,  5, 11,  1,  2,
     7  1,  6, 14,  5,  5, 14,  2, 11,  1,  1,
     8  1,  3,  3,  3,  2,  2,  2, 14, 14, 14,
     9 14, 14, 14,  4,  4,  4,  5,  5,  5/
C
C
C  COMMENT OUT FOLLOWING, USE SAME TABLES FOR ALL DEVICES
C
CCCCC DATA (IRGCLR(I),I=1,15)/
CCCCC1 'Aquamarine',
CCCCC2 'Aquamarine, Medium',
CCCCC3 'Black',
CCCCC4 'Blue',
CCCCC5 'Blue, Cadet',
CCCCC6 'Blue, Cornflower',
CCCCC7 'Blue, Dark Slate',
CCCCC8 'Blue, Light',
CCCCC9 'Blue, Light Steel',
CCCCC* 'Blue, Medium',
CCCCC1 'Blue, Medium Slate',
CCCCC2 'Blue, Midnight',
CCCCC3 'Blue, Navy',
CCCCC4 'Blue, Sky',
CCCCC5 'Blue, Slate'/
CCCCC DATA (IRGCLR(I),I=16,30)/
CCCCC1 'Blue, Steel',
CCCCC2 'Coral',
CCCCC3 'Cyan',
CCCCC4 'Firebrick',
CCCCC5 'Gold',
CCCCC6 'Goldenrod',
CCCCC7 'Goldenrod, Medium',
CCCCC8 'Green',
CCCCC9 'Green, Dark',
CCCCC* 'Green, Dark Olive',
CCCCC1 'Green, Forest',
CCCCC2 'Green, Lime',
CCCCC3 'Green, Medium Forest',
CCCCC4 'Green, Medium Sea',
CCCCC5 'Green, Medium Spring'/
CCCCC DATA (IRGCLR(I),I=31,45)/
CCCCC1 'Greeen, Pale',
CCCCC2 'Green, Sea',
CCCCC3 'Green, Spring',
CCCCC4 'Green, Yellow',
CCCCC5 'Grey, Dark Slate',
CCCCC6 'Grey, Dim',
CCCCC7 'Grey, Light',
CCCCC8 'Khaki',
CCCCC9 'Magenta',
CCCCC* 'Maroon',
CCCCC1 'Orange',
CCCCC2 'Orchid',
CCCCC3 'Orchid, Dark',
CCCCC4 'Orchid, Medium',
CCCCC5 'Pink'/
CCCCC DATA (IRGCLR(I),I=46,60)/
CCCCC1 'Plum',
CCCCC2 'Red',
CCCCC3 'Red, Indian',
CCCCC4 'Red, Medium Violet',
CCCCC5 'Red, Orange',
CCCCC6 'Red, Violet',
CCCCC7 'Salmon',
CCCCC8 'Sienna',
CCCCC9 'Tan',
CCCCC* 'Thistle',
CCCCC1 'Turquoise',
CCCCC2 'Turqoise, Dark',
CCCCC3 'Turqoise, Medium',
CCCCC4 'Violet',
CCCCC5 'Violet, Blue'/
CCCCC DATA (IRGCLR(I),I=61,64)/
CCCCC1 'Wheat',
CCCCC2 'White',
CCCCC3 'Yellow',
CCCCC4 'Yellow, Green'/
C
CCCCC DATA (IRGNAM(I),I=1,15)/
CCCCC1 'AQUA',
CCCCC2 '2',
CCCCC3 'BLACK',
CCCCC4 'BLUE',
CCCCC5 '5',
CCCCC6 '6',
CCCCC7 '7',
CCCCC8 '8',
CCCCC9 '9',
CCCCC* '10',
CCCCC1 '11',
CCCCC2 '12',
CCCCC3 '13',
CCCCC4 '14',
CCCCC5 '15'/
CCCCC DATA (IRGNAM(I),I=16,30)/
CCCCC1 '16',
CCCCC2 'CORAL',
CCCCC3 'CYAN',
CCCCC4 'FIREBRIC',
CCCCC5 'GOLD',
CCCCC6 '21',
CCCCC7 '22',
CCCCC8 'GREEN',
CCCCC9 '24',
CCCCC* '25',
CCCCC1 '26',
CCCCC2 '27',
CCCCC3 '28',
CCCCC4 '29',
CCCCC5 '30'/
CCCCC DATA (IRGNAM(I),I=31,45)/
CCCCC1 '31',
CCCCC2 '32',
CCCCC3 '33',
CCCCC4 '34',
CCCCC5 'GREY',
CCCCC6 '36',
CCCCC7 '37',
CCCCC8 'KHAKI',
CCCCC9 'MAGENTA',
CCCCC* 'MAROON',
CCCCC1 'ORANGE',
CCCCC2 'ORCHID',
CCCCC3 '43',
CCCCC4 '44',
CCCCC5 'PINK'/
CCCCC DATA (IRGNAM(I),I=46,60)/
CCCCC1 'PLUM',
CCCCC2 'RED',
CCCCC3 '48',
CCCCC4 '49',
CCCCC5 '50',
CCCCC6 '51',
CCCCC7 'SALMON',
CCCCC8 'SIENNA',
CCCCC9 'TAN',
CCCCC* 'THISTLE',
CCCCC1 'TURQUOIS',
CCCCC2 '57',
CCCCC3 '58',
CCCCC4 'VIOLET',
CCCCC5 '60'/
CCCCC DATA (IRGNAM(I),I=61,64)/
CCCCC1 'WHEAT',
CCCCC2 'WHITE',
CCCCC3 'YELLOW',
CCCCC4 '64'/
C
C  AUGUST 1992.
C  CHANGE IXCLR TO ICLR, IXNAM TO INAM, REORDER TO MATCH NEW ORDER.
C
CCCCC DATA (IXCLR(I),I=1,15)/
      DATA (ICLR(I),I=1,15)/
     1 'White',
     2 'Black',
     3 'Red',
     4 'Blue',
     5 'Green',
     6 'Magenta',
     7 'Orange',
     8 'Cyan',
     9 'Yellow',
     * 'Yellow Green',
     1 'Dark Green',
     2 'Light Blue',
     3 'Blue Violet',
     4 'Violet Red',
     5 'Dark Slate Gray'/
CCCCC DATA (IXCLR(I),I=16,30)/
      DATA (ICLR(I),I=16,30)/
     1 'Light Gray',
     2 'Aquamarine',
     3 'Brown',
     4 'Cadet Blue',
     5 'Coral',
     6 'Cornflower Blue',
     7 'Dark Olive Green',
     8 'Dark Orchid',
     9 'Dark Slate Blue',
     * 'Dark Turquoise',
     1 'Firebrick',
     2 'Forest Green',
     3 'Gold',
     4 'Goldenrod',
     5 'Gray'/
CCCCC DATA (IXCLR(I),I=31,45)/
      DATA (ICLR(I),I=31,45)/
     1 'Indian Red',
     2 'Khaki',
     3 'Dim Gray',
     4 'Light Blue Steel',
     5 'Lime Green',
     6 'Maroon',
     7 'Medium Aquamarine',
     8 'Medium Blue',
     9 'Medium Forest Green',
     * 'Medium Goldenrod',
     1 'Medium Orchid',
     2 'Medium Sea Green',
     3 'Medium Slate Blue',
     4 'Medium Spring Green',
     5 'Medium Turquoise'/
CCCCC DATA (IXCLR(I),I=46,60)/
      DATA (ICLR(I),I=46,60)/
     1 'Medium Violet Red',
     2 'Midnight Blue',
     3 'Navy',
     4 'Orange Red',
     5 'Orchid',
     6 'Pale Green',
     7 'Pink',
     8 'Plum',
     9 'Purple',
     * 'Salmon',
     1 'Sea Green',
     2 'Sienna',
     3 'Sky Blue',
     4 'Slate Blue',
     5 'Spring Green'/
CCCCC DATA (IXCLR(I),I=61,66)/
      DATA (ICLR(I),I=61,75)/
     1 'Steel Blue',
     2 'Tan',
     3 'Thistle',
     4 'Turquoise',
     5 'Violet',
     6 'Wheat',
     7 'Green Yellow',
     8 'Light Cyan',
     9 'Blue2',
     * 'Blue3',
     1 'Blue4',
     2 'Cyan2',
     3 'Cyan3',
     4 'Cyan4',
     5 'Green2'/
      DATA (ICLR(I),I=76,MAXCLR)/
     1 'Green3',
     2 'Green4',
     3 'Yellow2',
     4 'Yellow3',
     5 'Yellow4',
     6 'Orange2',
     7 'Orange3',
     8 'Orange4',
     9 'Red2',
     * 'Red3',
     1 'Red4',
     2 'Magenta2',
     3 'Magenta3',
     4 'Magenta4'/
C
CCCCC DATA (IXNAM(I),I=1,15)/
      DATA (INAM(I),I=1,15)/
     1 'WHITE',
     2 'BLACK',
     3 'RED',
     4 'BLUE',
     5 'GREEN',
     6 'MAGENTA',
     7 'ORANGE',
     8 'CYAN',
     9 'YELLOW',
     * 'YGRE',
     1 'DGRE',
     2 'LBLU',
     3 'VBLU',
     4 'VRED',
     5 'DGRA'/
CCCCC DATA (IXNAM(I),I=16,30)/
      DATA (INAM(I),I=16,30)/
     1 'LGRA',
     2 'AQUA',
     3 'BROWN',
     4 'CABL',
     5 'CORAL',
     6 'CBLU',
     7 'DOGR',
     8 'DORC',
     9 'DSBL',
     * 'DTUR',
     1 'FIRE',
     2 'FGRE',
     3 'GOLD',
     4 'GLDR',
     5 'GRAY'/
CCCCC DATA (IXNAM(I),I=31,45)/
      DATA (INAM(I),I=31,45)/
     1 'IRED',
     2 'KHAKI',
     3 'DMGR',
     4 'LSBL',
     5 'LGRE',
     6 'MAROON',
     7 'MAQU',
     8 'MBLU',
     9 'MFGR',
     * 'MGLD',
     1 'MORC',
     2 'MSGR',
     3 'MSBL',
     4 'MSPG',
     5 'MTUR'/
CCCCC DATA (IXNAM(I),I=46,60)/
      DATA (INAM(I),I=46,60)/
     1 'MVRD',
     2 'MDBL',
     3 'NAVY',
     4 'ORED',
     5 'ORCHID',
     6 'PGRE',
     7 'PINK',
     8 'PLUM',
     9 'PURPLE',
     * 'SALMON',
     1 'SGRE',
     2 'SIENNA',
     3 'SKBL',
     4 'SBLU',
     5 'SPGR'/
CCCCC DATA (IXNAM(I),I=61,66)/
      DATA (INAM(I),I=61,75)/
     1 'STBL',
     2 'TAN',
     3 'THISTLE',
     4 'TURQ',
     5 'VIOLET',
     6 'WHEAT',
     7 'GYEL',
     8 'LCYA',
     9 'BLU2',
     * 'BLU3',
     1 'BLU4',
     2 'CYA2',
     3 'CYA3',
     4 'CYA4',
     5 'GRE2'/
      DATA (INAM(I),I=76,MAXCLR)/
     1 'GRE3',
     2 'GRE4',
     3 'YEL2',
     4 'YEL3',
     5 'YEL4',
     6 'ORA2',
     7 'ORA3',
     8 'ORA4',
     9 'RED2',
     * 'RED3',
     1 'RED4',
     2 'MAG2',
     3 'MAG3',
     4 'MAG4'/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      IBUGG4='OFF'
      ISUBG4='-999'
C
      IF(IBUGO2.EQ.'OFF'.AND.ISUBRO.NE.'DEPM')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('AT THE BEGINNING OF DPDEPM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGO2
   53 FORMAT('IBUGO2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)IFOUND,IERROR
   60 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,68)NUMARG
   68 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO70I=1,NUMARG
      WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I)
   71 FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,2X,I8)
      CALL DPWRST('XXX','BUG ')
   70 CONTINUE
      WRITE(ICOUT,72)IHPGPF
   72 FORMAT('IHPGPF=',A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      IF(ICOM.EQ.'HPGL')GOTO1100
      IF(ICOM.EQ.'HP-G')GOTO1100
      IF(ICOM.EQ.'ZETA')GOTO2100
      IF(ICOM.EQ.'CALC')GOTO3100
      IF(ICOM.EQ.'REGI')GOTO4100
      IF(ICOM.EQ.'X11')GOTO5100
C  ADD FOLLOWING LINE AUGUST 1992.
      IF(ICOM.EQ.'SHOW')GOTO6100
C
C  *****************************************
C  **  HPGL CASE                          **
C  *****************************************
C
 1100 CONTINUE
      IF(IHARG(1).EQ.'PEN'.AND.IHARG(2).EQ.'MAP')GOTO1110
      IF(IHARG(1).EQ.'COLO'.AND.IHARG(2).EQ.'MAP')GOTO1110
      IF(IHARG(1).EQ.'PEN')GOTO1120
      IF(IHARG(1).EQ.'MAP')GOTO1120
      IF(IHARG(1).EQ.'COLO')GOTO1120
 
C
 1110 CONTINUE
      IARGCL=3
      IARGIN=4
      GOTO1190
C
 1120 CONTINUE
      IARGCL=2
      IARGIN=3
      GOTO1190
C
 1190 CONTINUE
      IFOUND='YES'
      IF(NUMARG.LT.IARGCL)GOTO1910
      IF(IHARG(IARGCL).EQ.'AUTO')GOTO1500
      IF(IHARG(IARGCL).EQ.'DEFA')GOTO1500
      IF(IHARG(IARGCL).EQ.'ON  ')GOTO1600
      IF(IHARG(IARGCL).EQ.'OFF ')GOTO1700
      IF(IHARG(IARGCL).EQ.'LIST')GOTO1800
      IF(IHARG(IARGCL).EQ.'SHOW')GOTO1800
      IF(IHARG(IARGCL).EQ.'?   ')GOTO1800
      IF(IHARG(IARGCL).EQ.'PRIN')GOTO1800
      IF(NUMARG.LT.IARGIN)GOTO1920
C
      ICOL=IHARG(IARGCL)
      INDEX=IARG(IARGIN)
      IF(INDEX.LT.1.OR.INDEX.GT.16)GOTO1930
      IHPGPM(INDEX)=ICOL
      IHPGPF='ON'
      IF(IFEEDB.EQ.'OFF')WRITE(ICOUT,1490)IHPGPM(INDEX),INDEX
 1490 FORMAT('COLOR ',A4,' WILL SELECT PEN ',I2,' FOR HP-GL')
      IF(IFEEDB.EQ.'OFF')CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1500 CONTINUE
      IF(IHPGCL.LE.4)THEN
        IHPGPM(1)='BLACK'
        IHPGPM(2)='RED '
        IHPGPM(3)='BLUE'
        IHPGPM(4)='GREEN'
        DO1510J=5,16
        ITEMP=MOD(J-1,4)+1
        IHPGPM(J)=IHPGPM(ITEMP)
 1510   CONTINUE
      ELSE
        IHPGPM(1)='BLACK'
        IHPGPM(2)='RED '
        IHPGPM(3)='BLUE'
        IHPGPM(4)='GREEN'
        IHPGPM(5)='MAGENTA'
        IHPGPM(6)='ORANGE'
        IHPGPM(7)='CYAN'
        IHPGPM(8)='YELLOW'
        DO1520J=9,16
        ITEMP=J-8
        IHPGPM(J)=IHPGPM(ITEMP)
 1520   CONTINUE
      END IF
      GOTO9000
C
 1600 CONTINUE
      IHPGPF='ON'
      GOTO9000
C
 1700 CONTINUE
      IHPGPF='OFF'
      GOTO9000
C
 1800 CONTINUE
      WRITE(ICOUT,1805)
 1805 FORMAT('FOR THE HP-GL PENPLOTTER:')
      CALL DPWRST('XXX','BUG ')
      DO1810I=1,16
      WRITE(ICOUT,1811)IHPGPM(I),I
      CALL DPWRST('XXX','BUG ')
 1810 CONTINUE
 1811 FORMAT('COLOR ',A8,' SET TO PEN ',I2)
      GOTO9000
C
 1910 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1911)
 1911 FORMAT('NO COLOR SPECIFIED FOR HPGL PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1920 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1921)
 1921 FORMAT('NO INDEX SPECIFIED FOR HPGL PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1930 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1931)
 1931 FORMAT('INVALID INDEX SPECIFIED FOR HPGL PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C  *****************************************
C  **  ZETA CASE                          **
C  *****************************************
C
 2100 CONTINUE
      IF(IHARG(1).EQ.'PEN'.AND.IHARG(2).EQ.'MAP')GOTO2110
      IF(IHARG(1).EQ.'COLO'.AND.IHARG(2).EQ.'MAP')GOTO2110
      IF(IHARG(1).EQ.'PEN')GOTO2120
      IF(IHARG(1).EQ.'MAP')GOTO2120
      IF(IHARG(1).EQ.'MAP')GOTO2120
      IF(IHARG(1).EQ.'COLO')GOTO2120
C
 2110 CONTINUE
      IARGCL=3
      IARGIN=4
      GOTO2190
C
 2120 CONTINUE
      IARGCL=2
      IARGIN=3
      GOTO2190
C
 2190 CONTINUE
      IFOUND='YES'
      IF(NUMARG.LT.IARGCL)GOTO2910
      IF(IHARG(IARGCL).EQ.'AUTO')GOTO2500
      IF(IHARG(IARGCL).EQ.'DEFA')GOTO2500
      IF(IHARG(IARGCL).EQ.'ON  ')GOTO2600
      IF(IHARG(IARGCL).EQ.'OFF ')GOTO2700
      IF(IHARG(IARGCL).EQ.'LIST')GOTO2800
      IF(IHARG(IARGCL).EQ.'SHOW')GOTO2800
      IF(IHARG(IARGCL).EQ.'?   ')GOTO2800
      IF(IHARG(IARGCL).EQ.'PRIN')GOTO2800
      IF(NUMARG.LT.IARGIN)GOTO2920
C
      ICOL=IHARG(IARGCL)
      INDEX=IARG(IARGIN)
      IF(INDEX.LT.1.OR.INDEX.GT.26)GOTO2930
      IZETPM(INDEX)=ICOL
      IZETPF='ON'
      IF(IFEEDB.EQ.'OFF')WRITE(ICOUT,2490)IZETPM(INDEX),INDEX
 2490 FORMAT('COLOR ',A4,' WILL SELECT PEN ',I2,' FOR ZETA')
      IF(IFEEDB.EQ.'OFF')CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 2500 CONTINUE
      IF(IZETCL.LE.4)THEN
        IZETPM(1)='BLAC'
        IZETPM(2)='RED '
        IZETPM(3)='BLUE'
        IZETPM(4)='GREE'
        DO2510J=5,16
        ITEMP=MOD(J-1,4)+1
        IZETPM(J)=IZETPM(ITEMP)
 2510   CONTINUE
      ELSE
        IZETPM(1)='BLAC'
        IZETPM(2)='RED '
        IZETPM(3)='BLUE'
        IZETPM(4)='GREE'
        IZETPM(5)='MAGE'
        IZETPM(6)='ORAN'
        IZETPM(7)='CYAN'
        IZETPM(8)='YELL'
        DO2520J=9,16
        ITEMP=J-8
        IZETPM(J)=IZETPM(ITEMP)
 2520   CONTINUE
      END IF
      GOTO9000
C
 2600 CONTINUE
      IZETPF='ON'
      GOTO9000
C
 2700 CONTINUE
      IZETPF='OFF'
      GOTO9000
C
 2800 CONTINUE
      WRITE(ICOUT,2805)
 2805 FORMAT('FOR THE ZETA PENPLOTTER:')
      CALL DPWRST('XXX','BUG ')
      DO2810I=1,16
      WRITE(ICOUT,2811)IZETPM(I),I
      CALL DPWRST('XXX','BUG ')
 2810 CONTINUE
 2811 FORMAT('COLOR ',A8,' SET TO PEN ',I2)
      GOTO9000
C
 2910 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,2911)
 2911 FORMAT('NO COLOR SPECIFIED FOR ZETA PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 2920 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,2921)
 2921 FORMAT('NO INDEX SPECIFIED FOR ZETA PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 2930 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,2931)
 2931 FORMAT('INVALID INDEX SPECIFIED FOR ZETA PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C  *****************************************
C  **  CALCOMP CASE                          **
C  *****************************************
C
 3100 CONTINUE
      IF(IHARG(1).EQ.'PEN'.AND.IHARG(2).EQ.'MAP')GOTO3110
      IF(IHARG(1).EQ.'COLO'.AND.IHARG(2).EQ.'MAP')GOTO3110
      IF(IHARG(1).EQ.'PEN')GOTO3120
      IF(IHARG(1).EQ.'MAP')GOTO3120
      IF(IHARG(1).EQ.'MAP')GOTO3120
      IF(IHARG(1).EQ.'COLO')GOTO3120
C
 3110 CONTINUE
      IARGCL=3
      IARGIN=4
      GOTO3390
C
 3120 CONTINUE
      IARGCL=2
      IARGIN=3
      GOTO3390
C
 3390 CONTINUE
      IFOUND='YES'
      IF(NUMARG.LT.IARGCL)GOTO3910
      IF(IHARG(IARGCL).EQ.'AUTO')GOTO3500
      IF(IHARG(IARGCL).EQ.'DEFA')GOTO3500
      IF(IHARG(IARGCL).EQ.'ON  ')GOTO3600
      IF(IHARG(IARGCL).EQ.'OFF ')GOTO3700
      IF(IHARG(IARGCL).EQ.'LIST')GOTO3800
      IF(IHARG(IARGCL).EQ.'PRIN')GOTO3800
      IF(IHARG(IARGCL).EQ.'SHOW')GOTO3800
      IF(IHARG(IARGCL).EQ.'?   ')GOTO3800
      IF(NUMARG.LT.IARGIN)GOTO3920
C
      ICOL=IHARG(IARGCL)
      INDEX=IARG(IARGIN)
      IF(INDEX.LT.1.OR.INDEX.GT.16)GOTO3930
      ICALPM(INDEX)=ICOL
      ICALPF='ON'
      IF(IFEEDB.EQ.'OFF')WRITE(ICOUT,3490)ICALPM(INDEX),INDEX
 3490 FORMAT('COLOR ',A4,' WILL SELECT PEN ',I2,' FOR CALCOMP ')
      IF(IFEEDB.EQ.'OFF')CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 3500 CONTINUE
      IF(ICALCL.LE.4)THEN
        ICALPM(1)='BLAC'
        ICALPM(2)='RED '
        ICALPM(3)='GREE'
        ICALPM(4)='BLUE'
        DO3510J=5,16
        ITEMP=MOD(J-1,4)+1
        ICALPM(J)=ICALPM(ITEMP)
 3510   CONTINUE
      ELSE
        ICALPM(1)='BLAC'
        ICALPM(2)='RED '
        ICALPM(3)='GREE'
        ICALPM(4)='BLUE'
        ICALPM(5)='MAGE'
        ICALPM(6)='ORAN'
        ICALPM(7)='CYAN'
        ICALPM(8)='YELL'
        DO3520J=9,16
        ITEMP=J-8
        ICALPM(J)=ICALPM(ITEMP)
 3520   CONTINUE
      END IF
      GOTO9000
C
 3600 CONTINUE
      ICALPF='ON'
      GOTO9000
C
 3700 CONTINUE
      ICALPF='OFF'
      GOTO9000
C
 3800 CONTINUE
      WRITE(ICOUT,3805)
 3805 FORMAT('FOR THE CALCOMP PENPLOTTER:')
      CALL DPWRST('XXX','BUG ')
      DO3810I=1,16
      WRITE(ICOUT,3811)ICALPM(I),I
      CALL DPWRST('XXX','BUG ')
 3810 CONTINUE
 3811 FORMAT('COLOR ',A8,' SET TO PEN ',I2)
      GOTO9000
C
 3910 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,3911)
 3911 FORMAT('NO COLOR SPECIFIED FOR CALCOMP PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 3920 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,3921)
 3921 FORMAT('NO INDEX SPECIFIED FOR CALCOMP PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 3930 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,3931)
 3931 FORMAT('INVALID INDEX SPECIFIED FOR CALCOMP PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C  *****************************************
C  **  REGIS CASE                         **
C  *****************************************
C
 4100 CONTINUE
C
      IF(IHARG(1).EQ.'PEN'.AND.IHARG(2).EQ.'MAP')GOTO4110
      IF(IHARG(1).EQ.'COLO'.AND.IHARG(2).EQ.'MAP')GOTO4110
      IF(IHARG(1).EQ.'PEN')GOTO4120
      IF(IHARG(1).EQ.'MAP')GOTO4120
      IF(IHARG(1).EQ.'COLO')GOTO4120
 
C
 4110 CONTINUE
      IARGCL=3
      IARGIN=4
      GOTO4190
C
 4120 CONTINUE
      IARGCL=2
      IARGIN=3
      GOTO4190
C
 4190 CONTINUE
      IFOUND='YES'
      IF(NUMARG.LT.IARGCL)GOTO4910
      IF(IHARG(IARGCL).EQ.'AUTO')GOTO4500
      IF(IHARG(IARGCL).EQ.'DEFA')GOTO4500
      IF(IHARG(IARGCL).EQ.'ON  ')GOTO4500
      IF(IHARG(IARGCL).EQ.'OFF ')GOTO9000
      IF(IHARG(IARGCL).EQ.'LIST')GOTO4800
      IF(IHARG(IARGCL).EQ.'SHOW')GOTO4800
      IF(IHARG(IARGCL).EQ.'?   ')GOTO4800
      IF(IHARG(IARGCL).EQ.'PRIN')GOTO4800
      IF(NUMARG.LT.IARGIN)GOTO4920
C
      ICOL=IHARG(IARGCL)
      INDEX=IARG(IARGIN)
      IF(INDEX.LT.1.OR.INDEX.GT.16)GOTO4930
C
C  AUGUST 1992.  FOLLOWING LIST MODIFIED TO REFLECT CURRENTLY
C  SUPPORTED NAMES AND INDICES.
C
      DO4150I=1,MAXCLR
        IF(ICOL.EQ.INAM(I)(1:4))THEN
          JINDEX=I-1
          GOTO4159
        ENDIF
 4150 CONTINUE
      IF(ICOL.EQ.'DGRY')JINDEX=14
      IF(ICOL.EQ.'LGRY')JINDEX=15
      IF(ICOL.EQ.'GREY')JINDEX=29
      IF(ICOL.EQ.'LRED')JINDEX=83
      IF(ICOL.EQ.'LMAG')JINDEX=86
      IF(ICOL.EQ.'SKYB')JINDEX=57
 4159 CONTINUE
C
C  CHECK FOR INDEX (0 THROUGH MAXCLR-1)
C
      CJUNK='    '
      DO4191I=0,9
        WRITE(CJUNK(1:1),'(I1)')I
        IF(ICOL(1:4).EQ.CJUNK(1:4))THEN
          JINDEX=I
          GOTO4194
        ENDIF
 4191 CONTINUE
 4194 CONTINUE
      CJUNK='    '
      DO4196I=10,MAXCLR-1
        WRITE(CJUNK(1:2),'(I2)')I
        IF(ICOL(1:4).EQ.CJUNK(1:4))THEN
          JINDEX=I
          GOTO4199
        ENDIF
 4196 CONTINUE
 4199 CONTINUE
C
C  CHECK FOR GREY SCALE (G0 - G100)
C
      IF(ICOL.EQ.'G0')JINDEX=1
      IF(ICOL.EQ.'G100')JINDEX=0
      IF(ICOL(1:1).EQ.'G')THEN
        CJUNK='    '
        DO4181I=1,9
          WRITE(CJUNK(1:1),'(I1)')I
          IF(ICOL(2:4).EQ.CJUNK(1:3))THEN
            JINDEX=-I
            GOTO4184
          ENDIF
 4181   CONTINUE
 4184   CONTINUE
        CJUNK='    '
        DO4186I=10,99
          WRITE(CJUNK(1:2),'(I2)')I
          IF(ICOL(2:4).EQ.CJUNK(1:3))THEN
            JINDEX=-I
            GOTO4189
           ENDIF
 4186   CONTINUE
 4189   CONTINUE
      ENDIF
      IF(JINDEX.LT.0)JINDEX=1
C
CCCCC IREGPM(INDEX)=JCOL
      IREGPM(INDEX)=JREGIS(JINDEX)
      IF(IFEEDB.EQ.'ON')WRITE(ICOUT,4490)INDEX,ICOL
 4490 FORMAT('REGIS WILL USE COLOR MAP ',I2,' FOR COLOR ',A4)
      IF(IFEEDB.EQ.'ON')CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 4500 CONTINUE
CCCCC IREGPM(1)=62
CCCCC IREGPM(2)=63
CCCCC IREGPM(3)=47
CCCCC IREGPM(4)=3
CCCCC IREGPM(5)=23
CCCCC IREGPM(6)=18
CCCCC IREGPM(7)=4
CCCCC IREGPM(8)=41
CCCCC IREGPM(9)=59
CCCCC IREGPM(10)=39
CCCCC IREGPM(11)=64
CCCCC IREGPM(12)=54
CCCCC IREGPM(13)=20
CCCCC IREGPM(14)=51
CCCCC IREGPM(15)=37
CCCCC IREGPM(16)=35
      IREGPM(1)=1
      IREGPM(2)=9
      IREGPM(3)=3
      IREGPM(4)=2
      IREGPM(5)=5
      IREGPM(6)=8
      IREGPM(7)=4
      IREGPM(8)=7
      IREGPM(9)=64
      IREGPM(10)=6
      IREGPM(11)=10
      IREGPM(12)=62
      IREGPM(13)=28
      IREGPM(14)=14
      IREGPM(15)=16
      IREGPM(16)=15
      IF(IFEEDB.EQ.'OFF')GOTO4519
      WRITE(ICOUT,4506)
 4506 FORMAT('FOR REGIS, THE DEFAULT COLORS ARE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4507)
 4507 FORMAT('COLOR MAP COLOR NAME')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4508)
 4508 FORMAT('========= ==========')
      CALL DPWRST('XXX','BUG ')
      DO4510I=1,IREGMC
CCCCC   WRITE(ICOUT,4511)I,IRGCLR(IREGPM(I))
CCCCC CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4511)I,ICLR(IREGPM(I))
      CALL DPWRST('XXX','BUG ')
 4510 CONTINUE
 4511 FORMAT(I2,8X,A25)
 4519 CONTINUE
      GOTO9000
C
 4800 CONTINUE
      IDEV='REGI'
      IDEV2='    '
      GOTO6200
C
 4910 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,4911)
 4911 FORMAT('NO COLOR SPECIFIED FOR REGIS PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 4920 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,4921)
 4921 FORMAT('NO INDEX SPECIFIED FOR REGIS PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 4930 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,4931)
 4931 FORMAT('INVALID INDEX SPECIFIED FOR REGIS PEN MAP COMMAND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C  *****************************************
C  **  X11 CASE                           **
C  **  CURRENTLY ONLY SUPPORT             **
C  **    X11 COLORS SHOW (OR SIMILIAR)    **
C  *****************************************
C
 5100 CONTINUE
C
      IF(IHARG(1).EQ.'PEN'.AND.IHARG(2).EQ.'MAP')GOTO5110
      IF(IHARG(1).EQ.'COLO'.AND.IHARG(2).EQ.'MAP')GOTO5110
      IF(IHARG(1).EQ.'PEN')GOTO5120
      IF(IHARG(1).EQ.'MAP')GOTO5120
      IF(IHARG(1).EQ.'COLO')GOTO5120
 
C
 5110 CONTINUE
      IARCL=3
      GOTO5190
C
 5120 CONTINUE
      IARGCL=2
      GOTO5190
C
 5190 CONTINUE
      IFOUND='YES'
      IF(IHARG(IARGCL).EQ.'AUTO')GOTO5800
      IF(IHARG(IARGCL).EQ.'DEFA')GOTO5800
      IF(IHARG(IARGCL).EQ.'ON  ')GOTO5800
      IF(IHARG(IARGCL).EQ.'OFF ')GOTO9000
      IF(IHARG(IARGCL).EQ.'LIST')GOTO5800
      IF(IHARG(IARGCL).EQ.'SHOW')GOTO5800
      IF(IHARG(IARGCL).EQ.'?   ')GOTO5800
      IF(IHARG(IARGCL).EQ.'PRIN')GOTO5800
      GOTO5800
C
 5800 CONTINUE
CCCCC THE FOLLOWING FORMAT STATEMENT WAS SPLIT   SEPTEMBER 1993
CCCCC WRITE(ICOUT,5805)
      WRITE(ICOUT,5801)
 5801 FORMAT('COLORS FOR THE X11 TERMINAL:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5802)
 5802 FORMAT('COLOR',24X,'NAME',6X,'INDEX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5803)
 5803 FORMAT('=====',24X,'====',6X,'=====')
      CALL DPWRST('XXX','BUG ')
      IHELMX=24
      NCPREH=0
      ICPREH=' '
      IRESP='YES'
      IBUGO2='OFF'
      NUMLPR=4
      DO5810I=1,MAXCLR
        NUMLPR=NUMLPR+1
        IF(NUMLPR.GE.IHELMX)THEN
          CALL DPMORE(NUMLPR,NCPREH,ICPREH,IRESP,IBUGO2,IERROR)
          NUMLPR=0
          IF(IRESP.EQ.'NO')GOTO9000
        END IF
        WRITE(ICOUT,5811)ICLR(I),INAM(I),I
        CALL DPWRST('XXX','BUG ')
 5810 CONTINUE
 5811 FORMAT(A25,5X,A8,2X,I2)
      GOTO9000
C
C  *****************************************
C  **  SHOW COLORS CASE                   **
C  *****************************************
C
 6100 CONTINUE
C
      IF(IHARG(1).EQ.'COLO'.AND.NUMARG.LE.1)GOTO6800
      IF(IHARG(1).EQ.'COLO'.AND.NUMARG.GE.2)GOTO6130
      IF(IHARG(2).EQ.'COLO')GOTO6140
      IF(IHARG(3).EQ.'COLO')GOTO6150
      GOTO9000
C
 6130 CONTINUE
      IDEV=IHARG(2)
      IDEV2=IHARG(3)
      GOTO6200
C
 6140 CONTINUE
      IDEV=IHARG(1)
      IDEV2=' '
      GOTO6200
C
 6150 CONTINUE
      IDEV=IHARG(1)
      IDEV2=IHARG(2)
      GOTO6200
C
 6200 CONTINUE
      IFOUND='YES'
      IGRAY='NO'
      IF((IDEV.EQ.'TEKT'.AND.IDEV2.EQ.'4027'))THEN
        DO6201I=1,MAXCLR
        JTEMP(I)=J4027(I)
        JINDEX=2
        IF(JTEMP(I).EQ.0)JINDEX=1
        IF(JTEMP(I).EQ.1)JINDEX=3
        IF(JTEMP(I).EQ.2)JINDEX=5
        IF(JTEMP(I).EQ.3)JINDEX=4
        IF(JTEMP(I).EQ.4)JINDEX=9
        IF(JTEMP(I).EQ.5)JINDEX=7
        IF(JTEMP(I).EQ.6)JINDEX=54
        IF(JTEMP(I).EQ.7)JINDEX=2
        JTEMP(I)=JINDEX
 6201   CONTINUE
      ELSEIF((IDEV.EQ.'TEKT'.AND.IDEV2.EQ.'4105').OR.
     1       (IDEV.EQ.'TEKT'.AND.IDEV2.EQ.'4113').OR.
     1       (IDEV.EQ.'TEKT'.AND.IDEV2.EQ.'4115').OR.
     1       (IDEV.EQ.'GENE'))THEN
        DO6202I=1,MAXCLR
        JTEMP(I)=J4105(I)
        JINDEX=2
        IF(JTEMP(I).EQ.0)JINDEX=2
        IF(JTEMP(I).EQ.1)JINDEX=1
        IF(JTEMP(I).EQ.2)JINDEX=3
        IF(JTEMP(I).EQ.3)JINDEX=5
        IF(JTEMP(I).EQ.4)JINDEX=4
        IF(JTEMP(I).EQ.5)JINDEX=8
        IF(JTEMP(I).EQ.6)JINDEX=6
        IF(JTEMP(I).EQ.7)JINDEX=9
        JTEMP(I)=JINDEX
 6202   CONTINUE
      ELSEIF((IDEV(1:2).EQ.'HP'.AND.IDEV2.EQ.'2622').OR.
     1       (IDEV(1:2).EQ.'HP'.AND.IDEV2.EQ.'2623').OR.
     1       (IDEV(1:2).EQ.'HP'.AND.IDEV2.EQ.'2627').OR.
     1       (IDEV(1:2).EQ.'HP'.AND.IDEV2.EQ.'2647'))THEN
        DO6205I=1,MAXCLR
        JTEMP(I)=J2622(I)
        JINDEX=2
        IF(JTEMP(I).EQ.0)JINDEX=2
        IF(JTEMP(I).EQ.1)JINDEX=3
        IF(JTEMP(I).EQ.2)JINDEX=5
        IF(JTEMP(I).EQ.3)JINDEX=9
        IF(JTEMP(I).EQ.4)JINDEX=4
        IF(JTEMP(I).EQ.5)JINDEX=6
        IF(JTEMP(I).EQ.6)JINDEX=8
        IF(JTEMP(I).EQ.7)JINDEX=1
        JTEMP(I)=JINDEX
 6205   CONTINUE
      ELSEIF((IDEV.EQ.'TEKT'.AND.IDEV2.EQ.'4662').OR.
     1       (IDEV.EQ.'CALC'.AND.ICALCL.LE.4).OR.
     1       (IDEV.EQ.'ZETA'.AND.IZETCL.LE.4).OR.
     1       (IDEV(1:2).EQ.'HP'.AND.IDEV2.EQ.'7221').OR.
     1       (IDEV(1:2).EQ.'HP'.AND.IHPGCL.LE.4))THEN
        DO6203I=1,MAXCLR
        JTEMP(I)=JPLOT4(I)
        JINDEX=1
        IF(JTEMP(I).EQ.1)JINDEX=2
        IF(JTEMP(I).EQ.2)JINDEX=3
        IF(JTEMP(I).EQ.3)JINDEX=4
        IF(JTEMP(I).EQ.4)JINDEX=5
        JTEMP(I)=JINDEX
 6203   CONTINUE
      ELSEIF(
     1       (IDEV.EQ.'CALC'.AND.ICALCL.GT.4).OR.
     1       (IDEV.EQ.'ZETA'.AND.IZETCL.GT.4).OR.
     1       (IDEV(1:2).EQ.'HP'.AND.IHPGCL.GT.4))THEN
        DO6204I=1,MAXCLR
        JTEMP(I)=JPLOT8(I)
        JINDEX=2
        IF(JTEMP(I).EQ.1)JINDEX=2
        IF(JTEMP(I).EQ.2)JINDEX=3
        IF(JTEMP(I).EQ.3)JINDEX=4
        IF(JTEMP(I).EQ.4)JINDEX=5
        IF(JTEMP(I).EQ.5)JINDEX=6
        IF(JTEMP(I).EQ.6)JINDEX=7
        IF(JTEMP(I).EQ.7)JINDEX=8
        IF(JTEMP(I).EQ.8)JINDEX=9
        JTEMP(I)=JINDEX
 6204   CONTINUE
      ELSEIF((IDEV.EQ.'CGM').OR.
     1       (IDEV.EQ.'POST'))THEN
        DO6206I=1,MAXCLR
        JTEMP(I)=JCGM(I)
 6206   CONTINUE
        IF(IDEV.EQ.'POST')IGRAY='YES'
      ELSEIF((IDEV.EQ.'SUN '))THEN
        DO6207I=1,MAXCLR
        JTEMP(I)=JSUN(I)
        JINDEX=2
        IF(JTEMP(I).EQ.0)JINDEX=2
        IF(JTEMP(I).EQ.1)JINDEX=3
        IF(JTEMP(I).EQ.2)JINDEX=5
        IF(JTEMP(I).EQ.3)JINDEX=4
        IF(JTEMP(I).EQ.4)JINDEX=9
        IF(JTEMP(I).EQ.5)JINDEX=2
        IF(JTEMP(I).EQ.6)JINDEX=6
        IF(JTEMP(I).EQ.7)JINDEX=1
        JTEMP(I)=JINDEX
 6207   CONTINUE
      ELSEIF((IDEV.EQ.'REGI'))THEN
        DO6208I=1,MAXCLR
        JTEMP(I)=JREGIS(I)
        JINDEX=JREG2(JTEMP(I))
        JTEMP(I)=JINDEX
 6208   CONTINUE
      ELSEIF((IDEV.EQ.'X11'))THEN
        DO6209I=1,MAXCLR
        JTEMP(I)=JCGM(I)
 6209   CONTINUE
        IGRAY='YES'
      ELSEIF(IDEV.EQ.'PC'.OR.
     1       IDEV.EQ.'IBM'.OR.
     1       IDEV.EQ.'TURB'.OR.
     1       IDEV.EQ.'VGA')THEN
        DO6210I=1,MAXCLR
        JTEMP(I)=JPC(I)
        JINDEX=2
        IF(JTEMP(I).EQ.0)JINDEX=2
        IF(JTEMP(I).EQ.1)JINDEX=4
        IF(JTEMP(I).EQ.2)JINDEX=5
        IF(JTEMP(I).EQ.3)JINDEX=8
        IF(JTEMP(I).EQ.4)JINDEX=3
        IF(JTEMP(I).EQ.5)JINDEX=6
        IF(JTEMP(I).EQ.6)JINDEX=18
        IF(JTEMP(I).EQ.7)JINDEX=30
        IF(JTEMP(I).EQ.8)JINDEX=15
        IF(JTEMP(I).EQ.9)JINDEX=12
        IF(JTEMP(I).EQ.10)JINDEX=16
        IF(JTEMP(I).EQ.11)JINDEX=68
        IF(JTEMP(I).EQ.12)JINDEX=84
        IF(JTEMP(I).EQ.13)JINDEX=87
        IF(JTEMP(I).EQ.14)JINDEX=9
        IF(JTEMP(I).EQ.15)JINDEX=1
        JTEMP(I)=JINDEX
 6210   CONTINUE
      ELSE
        WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6221)IDEV,IDEV2
 6221 FORMAT('DEVICE ',A4,1X,A4,' NOT RECOGNIZED')
      CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6255)IDEV,IDEV2
 6255 FORMAT('THE FOLLOWING SHOWS THE COLOR MAPPING FOR DEVICE: ',
     11X,A4,1X,A4)
      CALL DPWRST('XXX','BUG ')
      IF(IGRAY.EQ.'YES')THEN
         WRITE(ICOUT,6261)
 6261    FORMAT('THIS DEVICE SUPPORTS GRAY SCALE')
         CALL DPWRST('XXX','BUG ')
      ELSE
         WRITE(ICOUT,6262)
 6262    FORMAT('THIS DEVICE DOES NOT SUPPORT GRAY SCALE')
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC THE FOLLOWING FORMAT WAS SPLIT   SEPTEMBER 1993
CCCCC WRITE(ICOUT,6270)
      WRITE(ICOUT,6271)
 6271 FORMAT(5X,24X,'DATAPLOT',2X,'DATAPLOT',2X,'DEVICE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6272)
 6272 FORMAT('COLOR',24X,'NAME',6X,'INDEX',5X,'COLOR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6273)
 6273 FORMAT('=====',24X,'========',2X,'========',2X,'======')
      CALL DPWRST('XXX','BUG ')
C
      IHELMX=24
      NCPREH=0
      ICPREH=' '
      IRESP='YES'
      IBUGO2='OFF'
      NUMLPR=6
      DO6280I=1,MAXCLR
        NUMLPR=NUMLPR+1
        IF(NUMLPR.GE.IHELMX)THEN
          CALL DPMORE(NUMLPR,NCPREH,ICPREH,IRESP,IBUGO2,IERROR)
          NUMLPR=0
          IF(IRESP.EQ.'NO')GOTO9000
        END IF
        WRITE(ICOUT,6281)ICLR(I),INAM(I),I-1,INAM(JTEMP(I))
      CALL DPWRST('XXX','BUG ')
 6280 CONTINUE
 6281 FORMAT(A24,5X,A8,2X,I2,8X,A8)
      GOTO9000
C
 6800 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
CCCCC THE FOLLOWING SECTION STATMENT WAS CHANGED   SEPTEMBER 1993
CCCCC WRITE(ICOUT,6805)
      WRITE(ICOUT,6801)
 6801 FORMAT('THE FOLLOWING IS A LIST OF COLORS THAT DATAPLOT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6802)
 6802 FORMAT('CURRENTLY RECOGNIZES.  ALL DEVICES RECOGNIZE THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6803)
 6803 FORMAT('SAME COLOR NAMES.  HOWEVER, MOST DEVICES ONLY SUPPORT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6804)
 6804 FORMAT('A SUBSET OF THIS LIST.  AN UNSUPPORTED COLOR WILL BE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6805)
 6805 FORMAT('MAPPED TO A SUPPORTED COLOR.  IN ADDITION, GRAY SCALE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6806)
 6806 FORMAT('CAN BE REQUESTED BY USING G0 (BLACK) THROUGH G100')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6807)
 6807 FORMAT('(WHITE).  HOWEVER, ONLY A FEW DEVICES ACTUALLY SUPPORT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6808)
 6808 FORMAT('GRAY SCALE (POSTSCRIPT, X11).  OTHER DEVICES WILL MAP')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6809)
 6809 FORMAT('ALL GRAY SCALES TO EITHER BLACK OR WHITE.')
      CALL DPWRST('XXX','BUG ')
C
C     THE FOLLOWING FORMAT WAS SPLIT   SEPTEMBER 1993
CCCCC WRITE(ICOUT,6811)
      WRITE(ICOUT,6811)
 6811 FORMAT('SUPPORTED COLORS:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6812)
 6812 FORMAT('COLOR',24X,'NAME',6X,'INDEX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6813)
 6813 FORMAT('=====',24X,'====',6X,'=====')
      CALL DPWRST('XXX','BUG ')
C
      IHELMX=24
      NCPREH=0
      ICPREH=' '
      IRESP='YES'
      IBUGO2='OFF'
      NUMLPR=13
      DO6820I=1,MAXCLR
         NUMLPR=NUMLPR+1
         IF(NUMLPR.GE.IHELMX)THEN
            CALL DPMORE(NUMLPR,NCPREH,ICPREH,IRESP,IBUGO2,IERROR)
            NUMLPR=0
            IF(IRESP.EQ.'NO')GOTO9000
         END IF
         WRITE(ICOUT,6821)ICLR(I),INAM(I),I-1
 6821    FORMAT(A24,5X,A8,2X,I2)
         CALL DPWRST('XXX','BUG ')
 6820 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGO2.EQ.'OFF'.AND.ISUBRO.NE.'DEPM')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DPDEPM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGO2
 9013 FORMAT('IBUGO2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IBUGG4,ISUBG4
 9014 FORMAT('IBUGG4,ISUBG4 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)IFOUND,IERROR
 9020 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9028)NUMARG
 9028 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9030I=1,NUMARG
      WRITE(ICOUT,9031)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I)
 9031 FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,2X,I8)
      CALL DPWRST('XXX','BUG ')
 9030 CONTINUE
      WRITE(ICOUT,9032)IHPGPF
 9032 FORMAT('IHPGPF=',A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDEP2(IOPERA,IGENNU,IGENID,IGDFLG,ICAFLG,
     1                  ICAPSW,
     1                  IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--TURN ON OR TURN OFF (DEPENDING ON THE CONTENTS OF IOPERA)
C              A DEVICE BY CARRYING OUT APPROPRIATE (OPEN OR CLOSE) FILE
C              OPERATIONS.  THIS IS USED FOR TURNING ON OR OFF
C              ALTERNATE PLOTTING DEVICES.
C     INPUT  ARGUMENTS--IOPERA (A CHARACTER VARIABLE
C                              DESCRIBING THE DESIRED OPERATION
C                              (OPEN OR CLOSE)
C                     --IGENNU (AN INTEGER VALUE
C                              BY WHICH THE PLOT  FILE/SUBFILE
C                              MAY BE REFERENCED IN A FORTRAN
C                              I/O STATEMENT.
C                     --IGENID (A CHARACTER VARIABLE
C                              CONTAINING IDENTIFICATION INFORMATION
C                              FOR THE PLOT  FILE/SUBFILE
C                             (E.G., PLO1, PLO2, GENE, ETC.)
C                     --IANS   (A  CHARACTER VECTOR)
C                     --IWIDTH (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IFOUND ('YES' OR 'NO' )
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
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--JUNE      1978.
C     UPDATED         --APRIL     1979.
C     UPDATED         --OCTOBER   1980.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1986.
C     UPDATED         --MAY       1988.
C     UPDATED         --MAY       1990. REACTIVE "CLOSE" CODE
C     UPDATED         --APRIL     1992. FIX BUG FOR "CLOSE"
C                                       CALL GREXIT IF "CLOSE"
C     UPDATED         --MAY       1992. IFOUND NO --> YES
C     UPDATED         --MAY       1992. ADD DEBUG STATEMENTS
C     UPDATED         --MAY       1992. FIX IPL1CS,IPL2CS
C     UPDATED         --FEBRUARY  2001. IGDFLG
C     UPDATED         --SEPTEMBER 2002. HTML CAPTURE FOR GD AND
C                                       SVG DEVICES
C     UPDATED         --JANUARY   2003. HTML CAPTURE FOR POSTSCRIPT
C     UPDATED         --JANUARY   2003. SUPPORT FOR IPSTDV (CONVERT
C                                       POSTSCRIPT OUTPUT TO: JPEG,
C                                       PDF, TIFF, PBM USING
C                                       GHOSTSCRIPT
C     UPDATED         --SEPTEMBER 2003. LATEX CAPTURE FOR POSTSCRIPT
C     UPDATED         --FEBRUARY  2006. CALL GREXIT FOR DEVICE 1
C     UPDATED         --MARCH     2006. CHECK IF ANOTHER PROCESS
C                                       MIGHT HAVE PLOT FILE LOCKED.
C     UPDATED         --JUNE      2008. WHEN GENERATING <IMG SRC=,
C                                       PUT QUOTE AT END OF FILE
C                                       (PROBLEM WITH IE 7)
C     UPDATED         --MARCH     2009. FOR CONVERT PROGRAM, ALLOW
C                                       USER-SPECIFIED DENSITY
C     UPDATED         --SEPTEMBER 2010. UPDATE HTML CAPTURE FOR SVG
C     UPDATED         --DECEMBER  2013. CHECK FOR 32-BIT OR 64-BIT
C                                       VERSION OF GHOSTSCRIPT ON
C                                       WINDOWS PLATFORMS
C     UPDATED         --JULY      2015. CHECK FOR SVG "BACKUP FILE"
C     UPDATED         --DECEMBER  2015. SUPPORT "SET DEVICE 3 NAME
C                                       COUNTER ON" COMMAND
C     UPDATED         --DECEMBER  2015. SUPPORT "SET DEVICE 2 SPLIT ON"
C     UPDATED         --NOVEMBER  2016. ICAFLG ADDED
C     UPDATED         --MARCH     2019. SET SYSTEM PERSIST
C                                       SET SYSTEM HIDDEN
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOPERA
      CHARACTER*4 IGENID
      CHARACTER*4 IGDFLG
      CHARACTER*4 ICAFLG
      CHARACTER*4 IANS
      CHARACTER*4 IBUGO2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICAPSW
C
      CHARACTER*80 IFILE
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*256 ISTRIN
      CHARACTER*256 ISTRI2
C
      CHARACTER*80 IFIL2
      CHARACTER*80 IFILZ
      CHARACTER*1 IQUOTE
      CHARACTER*1 IBASLC
      CHARACTER*1 IFOSLC
      CHARACTER*1 IATEMP
      CHARACTER*4 IEXIST
      CHARACTER*12 IFWRIT
      CHARACTER*16 IFORMT
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
C
      DIMENSION IANS(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
CCCCC FOLLOWING THREE INCLUDE FILES NEEDED TO CALL GREXIT
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOGR.INC'
C
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
      LOGICAL IOPPLO
      COMMON/OPNPLT/IOPPLO
C
      CHARACTER*80 IFILSV
      COMMON/ICFILE/IFILSV,NCSAVE
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
CCCCC THE FOLLOWING LINE WAS FIXED                MAY 1992 (JJF)
CCCCC AS PART OF FIX FOR   PRINT PLOT   COMMAND   MAY 1992 (JJF)
CCCCC IFOUND='NO'
      IFOUND='YES'
      IERROR='NO'
      ISUBN1='DPDE'
      ISUBN2='P2  '
      IFILSV=' '
      NCSAVE=0
      IFLGSP=0
C
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IMANUF,IMODEL,ICAPSW,ICAPTY
   52   FORMAT('IMANUF,IMODEL,ICAPSW,ICAPTY = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGO2,IERROR,IDEV,IOPERA,IGENID
   53   FORMAT('IBUGO2,IERROR,IDEV,IOPERA,IGENID = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IGENNU,IPL1NU,IPL2NU,IWIDTH
   54   FORMAT('IGENNU,IPL1NU,IPL2NU,IWIDTH = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)(IANS(I),I=1,MIN(IWIDTH,120))
   55   FORMAT('IANS(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)IPL1NA
   62   FORMAT('IPL1NA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IPL1ST,IPL1FO,IPL1AC,IPL1FO,IPL1CS
   63   FORMAT('IPL1ST,IPL1FO,IPL1AC,IPL1FO,IPL1CS = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IPL2NA
   72   FORMAT('IPL2NA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)IPL2ST,IPL2FO,IPL2AC,IPL2FO,IPL2CS
   73   FORMAT('IPL2ST,IPL2FO,IPL2AC,IPL2FO,IPL2CS = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IGENID.EQ.'PLO1')THEN
C
C       2015/12: IF "SET DEVICE 2 SPLIT ON" COMMAND GIVEN, THEN
C                ADD A COUNTER TO THE NAME TO SAVE INDIVIDUAL FILES.
C
C       2019/3: CHECK FOR SPACE IN FILE NAME
C
        IOUNIT=IPL1NU
        IFILE=IPL1NA
C
        IF(IOPERA.EQ.'OPEN' .AND. IDV2SP.EQ.'ON')THEN
C
C         STEP 1: FIND LOCATION OF "." IN FILE NAME
C
          ICNTD2=ICNTD2+1
          IPOS=0
          DO1111JJ=1,80
            IF(IFILE(JJ:JJ).EQ.'.')THEN
              IPOS=JJ
              GOTO1112
            ENDIF
 1111     CONTINUE
 1112     CONTINUE
C
C         STEP 2: ADD COUNTER TO FILE NAME
C
          IF(IPOS.GT.0)THEN
            IF(ICNTD2.GE.1000 .AND. ICNTD2.LE.9999)THEN
              DO1113JJ=75,IPOS,-1
                IFILE(JJ+5:JJ+5)=IFILE(JJ:JJ)
 1113         CONTINUE
              IFILE(IPOS:IPOS)='_'
              WRITE(IFILE(IPOS+1:IPOS+4),'(I4)')ICNTD2
            ELSEIF(ICNTD2.GE.100 .AND. ICNTD2.LE.999)THEN
              DO1114JJ=76,IPOS,-1
                IFILE(II+4:II+4)=IFILE(II:II)
 1114         CONTINUE
              IFILE(IPOS:IPOS)='_'
              WRITE(IFILE(IPOS+1:IPOS+3),'(I3)')ICNTD2
            ELSEIF(ICNTD2.GE.10 .AND. ICNTD2.LE.99)THEN
              DO1115II=77,IPOS,-1
                IFILE(II+3:II+3)=IFILE(II:II)
 1115         CONTINUE
              IFILE(IPOS:IPOS)='_'
              WRITE(IFILE(IPOS+1:IPOS+2),'(I2)')ICNTD2
            ELSEIF(ICNTD2.GE.1 .AND. ICNTD2.LE.9)THEN
              DO1116II=78,IPOS,-1
                IFILE(II+2:II+2)=IFILE(II:II)
 1116         CONTINUE
              IFILE(IPOS:IPOS)='_'
              WRITE(IFILE(IPOS+1:IPOS+1),'(I1)')ICNTD2
            ENDIF
          ENDIF
        ENDIF
C
        ISTAT=IPL1ST
        IFORM=IPL1FO
        IACCES=IPL1AC
        IPROT=IPL1PR
        ICURST=IPL1CS
        ISUBN0='DEP2'
        IERRFI='NO'
        GOTO1190
      ELSEIF(IGENID.EQ.'SCRE')THEN
        GOTO1290
      ENDIF
C
C     2015/12: IF "SET DEVICE 3 NAME COUNTER ON" COMMAND GIVEN,
C              THEN ADD A COUNTER TO THE NAME TO SAVE INDIVIDUAL
C              FILES.
C
      IOUNIT=IPL2NU
      IFILE=IPL2NA
C
      IF(IOPERA.EQ.'OPEN' .AND. IDV3NC.EQ.'ON')THEN
C
C       STEP 1: FIND LOCATION OF "." IN FILE NAME
C
        ICNTD3=ICNTD3+1
        IPOS=0
        DO1121II=1,80
          IF(IFILE(II:II).EQ.'.')THEN
            IPOS=II
            GOTO1122
          ENDIF
 1121   CONTINUE
 1122   CONTINUE
C
C       STEP 2: ADD COUNTER TO FILE NAME
C
        IF(IPOS.GT.0)THEN
          IF(ICNTD3.GE.1000 .AND. ICNTD3.LE.9999)THEN
            DO1123II=75,IPOS,-1
              IFILE(II+5:II+5)=IFILE(II:II)
 1123       CONTINUE
            IFILE(IPOS:IPOS)='_'
            WRITE(IFILE(IPOS+1:IPOS+4),'(I4)')ICNTD3
          ELSEIF(ICNTD3.GE.100 .AND. ICNTD3.LE.999)THEN
            DO1124II=76,IPOS,-1
              IFILE(II+4:II+4)=IFILE(II:II)
 1124       CONTINUE
            IFILE(IPOS:IPOS)='_'
            WRITE(IFILE(IPOS+1:IPOS+3),'(I3)')ICNTD3
          ELSEIF(ICNTD3.GE.10 .AND. ICNTD3.LE.99)THEN
            DO1125II=77,IPOS,-1
              IFILE(II+3:II+3)=IFILE(II:II)
 1125       CONTINUE
            IFILE(IPOS:IPOS)='_'
            WRITE(IFILE(IPOS+1:IPOS+2),'(I2)')ICNTD3
          ELSEIF(ICNTD3.GE.1 .AND. ICNTD3.LE.9)THEN
            DO1126II=78,IPOS,-1
              IFILE(II+2:II+2)=IFILE(II:II)
 1126       CONTINUE
            IFILE(IPOS:IPOS)='_'
            WRITE(IFILE(IPOS+1:IPOS+1),'(I1)')ICNTD3
          ENDIF
        ENDIF
      ENDIF
C
      ISTAT=IPL2ST
      IFORM=IPL2FO
      IACCES=IPL2AC
      IPROT=IPL2PR
      ICURST=IPL2CS
      ISUBN0='DEP2'
      IERRFI='NO'
      GOTO1190
C
 1190 CONTINUE
C
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP2')THEN
        WRITE(ICOUT,1193)ISUBN0,IERRFI,IOUNIT
 1193   FORMAT('ISUBN0,IERRFI,IOUNIT = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1194)IFILE
 1194   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 12--                            **
C               **  CHECK TO SEE IF PLOT FILE MAY EXIST  **
C               *******************************************
C
      ISTEPN='12'
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPDEP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      THE DESIRED PLOTS CANNOT BE WRITTEN TO FILE ',
     1         'BECAUSE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)
 1214   FORMAT('      REQUIRED SYSTEM MASS STORAGE FILE WHICH STORES ',
     1         'SUCH ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)
 1216   FORMAT('      PLOTS IS NOT AVAILABLE AT THIS INSTALLATION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)ISTAT,IPL1ST,IPL2ST
 1217   FORMAT('ISTAT,IPL1ST,IPL2ST = ',2(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
 1290 CONTINUE
C
C               *****************************************
C               **  STEP 20--                          **
C               **  BRANCH TO THE APPROPRIATE CASE--   **
C               **    1) OPEN  THE PLOT FILE;          **
C               **    2) CLOSE THE PLOT FILE.          **
C               *****************************************
C
      ISTEPN='20'
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOPERA.EQ.'ON  ' .OR. IOPERA.EQ.'OPEN')THEN
C
C               ******************************************
C               **  STEP 21--                           **
C               **  OPEN THE PLOT FILE.                 **
C               **  PRIOR VERSIONS OF DATAPLOT HAD      **
C               **  OPEN, BUT NO REWIND                 **
C               ******************************************
C
C       NOTE: MARCH 2006.  ADD A CALL TO DPINF2 TO CHECK IF FILE
C             CAN BE OPENED IN "WRITE" MODE.  UNDER WINDOWS (INTEL
C             COMPILER), ANOTHER DATAPLOT PROCESS CAN HAVE A LOCK
C             ON THE PLOT FILE WHICH CAN CAUSE THE CURRENT SESSION
C             TO HANG.
C
C        NOTE: UNFORTUNETELY, DPINQF2 DOESN'T QUITE DO THE TRICK
C              (IT SIMPLY RETURNS AN "UNKNOWN" STATUS REGARDLESS
C              OF WHETHER A PREVIOUS PROCESS WAS RUNNING OR NOT).
C              FOR WINDOWS, WE REDEFINED THE "PROTECTION" OPTION
C              TO BE "WRITE" FOR THE PLOT FILES.  THIS WILL CAUSE
C              THE OPEN TO FAIL IF ANOTHER PROCESS HAS THE PLOT
C              FILE LOCKED.  ONE ADDITIONAL COMPLICATION IS THAT
C              DATAPLOT WILL TRY TO OPEN THE FILE IN THE DATAPLOT
C              DIRECTORIES IF THE INITIAL OPEN FAILS.  FOR THIS
C              REASON, WE WILL SET A FLAG IN A COMMON BLOCK SO
C              THAT DPOPFI WILL KNOW NOT TO TRY AND OPEN THE
C              FILE IN THE DATAPLOT SUB-DIRECTORIES.
C
        ISTEPN='21'
        IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP2')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IDEV=IGENID
C
C       MAY, 1988.  DON'T OPEN IF ALREADY OPEN
C
        IF(IDEV.EQ.'PLO1'.AND.IPL1CS.EQ.'OPEN')GOTO2199
        IF(IDEV.EQ.'PLO2'.AND.IPL2CS.EQ.'OPEN')GOTO2199
        IF(IDEV.EQ.'SCRE')GOTO2199
C
        IF(IGDFLG.EQ.'ON' .OR. ICAFLG.EQ.'ON')GOTO2198
C
        IFGPID=0
 2109   CONTINUE
        IOPPLO=.TRUE.
        IREWIN='OFF'
        CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1              IREWIN,ISUBN0,IERRFI,IBUGO2,ISUBRO,IERROR)
        IOPPLO=.FALSE.
        IF(IERROR.EQ.'YES')THEN
          IF(IGENID.EQ.'PLO1')IPL1CS='CLOSED'
          IF(IGENID.EQ.'PLO2')IPL2CS='CLOSED'
C
          IF(IFGPID.EQ.1)THEN
            WRITE(ICOUT,2181)
 2181       FORMAT('***** WARNING: DATAPLOT STILL UNABLE TO OPEN ',
     1             'THE PLOT FILE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2183)
 2183       FORMAT('      THE PLOT FILE WILL NOT BE GENERATED.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
          IFGPID=1
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2111)
 2111     FORMAT('***** WARNING: DATAPLOT UNABLE TO OPEN THE PLOT FILE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2113)IFILE
 2113     FORMAT('               ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2115)
 2115     FORMAT('      IN WRITE MODE.  LIKELY CAUSES ARE:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2117)
 2117     FORMAT('      1. YOU ARE TRYING TO OPEN THE FILE IN A ',
     1           'READ ONLY DIRECTORY.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2119)
 2119     FORMAT('      2. ANOTHER DATAPLOT PROCESS IS ACTIVE AND ',
     1           'HAS A LOCK ON THE FILE.')
          CALL DPWRST('XXX','BUG ')
          IF(IHOST1.EQ.'IBM-' .AND. ICOMPI.EQ.'MS-F')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2121)
 2121       FORMAT('      3. ON THE WINDOWS PLATFORM, IF A PREVIOUS ',
     1             'GUI SESSION DID NOT CLOSE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2123)
 2123       FORMAT('         CLEANLY, THE UNDERLYING DATAPLOT ',
     1             'EXECUTABLE MAY STILL BE RUNNING')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2125)
 2125       FORMAT('         AND HAVE A LOCK ON THE FILE.  TO CLEAR ',
     1             'THESE, AFTER EXITING THE CURRENT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2127)
 2127       FORMAT('         ENTER "CNTRL-ALT-DEL" TO BRING UP THE ',
     1             'TASK MANAGER AND THEN SELECT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2129)
 2129       FORMAT('         "PROCESSES".  DELETE ANY OCCURENCES OF ',
     1             '"DPLAHEY.EXE".')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
C
          IF(ITMPFI.EQ.'PID')THEN
            IFGPID=1
            WRITE(ICOUT,2131)
 2131       FORMAT('      DATAPLOT WILL APPEND THE PROCESS-ID TO THE ',
     1             'FILE NAME AND TRY AGAIN.')
            CALL DPWRST('XXX','BUG ')
            CALL DPPID2(IPID,ISUBRO,IERROR)
            IF(IPID.LE.0)THEN
              IERROR='YES'
              GOTO9000
            ELSE
 2139         CONTINUE
              IF(IPID.LT.10)THEN
                NCH=1
              ELSEIF(IPID.LT.100)THEN
                NCH=2
              ELSEIF(IPID.LT.1000)THEN
                NCH=3
              ELSEIF(IPID.LT.10000)THEN
                NCH=4
              ELSEIF(IPID.LT.100000)THEN
                NCH=5
              ELSEIF(IPID.LT.1000000)THEN
                NCH=6
              ELSE
                IPID=IPID/10
                GOTO2139
              ENDIF
              IFORMT=' '
              IFORMT='(I )'
              WRITE(IFORMT(3:3),'(I1)')NCH
            ENDIF
            IF(IGENID.EQ.'PLO1')THEN
              NLAST=80-NCH-1
              DO2141I=72,1,-1
                NLAST=I
                IF(IPL1NA(I:I).NE.' ')GOTO2149
 2141         CONTINUE
 2149         CONTINUE
              IPL1NA(NLAST+1:NLAST+1)='.'
              NLAST=NLAST+1
              WRITE(IPL1NA(NLAST+1:NLAST+NCH),IFORMT)IPID
              IFILE(1:80)=IPL1NA(1:80)
            ELSEIF(IGENID.EQ.'PLO2')THEN
              NLAST=80-NCH-1
              DO2151I=72,1,-1
                NLAST=I
                IF(IPL2NA(I:I).NE.' ')GOTO2159
 2151         CONTINUE
 2159         CONTINUE
              IPL2NA(NLAST+1:NLAST+1)='.'
              WRITE(IPL2NA(NLAST+2:NLAST+NCH+1),IFORMT)IPID
              IFILE(1:80)=IPL2NA(1:80)
            ENDIF
            GOTO2109
          ELSE
            IERROR='YES'
            GOTO9000
          ENDIF
        ENDIF
C
        IF(IFGPID.EQ.1)THEN
          WRITE(ICOUT,2161)
 2161     FORMAT('      PLOT FILE OPENED AS:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2163)IFILE
 2163     FORMAT('      ',A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 2198   CONTINUE
        IF(IGENID.EQ.'PLO1')IPL1CS='OPEN'
        IF(IGENID.EQ.'PLO2')IPL2CS='OPEN'
C
 2199   CONTINUE
        GOTO9000
      ELSE
C
C               ******************************************
C               **  STEP 22--                           **
C               **  CLOSE THE PLOT FILE.                **
C               **  PRIOR VERSIONS OF DATAPLOT HAD      **
C               **  ENDFILE, BUT NO REWIND AND NO CLOSE **
C               ******************************************
C
C       MAY, 1990.  REACTIVATE CLOSE FILE (WITH "SYSTEM" COMMAND, CAN NOW
C                   PRINT A PLOT FILE WITHOUT EXITING DATAPLOT ON SOME
C                   SYSTEMS, BUT NEED PLOT FILE TO BE CLOSED IN ORDER TO
C                   DO SO).
C
        IDEV=IGENID
C
        ISTEPN='22'
        IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP2')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,2211)IOPERA,IDEV,IPL1CS,IPL2CS
 2211     FORMAT('IOPERA,IDEV,IPL1CS,IPL2CS = ',A4,2X,A4,2X,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IOPERA.NE.'CLOS')GOTO9000
C
C  FOLLOWING 3 LINES MAY, 1990.  DON'T CLOSE IF NOT OPEN
C
        IF(IDEV.EQ.'PLO1'.AND.IPL1CS.NE.'OPEN')GOTO2299
        IF(IDEV.EQ.'PLO2'.AND.IPL2CS.NE.'OPEN')GOTO2299
C
CCCCC   APRIL 1992.  NEED TO CALL GREXIT IF CLOSE DEVICE.  NEEDED IN
CCCCC                PARTICULAR FOR LASER PRINTERS SUCH AS POSTSCRIPT
CCCCC                TO GET THE LAST PAGE PRINTED.
C
        IF(IDEV.EQ.'PLO1')IJUNK=2
        IF(IDEV.EQ.'PLO2')IJUNK=3
        IF(IDEV.EQ.'SCRE')IJUNK=1
        IF(IJUNK.LE.0 .OR. IJUNK.GT.3)GOTO9000
        IMANUF=IDMANU(IJUNK)
        IMODEL=IDMODE(IJUNK)
        IMODE2=IDMOD2(IJUNK)
        IMODE3=IDMOD3(IJUNK)
        IGCODE=IDCODE(IJUNK)
        IGUNIT=IDUNIT(IJUNK)
        NUMHPP=IDNHPP(IJUNK)
        ANUMHP=NUMHPP
        NUMVPP=IDNVPP(IJUNK)
        ANUMVP=NUMVPP
        IGCOLO=IDCOLO(IJUNK)
        IGFONT=IDFONT(IJUNK)
        PCHSCA=PDSCAL(IDEVIC)
        IGBAUD=IDBAUD(IJUNK)
        ISOFT=IDSOFT(IJUNK)
        ISOFT2=IDSOF2(IJUNK)
        ISOFT3=IDSOF3(IJUNK)
C
        IF(IDEV.EQ.'SCRE'.AND.ICAPSW.EQ.'ON')THEN
          IGUNIT=IGENNU
        ENDIF
C
        CALL GREXIT
        IF(IDEV.EQ.'SCR')GOTO9000
C
        IENDFI='ON'
        IREWIN='OFF'
C
C  MAY, 1990.  UNCOMMENTED FOLLOWING 3 LINES
C
        IF(IGDFLG.NE.'ON' .AND. ICAFLG.NE.'ON')THEN
          CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                IENDFI,IREWIN,ISUBN0,IERRFI,IBUGO2,ISUBRO,IERROR)
          IF(IERRFI.EQ.'YES')GOTO9000
        ENDIF
C
C       MAY, 1988.    DO ENDFILE ONLY IN DPEXIT.
C       MAY, 1990.    UNCOMMENT
C       APRIL, 1992.  ENDFILE OF CLOSED FILE CAUSES PC VERSION TO
C                     BOMB.  ENDFILE SHOULD BE HANDLED (IF NEEDED) IN
C                     DPCLFI.
CCCCC   ENDFILE IOUNIT
C
CCCCC   THE FOLLOWING 2 LINES WERE FIXED MAY 1992 (JJF)
CCCCC   IPL1CS='CLOSED'
CCCCC   IPL2CS='CLOSED'
        IF(IDEV.EQ.'PLO1')IPL1CS='CLOSED'
        IF(IDEV.EQ.'PLO2')IPL2CS='CLOSED'
CCCCC   JANUARY   2003.  FOR POSTSCRIPT OUTPUT, IF IPSTDV SET TO
CCCCC                    JPEG, PDF, TIFF, PBM, PNG, PPM, OR PPN, THEN
CCCCC                    USE GHOSTSCRIPT TO MAKE THE APPROPRIATE IMAGE
CCCCC                    FILE (ORIGINAL POSTSCRIPT FILE WILL NOT BE
CCCCC                    CHANGED).  IF CAPTURE HTML HAS BEEN ENTERED,
CCCCC                    HANDLE SEPARATELY.
C
        CALL DPDEP3(IOPERA,IGENNU,IGENID,ICAPSW,IFILE,
     1              IBUGO2,ISUBRO,IFOUND,IERROR)
C
 2299   CONTINUE
        GOTO9000
      ENDIF
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IDEV,IOPERA,IGENID,IGENNU
 9012   FORMAT('IDEV,IOPERA,IGENID,IGENNU = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,IDEV,IOUNIT
 9014   FORMAT('IERROR,IDEV,IOUNIT = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE
 9022   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ISTAT,IFORM,IACCES,IPROT,ICURST
 9023   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST  = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)IENDFI,IREWIN,ISUBN0,IERRFI
 9028   FORMAT('IENDFI,IREWIN,ISUBN0,IERRFI = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)IOPERA,IDEV,IPL1CS,IPL2CS
 9041   FORMAT('IOPERA,IDEV,IPL1CS,IPL2CS = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEP3(IOPERA,IGENNU,IGENID,ICAPSW,IFILE,
     1                  IBUGO2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CALLED BY DPDEP3 TO CONVERT PLOT FILE TO A
C              SPECIFIED FORMAT OR TO CREATE HTML/LATEX CODE
C              TO EMBED FILE.  PUT IN SEPARATE ROUTINE TO
C              MAKE IT EASIER TO CALL FROM DPEXIT IN ADDITION
C              TO DPDEP3.
C     INPUT  ARGUMENTS--IOPERA (A CHARACTER VARIABLE
C                              DESCRIBING THE DESIRED OPERATION
C                              (OPEN OR CLOSE)
C                     --IGENNU (AN INTEGER VALUE
C                              BY WHICH THE PLOT  FILE/SUBFILE
C                              MAY BE REFERENCED IN A FORTRAN
C                              I/O STATEMENT.
C                     --IGENID (A CHARACTER VARIABLE
C                              CONTAINING IDENTIFICATION INFORMATION
C                              FOR THE PLOT  FILE/SUBFILE
C                             (E.G., PLO1, PLO2, GENE, ETC.)
C                     --IANS   (A  CHARACTER VECTOR)
C                     --IWIDTH (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IFOUND ('YES' OR 'NO' )
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
C     VERSION NUMBER--2019/03
C     ORIGINAL VERSION--MARCH     2019. EXTRACT AS SEPARATE
C                                       SUBROUTINE
C     UPDATED         --AUGUST    2019. ADD "PS2PDF" OPTION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOPERA
      CHARACTER*4 IGENID
      CHARACTER*4 IGDFLG
      CHARACTER*4 ICAFLG
      CHARACTER*4 ICAPSW
      CHARACTER*4 IBUGO2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER*80 IFILE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*256 ISTRIN
      CHARACTER*256 ISTRI2
      CHARACTER*80 IFIL2
      CHARACTER*80 IFILZ
      CHARACTER*1 IQUOTE
      CHARACTER*1 IBASLC
      CHARACTER*1 IFOSLC
      CHARACTER*1 IATEMP
      CHARACTER*4 IEXIST
      CHARACTER*12 IFWRIT
      CHARACTER*16 IFORMT
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*3 IEXT
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
      LOGICAL IOPPLO
      COMMON/OPNPLT/IOPPLO
C
      CHARACTER*80 IFILSV
      COMMON/ICFILE/IFILSV,NCSAVE
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
      IFOUND='YES'
      IERROR='NO'
      ISUBN1='DPDE'
      ISUBN2='P3  '
      IFILSV=' '
      IQUOTE='"'
      CALL DPCONA(92,IBASLC)
C
      NCSAVE=0
C
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IMANUF,IMODEL,ICAPSW,ICAPTY
   52   FORMAT('IMANUF,IMODEL,ICAPSW,ICAPTY = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGO2,IERROR,IDEV,IOPERA,IGENID
   53   FORMAT('IBUGO2,IERROR,IDEV,IOPERA,IGENID = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IGENNU,IPL1NU,IPL2NU,IWIDTH
   54   FORMAT('IGENNU,IPL1NU,IPL2NU,IWIDTH = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)IPL1NA
   62   FORMAT('IPL1NA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IPL1ST,IPL1FO,IPL1AC,IPL1FO,IPL1CS
   63   FORMAT('IPL1ST,IPL1FO,IPL1AC,IPL1FO,IPL1CS = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IPL2NA
   72   FORMAT('IPL2NA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)IPL2ST,IPL2FO,IPL2AC,IPL2FO,IPL2CS
   73   FORMAT('IPL2ST,IPL2FO,IPL2AC,IPL2FO,IPL2CS = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)IFILE
   82   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ICON=0
      IF(IPSTDV.EQ.'PDF ')THEN
        ICON=1
        IEXT='pdf'
      ELSEIF(IPSTDV.EQ.'JPEG' .OR. IPSTDV.EQ.'JPG')THEN
        ICON=1
        IEXT='jpg'
      ELSEIF(IPSTDV.EQ.'PNG  ')THEN
        ICON=1
        IEXT='png'
      ELSEIF(IPSTDV.EQ.'TIFF')THEN
        ICON=1
        IEXT='tif'
      ELSEIF(IPSTDV.EQ.'PBM  ')THEN
        ICON=1
        IEXT='pbm'
      ELSEIF(IPSTDV.EQ.'PGM  ')THEN
        ICON=1
        IEXT='pgm'
      ELSEIF(IPSTDV.EQ.'PPM  ')THEN
        ICON=1
        IEXT='ppm'
      ELSEIF(IPSTDV.EQ.'PNM  ')THEN
        ICON=1
        IEXT='pnm'
      ENDIF
C
      ILAST=80
      IPEROD=0
      IFLGSP=0
C
      DO101I=ILAST,1,-1
        IF(IFILE(I:I).NE.' ')THEN
          ILASTT=I
          GOTO103
        ENDIF
  101 CONTINUE
      GOTO9000
  103 CONTINUE
C
      DO105I=1,ILASTT
        IF(IFILE(I:I).EQ.'.')THEN
          IPEROD=I
        ELSEIF(IFILE(I:I).EQ.' ')THEN
          IFLGSP=1
        ENDIF
  105 CONTINUE
C
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')THEN
        WRITE(ICOUT,131)ICON,ILAST,ILASTT,IPEROD,IFLGSP,IEXT
  131   FORMAT('ICON,ILAST,ILASTT,IPEROD,IFLGSP,IEXT = ',5I5,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC FOR POSTSCRIPT OUTPUT, IF IPSTDV SET TO JPEG, PDF, TIFF, PBM,
CCCCC PNG, PPM, OR PPN, USE GHOSTSCRIPT TO MAKE THE APPROPRIATE IMAGE
CCCCC FILE (ORIGINAL POSTSCRIPT FILE WILL NOT BE CHANGED).  IF
CCCCC CAPTURE HTML HAS BEEN ENTERED, HANDLE SEPARATELY.
C
      IF(ICAPSW.EQ.'OFF'.OR.
     1  (ICAPTY.NE.'HTML'.AND.ICAPTY.NE.'LATE'))THEN
C
        ISTEPN='11'
        IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IMANUF.EQ.'POST'.AND.ICON.GT.0)THEN
C
C         2015/12: IF "SET DEVICE 3 NAME COUNTER ON" IS
C                  GIVEN, THEN APPEND "_x" (WHERE x IS THE
C                  COUNTER VALUE) BEFORE THE PERIOD.
C
C
          ISTEPN='12'
          IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(IPEROD.GT.0)THEN
C
C           SET DEVICE 3 NAME COUNTER ON CASE.  NOTE THAT WE
C           ALSO NEED TO ADJUST IFILE NAME AS WELL (IFILE IS
C           CREATED WHEN FILE IS OPENED, BUT IPL2NA IS NOT
C           ADJUSTED, SO NEED TO REDO HERE).
C
            ISTEPN='121'
            IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
            IF(IDEV.EQ.'PLO2' .AND. IDV3NC.EQ.'ON')THEN
C
C             ADJUST "IFILE" NAME FIRST.
C
              ISTEPN='122'
              IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              NADJST=1
              IF(ICNTD3.GE.1000 .AND. ICNTD3.LE.9999)THEN
                NADJST=NADJST+4
              ELSEIF(ICNTD3.GE.100 .AND. ICNTD3.LE.999)THEN
                NADJST=NADJST+3
              ELSEIF(ICNTD3.GE.10 .AND. ICNTD3.LE.99)THEN
                NADJST=NADJST+2
              ELSEIF(ICNTD3.GE.1 .AND. ICNTD3.LE.9)THEN
                NADJST=NADJST+1
              ENDIF
              IPOS=IPEROD
              IFILE(IPOS+1+NADJST:80)=IFILE(IPOS+1:80-NADJST)
              IFILE(IPOS:IPOS)='_'
              IF(ICNTD3.GE.1000 .AND. ICNTD3.LE.9999)THEN
                WRITE(IFILE(IPOS+1:IPOS+4),'(I4)')ICNTD3
                IPOS=IPOS+4
              ELSEIF(ICNTD3.GE.100 .AND. ICNTD3.LE.999)THEN
                WRITE(IFILE(IPOS+1:IPOS+3),'(I3)')ICNTD3
                IPOS=IPOS+3
              ELSEIF(ICNTD3.GE.10 .AND. ICNTD3.LE.99)THEN
                WRITE(IFILE(IPOS+1:IPOS+2),'(I2)')ICNTD3
                IPOS=IPOS+2
              ELSEIF(ICNTD3.GE.1 .AND. ICNTD3.LE.9)THEN
                WRITE(IFILE(IPOS+1:IPOS+1),'(I1)')ICNTD3
                IPOS=IPOS+1
              ENDIF
              IPOS=IPOS+1
              IFILE(IPOS:IPOS)='.'
              IPEROD=IPOS
              ILASTT=ILASTT+NADJST
C
              IFIL2=' '
              IFIL2(1:IPEROD)=IFILE(1:IPEROD)
C
            ELSEIF(IDEV.EQ.'PLO1' .AND. IDV2SP.EQ.'ON')THEN
C
C             ADJUST "IFILE" NAME FIRST.
C
              ISTEPN='132'
              IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              NADJST=1
              IF(ICNTD2.GE.1000 .AND. ICNTD2.LE.9999)THEN
                NADJST=NADJST+4
              ELSEIF(ICNTD2.GE.100 .AND. ICNTD2.LE.999)THEN
                NADJST=NADJST+3
              ELSEIF(ICNTD2.GE.10 .AND. ICNTD2.LE.99)THEN
                NADJST=NADJST+2
              ELSEIF(ICNTD2.GE.1 .AND. ICNTD2.LE.9)THEN
                NADJST=NADJST+1
              ENDIF
              IPOS=IPEROD
              IFILE(IPOS+1+NADJST:80)=IFILE(IPOS+1:80-NADJST)
              IFILE(IPOS:IPOS)='_'
              IF(ICNTD2.GE.1000 .AND. ICNTD2.LE.9999)THEN
                WRITE(IFILE(IPOS+1:IPOS+4),'(I4)')ICNTD2
                IPOS=IPOS+4
              ELSEIF(ICNTD2.GE.100 .AND. ICNTD2.LE.999)THEN
                WRITE(IFILE(IPOS+1:IPOS+3),'(I3)')ICNTD2
                IPOS=IPOS+3
              ELSEIF(ICNTD2.GE.10 .AND. ICNTD2.LE.99)THEN
                WRITE(IFILE(IPOS+1:IPOS+2),'(I2)')ICNTD2
                IPOS=IPOS+2
              ELSEIF(ICNTD2.GE.1 .AND. ICNTD2.LE.9)THEN
                WRITE(IFILE(IPOS+1:IPOS+1),'(I1)')ICNTD2
                IPOS=IPOS+1
              ENDIF
              IPOS=IPOS+1
              IFILE(IPOS:IPOS)='.'
              IPEROD=IPOS
              ILASTT=ILASTT+NADJST
C
              IFIL2=' '
              IFIL2(1:IPEROD)=IFILE(1:IPEROD)
C
            ELSE
              IFIL2=' '
              IFIL2(1:IPEROD)=IFILE(1:IPEROD)
            ENDIF
C
            IFIL2(IPEROD+1:IPEROD+3)=IEXT(1:3)
            NCTEMP=IPEROD+3
            IFILSV(1:NCTEMP)=IFIL2(1:NCTEMP)
            NCSAVE=NCTEMP
          ELSE
            IF(ILASTT.GT.ILAST-4)GOTO9000
            IFIL2=' '
            IFIL2(1:ILASTT)=IFILE(1:ILASTT)
            IFIL2(ILASTT+1:ILASTT+1)='.'
            IFIL2(ILASTT+2:ILASTT+4)=IEXT(1:3)
            NCTEMP=ILASTT+4
            IFILSV(1:NCTEMP)=IFIL2(1:NCTEMP)
            NCSAVE=NCTEMP
          ENDIF
C
C         CONVERT FROM POSTSCRIPT TO REQUESTED FORMAT (GHOSTSCRIPT)
C
C         2007/10: CAN OPTIONALLY RUN CONVERT
C         2009/03: ALLOW USER SPECIFIED DENSITY
C         2019/03: FOR WINDOWS PLATFORM, WOULD LIKE TO RUN THE
C                  CONVERSION IN "HIDDEN" MODE.  HOWEVER, THIS
C                  REQUIRES USING "START ..." SYNTAX AND I HAVE A
C                  PROBLEM PASSING THE FILE NAME AS AN ARGUMENT
C                  WITH START.  UNTIL THEN, ONLY USE "HIDDEN" IF
C                  THE FILE NAME DOES NOT CONTAIN A SPACE CHARACTER.
C                  SO IN THE CODES BELOW, CHECK FOR A SPACE IN THE
C                  FILE NAME.
C         2019/08: CAN OPTIONALLY RUN PS2PDF
C
          IF(IPSTD2.EQ.'CONV')THEN
C
            ISTEPN='14'
            IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
            ISTRIN=' '
            ISTRIN(1:8)='convert '
            N0=8
            N0=N0+1
            ISTRIN(N0:N0+19)='-rotate 90 -density '
            N0=N0+20
C
            IF(ICONDH.GE.1000)THEN
              WRITE(ISTRIN(N0:N0+3),'(I4)')ICONDH
              N0=N0+4
            ELSEIF(ICONDH.GE.100)THEN
              WRITE(ISTRIN(N0:N0+2),'(I3)')ICONDH
              N0=N0+3
            ELSE
              WRITE(ISTRIN(N0:N0+1),'(I2)')ICONDH
              N0=N0+2
            ENDIF
C
            ISTRIN(N0:N0)='x'
            N0=N0+1
            IF(ICONDV.GE.1000)THEN
              WRITE(ISTRIN(N0:N0+3),'(I4)')ICONDV
              N0=N0+4
            ELSEIF(ICONDV.GE.100)THEN
              WRITE(ISTRIN(N0:N0+2),'(I3)')ICONDV
              N0=N0+3
            ELSE
              WRITE(ISTRIN(N0:N0+1),'(I2)')ICONDV
              N0=N0+2
            ENDIF
C
            ISTRIN(N0:N0)=' '
            IF(IFLGSP.EQ.1)THEN
              N0=N0+1
              ISTRIN(N0:N0)=IQUOTE
            ENDIF
            N0=N0+1
            ISTRIN(N0:N0+ILASTT-1)=IFILE(1:ILASTT)
            N0=N0+ILASTT
            ISTRIN(N0:N0)=' '
            N0=N0+1
            ISTRIN(N0:N0+NCTEMP-1)=IFIL2(1:NCTEMP)
            N0=N0+NCTEMP
            IF(IFLGSP.EQ.1)THEN
              N0=N0+1
              ISTRIN(N0:N0)=IQUOTE
            ENDIF
            ISSAV1=ISYSPE
            ISSAV2=ISYSHI
            ISYSPE='OFF'
            ISYSHI='ON'
            IF(IFLGSP.EQ.1)ISYSHI='OFF'
            CALL DPSYS2(ISTRIN,N0,ISUBRO,IERROR)
            ISYSPE=ISSAV1
            ISYSHI=ISSAV2
          ELSEIF(IPSTD2.EQ.'PS2P')THEN
C
C           2019/08: PS2PDF OPTION.  NOTE THAT PS2PDF IS INSTALLED
C                    AS PART OF GHOSTSCRIPT APPLICATION, SO CAN USE
C                    GHOSTSCRIPT PATH.  PS2PDF DOES ESSENTIALLY SAME
C                    THING AS DEFAULT "GHOSTSCRIPT" OPTION, BUT HAS A
C                    SIMPLER SYNTAX.
C
C                    NOTE THAT UNDER WINDOWS, THE PS2PDF SCRIPT
C                    IS CALLED PS2PDF.BAT AND IS LOCATED IN THE
C                    "LIB" DIRECTORY RATHER THAN THE "BIN" DIRECTORY.
C
C                    IT APPEARS THAT QUOTING THE "PS2PDF" PORTION
C                    OF THIS CAUSES PROBLEMS IN "HIDDEN" MODE
C                    UNDER WINDOWS.
C
            ISTEPN='14B'
            IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
            ISTRIN=' '
            N0=0
            IFLSP2=0
            IF(NCGHPA.GT.0)THEN
C
C             CHECK FOR SPACE IN NAME
C
              DO2213KK=1,NCGHPA
                IF(IGSTPA(KK:KK).EQ.' ')THEN
                  IFLSP2=1
                  GOTO2219
                ENDIF
 2213         CONTINUE
 2219         CONTINUE
C
              IF(ISTRIN(1:1).NE.IQUOTE .AND. IFLSP2.EQ.1)THEN
                ISTRIN(1:1)=IQUOTE
                ISTRIN(2:NCGHPA+1)=IGSTPA(1:NCGHPA)
                N0=NCGHPA+1
              ELSE
                ISTRIN(1:NCGHPA)=IGSTPA(1:NCGHPA)
                N0=NCGHPA
              ENDIF
C
              IF(IOPSY1.EQ.'UNIX')THEN
                IF(IGSTPA(NCGHPA:NCGHPA).NE.'/')THEN
                  N0=N0+1
                  ISTRIN(N0:N0)='/'
                ENDIF
              ELSEIF(IHOST1.EQ.'IBM-')THEN
                IF(IGSTPA(NCGHPA:NCGHPA).NE.IBASLC)THEN
                  N0=N0+1
                  ISTRIN(N0:N0)=IBASLC
                ENDIF
                NOM3=N0-3
                NOM1=N0-1
                IF(ISTRIN(NOM3:NOM1).EQ.'bin')ISTRIN(NOM3:NOM1)='lib'
                IF(ISTRIN(NOM3:NOM1).EQ.'BIN')ISTRIN(NOM3:NOM1)='LIB'
              ENDIF
C
            ENDIF
C
            N0=N0+1
            ISTRIN(N0:N0+5)='ps2pdf'
            N0=N0+5
            IF(IHOST1.EQ.'IBM-')THEN
              N0=N0+1
              ISTRIN(N0:N0+3)='.bat'
              N0=N0+3
            ENDIF
            IF(IFLSP2.EQ.1 .OR.ISTRIN(1:1).EQ.IQUOTE)THEN
              N0=N0+1
              ISTRIN(N0:N0)=IQUOTE
              IFLSP2=1
            ENDIF
            N0=N0+1
            ISTRIN(N0:N0)=' '
            IF(IFLGSP.EQ.1)THEN
              N0=N0+1
              ISTRIN(N0:N0)=IQUOTE
            ENDIF
C
            N0=N0+1
            ISTRIN(N0:N0+ILASTT-1)=IFILE(1:ILASTT)
            N0=N0+ILASTT-1
            IF(IFLGSP.EQ.1)THEN
              N0=N0+1
              ISTRIN(N0:N0)=IQUOTE
            ENDIF
            ISSAV1=ISYSPE
            ISSAV2=ISYSHI
            ISYSPE='OFF'
            ISYSHI='ON'
            IF(IFLGSP.EQ.1 .OR. IFLSP2.EQ.1)ISYSHI='OFF'
C
            IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')THEN
               WRITE(ICOUT,2201)N0,ISTRIN(1:N0)
 2201          FORMAT('BEFORE DPSYS2: N0,ISTRIN = ',I5,2X,A80)
               CALL DPWRST('XXX','BUG ')
            ENDIF
C
            CALL DPSYS2(ISTRIN,N0,ISUBRO,IERROR)
            ISYSPE=ISSAV1
            ISYSHI=ISSAV2
          ELSE
C
C           2015/11: SINCE GHOSTSCRIPT IS NOW INSTALLED IN
C                    "Program Files" UNDER WINDOWS, ENCLOSE THE
C                    GHOSTSCRIPT PATH IN QUOTES.
C
            ISTEPN='15'
            IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C           CHECK FOR SPACE IN NAME
C
            IFLSP2=0
            DO2223KK=1,NCGHPA
              IF(IGSTPA(KK:KK).EQ.' ')THEN
                IFLSP2=1
                GOTO2229
              ENDIF
 2223       CONTINUE
 2229       CONTINUE
C
            ISTRIN=' '
            IF(NCGHPA.GT.0)THEN
              IF(ISTRIN(1:1).NE.IQUOTE .AND. IFLSP2.EQ.1)THEN
                ISTRIN(1:1)=IQUOTE
                ISTRIN(2:NCGHPA+1)=IGSTPA(1:NCGHPA)
                N0=NCGHPA+1
              ELSE
                ISTRIN(1:NCGHPA)=IGSTPA(1:NCGHPA)
                N0=NCGHPA
                IFLSP2=1
              ENDIF
C
              IF(IOPSY1.EQ.'UNIX')THEN
                IF(IGSTPA(NCGHPA:NCGHPA).NE.'/')THEN
                  N0=N0+1
                  ISTRIN(N0:N0)='/'
                ENDIF
              ELSEIF(IHOST1.EQ.'IBM-')THEN
                IF(IGSTPA(NCGHPA:NCGHPA).NE.IBASLC)THEN
                  N0=N0+1
                  ISTRIN(N0:N0)=IBASLC
                ENDIF
              ENDIF
            ENDIF
C
            IF(IOPSY1.EQ.'UNIX')THEN
              N0=N0+1
              ISTRIN(N0:N0+1)='gs'
              N0=N0+1
            ELSEIF(IHOST1.EQ.'IBM-')THEN
              N0=N0+1
              IF(IGSTVR.EQ.'64')THEN
                ISTRIN(N0:N0+11)='GSWIN64C.EXE'
                N0=N0+11
              ELSE
                ISTRIN(N0:N0+11)='GSWIN32C.EXE'
                N0=N0+11
              ENDIF
            ENDIF
C
            IF(IFLSP2.EQ.1)THEN
              N0=N0+1
              ISTRIN(N0:N0)=IQUOTE
              N0=N0+1
              ISTRIN(N0:N0)=' '
            ENDIF
C
            N0=N0+1
            ISTRIN(N0:N0+29)='-dNOPAUSE -dBATCH -q -sDEVICE='
            N0=N0+30
            IF(IPSTDV.EQ.'PDF ')THEN
              ISTRIN(N0:N0+8)='pdfwrite '
              N0=N0+9
            ELSEIF(IPSTDV.EQ.'JPEG' .OR. IPSTDV.EQ.'JPG')THEN
              ISTRIN(N0:N0+4)='jpeg '
              N0=N0+5
            ELSEIF(IPSTDV.EQ.'TIFF')THEN
              ISTRIN(N0:N0+7)='tifflzw '
              N0=N0+8
            ELSEIF(IPSTDV.EQ.'PNG ' .AND. IHOST1.EQ.'IBM-')THEN
              ISTRIN(N0:N0+5)='png16 '
              N0=N0+6
            ELSE
              ISTRIN(N0:N0+3)=IEXT(1:3)
              N0=N0+4
            ENDIF
C
            ISTRIN(N0:N0+12)='-sOutputFile='
            N0=N0+12
            IF(IFLGSP.EQ.1)THEN
              N0=N0+1
              ISTRIN(N0:N0)=IQUOTE
            ENDIF
            N0=N0+1
            ISTRIN(N0:N0+NCTEMP-1)=IFIL2(1:NCTEMP)
            N0=N0+NCTEMP
            IF(IFLGSP.EQ.1)THEN
              N0=N0+1
              ISTRIN(N0:N0)=IQUOTE
            ENDIF
            N0=N0+1
            ISTRIN(N0:N0)=' '
            N0=N0+1
            ISTRIN(N0:N0+ILASTT-1)=IFILE(1:ILASTT)
            N0=N0+ILASTT-1
            ISSAV1=ISYSPE
            ISSAV2=ISYSHI
            ISYSPE='OFF'
            ISYSHI='ON'
            IF(IFLGSP.EQ.1 .OR. IFLSP2.EQ.1)ISYSHI='OFF'
            CALL DPSYS2(ISTRIN,N0,ISUBRO,IERROR)
            ISYSPE=ISSAV1
            ISYSHI=ISSAV2
          ENDIF
        ENDIF
C
CCCCC   SEPTEMBER 2002.  IF,
CCCCC      1) CAPTURE SWITCH ON
CCCCC      2) CAPTURE IS IN HTML FORMAT
CCCCC      3) DEVICE IS SVG OR GD
CCCCC   THEN PUT A "<img ..." OF THE JUST CREATED GRAPHIC FILE
CCCCC   IN THE HTML OUTPUT.
CCCCC   OCTOBER 2002.   SLIGHTLY DIFFERENT SYNTAX FOR SVG OUTPUT.
CCCCC   JANUARY 2003.   SUPPORT POSTSCRIPT GRAPHS AS WELL.  FOR POSTSCRIPT,
CCCCC                   RUN GHOSTSCRIPT ON POSTSCRIPT FILE.  SYNTAX WILL
CCCC                    BE DIFFERENT ON UNIX AND WINDOWS.
CCCCC   SEPTEMBER 2010. FOR SVG, USE "OBJECT" SYNTAX RATHER THAN "EMBED"
CCCCC                   SYNTAX
CCCCC   JULY      2015. FOR SVG, IF "SVG BACKUP FILE" HAS BEEN SET TO
CCCCC                   PNG, JPG, OR GIF, THEN ADD AN "<IMG" TAG TO THAT
CCCCC                   FILE.  NOTE THAT THE BACKUP FILE IS NOT CREATED
CCCCC                   AUTOMATICALLY HERE, RATHER IT IS ASSUMED THAT IT
CCCCC                   WAS PREVIOUSLY CREATED.
C
      ELSEIF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'HTML')THEN
C
        ISTEPN='2'
        IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IMANUF.EQ.'GD' .OR. IMANUF.EQ.'SVG' .OR.
     1    (IMANUF.EQ.'POST' .AND. IDEV.EQ.'PLO1'))THEN
C
          ISTEPN='21'
          IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1       CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          WRITE(ICOUT,2301)
 2301     FORMAT('</PRE>')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
C
CCCCC     JUNE 2008: FOR HTML, THE <IMG SRC=" ... " SEEMS TO
CCCCC                HAVE A PROBLEM IN INTERNET EXPLORER 7.
CCCCC                INSTEAD OF PRINTING "IFILE" AND THEN ADDING
CCCCC                A QUOTE, ADD THE QUOTE DIRECTLY TO IFILE
CCCCC                AFTER THE LAST NON-BLANK CHARACTER.
C
          IFILZ=' '
          IFILZ(1:ILASTT)=IFILE(1:ILASTT)
          IF(ILASTT.LT.80)THEN
            IFILZ(ILASTT+1:ILASTT+1)='"'
            ILASTT=ILASTT+1
            IFLAG=0
          ELSE
            IFLAG=1
          ENDIF
C
          IF(IMANUF.EQ.'GD')THEN
            WRITE(ICOUT,2307)
 2307       FORMAT('<IMG SRC=')
            CALL DPWRST('XXX','WRIT')
            IF(IFLAG.EQ.0)THEN
              WRITE(ICOUT,2309)IFILZ(1:ILASTT)
 2309         FORMAT('     "',A80)
              CALL DPWRST('XXX','WRIT')
            ELSE
              WRITE(ICOUT,2310)IFILZ(1:ILASTT)
 2310         FORMAT('     "',A80,'"')
              CALL DPWRST('XXX','WRIT')
            ENDIF
            WRITE(ICOUT,2319)
 2319       FORMAT('     ALT="DATAPLOT GRAPH">')
            CALL DPWRST('XXX','WRIT')
          ELSEIF(IMANUF.EQ.'SVG')THEN
            WRITE(ICOUT,2331)
 2331       FORMAT('<P>')
            CALL DPWRST('XXX','WRIT')
C
C           IF "SET SVG URL" COMMAND GIVEN, PRE-PEND THIS TO FILE
C           NAME.  IF THERE IS A PATH NAME ON FILE, EXTRACT THIS.
C
            IF(ISVGUR.NE.'NULL' .AND. ISVGUR.NE.' ')THEN
              ISTRIN=' '
              DO22231I=1,ILAST
                IF(ISVGUR(I:I).NE.' ')THEN
                  ISTRT1=I
                  GOTO22239
                ENDIF
22231         CONTINUE
22239         CONTINUE
C
              DO22331I=ILAST,1,-1
                IF(ISVGUR(I:I).NE.' ')THEN
                  ICNT=I-ISTRT1+1
                  ISTRIN(1:ICNT)=ISVGUR(ISTRT1:I)
                  GOTO22339
                ENDIF
22331         CONTINUE
22339         CONTINUE
            ELSE
              ISTRIN=' '
              ICNT=0
            ENDIF
C
            CALL DPCONA(47,IFOSLC)
            ISTRT2=1
            ISTOP2=1
            IFLAG2=0
            DO22341I=ILASTT,1,-1
              IATEMP=IFILZ(I:I)
              IF(IFLAG2.EQ.0 .AND. IATEMP.NE.' ')THEN
                ISTOP2=I
                IFLAG2=1
              ELSEIF(IATEMP.EQ.IBASLC .OR. IATEMP.EQ.IFOSLC)THEN
                ISTRT2=I+1
                GOTO22349
              ENDIF
22341       CONTINUE
22349       CONTINUE
C
            NLEN=ISTOP2-ISTRT2+1
            ISTRIN(ICNT+1:ICNT+NLEN)=IFILZ(ISTRT2:ISTOP2)
            ICNT=ICNT+NLEN
            ICNT=MIN(ICNT,255)
            IFORMT='(A14,A   )'
            WRITE(IFORMT(7:9),'(I3)')ICNT
            WRITE(ICOUT,IFORMT)'<OBJECT data="',ISTRIN(1:ICNT)
            CALL DPWRST('XXX','WRIT')
C
            WRITE(ICOUT,2333)INT(ANUMHP+0.1),INT(ANUMVP+0.1)
 2333       FORMAT('        width="',I5,'" height="',I5,'"')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,2335)
 2335       FORMAT('        type="image/svg+xml">')
            CALL DPWRST('XXX','WRIT')
C
C           WAS BACKUP FILE REQUESTED?
C
            IFLAG1=0
            IF(ISVGBU.NE.'NONE')THEN
              ISTRI2=' '
              IF(ISTRIN(ICNT:ICNT).EQ.'"')THEN
                ICNT=ICNT-1
              ENDIF
              ISTRI2(1:ICNT)=ISTRIN(1:ICNT)
              ISTRT=ICNT-2
              IF(ISTRT.GT.1)THEN
                IF(ISTRIN(ISTRT:ICNT).EQ.'SVG')THEN
                  IF(ISVGBU.EQ.'JPG')THEN
                    ISTRI2(ISTRT:ICNT)='JPG'
                    IFLAG1=1
                  ELSEIF(ISVGBU.EQ.'PNG')THEN
                    ISTRI2(ISTRT:ICNT)='PNG'
                    IFLAG1=1
                  ELSEIF(ISVGBU.EQ.'GIF')THEN
                    ISTRI2(ISTRT:ICNT)='GIF'
                    IFLAG1=1
                  ENDIF
                ELSEIF(ISTRIN(ISTRT:ICNT).EQ.'svg')THEN
                  IF(ISVGBU.EQ.'JPG')THEN
                    ISTRI2(ISTRT:ICNT)='jpg'
                    IFLAG1=1
                  ELSEIF(ISVGBU.EQ.'PNG')THEN
                    ISTRI2(ISTRT:ICNT)='png'
                    IFLAG1=1
                  ELSEIF(ISVGBU.EQ.'GIF')THEN
                    ISTRI2(ISTRT:ICNT)='gif'
                    IFLAG1=1
                  ENDIF
                ENDIF
C
                IF(IFLAG1.EQ.1)THEN
                  IFORMT='(8X,A10,A   ,A1)'
                  WRITE(IFORMT(10:12),'(I3)')ICNT
                  WRITE(ICOUT,IFORMT)'<IMG SRC="',ISTRI2(1:ICNT),'"'
                  CALL DPWRST('XXX','WRIT')
                  WRITE(ICOUT,22337)
22337             FORMAT(13X,'ALT="Dataplot Graph">')
                  CALL DPWRST('XXX','WRIT')
                ENDIF
              ENDIF
            ENDIF
C
            WRITE(ICOUT,2337)
 2337       FORMAT('</OBJECT>')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,2338)
 2338       FORMAT('<P>')
            CALL DPWRST('XXX','WRIT')
          ELSEIF(IMANUF.EQ.'POST')THEN
C
C           POSTSCRIPT TO JPEG USES GHOSTSCRIPT, CURRENTLY ONLY
C           AVAILABLE FOR WINDOWS AND UNIX.
C
C           NOTE: IF IPSTDV=PDF, THEN CONVERT TO PDF RATHER THAN JPEG
C
C           DETERMINE NAME FOR JPEG FILE
C
            ISTEPN='22'
            IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
            IF(IPEROD.GT.0)THEN
              IFIL2=' '
              IFIL2(1:IPEROD)=IFILE(1:IPEROD)
              IFIL2(IPEROD+1:IPEROD+3)='jpg'
              IF(IPSTDV.EQ.'PDF')IFIL2(IPEROD+1:IPEROD+3)='pdf'
              NCTEMP=IPEROD+3
            ELSE
              IF(ILASTT.GT.ILAST-4)GOTO9000
              IFIL2=' '
              IFIL2(1:ILASTT)=IFILE(1:ILASTT)
              IFIL2(ILASTT+1:ILASTT+4)='.jpg'
              IF(IPSTDV.EQ.'PDF')IFIL2(ILASTT+1:ILASTT+4)='.pdf'
              NCTEMP=ILASTT+4
            ENDIF
C
            IF(NCTEMP.LT.ILAST)THEN
              NCTEMP=NCTEMP+1
              IFIL2(NCTEMP:NCTEMP)='"'
              IFLAG=0
            ELSE
              IFLAG=1
            ENDIF
C
C           2018/04: FOR WINDOWS, USE "EMBED OPTION RATHER LINK.
C                    THIS MAY NOT WORK FOR ALL BROWSERS, BUT IT
C                    SHOULD PROVIDE BETTER LINK WHEN IT DOES.
C                    SPECIFICALLY, IT SHOULD WORK.
C
            IF(IPSTDV.EQ.'PDF')THEN
C
              ISTEPN='221'
              IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              IF(IHOST1.NE.'IBM-')THEN
                WRITE(ICOUT,2371)
 2371           FORMAT('<A HREF=')
                CALL DPWRST('XXX','WRIT')
                IF(IFLAG.EQ.0)THEN
                  WRITE(ICOUT,2373)IFIL2
 2373             FORMAT('     "',A80,'>')
                  CALL DPWRST('XXX','WRIT')
                ELSE
                  WRITE(ICOUT,2374)IFIL2
 2374             FORMAT('     "',A80,'">')
                  CALL DPWRST('XXX','WRIT')
                ENDIF
                WRITE(ICOUT,2375)
 2375           FORMAT('     DATAPLOT GRAPH (PDF FORMAT)</A>')
                CALL DPWRST('XXX','WRIT')
              ELSE
                WRITE(ICOUT,2370)
 2370           FORMAT('<embed src=')
                CALL DPWRST('XXX','WRIT')
                IF(IFLAG.EQ.0)THEN
                  WRITE(ICOUT,2359)IFIL2
                  CALL DPWRST('XXX','WRIT')
                ELSE
                  WRITE(ICOUT,2360)IFIL2
                  CALL DPWRST('XXX','WRIT')
                ENDIF
                WRITE(ICOUT,2378)
 2378           FORMAT('     width="700" height="525"')
                CALL DPWRST('XXX','WRIT')
                IQUOTE="'"
                WRITE(ICOUT,2372)IQUOTE,IQUOTE
 2372           FORMAT('     TYPE=',A1,'application/pdf',A1)
                CALL DPWRST('XXX','WRIT')
                WRITE(ICOUT,2376)
 2376           FORMAT('     alt="DATAPLOT GRAPH (PDF FORMAT)">')
                CALL DPWRST('XXX','WRIT')
                IQUOTE='"'
              ENDIF
            ELSE
C
              ISTEPN='222'
              IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              WRITE(ICOUT,2357)
 2357         FORMAT('<IMG SRC=')
              CALL DPWRST('XXX','WRIT')
              IF(IFLAG.EQ.0)THEN
                WRITE(ICOUT,2359)IFIL2
 2359           FORMAT('     "',A80)
                CALL DPWRST('XXX','WRIT')
              ELSE
                WRITE(ICOUT,2360)IFIL2
 2360           FORMAT('     "',A80,'"')
                CALL DPWRST('XXX','WRIT')
              ENDIF
              WRITE(ICOUT,2369)
 2369         FORMAT('     ALT="DATAPLOT GRAPH">')
              CALL DPWRST('XXX','WRIT')
            ENDIF
C
C           FOR POSTSCRIPT, NEED TO RUN GHOSTSCRIPT TO CONVERT FROM
C           POSTSCRIPT TO JPEG.
C
C           OCTOBER 2007: OPTIONALLY RUN CONVERT RATHER THAN GHOSTSCRIPT.
C           MARCH   2009: ALLOW USER SPECIFIED DENSITY
C
            IF(IPSTD2.EQ.'CONV')THEN
C
              ISTEPN='223'
              IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              ISTRIN=' '
              ISTRIN(1:8)='convert '
              N0=8
              N0=N0+1
              ISTRIN(N0:N0+19)='-rotate 90 -density '
              N0=N0+20
              IF(ICONDH.GE.1000)THEN
                WRITE(ISTRIN(N0:N0+3),'(I4)')ICONDH
                N0=N0+4
              ELSEIF(ICONDH.GE.100)THEN
                WRITE(ISTRIN(N0:N0+2),'(I3)')ICONDH
                N0=N0+3
              ELSE
                WRITE(ISTRIN(N0:N0+1),'(I2)')ICONDH
                N0=N0+2
              ENDIF
              ISTRIN(N0:N0)='x'
              N0=N0+1
              IF(ICONDV.GE.1000)THEN
                WRITE(ISTRIN(N0:N0+3),'(I4)')ICONDV
                N0=N0+4
              ELSEIF(ICONDV.GE.100)THEN
                WRITE(ISTRIN(N0:N0+2),'(I3)')ICONDV
                N0=N0+3
              ELSE
                WRITE(ISTRIN(N0:N0+1),'(I2)')ICONDV
                N0=N0+2
              ENDIF
C
              ISTRIN(N0:N0)=' '
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              N0=N0+1
              ISTRIN(N0:N0+ILASTT-1)=IFILE(1:ILASTT)
              N0=N0+ILASTT
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              N0=N0+1
              ISTRIN(N0:N0)=' '
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              N0=N0+1
              ISTRIN(N0:N0+NCTEMP-1)=IFIL2(1:NCTEMP)
              N0=N0+NCTEMP
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              ISSAV1=ISYSPE
              ISSAV2=ISYSHI
              ISYSPE='OFF'
              ISYSHI='ON'
              IF(IFLGSP.EQ.1)ISYSHI='OFF'
              CALL DPSYS2(ISTRIN,N0,ISUBRO,IERROR)
              ISYSPE=ISSAV1
              ISYSHI=ISSAV2
            ELSEIF(IPSTD2.EQ.'PS2P')THEN
C
              ISTEPN='224A'
              IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C             CHECK FOR SPACE IN NAME
C
              IFLSP2=0
              DO2233KK=1,NCGHPA
                IF(IGSTPA(KK:KK).EQ.' ')THEN
                  IFLSP2=1
                  GOTO2239
                ENDIF
 2233         CONTINUE
 2239         CONTINUE
C
              ISTRIN=' '
              IF(NCGHPA.GT.0)THEN
                IF(IGSTPA(1:1).NE.IQUOTE .AND. IFLSP2.EQ.1)THEN
                  ISTRIN(1:1)=IQUOTE
                  ISTRIN(2:NCGHPA+1)=IGSTPA(1:NCGHPA)
                  N0=NCGHPA+1
                ELSE
                  ISTRIN(1:NCGHPA)=IGSTPA(1:NCGHPA)
                  N0=NCGHPA
                ENDIF
                IF(IOPSY1.EQ.'UNIX')THEN
                  IF(IGSTPA(NCGHPA:NCGHPA).NE.'/')THEN
                    N0=N0+1
                    ISTRIN(N0:N0)='/'
                  ENDIF
                ELSEIF(IHOST1.EQ.'IBM-')THEN
                  IF(IGSTPA(NCGHPA:NCGHPA).NE.IBASLC)THEN
                    N0=N0+1
                    ISTRIN(N0:N0)=IBASLC
                  ENDIF
                  NOM3=N0-3
                  NOM1=N0-1
                  IF(ISTRIN(NOM3:NOM1).EQ.'BIN')ISTRIN(NOM3:NOM1)='LIB'
                  IF(ISTRIN(NOM3:NOM1).EQ.'bin')ISTRIN(NOM3:NOM1)='lib'
                ENDIF
              ENDIF
C
              N0=N0+1
              ISTRIN(N0:N0+5)='ps2pdf'
              N0=N0+5
              IF(IHOST1.EQ.'IBM-')THEN
                N0=N0+1
                ISTRIN(N0:N0+3)='.bat'
                N0=N0+3
              ENDIF
              IF(IFLSP2.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              N0=N0+1
              ISTRIN(N0:N0)=' '
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
C
              N0=N0+1
              ISTRIN(N0:N0+ILAST-1)=IFILE(1:ILAST)
              N0=N0+ILAST-1
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              ISSAV1=ISYSPE
              ISSAV2=ISYSHI
              ISYSPE='OFF'
              ISYSHI='ON'
              IF(IFLGSP.EQ.1 .OR. IFLSP2.EQ.1)ISYSHI='OFF'
              CALL DPSYS2(ISTRIN,N0,ISUBRO,IERROR)
              ISYSPE=ISSAV1
              ISYSHI=ISSAV2
            ELSE
C
              ISTEPN='224'
              IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
              ISTRIN=' '
              IFLSP2=0
C
C             CHECK FOR SPACE IN NAME
C
              DO2243KK=1,NCGHPA
                IF(IGSTPA(KK:KK).EQ.' ')THEN
                  IFLSP2=1
                  GOTO2249
                ENDIF
 2243         CONTINUE
 2249         CONTINUE
C
              IF(NCGHPA.GT.0)THEN
                IF(IGSTPA(1:1).NE.IQUOTE .AND. IFLSP2.EQ.1)THEN
                  ISTRIN(1:1)=IQUOTE
                  ISTRIN(2:NCGHPA+1)=IGSTPA(1:NCGHPA)
                  N0=NCGHPA+1
                ELSE
                  ISTRIN(1:NCGHPA)=IGSTPA(1:NCGHPA)
                  N0=NCGHPA
                ENDIF
                IF(IGSTPA(NCGHPA:NCGHPA).NE.IBASLC)THEN
                  N0=N0+1
                  ISTRIN(N0:N0)=IBASLC
                ENDIF
              ENDIF
              IF(IOPSY1.EQ.'UNIX')THEN
                N0=N0+1
                ISTRIN(N0:N0+3)='gs  '
                ISTRIN(N0+2:N0+2)=IQUOTE
                N0=N0+2
              ELSEIF(IHOST1.EQ.'IBM-')THEN
                N0=N0+1
                IF(IGSTVR.EQ.'64')THEN
                  ISTRIN(N0:N0+11)='GSWIN64C.EXE'
                  N0=N0+11
                ELSE
                  ISTRIN(N0:N0+11)='GSWIN32C.EXE'
                  N0=N0+11
                ENDIF
                IF(IFLSP2.EQ.1)THEN
                  N0=N0+1
                  ISTRIN(N0:N0)=IQUOTE
                ENDIF
              ENDIF
              N0=N0+1
              IF(IPSTDV.EQ.'PDF')THEN
                ISTRIN(N0:N0+38)=
     1            '-dNOPAUSE -dBATCH -q -sDEVICE=pdfwrite '
                N0=N0+39
              ELSE
                ISTRIN(N0:N0+34)=
     1            '-dNOPAUSE -dBATCH -q -sDEVICE=jpeg '
                N0=N0+35
              ENDIF
              ISTRIN(N0:N0+12)='-sOutputFile='
              N0=N0+12
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              N0=N0+1
              ISTRIN(N0:N0+NCTEMP-1)=IFIL2(1:NCTEMP)
              N0=N0+NCTEMP-1
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              N0=N0+1
              ISTRIN(N0:N0)=' '
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              N0=N0+1
              ISTRIN(N0:N0+ILAST-1)=IFILE(1:ILAST)
              N0=N0+ILAST-1
              IF(IFLGSP.EQ.1)THEN
                N0=N0+1
                ISTRIN(N0:N0)=IQUOTE
              ENDIF
              ISSAV1=ISYSPE
              ISSAV2=ISYSHI
              ISYSPE='OFF'
              ISYSHI='ON'
              IF(IFLGSP.EQ.1 .OR. IFLSP2.EQ.1)ISYSHI='OFF'
              CALL DPSYS2(ISTRIN,N0,ISUBRO,IERROR)
              ISYSPE=ISSAV1
              ISYSHI=ISSAV2
            ENDIF
          ENDIF
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2399)
 2399   FORMAT('<PRE>')
        CALL DPWRST('XXX','WRIT')
C
      ELSEIF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'LATE')THEN
C
        ISTEPN='224'
        IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IMANUF.EQ.'POST' .AND. IDEV.EQ.'PLO1')THEN
          WRITE(ICOUT,3001)IBASLC
 3001     FORMAT(A1,'end{verbatim}')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          IF(IORNSW.EQ.'PORT' .OR. IORNSW.EQ.'LAN2')THEN
            WRITE(ICOUT,3011)IBASLC,IFILE(1:ILAST)
 3011       FORMAT(A1,'PGRAPHIC{',A80,'}')
            CALL DPWRST('XXX','WRIT')
          ELSE
            WRITE(ICOUT,3016)IBASLC,IFILE(1:ILAST)
 3016       FORMAT(A1,'LGRAPHIC{',A80,'}')
            CALL DPWRST('XXX','WRIT')
          ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,3091)IBASLC
 3091     FORMAT(A1,'begin{verbatim}')
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
      ENDIF
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEP3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR
 9014   FORMAT('IERROR= ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE
 9022   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEPP(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IDEFVP,IDEFHP,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1IBUGO2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE NUMBER OF VERTICAL PICTURE POINTS
C              AND HORIZONTAL PICTURE POINTS FOR AN OUTPUT DEVICE.
C              THE NUMBER OF VERTICAL PICTURE POINTS
C              FOR DEVICE I WILL BE PLACED
C              WILL BE PLACED IN THE I-TH ELEMENT OF THE INTEGER
C              VECTOR IDNVPP(.).
C              THE NUMBER OF HORIZONTAL PICTURE POINTS
C              FOR DEVICE I WILL BE PLACED
C              WILL BE PLACED IN THE I-TH ELEMENT OF THE INTEGER
C              VECTOR IDNHPP(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFVP
C                     --IDEFHP
C                     --MAXDEV
C     OUTPUT ARGUMENTS--IDNVPP (AN INTEGER VECTOR
C                              WHOSE I-TH ELEMENT CONTAINS THE
C                              NUMBER OF VERTICAL PICTURE POINTS
C                              FOR DEVICE I.
C                     --IDNHPP (AN INTEGER VECTOR
C                              WHOSE I-TH ELEMENT CONTAINS THE
C                              NUMBER OF HORIZONTAL PICTURE POINTS
C                              FOR DEVICE I.
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
C
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDCOLO
C
      CHARACTER*4 IBUGO2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
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
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
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
      IF(NUMARG.GE.1.AND.IHARG(NUMARG).EQ.'?')GOTO8100
C
      IF(NUMARG.LE.1)GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'PICT'.AND.
     1IHARG(2).EQ.'POIN')GOTO1110
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'PICT'.AND.
     1IHARG(3).EQ.'POIN')GOTO1140
      GOTO1199
C
 1110 CONTINUE
      IF(NUMARG.LE.2)GOTO1120
      IF(IHARG(3).EQ.'ON')GOTO1120
      IF(IHARG(3).EQ.'OFF')GOTO1120
      IF(IHARG(3).EQ.'AUTO')GOTO1120
      IF(IHARG(3).EQ.'DEFA')GOTO1120
      IF(NUMARG.GE.4.AND.IARGT(3).EQ.'NUMB'.AND.
     1IARGT(4).EQ.'NUMB')GOTO1125
      GOTO1199
C
 1120 CONTINUE
      IHOLD1=IDEFHP
      IHOLD2=IDEFVP
      GOTO1130
C
 1125 CONTINUE
      IHOLD1=IARG(3)
      IHOLD2=IARG(4)
      GOTO1130
C
 1130 CONTINUE
      IFOUND='YES'
      DO1135I=1,NUMDEV
      IDNHPP(I)=IHOLD1
      IDNVPP(I)=IHOLD2
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('THE PICTURE POINTS FOR ALL DEVICES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('HAS JUST BEEN SET TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1138)IHOLD1,IHOLD2
 1138 FORMAT(I8,' (HORIZONTAL) BY ',I8,' (VERTICAL)')
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO1199
C
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GOTO1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPDEPP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE DEVICE ... PICTURE POINTS COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE DEVICE IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      DEVICE 3 PICTURE POINTS 781 1024')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXDEV)GOTO1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPDEPP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE DEVICE ... PICTURE POINTS COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF DEVICES MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXDEV
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',
     1'DEVICE.')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1160 CONTINUE
      IF(NUMARG.LE.3)GOTO1170
      IF(IHARG(4).EQ.'ON')GOTO1170
      IF(IHARG(4).EQ.'OFF')GOTO1170
      IF(IHARG(4).EQ.'AUTO')GOTO1170
      IF(IHARG(4).EQ.'DEFA')GOTO1170
      IF(NUMARG.GE.5.AND.IARGT(4).EQ.'NUMB'.AND.
     1IARGT(5).EQ.'NUMB')GOTO1175
      GOTO1199
C
 1170 CONTINUE
      IHOLD1=IDEFHP
      IHOLD2=IDEFVP
      GOTO1180
C
 1175 CONTINUE
      IHOLD1=IARG(4)
      IHOLD2=IARG(5)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IDNHPP(I)=IHOLD1
      IDNVPP(I)=IHOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1199
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)I
 1181 FORMAT('            DEVICE           --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IDUNIT(I)
 1182 FORMAT('            I/O UNIT         --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)IDMANU(I)
 1183 FORMAT('            MANUFACTURER     --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)IDMODE(I),IDMOD2(I),IDMOD3(I)
 1184 FORMAT('            MODEL            --',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1185)IDPOWE(I)
 1185 FORMAT('            POWER            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)IDCONT(I)
 1186 FORMAT('            CONTINUITY       --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1187)IDCOLO(I)
 1187 FORMAT('            COLOR            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IDNHPP(I)
 1188 FORMAT('            HORIZONTAL PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1189)IDNVPP(I)
 1189 FORMAT('            VERTICAL   PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
 1199 CONTINUE
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
      WRITE(ICOUT,8111)
 8111 FORMAT('THE CURRENT NUMBER OF PICTURE POINTS IS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)IDNHPP(1)
 8112 FORMAT('            --HORIZONTAL = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8113)IDNVPP(1)
 8113 FORMAT('            --VERTICAL   = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8121)
 8121 FORMAT('THE DEFAULT NUMBER OF PICTURE POINTS IS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8122)IDEFHP
 8122 FORMAT('            --HORIZONTAL = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8123)IDEFVP
 8123 FORMAT('            --VERTICAL   = ',I8)
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
      SUBROUTINE DPDEPW(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1                  IPL1NU,IPL1NA,
     1                  IPL2NU,IPL2NA,
     1                  IDEFPO,
     1                  NUMDEV,MAXDEV,
     1                  IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IDNVOF,IDNHOF,
     1                  ICAPSW,ICAPNU,
     1                  IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE POWER STATUS (ON/OFF) FOR AN OUTPUT DEVICE.
C              THE POWER (ON/OFF) FOR DEVICE I
C              WILL BE PLACED IN THE I-TH ELEMENT OF THE CHARACTER
C              VECTOR IDPOWE(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFPO
C                     --MAXDEV
C     OUTPUT ARGUMENTS--IDPOWE (A CHARACTER VECTOR
C                              WHOSE I-TH ELEMENT CONTAINS THE
C                              POWER (ON/OFF) FOR DEVICE I.
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
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --MAY       1988.  SEP. UNITS FOR GR & ALPHA I/O (ALAN)
C     UPDATED         --JANUARY   1989.  CALL LIST FOR OFFSET VAR (ALAN)
C     UPDATED         --JANUARY   1989.  DEVICE 2 OFF CASE (ALAN)
C     UPDATED         --MAY       1989.  POSTSCRIPT TRANSLATION FIX (ALAN)
C     UPDATED         --MARCH     1990.  X11 FIX
C     UPDATED         --MAY       1990.  OPEN AS SYNONYM FOR ON, ADD CLOSE
C     UPDATED         --MAY       1992.  IOPERA='CLOS'
C     UPDATED         --MAY       1992.  FIX BUG IGENID WHEN I = 3
C     UPDATED         --MAY       1992.  SKIP MESSAGE FOR DEVICE 3
C     UPDATED         --MAY       1992.  COMMENT OUT ISUBG4 & IBUGG4
C     UPDATED         --JUNE      1992.  DON'T CALL GRINDE FOR ON CASE
C     UPDATED         --OCTOBER   1996.  QWIN PATCH
C     UPDATED         --FEBRUARY  2001.  FOR GD AND GDI DEVICES, DO NOT OPEN OR
C                                        CLOSE OUTPUT FILE (DONE BY UNERLYING C
C                                        CODES), PASS IGDFLG TO DPDEP2
C     UPDATED         --SEPTEMBER 2002.  ICAPSW
C     UPDATED         --AUGUST    2004.  FOR DEVICE 3, CHECK DEFAULT
C                                        COLOR SETTING WHEN DEVICE IS
C                                        POSTSCRIPT
C     UPDATED         --AUGUST    2016.  IF THERE IS A WRITE ERROR AFTER
C                                        CALLING GRINDE, SET POWER
C                                        SWITCH TO "OFF"
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 ICAPSW
C
      CHARACTER*80 IPL1NA
      CHARACTER*80 IPL2NA
C
      CHARACTER*4 IDEFPO
C
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
C
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDCOLO
C
      CHARACTER*4 IANS
      CHARACTER*4 IBUGO2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
      CHARACTER*4 IOPERA
      CHARACTER*4 IGENID
      CHARACTER*4 IGDFLG
      CHARACTER*4 ICAFLG
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
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
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
C
      DIMENSION IDNVOF(*)
      DIMENSION IDNHOF(*)
C
      DIMENSION IANS(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOST.INC'
C
      COMMON/GRAERR/IGFLAG
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
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEPW')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPDEPW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)IBUGO2,IFOUND,IERROR,NUMARG
   60   FORMAT('IBUGO2,IFOUND,IERROR,NUMARG = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO70I=1,NUMARG
         WRITE(ICOUT,71)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I)
   71    FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I) = ',
     1          I8,3(2X,A4),2X,I8)
         CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
C
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'POWE')THEN
        IF(IHARG(2).EQ.'OFF' .OR. IHARG(2).EQ.'CLOS')THEN
          IHOLD='OFF'
        ELSEIF(IHARG(2).EQ.'DEFA')THEN
          IHOLD=IDEFPO
        ELSE
          IHOLD='ON'
        ENDIF
C
        IFOUND='YES'
        DO1135I=1,NUMDEV
          IF(IHOLD.EQ.'ON')THEN
            IF(IDMANU(I).NE.'    ')IDPOWE(I)=IHOLD
          ELSE
            IDPOWE(I)=IHOLD
          ENDIF
 1135   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1136)IHOLD
 1136     FORMAT('THE POWER FOR ALL DEVICES HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'POWE')THEN
        IF(IARGT(1).NE.'NUMB')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
 1141     FORMAT('***** ERROR IN DPDEPW--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
 1142     FORMAT('      IN THE DEVICE ... POWER COMMAND,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
 1143     FORMAT('      THE DEVICE IS IDENTIFIED BY A NUMBER, AS IN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1144)
 1144     FORMAT('      DEVICE 3 POWER ON')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        I=IARG(1)
C
        IF(I.LT.1 .OR. I.GT.MAXDEV)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1152)
 1152     FORMAT('      IN THE DEVICE ... POWER COMMAND, THE NUMBER OF')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1154)MAXDEV
 1154     FORMAT('      DEVICES MUST BE BETWEEN 1 AND ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1155)
 1155     FORMAT('      (INCLUSIVELY); SUCH WAS NOT THE CASE HERE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1156)I
 1156     FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH DEVICE.')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
C       MAY, 1990,  ADD "CLOSE" AND "OPEN"
C       JUNE 1992.  HANDLE "ON" AND "OPEN" DIFFERENTLY
C
      IF(IHARG(3).EQ.'OPEN')THEN
CCCCC   JUNE 1992.  FOLLOWING BLOCK ADDED TO HANDLE OPEN
        IDPOWE(I)='ON'
        IOPERA='OPEN'
        IFOUND='YES'
        IF(I.GT.NUMDEV)NUMDEV=I
        GOTO1179
      ELSEIF(IHARG(3).EQ.'OFF')THEN
        IHOLD='OFF'
        GOTO1180
      ELSEIF(IHARG(3).EQ.'CLOS')THEN
        IDPOWE(I)='OFF'
        IFOUND='YES'
        IF(I.GT.NUMDEV)NUMDEV=I
CCCCC   THE FOLLOWING LINE WAS ADDED   MAY 1992  (JJF)
        IOPERA='CLOS'
        GOTO1179
      ELSEIF(IHARG(3).EQ.'DEFA')THEN
        IHOLD=IDEFPO
        GOTO1180
      ELSE
        IHOLD='ON'
        GOTO1180
      ENDIF
C
 1180 CONTINUE
      IFOUND='YES'
      IF(IHOLD.EQ.'ON')THEN
        IF(IDMANU(I).NE.'    ')THEN
          IDPOWE(I)=IHOLD
        ELSE
          IHOLD='NULL'
          GOTO9000
        ENDIF
      ELSE
        IDPOWE(I)=IHOLD
      ENDIF
C
      IF(I.GT.NUMDEV.AND.IHOLD.EQ.'ON')NUMDEV=I
C
      IOPERA=IDPOWE(I)
C
C     MAY 1990: FOLLOWING LINE ADDED
C
 1179 CONTINUE
C
C     MAY, 1988.  DEFINE SEPARATE UNITS FOR GRAPHICS AND ALPHANUMERIC
C                 OUTPUT.  WILL BE SAME UNIT ON MOST SYSTEMS.  HOWEVER,
C                 SOME SUCH AS CDC NOS/VE REQUIRE DIFFERENT ATTRIBUTES
C                 FOR GRAPHICS OUTPUT.
CCCCC IGENNU=IPR
      IGENNU=IPRGR
C
      IF(I.EQ.1)THEN
        IGENID='SCRE'
        IF(IDMANU(1).EQ.'LATE'.AND.ICAPSW.EQ.'ON')THEN
          IGENNU=ICAPNU
          IPRGR=ICAPNU
        ELSE
          IGENNU=IPRGR
        ENDIF
      ELSEIF(I.EQ.2)THEN
        IGENNU=IPL1NU
        IGENID='PLO1'
      ELSEIF(I.EQ.3)THEN
        IGENNU=IPL2NU
        IGENID='PLO2'
      ELSEIF(I.GE.4)THEN
        IGENNU=IDUNIT(I)
        IGENID='GENE'
      ENDIF
C
      IGDFLG='OFF'
      IF(IDMANU(I).EQ.'GD  '.OR.IDMANU(I).EQ.'GDI ')IGDFLG='ON'
      ICAFLG='OFF'
      IF(IDMANU(I).EQ.'CAIR')ICAFLG='ON'
C
C     MAY,1988 CHANGE.
CCCCC IF(IGENNU.NE.IPR)
CCCCC IF(IGENNU.NE.IPRGR)
      CALL DPDEP2(IOPERA,IGENNU,IGENID,IGDFLG,ICAFLG,
     1            ICAPSW,
     1            IANS,IWIDTH,IBUGO2,ISUBRO,IFOUND,IERROR)
C
CCCCC JUNE 1992.  DON'T CALL GRINDE FOR ON CASE
      IF(IOPERA.EQ.'OPEN')GOTO2000
      IF(IOPERA.EQ.'ON')GOTO2000
      GOTO2090
 2000 CONTINUE
C
CCCCC AUGUST 2004: FOR DEVICE 3 POSTSCRIPT, CHECK FOR DEFAULT
CCCCC COLOR SETTING (IPSTDC).
C
      IF(I.EQ.3 .AND. IDMANU(I).EQ.'POST')THEN
        IF(IPSTDC.EQ.'ON')THEN
           IDCOLO(I)='ON'
           IGCOLO=IDCOLO(I)
        ELSE
           IDCOLO(I)='OFF'
           IGCOLO=IDCOLO(I)
        ENDIF
      ENDIF
C
      IMANUF=IDMANU(I)
      IMODEL=IDMODE(I)
      IGUNIT=IDUNIT(I)
C  AUGUST, 1988.  FOLLOWING LINE ADDED FOR POSTSCRIPT DEVICE
      ANUMVP=IDNVPP(I)
C  MARCH, 1990.  FOLLOWING LINE ADDED FOR X11 DEVICE
      ANUMHP=IDNHPP(I)
CCCCC THE FOLLOWING 2 LINES WERE ADDED           MAY 1989
CCCCC TO FIX POSTSCRIPT TRANSLATION (ALAN)       MAY 1989
      IOFFSV=IDNVOF(I)
      IOFFSH=IDNHOF(I)
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT MAY 1992 (JJF)
CCCCC IBUGG4=IBUGO2
CCCCC JUNE 1992. FOLLOWING LINE MODIFIED
CCCCC CALL GRINDE
      IF(IOPERA.EQ.'OPEN')THEN
         CALL GRINDE
         IF(IGFLAG.EQ.1)IDPOWE(I)='OFF'
      ENDIF
CCCCC THE FOLLOWING THREE LINES ADDED MARCH, 1990 (ALAN).  THE X11
CCCCC DEVICE CAN DYNAMICALLY CHANGE THE NUMBER OF PICTURE POINTS.
      IF(IMANUF.EQ.'X11'.OR.IMANUF.EQ.'QWIN')THEN
        IDNVPP(I)=ANUMVP
        IDNHPP(I)=ANUMHP
      ENDIF
 2090 CONTINUE
C
CCCCC THE FOLLOWING 3 LINES WERE ADDED   MAY 1992 (JJF)
      IF(NUMARG.GE.1)THEN
         IF(IARGT(1).EQ.'NUMB'.AND.IARG(1).EQ.3)GOTO1199
      ENDIF
      IF(IFEEDB.EQ.'OFF')GOTO1199
      IF(I.EQ.1.AND.IDMANU(I).EQ.'LATE'.AND.ICAPSW.EQ.'ON')GOTO1199
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)I
 1181 FORMAT('            DEVICE           --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IDUNIT(I)
 1182 FORMAT('            I/O UNIT         --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)IDMANU(I)
 1183 FORMAT('            MANUFACTURER     --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)IDMODE(I),IDMOD2(I),IDMOD3(I)
 1184 FORMAT('            MODEL            --',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1185)IDPOWE(I)
 1185 FORMAT('            POWER            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)IDCONT(I)
 1186 FORMAT('            CONTINUITY       --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1187)IDCOLO(I)
 1187 FORMAT('            COLOR            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IDNHPP(I)
 1188 FORMAT('            HORIZONTAL PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1189)IDNVPP(I)
 1189 FORMAT('            VERTICAL   PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IF(I.EQ.2)THEN
        WRITE(ICOUT,1192)IPL1NA
 1192   FORMAT('            FILE NAME (LOCAL)--',A80)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(I.EQ.3)THEN
        WRITE(ICOUT,1193)IPL2NA
 1193   FORMAT('            FILE NAME (LOCAL)--',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
 1199 CONTINUE
      GOTO9000
C
      ENDIF
C
 9000 CONTINUE
      IF(IBUGO2.EQ.'ON'.OR.ISUBRO.EQ.'DEPW')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DPDEPW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IBUGO2,IOPERA,IMANUF,IMODEL
 9015   FORMAT('IBUGO2,IOPERA,IMANUF,IMODEL = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IFOUND,IERROR,NUMARG
 9020   FORMAT('IFOUND,IERROR,NUMARG = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO9030I=1,NUMARG
          WRITE(ICOUT,9031)I,IHARG(I),IHARG2(I),IARGT(I),IARG(I)
 9031     FORMAT('I,IHARG(I),IHARG2(I),IARGT(I),IARG(I) = ',
     1           I8,3(2X,A4),2X,I8)
          CALL DPWRST('XXX','BUG ')
 9030   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDERS(NPTS,NLAB,
     1                  AMEAN,ASD,N,
     1                  AMEAN2,ASD2,N2,
     1                  XTEMP1,XTEMP2,XTEMP4,
     1                  SMOOTH,FT,DTEMP1,
     1                  YPLOT,XPLOT,NPLOT,
     1                  XDL,XDLS2,YDL,SEDLK1,SEDLK2,DLOWDL,DHIGDL,
     1                  SERUK1,SERUK2,DLOWD2,DHIGD2,
     1                  SEHDK1,SEHDK2,DLOWD3,DHIGD3,
     1                  SEBOK1,SEBOK2,DLOWD4,DHIGD4,
     1                  DLOWD5,DHIGD5,DLOWD6,DHIGD6,
     1                  AK2,AK3,
     1                  IWRITE,IOUNI5,
     1                  ICAPSW,ICAPTY,NUMDIG,ISEED,IBOOSS,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--IMPLEMENT DERSIMONIAN-LAIRD APPROACH TO CONSENSUS MEANS.
C     PRINTING--YES
C     SUBROUTINES NEEDED--NONE
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/3
C     ORIGINAL VERSION--MARCH     2006.
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --FEBRUARY  2010. USE DPDTA1 TO PRINT
C     UPDATED         --MAY       2010. ADD HORN-HORN-DUNCAN VARIANCE
C                                       ESTIMATOR
C     UPDATED         --MAY       2010. SPLIT INTO 3 DISTINCT
C                                       METHODS BASED ON VARIANCE
C                                       METHOD
C     UPDATED         --OCTOBER   2011. SPECIFY WHICH METHODS TO PRINT
C     UPDATED         --OCTOBER   2011. BOOTSTRAP BASED STANDARD ERROR
C     UPDATED         --JUNE      2012. ADD IBOOSS TO CALL LIST
C     UPDATED         --JULY      2012. ADDITIONAL UPDATES TO BOOTSTRAP
C     UPDATED         --OCTOBER   2014. SUPPORT CASE WHERE AN
C                                       UNCERTAINTY IS GIVEN RATHER
C                                       THAN SD AND SAMPLE SIZE
C     UPDATED         --APRIL     2017. FOR CASE WHERE SAMPLE SIZE IS
C                                       NEGATIVE, BUT NON-ZERO, USE THIS
C                                       AS THE SAMPLE SIZE FOR THE
C                                       BOOTSTRAP
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ISUBRO
      CHARACTER*4 ISUBN0
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASJB
      CHARACTER*4 IBOOFL
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*20 IMETH
C
      REAL AMEAN(*)
      REAL ASD(*)
      REAL AMEAN2(*)
      REAL ASD2(*)
      REAL XTEMP1(*)
      REAL XTEMP2(*)
      REAL XTEMP4(*)
      REAL XPLOT(*)
      REAL YPLOT(*)
C
      REAL APPF
      REAL XDL
      REAL XDLS2
      REAL XDLTMP
      REAL XDLS2T
      REAL AK2
      REAL AK3
      REAL SEDLK1
      REAL SEDLK2
      REAL SERUK1
      REAL SERUK2
      REAL SEHDK1
      REAL SEHDK2
      REAL XPERC
      REAL ALPHAL
      REAL ALPHAU
      REAL P
      REAL AN
      REAL ANI
      REAL REM
      REAL XPERC1
      REAL XPERC2
      REAL AIQ
      REAL ALOW
      REAL AUPP
      REAL ANU
      REAL XSD
C
      INTEGER N(*)
      INTEGER N2(*)
C
      REAL XTEMP3(1)
C
      PARAMETER (MAXLAB=100)
      REAL YLABID(MAXLAB)
C
      DOUBLE PRECISION SMOOTH(*)
      DOUBLE PRECISION FT(*)
      DOUBLE PRECISION DTEMP1(*)
C
C----------------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
C
      PARAMETER (MAXROW=20)
      CHARACTER*65 ITITLE
      CHARACTER*65 ITITLZ
      CHARACTER*65 ITITL9
      CHARACTER*65 ITEXT(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL IFRST
      LOGICAL ILAST
C
      REAL CPUMIN
      REAL CPUMAX
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPDE'
      ISUBN2='RS  '
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDERS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPTS,NLAB,ISEED,IBOOSS
   52   FORMAT('NPTS,NLAB,ISEED,IBOOSS = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO60I=1,NLAB
          WRITE(ICOUT,62)I,N(I),AMEAN(I),ASD(I)
   62     FORMAT('I,N(I),AMEAN(I),ASD(I) = ',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
      ENDIF
C
C     STEP 1: GRAYBILL DEAL ESTIMATE OF MEAN.  THIS
C             WILL BE USED AS AN INITIAL ESTIMATE OF
C             THE CONSENSUS MEAN THAT IS USED TO COMPUTE
C             THE DERSIMONIAN WEIGHTS.
C
C             XGD = SUM[n(i)*xmean(i)/xvar(i)]/SUM[n(i)/xvar(i)]
C
C             IF N(I) <= 0, THEN HAVE UNCERTAINTY CASE.
C
      DO901II=1,NLAB
        IF(N(II).LE.0)THEN
          DTEMP1(II)=DBLE(ASD(II))**2
        ELSE
          DNI=DBLE(N(II))
          DTEMP1(II)=DBLE(ASD(II))**2/DNI
        ENDIF
  901 CONTINUE
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO910I=1,NLAB
        DMEAN=DBLE(AMEAN(I))
        DSUM1=DSUM1 + DMEAN/DTEMP1(I)
        DSUM2=DSUM2 + 1.0D0/DTEMP1(I)
  910 CONTINUE
      XGD=REAL(DSUM1/DSUM2)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
        WRITE(ICOUT,912)XGD,DSUM1,DSUM2
  912   FORMAT('XGD,DSUM1,DSUM2 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     STEP 2: ESTIMATE YDL (ESTIMATE OF BETWEEN LAB VARIANCE)
C
C                YDL = MAX[0,DTERM1/(DTERM2 - DTERM3*DTERM4)]
C
C             WHERE
C
C                DTERM1 = SUM[n(i)*(xmean(i)-xgd)**2/xvari(i)] - NLAB + 1
C                DTERM2 = SUM[n(i)/xvari(i)]
C                DTERM3 = SUM[n(i)**2/xvari(i)**2]
C                DTERM4 = 1/SUM[n(i)/xvari(i)]
C
      DP=DBLE(NLAB)
C
      DTERM2=DSUM2
      DSUM1=0.0D0
      DSUM2=0.0D0
C
      DO920I=1,NLAB
        DMEAN=DBLE(AMEAN(I))
        DVARI=DTEMP1(I)
        DSUM1=DSUM1 + (DMEAN - XGD)**2/DVARI
        DSUM2=DSUM2 + DVARI**2
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
          WRITE(ICOUT,921)I,DMEAN,DVARI,DSUM1,DSUM2
  921     FORMAT('I,DMEAN,DVARI,DSUM1,DSUM2 = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  920 CONTINUE
      DTERM1=DSUM1 - DP + 1.0D0
      DTERM3=DTERM2 - DSUM2/DTERM2
      YDL=MAX(0.0D0,DTERM1/DTERM3)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
        WRITE(ICOUT,926)DTERM1,DTERM2,DTERM3
  926   FORMAT('DTERM1,DTERM2,DTERM3 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,928)YDL,DSUM1,DSUM2
  928   FORMAT('YDL,DSUM1,DSUM2 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     STEP 3: ESTIMATE THE DERSIMONIAN-LAIRD WEIGHTS AND USE
C             THIS TO COMPUTE THE DERSIMONIAN-LAIRD CONSENSUS
C             MEAN.
C
C             STEP 3A: COMPUTE THE SUM OF THE WEIGHTS
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO930I=1,NLAB
        DVARI=DTEMP1(I)
        DSUM1=DSUM1 + 1.0D0/(DVARI + YDL)
        DSUM2=DSUM2 + (1.0D0/(DVARI + YDL))**2
  930 CONTINUE
      DWS=DSUM1
      DWS2=DSUM2
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
        WRITE(ICOUT,931)DWS,DWS2
  931   FORMAT('DWS,DWS2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C             STEP 3B: COMPUTE THE SCALED WEIGHT TIMES THE LAB MEAN
C
      DSUM1=0.0D0
      DSUM2=0.0D0
C
      IF(IOUNI5.GT.0)THEN
        WRITE(IOUNI5,933)
  933   FORMAT('WEIGHTS FROM DERSIMONIAN-LAIRD')
      ENDIF
C
      DO935I=1,NLAB
        DMEAN=DBLE(AMEAN(I))
        DVARI=DTEMP1(I)
        DWI=1.0D0/(DVARI + YDL)
C
        IF(IOUNI5.GT.0)WRITE(IOUNI5,'(E15.7)')DWI
C
        DSUM1=DSUM1 + DMEAN*DWI
        DSUM2=DSUM2 + DWI
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
          WRITE(ICOUT,937)DWS,DWI,DSUM1
  937     FORMAT('DWS,DWI,DSUM1 = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  935 CONTINUE
C
      XDL=DSUM1/DWS
      XDLS2=1.0D0/DSUM2
      SEDLK1=SQRT(XDLS2)
      SEDLK2=2.0D0*SEDLK1
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
        WRITE(ICOUT,938)XDL,XDLS2,SEDLK1
  938   FORMAT('XDL,XDLS2,SEDLK1 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     STEP 4: ESTIMATE THE STANDARD ERROR OF THE DERSIMONIAN-LAIRD
C             CONSENSUS MEAN ESTIMATE
C
      IDF=NLAB-1
      ALPHA=0.975
      CALL TPPF(REAL(ALPHA),REAL(IDF),APPF)
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM3=0.0D0
      DPROD1=1.0D0
      DO940I=1,NLAB
C
        DMEAN=DBLE(AMEAN(I))
        DVARI=DTEMP1(I)
C
        DTERM1=(1.0D0/(DVARI + YDL))**2
        DTERM2=(1.0D0/(DVARI + YDL))
        DOMEGA=DTERM2/DWS
        DSUM2=DSUM2 + DOMEGA*(DMEAN - XDL)**2
        DPROD1=DPROD1*DOMEGA
        DSUM4=0.0D0
        DO945J=1,NLAB
          IF(J.NE.I)THEN
            DMEANJ=DBLE(AMEAN(J))
            IF(N(J).LE.0)THEN
              DVARIJ=DBLE(ASD(J))**2
            ELSE
              DNJ=DBLE(N(J))
              DVARIJ=DBLE(ASD(J))**2/DNJ
            ENDIF
            DTERMJ=1.0D0/(DVARIJ + YDL)
            DSUM4=DSUM4 + DTERMJ
          ENDIF
  945   CONTINUE
        DTERM3=(DMEAN - XDL)**2*DTERM1
        DSUM3=DSUM3 + DTERM3/DSUM4
  940 CONTINUE
C
C     RUKHIN EQUATION 20 FROM 2009 METROLOGIA PAPER
C
      DPP=1.0D0/DBLE(NLAB-1)
      DNUM=DBLE(APPF)*DSQRT(DSUM2)
      DTERM3=(DP**DP)*DPROD1
      DDENOM=(DP-1.0D0)*(DTERM3**DPP)
      SERUK1=REAL(DSQRT(DSUM2)/DSQRT(DDENOM))
      SERUK2=2.0*SERUK1
      DRI=DNUM/DSQRT(DDENOM)
C
C     HORN-DUNCAN VARIANCE ESTIMATE
C
      SEHDK1=REAL(DSQRT(DSUM3/DWS))
      SEHDK2=2.0*SEHDK1
      DHD=DBLE(APPF)*DSQRT(DSUM3/DWS)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
        WRITE(ICOUT,942)XDLS2,DSUM1,SEDLK1,SEDLK2
  942   FORMAT('XDLS2,DSUM1,SEDLK1,SEDLK2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     STEP 4: COMPUTE THE 95% CONFIDENCE INTERVAL FOR THE
C             CONSENSUS MEAN ESTIMATE
C
      DLOWDL=DBLE(XDL - APPF*SQRT(XDLS2))
      DHIGDL=DBLE(XDL + APPF*SQRT(XDLS2))
      DLOWD2=DBLE(XDL) - DRI
      DHIGD2=DBLE(XDL) + DRI
      DLOWD3=DBLE(XDL) - DHD
      DHIGD3=DBLE(XDL) + DHD
C
C     IF REQUESTED, COMPUTE A BOOTSTRAP ESTIMATE OF THE STANDARD ERROR.
C
      IF(IDS4CM.EQ.'ON')THEN
        ICASJB='BOOT'
        NRESAM=IBOOSS
C
C       MODEL BASED RESAMPLING:
C
        IF(IOUNI5.GT.0)THEN
          WRITE(IOUNI5,1132)
 1132     FORMAT('CONSENSUS MEAN ESTIMATES FROM DERSIMONIAN-LAIRD ',
     1           'BOOTSTRAP SAMPLES')
        ENDIF
C
C       NOTE: FOR BOOTSTRAP OPTION NEED EITHER SAMPLE SIZE OR
C             EFFECTIVE DEGREES OF FREEDOM.
C
        IBOOFL='YES'
        DO1110IRESAM=1,NRESAM
          CALL NORRAN(NLAB,ISEED,XTEMP1)
          DO1130IROW=1,NLAB
            IINDX=IROW
            IF(N(IINDX).EQ.0)THEN
              IBOOFL='NO'
              GOTO11610
            ELSEIF(N(IINDX).LT.0)THEN
              AVAL=ASD(IINDX)
              ANU=REAL(ABS(N(IINDX)) - 1) 
            ELSE
              ASQRTN=SQRT(REAL(N(IINDX)))
              AVAL=ASD(IINDX)/ASQRTN
              ANU=REAL(N(IINDX) - 1) 
            ENDIF
            AFACT=SQRT(YDL + (AVAL**2))
            AMEAN2(IROW)=XDL + AFACT*XTEMP1(IINDX)
            NTEMP=1
            CALL CHSRAN(NTEMP,ANU,ISEED,XTEMP3)
            ASD2(IROW)=ASD(IINDX)*SQRT(XTEMP3(1)/ANU)
            N2(IROW)=ABS(N(IINDX))
 1130     CONTINUE
          CALL DPDER2(NPTS,NLAB,
     1                AMEAN2,ASD2,N2,
     1                XDLTMP,XDLS2T,
     1                ISUBRO,IBUGA3,IERROR)
          XTEMP2(IRESAM)=XDLTMP
          DTEMP1(IRESAM)=DBLE(XTEMP2(IRESAM))
          IF(IOUNI5.GT.0)WRITE(IOUNI5,'(E15.7)')XDLTMP
 1110   CONTINUE
C
        CALL SD(XTEMP2,NRESAM,IWRITE,XSD,IBUGA3,IERROR)
        SEBOK1=XSD
        SEBOK2=2.0*XSD
        ALPHAL=100.0*0.025
        ALPHAU=100.0*0.975
C
C       3 METHODS FOR COMPUTING 95% CONFIDENCE INTERVAL:
C
C         1) PERCENTILE (THESE ARE NOT NECESSARILY SYMMETRIC
C
C         2) SYMMETRIC INTERVALS EASY WAY - TAKE LARGER OF
C            (MEAN - 0.025 PERCENTILE) AND (0.975 PERCENTILE - MEAN)
C
C         3) COMPUTE KERNEL DENSITY ESTIMATE, MOVE OUT FROM MEAN
C            IN EQUAL INCREMENTS UNTIL APPROPRIATE COVERAGE REACHED
C
        CALL PERCEN(ALPHAL,XTEMP2,NRESAM,IWRITE,XTEMP1,NRESAM,
     1              XPERC,IBUGA3,IERROR)
        XPERCL=XPERC
        CALL PERCEN(ALPHAU,XTEMP2,NRESAM,IWRITE,XTEMP1,NRESAM,
     1              XPERC,IBUGA3,IERROR)
        XPERCU=XPERC
C
        DLOWD4=DBLE(XPERCL)
        DHIGD4=DBLE(XPERCU)
        WIDTH=MAX(ABS(XDL - XPERCL),ABS(XPERCU  - XDL))
        DLOWD5=DBLE(XDL - WIDTH)
        DHIGD5=DBLE(XDL + WIDTH)
C
        AK2=WIDTH/SEBOK1
C
C       NOW GENERATE THE KERNEL DENSITY TRACE
C
        KFLAG=1
        CALL DSORT(DTEMP1,DTEMP1,NRESAM,KFLAG,IERROR)
        DN=REAL(NRESAM)
        DSUM=0.0D0
        DO11410I=1,NRESAM
          DSUM=DSUM + DTEMP1(I)
11410   CONTINUE
        DMEAN=DSUM/DN
        DSUM=0.0D0
        DO11420I=1,NRESAM
          DX=DTEMP1(I)
          DSUM=DSUM+(DX-DMEAN)**2
11420   CONTINUE
        DVAR=DSUM/(DN-1.0D0)
        DSD=0.0D0
        IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
C
        P=0.25
        AN=REAL(DN)
        ANI=P*(AN+1.0)
        NI=ANI
        A2NI=NI
        REM=ANI-A2NI
        NIP1=NI+1
        IF(NI.LE.1)NI=1
        IF(NI.GE.NRESAM)NI=NRESAM
        IF(NIP1.LE.1)NIP1=1
        IF(NIP1.GE.NRESAM)NIP1=NRESAM
        XPERC1=(1.0-REM)*DTEMP1(NI)+REM*DTEMP1(NIP1)
C
        P=0.75
        ANI=P*(AN+1.0)
        NI=ANI
        A2NI=NI
        REM=ANI-A2NI
        NIP1=NI+1
        IF(NI.LE.1)NI=1
        IF(NI.GE.RESAM)NI=NRESAM
        IF(NIP1.LE.1)NIP1=1
        IF(NIP1.GE.NRESAM)NIP1=NRESAM
        XPERC2=(1.0-REM)*DTEMP1(NI)+REM*DTEMP1(NIP1)
        AIQ=(XPERC2-XPERC1)/1.34
        AIQ=ABS(AIQ)
C
        DH=0.9D0*MIN(DSD,DBLE(AIQ))*DN**(-1.0D0/5.0D0)
        DLO=DTEMP1(1) - 3.0D0*DH
        DHI=DTEMP1(NRESAM) + 3.0D0*DH
C
        ICAL=0
        IKENDP=2048
        IERROR='NO'
       
        CALL DENEST(DTEMP1,NRESAM,DLO,DHI,DH,FT,SMOOTH,IKENDP,ICAL,
     1              IERROR)
        DIFF=CPUMAX
        DO11430I=1,IKENDP
          XTEMP1(I)=REAL(SMOOTH(I))
          XTEMP4(I)=REAL((DBLE(I) - 0.5D0)*(DHI-DLO)/DBLE(IKENDP))
          XTEMP4(I)=REAL(DLO) + XTEMP4(I)
          XPLOT(I)=XTEMP4(I)
          YPLOT(I)=XTEMP1(I)
          DIFFT=ABS(XTEMP4(I) - XDL)
          IF(DIFFT.LT.DIFF)THEN
            IINDX=I
            DIFF=DIFFT
          ENDIF
11430   CONTINUE
        NPLOT=IKENDP
        NUMVAR=2
        CALL CUMINT(XTEMP1,XTEMP4,IKENDP,NUMVAR,IWRITE,XTEMP2,
     1               IBUGA3,IERROR)
C
C       THE CUMULATIVE INTEGRAL IS NOW IN XTEMP2 (AND XTEMP4 IS THE
C       X-COORDINATE).  START FROM XDL VALUE AND MOVE IN EQUAL
C       INCREMENTS UNTIL SUFFICENT COVERAGE IS OBTAINED.
C
        ICNT=0
        DHIGD6=XTEMP4(IKENDP)
        DLOWD6=XTEMP4(1)
        DO11500I=IINDX,IKENDP
          ICNT=ICNT+1
          IUPP=IINDX+ICNT
          ILOW=IINDX-ICNT
          IF(ILOW.LT.1)GOTO11509
          AUPP=1.0 - XTEMP2(IUPP)
          ALOW=XTEMP2(ILOW)
          IF(ALOW+AUPP.LT.0.05)THEN
            DHIGD6=XTEMP4(IUPP)
            DLOWD6=XTEMP4(ILOW)
            GOTO11509
          ENDIF
11500   CONTINUE
11509   CONTINUE
C
        WIDTH=REAL(DHIGD6 - DBLE(XDL))
        AK3=WIDTH/SEBOK1
      ENDIF
C
11610 CONTINUE
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
C
      IF(IDSLCM.EQ.'OFF')GOTO4009
C
      ICNT=1
      ITEXT(ICNT)='4a. Method: DerSimonian Laird (original variance)'
      NCTEXT(ICNT)=49
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='    Estimate of Consensus Mean:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=XDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Estimate of Variance of Consensus Mean:'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=XDLS2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Estimate of Between Lab Variance:'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=YDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Standard Uncertainty (k = 1):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=SEDLK1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Expanded Uncertainty (k = 2):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=SEDLK2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Degrees of Freedom:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=IDF
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='    t Percent Point Value:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=APPF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Lower 95% (t-value) Confidence Limit:'
      NCTEXT(ICNT)=41
      AVALUE(ICNT)=DLOWDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Upper 95% (t-value) Confidence Limit:'
      NCTEXT(ICNT)=41
      AVALUE(ICNT)=DHIGDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Note: DerSimonian-Laird Best Usage:'
      NCTEXT(ICNT)=39
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='          Any Number of Labs:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO310I=1,NUMROW
        NTOT(I)=15
  310 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
 4009 CONTINUE
C
      IF(IDS2CM.EQ.'OFF')GOTO4019
C
      ICNT=1
      ITEXT(ICNT)=
     1 '4b. Method: DerSimonian Laird - Horn-Horn-Duncan Variance'
      NCTEXT(ICNT)=57
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of Consensus Mean:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=XDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of Variance of Consensus Mean:'
      NCTEXT(ICNT)=44
      AVALUE(ICNT)=SEHDK1**2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of Between Lab Variance:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=YDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Standard Uncertainty (k = 1):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=SEHDK1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Expanded Uncertainty (k = 2):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=SEHDK2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Degrees of Freedom:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=IDF
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='     t Percent Point Value:'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=APPF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Lower 95% (t-value) Confidence Limit:'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=DLOWD3
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Upper 95% (t-value) Confidence Limit:'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=DHIGD3
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Note: DerSimonian-Laird Best Usage:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='           Any Number of Labs:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO320I=1,NUMROW
        NTOT(I)=15
  320 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
 4019 CONTINUE
C
      IF(IDS3CM.EQ.'OFF')GOTO4029
      ICNT=1
      ITEXT(ICNT)=
     1 '4c. Method: DerSimonian Laird - Conservative Minmax Variance'
      NCTEXT(ICNT)=60
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of Consensus Mean:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=XDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of Variance of Consensus Mean:'
      NCTEXT(ICNT)=44
      AVALUE(ICNT)=SERUK1**2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of Between Lab Variance:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=YDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Standard Uncertainty (k = 1):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=SERUK1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Expanded Uncertainty (k = 2):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=SERUK2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Degrees of Freedom:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=IDF
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='     t Percent Point Value:'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=APPF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Lower 95% (t-value) Confidence Limit:'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=DLOWD2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Upper 95% (t-value) Confidence Limit:'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=DHIGD2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Note: DerSimonian-Laird Best Usage:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='           Any Number of Labs:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO330I=1,NUMROW
        NTOT(I)=15
  330 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
      ITITL9=' '
      NCTIT9=0
C
 4029 CONTINUE
C
      IF(IDS4CM.EQ.'OFF' .OR. IBOOFL.EQ.'NO')GOTO9000
      ICNT=1
      ITEXT(ICNT)=
     1 '4d. Method: DerSimonian Laird - Bootstrap Variance'
      NCTEXT(ICNT)=50
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='     Number of Bootstrap Samples:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=REAL(NRESAM)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of Consensus Mean:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=XDL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of Variance of Consensus Mean:'
      NCTEXT(ICNT)=44
      AVALUE(ICNT)=SEBOK1**2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Standard Uncertainty (k = 1):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=SEBOK1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Expanded Uncertainty (k = 2):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=SEBOK2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=
     1 '     Lower 95% (percentile bootstrap) Confidence Limit:'
      NCTEXT(ICNT)=55
      AVALUE(ICNT)=DLOWD4
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=
     1 '     Upper 95% (percentile bootstrap) Confidence Limit:'
      NCTEXT(ICNT)=55
      AVALUE(ICNT)=DHIGD4
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=
     1 '     Lower 95% (symmetric bootstrap) Confidence Limit:'
      NCTEXT(ICNT)=54
      AVALUE(ICNT)=DLOWD5
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=
     1 '     Upper 95% (symmetric bootstrap) Confidence Limit:'
      NCTEXT(ICNT)=54
      AVALUE(ICNT)=DHIGD5
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     K (symmetric bootstrap) Coverage Factor:'
      NCTEXT(ICNT)=45
      AVALUE(ICNT)=AK2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=
     1 '     Lower 95% (kernel bootstrap) Confidence Limit:'
      NCTEXT(ICNT)=51
      AVALUE(ICNT)=DLOWD6
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=
     1 '     Upper 95% (kernel bootstrap) Confidence Limit:'
      NCTEXT(ICNT)=51
      AVALUE(ICNT)=DHIGD6
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     K (kernel bootstrap) Coverage Factor:'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=AK3
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Note: DerSimonian-Laird Best Usage:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='           Any Number of Labs:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO340I=1,NUMROW
        NTOT(I)=15
  340 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DERS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDERS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPTS,NLAB
 9013   FORMAT('NPTS,NLAB = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)XDL,XDLS2
 9014   FORMAT('XDL,XDLS2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)DLOWDL,DHIGDL
 9015   FORMAT('DLOWDL,DHIGDL = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDER2(NPTS,NLAB,
     1                  AMEAN,ASD,N,
     1                  XDL,XDLS2,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--IN ORDER TO EASIER IMPLEMENT THE PARAMETERIC BOOTSTRAP
C              ESTIMATE OF THE STANDARD ERROR, EXTRACT THE PART OF
C              THE DPDERS CODE THAT JUST COMPUTES THE POINT ESTIMATE
C              OF THE DERSIMONIAN-LAIRD CONSENSUS MEAN.
C     PRINTING--NO
C     SUBROUTINES NEEDED--NONE
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/10
C     ORIGINAL VERSION--OCTOBER   2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 ISUBN0
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      REAL AMEAN(*)
      REAL ASD(*)
C
      REAL APPF
      REAL XDL
      REAL XDLS2
C
      INTEGER N(*)
C
C----------------------------------------------------------------
C
      REAL CPUMIN
      REAL CPUMAX
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT------------------------------------------------
C
      IERROR='NO'
C
      ISUBN1='DPDE'
      ISUBN2='R2  '
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DER2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDER2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPTS,NLAB
   52   FORMAT('NPTS,NLAB = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO60I=1,NLAB
          WRITE(ICOUT,62)I,AMEAN(I),ASD(I)
   62     FORMAT('I,AMEAN(I),ASD(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
      ENDIF
C
C     STEP 1: GRAYBILL DEAL ESTIMATE OF MEAN.  THIS
C             WILL BE USED AS AN INITIAL ESTIMATE OF
C             THE CONSENSUS MEAN THAT IS USED TO COMPUTE
C             THE DERSIMONIAN WEIGHTS.
C
C             XGD = SUM[n(i)*xmean(i)/xvar(i)]/SUM[n(i)/xvar(i)]
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO910I=1,NLAB
        DMEAN=DBLE(AMEAN(I))
        IF(N(I).EQ.0)THEN
          DVARI=DBLE(ASD(I))**2
        ELSE
          DNI=DBLE(N(I))
          DVARI=DBLE(ASD(I))**2/DNI
        ENDIF
        DSUM1=DSUM1 + DMEAN/DVARI
        DSUM2=DSUM2 + 1.0D0/DVARI
  910 CONTINUE
      XGD=REAL(DSUM1/DSUM2)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DER2')THEN
        WRITE(ICOUT,912)XGD,DSUM1,DSUM2
  912   FORMAT('XGD,DSUM1,DSUM2 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     STEP 2: ESTIMATE YDL (ESTIMATE OF BETWEEN LAB VARIANCE)
C
C                YDL = MAX[0,DTERM1/(DTERM2 - DTERM3*DTERM4)]
C
C             WHERE
C
C                DTERM1 = SUM[n(i)*(xmean(i)-xgd)**2/xvari(i)] - NLAB + 1
C                DTERM2 = SUM[n(i)/xvari(i)]
C                DTERM3 = SUM[n(i)**2/xvari(i)**2]
C                DTERM4 = 1/SUM[n(i)/xvari(i)]
C
      DP=DBLE(NLAB)
C
      DTERM2=DSUM2
      DSUM1=0.0D0
      DSUM2=0.0D0
C
      DO920I=1,NLAB
        DMEAN=DBLE(AMEAN(I))
        IF(N(I).LE.0)THEN
          DVARI=DBLE(ASD(I))**2
        ELSE
          DNI=DBLE(N(I))
          DVARI=DBLE(ASD(I))**2/DNI
        ENDIF
        DSUM1=DSUM1 + (DMEAN - XGD)**2/DVARI
        DSUM2=DSUM2 + (1.0D0/DVARI)**2
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DER2')THEN
          WRITE(ICOUT,921)I,DMEAN,DVARI,DNI,DSUM1,DSUM2
  921     FORMAT('I,DMEAN,DVARI,DNI,DSUM1,DSUM2 = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  920 CONTINUE
      DTERM1=DSUM1 - DP + 1.0D0
      DTERM3=DTERM2 - DSUM2/DTERM2
      YDL=MAX(0.0D0,DTERM1/DTERM3)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DER2')THEN
        WRITE(ICOUT,926)DTERM1,DTERM2,DTERM3
  926   FORMAT('DTERM1,DTERM2,DTERM3 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,928)YDL,DSUM1,DSUM2
  928   FORMAT('YDL,DSUM1,DSUM2 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     STEP 3: ESTIMATE THE DERSIMONIAN-LAIRD WEIGHTS AND USE
C             THIS TO COMPUTE THE DERSIMONIAN-LAIRD CONSENSUS
C             MEAN.
C
C             STEP 3A: COMPUTE THE SUM OF THE WEIGHTS
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO930I=1,NLAB
        IF(N(I).LE.0)THEN
          DVARI=DBLE(ASD(I))**2
        ELSE
          DNI=DBLE(N(I))
          DVARI=DBLE(ASD(I))**2/DNI
        ENDIF
        DSUM1=DSUM1 + 1.0D0/(DVARI + YDL)
        DSUM2=DSUM2 + (1.0D0/(DVARI + YDL))**2
  930 CONTINUE
      DWS=DSUM1
      DWS2=DSUM2
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DER2')THEN
        WRITE(ICOUT,931)DWS,DWS2
  931   FORMAT('DWS,DWS2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C             STEP 3B: COMPUTE THE SCALED WEIGHT TIMES THE LAB MEAN
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO935I=1,NLAB
        DMEAN=DBLE(AMEAN(I))
        IF(N(I).LE.0)THEN
          DVARI=DBLE(ASD(I))**2
        ELSE
          DNI=DBLE(N(I))
          DVARI=DBLE(ASD(I))**2/DNI
        ENDIF
        DWI=1.0D0/(DVARI + YDL)
        DSUM1=DSUM1 + DMEAN*DWI
        DSUM2=DSUM2 + DWI
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DER2')THEN
          WRITE(ICOUT,937)DWS,DWI,DSUM1
  937     FORMAT('DWS,DWI,DSUM1 = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  935 CONTINUE
C
      XDL=DSUM1/DWS
      XDLS2=1.0D0/DSUM2
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DER2')THEN
        WRITE(ICOUT,938)XDL
  938   FORMAT('XDL = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DER2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDER2--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDERV(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,
     1                  IFOUNZ,IBEGIN,IEND,ITYPE,IHOL,IHOL2,
     1                  INT1,FLOAT1,IERRO1,
     1                  NUMCL,NUMPL,NUMAOL,ITYW1L,ICAT1L,INLI1L,ITYW2L,
     1                  NUMCR,NUMPR,NUMAOR,ITYW1R,ICAT1R,INLI1R,ITYW2R,
     1                  IANGLU,RESULT,
     1                  IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--TREAT THE LET CASE FOR
C              FINDING THE DERIVATIVE OF A FUNCTION.
C     NOTE--THE OUTPUT MAY BE THE DERIVATIVE FUNCTION,
C           OR MAY BE THE DERIVATIVE EVALUATE AT A POINT
C           OR AT A SERIES OF POINTS.
C     EXAMPLE--LET A = DERIVATIVE X**3+2*X**2-4*X+5 FOR X = 1
C            --LET X = DERIVATIVE SIN(2*X) WRT X FOR X = 2
C            --LET X = DERIVATIVE SIN(A*B*X+2*C)+E*X**4 WRT X FOR X = Z
C            --LET X = DERIVATIVE F1 WRT X FOR X = 7
C            --LET FUNCTION X = DERIVATIVE F1 WRT X
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --MARCH     1979.
C     UPDATED         --JULY      1981.
C     UPDATED         --JANUARY   1982.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
C     UPDATED         --JULY      2019. CREATE STORAGE FOR RESULT ARRAY
C                                       CALLING ROUTINE
C
C---------------------------------------------------------------------
C
      DIMENSION RESULT(*)
C
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IA
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IFOUNZ
      CHARACTER*4 ITYPE
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IERRO1
C
      CHARACTER*4 ITYW1L
      CHARACTER*4 ICAT1L
      CHARACTER*4 INLI1L
      CHARACTER*4 ITYW2L
      CHARACTER*4 ITYW1R
      CHARACTER*4 ICAT1R
      CHARACTER*4 INLI1R
      CHARACTER*4 ITYW2R
C
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      CHARACTER*4 IDUMV
      CHARACTER*4 IDUMV2
      CHARACTER*4 ILAB
      CHARACTER*4 IOLD
      CHARACTER*4 IOLD2
      CHARACTER*4 INEW
      CHARACTER*4 IFUNC4
C
      CHARACTER*4 ITYPED
      CHARACTER*4 IWD1
      CHARACTER*4 IWD12
      CHARACTER*4 IWD2
      CHARACTER*4 IWD22
      CHARACTER*4 IKEY
      CHARACTER*4 IKEY2
      CHARACTER*4 IHOUT
      CHARACTER*4 IHOUT2
      CHARACTER*4 NEWNAM
      CHARACTER*4 INCLUN
      CHARACTER*4 IOLDNA
      CHARACTER*4 IOLDN2
      CHARACTER*4 MESSAG
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IDUQT1
      CHARACTER*4 IDUMVQ
      CHARACTER*4 INEW2
      CHARACTER*4 IUOUT
      CHARACTER*4 IHXPT1
      CHARACTER*4 IHPARN
      CHARACTER*4 IHPAR2
      CHARACTER*4 ICASEL
      CHARACTER*4 ITTEST
      CHARACTER*4 JUSE
      CHARACTER*4 IERRO2
      CHARACTER*4 IHL
      CHARACTER*4 IHL2
C
      CHARACTER*4 IFOUN3
      CHARACTER*4 IBUGIV
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
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
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
      DIMENSION IA(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
C
      DIMENSION IDUMV(100)
      DIMENSION IDUMV2(100)
      DIMENSION JLOC(100)
C
      DIMENSION ILAB(10)
      DIMENSION IOLD(10)
      DIMENSION IOLD2(10)
      DIMENSION INEW(10)
      DIMENSION INEW2(10)
C
      DIMENSION IFUNC4(1000)
C
C-----COMMON----------------------------------------------------------
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
      ISUBN1='DPDE'
      ISUBN2='RV  '
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
C               *******************************************
C               **  TREAT THE DEFINITE INTEGRAL SUBCASE  **
C               **  OF THE LET COMMAND                   **
C               *******************************************
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DERV')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPDERV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,ISUBRO
   52 FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGCO,IBUGEV
   53 FORMAT('IBUGCO,IBUGEV = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IBUGQ
   54 FORMAT('IBUGQ = ',A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
C
      MAXN2=MAXCHF
      MAXN3=MAXCHF
      MAXN4=MAXCHF
C
C               *******************************************************
C               **  STEP 1.5--                                       **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='1.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ****************************************************************
C               **  STEP 2--                                                   *
C               **  EXAMINE THE LEFT-HAND SIDE--                               *
C               **  IS THE VARIABLE NAME TO LEFT OF = SIGN                     *
C               **  ALREADY IN THE NAME LIST?                                  *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE           *
C               **  OF THE NAME ON THE LEFT.                                   *
C               ****************************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITYPED='V'
      IF(IHARG(1).EQ.'FUNC'.AND.IHARG2(1).EQ.'TION')ITYPED='F'
C
      IF(ITYPED.EQ.'F')IHLEFT=IHARG(2)
      IF(ITYPED.EQ.'F')IHLEF2=IHARG2(2)
      IF(ITYPED.EQ.'V')IHLEFT=IHARG(1)
      IF(ITYPED.EQ.'V')IHLEF2=IHARG2(1)
      DO2000I=1,NUMNAM
      I2=I
      IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))GOTO2100
 2000 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)GOTO2200
      GOTO2900
 2200 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
 2201 FORMAT('***** ERROR IN DPDERV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2202)
 2202 FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, & FUNCTION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2203)MAXNAM
 2203 FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2204)
 2204 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2205)
 2205 FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2206)
 2206 FORMAT('      AND THEN REDEFINE (REUSE) SOME OF THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2207)
 2207 FORMAT('      ALREADY-USED NAMES')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 2100 CONTINUE
      ILISTL=I2
 2900 CONTINUE
C
C               ***************************************************************
C               **  STEP 3--                                                 **
C               **  EXTRACT THE RIGHT-SIDE                                   **
C               **  EXPRESSION FROM THE INPUT COMMAND LINE                   **
C               **  (STARTING WITH THE FIRST NON-BLANK LOCATION AFTER THE    **
C               **  EQUAL SIGN AND ENDING WITH THE END OF THE LINE           **
C               **  OR WITH THE LAST NON-BLANK CHARACTER BEWRTE     FOR  .   **
C               ***************************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWD1='DERI'
      IWD12='VATI'
      IWD2='WRT '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,IFUNC2,N2,
     1IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'YES')GOTO3900
C
      IWD1='DIFF'
      IWD12='EREN'
      IWD2='WRT '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,IFUNC2,N2,
     1IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'YES')GOTO3900
C
      IWD1='PART'
      IWD12='IAL '
      IWD2='WRT '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,IFUNC2,N2,
     1IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'YES')GOTO3900
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3101)
 3101 FORMAT('***** ERROR IN DPDERV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3102)
 3102 FORMAT('      INVALID COMMAND FORM FOR DERIVATIVE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3103)
 3103 FORMAT('      GENERAL FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3104)
 3104 FORMAT('      LET FUNCTION ... = DERIVATIVE ... WRT ... ',
     1'FOR ... = ...')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3105)
 3105 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,3106)(IANS(I),I=1,IWIDTH)
 3106 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 3900 CONTINUE
C
C               ***********************************************************
C               **  STEP 4--                                             **
C               **  DETERMINE IF THE EXPRESSION HAS ANY FUNCTION NAMES   **
C               **  INBEDDED.  IF SO, REPLACE THE FUNCTION NAMES         **
C               **  BY EACH FUNCTION'S DEFINITION.  DO SO REPEATEDLY     **
C               **  UNTIL ALL FUNCTION REFERENCES HAVE BEEN ANNIHILATED  **
C               **  AND THE EXPRESSION IS LEFT ONLY WITH                 **
C               **  CONSTANTS, PARAMETERS, AND VARIABLES--NO FUNCTIONS.  **
C               **  PLACE THE RESULTING FUNCTIONAL EXPRESSION INTO IFUNC3(.) **
C               ***********************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,N3,MAXN3,
     1IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DERV')GOTO5090
C
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
C
      WRITE(ICOUT,5081)IDUMV(1),IDUMV2(1)
 5081 FORMAT('DIFFERENTIATION VARIABLE  = ',A4,A4)
      CALL DPWRST('XXX','BUG ')
C
 5090 CONTINUE
C
C               *************************************
C               **  STEP 5--                       **
C               **  EXTRACT QUALIFIER INFORMATION. **
C               *************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *********************************************************
C               **  STEP 5.1--                                         **
C               **  DETERMINE THE DUMMY VARIABLE FOR THE INTEGRATION.  **
C               *********************************************************
C
      ISTEPN='5.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IKEY='WRT '
      IKEY2='    '
      ISHIFT=1
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1IHARG,IHARG2,NUMARG,
     1INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,IUSE,IN,NUMNAM,
     1IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,VOUT,IUOUT,
     1INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO5119
      IDUMV(1)=IHOUT
      IDUMV2(1)=IHOUT2
      NUMDV=1
      ILOCDV=ILOC2
      GOTO5190
 5119 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5181)
 5181 FORMAT('***** ERROR IN DPDERV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5182)
 5182 FORMAT('      INVALID COMMAND FORM FOR DIFFERENTIATION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5183)
 5183 FORMAT('      NO VARIABLE OF DIFFERENTIATION DEFINED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5185)
 5185 FORMAT('      GENERAL FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5186)
 5186 FORMAT('      LET ... = DERIVATIVE ... WRT ... ',
     1'FOR ... = ... ...')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5187)
 5187 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,5189)(IANS(I),I=1,IWIDTH)
 5189 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 5190 CONTINUE
C
C               *************************************************
C               **  STEP 6.1--                                 **
C               **  DETERMINE THE EXACT ANALYTICAL DERIVATIVE  **
C               **  OF THE FUNCTION.                           **
C               *************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=2
      NUMPAR=0
CCCCC CALL COMPID(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPAR,IANGLE,
CCCCC1IDUMV,IDUMV2,NUMDV,IFUNC4,N4,IBUGA3,IFOUND,IERROR)
      CALL COMPID(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPAR,
     1IDUMV,IDUMV2,NUMDV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,IFUNC4,N4,
     1IBUGCO,IBUGEV,ISUBRO,IERROR)
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DERV')GOTO6139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6121)
 6121 FORMAT('IN DPDERV, AFTER RETURNING FROM COMPID--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6122)IPASS
 6122 FORMAT('IPASS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6123)N3,IFOUND
 6123 FORMAT('N3,IFOUND = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6124)(IFUNC3(I),I=1,N3)
 6124 FORMAT('IFUNC3(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6125)N4
 6125 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6126)(IFUNC4(I),I=1,N4)
 6126 FORMAT('IFUNC4(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6127)NUMPAR
 6127 FORMAT('NUMPAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO6128I=1,NUMPAR
      WRITE(ICOUT,6129)I,IPARN(I),IPARN2(I)
 6129 FORMAT('I,IPARN(I),IPARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6128 CONTINUE
 6139 CONTINUE
C
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *******************************************
C               **  STEP 6.2--                           **
C               **  PRINT OUT A BRIEF MESSAGE            **
C               **  INDICATING WHETHER OR NOT THE        **
C               **  ANALYTIC DERIVATIVE HAS BEEN FOUND,  **
C               **  AND (IF FOUND) GIVING EXPLICITELY    **
C               **  WHAT THE ANALYSTIC FUNCTION IS.      **
C               **  IF NOT FOUND, COPY IFUNC3(.)         **
C               **  INTO IFUNC4(.), AND COPY N3 INTO N4. **
C               *******************************************
C
      ISTEPN='6.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
C
      ILAB(1)='DIFF'
      ILAB(2)='EREN'
      ILAB(3)='TIAT'
      ILAB(4)='ION '
      ILAB(5)='VAR.'
      ILAB(6)='  = '
      NUMWDL=6
CCCCC CALL DPPRIF(ILAB,NUMWDL,IDUMV,1,IBUGA3)
      WRITE(ICOUT,6141)(ILAB(I),I=1,NUMWDL),IDUMV(1),IDUMV2(1)
 6141 FORMAT(20X,6A4,2A4)
      CALL DPWRST('XXX','BUG ')
C
      IF(IFOUND.EQ.'YES')GOTO6219
      IFUNC4(1)='N'
      IFUNC4(2)='O'
      IFUNC4(3)='T'
      IFUNC4(4)=' '
      IFUNC4(5)='F'
      IFUNC4(6)='O'
      IFUNC4(7)='U'
      IFUNC4(8)='N'
      IFUNC4(9)='D'
      N4=9
 6219 CONTINUE
C
      ILAB(1)='DERI'
      ILAB(2)='VATI'
      ILAB(3)='VE F'
      ILAB(4)='UNCT'
      ILAB(5)='ION '
      ILAB(6)='  = '
      NUMWDL=6
      CALL DPPRIF(ILAB,NUMWDL,IFUNC4,N4,IBUGA3)
C
      IF(IFOUND.EQ.'YES')GOTO6290
      IF(N3.LE.0)GOTO6229
      DO6220I=1,N3
      IFUNC4(I)=IFUNC3(I)
 6220 CONTINUE
 6229 CONTINUE
      N4=N3
C
 6290 CONTINUE
C
C               **************************************************************
C               **  STEP 6.3--                                              **
C               **  DISTINGUISH 4 CASES--                                   **
C               **       1.  IF THE OUTPUT IS TO BE A FUNCTION,             **
C               **           AND IF THE ANALYTIC DERIVATIVE WAS NOT FOUND,  **
C               **           THEN EXIT.                                     **
C               **       2.  IF THE OUTPUT IS TO BE A FUNCTION,             **
C               **           AND IF THE ANALYTIC DERIVATIVE WAS FOUND,      **
C               **           THEN SCAN ALL "FOR" QUALIFIERS                 **
C               **           FOR VARIABLE, PARAMETER, FUNCTION,             **
C               **           AND VALUE CHANGES IN THE ANALYTIC DERIVATIVE.  **
C               **       3.  IF THE OUTPUT IS TO BE A VALUE OR VALUES,      **
C               **           AND IF THE ANALYTIC DERIVATIVE WAS NOT FOUND,  **
C               **           THEN SCAN ALL "FOR" QUALIFIERS--               **
C               **           USE THE FIRST "FOR" QUALIFIER                  **
C               **           TO DEFINE THE POINT AT WHICH                   **
C               **           THE DERIVATIVE IS TO BE EVALUATED;             **
C               **           USE THE OTHER "FOR" QUALIFIERS TO DETERMINE    **
C               **           THE VARIABLE, PARAMETER, FUNCTION,             **
C               **           AND VALUE CHANGES IN THE ORIGINAL FUNCTION     **
C               **           (PRIOR TO THE NUMERICAL DIFFERENTIATION).      **
C               **       4.  IF THE OUTPUT IS TO BE A VALUE OR VALUES,      **
C               **           AND IF THE ANALYTIC DERIVATIVE WAS FOUND,      **
C               **           THEN SCAN ALL "FOR" QUALIFIERS--               **
C               **           USE THE FIRST "FOR" QUALIFIER                  **
C               **           TO DEFINE THE POINT AT WHICH                   **
C               **           THE DERIVATIVE IS TO BE EVALUATED;             **
C               **           USE THE OTHER "FOR" QUALIFIERS TO DETERMINE    **
C               **           THE VARIABLE, PARAMETER, FUNCTION,             **
C               **           AND VALUE CHANGES IN THE ANALYTIC DERIVATIVE   **
C               **           (PRIOR TO THE EXACT DIFFERENTIATION).          **
C               **  NOTE--THE OUTPUT FROM THIS SECTION WILL BE THE          **
C               **  UPDATED DERIVATIVE FUNCTION IFUNC4(.) WITH ALL CHANGES  **
C               **  AS DICTATED BY THE VARIOUS 'FOR' QUALIFICATIONS         **
C               **  INCORPORATED DIRECTLY INTO IFUNC4(.)                    **
C               **  WITH THE EXCEPTION OF ANY 'FOR' QUALIFICATION           **
C               **  INVOLVING THE VARIABLE OF DIFFERENTIATION               **
C               **  (SUCH QUALIFICATIONS WILL BE DEALT WITH LATER).         **
C               **************************************************************
C
      ISTEPN='6.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ITYPED.EQ.'F'.AND.IFOUND.EQ.'NO')GOTO9000
C
      NCHANG=0
      ISHIFT=1
      ILOC3=ILOCDV
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=1
      IHXPT1='UNKN'
      IDUMVQ='NO'
      DO6350IFORI=1,10
C
      ILOCA=ILOC3
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1IHARG,IHARG2,NUMARG,
     1INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,IUSE,IN,NUMNAM,
     1IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,VOUT,IUOUT,
     1INOUT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO6380
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO6370
C
      IF(ITYPED.EQ.'V'.AND.IHARG(ILOC2).EQ.IDUMV(1).AND.
     1IHARG2(ILOC2).EQ.IDUMV2(1))GOTO6351
      GOTO6355
C
 6351 CONTINUE
      ILOC3=ILOC1+3
      IF(ILOC3.GT.NUMARG)GOTO6380
      IHXPT1=IHARG(ILOC3)
      ISHIF3=ISHIFT+2
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIF3,ILOCA,ILOCB,
     1IHARG,IHARG2,NUMARG,
     1INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,IUSE,IN,NUMNAM,
     1IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,VOUT,IUOUT,
     1INOUT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO6380
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO6380
      IDUQT1=IUOUT
      IF(IDUQT1.EQ.'N')DUMVV1=VOUT
      IF(IDUQT1.EQ.'P')JLOCQ1=ILOUT
      IF(IDUQT1.EQ.'V')JLOCQ1=ILOUT
      IF(IDUQT1.EQ.'U')GOTO6380
      IDUMVQ='YES'
      GOTO6350
C
 6355 CONTINUE
      ILOC3=ILOC1+3
      IF(ILOC3.GT.NUMARG)GOTO6380
      NCHANG=NCHANG+1
      IOLD(NCHANG)=IHARG(ILOC2)
      IOLD2(NCHANG)=IHARG2(ILOC2)
      INEW(NCHANG)=IHARG(ILOC3)
      INEW2(NCHANG)=IHARG2(ILOC3)
      GOTO6350
C
 6350 CONTINUE
 6370 CONTINUE
      GOTO6390
C
 6380 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6381)
 6381 FORMAT('***** ERROR IN DPDERV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6382)
 6382 FORMAT('      INVALID COMMAND FORM FOR DERIVATIVE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6383)
 6383 FORMAT('      GENERAL FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6384)
 6384 FORMAT('      LET FUNCTION ... = DERIVATIVE ... WRT ... ',
     1'FOR ... = ...')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6385)
 6385 FORMAT('      LET ... = DERIVATIVE ... WRT ... ',
     1'FOR ... = ...')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6386)
 6386 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,6387)(IANS(I),I=1,IWIDTH)
 6387 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 6390 CONTINUE
C
C               **********************************************
C               **  STEP 6.4--                              **
C               **  CARRY OUT THE VARIABLE,                 **
C               **  PARAMETER, AND FUNCTION CHANGES         **
C               **  AND THEN PRINT OUT A BRIEF MESSAGE      **
C               **  INDICATING THAT THE CHANGES             **
C               **  HAVE BEEN MADE.                         **
C               **********************************************
C
      ISTEPN='6.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NCHANG.LE.0)GOTO6490
C
      IF(IPRINT.EQ.'OFF')GOTO6419
      IF(IFEEDB.EQ.'OFF')GOTO6419
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
 6419 CONTINUE
C
      CALL COMPIC(IFUNC4,N4,IOLD,IOLD2,INEW,INEW2,NCHANG,
     1IFUNC4,N4,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IPRINT.EQ.'OFF')GOTO6429
      IF(IFEEDB.EQ.'OFF')GOTO6429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      ILAB(1)='POST'
      ILAB(2)='-CHA'
      ILAB(3)='NGE '
      ILAB(4)='FUNC'
      ILAB(5)='TION'
      ILAB(6)='  = '
      NUMWDL=6
      CALL DPPRIF(ILAB,NUMWDL,IFUNC3,N3,IBUGA3)
 6429 CONTINUE
C
 6490 CONTINUE
C
C               *******************************************************
C               **  STEP 6.5--                                       **
C               **  FOR THE CASE WHEN THE OUTPUT IS A FUNCTION,      **
C               **  DETERMINE IF THE INSERTION  OF THE NEW FUNCTION  **
C               **  INTO THE GENERAL FUNCTION TABLE WOULD OVERFLOW   **
C               **  THE TABLE.  IF NOT, THEN INSERT THE FUNCTION     **
C               **  INTO THE GENERAL FUNCTION TABLE.                 **
C               **  MAKE ADJUSTMENTS TO THE INTERNAL LIST.           **
C               *******************************************************
C
      ISTEPN='6.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ITYPED.EQ.'F'.AND.IFOUND.EQ.'YES')GOTO6519
      GOTO6590
 6519 CONTINUE
C
      CALL DPINFU(IFUNC4,N4,IHNAME,IHNAM2,IUSE,IN,IVSTAR,IVSTOP,
     1NUMNAM,IANS,IWIDTH,IHLEFT,IHLEF2,ILISTL,NEWNAM,MAXN2,
     1IFUNC,NUMCHF,MAXCHF,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
 6590 CONTINUE
C
C               **********************************************
C               **  STEP 6.6--                              **
C               **  FOR THE CASE WHEN THE OUTPUT            **
C               **  IS A FUNCTION,                          **
C               **  PRINT OUT A BRIEF MESSAGE               **
C               **  INDICATING THAT THE FUNCTION            **
C               **  DEFINITION HAS BEEN CARRIED OUT;        **
C               **  THEN EXIT.                              **
C               **********************************************
C
      ISTEPN='6.6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ITYPED.EQ.'F'.AND.IFOUND.EQ.'YES')GOTO6619
      GOTO6690
 6619 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6606)IHLEFT,IHLEF2
 6606 FORMAT('THE NAME ',A4,A4,' HAS JUST BEEN EQUIVALENCED ')
      CALL DPWRST('XXX','BUG ')
      ILAB(1)='TO T'
      ILAB(2)='HE F'
      ILAB(3)='UNCT'
      ILAB(4)='ION '
      ILAB(5)='    '
      ILAB(6)=' -- '
      NUMWDL=6
      CALL DPPRIF(ILAB,NUMWDL,IFUNC4,N4,IBUGA3)
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 6690 CONTINUE
C
C               **********************************************************
C               **  STEP 7--                                            **
C               **  STEPS 7 THROUGH 10 DEAL ONLY                        **
C               **  WITH A DERIVATIVE EVALUATION                        **
C               **  (AS OPPOSED TO A DERIVATIVE FUNCTION).              **
C               **********************************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               **********************************************
C               **  STEP 7.1--                              **
C               **  CARRY OUT THE VARIABLE,                 **
C               **  PARAMETER, AND FUNCTION CHANGES         **
C               **  IN THE ORIGINAL FUNCTION.               **
C               **********************************************
C
      ISTEPN='7.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL COMPIC(IFUNC3,N3,IOLD,IOLD2,INEW,INEW2,NCHANG,
     1IFUNC3,N3,IBUGA3,IERROR)
C
C               **********************************************************
C               **  STEP 7.2--                                          **
C               **  IF THE ANALYTIC DERIVATIVE WAS FOUND, OR            **
C               **  IF THE ANALYTIC DERIVATIVE WAS NOT FOUND, MAKE      **
C               **  A NON-CALCULATING PASS AT THE ORIGINAL   FUNCTION   **
C               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.  **
C               **  NOTE--AT THE END OF THIS STEP,                      **
C               **  NUMPV WILL CONTAIN THE TOTAL NUMBER                 **
C               **  OF PARAMETERS AND VARIABLES IN THE                  **
C               **  ORIGINAL FUNCTION (AFTER CHANGES HAVE BEEN          **
C               **  MADE FOR ALL (EXCEPT THE DUMMY VARIABLE).           **
C               **  HOWEVER, THE DUMMY VARIABLE ITSELF                  **
C               **  WILL BE INCLUDED IN THE COUNT IN NUMPV.             **
C               **  NOTE THAT NUMPV SHOULD ALWAYS BE 1 OR               **
C               **  LARGER (UNLESS THE ORIGINAL FUNCTION                **
C               **  WAS A NUMBER OR COMBINATION OF NUMBERS.             **
C               **  EXAMPLE--SIN(3) WRT X, NUMPV = 0                    **
C               **  EXAMPLE--SIN(X) WRT X, NUMPV = 1                    **
C               **  EXAMPLE--SIN(X) WRT X FOR X=3, NUMPV = 1            **
C               **  EXAMPLE--SIN(A*X) WRT X, NUMPV = 2                  **
C               **  EXAMPLE--SIN(A*X) WRT X FOR X=Y, NUMPV = 2          **
C               **********************************************************
C
      ISTEPN='7.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=1
      CALL COMPIM(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')RETURN
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DERV')GOTO7900
      WRITE(ICOUT,7901)
 7901 FORMAT('IN DPDERV, AFTER RETURNING FROM COMPIM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7902)N4,IPASS
 7902 FORMAT('N4,IPASS = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO7903I=1,N3
      WRITE(ICOUT,7904)I,IFUNC3(I)
 7904 FORMAT('I,IFUNC3(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 7903 CONTINUE
      WRITE(ICOUT,7906)NUMPV
 7906 FORMAT('NUMPV = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO7907I=1,NUMPAR
      WRITE(ICOUT,7908)I,IPARN(I),IPARN2(I)
 7908 FORMAT('I,IPARN(I),IPARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 7907 CONTINUE
 7900 CONTINUE
C
C               ***********************************************
C               **  STEP 8.1--                               **
C               **  MAKE SURE THAT THE DUMMY VARIABLE        **
C               **  APPEARS IN IPARN(. )                     **
C               **  MAKE SURE THAT NUMPV IS 1 OR LARGER.     **
C               **  THIS IS TO BE DONE EVEN THOUGH           **
C               **  THE DUMMY VARIABLE MAY NOT               **
C               **  EXPLICITELY APPEAR                       **
C               **  IN THE ORIGINAL FUNCTION                 **
C               **  (EXAMPLE--SIN(A) WRT X).                 **
C               **  THE ABOVE WILL ASSURE THAT THE FIRST AND **
C               **  LAST POINTS AT WHICH THE DERIVATIVE      **
C               **  IS TO BE EVALUATED WILL IN FACT          **
C               **  BE PRINTED OUT IN STEP 9 BELOW.          **
C               **  -------                                  **
C               **  CHECK THAT ALL PARAMETERS AND VARIABLES  **
C               **  IN THE FUNCTION ARE ALREADY PRESENT      **
C               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
C               **  CHECK ALSO THAT ALL VARIABLES IN THE     **
C               **  FUNCTION        HAVE THE SAME LENGTH     **
C               **  AND THAT THEIR LENGTH IS GREATER THAN    **
C               **  ZERO.                                    **
C               **  IF A 'FOR' QUALIFICATION EXISTS FOR      **
C               **  THE DUMMY VARIABLE, THEN SKIP THE        **
C               **  CHECK FOR THE DUMMY VARIABLE (ONLY)      **
C               **  (BUT DO THE OTHER VARIABLES AND          **
C               **  PARAMETERS).                             **
C               **  IF NO 'FOR' QUALIFICATION EXISTS FOR     **
C               **  THE DUMMY VARIABLE, THEN DO THE          **
C               **  CHECK FOR THE DUMMY VARIABLE AS WELL AS  **
C               **  FOR THE OTHER VARIABLES AND              **
C               **  PARAMETERS.                              **
C               ***********************************************
C
      ISTEPN='8.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IP=0
      IV=0
      NUMEL=0
      IOLDNA='-999'
      IOLDN2='-999'
      IOLDNI=-999
      ITTEST='EITH'
      MESSAG='YES'
C
      IF(NUMPV.LE.0)NUMPV=0
      IF(NUMPV.LE.0)GOTO8590
      DO8500I=1,NUMPV
      IHPARN=IPARN(I)
      IHPAR2=IPARN2(I)
      IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1))GOTO8599
 8500 CONTINUE
 8590 CONTINUE
      NUMPV=NUMPV+1
      IPARN(NUMPV)=IDUMV(1)
      IPARN2(NUMPV)=IDUMV2(1)
 8599 CONTINUE
C
      IF(NUMPV.LE.0)GOTO8670
      DO8600J=1,NUMPV
      I2=I
      IHPARN=IPARN(J)
      IHPAR2=IPARN2(J)
      IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1).AND.
     1IDUMVQ.EQ.'YES')GOTO8600
      CALL CHECN2(IHPARN,IHPAR2,ITTEST,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,NUMNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,
     1JVALUE,AVALUE,JUSE,JN,
     1IOLDNA,IOLDN2,IOLDNI,IFOUN3,IBUGA3,ISUBRO,IERRO2)
      IF(IFOUN3.EQ.'NO')GOTO8650
      IF(IERRO2.EQ.'YES')GOTO8650
      IF(JUSE.EQ.'P')GOTO8610
      IF(JUSE.EQ.'V')GOTO8620
      GOTO8600
C
 8610 CONTINUE
      IP=IP+1
      JLOC(J)=ILOC
      GOTO8600
C
 8620 CONTINUE
      IV=IV+1
      JLOC(J)=ILOC
      IF(IV.EQ.1)JNOLD=JN
      IF(IV.EQ.1)GOTO8627
      IF(IV.GE.2.AND.JN.EQ.JNOLD)GOTO8627
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8621)
 8621 FORMAT('***** ERROR IN DPDERV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8622)
 8622 FORMAT('      NOT ALL VARIABLES INVOLVED IN THE DERIVATIVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8623)
 8623 FORMAT('      EVALUATION HAVE THE SAME LENGTH.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8624)JNOLD
 8624 FORMAT('      PREVIOUS VARIABLES          HAD LENGTH ',I8,'.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8625)IHNAME(ILOC),IHNAM2(ILOC),JN
 8625 FORMAT('      THIS     VARIABLE  (',A4,A4,') HAS LENGTH ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 8627 CONTINUE
      JNOLD=JN
      GOTO8600
C
 8650 CONTINUE
      IERROR='YES'
      GOTO9000
C
 8600 CONTINUE
      GOTO8680
 8670 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8671)IDUMV(1),IDUMV2(1)
 8671 FORMAT('NOTE--VARIABLE OF DIFFERENTIATION (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8672)
 8672 FORMAT('      NOT FOUND IN ORIGINAL FUNCTION.')
      CALL DPWRST('XXX','BUG ')
C
 8680 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DERV')GOTO8690
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8681)
 8681 FORMAT('***** AT THE END OF STEP 8.1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8682)NUMPV,IP,IV
 8682 FORMAT('NUMPV,IP,IV = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO8684I=1,NUMPV
      WRITE(ICOUT,8685)I,JLOC(I)
 8685 FORMAT('I,JLOC(I) = ',I8,2X,I8)
      CALL DPWRST('XXX','BUG ')
 8684 CONTINUE
 8690 CONTINUE
C
C               ***********************************************
C               **  STEP 8.2--                               **
C               **  IF A "FOR" QUALIFIER EXISTS              **
C               **  FOR THE DUMMY VARIABLE,                  **
C               **  CHECK THAT ALL PARAMETERS AND VARIABLES  **
C               **  IN THIS QUALIFICATION ARE ALREADY PRESENT**
C               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
C               **  CHECK ALSO THAT ALL VARIABLES IN THE     **
C               **  THIS QUALIFICATION HAVE THE SAME LENGTH  **
C               **  AND THAT THEIR LENGTH IS GREATER THAN    **
C               **  ZERO.                                    **
C               ***********************************************
C
      ISTEPN='8.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IDUMVQ.EQ.'NO')GOTO8729
C
      IF(IDUQT1.EQ.'N')GOTO8729
C
      IF(IDUQT1.EQ.'P')IP=IP+1
      IF(IDUQT1.EQ.'P')GOTO8729
C
      IF(IDUQT1.EQ.'V')IV=IV+1
      IF(IDUQT1.EQ.'V')JN=IN(JLOCQ1)
      IF(IV.EQ.1)JNOLD=JN
      IF(IV.EQ.1)GOTO8727
      IF(IV.GE.2.AND.JN.EQ.JNOLD)GOTO8727
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8721)
 8721 FORMAT('***** ERROR IN DPDERV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8722)
 8722 FORMAT('      NOT ALL VARIABLES INVOLVED IN THE DERIVATIVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8723)
 8723 FORMAT('      EVALUATION HAVE THE SAME LENGTH.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8724)JNOLD
 8724 FORMAT('      PREVIOUS VARIABLES          HAD LENGTH ',I8,'.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8725)IHNAME(ILOC),IHNAM2(ILOC),JN
 8725 FORMAT('      THIS     VARIABLE  (',A4,A4,') HAS LENGTH ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 8727 CONTINUE
      JNOLD=JN
 8729 CONTINUE
C
      NUMPAR=IP
      NUMVAR=IV
      NUMEL=JN
C
      ICASEL='P'
      IF(NUMVAR.GE.1)ICASEL='V'
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DERV')GOTO8790
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8781)
 8781 FORMAT('***** AT THE END OF STEP 8.2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8782)NUMPV,NUMPAR,NUMVAR,NUMEL
 8782 FORMAT('NUMPV,NUMPAR,NUMVAR,NUMEL = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8783)ICASEL,IDUMV(1),IDUMV2(1),IHXPT1,JLOCQ1
 8783 FORMAT('ICASEL,IDUMV(1),IDUMV2(1),IHXPT1,JLOCQ1 = ',
     1A4,2X,A4,2X,A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 8790 CONTINUE
C
C               *****************************************
C               **  STEP 9--                           **
C               **  EVALUATE THE DERIVATIVE AT THE     **
C               **  SPECIFIED POINT (OR POINTS).       **
C               **  IF THE EXACT ANALYTIC DERIVATIVE   **
C               **  HAD BEEN FOUND,                    **
C               **  DO A FUNCTION EVALUTION ON         **
C               **  THE DERIVATIVE;                    **
C               **  IF THE EXACT ANALYTIC DERIVATIVE   **
C               **  HAD NOT BEEN FOUND (NEVER),        **
C               **  COMPUTE A NUMERICAL DERIVATIVE.    **
C               **  NOTE--FROM STEP 8.1 ABOVE,         **
C               **  NUMPV SHOULD BE 1 OR LARGER,       **
C               **  AND THE DUMMY VARIABLE SHOULD      **
C               **  APPEAR SOMEWHERE IN IPARN(.).      **
C               **  THIS IS TO COVER THE INFREQUENT    **
C               **  CASE OF THE ORIGINAL FUNCTION      **
C               **  NOT CONTAINING THE DUMMY VARIABLE  **
C               **  (AND SO THE DERIVATIVE WILL BE 0)  **
C               **  AND YET WE WANT THE FIRST AND      **
C               **  LAST POINTS WHERE THE EVALUATION   **
C               **  WAS DONE TO BE DONE AND PRINTED    **
C               **  (FOR CONSISTENCY SAKE).            **
C               *****************************************
C
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEL.EQ.'P')IMAX=1
      IF(ICASEL.EQ.'V')IMAX=NUMEL
      DO8810I=1,IMAX
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,8811)I,IMAX,ICASEL,IDUQT1,JLOCQ1,
     1NUMPV
 8811 FORMAT('I,IMAX,ICASEL,IDUQT1,JLOCQ1,NUMPV = ',2I8,
     12X,A4,2X,A4,2I8)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,8812)IDUMV(1),IDUMV2(1),IDUMVQ
 8812 FORMAT('IDUMV(1),IDUMV2(1),IDUMVQ = ',A4,2X,A4,2X,A4)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
      IF(NUMPV.LE.0)GOTO8860
C
      DO8820J=1,NUMPV
      IHPARN=IPARN(J)
      IHPAR2=IPARN2(J)
      IF(IHPARN.NE.IDUMV(1).OR.IHPAR2.NE.IDUMV2(1))GOTO8859
      IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1).AND.
     1IDUMVQ.EQ.'NO')GOTO8821
      IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1).AND.
     1IDUMVQ.EQ.'YES')GOTO8825
C
      IBRAN=8816
      WRITE(ICOUT,8816)
 8816 FORMAT('***** INTERNAL ERROR IN DPDERV SUBROUTINE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8817)IBRAN
 8817 FORMAT('      IMPOSSIBLE CONDITION AT BRANCH POINT ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8818)J,IHPARN,IHPAR2,IDUMV(1),IDUMV2(1),IDUMVQ
 8818 FORMAT('J,IHPARN,IHPAR2,IDUMV(1),IDUMV2(1),IDUMVQ = ',
     1I8,2X,A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 8821 CONTINUE
      KLOC=JLOC(J)
      K2LOC=IVALUE(KLOC)
      IF(IUSE(KLOC).EQ.'P')PARAM(J)=VALUE(KLOC)
CCCCC IF(IUSE(KLOC).EQ.'V')PARAM(J)=V(I,K2LOC)
      IF(IUSE(KLOC).EQ.'V')IJ=MAXN*(K2LOC-1)+I
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.LE.MAXCOL)PARAM(J)=V(IJ)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP1)PARAM(J)=PRED(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP2)PARAM(J)=RES(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP3)PARAM(J)=YPLOT(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP4)PARAM(J)=XPLOT(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP5)PARAM(J)=X2PLOT(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP6)PARAM(J)=TAGPLO(I)
      IF(I.NE.1.AND.I.NE.IMAX)GOTO8820
      IF(ICASEL.EQ.'P')WRITE(ICOUT,999)
      IF(ICASEL.EQ.'P')CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'P')WRITE(ICOUT,8822)PARAM(J)
 8822 FORMAT('EVALUATION POINT      = ',E15.7)
      IF(ICASEL.EQ.'P')CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)WRITE(ICOUT,999)
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)WRITE(ICOUT,8823)PARAM(J)
 8823 FORMAT('FIRST EVALUATION PT.  = ',E15.7)
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.IMAX)WRITE(ICOUT,8824)PARAM(J)
 8824 FORMAT('LAST  EVALUATION PT.  = ',E15.7)
      IF(ICASEL.EQ.'V'.AND.I.EQ.IMAX)CALL DPWRST('XXX','BUG ')
      GOTO8820
C
 8825 CONTINUE
      IF(IDUQT1.EQ.'N')GOTO8830
      IF(IDUQT1.EQ.'P')GOTO8840
      IF(IDUQT1.EQ.'V')GOTO8850
C
      IBRAN=8826
      WRITE(ICOUT,8826)
 8826 FORMAT('***** INTERNAL ERROR IN DPDERV SUBROUTINE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8827)IBRAN
 8827 FORMAT('      IMPOSSIBLE CONDITION AT BRANCH POINT ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8828)J,IHPARN,IHPAR2,IDUMV(1),IDUMV2(1),IDUMVQ,IDUQT1
 8828 FORMAT('J,IHPARN,IHPAR2,IDUMV(1),IDUMV2(1),IDUMVQ,IDUQT1 = ',
     1I8,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 8830 CONTINUE
      PARAM(J)=DUMVV1
      IF(ICASEL.EQ.'P')WRITE(ICOUT,999)
      IF(ICASEL.EQ.'P')CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'P')WRITE(ICOUT,8832)PARAM(J)
 8832 FORMAT('EVALUATION POINT      = ',E15.7)
      IF(ICASEL.EQ.'P')CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)WRITE(ICOUT,999)
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)WRITE(ICOUT,8833)PARAM(J)
 8833 FORMAT('FIRST EVALUATION PT.  = ',E15.7)
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.IMAX)WRITE(ICOUT,8834)PARAM(J)
 8834 FORMAT('LAST  EVALUATION PT.  = ',E15.7)
      IF(ICASEL.EQ.'V'.AND.I.EQ.IMAX)CALL DPWRST('XXX','BUG ')
      GOTO8820
C
 8840 CONTINUE
      KLOC=JLOCQ1
      PARAM(J)=VALUE(KLOC)
      IF(ICASEL.EQ.'P')WRITE(ICOUT,999)
      IF(ICASEL.EQ.'P')CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'P')WRITE(ICOUT,8842)PARAM(J)
 8842 FORMAT('EVALUATION POINT      = ',E15.7)
      IF(ICASEL.EQ.'P')CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)WRITE(ICOUT,999)
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)WRITE(ICOUT,8843)PARAM(J)
 8843 FORMAT('FIRST EVALUATION PT.  = ',E15.7)
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.IMAX)WRITE(ICOUT,8844)PARAM(J)
 8844 FORMAT('LAST  EVALUATION PT.  = ',E15.7)
      IF(ICASEL.EQ.'V'.AND.I.EQ.IMAX)CALL DPWRST('XXX','BUG ')
      GOTO8820
C
 8850 CONTINUE
      KLOC=JLOCQ1
      K2LOC=IVALUE(KLOC)
CCCCC PARAM(J)=V(I,K2LOC)
      IJ=MAXN*(K2LOC-1)+I
      IF(K2LOC.LE.MAXCOL)PARAM(J)=V(IJ)
      IF(K2LOC.EQ.MAXCP1)PARAM(J)=PRED(I)
      IF(K2LOC.EQ.MAXCP2)PARAM(J)=RES(I)
      IF(K2LOC.EQ.MAXCP3)PARAM(J)=YPLOT(I)
      IF(K2LOC.EQ.MAXCP4)PARAM(J)=XPLOT(I)
      IF(K2LOC.EQ.MAXCP5)PARAM(J)=X2PLOT(I)
      IF(K2LOC.EQ.MAXCP6)PARAM(J)=TAGPLO(I)
      IF(ICASEL.EQ.'P')WRITE(ICOUT,999)
      IF(ICASEL.EQ.'P')CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'P')WRITE(ICOUT,8852)PARAM(J)
 8852 FORMAT('EVALUATION POINT      = ',E15.7)
      IF(ICASEL.EQ.'P')CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)WRITE(ICOUT,999)
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)WRITE(ICOUT,8853)PARAM(J)
 8853 FORMAT('FIRST EVALUATION PT.  = ',E15.7)
      IF(ICASEL.EQ.'V'.AND.I.EQ.1)CALL DPWRST('XXX','BUG ')
      IF(ICASEL.EQ.'V'.AND.I.EQ.IMAX)WRITE(ICOUT,8854)PARAM(J)
 8854 FORMAT('LAST  EVALUATION PT.  = ',E15.7)
      IF(ICASEL.EQ.'V'.AND.I.EQ.IMAX)CALL DPWRST('XXX','BUG ')
      GOTO8820
 8859 CONTINUE
C
      KLOC=JLOC(J)
      K2LOC=IVALUE(KLOC)
      IF(IUSE(KLOC).EQ.'P')PARAM(J)=VALUE(KLOC)
CCCCC IF(IUSE(KLOC).EQ.'V')PARAM(J)=V(I,K2LOC)
      IF(IUSE(KLOC).EQ.'V')IJ=MAXN*(K2LOC-1)+I
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.LE.MAXCOL)PARAM(J)=V(IJ)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP1)PARAM(J)=PRED(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP2)PARAM(J)=RES(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP3)PARAM(J)=YPLOT(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP4)PARAM(J)=XPLOT(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP5)PARAM(J)=X2PLOT(I)
      IF(IUSE(KLOC).EQ.'V'.AND.K2LOC.EQ.MAXCP6)PARAM(J)=TAGPLO(I)
C
 8820 CONTINUE
 8860 CONTINUE
C
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,8861)J,PARAM(J),KLOC,K2LOC,IFOUND
 8861 FORMAT('J,PARAM(J),KLOC,K2LOC,IFOUND = ',I8,E15.7,2I8,2X,A4)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,8862)
 8862 FORMAT('IN DPDERV, BEFORE ENTERING COMPIM/DERIVC--')
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
      IF(IFOUND.EQ.'YES')GOTO8871
      GOTO8872
C
 8871 CONTINUE
      IPASS=1
      CALL COMPIM(IFUNC4,N4,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1IBUGCO,IBUGEV,IERROR)
C
      IPASS=2
      CALL COMPIM(IFUNC4,N4,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,CALCD,
     1IBUGCO,IBUGEV,IERROR)
      GOTO8875
C
 8872 CONTINUE
      CALL DERIVC(IFUNC4,N4,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1IDUMV,IDUMV2,NUMDV,X0,CALCD,IBUGA3,IBUGCO,IBUGEV,IERROR)
 8875 CONTINUE
      IF(ICASEL.EQ.'P')RESULP=CALCD
      IF(ICASEL.EQ.'V')RESULT(I)=CALCD
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DERV')GOTO8889
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8881)I
 8881 FORMAT('IN DPDERV, STEP ',I8,' AFTER RETURNING FROM ',
     1'COMPIM/DERIVC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8882)N4,IPASS,CALCD
 8882 FORMAT('N4,IPASS,CALCD = ',2I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8883)(IFUNC4(L),L=1,N4)
 8883 FORMAT('IFUNC4(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8884)NUMPV
 8884 FORMAT('NUMPV = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO8886L=1,NUMPV
      WRITE(ICOUT,8887)L,IPARN(L),IPARN2(L)
 8887 FORMAT('L,IPARN(L),IPARN2(L) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 8886 CONTINUE
 8889 CONTINUE
C
 8810 CONTINUE
C
C               *****************************************
C               **  STEP 10--                          **
C               **  IF THE OUTPUT IS A PARAMETER VALUE,**
C               **  ENTER THE CALCULATED DERIVATIVE    **
C               **  INTO THE DATAPLOT PARAMETER.       **
C               **  IF THE OUTPUT IS A VARIABLE,       **
C               **  ENTER THE CALCULATED DERIVATIVES   **
C               **  INTO THE DATAPLOT ARRAY V(.)     **
C               *****************************************
C
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DERV')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IBUGIV=IBUGA3
C
      IHL=IHLEFT
      IHL2=IHLEF2
      CALL DPINVP(IHL,IHL2,ICASEL,RESULT,NUMEL,RESULP,IJUNK,
     1ISUBN1,ISUBN2,IBUGA3,IERROR)
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DERV')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END OF DPDERV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO
 9012 FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGCO,IBUGEV
 9013 FORMAT('IBUGCO,IBUGEV = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IBUGQ
 9014 FORMAT('IBUGQ = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NUMNAM
      WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)
 9016 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',
     1I8,2X,A4,A4,2X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9017)NUMCHF,MAXCHF,IWIDTH,N2
 9017 FORMAT('NUMCHF,MAXCHF,IWIDTH,N2 = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)(IFUNC(I),I=1,IWIDTH)
 9018 FORMAT('IFUNC(.) = ',115A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)(IFUNC2(I),I=1,N2)
 9019 FORMAT('IFUNC2(.) = ',115A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)N3
 9020 FORMAT('N3 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)(IFUNC3(I),I=1,N3)
 9021 FORMAT('IFUNC3(.) = ',115A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9030)N4
 9030 FORMAT('N4 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)(IFUNC4(I),I=1,N4)
 9031 FORMAT('IFUNC4(.) = ',115A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)NUMPV
 9032 FORMAT('NUMPV = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9033)IP,IV,IDUMV(1),IDUMV2(1),ILOCDV
 9033 FORMAT('IP,IV,IDUMV(1),IDUMV2(1),ILOCDV = ',I8,I8,2X,A4,A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9034)IHLEFT,IHLEF2
 9034 FORMAT('IHLEFT,IHLEF2 = ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9035)IFOUND,IERROR
 9035 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9036)X0,CALCD
 9036 FORMAT('X0,CALCD = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9037)ITYPED,ICASEL,NUMEL
 9037 FORMAT('ITYPED,ICASEL,NUMEL = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDESC(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1                  NUMDEV,MAXDEV,
     1                  IDMANU,IDMODE,IDMOD2,IDMOD3,IDPOWE,
     1                  IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1                  PDSCAL,
     1                  IBUGO2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FONT SCALING FACTOR FOR AN OUTPUT DEVICE.
C              THIS SCALING FACTOR APPLIES TO HARDWARE CHARACTERS
C              ONLY.  DEVICES CAN VARY IN HOW THEY DISPLAY HARDWARE
C              CHARACTERS SO THAT CHARACTER SIZING ON YOUR POSTSCRIPT
C              MAY LOOK GOOD WHILE THE CHARACTER SIZING ON THE
C              SCREEN IS EITHER TOO BIG OR TOO SMALL.  THIS COMMAND
C              MAY SOMETIMES HELP (ALTHOUGH IF A GIVEN DEVICE IS
C              RESTRICTED TO A FEW DISCRETE SIZES IT MAY NOT).
C              THE SCALING FACTOR FOR DEVICE I WILL BE PLACED IN THE
C              I-TH ELEMENT OF THE VECTOR PDSCAL(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --MAXDEV
C     OUTPUT ARGUMENTS--PDSCAL (A REAL VECTOR WHOSE I-TH ELEMENT
C                              CONTAINS THE SCALING FACTOR FOR DEVICE I.
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
C     VERSION NUMBER--2018/12
C     ORIGINAL VERSION--DECEMBER  2018.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGO2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IHARG2(*)
      CHARACTER*4 IARGT(*)
      INTEGER     IARG(*)
      DIMENSION   ARG(*)
C
      CHARACTER*4 IDMANU(*)
      CHARACTER*4 IDMODE(*)
      CHARACTER*4 IDMOD2(*)
      CHARACTER*4 IDMOD3(*)
C
      CHARACTER*4 IDPOWE(*)
      CHARACTER*4 IDCONT(*)
      CHARACTER*4 IDCOLO(*)
      CHARACTER*4 IDFONT(*)
      INTEGER     IDNVPP(*)
      INTEGER     IDNHPP(*)
      INTEGER     IDUNIT(*)
      DIMENSION   PDSCAL(*)
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
      PDEFSC=1.0
      PMINSC=0.2
      PMAXSC=3.0
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SCAL')THEN
        IF(NUMARG.LE.1 .OR. IHARG(2).EQ.'ON')THEN
          PHOLD=PDEFSC
        ELSEIF(IHARG(2).EQ.'OFF' .OR. IHARG(2).EQ.'AUTO' .OR.
     1         IHARG(2).EQ.'DEFA')THEN
          PHOLD=PDEFSC
        ELSE
          IF(IARGT(NUMARG).EQ.'NUMB')THEN
            PHOLD=ARG(NUMARG)
          ELSE
            PHOLD=PDEFSC
          ENDIF
        ENDIF
        IF(PHOLD.LT.PMINSC)PHOLD=PMINSC
        IF(PHOLD.GT.PMAXSC)PHOLD=PMAXSC
        IFOUND='YES'
        DO1135I=1,NUMDEV
          PDSCAL(I)=PHOLD
 1135   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1136)PHOLD
 1136     FORMAT('THE CHARACTER SCALING FOR ALL DEVICES HAS JUST ',
     1           'BEEN SET TO ',F7.2)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'SCAL')THEN
        IF(IARGT(1).EQ.'NUMB')THEN
          I=IARG(1)
          IF(I.LT.1 .OR. I.GT.MAXDEV)THEN
            IERROR='YES'
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1141)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1152)
 1152       FORMAT('      IN THE DEVICE ... SCALE COMMAND,')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1153)
 1153       FORMAT('      THE NUMBER OF DEVICES MUST BE ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1154)MAXDEV
 1154       FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1155)
 1155       FORMAT('      SUCH WAS NOT THE CASE HERE--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1156)I
 1156       FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',
     1             'DEVICE.')
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ELSE
            IF(NUMARG.LE.2 .OR. IHARG(3).EQ.'ON')THEN
              PHOLD=PDEFSC
            ELSEIF(IHARG(3).EQ.'OFF' .OR. IHARG(3).EQ.'AUTO' .OR.
     1             IHARG(3).EQ.'DEFA')THEN
              PHOLD=PDEFSC
            ELSE
              IF(IARGT(NUMARG).EQ.'NUMB')THEN
                PHOLD=ARG(NUMARG)
              ELSE
                PHOLD=PDEFSC
              ENDIF
            ENDIF
            IFOUND='YES'
            IF(PHOLD.LT.PMINSC)PHOLD=PMINSC
            IF(PHOLD.GT.PMAXSC)PHOLD=PMAXSC
            PDSCAL(I)=PHOLD
C
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1181)I
 1181         FORMAT('            DEVICE           --',I4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1182)IDUNIT(I)
 1182         FORMAT('            I/O UNIT         --',I4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1183)IDMANU(I)
 1183         FORMAT('            MANUFACTURER     --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1184)IDMODE(I),IDMOD2(I),IDMOD3(I)
 1184         FORMAT('            MODEL            --',2(A4,2X),A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1185)IDPOWE(I)
 1185         FORMAT('            POWER            --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1186)IDCONT(I)
 1186         FORMAT('            CONTINUITY       --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1187)IDCOLO(I)
 1187         FORMAT('            COLOR           --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1188)IDNHPP(I)
 1188         FORMAT('            HORIZONTAL PIXELS--',I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1189)IDNVPP(I)
 1189         FORMAT('            VERTICAL   PIXELS--',I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1191)IDFONT(I)
 1191         FORMAT('            FONT             --',A4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1193)PDSCAL(I)
 1193         FORMAT('            SCALE            --',F7.2)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            GOTO9000
          ENDIF
        ELSE
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
 1141     FORMAT('***** ERROR IN DPDESC--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
 1142     FORMAT('      IN THE DEVICE ... SCALE COMMAND,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
 1143     FORMAT('      THE DEVICE IS IDENTIFIED BY A NUMBER, AS IN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1144)
 1144     FORMAT('      DEVICE 3 FONT TEKTRONIX')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
      ENDIF
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPDET2(PMIN,PMAX,FMIN,FMAX,
     1ITICSW,ISCASW,
     1NMJT,INMJSW,
     1PTCOOR,ATCOOR,NMJT2,
     1NMNT,INMNSW,
     1PTCOMN,ATCOMN,NMNT2,
     1PTCOFL,PTCOFR,ITICUN)
C  ABOVE LINE ADDED MAY, 1990. (ALAN)
C
C     PURPOSE--DETERMINE AND SET TIC MARKS FOR A SINGLE FRAME LINE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED--JANUARY 1988. (TO ALLOW TIC LABELS WITHOUT TICS)
C     UPDATED       --???     19??. WEIBULL SCALE
C     UPDATED       --MAY     1990. TIC OFFSETS FOR LINEAR & LOG SCALES
C     UPDATED       --JUNE    1990. NORMAL SCALE
C     UPDATED       --JUNE    1994. RESTORE LOST MOD WHERE MINOR TICS
C                                   GO ONE MORE CYCLE WHEN THERE IS AN
C                                   OFFSET.
C     UPDATED       --JULY    1996. ALLOW ONLY 1 CYCLE FOR LOG SCALE
C
C-----NON-COMMON VARIABLES (GRAPHICS)----------------------------------
C
      CHARACTER*4 ITICSW
      CHARACTER*4 ISCASW
      CHARACTER*4 INMJSW
      CHARACTER*4 INMNSW
C FOLLOWING LINE ADDED MAY, 1990.
      CHARACTER*4 ITICUN
C
      DIMENSION PTCOOR(*)
      DIMENSION ATCOOR(*)
      DIMENSION PTCOMN(*)
      DIMENSION ATCOMN(*)
C
      DIMENSION WEIB21(25)
C
CCCCC THE FOLLOWING LINE WAS ADDED JUNE 1990
      DIMENSION ANORM(27)
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
C-----DATA STATEMENTS---------------------------------------------------
C
      DATA WEIB21( 1),WEIB21( 2),WEIB21( 3),WEIB21( 4),WEIB21( 5),
     1     WEIB21( 6),WEIB21( 7),WEIB21( 8),WEIB21( 9),WEIB21(10),
     1     WEIB21(11),WEIB21(12),WEIB21(13),WEIB21(14),WEIB21(15),
     1     WEIB21(16),WEIB21(17),WEIB21(18),WEIB21(19),WEIB21(20),
     1     WEIB21(21)
     1/0.000001,0.00001,0.0001,0.001,0.01,0.1,
     1 0.5,1.0,5.0,10.0,20.0,30.0,40.0,50.0,
     1 60.0,70.0,80.0,90.0,95.0,99.0,99.9/
C
CCCCC THE FOLLOWING DATA STATEMENT WAS ADDED JUNE 1990
      DATA ANORM( 1),ANORM( 2),ANORM( 3),ANORM( 4),ANORM( 5),
     1     ANORM( 6),ANORM( 7),ANORM( 8),ANORM( 9),ANORM(10),
     1     ANORM(11),ANORM(12),ANORM(13),ANORM(14),ANORM(15),
     1     ANORM(16),ANORM(17),ANORM(18),ANORM(19),ANORM(20),
     1     ANORM(21),ANORM(22),ANORM(23),ANORM(24),ANORM(25),
     1     ANORM(26),ANORM(27)
     1/0.000001,0.00001,0.0001,0.001,0.01,0.1,0.5,
     1 1.0,5.0,10.0,20.0,30.0,40.0,
     1 50.0,
     1 60.0,70.0,80.0,90.0,95.0,99.0,
     1 99.5,99.9,99.99,99.999,99.9999,99.99999,99.999999/
C
C-----START POINT-----------------------------------------------------
C
      EPS=0.0001
C
      EXPMIN=0.0
      EXPMAX=0.0
      IEXMIN=0
      IEXMAX=0
      DENOM=0.0
      NUMCYC=0
      NUMCYP=0
      PRANGE=0.0
      J=0
      XIMIN=0.0
      XIMAX=0.0
      NUMMAJ=0
      NUMMIN=0
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'DET2')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPDET2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)PMIN,PMAX
   52 FORMAT('PMIN,PMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)FMIN,FMAX
   53 FORMAT('FMIN,FMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)ITICSW,ISCASW
   54 FORMAT('ITICSW,ISCASW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)NMJT,INMJSW
   55 FORMAT('NMJT,INMJSW = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,56)NMNT,INMNSW
   56 FORMAT('NMNT,INMNSW = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4,ISUBG4,IERRG4
   59 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,70)ITICUN
   70 FORMAT('ITICUN = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)PTCOFL,PTCOFR
   71 FORMAT('PTCOFL,PTCOFR = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
   72 FORMAT('PX1TOR,PX2TOR,PY1TOT,PY2TOT = ',4E15.7)
   90 CONTINUE
C
C               *************************************
C               **  STEP 1--                       **
C               **  TREAT THE    TICS OFF    CASE  **
C               *************************************
C
      NMJT2=0
      NMNT2=0
CCCCC IF(ITICSW.EQ.'OFF')GOTO9000
C
C               ********************************
C               **  STEP 2--                  **
C               **  TREAT THE LOG SCALE CASE  **
C               ********************************
C
      IF(ISCASW.EQ.'LOG')GOTO1200
      GOTO1290
C
C               *******************************
C               **  STEP 2.1--               **
C               **  COMPUTE MAJOR TIC MARKS  **
C               **  FOR THE LOG    CASE      **
C               *******************************
C
 1200 CONTINUE
      IF(FMIN.GT.0.0.AND.FMAX.GT.0.0)GOTO1209
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1201)
 1201 FORMAT('***** ERROR IN DPDET2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1202)
 1202 FORMAT('      A LOG SCALE MAY NOT BE USED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1203)
 1203 FORMAT('      WHEN FRAME LIMITS ARE NON-POSITIVE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1204)FMIN,FMAX
 1204 FORMAT('      THE FRAME LIMITS = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1205)
 1205 FORMAT('      CORRECTIVE ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1206)
 1206 FORMAT('      CHANGE DATA OR CHANGE LIMITS.')
      CALL DPWRST('XXX','BUG ')
      IERRG4='YES'
      GOTO9000
 1209 CONTINUE
C
CCCCC FOLLOWING CODE MODIFIED TO HANDLE CASE WHERE FMIN=FMAX, I.E.,
CCCCC ONLY 1 MAJOR TIC, MINOR TICS HANDLED VIA TIC OFFSET,  JULY 1996.
      EXPMIN=LOG10(FMIN)
      EXPMAX=LOG10(FMAX)
      DENOM=EXPMAX-EXPMIN
C
      IF(FMIN.NE.FMAX)THEN
        IEXMIN=EXPMIN+0.01
        IF(EXPMIN.LT.0.0)IEXMIN=EXPMIN-0.01
        IEXMAX=EXPMAX+0.01
        IF(EXPMAX.LT.0.0)IEXMAX=EXPMAX-0.01
        IF(IEXMAX.EQ.IEXMIN)IEXMAX=IEXMIN+1
C
        NUMCYC=IEXMAX-IEXMIN
        NUMCYP=NUMCYC+1
        PRANGE=PMAX-PMIN
      ELSE
        IEXMIN=EXPMIN+0.01
        IF(EXPMIN.LT.0.0)IEXMIN=EXPMIN-0.01
        IEXMAX=IEXMIN
        NUMCYC=1
      ENDIF
C
C  ALGORITHIM ADJUSTED MAY, 1990 TO SUPPORT TIC OFFSETS.  NOTE THAT
C  OFFSET MAY BE DONE IN EITHER DATAPLOT UNITS OR DATA UNITS.
C
      IF(ITICUN.NE.'ABSO')GOTO1285
C
C  OFFSET IN DATAPLOT UNITS
C
      IF(FMIN.NE.FMAX)THEN
        PMIN2=PMIN+PTCOFL
        PMAX2=PMAX-PTCOFR
        IF(PMIN2.LT.PMAX2)GOTO1283
        PMIN2=PMIN
        PMAX2=PMAX
        PRANG2=PRANGE
        FMIN2=FMIN
        FMAX2=FMAX
        GOTO1289
 1283   CONTINUE
        PRANG2=PMAX2-PMIN2
        FMIN2=FMIN
        FMAX2=FMAX
        ATEMP=(EXPMAX-EXPMIN)/(PMAX2-PMIN2)
        ATMPMX=EXPMAX+(PTCOFR*ATEMP)
        ATMPMN=EXPMIN-(PTCOFL*ATEMP)
        FMAX=10.**ATMPMX
        FMIN=10.**ATMPMN
        GOTO1289
      ELSE
CCCCC   FOR 1 CYCLE CASE, TIC OFFSETS WILL BE CALCULATED IN DATA
CCCCC   UNITS REGARDLESS OF TIC OFFSET UNITS.
      ENDIF
C
C  OFFSET IN DATA UNITS
C
 1285 CONTINUE
      IF(FMIN.NE.FMAX)THEN
        FMIN2=FMIN
        FMAX2=FMAX
        FMIN=FMIN2-PTCOFL
        FMAX=FMAX2+PTCOFR
        IF(FMIN.GT.0.0)GOTO1287
        FMIN=FMIN2
        FMAX=FMAX2
        PMIN2=PMIN
        PMAX2=PMAX
        PRANG2=PMAX2-PMIN2
        GOTO1289
 1287   CONTINUE
        ATMPMN=LOG10(FMIN)
        ATMPMX=LOG10(FMAX)
        ATEMP=(ATMPMX-ATMPMN)/(PMAX-PMIN)
        PTEMP=(EXPMIN-ATMPMN)/ATEMP
        PMIN2=PMIN+PTEMP
        PTEMP=(ATMPMX-EXPMAX)/ATEMP
        PMAX2=PMAX-PTEMP
        PRANG2=PMAX2-PMIN2
        GOTO1289
      ELSE
        IF(PTCOFL.EQ.0.0 .AND. PTCOFR.EQ.0.0)THEN
          FMIN2=FMIN
          FMAX2=FMAX
          FMIN=FMIN2-FMIN/2.
          FMAX=FMAX2+FMAX/2.
        ELSE
          FMIN2=FMIN
          FMAX2=FMAX
          FMIN=FMIN2-PTCOFL
          IF(FMIN.LE.0.0)FMIN=FMIN2-FMIN/2.
          FMAX=FMAX2+PTCOFR
        ENDIF
        ATMPMN=LOG10(FMIN)
        ATMPMX=LOG10(FMAX)
        ATEMP=(ATMPMX-ATMPMN)/(PMAX-PMIN)
        PTEMP=(EXPMIN-ATMPMN)/ATEMP
        PMIN2=PMIN+PTEMP
        PMAX2=PMIN2
        PRANG2=PMAX2-PMIN2
        GOTO1289
      ENDIF
C
 1289 CONTINUE
C
CCCC A COUPLE OF LINES MODIFIED IN FOLLOWING LOOP MAY, 1990.
      IF(FMIN2.EQ.FMAX2)THEN
        PTCOOR(1)=PMIN2
        ATCOOR(1)=EXPMIN
        PTCOOR(2)=PMIN
        ATCOOR(2)=LOG10(FMIN)
        PTCOOR(3)=PMAX
        ATCOOR(3)=LOG10(FMAX)
        NMJT2=3
        GOTO1231
      ENDIF
C
      K=0
      DO1210I=1,NUMCYP
      AI=I
CCCCC ACYCST=FMIN*10.0**(I-1)
      ACYCST=FMIN2*10.0**(I-1)
      K=K+1
      XI=ACYCST
CCCCC XRATIO=(LOG10(XI)-LOG10(FMIN))/DENOM
      XRATIO=(LOG10(XI)-LOG10(FMIN2))/DENOM
CCCCC PTCOOR(K)=PMIN+XRATIO*PRANGE
      PTCOOR(K)=PMIN2+XRATIO*PRANG2
CCCCC ATCOOR(K)=XI
      ATCOOR(K)=EXPMIN+(AI-1.0)
      IF(I.EQ.NUMCYP.AND.J.LE.1)GOTO1229
 1210 CONTINUE
 1229 CONTINUE
      NMJT2=K
 1231 CONTINUE
C
C               *******************************
C               **  STEP 2.2--               **
C               **  COMPUTE MINOR TIC MARKS  **
C               **  FOR THE LOG    CASE      **
C               *******************************
C
CCCCC COUPLE LINES CHANGED MAY, 1990 IN FOLLOWING LOOP.
CCCCC JUNE 1994.  ADD ONE CYCLE OF MINOR TICS WHEN THERE IS A TIC
CCCCC OFFSET.  TWO CASES, AT LOW END AND AT HIGH END.
      IF(FMIN2.EQ.FMAX2)THEN
        K=0
        IF(FMIN2.LE.FMIN)GOTO11259
        ACYST=FMIN2*10.0**(-1)
        DO11250J=2,9
          AJ=J
          XI=AJ*ACYST
          IF(XI.LT.FMIN)GOTO11250
          K=K+1
          XRATIO=(LOG10(FMIN2)-LOG10(XI))/
     1           (LOG10(FMIN2)-LOG10(FMIN))
          PTCOMN(K)=PMIN2-XRATIO*(PMIN2-PMIN)
          ATCOMN(K)=XI
11250   CONTINUE
11259   CONTINUE
        ACYST=FMIN2*10.0**(0)
        DO11260J=2,9
          AJ=J
          XI=AJ*ACYST
          IF(XI.GT.FMAX)GOTO11260
          K=K+1
          XRATIO=(LOG10(XI)-LOG10(FMIN2))/
     1           (LOG10(FMAX)-LOG10(FMIN2))
          PTCOMN(K)=PMIN2+XRATIO*(PMAX-PMIN2)
          ATCOMN(K)=XI
11260   CONTINUE
11269   CONTINUE
        NMNT2=K
        GOTO9000
      ENDIF
C
      K=0
CCCCC JUNE 1994.  CASE FOR LOW END.
      IF(FMIN2.LE.FMIN)GOTO1259
      ACYST=FMIN2*10.0**(-1)
      DO1250J=2,9
      AJ=J
      XI=AJ*ACYST
      IF(XI.LT.FMIN)GOTO1250
      K=K+1
CCCCC XRATIO=(LOG10(XI)-LOG10(FMIN2))/DENOM
      XRATIO=(LOG10(FMIN2)-LOG10(XI))/DENOM
      PTCOMN(K)=PMIN2-XRATIO*PRANG2
      ATCOMN(K)=XI
 1250 CONTINUE
 1259 CONTINUE
C
      DO1260I=1,NUMCYC
CCCCC ACYCST=FMIN*10.0**(I-1)
      ACYCST=FMIN2*10.0**(I-1)
      DO1270J=2,9
      K=K+1
      AJ=J
      XI=AJ*ACYCST
CCCCC XRATIO=(LOG10(XI)-LOG10(FMIN))/DENOM
      XRATIO=(LOG10(XI)-LOG10(FMIN2))/DENOM
CCCCC PTCOMN(K)=PMIN+XRATIO*PRANGE
      PTCOMN(K)=PMIN2+XRATIO*PRANG2
      ATCOMN(K)=XI
      IF(I.EQ.NUMCYP.AND.J.LE.1)GOTO1279
 1270 CONTINUE
 1260 CONTINUE
 1279 CONTINUE
CCCCC JUNE 1994.  CASE FOR HIGH END.
      IF(FMAX2.GE.FMAX)GOTO1299
      ACYST=FMAX2
      DO1280J=2,9
      AJ=J
      XI=AJ*ACYST
      IF(XI.GT.FMAX)GOTO1280
      K=K+1
      XRATIO=(LOG10(XI)-LOG10(FMAX2))/DENOM
      PTCOMN(K)=PMAX2+XRATIO*PRANG2
      ATCOMN(K)=XI
 1280 CONTINUE
 1299 CONTINUE
C
      NMNT2=K
      GOTO9000
C
 1290 CONTINUE
C
C               ************************************
C               **  STEP 3--                      **
C               **  TREAT THE WEIBULL SCALE CASE  **
C               **  NOTE THAT THE COORDINATES WILL GO  **
C               **  FROM 0 TO 100 RATHER THAN FROM THE **
C               **  USUAL 0 TO 1                       **
C               ************************************
C
      IF(ISCASW.EQ.'WEIB')GOTO1300
      GOTO1390
C
C               *******************************
C               **  STEP 2.1--               **
C               **  COMPUTE MAJOR TIC MARKS  **
C               **  FOR THE WEIBULL CASE     **
C               *******************************
C
 1300 CONTINUE
      H=100.0
      IF(0.0.LT.FMIN.AND.FMIN.LT.100.0.AND.
     1   0.0.LT.FMAX.AND.FMAX.LT.100.0)GOTO1309
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1301)
 1301 FORMAT('***** ERROR IN DPDET2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1302)
 1302 FORMAT('      A WEIBULL SCALE MAY NOT BE USED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1303)
 1303 FORMAT('      UNLESS THE FRAME LIMITS ARE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1304)
 1304 FORMAT('      STRICTLY GREATER THAN 0 AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1305)
 1305 FORMAT('      STRICTLY LESS THAN 100.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1306)FMIN,FMAX
 1306 FORMAT('      THE FRAME LIMITS = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1307)
 1307 FORMAT('      CORRECTIVE ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1308)
 1308 FORMAT('      CHANGE DATA OR CHANGE LIMITS.')
      CALL DPWRST('XXX','BUG ')
      IERRG4='YES'
      GOTO9000
 1309 CONTINUE
C
      EXPMIN=LOG(LOG(H/(H-FMIN)))
      EXPMAX=LOG(LOG(H/(H-FMAX)))
      DENOM=EXPMAX-EXPMIN
C
      PRANGE=PMAX-PMIN
      FRANGE=FMAX-FMIN
C
      NUMMAJ=NMJT
      IF(NMJT.LE.16)NUMMAJ=16
      IF(NMJT.GE.21)NUMMAJ=21
C
      K=0
      DO1310I=1,NUMMAJ
      K=K+1
      IP=I+(21-NUMMAJ)
      XI=WEIB21(IP)
      XRATIO=(LOG(LOG(H/(H-XI)))-LOG(LOG(H/(H-FMIN))))/DENOM
      PTCOOR(K)=PMIN+XRATIO*PRANGE
      ATCOOR(K)=XI
 1310 CONTINUE
 1329 CONTINUE
      NMJT2=K
C
C               *******************************
C               **  STEP 2.2--               **
C               **  COMPUTE MINOR TIC MARKS  **
C               **  FOR THE WEIBULL CASE     **
C               *******************************
C
      NMNT2=0
      GOTO9000
C
 1390 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED JUNE 1990
C               ************************************
C               **  STEP 4--                      **
C               **  TREAT THE NORMAL SCALE CASE   **
C               **  NOTE THAT THE COORDINATES WILL GO  **
C               **  FROM 0 TO 100 RATHER THAN FROM THE **
C               **  USUAL 0 TO 1                       **
C               ************************************
C
      IF(ISCASW.EQ.'NORM')GOTO1400
      GOTO1490
C
C               *******************************
C               **  STEP 2.1--               **
C               **  COMPUTE MAJOR TIC MARKS  **
C               **  FOR THE NORMAL CASE      **
C               *******************************
C
 1400 CONTINUE
      H=100.0
      IF(0.0.LT.FMIN.AND.FMIN.LT.100.0.AND.
     1   0.0.LT.FMAX.AND.FMAX.LT.100.0)GOTO1409
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1401)
 1401 FORMAT('***** ERROR IN DPDET2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1402)
 1402 FORMAT('      A NORMAL SCALE MAY NOT BE USED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1403)
 1403 FORMAT('      UNLESS THE FRAME LIMITS ARE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1404)
 1404 FORMAT('      STRICTLY GREATER THAN 0 AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1405)
 1405 FORMAT('      STRICTLY LESS THAN 100.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1406)FMIN,FMAX
 1406 FORMAT('      THE FRAME LIMITS = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1407)
 1407 FORMAT('      CORRECTIVE ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1408)
 1408 FORMAT('      CHANGE DATA OR CHANGE LIMITS.')
      CALL DPWRST('XXX','BUG ')
      IERRG4='YES'
      GOTO9000
 1409 CONTINUE
C
CCCCC EXPMIN=LOG(LOG(H/(H-FMIN)))
      ARG=FMIN/H
      CALL NORPPF(ARG,EXPMIN)
CCCCC EXPMAX=LOG(LOG(H/(H-FMAX)))
      ARG=FMAX/H
      CALL NORPPF(ARG,EXPMAX)
      DENOM=EXPMAX-EXPMIN
C
      PRANGE=PMAX-PMIN
      FRANGE=FMAX-FMIN
C
      NUMMAJ=NMJT
      IF(NMJT.LE.15)NUMMAJ=15
      IF(NMJT.GE.27)NUMMAJ=27
      IHALF=NUMMAJ/2
      I1=14-IHALF
      I2=14+IHALF
      IF(I1.LE.1)I1=1
      IF(I2.GE.NUMMAJ)I2=NUMMAJ
C
      K=0
      DO1410I=1,NUMMAJ
      K=K+1
      IP=I1+(I-1)
      XI=ANORM(IP)
CCCCC XRATIO=(LOG(LOG(H/(H-XI)))-LOG(LOG(H/(H-FMIN))))/DENOM
      ARG1=XI/H
      ARG2=FMIN/H
      CALL NORPPF(ARG1,XOUT1)
      CALL NORPPF(ARG2,XOUT2)
      XRATIO=(XOUT1-XOUT2)/DENOM
      PTCOOR(K)=PMIN+XRATIO*PRANGE
      ATCOOR(K)=XI
 1410 CONTINUE
 1429 CONTINUE
      NMJT2=K
C
C               *******************************
C               **  STEP 2.2--               **
C               **  COMPUTE MINOR TIC MARKS  **
C               **  FOR THE NORMAL CASE      **
C               *******************************
C
      NMNT2=0
      GOTO9000
C
 1490 CONTINUE
C
C               ***********************************
C               **  STEP 38--                    **
C               **  TREAT THE LINEAR SCALE CASE  **
C               ***********************************
C
C               *******************************
C               **  STEP 38.1--              **
C               **  COMPUTE MAJOR TIC MARKS  **
C               **  FOR THE LINEAR CASE      **
C               *******************************
C
 4800 CONTINUE
C
      NUMMAJ=NMJT
      IF(INMJSW.EQ.'FLOA')CALL DPDETN(FMIN,FMAX,NUMMAJ)
C
      ANUMMA=NUMMAJ
      DENOM=ANUMMA-1.0
      PRANGE=PMAX-PMIN
      FRANGE=FMAX-FMIN
C
C  ALGORITHIM ADJUSTED MAY, 1990 TO SUPPORT TIC OFFSETS.  NOTE THAT
C  OFFSET MAY BE DONE IN EITHER DATAPLOT UNITS OR DATA UNITS.
C
      IF(ITICUN.NE.'ABSO')GOTO4805
C
C  OFFSET IN DATAPLOT UNITS
C
      PMIN2=PMIN+PTCOFL
      PMAX2=PMAX-PTCOFR
      IF(PMIN2.LT.PMAX2)GOTO4803
      PMIN2=PMIN
      PMAX2=PMAX
      PRANG2=PRANGE
      FMIN2=FMIN
      FMAX2=FMAX
      FRANG2=FRANGE
      GOTO4809
 4803 CONTINUE
      PRANG2=PMAX2-PMIN2
      FMIN2=FMIN
      FMAX2=FMAX
      FRANG2=FRANGE
      FSCALE=FRANG2/PRANG2
      FMIN=FMIN2-FSCALE*PTCOFL
      FMAX=FMAX2+FSCALE*PTCOFR
      FRANGE=FMAX-FMIN
      GOTO4809
C
C  OFFSET IN DATA UNITS
C
 4805 CONTINUE
      FMIN2=FMIN
      FMAX2=FMAX
      FMIN=FMIN2-PTCOFL
      FMAX=FMAX2+PTCOFR
      FRANG2=FMAX2-FMIN2
      FRANGE=FMAX-FMIN
      PSCALE=PRANGE/FRANGE
      PMIN2=PMIN+PSCALE*PTCOFL
      PMAX2=PMAX-PSCALE*PTCOFR
      PRANG2=PMAX2-PMIN2
      GOTO4809
C
 4809 CONTINUE
C
      K=0
      IF(NUMMAJ.LE.0)GOTO4819
      DO4815I=1,NUMMAJ
      AI=I
      XRATIO=(AI-1.0)/DENOM
      K=K+1
CCCCC PTCOOR(K)=PMIN+XRATIO*PRANGE
CCCCC ATCOOR(K)=FMIN+XRATIO*FRANGE
      PTCOOR(K)=PMIN2+XRATIO*PRANG2
      ATCOOR(K)=FMIN2+XRATIO*FRANG2
      IF(FRANGE.GE.1.AND.
     1(-EPS).LE.ATCOOR(K).AND.
     1ATCOOR(K).LE.EPS)ATCOOR(K)=0.0
 4815 CONTINUE
C
 4819 CONTINUE
      NMJT2=K
C
C               *******************************
C               **  STEP 8.2--               **
C               **  COMPUTE MINOR TIC MARKS  **
C               **  FOR THE LINEAR CASE      **
C               *******************************
C
 4900 CONTINUE
      K=0
C
      XIMIN=ATCOOR(1)
      XIMAX=ATCOOR(2)
C
      NUMMIN=NMNT
      IF(INMNSW.EQ.'FLOA')NUMMIN=1
C
      ANUMMI=NUMMIN
      DENOM=ANUMMI+1.0
C
      NUMMAM=NUMMAJ-1
      IF(NUMMAM.LE.0)GOTO4919
      IF(NMJT2.LE.1)GOTO4919
CCCCC JUNE 1994.  ADD ONE CYCLE OF MINOR TICS WHEN THERE IS A TIC
CCCCC OFFSET.  TWO CASES, AT LOW END AND AT HIGH END.
CCCCC JUNE 1994.  CASE FOR LOW END.
      IF(NUMMIN.LE.0)GOTO4919
      IF(FMIN2.LE.FMIN)GOTO4929
      PRANGE=PTCOOR(2)-PTCOOR(1)
      FRANGE=ATCOOR(2)-ATCOOR(1)
      PSTART=PTCOOR(1)-PRANGE
      FSTART=ATCOOR(1)-FRANGE
      DO4926J=1,NUMMIN
      AJ=J
      XRATIO=AJ/DENOM
      PTTEMP=PSTART+XRATIO*PRANGE
      IF(PTTEMP.LT.PMIN)GOTO4926
      K=K+1
      PTCOMN(K)=PTTEMP
      ATCOMN(K)=FSTART+XRATIO*FRANGE
 4926 CONTINUE
 4929 CONTINUE
C
      DO4915I=1,NUMMAM
      IP1=I+1
      PRANGE=PTCOOR(IP1)-PTCOOR(I)
      FRANGE=ATCOOR(IP1)-ATCOOR(I)
      IF(NUMMIN.LE.0)GOTO4919
      DO4916J=1,NUMMIN
      AJ=J
      XRATIO=AJ/DENOM
      K=K+1
      PTCOMN(K)=PTCOOR(I)+XRATIO*PRANGE
      ATCOMN(K)=ATCOOR(I)+XRATIO*FRANGE
 4916 CONTINUE
 4915 CONTINUE
C
CCCCC JUNE 1994.  CASE FOR HIGH END.
      IF(FMAX2.GE.FMAX)GOTO4939
      PRANGE=PTCOOR(2)-PTCOOR(1)
      FRANGE=ATCOOR(2)-ATCOOR(1)
      DO4936J=1,NUMMIN
      AJ=J
      XRATIO=AJ/DENOM
      PTTEMP=PTCOOR(NUMMAJ)+XRATIO*PRANGE
      IF(PTTEMP.GT.PMAX)GOTO4939
      K=K+1
      PTCOMN(K)=PTTEMP
      ATCOMN(K)=ATCOOR(NUMMAJ)+XRATIO*FRANGE
 4936 CONTINUE
 4939 CONTINUE
C
 4919 CONTINUE
      NMNT2=K
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'DET2')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPDET2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)PMIN,PMAX
 9012 FORMAT('PMIN,PMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)FMIN,FMAX
 9013 FORMAT('FMIN,FMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ITICSW,ISCASW
 9014 FORMAT('ITICSW,ISCASW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NMJT,INMJSW,NUMMAJ
 9015 FORMAT('NMJT,INMJSW,NUMMAJ = ',I8,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)NMNT,INMNSW
 9016 FORMAT('NMNT,INMNSW = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)NMJT2
 9021 FORMAT('NMJT2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)FMIN,PMIN
 9022 FORMAT('  FMIN     ,PMIN      = ',8X,2E15.7)
      CALL DPWRST('XXX','BUG ')
      IF(NMJT2.LE.0)GOTO9029
      DO9023I=1,NMJT2
      WRITE(ICOUT,9024)I,ATCOOR(I),PTCOOR(I)
 9024 FORMAT('I,ATCOOR(I),PTCOOR(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9023 CONTINUE
      WRITE(ICOUT,9025)FMAX,PMAX
 9025 FORMAT('  FMAX     ,PMAX      = ',8X,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9029 CONTINUE
      WRITE(ICOUT,9031)NMNT2
 9031 FORMAT('NMNT2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)FMIN,PMIN
 9032 FORMAT('  FMIN     ,PMIN      = ',8X,2E15.7)
      CALL DPWRST('XXX','BUG ')
      IF(NMNT2.LE.0)GOTO9039
      DO9033I=1,NMNT2
      WRITE(ICOUT,9034)I,ATCOMN(I),PTCOMN(I)
 9034 FORMAT('I,ATCOMN(I),PTCOMN(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9033 CONTINUE
      WRITE(ICOUT,9035)FMAX,PMAX
 9035 FORMAT('  FMAX     ,PMAX      = ',8X,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9039 CONTINUE
      WRITE(ICOUT,9041)EXPMIN,EXPMAX,DENOM
 9041 FORMAT('EXPMIN,EXPMAX,DENOM = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9042)IEXMIN,IEXMAX
 9042 FORMAT('IEXMIN,IEXMAX = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9043)NUMCYC,NUMCYP,PRANGE
 9043 FORMAT('NUMCYC,NUMCYP,PRANGE = ',2I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9044)NUMMAJ
 9044 FORMAT('NUMMAJ = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9045)NUMMIN
 9045 FORMAT('NUMMIN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9047)XIMIN,XIMAX
 9047 FORMAT('XIMIN,XIMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9049)IBUGG4,ISUBG4,IERRG4
 9049 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9052)PMIN2,PMAX2
 9052 FORMAT('PMIN2,PMAX2 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9053)FMIN2,FMAX2
 9053 FORMAT('FMIN2,FMAX2 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9054)FRANG2,PRANG2
 9054 FORMAT('FRANG2,PRANG2 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9055)ITICUN
 9055 FORMAT('ITICUN = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9056)PTCOFL,PTCOFR
 9056 FORMAT('PTCOFL,PTCOFR = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDETM(PXMIN,PYMIN,PXMAX,PYMAX,
     1ICASPL,ICAS3D,
     1FX1MIN,FX1MAX,FY1MIN,FY1MAX,
     1FX2MIN,FX2MAX,FY2MIN,FY2MAX,
     1IX1TSW,IX2TSW,IY1TSW,IY2TSW,
     1IX1JSW,IX2JSW,IY1JSW,IY2JSW,
     1NMJX1T,NMJX2T,NMJY1T,NMJY2T,
     1IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1PX1COO,PX2COO,PY1COO,PY2COO,
     1X1COOR,X2COOR,Y1COOR,Y2COOR,
     1NX1COO,NX2COO,NY1COO,NY2COO,
     1IX1NSW,IX2NSW,IY1NSW,IY2NSW,
     1NMNX1T,NMNX2T,NMNY1T,NMNY2T,
     1PX1CMN,PX2CMN,PY1CMN,PY2CMN,
     1X1COMN,X2COMN,Y1COMN,Y2COMN,
     1NX1CMN,NX2CMN,NY1CMN,NY2CMN,
     1PX1TOL,PX2TOL,PY1TOB,PY2TOB,
     1PX1TOR,PX2TOR,PY1TOT,PY2TOT,
     1ITICUN)
CCCC ABOVE 3 LINES ADDED TO CALL SEQUENCE MAY, 1990 (TO ADD TIC OFFSETS)
C
C     PURPOSE--DETERMINE AND SET TIC MARKS
C             ON ALL 4 FRAME LINES.
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED--MAY        1990.  ADD SUPPORT FOR TIC OFFSETS
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAS3D
C
      CHARACTER*4 IX1TSW
      CHARACTER*4 IX2TSW
      CHARACTER*4 IY1TSW
      CHARACTER*4 IY2TSW
C
      CHARACTER*4 IX1JSW
      CHARACTER*4 IX2JSW
      CHARACTER*4 IY1JSW
      CHARACTER*4 IY2JSW
C
      CHARACTER*4 IX1NSW
      CHARACTER*4 IX2NSW
      CHARACTER*4 IY1NSW
      CHARACTER*4 IY2NSW
C
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
C FOLLOWING LINE ADDED MAY, 1990.
      CHARACTER*4 ITICUN
C
      DIMENSION PX1COO(*)
      DIMENSION PX2COO(*)
      DIMENSION PY1COO(*)
      DIMENSION PY2COO(*)
C
      DIMENSION X1COOR(*)
      DIMENSION X2COOR(*)
      DIMENSION Y1COOR(*)
      DIMENSION Y2COOR(*)
C
      DIMENSION PX1CMN(*)
      DIMENSION PX2CMN(*)
      DIMENSION PY1CMN(*)
      DIMENSION PY2CMN(*)
C
      DIMENSION X1COMN(*)
      DIMENSION X2COMN(*)
      DIMENSION Y1COMN(*)
      DIMENSION Y2COMN(*)
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
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'DETM')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPDETM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)PXMIN,PYMIN,PXMAX,PYMAX
   52 FORMAT('PXMIN,PYMIN,PXMAX,PYMAX = ',4F10.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)FX1MIN,FX1MAX,FY1MIN,FY1MAX
   53 FORMAT('FX1MIN,FX1MAX,FY1MIN,FY1MAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)FX2MIN,FX2MAX,FY2MIN,FY2MAX
   54 FORMAT('FX2MIN,FX2MAX,FY2MIN,FY2MAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,56)IX1TSW,IX2TSW,IY1TSW,IY2TSW
   56 FORMAT('IX1TSW,IX2TSW,IY1TSW,IY2TSW = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,57)IX1JSW,IX2JSW,IY1JSW,IY2JSW
   57 FORMAT('IX1JSW,IX2JSW,IY1JSW,IY2JSW = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,58)NMJX1T,NMJX2T,NMJY1T,NMJY2T
   58 FORMAT('NMJX1T,NMJX2T,NMJY1T,NMJY2T = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)IX1NSW,IX2NSW,IY1NSW,IY2NSW
   61 FORMAT('IX1NSW,IX2NSW,IY1NSW,IY2NSW = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)NMNX1T,NMNX2T,NMNY1T,NMNY2T
   62 FORMAT('NMNX1T,NMNX2T,NMNY1T,NMNY2T = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)ICASPL,ICAS3D
   63 FORMAT('ICASPL,ICAS3D = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,70)ITICUN
   70 FORMAT('ITICUN = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)PX1TOL,PX2TOL,PY1TOB,PY2TOB
   71 FORMAT('PX1TOL,PX2TOL,PY1TOB,PY2TOB = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,72)PX1TOR,PX2TOR,PY1TOT,PY2TOT
   72 FORMAT('PX1TOR,PX2TOR,PY1TOT,PY2TOT = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE MAJOR TIC MARKS ON BOTTOM HORIZONTAL AXIS  **
C               ******************************************************
C
      CALL DPDET2(PXMIN,PXMAX,FX1MIN,FX1MAX,
     1IX1TSW,IX1TSC,
     1NMJX1T,IX1JSW,
     1PX1COO,X1COOR,NX1COO,
     1NMNX1T,IX1NSW,
     1PX1CMN,X1COMN,NX1CMN,
     1PX1TOL,PX1TOR,ITICUN)
C  ABOVE LINE ADDED MAY, 1990.
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 2--                                        **
C               **  DETERMINE MAJOR TIC MARKS ON TOP  HORIZONTAL   AXIS  **
C               ******************************************************
C
      CALL DPDET2(PXMIN,PXMAX,FX2MIN,FX2MAX,
     1IX2TSW,IX2TSC,
     1NMJX2T,IX2JSW,
     1PX2COO,X2COOR,NX2COO,
     1NMNX2T,IX2NSW,
     1PX2CMN,X2COMN,NX2CMN,
     1PX2TOL,PX2TOR,ITICUN)
C  ABOVE LINE ADDED MAY, 1990.
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  DETERMINE MAJOR TIC MARKS ON LEFT    VERTICAL AXIS  **
C               ******************************************************
C
      CALL DPDET2(PYMIN,PYMAX,FY1MIN,FY1MAX,
     1IY1TSW,IY1TSC,
     1NMJY1T,IY1JSW,
     1PY1COO,Y1COOR,NY1COO,
     1NMNY1T,IY1NSW,
     1PY1CMN,Y1COMN,NY1CMN,
     1PY1TOB,PY1TOT,ITICUN)
C  ABOVE LINE ADDED MAY, 1990.
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 4--                                        **
C               **  DETERMINE MAJOR TIC MARKS ON RIGHT   VERTICAL   AXIS  **
C               ******************************************************
C
      CALL DPDET2(PYMIN,PYMAX,FY2MIN,FY2MAX,
     1IY2TSW,IY2TSC,
     1NMJY2T,IY2JSW,
     1PY2COO,Y2COOR,NY2COO,
     1NMNY2T,IY2NSW,
     1PY2CMN,Y2COMN,NY2CMN,
     1PY2TOB,PY2TOT,ITICUN)
C  ABOVE LINE ADDED MAY, 1990.
      IF(IERRG4.EQ.'YES')GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'DETM')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPDETM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)PXMIN,PYMIN,PXMAX,PYMAX
 9012 FORMAT('PXMIN,PYMIN,PXMAX,PYMAX = ',4F10.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)FX1MIN,FX1MAX,FY1MIN,FY1MAX
 9013 FORMAT('FX1MIN,FX1MAX,FY1MIN,FY1MAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)FX2MIN,FX2MAX,FY2MIN,FY2MAX
 9014 FORMAT('FX2MIN,FX2MAX,FY2MIN,FY2MAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IX1TSW,IX2TSW,IY1TSW,IY2TSW
 9016 FORMAT('IX1TSW,IX2TSW,IY1TSW,IY2TSW = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IX1JSW,IX2JSW,IY1JSW,IY2JSW
 9017 FORMAT('IX1JSW,IX2JSW,IY1JSW,IY2JSW = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)NMJX1T,NMJX2T,NMJY1T,NMJY2T
 9018 FORMAT('NMJX1T,NMJX2T,NMJY1T,NMJY2T = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NX1COO,NX2COO,NY1COO,NY2COO
 9020 FORMAT('NX1COO,NX2COO,NY1COO,NY2COO = ',4I8)
      CALL DPWRST('XXX','BUG ')
C
      IF(NX1COO.LE.0)GOTO9029
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9021I=1,NX1COO
      WRITE(ICOUT,9022)I,PX1COO(I),X1COOR(I)
 9022 FORMAT('I,PX1COO(I),X1COOR(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9023)ICASPL,ICAS3D
 9023 FORMAT('ICASPL,ICAS3D = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9029 CONTINUE
C
      IF(NX2COO.LE.0)GOTO9039
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9031I=1,NX2COO
      WRITE(ICOUT,9032)I,PX2COO(I),X2COOR(I)
 9032 FORMAT('I,PX2COO(I),X2COOR(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9031 CONTINUE
 9039 CONTINUE
C
      IF(NY1COO.LE.0)GOTO9049
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9041I=1,NY1COO
      WRITE(ICOUT,9042)I,PY1COO(I),Y1COOR(I)
 9042 FORMAT('I,PY1COO(I),Y1COOR(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9041 CONTINUE
 9049 CONTINUE
C
      IF(NY2COO.LE.0)GOTO9059
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9051I=1,NY2COO
      WRITE(ICOUT,9052)I,PY2COO(I),Y2COOR(I)
 9052 FORMAT('I,PY2COO(I),Y2COOR(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9051 CONTINUE
 9059 CONTINUE
C
      WRITE(ICOUT,9117)IX1NSW,IX2NSW,IY1NSW,IY2NSW
 9117 FORMAT('IX1NSW,IX2NSW,IY1NSW,IY2NSW = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9118)NMNX1T,NMNX2T,NMNY1T,NMNY2T
 9118 FORMAT('NMNX1T,NMNX2T,NMNY1T,NMNY2T = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9120)NX1CMN,NX2CMN,NY1CMN,NY2CMN
 9120 FORMAT('NX1CMN,NX2CMN,NY1CMN,NY2CMN = ',4I8)
      CALL DPWRST('XXX','BUG ')
C
      IF(NX1CMN.LE.0)GOTO9129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9121I=1,NX1CMN
      WRITE(ICOUT,9122)I,PX1CMN(I)
 9122 FORMAT('I,PX1CMN(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9121 CONTINUE
 9129 CONTINUE
C
      IF(NX2CMN.LE.0)GOTO9139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9131I=1,NX2CMN
      WRITE(ICOUT,9132)I,PX2CMN(I)
 9132 FORMAT('I,PX2CMN(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9131 CONTINUE
 9139 CONTINUE
C
      IF(NY1CMN.LE.0)GOTO9149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9141I=1,NY1CMN
      WRITE(ICOUT,9142)I,PY1CMN(I)
 9142 FORMAT('I,PY1CMN(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9141 CONTINUE
 9149 CONTINUE
C
      IF(NY2CMN.LE.0)GOTO9159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9151I=1,NY2CMN
      WRITE(ICOUT,9152)I,PY2CMN(I)
 9152 FORMAT('I,PY2CMN(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9151 CONTINUE
 9159 CONTINUE
C
      WRITE(ICOUT,9189)IBUGG4,ISUBG4,IERRG4
 9189 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDETN(FMIN,FMAX,NUMTIC)
C
C     PURPOSE--GIVEN FRAME LIMITS,
C              COMPUTE THE NUMBER OF MAJOR TIC MARKS
C              (INCLUDING THE ENDS)
C
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ICINT
C
      DIMENSION NINTER(100)
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
C---------------------------------------------------------------------
C
      DATA (NINTER(I),I=1,100)
     1/4,4,6,4,5,6,7,8,9,10,
     1 11,6,4,7,3,8,4,9,4,4,
     1 7,11,4,8,5,4,9,7,4,6,
     1 4,8,11,4,7,12,4,4,6,8,
     1 4,7,4,11,9,4,4,8,7,5,
     1 4,4,4,9,11,7,6,4,4,6,
     1 4,4,7,8,5,11,4,4,6,7,
     1 4,9,4,4,5,4,11,4,4,8,
     1 9,4,4,7,5,4,6,11,4,9,
     1 7,8,6,4,5,8,4,7,11,10/
C
C-----START POINT-----------------------------------------------------
C
      IEXP=(-999)
      XTDEL=(-999)
      NINT=(-999)
      NUMINT=(-999)
C
      EPS=0.0001
      ONEMEP=1.0-EPS
      ONEPEP=1.0+EPS
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'DETN')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPDETN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)FMIN,FMAX
   52 FORMAT('FMIN,FMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4,ISUBG4,IERRG4
   59 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *************************************
C               **  STEP 1--                       **
C               **  COPY OVER THE INPUT VARAIBLES  **
C               **  INTO TEMPORARY VARIABLES       **
C               *************************************
C
      XTMIN=FMIN
      XTMAX=FMAX
      IF(FMAX.LT.FMIN)XTMIN=FMAX
      IF(FMAX.LT.FMIN)XTMAX=FMIN
      IF(FMIN.EQ.FMAX)NUMTIC=5
      IF(FMIN.EQ.FMAX)GOTO9000
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  SCALE DOWN (OR UP) THE DIFFERENCE IN THE LIMITS  **
C               **  UNTIL THE DIFFERENCE IS IN THE REGION 1 TO 10.   **
C               *******************************************************
C
      IEXP=0
 1200 CONTINUE
      XTDEL=XTMAX-XTMIN
      IF(IBUGG4.EQ.'ON')WRITE(ICOUT,1205)XTMIN,XTMAX,XTDEL,IEXP
 1205 FORMAT('XTMIN,XTMAX,XTDEL,IEXP = ',3F12.5,I8)
      IF(IBUGG4.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IF(XTDEL.LT.1.0)GOTO1210
      IF(XTDEL.GT.10.0)GOTO1220
      GOTO1250
C
 1210 CONTINUE
      XTMIN=XTMIN*10.0
      XTMAX=XTMAX*10.0
      IEXP=IEXP+1
      GOTO1200
C
 1220 CONTINUE
      XTMIN=XTMIN/10.0
      XTMAX=XTMAX/10.0
      IEXP=IEXP-1
      GOTO1200
C
C               ********************************************
C               **  STEP 3--                              **
C               **  DETERMINE A NEAT NUMBER OF TIC MARKS  **
C               **  BASED ON THE ROUNDED DIFFERENCE       **
C               **  IN THE 1 TO 10 RANGE.                 **
C               ********************************************
C
 1250 CONTINUE
      XTMAX2=XTDEL
      CALL CKINTE(XTMAX2,EPS,ONEMEP,ONEPEP,ICINT,IXTMX2)
      IF(ICINT.EQ.'YES')GOTO1259
C
      XTMAX2=XTMAX2*10.0
      CALL CKINTE(XTMAX2,EPS,ONEMEP,ONEPEP,ICINT,IXTMX2)
      IF(ICINT.EQ.'YES')GOTO1259
C
      XTMAX2=XTMAX2*10.0
      CALL CKINTE(XTMAX2,EPS,ONEMEP,ONEPEP,ICINT,IXTMX2)
      GOTO1259
 1259 CONTINUE
C
      NINT=IXTMX2
      IF(NINT.GT.100)NINT=100
      NUMINT=NINTER(NINT)
      NUMTIC=NUMINT+1
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'DETN')GOTO9090
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPDETN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)FMIN,FMAX
 9012 FORMAT('FMIN,FMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IEXP
 9014 FORMAT('IEXP = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)XTDEL,XTMAX2,EPS,ICINT,IXTMX2
 9021 FORMAT('XTDEL,XTMAX2,EPS,ICINT,IXTMX2 = ',3E15.7,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)IXTMX2,NINT,NUMINT,NUMTIC
 9022 FORMAT('IXTMX2,NINT,NUMINT,NUMTIC = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)IBUGG4,ISUBG4,IERRG4
 9019 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
CCCCC DEBUG TRACE,INIT
CCCCC AT 90
CCCCC TRACE ON
      END
      SUBROUTINE DPDETR(IP,IC,XS,YS,IVIS,NS,
     1IPHORI,ICHORI,AUPPER,ALOWER,XHORIZ,NHORP,
     1XMIN,XMAX,IPASS,
     1XOUT,YOUT,TAGOUT,NOUT,NTRACE,
     1IBUGU2,ISUBRO,IERROR)
C
C     PURPOSE--FOR A PAIR OF POINTS WITH INDICES IP AND IC,
C              (THAT IS, FOR POINT 1 = (XS(IP),YS(IP))
C              AND           POINT 2 = (XS(IP),YS(IP))
C              DETERMINE DRAWABLE OUTPUT TRACES BASED ON
C              COMPARISON OF THE (ASSUMED) LINEAR TRACE
C              BETWEEN THE 2 POINTS, AND
C              WITH CURRENT INTERMEDIATE HORIZON TABLES VALUES.
C              ALSO, UPDATE THE HORIZON TABLES AFTER THE FACT.
C     NOTE--THE 2 POINTS HAVE INDICES IP AND IC
C           WITHIN THE DATA VECTORS XS(.), YS(.), AND IVIS(.);
C           THE SAME 2 POINTS HAVE INDICES IPHORI AND IVHORI
C           WITHIN THE HORIZON VECTORS AUPPER(.), ALOWER(.), AND XHORIZ(.).
C     REFERENCE--ROGERS, DAVID F. (1985).  PROCEDURAL
C                ELEMENTS FOR COMPUTER GRAPHICS.
C                MCGRAW-HILL, NEW YORK, PAGE 197-201.
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     VERSION NUMBER--88/9
C     ORIGINAL VERSION--AUGUST    1988.
C     UPDATED         --APRIL     1992. DEFINE 6 SCALARS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVIS
      CHARACTER*4 IPVIS
      CHARACTER*4 ICVIS
C
      CHARACTER*4 IBUGU2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASEF
C
      CHARACTER*4 ICASHO
      CHARACTER*4 ICASIN
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION XS(*)
      DIMENSION YS(*)
      DIMENSION IVIS(*)
C
      DIMENSION AUPPER(*)
      DIMENSION ALOWER(*)
      DIMENSION XHORIZ(*)
C
      DIMENSION XOUT(*)
      DIMENSION YOUT(*)
      DIMENSION TAGOUT(*)
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
      ISUBN1='DPDE'
      ISUBN2='TR  '
C
CCCCC THE FOLLOWING 6 LINES WERE ADDED   APRIL 1992
      XTEMPO=(-999.0)
      YTEMPO=(-999.0)
      YCUTOL=(-999.0)
      XTEMP=(-999.0)
      YTEMP=(-999.0)
      YCUT=(-999.0)
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')GOTO50
      GOTO90
   50 CONTINUE
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPDETR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGU2,ISUBRO,IERROR
   52 FORMAT('IBUGU2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IP,IC,NS
   53 FORMAT('IP,IC,NS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=IP,IC
      WRITE(ICOUT,56)I,XS(I),YS(I),IVIS(I)
   56 FORMAT('I,XS(I),YS(I),IVIS(I) = ',I8,2E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,61)IPHORI,ICHORI,NHORP
   61 FORMAT('IPHORI,ICHORI,NHORP = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=IPHORI,ICHORI
      WRITE(ICOUT,66)I,AUPPER(I),ALOWER(I),XHORIZ(I)
   66 FORMAT('I,AUPPER(I),ALOWER(I),XHORIZ(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,71)XMIN,XMAX
   71 FORMAT('XMIN,XMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,72)IPASS
   72 FORMAT('IPASS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,81)NOUT,NTRACE
   81 FORMAT('NOUT,NTRACE = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO85I=1,NOUT
      WRITE(ICOUT,86)I,XOUT(I),YOUT(I),TAGOUT(I)
   86 FORMAT('I,XOUT(I),YOUT(I),TAGOUT(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
   85 CONTINUE
   90 CONTINUE
C
      XP=XS(IP)
      YP=YS(IP)
      IPVIS=IVIS(IP)
C
      XC=XS(IC)
      YC=YS(IC)
      ICVIS=IVIS(IC)
C
      YPU=AUPPER(IPHORI)
      YPL=ALOWER(IPHORI)
      YCU=AUPPER(ICHORI)
      YCL=ALOWER(ICHORI)
C
      XCUT=(XHORIZ(IPHORI)+XHORIZ(ICHORI))/2.0
      SLOEPS=0.000001
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')GOTO110
      GOTO119
  110 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** FROM THE EARLY MIDDLE OF DPDETR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)IPHORI,ICHORI
  112 FORMAT('IPHORI,ICHORI = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)XP,YP,YPU,YPL,IPVIS
  113 FORMAT('XP,YP,YPU,YPL,IPVIS = ',4E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,114)XC,YC,YCU,YCL,ICVIS
  114 FORMAT('XC,YC,YCU,YCL,ICVIS = ',4E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,115)XCUT
  115 FORMAT('XCUT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
  119 CONTINUE
C
C               **************************************************
C               **  STEP 10--                                   **
C               **  BRANCH TO 1 OF 6 CASES--                    **
C               **     1. SAME CELL,         INFINITE SLOPE     **
C               **     2. SAME CELL,         FINITE   SLOPE     **
C               **     3. ADJACENT CELL,     INFINITE SLOPE (IMPOSSIBLE)     **
C               **     4. ADJACENT CELL,     FINITE   SLOPE     **
C               **     5. NON-ADJACENT CELL, INFINITE SLOPE (IMPOSSIBLE)     **
C               **     6. NON-ADJACENT CELL, FINITE   SLOPE     **
C               **************************************************
C
      IDEL=ICHORI-IPHORI
      IF(IDEL.EQ.0)GOTO1000
      IF(IDEL.EQ.1)GOTO1100
      GOTO1200
C
 1000 CONTINUE
      IF(XC.EQ.XP)GOTO2000
      GOTO3000
 1100 CONTINUE
      IF(XC.EQ.XP)GOTO4000
      GOTO5000
 1200 CONTINUE
      IF(XC.EQ.XP)GOTO6000
      GOTO7000
C
C               **************************************************
C               **  STEP 20--                                   **
C     ----------**  TREAT THE CASE OF SAME HORIZON CELL         **----------
C               **  AND INFINITE SLOPE                          **
C               **************************************************
C
 2000 CONTINUE
      IF(IPVIS.EQ.'YES'.AND.ICVIS.EQ.'YES')GOTO2100
      IF(IPVIS.EQ.'YES'.AND.ICVIS.EQ.'NO')GOTO2200
      IF(IPVIS.EQ.'NO'.AND.ICVIS.EQ.'YES')GOTO2300
      GOTO2400
C
C               **************************************************
C               **  STEP 21--                                   **
C               **  FOR THE SAME CELL & INFINITE SLOPE CASE,    **
C               **  TREAT THE VISIBLE/VISIBLE SUBCASE.          **
C               **************************************************
C
 2100 CONTINUE
      ISTEPN='2100'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YP.GE.YPU.AND.YC.GE.YCU)GOTO2110
      IF(YP.LE.YPL.AND.YC.LE.YCL)GOTO2120
      IF(YP.LE.YPL.AND.YC.GE.YCU)GOTO2130
      GOTO2140
C
 2110 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      IF(YC.GT.YP)AUPPER(ICHORI)=YC
      GOTO9000
C
 2120 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      IF(YC.LT.YP)ALOWER(ICHORI)=YC
      GOTO9000
C
 2130 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XP
      YOUT(NOUT)=YPL
      TAGOUT(NOUT)=NTRACE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YCU
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      AUPPER(ICHORI)=YC
      GOTO9000
C
 2140 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XP
      YOUT(NOUT)=YPU
      TAGOUT(NOUT)=NTRACE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YCL
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      ALOWER(ICHORI)=YC
      GOTO9000
C
C               **************************************************
C               **  STEP 22--                                   **
C               **  FOR THE SAME CELL & INFINITE SLOPE CASE,    **
C               **  TREAT THE VISIBLE/INVISIBLE SUBCASE.        **
C               **************************************************
C
 2200 CONTINUE
      ISTEPN='2200'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YP.GE.YPU)GOTO2210
      GOTO2220
C
 2210 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XP
      YOUT(NOUT)=YPU
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      GOTO9000
C
 2220 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XP
      YOUT(NOUT)=YPL
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      GOTO9000
C
C               **************************************************
C               **  STEP 23--                                   **
C               **  FOR THE SAME CELL & INFINITE SLOPE CASE,    **
C               **  TREAT THE INVISIBLE/VISIBLE SUBCASE.        **
C               **************************************************
C
 2300 CONTINUE
      ISTEPN='2300'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YC.GE.YCU)GOTO2310
      GOTO2320
C
 2310 CONTINUE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YCU
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(ICHORI)=YC
      GOTO9000
C
 2320 CONTINUE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YCL
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(ICHORI)=YC
      GOTO9000
C
C               **************************************************
C               **  STEP 24--                                   **
C               **  FOR THE SAME CELL & INFINITE SLOPE CASE,    **
C               **  TREAT THE INVISIBLE/INVISIBLE SUBCASE.      **
C               **************************************************
C
 2400 CONTINUE
      ISTEPN='2400'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      GOTO9000
C
C               **************************************************
C               **  STEP 30--                                   **
C     ----------**  TREAT THE CASE OF SAME HORIZON CELL         **----------
C               **  AND FINITE SLOPE                            **
C               **************************************************
C
 3000 CONTINUE
      ISTEPN='3000'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SLOPE=(YC-YP)/(XC-XP)
      ABSSLO=ABS(SLOPE)
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')GOTO3010
      GOTO3019
 3010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3011)
 3011 FORMAT('***** FROM THE MIDDLE OF DPDETR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3012)YC,YP,XC,XP,SLOPE
 3012 FORMAT('YC,YP,XC,XP,SLOPE = ',5E15.7)
      CALL DPWRST('XXX','BUG ')
 3019 CONTINUE
C
      IF(IPVIS.EQ.'YES'.AND.ICVIS.EQ.'YES')GOTO3100
      IF(IPVIS.EQ.'YES'.AND.ICVIS.EQ.'NO')GOTO3200
      IF(IPVIS.EQ.'NO'.AND.ICVIS.EQ.'YES')GOTO3300
      GOTO3400
C
C               **************************************************
C               **  STEP 31--                                   **
C               **  FOR THE SAME CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE VISIBLE/VISIBLE SUBCASE.          **
C               **************************************************
C
 3100 CONTINUE
      ISTEPN='3100'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YP.GE.YPU.AND.YC.GE.YCU)GOTO3110
      IF(YP.LE.YPL.AND.YC.LE.YCL)GOTO3120
      IF(YP.LE.YPL.AND.YC.GE.YCU)GOTO3130
      GOTO3140
C
 3110 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      IF(YC.GT.YP)AUPPER(ICHORI)=YC
      GOTO9000
C
 3120 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      IF(YC.LT.YP)ALOWER(ICHORI)=YC
      GOTO9000
C
 3130 CONTINUE
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XP+(YPL-YP)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YPL
      TAGOUT(NOUT)=NTRACE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XC+(YCU-YC)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YCU
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      AUPPER(ICHORI)=YC
      GOTO9000
C
 3140 CONTINUE
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XP+(YPU-YP)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YPU
      TAGOUT(NOUT)=NTRACE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XC+(YCL-YC)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YCL
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      ALOWER(ICHORI)=YC
      GOTO9000
C
C               **************************************************
C               **  STEP 32--                                   **
C               **  FOR THE SAME CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE VISIBLE/INVISIBLE SUBCASE.        **
C               **************************************************
C
 3200 CONTINUE
      ISTEPN='3200'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YP.GE.YPU)GOTO3210
      GOTO3220
C
 3210 CONTINUE
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XP+(YPU-YP)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YPU
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      GOTO9000
C
 3220 CONTINUE
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XP+(YPL-YP)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YPL
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      GOTO9000
C
C               **************************************************
C               **  STEP 33--                                   **
C               **  FOR THE SAME CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE INVISIBLE/VISIBLE SUBCASE.        **
C               **************************************************
C
 3300 CONTINUE
      ISTEPN='3300'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YC.GE.YCU)GOTO3310
      GOTO3320
C
 3310 CONTINUE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XC+(YCU-YC)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YCU
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(ICHORI)=YC
      GOTO9000
C
 3320 CONTINUE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XC+(YCL-YC)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YCL
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(ICHORI)=YC
      GOTO9000
C
C               **************************************************
C               **  STEP 34--                                   **
C               **  FOR THE SAME CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE INVISIBLE/INVISIBLE SUBCASE.      **
C               **************************************************
C
 3400 CONTINUE
      ISTEPN='3400'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      GOTO9000
C
C               **************************************************
C               **  STEP 40--                                   **
C     ----------**  TREAT THE CASE OF ADJACENT HORIZON CELL     **----------
C               **  AND INFINITE SLOPE                          **
C               **  (SHOULD BE IMPOSSIBLE)                      **
C               **************************************************
C
 4000 CONTINUE
      ISTEPN='4000'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4010)
 4010 FORMAT('***** INTERNAL ERROR IN DPDETR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4011)
 4011 FORMAT('      AT BRANCH POINT 4000 (AN IMPOSSIBLE BRANCH)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4012)
 4012 FORMAT('      CONDITION = ADJACENT CELL BUT INFINITE SLOPE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4013)
 4013 FORMAT('      IF HAVE INFINITE SLOPE, THEN NECESSARILY MUST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4014)
 4014 FORMAT('      BE IN SAME CELL.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4015)IP,IC
 4015 FORMAT('IP,IC = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4016)IPHORI,ICHORI
 4016 FORMAT('IPHORI,ICHORI = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4017)XS(IP),YS(IP),XS(IC),YS(IC)
 4017 FORMAT('XS(IP),YS(IP),XS(IC),YS(IC) = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               **************************************************
C               **  STEP 50--                                   **
C     ----------**  TREAT THE CASE OF ADJACENT HORIZON CELL     **----------
C               **  AND FINITE SLOPE                            **
C               **************************************************
C
 5000 CONTINUE
      ISTEPN='5000'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SLOPE=(YC-YP)/(XC-XP)
      ABSSLO=ABS(SLOPE)
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')GOTO5010
      GOTO5019
 5010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5011)
 5011 FORMAT('***** FROM THE MIDDLE OF DPDETR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5012)YC,YP,XC,XP,SLOPE
 5012 FORMAT('YC,YP,XC,XP,SLOPE = ',5E15.7)
      CALL DPWRST('XXX','BUG ')
 5019 CONTINUE
C
      IF(IPVIS.EQ.'YES'.AND.ICVIS.EQ.'YES')GOTO5100
      IF(IPVIS.EQ.'YES'.AND.ICVIS.EQ.'NO')GOTO5200
      IF(IPVIS.EQ.'NO'.AND.ICVIS.EQ.'YES')GOTO5300
      GOTO5400
C
C               **************************************************
C               **  STEP 51--                                   **
C               **  FOR ADJACENT CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE VISIBLE/VISIBLE SUBCASE.          **
C               **  5130 AND 5140 ASSUMES HIGH RES. HOR. GRID   **
C               **************************************************
C
 5100 CONTINUE
      ISTEPN='5100'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YP.GE.YPU.AND.YC.GE.YCU)GOTO5110
      IF(YP.LE.YPL.AND.YC.LE.YCL)GOTO5120
      IF(YP.LE.YPL.AND.YC.GE.YCU)GOTO5130
      GOTO5140
C
 5110 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      AUPPER(ICHORI)=YC
      GOTO9000
C
 5120 CONTINUE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      ALOWER(ICHORI)=YC
      GOTO9000
C
 5130 CONTINUE
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XP+(YPL-YP)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YPL
      TAGOUT(NOUT)=NTRACE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XC+(YCU-YC)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YCU
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      AUPPER(ICHORI)=YC
      GOTO9000
C
 5140 CONTINUE
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XP+(YPU-YP)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YPU
      TAGOUT(NOUT)=NTRACE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XC+(YCL-YC)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YCL
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      ALOWER(ICHORI)=YC
      GOTO9000
C
C               **************************************************
C               **  STEP 52--                                   **
C               **  FOR ADJACENT CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE VISIBLE/INVISIBLE SUBCASE.        **
C               **  5210 AND 5220 ASSUMES HIGH RES. HOR. GRID   **
C               **************************************************
C
 5200 CONTINUE
      ISTEPN='5200'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YP.GE.YPU)GOTO5210
      GOTO5220
C
 5210 CONTINUE
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XP+(YPU-YP)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YPU
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      GOTO9000
C
 5220 CONTINUE
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XP+(YPL-YP)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YPL
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      GOTO9000
C
C               **************************************************
C               **  STEP 53--                                   **
C               **  FOR ADJACENT CELL & FINITE SLOPE   CASE,    **
C               **  5310 AND 5320 ASSUMES HIGH RES. HOR. GRID   **
C               **  TREAT THE INVISIBLE/VISIBLE SUBCASE.        **
C               **************************************************
C
 5300 CONTINUE
      ISTEPN='5300'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YC.GE.YCU)GOTO5310
      GOTO5320
C
 5310 CONTINUE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XC+(YCU-YC)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YCU
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(ICHORI)=YC
      GOTO9000
C
 5320 CONTINUE
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      IF(ABSSLO.LE.SLOEPS)XTEMP=XCUT
      IF(ABSSLO.GT.SLOEPS)XTEMP=XC+(YCL-YC)/SLOPE
      IF(SLOPE.NE.0.0.AND.XTEMP.GT.XCUT)XOUTT=XCUT
      XOUT(NOUT)=XTEMP
      YOUT(NOUT)=YCL
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(ICHORI)=YC
      GOTO9000
C
C               **************************************************
C               **  STEP 54--                                   **
C               **  FOR ADJACENT CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE INVISIBLE/INVISIBLE SUBCASE.      **
C               **************************************************
C
 5400 CONTINUE
      ISTEPN='5400'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      GOTO9000
C
C               **************************************************
C               **  STEP 60--                                   **
C     ----------**  TREAT THE CASE OF NON-ADJ. HORIZON CELL     **----------
C               **  AND INFINITE SLOPE                          **
C               **  (SHOULD BE IMPOSSIBLE)                      **
C               **************************************************
C
 6000 CONTINUE
      ISTEPN='6000'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6010)
 6010 FORMAT('***** INTERNAL ERROR IN DPDETR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6011)
 6011 FORMAT('      AT BRANCH POINT 4000 (AN IMPOSSIBLE BRANCH)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6012)
 6012 FORMAT('      CONDITION = ADJACENT CELL BUT INFINITE SLOPE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6013)
 6013 FORMAT('      IF HAVE INFINITE SLOPE, THEN NECESSARILY MUST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6014)
 6014 FORMAT('      BE IN SAME CELL.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6015)IP,IC
 6015 FORMAT('IP,IC = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6016)IPHORI,ICHORI
 6016 FORMAT('IPHORI,ICHORI = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6017)XS(IP),YS(IP),XS(IC),YS(IC)
 6017 FORMAT('XS(IP),YS(IP),XS(IC),YS(IC) = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               **************************************************
C               **  STEP 70--                                   **
C     ----------**  TREAT THE CASE OF NON-ADJ. HORIZON CELL     **----------
C               **  AND FINITE SLOPE                            **
C               **************************************************
C
 7000 CONTINUE
      ISTEPN='7000'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SLOPE=(YC-YP)/(XC-XP)
      ABSSLO=ABS(SLOPE)
C
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')GOTO7010
      GOTO7019
 7010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7011)
 7011 FORMAT('***** FROM THE MIDDLE OF DPDETR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7012)YC,YP,XC,XP,SLOPE
 7012 FORMAT('YC,YP,XC,XP,SLOPE = ',5E15.7)
      CALL DPWRST('XXX','BUG ')
 7019 CONTINUE
C
      IF(IPVIS.EQ.'YES'.AND.ICVIS.EQ.'YES')GOTO7100
      IF(IPVIS.EQ.'YES'.AND.ICVIS.EQ.'NO')GOTO7200
      IF(IPVIS.EQ.'NO'.AND.ICVIS.EQ.'YES')GOTO7300
      GOTO7400
C
C               **************************************************
C               **  STEP 71--                                   **
C               **  FOR NON-ADJ. CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE VISIBLE/VISIBLE SUBCASE.          **
C               **  7130 AND 7140 ASSUMES DENSE DATA POINTS     **
C               **  TO AVOID SLOPED LINE GOING THROUGH          **
C               **  INVISIBLE REGION MORE THAN ONCE.            **
C               **************************************************
C
 7100 CONTINUE
      ISTEPN='7100'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YP.GE.YPU.AND.YC.GE.YCU)GOTO7110
      IF(YP.LE.YPL.AND.YC.LE.YCL)GOTO7120
      IF(YP.LE.YPL.AND.YC.GE.YCU)GOTO7130
      GOTO7150
C
 7110 CONTINUE
      ISTEPN='7110'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      AUPPER(ICHORI)=YC
      ICASEF='UPPE'
      CALL FILLHT(IPHORI,ICHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      GOTO9000
C
 7120 CONTINUE
      ISTEPN='7120'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      ALOWER(ICHORI)=YC
      ICASEF='LOWE'
      CALL FILLHT(IPHORI,ICHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      GOTO9000
C
 7130 CONTINUE
      ISTEPN='7130'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISTART=IPHORI
      ICASHO='LOWE'
      ICASIN='GE'
      CALL DPCOIP(XP,YP,XC,YC,SLOPE,ABSSLO,SLOEPS,
     1XHORIZ,AUPPER,ALOWER,NHORP,IPHORI,ICHORI,
     1ISTART,ICASHO,ICASIN,
     1XMIN,XMAX,
     1XTEMP2,YTEMP2,ITHORI,
     1IBUGU2,ISUBRO,IERROR)
      NOUT=NOUT+1
      XOUT(NOUT)=XTEMP2
      YOUT(NOUT)=YTEMP2
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      ICASEF='LOWE'
      CALL FILLHT(IPHORI,ITHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
C
 7140 CONTINUE
      ISTART=ITHORI
      ICASHO='UPPE'
      ICASIN='GE'
      CALL DPCOIP(XP,YP,XC,YC,SLOPE,ABSSLO,SLOEPS,
     1XHORIZ,AUPPER,ALOWER,NHORP,IPHORI,ICHORI,
     1ISTART,ICASHO,ICASIN,
     1XMIN,XMAX,
     1XTEMP2,YTEMP2,ITHORI,
     1IBUGU2,ISUBRO,IERROR)
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XTEMP2
      YOUT(NOUT)=YTEMP2
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(ICHORI)=YC
      ICASEF='UPPE'
      CALL FILLHT(ITHORI,ICHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      GOTO9000
C
 7150 CONTINUE
      ISTEPN='7150'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISTART=IPHORI
      ICASHO='UPPE'
      ICASIN='LE'
      CALL DPCOIP(XP,YP,XC,YC,SLOPE,ABSSLO,SLOEPS,
     1XHORIZ,AUPPER,ALOWER,NHORP,IPHORI,ICHORI,
     1ISTART,ICASHO,ICASIN,
     1XMIN,XMAX,
     1XTEMP2,YTEMP2,ITHORI,
     1IBUGU2,ISUBRO,IERROR)
      NOUT=NOUT+1
      XOUT(NOUT)=XTEMP2
      YOUT(NOUT)=YTEMP2
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      ICASEF='UPPE'
      CALL FILLHT(IPHORI,ITHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
CCCCC GOTO9000   SHOULD THIS BE COMMENTED OUT?--IT WAS IN ORIG. VERSION OF D
C
 7160 CONTINUE
      ISTART=ITHORI
      ICASHO='LOWE'
      ICASIN='LE'
      CALL DPCOIP(XP,YP,XC,YC,SLOPE,ABSSLO,SLOEPS,
     1XHORIZ,AUPPER,ALOWER,NHORP,IPHORI,ICHORI,
     1ISTART,ICASHO,ICASIN,
     1XMIN,XMAX,
     1XTEMP2,YTEMP2,ITHORI,
     1IBUGU2,ISUBRO,IERROR)
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XTEMP2
      YOUT(NOUT)=YTEMP2
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(ICHORI)=YC
      ICASEF='LOWE'
      CALL FILLHT(ITHORI,ICHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      GOTO9000
C
C               **************************************************
C               **  STEP 72--                                   **
C               **  FOR NON-ADJ. CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE VISIBLE/INVISIBLE SUBCASE.        **
C               **  7210 AND 7220 ASSUMES HIGH RES. HOR. GRID   **
C               **  TO AVOID SLOPED LINE HITTING MULTIPLE STEPS **
C               **************************************************
C
 7200 CONTINUE
      ISTEPN='7200'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YP.GE.YPU)GOTO7210
      GOTO7220
C
 7210 CONTINUE
      ISTEPN='7210'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISTART=IPHORI
      ICASHO='UPPE'
      ICASIN='LE'
      CALL DPCOIP(XP,YP,XC,YC,SLOPE,ABSSLO,SLOEPS,
     1XHORIZ,AUPPER,ALOWER,NHORP,IPHORI,ICHORI,
     1ISTART,ICASHO,ICASIN,
     1XMIN,XMAX,
     1XTEMP2,YTEMP2,ITHORI,
     1IBUGU2,ISUBRO,IERROR)
      NOUT=NOUT+1
      XOUT(NOUT)=XTEMP2
      YOUT(NOUT)=YTEMP2
      TAGOUT(NOUT)=NTRACE
      AUPPER(IPHORI)=YP
      ICASEF='UPPE'
      CALL FILLHT(IPHORI,ITHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      GOTO9000
C
 7220 CONTINUE
      ISTEPN='7220'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISTART=IPHORI
      ICASHO='LOWE'
      ICASIN='GE'
      CALL DPCOIP(XP,YP,XC,YC,SLOPE,ABSSLO,SLOEPS,
     1XHORIZ,AUPPER,ALOWER,NHORP,IPHORI,ICHORI,
     1ISTART,ICASHO,ICASIN,
     1XMIN,XMAX,
     1XTEMP2,YTEMP2,ITHORI,
     1IBUGU2,ISUBRO,IERROR)
      NOUT=NOUT+1
      XOUT(NOUT)=XTEMP2
      YOUT(NOUT)=YTEMP2
      TAGOUT(NOUT)=NTRACE
      ALOWER(IPHORI)=YP
      ICASEF='LOWE'
      CALL FILLHT(IPHORI,ITHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      GOTO9000
C
C               **************************************************
C               **  STEP 73--                                   **
C               **  FOR NON-ADJ. CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE INVISIBLE/VISIBLE SUBCASE.        **
C               **  7310 AND 7320 ASSUMES HIGH RES. HOR. GRID   **
C               **  TO AVOID SLOPED LINE HITTING MULTIPLE STEPS **
C               **************************************************
C
 7300 CONTINUE
      ISTEPN='7300'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(YC.GE.YCU)GOTO7310
      GOTO7320
C
 7310 CONTINUE
      ISTEPN='7310'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISTART=IPHORI
      ICASHO='UPPE'
      ICASIN='GE'
      CALL DPCOIP(XP,YP,XC,YC,SLOPE,ABSSLO,SLOEPS,
     1XHORIZ,AUPPER,ALOWER,NHORP,IPHORI,ICHORI,
     1ISTART,ICASHO,ICASIN,
     1XMIN,XMAX,
     1XTEMP2,YTEMP2,ITHORI,
     1IBUGU2,ISUBRO,IERROR)
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XTEMP2
      YOUT(NOUT)=YTEMP2
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      AUPPER(ICHORI)=YC
      ICASEF='UPPE'
      CALL FILLHT(ITHORI,ICHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      GOTO9000
C
 7320 CONTINUE
      ISTEPN='7320'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISTART=IPHORI
      ICASHO='LOWE'
      ICASIN='LE'
      CALL DPCOIP(XP,YP,XC,YC,SLOPE,ABSSLO,SLOEPS,
     1XHORIZ,AUPPER,ALOWER,NHORP,IPHORI,ICHORI,
     1ISTART,ICASHO,ICASIN,
     1XMIN,XMAX,
     1XTEMP2,YTEMP2,ITHORI,
     1IBUGU2,ISUBRO,IERROR)
      NTRACE=NTRACE+1
      NOUT=NOUT+1
      XOUT(NOUT)=XTEMP2
      YOUT(NOUT)=YTEMP2
      TAGOUT(NOUT)=NTRACE
      NOUT=NOUT+1
      XOUT(NOUT)=XC
      YOUT(NOUT)=YC
      TAGOUT(NOUT)=NTRACE
      ALOWER(ICHORI)=YC
      ICASEF='LOWE'
      CALL FILLHT(ITHORI,ICHORI,AUPPER,ALOWER,XHORIZ,NHORP,ICASEF,
     1IBUGU2,ISUBRO,IERROR)
      GOTO9000
C
C               **************************************************
C               **  STEP 74--                                   **
C               **  FOR NON-ADJ. CELL & FINITE SLOPE   CASE,    **
C               **  TREAT THE INVISIBLE/INVISIBLE SUBCASE.      **
C               **************************************************
C
 7400 CONTINUE
      ISTEPN='7400'
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      GOTO9000
C
C               **************************************************
C               **  STEP 90--                                   **
C               **  EXIT.                                       **
C               **************************************************
C
 9000 CONTINUE
      IF(IBUGU2.EQ.'ON'.OR.ISUBRO.EQ.'DETR')GOTO9010
      GOTO9090
 9010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPDETR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGU2,ISUBRO,IERROR
 9012 FORMAT('IBUGU2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IP,IC,NS
 9013 FORMAT('IP,IC,NS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=IP,IC
      WRITE(ICOUT,9016)I,XS(I),YS(I),IVIS(I)
 9016 FORMAT('I,XS(I),YS(I),IVIS(I) = ',I8,2E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9021)IPHORI,ICHORI,NHORP
 9021 FORMAT('IPHORI,ICHORI,NHORP = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=IPHORI,ICHORI
      WRITE(ICOUT,9026)I,AUPPER(I),ALOWER(I),XHORIZ(I)
 9026 FORMAT('I,AUPPER(I),ALOWER(I),XHORIZ(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9031)XMIN,XMAX
 9031 FORMAT('XMIN,XMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)IPASS
 9032 FORMAT('IPASS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)NOUT,NTRACE
 9041 FORMAT('NOUT,NTRACE = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9045I=1,NOUT
      WRITE(ICOUT,9046)I,XOUT(I),YOUT(I),TAGOUT(I)
 9046 FORMAT('I,XOUT(I),YOUT(I),TAGOUT(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9045 CONTINUE
CCCCC WRITE(ICOUT,9051)I2
C9051 FORMAT('I2 (TOO FAR) = ',I8)
CCCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9052)XTEMPO,YTEMPO,YCUTOL
 9052 FORMAT('XTEMPO,YTEMPO,YCUTOL = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9053)XTEMP,YTEMP,YCUT
 9053 FORMAT('XTEMP,YTEMP,YCUT = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9055)XTEMP2,YTEMP2
 9055 FORMAT('XTEMP2,YTEMP2 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPDEUN(IHARG,IHARG2,IARGT,IARG,NUMARG,
     1IDEFUN,
     1NUMDEV,MAXDEV,
     1IDMANU,IDMODE,IDMOD2,IDMOD3,
     1IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1IBUGO2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE LOGICAL I/O UNIT NUMBER
C              FOR AN OUTPUT DEVICE.
C              THE LOGICAL I/O UNIT NUMBER
C              FOR DEVICE I WILL BE PLACED
C              IN THE I-TH ELEMENT OF THE INTEGER
C              VECTOR IDUNIT(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IHARG2 (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFUN
C                     --MAXDEV
C     OUTPUT ARGUMENTS--IDUNIT (AN INTEGER VECTOR
C                              WHOSE I-TH ELEMENT CONTAINS THE
C                              LOGICAL I/O UNIT NUMBER
C                              FOR DEVICE I.
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IDMANU
      CHARACTER*4 IDMODE
      CHARACTER*4 IDMOD2
      CHARACTER*4 IDMOD3
C
      CHARACTER*4 IDPOWE
      CHARACTER*4 IDCONT
      CHARACTER*4 IDCOLO
C
      CHARACTER*4 IBUGO2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
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
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
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
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'UNIT')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NUMB')GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'UNIT')GOTO1140
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'NUMB')GOTO1140
      GOTO1199
C
 1110 CONTINUE
      IF(NUMARG.LE.1)GOTO1120
      IF(IHARG(2).EQ.'ON')GOTO1120
      IF(IHARG(2).EQ.'OFF')GOTO1120
      IF(IHARG(2).EQ.'AUTO')GOTO1120
      IF(IHARG(2).EQ.'DEFA')GOTO1120
      GOTO1125
C
 1120 CONTINUE
      IHOLD=IDEFUN
      GOTO1130
C
 1125 CONTINUE
      IHOLD=IARG(2)
      GOTO1130
C
 1130 CONTINUE
      IFOUND='YES'
      DO1135I=1,NUMDEV
      IDUNIT(I)=IHOLD
      IDPOWE(I)='OFF'
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('THE UNIT NUMBER FOR ALL DEVICES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)IHOLD
 1137 FORMAT('HAS JUST BEEN SET TO',I8)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO1199
C
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GOTO1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPDEUN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE DEVICE ... UNIT NUMBER COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE DEVICE IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      DEVICE 3 UNIT NUMBER 25 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXDEV)GOTO1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPDEUN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE DEVICE ... UNIT NUMBER COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF DEVICES MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXDEV
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',
     1'DEVICE.')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1160 CONTINUE
      IF(NUMARG.LE.2)GOTO1170
      IF(IHARG(3).EQ.'ON')GOTO1170
      IF(IHARG(3).EQ.'OFF')GOTO1170
      IF(IHARG(3).EQ.'AUTO')GOTO1170
      IF(IHARG(3).EQ.'DEFA')GOTO1170
      GOTO1175
C
 1170 CONTINUE
      IHOLD=IDEFUN
      GOTO1180
C
 1175 CONTINUE
      IHOLD=IARG(3)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IDUNIT(I)=IHOLD
      IDPOWE(I)='OFF'
C
      IF(IFEEDB.EQ.'OFF')GOTO1199
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)I
 1181 FORMAT('            DEVICE           --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IDUNIT(I)
 1182 FORMAT('            I/O UNIT         --',I4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)IDMANU(I)
 1183 FORMAT('            MANUFACTURER     --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)IDMODE(I),IDMOD2(I),IDMOD3(I)
 1184 FORMAT('            MODEL            --',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1185)IDPOWE(I)
 1185 FORMAT('            POWER            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)IDCONT(I)
 1186 FORMAT('            CONTINUITY       --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1187)IDCOLO(I)
 1187 FORMAT('            COLOR            --',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IDNHPP(I)
 1188 FORMAT('            HORIZONTAL PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1189)IDNVPP(I)
 1189 FORMAT('            VERTICAL   PIXELS--',I8)
      CALL DPWRST('XXX','BUG ')
 1199 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPDEV(ID,IOP,ICOMP,ICAPSW,IBUGXX,ISUBRO,IERROR)
C
C     PURPOSE--DEFINE, OPEN, OR CLOSE 1, 2, OR 3 (DEPENDING ON SETTING
C              IN IOP)
C     INPUT ARGUMENTS--ID     = INTEGER NUMBER OF DEVICE:
C                               1, 2, 3, ..., 10
C                      IOP    = CHARACTER*4 FOR DESIRED OPERATION:
C                               DEFI(NE), OPEN, OR CLOS(E)
C                      ICOMP  = CHARACTER*4 FOR COMPANY
C                               TEKT, POST, HPGL, GENE, ...
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--92.6
C     ORIGINAL VERSION--MAY       1992.
C     UPDATED         --SEPTEMBER 2002. ICAPSW
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 IOP
      CHARACTER*4 ICOMP
      CHARACTER*4 ICAPSW
      CHARACTER*4 IBUGXX
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IFOUND
C
      CHARACTER*1 ICJUNK
C
      CHARACTER*4 ICM
      CHARACTER*4 ICM2
      CHARACTER*4 IHRG
      CHARACTER*4 IHRG2
      CHARACTER*4 IRGT
C
      DIMENSION IHRG(MAXSTR)
      DIMENSION IHRG2(MAXSTR)
      DIMENSION IRGT(MAXSTR)
      DIMENSION IRG(MAXSTR)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOF2.INC'
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
      IF(IBUGXX.EQ.'ON'.OR.ISUBRO.EQ.'DEV')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ID,IOP,ICOMP,IBUGXX,ISUBRO
   52   FORMAT('ID,IOP,ICOMP,IBUGXX,ISUBRO = ',I8,4(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)IERROR,IPL1CS,IPL2CS
   61   FORMAT('IERROR,IPL1CS,IPL2CS = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 1000 CONTINUE
      ICM='DEVI'
      ICM2='CE  '
C
      IF(ID.EQ.1)IHRG(1)='1   '
      IF(ID.EQ.2)IHRG(1)='2   '
      IF(ID.EQ.3)IHRG(1)='3   '
      IF(ID.EQ.4)IHRG(1)='4   '
      IF(ID.EQ.5)IHRG(1)='5   '
      IF(ID.EQ.6)IHRG(1)='6   '
      IF(ID.EQ.7)IHRG(1)='7   '
      IF(ID.EQ.8)IHRG(1)='8   '
      IF(ID.EQ.9)IHRG(1)='9   '
      IF(ID.EQ.10)IHRG(1)='10  '
      IHRG2(1)='    '
      IRGT(1)='NUMB'
      IRG(1)=ID
C
      IF(IOP.EQ.'DEFI')THEN
         IHRG(2)=ICOMP
         IHRG2(2)='    '
         IRGT(2)='WORD'
         IRG(2)=(-99)
         NUMRG=2
         CALL DPDEMN(IHRG,IHRG2,IRGT,IRG,NUMRG,
     1   IPL1NU,IPL1NA,
     1   IPL2NU,IPL2NA,
CCCCC    AUGUST 1992.  ADD FOLLOWING LINE
     1   IPL1CS,IPL2CS,
     1   IDEFMA,IDEFMO,IDEFM2,IDEFM3,
     1   IDEFPO,IDEFCN,IDEFDC,IDEFVP,IDEFHP,IDEFUN,
     1   NUMDEV,MAXDEV,
     1   IDMANU,IDMODE,IDMOD2,IDMOD3,
     1   IDPOWE,IDCONT,IDCOLO,IDFONT,IDNVPP,IDNHPP,IDUNIT,
     1   IDNVOF,IDNHOF,
     1   ICAPSW,ICAPNU,
     1   IANS,IWIDTH,IBUGXX,ISUBRO,IFOUND,IERROR)
      ENDIF
C
      IF(IOP.EQ.'OPEN'.OR.IOP.EQ.'CLOS')THEN
         IHRG(2)='POWE'
         IHRG2(2)='R   '
         IRGT(2)='WORD'
         IRG(2)=(-99)
         IHRG(3)=IOP
         IHRG2(3)='    '
         IRGT(3)='WORD'
         IRG(3)=(-99)
         NUMRG=3
         CALL DPDEPW(IHRG,IHRG2,IRGT,IRG,NUMRG,
     1               IPL1NU,IPL1NA,
     1               IPL2NU,IPL2NA,
     1               IDEFPO,
     1               NUMDEV,MAXDEV,
     1               IDMANU,IDMODE,IDMOD2,IDMOD3,
     1               IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1               IDNVOF,IDNHOF,
     1               ICAPSW,ICAPNU,
     1               IANS,IWIDTH,IBUGXX,ISUBRO,IFOUND,IERROR)
      ENDIF
C
      IF(IERROR.EQ.'YES')THEN
         WRITE(ICOUT,1011)
 1011    FORMAT('***** ERROR IN DPDEV--')
         CALL DPWRST('XXX','BUG ')
         IF(IOP.EQ.'DEFI')THEN
           WRITE(ICOUT,1012)ID
 1012      FORMAT('      COULD NOT DEFINE DEVICE ',I8)
           CALL DPWRST('XXX','BUG ')
         ELSEIF(IOP.EQ.'OPEN')THEN
           WRITE(ICOUT,1013)ID
 1013      FORMAT('      COULD NOT OPEN DEVICE ',I8)
           CALL DPWRST('XXX','BUG ')
         ELSEIF(IOP.EQ.'CLOS')THEN
           WRITE(ICOUT,1014)ID
 1014      FORMAT('      COULD NOT CLOSE DEVICE ',I8)
           CALL DPWRST('XXX','BUG ')
         ENDIF
         WRITE(ICOUT,1015)
 1015    FORMAT('HIT ENTER/CARRIAGE-RETURN TO CONTINUE...')
         CALL DPWRST('XXX','BUG ')
         READ(IRD,1016)ICJUNK
 1016    FORMAT(A1)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGXX.EQ.'ON'.OR.ISUBRO.EQ.'DEV')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IBUGXX,ISUBRO,IERROR,IFOUND
 9013   FORMAT('IBUGXX,ISUBRO,IERROR,IFOUND = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IPL1CS,IPL2CS,ICM,ICM2
 9022   FORMAT('IPL1CS,IPL2CS,ICM,ICM2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)NUMRG
 9028   FORMAT('NUMRG = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO9030I=1,NUMRG
          WRITE(ICOUT,9031)I,IHRG(I),IHRG2(I),IRGT(I),IRG(I)
 9031     FORMAT('I,IHRG(I),IHRG2(I),IRGT(I),IRG(I) = ',
     1           I8,3(2X,A4),2X,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9030   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEWI(IHARG,ARG,NUMARG,DEFDEW,
     1DEXWID,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE DESIGN OF EXPERIMENT PLOT WIDTH
C              OF THE LEVELS WITHIN A FACTOR.
C                     --IHARG  (A  HOLLERITH VECTOR)
C     INPUT  ARGUMENTS--IHARG (A HOLLARITH VECTOR)
C                     --ARG    (A REAL VECTOR)
C                     --NUMARG
C                     --DEFDEW
C     OUTPUT ARGUMENTS--DEXWID (A REAL VARIABLE
C                       DENOTING THE PLOT WIDTH OF THE LEVELS
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
C     VERSION NUMBER--89/5
C     ORIGINAL VERSION--MAY       1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC CHARACTER*4 ICOM
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
      IF(NUMARG.LE.0)GOTO1900
C
 1100 CONTINUE
      IF(NUMARG.EQ.1)GOTO1150
      IF(IHARG(2).EQ.'ON')GOTO1150
      IF(IHARG(2).EQ.'OFF')GOTO1150
      IF(IHARG(2).EQ.'AUTO')GOTO1150
      IF(IHARG(2).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      HOLD=DEFDEW
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      DEXWID=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE DESIGN OF EXPERIMENT WIDTH (WITHIN A FACTOR)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)HOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPDEXP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  MAXNXT,ISEED,ICONT,
     1                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE ONE OF THE FOLLOWING DESIGN OF EXPERIMENT
C              STATISTIC PLOTS--
C                 DEX SCATTER PLOT (NOT A STATISTIC)
C                 DEX SIGN PLOT (NOT A STATISTIC)
C                 DEX ORDERED PLOT (NOT A STATISTIC)
C                 DEX <STAT> XXX YYY  PLOT
C         WHERE <STAT> IS ONE OF DATAPLOT'S SUPPORTED STATISTICS
C         AND WHERE XXX MAY BE
C                   (OMITTED)
C                   EFFECTS
C                   ABSOLUTE EFFECTS
C         AND WHERE YYY MAY BE
C                   (OMITTED)
C                   PARETO
C                   YOUDEN
C                 DEX ... PARETO PLOT
C                 DEX ... YOUDEN PLOT (2**K DESIGNS ONLY)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG,MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/6
C     ORIGINAL VERSION--MAY       1989.
C     UPDATED         --JANUARY   1990. INITE & ININTE TO NXINTE
C     UPDATED         --JANUARY   1990. MAX VECTOR SIZE TO 10*MAXOBV
C     UPDATED         --JANUARY   1990. CHECK FOR OVERFLOWING VECTORS
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C                                       MOVE DIMENSION OF STAT FROM DPDEX2
C     UPDATED         --JUNE      1990. CORRECT 205 COMPILE ERROR
C     UPDATED         --APRIL     1992. COMMENT OUT NX
C     UPDATED         --MARCH     1995. ADD MAD AND AAD STATISTICS
C     UPDATED         --NOVEMBER  1998. ADD PERCENTILE STATISTICS
C     UPDATED         --NOVEMBER  1998. ADD CPM AND CC STATISTIC
C     UPDATED         --MARCH     1999. ADD CNPK STATISTIC
C     UPDATED         --NOVEMBER  2001. ADD BIWEIGHT LOCATION STATISTIC
C     UPDATED         --NOVEMBER  2001. ADD BIWEIGHT SCALE STATISTIC
C     UPDATED         --NOVEMBER  2001. ADD IQ RANGE STATISTIC
C     UPDATED         --NOVEMBER  2001. ADD HARMONIC MEAN STATISTIC
C     UPDATED         --NOVEMBER  2001. ADD GEOMETRIC MEAN STATISTIC
C     UPDATED         --NOVEMBER  2001. ADD GEOMETRIC SD STATISTIC
C     UPDATED         --JULY      2002. ADD WINSORIZED VARIANCE STATISTIC
C     UPDATED         --JULY      2002. ADD WINSORIZED SD STATISTIC
C     UPDATED         --JULY      2002. ADD WINSORIZED COVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD WINSORIZED CORRELATION PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCOVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCORRELATION PLOT
C     UPDATED         --JULY      2002. ADD PERCENTAGE BEND MIDVARIANCE
C                                           PLOT
C     UPDATED         --JULY      2002. ADD HODGES LEHMAN PLOT
C     UPDATED         --JULY      2002. ADD QUANTILE PLOT
C     UPDATED         --JULY      2002. ADD QUANTILE STANDARD ERROR PLOT
C     UPDATED         --JULY      2002. ADD TRIMMED MEAN STANDARD ERROR
C                                       PLOT
C     UPDATED         --APRIL     2003. ADD SN AND QN (REQUIRES
C                                       ADDITIONAL SCRATCH ARRAYS)
C     UPDATED         --OCTOBER   2004. ADD KEDNELLS TAU
C     UPDATED         --MAY       2007. ADD TRIMMED STANDARD DEVI
C     UPDATED         --AUGUST    2007. MOVE SOME ARRAY STORAGE TO COMMON
C     UPDATED         --AUGUST    2007. ADJUST SOME ARRAY DIMENSIONS
C                                       (PROBLEM WITH LARGE SAMPLES)
C     UPDATED         --NOVEMBER  2007. DOUBLE PRECISION ARRAYS FOR
C                                       CMPSTA
C     UPDATED         --NOVEMBER  2007. LP LOCATION
C     UPDATED         --NOVEMBER  2007. VARIANCE OF LP LOCATION
C     UPDATED         --NOVEMBER  2007. SD OF LP LOCATION
C     UPDATED         --SEPTEMBER 2008. BINOMIAL PROBABILITY
C     UPDATED         --FEBRUARY  2009. GRUBB
C     UPDATED         --FEBRUARY  2009. GRUBB CDF
C     UPDATED         --FEBRUARY  2009. ONE SAMPLE T TEST
C                                       ONE SAMPLE T TEST CDF
C     UPDATED         --FEBRUARY  2009. CHI-SQUARE SD TEST
C                                       CHI-SQUARE SD TEST CDF
C     UPDATED         --FEBRUARY  2009. FREQUENCY TEST
C                                       FREQUENCY TEST CDF
C     UPDATED         --FEBRUARY  2009. FREQUENCY WITHIN A BLOCK TEST
C                                       FREQUENCY WITHIN A BLOCK TEST CDF
C     UPDATED         --MARCH     2009. PARSE USING "EXTSTA"
C     UPDATED         --JUNE      2010. ACCOMODATE 3 RESPONSE VARIABLES
C                                       FOR STATISTICS
C     UPDATED         --JANUARY   2018. ADD DEX ORDERED PLOT
C     UPDATED         --FEBRUARY  2019. ADD DEX INTERACTION <STAT> PLOT
C     UPDATED         --FEBRUARY  2019. ADD CLASSIFICATION <STAT> PLOT
C     UPDATED         --FEBRUARY  2019. RECODE FOR MORE EFFICIENT USE OF
C                                       STORAGE
C     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 IDEXPA
      CHARACTER*4 IDEXYO
      CHARACTER*4 IDEXEF
      CHARACTER*4 IDEXOP
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASP3
      CHARACTER*4 ICASP9
      CHARACTER*4 IOP
      CHARACTER*16 CVAL
C
      PARAMETER (MAXSPN=99)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
C
      CHARACTER*40 INAME
      CHARACTER*60 ISTANM
      CHARACTER*4  ISTADF
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP2B(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION STAT(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
      DIMENSION TEMPZ(MAXOBV)
      DIMENSION TEMPZ2(MAXOBV)
      DIMENSION TEMPFA(MAXOBV)
      DIMENSION TEMPCO(MAXOBV)
      DIMENSION X1(10*MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),Y3(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2B(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMPZ(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMPZ2(1))
      EQUIVALENCE (GARBAG(IGAR10),TEMPFA(1))
      EQUIVALENCE (GARBAG(JGAR11),TEMPCO(1))
      EQUIVALENCE (GARBAG(JGAR12),XTEMP1(1))
      EQUIVALENCE (GARBAG(JGAR13),XTEMP2(1))
      EQUIVALENCE (GARBAG(JGAR14),XTEMP3(1))
      EQUIVALENCE (GARBAG(JGAR15),STAT(1))
      EQUIVALENCE (GARBAG(JGAR16),X1(1))
C
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
      INTEGER ITEMP4(MAXOBV)
      INTEGER ITEMP5(MAXOBV)
      INTEGER ITEMP6(MAXOBV)
      INTEGER INDX(MAXOBV)
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
      EQUIVALENCE (IGARBG(IIGAR7),INDX(1))
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCODE.INC'
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
      ICASPL='-999'
      ISUBN1='DPDE'
      ISUBN2='XP  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      MAX10=10*MAXOBV
      MAXV2=2
      MINN2=2
      ICOLL=0
      ICOLX=0
      ICOLXI=0
      NUMVAR=0
      NUMCOM=0
      NUMFAC=0
C
      IDEXPA='NONP'
      IDEXYO='NONY'
      IDEXEF='STAT'
      IDEXOP='NOOP'
      ICASP9='DEX '
      IF(ICOM.EQ.'CLAS')ICASP9='CLAS'
C
C               *********************************************
C               **  TREAT THE DEX ... STATISTIC PLOT CASE  **
C               *********************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DEXP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEXP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ISUBRO,IBUGG2,IBUGG3,IBUGQ
   52   FORMAT('ISUBRO,IBUGG2,IBUGG3,IBUGQ  = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)DEXWID,IDEXDE,IDEXHA
   54   FORMAT('DEXWID,IDEXDE,IDEXHA = ',G15.7,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IDEXPA,IDEXYO,IDEXEF
   55   FORMAT('IDEXPA,IDEXYO,IDEXEF = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 11--            **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
C               *********************************
C               **  STEP 12--                  **
C               **  DETERMINE IF OF THIS TYPE  **
C               **  AND BRANCH ACCORDINGLY.    **
C               *********************************
C
      ISTEPN='12'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DEXP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
C
C     CHECK FOR SPECIAL CASES:
C     1) DEX SCATTER PLOT
C     2) DEX SIGN PLOT
C     3) DEX ORDERED PLOT
C     4) CLASSIFICATION SCATTER PLOT
C
      ISTANR=1
      IF(IHARG(1).EQ.'SCAT'.AND.IHARG2(1).EQ.'TER '.AND.
     1   IHARG(2).EQ.'PLOT')THEN
        IFOUND='YES'
        ILASTC=2
        ICASPL='SCAT'
      ELSEIF(IHARG(1).EQ.'SIGN'.AND.IHARG2(1).EQ.'    '.AND.
     1       IHARG(2).EQ.'PLOT')THEN
        IFOUND='YES'
        ILASTC=2
        ICASPL='SIGN'
        IF(ICASP9.EQ.'CLAS')GOTO9000
      ELSEIF(IHARG(1).EQ.'ORDE'.AND.IHARG(2).EQ.'PLOT')THEN
        IFOUND='YES'
        ILASTC=2
        IDEXOP='ORDE'
        ICASPL='ORDE'
        IF(ICASP9.EQ.'CLAS')GOTO9000
      ELSE
C
CCCCC   MARCH 2009: USE "EXTSTA" TO PARSE.
C
        JMIN=1
        JMAX=-1
        DO200I=JMIN,NUMARG
          IF(IHARG(I).EQ.'EFFE' .AND. IDEXEF.NE.'ABSO')THEN
            IDEXEF='EFFE'
            IF(ICASP9.EQ.'CLAS')GOTO9000
          ELSEIF(IHARG(I).EQ.'ABSO')THEN
            IDEXEF='ABSO'
            IF(ICASP9.EQ.'CLAS')GOTO9000
          ELSEIF(IHARG(I).EQ.'PARE')THEN
            IDEXPA='PARE'
            IF(ICASP9.EQ.'CLAS')GOTO9000
          ELSEIF(IHARG(I).EQ.'YOUD')THEN
            IDEXYO='YOUD'
            IF(ICASP9.EQ.'CLAS')GOTO9000
          ELSEIF(IHARG(I).EQ.'PLOT')THEN
            IF(JMAX.LT.0)JMAX=I-1
            ILASTC=I
            GOTO209
          ENDIF
  200   CONTINUE
        IFOUND='NO'
        GOTO9000
  209   CONTINUE
        JMAX=MIN(JMAX,JMIN+6)
C
        CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,
     1              ICASPL,ISTANM,ISTANR,ISTADF,IFOUND,ILOCV,
     1              ISUBRO,IBUGG3,IERROR)
C
        IF(IFOUND.EQ.'NO')GOTO9000
C
        IF(ICASP9.EQ.'CLAS' .AND. ISTANR.GT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3081)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('      FOR THE CLASSIFICATION PLOT, ONLY STATISTICS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      BASED ON A SINGLE RESPONSE VARIABLE ARE ',
     1           'SUPPORTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,105)ISTANR,ISTANM
  105     FORMAT('      ',I1,' RESPONSE VARIABLES REQUIRED FOR ',A60)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
      ENDIF
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      INAME='DEX ... PLOT'
      IF(ICASP9.EQ.'CLAS')INAME='CLASSICIATION ... PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=-99
      MAXNVA=-99
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DEXP')THEN
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
      NUMFAC=NUMVAR-ISTANR
      NUMCOM=ISTANR+1
      IF(NUMFAC.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3081)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2312)
 2312   FORMAT('      THE NUMBER OF FACTORS MUST BE AT LEAST 1;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2319)
 2319   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2321)NUMFAC
 2321   FORMAT('      THE SPECIFIED NUMBER OF FACTORS WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2323)
 2323   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2324)(IANS(I),I=1,MIN(IWIDTH,80))
 2324     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IMAX=NRIGHT(1)
      IF(NQ.LT.NRIGHT(1))IMAX=NQ
      IUNC=0
      IF(IDEXOP.EQ.'ORDE' .AND. IDPAUN.EQ.'ON')IUNC=2
C
C     EXTRACT THE RESPONSE VARIABLES FIRST
C
      J=0
      DO3010I=1,IMAX
        IF(ISUB(I).EQ.0)GOTO3010
        J=J+1
        IF(ISTANR.GE.1)THEN
          IJ=MAXN*(ICOLR(1)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
        ENDIF
C
        IF(ISTANR.GE.2)THEN
          IJ=MAXN*(ICOLR(2)-1)+I
          IF(ICOLR(2).LE.MAXCOL)Y2(J)=V(IJ)
          IF(ICOLR(2).EQ.MAXCP1)Y2(J)=PRED(I)
          IF(ICOLR(2).EQ.MAXCP2)Y2(J)=RES(I)
          IF(ICOLR(2).EQ.MAXCP3)Y2(J)=YPLOT(I)
          IF(ICOLR(2).EQ.MAXCP4)Y2(J)=XPLOT(I)
          IF(ICOLR(2).EQ.MAXCP5)Y2(J)=X2PLOT(I)
          IF(ICOLR(2).EQ.MAXCP6)Y2(J)=TAGPLO(I)
        ENDIF
C
        IF(ISTANR.GE.3)THEN
          IJ=MAXN*(ICOLR(3)-1)+I
          IF(ICOLR(3).LE.MAXCOL)Y3(J)=V(IJ)
          IF(ICOLR(3).EQ.MAXCP1)Y3(J)=PRED(I)
          IF(ICOLR(3).EQ.MAXCP2)Y3(J)=RES(I)
          IF(ICOLR(3).EQ.MAXCP3)Y3(J)=YPLOT(I)
          IF(ICOLR(3).EQ.MAXCP4)Y3(J)=XPLOT(I)
          IF(ICOLR(3).EQ.MAXCP5)Y3(J)=X2PLOT(I)
          IF(ICOLR(3).EQ.MAXCP6)Y3(J)=TAGPLO(I)
        ENDIF
C
        IF(IDEXOP.EQ.'ORDE' .AND. IDPAUN.EQ.'ON')THEN
          IJ=MAXN*(ICOLR(ISTANR+1)-1)+I
          IF(ICOLR(ISTANR+1).LE.MAXCOL)TEMPFA(J)=V(IJ)
          IF(ICOLR(ISTANR+1).EQ.MAXCP1)TEMPFA(J)=PRED(I)
          IF(ICOLR(ISTANR+1).EQ.MAXCP2)TEMPFA(J)=RES(I)
          IF(ICOLR(ISTANR+1).EQ.MAXCP3)TEMPFA(J)=YPLOT(I)
          IF(ICOLR(ISTANR+1).EQ.MAXCP4)TEMPFA(J)=XPLOT(I)
          IF(ICOLR(ISTANR+1).EQ.MAXCP5)TEMPFA(J)=X2PLOT(I)
          IF(ICOLR(ISTANR+1).EQ.MAXCP6)TEMPFA(J)=TAGPLO(I)
C
          IJ=MAXN*(ICOLR(ISTANR+2)-1)+I
          IF(ICOLR(ISTANR+2).LE.MAXCOL)TEMPCO(J)=V(IJ)
          IF(ICOLR(ISTANR+2).EQ.MAXCP1)TEMPCO(J)=PRED(I)
          IF(ICOLR(ISTANR+2).EQ.MAXCP2)TEMPCO(J)=RES(I)
          IF(ICOLR(ISTANR+2).EQ.MAXCP3)TEMPCO(J)=YPLOT(I)
          IF(ICOLR(ISTANR+2).EQ.MAXCP4)TEMPCO(J)=XPLOT(I)
          IF(ICOLR(ISTANR+2).EQ.MAXCP5)TEMPCO(J)=X2PLOT(I)
          IF(ICOLR(ISTANR+2).EQ.MAXCP6)TEMPCO(J)=TAGPLO(I)
        ENDIF
 3010 CONTINUE
      NUMROW=J
C
      IF(NROW*NUMFAC.GT.MAX10)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3081)
 3081   FORMAT('***** ERROR IN DEX/CLASSIFICATION ... STATISTIC PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3082)
 3082   FORMAT('      THE INTERMEDIATE VECTORS BEING BUILT FOR ',
     1         'ENTRY INTO DPDEX2')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3083)
 3083   FORMAT('      HAVE GROWN TOO BIG.  IN PARTICULAR, THE ',
     1         'NUMBER OF FACTORS TIMES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3085)
 3085   FORMAT('      THE LENGTH OF EACH FACTOR HAS JUST EXCEEDED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3086)MAX10
 3086   FORMAT('      THE ALLOWABLE LIMIT OF = ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      J=0
      DO3160I=1,IMAX
        IF(ISUB(I).EQ.0)GOTO3160
        J=J+1
C
        DO3170K=1,NUMFAC
          IVALT=ISTANR+IUNC+K
          IVALT2=(K-1)*NUMROW
          ICOLX=ICOLR(IVALT)
          IJ=MAXN*(ICOLX-1)+I
          IF(ICOLR(IVALT).LE.MAXCOL)X1(IVALT2+J)=V(IJ)
          IF(ICOLR(IVALT).EQ.MAXCP1)X1(IVALT2+J)=PRED(I)
          IF(ICOLR(IVALT).EQ.MAXCP2)X1(IVALT2+J)=RES(I)
          IF(ICOLR(IVALT).EQ.MAXCP3)X1(IVALT2+J)=YPLOT(I)
          IF(ICOLR(IVALT).EQ.MAXCP4)X1(IVALT2+J)=XPLOT(I)
          IF(ICOLR(IVALT).EQ.MAXCP5)X1(IVALT2+J)=X2PLOT(I)
          IF(ICOLR(IVALT).EQ.MAXCP6)X1(IVALT2+J)=TAGPLO(I)
 3170   CONTINUE
 3160 CONTINUE
C
C     CREATE INTERACTION TERMS FOR THE FACTORS.
C
C     STEP 1: 2-TERM INTERACTIONS
C
      IF(IDEXIN.EQ.'2' .OR. IDEXIN.EQ.'3')THEN
C
        IOP='OPEN'
        IFLAG1=0
        IFLAG2=0
        IFLAG3=0
        IFLAG4=0
        IFLAG5=1
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        DO3201K=1,NUMFAC
          CVAL='X'
          IF(K.LE.9)THEN
            WRITE(CVAL(2:2),'(I1)')K
          ELSEIF(K.LE.99)THEN
            WRITE(CVAL(2:3),'(I2)')K
          ELSEIF(K.LE.999)THEN
            WRITE(CVAL(2:4),'(I3)')K
          ENDIF
          WRITE(IOUNI5,'(A4)')CVAL(1:4)
 3201   CONTINUE
C
C       2-TERM INTERACTIONS
C
        NUMFA2=NUMFAC
        IF(NUMFAC.LT.2)GOTO3299
        DO3210K=1,NUMFAC-1
          IINDX1=NUMROW*(K-1)
          KP1=K+1
          DO3220L=KP1,NUMFAC
            IINDX2=NUMROW*(L-1)
            IINDX3=NUMROW*NUMFA2
            DO3230I=1,NUMROW
              AVAL1=X1(IINDX1+I)
              AVAL2=X1(IINDX2+I)
              AVAL=AVAL1*AVAL2
              X1(IINDX3+I)=AVAL
 3230       CONTINUE
            NUMFA2=NUMFA2+1
            CVAL='X'
            IF(K.LE.9)THEN
              WRITE(CVAL(2:2),'(I1)')K
              NSTOP=2
            ELSEIF(K.LE.99)THEN
              WRITE(CVAL(2:3),'(I2)')K
              NSTOP=3
            ELSEIF(K.LE.999)THEN
              WRITE(CVAL(2:4),'(I3)')K
              NSTOP=4
            ENDIF
            NSTOP=NSTOP+1
            CVAL(NSTOP:NSTOP+1)='*X'
            NSTOP=NSTOP+2
            IF(L.LE.9)THEN
              WRITE(CVAL(NSTOP:NSTOP),'(I1)')L
            ELSEIF(L.LE.99)THEN
              WRITE(CVAL(NSTOP:NSTOP+1),'(I2)')L
              NSTOP=NSTOP+1
            ELSEIF(L.LE.999)THEN
              WRITE(CVAL(NSTOP:NSTOP+2),'(I3)')L
              NSTOP=NSTOP+2
            ENDIF
            WRITE(IOUNI5,'(A9)')CVAL(1:9)
 3220     CONTINUE
 3210   CONTINUE
 3299   CONTINUE
C
C     STEP 2: 3-TERM INTERACTIONS
C
        IF(IDEXIN.EQ.'2' .OR. NUMFAC.LT.3)GOTO3399
        DO3310K=1,NUMFAC-2
          IINDX1=NUMROW*(K-1)
          KP1=K+1
          DO3320L=KP1,NUMFAC-1
            IINDX2=NUMROW*(L-1)
            LP1=L+1
            DO3330M=LP1,NUMFAC
              IINDX3=NUMROW*(M-1)
              IINDX4=NUMROW*NUMFA2
              DO3340I=1,NUMROW
                AVAL1=X1(IINDX1+I)
                AVAL2=X1(IINDX2+I)
                AVAL3=X1(IINDX3+I)
                AVAL=AVAL1*AVAL2*AVAL3
                X1(IINDX4+I)=AVAL
 3340         CONTINUE
              NUMFA2=NUMFA2+1
              CVAL='X'
              IF(K.LE.9)THEN
                WRITE(CVAL(2:2),'(I1)')K
                NSTOP=2
              ELSEIF(K.LE.99)THEN
                WRITE(CVAL(2:3),'(I2)')K
                NSTOP=3
              ELSEIF(K.LE.999)THEN
                WRITE(CVAL(2:4),'(I3)')K
                NSTOP=4
              ENDIF
              NSTOP=NSTOP+1
              CVAL(NSTOP:NSTOP+1)='*X'
              NSTOP=NSTOP+2
              IF(L.LE.9)THEN
                WRITE(CVAL(NSTOP:NSTOP),'(I1)')L
              ELSEIF(L.LE.99)THEN
                WRITE(CVAL(NSTOP:NSTOP+1),'(I2)')L
                NSTOP=NSTOP+1
              ELSEIF(L.LE.999)THEN
                WRITE(CVAL(NSTOP:NSTOP+2),'(I3)')L
                NSTOP=NSTOP+2
              ENDIF
              NSTOP=NSTOP+1
              CVAL(NSTOP:NSTOP+1)='*X'
              NSTOP=NSTOP+2
              IF(M.LE.9)THEN
                WRITE(CVAL(NSTOP:NSTOP),'(I1)')M
              ELSEIF(M.LE.99)THEN
                WRITE(CVAL(NSTOP:NSTOP+1),'(I2)')M
                NSTOP=NSTOP+1
              ELSEIF(M.LE.999)THEN
                WRITE(CVAL(NSTOP:NSTOP+2),'(I3)')M
                NSTOP=NSTOP+2
              ENDIF
              WRITE(IOUNI5,'(A16)')CVAL(1:16)
 3330       CONTINUE
 3320     CONTINUE
 3310   CONTINUE
 3399   CONTINUE
C
        NUMFAC=NUMFA2
C
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGG3,ISUBRO,IERROR)
      ENDIF
C
C               *********************************************************
C               **  STEP 43--                                          **
C               **  COMPUTE THE APPROPRIATE STATISTIC PLOT STATISTIC-- **
C               **  (MEAN, STANDARD DEVIATION, RANGE, OR CUSUM).       **
C               **  COMPUTE CONFIDENCE LINES.                          **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS              **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.                 **
C               **  DEFINE THE VECTOR D(.) TO 1'S, 2'S, AND 3'S        **
C               **  FOR THE PLOTTED VALUE, THE LOWER CONFIDENCE LINE,  **
C               **  AND THE UPPER CONFIDENCE LINE.                     **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).      **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).      **
C               *********************************************************
C
      ISTEPN='43'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DEXP')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,4201)NUMROW,NUMFAC,NUMCOM
 4201   FORMAT('BEFORE CALL DPDEX2: NUMROW,NUMFAC,NUMCOM=',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL DPDEX2(Y1,Y2,Y3,X1,NUMROW,NUMCOM,NUMFAC,ICASPL,ICASP9,
     1            IDEXHA,IDEXDE,IDEXPA,IDEXYO,IDEXEF,DEXWID,
     1            IDPADI,IDPAID,IDPAUN,IDEXOP,
     1            TEMP,TEMPZ,TEMPZ2,TEMP2,TEMP2B,TEMP3,
     1            TEMPFA,TEMPCO,
     1            XTEMP1,XTEMP2,XTEMP3,MAXNXT,
     1            ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1            INDX,DTEMP1,DTEMP2,DTEMP3,
     1            IQUAME,IQUASE,PSTAMV,
     1            Y,X,D,NPLOTP,NPLOTV,STAT,
     1            ISUBRO,IBUGG3,IERROR)
C
      print *,'after dpdex2: icaspl,icasp9 = ',icaspl,icasp9
      ICASP3=ICASPL
      ICASPL='DEXP'
      IF(ICASP3.EQ.'SIGN')ICASPL='DEXS'
      IF(IDEXEF.EQ.'EFFE'.OR.IDEXEF.EQ.'ABSO')ICASPL='DEXE'
      IF(IDEXYO.EQ.'YOUD')ICASPL='DEXY'
      IF(IDEXOP.EQ.'ORDE')ICASPL='DEXO'
      IF(ICASP3.EQ.'SCAT' .AND. ICASP9.EQ.'CLAS')ICASPL='CLAS'
      print *,'before exit: icaspl = ',icaspl
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DEXP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEXP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',2(A4,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NUMROW,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NUMROW,ICASPL,IAND1,IAND2 = ',
     1         3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO9025I=1,NPLOTP
            WRITE(ICOUT,9026)I,Y(I),X(I),D(I)
 9026       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
        WRITE(ICOUT,9033)ICASP3,ICASPL,ICASP9
 9033   FORMAT('ICASP3,ICASPL,ICASP9 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEX2(Y1,Y2RESP,Y3,X,N,NUMCOM,NUMFAC,ICASPL,ICASP9,
     1                  IDEXHA,IDEXDE,IDEXPA,IDEXYO,IDEXEF,DEXWID,
     1                  IDPADI,IDPAID,IDPAUN,IDEXOR,
     1                  TEMP,TEMPZ,TEMPZ2,TEMPXI,TEMPX2,XIDTEM,
     1                  YUNCLW,YUNCUP,
     1                  XTEMP1,XTEMP2,XTEMP3,MAXNXT,
     1                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  INDX,DTEMP1,DTEMP2,DTEMP3,
     1                  IQUAME,IQUASE,PSTAMV,
     1                  Y2,X2,D2,N2,NPLOTV,STAT,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS THAT WILL DEFINE A
C              A DESIGN OF EXPERIMENT PLOT OF THE FOLLOWING TYPES--
C                 DEX SCATTER PLOT (NOT A STATISTIC)
C                 DEX SIGN PLOT (NOT A STATISTIC)
C                 DEX ORDERED PLOT (NOT A STATISTIC)
C                 DEX <STAT> XXX YYY  PLOT
C              WHERE <STAT> IS ONE OF DATAPLOT'S SUPPORTED STATISTICS
C                    (CURRENTLY LIMITED TO STATISTICS BASED ON A
C                    SINGLE RESPONSE VARIABLE)
C              AND WHERE XXX MAY BE
C                    (OMITTED)
C                    EFFECTS
C                    ABSOLUTE EFFECTS
C              AND WHERE YYY MAY BE
C                    (OMITTED)
C                    PARETO
C                    YOUDEN
C
C     NOTE--IDEXHA = FACT/TERM "HORIZONTAL" SWITCH.  IF IDEXHA = FACT
C                    (THE DEFAULT), THEN THE HORIZONTAL AXIS WILL CONSIST
C                    OF FACTORS.
C                    IF IDEXHA = TERM THEN THE HORIZONTAL AXIS WILL
C                    CONSIST OF TERMS
C                       1 = FOR MAIN EFFECTS
C                       2 = (FOR 2-TERM INTERACTIONS)
C                       3 = (FOR 3-TERM INTERACTIONS)
C                       ETC.
C     NOTE--IDEXDE = DEPTH INTO THE INTERACTION TERMS
C                  = NUMBER OF TERMS IN INTERACTIONS
C                    IF IDEXDE = 1, GET ONLY MAIN FACTORS,;
C                    IF IDEXDE = 2, GET MAIN FACTORS &
C                    2-TERM INTERACTION FACTORS;
C                    IF NTEXTE = 3, GET MAIN FACTORS &
C                    2-TERM & 3-TERM INTERACTION FACTORS;
C                    ETC.
C     NOTE--IDEXPA = PARE/NONP PARETO SWITCH.
C                    IF IDEXPA = PARE, THEN A PARETO PLOT OF THE STATS
C                    OR EFFECTS WILL BE FORMED.
C                    IF IDEXPA = NONP (THE DEFAULT), THEN NO PARETO PLOT
C                    WILL BE FORMED AND SO THE STAT RESULTS WILL BE
C                    PRESENTED IN "NATURAL" ORDER.
C     NOTE--IDEXYO = YOUD/NONY YOUDEN SWITCH.
C                    IF IDEXYO = YOUD, THEN A YOUDEN PLOT OF THE STATS
C                    OR EFFECTS WILL BE FORMED.
C                    IF IDEXYO = NONY (THE DEFAULT),
C                    THEN NO YOUDEN PLOT WILL BE FORMED.
C     NOTE--IDEXOR = ORDE/NOOD ORDERED POT SWITCH.
C                    IF IDEXOR = ORDE, THEN ESSENTIALLY HAVE A PARETO
C                    PLOT OF THE RAW DATA (DISTINCTION IS THAT THE
C                    DATA IS NOT AVERAGED).
C     NOTE--IDEXEF = STAT/EFFE/ABS "EFFECTS" PLOT.
C                    IF IDEXEF = STAT THEN NO DIFFERENCING WILL BE DONE
C                    AND THE INDIVIDUAL STAT VALUES WILL BE PRESENTED FOR
C                    EACH LEVEL OF EACH FACTOR.
C                    IF IDEXEF = EFFE AND THE NUMBER OF LEVELS WITHIN A
C                    FACTOR IS 2, THEN THE STAT AT THE LOW SIDE WILL BE
C                    SUBTRACTED FROM THE STAT AT THE HIGH SIDE AND
C                    PRESENTED AS THE SINGLE RESULT FOR A FACTOR.  THIS
C                    RESULT MAY BE + OR -).
C                    IF IDEXEF = EFFE AND THE NUMBER OF LEVELS WITHIN A
C                    FACTOR IS 3 OR MORE, THEN THE MIN VALUE OF THE
C                    STATISTIC WILL BE SUBTRACTED FROM THE MAX VALUE OF
C                    THE STATISTIC AND PRESENTED AS THE SINGLE RESULT FOR
C                    A FACTOR (THIS RESULT WILL BE + OR 0).
C                    IF IDEXEF = ABS,
C                    THEN THE ABSOLUTE VALUE OF THE EFFECT (AS DEFINED ABOVE)
C                    WILL BE COMPUTED AND PRESENTED AS THE SINGLE RESULT.
C     NOTE--DEXWID = WIDTH (ON THE PLOT) THAT WILL SPAN THE LEVELS
C                    (SETTINGS) WITHIN A FACTOR; DEXWID SHOULD BE BETWEEN
C                    0 AND 1; THE DEFAULT VALUE IS .4 (THEREFORE DATA
C                    WILL TAKE .4 AND THERE WILL BE A .6 SPACING BETWEEN
C                    LARGEST LEVEL OF ONE FACTOR AND SMALLEST LEVEL OF
C                    NEXT FACTOR).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/6
C     ORIGINAL VERSION--MAY       1988.
C     UPDATED         --JANUARY   1990. CHECK FOR OVERFLOW GRAPHICS VECTOR
C     UPDATED         --JUNE      1990. MOVE DIMENSION OF STAT TO DPDEXP
C     UPDATED         --APRIL     1992. MAX10 TO MAXPOP
C     UPDATED         --MAY       1995. ADD MAD AND AAD STATISTICS
C     UPDATED         --NOVEMBER  1998. ADD PERCENTILE STATISTICS
C     UPDATED         --NOVEMBER  1998. ADD CM AND CC STATISTICS
C     UPDATED         --MARCH     1999. ADD CNPK STATISTIC
C     UPDATED         --JULY      2002. ADD WINSORIZED COVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD WINSORIZED CORRELATION PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCOVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD PERCENTAGE BEND MIDVARIANCE
C                                           PLOT
C     UPDATED         --JULY      2002. ADD HODGES LEHMAN PLOT
C     UPDATED         --APRIL     2003. ADD SN AND QN (REQUIRED
C                                       ADDITIONAL SCRATCH ARRAYS)
C     UPDATED         --JANUARY   2018. UPDATES FOR PARETO OPTION AND
C                                       ADD ORDERED PLOT
C     UPDATED         --FEBRUARY  2019. CLASSIFICATION PLOT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP9
      CHARACTER*4 IQUAME
      CHARACTER*4 IQUASE
      CHARACTER*4 IDEXHA
      CHARACTER*4 IDEXPA
      CHARACTER*4 IDEXYO
      CHARACTER*4 IDEXEF
      CHARACTER*4 IDEXOR
      CHARACTER*4 IDPADI
      CHARACTER*4 IDPAID
      CHARACTER*4 IDPAUN
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASP2
      CHARACTER*4 IWRITE
      CHARACTER*4 IOP
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(*)
      DIMENSION Y2RESP(*)
      DIMENSION Y3(*)
      DIMENSION X(N,NUMFAC)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION TEMPXI(*)
      DIMENSION TEMPX2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION YUNCLW(*)
      DIMENSION YUNCUP(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
      INTEGER INDX(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
CCCCC JUNE, 1990.  STAT DIMENSIONED IN DPDEXP
CCCCC DIMENSION STAT(MAXOBV)
      DIMENSION STAT(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
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
      ISUBN1='DPDE'
      ISUBN2='X2  '
      IERROR='NO'
      IWRITE='OFF'
      IOP='NULL'
      NUMROW=N
      NUMCOL=NUMFAC
C
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=0
      IF(IDEXOR.EQ.'ORDE')IFLAG2=1
      IFLAG3=0
      IFLAG4=0
      IF(ICASP9.EQ.'CLAS')IFLAG4=1
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(ICASP9.EQ.'CLAS')THEN
        WRITE(IOUNI4,21)
   21   FORMAT('MEAN',11X,'SD',13X,'MINIMUM',8X,'MAXIMUM')
      ENDIF
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN DEX ... PLOT--')
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
        GOTO9000
      ENDIF
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPDEX2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IBUGG3,ISUBRO,IERROR,ICASPL,ICASP9
   71   FORMAT('IBUGG3,ISUBRO,IERROR,ICASPL,ICASP9 = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,NUMCOM,NUMFAC
   72   FORMAT('N,NUMCOM,NUMFAC = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)IDEXHA,IDEXPA,IDEXYO,IDEXEF,IDEXOR,IDEXDE,DEXWID
   74   FORMAT('IDEXHA,IDEXPA,IDEXYO,IDEXEF,IDEXOR,IDEXDE,DEXWID = ',
     1         5(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO75I=1,N
          WRITE(ICOUT,76)I,Y1(I),Y2RESP(I),(X(I,J),J=1,MIN(NUMFAC,10))
   76     FORMAT('I,Y1(I),Y2RESP(I)(X(I,J),J=1,10) = ',I8,12G15.7)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
C
C               ********************************************************
C               **  STEP 11--                                         **
C               **  SET UP A GLOBAL LOOP TO STEP THROUGH              **
C               **  THE NUMFAC FACTOR ID'S IN TAG(.)                  **
C               ********************************************************
C
      ANUMFA=NUMFAC
      ITAG=0
      J=0
      CALL MEAN(Y1,N,IWRITE,YBAR,IBUGG3,IERROR)
C
C     HANDLE "DEX ORDERED PLOT" SEPARATELY.  FOR THIS CASE,
C     GENERATE ONE OUTPUT POINT FOR EACH ROW IN THE DATA MATRIX.
C     EACH FACTOR HAS THE SAME NUMBER OF POINTS, SO USE THIS TO
C     DETERMINE HOW MANY POINTS TO GENERATE.
C
      IF(IDEXOR.EQ.'ORDE')THEN
        DO1010I=1,N
          J=J+1
          Y2(J)=Y1(I)
          X2(J)=REAL(J)
          D2(J)=1.0
 1010   CONTINUE
        GOTO1199
      ENDIF
C
      NUMSET=0
      IF(ICASP9.EQ.'CLAS')THEN
        CALL DISTIN(Y1,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        NMAX=20
        NTEMP=N/2
        IF(NUMSET.GT.NMAX)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1103)NMAX
 1103     FORMAT('      THE NUMBER OF UNIQUE VALUES FOR THE ',
     1           'RESPONSE VARIABLE IS GREATER THAN ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(NUMSET.GT.NTEMP)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1107)
 1107     FORMAT('      THE NUMBER OF UNIQUE VALUES FOR THE RESPONSE',
     1           ' VARIABLE IS GREATER THAN N/2')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1108)N
 1108     FORMAT('      THE SAMPLE SIZE (N) = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1109)NNUMSET
 1109     FORMAT('      THE NUMBER OF UNIQUE VALUES IN THE RESPONSE ',
     1           'VARIABLE = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      DO1100IFAC=1,NUMFAC
        AFAC=IFAC
C
C               ********************************************************
C               **  STEP 12--                                         **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
C               **  FOR THE GROUP VARIABLE (USUALLY VAR. 2)           **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
C               **  WHICH IS AN ERROR CONDITION FOR A PLOT.           **
C               ********************************************************
C
C       2019/02: FOR CLASSIFICATION PLOT, BASE GROUPS ON DISTINCT
C                VALUES OF Y (RESPONSE VARIABLE).
C
        ISTEPN='12'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(ICASP9.NE.'CLAS')THEN
          NUMSET=0
          DO1210I=1,NUMROW
            IF(NUMSET.EQ.0)GOTO1220
            DO1215I2=1,NUMSET
              IF(X(I,IFAC).EQ.XIDTEM(I2))GOTO1210
 1215       CONTINUE
 1220       CONTINUE
            NUMSET=NUMSET+1
            XIDTEM(NUMSET)=X(I,IFAC)
 1210     CONTINUE
        ENDIF
C
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
        AN=N
        ANUMSE=NUMSET
C
        IF(NUMSET.EQ.1 .OR. IDEXEF.EQ.'EFFE' .OR.
     1     IDEXEF.EQ.'ABSO')THEN
          A0=0.0
          A1=1.0
          DENOM=1.0
        ELSE
          A0=(-DEXWID/2.0)
          A1=DEXWID
          DENOM=ANUMSE-1.0
        ENDIF
C
        IF(NUMSET.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1252)
 1252     FORMAT('      NUMBER OF SETS    NUMSET = 0 ')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(NUMSET.EQ.N)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1262)
 1262     FORMAT('      NUMBER OF SETS    NUMSET   IDENTICAL TO ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1263)
 1263     FORMAT('      NUMBER OF OBSERVATIONS   N   .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1264)NUMSET
 1264     FORMAT('      NUMSET = N = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
C               ******************************************
C               **  STEP 21--                           **
C               **  FOR ALL CASES,                      **
C               **  COMPUTE THE SPECIFIED STATISTIC     **
C               **  FOR EACH LEVEL OF EACH FACTOR,      **
C               ******************************************
C
        ISTEPN='21'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        YMIN=CPUMAX
        YMAX=CPUMIN
C
        IF(ICASP9.EQ.'CLAS')THEN
          DO5100ISET=1,NUMSET
C
            K=0
            DO5110I=1,NUMROW
              IF(Y1(I).NE.XIDTEM(ISET))GOTO5110
              K=K+1
              TEMP(K)=X(I,IFAC)
C
              IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
                WRITE(ICOUT,5111)NUMSET,ISET,J,XIDTEM(ISET)
 5111           FORMAT('NUMSET,ISET,J,XIDTEM(ISET) = ',3I6,G12.4)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,5112)N,I,K,X(I,IFAC),Y1(I),TEMP(K)
 5112           FORMAT('N,I,K,X(I),Y(I),XINT(I),TEMP(K),TEMPXI(K) = ',
     1               3I6,5G12.4)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
 5110       CONTINUE
            NI=K
            NS2=NI
C
            IF(NS2.LT.1)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,31)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,5122)
 5122         FORMAT('NI FOR SOME CLASS = 0')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,5123)ISET,XIDTEM(ISET),NI
 5123         FORMAT('ISET,XIDTEM(ISET),NI = ',I8,E15.7,I8)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
C
C           COMPUTE MIN, MAX, MEAN, SD AND WRITE TO IOUNI4
C
            CALL MEAN(TEMP,NI,IWRITE,XMEAN,IBUGG3,IERROR)
            CALL SD(TEMP,NI,IWRITE,XSD,IBUGG3,IERROR)
            CALL MINIM(TEMP,NI,IWRITE,XMIN,IBUGG3,IERROR)
            CALL MAXIM(TEMP,NI,IWRITE,XMAX,IBUGG3,IERROR)
            WRITE(IOUNI4,'(4E15.7)')XMEAN,XSD,XMIN,XMAX
C
            IF(ICASPL.EQ.'SCAT')THEN
C
              ITAG=ITAG+1
              DO5131L=1,NS2
                J=J+1
C
                IF(J.GT.MAXPOP)THEN
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,31)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,5182)
 5182             FORMAT('      THE OUTPUT GRAPHICS VECTORS BEING ',
     1                 'BUILT HAVE GROWN TOO BIG.')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,5184)
 5184             FORMAT('      THE NUMBER OF FACTORS TIMES THE ',
     1                 'LENGTH OF EACH FACTOR HAS')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,5186)MAXPOP
 5186             FORMAT('      JUST EXCEEDED THE ALLOWABLE LIMIT OF ',
     1                   I8)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,5188)
 5188             FORMAT('      SUGGESTION--GENERATE MULTIPLE DEX ',
     1                 'SCATTER PLOTS.')
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GOTO9000
                ENDIF
C
                Y2(J)=TEMP(L)
                ASET=ISET
                IF(NUMSET.EQ.1)X2(J)=AFAC
                IF(NUMSET.NE.1)X2(J)=AFAC+A0+A1*(ASET-1.0)/DENOM
                D2(J)=ITAG
 5131         CONTINUE
              GOTO5100
            ENDIF
C
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
              WRITE(ICOUT,5151)NS2
 5151         FORMAT('AT 5150: BEFORE CALL DPDEX3: NS2 = ',I8)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            CALL DPDEX3(TEMP,TEMPXI,TEMPX2,XTEMP1,XTEMP2,XTEMP3,
     1                  NS2,MAXNXT,ICASPL,
     1                  RIGHT,
     1                  IQUAME,IQUASE,PSTAMV,
     1                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,
     1                  ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  IBUGG3,ISUBRO,IERROR)
C
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
              WRITE(ICOUT,5153)RIGHT
 5153         FORMAT('AT 5150: AFTER CALL DPDEX3: RIGHT = ',G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(IERROR.EQ.'YES')GOTO9000
C
            IF(RIGHT.LT.YMIN)YMIN=RIGHT
            IF(RIGHT.GT.YMAX)YMAX=RIGHT
            IF(ISET.EQ.NUMSET)THEN
              DEL=YMAX-YMIN
              RELDEL=0.0
              IF(YBAR.NE.0.0)RELDEL=100.*(DEL/YBAR)
              WRITE(IOUNI1,'(3E15.7)')DEL,RELDEL
            ENDIF
C
            J=J+1
            Y2(J)=RIGHT
            ASET=ISET
            IF(NUMSET.EQ.1)X2(J)=AFAC
            IF(NUMSET.NE.1)X2(J)=AFAC+A0+A1*(ASET-1.0)/DENOM
            D2(J)=AFAC
C
 5100     CONTINUE
          N2=J
          GOTO1100
        ENDIF
C
        DO2100ISET=1,NUMSET
C
          K=0
          DO2110I=1,NUMROW
            IF(X(I,IFAC).NE.XIDTEM(ISET))GOTO2110
            K=K+1
            TEMP(K)=Y1(I)
            TEMPXI(K)=Y2RESP(I)
            TEMPX2(K)=Y3(I)
C
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
              WRITE(ICOUT,2111)NUMSET,ISET,J,XIDTEM(ISET)
 2111         FORMAT('NUMSET,ISET,J,XIDTEM(ISET) = ',3I6,G12.4)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2112)N,I,K,X(I,IFAC),Y1(I),Y2(I),TEMP(K),TEMPXI(K)
 2112         FORMAT('N,I,K,X(I),Y(I),XINT(I),TEMP(K),TEMPXI(K) = ',
     1               3I6,5G12.4)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
 2110     CONTINUE
          NI=K
          NS2=NI
C
          IF(NS2.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2122)
 2122       FORMAT('NI FOR SOME CLASS = 0')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2123)ISET,XIDTEM(ISET),NI
 2123       FORMAT('ISET,XIDTEM(ISET),NI = ',I8,E15.7,I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(ICASPL.EQ.'SCAT')THEN
C
CCCCC       JUNE 2002: FOR DEX SCATTER PLOT, POINTS ASSOCIATED WITH
CCCCC                  A GIVEN LEVEL HAVE COMMON TAG (TO ALLOW EASY
CCCCC                  DRAWING OF CONNECTING LINE).
C
            ITAG=ITAG+1
            DO2131L=1,NS2
              J=J+1
C
              IF(J.GT.MAXPOP)THEN
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,31)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2182)
 2182           FORMAT('      THE OUTPUT GRAPHICS VECTORS BEING BUILT ',
     1                 'HAVE GROWN TOO BIG.')
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2184)
 2184           FORMAT('      THE NUMBER OF FACTORS TIMES THE LENGTH ',
     1                 'OF EACH FACTOR HAS')
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2186)MAXPOP
 2186           FORMAT('      JUST EXCEEDED THE ALLOWABLE LIMIT OF ',I8)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2188)
 2188           FORMAT('      SUGGESTION--GENERATE MULTIPLE DEX ',
     1                 'SCATTER PLOTS.')
                CALL DPWRST('XXX','BUG ')
                IERROR='YES'
                GOTO9000
              ENDIF
C
              Y2(J)=TEMP(L)
              ASET=ISET
              IF(NUMSET.EQ.1)X2(J)=AFAC
              IF(NUMSET.NE.1)X2(J)=AFAC+A0+A1*(ASET-1.0)/DENOM
              D2(J)=ITAG
 2131       CONTINUE
            GOTO2100
C
          ELSEIF(IDEXEF.EQ.'EFFE' .OR. IDEXEF.EQ.'ABSO' .OR.
     1           IDEXYO.EQ.'YOUD')THEN
            CALL DPDEX3(TEMP,TEMPXI,TEMPX2,XTEMP1,XTEMP2,XTEMP3,
     1                  NS2,MAXNXT,ICASPL,
     1                  RIGHT,
     1                  IQUAME,IQUASE,PSTAMV,
     1                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  IBUGG3,ISUBRO,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            STAT(ISET)=RIGHT
            GOTO2100
          ELSEIF(ICASPL.EQ.'SIGN')THEN
            DO2161L=1,NS2
              J=J+1
              Y2(J)=TEMP(L)
              ASET=ISET
              X2(J)=AFAC
              ASET=ISET
              D2(J)=ASET
 2161       CONTINUE
            GOTO2100
          ENDIF
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
            WRITE(ICOUT,2151)NS2
 2151       FORMAT('AT 2150: BEFORE CALL DPDEX3: NS2 = ',I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          CALL DPDEX3(TEMP,TEMPXI,TEMPX2,XTEMP1,XTEMP2,XTEMP3,
     1                NS2,MAXNXT,ICASPL,
     1                RIGHT,
     1                IQUAME,IQUASE,PSTAMV,
     1                ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                DTEMP1,DTEMP2,DTEMP3,
     1                IBUGG3,ISUBRO,IERROR)
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
            WRITE(ICOUT,2153)RIGHT
 2153       FORMAT('AT 2150: AFTER CALL DPDEX3: RIGHT = ',G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(IERROR.EQ.'YES')GOTO9000
C
          IF(RIGHT.LT.YMIN)YMIN=RIGHT
          IF(RIGHT.GT.YMAX)YMAX=RIGHT
          IF(ISET.EQ.NUMSET)THEN
            DEL=YMAX-YMIN
            RELDEL=0.0
            IF(YBAR.NE.0.0)RELDEL=100.*(DEL/YBAR)
            WRITE(IOUNI1,'(3E15.7)')DEL,RELDEL
          ENDIF
C
          J=J+1
          Y2(J)=RIGHT
          ASET=ISET
          IF(NUMSET.EQ.1)X2(J)=AFAC
          IF(NUMSET.NE.1)X2(J)=AFAC+A0+A1*(ASET-1.0)/DENOM
          D2(J)=AFAC
C
 2100   CONTINUE
C
C               ************************************************
C               **  STEP 22--                                 **
C               **  FOR THE EFFECTS & ABSOLUTE EFFECTS CASE,  **
C               **  COMPUTE THE EFFECT AND/OR                 **
C               **  ABSOLUTE EFFECT FOR EACH FACTOR           **
C               ************************************************
C
        IF(IDEXEF.EQ.'EFFE'.OR.IDEXEF.EQ.'ABSO')THEN
          IF(NUMSET.LE.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            IF(IDEXEF.EQ.'EFFE')THEN
              WRITE(ICOUT,2212)
 2212         FORMAT('      AN EFFECTS PLOT WAS CALLED FOR')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IDEXEF.EQ.'ABSO')THEN
              WRITE(ICOUT,2213)
 2213         FORMAT('      AN ABSOLUTE EFFECTS PLOT WAS CALLED FOR')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            WRITE(ICOUT,2214)
 2214       FORMAT('      BUT A FACTOR ONLY HAD ONE LEVEL')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2215)
 2215       FORMAT('      (THEREFORE, CANNOT COMPUTE AN "EFFECT".)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2216)IFAC
 2216       FORMAT('      IT WAS FACTOR NUMBER ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2217)XIDTEM(ISET)
 2217       FORMAT('      IT WAS LEVEL  NUMBER ',E15.7)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSEIF(NUMSET.EQ.2)THEN
            R1=STAT(1)
            IF(XIDTEM(2).LE.XIDTEM(1))R1=STAT(2)
            R2=STAT(2)
            IF(XIDTEM(2).LE.XIDTEM(1))R2=STAT(1)
          ELSE
            R1=STAT(1)
            R2=STAT(1)
            DO2231ISET=1,NUMSET
              IF(STAT(I).LT.R1)R1=STAT(I)
              IF(STAT(I).GT.R2)R2=STAT(I)
 2231       CONTINUE
          ENDIF
C
          EFFECT=R2-R1
          ABSEFF=ABS(EFFECT)
          J=J+1
          Y2(J)=EFFECT
          IF(IDEXEF.EQ.'ABSO')Y2(J)=ABSEFF
          X2(J)=AFAC
          D2(J)=1.0
C
        ENDIF
        N2=J
C
C               *********************************************
C               **  STEP 23--                              **
C               **  FOR THE YOUDEN PLOT,                   **
C               **  FORM PLOT COORDINATES                  **
C               *********************************************
C
        IF(IDEXYO.EQ.'YOUD')THEN
          IF(NUMSET.NE.2)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,31)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2322)
 2322       FORMAT('      A YOUDEN PLOT WAS CALLED FOR')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2324)
 2324       FORMAT('      BUT A FACTOR DID NOT HAVE EXACTLY TWO LEVELS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2325)
 2325       FORMAT('      (THEREFORE, CANNOT FORM THE 2 AXES')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2326)
 2326       FORMAT('      OF A YOUDEN PLOT.)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2327)AFAC
 2327       FORMAT('      THE FACTOR           = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2328)NUMSET
 2328       FORMAT('      THE NUMBER OF LEVELS = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(IFAC.EQ.1)YMIN=STAT(1)
          IF(IFAC.EQ.1)YMAX=STAT(1)
          IF(STAT(1).LT.YMIN)YMIN=STAT(1)
          IF(STAT(2).LT.YMIN)YMIN=STAT(2)
          IF(STAT(1).GT.YMAX)YMAX=STAT(1)
          IF(STAT(2).GT.YMAX)YMAX=STAT(2)
C
          J=J+1
          Y2(J)=STAT(1)
          X2(J)=STAT(2)
          D2(J)=AFAC
        ENDIF
C
 1100 CONTINUE
 1199 CONTINUE
C
      IF(ICASP9.EQ.'CLAS')THEN
C
C               ******************************************
C               **  STEP 58--                           **
C               **  FOR CLASSIFICATION PLOT, THE        **
C               **  REFERENCE LINE WILL BE THE MEAN     **
C               **  OF ALL THE PLOTTED POINTS.          **
C               ******************************************
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
          WRITE(ICOUT,2801)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        ICASP2=ICASPL
        IF(ICASPL.EQ.'SCAT')ICASP2='MEAN'
C
        CALL DPDEX3(Y2,TEMPXI,TEMPX2,XTEMP1,XTEMP2,XTEMP3,
     1              J,MAXNXT,ICASP2,
     1              RIGHT,
     1              IQUAME,IQUASE,PSTAMV,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
           WRITE(ICOUT,5812)
 5812      FORMAT('AFTER CALL DPDEX3: RIGHT = ',G15.7)
           CALL DPWRST('XXX','BUG ')
        ENDIF
C
        J=J+1
        Y2(J)=RIGHT
        X2(J)=1.0-(DEXWID/2.0)
        IF(ICASPL.EQ.'SCAT')THEN
          D2(J)=ITAG+1
        ELSE
          D2(J)=NUMFAC+1
        ENDIF
        J=J+1
        Y2(J)=RIGHT
        X2(J)=ANUMFA+(DEXWID/2.0)
        IF(ICASPL.EQ.'SCAT')THEN
          D2(J)=ITAG+1
        ELSE
          D2(J)=NUMFAC+1
        ENDIF
C
        N2=J
        NPLOTV=3
        GOTO9000
      ENDIF
C
C               *********************************************
C               **  STEP 24--                              **
C               **  FOR ALL CASES EXCEPT YOUDEN PLOT,      **
C               **  IF A PARETO PLOT HAS BEEN CALLED FOR,  **
C               **  SORT (DECENDING) ALL THE COORDINATES   **
C               *********************************************
C
C     2018/01: ADD FOLLOWING OPTIONS FOR ORDERED/PARETO PLOT CASES--
C
C              1) MOVE THE SORTING OUTSIDE THE LOOP SO THAT
C                 WE CAN KEEP TRACK OF THE ASSOCIATED FACTOR
C                 VARIABLES.
C
C              2) ALLOW USER TO SPECIFY WHETHER SORT IS IN
C                 DESCENDING (DEFAULT) OR ASCENDING ORDER.
C
C              3) FOR ORDERED PLOT, INCLUDE OPTION WHERE FOR EACH
C                 POINT, A COLUMN OF "+" AND "-" WILL BE INCLUDED
C                 TO IDENTIFY FACTOR SETTINGS.
C
      IF(IDEXPA.EQ.'PARE' .OR. IDEXOR.EQ.'ORDE')THEN
C
        IF(IDPADI.EQ.'DESC')THEN
          DO2420I=1,N2
            Y2(I)=(-Y2(I))
 2420     CONTINUE
        ENDIF
C
        IF(IDEXOR.EQ.'ORDE')THEN
C
          DO2422I=1,N2
            INDX(I)=I
 2422     CONTINUE
C
          CALL SORTC3(Y2,INDX,N2,TEMP,INDX)
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
            WRITE(ICOUT,2423)
 2423       FORMAT('AFTER SORTC3 CALL:')
            CALL DPWRST('XXX','BUG ')
            DO2245II=1,N2
              WRITE(ICOUT,2427)II,Y2(II),TEMP(II),INDX(II)
 2427         FORMAT('II,Y2(II),TEMP(II),INDX(II) = ',I8,2G15.7,I8)
              CALL DPWRST('XXX','BUG ')
 2245       CONTINUE
          ENDIF
C
        ELSE
          CALL SORT(Y2,N2,TEMP)
        ENDIF
C
        DO2430I=1,N2
          Y2(I)=TEMP(I)
          X2(I)=I
          D2(I)=1.0
 2430   CONTINUE
C
        IF(IDPADI.EQ.'DESC')THEN
          DO2440I=1,N2
            Y2(I)=(-Y2(I))
 2440     CONTINUE
        ENDIF
C
        N2SAVE=N2
      ENDIF
C
C               ******************************************
C               **  STEP 28--                           **
C               **  OPERATE ON THE FULL DATA SET.       **
C               **  FOR MOST CASES,                     **
C               **  COMPUTE THE SPECIFIED STATISTIC     **
C               **  FOR THE FULL DATA SET               **
C               **  FOR THE SCATTER PLOT AND            **
C               **  FOR THE SIGN PLOT,                  **
C               **  AUGMENT THE PLOT WITH A COMPUTED    **
C               **  MEAN LINE.                           **
C               **  FOR THE ... YOUDEN PLOT,             **
C               **  AUGMENT THE PLOT WITH A DIAGONAL LINE**
C               ******************************************
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
         WRITE(ICOUT,2801)
 2801    FORMAT('AT 1100:')
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IDEXYO.EQ.'YOUD')THEN
        J=J+1
        Y2(J)=YMIN
        X2(J)=YMIN
        D2(J)=NUMFAC+1
        J=J+1
        Y2(J)=YMAX
        X2(J)=YMAX
        D2(J)=NUMFAC+1
      ELSEIF(IDEXEF.NE.'EFFE'.AND.IDEXEF.NE.'ABSO'.AND.
     1       IDEXOR.NE.'ORDE')THEN
C
        K=0
        DO2810I=1,N
          K=K+1
          TEMP(K)=Y1(I)
          TEMPXI(K)=Y2RESP(I)
          TEMPX2(K)=Y3(I)
 2810   CONTINUE
        NI=K
        NS2=NI
C
        ICASP2=ICASPL
        IF(ICASPL.EQ.'SCAT')ICASP2='MEAN'
        IF(ICASPL.EQ.'SIGN')ICASP2='MEAN'
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
          WRITE(ICOUT,2811)
 2811      FORMAT('BEFORE CALL DPDEX3: NS2 = ',I8)
           CALL DPWRST('XXX','BUG ')
           IBUGG3='ON'
        ENDIF
C
        CALL DPDEX3(TEMP,TEMPXI,TEMPX2,XTEMP1,XTEMP2,XTEMP3,
     1              NS2,MAXNXT,ICASP2,
     1              RIGHT,
     1              IQUAME,IQUASE,PSTAMV,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
           WRITE(ICOUT,2812)
 2812      FORMAT('AFTER CALL DPDEX3: RIGHT = ',G15.7)
           CALL DPWRST('XXX','BUG ')
        ENDIF
C
        J=J+1
        Y2(J)=RIGHT
        X2(J)=1.0-(DEXWID/2.0)
        IF(IDEXPA.EQ.'PARE')X2(J)=1
        IF(ICASPL.EQ.'SCAT')THEN
          D2(J)=ITAG+1
        ELSEIF(IDEXPA.EQ.'PARE')THEN
          D2(J)=2.0
        ELSE
          D2(J)=NUMFAC+1
        ENDIF
        J=J+1
        Y2(J)=RIGHT
        X2(J)=ANUMFA+(DEXWID/2.0)
        IF(IDEXPA.EQ.'PARE')X2(J)=J-2
        IF(ICASPL.EQ.'SCAT')THEN
          D2(J)=ITAG+1
        ELSEIF(IDEXPA.EQ.'PARE')THEN
          D2(J)=2.0
        ELSE
          D2(J)=NUMFAC+1
        ENDIF
C
      ENDIF
C
C     NOW FOR ORDERED PLOT, CREATE THE COLUMNS OF "+" AND "-" VALUES.
C     FOR NOW, JUST WRITE THE FOLLOWING COLUMNS TO dpst1f.dat:
C
C        COLUMN 1: X-COORDINATE
C        COLUMN 2: FACTOR ID
C        COLUMN 3: CODE VALUE FOR "CHARACTER" COMMAND
C                  (E.G., "-1" => 1 AND "+1" => 2)
C
      IF(IDEXOR.EQ.'ORDE')THEN
C
C       INDEX IDENTIFIES THE ROW OF THE DATA MATRIX
C
        CNT2=1.0
        DO2450I=1,N2SAVE
          IVAL=INDX(I)
          XVAL=REAL(I)
C
          WRITE(IOUNI2,'(F7.0)')REAL(INDX(I))
C
          IF(IDPAUN.EQ.'ON')THEN
            IVAL3=INDX(I)
            CNT2=CNT2+1.0
            J=J+1
            X2(J)=REAL(I)
            Y2(J)=YUNCLW(IVAL3)
            D2(J)=CNT2
            J=J+1
            X2(J)=REAL(I)
            Y2(J)=YUNCUP(IVAL3)
            D2(J)=CNT2
          ENDIF
C
          DO2460JJ=1,NUMFAC
            WRITE(IOUNI1,'(4F7.0)')XVAL,REAL(JJ),X(IVAL,JJ)
C
            IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
              WRITE(ICOUT,2461)I,JJ,XVAL,X(IVAL,JJ)
 2461         FORMAT('2460 LOOP: I,JJ,XVAL,X(IVAL,JJ)=',
     1               3I8,3F8.0)
              CALL DPWRST('XXX','BUG ')
            ENDIF
 2460     CONTINUE
C
 2450   CONTINUE
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
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEX2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ICASPL,IERROR,N,NUMSET,N2
 9013   FORMAT('ICASPL,IERROR,N,NUMSET,N2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NUMCOM,NUMFAC,YMIN,YMAX
 9014   FORMAT('NUMCOM,NUMFAC,YMIN,YMAX = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)ANUMSE,A0,A1
 9017   FORMAT('ANUMSE,A0,A1 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        IF(IDEXEF.NE.'STAT')THEN
          DO9030I=1,NUMSET
            WRITE(ICOUT,9031)I,XIDTEM(I),STAT(I)
 9031       FORMAT('I,XIDTEM(I),STAT(I) = ',I8,2E15.7)
            CALL DPWRST('XXX','BUG ')
 9030     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDEX3(TEMP,TEMPXI,TEMPX2,
     1                  XTEMP1,XTEMP2,XTEMP3,
     1                  NS2,MAXNXT,ICASPL,
     1                  RIGHT,
     1                  IQUAME,IQUASE,PSTAMV,
     1                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--CALCULATE A STATISTIC IN CONNECTION WITH
C              THE ... STATISTIC PLOT COMMANDS,
C              THE DEX ... PLOT COMMANDS, ETC.
C              THE STATISTICS INCLUDE--
C                 MEAN
C                 MIDM
C                 MEDI
C                 SD
C                 MAD
C                 AAD
C                 VARI
C                 RSD
C                 RANG
C                 MINI
C                 MAXI
C                 SKEW
C                 KURT
C                 AUCR
C                 SDM
C                 AUCV
C                 RACV
C                 LOWH
C                 UPPH
C                 LOWQ
C                 UPPQ
C                 TRIM
C                 WINM
C                 MIDQ
C                 1DEC
C                 2DEC
C                 3DEC
C                 4DEC
C                 5DEC
C                 6DEC
C                 7DEC
C                 8DEC
C                 9DEC
C                 PERCENTILE
C                 SIN FREQUENCY
C                 SIN AMPLITUDE
C                 LINEAR INTERCEPT
C                 LINEAR SLOPE
C                 LINEAR RESSD
C                 LINEAR CORRELATION
C                 TAGUCHI SIGNAL-TO-NOISE
C                 PROPORTION
C                 CP
C                 CPK
C                 CNPK
C                 CPM
C                 CC
C                 EXPECTED LOSS
C                 PERCENT DEFECTIVE
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
C     ORIGINAL VERSION--MAY       1988.
C     UPDATED         --DECEMBER  1993. LINFIT ARGS
C     UPDATED         --MARCH     1995. ADD MAD AND AAD STATISTICS
C     UPDATED         --NOVEMBER  1998. ADD PERCENTILE STATISTICS
C     UPDATED         --NOVEMBER  1998. ADD CPM, CC STATISTICS
C     UPDATED         --MARCH     1999. ADD CNPK STATISTICS
C     UPDATED         --APRIL     2001. ARGUMENT LIST FOR CP, CPK, CPM
C     UPDATED         --JULY      2002. WINSORIZED VARIANCE
C     UPDATED         --JULY      2002. WINSORIZED SD
C     UPDATED         --JULY      2002. ADD WINSORIZED COVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD WINSORIZED CORRELATION PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD BIWEIGHT MIDCOVARIANCE PLOT
C     UPDATED         --JULY      2002. ADD PERCENTAGE BEND MIDVARIANCE
C                                           PLOT
C     UPDATED         --JULY      2002. ADD HODGES LEHMAN PLOT
C     UPDATED         --JULY      2002. ADD QUANTILE PLOT
C     UPDATED         --JULY      2002. ADD QUANTILE STANDARD ERROR PLOT
C     UPDATED         --JULY      2002. ADD TRIMMED MEAN STANDARD
C                                       ERROR PLOT
C     UPDATED         --APRIL     2003. ADD SN AND QN (REQUIRED
C                                       ADDITIONAL SUPPORT ARRAYS)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IQUAME
      CHARACTER*4 IQUASE
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
CCCCC CHARACTER*4 IHP
CCCCC CHARACTER*4 IHP2
CCCCC CHARACTER*4 IHWUSE
CCCCC CHARACTER*4 MESSAG
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
CCCCC CHARACTER*4 IFLAG
C
C---------------------------------------------------------------------
C
      DIMENSION TEMP(*)
      DIMENSION TEMPXI(*)
      DIMENSION TEMPX2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
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
      ISUBN1='DPDE'
      ISUBN2='X3  '
C
      IWRITE='OFF'
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDEX3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,IERROR
   52   FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,NS2,MAXNXT
   53   FORMAT('ICASPL,NS2,MAXNXT = ',A4,2I8)
        CALL DPWRST('XXX','BUG ')
        DO60I=1,NS2
          WRITE(ICOUT,61)I,TEMP(I),TEMPXI(I),XTEMP1(I),XTEMP2(I)
   61     FORMAT('I,TEMP(I),TEMPXI(I),XTEMP1(I),XTEMP2(I) = ',
     1           I8,4E15.7)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
      ENDIF
C
C               **************************************************
C               **  STEP 13--                                   **
C               **  BRANCH, DEPENDING ON THE DESIRED STATISTIC  **
C               **************************************************
C
      CALL CMPSTA(TEMP,TEMPXI,TEMPX2,XTEMP1,XTEMP2,XTEMP3,
     1            MAXNXT,NS2,NS2,NS2,NUMV2,ICASPL,
     1            ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1            DTEMP1,DTEMP2,DTEMP3,
CCCCC1            IQUAME,IQUASE,PSTAMV,
     1            RIGHT,
     1            ISUBRO,IBUGG3,IERROR)
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DEX3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDEX3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG3,ISUBRO,IERROR
 9012   FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ICASPL,NS2,MAXNXT,RIGHT
 9013   FORMAT('ICASPL,NS2,MAXNXT,RIGHT = ',A4,2I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,NS2
          WRITE(ICOUT,9021)I,TEMP(I),TEMPXI(I),XTEMP1(I),XTEMP2(I)
 9021     FORMAT('I,TEMP(I),TEMPXI(I),XTEMP1(I),XTEMP2(I) = ',
     1           I8,4E15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDFPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ISEED,
     1                 IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A DISTRIBUTIONAL FIT PLOT.  THIS PLOT ESSENTIALLY
C              PLOTS THE RESULTS OF THE "BEST DISTRIBUTIONAL FIT" COMMAND
C              FOR ONE OR MORE GROUPS IN THE FORM OF A TABULATION PLOT.
C
C              SPECIFICALLY,
C
C                1. FIT THE DATA IN EACH GROUP.  THE USER CAN SPECIFY
C                   MAXIMUM LIKELIHOOD OR PPCC/KS/ANDERSON-DARLING
C                   AS THE FIT METHOD.
C
C                2. COMPUTE A GOODNESS OF FIT STATISTIC.  THE USER CAN
C                   SPECIFY ANDERSON-DARLING, K-S, PPCC, OR AIC/BIC AS
C                   THE GOODNESS OF FIT CRITIERION.
C
C                3. DEPENDING ON THE FIT METHOD AND GOODNESS OF FIT
C                   CRITIERION, THERE WILL BE 25 TO 40 DISTRIBUTIONS THAT
C                   ARE FIT.
C
C                   THE GOODNESS OF FIT RESULTS WILL BE DISPLAYED AS A
C                   TABULATION PLOT.  THE X-AXIS WILL BE THE GROUPS IN
C                   THE DATA.  THE DISTRIBUTIONS ARE ON THE Y-AXIS.
C
C                THIS PLOT IS INTENDED AS A SCREENING TOOL FOR FINDING
C                GOOD CANDIDATE DISTRIBUTIONS.
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
C     VERSION NUMBER--2014/8
C     ORIGINAL VERSION--AUGUST    2014.
C     UPDATED         --JUNE      2018. OPTION TO SORT BY AVERAGE RANK
C     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE, NO
C                                       LONGER NEED TEMP24, TEMP25,
C                                       TEMP26
C     UPDATED         --JULY      2019. CALL LIST TO DPDFP2
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
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ILEVE2
      CHARACTER*4 IRELAT
      CHARACTER*4 IMETHD
      CHARACTER*4 ICENSO
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
C
      CHARACTER*4 ICASE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCODA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION D2(MAXOBV)
      DIMENSION YLEVEL(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION TAG1(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION TEMP7(MAXOBV)
      DIMENSION TEMP8(MAXOBV)
      DIMENSION TEMP9(MAXOBV)
      DIMENSION TEMP10(MAXOBV)
      DIMENSION TEMP11(MAXOBV)
      DIMENSION TEMP12(MAXOBV)
      DIMENSION TEMP13(MAXOBV)
      DIMENSION TEMP14(MAXOBV)
      DIMENSION TEMP15(MAXOBV)
      DIMENSION TEMP16(MAXOBV)
      DIMENSION TEMP17(MAXOBV)
      DIMENSION TEMP18(MAXOBV)
      DIMENSION TEMP19(MAXOBV)
      DIMENSION TEMP20(MAXOBV)
      DIMENSION TEMP21(MAXOBV)
      DIMENSION TEMP22(MAXOBV)
      DIMENSION TEMP23(MAXOBV)
      DIMENSION AVERAN(MAXOBV)
C
      DIMENSION ITEMP1(MAXOBV)
      DIMENSION ITEMP2(MAXOBV)
      DIMENSION IINDX(MAXOBV)
C
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),YLEVEL(1))
      EQUIVALENCE (GARBAG(IGARB3),TAG1(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMP5(1))
      EQUIVALENCE (GARBAG(IGAR10),TEMP6(1))
      EQUIVALENCE (GARBAG(JGAR11),TEMP7(1))
      EQUIVALENCE (GARBAG(JGAR12),TEMP8(1))
      EQUIVALENCE (GARBAG(JGAR13),TEMP9(1))
      EQUIVALENCE (GARBAG(JGAR14),TEMP10(1))
      EQUIVALENCE (GARBAG(JGAR15),TEMP11(1))
      EQUIVALENCE (GARBAG(JGAR16),TEMP12(1))
      EQUIVALENCE (GARBAG(JGAR17),TEMP13(1))
      EQUIVALENCE (GARBAG(JGAR18),TEMP14(1))
      EQUIVALENCE (GARBAG(JGAR19),TEMP15(1))
      EQUIVALENCE (GARBAG(JGAR20),TEMP16(1))
      EQUIVALENCE (GARBAG(IGAR11),TEMP17(1))
      EQUIVALENCE (GARBAG(IGAR12),TEMP18(1))
      EQUIVALENCE (GARBAG(IGAR13),TEMP19(1))
      EQUIVALENCE (GARBAG(IGAR14),TEMP20(1))
      EQUIVALENCE (GARBAG(IGAR15),TEMP21(1))
      EQUIVALENCE (GARBAG(IGAR16),TEMP22(1))
      EQUIVALENCE (GARBAG(IGAR17),TEMP23(1))
      EQUIVALENCE (GARBAG(IGAR18),Y2(1))
      EQUIVALENCE (GARBAG(IGAR19),X2(1))
      EQUIVALENCE (GARBAG(IGAR20),D2(1))
      EQUIVALENCE (GARBAG(IGAR21),AVERAN(1))
C
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),IINDX(1))
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
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
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPDF'
      ISUBN2='PL  '
      ICENSO='OFF'
      IMETHD='UNIM'
      IF(IPPCCN.EQ.'KAPL')IMETHD='KAPL'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DFPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDFPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICONT,IBUGG2,IBUGG3,IBUGQ
   53   FORMAT('ICONT,IBUGG2,IBUGG3,IBUGQ = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  EXTRACT THE COMMAND                              **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:          **
C               **    1) DISTRIBUTIONAL FIT PLOT Y X1                **
C               **    2) MULTIPLE DISTRIBUTIONAL FIT PLOT Y1 ... YK  **
C               **    3) REPLICATED DISTRIBUTIONAL FIT PLOT Y X1 ... XK
C               **  THE "REPLICATION" CASE IS ACTUALLY THE DEFAULT AND*
C               **  THE KEYWORD "REPLICATION" IS OPTIONAL. HOWEVER,  **
C               **  SUPPORT IT FOR COMPATABILITY WITH OTHER COMMANDS.**
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DFPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'DIST')GOTO89
      IF(ICOM.EQ.'MULT')GOTO89
      IF(ICOM.EQ.'REPL')GOTO89
      GOTO9000
C
   89 CONTINUE
      ICASPL='DFIT'
      IMULT='OFF'
      IREPL='OFF'
      ILASTC=-9999
C
      IF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
      ELSEIF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
      ENDIF
C
      ISTOP=NUMARG-1
      DO90I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT')THEN
          ISTOP=I
          IFOUN2='YES'
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
        ELSEIF(ICOM.EQ.'DIST' .AND. IHARG(I).EQ.'FIT')THEN
          IFOUN1='YES'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'DIST' .AND. IHARG(I+1).EQ.'FIT')THEN
          IFOUN1='YES'
          ILASTC=MAX(ILASTC,I+1)
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
  101     FORMAT('***** ERROR IN DISTRIBUTIONAL FIT PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,107)
  107     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THIS PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DFPL')THEN
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DFPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='DISTRIBUTIONAL FIT PLOT'
      MINNA=2
      MAXNA=100
      MINNVA=2
      MINN2=5
      IFLAGE=99
      IF(IMULT.EQ.'ON')THEN
        IFLAGE=0
      ELSE
        IREPL='ON'
      ENDIF
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
C
      IF(IREPL.EQ.'ON')THEN
        MAXNVA=7
      ELSEIF(IMULT.EQ.'ON')THEN
        MAXNVA=30
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DFPL')THEN
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
      NLVARI=1
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR - 1
      ELSE
        NRESP=1
        NREPL=NUMVAR - NRESP - NLVARI
        IF(NREPL.LT.0 .OR. NREPL.GT.6)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE 0 OR 1;  SUCH WAS NOT THE CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DFPL')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     CREATE THE LEVEL VARIABLE FROM THE LAST VARIABLE ON THE LIST
C
      J2=0
      IMAX=NRIGHT(NUMVAR)
      DO490I=1,IMAX
        J2=J2+1
C
        IJ=MAXN*(ICOLR(NUMVAR)-1)+I
        IF(ICOLR(NUMVAR).LE.MAXCOL)YLEVEL(J2)=V(IJ)
        IF(ICOLR(NUMVAR).EQ.MAXCP1)YLEVEL(J2)=PRED(I)
        IF(ICOLR(NUMVAR).EQ.MAXCP2)YLEVEL(J2)=RES(I)
        IF(ICOLR(NUMVAR).EQ.MAXCP3)YLEVEL(J2)=YPLOT(I)
        IF(ICOLR(NUMVAR).EQ.MAXCP4)YLEVEL(J2)=XPLOT(I)
        IF(ICOLR(NUMVAR).EQ.MAXCP5)YLEVEL(J2)=X2PLOT(I)
        IF(ICOLR(NUMVAR).EQ.MAXCP6)YLEVEL(J2)=TAGPLO(I)
  490 CONTINUE
      NLEVEL=J2
      NUMVAR=NUMVAR-1
C
C               **************************************************
C               **  STEP 7A--                                   **
C               **  CASE 1: NO "MULTIPLE" CASE--CAN HAVE EITHER **
C               **          1, 2, OR 3 VARIABLES.  THE FIRST    **
C               **          VARIABLE IS A RESPONSE VARIABLE     **
C               **          AND THE SECOND AND THIRD VARIABLES  **
C               **          ARE REPLICATION VARIABLES (IF       **
C               **          PRESENT).  NOTE THAT THIS VERSION   **
C               **          DOES NOT ACCEPT MATRIX ARGUMENTS    **
C               **          EVEN IF ONLY A SINGLE ARGUMENT IS   **
C               **          GIVEN (YOU CAN USE THE MULTIPLE     **
C               **          OPTION IN THAT CASE).               **
C               **************************************************
C
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DFPL')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,NLOCAL,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C       IF THERE ARE TWO OR MORE REPLICATION VARIABLES, COMBINE
C       THEM TO CREATE A SINGLE REPLICATION VARIABLE.
C
        IF(NUMVAR.EQ.2)THEN
          DO7010I=1,NLOCAL
            TAG1(I)=TEMP1(I)
 7010     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.3)THEN
          CALL CODCT2(TEMP1,TEMP2,NLOCAL,ICCTOF,ICCTG1,IWRITE,
     1                TEMP3,TEMP4,TEMP5,
     1                IBUGG3,ISUBRO,IERROR)
          DO7011I=1,NLOCAL
            TAG1(I)=TEMP3(I)
 7011     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.4)THEN
          CALL CODCT3(TEMP1,TEMP2,TEMP3,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,IWRITE,
     1                TEMP4,TEMP5,TEMP6,TEMP7,
     1                IBUGG3,ISUBRO,IERROR)
          DO7012I=1,NLOCAL
            TAG1(I)=TEMP4(I)
 7012     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.5)THEN
          CALL CODCT4(TEMP1,TEMP2,TEMP3,TEMP4,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,IWRITE,
     1                TEMP5,TEMP6,TEMP7,TEMP8,TEMP9,
     1                IBUGG3,ISUBRO,IERROR)
          DO7013I=1,NLOCAL
            TAG1(I)=TEMP5(I)
 7013     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.6)THEN
          CALL CODCT5(TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,IWRITE,
     1                TEMP6,TEMP7,TEMP8,TEMP9,TEMP10,TEMP11,
     1                IBUGG3,ISUBRO,IERROR)
          DO7014I=1,NLOCAL
            TAG1(I)=TEMP6(I)
 7014     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.7)THEN
          CALL CODCT6(TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,ICCTG5,IWRITE,
     1                TEMP7,TEMP8,TEMP9,TEMP10,TEMP11,TEMP12,TEMP13,
     1                IBUGG3,ISUBRO,IERROR)
          DO7015I=1,NLOCAL
            TAG1(I)=TEMP7(I)
 7015     CONTINUE
          NUMVAR=2
        ENDIF
C
C               ********************************************************
C               **  STEP 7B--                                         **
C               **  COMPUTE THE APPROPRIATE DISTRIBUTIONAL PLOT.      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS             **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.                **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).     **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).     **
C               ********************************************************
C
        CALL DPDFP2(Y1,TAG1,NLOCAL,YLEVEL,NLEVEL,ISEED,IDFITY,IDFISO,
     1              XIDTEM,
     1              TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,TEMP8,
     1              TEMP9,TEMP10,TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,
     1              TEMP16,TEMP17,TEMP18,TEMP19,TEMP20,TEMP21,
     1              TEMP22,TEMP23,AVERAN,
     1              ITEMP1,ITEMP2,IINDX,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              PTPLXI,PTPLYI,ITPLCD,
     1              YLOWLM,YUPPLM,A,B,MINMAX,
     1              IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1              ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1              IGOMDF,IKATDF,IGIGDF,IGEODF,IGAUDF,
     1              IEXPBC,IWEIBC,ICENTY,
     1              CLLIMI,CLWIDT,IHSTCW,IHSTOU,IRELAT,IRHSTG,
     1              IFLAGL,AL,
     1              IBFIME,IBFICR,IBFIFO,PBFILL,PBFIUL,
     1              IBFITY,PBFIXV,
     1              IPPLDP,IMETHD,IPPCBW,IPPCCC,IPPCFO,
     1              IPPCDP,IPPCAP,IPPCAO,PCHSLM,ILEVE2,
     1              Y2,X2,D2,Y,X,D,DCOLOR,
     1              NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               ***********************************************
C               **  STEP 8A--                                **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES.     **
C               **          THESE CAN BE EITHER VARIABLE OR  **
C               **          MATRIX ARGUMENTS.                **
C               ***********************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'DFPL')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        NUMVA2=NUMVAR
        CALL DPPAR8(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              TEMP1,Y1,TAG1,NLOCAL,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NUMVAR=2
C
C               *****************************************************
C               **  STEP 8B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               *****************************************************
C
        CALL DPDFP2(Y1,TAG1,NLOCAL,YLEVEL,NLEVEL,ISEED,IDFITY,IDFISO,
     1              XIDTEM,
     1              TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,TEMP8,
     1              TEMP9,TEMP10,TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,
     1              TEMP16,TEMP17,TEMP18,TEMP19,TEMP20,TEMP21,
     1              TEMP22,TEMP23,AVERAN,
     1              ITEMP1,ITEMP2,IINDX,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              PTPLXI,PTPLYI,ITPLCD,
     1              YLOWLM,YUPPLM,A,B,MINMAX,
     1              IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1              ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1              IGOMDF,IKATDF,IGIGDF,IGEODF,IGAUDF,
     1              IEXPBC,IWEIBC,ICENTY,
     1              CLLIMI,CLWIDT,IHSTCW,IHSTOU,IRELAT,IRHSTG,
     1              IFLAGL,AL,
     1              IBFIME,IBFICR,IBFIFO,PBFILL,PBFIUL,
     1              IBFITY,PBFIXV,
     1              IPPLDP,IMETHD,IPPCBW,IPPCCC,IPPCFO,
     1              IPPCDP,IPPCAP,IPPCAO,PCHSLM,ILEVE2,
     1              Y2,X2,D2,Y,X,D,DCOLOR,
     1              NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'DFPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDFPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IFENCE,ISIZE
 9014   FORMAT('IFENCE,ISIZE = ',A4,I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
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
      SUBROUTINE DPDFP2(Y1,TAG1,N,YLEVEL,NLEVEL,ISEED,IDFITY,IDFISO,
     1                  XIDTEM,
     1                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,TEMP8,
     1                  TEMP9,TEMP10,TEMP11,TEMP12,TEMP13,TEMP14,TEMP15,
     1                  TEMP16,TEMP17,TEMP18,TEMP19,TEMP20,TEMP21,
     1                  TEMP22,TEMP23,AVERAN,
     1                  ITEMP1,ITEMP2,IINDX,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  PTPLXI,PTPLYI,ITPLCD,
     1                  YLOWLM,YUPPLM,A,B,MINMAX,
     1                  IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1                  ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1                  IGOMDF,IKATDF,IGIGDF,IGEODF,IGAUDF,
     1                  IEXPBC,IWEIBC,ICENTY,
     1                  CLLIMI,CLWIDT,IHSTCW,IHSTOU,IRELAT,IRHSTG,
     1                  IFLAGL,AL,
     1                  IBFIME,IBFICR,IBFIFO,PBFILL,PBFIUL,
     1                  IBFITY,PBFIXV,
     1                  IPPLDP,IMETHD,IPPCBW,IPPCCC,IPPCFO,
     1                  IPPCDP,IPPCAP,IPPCAO,PCHSLM,ILEVE2,
     1                  Y2,X2,D2,Y,X,D,DCOLOR,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS THAT WILL DEFINE A
C              DISTRIBUTIONAL FIT TABULATION PLOT.
C     DESCRIPTION--IN THE TABULATION PLOT, WE CROSS-TABULATE OVER 1 TO 6
C                  GROUP-ID VARIABLES.  WE DEFINE A GRID BASED ON THESE
C                  GROUP-ID VARIABLES.  THEN FOR THE RESPONSE VALUES
C                  CORRESPONDING TO A GIVEN SET OF THESE GROUP-ID
C                  VARIABLES, WE COMPUTE A USER-SPECIFED STATISTIC.  THE
C                  VALUE OF THE STATISTIC IS THEN COMPARED TO SOME
C                  USER-SPECIFIED LEVELS (THESE ARE DEFINED IN THE
C                  YLEVEL VARIABLE).  A RECTANGLE IS DRAWN AND THE
C                  ATTRIBUTES (PRIMARILY FILL COLOR) ARE BASED ON
C                  THE VALUE OF THE STATISTIC RELATIVE TO YLEVEL.
C
C                  THE DISTRIBUTIONAL FIT PLOT IS A VARIATION OF THE
C                  TABULATION PLOT.  GIVEN A RESPONSE VARIABLE AND A
C                  GROUP-ID VARIABLE, WE DO THE FOLLOWING:
C
C                      1) FOR EACH GROUP, WE FIT THE DATA TO A NUMBER
C                         OF COMMON DISTRIBUTIONS.  THE USER CAN
C                         SPECIFY EITHER MAXIMUM LIKELIHOOD OR PPCC
C                         FITTING.
C
C                      2) A GOODNESS OF FIT STATISTIC IS COMPUTED FOR
C                         EACH FIT.  THE USER CAN SPECIFY PPCC,
C                         ANDERSON-DARLING, KOLMOGOROV-SMIRNOV, BIC/AIC
C                         FOR THE GOODNESS OF FIT CRITERION.
C
C                      3) A TWO-WAY TABULATION PLOT IS GENERATED FOR
C                         THE GOODNESS OF FIT VALUES.  THE Y-AXIS WILL
C                         CONTAIN THE DISTRIBUTIONS AND THE X-AXIS WILL
C                         CONTAIN THE GROUPS.
C
C                         IF THERE ARE MULTIPLE GROUPS, THESE WILL BE
C                         COMBINED INTO A SINGLE GROUP-ID VARIABLE BY
C                         THE CALLING ROUTINE.
C
C                  THIS PLOT IS USEFUL FOR VISUALLY SCREENING FOR
C                  DISTRIBUTIONS THAT FIT WELL ACROSS GROUPS OF THE
C                  DATA.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2889
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2014/8
C     ORIGINAL VERSION--AUGUST    2014.
C     UPDATED         --JUNE      2018. OPTION TO SORT BY AVERAGE RANK
C     UPDATED         --JUNE      2018. OPTIONALLY SELECT "TABLE" OR
C                                       "SCATTER" OPTION
C     UPDATED         --JULY      2019. CALL LIST TO DPBEF2, NO LONGER
C                                       NEED TEMP24, TEMP25, TEMP26
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ITPLCD
      CHARACTER*4 ITPLRM
      CHARACTER*4 IDFITY
      CHARACTER*4 IDFISO
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICENSO
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
      CHARACTER*4 IGAUDF
      CHARACTER*4 IEXPBC
      CHARACTER*4 IWEIBC
      CHARACTER*4 ICENTY
      CHARACTER*4 IDFTTY
      CHARACTER*4 IDFTT2
      CHARACTER*4 IPPCCC
      CHARACTER*4 IPPCFO
      CHARACTER*4 IPPCAO
      CHARACTER*4 IPPCBW
      CHARACTER*4 IMETHD
      CHARACTER*4 ILEVE2
      CHARACTER*4 IBFIME
      CHARACTER*4 IBFICR
      CHARACTER*4 IBFIFO
      CHARACTER*4 IBFITY
      CHARACTER*4 IHSTCW
      CHARACTER*4 IHSTOU
      CHARACTER*4 IRELAT
      CHARACTER*4 IRHSTG
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRIT2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISUBN3
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASPL
      CHARACTER*4 IOP
      CHARACTER*4 ISORDI
C
C---------------------------------------------------------------------
C
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
      DIMENSION IPPCAP(*)
C
      DIMENSION Y1(*)
      DIMENSION TAG1(*)
      DIMENSION YLEVEL(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION TEMP6(*)
      DIMENSION TEMP7(*)
      DIMENSION TEMP8(*)
      DIMENSION TEMP9(*)
      DIMENSION TEMP10(*)
      DIMENSION TEMP11(*)
      DIMENSION TEMP12(*)
      DIMENSION TEMP13(*)
      DIMENSION TEMP14(*)
      DIMENSION TEMP15(*)
      DIMENSION TEMP16(*)
      DIMENSION TEMP17(*)
      DIMENSION TEMP18(*)
      DIMENSION TEMP19(*)
      DIMENSION TEMP20(*)
      DIMENSION TEMP21(*)
      DIMENSION TEMP22(*)
      DIMENSION TEMP23(*)
      DIMENSION AVERAN(*)
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION DCOLOR(*)
C
      DIMENSION ITEMP1(*)
      DIMENSION ITEMP2(*)
      DIMENSION IINDX(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
      CHARACTER*60 INLON2(100)
      CHARACTER*60 INLON3(100)
      CHARACTER*4 IVARID(1)
      CHARACTER*4 IVARI2(1)
      DIMENSION PID(1)
      DIMENSION STATVA(100)
C
C     THIS PLOT WILL AUTOMATICALLY DEFINE SOME SETTINGS FOR THE
C     Y-AXIS LABEL.  NEED TO SAVE CURRENT SETTINGS.
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOPC.INC'
C
      CHARACTER*4 IY1MNZ
      CHARACTER*4 IY1MXZ
      CHARACTER*4 IY1JSZ
      CHARACTER*4 IY1NSZ
      CHARACTER*4 IY1ZFZ
      CHARACTER*2048 IY1ZCZ
      REAL GY1MNZ
      REAL GY1MXZ
      INTEGER NMJY1Z
      INTEGER NMNY1Z
      COMMON/DFP2/GY1MNZ,GY1MXZ,NMJY1Z,NMNY1Z,
     1            IY1MNZ,IY1MXZ,IY1JSZ,IY1MNSZ,IY1ZFZ,IY1ZCZ
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
      ISUBN1='DPDF'
      ISUBN2='P2  '
      ISUBN3='DFP2'
      IWRITE='OFF'
      IERROR='NO'
      ICAPSW='NULL'
      ICAPTY='NULL'
      IFORSW='EXPO'
      DO10I=1,MAXOBV
        AVERAN(I)=0.0
   10 CONTINUE
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN DISTRIBUTIONAL FIT PLOT--')
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
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DFP2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPDFP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)N,NLEVEL
   71   FORMAT('N,NLEVEL = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO72I=1,N
          WRITE(ICOUT,73)I,Y1(I),TAG1(I)
   73     FORMAT('I,Y1(I),TAG1(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
        DO82I=1,NLEVEL
          WRITE(ICOUT,83)I,YLEVEL(I)
   83     FORMAT('I,YLEVEL(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   82   CONTINUE
      ENDIF
C
C     EXTRACT THE DISTINCT ELEMENTS OF THE LEVEL VARIABLE AND
C     SORT FROM LOW TO HIGH
C
      CALL DISTIN(YLEVEL,NLEVEL,IWRITE,TEMP1,NTEMP,IBUGG3,IERROR)
      DO110I=1,NTEMP
        YLEVEL(I)=TEMP1(I)
  110 CONTINUE
      NLEVEL=NTEMP
      CALL SORT(YLEVEL,NLEVEL,YLEVEL)
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLE (TAG1).                  **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
C               **  WHICH IS AN ERROR CONDITION FOR THIS PLOT.      **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DFP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CODE THE GROUP-ID VARIABLE IF REQUESTED.
C
      IF(ITPLCD.EQ.'ON')THEN
        CALL CODE(TAG1,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
        DO910I=1,N
          TAG1(I)=TEMP1(I)
  910   CONTINUE
      ENDIF
      CALL DISTIN(TAG1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
C
      IF(NUMSE1.LT.1 .OR. NUMSE1.GT.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=1
        WRITE(ICOUT,111)ITEMP,NUMSE1
  111   FORMAT('      THE NUMBER OF SETS FOR THE GROUP ',I1,
     1         ' VARIABLE, ',I8,',')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      IS EITHER LESS THAN ONE OR GREATER THAN THE ',
     1         'NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
  115   FORMAT('      OF OBSERVATIONS, ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      AN=REAL(N)
      ANUMS1=REAL(NUMSE1)
C
C               ***********************************************
C               **  STEP 2--                                 **
C               **  COMPUTE THE GOODNESS OF FIT FOR EACH     **
C               **  GROUP.                                   **
C               ***********************************************
C
      J=0
      DO2110ISET1=1,NUMSE1
        K=0
        DO2130I=1,N
          IF(XIDTEM(ISET1).NE.TAG1(I))GOTO2130
          K=K+1
          TEMP1(K)=Y1(I)
 2130   CONTINUE
        NTEMP=K
C
        ISTEPN='2'
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DFP2')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,2131)ISET1,NTEMP
 2131     FORMAT('AT 2130: ISET1,NTEMP = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NTEMP.GE.5)THEN
          NREPL=1
          ICASPL='DFIT'
          CALL DPBEF2(TEMP1,TEMP1,TEMP1,NTEMP,ICASPL,MAXOBV,ISEED,
     1                PID,IVARID,IVARI2,NREPL,
     1                TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,NOUT,
     1                TEMP7,TEMP8,TEMP9,DTEMP1,DTEMP2,DTEMP3,
     1                ITEMP1,ITEMP2,
     1                TEMP10,TEMP11,TEMP12,TEMP13,TEMP14,
     1                TEMP15,TEMP16,TEMP17,TEMP18,TEMP19,
     1                TEMP20,TEMP21,TEMP22,TEMP23,
     1                YLOWLM,YUPPLM,A,B,MINMAX,
     1                IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1                ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1                IGOMDF,IKATDF,IGIGDF,IGEODF,IGAUDF,
     1                IEXPBC,IWEIBC,ICENTY,
     1                CLLIMI,CLWIDT,IHSTCW,IHSTOU,IRELAT,IRHSTG,
     1                IFLAGL,AL,
     1                ICAPSW,ICAPTY,IFORSW,
     1                IBFIME,IBFICR,IBFIFO,PBFILL,PBFIUL,
     1                IBFITY,PBFIXV,
     1                IPPLDP,IMETHD,IPPCBW,IPPCCC,IPPCFO,
     1                IPPCDP,IPPCAP,IPPCAO,PCHSLM,ILEVE2,
     1                STATVA,INLON2,ISUBN3,NLIST,
     1                IBUGG3,ISUBRO,IERROR)
        ELSE
          GOTO2110
        ENDIF
C
        DO2140JDIS=1,NLIST
          J=J+1
          Y2(J)=STATVA(JDIS)
          X2(J)=XIDTEM(ISET1)
          D2(J)=REAL(JDIS)
 2140   CONTINUE
C
C       IF AVERAGE RANK REQUESTED, THEN RANK CURRENT DATA SET.  CONVERT
C       CPUMIN VALUES (I.E., NOT FIT COMPUTED) TO CPUMAX FOR PURPOSES
C       RANKING.
C
        IF(IDFISO.EQ.'AVRA')THEN
          NRANK=NRANK+1
          DO2143JDIS=1,NLIST
            TEMP21(JDIS)=STATVA(JDIS)
            IF(TEMP21(JDIS).EQ.CPUMIN)TEMP21(JDIS)=CPUMAX
 2143     CONTINUE
          CALL RANK(TEMP21,NLIST,IWRITE,TEMP22,TEMP23,MAXOBV,
     1               IBUGG3,IERROR)
          DO2145KDIS=1,NLIST
            AVERAN(KDIS)=AVERAN(JDIS) + TEMP22(JDIS)
 2145     CONTINUE
        ENDIF
C
 2110 CONTINUE
C
      IF(IDFISO.EQ.'AVRA')THEN
        DO2148JDIS=1,NLIST
          AVERAN(JDIS)=AVERAN(JDIS)/REAL(NRANK)
 2148   CONTINUE
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DFP2')THEN
          DO2146JDIS=1,NLIST
            WRITE(ICOUT,2147)JDIS,AVERAN(JDIS),INLON2(JDIS)
 2147       FORMAT('JDIS,AVERAN(JDIS),INLON2(JDIS) = ',I5,F9.2,A60)
            CALL DPWRST('XXX','BUG ')
 2146     CONTINUE
        ENDIF
C
      ENDIF
C
      N2=J
C
C     FOR A GIVEN FIT/GOODNESS OF FIT COMBINATION, NOT ALL DISTRIBUTIONS
C     WILL BE COMPUTED.  SO LOOP THROUGH TO DETERMINE WHICH ONES TO
C     OMIT FROM PLOT.  NOTE THAT WE DO THIS AFTER ALL CASES HAVE BEEN
C     COMPUTED SINCE FOR SOME DISTRIBUTIONS YOU MAY HAVE SOME CASES
C     THAT WERE SUCCESSFULLY COMPUTED AND OTHER CASES THAT WERE NOT.
C
C     STEP 1: DETERMINE WHICH DISTRIBUTIONS TO OMIT.
C
      N2NEW=0
      DO2210K=1,NLIST
        AHOLD=REAL(K)
        ITEMP1(K)=0
        DO2220I=1,N2
          IF(D2(I).EQ.AHOLD)THEN
            IF(Y2(I).NE.CPUMIN)THEN
              ITEMP1(K)=1
              GOTO2210
            ENDIF
          ENDIF
 2220   CONTINUE
 2210 CONTINUE
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DFP2')THEN
        WRITE(ICOUT,2273)NLIST
 2273   FORMAT('AT 2210: NLIST = ',I6)
        CALL DPWRST('XXX','BUG ')
        DO2276I=1,NLIST
          WRITE(ICOUT,2277)I,ITEMP1(I)
 2277     FORMAT('I,ITEMP1(I) = ',2I6)
          CALL DPWRST('XXX','BUG ')
 2276   CONTINUE
      ENDIF
C
C     STEP 2: NOW PACK THE Y2, X2, D2 ARRAYS
C
C             RENUMBER THE Y-COORDINATE AS WELL
C
      IYCNT=0
      DO2305I=1,NLIST
        IF(ITEMP1(I).EQ.1)THEN
          IYCNT=IYCNT+1
          ITEMP2(I)=REAL(IYCNT)
        ELSE
          ITEMP2(I)=0.0
        ENDIF
 2305 CONTINUE
C
      DO2310I=1,N2
        IHOLD=INT(D2(I)+0.5)
        IHOLD2=ITEMP2(IHOLD)
        IF(ITEMP1(IHOLD).EQ.1)THEN
          N2NEW=N2NEW+1
          Y2(N2NEW)=Y2(I)
          X2(N2NEW)=X2(I)
          D2(N2NEW)=D2(IHOLD2)
        ENDIF
 2310 CONTINUE
      N2=N2NEW
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DFP2')THEN
        WRITE(ICOUT,2353)N2,NLIST
 2353   FORMAT('AT 2310: N2,NLIST = ',2I6)
        CALL DPWRST('XXX','BUG ')
        DO2356I=1,N2
          WRITE(ICOUT,2377)I,Y2(I),X2(I),D2(I)
          CALL DPWRST('XXX','BUG ')
 2356   CONTINUE
      ENDIF
C
      NLIST2=0
      DO2320I=1,NLIST
        IF(ITEMP1(I).EQ.1)THEN
          NLIST2=NLIST2+1
          INLON2(NLIST2)=INLON2(I)
          IF(IDFISO.EQ.'AVRA')AVERAN(NLIST2)=AVERAN(NLIST)
        ENDIF
 2320 CONTINUE
      NLIST=NLIST2
C
C     FLIP THE ORDER OF THE DISTRIBUTION NAMES IN "INLON2".
C
      DO2410JDIS=1,NLIST
        IPOS=NLIST - JDIS + 1
        INLON3(IPOS)=INLON2(JDIS)
        IF(IDFISO.EQ.'AVRA')TEMP23(IPOS)=AVERAN(JDIS)
 2410 CONTINUE
      DO2420JDIS=1,NLIST
        INLON2(JDIS)=INLON3(JDIS)
        IF(IDFISO.EQ.'AVRA')AVERAN(JDIS)=TEMP23(JDIS)
 2420 CONTINUE
C
C     NOW FLIP THE VALUE OF THE Y-COORDINATE
C
      DO2510J=1,N2
        JDIS=INT(D2(J)+0.5)
        IPOS=NLIST - JDIS + 1
        D2(J)=REAL(IPOS)
 2510 CONTINUE
C
      N2NEW=0
      DO2520J=1,N2
        IF(Y2(J).NE.CPUMIN)THEN
          N2NEW=N2NEW+1
          D2(N2NEW)=D2(J)
          X2(N2NEW)=X2(J)
          Y2(N2NEW)=Y2(J)
        ENDIF
 2520 CONTINUE
      N2=N2NEW
C
C     IF REQUESTED, SORT BY AVERAGE RANK
C
      IF(IDFISO.EQ.'AVRA')THEN
        CALL SORTI(AVERAN,NLIST,TEMP22,TEMP23)
C
C       FOR PPCC GOODNESS OF FIT, WANT DESCENDING SORT.  FOR OTHER
C       METHODS, WANT ASCENDING SORT.
C
C       TEMP22 CONTAINS THE SORTED VALUES OF AVERAN WHILE TEMP23
C       CONTAINS THE INDEX VARIABLE.
C
        IF(IBFICR.EQ.'PPCC')THEN
          DO2521II=1,NLIST
            TEMP20(II)=TEMP22(II)
            TEMP21(II)=TEMP23(II)
 2521     CONTINUE
          DO2523II=1,NLIST
            AVERAN(II)=TEMP20(II)
            TEMP23(II)=TEMP21(II)
 2523     CONTINUE
        ELSE
          DO2527II=1,NLIST
            AVERAN(II)=TEMP22(II)
 2527     CONTINUE
        ENDIF
C
C       FLIP THE ORDER BASED ON THE AVERAGE RANK
C
        DO2610JDIS=1,NLIST
          IPOS=TEMP23(JDIS) + 0.5
          INLON3(IPOS)=INLON2(JDIS)
 2610   CONTINUE
        DO2620JDIS=1,NLIST
          INLON2(JDIS)=INLON3(JDIS)
 2620   CONTINUE
C
C       NOW FLIP THE VALUE OF THE Y-COORDINATE
C
        DO2660J=1,N2
          JDIS=INT(D2(J)+0.5)
          IPOS=TEMP23(JDIS) + 0.5
          D2(J)=REAL(IPOS)
 2660   CONTINUE
C
        N2NEW=0
        DO2670J=1,N2
          IF(Y2(J).NE.CPUMIN)THEN
            N2NEW=N2NEW+1
            D2(N2NEW)=D2(J)
            X2(N2NEW)=X2(J)
            Y2(N2NEW)=Y2(J)
          ENDIF
 2670   CONTINUE
C
      ENDIF
C
C     WRITE DISTRIBUTION NAMES TO "dpst1f.dat"
C
      IOP='OPEN'
      IFLG11=1
      IFLG21=0
      IFLG31=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      WRITE(IOUNI1,'(I8)')NLIST2
      DO2330I=1,NLIST2
        WRITE(IOUNI1,'(A60)')INLON2(I)
 2330 CONTINUE
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG3,ISUBRO,IERROR)
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DFP2')THEN
        WRITE(ICOUT,2373)N2,NLIST
 2373   FORMAT('AT 2320: N2,NLIST = ',2I6)
        CALL DPWRST('XXX','BUG ')
        DO2376I=1,N2
          WRITE(ICOUT,2377)I,Y2(I),X2(I),D2(I)
 2377     FORMAT('I,Y2(I),X2(I),D2(I) = ',I6,3G15.7)
          CALL DPWRST('XXX','BUG ')
 2376   CONTINUE
      ENDIF
C
C               ***********************************************
C               **  STEP 3--                                 **
C               **  NOW DEFINE SOME SETTINGS FOR THE Y-AXIS  **
C               **  LABEL.                                   **
C               ***********************************************
C
      ISTEPN='3.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DFP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     FIRST SAVE ORIGINAL SETTINGS SO THEY CAN BE RESTORED
C     AFTER PLOT GENERATED.
C
      IY1MNZ=IY1MIN
      IY1MXZ=IY1MAX
      GY1MNZ=GY1MIN
      GY1MXZ=GY1MAX
      NMJY1Z=NMJY1T
      NMNY1Z=NMNY1T
      IY1JSZ=IY1JSW
      IY1NSZ=IY1NSW
      IY1ZFZ=IY1ZFM
      IY1ZCZ=IY1ZCN
C
C     NOW SET THEM FOR THIS PLOT.
C
      IY1MIN='FIXE'
      IY1MAX='FIXE'
      GY1MIN=1.0
      GY1MAX=REAL(NLIST)
      NMJY1T=NLIST
      NMNY1T=0
      IY1JSW='FIXE'
      IY1NSW='FIXE'
      IY1ZFM='ALPH'
C
      IY1ZCN=' '
      ICNT1=0
      DO3010I=1,NLIST
        DO3020J=60,1,-1
          IF(INLON2(I)(J:J).NE.' ')THEN
            NLAST=J
            GOTO3029
          ENDIF
 3020   CONTINUE
        NLAST=1
 3029   CONTINUE
        DO3030J=1,NLAST
          IF(INLON2(I)(J:J).NE.' ')THEN
            ICNT1=ICNT1+1
            IY1ZCN(ICNT1:ICNT1)=INLON2(I)(J:J)
          ELSE
            ICNT1=ICNT1+1
            IY1ZCN(ICNT1:ICNT1+3)='SP()'
            ICNT1=ICNT1+3
          ENDIF
 3030   CONTINUE
        ICNT1=ICNT1+1
        IY1ZCN(ICNT1:ICNT1)=' '
 3010 CONTINUE
C
C               ***********************************************
C               **  STEP 4--                                 **
C               **  NOW GENERATE THE PLOT COORDINATES.       **
C               **  DEFINE A RECTANGLE FOR EACH POINT.       **
C               ***********************************************
C
C     NOTE THAT THIS PLOT DOES NOT SUPPORT SOME OF THE OPTIONS
C     SUPPORTED BY THE REGULAR TABULATION PLOT.  SPECIFICALLY,
C     WE DO NOT SORT BY ROWS OR COLUMNS AND WE DO NOT HAVE AN
C     "UNCERTAINTY" OPTION.
C
      ISTEPN='4.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DFP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      ICNT=0
      ICNT2=0
      XINC=0.5 - PTPLXI
      YINC=0.5 - PTPLYI
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DFP2')THEN
        WRITE(ICOUT,4011)N2
 4011   FORMAT('DPDFP2: AFTER CALL DPTAP3--N2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4012)XINC,YINC
 4012   FORMAT('XINC,YINC = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO4010I=1,N2
        STAT=Y2(I)
        XVAL=X2(I)
        YVAL=D2(I)
C
        XCOOR1=XVAL - XINC
        XCOOR2=XVAL + XINC
        YCOOR1=YVAL - YINC
        YCOOR2=YVAL + YINC
        IF(STAT.LT.YLEVEL(1))THEN
          ILEVEL=1
        ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
          ILEVEL=NLEVEL+1
        ELSE
          DO4015J=2,NLEVEL
            IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
              ILEVEL=J
            ENDIF
 4015         CONTINUE
        ENDIF
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'DFP2')THEN
          WRITE(ICOUT,4016)I,STAT,YVAL,XVAL
 4016         FORMAT('I,STAT,YVAL,XVAL = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4017)XCOOR1,XCOOR2,YCOOR1,YCOOR2
 4017         FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2 = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4018)ILEVEL
 4018         FORMAT('ILEVEL = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        ICNT2=ICNT2+1
        ICNT=ICNT+1
        X(ICNT)=XCOOR1
        Y(ICNT)=YCOOR1
        D(ICNT)=REAL(ICNT2)
        DCOLOR(ICNT)=REAL(ILEVEL)
C
        ICNT=ICNT+1
        X(ICNT)=XCOOR2
        Y(ICNT)=YCOOR1
        D(ICNT)=REAL(ICNT2)
        DCOLOR(ICNT)=REAL(ILEVEL)
C
        ICNT=ICNT+1
        X(ICNT)=XCOOR2
        Y(ICNT)=YCOOR2
        D(ICNT)=REAL(ICNT2)
        DCOLOR(ICNT)=REAL(ILEVEL)
C
        ICNT=ICNT+1
        X(ICNT)=XCOOR1
        Y(ICNT)=YCOOR2
        D(ICNT)=REAL(ICNT2)
        DCOLOR(ICNT)=REAL(ILEVEL)
C
        ICNT=ICNT+1
        X(ICNT)=XCOOR1
        Y(ICNT)=YCOOR1
        D(ICNT)=REAL(ICNT2)
        DCOLOR(ICNT)=REAL(ILEVEL)
C
 4010 CONTINUE
C
      NPLOTP=ICNT
      NPLOTV=2
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DFP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDFP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NPLOTP,NPLOTV,IERROR
 9012   FORMAT('ICASCT,N,NPLOTP,NPLOTV,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,NPLOTP
          WRITE(ICOUT,9036)I,Y(I),X(I),D(I),DCOLOR(I)
 9036     FORMAT('I,Y(I),X(I),D(I),DCOLOR(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END

      SUBROUTINE DPSEPA(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,   &
                        MAXSEG,ISEGPA,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE PATTERN FOR A SEGMENT.
!              THE PATTERN FOR SEGMENT I WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR ISEGPA(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A HOLLERITH VECTOR)
!                     --IARG   (A HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFPA
!                     --MAXSEG
!     OUTPUT ARGUMENTS--ISEGPA (A HOLLERITH VECTOR
!                              WHOSE I-TH ELEMENT CONTAINS THE
!                              PATTERN FOR SEGMENT I.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 COMPUTER SERVICES DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!     UPDATED         --AUGUST    1995.  DASH2 BUG
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
      CHARACTER*4 IDEFPA
      CHARACTER*4 ISEGPA
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
!
      DIMENSION ISEGPA(*)
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
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PATT')GO TO 1140
      GO TO 1199
!
 1110 CONTINUE
      IF(NUMARG.LE.1)GO TO 1120
      IF(IHARG(2).EQ.'ON')GO TO 1120
      IF(IHARG(2).EQ.'OFF')GO TO 1120
      IF(IHARG(2).EQ.'AUTO')GO TO 1120
      IF(IHARG(2).EQ.'DEFA')GO TO 1120
      GO TO 1125
!
 1120 CONTINUE
      IHOLD=IDEFPA
      GO TO 1130
!
 1125 CONTINUE
      IHOLD=IHARG(2)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(2).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(2).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(2).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(2).EQ.'5')IHOLD='DA5'
      GO TO 1130
!
 1130 CONTINUE
      IFOUND='YES'
      DO 1135 I=1,MAXSEG
      ISEGPA(I)=IHOLD
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1136)ISEGPA(I)
 1136 FORMAT('ALL SEGMENT PATTERNS HAVE JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 1199
!
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GO TO 1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPSEPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE SEGMENT ... PATTERN COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE SEGMENT IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      SEGMENT 3 PATTERN SOLID')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXSEG)GO TO 1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPSEPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE SEGMENT ... PATTERN COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF SEGMENTS MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXSEG
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',   &
      'SEGMENT.')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1160 CONTINUE
      IF(NUMARG.LE.2)GO TO 1170
      IF(IHARG(3).EQ.'ON')GO TO 1170
      IF(IHARG(3).EQ.'OFF')GO TO 1170
      IF(IHARG(3).EQ.'AUTO')GO TO 1170
      IF(IHARG(3).EQ.'DEFA')GO TO 1170
      GO TO 1175
!
 1170 CONTINUE
      IHOLD=IDEFPA
      GO TO 1180
!
 1175 CONTINUE
      IHOLD=IHARG(3)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(3).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(3).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(3).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(3).EQ.'5')IHOLD='DA5'
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      ISEGPA(I)=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)I,ISEGPA(I)
 1186 FORMAT('THE PATTERN FOR SEGMENT ',I8,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPSEPA
      SUBROUTINE DPSEPC(ITEMP1,IFOUND,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--NORMALLY, CHARACTER AND LINE SETTINGS AND THEIR
!              ASSOCIATED ATTRIBUTES ARE SET WITH COMMANDS OF THE FORM
!
!                CHARACTER X Y Z
!                CHARACTER COLOR BLUE RED GREEN
!
!                LINE SOLID DASH DOTTED
!                LINE COLOR BLUE RED GREEN
!
!              THIS COMMAND ALLOWS SPECIFIC SETTING TO BE DEFINED.
!              A SINGLE SETTING CAN BE DEFINED WITH
!
!                 LET PLOT CHARACTER 25 = X
!                 LET PLOT CHARACTER COLOR 25 = RED
!
!              IN ADDITION, YOU CAN DEFINE A LIST OF VALUES.  FOR
!              EXAMPLE
!
!                  LET IINDX = DATA 5 10 15 20
!                  LET PLOT CHARACTER IINDX = X
!
!                  LET IINDX = DATA 5 10 15 20
!                  LET PLOT CHARACTER IINDX = W X Y Z
!
!              THIS SYNTAX CAN BE HELPFUL IF YOU WANT TO SET A PLOT
!              CONTROL SETTING WITH A HIGH INDEX NUMBER WITHOUT
!              REPEATING THE SEQUENCE THAT COMES BEFORE.  THE
!              INDEX FORM (I.E., THE SECOND SET OF EXAMPLES) CAN
!              BE USED FOR REPEATING SEQUENCES.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2014/08
!     ORIGINAL VERSION--AUGUST    2014.
!     UPDATED         --JUNE      2018. SET IHYPSW TO OFF FOR DPEXWO
!                                       (ISSUE WITH
!                                          LET PLOT CHAR 4 = -  )
!     UPDATED         --MAY       2021. SUPPORT FOR RGB COLORS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      INCLUDE 'DPCOPA.INC'
!
      INTEGER ITEMP1(*)
!
      CHARACTER*4 IFOUND
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASE2
      CHARACTER*4 ICASE3
      CHARACTER*4 ICASE4
      CHARACTER*4 ICASE5
      CHARACTER*4 IVAL1
      CHARACTER*4 IVAL2
      CHARACTER*4 IVAL3
      CHARACTER*4 IVAL4
      CHARACTER*4 IVAL5
      CHARACTER*4 IVAL6
      CHARACTER*4 IHYPSV
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*24 ISTR
!CCCC CHARACTER*80 ISTRIN
!CCCC CHARACTER*80 ISTRCH
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXSTR) :: ISTRCH
!
!---------------------------------------------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSE'
      ISUBN2='PC  '
      IERROR='NO'
      IHYPSV=IHYPSW
      IHYPSW='OFF'
!
      ILOC3=0
      NCCHAR=0
      NUMVAL=0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SEPC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSEPC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,NUMNAM
   52   FORMAT('IBUGA3,ISUBRO,NUMNAM = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NUMNAM
          WRITE(ICOUT,56)I,IHNAME(I),IHNAM2(I),IUSE(I),IARGT(I)
   56     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IARGT(I) =',   &
                 I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEPC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFOUND='NO'
      ICASE2='    '
      ICASE3='    '
      ICASE4='ALPH'
      ICASE5='    '
      ISTR=' '
      NARG=1
!
!               *****************************************************
!               **  STEP 2--                                       **
!               **  DETERMINE PLOT CONTROL SETTING BEING CHANGED   **
!               *****************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEPC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IHARG(1).EQ.'PLOT')THEN
        IF(IHARG(2).EQ.'CHAR')THEN
          ICASE2='CHAR'
          IF(IHARG(3).EQ.'COLO' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='COLO'
            ISTR='CHARACTER COLOR'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'RGB ' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='RGBC'
            ISTR='CHARACTER RGB COLOR'
            NINDX=5
            NARG=3
          ELSEIF(IHARG(3).EQ.'FILL' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='FILL'
            ISTR='CHARACTER FILL'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'FONT' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='FONT'
            ISTR='CHARACTER FONT'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'CASE' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='CASE'
            ISTR='CHARACTER CASE'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'JUST' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='JUST'
            ISTR='CHARACTER JUSTIFICATION'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'SIZE' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='SIZE'
            ISTR='CHARACTER SIZE'
            ICASE4='NUME'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'HW' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='HW  '
            ISTR='CHARACTER HW'
            ICASE4='NUME'
            NARG=2
            NINDX=4
          ELSEIF(IHARG(3).EQ.'THIC' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='THIC'
            ISTR='CHARACTER THICKNESS'
            ICASE4='NUME'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'OFFS' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='OFFS'
            ICASE4='NUME'
            ISTR='CHARACTER OFFSET'
            NARG=2
            NINDX=4
          ELSEIF(IHARG(3).EQ.'ANGL' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='ANGL'
            ICASE4='NUME'
            ISTR='CHARACTER ANGLE'
            NINDX=4
          ELSEIF(IHARG(4).EQ.'=   ')THEN
            ICASE3='CHAR'
            ISTR='CHARACTER'
            NINDX=3
          ELSE
            GO TO 9000
          ENDIF
        ELSEIF(IHARG(2).EQ.'LINE')THEN
          ICASE2='LINE'
          IF(IHARG(3).EQ.'COLO' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='COLO'
            ISTR='LINE COLOR'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'RGB ' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='RGBC'
            ISTR='LINE RGB COLOR'
            NINDX=5
            NARG=3
          ELSEIF(IHARG(3).EQ.'THIC' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='THIC'
            ISTR='LINE THICKNESS'
            ICASE4='NUME'
            NINDX=4
          ELSEIF(IHARG(4).EQ.'=   ')THEN
            ICASE3='LINE'
            ISTR='LINE'
            NINDX=3
          ELSE
            GO TO 9000
          ENDIF
        ELSEIF(IHARG(2).EQ.'SPIK')THEN
          ICASE2='SPIK'
          IF(IHARG(3).EQ.'COLO' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='COLO'
            ISTR='SPIKE COLOR'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'RGB ' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='RGBC'
            ISTR='SPIKE RGB COLOR'
            NINDX=5
            NARG=3
          ELSEIF(IHARG(3).EQ.'LINE' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='LINE'
            ISTR='SPIKE LINE'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'THIC' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='THIC'
            ISTR='SPIKE THICKNESS'
            ICASE4='NUME'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'BASE' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='BASE'
            ISTR='SPIKE BASE'
            ICASE4='NUME'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'DIRE' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='DIRE'
            ISTR='SPIKE DIRECTION'
            NINDX=4
          ELSEIF(IHARG(4).EQ.'=   ')THEN
            ICASE3='SPIK'
            ISTR='SPIKE'
            NINDX=3
          ELSE
            GO TO 9000
          ENDIF
        ELSEIF(IHARG(2).EQ.'BAR ')THEN
          ICASE2='BAR'
          IF(IHARG(3).EQ.'COLO' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='COLO'
            ISTR='BAR COLOR'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'RGB ' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='RGBC'
            ISTR='BAR RGB COLOR'
            NINDX=5
            NARG=3
          ELSEIF(IHARG(3).EQ.'FILL' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='FILL'
            ISTR='BAR FILL'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'THIC' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='THIC'
            ICASE4='NUME'
            ISTR='BAR THICKNESS'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'LINE' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='PLIN'
            ISTR='BAR PATTERN LINE'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='PCOL'
            ISTR='BAR PATTERN COLOR'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'RGB ' .AND.   &
                 IHARG(5).EQ.'COLO' .AND. IHARG(7).EQ.'=   ')THEN
            ICASE3='PCOR'
            ISTR='BAR PATTERN RGB COLOR'
            NINDX=6
            NARG=3
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'THIC' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='PTHI'
            ISTR='BAR PATTERN THICKNESS'
            ICASE4='NUME'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'SPAC' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='PSPA'
            ICASE4='NUME'
            ISTR='BAR PATTERN SPACING'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='PTYP'
            ISTR='BAR PATTERN TYPE'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'FILL' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='FCOL'
            ISTR='BAR FILL COLOR'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'FILL' .AND. IHARG(4).EQ.'RGB ' .AND.   &
                 IHARG(5).EQ.'COLO' .AND. IHARG(7).EQ.'=   ')THEN
            ICASE3='FCOR'
            ISTR='BAR FILL RGB COLOR'
            NINDX=6
            NARG=3
          ELSEIF(IHARG(3).EQ.'FILL' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='FILL'
            ISTR='BAR FILL'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'BORD' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='BCOL'
            ISTR='BAR BORDER COLOR'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'BORD' .AND. IHARG(4).EQ.'RGB ' .AND.   &
                 IHARG(5).EQ.'COLO' .AND. IHARG(7).EQ.'=   ')THEN
            ICASE3='BCOR'
            ISTR='BAR BORDER RGB COLOR'
            NINDX=6
            NARG=3
          ELSEIF(IHARG(3).EQ.'BORD' .AND. IHARG(4).EQ.'THIC' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='BTHI'
            ICASE4='NUME'
            ISTR='BAR BORDER THICKNESS'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'BORD' .AND. IHARG(4).EQ.'LINE' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='BLIN'
            ISTR='BAR BORDER LINE'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'WIDT' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='WIDT'
            ICASE4='NUME'
            ISTR='BAR WIDTH'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'BASE' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='BASE'
            ICASE4='NUME'
            ISTR='BAR BASE'
            NINDX=4
          ELSEIF(IHARG(4).EQ.'=   ')THEN
            ICASE3='BAR '
            ISTR='BAR'
            NINDX=3
          ELSE
            GO TO 9000
          ENDIF
        ELSEIF(IHARG(2).EQ.'REGI ')THEN
          ICASE2='REGI'
          IF(IHARG(3).EQ.'FILL' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='FILL'
            ISTR='REGION FILL'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='PATT'
            ISTR='REGION PATTERN'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'BASE' .AND. IHARG(5).EQ.'=   ')THEN
            ICASE3='BASE'
            ICASE4='NUME'
            ISTR='REGION BASE'
            NINDX=4
          ELSEIF(IHARG(3).EQ.'FILL' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='FCOL'
            ISTR='REGION FILL COLOR'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'FILL' .AND. IHARG(4).EQ.'RGB ' .AND.   &
                 IHARG(5).EQ.'COLO' .AND. IHARG(7).EQ.'=   ')THEN
            ICASE3='FCOR'
            ISTR='REGION FILL RGB COLOR'
            NINDX=6
            NARG=3
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='PCOL'
            ISTR='REGION PATTERN COLOR'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'RGB ' .AND.   &
                 IHARG(5).EQ.'COLO' .AND. IHARG(7).EQ.'=   ')THEN
            ICASE3='PCOR'
            ISTR='REGION PATTERN RGB COLOR'
            NINDX=5
            NARG=3
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'LINE' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='PLIN'
            ISTR='REGION PATTERN LINE'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'SPAC' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='PSPA'
            ICASE4='NUME'
            ISTR='REGION PATTERN SPACING'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'PATT' .AND. IHARG(4).EQ.'THIC' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='PTHI'
            ICASE4='NUME'
            ISTR='REGION PATTERN THICKNESS'
            NINDX=5
!
!         NOTE: CURRENTLY, THE "REGION BORDER" COMMANDS HAVE NO
!               EFFECT.  SO ALTHOUGH DATAPLOT ACCEPTS THESE
!               COMMANDS, THEY ARE ESSENTIALLY NULL.
!
          ELSEIF(IHARG(3).EQ.'BORD' .AND. IHARG(4).EQ.'COLO' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='BCOL'
            ISTR='REGION BORDER COLOR'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'BORD' .AND. IHARG(4).EQ.'RGB ' .AND.   &
                 IHARG(5).EQ.'COLO' .AND. IHARG(7).EQ.'=   ')THEN
            ICASE3='BCOR'
            ISTR='REGION BORDER RGB COLOR'
            NINDX=5
            NARG=3
          ELSEIF(IHARG(3).EQ.'BORD' .AND. IHARG(4).EQ.'LINE' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='BLIN'
            ISTR='REGION BORDER LINE'
            NINDX=5
          ELSEIF(IHARG(3).EQ.'BORD' .AND. IHARG(4).EQ.'THIC' .AND.   &
                 IHARG(6).EQ.'=   ')THEN
            ICASE3='BTHI'
            ICASE4='NUME'
            ISTR='REGION BORDER THICKNESS'
            NINDX=5
          ELSEIF(IHARG(4).EQ.'=   ')THEN
            ICASE3='REGI'
            ISTR='REGION'
            NINDX=3
          ELSE
            GO TO 9000
          ENDIF
        ENDIF
      ELSE
        GO TO 9000
      ENDIF
!
      IFOUND='YES'
!
!               *****************************************************
!               **  STEP 3--                                       **
!               **  DETERMINE INDEX PARAMETER/VARIABLE             **
!               *****************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEPC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IARGT(NINDX).EQ.'NUMB')THEN
        IINDX=INT(ARG(NINDX)+0.5)
        IF(IINDX.LT.1 .OR. IINDX.GT.100)THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
 2001     FORMAT('***** ERROR IN LET PLOT ... = ....   --')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2002)NINDX
 2002     FORMAT('      THE INDEX NUMBER, ARGUMENT ',I2,', IS NOT ',   &
                 'IN THE RANGE 1 TO 100.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2003)IINDX
 2003     FORMAT('      THE VALUE OF THE INDEX NUMBER IS ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        ICASE5='SCAL'
        GO TO 2199
      ELSE
        IHLEFT=IHARG(NINDX)
        IHLEF2=IHARG2(NINDX)
!
        DO 2100 I=1,NUMNAM
          I2=I
          IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
            IF(IUSE(I2).EQ.'P')THEN
              ICASE5='SCAL'
              IINDX=IVALUE(I2)
              IF(IINDX.LT.1 .OR. IINDX.GT.100)THEN
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2001)
                CALL DPWRST('XXX','BUG ')
                IERROR='YES'
                GO TO 9000
              ENDIF
              GO TO 2199
            ELSEIF(IUSE(I2).EQ.'V')THEN
              ICOLR=IVALUE(I2)
              NRIGHT=IN(I2)
              ICASE5='VARI'
!
!             EXTRACT THE INDEX VARIABLE
!
              MAXCP1=MAXCOL+1
              MAXCP2=MAXCOL+2
              MAXCP3=MAXCOL+3
              MAXCP4=MAXCOL+4
              MAXCP5=MAXCOL+5
              MAXCP6=MAXCOL+6
!
              IMAX=100
              J=0
              DO 2160 II=1,IMAX
!
                IF(II.LE.NRIGHT)THEN
                  J=J+1
                  IJ=MAXN*(ICOLR-1)+II
                  IF(ICOLR.LE.MAXCOL)ITEMP1(J)=INT(V(IJ) + 0.5)
                  IF(ICOLR.EQ.MAXCP1)ITEMP1(J)=INT(PRED(II) + 0.5)
                  IF(ICOLR.EQ.MAXCP2)ITEMP1(J)=INT(RES(II) + 0.5)
                  IF(ICOLR.EQ.MAXCP3)ITEMP1(J)=INT(YPLOT(II) + 0.5)
                  IF(ICOLR.EQ.MAXCP4)ITEMP1(J)=INT(XPLOT(II) + 0.5)
                  IF(ICOLR.EQ.MAXCP5)ITEMP1(J)=INT(X2PLOT(II) + 0.5)
                  IF(ICOLR.EQ.MAXCP6)ITEMP1(J)=INT(TAGPLO(II) + 0.5)
                ENDIF
 2160         CONTINUE
              NUMVAL=J
              GO TO 2199
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2113)IHLEFT,IHLEF2
 2113         FORMAT('      THE NAME FOR THE INDEX VARIABLE (',   &
                     A4,A4,')')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2115)
 2115         FORMAT('      ALREADY EXISTS, BUT NOT AS A PARAMETER ',   &
                     'OR A VARIABLE.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
          ENDIF
 2100   CONTINUE
      ENDIF
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2122)
 2122 FORMAT('      NO MATCH FOUND FOR THE INDEX VARIABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2123)IHLEFT,IHLEF2
 2123 FORMAT('      THE NAME OF THE INDEX VARIABLE IS ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
 2199 CONTINUE
!
!               *****************************************************
!               **  STEP 4--                                       **
!               **  NOW LOOP THROUGH THE INDEX VARIABLE            **
!               *****************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEPC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NINDX=NINDX+2
      IF(ICASE5.EQ.'SCAL')THEN
!
!               ********************************************
!               **  STEP 4A--                             **
!               **  CASE WHEN SINGLE INDEX SPECIFIED      **
!               ********************************************
!
        ISTEPN='4A'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEPC')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        AVAL1=CPUMIN
        AVAL2=CPUMIN
        AVAL3=CPUMIN
        IVAL1='NULL'
        IVAL2='NULL'
        IVAL3='NULL'
        IVAL4='NULL'
        IVAL5='NULL'
        IVAL6='NULL'
        IVAL1=IHARG(NINDX)
        IVAL2=IHARG2(NINDX)
        IF(IARGT(NINDX).EQ.'NUMB')THEN
          AVAL1=ARG(NINDX)
        ENDIF
        IF(NARG.GE.2)THEN
          NINDX=NINDX+1
          IF(IARGT(NINDX).EQ.'NUMB')THEN
            AVAL2=ARG(NINDX)
          ENDIF
          IVAL3=IHARG(NINDX)
          IVAL4=IHARG2(NINDX)
        ENDIF
        IF(NARG.GE.3)THEN
          NINDX=NINDX+1
          IF(IARGT(NINDX).EQ.'NUMB')THEN
            AVAL3=ARG(NINDX)
          ENDIF
          IVAL5=IHARG(NINDX)
          IVAL6=IHARG2(NINDX)
        ENDIF
!
!         2018/04: FOR CHARACTER COMMAND, CAN HAVE MORE THAN EIGHT
!                  CHARACTERS, SO USE DPEXWO.
!
!
        IF(ICASE2.EQ.'CHAR' .AND. ICASE3.EQ.'CHAR')THEN
          ISTART=1
          ISTOP=IWIDTH
          IWORD=NINDX+1
          NCCHAR=0
          ISTRIN=' '
          ISTRCH=' '
          DO 4001 II=1,IWIDTH
            ISTRIN(II:II)=IANSLC(II)(1:1)
 4001     CONTINUE
          CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                      ICOL1,ICOL2,ISTRCH,NCCHAR,   &
                      IBUGA3,ISUBRO,IERROR)
          IF(NCCHAR.GT.24)NCCHAR=24
          ICHAPA(IINDX)=' '
          ICHAPA(IINDX)(1:NCCHAR)=ISTRCH(1:NCCHAR)
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,4011)IINDX,ICHAPA(IINDX)
 4011       FORMAT('INDEX ',I3,' OF CHARACTER HAS BEEN SET TO ',A24)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 9000
        ENDIF
!
        IF(ICASE2.EQ.'CHAR')THEN
          IF(ICASE3.EQ.'CHAR')THEN
            IF(IVAL1.NE.'NULL')THEN
              ICHAPA(IINDX)(1:4)=IVAL1
              ICHAPA(IINDX)(5:8)=IVAL2
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              ICHAPA(IINDX)(1:4)=IVAL1
              ICHAPA(IINDX)(5:8)=IVAL2
            ENDIF
          ELSEIF(ICASE3.EQ.'COLO')THEN
            IF(IVAL1.NE.'NULL')THEN
              ICHACO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              ICHACO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'RGBC')THEN
            IF(AVAL1.GE.0.0)THEN
              ICHAC2(IINDX,1)=INT(AVAL1)
            ELSE
              ICHAC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              ICHAC2(IINDX,2)=INT(AVAL2)
            ELSE
              ICHAC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              ICHAC2(IINDX,3)=INT(AVAL3)
            ELSE
              ICHAC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(ICHAC2(IINDX,1))
            AVAL2=REAL(ICHAC2(IINDX,2))
            AVAL3=REAL(ICHAC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'FILL')THEN
            IF(IVAL1.NE.'NULL')THEN
              ICHAFI(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'FONT')THEN
            IF(IVAL1.NE.'NULL')THEN
              ICHAFO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'CASE')THEN
            IF(IVAL1.NE.'NULL')THEN
              ICHACA(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'JUST')THEN
            IF(IVAL1.NE.'NULL')THEN
              ICHAJU(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'SIZE')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PCHAHE(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'THIC')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PCHATH(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'ANGL')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              ACHAAN(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'HW')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PCHAHE(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
            IF(AVAL2.NE.CPUMIN)THEN
              PCHAWI(IINDX)=AVAL2
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'OFFS')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PCHAHO(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
            IF(AVAL2.NE.CPUMIN)THEN
              PCHAVO(IINDX)=AVAL2
            ELSE
              IERROR='YES'
            ENDIF
          ENDIF
        ELSEIF(ICASE2.EQ.'LINE')THEN
          IF(ICASE3.EQ.'LINE')THEN
            IF(IVAL1.NE.'NULL')THEN
              ILINPA(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'COLO')THEN
            IF(IVAL1.NE.'NULL')THEN
              ILINCO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'RGB')THEN
            IF(AVAL1.GE.0.0)THEN
              ILINC2(IINDX,1)=INT(AVAL1)
            ELSE
              ILINC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              ILINC2(IINDX,2)=INT(AVAL2)
            ELSE
              ILINC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              ILINC2(IINDX,3)=INT(AVAL3)
            ELSE
              ILINC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(ILINC2(IINDX,1))
            AVAL2=REAL(ILINC2(IINDX,2))
            AVAL3=REAL(ILINC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'THIC')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PLINTH(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ENDIF
        ELSEIF(ICASE2.EQ.'SPIK')THEN
          IF(ICASE3.EQ.'SPIK')THEN
            IF(IVAL1.NE.'NULL')THEN
              ISPISW(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'COLO')THEN
            IF(IVAL1.NE.'NULL')THEN
              ISPICO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              ISPICO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'RGBC')THEN
            IF(AVAL1.GE.0.0)THEN
              ISPIC2(IINDX,1)=INT(AVAL1)
            ELSE
              ISPIC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              ISPIC2(IINDX,2)=INT(AVAL2)
            ELSE
              ISPIC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              ISPIC2(IINDX,3)=INT(AVAL3)
            ELSE
              ISPIC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(ISPIC2(IINDX,1))
            AVAL2=REAL(ISPIC2(IINDX,2))
            AVAL3=REAL(ISPIC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'LINE')THEN
            IF(IVAL1.NE.'NULL')THEN
              ISPILI(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'DIRE')THEN
            IF(IVAL1.NE.'NULL')THEN
              ISPIDI(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'THIC')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PSPITH(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'BASE')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              ASPIBA(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ENDIF
        ELSEIF(ICASE2.EQ.'BAR')THEN
          IF(ICASE3.EQ.'BAR')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBARSW(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'FILL')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBAFSW(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'COLO')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBABCO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              IBABCO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'RGBC')THEN
            IF(AVAL1.GE.0.0)THEN
              IBABC2(IINDX,1)=INT(AVAL1)
            ELSE
              IBABC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              IBABC2(IINDX,2)=INT(AVAL2)
            ELSE
              IBABC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              IBABC2(IINDX,3)=INT(AVAL3)
            ELSE
              IBABC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(IBABC2(IINDX,1))
            AVAL2=REAL(IBABC2(IINDX,2))
            AVAL3=REAL(IBABC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'THIC')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PBABTH(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PLIN')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBAPLI(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PCOL')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBAPCO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              IBAPCO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PCOR')THEN
            IF(AVAL1.GE.0.0)THEN
              IBAPC2(IINDX,1)=INT(AVAL1)
            ELSE
              IBAPC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              IBAPC2(IINDX,2)=INT(AVAL2)
            ELSE
              IBAPC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              IBAPC2(IINDX,3)=INT(AVAL3)
            ELSE
              IBAPC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(IBAPC2(IINDX,1))
            AVAL2=REAL(IBAPC2(IINDX,2))
            AVAL3=REAL(IBAPC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'PTHI')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PBAPTH(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PSPA')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PBAPSP(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PTYP')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBAPTY(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'FCOL')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBAFCO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              IBAFCO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'FCOR')THEN
            IF(AVAL1.GE.0.0)THEN
              IBAFC2(IINDX,1)=INT(AVAL1)
            ELSE
              IBAFC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              IBAFC2(IINDX,2)=INT(AVAL2)
            ELSE
              IBAFC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              IBAFC2(IINDX,3)=INT(AVAL3)
            ELSE
              IBAFC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(IBAFC2(IINDX,1))
            AVAL2=REAL(IBAFC2(IINDX,2))
            AVAL3=REAL(IBAFC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'BCOL')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBABCO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              IBABCO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'BCOR')THEN
            IF(AVAL1.GE.0.0)THEN
              IBABC2(IINDX,1)=INT(AVAL1)
            ELSE
              IBABC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              IBABC2(IINDX,2)=INT(AVAL2)
            ELSE
              IBABC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              IBABC2(IINDX,3)=INT(AVAL3)
            ELSE
              IBABC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(IBABC2(IINDX,1))
            AVAL2=REAL(IBABC2(IINDX,2))
            AVAL3=REAL(IBABC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'BTHI')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PBABTH(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'BLIN')THEN
            IF(IVAL1.NE.'NULL')THEN
              IBABLI(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'WIDT')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              ABARWI(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'BASE')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              ABARBA(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ENDIF
        ELSEIF(ICASE2.EQ.'REGI')THEN
          IF(ICASE3.EQ.'REGI')THEN
            IF(IVAL1.NE.'NULL')THEN
              IREGSW(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'FILL')THEN
            IF(IVAL1.NE.'NULL')THEN
              IREFSW(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'FCOL')THEN
            IF(IVAL1.NE.'NULL')THEN
              IREFCO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              IREFCO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'FCOR')THEN
            IF(AVAL1.GE.0.0)THEN
              IREFC2(IINDX,1)=INT(AVAL1)
            ELSE
              IREFC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              IREFC2(IINDX,2)=INT(AVAL2)
            ELSE
              IREFC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              IREFC2(IINDX,3)=INT(AVAL3)
            ELSE
              IREFC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(IREFC2(IINDX,1))
            AVAL2=REAL(IREFC2(IINDX,2))
            AVAL3=REAL(IREFC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'PATT')THEN
            IF(IVAL1.NE.'NULL')THEN
              IREPTY(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PCOL')THEN
            IF(IVAL1.NE.'NULL')THEN
              IREPCO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              IREPCO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PCOR')THEN
            IF(AVAL1.GE.0.0)THEN
              IREPC2(IINDX,1)=INT(AVAL1)
            ELSE
              IREPC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              IREPC2(IINDX,2)=INT(AVAL2)
            ELSE
              IREPC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              IREPC2(IINDX,3)=INT(AVAL3)
            ELSE
              IREPC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(IREPC2(IINDX,1))
            AVAL2=REAL(IREPC2(IINDX,2))
            AVAL3=REAL(IREPC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'PLIN')THEN
            IF(IVAL1.NE.'NULL')THEN
              IREPLI(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'BCOL')THEN
            IF(IVAL1.NE.'NULL')THEN
              IREBCO(IINDX)=IVAL1
            ELSEIF(AVAL1.NE.CPUMIN)THEN
              IREBCO(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'BCOR')THEN
            IF(AVAL1.GE.0.0)THEN
              IREBC2(IINDX,1)=INT(AVAL1)
            ELSE
              IREBC2(IINDX,1)=-1
            ENDIF
            IF(AVAL2.GE.0.0)THEN
              IREBC2(IINDX,2)=INT(AVAL2)
            ELSE
              IREBC2(IINDX,2)=-1
            ENDIF
            IF(AVAL3.GE.0.0)THEN
              IREBC2(IINDX,3)=INT(AVAL3)
            ELSE
              IREBC2(IINDX,3)=-1
            ENDIF
            AVAL1=REAL(IREBC2(IINDX,1))
            AVAL2=REAL(IREBC2(IINDX,2))
            AVAL3=REAL(IREBC2(IINDX,3))
          ELSEIF(ICASE3.EQ.'BLIN')THEN
            IF(IVAL1.NE.'NULL')THEN
              IREBLI(IINDX)=IVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PTHI')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PREPTH(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'PSPA')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PREPSP(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'BTHI')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              PREBTH(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ELSEIF(ICASE3.EQ.'BASE')THEN
            IF(AVAL1.NE.CPUMIN)THEN
              AREGBA(IINDX)=AVAL1
            ELSE
              IERROR='YES'
            ENDIF
          ENDIF
        ENDIF
!
!               *****************************************************
!               **  STEP 4B--                                      **
!               **  NOW PRINT FEEDBACK MESSAGE                     **
!               *****************************************************
!
        ISTEPN='5'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEPC')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(IFEEDB.EQ.'ON')THEN
!
          IFLAGR=0
          IF(ICASE3.EQ.'RGBC')IFLAGR=1
          IF(ICASE3.EQ.'PCOR')IFLAGR=1
          IF(ICASE3.EQ.'FCOR')IFLAGR=1
          IF(ICASE3.EQ.'BCOR')IFLAGR=1
!
          IF(IFLAGR.EQ.0)THEN
            IF(ICASE4.EQ.'NUME' .AND. NARG.EQ.1)THEN
              WRITE(ICOUT,8011)IINDX,ISTR,AVAL1
 8011         FORMAT('INDEX ',I3,' OF ',A24,' HAS BEEN SET TO ',G15.7)
              CALL DPWRST('XXX','BUG ')
            ELSEIF(ICASE4.EQ.'NUME' .AND. NARG.EQ.2)THEN
              WRITE(ICOUT,8013)IINDX,ISTR,AVAL1,AVAL2
 8013         FORMAT('INDEX ',I3,' OF ',A24,' HAS BEEN SET TO ',G15.7,   &
                     ' AND ',G15.7)
              CALL DPWRST('XXX','BUG ')
            ELSEIF(ICASE4.EQ.'ALPH' .AND. NARG.EQ.1)THEN
              WRITE(ICOUT,8015)IINDX,ISTR,IVAL1
 8015         FORMAT('INDEX ',I3,' OF ',A24,' HAS BEEN SET TO ',A4)
              CALL DPWRST('XXX','BUG ')
            ELSEIF(ICASE4.EQ.'ALPH' .AND. NARG.EQ.2)THEN
              WRITE(ICOUT,8017)IINDX,ISTR,IVAL1,IVAL2
 8017         FORMAT('INDEX ',I3,' OF ',A24,' HAS BEEN SET TO ',2A4)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSE
            WRITE(ICOUT,8117)IINDX,ISTR,   &
                             INT(AVAL1),INT(AVAL2),INT(AVAL3)
 8117       FORMAT('INDEX ',I3,' OF ',A24,' HAS BEEN SET TO ',3I6)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
      ELSE
!
!               ********************************************
!               **  STEP 5--                              **
!               **  CASE WHEN VARIABLE INDEX SPECIFIED    **
!               ********************************************
!
        ISTEPN='5'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEPC')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(NUMVAL.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3001)
 3001     FORMAT('      THERE ARE NO ELEMENTS IN THE INDEX VARIABLE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        NINDSV=NINDX
        DO 3100 II=1,NUMVAL
          IINDXT=ITEMP1(II)
          IF(IINDXT.LT.1 .OR. IINDXT.GT.100)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3006)II
 3006       FORMAT('      ROW ',I8,' OF THE INDEX VARIABLE IS LESS ',   &
                   'THAN 1 OR GREATER THAN 100.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3008)IIDXT
 3008       FORMAT('      THE INDEX VALUE IS ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          AVAL1=CPUMIN
          AVAL2=CPUMIN
          AVAL3=CPUMIN
          IVAL1='NULL'
          IVAL2='NULL'
          IVAL3='NULL'
          IVAL4='NULL'
          IVAL5='NULL'
          IVAL6='NULL'
          IF(IARGT(NINDX).EQ.'NUMB')THEN
            AVAL1=ARG(NINDX)
          ELSE
            IVAL1=IHARG(NINDX)
            IVAL2=IHARG2(NINDX)
          ENDIF
          IF(NARG.GE.2)THEN
            NINDX=NINDX+1
            IF(IARGT(NINDX).EQ.'NUMB')THEN
              AVAL2=ARG(NINDX)
            ELSE
              IVAL3=IHARG(NINDX)
              IVAL4=IHARG2(NINDX)
            ENDIF
          ENDIF
          IF(NARG.GE.3)THEN
            NINDX=NINDX+1
            IF(IARGT(NINDX).EQ.'NUMB')THEN
              AVAL3=ARG(NINDX)
            ELSE
              IVAL5=IHARG(NINDX)
              IVAL6=IHARG2(NINDX)
            ENDIF
          ENDIF
          NINDX=NINDX+1
          IF(NINDX.GT.NUMARG)NINDX=NINDSV
!
!         2018/04: FOR CHARACTER COMMAND, CAN HAVE MORE THAN EIGHT
!                  CHARACTERS, SO USE DPEXWO.
!
          IF(ICASE2.EQ.'CHAR' .AND. ICASE3.EQ.'CHAR')THEN
            ISTART=1
            ISTOP=IWIDTH
            IWORD=NINDX+1
            NCCHAR=0
            ISTRIN=' '
            ISTRCH=' '
            DO 5001 JJ=1,IWIDTH
              ISTRIN(JJ:JJ)=IANSLC(JJ)(1:1)
 5001       CONTINUE
            CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
                        ICOL1,ICOL2,ISTRCH,NCCHAR,   &
                        IBUGA3,ISUBRO,IERROR)
            IF(NCCHAR.GT.24)NCCHAR=24
            ICHAPA(IINDXT)=' '
            ICHAPA(IINDXT)(1:NCCHAR)=ISTRCH(1:NCCHAR)
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,4011)IINDXT,ICHAPA(IINDXT)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            GO TO 3100
          ENDIF
!
          IF(ICASE2.EQ.'CHAR')THEN
            IF(ICASE3.EQ.'CHAR')THEN
              IF(IVAL1.NE.'NULL')THEN
                ICHAPA(IINDXT)(1:4)=IVAL1
                ICHAPA(IINDXT)(5:8)=IVAL2
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'COLO')THEN
              IF(IVAL1.NE.'NULL')THEN
                ICHACO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'RGBC')THEN
              IF(AVAL1.GE.0.0)THEN
                ICHAC2(IINDXT,1)=INT(AVAL1)
              ELSE
                ICHAC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                ICHAC2(IINDXT,2)=INT(AVAL2)
              ELSE
                ICHAC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                ICHAC2(IINDXT,3)=INT(AVAL3)
              ELSE
                ICHAC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(ICHAC2(IINDXT,1))
              AVAL2=REAL(ICHAC2(IINDXT,2))
              AVAL3=REAL(ICHAC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'FILL')THEN
              IF(IVAL1.NE.'NULL')THEN
                ICHAFI(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'FONT')THEN
              IF(IVAL1.NE.'NULL')THEN
                ICHAFO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'CASE')THEN
              IF(IVAL1.NE.'NULL')THEN
                ICHACA(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'JUST')THEN
              IF(IVAL1.NE.'NULL')THEN
                ICHAJU(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'SIZE')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PCHAHE(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'THIC')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PCHATH(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'ANGL')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                ACHAAN(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'HW')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PCHAHE(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
              IF(AVAL2.NE.CPUMIN)THEN
                PCHAWI(IINDXT)=AVAL2
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'OFFS')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PCHAHO(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
              IF(AVAL2.NE.CPUMIN)THEN
                PCHAVO(IINDXT)=AVAL2
              ELSE
                IERROR='YES'
              ENDIF
            ENDIF
          ELSEIF(ICASE2.EQ.'LINE')THEN
            IF(ICASE3.EQ.'LINE')THEN
              IF(IVAL1.NE.'NULL')THEN
                ILINPA(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'COLO')THEN
              IF(IVAL1.NE.'NULL')THEN
                ILINCO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'RGBC')THEN
              IF(AVAL1.GE.0.0)THEN
                ILINC2(IINDXT,1)=INT(AVAL1)
              ELSE
                ILINC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                ILINC2(IINDXT,2)=INT(AVAL2)
              ELSE
                ILINC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                ILINC2(IINDXT,3)=INT(AVAL3)
              ELSE
                ILINC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(ILINC2(IINDXT,1))
              AVAL2=REAL(ILINC2(IINDXT,2))
              AVAL3=REAL(ILINC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'THIC')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PLINTH(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ENDIF
          ELSEIF(ICASE2.EQ.'SPIK')THEN
            IF(ICASE3.EQ.'SPIK')THEN
              IF(IVAL1.NE.'NULL')THEN
                ISPISW(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'COLO')THEN
              IF(IVAL1.NE.'NULL')THEN
                ISPICO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'RGBC')THEN
              IF(AVAL1.GE.0.0)THEN
                ISPIC2(IINDXT,1)=INT(AVAL1)
              ELSE
                ISPIC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                ISPIC2(IINDXT,2)=INT(AVAL2)
              ELSE
                ISPIC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                ISPIC2(IINDXT,3)=INT(AVAL3)
              ELSE
                ISPIC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(ISPIC2(IINDXT,1))
              AVAL2=REAL(ISPIC2(IINDXT,2))
              AVAL3=REAL(ISPIC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'LINE')THEN
              IF(IVAL1.NE.'NULL')THEN
                ISPILI(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'DIRE')THEN
              IF(IVAL1.NE.'NULL')THEN
                ISPIDI(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'THIC')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PSPITH(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'BASE')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                ASPIBA(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ENDIF
          ELSEIF(ICASE2.EQ.'BAR')THEN
            IF(ICASE3.EQ.'BAR')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBARSW(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'FILL')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBAFSW(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'COLO')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBABCO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'RGBC')THEN
              IF(AVAL1.GE.0.0)THEN
                IBABC2(IINDXT,1)=INT(AVAL1)
              ELSE
                IBABC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                IBABC2(IINDXT,2)=INT(AVAL2)
              ELSE
                IBABC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                IBABC2(IINDXT,3)=INT(AVAL3)
              ELSE
                IBABC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(IBABC2(IINDXT,1))
              AVAL2=REAL(IBABC2(IINDXT,2))
              AVAL3=REAL(IBABC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'THIC')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PBABTH(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PLIN')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBAPLI(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PCOL')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBAPCO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PCOR')THEN
              IF(AVAL1.GE.0.0)THEN
                IBAPC2(IINDXT,1)=INT(AVAL1)
              ELSE
                IBAPC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                IBAPC2(IINDXT,2)=INT(AVAL2)
              ELSE
                IBAPC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                IBAPC2(IINDXT,3)=INT(AVAL3)
              ELSE
                IBAPC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(IBAPC2(IINDXT,1))
              AVAL2=REAL(IBAPC2(IINDXT,2))
              AVAL3=REAL(IBAPC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'PTHI')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PBAPTH(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PSPA')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PBAPSP(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PTYP')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBAPTY(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'FCOL')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBAFCO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'FCOR')THEN
              IF(AVAL1.GE.0.0)THEN
                IBAFC2(IINDXT,1)=INT(AVAL1)
              ELSE
                IBAFC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                IBAFC2(IINDXT,2)=INT(AVAL2)
              ELSE
                IBAFC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                IBAFC2(IINDXT,3)=INT(AVAL3)
              ELSE
                IBAFC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(IBAFC2(IINDXT,1))
              AVAL2=REAL(IBAFC2(IINDXT,2))
              AVAL3=REAL(IBAFC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'BCOL')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBABCO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'BCOR')THEN
              IF(AVAL1.GE.0.0)THEN
                IBABC2(IINDXT,1)=INT(AVAL1)
              ELSE
                IBABC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                IBABC2(IINDXT,2)=INT(AVAL2)
              ELSE
                IBABC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                IBABC2(IINDXT,3)=INT(AVAL3)
              ELSE
                IBABC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(IBABC2(IINDXT,1))
              AVAL2=REAL(IBABC2(IINDXT,2))
              AVAL3=REAL(IBABC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'BTHI')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PBABTH(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'BLIN')THEN
              IF(IVAL1.NE.'NULL')THEN
                IBABLI(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'WIDT')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                ABARWI(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'BASE')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                ABARBA(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ENDIF
          ELSEIF(ICASE2.EQ.'REGI')THEN
            IF(ICASE3.EQ.'REGI')THEN
              IF(IVAL1.NE.'NULL')THEN
                IREGSW(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'FILL')THEN
              IF(IVAL1.NE.'NULL')THEN
                IREFSW(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'FCOL')THEN
              IF(IVAL1.NE.'NULL')THEN
                IREFCO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'FCOR')THEN
              IF(AVAL1.GE.0.0)THEN
                IREFC2(IINDXT,1)=INT(AVAL1)
              ELSE
                IREFC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                IREFC2(IINDXT,2)=INT(AVAL2)
              ELSE
                IREFC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                IREFC2(IINDXT,3)=INT(AVAL3)
              ELSE
                IREFC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(IREFC2(IINDXT,1))
              AVAL2=REAL(IREFC2(IINDXT,2))
              AVAL3=REAL(IREFC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'PATT')THEN
              IF(IVAL1.NE.'NULL')THEN
                IREPTY(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PCOL')THEN
              IF(IVAL1.NE.'NULL')THEN
                IREPCO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PCOR')THEN
              IF(AVAL1.GE.0.0)THEN
                IREPC2(IINDXT,1)=INT(AVAL1)
              ELSE
                IREPC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                IREPC2(IINDXT,2)=INT(AVAL2)
              ELSE
                IREPC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                IREPC2(IINDXT,3)=INT(AVAL3)
              ELSE
                IREPC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(IREPC2(IINDXT,1))
              AVAL2=REAL(IREPC2(IINDXT,2))
              AVAL3=REAL(IREPC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'PLIN')THEN
              IF(IVAL1.NE.'NULL')THEN
                IREPLI(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'BCOL')THEN
              IF(IVAL1.NE.'NULL')THEN
                IREBCO(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'BCOR')THEN
              IF(AVAL1.GE.0.0)THEN
                IREBC2(IINDXT,1)=INT(AVAL1)
              ELSE
                IREBC2(IINDXT,1)=-1
              ENDIF
              IF(AVAL2.GE.0.0)THEN
                IREBC2(IINDXT,2)=INT(AVAL2)
              ELSE
                IREBC2(IINDXT,2)=-1
              ENDIF
              IF(AVAL3.GE.0.0)THEN
                IREBC2(IINDXT,3)=INT(AVAL3)
              ELSE
                IREBC2(IINDXT,3)=-1
              ENDIF
              AVAL1=REAL(IREBC2(IINDXT,1))
              AVAL2=REAL(IREBC2(IINDXT,2))
              AVAL3=REAL(IREBC2(IINDXT,3))
            ELSEIF(ICASE3.EQ.'BLIN')THEN
              IF(IVAL1.NE.'NULL')THEN
                IREBLI(IINDXT)=IVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PTHI')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PREPTH(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'PSPA')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PREPSP(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'BTHI')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                PREBTH(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ELSEIF(ICASE3.EQ.'BASE')THEN
              IF(AVAL1.NE.CPUMIN)THEN
                AREGBA(IINDXT)=AVAL1
              ELSE
                IERROR='YES'
              ENDIF
            ENDIF
          ENDIF
!
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3096)II
 3096       FORMAT('      FOR ROW ',I8,' OF THE INDEX VARIABLE, ',   &
                   'UNABLE TO MAKE THE ASSIGNMENT.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
          ELSE
            IF(IFEEDB.EQ.'ON')THEN
!
              IFLAGR=0
              IF(ICASE3.EQ.'RGBC')IFLAGR=1
              IF(ICASE3.EQ.'PCOR')IFLAGR=1
              IF(ICASE3.EQ.'FCOR')IFLAGR=1
              IF(ICASE3.EQ.'BCOR')IFLAGR=1
!
              IF(IFLAGR.EQ.0)THEN
                IF(ICASE4.EQ.'NUME' .AND. NARG.EQ.1)THEN
                  WRITE(ICOUT,8011)IINDXT,ISTR,AVAL1
                  CALL DPWRST('XXX','BUG ')
                ELSEIF(ICASE4.EQ.'NUME' .AND. NARG.EQ.2)THEN
                  WRITE(ICOUT,8013)IINDXT,ISTR,AVAL1,AVAL2
                  CALL DPWRST('XXX','BUG ')
                ELSEIF(ICASE4.EQ.'ALPH' .AND. NARG.EQ.1)THEN
                  WRITE(ICOUT,8015)IINDXT,ISTR,IVAL1
                  CALL DPWRST('XXX','BUG ')
                ELSEIF(ICASE4.EQ.'ALPH' .AND. NARG.EQ.2)THEN
                  WRITE(ICOUT,8017)IINDXT,ISTR,IVAL1,IVAL2
                  CALL DPWRST('XXX','BUG ')
                ENDIF
              ELSE
                WRITE(ICOUT,8117)IINDXT,ISTR,   &
                                 INT(AVAL1),INT(AVAL2),INT(AVAL3)
                CALL DPWRST('XXX','BUG ')
              ENDIF
            ENDIF
          ENDIF
!
 3100   CONTINUE
!
      ENDIF
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
      IHYPSW=IHYPSV
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SEPC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSEPC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,IINDX,NINDX
 9012   FORMAT('IFOUND,IERROR,IINDX,NINDX = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NARG,ICASE2,ICASE3,ICASE4,ICASE5
 9013   FORMAT('NARG,ICASE2,ICASE3,ICASE4,ICASE5 = ',I8,4(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IVAL1,IVAL2,AVAL1,AVAL2
 9014   FORMAT('IVAL1,IVAL2,AVAL2,AVAL2 = ',2(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSEPC
      SUBROUTINE DPSEQ(IHARG,IARGT,IARG,NUMARG,   &
                       ISEQSW,NUMSEQ,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE SEQUENCE SWITCH ISEQSW
!              AND THE START SEQUENCE NUMBER NUMSEQ .
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --IARG   (AN INTEGER VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--ISEQSW   ('ON'  OR 'OFF')
!                     --NUMSEQ   (AN INTEGER)
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1978.
!     UPDATED         --SEPTEMBER 1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 ISEQSW
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
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
      IF(NUMARG.EQ.0)GO TO 1110
      IF(NUMARG.EQ.1)GO TO 1120
      IF(NUMARG.GE.2)GO TO 1130
      GO TO 1190
!
 1110 CONTINUE
      ISEQSW='ON'
      NUMSEQ=1
      GO TO 1150
!
 1120 CONTINUE
      IF(IHARG(1).EQ.'ON')GO TO 1122
      IF(IHARG(1).EQ.'OFF')GO TO 1124
      IF(IHARG(1).EQ.'AUTO')GO TO 1122
      IF(IHARG(1).EQ.'DEFA')GO TO 1124
      IF(IARGT(1).EQ.'NUMB')GO TO 1126
      GO TO 1190
!
 1122 CONTINUE
      ISEQSW='ON'
      NUMSEQ=1
      GO TO 1150
!
 1124 CONTINUE
      ISEQSW='OFF'
      NUMSEQ=1
      GO TO 1160
!
 1126 CONTINUE
      ISEQSW='ON'
      NUMSEQ=IARG(1)
      GO TO 1150
!
 1130 CONTINUE
      IF(IHARG(1).EQ.'ON')GO TO 1132
      IF(IHARG(1).EQ.'OFF')GO TO 1134
      IF(IHARG(1).EQ.'AUTO')GO TO 1132
      IF(IHARG(1).EQ.'DEFA')GO TO 1134
      GO TO 1190
!
 1132 CONTINUE
      ISEQSW='ON'
      IF(IARGT(2).EQ.'NUMB')NUMSEQ=IARG(2)
      IF(IARGT(2).NE.'NUMB')NUMSEQ=1
      GO TO 1150
!
 1134 CONTINUE
      ISEQSW='OFF'
      NUMSEQ=1
      GO TO 1160
!
 1150 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('THE SEQUENCE SWITCH HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)NUMSEQ
 1156 FORMAT('(STARTING WITH SEQUENCE NUMBER ',I8,')')
      CALL DPWRST('XXX','BUG ')
 1159 CONTINUE
      GO TO 1180
!
 1160 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1169
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1165)
 1165 FORMAT('THE SEQUENCE SWITCH HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      GO TO 1190
!
 1190 CONTINUE
      RETURN
      END SUBROUTINE DPSEQ
      SUBROUTINE DPSEQU(IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A SEQUENCE.
!              GENERATE ELEMENTS OF A SEQUENCE
!              BY THE FORM (FOR EXAMPLE) LET Y = SEQUENCE 1 .01 10
!              OR BY THE ALTERNATE FORM   LET Y = 1 .01 10
!              (FOR A FULL VARIABLE OR PART OF A VARIABLE).
!     OUTPUT--NECESSARILY A VARIABLE.
!              EXAMPLE--LET Y    = 1 .01 10                  (A FULL VARIABLE)
!                     --LET Y    = 1 .01 10  SUBSET 2 3 5    (A PARTIAL VAR.)
!                     --LET Y    = 1 .01 10  FOR I = 1 2 10  (A PARTIAL VAR.)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION (IN DPLET)--DECEMBER  1977.
!     UPDATED         --MAY       1982.
!     ORIGINAL VERSION AS A SEPARATE SUBROUTINE--MARCH 1978.
!     UPDATED         --JUNE      1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --JUNE      1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --APRIL     1987.
!     UPDATED         --DECEMBER  1988. SHORTEN: LET Y = SEQU X
!     UPDATED         --DECEMBER  1988. PARAM TO VAR COLUMN BUG
!     UPDATED         --NOVEMBER  2010. ALLOW ARGUMENTS ON RIGHT HAND
!                                       SIDE OF "=" TO BE VARIABLES.
!     UPDATED         --JUNE      2018. TWEAK FORMAT FOR LARGE NUMBER OF
!                                       ROWS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ILEFT
      CHARACTER*4 ILEFT2
      CHARACTER*4 IH
      CHARACTER*4 IH2
!
!CCCC THE FOLLOWING LINE WAS INSERTED (DECEMBER 1988)
!CCCC TO FIX A PARAMETER TO VARIABLE COLUMN BUG (DECEMBER 1988)
      CHARACTER*4 IPTOV
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 ICASE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSE'
      ISUBN2='QU  '
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
      ICASE='PARA'
!
      I2=0
      NLEFT=0
      ICOLL=0
      N2=0
      NRAWSE=0
      NNUM=0
      NS2=0
      NS2MOD=0
!
      START=0.0
      REPS=1.0
      AINC=0.0
      STOP=0.0
      NSTART=1
      NREP=1
      NINC=1
      NSTOP=1
!
      ILEFT='UNKN'
      ILEFT2='UNKN'
!
!CCCC THE FOLLOWING LINE WAS INSERTED (DECEMBER 1988)
!CCCC TO FIX A PARAMETER TO VARIABLE COLUMN BUG (DECEMBER 1988)
      IPTOV='NO'
!
!               ********************************************************
!               **  TREAT THE SUBCASE OF GENERATING ELEMENTS          **
!               **  (EXPRESSED ON THE RIGHT AS 3 CONSTANTS--          **
!               **  START VALUE, INCREMENT, STOP VALUE)               **
!               **       1) FOR A FULL VARIABLE, OR                   **
!               **       2) FOR PART OF A VARIABLE.                   **
!               ********************************************************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SEQU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSEQU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA3,IBUGQ,ISUBRO = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='NO'
      NEWCOL='NO'
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=5
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
      IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *******************************************************
!               **  STEP 3--                                          *
!               **  EXAMINE THE LEFT-HAND SIDE--                      *
!               **  IS THE PARAMETER OR VARIABLE NAME TO LEFT OF =    *
!               **  SIGN ALREADY IN THE NAME LIST?                    *
!               **  NOTE THAT   ILEFT    IS THE NAME OF THE VARIABLE  *
!               **  ON THE LEFT.                                      *
!               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE  *
!               **  OF THE NAME ON THE LEFT.                          *
!               **  NOTE THAT   ICOLL  IS THE DATA COLUMN (1 TO 12)   *
!               **  FOR THE NAME OF THE LEFT.                         *
!               *******************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILEFT=IHARG(1)
      ILEFT2=IHARG2(1)
      DO 310 I=1,NUMNAM
        I2=I
        IF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.   &
          IUSE(I).EQ.'P')THEN
!CCCC     THE FOLLOWING LINE WAS INSERTED (DECEMBER 1988)
!CCCC     TO FIX A PARAMETER TO VARIABLE COLUMN BUG (DECEMBER 1988)
          IPTOV='YES'
          ILISTL=I2
          GO TO 330
        ELSEIF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.   &
          IUSE(I).EQ.'V')THEN
          ILISTL=I2
          ICOLL=IVALUE(ILISTL)
          NLEFT=IN(ILISTL)
          GO TO 390
        ELSEIF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.   &
          IUSE(I).EQ.'F')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,311)
  311     FORMAT('      THE NAME ON THE LEFT HAND SIDE OF THE  = ',   &
                 'SIGN')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,312)
  312     FORMAT('      WAS FOUND IN THE NAME LIST AS A ',   &
                 'STRING/FUNCTION.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.   &
          IUSE(I).EQ.'M')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,311)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,317)
  317     FORMAT('      WAS FOUND IN THE NAME LIST AS A MATRIX.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
        ENDIF
  310 CONTINUE
!
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,321)
  321   FORMAT('***** ERROR IN LET ... = SEQUENCE ...  COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,322)
  322   FORMAT('      THE NUMBER OF VARIABLE AND/OR PARAMETER NAMES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,323)MAXNAM
  323   FORMAT('      HAS JUST EXCEEDED THE MAXIMUM ALLOWABLE ',   &
               I8,'  .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,324)
  324   FORMAT('      SUGGESTED ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,325)
  325   FORMAT('      ENTER      STAT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,326)
  326   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,327)
  327   FORMAT('      AND THEN REDEFINE (REUSE) SOME OF THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,328)
  328   FORMAT('      ALREADY-USED NAMES')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
  330 CONTINUE
      NLEFT=0
      ICOLL=NUMCOL+1
      IF(ICOLL.GT.MAXCOL)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,321)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,342)
  342   FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST EXCEEDED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,343)MAXCOL
  343   FORMAT('      THE MAXIMUM ALLOWABLE (',I8,').')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,344)
  344   FORMAT('      SUGGESTED ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,345)
  345   FORMAT('      ENTER      STAT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,346)
  346   FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,347)
  347   FORMAT('      AND THEN OVERWRITE SOME COLUMNS.   EXAMPLE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,348)
  348   FORMAT('      IF       LET X = SEQUENCE 1 2 9        FAILED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,349)
  349   FORMAT('      THEN ONE MIGHT ENTER     NAME X 7')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,350)
  350   FORMAT('      (THEREBY EQUATING THE NAME X WITH COLUMN 7')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,351)
  351   FORMAT('      FOLLOWED BY        LET X = SEQUENCE 1 2 9')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,352)
  352   FORMAT('      (WHICH WILL ACTUALLY OVERWRITE COLUMN 7')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,353)
  353   FORMAT('      WITH THE NUMERIC CONSTANTS 3.14)')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
  390 CONTINUE
!
!               ********************************************************
!               **  STEP 4--                                           *
!               **  EXAMINE THE RIGHT-HAND SIDE--                      *
!               **  DO WE HAVE 3 OR 4 CONSTANTS,                       *
!               **  OR 3 OR 4 PARAMETERS,                              *
!               **  OR A MIXTURE OF CONSTANTS AND PARAMETERS?          *
!               **  (ALL OF THE ABOVE ARE ALLOWED.)                    *
!               ********************************************************
!
!     NOTE 11/2010:
!
!     1) ALLOW THE ARGUMENTS ON THE RHS TO BE EITHER PARAMETERS/CONSTANTS
!        OR VARIABLES OR A MIX OF ANY OF THESE.  NOTE THAT ARGUMENTS THAT
!        ARE VARIABLES MUST BE OF THE SAME LENGTH.
!
!
!     2) SUPPORT THE NEW SYNTAX
!
!             LET Y = SEQUENCE VALUE  REPEAT
!
!        WHERE VALUE AND REPEAT CAN BE EITHER PARAMETERS/CONSTANTS
!        OR VARIABLES.  IN THIS CASE, EACH ROW OF VALUE IS REPEATED
!        BY THE CORRESPONDING ROW IN REPEAT.
!
!        IF YOU HAVE A REGULAR SEQUENCE, YOU CAN JUST USE
!
!            LET Y = START REPEAT INC STOP
!
!        WHERE THIS SYNTAX CAN BE USEFUL IS WHEN YOU HAVE AN
!        IRREGULAR SEQUENCE.  FOR EXAMPLE
!
!            LET VALUE = DATA 1 2 5 7
!            LET REPEAT = DATA 3 3 3 2
!            LET Y = SEQUENCE VALUE REPEAT
!
!        THIS WOULD RETURN
!
!            1 1 1 2 2 2 5 5 5 7 7
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC THE FOLLOWING 2 LINES WERE COMMENTED OUT (DECEMBER 1988)
!CCCC AND REPLACED BY THE SUCCEEDING 2 LINES (DECEMBER 1988)
!CCCC SO THAT  SEQUENCE  NEED NOT BE SPELLED OUT FULLY. (DECEMBER 1988)
!CCCC IF(IHARG(3).EQ.'SEQU'.AND.IHARG2(3).EQ.'ENCE'.AND.
!CCCC NUMARG.GE.6)GO TO 1290
      IF(IHARG(3).EQ.'SEQU' .AND.  NUMARG.GE.5)GO TO 1290
      IF(NUMARG.GE.4)GO TO 1290
!
      WRITE(ICOUT,321)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      ILLEGAL SYNTAX FOR LET COMMAND.  THERE SHOULD BE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)
 1214 FORMAT('      AT LEAST TWO NUMBERS OR WORDS TO THE RIGHT OF ',   &
             '    SEQUENCE    OR   =')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1216)
 1216 FORMAT('      FOR THIS TYPE OF LET COMMAND.  SUCH WAS NOT THE ',   &
             'CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      NUMAM2=NUMARG-2
      WRITE(ICOUT,1218)NUMAM2
 1218 FORMAT('      NUMBER OF SUCH NUMBERS/WORDS FOUND = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1219)
 1219 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1220)(IANS(I),I=1,MIN(100,IWIDTH))
 1220   FORMAT('      ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
 1290 CONTINUE
!
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT (DECEMBER 1988)
!CCCC AND REPLACED BY THE SUCCEEDING LINE (DECEMBER 1988)
!CCCC SO THAT  SEQUENCE  NEED NOT BE SPELLED OUT FULLY. (DECEMBER 1988)
!CCCC IF(IHARG(3).EQ.'SEQU'.AND.IHARG2(3).EQ.'ENCE')GO TO 1302
      IF(IHARG(3).EQ.'SEQU')THEN
        NUMPAR=4
        IF(NUMARG.LE.5)THEN
          NUMPAR=2
        ELSEIF(NUMARG.LE.6)THEN
          NUMPAR=3
        ENDIF
        IF(IHARG(7).EQ.'SUBS'.AND.IHARG2(7).EQ.'ET')NUMPAR=3
        IF(IHARG(7).EQ.'EXCE'.AND.IHARG2(7).EQ.'PT')NUMPAR=3
        IF(IHARG(7).EQ.'FOR'.AND.IHARG2(7).EQ.'    ')NUMPAR=3
        IF(IHARG(6).EQ.'SUBS'.AND.IHARG2(7).EQ.'ET')NUMPAR=2
        IF(IHARG(6).EQ.'EXCE'.AND.IHARG2(7).EQ.'PT')NUMPAR=2
        IF(IHARG(6).EQ.'FOR'.AND.IHARG2(7).EQ.'    ')NUMPAR=2
      ELSE
        NUMPAR=4
        IF(NUMARG.LE.4)THEN
          NUMPAR=2
        ELSEIF(NUMARG.LE.5)THEN
          NUMPAR=3
        ENDIF
        IF(IHARG(6).EQ.'SUBS'.AND.IHARG2(6).EQ.'ET')NUMPAR=3
        IF(IHARG(6).EQ.'EXCE'.AND.IHARG2(6).EQ.'PT')NUMPAR=3
        IF(IHARG(6).EQ.'FOR'.AND.IHARG2(6).EQ.'    ')NUMPAR=3
        IF(IHARG(5).EQ.'SUBS'.AND.IHARG2(6).EQ.'ET')NUMPAR=2
        IF(IHARG(5).EQ.'EXCE'.AND.IHARG2(6).EQ.'PT')NUMPAR=2
        IF(IHARG(5).EQ.'FOR'.AND.IHARG2(6).EQ.'    ')NUMPAR=2
      ENDIF
!
!     CREATE THE "START" VARIABLE
!
      ILOCA=3
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT (DECEMBER 1988)
!CCCC AND REPLACED BY THE SUCCEEDING LINE (DECEMBER 1988)
!CCCC SO THAT  SEQUENCE  NEED NOT BE SPELLED OUT FULLY. (DECEMBER 1988)
!CCCC IF(IHARG(3).EQ.'SEQU'.AND.IHARG2(3).EQ.'ENCE')ILOCA=4
      IF(IHARG(3).EQ.'SEQU')ILOCA=4
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        START=ARG(ILOCA)
        NSTRT=1
        DSIZE(1)=START
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='V'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'NO')THEN
          ICOL=IVALUE(ILOC)
          NSTRT=IN(ILOC)
          ICNT=0
          DO 1311 II=1,NSTRT
            IJ=MAXN*(ICOL-1)+II
            ICNT=ICNT+1
            IF(ICOL.LE.MAXCOL)DSIZE(ICNT)=V(IJ)
            IF(ICOL.EQ.MAXCP1)DSIZE(ICNT)=PRED(IJ)
            IF(ICOL.EQ.MAXCP2)DSIZE(ICNT)=RES(IJ)
            IF(ICOL.EQ.MAXCP3)DSIZE(ICNT)=YPLOT(IJ)
            IF(ICOL.EQ.MAXCP4)DSIZE(ICNT)=XPLOT(IJ)
            IF(ICOL.EQ.MAXCP5)DSIZE(ICNT)=X2PLOT(IJ)
            IF(ICOL.EQ.MAXCP6)DSIZE(ICNT)=TAGPLO(IJ)
 1311     CONTINUE
          START=DSIZE(1)
        ELSE
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          START=VALUE(ILOC)
          DSIZE(1)=START
          NSTRT=1
        ENDIF
      ELSE
        GO TO 1370
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')THEN
        WRITE(ICOUT,1313)NSTRT,START,ICOL,ILOC
 1313   FORMAT('NSTRT,START,ICOL,ILOC=',I8,G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     CREATE THE "REPEAT" VARIABLE
!
      REPS=1.0
      NREP=1
      DSYMB(1)=REPS
!
      IF(NUMPAR.GT.3 .OR. NUMPAR.EQ.2)THEN
        ILOCA=ILOCA+1
        IF(IARGT(ILOCA).EQ.'NUMB')THEN
           REPS=ARG(ILOCA)
           DSYMB(1)=REPS
        ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
          IH=IHARG(ILOCA)
          IH2=IHARG2(ILOCA)
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'NO')THEN
            ICOL=IVALUE(ILOC)
            NREP=IN(ILOC)
            ICNT=0
            DO 1321 II=1,NREP
              IJ=MAXN*(ICOL-1)+II
              ICNT=ICNT+1
              IF(ICOL.LE.MAXCOL)DSYMB(ICNT)=V(IJ)
              IF(ICOL.EQ.MAXCP1)DSYMB(ICNT)=PRED(IJ)
              IF(ICOL.EQ.MAXCP2)DSYMB(ICNT)=RES(IJ)
              IF(ICOL.EQ.MAXCP3)DSYMB(ICNT)=YPLOT(IJ)
              IF(ICOL.EQ.MAXCP4)DSYMB(ICNT)=XPLOT(IJ)
              IF(ICOL.EQ.MAXCP5)DSYMB(ICNT)=X2PLOT(IJ)
              IF(ICOL.EQ.MAXCP6)DSYMB(ICNT)=TAGPLO(IJ)
 1321       CONTINUE
            REPS=DSYMB(1)
          ELSE
            IHWUSE='P'
            MESSAG='YES'
            CALL CHECKN(IH,IH2,IHWUSE,   &
                        IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                        NUMNAM,MAXNAM,   &
                        ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
            REPS=VALUE(ILOC)
            NREP=1
           DSYMB(1)=REPS
          ENDIF
        ELSE
          GO TO 1370
        ENDIF
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')THEN
        WRITE(ICOUT,1323)NREP,REPS,ICOL,ILOC
 1323   FORMAT('NREP,REPS,ICOL,ILOC=',I8,G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     CHECK FOR 2-PARAMETER CASE (I.E., VALUE AND REPEAT
!     FACTOR).
!
      IF(NUMPAR.EQ.2)THEN
        DO 1330 I=1,NSTRT
          DCOLOR(I)=0.0
          DFILL(I)=DSIZE(I)
 1330   CONTINUE
        NINC=NSTRT
        NSTOP=NSTRT
        GO TO 1390
      ENDIF
!
!     CREATE THE "INCREMENT" VARIABLE
!
      ILOCA=ILOCA+1
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        AINC=ARG(ILOCA)
        NINC=1
        DCOLOR(1)=AINC
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='V'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'NO')THEN
          ICOL=IVALUE(ILOC)
          NINC=IN(ILOC)
          ICNT=0
          DO 1331 II=1,NINC
            IJ=MAXN*(ICOL-1)+II
            ICNT=ICNT+1
            IF(ICOL.LE.MAXCOL)DCOLOR(ICNT)=V(IJ)
            IF(ICOL.EQ.MAXCP1)DCOLOR(ICNT)=PRED(IJ)
            IF(ICOL.EQ.MAXCP2)DCOLOR(ICNT)=RES(IJ)
            IF(ICOL.EQ.MAXCP3)DCOLOR(ICNT)=YPLOT(IJ)
            IF(ICOL.EQ.MAXCP4)DCOLOR(ICNT)=XPLOT(IJ)
            IF(ICOL.EQ.MAXCP5)DCOLOR(ICNT)=X2PLOT(IJ)
            IF(ICOL.EQ.MAXCP6)DCOLOR(ICNT)=TAGPLO(IJ)
 1331     CONTINUE
          AINC=DCOLOR(1)
        ELSE
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          AINC=VALUE(ILOC)
          NINC=1
          DCOLOR(1)=AINC
        ENDIF
      ELSE
        GO TO 1370
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')THEN
        WRITE(ICOUT,1333)NINC,AINC,ICOL,ILOC
 1333   FORMAT('NINC,AINC,ICOL,ILOC=',I8,G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     CREATE THE "STOP" VARIABLE
!
      ILOCA=ILOCA+1
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        STOP=ARG(ILOCA)
        NSTOP=1
        DFILL(1)=STOP
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='V'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'NO')THEN
          ICOL=IVALUE(ILOC)
          NSTOP=IN(ILOC)
          ICNT=0
          DO 1341 II=1,NSTOP
            IJ=MAXN*(ICOL-1)+II
            ICNT=ICNT+1
            IF(ICOL.LE.MAXCOL)DFILL(ICNT)=V(IJ)
            IF(ICOL.EQ.MAXCP1)DFILL(ICNT)=PRED(IJ)
            IF(ICOL.EQ.MAXCP2)DFILL(ICNT)=RES(IJ)
            IF(ICOL.EQ.MAXCP3)DFILL(ICNT)=YPLOT(IJ)
            IF(ICOL.EQ.MAXCP4)DFILL(ICNT)=XPLOT(IJ)
            IF(ICOL.EQ.MAXCP5)DFILL(ICNT)=X2PLOT(IJ)
            IF(ICOL.EQ.MAXCP6)DFILL(ICNT)=TAGPLO(IJ)
 1341     CONTINUE
          STOP=DFILL(1)
        ELSE
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          STOP=VALUE(ILOC)
          NSTOP=1
          DFILL(1)=STOP
        ENDIF
      ELSE
        GO TO 1370
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')THEN
        WRITE(ICOUT,1343)NSTOP,STOP,ICOL,ILOC
 1343   FORMAT('NSTOP,STOP,ICOL,ILOC=',I8,G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      GO TO 1390
!
 1370 CONTINUE
      WRITE(ICOUT,1371)
 1371 FORMAT('***** ERROR IN SEQUENCE COMMAND--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1372)
 1372 FORMAT('      AN ARGUMENT TYPE WHICH SHOULD BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1373)
 1373 FORMAT('      EITHER A NUMBER OR A WORD, IS NEITHER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1374)IHARG(ILOCA),IHARG2(ILOCA)
 1374 FORMAT('      ARGUMENT                  = ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1375)ILOCA
 1375 FORMAT('      LOCATION IN ARGUMENT LIST = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1376)IARGT(ILOCA)
 1376 FORMAT('      ARGUMENT TYPE             = ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1377)
 1377 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1220)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
!
 1390 CONTINUE
!
      NTEMP=MAX(NSTRT,NREP)
      NTEMP=MAX(NTEMP,NINC)
      NTEMP=MAX(NTEMP,NSTOP)
      IF(NTEMP.GT.1)THEN
        IF(NSTRT.GT.1 .AND. NSTRT.NE.NTEMP)THEN
          WRITE(ICOUT,1371)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1381)
 1381     FORMAT('      THE NUMBER OF VALUES IN THE START VARIABLE ',   &
                 'NOT WHAT WAS EXPECTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1382)NSTRT
 1382     FORMAT('      NUMBER OF VALUES FOUND    = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1383)NTEMP
 1383     FORMAT('      NUMBER OF VALUES EXPECTED = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(NREP.GT.1 .AND. NREP.NE.NTEMP)THEN
          WRITE(ICOUT,1371)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1386)
 1386     FORMAT('      THE NUMBER OF VALUES IN THE REPEAT VARIABLE ',   &
                 'NOT WHAT WAS EXPECTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1382)NREP
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1383)NTEMP
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(NINC.GT.1 .AND. NINC.NE.NTEMP)THEN
          WRITE(ICOUT,1371)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1387)
 1387     FORMAT('      THE NUMBER OF VALUES IN THE INCREMENT ',   &
                 'VARIABLE NOT WHAT WAS EXPECTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1382)NINC
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1383)NTEMP
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(NSTOP.GT.1 .AND. NSTOP.NE.NTEMP)THEN
          WRITE(ICOUT,1371)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1388)
 1388     FORMAT('      THE NUMBER OF VALUES IN THE STOP VARIABLE ',   &
                 'NOT WHAT WAS EXPECTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1382)NSTOP
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1383)NTEMP
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
!      NOTE 11/2010: NOW SET UP LOOP FOR SEQUENCE
!
      K=0
      DO 1499 KLOOP=1,NTEMP
!
        IF(NSTRT.GT.1)THEN
          START=DSIZE(KLOOP)
        ELSE
          START=DSIZE(1)
        ENDIF
        IF(NREP.GT.1)THEN
          REPS=DSYMB(KLOOP)
        ELSE
          REPS=DSYMB(1)
        ENDIF
        IF(NINC.GT.1)THEN
          AINC=DCOLOR(KLOOP)
        ELSE
          AINC=DCOLOR(1)
        ENDIF
        IF(NSTOP.GT.1)THEN
          STOP=DFILL(KLOOP)
        ELSE
          STOP=DFILL(1)
        ENDIF
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')THEN
          WRITE(ICOUT,1541)KLOOP,START,REPS,AINC,STOP
 1541     FORMAT('KLOOP,START,REPS,AINC,STOP=',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1543)NSTRT,NREP,NINC,NSTOP
 1543     FORMAT('NSTRT,NREP,NINC,NSTOP = ',4I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(START.EQ.STOP)AINC=0.0
        IF(START.LT.STOP.AND.AINC.LT.0.0)AINC=-AINC
        IF(START.GT.STOP.AND.AINC.GT.0.0)AINC=-AINC
        IFOUND='YES'
!
!               *********************************************************
!               **  STEP 6--                                           **
!               **  GENERATE    NRAWSE         NUMBERS                 **
!               **  IN   THE RAW SEQUENCE.                             **
!               **  STORE THEM TEMPORARILY IN                          **
!               **  THE VECTOR Y(.).                                   **
!               **  GENERATE THE VALUES FOR THE VARIABLE.              **
!               **  IT IS OF THE FORM--                                **
!               **  LET Z    = CONSTANT1  CONSTANT2  CONSTANT3         **
!               **  LET Z    = PARAMETER1  PARAMETER2  PARAMETER3      **
!               **  NOTE THAT COULD ALSO HAVE                          **
!               **  LET Z    = CONSTANT1  PARAMETER2  PARAMETER3       **
!               **  AND ALL OTHER SUCH MIXTURES.                       **
!               **  THIS IS THE IMPLICIT GENERATE COMMAND              **
!               **  WHICH GENERATES A VARIABLES STARTING WITH          **
!               **  THE VALUE CONSTANT1 AND INCREMENTING BY CONSTANT2  **
!               **  UNTIL IT ARIVES AT THE LAST VALUE NOT LARGER       **
!               ** (SMALLER) THAN CONSTANT3.                           **
!               **  THE OUTPUT IS NECESSARILY A VARIABLE.              **
!               *********************************************************
!
        ISTEPN='6'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       NOTE 11/2010: CURRENTLY, THE SYNTAX
!
!                        LET Y = SEQUENCE 1 1 1
!
!                      WILL GENERATE A COLUMN OF 1'S EQUAL
!                      TO THE MAXIMUM ROW SIZE.  WITH THE UPDATE
!                      TO ALLOW VARIABLES FOR THE ARGUMENTS, THIS
!                      COULD HAVE AN UNDESIRABLE EFFECT.  MODIFY
!                      SO THAT THIS SYNTAX GENERATES ONLY A SINGLE
!                      VALUE.
!
!CCCC   IF(AINC.EQ.0.0)N2=MAXN
        IF(AINC.EQ.0.0)THEN
          N2=1
        ELSEIF(AINC.NE.0.0)THEN
          N2=INT(((STOP-START)/AINC))
          IF(N2.LT.0)N2=-N2
          N2=N2+10
          IF(N2.GT.MAXN)N2=MAXN
        ENDIF
!
        IREP=1
        IF(REPS.LE.1.0)IREP=1
        IF(REPS.GT.1.0)IREP=INT(REPS+0.5)
!
        DO 1400 I=1,N2
          AI=I
          YCALC=START+(AI-1.0)*AINC
          DO 1410 J=1,IREP
            K=K+1
            IF(K.GT.MAXN)THEN
              NRAWSE=K-1
              K=K-1
              GO TO 1490
            ENDIF
            Y(K)=YCALC
 1410     CONTINUE
          IF(I.EQ.1)GO TO 1400
          IF(AINC.EQ.0.0)GO TO 1400
          IF((START.LT.STOP.AND.YCALC.GT.STOP) .OR.   &
             (START.GT.STOP.AND.YCALC.LT.STOP))THEN
             NRAWSE=K-IREP
             K=K-IREP
             GO TO 1490
          ENDIF
 1400   CONTINUE
        NRAWSE=K
!
 1490   CONTINUE
!
!               ******************************************************
!               **  STEP 7--                                        **
!               **  IF CALLED FOR (THAT IS, IF IBUGA3 IS ON),       **
!               **  PRINT OUT THE INTERMEDIATE VARIABLE Y(.).       **
!               **  THIS IS USEFUL FOR DIAGNOSTIC PURPOSES          **
!               **  IN REVIEWING THE OUTPUT FROM THIS SUBROUTINE.   **
!               ******************************************************
!
        ISTEPN='7'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,1551)
 1551     FORMAT('OUTPUT FROM MIDDLE OF DPSEQU AFTER THE RAW SEQUENCE ',   &
                 'HAS BEEN GENERATED--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1552)KLOOP,NRAWSE
 1552     FORMAT('KLOOP,NRAWSE = ',2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NRAWSE.GT.0)THEN
            DO 1554 I=1,NRAWSE
              WRITE(ICOUT,1555)I,Y(I)
 1555         FORMAT('I,Y(I) = ',I8,F12.5)
              CALL DPWRST('XXX','BUG ')
 1554       CONTINUE
          ENDIF
        ENDIF
!
 1499 CONTINUE
!
!               *****************************************
!               **  STEP 8--                           **
!               **  CHECK TO SEE THE TYPE SUBCASE      **
!               **  (BASED ON THE QUALIFIER)           **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 1670
      DO 1610 J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GO TO 1620
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GO TO 1620
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GO TO 1630
 1610 CONTINUE
      GO TO 1680
!
 1620 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GO TO 1680
!
 1630 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 1680
!
 1670 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1371)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1672)
 1672 FORMAT('      AT BRANCH POINT 1671--NUMARG LESS THAN 1 EVEN')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1674)
 1674 FORMAT('      THOUGH NUMARG HAD PREVIOUSLY PASSED THIS TEST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1675)NUMARG
 1675 FORMAT('      ONCE ALREADY.  VALUE OF NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1676)
 1676 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1220)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
!
 1680 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SEQU')THEN
        WRITE(ICOUT,1681)NUMARG,ILOCQ,ICASEQ
 1681   FORMAT('NUMARG,ILOCQ,ICASEQ = ',I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 9--                                        **
!               **  BRANCH TO THE APPROPRIATE SUBCASE               **
!               **  (BASED ON THE QUALIFIER);                       **
!               **  DETERMINE THE NUMBER (= NNUM)                   **
!               **  OF        NUMBERS TO BE GENERATED.              **
!               **  NOTE THAT THE VARIABLE NIISUB                   **
!               **  IS THE LENGTH OF THE RESULTING                  **
!               **  VARIABLE ISUB(.).                               **
!               **  NOTE THAT DPFOR AUTOMATICALLY EXTENDS           **
!               **  THE INPUT LENGTH OF ISUB(.) IF NECESSARY.       **
!               **  (HENCE THE REDEFINITION OF NIISUB TO NINEW      **
!               **  AFTER THE CALL TO DPFOR.                        **
!               ******************************************************
!
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL')GO TO 1710
      IF(ICASEQ.EQ.'SUBS')GO TO 1720
      IF(ICASEQ.EQ.'FOR')GO TO 1730
!
 1710 CONTINUE
!CCCC IF(NEWNAM.EQ.'NO')NIISUB=NLEFT
!CCCC IF(NEWNAM.EQ.'YES')NIISUB=NRAWSE
      NIISUB=NRAWSE
      DO 1715 I=1,NIISUB
      ISUB(I)=1
 1715 CONTINUE
      NS=NIISUB
      NNUM=NIISUB
      GO TO 1750
!
 1720 CONTINUE
      NIISUB=MAXN
      CALL DPSUBS(NIISUB,ILOCS,NS,IBUGQ,IERROR)
      NNUM=NS
      GO TO 1750
!
 1730 CONTINUE
      IF(NEWNAM.EQ.'NO')NIISUB=NLEFT
      IF(NEWNAM.EQ.'YES')NIISUB=MAXN
      CALL DPFOR(NIISUB,NINEW,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NIISUB=NINEW
      NNUM=NS
      GO TO 1750
!
 1750 CONTINUE
!
!               ******************************************************
!               **  STEP 10--                                       **
!               **  COPY THE        SEQUENCE                        **
!               **  FROM THE INTERMEDIATE VECTOR Y(.)               **
!               **  TO THE APPROPRIATE COLUMN                       **
!               **  (BASED ON THE QUALIFIER--FULL, SUBSET, OR FOR)  **
!               **  IN THE INTERNAL DATAPLOT DATA TABLE.            **
!               ******************************************************
!
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NS2=0
      NS2MOD=0
      DO 2100 I=1,NIISUB
      IJ=MAXN*(ICOLL-1)+I
      IF(ISUB(I).EQ.0)GO TO 2100
      NS2=NS2+1
      NS2MOD=NS2MOD+1
      IF(NS2.EQ.1)IROW1=I
      IF(NS2MOD.GT.NRAWSE)NS2MOD=NS2MOD-NRAWSE
      IF(ICOLL.LE.MAXCOL)V(IJ)=Y(NS2MOD)
      IF(ICOLL.EQ.MAXCP1)PRED(I)=Y(NS2MOD)
      IF(ICOLL.EQ.MAXCP2)RES(I)=Y(NS2MOD)
      IF(ICOLL.EQ.MAXCP3)YPLOT(I)=Y(NS2MOD)
      IF(ICOLL.EQ.MAXCP4)XPLOT(I)=Y(NS2MOD)
      IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=Y(NS2MOD)
      IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=Y(NS2MOD)
      IROWN=I
 2100 CONTINUE
      NNUM=NS2
!
!               *******************************************
!               **  STEP 11--                            **
!               **  CARRY OUT THE LIST UPDATING AND      **
!               **  GENERATE THE INFORMATIVE PRINTING.   **
!               *******************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SEQU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.GE.NRAWSE)NINEW=NLEFT
      IF(ICASEQ.EQ.'FULL'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.LT.NRAWSE)NINEW=NRAWSE
      IF(ICASEQ.EQ.'FULL'.AND.NEWNAM.EQ.'YES')NINEW=NIISUB
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.GE.IROWN)NINEW=NLEFT
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.LT.IROWN)NINEW=IROWN
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'YES')NINEW=IROWN
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.GE.IROWN)NINEW=NLEFT
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.LT.IROWN)NINEW=IROWN
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'YES')NINEW=IROWN
!
      IHNAME(ILISTL)=ILEFT
      IHNAM2(ILISTL)=ILEFT2
      IUSE(ILISTL)='V'
      IVALUE(ILISTL)=ICOLL
      VALUE(ILISTL)=ICOLL
      IN(ILISTL)=NINEW
!
!CCCC IUSE(ICOLL)='V'
!CCCC IVALUE(ICOLL)=ICOLL
!CCCC VALUE(ICOLL)=ICOLL
!CCCC IN(ICOLL)=NINEW
!
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWNAM.EQ.'YES')NUMCOL=NUMCOL+1
!CCCC THE FOLLOWING LINE WAS INSERTED (DECEMBER 1988)
!CCCC TO FIX A PARAMETER TO VARIABLE COLUMN BUG (DECEMBER 1988)
      IF(NEWNAM.EQ.'NO'.AND.IPTOV.EQ.'YES')NUMCOL=NUMCOL+1
!
      DO 2400 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)GO TO 2405
      GO TO 2400
 2405 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL
      VALUE(J4)=ICOLL
      IN(J4)=NINEW
 2400 CONTINUE
!
      IF(IPRINT.EQ.'OFF')GO TO 2459
      IF(IFEEDB.EQ.'OFF')GO TO 2459
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2411)ILEFT,ILEFT2,NNUM
 2411 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
!
      IJ=MAXN*(ICOLL-1)+IROW1
      IF(ICOLL.LE.MAXCOL)THEN
         WRITE(ICOUT,2421)ILEFT,ILEFT2,V(IJ),IROW1
 2421    FORMAT('THE FIRST           COMPUTED VALUE OF ',   &
         A4,A4,' = ',E15.7,'   (ROW ',I9,')')
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP1)THEN
         WRITE(ICOUT,2421)ILEFT,ILEFT2,PRED(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP2)THEN
         WRITE(ICOUT,2421)ILEFT,ILEFT2,RES(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP3)THEN
         WRITE(ICOUT,2421)ILEFT,ILEFT2,YPLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP4)THEN
         WRITE(ICOUT,2421)ILEFT,ILEFT2,XPLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP5)THEN
         WRITE(ICOUT,2421)ILEFT,ILEFT2,X2PLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP6)THEN
         WRITE(ICOUT,2421)ILEFT,ILEFT2,TAGPLO(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IJ=MAXN*(ICOLL-1)+IROWN
      IF(NNUM.NE.1)THEN
         IF(ICOLL.LE.MAXCOL)THEN
            WRITE(ICOUT,2431)NNUM,ILEFT,ILEFT2,V(IJ),IROWN
 2431       FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',   &
            A4,A4,' = ',E15.7,'   (ROW ',I9,')')
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP1)THEN
            WRITE(ICOUT,2431)NNUM,ILEFT,ILEFT2,PRED(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP2)THEN
            WRITE(ICOUT,2431)NNUM,ILEFT,ILEFT2,RES(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP3)THEN
            WRITE(ICOUT,2431)NNUM,ILEFT,ILEFT2,YPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP4)THEN
            WRITE(ICOUT,2431)NNUM,ILEFT,ILEFT2,XPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP5)THEN
            WRITE(ICOUT,2431)NNUM,ILEFT,ILEFT2,X2PLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP6)THEN
            WRITE(ICOUT,2431)NNUM,ILEFT,ILEFT2,TAGPLO(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ENDIF
      ENDIF
!
      IF(NNUM.NE.1)GO TO 2449
      WRITE(ICOUT,2441)
 2441 FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2442)
 2442 FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
      CALL DPWRST('XXX','BUG ')
 2449 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2451)ILEFT,ILEFT2,ICOLL
 2451 FORMAT('THE CURRENT COLUMN FOR ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2453)ILEFT,ILEFT2,NINEW
 2453 FORMAT('THE CURRENT LENGTH OF  ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 2459 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SEQU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSEQU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)MAXN,N2,NRAWSE,NS2,NS2MOD,NNUM
 9015   FORMAT('MAXN,N2,NRAWSE,NS2,NS2MOD,NNUM = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)NS,NIISUB,NNUM
 9016   FORMAT('NS,NIISUB,NNUM = ',I8,I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)START,REPS,AINC,STOP
 9017   FORMAT('START,REPS,AINC,STOP = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)NLEFT,NRAWSE,NIISUB,IROW1,IROWN,NINEW
 9018   FORMAT('NLEFT,NRAWSE,NIISUB,IROW1,IROWN,NINEW = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)ILEFT,ILEFT2,NEWNAM,ICOLL,NINEW
 9019   FORMAT('ILEFT,ILEFT2,NEWNAM,ICOLL,NINEW = ',A4,A4,2X,A4,I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)REPS,IREP
 9021   FORMAT('REPS,IREP = ',E15.7,I8)
        CALL DPWRST('XXX','BUG ')
!CCCC   THE FOLLOWING 2 LINES WERE INSERTED (DECEMBER 1988)
!CCCC   TO FIX A PARAMETER TO VARIABLE COLUMN BUG (DECEMBER 1988)
        WRITE(ICOUT,9022)IPTOV,NUMCOL
 9022   FORMAT('IPTOV,NUMCOL = ',A4,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSEQU
      SUBROUTINE DPSERI(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,   &
                        IMACRO,IMACNU,IMACCS,IOSW,IMALEV,           &
                        IREARW,ICOMCH,ICOMSW,                       &
                        NUMRCM,                                     &
                        IFCOLL,IFCOLU,                              &
                        IANSLO,ILOOST,ILOOLI,IREPCH,                &
                        IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
!CCCC ICOMCH, ICOMSW ADDED TO ARGUMENT LIST MAY, 1990.
!
!     PURPOSE--READ IN THE VALUES OF A VARIABLE.
!              THE DATA IS LISTED SERIALLY ACROSS
!              A LINE IMAGE
!              (E.G., X(1) X(2) X(3) ETC.)
!              THE DATA IS READ FROM A MASS STORAGE FILE
!              OR (IF NO FILE GIVEN) FROM THE DEFAULT INPUT UNIT
!              (WHICH WILL BE THE TERMINAL).
!     ASSUMPTION--THE INPUT  FILE ALREADY EXISTS;
!                 (THAT IS, DATAPLOT WILL AUTOMATICALLY
!                 OPEN THE FILE
!                 VIA (ON THE UNIVAC 1108), BY AN @ASG,AX ...)
!                 BUT WILL NOT AUTOMATICALLY CREATE THE FILE
!                 VIA (ON THE UNIVAC 1108), BY AN @ASG,UP ...))
!     ASSUMPTION--THE COMPUTER SYSTEM IS SUCH THAT
!                 EQUATING THE FILE NAME TO
!                 THE FORTRAN NUMERIC DESIGNATION
!                 OF 31 (OR HOWEVER THE VARIABLE    IREANU    IS DEFINED
!                 IN INITFO) IS PERMISSIBLE.
!     NOTE--INPUT FOR THE READ COMMAND MAY POTENTIALLY
!           COME FROM 2 DIFFERENT SOURCES--
!                1) THE TERMINAL ITSELF;
!                2) A FILE;
!           DIFFERENT SYSTEMS ALLOW DIFFERENT COMBINATIONS
!           OF THE ABOVE.
!           ALL SYSTEMS WILL ALLOW INPUT FROM THER TERMINAL ITSELF;
!           MOST SYSTEMS WILL ALLOW INPUT FROM A FILE;
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/1
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --JUNE      1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1985.
!     UPDATED         --FEBRUARY  1988.    (DEACT. COL. LIM. IF READ NON-FILE)
!     UPDATED         --DECEMBER  1988.    CORRECT BOMB ON 2ND   READ PARAMETER
!     UPDATED         --MAY       1989.    FIX IRIS PROBLEM--LOOP MAX & CPUMAX
!     UPDATED         --MAY       1990.    1) CHECK FOR COMMENT CHARACTER
!                                          2) ERROR CHECK FOR FORMATTED READ
!     UPDATED         --JULY      1990.    ICOMFL RENAMED AS ICOMSW
!     UPDATED         --SEPTEMBER 1995.  ROW LIMITS & BLANK LINES PROBLEM
!     UPDATED         --FEBRUARY  2003.  SUPPORT FOR LONGER DATA LINES
!     UPDATED         --DECEMBER  2004.  DO NOT ALLOW TERMINAL READ
!                                        WHILE RUNNING THE GUI.
!     UPDATED         --APRIL     2005.  ARGUMENT LIST TO DPREAL
!     UPDATED         --AUGUST    2008.  ISSUE WITH LINUX GFORTRAN
!                                        COMPILER
!     UPDATED         --APRIL     2009.  CALL LIST TO DPREAL
!     UPDATED         --JULY      2009.  ALLOW "Y1 TO Y1" (USEFUL FOR
!                                        MACROS WHERE NUMBER OF VARIABLES
!                                        UNKNOWN IN ADVANCE)
!     UPDATED         --APRIL     2010.  CALL LIST TO DPREAL
!     UPDATED         --JANUARY   2015.  WHEN DO SERIAL READ FROM WITHIN
!                                        LOOP, NEED TO EXTRACT DATA
!                                        LINES FROM SAVED LOOP LINES
!                                        RATHER THAN STANDARD INPUT
!     UPDATED         --JANUARY   2015.  CALL LIST TO DPREAL
!     UPDATED         --JUNE      2016.  CALL LIST TO DPREAL
!     UPDATED         --APRIL     2017.  CALL LIST TO DPREAL
!     UPDATED         --SEPTEMBER 2019.  CALL LIST TO DPREAL
!     UPDATED         --FEBRUARY  2025. CALL LIST TO DPREAL
!                                       1. ADD IREALT
!                                       2. ADD XTAG, IOUNI5
!                                          (OPEN dpst5f.dat IF NEEDED)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
      CHARACTER*4 ILOOST
      CHARACTER*1 IREPCH
!
      CHARACTER*4 IOSW
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZC.INC'
!
      CHARACTER*4 ICASEQ
      CHARACTER*4 ICASEA
      CHARACTER*4 IECASE
      CHARACTER*4 ISTOR1
      CHARACTER*4 ISTOR2
      CHARACTER*4 ISTOR3
      CHARACTER*4 IEND
      CHARACTER*4 JVNAM1
      CHARACTER*4 JPNAM1
      CHARACTER*4 JMNAM1
      CHARACTER*4 JFNAM1
      CHARACTER*4 JUNAM1
      CHARACTER*4 JENAM1
      CHARACTER*4 JVNAM2
      CHARACTER*4 JPNAM2
      CHARACTER*4 JMNAM2
      CHARACTER*4 JFNAM2
      CHARACTER*4 JUNAM2
      CHARACTER*4 JENAM2
      CHARACTER*4 IH1
      CHARACTER*4 IH2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 IOFILE
      CHARACTER*4 IOTERM
      CHARACTER*4 IOP
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
      CHARACTER*4 ICASRE
      CHARACTER*4 ICASTO
      CHARACTER*4 IREARW
!  FOLLOWING 3 LINES MAY, 1990.
!CCCC CHARACTER*80 IAJUNK
      CHARACTER*4 ICOMCH
      CHARACTER*4 ICOMSW
!CCCC THE FOLLOWING LINE WAS ADDED    SEPTEMBER 1995
      CHARACTER*4 LINETY
!
      CHARACTER*4 IB
!
      INTEGER IFCOLL(*)
      INTEGER IFCOLU(*)
!
      INTEGER I
      INTEGER ICOLVJ
      INTEGER ID
      INTEGER IJ
      INTEGER IE
      INTEGER IE2
      INTEGER IE3
      INTEGER IENDTY
      INTEGER IFRMIN
      INTEGER IFROW
      INTEGER IFRMAX
      INTEGER IFCOL1
      INTEGER IFCOL2
      INTEGER IFCOL3
      INTEGER IFCOL4
      INTEGER NUMV
      INTEGER NUMP
      INTEGER NUMM
      INTEGER NUMF
      INTEGER NUMU
      INTEGER NUME
      INTEGER NUMLRD
      INTEGER NUMVRD
!
      PARAMETER (MAXRDV=1024)
      PARAMETER (MAXCHV=20)
!
      DIMENSION JVNAM1(MAXRDV)
      DIMENSION JPNAM1(MAXRDV)
      DIMENSION JMNAM1(MAXRDV)
      DIMENSION JFNAM1(MAXRDV)
      DIMENSION JUNAM1(MAXRDV)
      DIMENSION JENAM1(MAXRDV)
!
!CCCC DIMENSION NIV(MAXRDV)
!
      DIMENSION JVNAM2(MAXRDV)
      DIMENSION JPNAM2(MAXRDV)
      DIMENSION JMNAM2(MAXRDV)
      DIMENSION JFNAM2(MAXRDV)
      DIMENSION JUNAM2(MAXRDV)
      DIMENSION JENAM2(MAXRDV)
!
      DIMENSION IEN(MAXRDV)
      DIMENSION IECOL2(MAXRDV)
      DIMENSION IECASE(MAXRDV)
      DIMENSION PVAL(MAXRDV)
      DIMENSION IFSTA2(MAXRDV)
      DIMENSION IFSTO2(MAXRDV)
!
      DIMENSION X0(MAXRDV)
      DIMENSION XTAG(MAXRDV)
      CHARACTER*24 IXC(MAXCHV)
      INTEGER ITYPE(MAXRDV)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
      DIMENSION ISTOR1(MAXRCL)
      DIMENSION ISTOR2(MAXRCL)
      DIMENSION ISTOR3(MAXRCL)
      DIMENSION IB(MAXRCL)
!
      CHARACTER*4 IANSLO(MAXLIL,MAXCIL)
!
!CCCC CHARACTER*255 ICANS
      CHARACTER (LEN=MAXSTR) :: ICANS
!CCCC CHARACTER*80 ISTRIN
!
      EQUIVALENCE (GARBAG(IGARB1),X0(1))
      EQUIVALENCE (GARBAG(IGARB1+MAXRDV),XTAG(1))
!
      EQUIVALENCE (IGARBG(IIGAR1),ITYPE(1))
      EQUIVALENCE (IGARBG(IIGAR1+3000),IEN(1))
      EQUIVALENCE (IGARBG(IIGAR1+5000),IECOL2(1))
      EQUIVALENCE (IGARBG(IIGAR1+7000),IFSTA2(1))
      EQUIVALENCE (IGARBG(IIGAR1+9000),IFSTO2(1))
!
      EQUIVALENCE (CGARBG(1),IECASE(1))
      EQUIVALENCE (CGARBG(120000),JVNAM1(1))
      EQUIVALENCE (CGARBG(130000),JPNAM1(1))
      EQUIVALENCE (CGARBG(140000),JMNAM1(1))
      EQUIVALENCE (CGARBG(150000),JFNAM1(1))
      EQUIVALENCE (CGARBG(160000),JUNAM1(1))
      EQUIVALENCE (CGARBG(170000),JENAM1(1))
      EQUIVALENCE (CGARBG(180000),JVNAM2(1))
      EQUIVALENCE (CGARBG(190000),JPNAM2(1))
      EQUIVALENCE (CGARBG(200000),JMNAM2(1))
      EQUIVALENCE (CGARBG(210000),JFNAM2(1))
      EQUIVALENCE (CGARBG(220000),JUNAM2(1))
      EQUIVALENCE (CGARBG(230000),JENAM2(1))
      EQUIVALENCE (CGARBG(240000),ISTOR1(1))
      EQUIVALENCE (CGARBG(300000),ISTOR2(1))
      EQUIVALENCE (CGARBG(360000),ISTOR3(1))
      EQUIVALENCE (CGARBG(420000),IB(1))
      EQUIVALENCE (CGARBG(600000),IXC(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSE'
      ISUBN2='RI  '
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
      IOFILE='-999'
      IOTERM='-999'
!
!CCCC THE FOLLOWING LINE WAS INSERTED MAY 1989
      IBILLI=INT(10.0**9 + 0.01)
!
      ICASRE='VARI'
      MAXN2=MAXCHF
!
      I2=0
      NUMVRD=0
      NCALL=0
      NCOLS=0
      JM1=0
      JP1=0
!
      IF(IREAPM.EQ.'ON' .OR. IREALT.EQ.'ON')THEN
        IOP='OPEN'
        IFLG11=0
        IFLG21=0
        IFLG31=0
        IFLAG4=0
        IFLAG5=1
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGS2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               ***************************
!               **  TREAT THE READ CASE  **
!               ***************************
!
      MAXV2=100
      MAXP2=100
      MAXM2=100
      MAXF2=100
      MAXU2=100
      MAXE2=100
!CCCC THE FOLLOWING LINE WAS ADDED    SEPTEMBER 1995
      LINETY='-999'
!
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSERI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IFROW1,IFROW2
   52   FORMAT('IFROW1,IFROW2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IFCOL1,IFCOL2,NUMRCM
   53   FORMAT('IFCOL1,IFCOL2,NUMRCM = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)ISKIP,INTINF,IBUGS2,IBUGQ
   54   FORMAT('ISKIP,INTINF,IBUGS2,IBUGQ = ',I8,I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IMACRO,IMACNU,IMACCS
   56   FORMAT('IMACRO,IMACNU,IMACCS = ',A4,I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IOSW,IRD,IRD2,IWIDTH,IREANU
   58   FORMAT('IOSW,IRD,IRD2,IWIDTH,IREANU = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IBUGS2,ISUBRO,IERROR
   63   FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,65)(IANSLC(I),I=1,IWIDTH)
   65     FORMAT('(IANSLC(I),I=1,IWIDTH) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,72)IREANA
   72   FORMAT('IREANA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)IREAST,IREAFO,IREAAC,IREAFO,IREACS,IREARW
   73   FORMAT('IREAST,IREAFO,IREAAC,IREAFO,IREACS,IREARW = ',   &
               5(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **  STEP 1--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LT.1)THEN
        IERROR='YES'
        GO TO 8800
      ENDIF
!
!               *******************************************************
!               **  STEP 2A--                                        **
!               **  DETERMINE THE TYPE OF READ CASE--                **
!               **       1) FROM TERMINAL;                           **
!               **       2) FROM FILE;                               **
!               **  NOTE--IOTERM  WILL = 'YES' ONLY IN EXPLICIT      **
!               **        TERMINAL CASE.                             **
!               **        (THAT IS, ONLY WHEN INPUT IOSW             **
!               **        = 'TERM')                                  **
!               **  NOTE--IOFILE  WILL = 'YES' ONLY IN FILE CASE.    **
!               *******************************************************
!
      ISTEPN='2A'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWORD=3
      CALL DPFILE(IANSLC,IWIDTH,IWORD,   &
      IOFILE,IBUGS2,ISUBRO,IERROR)
!
      IOTERM='NO'
      IF(IOFILE.EQ.'NO'.AND.IOSW.EQ.'TERM')IOTERM='YES'
      IF(IOTERM.EQ.'YES' .AND. ILOOST.EQ.'EXEC')IOTERM='LOOP'
!
!CCCC DECEMBER 2004.  IF GUI RUNNING, DO NOT ALLOW TERMINAL READ.
!
      IF(IOFILE.EQ.'NO' .AND. IGUIFL.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
  211   FORMAT('***** ERROR FROM READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)
  213   FORMAT('      TERMINAL READS (I.E., READ WITH NO FILE NAME ',   &
               'SPECIFIED)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,215)
  215   FORMAT('      ARE NOT PERMITTED WHEN RUNNING DATAPLOT FROM ',   &
               'THE GRAPHICAL USER INTERFACE)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,217)
  217   FORMAT('      ALTERNATIVELY, YOU CAN DO ONE OF THE FOLLOWING:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,219)
  219   FORMAT('      1) YOU CAN ENTER THE DATA DIRECTLY FROM THE ',   &
               'DATASHEET.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,221)
  221   FORMAT('      2) FROM THE COMMAND LINE WINDOW, YOU CAN USE ',   &
               'THE DATA COMMAND AS FOLLOWS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,223)
  223   FORMAT('         LET Y = DATA value1 value2 ...')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,225)
  225   FORMAT('      3) THE FIRST TWO METHODS ARE USEFUL FOR SMALL ',   &
               'AMOUNTS OF DATA.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,227)
  227   FORMAT('         FOR MORE THAN A FEW DATA POINTS, IT IS ',   &
               'RECOMMENDED THAT YOU')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,229)
  229   FORMAT('         CREATE THE DATA IN AN ASCII FILE AND THEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,231)
  231   FORMAT('         READ THE DATA FROM THAT FILE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *************************************
!               **  STEP 2B--                      **
!               **  IF HAVE THE FILE INPUT CASE--  **
!               **  COPY OVER VARIABLES            **
!               *************************************
!
      ISTEPN='2B'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'NO')GO TO 1190
!
      IOUNIT=IREANU
      IFILE=IREANA
      ISTAT=IREAST
      IFORM=IREAFO
      IACCES=IREAAC
      IPROT=IREAPR
      ICURST=IREACS
!
      ISUBN0='SERI'
      IERRFI='NO'
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        WRITE(ICOUT,1183)IOUNIT
 1183   FORMAT('IOUNIT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1184)IFILE
 1184   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1185)ISTAT,IFORM,IACCES,IPROT,ICURST
 1185   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',   &
               A12,2X,A12,2X,A12,2X,A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1186)ISUBN0,IERRFI
 1186   FORMAT('ISUBN0,IERRFI = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
 1190 CONTINUE
!
!               ***********************************************
!               **  STEP 2C--                                **
!               **  IF HAVE THE FILE INPUT CASE--            **
!               **  CHECK TO SEE IF THE READ FILE MAY EXIST  **
!               ***********************************************
!
      ISTEPN='2C'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'YES' .AND. ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** IMPLEMENTATION ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      THE DESIRED READING CANNOT BE CARRIED OUT ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)
 1214   FORMAT('      BECAUSE THE INTERNAL VARIABLE    IREAST ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1215)
 1215   FORMAT('      WHICH ALLOWS SUCH READING HAS BEEN SET TO ',   &
               '   NONE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)ISTAT,IREAST
 1217   FORMAT('ISTAT,IREAST = ',A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1218)
 1218   FORMAT('      ALL READING MUST BE DONE DIRECTLY FROM THE ',   &
               'TERMINAL.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               *************************************
!               **  STEP 2D--                      **
!               **  IF HAVE THE FILE INPUT CASE--  **
!               **  EXTRACT THE FILE NAME          **
!               *************************************
!
      ISTEPN='2D'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'YES')THEN
!
        DO 1310 I=1,MAXSTR
          ICANS(I:I)=IANSLC(I)(1:1)
 1310   CONTINUE
!
        ISTART=1
        ISTOP=IWIDTH
        IWORD=3
        CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,   &
                    ICOL1,ICOL2,IFILE,NCFILE,   &
                    IBUGS2,ISUBRO,IERROR)
!
        IF(NCFILE.LT.1)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1341)
 1341     FORMAT('***** ERROR IN SERIAL READ--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1342)
 1342     FORMAT('      A USER FILE NAME IS REQUIRED IN THE ',   &
                 'READ COMMAND')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1344)
 1344     FORMAT('      (FOR EXAMPLE,    READ CALIB.DAT X Y Z)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1345)
 1345     FORMAT('      BUT NONE WAS GIVEN HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1346)
 1346     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1347)(IANSLC(I),I=1,IWIDTH)
 1347       FORMAT('      ',80A1)
          ELSE
            WRITE(ICOUT,999)
          ENDIF
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
      ENDIF
!
!               *************************************
!               **  STEP 2E--                      **
!               **  IF HAVE THE FILE INPUT CASE--  **
!               **  OPEN THE FILE                  **
!               *************************************
!
      ISTEPN='2E'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'YES')THEN
!
        IREWIN='ON'
        IF(IREACS(1:4).EQ.'CLOS')THEN
          CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                      IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IREACS='OPEN'
        ENDIF
        IF(IERRFI.EQ.'YES')GO TO 9000
      ENDIF
!
!               ******************************************
!               **  STEP 2F--                           **
!               **  FOR THE 2 CASES--                   **
!               **      1) TERMINAL INPUT;              **
!               **      2) FILE INPUT;                  **
!               **  DEFINE THE INPUT READ UNIT NUMBER,  **
!               **  AND OTHER VARIABLES NEEDED          **
!               **  FOR UPCOMING READS.                 **
!               ******************************************
!
      ISTEPN='2F'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IRD2=IRD
!CCCC IF(IMACST.EQ.'OPFI')IRD2=IMACNU
!CCCC MARCH 1996.  BUG IF READ DONE WITHIN A MACRO AFTER A NESTED MACRO
!CCCC CALLED.
!CCCC IF(IMACCS.EQ.'OPEN')IRD2=IMACNU
      IF(IMACCS.EQ.'OPEN'.OR.IMALEV.GE.1)THEN
        IRD2=IMACNU+IMALEV-1
      ENDIF
      IF(IOFILE.EQ.'YES')IRD2=IREANU
      IF(IOTERM.EQ.'YES')IRD2=IRD
!
      IOUNIT=IRD2
      IDEV='SERI'
!
!               *****************************************
!               **  STEP 3--                           **
!               **  CHECK TO SEE THE TYPE CASE--       **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET; OR                    **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 390
      DO 300 J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GO TO 310
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GO TO 310
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GO TO 320
  300 CONTINUE
      GO TO 390
  310 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GO TO 390
  320 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 390
  390 CONTINUE
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        WRITE(ICOUT,391)NUMARG,ILOCQ
  391   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  DETERMINE THE TYPE AND NUMBER OF ITEMS           **
!               **  TO BE READ   .                                   **
!               **  NUMALL = TOTAL NUMBER OF READ  ITEMS             **
!               **           (AS DETERMINED BY INCLUDING ONLY ALL    **
!               **           BEFORE 'SUBSET' OR 'EXCEPT' OR 'FOR')   **
!               **  NUMV   = NUMBER OF VARIABLES TO BE READ    ;     **
!               **  NUMP   = NUMBER OF PARAMETERS TO BE READ    ;    **
!               **  NUMM   = NUMBER OF MODELS TO BE READ             **
!               **           (SHOULD = 0 OR 1)                       **
!               **  NUMF   = NUMBER OF FUNCTIONS TO BE READ          **
!               **  NUMU   = NUMBER OF UNKNOWNS TO BE READ    ;      **
!               **  NUME   = TOTAL NUMBER OF READ  ITEMS             **
!               **           (SHOULD = NUMALL);                      **
!               *******************************************************
!
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMALL=ILOCQ-1
      IF(IOFILE.EQ.'YES')NUMALL=ILOCQ-2
!
      IV=0
      IP=0
      IM=0
      IF=0
      IU=0
      IE=0
      IH1=' '
      IH2=' '
      JMIN=2
      IF(IOFILE.EQ.'YES')JMIN=3
      JMAX=ILOCQ-1
      IF(JMIN.GT.JMAX)GO TO 4290
!
      IISKIP=0
!
      DO 4200 J=JMIN,JMAX
!
        IF(IISKIP.EQ.1)THEN
          IISKIP=0
          GO TO 4200
        ENDIF
!
        IH1=IHARG(J)
        IH2=IHARG2(J)
!
!       ***************
!       THE FOLLOWING CODE ALLOWS THE    TO    KEYWORD
!       TO BE ACTIVATED, AS IN
!       SERIAL READ FILE.EXT Y1 TO Y10
!       DECEMBER 1986
!       ***************
!
        ICASTO='OFF'
        IF (IH1.EQ.'TO  ')GO TO 4210
        GO TO 4220
!
 4210   CONTINUE
        ICASTO='ON'
        JM1=J-1
        JP1=J+1
        CALL DPEXTL(IHARG(JM1),IHARG2(JM1),IHARG(JP1),IHARG2(JP1),   &
                    KNUMB,IVAL1,IVAL2,IBUGS2,ISUBRO,IERROR)
!
        IF(IVAL1.EQ.IVAL2)THEN
          IISKIP=1
          GO TO 4200
        ENDIF
!
        IVA1P1=IVAL1+1
        IVA2M1=IVAL2-1
        IF(IVA1P1.GT.IVA2M1)GO TO 4200
        IVAL=IVAL1
 4215   CONTINUE
        IVAL=IVAL+1
        IF(IVAL.GE.IVAL2)GO TO 4200
!
        CALL DPAPNU(IHARG(JM1),IHARG2(JM1),KNUMB,IVAL,   &
                    IH1,IH2,IBUGS2,ISUBRO,IERROR)
        GO TO 4220
!
 4220   CONTINUE
        ICASEA='    '
        DO 4300 I=1,NUMNAM
          I2=I
          IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))GO TO 4305
          GO TO 4300
 4305   CONTINUE
        IF(IUSE(I).EQ.'V')GO TO 4310
        IF(IUSE(I).EQ.'P')GO TO 4320
        IF(IUSE(I).EQ.'M')GO TO 4330
        IF(IUSE(I).EQ.'F')GO TO 4340
 4300   CONTINUE
        ICASEA='U'
        GO TO 4350
!
 4310   CONTINUE
        ICASEA='V'
        IV=IV+1
        IF(IV.GT.MAXV2)GO TO 4370
        JVNAM1(IV)=IH1
        JVNAM2(IV)=IH2
        GO TO 4370
!
 4320   CONTINUE
        ICASEA='P'
        IP=IP+1
        IF(IP.GT.MAXP2)GO TO 4370
!CCCC   THE FOLLOWING 2 CORRECTIONS WERE MADE IN DECEMBER 1988
!CCCC   TO CORRECT THE BOMB OF    READ PARAMETER   UPON 2ND USAGE
!CCCC   JPNAM1(IV)=IH1
!CCCC   JPNAM2(IV)=IH2
        JPNAM1(IP)=IH1
        JPNAM2(IP)=IH2
        PVAL(IP)=VALUE(I2)
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4321)
 4321   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4322)
 4322   FORMAT('      A NAME IN THE LIST OF VARIABLES TO BE READ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4324)
 4324   FORMAT('      INCLUDED THE NAME OF A PREVIOUSLY-DEFINED ',   &
               'PARAMETER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4326)IH1,IH2
 4326   FORMAT('      THE NAME OF THE PARAMETER WAS ',2A4,'  .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4327)
 4327   FORMAT('      NO READ WAS CARRIED OUT.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 8800
!
 4330   CONTINUE
        ICASEA='M'
        IM=IM+1
        IF(IM.GT.MAXM2)GO TO 4370
        JMNAM1(IM)=IH1
        JMNAM2(IM)=IH2
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4331)
 4331   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4332)
 4332   FORMAT('      A NAME IN THE LIST OF VARIABLES TO BE READ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4334)
 4334   FORMAT('      INCLUDED THE NAME OF A PREVIOUSLY-DEFINED ',   &
               'MODEL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4336)IH1,IH2
 4336   FORMAT('      THE NAME OF THE MODEL WAS ',2A4,'  .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4337)
 4337   FORMAT('      NO READ WAS CARRIED OUT.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 8800
!
 4340   CONTINUE
        ICASEA='F'
        IF=IF+1
        IF(IF.GT.MAXF2)GO TO 4370
        JFNAM1(IF)=IH1
        JFNAM2(IF)=IH2
        IFSTA2(IF)=IVSTAR(I2)
        IFSTO2(IF)=IVSTOP(I2)
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4341)
 4341   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4342)
 4342   FORMAT('      A NAME IN THE LIST OF VARIABLES TO BE READ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4344)
 4344   FORMAT('      INCLUDED THE NAME OF A PREVIOUSLY-DEFINED ',   &
               'FUNCTION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4346)IH1,IH2
 4346   FORMAT('      THE NAME OF THE FUNCTION WAS ',2A4,'  .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4347)
 4347   FORMAT('      NO READ WAS CARRIED OUT.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 8800
!
 4350   CONTINUE
        ICASEA='U'
        IU=IU+1
        IF(IU.GT.MAXU2)GO TO 4370
        JUNAM1(IU)=IH1
        JUNAM2(IU)=IH2
        GO TO 4370
!
 4370   CONTINUE
        IE=IE+1
        IF(IE.GT.MAXE2)GO TO 4380
        JENAM1(IE)=IH1
        JENAM2(IE)=IH2
        IECASE(IE)='NEW'
        IF(ICASEA.EQ.'V')IECASE(IE)='OLD'
        IECOL2(IE)=-1
        IF(ICASEA.EQ.'V')IECOL2(IE)=IVALUE(I2)
!CCCC   NOVEMBER 2002.  FIX FOLLOWING LINE FOR "TO" CASE.
!CCCC   GO TO 4200
        GO TO 4280
!
 4380   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4381)
 4381   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4382)
 4382   FORMAT('      THE NUMBER OF NAMES IN THE READ COMMAND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4383)
 4383   FORMAT('      HAS JUST EXCEEDED THE ALLOWABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4384)MAXE2
 4384   FORMAT('      MAXIMUM (',I5,')')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 8800
!
 4280   CONTINUE
        IF(ICASTO.EQ.'ON')GO TO 4215
!
 4200 CONTINUE
 4290 CONTINUE
      NUMV=IV
      NUMP=IP
      NUMM=IM
      NUMF=IF
      NUMU=IU
      NUME=IE
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        WRITE(ICOUT,4411)NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME
 4411   FORMAT('NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME = ',7I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4412)
 4412   FORMAT('I,JVNAM1(I),JVNAM2(I),JPNAM1(I),JPNAM2(I),', &
               'JMNAM1(I),JMNAM2(I),JFNAM1(I),JFNAM2(I),JUNAM1(I),',  &
               'JUNAM2(I)')
        CALL DPWRST('XXX','BUG ')
        DO 4420 I=1,15
          WRITE(ICOUT,4421)I,JVNAM1(I),JVNAM2(I),JPNAM1(I),JPNAM2(I),   &
          JMNAM1(I),JMNAM2(I),JFNAM1(I),JFNAM2(I),JUNAM1(I),JUNAM2(I)
 4421     FORMAT(I8,5X,2A4,1X,2A4,1X,2A4,1X,2A4,1X,2A4)
          CALL DPWRST('XXX','BUG ')
 4420   CONTINUE
      ENDIF
!
!               ***************************************************
!               **  STEP 5--                                     **
!               **  CHECK FOR A VALID NUMBER                     **
!               **  (1 TO 100) OF VARIABLES TO BE READ           **
!               **  (NOTE--THIS DOES NOT INCLUDE PARAMETERS      **
!               **  OR MODELS IN THE ABOVE COUNT--               **
!               **  ONLY VARIABLES.)                             **
!               **  CHECK FOR A VALID NUMBER                     **
!               **  (0 TO 100) OF CONSTANTS TO BE READ   .       **
!               **  CHECK FOR A VALID NUMBER                     **
!               **  (0 TO 100) OF MODELS TO BE READ   .          **
!               **  CHECK FOR A VALID NUMBER                     **
!               **  (0 TO 100) OF FUNCTIONS TO BE READ   .       **
!               **  CHECK FOR A VALID NUMBER                     **
!               **  (1 TO 100) OF UNKNOWNS TO BE READ   .        **
!               ***************************************************
!
      IF(NUMV.LT.0 .OR. NUMV.GT.MAXV2)THEN
!
        WRITE(ICOUT,511)
  511   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,512)
  512   FORMAT('      FOR A READ, THE NUMBER OF VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,513)
  513   FORMAT('      (NOT COUNTING PARAMETERS OR MODELS)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,514)MAXV2
  514   FORMAT('      MUST BE AT MOST ',I8,'  ;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,515)
  515   FORMAT('      SUCH WAS NOT THE CASE HERE;  THE SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,517)NUMV
  517   FORMAT('      NUMBER OF VARIABLES TO BE READ    WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,518)MAXV2
  518   FORMAT('      NOTE--ONLY THE FIRST ',I8,' VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,519)
  519   FORMAT('      WILL BE READ   .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
  520   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MAX(80,IWIDTH))
  521     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
      IF(NUMP.LT.0 .OR. NUMP.GT.MAXP2)THEN
        WRITE(ICOUT,531)
  531   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,532)
  532   FORMAT('      FOR A READ, THE NUMBER OF PARAMETERS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,534)MAXP2
  534   FORMAT('      (CONSTANTS) MUST BE AT MOST ',I8,'  ;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,535)
  535   FORMAT('      SUCH WAS NOT THE CASE HERE; THE SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,537)NUMP
  537   FORMAT('      NUMBER OF PARAMETERS TO BE READ    WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,538)MAXP2
  538   FORMAT('      NOTE--ONLY THE FIRST ',I8,' PARAMETERS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,539)
  539   FORMAT('      WILL BE READ   .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,540)
  540   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,541)(IANSLC(I),I=1,MAX(80,IWIDTH))
  541     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
      IF(NUMM.LT.0 .OR. NUMM.GT.MAXM2)THEN
!
        WRITE(ICOUT,551)
  551   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,552)
  552   FORMAT('      FOR A READ, THE NUMBER OF MODELS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,554)MAXM2
  554   FORMAT('      MUST BE AT MOST ',I8,'  ;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,555)
  555   FORMAT('      SUCH WAS NOT THE CASE HERE; THE SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,557)NUMM
  557   FORMAT('      NUMBER OF MODELS TO BE READ    WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,558)MAXM2
  558   FORMAT('      NOTE--ONLY THE FIRST ',I8,' MODELS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,559)
  559   FORMAT('      WILL BE READ   .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,560)
  560   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,561)(IANSLC(I),I=1,MIN(80,IWIDTH))
  561     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
      IF(NUMF.LT.0 .OR. NUMM.GT.MAXF2)THEN
!
        WRITE(ICOUT,571)
  571   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,572)
  572   FORMAT('      FOR A READ, THE NUMBER OF FUNCTIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,574)MAXF2
  574   FORMAT('      MUST BE AT MOST ',I8,'  ;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,575)
  575   FORMAT('      SUCH WAS NOT THE CASE HERE; THE SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,577)NUMF
  577   FORMAT('      NUMBER OF FUNCTIONS TO BE READ    WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,578)MAXF2
  578   FORMAT('      NOTE--ONLY THE FIRST ',I8,' FUNCTIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,579)
  579   FORMAT('      WILL BE READ   .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,560)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,561)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
      IF(NUMU.LT.0 .OR. NUMU.GT.MAXU2)THEN
!
        WRITE(ICOUT,611)
  611   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,612)
  612   FORMAT('      FOR A READ, THE NUMBER OF UNKNOWNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,614)MAXU2
  614   FORMAT('      MUST BE AT MOST ',I8,'  ;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,615)
  615   FORMAT('      SUCH WAS NOT THE CASE HERE; THE SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,617)NUMU
  617   FORMAT('      NUMBER OF UNKNOWNS TO BE READ    WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,618)MAXU2
  618   FORMAT('      NOTE--ONLY THE FIRST ',I8,' UNKNOWNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,619)
  619   FORMAT('      WILL BE READ   .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,620)
  620   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,621)(IANSLC(I),I=1,MAX(80,IWIDTH))
  621     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
      IF(NUME.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4451)
 4451   FORMAT('***** ERROR IN SERIAL READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4452)
 4452   FORMAT('      NO VARIABLE NAMES WERE PROVIDED IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4453)
 4453   FORMAT('      READ STATEMENT, HENCE NO READ WAS CARRIED OUT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4454)
 4454   FORMAT('      ILLUSTRATIVE EXAMPLE TO DEMONSTRATE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4455)
 4455   FORMAT('      THE PROPER FORM FOR THE READ COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4456)
 4456   FORMAT('      SUPPOSE THE ANALYST WISHES TO READ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4457)
 4457   FORMAT('      DATA FROM THE FILE    CALIB.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4458)
 4458   FORMAT('      INTO THE INTERNAL VARIABLES Y, X1, AND X2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4459)
 4459   FORMAT('      THIS IS DONE BY ENTERING THE COMMAND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4460)
 4460   FORMAT('      READ CALIB. Y X1 X2')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 8800
      ENDIF
!
!               *******************************************************
!               **  STEP 6--                                         **
!               **  THOSE NAMES WHICH ARE OF THE UNKNOWN CATEGORY    **
!               **  WILL BECOME  FUTURE VARIABLES.                   **
!               **  ASSIGN THESE VARIABLES TO THE NEXT AVAILABLE     **
!               **  COLUMNS, AND UPDATE THE NAME TABLE ACCORDINGLY.  **
!               *******************************************************
!
      IF(NUME.GT.0)THEN
        INAM=NUMNAM
        ICOL=NUMCOL
        DO 700 IE=1,NUME
          IF(IECOL2(IE).GE.1)GO TO 700
          INAM=INAM+1
          ICOL=ICOL+1
!
          IF(INAM.GT.MAXNAM)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,711)
  711       FORMAT('***** ERROR IN SERIAL READ--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,712)
  712       FORMAT('      THE NUMBER OF NAMES')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,713)
  713       FORMAT('      (PARAMETERS + VARIABLES + FUNCTIONS)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,714)
  714       FORMAT('      HAS JUST EXCEEDED THE MAXIMUM SIZE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,715)MAXNAM
  715       FORMAT('      (',I5,') OF THE INTERNAL NAME TABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 8800
          ENDIF
!
          IF(ICOL.GT.MAXCOL)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,721)
  721       FORMAT('***** ERROR IN SERIAL READ--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,722)
  722       FORMAT('      THE NUMBER OF COLUMNS IN THE INTERNAL ',   &
                   'DATAPLOT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,724)
  724       FORMAT('      DATA ARRAY HAS JUST EXCEEDED THE ALLOWABLE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,725)MAXCOL
  725       FORMAT('      MAXIMUM (',I5,')')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 8800
          ENDIF
!
          IHNAME(INAM)=JENAM1(IE)
          IHNAM2(INAM)=JENAM2(IE)
          IUSE(INAM)='V'
          IVALUE(INAM)=ICOL
          IECOL2(IE)=ICOL
          IN(INAM)=0
  700   CONTINUE
        NUMNAM=INAM
        NUMCOL=ICOL
      ENDIF
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,791)NUMNAM,NUMCOL,NUME
  791   FORMAT('NUMNAM,NUMCOL,NUME = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,793)IFILE
  793   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **  STEP 7--                                         **
!               **  FIRST, BRANCH TO THE APPROPRIATE SUBCASE         **
!               **  (DEPENDING ON WHETHER UNQUALIFIED, SUBSET OR FOR);*
!               **  THE DETERMINE THE LENGTH OF THE LONGEST          **
!               **  VARIABLE TO BE READ    IN ;                      **
!               **  THEN READ IN  THE VARIABLES                      **
!               **  THAT WERE SPECIFIED.                             **
!               *******************************************************
!
      ISTEPN='7'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MAXNRD=MAXN
      IF(ICASEQ.EQ.'FULL')GO TO 7310
      IF(ICASEQ.EQ.'SUBS')GO TO 7320
      IF(ICASEQ.EQ.'FOR')GO TO 7330
!
 7310 CONTINUE
      DO 7315 I=1,MAXNRD
      ISUB(I)=1
 7315 CONTINUE
      NQ2=MAXNRD
      GO TO 7350
!
 7320 CONTINUE
      NIOLD=MAXNRD
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ2=NIOLD
      GO TO 7350
!
 7330 CONTINUE
      NIOLD=MAXNRD
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ2=NFOR
      GO TO 7350
!
 7350 CONTINUE
!
!               *******************************************
!               **  STEP 8--                             **
!               **  IF A DATA ROW MINIMUM EXISTS AND SO  **
!               **  OUR ATTENTION IS FOCUSED ONLY ON     **
!               **  CERTAIN ROWS OF THE DATA FILE,       **
!               **  THEN GO DOWN TO THE FIRST SUCH ROW   **
!               **  IN THE FILE.                         **
!               *******************************************
!
      ISTEPN='8'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFROW1.LE.1)GO TO 7369
        IFRMIN=1
        IFRMAX=IFROW1-1
        IF(IFRMIN.GT.IFRMAX)GO TO 7369
        MINCO2=1
        MAXCO2=NUMRCM
        IF(IRD2.EQ.IRD)MAXCO2=80
        IFCOL3=IFCOL1
        IFCOL4=IFCOL2
!       THE FOLLOWING 2 LINES WERE INSERTED FEBRUARY 1988
!       TO "TURN OFF" THE    COLUMN LIMITS    IF READING FROM A
!       NON-FILE (THAT IS, IF READING FROM THE TERMINAL OR WITHIN A
!       MACRO).
        IF(IOFILE.EQ.'NO')IFCOL3=MINCO2
        IF(IOFILE.EQ.'NO')IFCOL4=MAXCO2
        IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
!
        DO 7360 IFROW=IFRMIN,IFRMAX
          IF(IOTERM.EQ.'LOOP')THEN
            ILOOLI=ILOOLI+1
            GO TO 7360
          ELSEIF(IOFILE.EQ.'NO')THEN
            READ(IRD2,7362,END=7363)IJUNK
 7362       FORMAT(A1)
          ELSEIF(IOFILE.EQ.'YES')THEN
            NUMCHA=-1
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                        IA,NUMCHA,   &
                        ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
              WRITE(ICOUT,77361)IFROW,IFRMIN,IFRMAX
77361         FORMAT('AT 7362: IFROW,IFRMIN,IFRMAX = ',3I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,77363)IFILE
77363         FORMAT('IFILE = ',A80)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
!
          IF(IERROR.EQ.'YES')GO TO 8800
          IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
            NUMCHA.EQ.3)GO TO 7363
          GO TO 7360
 7363     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7364)
 7364     FORMAT('***** ERROR IN SERIAL READ--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7365)
 7365     FORMAT('      END OF FILE ENCOUNTERED WHILE SKIPPING OVER',   &
                 'HEADER LINES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7367)
 7367     FORMAT('      NOTE SKIP AND ROW LIMITS SETTINGS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7368)ISKIP,IFROW1,IFROW2
 7368     FORMAT('      ISKIP,IFROW1,IFROW2 = ',3I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 8800
!
 7360   CONTINUE
 7369 CONTINUE
!
!               *******************************************
!               **  STEP 9--                             **
!               **  IN ADDITION, IF HEADER (= NON-DATA)  **
!               **  LINES EXIST WHICH ARE TO BE SKIPPED  **
!               **  OVER IN THE READ, DO SO HERE.        **
!               *******************************************
!
      ISTEPN='9'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'NO')GO TO 7389
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IFROW1.LE.1)THEN
          WRITE(ICOUT,7371)
 7371     FORMAT('THE NUMBER OF HEADER LINES')
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,7372)
 7372     FORMAT('THE NUMBER OF (ADDITIONAL) HEADER LINES')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,7373)ISKIP
 7373   FORMAT('    BEING SKIPPED = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ISKIP.LE.0)GO TO 7389
      IFRMIN=IFROW1
      IFRMAX=IFROW1+ISKIP-1
      IF(IFRMIN.GT.IFRMAX)GO TO 7389
      MINCO2=1
      MAXCO2=NUMRCM
      IF(IRD2.EQ.IRD)MAXCO2=80
      IFCOL3=IFCOL1
      IFCOL4=IFCOL2
!     THE FOLLOWING 2 LINES WERE INSERTED FEBRUARY 1988
!     TO "TURN OFF" THE    COLUMN LIMITS    IF READING FROM A NON-FILE
!     (THAT IS, IF READING FROM THE TERMINAL OR WITHIN A MACRO).
      IF(IOFILE.EQ.'NO')THEN
        IFCOL3=MINCO2
        IFCOL4=MAXCO2
      ENDIF
      IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
      DO 7380 IFROW=IFRMIN,IFRMAX
        IF(IOTERM.EQ.'LOOP')THEN
          ILOOLI=ILOOLI+1
          GO TO 7380
        ELSEIF(IOFILE.EQ.'NO')THEN
          READ(IRD2,7382,END=7383)IJUNK
 7382     FORMAT(A1)
        ELSE IF(IOFILE.EQ.'YES')THEN
          NUMCHA=-1
          CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                      IA,NUMCHA,   &
                      ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
            WRITE(ICOUT,77381)IFROW,IFRMIN,IFRMAX
77381       FORMAT('AT 7382: IFROW,IFRMIN,IFRMAX = ',3I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,77383)IFILE
77383       FORMAT('IFILE = ',A80)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
        IF(IERROR.EQ.'YES')GO TO 8800
        IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
          NUMCHA.EQ.3)GO TO 7383
        GO TO 7380
!
 7383   CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7384)
 7384     FORMAT('***** ERROR IN SERIAL READ--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7385)
 7385     FORMAT('      END OF FILE ENCOUNTERED WHILE SKIPPING ',   &
                 'HEADER LINES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7387)
 7387     FORMAT('      NOTE SKIP AND ROW LIMITS SETTINGS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7388)ISKIP,IFROW1,IFROW2
 7388     FORMAT('      ISKIP,IFROW1,IFROW2 = ',3I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 8800
!
 7380   CONTINUE
 7389 CONTINUE
!
!               ************************
!               **  STEP 10--         **
!               **  READ IN THE DATA  **
!               ************************
!
      ISTEPN='10'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7210)NUME
 7210   FORMAT('NUME = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7211)IRD,IRD2
 7211   FORMAT('IRD,IRD2 = ',I8,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 7260 I=1,MAXRCL
        ISTOR1(I)='    '
        ISTOR2(I)='    '
        ISTOR3(I)='    '
        IB(I)='    '
 7260 CONTINUE
!
      IF(NUME.GT.0)THEN
        DO 7300 I=1,NUME
          IEN(I)=0
 7300   CONTINUE
      ENDIF
!
      MINCO2=1
      MAXCO2=NUMRCM
      IF(IRD2.EQ.IRD)MAXCO2=80
      IFCOL3=IFCOL1
      IFCOL4=IFCOL2
!     THE FOLLOWING 2 LINES WERE INSERTED FEBRUARY 1988
!     TO "TURN OFF" THE    COLUMN LIMITS    IF READING FROM A NON-FILE
!     (THAT IS, IF READING FROM THE TERMINAL OR WITHIN A MACRO).
      IF(IOFILE.EQ.'NO')THEN
        IFCOL3=MINCO2
        IFCOL4=MAXCO2
      ENDIF
      IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
!
      I=0
!
      IE2=0
      IE3=0
!
      NUMLRD=0
      IENDTY=1
      IFRMIN=IFROW1+ISKIP
      IFRMAX=IFROW2
      IF(IHOST1.EQ.'CDC'.AND.IFRMAX.GT.130000)IFRMAX=130000
!CCCC THE FOLLOWING LINE WAS INSERTED APRIL 1989
      IF(IFRMAX.GE.IBILLI)IFRMAX=IBILLI
      IF(IFRMIN.GT.IFRMAX)GO TO 7470
      IFLGSV=0
      DO 7400 IFROW=IFRMIN,IFRMAX
        CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,X0,NUMDPL,IFLGSV, &
                    IXC,NXC,                                           &
                    ICASRE,IFUNC2,N2,MAXN2,                            &
                    IMACRO,IMACNU,IMACCS,                              &
                    IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,    &
                    IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,      &
                    ICOMCH,ICOMSW,LINETY,IGRPAU,                       &
                    IFCOLL,IFCOLU,ITYPE,NCOLS,NCALL,                   &
                    IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,                &
                    MAXRDV,MAXCHV,IFIETY,                              &
                    IDECPT,IDATMV,IDATNN,                              &
                    IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,IREALT,  &
                    XTAG,IOUNI5,                                       &
                    IREAAS,IREAPC,                                     &
                    IB,                                                &
                    IOTERM,IANSLO,MAXLIL,MAXCIL,ILOOST,ILOOLI,         &
                    IREPCH,IMALEV,IREANQ,                              &
                    IERRFI,IBUGS2,ISUBRO,IERROR)
!CCCC   ICOMCH AND ICOMFFL ADDED TO ARGUMENT LIST MAY, 1990.
!CCCC   THE    LINETY   ARGUMENT ADDED ABOVE      SEPTEMBER 1995
!CCCC   THE FOLLOWING LINE WAS ADDED    SEPTEMBER 1995
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,77401)
77401     FORMAT('READING DATA LINES (DO7400):')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,77403)IFROW,IFRMIN,IFRMAX
77403     FORMAT('IFROW,IFRMIN,IFRMAX = ',3I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,77405)LINETY,NUMLRD,I,MAXN
77405     FORMAT('LINETY,NUMLRD,I,MAXN=',A4,2X,3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(LINETY.EQ.'BLAN')GO TO 7400
        NUMLRD=NUMLRD+1
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
          WRITE(ICOUT,77407)NUMLRD,NUME
77407     FORMAT('NUMLRD,NUME=',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IERROR.EQ.'YES')GO TO 8800
        IF(IFROW.EQ.IFRMIN)THEN
          DO 7425 K=1,132
            ISTOR3(K)=ISTOR2(K)
 7425     CONTINUE
          GO TO 7440
        ENDIF
        IF(IEND.EQ.'YES')GO TO 7480
        GO TO 7440
!
 7440   CONTINUE
        IF(NUMDPL.LE.0)GO TO 7468
        DO 7465 ID=1,NUMDPL
          IE2=IE2+1
          IE3=IE3+1
          IF(IE2.GT.NUME)IE2=1
!
 7450     CONTINUE
          IF(IE2.GT.1)GO TO 7460
          I=I+1
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7451)
 7451       FORMAT('***** FROM THE MIDDLE  OF DPSERI (7450)--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7452)IFROW,IFRMIN,IFRMAX
 7452       FORMAT('IFROW,IFRMIN,IFRMAX = ',3I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7453)I,ISUB(I),NUME
 7453       FORMAT('I,ISUB(I),NUME = ',3I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7454)MAXN,MAXCOL,MAXCP1,MAXCP2
 7454       FORMAT('MAXN,MAXCOL,MAXCP1,MAXCP2 = ',4I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7455)X0(1),X0(2),X0(3)
 7455       FORMAT('X0(1),X0(2),X0(3) = ',3E15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7456)IECOL2(1),IECOL2(2),IECOL2(3)
 7456       FORMAT('IECOL2(1),IECOL2(2),IECOL2(3) = ',3I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7457)IEN(1),IEN(2),IEN(3)
 7457       FORMAT('IEN(1),IEN(2),IEN(3) = ',3I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(I.GT.MAXN)GO TO 7480
          IF(ISUB(I).EQ.1)GO TO 7460
          GO TO 7450
!
 7460     CONTINUE
          IE=IE2
          Z0=X0(ID)
          ICOLVJ=IECOL2(IE)
          IJ=MAXN*(ICOLVJ-1)+I
          IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
          IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
          IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
          IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
          IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
          IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
          IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
          IEN(IE)=I
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
            WRITE(ICOUT,7461)IE,Z0,MAXN,ICOLVJ,I,IJ
 7461       FORMAT('IE,Z0,MAXN,ICOLVJ,I,IJ = ',I8,E15.7,I8,I8,I8,I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7462)MAXCOL,MAXCP1,MAXCP2
 7462       FORMAT('MAXCOL,MAXCP1,MAXCP2 = ',I8,I8,I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7463)IEN(IE)
 7463       FORMAT('IEN(IE) = ',I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
 7465   CONTINUE
        NUMVRD=IE3
        IF(NUME.LE.IE3)NUMVRD=NUME
        GO TO 7469
 7468   CONTINUE
        NUMVRD=IE2-1
        GO TO 7469
 7469   CONTINUE
!
 7400 CONTINUE
 7470 CONTINUE
!
      IENDTY=2
      GO TO 7490
 7480 CONTINUE
      IENDTY=1
      NUMLRD=NUMLRD-1
      GO TO 7490
 7490 CONTINUE
!
!               *****************************
!               **  STEP 11--              **
!               **  UPDATE THE NAME TABLE  **
!               *****************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        WRITE(ICOUT,7601)NUMVRD
 7601   FORMAT('NUMVRD = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NUMVRD.GT.0)THEN
        DO 7610 IE=1,NUMVRD
          N=IEN(IE)
          ICOLVJ=IECOL2(IE)
          DO 7620 J=1,NUMNAM
            IF(IUSE(J).EQ.'V'.AND.IVALUE(J).EQ.ICOLVJ)GO TO 7625
            GO TO 7620
 7625       CONTINUE
            IUSE(J)='V'
            IVALUE(J)=ICOLVJ
            IF(N.GT.IN(J))IN(J)=N
            IVSTAR(J)=MAXN*(ICOLVJ-1)+1
            IVSTOP(J)=MAXN*(ICOLVJ-1)+N
 7620     CONTINUE
 7610   CONTINUE
      ENDIF
!
      NUMVRP=NUMVRD+1
      IF(NUMVRP.LE.NUME)THEN
        DO 7650 IE=NUMVRP,NUME
          IEREV=NUME-IE+NUMVRP
          IF(IEREV.GE.1)THEN
            IF(IECASE(IEREV).EQ.'NEW')THEN
              INAM=NUMNAM
              IHNAME(INAM)='    '
              IHNAM2(INAM)='    '
              IUSE(INAM)='    '
              IVALUE(INAM)=0
              IN(INAM)=0
              NUMNAM=NUMNAM-1
              NUMCOL=NUMCOL-1
            ENDIF
          ENDIF
 7650   CONTINUE
      ENDIF
      GO TO 7900
!
 7900 CONTINUE
!
!               *************************************
!               **  STEP 12--                      **
!               **  WRITE OUT SUMMARY INFORMATION  **
!               **  ABOUT THE FILE THAT WAS READ   **
!               *************************************
!
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8100)
 8100   FORMAT('INPUT DATA FILE SUMMARY INFORMATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8101)IRD2
 8101   FORMAT('INPUT UNIT DEVICE NUMBER       = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8102)IFCOL3,IFCOL4
 8102   FORMAT('INPUT FILE COLUMN     LIMITS   = ',I8,4X,I8)
        CALL DPWRST('XXX','BUG ')
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
          WRITE(ICOUT,1111)IFROW2,INTINF
 1111     FORMAT('IFROW2,INTINF = ',I11,2X,I11)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IFROW2.EQ.INTINF)THEN
          WRITE(ICOUT,8103)IFROW1
 8103     FORMAT('INPUT FILE ROW        LIMITS = ',I8,4X,'INFINITY')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8104)IFROW1,IFROW2
 8104     FORMAT('INPUT FILE ROW        LIMITS   = ',I8,4X,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,8105)ISKIP
 8105   FORMAT('NUMBER OF HEADER LINES SKIPPED = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8106)NUMLRD
 8106   FORMAT('NUMBER OF DATA   LINES READ    = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8107)NUMVRD
 8107   FORMAT('NUMBER OF VARIABLES    READ    = ',I8)
        CALL DPWRST('XXX','BUG ')
!
        IFRST=IFCOL3
        IF(IFRST+240-1.GE.IFCOL4)THEN
          ILAST=IFCOL4
        ELSE
          ILAST=IFRST+240-1
        ENDIF
!
        WRITE(ICOUT,8111)
 8111   FORMAT('THE SCANNED REGION OF THE FIRST DATA LINE READ = ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8112)(ISTOR3(J),J=IFRST,ILAST)
 8112   FORMAT(240A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8113)
 8113   FORMAT('THE SCANNED REGION OF THE LAST  DATA LINE READ = ')
        CALL DPWRST('XXX','BUG ')
        IF(IENDTY.EQ.1)THEN
          WRITE(ICOUT,8114)(ISTOR1(J),J=IFRST,ILAST)
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IENDTY.EQ.2)THEN
          WRITE(ICOUT,8114)(ISTOR2(J),J=IFRST,ILAST)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 8114   FORMAT(240A1)
      ENDIF
!
!               *********************************************
!               **  STEP 13--                              **
!               **  PRINT OUT SUMMARY INFORMATION          **
!               **  ABOUT THE VARAIBLES THAT WERE READ IN  **
!               *********************************************
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8211)
 8211   FORMAT('VARIABLE     COLUMN    OBS/VARIABLE')
        CALL DPWRST('XXX','BUG ')
!
        DO 8200 IE=1,NUME
          IH1=JENAM1(IE)
          IH2=JENAM2(IE)
          DO 8300 I=1,NUMNAM
            I2=I
            IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
              WRITE(ICOUT,8311)IH1,IH2,IVALUE(I2),IN(I2)
 8311         FORMAT(A4,A4,1X,I8,5X,I8)
              CALL DPWRST('XXX','BUG ')
            ENDIF
 8300     CONTINUE
 8200   CONTINUE
      ENDIF
!
!               ***************************************
!               **  STEP 88--                        **
!               **  FOR THE FILE CASE,               **
!               **  CLOSE THE FILE.                  **
!               ***************************************
!
 8800 CONTINUE
      ISTEPN='88'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,8803)IOFILE,ICURST,IREARW
 8803   FORMAT('IOFILE,ICURST,IREARW   = ',A4,A12,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IOFILE.EQ.'YES'.AND.IREACS.EQ.'OPEN')THEN
        IENDFI='OFF'
        IREWIN='ON'
        IF(IREARW.EQ.'ON')THEN
          CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                      IENDFI,IREWIN,ISUBN0,IERRFI,   &
                      IBUGS2,ISUBRO,IERROR)
          IREACS='CLOSED'
        ENDIF
      ENDIF
!
!               ******************************************
!               **  STEP 89--                           **
!               **  IF THE MACRO STATUS IS OPEN         **
!               **  THEN CHANGE IDEV FROM READ TO MACR  **
!               ******************************************
!
!CCCC IF(IMACST.EQ.'OPFI')IDEV='MACR'
!CCCC IF(IMACCS.EQ.'OPEN')IDEV='MACR'
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
!     FEBRUARY 2025: CLOSE dpst5f.dat IF NEEDED
!
      IF(IREAPM.EQ.'ON' .OR. IREALT.EQ.'ON')THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGS2,ISUBRO,IERROR)
      ENDIF
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SERI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSERI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFROW1,IFROW2,ICASRE
 9012   FORMAT('IFROW1,IFROW2,ICASRE = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFCOL1,IFCOL2
 9013   FORMAT('IFCOL1,IFCOL2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ISKIP,INTINF,IBUGS2,IBUGQ
 9014   FORMAT('ISKIP,INTINF,IBUGS2,IBUGQ = ',I8,I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR
 9015   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IMACRO,IMACNU,IMACCS
 9016   FORMAT('IMACRO,IMACNU,IMACCS = ',A4,I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IRD,IRD2
 9017   FORMAT('IRD,IRD2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IOSW,IOFILE,IOTERM
 9018   FORMAT('IOSW,IOFILE,IOTERM = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)IBUGS2,ISUBRO,IERROR
 9019   FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IOUNIT
 9021   FORMAT('IOUNIT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE
 9022   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ISTAT
 9023   FORMAT('ISTAT  = ',A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)IFORM
 9024   FORMAT('IFORM  = ',A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9025)IACCES
 9025   FORMAT('IACCES = ',A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9026)IPROT
 9026   FORMAT('IPROT  = ',A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9027)ICURST
 9027   FORMAT('ICURST = ',A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)IENDFI
 9028   FORMAT('IENDFI = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9029)IREWIN
 9029   FORMAT('IREWIN = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)ISUBN0
 9031   FORMAT('ISUBN0 = ',A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)IERRFI
 9032   FORMAT('IERRFI = ',A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9071)IREARW
 9071   FORMAT('IREARW = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSERI
      SUBROUTINE DPSESB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A SEASONAL SUBSERIES PLOT
!              (USED IN TIME SERIES TO IDENTIFY SEASONALITY)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/2
!     ORIGINAL VERSION--FEBRUARY  1999.
!     UPDATED         --JANUARY   2012. USE DPPARS AND DPPAR3
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
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHIGH
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
      DIMENSION X1(MAXOBV)
      DIMENSION YTEMP(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION XHIGH2(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB3),YTEMP(1))
      EQUIVALENCE (GARBAG(IGARB4),XHIGH(1))
      EQUIVALENCE (GARBAG(IGARB5),XHIGH2(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IHIGH='OFF'
      ISUBN1='DPSE'
      ISUBN2='SB  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SESB')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSESB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,MAXCOL
   52   FORMAT('ICASPL,IAND1,IAND2 = ',3(A4,2X),I8)
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SESB')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.2 .AND. ICOM.EQ.'SEAS' .AND.   &
         IHARG(1).EQ.'SUBS' .AND. IHARG(2).EQ.'PLOT')THEN
        ICASPL='SESB'
        ILASTC=2
      ELSEIF(NUMARG.GE.3 .AND.   &
            (ICOM.EQ.'SUBS' .OR. ICOM.EQ.'HIGH') .AND.   &
             IHARG(1).EQ.'SEAS' .AND. IHARG(2).EQ.'SUBS' .AND.   &
             IHARG(3).EQ.'PLOT')THEN
        ICASPL='SESB'
        ILASTC=3
        IHIGH='ON'
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SESB')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='SEASONAL SUBSERIES PLOT'
      MINNA=1
      MAXNA=100
      MINN2=3
      IFLAGE=1
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      IF(IHIGH.EQ.'ON')THEN
        MINNVA=2
        MAXNVA=2
      ELSE
        MINNVA=1
        MAXNVA=1
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SESB')THEN
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
                  Y1,XHIGH,Y1,NS,NS,NS,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***********************************************
!               **  STEP 3.2A--                              **
!               **  CHECK FOR PARAMETER PERIOD               **
!               ***********************************************
!
      IHP='PERI'
      IHP2='OD  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        PERIOD=12.0
      ELSE
        PERIOD=VALUE(ILOCP)
      ENDIF
!
!               ***********************************************
!               **  STEP 3.2B--                              **
!               **  CHECK FOR PARAMETER START                **
!               ***********************************************
!
      IHP='STAR'
      IHP2='T   '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        ISTART=1
      ELSE
        ISTART=INT(VALUE(ILOCP)+0.5)
      ENDIF
      IF(ISTART.LT.1)ISTART=1
!
!
!               *******************************************************
!               **  STEP 41--                                        **
!               **  FORM THE VERTICAL AND HORIZONTALAXIS             **
!               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY)FOR THE   **
!               **  PLOT.  FORM THE CURVE DESIGNATION VARIABLED(.) . **
!               **  THIS WILL BE ALL ONES.                           **
!               **  DEFINE THE NUMBER OF PLOT POINTS   (NPLOTP).     **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES(NPLOTV).     **
!               *******************************************************
!
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SESB')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPSES2(Y1,NS,X1,YTEMP,ICASPL,MAXN,   &
                  XHIGH,XHIGH2,IHIGH,   &
                  Y,X,D,NPLOTP,NPLOTV,   &
                  PERIOD,ISTART,   &
                  IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SESB')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSESB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               3I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO 9015 I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPSESB
      SUBROUTINE DPSES2(Y1,N,X1,YTEMP,ICASPL,MAXN,   &
                        XHIGH,XHIGH2,IHIGH,   &
                        Y,X,D,NPLOTP,NPLOTV,   &
                        PERIOD,ISTART,   &
                        IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A SEASONAL SUBSERIES PLOT
!     INPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED) OBSERVATIONS
!                               FOR THE FIRST  VARIABLE.
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!     CAUTION--THE INPUT VARIABLE Y1(.) WILL BE CHANGED HEREIN
!              (IT WILL BE SORTED)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/2
!     ORIGINAL VERSION--FEBRUARY  1998.
!     UPDATED         --JANUARY   2012. SUPPORT FOR HIGHLIGHT OPTION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IHIGH
      CHARACTER*4 IBUGG3
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION X1(*)
      DIMENSION YTEMP(*)
      DIMENSION XHIGH(*)
      DIMENSION XHIGH2(*)
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSE'
      ISUBN2='S2  '
      IERROR='NO'
!
      XCSAVE=CPUMIN
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SES2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSES2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,IERROR
   52   FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IHIGH,N,MAXN
   53   FORMAT('ICASPL,IHIGH,N,MAXN = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y1(I),XHIGH(I)
   56     FORMAT('I, Y1(I),XHIGH(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN SEASONAL SUBSERIES PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 3.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,114)N
  114   FORMAT('      THE NUMBER OF OBSERVATIONS      = ',I6)
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
!               ******************************************************
!               **  STEP 12--                                       **
!               **  COMPUTE COORDINATES FOR SEASONAL SUBSERIES PLOT **
!               **  CREATE A SEASONAL INDEX VARIABLE FIRST          **
!               ******************************************************
!
!
      IPER=INT(PERIOD+0.5)
      IF(IPER.LT.1)IPER=12
!
      DO 1210 I=1,N
        K=I+ISTART-1
        X1(I)=MOD(K-1,IPER) + 1
 1210 CONTINUE
!
!               ******************************************************
!               **  STEP 13--                                       **
!               **  FOR EACH VALUE OF THE PERIOD, COMPUTE           **
!               **  1) THE NUMER OF ELEMENTS                        **
!               **  2) THE MEAN OF THE ELEMENTS                     **
!               ******************************************************
!
      IADD=0
      IF(IHIGH.EQ.'ON')IADD=1
      IWRITE='OFF'
      XCOOR=0.0
      NPLOTP=0
      DO 1300 J=1,IPER
        NELEM=0
        DO 1310 I=1,N
          IF(X1(I).EQ.J)THEN
            NELEM=NELEM+1
            YTEMP(NELEM)=Y1(I)
            XHIGH2(NELEM)=XHIGH(I)
          ENDIF
 1310   CONTINUE
        IF(NELEM.LT.1)GO TO 1300
        CALL MEAN(YTEMP,NELEM,IWRITE,YMEAN,IBUGG3,IERROR)
        DO 1320 L=1,NELEM
          XCOOR=XCOOR+1.0
          IF(L.EQ.1)XCSAVE=XCOOR
!
          IF(IHIGH.EQ.'ON')THEN
            IF(XHIGH2(L).GE.0.5)THEN
              NPLOTP=NPLOTP+1
              X(NPLOTP)=XCOOR
              Y(NPLOTP)=YTEMP(L)
              D(NPLOTP)=1.0
            ENDIF
          ENDIF
!
          NPLOTP=NPLOTP+1
          X(NPLOTP)=XCOOR
          Y(NPLOTP)=YTEMP(L)
          D(NPLOTP)=REAL(IADD+2*J-1)
 1320   CONTINUE
        NPLOTP=NPLOTP+1
        X(NPLOTP)=XCSAVE
        Y(NPLOTP)=YMEAN
        D(NPLOTP)=REAL(IADD+2*J)
        NPLOTP=NPLOTP+1
        X(NPLOTP)=XCOOR
        Y(NPLOTP)=YMEAN
        D(NPLOTP)=REAL(IADD+2*J)
        XCOOR=XCOOR+1.0
 1300 CONTINUE
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
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SES2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSES2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,ICASPL,N,NPLOTP,NPLOTV
 9013   FORMAT('IERROR,ICASPL,N,NPLOTP,NPLOTV = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,Y1(I)
 9016     FORMAT('I, Y1(I), = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO 9022 I=1,NPLOTP
          WRITE(ICOUT,9023)I,Y(I),X(I),D(I)
 9023     FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9022   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSES2
      SUBROUTINE DPSETH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,   &
                        MAXSEG,PSEGTH,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE THICKNESS FOR A SEGMENT.
!              THE THICKNESS FOR SEGMENT I WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE REAL
!              VECTOR PSEGTH(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A HOLLERITH VECTOR)
!                     --IARG   (A HOLLERITH VECTOR)
!                     --ARG
!                     --NUMARG
!                     --PDEFTH
!                     --MAXSEG
!     OUTPUT ARGUMENTS--PSEGTH (A REAL VECTOR
!                              WHOSE I-TH ELEMENT CONTAINS THE
!                              THICKNESS FOR SEGMENT I.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--ALAN HECKERT
!                 COMPUTER SERVICES DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
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
      CHARACTER*4 IARGT
      REAL        PDEFTH
      REAL        PSEGTH
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      REAL        PHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
!
      DIMENSION PSEGTH(*)
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
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'THIC')GO TO 1140
      GO TO 1199
!
 1110 CONTINUE
      IF(NUMARG.LE.1)GO TO 1120
      IF(IHARG(2).EQ.'ON')GO TO 1120
      IF(IHARG(2).EQ.'OFF')GO TO 1120
      IF(IHARG(2).EQ.'AUTO')GO TO 1120
      IF(IHARG(2).EQ.'DEFA')GO TO 1120
      GO TO 1125
!
 1120 CONTINUE
      PHOLD=PDEFTH
      GO TO 1130
!
 1125 CONTINUE
      PHOLD=ARG(2)
      GO TO 1130
!
 1130 CONTINUE
      IFOUND='YES'
      DO 1135 I=1,MAXSEG
      PSEGTH(I)=PHOLD
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1136)PSEGTH(I)
 1136 FORMAT('ALL SEGMENT THICKNESSS HAVE JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 1199
!
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GO TO 1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPSETH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE SEGMENT ... THICKNESS COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE SEGMENT IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      SEGMENT 3 THICKNESS 0.3')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXSEG)GO TO 1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPSETH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE SEGMENT ... THICKNESS COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF SEGMENTS MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXSEG
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',   &
      'SEGMENT.')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1160 CONTINUE
      IF(NUMARG.LE.2)GO TO 1170
      IF(IHARG(3).EQ.'ON')GO TO 1170
      IF(IHARG(3).EQ.'OFF')GO TO 1170
      IF(IHARG(3).EQ.'AUTO')GO TO 1170
      IF(IHARG(3).EQ.'DEFA')GO TO 1170
      GO TO 1175
!
 1170 CONTINUE
      PHOLD=PDEFTH
      GO TO 1180
!
 1175 CONTINUE
      PHOLD=ARG(3)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PSEGTH(I)=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)I,PSEGTH(I)
 1186 FORMAT('THE THICKNESS FOR SEGMENT ',I8,   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPSETH
      SUBROUTINE DPSGTU(MAXNXT,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A 2-SAMPLE SIEGEL-TUKEY TEST FOR EQUAL
!              VARIATION.
!
!              NOTE THAT THIS TEST ASSUME EQUAL LOCATIONS FOR THE
!              TWO SAMPLES
!     EXAMPLE--SIEGEL TUKEY TEST Y1 Y2
!              SIEGEL TUKEY TEST Y1 Y2 Y3 Y4
!              SIEGEL TUKEY TEST Y1 TO Y10
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023/06/7
!     ORIGINAL VERSION--JUNE      2023.
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
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
      CHARACTER*4 ICTMP5
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
      DIMENSION YTEMP(2*MAXOBV)
      DIMENSION TEMP1(2*MAXOBV)
      DIMENSION TEMP2(2*MAXOBV)
      DIMENSION TEMP3(2*MAXOBV)
      EQUIVALENCE(GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE(GARBAG(IGARB3),TEMP2(1))
      EQUIVALENCE(GARBAG(IGARB5),TEMP3(1))
      EQUIVALENCE(GARBAG(IGARB7),YTEMP(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSG'
      ISUBN2='TU  '
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
!               ************************************************
!               **  TREAT THE SIEGEL-TUKEY          TEST CASE **
!               ************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SGTU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSGTU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SGTU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTZ=9999
      ICASAN='SGTU'
      ICASA2='TWOT'
!
!     LOOK FOR:
!
!          SIEGEL TUKEY TEST
!          LOWER TAILED
!          UPPER TAILED
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
        ICTMP5=IHARG(I+4)
!
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'SIEG' .AND. ICTMP2.EQ.'TUKE' .AND.   &
               ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='STTE'
          ILASTZ=I+2
        ELSEIF(ICTMP1.EQ.'SIEG' .AND. ICTMP2.EQ.'TUKE')THEN
          IFOUND='YES'
          ICASAN='STTE'
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'LOWE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA2='LOWE'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'UPPE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA2='UPPE'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SGTU')THEN
        WRITE(ICOUT,91)ICASAN,ICASA2,ISHIFT
   91   FORMAT('DPSGTU: ICASAN,ICASA2,ISHIFT = ',   &
               2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SGTU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='SIEGEL TUKEY TEST'
      MINNA=1
      MAXNA=100
      MINN2=3
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SGTU')THEN
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
!               ******************************************************
!               **  STEP 3A--                                       **
!               **  CASE 1: TWO RESPONSE VARIABLES, NO REPLICATION  **
!               **          HANDLE MULTIPLE RESPONSE VARIABLES      **
!               **          DIFFERENTLY FOR ONE SAMPLE AND TWO      **
!               **          SAMPLE TESTS.                           **
!               ******************************************************
!
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SGTU')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVA2=1
      DO 5210 I=1,NUMVAR
        ICOL=I
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    YTEMP,YTEMP,YTEMP,NS1,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        ISTRT2=I+1
        ISTOP2=NUMVAR
!
        DO 5220 J=ISTRT2,ISTOP2
!
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
!
!               *******************************************
!               **  STEP 52--                            **
!               **  PERFORM A SIEGEL TUKEY          TEST **
!               *******************************************
!
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SGTU')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DPSGTU, BEFORE CALL DPSGT2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN
 5212       FORMAT('I,J,NS1,NS2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
            DO 5215 II=1,MAX(NS1,NS2)
              WRITE(ICOUT,5216)II,YTEMP(II),X(II)
 5216         FORMAT('I,YTEMP(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
!
          IVARID=IVARN1(I)
          IVARI2=IVARN2(I)
          IVARI3=IVARN1(J)
          IVARI4=IVARN2(J)
          MAXNX2=2*MAXNXT
          CALL DPSGT2(YTEMP,NS1,X,NS2,ICASA2,   &
                     TEMP1,TEMP2,TEMP3,MAXNX2,   &
                     ICAPSW,ICAPTY,IFORSW,ISGTMC,   &
                     IVARID,IVARI2,IVARI3,IVARI4,   &
                     STATVA,STATCD,ITAB,   &
                     PVAL2T,PVALLT,PVALUT,   &
                     CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,   &
                     CTU999,CTU995,CTU990,CT975,CTU950,CTU900,   &
                     CVL001,CVL005,CVL010,CVL025,CVL050,CVL100,   &
                     CVU999,CVU995,CVU990,CVU975,CVU950,CVU900,   &
                     IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SGTU')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IF(NUMVAR.GT.2)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
          IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
          IF(ITAB.EQ.1)THEN
            CTL001=CVL001
            CTL005=CVL005
            CTL010=CVL010
            CTL025=CVL025
            CTL050=CVL050
            CTL100=CVL100
            CTU999=CVU999
            CTU995=CVU995
            CTU990=CVU990
            CTU975=CVU975
            CTU950=CVU950
            CTU900=CVU900
          ENDIF
          CALL DPMNN5(ICASA2,   &
                      STATVA,STATCD,   &
                      PVAL2T,PVALLT,PVALUT,   &
                      CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,   &
                      CTU999,CTU995,CTU990,CT975,CTU950,CTU900,   &
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SGTU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSGTU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSGTU
      SUBROUTINE DPSGT2(Y1,N1,Y2,N2,ICASAN,   &
                        TEMP1,TEMP2,TEMP3,MAXNXT,   &
                        ICAPSW,ICAPTY,IFORSW,ISGTMC,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        STATVA,STATCD,ITAB,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        CTL001,CTL005,CTL010,CTL025,CTL050,CTL100,   &
                        CTU999,CTU995,CTU990,CTU975,CTU950,CTU900,   &
                        CVL001,CVL005,CVL010,CVL025,CVL050,CVL100,   &
                        CVU999,CVU995,CVU990,CVU975,CVU950,CVU900,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT A 2-SAMPLE SIEGEL TUKEY TEST
!
!              NOTE THAT THIS COMPUTES THE W (RANK STATISTIC), NOT THE
!              U STATISTIC THAT SOME PREFER.
!
!     EXAMPLE--SIEGEL TUKEY TEST Y1 Y2
!     SAMPLE 1 IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS).
!     SAMPLE 2 IS IN INPUT VECTOR Y2 (WITH N2 OBSERVATIONS).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023/06
!     ORIGINAL VERSION--JUNE      2023.
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
      CHARACTER*4 ICASAN
      CHARACTER*4 ISGTMC
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
!
      PARAMETER (NUMALP=6)
      REAL ALPHA(NUMALP)
      PARAMETER (NUMAL2=4)
      REAL ALPHA2(NUMAL2)
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA/0.90, 0.95, 0.975, 0.99, 0.995, 0.999/
      DATA ALPHA2/0.80, 0.90, 0.95, 0.99/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSG'
      ISUBN2='T2  '
      IERROR='NO'
      IWRITE='OFF'
!
      CTL001=0.0
      CTL005=0.0
      CTL010=0.0
      CTL025=0.0
      CTL050=0.0
      CTL100=0.0
      CTU999=0.0
      CTU995=0.0
      CTU990=0.0
      CTU975=0.0
      CTU950=0.0
      CTU900=0.0
      CVL001=0.0
      CVL005=0.0
      CVL010=0.0
      CVL025=0.0
      CVL050=0.0
      CVL100=0.0
      CVL999=0.0
      CVL995=0.0
      CVL990=0.0
      CVL975=0.0
      CVL950=0.0
      CVL900=0.0
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
      CTL001=CPUMIN
      CTL005=CPUMIN
      CTL010=CPUMIN
      CTL025=CPUMIN
      CTL050=CPUMIN
      CTL100=CPUMIN
      CTU900=CPUMIN
      CTU950=CPUMIN
      CTU975=CPUMIN
      CTU990=CPUMIN
      CTU995=CPUMIN
      CTU999=CPUMIN
!
      CVL001=CPUMIN
      CVL005=CPUMIN
      CVL010=CPUMIN
      CVL025=CPUMIN
      CVL050=CPUMIN
      CVL100=CPUMIN
      CVU900=CPUMIN
      CVU950=CPUMIN
      CVU975=CPUMIN
      CVU990=CPUMIN
      CVU995=CPUMIN
      CVU999=CPUMIN
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SGT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSGT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN
   52   FORMAT('IBUGA3,ISUBRO,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IVARID,IVARI2,IVARI3,IVARI4
   53   FORMAT('IVARID,IVARI2,IVARI3,IVARI4 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N1,NUMDIG,D0
   55   FORMAT('N1,NUMDIG,D0 = ',2I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        IF(N1.GE.1)THEN
          DO 56 I=1,N1
            WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57       FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
      ENDIF
!
!               ************************************
!               **   STEP 1--                     **
!               **   CALL DPSGT3 TO COMPUTE THE   **
!               **   BASIC TEST STATISTIC         **
!               ************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL MEAN(Y1,N1,IWRITE,YMEAN1,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,YSD1,IBUGA3,IERROR)
      CALL MEDIAN(Y1,N1,IWRITE,TEMP1,MAXNXT,YMED1,IBUGA3,IERROR)
      CALL MEAN(Y2,N2,IWRITE,YMEAN2,IBUGA3,IERROR)
      CALL MEDIAN(Y2,N2,IWRITE,TEMP1,MAXNXT,YMED2,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,YSD2,IBUGA3,IERROR)
!
      IF(ISGTMC.EQ.'ON')THEN
        DO 110 II=1,N1
          Y1(II)=Y1(II) - YMED1
  110   CONTINUE
        DO 120 II=1,N2
          Y2(II)=Y2(II) - YMED2
  120   CONTINUE
      ENDIF
!
      CALL DPSGT3(Y1,N1,Y2,N2,   &
                  TEMP1,TEMP2,TEMP3,MAXNXT,   &
                  STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                  IBUGA3,ISUBRO,IERROR)
!
!               ***************************************
!               **  STEP 21--                        **
!               **  COMPUTE THE CRITICAL VALUES FOR  **
!               **  VARIOUS VALUES OF ALPHA          **
!               ***************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     LARGE SAMPLE NORMAL APPROXIMATION VALUES FIRST
!
      CALL NORPPF(.005,CTL005)
      CALL NORPPF(.010,CTL010)
      CALL NORPPF(.025,CTL025)
      CALL NORPPF(.050,CTL050)
      CALL NORPPF(.100,CTL100)
      CALL NORPPF(.200,CTL200)
      CALL NORPPF(.500,CTL500)
      CALL NORPPF(.500,CTU500)
      CALL NORPPF(.800,CTU800)
      CALL NORPPF(.900,CTU900)
      CALL NORPPF(.950,CTU950)
      CALL NORPPF(.975,CTU975)
      CALL NORPPF(.990,CTU990)
      CALL NORPPF(.995,CTU995)
!
!     NOW GENERATE EXACT CRITICAL VALUES FROM CONOVER TABLES
!     IF THERE ARE NO TIES AND N1 AND N2 ARE BOTH <= 20.
!
      ITAB=0
      IF(N1.LE.20 .AND. N2.LE.20)THEN
        ITAB=1
        CALL DPMNN6(N1,N2,   &
                    CVL001,CVL005,CVL010,CVL025,CVL050,CVL100,   &
                    IBUGA3,ISUBRO)
!
        AN1=REAL(N1)
        AN2=REAL(N2)
        CONST=AN1*(AN1+AN2+1.0)
        CVU999=CONST - CVL001
        CVU995=CONST - CVL005
        CVU990=CONST - CVL010
        CVU975=CONST - CVL025
        CVU950=CONST - CVL050
        CVU900=CONST - CVL100
      ENDIF
!
!               *************************************************
!               **   STEP 22--                                 **
!               **   WRITE OUT EVERYTHING                      **
!               **   FOR A SIEGEL TUKEY TEST                   **
!               *************************************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
      IF(ICASAN.EQ.'LOWE')THEN
        ITITLE='Two Sample Lower-Tailed Siegel Tukey Test'
        NCTITL=41
      ELSEIF(ICASAN.EQ.'UPPE')THEN
        ITITLE='Two Sample Upper-Tailed Siegel Tukey Test'
        NCTITL=41
      ELSE
        ITITLE='Two Sample Two-Sided Siegel Tukey Test'
        NCTITL=38
      ENDIF
      IF(ISGTMC.EQ.'ON')THEN
        ITITLZ='(Median Centering Applied)'
        NCTITZ=27
      ELSE
        ITITLZ=' '
        NCTITZ=0
      ENDIF
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='First Response Variable: '
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(30:33),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
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
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: Sigma1 = Sigma2'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(ICASAN.EQ.'LOWE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Sigma1 < Sigma2'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSEIF(ICASAN.EQ.'UPPE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Sigma1 > Sigma2'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Sigma1 not equal Sigma2'
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
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
      ITEXT(ICNT)='Number of Observations for Sample 1:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Mean for Sample 1:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=YMEAN1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Median for Sample 1:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=YMED1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Standard Deviation for Sample 1:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=YSD1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations for Sample 2:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=REAL(N2)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Mean for Sample 2:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=YMEAN2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Median for Sample 2:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=YMED2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Standard Deviation for Sample 2:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=YSD2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IF(ITAB.EQ.1)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Test (Small Sample, Exact):'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Test (Normal Approximation):'
        NCTEXT(ICNT)=30
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic Value:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASAN.EQ.'LOWE')THEN
        IF(ITAB.EQ.0)THEN
          ITITLE='Lower-Tailed Test: Normal Approximation'
          NCTITL=39
        ELSE
          ITITLE='Lower-Tailed Test: Exact - Small Sample'
          NCTITL=39
        ENDIF
      ELSEIF(ICASAN.EQ.'UPPE')THEN
        IF(ITAB.EQ.0)THEN
          ITITLE='Upper-Tailed Test: Normal Approximation'
          NCTITL=39
        ELSE
          ITITLE='Upper-Tailed Test: Exact - Small Sample'
          NCTITL=39
        ENDIF
      ELSE
        IF(ITAB.EQ.0)THEN
          ITITLE='Two-Tailed Test: Normal Approximation'
          NCTITL=37
        ELSE
          ITITLE='Two-Tailed Test: Exact - Small Sample'
          NCTITL=37
        ENDIF
      ENDIF
      ITITL9='H0: sigma1 = sigma2'
      NCTIT9=45
!
      DO 2130 J=1,NUMCLI
        DO 2140 I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2140   CONTINUE
 2130 CONTINUE
!
      IF(ITAB.EQ.0)THEN
        NUMCOL=4
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
      ELSE
        NUMCOL=5
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
        ITITL2(1,3)='Lower'
        NCTIT2(1,3)=5
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (<)'
        NCTIT2(3,3)=9
!
        ITITL2(1,4)='Upper'
        NCTIT2(1,4)=5
        ITITL2(2,4)='Critical'
        NCTIT2(2,4)=8
        ITITL2(3,4)='Value (>)'
        NCTIT2(3,4)=9
!
        ITITL2(1,5)='Null'
        NCTIT2(1,5)=4
        ITITL2(2,5)='Hypothesis'
        NCTIT2(2,5)=10
        ITITL2(3,5)='Conclusion'
        NCTIT2(3,5)=10
!
      ENDIF
!
      NMAX=0
      DO 2150 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(ITAB.EQ.0)THEN
          IF(I.EQ.1 .OR. I.EQ.4)THEN
            ITYPCO(I)='ALPH'
          ENDIF
        ELSE
          IF(I.EQ.1 .OR. I.EQ.5)THEN
            ITYPCO(I)='ALPH'
          ENDIF
        ENDIF
 2150 CONTINUE
!
      IWHTML(1)=125
      IWHTML(2)=175
      IWHTML(3)=175
      IWHTML(4)=175
      IWHTML(5)=175
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
!
      IF(ITAB.EQ.0)THEN
        ICNT=NUMAL2
        DO 2160 J=1,NUMAL2
!
          AMAT(J,2)=STATVA
          ALPHAT=ALPHA2(J)
          ATEMP=(1.0 - ALPHAT)/2.0
          ATEMP=1.0 - ATEMP
          CALL NORPPF(ATEMP,CUTTMP)
          AMAT(J,3)=CUTTMP
          IVALUE(J,4)(1:6)='REJECT'
          IF(ABS(STATVA).LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 2160   CONTINUE
      ELSE
        ICNT=NUMAL2
        DO 3160 J=1,ICNT
!
          AMAT(J,2)=STATVA
          ALPHAT=ALPHA2(J)
          IF(J.EQ.1)THEN
            AMAT(J,3)=CVL100
            AMAT(J,4)=CVU900
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CVL050
            AMAT(J,4)=CVU950
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CVL025
            AMAT(J,4)=CVU975
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CVL005
            AMAT(J,4)=CVU995
          ENDIF
          IVALUE(J,5)(1:6)='ACCEPT'
          IF(STATVA.LT.AMAT(J,3))THEN
            IVALUE(J,5)(1:6)='REJECT'
          ELSEIF(STATVA.GT.AMAT(J,4))THEN
            IVALUE(J,5)(1:6)='REJECT'
          ENDIF
          NCVALU(J,5)=6
!
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 3160   CONTINUE
      ENDIF
!
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
!
      IF(ICASAN.EQ.'TWOT')THEN
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
      IF(ICASAN.EQ.'LOWE')THEN
!
        IF(ITAB.EQ.0)THEN
          ITITLE='Lower-Tailed Test: Normal Approximation'
          NCTITL=39
        ELSE
          ITITLE='Lower-Tailed Test: Exact - Small Sample'
          NCTITL=39
        ENDIF
        ITITL9='H0: F(x) = G(x); Ha: F(x) < G(x)  for some x'
        NCTIT9=44
!
        IF(ITAB.EQ.0)THEN
          ITITL2(2,3)='Critical'
          NCTIT2(2,3)=8
          ITITL2(3,3)='Value (<)'
          NCTIT2(3,3)=9
          NUMCOL=4
        ELSE
          NUMCOL=4
          ITITL2(1,3)='Lower'
          NCTIT2(1,3)=5
          ITITL2(2,3)='Critical'
          NCTIT2(2,3)=8
          ITITL2(3,3)='Value (<)'
          NCTIT2(3,3)=9
!
          ITITL2(1,4)='Null'
          NCTIT2(1,4)=4
          ITITL2(2,4)='Hypothesis'
          NCTIT2(2,4)=10
          ITITL2(3,4)='Conclusion'
          NCTIT2(3,4)=10
        ENDIF
!
        NMAX=0
        DO 2250 I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 2250   CONTINUE
        ITYPCO(4)='ALPH'
!
        IF(ITAB.EQ.0)THEN
          ICNT=NUMALP
          DO 2260 J=1,NUMALP
!
            AMAT(J,2)=STATVA
            ALPHAT=ALPHA(J)
            ATEMP=(1.0 - ALPHAT)
            CALL NORPPF(ATEMP,CUTTMP)
            AMAT(J,3)=CUTTMP
            AMAT(J,4)=0.0
            IVALUE(J,4)(1:6)='ACCEPT'
            IF(ABS(STATVA).LT.AMAT(J,3))THEN
              IVALUE(J,4)(1:6)='REJECT'
            ENDIF
            NCVALU(J,4)=6
            WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
            IVALUE(J,1)(5:5)='%'
            NCVALU(J,1)=5
 2260     CONTINUE
        ELSE
          ICNT=NUMALP
          DO 3260 J=1,ICNT
!
            AMAT(J,2)=STATVA
            ALPHAT=ALPHA(J)
            IF(J.EQ.1)THEN
              AMAT(J,3)=CVL100
            ELSEIF(J.EQ.2)THEN
              AMAT(J,3)=CVL050
            ELSEIF(J.EQ.3)THEN
              AMAT(J,3)=CVL025
            ELSEIF(J.EQ.4)THEN
              AMAT(J,3)=CVL010
            ELSEIF(J.EQ.5)THEN
              AMAT(J,3)=CVL005
            ELSEIF(J.EQ.6)THEN
              AMAT(J,3)=CVL001
            ENDIF
            AMAT(J,4)=0.0
            IVALUE(J,4)(1:6)='ACCEPT'
            IF(STATVA.LT.AMAT(J,3))THEN
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,4)=6
!
            WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
            IVALUE(J,1)(5:5)='%'
            NCVALU(J,1)=5
 3260     CONTINUE
        ENDIF
!
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
      ENDIF
!
      IF(ICASAN.EQ.'UPPE')THEN
!
        IF(ITAB.EQ.0)THEN
          ITITLE='Upper-Tailed Test: Normal Approximation'
          NCTITL=39
        ELSE
          ITITLE='Upper-Tailed Test: Exact - Small Sample'
          NCTITL=39
        ENDIF
        ITITL9='H0: F(x) = G(x); Ha: F(x) > G(x)  for some x'
        NCTIT9=44
!
        IF(ITAB.EQ.0)THEN
          ITITL2(2,3)='Critical'
          NCTIT2(2,3)=8
          ITITL2(3,3)='Value (>)'
          NCTIT2(3,3)=9
          NUMCOL=4
        ELSE
          NUMCOL=4
          ITITL2(1,3)='Upper'
          NCTIT2(1,3)=5
          ITITL2(2,3)='Critical'
          NCTIT2(2,3)=8
          ITITL2(3,3)='Value (>)'
          NCTIT2(3,3)=9
!
          ITITL2(1,4)='Null'
          NCTIT2(1,4)=4
          ITITL2(2,4)='Hypothesis'
          NCTIT2(2,4)=10
          ITITL2(3,4)='Conclusion'
          NCTIT2(3,4)=10
        ENDIF
!
        NMAX=0
        DO 2350 I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 2350   CONTINUE
        ITYPCO(4)='ALPH'
!
        IF(ITAB.EQ.0)THEN
          ICNT=NUMALP
          DO 2360 J=1,NUMALP
!
            AMAT(J,2)=STATVA
            ALPHAT=ALPHA(J)
            ATEMP=ALPHAT
            CALL NORPPF(ATEMP,CUTTMP)
            AMAT(J,3)=CUTTMP
            AMAT(J,4)=0.0
            IVALUE(J,4)(1:6)='ACCEPT'
            IF(ABS(STATVA).GT.AMAT(J,3))THEN
              IVALUE(J,4)(1:6)='REJECT'
            ENDIF
            NCVALU(J,4)=6
            WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
            IVALUE(J,1)(5:5)='%'
            NCVALU(J,1)=5
 2360     CONTINUE
        ELSE
          ICNT=NUMALP
          DO 3360 J=1,ICNT
!
            AMAT(J,2)=STATVA
            ALPHAT=ALPHA(J)
            IF(J.EQ.1)THEN
              AMAT(J,3)=CVU900
            ELSEIF(J.EQ.2)THEN
              AMAT(J,3)=CVU950
            ELSEIF(J.EQ.3)THEN
              AMAT(J,3)=CVU975
            ELSEIF(J.EQ.4)THEN
              AMAT(J,3)=CVU990
            ELSEIF(J.EQ.5)THEN
              AMAT(J,3)=CVU995
            ELSEIF(J.EQ.6)THEN
              AMAT(J,3)=CVU999
            ENDIF
            AMAT(J,4)=0.0
            IVALUE(J,4)(1:6)='ACCEPT'
            IF(STATVA.GT.AMAT(J,3))THEN
              IVALUE(J,5)(1:6)='REJECT'
            ENDIF
            NCVALU(J,4)=6
!
            WRITE(IVALUE(J,1)(1:4),'(F4.1)')100.0*ALPHAT
            IVALUE(J,1)(5:5)='%'
            NCVALU(J,1)=5
 3360     CONTINUE
        ENDIF
!
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
      ENDIF
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SGT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSGT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)STATVA,STATCD,PVAL2T,PVALLT,PVALUT
 9013   FORMAT('STATVA,STATCD,PVAL2T,PVALLT,PVALUT = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSGT2
      SUBROUTINE DPSGT3(Y1,N1,Y2,N2,YRANK,TAG,TEMP1,MAXNXT,   &
                        STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE SIEGEL-TUKEY DISPERSION
!              TEST.  NOTE THAT THIS TEST IS BASED ON THE FOLLOWING:
!
!              1. COMBINE THE DATA FROM THE 2 DATASETS AND SORT
!                 FROM SMALLEST TO LARGEST.
!
!              2. ASSIGN RANKS AS FOLLOWS:
!
!                 A. ASSIGN RANK 1 TO THE SMALLEST OBSERVATION,
!                    RANK 2 TO THE LARGEST OBSERVATION, RANK 3
!                    TO THE NEXT LARGEST OBSERVATION, AND RANK 4
!                    TO THE SMALLEST OBSERVATION.  CONTINUE THIS
!                    PATTERN UNTIL ALL VALUES ARE RANKED.
!
!                 B. PERFORM A MANN WHITNEY RANK SUM TEST ON THESE
!                    RANKED OBSERVATIONS.
!
!     EXAMPLE--SIEGEL TUKEY TEST Y1 Y2
!              SAMPLE 1 IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS)
!              SAMPLE 2 IS IN INPUT VECTOR Y2 (WITH N2 OBSERVATIONS).
!     REFERENCE--HIGGINS (2004), "INTRODUCTION TO MODERN NONPARAMETRIC
!                STATISTICS", DUXBURY PRESS, PP. 52-53.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023/06
!     ORIGINAL VERSION--JUNE      2023.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
      DIMENSION YRANK(*)
      DIMENSION TAG(*)
      DIMENSION TEMP1(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSG'
      ISUBN2='T3  '
      IERROR='NO'
      IWRITE='OFF'
!
      STATVA=CPUMIN
      STATV1=CPUMIN
      STATV2=CPUMIN
      STATV3=CPUMIN
      STATCD=CPUMIN
      PVAL2T=CPUMIN
      PVALLT=CPUMIN
      PVALUT=CPUMIN
!
      DO 10 II=1,MAXNXT
        YRANK(II)=0.0
        TAG(II)=0.0
        TEMP1(II)=0.0
   10 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSGT3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2
   52   FORMAT('IBUGA3,ISUBRO,N1,N2 = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MAX(N1,N2)
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LE.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN SIEGEL-TUKEY TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',   &
               'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE MUST BE 4 OR LARGER.  SUCH WAS NOT THE ',   &
               'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N1
  117   FORMAT('      THE NUMBER OF OBSERVATIONS   = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N2.LE.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,122)
  122   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',   &
               'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N1+N2.LT.10 .OR. N1+N2.GT.MAXNXT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,132)
  132   FORMAT('      THE  COMBINED NUMBER OF OBSERVATIONS FOR THE ',   &
               'TWO RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      VARIABLES MUST BE AT LEAST 10 AND LESS THAN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,134)MAXNXT
  134   FORMAT('      OR EQUAL TO ',I10,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,135)N1
  135   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST  ',   &
               'RESPONSE VARIABLE  = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,137)N2
  137   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',   &
               'RESPONSE VARIABLE  = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y1(1)
      DO 145 I=2,N1
        IF(Y1(I).NE.HOLD)GO TO 149
  145 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,141)HOLD
  141 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
  149 CONTINUE
!
      HOLD=Y2(1)
      DO 155 I=2,N1
        IF(Y2(I).NE.HOLD)GO TO 159
  155 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,151)HOLD
  151 FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
  159 CONTINUE
!
!               ************************************
!               **   STEP 2--                     **
!               **   CREATE COMBINED DATA SET AND **
!               **   BASIC TEST STATISTIC         **
!               ************************************
!
!     TWO ASSUMPTIONS:
!
!        1. THE NUMBER OF COMBINED POINTS IS EVEN.  IF NOT,
!           DELETE THE MEDIAN VALUE BEFORE GENERATING THE
!           SIEGEL-TUKEY RANKS.
!
!        2. THE COMBINED DATA IS SORTED BEFORE DETERMINING THE
!           RANKS.  IF THERE ARE TIES IN THE DATA, DIFFERENT
!           SORTING ALGORITHMS COULD RESULT IN DIFFERENT RANKINGS.
!           DATAPLOT WILL PERFORM THE TEST IF THERE ARE TIES, BUT
!           SOME CAUTION SHOULD BE USED IN INTERPRETING THE RESULTS.
!
!        3. IT IS ASSUMED THAT THERE ARE AT LEAST 10 OBSERVATIONS
!           IN THE COMBINED DATA.  THIS IS CHECKED FOR IN THE
!           DPSGTU CALLING ROUTINE.
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 205 II=1,N1
        TAG(II)=1.0
  205 CONTINUE
      DO 210 II=1,N2
        Y1(N1+II)=Y2(II)
        TAG(N1+II)=2.0
  210 CONTINUE
      N=N1+N2
!
!     IN ORDER TO ACCOMODATE TIES, CONVERT THE DATA
!     TO RANKS FIRST.
!
      CALL RANK(Y1,N,IWRITE,Y1,TEMP1,MAXNXT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      CALL SORTC(Y1,TAG,N,Y1,TEMP1)
      DO 230 I=1,N
        TAG(I)=TEMP1(I)
  230 CONTINUE
!
      NTEMP=MOD(N,2)
      IF(NTEMP.EQ.1)THEN
        NOMIT=(N/2) + 1
        DO 220 I=NOMIT+1,N
          Y1(I-1)=Y1(I)
          TAG(I-1)=TAG(I)
  220   CONTINUE
        N=N-1
      ENDIF
!
      NTEMP=N/2
      ICNT=1
      YRANK(ICNT)=1.0
      NLOOP=NTEMP - (NTEMP/2) - 1
      DO 260 I=1,NLOOP
        IVAL=INT(YRANK(ICNT)+0.01)
        IVAL=IVAL+3
        ICNT=ICNT+1
        YRANK(ICNT)=REAL(IVAL)
        IVAL=IVAL+1
        ICNT=ICNT+1
        YRANK(ICNT)=REAL(IVAL)
  260 CONTINUE
      ICNT=ICNT+1
      YRANK(ICNT)=REAL(N)
      ICNT=N
      YRANK(ICNT)=2.0
      ICNT=N-1
      YRANK(ICNT)=3.0
      NLOOP=NTEMP - (NTEMP/2) - 1
      DO 270 I=1,NLOOP
        IVAL=INT(YRANK(ICNT)+0.01)
        IVAL=IVAL + 3
        ICNT=ICNT - 1
        YRANK(ICNT)=REAL(IVAL)
        ICNT=ICNT - 1
        IVAL=IVAL + 1
        YRANK(ICNT)=REAL(IVAL)
  270 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT3')THEN
        WRITE(ICOUT,291)
  291   FORMAT('**** AFTER SIEGEL-TUKEY RANKING')
        CALL DPWRST('XXX','WRIT')
        DO 293 I=1,N
          WRITE(ICOUT,295)I,Y1(I),TAG(I),YRANK(I)
  295     FORMAT('I,Y1(I),TAG(I),YRANK(I) = ',I10,G15.7,2F15.1)
          CALL DPWRST('XXX','WRIT')
  293   CONTINUE
      ENDIF
!
!               ************************************
!               **   STEP 3--                     **
!               **   COMPUTE RANK SUM TEST.       **
!               ************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     THE COMPUTED TEST STATISTIC DEPENDS ON WHETHER OR NOT THERE ARE
!     NO TIES IN THE RANKS.
!
!        1) CASE WHERE THERE ARE NO TIES:
!
!               T = SUM[i=1 to N1][R(Y1(i))]
!
!            WHERE R() IS THE RANK FROM THE COMBINED SAMPLE
!
!        2) CASE WHERE THERE ARE MANY TIES IN THE DATA:
!
!               T1 = (T - N1*(N1+N2+1)/2)/
!                    SQRT((N1*N2/((N1+N2)*N1+N2-1))*SUM[i=1 to N1+N2][R(i)**2] -
!                    (N1*N2*(N1+N2+1)**2/(4*(N1+N2-1))
!
!     SO CHECK FOR NUMBER OF TIED RANKS.  BASICALLY, IF THE RANK IS
!     A NON-INTEGER VALUE, THIS IMPLIES THAT IT IS A TIED RANK.
!
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MNN3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NTIES=0
      RSUM=0.0
      DO 1210 I=1,N
        ARANK=YRANK(I)
        RSUM=RSUM + ARANK**2
        ITEMP=INT(ARANK)
        ATEMP=ARANK - REAL(ITEMP)
        IF(ABS(ATEMP).GE.0.1)THEN
          NTIES=NTIES+1
        ENDIF
 1210 CONTINUE
!
      T=0.0
      DO 1230 I=1,N
        IF(TAG(I).LT.1.5)T=T+YRANK(I)
 1230 CONTINUE
      STATV1=T
      AN1=REAL(N1)
      AN2=REAL(N2)
      AN=REAL(N1 + N2)
      C1=AN1*(AN+1.0)/2.0
      C2=AN1*AN2/(AN*(AN-1.0))
      C3=AN1*AN2*(AN+1.0)**2/(4.0*(AN-1.0))
      T1=(T - C1)/SQRT(C2*RSUM - C3)
      STATV2=T1
      STATV3=AN1*AN2 + 0.5*AN1*(AN1 + 1.0) - T
!
      IF(NTIES.EQ.0 .AND. N1.LE.20 .AND. N2.LE.20)THEN
        STATVA=STATV1
        TP=AN1*(AN + 1.0)*T
        TVAL=MIN(T,TP)
        ANUM=TVAL + 0.5 -AN1*(AN+1.0)/2.0
        ADEN=SQRT(AN1*AN2*(AN+1.0)/12.0)
        AVAL=ANUM/ADEN
        CALL NORCDF(AVAL,CDF)
        PVAL2T=2.0*CDF
        IF(AVAL.LE.0.0)THEN
          PVAL2T=2.0*CDF
        ELSE
          PVAL2T=2.0*(1.0 - CDF)
        ENDIF
        ANUM=T + 0.5 -AN1*(AN+1.0)/2.0
        ADEN=SQRT(AN1*AN2*(AN+1.0)/12.0)
        AVAL=ANUM/ADEN
        CALL NORCDF(AVAL,CDF)
        PVALLT=CDF
        ANUM=TP + 0.5 -AN1*(AN+1.0)/2.0
        ADEN=SQRT(AN1*AN2*(AN+1.0)/12.0)
        AVAL=ANUM/ADEN
        CALL NORCDF(AVAL,CDF)
        PVALUT=CDF
        STATCD=PVALLT
      ELSE
        STATVA=STATV2
        CALL NORCDF(T1,VAL1)
        VAL2=1.0 - VAL1
        PVALLT=VAL1
        PVALUT=VAL2
        STATCD=VAL1
        IF(T1.LE.0.0)THEN
          PVAL2T=2.0*PVALLT
        ELSE
          PVAL2T=2.0*PVALUT
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SGT3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSGT3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
        IF(IERROR.EQ.'NO')THEN
          WRITE(ICOUT,9013)RSUM,STATVA,STATV1,STATCD
 9013     FORMAT('RSUM,STATVA,STATV1,STATCD = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,9014)PVALLT,PVALUT,PVAL2T
 9014     FORMAT('PVALLT,PVALUT,PVAL2T = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPSGT3
      SUBROUTINE DPSHI3(IANS,IWIDTH,ITARWD,IANS2,IWIDT2)
!
!     NOTE--THIS SUBROUTINE IS IDENTICAL TO THE    DPSHI2   SUBROUTINE
!           AND HAS BEEN CREATED TO ACHIEVE STORAGE ECONOMY IN MAPPING.
!
!     PURPOSE--SEARCH THE VECTOR IANS(.) FOR THE
!              ITARWD-TH WORD.  FORM THE VECTOR
!              IANS2(.) WHICH IS THE SAME AS IANS(.)
!              EXCEPT ALL CHARACTERS UP TO THE BEGINNING
!              OF THE ITARWD-TH WORD HAS BEEN OMITTED.
!              THE VECTOR IANS2(.) THUS BEGINS
!              WITH THE ITARWD-TH WORD.
!     INPUT  ARGUMENTS--IANS   (A  HOLLERITH VECTOR WHOSE
!                              I-TH ELEMENT CONTAINS THE
!                              I-TH CHARACTER OF THE
!                              ORIGINAL INPUT COMMAND LINE.
!                     --IWIDTH (AN INTEGER VARIABLE WHICH
!                              CONTAINS THE NUMBER OF CHARACTERS
!                              IN THE ORIGINAL COMMAND LINE.
!                     --ITARWD (AN INTEGER VARIABLE WHICH
!                              CONTAINS THE NUMBER OF THE WORD
!                              WHICH IS BEING SEARCHED FOR.
!     OUTPUT ARGUMENTS--IANS2  (A  HOLLERITH VECTOR WHOSE
!                              I-TH ELEMENT CONTAINS THE
!                              I-TH CHARACTER OF THE
!                              SHIFTED COMMAND LINE.
!                     --IWIDT2 (AN INTEGER VARIABLE WHICH
!                              CONTAINS THE NUMBER OF CHARACTERS
!                              IN THE SHIFTED COMMAND LINE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IANS
      CHARACTER*4 IANS2
!
!---------------------------------------------------------------------
!
      DIMENSION IANS(*)
      DIMENSION IANS2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               **********************************
!               **  STEP 1--                    **
!               **  SEARCH FOR THE FIRST BLANK  **
!               **********************************
!
      IF(ISUBG4.EQ.'SHI3')THEN
        WRITE(ICOUT,52)IWIDTH,ITARWD
   52   FORMAT('IWIDTH,ITARWD = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 100 I=1,IWIDTH
        I2=I
        IF(IANS(I).EQ.' ')GO TO 190
  100 CONTINUE
      I2=IWIDTH+1
  190 CONTINUE
!
!               *************************************
!               **  STEP 2--                       **
!               **  SEARCH FOR THE NEXT NON-BLANK  **
!               *************************************
!
      IMIN=I2+1
      IMAX=IWIDTH
      IF(IMIN.GT.IMAX)GO TO 250
      DO 200 I=IMIN,IMAX
        I3=I
        IF(IANS(I).NE.' ')GO TO 290
  200 CONTINUE
      I3=IWIDTH+1
      GO TO 290
  250 CONTINUE
      I3=IWIDTH+1
      GO TO 290
  290 CONTINUE
!
!               ***********************************
!               **  STEP 3--                     **
!               **  COMPUTE IANS2(.) AND IWIDT2  **
!               ***********************************
!
      J=0
      IMIN=I3
      IMAX=IWIDTH
      IF(IMIN.GT.IMAX)GO TO 350
      DO 300 I=IMIN,IMAX
        J=J+1
        IANS2(J)=IANS(I)
  300 CONTINUE
  350 CONTINUE
      IWIDT2=J
!
      RETURN
      END SUBROUTINE DPSHI3
      SUBROUTINE DPSHPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IANGLU,MAXNPP,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--FORM A SHIFT PLOT
!              PLOT Y(Q) - X(Q) VERSUS X(Q)
!              WHERE X(Q) IS THE QTH QUANTILE OF X AND Y(Q)
!              IS THE CORRESPONDING QTH QUANTILE OF Y.
!              SUPPORT TWO VARIATIONS.  IF THREE VARIABLES ARE
!              SPECIFIED, THEN THE THIRD VARIABLE ARE THE QUANTILES
!              THAT ARE PLOTTED.  IF ONLY TWO VARIABLES ARE
!              SPECIFIED, THEN COMPUTE THE QUANTILES CORRESPONDING
!              TO X VALUES.  THE X IS THE DATA FOR A CONTROL GROUP
!              AND Y IS THE DATA FROM AN EXPERIMENTAL GROUP.
!     REFERENCE--"INTRODUCTION TO ROBUST ESTIMATION AND HYPOTHESIS
!                TESTING", RAND R. WILCOX, ACADEMIC PRESS, 1997.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/2
!     ORIGINAL VERSION--FEBRUARY  2003.
!     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3
!     UPDATED         --FEBRUARY  2011. SUPPORT FOR "HIGHLIGHTED" OPTION
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
      ISUBN1='DPSH'
      ISUBN2='PL  '
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SHPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSHPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPLOTV,NPLOTP,NS,MAXN,MAXNPP
   52   FORMAT('NPLOTV,NPLOTP,NS,MAXN,MAXNPP = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
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
!               **  TREAT THE SHIFT PLOT            CASE **
!               *******************************************
!
!               ***************************
!               **  STEP 11--            **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IHIGH='OFF'
      IF(ICOM.EQ.'SHIF')THEN
        IF(NUMARG.GE.1 .AND.   &
          (IHARG(1).EQ.'HIGH' .OR. IHARG(1).EQ.'SUBS').AND.   &
          IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
          IHIGH='ON'
        ELSEIF(NUMARG.GE.1 .AND. IHARG(1).EQ.'PLOT')THEN
          ILASTC=1
        ELSE
          GO TO 9000
        ENDIF
      ELSEIF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')THEN
        IHIGH='ON'
        IF(NUMARG.GE.1 .AND.IHARG(1).EQ.'SHIF'.AND.   &
          IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
        ELSE
          GO TO 9000
        ENDIF
      ELSE
        GO TO 9000
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
      ICASPL='SHPL'
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SHPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='SHIFT PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=3
      IF(IHIGH.EQ.'ON')THEN
        MINNVA=3
        MAXNVA=4
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SHPL')THEN
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
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IHIGH.EQ.'ON')THEN
        ICOL=3
        IF(NUMVAR.EQ.3)THEN
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      XHIGH,XHIGH,XHIGH,NHIGH,NTEMP,NTEMP,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
        ELSEIF(NUMVAR.EQ.4)THEN
          ICOL=3
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y3,Y3,Y3,NS3,NS3,NS3,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          ICOL=4
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      XHIGH,XHIGH,XHIGH,NHIGH,NTEMP,NTEMP,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
        ENDIF
        IF(IERROR.EQ.'YES')GO TO 9000
      ELSE
        NHIGH=0
        IF(NUMVAR.EQ.3)THEN
          ICOL=3
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y3,Y3,Y3,NS3,NS3,NS3,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
        ENDIF
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SHPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPSHP2(Y1,NS1,Y2,NS2,Y3,NS3,NUMVAR,ICASPL,MAXN,   &
                  Y4,IQUAME,NHIGH,   &
                  Y,X,D,NPLOTP,NPLOTV,   &
                  IBUGG3,ISUBRO,IERROR)
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SHPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSHPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,ICASPL,IAND1,IAND2 = ',   &
               2I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NS1,NS2,NS3,NHIGH,NUMVAR
 9014   FORMAT('NS1,NS2,NS3,NHIGH,NUMVAR = ',5I8)
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
      END SUBROUTINE DPSHPL
      SUBROUTINE DPSHP2(Y,NY,X,NX,Z,NZ,NUMVAR,ICASPL,MAXN,   &
                        YTEMP,IQUAME,NHIGH,   &
                        Y2,X2,D2,NPLOTP,NPLOTV,   &
                        IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A SHIFT PLOT.  FOR THIS,
!              PLOT Y(Q) - X(Q) VERSUS X(Q) (THAT IS, THE
!              DIFFERENCE IN THE QUANTILES VERSUS THE QUANTILES).
!              WHERE X(Q) IS THE QTH QUANTILE OF X AND Y(Q)
!              IS THE CORRESPONDING QTH QUANTILE OF Y.
!              SUPPORT TWO VARIATIONS.  IF THREE VARIABLES ARE
!              SPECIFIED, THEN THE THIRD VARIABLE ARE THE QUANTILES
!              THAT ARE PLOTTED.  IF ONLY TWO VARIABLES ARE
!              SPECIFIED, THE COMPUTE THE QUANTILES CORRESPONDING
!              TO X VALUES.  THE X IS THE DATA FOR A CONTROL GROUP
!              AND Y IS THE DATA FROM AN EXPERIMENTAL GROUP.
!              SIMILAR PURPOSE TO QUANTILE-QUANTILE OR TUKEY MEAN
!              DIFFERENCE PLOT.
!              TWO CASES:
!                 1) IF ONLY TWO VARIABLES, THEN COMPUTE THE QUANTILES
!                    AT THE X POINTS.
!                 2) IF A THIRD VARIABLE PRESENT, THESE DEFINE THE
!                    DESIRED QUANTILES.
!     REFERENCE--"INTRODUCTION TO ROBUST ESTIMATION AND HYPOTHESIS
!                TESTING", RAND R. WILCOX, ACADEMIC PRESS, 1997.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/3
!     ORIGINAL VERSION--FEBRUARY  2003.
!     UPDATED         --FEBRUARY  2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IQUAME
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASE
      CHARACTER*4 ICASPL
      CHARACTER*4 IQUAM2
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Z(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION YTEMP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSH'
      ISUBN2='P2  '
      IERROR='NO'
!
      ICASE=ICASPL
      IQUAM2=IQUAME
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SHP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSHP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,ICASPL
   52   FORMAT('IBUGG3,ISUBRO,ICASPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MAXN,NX,NY,NZ,NUMVAR
   53   FORMAT('MAXN,NX,NY,NZ,NUMVAR = ',5I8)
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
   72       FORMAT('I,X(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   71     CONTINUE
        ENDIF
        IF(NZ.GE.1)THEN
          DO 81 I=1,NZ
            WRITE(ICOUT,72)I,Z(I)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(NY.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN SHIFT PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',   &
               'RESPONSE VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)
 1113   FORMAT('      MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NY
 1114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NX.LT.2)THEN
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
      ELSEIF((NHIGH.EQ.0.AND.NUMVAR.EQ.3.AND.NZ.LT.2) .OR.   &
             (NHIGH.GT.0.AND.NUMVAR.EQ.4.AND.NZ.LT.2))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1162)
 1162   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE QUANTILE ',   &
               'RESPONSE VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NZ
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
      HOLD=Y(1)
      DO 1130 I=1,NY
        IF(Y(I).NE.HOLD)GO TO 1139
 1130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ALL INPUT ELEMENTS FOR THE FIRST RESPONSE VARIABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1139 CONTINUE
!
      HOLD=X(1)
      DO 1140 I=1,NY
        IF(X(I).NE.HOLD)GO TO 1149
 1140 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      ALL INPUT ELEMENTS FOR THE SECOND RESPONSE ',   &
             'VARIABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1133)HOLD
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1149 CONTINUE
!
!               ****************************************************
!               **  STEP 21--                                     **
!               **  SORT THE INPUT VARIABLES                      **
!               ****************************************************
!
      CALL SORT(X,NX,X)
      CALL SORT(Y,NY,Y)
      IF(NUMVAR.EQ.3)CALL SORT(Z,NZ,Z)
!
!               *****************************************
!               **  STEP 22--                          **
!               **  CASE 1: THIRD VARIABLE NOT         **
!               **  SPECIFIED (SO BASE QUANTILES ON    **
!               **  THE X VARIABLE).  FOR THIS CASE,   **
!               **  Q = I/N, X(Q) = X(I).              **
!               *****************************************
!
      IWRITE='OFF'
!
      IF(NUMVAR.EQ.2)THEN
        DO 2200 I=1,NX
          Z(I)=REAL(I)/REAL(NX)
 2200   CONTINUE
        IF(NX.EQ.NY)THEN
          DO 2210 I=1,NX
            X2(I)=X(I)
            Y2(I)=Y(I) - X(I)
            D2(I)=1.0
 2210     CONTINUE
          N2=NX
        ELSE
          DO 2260 I=1,NX
            XQUANT=X(I)
            QNT=Z(I)
            CALL QUANT(QNT,Y,NY,IWRITE,YTEMP,MAXN,   &
                       IQUAM2,   &
                       YQUANT,IBUGG3,IERROR)
            X2(I)=X(I)
            Y2(I)=YQUANT - XQUANT
            D2(I)=1.0
 2260     CONTINUE
          N2=NX
        ENDIF
!
!               *****************************************
!               **  STEP 22--                          **
!               **  CASE 2: THIRD VARIABLE WAS         **
!               **  SPECIFIED (SO BASE QUANTILES ON    **
!               **  THIS VARIABLE).                    **
!               *****************************************
!
      ELSE
        DO 2360 I=1,NZ
          QNT=Z(I)
          CALL QUANT(QNT,X,NX,IWRITE,YTEMP,MAXN,   &
                     IQUAM2,   &
                     XQUANT,IBUGG3,IERROR)
          CALL QUANT(QNT,Y,NY,IWRITE,YTEMP,MAXN,   &
                     IQUAM2,   &
                     YQUANT,IBUGG3,IERROR)
          X2(I)=XQUANT
          Y2(I)=YQUANT - XQUANT
          D2(I)=1.0
 2360   CONTINUE
        N2=NZ
      ENDIF
!
      NPLOTP=N2
      NPLOTV=3
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'SHP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSHP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,ICASE,IERROR,N2
 9012   FORMAT('ICASPL,ICASE,IERROR,N2 = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSHP2
      SUBROUTINE DPSIAS(ISTART,IW,NW,IHOUT,NOUT,IBUGA3,IERROR)
!
!     PURPOSE--ADD A STRING INTO IW.
!              THE STRING IS LOCATED IN IHOUT(.).
!              THE LOCATION IN IW(.) WHERE THE STRING
!              IS TO BE INSERTED IS AT ISTART.
!              THE STRING WILL BE INSERTED BETWEEN
!              LOCATIONS ISTART AND ISTART+1.
!              THE PREVIOUS CONTENTS OF LOCATIONS
!              ISTART AND LARGER WILL BE AUTOMATICALLY
!              SHIFTED TO THE RIGHT.
!              THE CONTENTS OF IW(ISTART) WILL NOT BE OVERWRITTEN.
!              THE CONTENTS OF IW(ISTART), IW(ISTART+1), ETC.
!              WILL BE DISPLACED ACCORDING TO THE LENGTH
!              OF THE INSERTED STRING.
!     NOTE--THE INPUT ARGUMENTS IW(.) AND NW
!           AND ALTERED BY THIS SUBROUTINE.
!     NOTE--IF NOUT = 0 OR NEGATIVE, THEN THE CONVENTION
!           HAS BEEN TAKEN TO LEAVE IW(.) AND NW UNCHANGED.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--FEBRUARY  1979.
!     UPDATED         --JUNE      1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IHOUT
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IW(*)
      DIMENSION IHOUT(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      ISHIFT=0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIAS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISTART,NW,NOUT
   52 FORMAT('ISTART,NW,NOUT = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)(IHOUT(I),I=1,MIN(NOUT,100))
   53 FORMAT('(IHOUT(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)(IW(I),I=1,MIN(NW,100))
   54 FORMAT('(IW(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               *****************************
!               **  STEP 1--               **
!               **  INSERT    THE STRING.  **
!               *****************************
!
      IF(NOUT.GT.0)ISHIFT=NOUT
      IF(NOUT.LE.0)ISHIFT=0
      IMIN=ISTART+1
      IMAX=NW
      IF(IMIN.GT.IMAX)GO TO 150
      DO 100 I=IMIN,IMAX
      IPS=I+ISHIFT
      IREV=IMAX-I+IMIN
      IREVPS=IREV+ISHIFT
      IF(IREVPS.GE.IREV)IW(IREVPS)=IW(IREV)
      IF(IREVPS.LT.IREV)IW(IPS)=IW(I)
  100 CONTINUE
  150 CONTINUE
      NW=NW+ISHIFT
!
      J=ISTART
      IF(NOUT.LE.0)GO TO 250
      DO 200 I=1,NOUT
      J=J+1
      IW(J)=IHOUT(I)
  200 CONTINUE
  250 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END        OF DPSIAS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NW
 9012   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)(IW(I),I=1,MIN(NW,100))
 9013   FORMAT('(IW(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIAS
      SUBROUTINE DPSIA0(IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--ELIMINATE SUPERFLUOUS ADDITIONS
!              (AND SUBTRACTIONS) BY 0 AND BY (0)   .
!     NOTE--THE INPUT ARGUEMNTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IMIN=1
      I2=1
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIA0--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.) **
!               **  FOR THE SEARCH FOR    0    .       **
!               *****************************************
!
      NUMPAS=1000
      DO 100 IPASS=1,NUMPAS
!
!               ****************************
!               **  STEP 2--              **
!               **  SEARCH FOR    0    .  **
!               ****************************
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=I2+1
      IF(IMIN.GT.NW)GO TO 990
      DO 200 I=IMIN,NW
      I2=I
      IF(IW(I).EQ.'0   ')GO TO 210
  200 CONTINUE
      GO TO 990
!
  210 CONTINUE
      I=I2
      IM1=I-1
      IP1=I+1
!
!               ***********************************
!               **  STEP 3--                     **
!               **  TEST FOR THE    +0    CASE.  **
!               ***********************************
!
      IF(IM1.LT.1)GO TO 390
      IF(IW(IM1).EQ.'+   ')GO TO 310
      IF(IW(IM1).EQ.'-   ')GO TO 310
      GO TO 100
!
  310 CONTINUE
      IF(IP1.GT.NW)GO TO 330
      IF(IW(IP1).EQ.'+   ')GO TO 330
      IF(IW(IP1).EQ.'-   ')GO TO 330
      IF(IW(IP1).EQ.')   ')GO TO 320
      GO TO 100
!
  320 CONTINUE
      IM2=I-2
      IF(IM2.LE.0)GO TO 100
      IF(IW(IM2).EQ.'(   ')GO TO 325
      GO TO 330
!
  325 CONTINUE
      ISTART=IM1
      ISTOP=IM1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 100
!
  330 CONTINUE
      ISTART=IM1
      ISTOP=I
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 100
!
  390 CONTINUE
!
!               ***********************************
!               **  STEP 4--                     **
!               **  TEST FOR THE    0+    CASE.  **
!               ***********************************
!
      IF(IP1.GT.NW)GO TO 490
      IF(IW(IP1).EQ.'+   ')GO TO 410
      IF(IW(IP1).EQ.'-   ')GO TO 410
      GO TO 100
!
  410 CONTINUE
      IF(IM1.LT.1)GO TO 420
      IF(IW(IM1).EQ.'+   ')GO TO 420
      IF(IW(IM1).EQ.'-   ')GO TO 420
      IF(IW(IM1).EQ.'/   ')GO TO 420
      IF(IW(IM1).EQ.'(   ')GO TO 420
      GO TO 100
!
  420 CONTINUE
      ISTART=I
      ISTOP=IP1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 100
!
  490 CONTINUE
!
  100 CONTINUE
!
  990 CONTINUE
!
!               *****************************************
!               **  STEP 11--                          **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.) **
!               **  FOR THE SEARCH FOR    (0)    .     **
!               *****************************************
!
      NUMPAS=1000
      DO 1100 IPASS=1,NUMPAS
!
!               ****************************
!               **  STEP 12--             **
!               **  SEARCH FOR   (0)   .  **
!               ****************************
!
      IF(IPASS.EQ.1)IMIN=2
      IF(IPASS.GE.2)IMIN=I2+1
      NWM1=NW-1
      IF(IMIN.LT.2)GO TO 1990
      IF(IMIN.GT.NWM1)GO TO 1990
      DO 1200 I=IMIN,NWM1
      I2=I
      IM1=I-1
      IP1=I+1
      IF(IW(IM1).EQ.'(   '.AND.IW(I).EQ.'0   '.AND.   &
         IW(IP1).EQ.')   ')GO TO 1210
 1200 CONTINUE
      GO TO 1990
!
 1210 CONTINUE
      I=I2
      IM1=I-1
      IP1=I+1
      IM2=I-2
      IP2=I+2
!
!               ***********************************
!               **  STEP 13--                    **
!               **  TEST FOR THE   *(0)   CASE.  **
!               ***********************************
!
      IF(IM2.LT.1)GO TO 1390
      IF(IW(IM2).EQ.'+   ')GO TO 1310
      IF(IW(IM2).EQ.'-   ')GO TO 1310
      GO TO 1100
!
 1310 CONTINUE
      IF(IP2.GT.NW)GO TO 1320
      IF(IW(IP2).EQ.'+   ')GO TO 1320
      IF(IW(IP2).EQ.'-   ')GO TO 1320
      IF(IW(IP2).EQ.')   ')GO TO 1320
      GO TO 1100
!
 1320 CONTINUE
      ISTART=IM2
      ISTOP=IP1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 1100
!
 1390 CONTINUE
!
!               ***********************************
!               **  STEP 14--                    **
!               **  TEST FOR THE   (0)*   CASE.  **
!               ***********************************
!
      IF(IP2.GT.NW)GO TO 1490
      IF(IW(IP2).EQ.'+   ')GO TO 1410
      IF(IW(IP2).EQ.'-   ')GO TO 1410
      GO TO 1100
!
 1410 CONTINUE
      IF(IM2.LT.1)GO TO 1420
      IF(IW(IM2).EQ.'+   ')GO TO 1420
      IF(IW(IM2).EQ.'-   ')GO TO 1420
      IF(IW(IM2).EQ.'(   ')GO TO 1420
      GO TO 1100
!
 1420 CONTINUE
      ISTART=IM1
      ISTOP=IP2
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 1100
!
 1490 CONTINUE
!
 1100 CONTINUE
!
 1990 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIA0--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NW
 9012   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NW
          WRITE(ICOUT,9016)I,IW(I)
 9016     FORMAT('I,IW(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIA0
      SUBROUTINE DPSIA2(IW,NW,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--SIMPLIFY AN ENTIRE EXPRESSION BY PERFORMING
!           CERTAIN SIMPLE BINARY ARITHMETIC OPERATIONS
!           INVOLVING INTEGERS AND WITHIN PARENTHESES.
!           IF INTERNAL STRING IS AN INTEGER
!           AND OF LENGTH 1
!           (OR IF INTERNAL STRING IS REDUCABLE
!           TO AN INTEGER OF LENGTH 1)
!           THEN ELIMINATE THE IMMEDIATELY PRECEDING AND
!           THE IMMMEDIATELY TRAILING PARENTHESIS.
!     NOTE--THE INPUT ARGUMENTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IMIN=1
      IRIGHT=1
      ILEFT=1
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'SIA2')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIA2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,ISUBRO,IERROR
   52 FORMAT('IBUGA3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NW
   53 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               *****************************************
!
      NUMPAS=1000
      DO 100 IPASS=1,NUMPAS
      ISUM=0
!
!               **********************************************
!               **  STEP 3--                                **
!               **  SEARCH FOR THE NEXT RIGHT PARENTHESIS.  **
!               **********************************************
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=IRIGHT+1
      IF(IMIN.GT.NW)GO TO 9000
!
      DO 300 I=IMIN,NW
      I2=I
      IF(IW(I).EQ.')   ')GO TO 350
  300 CONTINUE
      GO TO 9000
  350 CONTINUE
      IRIGHT=I2
      ISUM=ISUM+1
!
!               **********************************************
!               **  STEP 4--                                **
!               **  SEARCH FOR THE NEXT (IN REVERSE ORDER)  **
!               **  LEFT PARENTHESIS.                       **
!               **********************************************
!
      IMAX=IRIGHT-1
      IF(IMAX.LT.1)GO TO 9000
!
      DO 400 I=1,IMAX
      IREV=IMAX-I+1
      IF(IW(IREV).EQ.'(   ')GO TO 401
      IF(IW(IREV).EQ.')   ')GO TO 402
      GO TO 400
  401 CONTINUE
      ISUM=ISUM-1
      IF(ISUM.EQ.0)ILEFT=IREV
      IF(ISUM.EQ.0)GO TO 490
      GO TO 400
  402 CONTINUE
      ISUM=ISUM+1
      GO TO 400
  400 CONTINUE
!
      WRITE(ICOUT,411)
  411 FORMAT('***** ERROR IN DPSIA2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,412)
  412 FORMAT('      NUMBER OF LEFT PARENTHESES DOES NOT EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,413)
  413 FORMAT('      NUMBER OF RIGHT PARENTHESES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,414)
  414 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,415)(IW(I),I=1,NW)
  415 FORMAT('      ',115A1)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  490 CONTINUE
!
!               ******************************************
!               **  STEP 5--                            **
!               **  CHECK INTERNAL STRING;              **
!               **  SIMPLIFY IF POSSIBLE.               **
!               ******************************************
!
      ISTART=ILEFT+1
      ISTOP=IRIGHT-1
      CALL DPSIS2(ISTART,ISTOP,IW,NW,IBUGA3,ISUBRO,IERROR)
!
  100 CONTINUE
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'SIA2')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSIA2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IERROR
 9012 FORMAT('IBUGA3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NW
 9013 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NW
      WRITE(ICOUT,9016)I,IW(I)
 9016 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPSIA2
      SUBROUTINE DPSIEP(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--ELIMINATE EXTRA PARENTHESES.
!              GIVEN THAT PARENTHESES EXIST AT LOCATIONS
!              ISTART AND ISTOP
!              (A LEFT PARENTHESIS EXISTS AT LOCATION ISTART;
!              A RIGHT PARENTHESIS EXISTS AT LOCATION ISTOP).
!              WORK OUTWARD FROM  THESE PARENTHESES
!              AND ELIMINATE REDUNDANT PAIRS OF PARENTHESES.
!     NOTE--THE PARENTHESES AT LOCATIONS
!           ISTART AND ISTOP ARE NOT THEMSELVES
!           ELIMINATED.
!     NOTE--THE 5 INPUT ARGUMENTS ARE ALL ALTERED
!           BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED--         FEBRUARY  1979.
!     UPDATED--         JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)---------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIEP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISTART,ISTOP,NW
   52 FORMAT('ISTART,ISTOP,NW = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  DETERMINE THE NUMBER OF EXTRA SETS OF PARENTHESES  **
!               **  (NOT COUNTING THE SET AT ISTART AND ISTOP).        **
!               *********************************************************
!
      NUMEXT=0
      DO 100 I=1,1000
      J1=ISTART-I
      J2=ISTOP+I
      IF(J1.LT.1.OR.J1.GT.NW)GO TO 190
      IF(J2.LT.1.OR.J2.GT.NW)GO TO 190
      IF(IW(J1).EQ.'(   '.AND.IW(J2).EQ.')   ')GO TO 150
      GO TO 190
  150 CONTINUE
      NUMEXT=NUMEXT+1
  100 CONTINUE
  190 CONTINUE
!
!               ***********************************************
!               **  STEP 2--                                 **
!               **  ELIMINATE THE EXTRA SETS OF PARENTHESES  **
!               **  (LEAVING ONLY THE ORIGINAL PAIR).        **
!               ***********************************************
!
      IF(NUMEXT.LE.0)GO TO 290
!
      IMIN=ISTOP+1
      IMAX=ISTOP+NUMEXT
      CALL DPSIES(IMIN,IMAX,IW,NW,IBUGA3,IERROR)
!
      IMIN=ISTART-NUMEXT
      IMAX=ISTART-1
      CALL DPSIES(IMIN,IMAX,IW,NW,IBUGA3,IERROR)
!
      ISTAR2=ISTART-NUMEXT
      IWITHI=ISTOP-ISTART-1
      ISTOP2=ISTAR2+IWITHI+1
!
      ISTART=ISTAR2
      ISTOP=ISTOP2
!
  290 CONTINUE
!
!               ******************************************
!               **  STEP 3--                            **
!               **  CHECK TO SEE IF A SINGLE PAIR (.)   **
!               **  WITH A 1-CHARACTER INTERNAL STRING  **
!               **  CAN BE COLLAPSED TO JUST            **
!               **  THE 1-CHARACTER INTERNAL STRING     **
!               **  (EXAMPLE--(X) TO X    ).            **
!               **  THIS CAN BE DONE PROVIDING THE      **
!               **  PREVIOUS WORD TO (.) IS             **
!               **  NOT A LIBRARY FUNCTION.             **
!               **         ***** CAUTION *****          **
!               **  IF SUCH A REDUCTION IS MADE,        **
!               **  ISTART AND ISTOP WILL END UP        **
!               **  WITH THE SAME VALUE                 **
!               **  AND THIS VALUE WILL BE TECHNICALLY  **
!               **  INCORRECT BECAUSE THERE WILL        **
!               **  BE NEITHER A LEFT PARENTHESES NOR A **
!               **  RIGHT PARENTHESES REMAINING AT      **
!               **  ISTART AND ISTOP--BUT RATHER        **
!               **  ONLY THE 1-CHARACTER INTERNAL       **
!               **  STRING WILL REMAIN THERE.           **
!               **  IF UNEXPLAINED PROBLEMS ARISE       **
!               **  IN SOME OF THE CALLING ROUTINES,    **
!               **  THIS TECHNICALITY MAY BE THE CAUSE  **
!               **  OF THE PROBLEM.                     **
!               ******************************************
!
      ISTAP2=ISTART+2
      IF(ISTOP.EQ.ISTAP2)GO TO 410
      GO TO 490
!
  410 CONTINUE
      ISTAM1=ISTART-1
      IF(ISTAM1.LE.0)GO TO 450
      IF(IW(ISTAM1).EQ.'+   ')GO TO 450
      IF(IW(ISTAM1).EQ.'-   ')GO TO 450
      IF(IW(ISTAM1).EQ.'*   ')GO TO 450
      IF(IW(ISTAM1).EQ.'/   ')GO TO 450
      IF(IW(ISTAM1).EQ.'**  ')GO TO 450
      IF(IW(ISTAM1).EQ.'(   ')GO TO 450
      GO TO 490
!
  450 CONTINUE
      IMIN=ISTOP
      IMAX=ISTOP
      CALL DPSIES(IMIN,IMAX,IW,NW,IBUGA3,IERROR)
!
      IMIN=ISTART
      IMAX=ISTART
      CALL DPSIES(IMIN,IMAX,IW,NW,IBUGA3,IERROR)
!
      ISTOP=ISTART
!
  490 CONTINUE
!
!               ****************
!               **  STEP 4--  **
!               **  EXIT.     **
!               ****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIEP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ISTART,ISTOP,NW
 9012   FORMAT('ISTART,ISTOP,NW = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NW
          WRITE(ICOUT,9016)I,IW(I)
 9016     FORMAT('I,IW(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIEP
      SUBROUTINE DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--SIMPLIFY AN EXPRESSION BY REMOVING
!              THE STRING STARTING WITH ISTART (INCLUSIVE)
!              THROUGH ISTOP (INCLUSIVE).
!     NOTE--THE INPUT ARGUMENTS IW(.) (FOR FIRST 4 CHARACTERS), AND
!           NW (= NUMBER OF ELEMENTS IN IW(.)
!           ARE ALTERED BY THIS SUBROUTINE.
!     NOTE--IT IS PERMISSABLE TO HAVE ISTART AND
!           ISTOP BEING THE SAME--THUS EFFECTIVELY
!           ELIMINATING A STRING OF LENGTH 1.
!     ORIGINAL VERSION--JANUARY  1979.
!     UPDATED         --JANUARY  1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)----------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSIES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ISTART,ISTOP,NW
   52   FORMAT('ISTART,ISTOP,NW = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NW
          WRITE(ICOUT,56)I,IW(I)
   56     FORMAT('I,IW(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *****************************
!               **  STEP 1--               **
!               **  ELIMINATE THE STRING.  **
!               *****************************
!
      J=ISTART-1
      IMIN=ISTOP+1
      IMAX=NW
      IF(IMIN.GT.IMAX)GO TO 150
      DO 100 I=IMIN,IMAX
        J=J+1
        IW(J)=IW(I)
  100 CONTINUE
  150 CONTINUE
      NW=J
!
!               *****************
!               **  STEP 2--   **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NW
 9012   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NW
          WRITE(ICOUT,9016)I,IW(I)
 9016     FORMAT('I,IW(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIES
      SUBROUTINE DPSIE0(IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--ELIMINATE SUPERFLUOUS EXPONENTIATIONS
!              BY 0.
!     NOTE--THE INPUT ARGUEMNTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)----------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='E0  '
      IERROR='NO'
!
      IMIN=1
      I2=1
      KREV=1
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIE0--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               **  FOR THE SEARCH FOR    0    .       **
!               *****************************************
!
      NUMPAS=1000
      DO 100 IPASS=1,NUMPAS
!
!               ****************************
!               **  STEP 2--              **
!               **  SEARCH FOR    0    .  **
!               ****************************
!
      NWM1=NW-1
      NWP1=NW+1
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=I2+1
      IF(IMIN.GE.NWP1)GO TO 990
      DO 200 I=IMIN,NW
      I2=I
      IF(IW(I).EQ.'0   ')GO TO 210
  200 CONTINUE
      GO TO 990
!
  210 CONTINUE
      I=I2
      IM1=I-1
      IM2=I-2
      IM3=I-3
      IP1=I+1
      IP2=I+2
      IP3=I+3
!
      IB=IM1
      IF(IB.LE.0)GO TO 100
      IF(IW(IB).EQ.'**  '.AND.IW(I).EQ.'0   ')GO TO 310
      IB=IM2
      IF(IB.LE.0)GO TO 100
      IF(IW(IB).EQ.'*   '.AND.IW(IM1).EQ.'*   '.AND.   &
      IW(I).EQ.'0   ')GO TO 310
      GO TO 100
!
!               ***********************************
!               **  STEP 3--                     **
!               **  TEST FOR THE   **0    CASE.  **
!               ***********************************
!
  310 CONTINUE
      IF(IP1.GE.NWP1)GO TO 320
      IF(IW(IP1).EQ.'+   ')GO TO 320
      IF(IW(IP1).EQ.'-   ')GO TO 320
      IF(IP1.EQ.NW.AND.IW(IP1).EQ.'*   ')GO TO 320
      IF(IP1.LE.NWM1.AND.IW(IP1).EQ.'*   '.AND.IW(IP2).NE.'*   ')GO TO 320
      IF(IW(IP1).EQ.'/   ')GO TO 320
      IF(IW(IP1).EQ.'(   ')GO TO 320
      GO TO 100
!
  320 CONTINUE
      IRIGHT=IB-1
      IF(IRIGHT.LE.0)GO TO 100
      ILEFT=IRIGHT
      IF(IW(IRIGHT).EQ.')   ')GO TO 333
      GO TO 339
  333 CONTINUE
      ISUM=0
      DO 335 K=1,IRIGHT
      KREV=IRIGHT-K+1
      IF(IW(KREV).EQ.')   ')ISUM=ISUM+1
      IF(IW(KREV).EQ.'(   ')ISUM=ISUM-1
      IF(ISUM.EQ.0)GO TO 337
  335 CONTINUE
      ILEFT=0
  337 CONTINUE
      ILEFT=KREV
  339 CONTINUE
!
      ISTART=ILEFT+1
      ISTOP=I
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      IW(I2)='1   '
      GO TO 100
!
  100 CONTINUE
!
  990 CONTINUE
!
!               *****************************************
!               **  STEP 11--                          **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               **  FOR THE SEARCH FOR    (0)    .     **
!               *****************************************
!
      NUMPAS=1000
      DO 1100 IPASS=1,NUMPAS
!
!               ***************************
!               **  STEP 12--            **
!               **  SEARCH FOR (0)    .  **
!               ***************************
!
      NWM1=NW-1
      NWP1=NW+1
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=I2+1
      IF(IMIN.LE.0)GO TO 1990
      IF(IMIN.GT.NWM1)GO TO 1990
      DO 1200 I=IMIN,NWM1
      I2=I
      IM1=I-1
      IP1=I+1
      IF(IW(IM1).EQ.'(   '.AND.IW(I).EQ.'0   '.AND.   &
         IW(IP1).EQ.')   ')GO TO 1210
 1200 CONTINUE
      GO TO 1990
!
 1210 CONTINUE
      I=I2
      IM1=I-1
      IM2=I-2
      IM3=I-3
      IP1=I+1
      IP2=I+2
      IP3=I+3
!
      IB=IM2
      IF(IB.LE.0)GO TO 1100
      IF(IW(IB).EQ.'**  '.AND.IW(I).EQ.'0   ')GO TO 1310
      IB=IM3
      IF(IB.LE.0)GO TO 1100
      IF(IW(IB).EQ.'*   '.AND.IW(IM2).EQ.'*   '.AND.   &
      IW(I).EQ.'0   ')GO TO 1310
      GO TO 1100
!
!               ***********************************
!               **  STEP 13--                    **
!               **  TEST FOR THE  **(0)   CASE.  **
!               ***********************************
!
 1310 CONTINUE
      IF(IP2.GE.NWP1)GO TO 1320
      IF(IW(IP2).EQ.'+   ')GO TO 1320
      IF(IW(IP2).EQ.'-   ')GO TO 1320
      IF(IP2.EQ.NW.AND.IW(IP2).EQ.'*   ')GO TO 1320
      IF(IP2.LE.NWM1.AND.IW(IP2).EQ.'*   '.AND.IW(IP3).NE.'*   ')   &
      GO TO 1320
      IF(IW(IP2).EQ.'/   ')GO TO 1320
      IF(IW(IP2).EQ.'(   ')GO TO 1320
      GO TO 1100
!
 1320 CONTINUE
      IRIGHT=IB-1
      IF(IRIGHT.LE.0)GO TO 1100
      ILEFT=IRIGHT
      IF(IW(IRIGHT).EQ.')   ')GO TO 1333
      GO TO 1339
 1333 CONTINUE
      ISUM=0
      DO 1335 K=1,IRIGHT
      KREV=IRIGHT-K+1
      IF(IW(KREV).EQ.')   ')ISUM=ISUM+1
      IF(IW(KREV).EQ.'(   ')ISUM=ISUM-1
      IF(ISUM.EQ.0)GO TO 1337
 1335 CONTINUE
      ILEFT=0
 1337 CONTINUE
      ILEFT=KREV
 1339 CONTINUE
!
      ISTART=ILEFT+1
      ISTOP=IP1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      IW(I2)='1   '
      GO TO 1100
!
 1100 CONTINUE
!
 1990 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIE0--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NW
 9012   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NW
          WRITE(ICOUT,9016)I,IW(I)
 9016     FORMAT('I,IW(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIE0
      SUBROUTINE DPSIE1(IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--ELIMINATE SUPERFLUOUS EXPONENTIATIONS
!              BY 1.
!     NOTE--THE INPUT ARGUEMNTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)----------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='E1  '
      IERROR='NO'
!
      IMIN=1
      I2=1
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIE1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               **  FOR THE SEARCH FOR    1    .       **
!               *****************************************
!
      NUMPAS=1000
      DO 100 IPASS=1,NUMPAS
!
!               ****************************
!               **  STEP 2--              **
!               **  SEARCH FOR    1    .  **
!               ****************************
!
      NWM1=NW-1
      NWP1=NW+1
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=I2+1
      IF(IMIN.GE.NWP1)GO TO 990
      DO 200 I=IMIN,NW
      I2=I
      IF(IW(I).EQ.'1   ')GO TO 210
  200 CONTINUE
      GO TO 990
!
  210 CONTINUE
      I=I2
      IM1=I-1
      IM2=I-2
      IM3=I-3
      IP1=I+1
      IP2=I+2
      IP3=I+3
!
!               ***********************************
!               **  STEP 3--                     **
!               **  TEST FOR THE   **1    CASE.  **
!               ***********************************
!
      IB=IM1
      IF(IB.LE.0)GO TO 100
      IF(IW(IB).EQ.'**  '.AND.IW(I).EQ.'1   ')GO TO 310
      IB=IM2
      IF(IB.LE.0)GO TO 100
      IF(IW(IB).EQ.'*   '.AND.IW(IM1).EQ.'*   '.AND.   &
      IW(I).EQ.'1   ')GO TO 310
      GO TO 100
!
  310 CONTINUE
      IF(IP1.GE.NWP1)GO TO 320
      IF(IW(IP1).EQ.'+   ')GO TO 320
      IF(IW(IP1).EQ.'-   ')GO TO 320
      IF(IP1.EQ.NW.AND.IW(IP1).EQ.'*   ')GO TO 320
      IF(IP1.LE.NWM1.AND.IW(IP1).EQ.'*   '.AND.IW(IP2).NE.'*   ')GO TO 320
      IF(IW(IP1).EQ.'/   ')GO TO 320
      IF(IW(IP1).EQ.')   ')GO TO 320
      GO TO 100
!
  320 CONTINUE
      ISTART=IB
      ISTOP=I
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 100
!
  100 CONTINUE
!
  990 CONTINUE
!
!               *****************************************
!               **  STEP 11--                          **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               **  FOR THE SEARCH FOR    (1)    .     **
!               *****************************************
!
      NUMPAS=1000
      DO 1100 IPASS=1,NUMPAS
!
!               ****************************
!               **  STEP 12--             **
!               **  SEARCH FOR   (1)   .  **
!               ****************************
!
      NWM1=NW-1
      NWP1=NW+1
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=I2+1
      IF(IMIN.LE.0)GO TO 1990
      IF(IMIN.GT.NWM1)GO TO 1990
      DO 1200 I=IMIN,NWM1
      I2=I
      IM1=I-1
      IP1=I+1
      IF(IW(IM1).EQ.'(   '.AND.IW(I).EQ.'1   '.AND.   &
         IW(IP1).EQ.')   ')GO TO 1210
 1200 CONTINUE
      GO TO 1990
!
 1210 CONTINUE
      I=I2
      IM1=I-1
      IM2=I-2
      IM3=I-3
      IP1=I+1
      IP2=I+2
      IP3=I+3
!
!               ***********************************
!               **  STEP 13--                    **
!               **  TEST FOR THE  **(1)   CASE.  **
!               ***********************************
!
      IB=IM2
      IF(IB.LE.0)GO TO 1100
      IF(IW(IB).EQ.'**  '.AND.IW(I).EQ.'1   ')GO TO 1310
      IB=IM3
      IF(IB.LE.0)GO TO 1100
      IF(IW(IB).EQ.'*   '.AND.IW(IM2).EQ.'*   '.AND.   &
      IW(I).EQ.'1   ')GO TO 1310
      GO TO 1100
!
 1310 CONTINUE
      IF(IP2.GE.NWP1)GO TO 1320
      IF(IW(IP2).EQ.'+   ')GO TO 1320
      IF(IW(IP2).EQ.'-   ')GO TO 1320
      IF(IP2.EQ.NW.AND.IW(IP2).EQ.'*   ')GO TO 1320
      IF(IP2.LE.NWM1.AND.IW(IP2).EQ.'*   '.AND.IW(IP3).NE.'*   ')   &
      GO TO 1320
      IF(IW(IP2).EQ.'/   ')GO TO 1320
      IF(IW(IP2).EQ.')   ')GO TO 1320
      GO TO 1100
!
 1320 CONTINUE
      ISTART=IB
      ISTOP=IP1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 1100
!
 1100 CONTINUE
!
 1990 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIE1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NW
 9012   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NW
          WRITE(ICOUT,9016)I,IW(I)
 9016     FORMAT('I,IW(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIE1
      SUBROUTINE DPSIEV(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE AN SIEVE PLOT--
!              THIS PLOT IS USED TO ANALYZE ASSOCIATION IN
!              TWO-WAY TABLES.
!                  SIEVE PLOT N11 N12 N21 N22
!                  SIEVE PLOT Y1 Y2
!                  SIEVE PLOT TABLE
!     EXAMPLES--SIEVE PLOT Y1 Y2
!             --SIEVE PLOT TABLE
!             --SIEVE PLOT N11 N12 N21 N22
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE      2007.
!     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3, DPPAR6
!     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
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
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
      INCLUDE 'DPCOZZ.INC'
!
      REAL Y1(MAXOBV)
      REAL Y2(MAXOBV)
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
      REAL TEMP3(MAXOBV)
      REAL XIDTEM(MAXOBV)
      REAL XIDTE2(MAXOBV)
!
      PARAMETER(MAXLEV=300)
      REAL XMAT(MAXLEV,MAXLEV)
      REAL EXPFRE(MAXLEV,MAXLEV)
      REAL RESFRE(MAXLEV,MAXLEV)
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB8),XMAT(1,1))
      EQUIVALENCE (GARBAG(IGARB9),EXPFRE(1,1))
      EQUIVALENCE (GARBAG(IGAR10),RESFRE(1,1))
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPAS'
      ISUBN2='SO  '
      ICASPL='SIEV'
      ICASE='PARA'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      N11=(-999)
      N21=(-999)
      N12=(-999)
      N22=(-999)
      AN11=0.0
      AN21=0.0
      AN12=0.0
      AN22=0.0
      NS1=(-999)
      NS2=(-999)
      NS3=(-999)
      NS4=(-999)
      MINN2=2
!
!               ****************************************
!               **  TREAT THE SIEVE PLOT CASE         **
!               ****************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SIEV')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSIEV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN,NS
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN,NS = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SIEV')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'SIEV' .AND. NUMARG.GE.1 .AND.   &
         IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
      ELSE
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SIEV')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='SIEVE PLOT'
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
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SIEV')THEN
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
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SIEV')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(N11.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
 2201     FORMAT('***** ERROR FROM SIEVE PLOT--')
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
                    IBUGG3,ISUBRO,IFOUND,IERROR)
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
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        ICASE='TABL'
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************
!               **  STEP 61--                      **
!               **  GENERATE THE SIEVE PLOT        **
!               *************************************
!
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SIEV')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,6001)NLOCAL,ICASPL
 6001   FORMAT('NLOCAL,ICASPL=',I5,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL DPSIE2(Y1,Y2,NS1,   &
                  AN11,AN21,AN12,AN22,   &
                  XMAT,EXPFRE,RESFRE,MAXLEV,NROW,NCOL,   &
                  XIDTEM,XIDTE2,TEMP1,TEMP2,   &
                  ICASE,   &
                  Y,X,D,DCOLOR,DFILL,   &
                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 9--   **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SIEV')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIEV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG2,IBUGG3,IBUGQ,ISUBRO
 9012   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',   &
               I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)NLOCAL
 9041   FORMAT('NLOCAL = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NLOCAL.GE.1)THEN
          DO 9042 I=1,NLOCAL
            WRITE(ICOUT,9043)I,Y1(I),Y2(I)
 9043       FORMAT('I,Y1(I),Y2(I) = ',I8,2E15.7)
            CALL DPWRST('XXX','BUG ')
 9042     CONTINUE
        ENDIF
        WRITE(ICOUT,9051)NPLOTP
 9051   FORMAT('NPLOTP = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO 9052 I=1,NPLOTP
            WRITE(ICOUT,9053)I,Y(I),X(I),D(I),DCOLOR(I)
 9053       FORMAT('I,Y(I),X(I),D(I),DCOLOR(I),',I8,4F12.5)
            CALL DPWRST('XXX','BUG ')
 9052     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIEV
      SUBROUTINE DPSIE2(Y1,Y2,N,   &
                        AN11,AN21,AN12,AN22,   &
                        XMAT,EXPFRE,RESFRE,MAXLEV,NROW,NCOL,   &
                        XIDTEM,XIDTE2,TEMP1,TEMP2,   &
                        ICASE,   &
                        Y,X,D,DCOLOR,DFILL,   &
                        N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN SIEVE PLOT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE      2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
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
      DIMENSION XMAT(MAXLEV,MAXLEV)
      DIMENSION EXPFRE(MAXLEV,MAXLEV)
      DIMENSION RESFRE(MAXLEV,MAXLEV)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION DCOLOR(*)
      DIMENSION DFILL(*)
!
      CHARACTER*10 IFORMT
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='E2  '
      IERROR='NO'
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSIE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,ICASE,MAXOBV
   52   FORMAT('IBUGG3,ISUBRO,ICASE,MAXOBV = ',4(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        IF(ICASE.EQ.'VARI')THEN
          WRITE(ICOUT,55)N
   55     FORMAT('N = ',I8)
          CALL DPWRST('XXX','WRIT')
          DO 56 I=1,N
            WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57       FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ELSEIF(ICASE.EQ.'PARA')THEN
          WRITE(ICOUT,75)AN11,AN21,AN12,AN22
   75     FORMAT('AN11,AN21,AN12,AN22 = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
        ELSEIF(ICASE.EQ.'TABL')THEN
          DO 81 I=1,NROW
            DO 83 J=1,NCOL
              WRITE(ICOUT,85)I,J,XMAT(I,J)
   85         FORMAT('I,J,XMAT(I,J) = ',2I8,G15.7)
              CALL DPWRST('XXX','WRIT')
   83       CONTINUE
   81     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  BRANCH TO APPROPRIATE CASE (PARAMETER **
!               **  OR VARIABLE)                          **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')   &
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
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')   &
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
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'MCN2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N11.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
 1201   FORMAT('***** ERROR FROM THE SIEVE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1203)
 1203   FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = ',   &
               'ROW 1, COLUMN 1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1204)
 1204   FORMAT('      MUST BE NON-NEGATIVE.')
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
 1303   FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = ',   &
               'ROW 2, COLUMN 1')
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
 1403   FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = ',   &
               'ROW 1, COLUMN 2')
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
 1503   FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = ',   &
               'ROW 2, COLUMN 2')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1504)
 1504   FORMAT('      MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1505)N22
 1505   FORMAT('      N22 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      XMAT(1,1)=AN11
      XMAT(2,1)=AN21
      XMAT(1,2)=AN12
      XMAT(2,2)=AN22
      NROW=2
      NCOL=2
!
      GO TO 4000
!
!               ********************************************
!               **  STEP 12--                             **
!               **  COMPUTE THE LOG ODDS RATIO TEST       **
!               ********************************************
!
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
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2101)
 2101   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 2. ')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N
 2103   FORMAT('SAMPLE SIZE = ',I8)
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
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(Y1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(Y2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
!
      IF(NUMSE1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN SIEVE PLOT--')
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
      AN=N
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
!
!               ***********************************************
!               **  STEP 2.3--                               **
!               **  CROSS-TABULATE THE TWO VARIABLES         **
!               ***********************************************
!
      ISTEPN='23'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'CHI2')   &
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
          DO 2330 I=1,N
            IF(XIDTEM(ISET1).EQ.Y1(I).AND.XIDTE2(ISET2).EQ.Y2(I))THEN
!
              K=K+1
            ENDIF
 2330     CONTINUE
          XMAT(ISET1,ISET2)=REAL(K)
!
 2320   CONTINUE
 2310 CONTINUE
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
!               **  NOTE THAT FOR THIS COMMAND IS         **
!               **  COMPUTED ON A 2X2 CONTINGENCY TABLE.  **
!               **  THEREFORE:                            **
!               **  1) IF NUMBER OF COLUMNS NOT EQUAL     **
!               **     TWO, FLAG AN ERROR.                **
!               **  2) IF NUMBER OF ROWS EQUAL TWO, THEN  **
!               **     EXTRACT THE RELEVANT 4 VALUES AND  **
!               **     GO TO THE PARAMETER CASE.          **
!               **  3) IF NUMBER OF ROWS GREATER THAN     **
!               **     TWO, THEN NEED TO CROSS-TABULATE   **
!               **     (I.E., HAVE THE VARIABLE CASE).    **
!               ********************************************
!
      ISTEPN='31'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERROR='NO'
!
      IF(NCOL.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3101)
 3101   FORMAT('      THE NUMBER OF COLUMNS IN THE INPUT MATRIX')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3103)
 3103   FORMAT('      IS LESS THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3105)NCOL
 3105   FORMAT('      THE NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NROW.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3111)
 3111   FORMAT('      THE NUMBER OF ROWS IN THE INPUT MATRIX')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3113)
 3113   FORMAT('      IS LESS THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3115)NROW
 3115   FORMAT('      THE NUMBER OF ROWS = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     ROUND TABLE ENTRIES TO NEAREST INTEGER AND CHECK
!     FOR NEGATIVE FREQUENCIES
!
      DO 3200 I=1,NROW
        DO 3300 J=1,NCOL
          ITEMP=INT(XMAT(I,J)+0.5)
          IF(ITEMP.LT.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3201)I,J
 3201       FORMAT('      ROW ',I8,' COLUMN ',I8,' OF THE INPUT ',   &
                   'TABLE')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3203)XMAT(I,J)
 3203       FORMAT('      CONTAINS A NEGATIVE FREQUENCY ( = ',G15.7,   &
                   ')')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
          XMAT(I,J)=REAL(ITEMP)
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
            WRITE(ICOUT,3285)I,J,XMAT(I,J)
 3285       FORMAT('I,J,XMAT(I,J) = ',2I8,G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
 3300   CONTINUE
 3200 CONTINUE
!
      GO TO 4000
!
 4000 CONTINUE
!
      ISTEPN='41'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     STEP 1: COMPUTE THE ROW TOTALS (TEMP1) AND COLUMN
!             TOTALS (TEMP2).
!
!             FOR SUBSEQUENT STEPS, PUT THE CUMULATIVE ROW
!             AND COLUMN TOTALS FOR EXPECTED FREQUENCIES IN
!             XIDTEM AND XIDTE2.
!
      SUM2=0.0
      DO 4100 I=1,NROW
        SUM1=0.0
        DO 4110 J=1,NCOL
          SUM1=SUM1+XMAT(I,J)
          SUM2=SUM2+XMAT(I,J)
 4110   CONTINUE
        TEMP1(I)=SUM1
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
          WRITE(ICOUT,4111)I,TEMP1(I)
 4111     FORMAT('      I,TEMP1(I),SUM2 = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 4100 CONTINUE
      ATOTAL=SUM2
!
      DO 4150 J=1,NCOL
        SUM1=0.0
        DO 4160 I=1,NROW
          SUM1=SUM1+XMAT(I,J)
 4160   CONTINUE
        TEMP2(J)=SUM1
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
          WRITE(ICOUT,4161)J,TEMP2(J)
 4161     FORMAT('      J,TEMP2(J) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 4150 CONTINUE
!
!     STEP 2: COMPUTE THE EXPECTED FREQUENCES AND THE
!             STANDARDIZED RESIDUALS.
!
      ISTEPN='42'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      AMXRES=0.0
      AMXFRE=0.0
      DO 4200 I=1,NROW
        DO 4210 J=1,NCOL
          EXPFRE(I,J)=TEMP1(I)*TEMP2(J)/ATOTAL
          ATEMP=SQRT(EXPFRE(I,J))
          IF(ATEMP.GT.AMXFRE)AMXFRE=ATEMP
!
          IF(EXPFRE(I,J).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4201)I,J
 4201       FORMAT('      ROW ',I8,' COLUMN ',I8,' OF THE EXPECTED')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4203)
 4203       FORMAT('      FREQUENCY TABLE IS ZERO.  UNABLE TO ',   &
                   'GENERATE THE SIEVE PLOT.')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4205)
 4205       FORMAT('      SUGGESTED FIX: COMBINE ROWS OR ',   &
                   'COLUMNS THAT HAVE ZERO FREQUENCY.')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          RESFRE(I,J)=(XMAT(I,J) - EXPFRE(I,J))/SQRT(EXPFRE(I,J))
          ATEMP=ABS(RESFRE(I,J))
          IF(ATEMP.GT.AMXRES)AMXRES=ATEMP
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
            WRITE(ICOUT,4211)I,J,XMAT(I,J),EXPFRE(I,J),RESFRE(I,J)
 4211       FORMAT('      I,J,XMAT(I,J),EXPFRE(I,J),RESFRE(I,J) = ',   &
                   2I8,3G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
 4210   CONTINUE
 4200 CONTINUE
!
!     STEP 3: COMPUTE THE ROW TOTALS (TEMP1) AND COLUMN
!             TOTALS (TEMP2) FOR THE EXPECTED FREQUENCIES.
!
!             FOR SUBSEQUENT STEPS, PUT THE CUMULATIVE ROW
!             AND COLUMN TOTALS FOR EXPECTED FREQUENCIES IN
!             XIDTEM AND XIDTE2.
!
      SUM2=0.0
      DO 4300 I=1,NROW
        SUM1=0.0
        DO 4310 J=1,NCOL
          SUM1=SUM1+EXPFRE(I,J)
          SUM2=SUM2+EXPFRE(I,J)
 4310   CONTINUE
        TEMP1(I)=SUM1
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
          WRITE(ICOUT,4311)I,TEMP1(I)
 4311     FORMAT('      I,TEMP1(I),SUM2 = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 4300 CONTINUE
!
      DO 4350 J=1,NCOL
        SUM1=0.0
        DO 4360 I=1,NROW
          SUM1=SUM1+EXPFRE(I,J)
 4360   CONTINUE
        TEMP2(J)=SUM1
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
          WRITE(ICOUT,4361)J,TEMP2(J)
 4361     FORMAT('      J,TEMP2(J) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 4350 CONTINUE
!
      DO 4420 I=1,NROW
        XIDTEM(I)=0.0
        DO 4425 J=1,I
          XIDTEM(I)=XIDTEM(I) + TEMP1(J)
 4425   CONTINUE
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
          WRITE(ICOUT,4421)I,XIDTEM(I)
 4421     FORMAT('      I,XIDTEM(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 4420 CONTINUE
!
      DO 4470 I=1,NCOL
        XIDTE2(I)=0.0
        DO 4475 J=1,I
          XIDTE2(I)=XIDTE2(I) + TEMP2(J)
 4475   CONTINUE
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
          WRITE(ICOUT,4471)I,XIDTE2(I)
 4471     FORMAT('      I,XIDTE2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 4470 CONTINUE
!
!     STEP 3: NOW GENERATE THE PLOT COORDINATES FOR THE
!             SIEVE PLOT.  AT EACH ENTRY OF THE TABLE
!             (I.E., ROW I, COLUMN J), GENERATE A BOX WITH
!             THE FOLLOWING WIDTH AND HEIGHT:
!
!             1) WIDTH OF BOX IS PROPORTIONAL TO
!                MARGINAL ROW TOTAL
!
!             2) HEIGHT OF BOX IS PROPORTIONAL TO THE
!                MARGINAL COLUMN TOTAL
!
      ISTEPN='45'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ASPABX=0.05
      AMINWD=0.5 + ASPABX
      AMAXWD=REAL(NCOL) + (0.5-ASPABX)
      AWIDTO=(AMAXWD - AMINWD) - REAL(NCOL-1)*ASPABX
      AMINHE=0.5 + ASPABX
      AMAXHE=REAL(NROW) + (0.5-ASPABX)
      AHEITO=(AMAXHE - AMINHE) - REAL(NROW-1)*ASPABX
      ABOXES=SQRT(ATOTAL)
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
          WRITE(ICOUT,4501)AMINWD,AMAXWD,AWIDTO
 4501     FORMAT('      AMINWD,AMAXWD,AWIDTO = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,4503)AMINHE,AMAXHE,AHEITO
 4503     FORMAT('      AMINHE,AMAXHE,AHEITO = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,4505)ABOXES
 4505     FORMAT('      ABOXES = ',G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
      ICNT=0
      ICNT2=0
!
      DO 4510 I=1,NROW
        DO 4520 J=1,NCOL
!
          IF(J.EQ.1)THEN
            XSTRT=AMINWD
          ELSE
            XSTRT=AMINWD + (XIDTE2(J-1)/XIDTE2(NCOL))*AWIDTO +   &
                  REAL(J-1)*ASPABX
          ENDIF
          IF(I.EQ.1)THEN
            YSTRT=AMINHE
          ELSE
            YSTRT=AMINHE + (XIDTEM(I-1)/XIDTEM(NROW))*AHEITO +   &
                  REAL(I-1)*ASPABX
          ENDIF
!
          AWIDTH=(TEMP2(J)/XIDTE2(NCOL))*AWIDTO
          AHEIGH=(TEMP1(I)/XIDTEM(NROW))*AHEITO
!
          ICNT2=ICNT2+1
          ICNT=ICNT+1
          X(ICNT)=XSTRT
          Y(ICNT)=YSTRT
          D(ICNT)=REAL(ICNT2)
          IF(RESFRE(I,J).GE.0.0)THEN
            DCOLOR(ICNT)=1.0
          ELSE
            DCOLOR(ICNT)=2.0
          ENDIF
          DFILL(ICNT)=1.0
!
          ICNT=ICNT+1
          X(ICNT)=XSTRT + AWIDTH
          Y(ICNT)=YSTRT
          D(ICNT)=REAL(ICNT2)
          IF(RESFRE(I,J).GE.0.0)THEN
            DCOLOR(ICNT)=1.0
          ELSE
            DCOLOR(ICNT)=2.0
          ENDIF
          DFILL(ICNT)=1.0
!
          ICNT=ICNT+1
          X(ICNT)=XSTRT + AWIDTH
          Y(ICNT)=YSTRT + AHEIGH
          D(ICNT)=REAL(ICNT2)
          IF(RESFRE(I,J).GE.0.0)THEN
            DCOLOR(ICNT)=1.0
          ELSE
            DCOLOR(ICNT)=2.0
          ENDIF
          DFILL(ICNT)=1.0
!
          ICNT=ICNT+1
          X(ICNT)=XSTRT
          Y(ICNT)=YSTRT + AHEIGH
          D(ICNT)=REAL(ICNT2)
          IF(RESFRE(I,J).GE.0.0)THEN
            DCOLOR(ICNT)=1.0
          ELSE
            DCOLOR(ICNT)=2.0
          ENDIF
          DFILL(ICNT)=1.0
!
          ICNT=ICNT+1
          X(ICNT)=XSTRT
          Y(ICNT)=YSTRT
          D(ICNT)=REAL(ICNT2)
          IF(RESFRE(I,J).GE.0.0)THEN
            DCOLOR(ICNT)=1.0
          ELSE
            DCOLOR(ICNT)=2.0
          ENDIF
          DFILL(ICNT)=1.0
!
!         NOW GENERATE THE VERTICAL CROSS-HATCHES
!
          U1=(EXPFRE(I,J)/TEMP1(I))*ABOXES
          ABOXOB=U1*SQRT(XMAT(I,J)/EXPFRE(I,J))
          NLOOP=INT(ABOXOB)
          AHORSP=AWIDTH/ABOXOB
          XINIT=XSTRT
          YCOOR1=YSTRT
          YCOOR2=YSTRT + AHEIGH
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
            WRITE(ICOUT,4561)XMAT(I,J),XIDTEM(I),U1
 4561       FORMAT('      XMAT(I,J),XIDTEM(I),U1 = ',3G15.7)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4563)NLOOP,AWIDTH,AHORSP
 4563       FORMAT('      NLOOP,AWIDTH,AHORSP = ',I8,2G15.7)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4565)XINIT,YCOOR1,YCOOR2
 4565       FORMAT('      XINIT,YCOOR1,YCOOR2 = ',3G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
          IF(NLOOP.GE.1)THEN
            DO 4560 II=1,NLOOP
              XCOOR=XINIT + REAL(II)*AHORSP
!
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR
              Y(ICNT)=YCOOR1
              D(ICNT)=REAL(ICNT2)
              IF(RESFRE(I,J).GE.0.0)THEN
                DCOLOR(ICNT)=1.0
              ELSE
                DCOLOR(ICNT)=2.0
              ENDIF
              DFILL(ICNT)=2.0
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR
              Y(ICNT)=YCOOR2
              D(ICNT)=REAL(ICNT2)
              IF(RESFRE(I,J).GE.0.0)THEN
                DCOLOR(ICNT)=1.0
              ELSE
                DCOLOR(ICNT)=2.0
              ENDIF
              DFILL(ICNT)=2.0
!
 4560       CONTINUE
          ENDIF
!
!         NOW GENERATE THE HORIZONTAL CROSS-HATCHES
!
          U1=(EXPFRE(I,J)/TEMP2(J))*ABOXES
          ABOXOB=U1*SQRT(XMAT(I,J)/EXPFRE(I,J))
          NLOOP=INT(ABOXOB)
          AVERSP=AHEIGH/ABOXOB
          YINIT=YSTRT
          XCOOR1=XSTRT
          XCOOR2=XSTRT + AWIDTH
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
            WRITE(ICOUT,4571)XMAT(I,J),XIDTE2(J),U1
 4571       FORMAT('      XMAT(I,J),XIDTE2(J),U1 = ',3G15.7)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4573)NLOOP,AHEIGH,AVERSP
 4573       FORMAT('      NLOOP,AHEIGH,AVERSP = ',I8,2G15.7)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4575)YINIT,XCOOR1,XCOOR2
 4575       FORMAT('      YINIT,XCOOR1,XCOOR2 = ',3G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
          IF(NLOOP.GE.1)THEN
            DO 4570 II=1,NLOOP
              YCOOR=YINIT + REAL(II)*AVERSP
!
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCOOR
              D(ICNT)=REAL(ICNT2)
              IF(RESFRE(I,J).GE.0.0)THEN
                DCOLOR(ICNT)=1.0
              ELSE
                DCOLOR(ICNT)=2.0
              ENDIF
              DFILL(ICNT)=3.0
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCOOR
              D(ICNT)=REAL(ICNT2)
              IF(RESFRE(I,J).GE.0.0)THEN
                DCOLOR(ICNT)=1.0
              ELSE
                DCOLOR(ICNT)=2.0
              ENDIF
              DFILL(ICNT)=3.0
!
 4570       CONTINUE
          ENDIF
!
 4520   CONTINUE
 4510 CONTINUE
!
      N2=ICNT
      NPLOTV=2
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=1
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IFORMT='(   E15.7)'
      IF(NCOL.LE.9)THEN
        WRITE(IFORMT(4:4),'(I1)')NCOL
      ELSEIF(NCOL.LE.99)THEN
        WRITE(IFORMT(3:4),'(I2)')NCOL
      ELSEIF(NCOL.LE.999)THEN
        WRITE(IFORMT(2:4),'(I3)')NCOL
      ELSE
        GO TO 7019
      ENDIF
      DO 7010 I=1,NROW
        WRITE(IOUNI1,IFORMT)(XMAT(I,J),J=1,NCOL)
        WRITE(IOUNI2,IFORMT)(EXPFRE(I,J),J=1,NCOL)
        WRITE(IOUNI3,IFORMT)(RESFRE(I,J),J=1,NCOL)
 7010 CONTINUE
 7019 CONTINUE
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SIE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,N,N2,IERROR
 9012   FORMAT('ICASPL,N,N2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)N2,NPLOTV
 9031   FORMAT('N2,NPLOTV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9035 I=1,N2
          WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036     FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIE2
      SUBROUTINE DPSIFL(IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--SIMPLIFY AN EXPRESSION BY REMOVING
!              ALL REDUNDANT PARENTHESES
!              AT THE BEGINNING AND END.
!     NOTE--THE INPUT ARGUMENTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)---------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO  '
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIFL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               *****************************************
!
      NUMPAS=1000
      DO 100 IPASS=1,NUMPAS
!
!               ****************************************
!               **  STEP 2--                          **
!               **  DETERMINE IF THE FIRST CHARACTER  **
!               **  IS A LEFT  PARENTHESIS.           **
!               **  DETERMINE IF THE LAST  CHARACTER  **
!               **  IS A RIGHT PARENTHESIS.           **
!               ****************************************
!
      IF(NW.LE.0)GO TO 9000
      IF(IW(1).EQ.'(    '.AND.IW(NW).EQ.')    ')GO TO 290
      GO TO 9000
!
  290 CONTINUE
!
!               ***********************************************
!               **  STEP 3--                                 **
!               **  DETERMINE IF THE RIGHT PARENTHESIS       **
!               **  IN THE LAST  LOCATION IS THE COMPLEMENT  **
!               **  TO THE LEFT  PARENTHESIS                 **
!               **  IN THE FIRST LOCATION.                   **
!               ***********************************************
!
      ISUM=0
      IMIN=1
      IMAX=NW
      DO 300 I=IMIN,IMAX
      IF(IW(I).EQ.'(    ')GO TO 301
      IF(IW(I).EQ.')    ')GO TO 302
      GO TO 300
  301 CONTINUE
      ISUM=ISUM-1
      ILOC0=I
      IF(ISUM.EQ.0)GO TO 350
      GO TO 300
  302 CONTINUE
      ISUM=ISUM+1
      ILOC0=I
      IF(ISUM.EQ.0)GO TO 350
      GO TO 300
  300 CONTINUE
!
      WRITE(ICOUT,311)
  311 FORMAT('***** ERROR IN DPSIFL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,312)
  312 FORMAT('      NUMBER OF LEFT PARENTHESES DOES NOT EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,313)
  313 FORMAT('      NUMBER OF RIGHT PARENTHESES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,314)
  314 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,315)(IW(I),I=1,NW)
  315 FORMAT('      ',115A1)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  350 CONTINUE
      IF(ILOC0.EQ.NW)GO TO 390
      GO TO 9000
  390 CONTINUE
!
!               *************************************
!               **  STEP 4--                       **
!               **  ELIMINATE THE PARENTHESES IN   **
!               **  LOCATION 1 AND LOCATION NW.    **
!               **  RESET THE VALUE IN NW.         **
!               *************************************
!
      ISTART=1
      ISTOP=1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
!
      ISTART=NW
      ISTOP=NW
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
!
  100 CONTINUE
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSIFL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)NW
 9012 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NW
      WRITE(ICOUT,9016)I,IW(I)
 9016 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPSIFL
      SUBROUTINE DPSIFN (X, N, KODE, M, ANS, NZ, IERR)
!***BEGIN PROLOGUE  DPSIFN
!***PURPOSE  Compute derivatives of the Psi function.
!***LIBRARY   SLATEC
!***CATEGORY  C7C
!***TYPE      DOUBLE PRECISION (PSIFN-S, DPSIFN-D)
!***KEYWORDS  DERIVATIVES OF THE GAMMA FUNCTION, POLYGAMMA FUNCTION,
!             PSI FUNCTION
!***AUTHOR  Amos, D. E., (SNLA)
!***DESCRIPTION
!
!         The following definitions are used in DPSIFN:
!
!      Definition 1
!         PSI(X) = d/dx (ln(GAMMA(X)), the first derivative of
!                  the log GAMMA function.
!      Definition 2
!                     K   K
!         PSI(K,X) = d /dx (PSI(X)), the K-th derivative of PSI(X).
!   ___________________________________________________________________
!      DPSIFN computes a sequence of SCALED derivatives of
!      the PSI function; i.e. for fixed X and M it computes
!      the M-member sequence
!
!                    ((-1)**(K+1)/GAMMA(K+1))*PSI(K,X)
!                       for K = N,...,N+M-1
!
!      where PSI(K,X) is as defined above.   For KODE=1, DPSIFN returns
!      the scaled derivatives as described.  KODE=2 is operative only
!      when K=0 and in that case DPSIFN returns -PSI(X) + LN(X).  That
!      is, the logarithmic behavior for large X is removed when KODE=2
!      and K=0.  When sums or differences of PSI functions are computed
!      the logarithmic terms can be combined analytically and computed
!      separately to help retain significant digits.
!
!         Note that CALL DPSIFN(X,0,1,1,ANS) results in
!                   ANS = -PSI(X)
!
!     Input      X is DOUBLE PRECISION
!           X      - Argument, X .gt. 0.0D0
!           N      - First member of the sequence, 0 .le. N .le. 100
!                    N=0 gives ANS(1) = -PSI(X)       for KODE=1
!                                       -PSI(X)+LN(X) for KODE=2
!           KODE   - Selection parameter
!                    KODE=1 returns scaled derivatives of the PSI
!                    function.
!                    KODE=2 returns scaled derivatives of the PSI
!                    function EXCEPT when N=0. In this case,
!                    ANS(1) = -PSI(X) + LN(X) is returned.
!           M      - Number of members of the sequence, M.ge.1
!
!    Output     ANS is DOUBLE PRECISION
!           ANS    - A vector of length at least M whose first M
!                    components contain the sequence of derivatives
!                    scaled according to KODE.
!           NZ     - Underflow flag
!                    NZ.eq.0, A normal return
!                    NZ.ne.0, Underflow, last NZ components of ANS are
!                             set to zero, ANS(M-K+1)=0.0, K=1,...,NZ
!           IERR   - Error flag
!                    IERR=0, A normal return, computation completed
!                    IERR=1, Input error,     no computation
!                    IERR=2, Overflow,        X too small or N+M-1 too
!                            large or both
!                    IERR=3, Error,           N too large. Dimensioned
!                            array TRMR(NMAX) is not large enough for N
!
!         The nominal computational accuracy is the maximum of unit
!         roundoff (=D1MACH(4)) and 1.0D-18 since critical constants
!         are given to only 18 digits.
!
!         PSIFN is the single precision version of DPSIFN.
!
! *Long Description:
!
!         The basic method of evaluation is the asymptotic expansion
!         for large X.ge.XMIN followed by backward recursion on a two
!         term recursion relation
!
!                  W(X+1) + X**(-N-1) = W(X).
!
!         This is supplemented by a series
!
!                  SUM( (X+K)**(-N-1) , K=0,1,2,... )
!
!         which converges rapidly for large N. Both XMIN and the
!         number of terms of the series are calculated from the unit
!         roundoff of the machine environment.
!
!***REFERENCES  Handbook of Mathematical Functions, National Bureau
!                 of Standards Applied Mathematics Series 55, edited
!                 by M. Abramowitz and I. A. Stegun, equations 6.3.5,
!                 6.3.18, 6.4.6, 6.4.9 and 6.4.10, pp.258-260, 1964.
!               D. E. Amos, A portable Fortran subroutine for
!                 derivatives of the Psi function, Algorithm 610, ACM
!                 Transactions on Mathematical Software 9, 4 (1983),
!                 pp. 494-502.
!***ROUTINES CALLED  D1MACH, I1MACH
!***REVISION HISTORY  (YYMMDD)
!   820601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   891006  Cosmetic changes to prologue.  (WRB)
!   891006  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DPSIFN
      INCLUDE 'DPCOMC.INC'
!
      INTEGER I, IERR, J, K, KODE, M, MM, MX, N, NMAX, NN, NP, NX, NZ,   &
        FN
      INTEGER I1MACH
      DOUBLE PRECISION ANS, ARG, B, DEN, ELIM, EPS, FLN,   &
       FX, RLN, RXSQ, R1M4, R1M5, S, SLOPE, T, TA, TK, TOL, TOLS, TRM,   &
       TRMR, TSS, TST, TT, T1, T2, WDTOL, X, XDMLN, XDMY, XINC, XLN,   &
       XM, XMIN, XQ, YINT
      DIMENSION B(22), TRM(22), TRMR(100), ANS(*)
      SAVE NMAX, B
      DATA NMAX /100/
!-----------------------------------------------------------------------
!             BERNOULLI NUMBERS
!-----------------------------------------------------------------------
      DATA B(1), B(2), B(3), B(4), B(5), B(6), B(7), B(8), B(9), B(10),   &
       B(11), B(12), B(13), B(14), B(15), B(16), B(17), B(18), B(19),   &
       B(20), B(21), B(22) /1.00000000000000000D+00,   &
       -5.00000000000000000D-01,1.66666666666666667D-01,   &
       -3.33333333333333333D-02,2.38095238095238095D-02,   &
       -3.33333333333333333D-02,7.57575757575757576D-02,   &
       -2.53113553113553114D-01,1.16666666666666667D+00,   &
       -7.09215686274509804D+00,5.49711779448621554D+01,   &
       -5.29124242424242424D+02,6.19212318840579710D+03,   &
       -8.65802531135531136D+04,1.42551716666666667D+06,   &
       -2.72982310678160920D+07,6.01580873900642368D+08,   &
       -1.51163157670921569D+10,4.29614643061166667D+11,   &
       -1.37116552050883328D+13,4.88332318973593167D+14,   &
       -1.92965793419400681D+16/
!
!***FIRST EXECUTABLE STATEMENT  DPSIFN
      IERR = 0
      NZ=0
      IF (X.LE.0.0D0) IERR=1
      IF (N.LT.0) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (M.LT.1) IERR=1
      IF (IERR.NE.0) RETURN
      MM=M
      NX = MIN(-I1MACH(15),I1MACH(16))
      R1M5 = D1MACH(5)
      R1M4 = D1MACH(4)*0.5D0
      WDTOL = MAX(R1M4,0.5D-18)
!-----------------------------------------------------------------------
!     ELIM = APPROXIMATE EXPONENTIAL OVER AND UNDERFLOW LIMIT
!-----------------------------------------------------------------------
      ELIM = 2.302D0*(NX*R1M5-3.0D0)
      XLN = LOG(X)
   41 CONTINUE
      NN = N + MM - 1
      FN = NN
      T = (FN+1)*XLN
!-----------------------------------------------------------------------
!     OVERFLOW AND UNDERFLOW TEST FOR SMALL AND LARGE X
!-----------------------------------------------------------------------
      IF (ABS(T).GT.ELIM) GO TO 290
      IF (X.LT.WDTOL) GO TO 260
!-----------------------------------------------------------------------
!     COMPUTE XMIN AND THE NUMBER OF TERMS OF THE SERIES, FLN+1
!-----------------------------------------------------------------------
      RLN = R1M5*I1MACH(14)
      RLN = MIN(RLN,18.06D0)
      FLN = MAX(RLN,3.0D0) - 3.0D0
      YINT = 3.50D0 + 0.40D0*FLN
      SLOPE = 0.21D0 + FLN*(0.0006038D0*FLN+0.008677D0)
      XM = YINT + SLOPE*FN
      MX = INT(XM) + 1
      XMIN = MX
      IF (N.EQ.0) GO TO 50
      XM = -2.302D0*RLN - MIN(0.0D0,XLN)
      ARG = XM/N
      ARG = MIN(0.0D0,ARG)
      EPS = EXP(ARG)
      XM = 1.0D0 - EPS
      IF (ABS(ARG).LT.1.0D-3) XM = -ARG
      FLN = X*XM/EPS
      XM = XMIN - X
      IF (XM.GT.7.0D0 .AND. FLN.LT.15.0D0) GO TO 200
   50 CONTINUE
      XDMY = X
      XDMLN = XLN
      XINC = 0.0D0
      IF (X.GE.XMIN) GO TO 60
      NX = INT(X)
      XINC = XMIN - NX
      XDMY = X + XINC
      XDMLN = LOG(XDMY)
   60 CONTINUE
!-----------------------------------------------------------------------
!     GENERATE W(N+MM-1,X) BY THE ASYMPTOTIC EXPANSION
!-----------------------------------------------------------------------
      T = FN*XDMLN
      T1 = XDMLN + XDMLN
      T2 = T + XDMLN
      TK = MAX(ABS(T),ABS(T1),ABS(T2))
      IF (TK.GT.ELIM) GO TO 380
      TSS = EXP(-T)
      TT = 0.5D0/XDMY
      T1 = TT
      TST = WDTOL*TT
      IF (NN.NE.0) T1 = TT + 1.0D0/FN
      RXSQ = 1.0D0/(XDMY*XDMY)
      TA = 0.5D0*RXSQ
      T = (FN+1)*TA
      S = T*B(3)
      IF (ABS(S).LT.TST) GO TO 80
      TK = 2.0D0
      DO 70 K=4,22
        T = T*((TK+FN+1)/(TK+1.0D0))*((TK+FN)/(TK+2.0D0))*RXSQ
        TRM(K) = T*B(K)
        IF (ABS(TRM(K)).LT.TST) GO TO 80
        S = S + TRM(K)
        TK = TK + 2.0D0
   70 CONTINUE
   80 CONTINUE
      S = (S+T1)*TSS
      IF (XINC.EQ.0.0D0) GO TO 100
!-----------------------------------------------------------------------
!     BACKWARD RECUR FROM XDMY TO X
!-----------------------------------------------------------------------
      NX = INT(XINC)
      NP = NN + 1
      IF (NX.GT.NMAX) GO TO 390
      IF (NN.EQ.0) GO TO 160
      XM = XINC - 1.0D0
      FX = X + XM
!-----------------------------------------------------------------------
!     THIS LOOP SHOULD NOT BE CHANGED. FX IS ACCURATE WHEN X IS SMALL
!-----------------------------------------------------------------------
      DO 90 I=1,NX
        TRMR(I) = FX**(-NP)
        S = S + TRMR(I)
        XM = XM - 1.0D0
        FX = X + XM
   90 CONTINUE
  100 CONTINUE
      ANS(MM) = S
      IF (FN.EQ.0) GO TO 180
!-----------------------------------------------------------------------
!     GENERATE LOWER DERIVATIVES, J.LT.N+MM-1
!-----------------------------------------------------------------------
      IF (MM.EQ.1) RETURN
      DO 150 J=2,MM
        FN = FN - 1
        TSS = TSS*XDMY
        T1 = TT
        IF (FN.NE.0) T1 = TT + 1.0D0/FN
        T = (FN+1)*TA
        S = T*B(3)
        IF (ABS(S).LT.TST) GO TO 120
        TK = 4 + FN
        DO 110 K=4,22
          TRM(K) = TRM(K)*(FN+1)/TK
          IF (ABS(TRM(K)).LT.TST) GO TO 120
          S = S + TRM(K)
          TK = TK + 2.0D0
  110   CONTINUE
  120   CONTINUE
        S = (S+T1)*TSS
        IF (XINC.EQ.0.0D0) GO TO 140
        IF (FN.EQ.0) GO TO 160
        XM = XINC - 1.0D0
        FX = X + XM
        DO 130 I=1,NX
          TRMR(I) = TRMR(I)*FX
          S = S + TRMR(I)
          XM = XM - 1.0D0
          FX = X + XM
  130   CONTINUE
  140   CONTINUE
        MX = MM - J + 1
        ANS(MX) = S
        IF (FN.EQ.0) GO TO 180
  150 CONTINUE
      RETURN
!-----------------------------------------------------------------------
!     RECURSION FOR N = 0
!-----------------------------------------------------------------------
  160 CONTINUE
      DO 170 I=1,NX
        S = S + 1.0D0/(X+NX-I)
  170 CONTINUE
  180 CONTINUE
      IF (KODE.EQ.2) GO TO 190
      ANS(1) = S - XDMLN
      RETURN
  190 CONTINUE
      IF (XDMY.EQ.X) RETURN
      XQ = XDMY/X
      ANS(1) = S - LOG(XQ)
      RETURN
!-----------------------------------------------------------------------
!     COMPUTE BY SERIES (X+K)**(-(N+1)) , K=0,1,2,...
!-----------------------------------------------------------------------
  200 CONTINUE
      NN = INT(FLN) + 1
      NP = N + 1
      T1 = (N+1)*XLN
      T = EXP(-T1)
      S = T
      DEN = X
      DO 210 I=1,NN
        DEN = DEN + 1.0D0
        TRM(I) = DEN**(-NP)
        S = S + TRM(I)
  210 CONTINUE
      ANS(1) = S
      IF (N.NE.0) GO TO 220
      IF (KODE.EQ.2) ANS(1) = S + XLN
  220 CONTINUE
      IF (MM.EQ.1) RETURN
!-----------------------------------------------------------------------
!     GENERATE HIGHER DERIVATIVES, J.GT.N
!-----------------------------------------------------------------------
      TOL = WDTOL/5.0D0
      DO 250 J=2,MM
        T = T/X
        S = T
        TOLS = T*TOL
        DEN = X
        DO 230 I=1,NN
          DEN = DEN + 1.0D0
          TRM(I) = TRM(I)/DEN
          S = S + TRM(I)
          IF (TRM(I).LT.TOLS) GO TO 240
  230   CONTINUE
  240   CONTINUE
        ANS(J) = S
  250 CONTINUE
      RETURN
!-----------------------------------------------------------------------
!     SMALL X.LT.UNIT ROUND OFF
!-----------------------------------------------------------------------
  260 CONTINUE
      ANS(1) = X**(-N-1)
      IF (MM.EQ.1) GO TO 280
      K = 1
      DO 270 I=2,MM
        ANS(K+1) = ANS(K)/X
        K = K + 1
  270 CONTINUE
  280 CONTINUE
      IF (N.NE.0) RETURN
      IF (KODE.EQ.2) ANS(1) = ANS(1) + XLN
      RETURN
  290 CONTINUE
      IF (T.GT.0.0D0) GO TO 380
      NZ=0
      IERR=2
      RETURN
  380 CONTINUE
      NZ=NZ+1
      ANS(MM)=0.0D0
      MM=MM-1
      IF (MM.EQ.0) RETURN
      GO TO 41
  390 CONTINUE
      NZ=0
      IERR=3
      RETURN
      END SUBROUTINE DPSIFN 
      SUBROUTINE DPSIGN(XTEMP1,XTEMP2,MAXNXT,   &
                        ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A ONE-SAMPLE OR TWO-SAMPLE SIGN TEST
!     EXAMPLE--SIGN TEST Y MU
!              SIGN TEST MU Y
!              SIGN TEST Y1 Y2
!              SIGN TEST Y1 Y2 D0
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/6
!     ORIGINAL VERSION--JUNE      1999.
!     UPDATED         --OCTOBER   2004. SUPPORT FOR HTML/LATEX
!     UPDATED         --APRIL     2011. USE DPPARS AND DPPAR3
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
      CHARACTER*4 IMULT
      CHARACTER*4 IREPL
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICASE
!
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
      DIMENSION XTEMP2(*)
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
      ISUBN1='DPSI'
      ISUBN2='GN  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IFOUND='YES'
      IERROR='NO'
      IREPL='OFF'
      IMULT='OFF'
      ICASA2='UNKN'
      ICASA3='BOTH'
!
!               ********************************
!               **  TREAT THE SIGN TEST CASE  **
!               ********************************
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SIGN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSIGN--')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SIGN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTZ=9999
      ICASAN='SIGN'
!
!     LOOK FOR:
!
!          SIGN TEST
!          LOWER TAILED
!          UPPER TAILED
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
        ELSEIF(ICTMP1.EQ.'SIGN' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='SIGN'
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'SIGN' .AND. ICTMP2.EQ.'RANK')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'SIGN')THEN
          IFOUND='YES'
          ICASAN='SIGN'
          ILASTZ=I
        ELSEIF(ICTMP1.EQ.'LOWE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA3='LOWE'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'UPPE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA3='UPPE'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SIGN')THEN
        WRITE(ICOUT,91)ICASAN,ICASA2,ISHIFT
   91   FORMAT('DPWILC: ICASAN,ICASA2,ISHIFT = ',   &
               2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SIGN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='SIGN TEST'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=1
      MINNVA=1
      MAXNVA=MAXSPN
      IFLAGP=29
      IF(IREPL.EQ.'ON')THEN
        IFLAGE=1
        IFLAGM=0
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SIGN')THEN
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
!     DETERMINE WHETHER WE HAVE THE ONE-SAMPLE SIGN TEST OR THE
!     TWO-SAMPLE SIGN TEST.  IN EITHER CASE, THE FIRST OR LAST
!     ARGUMENT CAN BE A PARAMETER.
!
      AMU0=0.0
!
      IF(IVARTY(1).EQ.'PARA')THEN
        ISTART=2
        ISTOP=NUMVAR
        AMU0=PVAR(1)
        IF(ICASA2.EQ.'UNKN')THEN
          IF(NUMVAR.EQ.2)THEN
            ICASA2='ONES'
          ELSE
            ICASA2='TWOS'
          ENDIF
        ENDIF
      ELSEIF(IVARTY(NUMVAR).EQ.'PARA')THEN
        ISTART=1
        ISTOP=NUMVAR-1
        AMU0=PVAR(NUMVAR)
        IF(ICASA2.EQ.'UNKN')THEN
          IF(NUMVAR.EQ.2)THEN
            ICASA2='ONES'
          ELSE
            ICASA2='TWOS'
          ENDIF
        ENDIF
      ELSE
        IF(NUMVAR.EQ.1)THEN
          ICASA2='ONES'
        ELSE
          ICASA2='TWOS'
        ENDIF
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SIGN')   &
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
!               **  PERFORM 2-SAMPLE SIGN TEST         **
!               *****************************************
!
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SIGN')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DPSIGN, BEFORE CALL DPSIG2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN
 5212       FORMAT('I,J,NS1,NS2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
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
          CALL DPSIG2(Y,NS1,X,NS2,AMU0,AMU0,ICASA2,ICASA3,   &
                      XTEMP1,XTEMP2,MAXNXT,   &
                      ICAPSW,ICAPTY,IFORSW,   &
                      IVARID,IVARI2,IVARI3,IVARI4,   &
                      STATV1,STATV2,STATC1,STATC2,   &
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SIGN')   &
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
          CALL DPSIG5(ICASA2,STATV1,STATC1,   &
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SIGN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIGN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIGN
      SUBROUTINE DPSIG2(Y1,N1,Y2,N2,AMU0,D0,ICASA2,ICASA3,   &
                        XTEMP1,XTEMP2,MAXNXT,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        STATV1,STATV2,STATC1,STATC2,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,   &
                        CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT ONE-SAMPLE OR TWO-SAMPLE
!              SIGN TEST
!     EXAMPLE--SIGN TEST Y MU
!              SIGN TEST MU Y
!              SIGN TEST Y1 Y2
!              SIGN TEST Y1 Y2 D0
!     SAMPLE 1 IS IN INPUT VECTOR Y1
!              (WITH N1 OBSERVATIONS).
!     SAMPLE 2 IS IN INPUT VECTOR Y2
!              (WITH N2 OBSERVATIONS).
!              (BUT N1 SHOULD EQUAL N2)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/6
!     ORIGINAL VERSION--JUNE      1999.
!     UPDATED         --AUGUST    2000. BIG FIX FOR ICONC2
!     UPDATED         --AUGUST    2002. MODIFY OUTPUT FOR BETTER
!                                       CLARITY
!     UPDATED         --OCTOBER   2004. SUPPORT FOR HTML/LATEX
!     UPDATED         --APRIL     2011. USE DPDTA1, DPDTA5 TO PRINT
!                                       OUTPUT.  REFORMAT OUTPUT
!                                       SOMEWHAT AS WELL.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA3
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
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
      DIMENSION XTEMP2(*)
!
      DOUBLE PRECISION DPPF
      DOUBLE PRECISION DPAR
!
      PARAMETER (NUMALP=6)
      REAL ALPHA(NUMALP)
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DATA ALPHA/0.50, 0.80, 0.90, 0.95, 0.99, 0.999/
!
      ISUBN1='DPSI'
      ISUBN2='G2  '
      IERROR='NO'
      IWRITE='OFF'
!
      STATVA=CPUMIN
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSIG2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASA2,ICASA3
   52   FORMAT('IBUGA3,ISUBRO,ICASA2,ICASA3 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IVARID,IVARI2,IVARI3,IVARI4
   53   FORMAT('IVARID,IVARI2,IVARI3,IVARI4 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N1,N2
   55   FORMAT('N1,N2 = ',2I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MAX(N1,N2)
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ************************************
!               **   STEP 21--                    **
!               **   BRANCH DEPENDING ON WHETHER  **
!               **   1-SAMPLE SIGN TEST OR        **
!               **   2-SAMPLE SIGN TEST.          **
!               ************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASA2.EQ.'ONES')GO TO 3100
      GO TO 4100
!
!               *********************************
!               **  STEP 31--                  **
!               **  CARRY OUT CALCULATIONS     **
!               **  FOR A 1-SAMPLE SIGN TEST   **
!               *********************************
!
 3100 CONTINUE
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPSIG3(Y1,N1,AMU0,IWRITE,   &
                  XTEMP1,XTEMP2,MAXNXT,   &
                  Y1MEAN,Y1MED,Y1SD,Y1MAD,   &
                  STATV1,STATC1,STATV2,STATC2,RTIES,NTEMP,   &
                  PVAL2T,PVALLT,PVALUT,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      DPAR=0.5D0
      CALL BINPPF(.0005D0,DPAR,NTEMP,DPPF)
      CTL999=DPPF
      CALL BINPPF(.005D0,DPAR,NTEMP,DPPF)
      CUTL99=DPPF
      CALL BINPPF(.025D0,DPAR,NTEMP,DPPF)
      CUTL95=DPPF
      CALL BINPPF(.05D0,DPAR,NTEMP,DPPF)
      CUTL90=DPPF
      CALL BINPPF(.1D0,DPAR,NTEMP,DPPF)
      CUTL80=DPPF
      CALL BINPPF(.25D0,DPAR,NTEMP,DPPF)
      CUTL50=DPPF
      CALL BINPPF(.75D0,DPAR,NTEMP,DPPF)
      CUTU50=DPPF
      CALL BINPPF(.90D0,DPAR,NTEMP,DPPF)
      CUTU80=DPPF
      CALL BINPPF(.95D0,DPAR,NTEMP,DPPF)
      CUTU90=DPPF
      CALL BINPPF(.975D0,DPAR,NTEMP,DPPF)
      CUTU95=DPPF
      CALL BINPPF(.995D0,DPAR,NTEMP,DPPF)
      CUTU99=DPPF
      CALL BINPPF(.9995D0,DPAR,NTEMP,DPPF)
      CTU999=DPPF
!
!               *********************************
!               **   STEP 32--                 **
!               **   WRITE OUT EVERYTHING      **
!               **   FOR A 1-SAMPLE SIGN TEST  **
!               *********************************
!
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
      ITITLE='One Sample Sign Test'
      NCTITL=20
      ITITLZ='(+ =>  > mu0, - =>  < mu0)'
      NCTITZ=26
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
      ITEXT(ICNT)='H0: P(+) = P(-)'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: P(+) <> P(-)'
      NCTEXT(ICNT)=16
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
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y1MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Median:'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=Y1MED
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y1SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Median Absolute Deviation:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=Y1MAD
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
      ITEXT(ICNT)='Mu0:'
      NCTEXT(ICNT)=4
      AVALUE(ICNT)=AMU0
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Positive Differences:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=STATV1
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Negative Differences:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=STATV2
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Ties:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=RTIES
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value for Positive Values:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=STATC1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value for Negative Values:'
      NCTEXT(ICNT)=30
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='31B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Two-Tailed Test'
      NCTITL=15
      ITITL9='H0: P(+) = P(-); Ha: P(+) <> P(-)'
      NCTIT9=33
!
      DO 3130 J=1,NUMCLI
        DO 3140 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 3140   CONTINUE
 3130 CONTINUE
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
      ITITL2(1,3)='Lower'
      NCTIT2(1,3)=5
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
!
      ITITL2(1,4)='Upper'
      NCTIT2(1,4)=5
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Value (>)'
      NCTIT2(3,4)=9
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
      DO 3150 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=0
        IF(I.EQ.1 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 3150 CONTINUE
!
      IWHTML(1)=125
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IINC=1600
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
!
      DO 3160 J=1,NUMALP
!
        AMAT(J,2)=STATV1
        ALPHAT=(1.0 - ALPHA(J))/2.0
        CALL BINPPF(DBLE(ALPHAT),DPAR,NTEMP,DPPF)
        AMAT(J,3)=REAL(DPPF)
        ALPHAT=1.0 - ALPHAT
        CALL BINPPF(DBLE(ALPHAT),DPAR,NTEMP,DPPF)
        AMAT(J,4)=REAL(DPPF)
        IVALUE(J,5)(1:6)='ACCEPT'
        IF(STATV1.LT.AMAT(J,3))IVALUE(J,5)(1:6)='REJECT'
        IF(STATV1.GT.AMAT(J,4))IVALUE(J,5)(1:6)='REJECT'
        NCVALU(J,5)=6
!
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 3160 CONTINUE
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
      ITITL9='H0: P(+) = P(-); Ha: P(+) < P(-)'
      NCTIT9=32
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
!
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
      ITYPCO(4)='ALPH'
!
      NMAX=0
      NUMCOL=4
      DO 3250 I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 3250 CONTINUE
!
      DO 3260 J=1,NUMALP
        ALPHAT=1.0 - ALPHA(J)
        CALL BINPPF(DBLE(ALPHAT),DPAR,NTEMP,DPPF)
        AMAT(J,3)=REAL(DPPF)
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.GE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 3260 CONTINUE
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
      ITITL9='H0: P(+) = P(-); Ha: P(+) > P(-)'
      NCTIT9=32
!
      ITITL2(1,3)='Upper'
      NCTIT2(1,3)=5
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
 3350 CONTINUE
!
      DO 3360 J=1,NUMALP
        ALPHAT=ALPHA(J)
        CALL BINPPF(DBLE(ALPHAT),DPAR,NTEMP,DPPF)
        AMAT(J,3)=REAL(DPPF)
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.LE.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
 3360 CONTINUE
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
!
!               *********************************
!               **  STEP 41--                  **
!               **  CARRY OUT CALCULATIONS     **
!               **  FOR A 2-SAMPLE SIGN TEST   **
!               *********************************
!
 4100 CONTINUE
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPSIG4(Y1,N1,Y2,N2,D0,IWRITE,   &
                  XTEMP1,XTEMP2,MAXNXT,   &
                  Y1MEAN,Y1MED,Y1SD,Y1MAD,   &
                  Y2MEAN,Y2MED,Y2SD,Y2MAD,   &
                  STATV1,STATC1,STATV2,STATC2,RTIES,NTEMP,   &
                  PVAL2T,PVALLT,PVALUT,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      DPAR=0.5D0
      CALL BINPPF(.0005D0,DPAR,NTEMP,DPPF)
      CTL999=DPPF
      CALL BINPPF(.005D0,DPAR,NTEMP,DPPF)
      CUTL99=DPPF
      CALL BINPPF(.025D0,DPAR,NTEMP,DPPF)
      CUTL95=DPPF
      CALL BINPPF(.05D0,DPAR,NTEMP,DPPF)
      CUTL90=DPPF
      CALL BINPPF(.1D0,DPAR,NTEMP,DPPF)
      CUTL80=DPPF
      CALL BINPPF(.25D0,DPAR,NTEMP,DPPF)
      CUTL50=DPPF
      CALL BINPPF(.75D0,DPAR,NTEMP,DPPF)
      CUTU50=DPPF
      CALL BINPPF(.90D0,DPAR,NTEMP,DPPF)
      CUTU80=DPPF
      CALL BINPPF(.95D0,DPAR,NTEMP,DPPF)
      CUTU90=DPPF
      CALL BINPPF(.975D0,DPAR,NTEMP,DPPF)
      CUTU95=DPPF
      CALL BINPPF(.995D0,DPAR,NTEMP,DPPF)
      CUTU99=DPPF
      CALL BINPPF(.9995D0,DPAR,NTEMP,DPPF)
      CTU999=DPPF
!
!               *********************************
!               **   STEP 32--                 **
!               **   WRITE OUT EVERYTHING      **
!               **   FOR A 2-SAMPLE SIGN TEST  **
!               *********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
      ITITLE='Two Sample Sign Test'
      NCTITL=20
      ITITLZ='(+ =>  Y1(i) > Y2(i), - => Y1(i) < Y2(i))'
      NCTITZ=41
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
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: P(+) = P(-)'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: P(+) <> P(-)'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Summary Statistics for Sample One:'
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
      AVALUE(ICNT)=Y1MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Median:'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=Y1MED
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y1SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Median Absolute Deviation:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=Y1MAD
      IDIGIT(ICNT)=NUMDIG
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
      ITEXT(ICNT)='Summary Statistics for Sample Two:'
      NCTEXT(ICNT)=34
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
      ITEXT(ICNT)='Sample Median:'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=Y2MED
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y2SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Median Absolute Deviation:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=Y2MAD
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
      ITEXT(ICNT)='Hypothesized Difference:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=D0
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Positive Differences:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=STATV1
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Negative Differences:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=STATV2
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Ties:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=RTIES
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value for Positive Values:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=STATC1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value for Negative Values:'
      NCTEXT(ICNT)=30
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
!
      NUMROW=ICNT
      DO 4110 I=1,NUMROW
        NTOT(I)=15
 4110 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='21A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Two-Tailed Test'
      NCTITL=15
      ITITL9='H0: P(+) = P(-); Ha: P(+) <> P(-)'
      NCTIT9=33
!
      DO 4130 J=1,NUMCLI
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
      ITITL2(1,3)='Lower'
      NCTIT2(1,3)=5
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
!
      ITITL2(1,4)='Upper'
      NCTIT2(1,4)=5
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Value (>)'
      NCTIT2(3,4)=9
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
      DO 4150 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=0
        IF(I.EQ.1 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 4150 CONTINUE
!
      IWHTML(1)=125
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IINC=1600
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
!
      DO 4160 J=1,NUMALP
!
        AMAT(J,2)=STATV1
        ALPHAT=(1.0 - ALPHA(J))/2.0
        CALL BINPPF(DBLE(ALPHAT),DPAR,NTEMP,DPPF)
        AMAT(J,3)=REAL(DPPF)
        ALPHAT=1.0 - ALPHAT
        CALL BINPPF(DBLE(ALPHAT),DPAR,NTEMP,DPPF)
        AMAT(J,4)=REAL(DPPF)
        IVALUE(J,5)(1:6)='ACCEPT'
        IF(STATV1.LT.AMAT(J,3))IVALUE(J,5)(1:6)='REJECT'
        IF(STATV1.GT.AMAT(J,4))IVALUE(J,5)(1:6)='REJECT'
        NCVALU(J,5)=6
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
      ITITL9='H0: P(+) = P(-); Ha: P(+) < P(-)'
      NCTIT9=32
!
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
!
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
      ITYPCO(4)='ALPH'
!
      NMAX=0
      NUMCOL=4
      DO 4250 I=1,NUMCOL
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
 4250 CONTINUE
!
      DO 4260 J=1,NUMALP
        ALPHAT=1.0 - ALPHA(J)
        CALL BINPPF(DBLE(ALPHAT),DPAR,NTEMP,DPPF)
        AMAT(J,3)=REAL(DPPF)
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
      ITITL9='H0: P(+) = P(-); Ha: P(+) > P(-)'
      NCTIT9=32
!
      ITITL2(1,3)='Upper'
      NCTIT2(1,3)=5
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
        CALL BINPPF(DBLE(ALPHAT),DPAR,NTEMP,DPPF)
        AMAT(J,3)=REAL(DPPF)
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SIG2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIG2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIG2
      SUBROUTINE DPSIG3(X,N,AMU0,IWRITE,   &
                        XTEMP1,XTEMP2,MAXNXT,   &
                        XMEAN,XMED,XSD,XMAD,   &
                        STATV1,STATC1,STATV2,STATC2,RTIES,NTEMP,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE ONE SAMPLE SIGN TEST (AND
!              ALTERNATIVELY THE CDF VALUE).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --AMU0   = THE SINGLE PRECISION VALUE FOR WHICH
!                                THE TEST IS PERFORMED (I.E.,
!                                H0: MU = AMU).
!     OUTPUT ARGUMENTS--STATV1 = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED STATISTIC (BASED ON POSITIVE
!                                DIFFERENCES)
!                     --STATC1 = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE STATV1 STATISTIC.
!                     --STATV2 = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED STATISTIC (BASED ON NEGATIVE
!                                DIFFERENCES)
!                     --STATC2 = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE STATV1 STATISTIC.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             TEST STATISTIC.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--BINCDF.
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
      DOUBLE PRECISION DCDF
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='G3  '
      IERROR='NO'
      IWRTSV=IWRITE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SIG3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSIG3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,AMU0
   52   FORMAT('IBUGA3,ISUBRO,N,AMU0 = ',2(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ************************************
!               **  COMPUTE ONE SAMPLE SIGN TEST  **
!               ************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      STATV1=-99.0
      STATC1=-99.0
      STATV2=-99.0
      STATC2=-99.0
      IWRITE='OFF'
!
      AN=N
!
      IF(N.LE.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN ONE SAMPLE SIGN TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',   &
               'RESPONSE VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      MUST BE TWO OR LARGER.  SUCH WAS NOT THE CASE ',   &
               'HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS    = ',I8,   &
               '.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               *****************************************
!               **  STEP 2--                           **
!               **  COMPUTE THE ONE SAMPLE SIGN TEST.  **
!               *****************************************
!
      CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
      CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
      CALL MEDIAN(X,N,IWRITE,XTEMP1,MAXNXT,XMED,IBUGA3,IERROR)
      CALL MAD(X,N,IWRITE,XTEMP1,XTEMP2,MAXNXT,XMAD,IBUGA3,IERROR)
!
      NTEMP=0
      RMINUS=0.0
      RPLUS=0.0
      RTIES=0.0
      DO 1200 I=1,N
        ADIFF=X(I) - AMU0
        IF(ADIFF.LT.0.0)THEN
          NTEMP=NTEMP+1
          RMINUS=RMINUS+1.0
        ELSEIF(ADIFF.GT.0.0)THEN
          NTEMP=NTEMP+1
          RPLUS=RPLUS+1.0
        ELSE
          RTIES=RTIES+1.0
        ENDIF
 1200 CONTINUE
      STATV1=RPLUS
      CALL BINCDF(DBLE(STATV1),0.5D0,NTEMP,DCDF)
      STATC1=REAL(DCDF)
      STATV2=RMINUS
      CALL BINCDF(DBLE(STATV2),0.5D0,NTEMP,DCDF)
      STATC2=REAL(DCDF)
      PVALLT=STATC1
      PVALUT=STATC2
      PVAL2T=2.0*MIN(STATC1,STATC2)
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
        WRITE(ICOUT,811)N,STATV1
  811   FORMAT('THE VALUE OF THE ONE SAMPLE SIGN TEST OF THE ',I8,   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SIG3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIG3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)STATV1,STATC1,STATV2,STATC2
 9015   FORMAT('STATV1,STATC1,STATV2,STATC2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIG3
      SUBROUTINE DPSIG4(Y1,N1,Y2,N2,D0,IWRITE,   &
                        XTEMP1,XTEMP2,MAXNXT,   &
                        Y1MEAN,Y1MED,Y1SD,Y1MAD,   &
                        Y2MEAN,Y2MED,Y2SD,Y2MAD,   &
                        STATV1,STATC1,STATV2,STATC2,RTIES,NTEMP,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PAIRED TWO SAMPLE SIGN TEST
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
!                     --D0     = THE DIFFERENCE BEING TESTED FOR.
!     OUTPUT ARGUMENTS--STATV1 = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED STATISTIC FOR POSITIVE
!                                DIFFERENCES.
!                     --STATC1 = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE STATV1 STATISTIC.
!     OUTPUT ARGUMENTS--STATV2 = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED STATISTIC FOR NEGATIVE
!                                DIFFERENCES.
!                     --STATC2 = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE STATV1 STATISTIC.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
!             TEST STATISTIC.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--BINCDF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JIM FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
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
      DOUBLE PRECISION DCDF
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='G4  '
      IERROR='NO'
      IWRTSV=IWRITE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SIG4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSIG4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N1,N2,D0
   53   FORMAT('N1,N2,D0 = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,MIN(N1,N2)
          WRITE(ICOUT,56)I,Y1(I),Y2(I)
   56     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE TWO SAMPLE PAIRED SIGN TEST  **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      STATV1=-99.0
      STATC1=-99.0
      STATV2=-99.0
      STATC2=-99.0
      IWRITE='OFF'
!
      IF(N1.NE.N2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      FOR THE TWO SAMPLE SIGN TEST, THE SAMPLE SIZES')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)
  103   FORMAT('      FOR THE RESPONSE VARIABLES MUST BE EQUAL.')
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
  111   FORMAT('***** ERROR IN SIGN TEST--')
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
!               **  COMPUTE THE TWO SAMPLE PAIRED SIGN TEST.    **
!               **************************************************
!
      CALL MEAN(Y1,N1,IWRITE,Y1MEAN,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,Y1SD,IBUGA3,IERROR)
      CALL MEDIAN(Y1,N1,IWRITE,XTEMP1,MAXNXT,Y1MED,IBUGA3,IERROR)
      CALL MAD(Y1,N1,IWRITE,XTEMP1,XTEMP2,MAXNXT,Y1MAD,IBUGA3,IERROR)
      CALL MEAN(Y2,N2,IWRITE,Y2MEAN,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,Y2SD,IBUGA3,IERROR)
      CALL MEDIAN(Y2,N2,IWRITE,XTEMP1,MAXNXT,Y2MED,IBUGA3,IERROR)
      CALL MAD(Y2,N2,IWRITE,XTEMP1,XTEMP2,MAXNXT,Y2MAD,IBUGA3,IERROR)
!
      NTEMP=0
      RMINUS=0.0
      RPLUS=0.0
      RTIES=0.0
      DO 4200 I=1,N1
        ADIFF=Y1(I) - Y2(I) - D0
        IF(ADIFF.LT.0.0)THEN
          NTEMP=NTEMP+1
          RMINUS=RMINUS+1.0
        ELSEIF(ADIFF.GT.0.0)THEN
          NTEMP=NTEMP+1
          RPLUS=RPLUS+1.0
        ELSE
          RTIES=RTIES+1
        ENDIF
 4200 CONTINUE
      STATV1=RPLUS
      CALL BINCDF(DBLE(STATV1),0.5D0,NTEMP,DCDF)
      STATC1=REAL(DCDF)
      STATV2=RMINUS
      CALL BINCDF(DBLE(STATV2),0.5D0,NTEMP,DCDF)
      STATC2=REAL(DCDF)
      PVALLT=STATC1
      PVALUT=STATC2
      PVAL2T=2.0*MIN(STATC1,STATC2)
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
        WRITE(ICOUT,811)STATV1
  811   FORMAT('THE VALUE OF THE TWO SAMPLE SIGN TEST = ',G15.7)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SIG4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIG4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)STATV1,STATC1,STATV2,STATC2
 9015   FORMAT('STATV1,STATC1,STATV2,STATC2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIG4
      SUBROUTINE DPSIG5(ICASAN,STATVA,STATCD,   &
                        PVAL2T,PVALLT,PVALUT,   &
                        CTL999,CUTL99,CUTL95,CUTL90,CUTL80,CUTL50,   &
                        CTU999,CUTU99,CUTU95,CUTU90,CUTU80,CUTU50,   &
                        IFLAGU,IFRST,ILAST,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--UTILITY ROUTINE USED BY DPSIGN TO UPDATE VARIOUS
!              INTERNAL PARAMETERS AFTER A SIGN TEST.
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
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SIG5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSIG5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)STATVA,STATCD,PVAL2T,PVALLT,PVALUT
   53   FORMAT('STATVA,STATCD,PVAL2T,PVALLT,PVALUT = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)CUTL50,CUTL80,CUTL90,CUTL95,CUTL99,CTL999
   54   FORMAT('CUTL50,CUTL80,CUTL90,CUTL95,CUTL99,CTL999 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)CUTU50,CUTU80,CUTU90,CUTU95,CUTU99,CTU999
   55   FORMAT('CUTU50,CUTU80,CUTU90,CUTU95,CUTU99,CTU999 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)ICASAN
   58   FORMAT('ICASAN = ',A4)
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
  295     FORMAT(11X,'STATVAL',8X,'STATCDF',8X,   &
                 'PVAL2T',9X,'PVALLT',X,'PVALUT',   &
                 7X,'CUTLOW50',7X,'CUTLOW80',7X,'CUTLOW90',   &
                 7X,'CUTLOW95',7X,'CUTLOW99',7X,'CUTLO999',   &
                 7X,'CUTUPP50',7X,'CUTUPP80',7X,'CUTUPP90',   &
                 7X,'CUTUPP95',7X,'CUTUPP99',7X,'CUTUP999')
        ENDIF
        WRITE(IOUNI1,299)STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                         CUTL50,CUTL80,CUTL90,CUTL95,CUTL99,CTL999,   &
                         CUTU50,CUTU80,CUTU90,CUTU95,CUTU99,CTU999
  299   FORMAT(17E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(STATVA.NE.CPUMIN)THEN
          IH='STAT'
          IH2='VAL '
          VALUE0=STATVA
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(STATCD.NE.CPUMIN)THEN
          IH='STAT'
          IH2='CDF '
          VALUE0=STATCD
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
        IF(CUTU50.NE.CPUMIN)THEN
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SIG5')THEN
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SIG5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPSIG5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIG5
      SUBROUTINE DPSIIS(ISTART,IW,NW,IHOUT,NOUT,IBUGA3,IERROR)
!
!     PURPOSE--INSERT A STRING INTO IW.
!              THE STRING IS LOCATED IN IHOUT(.).
!              THE LOCATION IN IW(.) WHERE THE STRING
!              IS TO BE INSERTED IS AT ISTART.
!              THE CONTENTS OF IW(ISTART) WILL BE OVERWRITTEN.
!              THE CONTENTS OF IW(ISTART+1), IW(ISTART+2), ETC.
!              WILL BE DISPLACED ACCORDING TO THE LENGTH
!              OF THE INSERTED STRING.
!     NOTE--THE INPUT ARGUMENTS IW(.) AND NW
!           AND ALTERED BY THIS SUBROUTINE.
!     NOTE--IF NOUT = 0 OR NEGATIVE, THEN THE CONVENTION
!           HAS BEEN TAKEN TO SHIFT THE REMAINING
!           STRING IN IW(.) STARTING AT ISTART+1
!           OVER 1 LOCATION SO THAT IT WOULD THEN
!           START AT ISTART.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IHOUT
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      DIMENSION IW(*)
      DIMENSION IHOUT(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='IS  '
      IERROR='NO'
!
      ISHIFT=(-999)
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,81)
   81 FORMAT('***** AT THE BEGINNING OF DPSIIS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,82)ISTART,NW,NOUT
   82 FORMAT('ISTART,NW,NOUT = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,83)(IHOUT(I),I=1,NOUT)
   83 FORMAT('(IHOUT(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,84)(IW(I),I=1,NW)
   84 FORMAT('(IW(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               *****************************
!               **  STEP 1--               **
!               **  INSERT    THE STRING.  **
!               *****************************
!
      IF(NOUT.GE.0)ISHIFT=NOUT-1
      IF(NOUT.LT.0)ISHIFT=(-1)
      IMIN=ISTART+1
      IMAX=NW
      IF(IMIN.GT.IMAX)GO TO 150
      DO 100 I=IMIN,IMAX
      IPS=I+ISHIFT
      IREV=IMAX-I+IMIN
      IREVPS=IREV+ISHIFT
      IF(IREVPS.GE.IREV)IW(IREVPS)=IW(IREV)
      IF(IREVPS.LT.IREV)IW(IPS)=IW(I)
  100 CONTINUE
  150 CONTINUE
      NW=NW+ISHIFT
!
      J=ISTART-1
      IF(NOUT.LE.0)GO TO 250
      DO 200 I=1,NOUT
      J=J+1
      IW(J)=IHOUT(I)
  200 CONTINUE
  250 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NW
 9012   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)(IW(I),I=1,MIN(115,NW))
 9013   FORMAT('(IW(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIIS
      SUBROUTINE DPSING(IHARG,NUMARG,IDEFPR,IHMXPR,   &
                        IPREC,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE PREICSION SWITCH
!              AS SINGLE PRECISION.
!              THIS IN TURN SPECIFIES THAT SUBSEQUENT
!              CALCULATIONS WILL ALL BE CARRIED OUT
!              IN SINGLE PRECISION.
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
!                 GAITHERSBUG, MD 20899-8980
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
!-----COMMON---------------------------------------------------------
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
      IHOLD='SING'
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
 1172 FORMAT('***** ERROR IN DPSING--')
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
      END SUBROUTINE DPSING
      SUBROUTINE DPSIP0(IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--ELIMINATE SUPERFLUOUS MULTIPLICATIONS
!              (BUT NOT DIVISIONS) BY 0 AND BY (0)   .
!     NOTE--THE INPUT ARGUEMNTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      CHARACTER*4 IFOUND
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)---------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='P0  '
      IERROR='NO'
!
      IMIN=1
      I2=1
      IM1=1
      IP1=1
      KREV=1
      K2=1
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIP0--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               **  FOR THE SEARCH FOR    1    .       **
!               *****************************************
!
      IFOUND='NO'
      NUMPAS=1000
      DO 1100 IPASS=1,NUMPAS
      ISTEPN='1100'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ****************************
!               **  STEP 2--              **
!               **  SEARCH FOR    0    .  **
!               ****************************
!
      NWM1=NW-1
      NWP1=NW+1
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2.AND.IFOUND.EQ.'YES')IMIN=I2
      IF(IPASS.GE.2.AND.IFOUND.EQ.'NO')IMIN=I2+1
      IFOUND='NO'
      IF(IMIN.GE.NWP1)GO TO 1990
      DO 200 I=IMIN,NW
      I2=I
      IF(IW(I).EQ.'0   ')GO TO 210
  200 CONTINUE
      GO TO 990
!
  210 CONTINUE
      ISTEPN='210'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      I=I2
      IM1=I-1
      IM2=I-2
      IM3=I-3
      IP1=I+1
      IP2=I+2
      IP3=I+3
!
!               ***********************************
!               **  STEP 3--                     **
!               **  TEST FOR THE    *0    CASE.  **
!               ***********************************
!
      IF(IM1.LE.0)GO TO 390
      IF(IM1.EQ.1.AND.IW(IM1).EQ.'*   ')GO TO 310
      IF(IM1.GE.2.AND.IW(IM1).EQ.'*   '.AND.IW(IM2).NE.'*   ')GO TO 310
!CCCC IF(IW(IM1).EQ.'/   ')GO TO 310
      GO TO 100
!
  310 CONTINUE
      ISTEPN='310'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IP1.GE.NWP1)GO TO 320
      IF(IW(IP1).EQ.'+   ')GO TO 320
      IF(IW(IP1).EQ.'-   ')GO TO 320
      IF(IP1.EQ.NW.AND.IW(IP1).EQ.'*   ')GO TO 320
      IF(IP1.LE.NWM1.AND.IW(IP1).EQ.'*   '.AND.IW(IP2).NE.'*   ')GO TO 320
      IF(IW(IP1).EQ.'/   ')GO TO 320
      IF(IW(IP1).EQ.')   ')GO TO 320
      GO TO 100
!
  320 CONTINUE
      ISTEPN='320'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IB=IM1
      IRIGHT=IB-1
      IF(IRIGHT.LE.0)GO TO 100
      ILEFT=IRIGHT
      IF(IW(IRIGHT).EQ.')   ')GO TO 333
      GO TO 339
  333 CONTINUE
      ISTEPN='333'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISUM=0
      DO 335 K=1,IRIGHT
      KREV=IRIGHT-K+1
      IF(IW(KREV).EQ.')   ')ISUM=ISUM+1
      IF(IW(KREV).EQ.'(   ')ISUM=ISUM-1
      IF(ISUM.EQ.0)GO TO 337
  335 CONTINUE
      ILEFT=0
  337 CONTINUE
      ILEFT=KREV
  339 CONTINUE
      ISTEPN='339'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTART=ILEFT+1
      ISTOP=I
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      IW(I2)='0   '
      IFOUND='YES'
      GO TO 1100
!
  390 CONTINUE
      ISTEPN='390'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ***********************************
!               **  STEP 4--                     **
!               **  TEST FOR THE    0*    CASE.  **
!               ***********************************
!
      IF(IP1.GE.NWP1)GO TO 490
      IF(IP1.EQ.NW.AND.IW(IP1).EQ.'*   ')GO TO 410
      IF(IP1.LE.NWM1.AND.IW(IP1).EQ.'*   '.AND.IW(IP2).NE.'*   ')GO TO 410
      IF(IW(IP1).EQ.'/   ')GO TO 410
      GO TO 100
!
  410 CONTINUE
      ISTEPN='410'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IM1.LE.0)GO TO 420
      IF(IW(IM1).EQ.'+   ')GO TO 420
      IF(IW(IM1).EQ.'-   ')GO TO 420
      IF(IM1.EQ.1.AND.IW(IM1).EQ.'*   ')GO TO 420
      IF(IM1.GE.2.AND.IW(IM1).EQ.'*   '.AND.IW(IM2).NE.'*   ')GO TO 420
!CCCC IF(IW(IM1).EQ.'/   ')GO TO 420
      IF(IW(IM1).EQ.'(   ')GO TO 420
      GO TO 100
!
  420 CONTINUE
      ISTEPN='420'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IB=IP1
      ILEFT=IB+1
      IF(ILEFT.GE.NWP1)GO TO 100
      IRIGHT=ILEFT
      IF(IW(ILEFT).EQ.'(   ')GO TO 433
      GO TO 439
  433 CONTINUE
      ISTEPN='433'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISUM=0
      DO 435 K=ILEFT,NW
      K2=K
      IF(IW(K).EQ.'(   ')ISUM=ISUM-1
      IF(IW(K).EQ.')   ')ISUM=ISUM+1
      IF(ISUM.EQ.0)GO TO 437
  435 CONTINUE
      IRIGHT=NW+1
  437 CONTINUE
      IRIGHT=K2
  439 CONTINUE
      ISTEPN='439'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTART=I+1
      ISTOP=IRIGHT
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      IW(I2)='0   '
      IFOUND='YES'
      GO TO 1100
!
  490 CONTINUE
      ISTEPN='490'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
  100 CONTINUE
      ISTEPN='100'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
  990 CONTINUE
      ISTEPN='990'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NWM1=NW-1
      IF(IM1.LE.0)GO TO 1100
      IF(IP1.GE.NWP1)GO TO 1100
      IF(IW(IM1).EQ.'(   '.AND.IW(I).EQ.'0   '.AND.   &
         IW(IP1).EQ.')   ')GO TO 1210
      GO TO 1100
!
 1210 CONTINUE
      ISTEPN='1210'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IM1=I-1
      IM2=I-2
      IM3=I-3
      IP1=I+1
      IP2=I+2
      IP3=I+3
!
!               ***********************************
!               **  STEP 13--                    **
!               **  TEST FOR THE   *(0)   CASE.  **
!               ***********************************
!
      IF(IM2.LE.0)GO TO 1390
      IF(IM2.EQ.1.AND.IW(IM2).EQ.'*   ')GO TO 1310
      IF(IM2.GE.2.AND.IW(IM2).EQ.'*   '.AND.IW(IM3).NE.'*   ')GO TO 1310
!CCCC IF(IW(IM2).EQ.'/   ')GO TO 1310
      GO TO 1100
!
 1310 CONTINUE
      ISTEPN='1310'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IP2.GE.NWP1)GO TO 1320
      IF(IW(IP2).EQ.'+   ')GO TO 1320
      IF(IW(IP2).EQ.'-   ')GO TO 1320
      IF(IP2.EQ.NW.AND.IW(IP2).EQ.'*   ')GO TO 1320
      IF(IP2.LE.NWM1.AND.IW(IP2).EQ.'*   '.AND.IW(IP3).NE.'*   ')   &
      GO TO 1320
      IF(IW(IP2).EQ.'/   ')GO TO 1320
      IF(IW(IP2).EQ.')   ')GO TO 1320
      GO TO 1100
!
 1320 CONTINUE
      ISTEPN='1320'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IB=IM2
      IRIGHT=IB-1
      IF(IRIGHT.LE.0)GO TO 1100
      ILEFT=IRIGHT
      IF(IW(IRIGHT).EQ.')   ')GO TO 1333
      GO TO 1339
 1333 CONTINUE
      ISTEPN='1333'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISUM=0
      DO 1335 K=1,IRIGHT
      KREV=IRIGHT-K+1
      IF(IW(KREV).EQ.')   ')ISUM=ISUM+1
      IF(IW(KREV).EQ.'(   ')ISUM=ISUM-1
      IF(ISUM.EQ.0)GO TO 1337
 1335 CONTINUE
      ILEFT=0
 1337 CONTINUE
      ILEFT=KREV
 1339 CONTINUE
      ISTEPN='1339'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTART=ILEFT+1
      ISTOP=IP1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      IW(I2)='0   '
      IFOUND='YES'
      GO TO 1100
!
 1390 CONTINUE
      ISTEPN='1390'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ***********************************
!               **  STEP 14--                    **
!               **  TEST FOR THE   (0)*   CASE.  **
!               ***********************************
!
      IF(IP2.GE.NWP1)GO TO 1490
      IF(IP2.EQ.NW.AND.IW(IP2).EQ.'*   ')GO TO 1410
      IF(IP2.LE.NWM1.AND.IW(IP2).EQ.'*   '.AND.IW(IP3).NE.'*   ')   &
      GO TO 1410
      IF(IW(IP2).EQ.'/   ')GO TO 1410
      GO TO 1100
!
 1410 CONTINUE
      ISTEPN='1410'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IM2.LE.0)GO TO 1420
      IF(IW(IM2).EQ.'+   ')GO TO 1420
      IF(IW(IM2).EQ.'-   ')GO TO 1420
      IF(IM2.EQ.1.AND.IW(IM2).EQ.'*   ')GO TO 1420
      IF(IM2.GE.2.AND.IW(IM2).EQ.'*   '.AND.IW(IM3).NE.'*   ')GO TO 1420
!CCCC IF(IW(IM2).EQ.'/   ')GO TO 1420
      IF(IW(IM2).EQ.'(   ')GO TO 1420
      GO TO 1100
!
 1420 CONTINUE
      ISTEPN='1420'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IB=IP2
      ILEFT=IB+1
      IF(ILEFT.GE.NWP1)GO TO 1100
      IRIGHT=ILEFT
      IF(IW(ILEFT).EQ.'(   ')GO TO 1433
      GO TO 1439
 1433 CONTINUE
      ISTEPN='1433'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ISUM=0
      DO 1435 K=ILEFT,NW
      K2=K
      IF(IW(K).EQ.'(   ')ISUM=ISUM-1
      IF(IW(K).EQ.')   ')ISUM=ISUM+1
      IF(ISUM.EQ.0)GO TO 1437
 1435 CONTINUE
      IRIGHT=NW+1
 1437 CONTINUE
      IRIGHT=K2
 1439 CONTINUE
      ISTEPN='1439'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTART=I
      ISTOP=IRIGHT
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      IW(I2)='0   '
      IFOUND='YES'
      GO TO 1100
!
 1490 CONTINUE
      ISTEPN='1490'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
 1100 CONTINUE
      ISTEPN='1101'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
 1990 CONTINUE
      ISTEPN='1990'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIP0--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NW
 9012   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NW
          WRITE(ICOUT,9016)I,IW(I)
 9016     FORMAT('I,IW(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIP0
      SUBROUTINE DPSIP1(IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--ELIMINATE SUPERFLUOUS MULTIPLICATIONS
!              (AND DIVISIONS) BY 1 AND BY (1)   .
!     NOTE--THE INPUT ARGUEMNTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)----------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='P1  '
      IERROR='YES'
!
      IMIN=1
      I2=1
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIP1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.) **
!               **  FOR THE SEARCH FOR    1    .       **
!               *****************************************
!
      NUMPAS=1000
      DO 100 IPASS=1,NUMPAS
!
!               ****************************
!               **  STEP 2--              **
!               **  SEARCH FOR    1    .  **
!               ****************************
!
      NWM1=NW-1
      NWP1=NW+1
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=I2+1
      IF(IMIN.GE.NWP1)GO TO 990
      DO 200 I=IMIN,NW
      I2=I
      IF(IW(I).EQ.'1   ')GO TO 210
  200 CONTINUE
      GO TO 990
!
  210 CONTINUE
      I=I2
      IM1=I-1
      IM2=I-2
      IM3=I-3
      IP1=I+1
      IP2=I+2
      IP3=I+3
!
!               ***********************************
!               **  STEP 3--                     **
!               **  TEST FOR THE    *1    CASE.  **
!               ***********************************
!
      IF(IM1.LE.0)GO TO 390
      IF(IM1.EQ.1.AND.IW(IM1).EQ.'*   ')GO TO 310
      IF(IM1.GE.2.AND.IW(IM1).EQ.'*   '.AND.IW(IM2).NE.'*   ')GO TO 310
      IF(IW(IM1).EQ.'/   ')GO TO 310
      GO TO 100
!
  310 CONTINUE
      IF(IP1.GE.NWP1)GO TO 320
      IF(IW(IP1).EQ.'+   ')GO TO 320
      IF(IW(IP1).EQ.'-   ')GO TO 320
      IF(IP1.EQ.NW.AND.IW(IP1).EQ.'*   ')GO TO 320
      IF(IP1.LE.NWM1.AND.IW(IP1).EQ.'*   '.AND.IW(IP2).NE.'*   ')GO TO 320
      IF(IW(IP1).EQ.'/   ')GO TO 320
      IF(IW(IP1).EQ.')   ')GO TO 320
      GO TO 100
!
  320 CONTINUE
      ISTART=IM1
      ISTOP=I
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 100
!
  390 CONTINUE
!
!               ***********************************
!               **  STEP 4--                     **
!               **  TEST FOR THE    1*    CASE.  **
!               ***********************************
!
      IF(IP1.GE.NWP1)GO TO 490
      IF(IP1.EQ.NW.AND.IW(IP1).EQ.'*   ')GO TO 410
      IF(IP1.LE.NWM1.AND.IW(IP1).EQ.'*   '.AND.IW(IP2).NE.'*   ')GO TO 410
      GO TO 100
!
  410 CONTINUE
      IF(IM1.LE.0)GO TO 420
      IF(IW(IM1).EQ.'+   ')GO TO 420
      IF(IW(IM1).EQ.'-   ')GO TO 420
      IF(IM1.EQ.1.AND.IW(IM1).EQ.'*   ')GO TO 420
      IF(IM1.GE.2.AND.IW(IM1).EQ.'*   '.AND.IW(IM2).NE.'*   ')GO TO 420
      IF(IW(IM1).EQ.'/   ')GO TO 420
      IF(IW(IM1).EQ.'(   ')GO TO 420
      GO TO 100
!
  420 CONTINUE
      ISTART=I
      ISTOP=IP1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 100
!
  490 CONTINUE
!
  100 CONTINUE
!
  990 CONTINUE
!
!               *****************************************
!               **  STEP 11--                          **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.) **
!               **  FOR THE SEARCH FOR    (1)    .     **
!               *****************************************
!
      NUMPAS=1000
      DO 1100 IPASS=1,NUMPAS
!
!               ****************************
!               **  STEP 12--             **
!               **  SEARCH FOR   (1)   .  **
!               ****************************
!
      NWM1=NW-1
      NWP1=NW+1
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=I2+1
      NWM1=NW-1
      IF(IMIN.LE.0)GO TO 1990
!CCCC IF(IMIN.GE.NWP1M1)GO TO 1990
      IF(IMIN.GE.NW)GO TO 1990
      DO 1200 I=IMIN,NWM1
      I2=I
      IM1=I-1
      IP1=I+1
      IF(IW(IM1).EQ.'(   '.AND.IW(I).EQ.'1   '.AND.   &
         IW(IP1).EQ.')   ')GO TO 1210
 1200 CONTINUE
      GO TO 1990
!
 1210 CONTINUE
      I=I2
      IM1=I-1
      IM2=I-2
      IM3=I-3
      IP1=I+1
      IP2=I+2
      IP3=I+3
!
!               ***********************************
!               **  STEP 13--                    **
!               **  TEST FOR THE   *(1)   CASE.  **
!               ***********************************
!
      IF(IM2.LE.0)GO TO 1390
      IF(IM2.EQ.1.AND.IW(IM2).EQ.'*   ')GO TO 1310
      IF(IM2.GE.2.AND.IW(IM2).EQ.'*   '.AND.IW(IM3).NE.'*   ')GO TO 1310
      IF(IW(IM2).EQ.'/   ')GO TO 1310
      GO TO 1100
!
 1310 CONTINUE
      IF(IP2.GE.NWP1)GO TO 1320
      IF(IW(IP2).EQ.'+   ')GO TO 1320
      IF(IW(IP2).EQ.'-   ')GO TO 1320
      IF(IP2.EQ.NW.AND.IW(IP2).EQ.'*   ')GO TO 1320
      IF(IP2.LE.NWM1.AND.IW(IP2).EQ.'*   '.AND.IW(IP3).NE.'*   ')   &
      GO TO 1320
      IF(IW(IP2).EQ.'/   ')GO TO 1320
      IF(IW(IP2).EQ.')   ')GO TO 1320
      GO TO 1100
!
 1320 CONTINUE
      ISTART=IM2
      ISTOP=IP1
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 1100
!
 1390 CONTINUE
!
!               ***********************************
!               **  STEP 14--                    **
!               **  TEST FOR THE   (1)*   CASE.  **
!               ***********************************
!
      IF(IP2.GE.NWP1)GO TO 1490
      IF(IP2.EQ.NW.AND.IW(IP2).EQ.'*   ')GO TO 1410
      IF(IP2.LE.NWM1.AND.IW(IP2).EQ.'*   '.AND.IW(IP3).NE.'*   ')   &
      GO TO 1410
      GO TO 1100
!
 1410 CONTINUE
      IF(IM2.LE.0)GO TO 1420
      IF(IW(IM2).EQ.'+   ')GO TO 1420
      IF(IW(IM2).EQ.'-   ')GO TO 1420
      IF(IM2.EQ.1.AND.IW(IM2).EQ.'*   ')GO TO 1420
      IF(IM2.GE.2.AND.IW(IM2).EQ.'*   '.AND.IW(IM3).NE.'*   ')   &
      GO TO 1420
      IF(IW(IM2).EQ.'/   ')GO TO 1420
      IF(IW(IM2).EQ.'(   ')GO TO 1420
      GO TO 1100
!
 1420 CONTINUE
      ISTART=IM1
      ISTOP=IP2
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      I2=ISTART-1
      GO TO 1100
!
 1490 CONTINUE
!
 1100 CONTINUE
!
 1990 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIP1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NW
 9012   FORMAT('NW = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NW
          WRITE(ICOUT,9016)I,IW(I)
 9016     FORMAT('I,IW(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIP1
      SUBROUTINE DPSIPA(IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--SIMPLIFY AN ENTIRE EXPRESSION BY REMOVING
!              ALL REDUNDANT PARENTHESES.
!     NOTE--THE INPUT ARGUMENTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY  1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)---------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='PA  '
      IERROR='NO'
!
      IMIN=1
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               *****************************************
!
      NUMPAS=1000
      DO 100 IPASS=1,NUMPAS
      ISUM=0
!
!               **********************************************
!               **  STEP 3--                                **
!               **  SEARCH FOR THE NEXT RIGHT PARENTHESIS.  **
!               **********************************************
!
      IF(IPASS.EQ.1)IMIN=1
      IF(IPASS.GE.2)IMIN=IRIGHT+1
      IF(IMIN.GT.NW)GO TO 9000
!
      DO 300 I=IMIN,NW
      I2=I
      IF(IW(I).EQ.')   ')GO TO 350
  300 CONTINUE
      GO TO 9000
  350 CONTINUE
      IRIGHT=I2
      ISUM=ISUM+1
!
!               **********************************************
!               **  STEP 4--                                **
!               **  SEARCH FOR THE NEXT (IN REVERSE ORDER)  **
!               **  LEFT PARENTHESIS.                       **
!               **********************************************
!
      IMAX=IRIGHT-1
      IF(IMAX.LT.1)GO TO 9000
!
      DO 400 I=1,IMAX
      IREV=IMAX-I+1
      IF(IW(IREV).EQ.'(   ')GO TO 401
      IF(IW(IREV).EQ.')   ')GO TO 402
      GO TO 400
  401 CONTINUE
      ISUM=ISUM-1
      IF(ISUM.EQ.0)ILEFT=IREV
      IF(ISUM.EQ.0)GO TO 490
      GO TO 400
  402 CONTINUE
      ISUM=ISUM+1
      GO TO 400
  400 CONTINUE
!
      WRITE(ICOUT,411)
  411 FORMAT('***** ERROR IN DPSIPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,412)
  412 FORMAT('      NUMBER OF LEFT PARENTHESES DOES NOT EQUAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,413)
  413 FORMAT('      NUMBER OF RIGHT PARENTHESES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,414)
  414 FORMAT('      THE STRING BEING OPERATED ON IS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      DO 415 I=1,NW
      WRITE(ICOUT,416)I,IW(I)
  416 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  415 CONTINUE
      IERROR='YES'
      GO TO 9000
!
  490 CONTINUE
!
!               *************************************
!               **  STEP 5--                       **
!               **  REDUCE REDUNDANT PARENTHESES   **
!               **  IN THIS VICINITY.              **
!               *************************************
!
      CALL DPSIEP(ILEFT,IRIGHT,IW,NW,IBUGA3,IERROR)
!
  100 CONTINUE
!
!               ****************
!               **  STEP 6--  **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSIPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)NW
 9012 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NW
      WRITE(ICOUT,9016)I,IW(I)
 9016 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPSIPA
      SUBROUTINE DPSIRS(IW1,NW1,ISTAR1,ISTOP1,IW2,NW2,ISTAR2,ISTOP2,   &
                        IBUGA3,IERROR)
!
!     PURPOSE--REPLACE THE STRING IN IW1(.)
!              RESIDING IN LOCATIONS ISTAR1 TO ISTOP1
!              (INCLUSIVELY)
!              BY THE STRING IN IW2(.)
!              RESIDING IN LOCATIONS ISTAR2 TO ISTOP2
!              (INCLUSIVELY)
!              DISPLACE THE ELEMENTS IN IW1(.)
!              BEYOND THE FIELD OF INTEREST APPROPRIATELY.
!              ACCORDINGLY ADJUST THE VALUE OF NW1 =
!              THE NUMBER OF ELEMENTS IN IW1(.).
!     NOTE--THE INPUT ARGUMENTS IW1(.) AND NW1
!           AND ALTERED BY THIS SUBROUTINE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JANUARY  1979.
!     UPDATED         --JUNE      1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IW1
      CHARACTER*4 IW2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IW1(*)
      DIMENSION IW2(*)
!
!-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPSIRS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISTAR1,ISTOP1,NW1
   52 FORMAT('ISTAR1,ISTOP1,NW1 = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)(IW1(I),I=1,MIN(NW1,100))
   53 FORMAT('(IW1(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)ISTAR2,ISTOP2,NW2
   54 FORMAT('ISTAR2,ISTOP2,NW2 = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)(IW2(I),I=1,MIN(100,NW2))
   55 FORMAT('(IW2(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               *****************************
!               **  STEP 1--               **
!               **  INSERT    THE STRING.  **
!               *****************************
!
      ILEN1=ISTOP1-ISTAR1+1
      ILEN2=ISTOP2-ISTAR2+1
      ISHIFT=ILEN2-ILEN1
      IMIN=ISTOP1+1
      IMAX=NW1
      IF(IMIN.GT.IMAX)GO TO 150
      DO 100 I=IMIN,IMAX
      IPS=I+ISHIFT
      IREV=IMAX-I+IMIN
      IREVPS=IREV+ISHIFT
      IF(IREVPS.GE.IREV)IW1(IREVPS)=IW1(IREV)
      IF(IREVPS.LT.IREV)IW1(IPS)=IW1(I)
  100 CONTINUE
  150 CONTINUE
      NW1=NW1+ISHIFT
!
      J=ISTAR1-1
      DO 200 I=ISTAR2,ISTOP2
      J=J+1
      IW1(J)=IW2(I)
  200 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSIRS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ISTAR1,ISTOP1,NW1
 9012   FORMAT('ISTAR1,ISTOP1,NW1 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)(IW1(I),I=1,MIN(NW1,100))
 9013   FORMAT('(IW1(.) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ISTAR2,ISTOP2,NW2
 9014   FORMAT('ISTAR2,ISTOP2,NW2 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)(IW2(I),I=1,MIN(NW2,100))
 9015   FORMAT('(IW2(.) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)ILEN1,ILEN2,ISHIFT
 9021   FORMAT('ILEN1,ILEN2,ISHIFT = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSIRS
      SUBROUTINE DPSISI(IW,NW,IBUGA3,IERROR)
!
!     PURPOSE--ELIMINATE SUPERFLUOUS JUXTAPOSITIONS
!              OF + AND - SIGNS.
!     NOTE--THE INPUT ARGUEMNTS IW(.) AND NW
!           ARE ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY  1979.
!     UPDATED         --JANUARY  1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      DIMENSION IW(*)
!
!-----COMMON VARIABLES (GENERAL)----------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSISI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,IW(I)
   56 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************************
!               **  STEP 1--                           **
!               **  SET UP A LARGE DO LOOP             **
!               **  FOR MULTIPLE PASSES THROUGH IW(.)  **
!               **  FOR THE SEARCH FOR SIGNS  .        **
!               *****************************************
!
      NUMPAS=1000
      DO 100 IPASS=1,NUMPAS
!
!               ****************************
!               **  STEP 2--              **
!               **  SEARCH FOR  SIGNS  .  **
!               ****************************
!
      NWM1=NW-1
      IF(NWM1.LT.1)GO TO 9000
      DO 200 I=1,NW
      I2=I
      IP1=I+1
      IF(IW(I).EQ.'+   '.AND.IW(IP1).EQ.'+   ')GO TO 210
      IF(IW(I).EQ.'+   '.AND.IW(IP1).EQ.'-   ')GO TO 220
      IF(IW(I).EQ.'-   '.AND.IW(IP1).EQ.'+   ')GO TO 230
      IF(IW(I).EQ.'-   '.AND.IW(IP1).EQ.'-   ')GO TO 240
  200 CONTINUE
      GO TO 9000
!
  210 CONTINUE
      ISTART=IP1
      ISTOP=ISTART
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      GO TO 100
!
  220 CONTINUE
      ISTART=I2
      ISTOP=ISTART
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      GO TO 100
!
  230 CONTINUE
      ISTART=IP1
      ISTOP=ISTART
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      GO TO 100
!
  240 CONTINUE
      ISTART=IP1
      ISTOP=ISTART
      CALL DPSIES(ISTART,ISTOP,IW,NW,IBUGA3,IERROR)
      IW(I2)='+   '
      GO TO 100
!
  100 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSISI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)NW
 9012 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9015 I=1,NW
      WRITE(ICOUT,9016)I,IW(I)
 9016 FORMAT('I,IW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPSISI
      SUBROUTINE DPSIS2(ISTART,ISTOP,IW,NW,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--SIMPLIFY CERTAIN STRINGS
!              INVOLVING SIMPLE (= BINARY) ARITHMETIC OPERATIONS
!              (ADDITION, SUBTRACTION, MULTIPLICATION,
!              AND EXPONENTIATION--NOT DIVISION)
!              OF 2 SINGLE-DIGIT INTEGERS WITH THE OUTPUT BEING NECESSARILY INTE
!              THE INTERNAL STRING STARTS WITH ISTART (INCLUSIVE)
!              AND STOPS WITH ISTOP (INCLUSIVE).
!              ALSO, IF THE STRING HAS LENGTH OF ONLY 1
!              (OR IS REDUCED TO LENGTH OF ONLY 1),
!              THEN AN ADDITIONAL STEP IS TAKEN IN
!              THE ELIMINATION OF THE ASSUMED PARENTHESES AT
!              LOCATIONS ISTART-1 AND ISTOP+1.
!     NOTE--THE INPUT ARGUMENTS IW(.) AND NW
!           AND ALTERED BY THIS SUBROUTINE.
!     ORIGINAL VERSION--JANUARY   1979.
!     UPDATED         --JANUARY   1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      CHARACTER*4 IOP
      CHARACTER*4 IVALID
      CHARACTER*4 IHOUT
!
      DIMENSION IW(*)
!
      DIMENSION IHOUT(80)
!
!-----COMMON VARIABLES (GENERAL)----------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSI'
      ISUBN2='S2  '
      IERROR='NO'
      IOP='UNKN'
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'SIS2')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSIS2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,ISUBRO,IERROR
   52 FORMAT('IBUGA3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ISTART,ISTOP,NW
   53 FORMAT('ISTART,ISTOP,NW = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)(IW(I),I=1,MIN(NW,100))
   54 FORMAT('(IW(.) = ',100A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               *******************************************
!               **  STEP 0--                             **
!               **  DETERMINE THE LENGTH OF THE STRING.  **
!               **  CHECK FOR LENGTH OF STRING = 1.      **
!               **  IF FOUND, AND IF AN INTEGER,         **
!               **  THEN ELIMINATE THE LEADING           **
!               **  AND TRAILING PARENTHESES.            **
!               **  CHECK FOR LENGTH OF STRING = 3.      **
!               **  IF FOUND, CONTINUE ON.               **
!               *******************************************
!
      ILEN=ISTOP-ISTART+1
      IF(ILEN.EQ.1)GO TO 210
      IF(ILEN.EQ.3)GO TO 230
      IF(ILEN.EQ.4)GO TO 240
      GO TO 9000
!
!               *********************************
!               **  STEP 1--                   **
!               **  TREAT THE LENGTH = 1 CASE  **
!               *********************************
!
  210 CONTINUE
!
!               **********************************
!               **  STEP 1.1--                  **
!               **  CONVERT THE WORD            **
!               **  FROM HOLLARITH TO INTEGER.  **
!               **********************************
!
      IMIN=ISTART
      IMAX=ISTART
      CALL DPCOHI(IMIN,IMAX,IW,NW,IVALID,VAL1,IVAL1,   &
      IBUGA3,IERROR)
      IF(IERROR.EQ.'YES'.OR.IVALID.EQ.'NO')GO TO 9000
!
!               ********************************************
!               **  STEP 1.2--                            **
!               **  CHECK FOR PRIOR AND POST PARENTHESES  **
!               **  AND ELIMINATE THEM.                   **
!               ********************************************
!
      ISTAM1=ISTART-1
      ISTAP1=ISTART+1
      IF(ISTAM1.LT.1)GO TO 9000
      IF(ISTAP1.GT.NW)GO TO 9000
      IF(IW(ISTAM1).EQ.'(   '.AND.IW(ISTAP1).EQ.')   ')GO TO 215
      GO TO 9000
!
  215 CONTINUE
      JMIN=ISTART+1
      JMAX=ISTART+1
      CALL DPSIES(JMIN,JMAX,IW,NW,IBUGA3,IERROR)
      JMIN=ISTART-1
      JMAX=ISTART-1
      CALL DPSIES(JMIN,JMAX,IW,NW,IBUGA3,IERROR)
      GO TO 9000
!
!               **********************************
!               **  STEP 3--                    **
!               **  TREAT THE LENGTH = 3 CASE.  **
!               **********************************
!
  230 CONTINUE
!
!               ***************************************
!               **  STEP 3.1--                       **
!               **  CONVERT FIRST WORD OUT OF THE 3  **
!               **  FROM HOLLARITH TO INTEGER.       **
!               **  CONVERT LAST  WORD OUT OF THE 3  **
!               **  FROM HOLLARITH TO INTEGER.       **
!               **  DETERMINE TYPE OF OPERATION      **
!               **  BY EXAMINING THE SECOND WORD     **
!               **  OUT OF THE 3.                    **
!               ***************************************
!
      IMIN=ISTART
      IMAX=ISTART
      CALL DPCOHI(IMIN,IMAX,IW,NW,IVALID,VAL1,IVAL1,   &
      IBUGA3,IERROR)
      IF(IERROR.EQ.'YES'.OR.IVALID.EQ.'NO')GO TO 9000
!
      IMIN=ISTOP
      IMAX=ISTOP
      CALL DPCOHI(IMIN,IMAX,IW,NW,IVALID,VAL2,IVAL2,   &
      IBUGA3,IERROR)
      IF(IERROR.EQ.'YES'.OR.IVALID.EQ.'NO')GO TO 9000
!
      ILOC=ISTART+1
      IF(IW(ILOC).EQ.'+   ')IOP='+   '
      IF(IW(ILOC).EQ.'-   ')IOP='-   '
      IF(IW(ILOC).EQ.'*   ')IOP='*   '
!CCCC IF(IW(ILOC).EQ.'/   ')IOP='/   '
      IF(IW(ILOC).EQ.'**  ')IOP='**  '
      IF(IOP.EQ.'UNKN')GO TO 9000
!
!               *******************************************
!               **  STEP 3.2--                           **
!               **  CARRY OUT THE ARITHMETIC OPERATION.  **
!               *******************************************
!
      IF(IOP.EQ.'+   ')IRES=IVAL1+IVAL2
      IF(IOP.EQ.'-   ')IRES=IVAL1-IVAL2
      IF(IOP.EQ.'*   ')IRES=IVAL1*IVAL2
!CCCC IF(IOP.EQ.'/   ')IRES=IVAL1/IVAL2
      IF(IOP.EQ.'**  ')IRES=IVAL1**IVAL2
!
!               **********************************
!               **  STEP 3.3--                  **
!               **  CONVERT RESULT              **
!               **  FROM INTEGER TO HOLLARITH.  **
!               **********************************
!
      CALL DPCOIH(IRES,IHOUT,NOUT,IVALID,IBUGA3,ISUBRO,IERROR)
!
!               ************************************************************
!               **  STEP 3.4--                                            **
!               **  ELIMINATE THE LAST 2 WORDS OF THE                     **
!               **  ORIGINAL 3-WORD STRING,                               **
!               ************************************************************
!
      JMIN=ISTOP-1
      JMAX=ISTOP
      CALL DPSIES(JMIN,JMAX,IW,NW,IBUGA3,IERROR)
!
!               *****************************
!               **  STEP 3.5--             **
!               **  INSERT RESULT STRING   **
!               **  (OF LENGTH NOUT)       **
!               **  INTO IW                **
!               **  (STARTING AT           **
!               **  LOCATION ISTART).      **
!               *****************************
!
      CALL DPSIIS(ISTART,IW,NW,IHOUT,NOUT,IBUGA3,IERROR)
!
!               ************************************************************
!               **  STEP 3.6--                                            **
!               **  IF THE INSERTED STRING HAD LENGTH = 1,                **
!               **  (SO THAT THE CURRENT EXPRESSION INSIDE THE            **
!               **  PARENTHESES NOW HAS INTERNAL LENGTH OF 1)             **
!               **  ELIMINATE THE PARENTHESES.                            **
!               ************************************************************
!
      IF(NOUT.NE.1)GO TO 9000
!
      ISTAM1=ISTART-1
      ISTAP1=ISTART+1
      IF(ISTAM1.LT.1)GO TO 9000
      IF(ISTAP1.GT.NW)GO TO 9000
      IF(IW(ISTAM1).EQ.'(   '.AND.IW(ISTAP1).EQ.')   ')GO TO 235
      GO TO 9000
!
  235 CONTINUE
      JMIN=ISTART+1
      JMAX=ISTART+1
      CALL DPSIES(JMIN,JMAX,IW,NW,IBUGA3,IERROR)
      JMIN=ISTART-1
      JMAX=ISTART-1
      CALL DPSIES(JMIN,JMAX,IW,NW,IBUGA3,IERROR)
      GO TO 9000
!
!               **********************************
!               **  STEP 4--                    **
!               **  TREAT THE LENGTH = 4 CASE.  **
!               **********************************
!
  240 CONTINUE
!
!               ***************************************
!               **  STEP 4.1--                       **
!               **  CONVERT FIRST WORD OUT OF THE 4  **
!               **  FROM HOLLARITH TO INTEGER.       **
!               **  CONVERT LAST  WORD OUT OF THE 4  **
!               **  FROM HOLLARITH TO INTEGER.       **
!               **  DETERMINE TYPE OF OPERATION      **
!               **  BY EXAMINING THE SECOND AND      **
!               **  THIRD WORDS OUT OF THE 4.        **
!               ***************************************
!
      IMIN=ISTART
      IMAX=ISTART
      CALL DPCOHI(IMIN,IMAX,IW,NW,IVALID,VAL1,IVAL1,   &
      IBUGA3,IERROR)
      IF(IERROR.EQ.'YES'.OR.IVALID.EQ.'NO')GO TO 9000
!
      IMIN=ISTOP
      IMAX=ISTOP
      CALL DPCOHI(IMIN,IMAX,IW,NW,IVALID,VAL2,IVAL2,   &
      IBUGA3,IERROR)
      IF(IERROR.EQ.'YES'.OR.IVALID.EQ.'NO')GO TO 9000
!
      ILOC=ISTART+1
      ILOCP1=ILOC+1
      IF(IW(ILOC).EQ.'*   '.AND.IW(ILOCP1).EQ.'*   ')IOP='**  '
      IF(IOP.EQ.'UNKN')GO TO 9000
!
!               *******************************************
!               **  STEP 4.2--                           **
!               **  CARRY OUT THE ARITHMETIC OPERATION.  **
!               *******************************************
!
      IF(IOP.EQ.'**  ')IRES=IVAL1**IVAL2
!
!               **********************************
!               **  STEP 4.3--                  **
!               **  CONVERT RESULT              **
!               **  FROM INTEGER TO HOLLARITH.  **
!               **********************************
!
      CALL DPCOIH(IRES,IHOUT,NOUT,IVALID,IBUGA3,ISUBRO,IERROR)
!
!               ************************************************************
!               **  STEP 4.4--                                            **
!               **  ELIMINATE THE LAST 3 WORDS OF THE                     **
!               **  ORIGINAL 4-WORD STRING,                               **
!               ************************************************************
!
      JMIN=ISTOP-2
      JMAX=ISTOP
      CALL DPSIES(JMIN,JMAX,IW,NW,IBUGA3,IERROR)
!
!               *****************************
!               **  STEP 4.5--             **
!               **  INSERT RESULT STRING   **
!               **  (OF LENGTH NOUT)       **
!               **  INTO IW                **
!               **  (STARTING AT           **
!               **  LOCATION ISTART).      **
!               *****************************
!
      CALL DPSIIS(ISTART,IW,NW,IHOUT,NOUT,IBUGA3,IERROR)
!
!               ************************************************************
!               **  STEP 4.6--                                            **
!               **  IF THE INSERTED STRING HAD LENGTH = 1,                **
!               **  (SO THAT THE CURRENT EXPRESSION INSIDE THE            **
!               **  PARENTHESES NOW HAS INTERNAL LENGTH OF 1)             **
!               **  ELIMINATE THE PARENTHESES.                            **
!               ************************************************************
!
      IF(NOUT.NE.1)GO TO 9000
!
      ISTAM1=ISTART-1
      ISTAP1=ISTART+1
      IF(ISTAM1.LT.1)GO TO 9000
      IF(ISTAP1.GT.NW)GO TO 9000
      IF(IW(ISTAM1).EQ.'(   '.AND.IW(ISTAP1).EQ.')   ')GO TO 245
      GO TO 9000
!
  245 CONTINUE
      JMIN=ISTART+1
      JMAX=ISTART+1
      CALL DPSIES(JMIN,JMAX,IW,NW,IBUGA3,IERROR)
      JMIN=ISTART-1
      JMAX=ISTART-1
      CALL DPSIES(JMIN,JMAX,IW,NW,IBUGA3,IERROR)
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'SIS2')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSIS2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,ISUBRO,IERROR
 9012 FORMAT('IBUGA3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NW
 9013 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)(IW(I),I=1,MIN(NW,115))
 9014 FORMAT('(IW(.) = ',115A1)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPSIS2
      SUBROUTINE DPSKIP(IHARG,IARGT,IARG,NUMARG,IDEFSK,   &
                        ISKIP,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE NUMBER OF LINES TO BE SKIPPED
!              AT THE BEGINNING OF A READ COMMAND
!              OR A SERIAL READ COMMAND.
!              THIS ALLOWS TEXT AND HEADER LINES TO BE
!              SKIPPED OVER AT THE BEGINNING OF A DATA FILE.
!              THE SPECIFIED NUMBER OF SKIP LINES WILL BE PLACED
!              IN THE FLOATING POINT VARIABLE ISKIP.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --IARG   (AN INTEGER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFSK (AN INTEGER VARIABLE)
!     OUTPUT ARGUMENTS--ISKIP  (AN INTEGER VARIABLE)
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER 1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --SEPTEMBER 1993.  ALLOW SKIP ----
!     UPDATED         --OCTOBER   1997.  FIX SKIP ----
!     UPDATED         --OCTOBER   1997.  SET SKIP AUTOMATIC
!                                        EQUIVALENT TO SKIP ----
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
      DIMENSION IARG(*)
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
      IF(NUMARG.LE.0)GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
!CCCC OCTOBER 1997.  MAKE "SKIP AUTOMATIC" EQUIVALENT TO A "SKIP ----"
!CCCC IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1170
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
!CCCC THE FOLLOWING LINE WAS ADDED    SEPTEMBER 1993
      IF(IHARG(NUMARG).EQ.'----')GO TO 1170
!CCCC THE FOLLOWING LINE WAS FIXED, COMMAND PARSING RETURNS A SINGLE
!CCCC "-".     OCTOBER 1997
      IF(IHARG(NUMARG).EQ.'-')GO TO 1170
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPSKIP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR SKIP ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE THE THE ANALYST WISHES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      TO SKIP OVER 3 NON-DATA LINES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      AT THE BEGINNING OF READS AND SERIAL READS,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      SKIP 3 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1150 CONTINUE
      ISKIP=IDEFSK
      GO TO 1180
!
 1160 CONTINUE
      ISKIP=IARG(NUMARG)
      GO TO 1180
!
!CCCC THE FOLLOWING SECTION WAS ADDED    SEPTEMBER 1993
 1170 CONTINUE
      IFOUND='YES'
      ISKIP=-1
      IF(IFEEDB.EQ.'OFF')GO TO 1179
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1171)
 1171 FORMAT('HEADER LINES WILL BE SKIPPED UNITL A LINE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1172)
 1172 FORMAT('    WITH    ----    IS ENCOUNTERED')
      CALL DPWRST('XXX','BUG ')
 1179 CONTINUE
      GO TO 9000
!
 1180 CONTINUE
      IFOUND='YES'
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE NUMBER OF HEADER LINES TO BE SKIPPED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)ISKIP
 1182 FORMAT('    HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 9000
!
!               ********************************************
!               **  STEP 81--                             **
!               **  TREAT THE    ?    CASE--              **
!               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
!               ********************************************
!
 8100 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)ISKIP
 8111 FORMAT('THE CURRENT NUMBER OF LINES TO BE SKIPPED IS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)IDEFSK
 8112 FORMAT('THE DEFAULT NUMBER OF LINES TO BE SKIPPED IS ',I8)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPSKIP
      SUBROUTINE DPSKOU(MAXNXT,ICAPSW,ICASAN,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES AN OUTLIER TEST BASED ON THE
!              SAMPLE SKEWNESS.  IF THE SAMPLE SKEWNESS IS ABOVE THE
!              CRITICAL VALUE, ASSUME THE OBSERVATION FURTHERST FROM
!              THE MEAN IS AN OUTLIER.  THIS TEST CAN BE REPEATED BY
!              REMOVING THE OUTLIER AND REPEATING THE TEST UNTIL THE
!              TEST INDICATES NO OUTLIER.  THIS TEST ASSUMES NORMALITY.
!              THIS ROUTINE ONLY TESTS FOR A SINGLE OUTLIER.  FOR
!              MULTIPLE OUTLIERS, THE USER SHOULD DELETE THE CURRENT
!              OUTLIER AND REPEAT THE TEST.
!
!              THIS TEST WAS ADDED TO SUPPORT THE ASTM E-178 STANDARD
!              (2016 EDITION).
!
!              CRITICAL VALUES CAN BE DETERMINED IN THE FOLLOWING
!              WAYS:
!
!                1. TABLES FROM ASTM E178 - 16a
!                2. SIMULATION
!
!              ALTERNATIVELY, IT CAN PERFORM THE OUTLIER TEST BASED ON
!              KURTOSIS.
!
!     REFERENCE--E178 - 16A (2016), "Standard Practice for Dealing with
!                Outlying Observations", ASTM International, 100 Barr
!                Harbor Drive, PO BOX C700, West Conshohocken, PA
!                19428-2959, USA.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORAOTRY
!                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2019/10
!     ORIGINAL VERSION--OCTOBER   2019.
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
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASP2
      CHARACTER*4 ISKOT2
      CHARACTER*4 IDATSW
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICASE
      CHARACTER*8 ISTROUT
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
      INCLUDE 'DPCOZZ.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION XDESGN(MAXOBV,7)
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB8),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB9),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGAR10),XIDTE4(1))
      EQUIVALENCE (GARBAG(JGAR11),XIDTE5(1))
      EQUIVALENCE (GARBAG(JGAR12),XIDTE6(1))
      EQUIVALENCE (GARBAG(JGAR13),XDESGN(1,1))
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
      ICASAN='SKEW'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPSK'
      ISUBN2='OU  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      MINN2=3
!
!               ***************************************************
!               **  TREAT THE SKEWNESS OUTLIER       TEST CASE   **
!               **  TREAT THE KURTOSIS OUTLIER       TEST CASE   **
!               ***************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SKOU')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSKOU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO,ICASAN
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,ICASAN = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)MAXNXT
   55   FORMAT('MAXNXT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:            **
!               **    1) SKEWNESS OUTLIER TEST Y                       **
!               **       KURTOSIS OUTLIER TEST Y                       **
!               **    2) SKEWNESS OUTLIER TEST Y LABID                 **
!               **       KURTOSIS OUTLIER TEST Y LABID                 **
!               **    3) MULTIPLE SKEWNESS OUTLIER TEST Y1 ... YK      **
!               **       MULTIPLE KURTOSIS OUTLIER TEST Y1 ... YK      **
!               **    4) REPLICATED SKEWNESS OUTLIER TEST Y X1 ... XK  **
!               **       REPLICATED KURTOSIS OUTLIER TEST Y X1 ... XK  **
!               **    5) REPLICATED SKEWNESS OUTLIER TEST Y LABID      **
!               **                                        X1 ... XK    **
!               **       REPLICATED KURTOSIS OUTLIER TEST Y LABID      **
!               **                                        X1 ... XK    **
!               **       REPLICATED SKEWNESS OUTLIER  TEST Y X1 ... XK **
!               **                                         LABID       **
!               **       REPLICATED KURTOSIS OUTLIER  TEST Y X1 ... XK **
!               **                                         LABID       **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTC=9999
      ILASTZ=9999
      ISHIFT=0
      IFOUND='NO'
      ICASAN='SKEW'
!
      DO 100 I=0,NUMARG-1
!
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
          ICTMP2=IHARG(I+1)
          ICTMP3=IHARG(I+2)
        ELSE
          ICTMP1=IHARG(I)
          ICTMP2=IHARG(I+1)
          ICTMP3=IHARG(I+2)
        ENDIF
!
        IF(ICTMP1.EQ.'SKEW' .AND. ICTMP2.EQ.'OUTL' .AND.   &
           ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+2
          ICASAN='SKEW'
        ELSEIF(ICTMP1.EQ.'SKEW' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
          ICASAN='SKEW'
        ELSEIF(ICTMP1.EQ.'KURT' .AND. ICTMP2.EQ.'OUTL' .AND.   &
           ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+2
          ICASAN='KURT'
        ELSEIF(ICTMP1.EQ.'KURT' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
          ICASAN='KURT'
        ELSEIF(ICTMP1.EQ.'SKEW' .AND. ICTMP2.EQ.'OUTL')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
          ICASAN='SKEW'
        ELSEIF(ICTMP1.EQ.'KURT' .AND. ICTMP2.EQ.'OUTL')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
          ICASAN='KURT'
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
      ISTROUT='SKEWS'
      IF(ICASAN.EQ.'KURT')ISTROUT='KURTOSIS'
!
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)ISTROUT
  101     FORMAT('***** ERROR IN ',A8,' OUTLIER TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)ISTROUT
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION" FOR THE ',A8,' OUTLIER TEST COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='SKEWNESS OUTLIER TEST'
      IF(ICASAN.EQ.'KURT')INAME='KURTOSIS OUTLIER TEST'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IF(IMULT.EQ.'ON')IFLAGE=0
      IFLAGM=1
      IF(IREPL.EQ.'ON')IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=-99
      MAXNVA=-99
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NRESP=0
      NREPL=0
      NLABID=0
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSEIF(IREPL.EQ.'ON')THEN
        NRESP=1
        IF(NUMVAR.EQ.2)THEN
          NLABID=0
          NREPL=1
        ELSE
          NLABID=1
          NREPL=NUMVAR-NRESP-NLABID
        ENDIF
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
        NRESP=1
        NLABID=NUMVAR-NRESP
        IF(NLABID.GT.1)NLABID=1
      ENDIF
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')THEN
        WRITE(ICOUT,521)NRESP,NLABID,NREPL
  521   FORMAT('NRESP,NLABID,NREPL = ',3I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 6--                                        **
!               **  GENERATE THE DAVID  TEST FOR THE VARIOUS CASES  **
!               ******************************************************
!
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************************************
!               **  STEP 7A--                          **
!               **  CASE 1: SINGLE RESPONSE VARIABLE   **
!               **          WITH NO REPLICATION        **
!               *****************************************
!
      IF(NRESP.EQ.1 .AND. NREPL.EQ.0)THEN
        ISTEPN='7A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
!
        ICOL=1
        NUMVA2=1
        IF(NLABID.GE.1)NUMVA2=2
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,X1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        IF(NLABID.EQ.0)THEN
          DO 720 I=1,NLOCAL
            X1(I)=REAL(I)
  720     CONTINUE
        ENDIF
!
!       *****************************************************
!       **  STEP 7B--                                      **
!       **  CALL DPSKO2 TO PERFORM SKEWNESS OUTLIER  TEST. **
!       **  CALL DPKUO2 TO PERFORM KURTOSIS OUTLIER  TEST. **
!       *****************************************************
!
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SKOU')THEN
          ISTEPN='7B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,711)
  711     FORMAT('***** FROM THE MIDDLE  OF DPSKOU--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,712)ICASAN,NUMVAR,IDATSW,NLOCAL
  712     FORMAT('ICASAN,NUMVAR,IDATSW,NQ = ',   &
                 A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 715 I=1,NLOCAL
              WRITE(ICOUT,716)I,Y1(I),X1(I)
  716         FORMAT('I,Y1(I),X1(I) = ',I8,2F12.5)
              CALL DPWRST('XXX','BUG ')
  715       CONTINUE
          ENDIF
        ENDIF
!
        NCURVE=1
        IF(ICASAN.EQ.'SKEW')THEN
          CALL DPSKO2(Y1,X1,NLOCAL,MAXNXT,ISKOTA,   &
                      XTEMP1,XTEMP2,PID,IVARID,IVARI2,NREPL,NLABID,   &
                      ICAPSW,ICAPTY,IFORSW,   &
                      STATVA,STATCD,PVAL,   &
                      CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                      ISUBRO,IBUGA3,IERROR)
        ELSE
          CALL DPKUO2(Y1,X1,NLOCAL,MAXNXT,IKUOTA,   &
                      XTEMP1,XTEMP2,PID,IVARID,IVARI2,NREPL,NLABID,   &
                      ICAPSW,ICAPTY,IFORSW,   &
                      STATVA,STATCD,PVAL,   &
                      CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                      ISUBRO,IBUGA3,IERROR)
        ENDIF
!
!               ***************************************
!               **  STEP 7C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
        ISTEPN='7C'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IFLAGU='ON'
        IFRST=.FALSE.
        ILAST=.FALSE.
        IF(ICASAN.EQ.'SKEW')THEN
          ISKOT2=ISKOTA
          IF(ISKOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
        ELSE
          ISKOT2=IKUOTA
          IF(IKUOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
        ENDIF
        CALL DPDAV4(STATVA,STATCD,PVAL,   &
                    CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                    IFLAGU,IFRST,ILAST,ICASP2,ISKOT2,   &
                    IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!               ******************************************
!               **  STEP 8A--                           **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES **
!               **          NOTE THAT A LABID VARIABLE  **
!               **          IS NOT SUPPORTED FOR THIS   **
!               **          CASE.                       **
!               ******************************************
!
      ELSEIF(NRESP.GT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')THEN
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
                      Y1,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          DO 820 I=1,NLOCAL
            X1(I)=REAL(I)
  820     CONTINUE
!
!         *****************************************************
!         **  STEP 8B--                                      **
!         *****************************************************
!
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SKOU')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPSKOU--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,IDATSW,NLOCAL
  823       FORMAT('ICASAN,NUMVAR,IDATSW,NQ = ',   &
                   A4,I8,2X,A4,I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO 825 I=1,NLOCAL
                WRITE(ICOUT,826)I,Y1(I),X1(I)
  826           FORMAT('I,Y1(I),X1(I) = ',I8,2F12.5)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
!
          IF(ICASAN.EQ.'SKEW')THEN
            CALL DPSKO2(Y1,X1,NLOCAL,MAXNXT,ISKOTA,   &
                        XTEMP1,XTEMP2,PID,IVARID,IVARI2,NREPL,NLABID,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                        ISUBRO,IBUGA3,IERROR)
          ELSE
            CALL DPKUO2(Y1,X1,NLOCAL,MAXNXT,IKUOTA,   &
                        XTEMP1,XTEMP2,PID,IVARID,IVARI2,NREPL,NLABID,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                        ISUBRO,IBUGA3,IERROR)
          ENDIF
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IFLAGU='FILE'
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          IF(ICASAN.EQ.'SKEW')THEN
            ISKOT2=ISKOTA
            IF(ISKOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
          ELSE
            ISKOT2=IKUOTA
            IF(IKUOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
          ENDIF
          CALL DPDAV4(STATVA,STATCD,PVAL,   &
                      CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                      IFLAGU,IFRST,ILAST,ICASP2,ISKOT2,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
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
      ELSEIF(IREPL.EQ.'ON')THEN
        ISTEPN='9A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
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
!         LABID VARIABLE IN X1
!
          IF(NLABID.GE.1)THEN
            ICOLC=ICOLC+1
            ICOLT=ICOLR(ICOLC)
            IJ=MAXN*(ICOLT-1)+I
            IF(ICOLT.LE.MAXCOL)X1(J)=V(IJ)
            IF(ICOLT.EQ.MAXCP1)X1(J)=PRED(I)
            IF(ICOLT.EQ.MAXCP2)X1(J)=RES(I)
            IF(ICOLT.EQ.MAXCP3)X1(J)=YPLOT(I)
            IF(ICOLT.EQ.MAXCP4)X1(J)=XPLOT(I)
            IF(ICOLT.EQ.MAXCP5)X1(J)=X2PLOT(I)
            IF(ICOLT.EQ.MAXCP6)X1(J)=TAGPLO(I)
          ELSE
            X1(J)=REAL(I)
          ENDIF
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
        ISTEPN='9B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SKOU')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       NOTE: CHECK TO SEE IF X1 HAS ALL UNIQUE ELEMENTS.  IF NOT,
!             THEN INTERPRET THIS AS A REPLICATION VARIABLE.
!
        CALL DISTIN(X1,NLOCAL,IWRITE,XTEMP2,NDIST,IBUGA3,IERROR)
        IF(NLOCAL.NE.NDIST)THEN
          NLABID=0
          IF(NREPL.GT.6)NREPL=6
          IF(NREPL.GE.1)THEN
            DO 930 J=1,NREPL-1
              DO 935 I=1,NLOCAL
                XDESGN(I,J+1)=XDESGN(I,J)
  935         CONTINUE
  930       CONTINUE
          ENDIF
          NREPL=NREPL+1
          DO 938 I=1,NLOCAL
            XDESGN(I,1)=X1(I)
            X1(I)=REAL(I)
  938     CONTINUE
        ENDIF
!
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
        IF(NLABID.EQ.1)THEN
          PID(2)=CPUMIN
          IVARID(2)=IVARN1(2)
          IVARI2(2)=IVARN2(2)
        ENDIF
        IADD=NRESP+NLABID
        DO 940 II=1,NREPL
          IVARID(II+IADD)=IVARN1(II+IADD)
          IVARI2(II+IADD)=IVARN2(II+IADD)
  940   CONTINUE
!
!       *****************************************************
!       **  STEP 9B--                                      **
!       **  CALL DPSKO2 TO PERFORM SKEWNESS OUTLIER TEST.  **
!       **  CALL DPKUO2 TO PERFORM KURTOSIS OUTLIER TEST.  **
!       *****************************************************
!
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SKOU')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPSKOU--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,942)ICASAN,NUMVAR,IDATSW,NLOCAL,NREPL
  942     FORMAT('ICASAN,NUMVAR,IDATSW,NLOCAL,NREPL = ',   &
                 A4,I8,2X,A4,2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 945 I=1,NLOCAL
              WRITE(ICOUT,946)I,Y1(I),X1(I),XDESGN(I,1),XDESGN(I,2)
  946         FORMAT('I,Y1(I),X1(I),XDESGN(I,1),XDESGN(I,2) = ',   &
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
        IF(NREPL.EQ.1)THEN
          J=0
          DO 1110 ISET1=1,NUMSE1
            K=0
            PID(IADD+1)=XIDTEM(ISET1)
            DO 1130 I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'SKEW')THEN
                CALL DPSKO2(TEMP1,TEMP2,NTEMP,MAXNXT,ISKOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPKUO2(TEMP1,TEMP2,NTEMP,MAXNXT,IKUOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NUMSE1)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            IF(ICASAN.EQ.'SKEW')THEN
              ISKOT2=ISKOTA
              IF(ISKOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ELSE
              ISKOT2=IKUOTA
              IF(IKUOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ENDIF
            CALL DPDAV4(STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                        IFLAGU,IFRST,ILAST,ICASP2,ISKOT2,   &
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'SKEW')THEN
                CALL DPSKO2(TEMP1,TEMP2,NTEMP,MAXOBV,ISKOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPKUO2(TEMP1,TEMP2,NTEMP,MAXOBV,IKUOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            IF(ICASAN.EQ.'SKEW')THEN
              ISKOT2=ISKOTA
              IF(ISKOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ELSE
              ISKOT2=IKUOTA
              IF(IKUOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ENDIF
            CALL DPDAV4(STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                        IFLAGU,IFRST,ILAST,ICASP2,ISKOT2,   &
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'SKEW')THEN
                CALL DPSKO2(TEMP1,TEMP2,NTEMP,MAXOBV,ISKOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPKUO2(TEMP1,TEMP2,NTEMP,MAXOBV,IKUOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            IF(ICASAN.EQ.'SKEW')THEN
              ISKOT2=ISKOTA
              IF(ISKOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ELSE
              ISKOT2=IKUOTA
              IF(IKUOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ENDIF
            CALL DPDAV4(STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                        IFLAGU,IFRST,ILAST,ICASP2,ISKOT2,   &
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'SKEW')THEN
                CALL DPSKO2(TEMP1,TEMP2,NTEMP,MAXOBV,ISKOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPKUO2(TEMP1,TEMP2,NTEMP,MAXOBV,IKUOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            IF(ICASAN.EQ.'SKEW')THEN
              ISKOT2=ISKOTA
              IF(ISKOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ELSE
              ISKOT2=IKUOTA
              IF(IKUOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ENDIF
            CALL DPDAV4(STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                        IFLAGU,IFRST,ILAST,ICASP2,ISKOT2,   &
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'SKEW')THEN
                CALL DPSKO2(TEMP1,TEMP2,NTEMP,MAXOBV,ISKOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPKUO2(TEMP1,TEMP2,NTEMP,MAXOBV,IKUOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            IF(ICASAN.EQ.'SKEW')THEN
              ISKOT2=ISKOTA
              IF(ISKOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ELSE
              ISKOT2=IKUOTA
              IF(IKUOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ENDIF
            CALL DPDAV4(STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                        IFLAGU,IFRST,ILAST,ICASP2,ISKOT2,   &
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'SKEW')THEN
                CALL DPSKO2(TEMP1,TEMP2,NTEMP,MAXOBV,ISKOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPKUO2(TEMP1,TEMP2,NTEMP,MAXOBV,IKUOTA,   &
                            XTEMP1,XTEMP2,PID,IVARID,IVARI2,   &
                            NREPL,NLABID,ICAPSW,ICAPTY,IFORSW,   &
                            STATVA,STATCD,PVAL,   &
                            CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                            ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            IF(ICASAN.EQ.'SKEW')THEN
              ISKOT2=ISKOTA
              IF(ISKOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ELSE
              ISKOT2=IKUOTA
              IF(IKUOTA.EQ.'ASTM' .AND. NLOCAL.GT.50)ISKOT2='SIMU'
            ENDIF
            CALL DPDAV4(STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,CUT975,CUT99,CUT995,   &
                        IFLAGU,IFRST,ILAST,ICASP2,ISKOT2,   &
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SKOU')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSKOU--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTP,NS,ICASAN
 9013   FORMAT('NPLOTP,NS,ICASAN = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSKOU
      SUBROUTINE DPSKO2(Y,X,N,MAXNXT,ISKOTA,   &
                        TEMP1,TEMP2,PID,IVARID,IVARI2,NREPL,NLABID,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        STATVA,STATCD,PVAL,   &
                        CUT80,CUT90,CUT95,   &
                        CUT975,CUT99,CUT995,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES AN OUTLIER TEST BASED ON THE
!              SAMPLE SKEWNESS.  IF THE SAMPLE SKEWNESS IS ABOVE THE
!              CRITICAL VALUE, ASSUME THE OBSERVATION FURTHERST FROM
!              THE MEAN IS AN OUTLIER.  THIS TEST CAN BE REPEATED BY
!              REMOVING THE OUTLIER AND REPEATING THE TEST UNTIL THE
!              TEST INDICATES NO OUTLIER.  THIS TEST ASSUMES NORMALITY.
!              THIS ROUTINE ONLY TESTS FOR A SINGLE OUTLIER.  FOR
!              MULTIPLE OUTLIERS, THE USER SHOULD DELETE THE CURRENT
!              OUTLIER AND REPEAT THE TEST.
!
!              THIS TEST WAS ADDED TO SUPPORT THE ASTM E-178 STANDARD
!              (2016 EDITION).
!
!              CRITICAL VALUES CAN BE DETERMINED IN THE FOLLOWING
!              WAYS:
!
!                1. TABLES FROM ASTM E178 - 16a
!                2. SIMULATION
!
!     REFERENCE--E178 - 16A (2016), "Standard Practice for Dealing with
!                Outlying Observations", ASTM International, 100 Barr
!                Harbor Drive, PO BOX C700, West Conshohocken, PA
!                19428-2959, USA.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2019/10
!     ORIGINAL VERSION--OCTOBER   2019.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISKOTA
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISKOT2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      REAL ALPHA(6)
      REAL CV(6)
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=30)
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
      LOGICAL IFLAGA
      LOGICAL IFLAGB
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION PID(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSK'
      ISUBN2='O2  '
      IERROR='NO'
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
      CUT80=CPUMIN
      CUT90=CPUMIN
      CUT95=CPUMIN
      CUT975=CPUMIN
      CUT99=CPUMIN
      CUT995=CPUMIN
!
      ISKOT2=ISKOTA
      IF(ISKOTA.EQ.'ASTM' .AND. N.GT.50)ISKOT2='SIMU'
      IF(ISKOT2.EQ.'ASTM')THEN
        NALPHA=3
        ALPHA(1)=90.
        ALPHA(2)=95.
        ALPHA(3)=99.
      ELSE
        NALPHA=6
        ALPHA(1)=80.
        ALPHA(2)=90.
        ALPHA(3)=95.
        ALPHA(4)=97.5
        ALPHA(5)=99.
        ALPHA(6)=99.5
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSKO2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)ISUBRO,IBUGA3,ISKOTA,N,MAXNXT
   52   FORMAT('ISUBRO,IBUGA3,ISKOTA,N,MAXNXT = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),X(I)
   57     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN SKEWNESS OUTLIER TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 3.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1114)N
 1114   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 1135 I=2,N
        IF(Y(I).NE.HOLD)GO TO 1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)HOLD
 1131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1139 CONTINUE
!
!               ***********************************
!               **  STEP 21--                    **
!               **  CARRY OUT CALCULATIONS       **
!               **  FOR  SKEWNESS OUTLIER  TEST  **
!               ***********************************
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SKO2')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
      CALL DPSKO3(Y,N,TEMP1,TEMP2,IWRITE,PSTAMV,   &
                  MAXNXT,ISKOT2,ISEED,   &
                  ALPHA,CV,NALPHA,   &
                  STATVA,YMEAN,YSD,YMIN,YMAX,YSKEW,   &
                  PVAL,STATCD,YIND,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(ISKOT2.EQ.'ASTM')THEN
        CUT90=CV(1)
        CUT95=CV(2)
        CUT99=CV(3)
        NCDF=3
      ELSE
        CUT80=CV(1)
        CUT90=CV(2)
        CUT95=CV(3)
        CUT975=CV(4)
        CUT99=CV(5)
        CUT995=CV(6)
        NCDF=6
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')THEN
        WRITE(ICOUT,2111)YMEAN,YSD,YSKEW
 2111   FORMAT('YMEAN,YSD,YSKEW=',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2113)STATVA,PVAL,CDF,YIND
 2113   FORMAT('STATVA,PVAL,CDF,YIND =',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!
!               *********************************
!               **   STEP 42--                 **
!               **   WRITE OUT EVERYTHING      **
!               **   FOR SKEWNESS OUTLIER TEST **
!               *********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')   &
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
      ITITLE(1:26)='Skewness Test for Outliers'
      NCTITL=26
      ITITLZ='(Assumption: Normality)'
      NCTITZ=23
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
        IADD=NLABID+NRESP
        DO 4101 I=1,NREPL
          ICNT=ICNT+1
          ITEMP=I+IADD
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
      ITEXT(ICNT)='H0: The most extreme point is not'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    an outlier'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      AVALUE(ICNT)=0.0
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The most extreme point is not'
      NCTEXT(ICNT)=33
      IDIGIT(ICNT)=-1
      AVALUE(ICNT)=0.0
      ICNT=ICNT+1
      ITEXT(ICNT)='    an outlier'
      NCTEXT(ICNT)=14
      IDIGIT(ICNT)=-1
      AVALUE(ICNT)=0.0
!
      IINDX=INT(YIND+0.1)
      YEXT=Y(IINDX)
      ICNT=ICNT+1
      ITEXT(ICNT)='Potential outlier value tested:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=YEXT
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='ID for potential outlier:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=X(INT(YIND))
      IDIGIT(ICNT)=0
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
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample SD:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=YSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Adjusted Skewness:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=YSKEW
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Skewness Outlier Test Statistic Value:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
!
!CCCC NOTE: CDF AND P-VALUE ONLY PRINTED IF CRITICAL
!CCCC       VALUES DETERMINED FROM SIMUMLATION
!
      IF(ISKOT2.EQ.'SIMU')THEN
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
      ENDIF
!
      NUMROW=ICNT
      DO 4210 I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      NUMLIN=1
      NUMROW=NCDF
      NUMCOL=5
      ITITL2(1,1)='Alpha'
      ITITL2(1,2)='CDF'
      ITITL2(1,3)='Statistic'
      ITITL2(1,4)='Critical Value'
      ITITL2(1,5)='Conclusion'
      NCTIT2(1,1)=5
      NCTIT2(1,2)=3
      NCTIT2(1,3)=9
      NCTIT2(1,4)=14
      NCTIT2(1,5)=10
!
      NMAX=0
      DO 4321 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
        IF(I.EQ.4)NTOT(I)=17
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=3
        ITYPCO(I)='ALPH'
 4321 CONTINUE
      ITYPCO(3)='NUME'
      ITYPCO(4)='NUME'
      IDIGIT(1)=0
      IDIGIT(2)=0
      DO 4323 I=1,NUMROW
        DO 4325 J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
 4325   CONTINUE
 4323 CONTINUE
      IF(ISKOT2.EQ.'ASTM')THEN
        IVALUE(1,1)='10%'
        IVALUE(2,1)='5%'
        IVALUE(3,1)='1%'
        IVALUE(1,2)='90%'
        IVALUE(2,2)='95%'
        IVALUE(3,2)='99%'
        NCVALU(1,1)=3
        NCVALU(2,1)=2
        NCVALU(3,1)=2
        NCVALU(1,2)=3
        NCVALU(2,2)=3
        NCVALU(3,2)=3
      ELSE
        IVALUE(1,1)='20%'
        IVALUE(2,1)='10%'
        IVALUE(3,1)='5%'
        IVALUE(4,1)='2.5%'
        IVALUE(5,1)='1%'
        IVALUE(6,1)='0.5%'
        IVALUE(1,2)='80%'
        IVALUE(2,2)='90%'
        IVALUE(3,2)='95%'
        IVALUE(4,2)='97.5%'
        IVALUE(5,2)='99%'
        IVALUE(6,2)='99.5%'
        NCVALU(1,1)=3
        NCVALU(2,1)=3
        NCVALU(3,1)=2
        NCVALU(4,1)=4
        NCVALU(5,1)=2
        NCVALU(6,1)=4
        NCVALU(1,2)=3
        NCVALU(2,2)=3
        NCVALU(3,2)=3
        NCVALU(4,2)=5
        NCVALU(5,2)=3
        NCVALU(6,2)=5
      ENDIF
      IVALUE(1,5)='Accept H0'
      IVALUE(2,5)='Accept H0'
      IVALUE(3,5)='Accept H0'
      IVALUE(4,5)='Accept H0'
      IVALUE(5,5)='Accept H0'
      IVALUE(6,5)='Accept H0'
      NCVALU(1,5)=9
      NCVALU(2,5)=9
      NCVALU(3,5)=9
      NCVALU(4,5)=9
      NCVALU(5,5)=9
      NCVALU(6,5)=9
      DO 4410 KK=1,NCDF
        AMAT(KK,3)=STATVA
        IF(STATVA.GT.CV(KK))IVALUE(KK,5)='Reject H0'
!CCCC   AMAT(KK,4)=RND(CV(KK),IDIGIT(3))
        AMAT(KK,4)=CV(KK)
 4410 CONTINUE
!
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWRTF(1)=1500
      IWRTF(2)=IWRTF(1)+1500
      IWRTF(3)=IWRTF(2)+2000
      IWRTF(4)=IWRTF(3)+2000
      IWRTF(5)=IWRTF(4)+2000
      IFRST=.FALSE.
      ILAST=.TRUE.
!
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')   &
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
      IF(ISKOT2.EQ.'SIMU')THEN
        ITITLE='Critical Values Based on 50,000 Simulations'
        NCTEMP=43
      ELSEIF(ISKOT2.EQ.'ASTM')THEN
        ITITLE='Critical Values Based on ASTM E-178 Tables'
        NCTEMP=42
      ENDIF
      IRTFMD='OFF'
      IFNTSZ=-1
      IFLAGA=.TRUE.
      IFLAGB=.TRUE.
      ISIZE=-1
      NTOTAL=NCTEMP
      NBLNK1=2
      NBLNK2=1
      ITYPE=2
      AVAL=CPUMIN
      CALL DPDTXT(ITITLE,NCTEMP,AVAL,NUMDIG,   &
                  NTOTAL,NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,   &
                  ISUBRO,IBUGA3,IERROR)
      ISIZE=-99
      IFNTSZ=0
!
      ISTEPN='42F'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SKO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSKO2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)IERROR,STATVA,STATCD,PVAL
 9013   FORMAT('IERROR,STATVA,STATCD,PVAL = ',A4,2X,3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSKO2
      SUBROUTINE DPSKO3(X,N,TEMP1,TEMP2,IWRITE,PSTAMV,   &
                        MAXNXT,ISKOTA,ISEED,   &
                        ALPHA,CV,NALPHA,   &
                        STATVA,XMEAN,XSD,XMIN,XMAX,XSKEW,   &
                        PVAL,STATCD,XIND,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES AN OUTLIER TEST BASED ON THE
!              SAMPLE SKEWNESS.  IF THE SAMPLE SKEWNESS IS ABOVE THE
!              CRITICAL VALUE, ASSUME THE OBSERVATION FURTHERST FROM
!              THE MEAN IS AN OUTLIER.  THIS TEST CAN BE REPEATED BY
!              REMOVING THE OUTLIER AND REPEATING THE TEST UNTIL THE
!              TEST INDICATES NO OUTLIER.  THIS TEST ASSUMES NORMALITY.
!              THIS ROUTINE ONLY TESTS FOR A SINGLE OUTLIER.  FOR
!              MULTIPLE OUTLIERS, THE USER SHOULD DELETE THE CURRENT
!              OUTLIER AND REPEAT THE TEST.
!
!              THIS TEST WAS ADDED TO SUPPORT THE ASTM E-178 STANDARD
!              (2016 EDITION).
!
!              CRITICAL VALUES CAN BE DETERMINED IN THE FOLLOWING
!              WAYS:
!
!                1. TABLES FROM ASTM E178 - 16a
!                2. SIMULATION
!
!     REFERENCE--E178 - 16A (2016), "Standard Practice for Dealing with
!                Outlying Observations", ASTM International, 100 Barr
!                Harbor Drive, PO BOX C700, West Conshohocken, PA
!                19428-2959, USA.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED SKEWNESS OUTLIER STATISTIC.
!                     --CDF    = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED CDF OF THE TEST STATISTIC.
!                     --PVAL   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED P-VALUE OF THE TEST STATISTIC.
!                     --XIND   = THE SINGLE PRECISION VALUE OF THE
!                                COMPUTED INDEX OF THE POTENTIAL
!                                OUTLIER.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE STATISTIC.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT, MEAN, SD, SKEW.
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
!     VERSION NUMBER--2019.10
!     ORIGINAL VERSION--OCTOBER   2019.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISKOTA
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISKOT2
      CHARACTER*4 IOP
      CHARACTER*4 IDIR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION ALPHA(*)
      DIMENSION CV(*)
!
      REAL LININ3
      EXTERNAL LININ3
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DFACT
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DENOM
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSK'
      ISUBN2='O3  '
      IERROR='NO'
      ISKOT2=ISKOTA
      IF(ISKOTA.EQ.'ASTM')THEN
        IF(N.GT.50)ISKOT2='SIMU'
        IF(NALPHA.EQ.1)THEN
          ALPT=ALPHA(1)
          IF(ALPT.GT.1.0 .AND. ALPT.LT.100.0)ALPT=ALPT/100.0
          IF(ALPT.LT.0.5)ALPT=1.0 - ALPT
          IF(ALPT.NE.0.90 .AND. ALPT.NE.0.95 .AND.   &
             ALPT.NE.0.99)ISKOT2='SIMU'
        ENDIF
      ENDIF
!
      STATVA=CPUMIN
      XSD=CPUMIN
      XMEAN=CPUMIN
      XMIN=CPUMIN
      XMAX=CPUMIN
      XIND=CPUMIN
      XSKEW=CPUMIN
      CDF=CPUMIN
      PVAL=CPUMIN
      XIND=0.0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SKO3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSKO3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ISKOTA,ISKOT2,ISEED,N
   52   FORMAT('IBUGA3,ISUBRO,ISKOTA,ISKOT2,ISEED,N = ',4(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NALPHA,ALPHA(1)
   54   FORMAT('NALPHA,ALPHA(1) = ',I5,F10.5)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.3)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN SKEW OUTLIER TEST STATISTIC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE MUST BE AT LEAST 3.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ******************************************
!               **  STEP 2--                            **
!               **  COMPUTE THE SKEW OUTLIER STATISTIC. **
!               ******************************************
!
      IWRITE='OFF'
      CALL SORT(X,N,TEMP1)
      XMIN=TEMP1(1)
      XMAX=TEMP1(N)
      CALL MEAN(X,N,IWRITE,XMEAN,IBUGA3,IERROR)
      CALL SD(X,N,IWRITE,XSD,IBUGA3,IERROR)
!CCCC CALL STMOM3(X,N,IWRITE,XSKEW,IBUGA3,IERROR)
!
!     USE DEFINITION OF SKEWNESS GIVEN IN ASTM E-178
!     (THIS IS EQUIVALENT TO THE "ADJUSTED PEARSON-FISHER"
!     DEFINITION USED IN STMOM3):
!
!          N*SUM[X(i) - XMEAN)**3/{(N-1)*(N-2)*S**3}
!
      DN=DBLE(N)
      DFACT=DN/((DN-1.0D0)*(DN-2.0D0))
      DENOM=DBLE(XSD)**3
      DSUM1=0.0D0
      DO 210 II=1,N
        DSUM1=DSUM1 + (DBLE(X(II)) - DBLE(XMEAN))**3
  210 CONTINUE
      XSKEW=REAL(DFACT*DSUM1/DENOM)
      STATVA=XSKEW
!
      CALL MAXIND(X,N,IWRITE,PSTAMV,XINDMX,ISUBRO,IBUGA3,IERROR)
      CALL MININD(X,N,IWRITE,PSTAMV,XINDMN,ISUBRO,IBUGA3,IERROR)
      D1=XMEAN - XMIN
      D2=XMAX - XMEAN
      IF(D1.GT.D2)THEN
        XIND=XINDMN
        ASIGN=-1.0
      ELSE
        XIND=XINDMX
        ASIGN=+1.0
      ENDIF
!
!               *****************************************
!               **  STEP 3--                           **
!               **  COMPUTE THE CRITICAL VALUES        **
!               *****************************************
!
      IF(ISKOT2.EQ.'SIMU')THEN
        IOP='OPEN'
        IFLAG1=0
        IFLAG2=1
        IFLAG3=0
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        NSIM=50000
        DO 3200 II=1,NSIM
          CALL NORRAN(N,ISEED,TEMP2)
          CALL MEAN(TEMP2,N,IWRITE,XMEAN2,IBUGA3,IERROR)
          CALL SD(TEMP2,N,IWRITE,XSD2,IBUGA3,IERROR)
          DENOM=DBLE(XSD2)**3
          DSUM1=0.0D0
          DO 3201 JJ=1,N
            DSUM1=DSUM1 + (DBLE(TEMP2(JJ)) - DBLE(XMEAN2))**3
 3201     CONTINUE
          TEMP1(II)=REAL(DFACT*DSUM1/DENOM)
          WRITE(IOUNI2,'(E15.7)')TEMP1(II)
 3200   CONTINUE
!
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        CALL SORT(TEMP1,NSIM,TEMP1)
!
        DO 3210 II=1,NALPHA
          ALPT=ALPHA(II)
          IF(ALPT.GT.1.0 .AND. ALPT.LT.100.0)ALPT=ALPT/100.0
          IF(ALPT.LE.0.0 .OR. ALPT.GT.1.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3111)ALPHA(II)
 3111       FORMAT('      INVALID VALUE OF ALPHA (',G15.7,'),')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
          IF(ALPT.LT.0.5)ALPT=1.0 - ALPT
          IF(ASIGN.EQ.1.0)THEN
            P100=100.0*ALPT
          ELSE
            P100=100.0*(1.0 - ALPT)
          ENDIF
          CALL PERCEN(P100,TEMP1,NSIM,IWRITE,TEMP2,MAXNXT,   &
                      XPERC,IBUGA3,IERROR)
          CV(II)=XPERC
 3210   CONTINUE
        IDIR='UPPE'
        IF(ASIGN.LT.0.0)IDIR='LOWE'
        CALL DPGOF8(TEMP1,NSIM,STATVA,PVAL,IDIR,IBUGA3,ISUBRO,IERROR)
        STATCD=1.0 - PVAL
      ELSEIF(ISKOT2.EQ.'ASTM')THEN
        DO 3300 II=1,NALPHA
          ALPT=ALPHA(II)
          IF(ALPT.GT.1.0 .AND. ALPT.LT.100.0)ALPT=ALPT/100.0
          IF(ALPT.LE.0.0 .OR. ALPT.GT.1.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3111)ALPHA(II)
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
          IF(ALPT.LT.0.5)ALPT=1.0 - ALPT
          IF(ALPT.EQ.0.90)THEN
            IF(N.EQ.3)THEN
              CV(II)=1.647
            ELSEIF(N.EQ.4)THEN
              CV(II)=1.439
            ELSEIF(N.EQ.5)THEN
              CV(II)=1.224
            ELSEIF(N.EQ.6)THEN
              CV(II)=1.090
            ELSEIF(N.EQ.7)THEN
              CV(II)=1.014
            ELSEIF(N.EQ.8)THEN
              CV(II)=0.956
            ELSEIF(N.EQ.9)THEN
              CV(II)=0.903
            ELSEIF(N.EQ.10)THEN
              CV(II)=0.862
            ELSEIF(N.EQ.11)THEN
              CV(II)=0.828
            ELSEIF(N.EQ.12)THEN
              CV(II)=0.798
            ELSEIF(N.EQ.13)THEN
              CV(II)=0.770
            ELSEIF(N.EQ.14)THEN
              CV(II)=0.744
            ELSEIF(N.EQ.15)THEN
              CV(II)=0.722
            ELSEIF(N.EQ.16)THEN
              CV(II)=0.702
            ELSEIF(N.EQ.17)THEN
              CV(II)=0.684
            ELSEIF(N.EQ.18)THEN
              CV(II)=0.667
            ELSEIF(N.EQ.19)THEN
              CV(II)=0.651
            ELSEIF(N.EQ.20)THEN
              CV(II)=0.636
            ELSEIF(N.EQ.21)THEN
              CV(II)=0.624
            ELSEIF(N.EQ.22)THEN
              CV(II)=0.610
            ELSEIF(N.EQ.23)THEN
              CV(II)=0.599
            ELSEIF(N.EQ.24)THEN
              CV(II)=0.587
            ELSEIF(N.EQ.25)THEN
              CV(II)=0.578
            ELSEIF(N.EQ.26)THEN
              CV(II)=0.567
            ELSEIF(N.EQ.27)THEN
              CV(II)=0.558
            ELSEIF(N.EQ.28)THEN
              CV(II)=0.549
            ELSEIF(N.EQ.29)THEN
              CV(II)=0.541
            ELSEIF(N.EQ.30)THEN
              CV(II)=0.532
            ELSEIF(N.GE.31 .AND. N.LE.34)THEN
              X1=30.
              X2=35.
              X3=REAL(N)
              Y1=0.532
              Y2=0.497
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.35)THEN
              CV(II)=0.497
            ELSEIF(N.GE.36 .AND. N.LE.39)THEN
              X1=35.
              X2=40.
              X3=REAL(N)
              Y1=0.497
              Y2=0.467
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.40)THEN
              CV(II)=0.467
            ELSEIF(N.GE.41 .AND. N.LE.44)THEN
              X1=40.
              X2=45.
              X3=REAL(N)
              Y1=0.467
              Y2=0.442
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.45)THEN
              CV(II)=0.442
            ELSEIF(N.GE.46 .AND. N.LE.49)THEN
              X1=45.
              X2=50.
              X3=REAL(N)
              Y1=0.442
              Y2=0.422
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.50)THEN
              CV(II)=0.422
            ENDIF
          ELSEIF(ALPT.EQ.0.95)THEN
            IF(N.EQ.3)THEN
              CV(II)=1.711
            ELSEIF(N.EQ.4)THEN
              CV(II)=1.709
            ELSEIF(N.EQ.5)THEN
              CV(II)=1.564
            ELSEIF(N.EQ.6)THEN
              CV(II)=1.428
            ELSEIF(N.EQ.7)THEN
              CV(II)=1.320
            ELSEIF(N.EQ.8)THEN
              CV(II)=1.246
            ELSEIF(N.EQ.9)THEN
              CV(II)=1.183
            ELSEIF(N.EQ.10)THEN
              CV(II)=1.131
            ELSEIF(N.EQ.11)THEN
              CV(II)=1.086
            ELSEIF(N.EQ.12)THEN
              CV(II)=1.049
            ELSEIF(N.EQ.13)THEN
              CV(II)=1.011
            ELSEIF(N.EQ.14)THEN
              CV(II)=0.977
            ELSEIF(N.EQ.15)THEN
              CV(II)=0.950
            ELSEIF(N.EQ.16)THEN
              CV(II)=0.922
            ELSEIF(N.EQ.17)THEN
              CV(II)=0.899
            ELSEIF(N.EQ.18)THEN
              CV(II)=0.875
            ELSEIF(N.EQ.19)THEN
              CV(II)=0.856
            ELSEIF(N.EQ.20)THEN
              CV(II)=0.836
            ELSEIF(N.EQ.21)THEN
              CV(II)=0.818
            ELSEIF(N.EQ.22)THEN
              CV(II)=0.800
            ELSEIF(N.EQ.23)THEN
              CV(II)=0.786
            ELSEIF(N.EQ.24)THEN
              CV(II)=0.770
            ELSEIF(N.EQ.25)THEN
              CV(II)=0.757
            ELSEIF(N.EQ.26)THEN
              CV(II)=0.743
            ELSEIF(N.EQ.27)THEN
              CV(II)=0.731
            ELSEIF(N.EQ.28)THEN
              CV(II)=0.718
            ELSEIF(N.EQ.29)THEN
              CV(II)=0.708
            ELSEIF(N.EQ.30)THEN
              CV(II)=0.695
            ELSEIF(N.GE.31 .AND. N.LE.34)THEN
              X1=30.
              X2=35.
              X3=REAL(N)
              Y1=0.695
              Y2=0.649
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.35)THEN
              CV(II)=0.649
            ELSEIF(N.GE.36 .AND. N.LE.39)THEN
              X1=35.
              X2=40.
              X3=REAL(N)
              Y1=0.649
              Y2=0.610
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.40)THEN
              CV(II)=0.610
            ELSEIF(N.GE.41 .AND. N.LE.44)THEN
              X1=40.
              X2=45.
              X3=REAL(N)
              Y1=0.610
              Y2=0.578
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.45)THEN
              CV(II)=0.578
            ELSEIF(N.GE.46 .AND. N.LE.49)THEN
              X1=45.
              X2=50.
              X3=REAL(N)
              Y1=0.578
              Y2=0.551
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.50)THEN
              CV(II)=0.551
            ENDIF
          ELSEIF(ALPT.EQ.0.99)THEN
            IF(N.EQ.3)THEN
              CV(II)=1.731
            ELSEIF(N.EQ.4)THEN
              CV(II)=1.940
            ELSEIF(N.EQ.5)THEN
              CV(II)=1.994
            ELSEIF(N.EQ.6)THEN
              CV(II)=1.959
            ELSEIF(N.EQ.7)THEN
              CV(II)=1.886
            ELSEIF(N.EQ.8)THEN
              CV(II)=1.813
            ELSEIF(N.EQ.9)THEN
              CV(II)=1.735
            ELSEIF(N.EQ.10)THEN
              CV(II)=1.668
            ELSEIF(N.EQ.11)THEN
              CV(II)=1.610
            ELSEIF(N.EQ.12)THEN
              CV(II)=1.556
            ELSEIF(N.EQ.13)THEN
              CV(II)=1.504
            ELSEIF(N.EQ.14)THEN
              CV(II)=1.461
            ELSEIF(N.EQ.15)THEN
              CV(II)=1.418
            ELSEIF(N.EQ.16)THEN
              CV(II)=1.379
            ELSEIF(N.EQ.17)THEN
              CV(II)=1.345
            ELSEIF(N.EQ.18)THEN
              CV(II)=1.310
            ELSEIF(N.EQ.19)THEN
              CV(II)=1.281
            ELSEIF(N.EQ.20)THEN
              CV(II)=1.252
            ELSEIF(N.EQ.21)THEN
              CV(II)=1.225
            ELSEIF(N.EQ.22)THEN
              CV(II)=1.196
            ELSEIF(N.EQ.23)THEN
              CV(II)=1.175
            ELSEIF(N.EQ.24)THEN
              CV(II)=1.150
            ELSEIF(N.EQ.25)THEN
              CV(II)=1.132
            ELSEIF(N.EQ.26)THEN
              CV(II)=1.108
            ELSEIF(N.EQ.27)THEN
              CV(II)=1.091
            ELSEIF(N.EQ.28)THEN
              CV(II)=1.070
            ELSEIF(N.EQ.29)THEN
              CV(II)=1.056
            ELSEIF(N.EQ.30)THEN
              CV(II)=1.036
            ELSEIF(N.GE.31 .AND. N.LE.34)THEN
              X1=30.
              X2=35.
              X3=REAL(N)
              Y1=1.036
              Y2=0.965
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.35)THEN
              CV(II)=0.965
            ELSEIF(N.GE.36 .AND. N.LE.39)THEN
              X1=35.
              X2=40.
              X3=REAL(N)
              Y1=0.965
              Y2=0.904
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.40)THEN
              CV(II)=0.904
            ELSEIF(N.GE.41 .AND. N.LE.44)THEN
              X1=40.
              X2=45.
              X3=REAL(N)
              Y1=0.904
              Y2=0.853
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.45)THEN
              CV(II)=0.853
            ELSEIF(N.GE.46 .AND. N.LE.49)THEN
              X1=45.
              X2=50.
              X3=REAL(N)
              Y1=0.853
              Y2=0.812
              AVAL=LININ3(X1,Y1,X2,Y2,X3,IBUGA3,ISUBRO,IERROR)
              CV(II)=AVAL
            ELSEIF(N.EQ.50)THEN
              CV(II)=0.812
            ENDIF
          ELSE
            CV(II)=CPUMIN
          ENDIF
          IF(CV(II).NE.CPUMIN)CV(II)=ASIGN*CV(II)
 3300   CONTINUE
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
  811   FORMAT('THE VALUE OF THE SKEWNESS OUTLIER STATISTIC OF THE ',I8,   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SKO3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSKO3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N
 9013   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)XMIN,XMAX,XMEAN,XSD,XSKEW
 9015   FORMAT('XMIN,XMAX,XMEAN,XSD,XSKEW = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)STATVA,CDF,PVAL,XIND
 9016   FORMAT('STATVA,CDF,PVAL,XIND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSKO3
      SUBROUTINE DPSLI2(YA,NROW,   &
                        WORK1,IWORK,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--PERFORM A SINGLE LINKAGE (NEAREST NEIGHBOR) CLUSTER
!              ANALYSIS USING HARTIGAN'S CLUSTER ANALYSIS ROUTINE
!              SLINK.
!
!              NOTE THAT THIS METHOD STARTS WITH A "DISTANCE" MATRIX
!              AS THE INPUT (THE CHOICE AND COMPUTATION FOR THE DISTANCE
!              MATRIX SHOULD BE PERFORMED BEFORE USING THIS COMMAND).
!
!     REFERENCES--JOHN HARTIGAN (1975), "CLUSTERING ALGORITHMS",
!                 JOHN WILEY, PP. 191-215.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2017/04
!     ORIGINAL VERSION--APRIL       2017.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION YA(NROW,*)
      DIMENSION WORK1(*)
      INTEGER IWORK(4,*)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*10 ITITLE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSL'
      ISUBN2='I2  '
      IWRITE='OFF'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SLI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPSLI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)NROW
   72   FORMAT('NROW = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 75 I=1,NROW
          WRITE(ICOUT,77)I,(YA(I,J),J=1,MIN(NROW,6))
   77     FORMAT('I,YA(I,1),YA(I,2),...,YA(I,6) = ',I8,2X,6G15.7)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
!
!               ************************************
!               **   STEP 2--                     **
!               **   PERFORM THE CLUSTER ANALYSIS **
!               ************************************
!
      IFAULT=0
      IDMWRK=4
!
      IF(INMCTI(1:4).EQ.'NULL')THEN
        ITITLE=' '
      ELSE
        ITITLE=INMCTI
      ENDIF
!
!CCCC CALL SLINK(NROW,NROW,YA,CLAB,ITITLE,IDMWRK,IWORK,WORK1,
!CCCC1           TLAB,IOUT,IFAULT)
      CALL SLINK(NROW,NROW,YA,IDMWRK,IWORK,WORK1)
!
      IF(IFAULT.EQ.1)THEN
        WRITE(ICOUT,211)
  211   FORMAT('****** ERROR IN SINGLE LINKAGE CLUSTERING--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)
  213   FORMAT('       EITHER THE FIRST OR LAST CASES OR THE CLUSTER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,215)
  215   FORMAT('       DIAMETER FOR A A CLUSTER IS OUT OF BOUNDS.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(IFAULT.EQ.2)THEN
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,215)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SLI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSLI2--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSLI2
      SUBROUTINE DPSLOC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A SPREAD-LOCATION (S-L) PLOT--
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/8
!     ORIGINAL VERSION--AUGUST    1999.
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
      CHARACTER*4 IHIGH
      CHARACTER*4 ICASE
!
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
!---------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
!
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION XHIGH2(MAXOBV)
      DIMENSION XHIGH3(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y1(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB6),XHIGH(1))
      EQUIVALENCE (GARBAG(IGARB7),XHIGH2(1))
      EQUIVALENCE (GARBAG(IGARB8),XHIGH3(1))
!
!-----COMMON----------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------
!
      IERROR='NO'
      IHIGH='OFF'
      ISUBN1='DPSL'
      ISUBN2='OC  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               *******************************************
!               **  TREAT THE SPREAD-LOCATION PLOT CASE  **
!               *******************************************
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SLOC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSLOC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SLOC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.2 .AND. ICOM.EQ.'SPRE' .AND.   &
         IHARG(1).EQ.'LOCA' .AND. IHARG(2).EQ.'PLOT')THEN
        ICASPL='SLOC'
        ILASTC=2
      ELSEIF(NUMARG.GE.3 .AND.   &
            (ICOM.EQ.'SUBS' .OR. ICOM.EQ.'HIGH') .AND.   &
             IHARG(1).EQ.'SPRE' .AND. IHARG(2).EQ.'LOCA' .AND.   &
             IHARG(3).EQ.'PLOT')THEN
        ICASPL='SLOC'
        ILASTC=3
        IHIGH='ON'
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SLOC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='SPREAD LOCATION PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      IF(IHIGH.EQ.'ON')THEN
        MINNVA=3
        MAXNVA=3
      ELSE
        MINNVA=2
        MAXNVA=2
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SLOC')THEN
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
                  Y1,X1,XHIGH,NLOCAL,NLOCAL,NLOCAL,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS       **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.          **
!               **************************************************
!
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SLOC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPSLO2(Y1,X1,NLOCAL,NUMVAR,ICASPL,ISIZE,   &
                  XIDTEM,TEMP,TEMP2,MAXOBV,   &
                  IHIGH,XHIGH,XHIGH2,XHIGH3,   &
                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SLOC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSLOC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,IHIGH,ISIZE
 9012   FORMAT('IFOUND,IERROR,IHIGH,ISIZE = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
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
      END SUBROUTINE DPSLOC
      SUBROUTINE DPSLO2(Y,X,N,NUMV2,ICASPL,ISIZE,   &
                        XIDTEM,TEMP,TEMP2,MAXOBV,   &
                        IHIGH,XHIGH,XHIGH2,XHIGH3,   &
                        Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN SPREAD-LOCATION PLOT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/8
!     ORIGINAL VERSION--AUGUST    1999.
!     UPDATED         --JANUARY   2012. SUPPORT FOR "HIGHLIGHT"
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IHIGH
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!----------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION XHIGH(*)
      DIMENSION XHIGH2(*)
      DIMENSION XHIGH3(*)
!
      DIMENSION XIDTEM(*)
      DIMENSION TEMP(*)
      DIMENSION TEMP2(*)
!
!-----COMMON-----------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT------------------------------------------------
!
      ISUBN1='DPSL'
      ISUBN2='O2  '
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'SLO2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPSLO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ICASPL,IHIGH,N,NUMV2,ISIZE
   71   FORMAT('ICASPL,IHIGH,N,NUMV2,ISIZE = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        DO 72 I=1,N
          WRITE(ICOUT,73)I,Y(I),X(I),XHIGH(I)
   73     FORMAT('I,Y(I),X(I),XHIGH(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
      ENDIF
!
      I2=0
      AN=0.0
!
      N50=1
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LE.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN SPREAD LOCATION PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.')
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
      HOLD=Y(1)
      DO 60 I=1,N
        IF(Y(I).NE.HOLD)GO TO 69
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)HOLD
   63 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS ARE IDENTICALLY ',   &
             'EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
   69 CONTINUE
!
!               *************************************************
!               **  STEP 1--                                   **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES    **
!               **  FOR VARIABLE 2 (THE GROUP VARIABLE).       **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS      **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE    **
!               **  WHICH IS AN ERROR CONDITION FOR A          **
!               **  SPREAD-LOCATION PLOT .                     **
!               *************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SLO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMSET=0
      DO 160 I=1,N
        IF(NUMSET.EQ.0)GO TO 165
        DO 170 J=1,NUMSET
          IF(X(I).EQ.XIDTEM(J))GO TO 160
  170   CONTINUE
  165   CONTINUE
        NUMSET=NUMSET+1
        XIDTEM(NUMSET)=X(I)
  160 CONTINUE
      CALL SORT(XIDTEM,NUMSET,XIDTEM)
!
      XID1=XIDTEM(1)
      XID2=XIDTEM(NUMSET)
!
      IF(NUMSET.EQ.0)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,191)
  191   FORMAT('      THE NUMBER OF GROUPS = 0')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NUMSET.EQ.N)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      THE NUMBER OF GROUPS = THE NUMBER OF ',   &
               'OBSERVATIONS.')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ************************************************
!               **  STEP 4--                                  **
!               **  DETERMINE PLOT COORDINATES                **
!               ************************************************
!
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SLO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      ITAG=1
      DO 1110 ISET=1,NUMSET
!
        K=0
        DO 1120 I=1,N
          IF(X(I).EQ.XIDTEM(ISET))THEN
            K=K+1
            TEMP(K)=Y(I)
            XHIGH2(K)=XHIGH(I)
          ENDIF
 1120   CONTINUE
        NI=K
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'SLO2')THEN
          WRITE(ICOUT,1121)ISET,XIDTEM(ISET),NI
 1121     FORMAT('ISET,XIDTEM(ISET),NI = ',I8,G15.7,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(NI.LE.1)GO TO 1110
        IF(IHIGH.EQ.'ON')THEN
          CALL SORTC(TEMP,XHIGH2,NI,TEMP,XHIGH3)
        ELSE
          CALL SORT(TEMP,NI,TEMP)
        ENDIF
        IWRITE='OFF'
        CALL MEDIAN(TEMP,NI,IWRITE,TEMP2,MAXOBV,XMED,IBUGG3,IERROR)
        ITAG=ITAG+1
        JSTART=J+1
        DO 1130 I=1,NI
          J=J+1
          Y2(J)=ABS(TEMP(I)-XMED)
          X2(J)=XMED
          D2(J)=REAL(ITAG)
 1130   CONTINUE
!
        CALL MEDIAN(Y2(JSTART),NI,IWRITE,TEMP2,MAXOBV,XMED2,IBUGG3,   &
                    IERROR)
!
        IF(IHIGH.EQ.'ON')THEN
          DO 1135 I=1,NI
            IF(XHIGH3(I).GE.0.5)THEN
              J=J+1
              Y2(J)=ABS(TEMP(I)-XMED)
              X2(J)=XMED
              D2(J)=REAL(ITAG+NUMSET)
            ENDIF
 1135     CONTINUE
        ENDIF
!
        DO 1140 I=JSTART,J
          Y2(I)=SQRT(Y2(I))
 1140   CONTINUE
        J=J+1
        Y2(J)=SQRT(XMED2)
        X2(J)=XMED
        D2(J)=1.0
!
 1110 CONTINUE
!
      N2=J
      NPLOTV=2
      GO TO 9000
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'SLO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSLO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NUMSET,N2,NI,NUMV2,AN,IERROR
 9012   FORMAT('NUMSET,N2,NI,NUMV2,AN,IERROR = ',4I8,G15.7,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSLO2
      SUBROUTINE DPSMAL(X,N,NMIN,XOUT,NOUT,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE RETURNS THE "NMIN" SMALLEST ELEMENTS
!              OF THE DATA IN THE INPUT VECTOR X.
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --NMIN   = THE NUMBER OF ELEMENTS TO RETURN
!     OUTPUT ARGUMENTS--XOUT   = THE SINGLE PRECISION VECTOR TO
!                                CONTAIN THE SMALLEST VALUES
!     OUTPUT--THE NMIN SMALLEST VALUES IN X.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--SORT.
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
!     VERSION NUMBER--2018.10
!     ORIGINAL VERSION--OCTOBER   2018.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XOUT(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSM'
      ISUBN2='AL  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SMAL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSMAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,NMIN
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I10,I6)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ***************************
!               **  FIND SMALLEST VALUES  **
!               ***************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DPSMAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      IN THE RESPONSE VARIABLE MUST BE 1 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ***********************************
!               **  STEP 2--                     **
!               **  SORT THE DATA AND EXTRACT    **
!               **  THE SMALLEST                  **
!               ***********************************
!
      CALL SORT(X,N,X)
      NSTOP=NMIN
      IF(NSTOP.GT.N)NSTOP=N
      NOUT=0
      DO 200 I=1,NSTOP
        NOUT=NOUT+1
        XOUT(NOUT)=X(I)
  200 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SMAL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSMAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NOUT
 9012   FORMAT('IERROR,NOUT = ',A4,2X,I10)
        CALL DPWRST('XXX','BUG ')
        IF(NOUT.GE.1)THEN
          DO 9014 I=1,NOUT
            WRITE(ICOUT,9015)I,XOUT(I)
 9015       FORMAT('I,XOUT(I) = ',I10,2X,G15.7)
            CALL DPWRST('XXX','BUG ')
 9014     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPSMAL
      SUBROUTINE DPSMO2(Y,W,N,ICASSM,IFILWI,IDEGRE,IRSTRI,NUMCRS,MAXCRS,   &
                        TEMP,MAXN,   &
                        RESSD,RESDF,PRED2,RES2,   &
                        IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE PERFORMS A SMOOTHING
!              OF THE DATA IN THE INPUT VECTOR Y.
!     NOTE--ASSUMPTION--DATA ARE EQUALLY-SPACED.
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                OF EQUALLY-SPACED OBSERVATIONS
!                                TO BE SMOOTHED.
!                       N      = THE INTEGER NUMBER OF
!                                OBSERVATIONS IN THE VECTOR Y.
!                       IFILWI = THE ODD INTEGER WIDTH OF THE
!                                SMOOTHING FUNCTION.
!                                (IFILWI MUST BE ODD,
!                                MUST BE BETWEEN 1 AND 999
!                                (INCLUSIVE),
!                                AND MUST BE BETWEEN IDEGRE+1 AND N
!                                (INCLUSIVE)).
!                       IDEGRE = THE INTEGER DEGREE OF THE LEAST
!                                SQUARES POLYNOMIAL.
!                                (IDEGRE MUST BE BETWEEN 0 AND 5,
!                                INCLUSIVE).
!     OUTPUT ARGUMENTS--PRED2  = THE SINGLE PRECISION VECTOR
!                                OF 'PREDICTED' OR SMOOTHED
!                                VALUES.
!                       RES2   = THE SINGLE PRECISION VECTOR
!                                OF RESIDUALS.
!                                (THE I-TH RESIDUAL IS THE DIFFERENCE
!                                BETWEEN THE I-TH INPUT VALUE AND
!                                THE I-TH SMOOTHED VALUE--
!                                RES2(I) = Y(I) - PRED2(I)).
!                       S      = THE SINGLE PRECISION VALUE OF THE
!                                RESIDUAL STANDARD DEVIATION
!                                (A MEASURE OF THE GOODNESS OF
!                                THE FIT OR THE SMOOTHING).
!     OUTPUT--COMPUTED (MOVING) LEAST SQUARES SMOOTHED VALUES,
!             RESIDUALS, AND THE RESIDUAL STANDARD DEVIATION.
!     PRINTING--YES (6 LINES)
!               OF INFORMATION INVOLVING
!               THE NUMBER OF OBSERVATIONS,
!               THE DEGREE OF THE SMOOTHING FUNCTION,
!               THE WIDTH  OF THE SMOOTHING FUNCTION,
!               THE STANDARD DEVIATION OF THE ORIGINAL
!               (UNSMOOTHED) DATA ABOUT THE SAMPLE MEAN,
!               THE STANDARD DEVIATION OF THE RESIDUALS
!               AFTER A (MOVING) AVERAGE SMOOTHING WITH
!               THE SAME WIDTH (IFILWI),
!               AND THE STANDARD DEVIATION OF THE RESIDUALS
!               AFTER THE LEAST SQUARES SMOOTHING WITH
!               DEGREE IDEGRE AND WIDTH IFILWI.
!     RESTRICTIONS--THERE IS NO UPPER LIMIT RESTRICTION ON N.
!                   IDEGRE MUST BE BETWEEN 0 AND 5 (INCLUSIVE).
!                   IFILWI MUST BE ODD,
!                          MUST BE BETWEEN 1 AND 999 (INCLUSIVE), AND
!                          MUST BE BETWEEN IDEGRE+1 AND N (INCLUSIVE).
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--MACHINE-INDEPENDENT ANSI FORTRAN (1977)
!     COMMENT--NEAR THE ENDS OF THE DATA SET WHERE
!              THE SMOOTHING FUNCTION GOES 'OFF THE END',
!              THE PREDICTED VALUE IS ASSIGNED THE VALUE OF THE
!              OBSERVATION ITSELF.  THIS IS DUE TO THE
!              COMPLICATED FORM OF THE NON-SYMMETRIC WEIGHTING
!              FOR THE LEAST SQUARES SMOOTHING NEAR THE ENDPOINTS.
!              THIS WILL BE CORRECTED IN THE FUTURE.
!     REFERENCE--HILDEBRAND, F. B.  INTRODUCTION TO NUMERICAL
!                ANALYSISY PAGES 295-302, ESPECIALLY 301.
!              --RALSTON, A.  A FIRST COURSE IN NUMERICAL ANALYSIS,
!                PAGES 250-254.
!              --SAVITSKY, A. AND GOLAY, M. J. E.  'SMOOTHING AND
!                DIFFERENTIATION OF DATA BY SIMPLIFIED LEAST
!                SQUARES PROCEDURES', ANALYTICAL CHEMISTRY,
!                JULY, 1964, PAGES 1627-1639.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MAY       1975.
!     UPDATED         --JULY      1976.
!     UPDATED         --JUNE      1978.
!     UPDATED         --FEBRUARY  1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JULY      1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASSM
      CHARACTER*4 IRSTRI
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASS2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION W(*)
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
!
      DIMENSION IRSTRI(*)
      DIMENSION TEMP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSM'
      ISUBN2='O2  '
      IERROR='NO'
!
      MAXDEG=6
      MAXWIN=MAXN-1
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('**** AT THE BEGINNING OF DPSMO2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)N,IBUGA3
   52 FORMAT('N,IBUGA3 = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICASSM,IFILWI,IDEGRE
   53 FORMAT('ICASSM,IFILWI,IDEGRE = ',A4,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMCRS,MAXCRS
   54 FORMAT('NUMCRS,MAXCRS = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)(IRSTRI(I),I=1,MAXCRS)
   55 FORMAT('IRSTRI(.) = ',30A1)
      CALL DPWRST('XXX','BUG ')
      DO 56 I=1,N
      WRITE(ICOUT,57)I,Y(I),W(I)
   57 FORMAT('I,Y(I),W(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   56 CONTINUE
   90 CONTINUE
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASS2='OTSM'
      IF(ICASSM.EQ.'SM')ICASS2='SM'
      IF(ICASSM.EQ.'0SM')ICASS2='SM'
      IF(ICASSM.EQ.'1SM')ICASS2='SM'
      IF(ICASSM.EQ.'2SM')ICASS2='SM'
      IF(ICASSM.EQ.'3SM')ICASS2='SM'
      IF(ICASSM.EQ.'4SM')ICASS2='SM'
      IF(ICASSM.EQ.'5SM')ICASS2='SM'
      IF(ICASSM.EQ.'6SM')ICASS2='SM'
      IF(ICASSM.EQ.'7SM')ICASS2='SM'
      IF(ICASSM.EQ.'8SM')ICASS2='SM'
      IF(ICASSM.EQ.'9SM')ICASS2='SM'
      IF(ICASSM.EQ.'10SM')ICASS2='SM'
      IF(ICASSM.EQ.'ROSM')ICASS2='ROSM'
!
      IF(N.LT.1)GO TO 110
      GO TO 119
  110 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN DPSMO2--THE NUMBER OF OBSERVATIONS ',   &
      'IN THE RESPONSE VARIABLE IS NON-POSITIVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)N
  112 FORMAT('SAMPLE SIZE = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  119 CONTINUE
!
      IF(N.EQ.1)GO TO 120
      GO TO 129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** NOTE FROM DPSMO2--THE RESPONSE VARIABLE ',   &
      'ONLY HAS 1 ELEMENT')
      CALL DPWRST('XXX','BUG ')
      DO 122 I=1,N
      PRED2(I)=Y(I)
      RES2(I)=0.0
  122 CONTINUE
      GO TO 9000
  129 CONTINUE
!
      HOLD=Y(1)
      DO 135 I=2,N
      IF(Y(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('***** NOTE FROM DPSMO2--THE RESPONSE VARIABLE ',   &
      'HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 132 I=1,N
      PRED2(I)=Y(I)
      RES2(I)=0.0
  132 CONTINUE
      GO TO 9000
  139 CONTINUE
!
      IF(IFILWI.GT.N)GO TO 140
      GO TO 149
  140 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,141)
  141 FORMAT('***** ERROR IN DPSMO2--THE WIDTH OF THE SMOOTHING ',   &
      ' WINDOW MUST NOT BE LARGER THAN THE SAMPLE SIZE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,142)IFILWI,N
  142 FORMAT('      WIDTH = ',I8,' SAMPLE SIZE = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  149 CONTINUE
!
      IF(IFILWI.GT.MAXWIN)GO TO 150
      GO TO 159
  150 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,151)MAXWIN
  151 FORMAT('***** ERROR IN DPSMO2--THE WIDTH OF THE SMOOTHING ',   &
      'WINDOW MUST NOT BE LARGER THAN ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,152)IFILWI
  152 FORMAT('WIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  159 CONTINUE
!
      IF(IFILWI.EQ.1)GO TO 160
      GO TO 169
  160 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,161)
  161 FORMAT('***** NOTE FROM DPSMO2--THE WIDTH OF THE SMOOTHING ',   &
      'WINDOW IS 1')
      CALL DPWRST('XXX','BUG ')
      DO 162 I=1,N
      PRED2(I)=Y(I)
      RES2(I)=0.0
  162 CONTINUE
      IERROR='YES'
      GO TO 9000
  169 CONTINUE
!
      IF(IFILWI.EQ.N)GO TO 170
      GO TO 179
  170 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,171)
  171 FORMAT('***** ERROR IN DPSMO2--THE WIDTH OF THE SMOOTHING ',   &
      'WINDOW IS IDENTICAL TO THE SAMPLE SIZE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,172)IFILWI,N
  172 FORMAT('WIDTH = ',I8,' SAMPLE SIZE = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  179 CONTINUE
!
      IEVODD=IFILWI-2*(IFILWI/2)
      IF(IEVODD.EQ.0)GO TO 180
      GO TO 189
  180 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,181)
  181 FORMAT('***** ERROR IN DPSMO2--THE WIDTH OF THE SMOOTHIN ',   &
      'WINDOW MUST BE ODD (AS OPPOSED TO EVEN')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,182)
  182 FORMAT('WIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  189 CONTINUE
!
      IF(IFILWI.LE.0)GO TO 200
      GO TO 209
  200 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,201)
  201 FORMAT('***** ERROR IN DPSMO2--THE WIDTH OF THE SMOOTHING ',   &
      'WINDOW WAS NON-POSITIVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,202)IFILWI
  202 FORMAT('WIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  209 CONTINUE
!
      IF(ICASSM.NE.'SM')GO TO 229
      IF(IDEGRE.GE.IFILWI)GO TO 220
      GO TO 229
  220 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,221)
  221 FORMAT('***** ERROR IN DPSMO2--THE DEGREE OF THE SMOOTHING ',   &
      'FUNCTION MUST BE SMALLER THAN THE SMOOTHING WIDTH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,222)IDEGRE,IFILWI
  222 FORMAT('DEGREE = ',I8,' WIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  229 CONTINUE
!
      IF(ICASSM.NE.'SM')GO TO 239
      IF(IDEGRE.GT.MAXDEG)GO TO 230
      GO TO 239
  230 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,231)MAXDEG
  231 FORMAT('***** ERROR IN DPSMO2--THE DEGREE OF THE SMOOTHING ',   &
      'FUNCTION MUST NOT EXCEED ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,232)IDEGRE
  232 FORMAT('DEGREE = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  239 CONTINUE
!
      IF(ICASSM.NE.'SM')GO TO 249
      IWM1=IFILWI-1
      IF(IDEGRE.EQ.IWM1)GO TO 240
      GO TO 249
  240 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,241)
  241 FORMAT('***** NOTE FROM DPSMO2--THE DEGREE OF THE SMOOTHING ',   &
      'FUNCTION WAS ONE LESS THAN THE WIDTH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,242)IFILWI
  242 FORMAT('      THEREFORE, THE SMOOTHED VALUES WILL BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,243)
  243 FORMAT('      IDENTICAL TO THE RAW DATA VALUES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,244)IDEGRE,IFILWI
  244 FORMAT('DEGREE = ',I8,' WIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 245 I=1,N
      PRED2(I)=Y(I)
      RES2(I)=Y(I)
  245 CONTINUE
      GO TO 9000
  249 CONTINUE
!
!CCCC IF(ICASS2.EQ.'ROSM')GO TO 260
!CCCC GO TO 269
!C260 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,261)
!C261 FORMAT('***** ERROR IN DPSMO2--THE ROBUST SMOOTHING')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,262)
!C262 FORMAT('      CAPABILITY IS NOT YET AVAILABLE')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!C269 CONTINUE
!
!C290 CONTINUE
!
!               ************************************************
!               **  STEP 2--                                  **
!               **  BRANCH TO THE APPROPRIATE SMOOTHING CASE  **
!               ************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFILWI.LE.0)IFILWI=3
      IF(IFILWI.GE.1)IFILWI=IFILWI
      IEVODD=IFILWI-2*(IFILWI/2)
      IF(IEVODD.EQ.0)IFILWI=IFILWI+1
!
      WIDTH=IFILWI
      IWHALF=(IFILWI/2)+1
      M=IFILWI/2
      AM=M
!
      IF(ICASSM.EQ.'0SM')GO TO 1100
      IF(ICASSM.EQ.'1SM')GO TO 1100
      IF(ICASSM.EQ.'2SM')GO TO 1120
      IF(ICASSM.EQ.'3SM')GO TO 1120
      IF(ICASSM.EQ.'4SM')GO TO 1140
      IF(ICASSM.EQ.'5SM')GO TO 1140
      IF(ICASSM.EQ.'6SM')GO TO 1160
      IF(ICASSM.EQ.'7SM')GO TO 1160
      IF(ICASSM.EQ.'8SM')GO TO 1180
      IF(ICASSM.EQ.'9SM')GO TO 1180
      IF(ICASSM.EQ.'10SM')GO TO 1200
!
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.0)GO TO 1100
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.1)GO TO 1100
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.2)GO TO 1120
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.3)GO TO 1120
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.4)GO TO 1140
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.5)GO TO 1140
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.6)GO TO 1160
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.7)GO TO 1160
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.8)GO TO 1180
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.8)GO TO 1180
      IF(ICASSM.EQ.'SM'.AND.IDEGRE.EQ.10)GO TO 1200
!
      IF(ICASSM.EQ.'MESM')GO TO 2100
      IF(ICASSM.EQ.'MDSM')GO TO 2200
      IF(ICASSM.EQ.'MMSM')GO TO 2300
      IF(ICASSM.EQ.'MRSM')GO TO 2400
      IF(ICASSM.EQ.'UQSM')GO TO 2500
      IF(ICASSM.EQ.'LQSM')GO TO 2600
      IF(ICASSM.EQ.'MXSM')GO TO 2700
      IF(ICASSM.EQ.'MNSM')GO TO 2800
      IF(ICASSM.EQ.'TRSM')GO TO 2900
      IF(ICASSM.EQ.'HMSM')GO TO 3000
!
      IF(ICASSM.EQ.'ROSM')GO TO 3100
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)
  811 FORMAT('***** INTERNAL ERROR IN DPSMO2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,812)
  812 FORMAT('      ICASSM NOT ONE OF THE ALLOWABLE TYPES--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,813)
  813 FORMAT('      SM, 0SM, 1SM, ..., 10SM, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,814)
  814 FORMAT('      MESM, MDSM, MMSM, MRSM, UQSM, LQSM, MXSM, ',   &
      'MNSM,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,815)
  815 FORMAT('      TRSM, OR ROSM')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,816)ICASSM
  816 FORMAT('      ICASSM = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
!               **********************************************
!               **  STEP 3--                                **
!               **  TREAT THE LEAST SQUARES SMOOTHING CASE  **
!               **********************************************
!
!               ********************************************
!               **  STEP 3.1--                            **
!               **  DETERMINE LEAST SQUARES COEFFICIENTS  **
!               **  FOR THE SPECIFIED DEGREE AND WIDTH    **
!               ********************************************
!
 1100 CONTINUE
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      FACTOR=1.0/WIDTH
      DO 1101 I=1,IWHALF
      IREV=IFILWI-I+1
      TEMP(I)=FACTOR
      TEMP(IREV)=TEMP(I)
 1101 CONTINUE
      GO TO 1900
!
 1120 CONTINUE
      FACTOR=3.0/((4.0*AM*AM-1.0)*(2.0*AM+3.0))
      DO 1121 I=1,IWHALF
      IREV=IFILWI-I+1
      R=I-M-1
      TEMP(I)=FACTOR*((3.0*AM*AM+3.0*AM-1.0)-(5.0*R*R))
      TEMP(IREV)=TEMP(I)
 1121 CONTINUE
      GO TO 1900
!
 1140 CONTINUE
      FACTOR=15.0/(4.0*(4.0*AM*AM-1.0)*(4.0*AM*AM-9.0)*(2.0*AM+5.0))
      DO 1141 I=1,IWHALF
      IREV=IFILWI-I+1
      R=I-M-1
      TERM1=15.0*(AM**4)+30.0*(AM**3)-35.0*(AM**2)-50.0*AM+12.0
      TERM2=35.0*(2.0*(AM**2)+2.0*AM-3.0)*(R**2)
      TERM3=63.0*(R**4)
      TEMP(I)=FACTOR*(TERM1-TERM2+TERM3)
      TEMP(IREV)=TEMP(I)
 1141 CONTINUE
      GO TO 1900
!
 1160 CONTINUE
      GO TO 1290
!
 1180 CONTINUE
      GO TO 1290
!
 1200 CONTINUE
      GO TO 1290
!
 1290 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1291)
 1291 FORMAT('***** ERROR IN DPSMO2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1292)
 1292 FORMAT('      THE CURRENT MAXIMUM ALLOWABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1293)
 1293 FORMAT('      DEGREE FOR LEAST SQUARES SMOOTHING')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1294)MAXDEG
 1294 FORMAT('      IS DEGREE ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1295)IDEGRE
 1295 FORMAT('      THE SPECIFIED DEGREE = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
!               **********************************************
!               **  STEP 3.2--                              **
!               **  COMPUTE SMOOTHED (= PREDICTED) VALUES.  **
!               **********************************************
!
 1900 CONTINUE
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1910 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 1930
      SUM=0.0
      ICOUNT=0
      DO 1920 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      SUM=SUM+TEMP(ICOUNT)*Y(J)
 1920 CONTINUE
      PRED2(I)=SUM
      GO TO 1910
 1930 CONTINUE
      PRED2(I)=Y(I)
 1910 CONTINUE
      GO TO 5000
!
!               ***********************************
!               **  STEP 4--                     **
!               **  TREAT THE FOLLOWING CASES--  **
!               **     1) MOVING MEAN            **
!               **     2) MOVING MEDIAN          **
!               **     3) MOVING MIDMEAN         **
!               **     4) MOVING MIDRANGE        **
!               **     5) MOVING UPPER QUARTILE  **
!               **     6) MOVING LOWER QUARTILE  **
!               **     7) MOVING MINIMUM         **
!               **     8) MOVING MAXIMUM         **
!               **     9) MOVING TRIANGLE        **
!               **    10) HAMMING                **
!               ***********************************
!
!               *************************************
!               **  STEP 4.1--                     **
!               **  TREAT THE MOVING AVERAGE CASE  **
!               *************************************
!
 2100 CONTINUE
      ISTEPN='4.1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=IFILWI
      IWHALF=(IFILWI/2)+1
      M=IFILWI/2
      COEF=1.0/WIDTH
!
      DO 2110 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2150
      SUM=0.0
      ICOUNT=0
      DO 2120 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      SUM=SUM+COEF*Y(J)
 2120 CONTINUE
      PRED2(I)=SUM
      GO TO 2110
 2150 CONTINUE
      PRED2(I)=Y(I)
 2110 CONTINUE
!
      GO TO 5000
!
!               ************************************
!               **  STEP 4.2--                    **
!               **  TREAT THE MOVING MEDIAN CASE  **
!               ************************************
!
 2200 CONTINUE
      ISTEPN='4.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=IFILWI
      IWHALF=(IFILWI/2)+1
      HALFSQ=IWHALF*IWHALF
      M=IFILWI/2
!
      DO 2210 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2250
      SUM=0.0
      ICOUNT=0
      DO 2220 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      TEMP(ICOUNT)=Y(J)
 2220 CONTINUE
      CALL SORT(TEMP,IFILWI,TEMP)
      PRED2(I)=TEMP(IWHALF)
      GO TO 2210
 2250 CONTINUE
      PRED2(I)=Y(I)
 2210 CONTINUE
!
      GO TO 5000
!
!               *************************************
!               **  STEP 4.3--                     **
!               **  TREAT THE MOVING MIDMEAN CASE  **
!               *************************************
!
 2300 CONTINUE
      ISTEPN='4.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      P1=0.25
      P2=0.25
!
      WIDTH=IFILWI
      IWHALF=(IFILWI/2)+1
      HALFSQ=IWHALF*IWHALF
      M=IFILWI/2
!
      DO 2310 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2370
      SUM=0.0
      ICOUNT=0
      DO 2320 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      TEMP(ICOUNT)=Y(J)
 2320 CONTINUE
      CALL SORT(TEMP,IFILWI,TEMP)
      IWP1=INT(P1*WIDTH+0.0001)
      ISTART=IWP1+1
      IWP2=INT(P2*WIDTH+0.0001)
      ISTOP=IFILWI-IWP2
      SUM=0.0
      K=0
      IF(ISTART.GT.ISTOP)GO TO 2360
      DO 2330 L=ISTART,ISTOP
      K=K+1
      SUM=SUM+TEMP(L)
 2330 CONTINUE
      AK=K
      YMIDM=SUM/AK
      GO TO 2380
 2360 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2361)
 2361 FORMAT('***** INTERNAL ERROR IN MMMSMO SUBROUTINE--',   &
       'THE START INDEX IS HIGHER THAN THE STOP INDEX ',   &
      'IN DO LOOP 2330')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 2380 CONTINUE
      PRED2(I)=YMIDM
      GO TO 2310
 2370 CONTINUE
      PRED2(I)=Y(I)
 2310 CONTINUE
!
      GO TO 5000
!
!               **************************************
!               **  STEP 4.4--                      **
!               **  TREAT THE MOVING MIDRANGE CASE  **
!               **************************************
!
 2400 CONTINUE
      ISTEPN='4.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=IFILWI
      M=IFILWI/2
!
      DO 2410 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2450
      SUM=0.0
      ICOUNT=0
      DO 2420 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      TEMP(ICOUNT)=Y(J)
 2420 CONTINUE
      CALL SORT(TEMP,IFILWI,TEMP)
      PRED2(I)=(TEMP(1)+TEMP(IFILWI))/2.0
      GO TO 2410
 2450 CONTINUE
      PRED2(I)=Y(I)
 2410 CONTINUE
!
      GO TO 5000
!
!               ********************************************
!               **  STEP 4.5--                            **
!               **  TREAT THE MOVING UPPER QUARTILE CASE  **
!               ********************************************
!
 2500 CONTINUE
      ISTEPN='4.5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=IFILWI
      P1=0.25
      IWP1=INT(P1*WIDTH+0.0001)
      IWP2=INT(IFILWI-IWP1)
      M=IFILWI/2
!
      DO 2510 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2550
      SUM=0.0
      ICOUNT=0
      DO 2520 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      TEMP(ICOUNT)=Y(J)
 2520 CONTINUE
      CALL SORT(TEMP,IFILWI,TEMP)
      PRED2(I)=TEMP(IWP2)
      GO TO 2510
 2550 CONTINUE
      PRED2(I)=Y(I)
 2510 CONTINUE
!
      GO TO 5000
!
!               ********************************************
!               **  STEP 4.6--                            **
!               **  TREAT THE MOVING LOWER QUARTILE CASE  **
!               ********************************************
!
 2600 CONTINUE
      ISTEPN='4.6'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=IFILWI
      P1=0.25
      IWP1=INT(P1*WIDTH+0.0001)
      M=IFILWI/2
!
      DO 2610 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2650
      SUM=0.0
      ICOUNT=0
      DO 2620 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      TEMP(ICOUNT)=Y(J)
 2620 CONTINUE
      CALL SORT(TEMP,IFILWI,TEMP)
      PRED2(I)=TEMP(IWP1)
      GO TO 2610
 2650 CONTINUE
      PRED2(I)=Y(I)
 2610 CONTINUE
!
!               *************************************
!               **  STEP 4.7--                     **
!               **  TREAT THE MOVING MAXIMUM CASE  **
!               *************************************
!
 2700 CONTINUE
      ISTEPN='4.7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=IFILWI
      M=IFILWI/2
!
      DO 2710 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2750
      YMAX=Y(JMIN)
      DO 2720 J=JMIN,JMAX
      IF(Y(J).GT.YMAX)YMAX=Y(J)
 2720 CONTINUE
      PRED2(I)=YMAX
      GO TO 2710
 2750 CONTINUE
      PRED2(I)=Y(I)
 2710 CONTINUE
!
      GO TO 5000
!
!               *************************************
!               **  STEP 4.8--                     **
!               **  TREAT THE MOVING MINIMUM CASE  **
!               *************************************
!
 2800 CONTINUE
      ISTEPN='4.8'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=IFILWI
      M=IFILWI/2
!
      DO 2810 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2850
      YMIN=Y(JMIN)
      DO 2820 J=JMIN,JMAX
      IF(Y(J).LT.YMIN)YMIN=Y(J)
 2820 CONTINUE
      PRED2(I)=YMIN
      GO TO 2810
 2850 CONTINUE
      PRED2(I)=Y(I)
 2810 CONTINUE
!
      GO TO 5000
!
!               *******************************************
!               **  STEP 4.9--                           **
!               **  TREAT THE TRIANGULAR SMOOTHING CASE  **
!               *******************************************
!
 2900 CONTINUE
      ISTEPN='4.9'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=IFILWI
      IWHALF=(IFILWI/2)+1
      HALFSQ=IWHALF*IWHALF
      M=IFILWI/2
!
      DO 2910 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 2950
      SUM=0.0
      ICOUNT=0
      DO 2920 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      IF(ICOUNT.LE.IWHALF)COEF=ICOUNT
      IF(ICOUNT.GT.IWHALF)COEF=IFILWI-ICOUNT+1
      COEF=COEF/HALFSQ
      SUM=SUM+COEF*Y(J)
 2920 CONTINUE
      PRED2(I)=SUM
      GO TO 2910
 2950 CONTINUE
      PRED2(I)=Y(I)
 2910 CONTINUE
!
      GO TO 5000
!
!               *******************************************
!               **  STEP 4.10--                          **
!               **  TREAT THE HAMMING  SMOOTHING CASE    **
!               *******************************************
!
 3000 CONTINUE
      ISTEPN='4.10'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WIDTH=3
      IWHALF=2
      HALFSQ=IWHALF*IWHALF
      M=1
!
      DO 3010 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 3050
      SUM=0.0
      ICOUNT=0
      DO 3020 J=JMIN,JMAX
      ICOUNT=ICOUNT+1
      IF(ICOUNT.LE.IWHALF)COEF=ICOUNT
      IF(ICOUNT.GT.IWHALF)COEF=IFILWI-ICOUNT+1
      COEF=COEF/HALFSQ
      SUM=SUM+COEF*Y(J)
 3020 CONTINUE
      PRED2(I)=SUM
      GO TO 3010
 3050 CONTINUE
      PRED2(I)=Y(I)
 3010 CONTINUE
!
      GO TO 5000
!
!               ***************************************
!               **  STEP 5--                         **
!               **  TREAT THE ROBUST SMOOTHING CASE  **
!               ***************************************
!
 3100 CONTINUE
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3191)
!3191 FORMAT('***** ERROR IN DPSMO2--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3192)
!3192 FORMAT('      THE ROBUST SMOOTHING CAPABILITY')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,3193)
!3193 FORMAT('      IS NOT YET AVAILABLE IN DATAPLOT.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!
      CALL DP3RSR(Y,TEMP,N,PRED2,RES2,IBUGA3,IERROR)
      GO TO 5000
!
!               *************************
!               **  STEP 11--          **
!               **  COMPUTE RESIDUALS  **
!               *************************
!
 5000 CONTINUE
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 5050 I=1,N
      RES2(I)=Y(I)-PRED2(I)
 5050 CONTINUE
!
!               *********************************************************
!               **  STEP 12--                                          **
!               **  COMPUTE VARIOUS MEASURES OF    GOODNESS OF FIT   --    **
!               **    1) THE STANDARD DEVIATION AND                    **
!               **       THE AVERAGE ABSOLUTE DEVIATION                **
!               **       OF THE RAW DATA                               **
!               **       (THAT IS, THE UNSMOOTHED DATA);               **
!               **    2) THE STANDARD DEVIATION AND                    **
!               **       THE AVERAGE ABSOLUTE DEVIATION                **
!               **       OF THE RESIDUALS FROM THE                     **
!               **       MOVING AVERAGE FIT WITH THE SPECIFIED WIDTH;  **
!               **    3) THE STANDARD DEVIATION AND                    **
!               **       THE AVERAGE ABSOLUTE DEVIATION                **
!               **       OF THE RESIDUALS FROM THE                     **
!               **       MOVING LEAST SQUARES FIT WITH THE             **
!               **       SPECIFIED DEGREE AND WIDTH.                   **
!               *********************************************************
!
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!
      AN=N
      SUM=0.0
      DO 5150 I=1,N
      SUM=SUM+Y(I)
 5150 CONTINUE
      YBAR=SUM/AN
!
      SUMSQ=0.0
      SUMAB=0.0
      DO 5155 I=1,N
      RESY=Y(I)-YBAR
      SUMSQ=SUMSQ+RESY*RESY
      SUMAB=SUMAB+ABS(RESY)
 5155 CONTINUE
      VARY=SUMSQ/(AN-1.0)
      SDY=0.0
      IF(VARY.GT.0.0)SDY=SQRT(VARY)
      AARY=SUMAB/AN
!
      SUMSQ=0.0
      SUMAB=0.0
      DO 5160 I=1,N
      JMIN=I-M
      JMAX=I+M
      IF(JMIN.LT.1.OR.JMAX.GT.N)GO TO 5160
      SUM=0.0
      DO 5165 J=JMIN,JMAX
      SUM=SUM+(1.0/WIDTH)*Y(J)
 5165 CONTINUE
      PREDMA=SUM
      RESMA=Y(I)-PREDMA
      SUMSQ=SUMSQ+RESMA*RESMA
      SUMAB=SUMAB+ABS(RESMA)
 5160 CONTINUE
      VARMA=SUMSQ/(AN-1.0)
      SDMA=0.0
      IF(VARMA.GT.0.0)SDMA=SQRT(VARMA)
      AARMA=SUMAB/AN
!
      DENOM=N-1
      SUMSQ=0.0
      SUMAB=0.0
      DO 5170 I=1,N
      SUMSQ=SUMSQ+RES2(I)**2
      SUMAB=SUMAB+ABS(RES2(I))
 5170 CONTINUE
      VAR=SUMSQ/DENOM
      S=0.0
      IF(VAR.GT.0.0)S=SQRT(VAR)
      RESDF=DENOM
      RESSD=S
      RESAAR=SUMAB/AN
!
!               ****************************
!               **  STEP 13--             **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='13'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 6190
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6105)
 6105 FORMAT('SMOOTHING')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
!
      IF(ICASS2.EQ.'SM')WRITE(ICOUT,6111)
 6111 FORMAT('      SMOOTHING FUNCTION--LEAST SQUARES')
      IF(ICASS2.EQ.'SM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MESM')WRITE(ICOUT,6112)
 6112 FORMAT('      SMOOTHING FUNCTION--MOVING MEAN')
      IF(ICASSM.EQ.'MESM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MDSM')WRITE(ICOUT,6113)
 6113 FORMAT('      SMOOTHING FUNCTION--MOVING MEDIAN')
      IF(ICASSM.EQ.'MDSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MMSM')WRITE(ICOUT,6114)
 6114 FORMAT('      SMOOTHING FUNCTION--MOVING MIDMEAN')
      IF(ICASSM.EQ.'MMSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MRSM')WRITE(ICOUT,6115)
 6115 FORMAT('      SMOOTHING FUNCTION--MOVING MIDRANGE')
      IF(ICASSM.EQ.'MRSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'UQSM')WRITE(ICOUT,6116)
 6116 FORMAT('      SMOOTHING FUNCTION--MOVING UPPER QUARTILE')
      IF(ICASSM.EQ.'UQSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'LQSM')WRITE(ICOUT,6117)
 6117 FORMAT('      SMOOTHING FUNCTION--MOVING LOWER QUARTILE')
      IF(ICASSM.EQ.'LQSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MXSM')WRITE(ICOUT,6118)
 6118 FORMAT('      SMOOTHING FUNCTION--MOVING MAXIMUM')
      IF(ICASSM.EQ.'MXSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MNSM')WRITE(ICOUT,6119)
 6119 FORMAT('      SMOOTHING FUNCTION--MOVING MINIMUM')
      IF(ICASSM.EQ.'MNSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'TRSM')WRITE(ICOUT,6120)
 6120 FORMAT('      SMOOTHING FUNCTION--MOVING TRIANGLE')
      IF(ICASSM.EQ.'TRSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'HMSM')WRITE(ICOUT,6121)
 6121 FORMAT('      SMOOTHING FUNCTION--HAMMING')
      IF(ICASSM.EQ.'HMSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'ROSM')WRITE(ICOUT,6122)
 6122 FORMAT('      SMOOTHING FUNCTION--ROBUST (3RSR)')
      IF(ICASSM.EQ.'ROSM')CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,6131)N
 6131 FORMAT('      NUMBER OF OBSERVATIONS       = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(ICASS2.EQ.'LSSQ')WRITE(ICOUT,6132)IFILWI
 6132 FORMAT('      WIDTH  OF SMOOTHING FUNCTION = ',I8)
      IF(ICASS2.EQ.'LSSQ')CALL DPWRST('XXX','BUG ')
      IF(ICASS2.EQ.'OTSQ')WRITE(ICOUT,6132)IFILWI
      IF(ICASS2.EQ.'OTSQ')CALL DPWRST('XXX','BUG ')
      IF(ICASS2.EQ.'LSSQ')WRITE(ICOUT,6133)IDEGRE
 6133 FORMAT('      DEGREE OF SMOOTHING FUNCTION = ',I8)
      IF(ICASS2.EQ.'LSSQ')CALL DPWRST('XXX','BUG ')
      IF(ICASS2.EQ.'ROSM')WRITE(ICOUT,6134)(IRSTRI(I),I=1,30)
 6134 FORMAT('      DEFINING STRING              = ',30A1)
      IF(ICASS2.EQ.'ROSM')CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6135)
 6135 FORMAT('****************************************************',   &
      '*******************')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6136)
 6136 FORMAT('*                               * ','   RESIDUAL    ',   &
      ' * ','   AVERAGE     ',' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6137)
 6137 FORMAT('*                               * ','   STANDARD    ',   &
      ' * ','   ABSOLUTE    ',' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6138)
 6138 FORMAT('*                               * ','   DEVIATION   ',   &
      ' * ','   RESIDUAL    ',' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6139)
 6139 FORMAT('*                               * ',' (DIVISOR=N-1) ',   &
      ' * ','  (DIVISOR=N)  ',' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6135)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6141)SDY,AARY
 6141 FORMAT('* NO SMOOTHING                  * ',F15.7,' * ',F15.7,   &
      ' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6142)
 6142 FORMAT('* (RAW DATA)',5X,'         ',4X,'  * ',15X,' * ',15X,   &
      ' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6143)
 6143 FORMAT('*           ',5X,'         ',4X,'  * ',15X,' * ',15X,   &
      ' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6144)SDMA,AARMA
 6144 FORMAT('* MOVING AVERAGE SMOOTHING      * ',F15.7,' * ',F15.7,   &
      ' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6145)IFILWI
 6145 FORMAT('* WIDTH =',I8,'         ',4X,'  * ',15X,' * ',15X,   &
      ' *')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6143)
      CALL DPWRST('XXX','BUG ')
!
      IF(ICASS2.EQ.'SM')WRITE(ICOUT,6151)RESSD,RESAAR
 6151 FORMAT('* LEAST SQUARES SMOOTHING       * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASS2.EQ.'SM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MESM')WRITE(ICOUT,6152)RESSD,RESAAR
 6152 FORMAT('* MOVING MEAN SMOOTHING         * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'MESM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MDSM')WRITE(ICOUT,6153)RESSD,RESAAR
 6153 FORMAT('* MOVING MEDIAN SMOOTHING       * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'MDSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MMSM')WRITE(ICOUT,6154)RESSD,RESAAR
 6154 FORMAT('* MOVING MIDMEAN SMOOTHING      * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'MMSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MRSM')WRITE(ICOUT,6155)RESSD,RESAAR
 6155 FORMAT('* MOVING MIDRANGE SMOOTHING     * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'MRSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'UQSM')WRITE(ICOUT,6156)RESSD,RESAAR
 6156 FORMAT('* MOVING UPPER QUAR. SMOOTHING  * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'UQSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'LQSM')WRITE(ICOUT,6157)RESSD,RESAAR
 6157 FORMAT('* MOVING LOWER QUAR. SMOOTHING  * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'LQSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MXSM')WRITE(ICOUT,6158)RESSD,RESAAR
 6158 FORMAT('* MOVING MAXIMUM SMOOTHING      * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'MXSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'MNSM')WRITE(ICOUT,6159)RESSD,RESAAR
 6159 FORMAT('* MOVING MINIMUM SMOOTHING      * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'MNSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'TRSM')WRITE(ICOUT,6160)RESSD,RESAAR
 6160 FORMAT('* MOVING TRIANGLE SMOOTHING     * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'TRSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'HMSM')WRITE(ICOUT,6161)RESSD,RESAAR
 6161 FORMAT('* HAMMING SMOOTHING             * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'HMSM')CALL DPWRST('XXX','BUG ')
      IF(ICASSM.EQ.'ROSM')WRITE(ICOUT,6162)RESSD,RESAAR
 6162 FORMAT('* ROBUST SMOOTHING (3RSR)       * ',F15.7,' * ',F15.7,   &
      ' *')
      IF(ICASSM.EQ.'ROSM')CALL DPWRST('XXX','BUG ')
!
      IF(ICASS2.EQ.'SM')WRITE(ICOUT,6171)IFILWI,IDEGRE
 6171 FORMAT('* WIDTH =',I8,' DEGREE =',I4,'  * ',15X,' * ',15X,   &
      ' *')
      IF(ICASS2.EQ.'SM')CALL DPWRST('XXX','BUG ')
      IF(ICASS2.EQ.'OTSM')WRITE(ICOUT,6172)IFILWI
 6172 FORMAT('* WIDTH =',I8,'         ',4X,'  * ',15X,' * ',15X,   &
      ' *')
      IF(ICASS2.EQ.'OTSM')CALL DPWRST('XXX','BUG ')
      IF(ICASS2.EQ.'ROSM')WRITE(ICOUT,6173)(IRSTRI(I),I=1,30)
 6173 FORMAT('* ',30A1,'* ',15X,' * ',15X,   &
      ' *')
      IF(ICASS2.EQ.'ROSM')CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,6135)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 6190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSMO2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012 FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICASSM,ICASS2,IFILWI,IDEGRE
 9013 FORMAT('ICASSM,ICASS2,IFILWI,IDEGRE = ',A4,2X,A4,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMCRS,MAXCRS
 9014 FORMAT('NUMCRS,MAXCRS = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)(IRSTRI(I),I=1,MAXCRS)
 9015 FORMAT('IRSTRI(.) = ',30A1)
      CALL DPWRST('XXX','BUG ')
      DO 9016 I=1,N
      WRITE(ICOUT,9017)I,Y(I),W(I),PRED2(I),RES2(I)
 9017 FORMAT('I,Y(I),W(I),PRED2(I),RES2(I) = ',I8,4E15.7)
      CALL DPWRST('XXX','BUG ')
 9016 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPSMO2
      SUBROUTINE DPSMOO(IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A SMOOTHING OPERATION.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBUG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JUNE      1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --JULY      1979.
!     UPDATED         --FEBRUARY  1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JULY      1983.
!     UPDATED         --MARCH     1988.      ADD LOFCDF
!     UPDATED         --JUNE      1990.      TEMPORARY ARRAYS TO GARBAGE COMMON
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IRSTRI
      CHARACTER*4 ICASSM
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION TEMP(MAXOBV)
      DIMENSION IRSTRI(30)
!
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
!
      DIMENSION W(MAXOBV)
!
!CCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB2),PRED2(1))
      EQUIVALENCE (GARBAG(IGARB3),RES2(1))
      EQUIVALENCE (GARBAG(IGARB4),W(1))
!CCCC END CHANGE
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DPSM'
      ISUBN2='OO  '
      ICASEQ='UNKN'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      MAXV2=1
      MINN2=2
      MAXCRS=30
      NUMCRS=0
      DO 10 I=1,MAXCRS
        IRSTRI(I)=' '
   10 CONTINUE
!
!               ********************************
!               **  TREAT THE SMOOTHING CASE  **
!               ********************************
!
!
      IF(IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSMOO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *********************************
!               **  STEP 1.1--                 **
!               **  SEARCH FOR SMOOTH          **
!               **  (WITH UNSPECIFIED DEGREE)  **
!               *********************************
!
      ICASSM='SM'
!
      IF(ICOM.EQ.'SMOO')GO TO 110
!
!               *******************************************
!               **  STEP 1.2--                           **
!               **  SEARCH FOR ROBUST         SMOOTHING  **
!               *******************************************
!
      ICASSM='ROSM'
!
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'ROBU'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.3--                           **
!               **  SEARCH FOR MOVING AVERAGE SMOOTHING  **
!               *******************************************
!
      ICASSM='MESM'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'AVER'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'MEAN'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'XBAR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'AVER'.AND.IHARG(1).EQ.'SMOO')GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'MEAN'.AND.IHARG(1).EQ.'SMOO')GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'XBAR'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.4--                           **
!               **  SEARCH FOR MOVING MEDIAN   SMOOTHING  **
!               *******************************************
!
      ICASSM='MDSM'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'MEDI'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'MEDI'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.5--                           **
!               **  SEARCH FOR MOVING MIDMEAN SMOOTHING  **
!               *******************************************
!
      ICASSM='MMSM'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'MIDM'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'MIDM'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.6--                           **
!               **  SEARCH FOR MOVING MIDRANGE SMOOTHING **
!               *******************************************
!
      ICASSM='MRSM'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'MIDR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'MIDR'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               **************************************************
!               **  STEP 1.7--                                  **
!               **  SEARCH FOR MOVING UPPER QUARTILE SMOOTHING  **
!               **************************************************
!
      ICASSM='UQSM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'UPPE'.AND.IHARG(2).EQ.'QUAR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'UPPE'.AND.IHARG(1).EQ.'QUAR'.AND.   &
      IHARG(2).EQ.'SMOO')GO TO 112
!
!               **************************************************
!               **  STEP 1.8--                                  **
!               **  SEARCH FOR MOVING LOWER QUARTILE SMOOTHING  **
!               **************************************************
!
      ICASSM='LQSM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'LOWE'.AND.IHARG(2).EQ.'QUAR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'LOWE'.AND.IHARG(1).EQ.'QUAR'.AND.   &
      IHARG(2).EQ.'SMOO')GO TO 112
!
!               *******************************************
!               **  STEP 1.9--                           **
!               **  SEARCH FOR MOVING MAXIMUM  SMOOTHING **
!               *******************************************
!
      ICASSM='MXSM'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'MAXI'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'MAXI'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.10--                          **
!               **  SEARCH FOR MOVING MINIMUM  SMOOTHING **
!               *******************************************
!
      ICASSM='MNSM'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'MINI'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'MIDR'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *********************************************
!               **  STEP 1.11--                            **
!               **  SEARCH FOR MOVING TRIANGULAR SMOOTHING **
!               *********************************************
!
      ICASSM='TRSM'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'TRIA'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'TRIA'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *********************************************
!               **  STEP 1.12--                            **
!               **  SEARCH FOR HAMMING SMOOTHING **
!               *********************************************
!
      ICASSM='HMSM'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MOVI'.AND.IHARG(1).EQ.'HAMM'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'HAMM'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.20--                          **
!               **  SEARCH FOR 0-TH DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='0SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'0'.AND.IHARG(1).EQ.'TH'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'0TH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'ZERO'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'0'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'0'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'ZERO'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'CONS'.AND.IHARG(1).EQ.'SMOO')GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'RECT'.AND.IHARG(1).EQ.'SMOO')GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'FLAT'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.21--                          **
!               **  SEARCH FOR 1-ST DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='1SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'1'.AND.IHARG(1).EQ.'ST'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'1ST'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'FIRS'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'1'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'ONE'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'1'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'ONE'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'LINE'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.22--                          **
!               **  SEARCH FOR 2-ND DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='2SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'2'.AND.IHARG(1).EQ.'ND'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'2ND'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'SECO'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'2'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'TWO'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'2'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'TWO'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'QUAD'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.23--                          **
!               **  SEARCH FOR 3-RD DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='3SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'3'.AND.IHARG(1).EQ.'RD'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'3RD'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'THIR'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'3'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'THRE'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'3'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'THRE'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'CUBI'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.24--                          **
!               **  SEARCH FOR 4-TH DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='4SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'4'.AND.IHARG(1).EQ.'TH'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'4TH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'FOUR'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'4'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'FOUR'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'4'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'FOUR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'QUAR'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.25--                          **
!               **  SEARCH FOR 5-TH DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='5SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'5'.AND.IHARG(1).EQ.'TH'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'5TH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'FIFT'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'5'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'FIVE'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'5'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'FIVE'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'QUIN'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.26--                          **
!               **  SEARCH FOR 6-TH DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='6SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'6'.AND.IHARG(1).EQ.'TH'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'6TH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'SIXT'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'6'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'SIX'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'6'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'SIX'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'SEXT'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.27--                          **
!               **  SEARCH FOR 7-TH DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='7SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'7'.AND.IHARG(1).EQ.'TH'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'7TH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'SEVE'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'7'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'SEVE'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'7'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'SEVE'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'SEPT'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.28--                          **
!               **  SEARCH FOR 8-TH DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='8SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'8'.AND.IHARG(1).EQ.'TH'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'8TH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'EIGH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'8'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'EIGH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'8'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'EIGH'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'QUIN'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.29--                          **
!               **  SEARCH FOR 9-TH DEGREE    SMOOTHING  **
!               *******************************************
!
      ICASSM='9SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'9'.AND.IHARG(1).EQ.'TH'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'9TH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'NINT'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'9'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'NINE'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'9'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'NINE'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'NONI'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               *******************************************
!               **  STEP 1.20--                          **
!               **  SEARCH FOR 10-TH DEGREE   SMOOTHING  **
!               *******************************************
!
      ICASSM='10SM'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'10'.AND.IHARG(1).EQ.'TH'.AND.IHARG(2).EQ.'DEGR'.AND.   &
      IHARG(3).EQ.'SMOO')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'10TH'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'TENT'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'10'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'TEN'.AND.IHARG(1).EQ.'DEGR'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'10'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'DEGR'.AND.IHARG(1).EQ.'TEN'.AND.IHARG(2).EQ.'SMOO')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'DEXI'.AND.IHARG(1).EQ.'SMOO')GO TO 111
!
!               ********************************************
!               **  STEP 1.31--                           **
!               **  SINCE VALID COMMAND NOT FOUND, EXIT.  **
!               ********************************************
!
      ICASSM='    '
!
      IFOUND='NO'
      GO TO 9000
!
  110 CONTINUE
      ILASTC=0
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  111 CONTINUE
      ILASTC=1
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  112 CONTINUE
      ILASTC=2
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  113 CONTINUE
      ILASTC=3
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  180 CONTINUE
      IFOUND='YES'
      GO TO 190
!
  190 CONTINUE
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
      IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************************
!               **  STEP 3--                              **
!               **  CHECK THE VALIDITY OF ARGUMENT 1      **
!               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
!               ********************************************
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
!
!               ***********************************************************
!               **  STEP 4--                                             **
!               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)  **
!               **  FOR THE RESPONSE VARIABLE IS 2 OR MORE.              **
!               ***********************************************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NLEFT.GE.MINN2)GO TO 390
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,311)
  311 FORMAT('***** ERROR IN DPSMOO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,312)
  312 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,313)
  313 FORMAT('      (FOR WHICH AN SMOOTHING ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,314)
  314 FORMAT('      WAS TO HAVE BEEN CARRIED OUT)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,315)MINN2
  315 FORMAT('      MUST BE ',I8,' OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,316)
  316 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,317)
  317 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,318)(IANS(I),I=1,IWIDTH)
  318 FORMAT(80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  390 CONTINUE
!
!               *****************************************
!               **  STEP 5--                           **
!               **  CHECK TO SEE THE TYPE CASE--       **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 490
      DO 400 J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GO TO 410
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GO TO 410
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GO TO 420
  400 CONTINUE
      GO TO 490
  410 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GO TO 490
  420 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 490
  490 CONTINUE
      IF(IBUGA2.EQ.'OFF')GO TO 495
      WRITE(ICOUT,491)NUMARG,ILOCQ
  491 FORMAT('NUMARG,ILOCQ = ',2I8)
      CALL DPWRST('XXX','BUG ')
  495 CONTINUE
!
!               *********************************************
!               **  STEP 5--                               **
!               **  TEMPORARILY FORM THE VARIABLE Y(.)     **
!               **  WHICH WILL HOLD THE RESPONSE VARIABLE. **
!               **  FORM THIS VARIABLE BY                  **
!               **  BRANCHING TO THE APPROPRIATE SUBCASE   **
!               **  (FULL, SUBSET, OR FOR).                **
!               *********************************************
!
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL')GO TO 510
      IF(ICASEQ.EQ.'SUBS')GO TO 520
      IF(ICASEQ.EQ.'FOR')GO TO 530
!
  510 CONTINUE
      DO 515 I=1,NLEFT
      ISUB(I)=1
  515 CONTINUE
      NQ=NLEFT
      GO TO 550
!
  520 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GO TO 550
!
  530 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GO TO 550
!
  550 CONTINUE
      IF(NQ.GE.MINN2)GO TO 560
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,551)
  551 FORMAT('***** ERROR IN DPSMOO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,552)
  552 FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',   &
      'EXTRACTED,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,553)IHLEFT,IHLEF2
  553 FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING',   &
      'FROM VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,554)
  554 FORMAT('      (FOR WHICH SMOOTHING ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,555)
  555 FORMAT('      IS TO BE DONE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,556)MINN2
  556 FORMAT('      MUST BE ',I8,' OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,557)
  557 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,558)
  558 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,559)(IANS(I),I=1,IWIDTH)
  559 FORMAT('      ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  560 CONTINUE
      J=0
      IMAX=NLEFT
      IF(NQ.LT.NLEFT)IMAX=NQ
      DO 570 I=1,IMAX
      IF(ISUB(I).EQ.0)GO TO 570
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
  570 CONTINUE
      NS=J
!
!               ***********************************************************
!               **  STEP 6--                                             **
!               **  DETERMINE IF THE ANALYST                             **
!               **  HAS SPECIFIED THE WIDTH                              **
!               **  DESIRED FOR THE SMOOTHING FUNCTION.                  **
!               **  THIS IS DONE BY PRIOR USE OF THE                     **
!               **  FILTER WIDTH    COMMAND.                             **
!               **  IF FOUND, USE THE SPECIFIED VALUE                    **
!               **  (WHICH MUST BE BETWEEN 1 AND 1000, INCLUSIVE);       **
!               **  IF NOT FOUND, USE THE DEFAULT VALUE                  **
!               **  (USUALLY 11) WHICH WILL BE DEFINED                   **
!               **  IN THE SUBROUTINE DPSMO2.                            **
!               **  DETERMINE IF THE ANALYST                             **
!               **  HAS SPECIFIED THE DEGREE                             **
!               **  DESIRED FOR THE SMOOTHING FUNCTION.                  **
!               **  THIS IS DONE BY PRIOR USE OF THE                     **
!               **  POLYNOMIAL DEGREE    COMMAND.                        **
!               **  IF FOUND, USE THE SPECIFIED VALUE                    **
!               **  (WHICH MUST BE BETWEEN 1 AND 1000, INCLUSIVE);       **
!               **  IF NOT FOUND, USE THE DEFAULT VALUE                  **
!               **  (USUALLY 1) WHICH WILL BE DEFINED                    **
!               **  IN THE SUBROUTINE DPSMO2.                            **
!               ***********************************************************
!
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(FILWID.EQ.CPUMIN)IFILWI=3
      IF(FILWID.NE.CPUMIN)IFILWI=INT(FILWID+0.5)
!
      IDEGRE=IDEG
      IF(IDEG.LT.0)IDEGRE=1
      IF(ICASSM.EQ.'0SM')IDEGRE=0
      IF(ICASSM.EQ.'1SM')IDEGRE=1
      IF(ICASSM.EQ.'2SM')IDEGRE=2
      IF(ICASSM.EQ.'3SM')IDEGRE=3
      IF(ICASSM.EQ.'4SM')IDEGRE=4
      IF(ICASSM.EQ.'5SM')IDEGRE=5
      IF(ICASSM.EQ.'6SM')IDEGRE=6
      IF(ICASSM.EQ.'7SM')IDEGRE=7
      IF(ICASSM.EQ.'8SM')IDEGRE=8
      IF(ICASSM.EQ.'9SM')IDEGRE=9
      IF(ICASSM.EQ.'10SM')IDEGRE=10
!
!               *******************************************
!               **  STEP 7--                             **
!               **  FOR THE ROBUST SMOOTHING CASE ONLY,  **
!               **  EXTRACT THE DEFINING STRING.         **
!               *******************************************
!
      ISTEPN='7'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC IF(ICASSM.EQ.'ROSM')GO TO 810
      GO TO 990
!
!C810 CONTINUE
      IMAX=IWIDTH-5
      IF(IMAX.LT.1)GO TO 829
      DO 820 I=1,IMAX
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
      IF(IANS(I).EQ.'R'.AND.IANS(IP1).EQ.'O'.AND.   &
      IANS(IP2).EQ.'B'.AND.IANS(IP3).EQ.'U'.AND.   &
      IANS(IP4).EQ.'S'.AND.IANS(IP5).EQ.'T')GO TO 839
  820 CONTINUE
  829 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,831)
  831 FORMAT('***** INTERNAL ERROR IN DPSMOO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,832)
  832 FORMAT('      THE 6A1 STRING   ROBUST   NOT FOUND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,833)
  833 FORMAT('      ON THE COMMAND LINE EVEN THOUGH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,834)
  834 FORMAT('      THE CASE WAS PREVIOUSLY IDENTIFIED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,835)
  835 FORMAT('      AS BEING THE ROBUST SMOOTHING CASE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,836)ICASSM,IFILWI
  836 FORMAT('ICASSM,IFILWI = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,837)
  837 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,838)(IANS(I),I=1,IWIDTH)
  838 FORMAT('      ',120A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  839 CONTINUE
      IEND1=IP5
!
      IEND1P=IEND1+1
      IF(IEND1P.GT.IWIDTH)GO TO 859
      DO 850 I=IEND1P,IWIDTH
      I2=I
      IF(IANS(I).NE.' ')GO TO 869
  850 CONTINUE
  859 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,861)
  861 FORMAT('***** ERROR IN DPSMOO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,862)
  862 FORMAT('      THE WORD     ROBUST   ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,863)
  863 FORMAT('      SHOULD HAVE BEEN (BUT WAS NOT)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,864)
  864 FORMAT('      FOLLOWED BY A CHARACTER STRING')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,865)
  865 FORMAT('      DEFINING THE DESIRED ROBUST SMOOTHER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,866)ICASSM,IFILWI
  866 FORMAT('ICASSM,IFILWI = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,867)
  867 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,868)(IANS(I),I=1,IWIDTH)
  868 FORMAT('      ',120A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  869 CONTINUE
      ISTAR2=I2
!
      ICOUNT=0
      IF(ISTAR2.GT.IWIDTH)GO TO 889
      DO 880 I=ISTAR2,IWIDTH
      IF(IANS(I).EQ.' ')GO TO 899
      ICOUNT=ICOUNT+1
      IF(ICOUNT.GT.MAXCRS)GO TO 889
      IRSTRI(ICOUNT)=IANS(I)
  880 CONTINUE
      GO TO 899
  889 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,891)
  891 FORMAT('***** ERROR IN DPSMOO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,892)
  892 FORMAT('      THE CHARACTER STRING WHICH DEFINES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,893)
  893 FORMAT('      THE DESIRED ROBUST SMOOTHER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,894)
  894 FORMAT('      HAS EXCEEDED THE MAXIMUM ALLOWABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,895)MAXCRS
  895 FORMAT('      LENGTH OF ',I8,' CHARACTERS.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,897)
  897 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,898)(IANS(I),I=1,IWIDTH)
  898 FORMAT('      ',120A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  899 CONTINUE
      NUMCRS=ICOUNT
!
  990 CONTINUE
!
!               ****************************************************************
!               **  STEP 8--
!               **  PREPARE FOR ENTRANCE INTO DPSMO2--
!               **  SET THE WEIGHT VECTOR TO UNITY THROUGHOUT.
!               ****************************************************************
!
      ISTEPN='8'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1110 I=1,NS
      W(I)=1.0
 1110 CONTINUE
!
!               *********************************
!               **  STEP 9--                   **
!               **  FORM THE SMOOTHED VALUES.  **
!               *********************************
!
      ISTEPN='9'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IBUGA2.EQ.'OFF')GO TO 1290
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** FROM DPSMOO, AS WE ARE ABOUT TO CALL DPSMO2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)NLEFT,MAXN,NS
 1212 FORMAT('NLEFT,MAXN,NS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO 1215 I=1,NS
      WRITE(ICOUT,1216)I,Y(I),W(I)
 1216 FORMAT('I,Y(I),W(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 1215 CONTINUE
!CCCC IBUGA3='ABCD'
      WRITE(ICOUT,1231)IBUGA3
 1231 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','BUG ')
 1290 CONTINUE
!
      CALL DPSMO2(Y,W,NS,ICASSM,IFILWI,IDEGRE,IRSTRI,NUMCRS,MAXCRS,   &
      TEMP,MAXN,   &
      RESSD,RESDF,PRED2,RES2,   &
      IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 10--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='10'
      IF(IBUGA2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
!
!     THE FOLLOWING CORRECTION WAS BASED ON
!     COMMENTS FROM DAVE EVANS     AUGUST 1987
!CCCC IREPU='ON'
      IREPU='OFF'
      REPSD=(-999.0)
      REPDF=(-999.0)
      ALFCDF=(-999.99)
!
      IRESU='ON'
!
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NLEFT,   &
      IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSMOO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA2,IBUGA3
 9012 FORMAT('IBUGA2,IBUGA3 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGQ
 9013 FORMAT('IBUGQ = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NS,ICASSM
 9014 FORMAT('NS,ICASSM = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)ICASEQ
 9015 FORMAT('ICASEQ = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IFOUND,IERROR
 9016 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPSMOO

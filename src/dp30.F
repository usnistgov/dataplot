      SUBROUTINE DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
C
C     NOTE--THIS SUBROUTINE IS IDENTICAL TO THE DPSUB2 SUBROUTINE
C           AND THE DPSUB3 SUBROUTINE
C           AND HAS BEEN DUPLICATED TO THEM ONLY FOR ECONOMY OF MAPPING PURPOSES
C           THAT IS, TO SAVE STORAGE IN THE MAPPING.
C           FOR VIRTUAL OPERATING SYSTEMS, THIS DUPLICATION IS NEEDLESS.
C           ANY CALLS TO SUBROUTINES DPSUB2 AND SPSUB3 COULD BE CHANGED
C           TO CALLS TO DPSUBS.
C
C     PURPOSE--DEFINE AN INTEGER 0-1 VECTOR ISUB
C              WHICH WILL BE USED IN OTHER SUBROUTINES
C              FOR EXTRACTING SUBSETS.
C     NOTE THAT IF THE WORDS   SUBSET   OR   EXCEPT   IS NOT
C     IN THE ARGUMENT LIST,
C     THEN THE OUTPUT PARAMETER WILL BE SET TO NUMARG+1.
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
C     ORIGINAL VERSION--JANUARY  1978.
C     UPDATED         --JANUARY   1978.
C     UPDATED         --FEBRUARY  1978.
C     UPDATED         --MAY       1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1988.  ALLOW    NOT EQUAL   <> >< NOT=
C     UPDATED         --JANUARY   1989.  CHECK FOR EMPTY SUBSETS (ALAN)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGQ
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASSC
      CHARACTER*4 ICASQU
      CHARACTER*4 ICASVA
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASOP
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
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
      ISUBN1='DPSU'
      ISUBN2='BS  '
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      TARGET=0.0
C
C               ********************************
C               **  TREAT THE SUBSET CASE     **
C               ********************************
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSUBS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NIOLD,ILOCS,NS
   52   FORMAT('NIOLD,ILOCS,NS = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGQ,IERROR
   54   FORMAT('IBUGQ,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NUMARG,NUMNAM,MAXNAM,N,MAXN
   55   FORMAT('NUMARG,NUMNAM,MAXNAM,N,MAXN = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IWIDTH,ILOCS,ILOCS2,ILOCTG
   56   FORMAT('IWIDTH,ILOCS,ILOCS2,ILOCTG = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  INITIALIZE THE SUBSET SIZE (NS) TO NIOLD. CHECK   **
C               **  FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  ALSO   **
C               **  CHECK THAT THE RELEVANT NUMBER OF OBSERVATIONS    **
C               **  (NIOLD) IS POSITIVE.                              **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NS=NIOLD
      ILOCS=NUMARG+1
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1            IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(NIOLD.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DPSUBS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS (FROM WHICH')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      A SUBSET WAS TO HAVE BEEN EXTRACTED) IS 0.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 2.1--                                        **
C               **  INITIALIZE ALL ELEMENTS IN ISUB(.) TO 11          **
C               **  ISUB(.) WILL TAKE ON 4 VALUES AT MOST--           **
C               **  00, 01, 10, 11   .                                **
C               **  THE FIRST  DIGIT INDICATES WHETHER OR NOT THE     **
C               **  GIVEN ELEMENT IS OUT (0) OR IN (1) OF THE LOCAL   **
C               **  CUMULATIVE UNION SET.                             **
C               **  THE SECOND DIGIT INDICATES WHETHER OR NOT THE     **
C               **  GIVEN ELEMENT IS OUT (0) OR IN (1) OF THE GLOBAL  **
C               **  CUMULATIVE INTERSECTION SET.                      **
C               **  THE INITIALIZATION OF ALL ELEMENTS TO 11 THUS     **
C               **  INDICATES THAT INITIALLY ALL ELEMENTS (TEMPORARILY)*
C               **  ARE IN THE LOCAL UNION SET, AND INITIALLY ALL     **
C               **  ELEMENTS ARE IN THE GLOBAL INTERSECTION SET.      **
C               ********************************************************
C
      ISTEPN='2.1'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO200I=1,NIOLD
        ISUB(I)=11
  200 CONTINUE
C
C               *************************************************
C               **  STEP 2.2--                                 **
C               **  IF EXISTENT,                               **
C               **  PACK < = INTO <=                           **
C               **  PACK = < INTO =<                           **
C               **  PACK > = INTO >=                           **
C               **  PACK = > INTO =>                           **
C               **  THIS IS BECAUSE = SIGNS ARE AUTOMATICALLY  **
C               **  GIVEN A SPACE IN DPTYPE AND TREATED AS     **
C               **  AS A SEPARATE WORD.                        **
C               **  NOTE THAT NUMARG WILL BE CHANGED.          **
C               *************************************************
C
      ISTEPN='2.2'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL ADJUS2(IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               ************************************************
C               **  STEP 3.1--                                **
C               **  CHECK TO SEE IF HAVE THE  SUBSET  CASE.   **
C               **  CHECK TO SEE IF HAVE THE  EXCEPT  CASE.   **
C               **  LOCATE THE POSITION IN THE ARGUMENT LIST  **
C               **  OF THE WORD   SUBSET   OR   EXCEPT  .     **
C               ************************************************
C
      ISTEPN='3.1'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMAX=0
      ICASSC='SEAR'
      ICASQU='UNKN'
      NUMSV=0
      DO300IPASS=1,100
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
  301   FORMAT('***** AT THE BEGINNING OF ANOTHER PASS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)IPASS,ILOCTG
  302   FORMAT('IPASS,ILOCTG = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(ILOCTG.GE.1)THEN
          WRITE(ICOUT,303)ICASSC,ILOCTG,IHARG(ILOCTG),IHARG2(ILOCTG)
  303     FORMAT('ICASSC,ILOCTG,IHARG(ILOCTG),IHARG2(ILOCTG) = ',
     1           A4,I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,304)JMAX
  304   FORMAT('JMAX= ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASSC.EQ.'STOP')GOTO1100
      JMIN=JMAX+1
      IF(JMIN.GT.NUMARG)GOTO1100
      IF(JMIN.EQ.NUMARG.AND.IHARG(JMIN).EQ.'AND '.AND.
     1IHARG2(JMIN).EQ.'    ')GOTO1100
C
      IF(ICASSC.EQ.'CONT')GOTO600
      DO310I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=00
        IF(ITEMP.EQ.10)ISUB(I)=00
        IF(ITEMP.EQ.01)ISUB(I)=00
        IF(ITEMP.EQ.11)ISUB(I)=11
  310 CONTINUE
      ICASQU='UNKN'
      DO340J=JMIN,NUMARG
        J2=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')THEN
          ICASQU='SUBS'
          ILOCS=J2
CCCCC     THE FOLLOWING 6 LINES WERE INSERTED MARCH 1988.
          ILOCS2=ILOCS+2
          IHSET=IHARG(ILOCS2)
          IHSET2=IHARG2(ILOCS2)
          IF(IHSET.EQ.'<>  ')ICASQU='EXCE'
          IF(IHSET.EQ.'><  ')ICASQU='EXCE'
          IF(IHSET.EQ.'NOT=')ICASQU='EXCE'
          GOTO390
        ELSEIF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')THEN
          ICASQU='EXCE'
          ILOCS=J2
          GOTO390
        ENDIF
  340 CONTINUE
      ILOCS=NUMARG+1
      GOTO1100
C
  390 CONTINUE
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,391)IPASS,ICASQU,ILOCS
  391   FORMAT('IPASS,ICASQU,ILOCS = ',I8,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 3.2--                           **
C               **  IF HAVE THE SUBSET CASE,             **
C               **  INITIALIZE ISUB(.) TO 0X--00 OR 01.  **
C               **  IF HAVE THE EXCEPT CASE,             **
C               **  INITIALIZE ISUB(.) TO 1X--10 OR 11.  **
C               *******************************************
C
      ISTEPN='3.2'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASQU.EQ.'SUBS')THEN
        DO401I=1,NIOLD
          ITEMP=ISUB(I)
          IF(ITEMP.EQ.00)ISUB(I)=00
          IF(ITEMP.EQ.10)ISUB(I)=00
          IF(ITEMP.EQ.01)ISUB(I)=01
          IF(ITEMP.EQ.11)ISUB(I)=01
  401   CONTINUE
      ELSE
        DO406I=1,NIOLD
          ITEMP=ISUB(I)
          IF(ITEMP.EQ.00)ISUB(I)=10
          IF(ITEMP.EQ.10)ISUB(I)=10
          IF(ITEMP.EQ.01)ISUB(I)=11
          IF(ITEMP.EQ.11)ISUB(I)=11
  406   CONTINUE
      ENDIF
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  CHECK VALIDITY OF FIRST ARGUMENT AFTER     SUBSET **
C               **  OR    EXCEPT    .                                 **
C               **  THIS SHOULD BE THE SUBSET VARIABLE                **
C               **  OR THE DUMMY INDEX    I   .                       **
C               ********************************************************
C
      ISTEPN='4'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASVA='UNKN'
      ILOCS1=ILOCS+1
      JMAX=ILOCS1
      IF(ILOCS1.GT.NUMARG)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,412)
  412   FORMAT('      THE WORD    SUBSET    OR    EXCEPT    WAS THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,413)
  413   FORMAT('      FINAL WORD ON THE COMMAND LINE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,414)
  414   FORMAT('      THE WORD    SUBSET  OR   EXCEPT   SHOULD HAVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,415)
  415   FORMAT('      BEEN FOLLOWED BY OTHER ARGUMENTS, AS IN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,416)
  416   FORMAT('           SUBSET X = 4')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,417)
  417   FORMAT('           SUBSET X = 4 7 9 15 22')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,418)
  418   FORMAT('           SUBSET X = 4 TO 10')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,419)
  419   FORMAT('           SUBSET X >= 7')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,420)
  420   FORMAT('           AND SO FORTH.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,421)
  421   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
  422     FORMAT('      ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IHSET=IHARG(ILOCS1)
      IHSET2=IHARG2(ILOCS1)
C
      IF(IHSET.EQ.'I   '.AND.IHSET2.EQ.'    ')THEN
        ICASVA='I   '
        IF(NUMNAM.LE.0)GOTO490
        DO435I=1,NUMNAM
          IF(IHNAME(I).EQ.IHSET.AND.IHNAM2(I).EQ.IHSET2.AND.
     1       IUSE(I).EQ.'V   ')GOTO440
  435   CONTINUE
        GOTO490
      ENDIF
C
  440 CONTINUE
      ICASVA='V   '
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ISETV=IVALUE(ILOC)
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,451)ILOCS1,IHSET,IHSET2,ISETV
  451   FORMAT('ILOCS1,IHSET,IHSET2,ISETV = ',I8,3X,2A4,3X,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      GOTO490
C
  490 CONTINUE
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,491)IPASS,IHSET,IHSET2,ICASVA,ISETV
  491   FORMAT('IPASS,IHSET,IHSET2,ICASVA,ISETV = ',I8,3(2X,A4),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************************
C               **  STEP 5--                                         **
C               **  CHECK TO SEE IF NEXT ARGUMENT IS                 **
C               **        <                                          **
C               **        <=                                         **
C               **        =                                          **
C               **        >=                                         **
C               **        >                                          **
C               **        <>   ><   NOT=                             **
C               **  IF NONE OF THE ABOVE, THEN ASSUME  =   .         **
C               *******************************************************
C
      ISTEPN='5'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASOP='UNKN'
      ILOCS2=ILOCS+2
      JMAX=ILOCS2
      IF(ILOCS2.GT.NUMARG)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,502)
  502   FORMAT('      THE SUBSET/EXCEPT VARIABLE NAME WAS THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,503)
  503   FORMAT('      FINAL WORD ON THE COMMAND LINE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,504)
  504   FORMAT('      THE SUBSET/EXCEPT VARIABLE NAME SHOULD HAVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,505)
  505   FORMAT('      BEEN FOLLOWED BY OTHER ARGUMENTS, AS IN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,506)
  506   FORMAT('           SUBSET X = 4')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
  507   FORMAT('           SUBSET X = 4 7 9 15 22')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,508)
  508   FORMAT('           SUBSET X = 4 TO 10')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,509)
  509   FORMAT('           SUBSET X >= 7')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,510)
  510   FORMAT('           AND SO FORTH.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,521)
  521   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IHSET=IHARG(ILOCS2)
      IHSET2=IHARG2(ILOCS2)
C
      IF(IHSET.EQ.'<   ')THEN
        ICASOP='<   '
        ILOCTG=ILOCS2
      ELSEIF(IHSET.EQ.'<=  ' .OR. IHSET.EQ.'=<  ')THEN
        ICASOP='<=  '
        ILOCTG=ILOCS2
      ELSEIF(IHSET.EQ.'=   ')THEN
        ICASOP='=   '
        ILOCTG=ILOCS2
      ELSEIF(IHSET.EQ.'>=  ' .OR. IHSET.EQ.'=>  ')THEN
        ICASOP='>=  '
        ILOCTG=ILOCS2
      ELSEIF(IHSET.EQ.'>   ')THEN
        ICASOP='>   '
        ILOCTG=ILOCS2
      ELSEIF(IHSET.EQ.'<>  ' .OR. IHSET.EQ.'><  ' .OR.
     1       IHSET.EQ.'NOT=')THEN
        ICASOP='=   '
        ILOCTG=ILOCS2
      ELSE
        ICASOP='=ASS'
        ILOCTG=ILOCS2-1
        GOTO590
      ENDIF
C
  590 CONTINUE
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,591)IPASS,IHSET,IHSET2,ICASVA,ICASOP
  591   FORMAT('IPASS,IHSET,IHSET2,ICASVA,ICASOP = ',
     1         I8,4(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 6--                                          **
C               **  DETERMINE THE LOWER LIMIT OF THE INTERVAL OF      **
C               **  INTEREST.  THIS IS DONE BY CHECKING THE FIRST     **
C               **  (NEXT) ARGUMENT IN THE LIST.                      **
C               **  ALSO, FOR THOSE 4 CASES IN WHICH                  **
C               **  ICASOP IS   <   <=   >=   >                       **
C               **  DETERMINE THE UPPER LIMIT OF THE INTERVAL OF      **
C               **  INTEREST.                                         **
C               ********************************************************
C
  600 CONTINUE
C
      ISTEPN='6'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,601)
  601   FORMAT('     AT THE BEGINNING OF STEP 6 IN DPSUBS--')
        CALL DPWRST('XXX','BUG ')
        DO605I=1,NIOLD
          WRITE(ICOUT,606)I,ISUB(I)
  606     FORMAT('I,ISUB(I) = ',I8,I8)
          CALL DPWRST('XXX','BUG ')
  605   CONTINUE
      ENDIF
C
      ILOCTG=ILOCTG+1
      JMAX=ILOCTG
      IF(ILOCTG.GT.NUMARG)THEN
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,612)
  612   FORMAT('      THE SUBSET/EXCEPT OPERATION   <   <=  =  >=  >')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,613)
  613   FORMAT('      WAS THE FINAL WORD ON THE COMMAND LINE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,614)
  614   FORMAT('      THE SUBSET/EXCEPT VARIABLE NAME SHOULD HAVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,615)
  615   FORMAT('      BEEN FOLLOWED BY OTHER ARGUMENTS, AS IN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,506)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,508)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,509)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,510)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,521)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IARGT(ILOCTG).EQ.'NUMB')THEN
        DMIN=ARG(ILOCTG)
        DMAX=ARG(ILOCTG)
        IF(ICASOP.EQ.'<   ')THEN
          DMIN=CPUMIN
          DMAX=ARG(ILOCTG)
        ELSEIF(ICASOP.EQ.'<=  ')THEN
          DMIN=CPUMIN
          DMAX=ARG(ILOCTG)
        ELSEIF(ICASOP.EQ.'>=  ')THEN
          DMIN=ARG(ILOCTG)
          DMAX=CPUMAX
        ELSEIF(ICASOP.EQ.'>   ')THEN
          DMIN=ARG(ILOCTG)
          DMAX=CPUMAX
        ENDIF
      ELSEIF(IARGT(ILOCTG).EQ.'WORD')THEN
        IH=IHARG(ILOCTG)
        IH2=IHARG2(ILOCTG)
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        DMIN=VALUE(ILOC)
        DMAX=VALUE(ILOC)
        IF(ICASOP.EQ.'<   ')THEN
          DMIN=CPUMIN
          DMAX=VALUE(ILOC)
        ELSEIF(ICASOP.EQ.'<=  ')THEN
          DMIN=CPUMIN
          DMAX=VALUE(ILOC)
        ELSEIF(ICASOP.EQ.'>=  ')THEN
          DMIN=VALUE(ILOC)
          DMAX=CPUMAX
        ELSEIF(ICASOP.EQ.'>   ')THEN
          DMIN=VALUE(ILOC)
          DMAX=CPUMAX
        ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,632)
  632   FORMAT('      AN ARGUMENT TYPE WHICH SHOULD BE ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,633)
  633   FORMAT('      EITHER A NUMBER OR A WORD, IS NEITHER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,634)IHARG(ILOCTG),IHARG2(ILOCTG)
  634   FORMAT('      ARGUMENT                  = ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,635)ILOCTG
  635   FORMAT('      LOCATION IN ARGUMENT LIST = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,636)IARGT(ILOCTG)
  636   FORMAT('      ARGUMENT TYPE             = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,521)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,691)IPASS,ICASVA,ICASOP,IH,IH2,DMIN,DMAX
  691   FORMAT('IPASS,ICASVA,ICASOP,IH,IH2,DMIN,DMAX = ',
     1         I8,4(2X,A4),2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 7--                                          **
C               **  DETERMINE THE UPPER LIMIT OF THE INTERVAL OF      **
C               **  INTEREST.  NOTE THAT FOR THOSE 4 CASES IN WHICH   **
C               **  ICASOP IS   <   <=   >=   >                       **
C               **  THE UPPER LIMIT OF THE INTERVAL                   **
C               **  HAS ALREADY BEEN DETERMINED AND SO                **
C               **  ALL OF THE CODE OF THIS SECTION MAY BE SKIPPED.   **
C               **  ON THE OTHER HAND WHEN THE OPERATION IS    =   ,  **
C               **  (EXPLICITLY OR ASSUMED),                          **
C               **  THE UPPER LIMIT MUST BE DETERMINED.               **
C               **  THIS IS DONE BY CHECKING THE NEXT ARGUMENT        **
C               **  IN THE LIST.                                      **
C               **  IF THIS NEXT ARGUMENT IS    TO   ,   THIS         **
C               **  IMPLIES THAT AN UPPER LIMIT WILL BE PROVIDED      **
C               **  (IN THE ARGUMENT AFTER THE   TO   ).              **
C               **  HOWEVER, IF THE NEXT ARGUMENT IS NOT A    TO   ,  **
C               **  THEN THIS IMPLIES THAT THE LIST CONSISTS          **
C               **  OF INDIVIDUAL ELEMENTS OF THE SUBSET              **
C               **  AND SO THE UPPER LIMIT WILL BE IDENTICAL          **
C               **  TO THE LOWER LIMIT.                               **
C               ********************************************************
C
      ISTEPN='7'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASOP.EQ.'<   ' .OR. ICASOP.EQ.'<=  ' .OR.
     1   ICASOP.EQ.'>=  ' .OR. ICASOP.EQ.'>   ')THEN
        ICASSC='SEAR'
        GOTO790
      ENDIF
C
      ILOCTG=ILOCTG+1
C
      IF(ILOCTG.GT.NUMARG)GOTO710
      IF(ILOCTG.EQ.NUMARG.AND.IHARG(ILOCTG).EQ.'AND '.AND.
     1IHARG2(ILOCTG).EQ.'    ')GOTO710
      IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'SUBS'.AND.
     1IHARG2(ILOCTG).EQ.'ET  ')GOTO720
      IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'EXCE'.AND.
     1IHARG2(ILOCTG).EQ.'PT  ')GOTO720
      IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'TO  '.AND.
     1IHARG2(ILOCTG).EQ.'    ')GOTO750
      GOTO730
C
  710 CONTINUE
      ILOCTG=ILOCTG-1
      JMAX=ILOCTG
      ICASSC='STOP'
      DMAX=DMIN
      GOTO790
C
  720 CONTINUE
      ILOCTG=ILOCTG-1
      JMAX=ILOCTG
      ICASSC='SEAR'
      DMAX=DMIN
      GOTO790
C
  730 CONTINUE
      ILOCTG=ILOCTG-1
      JMAX=ILOCTG
      ICASSC='CONT'
      DMAX=DMIN
      GOTO790
C
  750 CONTINUE
      ILOCTG=ILOCTG+1
      JMAX=ILOCTG
      IF(ILOCTG.GT.NUMARG)GOTO760
      IF(ILOCTG.EQ.NUMARG.AND.IHARG(ILOCTG).EQ.'AND '.AND.
     1IHARG2(ILOCTG).EQ.'    ')GOTO760
      IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'SUBS'.AND.
     1IHARG2(ILOCTG).EQ.'ET  ')GOTO760
      IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'EXCE'.AND.
     1IHARG2(ILOCTG).EQ.'PT  ')GOTO760
      IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'TO  '.AND.
     1IHARG2(ILOCTG).EQ.'    ')GOTO760
      GOTO770
C
  760 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,762)
  762 FORMAT('      THE WORD    TO    SHOULD HAVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,763)
  763 FORMAT('      BEEN FOLLOWED BY A NUMBER OR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,764)
  764 FORMAT('      BY A PARAMETER NAME, BUT WAS NOT.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,765)IHARG(ILOCTG),IHARG2(ILOCTG)
  765 FORMAT('      TO    WAS FOLLOWED BY THE WORD   ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,521)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
  770 CONTINUE
      IF(IARGT(ILOCTG).EQ.'NUMB')THEN
        DMAX=ARG(ILOCTG)
      ELSEIF(IARGT(ILOCTG).EQ.'WORD')THEN
        IH=IHARG(ILOCTG)
        IH2=IHARG2(ILOCTG)
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        DMAX=VALUE(ILOC)
      ELSE
        IBRAN=770
        WRITE(ICOUT,771)IBRAN
  771   FORMAT('***** INTERNAL ERROR IN DPSUBS--',
     1         'IMPOSSIBLE BRANCH CONDITION AT BRANCH POINT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,772)ILOCTG,IARGT(ILOCTG)
  772   FORMAT('ILOCTG, IARGT(ILOCTG) = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      ILOCTG=ILOCTG+1
      ICASSC='CONT'
      IF(ILOCTG.GT.NUMARG)ICASSC='STOP'
      IF(ILOCTG.EQ.NUMARG.AND.IHARG(ILOCTG).EQ.'AND '.AND.
     1IHARG2(ILOCTG).EQ.'    ')ICASSC='STOP'
      IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'SUBS'.AND.
     1IHARG2(ILOCTG).EQ.'ET  ')ICASSC='SEAR'
      IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'EXCE'.AND.
     1IHARG2(ILOCTG).EQ.'PT  ')ICASSC='SEAR'
      ILOCTG=ILOCTG-1
      JMAX=ILOCTG
C
  790 CONTINUE
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,791)IPASS,ICASVA,ICASOP,IH,IH2,DMIN,DMAX
  791   FORMAT('IPASS,ICASVA,ICASOP,IH,IH2,DMIN,DMAX = ',
     1         I8,4(2X,A4),2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************************************
C               **  STEP 8--                                     **
C               **  TO ALLOW FOR ROUNDOFF ERRORS IN THE          **
C               **  STORAGE OF NUMBERS,                          **
C               **  JUDICIOUSLY EXPAND THE INTERVAL OF INTEREST  **
C               **  BY AN    EPSILON    AMOUNT.                  **
C               ***************************************************
C
      ISTEPN='8'
C
      IF(IBUGQ.EQ.'ON')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,801)
  801   FORMAT('      AT THE BEGINNING OF STEP 8--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,802)DMIN,DMAX
  802   FORMAT('DMIN,DMAX = ',2E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(DMIN.LE.DMAX)GOTO809
      HOLD=DMIN
      DMIN=DMAX
      DMAX=HOLD
  809 CONTINUE
C
      IF(DMIN.EQ.CPUMIN)GOTO819
      IF(DMIN.EQ.CPUMAX)GOTO819
      IF(ABS(DMIN).EQ.0.0)EPS=0.000001
      IF(ABS(DMIN).NE.0.0)EPS=ABS(DMIN*0.000001)
      IF(ICASOP.EQ.'=   ')DMIN=DMIN-EPS
      IF(ICASOP.EQ.'=ASS')DMIN=DMIN-EPS
      IF(ICASOP.EQ.'<   ')DMIN=DMIN-EPS
      IF(ICASOP.EQ.'<=  ')DMIN=DMIN-EPS
      IF(ICASOP.EQ.'>=  ')DMIN=DMIN-EPS
      IF(ICASOP.EQ.'>   ')DMIN=DMIN+EPS
  819 CONTINUE
C
      IF(DMAX.EQ.CPUMAX)GOTO829
      IF(DMAX.EQ.CPUMIN)GOTO829
      IF(ABS(DMAX).EQ.0.0)EPS=0.000001
      IF(ABS(DMAX).NE.0.0)EPS=ABS(DMAX*0.000001)
      IF(ICASOP.EQ.'=   ')DMAX=DMAX+EPS
      IF(ICASOP.EQ.'=ASS')DMAX=DMAX+EPS
      IF(ICASOP.EQ.'<   ')DMAX=DMAX-EPS
      IF(ICASOP.EQ.'<=  ')DMAX=DMAX+EPS
      IF(ICASOP.EQ.'>=  ')DMAX=DMAX+EPS
      IF(ICASOP.EQ.'>   ')DMAX=DMAX+EPS
  829 CONTINUE
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,891)IPASS,ICASVA,ICASOP,IH,IH2
  891   FORMAT('IPASS,ICASVA,ICASOP,IH,IH2 = ',I8,4(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,892)EPS,DMIN,DMAX,CPUMIN,CPUMAX
  892   FORMAT('EPS,DMIN,DMAX,CPUMIN,CPUMAX = ',5E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************************
C               **  STEP 9--                                      **
C               **  DEFINE THE ISUB(.) VECTOR--                   **
C               **  FOR ANY K (K = 1 TO NIOLD),                  **
C               **  IF THE K-TH ELEMENT OF THE                    **
C               **  SUBSET SPECIFICATION VARIABLE                 **
C               **  (THE VARIABLE SPECIFIED AFTER    SUBSET   **
C               **  IN THE COMMAND LINE)                          **
C               **  IS WITHIN THE SPECIFIED (DMIN,DMAX) LIMITS,   **
C               **  THEN ISUB(K) SHOULD RESULT IN A VALUE OF 1;   **
C               **  BUT IF THE K-TH ELEMENT OF THE                **
C               **  SUBSET SPECIFICATION VARIABLE                 **
C               **  IS OUTSIDE THE SPECIFIED (DMIN,DMAX) LIMITS,  **
C               **  THEN ISUB(K) SHOULD RESULT IN A 0 .           **
C               ****************************************************
C
      ISTEPN='9'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGQ.EQ.'ON')WRITE(ICOUT,901)ILOCS1,IHSET,IHSET2,ICASVA,ISETV,
     1MAXCOL
  901 FORMAT('ILOCS1,IHSET,IHSET2,ICASVA,ISETV,MAXCOL = ',
     1I8,2X,A4,2X,A4,2X,A4,I8,I8)
      IF(IBUGQ.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
      IF(ICASVA.EQ.'UNKN')GOTO910
      IF(ICASVA.EQ.'I   ')GOTO930
      IF(ISETV.LE.MAXCOL)GOTO940
      IF(ISETV.EQ.MAXCP1)GOTO950
CCCCC IF(ISETV.EQ.MAXCP2)GOTO960
      IF(ISETV.EQ.MAXCP2)GOTO950
      IF(ISETV.EQ.MAXCP3)GOTO950
      IF(ISETV.EQ.MAXCP4)GOTO950
      IF(ISETV.EQ.MAXCP5)GOTO950
      IF(ISETV.EQ.MAXCP6)GOTO950
C
  910 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,911)
  911 FORMAT('***** INTERNAL ERROR IN DPSUBS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,912)
  912 FORMAT('      IMPROPER VALUE FOR ICASVA AND/OR ISETV')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,913)ICASVA,ISETV,MAXCOL,MAXCP1,MAXCP2
  913 FORMAT('      ICASVA,ISETV,MAXCOL,MAXCP1,MAXCP2 = ',A4,4I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  930 CONTINUE
      NS=0
      ND=0
      DO931I=1,NIOLD
      TARGET=I
      IF(ICASQU.EQ.'SUBS'.AND.DMIN.LE.TARGET.AND.TARGET.LE.DMAX)
     1GOTO932
      IF(ICASQU.EQ.'SUBS')GOTO933
      IF(ICASQU.EQ.'EXCE'.AND.DMIN.LE.TARGET.AND.TARGET.LE.DMAX)
     1GOTO934
      IF(ICASQU.EQ.'EXCE')GOTO935
      GOTO931
  932 CONTINUE
      ITEMP=ISUB(I)
      IF(ITEMP.EQ.00)ISUB(I)=10
      IF(ITEMP.EQ.10)ISUB(I)=10
      IF(ITEMP.EQ.01)ISUB(I)=11
      IF(ITEMP.EQ.11)ISUB(I)=11
      NS=NS+1
      GOTO931
  933 CONTINUE
      ND=ND+1
      GOTO931
  934 CONTINUE
      ITEMP=ISUB(I)
      IF(ITEMP.EQ.00)ISUB(I)=00
      IF(ITEMP.EQ.10)ISUB(I)=00
      IF(ITEMP.EQ.01)ISUB(I)=01
      IF(ITEMP.EQ.11)ISUB(I)=01
      ND=ND+1
      GOTO931
  935 CONTINUE
      NS=NS+1
      GOTO931
  931 CONTINUE
      GOTO990
C
  940 CONTINUE
      NS=0
      ND=0
      DO941I=1,NIOLD
      IJ=MAXN*(ISETV-1)+I
      VIJ=V(IJ)
      IF(IBUGQ.EQ.'ON')WRITE(9,947)I,NIOLD,ISETV,DMIN,DMAX,VIJ
  947 FORMAT('I,NIOLD,ISETV,DMIN,DMAX,VIJ = ',
     13I8,3E12.4)
      TARGET=VIJ
      IF(ICASQU.EQ.'SUBS'.AND.DMIN.LE.TARGET.AND.TARGET.LE.DMAX)
     1GOTO942
      IF(ICASQU.EQ.'SUBS')GOTO943
      IF(ICASQU.EQ.'EXCE'.AND.DMIN.LE.TARGET.AND.TARGET.LE.DMAX)
     1GOTO944
      IF(ICASQU.EQ.'EXCE')GOTO945
      GOTO941
  942 CONTINUE
      ITEMP=ISUB(I)
      IF(ITEMP.EQ.00)ISUB(I)=10
      IF(ITEMP.EQ.10)ISUB(I)=10
      IF(ITEMP.EQ.01)ISUB(I)=11
      IF(ITEMP.EQ.11)ISUB(I)=11
      NS=NS+1
      GOTO941
  943 CONTINUE
      ND=ND+1
      GOTO941
  944 CONTINUE
      ITEMP=ISUB(I)
      IF(ITEMP.EQ.00)ISUB(I)=00
      IF(ITEMP.EQ.10)ISUB(I)=00
      IF(ITEMP.EQ.01)ISUB(I)=01
      IF(ITEMP.EQ.11)ISUB(I)=01
      ND=ND+1
      GOTO941
  945 CONTINUE
      NS=NS+1
      GOTO941
  941 CONTINUE
      GOTO990
C
  950 CONTINUE
      NS=0
      ND=0
      DO951I=1,NIOLD
CCCCC TARGET=PRED(I)
      IF(ISETV.EQ.MAXCP1)TARGET=PRED(I)
      IF(ISETV.EQ.MAXCP2)TARGET=RES(I)
      IF(ISETV.EQ.MAXCP3)TARGET=YPLOT(I)
      IF(ISETV.EQ.MAXCP4)TARGET=XPLOT(I)
      IF(ISETV.EQ.MAXCP5)TARGET=X2PLOT(I)
      IF(ISETV.EQ.MAXCP6)TARGET=TAGPLO(I)
      IF(ICASQU.EQ.'SUBS'.AND.DMIN.LE.TARGET.AND.TARGET.LE.DMAX)
     1GOTO952
      IF(ICASQU.EQ.'SUBS')GOTO953
      IF(ICASQU.EQ.'EXCE'.AND.DMIN.LE.TARGET.AND.TARGET.LE.DMAX)
     1GOTO954
      IF(ICASQU.EQ.'EXCE')GOTO955
      GOTO951
  952 CONTINUE
      ITEMP=ISUB(I)
      IF(ITEMP.EQ.00)ISUB(I)=10
      IF(ITEMP.EQ.10)ISUB(I)=10
      IF(ITEMP.EQ.01)ISUB(I)=11
      IF(ITEMP.EQ.11)ISUB(I)=11
      NS=NS+1
      GOTO951
  953 CONTINUE
      ND=ND+1
      GOTO951
  954 CONTINUE
      ITEMP=ISUB(I)
      IF(ITEMP.EQ.00)ISUB(I)=00
      IF(ITEMP.EQ.10)ISUB(I)=00
      IF(ITEMP.EQ.01)ISUB(I)=01
      IF(ITEMP.EQ.11)ISUB(I)=01
      ND=ND+1
      GOTO951
  955 CONTINUE
      NS=NS+1
      GOTO951
  951 CONTINUE
      GOTO990
C
CC960 CONTINUE
CCCCC NS=0
CCCCC ND=0
CCCCC DO961I=1,NIOLD
CCCCC TARGET=RES(I)
CCCCC IF(ICASQU.EQ.'SUBS'.AND.DMIN.LE.TARGET.AND.TARGET.LE.DMAX)
CCCCC1GOTO962
CCCCC IF(ICASQU.EQ.'SUBS')GOTO963
CCCCC IF(ICASQU.EQ.'EXCE'.AND.DMIN.LE.TARGET.AND.TARGET.LE.DMAX)
CCCCC1GOTO964
CCCCC IF(ICASQU.EQ.'EXCE')GOTO965
CCCCC GOTO961
CC962 CONTINUE
CCCCC ITEMP=ISUB(I)
CCCCC IF(ITEMP.EQ.00)ISUB(I)=10
CCCCC IF(ITEMP.EQ.10)ISUB(I)=10
CCCCC IF(ITEMP.EQ.01)ISUB(I)=11
CCCCC IF(ITEMP.EQ.11)ISUB(I)=11
CCCCC NS=NS+1
CCCCC GOTO961
CC963 CONTINUE
CCCCC ND=ND+1
CCCCC GOTO961
CC964 CONTINUE
CCCCC ITEMP=ISUB(I)
CCCCC IF(ITEMP.EQ.00)ISUB(I)=00
CCCCC IF(ITEMP.EQ.10)ISUB(I)=00
CCCCC IF(ITEMP.EQ.01)ISUB(I)=01
CCCCC IF(ITEMP.EQ.11)ISUB(I)=01
CCCCC ND=ND+1
CCCCC GOTO961
CC965 CONTINUE
CCCCC NS=NS+1
CCCCC GOTO961
CC961 CONTINUE
CCCCC GOTO990
C
  990 CONTINUE
      IF(IBUGQ.EQ.'ON')WRITE(ICOUT,991)IPASS,ICASQU,DMIN,DMAX,EPS,
     1NIOLD,NS,ND
  991 FORMAT('IPASS,ICASQU,DMIN,DMAX,EPS,NIOLD,NS,ND = ',
     1I8,2X,A4,3E15.7,3I8)
      IF(IBUGQ.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IF(IBUGQ.EQ.'OFF')GOTO994
      DO992I=1,NIOLD
      WRITE(ICOUT,993)I,ISUB(I)
  993 FORMAT('I,ISUB(I) = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
  992 CONTINUE
  994 CONTINUE
C
C               *************************************************
C               **  STEP 10--                                  **
C               **  WRITE OUT A MESSAGE FOR THIS STEP          **
C               **  INDICATING                                 **
C               **  THE SUBSET VARIABLE NAME,                  **
C               **  THE SUBSET MINIMUM,                        **
C               **  THE SUBSET MAXIMUM,                        **
C               **  THE INPUT NUMBER OF OBSERVATIONS (LOCAL),  **
C               **  THE NUMBER OF OBSERVATIONS IGNORED         **
C               **  AND THE OUTPUT NUMBER OF OBSERVATIONS      **
C               **  (THAT IS, THE SUBSET SAMPLE SIZE).         **
C               **  ALSO, CHECK THAT NS IS POSITIVE.           **
C               *************************************************
C
      ISTEPN='10'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASQU.EQ.'EXCE')GOTO1020
      GOTO1010
C
 1010 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1019
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1011)
 1011 FORMAT('***** NOTE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1012)IHARG(ILOCS1),IHARG2(ILOCS1)
 1012 FORMAT('      SUBSET VARIABLE = ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1013)DMIN
 1013 FORMAT('      SUBSET MINIMUM  = ',E17.10)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1014)DMAX
 1014 FORMAT('      SUBSET MAXIMUM  = ',E17.10)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1015)NIOLD
 1015 FORMAT('      INPUT  NUMBER OF OBSERVATIONS  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1016)ND
 1016 FORMAT('      NUMBER OF OBSERVATIONS IGNORED = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1017)NS
 1017 FORMAT('      OUTPUT NUMBER OF OBSERVATIONS  = ',I8)
      CALL DPWRST('XXX','BUG ')
 1019 CONTINUE
      GOTO1050
C
 1020 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1029
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1021)
 1021 FORMAT('***** NOTE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1022)IHARG(ILOCS1),IHARG2(ILOCS1)
 1022 FORMAT('      EXCEPTED SUBSET VARIABLE = ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1023)DMIN
 1023 FORMAT('      EXCEPTED SUBSET MINIMUM  = ',E17.10)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1024)DMAX
 1024 FORMAT('      EXCEPTED SUBSET MAXIMUM  = ',E17.10)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1025)NIOLD
 1025 FORMAT('      INPUT  NUMBER OF OBSERVATIONS  = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1026)ND
 1026 FORMAT('      NUMBER OF OBSERVATIONS IGNORED = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1027)NS
 1027 FORMAT('      OUTPUT NUMBER OF OBSERVATIONS  = ',I8)
      CALL DPWRST('XXX','BUG ')
 1029 CONTINUE
      GOTO1050
C
 1050 CONTINUE
CCCCC IF(NS.GE.1)GOTO1059
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1051)
C1051 FORMAT('***** ERROR IN DPSUBS--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1052)
C1052 FORMAT('      THE SUBSET IS EMPTY--IT HAS NO ELEMENTS IN IT.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C1059 CONTINUE
C
      NUMSV=IPASS
C
  300 CONTINUE
C
 1100 CONTINUE
      DO1110I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=00
        IF(ITEMP.EQ.10)ISUB(I)=00
        IF(ITEMP.EQ.01)ISUB(I)=00
        IF(ITEMP.EQ.11)ISUB(I)=11
 1110 CONTINUE
C
C               *************************************
C               **  STEP 11--                      **
C               **  PUT ISUB(.) IN FINAL 0,1 FORM  **
C               *************************************
C
      ISTEPN='11'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1210I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=0
        IF(ITEMP.EQ.10)ISUB(I)=0
        IF(ITEMP.EQ.01)ISUB(I)=1
        IF(ITEMP.EQ.11)ISUB(I)=1
 1210 CONTINUE
C
C               *****************************************
C               **  STEP 12--                          **
C               **  IF THERE WERE 2 OR MORE SUBSET     **
C               **  VARIABLES, GATHER INFORMATION      **
C               **  FOR A FINAL SUMMARY MESSAGE BY     **
C               **  DETERMINING THE FINAL NUMBER OF    **
C               **  ELEMENTS IN THE SUBSET             **
C               **  (AFTER ALL VARIABLES HAVE          **
C               **  BEEN INDIVIDUALLY ACCOUNTED FOR).  **
C               *****************************************
C
      ISTEPN='12'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMSV.GT.1)THEN
        NS=0
        DO1510I=1,NIOLD
          IF(ISUB(I).EQ.1)NS=NS+1
 1510   CONTINUE
      ENDIF
C
C               *************************************************
C               **  STEP 13--                                  **
C               **  IF THERE WERE 2 OR MORE SUBSET VARIABLES,  **
C               **  WRITE OUT A FINAL MESSAGE                  **
C               **  SUMMARIZING FOR ALL VARIABLES              **
C               **  THE NUMBER OF SUBSET VARIABLES             **
C               **  THE INPUT NUMBER OF OBSERVATIONS (LOCAL),  **
C               **  THE NUMBER OF OBSERVATIONS IGNORED         **
C               **  AND THE OUTPUT NUMBER OF OBSERVATIONS      **
C               **  (THAT IS, THE SUBSET SAMPLE SIZE).         **
C               **  ALSO, CHECK THAT NS IS POSITIVE.           **
C               *************************************************
C
      ISTEPN='13'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMSV.LE.1)GOTO1690
      ND=NIOLD-NS
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1601)
 1601   FORMAT('***** SUBSET/EXCEPT SUMMARY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1602)NUMSV
 1602   FORMAT('      NUMBER OF SPECIFICATIONS       = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1605)NIOLD
 1605   FORMAT('      INPUT  NUMBER OF OBSERVATIONS  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1606)ND
 1606   FORMAT('      NUMBER OF OBSERVATIONS IGNORED = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1607)NS
 1607   FORMAT('      OUTPUT NUMBER OF OBSERVATIONS  = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NS.GE.1)GOTO1690
C
C     AUGUST, 1987: FOR EMPTY SUBSETS, DO NO PRINT ERROR MESSAGE
C                   UNLESS FEEDBACK SWITCH IS ON
C
C     SEPTEMBER 2018: DO NOT TREAT AN EMPTY SUBSET AS AN ERROR
C
CCCCC IF(IFEEDB.EQ.'ON')THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1611)
C1611   FORMAT('***** ERROR IN DPSUBS--')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1612)
C1612   FORMAT('      THE SUBSET IS EMPTY--IT HAS NO ELEMENTS IN IT.')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
CCCCC ENDIF
C
 1690 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSUBS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,NUMSV,ND
 9014   FORMAT('IERROR,NUMSV,ND = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)ICASQU,ICASVA,ICASOP,ICASSC
 9018   FORMAT('ICASQU,ICASVA,ICASOP,ICASSC = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,NIOLD
          WRITE(ICOUT,9021)I,ISUB(I)
 9021     FORMAT('I,ISUB(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSUM2(Y,W,N,XTEMP1,XTEMP2,XTEMP3,
     1                  DTEMP1,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE GENERATES A SUMMARY
C              OF THE DATA IN THE INPUT VECTOR Y.
C     NOTE--ASSUMPTION--MODEL IS   RESPONSE = CONSTANT + ERROR.
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JULY      1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   2002.  SUPPORT FOR HTML OUTPUT
C                                        (ADD ICAPSW, ICAPTY TO CALL
C                                        LIST)
C     UPDATED         --OCTOBER   2003.  SUPPORT FOR LATEX OUTPUT
C     UPDATED         --MAY       2011.  SUPPORT FOR REPLICATION AND
C                                        MULTIPLE RESPONSE
C     UPDATED         --MAY       2011.  USE DPDTA1 AND DPDT5B TO PRINT
C                                        THE TABLES
C     UPDATED         --JUNE      2016.  CALL LIST TO NORPPC
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASAN
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IWRITE
      CHARACTER*20 IDIST
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IGEPDF
      CHARACTER*4 ICASE
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION W(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION PID(*)
C
      DOUBLE PRECISION DTEMP1(*)
C
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=1)
      PARAMETER (MAXROW=10)
      PARAMETER (MAXRO2=10)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXRO2)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*21 IVALUE(MAXROW,NUMCLI)
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
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSU'
      ISUBN2='M2  '
      IERROR='NO'
      IWRITE='OFF'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SUM2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSUM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,N,MAXNXT = ',3(A4,2X),2I8)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN SUMMARY--')
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
      GOTO9000
  139 CONTINUE
C
C               **********************************************
C               **  STEP 3--                                **
C               **  COMPUTE VARIOUS MEASURES OF LOCATION--  **
C               **     1) MIDRANGE                          **
C               **     2) MEAN                              **
C               **     3) MIDMEAN                           **
C               **     4) MEDIAN                            **
C               **********************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL MIDRAN(Y,N,IWRITE,YMIDR,IBUGA3,IERROR)
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL MIDMEA(Y,N,IWRITE,XTEMP1,MAXNXT,YMIDM,IBUGA3,IERROR)
      CALL MEDIAN(Y,N,IWRITE,XTEMP1,MAXNXT,YMED,IBUGA3,IERROR)
C
C               **********************************************
C               **  STEP 4--                                **
C               **  COMPUTE VARIOUS MEASURES OF DISPERSION  **
C               **     1) RANGE                             **
C               **     2) STANDARD DEVIATION                **
C               **     3) AVERAGE ABSOLUTE DEVIATION        **
C               **     4) MINIMUM                           **
C               **     5) LOWER QUARTILE                    **
C               **     6) LOWER HINGE                       **
C               **     7) UPPER HINGE                       **
C               **     8) UPPER QUARTILE                    **
C               **     9) MAXIMUM                           **
C               **********************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL RANGDP(Y,N,IWRITE,YRANGE,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      ICASE='MEAN'
      CALL AAD(Y,N,IWRITE,XTEMP1,MAXNXT,YAAD,ICASE,IBUGA3,IERROR)
      CALL MINIM(Y,N,IWRITE,YMIN,IBUGA3,IERROR)
      CALL LOWQUA(Y,N,IWRITE,XTEMP1,MAXNXT,YLOWQ,IBUGA3,IERROR)
      CALL LOWHIN(Y,N,IWRITE,XTEMP1,MAXNXT,YLOWH,IBUGA3,IERROR)
      CALL UPPHIN(Y,N,IWRITE,XTEMP1,MAXNXT,YUPPH,IBUGA3,IERROR)
      CALL UPPQUA(Y,N,IWRITE,XTEMP1,MAXNXT,YUPPQ,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,YMAX,IBUGA3,IERROR)
C
C               ********************************************************
C               **  STEP 5--                                          **
C               **  COMPUTE VARIOUS DISTRIBUTIONAL MEASURES--         **
C               **     1) STANDARDIZED THIRD CENTRAL MOMENT           **
C               **     2) STANDARDIZED FOURTH CENTRAL MOMENT          **
C               **     3) STANDARDIZED WILK-SHAPIRO STATISTIC         **
C               **     4) UNIFORM PROBABILITY PLOT CORRELATION COEFF  **
C               **     5) NORMAL  PROBABILITY PLOT CORRELATION COEFF  **
C               **     6) TUKEY LAMBDA = -0.5 PROBABILITY PLOT        **
C               **                            CORRELATION COEFF       **
C               **     7) CAUCHY  PROBABILITY PLOT CORRELATION COEFF  **
C               *********************************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL STMOM3(Y,N,IWRITE,YST3MO,IBUGA3,IERROR)
      CALL STMOM4(Y,N,IWRITE,YST4MO,IBUGA3,IERROR)
      CALL STWS(Y,N,XTEMP1,IWRITE,YSTWS,MAXNXT,IBUGA3,IERROR)
C
      ALAMB=0.0
      ALAMB2=0.0
      MINMAX=1
      IGEPDF='NULL'
      IDIST='UNIFORM'
      CALL NORPPC(Y,N,IDIST,ALAMB,ALAMB2,
     1            IWRITE,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,
     1            MINMAX,IGEPDF,
     1            YUNIPP,SHAPE,SHAPE2,ALOC,SCALE,
     1            IBUGA3,ISUBRO,IERROR)
      IDIST='NORMAL'
      CALL NORPPC(Y,N,IDIST,ALAMB,ALAMB2,
     1            IWRITE,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,
     1            MINMAX,IGEPDF,
     1            YNORPP,SHAPE,SHAPE2,ALOC,SCALE,IBUGA3,ISUBRO,IERROR)
      IDIST='CAUCHY'
      CALL NORPPC(Y,N,IDIST,ALAMB,ALAMB2,
     1            IWRITE,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,
     1            MINMAX,IGEPDF,
     1            YCAUPP,SHAPE,SHAPE2,ALOC,SCALE,
     1            IBUGA3,ISUBRO,IERROR)
      ALAMB=-0.5
      IDIST='TUKEY-LAMBDA'
      CALL NORPPC(Y,N,IDIST,ALAMB,ALAMB2,
     1            IWRITE,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,
     1            MINMAX,IGEPDF,
     1            YLAMPP,SHAPE,SHAPE2,ALOC,SCALE,
     1            IBUGA3,ISUBRO,IERROR)
C
C               *******************************************************
C               **  STEP 6--                                         **
C               **  COMPUTE VARIOUS RANDOMNESS MEASURES              **
C               **     1) AUTOCORRELATION COEFFICIENT                **
C               **     2) STANDARDIZED LENGTH OF LONGEST RUN (UP OR  **
C               **        DOWN)                                      **
C               **     3) STANDARDIZED NUMBER OF RUNS (UP + DOWN)    **
C               ********************************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL AUTOCR(Y,N,IWRITE,YAUTOC,IBUGA3,IERROR)
CCCCC CALL STLLRU(Y,N,IWRITE,YSTLLR,IBUGA3,IERROR)
      YSTLLR=0.0
CCCCC CALL STNRUN(Y,N,IWRITE,YSTNRU,IBUGA3,IERROR)
      YSTNRU=0.0
C
C               ****************************
C               **  STEP 7--              **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')
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
      ITITLE='Summary of xxxxxxxxxx Observations'
      WRITE(ITITLE(12:21),'(I10)')N
      NCTITL=34
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=1
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
      NUMROW=ICNT
      DO2310I=1,NUMROW
        NTOT(I)=15
 2310 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE=' '
      NCTITL=-99
      ITITL9=' '
      NCTIT9=0
C
      NUMCOL=5
      NUMLIN=1
C
      ITITL2(1,1)='Location Measures'
      NCTIT2(1,1)=17
      NCOLSP(1,1)=2
      ITITL2(1,2)=' '
      NCTIT2(1,2)=0
      NCOLSP(1,2)=0
      ITITL2(1,3)=' | '
      NCTIT2(1,3)=3
      NCOLSP(1,3)=1
      ITITL2(1,4)='Dispersion Measures'
      NCTIT2(1,4)=19
      NCOLSP(1,4)=2
      ITITL2(1,5)=' '
      NCTIT2(1,5)=0
      NCOLSP(1,5)=0
C
      NMAX=0
      DO4210I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=21
        IF(I.EQ.4)NTOT(I)=20
        IF(I.EQ.3)NTOT(I)=3
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.1 .OR. I.EQ.3 .OR. I.EQ.4)ITYPCO(I)='ALPH'
        DO4213J=1,MAXROW
          IDIGI2(J,I)=NUMDIG
          IF(I.EQ.1 .OR. I.EQ.3 .OR. I.EQ.4)THEN
            IDIGI2(J,I)=-1
          ENDIF
 4213   CONTINUE
 4210 CONTINUE
C
      DO4289J=1,MAXROW
        IVALUE(J,1)=' '
        IVALUE(J,2)=' '
        IVALUE(J,3)=' '
        IVALUE(J,4)=' '
        IVALUE(J,5)=' '
        NCVALU(J,1)=0
        NCVALU(J,2)=0
        NCVALU(J,3)=0
        NCVALU(J,4)=0
        NCVALU(J,5)=0
        AMAT(J,1)=0.0
        AMAT(J,2)=0.0
        AMAT(J,3)=0.0
        AMAT(J,4)=0.0
        AMAT(J,5)=0.0
        ROWSEP(J)=0
 4289 CONTINUE
      AMAT(1,2)=YMIDR
      AMAT(1,5)=YRANGE
      AMAT(2,2)=YMEAN
      AMAT(2,5)=YSD
      AMAT(3,2)=YMIDM
      AMAT(3,5)=YAAD
      AMAT(4,2)=YMED
      AMAT(4,5)=YMIN
      AMAT(5,2)=0.0
      IDIGI2(5,2)=-1
      AMAT(5,5)=YLOWQ
      AMAT(6,2)=0.0
      IDIGI2(6,2)=-1
      AMAT(6,5)=YLOWH
      AMAT(7,2)=0.0
      IDIGI2(7,2)=-1
      AMAT(7,5)=YUPPH
      AMAT(8,2)=0.0
      IDIGI2(8,2)=-1
      AMAT(8,5)=YUPPQ
      AMAT(9,2)=0.0
      IDIGI2(9,2)=-1
      AMAT(9,5)=YMAX
CCCCC ROWSEP(9)=1
C
      IVALUE(1,1)='Midrange:'
      NCVALU(1,1)=9
      IVALUE(2,1)='Mean:'
      NCVALU(2,1)=5
      IVALUE(3,1)='Midmean:'
      NCVALU(3,1)=8
      IVALUE(4,1)='Median:'
      NCVALU(4,1)=7
C
      DO4330I=1,9
        IVALUE(I,3)=' | '
        NCVALU(I,3)=3
 4330 CONTINUE
C
      IVALUE(1,4)='Range:'
      NCVALU(1,4)=6
      IVALUE(2,4)='Standard Deviation:'
      NCVALU(2,4)=19
      IVALUE(3,4)='Average Abs. Dev.:'
      NCVALU(3,4)=18
      IVALUE(4,4)='Minimum:'
      NCVALU(4,4)=8
      IVALUE(5,4)='Lower Quartile:'
      NCVALU(5,4)=15
      IVALUE(6,4)='Lower Hinge:'
      NCVALU(6,4)=12
      IVALUE(7,4)='Upper Hinge:'
      NCVALU(7,4)=12
      IVALUE(8,4)='Upper Quartile:'
      NCVALU(8,4)=15
      IVALUE(9,4)='Maximum:'
      NCVALU(9,4)=8
C
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=25
      IWHTML(4)=150
      IWHTML(5)=150
      IINC=1800
      IINC2=200
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC2
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
C
      ICNT=9
      IFRST=.TRUE.
      ILAST=.FALSE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDT5B(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            NCOLSP,ROWSEP,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITL2(1,1)='Randomness Measures'
      NCTIT2(1,1)=19
      ITITL2(1,4)='Distributional Measures'
      NCTIT2(1,4)=23
C
      DO4389J=1,MAXROW
        IVALUE(J,1)=' '
        IVALUE(J,2)=' '
        IVALUE(J,3)=' '
        IVALUE(J,4)=' '
        IVALUE(J,5)=' '
        NCVALU(J,1)=0
        NCVALU(J,2)=0
        NCVALU(J,3)=0
        NCVALU(J,4)=0
        NCVALU(J,5)=0
        AMAT(J,1)=0.0
        AMAT(J,2)=0.0
        AMAT(J,3)=0.0
        AMAT(J,4)=0.0
        AMAT(J,5)=0.0
        ROWSEP(J)=0
 4389 CONTINUE
      AMAT(1,2)=YAUTOC
      AMAT(1,5)=YST3MO
      AMAT(2,2)=0.0
      IDIGI2(2,2)=-1
      AMAT(2,5)=YST4MO
      AMAT(3,2)=0.0
      IDIGI2(3,2)=-1
      AMAT(3,5)=YSTWS
      AMAT(4,2)=0.0
      IDIGI2(4,2)=-1
      AMAT(4,5)=YUNIPP
      AMAT(5,2)=0.0
      IDIGI2(5,2)=-1
      AMAT(5,5)=YNORPP
      AMAT(6,2)=0.0
      IDIGI2(6,2)=-1
      AMAT(6,5)=YLAMPP
      AMAT(7,2)=0.0
      IDIGI2(7,2)=-1
      AMAT(7,5)=YCAUPP
      ROWSEP(7)=1
C
      IVALUE(1,1)='Autocorrelation Coef:'
      NCVALU(1,1)=21
C
      DO4350I=1,9
        IVALUE(I,3)=' | '
        NCVALU(I,3)=3
 4350 CONTINUE
C
      IVALUE(1,4)='St. Third Moment:'
      NCVALU(1,4)=17
      IVALUE(2,4)='St. Fourth Moment:'
      NCVALU(2,4)=18
      IVALUE(3,4)='St. Wilk-Shapiro:'
      NCVALU(3,4)=17
      IVALUE(4,4)='Uniform PPCC:'
      NCVALU(4,4)=13
      IVALUE(5,4)='Normal PPCC:'
      NCVALU(5,4)=12
      IVALUE(6,4)='Tukey-Lam -.5 PPCC:'
      NCVALU(6,4)=19
      IVALUE(7,4)='Cauchy PPCC:'
      NCVALU(7,4)=12
C
      ICNT=7
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDT5B(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            NCOLSP,ROWSEP,
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SUM2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSUM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSUMM(XTEMP1,XTEMP2,MAXNXT,
     1                  ICASAN,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A BATTERY OF SUMMARY STATISTICS.
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
C     ORIGINAL VERSION--JULY      1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --OCTOBER   2002. SUPPORT FOR HTML OUTPUT
C                                       (ADD ICAPSW TO CALL LIST)
C     UPDATED         --MAY       2011. USE DPPARS
C     UPDATED         --MAY       2011. SUPPORT FOR "MULTIPLE" AND
C                                       "REPLICATION" OPTIONS
C     UPDATED         --JUNE      2016. CALL LIST TO DPSUM2
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
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
      DIMENSION XTEMP2(*)
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
      DIMENSION XTEMP3(MAXOBV)
C
      DOUBLE PRECISION DTEMP1(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
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
      EQUIVALENCE (GARBAG(IGAR10),XTEMP3(1))
      EQUIVALENCE (GARBAG(JGAR11),XDESGN(1,1))
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
      ICASAN='SUMM'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPSU'
      ISUBN2='MM  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***********************************************
C               **  TREAT THE SUMMARY                CASE    **
C               ***********************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SUMM')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSUMM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE COMMAND                            **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:        **
C               **    1) SUMMARY             Y                     **
C               **    2) MULTIPLE SUMMARY    Y1 ... YK             **
C               **    3) REPLICATED SUMMARY  Y X1 ... XK           **
C               *****************************************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTC=9999
      ILASTZ=9999
      ICASAN='SUMM'
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
        ELSEIF(ICTMP1.EQ.'SUMM')THEN
          IFOUND='YES'
          ICASAN='SUMM'
          ILASTC=I
          ILASTZ=I
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'TOLE' .AND. ICTMP2.EQ.'LIMI')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'TOLE' .AND. ICTMP2.EQ.'INTE')THEN
          IFOUND='NO'
          GOTO9000
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')THEN
        WRITE(ICOUT,91)ICASAN,IMULT,IREPL,ISHIFT
   91   FORMAT('DPSUMM: ICASAN,IMULT,IREPL,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN SUMMARY--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE SUMMARY COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='SUMMARY'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 6--                                    **
C               **  GENERATE THE SUMMARY FOR VARIOUS CASES      **
C               ***************************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 1: NO REPLICATION VARIABLES    **
C               ******************************************
C
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')THEN
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SUMM')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPSUMM--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,NS1
  823       FORMAT('ICASAN,NUMVAR,NS1 = ',A4,2I8)
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
          CALL DPSUM2(Y,W,NS1,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,
     1                ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                PID,IVARID,IVARI2,NREPL,
     1                ISUBRO,IBUGA3,IERROR)
C
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')
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
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SUMM')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPSUMM--')
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
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,
     1                    DTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
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
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,
     1                    DTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
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
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,
     1                    DTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
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
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,
     1                    DTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
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
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,
     1                    DTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
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
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,
     1                    DTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SUMM')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSUMM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASAN
 9012   FORMAT('IFOUND,IERROR,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSWAP(IOP3,NUMNAM,IHNAME,IHNAM2,IUSE,IN,
     1IVALUE,MAXN2,MAXCO2,MAXIJ2,IBUGS2,ISUBRO,IERROR)
CCCCC SUBROUTINE DPSWAP(IOP3,V,NUMNAM,IHNAME,IHNAM2,IUSE,IN,
CCCCC1IVALUE,MAXN,MAXCOL,MAXN2,MAXCO2,MAXIJ2,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--SWAP (WRITE OUT OR READ IN) THE VECTOR V(.)
C              FROM MASS STORAGE.
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
C     ORIGINAL VERSION--MARCH     1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1986.
C     UPDATED         --OCTOBER   1991.  SUN HAS LIMIT ON NUMBER OF WORDS
C                                        THAT CAN BE WRITTEN (ALAN)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOP3
      CHARACTER*4 IHNAME(*)
      CHARACTER*4 IHNAM2(*)
      CHARACTER*4 IUSE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
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
CCCCC CHARACTER*4 IFOUND
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      INCLUDE 'DPCODA.INC'
CCCCC DIMENSION V(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
C  FOLLOWING LINE ADDED OCTOBER 1991.
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSW'
      ISUBN2='AP  '
      ISUBN0='SWAP'
      IERROR='NO'
C
      IWIDTH=(-999)
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWAP')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSWAP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGS2,IOP3
   53 FORMAT('IBUGS2,IOP3 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)V(1),V(2),V(3)
   54 FORMAT('V(1),V(2),V(3) = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)NUMNAM,MAXN,MAXCOL
   55 FORMAT('NUMNAM,MAXN,MAXCOL = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)ISCRNU
   71 FORMAT('ISCRNU = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,72)ISCRNA(1:80)
   72 FORMAT('ISCRNA = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,73)ISCRST
   73 FORMAT('ISCRST = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,74)ISCRFO
   74 FORMAT('ISCRFO = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,75)ISCRAC
   75 FORMAT('ISCRAC = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,76)ISCRFO
   76 FORMAT('ISCRFO = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,77)ISCRCS
   77 FORMAT('ISCRCS = ',A12)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=ISCRNU
      IFILE=ISCRNA
      ISTAT=ISCRST
      IFORM=ISCRFO
      IACCES=ISCRAC
      IPROT=ISCRPR
      ICURST=ISCRCS
C
      ISUBN0='SWAP'
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWAP')GOTO1199
      WRITE(ICOUT,1193)IOUNIT
 1193 FORMAT('IOUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1194)IFILE(1:80)
 1194 FORMAT('IFILE = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195 FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',
     1A12,2X,A12,2X,A12,2X,A12,2X,A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1196)ISUBN0,IERRFI
 1196 FORMAT('ISUBN0,IERRFI = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1199 CONTINUE
C
C               **********************************************
C               **  STEP 12--                               **
C               **  CHECK TO SEE IF SCRATCH FILE MAY EXIST  **
C               **********************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')GOTO1200
      GOTO1290
 1200 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** IMPLEMENTATION ERROR IN DPSWAP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      THE DESIRED FIT REQUIRES THE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1213)
 1213 FORMAT('      BEHIND-THE-SCENES USE OF A SCRATCH FILE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)
 1214 FORMAT('      BUT THE USE OF SUCH A SCRATCH FILE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('      CANNOT BE DONE BECAUSE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1216)
 1216 FORMAT('      THE INTERNAL VARIABLE    ISCRST ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1217)
 1217 FORMAT('      WHICH ALLOWS SUCH SCRATCH FILE USE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1218)
 1218 FORMAT('      HAS BEEN SET TO    NONE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1219)ISTAT,ISCRST
 1219 FORMAT('ISTAT,ISCRST = ',A12,2X,A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1220)
 1220 FORMAT('      PLEASE CONTACT THE DATAPLOT IMPLEMENTOR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1221)
 1221 FORMAT('      AND HAVE THE ISCRST SETTING CHANGED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1222)
 1222 FORMAT('      (FROM   NONE   TO   UNKNOWN)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1223)
 1223 FORMAT('      IN SUBROUTINE INITFO.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1290 CONTINUE
C
C               *****************************************
C               **  STEP 20--                          **
C               **  BRANCH TO THE APPROPRIATE CASE--   **
C               **    1) WRITE OUT TO   MASS STORGE;   **
C               **    2) READ IN   FROM MASS STORAGE.  **
C               *****************************************
C
      ISTEPN='20'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOP3.EQ.'WRIT')GOTO2100
      GOTO2200
C
C               ******************************************
C               **  STEP 21--                           **
C               **  WRITE THE V(.) VECTOR               **
C               **  OUT TO THE MASS STORAGE FILE        **
C               **  WITH NUMERIC DESIGNATION    ISCRNU  **
C               ******************************************
C
 2100 CONTINUE
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MAXN2=0
      MAXCO2=0
      MAXIJ2=0
C
      IF(NUMNAM.LE.0)GOTO2129
      DO2110J=1,NUMNAM
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWAP')GOTO2119
      WRITE(ICOUT,2111)J,IHNAME(J),IHNAM2(J),IUSE(J),IN(J),IVALUE(J)
 2111 FORMAT('J,IHNAME(J),IHNAM2(J),ISE(J),IN(J),IVALUE(J) = ',
     1I8,2X,A4,2X,A4,2X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
 2119 CONTINUE
      IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GOTO2110
      IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GOTO2110
      IF(IUSE(J).EQ.'V')GOTO2115
      GOTO2110
 2115 CONTINUE
      IROW=IN(J)
      ICOL=IVALUE(J)
      IF(ICOL.GT.MAXCOL)GOTO2110
      IF(IROW.GT.MAXN2)MAXN2=IROW
      IF(ICOL.GT.MAXCO2)MAXCO2=ICOL
 2110 CONTINUE
 2129 CONTINUE
C
      MAXIJ2=MAXN*(MAXCO2-1)+MAXN2
      IF(MAXIJ2.LE.0)GOTO9000
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1WRITE(ICOUT,999)
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL DPWRST('XXX','BUG ')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1WRITE(ICOUT,2191)
 2191 FORMAT('***** A SWAP OUT IS ABOUT TO BE EXECUTED.')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL DPWRST('XXX','BUG ')
C
      IDEV='SCRA'
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
CCCCC IF(MAXIJ2.GE.1)WRITE(IOUNIT)(V(IJ),IJ=1,MAXIJ2)
C
C  OCTOBER 1991.  SUN HAS LIMIT (SEEMS TO BE 2,046 WORDS) ON NUMBER OF
C  WORDS THAT CAN BE WRITTEN IN ONE RECORD.  ABOVE LINE REPLACED WITH
C  FOLLOWING BLOCK OF CODE.
C
C  MAY 2009.  ABOVE ISSUE IS NO LONGER A PROBLEM.  HOWEVER, WITH THE
C             LARGER DATA SET SIZE NOW SUPPORTED BY DATAPLOT, THIS ROUTINE
C             IS BECOMING A BIT OF A POTENTIAL BOTTLE NECK.  SPECIFICALLY,
C
C             1) IF WE USE
C
C                   WRITE(IOUNT)V
C
C                WE DECREASE THE CPU TIME USED.  HOWEVER, IT INCREASES
C                THE WALL CLOCK TIME (WRITING 10,0000,0000 VALUES AT
C                ONE TIME PROBABLY INCREASES "SWAPPING" ISSUES).
C
C             2) IF WE USE
C
C                   WRITE(IOUNT)(V(IJ),IJ=1,MAXIJ2)
C
C                 WE GREATLY INCREASE THE CPU TIME.
C
C             FOR NOW, I WILL WRITE OUT IN CHUNKS OF 10,000 (THIS WILL BE
C             SET IN MAXWRD).
C
      IF(MAXIJ2.GE.1)THEN
CCCCC   WRITE(IOUNIT)(V(IJ),IJ=1,MAXIJ2)
CCCCC   WRITE(IOUNIT)V
C
CCCCC   MAXWRD=100000
CCCCC   MAXWRD=1000000
CCCCC   IF(IHOST1.EQ.'SUN')MAXWRD=2046
        MAXWRD=10000
        IF(MAXWRD.EQ.MAXOBW)THEN
          WRITE(IOUNIT)V
          GOTO2199
        ENDIF
        NLOOPF=(MAXIJ2/MAXWRD)+1
        IF(NLOOPF.LT.1)GOTO2197
        DO2192IK=1,NLOOPF
          JSTART=(IK-1)*MAXWRD+1
          IF(JSTART.GT.MAXIJ2)GOTO2197
          JSTOP=IK*MAXWRD
          IF(JSTOP.GT.MAXIJ2)JSTOP=MAXIJ2
          WRITE(IOUNIT) (V(IJ),IJ=JSTART,JSTOP)
 2192   CONTINUE
 2197   CONTINUE
 2199   CONTINUE
C
      ENDIF
C  END CHANGE
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
      GOTO9000
C
C               ******************************************
C               **  STEP 22--                           **
C               **  READ  THE V(.) VECTOR               **
C               **  IN FROM THE MASS STORAGE FILE       **
C               **  WITH NUMERIC DESIGNATION    ISCRNU  **
C               ******************************************
C
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(MAXIJ2.LE.0)GOTO9000
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1WRITE(ICOUT,999)
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL DPWRST('XXX','BUG ')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1WRITE(ICOUT,2291)
 2291 FORMAT('***** A SWAP IN  IS ABOUT TO BE EXECUTED.')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')
     1CALL DPWRST('XXX','BUG ')
C
      IDEV='SCRA'
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
CCCCC IF(MAXIJ2.GE.1)READ(IOUNIT)(V(IJ),IJ=1,MAXIJ2)
C
C  OCTOBER 1991.  SUN HAS LIMIT (SEEMS TO BE 2,046 WORDS) ON NUMBER OF
C  WORDS THAT CAN BE WRITTEN IN ONE RECORD.  ABOVE LINE REPLACED WITH
C  FOLLOWING BLOCK OF CODE.
C
C  MAY 2009.  SEE COMMENTS ABOVE FOR WRITE CASE.
C
      IF(MAXIJ2.GE.1)THEN
CCCCC   READ(IOUNIT)(V(IJ),IJ=1,MAXIJ2)
CCCCC   READ(IOUNIT)V
CCCCC   MAXWRD=100000
CCCCC   MAXWRD=1000000
CCCCC   IF(IHOST1.EQ.'SUN')MAXWRD=2046
        MAXWRD=10000
        IF(MAXWRD.EQ.MAXOBW)THEN
          READ(IOUNIT)V
          GOTO2299
        ENDIF
        NLOOPF=(MAXIJ2/MAXWRD)+1
        IF(NLOOPF.LT.1)GOTO2297
        DO2292IK=1,NLOOPF
          JSTART=(IK-1)*MAXWRD+1
          IF(JSTART.GT.MAXIJ2)GOTO2297
          JSTOP=IK*MAXWRD
          IF(JSTOP.GT.MAXIJ2)JSTOP=MAXIJ2
          READ(IOUNIT) (V(IJ),IJ=JSTART,JSTOP)
 2292   CONTINUE
 2297   CONTINUE
 2299   CONTINUE
      ENDIF
C  END CHANGE
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWAP')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSWAP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGS2,IOP3
 9013 FORMAT('IBUGS2,IOP3 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)MAXN2,MAXCO2,MAXIJ2
 9014 FORMAT('MAXN2,MAXCO2,MAXIJ2 = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)IBUGS2,ISUBRO,IERROR
 9019 FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)IOUNIT
 9021 FORMAT('IOUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)IFILE
 9022 FORMAT('IFILE  = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9023)ISTAT
 9023 FORMAT('ISTAT  = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9024)IFORM
 9024 FORMAT('IFORM  = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9025)IACCES
 9025 FORMAT('IACCES = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9026)IPROT
 9026 FORMAT('IPROT  = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9027)ICURST
 9027 FORMAT('ICURST = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9028)IENDFI
 9028 FORMAT('IENDFI = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9029)IREWIN
 9029 FORMAT('IREWIN = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)ISUBN0
 9031 FORMAT('ISUBN0 = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)IERRFI
 9032 FORMAT('IERRFI = ',A12)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPSWA2(IOP3,IFILE,V,MAXIJ2,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--SWAP (WRITE OUT OR READ IN) THE VECTOR V(.)
C              FROM MASS STORAGE.
C              THIS IS A VARIATION OF DPSWAP.  THE DIFFERENCE
C              IS THAT THIS READS/WRITES AN ARBITRARY MATRIX,
C              NOT NECCESSARILY THE INTERNAL V MATRIX, WITH
C              MAXIJ2 DEFINING THE NUMBER OF VALUES TO READ/WRITE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST    1997.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOP3
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
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
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      DOUBLE PRECISION V(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSW'
      ISUBN2='A2  '
      ISUBN0='SWA2'
      IERROR='NO'
      IWIDTH=(-999)
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWA2')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSWA2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGS2,IOP3
   53 FORMAT('IBUGS2,IOP3 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)V(1),V(2),V(3)
   54 FORMAT('V(1),V(2),V(3) = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)ISCRNU
   71 FORMAT('ISCRNU = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,72)ISCRNA(1:80)
   72 FORMAT('ISCRNA = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,73)ISCRST
   73 FORMAT('ISCRST = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,74)ISCRFO
   74 FORMAT('ISCRFO = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,75)ISCRAC
   75 FORMAT('ISCRAC = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,76)ISCRFO
   76 FORMAT('ISCRFO = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,77)ISCRCS
   77 FORMAT('ISCRCS = ',A12)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=ISCRNU
CCCCC PASS IN FILE NAME, RECIPE CODE USES MULTIPLE SCRATCH FILES.
CCCCC IFILE=ISCRNA
      ISTAT=ISCRST
      IFORM=ISCRFO
      IACCES=ISCRAC
      IPROT=ISCRPR
      ICURST=ISCRCS
C
      ISUBN0='SWA2'
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWA2')GOTO1199
      WRITE(ICOUT,1193)IOUNIT
 1193 FORMAT('IOUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1194)IFILE
 1194 FORMAT('IFILE = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195 FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',
     1A12,2X,A12,2X,A12,2X,A12,2X,A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1196)ISUBN0,IERRFI
 1196 FORMAT('ISUBN0,IERRFI = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1199 CONTINUE
C
C               **********************************************
C               **  STEP 12--                               **
C               **  CHECK TO SEE IF SCRATCH FILE MAY EXIST  **
C               **********************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')GOTO1200
      GOTO1290
 1200 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** IMPLEMENTATION ERROR IN DPSWA2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      THE DESIRED RECIPE OPERATION REQUIRES THE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1213)
 1213 FORMAT('      BEHIND-THE-SCENES USE OF A SCRATCH FILE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)
 1214 FORMAT('      BUT THE USE OF SUCH A SCRATCH FILE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('      CANNOT BE DONE BECAUSE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1216)
 1216 FORMAT('      THE INTERNAL VARIABLE    ISCRST ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1217)
 1217 FORMAT('      WHICH ALLOWS SUCH SCRATCH FILE USE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1218)
 1218 FORMAT('      HAS BEEN SET TO    NONE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1219)ISTAT,ISCRST
 1219 FORMAT('ISTAT,ISCRST = ',A12,2X,A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1220)
 1220 FORMAT('      PLEASE CONTACT THE DATAPLOT IMPLEMENTOR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1221)
 1221 FORMAT('      AND HAVE THE ISCRST SETTING CHANGED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1222)
 1222 FORMAT('      (FROM   NONE   TO   UNKNOWN)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1223)
 1223 FORMAT('      IN SUBROUTINE INITFO.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1290 CONTINUE
C
C               *****************************************
C               **  STEP 20--                          **
C               **  BRANCH TO THE APPROPRIATE CASE--   **
C               **    1) WRITE OUT TO   MASS STORGE;   **
C               **    2) READ IN   FROM MASS STORAGE.  **
C               *****************************************
C
      ISTEPN='20'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOP3.EQ.'WRIT')GOTO2100
      GOTO2200
C
C               ******************************************
C               **  STEP 21--                           **
C               **  WRITE THE V(.) VECTOR               **
C               **  OUT TO THE MASS STORAGE FILE        **
C               **  WITH NUMERIC DESIGNATION    ISCRNU  **
C               ******************************************
C
 2100 CONTINUE
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1WRITE(ICOUT,999)
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL DPWRST('XXX','BUG ')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1WRITE(ICOUT,2191)
 2191 FORMAT('***** A SWAP OUT IS ABOUT TO BE EXECUTED.')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL DPWRST('XXX','BUG ')
C
      IDEV='SCRA'
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
      IF(MAXIJ2.LT.1)GOTO2199
      MAXWRD=100000
      IF(IHOST1.EQ.'SUN')MAXWRD=2046
      NLOOPF=(MAXIJ2/MAXWRD)+1
      IF(NLOOPF.LT.1)GOTO2197
      DO2192IK=1,NLOOPF
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXIJ2)GOTO2197
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXIJ2)JSTOP=MAXIJ2
      WRITE(IOUNIT) (V(IJ),IJ=JSTART,JSTOP)
 2192 CONTINUE
 2197 CONTINUE
 2199 CONTINUE
C  END CHANGE
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
      GOTO9000
C
C               ******************************************
C               **  STEP 22--                           **
C               **  READ  THE V(.) VECTOR               **
C               **  IN FROM THE MASS STORAGE FILE       **
C               **  WITH NUMERIC DESIGNATION    ISCRNU  **
C               ******************************************
C
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(MAXIJ2.LE.0)GOTO9000
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1WRITE(ICOUT,999)
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL DPWRST('XXX','BUG ')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1WRITE(ICOUT,2291)
 2291 FORMAT('***** A SWAP IN  IS ABOUT TO BE EXECUTED.')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')
     1CALL DPWRST('XXX','BUG ')
C
      IDEV='SCRA'
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
      IF(MAXIJ2.LT.1)GOTO2299
      MAXWRD=100000
      IF(IHOST1.EQ.'SUN')MAXWRD=2046
      NLOOPF=(MAXIJ2/MAXWRD)+1
      IF(NLOOPF.LT.1)GOTO2297
      DO2292IK=1,NLOOPF
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXIJ2)GOTO2297
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXIJ2)JSTOP=MAXIJ2
      READ(IOUNIT) (V(IJ),IJ=JSTART,JSTOP)
 2292 CONTINUE
 2297 CONTINUE
 2299 CONTINUE
C  END CHANGE
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWA2')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSWA2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGS2,IOP3
 9013 FORMAT('IBUGS2,IOP3 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)MAXIJ2
 9014 FORMAT('MAXIJ2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)IBUGS2,ISUBRO,IERROR
 9019 FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)IOUNIT
 9021 FORMAT('IOUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)IFILE(1:80)
 9022 FORMAT('IFILE  = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9023)ISTAT
 9023 FORMAT('ISTAT  = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9024)IFORM
 9024 FORMAT('IFORM  = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9025)IACCES
 9025 FORMAT('IACCES = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9026)IPROT
 9026 FORMAT('IPROT  = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9027)ICURST
 9027 FORMAT('ICURST = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9028)IENDFI
 9028 FORMAT('IENDFI = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9029)IREWIN
 9029 FORMAT('IREWIN = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)ISUBN0
 9031 FORMAT('ISUBN0 = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)IERRFI
 9032 FORMAT('IERRFI = ',A12)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPSYMB(IHARG,NUMARG,IDEFSY,ITEXSY,
     1                  IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE SYMBOL CHARACTER WHICH MAY
C              BE USED TO DENOTE IN-LINE TEXT SUB-COMMANDS.
C              WHEN A TEXT STRING IS PROCESSED,
C              IT IS SCANNED FOR THE SYMBOL CHARACTER;
C              IF IT IS FOUND, THE IN-LINE SUB-COMMAND
C              BEFORE THE SYMBOL CHARACTER IS EXECUTED
C              RATHER THAN THE LITERAL TEXT SUB-STRING BEING WRITTEN OUT.
C              ANY NUMBER OF SYMBOL CHARACTERS ARE ALLOWED PER LINE.
C              THE SYMBOL CHARACTER CAPABILITY ALLOWS THE ANALYST
C              TO WRITE GREEK, MATH, AND OTHER SPECIAL SYMBOLS.
C              THE SPECIFIED SYMBOL CHARACTER WILL BE PLACED
C              IN THE CHARACTER VARIABLE ITEXSY.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFSY (A  CHARACTER VARIABLE)
C                     --IBUGD2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--ITEXSY (A CHARACTER VARIABLE)
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
      CHARACTER*4 IDEFSY
      CHARACTER*4 ITEXSY
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPSYMB--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFSY
   53 FORMAT('IDEFSY = ',A4)
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
      IF(NUMARG.LE.0)GOTO1150
      GOTO1110
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFSY
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      ITEXSY=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE SYMBOL CHARACTER (TO DENOTE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT(' GREEK, MATH, AND OTHER SPECIAL SYMBOLS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1183)
 1183 FORMAT('IN THE TEXT, TITLE, LABEL, AND LEGEND COMMANDS)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1184)ITEXSY
 1184 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSYMB--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFSY,ITEXSY
 9013 FORMAT('IDEFSY,ITEXSY = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPSYMM(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A SYMMETRY PLOT
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/7
C     ORIGINAL VERSION--APRIL     1986.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --NOVEMBER  2011. 1) USE DPPARS TO PERFORM
C                                          SOME OF THE PARSING
C                                       2) SUPPORT "REPLICATION" AND
C                                          "MULTIPLE" KEYWORDS
C                                       3) SUPPORT "HIGHLIGHT" OPTION
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
      CHARACTER*4 IDATSW
      CHARACTER*4 IREPL
      CHARACTER*4 IHIGH
      CHARACTER*4 IWRITE
      CHARACTER*4 IMULT
      CHARACTER*4 IGROUP
      CHARACTER*4 ITERM1
      CHARACTER*4 ITERM2
      CHARACTER*4 ITERM3
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASE
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
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
      DIMENSION XTEMP4(MAXOBV)
      DIMENSION XDESGN(MAXOBV,6)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION ZY(MAXOBV)
      DIMENSION TAG1(MAXOBV)
CCCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y1(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),XTEMP3(1))
      EQUIVALENCE (GARBAG(IGARB6),XTEMP4(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB8),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB9),ZY(1))
      EQUIVALENCE (GARBAG(JGAR11),TAG1(1))
      EQUIVALENCE (GARBAG(JGAR12),XDESGN(1,1))
CCCCC END CHANGE
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPSY'
      ISUBN2='MM  '
C
      IHIGH='OFF'
      IMULT='OFF'
      IREPL='OFF'
      ICASE='NONE'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***************************************
C               **  TREAT THE SYMMETRY    PLOT CASE  **
C               ***************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSYMM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
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
C     LOOK FOR THE WORDS "SYMMETRY PLOT".  ALSO LOOK
C     FOR THE KEYWORDS "MULTIPLE", "REPLICATION", OR "HIGHLIGHT".
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTC=-9999
      DO100I=0,NUMARG-1
C
        IF(I.EQ.0)THEN
          ITERM1=ICOM
          ITERM2=IHARG(I+1)
          ITERM3=IHARG(I+2)
        ELSE
          ITERM1=IHARG(I)
          ITERM2=IHARG(I+1)
          ITERM3=IHARG(I+2)
        ENDIF
C
        IF(ITERM1.EQ.'SYMM' .AND. ITERM2.EQ.'PLOT')THEN
          IFOUND='YES'
          ILASTC=MAX(ILASTC,I+1)
        ELSEIF(ITERM1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(ITERM1.EQ.'MULT')THEN
          IMULT='ON'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(ITERM1.EQ.'HIGH')THEN
          IHIGH='ON'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(ITERM1.EQ.'GROU' .OR. ITERM1.EQ.'BINN')THEN
          IGROUP='ON'
          ILASTC=MAX(ILASTC,I)
        ENDIF
  100 CONTINUE
C
      IF(IFOUND.EQ.'NO')GOTO9000
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN SYMMETRY PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THE SYMMETRY PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(IHIGH.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)
  122     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"HIGHTLIGHTED" FOR THE SYMMETRY PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
      ICASPL='SYMM'
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='SYMMETRY PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IF(IMULT.EQ.'ON')THEN
        IFLAGE=0
        IFLAGM=1
      ELSE
         IF(IREPL.EQ.'OFF' .AND. IHIGH.EQ.'OFF')IFLAGM=1
      ENDIF
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      IF(IMULT.EQ.'OFF' .AND. IHIGH.EQ.'OFF' .AND. IREPL.EQ.'OFF')THEN
        MINNVA=1
        MAXNVA=3
        IFLAGM=1
      ELSEIF(IHIGH.EQ.'ON')THEN
        MINNVA=2
        MAXNVA=3
        IFLAGM=0
      ELSE
        MINNVA=-99
        MAXNVA=-99
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')THEN
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
C               **  STEP 3--                                 **
C               **  DETERMINE:                               **
C               **  1) NUMBER OF REPLICATION VARIABLES (0-2) **
C               **  2) NUMBER OF GROUPING    VARIABLES (0-2) **
C               **  3) NUMBER OF RESPONSE    VARIABLES (>= 1)**
C               **  4) NUMBER OF HIGHLIGHT   VARIABLES (0-2) **
C               ***********************************************
C
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NRESP=0
      NREPL=0
      NGROUP=0
      NHIGH=0
      IDATSW='RAW'
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSEIF(IHIGH.EQ.'ON')THEN
        NRESP=1
        NHIGH=NUMVAR-1
        IF(NHIGH.LT.1 .OR. NHIGH.GT.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,501)
  501     FORMAT('      FOR THE HIGHLIGHTED CASE, THE NUMBER OF ',
     1           'HIGHLIGHT VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,502)
  502     FORMAT('      MUST BE ONE OR TWO;  SUCH WAS NOT THE ',
     1           'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,503)NHIGH
  503     FORMAT('      THE NUMBER OF HIGHLIGHT VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
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
      ENDIF
C
C               *********************************************
C               **  STEP 7A--                              **
C               **  CASE 1: NO REPLICATION, NO MULTIPLE,   **
C               **          AND NO HIGHLIGHTING            **
C               *********************************************
C
C     FOR THIS CASE, CAN HAVE ONE TO TWO RESPONSE VARIABLES
C     (DEPDENDING ON WHETHER WE HAVE BINNED DATA OR RAW DATA).
C
C     FOR THIS CASE, ONLY SUPPORT MATRIX ARGUMENT FOR RAW DATA
C     NUMBER OF OBSERVATIONS MUST BE THE SAME FOR ALL VARIABLES.
C
      IF(IMULT.EQ.'OFF' .AND. NREPL.EQ.0 .AND. NHIGH.EQ.0)THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        IF(NUMVAR.EQ.1)THEN
          IDATSW='RAW'
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,X1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
        ELSEIF(NUMVAR.EQ.2)THEN
          IDATSW='FREQ'
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,X1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
        ENDIF
        IF(ICASE.EQ.'MATR' .AND. NUMVAR.GT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,701)
  701     FORMAT('      MATRIX ARGUMENTS ARE ONLY SUPPORTED FOR THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,703)
  703     FORMAT('      RAW DATA CASE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(NUMVAR.EQ.2 .AND. NLOCAL.NE.NLOCA2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,711)
  711     FORMAT('      FOR THE FREQUENCY CASE, THE NUMBER OF ',
     1           'OBSERVATIONS FOR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,713)
  713     FORMAT('      THE TWO VARIABLES MUST BE EQUAL.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,715)IVARN1(1),IVARN2(1),NLOCAL
  715     FORMAT('      ',A4,A4,' HAS ',I8,' OBSERVATIONS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,715)IVARN1(2),IVARN2(2),NLOCA2
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
C       *****************************************************
C       **  STEP 7B--                                      **
C       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C       **  RESET THE VECTOR D(.) TO ALL ONES.             **
C       **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
C       **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
C       *****************************************************
C
C
        IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SYMM')THEN
          ISTEPN='7B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,731)
  731     FORMAT('***** FROM THE MIDDLE  OF DPSYMM--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,732)ICASPL,NUMVAR,IDATSW,NLOCAL
  732     FORMAT('ICASPL,NUMVAR,IDATSW,NLOCAL = ',
     1           A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO735I=1,NLOCAL
              WRITE(ICOUT,736)I,Y1(I),X1(I)
  736         FORMAT('I,Y1(I),X1(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
  735       CONTINUE
          ENDIF
        ENDIF
C
        NPLOTP=0
        NCURVE=1
        CALL DPSYM2(Y1,X1,NLOCAL,ICASPL,IDATSW,MAXOBV,
     1              NUMVAR,NCURVE,NHIGH,
     1              TAG1,XTEMP1,XTEMP2,
     1              Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES **
C               **          NOTE THAT HIGHLIGHTING AND  **
C               **          GROUPING ARE NOT SUPPORTED  **
C               **          FOR THIS CASE.              **
C               ******************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
        NPLOTP=0
        NCURVE=0
        DO810IRESP=1,NRESP
          NCURVE=NCURVE+1
C
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PERC')THEN
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
C
C         *****************************************************
C         **  STEP 8B--                                      **
C         **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C         **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C         **  RESET THE VECTOR D(.) TO ALL ONES.             **
C         **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
C         **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
C         *****************************************************
C
C
          IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SYMM')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPSYMM--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASPL,NUMVAR,IDATSW,NLOCAL
  823       FORMAT('ICASPL,NUMVAR,IDATSW,NLOCAL = ',
     1             A4,I8,2X,A4,I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO825I=1,NLOCAL
                WRITE(ICOUT,826)I,Y1(I)
  826           FORMAT('I,X1(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
C
          CALL DPSYM2(Y1,X1,NLOCAL,ICASPL,IDATSW,MAXOBV,
     1                NUMVAR,NCURVE,NHIGH,
     1                TAG1,XTEMP1,XTEMP2,
     1                Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
  810   CONTINUE
C
C               ***************************************************
C               **  STEP 9A--                                    **
C               **  CASE 3: ONE OR MORE REPLICATION VARIABLES.   **
C               **          CURRENTLY, ONLY SUPPORT THIS OPTION  **
C               **          FOR UNBINNED DATA.                   **
C               ***************************************************
C
      ELSEIF(NRESP.GE.1 .AND. NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO910I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO910
          J=J+1
C
C         RESPONSE VARIABLE IN X1 (OR Y1 IF GROUPED DATA)
C
          IJ=MAXN*(ICOLR(1)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
          ICOLC=1
C
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
        IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SYMM')THEN
          ISTEPN='9B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,931)
  931     FORMAT('***** FROM THE MIDDLE  OF DPSYMM--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,932)ICASPL,NUMVAR,IDATSW,NLOCAL
  932     FORMAT('ICASPL,NUMVAR,IDATSW,NLOCAL = ',
     1           A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO935I=1,NLOCAL
              WRITE(ICOUT,936)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  936         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2)=',I8,3G15.7)
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
        CALL DPPP5(XDESGN(1,1),XDESGN(1,2),XDESGN(1,3),
     1             XDESGN(1,4),XDESGN(1,5),XDESGN(1,6),
     1             NREPL,NLOCAL,MAXOBV,
     1             XIDTEM,XIDTE2,XIDTE2,XIDTE2,XIDTE2,XIDTE2,
     1             XTEMP1,XTEMP2,
     1             NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,
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
            NUMVA2=1
            IF(NTEMP.GT.0)THEN
              CALL DPSYM2(ZY,X1,NTEMP,ICASPL,IDATSW,MAXOBV,
     1                    NUMVA2,NCURVE,NHIGH,
     1                    TAG1,XTEMP1,XTEMP2,
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
            NUMVA2=1
            IF(NTEMP.GT.0)THEN
              CALL DPSYM2(ZY,X1,NTEMP,ICASPL,IDATSW,MAXOBV,
     1                    NUMVA2,NCURVE,NHIGH,
     1                    TAG1,XTEMP1,XTEMP2,
     1                    Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1220     CONTINUE
 1210     CONTINUE
        ENDIF
C
C               ***************************************************
C               **  STEP 10A--                                   **
C               **  CASE 4: ONE OR TWO HIGHLIGHT VARIABLES.      **
C               **          THIS CASE DOES NOT SUPPORT GROUPED   **
C               **          DATA AND ALL VARIABLES MUST HAVE     **
C               **          SAME LENGTH.                         ** 
C               ***************************************************
C
      ELSEIF(NRESP.EQ.1 .AND. NHIGH.GE.1)THEN
        ISTEPN='10A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          ICOL=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,X1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
C
        IF(NHIGH.EQ.1)THEN
          CALL CODE(X1,NLOCAL,IWRITE,TAG1,XTEMP1,MAXOBV,
     1              IBUGG3,IERROR)
        ELSE
          ICCTOF=0
          ICCTG1=0
          CALL CODCT2(X1,XTEMP1,NLOCAL,ICCTOF,ICCTG1,IWRITE,
     1                TAG1,XTEMP2,XTEMP3,
     1                IBUGG3,ISUBRO,IERROR)
        ENDIF
C
C       *****************************************************
C       **  STEP 10B--                                     **
C       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C       **                                                 **
C       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
C       **  VARIOUS REPLICATIONS.                          **
C       *****************************************************
C
        IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SYMM')THEN
          ISTEPN='10B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1731)
 1731     FORMAT('***** FROM THE MIDDLE  OF DPSYMM--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1732)ICASPL,NUMVAR,IDATSW,NLOCAL
 1732     FORMAT('ICASPL,NUMVAR,IDATSW,NQ = ',
     1           A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO1735I=1,NLOCAL
              WRITE(ICOUT,1736)I,Y1(I),TAG1(I)
 1736         FORMAT('I,Y1(I),TAG1(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 1735       CONTINUE
          ENDIF
        ENDIF
C
C       ************************************
C       **  STEP 10C--                    **
C       **  GENERATE THE SYMMETRY PLOT    **
C       ************************************
C
        NPLOTP=0
        NCURVE=1
        CALL DPSYM2(Y1,X1,NLOCAL,ICASPL,IDATSW,MAXOBV,
     1              NUMVAR,NCURVE,NHIGH,
     1              TAG1,XTEMP1,XTEMP2,
     1              Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSYMM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO9020I=1,NPLOTP
           WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021      FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
           CALL DPWRST('XXX','BUG ')
 9020    CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSYM2(Y,X,N,ICASPL,IDATSW,MAXOBV,
     1                  NUMVAR,NCURVE,NHIGH,
     1                  TAG1,XTEMP1,XTEMP2,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A SYMMETRY PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/7
C     ORIGINAL VERSION--APRIL     1986.
C     UPDATED         --NOVEMBER  2011. SUPPORT FOR HIGHLIGHTED CASE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IDATSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRIT2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION TAG1(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSY'
      ISUBN2='M2  '
      IERROR='NO'
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SYM2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPSYM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ICASPL,IDATSW,N,NPLOTV,N2,NUMVAR
   72   FORMAT('ICASPL,IDATSW,N,NPLOTV,N2,NUMVAR = ',2(A4,2X),4I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO85I=1,N
            WRITE(ICOUT,86)I,Y(I),X(I)
   86       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   85     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN SYMMETRY PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS WAS LESS THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS      = ',I6)
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
   62 FORMAT('      ALL INPUT VERTICAL AXIS ELEMENTS')
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
C               **************************************
C               **  STEP 4--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **  AND DETERMINE PLOT COORDINATES  **
C               **************************************
C
      IF(IDATSW.EQ.'RAW')THEN
C
C               ****************************************
C               **  STEP 4.1--                        **
C               **  DETERMINE PLOT COORDINATES        **
C               **  FOR THE 1-VARIABLE CASE           **
C               **  (THAT IS, FOR THE RAW DATA CASE)  **
C               ****************************************
C
        IWRIT2='OFF'
        CALL MEDIAN(Y,N,IWRIT2,XTEMP1,MAXOBV,XMED,IBUGG3,IERROR)
C
        IF(NHIGH.EQ.0)THEN
          CALL SORT(Y,N,XTEMP1)
          NHALFP=(N+1)/2
          DO1110I=1,NHALFP
            IREV=N-I+1
            Y2(N2+I)=XTEMP1(IREV)-XMED
            X2(N2+I)=XMED-XTEMP1(I)
            D2(N2+I)=REAL(NCURVE)
 1110     CONTINUE
          N2=N2+NHALFP
          NPLOTV=2
        ELSE
C
C         HIGHLIGHT CASE: BASE HIGHLIGHTING ON MAXIMUM OF
C                         THE TWO POINTS THAT GENERATE A SINGLE
C                         PLOT POINT.
C
          CALL SORTC(Y,TAG1,N,XTEMP1,XTEMP2)
          NHALFP=(N+1)/2
          DO1210I=1,NHALFP
            IREV=N-I+1
            Y2(N2+I)=XTEMP1(IREV)-XMED
            X2(N2+I)=XMED-XTEMP1(I)
            D2(N2+I)=MAX(XTEMP2(I),XTEMP2(IREV))
 1210     CONTINUE
          N2=N2+NHALFP
          NPLOTV=2
        ENDIF
      ELSEIF(IDATSW.EQ.'FREQ')THEN
C
C               ********************************************
C               **  STEP 4.2--                            **
C               **  DETERMINE PLOT COORDINATES            **
C               **  FOR THE 2-VARIABLE CASE               **
C               **  (THAT IS, FOR THE GROUPED DATA CASE)  **
C               ********************************************
C
        CALL SORTC(X,Y,N,D2,Y2)
C
        SUM=0.0
        DO2110I=1,N
         SUM=SUM+Y(I)
 2110   CONTINUE
        NTOT=INT(SUM+0.5)
C
        IF(NTOT.GT.1000)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2111)
 2111     FORMAT('***** ERROR IN DPSYM2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2113)
 2113     FORMAT('      FOR THE 2-VARIABLE (GROUPED) CASE, THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2114)
 2114     FORMAT('      UNGROUPED NUMBER OF OBSERVATIONS IS TOO ',
     1           'LARGE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2116)NTOT
 2116     FORMAT('      NTOT = ',I8)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        K=0
        DO2121I=1,N
          NI=INT(Y2(I)+0.5)
          IF(NI.LE.0)GOTO2121
          DO2122J=1,NI
            K=K+1
            X2(K)=D2(I)
 2122     CONTINUE
 2121   CONTINUE
C
        IWRIT2='OFF'
        MAXND2=1000
        CALL MEDIAN(X2,K,IWRIT2,D2,MAXND2,XMED,IBUGG3,IERROR)
        CALL SORT(X2,K,D2)
C
        KHALFP=(K+1)/2
        DO2130I=1,KHALFP
          IREV=K-I+1
          Y2(I)=D2(IREV)-XMED
          X2(I)=XMED-D2(I)
 2130   CONTINUE
        DO2140I=1,KHALFP
          D2(I)=1.0
 2140   CONTINUE
        N2=KHALFP
        NPLOTV=2
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1012)
 1012   FORMAT('      IDATSW SHOULD BE EITHER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1013)
 1013   FORMAT('      RAW   OR    FREQ, BUT IS NEITHER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1014)IDATSW
 1014   FORMAT('      IDATSW = ',A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SYM2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSYM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,IDATSW,N2,IERROR
 9012   FORMAT('ICASPL,IDATSW,N2,IERROR = ',A4,2X,A4,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,NHALFP,NTOT,K,KHALFP
 9013   FORMAT('N,NHALFP,NTOT,K,KHALFP = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)N2,NPLOTV
 9014   FORMAT('N2,NPLOTV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSYST(IANS,IANSLC,IWIDTH,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1                  IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--ENTER AN OPERATING SYSTEM COMMAND.  NOTE THAT THIS COMMAND
C              IS SITE AND HOST DEPENDENT.  IT IS PROVIODED TO ACCOMODATE
C              THOSE OPERATING SYSTEMS THAT ALLOW HOOKS INTO THE OPERATING
C              SYSTEM.  IT IS LEFT UP TO THE LOCAL IMPLEMENTOR AS TO HOW
C              THIS COMMAND WILL BE USED.
C
C              THE CALL TO THE OPERATING SYSTEM IS DONE BELOW IN
C                    CALL SCLCMD
C              IF YOUR COMPUTER DOES NOT ALLOW SUCH A HOOK, DO NOTHING.
C              IF YOUR COMPUTER DOES ALLOW SUCH A HOOK, THEN THE
C              IMPLEMENTER SHOULD REPLACE THE CALL TO SCLCMD
C              (WHICH IS APPROPRIATE ONLY FOR CDC CYBER NOS/VE)
C              WITH THE APPROPRIATE SYSTEM CALL;
C              THE LINE SHOULD ALSO BE UN-COMMENTED OUT.
C
C              NOTE THAT IF A COMMAND IS PASSED TO THE OPERATING SYSTEM,
C              DATAPLOT WILL DO NO ERROR CHECKING.  IT WILL SIMPLY PASS
C              THE COMMAND AS GIVEN.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--89.3
C     ORIGINAL VERSION--FEBRUARY   1989.
C     UPDATED         --MARCH      1990. USE "IANSLC" SINCE SOME SYSTEMS
C                                        ARE CASE SENSITIVE (E.G., UNIX)
C     UPDATED         --APRIL      1992. DO OPERATING SPECIFIC CALL IN DPSYS2
C     UPDATED         --APRIL      1992. ADD ISUBRO IN CALL TO DPSYS2
C     UPDATED         --APRIL      1992. ADD UNIX & DOS
C     UPDATED         --APRIL      1992. ADD OTG CHECK
C     UPDATED         --APRIL      1992. AUGMENT ERROR INFO
C     UPDATED         --APRIL      2018. SINCE THE PLATFORM DEPENDENT
C                                        CODE IS IN DPSYS2, RECODE THIS
C                                        ROUTINE AND MOVE IT OUT OF
C                                        dp1.F.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 IANS
      CHARACTER*4 IANSLC
C
      CHARACTER*4 ITEXTE
      CHARACTER*4 ITEXTF
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER*4 IFUNC
      CHARACTER*1 IREPCH
C
      DIMENSION IANS(*)
      DIMENSION IANSLC(*)
C
      PARAMETER(MAXCH=256)
      DIMENSION ITEXTE(MAXCH)
      DIMENSION ITEXTF(MAXCH)
      CHARACTER*256 ITEXT2
      CHARACTER*256 ITEXT3
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      J2=0
C
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'SYST')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSYST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,ISUBRO,IWIDTH,NUMNAM
   53   FORMAT('IBUGD2,ISUBRO,IWIDTH,NUMNAM= ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANS(I),I=1,MIN(255,IWIDTH))
   54   FORMAT('(IANS(I),I=1,IWIDTH) = ',255A4)
        CALL DPWRST('XXX','BUG ')
        DO76I=1,NUMNAM
          WRITE(ICOUT,77)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                   IVALUE(I),VALUE(I)
   77     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)= ',
     1           I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   76   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
C               *****************************************************
C
C               *****************************************
C               **  STEP 1.1--                         **
C               **  DETERMINE THE COMMAND              **
C               **  (SYSTEM OR SYST) AND ITS LOCATION  **
C               **  ON THE LINE.                       **
C               **  DETERMINE THE START POSITION       **
C               **  (XSTART) OF THE FIRST CHARACTER    **
C               **  FOR THE STRING TO BE PRINTED.      **
C               *****************************************
C
C     CHECK FOR "SYSTEM" FIRST
C
      DO1115I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
        IP6=I+6
C
        IF(IP6.GT.IWIDTH)GOTO1115
        ISTART=IP6+1
        IF(IANS(I).EQ.'S'.AND.IANS(IP1).EQ.'Y'.AND.
     1     IANS(IP2).EQ.'S'.AND.IANS(IP3).EQ.'T'.AND.
     1     IANS(IP4).EQ.'E'.AND.IANS(IP5).EQ.'M'.AND.
     1     IANS(IP6).EQ.' ')GOTO1190
 1115 CONTINUE
C
C     CHECK FOR "SYST"
C
      DO1125I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
C
        IF(IP4.GT.IWIDTH)GOTO1125
        ISTART=IP5
        IF(IANS(I).EQ.'S'.AND.IANS(IP1).EQ.'Y'.AND.
     1     IANS(IP2).EQ.'S'.AND.IANS(IP3).EQ.'T'.AND.
     1     IANS(IP4).EQ.' ')GOTO1190
 1125 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED   APRIL 1992
C     CHECK FOR "UNIX"
C
      DO1135I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
C
        IF(IP4.GT.IWIDTH)GOTO1135
        ISTART=IP5
        IF(IANS(I).EQ.'U'.AND.IANS(IP1).EQ.'N'.AND.
     1     IANS(IP2).EQ.'I'.AND.IANS(IP3).EQ.'X'.AND.
     1     IANS(IP4).EQ.' ')GOTO1190
 1135 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED   APRIL 1992
C     CHECK FOR "DOS"
C
      DO1145I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
C
        IF(IP3.GT.IWIDTH)GOTO1145
        ISTART=IP4
        IF(IANS(I).EQ.'D'.AND.IANS(IP1).EQ.'O'.AND.
     1     IANS(IP2).EQ.'S'.AND.IANS(IP3).EQ.' ')GOTO1190
 1145 CONTINUE
C
CCCCC THE FOLLOWING SECTION WAS ADDED   APRIL 2018
C     CHECK FOR "LUNIX"
C
      DO1155I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
C
        IF(IP4.GT.IWIDTH)GOTO1155
        ISTART=IP5
        IF(IANS(I).EQ.'L'.AND.IANS(IP1).EQ.'I'.AND.
     1     IANS(IP2).EQ.'N'.AND.IANS(IP3).EQ.'U'.AND.
     1     IANS(IP4).EQ.'X'.AND.IANS(IP5).EQ.' ')GOTO1190
 1155 CONTINUE
C
C     NO MATCH
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN SYSTEM COMMAND--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO:  SYSTEM, SYST, UNIX, LINUX ',
     1       'OR DOS.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1190 CONTINUE
C
C               **********************************************************
C               **  STEP 1.2--                                          **
C               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.   **
C               **********************************************************
C
      IFOUND='YES'
C
      ISTOP=0
      IF(ISTART.LE.IWIDTH)THEN
        DO1220I=ISTART,IWIDTH
          IREV=IWIDTH-I+ISTART
          IF(IANS(IREV).NE.' ')THEN
            ISTOP=IREV
            GOTO1229
          ENDIF
 1220   CONTINUE
 1229   CONTINUE
      ENDIF
C
C               *****************************************
C               **  STEP 1.3--                         **
C               **  COPY OVER THE STRING OF INTEREST.  **
C               *****************************************
C
      IF(ISTART.GT.ISTOP .OR. ISTOP.EQ.0)THEN
        NCTEX=0
      ELSE
        ITEMP=ISTOP-ISTART+1
        IF(ITEMP.GT.MAXCH)ITEMP=MAXCH
        ISTOP=ISTART+ITEMP-1
C
        J=0
        DO1310I=ISTART,ISTOP
          J=J+1
          J2=J
          ITEXTE(J)=IANS(I)
          ITEXTF(J)=IANSLC(I)
 1310   CONTINUE
        NCTEX=J2
      ENDIF
C
C               ******************************************************
C               **  STEP 1.4--                                      **
C               **  CALL THE SUBROUTINE DPREPL                      **
C               **  WHICH WILL SCAN THE STRING FOR ALL OCCURRANCES  **
C               **  OF THE SUBSTRING VALU()                         **
C               **  AND REPLACE THEM BY THEIR LITERAL VALUES.       **
C               ******************************************************
C
      NCTEXT=NCTEX
      IF(NCTEXT.GE.1)THEN
        CALL DPREPL(ITEXTE,NCTEXT,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1              IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1              IBUGD2,IERROR)
      ENDIF
C
      IF(NCTEXT.GE.1)THEN
        DO1510I=1,NCTEXT
          ITEXT2(I:I)=ITEXTE(I)(1:1)
 1510   CONTINUE
      ENDIF
C
      NCTEXT=NCTEX
      IF(NCTEXT.GE.1)THEN
        CALL DPREPL(ITEXTF,NCTEXT,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1              IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1              IBUGD2,IERROR)
      ENDIF
      IF(NCTEXT.GE.1)THEN
        DO1610I=1,NCTEXT
          ITEXT3(I:I)=ITEXTF(I)(1:1)
 1610   CONTINUE
      ENDIF
C
C               *****************************************
C               **  STEP 2--                           **
C               **  CALL DPSYS2 TO EXECUTE THE COMMAND **
C               *****************************************
C
C     2018/04: ALTHOUGH WINDOWS COMMANDS ARE NOT CASE SENSITIVE,
C              ARGUMENTS MAY BE.  SO USE LOWER CASE VARIANT FOR
C              ALL SYSTEMS.
C
CCCCC IF(IOPSY1.EQ.'UNIX')THEN
        CALL DPSYS2(ITEXT3,NCTEXT,ISUBRO,IERROR)
CCCCC ELSE
CCCCC   CALL DPSYS2(ITEXT2,NCTEXT,ISUBRO,IERROR)
CCCCC ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'SYST')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSYST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)(ITEXTE(I),I=1,NCTEX)
 9016   FORMAT('(ITEXTE(I),I =1,NCTEX) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IFOUND,IERROR,IREPCH,NCTEXT,NCTEX
 9017   FORMAT('IFOUND,IERROR,IREPCH,NCTEXT,NCTEX = ',
     1         2(A4,2X),A1,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT2(J:J),J=1,NCTEXT)
 9018   FORMAT('(ITEXT2(I),I=1,NCTEXT) = ',25A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAB1(IHEAD,NHEAD,CAPTN,NCAP,IFLAG1)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED
C              TO INITIATE THE TABULAR OUTPUT.  THE ONLY OPTIONAL ELEMENT
C              IS THE CAPTION.
C     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
C                                THE TEXT FOR THE HEADER
C                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                HEADER.
C                     --CAPTN  = THE CHARACTER STRING CONTAINING
C                                THE CAPTION.
C                     --NCAP   = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                CAPTION.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/3
C     ORIGINAL VERSION--MARCH     2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) CAPTN
      CHARACTER*(*) IHEAD
C
      LOGICAL IFLAG1
      CHARACTER*10 IFORMT
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: WRITE A HEADER
C
  999 FORMAT(1X)
C
      IF(IFLAG1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IF(NHEAD.GE.1)THEN
        IFORMT=' '
        IFORMT(1:9)='(12X,A  )'
        IF(NHEAD.GT.0 .AND. NHEAD.LE.99)THEN
          WRITE(IFORMT(7:8),'(I2)')NHEAD
          WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD)
          CALL DPWRST('XXX','WRIT')
        ENDIF
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C  STEP 2: START TABLE AND DEFINE A CAPTION
C
      IF(NCAP.GT.0 .AND. NCAP.LT.130)THEN
        IFORMT=' '
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NCAP
        WRITE(ICOUT,IFORMT)CAPTN(1:NCAP)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTABA(IHEAD,NHEAD,CAPTN,NCAP,IFLAG1)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED
C              TO INITIATE THE TABULAR OUTPUT.  THE ONLY OPTIONAL ELEMENT
C              IS THE CAPTION.
C
C              NOTE: THIS IS A SLIGHT VARIANT OF DPTAB1.  DIFFERS
C                    IN POSITIONING OF "CAPTN" STRING.
C
C     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
C                                THE TEXT FOR THE HEADER
C                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                HEADER.
C                     --CAPTN  = THE CHARACTER STRING CONTAINING
C                                THE CAPTION.
C                     --NCAP   = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                CAPTION.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/3
C     ORIGINAL VERSION--MARCH     2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) CAPTN
      CHARACTER*(*) IHEAD
C
      LOGICAL IFLAG1
      CHARACTER*10 IFORMT
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: WRITE A HEADER
C
  999 FORMAT(1X)
C
      IF(IFLAG1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IF(NHEAD.GE.1)THEN
        IFORMT=' '
        IFORMT(1:9)='(12X,A  )'
        WRITE(IFORMT(7:8),'(I2)')NHEAD
        WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C  STEP 2: START TABLE AND DEFINE A CAPTION
C
      NSTRT=12
      NDIFF=NHEAD-NCAP
      IF(NDIFF.GE.2)THEN
        NDIFF=NDIFF/2
        NSTRT=NSTRT+NDIFF
      ENDIF
      IF(NCAP.GT.0)THEN
        IFORMT=' '
        IFORMT(1:9)='(  X,A  )'
        WRITE(IFORMT(2:3),'(I2)')NSTRT
        WRITE(IFORMT(7:8),'(I2)')NCAP
        WRITE(ICOUT,IFORMT)CAPTN(1:NCAP)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAB4(IVALUE,NCHAR,NHEAD,IFLAG1,IFLAG2,NMAX)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED TO
C              GENERATE A HEADER ROW FOR A TABLE.  YOU CAN ALSO OPTIONALLY
C              ADD A RULE LINE BEFORE OR AFTER THE HEADER.
C
C     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING ARRAY
C                                 CONTAINING THE TEXT FOR THE
C                                 HEADER VALUES.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 HEADER VALUES.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF HEADER VALUES.
C                     --IFLAG1  = A LOGICAL VALUE THAT SPECIFIES
C                                 WHETHER A RULE LINE IS DRAWN BEFORE
C                                 THE HEADER.
C                     --IFLAG2  = A LOGICAL VALUE THAT SPECIFIES
C                                 WHETHER A RULE LINE IS DRAWN AFTER
C                                 THE HEADER.
C                     --NMAX    = NUMBER OF CHARACTERS FOR "RULE" LINE
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/3
C     ORIGINAL VERSION--MARCH     2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE(NHEAD)
      INTEGER NCHAR(NHEAD)
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      CHARACTER*255 IATEMP
C
      LOGICAL IFLAG1
      LOGICAL IFLAG2
C
      CHARACTER*20 IFORMT
      CHARACTER*160 ISTR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
CC999 FORMAT(1X)
C
C  STEP 1: PRINT INITIAL RULE LINE
C
      IF(NHEAD.GE.1)THEN
        IF(IFLAG1 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO8010I=1,MIN(NMAX,255)
            IATEMP(I:I)='-'
 8010     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
C  STEP 2: PRINT TEXT FIELDS
C
        IFORMT=' '
        NCSTR=0
        DO8020I=1,NHEAD
          IF(NCHAR(I).GE.1)THEN
            NCSTR=NCSTR+1
            NCSTR2=NCSTR+NCHAR(I)-1
            IFORMT(1:5)='(A  )'
            WRITE(IFORMT(3:4),'(I2)')NCHAR(I)
            WRITE(ISTR(NCSTR:NCSTR2),IFORMT)IVALUE(I)(1:NCHAR(I))
            NCSTR=NCSTR2
          ENDIF
 8020   CONTINUE
        IFORMT=' '
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NCSTR
        IF(NCSTR.GE.1)THEN
          WRITE(ICOUT,IFORMT)ISTR(1:NCSTR)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
C  STEP 3: PRINT TRAILING RULE LINE
C
        IF(IFLAG2 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO8030I=1,NMAX
            IATEMP(I:I)='-'
 8030     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTA44(IVALUE,NCHAR,NHEAD,NCOLSP,IFLAG1,IFLAG2,NMAX)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED TO
C              GENERATE A HEADER ROW FOR A TABLE.  YOU CAN ALSO OPTIONALLY
C              ADD A RULE LINE BEFORE OR AFTER THE HEADER.
C
C              THIS IS A MODIFIED VERSION OF DPTAB4 THAT ALLOWS
C              HEADERS THAT SPAN MULTIPLE COLUMNS.
C
C     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING ARRAY
C                                 CONTAINING THE TEXT FOR THE
C                                 HEADER VALUES.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 HEADER VALUES.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF HEADER VALUES.
C                     --NCOLSP  = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF SPANNING COLUMNS.
C                     --IFLAG1  = A LOGICAL VALUE THAT SPECIFIES
C                                 WHETHER A RULE LINE IS DRAWN BEFORE
C                                 THE HEADER.
C                     --IFLAG2  = A LOGICAL VALUE THAT SPECIFIES
C                                 WHETHER A RULE LINE IS DRAWN AFTER
C                                 THE HEADER.
C                     --NMAX    = NUMBER OF CHARACTERS FOR "RULE" LINE
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/1
C     ORIGINAL VERSION--JANUARY   2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE(NHEAD)
      INTEGER NCHAR(NHEAD)
      INTEGER NCOLSP(NHEAD)
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      CHARACTER*255 IATEMP
C
      LOGICAL IFLAG1
      LOGICAL IFLAG2
C
      CHARACTER*20 IFORMT
      CHARACTER*160 ISTR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
CC999 FORMAT(1X)
C
C  STEP 1: PRINT INITIAL RULE LINE
C
      IF(NHEAD.GE.1)THEN
        IF(IFLAG1 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO8010I=1,MIN(NMAX,255)
            IATEMP(I:I)='-'
 8010     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
C  STEP 2: PRINT TEXT FIELDS
C
        IFORMT=' '
        NCSTR=0
        DO8020I=1,NHEAD
          IF(NCHAR(I).GE.1 .AND. NCOLSP(I).GT.0)THEN
            NCSTR=NCSTR+1
            NCSTR2=NCSTR+NCHAR(I)-1
            IFORMT(1:5)='(A  )'
            WRITE(IFORMT(3:4),'(I2)')NCHAR(I)
            WRITE(ISTR(NCSTR:NCSTR2),IFORMT)IVALUE(I)(1:NCHAR(I))
            NCSTR=NCSTR2
          ENDIF
 8020   CONTINUE
        IFORMT=' '
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NCSTR
        IF(NCSTR.GE.1)THEN
          WRITE(ICOUT,IFORMT)ISTR(1:NCSTR)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
C  STEP 3: PRINT TRAILING RULE LINE
C
        IF(IFLAG2 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO8030I=1,NMAX
            IATEMP(I:I)='-'
 8030     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAB5(IVALUE,NCHAR,AVALUE,NHEAD,IFLAG1,NMAX,NTOT,
     1                  ICSVWR)
C
C     PURPOSE--THIS ROUTINE IS A UTILITY ROUTINE FOR CREATING
C              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED TO
C              GENERATE A DATA ROW FOR A TABLE.  THE FIRST FIELD CAN
C              BE A TEXT VALUE (FOR A ROW LABEL).
C
C     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING CONTAINING
C                                 THE TEXT FOR THE FIRST COLUMN.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 FIRST TEXT FIELD.
C                     --AVALUE  = A REAL ARRAY CONTAINING THE DATA
C                                 TO BE GENERATED.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF NUMERIC VALUES.
C                     --IFLAG1  = A LOGICAL VALUE THAT SPECIFIES WHETHER
C                                 A RULE LINE WILL BE PRINTED AFTER THE
C                                 ROW
C                     --NMAX    = NUMBER OF CHARACTERS IN RULE LINE
C                     --NTOT    = AN INTEGER ARRAY CONTAINING THE TOTAL
C                                 NUMBER OF CHARACTERS IN EACH FIELD
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/3
C     ORIGINAL VERSION--MARCH     2009.
C     UPDATED         --APRIL     2009. ADDITIONAL FORMATTING OPTIONS
C     UPDATED         --APRIL     2015. SUPPORT HORIZONTAL ALIGNMENT
C                                       (LEFT, CENTER, RIGHT)
C     UPDATED         --FEBRUARY  2020. OPTION FOR WRITING CVS FILES
C                                       (FOR IMPORTING INTO OTHER
C                                       PROGRAMS)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE
      CHARACTER*4 ICSVWR
      REAL AVALUE(NHEAD)
      INTEGER NTOT(*)
      INTEGER NCHAR
C
      LOGICAL IFLAG1
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      CHARACTER*20  IFORMT
      CHARACTER*240 ISTR
      CHARACTER*255 IATEMP
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: PRINT ROW LABEL IF REQUESTED
C
      NCSTR=0
      ISTR=' '
      ICNT=0
      IF(NCHAR.GT.0)THEN
        ICNT=ICNT+1
        IF(ALIGN(ICNT).EQ.'l')THEN
          ISTR(NCSTR+1:NCSTR+NCHAR)=IVALUE(1:NCHAR)
          NCSTR=NCSTR+NCHAR
          IF(NTOT(ICNT).GT.NCHAR)THEN
            NCSTR=NCSTR+1
            NCSTR2=NCSTR+(NTOT(ICNT)-NCHAR)-1
            ISTR(NCSTR:NCSTR2)=' '
            NCSTR=NCSTR2
          ENDIF
        ELSEIF(ALIGN(ICNT).EQ.'r')THEN
          NSKIP=NTOT(ICNT) - NCHAR
          IF(NSKIP.GE.0)THEN
            ISTR(NSKIP+1:NSKIP+NCHAR)=IVALUE(1:NCHAR)
            NCSTR=NSKIP+NCHAR
          ELSE
            ISTR(1:NTOT(ICNT))=IVALUE(1:NTOT(ICNT))
            NCSTR=NTOT(ICNT)
          ENDIF
        ELSEIF(ALIGN(ICNT).EQ.'c')THEN
          NSKIP=NTOT(ICNT) - NCHAR
          NSKIP=NSKIP/2
          IF(NSKIP.GE.0)THEN
            ISTR(NSKIP+1:NSKIP+NCHAR)=IVALUE(1:NCHAR)
            NCSTR=NSKIP+NCHAR
          ELSE
            ISTR(1:NTOT(ICNT))=IVALUE(1:NTOT(ICNT))
            NCSTR=NTOT(ICNT)
          ENDIF
        ENDIF
        IF(ICSVWR.EQ.'ON')THEN
          NCSTR=NCSTR+1
          ISTR(NCSTR:NCSTR)=','
        ENDIF
      ENDIF
C
C     STEP 2: LOOP THROUGH THE NUMERIC FIELDS
C
C     APRIL 2009: SUPPORT THE FOLLOWING FORMATTING OPTIONS
C
C                  NUMDIG(I) > 0          => Fyy.xx FORMAT
C                  NUMDIG(I) = 0          => I12 FORMAT
C                  NUMDIG(I) = -1         => BLANK
C                  NUMDIG(I) = -2         => G15.7
C                  NUMDIG(I) = -3 to -20  => Eyy.xx
C                  NUMDIG(I) = -99        => '**'
C
      IF(NHEAD.GE.1)THEN
C
        DO8000I=1,NHEAD
          ICNT=ICNT+1
          IFORMT=' '
          ATEMP=AVALUE(I)
          IF(NUMDIG(I).GT.0)THEN
            NCHTOT=NTOT(ICNT)
            NCHDEC=NUMDIG(I)
            CALL GRTRR1(ATEMP,NCHTOT,NCHDEC,ISTR,NCSTR,ALIGN(ICNT))
          ELSEIF(NUMDIG(I).EQ.0)THEN
            IF(ATEMP.GE.0.0)THEN
              ITEMP=INT(ATEMP+0.5)
            ELSE
              ITEMP=INT(ATEMP-0.5)
            ENDIF
            NCHTOT=NTOT(ICNT)
            CALL GRTRI1(ITEMP,NCHTOT,ISTR,NCSTR,ALIGN(ICNT))
          ELSEIF(NUMDIG(I).EQ.-1)THEN
            NCSTR=NCSTR+1
            ISTR(NCSTR:NCSTR)=' '
          ELSEIF(NUMDIG(I).EQ.-2)THEN
            NCSTR=NCSTR+1
            NCSTR2=NCSTR+14
            WRITE(ISTR(NCSTR:NCSTR2),'(G15.7)')ATEMP
            NCSTR=NCSTR2
          ELSEIF(NUMDIG(I).LT.-2 .AND. NUMDIG(I).GT.-20)THEN
            IXX=ABS(NUMDIG(I))
            IYY=IXX+8
            NCSTR=NCSTR+1
            NCSTR2=NCSTR+IYY-1
            IFORMT='(E  .  )'
            WRITE(IFORMT(3:4),'(I2)')IYY
            WRITE(IFORMT(6:7),'(I2)')IXX
            WRITE(ISTR(NCSTR:NCSTR2),IFORMT)ATEMP
            NCSTR=NCSTR2
          ELSEIF(NUMDIG(I).EQ.-99)THEN
            NCHTOT=NTOT(ICNT)
            IF(NCHTOT.GT.2)THEN
              DO7010J=1,NCHTOT-2
                NCSTR=NCSTR+1
                ISTR(NCSTR:NCSTR)=' '
 7010         CONTINUE
            ENDIF
            NCSTR=NCSTR+1
            NCSTR2=NCSTR+1
            ISTR(NCSTR:NCSTR2)='**'
            NCSTR=NCSTR2
          ELSE
            NCSTR=NCSTR+1
            ISTR(NCSTR:NCSTR)=' '
          ENDIF
          IF(ICSVWR.EQ.'ON' .AND. I.LT.NHEAD)THEN
            NCSTR=NCSTR+1
            ISTR(NCSTR:NCSTR)=','
          ENDIF
 8000   CONTINUE
C
        IF(NCSTR.GE.1)THEN
          IFORMT='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NCSTR
          WRITE(ICOUT,IFORMT)ISTR(1:NCSTR)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
C       STEP 3: WRITE RULE LINE IF REQUESTED
C
        IF(IFLAG1 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO8030I=1,NMAX
            IATEMP(I:I)='-'
 8030     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAB6(IHEAD,NHEAD,CAPTN,NCAP,NMAX,IFLAG1,IFLAG2)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED
C              TO INITIATE THE TABULAR OUTPUT.  IT WILL OPTIONALLY
C              DRAW A RULE LINE BEFORE AND/OR AFTER THE TITLE.
C              IS THE CAPTION.  THIS IS A VARIANT OF DPTAB1 (THIS
C              ROUTINE ALLOWS THE RULE LINES).
C     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
C                                THE TEXT FOR THE HEADER
C                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                HEADER.
C                     --CAPTN  = THE CHARACTER STRING CONTAINING
C                                THE CAPTION.
C                     --NCAP   = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                CAPTION.
C                     --NMAX   = THE INTEGER NUMBER THAT SPECIFIES
C                                THE TOTAL NUMBER OF COLUMNS IN THE
C                                TABLE.
C                     --IFLAG1 = A LOGICAL PARAMETER THAT SPECIFIES
C                                WHETHER A RULE LINE IS DRAWN BEFORE
C                                THE TABLE HEADER.
C                     --IFLAG2 = A LOGICAL PARAMETER THAT SPECIFIES
C                                WHETHER A RULE LINE IS DRAWN AFTER
C                                THE TABLE HEADER.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/4
C     ORIGINAL VERSION--APRIL     2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) CAPTN
      CHARACTER*(*) IHEAD
C
      CHARACTER*132 IATEMP
C
      LOGICAL IFLAG1
      LOGICAL IFLAG2
      CHARACTER*10 IFORMT
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: WRITE A HEADER
C
  999 FORMAT(1X)
C
      IF(IFLAG1.AND.NMAX.GT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        IFORMT=' '
        DO8010I=1,MIN(NMAX,132)
          IATEMP(I:I)='-'
 8010   CONTINUE
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NMAX
        WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IF(NHEAD.GE.1)THEN
        IFORMT=' '
        IFORMT(1:9)='(12X,A  )'
        WRITE(IFORMT(7:8),'(I2)')NHEAD
        WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C  STEP 2: START TABLE AND DEFINE A CAPTION
C
      IF(NCAP.GT.0)THEN
        IFORMT=' '
        IFORMT(1:5)='(A  )'
        WRITE(IFORMT(3:4),'(I2)')NCAP
        WRITE(ICOUT,IFORMT)CAPTN(1:NCAP)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IF(IFLAG2.AND.NMAX.GT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        IFORMT=' '
        DO8090I=1,MIN(NMAX,132)
          IATEMP(I:I)='-'
 8090   CONTINUE
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NMAX
        WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTABY(IVALUE,NCHAR,AVALUE,NHEAD,ITYPE,
     1                  IFLAGA,IFLAGB,NMAX,NTOT,ICSVWR,IOUNI1,
     1                  IBUGA3,ISUBRO)
C
C     PURPOSE--THIS ROUTINE IS A UTILITY ROUTINE FOR CREATING
C              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED TO
C              GENERATE A DATA ROW FOR A TABLE WHERE THE FIELDS CAN
C              BE A MIX OF CHARACTER AND NUMERIC VALUES.
C
C     INPUT  ARGUMENTS--IVALUE  = AN ARRAY OF CHARACTER STRINGS.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 CHARACTER FIELDS.
C                     --AVALUE  = A REAL ARRAY CONTAINING THE DATA
C                                 TO BE GENERATED.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF NUMERIC VALUES.
C                     --ITYPE   = A CHARACTER ARRAY THAT SPECIFIES
C                                 WHICH FIELDS ARE NUMERIC AND
C                                 WHICH ARE CHARACTER.
C                     --IFLAGA  = GENERATE A SEPARATOR LINE AFTER THE
C                                 CURRENT LINE.
C                     --IFLAGB  = GENERATE A SEPARATOR LINE BEFORE THE
C                                 CURRENT LINE.
C                     --NMAX    = NUMBER OF CHARACTERS IN RULE LINE
C                     --NTOT    = AN INTEGER ARRAY CONTAINING THE TOTAL
C                                 NUMBER OF CHARACTERS IN EACH FIELD
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009.
C     UPDATED         --APRIL     2015. SUPPORT FOR HORIZONTAL ALIGNMENT
C                                       FOR Fx.x AND Ix FORMATS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE(*)
      CHARACTER*4 ITYPE(*)
      CHARACTER*4 ICSVWR
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      REAL AVALUE(NHEAD)
      INTEGER NCHAR(*)
      INTEGER NTOT(*)
C
      LOGICAL IFLAGA
      LOGICAL IFLAGB
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      CHARACTER*20  IFORMT
      CHARACTER*240 ISTR
      CHARACTER*132 IATEMP
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     STEP 1: LOOP THROUGH THE FIELDS
C
C     SUPPORT THE FOLLOWING FORMATTING OPTIONS FOR NUMERIC FIELDS
C
C           NUMDIG(I) > 0          => Fyy.xx FORMAT
C           NUMDIG(I) = 0          => I12 FORMAT
C           NUMDIG(I) = -1         => BLANK
C           NUMDIG(I) = -2         => G15.7
C           NUMDIG(I) = -3 to -20  => Eyy.xx
C           NUMDIG(I) = -99        => '**'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TABY')THEN
        WRITE(ICOUT,1001)NHEAD,NMAX
 1001   FORMAT('NHEAD,NMAX = ',2I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IF(NHEAD.GE.1)THEN
C
C       STEP 1: WRITE RULE LINE BEFORE CURRENT LINE IF REQUESTED
C
        IF(IFLAGB .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO7030I=1,NMAX
            IATEMP(I:I)='-'
 7030     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          IF(IOUNI1.GT.0)THEN
            WRITE(IOUNI1,IFORMT)IATEMP(1:NMAX)
          ELSE
            WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TABY')THEN
            WRITE(ICOUT,7031)
 7031       FORMAT('AFTER WRITE BORDER LINE')
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
        ENDIF
C
        ISTR=' '
        NCSTR=0
        ICNT=0
C
        DO8000I=1,NHEAD
          ICNT=ICNT+1
          IFORMT=' '
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TABY')THEN
            WRITE(ICOUT,8001)I,ICNT,NCSTR,ITYPE(I)
 8001       FORMAT('I,ICNT,NCSTR,ITYPE(I) = ',3I8,2X,A4)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,8002)I,AVALUE(I),NUMDIG(I)
 8002       FORMAT('I,AVALUE(I),NUMDIG(I) = ',I8,2X,G15.7,I8)
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
          IF(ITYPE(I).NE.'ALPH')THEN
            ATEMP=AVALUE(I)
            IF(NUMDIG(I).GT.0)THEN
              NCHTOT=NTOT(ICNT)
              NCHDEC=NUMDIG(I)
              CALL GRTRR1(ATEMP,NCHTOT,NCHDEC,ISTR,NCSTR,ALIGN(ICNT))
            ELSEIF(NUMDIG(I).EQ.0)THEN
              IF(ATEMP.GE.0.0)THEN
                ITEMP=INT(ATEMP+0.5)
              ELSE
                ITEMP=INT(ATEMP-0.5)
              ENDIF
              NCHTOT=NTOT(ICNT)
              CALL GRTRI1(ITEMP,NCHTOT,ISTR,NCSTR,ALIGN(ICNT))
            ELSEIF(NUMDIG(I).EQ.-1)THEN
              NJUNK=NTOT(I)
              NCSTR=NCSTR+1
              NCSTR2=NCSTR+NJUNK-1
              ISTR(NCSTR:NCSTR2)=' '
              NCSTR=NCSTR2
            ELSEIF(NUMDIG(I).EQ.-2)THEN
              NCSTR=NCSTR+1
              NCSTR2=NCSTR+14
              WRITE(ISTR(NCSTR:NCSTR2),'(G15.7)')ATEMP
              NCSTR=NCSTR2
            ELSEIF(NUMDIG(I).LT.-2 .AND. NUMDIG(I).GT.-20)THEN
              IXX=ABS(NUMDIG(I))
              IYY=IXX+8
              NCSTR=NCSTR+1
              NCSTR2=NCSTR+IYY-1
              IFORMT='(E  .  )'
              WRITE(IFORMT(3:4),'(I2)')IYY
              WRITE(IFORMT(6:7),'(I2)')IXX
              WRITE(ISTR(NCSTR:NCSTR2),IFORMT)ATEMP
              NCSTR=NCSTR2
            ELSEIF(NUMDIG(I).EQ.-99)THEN
              NCHTOT=NTOT(ICNT)
              IF(NCHTOT.GT.2)THEN
                DO7010J=1,NCHTOT-2
                  NCSTR=NCSTR+1
                  ISTR(NCSTR:NCSTR)=' '
 7010         CONTINUE
              ENDIF
              NCSTR=NCSTR+1
              NCSTR2=NCSTR+1
              ISTR(NCSTR:NCSTR2)='**'
              NCSTR=NCSTR2
            ELSE
              NCSTR=NCSTR+1
              ISTR(NCSTR:NCSTR)=' '
            ENDIF
C
C         CHARACTER FIELDS
C
          ELSE
C
            NTEMP=NCHAR(I)
            IF(NTEMP.GT.NTOT(I))NTEMP=NTOT(I)
            NCSTR=NCSTR+1
            NCSTR3=NCSTR+NTOT(I)-1
            ISTR(NCSTR:NCSTR3)=' '
C
            IF(NTEMP.GT.0)THEN
              IF(ALIGN(I).EQ.'l')THEN
                NCSTR2=NCSTR+NTEMP-1
                ISTR(NCSTR:NCSTR2)=IVALUE(ICNT)(1:NTEMP)
              ELSEIF(ALIGN(I).EQ.'c')THEN
                NBLANK=(NTOT(I)-NTEMP)/2
                NCSTR=NCSTR+NBLANK
                NCSTR2=NCSTR+NTEMP-1
                ISTR(NCSTR:NCSTR2)=IVALUE(ICNT)(1:NTEMP)
              ELSEIF(ALIGN(I).EQ.'r')THEN
                NBLANK=NTOT(I)-NTEMP
                NCSTR=NCSTR+NBLANK
                NCSTR2=NCSTR+NTEMP-1
                ISTR(NCSTR:NCSTR2)=IVALUE(ICNT)(1:NTEMP)
              ENDIF
            ENDIF
            NCSTR=NCSTR3
          ENDIF
C
          IF(ICSVWR.EQ.'ON' .AND. I.LT.NHEAD)THEN
            NCSTR=NCSTR+1
            ISTR(NCSTR:NCSTR)=','
          ENDIF
C
 8000   CONTINUE
C
        IF(NCSTR.GE.1)THEN
          IFORMT='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NCSTR
          IF(IOUNI1.GT.0)THEN
            WRITE(IOUNI1,IFORMT)ISTR(1:NCSTR)
          ELSE
            WRITE(ICOUT,IFORMT)ISTR(1:NCSTR)
            CALL DPWRST('XXX','WRIT')
          ENDIF
        ENDIF
C
C       STEP 3: WRITE RULE LINE AFTER CURRENT LINE IF REQUESTED
C
        IF(IFLAGA .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO8030I=1,NMAX
            IATEMP(I:I)='-'
 8030     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          IF(IOUNI1.GT.0)THEN
            WRITE(IOUNI1,IFORMT)IATEMP(1:NMAX)
          ELSE
            WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
            CALL DPWRST('XXX','WRIT')
          ENDIF
        ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAC2(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,YLEVEL,NLEVEL,
     1                  NUMV2,ICASCT,ICTNAM,ISTANR,
     1                  XIDTEM,XIDTE2,XIDTE3,XIDTE4,
     1                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1                  TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,
     1                  XACLOW,XACUPP,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,ICTAMV,PSTAMV,PCTAMV,ALPHA,IQUASE,
     1                  NCRTV,MAXOBV,PTPLXI,PTPLYI,ITPLDI,ITPLUN,
     1                  ITPLNI,ITPLCD,ITPLSO,ITPLSR,ITPLSC,
     1                  ITPLRM,ITPLCM,
     1                  Y,X,D,X3D,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN TABULATION PLOT
C
C              THIS SUPPORTS THE "CHARACTER" VARIANT.  IN THIS VARIANT, WE PLOT
C              THE VALUE OF THE STATISTIC (FOR RAW DATA, WE CAN USE THE MEAN AS
C              THE DESIRED STATISTIC).
C
C     DESCRIPTION--IN THE TABULATION PLOT, WE CROSS-TABULATE OVER
C                  1 TO 4 GROUP-ID VARIABLES (ANALAGOUS TO A
C                  FLUCTUATION PLOT).  WE DEFINE A GRID BASED ON THE
C                  THESE GROUP-ID VARIABLES.  THEN FOR THE RESPONSE
C                  VALUES CORRESPONDING TO A GIVEN SET OF THESE
C                  GROUP-ID VARIABLES, WE COMPUTE A USER-SPECIFED
C                  STATISTIC (THE DEFAULT IS THE MEAN).  THE VALUE
C                  OF THE STATISTIC IS THEN COMPARED TO SOME
C                  USER-SPECIFIED LEVELS (THESE ARE DEFINED IN THE
C                  YLEVEL VARIABLE).  A RECTANGLE IS DRAWN AND THE
C                  ATTRIBUTES (PRIMARILY FILL COLOR) ARE BASED ON
C                  THE VALUE OF THE STATISTIC RELATIVE TO YLEVEL.
C
C                  THIS PLOT IS USEFUL FOR VISUALLY IDENTIFYING
C                  AREAS WITH "HIGH" AND "LOW" VALUES OF THE
C                  STATISTIC ACROSS GROUPS.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2889
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/6
C     ORIGINAL VERSION--JUNE      2010. THIS VARIANT ADDED TO THE
C                                       TABULATION PLOT
C     UPDATED         --AUGUST    2010. ROW/COLUMN "MINMAX" OPTION
C                                       FOR TWO GROUP-ID VARIABLES CASE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*60 ICTNAM
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
      CHARACTER*4 ITPLDI
      CHARACTER*4 ITPLUN
      CHARACTER*4 ITPLCD
      CHARACTER*4 ITPLSO
      CHARACTER*4 ITPLSR
      CHARACTER*4 ITPLSC
      CHARACTER*4 ITPLCM
      CHARACTER*4 ITPLRM
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION YLEVEL(*)
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
C
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION TEMP6(*)
      DIMENSION TEMP7(*)
      DIMENSION TEMP8(*)
      DIMENSION TEMP9(*)
      DIMENSION TMP10(*)
      DIMENSION TMP11(*)
C
      DIMENSION ITEMP1(*)
      DIMENSION ITEMP2(*)
      DIMENSION ITEMP3(*)
      DIMENSION ITEMP4(*)
      DIMENSION ITEMP5(*)
      DIMENSION ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION X3D(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      COMMON/ITABC2/IADD
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTA'
      ISUBN2='C2  '
      IWRITE='OFF'
      IERROR='NO'
      IADD=0
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN CHARACTER TABULATION PLOT--')
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
        GOTO9000
      ENDIF
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPTAC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ICASCT,N,NUMV2,NCRTV,NLEVEL,ISTANR
   71   FORMAT('ICASCT,N,NUMV2,NCRTV,NLEVEL,ISTANR = ',A4,2X,5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)ICTNAM
   74   FORMAT('ICTNAM = ',A60)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,78)ITPLUN,ITPLNI,PTPLXI,PTPLYI
   78   FORMAT('ITPLUN,ITPLNI,PTPLXI,PTPLYI = ',A4,2X,I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO72I=1,N
          WRITE(ICOUT,73)I,Y1(I),Y2(I),TAG1(I),TAG2(I),TAG3(I),
     1                   TAG4(I)
   73     FORMAT('I,Y(I),Y2(I),TAG1-6(I) = ',I8,9F10.3)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
        IF(NLEVEL.GE.1)THEN
          DO82I=1,NLEVEL
            WRITE(ICOUT,83)I,YLEVEL(I)
   83       FORMAT('I,YLEVEL(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   82     CONTINUE
        ENDIF
      ENDIF
C
      IF(NLEVEL.GE.1)THEN
        CALL DISTIN(YLEVEL,NLEVEL,IWRITE,TEMP1,NTEMP,IBUGG3,IERROR)
        DO110I=1,NTEMP
          YLEVEL(I)=TEMP1(I)
  110   CONTINUE
        NLEVEL=NTEMP
        CALL SORT(YLEVEL,NLEVEL,YLEVEL)
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (TAG1, TAG2)            **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
C               **  WHICH IS AN ERROR CONDITION FOR A PLOT.         **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      IF(NCRTV.GE.2)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG2,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO920I=1,N
            TAG2(I)=TEMP1(I)
  920     CONTINUE
        ENDIF
        CALL DISTIN(TAG2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
      ENDIF
C
      IF(NCRTV.GE.3)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG3,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO930I=1,N
            TAG3(I)=TEMP1(I)
  930     CONTINUE
        ENDIF
        CALL DISTIN(TAG3,N,IWRITE,XIDTE3,NUMSE3,IBUGG3,IERROR)
        CALL SORT(XIDTE3,NUMSE3,XIDTE3)
      ELSE
        NUMSE3=0
      ENDIF
C
      IF(NCRTV.GE.4)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG4,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO940I=1,N
            TAG4(I)=TEMP1(I)
  940     CONTINUE
        ENDIF
        CALL DISTIN(TAG4,N,IWRITE,XIDTE4,NUMSE4,IBUGG3,IERROR)
        CALL SORT(XIDTE4,NUMSE4,XIDTE4)
      ELSE
        NUMSE4=0
      ENDIF
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
      IF(NCRTV.GE.2 .AND. (NUMSE2.LT.1 .OR. NUMSE2.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=2
        WRITE(ICOUT,111)ITEMP,NUMSE2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.3 .AND. (NUMSE3.LT.1 .OR. NUMSE3.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=3
        WRITE(ICOUT,111)ITEMP,NUMSE3
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.4 .AND. (NUMSE4.LT.1 .OR. NUMSE4.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=4
        WRITE(ICOUT,111)ITEMP,NUMSE4
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      AN=REAL(N)
      ANUMS1=REAL(NUMSE1)
      ANUMS2=REAL(NUMSE2)
      ANUMS3=REAL(NUMSE3)
      ANUMS4=REAL(NUMSE4)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAC2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,201)NUMSE1,NUMSE2,NUMSE3,NUMSE4
  201   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMSE1.GE.1)THEN
          DO210I=1,NUMSE1
            WRITE(ICOUT,211)I,XIDTEM(I)
  211       FORMAT('I,XIDTEM(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  210     CONTINUE
        ENDIF
C
        IF(NUMSE2.GE.1)THEN
          DO220I=1,NUMSE2
            WRITE(ICOUT,221)I,XIDTE2(I)
  221       FORMAT('I,XIDTE2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  220     CONTINUE
        ENDIF
      ENDIF
C
      IWRITE='OFF'
C
      IF(NCRTV.EQ.1)THEN
        CALL DPTAP0(Y1,Y2,Y3,TAG1,N,
     1              NUMV2,ICASCT,ISTANR,
     1              XIDTEM,
     1              NUMSE1,
     1              TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ALPHA,
     1              ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1              TEMP6,TEMP7,XACLOW,XACUPP,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  SET "X3D" TO VALUE
CCCCC   OF STATISTIC FOR EACH POINT.
C
        ICNT=0
C
        DO1001I=1,N2
          STAT=TEMP6(I)
          IF(ITPLDI.EQ.'X')THEN
            XVAL=TEMP7(I)
            YVAL=1.0
          ELSE
            YVAL=TEMP7(I)
            XVAL=1.0
          ENDIF
          XCOOR1=XVAL
          YCOOR1=YVAL
          IF(STAT.LT.YLEVEL(1))THEN
            ILEVEL=1
          ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
            ILEVEL=NLEVEL+1
          ELSE
            DO1003J=2,NLEVEL
              IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                ILEVEL=J
              ENDIF
 1003       CONTINUE
          ENDIF
C
          ICNT=ICNT+1
          X(ICNT)=XCOOR1
          Y(ICNT)=YCOOR1
          X3D(ICNT)=STAT
          D(ICNT)=REAL(ILEVEL)
C
 1001   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
C       WHEN THERE ARE EXACTLY TWO CROSS-TABULATION VARIABLES, THEN
C       SUPPORT A "SORT" OPTION.  FIRST NEED TO OBTAIN ROW AND COLUMN
C       VALUES FOR THE STATISTICS.  FROM THESE, CREATE "INDEX" VARIABLES.
C
      ELSEIF(NCRTV.EQ.2)THEN
C
C       SORT THE ROWS.  FOR THIS APPLICATION, NEED A RANK.  SINCE THE
C       RANK WILL SERVE AS AN ARRAY INDEX, NEED TO CHECK FOR TIES.
C
        IF(ITPLSO.EQ.'ON' .OR. ITPLSO.EQ.'ROW')THEN
          CALL DPTAP0(Y1,Y2,Y3,TAG1,N,
     1                NUMV2,ICASCT,ISTANR,
     1                XIDTEM,
     1                NUMSE1,
     1                TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1                ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                DTEMP1,DTEMP2,DTEMP3,
     1                ISEED,ALPHA,
     1                ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1                TEMP9,TEMP7,XACLOW,XACUPP,N2,
     1                ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TEMP9,NUMSE1,IWRITE,XIDTE3,TEMP7,ITEMP1,MAXOBV,
     1              IBUGG3,IERROR)
          CALL DISTIN(XIDTE3,NUMSE1,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE1)THEN
            DO1006II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1006       CONTINUE
          ENDIF
          IF(ITPLSR.EQ.'DESC')THEN
            DO2006I=1,N
              IRANK=INT(XIDTE3(I)+0.1)
              IRANK2=NUMSE1 - IRANK + 1
              XIDTE3(I)=REAL(IRANK2)
 2006       CONTINUE
          ENDIF
        ELSE
          IF(ITPLSR.EQ.'DESC')THEN
            DO3007II=1,NUMSE1
              IVAL=NUMSE1 - II + 1
              XIDTE3(II)=XIDTEM(IVAL)
 3007       CONTINUE
          ELSE
            DO1007II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1007       CONTINUE
          ENDIF
        ENDIF
C
C       SORT THE COLUMNS
C
        IF(ITPLSO.EQ.'ON' .OR. ITPLSO.EQ.'COLU')THEN
          CALL DPTAP0(Y1,Y2,Y3,TAG2,N,
     1                NUMV2,ICASCT,ISTANR,
     1                XIDTE2,
     1                NUMSE2,
     1                TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1                ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                DTEMP1,DTEMP2,DTEMP3,
     1                ISEED,ALPHA,
     1                ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1                TMP10,TEMP7,XACLOW,XACUPP,N2,
     1                ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TMP10,NUMSE2,IWRITE,XIDTE4,TEMP7,ITEMP1,MAXOBV,
     1              IBUGG3,IERROR)
          CALL DISTIN(XIDTE4,NUMSE2,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE2)THEN
            DO1008II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1008       CONTINUE
          ENDIF
          IF(ITPLSC.EQ.'DESC')THEN
            DO2008I=1,N
              IRANK=INT(XIDTE4(I)+0.1)
              IRANK2=NUMSE2 - IRANK + 1
              XIDTE4(I)=REAL(IRANK2)
 2008       CONTINUE
          ENDIF
        ELSE
          IF(ITPLSR.EQ.'DESC')THEN
            DO3008II=1,NUMSE2
              IVAL=NUMSE2 - II + 1
              XIDTE4(II)=XIDTE2(IVAL)
 3008       CONTINUE
          ELSE
             DO1009II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1009       CONTINUE
          ENDIF
        ENDIF
C
        CALL DPTAP3(Y1,Y2,Y3,TAG1,TAG2,N,
     1              NUMV2,ICASCT,ISTANR,
     1              XIDTEM,XIDTE2,
     1              NUMSE1,NUMSE2,
     1              TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1              TMP10,TMP11,ITPLCM,ITPLRM,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ALPHA,
     1              ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1              TEMP6,TEMP7,TEMP8,XACLOW,XACUPP,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.
C
        ICNT=0
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
          WRITE(ICOUT,1011)N2,ITPLSO,ITPLDI
 1011     FORMAT('DPTAC2 AFTER CALL DPTAP3: N2,ITPLSO,ITPLDI = ',
     1           I8,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
          DO1012I=1,NUMSE1
            WRITE(ICOUT,1013)I,XIDTE3(I)
 1013       FORMAT('I,XIDTE3(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 1012     CONTINUE
          DO1014I=1,NUMSE2
            WRITE(ICOUT,1015)I,XIDTE4(I)
 1015       FORMAT('I,XIDTE4(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 1014     CONTINUE
        ENDIF
C
        DO1010I=1,N2
          STAT=TEMP6(I)
C
          IF(ITPLDI.EQ.'X')THEN
            INDEXX=INT(TEMP7(I)+0.1)
            INDEXY=INT(TEMP8(I)+0.1)
            XVAL=XIDTE3(INDEXX)
            YVAL=XIDTE4(INDEXY)
          ELSE
CCCCC       INDEXX=INT(TEMP8(I)+0.1)
CCCCC       INDEXY=INT(TEMP7(I)+0.1)
CCCCC       XVAL=XIDTE4(INDEXX)
CCCCC       YVAL=XIDTE3(INDEXY)
            INDEXX=INT(TEMP8(I)+0.1)
            INDEXY=INT(TEMP7(I)+0.1)
            XVAL=XIDTE4(INDEXX)
            YVAL=XIDTE3(INDEXY)
          ENDIF
C
          XCOOR1=XVAL
          YCOOR1=YVAL
          ILEVEL=-99
          IF(NLEVEL.GE.1)THEN
            IF(STAT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1016J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1016         CONTINUE
            ENDIF
          ENDIF
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
            WRITE(ICOUT,1017)I,STAT,INDEXX,XVAL,XCOOR1,
     1                       INDEXY,YVAL,YCOOR1
 1017       FORMAT('I,STAT,INDEXX,XVAL,XCOOR1,INDEXY,YVAL,YCOOR1 = ',
     1             I8,G15.7,2(I6,2F12.5))
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          ICNT=ICNT+1
          X(ICNT)=XCOOR1
          Y(ICNT)=YCOOR1
          X3D(ICNT)=STAT
          D(ICNT)=REAL(ILEVEL)
C
 1010   CONTINUE
C
C       IF REQUESTED, FLAG COLUMN/ROW MIN/MAX POINTS
C
        IF(ITPLCM.EQ.'OFF' .AND. ITPLRM.EQ.'OFF')GOTO5099
C
C       PERFORM DUPLICATION OF ARRAYS FIRST (ADD MIN/MAX PART
C       AT END)
C
        IF(ICNT.GT.0)THEN
          DO5010I=1,ICNT
            ICNT=ICNT+1
            X(ICNT)=X(I)
            Y(ICNT)=Y(I)
            X3D(ICNT)=X3D(I)
            D(ICNT)=D(I) + REAL(NLEVEL+1)
 5010     CONTINUE
        ENDIF
        CALL MAXIM(D,ICNT,IWRITE,DMAX,IBUGG3,IERROR)
C
        IF(ITPLDI.EQ.'Y')THEN
          IADD=0
C
          IF(ITPLCM.EQ.'ON')THEN
            IADD=IADD+1
            DO5020I=1,N2
              IF(TMP10(I).EQ.1.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(2*(NLEVEL+1)+IADD)
              ENDIF
              IF(TMP10(I).EQ.2.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(2*(NLEVEL+1)+IADD+1)
              ENDIF
 5020       CONTINUE
          ENDIF
C
          IF(ITPLRM.EQ.'ON')THEN
            IADD=IADD+1
            DO5030I=1,N2
              IF(TMP11(I).EQ.1.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(2*(NLEVEL+1)+IADD)
              ENDIF
              IF(TMP11(I).EQ.2.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(2*(NLEVEL+1)+IADD+1)
              ENDIF
 5030       CONTINUE
            IADD=IADD+1
          ENDIF
C
        ELSEIF(ITPLDI.EQ.'X')THEN
          IADD=0
C
          IF(ITPLRM.EQ.'ON')THEN
            IADD=IADD+1
            DO5040I=1,N2
              IF(TMP11(I).EQ.1.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(NLEVEL+1+IADD)
              ENDIF
              IF(TMP11(I).EQ.2.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(NLEVEL+1+IADD+1)
              ENDIF
 5040       CONTINUE
            IADD=IADD+1
          ENDIF
C
          IF(ITPLCM.EQ.'ON')THEN
            IADD=IADD+1
            DO5050I=1,N2
              IF(TMP10(I).EQ.1.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(NLEVEL+1+IADD)
              ENDIF
              IF(TMP10(I).EQ.2.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(NLEVEL+1+IADD+1)
              ENDIF
 5050       CONTINUE
            IADD=IADD+1
          ENDIF
C
          NPLOTP=ICNT
          NPLOTV=2
          GOTO9000
C
        ENDIF
C
        NPLOTP=ICNT
        NPLOTV=2
        GOTO9000
C
 5099   CONTINUE
        NPLOTP=ICNT
        NPLOTV=2
C
      ELSEIF(NCRTV.EQ.3)THEN
        CALL DPTAP4(Y1,Y2,Y3,TAG1,TAG2,TAG3,N,
     1              NUMV2,ICASCT,ISTANR,
     1              XIDTEM,XIDTE2,XIDTE3,
     1              NUMSE1,NUMSE2,NUMSE3,
     1              TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ALPHA,
     1              ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1              TEMP6,TEMP7,TEMP8,TEMP9,XACLOW,XACUPP,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.
C
        ICNT=0
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
          WRITE(ICOUT,1021)N2
 1021     FORMAT('DPTAC2: AFTER CALL DPTAP4--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO1020I=1,N2
          STAT=TEMP6(I)
          IF(ITPLDI.EQ.'X')THEN
            XVAL=TEMP7(I)
            YVAL=TEMP8(I)
            XVAL2=TEMP9(I)
            XCOOR1=XVAL + XVAL2/REAL(NUMSE3)
            YCOOR1=YVAL
          ELSE
            YVAL=TEMP7(I)
            XVAL=TEMP8(I)
            YVAL2=TEMP9(I)
            XCOOR1=XVAL
            YCOOR1=YVAL + YVAL2/REAL(NUMSE3)
          ENDIF
          IF(STAT.LT.YLEVEL(1))THEN
            ILEVEL=1
          ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
            ILEVEL=NLEVEL+1
          ELSE
            DO1025J=2,NLEVEL
              IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                ILEVEL=J
              ENDIF
 1025       CONTINUE
          ENDIF
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
            WRITE(ICOUT,1026)I,STAT,YVAL,XVAL,YVAL2
 1026       FORMAT('I,STAT,YVAL,XVAL,YVAL2 = ',I8,4G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1027)XCOOR1,YCOOR1,ILEVEL
 1027       FORMAT('XCOOR1,YCOOR1,ILEVEL = ',2G15.7,I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1028)ILEVEL
 1028       FORMAT('ILEVEL = ',I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          ICNT=ICNT+1
          X(ICNT)=XCOOR1
          Y(ICNT)=YCOOR1
          X3D(ICNT)=STAT
          D(ICNT)=REAL(ILEVEL)
C
 1020   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ELSEIF(NCRTV.EQ.4)THEN
        CALL DPTAP5(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,
     1              NUMV2,ICASCT,ISTANR,
     1              XIDTEM,XIDTE2,XIDTE3,XIDTE4,
     1              NUMSE1,NUMSE2,NUMSE3,NUMSE4,
     1              TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ALPHA,
     1              ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1              TEMP6,TEMP7,TEMP8,TEMP9,TMP10,XACLOW,XACUPP,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.
C
        ICNT=0
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
          WRITE(ICOUT,1031)N2
 1031     FORMAT('DPTAC2: AFTER CALL DPTAP5--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO1030I=1,N2
          STAT=TEMP6(I)
          IF(ITPLDI.EQ.'X')THEN
            XVAL=TEMP7(I)
            YVAL=TEMP8(I)
            XVAL2=TEMP9(I)
            YVAL2=TMP10(I)
          ELSE
            YVAL=TEMP7(I)
            XVAL=TEMP8(I)
            YVAL2=TEMP9(I)
            XVAL2=TMP10(I)
          ENDIF
          XCOOR1=XVAL + XVAL2/REAL(NUMSE3)
          YCOOR1=YVAL + YVAL2/REAL(NUMSE4)
          YCOOR2=YCOOR1 + YINC2
          IF(STAT.LT.YLEVEL(1))THEN
            ILEVEL=1
          ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
            ILEVEL=NLEVEL+1
          ELSE
            DO1035J=2,NLEVEL
              IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                ILEVEL=J
              ENDIF
 1035       CONTINUE
          ENDIF
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
            WRITE(ICOUT,1036)I,STAT,YVAL,XVAL,YVAL2,XVAL2
 1036       FORMAT('I,STAT,YVAL,XVAL,YVAL2,XVAL2 = ',I8,5G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1037)XCOOR1,YCOOR1,ILEVEL
 1037       FORMAT('XCOOR1,YCOOR1,ILEVEL = ',2G15.7,I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          ICNT=ICNT+1
          X(ICNT)=XCOOR1
          Y(ICNT)=YCOOR1
          X3D(ICNT)=STAT
          D(ICNT)=REAL(ILEVEL)
C
 1030   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ENDIF
C
C     NOW DUPLICATE ARRAYS
C
      IF(NPLOTP.GT.0)THEN
        DO2010I=1,NPLOTP
          NPLOTP=NPLOTP+1
          X(NPLOTP)=X(I)
          Y(NPLOTP)=Y(I)
          X3D(NPLOTP)=X3D(I)
CCCCC     D(NPLOTP)=D(I) + REAL(NLEVEL+1+IADD+1)
          D(NPLOTP)=D(I) + REAL(NLEVEL+1+IADD)
 2010   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NPLOTP,NPLOTV,IERROR
 9012   FORMAT('ICASCT,N,NPLOTP,NPLOTV,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,NPLOTP
          WRITE(ICOUT,9036)I,Y(I),X(I),X3D(I),D(I)
 9036     FORMAT('I,Y(I),X(I),X3D(I),D(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAIL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN (EMPIRICAL) TAIL AREA PLOT
C              (A SYNONYM IS SURVIVAL PLOT)
C              VERTICAL AXIS   = 1-F(X)  (ON A LOG10 SCALE)
C              HORIZONTAL AXIS = SORTED DATA
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
C     ORIGINAL VERSION--MAY       1989.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --APRIL     1992. MAXCP31 TO MAXCP6
C     UPDATED         --JANUARY   2012. USE DPPARS
C     UPDATED         --JANUARY   2012. SUPPORT FOR MULTIPLE AND
C                                       REPLICATION OPTIONS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICASE
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
      DIMENSION Y1(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION ZY1(MAXOBV)
      DIMENSION XDESGN(MAXOBV,2)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB7),ZY1(1))
      EQUIVALENCE (GARBAG(IGARB8),XDESGN(1,1))
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
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPTA'
      ISUBN2='IL  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTAIL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,MAXCOL
   52   FORMAT('ICASPL,IAND1,IAND2 = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C
C               **********************************
C               **  TREAT THE TAIL AREA PLOT    **
C               **  =     THE SURVIVAL PLOT     **
C               **********************************
C
C               *******************************************
C               **  STEP 1--                             **
C               **  SEARCH FOR TAIL AREA PLOT            **
C               **  OR SURVIVAL PLOT                     **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASPL='TAIL'
C
      IF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
        IF((IHARG(1).EQ.'TAIL' .OR. IHARG(1).EQ.'SURV') .AND.
     1     IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
        ELSEIF(IHARG(1).EQ.'TAIL' .AND. IHARG(2).EQ.'AREA' .AND.
     1     IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
        ENDIF
      ELSEIF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
        IF((IHARG(1).EQ.'TAIL' .OR. IHARG(1).EQ.'SURV') .AND.
     1     IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
        ELSEIF(IHARG(1).EQ.'TAIL' .AND. IHARG(2).EQ.'AREA' .AND.
     1     IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
        ENDIF
      ELSEIF((ICOM.EQ.'TAIL' .OR. ICOM.EQ.'SURV') .AND.
     1  IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
      ELSEIF(ICOM.EQ.'TAIL' .AND. IHARG(1).EQ.'AREA' .AND.
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='TAIL AREA PLOT'
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
      MAXNVA=1
      IF(IREPL.EQ.'ON')THEN
        MINNVA=MINNVA+1
        MAXNVA=MAXNVA+2
        IFLAGM=0
      ELSEIF(IMULT.EQ.'ON')THEN
        MINNVA=1
        MAXNVA=MAXSPN
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')THEN
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
  101     FORMAT('***** ERROR IN TAIL ERROR PLOT--')
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
C               **  GENERATE THE TAIL AREA      PLOTS FOR **
C               **  THE VARIOUS CASES.                    **
C               ********************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NREPL.EQ.0)THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
        NPLOTP=0
        DO810IRESP=1,NRESP
          NCURVE=IRESP
C
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SPEC')THEN
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
     1                Y1,Y1,Y1,NS,NS,NS,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 8B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               *****************************************************
C
          CALL DPTAI2(Y1,NS,NCURVE,ICASPL,MAXN,
     1                Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
  810   CONTINUE
C
C               *****************************************************
C               **  STEP 9A--                                      **
C               **  CASE 3: ONE OR TWO  REPLICATION VARIABLES.     **
C               **          FOR THIS CASE, THE NUMBER OF RESPONSE  **
C               **          VARIABLES MUST BE EXACTLY 1.           **
C               *****************************************************
C
      ELSEIF(NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
          ICOLC=1
C
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
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,931)
  931     FORMAT('***** FROM THE MIDDLE  OF DPSPEC--')
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
                ZY1(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTAI2(ZY1,NTEMP,NCURVE,ICASPL,MAXN,
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
                ZY1(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTAI2(ZY1,NTEMP,NCURVE,ICASPL,MAXN,
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAIL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
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
      SUBROUTINE DPTAI2(Y1,N,NCURVE,ICASPL,MAXN,
     1                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN (EMPIRICAL) TAIL AREA PLOT
C              (A SYNONYM IS SURVIVAL PLOT)
C              VERTICAL AXIS   = 1-F(X)  (ON A LOG10 SCALE)
C              HORIZONTAL AXIS = SORTED DATA
C     INPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED) OBSERVATIONS
C                               FOR THE FIRST  VARIABLE.
C                      N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X.
C     CAUTION--THE INPUT VARIABLE Y1(.) WILL BE CHANGED HEREIN
C              (IT WILL BE SORTED)
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
      DIMENSION Y1(*)
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTA'
      ISUBN2='I2  '
      IERROR='NO'
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTAI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,IERROR
   52   FORMAT('IBUGG3,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,MAXN,NCURVE,ICASPL
   53   FORMAT('N,MAXN,NCURVE,ICASPL = ',3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,Y1(I)
   56     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN TAIL AREA PLOT--')
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
        GOTO9000
      ENDIF
C
      HOLD=Y1(1)
      DO120I=1,N
      IF(Y1(I).NE.HOLD)GOTO129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,122)HOLD
  122 FORMAT('      ALL ELEMENTS IN THE RESPONSE VARIABLE ARE ',
     1       'IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  129 CONTINUE
C
C               ***********************************************
C               **  STEP 12--                                **
C               **  COMPUTE COORDINATES FOR TAIL AREA PLOT   **
C               **  (INCORPORATE STAIR-STEP APPEARANCE)      **
C               **  NOTE--THE LOGGING OF THE 1-F(X) WILL     **
C               **        NOTE BE DONE HEREIN BUT WILL       **
C               **        BE DONE IN THE UNDERLYING          **
C               **        GRAPHICS BY LOG SCALE              **
C               ***********************************************
C
C
      CALL SORT(Y1,N,Y1)
C
      ANP1=N+1
      J=0
      DO1100I=1,N
        ARG1=N-I+1
        ARG2=N-I
        J=J+1
        X(J+NPLOTP)=Y1(I)
        Y(J+NPLOTP)=ARG1/ANP1
        D(J+NPLOTP)=REAL(NCURVE)
        IF(I.GE.N)GOTO1100
        J=J+1
        X(J+NPLOTP)=Y1(I)
        Y(J+NPLOTP)=ARG2/ANP1
        D(J+NPLOTP)=REAL(NCURVE)
 1100 CONTINUE
      NPLOTP=NPLOTP+J
      NPLOTV=2
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAI2--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N
          WRITE(ICOUT,9016)I,Y1(I)
 9016     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9021)NPLOTP,NPLOTV,IERROR
 9021   FORMAT('NPLOTP,NPLOTV,IERROR = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9022I=1,NPLOTP
          WRITE(ICOUT,9023)I,Y(I),X(I),D(I)
 9023     FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9022   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAP0(Y,Z,Z2,TAG1,N,
     1                  NUMV2,ICASCT,ISTANR,
     1                  XIDTEM,
     1                  NUMSE1,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,ALPHA,
     1                  ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1                  Y2,X2,XACLOW,XACUPP,N2,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A ONE-WAY TABULATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009.
C     UPDATED         --DECEMBER  2009. UNCERTAINTY OPTION FOR
C                                       BINOMIAL PROBABILITY, MEAN AND
C                                       MEDIAN CONFIDENCE INTERVAL
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
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
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
C
      DIMENSION TAG1(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTA'
      ISUBN2='P0  '
      IWRITE='OFF'
C
      I2=0
C
      AN=INT(N+0.01)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      NRESP=NUMV2-1
      DO1110ISET1=1,NUMSE1
C
        K=0
        DO1130I=1,N
          IF(XIDTEM(ISET1).EQ.TAG1(I))GOTO1131
          GOTO1130
 1131     CONTINUE
C
          K=K+1
          TEMP(K)=0.0
          TEMPZ(K)=0.0
          TEMPZ2(K)=0.0
          IF(ISTANR.GE.1)TEMP(K)=Y(I)
          IF(ISTANR.GE.2)TEMPZ(K)=Z(I)
          IF(ISTANR.GE.3)TEMPZ2(K)=Z2(I)
 1130   CONTINUE
        NTEMP=K
C
        NTRIAL=0
        ALOWLM=0.0
        AUPPLM=0.0
        IF(NTEMP.EQ.0)THEN
          IF(ICTAMV.EQ.'ZERO')THEN
            STAT=0.0
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1         ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
              NTRIAL=0
              ALOWLM=0.0
              AUPPLM=0.0
            ENDIF
          ELSEIF(ICTAMV.EQ.'MV  ')THEN
            STAT=PCTAMV
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1         ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
              NTRIAL=0
              ALOWLM=PCTAMV
              AUPPLM=PCTAMV
            ENDIF
          ELSE
            GOTO1110
          ENDIF
        ELSE
          CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
            PTEMP=STAT
            NTRIAL=NTEMP
            IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              ALPHAT=ALPHA
              IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
              CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                    ALOWLM,AUPPLM,IBUGG3,IERROR)
            ENDIF
          ELSEIF(ICASCT.EQ.'MECL')THEN
            XMEAN=STAT
            NTRIAL=NTEMP
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
              ALPHAT=ALPHA
              CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                    ALOWLM,AUPPLM,IBUGG3,IERROR)
            ENDIF
          ELSEIF(ICASCT.EQ.'MDCL')THEN
            XMED=STAT
            NTRIAL=NTEMP
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              XQ=0.5
              CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                    QUASE,IBUGG3,IERROR)
              ALPHAT=ALPHA
              CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                    ALOWLM,AUPPLM,IBUGG3,IERROR)
            ENDIF
          ENDIF
        ENDIF
C
        J=J+1
        Y2(J)=STAT
        X2(J)=XIDTEM(ISET1)
        IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1     ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
          IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
          IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
          XACLOW(J)=ALOWLM
          XACUPP(J)=AUPPLM
        ENDIF
C
 1110 CONTINUE
      N2=J
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP0')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAP0--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NUMV2,IERROR
 9012   FORMAT('ICASCT,N,NUMV2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMSE1,N2
 9015   FORMAT('NUMSE1,N2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I)
 9021     FORMAT('I,Y2(I),X2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A TABULATION PLOT.
C
C              THIS IS SOMEWHAT SIMILAR TO A FLUCTUATION PLOT.
C              HOWEVER, INSTEAD OF A FILLED BAR BASED ON THE
C              VALUE OF A STATISTIC, WE COLOR CODE BASED ON
C              THE LEVEL OF THE RESPONSE VARIABLE (I.E.,
C              LIKE SPECIFYING THE LEVELS IN A CONTOUR PLOT).
C              WE CURRENTLY SUPPORT THIS PLOT FOR ONE-WAY THROUGH
C              FOUR-WAY TABLES.
C
C                  X1  = CATEGORY LEVEL FOR VARIABLE 1
C                  X2  = CATEGORY LEVEL FOR VARIABLE 2
C                  X3  = CATEGORY LEVEL FOR VARIABLE 3
C                  X4  = CATEGORY LEVEL FOR VARIABLE 4
C
C              NOTE THAT WE EXTENED THE TABULATION PLOT TO ALLOW
C              ANY OF DATAPLOT'S SUPPORTED STATISTICS TO BE
C              PLOTTED (THE DEFAULT IS THE MEAN).
C
C     EXAMPLES--TABULATION PLOT Y X1 X2 ZLEVEL
C             --TABULATION PLOT Y X1 X2 X3 ZLEVEL
C             --TABULATION PLOT Y X1 X2 X3 X4 ZLEVEL
C             --TABULATION PLOT TABLE ZLEVEL
C             --MEAN TABULATION PLOT Y X1 X2 ZLEVEL
C             --SD TABULATION PLOT Y X1 X2 ZLEVEL
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009.
C     UPDATED         --JUNE      2010. ADD "CHARACTER TABULATION PLOT"
C                                       CASE.  THIS IS A VARIANT THAT
C                                       PLOTS THE NUMERICAL VALUE OF THE
C                                       STATISTIC RATHER THAN A COLORED
C                                       RECTANGLE
C     UPDATED         --SEPTEMBER 2016. SUPPORT FOR MATRIX ARGUMENTS
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
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
      CHARACTER*4 ICASCT
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASE
      CHARACTER*4 ISTADF
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      PARAMETER (MAXSPN=20)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
      CHARACTER*40 INAME
C
      CHARACTER*8 IYNAM
      CHARACTER*8 IXNAM
      CHARACTER*8 IX1NAM
      CHARACTER*8 IX2NAM
      CHARACTER*8 IX3NAM
      CHARACTER*8 IX4NAM
      CHARACTER*60 ICTNAM
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION YLEVEL(MAXOBV)
C
      DIMENSION XH1DIS(MAXOBV)
      DIMENSION XH2DIS(MAXOBV)
      DIMENSION XH3DIS(MAXOBV)
      DIMENSION XH4DIS(MAXOBV)
C
      DIMENSION X1(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION X4(MAXOBV)
C
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION TEMP7(MAXOBV)
      DIMENSION TEMP8(MAXOBV)
      DIMENSION TEMP9(MAXOBV)
      DIMENSION TMP10(MAXOBV)
      DIMENSION TMP11(MAXOBV)
C
      DIMENSION XACLOW(MAXOBV)
      DIMENSION XACUPP(MAXOBV)
C
      DIMENSION ITEMP1(MAXOBV)
      DIMENSION ITEMP2(MAXOBV)
      DIMENSION ITEMP3(MAXOBV)
      DIMENSION ITEMP4(MAXOBV)
      DIMENSION ITEMP5(MAXOBV)
      DIMENSION ITEMP6(MAXOBV)
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),YLEVEL(1))
      EQUIVALENCE (GARBAG(IGARB4),X1(1))
      EQUIVALENCE (GARBAG(IGARB5),X2(1))
      EQUIVALENCE (GARBAG(IGARB6),X3(1))
      EQUIVALENCE (GARBAG(IGARB7),X4(1))
      EQUIVALENCE (GARBAG(IGARB8),XH1DIS(1))
      EQUIVALENCE (GARBAG(IGARB9),XH2DIS(1))
      EQUIVALENCE (GARBAG(IGAR10),XH3DIS(1))
      EQUIVALENCE (GARBAG(JGAR11),XH4DIS(1))
      EQUIVALENCE (GARBAG(JGAR12),TEMP1(1))
      EQUIVALENCE (GARBAG(JGAR13),TEMP2(1))
      EQUIVALENCE (GARBAG(JGAR14),TEMP3(1))
      EQUIVALENCE (GARBAG(JGAR15),TEMP4(1))
      EQUIVALENCE (GARBAG(JGAR16),TEMP5(1))
      EQUIVALENCE (GARBAG(JGAR17),TEMP6(1))
      EQUIVALENCE (GARBAG(JGAR18),TEMP7(1))
      EQUIVALENCE (GARBAG(JGAR19),TEMP8(1))
      EQUIVALENCE (GARBAG(JGAR20),TEMP9(1))
      EQUIVALENCE (GARBAG(IGAR11),TMP10(1))
      EQUIVALENCE (GARBAG(IGAR12),XACLOW(1))
      EQUIVALENCE (GARBAG(IGAR13),XACUPP(1))
      EQUIVALENCE (GARBAG(IGAR14),Y3(1))
      EQUIVALENCE (GARBAG(IGAR15),TMP11(1))
C
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
C
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPTA'
      ISUBN2='PL  '
C
      IYNAM=' '
      IXNAM=' '
      IX1NAM=' '
      IX2NAM=' '
      IX3NAM=' '
      IX4NAM=' '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      MAXV2=7
      MINN2=2
      J2=0
C
C               ****************************************
C               **  TREAT THE TABULATION PLOT CASE    **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTAPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN,NS
   53   FORMAT('ICASPL,IAND1,IAND2,NS = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ****************************************************
C               **  STEP 1.5--                                    **
C               **  SEARCH FOR TABULATION <STAT> PLOT             **
C               **  SEARCH FOR CHARACTER TABULATION <STAT> PLOT   **
C               ****************************************************
C
      ICASCT=' '
C
      IF(NUMARG.LE.1)GOTO9000
      IF(ICOM.EQ.'TABU')THEN
        ICASPL='TABU'
        JMIN=1
      ELSEIF(ICOM.EQ.'CHAR' .AND. IHARG(1).EQ.'TABU')THEN
        ICASPL='TABC'
        JMIN=2
      ELSE
        GOTO9000
      ENDIF
C
CCCCC USE "EXTSTA" TO PARSE.  NOTE THAT IF NO STATISTIC IS GIVEN,
CCCCC  WE ASSUME THE "MEAN" CASE.
C
      JMAX=MIN(NUMARG,JMIN+6)
      DO200I=JMIN,JMAX
        IF(IHARG(I).EQ.'PLOT')THEN
          JMAX=I-1
          ILASTC=I
          GOTO209
        ENDIF
  200 CONTINUE
      IFOUND='NO'
      GOTO9000
  209 CONTINUE
C
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,
     1            ICASCT,ICTNAM,ISTANR,ISTADF,IFOUND,ILOCV,
     1            ISUBRO,IBUGG3,IERROR)
C
      IF(IFOUND.EQ.'NO')THEN
        ICTNAM='NUMBER'
        ILOCV=2
        IFOUND='YES'
      ENDIF
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2016/09: ALLOW MATRIX ARGUMENTS
C
      INAME='TABULATION PLOT'
      MINNA=1
      MAXNA=100
      MAXVAR=100
      MINN2=2
      IFLAGE=99
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=7
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXVAR,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,251)
  251   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,252)NQ,NUMVAR
  252   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO255I=1,NUMVAR
            WRITE(ICOUT,257)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  257       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  255     CONTINUE
        ENDIF
      ENDIF
C
C     IF MATRIX ARGUMENTS GIVEN, THEN ALL RESPONSES MUST BE MATRICES
C     AND ALL MATRICES MUST HAVE SAME DIMENSION.
C
      IFLAGM=0
      DO260I=1,NUMVAR
        IF(IVARTY(I).EQ.'MATR')IFLAGM=1
  260 CONTINUE
C
      IF(IFLAGM.EQ.1)THEN
C
        NRESP=ISTANR
        NLVARI=1
        IF(ICASPL.EQ.'TABC')NLVARI=0
        NCRTV=2
C
        DO291I=1,NRESP
          IF(IVARTY(I).NE.'MATR')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,311)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,292)
  292       FORMAT('      IF ONE RESPONSE VARIABLE IS A MATRIX, ',
     1             'THEN ALL MUST BE MATRICES.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,293)I
  293       FORMAT('      RESPONSE VARIABLE ',I5,' IS NOT A MATRIX.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSE
             ILISR=ILIS(I)
             NRTEMP=IN(ILISR)
             ICOL1=IVALUE(ILISR)
             ICOL2=IVALU2(ILISR)
             NCTEMP=(ICOL2 - ICOL1) + 1
             IF(I.EQ.1)THEN
               NROW=NRTEMP
               NCOL=NCTEMP
             ELSE
               IF(NRTEMP.NE.NROW .OR. NCTEMP.NE.NCOL)THEN
                 WRITE(ICOUT,999)
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,311)
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,296)
  296            FORMAT('      FOR MATRIX RESPONSE VARIABLES, THE ',
     1                  'ROW AND COLUMN DIMENSIONS MUST BE EQUAL.')
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,297)NROW,NCOL
  297            FORMAT('      THE FIRST MATRIX HAS ',I5,' ROWS AND ',
     1                  I5,' COLUMNS.')
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,298)I,NRTEMP,NCTEMP
  298            FORMAT('      MATRIX ',I2,' HAS ',I5,' ROWS AND ',
     1                  I5,' COLUMNS.')
                 CALL DPWRST('XXX','BUG ')
                 IERROR='YES'
                 GOTO9000
               ENDIF
             ENDIF
          ENDIF
  291   CONTINUE
C
        NTEMP=NRESP + NLVARI
        IF(NTEMP.NE.NUMVAR)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,311)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,272)
  272     FORMAT('      WHEN MATRIX ARGUMENTS ARE GIVEN, THE ',
     1           'NUMBER OF MATRICES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,274)
  274     FORMAT('      MUST BE THE SAME AS THE NUMBER OF RESPONSE ',
     1           'VARIABLES FOR THE SELECTED STATISTIC.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,276)NUMVAR-NLVARI
  276     FORMAT('      THE NUMBER OF MATRICES ENTERED = ',I5)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,278)ISTANR
  278     FORMAT('      THE NUMBER OF MATRICES EXPECTED = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        GOTO400
C
      ENDIF
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  CHECK FOR ALLOWABLE NUMBER OF CROSS TABULATION  **
C               **  VARIABLES.                                      **
C               ******************************************************
C
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     FOR "CHARACTER TABULATION" CASE, THE "LEVELS" VARIABLE IS OPTIONAL.
C     IF LAST VARIABLE HAS SAME NUMBER OF OBSERVATIONS AS FIRST VARIABLE,
C     ASSUME NO "LEVEL" VARIABLE GIVEN.
C
      NRESP=ISTANR
      NLVARI=1
      IF(ICASPL.EQ.'TABC' .AND. NRIGHT(1).EQ.NRIGHT(NUMVAR)) NLVARI=0
      NCRTV=NUMVAR - NRESP - NLVARI
C
      IF(NCRTV.LT.1 .OR. NCRTV.GT.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,311)
  311   FORMAT('***** ERROR IN TABULATION PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)
  312   FORMAT('      THE NUMBER OF CROSS TABULATION VARIABLES MUST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,313)
  313   FORMAT('      BE BETWEEN 1 AND 4.  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,314)NCRTV
  314   FORMAT('      THE SPECIFIED NUMBER OF CROSS TABULATION ',
     1         'VARIABLES WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,318)(IANS(I),I=1,MIN(80,IWIDTH))
  318     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************************************
C               **  STEP 4--                                        **
C               **  CREATE THE VARIABLES                            **
C               ******************************************************
C
  400 CONTINUE
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFLAGM.EQ.1)THEN
        ICOL=1
        CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,X1,X2,NLOCAL,
     1              IBUGG2,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(NRESP.GE.2)THEN
          ICOL=2
          CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y2,X3,X4,N2,
     1                IBUGG2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF(NRESP.GE.3)THEN
          ICOL=3
          CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y3,X3,X4,N3,
     1                IBUGG2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        GOTO499
      ENDIF
C
      J=0
      IMAX=NRIGHT(1)
      IF(NQ.LT.NRIGHT(1))IMAX=NQ
      DO410I=1,IMAX
        IF(ISUB(I).EQ.0)GOTO410
        J=J+1
C
        IJ=MAXN*(ICOLR(1)-1)+I
        IF(ISTANR.LT.1)THEN
          Y1(J)=0.0
        ELSE
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
        ENDIF
C
        IJ=MAXN*(ICOLR(2)-1)+I
        IF(ISTANR.LT.2)THEN
          Y2(J)=0.0
        ELSE
          IF(ICOLR(2).LE.MAXCOL)Y2(J)=V(IJ)
          IF(ICOLR(2).EQ.MAXCP1)Y2(J)=PRED(I)
          IF(ICOLR(2).EQ.MAXCP2)Y2(J)=RES(I)
          IF(ICOLR(2).EQ.MAXCP3)Y2(J)=YPLOT(I)
          IF(ICOLR(2).EQ.MAXCP4)Y2(J)=XPLOT(I)
          IF(ICOLR(2).EQ.MAXCP5)Y2(J)=X2PLOT(I)
          IF(ICOLR(2).EQ.MAXCP6)Y2(J)=TAGPLO(I)
        ENDIF
C
        IJ=MAXN*(ICOLR(3)-1)+I
        IF(ISTANR.LT.3)THEN
          Y3(J)=0.0
        ELSE
          IF(ICOLR(3).LE.MAXCOL)Y3(J)=V(IJ)
          IF(ICOLR(3).EQ.MAXCP1)Y3(J)=PRED(I)
          IF(ICOLR(3).EQ.MAXCP2)Y3(J)=RES(I)
          IF(ICOLR(3).EQ.MAXCP3)Y3(J)=YPLOT(I)
          IF(ICOLR(3).EQ.MAXCP4)Y3(J)=XPLOT(I)
          IF(ICOLR(3).EQ.MAXCP5)Y3(J)=X2PLOT(I)
          IF(ICOLR(3).EQ.MAXCP6)Y3(J)=TAGPLO(I)
        ENDIF
C
        ICNT=ISTANR+1
        IF(NCRTV.GE.1)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X1(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X1(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X1(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X1(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X1(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X1(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X1(J)=TAGPLO(I)
        ELSE
          X1(J)=0.0
        ENDIF
C
        ICNT=ISTANR+2
        IF(NCRTV.GE.2)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X2(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X2(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X2(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X2(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X2(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X2(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X2(J)=TAGPLO(I)
        ELSE
          X2(J)=0.0
        ENDIF
C
        ICNT=ISTANR+3
        IF(NCRTV.GE.3)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X3(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X3(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X3(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X3(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X3(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X3(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X3(J)=TAGPLO(I)
        ELSE
          X3(J)=0.0
        ENDIF
C
        ICNT=ISTANR+4
        IF(NCRTV.GE.4)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X4(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X4(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X4(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X4(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X4(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X4(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X4(J)=TAGPLO(I)
        ELSE
          X4(J)=0.0
        ENDIF
C
  410 CONTINUE
      NLOCAL=J
C
  499 CONTINUE
C
      IF(NLVARI.GE.1)THEN
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
  490   CONTINUE
        NLEVEL=J2
      ELSE
        YLEVEL(J2)=CPUMIN
        NLEVEL=-99
      ENDIF
C
C               *************************************
C               **  STEP 5--                       **
C               **  GENERATE THE TABULATION PLOT   **
C               *************************************
C
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,6001)NLOCAL,NLEVEL,ICASPL
 6001   FORMAT('NLOCAL,NLEVEL,ICASPL=',2I8,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1   ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
        IHP='ALPH'
        IHP2='A   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1              NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,
     1              ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          ALPHA=0.05
        ELSE
          ALPHA=VALUE(ILOCP)
          IF(ALPHA.LE.0.0)ALPHA=0.05
          IF(ALPHA.GE.1.0)ALPHA=0.05
        ENDIF
      ELSE
        ALPHA=0.05
      ENDIF
C
      IF(ICASPL.EQ.'TABU')THEN
        CALL DPTAP2(Y1,Y2,Y3,X1,X2,X3,X4,NLOCAL,YLEVEL,NLEVEL,
     1              NUMVAR,ICASCT,ICTNAM,ISTANR,
     1              XH1DIS,XH2DIS,XH3DIS,XH4DIS,
     1              TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1              TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,
     1              XACLOW,XACUPP,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ICTAMV,PSTAMV,PCTAMV,ALPHA,IQUASE,
     1              NCRTV,MAXOBV,PTPLXI,PTPLYI,ITPLDI,ITPLUN,
     1              ITPLNI,ITPLCD,
     1              ITPLSO,ITPLSR,ITPLSC,
     1              ITPLRM,ITPLCM,
     1              Y,X,D,DCOLOR,
     1              NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
      ELSE
        CALL DPTAC2(Y1,Y2,Y3,X1,X2,X3,X4,NLOCAL,YLEVEL,NLEVEL,
     1              NUMVAR,ICASCT,ICTNAM,ISTANR,
     1              XH1DIS,XH2DIS,XH3DIS,XH4DIS,
     1              TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1              TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,
     1              XACLOW,XACUPP,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ICTAMV,PSTAMV,PCTAMV,ALPHA,IQUASE,
     1              NCRTV,MAXOBV,PTPLXI,PTPLYI,ITPLDI,ITPLUN,
     1              ITPLNI,ITPLCD,ITPLSO,ITPLSR,ITPLSC,
     1              ITPLRM,ITPLCM,
     1              Y,X,D,X3D,
     1              NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG2,IBUGG3,IBUGQ,ISUBRO
 9012   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)NLOCAL
 9041   FORMAT('NLOCAL = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NLOCAL.GE.1 .AND. ICASE.EQ.'VARI')THEN
          DO9042I=1,NLOCAL
            WRITE(ICOUT,9043)I,Y1(I),Y2(I)
 9043       FORMAT('I,Y1(I),Y2(I) = ',I8,2E15.7)
            CALL DPWRST('XXX','BUG ')
 9042     CONTINUE
        ENDIF
        WRITE(ICOUT,9051)NPLOTP
 9051   FORMAT('NPLOTP = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9052I=1,NPLOTP
            WRITE(ICOUT,9053)I,Y(I),X(I),D(I),DCOLOR(I)
 9053       FORMAT('I,Y(I),X(I),D(I),DCOLOR(I),',I8,4F12.5)
            CALL DPWRST('XXX','BUG ')
 9052     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAP2(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,YLEVEL,NLEVEL,
     1                  NUMV2,ICASCT,ICTNAM,ISTANR,
     1                  XIDTEM,XIDTE2,XIDTE3,XIDTE4,
     1                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1                  TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,
     1                  XACLOW,XACUPP,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,ICTAMV,PSTAMV,PCTAMV,ALPHA,IQUASE,
     1                  NCRTV,MAXOBV,PTPLXI,PTPLYI,ITPLDI,ITPLUN,
     1                  ITPLNI,ITPLCD,ITPLSO,ITPLSR,ITPLSC,
     1                  ITPLRM,ITPLCM,
     1                  Y,X,D,DCOLOR,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN TABULATION PLOT
C     DESCRIPTION--IN THE TABULATION PLOT, WE CROSS-TABULATE OVER
C                  1 TO 4 GROUP-ID VARIABLES (ANALAGOUS TO A
C                  FLUCTUATION PLOT).  WE DEFINE A GRID BASED ON THE
C                  THESE GROUP-ID VARIABLES.  THEN FOR THE RESPONSE
C                  VALUES CORRESPONDING TO A GIVEN SET OF THESE
C                  GROUP-ID VARIABLES, WE COMPUTE A USER-SPECIFED
C                  STATISTIC (THE DEFAULT IS THE MEAN).  THE VALUE
C                  OF THE STATISTIC IS THEN COMPARED TO SOME
C                  USER-SPECIFIED LEVELS (THESE ARE DEFINED IN THE
C                  YLEVEL VARIABLE).  A RECTANGLE IS DRAWN AND THE
C                  ATTRIBUTES (PRIMARILY FILL COLOR) ARE BASED ON
C                  THE VALUE OF THE STATISTIC RELATIVE TO YLEVEL.
C
C                  THIS PLOT IS USEFUL FOR VISUALLY IDENTIFYING
C                  AREAS WITH "HIGH" AND "LOW" VALUES OF THE
C                  STATISTIC ACROSS GROUPS.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2889
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009.
C     UPDATED         --DECEMBER  2009. SUPPORT FOR "UNCERTAINTY" OPTION
C                                       FOR BINOMIAL PROBABILITIES
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C     UPDATED         --JANUARY   2010. OPTION TO LEAVE AXIS VARIABLES
C                                       UNCODED
C     UPDATED         --JUNE      2010. SUPPORT FOR "SORTED" OPTION FOR
C                                       THE TWO GROUP-ID VARIABLE CASE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*60 ICTNAM
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
      CHARACTER*4 ITPLDI
      CHARACTER*4 ITPLUN
      CHARACTER*4 ITPLCD
      CHARACTER*4 ITPLSO
      CHARACTER*4 ITPLSR
      CHARACTER*4 ITPLSC
      CHARACTER*4 ITPLRM
      CHARACTER*4 ITPLCM
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION YLEVEL(*)
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
C
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION TEMP6(*)
      DIMENSION TEMP7(*)
      DIMENSION TEMP8(*)
      DIMENSION TEMP9(*)
      DIMENSION TMP10(*)
      DIMENSION TMP11(*)
C
      DIMENSION ITEMP1(*)
      DIMENSION ITEMP2(*)
      DIMENSION ITEMP3(*)
      DIMENSION ITEMP4(*)
      DIMENSION ITEMP5(*)
      DIMENSION ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION DCOLOR(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTA'
      ISUBN2='P2  '
      IWRITE='OFF'
      IERROR='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN TABULATION PLOT--')
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
        GOTO9000
      ENDIF
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPTAP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ICASCT,N,NUMV2,NCRTV,NLEVEL,ISTANR
   71   FORMAT('ICASCT,N,NUMV2,NCRTV,NLEVEL,ISTANR = ',A4,2X,5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)ICTNAM
   74   FORMAT('ICTNAM = ',A60)
        CALL DPWRST('XXX','BUG ')
        DO72I=1,N
          WRITE(ICOUT,73)I,Y1(I),Y2(I),TAG1(I),TAG2(I),TAG3(I),
     1                   TAG4(I)
   73     FORMAT('I,Y(I),Y2(I),TAG1-6(I) = ',I8,9F10.3)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
        DO82I=1,NLEVEL
          WRITE(ICOUT,83)I,YLEVEL(I)
   83     FORMAT('I,YLEVEL(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   82   CONTINUE
      ENDIF
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
C               **  FOR THE GROUP VARIABLES (TAG1, TAG2)            **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
C               **  WHICH IS AN ERROR CONDITION FOR A PLOT.         **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      IF(NCRTV.GE.2)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG2,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO920I=1,N
            TAG2(I)=TEMP1(I)
  920     CONTINUE
        ENDIF
        CALL DISTIN(TAG2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
      ENDIF
C
      IF(NCRTV.GE.3)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG3,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO930I=1,N
            TAG3(I)=TEMP1(I)
  930     CONTINUE
        ENDIF
        CALL DISTIN(TAG3,N,IWRITE,XIDTE3,NUMSE3,IBUGG3,IERROR)
        CALL SORT(XIDTE3,NUMSE3,XIDTE3)
      ELSE
        NUMSE3=0
      ENDIF
C
      IF(NCRTV.GE.4)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG4,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO940I=1,N
            TAG4(I)=TEMP1(I)
  940     CONTINUE
        ENDIF
        CALL DISTIN(TAG4,N,IWRITE,XIDTE4,NUMSE4,IBUGG3,IERROR)
        CALL SORT(XIDTE4,NUMSE4,XIDTE4)
      ELSE
        NUMSE4=0
      ENDIF
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
      IF(NCRTV.GE.2 .AND. (NUMSE2.LT.1 .OR. NUMSE2.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=2
        WRITE(ICOUT,111)ITEMP,NUMSE2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.3 .AND. (NUMSE3.LT.1 .OR. NUMSE3.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=3
        WRITE(ICOUT,111)ITEMP,NUMSE3
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.4 .AND. (NUMSE4.LT.1 .OR. NUMSE4.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=4
        WRITE(ICOUT,111)ITEMP,NUMSE4
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      AN=REAL(N)
      ANUMS1=REAL(NUMSE1)
      ANUMS2=REAL(NUMSE2)
      ANUMS3=REAL(NUMSE3)
      ANUMS4=REAL(NUMSE4)
C
C     FOR THE BINOMIAL PROPORTION, MEAN CONFIDENCE LIMIT, AND
C     MEDIAN CONFIDENCE LIMIT, INSTEAD OF A SINGLE SHADED RECTANGLE,
C     DEFINE "ITPLNI" INTERVALS THAT WILL BE SHADED FROM LOWEST
C     CONFIDENCE VALUE TO HIGHEST CONFIDENCE VALUE.
C
      IFLAGU=0
      IF((ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1    ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT') .AND.
     1    ITPLUN.EQ.'ON')THEN
        IFLAGU=1
      ENDIF
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      IF(NCRTV.EQ.1)THEN
        CALL DPTAP0(Y1,Y2,Y3,TAG1,N,
     1              NUMV2,ICASCT,ISTANR,
     1              XIDTEM,
     1              NUMSE1,
     1              TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ALPHA,
     1              ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1              TEMP6,TEMP7,XACLOW,XACUPP,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE A RECTANGLE
CCCCC   FOR EACH POINT.
CCCCC
C
        XINC=0.5 - PTPLXI
        YINC=0.5 - PTPLYI
        ICNT=0
        ICNT2=0
C
        IF(IFLAGU.EQ.1)THEN
          DO2000I=1,N2
            STAT=TEMP6(I)
            STATMN=XACLOW(I)
            STATMX=XACUPP(I)
            IF(ITPLDI.EQ.'X')THEN
              XVAL=TEMP7(I)
              YVAL=1.0
            ELSE
              YVAL=TEMP7(I)
              XVAL=1.0
            ENDIF
C
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
C
C           DIVIDE RECTANGLE INTO "ITPLNI" VERTICAL INCREMENTS AND
C           COMPUTE LEVEL-COLOR INDEPENDENTLY FOR EACH OF THESE
C           MINI-RECTANGLES.
C
            STATIN=(STATMX - STATMN)/REAL(ITPLNI)
            STATZ=STATMN - STATIN
            AINC=(YCOOR2 - YCOOR1)/REAL(ITPLNI)
            YCZ2=YCOOR1
C
            DO2009IROW=1,ITPLNI
C
              YCZ1=YCZ2
              YCZ2=YCZ1 + AINC
C
              STATZ=STATZ + STATIN
              IF(STATZ.LT.YLEVEL(1))THEN
                ILEVEL=1
              ELSEIF(STATZ.GE.YLEVEL(NLEVEL))THEN
                ILEVEL=NLEVEL+1
              ELSE
                DO2005J=2,NLEVEL
                  IF(STATZ.GE.YLEVEL(J-1) .AND. STATZ.LT.YLEVEL(J))THEN
                    ILEVEL=J
                  ENDIF
 2005           CONTINUE
              ENDIF
C
              IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
                WRITE(ICOUT,2006)I,IROW,STAT,STATZ,STATMN,STATMX,STATIN
 2006           FORMAT('I,IROW,STAT,STATZ,STATMN,STATMX,STATIN = ',
     1                 2I8,5G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2007)XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2
 2007           FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2 = ',
     1                 6G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2008)IROW,ILEVEL
 2008           FORMAT('IROW,ILEVEL = ',2I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
 2009       CONTINUE
C
 2000     CONTINUE
        ELSE
          DO1001I=1,N2
            STAT=TEMP6(I)
            IF(ITPLDI.EQ.'X')THEN
              XVAL=TEMP7(I)
              YVAL=1.0
            ELSE
              YVAL=TEMP7(I)
              XVAL=1.0
            ENDIF
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
            IF(STAT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1005J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1005       CONTINUE
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
 1001     CONTINUE
       ENDIF
C
        NPLOTP=ICNT
        NPLOTV=2
C
C       WHEN THERE ARE EXACTLY TWO CROSS-TABULATION VARIABLES, THEN
C       SUPPORT A "SORT" OPTION.  FIRST NEED TO OBTAIN ROW AND COLUMN
C       VALUES FOR THE STATISTICS.  FROM THESE, CREATE "INDEX" VARIABLES.
C
      ELSEIF(NCRTV.EQ.2)THEN
C
C       SORT THE ROWS.  FOR THIS APPLICATION, NEED A RANK.  SINCE THE
C       RANK WILL SERVE AS AN ARRAY INDEX, NEED TO CHECK FOR TIES.
C
        IF(ITPLSO.EQ.'ON' .OR. ITPLSO.EQ.'ROW')THEN
          CALL DPTAP0(Y1,Y2,Y3,TAG1,N,
     1                NUMV2,ICASCT,ISTANR,
     1                XIDTEM,
     1                NUMSE1,
     1                TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1                ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                DTEMP1,DTEMP2,DTEMP3,
     1                ISEED,ALPHA,
     1                ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1                TEMP9,TEMP7,XACLOW,XACUPP,N2,
     1                ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TEMP9,NUMSE1,IWRITE,XIDTE3,TEMP7,ITEMP1,MAXOBV,
     1              IBUGG3,IERROR)
          CALL DISTIN(XIDTE3,NUMSE1,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE1)THEN
            DO1006II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1006       CONTINUE
          ENDIF
          IF(ITPLSR.EQ.'DESC')THEN
            DO4006I=1,N
              IRANK=INT(XIDTE3(I)+0.1)
              IRANK2=NUMSE1 - IRANK + 1
              XIDTE3(I)=REAL(IRANK2)
 4006       CONTINUE
          ENDIF
        ELSE
          IF(ITPLSR.EQ.'DESC')THEN
            DO4007II=1,NUMSE1
              IVAL=NUMSE1 - II + 1
              XIDTE3(II)=XIDTEM(IVAL)
 4007       CONTINUE
          ELSE
            DO1007II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1007       CONTINUE
          ENDIF
        ENDIF
C
C       SORT THE COLUMNS
C
        IF(ITPLSO.EQ.'ON' .OR. ITPLSO.EQ.'COLU')THEN
          CALL DPTAP0(Y1,Y2,Y3,TAG2,N,
     1                NUMV2,ICASCT,ISTANR,
     1                XIDTE2,
     1                NUMSE2,
     1                TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1                ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                DTEMP1,DTEMP2,DTEMP3,
     1                ISEED,ALPHA,
     1                ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1                TMP10,TEMP7,XACLOW,XACUPP,N2,
     1                ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TMP10,NUMSE2,IWRITE,XIDTE4,TEMP7,ITEMP1,MAXOBV,
     1              IBUGG3,IERROR)
          CALL DISTIN(XIDTE4,NUMSE2,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE2)THEN
            DO1008II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1008       CONTINUE
          ENDIF
          IF(ITPLSC.EQ.'DESC')THEN
            DO4008I=1,N
              IRANK=INT(XIDTE4(I)+0.1)
              IRANK2=NUMSE2 - IRANK + 1
              XIDTE4(I)=REAL(IRANK2)
 4008       CONTINUE
          ENDIF
        ELSE
          IF(ITPLSR.EQ.'DESC')THEN
            DO5008II=1,NUMSE2
              IVAL=NUMSE2 - II + 1
              XIDTE4(II)=XIDTE2(IVAL)
 5008       CONTINUE
          ELSE
             DO1009II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1009       CONTINUE
          ENDIF
        ENDIF
C
        CALL DPTAP3(Y1,Y2,Y3,TAG1,TAG2,N,
     1              NUMV2,ICASCT,ISTANR,
     1              XIDTEM,XIDTE2,
     1              NUMSE1,NUMSE2,
     1              TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1              TMP10,TMP11,ITPLRM,ITPLCM,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ALPHA,
     1              ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1              TEMP6,TEMP7,TEMP8,XACLOW,XACUPP,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE A RECTANGLE
CCCCC   FOR EACH POINT.
C
        ICNT=0
        ICNT2=0
        XINC=0.5 - PTPLXI
        YINC=0.5 - PTPLYI
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
          WRITE(ICOUT,1011)N2
 1011     FORMAT('DPTAP2: AFTER CALL DPTAP3--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1012)XINC,YINC
 1012     FORMAT('XINC,YINC = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IFLAGU.EQ.1)THEN
          DO2010I=1,N2
            STAT=TEMP6(I)
            STATMN=XACLOW(I)
            STATMX=XACUPP(I)
CCCCC       JUNE 2010: MODIFIED TO ACCOUNT FOR SORTING
CCCCC       IF(ITPLDI.EQ.'X')THEN
CCCCC         XVAL=TEMP7(I)
CCCCC         YVAL=TEMP8(I)
CCCCC       ELSE
CCCCC         YVAL=TEMP7(I)
CCCCC         XVAL=TEMP8(I)
CCCCC       ENDIF
            IF(ITPLSO.EQ.'OFF' .AND. ITPLCD.EQ.'OFF')THEN
              IF(ITPLDI.EQ.'X')THEN
                XVAL=TEMP7(I)
                YVAL=TEMP8(I)
              ELSE
                XVAL=TEMP8(I)
                YVAL=TEMP7(I)
              ENDIF
            ELSE
              IF(ITPLDI.EQ.'X')THEN
                INDEXX=INT(TEMP7(I)+0.1)
                INDEXY=INT(TEMP8(I)+0.1)
                XVAL=XIDTE3(INDEXX)
                YVAL=XIDTE4(INDEXY)
              ELSE
                INDEXX=INT(TEMP8(I)+0.1)
                INDEXY=INT(TEMP7(I)+0.1)
                XVAL=XIDTE4(INDEXX)
                YVAL=XIDTE3(INDEXY)
              ENDIF
            ENDIF
C
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
C
C           DIVIDE RECTANGLE INTO "ITPLNI" VERTICAL INCREMENTS AND
C           COMPUTE LEVEL-COLOR INDEPENDENTLY FOR EACH OF THESE
C           MINI-RECTANGLES.
C
            STATIN=(STATMX - STATMN)/REAL(ITPLNI)
            STATZ=STATMN - STATIN
            AINC=(YCOOR2 - YCOOR1)/REAL(ITPLNI)
            YCZ2=YCOOR1
C
            DO2019IROW=1,ITPLNI
C
              YCZ1=YCZ2
              YCZ2=YCZ1 + AINC
C
              STATZ=STATZ + STATIN
              IF(STATZ.LT.YLEVEL(1))THEN
                ILEVEL=1
              ELSEIF(STATZ.GE.YLEVEL(NLEVEL))THEN
                ILEVEL=NLEVEL+1
              ELSE
                DO2015J=2,NLEVEL
                  IF(STATZ.GE.YLEVEL(J-1) .AND. STATZ.LT.YLEVEL(J))THEN
                    ILEVEL=J
                  ENDIF
 2015           CONTINUE
              ENDIF
C
              IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
                WRITE(ICOUT,2016)I,IROW,STAT,STATZ,STATMN,STATMX,STATIN
 2016           FORMAT('I,IROW,STAT,STATZ,STATMN,STATMX,STATIN = ',
     1                 2I8,5G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2017)XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2
 2017           FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2 = ',
     1                 6G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2018)IROW,ILEVEL
 2018           FORMAT('IROW,ILEVEL = ',2I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
 2019       CONTINUE
C
 2010     CONTINUE
        ELSE
          DO1010I=1,N2
            STAT=TEMP6(I)
CCCCC       JUNE 2010: ACCOUNT FOR SORTING
CCCCC       IF(ITPLDI.EQ.'X')THEN
CCCCC         XVAL=TEMP7(I)
CCCCC         YVAL=TEMP8(I)
CCCCC       ELSE
CCCCC         YVAL=TEMP7(I)
CCCCC         XVAL=TEMP8(I)
CCCCC       ENDIF
            IF(ITPLSO.EQ.'OFF' .AND. ITPLCD.EQ.'OFF')THEN
              IF(ITPLDI.EQ.'X')THEN
                XVAL=TEMP7(I)
                YVAL=TEMP8(I)
              ELSE
                XVAL=TEMP8(I)
                YVAL=TEMP7(I)
              ENDIF
            ELSE
              IF(ITPLDI.EQ.'X')THEN
                INDEXX=INT(TEMP7(I)+0.1)
                INDEXY=INT(TEMP8(I)+0.1)
                XVAL=XIDTE3(INDEXX)
                YVAL=XIDTE4(INDEXY)
              ELSE
                INDEXX=INT(TEMP8(I)+0.1)
                INDEXY=INT(TEMP7(I)+0.1)
                XVAL=XIDTE4(INDEXX)
                YVAL=XIDTE3(INDEXY)
              ENDIF
            ENDIF
C
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
              DO1015J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1015         CONTINUE
            ENDIF
C
            IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
              WRITE(ICOUT,1016)I,STAT,YVAL,XVAL
 1016         FORMAT('I,STAT,YVAL,XVAL = ',I8,3G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1017)XCOOR1,XCOOR2,YCOOR1,YCOOR2
 1017         FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2 = ',4G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1018)ILEVEL
 1018         FORMAT('ILEVEL = ',I8)
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
 1010     CONTINUE
        ENDIF
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ELSEIF(NCRTV.EQ.3)THEN
        CALL DPTAP4(Y1,Y2,Y3,TAG1,TAG2,TAG3,N,
     1              NUMV2,ICASCT,ISTANR,
     1              XIDTEM,XIDTE2,XIDTE3,
     1              NUMSE1,NUMSE2,NUMSE3,
     1              TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ALPHA,
     1              ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1              TEMP6,TEMP7,TEMP8,TEMP9,XACLOW,XACUPP,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE A RECTANGLE
CCCCC   FOR EACH POINT.
C
        ICNT=0
        ICNT2=0
        XINC=0.5 - PTPLXI
        YINC=0.5 - PTPLYI
        YINC2=2.0*YINC/REAL(NUMSE3)
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
          WRITE(ICOUT,1021)N2
 1021     FORMAT('DPTAP2: AFTER CALL DPTAP2--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1022)XINC,YINC,YINC2
 1022     FORMAT('XINC,YINC,YINC2 = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IFLAGU.EQ.1)THEN
          DO2020I=1,N2
            STAT=TEMP6(I)
            STATMN=XACLOW(I)
            STATMX=XACUPP(I)
            IF(ITPLDI.EQ.'X')THEN
CCCCC         XVAL=TEMP7(I)
CCCCC         YVAL=TEMP8(I)
CCCCC         XVAL2=TEMP9(I)
              XVAL=TEMP8(I)
              YVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              XCOOR1=XVAL - XINC
              XCOOR2=XVAL + XINC
              YCOOR1=YVAL - YINC
              YCOOR2=YVAL + YINC
            ELSE
CCCCC         YVAL=TEMP7(I)
CCCCC         XVAL=TEMP8(I)
CCCCC         YVAL2=TEMP9(I)
              XCOOR1=XVAL - XINC
              XCOOR2=XVAL + XINC
              YCOOR1=YVAL - YINC
              YCOOR2=YVAL + YINC
            ENDIF
C
C           DIVIDE RECTANGLE INTO "ITPLNI" VERTICAL INCREMENTS AND
C           COMPUTE LEVEL-COLOR INDEPENDENTLY FOR EACH OF THESE
C           MINI-RECTANGLES.
C
            STATIN=(STATMX - STATMN)/REAL(ITPLNI)
            STATZ=STATMN - STATIN
            AINC=(YCOOR2 - YCOOR1)/REAL(ITPLNI)
            YCZ2=YCOOR1
C
            DO2029IROW=1,ITPLNI
C
              YCZ1=YCZ2
              YCZ2=YCZ1 + AINC
C
              STATZ=STATZ + STATIN
              IF(STATZ.LT.YLEVEL(1))THEN
                ILEVEL=1
              ELSEIF(STATZ.GE.YLEVEL(NLEVEL))THEN
                ILEVEL=NLEVEL+1
              ELSE
                DO2025J=2,NLEVEL
                  IF(STATZ.GE.YLEVEL(J-1) .AND. STATZ.LT.YLEVEL(J))THEN
                    ILEVEL=J
                  ENDIF
 2025           CONTINUE
              ENDIF
C
              IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
                WRITE(ICOUT,2026)I,IROW,STAT,STATZ,STATMN,STATMX,STATIN
 2026           FORMAT('I,IROW,STAT,STATZ,STATMN,STATMX,STATIN = ',
     1                 2I8,5G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2027)XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2
 2027           FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2 = ',
     1                 6G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2028)IROW,ILEVEL
 2028           FORMAT('IROW,ILEVEL = ',2I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
 2029       CONTINUE
C
 2020     CONTINUE
        ELSE
          DO1020I=1,N2
            STAT=TEMP6(I)
            IF(ITPLDI.EQ.'X')THEN
CCCCC         XVAL=TEMP7(I)
CCCCC         YVAL=TEMP8(I)
CCCCC         XVAL2=TEMP9(I)
              XVAL=TEMP8(I)
              YVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              XCOOR1=XVAL - XINC
              XCOOR2=XVAL + XINC
              YCOOR1=YVAL - YINC
              YCOOR2=YVAL + YINC
            ELSE
CCCCC         YVAL=TEMP7(I)
CCCCC         XVAL=TEMP8(I)
CCCCC         YVAL2=TEMP9(I)
              XVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              YVAL=TEMP8(I)
              XCOOR1=XVAL - XINC
              XCOOR2=XVAL + XINC
              YCOOR1=YVAL - YINC
              YCOOR2=YVAL + YINC
            ENDIF
            IF(STAT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1025J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1025       CONTINUE
            ENDIF
C
            IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
              WRITE(ICOUT,1026)I,STAT,YVAL,XVAL,YVAL2
 1026       FORMAT('I,STAT,YVAL,XVAL,YVAL2 = ',I8,4G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1027)XCOOR1,XCOOR2,YCOOR1,YCOOR2
 1027       FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2 = ',4G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1028)ILEVEL
 1028       FORMAT('ILEVEL = ',I8)
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
 1020     CONTINUE
        ENDIF
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ELSEIF(NCRTV.EQ.4)THEN
        CALL DPTAP5(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,
     1              NUMV2,ICASCT,ISTANR,
     1              XIDTEM,XIDTE2,XIDTE3,XIDTE4,
     1              NUMSE1,NUMSE2,NUMSE3,NUMSE4,
     1              TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,ALPHA,
     1              ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1              TEMP6,TEMP7,TEMP8,TEMP9,TMP10,XACLOW,XACUPP,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE A RECTANGLE
CCCCC   FOR EACH POINT.
C
        ICNT=0
        ICNT2=0
        XINC=0.5 - PTPLXI
        YINC=0.5 - PTPLYI
        YINC2=2.0*YINC/REAL(NUMSE3)
        XINC2=2.0*XINC/REAL(NUMSE4)
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
          WRITE(ICOUT,1031)N2
 1031     FORMAT('DPTAP2: AFTER CALL DPTAP5--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1032)XINC,YINC,XINC2,YINC2
 1032     FORMAT('XINC,YINC,XINC2,YINC2 = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IFLAGU.EQ.1)THEN
          DO2030I=1,N2
            STAT=TEMP6(I)
            STATMN=XACLOW(I)
            STATMX=XACUPP(I)
            IF(ITPLDI.EQ.'X')THEN
CCCCC         XVAL=TEMP7(I)
CCCCC         YVAL=TEMP8(I)
CCCCC         XVAL2=TEMP9(I)
CCCCC         YVAL2=TMP10(I)
              XVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
              YVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
            ELSE
CCCCC         YVAL=TEMP7(I)
CCCCC         XVAL=TEMP8(I)
CCCCC         YVAL2=TEMP9(I)
CCCCC         XVAL2=TMP10(I)
              XVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              YVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
            ENDIF
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
C
C           DIVIDE RECTANGLE INTO "ITPLNI" VERTICAL INCREMENTS AND
C           COMPUTE LEVEL-COLOR INDEPENDENTLY FOR EACH OF THESE
C           MINI-RECTANGLES.
C
            STATIN=(STATMX - STATMN)/REAL(ITPLNI)
            STATZ=STATMN - STATIN
            AINC=(YCOOR2 - YCOOR1)/REAL(ITPLNI)
            YCZ2=YCOOR1
C
            DO2039IROW=1,ITPLNI
C
              YCZ1=YCZ2
              YCZ2=YCZ1 + AINC
C
              STATZ=STATZ + STATIN
              IF(STATZ.LT.YLEVEL(1))THEN
                ILEVEL=1
              ELSEIF(STATZ.GE.YLEVEL(NLEVEL))THEN
                ILEVEL=NLEVEL+1
              ELSE
                DO2035J=2,NLEVEL
                  IF(STATZ.GE.YLEVEL(J-1) .AND. STATZ.LT.YLEVEL(J))THEN
                    ILEVEL=J
                  ENDIF
 2035           CONTINUE
              ENDIF
C
              IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
                WRITE(ICOUT,2036)I,IROW,STAT,STATZ,STATMN,STATMX,STATIN
 2036           FORMAT('I,IROW,STAT,STATZ,STATMN,STATMX,STATIN = ',
     1                 2I8,5G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2037)XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2
 2037           FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2 = ',
     1                 6G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2038)IROW,ILEVEL
 2038           FORMAT('IROW,ILEVEL = ',2I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
C
 2039       CONTINUE
C
 2030     CONTINUE
        ELSE
          DO1030I=1,N2
            STAT=TEMP6(I)
            IF(ITPLDI.EQ.'X')THEN
CCCCC         XVAL=TEMP7(I)
CCCCC         YVAL=TEMP8(I)
CCCCC         XVAL2=TEMP9(I)
CCCCC         YVAL2=TMP10(I)
              XVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
              YVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
            ELSE
CCCCC         YVAL=TEMP7(I)
CCCCC         XVAL=TEMP8(I)
CCCCC         YVAL2=TEMP9(I)
CCCCC         XVAL2=TMP10(I)
              XVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              YVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
            ENDIF
CCCCC       XCOOR1=XVAL - XINC) + (XVAL2 - 1.0)*XINC2
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
CCCCCC      YCOOR1=(YVAL - YINC) + (YVAL2 - 1.0)*YINC2
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
            IF(STAT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1035J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1035       CONTINUE
            ENDIF
C
            IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
              WRITE(ICOUT,1036)I,STAT,YVAL,XVAL,YVAL2,XVAL2
 1036       FORMAT('I,STAT,YVAL,XVAL,YVAL2,XVAL2 = ',I8,5G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1037)XCOOR1,XCOOR2,YCOOR1,YCOOR2
 1037       FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2 = ',4G15.7)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1038)ILEVEL
 1038       FORMAT('ILEVEL = ',I8)
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
 1030     CONTINUE
        ENDIF
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ENDIF
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAP2--')
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
      SUBROUTINE DPTAP3(Y,Z,Z2,TAG1,TAG2,N,
     1                  NUMV2,ICASCT,ISTANR,
     1                  XIDTEM,XIDTE2,
     1                  NUMSE1,NUMSE2,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  YCMNMX,YRMNMX,ITPLCM,ITPLRM,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,ALPHA,
     1                  ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1                  Y2,X2,D2,XACLOW,XACUPP,N2,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A TWO-WAY TABULATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009.
C     UPDATED         --DECEMBER  2009. UNCERTAINTY OPTION FOR
C                                       BINOMIAL PROBABILITY, MEAN AND
C                                       MEDIAN CONFIDENCE INTERVAL
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C     UPDATED         --AUGUST    2010. FOR EACH VALUE, DETERMINE IF
C                                       IT A ROW COLUMN MINIMUM OR
C                                       MAXIMUM VALUE FOR THE STATISTIC
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
      CHARACTER*4 ITPLCM
      CHARACTER*4 ITPLRM
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION YCMNMX(*)
      DIMENSION YRMNMX(*)
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTA'
      ISUBN2='P3  '
C
      I2=0
C
      AN=INT(N+0.01)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
C     FOR EACH ROW/COLUMN COMBINATION, DETERMINE IF IT IS A
C     ROW OR COLUMN MINIMUM OR MAXIMUM.
      J=0
      NRESP=NUMV2-2
      DO1110ISET1=1,NUMSE1
        DO1120ISET2=1,NUMSE2
C
          K=0
          DO1130I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.XIDTE2(ISET2).EQ.TAG2(I))
     1        GOTO1131
            GOTO1130
 1131       CONTINUE
C
            K=K+1
            TEMP(K)=0.0
            TEMPZ(K)=0.0
            TEMPZ2(K)=0.0
            IF(ISTANR.GE.1)TEMP(K)=Y(I)
            IF(ISTANR.GE.2)TEMPZ(K)=Z(I)
            IF(ISTANR.GE.3)TEMPZ2(K)=Z2(I)
 1130     CONTINUE
          NTEMP=K
C
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GOTO1120
            ENDIF
          ELSE
            CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              PTEMP=STAT
              NTRIAL=NTEMP
              IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
                CALL DPAGCO(PTEMP,NTRAIL,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MECL')THEN
              XMEAN=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                      QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
C
          J=J+1
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          AMNMAX=0.0
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1       ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
            IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
C
 1120   CONTINUE
 1110 CONTINUE
      N2=J
C
C     DETERMINE THE COLUMN MINIMUM AND MAXIMUM POINTS
C
      IF(ITPLCM.EQ.'OFF' .AND. ITPLRM.EQ.'OFF')GOTO9000
C
      DO3101I=1,N
        YCMNMX(I)=0.0
        YRMNMX(I)=0.0
 3101 CONTINUE
C
      DO3110ISET2=1,NUMSE2
        ACOLMN=CPUMIN
        ACOLMX=CPUMIN
C
C       DETERMINE COLUMN MIN/MAX
C
        DO3120I=1,N
          IF(XIDTE2(ISET2).EQ.D2(I))THEN
            IF(Y2(I).NE.PSTAMV .AND. Y2(I).NE.CPUMIN)THEN
              IF(ACOLMN.EQ.CPUMIN)THEN
                ACOLMN=Y2(I)
                ACOLMX=Y2(I)
              ELSE
                IF(Y2(I).LE.ACOLMN)ACOLMN=Y2(I)
                IF(Y2(I).GE.ACOLMX)ACOLMX=Y2(I)
              ENDIF
            ENDIF
          ENDIF
 3120   CONTINUE
C
C       NOW SET YCMNMX TO:
C
C           0 = NEITHER MIN NOR MAX
C           1 = EQUAL TO COLUMN MINIMUM
C           2 = EQUAL TO COLUMN MAXIMUM
C
        DO3130I=1,N
          IF(XIDTE2(ISET2).EQ.D2(I))THEN
            YCMNMX(I)=0.0
            IF(Y2(I).EQ.ACOLMN)YCMNMX(I)=1.0
            IF(Y2(I).EQ.ACOLMX)YCMNMX(I)=2.0
          ENDIF
 3130   CONTINUE
C
 3110 CONTINUE
C
C     DETERMINE THE ROW MINIMUM AND MAXIMUM POINTS
C
      DO4110ISET1=1,NUMSE1
        AROWMN=CPUMIN
        AROWMX=CPUMIN
C
C       DETERMINE ROW MIN/MAX
C
        DO4120I=1,N
          IF(XIDTEM(ISET1).EQ.X2(I))THEN
            IF(Y2(I).NE.PSTAMV .AND. Y2(I).NE.CPUMIN)THEN
              IF(AROWMN.EQ.CPUMIN)THEN
                AROWMN=Y2(I)
                AROWMX=Y2(I)
              ELSE
                IF(Y2(I).LE.AROWMN)AROWMN=Y2(I)
                IF(Y2(I).GE.AROWMX)AROWMX=Y2(I)
              ENDIF
            ENDIF
          ENDIF
 4120   CONTINUE
C
C       NOW SET YRMNMX TO:
C
C           0 = NEITHER MIN NOR MAX
C           1 = EQUAL TO ROW MINIMUM
C           2 = EQUAL TO ROW MAXIMUM
C
        DO4130I=1,N
          IF(XIDTEM(ISET1).EQ.X2(I))THEN
            YRMNMX(I)=0.0
            IF(Y2(I).EQ.AROWMN)YRMNMX(I)=1.0
            IF(Y2(I).EQ.AROWMX)YRMNMX(I)=2.0
          ENDIF
 4130   CONTINUE
C
 4110 CONTINUE
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NUMV2,IERROR
 9012   FORMAT('ICASCT,N,NUMV2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMSE1,NUMSE2,N2
 9015   FORMAT('NUMSE1,NUMSE2,N2 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),YCMNMX(I),YRMNMX(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),YCMNMX(I),YRMNMX(I) = ',
     1           I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAP4(Y,Z,Z2,TAG1,TAG2,TAG3,N,
     1                  NUMV2,ICASCT,ISTANR,
     1                  XIDTEM,XIDTE2,XIDTE3,
     1                  NUMSE1,NUMSE2,NUMSE3,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,ALPHA,
     1                  ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1                  Y2,X2,D2,D3,XACLOW,XACUPP,N2,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A TWO-WAY TABULATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009.
C     UPDATED         --DECEMBER  2009. UNCERTAINTY OPTION FOR
C                                       BINOMIAL PROBABILITY, MEAN AND
C                                       MEDIAN CONFIDENCE INTERVAL
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION D3(*)
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTA'
      ISUBN2='P4  '
C
      I2=0
C
      AN=INT(N+0.01)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      J=0
      NRESP=NUMV2-3
      DO1110ISET1=1,NUMSE1
        DO1120ISET2=1,NUMSE2
        DO1130ISET3=1,NUMSE3
C
          K=0
          DO1180I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.
     1         XIDTE2(ISET2).EQ.TAG2(I).AND.
     1         XIDTE3(ISET3).EQ.TAG3(I)
     1       )GOTO1181
            GOTO1180
 1181       CONTINUE
C
            K=K+1
            TEMP(K)=0.0
            TEMPZ(K)=0.0
            TEMPZ2(K)=0.0
            IF(ISTANR.GE.1)TEMP(K)=Y(I)
            IF(ISTANR.GE.2)TEMPZ(K)=Z(I)
            IF(ISTANR.GE.3)TEMPZ2(K)=Z2(I)
 1180     CONTINUE
          NTEMP=K
C
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GOTO1130
            ENDIF
          ELSE
            CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              PTEMP=STAT
              NTRIAL=NTEMP
              IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
                CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MECL')THEN
              XMEAN=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                      QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
C
          J=J+1
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          D3(J)=XIDTE3(ISET3)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1       ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
            IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
C
 1130   CONTINUE
 1120   CONTINUE
 1110 CONTINUE
      N2=J
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAP4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NUMV2,IERROR
 9012   FORMAT('ICASCT,N,NUMV2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMSE1,NUMSE2,NUMSE3,N2
 9015   FORMAT('NUMSE1,NUMSE2,NUMSE3,N2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),D3(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),D3(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAP5(Y,Z,Z2,TAG1,TAG2,TAG3,TAG4,N,
     1                  NUMV2,ICASCT,ISTANR,
     1                  XIDTEM,XIDTE2,XIDTE3,XIDTE4,
     1                  NUMSE1,NUMSE2,NUMSE3,NUMSE4,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,ALPHA,
     1                  ICTAMV,PCTAMV,PSTAMV,IQUASE,
     1                  Y2,X2,D2,D3,D4,XACLOW,XACUPP,N2,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A TWO-WAY TABULATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009.
C     UPDATED         --DECEMBER  2009. UNCERTAINTY OPTION FOR
C                                       BINOMIAL PROBABILITY, MEAN AND
C                                       MEDIAN CONFIDENCE INTERVAL
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION D3(*)
      DIMENSION D4(*)
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTA'
      ISUBN2='P5  '
C
      I2=0
C
      AN=INT(N+0.01)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP5')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      J=0
      NRESP=NUMV2-4
      DO1110ISET1=1,NUMSE1
        DO1120ISET2=1,NUMSE2
        DO1130ISET3=1,NUMSE3
        DO1140ISET4=1,NUMSE4
C
          K=0
          DO1180I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.
     1         XIDTE2(ISET2).EQ.TAG2(I).AND.
     1         XIDTE3(ISET3).EQ.TAG3(I).AND.
     1         XIDTE4(ISET4).EQ.TAG4(I)
     1        )GOTO1181
            GOTO1180
 1181       CONTINUE
C
            K=K+1
            TEMP(K)=0.0
            TEMPZ(K)=0.0
            TEMPZ2(K)=0.0
            IF(ISTANR.GE.1)TEMP(K)=Y(I)
            IF(ISTANR.GE.2)TEMPZ(K)=Z(I)
            IF(ISTANR.GE.3)TEMPZ2(K)=Z2(I)
 1180     CONTINUE
          NTEMP=K
C
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GOTO1140
            ENDIF
          ELSE
            CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              PTEMP=STAT
              NTRIAL=NTEMP
              IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
                CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MECL')THEN
              XMEAN=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                      QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
C
          J=J+1
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          D3(J)=XIDTE3(ISET3)
          D4(J)=XIDTE4(ISET4)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1       ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
            IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
C
 1140   CONTINUE
 1130   CONTINUE
 1120   CONTINUE
 1110 CONTINUE
      N2=J
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP5')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAP5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NUMV2,IERROR
 9012   FORMAT('ICASCT,N,NUMV2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMSE1,NUMSE2,NUMSE3,NUMSE4,N2
 9015   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4,N2 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),D3(I),D4(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),D3(I),D4(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTAWI(IFORWI,IFORWR,MAXNWI,
     1                  ISUBRO,IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--IMPLEMENT THE COMMAND
C
C                  TABLE WIDTH  <SIGDIG>   <TOTWID>
C
C              THIS IS AN ALTERNATIVE TO "SET WRITE DECIMALS" AND
C              "SET WRITE FORMAT" FOR DEFINING HOW TO PRINT
C              VARIABLES WITH THE WRITE COMMAND.  THE LIMITATION
C              OF "SET WRITE DECIMALS" IS THAT IT ONLY ALLOWS YOU
C              TO SPECIFY THE NUMBER OF DIGITS TO THE RIGHT OF
C              THE DECIMAL POINT AND IT SETS ALL COLUMNS TO THE
C              SAME VALUE.  THE LIMITATION OF SET WRITE FORMAT
C              IS THAT IT CANNOT BE EASILY APPLIED TO HTML, LATEK,
C              OR RTF OUTPUT.
C
C              THE <SIGDIG> VARIABLE DEFINES THE NUMBER OF DIGITS
C              TO THE RIGHT OF THE DECIMAL POINT AND <TOTWID> DEFINES
C              THE TOTAL WIDTH OF THE FIELD (SO THIS SETS Fxx.yy
C              FORMAT WHERE WE ARE DEFINING "yy" AND "xx").
C
C              IF EITHER <SIGDIG> OR <TOTWID> IS NEGATIVE, THEN
C              WE USE   Exx.yy FORMAT.
C
C              IF <SIGDIG> OR <TOTWID> IS A SCALAR, THEN ALL ROWS
C              OF IFORWI AND IFORWR WILL BE SET.  IF ONLY <SIGDIG>
C              IS SPECIFIED, <TOTWID> WILL BE SET TO -99 (THIS IS
C              EQUIVALENT TO USING SET WRITE DECIMALS) FOR F FORMAT
C              AND TO <SIGDIG> + 8 FOR E FORMAT.
C
C     INPUT ARGUMENTS --MAXNWI      = MAXIMUM NUMBER OF FIELDS THAT
C                                     CAN BE SPECIFIED
C     OUTPUT ARGUMENTS--IFORWI      = INTEGER ARRAY THAT DEFINES THE
C                                     TOTAL WIDTH OF THE FIELDS
C                     --IFORWR      = INTEGER ARRAY THAT DEFINES THE
C                                     NUMBER OF DIGITS TO THE RIGHT OF
C                                     THE DECIMAL
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/3
C     ORIGINAL VERSION--MARCH     2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 IH11
      CHARACTER*4 IH12
      CHARACTER*4 MESSAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOM2.INC'
C
      DIMENSION IFORWI(*)
      DIMENSION IFORWR(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IHOLD1=0
      IHOLD2=0
      I1=-99
      I2=-99
      I3=-99
      ICOL2=0
C
      IF(ISUBRO.EQ.'TAWI' .OR. IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('****AT THE BEGINNING OF DPTAWI')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MAXNWI
   53   FORMAT('MAXNWI = ',I5)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,MAXNWI
          WRITE(ICOUT,57)I,IFORWI(I),IFORWR(I)
   57     FORMAT('I,IFORWI(I),IFORWR(I) = ',3I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ****************************************************
C               **  TREAT THE CASE WHEN                           **
C               **  THE FORMAT WIDTHS ARE TO BE CHANGED           **
C               ****************************************************
C
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WIDT')GOTO1110
      GOTO1190
C
 1110 CONTINUE
      IF(NUMARG.EQ.1)GOTO1120
      IF(IHARG(NUMARG).EQ.'ON')GOTO1120
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1120
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1120
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      IF(NUMARG.GE.3.AND.IARGT(2).EQ.'NUMB'.AND.
     1IARGT(3).EQ.'NUMB')GOTO1130
      IF(NUMARG.GE.2.AND.IARGT(2).EQ.'NUMB')GOTO1140
      GOTO3140
C
C     CASE 1: RESET DEFAULT
C
 1120 CONTINUE
      I1=-99
      I2=-99
      DO1122I=1,MAXNWI
        IFORWI(I)=I1
        IFORWR(I)=I2
 1122 CONTINUE
      GOTO1180
C
C     CASE 2: BOTH VALUES SCALARS
C
 1130 CONTINUE
      I1=IARG(2)
      I2=IARG(3)
      DO1132I=1,MAXNWI
        IFORWI(I)=I1
        IFORWR(I)=I2
 1132 CONTINUE
      GOTO1180
C
C     CASE 3: ONE SCALAR SPECIFIED
C
 1140 CONTINUE
      I1=-99
      I2=IARG(2)
      DO1142I=1,MAXNWI
        IFORWI(I)=I1
        IFORWR(I)=I2
 1142 CONTINUE
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1185)I1
 1185   FORMAT('THE TABLE WIDTHS SET TO ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1188)I2
 1188   FORMAT('THE TABLE DIGITS SET TO ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
 1190 CONTINUE
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
      WRITE(ICOUT,8109)
 8109 FORMAT('FIELD WIDTH     FIELD DIGITS')
      CALL DPWRST('XXX','BUG ')
      DO8110I=1,MAXNWI
        WRITE(ICOUT,8111)IFORWI(I),IFORWR(I)
 8111   FORMAT(I11,5X,I12)
        CALL DPWRST('XXX','BUG ')
 8110 CONTINUE
      GOTO9000
C
 3140 CONTINUE
C
      IF(IARGT(2).EQ.'NUMB')THEN
        I2=IARG(2)
        N1=-99
      ELSE
        IH11=IHARG(2)
        IH12=IHARG2(2)
        IHWUSE='V'
        MESSAG='YES'
        CALL CHECKN(IH11,IH12,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'NO')THEN
          ICOL1=IVALUE(ILOCV)
          N1=IN(ILOCV)
        ELSE
          GOTO9000
        ENDIF
      ENDIF
C
      IF(IARGT(3).EQ.'NUMB')THEN
        I3=IARG(3)
        N2=-99
      ELSE
        IH11=IHARG(3)
        IH12=IHARG2(3)
        IHWUSE='V'
        MESSAG='YES'
        CALL CHECKN(IH11,IH12,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'NO')THEN
          ICOL2=IVALUE(ILOCV)
          N2=IN(ILOCV)
        ELSE
          GOTO9000
        ENDIF
      ENDIF
C
      IF(N1.GT.0)THEN
        J=0
        IMAX=MIN(MAXNWI,N1)
        DO3160I=1,IMAX
C
          IF(I.GT.IMAX)GOTO3169
          J=J+1
          IFORWI(J)=-99
          IJ=MAXN*(ICOL1-1)+I
          IF(ICOL1.LE.MAXCOL)IFORWI(J)=INT(V(IJ))
          IF(ICOL1.EQ.MAXCP1)IFORWI(J)=INT(PRED(I))
          IF(ICOL1.EQ.MAXCP2)IFORWI(J)=INT(RES(I))
          IF(ICOL1.EQ.MAXCP3)IFORWI(J)=INT(YPLOT(I))
          IF(ICOL1.EQ.MAXCP4)IFORWI(J)=INT(XPLOT(I))
          IF(ICOL1.EQ.MAXCP5)IFORWI(J)=INT(X2PLOT(I))
          IF(ICOL1.EQ.MAXCP6)IFORWI(J)=INT(TAGPLO(I))
C
 3160   CONTINUE
 3169   CONTINUE
C
      ELSE
        DO3165J=1,MAXNWI
          IFORWI(J)=I2
 3165   CONTINUE
      ENDIF
C
      IF(N2.GT.0)THEN
        J=0
        IMAX=MIN(MAXNWI,N2)
        DO3170I=1,IMAX
C
          IF(I.GT.IMAX)GOTO3179
          J=J+1
          IFORWR(J)=-99
          IJ=MAXN*(ICOL2-1)+I
          IF(ICOL2.LE.MAXCOL)IFORWR(J)=INT(V(IJ))
          IF(ICOL2.EQ.MAXCP1)IFORWR(J)=INT(PRED(I))
          IF(ICOL2.EQ.MAXCP2)IFORWR(J)=INT(RES(I))
          IF(ICOL2.EQ.MAXCP3)IFORWR(J)=INT(YPLOT(I))
          IF(ICOL2.EQ.MAXCP4)IFORWR(J)=INT(XPLOT(I))
          IF(ICOL2.EQ.MAXCP5)IFORWR(J)=INT(X2PLOT(I))
          IF(ICOL2.EQ.MAXCP6)IFORWR(J)=INT(TAGPLO(I))
C
 3170   CONTINUE
 3179   CONTINUE
C
      ELSE
        DO3175J=1,MAXNWI
          IFORWR(J)=I3
 3175   CONTINUE
      ENDIF
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8109)
        CALL DPWRST('XXX','BUG ')
        ILAST=MAX(N1,N2)
        ILAST=MIN(ILAST,MAXNWI)
        DO3190I=1,ILAST
          WRITE(ICOUT,8111)IFORWI(I),IFORWR(I)
          CALL DPWRST('XXX','BUG ')
 3190   CONTINUE
      ENDIF
C
      IFOUND='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(ISUBRO.EQ.'TAWI' .OR. IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('****AT THE END OF DPTAWI')
        CALL DPWRST('XXX','BUG ')
        DO9055I=1,MAXNWI
          WRITE(ICOUT,9057)I,IFORWI(I),IFORWR(I)
 9057     FORMAT('I,IFORWI(I),IFORWR(I) = ',3I8)
          CALL DPWRST('XXX','BUG ')
 9055   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTBCO(IHARG,NUMARG,IDETBC,MAXTEX,ITEBCO,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TEXT BORDER COLORS = THE COLORS
C              OF THE BORDER LINE AROUND THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR ITEBCO(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDETBC
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ITEBCO (A CHARACTER VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDETBC
      CHARACTER*4 ITEBCO
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION ITEBCO(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTB'
      ISUBN2='CO  '
C
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTBCO--')
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
      WRITE(ICOUT,55)IDETBC
   55 FORMAT('IDETBC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEBCO(1)
   70 FORMAT('ITEBCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ITEBCO(I)
   76 FORMAT('I,ITEBCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      GOTO1150
C
 1120 CONTINUE
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      ITEBCO(1)=IDETBC
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETBC
      ITEBCO(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEBCO(I)
 1276 FORMAT('THE COLOR OF TEXT BORDER ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETBC
      DO1315I=1,NUMTEX
      ITEBCO(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEBCO(I)
 1316 FORMAT('THE COLOR OF ALL TEXT BORDERS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTBCO--')
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
      WRITE(ICOUT,9015)IDETBC
 9015 FORMAT('IDETBC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEBCO(1)
 9030 FORMAT('ITEBCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,ITEBCO(I)
 9036 FORMAT('I,ITEBCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTBLI(IHARG,IHARG2,NUMARG,IDETBL,MAXTEX,ITEBLI,
CCCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
CCCCC SUBROUTINE DPTBLI(IHARG,NUMARG,IDETBL,MAXTEX,ITEBLI,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE BORDER LINES = THE LINES TYPES
C              OF THE BORDER AROUND THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR ITEBLI(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDETBL
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ITEBLI (A CHARACTER VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C     UPDATED         --AUGUST    1995.  DASH2 BUG
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IDETBL
      CHARACTER*4 ITEBLI
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
      DIMENSION ITEBLI(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTB'
      ISUBN2='LI  '
C
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTBLI--')
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
      WRITE(ICOUT,55)IDETBL
   55 FORMAT('IDETBL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEBLI(1)
   70 FORMAT('ITEBLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ITEBLI(I)
   76 FORMAT('I,ITEBLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO9000
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      IF(NUMARG.EQ.5)GOTO1150
      GOTO1160
C
 1130 CONTINUE
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(5).EQ.'ALL')IHOLD1='    '
      IF(IHARG(5).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
CCCCC IF(IHARG(5).EQ.'ALL')IHOLD1=IHARG(6)
CCCCC IF(IHARG(5).EQ.'ALL')GOTO1300
CCCCC IF(IHARG(6).EQ.'ALL')IHOLD1=IHARG(5)
CCCCC IF(IHARG(6).EQ.'ALL')GOTO1300
CCCCC APRIL 1996.  CHANGE IHOLD TO IHOLD1 BELOW
      IF(IHARG(5).EQ.'ALL')THEN
        IHOLD1=IHARG(6)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'5')IHOLD1='DA5'
        GOTO1300
      ENDIF
      IF(IHARG(6).EQ.'ALL')THEN
        IHOLD1=IHARG(5)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'5')IHOLD1='DA5'
        GOTO1300
      ENDIF
      GOTO1200
C
 1160 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.3)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      ITEBLI(1)='    '
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-3
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+3
      IHOLD1=IHARG(J)
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'2')IHOLD1='DA2'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'3')IHOLD1='DA3'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'4')IHOLD1='DA4'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'5')IHOLD1='DA5'
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETBL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETBL
      ITEBLI(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEBLI(I)
 1276 FORMAT('THE LINE TYPE FOR TEXT BORDER ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETBL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETBL
      DO1315I=1,NUMTEX
      ITEBLI(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEBLI(I)
 1316 FORMAT('THE LINE TYPE FOR ALL TEXT BORDERS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTBLI--')
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
      WRITE(ICOUT,9015)IDETBL
 9015 FORMAT('IDETBL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEBLI(1)
 9030 FORMAT('ITEBLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,ITEBLI(I)
 9036 FORMAT('I,ITEBLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTBTH(IHARG,IARGT,ARG,NUMARG,PDETBT,MAXTEX,PTEBTH,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TEXT (BORDER) LINE THICKNESSES = THE THICKNESSES
C              OF THE BORDER LINE AROUND THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR PTEBTH(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --PDETBT
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--PTEBTH (A FLOATING POINT VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PTEBTH(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTB'
      ISUBN2='TH  '
C
      NUMTEX=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTBTH--')
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
      WRITE(ICOUT,55)PDETBT
   55 FORMAT('PDETBT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PTEBTH(1)
   70 FORMAT('PTEBTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,PTEBTH(I)
   76 FORMAT('I,PTEBTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      GOTO1150
C
 1120 CONTINUE
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDETBT
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      PTEBTH(1)=PDETBT
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      HOLD1=ARG(J)
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETBT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETBT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETBT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETBT
      PTEBTH(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,PTEBTH(I)
 1276 FORMAT('THE THICKNESS OF TEXT BORDER ',I6,
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETBT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETBT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETBT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETBT
      DO1315I=1,NUMTEX
      PTEBTH(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PTEBTH(I)
 1316 FORMAT('THE THICKNESS OF ALL TEXT BORDERS',
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTBTH--')
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
      WRITE(ICOUT,9015)PDETBT
 9015 FORMAT('PDETBT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PTEBTH(1)
 9030 FORMAT('PTEBTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,PTEBTH(I)
 9036 FORMAT('I,PTEBTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTCCL(ICOM,IHARG,NUMARG,
     1IDEFCO,
     1IX1TCO,IX2TCO,IY1TCO,IY2TCO,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK COLOR SWITCHES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK SWITCHES DESCRIBE
C              THE TIC MARK COLOR ON THE 4 FRAME LINES OF A PLOT.
C              THE CONTENTS OF A TIC MARK COLOR SWITCH ARE
C              A COLOR.
C              THE TIC MARK COLOR SWITCHES FOR THE 4 FRAME LINES
C              ARE CONTAINED IN THE 4 VARIABLES
C              IX1TCO,IX2TCO,IY1TCO,IY2TCO
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFCO
C     OUTPUT ARGUMENTS--IX1TCO = COLOR FOR BOTTOM HORIZ. TICS
C                     --IX2TCO = COLOR FOR TOP    HORIZ. TICS
C                     --IY1TCO = COLOR FOR LEFT   VERT.  TICS
C                     --IY2TCO = COLOR FOR RIGHT  VERT.  TICS
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
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IDEFCO
C
      CHARACTER*4 IX1TCO
      CHARACTER*4 IX2TCO
      CHARACTER*4 IY1TCO
      CHARACTER*4 IY2TCO
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'COLO')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFCO
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1TCO=IHOLD
      IX2TCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK COLOR (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFCO
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1TCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK COLOR (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFCO
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2TCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK COLOR (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFCO
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1TCO=IHOLD
      IY2TCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK COLOR (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFCO
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1TCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK COLOR (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFCO
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2TCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK COLOR (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'COLO')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFCO
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1TCO=IHOLD
      IX2TCO=IHOLD
      IY1TCO=IHOLD
      IY2TCO=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK COLOR (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTCDP(ICOM,IHARG,IARG,NUMARG,
     1                  IDEFDP,
     1                  IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP,
     1                  IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK LABEL DECIMAL PLACES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK LABEL SWITCHES DESCRIBE
C              THE NUMBER OF TIC MARK LABEL DECIMAL PLACES ON THE 4 FRAME LINES
C              THE CONTENTS OF A TIC MARK LABEL DECIMAL PLACE ARE
C              AN INTEGER NUMBER.
C              THE TIC MARK LABEL DECIMAL PLACES FOR THE 4 FRAME LINES
C              ARE CONTAINED IN THE 4 VARIABLES
C              IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --IARG  (AN INTEGER VECTOR)
C                     --NUMARG
C                     --IDEFDP
C     OUTPUT ARGUMENTS--IX1ZDP = NUM. DEC. FOR BOTTOM HORIZ. TIC LABELS
C                     --IX2ZDP = NUM. DEC. FOR TOP    HORIZ. TIC LABELS
C                     --IY1ZDP = NUM. DEC. FOR LEFT   VERT.  TIC LABELS
C                     --IY2ZDP = NUM. DEC. FOR RIGHT  VERT.  TIC LABELS
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
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
C
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI')GOTO1090
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLAC')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DECI'.AND.
     1IHARG(2).EQ.'PLAC')GOTO1090
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'DECI')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'PLAC')GOTO1090
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'DECI')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.
     1IHARG(2).EQ.'PLAC')GOTO1090
C
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(3).EQ.'PLAC')GOTO1090
CCCCC JUNE 1994.  FOLLOWING 3 LINES ADDED (FOR TIC MARK LABEL DECIMAL)
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE'.AND.
     1IHARG(3).EQ.'DECI')GOTO1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'LABEL'.AND.
     1IHARG(3).EQ.'PLAC')GOTO1090
CCCCC JUNE 1994.  FOLLOWING 2 LINES ADDED (FOR TIC MARK LABEL DECIMAL)
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(4).EQ.'PLAC')GOTO1090
C
      GOTO9000
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'DECI')GOTO1150
      IF(IHARG(NUMARG).EQ.'PLAC')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFDP
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1ZDP=IHOLD
      IX2ZDP=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC LABEL DECIMALS (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1183)
 1183 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO9000
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'DECI')GOTO1250
      IF(IHARG(NUMARG).EQ.'PLAC')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFDP
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1ZDP=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC LABEL DECIMALS (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1283)
 1283 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO9000
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'DECI')GOTO1350
      IF(IHARG(NUMARG).EQ.'PLAC')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFDP
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2ZDP=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC LABEL DECIMALS (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1383)
 1383 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO9000
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'DECI')GOTO1450
      IF(IHARG(NUMARG).EQ.'PLAC')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFDP
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1ZDP=IHOLD
      IY2ZDP=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC LABEL DECIMALS (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1483)
 1483 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO9000
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'DECI')GOTO1550
      IF(IHARG(NUMARG).EQ.'PLAC')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFDP
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1ZDP=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC LABEL DECIMALS (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1583)
 1583 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO9000
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'DECI')GOTO1650
      IF(IHARG(NUMARG).EQ.'PLAC')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFDP
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2ZDP=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC LABEL DECIMALS (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1683)
 1683 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO9000
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'DECI')GOTO1750
      IF(IHARG(NUMARG).EQ.'PLAC')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFDP
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1ZDP=IHOLD
      IX2ZDP=IHOLD
      IY1ZDP=IHOLD
      IY2ZDP=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC LABEL DECIMALS (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1783)
 1783 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO9000
C
 1799 CONTINUE
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
 8111 FORMAT('THE CURRENT NUMBER OF TIC LABEL DECIMAL PLACES IS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)IX1ZDP
 8112 FORMAT('            --X1 (BOTTOM HORIZONTAL) = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8113)IX2ZDP
 8113 FORMAT('            --X2 (TOP    HORIZONTAL) = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8114)IY1ZDP
 8114 FORMAT('            --Y1 (LEFT   VERTICAL  ) = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8115)IY2ZDP
 8115 FORMAT('            --Y2 (RIGHT  VERTICAL  ) = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8116)
 8116 FORMAT('            --NEGATIVE VALUES INDICATE THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8117)
 8117 FORMAT('              NUMBER OF DECIMALS FLOAT AND NEAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8121)
 8121 FORMAT('THE DEFAULT NUMBER OF TIC LABEL DECIMAL PLACES ARE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8122)
 8122 FORMAT('            --X1 (BOTTOM HORIZONTAL) = FLOAT & NEAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8123)
 8123 FORMAT('            --X2 (TOP    HORIZONTAL) = FLOAT & NEAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8124)
 8124 FORMAT('            --Y1 (LEFT   VERTICAL  ) = FLOAT & NEAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8125)
 8125 FORMAT('            --Y2 (BOTTOM VERTICAL  ) = FLOAT & NEAT')
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
      SUBROUTINE DPTCJU(ICOM,IHARG,NUMARG,
     1IX1TJU,IX2TJU,IY1TJU,IY2TJU,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK JUSTIFICATION SWITCHES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK SWITCHES DESCRIBE
C              THE TIC MARK JUSTIFICATION (THRU, IN, OR OUT) ON THE 4 FRAME LINE
C              THE CONTENTS OF A TIC MARK JUSTIFICATION SWITCH ARE
C              A JUSTIFICATION (THRU, IN, OR OUT).
C              THE TIC MARK JUSTIFICATION SWITCHES FOR THE 4 FRAME LINES
C              ARE CONTAINED IN THE 4 VARIABLES
C              IX1TJU,IX2TJU,IY1TJU,IY2TJU
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--IX1TJU = JUSTIFICATION FOR BOTTOM HORIZ. TICS
C                     --IX2TJU = JUSTIFICATION FOR TOP    HORIZ. TICS
C                     --IY1TJU = JUSTIFICATION FOR LEFT   VERT.  TICS
C                     --IY2TJU = JUSTIFICATION FOR RIGHT  VERT.  TICS
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
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IX1TJU
      CHARACTER*4 IX2TJU
      CHARACTER*4 IY1TJU
      CHARACTER*4 IY2TJU
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'POSI')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'POSI')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'POSI')GOTO1150
      IF(IHARG(NUMARG).EQ.'IN')GOTO1130
      IF(IHARG(NUMARG).EQ.'INSI')GOTO1130
      IF(IHARG(NUMARG).EQ.'OUT')GOTO1140
      IF(IHARG(NUMARG).EQ.'OUTS')GOTO1140
      IF(IHARG(NUMARG).EQ.'THRO')GOTO1150
      IF(IHARG(NUMARG).EQ.'THRU')GOTO1150
      IF(IHARG(NUMARG).EQ.'CENT')GOTO1150
      IERROR='YES'
      GOTO1900
C
 1130 CONTINUE
      IFOUND='YES'
      IX1TJU='IN'
      IX2TJU='IN'
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GOTO1900
C
 1140 CONTINUE
      IFOUND='YES'
      IX1TJU='OUT'
      IX2TJU='OUT'
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1146)
 1146 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GOTO1900
C
 1150 CONTINUE
      IFOUND='YES'
      IX1TJU='THRU'
      IX2TJU='THRU'
C
      IF(IFEEDB.EQ.'OFF')GOTO1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)
 1156 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1159 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'POSI')GOTO1250
      IF(IHARG(NUMARG).EQ.'IN')GOTO1230
      IF(IHARG(NUMARG).EQ.'INSI')GOTO1230
      IF(IHARG(NUMARG).EQ.'OUT')GOTO1240
      IF(IHARG(NUMARG).EQ.'OUTS')GOTO1240
      IF(IHARG(NUMARG).EQ.'THRO')GOTO1250
      IF(IHARG(NUMARG).EQ.'THRU')GOTO1250
      IF(IHARG(NUMARG).EQ.'CENT')GOTO1250
      IERROR='YES'
      GOTO1900
C
 1230 CONTINUE
      IFOUND='YES'
      IX1TJU='IN'
C
      IF(IFEEDB.EQ.'OFF')GOTO1239
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1235)
 1235 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1236)
 1236 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1239 CONTINUE
      GOTO1900
C
 1240 CONTINUE
      IFOUND='YES'
      IX1TJU='OUT'
C
      IF(IFEEDB.EQ.'OFF')GOTO1249
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1245)
 1245 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1246)
 1246 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1249 CONTINUE
      GOTO1900
C
 1250 CONTINUE
      IFOUND='YES'
      IX1TJU='THRU'
C
      IF(IFEEDB.EQ.'OFF')GOTO1259
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1255)
 1255 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1256)
 1256 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1259 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'POSI')GOTO1350
      IF(IHARG(NUMARG).EQ.'IN')GOTO1330
      IF(IHARG(NUMARG).EQ.'INSI')GOTO1330
      IF(IHARG(NUMARG).EQ.'OUT')GOTO1340
      IF(IHARG(NUMARG).EQ.'OUTS')GOTO1340
      IF(IHARG(NUMARG).EQ.'THRO')GOTO1350
      IF(IHARG(NUMARG).EQ.'THRU')GOTO1350
      IF(IHARG(NUMARG).EQ.'CENT')GOTO1350
      IERROR='YES'
      GOTO1900
C
 1330 CONTINUE
      IFOUND='YES'
      IX2TJU='IN'
C
      IF(IFEEDB.EQ.'OFF')GOTO1339
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1335)
 1335 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1336)
 1336 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1339 CONTINUE
      GOTO1900
C
 1340 CONTINUE
      IFOUND='YES'
      IX2TJU='OUT'
C
      IF(IFEEDB.EQ.'OFF')GOTO1349
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1345)
 1345 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1346)
 1346 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1349 CONTINUE
      GOTO1900
C
 1350 CONTINUE
      IFOUND='YES'
      IX2TJU='THRU'
C
      IF(IFEEDB.EQ.'OFF')GOTO1359
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1355)
 1355 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1356)
 1356 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1359 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'POSI')GOTO1450
      IF(IHARG(NUMARG).EQ.'IN')GOTO1430
      IF(IHARG(NUMARG).EQ.'INSI')GOTO1430
      IF(IHARG(NUMARG).EQ.'OUT')GOTO1440
      IF(IHARG(NUMARG).EQ.'OUTS')GOTO1440
      IF(IHARG(NUMARG).EQ.'THRO')GOTO1450
      IF(IHARG(NUMARG).EQ.'THRU')GOTO1450
      IF(IHARG(NUMARG).EQ.'CENT')GOTO1450
      IERROR='YES'
      GOTO1900
C
 1430 CONTINUE
      IFOUND='YES'
      IY1TJU='IN'
      IY2TJU='IN'
C
      IF(IFEEDB.EQ.'OFF')GOTO1439
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1435)
 1435 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1436)
 1436 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1439 CONTINUE
      GOTO1900
C
 1440 CONTINUE
      IFOUND='YES'
      IY1TJU='OUT'
      IY2TJU='OUT'
C
      IF(IFEEDB.EQ.'OFF')GOTO1449
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1445)
 1445 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1446)
 1446 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1449 CONTINUE
      GOTO1900
C
 1450 CONTINUE
      IFOUND='YES'
      IY1TJU='THRU'
      IY2TJU='THRU'
C
      IF(IFEEDB.EQ.'OFF')GOTO1459
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1455)
 1455 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1456)
 1456 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1459 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'POSI')GOTO1550
      IF(IHARG(NUMARG).EQ.'IN')GOTO1530
      IF(IHARG(NUMARG).EQ.'INSI')GOTO1530
      IF(IHARG(NUMARG).EQ.'OUT')GOTO1540
      IF(IHARG(NUMARG).EQ.'OUTS')GOTO1540
      IF(IHARG(NUMARG).EQ.'THRO')GOTO1550
      IF(IHARG(NUMARG).EQ.'THRU')GOTO1550
      IF(IHARG(NUMARG).EQ.'CENT')GOTO1550
      IERROR='YES'
      GOTO1900
C
 1530 CONTINUE
      IFOUND='YES'
      IY1TJU='IN'
C
      IF(IFEEDB.EQ.'OFF')GOTO1539
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1535)
 1535 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1536)
 1536 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1539 CONTINUE
      GOTO1900
C
 1540 CONTINUE
      IFOUND='YES'
      IY1TJU='OUT'
C
      IF(IFEEDB.EQ.'OFF')GOTO1549
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1545)
 1545 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1546)
 1546 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1549 CONTINUE
      GOTO1900
C
 1550 CONTINUE
      IFOUND='YES'
      IY1TJU='THRU'
C
      IF(IFEEDB.EQ.'OFF')GOTO1559
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1555)
 1555 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1556)
 1556 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1559 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'POSI')GOTO1650
      IF(IHARG(NUMARG).EQ.'IN')GOTO1630
      IF(IHARG(NUMARG).EQ.'INSI')GOTO1630
      IF(IHARG(NUMARG).EQ.'OUT')GOTO1640
      IF(IHARG(NUMARG).EQ.'OUTS')GOTO1640
      IF(IHARG(NUMARG).EQ.'THRO')GOTO1650
      IF(IHARG(NUMARG).EQ.'THRU')GOTO1650
      IF(IHARG(NUMARG).EQ.'CENT')GOTO1650
      IERROR='YES'
      GOTO1900
C
 1630 CONTINUE
      IFOUND='YES'
      IY2TJU='IN'
C
      IF(IFEEDB.EQ.'OFF')GOTO1639
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1635)
 1635 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1636)
 1636 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1639 CONTINUE
      GOTO1900
C
 1640 CONTINUE
      IFOUND='YES'
      IY2TJU='OUT'
C
      IF(IFEEDB.EQ.'OFF')GOTO1649
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1645)
 1645 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1646)
 1646 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1649 CONTINUE
      GOTO1900
C
 1650 CONTINUE
      IFOUND='YES'
      IY2TJU='THRU'
C
      IF(IFEEDB.EQ.'OFF')GOTO1659
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1655)
 1655 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1656)
 1656 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1659 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'POSI')GOTO1750
      IF(IHARG(NUMARG).EQ.'IN')GOTO1730
      IF(IHARG(NUMARG).EQ.'INSI')GOTO1730
      IF(IHARG(NUMARG).EQ.'OUT')GOTO1740
      IF(IHARG(NUMARG).EQ.'OUTS')GOTO1740
      IF(IHARG(NUMARG).EQ.'THRO')GOTO1750
      IF(IHARG(NUMARG).EQ.'THRU')GOTO1750
      IF(IHARG(NUMARG).EQ.'CENT')GOTO1750
      IERROR='YES'
      GOTO1900
C
 1730 CONTINUE
      IFOUND='YES'
      IX1TJU='IN'
      IX2TJU='IN'
      IY1TJU='IN'
      IY2TJU='IN'
C
      IF(IFEEDB.EQ.'OFF')GOTO1739
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1735)
 1735 FORMAT('THE TIC MARKS (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1736)
 1736 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1739 CONTINUE
      GOTO1900
C
 1740 CONTINUE
      IFOUND='YES'
      IX1TJU='OUT'
      IX2TJU='OUT'
      IY1TJU='OUT'
      IY2TJU='OUT'
C
      IF(IFEEDB.EQ.'OFF')GOTO1749
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1745)
 1745 FORMAT('THE TIC MARKS (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1746)
 1746 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1749 CONTINUE
      GOTO1900
C
 1750 CONTINUE
      IFOUND='YES'
      IX1TJU='THRU'
      IX2TJU='THRU'
      IY1TJU='THRU'
      IY2TJU='THRU'
C
      IF(IFEEDB.EQ.'OFF')GOTO1759
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1755)
 1755 FORMAT('THE TIC MARKS (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1756)
 1756 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1759 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTCOF(ICOM,IHARG,IARGT,ARG,NUMARG,
     1DEFTOF,IDEFTU,
     1ITICUN,
     1PX1TOL,PX2TOL,PY1TOB,PY2TOB,
     1PX1TOR,PX2TOR,PY1TOT,PY2TOT,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK OFFSETS
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK OFFSETS DEFINE THE DISTANCE (IN EITHER
C              DATA UNITS OR DATAPLOT PERCENT UNITS) FROM THE FIRST OR
C              LAST TIC MARK TO THE FRAME LIMIT.  NOTE THAT THIS VALUE
C              WILL BE ADDED TO THE CURRENT DATA LIMITS (EITHER DEFINED
C              VIA THE LIMITS COMMAND OR AS AUTOMATICALLY DETERMINED
C              BY DATAPLOT).
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C                     --DEFTOF = DEFAULT OFFSET
C                     --IDEFTU = DEFAULT TIC UNITS
C     OUTPUT ARGUMENTS--
C                     --PX1TOL = BOTTOM HORIZONTAL TIC LEFT OFFSET
C                     --PX2TOL = TOP    HORIZONTAL TIC LEFT OFFSET
C                     --PY1TOB = LEFT   VERTICAL   TIC BOTTOM OFFSET
C                     --PY2TOB = RIGHT  VERTICAL   TIC BOTTOM OFFSET
C                     --PX1TOL = BOTTOM HORIZONTAL TIC LEFT OFFSET
C                     --PX2TOL = TOP    HORIZONTAL TIC LEFT OFFSET
C                     --PY1TOB = LEFT   VERTICAL   TIC BOTTOM OFFSET
C                     --PY2TOB = RIGHT  VERTICAL   TIC BOTTOM OFFSET
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
C     VERSION NUMBER--90/5
C     ORIGINAL VERSION--MAY       1990.
C     UPDATED         --OCTOBER    1991. INSERT FEEDBACK OFF JUMP
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 ITICUN
      CHARACTER*4 IDEFTU
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'OFFS'.AND.
     1IHARG(2).EQ.'UNIT')GOTO2090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'OFFS'.AND.IHARG(3).EQ.'UNIT')GOTO2090
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'OFFS')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'OFFS')GOTO1090
      GOTO1900
C
 1090 CONTINUE
      IFOUND='YES'
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      ILEFT=2
      IF(IHARG(2).EQ.'OFFS')ILEFT=3
      IRIGHT=ILEFT+1
      IF(ILEFT.GT.NUMARG)ILEFT=0
      IF(IRIGHT.GT.NUMARG)IRIGHT=0
C
C               *****************************************************
C               **  TREAT THE LEFT OFFSET                          **
C               **  NO ARGUMENT WILL SET THE DEFAULT               **
C               *****************************************************
C
      IF(ILEFT.EQ.0)GOTO1110
      IF(IHARG(ILEFT).EQ.'ON')GOTO1110
      IF(IHARG(ILEFT).EQ.'OFF')GOTO1110
      IF(IHARG(ILEFT).EQ.'AUTO')GOTO1110
      IF(IHARG(ILEFT).EQ.'DEFA')GOTO1110
      IF(IHARG(ILEFT).EQ.'FLOA')GOTO1110
      IF(IARGT(ILEFT).EQ.'NUMB')GOTO1120
      IERROR='YES'
      GOTO1900
C
 1110 CONTINUE
      HOLD=DEFTOF
      GOTO1140
C
 1120 CONTINUE
      HOLD=ARG(ILEFT)
      GOTO1140
C
 1140 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PX1TOL=HOLD
      PX2TOL=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1149
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('THE TIC MARK LEFT OFFSET (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)HOLD
 1142 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
C
C               *****************************************************
C               **  TREAT THE RIGHT OFFSET                         **
C               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
C               *****************************************************
C
      IF(IRIGHT.EQ.0)GOTO1160
      IF(IHARG(IRIGHT).EQ.'ON')GOTO1170
      IF(IHARG(IRIGHT).EQ.'OFF')GOTO1170
      IF(IHARG(IRIGHT).EQ.'AUTO')GOTO1170
      IF(IHARG(IRIGHT).EQ.'DEFA')GOTO1170
      IF(IHARG(IRIGHT).EQ.'FLOA')GOTO1170
      IF(IARGT(IRIGHT).EQ.'NUMB')GOTO1180
      IERROR='YES'
      GOTO1900
C
 1160 CONTINUE
      HOLD=PX1TOR
      GOTO1190
C
 1170 CONTINUE
      HOLD=DEFTOF
      GOTO1190
C
 1180 CONTINUE
      HOLD=ARG(IRIGHT)
      GOTO1190
C
 1190 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PX1TOR=HOLD
      PX2TOR=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1197
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1191)
 1191 FORMAT('THE TIC MARK RIGHT OFFSET (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1192)HOLD
 1192 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
C
 1197 CONTINUE
C
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
C
      ILEFT=2
      IF(IHARG(2).EQ.'OFFS')ILEFT=3
      IRIGHT=ILEFT+1
      IF(ILEFT.GT.NUMARG)ILEFT=0
      IF(IRIGHT.GT.NUMARG)IRIGHT=0
C
C               *****************************************************
C               **  TREAT THE LEFT OFFSET                          **
C               **  NO ARGUMENT WILL SET THE DEFAULT               **
C               *****************************************************
C
      IF(ILEFT.EQ.0)GOTO1210
      IF(IHARG(ILEFT).EQ.'ON')GOTO1210
      IF(IHARG(ILEFT).EQ.'OFF')GOTO1210
      IF(IHARG(ILEFT).EQ.'AUTO')GOTO1210
      IF(IHARG(ILEFT).EQ.'DEFA')GOTO1210
      IF(IHARG(ILEFT).EQ.'FLOA')GOTO1210
      IF(IARGT(ILEFT).EQ.'NUMB')GOTO1220
      IERROR='YES'
      GOTO1900
C
 1210 CONTINUE
      HOLD=DEFTOF
      GOTO1240
C
 1220 CONTINUE
      HOLD=ARG(ILEFT)
      GOTO1240
C
 1240 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PX1TOL=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1249
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1241)
 1241 FORMAT('THE TIC MARK LEFT OFFSET (FOR BOTTOM HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1242)HOLD
 1242 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1249 CONTINUE
C
C               *****************************************************
C               **  TREAT THE RIGHT OFFSET                         **
C               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
C               *****************************************************
C
      IF(IRIGHT.EQ.0)GOTO1260
      IF(IHARG(IRIGHT).EQ.'ON')GOTO1270
      IF(IHARG(IRIGHT).EQ.'OFF')GOTO1270
      IF(IHARG(IRIGHT).EQ.'AUTO')GOTO1270
      IF(IHARG(IRIGHT).EQ.'DEFA')GOTO1270
      IF(IHARG(IRIGHT).EQ.'FLOA')GOTO1270
      IF(IARGT(IRIGHT).EQ.'NUMB')GOTO1280
      IERROR='YES'
      GOTO1900
C
 1260 CONTINUE
      HOLD=PX2TOR
      GOTO1290
C
 1270 CONTINUE
      HOLD=DEFTOF
      GOTO1290
C
 1280 CONTINUE
      HOLD=ARG(IRIGHT)
      GOTO1290
C
 1290 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PX1TOR=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1297
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1291)
 1291 FORMAT('THE TIC MARK RIGHT OFFSET (FOR BOTTOM HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1292)HOLD
 1292 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
C
 1297 CONTINUE
C
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
C
      ILEFT=2
      IF(IHARG(2).EQ.'OFFS')ILEFT=3
      IRIGHT=ILEFT+1
      IF(ILEFT.GT.NUMARG)ILEFT=0
      IF(IRIGHT.GT.NUMARG)IRIGHT=0
C
C               *****************************************************
C               **  TREAT THE LEFT OFFSET                          **
C               **  NO ARGUMENT WILL SET THE DEFAULT               **
C               *****************************************************
C
      IF(ILEFT.EQ.0)GOTO1310
      IF(IHARG(ILEFT).EQ.'ON')GOTO1310
      IF(IHARG(ILEFT).EQ.'OFF')GOTO1310
      IF(IHARG(ILEFT).EQ.'AUTO')GOTO1310
      IF(IHARG(ILEFT).EQ.'DEFA')GOTO1310
      IF(IHARG(ILEFT).EQ.'FLOA')GOTO1310
      IF(IARGT(ILEFT).EQ.'NUMB')GOTO1320
      IERROR='YES'
      GOTO1900
C
 1310 CONTINUE
      HOLD=DEFTOF
      GOTO1340
C
 1320 CONTINUE
      HOLD=ARG(ILEFT)
      GOTO1340
C
 1340 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PX2TOL=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1349
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1341)
 1341 FORMAT('THE TIC MARK LEFT OFFSET (FOR TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1342)HOLD
 1342 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1349 CONTINUE
C
C               *****************************************************
C               **  TREAT THE RIGHT OFFSET                         **
C               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
C               *****************************************************
C
      IF(IRIGHT.EQ.0)GOTO1360
      IF(IHARG(IRIGHT).EQ.'ON')GOTO1370
      IF(IHARG(IRIGHT).EQ.'OFF')GOTO1370
      IF(IHARG(IRIGHT).EQ.'AUTO')GOTO1370
      IF(IHARG(IRIGHT).EQ.'DEFA')GOTO1370
      IF(IHARG(IRIGHT).EQ.'FLOA')GOTO1370
      IF(IARGT(IRIGHT).EQ.'NUMB')GOTO1380
      IERROR='YES'
      GOTO1900
C
 1360 CONTINUE
      HOLD=PX2TOR
      GOTO1390
C
 1370 CONTINUE
      HOLD=DEFTOF
      GOTO1390
C
 1380 CONTINUE
      HOLD=ARG(IRIGHT)
      GOTO1390
C
 1390 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PX2TOR=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1397
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1391)
 1391 FORMAT('THE TIC MARK RIGHT OFFSET (FOR TOP HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1392)HOLD
 1392 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
C
 1397 CONTINUE
C
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
C
      ILEFT=2
      IF(IHARG(2).EQ.'OFFS')ILEFT=3
      IRIGHT=ILEFT+1
      IF(ILEFT.GT.NUMARG)ILEFT=0
      IF(IRIGHT.GT.NUMARG)IRIGHT=0
C
C               *****************************************************
C               **  TREAT THE BOTTOM OFFSET                        **
C               **  NO ARGUMENT WILL SET THE DEFAULT               **
C               *****************************************************
C
      IF(ILEFT.EQ.0)GOTO1410
      IF(IHARG(ILEFT).EQ.'ON')GOTO1410
      IF(IHARG(ILEFT).EQ.'OFF')GOTO1410
      IF(IHARG(ILEFT).EQ.'AUTO')GOTO1410
      IF(IHARG(ILEFT).EQ.'DEFA')GOTO1410
      IF(IHARG(ILEFT).EQ.'FLOA')GOTO1410
      IF(IARGT(ILEFT).EQ.'NUMB')GOTO1420
      IERROR='YES'
      GOTO1900
C
 1410 CONTINUE
      HOLD=DEFTOF
      GOTO1440
C
 1420 CONTINUE
      HOLD=ARG(ILEFT)
      GOTO1440
C
 1440 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PY1TOB=HOLD
      PY2TOB=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1449
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1441)
 1441 FORMAT('THE TIC MARK BOTTOM OFFSET (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1442)HOLD
 1442 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1449 CONTINUE
C
C               *****************************************************
C               **  TREAT THE TOP OFFSET                           **
C               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
C               *****************************************************
C
      IF(IRIGHT.EQ.0)GOTO1460
      IF(IHARG(IRIGHT).EQ.'ON')GOTO1470
      IF(IHARG(IRIGHT).EQ.'OFF')GOTO1470
      IF(IHARG(IRIGHT).EQ.'AUTO')GOTO1470
      IF(IHARG(IRIGHT).EQ.'DEFA')GOTO1470
      IF(IHARG(IRIGHT).EQ.'FLOA')GOTO1470
      IF(IARGT(IRIGHT).EQ.'NUMB')GOTO1480
      IERROR='YES'
      GOTO1900
C
 1460 CONTINUE
      HOLD=PY1TOT
      GOTO1490
C
 1470 CONTINUE
      HOLD=DEFTOF
      GOTO1490
C
 1480 CONTINUE
      HOLD=ARG(IRIGHT)
      GOTO1490
C
 1490 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PY1TOT=HOLD
      PY2TOT=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1497
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1491)
 1491 FORMAT('THE TIC MARK TOP OFFSET (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1492)HOLD
 1492 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
C
 1497 CONTINUE
C
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT VERTICAL TIC OFFSETS ARE TO BE CHANGED    **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
C
      ILEFT=2
      IF(IHARG(2).EQ.'OFFS')ILEFT=3
      IRIGHT=ILEFT+1
      IF(ILEFT.GT.NUMARG)ILEFT=0
      IF(IRIGHT.GT.NUMARG)IRIGHT=0
C
C               *****************************************************
C               **  TREAT THE BOTTOM OFFSET                        **
C               **  NO ARGUMENT WILL SET THE DEFAULT               **
C               *****************************************************
C
      IF(ILEFT.EQ.0)GOTO1510
      IF(IHARG(ILEFT).EQ.'ON')GOTO1510
      IF(IHARG(ILEFT).EQ.'OFF')GOTO1510
      IF(IHARG(ILEFT).EQ.'AUTO')GOTO1510
      IF(IHARG(ILEFT).EQ.'DEFA')GOTO1510
      IF(IHARG(ILEFT).EQ.'FLOA')GOTO1510
      IF(IARGT(ILEFT).EQ.'NUMB')GOTO1520
      IERROR='YES'
      GOTO1900
C
 1510 CONTINUE
      HOLD=DEFTOF
      GOTO1540
C
 1520 CONTINUE
      HOLD=ARG(ILEFT)
      GOTO1540
C
 1540 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PY1TOB=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1549
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1541)
 1541 FORMAT('THE TIC MARK BOTTOM OFFSET (FOR LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1542)HOLD
 1542 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1549 CONTINUE
C
C               *****************************************************
C               **  TREAT THE TOP OFFSET                           **
C               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
C               *****************************************************
C
      IF(IRIGHT.EQ.0)GOTO1560
      IF(IHARG(IRIGHT).EQ.'ON')GOTO1570
      IF(IHARG(IRIGHT).EQ.'OFF')GOTO1570
      IF(IHARG(IRIGHT).EQ.'AUTO')GOTO1570
      IF(IHARG(IRIGHT).EQ.'DEFA')GOTO1570
      IF(IHARG(IRIGHT).EQ.'FLOA')GOTO1570
      IF(IARGT(IRIGHT).EQ.'NUMB')GOTO1580
      IERROR='YES'
      GOTO1900
C
 1560 CONTINUE
      HOLD=PY1TOT
      GOTO1590
C
 1570 CONTINUE
      HOLD=DEFTOF
      GOTO1590
C
 1580 CONTINUE
      HOLD=ARG(IRIGHT)
      GOTO1590
C
 1590 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PY1TOT=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1597
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1591)
 1591 FORMAT('THE TIC MARK TOP OFFSET (FOR LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1592)HOLD
 1592 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
C
 1597 CONTINUE
C
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT VERTICAL TIC OFFSETS ARE TO BE CHANGED   **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
C
      ILEFT=2
      IF(IHARG(2).EQ.'OFFS')ILEFT=3
      IRIGHT=ILEFT+1
      IF(ILEFT.GT.NUMARG)ILEFT=0
      IF(IRIGHT.GT.NUMARG)IRIGHT=0
C
C               *****************************************************
C               **  TREAT THE BOTTOM OFFSET                        **
C               **  NO ARGUMENT WILL SET THE DEFAULT               **
C               *****************************************************
C
      IF(ILEFT.EQ.0)GOTO1610
      IF(IHARG(ILEFT).EQ.'ON')GOTO1610
      IF(IHARG(ILEFT).EQ.'OFF')GOTO1610
      IF(IHARG(ILEFT).EQ.'AUTO')GOTO1610
      IF(IHARG(ILEFT).EQ.'DEFA')GOTO1610
      IF(IHARG(ILEFT).EQ.'FLOA')GOTO1610
      IF(IARGT(ILEFT).EQ.'NUMB')GOTO1620
      IERROR='YES'
      GOTO1900
C
 1610 CONTINUE
      HOLD=DEFTOF
      GOTO1640
C
 1620 CONTINUE
      HOLD=ARG(ILEFT)
      GOTO1640
C
 1640 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PY2TOB=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1649
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1641)
 1641 FORMAT('THE TIC MARK BOTTOM OFFSET (FOR RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1642)HOLD
 1642 FORMAT('HAS JUST BEEN SET TO ',E16.7)
      CALL DPWRST('XXX','BUG ')
 1649 CONTINUE
C
C               *****************************************************
C               **  TREAT THE TOP OFFSET                           **
C               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
C               *****************************************************
C
      IF(IRIGHT.EQ.0)GOTO1660
      IF(IHARG(IRIGHT).EQ.'ON')GOTO1670
      IF(IHARG(IRIGHT).EQ.'OFF')GOTO1670
      IF(IHARG(IRIGHT).EQ.'AUTO')GOTO1670
      IF(IHARG(IRIGHT).EQ.'DEFA')GOTO1670
      IF(IHARG(IRIGHT).EQ.'FLOA')GOTO1670
      IF(IARGT(IRIGHT).EQ.'NUMB')GOTO1680
      IERROR='YES'
      GOTO1900
C
 1660 CONTINUE
      HOLD=PY2TOT
      GOTO1690
C
 1670 CONTINUE
      HOLD=DEFTOF
      GOTO1690
C
 1680 CONTINUE
      HOLD=ARG(IRIGHT)
      GOTO1690
C
 1690 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PY2TOT=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1697
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1691)
 1691 FORMAT('THE TIC MARK TOP OFFSET (FOR RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1692)HOLD
 1692 FORMAT('HAS JUST BEEN SET TO ',E16.7)
      CALL DPWRST('XXX','BUG ')
C
 1697 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
C
      ILEFT=2
      IF(IHARG(2).EQ.'OFFS')ILEFT=3
      IRIGHT=ILEFT+1
      IF(ILEFT.GT.NUMARG)ILEFT=0
      IF(IRIGHT.GT.NUMARG)IRIGHT=0
C
C               *****************************************************
C               **  TREAT THE BOTTOM OFFSET                        **
C               **  NO ARGUMENT WILL SET THE DEFAULT               **
C               *****************************************************
C
      IF(ILEFT.EQ.0)GOTO1710
      IF(IHARG(ILEFT).EQ.'ON')GOTO1710
      IF(IHARG(ILEFT).EQ.'OFF')GOTO1710
      IF(IHARG(ILEFT).EQ.'AUTO')GOTO1710
      IF(IHARG(ILEFT).EQ.'DEFA')GOTO1710
      IF(IHARG(ILEFT).EQ.'FLOA')GOTO1710
      IF(IARGT(ILEFT).EQ.'NUMB')GOTO1720
      IERROR='YES'
      GOTO1900
C
 1710 CONTINUE
      HOLD=DEFTOF
      GOTO1740
C
 1720 CONTINUE
      HOLD=ARG(ILEFT)
      GOTO1740
C
 1740 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PX1TOL=HOLD
      PX2TOL=HOLD
      PY1TOB=HOLD
      PY2TOB=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1749
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1741)
 1741 FORMAT('THE TIC MARK BOTTOM OFFSET (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1742)HOLD
 1742 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1743)
 1743 FORMAT('THE TIC MARK LEFT OFFSET (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1742)HOLD
      CALL DPWRST('XXX','BUG ')
 1749 CONTINUE
C
C               *****************************************************
C               **  TREAT THE TOP OFFSET                           **
C               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
C               *****************************************************
C
      IF(IRIGHT.EQ.0)GOTO1760
      IF(IHARG(IRIGHT).EQ.'ON')GOTO1770
      IF(IHARG(IRIGHT).EQ.'OFF')GOTO1770
      IF(IHARG(IRIGHT).EQ.'AUTO')GOTO1770
      IF(IHARG(IRIGHT).EQ.'DEFA')GOTO1770
      IF(IHARG(IRIGHT).EQ.'FLOA')GOTO1770
      IF(IARGT(IRIGHT).EQ.'NUMB')GOTO1780
      IERROR='YES'
      GOTO1900
C
 1760 CONTINUE
      GOTO1900
C
 1770 CONTINUE
      HOLD=DEFTOF
      GOTO1790
C
 1780 CONTINUE
      HOLD=ARG(IRIGHT)
      GOTO1790
C
 1790 CONTINUE
      IFOUND='YES'
      HOLD=ABS(HOLD)
      PX1TOR=HOLD
      PX2TOR=HOLD
      PY1TOT=HOLD
      PY2TOT=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1797
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1791)
 1791 FORMAT('THE TIC MARK TOP OFFSET (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1792)HOLD
 1792 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1793)
 1793 FORMAT('THE TIC MARK RIGHT OFFSET (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1792)HOLD
      CALL DPWRST('XXX','BUG ')
C
 1797 CONTINUE
C
      GOTO1900
C
 1799 CONTINUE
      GOTO1900
C
C               *****************************************************
C               **  TREAT THE OFFSET UNITS CASE                    **
C               **  NOTE THAT CURRENTLY THERE IS ONLY ONE UNITS    **
C               **  SWITCH, I.E., ALL 4 FRAME LINES WILL USE THE   **
C               **  SAME UNITS.  THE CHOICES ARE "DATA", (OFFSETS  **
C               **  IN UNITS OF THE DATA) AND "ABSOLUTE" (OFFSETS  **
C               **  IN DATAPLOT 0. TO 100. PERCENT UNITS).         **
C               *****************************************************
C
 2090 CONTINUE
      IFOUND='YES'
C
      IF(IHARG(NUMARG).EQ.'ON')GOTO2150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO2150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO2150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO2150
      IF(IHARG(NUMARG).EQ.'FLOA')GOTO2150
      IF(IHARG(NUMARG).EQ.'DATA')GOTO2160
      IF(IHARG(NUMARG).EQ.'SCRE')GOTO2170
      IF(IHARG(NUMARG).EQ.'ABSO')GOTO2170
      GOTO2150
C
 2150 CONTINUE
      ITICUN=IDEFTU
CCCCC THE FOLLOWING LINE (AND THE CONTINUE) WERE ADDED OCTOBER 1991
      IF(IFEEDB.EQ.'OFF')GOTO2159
      WRITE(ICOUT,2151)ITICUN
 2151 FORMAT('TIC MARK OFFSETS WILL BE CALCULATED IN ',A4,
     1' UNITS.')
      CALL DPWRST('XXX','BUG ')
 2159 CONTINUE
      GOTO1900
C
 2160 CONTINUE
      ITICUN='DATA'
CCCCC THE FOLLOWING LINE (AND THE CONTINUE) WERE ADDED OCTOBER 1991
      IF(IFEEDB.EQ.'OFF')GOTO2169
      WRITE(ICOUT,2161)
 2161 FORMAT('TIC MARK OFFSETS WILL BE CALCULATED IN DATA',
     1' UNITS.')
      CALL DPWRST('XXX','BUG ')
 2169 CONTINUE
      GOTO1900
C
 2170 CONTINUE
      ITICUN='ABSO'
CCCCC THE FOLLOWING LINE (AND THE CONTINUE) WERE ADDED OCTOBER 1991
      IF(IFEEDB.EQ.'OFF')GOTO2179
      WRITE(ICOUT,2171)
 2171 FORMAT('TIC MARK OFFSETS WILL BE CALCULATED IN',
     1' DATAPLOT SCREEN UNITS.')
      CALL DPWRST('XXX','BUG ')
 2179 CONTINUE
      GOTO1900
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTCPA(ICOM,IHARG,NUMARG,
     1IDEFPA,
     1IX1TPA,IX2TPA,IY1TPA,IY2TPA,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK PATTERN SWITCHES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK SWITCHES DESCRIBE
C              THE TIC MARK PATTERN ON THE 4 FRAME LINES OF A PLOT.
C              THE CONTENTS OF A TIC MARK PATTERN SWITCH ARE
C              A PATTERN.
C              THE TIC MARK PATTERN SWITCHES FOR THE 4 FRAME LINES
C              ARE CONTAINED IN THE 4 VARIABLES
C              IX1TPA,IX2TPA,IY1TPA,IY2TPA
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFPA
C     OUTPUT ARGUMENTS--IX1TPA = PATTERN FOR BOTTOM HORIZ. TICS
C                     --IX2TPA = PATTERN FOR TOP    HORIZ. TICS
C                     --IY1TPA = PATTERN FOR LEFT   VERT.  TICS
C                     --IY2TPA = PATTERN FOR RIGHT  VERT.  TICS
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IDEFPA
C
      CHARACTER*4 IX1TPA
      CHARACTER*4 IX2TPA
      CHARACTER*4 IY1TPA
      CHARACTER*4 IY2TPA
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'PATT')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFPA
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1TPA=IHOLD
      IX2TPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK PATTERN (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFPA
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1TPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK PATTERN (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFPA
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2TPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK PATTERN (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFPA
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1TPA=IHOLD
      IY2TPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK PATTERN (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFPA
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1TPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK PATTERN (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFPA
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2TPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK PATTERN (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFPA
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1TPA=IHOLD
      IX2TPA=IHOLD
      IY1TPA=IHOLD
      IY2TPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK PATTERN (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTCSZ(ICOM,IHARG,IARGT,ARG,NUMARG,
     1DEFTL,
     1PX1TLE,PX2TLE,PY1TLE,PY2TLE,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK SIZES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK SWITCHES DEFINE THE SIZE (LENGTH)
C              OF THE MAJOR TIC MARKS ON THE 4 FRAME LINES OF A PLOT.
C              (THE SIZE OF THE MINOR TIC MARKS IS ALWAYS
C              1/2 THE SIZE OF THE MAJOR TIC MARKS.)
C              THE TIC MARK SIZE SWITCHES FOR THE 4 FRAME LINES
C              ARE CONTAINED IN THE 4 VARIABLES
C              PX1TLE,PX2TLE,PY1TLE,PY2TLE,
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C                     --DEFTL
C     OUTPUT ARGUMENTS--
C                     --PX1TLE = BOTTOM HORIZONTAL TIC LENGTH
C                     --PX2TLE = TOP    HORIZONTAL TIC LENGTH
C                     --PY1TLE = LEFT   VERTICAL   TIC LENGTH
C                     --PY2TLE = RIGHT  VERTICAL   TIC LENGTH
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
      CHARACTER*4 ICOM
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
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIZE')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'SIZE')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1150
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      IERROR='YES'
      GOTO1900
C
 1150 CONTINUE
      HOLD=DEFTL
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PX1TLE=HOLD
      PX2TLE=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK SIZE (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)HOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1250
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1260
      IERROR='YES'
      GOTO1900
C
 1250 CONTINUE
      HOLD=DEFTL
      GOTO1280
C
 1260 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      PX1TLE=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK SIZE (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)HOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1350
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1360
      IERROR='YES'
      GOTO1900
C
 1350 CONTINUE
      HOLD=DEFTL
      GOTO1380
C
 1360 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      PX2TLE=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK SIZE (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)HOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1450
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1460
      IERROR='YES'
      GOTO1900
C
 1450 CONTINUE
      HOLD=DEFTL
      GOTO1480
C
 1460 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      PY1TLE=HOLD
      PY2TLE=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK SIZE (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)HOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1550
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1560
      IERROR='YES'
      GOTO1900
C
 1550 CONTINUE
      HOLD=DEFTL
      GOTO1580
C
 1560 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      PY1TLE=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK SIZE (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)HOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1650
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1660
      IERROR='YES'
      GOTO1900
C
 1650 CONTINUE
      HOLD=DEFTL
      GOTO1680
C
 1660 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      PY2TLE=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK SIZE (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)HOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'SIZE')GOTO1750
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1760
      IERROR='YES'
      GOTO1900
C
 1750 CONTINUE
      HOLD=DEFTL
      GOTO1780
C
 1760 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      PX1TLE=HOLD
      PX2TLE=HOLD
      PY1TLE=HOLD
      PY2TLE=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK SIZE (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)HOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTCTH(ICOM,IHARG,ARG,NUMARG,
     1PDEFTH,
     1PTICTH,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TIC MARK THICKNESS SWITCHES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH TIC MARK SWITCHES DESCRIBE
C              THE TIC MARK THICKNESS ON THE 4 FRAME LINES OF A PLOT.
C              THE CONTENTS OF A TIC MARK THICKNESS SWITCH ARE
C              A THICKNESS.
C              CURRENTLY, THE TIC MARK THICKNESS FOR ALL 4 SIDES
C              MUST BE THE SAME AND ARE CONTAINED IN THE VARIABLE
C              PTICTH
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --ARG    (A REAL VECTOR)
C                     --NUMARG
C                     --PDEFTH
C     OUTPUT ARGUMENTS--PTICTH = THICKNESS FOR ALL 4 FRAME SIDE TICS
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      REAL        PDEFTH
C
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      REAL        PHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION ARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'THIC')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      PHOLD=PDEFTH
      GOTO1180
C
 1160 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK THICKNESS (FOR ALL  ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      PHOLD=PDEFTH
      GOTO1280
C
 1260 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)PHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      PHOLD=PDEFTH
      GOTO1380
C
 1360 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)PHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      PHOLD=PDEFTH
      GOTO1480
C
 1460 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK THICKNESS (FOR ALL',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)PHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      PHOLD=PDEFTH
      GOTO1580
C
 1560 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)PHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      PHOLD=PDEFTH
      GOTO1680
C
 1660 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK THICKNESS (FOR ALL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)PHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      PHOLD=PDEFTH
      GOTO1780
C
 1760 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK THICKNESS (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)PHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTEBA(IHARG,IARGT,ARG,NUMARG,ADETBA,MAXTEX,ATEXBA,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TEXT BASES.
C              THESE ARE LOCATED IN THE VECTOR ATEXBA(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --ADETBA
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ATEXBA (A FLOATING POINT VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION ATEXBA(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTE'
      ISUBN2='BA  '
C
      NUMTEX=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTEBA--')
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
      WRITE(ICOUT,55)ADETBA
   55 FORMAT('ADETBA = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ATEXBA(1)
   70 FORMAT('ATEXBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ATEXBA(I)
   76 FORMAT('I,ATEXBA(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.EQ.1)GOTO1110
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      GOTO1140
C
 1110 CONTINUE
      GOTO1200
C
 1120 CONTINUE
      IF(IHARG(2).EQ.'ALL')IHOLD1='    '
      IF(IHARG(2).EQ.'ALL')HOLD1=ADETBA
      IF(IHARG(2).EQ.'ALL')GOTO1300
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(2).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(2).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(2).EQ.'ALL')GOTO1300
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(2)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(2)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      ATEXBA(1)=ADETBA
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-1
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+1
      IHOLD1=IHARG(J)
      HOLD1=ARG(J)
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=ADETBA
      IF(IHOLD1.EQ.'OFF')HOLD2=ADETBA
      IF(IHOLD1.EQ.'AUTO')HOLD2=ADETBA
      IF(IHOLD1.EQ.'DEFA')HOLD2=ADETBA
      ATEXBA(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,ATEXBA(I)
 1276 FORMAT('THE BASE OF TEXT ',I6,
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=ADETBA
      IF(IHOLD1.EQ.'OFF')HOLD2=ADETBA
      IF(IHOLD1.EQ.'AUTO')HOLD2=ADETBA
      IF(IHOLD1.EQ.'DEFA')HOLD2=ADETBA
      DO1315I=1,NUMTEX
      ATEXBA(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ATEXBA(I)
 1316 FORMAT('THE BASE OF ALL TEXTS',
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTEBA--')
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
      WRITE(ICOUT,9015)ADETBA
 9015 FORMAT('ADETBA = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ATEXBA(1)
 9030 FORMAT('ATEXBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,ATEXBA(I)
 9036 FORMAT('I,ATEXBA(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTECH(IHARG,NUMARG,
     1IDEFTC,
     1ITERCH,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TERMINATOR CHARACTOR WHICH MAY
C              BE USED TO PUT MULTIPLE COMMAND STATEMENTS
C              ON A SINGLE COMMAND LINE.
C              WHEN A COMMAND LINE IS READ,
C              IT IS SEARCHED FOR THE TERMINATOR CHARACTER;
C              IF IT IS FOUND, THE COMMAND STATEMENT
C              BEFORE THE TERMINATOR CHARACTOR IS EXECUTED;
C              AFTER EXECUTION, THE COMMAND STAEMENT AFTER THE
C              TERMINATOR CHARACTOR IS EXECUTED.
C              ANY NUMBER OF TERMINATOR CHARACTORS ARE ALLOWED PER LINE.
C              THE COMMAND CHARACTER CAPABILITY ALLOWS THE ANALYST
C              TO PACK SEVERAL COMMANDS PER LINE.
C              THE SPECIFIED TERMINATOR CHARACTOR WILL BE PLACED
C              IN THE CHARACTER VARIABLE ITERCH.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFTC (A  CHARACTER VARIABLE)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--ITERCH (A CHARACTER VARIABLE)
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
      CHARACTER*4 IDEFTC
      CHARACTER*4 ITERCH
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
C-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPTECH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFTC
   53 FORMAT('IDEFTC = ',A4)
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
      IHOLD=IDEFTC
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      ITERCH=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ITERCH
 1181 FORMAT('THE TERMINATOR CHARACTOR HAVE JUST BEEN SET TO ',
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
 9011 FORMAT('***** AT THE END       OF DPECH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR
 9012 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFTC,ITERCH
 9013 FORMAT('IDEFTC,ITERCH = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTEXT(IANS,IANSLC,IWIDTH,
     1                  ITEXTE,NCTEX,
     1                  PXSTAR,PYSTAR,PXEND,PYEND,
     1                  IGRASW,IDIASW,PRV,PDIARV,
     1                  ILINPA,ILINCO,PLINTH,
     1                  ATEXBA,ITEBLI,ITEBCO,PTEBTH,
     1                  ITEFSW,ITEFCO,
     1                  ITEPTY,ITEPLI,ITEPCO,PTEPTH,PTEPSP,
     1                  PTEXMR,ITEXCV,ATEXAN,PTEXRV,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1                  NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IDNVOF,IDNHOF,IDFONT,PDSCAL,
     1                  IMPSW2,AMPSCH,AMPSCW,
     1                  IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--WRITE OUT A TEXT STRING.
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
C     UPDATED         --DECEMBER    1986.
C     UPDATED         --JULY        1988.
C     UPDATED         --JANUARY     1989.  CALL LIST FOR OFFSET
C                                          VARIABLES (ALAN)
C     UPDATED         --MARCH       1993.
C     UPDATED         --SEPTEMBER   1993. ALLOW LOWER CASE
C     UPDATED         --MARCH       1997. DEVICE FONT SUPPORT
C     UPDATED         --DECEMBER    2018. SUPPORT FOR DEVICE ... SCALE
C                                         COMMAND (PDSCAL TO CALL LIST)
C
C-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
C
      CHARACTER*4 IANS
CCCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
CCCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      CHARACTER*4 IANSLC
C
      CHARACTER*4 IGRASW
      CHARACTER*4 IDIASW
C
      CHARACTER*4 ILINPA
      CHARACTER*4 ILINCO
C
      CHARACTER*4 ITEBLI
      CHARACTER*4 ITEBCO
      CHARACTER*4 ITEFSW
      CHARACTER*4 ITEFCO
      CHARACTER*4 ITEPTY
      CHARACTER*4 ITEPLI
      CHARACTER*4 ITEPCO
C
      CHARACTER*4 ITEXTE
      CHARACTER*4 ITEXFO
      CHARACTER*4 ITEXCA
      CHARACTER*4 ITEXJU
      CHARACTER*4 ITEXDI
      CHARACTER*4 ITEXFI
      CHARACTER*4 ITEXCO
C
      CHARACTER*4 ITEXCR
      CHARACTER*4 ITEXLF
C
      CHARACTER*4 ITEXSY
      CHARACTER*4 ITEXSP
C
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IFUNC
C
      CHARACTER*1 IREPCH
C
      CHARACTER*4 IMPSW2
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
C
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IBELSW
      CHARACTER*4 IERASW
      CHARACTER*4 ICOPSW
      CHARACTER*4 IBACCO
C
      CHARACTER*4 ICTEXT
C
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 IDIR
      CHARACTER*4 IFILL
      CHARACTER*4 ICOL
C
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
C
      CHARACTER*4 ITEXCV
C
      DIMENSION PRV(6)
      DIMENSION PDIARV(4)
      DIMENSION ITEXCV(10)
      DIMENSION PTEXRV(5)
C
      DIMENSION IANS(*)
CCCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
CCCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      DIMENSION IANSLC(*)
C
      DIMENSION ILINPA(*)
      DIMENSION ILINCO(*)
      DIMENSION PLINTH(*)
C
      DIMENSION ATEXBA(*)
      DIMENSION ITEBLI(*)
      DIMENSION ITEBCO(*)
      DIMENSION PTEBTH(*)
      DIMENSION ITEFSW(*)
      DIMENSION ITEFCO(*)
      DIMENSION ITEPTY(*)
      DIMENSION ITEPLI(*)
      DIMENSION ITEPCO(*)
      DIMENSION PTEPTH(*)
      DIMENSION PTEPSP(*)
      DIMENSION PDSCAL(*)
C
      DIMENSION ITEXTE(*)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
C
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
C
CCCCC DIMENSION ICTEXT(130)
      INCLUDE 'DPCOPA.INC'
      DIMENSION ICTEXT(MAXCH)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      PGRAXF=PRV(1)
      PGRAYF=PRV(2)
      PDIAXC=PRV(3)
      PDIAYC=PRV(4)
      PDIAX2=PRV(5)
      PDIAY2=PRV(6)
C
      PDIAHE=PDIARV(1)
      PDIAWI=PDIARV(2)
      PDIAVG=PDIARV(3)
      PDIAHG=PDIARV(4)
C
      ITEXFO=ITEXCV(1)
      ITEXCA=ITEXCV(2)
      ITEXJU=ITEXCV(3)
      ITEXDI=ITEXCV(4)
      ITEXCR=ITEXCV(5)
      ITEXLF=ITEXCV(6)
      ITEXSY=ITEXCV(7)
      ITEXSP=ITEXCV(8)
      ITEXFI=ITEXCV(9)
      ITEXCO=ITEXCV(10)
C
      PTEXHE=PTEXRV(1)
      PTEXWI=PTEXRV(2)
      PTEXVG=PTEXRV(3)
      PTEXHG=PTEXRV(4)
      PTEXTH=PTEXRV(5)
C
      IFOUND='NO'
      IERROR='NO'
C
      J2=0
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TEXT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTEXT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IWIDTH,NUMNAM,NUMDEV
   53   FORMAT('IWIDTH,NUMNAM,NUMDEV= ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANS(I),I=1,MIN(25,IWIDTH))
   54   FORMAT('(IANS(I),I=1,IWIDTH) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IDIASW,PDIAXC,PDIAYC
   58   FORMAT('IDIASW,PDIAXC,PDIAYC = ',A4,2X,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)PXSTAR,PYSTAR,PXEND,PYEND
   60   FORMAT('PXSTAR,PYSTAR,PXEND,PYEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)ILINPA(1),ILINCO(1),PLINTH(1)
   61   FORMAT('ILINPA(1),ILINCO(1),PLINTH(1) = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)ATEXBA(1),PTEBTH(1)
   62   FORMAT('ATEXBA(1),PTEBTH(1) = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ITEBLI(1),ITEBCO(1),ITEFSW(1),ITEFCO(1)
   63   FORMAT('ITEBLI(1),ITEBCO(1),ITEFSW(1),ITEFCO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)ITEPTY(1),ITEPLI(1),ITEPCO(1),PTEPTH(1),PTEPSP(1)
   65   FORMAT('ITEPTY(1),ITEPLI(1),ITEPCO(1),PTEPTH(1),PTEPSP(1) = ',
     1         3(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,66)ITEXCR,ITEXLF,PTEXMR
   66   FORMAT('ITEXCR,ITEXLF,PTEXMR = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)ITEXSY,ITEXSP,ITEXFO,ITEXCA,ITEXJU
   67   FORMAT('ITEXSY,ITEXSP,ITEXFO,ITEXCA,ITEXJU = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)ITEXDI,ATEXAN,ITEXFI,ITEXCO
   68   FORMAT('ITEXDI,ATEXAN,ITEXFI,ITEXCO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)PTEXHE,PTEXWI,PTEXVG,PTEXHG,PTEXTH
   70   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG,PTEXTH= ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        DO76I=1,NUMNAM
          WRITE(ICOUT,77)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                   IVALUE(I),VALUE(I)
   77     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)= ',
     1           I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   76   CONTINUE
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
        WRITE(ICOUT,87)ISUBG4,IERRG4,IBUGD2,IFOUND,IERROR,IREPCH
   87   FORMAT('ISUBG4,IERRG4,IBUGD2,IFOUND,IERROR,IREPCH = ',
     1         5(A4,2X),A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
C               *****************************************************
C
C               *****************************************
C               **  STEP 1.1--                         **
C               **  DETERMINE THE COMMAND              **
C               **  (TEXT) AND ITS LOCATION            **
C               **  ON THE LINE.                       **
C               **  DETERMINE THE START POSITION       **
C               **  (XSTART) OF THE FIRST CHARACTER    **
C               **  FOR THE STRING TO BE PRINTED.      **
C               *****************************************
C
      DO1115I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
C
        IF(IP3.EQ.IWIDTH)GOTO1190
        IF(IP4.EQ.IWIDTH)GOTO1190
        IF(IANS(I).EQ.'T'.AND.IANS(IP1).EQ.'E'.AND.
     1     IANS(IP2).EQ.'X'.AND.IANS(IP3).EQ.'T'.AND.
     1     IANS(IP4).EQ.' ')GOTO1190
 1115 CONTINUE
C
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN DPTEXT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      NO MATCH FOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 1190 CONTINUE
C
C               ********************************************************
C               **  STEP 1.2--                                        **
C               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING. **
C               ********************************************************
C
      IFOUND='YES'
C
      ISTART=IP5
      ISTOP=0
      IF(ISTART.LE.IWIDTH)THEN
        DO1220I=ISTART,IWIDTH
          IREV=IWIDTH-I+ISTART
          IF(IANS(IREV).NE.' ')THEN
            ISTOP=IREV
            GOTO1225
          ENDIF
 1220   CONTINUE
 1225   CONTINUE
      ENDIF
C
C               *****************************************
C               **  STEP 1.3--                         **
C               **  COPY OVER THE STRING OF INTEREST.  **
C               *****************************************
C
      IF(ISTART.GT.ISTOP .OR. ISTOP.EQ.0)THEN
        NCTEX=0
      ELSE
C       SEPTEMBER, 1987 (CHECK IF MAXIMUM SIZE STRING EXCEEDED)
        ITEMP=ISTOP-ISTART+1
        IF(ITEMP.GT.MAXCH)ITEMP=MAXCH
        ISTOP=ISTART+ITEMP-1
C
        J=0
        DO1310I=ISTART,ISTOP
          J=J+1
          J2=J
CCCCC     THE FOLLOWING LINE WAS CHANGED     SEPTEMBER 1993
CCCCC     TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
CCCCC     CHECK FOR CASE "ASIS"              OCTOBER   1993
CCCCC     ITEXTE(J)=IANS(I)
          IF(ITEXCA.EQ.'ASIS')THEN
            ITEXTE(J)=IANSLC(I)
          ELSE
            ITEXTE(J)=IANS(I)
          ENDIF
 1310   CONTINUE
        NCTEX=J2
      ENDIF
C
C               ******************************************
C               **  STEP 1.4--                          **
C               **  COPY OVER THE ORIGINAL TEXT STRING  **
C               **  SO AS TO PRESERVE IT IN COMMON.     **
C               ******************************************
C
      NCTEXT=NCTEX
      IF(NCTEX.GT.0)THEN
        DO1410I=1,NCTEX
          ICTEXT(I)=ITEXTE(I)
 1410   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 1.4--                                      **
C               **  CALL THE SUBROUTINE DPREPL                      **
C               **  WHICH WILL SCAN THE STRING FOR ALL OCCURRANCES  **
C               **  OF THE SUBSTRING VALU()                         **
C               **  AND REPLACE THEM BY THEIR LITERAL VALUES.       **
C               ******************************************************
C
      IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1IBUGD2,IERROR)
C
C               ********************************
C               **  STEP 2--                  **
C               **  STEP THROUGH EACH DEVICE  **
C               ********************************
C
      IF(NUMDEV.LE.0)GOTO9000
C  JULY, 1988.  BUG: IF DEVICE 1 OFF AND DEVICE 2 ON,
C  STARTING COORDINATES PX1 AND PY1 WERE NOT GETTING SET.
C  MOVE FROM INSIDE LOOP TO HERE.
      PX1=PXSTAR
      PY1=PYSTAR
C  END BUG FIX
      DO8000IDEVIC=1,NUMDEV
C
        IF(IDPOWE(IDEVIC).EQ.'OFF')GOTO8000
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
C               **  STEP 3--                      **
C               **  CARRY OUT OPENING OPERATIONS  **
C               **  ON THE GRAPHICS DEVICES       **
C               ************************************
C
        CALL DPOPDE()
C
        IBELSW='OFF'
        NUMRIN=0
        IERASW='OFF'
        IBACCO='JUNK'
C
        CALL DPOPPL(IGRASW,IBELSW,NUMRIN,IERASW,IBACCO)
C
C               *****************************
C               **  STEP 4--               **
C               **  WRITE OUT THE TEXT     **
C               *****************************
C
        IFONT=ITEXFO
        ICASE=ITEXCA
        IJUST=ITEXJU
        IDIR=ITEXDI
        ANGLE=ATEXAN
        IFILL=ITEXFI
        ICOL=ITEXCO
        PHEIGH=PTEXHE
        PWIDTH=PTEXWI
        PHOGAP=PTEXHG
        PVEGAP=PTEXVG
        PTHICK=PTEXTH
        ISYMBL=ITEXSY
        ISPAC=ITEXSP
C
C      JULY, 1988.  MOVE FOLLOWING 4 LINES TO BEFORE LOOP.
CCCCC   IF(IDEVIC.GE.2)GOTO1610
CCCCC   PX1=PXSTAR
CCCCC   PY1=PYSTAR
C1610   CONTINUE
C
        IF(NCTEXT.GE.1)CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1     IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1     PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1     ISYMBL,ISPAC,
     1     IMPSW2,AMPSCH,AMPSCW,
     1     PX99,PY99)
C
CCCCC   MARCH 1993.  MOVE FOLLOWING SECTION OUTSIDE LOOP.
CCCCC   IF(IDEVIC.GE.2)GOTO1690
CCCCC   PXEND=PX99
CCCCC   PYEND=PY99
CCCCC   IF(ITEXCR.EQ.'ON')PXEND=PTEXMR
CCCCC   IF(ITEXLF.EQ.'ON')PYEND=PYSTAR-PTEXHE-PTEXVG
C
CCCCC   PXSTAR=PXEND
CCCCC   PYSTAR=PYEND
C
C               ************************************
C               **  STEP 5--                      **
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
C
C  MARCH, 1993.  BUG: IF DEVICE 1 OFF AND DEVICE 2 ON,
C  NEW VALUES OF PXSTAR AND PYSTAR NOT SET.
C  MOVE FROM INSIDE LOOP TO HERE.
C
      PXEND=PX99
      PYEND=PY99
      IF(ITEXCR.EQ.'ON')PXEND=PTEXMR
      IF(ITEXLF.EQ.'ON')PYEND=PYSTAR-PTEXHE-PTEXVG
C
      PXSTAR=PXEND
      PYSTAR=PYEND
C  END CHANGE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IERROR=IERRG4
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TEXT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTEXT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NCTEX,NCTEXT
 9015   FORMAT('NCTEX,NCTEXT  = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)(ITEXTE(I),I=1,MIN(25,NCTEX))
 9016   FORMAT('(ITEXTE(I),I =1,NCTEX) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ICTEXT(I),I=1,MIN(25,NCTEXT))
 9018   FORMAT('(ICTEXT(I),I=1,NCTEXT) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)PXSTAR,PYSTAR,PXEND,PYEND
 9019   FORMAT('PXSTAR,PYSTAR,PXEND,PYEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9033)PX1,PY1,PX99,PY99
 9033   FORMAT('PX1,PY1,PX99,PY99  = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9035)IMANUF,IMODEL,IFOUND
 9035   FORMAT('IMANUF,IMODEL,IFOUND = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTFCO(IHARG,NUMARG,IDETFC,MAXTEX,ITEFCO,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TEXT FILL COLORS = THE COLORS
C              OF THE (BACKGROUND) FILL WITHIN THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR ITEFCO(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDETFC
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ITEFCO (A CHARACTER VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDETFC
      CHARACTER*4 ITEFCO
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION ITEFCO(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTF'
      ISUBN2='CO  '
C
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTFCO--')
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
      WRITE(ICOUT,55)IDETFC
   55 FORMAT('IDETFC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEFCO(1)
   70 FORMAT('ITEFCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ITEFCO(I)
   76 FORMAT('I,ITEFCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      GOTO1150
C
 1120 CONTINUE
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      ITEFCO(1)=IDETFC
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETFC
      ITEFCO(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEFCO(I)
 1276 FORMAT('THE FILL COLOR OF TEXT ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETFC
      DO1315I=1,NUMTEX
      ITEFCO(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEFCO(I)
 1316 FORMAT('THE FILL COLOR OF ALL TEXTS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTFCO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXTEX,NUMTEX,NUMARG
 9013 FORMAT('MAXTEX,NUMTEX,NUMARG = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,IHOLD2,IDETFC
 9014 FORMAT('IHOLD1,IHOLD2,IDETFC = ',2(A4,2X),A4)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
        WRITE(ICOUT,9026)IHARG(I)
 9026   FORMAT('IHARG(I) = ',A4)
        CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      DO9035I=1,10
        WRITE(ICOUT,9036)I,ITEFCO(I)
 9036   FORMAT('I,ITEFCO(I) = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTFSW(IHARG,NUMARG,IDETFS,MAXTEX,ITEFSW,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE TEXT FILL SWITCHES = THE ON/OFF SWITCHES
C              OF THE (BACKGROUND) FILL WITHIN THE TEXTS.
C              THESE ARE LOCATED IN THE VECTOR ITEFSW(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDETFS
C                     --MAXTEX
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ITEFSW (A CHARACTER VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDETFS
      CHARACTER*4 ITEFSW
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION ITEFSW(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTF'
      ISUBN2='SW  '
C
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPTFSW--')
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
      WRITE(ICOUT,55)IDETFS
   55 FORMAT('IDETFS = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEFSW(1)
   70 FORMAT('ITEFSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ITEFSW(I)
   76 FORMAT('I,ITEFSW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      GOTO1150
C
 1120 CONTINUE
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='ON'
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMTEX=1
      ITEFSW(1)='ON'
      GOTO1270
C
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO1225I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETFS
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETFS
      ITEFSW(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEFSW(I)
 1276 FORMAT('THE FILL SWITCH FOR TEXT ',I6,
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 3--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETFS
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETFS
      DO1315I=1,NUMTEX
      ITEFSW(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEFSW(I)
 1316 FORMAT('THE FILL SWITCH FOR ALL TEXTS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTFSW--')
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
      WRITE(ICOUT,9015)IDETFS
 9015 FORMAT('IDETFS = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEFSW(1)
 9030 FORMAT('ITEFSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,ITEFSW(I)
 9036 FORMAT('I,ITEFSW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTHIC(IHARG,IARGT,ARG,NUMARG,
     1PDEFTH,
     1PTEXTH,
C  DECEMBER 1987: SET ALL THICKNESS (CAN THEN
C  OVERRIDE ANY INDIVIDUALLY)
     1PFRATH,PTICTH,PTIZTH,PVGRTH,PHGRTH,PTITTH,PX1LTH,PX2LTH,PY1LTH,
     1PY2LTH,PLEGTH,MAXLG,PBOPTH,PBOFTH,MAXBX,PARRTH,MAXAR,
     1PSEGTH,MAXSG,PLINTH,MAXLN,PCHATH,MAXCH2,PFILTH,MAXFL,
     1PPATTH,MAXPT,PSPITH,MAXSP,PBABTH,PBAPTH,MAXBA,PREPTH,MAXRG,
     1PMABTH,PMAPTH,MAXMR,PTEBTH,PTEPTH,MAXTX,
C  END CHANGE
     1IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE THICKNESS FOR TEXT CHARACTERS.
C              THE THICKNESS FOR TEXT CHARACTERS WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE PTEXTH.
C     NOTE--THE THICKNESS IS IN STANDARDIZED UNITS (0.0 TO 100.0).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT
C                     --ARG
C                     --NUMARG
C                     --PDEFTH
C                     --IBUGD2
C     OUTPUT ARGUMENTS--PTEXTH
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
C     UPDATED         --JANUARY   1989.  SET ALL THICKNESS PARAMETERS (ALAN)
C     UPDATED         --SEPTEMBER 1993.  FIX BUG FORMAT STATEMENT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
C  DECEMBER 1987
      DIMENSION PLEGTH(*)
      DIMENSION PBOPTH(*)
      DIMENSION PBOFTH(*)
      DIMENSION PARRTH(*)
      DIMENSION PSEGTH(*)
      DIMENSION PLINTH(*)
      DIMENSION PCHATH(*)
      DIMENSION PFILTH(*)
      DIMENSION PPATTH(*)
      DIMENSION PSPITH(*)
      DIMENSION PBABTH(*)
      DIMENSION PBAPTH(*)
      DIMENSION PREPTH(*)
      DIMENSION PMABTH(*)
      DIMENSION PMAPTH(*)
      DIMENSION PTEBTH(*)
      DIMENSION PTEPTH(*)
C  END CHANGE
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
C
C-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPTHIC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)PDEFTH
   53 FORMAT('PDEFTH = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMARG
   54 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I),IARGT(I),ARG(I)
   56 FORMAT('I,IHARG(I),IARGT(I),ARG(I) = ',I8,2X,A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               *****************************
C               **  TREAT THE THICKNESS CASE  **
C               *****************************
C
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
C
      IF(NUMARG.GE.1.AND.IARGT(NUMARG).EQ.'NUMB')
     1GOTO1160
C
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPTHIC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR THICKNESS COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE IT IS DESIRED THAT ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      THE TEXT CHARACTERS HAVE A THICKNESS OF 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      (WHERE THE VERTICAL SCREEN UNITS RANGE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      FROM 0 TO 100, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('           THICKNESS 1 ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      PTEXTH=PDEFTH
      GOTO1180
C
 1160 CONTINUE
      PTEXTH=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
C  DECEMBER 1987: SET ALL THICKNESSES TO THE SET VALUE
      PFRATH=PTEXTH
      PTICTH=PTEXTH
      PTIZTH=PTEXTH
      PVGRTH=PTEXTH
      PHGRTH=PTEXTH
      PTITTH=PTEXTH
      PX1LTH=PTEXTH
      PX2LTH=PTEXTH
      PY1LTH=PTEXTH
      PY2LTH=PTEXTH
      DO2010I=1,MAXLG
      PLEGTH(I)=PTEXTH
 2010 CONTINUE
      DO2020I=1,MAXBX
      PBOPTH(I)=PTEXTH
      PBOFTH(I)=PTEXTH
 2020 CONTINUE
      DO2030I=1,MAXAR
      PARRTH(I)=PTEXTH
 2030 CONTINUE
      DO2040I=1,MAXSG
      PSEGTH(I)=PTEXTH
 2040 CONTINUE
      DO2050I=1,MAXLN
      PLINTH(I)=PTEXTH
 2050 CONTINUE
      DO2060I=1,MAXCH2
      PCHATH(I)=PTEXTH
 2060 CONTINUE
      DO2070I=1,MAXFL
      PFILTH(I)=PTEXTH
 2070 CONTINUE
      DO2080I=1,MAXPT
      PPATTH(I)=PTEXTH
 2080 CONTINUE
      DO2090I=1,MAXSP
      PSPITH(I)=PTEXTH
 2090 CONTINUE
      DO2100I=1,MAXBA
      PBABTH(I)=PTEXTH
      PBAPTH(I)=PTEXTH
 2100 CONTINUE
      DO2110I=1,MAXRG
      PREPTH(I)=PTEXTH
 2110 CONTINUE
      DO2120I=1,MAXMR
      PMABTH(I)=PTEXTH
      PMAPTH(I)=PTEXTH
 2120 CONTINUE
      DO2130I=1,MAXTX
      PTEBTH(I)=PTEXTH
      PTEPTH(I)=PTEXTH
 2130 CONTINUE
C  END CHANGE
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE THICKNESS (FOR TEXT CHARACTERS)  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PTEXTH
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
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
      WRITE(ICOUT,8111)PTEXTH
 8111 FORMAT('THE CURRENT (TEXT) THICKNESS IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)PDEFTH
 8112 FORMAT('THE DEFAULT (TEXT) THICKNESS IS ',E15.7)
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
 9011 FORMAT('***** AT THE END       OF DPTHIC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
CCCCC THE FOLLOWING LINE WAS FIXED    SEPTEMBER 1993
C9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',,A4,2X,A4,2X,A4)
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)PTEXTH
 9013 FORMAT('PTEXTH = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPTIC(ICOM,IHARG,NUMARG,
     1IX1TSW,IX2TSW,IY1TSW,IY2TSW,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 4 TIC MARK SWITCHES CONTAINED IN THE
C              4 VARIABLES IX1TSW,IX2TSW,IY1TSW,IY2TSW
C              SUCH TIC MARK SWITCHES TURN ON OR OFF
C              THE TIC MARKS ON THE 4 FRAME LINES OF A PLOT.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
C                     --IX1TSW = LOWER HORIZONTAL FRAME TIC MARKS
C                     --IX2TSW = UPPER HORIZONTAL FRAME TIC MARKS
C                     --IY1TSW = LEFT  VERTICAL   FRAME TIC MARKS
C                     --IY2TSW = RIGHT VERTICAL   FRAME TIC MARKS
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
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1988. (ALLOW FOR TIC NUMBER COMMAND)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IX1TSW
      CHARACTER*4 IX2TSW
      CHARACTER*4 IY1TSW
      CHARACTER*4 IY2TSW
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'POSI')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIZE')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'HW')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LABE')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLAC')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NUMB')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'OFFS')GOTO1900
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'COLO')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'COOR')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'POSI')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'SIZE')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'HW')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'LABE')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'DECI')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'PLAC')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'NUMB')GOTO1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'OFFS')GOTO1900
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XTIC')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(NUMARG.LE.0)GOTO1160
      IF(IHARG(NUMARG).EQ.'MARK')GOTO1160
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
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
      IX1TSW=IHOLD
      IX2TSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARKS (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1TI')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(NUMARG.LE.0)GOTO1260
      IF(IHARG(NUMARG).EQ.'MARK')GOTO1260
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1260
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      GOTO1250
C
 1250 CONTINUE
      IHOLD='ON'
      GOTO1280
C
 1260 CONTINUE
      IHOLD='OFF'
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1TSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARKS (FOR THE BOTTOM ',
     1'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2TI')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(NUMARG.LE.0)GOTO1360
      IF(IHARG(NUMARG).EQ.'MARK')GOTO1360
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1360
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      GOTO1350
C
 1350 CONTINUE
      IHOLD='ON'
      GOTO1380
C
 1360 CONTINUE
      IHOLD='OFF'
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2TSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARKS (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YTIC')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(NUMARG.LE.0)GOTO1460
      IF(IHARG(NUMARG).EQ.'MARK')GOTO1460
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1460
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      GOTO1450
C
 1450 CONTINUE
      IHOLD='ON'
      GOTO1480
C
 1460 CONTINUE
      IHOLD='OFF'
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1TSW=IHOLD
      IY2TSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARKS (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1TI')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(NUMARG.LE.0)GOTO1560
      IF(IHARG(NUMARG).EQ.'MARK')GOTO1560
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1560
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      GOTO1550
C
 1550 CONTINUE
      IHOLD='ON'
      GOTO1580
C
 1560 CONTINUE
      IHOLD='OFF'
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1TSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARKS (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2TI')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(NUMARG.LE.0)GOTO1660
      IF(IHARG(NUMARG).EQ.'MARK')GOTO1660
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1660
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      GOTO1650
C
 1650 CONTINUE
      IHOLD='ON'
      GOTO1680
C
 1660 CONTINUE
      IHOLD='OFF'
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2TSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARKS (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
C               *****************************************************
C
      IF(ICOM.EQ.'TIC')GOTO1700
      IF(ICOM.EQ.'TICS')GOTO1700
      IF(ICOM.EQ.'XYTI')GOTO1700
      IF(ICOM.EQ.'YXTI')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(NUMARG.LE.0)GOTO1760
      IF(IHARG(NUMARG).EQ.'MARK')GOTO1760
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1760
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      GOTO1750
C
 1750 CONTINUE
      IHOLD='ON'
      GOTO1780
C
 1760 CONTINUE
      IHOLD='OFF'
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1TSW=IHOLD
      IX2TSW=IHOLD
      IY1TSW=IHOLD
      IY2TSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARKS (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPTICA(IHARG,NUMARG,IDEFCA,ITITCA,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE CASE FOR THE TITLE
C              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
C              THE CASE FOR THE TITLE WILL BE PLACED
C              IN THE HOLLERITH VARIABLE ITITCA.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFCA
C     OUTPUT ARGUMENTS--ITITCA
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCA
      CHARACTER*4 ITITCA
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1199
      IF(IHARG(1).EQ.'CASE')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(NUMARG.EQ.1)GOTO1150
      GOTO1160
C
 1150 CONTINUE
      ITITCA=IDEFCA
      GOTO1180
C
 1160 CONTINUE
      ITITCA=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ITITCA
 1181 FORMAT('THE TITLE CASE HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPTICL(IHARG,NUMARG,IDEFCO,ITITCO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE COLOR FOR THE TITLE
C              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
C              THE COLOR FOR THE TITLE WILL BE PLACED
C              IN THE HOLLERITH VARIABLE ITITCO.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFCO
C     OUTPUT ARGUMENTS--ITITCO
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
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCO
      CHARACTER*4 ITITCO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1199
      IF(IHARG(1).EQ.'COLO')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(NUMARG.EQ.1)GOTO1150
      GOTO1160
C
 1150 CONTINUE
      ITITCO=IDEFCO
      GOTO1180
C
 1160 CONTINUE
      ITITCO=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ITITCO
 1181 FORMAT('THE TITLE COLOR HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPTIET(XTEMP1,MAXNXT,
     1                  ICAPSW,ICASAN,IFORSW,ISEED,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--PERFORM TIETJEN-MOORE TEST FOR UNIVARIATE OUTLIERS.
C              THIS IS A GENERALIZATION OF THE GRUBB TEST (WHICH
C              LOOKS FOR A SINGLE OUTLIER) TO LOOK FOR "K" OUTLIERS.
C              LIKE GRUBBS TEST, THIS TEST ASSUMES THE DATA FOLLOWS AN
C              APPROXIMATELY NORMAL DISRIBUTION).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/11
C     ORIGINAL VERSION--NOVEMBER  2009.
C     UPDATED         --JANUARY   2009. PRINT VALUES OF POTENTIAL
C                                       OUTLIERS
C     UPDATED         --AUGUST    2010. FOR TWO-SIDED CASE, POTENTIAL
C                                       OUTLIERS PRINTED WERE CORRECT
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
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
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IDATSW
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICASE
      CHARACTER*4 IRANSV
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
C
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
      DIMENSION XTEMP4(MAXOBV)
      DIMENSION YSTAT(MAXOBV)
C
      DIMENSION XDESGN(MAXOBV,7)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
C
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB4),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),XTEMP3(1))
      EQUIVALENCE (GARBAG(IGARB6),XTEMP4(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB9),YSTAT(1))
      EQUIVALENCE (GARBAG(IGAR10),XIDTEM(1))
      EQUIVALENCE (GARBAG(JGAR11),XIDTE2(1))
      EQUIVALENCE (GARBAG(JGAR12),XIDTE3(1))
      EQUIVALENCE (GARBAG(JGAR13),XIDTE4(1))
      EQUIVALENCE (GARBAG(JGAR14),XIDTE5(1))
      EQUIVALENCE (GARBAG(JGAR15),XIDTE6(1))
      EQUIVALENCE (GARBAG(IGAR11),XDESGN(1,1))
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOF2.INC'
C
      COMMON/ISED/ISED1,ISED2,ISED3,ISED4,ISED5,ISED6,
     1            ISED7,ISED8,ISED9,ISED10,ISED11
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      ICASAN='    '
      IREPL='OFF'
      IMULT='OFF'
      IRANSV=IRANAL
      IRANAL='FINC'
      ISEESV=ISEED
      ISEED=2503
      ISUBN1='DPTI'
      ISUBN2='ET  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      MINN2=3
C
C               ***************************************************
C               **  TREAT THE TIETJEN MOORE             CASE     **
C               ***************************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIET')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTIET--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASAN
   52   FORMAT('ICASAN = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASAN,IBUGA2,IBUGA3,IBUGQ,MAXNXT
   53   FORMAT('ICASAN,IBUGA2,IBUGA3,IBUGQ,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************************
C               **  STEP 1--                                           **
C               **  EXTRACT THE COMMAND                                **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:            **
C               **    1) TIETJEN MOORE TEST Y                          **
C               **    2) TIETJEN MOORE TEST Y LABID                    **
C               **    3) TIETJEN MOORE TEST Y1 ... YK                  **
C               **    4) REPLICATED TIETJEN MOORE TEST Y X1 ... XK     **
C               **    5) REPLICATED TIETJEN MOORE TEST Y LABID X1 ... XK *
C               *********************************************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTC=9999
      ILASTZ=9999
      IFOUND='NO'
      ICASAN='TWOS'
C
      DO100I=0,NUMARG-1
C
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
          ICTMP2=IHARG(I+1)
          ICTMP3=IHARG(I+2)
        ELSE
          ICTMP1=IHARG(I)
          ICTMP2=IHARG(I+1)
          ICTMP3=IHARG(I+2)
        ENDIF
C
        IF(ICTMP1.EQ.'TIET' .AND. ICTMP2.EQ.'MOOR' .AND.
     1     ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+2
        ELSEIF(ICTMP1.EQ.'TIET' .AND. ICTMP2.EQ.'MOOR')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'MINI')THEN
          ICASAN='MINI'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'MAXI')THEN
          ICASAN='MAXI'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'MULT')THEN
          IMULT='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'TEST')THEN
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ENDIF
  100 CONTINUE
C
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGA2,IERROR)
C
      IF(IFOUND.EQ.'NO')GOTO9000
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN TIETJEN-MOORE TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      THE TIETJEN-MOORE TEST COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='TIETJEN-MOORE TEST FOR OUTLIERS'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        NRESP=1
        NLABID=NUMVAR-NRESP
        IF(NLABID.GT.1)NLABID=1
      ENDIF
C
      IHP='NOUT'
      IHP2='LIER'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        IR=1
      ELSE
        AR=VALUE(ILOCV)
        IR=INT(AR+0.1)
        IF(IR.LT.1)IR=1
      ENDIF
C
      IOP='OPEN'
      IFLAG1=0
      IFLAG2=1
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')THEN
        WRITE(ICOUT,521)NRESP,NLABID,NREPL,IR
  521   FORMAT('NRESP,NLABID,NREPL,IR = ',4I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 6--                                        **
C               **  GENERATE THE TIETJEN-MOORE TEST FOR THE VARIOUS **
C               **  CASES                                           **
C               ******************************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 7A--                          **
C               **  CASE 1: SINGLE RESPONSE VARIABLE   **
C               **          WITH NO REPLICATION        **
C               *****************************************
C
      IF(NRESP.EQ.1 .AND. NREPL.EQ.0)THEN
        ISTEPN='7A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
C
        ICOL=1
        NUMVA2=1
        IF(NLABID.GE.1)NUMVA2=2
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,X1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C       *****************************************************
C       **  STEP 7B--                                      **
C       **  CALL DPTIE2 TO PERFORM THE OUTLIER TEST.       **
C       *****************************************************
C
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIET')THEN
          ISTEPN='7B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,711)
  711     FORMAT('***** FROM THE MIDDLE  OF DPTIET--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,712)ICASAN,NUMVAR,IDATSW,NLOCAL
  712     FORMAT('ICASAN,NUMVAR,IDATSW,NQ = ',
     1           A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO715I=1,NLOCAL
              WRITE(ICOUT,716)I,Y1(I),X1(I)
  716         FORMAT('I,Y1(I),X1(I) = ',I8,2F12.5)
              CALL DPWRST('XXX','BUG ')
  715       CONTINUE
          ENDIF
        ENDIF
C
        NREPL=0
        NCURVE=1
        CALL DPTIE2(Y1,X1,NLOCAL,ICASAN,IOUNI2,ISEED,
     1              YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1              ITEMP1,ITEMP2,ITEMP3,
     1              PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1              ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1              STATVA,STATCD,PVAL,
     1              CUT0,CUT01,CUT025,CUT05,CUT10,
     1              CUT25,CUT50,CUT100,
     1              ISUBRO,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 7C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
        ISTEPN='7C'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IFLAGU='ON'
        IFRST=.FALSE.
        ILAST=.FALSE.
        CALL DPTIE4(STATVA,STATCD,PVAL,
     1              CUT0,CUT01,CUT025,CUT05,CUT10,
     1              CUT25,CUT50,CUT100,
     1              IFLAGU,IFRST,ILAST,ICASP2,
     1              IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES **
C               **          NOTE THAT A LABID VARIABLE  **
C               **          IS NOT SUPPORTED FOR THIS   **
C               **          CASE.                       **
C               ******************************************
C
      ELSEIF(NRESP.GT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')THEN
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
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          DO820I=1,NLOCAL
            X1(I)=REAL(I)
  820     CONTINUE
C
C         *****************************************************
C         **  STEP 8B--                                      **
C         **  CALL DPTIE2 TO PERFORM THE OUTLIER TEST.       **
C         *****************************************************
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIET')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPTIET--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,IDATSW,NLOCAL
  823       FORMAT('ICASAN,NUMVAR,IDATSW,NQ = ',
     1             A4,I8,2X,A4,I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO825I=1,NLOCAL
                WRITE(ICOUT,826)I,Y1(I),X1(I)
  826           FORMAT('I,Y1(I),X1(I) = ',I8,2F12.5)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
C
          CALL DPTIE2(Y1,X1,NLOCAL,ICASAN,IOUNI2,ISEED,
     1                YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                ITEMP1,ITEMP2,ITEMP3,
     1                PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1                ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                STATVA,STATCD,PVAL,
     1                CUT0,CUT01,CUT025,CUT05,CUT10,
     1                CUT25,CUT50,CUT100,
     1                ISUBRO,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  COMPUTE GRUBB     STAT           **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IFLAGU='FILE'
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          IFLAGU='ON'
          IFRST=.FALSE.
          ILAST=.FALSE.
          CALL DPTIE4(STATVA,STATCD,PVAL,
     1                CUT0,CUT01,CUT025,CUT05,CUT10,
     1                CUT25,CUT50,CUT100,
     1                IFLAGU,IFRST,ILAST,ICASP2,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
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
      ELSEIF(IREPL.EQ.'ON')THEN
        ISTEPN='9A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO910I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO910
          J=J+1
C
C         RESPONSE VARIABLE IN Y1
C
          ICOLC=1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)Y1(J)=TAGPLO(I)
C
C         LABID VARIABLE IN X1
C
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
        ISTEPN='9B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       NOTE: CHECK TO SEE IF X1 HAS ALL UNIQUE ELEMENTS.  IF NOT,
C             THEN INTERPRET THIS AS A REPLICATION VARIABLE.
C
        CALL DISTIN(X1,NLOCAL,IWRITE,XTEMP2,NDIST,IBUGA3,IERROR)
        IF(NLOCAL.NE.NDIST)THEN
          NLABID=0
          IF(NREPL.GT.6)NREPL=6
          IF(NREPL.GE.1)THEN
            DO930J=1,NREPL-1
              DO935I=1,NLOCAL
                XDESGN(I,J+1)=XDESGN(I,J)
  935         CONTINUE
  930       CONTINUE
          ENDIF
          NREPL=NREPL+1
          DO938I=1,NLOCAL
            XDESGN(I,1)=X1(I)
            X1(I)=REAL(I)
  938     CONTINUE
        ENDIF
C
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
        IF(NLABID.EQ.1)THEN
          PID(2)=CPUMIN
          IVARID(2)=IVARN1(2)
          IVARI2(2)=IVARN2(2)
        ENDIF
        IADD=NRESP+NLABID
        DO940II=1,NREPL
          IVARID(II+IADD)=IVARN1(II+IADD)
          IVARI2(II+IADD)=IVARN2(II+IADD)
  940   CONTINUE
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
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIET')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPTIET--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,942)ICASAN,NUMVAR,IDATSW,NLOCAL,NREPL
  942     FORMAT('ICASAN,NUMVAR,IDATSW,NLOCAL,NREPL = ',
     1           A4,I8,2X,A4,2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO945I=1,NLOCAL
              WRITE(ICOUT,946)I,Y1(I),X1(I),XDESGN(I,1),XDESGN(I,2)
  946         FORMAT('I,Y1(I),X1(I),XDESGN(I,1),XDESGN(I,2) = ',
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
     1             XTEMP1,XTEMP2,
     1             NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,
     1             IBUGA3,ISUBRO,IERROR)
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
            PID(IADD+1)=XIDTEM(ISET1)
            DO1130I=1,NLOCAL
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
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,
     1                    YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                    ITEMP1,ITEMP2,ITEMP3,
     1                    PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT01,CUT025,CUT05,CUT10,
     1                    CUT25,CUT50,CUT100,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NUMSE1)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,
     1                  CUT0,CUT01,CUT025,CUT05,CUT10,
     1                  CUT25,CUT50,CUT100,
     1                  IFLAGU,IFRST,ILAST,ICASP2,
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,
     1                    YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                    ITEMP1,ITEMP2,ITEMP3,
     1                    PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT01,CUT025,CUT05,CUT10,
     1                    CUT25,CUT50,CUT100,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,
     1                  CUT0,CUT01,CUT025,CUT05,CUT10,
     1                  CUT25,CUT50,CUT100,
     1                  IFLAGU,IFRST,ILAST,ICASP2,
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,
     1                    YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                    ITEMP1,ITEMP2,ITEMP3,
     1                    PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT01,CUT025,CUT05,CUT10,
     1                    CUT25,CUT50,CUT100,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,
     1                  CUT0,CUT01,CUT025,CUT05,CUT10,
     1                  CUT25,CUT50,CUT100,
     1                  IFLAGU,IFRST,ILAST,ICASP2,
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,
     1                    YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                    ITEMP1,ITEMP2,ITEMP3,
     1                    PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT01,CUT025,CUT05,CUT10,
     1                    CUT25,CUT50,CUT100,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,
     1                  CUT0,CUT01,CUT025,CUT05,CUT10,
     1                  CUT25,CUT50,CUT100,
     1                  IFLAGU,IFRST,ILAST,ICASP2,
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,
     1                    YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                    ITEMP1,ITEMP2,ITEMP3,
     1                    PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT01,CUT025,CUT05,CUT10,
     1                    CUT25,CUT50,CUT100,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,
     1                  CUT0,CUT01,CUT025,CUT05,CUT10,
     1                  CUT25,CUT50,CUT100,
     1                  IFLAGU,IFRST,ILAST,ICASP2,
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
                TEMP1(K)=Y1(I)
                TEMP2(K)=X1(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,
     1                    YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                    ITEMP1,ITEMP2,ITEMP3,
     1                    PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT01,CUT025,CUT05,CUT10,
     1                    CUT25,CUT50,CUT100,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,
     1                  CUT0,CUT01,CUT025,CUT05,CUT10,
     1                  CUT25,CUT50,CUT100,
     1                  IFLAGU,IFRST,ILAST,ICASP2,
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
      IRANAL=IRANSV
      ISEED=ISEESV
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
C
      IF(IERROR.EQ.'YES')THEN
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9001)(IANS(I),I=1,MIN(100,IWIDTH))
 9001     FORMAT(100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIET')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTIET--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTP,NS,ICASAN
 9013   FORMAT('NPLOTP,NS,ICASAN = ',I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTIE2(Y,X,N,ICASAN,IOUNI2,ISEED,
     1                  YSTAT,TEMP1,TEMP2,TEMP3,TEMP4,
     1                  ITEMP1,ITEMP2,ITEMP3,
     1                  PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,
     1                  ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                  STATVA,STATCD,PVAL,
     1                  CUT0,CUT01,CUT025,CUT05,CUT10,
     1                  CUT25,CUT50,CUT100,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT THE TIETJEN-MOORE TEST FOR
C              UNIVARIATE OUTLIERS (DATA ASSUMED TO FOLLOW AN
C              APPROXIMATELY NORMAL DISTRIBUTION).  THE NUMBER OF
C              SUSPECTED OUTLIERS MUST BE SPECIFIED IN ADVANCE.
C     EXAMPLE--TIETJEN-MOORE TEST Y
C     REFERENCE--GARY TIETJEN AND ROGER MOORE (AUGUST 1972), "SOME
C                GRUBBS-TYPE STATISTICS FOR THE DETECTION OF SEVERAL
C                OUTLIERS", TECHNOMETRICS, VOL. 14, NO. 3, PP. 583-597.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/11
C     ORIGINAL VERSION--NOVEMBER  2009.
C     UPDATED         --JULY      2014. ADD SKEWNESS AND KURTOSIS TO
C                                       SUMMARY STATISTICS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASAN
C
      CHARACTER*40 IRTFFF
      CHARACTER*40 IRTFFP
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
C
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
C
      CHARACTER*1  IBASLC
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=50)
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
      LOGICAL IFLAG1
      LOGICAL IFLAG2
      LOGICAL IFLAG3
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION YSTAT(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION PID(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA ALPHA/
     1 0.0, 1.0, 2.5, 5.0, 10.0, 25.0, 50.0, 100.0/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTI'
      ISUBN2='E2  '
      IERROR='NO'
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
      CUT0=CPUMIN
      CUT01=CPUMIN
      CUT025=CPUMIN
      CUT05=CPUMIN
      CUT10=CPUMIN
      CUT25=CPUMIN
      CUT50=CPUMIN
      CUT100=CPUMIN
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTIE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)ISUBRO,IBUGA3,ICASAN
   52   FORMAT('ISUBRO,IBUGA3,ICASAN = ',3(A4,2X))
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N
   55   FORMAT('N = ',I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),X(I)
   57     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN TIETJEN-MOORE TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 3.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1114)N
 1114   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IR.GE.N/2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1121)
 1121   FORMAT('      THE SPECIFIED NUMBER OF SUSPECTED OUTLIERS IS ',
     1         'GREATER THAN N/2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1123)IR
 1123   FORMAT('THE SUSPECTED NUMBER OF OUTLIERS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1125)N
 1125   FORMAT('THE SAMPLE SIZE                  = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO1135I=2,N
        IF(Y(I).NE.HOLD)GOTO1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)HOLD
 1131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
 1139 CONTINUE
C
C               ************************************
C               **  STEP 21--                     **
C               **  CARRY OUT CALCULATIONS        **
C               **  FOR    TIETJEN-MOORE    TEST  **
C               ************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPTIE3(Y,N,ICASAN,IR,
     1            TEMP1,TEMP2,TEMP3,ITEMP1,ITEMP3,
     1            STATVA,YMEAN,YSD,YMIN,YMAX,
     1            ISUBRO,IBUGA3,IERROR)
C
      CALL STMOM3(Y,N,IWRITE,YSKEW,IBUGA3,IERROR)
      CALL STMOM4(Y,N,IWRITE,YKURT,IBUGA3,IERROR)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')THEN
        WRITE(ICOUT,2131)YMEAN,YSD,YMIN,YMAX,STATVA
 2131   FORMAT('YMEAN,YSD,YMIN,YMAX,STATVA = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C               ************************************
C               **  STEP 22--                     **
C               **  COMPUTE CRITICAL VALUES VIA   **
C               **  MONTE-CARLO SIMULATION        **
C               ************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NMCSAM=10000
      NTEMP=N
      DO2210I=1,NMCSAM
        CALL NORRAN(NTEMP,ISEED,TEMP4)
        CALL DPTIE3(TEMP4,NTEMP,ICASAN,IR,
     1              TEMP1,TEMP2,TEMP3,ITEMP1,ITEMP2,
     1              STATV2,YMEAN2,YSD2,YMIN2,YMAX2,
     1              ISUBRO,IBUGA3,IERROR)
        YSTAT(I)=STATV2
        WRITE(IOUNI2,'(3I8,2X,E15.7)')NCURVE,NREPL,I,YSTAT(I)
 2210 CONTINUE
      IDIR='LOWE'
      CALL DPGOF8(YSTAT,NMCSAM,STATVA,PVAL,IDIR,
     1            IBUGA3,ISUBRO,IERROR)
      STATCD=1.0 - PVAL
      CUT0=YSTAT(1)
      CUT100=YSTAT(NMCSAM)
      IWRITE='OFF'
      DO2220I=2,7
        P100=ALPHA(I)
        CALL PERCEN(P100,YSTAT,NMCSAM,IWRITE,TEMP1,NMCSAM,
     1              XSTAT,IBUGA3,IERROR)
        IF(I.EQ.2)CUT01=XSTAT
        IF(I.EQ.3)CUT025=XSTAT
        IF(I.EQ.4)CUT05=XSTAT
        IF(I.EQ.5)CUT10=XSTAT
        IF(I.EQ.6)CUT25=XSTAT
        IF(I.EQ.7)CUT50=XSTAT
 2220 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')THEN
        WRITE(ICOUT,2231)PVAL,STATCD,CUT0,CUT01,CUT025
 2231   FORMAT('PVAL,STATCD,CUT0,CUT01,CUT025 = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2233)CUT05,CUT10,CUT25,CUT50,CUT100
 2233   FORMAT('CUT05,CUT10,CUT25,CUT50,CUT100 = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C
C               *********************************
C               **   STEP 42--                 **
C               **   WRITE OUT EVERYTHING      **
C               **   FOR TIETJEN-MOORE TEST    **
C               *********************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
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
      IF(ICASAN.EQ.'TWOS')THEN
        ITITLE=
     1  'Tietjen-Moore Test for Multiple Outliers: Two-Sided Case'
        NCTITL=56
        ITITLZ='(Assumption: Normality)'
        NCTITZ=23
      ELSEIF(ICASAN.EQ.'MINI')THEN
        ITITLE='Tietjen-Moore Test for Multiple Outliers: Minimum Case'
        NCTITL=54
        ITITLZ='(Assumption: Normality)'
        NCTITZ=23
      ELSEIF(ICASAN.EQ.'MAXI')THEN
        ITITLE='Tietjen-Moore Test for Multiple Outliers: Maximum Case'
        NCTITL=54
        ITITLZ='(Assumption: Normality)'
        NCTITZ=23
      ENDIF
C
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
C
      IF(NREPL.GT.0)THEN
        NRESP=1
        IADD=NLABID+NRESP
        DO4101I=1,NREPL
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
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: There are no outliers'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
C
      ITEXT(ICNT)(1:8)='Ha: The '
      WRITE(ITEXT(ICNT)(9:13),'(I5)')IR
      ISTRT=N-IR+1
      IF(ICASAN.EQ.'TWOS')THEN
        ITEXT(ICNT)(14:46)=' most extreme points are outliers'
        NCTEXT(ICNT)=46
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        DO4111I=ISTRT,N
          ICNT=ICNT+1
          ITEXT(ICNT)='Potential Outlier Value Tested:'
          NCTEXT(ICNT)=31
CCCCC     DPTIE3 SORTS Y APPROPRIATELY, SO ITEMP3 RETURNS WRONG
CCCCC     VALUE, JUST PRINT THE Y
CCCCC     INDOUT=ITEMP3(I)
CCCCC     AVALUE(ICNT)=Y(INDOUT)
          AVALUE(ICNT)=Y(I)
          IDIGIT(ICNT)=NUMDIG
 4111   CONTINUE
      ELSEIF(ICASAN.EQ.'MINI')THEN
        ITEXT(ICNT)(14:41)=' minimum points are outliers'
        NCTEXT(ICNT)=41
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        DO4113I=ISTRT,N
          ICNT=ICNT+1
          ITEXT(ICNT)='Potential Outlier Value Tested:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=Y(I)
          IDIGIT(ICNT)=NUMDIG
 4113   CONTINUE
      ELSEIF(ICASAN.EQ.'MAXI')THEN
        ITEXT(ICNT)(14:41)=' maximum points are outliers'
        NCTEXT(ICNT)=41
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        DO4115I=ISTRT,N
          ICNT=ICNT+1
          ITEXT(ICNT)='Potential Outlier Value Tested:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=Y(I)
          IDIGIT(ICNT)=NUMDIG
 4115   CONTINUE
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
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN
      IDIGIT(ICNT)=NUMDIG
CCCCC ICNT=ICNT+1
CCCCC ITEXT(ICNT)='ID for Sample Minimum:'
CCCCC NCTEXT(ICNT)=22
CCCCC AVALUE(ICNT)=X(INDMIN)
CCCCC IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX
      IDIGIT(ICNT)=NUMDIG
CCCCC ICNT=ICNT+1
CCCCC ITEXT(ICNT)='ID for Sample Maximum:'
CCCCC NCTEXT(ICNT)=22
CCCCC AVALUE(ICNT)=X(INDMAX)
CCCCC IDIGIT(ICNT)=0
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
      ITEXT(ICNT)='Sample Skewness:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=YSKEW
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Kurtosis:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=YKURT
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Tietjen-Moore Test Statistic Value:'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
C
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
      DO4210I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE=' '
      NCTITL=0
C
      ITITL9=' '
      NCTIT9=0
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
      DO4221I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.2)NTOT(I)=5
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
 4221 CONTINUE
      ITYPCO(2)='ALPH'
      IDIGIT(1)=1
      IDIGIT(3)=3
      DO4223I=1,NUMROW
        DO4225J=1,NUMCOL
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
            IF(I.EQ.1)THEN
              AMAT(I,J)=RND(CUT0,IDIGIT(J))
            ELSEIF(I.EQ.2)THEN
              AMAT(I,J)=RND(CUT01,IDIGIT(J))
            ELSEIF(I.EQ.3)THEN
              AMAT(I,J)=RND(CUT025,IDIGIT(J))
            ELSEIF(I.EQ.4)THEN
              AMAT(I,J)=RND(CUT05,IDIGIT(J))
            ELSEIF(I.EQ.5)THEN
              AMAT(I,J)=RND(CUT10,IDIGIT(J))
            ELSEIF(I.EQ.6)THEN
              AMAT(I,J)=RND(CUT25,IDIGIT(J))
            ELSEIF(I.EQ.7)THEN
              AMAT(I,J)=RND(CUT50,IDIGIT(J))
            ELSEIF(I.EQ.8)THEN
              AMAT(I,J)=RND(CUT100,IDIGIT(J))
            ENDIF
          ENDIF
 4225   CONTINUE
 4223 CONTINUE
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
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
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
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CDF1=CUT10
      CDF2=CUT05
      CDF3=CUT025
      CDF4=CUT01
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions (Lower 1-Tailed Test)'
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
      DO4321I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
        IF(I.EQ.3)NTOT(I)=17
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=3
        ITYPCO(I)='ALPH'
 4321 CONTINUE
      ITYPCO(3)='NUME'
      IDIGIT(1)=0
      IDIGIT(2)=0
      DO4323I=1,NUMROW
        DO4325J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
 4325   CONTINUE
 4323 CONTINUE
      IVALUE(1,1)='10%'
      IVALUE(2,1)='5%'
      IVALUE(3,1)='2.5%'
      IVALUE(4,1)='1%'
      IVALUE(1,2)='10%'
      IVALUE(2,2)='5%'
      IVALUE(3,2)='2.5%'
      IVALUE(4,2)='1%'
      NCVALU(1,1)=3
      NCVALU(2,1)=2
      NCVALU(3,1)=4
      NCVALU(4,1)=2
      NCVALU(1,2)=3
      NCVALU(2,2)=2
      NCVALU(3,2)=4
      NCVALU(4,2)=2
      IVALUE(1,4)='Accept H0'
      IVALUE(2,4)='Accept H0'
      IVALUE(3,4)='Accept H0'
      IVALUE(4,4)='Accept H0'
      NCVALU(1,4)=9
      NCVALU(2,4)=9
      NCVALU(3,4)=9
      NCVALU(4,4)=9
      IF(STATVA.LT.CDF1)IVALUE(1,4)='Reject H0'
      IF(STATVA.LT.CDF2)IVALUE(2,4)='Reject H0'
      IF(STATVA.LT.CDF3)IVALUE(3,4)='Reject H0'
      IF(STATVA.LT.CDF4)IVALUE(4,4)='Reject H0'
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
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')
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
      ITITLE(1:26)='*Critical Values Based on '
      WRITE(ITITLE(27:34),'(I8)')NMCSAM
      ITITLE(35:58)=' Monte Carlo Simulations'
      NCTITL=58
C
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'HTML')THEN
        CALL DPHTMV(ITITLE,NCTITL,CPUMIN,NUMDIG)
      ELSEIF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'LATE')THEN
        CALL DPLATV(ITITLE,NCTITL,CPUMIN,NUMDIG)
        IFLAG1=.FALSE.
        IFLAG2=.TRUE.
        IFLAG3=.TRUE.
        CALL DPLATZ(IFLAG1,IFLAG2,IFLAG3,NHEAD)
      ELSEIF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'RTF ')THEN
C
        CALL DPCONA(92,IBASLC)
        IRTFMD='OFF'
        IPTSZ=14
        WRITE(ICOUT,8199)IBASLC,IPTSZ
 8199   FORMAT(A1,'fs',I2)
        CALL DPWRST(ICOUT,'WRIT')
        IF(IRTFFF.EQ.'Courier New')THEN
          ITEMP=1
        ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
          ITEMP=8
        ENDIF 
        WRITE(ICOUT,8301)IBASLC,ITEMP
        CALL DPWRST(ICOUT,'WRIT')
        CALL DPRTFZ(ITITLE,NCTITL,CPUMIN,NUMDIG)
        IF(IRTFFP.EQ.'Times New Roman')THEN
          ITEMP=0
        ELSEIF(IRTFFP.EQ.'Lucida Sans')THEN
          ITEMP=6
        ELSEIF(IRTFFP.EQ.'Arial')THEN
          ITEMP=2
        ELSEIF(IRTFFP.EQ.'Bookman')THEN
          ITEMP=3
        ELSEIF(IRTFFP.EQ.'Georgia')THEN
          ITEMP=4
        ELSEIF(IRTFFP.EQ.'Tahoma')THEN
          ITEMP=5
        ELSEIF(IRTFFP.EQ.'Verdana')THEN
          ITEMP=7
        ENDIF 
        WRITE(ICOUT,8301)IBASLC,ITEMP
 8301   FORMAT(A1,'f',I1)
        CALL DPWRST(ICOUT,'WRIT')
C
C       END TABLE AND RESET "ASIS" MODE
C
        IF(IRTFFF.EQ.'Courier New')THEN
          ITEMP=1
        ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
          ITEMP=8
        ENDIF 
        WRITE(ICOUT,8091)IBASLC,ITEMP
 8091   FORMAT(A1,'f',I1)
        CALL DPWRST(ICOUT,'WRIT')
C
        CALL DPRTF6(NHEAD)
        CALL DPRTF6(NHEAD)
        IRTFMD='VERB'
      ELSE
        WRITE(ICOUT,2589)ITITLE(1:58)
 2589   FORMAT(A60)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTIE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)N,IERROR
 9012   FORMAT('N,IERROR = ',I8,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)STATVA,STATCD,PVAL
 9013   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTIE3(Y,N,ICASAN,IR,
     1                  TEMP1,TEMP2,TEMP3,ITEMP1,ITEMP2,
     1                  STATVA,YMEAN,YSD,YMIN,YMAX,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE IS SPLIT OFF FROM DPTIE2 TO COMPUTE
C              TIETJEN-MOORE STATISTIC.  THIS ROUTINE JUST RETURNS
C              THE VALUE OF THE TEST STATISTIC (I.E., NO CRITICAL
C              VALUES OR PRINTING).  THIS SIMPLIFIES THE SIMULATION
C              STEP USED TO OBTAIN THE CRITICAL VALUES.
C     REFERENCE--GARY TIETJEN AND ROGER MOORE (AUGUST 1972), "SOME
C                GRUBBS-TYPE STATISTICS FOR THE DETECTION OF SEVERAL
C                OUTLIERS", TECHNOMETRICS, VOL. 14, NO. 3, PP. 583-597.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/11
C     ORIGINAL VERSION--NOVEMBER  2009.
C     UPDATED         --JANUARY   2009. SAVE INDICES FOR VALUES TO
C                                       BE TESTED AS OUTLIERS
C     UPDATED         --JULY      2014. ADD SKEWNESS AND KURTOSIS TO
C                                       SUMMARY STATISTICS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 ICASAN
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DOUBLE PRECISION DSUMN
      DOUBLE PRECISION DSUMD
      DOUBLE PRECISION DTERM1
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTI'
      ISUBN2='E3  '
      IERROR='NO'
      STATVA=CPUMIN
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPTIE3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)ISUBRO,IBUGA3,ICASAN
   52   FORMAT('ISUBRO,IBUGA3,ICASAN = ',3(A4,2X))
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N
   55   FORMAT('N = ',I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN TIETJEN-MOORE TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 3.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1114)N
 1114   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IR.GE.N/2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1121)
 1121   FORMAT('      THE SPECIFIED NUMBER OF SUSPECTED OUTLIERS IS ',
     1         'GREATER THAN N/2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1123)IR
 1123   FORMAT('THE SUSPECTED NUMBER OF OUTLIERS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1125)N
 1125   FORMAT('THE SAMPLE SIZE                  = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO1135I=2,N
        IF(Y(I).NE.HOLD)GOTO1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)HOLD
 1131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
 1139 CONTINUE
C
C               ************************************
C               **  STEP 21--                     **
C               **  CARRY OUT CALCULATIONS        **
C               **  FOR    TIETJEN-MOORE    TEST  **
C               ************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
      CALL SORT(Y,N,Y)
      YMIN=Y(1)
      YMAX=Y(N)
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      DO2101I=1,N
        ITEMP1(I)=I
 2101 CONTINUE
C
      IF(ICASAN.EQ.'TWOS')THEN
        DO2110I=1,N
          TEMP1(I)=ABS(Y(I)-YMEAN)
 2110   CONTINUE
CCCCC   CALL SORTC3(TEMP1,ITEMP1,N,TEMP2,ITEMP2)
        CALL SORTC(TEMP1,Y,N,TEMP2,TEMP3)
        DO2115I=1,N
          Y(I)=TEMP3(I)
 2115   CONTINUE
      ELSEIF(ICASAN.EQ.'MINI')THEN
         CALL REVERS(Y,N,IWRITE,TEMP1,TEMP2,IBUGA3,IERROR)
         DO2117I=1,N
           Y(I)=TEMP1(I)
 2117    CONTINUE
      ENDIF
      NLAST=N-IR
      CALL MEAN(Y,NLAST,IWRITE,YMEANN,IBUGA3,IERROR)
C
      DSUMN=0.0D0
      DSUMD=0.0D0
      DO2120I=1,N
        DTERM1=DBLE(Y(I) - YMEAN)
        DSUMD=DSUMD + DTERM1**2
 2120 CONTINUE
C
      DO2125I=1,NLAST
        DTERM1=DBLE(Y(I) - YMEANN)
        DSUMN=DSUMN + DTERM1**2
 2125 CONTINUE
C
      DTERM1=DSUMN/DSUMD
      STATVA=REAL(DTERM1)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTIE3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)YMEAN,YSD,YMIN,YMAX
 9013   FORMAT('YMEAN,YSD,YMIN,YMAX = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9015)YMEANN,YSDN,ITEMP2(1)
 9015   FORMAT('YMEANN,YSDN,YMIN,YMAX,ITEMP2(1) = ',4G15.7,I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)DSUM1,DSUM2,STATVA
 9017   FORMAT('DSUM1,DSUM2,STATVA = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTIE4(STATVA,STATCD,PVAL,
     1                  CUT0,CUT01,CUT025,CUT05,CUT10,
     1                  CUT25,CUT50,CUT100,
     1                  IFLAGU,IFRST,ILAST,ICASPL,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPTIET.  THIS ROUTINE
C              UPDATES THE PARAMETERS "STATVAL", "STATCDF", AND
C              "PVALUE" AFTER A TIETJEN-MOORE TEST.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/11
C     ORIGINAL VERSION--NOVEMBER  2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFLAGU
      CHARACTER*4 ICASPL
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
C
      SAVE IOUNI1
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIE4')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTIE4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,STATVA,STATCD,PVAL
   53   FORMAT('ICASPL,STATVA,STATCD,PVAL = ',A4,2X,3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)CUT0,CUT01,CUT025,CUT05
   54   FORMAT('CUT0,CUT01,CUT025,CUT05 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)CUT10,CUT25,CUT50,CUT100
   55   FORMAT('CUT10,CUT25,CUT50,CUT100 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
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
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIE4')THEN
            ISTEPN='2A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,201)
  201       FORMAT('AFTER CALL DPOPFI, IERRF1 = ',A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,203)IOUNI1,IFILE1
  203       FORMAT('IOUNI1,IFILE1 = ',I5,A80)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(IERROR.EQ.'YES')GOTO9000
C
          WRITE(IOUNI1,295)
  295     FORMAT(11X,'STATVAL',8X,'STATCDF',8X,'PVALUE',
     1           7X,'CUTOFF0',7X,'CUTOFF01',6X,'CUTOFF025',
     1           7X,'CUTOFF05',7X,'CUTOFF10',7X,'CUTOF25',
     1           7X,'CUTOFF50',7X,'CUTOF100')
          WRITE(IOUNI1,299)STATVA,STATCD,PVAL,CUT0,CUT01,CUT025,
     1                     CUT05,CUT10,CUT25,CUT50,CUT100
  299     FORMAT(11E15.7)
        ENDIF
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(STATCD.NE.CPUMIN)THEN
          IH='STAT'
          IH2='VAL '
          VALUE0=STATVA
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(STATCD.NE.CPUMIN)THEN
          IH='STAT'
          IH2='CDF '
          VALUE0=STATCD
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(PVAL.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UE  '
          VALUE0=PVAL
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT0.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF0'
          VALUE0=CUT0
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT01.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF01'
          VALUE0=CUT01
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT025.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='F025'
          VALUE0=CUT025
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT05.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF05'
          VALUE0=CUT05
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT10.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF10'
          VALUE0=CUT10
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT25.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF25'
          VALUE0=CUT25
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT50.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF50'
          VALUE0=CUT50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT100.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='F100'
          VALUE0=CUT100
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIE4')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)
  301       FORMAT('AFTER CALL DPCLFI, IERRF1 = ',A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,303)IOUNI1,IFILE1
  303       FORMAT('IOUNI1,IFILE1 = ',I5,A80)
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIE4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPTIE4--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTIFO(IHARG,NUMARG,IDEFFO,ITITFO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FONT FOR THE TITLE
C              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
C              THE FONT FOR THE TITLE WILL BE PLACED
C              IN THE HOLLERITH VARIABLE ITITFO.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFFO
C     OUTPUT ARGUMENTS--ITITFO
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
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION--JANUARY   1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFO
      CHARACTER*4 ITITFO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1199
      IF(IHARG(1).EQ.'FONT')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(NUMARG.EQ.1)GOTO1150
      GOTO1160
C
 1150 CONTINUE
      ITITFO=IDEFFO
      GOTO1180
C
 1160 CONTINUE
      ITITFO=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ITITFO
 1181 FORMAT('THE TITLE FONT HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END

      SUBROUTINE DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
!
!     PURPOSE--DEFINE AN INTEGER 0-1 VECTOR ISUB WHICH WILL BE USED
!              IN OTHER SUBROUTINES FOR EXTRACTING SUBSETS.
!     NOTE THAT IF THE WORDS   SUBSET   OR   EXCEPT   IS NOT IN THE
!     ARGUMENT LIST, THEN THE OUTPUT PARAMETER WILL BE SET TO NUMARG+1.
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
!     ORIGINAL VERSION--JANUARY  1978.
!     UPDATED         --JANUARY   1978.
!     UPDATED         --FEBRUARY  1978.
!     UPDATED         --MAY       1978.
!     UPDATED         --OCTOBER   1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --NOVEMBER  1980.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1988. ALLOW    NOT EQUAL   <> >< NOT=
!     UPDATED         --JANUARY   1989. CHECK FOR EMPTY SUBSETS (ALAN)
!     UPDATED         --JULY      2021. ALLOW VARIABLES ON BOTH SIDES
!                                       OF THE OPERATOR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGQ
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASSC
      CHARACTER*4 ICASQU
      CHARACTER*4 ICASVA
      CHARACTER*4 ICASV2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASOP
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
      CHARACTER*4 IH
      CHARACTER*4 IH2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
      ISUBN1='DPSU'
      ISUBN2='BS  '
      IERROR='NO'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      TARGET=0.0
      ISETV2=-1
!
!               ********************************
!               **  TREAT THE SUBSET CASE     **
!               ********************************
!
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSUBS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGQ,IERROR,NIOLD,ILOCS,NS
   52   FORMAT('IBUGQ,IERROR,NIOLD,ILOCS,NS = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NUMARG,NUMNAM,MAXNAM,N,MAXN
   55   FORMAT('NUMARG,NUMNAM,MAXNAM,N,MAXN = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IWIDTH,ILOCS,ILOCS2,ILOCTG
   56   FORMAT('IWIDTH,ILOCS,ILOCS2,ILOCTG = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  INITIALIZE THE SUBSET SIZE (NS) TO NIOLD. CHECK   **
!               **  FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  ALSO   **
!               **  CHECK THAT THE RELEVANT NUMBER OF OBSERVATIONS    **
!               **  (NIOLD) IS POSITIVE.                              **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NS=NIOLD
      ILOCS=NUMARG+1
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
                  IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(NIOLD.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN SUBSET (DPSUBS)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FROM WHICH A SUBSET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      WAS TO HAVE BEEN EXTRACTED IS 0.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************************
!               **  STEP 2.1--                                        **
!               **  INITIALIZE ALL ELEMENTS IN ISUB(.) TO 11          **
!               **  ISUB(.) WILL TAKE ON 4 VALUES AT MOST--           **
!               **  00, 01, 10, 11   .                                **
!               **  THE FIRST  DIGIT INDICATES WHETHER OR NOT THE     **
!               **  GIVEN ELEMENT IS OUT (0) OR IN (1) OF THE LOCAL   **
!               **  CUMULATIVE UNION SET.                             **
!               **  THE SECOND DIGIT INDICATES WHETHER OR NOT THE     **
!               **  GIVEN ELEMENT IS OUT (0) OR IN (1) OF THE GLOBAL  **
!               **  CUMULATIVE INTERSECTION SET.                      **
!               **  THE INITIALIZATION OF ALL ELEMENTS TO 11 THUS     **
!               **  INDICATES THAT INITIALLY ALL ELEMENTS (TEMPORARILY)*
!               **  ARE IN THE LOCAL UNION SET, AND INITIALLY ALL     **
!               **  ELEMENTS ARE IN THE GLOBAL INTERSECTION SET.      **
!               ********************************************************
!
      ISTEPN='2.1'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 200 I=1,NIOLD
        ISUB(I)=11
  200 CONTINUE
!
!               *************************************************
!               **  STEP 2.2--                                 **
!               **  IF EXISTENT,                               **
!               **  PACK < = INTO <=                           **
!               **  PACK = < INTO =<                           **
!               **  PACK > = INTO >=                           **
!               **  PACK = > INTO =>                           **
!               **  THIS IS BECAUSE = SIGNS ARE AUTOMATICALLY  **
!               **  GIVEN A SPACE IN DPTYPE AND TREATED AS     **
!               **  AS A SEPARATE WORD.                        **
!               **  NOTE THAT NUMARG WILL BE CHANGED.          **
!               *************************************************
!
      ISTEPN='2.2'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL ADJUS2(IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
!
!               ************************************************
!               **  STEP 3.1--                                **
!               **  CHECK TO SEE IF HAVE THE  SUBSET  CASE.   **
!               **  CHECK TO SEE IF HAVE THE  EXCEPT  CASE.   **
!               **  LOCATE THE POSITION IN THE ARGUMENT LIST  **
!               **  OF THE WORD   SUBSET   OR   EXCEPT  .     **
!               ************************************************
!
      ISTEPN='3.1'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      JMAX=0
      ICASSC='SEAR'
      ICASQU='UNKN'
      NUMSV=0
      DO 300 IPASS=1,100
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)
  301     FORMAT('***** AT THE BEGINNING OF ANOTHER PASS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,302)IPASS,ILOCTG,JMAX
  302     FORMAT('IPASS,ILOCTG,JMAX = ',3I8)
          CALL DPWRST('XXX','BUG ')
          IF(ILOCTG.GE.1)THEN
            WRITE(ICOUT,303)ICASSC,ILOCTG,IHARG(ILOCTG),IHARG2(ILOCTG)
  303       FORMAT('ICASSC,ILOCTG,IHARG(ILOCTG),IHARG2(ILOCTG) = ',   &
                   A4,I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
        IF(ICASSC.EQ.'STOP')GO TO 1100
        JMIN=JMAX+1
        IF(JMIN.GT.NUMARG)GO TO 1100
        IF(JMIN.EQ.NUMARG.AND.IHARG(JMIN).EQ.'AND '.AND.   &
           IHARG2(JMIN).EQ.'    ')GO TO 1100
!
        IF(ICASSC.EQ.'CONT')GO TO 600
        DO 310 I=1,NIOLD
          ITEMP=ISUB(I)
          IF(ITEMP.EQ.00)ISUB(I)=00
          IF(ITEMP.EQ.10)ISUB(I)=00
          IF(ITEMP.EQ.01)ISUB(I)=00
          IF(ITEMP.EQ.11)ISUB(I)=11
  310   CONTINUE
        ICASQU='UNKN'
        DO 340 J=JMIN,NUMARG
          J2=J
          IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')THEN
            ICASQU='SUBS'
            ILOCS=J2
!CCCC       THE FOLLOWING 6 LINES WERE INSERTED MARCH 1988.
            ILOCS2=ILOCS+2
            IHSET=IHARG(ILOCS2)
            IHSET2=IHARG2(ILOCS2)
            IF(IHSET.EQ.'<>  ')ICASQU='EXCE'
            IF(IHSET.EQ.'><  ')ICASQU='EXCE'
            IF(IHSET.EQ.'NOT=')ICASQU='EXCE'
            GO TO 390
          ELSEIF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')THEN
            ICASQU='EXCE'
            ILOCS=J2
            GO TO 390
          ENDIF
  340   CONTINUE
        ILOCS=NUMARG+1
        GO TO 1100
!
  390   CONTINUE
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,391)IPASS,ILOCS,ICASQU
  391     FORMAT('IPASS,ILOCS,ICASQU = ',2I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *******************************************
!               **  STEP 3.2--                           **
!               **  IF HAVE THE SUBSET CASE,             **
!               **  INITIALIZE ISUB(.) TO 0X--00 OR 01.  **
!               **  IF HAVE THE EXCEPT CASE,             **
!               **  INITIALIZE ISUB(.) TO 1X--10 OR 11.  **
!               *******************************************
!
        ISTEPN='3.2'
        IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(ICASQU.EQ.'SUBS')THEN
          DO 401 I=1,NIOLD
            ITEMP=ISUB(I)
            IF(ITEMP.EQ.00)ISUB(I)=00
            IF(ITEMP.EQ.10)ISUB(I)=00
            IF(ITEMP.EQ.01)ISUB(I)=01
            IF(ITEMP.EQ.11)ISUB(I)=01
  401     CONTINUE
        ELSE
          DO 406 I=1,NIOLD
            ITEMP=ISUB(I)
            IF(ITEMP.EQ.00)ISUB(I)=10
            IF(ITEMP.EQ.10)ISUB(I)=10
            IF(ITEMP.EQ.01)ISUB(I)=11
            IF(ITEMP.EQ.11)ISUB(I)=11
  406     CONTINUE
        ENDIF
!
!               ********************************************************
!               **  STEP 4--                                          **
!               **  CHECK VALIDITY OF FIRST ARGUMENT AFTER     SUBSET **
!               **  OR    EXCEPT    .                                 **
!               **  THIS SHOULD BE THE SUBSET VARIABLE                **
!               **  OR THE DUMMY INDEX    I   .                       **
!               ********************************************************
!
        ISTEPN='4'
        IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICASVA='UNKN'
        ILOCS1=ILOCS+1
        JMAX=ILOCS1
        IF(ILOCS1.GT.NUMARG)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,412)
  412     FORMAT('      THE WORD    SUBSET    OR    EXCEPT    WAS THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,413)
  413     FORMAT('      FINAL WORD ON THE COMMAND LINE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,414)
  414     FORMAT('      THE WORD    SUBSET  OR   EXCEPT   SHOULD HAVE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,415)
  415     FORMAT('      BEEN FOLLOWED BY OTHER ARGUMENTS, AS IN')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,416)
  416     FORMAT('           SUBSET X = 4')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,417)
  417     FORMAT('           SUBSET X = 4 7 9 15 22')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,418)
  418     FORMAT('           SUBSET X = 4 TO 10')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,419)
  419     FORMAT('           SUBSET X >= 7')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,420)
  420     FORMAT('           AND SO FORTH.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,421)
  421     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
  422       FORMAT('      ',100A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        IHSET=IHARG(ILOCS1)
        IHSET2=IHARG2(ILOCS1)
!
        IF(IHSET.EQ.'I   '.AND.IHSET2.EQ.'    ')THEN
          ICASVA='I   '
        ELSE
          IF(NUMNAM.LE.0)GO TO 490
          DO 435 I=1,NUMNAM
            IF(IHNAME(I).EQ.IHSET.AND.IHNAM2(I).EQ.IHSET2.AND.   &
               IUSE(I).EQ.'V   ')THEN
              ICASVA='V   '
              IHWUSE='V'
              MESSAG='YES'
              CALL CHECKN(IHSET,IHSET2,IHWUSE,   &
                          IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                          NUMNAM,MAXNAM,   &
                          ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
              IF(IERROR.EQ.'YES')GO TO 9000
              ISETV=IVALUE(ILOC)
!
              IF(IBUGQ.EQ.'ON')THEN
                WRITE(ICOUT,451)ILOCS1,IHSET,IHSET2,ISETV
  451           FORMAT('ILOCS1,IHSET,IHSET2,ISETV = ',I8,3X,2A4,3X,I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              GO TO 490
            ENDIF
  435     CONTINUE
        ENDIF
!
  490   CONTINUE
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,491)IPASS,IHSET,IHSET2,ICASVA,ISETV
  491     FORMAT('IPASS,IHSET,IHSET2,ICASVA,ISETV = ',I8,3(2X,A4),I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *******************************************************
!               **  STEP 5--                                         **
!               **  CHECK TO SEE IF NEXT ARGUMENT IS                 **
!               **        <                                          **
!               **        <=                                         **
!               **        =                                          **
!               **        >=                                         **
!               **        >                                          **
!               **        <>   ><   NOT=                             **
!               **  IF NONE OF THE ABOVE, THEN ASSUME  =   .         **
!               *******************************************************
!
        ISTEPN='5'
        IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICASOP='UNKN'
        ILOCS2=ILOCS+2
        JMAX=ILOCS2
!
        IF(ILOCS2.GT.NUMARG)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,502)
  502     FORMAT('      THE SUBSET/EXCEPT VARIABLE NAME WAS THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,503)
  503     FORMAT('      FINAL WORD ON THE COMMAND LINE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,504)
  504     FORMAT('      THE SUBSET/EXCEPT VARIABLE NAME SHOULD HAVE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,415)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,416)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,417)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,418)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,419)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,420)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,421)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        IHSET=IHARG(ILOCS2)
        IHSET2=IHARG2(ILOCS2)
!
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
        ELSEIF(IHSET.EQ.'<>  ' .OR. IHSET.EQ.'><  ' .OR.   &
             IHSET.EQ.'NOT=')THEN
          ICASOP='=   '
          ILOCTG=ILOCS2
        ELSE
          ICASOP='=ASS'
          ILOCTG=ILOCS2-1
          GO TO 590
        ENDIF
!
  590   CONTINUE
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,591)IPASS,IHSET,IHSET2,ICASVA,ICASOP
  591     FORMAT('IPASS,IHSET,IHSET2,ICASVA,ICASOP = ',   &
                 I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ********************************************************
!               **  STEP 6--                                          **
!               **  DETERMINE THE LOWER LIMIT OF THE INTERVAL OF      **
!               **  INTEREST.  THIS IS DONE BY CHECKING THE FIRST     **
!               **  (NEXT) ARGUMENT IN THE LIST.                      **
!               **  ALSO, FOR THOSE 4 CASES IN WHICH                  **
!               **  ICASOP IS   <   <=   >=   >                       **
!               **  DETERMINE THE UPPER LIMIT OF THE INTERVAL OF      **
!               **  INTEREST.                                         **
!               ********************************************************
!
!       2021/07: CHECK WHETHER THIS ARGUMENT IS A VARIABLE NAME
!
  600   CONTINUE
!
        ISTEPN='6'
        IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,601)
  601     FORMAT('     AT THE BEGINNING OF STEP 6 IN DPSUBS--')
          CALL DPWRST('XXX','BUG ')
          DO 605 I=1,NIOLD
            WRITE(ICOUT,606)I,ISUB(I)
  606       FORMAT('I,ISUB(I) = ',I8,I8)
            CALL DPWRST('XXX','BUG ')
  605     CONTINUE
        ENDIF
!
        ILOCTG=ILOCTG+1
        JMAX=ILOCTG
        ICASV2='OFF '
        IF(ILOCTG.GT.NUMARG)THEN
!
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,612)
  612     FORMAT('      THE SUBSET/EXCEPT OPERATION  < <=  =  >=  >')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,613)
  613     FORMAT('      WAS THE FINAL WORD ON THE COMMAND LINE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,614)
  614     FORMAT('      THE SUBSET/EXCEPT VARIABLE NAME SHOULD HAVE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,415)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,416)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,417)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,418)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,419)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,420)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,421)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
!
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
!
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                      NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'NO')THEN
            ISETV2=IVALUE(ILOC)
            ICASV2='V'
            GO TO 899
          ELSE
            ICASV2='P'
          ENDIF
!
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
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
  632     FORMAT('      AN ARGUMENT TYPE WHICH SHOULD BE ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,633)
  633     FORMAT('      EITHER A NUMBER OR A WORD, IS NEITHER.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,634)IHARG(ILOCTG),IHARG2(ILOCTG)
  634     FORMAT('      ARGUMENT                  = ',2A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,635)ILOCTG
  635     FORMAT('      LOCATION IN ARGUMENT LIST = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,636)IARGT(ILOCTG)
  636     FORMAT('      ARGUMENT TYPE             = ',A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,421)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,691)IPASS,ICASVA,ICASOP,IH,IH2,DMIN,DMAX
  691     FORMAT('IPASS,ICASVA,ICASOP,IH,IH2,DMIN,DMAX = ',   &
                 I8,4(2X,A4),2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ********************************************************
!               **  STEP 7--                                          **
!               **  DETERMINE THE UPPER LIMIT OF THE INTERVAL OF      **
!               **  INTEREST.  NOTE THAT FOR THOSE 4 CASES IN WHICH   **
!               **  ICASOP IS   <   <=   >=   >                       **
!               **  THE UPPER LIMIT OF THE INTERVAL                   **
!               **  HAS ALREADY BEEN DETERMINED AND SO                **
!               **  ALL OF THE CODE OF THIS SECTION MAY BE SKIPPED.   **
!               **  ON THE OTHER HAND WHEN THE OPERATION IS    =   ,  **
!               **  (EXPLICITLY OR ASSUMED),                          **
!               **  THE UPPER LIMIT MUST BE DETERMINED.               **
!               **  THIS IS DONE BY CHECKING THE NEXT ARGUMENT        **
!               **  IN THE LIST.                                      **
!               **  IF THIS NEXT ARGUMENT IS    TO   ,   THIS         **
!               **  IMPLIES THAT AN UPPER LIMIT WILL BE PROVIDED      **
!               **  (IN THE ARGUMENT AFTER THE   TO   ).              **
!               **  HOWEVER, IF THE NEXT ARGUMENT IS NOT A    TO   ,  **
!               **  THEN THIS IMPLIES THAT THE LIST CONSISTS          **
!               **  OF INDIVIDUAL ELEMENTS OF THE SUBSET              **
!               **  AND SO THE UPPER LIMIT WILL BE IDENTICAL          **
!               **  TO THE LOWER LIMIT.                               **
!               ********************************************************
!
        ISTEPN='7'
        IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(ICASOP.EQ.'<   ' .OR. ICASOP.EQ.'<=  ' .OR.   &
           ICASOP.EQ.'>=  ' .OR. ICASOP.EQ.'>   ')THEN
          ICASSC='SEAR'
          GO TO 790
        ENDIF
!
        ILOCTG=ILOCTG+1
!
        IF(ILOCTG.GT.NUMARG .OR.   &
          (ILOCTG.EQ.NUMARG.AND.IHARG(ILOCTG).EQ.'AND '.AND.   &
           IHARG2(ILOCTG).EQ.'    '))THEN
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='STOP'
          DMAX=DMIN
          GO TO 790
        ELSEIF((ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'SUBS'.AND.   &
                IHARG2(ILOCTG).EQ.'ET  ') .OR.   &
               (ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'EXCE'.AND.   &
                IHARG2(ILOCTG).EQ.'PT  '))THEN
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='SEAR'
          DMAX=DMIN
          GO TO 790
        ELSEIF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'TO  '.AND.   &
               IHARG2(ILOCTG).EQ.'    ')THEN
          ILOCTG=ILOCTG+1
          JMAX=ILOCTG
          IF(ILOCTG.GT.NUMARG)GO TO 760
          IF(ILOCTG.EQ.NUMARG.AND.IHARG(ILOCTG).EQ.'AND '.AND.   &
             IHARG2(ILOCTG).EQ.'    ')GO TO 760
          IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'SUBS'.AND.   &
             IHARG2(ILOCTG).EQ.'ET  ')GO TO 760
          IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'EXCE'.AND.   &
             IHARG2(ILOCTG).EQ.'PT  ')GO TO 760
          IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'TO  '.AND.   &
             IHARG2(ILOCTG).EQ.'    ')GO TO 760
          GO TO 770
        ELSE
          ILOCTG=ILOCTG-1
          JMAX=ILOCTG
          ICASSC='CONT'
          DMAX=DMIN
          GO TO 790
        ENDIF
!
  760   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,762)
  762   FORMAT('      THE WORD    TO    SHOULD HAVE BEEN FOLLOWED BY A')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,764)
  764   FORMAT('      NUMBER OR BY A PARAMETER NAME, BUT WAS NOT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,765)IHARG(ILOCTG),IHARG2(ILOCTG)
  765   FORMAT('      TO    WAS FOLLOWED BY THE WORD   ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,421)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,422)(IANS(I),I=1,MIN(IWIDTH,100))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 9000
!
  770   CONTINUE
        IF(IARGT(ILOCTG).EQ.'NUMB')THEN
          DMAX=ARG(ILOCTG)
        ELSEIF(IARGT(ILOCTG).EQ.'WORD')THEN
          IH=IHARG(ILOCTG)
          IH2=IHARG2(ILOCTG)
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IH,IH2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          DMAX=VALUE(ILOC)
        ELSE
          IBRAN=770
          WRITE(ICOUT,771)IBRAN
  771     FORMAT('***** INTERNAL ERROR IN DPSUBS--',   &
                 'IMPOSSIBLE BRANCH CONDITION AT BRANCH POINT = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,772)ILOCTG,IARGT(ILOCTG)
  772     FORMAT('ILOCTG, IARGT(ILOCTG) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        ILOCTG=ILOCTG+1
        ICASSC='CONT'
        IF(ILOCTG.GT.NUMARG)ICASSC='STOP'
        IF(ILOCTG.EQ.NUMARG.AND.IHARG(ILOCTG).EQ.'AND '.AND.   &
           IHARG2(ILOCTG).EQ.'    ')ICASSC='STOP'
        IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'SUBS'.AND.   &
           IHARG2(ILOCTG).EQ.'ET  ')ICASSC='SEAR'
        IF(ILOCTG.LE.NUMARG.AND.IHARG(ILOCTG).EQ.'EXCE'.AND.   &
           IHARG2(ILOCTG).EQ.'PT  ')ICASSC='SEAR'
        ILOCTG=ILOCTG-1
        JMAX=ILOCTG
!
  790   CONTINUE
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,791)IPASS,ICASVA,ICASOP,IH,IH2,DMIN,DMAX
  791     FORMAT('IPASS,ICASVA,ICASOP,IH,IH2,DMIN,DMAX = ',   &
                 I8,4(2X,A4),2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ***************************************************
!               **  STEP 8--                                     **
!               **  TO ALLOW FOR ROUNDOFF ERRORS IN THE          **
!               **  STORAGE OF NUMBERS,                          **
!               **  JUDICIOUSLY EXPAND THE INTERVAL OF INTEREST  **
!               **  BY AN    EPSILON    AMOUNT.                  **
!               ***************************************************
!
        ISTEPN='8'
!
        IF(IBUGQ.EQ.'ON')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,801)
  801     FORMAT('      AT THE BEGINNING OF STEP 8--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,802)DMIN,DMAX
  802     FORMAT('DMIN,DMAX = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(DMIN.GT.DMAX)THEN
          HOLD=DMIN
          DMIN=DMAX
          DMAX=HOLD
        ENDIF
!
        IF(DMIN.EQ.CPUMIN)GO TO 819
        IF(DMIN.EQ.CPUMAX)GO TO 819
        IF(ABS(DMIN).EQ.0.0)EPS=0.000001
        IF(ABS(DMIN).NE.0.0)EPS=ABS(DMIN*0.000001)
        IF(ICASOP.EQ.'=   ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'=ASS')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'<   ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'<=  ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'>=  ')DMIN=DMIN-EPS
        IF(ICASOP.EQ.'>   ')DMIN=DMIN+EPS
  819   CONTINUE
!
        IF(DMAX.EQ.CPUMAX)GO TO 829
        IF(DMAX.EQ.CPUMIN)GO TO 829
        IF(ABS(DMAX).EQ.0.0)EPS=0.000001
        IF(ABS(DMAX).NE.0.0)EPS=ABS(DMAX*0.000001)
        IF(ICASOP.EQ.'=   ')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'=ASS')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'<   ')DMAX=DMAX-EPS
        IF(ICASOP.EQ.'<=  ')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'>=  ')DMAX=DMAX+EPS
        IF(ICASOP.EQ.'>   ')DMAX=DMAX+EPS
  829   CONTINUE
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,891)IPASS,ICASVA,ICASOP,IH,IH2
  891     FORMAT('IPASS,ICASVA,ICASOP,IH,IH2 = ',I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,892)EPS,DMIN,DMAX,CPUMIN,CPUMAX
  892     FORMAT('EPS,DMIN,DMAX,CPUMIN,CPUMAX = ',5E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
  899   CONTINUE
!
!               *****************************************************
!               **  STEP 9--                                       **
!               **  DEFINE THE ISUB(.) VECTOR--                    **
!               **  FOR ANY K (K = 1 TO NIOLD), IF THE K-TH        **
!               **  ELEMENT OF THE SUBSET SPECIFICATION VARIABLE   **
!               **  (THE VARIABLE SPECIFIED AFTER    SUBSET        **
!               **  IN THE COMMAND LINE) IS WITHIN THE SPECIFIED   **
!               **  (DMIN,DMAX) LIMITS, THEN ISUB(K) SHOULD RESULT **
!               **  IN A VALUE OF 1; BUT IF THE K-TH ELEMENT OF    **
!               **  THE SUBSET SPECIFICATION VARIABLE IS OUTSIDE   **
!               **  THE SPECIFIED (DMIN,DMAX) LIMITS, THEN ISUB(K) **
!               **  SHOULD RESULT IN A 0 .                         **
!               *****************************************************
!
        ISTEPN='9'
        IF(IBUGQ.EQ.'ON')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,901)ILOCS1,IHSET,IHSET2,ICASVA,ISETV,MAXCOL
  901     FORMAT('ILOCS1,IHSET,IHSET2,ICASVA,ISETV,MAXCOL = ',   &
                 I8,3(2X,A4),2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(ICASVA.EQ.'UNKN')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,911)
  911     FORMAT('***** INTERNAL ERROR IN DPSUBS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,912)
  912     FORMAT('      IMPROPER VALUE FOR ICASVA AND/OR ISETV')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,913)ICASVA,ISETV,MAXCOL,MAXCP1,MAXCP2
  913     FORMAT('      ICASVA,ISETV,MAXCOL,MAXCP1,MAXCP2 = ',A4,4I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(ICASVA.EQ.'I   ')THEN
          NS=0
          ND=0
          DO 931 I=1,NIOLD
            TARGET=I
!
            IF(ICASV2.EQ.'V')THEN
              IJZ=MAXN*(ISETV2-1)+I
              AVAL=V(IJZ)
              DMIN=V(IJZ)
              DMAX=V(IJZ)
              IF(ICASOP.EQ.'<   ')THEN
                 DMIN=CPUMIN
                 DMAX=AVAL
              ELSEIF(ICASOP.EQ.'<=  ')THEN
                 DMIN=CPUMIN
                 DMAX=AVAL
              ELSEIF(ICASOP.EQ.'>=  ')THEN
                 DMIN=AVAL
                 DMAX=CPUMAX
              ELSEIF(ICASOP.EQ.'>   ')THEN
                 DMIN=AVAL
                 DMAX=CPUMAX
              ENDIF
              IF(ABS(DMIN).EQ.0.0)EPS=0.000001
              IF(ABS(DMIN).NE.0.0)EPS=ABS(DMIN*0.000001)
              IF(DMIN.NE.CPUMIN .AND. DMIN.EQ.CPUMAX)THEN
                IF(ICASOP.EQ.'=   ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'=ASS')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'<   ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'<=  ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'>=  ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'>   ')DMIN=DMIN+EPS
              ENDIF
              IF(ABS(DMAX).EQ.0.0)EPS=0.000001
              IF(ABS(DMAX).NE.0.0)EPS=ABS(DMAX*0.000001)
              IF(DMAX.NE.CPUMAX .AND. DMAX.NE.CPUMIN)THEN
                IF(ICASOP.EQ.'=   ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'=ASS')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'<   ')DMAX=DMAX-EPS
                IF(ICASOP.EQ.'<=  ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'>=  ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'>   ')DMAX=DMAX+EPS
              ENDIF
!
              IF(IBUGQ.EQ.'ON')THEN
                WRITE(ICOUT,939)I,ISETV2,DMIN,DMAX,AVAL
  939           FORMAT('AT 939: I,ISETV2,DMIN,DMAX,AVAL = ',2I8,3G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
            ENDIF
!
            IF(ICASQU.EQ.'SUBS'.AND.DMIN.LE.TARGET.AND.   &
               TARGET.LE.DMAX)THEN
              ITEMP=ISUB(I)
              IF(ITEMP.EQ.00)ISUB(I)=10
              IF(ITEMP.EQ.10)ISUB(I)=10
              IF(ITEMP.EQ.01)ISUB(I)=11
              IF(ITEMP.EQ.11)ISUB(I)=11
              NS=NS+1
            ELSEIF(ICASQU.EQ.'SUBS')THEN
              ND=ND+1
            ELSEIF(ICASQU.EQ.'EXCE'.AND.DMIN.LE.TARGET.AND.   &
                   TARGET.LE.DMAX)THEN
              ITEMP=ISUB(I)
              IF(ITEMP.EQ.00)ISUB(I)=00
              IF(ITEMP.EQ.10)ISUB(I)=00
              IF(ITEMP.EQ.01)ISUB(I)=01
              IF(ITEMP.EQ.11)ISUB(I)=01
              ND=ND+1
            ELSEIF(ICASQU.EQ.'EXCE')THEN
              NS=NS+1
            ENDIF
  931     CONTINUE
          GO TO 990
        ELSEIF(ISETV.LE.MAXCOL)THEN
          NS=0
          ND=0
          DO 941 I=1,NIOLD
            IJ=MAXN*(ISETV-1)+I
            VIJ=V(IJ)
!
            IF(ICASV2.EQ.'V')THEN
              IJZ=MAXN*(ISETV2-1)+I
              AVAL=V(IJZ)
              DMIN=V(IJZ)
              DMAX=V(IJZ)
              IF(ICASOP.EQ.'<   ')THEN
                 DMIN=CPUMIN
                 DMAX=AVAL
              ELSEIF(ICASOP.EQ.'<=  ')THEN
                 DMIN=CPUMIN
                 DMAX=AVAL
              ELSEIF(ICASOP.EQ.'>=  ')THEN
                 DMIN=AVAL
                 DMAX=CPUMAX
              ELSEIF(ICASOP.EQ.'>   ')THEN
                 DMIN=AVAL
                 DMAX=CPUMAX
              ENDIF
              IF(ABS(DMIN).EQ.0.0)EPS=0.000001
              IF(ABS(DMIN).NE.0.0)EPS=ABS(DMIN*0.000001)
              IF(DMIN.NE.CPUMIN .AND. DMIN.EQ.CPUMAX)THEN
                IF(ICASOP.EQ.'=   ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'=ASS')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'<   ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'<=  ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'>=  ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'>   ')DMIN=DMIN+EPS
              ENDIF
              IF(ABS(DMAX).EQ.0.0)EPS=0.000001
              IF(ABS(DMAX).NE.0.0)EPS=ABS(DMAX*0.000001)
              IF(DMAX.NE.CPUMAX .AND. DMAX.NE.CPUMIN)THEN
                IF(ICASOP.EQ.'=   ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'=ASS')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'<   ')DMAX=DMAX-EPS
                IF(ICASOP.EQ.'<=  ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'>=  ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'>   ')DMAX=DMAX+EPS
              ENDIF
!
              IF(IBUGQ.EQ.'ON')THEN
                WRITE(ICOUT,949)I,ISETV2,DMIN,DMAX,AVAL
  949           FORMAT('AT 949: I,ISETV2,DMIN,DMAX,AVAL = ',2I8,3G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
            ENDIF
!
            IF(IBUGQ.EQ.'ON')THEN
              WRITE(ICOUT,947)I,NIOLD,ISETV,DMIN,DMAX,VIJ
  947         FORMAT('I,NIOLD,ISETV,DMIN,DMAX,VIJ = ',3I8,3E12.4)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            TARGET=VIJ
            IF(ICASQU.EQ.'SUBS'.AND.DMIN.LE.TARGET.AND.   &
               TARGET.LE.DMAX)THEN
              ITEMP=ISUB(I)
              IF(ITEMP.EQ.00)ISUB(I)=10
              IF(ITEMP.EQ.10)ISUB(I)=10
              IF(ITEMP.EQ.01)ISUB(I)=11
              IF(ITEMP.EQ.11)ISUB(I)=11
              NS=NS+1
              GO TO 941
            ELSEIF(ICASQU.EQ.'SUBS')THEN
              ND=ND+1
              GO TO 941
            ELSEIF(ICASQU.EQ.'EXCE'.AND.DMIN.LE.TARGET.AND.   &
                   TARGET.LE.DMAX)THEN
              ITEMP=ISUB(I)
              IF(ITEMP.EQ.00)ISUB(I)=00
              IF(ITEMP.EQ.10)ISUB(I)=00
              IF(ITEMP.EQ.01)ISUB(I)=01
              IF(ITEMP.EQ.11)ISUB(I)=01
              ND=ND+1
              GO TO 941
            ELSEIF(ICASQU.EQ.'EXCE')THEN
              NS=NS+1
              GO TO 941
            ELSE
              GO TO 941
            ENDIF
  941     CONTINUE
          GO TO 990
        ELSEIF(ISETV.EQ.MAXCP1 .OR. ISETV.EQ.MAXCP2 .OR.   &
               ISETV.EQ.MAXCP3 .OR. ISETV.EQ.MAXCP4 .OR.   &
               ISETV.EQ.MAXCP5 .OR. ISETV.EQ.MAXCP6)THEN
          NS=0
          ND=0
          DO 951 I=1,NIOLD
!
            IF(ICASV2.EQ.'V')THEN
              IJZ=MAXN*(ISETV2-1)+I
              AVAL=V(IJZ)
              DMIN=V(IJZ)
              DMAX=V(IJZ)
              IF(ICASOP.EQ.'<   ')THEN
                 DMIN=CPUMIN
                 DMAX=AVAL
              ELSEIF(ICASOP.EQ.'<=  ')THEN
                 DMIN=CPUMIN
                 DMAX=AVAL
              ELSEIF(ICASOP.EQ.'>=  ')THEN
                 DMIN=AVAL
                 DMAX=CPUMAX
              ELSEIF(ICASOP.EQ.'>   ')THEN
                 DMIN=AVAL
                 DMAX=CPUMAX
              ENDIF
              IF(ABS(DMIN).EQ.0.0)EPS=0.000001
              IF(ABS(DMIN).NE.0.0)EPS=ABS(DMIN*0.000001)
              IF(DMIN.NE.CPUMIN .AND. DMIN.EQ.CPUMAX)THEN
                IF(ICASOP.EQ.'=   ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'=ASS')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'<   ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'<=  ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'>=  ')DMIN=DMIN-EPS
                IF(ICASOP.EQ.'>   ')DMIN=DMIN+EPS
              ENDIF
              IF(ABS(DMAX).EQ.0.0)EPS=0.000001
              IF(ABS(DMAX).NE.0.0)EPS=ABS(DMAX*0.000001)
              IF(DMAX.NE.CPUMAX .AND. DMAX.NE.CPUMIN)THEN
                IF(ICASOP.EQ.'=   ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'=ASS')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'<   ')DMAX=DMAX-EPS
                IF(ICASOP.EQ.'<=  ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'>=  ')DMAX=DMAX+EPS
                IF(ICASOP.EQ.'>   ')DMAX=DMAX+EPS
              ENDIF
!
              IF(IBUGQ.EQ.'ON')THEN
                WRITE(ICOUT,949)I,ISETV2,DMIN,DMAX,AVAL
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
            ENDIF
!
            IF(ISETV.EQ.MAXCP1)TARGET=PRED(I)
            IF(ISETV.EQ.MAXCP2)TARGET=RES(I)
            IF(ISETV.EQ.MAXCP3)TARGET=YPLOT(I)
            IF(ISETV.EQ.MAXCP4)TARGET=XPLOT(I)
            IF(ISETV.EQ.MAXCP5)TARGET=X2PLOT(I)
            IF(ISETV.EQ.MAXCP6)TARGET=TAGPLO(I)
            IF(ICASQU.EQ.'SUBS'.AND.DMIN.LE.TARGET.AND.   &
               TARGET.LE.DMAX)THEN
              ITEMP=ISUB(I)
              IF(ITEMP.EQ.00)ISUB(I)=10
              IF(ITEMP.EQ.10)ISUB(I)=10
              IF(ITEMP.EQ.01)ISUB(I)=11
              IF(ITEMP.EQ.11)ISUB(I)=11
              NS=NS+1
              GO TO 951
            ELSEIF(ICASQU.EQ.'SUBS')THEN
              ND=ND+1
              GO TO 951
            ELSEIF(ICASQU.EQ.'EXCE'.AND.DMIN.LE.TARGET.AND.   &
                   TARGET.LE.DMAX)THEN
              ITEMP=ISUB(I)
              IF(ITEMP.EQ.00)ISUB(I)=00
              IF(ITEMP.EQ.10)ISUB(I)=00
              IF(ITEMP.EQ.01)ISUB(I)=01
              IF(ITEMP.EQ.11)ISUB(I)=01
              ND=ND+1
              GO TO 951
            ELSEIF(ICASQU.EQ.'EXCE')THEN
              NS=NS+1
              GO TO 951
            ELSE
              GO TO 951
            ENDIF
  951     CONTINUE
          GO TO 990
        ENDIF
!
  990   CONTINUE
!
        IF(IBUGQ.EQ.'ON')THEN
          WRITE(ICOUT,991)IPASS,ICASQU,DMIN,DMAX,EPS,   &
                          NIOLD,NS,ND
  991     FORMAT('IPASS,ICASQU,DMIN,DMAX,EPS,NIOLD,NS,ND = ',   &
                 I8,2X,A4,3G15.7,3I8)
          CALL DPWRST('XXX','BUG ')
          DO 992 I=1,NIOLD
            WRITE(ICOUT,993)I,ISUB(I)
  993       FORMAT('I,ISUB(I) = ',I8,I8)
            CALL DPWRST('XXX','BUG ')
  992     CONTINUE
        ENDIF
!
!               *************************************************
!               **  STEP 10--                                  **
!               **  WRITE OUT A MESSAGE FOR THIS STEP          **
!               **  INDICATING                                 **
!               **  THE SUBSET VARIABLE NAME,                  **
!               **  THE SUBSET MINIMUM,                        **
!               **  THE SUBSET MAXIMUM,                        **
!               **  THE INPUT NUMBER OF OBSERVATIONS (LOCAL),  **
!               **  THE NUMBER OF OBSERVATIONS IGNORED         **
!               **  AND THE OUTPUT NUMBER OF OBSERVATIONS      **
!               **  (THAT IS, THE SUBSET SAMPLE SIZE).         **
!               **  ALSO, CHECK THAT NS IS POSITIVE.           **
!               *************************************************
!
        ISTEPN='10'
        IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(ICASQU.EQ.'EXCE')THEN
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1021)
 1021       FORMAT('***** NOTE--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1022)IHARG(ILOCS1),IHARG2(ILOCS1)
 1022       FORMAT('      EXCEPTED SUBSET VARIABLE = ',2A4)
            CALL DPWRST('XXX','BUG ')
            IF(ICASV2.EQ.'V')THEN
              WRITE(ICOUT,2023)
 2023         FORMAT('      EXCEPTED SUBSET MINIMUM/MAXIMUM VARIES ',   &
                     'WITH VARIABLE ROW.')
              CALL DPWRST('XXX','BUG ')
            ELSE
              WRITE(ICOUT,1023)DMIN
 1023         FORMAT('      EXCEPTED SUBSET MINIMUM  = ',E17.10)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1024)DMAX
 1024         FORMAT('      EXCEPTED SUBSET MAXIMUM  = ',E17.10)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            WRITE(ICOUT,1025)NIOLD
 1025       FORMAT('      INPUT  NUMBER OF OBSERVATIONS  = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1026)ND
 1026       FORMAT('      NUMBER OF OBSERVATIONS IGNORED = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1027)NS
 1027       FORMAT('      OUTPUT NUMBER OF OBSERVATIONS  = ',I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
 1011       FORMAT('***** NOTE--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1012)IHARG(ILOCS1),IHARG2(ILOCS1)
 1012       FORMAT('      SUBSET VARIABLE = ',2A4)
            CALL DPWRST('XXX','BUG ')
            IF(ICASV2.EQ.'V')THEN
              WRITE(ICOUT,2023)
              CALL DPWRST('XXX','BUG ')
            ELSE
              WRITE(ICOUT,1013)DMIN
 1013         FORMAT('      SUBSET MINIMUM  = ',E17.10)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1014)DMAX
 1014         FORMAT('      SUBSET MAXIMUM  = ',E17.10)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            WRITE(ICOUT,1015)NIOLD
 1015       FORMAT('      INPUT  NUMBER OF OBSERVATIONS  = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1016)ND
 1016       FORMAT('      NUMBER OF OBSERVATIONS IGNORED = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1017)NS
 1017       FORMAT('      OUTPUT NUMBER OF OBSERVATIONS  = ',I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
        NUMSV=IPASS
!
  300 CONTINUE
!
 1100 CONTINUE
      DO 1110 I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=00
        IF(ITEMP.EQ.10)ISUB(I)=00
        IF(ITEMP.EQ.01)ISUB(I)=00
        IF(ITEMP.EQ.11)ISUB(I)=11
 1110 CONTINUE
!
!               *************************************
!               **  STEP 11--                      **
!               **  PUT ISUB(.) IN FINAL 0,1 FORM  **
!               *************************************
!
      ISTEPN='11'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1210 I=1,NIOLD
        ITEMP=ISUB(I)
        IF(ITEMP.EQ.00)ISUB(I)=0
        IF(ITEMP.EQ.10)ISUB(I)=0
        IF(ITEMP.EQ.01)ISUB(I)=1
        IF(ITEMP.EQ.11)ISUB(I)=1
 1210 CONTINUE
!
!               *****************************************
!               **  STEP 12--                          **
!               **  IF THERE WERE 2 OR MORE SUBSET     **
!               **  VARIABLES, GATHER INFORMATION      **
!               **  FOR A FINAL SUMMARY MESSAGE BY     **
!               **  DETERMINING THE FINAL NUMBER OF    **
!               **  ELEMENTS IN THE SUBSET             **
!               **  (AFTER ALL VARIABLES HAVE          **
!               **  BEEN INDIVIDUALLY ACCOUNTED FOR).  **
!               *****************************************
!
      ISTEPN='12'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMSV.GT.1)THEN
        NS=0
        DO 1510 I=1,NIOLD
          IF(ISUB(I).EQ.1)NS=NS+1
 1510   CONTINUE
      ENDIF
!
!               *************************************************
!               **  STEP 13--                                  **
!               **  IF THERE WERE 2 OR MORE SUBSET VARIABLES,  **
!               **  WRITE OUT A FINAL MESSAGE                  **
!               **  SUMMARIZING FOR ALL VARIABLES              **
!               **  THE NUMBER OF SUBSET VARIABLES             **
!               **  THE INPUT NUMBER OF OBSERVATIONS (LOCAL),  **
!               **  THE NUMBER OF OBSERVATIONS IGNORED         **
!               **  AND THE OUTPUT NUMBER OF OBSERVATIONS      **
!               **  (THAT IS, THE SUBSET SAMPLE SIZE).         **
!               **  ALSO, CHECK THAT NS IS POSITIVE.           **
!               *************************************************
!
      ISTEPN='13'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMSV.LE.1)GO TO 1690
      ND=NIOLD-NS
!
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
!
      IF(NS.GE.1)GO TO 1690
!
!     AUGUST, 1987: FOR EMPTY SUBSETS, DO NO PRINT ERROR MESSAGE
!                   UNLESS FEEDBACK SWITCH IS ON
!
!     SEPTEMBER 2018: DO NOT TREAT AN EMPTY SUBSET AS AN ERROR
!
!CCCC IF(IFEEDB.EQ.'ON')THEN
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1611)
!1611   FORMAT('***** ERROR IN DPSUBS--')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1612)
!1612   FORMAT('      THE SUBSET IS EMPTY--IT HAS NO ELEMENTS IN IT.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
!CCCC ENDIF
!
 1690 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
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
        DO 9020 I=1,NIOLD
          WRITE(ICOUT,9021)I,ISUB(I)
 9021     FORMAT('I,ISUB(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSUBS
      SUBROUTINE DPSUM2(Y,W,N,XTEMP1,XTEMP2,XTEMP3,   &
                        DTEMP1,MAXNXT,   &
                        ICAPSW,ICAPTY,IFORSW,ICASAN,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE GENERATES A SUMMARY
!              OF THE DATA IN THE INPUT VECTOR Y.
!     NOTE--ASSUMPTION--MODEL IS   RESPONSE = CONSTANT + ERROR.
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JULY      1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --FEBRUARY  1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2002.  SUPPORT FOR HTML OUTPUT
!                                        (ADD ICAPSW, ICAPTY TO CALL
!                                        LIST)
!     UPDATED         --OCTOBER   2003.  SUPPORT FOR LATEX OUTPUT
!     UPDATED         --MAY       2011.  SUPPORT FOR REPLICATION AND
!                                        MULTIPLE RESPONSE
!     UPDATED         --MAY       2011.  USE DPDTA1 AND DPDT5B TO PRINT
!                                        THE TABLES
!     UPDATED         --JUNE      2016.  CALL LIST TO NORPPC
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASAN
!
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
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION W(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION PID(*)
!
      DOUBLE PRECISION DTEMP1(*)
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSU'
      ISUBN2='M2  '
      IERROR='NO'
      IWRITE='OFF'
!
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN SUMMARY--')
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
      GO TO 9000
  139 CONTINUE
!
!               **********************************************
!               **  STEP 3--                                **
!               **  COMPUTE VARIOUS MEASURES OF LOCATION--  **
!               **     1) MIDRANGE                          **
!               **     2) MEAN                              **
!               **     3) MIDMEAN                           **
!               **     4) MEDIAN                            **
!               **********************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL MIDRAN(Y,N,IWRITE,YMIDR,IBUGA3,IERROR)
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL MIDMEA(Y,N,IWRITE,XTEMP1,MAXNXT,YMIDM,IBUGA3,IERROR)
      CALL MEDIAN(Y,N,IWRITE,XTEMP1,MAXNXT,YMED,IBUGA3,IERROR)
!
!               **********************************************
!               **  STEP 4--                                **
!               **  COMPUTE VARIOUS MEASURES OF DISPERSION  **
!               **     1) RANGE                             **
!               **     2) STANDARD DEVIATION                **
!               **     3) AVERAGE ABSOLUTE DEVIATION        **
!               **     4) MINIMUM                           **
!               **     5) LOWER QUARTILE                    **
!               **     6) LOWER HINGE                       **
!               **     7) UPPER HINGE                       **
!               **     8) UPPER QUARTILE                    **
!               **     9) MAXIMUM                           **
!               **********************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
!               ********************************************************
!               **  STEP 5--                                          **
!               **  COMPUTE VARIOUS DISTRIBUTIONAL MEASURES--         **
!               **     1) STANDARDIZED THIRD CENTRAL MOMENT           **
!               **     2) STANDARDIZED FOURTH CENTRAL MOMENT          **
!               **     3) STANDARDIZED WILK-SHAPIRO STATISTIC         **
!               **     4) UNIFORM PROBABILITY PLOT CORRELATION COEFF  **
!               **     5) NORMAL  PROBABILITY PLOT CORRELATION COEFF  **
!               **     6) TUKEY LAMBDA = -0.5 PROBABILITY PLOT        **
!               **                            CORRELATION COEFF       **
!               **     7) CAUCHY  PROBABILITY PLOT CORRELATION COEFF  **
!               *********************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL STMOM3(Y,N,IWRITE,YST3MO,IBUGA3,IERROR)
      CALL STMOM4(Y,N,IWRITE,YST4MO,IBUGA3,IERROR)
      CALL STWS(Y,N,XTEMP1,IWRITE,YSTWS,MAXNXT,IBUGA3,IERROR)
!
      ALAMB=0.0
      ALAMB2=0.0
      MINMAX=1
      IGEPDF='NULL'
      IDIST='UNIFORM'
      CALL NORPPC(Y,N,IDIST,ALAMB,ALAMB2,   &
                  IWRITE,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,   &
                  MINMAX,IGEPDF,   &
                  YUNIPP,SHAPE,SHAPE2,ALOC,SCALE,   &
                  IBUGA3,ISUBRO,IERROR)
      IDIST='NORMAL'
      CALL NORPPC(Y,N,IDIST,ALAMB,ALAMB2,   &
                  IWRITE,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,   &
                  MINMAX,IGEPDF,   &
                  YNORPP,SHAPE,SHAPE2,ALOC,SCALE,IBUGA3,ISUBRO,IERROR)
      IDIST='CAUCHY'
      CALL NORPPC(Y,N,IDIST,ALAMB,ALAMB2,   &
                  IWRITE,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,   &
                  MINMAX,IGEPDF,   &
                  YCAUPP,SHAPE,SHAPE2,ALOC,SCALE,   &
                  IBUGA3,ISUBRO,IERROR)
      ALAMB=-0.5
      IDIST='TUKEY-LAMBDA'
      CALL NORPPC(Y,N,IDIST,ALAMB,ALAMB2,   &
                  IWRITE,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,   &
                  MINMAX,IGEPDF,   &
                  YLAMPP,SHAPE,SHAPE2,ALOC,SCALE,   &
                  IBUGA3,ISUBRO,IERROR)
!
!               *******************************************************
!               **  STEP 6--                                         **
!               **  COMPUTE VARIOUS RANDOMNESS MEASURES              **
!               **     1) AUTOCORRELATION COEFFICIENT                **
!               **     2) STANDARDIZED LENGTH OF LONGEST RUN (UP OR  **
!               **        DOWN)                                      **
!               **     3) STANDARDIZED NUMBER OF RUNS (UP + DOWN)    **
!               ********************************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL AUTOCR(Y,N,IWRITE,YAUTOC,IBUGA3,IERROR)
!CCCC CALL STLLRU(Y,N,IWRITE,YSTLLR,IBUGA3,IERROR)
      YSTLLR=0.0
!CCCC CALL STNRUN(Y,N,IWRITE,YSTNRU,IBUGA3,IERROR)
      YSTNRU=0.0
!
!               ****************************
!               **  STEP 7--              **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SUM2')   &
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
      ITITLE='Summary of xxxxxxxxxx Observations'
      WRITE(ITITLE(12:21),'(I10)')N
      NCTITL=34
      ITITLZ=' '
      NCTITZ=0
!
      ICNT=1
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
      NUMROW=ICNT
      DO 2310 I=1,NUMROW
        NTOT(I)=15
 2310 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,   &
                  NCTEXT,AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE=' '
      NCTITL=-99
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=5
      NUMLIN=1
!
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
!
      NMAX=0
      DO 4210 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=21
        IF(I.EQ.4)NTOT(I)=20
        IF(I.EQ.3)NTOT(I)=3
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.1 .OR. I.EQ.3 .OR. I.EQ.4)ITYPCO(I)='ALPH'
        DO 4213 J=1,MAXROW
          IDIGI2(J,I)=NUMDIG
          IF(I.EQ.1 .OR. I.EQ.3 .OR. I.EQ.4)THEN
            IDIGI2(J,I)=-1
          ENDIF
 4213   CONTINUE
 4210 CONTINUE
!
      DO 4289 J=1,MAXROW
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
!CCCC ROWSEP(9)=1
!
      IVALUE(1,1)='Midrange:'
      NCVALU(1,1)=9
      IVALUE(2,1)='Mean:'
      NCVALU(2,1)=5
      IVALUE(3,1)='Midmean:'
      NCVALU(3,1)=8
      IVALUE(4,1)='Median:'
      NCVALU(4,1)=7
!
      DO 4330 I=1,9
        IVALUE(I,3)=' | '
        NCVALU(I,3)=3
 4330 CONTINUE
!
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
!
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
!
      ICNT=9
      IFRST=.TRUE.
      ILAST=.FALSE.
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
!
      ITITL2(1,1)='Randomness Measures'
      NCTIT2(1,1)=19
      ITITL2(1,4)='Distributional Measures'
      NCTIT2(1,4)=23
!
      DO 4389 J=1,MAXROW
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
!
      IVALUE(1,1)='Autocorrelation Coef:'
      NCVALU(1,1)=21
!
      DO 4350 I=1,9
        IVALUE(I,3)=' | '
        NCVALU(I,3)=3
 4350 CONTINUE
!
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
!
      ICNT=7
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPSUM2
      SUBROUTINE DPSUMM(XTEMP1,XTEMP2,MAXNXT,   &
                        ICASAN,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A BATTERY OF SUMMARY STATISTICS.
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
!     ORIGINAL VERSION--JULY      1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --OCTOBER   2002. SUPPORT FOR HTML OUTPUT
!                                       (ADD ICAPSW TO CALL LIST)
!     UPDATED         --MAY       2011. USE DPPARS
!     UPDATED         --MAY       2011. SUPPORT FOR "MULTIPLE" AND
!                                       "REPLICATION" OPTIONS
!     UPDATED         --JUNE      2016. CALL LIST TO DPSUM2
!     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
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
      DIMENSION XTEMP2(*)
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
      DIMENSION XTEMP3(MAXOBV)
!
      DOUBLE PRECISION DTEMP1(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
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
      EQUIVALENCE (GARBAG(IGAR10),XTEMP3(1))
      EQUIVALENCE (GARBAG(JGAR11),XDESGN(1,1))
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      ICASAN='SUMM'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPSU'
      ISUBN2='MM  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ***********************************************
!               **  TREAT THE SUMMARY                CASE    **
!               ***********************************************
!
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
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE COMMAND                            **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:        **
!               **    1) SUMMARY             Y                     **
!               **    2) MULTIPLE SUMMARY    Y1 ... YK             **
!               **    3) REPLICATED SUMMARY  Y X1 ... XK           **
!               *****************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTC=9999
      ILASTZ=9999
      ICASAN='SUMM'
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
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'TOLE' .AND. ICTMP2.EQ.'INTE')THEN
          IFOUND='NO'
          GO TO 9000
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')THEN
        WRITE(ICOUT,91)ICASAN,IMULT,IREPL,ISHIFT
   91   FORMAT('DPSUMM: ICASAN,IMULT,IREPL,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN SUMMARY--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE SUMMARY COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')   &
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **************************************************
!               **  STEP 6--                                    **
!               **  GENERATE THE SUMMARY FOR VARIOUS CASES      **
!               ***************************************************
!
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ******************************************
!               **  STEP 8A--                           **
!               **  CASE 1: NO REPLICATION VARIABLES    **
!               ******************************************
!
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')   &
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')THEN
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
              DO 825 I=1,NS1
                WRITE(ICOUT,826)I,Y(I)
  826           FORMAT('I,Y(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
!
          CALL DPSUM2(Y,W,NS1,XTEMP1,XTEMP2,XTEMP3,DTEMP1,MAXNXT,   &
                      ICAPSW,ICAPTY,IFORSW,ICASAN,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      ISUBRO,IBUGA3,IERROR)
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
      ELSEIF(NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SUMM')   &
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
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SUMM')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPSUMM--')
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
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,   &
                          DTEMP1,MAXNXT,   &
                          ICAPSW,ICAPTY,IFORSW,ICASAN,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
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
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,   &
                          DTEMP1,MAXNXT,   &
                          ICAPSW,ICAPTY,IFORSW,ICASAN,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
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
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,   &
                          DTEMP1,MAXNXT,   &
                          ICAPSW,ICAPTY,IFORSW,ICASAN,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
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
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,   &
                          DTEMP1,MAXNXT,   &
                          ICAPSW,ICAPTY,IFORSW,ICASAN,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
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
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,   &
                          DTEMP1,MAXNXT,   &
                          ICAPSW,ICAPTY,IFORSW,ICASAN,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
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
              CALL DPSUM2(TEMP1,W,NTEMP,XTEMP1,XTEMP2,XTEMP3,   &
                          DTEMP1,MAXNXT,   &
                          ICAPSW,ICAPTY,IFORSW,ICASAN,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
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
!
      RETURN
      END SUBROUTINE DPSUMM
      SUBROUTINE DPSWAP(IOP3,NUMNAM,IHNAME,IHNAM2,IUSE,IN,   &
      IVALUE,MAXN2,MAXCO2,MAXIJ2,IBUGS2,ISUBRO,IERROR)
!CCCC SUBROUTINE DPSWAP(IOP3,V,NUMNAM,IHNAME,IHNAM2,IUSE,IN,
!CCCC1IVALUE,MAXN,MAXCOL,MAXN2,MAXCO2,MAXIJ2,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--SWAP (WRITE OUT OR READ IN) THE VECTOR V(.)
!              FROM MASS STORAGE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/1
!     ORIGINAL VERSION--MARCH     1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1986.
!     UPDATED         --OCTOBER   1991.  SUN HAS LIMIT ON NUMBER OF WORDS
!                                        THAT CAN BE WRITTEN (ALAN)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IOP3
      CHARACTER*4 IHNAME(*)
      CHARACTER*4 IHNAM2(*)
      CHARACTER*4 IUSE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
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
!CCCC CHARACTER*4 IFOUND
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      INCLUDE 'DPCODA.INC'
!CCCC DIMENSION V(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
!  FOLLOWING LINE ADDED OCTOBER 1991.
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSW'
      ISUBN2='AP  '
      ISUBN0='SWAP'
      IERROR='NO'
!
      IWIDTH=(-999)
!
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWAP')GO TO 90
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
!
!               **************************
!               **  STEP 11--           **
!               **  COPY OVER VARIABLES **
!               **************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOUNIT=ISCRNU
      IFILE=ISCRNA
      ISTAT=ISCRST
      IFORM=ISCRFO
      IACCES=ISCRAC
      IPROT=ISCRPR
      ICURST=ISCRCS
!
      ISUBN0='SWAP'
      IERRFI='NO'
!
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWAP')GO TO 1199
      WRITE(ICOUT,1193)IOUNIT
 1193 FORMAT('IOUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1194)IFILE(1:80)
 1194 FORMAT('IFILE = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195 FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',   &
      A12,2X,A12,2X,A12,2X,A12,2X,A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1196)ISUBN0,IERRFI
 1196 FORMAT('ISUBN0,IERRFI = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1199 CONTINUE
!
!               **********************************************
!               **  STEP 12--                               **
!               **  CHECK TO SEE IF SCRATCH FILE MAY EXIST  **
!               **********************************************
!
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ISTAT.EQ.'NONE')GO TO 1200
      GO TO 1290
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
      GO TO 9000
 1290 CONTINUE
!
!               *****************************************
!               **  STEP 20--                          **
!               **  BRANCH TO THE APPROPRIATE CASE--   **
!               **    1) WRITE OUT TO   MASS STORGE;   **
!               **    2) READ IN   FROM MASS STORAGE.  **
!               *****************************************
!
      ISTEPN='20'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOP3.EQ.'WRIT')GO TO 2100
      GO TO 2200
!
!               ******************************************
!               **  STEP 21--                           **
!               **  WRITE THE V(.) VECTOR               **
!               **  OUT TO THE MASS STORAGE FILE        **
!               **  WITH NUMERIC DESIGNATION    ISCRNU  **
!               ******************************************
!
 2100 CONTINUE
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MAXN2=0
      MAXCO2=0
      MAXIJ2=0
!
      IF(NUMNAM.LE.0)GO TO 2129
      DO 2110 J=1,NUMNAM
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWAP')GO TO 2119
      WRITE(ICOUT,2111)J,IHNAME(J),IHNAM2(J),IUSE(J),IN(J),IVALUE(J)
 2111 FORMAT('J,IHNAME(J),IHNAM2(J),ISE(J),IN(J),IVALUE(J) = ',   &
      I8,2X,A4,2X,A4,2X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
 2119 CONTINUE
      IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GO TO 2110
      IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GO TO 2110
      IF(IUSE(J).EQ.'V')GO TO 2115
      GO TO 2110
 2115 CONTINUE
      IROW=IN(J)
      ICOL=IVALUE(J)
      IF(ICOL.GT.MAXCOL)GO TO 2110
      IF(IROW.GT.MAXN2)MAXN2=IROW
      IF(ICOL.GT.MAXCO2)MAXCO2=ICOL
 2110 CONTINUE
 2129 CONTINUE
!
      MAXIJ2=MAXN*(MAXCO2-1)+MAXN2
      IF(MAXIJ2.LE.0)GO TO 9000
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      WRITE(ICOUT,999)
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL DPWRST('XXX','BUG ')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      WRITE(ICOUT,2191)
 2191 FORMAT('***** A SWAP OUT IS ABOUT TO BE EXECUTED.')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL DPWRST('XXX','BUG ')
!
      IDEV='SCRA'
!
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
!
!CCCC IF(MAXIJ2.GE.1)WRITE(IOUNIT)(V(IJ),IJ=1,MAXIJ2)
!
!  OCTOBER 1991.  SUN HAS LIMIT (SEEMS TO BE 2,046 WORDS) ON NUMBER OF
!  WORDS THAT CAN BE WRITTEN IN ONE RECORD.  ABOVE LINE REPLACED WITH
!  FOLLOWING BLOCK OF CODE.
!
!  MAY 2009.  ABOVE ISSUE IS NO LONGER A PROBLEM.  HOWEVER, WITH THE
!             LARGER DATA SET SIZE NOW SUPPORTED BY DATAPLOT, THIS ROUTINE
!             IS BECOMING A BIT OF A POTENTIAL BOTTLE NECK.  SPECIFICALLY,
!
!             1) IF WE USE
!
!                   WRITE(IOUNT)V
!
!                WE DECREASE THE CPU TIME USED.  HOWEVER, IT INCREASES
!                THE WALL CLOCK TIME (WRITING 10,0000,0000 VALUES AT
!                ONE TIME PROBABLY INCREASES "SWAPPING" ISSUES).
!
!             2) IF WE USE
!
!                   WRITE(IOUNT)(V(IJ),IJ=1,MAXIJ2)
!
!                 WE GREATLY INCREASE THE CPU TIME.
!
!             FOR NOW, I WILL WRITE OUT IN CHUNKS OF 10,000 (THIS WILL BE
!             SET IN MAXWRD).
!
      IF(MAXIJ2.GE.1)THEN
!CCCC   WRITE(IOUNIT)(V(IJ),IJ=1,MAXIJ2)
!CCCC   WRITE(IOUNIT)V
!
!CCCC   MAXWRD=100000
!CCCC   MAXWRD=1000000
!CCCC   IF(IHOST1.EQ.'SUN')MAXWRD=2046
        MAXWRD=10000
        IF(MAXWRD.EQ.MAXOBW)THEN
          WRITE(IOUNIT)V
          GO TO 2199
        ENDIF
        NLOOPF=(MAXIJ2/MAXWRD)+1
        IF(NLOOPF.LT.1)GO TO 2197
        DO 2192 IK=1,NLOOPF
          JSTART=(IK-1)*MAXWRD+1
          IF(JSTART.GT.MAXIJ2)GO TO 2197
          JSTOP=IK*MAXWRD
          IF(JSTOP.GT.MAXIJ2)JSTOP=MAXIJ2
          WRITE(IOUNIT) (V(IJ),IJ=JSTART,JSTOP)
 2192   CONTINUE
 2197   CONTINUE
 2199   CONTINUE
!
      ENDIF
!  END CHANGE
!
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
      GO TO 9000
!
!               ******************************************
!               **  STEP 22--                           **
!               **  READ  THE V(.) VECTOR               **
!               **  IN FROM THE MASS STORAGE FILE       **
!               **  WITH NUMERIC DESIGNATION    ISCRNU  **
!               ******************************************
!
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(MAXIJ2.LE.0)GO TO 9000
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      WRITE(ICOUT,999)
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL DPWRST('XXX','BUG ')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      WRITE(ICOUT,2291)
 2291 FORMAT('***** A SWAP IN  IS ABOUT TO BE EXECUTED.')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWAP')   &
      CALL DPWRST('XXX','BUG ')
!
      IDEV='SCRA'
!
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
!
!CCCC IF(MAXIJ2.GE.1)READ(IOUNIT)(V(IJ),IJ=1,MAXIJ2)
!
!  OCTOBER 1991.  SUN HAS LIMIT (SEEMS TO BE 2,046 WORDS) ON NUMBER OF
!  WORDS THAT CAN BE WRITTEN IN ONE RECORD.  ABOVE LINE REPLACED WITH
!  FOLLOWING BLOCK OF CODE.
!
!  MAY 2009.  SEE COMMENTS ABOVE FOR WRITE CASE.
!
      IF(MAXIJ2.GE.1)THEN
!CCCC   READ(IOUNIT)(V(IJ),IJ=1,MAXIJ2)
!CCCC   READ(IOUNIT)V
!CCCC   MAXWRD=100000
!CCCC   MAXWRD=1000000
!CCCC   IF(IHOST1.EQ.'SUN')MAXWRD=2046
        MAXWRD=10000
        IF(MAXWRD.EQ.MAXOBW)THEN
          READ(IOUNIT)V
          GO TO 2299
        ENDIF
        NLOOPF=(MAXIJ2/MAXWRD)+1
        IF(NLOOPF.LT.1)GO TO 2297
        DO 2292 IK=1,NLOOPF
          JSTART=(IK-1)*MAXWRD+1
          IF(JSTART.GT.MAXIJ2)GO TO 2297
          JSTOP=IK*MAXWRD
          IF(JSTOP.GT.MAXIJ2)JSTOP=MAXIJ2
          READ(IOUNIT) (V(IJ),IJ=JSTART,JSTOP)
 2292   CONTINUE
 2297   CONTINUE
 2299   CONTINUE
      ENDIF
!  END CHANGE
!
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWAP')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPSWAP
      SUBROUTINE DPSWA2(IOP3,IFILE,V,MAXIJ2,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--SWAP (WRITE OUT OR READ IN) THE VECTOR V(.)
!              FROM MASS STORAGE.
!              THIS IS A VARIATION OF DPSWAP.  THE DIFFERENCE
!              IS THAT THIS READS/WRITES AN ARBITRARY MATRIX,
!              NOT NECCESSARILY THE INTERNAL V MATRIX, WITH
!              MAXIJ2 DEFINING THE NUMBER OF VALUES TO READ/WRITE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST    1997.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IOP3
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
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
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      DOUBLE PRECISION V(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSW'
      ISUBN2='A2  '
      ISUBN0='SWA2'
      IERROR='NO'
      IWIDTH=(-999)
!
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWA2')GO TO 90
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
!
!               **************************
!               **  STEP 11--           **
!               **  COPY OVER VARIABLES **
!               **************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOUNIT=ISCRNU
!CCCC PASS IN FILE NAME, RECIPE CODE USES MULTIPLE SCRATCH FILES.
!CCCC IFILE=ISCRNA
      ISTAT=ISCRST
      IFORM=ISCRFO
      IACCES=ISCRAC
      IPROT=ISCRPR
      ICURST=ISCRCS
!
      ISUBN0='SWA2'
      IERRFI='NO'
!
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWA2')GO TO 1199
      WRITE(ICOUT,1193)IOUNIT
 1193 FORMAT('IOUNIT = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1194)IFILE
 1194 FORMAT('IFILE = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195 FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',   &
      A12,2X,A12,2X,A12,2X,A12,2X,A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1196)ISUBN0,IERRFI
 1196 FORMAT('ISUBN0,IERRFI = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1199 CONTINUE
!
!               **********************************************
!               **  STEP 12--                               **
!               **  CHECK TO SEE IF SCRATCH FILE MAY EXIST  **
!               **********************************************
!
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ISTAT.EQ.'NONE')GO TO 1200
      GO TO 1290
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
      GO TO 9000
 1290 CONTINUE
!
!               *****************************************
!               **  STEP 20--                          **
!               **  BRANCH TO THE APPROPRIATE CASE--   **
!               **    1) WRITE OUT TO   MASS STORGE;   **
!               **    2) READ IN   FROM MASS STORAGE.  **
!               *****************************************
!
      ISTEPN='20'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOP3.EQ.'WRIT')GO TO 2100
      GO TO 2200
!
!               ******************************************
!               **  STEP 21--                           **
!               **  WRITE THE V(.) VECTOR               **
!               **  OUT TO THE MASS STORAGE FILE        **
!               **  WITH NUMERIC DESIGNATION    ISCRNU  **
!               ******************************************
!
 2100 CONTINUE
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      WRITE(ICOUT,999)
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL DPWRST('XXX','BUG ')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      WRITE(ICOUT,2191)
 2191 FORMAT('***** A SWAP OUT IS ABOUT TO BE EXECUTED.')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL DPWRST('XXX','BUG ')
!
      IDEV='SCRA'
!
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
!
      IF(MAXIJ2.LT.1)GO TO 2199
      MAXWRD=100000
      IF(IHOST1.EQ.'SUN')MAXWRD=2046
      NLOOPF=(MAXIJ2/MAXWRD)+1
      IF(NLOOPF.LT.1)GO TO 2197
      DO 2192 IK=1,NLOOPF
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXIJ2)GO TO 2197
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXIJ2)JSTOP=MAXIJ2
      WRITE(IOUNIT) (V(IJ),IJ=JSTART,JSTOP)
 2192 CONTINUE
 2197 CONTINUE
 2199 CONTINUE
!  END CHANGE
!
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
      GO TO 9000
!
!               ******************************************
!               **  STEP 22--                           **
!               **  READ  THE V(.) VECTOR               **
!               **  IN FROM THE MASS STORAGE FILE       **
!               **  WITH NUMERIC DESIGNATION    ISCRNU  **
!               ******************************************
!
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(MAXIJ2.LE.0)GO TO 9000
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      WRITE(ICOUT,999)
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL DPWRST('XXX','BUG ')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      WRITE(ICOUT,2291)
 2291 FORMAT('***** A SWAP IN  IS ABOUT TO BE EXECUTED.')
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SWA2')   &
      CALL DPWRST('XXX','BUG ')
!
      IDEV='SCRA'
!
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
!
      IF(MAXIJ2.LT.1)GO TO 2299
      MAXWRD=100000
      IF(IHOST1.EQ.'SUN')MAXWRD=2046
      NLOOPF=(MAXIJ2/MAXWRD)+1
      IF(NLOOPF.LT.1)GO TO 2297
      DO 2292 IK=1,NLOOPF
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXIJ2)GO TO 2297
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXIJ2)JSTOP=MAXIJ2
      READ(IOUNIT) (V(IJ),IJ=JSTART,JSTOP)
 2292 CONTINUE
 2297 CONTINUE
 2299 CONTINUE
!  END CHANGE
!
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SWA2')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPSWA2
      SUBROUTINE DPSYMB(IHARG,NUMARG,IDEFSY,ITEXSY,   &
                        IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE SYMBOL CHARACTER WHICH MAY
!              BE USED TO DENOTE IN-LINE TEXT SUB-COMMANDS.
!              WHEN A TEXT STRING IS PROCESSED,
!              IT IS SCANNED FOR THE SYMBOL CHARACTER;
!              IF IT IS FOUND, THE IN-LINE SUB-COMMAND
!              BEFORE THE SYMBOL CHARACTER IS EXECUTED
!              RATHER THAN THE LITERAL TEXT SUB-STRING BEING WRITTEN OUT.
!              ANY NUMBER OF SYMBOL CHARACTERS ARE ALLOWED PER LINE.
!              THE SYMBOL CHARACTER CAPABILITY ALLOWS THE ANALYST
!              TO WRITE GREEK, MATH, AND OTHER SPECIAL SYMBOLS.
!              THE SPECIFIED SYMBOL CHARACTER WILL BE PLACED
!              IN THE CHARACTER VARIABLE ITEXSY.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFSY (A  CHARACTER VARIABLE)
!                     --IBUGD2 (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--ITEXSY (A CHARACTER VARIABLE)
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
!     ORIGINAL VERSION--NOVEMBER 1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFSY
      CHARACTER*4 ITEXSY
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
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
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
      DO 55 I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
      IF(NUMARG.LE.0)GO TO 1150
      GO TO 1110
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD=IDEFSY
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      ITEXSY=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
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
      GO TO 9000
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPSYMB
      SUBROUTINE DPSYMM(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A SYMMETRY PLOT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/7
!     ORIGINAL VERSION--APRIL     1986.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --NOVEMBER  2011. 1) USE DPPARS TO PERFORM
!                                          SOME OF THE PARSING
!                                       2) SUPPORT "REPLICATION" AND
!                                          "MULTIPLE" KEYWORDS
!                                       3) SUPPORT "HIGHLIGHT" OPTION
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
!
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
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
!CCCC FOLLOWING LINES ADDED JUNE, 1990
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
!CCCC END CHANGE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPSY'
      ISUBN2='MM  '
!
      IHIGH='OFF'
      IMULT='OFF'
      IREPL='OFF'
      ICASE='NONE'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ***************************************
!               **  TREAT THE SYMMETRY    PLOT CASE  **
!               ***************************************
!
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
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
!     LOOK FOR THE WORDS "SYMMETRY PLOT".  ALSO LOOK
!     FOR THE KEYWORDS "MULTIPLE", "REPLICATION", OR "HIGHLIGHT".
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTC=-9999
      DO 100 I=0,NUMARG-1
!
        IF(I.EQ.0)THEN
          ITERM1=ICOM
          ITERM2=IHARG(I+1)
          ITERM3=IHARG(I+2)
        ELSE
          ITERM1=IHARG(I)
          ITERM2=IHARG(I+1)
          ITERM3=IHARG(I+2)
        ENDIF
!
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
!
      IF(IFOUND.EQ.'NO')GO TO 9000
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN SYMMETRY PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION" FOR THE SYMMETRY PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(IHIGH.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)
  122     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"HIGHTLIGHTED" FOR THE SYMMETRY PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
      ICASPL='SYMM'
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!               **  STEP 3--                                 **
!               **  DETERMINE:                               **
!               **  1) NUMBER OF REPLICATION VARIABLES (0-2) **
!               **  2) NUMBER OF GROUPING    VARIABLES (0-2) **
!               **  3) NUMBER OF RESPONSE    VARIABLES (>= 1)**
!               **  4) NUMBER OF HIGHLIGHT   VARIABLES (0-2) **
!               ***********************************************
!
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
  501     FORMAT('      FOR THE HIGHLIGHTED CASE, THE NUMBER OF ',   &
                 'HIGHLIGHT VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,502)
  502     FORMAT('      MUST BE ONE OR TWO;  SUCH WAS NOT THE ',   &
                 'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,503)NHIGH
  503     FORMAT('      THE NUMBER OF HIGHLIGHT VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
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
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',   &
                 'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN 1 AND 2;  SUCH WAS NOT THE ',   &
                 'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
!               *********************************************
!               **  STEP 7A--                              **
!               **  CASE 1: NO REPLICATION, NO MULTIPLE,   **
!               **          AND NO HIGHLIGHTING            **
!               *********************************************
!
!     FOR THIS CASE, CAN HAVE ONE TO TWO RESPONSE VARIABLES
!     (DEPDENDING ON WHETHER WE HAVE BINNED DATA OR RAW DATA).
!
!     FOR THIS CASE, ONLY SUPPORT MATRIX ARGUMENT FOR RAW DATA
!     NUMBER OF OBSERVATIONS MUST BE THE SAME FOR ALL VARIABLES.
!
      IF(IMULT.EQ.'OFF' .AND. NREPL.EQ.0 .AND. NHIGH.EQ.0)THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        IF(NUMVAR.EQ.1)THEN
          IDATSW='RAW'
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y1,X1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
        ELSEIF(NUMVAR.EQ.2)THEN
          IDATSW='FREQ'
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y1,X1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
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
          GO TO 9000
        ELSEIF(NUMVAR.EQ.2 .AND. NLOCAL.NE.NLOCA2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,711)
  711     FORMAT('      FOR THE FREQUENCY CASE, THE NUMBER OF ',   &
                 'OBSERVATIONS FOR')
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
          GO TO 9000
        ENDIF
!
!       *****************************************************
!       **  STEP 7B--                                      **
!       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!       **  RESET THE VECTOR D(.) TO ALL ONES.             **
!       **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
!       **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
!       *****************************************************
!
!
        IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SYMM')THEN
          ISTEPN='7B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,731)
  731     FORMAT('***** FROM THE MIDDLE  OF DPSYMM--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,732)ICASPL,NUMVAR,IDATSW,NLOCAL
  732     FORMAT('ICASPL,NUMVAR,IDATSW,NLOCAL = ',   &
                 A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 735 I=1,NLOCAL
              WRITE(ICOUT,736)I,Y1(I),X1(I)
  736         FORMAT('I,Y1(I),X1(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
  735       CONTINUE
          ENDIF
        ENDIF
!
        NPLOTP=0
        NCURVE=1
        CALL DPSYM2(Y1,X1,NLOCAL,ICASPL,IDATSW,MAXOBV,   &
                    NUMVAR,NCURVE,NHIGH,   &
                    TAG1,XTEMP1,XTEMP2,   &
                    Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               ******************************************
!               **  STEP 8A--                           **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES **
!               **          NOTE THAT HIGHLIGHTING AND  **
!               **          GROUPING ARE NOT SUPPORTED  **
!               **          FOR THIS CASE.              **
!               ******************************************
!
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
        NPLOTP=0
        NCURVE=0
        DO 810 IRESP=1,NRESP
          NCURVE=NCURVE+1
!
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PERC')THEN
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
                      IBUGG3,ISUBRO,IFOUND,IERROR)
!
!         *****************************************************
!         **  STEP 8B--                                      **
!         **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!         **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!         **  RESET THE VECTOR D(.) TO ALL ONES.             **
!         **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
!         **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
!         *****************************************************
!
!
          IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SYMM')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPSYMM--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASPL,NUMVAR,IDATSW,NLOCAL
  823       FORMAT('ICASPL,NUMVAR,IDATSW,NLOCAL = ',   &
                   A4,I8,2X,A4,I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO 825 I=1,NLOCAL
                WRITE(ICOUT,826)I,Y1(I)
  826           FORMAT('I,X1(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
!
          CALL DPSYM2(Y1,X1,NLOCAL,ICASPL,IDATSW,MAXOBV,   &
                      NUMVAR,NCURVE,NHIGH,   &
                      TAG1,XTEMP1,XTEMP2,   &
                      Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
  810   CONTINUE
!
!               ***************************************************
!               **  STEP 9A--                                    **
!               **  CASE 3: ONE OR MORE REPLICATION VARIABLES.   **
!               **          CURRENTLY, ONLY SUPPORT THIS OPTION  **
!               **          FOR UNBINNED DATA.                   **
!               ***************************************************
!
      ELSEIF(NRESP.GE.1 .AND. NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO 910 I=1,IMAX
          IF(ISUB(I).EQ.0)GO TO 910
          J=J+1
!
!         RESPONSE VARIABLE IN X1 (OR Y1 IF GROUPED DATA)
!
          IJ=MAXN*(ICOLR(1)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
          ICOLC=1
!
          DO 920 IR=1,MIN(NREPL,2)
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
!
  910   CONTINUE
        NLOCAL=J
!
!       *****************************************************
!       **  STEP 9B--                                      **
!       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!       **                                                 **
!       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
!       **  VARIOUS REPLICATIONS.                          **
!       *****************************************************
!
        IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SYMM')THEN
          ISTEPN='9B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,931)
  931     FORMAT('***** FROM THE MIDDLE  OF DPSYMM--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,932)ICASPL,NUMVAR,IDATSW,NLOCAL
  932     FORMAT('ICASPL,NUMVAR,IDATSW,NLOCAL = ',   &
                 A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 935 I=1,NLOCAL
              WRITE(ICOUT,936)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  936         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2)=',I8,3G15.7)
              CALL DPWRST('XXX','BUG ')
  935       CONTINUE
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
                   XIDTEM,XIDTE2,XIDTE2,XIDTE2,XIDTE2,XIDTE2,   &
                   XTEMP1,XTEMP2,   &
                   NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,   &
                   IBUGG3,ISUBRO,IERROR)
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
            DO 1130 I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                ZY(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NUMVA2=1
            IF(NTEMP.GT.0)THEN
              CALL DPSYM2(ZY,X1,NTEMP,ICASPL,IDATSW,MAXOBV,   &
                          NUMVA2,NCURVE,NHIGH,   &
                          TAG1,XTEMP1,XTEMP2,   &
                          Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO 1210 ISET1=1,NUMSE1
          DO 1220 ISET2=1,NUMSE2
            K=0
            DO 1290 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2)   &
                )THEN
                K=K+1
                ZY(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NUMVA2=1
            IF(NTEMP.GT.0)THEN
              CALL DPSYM2(ZY,X1,NTEMP,ICASPL,IDATSW,MAXOBV,   &
                          NUMVA2,NCURVE,NHIGH,   &
                          TAG1,XTEMP1,XTEMP2,   &
                          Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1220     CONTINUE
 1210     CONTINUE
        ENDIF
!
!               ***************************************************
!               **  STEP 10A--                                   **
!               **  CASE 4: ONE OR TWO HIGHLIGHT VARIABLES.      **
!               **          THIS CASE DOES NOT SUPPORT GROUPED   **
!               **          DATA AND ALL VARIABLES MUST HAVE     **
!               **          SAME LENGTH.                         **
!               ***************************************************
!
      ELSEIF(NRESP.EQ.1 .AND. NHIGH.GE.1)THEN
        ISTEPN='10A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SYMM')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          ICOL=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y1,X1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
!
        IF(NHIGH.EQ.1)THEN
          CALL CODE(X1,NLOCAL,IWRITE,TAG1,XTEMP1,MAXOBV,   &
                    IBUGG3,IERROR)
        ELSE
          ICCTOF=0
          ICCTG1=0
          CALL CODCT2(X1,XTEMP1,NLOCAL,ICCTOF,ICCTG1,IWRITE,   &
                      TAG1,XTEMP2,XTEMP3,   &
                      IBUGG3,ISUBRO,IERROR)
        ENDIF
!
!       *****************************************************
!       **  STEP 10B--                                     **
!       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!       **                                                 **
!       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
!       **  VARIOUS REPLICATIONS.                          **
!       *****************************************************
!
        IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'SYMM')THEN
          ISTEPN='10B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1731)
 1731     FORMAT('***** FROM THE MIDDLE  OF DPSYMM--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1732)ICASPL,NUMVAR,IDATSW,NLOCAL
 1732     FORMAT('ICASPL,NUMVAR,IDATSW,NQ = ',   &
                 A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 1735 I=1,NLOCAL
              WRITE(ICOUT,1736)I,Y1(I),TAG1(I)
 1736         FORMAT('I,Y1(I),TAG1(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 1735       CONTINUE
          ENDIF
        ENDIF
!
!       ************************************
!       **  STEP 10C--                    **
!       **  GENERATE THE SYMMETRY PLOT    **
!       ************************************
!
        NPLOTP=0
        NCURVE=1
        CALL DPSYM2(Y1,X1,NLOCAL,ICASPL,IDATSW,MAXOBV,   &
                    NUMVAR,NCURVE,NHIGH,   &
                    TAG1,XTEMP1,XTEMP2,   &
                    Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9020 I=1,NPLOTP
           WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021      FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
           CALL DPWRST('XXX','BUG ')
 9020    CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPSYMM
      SUBROUTINE DPSYM2(Y,X,N,ICASPL,IDATSW,MAXOBV,   &
                        NUMVAR,NCURVE,NHIGH,   &
                        TAG1,XTEMP1,XTEMP2,   &
                        Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A SYMMETRY PLOT.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/7
!     ORIGINAL VERSION--APRIL     1986.
!     UPDATED         --NOVEMBER  2011. SUPPORT FOR HIGHLIGHTED CASE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IDATSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRIT2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION TAG1(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPSY'
      ISUBN2='M2  '
      IERROR='NO'
!
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
          DO 85 I=1,N
            WRITE(ICOUT,86)I,Y(I),X(I)
   86       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   85     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
      WRITE(ICOUT,62)
   62 FORMAT('      ALL INPUT VERTICAL AXIS ELEMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)HOLD
   63 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
   69 CONTINUE
!
!               **************************************
!               **  STEP 4--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **  AND DETERMINE PLOT COORDINATES  **
!               **************************************
!
      IF(IDATSW.EQ.'RAW')THEN
!
!               ****************************************
!               **  STEP 4.1--                        **
!               **  DETERMINE PLOT COORDINATES        **
!               **  FOR THE 1-VARIABLE CASE           **
!               **  (THAT IS, FOR THE RAW DATA CASE)  **
!               ****************************************
!
        IWRIT2='OFF'
        CALL MEDIAN(Y,N,IWRIT2,XTEMP1,MAXOBV,XMED,IBUGG3,IERROR)
!
        IF(NHIGH.EQ.0)THEN
          CALL SORT(Y,N,XTEMP1)
          NHALFP=(N+1)/2
          DO 1110 I=1,NHALFP
            IREV=N-I+1
            Y2(N2+I)=XTEMP1(IREV)-XMED
            X2(N2+I)=XMED-XTEMP1(I)
            D2(N2+I)=REAL(NCURVE)
 1110     CONTINUE
          N2=N2+NHALFP
          NPLOTV=2
        ELSE
!
!         HIGHLIGHT CASE: BASE HIGHLIGHTING ON MAXIMUM OF
!                         THE TWO POINTS THAT GENERATE A SINGLE
!                         PLOT POINT.
!
          CALL SORTC(Y,TAG1,N,XTEMP1,XTEMP2)
          NHALFP=(N+1)/2
          DO 1210 I=1,NHALFP
            IREV=N-I+1
            Y2(N2+I)=XTEMP1(IREV)-XMED
            X2(N2+I)=XMED-XTEMP1(I)
            D2(N2+I)=MAX(XTEMP2(I),XTEMP2(IREV))
 1210     CONTINUE
          N2=N2+NHALFP
          NPLOTV=2
        ENDIF
      ELSEIF(IDATSW.EQ.'FREQ')THEN
!
!               ********************************************
!               **  STEP 4.2--                            **
!               **  DETERMINE PLOT COORDINATES            **
!               **  FOR THE 2-VARIABLE CASE               **
!               **  (THAT IS, FOR THE GROUPED DATA CASE)  **
!               ********************************************
!
        CALL SORTC(X,Y,N,D2,Y2)
!
        SUM=0.0
        DO 2110 I=1,N
         SUM=SUM+Y(I)
 2110   CONTINUE
        NTOT=INT(SUM+0.5)
!
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
 2114     FORMAT('      UNGROUPED NUMBER OF OBSERVATIONS IS TOO ',   &
                 'LARGE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2116)NTOT
 2116     FORMAT('      NTOT = ',I8)
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
        K=0
        DO 2121 I=1,N
          NI=INT(Y2(I)+0.5)
          IF(NI.LE.0)GO TO 2121
          DO 2122 J=1,NI
            K=K+1
            X2(K)=D2(I)
 2122     CONTINUE
 2121   CONTINUE
!
        IWRIT2='OFF'
        MAXND2=1000
        CALL MEDIAN(X2,K,IWRIT2,D2,MAXND2,XMED,IBUGG3,IERROR)
        CALL SORT(X2,K,D2)
!
        KHALFP=(K+1)/2
        DO 2130 I=1,KHALFP
          IREV=K-I+1
          Y2(I)=D2(IREV)-XMED
          X2(I)=XMED-D2(I)
 2130   CONTINUE
        DO 2140 I=1,KHALFP
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
        GO TO 9000
      ENDIF
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPSYM2
      SUBROUTINE DPSYST(IANS,IANSLC,IWIDTH,                       &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                        IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,        &
                        IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--ENTER AN OPERATING SYSTEM COMMAND.  NOTE THAT THIS COMMAND
!              IS SITE AND HOST DEPENDENT.  IT IS PROVIODED TO ACCOMODATE
!              THOSE OPERATING SYSTEMS THAT ALLOW HOOKS INTO THE OPERATING
!              SYSTEM.  IT IS LEFT UP TO THE LOCAL IMPLEMENTOR AS TO HOW
!              THIS COMMAND WILL BE USED.
!
!              THE CALL TO THE OPERATING SYSTEM IS DONE BELOW IN
!                    CALL SCLCMD
!              IF YOUR COMPUTER DOES NOT ALLOW SUCH A HOOK, DO NOTHING.
!              IF YOUR COMPUTER DOES ALLOW SUCH A HOOK, THEN THE
!              IMPLEMENTER SHOULD REPLACE THE CALL TO SCLCMD
!              (WHICH IS APPROPRIATE ONLY FOR CDC CYBER NOS/VE)
!              WITH THE APPROPRIATE SYSTEM CALL;
!              THE LINE SHOULD ALSO BE UN-COMMENTED OUT.
!
!              NOTE THAT IF A COMMAND IS PASSED TO THE OPERATING SYSTEM,
!              DATAPLOT WILL DO NO ERROR CHECKING.  IT WILL SIMPLY PASS
!              THE COMMAND AS GIVEN.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--89.3
!     ORIGINAL VERSION--FEBRUARY   1989.
!     UPDATED         --MARCH      1990. USE "IANSLC" SINCE SOME SYSTEMS
!                                        ARE CASE SENSITIVE (E.G., UNIX)
!     UPDATED         --APRIL      1992. DO OPERATING SPECIFIC CALL IN DPSYS2
!     UPDATED         --APRIL      1992. ADD ISUBRO IN CALL TO DPSYS2
!     UPDATED         --APRIL      1992. ADD UNIX & DOS
!     UPDATED         --APRIL      1992. ADD OTG CHECK
!     UPDATED         --APRIL      1992. AUGMENT ERROR INFO
!     UPDATED         --APRIL      2018. SINCE THE PLATFORM DEPENDENT
!                                        CODE IS IN DPSYS2, RECODE THIS
!                                        ROUTINE AND MOVE IT OUT OF
!                                        dp1.F.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
!
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
!
      PARAMETER(MAXCH=256)
      DIMENSION ITEXTE(MAXCH)
      DIMENSION ITEXTF(MAXCH)
      CHARACTER*256 ITEXT2
      CHARACTER*256 ITEXT3
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      J2=0
!
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
        DO 76 I=1,NUMNAM
          WRITE(ICOUT,77)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                         IVALUE(I),VALUE(I)
   77     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)= ',   &
                 I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   76   CONTINUE
      ENDIF
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
!               *****************************************************
!
!               *****************************************
!               **  STEP 1.1--                         **
!               **  DETERMINE THE COMMAND              **
!               **  (SYSTEM OR SYST) AND ITS LOCATION  **
!               **  ON THE LINE.                       **
!               **  DETERMINE THE START POSITION       **
!               **  (XSTART) OF THE FIRST CHARACTER    **
!               **  FOR THE STRING TO BE PRINTED.      **
!               *****************************************
!
!     CHECK FOR "SYSTEM" FIRST
!
      DO 1115 I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
        IP6=I+6
!
        IF(IP6.GT.IWIDTH)GO TO 1115
        ISTART=IP6+1
        IF(IANS(I).EQ.'S'.AND.IANS(IP1).EQ.'Y'.AND.   &
           IANS(IP2).EQ.'S'.AND.IANS(IP3).EQ.'T'.AND.   &
           IANS(IP4).EQ.'E'.AND.IANS(IP5).EQ.'M'.AND.   &
           IANS(IP6).EQ.' ')GO TO 1190
 1115 CONTINUE
!
!     CHECK FOR "SYST"
!
      DO 1125 I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
!
        IF(IP4.GT.IWIDTH)GO TO 1125
        ISTART=IP5
        IF(IANS(I).EQ.'S'.AND.IANS(IP1).EQ.'Y'.AND.   &
           IANS(IP2).EQ.'S'.AND.IANS(IP3).EQ.'T'.AND.   &
           IANS(IP4).EQ.' ')GO TO 1190
 1125 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED   APRIL 1992
!     CHECK FOR "UNIX"
!
      DO 1135 I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
!
        IF(IP4.GT.IWIDTH)GO TO 1135
        ISTART=IP5
        IF(IANS(I).EQ.'U'.AND.IANS(IP1).EQ.'N'.AND.   &
           IANS(IP2).EQ.'I'.AND.IANS(IP3).EQ.'X'.AND.   &
           IANS(IP4).EQ.' ')GO TO 1190
 1135 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED   APRIL 1992
!     CHECK FOR "DOS"
!
      DO 1145 I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
!
        IF(IP3.GT.IWIDTH)GO TO 1145
        ISTART=IP4
        IF(IANS(I).EQ.'D'.AND.IANS(IP1).EQ.'O'.AND.   &
           IANS(IP2).EQ.'S'.AND.IANS(IP3).EQ.' ')GO TO 1190
 1145 CONTINUE
!
!CCCC THE FOLLOWING SECTION WAS ADDED   APRIL 2018
!     CHECK FOR "LUNIX"
!
      DO 1155 I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
!
        IF(IP4.GT.IWIDTH)GO TO 1155
        ISTART=IP5
        IF(IANS(I).EQ.'L'.AND.IANS(IP1).EQ.'I'.AND.   &
           IANS(IP2).EQ.'N'.AND.IANS(IP3).EQ.'U'.AND.   &
           IANS(IP4).EQ.'X'.AND.IANS(IP5).EQ.' ')GO TO 1190
 1155 CONTINUE
!
!     NO MATCH
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN SYSTEM COMMAND--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO:  SYSTEM, SYST, UNIX, LINUX ',   &
             'OR DOS.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1190 CONTINUE
!
!               **********************************************************
!               **  STEP 1.2--                                          **
!               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.   **
!               **********************************************************
!
      IFOUND='YES'
!
      ISTOP=0
      IF(ISTART.LE.IWIDTH)THEN
        DO 1220 I=ISTART,IWIDTH
          IREV=IWIDTH-I+ISTART
          IF(IANS(IREV).NE.' ')THEN
            ISTOP=IREV
            GO TO 1229
          ENDIF
 1220   CONTINUE
 1229   CONTINUE
      ENDIF
!
!               *****************************************
!               **  STEP 1.3--                         **
!               **  COPY OVER THE STRING OF INTEREST.  **
!               *****************************************
!
      IF(ISTART.GT.ISTOP .OR. ISTOP.EQ.0)THEN
        NCTEX=0
      ELSE
        ITEMP=ISTOP-ISTART+1
        IF(ITEMP.GT.MAXCH)ITEMP=MAXCH
        ISTOP=ISTART+ITEMP-1
!
        J=0
        DO 1310 I=ISTART,ISTOP
          J=J+1
          J2=J
          ITEXTE(J)=IANS(I)
          ITEXTF(J)=IANSLC(I)
 1310   CONTINUE
        NCTEX=J2
      ENDIF
!
!               ******************************************************
!               **  STEP 1.4--                                      **
!               **  CALL THE SUBROUTINE DPREPL                      **
!               **  WHICH WILL SCAN THE STRING FOR ALL OCCURRANCES  **
!               **  OF THE SUBSTRING VALU()                         **
!               **  AND REPLACE THEM BY THEIR LITERAL VALUES.       **
!               ******************************************************
!
      NCTEXT=NCTEX
      IF(NCTEXT.GE.1)THEN
        CALL DPREPL(ITEXTE,NCTEXT,   &
                    IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                    IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
                    IBUGD2,IERROR)
      ENDIF
!
      IF(NCTEXT.GE.1)THEN
        DO 1510 I=1,NCTEXT
          ITEXT2(I:I)=ITEXTE(I)(1:1)
 1510   CONTINUE
      ENDIF
!
      NCTEXT=NCTEX
      IF(NCTEXT.GE.1)THEN
        CALL DPREPL(ITEXTF,NCTEXT,   &
                    IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                    IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
                    IBUGD2,IERROR)
      ENDIF
      IF(NCTEXT.GE.1)THEN
        DO 1610 I=1,NCTEXT
          ITEXT3(I:I)=ITEXTF(I)(1:1)
 1610   CONTINUE
      ENDIF
!
!               *****************************************
!               **  STEP 2--                           **
!               **  CALL DPSYS2 TO EXECUTE THE COMMAND **
!               *****************************************
!
!     2018/04: ALTHOUGH WINDOWS COMMANDS ARE NOT CASE SENSITIVE,
!              ARGUMENTS MAY BE.  SO USE LOWER CASE VARIANT FOR
!              ALL SYSTEMS.
!
!CCCC IF(IOPSY1.EQ.'UNIX')THEN
        CALL DPSYS2(ITEXT3,NCTEXT,ISUBRO,IERROR)
!CCCC ELSE
!CCCC   CALL DPSYS2(ITEXT2,NCTEXT,ISUBRO,IERROR)
!CCCC ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9017   FORMAT('IFOUND,IERROR,IREPCH,NCTEXT,NCTEX = ',   &
               2(A4,2X),A1,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT2(J:J),J=1,NCTEXT)
 9018   FORMAT('(ITEXT2(I),I=1,NCTEXT) = ',25A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPSYST
      SUBROUTINE DPTAB1(IHEAD,NHEAD,CAPTN,NCAP,IFLAG1)
!
!     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
!              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED
!              TO INITIATE THE TABULAR OUTPUT.  THE ONLY OPTIONAL ELEMENT
!              IS THE CAPTION.
!     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
!                                THE TEXT FOR THE HEADER
!                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
!                                THE NUMBER OF CHARACTERS IN THE
!                                HEADER.
!                     --CAPTN  = THE CHARACTER STRING CONTAINING
!                                THE CAPTION.
!                     --NCAP   = THE INTEGER NUMBER THAT SPECIFIES
!                                THE NUMBER OF CHARACTERS IN THE
!                                CAPTION.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABOARATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/3
!     ORIGINAL VERSION--MARCH     2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*(*) CAPTN
      CHARACTER*(*) IHEAD
!
      LOGICAL IFLAG1
      CHARACTER*10 IFORMT
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  STEP 1: WRITE A HEADER
!
  999 FORMAT(1X)
!
      IF(IFLAG1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
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
!
!  STEP 2: START TABLE AND DEFINE A CAPTION
!
      IF(NCAP.GT.0 .AND. NCAP.LT.130)THEN
        IFORMT=' '
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NCAP
        WRITE(ICOUT,IFORMT)CAPTN(1:NCAP)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAB1
      SUBROUTINE DPTABA(IHEAD,NHEAD,CAPTN,NCAP,IFLAG1)
!
!     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
!              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED
!              TO INITIATE THE TABULAR OUTPUT.  THE ONLY OPTIONAL ELEMENT
!              IS THE CAPTION.
!
!              NOTE: THIS IS A SLIGHT VARIANT OF DPTAB1.  DIFFERS
!                    IN POSITIONING OF "CAPTN" STRING.
!
!     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
!                                THE TEXT FOR THE HEADER
!                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
!                                THE NUMBER OF CHARACTERS IN THE
!                                HEADER.
!                     --CAPTN  = THE CHARACTER STRING CONTAINING
!                                THE CAPTION.
!                     --NCAP   = THE INTEGER NUMBER THAT SPECIFIES
!                                THE NUMBER OF CHARACTERS IN THE
!                                CAPTION.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABOARATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/3
!     ORIGINAL VERSION--MARCH     2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*(*) CAPTN
      CHARACTER*(*) IHEAD
!
      LOGICAL IFLAG1
      CHARACTER*10 IFORMT
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  STEP 1: WRITE A HEADER
!
  999 FORMAT(1X)
!
      IF(IFLAG1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(NHEAD.GE.1)THEN
        IFORMT=' '
        IFORMT(1:9)='(12X,A  )'
        WRITE(IFORMT(7:8),'(I2)')NHEAD
        WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!  STEP 2: START TABLE AND DEFINE A CAPTION
!
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
!
      RETURN
      END SUBROUTINE DPTABA
      SUBROUTINE DPTAB4(IVALUE,NCHAR,NHEAD,IFLAG1,IFLAG2,NMAX)
!
!     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
!              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED TO
!              GENERATE A HEADER ROW FOR A TABLE.  YOU CAN ALSO OPTIONALLY
!              ADD A RULE LINE BEFORE OR AFTER THE HEADER.
!
!     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING ARRAY
!                                 CONTAINING THE TEXT FOR THE
!                                 HEADER VALUES.
!                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
!                                 THE NUMBER OF CHARACTERS IN THE
!                                 HEADER VALUES.
!                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
!                                 THE NUMBER OF HEADER VALUES.
!                     --IFLAG1  = A LOGICAL VALUE THAT SPECIFIES
!                                 WHETHER A RULE LINE IS DRAWN BEFORE
!                                 THE HEADER.
!                     --IFLAG2  = A LOGICAL VALUE THAT SPECIFIES
!                                 WHETHER A RULE LINE IS DRAWN AFTER
!                                 THE HEADER.
!                     --NMAX    = NUMBER OF CHARACTERS FOR "RULE" LINE
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABOARATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/3
!     ORIGINAL VERSION--MARCH     2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*(*) IVALUE(NHEAD)
      INTEGER NCHAR(NHEAD)
!
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
!
      CHARACTER*255 IATEMP
!
      LOGICAL IFLAG1
      LOGICAL IFLAG2
!
      CHARACTER*20 IFORMT
      CHARACTER*160 ISTR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!C999 FORMAT(1X)
!
!  STEP 1: PRINT INITIAL RULE LINE
!
      IF(NHEAD.GE.1)THEN
        IF(IFLAG1 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO 8010 I=1,MIN(NMAX,255)
            IATEMP(I:I)='-'
 8010     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
!  STEP 2: PRINT TEXT FIELDS
!
        IFORMT=' '
        NCSTR=0
        DO 8020 I=1,NHEAD
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
!
!  STEP 3: PRINT TRAILING RULE LINE
!
        IF(IFLAG2 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO 8030 I=1,NMAX
            IATEMP(I:I)='-'
 8030     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAB4
      SUBROUTINE DPTA44(IVALUE,NCHAR,NHEAD,NCOLSP,IFLAG1,IFLAG2,NMAX)
!
!     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
!              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED TO
!              GENERATE A HEADER ROW FOR A TABLE.  YOU CAN ALSO OPTIONALLY
!              ADD A RULE LINE BEFORE OR AFTER THE HEADER.
!
!              THIS IS A MODIFIED VERSION OF DPTAB4 THAT ALLOWS
!              HEADERS THAT SPAN MULTIPLE COLUMNS.
!
!     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING ARRAY
!                                 CONTAINING THE TEXT FOR THE
!                                 HEADER VALUES.
!                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
!                                 THE NUMBER OF CHARACTERS IN THE
!                                 HEADER VALUES.
!                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
!                                 THE NUMBER OF HEADER VALUES.
!                     --NCOLSP  = THE INTEGER ARRAY THAT SPECIFIES
!                                 THE NUMBER OF SPANNING COLUMNS.
!                     --IFLAG1  = A LOGICAL VALUE THAT SPECIFIES
!                                 WHETHER A RULE LINE IS DRAWN BEFORE
!                                 THE HEADER.
!                     --IFLAG2  = A LOGICAL VALUE THAT SPECIFIES
!                                 WHETHER A RULE LINE IS DRAWN AFTER
!                                 THE HEADER.
!                     --NMAX    = NUMBER OF CHARACTERS FOR "RULE" LINE
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABOARATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/1
!     ORIGINAL VERSION--JANUARY   2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*(*) IVALUE(NHEAD)
      INTEGER NCHAR(NHEAD)
      INTEGER NCOLSP(NHEAD)
!
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
!
      CHARACTER*255 IATEMP
!
      LOGICAL IFLAG1
      LOGICAL IFLAG2
!
      CHARACTER*20 IFORMT
      CHARACTER*160 ISTR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!C999 FORMAT(1X)
!
!  STEP 1: PRINT INITIAL RULE LINE
!
      IF(NHEAD.GE.1)THEN
        IF(IFLAG1 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO 8010 I=1,MIN(NMAX,255)
            IATEMP(I:I)='-'
 8010     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
!  STEP 2: PRINT TEXT FIELDS
!
        IFORMT=' '
        NCSTR=0
        DO 8020 I=1,NHEAD
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
!
!  STEP 3: PRINT TRAILING RULE LINE
!
        IF(IFLAG2 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO 8030 I=1,NMAX
            IATEMP(I:I)='-'
 8030     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
      ENDIF
!
      RETURN
      END SUBROUTINE DPTA44
      SUBROUTINE DPTAB5(IVALUE,NCHAR,AVALUE,NHEAD,IFLAG1,NMAX,NTOT,   &
                        ICSVWR)
!
!     PURPOSE--THIS ROUTINE IS A UTILITY ROUTINE FOR CREATING
!              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED TO
!              GENERATE A DATA ROW FOR A TABLE.  THE FIRST FIELD CAN
!              BE A TEXT VALUE (FOR A ROW LABEL).
!
!     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING CONTAINING
!                                 THE TEXT FOR THE FIRST COLUMN.
!                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
!                                 THE NUMBER OF CHARACTERS IN THE
!                                 FIRST TEXT FIELD.
!                     --AVALUE  = A REAL ARRAY CONTAINING THE DATA
!                                 TO BE GENERATED.
!                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
!                                 THE NUMBER OF NUMERIC VALUES.
!                     --IFLAG1  = A LOGICAL VALUE THAT SPECIFIES WHETHER
!                                 A RULE LINE WILL BE PRINTED AFTER THE
!                                 ROW
!                     --NMAX    = NUMBER OF CHARACTERS IN RULE LINE
!                     --NTOT    = AN INTEGER ARRAY CONTAINING THE TOTAL
!                                 NUMBER OF CHARACTERS IN EACH FIELD
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABOARATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/3
!     ORIGINAL VERSION--MARCH     2009.
!     UPDATED         --APRIL     2009. ADDITIONAL FORMATTING OPTIONS
!     UPDATED         --APRIL     2015. SUPPORT HORIZONTAL ALIGNMENT
!                                       (LEFT, CENTER, RIGHT)
!     UPDATED         --FEBRUARY  2020. OPTION FOR WRITING CVS FILES
!                                       (FOR IMPORTING INTO OTHER
!                                       PROGRAMS)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*(*) IVALUE
      CHARACTER*4 ICSVWR
      REAL AVALUE(NHEAD)
      INTEGER NTOT(*)
      INTEGER NCHAR
!
      LOGICAL IFLAG1
!
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
!
      CHARACTER*20  IFORMT
      CHARACTER*240 ISTR
      CHARACTER*255 IATEMP
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  STEP 1: PRINT ROW LABEL IF REQUESTED
!
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
!
!     STEP 2: LOOP THROUGH THE NUMERIC FIELDS
!
!     APRIL 2009: SUPPORT THE FOLLOWING FORMATTING OPTIONS
!
!                  NUMDIG(I) > 0          => Fyy.xx FORMAT
!                  NUMDIG(I) = 0          => I12 FORMAT
!                  NUMDIG(I) = -1         => BLANK
!                  NUMDIG(I) = -2         => G15.7
!                  NUMDIG(I) = -3 to -20  => Eyy.xx
!                  NUMDIG(I) = -99        => '**'
!
      IF(NHEAD.GE.1)THEN
!
        DO 8000 I=1,NHEAD
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
              DO 7010 J=1,NCHTOT-2
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
!
        IF(NCSTR.GE.1)THEN
          IFORMT='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NCSTR
          WRITE(ICOUT,IFORMT)ISTR(1:NCSTR)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
!       STEP 3: WRITE RULE LINE IF REQUESTED
!
        IF(IFLAG1 .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO 8030 I=1,NMAX
            IATEMP(I:I)='-'
 8030     CONTINUE
          IFORMT(1:6)='(A   )'
          WRITE(IFORMT(3:5),'(I3)')NMAX
          WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAB5
      SUBROUTINE DPTAB6(IHEAD,NHEAD,CAPTN,NCAP,NMAX,IFLAG1,IFLAG2)
!
!     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
!              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED
!              TO INITIATE THE TABULAR OUTPUT.  IT WILL OPTIONALLY
!              DRAW A RULE LINE BEFORE AND/OR AFTER THE TITLE.
!              IS THE CAPTION.  THIS IS A VARIANT OF DPTAB1 (THIS
!              ROUTINE ALLOWS THE RULE LINES).
!     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
!                                THE TEXT FOR THE HEADER
!                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
!                                THE NUMBER OF CHARACTERS IN THE
!                                HEADER.
!                     --CAPTN  = THE CHARACTER STRING CONTAINING
!                                THE CAPTION.
!                     --NCAP   = THE INTEGER NUMBER THAT SPECIFIES
!                                THE NUMBER OF CHARACTERS IN THE
!                                CAPTION.
!                     --NMAX   = THE INTEGER NUMBER THAT SPECIFIES
!                                THE TOTAL NUMBER OF COLUMNS IN THE
!                                TABLE.
!                     --IFLAG1 = A LOGICAL PARAMETER THAT SPECIFIES
!                                WHETHER A RULE LINE IS DRAWN BEFORE
!                                THE TABLE HEADER.
!                     --IFLAG2 = A LOGICAL PARAMETER THAT SPECIFIES
!                                WHETHER A RULE LINE IS DRAWN AFTER
!                                THE TABLE HEADER.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABOARATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/4
!     ORIGINAL VERSION--APRIL     2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*(*) CAPTN
      CHARACTER*(*) IHEAD
!
      CHARACTER*132 IATEMP
!
      LOGICAL IFLAG1
      LOGICAL IFLAG2
      CHARACTER*10 IFORMT
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  STEP 1: WRITE A HEADER
!
  999 FORMAT(1X)
!
      IF(IFLAG1.AND.NMAX.GT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        IFORMT=' '
        DO 8010 I=1,MIN(NMAX,132)
          IATEMP(I:I)='-'
 8010   CONTINUE
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NMAX
        WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(NHEAD.GE.1)THEN
        IFORMT=' '
        IFORMT(1:9)='(12X,A  )'
        WRITE(IFORMT(7:8),'(I2)')NHEAD
        WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!  STEP 2: START TABLE AND DEFINE A CAPTION
!
      IF(NCAP.GT.0)THEN
        IFORMT=' '
        IFORMT(1:5)='(A  )'
        WRITE(IFORMT(3:4),'(I2)')NCAP
        WRITE(ICOUT,IFORMT)CAPTN(1:NCAP)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(IFLAG2.AND.NMAX.GT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        IFORMT=' '
        DO 8090 I=1,MIN(NMAX,132)
          IATEMP(I:I)='-'
 8090   CONTINUE
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NMAX
        WRITE(ICOUT,IFORMT)IATEMP(1:NMAX)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAB6
      SUBROUTINE DPTABY(IVALUE,NCHAR,AVALUE,NHEAD,ITYPE,   &
                        IFLAGA,IFLAGB,NMAX,NTOT,ICSVWR,IOUNI1,   &
                        IBUGA3,ISUBRO)
!
!     PURPOSE--THIS ROUTINE IS A UTILITY ROUTINE FOR CREATING
!              TABULAR OUTPUT IN ASCII FORMAT.  THIS ROUTINE IS USED TO
!              GENERATE A DATA ROW FOR A TABLE WHERE THE FIELDS CAN
!              BE A MIX OF CHARACTER AND NUMERIC VALUES.
!
!     INPUT  ARGUMENTS--IVALUE  = AN ARRAY OF CHARACTER STRINGS.
!                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
!                                 THE NUMBER OF CHARACTERS IN THE
!                                 CHARACTER FIELDS.
!                     --AVALUE  = A REAL ARRAY CONTAINING THE DATA
!                                 TO BE GENERATED.
!                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
!                                 THE NUMBER OF NUMERIC VALUES.
!                     --ITYPE   = A CHARACTER ARRAY THAT SPECIFIES
!                                 WHICH FIELDS ARE NUMERIC AND
!                                 WHICH ARE CHARACTER.
!                     --IFLAGA  = GENERATE A SEPARATOR LINE AFTER THE
!                                 CURRENT LINE.
!                     --IFLAGB  = GENERATE A SEPARATOR LINE BEFORE THE
!                                 CURRENT LINE.
!                     --NMAX    = NUMBER OF CHARACTERS IN RULE LINE
!                     --NTOT    = AN INTEGER ARRAY CONTAINING THE TOTAL
!                                 NUMBER OF CHARACTERS IN EACH FIELD
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABOARATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --APRIL     2015. SUPPORT FOR HORIZONTAL ALIGNMENT
!                                       FOR Fx.x AND Ix FORMATS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*(*) IVALUE(*)
      CHARACTER*4 ITYPE(*)
      CHARACTER*4 ICSVWR
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      REAL AVALUE(NHEAD)
      INTEGER NCHAR(*)
      INTEGER NTOT(*)
!
      LOGICAL IFLAGA
      LOGICAL IFLAGB
!
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
!
      CHARACTER*20  IFORMT
      CHARACTER*240 ISTR
      CHARACTER*132 IATEMP
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     STEP 1: LOOP THROUGH THE FIELDS
!
!     SUPPORT THE FOLLOWING FORMATTING OPTIONS FOR NUMERIC FIELDS
!
!           NUMDIG(I) > 0          => Fyy.xx FORMAT
!           NUMDIG(I) = 0          => I12 FORMAT
!           NUMDIG(I) = -1         => BLANK
!           NUMDIG(I) = -2         => G15.7
!           NUMDIG(I) = -3 to -20  => Eyy.xx
!           NUMDIG(I) = -99        => '**'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TABY')THEN
        WRITE(ICOUT,1001)NHEAD,NMAX
 1001   FORMAT('NHEAD,NMAX = ',2I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(NHEAD.GE.1)THEN
!
!       STEP 1: WRITE RULE LINE BEFORE CURRENT LINE IF REQUESTED
!
        IF(IFLAGB .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO 7030 I=1,NMAX
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
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TABY')THEN
            WRITE(ICOUT,7031)
 7031       FORMAT('AFTER WRITE BORDER LINE')
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
        ENDIF
!
        ISTR=' '
        NCSTR=0
        ICNT=0
!
        DO 8000 I=1,NHEAD
          ICNT=ICNT+1
          IFORMT=' '
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'TABY')THEN
            WRITE(ICOUT,8001)I,ICNT,NCSTR,ITYPE(I)
 8001       FORMAT('I,ICNT,NCSTR,ITYPE(I) = ',3I8,2X,A4)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,8002)I,AVALUE(I),NUMDIG(I)
 8002       FORMAT('I,AVALUE(I),NUMDIG(I) = ',I8,2X,G15.7,I8)
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
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
                DO 7010 J=1,NCHTOT-2
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
!
!         CHARACTER FIELDS
!
          ELSE
!
            NTEMP=NCHAR(I)
            IF(NTEMP.GT.NTOT(I))NTEMP=NTOT(I)
            NCSTR=NCSTR+1
            NCSTR3=NCSTR+NTOT(I)-1
            ISTR(NCSTR:NCSTR3)=' '
!
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
!
          IF(ICSVWR.EQ.'ON' .AND. I.LT.NHEAD)THEN
            NCSTR=NCSTR+1
            ISTR(NCSTR:NCSTR)=','
          ENDIF
!
 8000   CONTINUE
!
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
!
!       STEP 3: WRITE RULE LINE AFTER CURRENT LINE IF REQUESTED
!
        IF(IFLAGA .AND. NMAX.GT.0)THEN
          IFORMT=' '
          DO 8030 I=1,NMAX
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
!
      ENDIF
!
      RETURN
      END SUBROUTINE DPTABY
      SUBROUTINE DPTAC2(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,YLEVEL,NLEVEL,   &
                        NUMV2,ICASCT,ICTNAM,ISTANR,ISTARA,   &
                        XIDTEM,XIDTE2,XIDTE3,XIDTE4,   &
                        TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                        TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,TMP12,   &
                        XACLOW,XACUPP,XTEMP1,XTEMP2,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ISEED,ICTAMV,PSTAMV,PCTAMV,ALPHA,IQUASE,   &
                        NCRTV,MAXOBV,PTPLXI,PTPLYI,ITPLDI,ITPLUN,   &
                        ITPLNI,ITPLCD,ITPLSO,ITPLSR,ITPLSC,   &
                        ITPLRM,ITPLCM,ITPLGM,   &
                        Y,X,D,X3D,   &
                        NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN TABULATION PLOT
!
!              THIS SUPPORTS THE "CHARACTER" VARIANT.  IN THIS VARIANT, WE PLOT
!              THE VALUE OF THE STATISTIC (FOR RAW DATA, WE CAN USE THE MEAN AS
!              THE DESIRED STATISTIC).
!
!     DESCRIPTION--IN THE TABULATION PLOT, WE CROSS-TABULATE OVER
!                  1 TO 4 GROUP-ID VARIABLES (ANALAGOUS TO A
!                  FLUCTUATION PLOT).  WE DEFINE A GRID BASED ON THE
!                  THESE GROUP-ID VARIABLES.  THEN FOR THE RESPONSE
!                  VALUES CORRESPONDING TO A GIVEN SET OF THESE
!                  GROUP-ID VARIABLES, WE COMPUTE A USER-SPECIFED
!                  STATISTIC (THE DEFAULT IS THE MEAN).  THE VALUE
!                  OF THE STATISTIC IS THEN COMPARED TO SOME
!                  USER-SPECIFIED LEVELS (THESE ARE DEFINED IN THE
!                  YLEVEL VARIABLE).  A RECTANGLE IS DRAWN AND THE
!                  ATTRIBUTES (PRIMARILY FILL COLOR) ARE BASED ON
!                  THE VALUE OF THE STATISTIC RELATIVE TO YLEVEL.
!
!                  THIS PLOT IS USEFUL FOR VISUALLY IDENTIFYING
!                  AREAS WITH "HIGH" AND "LOW" VALUES OF THE
!                  STATISTIC ACROSS GROUPS.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2889
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/6
!     ORIGINAL VERSION--JUNE      2010. THIS VARIANT ADDED TO THE
!                                       TABULATION PLOT
!     UPDATED         --AUGUST    2010. ROW/COLUMN "MINMAX" OPTION
!                                       FOR TWO GROUP-ID VARIABLES CASE
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASCT
      CHARACTER*4 ISTARA
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
      CHARACTER*4 ITPLGM
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
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
      DIMENSION Y3(*)
      DIMENSION YLEVEL(*)
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
!
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
!
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
      DIMENSION TMP12(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
!
      DIMENSION ITEMP1(*)
      DIMENSION ITEMP2(*)
      DIMENSION ITEMP3(*)
      DIMENSION ITEMP4(*)
      DIMENSION ITEMP5(*)
      DIMENSION ITEMP6(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION X3D(*)
!
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
!
      COMMON/ITABC2/IADD
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTA'
      ISUBN2='C2  '
      IWRITE='OFF'
      IERROR='NO'
      IADD=0
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
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
        GO TO 9000
      ENDIF
!
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
        DO 72 I=1,N
          WRITE(ICOUT,73)I,Y1(I),Y2(I),TAG1(I),TAG2(I),TAG3(I),   &
                         TAG4(I)
   73     FORMAT('I,Y(I),Y2(I),TAG1-6(I) = ',I8,9F10.3)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
        IF(NLEVEL.GE.1)THEN
          DO 82 I=1,NLEVEL
            WRITE(ICOUT,83)I,YLEVEL(I)
   83       FORMAT('I,YLEVEL(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   82     CONTINUE
        ENDIF
      ENDIF
!
      IF(NLEVEL.GE.1)THEN
        CALL DISTIN(YLEVEL,NLEVEL,IWRITE,TEMP1,NTEMP,IBUGG3,IERROR)
        DO 110 I=1,NTEMP
          YLEVEL(I)=TEMP1(I)
  110   CONTINUE
        NLEVEL=NTEMP
        CALL SORT(YLEVEL,NLEVEL,YLEVEL)
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
!               **  FOR THE GROUP VARIABLES (TAG1, TAG2)            **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
!               **  WHICH IS AN ERROR CONDITION FOR A PLOT.         **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ITPLCD.EQ.'ON')THEN
        CALL CODE(TAG1,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
        DO 910 I=1,N
          TAG1(I)=TEMP1(I)
  910   CONTINUE
      ENDIF
      CALL DISTIN(TAG1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
!
      IF(NCRTV.GE.2)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG2,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO 920 I=1,N
            TAG2(I)=TEMP1(I)
  920     CONTINUE
        ENDIF
        CALL DISTIN(TAG2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
      ENDIF
!
      IF(NCRTV.GE.3)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG3,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO 930 I=1,N
            TAG3(I)=TEMP1(I)
  930     CONTINUE
        ENDIF
        CALL DISTIN(TAG3,N,IWRITE,XIDTE3,NUMSE3,IBUGG3,IERROR)
        CALL SORT(XIDTE3,NUMSE3,XIDTE3)
      ELSE
        NUMSE3=0
      ENDIF
!
      IF(NCRTV.GE.4)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG4,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO 940 I=1,N
            TAG4(I)=TEMP1(I)
  940     CONTINUE
        ENDIF
        CALL DISTIN(TAG4,N,IWRITE,XIDTE4,NUMSE4,IBUGG3,IERROR)
        CALL SORT(XIDTE4,NUMSE4,XIDTE4)
      ELSE
        NUMSE4=0
      ENDIF
!
      IF(NUMSE1.LT.1 .OR. NUMSE1.GT.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=1
        WRITE(ICOUT,111)ITEMP,NUMSE1
  111   FORMAT('      THE NUMBER OF SETS FOR THE GROUP ',I1,   &
               ' VARIABLE, ',I8,',')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      IS EITHER LESS THAN ONE OR GREATER THAN THE ',   &
               'NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
  115   FORMAT('      OF OBSERVATIONS, ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
      AN=REAL(N)
      ANUMS1=REAL(NUMSE1)
      ANUMS2=REAL(NUMSE2)
      ANUMS3=REAL(NUMSE3)
      ANUMS4=REAL(NUMSE4)
!
!               ***********************************************
!               **  STEP 5--                                 **
!               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
!               ***********************************************
!
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAC2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,201)NUMSE1,NUMSE2,NUMSE3,NUMSE4
  201   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMSE1.GE.1)THEN
          DO 210 I=1,NUMSE1
            WRITE(ICOUT,211)I,XIDTEM(I)
  211       FORMAT('I,XIDTEM(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  210     CONTINUE
        ENDIF
!
        IF(NUMSE2.GE.1)THEN
          DO 220 I=1,NUMSE2
            WRITE(ICOUT,221)I,XIDTE2(I)
  221       FORMAT('I,XIDTE2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  220     CONTINUE
        ENDIF
      ENDIF
!
      IWRITE='OFF'
!
      IF(NCRTV.EQ.1)THEN
        CALL DPTAP0(Y1,Y2,Y3,TAG1,N,   &
                    NUMV2,ICASCT,ISTANR,ISTARA,   &
                    XIDTEM,   &
                    NUMSE1,   &
                    TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                    XTEMP1,XTEMP2,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ALPHA,   &
                    ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                    TEMP6,TEMP7,XACLOW,XACUPP,N2,   &
                    ISUBRO,IBUGG3,IERROR)
!
!CCCC   NOW GENERATE THE PLOT COORDINATES.  SET "X3D" TO VALUE
!CCCC   OF STATISTIC FOR EACH POINT.
!
        ICNT=0
!
        DO 1001 I=1,N2
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
            DO 1003 J=2,NLEVEL
              IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                ILEVEL=J
              ENDIF
 1003       CONTINUE
          ENDIF
!
          ICNT=ICNT+1
          X(ICNT)=XCOOR1
          Y(ICNT)=YCOOR1
          X3D(ICNT)=STAT
          D(ICNT)=REAL(ILEVEL)
!
 1001   CONTINUE
!
        NPLOTP=ICNT
        NPLOTV=2
!
!       WHEN THERE ARE EXACTLY TWO CROSS-TABULATION VARIABLES, THEN
!       SUPPORT A "SORT" OPTION.  FIRST NEED TO OBTAIN ROW AND COLUMN
!       VALUES FOR THE STATISTICS.  FROM THESE, CREATE "INDEX" VARIABLES.
!
      ELSEIF(NCRTV.EQ.2)THEN
!
!       SORT THE ROWS.  FOR THIS APPLICATION, NEED A RANK.  SINCE THE
!       RANK WILL SERVE AS AN ARRAY INDEX, NEED TO CHECK FOR TIES.
!
        IF(ITPLSO.EQ.'ON' .OR. ITPLSO.EQ.'ROW')THEN
          CALL DPTAP0(Y1,Y2,Y3,TAG1,N,   &
                      NUMV2,ICASCT,ISTANR,ISTARA,   &
                      XIDTEM,   &
                      NUMSE1,   &
                      TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                      XTEMP1,XTEMP2,   &
                      ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                      DTEMP1,DTEMP2,DTEMP3,   &
                      ISEED,ALPHA,   &
                      ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                      TEMP9,TEMP7,XACLOW,XACUPP,N2,   &
                      ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TEMP9,NUMSE1,IWRITE,XIDTE3,TEMP7,ITEMP1,MAXOBV,   &
                    IBUGG3,IERROR)
          CALL DISTIN(XIDTE3,NUMSE1,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE1)THEN
            DO 1006 II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1006       CONTINUE
          ENDIF
          IF(ITPLSR.EQ.'DESC')THEN
            DO 2006 I=1,N
              IRANK=INT(XIDTE3(I)+0.1)
              IRANK2=NUMSE1 - IRANK + 1
              XIDTE3(I)=REAL(IRANK2)
 2006       CONTINUE
          ENDIF
        ELSE
          IF(ITPLSR.EQ.'DESC')THEN
            DO 3007 II=1,NUMSE1
              IVAL=NUMSE1 - II + 1
              XIDTE3(II)=XIDTEM(IVAL)
 3007       CONTINUE
          ELSE
            DO 1007 II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1007       CONTINUE
          ENDIF
        ENDIF
!
!       SORT THE COLUMNS
!
        IF(ITPLSO.EQ.'ON' .OR. ITPLSO.EQ.'COLU')THEN
          CALL DPTAP0(Y1,Y2,Y3,TAG2,N,   &
                      NUMV2,ICASCT,ISTANR,ISTARA,   &
                      XIDTE2,   &
                      NUMSE2,   &
                      TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                      XTEMP1,XTEMP2,   &
                      ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                      DTEMP1,DTEMP2,DTEMP3,   &
                      ISEED,ALPHA,   &
                      ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                      TMP10,TEMP7,XACLOW,XACUPP,N2,   &
                      ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TMP10,NUMSE2,IWRITE,XIDTE4,TEMP7,ITEMP1,MAXOBV,   &
                    IBUGG3,IERROR)
          CALL DISTIN(XIDTE4,NUMSE2,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE2)THEN
            DO 1008 II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1008       CONTINUE
          ENDIF
          IF(ITPLSC.EQ.'DESC')THEN
            DO 2008 I=1,N
              IRANK=INT(XIDTE4(I)+0.1)
              IRANK2=NUMSE2 - IRANK + 1
              XIDTE4(I)=REAL(IRANK2)
 2008       CONTINUE
          ENDIF
        ELSE
          IF(ITPLSR.EQ.'DESC')THEN
            DO 3008 II=1,NUMSE2
              IVAL=NUMSE2 - II + 1
              XIDTE4(II)=XIDTE2(IVAL)
 3008       CONTINUE
          ELSE
             DO 1009 II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1009       CONTINUE
          ENDIF
        ENDIF
!
        CALL DPTAP3(Y1,Y2,Y3,TAG1,TAG2,N,   &
                    NUMV2,ICASCT,ISTANR,ISTARA,   &
                    XIDTEM,XIDTE2,   &
                    NUMSE1,NUMSE2,   &
                    TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                    XTEMP1,XTEMP2,   &
                    TMP10,TMP11,TMP12,ITPLCM,ITPLRM,ITPLGM,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ALPHA,   &
                    ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                    TEMP6,TEMP7,TEMP8,XACLOW,XACUPP,N2,   &
                    ISUBRO,IBUGG3,IERROR)
!
!CCCC   NOW GENERATE THE PLOT COORDINATES.
!
        ICNT=0
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
          WRITE(ICOUT,1011)N2,ITPLSO,ITPLDI
 1011     FORMAT('DPTAC2 AFTER CALL DPTAP3: N2,ITPLSO,ITPLDI = ',   &
                 I8,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
          DO 1012 I=1,NUMSE1
            WRITE(ICOUT,1013)I,XIDTE3(I)
 1013       FORMAT('I,XIDTE3(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 1012     CONTINUE
          DO 1014 I=1,NUMSE2
            WRITE(ICOUT,1015)I,XIDTE4(I)
 1015       FORMAT('I,XIDTE4(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 1014     CONTINUE
        ENDIF
!
        DO 1010 I=1,N2
          STAT=TEMP6(I)
!
          IF(ITPLDI.EQ.'X')THEN
            INDEXX=INT(TEMP7(I)+0.1)
            INDEXY=INT(TEMP8(I)+0.1)
            XVAL=XIDTE3(INDEXX)
            YVAL=XIDTE4(INDEXY)
          ELSE
!CCCC       INDEXX=INT(TEMP8(I)+0.1)
!CCCC       INDEXY=INT(TEMP7(I)+0.1)
!CCCC       XVAL=XIDTE4(INDEXX)
!CCCC       YVAL=XIDTE3(INDEXY)
            INDEXX=INT(TEMP8(I)+0.1)
            INDEXY=INT(TEMP7(I)+0.1)
            XVAL=XIDTE4(INDEXX)
            YVAL=XIDTE3(INDEXY)
          ENDIF
!
          XCOOR1=XVAL
          YCOOR1=YVAL
          ILEVEL=-99
          IF(NLEVEL.GE.1)THEN
            IF(STAT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO 1016 J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1016         CONTINUE
            ENDIF
          ENDIF
!
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
            WRITE(ICOUT,1017)I,STAT,INDEXX,XVAL,XCOOR1,   &
                             INDEXY,YVAL,YCOOR1
 1017       FORMAT('I,STAT,INDEXX,XVAL,XCOOR1,INDEXY,YVAL,YCOOR1 = ',   &
                   I8,G15.7,2(I6,2F12.5))
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          ICNT=ICNT+1
          X(ICNT)=XCOOR1
          Y(ICNT)=YCOOR1
          X3D(ICNT)=STAT
          D(ICNT)=REAL(ILEVEL)
!
 1010   CONTINUE
!
!       IF REQUESTED, FLAG COLUMN/ROW/GRAND MIN/MAX POINTS
!
        IF(ITPLCM.EQ.'OFF' .AND. ITPLRM.EQ.'OFF' .AND.   &
           ITPLGM.EQ.'OFF')GO TO 5099
!
!       PERFORM DUPLICATION OF ARRAYS FIRST (ADD MIN/MAX PART
!       AT END)
!
        IF(ICNT.GT.0)THEN
          DO 5010 I=1,ICNT
            ICNT=ICNT+1
            X(ICNT)=X(I)
            Y(ICNT)=Y(I)
            X3D(ICNT)=X3D(I)
            D(ICNT)=D(I) + REAL(NLEVEL+1)
 5010     CONTINUE
        ENDIF
        CALL MAXIM(D,ICNT,IWRITE,DMAX,IBUGG3,IERROR)
!
        IF(ITPLDI.EQ.'Y')THEN
          IADD=0
!
          IF(ITPLCM.EQ.'ON')THEN
!
!           COLUMN MIN/MAX
!
            IADD=IADD+1
            DO 5020 I=1,N2
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
!
          IF(ITPLRM.EQ.'ON')THEN
!
!           ROW MIN/MAX
!
            IADD=IADD+1
            DO 5030 I=1,N2
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
!
          IF(ITPLGM.EQ.'ON')THEN
!
!           GRAND MIN/MAX
!
            IADD=IADD+1
            DO 5035 I=1,N2
              IF(TMP12(I).EQ.1.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(2*(NLEVEL+1)+IADD)
              ENDIF
              IF(TMP12(I).EQ.2.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(2*(NLEVEL+1)+IADD+1)
              ENDIF
 5035       CONTINUE
            IADD=IADD+1
          ENDIF
!
        ELSEIF(ITPLDI.EQ.'X')THEN
          IADD=0
!
          IF(ITPLRM.EQ.'ON')THEN
            IADD=IADD+1
            DO 5040 I=1,N2
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
!
          IF(ITPLCM.EQ.'ON')THEN
            IADD=IADD+1
            DO 5050 I=1,N2
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
!
          IF(ITPLGM.EQ.'ON')THEN
            IADD=IADD+1
            DO 5060 I=1,N2
              IF(TMP12(I).EQ.1.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(NLEVEL+1+IADD)
              ENDIF
              IF(TMP12(I).EQ.2.0)THEN
                ICNT=ICNT+1
                X(ICNT)=X(I)
                Y(ICNT)=Y(I)
                X3D(ICNT)=X3D(I)
                D(ICNT)=REAL(NLEVEL+1+IADD+1)
              ENDIF
 5060       CONTINUE
            IADD=IADD+1
          ENDIF
!
          NPLOTP=ICNT
          NPLOTV=2
          GO TO 9000
!
        ENDIF
!
        NPLOTP=ICNT
        NPLOTV=2
        GO TO 9000
!
 5099   CONTINUE
        NPLOTP=ICNT
        NPLOTV=2
!
      ELSEIF(NCRTV.EQ.3)THEN
        CALL DPTAP4(Y1,Y2,Y3,TAG1,TAG2,TAG3,N,   &
                    NUMV2,ICASCT,ISTANR,ISTARA,   &
                    XIDTEM,XIDTE2,XIDTE3,   &
                    NUMSE1,NUMSE2,NUMSE3,   &
                    TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                    XTEMP1,XTEMP2,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ALPHA,   &
                    ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                    TEMP6,TEMP7,TEMP8,TEMP9,XACLOW,XACUPP,N2,   &
                    ISUBRO,IBUGG3,IERROR)
!
!CCCC   NOW GENERATE THE PLOT COORDINATES.
!
        ICNT=0
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
          WRITE(ICOUT,1021)N2
 1021     FORMAT('DPTAC2: AFTER CALL DPTAP4--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        DO 1020 I=1,N2
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
            DO 1025 J=2,NLEVEL
              IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                ILEVEL=J
              ENDIF
 1025       CONTINUE
          ENDIF
!
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
!
          ICNT=ICNT+1
          X(ICNT)=XCOOR1
          Y(ICNT)=YCOOR1
          X3D(ICNT)=STAT
          D(ICNT)=REAL(ILEVEL)
!
 1020   CONTINUE
!
        NPLOTP=ICNT
        NPLOTV=2
!
      ELSEIF(NCRTV.EQ.4)THEN
        CALL DPTAP5(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,   &
                    NUMV2,ICASCT,ISTANR,ISTARA,   &
                    XIDTEM,XIDTE2,XIDTE3,XIDTE4,   &
                    NUMSE1,NUMSE2,NUMSE3,NUMSE4,   &
                    TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                    XTEMP1,XTEMP2,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ALPHA,   &
                    ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                    TEMP6,TEMP7,TEMP8,TEMP9,TMP10,XACLOW,XACUPP,N2,   &
                    ISUBRO,IBUGG3,IERROR)
!
!CCCC   NOW GENERATE THE PLOT COORDINATES.
!
        ICNT=0
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
          WRITE(ICOUT,1031)N2
 1031     FORMAT('DPTAC2: AFTER CALL DPTAP5--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        DO 1030 I=1,N2
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
!CCCC     YCOOR2=YCOOR1 + YINC2
          IF(STAT.LT.YLEVEL(1))THEN
            ILEVEL=1
          ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
            ILEVEL=NLEVEL+1
          ELSE
            DO 1035 J=2,NLEVEL
              IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                ILEVEL=J
              ENDIF
 1035       CONTINUE
          ENDIF
!
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAC2')THEN
            WRITE(ICOUT,1036)I,STAT,YVAL,XVAL,YVAL2,XVAL2
 1036       FORMAT('I,STAT,YVAL,XVAL,YVAL2,XVAL2 = ',I8,5G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1037)XCOOR1,YCOOR1,ILEVEL
 1037       FORMAT('XCOOR1,YCOOR1,ILEVEL = ',2G15.7,I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          ICNT=ICNT+1
          X(ICNT)=XCOOR1
          Y(ICNT)=YCOOR1
          X3D(ICNT)=STAT
          D(ICNT)=REAL(ILEVEL)
!
 1030   CONTINUE
!
        NPLOTP=ICNT
        NPLOTV=2
!
      ENDIF
!
!     NOW DUPLICATE ARRAYS
!
      IF(NPLOTP.GT.0)THEN
        DO 2010 I=1,NPLOTP
          NPLOTP=NPLOTP+1
          X(NPLOTP)=X(I)
          Y(NPLOTP)=Y(I)
          X3D(NPLOTP)=X3D(I)
!CCCC     D(NPLOTP)=D(I) + REAL(NLEVEL+1+IADD+1)
          D(NPLOTP)=D(I) + REAL(NLEVEL+1+IADD)
 2010   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9035 I=1,NPLOTP
          WRITE(ICOUT,9036)I,Y(I),X(I),X3D(I),D(I)
 9036     FORMAT('I,Y(I),X(I),X3D(I),D(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAC2
      SUBROUTINE DPTAIL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN (EMPIRICAL) TAIL AREA PLOT
!              (A SYNONYM IS SURVIVAL PLOT)
!              VERTICAL AXIS   = 1-F(X)  (ON A LOG10 SCALE)
!              HORIZONTAL AXIS = SORTED DATA
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
!     ORIGINAL VERSION--MAY       1989.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --APRIL     1992. MAXCP31 TO MAXCP6
!     UPDATED         --JANUARY   2012. USE DPPARS
!     UPDATED         --JANUARY   2012. SUPPORT FOR MULTIPLE AND
!                                       REPLICATION OPTIONS
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
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION ZY1(MAXOBV)
      DIMENSION XDESGN(MAXOBV,2)
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB7),ZY1(1))
      EQUIVALENCE (GARBAG(IGARB8),XDESGN(1,1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPTA'
      ISUBN2='IL  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
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
!
!
!               **********************************
!               **  TREAT THE TAIL AREA PLOT    **
!               **  =     THE SURVIVAL PLOT     **
!               **********************************
!
!               *******************************************
!               **  STEP 1--                             **
!               **  SEARCH FOR TAIL AREA PLOT            **
!               **  OR SURVIVAL PLOT                     **
!               *******************************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASPL='TAIL'
!
      IF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
        IF((IHARG(1).EQ.'TAIL' .OR. IHARG(1).EQ.'SURV') .AND.   &
           IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
        ELSEIF(IHARG(1).EQ.'TAIL' .AND. IHARG(2).EQ.'AREA' .AND.   &
           IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
        ENDIF
      ELSEIF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
        IF((IHARG(1).EQ.'TAIL' .OR. IHARG(1).EQ.'SURV') .AND.   &
           IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
        ELSEIF(IHARG(1).EQ.'TAIL' .AND. IHARG(2).EQ.'AREA' .AND.   &
           IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
        ENDIF
      ELSEIF((ICOM.EQ.'TAIL' .OR. ICOM.EQ.'SURV') .AND.   &
        IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
      ELSEIF(ICOM.EQ.'TAIL' .AND. IHARG(1).EQ.'AREA' .AND.   &
        IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
      ELSE
        GO TO 9000
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',   &
                 'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN 1 AND 2;  SUCH WAS NOT THE ',   &
                 'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
        NRESP=1
      ENDIF
!
!               ********************************************
!               **  STEP 6--                              **
!               **  GENERATE THE TAIL AREA      PLOTS FOR **
!               **  THE VARIOUS CASES.                    **
!               ********************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NREPL.EQ.0)THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
        NPLOTP=0
        DO 810 IRESP=1,NRESP
          NCURVE=IRESP
!
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SPEC')THEN
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
                      Y1,Y1,Y1,NS,NS,NS,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************************
!               **  STEP 8B--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!               *****************************************************
!
          CALL DPTAI2(Y1,NS,NCURVE,ICASPL,MAXN,   &
                      Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
  810   CONTINUE
!
!               *****************************************************
!               **  STEP 9A--                                      **
!               **  CASE 3: ONE OR TWO  REPLICATION VARIABLES.     **
!               **          FOR THIS CASE, THE NUMBER OF RESPONSE  **
!               **          VARIABLES MUST BE EXACTLY 1.           **
!               *****************************************************
!
      ELSEIF(NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAIL')   &
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
          IJ=MAXN*(ICOLR(1)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
          ICOLC=1
!
          DO 920 IR=1,MIN(NREPL,2)
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
!
  910   CONTINUE
        NLOCAL=J
!
!       *****************************************************
!       **  STEP 9B--                                      **
!       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!       **                                                 **
!       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
!       **  VARIOUS REPLICATIONS.                          **
!       *****************************************************
!
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
            DO 935 I=1,NLOCAL
              WRITE(ICOUT,936)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  936         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2) = ',I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
  935       CONTINUE
          ENDIF
        ENDIF
!
!       *****************************************************
!       **  STEP 9C--                                      **
!       **  FIND THE DISTINCT VALUES IN EACH OF THE        **
!       **  REPLICATION VARIABLES.                         **
!       *****************************************************
!
        CALL DPFRE5(XDESGN(1,1),XDESGN(1,2),   &
                   NREPL,NLOCAL,MAXOBV,   &
                   XIDTEM,XIDTE2,   &
                   XTEMP1,XTEMP2,   &
                   NUMSE1,NUMSE2,   &
                   IBUGG3,ISUBRO,IERROR)
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
            DO 1130 I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                ZY1(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTAI2(ZY1,NTEMP,NCURVE,ICASPL,MAXN,   &
                          Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO 1210 ISET1=1,NUMSE1
          DO 1220 ISET2=1,NUMSE2
            K=0
            DO 1290 I=1,NLOCAL
              IF(   &
                 XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.   &
                 XIDTE2(ISET2).EQ.XDESGN(I,2)   &
                )THEN
                K=K+1
                ZY1(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPTAI2(ZY1,NTEMP,NCURVE,ICASPL,MAXN,   &
                          Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1220     CONTINUE
 1210     CONTINUE
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9015 I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAIL
      SUBROUTINE DPTAI2(Y1,N,NCURVE,ICASPL,MAXN,   &
                        Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN (EMPIRICAL) TAIL AREA PLOT
!              (A SYNONYM IS SURVIVAL PLOT)
!              VERTICAL AXIS   = 1-F(X)  (ON A LOG10 SCALE)
!              HORIZONTAL AXIS = SORTED DATA
!     INPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED) OBSERVATIONS
!                               FOR THE FIRST  VARIABLE.
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!     CAUTION--THE INPUT VARIABLE Y1(.) WILL BE CHANGED HEREIN
!              (IT WILL BE SORTED)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/6
!     ORIGINAL VERSION--MAY       1989.
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
      DIMENSION Y1(*)
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
      ISUBN1='DPTA'
      ISUBN2='I2  '
      IERROR='NO'
!
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
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y1(I)
   56     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
!               ***********************************************
!               **  STEP 12--                                **
!               **  COMPUTE COORDINATES FOR TAIL AREA PLOT   **
!               **  (INCORPORATE STAIR-STEP APPEARANCE)      **
!               **  NOTE--THE LOGGING OF THE 1-F(X) WILL     **
!               **        NOTE BE DONE HEREIN BUT WILL       **
!               **        BE DONE IN THE UNDERLYING          **
!               **        GRAPHICS BY LOG SCALE              **
!               ***********************************************
!
!
      CALL SORT(Y1,N,Y1)
!
      ANP1=N+1
      J=0
      DO 1100 I=1,N
        ARG1=N-I+1
        ARG2=N-I
        J=J+1
        X(J+NPLOTP)=Y1(I)
        Y(J+NPLOTP)=ARG1/ANP1
        D(J+NPLOTP)=REAL(NCURVE)
        IF(I.GE.N)GO TO 1100
        J=J+1
        X(J+NPLOTP)=Y1(I)
        Y(J+NPLOTP)=ARG2/ANP1
        D(J+NPLOTP)=REAL(NCURVE)
 1100 CONTINUE
      NPLOTP=NPLOTP+J
      NPLOTV=2
      GO TO 9000
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTAI2--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N
          WRITE(ICOUT,9016)I,Y1(I)
 9016     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9021)NPLOTP,NPLOTV,IERROR
 9021   FORMAT('NPLOTP,NPLOTV,IERROR = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 9022 I=1,NPLOTP
          WRITE(ICOUT,9023)I,Y(I),X(I),D(I)
 9023     FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9022   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAI2
      SUBROUTINE DPTAP0(Y,Z,Z2,TAG1,N,   &
                        NUMV2,ICASCT,ISTANR,ISTARA,   &
                        XIDTEM,   &
                        NUMSE1,   &
                        TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                        XTEMP4,XTEMP5,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ISEED,ALPHA,   &
                        ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                        Y2,X2,XACLOW,XACUPP,N2,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--GENERATE A ONE-WAY TABULATION PLOT.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --DECEMBER  2009. UNCERTAINTY OPTION FOR
!                                       BINOMIAL PROBABILITY, MEAN AND
!                                       MEDIAN CONFIDENCE INTERVAL
!     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
!                                       FOR BINOMIAL RATIO
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASCT
      CHARACTER*4 ISTARA
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
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
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
!
      DIMENSION TAG1(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION XTEMP4(*)
      DIMENSION XTEMP5(*)
!
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
!
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTA'
      ISUBN2='P0  '
      IWRITE='OFF'
!
      I2=0
!
      AN=INT(N+0.01)
!
!               ***********************************************
!               **  STEP 5--                                 **
!               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
!               ***********************************************
!
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      NRESP=NUMV2-1
      DO 1110 ISET1=1,NUMSE1
!
        K=0
        DO 1130 I=1,N
          IF(XIDTEM(ISET1).EQ.TAG1(I))GO TO 1131
          GO TO 1130
 1131     CONTINUE
!
          K=K+1
          TEMP(K)=0.0
          TEMPZ(K)=0.0
          TEMPZ2(K)=0.0
          IF(ISTANR.GE.1)TEMP(K)=Y(I)
          IF(ISTANR.GE.2)TEMPZ(K)=Z(I)
          IF(ISTANR.GE.3)TEMPZ2(K)=Z2(I)
 1130   CONTINUE
        NTEMP=K
!
        NTRIAL=0
        ALOWLM=0.0
        AUPPLM=0.0
        IF(NTEMP.EQ.0)THEN
          IF(ICTAMV.EQ.'ZERO')THEN
            STAT=0.0
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
               ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
              NTRIAL=0
              ALOWLM=0.0
              AUPPLM=0.0
            ENDIF
          ELSEIF(ICTAMV.EQ.'MV  ')THEN
            STAT=PCTAMV
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
               ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
              NTRIAL=0
              ALOWLM=PCTAMV
              AUPPLM=PCTAMV
            ENDIF
          ELSE
            GO TO 1110
          ENDIF
        ELSE
          CALL CMPSTA(   &
                    TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                    XTEMP4,XTEMP5,   &
                    MAXNXT,NTEMP,NTEMP,NTEMP,   &
                    NRESP,ICASCT,ISTARA,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    STAT,   &
                    ISUBRO,IBUGG3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
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
              CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,   &
                          ALOWLM,AUPPLM,IBUGG3,IERROR)
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
              CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,   &
                          ALOWLM,AUPPLM,IBUGG3,IERROR)
            ENDIF
          ELSEIF(ICASCT.EQ.'MDCL')THEN
            XMED=STAT
            NTRIAL=NTEMP
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              XQ=0.5
              CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,   &
                          QUASE,IBUGG3,IERROR)
              ALPHAT=ALPHA
              CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,   &
                          ALOWLM,AUPPLM,IBUGG3,IERROR)
            ENDIF
          ENDIF
        ENDIF
!
        J=J+1
        Y2(J)=STAT
        X2(J)=XIDTEM(ISET1)
        IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
          IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
          IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
          XACLOW(J)=ALOWLM
          XACUPP(J)=AUPPLM
        ENDIF
!
 1110 CONTINUE
      N2=J
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9020 I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I)
 9021     FORMAT('I,Y2(I),X2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAP0
      SUBROUTINE DPTAPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A TABULATION PLOT.
!
!              THIS IS SOMEWHAT SIMILAR TO A FLUCTUATION PLOT.
!              HOWEVER, INSTEAD OF A FILLED BAR BASED ON THE
!              VALUE OF A STATISTIC, WE COLOR CODE BASED ON
!              THE LEVEL OF THE RESPONSE VARIABLE (I.E.,
!              LIKE SPECIFYING THE LEVELS IN A CONTOUR PLOT).
!              WE CURRENTLY SUPPORT THIS PLOT FOR ONE-WAY THROUGH
!              FOUR-WAY TABLES.
!
!                  X1  = CATEGORY LEVEL FOR VARIABLE 1
!                  X2  = CATEGORY LEVEL FOR VARIABLE 2
!                  X3  = CATEGORY LEVEL FOR VARIABLE 3
!                  X4  = CATEGORY LEVEL FOR VARIABLE 4
!
!              NOTE THAT WE EXTENED THE TABULATION PLOT TO ALLOW
!              ANY OF DATAPLOT'S SUPPORTED STATISTICS TO BE
!              PLOTTED (THE DEFAULT IS THE MEAN).
!
!     EXAMPLES--TABULATION PLOT Y X1 X2 ZLEVEL
!             --TABULATION PLOT Y X1 X2 X3 ZLEVEL
!             --TABULATION PLOT Y X1 X2 X3 X4 ZLEVEL
!             --TABULATION PLOT TABLE ZLEVEL
!             --MEAN TABULATION PLOT Y X1 X2 ZLEVEL
!             --SD TABULATION PLOT Y X1 X2 ZLEVEL
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --JUNE      2010. ADD "CHARACTER TABULATION PLOT"
!                                       CASE.  THIS IS A VARIANT THAT
!                                       PLOTS THE NUMERICAL VALUE OF THE
!                                       STATISTIC RATHER THAN A COLORED
!                                       RECTANGLE
!     UPDATED         --SEPTEMBER 2016. SUPPORT FOR MATRIX ARGUMENTS
!     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
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
      CHARACTER*4 ICASCT
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISTADF
      CHARACTER*4 ISTARA
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      PARAMETER (MAXSPN=20)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
      CHARACTER*40 INAME
!
      CHARACTER*8 IYNAM
      CHARACTER*8 IXNAM
      CHARACTER*8 IX1NAM
      CHARACTER*8 IX2NAM
      CHARACTER*8 IX3NAM
      CHARACTER*8 IX4NAM
      CHARACTER*60 ICTNAM
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION YLEVEL(MAXOBV)
!
      DIMENSION XH1DIS(MAXOBV)
      DIMENSION XH2DIS(MAXOBV)
      DIMENSION XH3DIS(MAXOBV)
      DIMENSION XH4DIS(MAXOBV)
!
      DIMENSION X1(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION X4(MAXOBV)
!
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
      DIMENSION TMP12(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
!
      DIMENSION XACLOW(MAXOBV)
      DIMENSION XACUPP(MAXOBV)
!
      DIMENSION ITEMP1(MAXOBV)
      DIMENSION ITEMP2(MAXOBV)
      DIMENSION ITEMP3(MAXOBV)
      DIMENSION ITEMP4(MAXOBV)
      DIMENSION ITEMP5(MAXOBV)
      DIMENSION ITEMP6(MAXOBV)
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
!
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
      EQUIVALENCE (GARBAG(IGAR16),TMP12(1))
      EQUIVALENCE (GARBAG(IGAR17),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGAR18),XTEMP2(1))
!
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
!
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPTA'
      ISUBN2='PL  '
!
      IYNAM=' '
      IXNAM=' '
      IX1NAM=' '
      IX2NAM=' '
      IX3NAM=' '
      IX4NAM=' '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      MAXV2=7
      MINN2=2
      J2=0
!
!               ****************************************
!               **  TREAT THE TABULATION PLOT CASE    **
!               ****************************************
!
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
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ****************************************************
!               **  STEP 1.5--                                    **
!               **  SEARCH FOR TABULATION <STAT> PLOT             **
!               **  SEARCH FOR CHARACTER TABULATION <STAT> PLOT   **
!               ****************************************************
!
      ICASCT=' '
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(ICOM.EQ.'TABU')THEN
        ICASPL='TABU'
        JMIN=1
      ELSEIF(ICOM.EQ.'CHAR' .AND. IHARG(1).EQ.'TABU')THEN
        ICASPL='TABC'
        JMIN=2
      ELSE
        GO TO 9000
      ENDIF
!
!CCCC USE "EXTSTA" TO PARSE.  NOTE THAT IF NO STATISTIC IS GIVEN,
!CCCC  WE ASSUME THE "MEAN" CASE.
!
      JMAX=MIN(NUMARG,JMIN+6)
      DO 200 I=JMIN,JMAX
        IF(IHARG(I).EQ.'PLOT')THEN
          JMAX=I-1
          ILASTC=I
          GO TO 209
        ENDIF
  200 CONTINUE
      IFOUND='NO'
      GO TO 9000
  209 CONTINUE
!
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,   &
                  ICASCT,ICTNAM,ISTANR,ISTADF,ISTARA,   &
                  IFOUND,ILOCV,ISUBRO,IBUGG3,IERROR)
!
      IF(IFOUND.EQ.'NO')THEN
        ICTNAM='NUMBER'
        ILOCV=2
        IFOUND='YES'
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     2016/09: ALLOW MATRIX ARGUMENTS
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXVAR,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 255 I=1,NUMVAR
            WRITE(ICOUT,257)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  257       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  255     CONTINUE
        ENDIF
      ENDIF
!
!     IF MATRIX ARGUMENTS GIVEN, THEN ALL RESPONSES MUST BE MATRICES
!     AND ALL MATRICES MUST HAVE SAME DIMENSION.
!
      IFLAGM=0
      DO 260 I=1,NUMVAR
        IF(IVARTY(I).EQ.'MATR')IFLAGM=1
  260 CONTINUE
!
      IF(IFLAGM.EQ.1)THEN
!
        NRESP=ISTANR
        NLVARI=1
        IF(ICASPL.EQ.'TABC')NLVARI=0
        NCRTV=2
!
        DO 291 I=1,NRESP
          IF(IVARTY(I).NE.'MATR')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,311)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,292)
  292       FORMAT('      IF ONE RESPONSE VARIABLE IS A MATRIX, ',   &
                   'THEN ALL MUST BE MATRICES.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,293)I
  293       FORMAT('      RESPONSE VARIABLE ',I5,' IS NOT A MATRIX.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
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
  296            FORMAT('      FOR MATRIX RESPONSE VARIABLES, THE ',   &
                        'ROW AND COLUMN DIMENSIONS MUST BE EQUAL.')
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,297)NROW,NCOL
  297            FORMAT('      THE FIRST MATRIX HAS ',I5,' ROWS AND ',   &
                        I5,' COLUMNS.')
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,298)I,NRTEMP,NCTEMP
  298            FORMAT('      MATRIX ',I2,' HAS ',I5,' ROWS AND ',   &
                        I5,' COLUMNS.')
                 CALL DPWRST('XXX','BUG ')
                 IERROR='YES'
                 GO TO 9000
               ENDIF
             ENDIF
          ENDIF
  291   CONTINUE
!
        NTEMP=NRESP + NLVARI
        IF(NTEMP.NE.NUMVAR)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,311)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,272)
  272     FORMAT('      WHEN MATRIX ARGUMENTS ARE GIVEN, THE ',   &
                 'NUMBER OF MATRICES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,274)
  274     FORMAT('      MUST BE THE SAME AS THE NUMBER OF RESPONSE ',   &
                 'VARIABLES FOR THE SELECTED STATISTIC.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,276)NUMVAR-NLVARI
  276     FORMAT('      THE NUMBER OF MATRICES ENTERED = ',I5)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,278)ISTANR
  278     FORMAT('      THE NUMBER OF MATRICES EXPECTED = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        GO TO 400
!
      ENDIF
!
!               ******************************************************
!               **  STEP 3--                                        **
!               **  CHECK FOR ALLOWABLE NUMBER OF CROSS TABULATION  **
!               **  VARIABLES.                                      **
!               ******************************************************
!
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     FOR "CHARACTER TABULATION" CASE, THE "LEVELS" VARIABLE IS OPTIONAL.
!     IF LAST VARIABLE HAS SAME NUMBER OF OBSERVATIONS AS FIRST VARIABLE,
!     ASSUME NO "LEVEL" VARIABLE GIVEN.
!
      NRESP=ISTANR
      NLVARI=1
      IF(ICASPL.EQ.'TABC' .AND. NRIGHT(1).EQ.NRIGHT(NUMVAR)) NLVARI=0
      NCRTV=NUMVAR - NRESP - NLVARI
!
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
  314   FORMAT('      THE SPECIFIED NUMBER OF CROSS TABULATION ',   &
               'VARIABLES WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,318)(IANS(I),I=1,MIN(80,IWIDTH))
  318     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 4--                                        **
!               **  CREATE THE VARIABLES                            **
!               ******************************************************
!
  400 CONTINUE
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFLAGM.EQ.1)THEN
        ICOL=1
        CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,X1,X2,NLOCAL,   &
                    IBUGG2,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(NRESP.GE.2)THEN
          ICOL=2
          CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y2,X3,X4,N2,   &
                      IBUGG2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(NRESP.GE.3)THEN
          ICOL=3
          CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y3,X3,X4,N3,   &
                      IBUGG2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
        ENDIF
!
        GO TO 499
      ENDIF
!
      J=0
      IMAX=NRIGHT(1)
      IF(NQ.LT.NRIGHT(1))IMAX=NQ
      DO 410 I=1,IMAX
        IF(ISUB(I).EQ.0)GO TO 410
        J=J+1
!
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
!
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
!
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
!
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
!
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
!
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
!
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
!
  410 CONTINUE
      NLOCAL=J
!
  499 CONTINUE
!
      IF(NLVARI.GE.1)THEN
        J2=0
        IMAX=NRIGHT(NUMVAR)
        DO 490 I=1,IMAX
          J2=J2+1
!
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
!
!               *************************************
!               **  STEP 5--                       **
!               **  GENERATE THE TABULATION PLOT   **
!               *************************************
!
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'TAPL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,6001)NLOCAL,NLEVEL,ICASPL
 6001   FORMAT('NLOCAL,NLEVEL,ICASPL=',2I8,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
         ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
        IHP='ALPH'
        IHP2='A   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,   &
                    NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,   &
                    ILOCP,IERROR)
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
!
      IF(ICASPL.EQ.'TABU')THEN
        CALL DPTAP2(Y1,Y2,Y3,X1,X2,X3,X4,NLOCAL,YLEVEL,NLEVEL,   &
                    NUMVAR,ICASCT,ICTNAM,ISTANR,ISTARA,   &
                    XH1DIS,XH2DIS,XH3DIS,XH4DIS,   &
                    TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                    TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,TMP12,   &
                    XACLOW,XACUPP,XTEMP1,XTEMP2,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ICTAMV,PSTAMV,PCTAMV,ALPHA,IQUASE,   &
                    NCRTV,MAXOBV,PTPLXI,PTPLYI,ITPLDI,ITPLUN,   &
                    ITPLNI,ITPLCD,   &
                    ITPLSO,ITPLSR,ITPLSC,   &
                    ITPLRM,ITPLCM,ITPLGM,   &
                    Y,X,D,DCOLOR,   &
                    NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
      ELSE
        CALL DPTAC2(Y1,Y2,Y3,X1,X2,X3,X4,NLOCAL,YLEVEL,NLEVEL,   &
                    NUMVAR,ICASCT,ICTNAM,ISTANR,ISTARA,   &
                    XH1DIS,XH2DIS,XH3DIS,XH4DIS,   &
                    TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                    TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,TMP12,   &
                    XACLOW,XACUPP,XTEMP1,XTEMP2,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ICTAMV,PSTAMV,PCTAMV,ALPHA,IQUASE,   &
                    NCRTV,MAXOBV,PTPLXI,PTPLYI,ITPLDI,ITPLUN,   &
                    ITPLNI,ITPLCD,ITPLSO,ITPLSR,ITPLSC,   &
                    ITPLRM,ITPLCM,ITPLGM,   &
                    Y,X,D,X3D,   &
                    NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!       2020/07: ROUND VALUES IN X3D IF REQUESTED.
!
        IF(ITBCDI.GE.1 .AND. ITBCDI.LE.9)THEN
          DO 7010 I=1,NPLOTP
            AVAL=RND(X3D(I),ITBCDI)
            X3D(I)=AVAL
 7010     CONTINUE
        ENDIF
!
      ENDIF
!
!               *****************
!               **  STEP 9--   **
!               **  EXIT       **
!               *****************
!
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
      END SUBROUTINE DPTAPL
      SUBROUTINE DPTAP2(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,YLEVEL,NLEVEL,   &
                        NUMV2,ICASCT,ICTNAM,ISTANR,ISTARA,   &
                        XIDTEM,XIDTE2,XIDTE3,XIDTE4,   &
                        TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,   &
                        TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,TMP12,   &
                        XACLOW,XACUPP,XTEMP1,XTEMP2,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ISEED,ICTAMV,PSTAMV,PCTAMV,ALPHA,IQUASE,   &
                        NCRTV,MAXOBV,PTPLXI,PTPLYI,ITPLDI,ITPLUN,   &
                        ITPLNI,ITPLCD,ITPLSO,ITPLSR,ITPLSC,   &
                        ITPLRM,ITPLCM,ITPLGM,   &
                        Y,X,D,DCOLOR,   &
                        NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN TABULATION PLOT
!     DESCRIPTION--IN THE TABULATION PLOT, WE CROSS-TABULATE OVER
!                  1 TO 4 GROUP-ID VARIABLES (ANALAGOUS TO A
!                  FLUCTUATION PLOT).  WE DEFINE A GRID BASED ON THE
!                  THESE GROUP-ID VARIABLES.  THEN FOR THE RESPONSE
!                  VALUES CORRESPONDING TO A GIVEN SET OF THESE
!                  GROUP-ID VARIABLES, WE COMPUTE A USER-SPECIFED
!                  STATISTIC (THE DEFAULT IS THE MEAN).  THE VALUE
!                  OF THE STATISTIC IS THEN COMPARED TO SOME
!                  USER-SPECIFIED LEVELS (THESE ARE DEFINED IN THE
!                  YLEVEL VARIABLE).  A RECTANGLE IS DRAWN AND THE
!                  ATTRIBUTES (PRIMARILY FILL COLOR) ARE BASED ON
!                  THE VALUE OF THE STATISTIC RELATIVE TO YLEVEL.
!
!                  THIS PLOT IS USEFUL FOR VISUALLY IDENTIFYING
!                  AREAS WITH "HIGH" AND "LOW" VALUES OF THE
!                  STATISTIC ACROSS GROUPS.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2889
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --DECEMBER  2009. SUPPORT FOR "UNCERTAINTY" OPTION
!                                       FOR BINOMIAL PROBABILITIES
!     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
!                                       FOR BINOMIAL RATIO
!     UPDATED         --JANUARY   2010. OPTION TO LEAVE AXIS VARIABLES
!                                       UNCODED
!     UPDATED         --JUNE      2010. SUPPORT FOR "SORTED" OPTION FOR
!                                       THE TWO GROUP-ID VARIABLE CASE
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASCT
      CHARACTER*4 ISTARA
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
      CHARACTER*4 ITPLGM
      CHARACTER*4 IBUGG3
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
      DIMENSION Y3(*)
      DIMENSION YLEVEL(*)
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
!
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
!
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
      DIMENSION TMP12(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
!
      DIMENSION ITEMP1(*)
      DIMENSION ITEMP2(*)
      DIMENSION ITEMP3(*)
      DIMENSION ITEMP4(*)
      DIMENSION ITEMP5(*)
      DIMENSION ITEMP6(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION DCOLOR(*)
!
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTA'
      ISUBN2='P2  '
      IWRITE='OFF'
      IERROR='NO'
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
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
        GO TO 9000
      ENDIF
!
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
        DO 72 I=1,N
          WRITE(ICOUT,73)I,Y1(I),Y2(I),TAG1(I),TAG2(I),TAG3(I),   &
                         TAG4(I)
   73     FORMAT('I,Y(I),Y2(I),TAG1-6(I) = ',I8,9F10.3)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
        DO 82 I=1,NLEVEL
          WRITE(ICOUT,83)I,YLEVEL(I)
   83     FORMAT('I,YLEVEL(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   82   CONTINUE
      ENDIF
!
      CALL DISTIN(YLEVEL,NLEVEL,IWRITE,TEMP1,NTEMP,IBUGG3,IERROR)
      DO 110 I=1,NTEMP
        YLEVEL(I)=TEMP1(I)
  110 CONTINUE
      NLEVEL=NTEMP
      CALL SORT(YLEVEL,NLEVEL,YLEVEL)
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
!               **  FOR THE GROUP VARIABLES (TAG1, TAG2)            **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
!               **  WHICH IS AN ERROR CONDITION FOR A PLOT.         **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ITPLCD.EQ.'ON')THEN
        CALL CODE(TAG1,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
        DO 910 I=1,N
          TAG1(I)=TEMP1(I)
  910   CONTINUE
      ENDIF
      CALL DISTIN(TAG1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
!
      IF(NCRTV.GE.2)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG2,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO 920 I=1,N
            TAG2(I)=TEMP1(I)
  920     CONTINUE
        ENDIF
        CALL DISTIN(TAG2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
      ENDIF
!
      IF(NCRTV.GE.3)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG3,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO 930 I=1,N
            TAG3(I)=TEMP1(I)
  930     CONTINUE
        ENDIF
        CALL DISTIN(TAG3,N,IWRITE,XIDTE3,NUMSE3,IBUGG3,IERROR)
        CALL SORT(XIDTE3,NUMSE3,XIDTE3)
      ELSE
        NUMSE3=0
      ENDIF
!
      IF(NCRTV.GE.4)THEN
        IF(ITPLCD.EQ.'ON')THEN
          CALL CODE(TAG4,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO 940 I=1,N
            TAG4(I)=TEMP1(I)
  940     CONTINUE
        ENDIF
        CALL DISTIN(TAG4,N,IWRITE,XIDTE4,NUMSE4,IBUGG3,IERROR)
        CALL SORT(XIDTE4,NUMSE4,XIDTE4)
      ELSE
        NUMSE4=0
      ENDIF
!
      IF(NUMSE1.LT.1 .OR. NUMSE1.GT.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=1
        WRITE(ICOUT,111)ITEMP,NUMSE1
  111   FORMAT('      THE NUMBER OF SETS FOR THE GROUP ',I1,   &
               ' VARIABLE, ',I8,',')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      IS EITHER LESS THAN ONE OR GREATER THAN THE ',   &
               'NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
  115   FORMAT('      OF OBSERVATIONS, ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
!
      AN=REAL(N)
      ANUMS1=REAL(NUMSE1)
      ANUMS2=REAL(NUMSE2)
      ANUMS3=REAL(NUMSE3)
      ANUMS4=REAL(NUMSE4)
!
!     FOR THE BINOMIAL PROPORTION, MEAN CONFIDENCE LIMIT, AND
!     MEDIAN CONFIDENCE LIMIT, INSTEAD OF A SINGLE SHADED RECTANGLE,
!     DEFINE "ITPLNI" INTERVALS THAT WILL BE SHADED FROM LOWEST
!     CONFIDENCE VALUE TO HIGHEST CONFIDENCE VALUE.
!
      IFLAGU=0
      IF((ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
          ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT') .AND.   &
          ITPLUN.EQ.'ON')THEN
        IFLAGU=1
      ENDIF
!
!               ***********************************************
!               **  STEP 5--                                 **
!               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
!               ***********************************************
!
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
!
      IF(NCRTV.EQ.1)THEN
        CALL DPTAP0(Y1,Y2,Y3,TAG1,N,   &
                    NUMV2,ICASCT,ISTANR,ISTARA,   &
                    XIDTEM,   &
                    NUMSE1,   &
                    TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                    XTEMP1,XTEMP2,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ALPHA,   &
                    ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                    TEMP6,TEMP7,XACLOW,XACUPP,N2,   &
                    ISUBRO,IBUGG3,IERROR)
!
!CCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE A RECTANGLE
!CCCC   FOR EACH POINT.
!CCCC
!
        XINC=0.5 - PTPLXI
        YINC=0.5 - PTPLYI
        ICNT=0
        ICNT2=0
!
        IF(IFLAGU.EQ.1)THEN
          DO 2000 I=1,N2
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
!
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
!
!           DIVIDE RECTANGLE INTO "ITPLNI" VERTICAL INCREMENTS AND
!           COMPUTE LEVEL-COLOR INDEPENDENTLY FOR EACH OF THESE
!           MINI-RECTANGLES.
!
            STATIN=(STATMX - STATMN)/REAL(ITPLNI)
            STATZ=STATMN - STATIN
            AINC=(YCOOR2 - YCOOR1)/REAL(ITPLNI)
            YCZ2=YCOOR1
!
            DO 2009 IROW=1,ITPLNI
!
              YCZ1=YCZ2
              YCZ2=YCZ1 + AINC
!
              STATZ=STATZ + STATIN
              IF(STATZ.LT.YLEVEL(1))THEN
                ILEVEL=1
              ELSEIF(STATZ.GE.YLEVEL(NLEVEL))THEN
                ILEVEL=NLEVEL+1
              ELSE
                DO 2005 J=2,NLEVEL
                  IF(STATZ.GE.YLEVEL(J-1) .AND. STATZ.LT.YLEVEL(J))THEN
                    ILEVEL=J
                  ENDIF
 2005           CONTINUE
              ENDIF
!
              IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
                WRITE(ICOUT,2006)I,IROW,STAT,STATZ,STATMN,STATMX,STATIN
 2006           FORMAT('I,IROW,STAT,STATZ,STATMN,STATMX,STATIN = ',   &
                       2I8,5G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2007)XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2
 2007           FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2 = ',   &
                       6G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2008)IROW,ILEVEL
 2008           FORMAT('IROW,ILEVEL = ',2I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
 2009       CONTINUE
!
 2000     CONTINUE
        ELSE
          DO 1001 I=1,N2
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
              DO 1005 J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1005       CONTINUE
            ENDIF
!
            ICNT2=ICNT2+1
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR2
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR2
            Y(ICNT)=YCOOR2
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR2
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
 1001     CONTINUE
       ENDIF
!
        NPLOTP=ICNT
        NPLOTV=2
!
!       WHEN THERE ARE EXACTLY TWO CROSS-TABULATION VARIABLES, THEN
!       SUPPORT A "SORT" OPTION.  FIRST NEED TO OBTAIN ROW AND COLUMN
!       VALUES FOR THE STATISTICS.  FROM THESE, CREATE "INDEX" VARIABLES.
!
      ELSEIF(NCRTV.EQ.2)THEN
!
!       SORT THE ROWS.  FOR THIS APPLICATION, NEED A RANK.  SINCE THE
!       RANK WILL SERVE AS AN ARRAY INDEX, NEED TO CHECK FOR TIES.
!
        IF(ITPLSO.EQ.'ON' .OR. ITPLSO.EQ.'ROW')THEN
          CALL DPTAP0(Y1,Y2,Y3,TAG1,N,   &
                      NUMV2,ICASCT,ISTANR,ISTARA,   &
                      XIDTEM,   &
                      NUMSE1,   &
                      TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                      XTEMP1,XTEMP2,   &
                      ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                      DTEMP1,DTEMP2,DTEMP3,   &
                      ISEED,ALPHA,   &
                      ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                      TEMP9,TEMP7,XACLOW,XACUPP,N2,   &
                      ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TEMP9,NUMSE1,IWRITE,XIDTE3,TEMP7,ITEMP1,MAXOBV,   &
                    IBUGG3,IERROR)
          CALL DISTIN(XIDTE3,NUMSE1,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE1)THEN
            DO 1006 II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1006       CONTINUE
          ENDIF
          IF(ITPLSR.EQ.'DESC')THEN
            DO 4006 I=1,N
              IRANK=INT(XIDTE3(I)+0.1)
              IRANK2=NUMSE1 - IRANK + 1
              XIDTE3(I)=REAL(IRANK2)
 4006       CONTINUE
          ENDIF
        ELSE
          IF(ITPLSR.EQ.'DESC')THEN
            DO 4007 II=1,NUMSE1
              IVAL=NUMSE1 - II + 1
              XIDTE3(II)=XIDTEM(IVAL)
 4007       CONTINUE
          ELSE
            DO 1007 II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1007       CONTINUE
          ENDIF
        ENDIF
!
!       SORT THE COLUMNS
!
        IF(ITPLSO.EQ.'ON' .OR. ITPLSO.EQ.'COLU')THEN
          CALL DPTAP0(Y1,Y2,Y3,TAG2,N,   &
                      NUMV2,ICASCT,ISTANR,ISTARA,   &
                      XIDTE2,   &
                      NUMSE2,   &
                      TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                      XTEMP1,XTEMP2,   &
                      ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                      DTEMP1,DTEMP2,DTEMP3,   &
                      ISEED,ALPHA,   &
                      ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                      TMP10,TEMP7,XACLOW,XACUPP,N2,   &
                      ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TMP10,NUMSE2,IWRITE,XIDTE4,TEMP7,ITEMP1,MAXOBV,   &
                    IBUGG3,IERROR)
          CALL DISTIN(XIDTE4,NUMSE2,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE2)THEN
            DO 1008 II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1008       CONTINUE
          ENDIF
          IF(ITPLSC.EQ.'DESC')THEN
            DO 4008 I=1,N
              IRANK=INT(XIDTE4(I)+0.1)
              IRANK2=NUMSE2 - IRANK + 1
              XIDTE4(I)=REAL(IRANK2)
 4008       CONTINUE
          ENDIF
        ELSE
          IF(ITPLSR.EQ.'DESC')THEN
            DO 5008 II=1,NUMSE2
              IVAL=NUMSE2 - II + 1
              XIDTE4(II)=XIDTE2(IVAL)
 5008       CONTINUE
          ELSE
             DO 1009 II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1009       CONTINUE
          ENDIF
        ENDIF
!
        CALL DPTAP3(Y1,Y2,Y3,TAG1,TAG2,N,   &
                    NUMV2,ICASCT,ISTANR,ISTARA,   &
                    XIDTEM,XIDTE2,   &
                    NUMSE1,NUMSE2,   &
                    TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                    XTEMP1,XTEMP2,   &
                    TMP10,TMP11,TMP12,ITPLRM,ITPLCM,ITPLGM,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ALPHA,   &
                    ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                    TEMP6,TEMP7,TEMP8,XACLOW,XACUPP,N2,   &
                    ISUBRO,IBUGG3,IERROR)
!
!CCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE A RECTANGLE
!CCCC   FOR EACH POINT.
!
        ICNT=0
        ICNT2=0
        XINC=0.5 - PTPLXI
        YINC=0.5 - PTPLYI
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
          WRITE(ICOUT,1011)N2
 1011     FORMAT('DPTAP2: AFTER CALL DPTAP3--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1012)XINC,YINC
 1012     FORMAT('XINC,YINC = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IFLAGU.EQ.1)THEN
          DO 2010 I=1,N2
            STAT=TEMP6(I)
            STATMN=XACLOW(I)
            STATMX=XACUPP(I)
!CCCC       JUNE 2010: MODIFIED TO ACCOUNT FOR SORTING
!CCCC       IF(ITPLDI.EQ.'X')THEN
!CCCC         XVAL=TEMP7(I)
!CCCC         YVAL=TEMP8(I)
!CCCC       ELSE
!CCCC         YVAL=TEMP7(I)
!CCCC         XVAL=TEMP8(I)
!CCCC       ENDIF
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
!
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
!
!           DIVIDE RECTANGLE INTO "ITPLNI" VERTICAL INCREMENTS AND
!           COMPUTE LEVEL-COLOR INDEPENDENTLY FOR EACH OF THESE
!           MINI-RECTANGLES.
!
            STATIN=(STATMX - STATMN)/REAL(ITPLNI)
            STATZ=STATMN - STATIN
            AINC=(YCOOR2 - YCOOR1)/REAL(ITPLNI)
            YCZ2=YCOOR1
!
            DO 2019 IROW=1,ITPLNI
!
              YCZ1=YCZ2
              YCZ2=YCZ1 + AINC
!
              STATZ=STATZ + STATIN
              IF(STATZ.LT.YLEVEL(1))THEN
                ILEVEL=1
              ELSEIF(STATZ.GE.YLEVEL(NLEVEL))THEN
                ILEVEL=NLEVEL+1
              ELSE
                DO 2015 J=2,NLEVEL
                  IF(STATZ.GE.YLEVEL(J-1) .AND. STATZ.LT.YLEVEL(J))THEN
                    ILEVEL=J
                  ENDIF
 2015           CONTINUE
              ENDIF
!
              IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
                WRITE(ICOUT,2016)I,IROW,STAT,STATZ,STATMN,STATMX,STATIN
 2016           FORMAT('I,IROW,STAT,STATZ,STATMN,STATMX,STATIN = ',   &
                       2I8,5G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2017)XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2
 2017           FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2 = ',   &
                       6G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2018)IROW,ILEVEL
 2018           FORMAT('IROW,ILEVEL = ',2I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
 2019       CONTINUE
!
 2010     CONTINUE
        ELSE
          DO 1010 I=1,N2
            STAT=TEMP6(I)
!CCCC       JUNE 2010: ACCOUNT FOR SORTING
!CCCC       IF(ITPLDI.EQ.'X')THEN
!CCCC         XVAL=TEMP7(I)
!CCCC         YVAL=TEMP8(I)
!CCCC       ELSE
!CCCC         YVAL=TEMP7(I)
!CCCC         XVAL=TEMP8(I)
!CCCC       ENDIF
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
!
!
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
            IF(STAT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO 1015 J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1015         CONTINUE
            ENDIF
!
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
!
            ICNT2=ICNT2+1
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR2
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR2
            Y(ICNT)=YCOOR2
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR2
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
 1010     CONTINUE
        ENDIF
!
        NPLOTP=ICNT
        NPLOTV=2
!
      ELSEIF(NCRTV.EQ.3)THEN
        CALL DPTAP4(Y1,Y2,Y3,TAG1,TAG2,TAG3,N,   &
                    NUMV2,ICASCT,ISTANR,ISTARA,   &
                    XIDTEM,XIDTE2,XIDTE3,   &
                    NUMSE1,NUMSE2,NUMSE3,   &
                    TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                    XTEMP1,XTEMP2,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ALPHA,   &
                    ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                    TEMP6,TEMP7,TEMP8,TEMP9,XACLOW,XACUPP,N2,   &
                    ISUBRO,IBUGG3,IERROR)
!
!CCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE A RECTANGLE
!CCCC   FOR EACH POINT.
!
        ICNT=0
        ICNT2=0
        XINC=0.5 - PTPLXI
        YINC=0.5 - PTPLYI
        YINC2=2.0*YINC/REAL(NUMSE3)
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
          WRITE(ICOUT,1021)N2
 1021     FORMAT('DPTAP2: AFTER CALL DPTAP2--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1022)XINC,YINC,YINC2
 1022     FORMAT('XINC,YINC,YINC2 = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IFLAGU.EQ.1)THEN
          DO 2020 I=1,N2
            STAT=TEMP6(I)
            STATMN=XACLOW(I)
            STATMX=XACUPP(I)
            IF(ITPLDI.EQ.'X')THEN
!CCCC         XVAL=TEMP7(I)
!CCCC         YVAL=TEMP8(I)
!CCCC         XVAL2=TEMP9(I)
              XVAL=TEMP8(I)
              YVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              XCOOR1=XVAL - XINC
              XCOOR2=XVAL + XINC
              YCOOR1=YVAL - YINC
              YCOOR2=YVAL + YINC
            ELSE
!CCCC         YVAL=TEMP7(I)
!CCCC         XVAL=TEMP8(I)
!CCCC         YVAL2=TEMP9(I)
              XCOOR1=XVAL - XINC
              XCOOR2=XVAL + XINC
              YCOOR1=YVAL - YINC
              YCOOR2=YVAL + YINC
            ENDIF
!
!           DIVIDE RECTANGLE INTO "ITPLNI" VERTICAL INCREMENTS AND
!           COMPUTE LEVEL-COLOR INDEPENDENTLY FOR EACH OF THESE
!           MINI-RECTANGLES.
!
            STATIN=(STATMX - STATMN)/REAL(ITPLNI)
            STATZ=STATMN - STATIN
            AINC=(YCOOR2 - YCOOR1)/REAL(ITPLNI)
            YCZ2=YCOOR1
!
            DO 2029 IROW=1,ITPLNI
!
              YCZ1=YCZ2
              YCZ2=YCZ1 + AINC
!
              STATZ=STATZ + STATIN
              IF(STATZ.LT.YLEVEL(1))THEN
                ILEVEL=1
              ELSEIF(STATZ.GE.YLEVEL(NLEVEL))THEN
                ILEVEL=NLEVEL+1
              ELSE
                DO 2025 J=2,NLEVEL
                  IF(STATZ.GE.YLEVEL(J-1) .AND. STATZ.LT.YLEVEL(J))THEN
                    ILEVEL=J
                  ENDIF
 2025           CONTINUE
              ENDIF
!
              IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
                WRITE(ICOUT,2026)I,IROW,STAT,STATZ,STATMN,STATMX,STATIN
 2026           FORMAT('I,IROW,STAT,STATZ,STATMN,STATMX,STATIN = ',   &
                       2I8,5G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2027)XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2
 2027           FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2 = ',   &
                       6G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2028)IROW,ILEVEL
 2028           FORMAT('IROW,ILEVEL = ',2I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
 2029       CONTINUE
!
 2020     CONTINUE
        ELSE
          DO 1020 I=1,N2
            STAT=TEMP6(I)
            IF(ITPLDI.EQ.'X')THEN
!CCCC         XVAL=TEMP7(I)
!CCCC         YVAL=TEMP8(I)
!CCCC         XVAL2=TEMP9(I)
              XVAL=TEMP8(I)
              YVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              XCOOR1=XVAL - XINC
              XCOOR2=XVAL + XINC
              YCOOR1=YVAL - YINC
              YCOOR2=YVAL + YINC
            ELSE
!CCCC         YVAL=TEMP7(I)
!CCCC         XVAL=TEMP8(I)
!CCCC         YVAL2=TEMP9(I)
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
              DO 1025 J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1025       CONTINUE
            ENDIF
!
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
!
            ICNT2=ICNT2+1
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR2
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR2
            Y(ICNT)=YCOOR2
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR2
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
 1020     CONTINUE
        ENDIF
!
        NPLOTP=ICNT
        NPLOTV=2
!
      ELSEIF(NCRTV.EQ.4)THEN
        CALL DPTAP5(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,   &
                    NUMV2,ICASCT,ISTANR,ISTARA,   &
                    XIDTEM,XIDTE2,XIDTE3,XIDTE4,   &
                    NUMSE1,NUMSE2,NUMSE3,NUMSE4,   &
                    TEMP1,TEMP2,TMP11,TEMP3,TEMP4,TEMP5,   &
                    XTEMP1,XTEMP2,   &
                    ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ISEED,ALPHA,   &
                    ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                    TEMP6,TEMP7,TEMP8,TEMP9,TMP10,XACLOW,XACUPP,N2,   &
                    ISUBRO,IBUGG3,IERROR)
!
!CCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE A RECTANGLE
!CCCC   FOR EACH POINT.
!
        ICNT=0
        ICNT2=0
        XINC=0.5 - PTPLXI
        YINC=0.5 - PTPLYI
        YINC2=2.0*YINC/REAL(NUMSE3)
        XINC2=2.0*XINC/REAL(NUMSE4)
!
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
          WRITE(ICOUT,1031)N2
 1031     FORMAT('DPTAP2: AFTER CALL DPTAP5--N2 = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1032)XINC,YINC,XINC2,YINC2
 1032     FORMAT('XINC,YINC,XINC2,YINC2 = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IFLAGU.EQ.1)THEN
          DO 2030 I=1,N2
            STAT=TEMP6(I)
            STATMN=XACLOW(I)
            STATMX=XACUPP(I)
            IF(ITPLDI.EQ.'X')THEN
!CCCC         XVAL=TEMP7(I)
!CCCC         YVAL=TEMP8(I)
!CCCC         XVAL2=TEMP9(I)
!CCCC         YVAL2=TMP10(I)
              XVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
              YVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
            ELSE
!CCCC         YVAL=TEMP7(I)
!CCCC         XVAL=TEMP8(I)
!CCCC         YVAL2=TEMP9(I)
!CCCC         XVAL2=TMP10(I)
              XVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              YVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
            ENDIF
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
!
!           DIVIDE RECTANGLE INTO "ITPLNI" VERTICAL INCREMENTS AND
!           COMPUTE LEVEL-COLOR INDEPENDENTLY FOR EACH OF THESE
!           MINI-RECTANGLES.
!
            STATIN=(STATMX - STATMN)/REAL(ITPLNI)
            STATZ=STATMN - STATIN
            AINC=(YCOOR2 - YCOOR1)/REAL(ITPLNI)
            YCZ2=YCOOR1
!
            DO 2039 IROW=1,ITPLNI
!
              YCZ1=YCZ2
              YCZ2=YCZ1 + AINC
!
              STATZ=STATZ + STATIN
              IF(STATZ.LT.YLEVEL(1))THEN
                ILEVEL=1
              ELSEIF(STATZ.GE.YLEVEL(NLEVEL))THEN
                ILEVEL=NLEVEL+1
              ELSE
                DO 2035 J=2,NLEVEL
                  IF(STATZ.GE.YLEVEL(J-1) .AND. STATZ.LT.YLEVEL(J))THEN
                    ILEVEL=J
                  ENDIF
 2035           CONTINUE
              ENDIF
!
              IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'TAP2')THEN
                WRITE(ICOUT,2036)I,IROW,STAT,STATZ,STATMN,STATMX,STATIN
 2036           FORMAT('I,IROW,STAT,STATZ,STATMN,STATMX,STATIN = ',   &
                       2I8,5G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2037)XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2
 2037           FORMAT('XCOOR1,XCOOR2,YCOOR1,YCOOR2,YCZ1,YCZ2 = ',   &
                       6G15.7)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2038)IROW,ILEVEL
 2038           FORMAT('IROW,ILEVEL = ',2I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              ICNT2=ICNT2+1
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR2
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ2
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
              ICNT=ICNT+1
              X(ICNT)=XCOOR1
              Y(ICNT)=YCZ1
              D(ICNT)=REAL(ICNT2)
              DCOLOR(ICNT)=REAL(ILEVEL)
!
 2039       CONTINUE
!
 2030     CONTINUE
        ELSE
          DO 1030 I=1,N2
            STAT=TEMP6(I)
            IF(ITPLDI.EQ.'X')THEN
!CCCC         XVAL=TEMP7(I)
!CCCC         YVAL=TEMP8(I)
!CCCC         XVAL2=TEMP9(I)
!CCCC         YVAL2=TMP10(I)
              XVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
              YVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
            ELSE
!CCCC         YVAL=TEMP7(I)
!CCCC         XVAL=TEMP8(I)
!CCCC         YVAL2=TEMP9(I)
!CCCC         XVAL2=TMP10(I)
              XVAL=ANUMS1*(TEMP9(I)  - 1.0) + TEMP7(I)
              YVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
            ENDIF
!CCCC       XCOOR1=XVAL - XINC) + (XVAL2 - 1.0)*XINC2
            XCOOR1=XVAL - XINC
            XCOOR2=XVAL + XINC
!CCCCC      YCOOR1=(YVAL - YINC) + (YVAL2 - 1.0)*YINC2
            YCOOR1=YVAL - YINC
            YCOOR2=YVAL + YINC
            IF(STAT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STAT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO 1035 J=2,NLEVEL
                IF(STAT.GE.YLEVEL(J-1) .AND. STAT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1035       CONTINUE
            ENDIF
!
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
!
            ICNT2=ICNT2+1
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR2
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR2
            Y(ICNT)=YCOOR2
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR2
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
            ICNT=ICNT+1
            X(ICNT)=XCOOR1
            Y(ICNT)=YCOOR1
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=REAL(ILEVEL)
!
 1030     CONTINUE
        ENDIF
!
        NPLOTP=ICNT
        NPLOTV=2
!
      ENDIF
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9035 I=1,NPLOTP
          WRITE(ICOUT,9036)I,Y(I),X(I),D(I),DCOLOR(I)
 9036     FORMAT('I,Y(I),X(I),D(I),DCOLOR(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAP2
      SUBROUTINE DPTAP3(Y,Z,Z2,TAG1,TAG2,N,   &
                        NUMV2,ICASCT,ISTANR,ISTARA,   &
                        XIDTEM,XIDTE2,   &
                        NUMSE1,NUMSE2,   &
                        TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                        XTEMP4,XTEMP5,   &
                        YCMNMX,YRMNMX,YGMNMX,ITPLCM,ITPLRM,ITPLGM,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ISEED,ALPHA,   &
                        ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                        Y2,X2,D2,XACLOW,XACUPP,N2,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--GENERATE A TWO-WAY TABULATION PLOT.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --DECEMBER  2009. UNCERTAINTY OPTION FOR
!                                       BINOMIAL PROBABILITY, MEAN AND
!                                       MEDIAN CONFIDENCE INTERVAL
!     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
!                                       FOR BINOMIAL RATIO
!     UPDATED         --AUGUST    2010. FOR EACH VALUE, DETERMINE IF
!                                       IT A ROW COLUMN MINIMUM OR
!                                       MAXIMUM VALUE FOR THE STATISTIC
!     UPDATED         --JULY      2021. SUPPORT FOR "GRAND" MINIMUM OR
!                                       MAXIMUM
!     UPDATED         --JULY      2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASCT
      CHARACTER*4 ISTARA
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
      CHARACTER*4 ITPLCM
      CHARACTER*4 ITPLRM
      CHARACTER*4 ITPLGM
      CHARACTER*4 IBUGG3
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
!
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION XTEMP4(*)
      DIMENSION XTEMP5(*)
      DIMENSION YCMNMX(*)
      DIMENSION YRMNMX(*)
      DIMENSION YGMNMX(*)
!
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTA'
      ISUBN2='P3  '
!
      I2=0
!
      AN=INT(N+0.01)
!
!               ***********************************************
!               **  STEP 5--                                 **
!               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
!               ***********************************************
!
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
!
!     FOR EACH ROW/COLUMN COMBINATION, DETERMINE IF IT IS A
!     ROW OR COLUMN MINIMUM OR MAXIMUM.
      J=0
      NRESP=NUMV2-2
      DO 1110 ISET1=1,NUMSE1
        DO 1120 ISET2=1,NUMSE2
!
          K=0
          DO 1130 I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.XIDTE2(ISET2).EQ.TAG2(I))   &
              GO TO 1131
            GO TO 1130
 1131       CONTINUE
!
            K=K+1
            TEMP(K)=0.0
            TEMPZ(K)=0.0
            TEMPZ2(K)=0.0
            IF(ISTANR.GE.1)TEMP(K)=Y(I)
            IF(ISTANR.GE.2)TEMPZ(K)=Z(I)
            IF(ISTANR.GE.3)TEMPZ2(K)=Z2(I)
 1130     CONTINUE
          NTEMP=K
!
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
                 ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
                 ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GO TO 1120
            ENDIF
          ELSE
            CALL CMPSTA(   &
                    TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                    XTEMP4,XTEMP5,   &
                    MAXNXT,NTEMP,NTEMP,NTEMP,   &
                    NRESP,ICASCT,ISTARA,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    STAT,   &
                    ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
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
                CALL DPAGCO(PTEMP,NTRAIL,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
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
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,   &
                            QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
!
          J=J+1
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          AMNMAX=0.0
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
             ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
            IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
!
 1120   CONTINUE
 1110 CONTINUE
      N2=J
!
!     DETERMINE THE COLUMN/ROW/GRAND MINIMUM AND MAXIMUM POINTS
!
      IF(ITPLCM.EQ.'OFF' .AND. ITPLRM.EQ.'OFF' .AND.   &
         ITPLGM.EQ.'OFF')GO TO 9000
!
      DO 3101 I=1,N
        YCMNMX(I)=0.0
        YRMNMX(I)=0.0
        YGMNMX(I)=0.0
 3101 CONTINUE
!
      DO 3110 ISET2=1,NUMSE2
        ACOLMN=CPUMIN
        ACOLMX=CPUMIN
!
!       DETERMINE COLUMN MIN/MAX
!
        DO 3120 I=1,N
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
!
!       NOW SET YCMNMX TO:
!
!           0 = NEITHER MIN NOR MAX
!           1 = EQUAL TO COLUMN MINIMUM
!           2 = EQUAL TO COLUMN MAXIMUM
!
        DO 3130 I=1,N
          IF(XIDTE2(ISET2).EQ.D2(I))THEN
            YCMNMX(I)=0.0
            IF(Y2(I).EQ.ACOLMN)YCMNMX(I)=1.0
            IF(Y2(I).EQ.ACOLMX)YCMNMX(I)=2.0
          ENDIF
 3130   CONTINUE
!
 3110 CONTINUE
!
!     DETERMINE THE ROW MINIMUM AND MAXIMUM POINTS
!
      DO 4110 ISET1=1,NUMSE1
        AROWMN=CPUMIN
        AROWMX=CPUMIN
!
!       DETERMINE ROW MIN/MAX
!
        DO 4120 I=1,N
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
!
!       NOW SET YRMNMX TO:
!
!           0 = NEITHER MIN NOR MAX
!           1 = EQUAL TO ROW MINIMUM
!           2 = EQUAL TO ROW MAXIMUM
!
        DO 4130 I=1,N
          IF(XIDTEM(ISET1).EQ.X2(I))THEN
            YRMNMX(I)=0.0
            IF(Y2(I).EQ.AROWMN)YRMNMX(I)=1.0
            IF(Y2(I).EQ.AROWMX)YRMNMX(I)=2.0
          ENDIF
 4130   CONTINUE
!
 4110 CONTINUE
!
!     DETERMINE THE GRAND MINIMUM AND MAXIMUM POINTS
!
      AGRAMN=CPUMIN
      AGRAMX=CPUMIN
!
!       DETERMINE ROW MIN/MAX
!
      DO 5120 I=1,N
        IF(Y2(I).NE.PSTAMV .AND. Y2(I).NE.CPUMIN)THEN
          IF(AGRAMN.EQ.CPUMIN)THEN
            AGRAMN=Y2(I)
            AGRAMX=Y2(I)
          ELSE
            IF(Y2(I).LE.AGRAMN)AGRAMN=Y2(I)
            IF(Y2(I).GE.AGRAMX)AGRAMX=Y2(I)
          ENDIF
        ENDIF
 5120 CONTINUE
!
!     NOW SET YGMNMX TO:
!
!           0 = NEITHER MIN NOR MAX
!           1 = EQUAL TO GRAND MINIMUM
!           2 = EQUAL TO GRAND MAXIMUM
!
      DO 5130 I=1,N
        YGMNMX(I)=0.0
        IF(Y2(I).EQ.AGRAMN)YGMNMX(I)=1.0
        IF(Y2(I).EQ.AGRAMX)YGMNMX(I)=2.0
 5130 CONTINUE
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9020 I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),YCMNMX(I),YRMNMX(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),YCMNMX(I),YRMNMX(I) = ',   &
                 I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAP3
      SUBROUTINE DPTAP4(Y,Z,Z2,TAG1,TAG2,TAG3,N,   &
                        NUMV2,ICASCT,ISTANR,ISTARA,   &
                        XIDTEM,XIDTE2,XIDTE3,   &
                        NUMSE1,NUMSE2,NUMSE3,   &
                        TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                        XTEMP4,XTEMP5,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ISEED,ALPHA,   &
                        ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                        Y2,X2,D2,D3,XACLOW,XACUPP,N2,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--GENERATE A TWO-WAY TABULATION PLOT.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --DECEMBER  2009. UNCERTAINTY OPTION FOR
!                                       BINOMIAL PROBABILITY, MEAN AND
!                                       MEDIAN CONFIDENCE INTERVAL
!     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
!                                       FOR BINOMIAL RATIO
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASCT
      CHARACTER*4 ISTARA
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
      CHARACTER*4 IBUGG3
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
!
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION XTEMP4(*)
      DIMENSION XTEMP5(*)
!
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
!
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTA'
      ISUBN2='P4  '
!
      I2=0
!
      AN=INT(N+0.01)
!
!               ***********************************************
!               **  STEP 5--                                 **
!               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
!               ***********************************************
!
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
!
      J=0
      NRESP=NUMV2-3
      DO 1110 ISET1=1,NUMSE1
        DO 1120 ISET2=1,NUMSE2
        DO 1130 ISET3=1,NUMSE3
!
          K=0
          DO 1180 I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.   &
               XIDTE2(ISET2).EQ.TAG2(I).AND.   &
               XIDTE3(ISET3).EQ.TAG3(I)   &
             )GO TO 1181
            GO TO 1180
 1181       CONTINUE
!
            K=K+1
            TEMP(K)=0.0
            TEMPZ(K)=0.0
            TEMPZ2(K)=0.0
            IF(ISTANR.GE.1)TEMP(K)=Y(I)
            IF(ISTANR.GE.2)TEMPZ(K)=Z(I)
            IF(ISTANR.GE.3)TEMPZ2(K)=Z2(I)
 1180     CONTINUE
          NTEMP=K
!
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
                 ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
                 ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GO TO 1130
            ENDIF
          ELSE
            CALL CMPSTA(   &
                    TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                    XTEMP4,XTEMP5,   &
                    MAXNXT,NTEMP,NTEMP,NTEMP,   &
                    NRESP,ICASCT,ISTARA,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    STAT,   &
                    ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
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
                CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
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
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,   &
                            QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
!
          J=J+1
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          D3(J)=XIDTE3(ISET3)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
             ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
            IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
!
 1130   CONTINUE
 1120   CONTINUE
 1110 CONTINUE
      N2=J
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9020 I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),D3(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),D3(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAP4
      SUBROUTINE DPTAP5(Y,Z,Z2,TAG1,TAG2,TAG3,TAG4,N,   &
                        NUMV2,ICASCT,ISTANR,ISTARA,   &
                        XIDTEM,XIDTE2,XIDTE3,XIDTE4,   &
                        NUMSE1,NUMSE2,NUMSE3,NUMSE4,   &
                        TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                        XTEMP4,XTEMP5,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ISEED,ALPHA,   &
                        ICTAMV,PCTAMV,PSTAMV,IQUASE,   &
                        Y2,X2,D2,D3,D4,XACLOW,XACUPP,N2,   &
                        ISUBRO,IBUGG3,IERROR)
!
!     PURPOSE--GENERATE A TWO-WAY TABULATION PLOT.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --DECEMBER  2009. UNCERTAINTY OPTION FOR
!                                       BINOMIAL PROBABILITY, MEAN AND
!                                       MEDIAN CONFIDENCE INTERVAL
!     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
!                                       FOR BINOMIAL RATIO
!     UPDATED         --AUGUST    2023. CALL LIST TO CMPSTA
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASCT
      CHARACTER*4 ISTARA
      CHARACTER*4 ICTAMV
      CHARACTER*4 IQUASE
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
      CHARACTER*4 ISUBRO
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
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
!
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
      DIMENSION XTEMP4(*)
      DIMENSION XTEMP5(*)
!
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
!
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTA'
      ISUBN2='P5  '
!
      I2=0
!
      AN=INT(N+0.01)
!
!               ***********************************************
!               **  STEP 5--                                 **
!               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
!               ***********************************************
!
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'TAP5')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
!
      J=0
      NRESP=NUMV2-4
      DO 1110 ISET1=1,NUMSE1
        DO 1120 ISET2=1,NUMSE2
        DO 1130 ISET3=1,NUMSE3
        DO 1140 ISET4=1,NUMSE4
!
          K=0
          DO 1180 I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.   &
               XIDTE2(ISET2).EQ.TAG2(I).AND.   &
               XIDTE3(ISET3).EQ.TAG3(I).AND.   &
               XIDTE4(ISET4).EQ.TAG4(I)   &
              )GO TO 1181
            GO TO 1180
 1181       CONTINUE
!
            K=K+1
            TEMP(K)=0.0
            TEMPZ(K)=0.0
            TEMPZ2(K)=0.0
            IF(ISTANR.GE.1)TEMP(K)=Y(I)
            IF(ISTANR.GE.2)TEMPZ(K)=Z(I)
            IF(ISTANR.GE.3)TEMPZ2(K)=Z2(I)
 1180     CONTINUE
          NTEMP=K
!
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
                 ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
                 ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GO TO 1140
            ENDIF
          ELSE
            CALL CMPSTA(   &
                    TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,   &
                    XTEMP4,XTEMP5,   &
                    MAXNXT,NTEMP,NTEMP,NTEMP,   &
                    NRESP,ICASCT,ISTARA,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    STAT,   &
                    ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
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
                CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
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
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,   &
                            QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,   &
                            ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
!
          J=J+1
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          D3(J)=XIDTE3(ISET3)
          D4(J)=XIDTE4(ISET4)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.   &
             ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
            IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
!
 1140   CONTINUE
 1130   CONTINUE
 1120   CONTINUE
 1110 CONTINUE
      N2=J
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
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
        DO 9020 I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),D3(I),D4(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),D3(I),D4(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAP5
      SUBROUTINE DPTAWI(IFORWI,IFORWR,MAXNWI,   &
                        ISUBRO,IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--IMPLEMENT THE COMMAND
!
!                  TABLE WIDTH  <SIGDIG>   <TOTWID>
!
!              THIS IS AN ALTERNATIVE TO "SET WRITE DECIMALS" AND
!              "SET WRITE FORMAT" FOR DEFINING HOW TO PRINT
!              VARIABLES WITH THE WRITE COMMAND.  THE LIMITATION
!              OF "SET WRITE DECIMALS" IS THAT IT ONLY ALLOWS YOU
!              TO SPECIFY THE NUMBER OF DIGITS TO THE RIGHT OF
!              THE DECIMAL POINT AND IT SETS ALL COLUMNS TO THE
!              SAME VALUE.  THE LIMITATION OF SET WRITE FORMAT
!              IS THAT IT CANNOT BE EASILY APPLIED TO HTML, LATEK,
!              OR RTF OUTPUT.
!
!              THE <SIGDIG> VARIABLE DEFINES THE NUMBER OF DIGITS
!              TO THE RIGHT OF THE DECIMAL POINT AND <TOTWID> DEFINES
!              THE TOTAL WIDTH OF THE FIELD (SO THIS SETS Fxx.yy
!              FORMAT WHERE WE ARE DEFINING "yy" AND "xx").
!
!              IF EITHER <SIGDIG> OR <TOTWID> IS NEGATIVE, THEN
!              WE USE   Exx.yy FORMAT.
!
!              IF <SIGDIG> OR <TOTWID> IS A SCALAR, THEN ALL ROWS
!              OF IFORWI AND IFORWR WILL BE SET.  IF ONLY <SIGDIG>
!              IS SPECIFIED, <TOTWID> WILL BE SET TO -99 (THIS IS
!              EQUIVALENT TO USING SET WRITE DECIMALS) FOR F FORMAT
!              AND TO <SIGDIG> + 8 FOR E FORMAT.
!
!     INPUT ARGUMENTS --MAXNWI      = MAXIMUM NUMBER OF FIELDS THAT
!                                     CAN BE SPECIFIED
!     OUTPUT ARGUMENTS--IFORWI      = INTEGER ARRAY THAT DEFINES THE
!                                     TOTAL WIDTH OF THE FIELDS
!                     --IFORWR      = INTEGER ARRAY THAT DEFINES THE
!                                     NUMBER OF DIGITS TO THE RIGHT OF
!                                     THE DECIMAL
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/3
!     ORIGINAL VERSION--MARCH     2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHWUSE
      CHARACTER*4 IH11
      CHARACTER*4 IH12
      CHARACTER*4 MESSAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOM2.INC'
!
      DIMENSION IFORWI(*)
      DIMENSION IFORWR(*)
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
      IHOLD1=0
      IHOLD2=0
      I1=-99
      I2=-99
      I3=-99
      ICOL2=0
!
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
        DO 55 I=1,MAXNWI
          WRITE(ICOUT,57)I,IFORWI(I),IFORWR(I)
   57     FORMAT('I,IFORWI(I),IFORWR(I) = ',3I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ****************************************************
!               **  TREAT THE CASE WHEN                           **
!               **  THE FORMAT WIDTHS ARE TO BE CHANGED           **
!               ****************************************************
!
      IF(NUMARG.LE.0)GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'WIDT')GO TO 1110
      GO TO 1190
!
 1110 CONTINUE
      IF(NUMARG.EQ.1)GO TO 1120
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1120
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1120
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1120
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1120
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
      IF(NUMARG.GE.3.AND.IARGT(2).EQ.'NUMB'.AND.   &
      IARGT(3).EQ.'NUMB')GO TO 1130
      IF(NUMARG.GE.2.AND.IARGT(2).EQ.'NUMB')GO TO 1140
      GO TO 3140
!
!     CASE 1: RESET DEFAULT
!
 1120 CONTINUE
      I1=-99
      I2=-99
      DO 1122 I=1,MAXNWI
        IFORWI(I)=I1
        IFORWR(I)=I2
 1122 CONTINUE
      GO TO 1180
!
!     CASE 2: BOTH VALUES SCALARS
!
 1130 CONTINUE
      I1=IARG(2)
      I2=IARG(3)
      DO 1132 I=1,MAXNWI
        IFORWI(I)=I1
        IFORWR(I)=I2
 1132 CONTINUE
      GO TO 1180
!
!     CASE 3: ONE SCALAR SPECIFIED
!
 1140 CONTINUE
      I1=-99
      I2=IARG(2)
      DO 1142 I=1,MAXNWI
        IFORWI(I)=I1
        IFORWR(I)=I2
 1142 CONTINUE
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
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
      GO TO 9000
!
 1190 CONTINUE
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
      WRITE(ICOUT,8109)
 8109 FORMAT('FIELD WIDTH     FIELD DIGITS')
      CALL DPWRST('XXX','BUG ')
      DO 8110 I=1,MAXNWI
        WRITE(ICOUT,8111)IFORWI(I),IFORWR(I)
 8111   FORMAT(I11,5X,I12)
        CALL DPWRST('XXX','BUG ')
 8110 CONTINUE
      GO TO 9000
!
 3140 CONTINUE
!
      IF(IARGT(2).EQ.'NUMB')THEN
        I2=IARG(2)
        N1=-99
      ELSE
        IH11=IHARG(2)
        IH12=IHARG2(2)
        IHWUSE='V'
        MESSAG='YES'
        CALL CHECKN(IH11,IH12,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'NO')THEN
          ICOL1=IVALUE(ILOCV)
          N1=IN(ILOCV)
        ELSE
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(IARGT(3).EQ.'NUMB')THEN
        I3=IARG(3)
        N2=-99
      ELSE
        IH11=IHARG(3)
        IH12=IHARG2(3)
        IHWUSE='V'
        MESSAG='YES'
        CALL CHECKN(IH11,IH12,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'NO')THEN
          ICOL2=IVALUE(ILOCV)
          N2=IN(ILOCV)
        ELSE
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(N1.GT.0)THEN
        J=0
        IMAX=MIN(MAXNWI,N1)
        DO 3160 I=1,IMAX
!
          IF(I.GT.IMAX)GO TO 3169
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
!
 3160   CONTINUE
 3169   CONTINUE
!
      ELSE
        DO 3165 J=1,MAXNWI
          IFORWI(J)=I2
 3165   CONTINUE
      ENDIF
!
      IF(N2.GT.0)THEN
        J=0
        IMAX=MIN(MAXNWI,N2)
        DO 3170 I=1,IMAX
!
          IF(I.GT.IMAX)GO TO 3179
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
!
 3170   CONTINUE
 3179   CONTINUE
!
      ELSE
        DO 3175 J=1,MAXNWI
          IFORWR(J)=I3
 3175   CONTINUE
      ENDIF
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8109)
        CALL DPWRST('XXX','BUG ')
        ILAST=MAX(N1,N2)
        ILAST=MIN(ILAST,MAXNWI)
        DO 3190 I=1,ILAST
          WRITE(ICOUT,8111)IFORWI(I),IFORWR(I)
          CALL DPWRST('XXX','BUG ')
 3190   CONTINUE
      ENDIF
!
      IFOUND='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IF(ISUBRO.EQ.'TAWI' .OR. IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('****AT THE END OF DPTAWI')
        CALL DPWRST('XXX','BUG ')
        DO 9055 I=1,MAXNWI
          WRITE(ICOUT,9057)I,IFORWI(I),IFORWR(I)
 9057     FORMAT('I,IFORWI(I),IFORWR(I) = ',3I8)
          CALL DPWRST('XXX','BUG ')
 9055   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPTAWI
      SUBROUTINE DPTBCO(IHARG,NUMARG,IDETBC,MAXTEX,ITEBCO,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TEXT BORDER COLORS = THE COLORS
!              OF THE BORDER LINE AROUND THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR ITEBCO(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDETBC
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--ITEBCO (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDETBC
      CHARACTER*4 ITEBCO
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION ITEBCO(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTB'
      ISUBN2='CO  '
!
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEBCO(1)
   70 FORMAT('ITEBCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,ITEBCO(I)
   76 FORMAT('I,ITEBCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      ITEBCO(1)=IDETBC
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETBC
      ITEBCO(I)=IHOLD2
 1225 CONTINUE
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEBCO(I)
 1276 FORMAT('THE COLOR OF TEXT BORDER ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETBC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETBC
      DO 1315 I=1,NUMTEX
      ITEBCO(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEBCO(I)
 1316 FORMAT('THE COLOR OF ALL TEXT BORDERS',   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEBCO(1)
 9030 FORMAT('ITEBCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,ITEBCO(I)
 9036 FORMAT('I,ITEBCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTBCO
      SUBROUTINE DPTBLI(IHARG,IHARG2,NUMARG,IDETBL,MAXTEX,ITEBLI,   &
                        IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE BORDER LINES = THE LINES TYPES
!              OF THE BORDER AROUND THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR ITEBLI(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDETBL
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--ITEBLI (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!     UPDATED         --AUGUST    1995.  DASH2 BUG
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IDETBL
      CHARACTER*4 ITEBLI
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
      DIMENSION ITEBLI(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTB'
      ISUBN2='LI  '
!
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEBLI(1)
   70 FORMAT('ITEBLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,ITEBLI(I)
   76 FORMAT('I,ITEBLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 9000
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      IF(NUMARG.EQ.5)GO TO 1150
      GO TO 1160
!
 1130 CONTINUE
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(5).EQ.'ALL')IHOLD1='    '
      IF(IHARG(5).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
!CCCC IF(IHARG(5).EQ.'ALL')IHOLD1=IHARG(6)
!CCCC IF(IHARG(5).EQ.'ALL')GO TO 1300
!CCCC IF(IHARG(6).EQ.'ALL')IHOLD1=IHARG(5)
!CCCC IF(IHARG(6).EQ.'ALL')GO TO 1300
!CCCC APRIL 1996.  CHANGE IHOLD TO IHOLD1 BELOW
      IF(IHARG(5).EQ.'ALL')THEN
        IHOLD1=IHARG(6)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'5')IHOLD1='DA5'
        GO TO 1300
      ENDIF
      IF(IHARG(6).EQ.'ALL')THEN
        IHOLD1=IHARG(5)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'5')IHOLD1='DA5'
        GO TO 1300
      ENDIF
      GO TO 1200
!
 1160 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.3)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      ITEBLI(1)='    '
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-3
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEBLI(I)
 1276 FORMAT('THE LINE TYPE FOR TEXT BORDER ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETBL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETBL
      DO 1315 I=1,NUMTEX
      ITEBLI(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEBLI(I)
 1316 FORMAT('THE LINE TYPE FOR ALL TEXT BORDERS',   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEBLI(1)
 9030 FORMAT('ITEBLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,ITEBLI(I)
 9036 FORMAT('I,ITEBLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTBLI
      SUBROUTINE DPTBTH(IHARG,IARGT,ARG,NUMARG,PDETBT,MAXTEX,PTEBTH,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TEXT (BORDER) LINE THICKNESSES = THE THICKNESSES
!              OF THE BORDER LINE AROUND THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR PTEBTH(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT  (A  CHARACTER VECTOR)
!                     --ARG
!                     --NUMARG
!                     --PDETBT
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--PTEBTH (A FLOATING POINT VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PTEBTH(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTB'
      ISUBN2='TH  '
!
      NUMTEX=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PTEBTH(1)
   70 FORMAT('PTEBTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,PTEBTH(I)
   76 FORMAT('I,PTEBTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDETBT
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      PTEBTH(1)=PDETBT
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,PTEBTH(I)
 1276 FORMAT('THE THICKNESS OF TEXT BORDER ',I6,   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDETBT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDETBT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDETBT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDETBT
      DO 1315 I=1,NUMTEX
      PTEBTH(I)=HOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PTEBTH(I)
 1316 FORMAT('THE THICKNESS OF ALL TEXT BORDERS',   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PTEBTH(1)
 9030 FORMAT('PTEBTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,PTEBTH(I)
 9036 FORMAT('I,PTEBTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTBTH
      SUBROUTINE DPTCCL(ICOM,IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                        IDEFCO,IRGBMX,   &
                        IX1TCO,IX2TCO,IY1TCO,IY2TCO,   &
                        IX1TC2,IX2TC2,IY1TC2,IY2TC2,   &
                        IBUGPC,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK COLOR SWITCHES FOR ANY OF THE 4 FRAME
!              LINES.  SUCH TIC MARK SWITCHES DESCRIBE THE TIC MARK
!              COLOR ON THE 4 FRAME LINES OF A PLOT.  THE CONTENTS OF A
!              TIC MARK COLOR SWITCH ARE A COLOR.  THE TIC MARK COLOR
!              SWITCHES FOR THE 4 FRAME LINES ARE CONTAINED IN THE 4
!              VARIABLES IX1TCO,IX2TCO,IY1TCO,IY2TCO.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --IARG
!                     --NUMARG
!                     --IDEFCO
!     OUTPUT ARGUMENTS--IX1TCO = COLOR FOR BOTTOM HORIZ. TICS
!                     --IX2TCO = COLOR FOR TOP    HORIZ. TICS
!                     --IY1TCO = COLOR FOR LEFT   VERT.  TICS
!                     --IY2TCO = COLOR FOR RIGHT  VERT.  TICS
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
!     ORIGINAL VERSION--OCTOBER   1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLORS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IDEFCO
      CHARACTER*4 IX1TCO
      CHARACTER*4 IX2TCO
      CHARACTER*4 IY1TCO
      CHARACTER*4 IY2TCO
      CHARACTER*4 IBUGPC
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
      CHARACTER*4 ICASCL
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IHARG2(*)
      CHARACTER*4 IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IX1TC2(3)
      DIMENSION IX2TC2(3)
      DIMENSION IY1TC2(3)
      DIMENSION IY2TC2(3)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ICASCL='STAN'
      IHOLD=' '
      JHOLD1=-1
      JHOLD2=-1
      JHOLD3=-1
!
      IF(NUMARG.LE.0)GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')THEN
        GO TO 1090
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
             IHARG(2).EQ.'COLO')THEN
        GO TO 1090
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'RGB '.AND.   &
             IHARG(2).EQ.'COLO')THEN
        ICASCL='RGB '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGPC,IERROR)
        IHARG(1)='COLO'
        IHARG2(1)='R   '
        GO TO 1090
      ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
             IHARG(2).EQ.'RGB '.AND.IHARG(3).EQ.'COLO')THEN
        ICASCL='RGB '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGPC,IERROR)
        IHARG(1)='MARK'
        IHARG2(1)='    '
        IHARG(2)='COLO'
        IHARG2(2)='R   '
        GO TO 1090
      ENDIF
!
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IX1TC2(1)=JHOLD1
          IX1TC2(2)=JHOLD2
          IX1TC2(3)=JHOLD3
          IX2TC2(1)=JHOLD1
          IX2TC2(2)=JHOLD2
          IX2TC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1186)
 1186       FORMAT('THE TIC MARK RGB COLOR (FOR BOTH HORIZONTAL ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
 1187       FORMAT('HAS JUST BEEN SET TO ',3I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IX1TCO=IHOLD
          IX2TCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1181)
 1181       FORMAT('THE TIC MARK COLOR (FOR BOTH HORIZONTAL ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
 1182       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      ELSEIF(ICOM.EQ.'X1TI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IX1TC2(1)=JHOLD1
          IX1TC2(2)=JHOLD2
          IX1TC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1286)
 1286       FORMAT('THE TIC MARK RGB COLOR (FOR THE BOTTOM HORIZONTAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IX1TCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1281)
 1281       FORMAT('THE TIC MARK COLOR (FOR THE BOTTOM HORIZONTAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      ELSEIF(ICOM.EQ.'X2TI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IX2TC2(1)=JHOLD1
          IX2TC2(2)=JHOLD2
          IX2TC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1386)
 1386       FORMAT('THE TIC MARK RGB COLOR (FOR THE TOP HORIZONTAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IX2TCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1381)
 1381       FORMAT('THE TIC MARK COLOR (FOR THE TOP HORIZONTAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      ELSEIF(ICOM.EQ.'YTIC')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IY1TC2(1)=JHOLD1
          IY1TC2(2)=JHOLD2
          IY1TC2(3)=JHOLD3
          IY2TC2(1)=JHOLD1
          IY2TC2(2)=JHOLD2
          IY2TC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1486)
 1486       FORMAT('THE TIC MARK RGB COLOR (FOR BOTH VERTICAL ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IY1TCO=IHOLD
          IY2TCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1481)
 1481       FORMAT('THE TIC MARK COLOR (FOR BOTH VERTICAL ',   &
                   'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      ELSEIF(ICOM.EQ.'Y1TI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IY1TC2(1)=JHOLD1
          IY1TC2(2)=JHOLD2
          IY1TC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1586)
 1586       FORMAT('THE TIC MARK RGB COLOR (FOR THE LEFT VERTICAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IY1TCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1581)
 1581       FORMAT('THE TIC MARK COLOR (FOR THE LEFT VERTICAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      ELSEIF(ICOM.EQ.'Y2TI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IY2TC2(1)=JHOLD1
          IY2TC2(2)=JHOLD2
          IY2TC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1686)
 1686       FORMAT('THE TIC MARK RGB COLOR (FOR THE RIGHT VERTICAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IY2TCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1681)
 1681       FORMAT('THE TIC MARK COLOR (FOR THE RIGHT VERTICAL ',   &
                   'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      ELSEIF(ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.   &
            ICOM.EQ.'XYTI' .OR. ICOM.EQ.'YXTI')THEN
        IFOUND='YES'
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
!
        IF(ICASCL.EQ.'RGB ')THEN
          IX1TC2(1)=JHOLD1
          IX1TC2(2)=JHOLD2
          IX1TC2(3)=JHOLD3
          IX2TC2(1)=JHOLD1
          IX2TC2(2)=JHOLD2
          IX2TC2(3)=JHOLD3
          IY1TC2(1)=JHOLD1
          IY1TC2(2)=JHOLD2
          IY1TC2(3)=JHOLD3
          IY2TC2(1)=JHOLD1
          IY2TC2(2)=JHOLD2
          IY2TC2(3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1786)
 1786       FORMAT('THE TIC MARK RGB COLOR (FOR ALL 4 FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IX1TCO=IHOLD
          IX2TCO=IHOLD
          IY1TCO=IHOLD
          IY2TCO=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1781)
 1781       FORMAT('THE TIC MARK COLOR (FOR ALL 4 FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
!
      ENDIF
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTCCL
      SUBROUTINE DPTCDP(ICOM,IHARG,IARG,NUMARG,   &
                        IDEFDP,   &
                        IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK LABEL DECIMAL PLACES
!              FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK LABEL SWITCHES DESCRIBE
!              THE NUMBER OF TIC MARK LABEL DECIMAL PLACES ON THE 4 FRAME LINES
!              THE CONTENTS OF A TIC MARK LABEL DECIMAL PLACE ARE
!              AN INTEGER NUMBER.
!              THE TIC MARK LABEL DECIMAL PLACES FOR THE 4 FRAME LINES
!              ARE CONTAINED IN THE 4 VARIABLES
!              IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --IARG  (AN INTEGER VECTOR)
!                     --NUMARG
!                     --IDEFDP
!     OUTPUT ARGUMENTS--IX1ZDP = NUM. DEC. FOR BOTTOM HORIZ. TIC LABELS
!                     --IX2ZDP = NUM. DEC. FOR TOP    HORIZ. TIC LABELS
!                     --IY1ZDP = NUM. DEC. FOR LEFT   VERT.  TIC LABELS
!                     --IY2ZDP = NUM. DEC. FOR RIGHT  VERT.  TIC LABELS
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
!     ORIGINAL VERSION--OCTOBER   1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
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
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
!
      IF(NUMARG.LE.0)GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI')GO TO 1090
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLAC')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'DECI'.AND.   &
      IHARG(2).EQ.'PLAC')GO TO 1090
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'DECI')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'PLAC')GO TO 1090
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'DECI')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LABE'.AND.   &
      IHARG(2).EQ.'PLAC')GO TO 1090
!
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(3).EQ.'PLAC')GO TO 1090
!CCCC JUNE 1994.  FOLLOWING 3 LINES ADDED (FOR TIC MARK LABEL DECIMAL)
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'LABE'.AND.   &
      IHARG(3).EQ.'DECI')GO TO 1090
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'LABEL'.AND.   &
      IHARG(3).EQ.'PLAC')GO TO 1090
!CCCC JUNE 1994.  FOLLOWING 2 LINES ADDED (FOR TIC MARK LABEL DECIMAL)
      IF(NUMARG.GE.4.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(4).EQ.'PLAC')GO TO 1090
!
      GO TO 9000
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DECI')GO TO 1150
      IF(IHARG(NUMARG).EQ.'PLAC')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD=IDEFDP
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IX1ZDP=IHOLD
      IX2ZDP=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC LABEL DECIMALS (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1183)
 1183 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 9000
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DECI')GO TO 1250
      IF(IHARG(NUMARG).EQ.'PLAC')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      IHOLD=IDEFDP
      GO TO 1280
!
 1260 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      IX1ZDP=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC LABEL DECIMALS (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1283)
 1283 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 9000
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DECI')GO TO 1350
      IF(IHARG(NUMARG).EQ.'PLAC')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      IHOLD=IDEFDP
      GO TO 1380
!
 1360 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      IX2ZDP=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC LABEL DECIMALS (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1383)
 1383 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 9000
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DECI')GO TO 1450
      IF(IHARG(NUMARG).EQ.'PLAC')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      IHOLD=IDEFDP
      GO TO 1480
!
 1460 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      IY1ZDP=IHOLD
      IY2ZDP=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC LABEL DECIMALS (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1483)
 1483 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 9000
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DECI')GO TO 1550
      IF(IHARG(NUMARG).EQ.'PLAC')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      IHOLD=IDEFDP
      GO TO 1580
!
 1560 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      IY1ZDP=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC LABEL DECIMALS (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1583)
 1583 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 9000
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DECI')GO TO 1650
      IF(IHARG(NUMARG).EQ.'PLAC')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      IHOLD=IDEFDP
      GO TO 1680
!
 1660 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      IY2ZDP=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC LABEL DECIMALS (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1683)
 1683 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 9000
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DECI')GO TO 1750
      IF(IHARG(NUMARG).EQ.'PLAC')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      IHOLD=IDEFDP
      GO TO 1780
!
 1760 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      IX1ZDP=IHOLD
      IX2ZDP=IHOLD
      IY1ZDP=IHOLD
      IY2ZDP=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC LABEL DECIMALS (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAVE JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IHOLD.LT.0)WRITE(ICOUT,1783)
 1783 FORMAT('THAT IS, THEY WILL FLOAT WITH THE VALUE AND BE NEAT.')
      IF(IHOLD.LT.0)CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 9000
!
 1799 CONTINUE
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
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPTCDP
      SUBROUTINE DPTCJU(ICOM,IHARG,NUMARG,   &
      IX1TJU,IX2TJU,IY1TJU,IY2TJU,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK JUSTIFICATION SWITCHES
!              FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK SWITCHES DESCRIBE
!              THE TIC MARK JUSTIFICATION (THRU, IN, OR OUT) ON THE 4 FRAME LINE
!              THE CONTENTS OF A TIC MARK JUSTIFICATION SWITCH ARE
!              A JUSTIFICATION (THRU, IN, OR OUT).
!              THE TIC MARK JUSTIFICATION SWITCHES FOR THE 4 FRAME LINES
!              ARE CONTAINED IN THE 4 VARIABLES
!              IX1TJU,IX2TJU,IY1TJU,IY2TJU
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--IX1TJU = JUSTIFICATION FOR BOTTOM HORIZ. TICS
!                     --IX2TJU = JUSTIFICATION FOR TOP    HORIZ. TICS
!                     --IY1TJU = JUSTIFICATION FOR LEFT   VERT.  TICS
!                     --IY2TJU = JUSTIFICATION FOR RIGHT  VERT.  TICS
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
!     ORIGINAL VERSION--OCTOBER   1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IX1TJU
      CHARACTER*4 IX2TJU
      CHARACTER*4 IY1TJU
      CHARACTER*4 IY2TJU
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
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
      IF(NUMARG.LE.0)GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'POSI')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'POSI')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'POSI')GO TO 1150
      IF(IHARG(NUMARG).EQ.'IN')GO TO 1130
      IF(IHARG(NUMARG).EQ.'INSI')GO TO 1130
      IF(IHARG(NUMARG).EQ.'OUT')GO TO 1140
      IF(IHARG(NUMARG).EQ.'OUTS')GO TO 1140
      IF(IHARG(NUMARG).EQ.'THRO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'THRU')GO TO 1150
      IF(IHARG(NUMARG).EQ.'CENT')GO TO 1150
      IERROR='YES'
      GO TO 1900
!
 1130 CONTINUE
      IFOUND='YES'
      IX1TJU='IN'
      IX2TJU='IN'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 1900
!
 1140 CONTINUE
      IFOUND='YES'
      IX1TJU='OUT'
      IX2TJU='OUT'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1146)
 1146 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 1900
!
 1150 CONTINUE
      IFOUND='YES'
      IX1TJU='THRU'
      IX2TJU='THRU'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1159
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)
 1156 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1159 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'POSI')GO TO 1250
      IF(IHARG(NUMARG).EQ.'IN')GO TO 1230
      IF(IHARG(NUMARG).EQ.'INSI')GO TO 1230
      IF(IHARG(NUMARG).EQ.'OUT')GO TO 1240
      IF(IHARG(NUMARG).EQ.'OUTS')GO TO 1240
      IF(IHARG(NUMARG).EQ.'THRO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'THRU')GO TO 1250
      IF(IHARG(NUMARG).EQ.'CENT')GO TO 1250
      IERROR='YES'
      GO TO 1900
!
 1230 CONTINUE
      IFOUND='YES'
      IX1TJU='IN'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1239
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1235)
 1235 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1236)
 1236 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1239 CONTINUE
      GO TO 1900
!
 1240 CONTINUE
      IFOUND='YES'
      IX1TJU='OUT'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1249
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1245)
 1245 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1246)
 1246 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1249 CONTINUE
      GO TO 1900
!
 1250 CONTINUE
      IFOUND='YES'
      IX1TJU='THRU'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1259
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1255)
 1255 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1256)
 1256 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1259 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'POSI')GO TO 1350
      IF(IHARG(NUMARG).EQ.'IN')GO TO 1330
      IF(IHARG(NUMARG).EQ.'INSI')GO TO 1330
      IF(IHARG(NUMARG).EQ.'OUT')GO TO 1340
      IF(IHARG(NUMARG).EQ.'OUTS')GO TO 1340
      IF(IHARG(NUMARG).EQ.'THRO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'THRU')GO TO 1350
      IF(IHARG(NUMARG).EQ.'CENT')GO TO 1350
      IERROR='YES'
      GO TO 1900
!
 1330 CONTINUE
      IFOUND='YES'
      IX2TJU='IN'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1339
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1335)
 1335 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1336)
 1336 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1339 CONTINUE
      GO TO 1900
!
 1340 CONTINUE
      IFOUND='YES'
      IX2TJU='OUT'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1349
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1345)
 1345 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1346)
 1346 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1349 CONTINUE
      GO TO 1900
!
 1350 CONTINUE
      IFOUND='YES'
      IX2TJU='THRU'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1359
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1355)
 1355 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1356)
 1356 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1359 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'POSI')GO TO 1450
      IF(IHARG(NUMARG).EQ.'IN')GO TO 1430
      IF(IHARG(NUMARG).EQ.'INSI')GO TO 1430
      IF(IHARG(NUMARG).EQ.'OUT')GO TO 1440
      IF(IHARG(NUMARG).EQ.'OUTS')GO TO 1440
      IF(IHARG(NUMARG).EQ.'THRO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'THRU')GO TO 1450
      IF(IHARG(NUMARG).EQ.'CENT')GO TO 1450
      IERROR='YES'
      GO TO 1900
!
 1430 CONTINUE
      IFOUND='YES'
      IY1TJU='IN'
      IY2TJU='IN'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1439
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1435)
 1435 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1436)
 1436 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1439 CONTINUE
      GO TO 1900
!
 1440 CONTINUE
      IFOUND='YES'
      IY1TJU='OUT'
      IY2TJU='OUT'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1449
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1445)
 1445 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1446)
 1446 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1449 CONTINUE
      GO TO 1900
!
 1450 CONTINUE
      IFOUND='YES'
      IY1TJU='THRU'
      IY2TJU='THRU'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1459
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1455)
 1455 FORMAT('THE TIC MARK JUSTIFICATION (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1456)
 1456 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1459 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'POSI')GO TO 1550
      IF(IHARG(NUMARG).EQ.'IN')GO TO 1530
      IF(IHARG(NUMARG).EQ.'INSI')GO TO 1530
      IF(IHARG(NUMARG).EQ.'OUT')GO TO 1540
      IF(IHARG(NUMARG).EQ.'OUTS')GO TO 1540
      IF(IHARG(NUMARG).EQ.'THRO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'THRU')GO TO 1550
      IF(IHARG(NUMARG).EQ.'CENT')GO TO 1550
      IERROR='YES'
      GO TO 1900
!
 1530 CONTINUE
      IFOUND='YES'
      IY1TJU='IN'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1539
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1535)
 1535 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1536)
 1536 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1539 CONTINUE
      GO TO 1900
!
 1540 CONTINUE
      IFOUND='YES'
      IY1TJU='OUT'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1549
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1545)
 1545 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1546)
 1546 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1549 CONTINUE
      GO TO 1900
!
 1550 CONTINUE
      IFOUND='YES'
      IY1TJU='THRU'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1559
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1555)
 1555 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1556)
 1556 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1559 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'POSI')GO TO 1650
      IF(IHARG(NUMARG).EQ.'IN')GO TO 1630
      IF(IHARG(NUMARG).EQ.'INSI')GO TO 1630
      IF(IHARG(NUMARG).EQ.'OUT')GO TO 1640
      IF(IHARG(NUMARG).EQ.'OUTS')GO TO 1640
      IF(IHARG(NUMARG).EQ.'THRO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'THRU')GO TO 1650
      IF(IHARG(NUMARG).EQ.'CENT')GO TO 1650
      IERROR='YES'
      GO TO 1900
!
 1630 CONTINUE
      IFOUND='YES'
      IY2TJU='IN'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1639
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1635)
 1635 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1636)
 1636 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1639 CONTINUE
      GO TO 1900
!
 1640 CONTINUE
      IFOUND='YES'
      IY2TJU='OUT'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1649
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1645)
 1645 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1646)
 1646 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1649 CONTINUE
      GO TO 1900
!
 1650 CONTINUE
      IFOUND='YES'
      IY2TJU='THRU'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1659
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1655)
 1655 FORMAT('THE TIC MARK JUSTIFICATION (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1656)
 1656 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1659 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'POSI')GO TO 1750
      IF(IHARG(NUMARG).EQ.'IN')GO TO 1730
      IF(IHARG(NUMARG).EQ.'INSI')GO TO 1730
      IF(IHARG(NUMARG).EQ.'OUT')GO TO 1740
      IF(IHARG(NUMARG).EQ.'OUTS')GO TO 1740
      IF(IHARG(NUMARG).EQ.'THRO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'THRU')GO TO 1750
      IF(IHARG(NUMARG).EQ.'CENT')GO TO 1750
      IERROR='YES'
      GO TO 1900
!
 1730 CONTINUE
      IFOUND='YES'
      IX1TJU='IN'
      IX2TJU='IN'
      IY1TJU='IN'
      IY2TJU='IN'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1739
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1735)
 1735 FORMAT('THE TIC MARKS (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1736)
 1736 FORMAT('HAS JUST BEEN SET TO      INSIDE ')
      CALL DPWRST('XXX','BUG ')
 1739 CONTINUE
      GO TO 1900
!
 1740 CONTINUE
      IFOUND='YES'
      IX1TJU='OUT'
      IX2TJU='OUT'
      IY1TJU='OUT'
      IY2TJU='OUT'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1749
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1745)
 1745 FORMAT('THE TIC MARKS (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1746)
 1746 FORMAT('HAS JUST BEEN SET TO      OUTSIDE ')
      CALL DPWRST('XXX','BUG ')
 1749 CONTINUE
      GO TO 1900
!
 1750 CONTINUE
      IFOUND='YES'
      IX1TJU='THRU'
      IX2TJU='THRU'
      IY1TJU='THRU'
      IY2TJU='THRU'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1759
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1755)
 1755 FORMAT('THE TIC MARKS (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1756)
 1756 FORMAT('HAS JUST BEEN SET TO      THROUGH ')
      CALL DPWRST('XXX','BUG ')
 1759 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTCJU
      SUBROUTINE DPTCOF(ICOM,IHARG,IARGT,ARG,NUMARG,        &
                        DEFTOF,IDEFTU,                      &
                        ITICX1,ITICX2,ITICY1,ITICY2,        &
                        PX1TOL,PX2TOL,PY1TOB,PY2TOB,        &
                        PX1TOR,PX2TOR,PY1TOT,PY2TOT,        &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK OFFSETS FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK OFFSETS DEFINE THE DISTANCE (IN EITHER
!              DATA UNITS OR DATAPLOT PERCENT UNITS) FROM THE FIRST OR
!              LAST TIC MARK TO THE FRAME LIMIT.  NOTE THAT THIS VALUE
!              WILL BE ADDED TO THE CURRENT DATA LIMITS (EITHER DEFINED
!              VIA THE LIMITS COMMAND OR AS AUTOMATICALLY DETERMINED
!              BY DATAPLOT).
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG
!                     --DEFTOF = DEFAULT OFFSET
!                     --IDEFTU = DEFAULT TIC UNITS
!     OUTPUT ARGUMENTS--
!                     --PX1TOL = BOTTOM HORIZONTAL TIC LEFT OFFSET
!                     --PX2TOL = TOP    HORIZONTAL TIC LEFT OFFSET
!                     --PY1TOB = LEFT   VERTICAL   TIC BOTTOM OFFSET
!                     --PY2TOB = RIGHT  VERTICAL   TIC BOTTOM OFFSET
!                     --PX1TOL = BOTTOM HORIZONTAL TIC LEFT OFFSET
!                     --PX2TOL = TOP    HORIZONTAL TIC LEFT OFFSET
!                     --PY1TOB = LEFT   VERTICAL   TIC BOTTOM OFFSET
!                     --PY2TOB = RIGHT  VERTICAL   TIC BOTTOM OFFSET
!                     --ITICX1 = BOTTOM HORIZONTAL TIC UNITS
!                     --ITICX2 = TOP    HORIZONTAL TIC UNITS
!                     --ITICY1 = LEFT   VERTICAL   TIC UNITS
!                     --ITICY2 = RIGHT  VERTICAL   TIC UNITS
!                     --ITICY2 = RIGHT  VERTICAL   TIC UNITS
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
!     VERSION NUMBER--90/5
!     ORIGINAL VERSION--MAY        1990.
!     UPDATED         --OCTOBER    1991. INSERT FEEDBACK OFF JUMP
!     UPDATED         --NOVEMBER   2025. SEPARATE VALUES FOR TIC
!                                        OFFSET UNITS FOR THE 4 FRAME
!                                        LINES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 ITICX1
      CHARACTER*4 ITICX2
      CHARACTER*4 ITICY1
      CHARACTER*4 ITICY2
      CHARACTER*4 IDEFTU
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
!
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.0)THEN
        GO TO 1900
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'OFFS'.AND.   &
         IHARG(2).EQ.'UNIT')THEN
        GO TO 2090
      ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
             IHARG(2).EQ.'OFFS'.AND.IHARG(3).EQ.'UNIT')THEN
        GO TO 2090
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'OFFS')THEN
        GO TO 1090
      ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
             IHARG(2).EQ.'OFFS')THEN
        GO TO 1090
      ELSE
        GO TO 1900
      ENDIF
!
 1090 CONTINUE
      IFOUND='YES'
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')THEN
        ILEFT=2
        IF(IHARG(2).EQ.'OFFS')ILEFT=3
        IRIGHT=ILEFT+1
        IF(ILEFT.GT.NUMARG)ILEFT=0
        IF(IRIGHT.GT.NUMARG)IRIGHT=0
!
!               *****************************************************
!               **  TREAT THE LEFT OFFSET                          **
!               **  NO ARGUMENT WILL SET THE DEFAULT               **
!               *****************************************************
!
        IF(ILEFT.EQ.0 .OR. IHARG(ILEFT).EQ.'ON' .OR.                 &
           IHARG(ILEFT).EQ.'OFF' .OR. IHARG(ILEFT).EQ.'AUTO' .OR.    &
           IHARG(ILEFT).EQ.'DEFA' .OR.  IHARG(ILEFT).EQ.'FLOA') THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(ILEFT).EQ.'NUMB')THEN
          HOLD=ARG(ILEFT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PX1TOL=HOLD
        PX2TOL=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
 1141     FORMAT('THE TIC MARK LEFT OFFSET (FOR BOTH HORIZONTAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)HOLD
 1142     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *****************************************************
!               **  TREAT THE RIGHT OFFSET                         **
!               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
!               *****************************************************
!
        IF(IRIGHT.EQ.0)THEN
          HOLD=PX1TOR
        ELSEIF(IHARG(IRIGHT).EQ.'ON' .OR. IHARG(IRIGHT).EQ.'OFF' .OR. &
           IHARG(IRIGHT).EQ.'AUTO' .OR. IHARG(IRIGHT).EQ.'DEFA' .OR.  &
           IHARG(IRIGHT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(IRIGHT).EQ.'NUMB')THEN
          HOLD=ARG(IRIGHT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PX1TOR=HOLD
        PX2TOR=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1191)
 1191     FORMAT('THE TIC MARK RIGHT OFFSET (FOR BOTH HORIZONTAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1192)HOLD
 1192     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
!
!               ********************************************************
!               **  TREAT THE CASE WHEN ONLY THE BOTTOM HORIZONTAL    **
!               **  TIC MARKS ARE TO BE CHANGED                       **
!               ********************************************************
!
      ELSEIF(ICOM.EQ.'X1TI')THEN
!
        ILEFT=2
        IF(IHARG(2).EQ.'OFFS')ILEFT=3
        IRIGHT=ILEFT+1
        IF(ILEFT.GT.NUMARG)ILEFT=0
        IF(IRIGHT.GT.NUMARG)IRIGHT=0
!
!               *****************************************************
!               **  TREAT THE LEFT OFFSET                          **
!               **  NO ARGUMENT WILL SET THE DEFAULT               **
!               *****************************************************
!
        IF(ILEFT.EQ.0 .OR. IHARG(ILEFT).EQ.'ON' .OR.              &
           IHARG(ILEFT).EQ.'OFF' .OR. IHARG(ILEFT).EQ.'AUTO' .OR. &
           IHARG(ILEFT).EQ.'DEFA' .OR. IHARG(ILEFT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(ILEFT).EQ.'NUMB')THEN
          HOLD=ARG(ILEFT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PX1TOL=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1241)
 1241     FORMAT('THE TIC MARK LEFT OFFSET (FOR BOTTOM HORIZONTAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1242)HOLD
 1242     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *****************************************************
!               **  TREAT THE RIGHT OFFSET                         **
!               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
!               *****************************************************
!
        IF(IRIGHT.EQ.0)THEN
          HOLD=PX2TOR
        ELSEIF(IHARG(IRIGHT).EQ.'ON' .OR. IHARG(IRIGHT).EQ.'OFF' .OR.   &
               IHARG(IRIGHT).EQ.'AUTO' .OR. IHARG(IRIGHT).EQ.'DEFA' .OR.&
               IHARG(IRIGHT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(IRIGHT).EQ.'NUMB')THEN
          HOLD=ARG(IRIGHT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PX1TOR=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1291)
 1291     FORMAT('THE TIC MARK RIGHT OFFSET (FOR BOTTOM HORIZONTAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1292)HOLD
 1292     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
!
!               ********************************************************
!               **  TREAT THE CASE WHEN ONLY THE TOP HORIZONTAL TIC   **
!               **  MARKS ARE TO BE CHANGED                           **
!               ********************************************************
!
      ELSEIF(ICOM.EQ.'X2TI')THEN
!
        ILEFT=2
        IF(IHARG(2).EQ.'OFFS')ILEFT=3
        IRIGHT=ILEFT+1
        IF(ILEFT.GT.NUMARG)ILEFT=0
        IF(IRIGHT.GT.NUMARG)IRIGHT=0
!
!               *****************************************************
!               **  TREAT THE LEFT OFFSET                          **
!               **  NO ARGUMENT WILL SET THE DEFAULT               **
!               *****************************************************
!
        IF(ILEFT.EQ.0 .OR. IHARG(ILEFT).EQ.'ON' .OR.                 &
           IHARG(ILEFT).EQ.'OFF' .OR. IHARG(ILEFT).EQ.'AUTO' .OR.   &
           IHARG(ILEFT).EQ.'DEFA' .OR. IHARG(ILEFT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(ILEFT).EQ.'NUMB')THEN
          HOLD=ARG(ILEFT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PX2TOL=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1341)
 1341     FORMAT('THE TIC MARK LEFT OFFSET (FOR TOP HORIZONTAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1342)HOLD
 1342     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *****************************************************
!               **  TREAT THE RIGHT OFFSET                         **
!               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
!               *****************************************************
!
        IF(IRIGHT.EQ.0)THEN
          HOLD=PX2TOR
        ELSEIF(IHARG(IRIGHT).EQ.'ON' .OR. IHARG(IRIGHT).EQ.'AUTO' .OR.  &
               IHARG(IRIGHT).EQ.'OFF' .OR. IHARG(IRIGHT).EQ.'DEFA' .OR. &
               IHARG(IRIGHT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(IRIGHT).EQ.'NUMB')THEN
          HOLD=ARG(IRIGHT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PX2TOR=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1391)
 1391     FORMAT('THE TIC MARK RIGHT OFFSET (FOR TOP HORIZONTAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1392)HOLD
 1392     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      ELSEIF(ICOM.EQ.'YTIC')THEN
!
        ILEFT=2
        IF(IHARG(2).EQ.'OFFS')ILEFT=3
        IRIGHT=ILEFT+1
        IF(ILEFT.GT.NUMARG)ILEFT=0
        IF(IRIGHT.GT.NUMARG)IRIGHT=0
!
!               *****************************************************
!               **  TREAT THE BOTTOM OFFSET                        **
!               **  NO ARGUMENT WILL SET THE DEFAULT               **
!               *****************************************************
!
        IF(ILEFT.EQ.0 .OR. IHARG(ILEFT).EQ.'ON' .OR.                   &
           IHARG(ILEFT).EQ.'OFF' .OR. IHARG(ILEFT).EQ.'AUTO' .OR.      &
           IHARG(ILEFT).EQ.'DEFA' .OR. IHARG(ILEFT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(ILEFT).EQ.'NUMB')THEN
          HOLD=ARG(ILEFT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PY1TOB=HOLD
        PY2TOB=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1441)
 1441     FORMAT('THE TIC MARK BOTTOM OFFSET (FOR BOTH VERTICAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1442)HOLD
 1442     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *****************************************************
!               **  TREAT THE TOP OFFSET                           **
!               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
!               *****************************************************
!
        IF(IRIGHT.EQ.0)THEN
          HOLD=PY1TOT
        ELSEIF(IHARG(IRIGHT).EQ.'ON' .OR. IHARG(IRIGHT).EQ.'AUTO' .OR.  &
               IHARG(IRIGHT).EQ.'OFF' .OR. IHARG(IRIGHT).EQ.'DEFA' .OR. &
               IHARG(IRIGHT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(IRIGHT).EQ.'NUMB')THEN
          HOLD=ARG(IRIGHT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PY1TOT=HOLD
        PY2TOT=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1491)
 1491     FORMAT('THE TIC MARK TOP OFFSET (FOR BOTH VERTICAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1492)HOLD
 1492     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
!
!               *********************************************************
!               **  TREAT THE CASE WHEN ONLY THE LEFT VERTICAL TIC     **
!               **  OFFSETS ARE TO BE CHANGED                          **
!               *********************************************************
!
      ELSEIF(ICOM.EQ.'Y1TI')THEN
!
        ILEFT=2
        IF(IHARG(2).EQ.'OFFS')ILEFT=3
        IRIGHT=ILEFT+1
        IF(ILEFT.GT.NUMARG)ILEFT=0
        IF(IRIGHT.GT.NUMARG)IRIGHT=0
!
!               *****************************************************
!               **  TREAT THE BOTTOM OFFSET                        **
!               **  NO ARGUMENT WILL SET THE DEFAULT               **
!               *****************************************************
!
        IF(ILEFT.EQ.0 .OR. IHARG(ILEFT).EQ.'ON' .OR.               &
           IHARG(ILEFT).EQ.'OFF' .OR. IHARG(ILEFT).EQ.'AUTO' .OR.  &
           IHARG(ILEFT).EQ.'DEFA' .OR. IHARG(ILEFT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(ILEFT).EQ.'NUMB')THEN
          HOLD=ARG(ILEFT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PY1TOB=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1541)
 1541     FORMAT('THE TIC MARK BOTTOM OFFSET (FOR LEFT VERTICAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1542)HOLD
 1542     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *****************************************************
!               **  TREAT THE TOP OFFSET                           **
!               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
!               *****************************************************
!
        IF(IRIGHT.EQ.0)THEN
          HOLD=PY1TOT
        ELSEIF(IHARG(IRIGHT).EQ.'ON' .OR.  IHARG(IRIGHT).EQ.'AUTO' .OR. &
               IHARG(IRIGHT).EQ.'OFF' .OR. IHARG(IRIGHT).EQ.'DEFA' .OR. &
               IHARG(IRIGHT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(IRIGHT).EQ.'NUMB')THEN
          HOLD=ARG(IRIGHT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PY1TOT=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1591)
 1591     FORMAT('THE TIC MARK TOP OFFSET (FOR LEFT VERTICAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1592)HOLD
 1592     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
!
!               ********************************************************
!               **  TREAT THE CASE WHEN ONLY THE RIGHT VERTICAL TIC   **
!               **  OFFSETS ARE TO BE CHANGED                         **
!               ********************************************************
!
      ELSEIF(ICOM.EQ.'Y2TI')THEN
!
        ILEFT=2
        IF(IHARG(2).EQ.'OFFS')ILEFT=3
        IRIGHT=ILEFT+1
        IF(ILEFT.GT.NUMARG)ILEFT=0
        IF(IRIGHT.GT.NUMARG)IRIGHT=0
!
!               *****************************************************
!               **  TREAT THE BOTTOM OFFSET                        **
!               **  NO ARGUMENT WILL SET THE DEFAULT               **
!               *****************************************************
!
        IF(ILEFT.EQ.0 .OR. IHARG(ILEFT).EQ.'ON' .OR.              &
           IHARG(ILEFT).EQ.'OFF' .OR. IHARG(ILEFT).EQ.'AUTO' .OR. &
           IHARG(ILEFT).EQ.'DEFA' .OR. IHARG(ILEFT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(ILEFT).EQ.'NUMB')THEN
          HOLD=ARG(ILEFT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PY2TOB=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1641)
 1641     FORMAT('THE TIC MARK BOTTOM OFFSET (FOR RIGHT VERTICAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1642)HOLD
 1642     FORMAT('HAS JUST BEEN SET TO ',E16.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *****************************************************
!               **  TREAT THE TOP OFFSET                           **
!               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
!               *****************************************************
!
        IF(IRIGHT.EQ.0)THEN
          HOLD=PY2TOT
        ELSEIF(IHARG(IRIGHT).EQ.'ON' .OR. IHARG(IRIGHT).EQ.'AUTO' .OR.  &
               IHARG(IRIGHT).EQ.'OFF' .OR. IHARG(IRIGHT).EQ.'DEFA' .OR. &
               IHARG(IRIGHT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(IRIGHT).EQ.'NUMB')THEN
          HOLD=ARG(IRIGHT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PY2TOT=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1691)
 1691     FORMAT('THE TIC MARK TOP OFFSET (FOR RIGHT VERTICAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1692)HOLD
 1692     FORMAT('HAS JUST BEEN SET TO ',E16.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      ELSEIF(ICOM.EQ.'TIC' .OR. ICOM.EQ.'TICS' .OR.   &
             ICOM.EQ.'XYTI' .OR.  ICOM.EQ.'YXTI')THEN
!
        ILEFT=2
        IF(IHARG(2).EQ.'OFFS')ILEFT=3
        IRIGHT=ILEFT+1
        IF(ILEFT.GT.NUMARG)ILEFT=0
        IF(IRIGHT.GT.NUMARG)IRIGHT=0
!
!               *****************************************************
!               **  TREAT THE BOTTOM OFFSET                        **
!               **  NO ARGUMENT WILL SET THE DEFAULT               **
!               *****************************************************
!
        IF(ILEFT.EQ.0 .OR. IHARG(ILEFT).EQ.'ON' .OR.              &
           IHARG(ILEFT).EQ.'OFF' .OR. IHARG(ILEFT).EQ.'AUTO' .OR. &
           IHARG(ILEFT).EQ.'DEFA' .OR. IHARG(ILEFT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(ILEFT).EQ.'NUMB')THEN
          HOLD=ARG(ILEFT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PX1TOL=HOLD
        PX2TOL=HOLD
        PY1TOB=HOLD
        PY2TOB=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1741)
 1741     FORMAT('THE TIC MARK BOTTOM OFFSET (FOR BOTH VERTICAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1742)HOLD
 1742     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1743)
 1743     FORMAT('THE TIC MARK LEFT OFFSET (FOR BOTH HORIZONTAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1742)HOLD
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               *****************************************************
!               **  TREAT THE TOP OFFSET                           **
!               **  NO ARGUMENT WILL LEAVE THE CURRENT VALUE       **
!               *****************************************************
!
        IF(IRIGHT.EQ.0)THEN
          HOLD=DEFTOF
        ELSEIF(IHARG(IRIGHT).EQ.'ON' .OR. IHARG(IRIGHT).EQ.'AUTO' .OR.  &
               IHARG(IRIGHT).EQ.'OFF' .OR. IHARG(IRIGHT).EQ.'DEFA' .OR. &
               IHARG(IRIGHT).EQ.'FLOA')THEN
          HOLD=DEFTOF
        ELSEIF(IARGT(IRIGHT).EQ.'NUMB')THEN
          HOLD=ARG(IRIGHT)
        ELSE
          IERROR='YES'
          GO TO 1900
        ENDIF
!
        IFOUND='YES'
        HOLD=ABS(HOLD)
        PX1TOR=HOLD
        PX2TOR=HOLD
        PY1TOT=HOLD
        PY2TOT=HOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1791)
 1791     FORMAT('THE TIC MARK TOP OFFSET (FOR BOTH VERTICAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1792)HOLD
 1792     FORMAT('HAS JUST BEEN SET TO ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1793)
 1793     FORMAT('THE TIC MARK RIGHT OFFSET (FOR BOTH HORIZONTAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1792)HOLD
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
      ENDIF
      GO TO 1900
!
!               *****************************************************
!               **  TREAT THE OFFSET UNITS CASE.  THE CHOICES ARE  **
!               **  "DATA", (OFFSETS IN UNITS OF THE DATA) AND     **
!               **  "ABSOLUTE" (OFFSETS IN DATAPLOT 0. TO 100.     **
!               **  PERCENT UNITS).                                **
!               *****************************************************
!
 2090 CONTINUE
      IFOUND='YES'
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC' .AND.                                 &
        ((IHARG(1).EQ.'OFFS' .AND. IHARG(2).EQ.'UNIT') .OR.   &
         (IHARG(2).EQ.'OFFS' .AND. IHARG(3).EQ.'UNIT')))THEN
        ILEFT=3
        IF(IHARG(3).EQ.'UNIT')ILEFT=4
        IF(ILEFT.GT.NUMARG)ILEFT=0
!
        IF(ILEFT.GT.0 .AND. IHARG(ILEFT).EQ.'DATA')THEN
          IHOLD='DATA'
        ELSEIF(ILEFT.GT.0 .AND.    &
              (IHARG(ILEFT).EQ.'SCRE' .OR. IHARG(ILEFT).EQ.'ABSO'))THEN
          IHOLD='ABSO'
        ELSE
          IHOLD=IDEFTU
        ENDIF
!
        IFOUND='YES'
        ITICX1=IHOLD
        ITICX2=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2141)
 2141     FORMAT('THE TIC MARK OFFSET UNITS (FOR BOTH HORIZONTAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2142)IHOLD
 2142     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
!
!               ********************************************************
!               **  TREAT THE CASE WHEN ONLY THE BOTTOM HORIZONTAL    **
!               **  TIC OFFSET UNITS ARE TO BE CHANGED                **
!               ********************************************************
!
      ELSEIF(ICOM.EQ.'X1TI' .AND.                                 &
            ((IHARG(1).EQ.'OFFS' .AND. IHARG(2).EQ.'UNIT') .OR.   &
             (IHARG(2).EQ.'OFFS' .AND. IHARG(3).EQ.'UNIT')))THEN
        ILEFT=3
        IF(IHARG(3).EQ.'UNIT')ILEFT=4
        IF(ILEFT.GT.NUMARG)ILEFT=0
!
        IF(ILEFT.GT.0 .AND. IHARG(ILEFT).EQ.'DATA')THEN
          IHOLD='DATA'
        ELSEIF(ILEFT.GT.0 .AND.    &
              (IHARG(ILEFT).EQ.'SCRE' .OR. IHARG(ILEFT).EQ.'ABSO'))THEN
          IHOLD='ABSO'
        ELSE
          IHOLD=IDEFTU
        ENDIF
!
        IFOUND='YES'
        ITICX1=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2241)
 2241     FORMAT('THE TIC MARK OFFSET UNITS (FOR BOTTOM HORIZONTAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2242)IHOLD
 2242     FORMAT('HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        GO TO 1900
!
!               ********************************************************
!               **  TREAT THE CASE WHEN ONLY THE TOP HORIZONTAL TIC   **
!               **  MARKS ARE TO BE CHANGED                           **
!               ********************************************************
!
      ELSEIF(ICOM.EQ.'X2TI' .AND.                                 &
            ((IHARG(1).EQ.'OFFS' .AND. IHARG(2).EQ.'UNIT') .OR.   &
             (IHARG(2).EQ.'OFFS' .AND. IHARG(3).EQ.'UNIT')))THEN
        ILEFT=3
        IF(IHARG(3).EQ.'UNIT')ILEFT=4
        IF(ILEFT.GT.NUMARG)ILEFT=0
!
        IF(ILEFT.GT.0 .AND. IHARG(ILEFT).EQ.'DATA')THEN
          IHOLD='DATA'
        ELSEIF(ILEFT.GT.0 .AND.    &
              (IHARG(ILEFT).EQ.'SCRE' .OR. IHARG(ILEFT).EQ.'ABSO'))THEN
          IHOLD='ABSO'
        ELSE
          IHOLD=IDEFTU
        ENDIF
!
        IFOUND='YES'
        ITICX2=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2341)
 2341     FORMAT('THE TIC MARK OFFSET UNITS (FOR TOP HORIZONTAL ',   &
                 'FRAME LINE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2142)IHOLD
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 1900
!
!               ******************************************************
!               **  TREAT THE CASE WHEN                             **
!               **  BOTH VERTICAL AXIS TIC UNITS ARE TO BE CHANGED  **
!               ******************************************************
!
      ELSEIF(ICOM.EQ.'YTIC' .AND.                                 &
            ((IHARG(1).EQ.'OFFS' .AND. IHARG(2).EQ.'UNIT') .OR.   &
             (IHARG(2).EQ.'OFFS' .AND. IHARG(3).EQ.'UNIT')))THEN
        ILEFT=3
        IF(IHARG(3).EQ.'UNIT')ILEFT=4
        IF(ILEFT.GT.NUMARG)ILEFT=0
!
        IF(ILEFT.GT.0 .AND. IHARG(ILEFT).EQ.'DATA')THEN
          IHOLD='DATA'
        ELSEIF(ILEFT.GT.0 .AND.    &
              (IHARG(ILEFT).EQ.'SCRE' .OR. IHARG(ILEFT).EQ.'ABSO'))THEN
          IHOLD='ABSO'
        ELSE
          IHOLD=IDEFTU
        ENDIF
!
        IFOUND='YES'
        ITICY1=IHOLD
        ITICY2=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2441)
 2441     FORMAT('THE TIC MARK OFFSET UNITS (FOR BOTH VERTICAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2142)IHOLD
          CALL DPWRST('XXX','BUG ')
        ENDIF

        GO TO 1900
!
!               *********************************************************
!               **  TREAT THE CASE WHEN ONLY THE LEFT VERTICAL TIC     **
!               **  OFFSET UNITS ARE TO BE CHANGED                     **
!               *********************************************************
!
      ELSEIF(ICOM.EQ.'Y1TI' .AND.                                 &
            ((IHARG(1).EQ.'OFFS' .AND. IHARG(2).EQ.'UNIT') .OR.   &
             (IHARG(2).EQ.'OFFS' .AND. IHARG(3).EQ.'UNIT')))THEN
        ILEFT=3
        IF(IHARG(3).EQ.'UNIT')ILEFT=4
        IF(ILEFT.GT.NUMARG)ILEFT=0
!
        IF(ILEFT.GT.0 .AND. IHARG(ILEFT).EQ.'DATA')THEN
          IHOLD='DATA'
        ELSEIF(ILEFT.GT.0 .AND.    &
              (IHARG(ILEFT).EQ.'SCRE' .OR. IHARG(ILEFT).EQ.'ABSO'))THEN
          IHOLD='ABSO'
        ELSE
          IHOLD=IDEFTU
        ENDIF
!
        IFOUND='YES'
        ITICY1=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2541)
 2541     FORMAT('THE TIC MARK OFFSET UNITS (FOR THE LEFT VERTICAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2142)IHOLD
          CALL DPWRST('XXX','BUG ')
        ENDIF

        GO TO 1900
!
!               ********************************************************
!               **  TREAT THE CASE WHEN ONLY THE RIGHT VERTICAL TIC   **
!               **  OFFSET UNITS ARE TO BE CHANGED                    **
!               ********************************************************
!
      ELSEIF(ICOM.EQ.'Y2TI' .AND.                                 &
            ((IHARG(1).EQ.'OFFS' .AND. IHARG(2).EQ.'UNIT') .OR.   &
             (IHARG(2).EQ.'OFFS' .AND. IHARG(3).EQ.'UNIT')))THEN
        ILEFT=3
        IF(IHARG(3).EQ.'UNIT')ILEFT=4
        IF(ILEFT.GT.NUMARG)ILEFT=0
!
        IF(ILEFT.GT.0 .AND. IHARG(ILEFT).EQ.'DATA')THEN
          IHOLD='DATA'
        ELSEIF(ILEFT.GT.0 .AND.    &
              (IHARG(ILEFT).EQ.'SCRE' .OR. IHARG(ILEFT).EQ.'ABSO'))THEN
          IHOLD='ABSO'
        ELSE
          IHOLD=IDEFTU
        ENDIF
!
        IFOUND='YES'
        ITICY2=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2641)
 2641     FORMAT('THE TIC MARK OFFSET UNITS (FOR THE RIGHT VERTICAL ',   &
                 'FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2142)IHOLD
          CALL DPWRST('XXX','BUG ')
        ENDIF

        GO TO 1900
!
!               *****************************************************
!               **  TREAT THE CASE WHEN ALL 4 FRAME TIC UNITS ARE  **
!               **  TO BE CHANGED                                  **
!               *****************************************************
!
      ELSEIF((ICOM.EQ.'TIC'  .OR. ICOM.EQ.'TICS' .OR.         &
              ICOM.EQ.'XTTI' .OR. ICOM.EQ.'YXTI') .AND.       &
            ((IHARG(1).EQ.'OFFS' .AND. IHARG(2).EQ.'UNIT') .OR.   &
             (IHARG(2).EQ.'OFFS' .AND. IHARG(3).EQ.'UNIT')))THEN
        ILEFT=3
        IF(IHARG(3).EQ.'UNIT')ILEFT=4
        IF(ILEFT.GT.NUMARG)ILEFT=0
!
        IF(ILEFT.GT.0 .AND. IHARG(ILEFT).EQ.'DATA')THEN
          IHOLD='DATA'
        ELSEIF(ILEFT.GT.0 .AND.    &
              (IHARG(ILEFT).EQ.'SCRE' .OR. IHARG(ILEFT).EQ.'ABSO'))THEN
          IHOLD='ABSO'
        ELSE
          IHOLD=IDEFTU
        ENDIF
!
        IFOUND='YES'
        ITICX1=IHOLD
        ITICX2=IHOLD
        ITICY1=IHOLD
        ITICY2=IHOLD
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2741)
 2741     FORMAT('THE TIC MARK OFFSET UNITS (FOR ALL 4 FRAME LINES)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2142)IHOLD
          CALL DPWRST('XXX','BUG ')
        ENDIF

        GO TO 1900
      ENDIF
      GO TO 1900
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTCOF
      SUBROUTINE DPTCPA(ICOM,IHARG,NUMARG,IDEFPA,      &
                        IX1TPA,IX2TPA,IY1TPA,IY2TPA,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK PATTERN SWITCHES
!              FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK SWITCHES DESCRIBE
!              THE TIC MARK PATTERN ON THE 4 FRAME LINES OF A PLOT.
!              THE CONTENTS OF A TIC MARK PATTERN SWITCH ARE
!              A PATTERN.
!              THE TIC MARK PATTERN SWITCHES FOR THE 4 FRAME LINES
!              ARE CONTAINED IN THE 4 VARIABLES
!              IX1TPA,IX2TPA,IY1TPA,IY2TPA
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFPA
!     OUTPUT ARGUMENTS--IX1TPA = PATTERN FOR BOTTOM HORIZ. TICS
!                     --IX2TPA = PATTERN FOR TOP    HORIZ. TICS
!                     --IY1TPA = PATTERN FOR LEFT   VERT.  TICS
!                     --IY2TPA = PATTERN FOR RIGHT  VERT.  TICS
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
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IDEFPA
!
      CHARACTER*4 IX1TPA
      CHARACTER*4 IX2TPA
      CHARACTER*4 IY1TPA
      CHARACTER*4 IY2TPA
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
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
      IF(NUMARG.LE.0)GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'PATT')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'PATT')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      IHOLD=IDEFPA
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IX1TPA=IHOLD
      IX2TPA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK PATTERN (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'PATT')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      IHOLD=IDEFPA
      GO TO 1280
!
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      IX1TPA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK PATTERN (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'PATT')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      IHOLD=IDEFPA
      GO TO 1380
!
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      IX2TPA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK PATTERN (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'PATT')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      IHOLD=IDEFPA
      GO TO 1480
!
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      IY1TPA=IHOLD
      IY2TPA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK PATTERN (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'PATT')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      IHOLD=IDEFPA
      GO TO 1580
!
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      IY1TPA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK PATTERN (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'PATT')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      IHOLD=IDEFPA
      GO TO 1680
!
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      IY2TPA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK PATTERN (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'PATT')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      IHOLD=IDEFPA
      GO TO 1780
!
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      IX1TPA=IHOLD
      IX2TPA=IHOLD
      IY1TPA=IHOLD
      IY2TPA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK PATTERN (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTCPA
      SUBROUTINE DPTCSZ(ICOM,IHARG,IARGT,ARG,NUMARG,   &
      DEFTL,   &
      PX1TLE,PX2TLE,PY1TLE,PY2TLE,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK SIZES
!              FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK SWITCHES DEFINE THE SIZE (LENGTH)
!              OF THE MAJOR TIC MARKS ON THE 4 FRAME LINES OF A PLOT.
!              (THE SIZE OF THE MINOR TIC MARKS IS ALWAYS
!              1/2 THE SIZE OF THE MAJOR TIC MARKS.)
!              THE TIC MARK SIZE SWITCHES FOR THE 4 FRAME LINES
!              ARE CONTAINED IN THE 4 VARIABLES
!              PX1TLE,PX2TLE,PY1TLE,PY2TLE,
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG
!                     --DEFTL
!     OUTPUT ARGUMENTS--
!                     --PX1TLE = BOTTOM HORIZONTAL TIC LENGTH
!                     --PX2TLE = TOP    HORIZONTAL TIC LENGTH
!                     --PY1TLE = LEFT   VERTICAL   TIC LENGTH
!                     --PY2TLE = RIGHT  VERTICAL   TIC LENGTH
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
!     ORIGINAL VERSION--OCTOBER   1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIZE')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'SIZE')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      IERROR='YES'
      GO TO 1900
!
 1150 CONTINUE
      HOLD=DEFTL
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PX1TLE=HOLD
      PX2TLE=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK SIZE (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)HOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1250
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1260
      IERROR='YES'
      GO TO 1900
!
 1250 CONTINUE
      HOLD=DEFTL
      GO TO 1280
!
 1260 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      PX1TLE=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK SIZE (FOR THE BOTTOM HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)HOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1350
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1360
      IERROR='YES'
      GO TO 1900
!
 1350 CONTINUE
      HOLD=DEFTL
      GO TO 1380
!
 1360 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      PX2TLE=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK SIZE (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)HOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1450
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1460
      IERROR='YES'
      GO TO 1900
!
 1450 CONTINUE
      HOLD=DEFTL
      GO TO 1480
!
 1460 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      PY1TLE=HOLD
      PY2TLE=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK SIZE (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)HOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1550
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1560
      IERROR='YES'
      GO TO 1900
!
 1550 CONTINUE
      HOLD=DEFTL
      GO TO 1580
!
 1560 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      PY1TLE=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK SIZE (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)HOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1650
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1660
      IERROR='YES'
      GO TO 1900
!
 1650 CONTINUE
      HOLD=DEFTL
      GO TO 1680
!
 1660 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      PY2TLE=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK SIZE (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)HOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'SIZE')GO TO 1750
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1760
      IERROR='YES'
      GO TO 1900
!
 1750 CONTINUE
      HOLD=DEFTL
      GO TO 1780
!
 1760 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      PX1TLE=HOLD
      PX2TLE=HOLD
      PY1TLE=HOLD
      PY2TLE=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK SIZE (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)HOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTCSZ
      SUBROUTINE DPTCTH(ICOM,IHARG,ARG,NUMARG,   &
      PDEFTH,   &
      PTICTH,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TIC MARK THICKNESS SWITCHES
!              FOR ANY OF THE 4 FRAME LINES.
!              SUCH TIC MARK SWITCHES DESCRIBE
!              THE TIC MARK THICKNESS ON THE 4 FRAME LINES OF A PLOT.
!              THE CONTENTS OF A TIC MARK THICKNESS SWITCH ARE
!              A THICKNESS.
!              CURRENTLY, THE TIC MARK THICKNESS FOR ALL 4 SIDES
!              MUST BE THE SAME AND ARE CONTAINED IN THE VARIABLE
!              PTICTH
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --ARG    (A REAL VECTOR)
!                     --NUMARG
!                     --PDEFTH
!     OUTPUT ARGUMENTS--PTICTH = THICKNESS FOR ALL 4 FRAME SIDE TICS
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
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      REAL        PDEFTH
!
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      REAL        PHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION ARG(*)
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
      IF(NUMARG.LE.0)GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')GO TO 1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
      IHARG(2).EQ.'THIC')GO TO 1090
      GO TO 1900
 1090 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      PHOLD=PDEFTH
      GO TO 1180
!
 1160 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARK THICKNESS (FOR ALL  ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1250
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1250
      GO TO 1260
!
 1250 CONTINUE
      PHOLD=PDEFTH
      GO TO 1280
!
 1260 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARK THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)PHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1350
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1350
      GO TO 1360
!
 1350 CONTINUE
      PHOLD=PDEFTH
      GO TO 1380
!
 1360 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARK THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)PHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1450
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1450
      GO TO 1460
!
 1450 CONTINUE
      PHOLD=PDEFTH
      GO TO 1480
!
 1460 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARK THICKNESS (FOR ALL',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)PHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1550
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1550
      GO TO 1560
!
 1550 CONTINUE
      PHOLD=PDEFTH
      GO TO 1580
!
 1560 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARK THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)PHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1650
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1650
      GO TO 1660
!
 1650 CONTINUE
      PHOLD=PDEFTH
      GO TO 1680
!
 1660 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARK THICKNESS (FOR ALL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)PHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1750
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      IF(IHARG(NUMARG).EQ.'THIC')GO TO 1750
      GO TO 1760
!
 1750 CONTINUE
      PHOLD=PDEFTH
      GO TO 1780
!
 1760 CONTINUE
      PHOLD=ARG(NUMARG)
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      PTICTH=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARK THICKNESS (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)PHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTCTH
      SUBROUTINE DPTEBA(IHARG,IARGT,ARG,NUMARG,ADETBA,MAXTEX,ATEXBA,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TEXT BASES.
!              THESE ARE LOCATED IN THE VECTOR ATEXBA(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT  (A  CHARACTER VECTOR)
!                     --ARG
!                     --NUMARG
!                     --ADETBA
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--ATEXBA (A FLOATING POINT VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION ATEXBA(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTE'
      ISUBN2='BA  '
!
      NUMTEX=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ATEXBA(1)
   70 FORMAT('ATEXBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,ATEXBA(I)
   76 FORMAT('I,ATEXBA(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.0)GO TO 9000
      IF(NUMARG.EQ.1)GO TO 1110
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      GO TO 1140
!
 1110 CONTINUE
      GO TO 1200
!
 1120 CONTINUE
      IF(IHARG(2).EQ.'ALL')IHOLD1='    '
      IF(IHARG(2).EQ.'ALL')HOLD1=ADETBA
      IF(IHARG(2).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(2).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(2).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(2).EQ.'ALL')GO TO 1300
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(2)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(2)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      ATEXBA(1)=ADETBA
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-1
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,ATEXBA(I)
 1276 FORMAT('THE BASE OF TEXT ',I6,   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=ADETBA
      IF(IHOLD1.EQ.'OFF')HOLD2=ADETBA
      IF(IHOLD1.EQ.'AUTO')HOLD2=ADETBA
      IF(IHOLD1.EQ.'DEFA')HOLD2=ADETBA
      DO 1315 I=1,NUMTEX
      ATEXBA(I)=HOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ATEXBA(I)
 1316 FORMAT('THE BASE OF ALL TEXTS',   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ATEXBA(1)
 9030 FORMAT('ATEXBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,ATEXBA(I)
 9036 FORMAT('I,ATEXBA(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTEBA
      SUBROUTINE DPTECH(IHARG,NUMARG,   &
      IDEFTC,   &
      ITERCH,   &
      IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TERMINATOR CHARACTOR WHICH MAY
!              BE USED TO PUT MULTIPLE COMMAND STATEMENTS
!              ON A SINGLE COMMAND LINE.
!              WHEN A COMMAND LINE IS READ,
!              IT IS SEARCHED FOR THE TERMINATOR CHARACTER;
!              IF IT IS FOUND, THE COMMAND STATEMENT
!              BEFORE THE TERMINATOR CHARACTOR IS EXECUTED;
!              AFTER EXECUTION, THE COMMAND STAEMENT AFTER THE
!              TERMINATOR CHARACTOR IS EXECUTED.
!              ANY NUMBER OF TERMINATOR CHARACTORS ARE ALLOWED PER LINE.
!              THE COMMAND CHARACTER CAPABILITY ALLOWS THE ANALYST
!              TO PACK SEVERAL COMMANDS PER LINE.
!              THE SPECIFIED TERMINATOR CHARACTOR WILL BE PLACED
!              IN THE CHARACTER VARIABLE ITERCH.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFTC (A  CHARACTER VARIABLE)
!                     --IBUGS2 (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--ITERCH (A CHARACTER VARIABLE)
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
!     ORIGINAL VERSION--NOVEMBER 1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFTC
      CHARACTER*4 ITERCH
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
!-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPTECH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFTC
   53 FORMAT('IDEFTC = ',A4)
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
      IHOLD=IDEFTC
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      ITERCH=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ITERCH
 1181 FORMAT('THE TERMINATOR CHARACTOR HAVE JUST BEEN SET TO ',   &
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
 9011 FORMAT('***** AT THE END       OF DPECH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR
 9012 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFTC,ITERCH
 9013 FORMAT('IDEFTC,ITERCH = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTECH
      SUBROUTINE DPTEXT(IANS,IANSLC,IWIDTH,   &
                        ITEXTE,NCTEX,   &
                        PXSTAR,PYSTAR,PXEND,PYEND,   &
                        IGRASW,IDIASW,PRV,PDIARV,   &
                        ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                        ATEXBA,ITEBLI,ITEBCO,ITEBC2,MAXTXZ,PTEBTH,   &
                        ITEFSW,ITEFCO,ITEFC2,   &
                        ITEPTY,ITEPLI,ITEPCO,ITEPC2,PTEPTH,PTEPSP,   &
                        PTEXMR,ITEXCV,ATEXAN,PTEXRV,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                        IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
                        NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                        IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                        IDNVOF,IDNHOF,IDFONT,PDSCAL,   &
                        IMPSW2,AMPSCH,AMPSCW,   &
                        IBACCO,IBACC2,   &
                        IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--WRITE OUT A TEXT STRING.
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
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!     UPDATED         --DECEMBER    1986.
!     UPDATED         --JULY        1988.
!     UPDATED         --JANUARY     1989.  CALL LIST FOR OFFSET
!                                          VARIABLES (ALAN)
!     UPDATED         --MARCH       1993.
!     UPDATED         --SEPTEMBER   1993. ALLOW LOWER CASE
!     UPDATED         --MARCH       1997. DEVICE FONT SUPPORT
!     UPDATED         --DECEMBER    2018. SUPPORT FOR DEVICE ... SCALE
!                                         COMMAND (PDSCAL TO CALL LIST)
!
!-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
!
      CHARACTER*4 IANS
!CCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
!CCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      CHARACTER*4 IANSLC
!
      CHARACTER*4 IGRASW
      CHARACTER*4 IDIASW
!
      CHARACTER*4 ILINPA
      CHARACTER*4 ILINCO
!
      CHARACTER*4 ITEBLI
      CHARACTER*4 ITEBCO
      CHARACTER*4 ITEFSW
      CHARACTER*4 ITEFCO
      CHARACTER*4 ITEPTY
      CHARACTER*4 ITEPLI
      CHARACTER*4 ITEPCO
!
      CHARACTER*4 ITEXTE
      CHARACTER*4 ITEXFO
      CHARACTER*4 ITEXCA
      CHARACTER*4 ITEXJU
      CHARACTER*4 ITEXDI
      CHARACTER*4 ITEXFI
      CHARACTER*4 ITEXCO
!
      CHARACTER*4 ITEXCR
      CHARACTER*4 ITEXLF
!
      CHARACTER*4 ITEXSY
      CHARACTER*4 ITEXSP
!
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IFUNC
!
      CHARACTER*1 IREPCH
!
      CHARACTER*4 IMPSW2
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
!
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IBELSW
      CHARACTER*4 IERASW
      CHARACTER*4 ICOPSW
      CHARACTER*4 IBACCO
!
      CHARACTER*4 ICTEXT
!
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 IDIR
      CHARACTER*4 IFILL
      CHARACTER*4 ICOL
!
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
!
      CHARACTER*4 ITEXCV
!
      DIMENSION PRV(6)
      DIMENSION PDIARV(4)
      DIMENSION ITEXCV(10)
      DIMENSION PTEXRV(5)
!
      DIMENSION IANS(*)
!CCCC THE FOLLOWING LINE WAS ADDED       SEPTEMBER 1993
!CCCC TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
      DIMENSION IANSLC(*)
!
      DIMENSION ILINPA(*)
      DIMENSION ILINCO(*)
      DIMENSION ILINC2(MAXLNZ,3)
      DIMENSION PLINTH(*)
!
      DIMENSION ATEXBA(*)
      DIMENSION ITEBLI(*)
      DIMENSION ITEBCO(*)
      DIMENSION ITEBC2(MAXTXZ,3)
      DIMENSION PTEBTH(*)
      DIMENSION ITEFSW(*)
      DIMENSION ITEFCO(*)
      DIMENSION ITEFC2(MAXTXZ,3)
      DIMENSION ITEPTY(*)
      DIMENSION ITEPLI(*)
      DIMENSION ITEPCO(*)
      DIMENSION ITEPC2(MAXTXZ,3)
      DIMENSION PTEPTH(*)
      DIMENSION PTEPSP(*)
      DIMENSION PDSCAL(*)
!
      DIMENSION ITEXTE(*)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
!
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
!CCCC DIMENSION ICTEXT(130)
      INCLUDE 'DPCOPA.INC'
      DIMENSION ICTEXT(MAXCH)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      PGRAXF=PRV(1)
      PGRAYF=PRV(2)
      PDIAXC=PRV(3)
      PDIAYC=PRV(4)
      PDIAX2=PRV(5)
      PDIAY2=PRV(6)
!
      PDIAHE=PDIARV(1)
      PDIAWI=PDIARV(2)
      PDIAVG=PDIARV(3)
      PDIAHG=PDIARV(4)
!
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
!
      PTEXHE=PTEXRV(1)
      PTEXWI=PTEXRV(2)
      PTEXVG=PTEXRV(3)
      PTEXHG=PTEXRV(4)
      PTEXTH=PTEXRV(5)
!
      IFOUND='NO'
      IERROR='NO'
!
      J2=0
!
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
   65   FORMAT('ITEPTY(1),ITEPLI(1),ITEPCO(1),PTEPTH(1),PTEPSP(1) = ',   &
               3(A4,2X),2G15.7)
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
        DO 76 I=1,NUMNAM
          WRITE(ICOUT,77)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                         IVALUE(I),VALUE(I)
   77     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)= ',   &
                 I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   76   CONTINUE
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
        WRITE(ICOUT,86)ILINC2(1,1),ILINC2(1,2),ILINC2(1,3)
   86   FORMAT('ILINC2(1,1),ILINC2(1,2),ILINC2(1,3) = ',3I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,87)ITEBC2(1,1),ITEBC2(1,2),ITEBC2(1,3)
   87   FORMAT('ITEBC2(1,1),ITEBC2(1,2),ITEBC2(1,3) = ',3I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,88)ITEPC2(1,1),ITEPC2(1,2),ITEPC2(1,3)
   88   FORMAT('ITEPC2(1,1),ITEPC2(1,2),ITEPC2(1,3) = ',3I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)ISUBG4,IERRG4,IBUGD2,IFOUND,IERROR,IREPCH
   93   FORMAT('ISUBG4,IERRG4,IBUGD2,IFOUND,IERROR,IREPCH = ',   &
               5(A4,2X),A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
!               *****************************************************
!
!               *****************************************
!               **  STEP 1.1--                         **
!               **  DETERMINE THE COMMAND              **
!               **  (TEXT) AND ITS LOCATION            **
!               **  ON THE LINE.                       **
!               **  DETERMINE THE START POSITION       **
!               **  (XSTART) OF THE FIRST CHARACTER    **
!               **  FOR THE STRING TO BE PRINTED.      **
!               *****************************************
!
      DO 1115 I=1,IWIDTH
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
!
        IF(IP3.EQ.IWIDTH)GO TO 1190
        IF(IP4.EQ.IWIDTH)GO TO 1190
        IF(IANS(I).EQ.'T'.AND.IANS(IP1).EQ.'E'.AND.   &
           IANS(IP2).EQ.'X'.AND.IANS(IP3).EQ.'T'.AND.   &
           IANS(IP4).EQ.' ')GO TO 1190
 1115 CONTINUE
!
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN DPTEXT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      NO MATCH FOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
 1190 CONTINUE
!
!               ********************************************************
!               **  STEP 1.2--                                        **
!               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING. **
!               ********************************************************
!
      IFOUND='YES'
!
      ISTART=IP5
      ISTOP=0
      IF(ISTART.LE.IWIDTH)THEN
        DO 1220 I=ISTART,IWIDTH
          IREV=IWIDTH-I+ISTART
          IF(IANS(IREV).NE.' ')THEN
            ISTOP=IREV
            GO TO 1225
          ENDIF
 1220   CONTINUE
 1225   CONTINUE
      ENDIF
!
!               *****************************************
!               **  STEP 1.3--                         **
!               **  COPY OVER THE STRING OF INTEREST.  **
!               *****************************************
!
      IF(ISTART.GT.ISTOP .OR. ISTOP.EQ.0)THEN
        NCTEX=0
      ELSE
!       SEPTEMBER, 1987 (CHECK IF MAXIMUM SIZE STRING EXCEEDED)
        ITEMP=ISTOP-ISTART+1
        IF(ITEMP.GT.MAXCH)ITEMP=MAXCH
        ISTOP=ISTART+ITEMP-1
!
        J=0
        DO 1310 I=ISTART,ISTOP
          J=J+1
          J2=J
!CCCC     THE FOLLOWING LINE WAS CHANGED     SEPTEMBER 1993
!CCCC     TO ALLOW FOR LOWER CASE            SEPTEMBER 1993
!CCCC     CHECK FOR CASE "ASIS"              OCTOBER   1993
!CCCC     ITEXTE(J)=IANS(I)
          IF(ITEXCA.EQ.'ASIS')THEN
            ITEXTE(J)=IANSLC(I)
          ELSE
            ITEXTE(J)=IANS(I)
          ENDIF
 1310   CONTINUE
        NCTEX=J2
      ENDIF
!
!               ******************************************
!               **  STEP 1.4--                          **
!               **  COPY OVER THE ORIGINAL TEXT STRING  **
!               **  SO AS TO PRESERVE IT IN COMMON.     **
!               ******************************************
!
      NCTEXT=NCTEX
      IF(NCTEX.GT.0)THEN
        DO 1410 I=1,NCTEX
          ICTEXT(I)=ITEXTE(I)
 1410   CONTINUE
      ENDIF
!
!               ******************************************************
!               **  STEP 1.4--                                      **
!               **  CALL THE SUBROUTINE DPREPL                      **
!               **  WHICH WILL SCAN THE STRING FOR ALL OCCURRANCES  **
!               **  OF THE SUBSTRING VALU()                         **
!               **  AND REPLACE THEM BY THEIR LITERAL VALUES.       **
!               ******************************************************
!
      IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,   &
      IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
      IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
      IBUGD2,IERROR)
!
!               ********************************
!               **  STEP 2--                  **
!               **  STEP THROUGH EACH DEVICE  **
!               ********************************
!
      IF(NUMDEV.LE.0)GO TO 9000
!  JULY, 1988.  BUG: IF DEVICE 1 OFF AND DEVICE 2 ON,
!  STARTING COORDINATES PX1 AND PY1 WERE NOT GETTING SET.
!  MOVE FROM INSIDE LOOP TO HERE.
      PX1=PXSTAR
      PY1=PYSTAR
!  END BUG FIX
      DO 8000 IDEVIC=1,NUMDEV
!
        IF(IDPOWE(IDEVIC).EQ.'OFF')GO TO 8000
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
!               **  STEP 3--                      **
!               **  CARRY OUT OPENING OPERATIONS  **
!               **  ON THE GRAPHICS DEVICES       **
!               ************************************
!
        CALL DPOPDE()
!
        IBELSW='OFF'
        NUMRIN=0
        IERASW='OFF'
!
        CALL DPOPPL(IGRASW,IBELSW,NUMRIN,IERASW,IBACCO,IBACC2)
!
!               *****************************
!               **  STEP 4--               **
!               **  WRITE OUT THE TEXT     **
!               *****************************
!
        IFONT=ITEXFO
        ICASE=ITEXCA
        IJUST=ITEXJU
        IDIR=ITEXDI
        ANGLE=ATEXAN
        IFILL=ITEXFI
        ICOL=ITEXCO
        ICOLR=-1
        ICOLG=-1
        ICOLB=-1
        PHEIGH=PTEXHE
        PWIDTH=PTEXWI
        PHOGAP=PTEXHG
        PVEGAP=PTEXVG
        PTHICK=PTEXTH
        ISYMBL=ITEXSY
        ISPAC=ITEXSP
!
!      JULY, 1988.  MOVE FOLLOWING 4 LINES TO BEFORE LOOP.
!CCCC   IF(IDEVIC.GE.2)GO TO 1610
!CCCC   PX1=PXSTAR
!CCCC   PY1=PYSTAR
!1610   CONTINUE
!
        ICOLR=ITEFC2(1,1)
        ICOLR=ITEFC2(1,2)
        ICOLR=ITEFC2(1,3)
        IF(NCTEXT.GE.1)   &
           CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,   &
                       IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,   &
                       ICOL,ICOLR,ICOLG,ICOLB,   &
                       PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,   &
                       ISYMBL,ISPAC,   &
                       IMPSW2,AMPSCH,AMPSCW,   &
                       PX99,PY99)
!
!CCCC   MARCH 1993.  MOVE FOLLOWING SECTION OUTSIDE LOOP.
!CCCC   IF(IDEVIC.GE.2)GO TO 1690
!CCCC   PXEND=PX99
!CCCC   PYEND=PY99
!CCCC   IF(ITEXCR.EQ.'ON')PXEND=PTEXMR
!CCCC   IF(ITEXLF.EQ.'ON')PYEND=PYSTAR-PTEXHE-PTEXVG
!
!CCCC   PXSTAR=PXEND
!CCCC   PYSTAR=PYEND
!
!               ************************************
!               **  STEP 5--                      **
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
!  MARCH, 1993.  BUG: IF DEVICE 1 OFF AND DEVICE 2 ON,
!  NEW VALUES OF PXSTAR AND PYSTAR NOT SET.
!  MOVE FROM INSIDE LOOP TO HERE.
!
      PXEND=PX99
      PYEND=PY99
      IF(ITEXCR.EQ.'ON')PXEND=PTEXMR
      IF(ITEXLF.EQ.'ON')PYEND=PYSTAR-PTEXHE-PTEXVG
!
      PXSTAR=PXEND
      PYSTAR=PYEND
!  END CHANGE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPTEXT
      SUBROUTINE DPTFCO(IHARG,NUMARG,IDETFC,MAXTEX,ITEFCO,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TEXT FILL COLORS = THE COLORS
!              OF THE (BACKGROUND) FILL WITHIN THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR ITEFCO(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDETFC
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--ITEFCO (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDETFC
      CHARACTER*4 ITEFCO
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION ITEFCO(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTF'
      ISUBN2='CO  '
!
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEFCO(1)
   70 FORMAT('ITEFCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,ITEFCO(I)
   76 FORMAT('I,ITEFCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      ITEFCO(1)=IDETFC
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETFC
      ITEFCO(I)=IHOLD2
 1225 CONTINUE
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEFCO(I)
 1276 FORMAT('THE FILL COLOR OF TEXT ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETFC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETFC
      DO 1315 I=1,NUMTEX
      ITEFCO(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEFCO(I)
 1316 FORMAT('THE FILL COLOR OF ALL TEXTS',   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      DO 9025 I=1,NUMARG
        WRITE(ICOUT,9026)IHARG(I)
 9026   FORMAT('IHARG(I) = ',A4)
        CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      DO 9035 I=1,10
        WRITE(ICOUT,9036)I,ITEFCO(I)
 9036   FORMAT('I,ITEFCO(I) = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTFCO
      SUBROUTINE DPTFSW(IHARG,NUMARG,IDETFS,MAXTEX,ITEFSW,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE TEXT FILL SWITCHES = THE ON/OFF SWITCHES
!              OF THE (BACKGROUND) FILL WITHIN THE TEXTS.
!              THESE ARE LOCATED IN THE VECTOR ITEFSW(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDETFS
!                     --MAXTEX
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--ITEFSW (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDETFS
      CHARACTER*4 ITEFSW
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION ITEFSW(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPTF'
      ISUBN2='SW  '
!
      NUMTEX=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ITEFSW(1)
   70 FORMAT('ITEFSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,ITEFSW(I)
   76 FORMAT('I,ITEFSW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='ON'
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMTEX=1
      ITEFSW(1)='ON'
      GO TO 1270
!
 1220 CONTINUE
      NUMTEX=NUMARG-2
      IF(NUMTEX.GT.MAXTEX)NUMTEX=MAXTEX
      DO 1225 I=1,NUMTEX
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETFS
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETFS
      ITEFSW(I)=IHOLD2
 1225 CONTINUE
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMTEX
      WRITE(ICOUT,1276)I,ITEFSW(I)
 1276 FORMAT('THE FILL SWITCH FOR TEXT ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMTEX=MAXTEX
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDETFS
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDETFS
      DO 1315 I=1,NUMTEX
      ITEFSW(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ITEFSW(I)
 1316 FORMAT('THE FILL SWITCH FOR ALL TEXTS',   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ITEFSW(1)
 9030 FORMAT('ITEFSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,ITEFSW(I)
 9036 FORMAT('I,ITEFSW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTFSW
      SUBROUTINE DPTHIC(IHARG,IARGT,ARG,NUMARG,   &
      PDEFTH,   &
      PTEXTH,   &
      PFRATH,PTICTH,PTIZTH,PVGRTH,PHGRTH,PTITTH,PX1LTH,PX2LTH,PY1LTH,   &
      PY2LTH,PLEGTH,MAXLG,PBOPTH,PBOFTH,MAXBX,PARRTH,MAXAR,   &
      PSEGTH,MAXSG,PLINTH,MAXLN,PCHATH,MAXCH2,PFILTH,MAXFL,   &
      PPATTH,MAXPT,PSPITH,MAXSP,PBABTH,PBAPTH,MAXBA,PREPTH,MAXRG,   &
      PMABTH,PMAPTH,MAXMR,PTEBTH,PTEPTH,MAXTX,   &
      IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE THICKNESS FOR TEXT CHARACTERS.
!              THE THICKNESS FOR TEXT CHARACTERS WILL BE PLACED
!              IN THE FLOATING POINT VARIABLE PTEXTH.
!     NOTE--THE THICKNESS IS IN STANDARDIZED UNITS (0.0 TO 100.0).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT
!                     --ARG
!                     --NUMARG
!                     --PDEFTH
!                     --IBUGD2
!     OUTPUT ARGUMENTS--PTEXTH
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
!     UPDATED         --JANUARY   1989.  SET ALL THICKNESS PARAMETERS (ALAN)
!     UPDATED         --SEPTEMBER 1993.  FIX BUG FORMAT STATEMENT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
!  DECEMBER 1987
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
!  END CHANGE
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
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
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
      DO 55 I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I),IARGT(I),ARG(I)
   56 FORMAT('I,IHARG(I),IARGT(I),ARG(I) = ',I8,2X,A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
!
!               *****************************
!               **  TREAT THE THICKNESS CASE  **
!               *****************************
!
      IF(NUMARG.LE.0)GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
!
      IF(NUMARG.GE.1.AND.IARGT(NUMARG).EQ.'NUMB')   &
      GO TO 1160
!
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPTHIC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR THICKNESS COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
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
      GO TO 9000
!
 1150 CONTINUE
      PTEXTH=PDEFTH
      GO TO 1180
!
 1160 CONTINUE
      PTEXTH=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
!  DECEMBER 1987: SET ALL THICKNESSES TO THE SET VALUE
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
      DO 2010 I=1,MAXLG
      PLEGTH(I)=PTEXTH
 2010 CONTINUE
      DO 2020 I=1,MAXBX
      PBOPTH(I)=PTEXTH
      PBOFTH(I)=PTEXTH
 2020 CONTINUE
      DO 2030 I=1,MAXAR
      PARRTH(I)=PTEXTH
 2030 CONTINUE
      DO 2040 I=1,MAXSG
      PSEGTH(I)=PTEXTH
 2040 CONTINUE
      DO 2050 I=1,MAXLN
      PLINTH(I)=PTEXTH
 2050 CONTINUE
      DO 2060 I=1,MAXCH2
      PCHATH(I)=PTEXTH
 2060 CONTINUE
      DO 2070 I=1,MAXFL
      PFILTH(I)=PTEXTH
 2070 CONTINUE
      DO 2080 I=1,MAXPT
      PPATTH(I)=PTEXTH
 2080 CONTINUE
      DO 2090 I=1,MAXSP
      PSPITH(I)=PTEXTH
 2090 CONTINUE
      DO 2100 I=1,MAXBA
      PBABTH(I)=PTEXTH
      PBAPTH(I)=PTEXTH
 2100 CONTINUE
      DO 2110 I=1,MAXRG
      PREPTH(I)=PTEXTH
 2110 CONTINUE
      DO 2120 I=1,MAXMR
      PMABTH(I)=PTEXTH
      PMAPTH(I)=PTEXTH
 2120 CONTINUE
      DO 2130 I=1,MAXTX
      PTEBTH(I)=PTEXTH
      PTEPTH(I)=PTEXTH
 2130 CONTINUE
!  END CHANGE
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE THICKNESS (FOR TEXT CHARACTERS)  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PTEXTH
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
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
      WRITE(ICOUT,8111)PTEXTH
 8111 FORMAT('THE CURRENT (TEXT) THICKNESS IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)PDEFTH
 8112 FORMAT('THE DEFAULT (TEXT) THICKNESS IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPTHIC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
!CCCC THE FOLLOWING LINE WAS FIXED    SEPTEMBER 1993
!9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',,A4,2X,A4,2X,A4)
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)PTEXTH
 9013 FORMAT('PTEXTH = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPTHIC
      SUBROUTINE DPTIC(ICOM,IHARG,NUMARG,   &
                       IX1TSW,IX2TSW,IY1TSW,IY2TSW,   &
                       IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 4 TIC MARK SWITCHES CONTAINED IN THE
!              4 VARIABLES IX1TSW,IX2TSW,IY1TSW,IY2TSW
!              SUCH TIC MARK SWITCHES TURN ON OR OFF
!              THE TIC MARKS ON THE 4 FRAME LINES OF A PLOT.
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--
!                     --IX1TSW = LOWER HORIZONTAL FRAME TIC MARKS
!                     --IX2TSW = UPPER HORIZONTAL FRAME TIC MARKS
!                     --IY1TSW = LEFT  VERTICAL   FRAME TIC MARKS
!                     --IY2TSW = RIGHT VERTICAL   FRAME TIC MARKS
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
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1988. (ALLOW FOR TIC NUMBER COMMAND)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
!
      CHARACTER*4 IX1TSW
      CHARACTER*4 IX2TSW
      CHARACTER*4 IY1TSW
      CHARACTER*4 IY2TSW
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
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
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RGB '.AND.   &
         IHARG(2).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'POSI')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIZE')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'HW')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LABE')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DECI')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLAC')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'NUMB')GO TO 1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'OFFS')GO TO 1900
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'RGB '.AND.IHARG(3).EQ.'COLO')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'COOR')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'POSI')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'SIZE')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'HW')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'LABE')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'DECI')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'PLAC')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'NUMB')GO TO 1900
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.   &
         IHARG(2).EQ.'OFFS')GO TO 1900
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH HORIZONTAL AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'XTIC')GO TO 1100
      GO TO 1199
!
 1100 CONTINUE
      IF(NUMARG.LE.0)GO TO 1160
      IF(IHARG(NUMARG).EQ.'MARK')GO TO 1160
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1160
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      GO TO 1150
!
 1150 CONTINUE
      IHOLD='ON'
      GO TO 1180
!
 1160 CONTINUE
      IHOLD='OFF'
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IX1TSW=IHOLD
      IX2TSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE TIC MARKS (FOR BOTH HORIZONTAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1900
!
 1199 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE BOTTOM HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X1TI')GO TO 1200
      GO TO 1299
!
 1200 CONTINUE
      IF(NUMARG.LE.0)GO TO 1260
      IF(IHARG(NUMARG).EQ.'MARK')GO TO 1260
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1250
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1260
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1250
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1250
      GO TO 1250
!
 1250 CONTINUE
      IHOLD='ON'
      GO TO 1280
!
 1260 CONTINUE
      IHOLD='OFF'
      GO TO 1280
!
 1280 CONTINUE
      IFOUND='YES'
      IX1TSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE TIC MARKS (FOR THE BOTTOM ',   &
      'HORIZONTAL FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GO TO 1900
!
 1299 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE TOP    HORIZONTAL TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'X2TI')GO TO 1300
      GO TO 1399
!
 1300 CONTINUE
      IF(NUMARG.LE.0)GO TO 1360
      IF(IHARG(NUMARG).EQ.'MARK')GO TO 1360
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1350
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1360
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1350
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1350
      GO TO 1350
!
 1350 CONTINUE
      IHOLD='ON'
      GO TO 1380
!
 1360 CONTINUE
      IHOLD='OFF'
      GO TO 1380
!
 1380 CONTINUE
      IFOUND='YES'
      IX2TSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE TIC MARKS (FOR THE TOP HORIZONTAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GO TO 1900
!
 1399 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  BOTH VERTICAL   AXIS TICS ARE TO BE CHANGED    **
!               *****************************************************
!
      IF(ICOM.EQ.'YTIC')GO TO 1400
      GO TO 1499
!
 1400 CONTINUE
      IF(NUMARG.LE.0)GO TO 1460
      IF(IHARG(NUMARG).EQ.'MARK')GO TO 1460
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1450
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1460
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1450
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1450
      GO TO 1450
!
 1450 CONTINUE
      IHOLD='ON'
      GO TO 1480
!
 1460 CONTINUE
      IHOLD='OFF'
      GO TO 1480
!
 1480 CONTINUE
      IFOUND='YES'
      IY1TSW=IHOLD
      IY2TSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE TIC MARKS (FOR BOTH VERTICAL ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GO TO 1900
!
 1499 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE LEFT   VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y1TI')GO TO 1500
      GO TO 1599
!
 1500 CONTINUE
      IF(NUMARG.LE.0)GO TO 1560
      IF(IHARG(NUMARG).EQ.'MARK')GO TO 1560
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1550
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1560
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1550
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1550
      GO TO 1550
!
 1550 CONTINUE
      IHOLD='ON'
      GO TO 1580
!
 1560 CONTINUE
      IHOLD='OFF'
      GO TO 1580
!
 1580 CONTINUE
      IFOUND='YES'
      IY1TSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE TIC MARKS (FOR THE LEFT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GO TO 1900
!
 1599 CONTINUE
!
!               **************************************************************
!               **  TREAT THE CASE WHEN                                     **
!               **  ONLY THE RIGHT  VERTICAL   TIC MARKS ARE TO BE CHANGED  **
!               **************************************************************
!
      IF(ICOM.EQ.'Y2TI')GO TO 1600
      GO TO 1699
!
 1600 CONTINUE
      IF(NUMARG.LE.0)GO TO 1660
      IF(IHARG(NUMARG).EQ.'MARK')GO TO 1660
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1650
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1660
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1650
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1650
      GO TO 1650
!
 1650 CONTINUE
      IHOLD='ON'
      GO TO 1680
!
 1660 CONTINUE
      IHOLD='OFF'
      GO TO 1680
!
 1680 CONTINUE
      IFOUND='YES'
      IY2TSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE TIC MARKS (FOR THE RIGHT VERTICAL ',   &
      'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GO TO 1900
!
 1699 CONTINUE
!
!               *****************************************************
!               **  TREAT THE CASE WHEN                            **
!               **  ALL 4 FRAME TICS ARE TO BE CHANGED             **
!               *****************************************************
!
      IF(ICOM.EQ.'TIC')GO TO 1700
      IF(ICOM.EQ.'TICS')GO TO 1700
      IF(ICOM.EQ.'XYTI')GO TO 1700
      IF(ICOM.EQ.'YXTI')GO TO 1700
      GO TO 1799
!
 1700 CONTINUE
      IF(NUMARG.LE.0)GO TO 1760
      IF(IHARG(NUMARG).EQ.'MARK')GO TO 1760
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1750
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1760
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1750
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1750
      GO TO 1750
!
 1750 CONTINUE
      IHOLD='ON'
      GO TO 1780
!
 1760 CONTINUE
      IHOLD='OFF'
      GO TO 1780
!
 1780 CONTINUE
      IFOUND='YES'
      IX1TSW=IHOLD
      IX2TSW=IHOLD
      IY1TSW=IHOLD
      IY2TSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE TIC MARKS (FOR ALL 4 ',   &
      'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAVE JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GO TO 1900
!
 1799 CONTINUE
!
 1900 CONTINUE
      RETURN
      END SUBROUTINE DPTIC
      SUBROUTINE DPTICA(IHARG,NUMARG,IDEFCA,ITITCA,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE CASE FOR THE TITLE
!              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
!              THE CASE FOR THE TITLE WILL BE PLACED
!              IN THE HOLLERITH VARIABLE ITITCA.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFCA
!     OUTPUT ARGUMENTS--ITITCA
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
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCA
      CHARACTER*4 ITITCA
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
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
      IF(NUMARG.LE.0)GO TO 1199
      IF(IHARG(1).EQ.'CASE')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(NUMARG.EQ.1)GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      ITITCA=IDEFCA
      GO TO 1180
!
 1160 CONTINUE
      ITITCA=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ITITCA
 1181 FORMAT('THE TITLE CASE HAS JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPTICA
      SUBROUTINE DPTICL(IHARG,IARG,NUMARG,IDEFCO,IRGBMX,ITITCO,ITITC2,   &
                        IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE COLOR FOR THE TITLE (THE HORIZONTAL STRING
!              ABOVE THE UPPER HORIZONTAL FRAME).  THE COLOR FOR THE
!              TITLE WILL BE PLACED IN THE HOLLERITH VARIABLE ITITCO.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFCO
!     OUTPUT ARGUMENTS--ITITCO
!                     --ITITC2
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
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCO
      CHARACTER*4 ITITCO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARG(*)
      DIMENSION ITITC2(*)
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
      IF(NUMARG.LE.0)GO TO 9000
      IF(IHARG(1).EQ.'COLO')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           NUMARG.EQ.1)THEN
          ITITCO=IDEFCO
        ELSE
          ITITCO=IHARG(NUMARG)
        ENDIF
!
        IFOUND='YES'
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)ITITCO
 1181     FORMAT('THE TITLE COLOR HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSEIF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.   &
           IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.   &
           NUMARG.LE.4)THEN
          ITITC2(1)=-1
          ITITC2(2)=-1
          ITITC2(3)=-1
        ELSE
          ITITC2(1)=IARG(3)
          ITITC2(2)=IARG(4)
          ITITC2(3)=IARG(5)
          IF(ITITC2(1).LT.0 .OR. ITITC2(1).GT.IRGBMX)ITITC2(1)=-1
          IF(ITITC2(2).LT.0 .OR. ITITC2(2).GT.IRGBMX)ITITC2(2)=-1
          IF(ITITC2(3).LT.0 .OR. ITITC2(3).GT.IRGBMX)ITITC2(3)=-1
        ENDIF
!
        IFOUND='YES'
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2181)ITITC2(1),ITITC2(2),ITITC2(3)
 2181     FORMAT('THE TITLE RGB COLORS HAVE JUST BEEN SET TO ',3I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPTICL
      SUBROUTINE DPTIET(XTEMP1,MAXNXT,   &
                        ICAPSW,ICASAN,IFORSW,ISEED,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--PERFORM TIETJEN-MOORE TEST FOR UNIVARIATE OUTLIERS.
!              THIS IS A GENERALIZATION OF THE GRUBB TEST (WHICH
!              LOOKS FOR A SINGLE OUTLIER) TO LOOK FOR "K" OUTLIERS.
!              LIKE GRUBBS TEST, THIS TEST ASSUMES THE DATA FOLLOWS AN
!              APPROXIMATELY NORMAL DISRIBUTION).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORAOTRY
!                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/11
!     ORIGINAL VERSION--NOVEMBER  2009.
!     UPDATED         --JANUARY   2009. PRINT VALUES OF POTENTIAL
!                                       OUTLIERS
!     UPDATED         --AUGUST    2010. FOR TWO-SIDED CASE, POTENTIAL
!                                       OUTLIERS PRINTED WERE CORRECT
!     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IDATSW
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
!
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
!
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICASE
      CHARACTER*4 IRANSV
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
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
!
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
      DIMENSION XTEMP4(MAXOBV)
      DIMENSION YSTAT(MAXOBV)
!
      DIMENSION XDESGN(MAXOBV,7)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
!
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
!
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
      INCLUDE 'DPCOF2.INC'
!
      COMMON/ISED/ISED1,ISED2,ISED3,ISED4,ISED5,ISED6,   &
                  ISED7,ISED8,ISED9,ISED10,ISED11
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      MINN2=3
!
!               ***************************************************
!               **  TREAT THE TIETJEN MOORE             CASE     **
!               ***************************************************
!
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
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:            **
!               **    1) TIETJEN MOORE TEST Y                          **
!               **    2) TIETJEN MOORE TEST Y LABID                    **
!               **    3) TIETJEN MOORE TEST Y1 ... YK                  **
!               **    4) REPLICATED TIETJEN MOORE TEST Y X1 ... XK     **
!               **    5) REPLICATED TIETJEN MOORE TEST Y LABID X1 ... XK *
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTC=9999
      ILASTZ=9999
      IFOUND='NO'
      ICASAN='TWOS'
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
        IF(ICTMP1.EQ.'TIET' .AND. ICTMP2.EQ.'MOOR' .AND.   &
           ICTMP3.EQ.'TEST')THEN
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
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IFOUND.EQ.'NO')GO TO 9000
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN TIETJEN-MOORE TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION" FOR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      THE TIETJEN-MOORE TEST COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
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
      IHP='NOUT'
      IHP2='LIER'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        IR=1
      ELSE
        AR=VALUE(ILOCV)
        IR=INT(AR+0.1)
        IF(IR.LT.1)IR=1
      ENDIF
!
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')THEN
        WRITE(ICOUT,521)NRESP,NLABID,NREPL,IR
  521   FORMAT('NRESP,NLABID,NREPL,IR = ',4I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 6--                                        **
!               **  GENERATE THE TIETJEN-MOORE TEST FOR THE VARIOUS **
!               **  CASES                                           **
!               ******************************************************
!
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
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
!
!       *****************************************************
!       **  STEP 7B--                                      **
!       **  CALL DPTIE2 TO PERFORM THE OUTLIER TEST.       **
!       *****************************************************
!
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIET')THEN
          ISTEPN='7B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,711)
  711     FORMAT('***** FROM THE MIDDLE  OF DPTIET--')
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
        NREPL=0
        NCURVE=1
        CALL DPTIE2(Y1,X1,NLOCAL,ICASAN,IOUNI2,ISEED,   &
                    YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                    ITEMP1,ITEMP2,ITEMP3,   &
                    PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                    STATVA,STATCD,PVAL,   &
                    CUT0,CUT01,CUT025,CUT05,CUT10,   &
                    CUT25,CUT50,CUT100,   &
                    ISUBRO,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 7C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
        ISTEPN='7C'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IFLAGU='ON'
        IFRST=.FALSE.
        ILAST=.FALSE.
        CALL DPTIE4(STATVA,STATCD,PVAL,   &
                    CUT0,CUT01,CUT025,CUT05,CUT10,   &
                    CUT25,CUT50,CUT100,   &
                    IFLAGU,IFRST,ILAST,ICASP2,   &
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')THEN
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
!         **  CALL DPTIE2 TO PERFORM THE OUTLIER TEST.       **
!         *****************************************************
!
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIET')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPTIET--')
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
          CALL DPTIE2(Y1,X1,NLOCAL,ICASAN,IOUNI2,ISEED,   &
                      YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                      ITEMP1,ITEMP2,ITEMP3,   &
                      PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                      ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                      STATVA,STATCD,PVAL,   &
                      CUT0,CUT01,CUT025,CUT05,CUT10,   &
                      CUT25,CUT50,CUT100,   &
                      ISUBRO,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  COMPUTE GRUBB     STAT           **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IFLAGU='FILE'
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          IFLAGU='ON'
          IFRST=.FALSE.
          ILAST=.FALSE.
          CALL DPTIE4(STATVA,STATCD,PVAL,   &
                      CUT0,CUT01,CUT025,CUT05,CUT10,   &
                      CUT25,CUT50,CUT100,   &
                      IFLAGU,IFRST,ILAST,ICASP2,   &
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'TIET')   &
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
!       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!       **                                                 **
!       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
!       **  VARIOUS REPLICATIONS.                          **
!       *****************************************************
!
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIET')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPTIET--')
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
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,   &
                          YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                          ITEMP1,ITEMP2,ITEMP3,   &
                          PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          STATVA,STATCD,PVAL,   &
                          CUT0,CUT01,CUT025,CUT05,CUT10,   &
                          CUT25,CUT50,CUT100,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NUMSE1)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,   &
                        CUT0,CUT01,CUT025,CUT05,CUT10,   &
                        CUT25,CUT50,CUT100,   &
                        IFLAGU,IFRST,ILAST,ICASP2,   &
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
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,   &
                          YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                          ITEMP1,ITEMP2,ITEMP3,   &
                          PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          STATVA,STATCD,PVAL,   &
                          CUT0,CUT01,CUT025,CUT05,CUT10,   &
                          CUT25,CUT50,CUT100,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,   &
                        CUT0,CUT01,CUT025,CUT05,CUT10,   &
                        CUT25,CUT50,CUT100,   &
                        IFLAGU,IFRST,ILAST,ICASP2,   &
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
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,   &
                          YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                          ITEMP1,ITEMP2,ITEMP3,   &
                          PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          STATVA,STATCD,PVAL,   &
                          CUT0,CUT01,CUT025,CUT05,CUT10,   &
                          CUT25,CUT50,CUT100,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,   &
                        CUT0,CUT01,CUT025,CUT05,CUT10,   &
                        CUT25,CUT50,CUT100,   &
                        IFLAGU,IFRST,ILAST,ICASP2,   &
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
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,   &
                          YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                          ITEMP1,ITEMP2,ITEMP3,   &
                          PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          STATVA,STATCD,PVAL,   &
                          CUT0,CUT01,CUT025,CUT05,CUT10,   &
                          CUT25,CUT50,CUT100,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,   &
                        CUT0,CUT01,CUT025,CUT05,CUT10,   &
                        CUT25,CUT50,CUT100,   &
                        IFLAGU,IFRST,ILAST,ICASP2,   &
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
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,   &
                          YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                          ITEMP1,ITEMP2,ITEMP3,   &
                          PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          STATVA,STATCD,PVAL,   &
                          CUT0,CUT01,CUT025,CUT05,CUT10,   &
                          CUT25,CUT50,CUT100,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,   &
                        CUT0,CUT01,CUT025,CUT05,CUT10,   &
                        CUT25,CUT50,CUT100,   &
                        IFLAGU,IFRST,ILAST,ICASP2,   &
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
              CALL DPTIE2(TEMP1,TEMP2,NTEMP,ICASAN,IOUNI2,ISEED,   &
                          YSTAT,XTEMP1,XTEMP2,XTEMP3,XTEMP4,   &
                          ITEMP1,ITEMP2,ITEMP3,   &
                          PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          STATVA,STATCD,PVAL,   &
                          CUT0,CUT01,CUT025,CUT05,CUT10,   &
                          CUT25,CUT50,CUT100,   &
                          ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPTIE4(STATVA,STATCD,PVAL,   &
                        CUT0,CUT01,CUT025,CUT05,CUT10,   &
                        CUT25,CUT50,CUT100,   &
                        IFLAGU,IFRST,ILAST,ICASP2,   &
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
      IRANAL=IRANSV
      ISEED=ISEESV
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
!
      IF(IERROR.EQ.'YES')THEN
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9001)(IANS(I),I=1,MIN(100,IWIDTH))
 9001     FORMAT(100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
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
!
      RETURN
      END SUBROUTINE DPTIET
      SUBROUTINE DPTIE2(Y,X,N,ICASAN,IOUNI2,ISEED,   &
                        YSTAT,TEMP1,TEMP2,TEMP3,TEMP4,   &
                        ITEMP1,ITEMP2,ITEMP3,   &
                        PID,IVARID,IVARI2,NCURVE,NREPL,NLABID,IR,   &
                        ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                        STATVA,STATCD,PVAL,   &
                        CUT0,CUT01,CUT025,CUT05,CUT10,   &
                        CUT25,CUT50,CUT100,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT THE TIETJEN-MOORE TEST FOR
!              UNIVARIATE OUTLIERS (DATA ASSUMED TO FOLLOW AN
!              APPROXIMATELY NORMAL DISTRIBUTION).  THE NUMBER OF
!              SUSPECTED OUTLIERS MUST BE SPECIFIED IN ADVANCE.
!     EXAMPLE--TIETJEN-MOORE TEST Y
!     REFERENCE--GARY TIETJEN AND ROGER MOORE (AUGUST 1972), "SOME
!                GRUBBS-TYPE STATISTICS FOR THE DETECTION OF SEVERAL
!                OUTLIERS", TECHNOMETRICS, VOL. 14, NO. 3, PP. 583-597.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/11
!     ORIGINAL VERSION--NOVEMBER  2009.
!     UPDATED         --JULY      2014. ADD SKEWNESS AND KURTOSIS TO
!                                       SUMMARY STATISTICS
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
      CHARACTER*4 ICASAN
!
      CHARACTER*40 IRTFFF
      CHARACTER*40 IRTFFP
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
!
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
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION YSTAT(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION PID(*)
!
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA/   &
       0.0, 1.0, 2.5, 5.0, 10.0, 25.0, 50.0, 100.0/
!
!-----START POINT-----------------------------------------------------
!
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
!
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
      IF(IR.GE.N/2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1121)
 1121   FORMAT('      THE SPECIFIED NUMBER OF SUSPECTED OUTLIERS IS ',   &
               'GREATER THAN N/2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1123)IR
 1123   FORMAT('THE SUSPECTED NUMBER OF OUTLIERS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1125)N
 1125   FORMAT('THE SAMPLE SIZE                  = ',I8)
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
!               ************************************
!               **  STEP 21--                     **
!               **  CARRY OUT CALCULATIONS        **
!               **  FOR    TIETJEN-MOORE    TEST  **
!               ************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPTIE3(Y,N,ICASAN,IR,   &
                  TEMP1,TEMP2,TEMP3,ITEMP1,ITEMP3,   &
                  STATVA,YMEAN,YSD,YMIN,YMAX,   &
                  ISUBRO,IBUGA3,IERROR)
!
      CALL STMOM3(Y,N,IWRITE,YSKEW,IBUGA3,IERROR)
      CALL STMOM4(Y,N,IWRITE,YKURT,IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')THEN
        WRITE(ICOUT,2131)YMEAN,YSD,YMIN,YMAX,STATVA
 2131   FORMAT('YMEAN,YSD,YMIN,YMAX,STATVA = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!               ************************************
!               **  STEP 22--                     **
!               **  COMPUTE CRITICAL VALUES VIA   **
!               **  MONTE-CARLO SIMULATION        **
!               ************************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NMCSAM=10000
      NTEMP=N
      DO 2210 I=1,NMCSAM
        CALL NORRAN(NTEMP,ISEED,TEMP4)
        CALL DPTIE3(TEMP4,NTEMP,ICASAN,IR,   &
                    TEMP1,TEMP2,TEMP3,ITEMP1,ITEMP2,   &
                    STATV2,YMEAN2,YSD2,YMIN2,YMAX2,   &
                    ISUBRO,IBUGA3,IERROR)
        YSTAT(I)=STATV2
        WRITE(IOUNI2,'(3I8,2X,E15.7)')NCURVE,NREPL,I,YSTAT(I)
 2210 CONTINUE
      IDIR='LOWE'
      CALL DPGOF8(YSTAT,NMCSAM,STATVA,PVAL,IDIR,   &
                  IBUGA3,ISUBRO,IERROR)
      STATCD=1.0 - PVAL
      CUT0=YSTAT(1)
      CUT100=YSTAT(NMCSAM)
      IWRITE='OFF'
      DO 2220 I=2,7
        P100=ALPHA(I)
        CALL PERCEN(P100,YSTAT,NMCSAM,IWRITE,TEMP1,NMCSAM,   &
                    XSTAT,IBUGA3,IERROR)
        IF(I.EQ.2)CUT01=XSTAT
        IF(I.EQ.3)CUT025=XSTAT
        IF(I.EQ.4)CUT05=XSTAT
        IF(I.EQ.5)CUT10=XSTAT
        IF(I.EQ.6)CUT25=XSTAT
        IF(I.EQ.7)CUT50=XSTAT
 2220 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')THEN
        WRITE(ICOUT,2231)PVAL,STATCD,CUT0,CUT01,CUT025
 2231   FORMAT('PVAL,STATCD,CUT0,CUT01,CUT025 = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2233)CUT05,CUT10,CUT25,CUT50,CUT100
 2233   FORMAT('CUT05,CUT10,CUT25,CUT50,CUT100 = ',5G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!
!               *********************************
!               **   STEP 42--                 **
!               **   WRITE OUT EVERYTHING      **
!               **   FOR TIETJEN-MOORE TEST    **
!               *********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
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
      IF(ICASAN.EQ.'TWOS')THEN
        ITITLE=   &
        'Tietjen-Moore Test for Multiple Outliers: Two-Sided Case'
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
      ITEXT(ICNT)='H0: There are no outliers'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
!
      ITEXT(ICNT)(1:8)='Ha: The '
      WRITE(ITEXT(ICNT)(9:13),'(I5)')IR
      ISTRT=N-IR+1
      IF(ICASAN.EQ.'TWOS')THEN
        ITEXT(ICNT)(14:46)=' most extreme points are outliers'
        NCTEXT(ICNT)=46
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        DO 4111 I=ISTRT,N
          ICNT=ICNT+1
          ITEXT(ICNT)='Potential Outlier Value Tested:'
          NCTEXT(ICNT)=31
!CCCC     DPTIE3 SORTS Y APPROPRIATELY, SO ITEMP3 RETURNS WRONG
!CCCC     VALUE, JUST PRINT THE Y
!CCCC     INDOUT=ITEMP3(I)
!CCCC     AVALUE(ICNT)=Y(INDOUT)
          AVALUE(ICNT)=Y(I)
          IDIGIT(ICNT)=NUMDIG
 4111   CONTINUE
      ELSEIF(ICASAN.EQ.'MINI')THEN
        ITEXT(ICNT)(14:41)=' minimum points are outliers'
        NCTEXT(ICNT)=41
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        DO 4113 I=ISTRT,N
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
        DO 4115 I=ISTRT,N
          ICNT=ICNT+1
          ITEXT(ICNT)='Potential Outlier Value Tested:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=Y(I)
          IDIGIT(ICNT)=NUMDIG
 4115   CONTINUE
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
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN
      IDIGIT(ICNT)=NUMDIG
!CCCC ICNT=ICNT+1
!CCCC ITEXT(ICNT)='ID for Sample Minimum:'
!CCCC NCTEXT(ICNT)=22
!CCCC AVALUE(ICNT)=X(INDMIN)
!CCCC IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX
      IDIGIT(ICNT)=NUMDIG
!CCCC ICNT=ICNT+1
!CCCC ITEXT(ICNT)='ID for Sample Maximum:'
!CCCC NCTEXT(ICNT)=22
!CCCC AVALUE(ICNT)=X(INDMAX)
!CCCC IDIGIT(ICNT)=0
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
!
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
      DO 4210 I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE=' '
      NCTITL=0
!
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
!
      NMAX=0
      DO 4221 I=1,NUMCOL
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
      DO 4223 I=1,NUMROW
        DO 4225 J=1,NUMCOL
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
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
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
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CDF1=CUT10
      CDF2=CUT05
      CDF3=CUT025
      CDF4=CUT01
!
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
!
      NMAX=0
      DO 4321 I=1,NUMCOL
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
      DO 4323 I=1,NUMROW
        DO 4325 J=1,NUMCOL
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
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE2')   &
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
      ITITLE(1:26)='*Critical Values Based on '
      WRITE(ITITLE(27:34),'(I8)')NMCSAM
      ITITLE(35:58)=' Monte Carlo Simulations'
      NCTITL=58
!
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'HTML')THEN
        CALL DPHTMV(ITITLE,NCTITL,CPUMIN,NUMDIG)
      ELSEIF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'LATE')THEN
        CALL DPLATV(ITITLE,NCTITL,CPUMIN,NUMDIG)
        IFLAG1=.FALSE.
        IFLAG2=.TRUE.
        IFLAG3=.TRUE.
        CALL DPLATZ(IFLAG1,IFLAG2,IFLAG3,NHEAD)
      ELSEIF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'RTF ')THEN
!
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
!
!       END TABLE AND RESET "ASIS" MODE
!
        IF(IRTFFF.EQ.'Courier New')THEN
          ITEMP=1
        ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
          ITEMP=8
        ENDIF
        WRITE(ICOUT,8091)IBASLC,ITEMP
 8091   FORMAT(A1,'f',I1)
        CALL DPWRST(ICOUT,'WRIT')
!
        CALL DPRTF6(NHEAD)
        CALL DPRTF6(NHEAD)
        IRTFMD='VERB'
      ELSE
        WRITE(ICOUT,2589)ITITLE(1:58)
 2589   FORMAT(A60)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPTIE2
      SUBROUTINE DPTIE3(Y,N,ICASAN,IR,   &
                        TEMP1,TEMP2,TEMP3,ITEMP1,ITEMP2,   &
                        STATVA,YMEAN,YSD,YMIN,YMAX,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE IS SPLIT OFF FROM DPTIE2 TO COMPUTE
!              TIETJEN-MOORE STATISTIC.  THIS ROUTINE JUST RETURNS
!              THE VALUE OF THE TEST STATISTIC (I.E., NO CRITICAL
!              VALUES OR PRINTING).  THIS SIMPLIFIES THE SIMULATION
!              STEP USED TO OBTAIN THE CRITICAL VALUES.
!     REFERENCE--GARY TIETJEN AND ROGER MOORE (AUGUST 1972), "SOME
!                GRUBBS-TYPE STATISTICS FOR THE DETECTION OF SEVERAL
!                OUTLIERS", TECHNOMETRICS, VOL. 14, NO. 3, PP. 583-597.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/11
!     ORIGINAL VERSION--NOVEMBER  2009.
!     UPDATED         --JANUARY   2009. SAVE INDICES FOR VALUES TO
!                                       BE TESTED AS OUTLIERS
!     UPDATED         --JULY      2014. ADD SKEWNESS AND KURTOSIS TO
!                                       SUMMARY STATISTICS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 ICASAN
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DOUBLE PRECISION DSUMN
      DOUBLE PRECISION DSUMD
      DOUBLE PRECISION DTERM1
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
!
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTI'
      ISUBN2='E3  '
      IERROR='NO'
      STATVA=CPUMIN
!
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
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
      IF(IR.GE.N/2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1121)
 1121   FORMAT('      THE SPECIFIED NUMBER OF SUSPECTED OUTLIERS IS ',   &
               'GREATER THAN N/2')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1123)IR
 1123   FORMAT('THE SUSPECTED NUMBER OF OUTLIERS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1125)N
 1125   FORMAT('THE SAMPLE SIZE                  = ',I8)
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
!               ************************************
!               **  STEP 21--                     **
!               **  CARRY OUT CALCULATIONS        **
!               **  FOR    TIETJEN-MOORE    TEST  **
!               ************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'TIE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
      CALL SORT(Y,N,Y)
      YMIN=Y(1)
      YMAX=Y(N)
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      DO 2101 I=1,N
        ITEMP1(I)=I
 2101 CONTINUE
!
      IF(ICASAN.EQ.'TWOS')THEN
        DO 2110 I=1,N
          TEMP1(I)=ABS(Y(I)-YMEAN)
 2110   CONTINUE
!CCCC   CALL SORTC3(TEMP1,ITEMP1,N,TEMP2,ITEMP2)
        CALL SORTC(TEMP1,Y,N,TEMP2,TEMP3)
        DO 2115 I=1,N
          Y(I)=TEMP3(I)
 2115   CONTINUE
      ELSEIF(ICASAN.EQ.'MINI')THEN
         CALL REVERS(Y,N,IWRITE,TEMP1,TEMP2,IBUGA3,IERROR)
         DO 2117 I=1,N
           Y(I)=TEMP1(I)
 2117    CONTINUE
      ENDIF
      NLAST=N-IR
      CALL MEAN(Y,NLAST,IWRITE,YMEANN,IBUGA3,IERROR)
!
      DSUMN=0.0D0
      DSUMD=0.0D0
      DO 2120 I=1,N
        DTERM1=DBLE(Y(I) - YMEAN)
        DSUMD=DSUMD + DTERM1**2
 2120 CONTINUE
!
      DO 2125 I=1,NLAST
        DTERM1=DBLE(Y(I) - YMEANN)
        DSUMN=DSUMN + DTERM1**2
 2125 CONTINUE
!
      DTERM1=DSUMN/DSUMD
      STATVA=REAL(DTERM1)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPTIE3
      SUBROUTINE DPTIE4(STATVA,STATCD,PVAL,   &
                        CUT0,CUT01,CUT025,CUT05,CUT10,   &
                        CUT25,CUT50,CUT100,   &
                        IFLAGU,IFRST,ILAST,ICASPL,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--UTILITY ROUTINE USED BY DPTIET.  THIS ROUTINE
!              UPDATES THE PARAMETERS "STATVAL", "STATCDF", AND
!              "PVALUE" AFTER A TIETJEN-MOORE TEST.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORAOTRY
!                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/11
!     ORIGINAL VERSION--NOVEMBER  2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IFLAGU
      CHARACTER*4 ICASPL
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
!
      SAVE IOUNI1
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
!
      IF(IFLAGU.EQ.'FILE')THEN
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
!
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
!
          IF(IERROR.EQ.'YES')GO TO 9000
!
          WRITE(IOUNI1,295)
  295     FORMAT(11X,'STATVAL',8X,'STATCDF',8X,'PVALUE',   &
                 7X,'CUTOFF0',7X,'CUTOFF01',6X,'CUTOFF025',   &
                 7X,'CUTOFF05',7X,'CUTOFF10',7X,'CUTOF25',   &
                 7X,'CUTOFF50',7X,'CUTOF100')
          WRITE(IOUNI1,299)STATVA,STATCD,PVAL,CUT0,CUT01,CUT025,   &
                           CUT05,CUT10,CUT25,CUT50,CUT100
  299     FORMAT(11E15.7)
        ENDIF
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(STATCD.NE.CPUMIN)THEN
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
        IF(PVAL.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UE  '
          VALUE0=PVAL
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUT0.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF0'
          VALUE0=CUT0
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUT01.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF01'
          VALUE0=CUT01
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUT025.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='F025'
          VALUE0=CUT025
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUT05.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF05'
          VALUE0=CUT05
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUT10.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF10'
          VALUE0=CUT10
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUT25.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF25'
          VALUE0=CUT25
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUT50.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF50'
          VALUE0=CUT50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUT100.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='F100'
          VALUE0=CUT100
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'TIE4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPTIE4--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPTIE4
      SUBROUTINE DPTIFO(IHARG,NUMARG,IDEFFO,ITITFO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE FONT FOR THE TITLE
!              (THE HORIZONTAL STRING ABOVE THE UPPER HORIZONTAL FRAME).
!              THE FONT FOR THE TITLE WILL BE PLACED
!              IN THE HOLLERITH VARIABLE ITITFO.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFFO
!     OUTPUT ARGUMENTS--ITITFO
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
!     VERSION NUMBER--89/2
!     ORIGINAL VERSION--JANUARY   1989.
!     UPDATED         --JULY      2021. "HARDWARE" AS SYNONYM FOR
!                                       "TEKTRONIX
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFO
      CHARACTER*4 ITITFO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
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
      IF(NUMARG.LE.0)GO TO 1199
      IF(IHARG(1).EQ.'FONT')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(NUMARG.EQ.1)GO TO 1150
      GO TO 1160
!
 1150 CONTINUE
      ITITFO=IDEFFO
      GO TO 1180
!
 1160 CONTINUE
      ITITFO=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(ITITFO.EQ.'HARD')ITITFO='TEKT'
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)ITITFO
 1181   FORMAT('THE TITLE FONT HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPTIFO

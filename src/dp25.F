      SUBROUTINE DPREAL(IRD2,ICOL1,ICOL2,MINCO2,MAXCO2,X,N,IFLGSV,
     1                  IXC,NXC,
     1                  ICASRE,IFUNC2,N2,MAXN2,
     1                  IMACRO,IMACNU,IMACCS,
     1                  IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,
     1                  IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                  ICOMCH,ICOMSW,LINETY,IGRPAU,
     1                  ICOLL,ICOLU,ITYPE,NCOLS,NCALL,
     1                  IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,
     1                  MAXRDV,MAXCHV,IFIETY,
     1                  IDECPT,IDATMV,IDATNN,
     1                  IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,
     1                  IREAAS,IREAPC,
     1                  IB,
     1                  IOTERM,IANSLO,MAXLI2,MAXCI2,ILOOST,ILOOLI,
     1                  IREPCH,IMALEV,
     1                  IERRFI,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE PERFORMS A FORMAT-FREE READ
C              OF ONE LINE OF DATA FROM INPUT UNIT = IRD2.
C              ONLY THE CARD COLUMNS BETWEEN ICOL1 AND ICOL2
C              (INCLUSIVELY) ARE SCANNED FOR THE READ.
C              THIS SUBROUTINE GIVES THE DATA ANALYST THE ABILITY
C              TO GET DATA INTO THE MACHINE FROM A VARIETY OF INPUT
C              SOURCES (CARD, TAPE, DISC, ETC.) WITHOUT HAVING
C              TO WORRY ABOUT AND SPECIFY FORMATS.  THE DATA CARD
C              IMAGES MAY BE MADE WITHOUT REGARD TO ANY PARTICULAR
C              FORMAT AND MAY BE ENTERED INTO THE MACHINE
C              WITHOUT DEFINING ANY FORMATS.
C     INPUT  ARGUMENTS--IRD2   = THE INTEGER VALUE SPECIFYING
C                                THE INPUT UNIT FROM WHICH
C                                THE CARD IMAGES WILL COME.
C                     --ICOL1  = THE INTEGER CARD COLUMN NUMBER
C                                WHICH DEFINES THE LOWER BOUND
C                                (INCLUSIVELY) OF THE INTERVAL
C                                ON EACH CARD IMAGE TO BE SCANNED
C                                FOR THE READ.
C                     --ICOL2  = THE INTEGER CARD COLUMN NUMBER
C                                WHICH DEFINES THE UPPER BOUND
C                                (INCLUSIVELY) OF THE INTERVAL
C                                ON EACH CARD IMAGE TO BE SCANNED
C                                FOR THE READ.
C     OUTPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
C                                INTO WHICH THE READ DATA VALUES
C                                WILL BE SEQUENTIALLY PLACED.
C                     --N      = THE INTEGER VALUE
C                                WHICH WILL EQUAL THE NUMBER OF DATA
C                                VALUES WHICH WERE READ.
C     OUTPUT--THE SINGLE PRECISION VECTOR X WHICH
C             WILL CONTAIN THE READ DATA VALUES, AND
C             THE INTEGER VALUE N WHICH WILL EQUAL THE NUMBER OF
C             DATA VALUES READ INTO X.  ALSO, 7 LINES OF SUMMARY
C             INFORMATION WILL BE GENERATED--
C             REGARDING WHAT WAS IN FACT READ INTO THE MACHINE--
C             1) THE VALUES OF ICOL1 AND ICOL2;
C             2) THE (ENTIRE) FIRST DATA CARD READ;
C             3) THE (ENTIRE) LAST DATA CARD READ;
C             4) THE TOTAL NUMBER OF DATA CARDS READ;
C             5) THE TOTAL NUMBER OF DATA VALUES READ.
C     PRINTING--YES.
C     RESTRICTIONS--ICOL1 AND ICOL2 MUST BE BETWEEN 1 AND 132,
C                   INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--ADJACENT DATA VALUES ON THE SAME CARD MUST BE
C              SEPARATED BY AT LEAST 1 BLANK OR 1 ALPHABETIC
C              CHARACTER, OR BY  ANY COMBINATION OF BLANKS AND
C              ALPHABETIC CHARACTERS.  IN THIS CONTEXT, AN
C              ALPHABETIC CHARACTER IS ANY CHARACTER
C              OTHER THAN 0, 1, 2, ..., 9, +, -, OR ..
C              IN EFFECT, THEREFORE, ALL ALPHABETIC INFORMATION
C              IN THE INTERVAL DEFINED BY ICOL1 AND ICOL2
C              (INCLUSIVELY) IS IGNORED FOR READING PURPOSES.
C              ALL INFORMATION (BOTH NUMERIC AND ALPHABETIC)
C              OUTSIDE THE DEFINED INTERVAL IS ALSO IGNORED
C              FOR READING PURPOSES.
C     COMMENT--THE DATA VALUES ON THE CARDS ARE FREE-FORMAT.
C              THEY MAY BE EITHER INTEGER OR FLOATING POINT
C              (THAT IS, WITHOUT OR WITH THE DECIMAL POINTS).
C              EXPONENTIAL FLOATING POINT FORMAT (E FORMAT)
C              IS NOT PERMITTED.  ALL DATA, WHETHER WITHOUT OR WITH
C              THE DECIMAL POINT ON THE CARDS, WILL BE READ INTO
C              THE MACHINE INTO THE X VECTOR AND WILL RESIDE THERE
C              AS FLOATING POINT NUMBERS.
C     COMMENT--ANY PARTICULAR DATA VALUE MUST START AND END
C              ON THE SAME DATA CARD; DATA VALUES MAY NOT
C              START ON ONE CARD AND FINISH ON THE NEXT.
C              VARIOUS ILLEGAL COMBINATIONS (SUCH AS
C              MULTIPLE DECIMAL POINTS, MULTIPLE PLUSSES OR
C              MINUSES, INCOMPLETE VALUES CONSISTING ONLY
C              OF A DECIMAL POINT, OR ONLY OF A SIGN AND A DECIMAL
C              POINT, ETC. ARE NOT ACCEPTED AND THE
C              DATA ANALYST WILL BE INFORMED OF THE EXISTENCE OF
C              SUCH BY AN ERROR DIAGNOSTIC.
C              IN THE EVENT OF SUCH AN ILLEGAL COMBINATION,
C              THAT 'NUMBER' AND ALL REMAINING NUMBERS ON THAT CARD WILL
C              WILL BE IGNORED (NOT READ INTO THE MACHINE)
C              AND THE NEXT DATA CARD WILL THEN BE READ.
C     COMMENT--THIS SUBROUTINE WILL CONTINUOUSLY AND
C              SEQUENTIALLY READ CARDS UNTIL A CARD WITH
C              THE WORD         END       (SOMEWHERE BETWEEN
C              COLUMNS ICOL1 AND ICOL2 (INCLUSIVELY) IS ENCOUNTERED.
C              TO TERMINATE A DATA SET, THE ANALYST SHOULD
C              APPEND SUCH A CARD WHICH HAS THE WORD
C              END        SOMEWHERE IN THE INTERVAL
C              DEFINED BY ICOL1 AND ICOL2.  FOR EXAMPLE, IF
C              ICOL1 = 1 AND ICOL2 = 20, THEN A SEPARATE CARD WITH
C                 END   IN COLUMNS 1, 2, AND 3, OR IN COLUMNS 10, 11,
C              AND 12, ETC.  WOULD TERMINATE THE READ.
C              IT IS IMPORTANT TO APPEND SUCH A CARD--
C              FAILURE TO DO SO WILL RESULT IN AN INCOMPLETE
C              DATA SET OR (ON SOME COMPUTERS) AN
C              UNPREDICTABLE RUN TERMINATION.
C     REFERENCES--NONE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--DECEMBER  1972.
C     UPDATED         --AUGUST    1974.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --OCTOBER   1976.
C     UPDATED         --JANUARY   1977.
C     UPDATED         --MARCH     1977.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1985.
C     UPDATED         --MARCH     1986.
C     UPDATED         --NOVEMBER  1989. READ STRINGS FROM CERTAIN COLUMNS
C     UPDATED         --MAY       1990.  1) IGNORE BLANK LINES
C                                        2) CHECK FOR "D" EXPONENTIAL NOTATION
C                                        3) CHECK FOR COMMENT CHARACTER COL 1
C     UPDATED         --JULY      1990. ICOMFL RENAMED AS ICOMSW
C     UPDATED         --FEBRUARY  1994. WRITE STATEMENTS: 132->80
C     UPDATED         --SEPTEMBER 1995. REPORT BLANK LINE VIA LINETY
C     UPDATED         --JANUARY   1998. CHECK LINE FOR NON-PRINTING
C                                       CHARACTERS (CONVERT TO SPACE)
C     UPDATED         --DECEMBER  1999. ROW LABEL CASE (ROWI)
C     UPDATED         --FEBRUARY  2003. INCREASE MAXIMUM RECORD LENGTH
C                                       THAT CAN BE READ
C     UPDATED         --JANUARY   2004. RECODE FOR BETTER CLARITY
C     UPDATED         --JANUARY   2004. SUPPORT FOR CHARACTER DATA
C     UPDATED         --APRIL     2005. HANDLE BLANK FIELDS FOR
C                                       VECTOR COLUMN LIMITS CASE
C     UPDATED         --APRIL     2005. SUPPORT "," AS DECIMAL POINT
C                                       (FOR INTERNATIONAL)
C     UPDATED         --SEPTEMBER 2006. IF USING COLUMN LIMITS TO READ
C                                       CHARACTER DATA, MAKE SURE FIRST
C                                       OCCURRENCE EXTRACTS FULL STRING
C                                       (I.E., NEED TO ACCOUNT FOR BLANKS)
C     UPDATED         --APRIL     2009. CHECK FOR NaN (NOT A NUMBER)
C                                       IN NUMERIC FIELDS
C     UPDATED         --APRIL     2009. CHECK FOR A "MISSING VALUE"
C                                       CHARACTER STRING IN NUMERIC
C                                       FIELDS (DEFAULT IS "MV", BUT
C                                       IS USER-SETTABLE)
C     UPDATED         --JANUARY   2010. TREAT ASCII 127 AS A
C                                       NON-PRINTING CHARACTER
C     UPDATED         --APRIL     2010. INITIALIZE VECTOR WITH
C                                       DATA VALUES (X) TO MISSING
C                                       VALUE.  THIS WAY, IF A "SHORT"
C                                       LINE IS ENCOUNTERED, MISSING
C                                       VALUES AT END WILL BE SET TO
C                                       MISSING VALUE RATHER THAN ZERO.
C     UPDATED         --APRIL     2010. IF HAVE "MV,", THEN MOVE
C                                       POINTER TO "," POSITION TO
C                                       AVOID EXTRA MISSING VALUE
C     UPDATED         --SEPTEMBER 2012. ADD MISSING VALUE IF RECORD
C                                       ENDS WITH DELIMITER
C     UPDATED         --SEPTEMBER 2012. ALLOW USER TO SPECIFY CERTAIN
C                                       COLUMNS AS CHARACTER
C     UPDATED         --JANUARY   2015. IF DOING A TERMINAL READ WHEN A
C                                       LOOP IS ACTIVE, THEN NEED TO
C                                       EXTRACT LINE FROM SAVED LOOP
C                                       LINES RATHER THAN STANDARD INPUT
C     UPDATED         --JUNE      2016. ADD IDATDL AND ITIMDL TO SUPPORT
C                                       DELIMITERS FOR DATE/TIME FIELDS
C     UPDATED         --JUNE      2016. ADD IRDIPA TO READ IP ADDRESSES
C     UPDATED         --JUNE      2018. CORRECTED ISSUES:
C                                        1) 3Dxxxx SHOULD BE RECOGNIZED
C                                           AS CHARACTER FIELD RATHER
C                                           THAN EXPONENTIAL NUMBER
C                                        2) HANDLE "-3.27+105" CORRECTLY
C                                           (ISSUE OCCURS WHEN THERE WAS
C                                           A SIGN BOTH AT THE BEGINNING
C                                           AND AT THE START OF THE
C                                           EXPONENT)
C     UPDATED         --JUNE      2018. IGRPAU = CATEGORICAL
C     UPDATED         --JULY      2018. SOME UPDATES TO SUPPORT READING
C                                       CHARACTER DATA BASED ON THE
C                                       DELIMITER
C     UPDATED         --AUGUST    2018. ISSUE WHEN SPACE IS DELIMITER
C     UPDATED         --APRIL     2019. BUG WITH VECTOR COLUMN LIMITS
C     UPDATED         --APRIL     2019. SET READ ASTERISK IGNORE
C     UPDATED         --JUNE      2019. MAXIMUM NUMBER OF CHARACTER
C                                       VARIABLES RAISED TO 50
C     UPDATED         --JUNE      2019. BETTER CHECKING FOR LIMITS
C                                       WHEN READING CHARACTER DATA
C     UPDATED         --SEPTEMBER 2019. TO ALLOW READING CHARACTER
C                                       VARIABLES FROM THE TERMINAL,
C                                       USE "IFLGSV" TO PARSE A SAVED
C                                       LINE (TERMINAL READ WILL USE A
C                                       SAVED LINE FOR THE FIRST LINE)
C     UPDATED         --OCTOBER   2019. ADD "IREAPR" TO CHECK FOR
C                                       PERCENT SIGN IN NUMERIC
C                                       VARIABLES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASRE
      CHARACTER*4 IFUNC2
C
      CHARACTER*4 IOTERM
      CHARACTER*4 ILOOST
      CHARACTER*1 IREPCH
      CHARACTER*4 IANSLO(MAXLI2,MAXCI2)
C
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
      CHARACTER*4 IANSLC
      CHARACTER*12 IREACS
      CHARACTER*4 ISTOR1
      CHARACTER*4 ISTOR2
      CHARACTER*4 IEND
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
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IOFILE
C
      CHARACTER*4 IDATMV
      CHARACTER*4 IDATNN
      CHARACTER*4 IREACD
      CHARACTER*4 IREACM
      CHARACTER*4 IREADS
      CHARACTER*4 IREAPC
      CHARACTER*4 IREAPM
      CHARACTER*4 IREAAS
      CHARACTER*8 IREAMC
      CHARACTER*4 IB
      CHARACTER*4 ICHAR3
CCCCC CHARACTER*4 IC
      CHARACTER*4 ICHEXP
C
      CHARACTER*1 IQUOTE
      CHARACTER*4 ICOMCH
      CHARACTER*4 ICOMSW
      CHARACTER*4 LINETY
      CHARACTER*4 IGRPAU
      CHARACTER*4 IREADL
      CHARACTER*4 IREAD2
      CHARACTER*4 IREAD3
      CHARACTER*4 IDATDL
      CHARACTER*4 IDATD2
      CHARACTER*4 ITIMDL
      CHARACTER*4 ITIMD2
      CHARACTER*4 IRDIPA
      CHARACTER*4 IDECPT
C
C---------------------------------------------------------------------
C
      CHARACTER*24 IXC(*)
      DIMENSION X(*)
C
      INTEGER ICOLL(*)
      INTEGER ICOLU(*)
      INTEGER ITYPE(*)
      INTEGER IFIETY(*)
C
      DIMENSION IFUNC2(*)
C
      DIMENSION ISTOR1(*)
      DIMENSION ISTOR2(*)
      DIMENSION IANSLC(*)
      DIMENSION IB(*)
      DIMENSION ICHAR3(41)
      DIMENSION ICHEXP(41)
CCCCC DIMENSION IC(10)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCONP.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
CCCCC DATA IC(1),IC(2),IC(3),IC(4),IC(5),IC(6),IC(7),IC(8),IC(9),IC(10)
CCCCC1/'0','1','2','3','4','5','6','7','8','9'/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRE'
      ISUBN2='AL  '
      ISUBN0='REAL'
      ICOL2S=ICOL2
      IDATD2=IDATDL
      ITIMD2=ITIMDL
      IREAD3=IREADL
      IF(IREADL.EQ.',' .AND. IREACM.EQ.'ON')IREADL='    '
C
C     THE FOLLOWING NULL-CORRECTION WAS MADE IN APRIL OF 1987 (ELGIN PERRY
C     AND DICK ATLEE FROM THE UNIV. OF MARYLAND; UNIVAC COMPILER MESSAGE)
C
      IOFILE='-999'
C
      IEND='NO'
      IERROR='NO'
      DO2I=1,MAXRDV
        X(I)=PREAMV
    2 CONTINUE
      DO4I=1,100
        IFUNC2(I)=' '
    4 CONTINUE
      NPOS3=0
      NLASTZ=0
      N=0
      NXC=0
      I=0
      ICOL22=0
      LINETY='NUME'
      CALL DPCONA(39,IQUOTE)
      IZERO=48
      IF(IHOST1.EQ.'PRIM')IZERO=48+128
      IF(IHOST1.EQ.'IBM')IZERO=240
      IF(IHOST1.EQ.'CDC')IZERO=16
      NCDAMV=0
      DO6I=4,1,-1
        IF(IDATMV(I:I).NE.' ')THEN
          NCDAMV=I
          GOTO7
        ENDIF
    6 CONTINUE
    7 CONTINUE
      NCDNAN=0
      DO8I=4,1,-1
        IF(IDATNN(I:I).NE.' ')THEN
          NCDNAN=I
          GOTO9
        ENDIF
    8 CONTINUE
    9 CONTINUE
C
      IFLAGD=0
      IFLAGQ=0
      IFLGQ2=0
C
C               **************************************
C               **  CHECK FOR VECTOR COLUMN LIMITS  **
C               **************************************
C
      IF(NCALL.EQ.0)THEN
        DO11I=1,MAXRDV
          ITYPE(I)=-1
   11   CONTINUE
        NCOLS=0
        DO20I=1,50
          IF(ICOLL(I).GT.0 .AND. ICOLU(I).GT.0)THEN
            NCOLS=NCOLS+1
          ELSE
            GOTO29
          ENDIF
   20   CONTINUE
   29   CONTINUE
      ENDIF
      NREAD=0
C
C               *************************
C               **  READ A LINE IMAGE  **
C               *************************
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IRD2,ICOL1,ICOL2,IWIDTH,IBUGS2
   52   FORMAT('IRD2,ICOL1,ICOL2,IWIDTH,IBUGS2 = ',4I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MINCO2,MAXCO2,NCALL,NCOLS
   53   FORMAT('MINCO2,MAXCO2,NCALL,NCOLS = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)(IANSLC(I),I=1,MIN(100,IWIDTH))
   56   FORMAT('(IANSLC(I),I=1,IWIDTH) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IMACRO,IMACCS,IREACS,IOUNIT
   57   FORMAT('IMACRO,IMACCS,IREACS,IOUNIT = ',A4,2(2X,A12),2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IOTERM,ILOOST,ILOOLI
   58   FORMAT('IOTERM,ILOOST,ILOOLI = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)ICASRE,IREADL,IDATDL,ITIMDL,IDECPT
   61   FORMAT('ICASRE,IREADL,IDATDL,ITIMDL,IDECPT = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)PREAMV,N2,MAXN2,IMACNU
   62   FORMAT('PREAMV,N2,MAXN2,IMACNU =',F10.5,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IREAMC,IREADS,IREAPM,IREACD,IREACM,IREAAS,IREAPC
   63   FORMAT('IREAMC,IREADS,IREAPM,IREACD,IREACM,IREAAS,IREAPC = ',
     1         A8,6(2X,A4))
        CALL DPWRST('XXX','BUG ')
        DO65I=1,5
          WRITE(ICOUT,67)I,ICOLL(I),ICOLU(I)
   67     FORMAT('I,ICOLL(I),ICOLU(I) = ',3I7)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(MINCO2.LE.ICOL1.AND.ICOL1.LE.MAXCO2.AND.
     1   MINCO2.LE.ICOL2.AND.ICOL2.LE.MAXCO2)GOTO 89
        WRITE(ICOUT,81)
   81   FORMAT('***** ERROR IN DPREAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)
   82   FORMAT('      THE SPECIFIED COLUMN LIMITS ARE OUTSIDE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,83)
   83   FORMAT('      THE ALLOWABLE LIMITS FOR THIS INPUT DEVICE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,84)IRD2
   84   FORMAT('      INPUT UNIT NUMBER     = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,85)ICOL1,ICOL2
   85   FORMAT('      SPECIFIED COLUMN LIMITS = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,86)MINCO2,MAXCO2
   86   FORMAT('      ALLOWABLE COLUMN LIMITS = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
   89 CONTINUE
C
      NUMCRD=0
C
C               *********************************************
C               **  STEP 2--                               **
C               **  IF THE READ IS DONE FROM A FILE,       **
C               **  THEN CARRY OUT THE FILE READ OPERATION.**
C               *********************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,91)IOFILE,IRD,IRD2,IOUNIT,IMACCS
   91   FORMAT('IOFILE,IRD,IRD2,IOUNIT,IMACCS = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IOFILE.EQ.'NO')THEN
C
        IF(IFLGSV.EQ.0)THEN
          IF(IRD2.EQ.IRD)THEN
            READ(IRD2,93,END=8000)(IB(IZ),IZ=1,ICOL2)
   93       FORMAT(255A1)
          ELSEIF(IMACCS.EQ.'OPEN')THEN
            NUMCHA=MAXCO2
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                  IB,NUMCHA,
     1                  ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          ENDIF
        ELSE
          NUMCHA=1
          DO2993KK=MAXCO2,1,-1
            IF(IB(KK).NE.' ')THEN
              NUMCHA=KK
              GOTO2995
            ENDIF
 2993     CONTINUE
 2995     CONTINUE
          IFLGSV=0
        ENDIF
C
      ELSE
        IF(IOTERM.EQ.'LOOP')THEN
          ILOOLI=ILOOLI+1
          ILAST=1
          IFLAG=0
          DO92IZ=MAXCI2,1,-1
            IB(IZ)=IANSLO(ILOOLI,IZ)
            IF(IB(IZ).NE.' ' .AND. IFLAG.EQ.0)THEN
              ILAST=IZ
              IFLAG=1
            ENDIF
   92     CONTINUE
          NUMCHA=ILAST
C
C         CALL DPREP3 TO SEARCH FOR REPLACEMENT CHARACTER.
C         (LINES EXTRACTED FROM SAVED LOOP COMMANDS WILL NOT
C         HAVE THE REPLACEMENT CHARACTER CHECK PERFORMED YET).
C
          CALL DPREP3(IB,NUMCHA,IREPCH,IMALEV,
     1                IBUGS2,IERROR)
          IF(NUMCHA.LT.MAXCI2)THEN
            DO9993IZ=NUMCHA+1,MAXCI2
              IB(IZ)=' '
 9993       CONTINUE
          ENDIF
        ELSE
          NUMCHA=MAXCO2
          IF(IFLGSV.EQ.0)THEN
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                  IB,NUMCHA,
     1                  ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          ELSE
            NUMCHA=1
            DO2998KK=ICOL2,1,-1
              IF(IB(KK).NE.' ')THEN
                NUMCHA=KK
                GOTO2999
              ENDIF
 2998       CONTINUE
 2999       CONTINUE
            IFLGSV=0
          ENDIF
        ENDIF
      ENDIF
C
CCCCC CHECK FOR COMMENT LINE IN DATA FILE
C
      IF(ICOMSW.EQ.'ON  '.AND.IB(1).EQ.ICOMCH(1:1))THEN
         LINETY='BLAN'
         GOTO9000
      ENDIF
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,96)
   96   FORMAT('***** FROM THE MIDDLE OF DPREAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,97)(IB(J)(1:1),J=1,80)
   97   FORMAT('IB(.) = ',80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC REMOVE NON-PRINTING CHARACTERS.  CHECK FOR BLANK LINE
CCCCC AS WELL
CCCCC
CCCCC NOTE 1/2010: TREAT "127" AS PRINTING CHARACTER AS WELL
CCCCC              (THIS IS A "DELETE").
CCCCC
CCCCC              ALSO, CONVERT SPACE, "TAB" OR "READ DELIMITER" TO
CCCCC              SPACE.  OTHERWISE, SIMPLY REMOVE.
CCCCC
CCCCC              2019/02: WHEN READING STRINGS, YOU MAY WANT TO LEAVE
CCCCC                       TAB CHARACTERS INTACT.
CCCCC
CCCCC NOTE 9/2012: DELIMITER CHARACTER SHOULD REMAIN IN LINE
CCCCC              SINCE CONSECUTIVE DELIMITER CHARACTERS IMPLY
CCCCC              A MISSING VALUE SHOULD BE INSERTED.
CCCCC
CCCCC              IF VECTOR COLUMN LIMITS GIVEN, DO NOT CONVERT
CCCCC              MULTIPLE SPACES TO A SINGLE SPACE.  HOWEVER,
CCCCC              IF VECTOR COLUMN LIMITS NOT GIVEN, THEN CHECK
CCCCC              LOWER AND UPPER COLUMN LIMITS.  IF LOWER LIMIT
CCCCC              IS NOT 1, THEN DO NOT COMPRESS MULTIPLE SPACES.
CCCCC              HOWEVER, IF LOWER LIMIT IS 1, THEN ONLY PROCESS
CCCCC              UP TO THE UPPER COLUMN LIMIT AND COMPRESS MULTIPLE
CCCCC              SPACES TO A SINGLE SPACE.
CCCCC
CCCCC              NOTE THAT ANYTHING ENCLOSED IN DOUBLE QUOTES (34)
CCCCC              SINGLE QUOTES (39) ARE TREATED AS JUST A CHARACTER.
CCCCC
CCCCC              ADJUST UPPER COLUMN LIMIT (I.E., SUBTRACT BY 1
CCCCC              WHEN SKIP SPACE)
CCCCC
C
      IVECLM=0
      IF(ICOLL(1).GT.0)IVECLM=1
C
      IBLANK=0
      IFLAGQ=0
      CALL DPCOAN(IREADL(1:1),ITEMPD)
      IF(IDATDL.NE.'NULL')THEN
        CALL DPCOAN(IDATDL(1:1),ITMPD2)
      ELSE
        ITMPD2=-1
        IDATDL=IREADL
      ENDIF
      IF(ITIMDL.NE.'NULL')THEN
        CALL DPCOAN(ITIMDL(1:1),ITMPD3)
      ELSE
        ITMPD3=-1
        ITIMDL=IREADL
      ENDIF
      ICNT=0
      ILAST=MAXCO2
      IFLAGA=0
      IF(NCOLS.EQ.0 .AND. ICOL1.EQ.1)THEN
        ILAST=ICOL2
        IFLAGA=1
      ENDIF
C
CCCCC CHECK FOR VECTOR COLUMN LIMITS, DO NOT STRIP ANY CHARACTERS
CCCCC IF FOUND.
C
      IF(IVECLM.EQ.1)THEN
        ICNT=ICOL2
        DO105J=1,ICOL2
          ISTOR1(J)=ISTOR2(J)
          IF(IB(J).NE.' ')IBLANK=1
  105   CONTINUE
        GOTO109
      ENDIF
C
CCCCC DO103J=1,MAXCO2
      DO103J=1,ICOL2
        ISTOR1(J)=ISTOR2(J)
        CALL DPCOAN(IB(J),ITEMPV)
        IF(ITEMPV.EQ.34)THEN
          IF(IFLAGQ.EQ.0)THEN
            IFLAGQ=1
            ICNT=ICNT+1
            IB(ICNT)=IB(J)
            NLASTZ=ICNT
          ELSE
            IFLAGQ=0
            ICNT=ICNT+1
            IB(ICNT)=IB(J)
            NLASTZ=ICNT
          ENDIF
        ELSEIF(IFLAGQ.EQ.1)THEN
          ICNT=ICNT+1
          IB(ICNT)=IB(J)
          NLASTZ=ICNT
        ELSEIF(ITEMPV.EQ.ITEMPD)THEN
          ICNT=ICNT+1
          IB(ICNT)=IB(J)
          NLASTZ=ICNT
C
C         2016/06: FOR DATE OR TIME DELIMITER, JUST CONVERT TO SPACE.
C
        ELSEIF(ITEMPV.EQ.ITMPD2 .AND. ITMPD2.GT.0)THEN
          ICNT=ICNT+1
          IB(ICNT)=' '
          NLASTZ=ICNT
        ELSEIF(ITEMPV.EQ.ITMPD3 .AND. ITMPD3.GT.0)THEN
          ICNT=ICNT+1
          IB(ICNT)=' '
          NLASTZ=ICNT
        ELSEIF(ITEMPV.LE.32.OR.ITEMPV.GE.127)THEN
          IF(ITEMPV.EQ.32)THEN
            IF(IFLAGA.EQ.1)THEN
              IF(ICNT.GE.1)THEN
                IF(IB(ICNT).NE.' ')THEN
                  ICNT=ICNT+1
                  IB(ICNT)=' '
                ENDIF
              ENDIF
            ELSE
              ICNT=ICNT+1
              IB(ICNT)=' '
            ENDIF
          ELSEIF(ITEMPV.EQ.9)THEN
            IF(IFLAGA.EQ.1)THEN
              IF(ICNT.GE.1)THEN
                IF(IB(ICNT).NE.' ')THEN
                  IF(ITABNC.NE.1)THEN
                    ICNT=ICNT+1
                    IB(ICNT)=IB(J)
                  ELSE
                    ICNT=ICNT+1
                    IB(ICNT)=' '
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              ICNT=ICNT+1
              IB(ICNT)=' '
            ENDIF
          ENDIF
        ELSE
          ICNT=ICNT+1
          IB(ICNT)=IB(J)
          NLASTZ=ICNT
        ENDIF
        IF(ICNT.GE.1)THEN
          IF(IB(ICNT).NE.' ')IBLANK=1
          ISTOR2(ICNT)=IB(ICNT)
        ENDIF
  103 CONTINUE
  109 CONTINUE
C
C     2016/06: IF SET READ IP ADDRESSES ON ENTERED, THEN LOOK FOR
C              FIELDS LIKE THE FOLLOWING:
C
C                 129.6.48.55
C
C              IF THIS FOUND, THEN CONVERT PERIODS TO SPACES SO THAT
C              THIS WILL BE READ AS 4 NUMERIC FIELDS.
C
C              DO NOT DO THIS IF THE FIELD IS EMBEDDED IN QUOTES.
C
      IF(IRDIPA.EQ.'ON')THEN
        IFLAGQ=0
        NDEC=0
C
        DO3103J=1,ICNT
C
C         CHECK FOR QUOTE
C
          IF(IB(J).EQ.'"')THEN
            IF(IFLAGQ.EQ.0)THEN
              IFLAGQ=1
            ELSE
              IFLAGQ=0
            ENDIF
          ENDIF
          IF(IFLAGQ.EQ.1)GOTO3103
C
C         CHECK FOR DECIMALS
C
C         LOOK FOR "." EMBEDDED WITHIN A STRING OF NUMBERS
C
          IF(NDEC.GE.1)THEN
            IF(IB(J).EQ.'.')THEN
              NDEC=NDEC+1
              IF(NDEC.EQ.2)NPOS2=J
              IF(NDEC.EQ.3)NPOS3=J
            ELSE
              CALL DPCOAN(IB(J),ITEMPV)
              IF(ITEMPV.LT.48 .OR. ITEMPV.GT.57)THEN
                IF(NDEC.GE.2)THEN
                  IB(NPOS1)=' '
                  IB(NPOS2)=' '
                  IF(NDEC.GE.3)IB(NPOS3)=' '
                ENDIF
                NDEC=0
              ENDIF
            ENDIF
          ELSE
            IF(IB(J).EQ.'.')NDEC=1
            NPOS1=J
          ENDIF
 3103   CONTINUE
      ENDIF
C
      IF(ICNT.LT.MAXCO2)THEN
        DO104J=ICNT+1,MAXCO2
          IB(J)=' '
          ISTOR2(J)=IB(J)
  104   CONTINUE
      ENDIF
C
      IF(IBLANK.EQ.0)THEN
        LINETY='BLAN'
        GOTO9000
      ENDIF
C
      N2=0
      DO106J=ICOL2,ICOL1,-1
        IF(IB(J)(1:1).NE.' ')THEN
          NLAST=J
          GOTO107
        ENDIF
  106 CONTINUE
      NLAST=ICOL1
  107 CONTINUE
      DO108J=ICOL1,NLAST
        N2=N2+1
        IFUNC2(N2)=IB(J)
  108 CONTINUE
      ICOL22=N2+ICOL1-1
C
      IF(ICASRE.EQ.'FUNC')GOTO8000
C
      IF(IB(1).EQ.'E'.AND.IB(2).EQ.'O'.AND.IB(3).EQ.'F')GOTO8000
      IF(ICOL22.LT.ICOL1)THEN
        LINETY='BLAN'
        GOTO9000
      ENDIF
C
C               *******************************************************
C               **  STEP 4--                                         **
C               **  SCAN FOR THE PHRASE     END DATA                 **
C               **  OR THE PHRASE           END OF DATA              **
C               **  OR THE PHRASE           END OF READ              **
C               **  OR THE PHRASE           EOF                      **
C               **  SCAN FOR THE PHRASE BETWEEN COLUMNS 1 TO ICOL2.  **
C               **  EXCEPTION--IF ICOL2 IS LESS THAT 11              **
C               **  (11 = THE NUMBER OF LETTERS IN THE PHRASE        **
C               **  END OF DATA  )                                   **
C               **  THEN EXPAND THE SCAN TO COVER THE COLUMNS 1 TO 11.*
C               *******************************************************
C
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMAX=ICOL22
      IF(JMAX.GE.3)THEN
        DO130J=1,JMAX-2
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.
     1       (IB(J+1).EQ.'O'.OR.IB(J+1).EQ.'o').AND.
     1       (IB(J+2).EQ.'F'.OR.IB(J+2).EQ.'f'))GOTO8000
  130   CONTINUE
      ENDIF
C
      JMAX=ICOL22
      IF(JMAX.GE.11)THEN
        DO132J=1,JMAX-10
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.
     1       (IB(J+1).EQ.'N'.OR.IB(J+1).EQ.'n').AND.
     1       (IB(J+2).EQ.'D'.OR.IB(J+2).EQ.'d').AND.
     1       (IB(J+3).EQ.' ').AND.
     1       (IB(J+4).EQ.'O'.OR.IB(J+4).EQ.'o').AND.
     1       (IB(J+5).EQ.'F'.OR.IB(J+5).EQ.'f').AND.
     1       (IB(J+6).EQ.' ').AND.
     1       (IB(J+7).EQ.'D'.OR.IB(J+7).EQ.'d').AND.
     1       (IB(J+8).EQ.'A'.OR.IB(J+8).EQ.'a').AND.
     1       (IB(J+9).EQ.'T'.OR.IB(J+9).EQ.'t').AND.
     1       (IB(J+10).EQ.'A'.OR.IB(J+10).EQ.'a'))GOTO8000
  132   CONTINUE
      ENDIF
C
      JMAX=ICOL22
      IF(JMAX.GE.11)THEN
        DO133J=1,JMAX-10
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.
     1       (IB(J+1).EQ.'N'.OR.IB(J+1).EQ.'n').AND.
     1       (IB(J+2).EQ.'D'.OR.IB(J+2).EQ.'d').AND.
     1       (IB(J+3).EQ.' ').AND.
     1       (IB(J+4).EQ.'O'.OR.IB(J+4).EQ.'o').AND.
     1       (IB(J+5).EQ.'F'.OR.IB(J+5).EQ.'f').AND.
     1       (IB(J+6).EQ.' ').AND.
     1       (IB(J+7).EQ.'R'.OR.IB(J+7).EQ.'r').AND.
     1       (IB(J+8).EQ.'E'.OR.IB(J+8).EQ.'e').AND.
     1       (IB(J+9).EQ.'A'.OR.IB(J+9).EQ.'a').AND.
     1       (IB(J+10).EQ.'D'.OR.IB(J+10).EQ.'d'))GOTO8000
  133   CONTINUE
      ENDIF
C
      JMAX=ICOL22
      IF(JMAX.GE.8)THEN
        DO134J=1,JMAX-7
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.
     1       (IB(J+1).EQ.'N'.OR.IB(J+1).EQ.'n').AND.
     1       (IB(J+2).EQ.'D'.OR.IB(J+2).EQ.'d').AND.
     1       (IB(J+3).EQ.' ').AND.
     1       (IB(J+4).EQ.'D'.OR.IB(J+4).EQ.'d').AND.
     1       (IB(J+5).EQ.'A'.OR.IB(J+5).EQ.'a').AND.
     1       (IB(J+6).EQ.'T'.OR.IB(J+6).EQ.'t').AND.
     1       (IB(J+7).EQ.'A'.OR.IB(J+7).EQ.'a'))GOTO8000
  134   CONTINUE
      ENDIF
C
      JMAX=ICOL22
      IF(JMAX.GE.8)THEN
        DO135J=1,JMAX-7
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.
     1       (IB(J+1).EQ.'N'.OR.IB(J+1).EQ.'n').AND.
     1       (IB(J+2).EQ.'D'.OR.IB(J+2).EQ.'d').AND.
     1       (IB(J+3).EQ.' ').AND.
     1       (IB(J+4).EQ.'R'.OR.IB(J+4).EQ.'r').AND.
     1       (IB(J+5).EQ.'E'.OR.IB(J+5).EQ.'e').AND.
     1       (IB(J+6).EQ.'A'.OR.IB(J+6).EQ.'a').AND.
     1       (IB(J+7).EQ.'D'.OR.IB(J+7).EQ.'d'))GOTO8000
  135   CONTINUE
      ENDIF
C
C               *************************************
C               **  STEP 4.2--                     **
C               **  INCREMENT THE NUMBER OF CARDS  **
C               *************************************
C
      ISTEPN='4.2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCRD=NUMCRD+1
      IF(ICASRE.EQ.'ROWI')GOTO9000
      IFLAGD=1
C
      IF(NCOLS.GT.0)ICOL1=ICOLL(1)
      I=ICOL1
C
C  START OF A NEW VARIABLE.  BASIC ALGORITHM IS:
C
C  1) IF FIRST CHARACTER IS A NUMBER OR A "+" OR "-" OR A ".", ASSUME
C     WE HAVE A NUMBER.
C
C     A) A NUMBER MAY CONTAIN A ".", "+", "-", "E", OR "D".
C
C        2005/04: DECIMAL POINT IS NOW USER SETTABLE.
C
C        2018/06: TWO ISSUES WITH EXPONENTIAL FORMAT
C
C                 1) NUMBERS LIKE  3.27+301
C
C                    IF THE EXPONENENT IS 3 DIGITS LONG, THE FORTRAN
C                    COMPILER MAY DROP THE "E" OR "D".  SO IF ENCOUNTER
C                    "+" OR "-" IN THE MIDDLE OF THE FIELD, CHECK FOR
C                    THIS CASE.
C
C                 2) THE STRING   3DResNet   FAILED.  THIS IS DUE TO THE
C                    FACT THAT "3D" IS INTERPRETED AS EXPONENTIAL
C                    NOTATION.  SO WHEN ENCOUNTER A "D" OR "E", CHECK
C                    WHAT FOLLOWS TO MAKE SURE THAT IT IS IN FACT NOT
C                    A CHARACTER FIELD.
C
C     B) IF A SINGLE OR DOUBLE QUOTE IS ENCOUNTERED, END NUMBER
C        AND START A STRING.
C
C     C) IF A SPACE, ",", ":", ";", "/", "\", "[", "(", ")", "]",
C        TREAT AS A DELIMITER.  THAT IS, END THE NUMERIC VARIABLE.
C
C     D) MAKE EXPLICIT CHECK FOR "NAN" AND SYMBOLIC MISSING VALUE
C        (SET BY IDATMV).
C
C     E) ANY OTHER CHARACTER IS ASSUMED TO BE PART OF A CHARACTER
C        VARIABLE.  DEPENDING ON IGRPAU:
C
C        ERROR      - TREAT NON-NUMERIC CHARACTER AS AN ERROR
C        IGNORE     - SKIP THE CURRENT CHARACTER AND ALL SUBSEQUENT
C                     CHARACTERS UNTIL THE NEXT NUMERIC VARIABLE IS
C                     FOUND.
C        CHARACTER  - SEARCH UNTIL ONE OF THE DELIMITERS IS FOUND
C                     AND SAVE STRING IN IXC.
C        CATEGORICAL - SAME AS "CHARACTER" IN THIS ROUTINE
C
C  2) IF FIRST CHARACTER IS A DELIMITER, SIMPLY GO TO NEXT CHARACTER.
C
C     CHECK FOR SPECIAL CASE WHERE DELIMITER IS LAST CHARACTER
C     OF THE RECORD.  IN THIS CASE, NEED TO ADD A MISSING VALUE
C     TO THE LIST.
C
C  3) IF THE FIRST CHARACTER IS A NON-NUMERIC CHARACTER AND NOT A
C     DELIMITER, THEN ASSUME A CHARACTER VARIABLE.  INTERPERT BASED
C     ON VALUE OF IGRPAU AS DESCRIBED ABOVE.
C
C     IF THE FIRST CHARACTER IS A SINGLE OR DOUBLE QUOTE, ASSUME
C     ANY CHARACTERS UNTIL MATCHING QUOTE FOUND IS PART OF THE
C     CHARACTER VARIABLE.
C
C  APRIL 2005.  FOR VECTOR COLUMN LIMITS, CHECK IMMEDIATELY FOR
C               A BLANK FIELD.  IF BLANK FIELD ENCOUNTERED, THEN
C               SET TO MISSING VALUE AND CONTINUE TO NEXT FIELD.
C
C  APRIL 2017.
C
C               1) CHECK FOR EMPTY FIRST FIELD (E.G., ",23,12").  THAT
C                  IS, NEED TO INSERT AN INITIAL MISSING VALUE FOR THIS
C                  CASE.
C
C               2) ALLOW MISSING DATA FOR CHARACTER FIELDS.  USER CAN
C                  SET PREFERRED STRING (ZZZZNULL BY DEFAULT).
C
C               3) CHECK FOR FIELDS LIKE
C
C                      $236,219.11
C
C                  MONETARY DATA MAY ON OCCASSION BE ENTERED THIS WAY.
C                  USER CAN ENTER COMMANDS
C
C                      SET READ DOLLAR SIGN IGNORE <ON/OFF>
C                      SET READ COMMA <ON/OFF>
C
C                  THE FIRST COMMAND SPECIFIES WHETHER A DOLLAR SIGN IN
C                  A NUMERIC FIELD WILL BE IGNORED OR NOT.  THE SECOND
C                  COMMAND SPECIFIES THAT "," IN A NUMERIC FIELD WILL BE
C                  IGNORED.  NOTE THAT THIS MEANS THAT YOU CANNOT SET
C                  THE "," AS A DECIMAL POINT (CONVENTION IN SOME
C                  EUROPEAN COUNTRIES) AND FURTHER THAT YOU CANNOT TREAT
C                  THE COMMA AS A DELIMITER (DATAPLOT HAS NO WAY TO
C                  DISTINGUISH WHETHER YOU INTEND A COMMAND TO MEAN A
C                  DELIMITER OR TO SIGNIFY MONETARY DATA).
C
  149 CONTINUE
      NREAD=NREAD+1
C
C     CHECK IF FIELD IS CHARACTER OR NUMERIC
C
      ITYP=0
      IF(NCALL.GT.0)THEN
        ITYP=ITYPE(NREAD)
      ELSE
        IF(NREAD.GE.1 .AND. NREAD.LE.250)THEN
          IF(IFIETY(NREAD).EQ.1)THEN
            ITYPE(NREAD)=1
            ITYP=1
          ENDIF
        ENDIF
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,30149)NREAD,NCALL,ITYP,NCOLS,I
30149   FORMAT('AT 149: NREAD,NCALL,ITYP,NCOLS,I = ',5I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NCOLS.GT.0)THEN
C
C       CASE WHERE COLUMN LIMITS HAVE BEEN SPECIFIED.  NOTE THAT
C       THIS CASE HAS NOT GONE THROUGH CLEANING PROCESS, SO CHECK
C       FOR LEADING/TRAILING SPACES.
C
        IF(NREAD.GT.NCOLS)GOTO9000
        I=ICOLL(NREAD)
        ICOL22=ICOLU(NREAD)
        DO22120II=I,ICOL22
          IF(IB(II).NE.' ')THEN
            I=II
            GOTO22129
          ENDIF
22120   CONTINUE
22129   CONTINUE
C
        DO22130II=ICOL22,I,-1
          IF(IB(II).NE.' ')THEN
            ICOL22=II
            GOTO22139
          ENDIF
22130   CONTINUE
22139   CONTINUE
C
C       CHECK FOR BLANK FIELD.  ALSO CHECK IF FIRST CHARACTER IS THE
C       FIELD DELIMITER.  IN EITHER CASE, SET TO MISSING VALUE.  BUT
C       NEED TO CHECK WHETHER WE ARE SETTING THE MISSING VALUE FOR A
C       NUMERIC VARIABLE OR A CHARACTER VARIABLE.
C
        IF(IB(I).NE.IREADL)THEN
          DO22140II=I,ICOL22
CCCCC       IF(IB(II).NE.' ')GOTO22149
            IF(IB(II).NE.IREADL)GOTO22149
22140     CONTINUE
        ENDIF
C
        IF(ITYP.EQ.1)THEN
          IF(NXC.LT.MAXCHV)THEN
            NXC=NXC+1
            IXC(NXC)=' '
            IXC(NXC)(1:8)=IREAMC(1:8)
          ENDIF
        ELSE
          N=N+1
          X(N)=PREAMV
        ENDIF
        GOTO149
C
22149   CONTINUE
      ENDIF
C
C     DETERMINE FIRST AND LAST NON-BLANK CHARACTERS IN CURRENT COLUMN
C
C     IF "SET CHARACTER FIELD COMMA DELIMITER ON" COMMAND WAS ENTERED,
C     SEARCH FOR A ",".
C
C     2020/06/22: FOR COMMA DELIMITED FIELDS, CHECK FOR LEADING QUOTE (").
C                 IF FOUND, THEN IGNORE ALL COMMAS UNTIL MATCHING QUOTE
C                 FOUND.
C
      IF(I.GT.ICOL22)GOTO9000
C
      IFRST=I
      ILAST=ICOL22
      ILAST2=ICOL22
      IFLAGZ=0
C
      IF(IREACD.EQ.'ON')THEN
        IFLAGQ=0
        IFLGQ2=0
        DO11150L=I,ICOL22
          IF(IB(L).NE.' ')THEN
            IFRST=L
            GOTO11151
          ENDIF
11150   CONTINUE
        IFRST=ICOL22
11151   CONTINUE
C
        IF(IB(IFRST).EQ.'"')THEN
C
C         IF FIRST CHARACTER IS DOUBLE QUOTE, THEN SEARCH FOR ENDING
C         MATCHING QUOTE.  FOR NOW, DO NOT SUPPORT QUOTES (E.G., \" OR
C         """) EMBEDDED WITHIN QUOTED STRING.  ONCE ENDING QUOTE FOUND,
C         SEARCH FOR THE NEXT COMMA TO INDICATE START OF NEXT FIELD.
C
          IFLAGQ=1
          IFLGQ2=1
          DO81153L=IFRST+1,ICOL22
            IF(IFLAGQ.EQ.1)THEN
              IF(IB(L).EQ.'"')THEN
                ILAST2=L
                IFLAGQ=0
              ENDIF
            ELSE
              IF(IB(L).EQ.',')THEN
                ILAST=L
                GOTO81156
              ENDIF
            ENDIF
81153     CONTINUE
          ILAST=ICOL22
81156     CONTINUE
        ELSE
          DO11153L=IFRST,ICOL22
            IF(IB(L).EQ.',')THEN
              ILAST=L
              GOTO11154
            ENDIF
11153     CONTINUE
          ILAST=ICOL22
11154     CONTINUE
          ILAST2=ILAST
        ENDIF
C
C       IF USER HAS SPECIFIED A READ DELIMITER OTHER THAN A BLANK SPACE,
C       THEN SEARCH FOR THE NEXT DELIMITER.
C
      ELSEIF(IREADL.NE.' ')THEN
        DO12150L=I,ICOL22
          IF(IB(L).NE.' ')THEN
            IFRST=L
            GOTO12151
          ENDIF
12150   CONTINUE
        IFRST=ICOL22
12151   CONTINUE
        DO12153L=IFRST,ICOL22
          IF(IB(L).EQ.IREADL)THEN
            ILAST=L
            ILAST2=L-1
            GOTO12154
          ENDIF
12153   CONTINUE
        ILAST=ICOL22
        ILAST2=ILAST
12154   CONTINUE
      ELSE
C
C       SPACE DELIMITED CASE
C
        DO1153L=I,ICOL22
          IF(IFLAGZ.EQ.0.AND.IB(L).NE.' ')THEN
             IFRST=L
             IFLAGZ=1
             GOTO1153
          ELSEIF(IFLAGZ.EQ.1 .AND.
     1          (IB(L).EQ.' ' .OR. IB(L).EQ.IREADL .OR.
     1           IB(L).EQ.IDATDL .OR. IB(L).EQ.ITIMDL))THEN
            ILAST=L
            ILAST2=L-1
            GOTO1154
          ENDIF
 1153   CONTINUE
 1154   CONTINUE
CCCCC   DO1157L=ICOL22,I,-1
CCCCC     IF(IB(L).NE.' ')THEN
CCCCC       ILAST=L
CCCCC       ILAST2=L
CCCCC       GOTO1159
CCCCC     ENDIF
C1157   CONTINUE
CCCCC   ILAST=IFRST
CCCCC   ILAST2=ILAST
C1159   CONTINUE
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,1148)NREAD,NCALL,I,ICOL22
 1148   FORMAT('NREAD,NCALL,I,ICOL22 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1149)ITYPE(NREAD),IFRST,ILAST,ILAST2
 1149   FORMAT('ITYPE(NREAD),IFRST,ILAST,ILAST2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
  150 CONTINUE
C
      DO151J=1,41
        ICHAR3(J)=' '
  151 CONTINUE
C
      NC=0
      NDP=0
      NSIGN=0
      NCE=0
      NUMDEX=0
      ISUMEX=0
      LOCPT=0
      AFACT=1.0
      NSTR=0
C
      IF(IDECPT.NE.'.')THEN
        IF(IDECPT.EQ.IREADL)THEN
          IF(IREADL.NE.';')THEN
            IREAD2=';'
          ELSE
            IREAD2=':'
          ENDIF
        ENDIF
      ELSE
        IREAD2=IREADL
      ENDIF
C
C               **************************************************
C               **  VECTOR COLUMN LIMITS CASE                   **
C               **************************************************
C
C     NOTE 2012/09: CHECK IF USER HAS SPECIFIED FIELD AS
C                   CHARACTER TYPE.
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,2149)NCALL,NCOLS,ITYPE(NREAD),IFIETY(NREAD),ITYP
 2149   FORMAT('NCALL,NCOLS,ITYPE(NREAD),IFIETY(NREAD),ITYP = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C
C  HANDLE CHARACTER FIELD IMMEDIATELY
C
C  SEPTEMBER 2006: FOR FIRST OCCURENCE (NCALL=0), NEED TO
C                  MAKE SURE WE GET FULL FIELD IF CHARACTER
C                  DATA.
C
C     VECTOR COLUMN LIMITS CASE
C
      IF(NCOLS.GT.0 .AND. ITYP.EQ.1)THEN
        IF(IGRPAU.EQ.'IGNO')GOTO149
        IF(IFRST.LT.ILAST)GOTO154
        IF(IFRST.EQ.ILAST.AND.IB(IFRST).NE.' ')GOTO154
        IF(NXC.LT.MAXCHV)THEN
          NXC=NXC+1
          IXC(NXC)(1:8)=IREAMC(1:8)
        ENDIF
        GOTO149
  154   CONTINUE
        IF(NXC.LT.MAXCHV)THEN
          NXC=NXC+1
          IXC(NXC)=' '
          J=0
          DO158II=IFRST,ILAST
            IF(J.LE.24)THEN
              J=J+1
              IXC(NXC)(J:J)=IB(II)(1:1)
            ENDIF
  158     CONTINUE
        ENDIF
        GOTO149
C
C     NO VECTOR COLUMN LIMITS CASE.  FOR THIS CASE,
C     SEARCH FOR FIRST AND LAST NON-BLANK CHARACTERS.
C     ALSO NEED TO CHECK FOR LEADING QUOTE MARK.
C
C     NOTE: SOME FILES DELINEATE CHARACTER FIELDS BY
C           COMMAS AND DO NOT INCLUDE CHARACTER DATA
C           IN QUOTES EVEN IF IT CONTAINS SPACES.  IF
C           THE COMMAND
C
C              SET CHARACTER FIELD COMMA DELIMITER ON
C
C           WAS ENTERED, THEN DELINEATE FIELDS BY COMMAS
C           IRRESPECTIVE OF SPACES AND QUOTES.
C
      ELSEIF(NCOLS.EQ.0 .AND. ITYP.EQ.1)THEN
C
C       STEP 1: FIND FIRST NON-BLANK CHARACTER
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
          WRITE(ICOUT,21161)I,IFRST,ILAST
21161     FORMAT('AT NCOLS = 0, ITYPE = 1, I,IFRST,ILAST = ',3I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IB(IFRST).EQ.'"' .AND. IB(ILAST2).EQ.'"')IFLGQ2=1
        DO161II=IFRST,ILAST
          IF(IB(II).NE.' ')THEN
            IFRST=II
            GOTO162
          ENDIF
  161   CONTINUE
        GOTO9000
C
C       STEP 2: FIND END OF CURRENT FIELD.
C
C               A) LOOK FOR MATCHING QUOTE
C               B) FOR  "SET CHARACTER FIELD COMMA DELIMITER ON CASE,
C                  SEARCH FOR NEXT ",".
C               C) OTHERWISE SEARCH FOR SPACE OR DELIMITER.
C
  162   CONTINUE
        IF(IFLGQ2.EQ.1)THEN
          ISTRT=IFRST+1
          IFLAGQ=1
        ELSE
          ISTRT=IFRST
          IFLAGQ=0
        ENDIF
        DO164II=ISTRT,ILAST
          IF(IFLAGQ.EQ.1)THEN
            IF(IB(II).EQ.'"')THEN
              ILAST2=II
              IFLAGQ=0
              GOTO164
            ENDIF
          ELSEIF(IREACD.EQ.'ON' .AND. IB(II).EQ.',')THEN
            ILAST=II-1
            GOTO165
          ELSEIF(IB(II).EQ.IREADL .OR.
     1           IB(II).EQ.IDATDL .OR. IB(II).EQ.ITIMDL)THEN
              ILAST=II-1
              GOTO165
          ENDIF
  164   CONTINUE
  165   CONTINUE
        IF(IGRPAU.EQ.'IGNO')THEN
          CONTINUE
        ELSEIF(IFRST.EQ.ILAST.AND.IB(IFRST).EQ.' ')THEN
          IF(NXC.LT.MAXCHV)THEN
            NXC=NXC+1
            IXC(NXC)(1:8)=IREAMC(1:8)
          ENDIF
        ELSE
          IF(NXC.LT.MAXCHV)THEN
            NXC=NXC+1
            IXC(NXC)=' '
            J=0
            IF(IFLGQ2.EQ.1)THEN
              ISTRT=IFRST+1
              ISTOP=ILAST2-1
            ELSE
              ISTRT=IFRST
              ISTOP=ILAST
            ENDIF
            DO168II=ISTRT,ISTOP
              IF(J.LE.24)THEN
                J=J+1
                IXC(NXC)(J:J)=IB(II)(1:1)
              ENDIF
  168       CONTINUE
            IF(J.LE.1 .AND. IXC(NXC)(1:1).EQ.' ')
     1         IXC(NXC)(1:8)=IREAMC(1:8)
          ENDIF
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
            WRITE(ICOUT,169)NXC,IXC(NXC)(1:J),IFRST,ILAST
  169       FORMAT('AT 168: NXC,IXC(NXC),IFRST,ILAST=',I8,A24,2X,2I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
        ENDIF
        IF(IREACD.EQ.'ON')THEN
          I=ILAST+2
        ELSE
          IF(IB(ILAST+1).EQ.IREADL .OR. IB(ILAST+1).EQ.ITIMDL .OR.
     1       IB(ILAST+1).EQ.IDATDL)THEN
            I=ILAST+2
          ELSE
            I=ILAST+1
          ENDIF
        ENDIF
        GOTO149
      ELSEIF(NCOLS.GE.0 .AND. NCALL.EQ.0)THEN
C
C       CHECK FOR CHARACTER FIELD.  CHECK FOR FIRST NON-BLANK CHARACTER
C       AND IF IT IS NOT A NUMBER, A DECIMAL POINT, OR A +/- SIGN, THEN
C       ASSUME CHARACTER.  NOTE THAT THIS IS A QUICK CHECK, MAY MISS
C       SOME CASES.  SO IF YOU HAVE A CHARACTER STRING THAT STARTS WITH
C       A NUMBER, DECIMAL POINT, OR +/- SIGN AND ALSO CONTAINS EMBEDDED
C       SPACES, YOU MAY HAVE A TRUNCATED STRING FOR FIRST LINE.
C
C       AMBIGUOUS CASE: IF FIRST CHARACTER IS ",", THEN DO WE HAVE A
C       NUMERIC OR A CHARACTER FIELD?  IF TYPE IS NOT ALREADY
C       DETERMINED, ASSUME NUMERIC FIELD.
C 
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
          WRITE(ICOUT,21162)I,IFRST,ILAST
21162     FORMAT('AT NCOLS >= 0, NCOL = 0, I,IFRST,ILAST = ',3I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IFRST.EQ.ILAST .AND. IB(IFRST).EQ.' ')GOTO7199
C
C       NOW CHECK FIRST CHARACTER (BUT FIRST CHECK FOR NAN AND
C       MISSING VALUE SINCE THESE WILL BE INTERPRETED AS NUMERIC
C       FIELDS)
C
        IF(ILAST-IFRST+1.EQ.3)THEN
          IF((IB(IFRST).EQ.'N' .OR. IB(IFRST).EQ.'n') .AND.
     1       (IB(IFRST+1).EQ.'A' .OR. IB(IFRST+1).EQ.'a') .AND.
     1       (IB(IFRST+2).EQ.'N' .OR. IB(IFRST+2).EQ.'n') .AND.
     1       IB(IFRST+3).EQ.' ')THEN
            GOTO7199
          ENDIF
        ENDIF
C
        IF(NCDNAN.GT.0 .AND. ILAST-IFRST+1.EQ.NCDNAN .AND.
     1     IDATNN.NE.'NAN')THEN
          DO77167LL=1,NCDNAN
            IF(IB(IFRST+LL-1).NE.IDATNN(LL:LL))GOTO77169
77167     CONTINUE
          GOTO7199
77169     CONTINUE
        ENDIF
C
        IF(NCDAMV.GT.0 .AND. ILAST-IFRST+1.EQ.NCDAMV)THEN
          DO77157LL=1,NCDAMV
            IF(IB(IFRST+LL-1).NE.IDATMV(LL:LL))GOTO77159
77157     CONTINUE
          GOTO7199
77159     CONTINUE
        ENDIF
C
C       IF "SET DOLLAR SIGN IGNORE ON" COMMAND WAS ENTERED,
C       CHECK FOR (AND IGNORE) LEADING "$".  NOTE THAT THIS
C       ONLY APPLIES TO THE FIRST CHARACTER IN THE FIELD.
C
C       ALSO, IF "SET TRAILING PLUS MINUS IGNORE ON" COMMAND
C       WAS ENTERED, IGNORE A "+" OR "-" CHARACTER IN THE LAST
C       COLUMN OF THE FIELD (SOME FILES USE THIS SYNTAX TO
C       INDICATE A VALUE GREATER THAN OR EQUAL TO (+) OR LESS
C       THAN OR EQUAL TO (-).  DATAPLOT WILL SIMPLY IGNORE THIS
C       TRAILING +/- SIGN.
C
C       IF "SET ASTERISK IGNORE ON" COMMAND WAS ENTERED, CHECK
C       FOR EITHER LEADING OR TRAILING ASTERISK.  UP TO 3 TRAILING
C       ASTERISKS ARE CHECKED FOR.
C
        IF(IREADS.EQ.'ON' .AND. IB(IFRST).EQ.'$')IFRST=IFRST+1
        IF(IREAAS.EQ.'ON' .AND. IB(IFRST).EQ.'*')IFRST=IFRST+1
        IF(IREAPM.EQ.'ON' .AND. IB(ILAST).EQ.'+')IB(ILAST)=' '
        IF(IREAPM.EQ.'ON' .AND. IB(ILAST).EQ.'-')IB(ILAST)=' '
        IF(IREAPC.EQ.'ON' .AND. IB(ILAST).EQ.'%')IB(ILAST)=' '
        IF(IREAPC.EQ.'ON' .AND. IB(IFRST).EQ.'%')IFRST=IFRST+1
        IF(IREAAS.EQ.'ON' .AND. IB(ILAST).EQ.'*')THEN
          IB(ILAST)=' '
          IF(IB(ILAST-1).EQ.'*')THEN
            IB(ILAST-1)=' '
            IF(IB(ILAST-2).EQ.'*')THEN
              IB(ILAST-2)=' '
            ENDIF
          ENDIF
        ELSEIF(IREAAS.EQ.'ON' .AND. IB(ILAST).EQ.' ' .AND.
     1         IB(ILAST-1).EQ.'*')THEN
          IB(ILAST-1)=' '
          IF(IB(ILAST-2).EQ.'*')THEN
            IB(ILAST-2)=' '
            IF(IB(ILAST-3).EQ.'*')THEN
              IB(ILAST-3)=' '
            ENDIF
          ENDIF
        ENDIF
        IF(IB(IFRST).EQ.'.')GOTO7199
        IF(IB(IFRST).EQ.'+')GOTO7199
        IF(IB(IFRST).EQ.'-')GOTO7199
        IF(IB(IFRST).EQ.'0')GOTO7199
        IF(IB(IFRST).EQ.'1')GOTO7199
        IF(IB(IFRST).EQ.'2')GOTO7199
        IF(IB(IFRST).EQ.'3')GOTO7199
        IF(IB(IFRST).EQ.'4')GOTO7199
        IF(IB(IFRST).EQ.'5')GOTO7199
        IF(IB(IFRST).EQ.'6')GOTO7199
        IF(IB(IFRST).EQ.'7')GOTO7199
        IF(IB(IFRST).EQ.'8')GOTO7199
        IF(IB(IFRST).EQ.'9')GOTO7199
        IF(IB(IFRST).EQ.',')GOTO7199
C
        IF(NXC.LT.MAXCHV)THEN
          NXC=NXC+1
          IXC(NXC)=' '
          J=0
C
          IF(IB(IFRST).EQ.'"' .AND. IB(ILAST2).EQ.'"')IFLGQ2=1
          IF(IFLGQ2.EQ.1)THEN
            ISTRT=IFRST+1
            ISTOP=ILAST2-1
          ELSE
            ISTRT=IFRST
            ISTOP=ILAST
          ENDIF
          DO7158II=ISTRT,ISTOP
            IF(II.EQ.ILAST .AND. IB(II).EQ.IREADL)GOTO7158
            IF(J.LE.24)THEN
              J=J+1
              IXC(NXC)(J:J)=IB(II)(1:1)
            ENDIF
 7158     CONTINUE
          IF(J.LE.1 .AND. IXC(NXC)(1:1).EQ.' ')IXC(NXC)(1:8)=IREAMC(1:8)
        ENDIF
        ITYPE(NREAD)=1
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
          WRITE(ICOUT,7156)NXC,IXC(NXC)(1:J),IFRST,ILAST,J
 7156     FORMAT('AT 7156: NXC,IXC(NXC),IFRST,ILAST,J=',I8,A24,3I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        I=ILAST+1
CCCCC   IF(IREADL.NE.' ' .AND. IB(ILAST).EQ.IREADL)THEN
CCCCC     I=ILAST+2
CCCCC   ELSE
CCCCC     I=ILAST+1
CCCCC   ENDIF
        GOTO149
      ENDIF
C
 7199 CONTINUE
      IF(NCALL.EQ.0)ITYPE(NREAD)=1
C
C               **************************************************
C               **  STEP 6--                                    **
C               **  EXAMINE THE I-TH CHARACTER IN THIS STRING,  **
C               **************************************************
C
C
C     NOTE 4/2009: CHECK FOR "NaN" OR MISSING VALUE FIRST
C     NOTE 4/2010: ALLOW USER SPECIFIED "NAN" VALUE
C
      IF(ILAST2-IFRST+1.EQ.3)THEN
        IF((IB(IFRST).EQ.'N' .OR. IB(IFRST).EQ.'n') .AND.
     1       (IB(IFRST+1).EQ.'A' .OR. IB(IFRST+1).EQ.'a') .AND.
     1       (IB(IFRST+2).EQ.'N' .OR. IB(IFRST+2).EQ.'n'))THEN
          IF(NCALL.EQ.0)THEN
            ITYPE(NREAD)=0
            IF(NREAD.GE.1 .AND. NREAD.LE.250 .AND. 
     1         IFIETY(NREAD).EQ.1)ITYPE(NREAD)=1
          ENDIF
          N=N+1
          X(N)=PREAMV
          I=I+3
          IF(I.LE.ICOL22 .OR. NCOLS.GT.0)GOTO149
          GOTO9000
        ENDIF
      ENDIF
C
C     FOR NAN VALUE, WE DON'T WANT IT TO BE CASE SENSITIVE.
C     SO CONVERT TO UPPER CASE BEFORE COMPARE.
C
      IF(NCDNAN.GT.0 .AND. ILAST2-IFRST+1.EQ.NCDNAN .AND.
     1   IDATNN.NE.'NAN')THEN
        DO77281LL=1,NCDNAN
          CALL DPCOAN(IB(IFRST+LL-1),IASC1)
          CALL DPCOAN(IDATNN(LL:LL),IASC2)
          IF(IASC1.GE.97 .AND. IASC1.LE.122)IASC1=IASC1-32
          IF(IASC2.GE.97 .AND. IASC2.LE.122)IASC2=IASC2-32
          IF(IASC1.NE.IASC2)GOTO77289
77281   CONTINUE
        IF(NCALL.EQ.0)THEN
          ITYPE(NREAD)=0
          IF(NREAD.GE.1 .AND. NREAD.LE.250 .AND. 
     1       IFIETY(NREAD).EQ.1)ITYPE(NREAD)=1
        ENDIF
        N=N+1
        X(N)=PREAMV
        I=I+NCDNAN
        IF(IB(I+1).EQ.IREADL .OR. IB(I+1).EQ.IDATDL .OR.
     1     IB(I+1).EQ.ITIMDL)I=I+1
        IF(I.LE.ICOL22 .OR. NCOLS.GT.0)GOTO149
        GOTO9000
77289   CONTINUE
      ENDIF
C
C     FOR MISSING VALUE, WE DON'T WANT IT TO BE CASE SENSITIVE.
C     SO CONVERT TO UPPER CASE BEFORE COMPARE.
C
      IF(NCDAMV.GT.0 .AND. ILAST2-IFRST+1.EQ.NCDAMV)THEN
        DO77181LL=1,NCDAMV
          CALL DPCOAN(IB(IFRST+LL-1),IASC1)
          CALL DPCOAN(IDATMV(LL:LL),IASC2)
          IF(IASC1.GE.97 .AND. IASC1.LE.122)IASC1=IASC1-32
          IF(IASC2.GE.97 .AND. IASC2.LE.122)IASC2=IASC2-32
          IF(IASC1.NE.IASC2)GOTO77189
77181   CONTINUE
        IF(NCALL.EQ.0)THEN
          ITYPE(NREAD)=0
          IF(NREAD.GE.1 .AND. NREAD.LE.250 .AND. 
     1       IFIETY(NREAD).EQ.1)ITYPE(NREAD)=1
        ENDIF
        N=N+1
        X(N)=PREAMV
        I=I+NCDAMV
        IF(IB(I+1).EQ.IREADL .OR. IB(I+1).EQ.IDATDL .OR.
     1     IB(I+1).EQ.ITIMDL)I=I+1
        IF(I.LE.ICOL22 .OR. NCOLS.GT.0)GOTO149
        GOTO9000
77189   CONTINUE
      ENDIF
C
  160 CONTINUE
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1161)I,IB(I),N,NXC
 1161   FORMAT('I,IB(I),N,NXC = ',I8,2X,A4,2X,2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************
C               **  STEP 6.1--             **
C               **  TREAT THE 0 TO 9 CASE  **
C               *****************************
C
C     CHECK IF LAST CHARACTER IS THE READ DELIMITER
C
C     2019/10: IGNORE "%" IF LAST CHARACTER IN FIELD
C
      IBASCI=ICHAR(IB(I)(1:1)) - IZERO
      IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
        IF(NC.EQ.0 .AND. NSIGN.EQ.0)NSIGN=NSIGN+1
        NC=NC+1
        ICHAR3(NC)=IB(I)
        I=I+1
        IF(I.LE.ILAST)GOTO160
        GOTO1050
      ELSEIF(IB(I).EQ.',' .AND. IREACM.EQ.'ON')THEN
        I=I+1
        GOTO160
      ELSEIF(IB(I).EQ.'%')THEN
        IF(IREAPC.EQ.'ON')THEN
          IF(I.EQ.ILAST)GOTO1050
          IF(IB(I+1).EQ.IREADL .AND. I.EQ.ILAST-1)GOTO1050
        ENDIF
      ELSEIF(IB(I).EQ.'+' .OR. IB(I).EQ.'-')THEN
C
C               **************************
C               **  STEP 6.3--          **
C               **  TREAT THE +/- CASE  **
C               **************************
C
C       2017/04: IGNORE "+/-" IF LAST CHARACTER IN FIELD
C
C       2018/06: ISSUE WITH "-3.27+309".  NOTE THAT "3.27+309" DOES
C                WORK AS EXPECTED.  THE ISSUE IS WHEN THERE ARE 2
C                "+/-" SIGNS IN THE FIELD.
C
        ISTEPN='6.3'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       IGNORE TRAILING +/- OR TRAILING % IF REQUESTED
C
C       NOTE THAT ILAST MAY BE THE DELIMITER, SO MAY NEED TO
C       CHECK CHARACTER BEFORE DELIMITER
C
        IF(IREAPM.EQ.'ON')THEN
          IF(I.EQ.ILAST)GOTO1050
          IF(IB(I+1).EQ.IREADL .AND. I.EQ.ILAST-1)GOTO1050
        ENDIF
C
C       FIRST CHECK IF THIS +/- IS FIRST CHARACTER OF FIELD
C
        IF(I.EQ.IFRST)THEN
          NSIGN=NSIGN+1
          AFACT=1.0
          IF(IB(I).EQ.'-')AFACT=-1.0
          I=I+1
          IF(I.LE.ICOL22)GOTO160
          N=N+1
          X(N)=AFACT
          GOTO149
        ENDIF
C
C       NOW CASE WHERE +/- OCCURS IN MIDDLE OF FIELD.  NOTE THAT
C       ONLY ONE +/- SIGN SHOULD OCCUR (NOT INCLUDING A LEADING
C       +/- SIGN).
C
C
        IF(NC.NE.0)THEN
          NSIGN=NSIGN+1
          IF(NSIGN.GT.2)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
 2001       FORMAT('***** INPUT    DATA ERROR--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2022)
 2022       FORMAT('      AN UNEXPECTED  PLUS OR MINUS HAS OCCURRED ',
     1            'IN THE MIDDLE OF A DATA VALUE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2021)
 2021       FORMAT('      THIS ILLEGAL DATA VALUE AND ALL SUBSEQUENT ',
     1             'DATA VALUES ON THIS LINE HAVE BEEN DELETED')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO8100
          ENDIF
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
            WRITE(ICOUT,3001)I,IB(I)
 3001       FORMAT('AT START OF EXPONENT EVALUATION--I,IB(I) = ',
     1             I6,2X,A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          NCE=1
          NUMDEX=NCE-1
          ICHEXP(1)=IB(I)
          I=I+1
          IF(I.LE.ICOL22)THEN
 3160       CONTINUE
C
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
              WRITE(ICOUT,3002)I,IB(I),IREADL
 3002         FORMAT('IN MIDST OF EXPONENT EXTRACTION--I,IB(I) = ',
     1               I6,2X,A1)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
C           ONLY NUMERIC VALUES ALLOWED AT THIS POINT (HOWEVER,
C           CHECK FOR DELIMITER)
C
            IF(IB(I).EQ.' ' .OR. IB(I).EQ.IREADL)THEN
              GOTO1050
            ENDIF
C
            IBASCI=ICHAR(IB(I)(1:1)) - IZERO
            IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
              NCE=NCE+1
              NUMDEX=NCE-1
              ICHEXP(NCE)=IB(I)
              I=I+1
              IF(I.LE.ICOL22)GOTO3160
              GOTO1050
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2051)
 2051         FORMAT('      A NON-NUMERIC CHARACTER HAS OCCURRED IN ',
     1               'THE EXPONENT ON THE CURRENT LINE.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2052)IB(I)
 2052         FORMAT('THE ILLEGAL CHARACTER WAS ',A1)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8100
            ENDIF
            GOTO1050
          ENDIF
          GOTO1050
        ENDIF
C
C               **************************
C               **  STEP 6.3B-          **
C               **  TREAT THE *   CASE  **
C               **************************
C
C       2019/04: LEADING OR TRAILING ASTERISKS CAN OPTIONALLY BE IGNORED
C
      ELSEIF(IB(I).EQ.'*')THEN
        ISTEPN='6.3B'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       IGNORE LEADING/TRAILING IF REQUESTED
C
C       NOTE THAT ILAST MAY BE THE DELIMITER, SO MAY NEED TO
C       CHECK CHARACTER BEFORE DELIMITER.  NOTE THAT FROM 1 TO
C       3 ASTERISKS MAY BE IGNORED.
C
        IF(IREAAS.EQ.'ON')THEN
          IF(IB(I+1).EQ.'*' .AND. IB(I+2).EQ.'*')THEN
C
C           THREE ASTERISKS
C
            IF(I+2.EQ.ILAST)GOTO1050
            IF(IB(I+3).EQ.IREADL .AND. I.EQ.ILAST-3)GOTO1050
          ELSEIF(IB(I+1).EQ.'*')THEN
C
C           TWO ASTERISKS
C
            IF(I+1.EQ.ILAST)GOTO1050
            IF(IB(I+2).EQ.IREADL .AND. I.EQ.ILAST-2)GOTO1050
          ELSE
C
C           SINGLE ASTERISK
C
            IF(I.EQ.ILAST)GOTO1050
            IF(IB(I+1).EQ.IREADL .AND. I.EQ.ILAST-1)GOTO1050
          ENDIF
        ENDIF
C
C       CHECK IF * IS FIRST CHARACTER OF FIELD
C
        IF(I.EQ.IFRST)THEN
          I=I+1
          IF(I.LE.ICOL22)GOTO160
          N=N+1
          X(N)=0.0
          GOTO149
        ENDIF
C
C       NOW CASE WHERE * OCCURS IN MIDDLE OF FIELD.  THIS IS AN
C       ERROR.
C
        IF(NC.NE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2032)
 2032     FORMAT('      AN UNEXPECTED  ATERISK ("*") HAS OCCURRED ',
     1           'IN THE MIDDLE OF A DATA VALUE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2021)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO8100
        ENDIF
C
      ELSEIF(IB(I).EQ.IDECPT)THEN
C
C               *****************************************
C               **  STEP 7--                           **
C               **  TREAT THE DECIMAL POINT CASE--     **
C               *****************************************
C
        ISTEPN='7'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(NC.EQ.0 .AND. NSIGN.EQ.0)THEN
          AFACT=1.0
          NSIGN=NSIGN+1
        ENDIF
        NC=NC+1
        ICHAR3(NC)=IB(I)
        NDP=NDP+1
        I=I+1
        LOCPT=NC
        IF(I.LE.ICOL22)GOTO160
        IF(NC.GE.2)THEN
          IF(NDP.EQ.1)THEN
            GOTO1050
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2005)
 2005       FORMAT('***** INPUT     DATA ERROR--',
     1             'SOME DATA VALUE ON THE CARD BELOW ',
     1             'HAS MULTIPLE DECIMAL POINTS *****')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2021)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO8100
          ENDIF
        ELSE        
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IF(NC.EQ.0)THEN
            WRITE(ICOUT,2007)
 2007       FORMAT('***** INPUT     DATA ERROR--',
     1             'THE LAST DATA VALUE ON THE CARD BELOW',
     1             'CONSISTS ONLY OF A DECIMAL POINT')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(NC.EQ.1)THEN
            WRITE(ICOUT,2006)IDECPT,IDECPT
 2006       FORMAT('***** INPUT     DATA ERROR--',
     1             'THE LAST DATA VALUE ON THE CARD BELOW ',
     1             'CONSISTS OF ONLY A    +',A1,'  OR   -',A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,2021)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO8100
        ENDIF
      ELSEIF((IB(I).EQ.'E'.OR.IB(I).EQ.'e'.OR.
     1       IB(I).EQ.'D'.OR.IB(I).EQ.'d') .AND. NC.GT.0)THEN
C
C               ***************************************************
C               **  STEP 10--                                    **
C               **  FORM THE VECTOR ICHEXP(.) WHICH              **
C               **  CONTAINS CHARACTERS OF THE EXPONENT (IF ANY).**
C               ***************************************************
C
C       2018/06: CHECK TO ENSURE THAT COLUMNNS TO RIGHT OF "D" OR "E"
C                CONTAIN NUMERIC VALUES.
C
        ISTEPN='10'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        NCE=0
        I=I+1
        IF(I.GT.ICOL22)GOTO1050
C
C       CHECK IF FIRST CHARACTER IS "+/-" SIGN.  IF NO SIGN GIVEN,
C       ASSUME EXPONENT IS POSITIVE.
C
        IF(IB(I).NE.'+' .AND. IB(I).NE.'-')THEN
            NCE=NCE+1
            NUMDEX=NCE-1
            ICHEXP(NCE)='+'
          ELSEIF(IB(I).EQ.'+' .OR. IB(I).EQ.'-')THEN
            NCE=NCE+1
            NUMDEX=NCE-1
            ICHEXP(NCE)=IB(I)
            I=I+1
          ENDIF
C
          IF(I.GT.ICOL22)GOTO1050
C
 4160     CONTINUE
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
            WRITE(ICOUT,3002)I,IB(I)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,33002)NCE,NUMDEX,ICHEXP(NCE)
33002       FORMAT('NCE,NUMDEX,ICHEXP(NCE) = ',2I5,2X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
C         AFTER  "+/-", CHARACTERS SHOULD BE NUMERIC
C
          IBASCI=ICHAR(IB(I)(1:1)) - IZERO
          IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
            NCE=NCE+1
            NUMDEX=NCE-1
            ICHEXP(NCE)=IB(I)
            I=I+1
            IF(I.LE.ICOL22)GOTO4160
            GOTO1050
CCCCC     ELSEIF(IB(I).EQ.'+'.OR.IB(I).EQ.'-'.OR.IB(I).EQ.IDECPT)THEN
          ELSEIF(IB(I).EQ.' ' .OR. IB(I).EQ.IREADL)THEN
            GOTO1050
          ELSE
C
C           2018/06: NON-NUMERIC CHARACTER FOUND.  IF THIS IS THE
C                    FIRST ROW, DEFINE THIS FIELD AS CHARACTER,
C                    OTHERWISE REPORT AN ERROR.
C
            IF(NCALL.GT.0)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2051)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2052)IB(I)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8100
            ELSE
              IF(NXC.LT.MAXCHV)THEN
                NXC=NXC+1
                IXC(NXC)=' '
                J=0
                DO7258II=IFRST,ILAST
                  IF(IREACD.EQ.'ON' .AND. IB(II).EQ.',')THEN
                    I=II
                    GOTO7259
CCCCC             ELSEIF(IB(II).EQ.' ' .OR. IB(II).EQ.IREADL)THEN
                  ELSEIF(IB(II).EQ.IREADL)THEN
                    I=II
                    GOTO7259
                  ENDIF
                  IF(J.LE.24)THEN
                    J=J+1
                    IXC(NXC)(J:J)=IB(II)(1:1)
                  ENDIF
 7258           CONTINUE
 7259           CONTINUE
                ITYPE(NREAD)=1
              ENDIF
              GOTO149
            ENDIF
            GOTO1050
          ENDIF
          GOTO1050
C
        GOTO1050
      ELSE
C
CCCCCC  2013/04: BUG FIX FOR ICHAR3
CCCCCC  IF(NC.EQ.1.AND.ICHAR3(I).EQ.IDECPT.AND.IGRPAU.EQ.'ERRO')THEN
        IF(NC.EQ.1)THEN
          IF(ICHAR3(NC).EQ.IDECPT.AND.IGRPAU.EQ.'ERRO')THEN
C
C               ****************************************************
C               **  STEP 8.3--                                    **
C               **  TREAT THE SPECIAL CASE WHEN THE SECOND OR MORE**
C               **  CHARACTER  IS ALPHABETIC, BLANK, ETC.         **
C               ****************************************************
C
            ISTEPN='8.3'
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2009)IDECPT,IDECPT
 2009       FORMAT('***** INPUT    DATA ERROR--',
     1             'SOME DATA VALUE ON THE CARD BELOW ',
     1           'CONSISTS OF ONLY A   +',A1,' OR -',A1)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2021)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO8100
          ENDIF
        ENDIF
        IF(NC.GE.1)THEN
          IF(IB(I).EQ.' ' .OR. IB(I).EQ.IREAD2)GOTO1050
        ENDIF
        IQFLAG=0
        ISFLAG=0
 6160   CONTINUE
C
        IF(IB(I).EQ.' ')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.IREAD2)THEN
          IDELIM=1
          IF(NC.EQ.0)THEN
            IF(NCALL.EQ.0)THEN
              N=N+1
              X(N)=PREAMV
              ITYPE(NREAD)=0
            ELSE
              IF(ITYP.EQ.0)THEN
                N=N+1
                X(N)=PREAMV
              ELSE
                IF(NXC.LT.MAXCHV)THEN
                  NXC=NXC+1
                  IXC(NXC)=' '
                ENDIF
              ENDIF
            ENDIF
            I=I+1
            IF(I.LE.ICOL22)GOTO149
            GOTO1040
          ENDIF
        ELSEIF(IB(I).EQ.IREAD2)THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.':')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.';')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.'%')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.'/')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.'(')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.'[')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.')')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.']')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.IBASLC)THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.'"')THEN
          IDELIM=2
        ELSEIF(IB(I).EQ.IQUOTE)THEN
          IDELIM=3
        ELSE
          IDELIM=0
        ENDIF
C
        IF(NC.EQ.0 .AND. ISFLAG.EQ.0)THEN
C
C            *******************************************************
C            **  STEP 8.1--                                       **
C            **  TREAT THE SPECIAL CASE WHEN THE LEADING CHARACTER**
C            **  IS ALPHABETIC, BLANK, ETC.                       **
C            *******************************************************
C
          ISTEPN='8.1'
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,6081)IDELIM,ISFLAG,IQFLAG,NC,NSTR,NSIGN
 6081       FORMAT('IDELIM,ISFLAG,IQFLAG,NC,NSTR,NSIGN=',6I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IF(NSIGN.EQ.1)THEN
            N=N+1
            X(N)=AFACT
            GOTO149
          ENDIF
C
C  FOR ALL CASES, TREAT DELIMITER
C
          IF(IGRPAU.EQ.'ERRO')THEN
            I=I+1
            IF(I.LE.ICOL22)GOTO160
            GOTO9000
          ELSEIF(IGRPAU.EQ.'IGNO' .OR. IGRPAU.EQ.'CHAR' .OR.
     1           IGRPAU.EQ.'CATE')THEN
            IF(IDELIM.EQ.0)THEN
              IF(NXC.LT.MAXCHV)THEN
                NXC=NXC+1
                IXC(NXC)=' '
                NSTR=1
                IXC(NXC)(NSTR:NSTR)=IB(I)(1:1)
              ENDIF
              ISFLAG=1
              NC=NC+1
              I=I+1
              IF(I.LE.ICOL22)GOTO6160
              GOTO1040
            ELSEIF(IDELIM.EQ.1)THEN
              I=I+1
              IF(I.LE.ICOL22)GOTO150
              IF(NCOLS.GT.0)THEN
                N=N+1
                X(N)=PREAMV
                IF(NCALL.EQ.0)ITYPE(NREAD)=0
                GOTO149
              ELSE
                GOTO9000
              ENDIF
            ELSEIF(IDELIM.EQ.2)THEN
              ISFLAG=1
              IF(NXC.LT.MAXCHV)THEN
                NXC=NXC+1
                IXC(NXC)=' '
              ENDIF
              IQFLAG=1
              IF(I.LE.ICOL22)GOTO6160
              GOTO1040
            ELSEIF(IDELIM.EQ.3)THEN
              ISFLAG=1
              IF(NXC.LT.MAXCHV)THEN
                NXC=NXC+1
                IXC(NXC)=' '
              ENDIF
              IQFLAG=2
              IF(I.LE.ICOL22)GOTO6160
              GOTO1040
            ENDIF
          ENDIF
        ELSEIF(NC.GE.1 .OR. ISFLAG.EQ.1)THEN
          IF(IGRPAU.EQ.'ERRO')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6009)
 6009       FORMAT('***** INPUT    DATA ERROR--',
     1             'SOME DATA VALUE ON THE CARD BELOW ',
     1             'CONSISTS OF A NON-NUMERIC CHARACTER.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2021)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO8100
          ELSEIF(IGRPAU.EQ.'IGNO' .OR. IGRPAU.EQ.'CHAR' .OR.
     1           IGRPAU.EQ.'CATE')THEN
            IF(IQFLAG.EQ.1)THEN
              IF(IDELIM.EQ.2)THEN
                IQFLAG=0
                ISFLAG=0
                I=I+1
                IF(I.LE.ICOL22)GOTO149
                GOTO1040
              ELSE
                IF((IGRPAU.EQ.'CHAR'.OR.IGRPAU.EQ.'CATE') .AND.
     1             NXC.LE.MAXCHV)THEN
                  IF(NSTR.LE.24)THEN
                    NSTR=NSTR+1
                    IXC(NXC)(NSTR:NSTR)=IB(I)(1:1)
                  ENDIF
                ENDIF
                I=I+1
                IF(I.LE.ICOL22)GOTO6160
                GOTO1040
              ENDIF
            ELSEIF(IQFLAG.EQ.2)THEN
              IF(IDELIM.EQ.3)THEN
                IQFLAG=0
                I=I+1
                IF(I.LE.ICOL22)GOTO149
                GOTO1040
              ELSE
                IF((IGRPAU.EQ.'CHAR'.OR.IGRPAU.EQ.'CATE') .AND.
     1             NXC.LE.MAXCHV)THEN
                  IF(NSTR.LE.24)THEN
                    NSTR=NSTR+1
                    IXC(NXC)(NSTR:NSTR)=IB(I)(1:1)
                  ENDIF
                ENDIF
                I=I+1
                IF(I.LE.ICOL22)GOTO6160
                GOTO1040
              ENDIF
            ENDIF
            IF(IDELIM.EQ.0)THEN
              IF((IGRPAU.EQ.'CHAR'.OR.IGRPAU.EQ.'CATE') .AND.
     1           NXC.LE.MAXCHV)THEN
                IF(NSTR.LE.24)THEN
                  NSTR=NSTR+1
                  IXC(NXC)(NSTR:NSTR)=IB(I)(1:1)
                ENDIF
              ENDIF
              NC=NC+1
              I=I+1
              IF(I.LE.ICOL22)GOTO6160
              GOTO1040
            ELSEIF(IDELIM.EQ.1)THEN
              IF(ISFLAG.EQ.1)ISFLAG=0
              I=I+1
              IF(I.LE.ICOL22)GOTO149
              GOTO1040
            ELSEIF(IDELIM.EQ.2)THEN
              NC=0
              IQFLAG=1
              ISFLAG=1
              IF(I.LE.ICOL22)GOTO6160
              GOTO1040
            ELSEIF(IDELIM.EQ.3)THEN
              NC=0
              IQFLAG=2
              ISFLAG=1
              IF(I.LE.ICOL22)GOTO6160
              GOTO1040
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      GOTO1050
C
C               **************************************
C               **  FOR VECTOR COLUMN LIMITS, GO TO **
C               **  NEXT FIELD.                     **
C               **************************************
C
 1040 CONTINUE
      IF(NCOLS.GT.0)GOTO150
      GOTO9000
C
C               **************************************
C               **  STEP 12--                       **
C               **  OPERATE ON THE ICHAR3(.) VECTOR **
C               **************************************
C
 1050 CONTINUE
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1051)NC
 1051   FORMAT('NC = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1052)(ICHAR3(IZ),IZ=1,NC)
 1052   FORMAT('ICHAR3(.) = ',30A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ************************************************
C               **  STEP 12.1--                               **
C               **  LOCATE THE POSITION OF THE DECIMAL POINT  **
C               ************************************************
C
      IF(NCALL.EQ.0)ITYPE(NREAD)=0
      IF(NC.EQ.0)THEN
        IF(NSIGN.EQ.1)THEN
          N=N+1
          X(N)=AFACT
        ELSEIF(NCOLS.GT.0)THEN
          N=N+1
          X(N)=PREAMV
        ENDIF
        GOTO149
      ENDIF
C
      IF(NDP.EQ.0)THEN
        NC=NC+1
        ICHAR3(NC)=IDECPT
        LOCPT=NC
      ENDIF
C
C               ********************************
C               **  STEP 12.2--               **
C               **  COMPUTE THE INTEGER PART  **
C               ********************************
C
C
      ISTEPN='12.2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SUMINT=0.0
      NUMINT=LOCPT-1
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,1201)NC,LOCPT,NUMINT
 1201   FORMAT('NC,LOCPT,NUMINT = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NUMINT.GT.0)THEN
        IPOWER=-1
        DO1200J=LOCPT-1,1,-1
          IBASCI=ICHAR(ICHAR3(J)(1:1)) - IZERO
          IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
            IPOWER=IPOWER+1
            SUMINT=SUMINT+REAL(IBASCI)*(10.0**IPOWER)
            GOTO1200
          ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2024)
 2024     FORMAT('***** INTERNAL ERROR IN DPREAL--A ',
     1           'NON-NUMERIC CHARACTER WAS ENCOUNTERED IN ',
     1           'CONVERTING THE INTEGER PART')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2025)(ICHAR3(L),L=1,41)
 2025     FORMAT('     OF THE FOLLOWING DATA VALUE--',41A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO8100
C
 1200   CONTINUE
      ENDIF
C
C               ***********************************
C               **  STEP 12.2--                  **
C               **  COMPUTE THE FRACTIONAL PART  **
C               ***********************************
C
      SUMDEC=0.0
      NUMDEC=NC-LOCPT
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        ISTEPN='12.2'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1601)NC,LOCPT,NUMDEC
 1601   FORMAT('NC,LOCPT,NUMDEC = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NUMDEC.NE.0)THEN
        IPOWER=0
        DO1500J=LOCPT+1,NC
          IBASCI=ICHAR(ICHAR3(J)(1:1)) - IZERO
          IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
            IPOWER=IPOWER+1
            SUMDEC=SUMDEC+REAL(IBASCI)/(10.0**IPOWER)
            GOTO1500
          ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2026)
 2026     FORMAT('***** INTERNAL ERROR IN DPREAL--A NON-NUMERIC ',
     1           'CHARACTER WAS ENCOUNTERED IN CONVERTING ',
     1           'THE DECIMAL PART')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2025)(ICHAR3(L),L=1,41)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO8100
 1500   CONTINUE
      ENDIF
C
C               ***************************************************
C               **  STEP 12.3--                                  **
C               **  IF EXPONENTIAL FORMAT, COMPUTE THE           **
C               **  EXPONENTIAL PART.                            **
C               ***************************************************
C
      ISTEPN='12.3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1851)NCE,NUMDEX,ICHEXP(1),ICHEXP(2),ICHEXP(3)
        CALL DPWRST('XXX','BUG ')
      ENDIF
 1851 FORMAT('NCE,NUMDEX,ICHEXP(1),ICHEXP(2),ICHEXP(3) = ',2I8,
     12X,A4,2X,A4,2X,A4)
      ISUMEX=0
      IF(NUMDEX.NE.0)THEN
        ISTART=2
        ISTOP=NUMDEX+1
        IPOWER=-1
        DO1860J=ISTART,ISTOP
          JREV=ISTOP-J+2
          IBASCI=ICHAR(ICHEXP(JREV)(1:1)) - IZERO
          IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
            IPOWER=IPOWER+1
            ISUMEX=ISUMEX+IBASCI*(INT(10.0**IPOWER + 0.01))
              GOTO1860
            ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2027)
 2027     FORMAT('***** INTERNAL ERROR IN DPREAL--A NON-NUMERIC',
     1           'CHARACTER WAS ENCOUNTERED IN CONVERTING ',
     1           'THE EXPONENTIAL PART')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2028)(ICHEXP(L),L=1,41)
 2028     FORMAT('      THE EXPONENT WAS AS FOLLOWS--',41A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO8100
 1860   CONTINUE
        IF(ICHEXP(1).EQ.'-')ISUMEX=-ISUMEX
      ENDIF
C
C               ****************************************************
C               **  STEP 12.4--                                   **
C               **  FINAL STEPS:                                  **
C               **  1) COMBINE THE INTEGER, DECIMAL, AND          **
C               **     EXPONENTIAL PARTS                          **
C               **  2) DETERMINE THE SIGN FOR THE ENTIRE NUMBER   **
C               **  3) PLACE THE COMPUTED NUMBER                  **
C               **     IN THE PROPER ELEMENT OF X(.)  **
C               ****************************************************
C
      SUM=AFACT*(SUMINT+SUMDEC)*(10.0**ISUMEX)
      N=N+1
      X(N)=SUM
C
      ISTEPN='12.4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,3003)SUMINT,SUMDEC,SUM,ISUMEX,NUMDEX
 3003   FORMAT('SUMINT,SUMDEC,SUM,ISUMEX,NUMDEX = ',3E15.7,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************
C               **  STEP 15--                              **
C               **  INCREMENT THE COLUMN AND DETERMINE IF  **
C               **  THE READ OF THE LINE IS FINISHED       **
C               *********************************************
C
      ISTEPN='15'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)I,ICOL22,N,NXC,X(N)
 3011   FORMAT('I,ICOL22,N,NXC,X(N) = ',4I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      I=I+1
      IF(I.LE.ICOL22 .OR. NCOLS.GT.0)GOTO149
      GOTO9000
C               **********************************
C               **  STEP 18--                   **
C               **  TREAT THE END OF FILE CASE  **
C               **********************************
C
 8000 CONTINUE
C
      ISTEPN='18'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IEND='YES'
      GOTO9000
C               **********************************
C               **  STEP 19                     **
C               **  ERROR READING FILE          **
C               **********************************
C
 8100 CONTINUE
C
      ISTEPN='19'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NUMLR2=NUMLRD+NUMCRD
      WRITE(ICOUT,8122)NUMLR2
 8122 FORMAT('      THIS CARD IMAGE WAS THE ',I8,' TH DATA CARD ',
     1       'IMAGE THAT WAS READ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8123)(IB(J),J=1,80)
 8123 FORMAT('      THE CARD IMAGE IS AS FOLLOWS--',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
C     IF LAST NON-BLANK CHARACTER IS A DELIMITER, THEN NEED TO ADD A
C     MISSING VALUE FIELD.  ONLY DO THIS IF DELIMITER IS NOT THE SPACE
C     CHARACTER (I.E., TRAILING SPACES ARE NOT INDICATIVE OF A MISSING
C     VALUE).  WE ARE TRYING TO CAPTURE CASE WHERE THE LINE ENDS WITH
C     A NON-BLANK DELIMITER, SUCH AS A COMMA, WHICH INDICATES THERE
C     SHOULD BE A "MISSING" FIELD.
C
      IF(IFLAGD.EQ.1 .AND. IB(NLASTZ).EQ.IREADL .AND.
     1   IREADL.NE.' ')THEN
        N=N+1
        X(N)=PREAMV
      ENDIF
      ICOL2=ICOL2S
C
      IDATDL=IDATD2
      ITIMDL=ITIMD2
      IREADL=IREAD3
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)I,ICOL1,ICOL2,ICOL22,N,NXC
 9012   FORMAT('I,ICOL1,ICOL2,ICOL22,N,NXC = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)MINCO2,MAXCO2,IWIDTH
 9013   FORMAT('MINCO2,MAXCO2,IWIDTH = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)X(1),X(2),X(3),X(4),X(5),X(6),X(N)
 9016   FORMAT('X(1),...,X(6),X(N) = ',7G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NXC.GE.1)THEN
          DO9119I=1,NXC
            WRITE(ICOUT,9117)I,IXC(I)
 9117       FORMAT('I,IXC(I) = ',I8,A24)
            CALL DPWRST('XXX','BUG ')
 9119     CONTINUE
        ENDIF
        WRITE(ICOUT,9018)IEND,IERROR,IOFILE,ICASRE,IMACCS
 9018   FORMAT('IEND,IERROR,IMACCS,ICASRE,IOFILE = ',4(A4,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)(IANSLC(I),I=1,MIN(100,IWIDTH))
 9022   FORMAT('(IANSLC(I),I=1,IWIDTH) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)IOUNIT,N2,MAXN2
 9024   FORMAT('IOUNIT,N2,MAXN2 = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)(IFUNC2(I),I=1,100)
 9032   FORMAT('(IFUNC2(I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPREBA(ADERBA,MAXREG,AREGBA,IREBIN,IREBPL,
CCCCC MARCH 1994.  ADD IREBPL ARGUMENT.
CCCCC SUBROUTINE DPREBA(ADERBA,MAXREG,AREGBA,IREBIN,
CCCCC SUBROUTINE DPREBA(IHARG,IARGT,ARG,NUMARG,ADERBA,MAXREG,AREGBA,
CCCCC OCTOBER 1993.  ABOVE LINE MODIFIED (DPCOHK.INC NOW INCLUDED
CCCCC IN THIS ROUTINE, SO NO NEED TO PASS).
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REGION BASES.
C              THESE ARE LOCATED IN THE VECTOR AREGBA(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --ADERBA
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--AREGBA (A FLOATING POINT VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C     UPDATED         --OCTOBER   1993.  ADD REGION BASE AUTOMATIC Y
C     UPDATED         --OCTOBER   1993.  ADD REGION BASE INTERPOLATE
C                                               <ON/OFF>
C     UPDATED         --MARCH     1994.  ADD REGION BASE POLYGON
C                                               <ON/OFF>
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC OCTOBER 1993.  COMMENT OUT FOLLOWING 2 LINES
CCCCC CHARACTER*4 IHARG
CCCCC CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
CCCCC ADD FOLLOWING LINE NOVEMBER 1994.
      CHARACTER*4 IBUGQ
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
CCCCC OCTOBER 1993.  ADD FOLLOWING SECTION.
      CHARACTER*4 IREBIN
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IWRITE
CCCCC MARCH 1994.  ADD FOLLOWING LINE.
      CHARACTER*4 IREBPL
C
CCCCC OCTOBER 1993.  COMMENT OUT FOLLOWING 3 LINES
CCCCC DIMENSION IHARG(*)
CCCCC DIMENSION IARGT(*)
CCCCC DIMENSION ARG(*)
      DIMENSION AREGBA(*)
C
CCCCC OCTOBER 1993.  ADD FOLLOWING COMMON BLOCKS
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRE'
      ISUBN2='BA  '
C
      NUMREG=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPREBA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXREG,NUMREG
   53 FORMAT('MAXREG,NUMREG = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,HOLD1,HOLD2
   54 FORMAT('IHOLD1,HOLD1,HOLD2 = ',A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)ADERBA
   55 FORMAT('ADERBA = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)AREGBA(1)
   70 FORMAT('AREGBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,AREGBA(I)
   76 FORMAT('I,AREGBA(I) = ',I8,2X,E15.7)
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
CCCCC OCTOBER 1993.  ADD REGION BASE AUTOMATIC <VAR>
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'AUTO')GOTO3000
      IF(NUMARG.GE.4.AND.IHARG(3).EQ.'AUTO')GOTO3000
CCCCC OCTOBER 1993.  ADD REGION BASE INTERPOLATE <ON/OFF>
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'INTE')GOTO4000
CCCCC MARCH 1994.  ADD REGION BASE POLYGON <ON/OFF>
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'POLY')GOTO5000
C
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
      IF(IHARG(2).EQ.'ALL')HOLD1=ADERBA
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
      NUMREG=1
      AREGBA(1)=ADERBA
      GOTO1270
C
 1220 CONTINUE
      NUMREG=NUMARG-1
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO1225I=1,NUMREG
      J=I+1
      IHOLD1=IHARG(J)
      HOLD1=ARG(J)
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=ADERBA
      IF(IHOLD1.EQ.'OFF')HOLD2=ADERBA
      IF(IHOLD1.EQ.'AUTO')HOLD2=ADERBA
      IF(IHOLD1.EQ.'DEFA')HOLD2=ADERBA
      AREGBA(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,AREGBA(I)
 1276 FORMAT('THE BASE OF REGION ',I6,
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
      NUMREG=MAXREG
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=ADERBA
      IF(IHOLD1.EQ.'OFF')HOLD2=ADERBA
      IF(IHOLD1.EQ.'AUTO')HOLD2=ADERBA
      IF(IHOLD1.EQ.'DEFA')HOLD2=ADERBA
      DO1315I=1,NUMREG
      AREGBA(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)AREGBA(I)
 1316 FORMAT('THE BASE OF ALL REGIONS',
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               ******************************************************
C               **  STEP 30--                                       **
C               **  TREAT THE REGION BASEAUTOMATIC <VARIABLE>   CASE**
C               ******************************************************
C
 3000 CONTINUE
C
C               ********************************************
C               **  STEP 31--                             **
C               **  CHECK THE VALIDITY OF ARGUMENT 2 (OR 3)**
C               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
C               ********************************************
C
      ISTEPN='31'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(3)
      IHLEF2=IHARG2(3)
      IF(IHARG(3).EQ.'DIST'.AND.IHARG2(3).EQ.'INCT')IHLEFT=IHARG(4)
      IF(IHARG(3).EQ.'DIST'.AND.IHARG2(3).EQ.'INCT')IHLEF2=IHARG2(4)
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
 3351 FORMAT('***** ERROR IN DPREBA--')
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
 3354 FORMAT('      (FOR WHICH REGION BASE DEFINITIONS ')
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
C               **  IF HAVE THE FORM--                 **
C               **  REGION BASE AUTOMATIC DISTINCT X   **
C               **  EXTRACT THE DISTINCT VALUES        **
C               **  FROM THE TARGET VARIABLE Y(.)   .  **
C               **  STORE THEM IN X(.)   .             **
C               **  IF HAVE THE FORM--                 **
C               **  CHARACTERS AUTOMATIC X             **
C               **  DO NOTHING                         **
C               *****************************************
C
      IF(IHARG(3).EQ.'DIST'.AND.IHARG2(3).EQ.'INCT')GOTO3420
C
      DO3411I=1,NY
        X(I)=Y(I)
 3411 CONTINUE
      NX=NY
      GOTO3490
C
 3420 CONTINUE
      IWRITE='OFF'
      CALL DISTIN(Y,NY,IWRITE,X,NX,IBUGP2,IERROR)
      GOTO3490
C
 3490 CONTINUE
C
C               ******************************************
C               **  STEP 36--                           **
C               **  COPY VALUES IN X(.) TO ABARBA       **
C               **        MAX NUMBER OF BARS    = 100   **
C               ******************************************
C
      IMAX=NX
      IF(IMAX.GT.MAXREG)IMAX=MAXREG
      DO3650I=1,IMAX
      AREGBA(I)=X(I)
 3650 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO3679
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO3675I=1,IMAX
      WRITE(ICOUT,3676)I,AREGBA(I)
 3676 FORMAT('REGION BASE ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 3675 CONTINUE
 3679 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               ******************************************
C               **  STEP 40--                           **
C               **  REGION BASE INTERPOLATE <ON/OFF>    **
C               ******************************************
 4000 CONTINUE
      IREBIN='ON'
      IF(NUMARG.EQ.2)THEN
        IREBIN='ON'
      ELSE IF(NUMARG.EQ.3)THEN
        IF(IHARG(3).EQ.'ON')IREBIN='ON'
        IF(IHARG(3).EQ.'YES')IREBIN='ON'
        IF(IHARG(3).EQ.'TRUE')IREBIN='ON'
        IF(IHARG(3).EQ.'DEFA')IREBIN='ON'
        IF(IHARG(3).EQ.'AUTO')IREBIN='ON'
        IF(IHARG(3).EQ.'OFF')IREBIN='OFF'
        IF(IHARG(3).EQ.'NO')IREBIN='OFF'
        IF(IHARG(3).EQ.'FALS')IREBIN='OFF'
      ENDIF
C
      IF(IFEEDB.EQ.'OFF')GOTO4099
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4010)IREBIN
 4010 FORMAT('REGION BASE INTERPOLATE HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 4099 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               ******************************************
C               **  STEP 50--                           **
C               **  REGION BASE POLYGON     <ON/OFF>    **
C               ******************************************
 5000 CONTINUE
      IREBPL='ON'
      IF(NUMARG.EQ.2)THEN
        IREBPL='ON'
      ELSE IF(NUMARG.EQ.3)THEN
        IF(IHARG(3).EQ.'ON')IREBPL='ON'
        IF(IHARG(3).EQ.'YES')IREBPL='ON'
        IF(IHARG(3).EQ.'TRUE')IREBPL='ON'
        IF(IHARG(3).EQ.'DEFA')IREBPL='ON'
        IF(IHARG(3).EQ.'AUTO')IREBPL='ON'
        IF(IHARG(3).EQ.'OFF')IREBPL='OFF'
        IF(IHARG(3).EQ.'NO')IREBPL='OFF'
        IF(IHARG(3).EQ.'FALS')IREBPL='OFF'
      ENDIF
C
      IF(IFEEDB.EQ.'OFF')GOTO5099
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5010)IREBPL
 5010 FORMAT('REGION BASE POLYGON HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 5099 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPREBA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXREG,NUMREG
 9013 FORMAT('MAXREG,NUMREG = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,HOLD1,HOLD2
 9014 FORMAT('IHOLD1,HOLD1,HOLD2 = ',A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)ADERBA
 9015 FORMAT('ADERBA = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)AREGBA(1)
 9030 FORMAT('AREGBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,AREGBA(I)
 9036 FORMAT('I,AREGBA(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRECF(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,
     1                  IANGLU,
     1                  IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
C
C     PURPOSE--TREAT THE LET CASE FOR A RECURSIVE FUNCTION.
C     EXAMPLE--LET X(1) = 1
C              LET Y = RECURSIVE FUNCTION 2*X  FOR X = 2 100
C
C              THIS IS EQUIVALENT TO
C
C              LET X(1) = 1
C              LOOP FOR K = 2 1 100
C                  LET KM1 = K-1
C                  LET ATEMP = X(KM1)
C                  LET AVAL = 2*ATEMP
C              END OF LOOP
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
C     VERSION NUMBER--2011/3
C     ORIGINAL VERSION--MARCH     2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IA
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 IWD1
      CHARACTER*4 IWD12
      CHARACTER*4 IWD2
      CHARACTER*4 IWD22
      CHARACTER*4 ILAB
      CHARACTER*4 IKEY
      CHARACTER*4 IKEY2
      CHARACTER*4 INCLUN
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASUP
      CHARACTER*4 IERRO2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IOLD
      CHARACTER*4 IOLD2
      CHARACTER*4 INEW
      CHARACTER*4 INEW2
      CHARACTER*4 IHPARN
      CHARACTER*4 IHPAR2
      CHARACTER*4 IHL
      CHARACTER*4 IHL2
      CHARACTER*4 IDUMV
      CHARACTER*4 IDUMV2
      CHARACTER*4 IHOUT
      CHARACTER*4 IHOUT2
      CHARACTER*4 IUOUT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IFOUND
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
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
      DIMENSION ROOTS2(100)
C
      DIMENSION ILAB(10)
      DIMENSION IOLD(10)
      DIMENSION IOLD2(10)
      DIMENSION INEW(10)
      DIMENSION INEW2(10)
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
      ISUBN1='DPRE'
      ISUBN2='CF  '
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
      ILOCMX=0
      NUMLIM=0
      ILOC3=0
      NCHANF=0
C
C               ********************************************
C               **  TREAT THE RECURSIVE FUNCTION SUBCASE  **
C               **  OF THE LET COMMAND                    **
C               ********************************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RECF')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRECF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGCO,IBUGEV,ISUBRO,IA(1)
   52   FORMAT('IBUGA3,IBUGCO,IBUGEV,ISUBRO,IA(1) = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
C
      MAXN2=MAXCHF
      MAXN3=MAXCHF
C
C               *********************************************************
C               **  STEP 2--                                            *
C               **  EXAMINE THE LEFT-HAND SIDE--                        *
C               **  IS THE VARIABLE NAME TO LEFT OF = SIGN              *
C               **  ALREADY IN THE NAME LIST?                           *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE    *
C               **  OF THE NAME ON THE LEFT.                            *
C               *********************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO2000I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          ILISTL=I2
          GOTO2900
        ENDIF
 2000 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN RECURSIVE FUNCTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND FUNCTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2203)MAXNAM
 2203   FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
 2900 CONTINUE
C               ***************************************************************
C               **  STEP 3.1--                                               **
C               **  EXTRACT THE RIGHT-SIDE FUNCTIONAL
C               **  EXPRESSION FROM THE INPUT COMMAND LINE                   **
C               **  (STARTING WITH THE FIRST NON-BLANK LOCATION AFTER THE    **
C               **  EQUAL SIGN AND ENDING WITH THE END OF THE LINE           **
C               **  OR WITH THE LAST NON-BLANK CHARACTER BEFORE     WRT  .   **
C               **  PLACE THE FUNCTION IN IFUNC2(.)  .                       **
C               ***************************************************************
C
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWD1=IHARG(3)
      IWD12=IHARG2(3)
      IWD2='WRT '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1            IFUNC2,N2,IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'YES')GOTO3500
C
      IWD1=IHARG(3)
      IWD12=IHARG2(3)
      IWD2='FOR '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1            IFUNC2,N2,IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'YES')GOTO3500
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3102)
 3102 FORMAT('      INVALID COMMAND FORM FOR RECURSIVE FUNCTION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3103)
 3103 FORMAT('      GENERAL FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3104)
 3104 FORMAT('      LET ... = RECURSIVE FUNCTION  ... WRT  ... ',
     1'FOR ...  =  ...   ...')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3105)
 3105 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,3106)(IANS(I),I=1,MIN(100,IWIDTH))
 3106   FORMAT('      ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
 3500 CONTINUE
C
C               *****************************************************
C               **  STEP 3.2--                                     **
C               **  DETERMINE IF THE RIGHT-HAND SIDE IS            **
C               **  IN FUNCTION FORM OR IS IN EQUATION FORM.       **
C               **  IF IN EQUATION FORM, CONVERT TO FUNCTION FORM  **
C               **  BY REPLACING THE EQUAL SIGN BY A MINUS SIGN    **
C               **  AND ENCLOSING THE REST OF THE EXPRESSION IN    **
C               **  PARENTHESES.                                   **
C               **  PLACE THE OUTPUT FUNCTION BACK IN IFUNC2(.)    **
C               *****************************************************
C
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO3600I=1,N2
        I2=I
        IF(IFUNC2(I).EQ.'=')GOTO3610
 3600 CONTINUE
      GOTO3900
C
 3610 CONTINUE
      ILOCE2=I2
C
      IMIN=ILOCE2+1
      IF(IMIN.GT.N2)GOTO3690
      DO3650I=IMIN,N2
        IREV=N2-I+IMIN
        IREVP1=IREV+1
        IFUNC2(IREVP1)=IFUNC2(IREV)
 3650 CONTINUE
      I=ILOCE2
      IFUNC2(I)='-'
      I=ILOCE2+1
      IFUNC2(I)='('
      I=N2+2
      IFUNC2(I)=')'
      N2=I
 3690 CONTINUE
C
 3900 CONTINUE
C
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1            NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,
     1            N3,MAXN3,
     1            IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RECF')THEN
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
 5081   FORMAT('RECURSIVE VARIABLE         = ',A4,A4)
        CALL DPWRST('XXX','BUG ')
C
      ENDIF
C
C               *************************************
C               **  STEP 5--                       **
C               **  EXTRACT QUALIFIER INFORMATION. **
C               *************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               **************************************************
C               **  STEP 5.1--                                  **
C               **  DETERMINE THE DUMMY VARIABLE FOR THE ROOT.  **
C               **************************************************
C
      ISTEPN='5.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IKEY='WRT '
      IKEY2='    '
      ISHIFT=1
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1            IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1            IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1            IVOUT,VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO5119
        IDUMV(1)=IHOUT
        IDUMV2(1)=IHOUT2
        NUMDV=1
        GOTO5190
 5119 CONTINUE
C
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=1
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1            IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1            IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1            IVOUT,VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO5129
        IDUMV(1)=IHOUT
        IDUMV2(1)=IHOUT2
        NUMDV=1
        GOTO5190
 5129 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5182)
 5182 FORMAT('      INVALID COMMAND FORM FOR RECURSIVE FUNCTION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5183)
 5183 FORMAT('      NO VARIABLE FOR RECURSION DEFINED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3103)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3104)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3105)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,3106)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
 5190 CONTINUE
C
C               **************************************************
C               **  STEP 5.2--                                  **
C               **  DETERMINE THE LIMITS FOR   THE RECURSION.   **
C               **************************************************
C
      ISTEPN='5.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMLIM=0
C
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=3
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1            IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,
     1            VALUE,IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1            IVOUT,VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO5219
        XMIN=VOUT
        NUMLIM=NUMLIM+1
 5219 CONTINUE
C
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=4
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1            IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,
     1            VALUE,IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1            IVOUT,VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO5239
      IF(IHOUT.EQ.'TO  '.AND.IHOUT2.EQ.'    ')GOTO5229
        XMAX=VOUT
        ILOCMX=ILOC2
        NUMLIM=NUMLIM+1
 5229 CONTINUE
C
      IF(NUMLIM.EQ.2)GOTO5239
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=5
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1            IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1            IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1            IVOUT,VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO5239
        XMAX=VOUT
        ILOCMX=ILOC2
        NUMLIM=NUMLIM+1
 5239 CONTINUE
C
      IF(NUMLIM.NE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5282)
 5282   FORMAT('      INVALID COMMAND FORM FOR ROOT-FINDING.')
        CALL DPWRST('XXX','BUG ')
        IF(NUMLIM.LE.0)THEN
          WRITE(ICOUT,5283)
 5283     FORMAT('      NO LIMITS FOR RECURSIVE FUNCTION DEFINED.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(NUMLIM.EQ.1)THEN
          WRITE(ICOUT,5284)
 5284     FORMAT('      ONLY ONE LIMIT FOR ROOT-FINDING DEFINED.')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(NUMLIM.GT.2)THEN
          WRITE(ICOUT,5285)NUMLIM
 5285     FORMAT('      NUMBER OF LIMITS DEFINED = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,3103)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3104)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3105)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3106)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               **********************************************
C               **  STEP 6.3--                              **
C               **  SCAN THE QUALIFIERS FOR VARIABLE,       **
C               **  PARAMETER, FUNCTION, AND VALUE CHANGES  **
C               **  IN THE FUNCTION.                        **
C               **********************************************
C
      ISTEPN='6.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NCHANG=0
      DO6300IFORI=1,10
C
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=1
        ILOCA=ILOC3
        IF(IFORI.EQ.1)ILOCA=ILOCMX
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1              IHARG,IHARG2,NUMARG,
     1              INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1              IUSE,IN,NUMNAM,
     1              IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1              IVOUT,VOUT,IUOUT,
     1              INOUT,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6302)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3103)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3104)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3105)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,3106)(IANS(I),I=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
        IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO6350
C
        ILOC3=ILOC2+2
        IF(ILOC3.GT.NUMARG)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6302)
 6302     FORMAT('      INVALID COMMAND FORM FOR ROOT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3103)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3104)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3105)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,3106)(IANS(I),I=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
        NCHANG=NCHANG+1
        IOLD(NCHANG)=IHARG(ILOC2)
        IOLD2(NCHANG)=IHARG2(ILOC2)
        INEW(NCHANG)=IHARG(ILOC3)
        INEW2(NCHANG)=IHARG2(ILOC3)
C
 6300 CONTINUE
 6350 CONTINUE
      GOTO6390
C
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON' .AND. NCHANF.GE.1)THEN
C
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
C
        CALL COMPIC(IFUNC3,N3,IOLD,IOLD2,INEW,INEW2,NCHANG,IFUNC3,N3,
     1              IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        ILAB(1)='POST'
        ILAB(2)='-CHA'
        ILAB(3)='NGE '
        ILAB(4)='FUNC'
        ILAB(5)='TION'
        ILAB(6)='  = '
        NUMWDL=6
        CALL DPPRIF(ILAB,NUMWDL,IFUNC3,N3,IBUGA3)
C
      ENDIF
C
C               **********************************************************
C               **  STEP 6.7--                                          **
C               **  MAKE A NON-CALCULATING PASS AT THE FUNCTION         **
C               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.  **
C               **********************************************************
C
      ISTEPN='6.8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=1
      CALL COMPIM(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1            IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1            IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ***********************************************
C               **  STEP 7--                                 **
C               **  CHECK THAT ALL PARAMETERS                **
C               **  IN THE FUNCTION ARE ALREADY PRESENT      **
C               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
C               **  ALSO CHECK THAT THE VARIABLE NAME        **
C               **  THAT FOLLOWS FOR (THAT IS, THE DUMMY     **
C               **  VARIABLE IS IN THE FUNCTION.             **
C               **  NOTE--ALL PARAMETERS AND VARIABLES       **
C               **  THAT ARE NOT FOUND IN IHNAME(.)          **
C               **  WILL BE AUTOMATICALLY SET TO 0.0         **
C               **  (BUT ONLY TEMPORARILY);                  **
C               **  THIS CONVENTION ALLOWS AN AUTOMATIC      **
C               **  SOLUTION TO THE PROBLEM OF SOLVING       **
C               **  FOR ROOTS OF EQUATIONS                   **
C               **  (AS OPPOSED TO FUNCTIONS)                **
C               **  SINCE 'Y' WILL TYPICALLY BE SET TO ZERO  **
C               **  AS ONE WOULD WANT FOR SOLVING            **
C               **  FOR A ROOT (= A FUNCTION ZERO).          **
C               ***********************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IP=0
      IV=0
      IF(NUMPV.LE.0)GOTO7650
      DO7600J=1,NUMPV
        IHPARN=IPARN(J)
        IHPAR2=IPARN2(J)
        IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1))THEN
          IV=IV+1
          LOCDUM=J
          GOTO7600
        ENDIF
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHPARN,IHPAR2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
        IF(IERRO2.EQ.'YES')THEN
          IP=IP+1
          PARAM(J)=0.0
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7606)IHPARN,IHPAR2
 7606     FORMAT('NOTE--',A4,A4,' HAS BEEN TEMPORARILY SET TO ZERO')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7607)
 7607     FORMAT('             FOR THE RECURSION PROCESS.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          IP=IP+1
          PARAM(J)=VALUE(ILOCP)
        ENDIF
 7600 CONTINUE
 7650 CONTINUE
C
C               ******************************
C               **  STEP 8--                **
C               **  DETERMINE THE ROOTS  .  **
C               ******************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RECF')THEN
        ISTEPN='8'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7711)
 7711   FORMAT('***** FROM DPRECF, IMMEDIATELY BEFORE CALLING ',
     1         'ROOTS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7712)N3,NUMPV
 7712   FORMAT('N3,NUMPV = ',I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7713)NUMDV,XMIN,XMAX
 7713   FORMAT('NUMDV,XMIN,XMAX = ',I8,2E15.7)
        CALL DPWRST('XXX','BUG ')
        DO7714I=1,NUMDV
          WRITE(ICOUT,7715)I,IDUMV(I),IDUMV2(I)
 7715     FORMAT('I,IDUMV(I),IDUMV2(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
 7714   CONTINUE
      ENDIF
C
CCCCC CALL DPREC2(IFUNC3,N3,PARAM,IPARN,IPARN2,NUMPV,
CCCCC1            IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
CCCCC1            IDUMV,IDUMV2,NUMDV,XMIN,XMAX,ROOTS2,NROOTS,
CCCCC1            IBUGA3,IBUGCO,IBUGEV,IERROR)
C
C               *****************************************
C               **  STEP 9--                           **
C               **  ENTER THE ROOTS INTO THE DATAPLOT  **
C               **  ARRAY V(.).                        **
C               **  ENTER THE FOUND NUMBER OF ROOTS    **
C               **  INTO THE DATAPLOT PARAMETER        **
C               **  NROOTS   .                         **
C               *****************************************
C
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHL=IHLEFT
      IHL2=IHLEF2
      ICASUP='V'
      CALL DPINVP(IHL,IHL2,ICASUP,ROOTS2,NROOTS,AROOTS,NROOTS,
     1ISUBN1,ISUBN2,IBUGA3,IERROR)
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RECF')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPRECF--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',
     1           I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9017)NUMCHF,MAXCHF,IWIDTH,N2,N3,NUMPV
 9017   FORMAT('NUMCHF,MAXCHF,IWIDTH,N2,N3,NUMPV = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(IFUNC(I),I=1,MIN(IWIDTH,115))
 9018   FORMAT('IFUNC(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)(IFUNC2(I),I=1,MIN(N2,115))
 9019   FORMAT('IFUNC2(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)(IFUNC3(I),I=1,MIN(120,N3))
 9021   FORMAT('IFUNC3(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)IHLEFT,IHLEF2,IDUMV,IDUMV2
 9023   FORMAT('IHLEFT,IHLEF2,IDUMV,IDUMV2 = ',A4,A4,2X,A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)ICASUP,IFOUND,IERROR
 9024   FORMAT('ICASUP,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRECH(IHARG,NUMARG,
     1IBASLC,
     1IREPCH,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REPLACEMENT CHARACTOR WHICH MAY
C              BE USED TO REPLACE A PARAMETER NAME
C              OR A STRING NAME BY ITS CONTENTS.
C              WHEN A COMMAND LINE IS READ,
C              IT IS SEARCHED FOR THE REPLACEMENT CHARACTER;
C              IF IT IS FOUND, THE PARAMETER OR STRING
C              NAME IMMEDIATELY FOLLOWING THE REPLACEMENT CHARACTER
C              IS REPLACEWD/SUBSTITUTED IN LITERALLY
C              AND IMMEDIATELY.
C              THE REPLACEMENT CHARACTER CAPABILITY ALLOWS THE ANALYST
C              TO FILL IN CURRENT VALUES OF PARAMETERS
C              AS LABELS AND LEGENDS ON PLOTS,
C              IT ALSO ALLOWS FILE NAMES TO BE SYMBOLICALLY
C              BUILT INSIDE A LOOP, ETC.
C              THE SPECIFIED REPLACEMENT CHARACTOR WILL BE PLACED
C              IN THE CHARACTER VARIABLE IREPCH.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IBASLC (A CHARACTER VARIABLE--BACKSLASH)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--IREPCH (A CHARACTER VARIABLE)
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
C     VERSION NUMBER--89/7
C     ORIGINAL VERSION--JUNE     1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*1 IBASLC
      CHARACTER*1 IREPCH
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHARG4
      CHARACTER*1 IHOLD
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
   51 FORMAT('***** AT THE BEGINNING OF DPRECH--')
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
      IHOLD=IBASLC
      GOTO1180
C
 1160 CONTINUE
      IHARG4=IHARG(NUMARG)
      IHOLD=IHARG4(1:1)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IREPCH=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IREPCH
 1181 FORMAT('THE REPLACEMENT CHARACTOR HAS JUST BEEN SET TO ',
     1A1)
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
      WRITE(ICOUT,9013)IHARG4,IHOLD
 9013 FORMAT('IHARG4,IHOLD = ',A4,2X,A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IREPCH
 9014 FORMAT('IREPCH = ',A1)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRECI(ISEED,IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ,
     1                  ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT MARK VANGEL'S RECIPE FIT
C              FOR LINEAR MODELS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/2
C     ORIGINAL VERSION--AUGUST   1997.
C     UPDATED         --JULY     2019. TWEAK SCRATCH SPACE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASRE
      CHARACTER*4 ICASDG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASEQ
      CHARACTER*4 IKEY
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
      CHARACTER*4 IFLAG
C
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 IWRITE
C
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*20 IMODEL
C
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMEAN
C
      LOGICAL SATT
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
C
      DIMENSION ILIS(100)
      DIMENSION ICOLR(100)
C
      DOUBLE PRECISION XDESGN(MAXOBV)
      DOUBLE PRECISION XPTS(MAXOBV)
      DOUBLE PRECISION V2(MAXOBV)
      DOUBLE PRECISION TLM0(MAXOBV)
      DOUBLE PRECISION TLM1(MAXOBV)
      DOUBLE PRECISION ETA0(MAXOBV)
      DOUBLE PRECISION ETA1(MAXOBV)
      DOUBLE PRECISION XM(MAXOBV)
      DOUBLE PRECISION WK2(MAXOBV)
      DOUBLE PRECISION WK3(MAXOBV)
      DOUBLE PRECISION XN(MAXOBV)
      DOUBLE PRECISION T(MAXOBV)
      DOUBLE PRECISION CRT(MAXOBV)
      DOUBLE PRECISION Y2(MAXOBV)
C
      DIMENSION IP(MAXOBV)
      DIMENSION IQ(MAXOBV)
C
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
C
      DOUBLE PRECISION XMAT(5*MAXOBV)
      DOUBLE PRECISION SCRTCH(10*MAXOBV)
C
      DOUBLE PRECISION XTX(100)
      DOUBLE PRECISION XTXI(100)
      DOUBLE PRECISION S1(100)
      DOUBLE PRECISION S2(100)
      DOUBLE PRECISION V1(100)
      DOUBLE PRECISION COEF(100)
C
      DIMENSION IVARN1(100)
      DIMENSION IVARN2(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      EQUIVALENCE (Y2(1),X3D(1))
      EQUIVALENCE (PRED2(1),X(1))
      EQUIVALENCE (RES2(1),D(1))
      EQUIVALENCE (CRT(1),DSIZE(1))
      EQUIVALENCE (XN(1),DSYMB(1))
      EQUIVALENCE (T(1),DFILL(1))
      EQUIVALENCE (XTX(1),DCOLOR(1))
      EQUIVALENCE (XTXI(1),DCOLOR(1001))
      EQUIVALENCE (S1(1),DCOLOR(2001))
      EQUIVALENCE (S2(1),DCOLOR(3001))
      EQUIVALENCE (V1(1),DCOLOR(4001))
C
      EQUIVALENCE (IGARBG(IIGAR1),IQ(1))
      EQUIVALENCE (IGARBG(IIGAR2),IP(1))
C
      EQUIVALENCE (GARBAG(IGARB1),XMAT(1))
      EQUIVALENCE (GARBAG(IGAR11),SCRTCH(1))
C
      EQUIVALENCE (DGARBG(IDGAR1),XDESGN(1))
      EQUIVALENCE (DGARBG(IDGAR2),XPTS(1))
      EQUIVALENCE (DGARBG(IDGAR3),V2(1))
      EQUIVALENCE (DGARBG(IDGAR4),TLM0(1))
      EQUIVALENCE (DGARBG(IDGAR5),TLM1(1))
      EQUIVALENCE (DGARBG(IDGAR6),ETA0(1))
      EQUIVALENCE (DGARBG(IDGAR7),ETA1(1))
      EQUIVALENCE (DGARBG(IDGAR8),XM(1))
      EQUIVALENCE (DGARBG(IDGAR9),WK2(1))
      EQUIVALENCE (DGARBG(IDGA10),WK3(1))
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRE'
      ISUBN2='CI  '
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      MAXPAR=20
      MAXV2=MAXPAR
      MINN2=2
      MAXN2=MAXCHF
      MAXN3=MAXCHF
      MAXN4=MAXCHF
      MAXLVL=INT(SQRT(REAL(IGARB0)))
CCCCC MAXPT1=20*MAXOBV
CCCCC MAXPT2=10*MAXOBV
      MAXPT1=10*MAXOBV
      MAXPT2=5*MAXOBV
      NPAR=0
      NTOT=0
      NBCH=0
      NLEFT=0
      NLOOP=0
      ILOCXP=0
      ILOCB=0
      NUMVAR=0
C
      CPUEPS=R1MACH(3)
C
C               *****************************
C               **  TREAT THE RECIPE CASE  **
C               *****************************
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRECI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ
   54   FORMAT('IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)NUMNAM
   56   FORMAT('NUMNAM = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO57I=1,NUMNAM
          WRITE(ICOUT,58)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I),
     1                   VALUE(I)
   58     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I)',
     1           'VALUE(I) = ',I8,2X,2A4,2X,A4,2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
        WRITE(ICOUT,61)IRECSA,RECIDG,RECIPC,RECICO
   61   FORMAT('IRECSA,RECIDG,RECIPC,RECICO=',A4,1X,3(E15.7))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  EXTRACT THE COMMAND         **
C               **    RECIPE FIT                **
C               **    RECIPE ANOVA              **
C               **    RECIPE Y <UNIVARIATE CASE **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'RECI'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'FIT')THEN
        IFOUND='YES'
        ICASRE='FREC'
        IFITFC=INT(RECIFF+0.5)
        IF(IFITFC.LT.0)IFITFC=0
        IF(IFITFC.GT.1)THEN
          ICASDG='1'
        ELSE
          IJUNK=INT(RECIDG+0.5)
          ICASDG='1'
          IF(IJUNK.EQ.0)ICASDG='0'
          IF(IJUNK.EQ.1)ICASDG='1'
          IF(IJUNK.EQ.2)ICASDG='2'
          IF(IJUNK.EQ.3)ICASDG='3'
          IF(IJUNK.EQ.4)ICASDG='4'
          IF(IJUNK.EQ.5)ICASDG='5'
          IF(IJUNK.EQ.6)ICASDG='6'
          IF(IJUNK.EQ.7)ICASDG='7'
          IF(IJUNK.EQ.8)ICASDG='8'
          IF(IJUNK.EQ.9)ICASDG='9'
          IF(IJUNK.EQ.10)ICASDG='10'
        ENDIF
      ELSEIF(ICOM.EQ.'RECI'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'ANOV')THEN
        IFOUND='YES'
        ICASRE='AREC'
      ELSEIF(ICOM.EQ.'RECI'.AND.NUMARG.GE.1)THEN
        IFOUND='YES'
        ICASRE='UREC'
      ENDIF
C
      IF(IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,66)ICASRE
   66   FORMAT('ICASRE=',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF 
C
      IF(ICASRE.EQ.'    ')GOTO9000
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  IN PARTICULAR, CHECK THAT THE NUMBER OF ARGUMENTS*
C               **  IS AT LEAST 1,                                  **
C               *******************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LT.1)THEN
        WRITE(ICOUT,2001)
 2001   FORMAT('***** ERROR IN RECIPE (DPRECI)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2002)
 2002   FORMAT('      THE NUMBER OF ARGUMENTS DETECTED IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2003)NUMARG
 2003   FORMAT('      RECIPE COMMAND = 0.  NUMARG = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,318)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2008)(IANS(J),J=1,MIN(IWIDTH,100))
 2008     FORMAT('      COMMAND LINE--',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO2100J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GOTO2110
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GOTO2110
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GOTO2110
 2100 CONTINUE
      ILOCQ=NUMARG+1
      GOTO2120
 2110 CONTINUE
      ILOCQ=J1
      GOTO2120
 2120 CONTINUE
C
C               *******************************************************
C               **  STEP 4--                                         **
C               **  FOR RECIPE FIT AND RECIPE ANOVA,                 **
C               **  THE SECOND WORD AFTER  RECIPE SHOULD BE THE      **
C               **  RESPONSE VARIABLE (= THE DEPENDENT VARIABLE).    **
C               **  FOR RECIPE <Y>, RESPONSE VARIABLE IS FIRST WORD. **
C               **  EXTRACT THE RESPONSE VARIABLE AND DETERMINE      **
C               **  IF IT IS ALREADY IN THE NAME LIST AND IS, IN FACT,*
C               **  A VARIABLE (AS OPPOSED TO A PARAMETER).          **
C               *******************************************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCY=2
      IF(ICASRE.EQ.'UREC')ILOCY=1
      IHLEFT=IHARG(ILOCY)
      IHLEF2=IHARG2(ILOCY)
      DO2350I=1,NUMNAM
      I2=I
      IF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2).AND.
     1IUSE(I2).EQ.'V')GOTO2379
 2350 CONTINUE
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2362)
 2362 FORMAT('      THE NAME FOLLOWING THE WORD RECIPE FIT ',
     1'(OR RECIPE ANOVA),')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2363)
 2363 FORMAT('      WHICH SHOULD BE THE RESPONSE VARIABLE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2364)
 2364 FORMAT('      EITHER DOES NOT EXIST OR IS NOT A VARIABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2367)
 2367 FORMAT('      IN THE CURRENT NAME LIST.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2369)IHLEFT,IHLEF2
 2369 FORMAT('      NAME AFTER THE WORD RECIPE FIT/ANOVA = ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,318)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,2008)(IANS(J),J=1,MIN(IWIDTH,100))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
 2379 CONTINUE
      ILOCV=I2
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
C
C               *******************************************************
C               **  STEP 5--                                         **
C               **  FOR ALL VARIATIONS OF THE RECIPE COMMAND,        **
C               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)
C               **  FOR THE RESPONSE VARIABLE IS 2 OR LARGER.        **
C               *******************************************************
C
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NLEFT.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)IHLEFT,IHLEF2
  312   FORMAT('      THE NUMBER OF OBSERVATIONS IN VARIABLE ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,315)MINN2
  315   FORMAT('      MUST BE ',I8,' OR LARGER;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,316)
  316   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,317)NLEFT
  317   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,318)
  318   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,2008)(IANS(I),I=1,IWIDTH)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  EXTRACT THE INDEPENDENT VARIABLES           **
C               **  FOR RECIPE FIT:                             **
C               **      Y X <BATCH> <XPRED>                     **
C               **  FOR RECIPE ANOVA:                           **
C               **      Y X1 ... XK <BATCH>                     **
C               **  FOR RECIPE :                                **
C               **      Y <BATCH>                               **
C               **  IF THE   TO   FEATURE IS USED IN THE        **
C               **  ARGUMENT LIST, TRANSLATE THE   TO   TO      **
C               **  EXPLICIT VARIABLE NAMES             INTO    **
C               **************************************************
C
      ISTEPN='12'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        MAXREC=3
        JMIN=ILOCY+1
        JMAX=ILOCQ-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXREC,
     1  IHNAME,IHNAM2,IUSE,NUMNAM,
     1  IVARN1,IVARN2,NUMVAR,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        IF(NUMVAR.EQ.1)THEN
          ILOCX=ILOCY+1
          ILOCB=-1
          ILOCXP=-1
        ELSEIF(NUMVAR.EQ.2)THEN
          ILOCX=ILOCY+1
          ILOCB=ILOCX+1
          ILOCXP=-1
        ELSEIF(NUMVAR.EQ.3)THEN
          ILOCX=ILOCY+1
          ILOCB=ILOCX+1
          ILOCXP=ILOCB+1
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,412)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,413)NUMVAR
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
  412   FORMAT('      BETWEEN 1 AND 4 VARIABLE NAMES CAN BE SPECIFIED ',
     1         'FOR THIS COMMAND')
  413   FORMAT('      ',I8,' VARIABLES WERE GIVEN.')
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        MAXREC=2*IFITFC+1
        JMIN=ILOCY+1
        JMAX=ILOCQ-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXREC,
     1  IHNAME,IHNAM2,IUSE,NUMNAM,
     1  IVARN1,IVARN2,NUMVAR,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        IF(NUMVAR.LT.IFITFC)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1422)NUMVAR,IFITFC
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ELSEIF(NUMVAR.EQ.IFITFC)THEN
          ILOCX=ILOCY+1
          ILOCB=-1
          ILOCXP=-1
        ELSEIF(NUMVAR.EQ.IFITFC+1)THEN
          ILOCX=ILOCY+1
          ILOCB=ILOCX+IFITFC
          ILOCXP=-1
        ELSEIF(NUMVAR.EQ.2*IFITFC)THEN
          ILOCX=ILOCY+1
          ILOCB=-1
          ILOCXP=ILOCX+1
        ELSEIF(NUMVAR.EQ.2*IFITFC+1)THEN
          ILOCX=ILOCY+1
          ILOCB=ILOCX+IFITFC
          ILOCXP=ILOCB+1
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1412)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 1422   FORMAT('      THE NUMBER OF VARIABLES ENTERED ',I5,' IS LESS ',
     1         'THAN THE NUMBER OF FIT FACTORS ',I5)
 1412   FORMAT('      AN IMPROPER NUMBER OF VARIABLE NAMES HAS BEEN ',
     1         'SPECIFIED FOR THIS COMMAND.')
      ELSEIF(ICASRE.EQ.'UREC')THEN
        MAXREC=1
        JMIN=ILOCY+1
        JMAX=ILOCQ-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXREC,
     1  IHNAME,IHNAM2,IUSE,NUMNAM,
     1  IVARN1,IVARN2,NUMVAR,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        ILOCX=-1
        ILOCXP=-1
        IF(NUMVAR.EQ.1)THEN
          ILOCB=ILOCX+1
        ELSEIF(NUMVAR.EQ.0)THEN
          ILOCB=-1
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,422)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,423)NUMVAR
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
  422   FORMAT('      BETWEEN 0 AND 1 VARIABLE NAMES CAN BE SPECIFIED ',
     1         'FOR THIS COMMAND')
  423   FORMAT('      ',I8,' VARIABLES WERE GIVEN.')
      ELSEIF(ICASRE.EQ.'AREC')THEN
        NUMFAC=INT(RECIFA + 0.1)
CCCCC   IF(NUMFAC.GT.MAXPAR)THEN
CCCCC     WRITE(ICOUT,999)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,511)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,512)NUMFAC,MAXPAR
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     IERROR='YES'
CCCCC     GOTO9000
CCCCC   ENDIF
CC511   FORMAT('***** ERROR IN DPRECI (RECIPE ANOVA)--')
CC512   FORMAT('      THE REQUESTED NUMBER OF FACTORS ',I8,
CCCCC1        ' IS GREATER THAN THE ALLOWED MAXIMUM OF ',I8)
        MAXREC=NUMFAC+1
        JMIN=ILOCY+1
        JMAX=ILOCQ-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXREC,
     1  IHNAME,IHNAM2,IUSE,NUMNAM,
     1  IVARN1,IVARN2,NUMVAR,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        IF(NUMVAR.EQ.NUMFAC)THEN
          ILOCX=ILOCY+1
          ILOCB=-1
          ILOCXP=-1
        ELSEIF(NUMVAR.EQ.NUMFAC+1)THEN
          ILOCX=ILOCY+1
          ILOCB=ILOCX+NUMFAC
          ILOCXP=-1
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,612)NUMFAC,NUMVAR
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
  612   FORMAT('      ',I8,' FACTORS WERE SPECIFIED, BUT ONLY ',I8,
     1         ' VARIABLES WERE GIVEN ON THE COMMAND LINE.')
      ENDIF
C
      IF(IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,71)NUMVAR,NUMFAC
   71   FORMAT('NUMVAR,NUMFAC=',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************************
C               **  STEP 13--                        **
C               **  CHECK THE VALIDITY OF EACH       **
C               **  OF THE VARIABLES.                **
C               **  THE DESIGN MATRIX (X) AND BATCH  **
C               **  IDENTIFIER VARIABLE MUST HAVE THE**
C               **  SAME NUMER OF OBSERVATIONS AS THE**
C               **  Y VARIABLE.  THE XPRED VARIABLE  **
C               **  MUST HAVE AT LEAST 2 OBSERVATIONS**
C               ***************************************
C
      ISTEPN='13'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASRE.EQ.'UREC'.AND.NUMVAR.EQ.0)GOTO1399
      NPRED=-1
      IFITVA=IFITFC
      IF(ILOCB.GT.0)IFITVA=IFITVA+1
      DO1300I=1,NUMVAR
C
      IHRIGH=IVARN1(I)
      IHRIG2=IVARN2(I)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHRIGH,IHRIG2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      NRIGHT=IN(ILOCV)
      ILIS(I)=ILOCV
      ICOLR(I)=IVALUE(ILOCV)
C
      IF(ICASRE.EQ.'AREC'.OR.(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1).OR.
     1   ILOCXP.LT.0)THEN
        IF(ILOCXP.GT.0 .AND. I.EQ.NUMVAR)NPRED=NRIGHT
        IF(NRIGHT.EQ.NLEFT)GOTO1390
        IF(ILOCXP.GT.0 .AND. I.EQ.NUMVAR .AND. NRIGHT.GT.2)GOTO1390
        GOTO1309
      ENDIF
C
      IF(I.GT.IFITVA)THEN
        IF(NPRED.LT.0)THEN
          NPRED=NRIGHT
          GOTO1390
        ELSE
          NPREDN=NRIGHT
          IF(NPREDN.NE.NPRED.OR.NPRED.LT.2)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11313)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11315)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
11313       FORMAT('      THE VARIABLES FOR THE PREDICTED VARIABLES ',
     1             'DO NOT ALL CONTAIN THE SAME')
11315       FORMAT('      NUMBER OF ELEMENTS FOR THE MULTI-LINEAR FIT ',
     1             'CASE.')
          ELSE
            GOTO1390
          ENDIF
        ENDIF
      ELSE
        IF(NRIGHT.EQ.NLEFT)GOTO1390
      ENDIF
C
 1309 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1312)
 1312 FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE INDEPENDENT ',
     1       'VARIABLES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1322)
 1322 FORMAT('      MUST BE THE SAME AS THE DEPENDENT VARIABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1323)
 1323 FORMAT('      IN ADDITION, THE VARIABLE CONTAINING THE X ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1324)
 1324 FORMAT('      VALUES FOR THE TOLERANCE LIMITS MUST HAVE AT ',
     1       'LEAST 2 ELEMENTS.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1327)
 1327 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,318)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,2008)(IANS(J),J=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
 1390 CONTINUE
C
 1300 CONTINUE
 1399 CONTINUE
C
C               **********************************************
C               **  STEP 6.3--                              **
C               **  FOR ALL VARIATIONS OF THE RECIPE COMMAND,*
C               **  CHECK TO SEE THE TYPE CASE--            **
C               **    1) UNQUALIFIED (THAT IS, FULL);       **
C               **    2) SUBSET/EXCEPT; OR                  **
C               **    3) FOR.                               **
C               **********************************************
C
      ISTEPN='6.3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO490
      DO400J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GOTO410
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GOTO410
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GOTO420
  400 CONTINUE
      GOTO490
  410 CONTINUE
      ICASEQ='SUBS'
      IKEY='SUBS'
      IF(IHARG(J1).EQ.'EXCE')IKEY='EXCE'
      ILOCQ=J1
      GOTO490
  420 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO490
  490 CONTINUE
      IF(IBUGA2.EQ.'OFF'.AND.ISUBRO.NE.'RECI')GOTO495
      WRITE(ICOUT,491)NUMARG,ILOCQ
  491 FORMAT('NUMARG,ILOCQ = ',2I8)
      CALL DPWRST('XXX','BUG ')
  495 CONTINUE
C
C               *******************************************************
C               **  STEP 12--                                        **
C               **  BRANCH TO THE APPROPRIATE SUBCASE; THEN          **
C               **  COPY OVER THE RESPONSE VECTOR TO BE USED IN THE  **
C               **  MODEL INTO THE VECTOR Y2; AND                    **
C               **  COPY OVER THE VECTORS THAT WERE USED IN THE MODEL**
C               **  INTO THE FULL DESIGN MATRIX                      **
C               *******************************************************
C
      ISTEPN='12'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')WRITE(ICOUT,601)NLEFT,NUMVAR
  601 FORMAT('NLEFT,NUMVAR = ',2I8)
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')CALL DPWRST('XXX','BUG ')
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
      NTOT=NQ
      K=ICOLL
      J=0
      DO4500I=1,NLEFT
      IF(ISUB(I).EQ.0)GOTO4500
      J=J+1
      IJ=MAXN*(K-1)+I
      IF(K.LE.MAXCOL)Y2(J)=DBLE(V(IJ))
      IF(K.EQ.MAXCP1)Y2(J)=DBLE(PRED(I))
      IF(K.EQ.MAXCP2)Y2(J)=DBLE(RES(I))
      IF(K.EQ.MAXCP3)Y2(J)=DBLE(YPLOT(I))
      IF(K.EQ.MAXCP4)Y2(J)=DBLE(XPLOT(I))
      IF(K.EQ.MAXCP5)Y2(J)=DBLE(X2PLOT(I))
      IF(K.EQ.MAXCP6)Y2(J)=DBLE(TAGPLO(I))
 4500 CONTINUE
C
      IF(IBUGA2.EQ.'ON')THEN
        DO4503I=1,NTOT
        WRITE(ICOUT,4504)I,Y2(I)
 4504   FORMAT('I,Y2(I)=',I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
 4503   CONTINUE
      ENDIF
C
C     ********************************************************
C     ** DEFINE A VECTOR OF ALL 1'S (FOR THE CONSTANT TERM) **
C     ** IN THE DESIGN MATRIX.                              **
C     ********************************************************
C
      J=0
      DO380I=1,NLEFT
      IF(ISUB(I).EQ.0)GOTO380
      J=J+1
      XMAT(J)=1.0D0
  380 CONTINUE
C
C     ********************************************************
C     ** DETERMINE IF THERE IS A BATCH VARIABLE.  IF NOT,   **
C     ** CREATE ONE EQUAL TO ALL 1'S.  IF YES, DETERMINE    **
C     ** HOW MANY UNIQUE VALUES.                            **
C     ********************************************************
C
      IF(ILOCB.LE.0)THEN
        J=0
        DO4610I=1,NLEFT
          IF(ISUB(I).EQ.0)GOTO4610
          J=J+1
          IQ(J)=1
 4610   CONTINUE
        NBCH=1
        GOTO4699
      ENDIF
C
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        K=ICOLR(NUMVAR)
        IF(ILOCXP.GT.0)K=ICOLR(NUMVAR-1)
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        K=ICOLR(ILOCB)
      ELSE
        K=ICOLR(NUMVAR)
      ENDIF
C
      J=0
      DO4600I=1,NLEFT
      IF(ISUB(I).EQ.0)GOTO4600
      J=J+1
      IJ=MAXN*(K-1)+I
      IF(K.LE.MAXCOL)RES2(J)=V(IJ)
      IF(K.EQ.MAXCP1)RES2(J)=PRED(I)
      IF(K.EQ.MAXCP2)RES2(J)=RES(I)
      IF(K.EQ.MAXCP3)RES2(J)=YPLOT(I)
      IF(K.EQ.MAXCP4)RES2(J)=XPLOT(I)
      IF(K.EQ.MAXCP5)RES2(J)=X2PLOT(I)
      IF(K.EQ.MAXCP6)RES2(J)=TAGPLO(I)
 4600 CONTINUE
C
      CALL SORT(RES2,NQ,PRED2)
      IWRITE='NO'
      CALL DISTIN(PRED2,NQ,IWRITE,PRED2,NBCH,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      DO4650I=1,NQ
        IQ(I)=0
        DO4660J=1,NBCH
          IF(RES2(I).EQ.PRED2(J))THEN
            IQ(I)=J
            GOTO4650
          ENDIF
 4660   CONTINUE
 4650 CONTINUE
C
 4699 CONTINUE
C
      IF(IBUGA2.EQ.'ON')THEN
        DO4603I=1,NTOT
        WRITE(ICOUT,4604)I,IQ(I)
 4604   FORMAT('I,IQ(I)=',I8,2X,I8)
        CALL DPWRST('XXX','BUG ')
 4603   CONTINUE
      ENDIF
C
C     ********************************************************
C     ** DETERMINE IF THERE IS A PREDICTED VARIABLE (FIT    **
C     ** CASE ONLY).  IF SO, EXTRACT AND PUT IN XPTS.       **
C     ********************************************************
C
      IF(ICASRE.EQ.'UREC')THEN
        XPTS(1)=1.D0
        NPRED=1
        NPAR=1
        GOTO4799
      ELSEIF(ILOCXP.LT.0.OR.ICASRE.EQ.'AREC')THEN
        DO4701I=1,MAXOBV/2
          XPTS(I)=0.D0
 4701   CONTINUE
        NPRED=0
        GOTO4799
      ENDIF
C
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        K=ICOLR(NUMVAR)
        DO4703I=1,NPRED
          XPTS(I)=1.D0
 4703   CONTINUE
        J=NPRED
        DO4700I=1,NPRED
        IF(ISUB(I).EQ.0)GOTO4700
        J=J+1
        IJ=MAXN*(K-1)+I
        IF(K.LE.MAXCOL)XPTS(J)=DBLE(V(IJ))
        IF(K.EQ.MAXCP1)XPTS(J)=DBLE(PRED(I))
        IF(K.EQ.MAXCP2)XPTS(J)=DBLE(RES(I))
        IF(K.EQ.MAXCP3)XPTS(J)=DBLE(YPLOT(I))
        IF(K.EQ.MAXCP4)XPTS(J)=DBLE(XPLOT(I))
        IF(K.EQ.MAXCP5)XPTS(J)=DBLE(X2PLOT(I))
        IF(K.EQ.MAXCP6)XPTS(J)=DBLE(TAGPLO(I))
 4700   CONTINUE
C
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        DO5903I=1,NPRED
          XPTS(I)=1.D0
 5903   CONTINUE
        NLOOP=IFITFC
        ISTRT=IFITFC+1
        IF(ILOCB.GT.0)ISTRT=ISTRT+1
        ISTOP=ISTRT+IFITFC-1 
        DO5376IVAR=ISTRT,ISTOP
          K=ICOLR(IVAR)
          J=(IVAR-ISTRT+1)*NPRED
          DO5371I=1,NPRED
            IF(ISUB(I).EQ.0)GOTO5371
            J=J+1
            IJ=MAXN*(K-1)+I
            IF(K.LE.MAXCOL)XPTS(J)=DBLE(V(IJ))
            IF(K.EQ.MAXCP1)XPTS(J)=DBLE(PRED(I))
            IF(K.EQ.MAXCP2)XPTS(J)=DBLE(RES(I))
            IF(K.EQ.MAXCP3)XPTS(J)=DBLE(YPLOT(I))
            IF(K.EQ.MAXCP4)XPTS(J)=DBLE(XPLOT(I))
            IF(K.EQ.MAXCP5)XPTS(J)=DBLE(X2PLOT(I))
            IF(K.EQ.MAXCP6)XPTS(J)=DBLE(TAGPLO(I))
 5371     CONTINUE
 5376   CONTINUE
      ENDIF
C
 4799 CONTINUE
C
      IF(IBUGA2.EQ.'ON')THEN
        DO4713I=1,2*NPRED
        WRITE(ICOUT,4714)I,XPTS(I)
 4714   FORMAT('I,XPTS(I)=',I8,2X,D15.7)
        CALL DPWRST('XXX','BUG ')
 4713   CONTINUE
      ENDIF
C
C     ********************************************************
C     ** COPY OVER THE FULL DESIGN MATRIX.                  **
C     ********************************************************
C
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        NPAR=1
        IF(ICASDG.EQ.'0')GOTO379
        IF(ICASDG.EQ.'1')NLOOP=1
        IF(ICASDG.EQ.'2')NLOOP=2
        IF(ICASDG.EQ.'3')NLOOP=3
        IF(ICASDG.EQ.'4')NLOOP=4
        IF(ICASDG.EQ.'5')NLOOP=5
        IF(ICASDG.EQ.'6')NLOOP=6
        IF(ICASDG.EQ.'7')NLOOP=7
        IF(ICASDG.EQ.'8')NLOOP=8
        IF(ICASDG.EQ.'9')NLOOP=9
        IF(ICASDG.EQ.'10')NLOOP=10
        K=ICOLR(1)
        DO376IVAR=1,NLOOP
          J=IVAR*NTOT
          DO371I=1,NLEFT
            IF(ISUB(I).EQ.0)GOTO371
            J=J+1
C
            IF(J.GT.MAXPT2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,377)
  377         FORMAT('      STORAGE SPACE FOR DESIGN SPACE ',
     1               'EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
C
            IJ=MAXN*(K-1)+I
            IF(K.LE.MAXCOL)XMAT(J)=DBLE(V(IJ)**NLOOP)
            IF(K.EQ.MAXCP1)XMAT(J)=DBLE(PRED(I)**NLOOP)
            IF(K.EQ.MAXCP2)XMAT(J)=DBLE(RES(I)**NLOOP)
            IF(K.EQ.MAXCP3)XMAT(J)=DBLE(YPLOT(I)**NLOOP)
            IF(K.EQ.MAXCP4)XMAT(J)=DBLE(XPLOT(I)**NLOOP)
            IF(K.EQ.MAXCP5)XMAT(J)=DBLE(X2PLOT(I)**NLOOP)
            IF(K.EQ.MAXCP6)XMAT(J)=DBLE(TAGPLO(I)**NLOOP)
  371     CONTINUE
  376   CONTINUE
        NPAR=NLOOP+1
  379   CONTINUE
C
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        NPAR=1
        NLOOP=IFITFC
        DO1376IVAR=1,NLOOP
          K=ICOLR(IVAR)
          J=IVAR*NTOT
          DO1371I=1,NLEFT
            IF(ISUB(I).EQ.0)GOTO1371
            J=J+1
C
            IF(J.GT.MAXPT2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,377)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
C
            IJ=MAXN*(K-1)+I
            IF(K.LE.MAXCOL)XMAT(J)=DBLE(V(IJ))
            IF(K.EQ.MAXCP1)XMAT(J)=DBLE(PRED(I))
            IF(K.EQ.MAXCP2)XMAT(J)=DBLE(RES(I))
            IF(K.EQ.MAXCP3)XMAT(J)=DBLE(YPLOT(I))
            IF(K.EQ.MAXCP4)XMAT(J)=DBLE(XPLOT(I))
            IF(K.EQ.MAXCP5)XMAT(J)=DBLE(X2PLOT(I))
            IF(K.EQ.MAXCP6)XMAT(J)=DBLE(TAGPLO(I))
 1371     CONTINUE
 1376   CONTINUE
        NPAR=NLOOP+1
      ELSEIF(ICASRE.EQ.'UREC')THEN
        NPAR=1
        J=NTOT
CCCCC   DO372I=1,NLEFT
CCCCC     IF(ISUB(I).EQ.0)GOTO372
CCCCC     J=J+1
CCCCC     XMAT(J)=1.D0
C372    CONTINUE
      ELSEIF(ICASRE.EQ.'AREC')THEN
        NLOOP=NUMVAR
        IF(ILOCB.GT.0)NLOOP=NUMVAR-1
        DO389IVAR=1,NLOOP
          K=ICOLR(IVAR)
          J=IVAR*NTOT
          DO381I=1,NLEFT
            IF(ISUB(I).EQ.0)GOTO381
            J=J+1
C
            IF(J.GT.MAXPT2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,377)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
C
            IJ=MAXN*(K-1)+I
            IF(K.LE.MAXCOL)XMAT(J)=DBLE(V(IJ))
            IF(K.EQ.MAXCP1)XMAT(J)=DBLE(PRED(I))
            IF(K.EQ.MAXCP2)XMAT(J)=DBLE(RES(I))
            IF(K.EQ.MAXCP3)XMAT(J)=DBLE(YPLOT(I))
            IF(K.EQ.MAXCP4)XMAT(J)=DBLE(XPLOT(I))
            IF(K.EQ.MAXCP5)XMAT(J)=DBLE(X2PLOT(I))
            IF(K.EQ.MAXCP6)XMAT(J)=DBLE(TAGPLO(I))
  381     CONTINUE
  389   CONTINUE
        NPAR=NLOOP+1
      ENDIF
C
      IF(IBUGA2.EQ.'ON')THEN
        DO4803I=1,NTOT*NPAR
        WRITE(ICOUT,4804)I,XMAT(I)
 4804   FORMAT('I,XMAT(I)=',I8,2X,D15.7)
        CALL DPWRST('XXX','BUG ')
 4803   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 14--                                       **
C               **  CARRY OUT THE ACTUAL FIT                        **
C               **  VIA CALLING                                     **
C               **  REGINI AND REGDAT                               **
C               ******************************************************
C
      NSTOR=NTOT*(NPAR+NBCH)
      IF(NSTOR.GT.MAXPT1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6071)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6072)NSTOR
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6073)MAXPT1
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000 
      ENDIF
 6071 FORMAT('      THE AMOUNT OF SCRATCH STORAGE REQUIRED EQUALS')
 6072 FORMAT('     NUMBER OF POINTS*(NUMBER OF PARAMETERS + NUMBER OF',
     1       ' BATCHES) = ',I8)
 6073 FORMAT('     EXCEEDS THE MAXIMIM ALLOWABLE OF ',I8)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')THEN
        ISTEPN='14'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6081)
 6081   FORMAT('***** FROM DPRECI, AS ABOUT TO CALL REGINI--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      SATT=.FALSE.
      IF(IRECSA.EQ.'YES'.OR.IRECSA.EQ.'TRUE'.OR.IRECSA.EQ.'ON')
     1   SATT=.TRUE.
      NREPS=IRECR2
      MAXREP=10*MAXOBV
      IF(NREPS.GT.MAXREP)THEN
        NREPS=MAXREP
        WRITE(ICOUT,998)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,6531)NREPS,MAXREP
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,6532)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,998)
        CALL DPWRST('XXX','WRIT')
      ENDIF
 6531 FORMAT('THE REQUESTED NUMBER OF SIMULATION REPLICATIONS ',I8,
     1' IS GREATER THAN THE ALLOWED MAXIMUM OF ',I8)
 6532 FORMAT('THE MAXIMUM ALLOWED NUMBER OF REPLICATIONS WILL BE ',
     1'USED.')
      CALL REGINI(NLVL,NPAR,NTOT,NBCH,NPRED,XDESGN,XPTS,IP,IQ,
     1            DBLE(RECIPC),DBLE(RECICO),XMAT,XTX,XTXI,XN,SCRTCH,
     1            S1,V1,S2,V2,TLM0,TLM1,ETA0,ETA1,
     1            SATT,IN2,WK2,WK3,
     1            CRT,ISEED,MAXREP,MAXLVL,
     1            ICASRE,ISUBRO,IBUGA2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6181)
 6181   FORMAT('***** FROM DPRECI, AS ABOUT TO CALL REGDAT--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFLAG='RECI'
      CALL REGDAT(NPAR,NTOT,NBCH,NPRED,XPTS,Y2,COEF,
     1            SCRTCH,S1,V1,TLM0,TLM1,ETA0,ETA1,
     1            XMAT,XM,T,XDESGN,NLVL,
     1            ICASRE,IFLAG,ISUBRO,IBUGA2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      DSUM1=0.D0
      IF(ICASRE.EQ.'AREC'.OR.(ICASRE.EQ.'FREC'.AND.ILOCXP.LE.0))THEN
        DO1029I=1,NLVL
          TLM0(I)=T(I)
 1029   CONTINUE
      ENDIF
      DO1030I=1,NTOT
        INDX=IP(I)
        PRED2(I)=SNGL(XM(INDX))
        RES2(I)=SNGL(Y2(I))-PRED2(I)
        IF(ICASRE.EQ.'AREC'.OR.(ICASRE.EQ.'FREC'.AND.ILOCXP.LE.0))THEN
          T(I)=TLM0(INDX)
        ENDIF
C
        IF(IBUGA2.EQ.'ON')THEN
          WRITE(ICOUT,11030)I,INDX,PRED2(I),RES2(I)
11030     FORMAT('I,INDX,PRED2(I),RES2(I)=',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DSUM1=DSUM1+DBLE(RES2(I))
 1030 CONTINUE
      DMEAN=DSUM1/DBLE(NTOT)
      DSUM1=0.D0
      DO1031I=1,NTOT
        DSUM1=DSUM1+(DBLE(RES2(I))-DMEAN)**2
 1031 CONTINUE
      RESDF=REAL(NTOT-NPAR)
      IF(ICASRE.EQ.'AREC')RESDF=REAL(NTOT-(NLVL-NUMFAC)-1)
      IF(ICASRE.EQ.'UREC')RESDF=REAL(NTOT-1)
      RESSD=SNGL(DSQRT(DSUM1)/DBLE(RESDF))
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1032)
 1032 FORMAT(20X,'RECIPE ANALYSIS')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1132)
 1132 FORMAT(18X,'(MARK VANGEL, NIST)')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,998)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1033)NTOT
 1033 FORMAT('NUMBER OF OBSERVATIONS         = ',I8)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1034)NLVL
 1034 FORMAT('NUMBER OF UNIQUE DESIGN POINTS = ',I8)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1035)NBCH
 1035 FORMAT('NUMBER OF BATCHES              = ',I8)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,998)
      CALL DPWRST('XXX','WRIT')
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        IMODEL='LINEAR FIT'
        IF(ICASDG.EQ.'0')IMODEL='0-DEGREE FIT'
        IF(ICASDG.EQ.'2')IMODEL='QUADRATIC FIT'
        IF(ICASDG.EQ.'3')IMODEL='CUBIC FIT'
        IF(ICASDG.EQ.'4')IMODEL='4TH-DEGREE FIT'
        IF(ICASDG.EQ.'5')IMODEL='5TH-DEGREE FIT'
        IF(ICASDG.EQ.'6')IMODEL='6TH-DEGREE FIT'
        IF(ICASDG.EQ.'7')IMODEL='7TH-DEGREE FIT'
        IF(ICASDG.EQ.'8')IMODEL='8TH-DEGREE FIT'
        IF(ICASDG.EQ.'9')IMODEL='9TH-DEGREE FIT'
        IF(ICASDG.EQ.'10')IMODEL='10TH-DEGREE FIT'
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        IMODEL='MULTI-LINEAR FIT'
      ELSEIF(ICASRE.EQ.'UREC')THEN
        IMODEL='UNIVARIATE'
      ELSE
        IF(NUMFAC.EQ.0)THEN
          IMODEL='UNIVARIATE'
        ELSE
          IMODEL='ANOVA'
        ENDIF
      ENDIF
      WRITE(ICOUT,1036)IMODEL
 1036 FORMAT('MODEL: ',A20)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1037)RESSD
 1037 FORMAT('RESSD FROM THE FITTED MODEL    = ',E15.7)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1237)RESDF
 1237 FORMAT('RESDF FROM THE FITTED MODEL    = ',F10.0)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,998)
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,1136)100*RECIPC
 1136 FORMAT('PROBABILITY CONTENT            = ',F10.5,'%')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1137)100*RECICO
 1137 FORMAT('PROBABILITY CONFIDENCE         = ',F10.5,'%')
      CALL DPWRST('XXX','WRIT')
      IF(IRECSA.EQ.'YES'.OR.IRECSA.EQ.'ON'.OR.IRECSA.EQ.'TRUE')THEN
        WRITE(ICOUT,1138)
 1138   FORMAT('SATTERTHWAITE APPROXIMATION USED')
        CALL DPWRST('XXX','WRIT')
      ELSE
        WRITE(ICOUT,1139)MAXREP
 1139   FORMAT('SIMULATED CRITICAL VALUES (SIMPVT) USED WITH ',
     1         I8,' REPLICATIONS')
        CALL DPWRST('XXX','WRIT')
      ENDIF
      WRITE(ICOUT,998)
  998 FORMAT(' ')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,998)
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1701)IRECTN(1:8)
 1701 FORMAT('TOLERANCE VALUES STORED IN VARIABLE ',A8)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1702)
 1702 FORMAT('RESIDUALS        STORED IN VARIABLE RES')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1703)
 1703 FORMAT('PREDICTED VALUES STORED IN VARIABLE PRED')
      CALL DPWRST('XXX','WRIT')
C
C               ***************************************
C               **  STEP 15--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='15'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
      IREPU='OFF'
      IRESU='ON'
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NLEFT,
     1IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 16--                        **
C               **  STORE THE TOLERANCE VALUES       **
C               ***************************************
      IH=IRECTN(1:4)
      IH2=IRECTN(5:8)
C
      NEWNAM='NO'
      DO7650I=1,NUMNAM
      I2=I
      IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1IUSE(I).EQ.'V')THEN
        ICOLL1=IVALUE(I2)
        GOTO7680
      ENDIF
      IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1IUSE(I).NE.'V')THEN
        WRITE(ICOUT,7646)
 7646   FORMAT('***** ERROR IN DPRECI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7647)IRECTN
 7647   FORMAT('      THE REQUESTED NAME FOR THE TOLERANCE ',
     1         'VARIABLE, ',A8,', WAS FOUND IN THE') 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7648)
 7648   FORMAT('      CURRENT NAME LIST, BUT NOT AS A VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7649)
 7649   FORMAT('      THEREFORE THE TOLERANCE VARIABLE WAS NOT ',
     1         'UPDATED.')
        CALL DPWRST('XXX','BUG ')
        GOTO7699
      ENDIF
 7650 CONTINUE
      NEWNAM='YES'
C
C  NEW VARIABLE, CHECK TO ENSURE MAXIMUM NAMES AND MAXIMUM
C  COLUMNS NOT EXCEEDED.
C
      IF(NUMNAM.GE.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7652)
 7652   FORMAT('      THE TOTAL NUMBER OF (VARIABLE + PARAMETER)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7653)MAXNAM
 7653   FORMAT('      NAMES MUST BE AT MOST ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7654)
 7654   FORMAT('      SUCH WAS NOT THE CASE HERE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7655)
 7655   FORMAT('      THE MAXIMUM ALLOWABLE NUMBER OF NAMES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7656)
 7656   FORMAT('      WAS JUST EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7657)
 7657   FORMAT('      SUGGESTED ACTION--ENTER     STAT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7658)
 7658   FORMAT('      TO DETERMINE THE IMPORTANT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7659)
 7659   FORMAT('      (VERSUS UNIMPORTANT) VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7660)
 7660   FORMAT('      AND PARAMETERS, AND THEN REUSE SOME')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7661)
 7661   FORMAT('      OF THE NAMES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7662)
 7662   FORMAT('      THE TOLERANCE VARIABLE WAS NOT UPDATED--')
        CALL DPWRST('XXX','BUG ')
        GOTO7699
      ENDIF
C
      IF(NUMCOL.GE.MAXCOL)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2001)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7666)
 7666   FORMAT('      THE NUMBER OF DATA COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7667)MAXCOL
 7667   FORMAT('      HAS JUST EXCEEDED THE MAX ALLOWABLE ',I8,'  .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7668)
 7668   FORMAT('      SUGGESTED ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7669)
 7669   FORMAT('      ENTER      STATUS VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7670)
 7670   FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7671)
 7671   FORMAT('      AND THEN DELETE SOME COLUMNS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7672)
 7672   FORMAT('      THE TOLERANCE VARIABLE WAS NOT UPDATED--')
        CALL DPWRST('XXX','BUG ')
        GOTO7699
      ENDIF
C
 7680 CONTINUE
      IF(NEWNAM.EQ.'YES')THEN
        NUMCOL=NUMCOL+1
        ICOLL1=NUMCOL
        NUMNAM=NUMNAM+1
        IHNAME(NUMNAM)=IH
        IHNAM2(NUMNAM)=IH2
        IUSE(NUMNAM)='V'
        VALUE(NUMNAM)=ICOLL1
        IVALUE(NUMNAM)=ICOLL1
        NTEMP=NTOT
        IF(ICASRE.EQ.'FREC'.AND.ILOCXP.GT.0)NTEMP=NPRED
        IN(NUMNAM)=NTEMP
        IF(IBUGA2.EQ.'ON')THEN
          WRITE(ICOUT,7683)IN(NUMNAM)
 7683     FORMAT('IN(NUMNAM)=',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSE
        NTEMP=NTOT
        IF(ICASRE.EQ.'FREC'.AND.ILOCXP.GT.0)NTEMP=NPRED
        IF(ICASRE.EQ.'UREC')NTEMP=1
        IN(ICOLL1)=NTEMP
        IF(IBUGA2.EQ.'ON')THEN
          WRITE(ICOUT,7686)IN(ICOLL1)
 7686     FORMAT('IN(ICOLL1)=',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,7681)NEWNAM,ICOLL1,NUMCOL,NUMNAM,NPRED,NTEMP
        CALL DPWRST('XXX','BUG ')
 7681   FORMAT('NEWNAM,ICOLL1,NUMCOL,NUMNAM,NPRED,NTEMP =',
     1         A4,1X,5I8)
      ENDIF
C
      K=ICOLL1
      DO7682I=1,NTEMP
        IJ=MAXN*(K-1)+I
        IF(K.LE.MAXCOL)V(IJ)=T(I)
        IF(K.EQ.MAXCP1)PRED(I)=T(I)
        IF(K.EQ.MAXCP1)RES(I)=T(I)
        IF(K.EQ.MAXCP1)YPLOT(I)=T(I)
        IF(K.EQ.MAXCP1)XPLOT(I)=T(I)
        IF(K.EQ.MAXCP1)X2PLOT(I)=T(I)
        IF(K.EQ.MAXCP1)TAGPLO(I)=T(I)
 7682 CONTINUE
C
 7699 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRECI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NPAR,NTOT,NBCH,NLVL,NUMNAM,IWIDTH
 9015   FORMAT('NPAR,NTOT,NBCH,NLEVL,NUMNAM,IWIDTH = ',6I8)
        CALL DPWRST('XXX','BUG ')
        DO9017I=1,NUMNAM
          WRITE(ICOUT,9018)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),
     1                     IVALUE(I),VALUE(I)
 9018     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I)',
     1           'VALUE(I) = ',I8,2X,2A4,2X,A4,2I8,F15.7)
          CALL DPWRST('XXX','BUG ')
 9017   CONTINUE
        WRITE(ICOUT,9052)ICASRE,ICASEQ,IFOUND,IERROR
 9052   FORMAT('ICASRE,ICASEQ,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9062)(IANS(I),I=1,MIN(100,IWIDTH))
 9062     FORMAT('(IANS(I),I=1,IWIDTH) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRECO(IHARG,IARGT,ARG,NUMARG,DEFRCO,
     1RECICO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE RECIPE CONFIDENCE
C              IN THE FLOATING POINT VARIABLE RECICO.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFRCO (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--RECICO  (A  FLOATING POINT VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
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
      HOLD=CPUMIN
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1140
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONF')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'CONT')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPRECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE CONFIDENCE ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE CONFIDENCE .90 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1140 CONTINUE
      IF(NUMARG.EQ.2)HOLD=DEFRCO
      IF(NUMARG.GT.2.AND.IARGT(NUMARG).EQ.'NUMB')HOLD=ARG(NUMARG)
      GOTO1180
C
 1150 CONTINUE
      HOLD=DEFRCO
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(HOLD.GE.1.0 .AND. HOLD.LT.100.0)HOLD=HOLD/100.
      IF(HOLD.LE.0.0 .OR. HOLD.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182 FORMAT('**** THE RECIPE CONFIDENCE MUST BE SET TO BETWEEN 0 AND',
     1' 1 EXCLUSIVE (TYPICAL VALUES BETWEEN .9 AND .99)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)HOLD
 1183 FORMAT('     THE VALUE ENTERED WAS ',E15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO1199
      ENDIF
      RECICO=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECICO
 1181 FORMAT('THE RECIPE CONFIDENCE HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPRECR(IHARG,IARGT,IARG,NUMARG,IDEFCR,
     1                  IRECCR,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE NUMBER OF RECIPE CORRELATION POINTS
C              TO USE FOR THE SIMCOV COMMAND
C              IN THE INTEGER POINT VARIABLE IRECCR.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --IARG   (A  INTEGER POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFCR (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--IRECCR  (A  FLOATING POINT VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IHOLD=0
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1140
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORR')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'CORR')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPRECR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE CORRELATION ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE CORRELATION 11 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1140 CONTINUE
      IF(NUMARG.EQ.2)IHOLD=IDEFCR
      IF(NUMARG.GT.2.AND.IARGT(NUMARG).EQ.'NUMB')IHOLD=IARG(NUMARG)
      GOTO1180
C
 1150 CONTINUE
      IHOLD=IDEFCR
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(IHOLD.LE.3)IHOLD=3
      IF(IHOLD.GE.100)IHOLD=100
      IRECCR=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECCR
 1181 FORMAT('THE NUMBER OF CORRELATION POINTS FOR THE SIMCOV ',
     1'COMMAND HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPREDG(IHARG,IARGT,ARG,NUMARG,DEFRDG,
     1RECIDG,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE RECIPE FIT DEGREE
C              IN THE FLOATING POINT VARIABLE RECIDG.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFRDG (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--RECIDG  (A  FLOATING POINT VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
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
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'FIT '.AND.IHARG(2).EQ.'DEGR')
     1GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DEGR')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'DEGR')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPREDG--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE FIT DEGREE ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE FIT DEGREE 2 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFRDG
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(HOLD.LT.0.0 .OR. HOLD.GT.10.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182 FORMAT('**** THE RECIPE DEGREE MUST BE SET TO BETWEEN 0 AND 10',
     1' (WITH TYPICAL VALUES BEING 1 OR 2)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)HOLD
 1183 FORMAT('     THE VALUE ENTERED WAS ',E15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO1199
      ENDIF
      RECIDG=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECIDG
 1181 FORMAT('THE RECIPE FIT DEGREE HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPREFA(IHARG,IARGT,ARG,NUMARG,DEFRFA,
     1RECIFA,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE RECIPE ANOVA FACTORS
C              IN THE FLOATING POINT VARIABLE RECIFA.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFRFA (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--RECIFA  (A  FLOATING POINT VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
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
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'ANOV'.AND.IHARG(2).EQ.'FACT')
     1GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FACT')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'FACT')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPREFA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE ANOVA FACTORS ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE ANOVA FACTORS 2 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFRFA
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(HOLD.LT.0.51)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182 FORMAT('**** THE RECIPE FACTORS MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)HOLD
 1183 FORMAT('     THE VALUE ENTERED WAS ',E15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO1199
      ENDIF
      RECIFA=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECIFA
 1181 FORMAT('THE NUMBER OF FACTORS FOR RECIPE ANOVA HAS JUST BEEN ',
     1'SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPREFF(IHARG,IARGT,ARG,NUMARG,DEFRFF,
     1RECIFF,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE RECIPE FIT FACTORS
C              IN THE FLOATING POINT VARIABLE RECIFF.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFRFF (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--RECIFF  (A  FLOATING POINT VARIABLE)
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
C     VERSION NUMBER--98/4
C     ORIGINAL VERSION--APRIL    1998.
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
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'FIT '.AND.IHARG(2).EQ.'FACT')
     1GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FACT')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'FACT')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPREFF--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE FIT FACTORS ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE FIT FACTORS 2 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFRFF
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(HOLD.LT.0.51)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182 FORMAT('**** THE RECIPE FACTORS MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)HOLD
 1183 FORMAT('     THE VALUE ENTERED WAS ',E15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO1199
      ENDIF
      RECIFF=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECIFF
 1181 FORMAT('THE NUMBER OF FACTORS FOR RECIPE FIT HAS JUST BEEN ',
     1'SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPREGR(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--REPEAT A PREVIOUSLY CREATED PLOT AND PUT ON A DISTINCT
C              GRAPHICS WINDOW.
C
C                  REPEAT PLOT <FILE NAME>:
C                      REDRAWS THE PIXMAP FROM A SPECIFIED FILE
C
C                  REPEAT PLOT <+N>:
C                      REDRAWS THE Nth PIXMAP FROM THE CURRENT LIST
C
C                  REPEAT PLOT <-N>:
C                      REDRAWS THE Nth PIXMAP AGO FROM THE CURRENT LIST
C                      (E.G., IF THERE ARE CURRENTLY 8 PIXMAPS,
C                      REPEAT PLOT -2 PLOTS THE SIXTH PIXMAP
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGU
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--97/4
C     ORIGINAL VERSION--APRIL     1997.
C     UPDATED         --AUGUST    1997. MOVE SOME CODE TO A LOWER LEVEL
C                                       TO SUPPORT NON-X11 DEVICES
C                                       (SPECIFICALLY PC FOR NOW)
C     UPDATED         --AUGUST    2016. ARGUMENT LIST TO DPINFI
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IARGT(*)
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPEN
      CHARACTER*8 IACC
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUND
C
      CHARACTER*4 IC4
      CHARACTER*4 ICODE
      CHARACTER*128 CTEMP
C  DIMENSION FOLLOWING 2 LINES TO MAXSTR
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXSTR) :: ISTRI2
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      DIMENSION IARG(*)
CCCCC DIMENSION IADE(128)
CCCCC DIMENSION IADE2(138)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPM.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRE'
      ISUBN2='GR  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REGR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPREGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,IFOUND,IERROR
   52   FORMAT('IBUGS2,ISUBRO,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IWIDTH,NUMARG
   53   FORMAT('IWIDTH,NUMARG = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,54)(IANSLC(I),I=1,MIN(80,IWIDTH))
   54     FORMAT('(IANSLC(I),I=1,IWIDTH) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(NUMARG.GE.1)THEN
          DO56I=1,NUMARG
            WRITE(ICOUT,57)I,IHARG(I)
   57       FORMAT('I,IHARG(I) = ',I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
   56     CONTINUE
        ENDIF
      ENDIF
C
      IFOUND='YES'
C
C               ******************************************************
C               **  STEP 11--                                       **
C               **  DETERMINE IF HAVE AN EXPLICIT FILE REFERENCE    **
C               **  WHERE THE PIXMAP FILE IS STORED                 **
C               ******************************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REGR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFILWD=(-999)
C
      DO1100I=1,MAXSTR
        IC4=IANSLC(I)
        ISTRIN(I:I)=IC4(1:1)
 1100 CONTINUE
C
      IWORD=1
      ISTART=1
      ISTOP=MAXSTR-1
      CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1ICOL1,ICOL2,ISTRI2,NCSTR2,
     1IBUGS2,ISUBRO,IERROR)
C
      IF(NUMARG.LE.0)GOTO1129
      IWORD=2
      ISTART=1
      ISTOP=MAXSTR-1
      CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1ICOL1,ICOL2,ISTRI2,NCSTR2,
     1IBUGS2,ISUBRO,IERROR)
      IF(NCSTR2.LE.0)GOTO1129
      DO1121I=1,NCSTR2
      IF(ISTRI2(I:I).EQ.'.')GOTO1122
 1121 CONTINUE
      GOTO1129
 1122 CONTINUE
      IFILWD=2
      GOTO1150
 1129 CONTINUE
C
      IF(NUMARG.LE.1)GOTO1139
      IWORD=3
      ISTART=1
      ISTOP=MAXSTR-1
      CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1ICOL1,ICOL2,ISTRI2,NCSTR2,
     1IBUGS2,ISUBRO,IERROR)
      IF(NCSTR2.LE.0)GOTO1139
      DO1131I=1,NCSTR2
      IF(ISTRI2(I:I).EQ.'.')GOTO1132
 1131 CONTINUE
      GOTO1139
 1132 CONTINUE
      IFILWD=3
      GOTO1150
 1139 CONTINUE
C
 1150 CONTINUE
      ISTAM1=0
      IF(IFILWD.EQ.2.OR.IFILWD.EQ.3)ISTAM1=1
C
C               ******************************************************
C               **  STEP 11.B--IF NO FILE, CHECK FOR NUMBER         **
C               ******************************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REGR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFILWD.LE.0)THEN
        IF(NUMARG.GE.1.AND.IARGT(NUMARG).EQ.'NUMB')THEN
          IHOLD=IARG(NUMARG)
          IF(IHOLD.GT.0)THEN
            IF(IHOLD.LE.NUMPXM)THEN
              ICURPM=IHOLD
              ISTRI2(1:128)=IPXMFN(ICURPM)(1:128)
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1161)IHOLD
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1163)NUMPXM
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
          ELSE
            IF(NUMPXM-ABS(IHOLD).GT.0)THEN
              ICURPM=NUMPXM-ABS(IHOLD)
              ISTRI2(1:128)=IPXMFN(ICURPM)(1:128)
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1171)IHOLD
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1173)NUMPXM
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
          ENDIF
        ELSE
          IF(NUMPXM.GT.0)THEN
            ICURPM=NUMPXM
            ISTRI2(1:128)=IPXMFN(ICURPM)(1:128)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1181)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1183)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
        ENDIF
        NCSTR2=0
        DO1187I=128,1,-1
            NCSTR2=I
            IF(ISTRI2(I:I).NE.' ')GOTO1189
 1187   CONTINUE
 1189   CONTINUE
C
C  FOR PIXMAP SPECIFIED BY FILE NAME, CHECK CURRENT LIST.  IF NOT
C  FOUND, ADD TO LIST.
C
      ELSE
        ICURPM=0
        IF(NUMPXM.GE.1)THEN
          DO1191I=1,NUMPXM
            IF(ISTRI2(1:128).EQ.IPXMFN(I)(1:128))THEN
              ICURPM=I
              GOTO1199
            ENDIF
 1191     CONTINUE
          IF(NUMPXM.LT.MAXPM)THEN
            NUMPXM=NUMPXM+1
            IPXMFN(NUMPXM)(1:128)=ISTRI2(1:128)
            IPXMCM(NUMPXM)(1:128)=ISTRI2(1:128)
            ICURPM=NUMPXM
          ENDIF
 1199     CONTINUE
        ELSE
          NUMPXM=NUMPXM+1
          IPXMFN(NUMPXM)(1:128)=ISTRI2(1:128)
          IPXMCM(NUMPXM)(1:128)=ISTRI2(1:128)
          ICURPM=NUMPXM
        ENDIF
      ENDIF
 1161 FORMAT('***** ERROR IN DPREGR: THE SPECIFIED PIXMAP NUMBER (',I5,
     1') IS GREATER THAN ')
 1163 FORMAT('      THE NUMBER OF CURRENTLY DEFINED PIXMAPS (',I5,')')
 1171 FORMAT('***** ERROR IN DPREGR: YOU ASKED FOR (',I5,') PLOTS ',
     1'AGO AND')
 1173 FORMAT('      THERE ARE ONLY (',I5,') PIXMAPS CURRENTLY SAVED.')
 1181 FORMAT('***** ERROR IN DPREGR: NO PIXMAP FILE NAME OR NUMBER ',
     1'WAS SPECIFIED.')
 1183 FORMAT('      HOWEVER, THERE ARE CURRENTLY NO PIXMAPS SAVED.')
C
C               *******************************
C               **  STEP 12--                **
C               **  CALL XRESTG              **
C               *******************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REGR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISUBN0='REGR'
      CALL DPINFI(ISTRI2,IEXIST,IOPEN,IACC,ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.NE.'YES'.OR.IERROR.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1203)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1204)ISTRI2(1:MIN(NCSTR2,80))
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
 1203 FORMAT('***** ERROR IN DPREGR--UNABLE TO OPEN THE REQUESTED ',
     1       'PLOT.')
 1204 FORMAT('      THE REQUESTED FILE IS: ',A80)
C
      IF(NCSTR2.GT.127)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1209)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
 1209 FORMAT('***** ERROR IN DPREGR--FILE NAME EXCEEDS 127 ',
     1'CHARACTERS.')
      ENDIF
      CTEMP=' '
      IF(ICURPM.LE.9)THEN
        CTEMP(1:4)='  - '
        WRITE(CTEMP(1:1),'(I1)')ICURPM
        NCTEMP=4
      ELSEIF(ICURPM.LE.99)THEN
        CTEMP(1:5)='   - '
        WRITE(CTEMP(1:2),'(I2)')ICURPM
        NCTEMP=5
      ELSEIF(ICURPM.LE.999)THEN
        CTEMP(1:6)='    - '
        WRITE(CTEMP(1:3),'(I3)')ICURPM
        NCTEMP=6
      ENDIF
C
      DO1220I=1,NCSTR2
        NCTEMP=NCTEMP+1
        CTEMP(NCTEMP:NCTEMP)=ISTRI2(I:I)
 1220 CONTINUE
C
C  AUGUST 1997.  GENERALIZE THIS ROUTINE FOR NON-X11 DEVICES.
C  CALL A LOWER LEVEL ROUTINE, MOVE FOLLOWING CODE TO THAT ROUTINE.
C
      ICODE='REST'
      CALL GRSAGR(ICODE,ISTRI2,NCSTR2,CTEMP,NCTEMP)
CCCCC DO1215I=1,NCTEMP
CCCCC   CALL DPCOAN(CTEMP(I:I),IADE2(I))
C1215 CONTINUE
CCCCC DO1220I=1,NCSTR2
CCCCC   CALL DPCOAN(ISTRI2(I:I),IADE(I))
CCCCC   CALL DPCOAN(ISTRI2(I:I),IADE2(I+NCTEMP))
C1220 CONTINUE
CCCCC IADE(NCSTR2+1)=0
CCCCC IADE2(NCSTR2+NCTEMP+1)=0
C
CCCCC IERR=0
CCCCC CALL XRESTG(IADE,IADE2,IERR)
CCCCC IF(IERR.EQ.1)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1251)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1251 FORMAT('***** ERROR IN DPREGR--READING BIT MAP UNSUCCESSFUL.')
CCCCC ELSEIF(IERR.EQ.2)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1261)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1261 FORMAT('***** ERROR IN DPREGR--NO CURRENT PIXMAP TO SAVE.')
CCCCC ELSEIF(IERR.EQ.3)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1271)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1271 FORMAT('***** ERROR IN DPREGR--X11 HAS NOT BEEN OPENED.')
CCCCC ELSEIF(IERR.EQ.4)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1281)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1281 FORMAT('***** ERROR IN DPREGR--X11 NOT INSTALLED ON THIS ',
CCCCC1'IMPLEMENTATION.')
CCCCC ELSEIF(IERR.EQ.5)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1286)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1286 FORMAT('***** ERROR IN DPREGR--UNABLE TO OPEN NEW X11 WINDOW ')
CCCCC ELSE
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1291)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1292)ISTRI2(1:NCSTR2)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1291 FORMAT('***** CURRENT PIXMAP SUCCESSFULLY COPIED FROM FILE ')
C1292 FORMAT('      ',A128)
CCCCC ENDIF
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REGR')THEN
        WRITE(ICOUT,1293)ISTRI2(1:NCSTR2)
 1293   FORMAT('ISTRI2 = ',A128)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1294)NCSTR2
 1294   FORMAT('NCSTR2 = ',I4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REGR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DPREGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPREMA(IHARG,NUMARG,
     1                  IMACSC,IDEFMS,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE MACRO SUBSTITUTION CHARACTOR WHICH MAY
C              BE USED TO REPLACE A COMMAND LINE ARGUMENT TO
C              A MACRO.  FOR EXAMPLE
C
C                  CALL SAMPLE.DP BERGER1.DAT Y X
C
C              IN SAMPLE.DP, $1 WILL BE REPLACED BY BERGER1.DAT,
C              $2 WILL BE REPLACED BY Y, AND $3 WILL BE REPLACED
C              BY X.  THIS ROUTINE LETS YOU SPECIFY A CHARACTER
C              OTHER THAN  "$" TO SIGNIFY A COMMAND LINE ARGUMENT.
C              THE SPECIFIED MACRO SUBSTITUTION CHARACTOR WILL BE
C              PLACED IN THE CHARACTER VARIABLE IMACSC.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IBASLC (A CHARACTER VARIABLE--BACKSLASH)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--IMACSC (A CHARACTER VARIABLE)
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
C     VERSION NUMBER--2005/9
C     ORIGINAL VERSION--SEPTEMBER  2005.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*1 IMACSC
      CHARACTER*1 IDEFMS
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHARG4
      CHARACTER*1 IHOLD
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
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REMA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREMA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NUMARG
   54   FORMAT('NUMARG = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NUMARG
          WRITE(ICOUT,56)I,IHARG(I)
   56     FORMAT('I,IHARG(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
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
      IHOLD=IDEFMS
      GOTO1180
C
 1160 CONTINUE
      IHARG4=IHARG(NUMARG)
      IHOLD=IHARG4(1:1)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IMACSC=IHOLD
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)IMACSC
 1181   FORMAT('THE MACRO SUBSTITUTION CHARACTOR HAS BEEN SET TO ',
     1         A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REMA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREMA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR
 9012   FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IHARG4,IHOLD
 9013   FORMAT('IHARG4,IHOLD = ',A4,2X,A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IMACSC
 9014   FORMAT('IMACSC = ',A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPREMO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IANGLU,DEMOFR,DEMODF,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A COMPLEX REMODULATION PLOT
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
C     ORIGINAL VERSION--MARCH     1986.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
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
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
      ISUBN1='DPRE'
      ISUBN2='MO  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      MAXV2=1
      MINN2=2
C
C               ***********************************************
C               **  TREAT THE COMPLEX REMODULATION CASE      **
C               ***********************************************
C
      IF(IBUGG2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPREMO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52 FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ
   53 FORMAT('IBUGG2,IBUGG3,IBUGQ = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'REMO'.AND.IHARG(1).EQ.'PLOT')GOTO110
C
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'COMP'.AND.IHARG(1).EQ.'REMO'.AND.IHARG(2).EQ.'PLOT')
     1GOTO120
C
      IFOUND='NO'
      GOTO9000
C
  110 CONTINUE
      ICASPL='CR'
      ILASTC=1
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GOTO180
C
  120 CONTINUE
      ICASPL='CR'
      ILASTC=2
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GOTO180
C
  180 CONTINUE
      IFOUND='YES'
      GOTO190
C
  190 CONTINUE
C
C               ***********************************************************
C               **  STEP 1--                                             **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.      **
C               ***********************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ********************************************
C               **  STEP 2--                              **
C               **  CHECK THE VALIDITY OF ARGUMENT 1      **
C               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
C               ********************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
      IF(IBUGG2.EQ.'ON')WRITE(ICOUT,211)IHLEFT,ICOLL,NLEFT
  211 FORMAT('IHLEFT,ICOLL,NLEFT = ',A4,I8,I8)
      IF(IBUGG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
C               ***************************************************************
C               **  STEP 3--                                                 **
C               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)      **
C               **  FOR THE RESPONSE VARIABLE IS POSITIVE.                   **
C               ***************************************************************
C
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NLEFT.GE.MINN2)GOTO390
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,311)
  311 FORMAT('***** ERROR IN DPREMO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,312)
  312 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,321)
  321 FORMAT('      (FOR WHICH A COMPLEX REMODULATION PLOT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,314)
  314 FORMAT('      WAS TO HAVE BEEN FORMED)')
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
      GOTO9000
  390 CONTINUE
C
C               *****************************************
C               **  STEP 4--                           **
C               **  CHECK TO SEE THE TYPE SUBCASE      **
C               **  (BASED ON THE QUALIFIER)--         **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO480
      DO400J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO410
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO410
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO420
  400 CONTINUE
      GOTO490
  410 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO490
  420 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO490
C
  480 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,481)
  481 FORMAT('***** INTERNAL ERROR IN DPREMO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,482)
  482 FORMAT('      AT BRANCH POINT 481--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,483)
  483 FORMAT('      NUMARG LESS THAN 1 EVEN THOUGH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,484)
  484 FORMAT('      NUMARG HAD PREVIOUSLY PASSED THIS TEST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,485)NUMARG
  485 FORMAT('      ONCE ALREADY.  VALUE OF NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,486)
  486 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,487)(IANS(I),I=1,IWIDTH)
  487 FORMAT(80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  490 CONTINUE
      IF(IBUGG2.EQ.'OFF')GOTO495
      WRITE(ICOUT,491)NUMARG,ILOCQ,ICASEQ
  491 FORMAT('NUMARG,ILOCQ,ICASEQ = ',I8,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  495 CONTINUE
C
C               *********************************************
C               **  STEP 5--                               **
C               **  CHECK FOR PROPER NUMBER OF VARIABLES.  **
C               **  FOR A COMPLEX REMODULATION PLOT,       **
C               **  THE PROPER NUMBER OF VARIABLES IS      **
C               **  EXACTLY 1.                             **
C               *********************************************
C
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMV2=ILOCQ-1
      IF(NUMV2.EQ.1)GOTO590
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,551)
  551 FORMAT('***** ERROR IN DPREMO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,552)
  552 FORMAT('      (FOR A COMPLEX REMODULATION PLOT,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,558)
  558 FORMAT('      THE NUMBER OF VARIABLES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,559)
  559 FORMAT('      MUST BE EXACTLY 1  ;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,560)
  560 FORMAT('      SUCH WAS NOT THE CASE HERE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,561)
  561 FORMAT('      THE SPECIFIED NUMBER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,562)NUMV2
  562 FORMAT('      OF VARIABLES WAS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,563)
  563 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,564)(IANS(I),I=1,IWIDTH)
  564 FORMAT(80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  590 CONTINUE
C
C               *************************************************
C               **  STEP 6--                                   **
C               **  BRANCH TO THE APPROPRIATE SUBCASE;         **
C               **  (BASED ON THE QUALIFIER)                   **
C               **  THEN FORM THE RESPONSE VARIABLE            **
C               **  AND THE SECOND VARIABLE (IF EXISTENT)      **
C               *************************************************
C
      ISTEPN='6'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      J=0
      IMAX=NLEFT
      IF(NQ.LT.NLEFT)IMAX=NQ
      DO660I=1,IMAX
      IF(ISUB(I).EQ.0)GOTO660
      J=J+1
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)Y1(J)=V(IJ)
      IF(ICOLL.EQ.MAXCP1)Y1(J)=PRED(I)
      IF(ICOLL.EQ.MAXCP2)Y1(J)=RES(I)
      IF(ICOLL.EQ.MAXCP3)Y1(J)=YPLOT(I)
      IF(ICOLL.EQ.MAXCP4)Y1(J)=XPLOT(I)
      IF(ICOLL.EQ.MAXCP5)Y1(J)=X2PLOT(I)
      IF(ICOLL.EQ.MAXCP6)Y1(J)=TAGPLO(I)
  660 CONTINUE
      NLOCAL=J
C
C               ***********************************************************
C               **  STEP 7--                                             **
C               **  DETERMINE IF THE ANALYST                             **
C               **  HAS SPECIFIED    THE DEMODULATION FREQUENCY          **
C               **  FOR THE COMPLEX DEMODULATION ANALYSIS.               **
C               **  THE FREQUENCY SETTING IS DEFINED BY PRE-USE          **
C               **  OF THE DEMODULATION FREQUENCY     COMMAND.           **
C               **  IF FOUND, USE THE SPECIFIED VALUE.                   **
C               **  IF NOT FOUND, GENERATE AN ERROR MESSAGE.             **
C               ***********************************************************
C
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DEMOF2=DEMOFR
      IF(IANGLU.EQ.'DEGR')DEMOF2=DEMOF2*PI/180.0
      IF(IANGLU.EQ.'GRAD')DEMOF2=DEMOF2*PI/200.0
      IF(0.0.LT.DEMOF2.AND.DEMOF2.LT.0.5)GOTO790
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,741)
  741 FORMAT('***** ERROR IN DPREMO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,742)
  742 FORMAT('       FOR A COMPLEX REMODULATION PLOT,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,744)
  744 FORMAT('       THE FREQUENCY AT WHICH THE DEMODULATION/')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,745)
  745 FORMAT('       REMODULATION IS TO BE PERFORMED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,746)
  746 FORMAT('       MUST BE PRE-SPECIFIED BY THE ANALYST,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,747)
  747 FORMAT('       AND MUST BE BETWEEN 0 AND 0.5 RADIANS;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,748)
  748 FORMAT('       SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,749)DEMOFR,IANGLU
  749 FORMAT('       THE DEMODULATION FREQUENCY = ',E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
      IF(IANGLU.NE.'RADI')WRITE(ICOUT,750)DEMOF2
  750 FORMAT('       THE DEMODULATION FREQUENCY = ',E15.7,2X,
     1'RADIANS')
      IF(IANGLU.NE.'RADI')CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,751)
  751 FORMAT('       TO DEFINE THE DEMODULATION FREQUENCY,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,752)
  752 FORMAT('       THE ANALYST USES THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,753)
  753 FORMAT('       DEMODULATION FREQUENCY     COMMAND, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,754)
  754 FORMAT('            DEMODULATION FREQUENCY 0.3')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,755)
  755 FORMAT('            DEMODULATION FREQUENCY 0.155')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  790 CONTINUE
C
C               ****************************************************************
C               **  STEP 8--                                                   *
C               **  COMPUTE THE APPROPRIATE COMPLEX REMODULATION PLOT.         *
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS                      *
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.                         *
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).              *
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).              *
C               ****************************************************************
C
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPREM2(Y1,NLEFT,ICASPL,DEMOF2,DEMODF,
     1Y,X,D,NPLOTP,NPLOTV,IBUGG3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPREMO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)DEMOFR,IANGLU,DEMOF2
 9014 FORMAT('DEMOFR,IANGLU,DEMOF2 = ',E15.7,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
      IF(NPLOTP.LE.0)GOTO9090
      DO9015I=1,NPLOTP
      WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016 FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPREM2(Y,N,ICASPL,F,DEMODF,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE PERFORMS A COMPLEX REMODULATION
C              ON THE DATA IN THE INPUT VECTOR X
C              AT THE INPUT DEMODULATION FREQUENCY = F.
C              THE COMPLEX REMODULATION CONSISTS OF
C              DEMODULATING AT THE SPECIFIED FREQUENCY
C              AND THEN REMODULATING TO FORM A PLOT
C              IN THE ORIGINAL UNITS OF THE DATA WHICH
C              SHOWS THE CONTRIBUTION AT THAT FREQUENCY
C              TO THE ORIGINAL SERIES.  IT IS USEFUL
C              FOR FORMING A BAND-PASS FILTERED SERIES
C              AND (AFTER SUBTRACTION) A REJECTION-PASS
C              FILTERED SERIES.
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
C     PRINTING--YES.
C     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
C                   FOR THIS SUBROUTINE IS 5000.
C                 --THE SAMPLE SIZE N MUST BE GREATER
C                   THAN OR EQUAL TO 3.
C                 --THE INPUT FREQUENCY F MUST BE
C                   GREATER THAN OR EQUAL TO 2/(N-2).
C                 --THE INPUT FREQUENCY F MUST BE
C                   SMALLER THAN 0.5.
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
C                 ESPECIALLY PAGES 174 AND 175.
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
C     ORIGINAL VERSION--NOVEMBER  1972.
C     UPDATED         --JANUARY   1989.  PARAMETER STATEMENT MISPLACED (ALAN)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA PI/3.141592653/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRE'
      ISUBN2='2  '
      IERROR='NO'
C
      IF(IBUGG3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,N,DEMODF
   52   FORMAT('ICASPL,N,DEMODF = ',A4,2X,I8,G15.7)
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
     1'(A VECTOR) TO THE DPREM2  SUBROUTINE HAS ALL ELEMENTS = ',
     1G15.7)
   17 FORMAT('***** ERROR--THE SECOND ARGUMENT TO THE ',
     1'DPREM2  SUBROUTINE IS OUTSIDE THE ALLOWABLE (',I6,',',I6,') ',
     1'INTERVAL')
   27 FORMAT('***** ERROR--THE THIRD ARGUMENT TO THE ',
     1'DPREM2  SUBROUTINE IS OUTSIDE THE ALLOWABLE (',I6,'0.5) ',
     1'INTERVAL')
   28 FORMAT('                   THE ABOVE LOWER LIMIT (',F11.8,
     1') = 2/(N-2) WHERE N = THE INPUT SAMPLE SIZE = ',I8)
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
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
C               ***********************************************************
C               **  STEP 2--                                             **
C               **  FORM THE FIRST MOVING AVERAGE FOR THE COSINE SERIES  **
C               ***********************************************************
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
  500 CONTINUE
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
  900 CONTINUE
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
 1100 CONTINUE
      SUM=SUM+D2(I)/2.0+D2(IENDP1)/2.0
      X2(I)=SUM/ALEN2
 1000 CONTINUE
C
C               *****************************************
C               **  STEP 7--                           **
C               **  FORM THE REMODULATED SERIES        **
C               *****************************************
C
      IHALF=(LENMA1+LENMA2)/2
      ISTART=IHALF+1
      ISTOP=N-IHALF
C
CCCCC DO1450I=1,IMAX2
      DO1450I=1,N
      IF(I.LT.ISTART)GOTO1410
      IF(I.GT.ISTOP)GOTO1410
      GOTO1420
C
 1410 CONTINUE
      Y2(I)=Y(I)
      X2(I)=I
      D2(I)=1.0
      GOTO1450
C
 1420 CONTINUE
      AI=I
      TERM1=2.0*Y2(I)*SIN(2.0*PI*F*AI)
      TERM2=2.0*X2(I)*COS(2.0*PI*F*AI)
      Y2(I)=TERM1+TERM2
      X2(I)=I
      D2(I)=1.0
      GOTO1450
C
 1450 CONTINUE
CCCCC N2=IMAX2
      N2=N
      NPLOTV=2
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPREM2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)N,ICASPL
 9012 FORMAT('N,ICASPL = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)LENMA1,LENMA2
 9013 FORMAT('LENMA1,LENMA2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IMAX1,IMAX2
 9014 FORMAT('IMAX1,IMAX2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IHALF,ISTART,ISTOP
 9015 FORMAT('IHALF,ISTART,ISTOP = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPREPC(IHARG,IARGT,ARG,NUMARG,DEFRPC,
     1RECIPC,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE RECIPE PROBABILITY CONTENT
C              IN THE FLOATING POINT VARIABLE RECIPC.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFRPC (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--RECIPC  (A  FLOATING POINT VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
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
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'PROB'.AND.IHARG(2).EQ.'CONT')
     1GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONT')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PROB')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'CONT')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPREPC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE PROBABILITY CONTENT ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE PROBABILITY CONTENT .90 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFRPC
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(HOLD.GE.1.0 .AND. HOLD.LT.100.0)HOLD=HOLD/100.
      IF(HOLD.LE.0.0 .OR. HOLD.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182 FORMAT('**** THE RECIPE PROBABILITY CONTENT MUST BE SET BETWEEN',
     1' 0 AND 1 EXCLUSIVE (TYPICALLY BETWEEN .9 AND .99)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)HOLD
 1183 FORMAT('     THE VALUE ENTERED WAS ',E15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO1199
      ENDIF
      RECIPC=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECIPC
 1181 FORMAT('THE RECIPE PROBABILITY CONTENT HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPREPL(ITEXHO,NUMTEC,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1                  IBUGD2,IERROR)
C
C     PURPOSE--TRANSLATE A STRING AS DICTATED BY THE   VALU()   OPERATOR.
C              THAT IS, REPLACE ALL OCCURRANCES OF  XXXVALU()YYY
C              (WHERE XXX IS AN ARBITRARY STRING, AND
C              YYY IS A DATAPLOT PARAMETER NAME OR A FUNCTION NAME)
C              BY THE NUMERIC VALUE OF THE PARAMETER YYY, OR
C              THE FUNCTIONAL STRING IN THE FUNCTION YYY.
C              (E.G., IF PARAMETER K HAS THE VALUE 7,
C              THEN   YVALU()K   BECOMES   Y7
C              OR     IF THE FUNCTION K HAS THE CONTENTS XYZ,
C              THEN   YVALU()K   BECOMES   YABC  ).
C     NOTE--THIS SUBROUTINE CHANGES THE CONTENTS OF THE INPUT VECTOR ITEXHO()
C           AND THE INPUT VARIABLE NUMTEC.
C     NOTE--THIS SUBROUTINE IS SIMILAR TO (BUT NOT IDENTICAL TO) DPREP2.
C           SUBROUTINE DPREPL TRANSLATES THE VALU() OPERATOR.
C           SUBROUTINE DPREPL TRANSLATES THE \      OPERATOR.
C     NOTE--ALTHOUGH IREPCH IS AN INPUT ARGUMENT TO THIS SUBROUTINE,
C           IT IS NEVER USED HEREIN.
C           IT IS ALLOWED TO REMAIN AS AN INPUT ARGUMENT
C           ONLY TO MAINTAIN CONSISTENCY WITH SUBROUTINE DPREP2
C           (WHICH DOES USE IT).
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
C     ORIGINAL VERSION--MARCH      1983.
C     UPDATED         --DECEMBER  1986. STOP WITH "
C     UPDATED         --DECEMBER  1988. STOP WITH )
C     UPDATED         --DECEMBER  1988. STOP WITH & AND COLLAPSE
C     UPDATED         --MAY       1992. ADD 8 DELIMITERS FOR ^
C     UPDATED         --JULY      1992. ADD . AND ^ AS DELIMITERS
C     UPDATED         --DECEMBER  1993. ALLOW LOWER CASE: valu()
C     UPDATED         --DECEMBER  1993. ALLOW LOWER CASE PAR. NAME
C     UPDATED         --JULY      1995. COMMENT OUT 2 LINES
C     UPDATED         --AUGUST    2002. ADD "?" AS DELIMITER
C     UPDATED         --JUNE      2003. TREAT ANYTHING THAT IS NOT A
C                                       NUMBER OR LETTER OR UNDERSCORE
C                                       AS DELIMITER
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ITEXHO
      CHARACTER*4 IBUGD2
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTR
      CHARACTER*4 IH
      CHARACTER*4 IFOUNV
      CHARACTER*4 IFOUNG
      CHARACTER*4 IWORD1
      CHARACTER*4 IWORD2
      CHARACTER*4 IHNAP1
      CHARACTER*4 IHNAP2
      CHARACTER*4 IHNAP3
      CHARACTER*4 IHNAP4
      CHARACTER*1 IH1
      CHARACTER*4 IUS
C
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
C
      CHARACTER*4 IFUNC
C
      CHARACTER*1 IREPCH
C
      CHARACTER*4 IAMPER
C
C---------------------------------------------------------------------
C
      DIMENSION ITEXHO(*)
CCCCC DIMENSION ISTR(20) JAN 1987--PROBLEMS WITH \ AND LONG TITLES
      DIMENSION ISTR(200)
CCCCC DIMENSION IH(20)   JAN 1987--PROBLEMS WITH \ AND LONG TITLES
      DIMENSION IH(200)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      NUMCHN=0
C
      ILOC1=0
      ILOC2=0
      ILOC3=0
      I2=0
      IHNAP1='-999'
      IHNAP2='-999'
      IUS='-999'
C
      IF(IBUGD2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IREPCH,NUMTEC
   53   FORMAT('IREPCH,NUMTEC = ',A4,2X,I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ITEXHO(I),I=1,MIN(100,NUMTEC))
   54   FORMAT('HOLLERITH ITEXHO(1) --',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 10--                                      **
C               **  LOOP THROUGH (AT MOST) 100 PASSES.  EACH PASS  **
C               **  SEARCHES FOR THE NEXT OCCURRANCE OF VALU().  A **
C               **  GIVEN PASS WIPES OUT VALU()XX AND REPLACES IT  **
C               **  WITH THE NUMERIC VALUE OF PARAMETER XX.  NOTE  **
C               **  THAT EACH PASS CHANGES THE CONTENTS OF INOUT   **
C               **  VARIABLE ITEXHO() AND INPUT VALUE NUMTEC.      **
C               *****************************************************
C
      IFOUNG='NO'
      DO1000IPASS=1,100
C
C               ****************************************************
C               **  STEP 11--                                     **
C               **  FOR THIS PASS,                                **
C               **  SEARCH THE STRING FOR THE NEXT OCCURRANCE OF  **
C               **  THE SUBSTRING VALU()                          **
C               **  PROCEED RIGHT TO LEFT (DEC. 1986).            **
C               **  IF FOUND, THEN PROCEED FURTHER.               **
C               **  IF NOT FOUND, THEN EXIT.                      **
C               ****************************************************
C
        IFOUNV='NO'
        IF(NUMTEC.LE.0)GOTO9000
C
        NUMTM5=NUMTEC-5
        IF(NUMTM5.GT.0)THEN
          DO1110IDUMMY=1,NUMTM5
            I=NUMTM5-IDUMMY+1
C
            IP1=I+1
            IP2=I+2
            IP3=I+3
            IP4=I+4
            IP5=I+5
C
            IF(ITEXHO(I).NE.'V'.AND.ITEXHO(I).NE.'v')GOTO1110
            IF(ITEXHO(IP1).NE.'A'.AND.ITEXHO(IP1).NE.'a')GOTO1110
            IF(ITEXHO(IP2).NE.'L'.AND.ITEXHO(IP2).NE.'l')GOTO1110
            IF(ITEXHO(IP3).NE.'U'.AND.ITEXHO(IP3).NE.'u')GOTO1110
            IF(ITEXHO(IP4).NE.'(')GOTO1110
            IF(ITEXHO(IP5).NE.')')GOTO1110
C
            IFOUNV='YES'
            IFOUNG='YES'
            ILOC1=I
            ILOC2=IP5
            GOTO1190
C
 1110     CONTINUE
        ENDIF
        GOTO9000
C
 1190   CONTINUE
C
C               ****************************************************
C               **  STEP 12--                                     **
C               **  EXTRACT THE PARAMETER OR FUNCTION NAME.  THIS **
C               **  WILL BE THE STRING IMMEDIATELY FOLLOWING ()   **
C               **  UNTIL A BLANK IS FOUND                        **
C               **  OR UNTIL A    "   IS FOUND (DEC. 1986)        **
C               **  OR UNTIL A    )   IS FOUND (DEC. 1988)        **
C               **  OR UNTIL A    &   IS FOUND (DEC. 1988)        **
C               ****************************************************
C
        DO1210I=1,8
          ISTR(I)=' '
 1210   CONTINUE
C
        IAMPER='NO'
C
        IMIN=ILOC2+1
        IMAX=IMIN+7
        IF(IMAX.GT.NUMTEC)IMAX=NUMTEC
        J=0
C
C  JUNE 2003.  BASICALLY, A DATAPLOT NAME CONSISTS OF NUMBERS OR
C              ALPABETIC CHARACTERS OR UNDERSCORE.  ANYTHING ELSE
C              SHOULD TERMINATE THE NAME.
C
        DO1250I=IMIN,IMAX
          I2=I
          ITEMP=ICHAR(ITEXHO(I)(1:1))
          IF(ITEMP.LT.48 .OR. ITEMP.GT.122 .OR.
     1       (ITEMP.GT.57 .AND. ITEMP.LT.65) .OR.
     1       (ITEMP.GT.90 .AND. ITEMP.LT.97 .AND. ITEMP.NE.95)
     1       )THEN
             ILOC3=I2-1
             IF(ITEMP.EQ.38)IAMPER='YES'
             GOTO1290
          ENDIF
          J=J+1
          ISTR(J)=ITEXHO(I)
 1250   CONTINUE
        ILOC3=I2
C
 1290   CONTINUE
        NUMCHN=J
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1291)ILOC1,ILOC2,ILOC3,IMIN,IMAX,NUMCHN
 1291     FORMAT('ILOC1,ILOC2,ILOC3,IMIN,IMAX,NUMCHN = ',6I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ****************************************************
C               **  STEP 13--                                     **
C               **  PACK THE PARAMETER/FUNCTION NAME STRING INTO  **
C               **  2 4-BYTE WORDS.                               **
C               ****************************************************
C
        IWORD1='    '
        IWORD2='    '
        NUMASC=4
        IMAX=2*NUMASC
        IF(NUMCHN.LE.0)GOTO1390
        IF(NUMCHN.LT.IMAX)IMAX=NUMCHN
C
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1301)IMAX
 1301     FORMAT('IMAX = ',I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO1300I=1,IMAX
          IF(ISTR(I).EQ.' ')GOTO1390
          J=I
          IF(I.GT.NUMASC)J=I-NUMASC
          ISTAR3=NUMBPC*(J-1)
          ISTAR3=IABS(ISTAR3)
          IF(I.LE.NUMASC)THEN
            CALL DPCHEX(0,NUMBPC,ISTR(I),ISTAR3,NUMBPC,IWORD1)
          ELSE
            CALL DPCHEX(0,NUMBPC,ISTR(I),ISTAR3,NUMBPC,IWORD2)
          ENDIF
 1300   CONTINUE
 1390   CONTINUE
        IHNAP1=IWORD1
        IHNAP2=IWORD2
        IHNAP3=IHNAP1
        IHNAP4=IHNAP2
C
        DO1395I=1,4
          IH1=IHNAP3(I:I)
          INH1=ICHAR(IH1)
          IF(97.LE.INH1.AND.INH1.LE.122)IH1=CHAR(INH1-32)
          IHNAP3(I:I)=IH1
          IH1=IHNAP4(I:I)
          INH1=ICHAR(IH1)
          IF(97.LE.INH1.AND.INH1.LE.122)IH1=CHAR(INH1-32)
          IHNAP4(I:I)=IH1
 1395   CONTINUE
C
C               ****************************************
C               **  STEP 15--                         **
C               **  DETERMINE IF THE NAME IS IN THE   **
C               **  INTERNAL DATAPLOT NAME LIST,      **
C               **  AND AS A PARAMETER OR FUNCTION.   **
C               ****************************************
C
        IF(NUMNAM.GE.1)THEN
          DO1500I=1,NUMNAM
            I2=I
            IF(IHNAP1.EQ.IHNAME(I).AND.IHNAP2.EQ.IHNAM2(I))GOTO1550
            IF(IHNAP3.EQ.IHNAME(I).AND.IHNAP4.EQ.IHNAM2(I))GOTO1550
 1500     CONTINUE
C
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1511)
 1511     FORMAT('***** ERROR IN DPREPL--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1512)
 1512     FORMAT('      THE EXTRACTED NAME FOR TEXT STRING WAS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1513)
 1513     FORMAT('      NOT FOUND IN INTERNAL NAME LIST.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1514)IHNAP1,IHNAP2
 1514     FORMAT('      EXTRACTED NAME = ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
 1550     CONTINUE
          IVAL=IVALUE(I2)
          VAL=VALUE(I2)
          IUS=IUSE(I2)
          IL1=IVSTAR(I2)
          IL2=IVSTOP(I2)
C
          IF(IUS.NE.'P' .AND. IUS.NE.'F')THEN
            WRITE(ICOUT,1561)
 1561       FORMAT('***** ERROR IN DPREPL--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1562)
 1562       FORMAT('      THE EXTRACTED NAME FOR THE TEXT STRING WAS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1563)
 1563       FORMAT('      FOUND IN THE INTERNAL NAME LIST, BUT NOT AS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1564)
 1564       FORMAT('      A PARAMETER, A VARIABLE, OR A FUNCTION.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1566)IHNAP1,IHNAP2
 1566       FORMAT('      EXTRACTED NAME = ',A4,A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1567)IUS
 1567       FORMAT('      USE = ',A4)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
        ENDIF
C
C               ************************************************
C               **  STEP 16--                                 **
C               **  FOR THE CASE WHEN HAVE A PARAMETER NAME,  **
C               **  DETERMINE THE LITERAL STRING ASSOCIATED   **
C               **  WITH THE PARAMETER VALUE.                 **
C               ************************************************
C
        IF(IUS.EQ.'P')THEN
          CALL DPCONH(IVAL,VAL,IH,NH,IBUGD2,IERROR)
        ELSEIF(IUS.EQ.'F')THEN
          CALL DPCOFH(IL1,IL2,IFUNC,NUMCHF,IH,NH,IBUGD2,IERROR)
        ENDIF
C
C               ****************************************************
C               **  STEP 21--                                     **
C               **  COLLAPSE THE SUBSTRING VALU() FOLLOWED BY     **
C               **  THE PARAMETER/FUNCTION NAME TO A NULL STRING. **
C               ****************************************************
C
        ILOC1M=ILOC1-1
        ILOC3P=ILOC3+1
        J=ILOC1M
        IF(NUMTEC.GE.ILOC3P)THEN
          DO2100I=ILOC3P,NUMTEC
            J=J+1
            ITEXHO(J)=ITEXHO(I)
 2100     CONTINUE
        ENDIF
        NUMTE2=J
C
C               ****************************************************
C               **  STEP 22--                                     **
C               **  INSERT THE LITERAL VALUE                      **
C               **  AT THE PROPER PLACE IN THE COLLAPSED STRING.  **
C               ****************************************************
C
        J=ILOC1M
        IF(NUMTE2.GE.ILOC1)THEN
          DO2200I=ILOC1,NUMTE2
            IREV=NUMTE2-I+ILOC1
            IREVNH=IREV+NH
            ITEXHO(IREVNH)=ITEXHO(IREV)
 2200     CONTINUE
        ENDIF
C
        IF(NH.GE.1)THEN
          DO2300I=1,NH
            J=ILOC1M+I
            ITEXHO(J)=IH(I)
 2300     CONTINUE
        ENDIF
        NUMTE3=NUMTE2+NH
        NUMTEC=NUMTE3
C
C               ****************************************************
C               **  STEP 24--                                     **
C               **  IF THE TERMINATOR WAS &,                      **
C               **  THEN COLLAPSE & TO A NULL STRING.             **
C               **  (THUS & SERVES AS A USEFUL CONCATONATION      **
C               **  CHARACTER.                                    **
C               **  (DECEMBER 1988)                               **
C               ****************************************************
C
        IF(IAMPER.EQ.'YES')THEN
          ILOC4=ILOC1+NH
          ILOC4M=ILOC4-1
          ILOC4P=ILOC4+1
          J=ILOC4M
          IF(NUMTEC.GE.ILOC4P)THEN
            DO2420I=ILOC4P,NUMTEC
              J=J+1
              ITEXHO(J)=ITEXHO(I)
 2420     CONTINUE
            NUMTEC=J
          ENDIF
        ENDIF
C
 1000 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
C
      IF(IBUGD2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMTEC
 9013   FORMAT('NUMTEC = ',I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ITEXHO(I),I=1,MIN(100,NUMTEC))
 9014   FORMAT('HOLLERITH ITEXHO(1) --',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ILOC1,ILOC2,ILOC3,NUMTEC,NUMTM5
 9015   FORMAT('ILOC1,ILOC2,ILOC3,NUMTEC,NUMTM5 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)NUMCHN
 9016   FORMAT('NUMCHN = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)(ISTR(I),I=1,MIN(80,NUMCHN))
 9017   FORMAT('(ISTR(I),I=1,NUMCHN) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IWORD1,IWORD2,IHNAP1,IHNAP2
 9018   FORMAT('IWORD1,IWORD2,IHNAP1,IHNAP2 = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IAMPER
 9022   FORMAT('IAMPER = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ILOC4M,ILOC4,ILOC4P,NUMTEC
 9023   FORMAT('ILOC4M,ILOC4,ILOC4P,NUMTEC = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPREP2(ITEXHO,NUMTEC,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVARLB,IROWLB,MAXNXT,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,
     1                  IBUGD2,ISUBRO,IERROR)
C
C     PURPOSE--TRANSLATE A STRING AS DICTATED BY THE   ^   OPERATOR.
C              THAT IS, REPLACE ALL OCCURRANCES OF  XXX^YYY
C              (WHERE XXX IS AN ARBITRARY STRING, AND
C              YYY IS A DATAPLOT PARAMETER NAME OR A FUNCTION NAME)
C              BY THE NUMERIC VALUE OF THE PARAMETER YYY, OR
C              THE FUNCTIONAL STRING IN THE FUNCTION YYY.
C              (E.G., IF PARAMETER K HAS THE VALUE 7,
C              THEN   Y^K   BECOMES   Y7
C              OR     IF THE FUNCTION K HAS THE CONTENTS XYZ,
C              THEN   Y^K   BECOMES   YABC  ).
C     NOTE--THIS SUBROUTINE CHANGES THE CONTENTS OF THE INPUT VECTOR ITEXHO()
C           AND THE INPUT VARIABLE NUMTEC.
C     NOTE--THIS SUBROUTINE IS SIMILAR TO (BUT NOT IDENTICAL TO) DPREPL.
C           SUBROUTINE DPREP2 TRANSLATES THE ^      OPERATOR.
C           SUBROUTINE DPREPL TRANSLATES THE VALU() OPERATOR.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/1
C     ORIGINAL VERSION--DECEMBER  1986.
C     UPDATED         --DECEMBER  1986. STOP WITH "
C     UPDATED         --JUNE      1987.
C     UPDATED         --DECEMBER  1988. STOP WITH )
C     UPDATED         --DECEMBER  1988. STOP WITH & AND COLLAPSE
C     UPDATED         --OCTOBER   1991. SIMPLIFY A SECTION (ALAN)
C     UPDATED         --APRIL     1992. FIX ^ LOWER CASE CONVERSION
C     UPDATED         --APRIL     1992. FIX DEBUG STATMENT
C     UPDATED         --MAY       1992. ADD 8 DELIMITERS FOR ^
C     UPDATED         --JULY      1992. ADD . AND ^ AS DELIMITERS
C     UPDATED         --AUGUST    1992. NON-EXISTENT K: ^K ==> BLANK
C     UPDATED         --OCTOBER   1993. TOP WITH (
C     UPDATED         --JANUARY   2000. REPLACE VARIABLE NAME WITH
C                                       VARIABLE LABEL.
C     UPDATED         --AUGUST    2002. ADD "?" AS DELIMITER
C     UPDATED         --JUNE      2003. TREAT ANYTHING THAT IS NOT A
C                                       NUMBER OR LETTER AS DELIMITER
C     UPDATED         --FEBRUARY  2005. CASE OF "&"
C     UPDATED         --SEPTEMBER 2005. SUPPORT ARGUMENTS TO MACROS
C                                       ($1, $2, ETC.)
C     UPDATED         --SEPTEMBER 2007. SUPPORT ^ROWLABEL^K SYNTAX
C                                       (I.E., EXTRACT A ROW LABEL)
C     UPDATED         --OCTOBER   2016. SUPPORT NAMED ARGUMENTS
C                                       ($Y, $TITLE, ETC.)
C     UPDATED         --OCTOBER   2016. INCREASE NUMBER OF ORDERED
C                                       ARGUMENTS FROM 10 TO 20
C     UPDATED         --MAY       2018. $99 TO RETRIEVE THE FULL CALL
C                                       COMMAND CONTAINING THE ARGUMENTS
C     UPDATED         --JANUARY   2019. OPTION TO SUPPRESS COMMAND
C                                       (^) SUBSTITUTION
C     UPDATED         --JANUARY   2019. OPTION TO SPECIFY FORTRAN
C                                       FORMAT FOR SUBSTITUTION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ITEXHO
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ISTR
      CHARACTER*4 IH
      CHARACTER*4 IFOUNV
      CHARACTER*4 IFOUNG
      CHARACTER*4 IWORD1
      CHARACTER*4 IWORD2
      CHARACTER*4 IHNAP1
      CHARACTER*4 IHNAP2
      CHARACTER*4 IUS
C
      CHARACTER*4 IROWFL
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
C
      CHARACTER*40 IVARLB(*)
      CHARACTER*40 ILABT
CCCCC CHARACTER*255 ITTEMP
      CHARACTER (LEN=MAXSTR) :: ITTEMP
      CHARACTER*4  IFUNC
      CHARACTER*24 IROWLB(*)
C
      CHARACTER*1 IREPCH
      CHARACTER*4 IJUNK1
      CHARACTER*4 IJUNK2
      CHARACTER*1 IC1
      CHARACTER*1 IC2
      CHARACTER*2 IAJUNK
      CHARACTER*4 IAMPER
      CHARACTER*10 IFORMT
      CHARACTER*20 IVALXX
C
C-------------------------------------------------------------------
C
      DIMENSION ITEXHO(*)
      DIMENSION ISTR(200)
      DIMENSION IH(200)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
C
C-----COMMON VARIABLES (GENERAL)------------------------------------
C
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT---------------------------------------------------
C
      IERROR='NO'
      NUMCHN=0
C
      ILOC1=0
      ILOC2=0
      ILOC3=0
      I2=0
      IHNAP1='-999'
      IHNAP2='-999'
      IUS='-999'
      ILAST=0
      II=0
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IREPCH,IMACSC
   53   FORMAT('IREPCH,IMACSC = ',A1,1X,A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ITEXHO(I),I=1,MIN(100,NUMTEC))
   54   FORMAT('HOLLERITH ITEXHO(1) --',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NUMTEC,IMALEV,NMACAG,NMACLA,ITABNC,MAXNXT
   55   FORMAT('NUMTEC,IMALEV,NMACAG,NMACLA,ITABNC,MAXNXT = ',
     1         5I5,I10)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHF.GT.0)THEN
          DO58I=1,NUMCHF
            WRITE(ICOUT,59)I,IFUNC(I)
   59       FORMAT('I,IFUNC(I) = ',I8,A1)
            CALL DPWRST('XXX','BUG ')
   58     CONTINUE
        ENDIF
        IF(NMACLA.GT.0)THEN
          DO61I=1,NMACLA
            WRITE(ICOUT,63)I,IMACLL(I),IMACNC(I),IMACLA(I)
   63       FORMAT('I,IMACLL(I),IMACNC(I),IMACLA(I) = ',3I8,2X,A80)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
      ENDIF
C
C               ****************************************************
C               **  STEP 10--                                     **
C               **  LOOP THROUGH (AT MOST) 100 PASSES.  EACH PASS **
C               **  SEARCHES FOR THE NEXT OCCURRANCE OF ^.  A     **
C               **  GIVEN PASS WIPES OUT ^XX AND REPLACES IT WITH **
C               **  THE NUMERIC VALUE OF PARAMETER XX OR THE      **
C               **  STRING CONTENTS VALUE OF FUNCTION XX.  NOTE   **
C               **  THAT EACH PASS CHANGES THE CONTENTS OF INPUT  **
C               **  VARIABLE ITEXHO() AND INPUT VALUE NUMTEC.     **
C               ****************************************************
C
C       2019/01: OPTION TO SUPPRESS THIS STEP
C
      IFOUNG='NO'
      IF(ISUBSK.EQ.'OFF')GOTO1009
      DO1000IPASS=1,100
C
C               ****************************************************
C               **  STEP 11--                                     **
C               **  FOR THIS PASS,                                **
C               **  SEARCH THE STRING FOR THE NEXT OCCURRANCE OF  **
C               **  THE SUBSTRING IN IREPCH (USUALLY ^ )          **
C               **  PROCEED RIGHT TO LEFT (DEC. 1986).            **
C               **  IF FOUND, THEN PROCEED FURTHER.               **
C               **  IF NOT FOUND, THEN EXIT.                      **
C               ****************************************************
C
        IFOUNV='NO'
C
        IF(NUMTEC.GT.0)THEN
          DO1120IDUMMY=1,NUMTEC
            I=NUMTEC-IDUMMY+1
            IF(ITEXHO(I).EQ.IREPCH)THEN
              IFOUNV='YES'
              IFOUNG='YES'
              ILOC1=I
              ILOC2=I
              GOTO1190
            ENDIF
 1120     CONTINUE
          GOTO2500
        ELSE
          GOTO9000
        ENDIF
C
 1190   CONTINUE
C
C               *****************************************************
C               **  STEP 12--                                      **
C               **  EXTRACT THE PARAMETER OR FUNCTION NAME.        **
C               **  THIS WILL BE THE STRING IMMEDIATELY FOLLOWING  **
C               **  ^ UNTIL A BLANK IS FOUND                       **
C               **  OR UNTIL A    "   IS FOUND (DEC. 1986)         **
C               **  OR UNTIL A    )   IS FOUND (DEC. 1988)         **
C               **  OR UNTIL A    &   IS FOUND (DEC. 1988)         **
C               *****************************************************
C
C       SEPTEMBER 2007.  CHECK FOR ^ROWLABEL^K TYPE SYNTAX.  THIS
C                        WILL SUBSTITUTE THE K-TH ROW LABEL.
C
        DO1210I=1,8
          ISTR(I)=' '
 1210   CONTINUE
C
        IAMPER='NO'
C
        IMIN=ILOC2+1
        IMAX=IMIN+7
        IF(IMAX.GT.NUMTEC)IMAX=NUMTEC
        J=0
C
C  JUNE 2003.  BASICALLY, A DATAPLOT NAME CONSISTS OF NUMBERS OR
C              ALPABETIC CHARACTERS OR UDERSCORES.  ANYTHING ELSE
C              SHOULD TERMINATE THE NAME.
C
        DO1250I=IMIN,IMAX
          I2=I
          ITEMP=ICHAR(ITEXHO(I)(1:1))
C
          IF(IBUGD2.EQ.'ON')THEN
            WRITE(ICOUT,1251)I,IPASS,ITEXHO(I)
 1251       FORMAT('I,IPASS,ITEXHO(I) = ',2I8,1X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(ITEMP.LT.48 .OR. ITEMP.GT.122 .OR.
     1       (ITEMP.GT.57 .AND. ITEMP.LT.65) .OR.
     1       (ITEMP.GT.90 .AND. ITEMP.LT.97 .AND. ITEMP.NE.95)
     1       )THEN
             ILOC3=I2-1
             IF(ITEMP.EQ.38)IAMPER='YES'
             GOTO1290
          ENDIF
          J=J+1
          ISTR(J)=ITEXHO(I)
 1250   CONTINUE
        ILOC3=I2
C
 1290   CONTINUE
        NUMCHN=J
        IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
          WRITE(ICOUT,1291)ILOC1,ILOC2,ILOC3
 1291     FORMAT('ILOC1,ILOC2,ILOC3 = ',3I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1292)IMIN,IMAX,NUMCHN
 1292     FORMAT('IMIN,IMAX,NUMCHN = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ****************************************************
C               **  STEP 13--                                     **
C               **  PACK THE PARAMETER/FUNCTION NAME STRING INTO  **
C               **  2 4-BYTE WORDS.                               **
C               ****************************************************
C
        IWORD1='    '
        IWORD2='    '
        DO1310I=1,4
          IWORD1(I:I)=ISTR(I)(1:1)
          IWORD2(I:I)=ISTR(I+4)(1:1)
1310    CONTINUE
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1302)IWORD1,IWORD2
1302      FORMAT('IWORD1,IWORD2=',A4,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IHNAP1=IWORD1
        IHNAP2=IWORD2
C
C               ****************************************************
C               **  STEP 14--                                     **
C               **  CONVERT THE 2 4-BYTE WORDS INTO UPPER CASE.   **
C               **  (JUNE 1987)                                   **
C               ****************************************************
C
        IJUNK1=IHNAP1
        IJUNK2=IHNAP2
        CALL DPUPP4(IJUNK1,IJUNK1,IBUGD2,IERROR)
        CALL DPUPP4(IJUNK2,IJUNK2,IBUGD2,IERROR)
        IHNAP1=IJUNK1
        IHNAP2=IJUNK2
C
C               ****************************************
C               **  STEP 15--                         **
C               **  DETERMINE IF THE NAME IS IN THE   **
C               **  INTERNAL DATAPLOT NAME LIST,      **
C               **  AND AS A PARAMETER OR FUNCTION.   **
C               ****************************************
C
        IF(NUMNAM.GT.0)THEN
          DO1500I=1,NUMNAM
            I2=I
            IF(IHNAP1.EQ.IHNAME(I).AND.IHNAP2.EQ.IHNAM2(I))GOTO1550
 1500     CONTINUE
C
          NH=1
          IH(1)='    '
          GOTO2100
C
 1550     CONTINUE
          IVAL=IVALUE(I2)
          VAL=VALUE(I2)
          IUS=IUSE(I2)
          IL1=IVSTAR(I2)
          IL2=IVSTOP(I2)
C
          IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
            WRITE(ICOUT,1551)IPASS,IL1,IL2
 1551       FORMAT('IPASS,IL1,IL2 = ',3I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1553)IVAL,VAL
 1553       FORMAT('IVAL,VAL = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
            IF(IUS.EQ.'F')THEN
              ITEMP=IL2-IL1+1
              IF(ITEMP.GT.100)THEN
                ITEMP=IL1+99
              ELSE
                ITEMP=IL2
              ENDIF
              WRITE(ICOUT,1555)(IFUNC(KKK),KKK=IL1,ITEMP)
 1555         FORMAT('IFUNC(IL1:IL2) = ',100A1)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
C
          ILABT=' '
          IF(IVAL.GT.0.AND.IUS.EQ.'V')ILABT(1:40)=IVARLB(IVAL)(1:40)
C
          IF(IUS.NE.'P' .AND. IUS.NE.'F' .AND. IUS.NE.'V')THEN
            WRITE(ICOUT,1561)
 1561       FORMAT('***** ERROR IN DPREP2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1562)
 1562       FORMAT('      THE EXTRACTED NAME FOR THE TEXT STRING')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1563)
 1563       FORMAT('      WAS FOUND IN INTERNAL NAME LIST, BUT NOT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1564)
 1564       FORMAT('      AS A PARAMETER, A VARIABLE, OR A FUNCTION.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1566)IHNAP1,IHNAP2
 1566       FORMAT('      EXTRACTED NAME = ',A4,A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1567)IUS
 1567       FORMAT('      USE = ',A4)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
        ENDIF
C
C               ************************************************
C               **  STEP 16--                                 **
C               **  FOR THE CASE WHEN HAVE A PARAMETER NAME,  **
C               **  DETERMINE THE LITERAL STRING ASSOCIATED   **
C               **  WITH THE PARAMETER VALUE.                 **
C               ************************************************
C
CCCCCC  SEPTEMBER 2007: CHECK FOR ROWLABEL:
C
        IF(IUS.EQ.'P')THEN
          IROWFL='OFF'
          IF(ILOC1.GE.9 .AND. IVAL.GT.0 .AND. IVAL.LE.MAXNXT)THEN
            IF((ITEXHO(ILOC1-8)(1:1).EQ.'R' .OR.
     1          ITEXHO(ILOC1-8)(1:1).EQ.'r') .AND.
     1         (ITEXHO(ILOC1-7)(1:1).EQ.'O' .OR. 
     1          ITEXHO(ILOC1-7)(1:1).EQ.'o') .AND.
     1         (ITEXHO(ILOC1-6)(1:1).EQ.'W' .OR.
     1          ITEXHO(ILOC1-6)(1:1).EQ.'w') .AND.
     1         (ITEXHO(ILOC1-5)(1:1).EQ.'L' .OR.
     1          ITEXHO(ILOC1-5)(1:1).EQ.'l') .AND.
     1         (ITEXHO(ILOC1-4)(1:1).EQ.'A' .OR.
     1          ITEXHO(ILOC1-4)(1:1).EQ.'a') .AND.
     1         (ITEXHO(ILOC1-3)(1:1).EQ.'B' .OR.
     1          ITEXHO(ILOC1-3)(1:1).EQ.'b') .AND.
     1         (ITEXHO(ILOC1-2)(1:1).EQ.'E' .OR.
     1          ITEXHO(ILOC1-2)(1:1).EQ.'e') .AND.
     1         (ITEXHO(ILOC1-1)(1:1).EQ.'L' .OR.
     1          ITEXHO(ILOC1-1)(1:1).EQ.'l'))THEN
              IROWFL='YES'
              ILOC1=ILOC1-9
            ENDIF
          ENDIF
          IF(IROWFL.EQ.'ON')THEN
            ILABT=' '
            ILABT(1:24)=IROWLB(IVAL)(1:24)
            NH=24
            DO1710I=24,1,-1
              IF(ILABT(I:I).NE.' ')THEN
                NH=I
                GOTO1719
              ENDIF
 1710       CONTINUE
            NH=0
 1719       CONTINUE
            IF(NH.LE.0)THEN
              IH(1)='R'
              IH(2)='O'
              IH(3)='W'
              IH(4)=' '
              CALL DPCONH(IVAL,VAL,IH(5),NHTEMP,IBUGD2,IERROR)
              NH=NHTEMP+4
            ELSE
              DO1730I=1,NH
                IH(I)=' '
                IH(I)(1:1)=ILABT(I:I)
 1730         CONTINUE
            ENDIF
          ELSE
C
C           2019/01: USE FORTRAN FORMAT STATEMENT FOR THE
C                    PARAMETER SUBSTITUTION
C
            IF(ISUBFM.NE.'NULL')THEN
              IFORMT=' '
              IFORMT(1:8)=ISUBFM(1:8)
              IF(IFORMT(1:1).NE.'(')THEN
                IFORMT(2:9)=IFORMT(1:8)
                IFORMT(1:1)='('
              ENDIF
              DO1722II=10,2,-1
                IF(IFORMT(II:II).NE.' ')THEN
                  ILAST=II
                  GOTO1723
                ENDIF
 1722         CONTINUE
 1723         CONTINUE
              IF(IFORMT(ILAST:ILAST).NE.')')THEN
                IFORMT(ILAST+1:ILAST+1)=')'
              ENDIF
              IVALXX=' '
              WRITE(IVALXX,IFORMT,ERR=1736)VAL
              IFRST=1
              ILAST=20
              DO1731II=1,20
                IF(IVALXX(II:II).NE.' ')THEN
                  IFRST=II
                  GOTO1732
                ENDIF
 1731         CONTINUE
 1732         CONTINUE
              ILAST=20
              DO1733II=20,1,-1
                IF(IVALXX(II:II).NE.' ')THEN
                  ILAST=II
                  GOTO1734
                ENDIF
 1733         CONTINUE
 1734         CONTINUE
              IF(ILAST.LT.IFRST)GOTO1736
              NCH=ILAST-IFRST+1
              IVALXX(1:NCH)=IVALXX(IFRST:ILAST)
              IF(NCH.LT.20)IVALXX(NCH+1:20)=' '
              NH=0
              DO1735II=1,NCH
                NH=NH+1
                IH(NH)=' '
                IH(NH)(1:1)=IVALXX(II:II)
 1735         CONTINUE
              GOTO1739
 1736         CONTINUE
              CALL DPCONH(IVAL,VAL,IH,NH,IBUGD2,IERROR)
 1739         CONTINUE
            ELSE
              CALL DPCONH(IVAL,VAL,IH,NH,IBUGD2,IERROR)
            ENDIF
          ENDIF
        ELSEIF(IUS.EQ.'F')THEN
          CALL DPCOFH(IL1,IL2,IFUNC,NUMCHF,IH,NH,IBUGD2,IERROR)
        ELSEIF(IUS.EQ.'V')THEN
          NH=52
          DO1610I=52,1,-1
            IF(ILABT(I:I).NE.' ')THEN
              NH=I
              GOTO1619
            ENDIF
 1610     CONTINUE
          NH=0
 1619     CONTINUE
          IF(NH.EQ.0)THEN
            DO1620I=1,4
              IH(I)=' '
              IH(I+4)=' '
              IH(I)(1:1)=IHNAP1(I:I)
              IH(I+4)(1:1)=IHNAP2(I:I)
 1620       CONTINUE
            NH=8
            DO1625I=8,1,-1
              IF(IH(I).NE.' ')THEN
                NH=I
                GOTO1629
              ENDIF
 1625       CONTINUE
 1629       CONTINUE
          ELSE
            DO1630I=1,NH
              IH(I)=' '
              IH(I)(1:1)=ILABT(I:I)
 1630       CONTINUE
          ENDIF
        ENDIF
C
C               ****************************************************
C               **  STEP 21--                                     **
C               **  COLLAPSE THE SUBSTRING ^ FOLLOWED BY          **
C               **  THE PARAMETER/FUNCTION NAME TO A NULL STRING. **
C               ****************************************************
C
 2100   CONTINUE
        ILOC1M=ILOC1-1
        ILOC3P=ILOC3+1
        J=ILOC1M
        IF(NUMTEC.GE.ILOC3P)THEN
          DO2110I=ILOC3P,NUMTEC
            J=J+1
            ITEXHO(J)=ITEXHO(I)
 2110     CONTINUE
        ENDIF
        NUMTE2=J
C
C               ****************************************************
C               **  STEP 22--                                     **
C               **  INSERT THE LITERAL VALUE                      **
C               **  AT THE PROPER PLACE IN THE COLLAPSED STRING.  **
C               ****************************************************
C
        J=ILOC1M
        IF(NUMTE2.GE.ILOC1)THEN
          DO2200I=ILOC1,NUMTE2
            IREV=NUMTE2-I+ILOC1
            IREVNH=IREV+NH
            ITEXHO(IREVNH)=ITEXHO(IREV)
 2200     CONTINUE
        ENDIF
C
        IF(NH.GE.1)THEN
          DO2300I=1,NH
            J=ILOC1M+I
            ITEXHO(J)=IH(I)
 2300     CONTINUE
        ENDIF
        NUMTE3=NUMTE2+NH
        NUMTEC=NUMTE3
C
C               ****************************************************
C               **  STEP 24--                                     **
C               **  IF THE TERMINATOR WAS &,                      **
C               **  THEN COLLAPSE & TO A NULL STRING.             **
C               **  (THUS & SERVES AS A USEFUL CONCATONATION      **
C               **  CHARACTER.                                    **
C               **  (DECEMBER 1988)                               **
C               ****************************************************
C
        IF(IAMPER.EQ.'YES')THEN
          ILOC4=ILOC1+NH
          ILOC4M=ILOC4-1
          ILOC4P=ILOC4+1
          J=ILOC4M
          IF(NUMTEC.GE.ILOC4P)THEN
            DO2420I=ILOC4P,NUMTEC
              J=J+1
              ITEXHO(J)=ITEXHO(I)
 2420       CONTINUE
            NUMTEC=J
          ENDIF
        ENDIF
C
 1000 CONTINUE
 1009 CONTINUE
C
C
C               ****************************************************
C               **  STEP 25--                                     **
C               **  NOW CHECK FOR ANY MACRO SUBSTITUTION          **
C               **  CHARACTERS.  THESE ARE IDENTIFIED BY A        **
C               **  $1, $2, ..., $10.  NOTE THAT $0 IS USED TO    **
C               **  DENOTE THE NUMBER OF MACRO ARGUMENTS.         **
C               **  (SEPTEMBER  2005)                             **
C               ****************************************************
C
C     2018/05: $99 TO RETRIEVE FULL CALL COMMAND
C     2018/05: DO SUBSTITUTION FOR $0 AND $00 EVEN IF
C              IMALEV = 0
C
 2500 CONTINUE
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
        WRITE(ICOUT,22700)NUMTEC,IMALEV
22700   FORMAT('AT 2500: NUMTEC,IMALEV = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC IF(NUMTEC.GT.0 .AND. IMALEV.GE.1)THEN
      IF(NUMTEC.GT.0)THEN
        MAXNCH=80
        ITTEMP=' '
        NCH=0
        DO2510I=1,NUMTEC-1
          IF(ITEXHO(I).EQ.IMACSC)THEN
            IP1=I+1
            IP2=I+2
            IP3=I+3
            IF(ITEXHO(IP1).EQ.'0' .AND. ITEXHO(IP2).EQ.'0')THEN
              ITTEMP=' '
              WRITE(ITTEMP(1:2),'(I2)')NMACLA
              NCH=2
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'0')THEN
              ITTEMP=' '
              WRITE(ITTEMP(1:2),'(I2)')NMACAG
              NCH=2
              ILOC1=I
              ILOC2=IP1+1
              GOTO2519
            ELSEIF(IMALEV.LT.1)THEN
              GOTO9000
            ELSEIF(ITEXHO(IP1).EQ.'9' .AND. ITEXHO(IP2).EQ.'9')THEN
              ITTEMP=' '
              NCH=0
              IF(NMACCL.GT.0)THEN
                ITTEMP(1:NMACCL)=IMACCL(1:NMACCL)
                NCH=NMACCL
              ENDIF
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF((ITEXHO(IP1).EQ.'1' .OR. ITEXHO(IP1).EQ.'2' .OR.
     1              ITEXHO(IP1).EQ.'3' .OR. ITEXHO(IP1).EQ.'4' .OR.
     1              ITEXHO(IP1).EQ.'4' .OR. ITEXHO(IP1).EQ.'5' .OR.
     1              ITEXHO(IP1).EQ.'6' .OR. ITEXHO(IP1).EQ.'7' .OR.
     1              ITEXHO(IP1).EQ.'8' .OR. ITEXHO(IP1).EQ.'9') .AND.
     1             (ITEXHO(IP2).EQ.'1' .OR. ITEXHO(IP2).EQ.'2' .OR.
     1              ITEXHO(IP2).EQ.'3' .OR. ITEXHO(IP2).EQ.'4' .OR.
     1              ITEXHO(IP2).EQ.'4' .OR. ITEXHO(IP2).EQ.'5' .OR.
     1              ITEXHO(IP2).EQ.'6' .OR. ITEXHO(IP2).EQ.'7' .OR.
     1              ITEXHO(IP2).EQ.'8' .OR. ITEXHO(IP2).EQ.'9')
     1            )THEN
              IAJUNK(1:1)=ITEXHO(IP1)(1:1)
              IAJUNK(2:2)=ITEXHO(IP2)(1:1)
              READ(ITEXHO(IP1)(1:2),'(I2)')IITEMP
              IF(IITEMP.LE.0 .OR. IITEMP.GT.50)THEN
                WRITE(ICOUT,1561)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,3690)
 3690           FORMAT('      SPECIFIED POSITIONAL COMMAND LINE ',
     1                 'ARGUMENT IS LESS THAN 1 OR GREATER THAN 50.')
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,3693)IITEMP
 3693           FORMAT('      THE SPECIFIED ARGUMENT IS ',I8)
                CALL DPWRST('XXX','BUG ')
                IERROR='YES'
                GOTO9000
              ENDIF
              MAXNCH=80
              NCH=0
              DO3630II=MAXNCH,1,-1
                IF(IMACAR(IITEMP)(II:II).NE.' ')THEN
                  NCH=II
                  ITTEMP(1:NCH)=IMACAR(IITEMP)(1:NCH)
                  GOTO3639
                ENDIF
 3630         CONTINUE
 3639         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .OR. ITEXHO(IP1).EQ.'2' .OR.
     1             ITEXHO(IP1).EQ.'3' .OR. ITEXHO(IP1).EQ.'4' .OR.
     1             ITEXHO(IP1).EQ.'4' .OR. ITEXHO(IP1).EQ.'5' .OR.
     1             ITEXHO(IP1).EQ.'6' .OR. ITEXHO(IP1).EQ.'7' .OR.
     1             ITEXHO(IP1).EQ.'8' .OR. ITEXHO(IP1).EQ.'9')THEN
              READ(ITEXHO(IP1)(1:1),'(I1)')IITEMP
              MAXNCH=80
              NCH=0
              DO2630II=MAXNCH,1,-1
                IF(IMACAR(IITEMP)(II:II).NE.' ')THEN
                  NCH=II
                  ITTEMP(1:NCH)=IMACAR(IITEMP)(1:NCH)
                  GOTO2639
                ENDIF
 2630         CONTINUE
 2639         CONTINUE
              ILOC1=I
              ILOC2=IP1+1
              GOTO2519
            ELSE
C
C             CHECK FOR NAMED ARGUMENTS
C
              IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                WRITE(ICOUT,22701)NMACLA
22701           FORMAT('AT CHECK FOR NAMED ARGUMENTS: NMACLA = ',I5)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
              IF(NMACLA.GE.1)THEN
C
C               2018/07: NEED TO CHECK THE NUMBER OF CHARACTERS IN
C                        ITEXHO TO BE INCLUDED IN THE MATCH.  THE
C                        LABEL NAME SHOULD ONLY CONTAIN NUMBERS,
C                        LOWER OR UPPER CASE ALPHABETIC, OR UNDERSCORE
C                        CHARACTER.  ANY OTHER CHARACTER TERMINATES
C                        CURRENT NAME.
C
                NMAXCH=0
                DO2640JJ=IP1+1,NUMTEC
                  NMAXCH=NMAXCH+1
                  IC1=ITEXHO(IP1+KK-1)(1:1)
                  IF(IC1.EQ.'_')GOTO2640
                  CALL DPCOAN(IC1,IVAL1)
                  IF(IVAL1.GE.48 .AND. IVAL1.LE.57)GOTO2640
                  IF(IVAL1.GE.97 .AND. IVAL1.LE.122)GOTO2640
                  IF(IVAL1.GE.65 .AND. IVAL1.LE.90)GOTO2640
                  NMAXCH=NMAXCH-1
                  GOTO2641
 2640           CONTINUE
 2641           CONTINUE
C
                DO2642JJ=1,NMACLA
                  IFLAG=1
C
                  IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                    WRITE(ICOUT,22703)JJ,IMACNC(JJ)
22703               FORMAT('JJ,IMACNC(JJ) = ',3I5)
                    CALL DPWRST('XXX','BUG ')
                  ENDIF
C
CCCCC             DO2643KK=1,IMACNC(JJ)
                  ILAST=MAX(IMACNC(JJ),NMAXCH)
                  DO2643KK=1,ILAST
C
C                   MAKE NAME SEARCH CASE INSENSITIVE
C
                    IC1=ITEXHO(IP1+KK-1)(1:1)
                    CALL DPCOAN(IC1,IVAL1)
                    IF(IVAL1.GE.97 .AND. IVAL1.LE.122)IVAL1=IVAL1-32
                    CALL DPCONA(IVAL1,IC1)
                    IC2=IMACLA(JJ)(KK:KK)
                    CALL DPCOAN(IC2,IVAL2)
                    IF(IVAL2.GE.97 .AND. IVAL2.LE.122)IVAL2=IVAL2-32
                    CALL DPCONA(IVAL2,IC2)
C
                    IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                      WRITE(ICOUT,22731)JJ,KK,IC1,IC2
22731                 FORMAT('AT 2642: JJ,KK,IC1,IC2 = ',2I5,2(A1,1X))
                      CALL DPWRST('XXX','BUG ')
                    ENDIF
C
                    IF(IC1.NE.IC2)THEN
                      IFLAG=0
                      GOTO2642
                    ENDIF
 2643             CONTINUE
                  IF(IFLAG.EQ.1)THEN
                    IROWXX=IMACLL(JJ)
                    NCHTMP=IMACNC(JJ)
C
                    IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                      WRITE(ICOUT,22711)JJ,IROWXX,NCHTMP
22711                 FORMAT('AT 2643: JJ,IROWXX,NCHTMP=',3I8)
                      CALL DPWRST('XXX','BUG ')
                    ENDIF
C
                    GOTO2645
                  ENDIF
 2642           CONTINUE
C
C               NO MATCH FOUND
C
                GOTO2510
C
C               MATCH FOUND
C
 2645           CONTINUE
                MAXNCH=80
                DO2650II=MAXNCH,1,-1
                  IF(IMACAR(IROWXX)(II:II).NE.' ')THEN
                    NCH=II
                    ITTEMP(1:NCH)=IMACAR(IROWXX)(1:NCH)
                    GOTO2659
                  ENDIF
 2650           CONTINUE
 2659           CONTINUE
C
                IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                  WRITE(ICOUT,22721)IMACAR(IROWXX)
22721             FORMAT('AT 2659: IMACAR(IROWXX) = ',A80)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,22722)NCH,ITTEMP
22722             FORMAT('NCH,ITTEMP = ',I5,A80)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
C
                ILOC1=I
                ILOC2=IP1+NCHTMP
                GOTO2519
C
              ENDIF
            ENDIF
          ENDIF
          GOTO2510
C
 2519     CONTINUE
          IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
            WRITE(ICOUT,2711)ILOC1,ILOC2,NUMTEC,IP1,NCH,NCHTMP
 2711       FORMAT('AT 2519: ILOC1,ILOC2,NUMTEC,IP1,NCH,NCHTMP = ',6I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2713)ITEXHO(ILOC1),ITEXHO(ILOC2)
 2713       FORMAT('ITEXHO(ILOC1),ITEXHO(ILOC2) = ',A4,2X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          NUMTE2=0
          IF(ILOC1.GE.1)THEN
            DO2720KK=1,ILOC1-1
              NUMTE2=NUMTE2+1
              ISTR(NUMTE2)=ITEXHO(KK)
 2720       CONTINUE
          ENDIF
C
          IF(NCH.GT.0)THEN
            DO2730KK=1,NCH
              NUMTE2=NUMTE2+1
              ISTR(NUMTE2)=ITTEMP(KK:KK)
 2730       CONTINUE
          ENDIF
C
          IF(NUMTEC.GE.ILOC2)THEN
            DO2740KK=ILOC2,NUMTEC
              NUMTE2=NUMTE2+1
              ISTR(NUMTE2)=ITEXHO(KK)
 2740       CONTINUE
          ENDIF
          NUMTEC=NUMTE2
          IF(NUMTEC.GE.1)THEN
            DO2750KK=1,NUMTEC
              ITEXHO(KK)=ISTR(KK)
 2750       CONTINUE
          ENDIF
C
 2510   CONTINUE
      ENDIF
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMTEC,NUMCHN
 9013   FORMAT('NUMTEC,NUMCHN = ',2I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ITEXHO(I),I=1,MIN(100,NUMTEC))
 9014   FORMAT('HOLLERITH ITEXHO(1) --',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ILOC1,ILOC2,ILOC3,NUMTEC
 9015   FORMAT('ILOC1,ILOC2,ILOC3,NUMTEC = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)(ISTR(I),I=1,MIN(80,NUMCHN))
 9017   FORMAT('(ISTR(I),I=1,NUMCHN) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IWORD1,IWORD2,IHNAP1,IHNAP2
 9018   FORMAT('IWORD1,IWORD2,IHNAP1,IHNAP2 = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IREPCH
 9021   FORMAT('IREPCH = ',A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IAMPER
 9022   FORMAT('IAMPER = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ILOC4M,ILOC4,ILOC4P,NUMTEC
 9023   FORMAT('ILOC4M,ILOC4,ILOC4P,NUMTEC = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHF.GT.0)THEN
          DO9028I=1,NUMCHF
            WRITE(ICOUT,9029)I,IFUNC(I)
 9029       FORMAT('I,IFUNC(I) = ',I8,A1)
            CALL DPWRST('XXX','BUG ')
 9028     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPREP3(ITEXHO,NUMTEC,IREPCH,IMALEV,
     1                  IBUGD2,IERROR)
C
C     PURPOSE--TRANSLATE A STRING AS DICTATED BY THE   ^   OPERATOR.
C              THAT IS, REPLACE ALL OCCURRANCES OF  XXX^YYY
C              (WHERE XXX IS AN ARBITRARY STRING, AND
C              YYY IS A DATAPLOT PARAMETER NAME OR A FUNCTION NAME)
C              BY THE NUMERIC VALUE OF THE PARAMETER YYY, OR
C              THE FUNCTIONAL STRING IN THE FUNCTION YYY.
C              (E.G., IF PARAMETER K HAS THE VALUE 7,
C              THEN   Y^K   BECOMES   Y7
C              OR     IF THE FUNCTION K HAS THE CONTENTS XYZ,
C              THEN   Y^K   BECOMES   YABC  ).
C     NOTE--THIS SUBROUTINE CHANGES THE CONTENTS OF THE INPUT VECTOR ITEXHO()
C           AND THE INPUT VARIABLE NUMTEC.
C     NOTE--THIS SUBROUTINE IS A COPY OF DPREP2, BUT WITH A NUMBER
C           OF THE ARGUMENTS TAKEN FROM THE INCLUDE FILES RATHER
C           THAN BEING EXPLICITLY PASSED.  THIS ROUTINE HAS BEEN
C           CREATED TO ALLOW IT TO BE CALLED FROM THE DPREAL
C           ROUTINE.   USE WHEN READING FROM STORED LOOP COMMANDS
C           RATHER THAN THE TERMINAL OR FILE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/1
C     ORIGINAL VERSION--JANUARY   2015.
C     UPDATED         --OCTOBER   2016. SUPPORT NAMED ARGUMENTS
C                                       ($Y, $TITLE, ETC.)
C     UPDATED         --OCTOBER   2016. INCREASE NUMBER OF ORDERED
C                                       ARGUMENTS FROM 10 TO 20
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ITEXHO
      CHARACTER*4 IBUGD2
      CHARACTER*1 IREPCH
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTR
      CHARACTER*4 IH
      CHARACTER*4 IFOUNV
      CHARACTER*4 IFOUNG
      CHARACTER*4 IWORD1
      CHARACTER*4 IWORD2
      CHARACTER*4 IHNAP1
      CHARACTER*4 IHNAP2
      CHARACTER*4 IUS
      CHARACTER*4 IROWFL
      CHARACTER*4 IJUNK1
      CHARACTER*4 IJUNK2
      CHARACTER*4 IAMPER
      CHARACTER*1 IC1
      CHARACTER*1 IC2
C
      CHARACTER*40 ILABT
      CHARACTER*40 ITTEMP
C
C-------------------------------------------------------------------
C
      DIMENSION ITEXHO(*)
      DIMENSION ISTR(200)
      DIMENSION IH(200)
C
C-----COMMON VARIABLES (GENERAL)------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT---------------------------------------------------
C
      IERROR='NO'
C
      NCH=0
      NUMCHN=0
      ILOC1=0
      ILOC2=0
      ILOC3=0
      I2=0
      IHNAP1='-999'
      IHNAP2='-999'
      IUS='-999'
      IITEMP=0
C
      IF(IBUGD2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NUMTEC,IREPCH
   53   FORMAT('NUMTEC,IREPCH = ',I5,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ITEXHO(I),I=1,MIN(100,NUMTEC))
   54   FORMAT('HOLLERITH ITEXHO(1) --',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************************
C               **  STEP 10--                                     **
C               **  LOOP THROUGH (AT MOST) 100 PASSES.  EACH PASS **
C               **  SEARCHES FOR THE NEXT OCCURRANCE OF ^.  A     **
C               **  GIVEN PASS WIPES OUT ^XX AND REPLACES IT WITH **
C               **  THE NUMERIC VALUE OF PARAMETER XX OR THE      **
C               **  STRING CONTENTS VALUE OF FUNCTION XX.  NOTE   **
C               **  THAT EACH PASS CHANGES THE CONTENTS OF INPUT  **
C               **  VARIABLE ITEXHO() AND INPUT VALUE NUMTEC.     **
C               ****************************************************
C
      IFOUNG='NO'
      DO1000IPASS=1,100
C
C               ****************************************************
C               **  STEP 11--                                     **
C               **  FOR THIS PASS,                                **
C               **  SEARCH THE STRING FOR THE NEXT OCCURRANCE OF  **
C               **  THE SUBSTRING IN IREPCH (USUALLY ^ )          **
C               **  PROCEED RIGHT TO LEFT (DEC. 1986).            **
C               **  IF FOUND, THEN PROCEED FURTHER.               **
C               **  IF NOT FOUND, THEN EXIT.                      **
C               ****************************************************
C
        IFOUNV='NO'
C
        IF(NUMTEC.GT.0)THEN
          DO1120IDUMMY=1,NUMTEC
            I=NUMTEC-IDUMMY+1
            IF(ITEXHO(I).EQ.IREPCH)THEN
              IFOUNV='YES'
              IFOUNG='YES'
              ILOC1=I
              ILOC2=I
              GOTO1190
            ENDIF
 1120     CONTINUE
          GOTO2500
        ELSE
          GOTO9000
        ENDIF
C
 1190   CONTINUE
C
C               *****************************************************
C               **  STEP 12--                                      **
C               **  EXTRACT THE PARAMETER OR FUNCTION NAME.        **
C               **  THIS WILL BE THE STRING IMMEDIATELY FOLLOWING  **
C               **  ^ UNTIL A BLANK IS FOUND                       **
C               **  OR UNTIL A    "   IS FOUND (DEC. 1986)         **
C               **  OR UNTIL A    )   IS FOUND (DEC. 1988)         **
C               **  OR UNTIL A    &   IS FOUND (DEC. 1988)         **
C               *****************************************************
C
C       SEPTEMBER 2007.  CHECK FOR ^ROWLABEL^K TYPE SYNTAX.  THIS
C                        WILL SUBSTITUTE THE K-TH ROW LABEL.
C
        DO1210I=1,8
          ISTR(I)=' '
 1210   CONTINUE
C
        IAMPER='NO'
C
        IMIN=ILOC2+1
        IMAX=IMIN+7
        IF(IMAX.GT.NUMTEC)IMAX=NUMTEC
        J=0
C
C  JUNE 2003.  BASICALLY, A DATAPLOT NAME CONSISTS OF NUMBERS OR
C              ALPABETIC CHARACTERS OR UDERSCORES.  ANYTHING ELSE
C              SHOULD TERMINATE THE NAME.
C
        DO1250I=IMIN,IMAX
          I2=I
          ITEMP=ICHAR(ITEXHO(I)(1:1))
C
          IF(IBUGD2.EQ.'ON')THEN
            WRITE(ICOUT,1251)I,IPASS,ITEXHO(I)
 1251       FORMAT('I,IPASS,ITEXHO(I) = ',2I8,1X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(ITEMP.LT.48 .OR. ITEMP.GT.122 .OR.
     1       (ITEMP.GT.57 .AND. ITEMP.LT.65) .OR.
     1       (ITEMP.GT.90 .AND. ITEMP.LT.97 .AND. ITEMP.NE.95)
     1       )THEN
             ILOC3=I2-1
             IF(ITEMP.EQ.38)IAMPER='YES'
             GOTO1290
          ENDIF
          J=J+1
          ISTR(J)=ITEXHO(I)
 1250   CONTINUE
        ILOC3=I2
C
 1290   CONTINUE
        NUMCHN=J
C
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1291)ILOC1,ILOC2,ILOC3
 1291     FORMAT('ILOC1,ILOC2,ILOC3 = ',3I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1292)IMIN,IMAX,NUMCHN
 1292     FORMAT('IMIN,IMAX,NUMCHN = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ****************************************************
C               **  STEP 13--                                     **
C               **  PACK THE PARAMETER/FUNCTION NAME STRING INTO  **
C               **  2 4-BYTE WORDS.                               **
C               ****************************************************
C
        IWORD1='    '
        IWORD2='    '
        DO1310I=1,4
          IWORD1(I:I)=ISTR(I)(1:1)
          IWORD2(I:I)=ISTR(I+4)(1:1)
1310    CONTINUE
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1302)IWORD1,IWORD2
1302      FORMAT('IWORD1,IWORD2=',A4,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IHNAP1=IWORD1
        IHNAP2=IWORD2
C
C               ****************************************************
C               **  STEP 14--                                     **
C               **  CONVERT THE 2 4-BYTE WORDS INTO UPPER CASE.   **
C               **  (JUNE 1987)                                   **
C               ****************************************************
C
        IJUNK1=IHNAP1
        IJUNK2=IHNAP2
        CALL DPUPP4(IJUNK1,IJUNK1,IBUGD2,IERROR)
        CALL DPUPP4(IJUNK2,IJUNK2,IBUGD2,IERROR)
        IHNAP1=IJUNK1
        IHNAP2=IJUNK2
C
C               ****************************************
C               **  STEP 15--                         **
C               **  DETERMINE IF THE NAME IS IN THE   **
C               **  INTERNAL DATAPLOT NAME LIST,      **
C               **  AND AS A PARAMETER OR FUNCTION.   **
C               ****************************************
C
        IF(NUMNAM.GT.0)THEN
          DO1500I=1,NUMNAM
            I2=I
            IF(IHNAP1.EQ.IHNAME(I).AND.IHNAP2.EQ.IHNAM2(I))GOTO1550
 1500     CONTINUE
C
          NH=1
          IH(1)='    '
          GOTO2100
C
 1550     CONTINUE
          IVAL=IVALUE(I2)
          VAL=VALUE(I2)
          IUS=IUSE(I2)
          IL1=IVSTAR(I2)
          IL2=IVSTOP(I2)
C
          IF(IBUGD2.EQ.'ON')THEN
            WRITE(ICOUT,1551)IPASS,IL1,IL2
 1551       FORMAT('IPASS,IL1,IL2 = ',3I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1553)IVAL,VAL
 1553       FORMAT('IVAL,VAL = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
            IF(IUS.EQ.'F')THEN
              ITEMP=IL2-IL1+1
              IF(ITEMP.GT.100)THEN
                ITEMP=IL1+99
              ELSE
                ITEMP=IL2
              ENDIF
              WRITE(ICOUT,1555)(IFUNC(KKK),KKK=IL1,ITEMP)
 1555         FORMAT('IFUNC(IL1:IL2) = ',100A1)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
C
          ILABT=' '
          IF(IVAL.GT.0.AND.IUS.EQ.'V')ILABT(1:40)=IVARLB(IVAL)(1:40)
C
          IF(IUS.NE.'P' .AND. IUS.NE.'F' .AND. IUS.NE.'V')THEN
            WRITE(ICOUT,1561)
 1561       FORMAT('***** ERROR IN DPREP3--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1562)
 1562       FORMAT('      THE EXTRACTED NAME FOR THE TEXT STRING')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1563)
 1563       FORMAT('      WAS FOUND IN INTERNAL NAME LIST, BUT NOT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1564)
 1564       FORMAT('      AS A PARAMETER, A VARIABLE, OR A FUNCTION.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1566)IHNAP1,IHNAP2
 1566       FORMAT('      EXTRACTED NAME = ',A4,A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1567)IUS
 1567       FORMAT('      USE = ',A4)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
        ENDIF
C
C               ************************************************
C               **  STEP 16--                                 **
C               **  FOR THE CASE WHEN HAVE A PARAMETER NAME,  **
C               **  DETERMINE THE LITERAL STRING ASSOCIATED   **
C               **  WITH THE PARAMETER VALUE.                 **
C               ************************************************
C
CCCCCC  SEPTEMBER 2007: CHECK FOR ROWLABEL:
C
        IF(IUS.EQ.'P')THEN
          IROWFL='OFF'
          IF(ILOC1.GE.9 .AND. IVAL.GT.0 .AND. IVAL.LE.MAXOBV)THEN
            IF((ITEXHO(ILOC1-8)(1:1).EQ.'R' .OR.
     1          ITEXHO(ILOC1-8)(1:1).EQ.'r') .AND.
     1         (ITEXHO(ILOC1-7)(1:1).EQ.'O' .OR. 
     1          ITEXHO(ILOC1-7)(1:1).EQ.'o') .AND.
     1         (ITEXHO(ILOC1-6)(1:1).EQ.'W' .OR.
     1          ITEXHO(ILOC1-6)(1:1).EQ.'w') .AND.
     1         (ITEXHO(ILOC1-5)(1:1).EQ.'L' .OR.
     1          ITEXHO(ILOC1-5)(1:1).EQ.'l') .AND.
     1         (ITEXHO(ILOC1-4)(1:1).EQ.'A' .OR.
     1          ITEXHO(ILOC1-4)(1:1).EQ.'a') .AND.
     1         (ITEXHO(ILOC1-3)(1:1).EQ.'B' .OR.
     1          ITEXHO(ILOC1-3)(1:1).EQ.'b') .AND.
     1         (ITEXHO(ILOC1-2)(1:1).EQ.'E' .OR.
     1          ITEXHO(ILOC1-2)(1:1).EQ.'e') .AND.
     1         (ITEXHO(ILOC1-1)(1:1).EQ.'L' .OR.
     1          ITEXHO(ILOC1-1)(1:1).EQ.'l'))THEN
              IROWFL='YES'
              ILOC1=ILOC1-9
            ENDIF
          ENDIF
          IF(IROWFL.EQ.'ON')THEN
            ILABT=' '
            ILABT(1:24)=IROWLB(IVAL)(1:24)
            NH=24
            DO1710I=24,1,-1
              IF(ILABT(I:I).NE.' ')THEN
                NH=I
                GOTO1719
              ENDIF
 1710       CONTINUE
            NH=0
 1719       CONTINUE
            IF(NH.LE.0)THEN
              IH(1)='R'
              IH(2)='O'
              IH(3)='W'
              IH(4)=' '
              CALL DPCONH(IVAL,VAL,IH(5),NHTEMP,IBUGD2,IERROR)
              NH=NHTEMP+4
            ELSE
              DO1730I=1,NH
                IH(I)=' '
                IH(I)(1:1)=ILABT(I:I)
 1730         CONTINUE
            ENDIF
          ELSE
            CALL DPCONH(IVAL,VAL,IH,NH,IBUGD2,IERROR)
          ENDIF
        ELSEIF(IUS.EQ.'F')THEN
          CALL DPCOFH(IL1,IL2,IFUNC,NUMCHF,IH,NH,IBUGD2,IERROR)
        ELSEIF(IUS.EQ.'V')THEN
          NH=52
          DO1610I=52,1,-1
            IF(ILABT(I:I).NE.' ')THEN
              NH=I
              GOTO1619
            ENDIF
 1610     CONTINUE
          NH=0
 1619     CONTINUE
          IF(NH.EQ.0)THEN
            DO1620I=1,4
              IH(I)=' '
              IH(I+4)=' '
              IH(I)(1:1)=IHNAP1(I:I)
              IH(I+4)(1:1)=IHNAP2(I:I)
 1620       CONTINUE
            NH=8
            DO1625I=8,1,-1
              IF(IH(I).NE.' ')THEN
                NH=I
                GOTO1629
              ENDIF
 1625       CONTINUE
 1629       CONTINUE
          ELSE
            DO1630I=1,NH
              IH(I)=' '
              IH(I)(1:1)=ILABT(I:I)
 1630       CONTINUE
          ENDIF
        ENDIF
C
C               ****************************************************
C               **  STEP 21--                                     **
C               **  COLLAPSE THE SUBSTRING ^ FOLLOWED BY          **
C               **  THE PARAMETER/FUNCTION NAME TO A NULL STRING. **
C               ****************************************************
C
 2100   CONTINUE
        ILOC1M=ILOC1-1
        ILOC3P=ILOC3+1
        J=ILOC1M
        IF(NUMTEC.GE.ILOC3P)THEN
          DO2110I=ILOC3P,NUMTEC
            J=J+1
            ITEXHO(J)=ITEXHO(I)
 2110     CONTINUE
        ENDIF
        NUMTE2=J
C
C               ****************************************************
C               **  STEP 22--                                     **
C               **  INSERT THE LITERAL VALUE                      **
C               **  AT THE PROPER PLACE IN THE COLLAPSED STRING.  **
C               ****************************************************
C
        J=ILOC1M
        IF(NUMTE2.GE.ILOC1)THEN
          DO2200I=ILOC1,NUMTE2
            IREV=NUMTE2-I+ILOC1
            IREVNH=IREV+NH
            ITEXHO(IREVNH)=ITEXHO(IREV)
 2200     CONTINUE
        ENDIF
C
        IF(NH.GE.1)THEN
          DO2300I=1,NH
            J=ILOC1M+I
            ITEXHO(J)=IH(I)
 2300     CONTINUE
        ENDIF
        NUMTE3=NUMTE2+NH
        NUMTEC=NUMTE3
C
C               ****************************************************
C               **  STEP 24--                                     **
C               **  IF THE TERMINATOR WAS &,                      **
C               **  THEN COLLAPSE & TO A NULL STRING.             **
C               **  (THUS & SERVES AS A USEFUL CONCATONATION      **
C               **  CHARACTER.                                    **
C               **  (DECEMBER 1988)                               **
C               ****************************************************
C
        IF(IAMPER.EQ.'YES')THEN
          ILOC4=ILOC1+NH
          ILOC4M=ILOC4-1
          ILOC4P=ILOC4+1
          J=ILOC4M
          IF(NUMTEC.GE.ILOC4P)THEN
            DO2420I=ILOC4P,NUMTEC
              J=J+1
              ITEXHO(J)=ITEXHO(I)
 2420       CONTINUE
            NUMTEC=J
          ENDIF
        ENDIF
C
 1000 CONTINUE
C
C
C               ****************************************************
C               **  STEP 25--                                     **
C               **  NOW CHECK FOR ANY MACRO SUBSTITUTION          **
C               **  CHARACTERS.  THESE ARE IDENTIFIED BY A        **
C               **  $1, $2, ..., $10.  NOTE THAT $0 IS USED TO    **
C               **  DENOTE THE NUMBER OF MACRO ARGUMENTS.         **
C               **  (SEPTEMBER  2005)                             **
C               ****************************************************
C
 2500 CONTINUE
      IF(NUMTEC.GT.0 .AND. IMALEV.GE.1)THEN
        MAXNCH=80
        DO2510I=1,NUMTEC-1
          IF(ITEXHO(I).EQ.IMACSC)THEN
            IP1=I+1
            IP2=I+2
            IP3=I+3
            IF(ITEXHO(IP1).EQ.'0' .AND. ITEXHO(IP2).EQ.'0')THEN
              ITTEMP=' '
              WRITE(ITTEMP(1:2),'(I2)')NMACLA
              NCH=2
              ILOC1=I
              ILOC2=IP2+2
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'0')THEN
              ITTEMP=' '
              WRITE(ITTEMP(1:2),'(I2)')NMACAG
              NCH=2
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'0')THEN
              DO2610II=MAXNCH,1,-1
                IF(IMACAR(10)(II:II).NE.' ')THEN
                  NCH=II
                  DO2613JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(10)(JJ:JJ)
 2613             CONTINUE
                  GOTO2619
                ENDIF
 2610         CONTINUE
 2619         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'1')THEN
              DO3610II=MAXNCH,1,-1
                IF(IMACAR(11)(II:II).NE.' ')THEN
                  NCH=II
                  DO3613JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(11)(JJ:JJ)
 3613             CONTINUE
                  GOTO3619
                ENDIF
 3610         CONTINUE
 3619         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'2')THEN
              DO3620II=MAXNCH,1,-1
                IF(IMACAR(12)(II:II).NE.' ')THEN
                  NCH=II
                  DO3623JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(12)(JJ:JJ)
 3623             CONTINUE
                  GOTO3629
                ENDIF
 3620         CONTINUE
 3629         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'3')THEN
              DO3630II=MAXNCH,1,-1
                IF(IMACAR(13)(II:II).NE.' ')THEN
                  NCH=II
                  DO3633JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(13)(JJ:JJ)
 3633             CONTINUE
                  GOTO3639
                ENDIF
 3630         CONTINUE
 3639         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'4')THEN
              DO3640II=MAXNCH,1,-1
                IF(IMACAR(14)(II:II).NE.' ')THEN
                  NCH=II
                  DO3643JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(14)(JJ:JJ)
 3643             CONTINUE
                  GOTO3649
                ENDIF
 3640         CONTINUE
 3649         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'5')THEN
              DO3650II=MAXNCH,1,-1
                IF(IMACAR(15)(II:II).NE.' ')THEN
                  NCH=II
                  DO3653JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(15)(JJ:JJ)
 3653             CONTINUE
                  GOTO3659
                ENDIF
 3650         CONTINUE
 3659         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'6')THEN
              DO3660II=MAXNCH,1,-1
                IF(IMACAR(16)(II:II).NE.' ')THEN
                  NCH=II
                  DO3663JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(16)(JJ:JJ)
 3663             CONTINUE
                  GOTO3669
                ENDIF
 3660         CONTINUE
 3669         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'7')THEN
              DO3670II=MAXNCH,1,-1
                IF(IMACAR(17)(II:II).NE.' ')THEN
                  NCH=II
                  DO3673JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(17)(JJ:JJ)
 3673             CONTINUE
                  GOTO3679
                ENDIF
 3670         CONTINUE
 3679         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'8')THEN
              DO3680II=MAXNCH,1,-1
                IF(IMACAR(18)(II:II).NE.' ')THEN
                  NCH=II
                  DO3683JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(18)(JJ:JJ)
 3683             CONTINUE
                  GOTO3689
                ENDIF
 3680         CONTINUE
 3689         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'9')THEN
              DO3690II=MAXNCH,1,-1
                IF(IMACAR(19)(II:II).NE.' ')THEN
                  NCH=II
                  DO3693JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(19)(JJ:JJ)
 3693             CONTINUE
                  GOTO3699
                ENDIF
 3690         CONTINUE
 3699         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'2' .AND. ITEXHO(IP2).EQ.'0')THEN
              DO3710II=MAXNCH,1,-1
                IF(IMACAR(20)(II:II).NE.' ')THEN
                  NCH=II
                  DO3713JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(20)(JJ:JJ)
 3713             CONTINUE
                  GOTO3719
                ENDIF
 3710         CONTINUE
 3719         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'0')THEN
              IF(NMACAG.LE.9)THEN
                WRITE(ITTEMP(1:1),'(I1)')NMACAG
                NCH=1
              ELSEIF(NMACAG.LE.99)THEN
                WRITE(ITTEMP(1:2),'(I2)')NMACAG
                NCH=2
              ELSE
                GOTO2510
              ENDIF
              ILOC1=I
              ILOC2=IP1+1
              GOTO2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .OR. ITEXHO(IP1).EQ.'2' .OR.
     1             ITEXHO(IP1).EQ.'3' .OR. ITEXHO(IP1).EQ.'4' .OR.
     1             ITEXHO(IP1).EQ.'4' .OR. ITEXHO(IP1).EQ.'5' .OR.
     1             ITEXHO(IP1).EQ.'6' .OR. ITEXHO(IP1).EQ.'7' .OR.
     1             ITEXHO(IP1).EQ.'8' .OR. ITEXHO(IP1).EQ.'9')THEN
              IF(ITEXHO(IP1).EQ.'1')IITEMP=1
              IF(ITEXHO(IP1).EQ.'2')IITEMP=2
              IF(ITEXHO(IP1).EQ.'3')IITEMP=3
              IF(ITEXHO(IP1).EQ.'4')IITEMP=4
              IF(ITEXHO(IP1).EQ.'5')IITEMP=5
              IF(ITEXHO(IP1).EQ.'6')IITEMP=6
              IF(ITEXHO(IP1).EQ.'7')IITEMP=7
              IF(ITEXHO(IP1).EQ.'8')IITEMP=8
              IF(ITEXHO(IP1).EQ.'9')IITEMP=9
              MAXNCH=80
              DO2630II=MAXNCH,1,-1
                IF(IMACAR(IITEMP)(II:II).NE.' ')THEN
                  NCH=II
                  DO2633JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(IITEMP)(JJ:JJ)
 2633             CONTINUE
                  GOTO2639
                ENDIF
 2630         CONTINUE
 2639         CONTINUE
              ILOC1=I
              ILOC2=IP1+1
              GOTO2519
            ELSE
C
C             CHECK FOR NAMED ARGUMENTS
C
              IF(NMACLA.GE.1)THEN
C
C               2018/07: NEED TO CHECK THE NUMBER OF CHARACTERS IN
C                        ITEXHO TO BE INCLUDED IN THE MATCH.  THE
C                        LABEL NAME SHOULD ONLY CONTAIN NUMBERS,
C                        LOWER OR UPPER CASE ALPHABETIC, OR UNDERSCORE
C                        CHARACTER.  ANY OTHER CHARACTER TERMINATES
C                        CURRENT NAME.
C
                NMAXCH=0
                DO2640JJ=IP1+1,NUMTEC
                  NMAXCH=NMAXCH+1
                  IC1=ITEXHO(IP1+KK-1)(1:1)
                  IF(IC1.EQ.'_')GOTO2640
                  CALL DPCOAN(IC1,IVAL1)
                  IF(IVAL1.GE.48 .AND. IVAL1.LE.57)GOTO2640
                  IF(IVAL1.GE.97 .AND. IVAL1.LE.122)GOTO2640
                  IF(IVAL1.GE.65 .AND. IVAL1.LE.90)GOTO2640
                  NMAXCH=NMAXCH-1
                  GOTO2641
 2640           CONTINUE
 2641           CONTINUE
C
                IF(IBUGD2.EQ.'ON')THEN
                  WRITE(ICOUT,22703)JJ,IMACNC(JJ)
22703             FORMAT('JJ,IMACNC(JJ) = ',3I5)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
C
                DO2642JJ=1,NMACLA
                  IFLAG=1
                  ILAST=MAX(IMACNC(JJ),NMAXCH)
                  DO2643KK=1,ILAST
C
C                   MAKE NAME SEARCH CASE INSENSITIVE
C
                    IC1=ITEXHO(IP1+KK-1)(1:1)
                    CALL DPCOAN(IC1,IVAL1)
                    IF(IVAL1.GE.97 .AND. IVAL1.LE.122)IVAL1=IVAL1-32
                    CALL DPCONA(IVAL1,IC1)
                    IC2=IMACLA(JJ)(KK:KK)
                    CALL DPCOAN(IC2,IVAL2)
                    IF(IVAL2.GE.97 .AND. IVAL2.LE.122)IVAL2=IVAL2-32
                    CALL DPCONA(IVAL2,IC2)
C
                    IF(IBUGD2.EQ.'ON')THEN
                      WRITE(ICOUT,22731)JJ,KK,IC1,IC2
22731                 FORMAT('AT 2642: JJ,KK,IC1,IC2 = ',2I5,2(A1,1X))
                      CALL DPWRST('XXX','BUG ')
                    ENDIF
C
                    IF(IC1.NE.IC2)THEN
                      IFLAG=0
                      GOTO2642
                    ENDIF
 2643             CONTINUE
                  IF(IFLAG.EQ.1)THEN
                    IROWXX=IMACLL(JJ)
                    NCHTMP=IMACNC(JJ)
C
                    IF(IBUGD2.EQ.'ON')THEN
                      WRITE(ICOUT,22711)JJ,IROWXX,NCHTMP
22711                 FORMAT('AT 2643: JJ,IROWXX,NCHTMP=',3I8)
                      CALL DPWRST('XXX','BUG ')
                    ENDIF
C
                    GOTO2645
                  ENDIF
 2642           CONTINUE
C
C               NO MATCH FOUND
C
                GOTO2510
C
C               MATCH FOUND
C
 2645           CONTINUE
                MAXNCH=80
                DO2650II=MAXNCH,1,-1
                  IF(IMACAR(IROWXX)(II:II).NE.' ')THEN
                    NCH=II
                    DO2653JJ=1,NCH
                      ITTEMP(JJ:JJ)=IMACAR(IROWXX)(JJ:JJ)
 2653               CONTINUE
                    GOTO2659
                  ENDIF
 2650           CONTINUE
 2659           CONTINUE
                ILOC1=I
                ILOC2=IP1+NCHTMP
                GOTO2519
C
              ENDIF
            ENDIF
          ENDIF
          GOTO2510
C
 2519     CONTINUE
          NUMTE2=0
          DO2720KK=1,ILOC1-1
            NUMTE2=NUMTE2+1
            ISTR(NUMTE2)=ITEXHO(KK)
 2720     CONTINUE
          DO2730KK=1,NCH
            NUMTE2=NUMTE2+1
            ISTR(NUMTE2)=ITTEMP(KK:KK)
 2730     CONTINUE
          IF(NUMTEC.GE.ILOC2)THEN
            DO2740KK=ILOC2,NUMTEC
              NUMTE2=NUMTE2+1
              ISTR(NUMTE2)=ITEXHO(KK)
 2740       CONTINUE
          ENDIF
          NUMTEC=NUMTE2
          DO2750KK=1,NUMTEC
            ITEXHO(KK)=ISTR(KK)
 2750     CONTINUE
C
 2510   CONTINUE
      ENDIF
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
C
      IF(IBUGD2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMTEC
 9013   FORMAT('NUMTEC = ',I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ITEXHO(I),I=1,MIN(100,NUMTEC))
 9014   FORMAT('HOLLERITH ITEXHO(1) --',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ILOC1,ILOC2,ILOC3,NUMTEC
 9015   FORMAT('ILOC1,ILOC2,ILOC3,NUMTEC = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)NUMCHN
 9016   FORMAT('NUMCHN = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)(ISTR(I),I=1,MIN(80,NUMCHN))
 9017   FORMAT('(ISTR(I),I=1,NUMCHN) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IWORD1,IWORD2,IHNAP1,IHNAP2
 9018   FORMAT('IWORD1,IWORD2,IHNAP1,IHNAP2 = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ILOC4M,ILOC4,ILOC4P,NUMTEC
 9023   FORMAT('ILOC4M,ILOC4,ILOC4P,NUMTEC = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHF.GT.0)THEN
          DO9028I=1,NUMCHF
            WRITE(ICOUT,9029)I,IFUNC(I)
 9029       FORMAT('I,IFUNC(I) = ',I8,A1)
            CALL DPWRST('XXX','BUG ')
 9028     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPREPS(Y,X1,MAXNXT,N,NUMVAR,DUM1,DUM2,
     1                  IREP,REPSS,REPMS,REPSD,REPDF,NUMSET,
     1                  IBUGA3,IERROR)
C
C     PURPOSE--DETERMINE IF REPLICATION EXISTS AND
C              (IF EXISTENT) COMPUTE THE REPLIATION STANDARD DEVIATION
C              AND REPLICATION DEGREES OF FREEDOM.
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
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1992. FIX FORMAT
C     UPDATED         --JULY      2019. CONVERT X1 ... X5 TO SINGLE
C                                       MATRIX ARGUMENT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IREP
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X1(MAXNXT,NUMVAR)
      DIMENSION DUM1(*)
      DIMENSION DUM2(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRE'
      ISUBN2='PS  '
      IERROR='NO'
C
      IREP='NO'
      REPSS=0.0
      REPMS=0.0
      REPSD=0.0
      REPDF=0.0
      NUMSET=0
C
C               ********************************************************
C               **  CHECK FOR REPLICATION AND IF EXISTENT COMPUTE     **
C               **  A (MODEL-FREE) REPLICATION STANDARD DEVIATION.    **
C               ********************************************************
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREPS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,NUMVAR
   52   FORMAT('IBUGA3,N,NUMVAR = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)Y(1),(X1(1,J),J=1,MAX(NUMVAR,5))
   54   FORMAT('Y(1),X1(1),X2(1),X3(1),X4(I),X5(1) = ',6E13.5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)DUM1(1),DUM2(1)
   55   FORMAT('DUM1(1),DUM2(1) = ',2E13.5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  DETERMINE THE NUMBER OF DISTINCT SUBSETS          **
C               **  FOR VARIABLE 1;                                   **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
C               **  WITHOUT FURTHER CHECKING OF THE OTHER VARIABLES.  **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMSET=0
      DO4200I=1,N
        IF(NUMSET.EQ.0)GOTO4350
        DO4300J=1,NUMSET
          IF(X1(I,1).EQ.DUM1(J))GOTO4200
 4300   CONTINUE
 4350   NUMSET=NUMSET+1
        DUM1(NUMSET)=X1(I,1)
 4200 CONTINUE
      IF(NUMSET.EQ.0)THEN
        WRITE(ICOUT,4205)
 4205   FORMAT('ERROR IN DPREPS   SUBROUTINE--NUMSET = 0')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NUMSET.EQ.N)THEN
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 2--                                          **
C               **  FOR THE CASE WHEN HAVE SOME REPLICATION FOR X1,   **
C               **  AND WHEN THE NUMBER OF VARIABLES IS 1, COPY OUT   **
C               **  THE Y'S FOR EACH X1 SUBSET INTO THE DUM2 VECTOR   **
C               **  AND ANALYZE IT THEREIN.                           **
C               ********************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMVAR.LE.1)THEN
        IREP='YES'
        IREPDF=0
        REPSS=0.0
C
        DO4600ISET=1,NUMSET
          NI=0
          DO4700I=1,N
            IF(X1(I,1).EQ.DUM1(ISET))NI=NI+1
            IF(X1(I,1).EQ.DUM1(ISET))DUM2(NI)=Y(I)
 4700     CONTINUE
          ANI=NI
          SUM=0.0
          DO5100I=1,NI
            SUM=SUM+DUM2(I)
 5100     CONTINUE
          YMEAN=SUM/ANI
          SUM=0.0
          DO5200I=1,NI
            SUM=SUM+(DUM2(I)-YMEAN)**2
 5200     CONTINUE
          IREPDF=IREPDF+NI-1
          REPSS=REPSS+SUM
 4600   CONTINUE
        GOTO4800
      ENDIF
C
C               ********************************************************
C               **  STEP 3--                                          **
C               **  FOR THE CASE WHEN HAVE SOME REPLICATION FOR X1    **
C               **  AND THE NUMBER OF VARIABLES IS 2 OR MORE,         **
C               **  CARRY OUT A DETAILED EXAMINATION FOR REPLICATION  **
C               **  AND ANALYZE APPROPRIATELY.                        **
C               ********************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IREP='YES'
      IREPDF=0
      REPSS=0.0
C
      DO4405I=1,N
        DUM1(I)=-1.0
 4405 CONTINUE
C
      NUMSET=0
      DO4410I=1,N
        IF(DUM1(I).GT.0.0)GOTO4410
        NI=0
        DO4420J=I,N
          DO4425K=1,NUMVAR
            IF(X1(J,K).NE.X1(I,K))GOTO4420
 4425     CONTINUE
          NI=NI+1
          DUM1(J)=1.0
          DUM2(NI)=Y(J)
 4420   CONTINUE
        NUMSET=NUMSET+1
        IF(NI.LE.1)GOTO4410
        ANI=NI
        SUM=0.0
        DO4450L=1,NI
          SUM=SUM+DUM2(L)
 4450   CONTINUE
        YMEAN=SUM/ANI
        SUM=0.0
        DO4460L=1,NI
          SUM=SUM+(DUM2(L)-YMEAN)**2
 4460   CONTINUE
        IREPDF=IREPDF+NI-1
        REPSS=REPSS+SUM
 4410 CONTINUE
C
 4800 CONTINUE
      IF(IREPDF.LE.0)THEN
        IREP='NO'
        GOTO9000
      ENDIF
      REPDF=IREPDF
      REPMS=REPSS/REPDF
      REPSD=0.0
      IF(REPMS.GT.0.0)REPSD=SQRT(REPMS)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREPS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IERROR,IREP,REPSD,REPDF,NUMSET
 9014   FORMAT('IERROR,IREP,REPSD,REPDF,NUMSET = ',2(A4,2X),2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRES2(X1,Y1,X2,Y2,PX,PY,
     1                  IFIG,ILINPA,ILINCO,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,PREBTH,
     1                  IREFSW,IREFCO,
     1                  IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
C     PURPOSE--DRAW A RESISITOR WITH ONE END AT (X1,Y1)
C              AND THE OTHER END AT (X2,Y2).
C     NOTE--THE HEIGHT OF EACH RIPPLE IS PTEXHE.
C           THE WIDTH  OF EACH RIPPLE IS PTEXWI.
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
C     UPDATED         --JANUARY   1989.  MODIFY CALLS TO DPDRPL (ALAN)
C     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPRESI
C                                       RATHER THAN DPRES2
C
C-----NON-COMMON VARIABLES-------------------------------------
C
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
CCCCC CHARACTER*4 ICOLF
CCCCC CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
C
      DIMENSION PX(*)
      DIMENSION PY(*)
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'RES2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRES2--')
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
C               *********************************
C               **  STEP 1--                   **
C               **  DETERMINE THE COORDINATES  **
C               **  FOR THE RESISTOR           **
C               *********************************
C
      AJY2=0
C
      DELX=X2-X1
      DELY=Y2-Y1
      ALEN=0.0
      TERM=(X2-X1)**2+(Y2-Y1)**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
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
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      X=XMIN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      L=0
CCCCC DO1450JX=AJXMIN,AJXMAX,AJXDEL
      AJX=AJXMIN-AJXDEL
 1440 CONTINUE
      AJX=AJX+AJXDEL
      IF(AJX.GT.AJXMAX)GOTO1460
C
      L=L+1
      L01=L-2*(L/2)
C
      AJX1=AJX
      AJX2=AJX+AJXDEL/2.0
      AJX3=AJX+AJXDEL
      AJY1=0.0
      IF(L01.EQ.0)AJY2=AJYDEL/2.0
      IF(L01.EQ.1)AJY2=-AJYDEL/2.0
      AJY3=0
C
      X=AJX1
      Y=AJY1
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      X=AJX2
      Y=AJY2
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      X=AJX3
      Y=AJY3
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      GOTO1440
C
 1460 CONTINUE
      X=ALEN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
C
      NP=K
C
C               ***********************
C               **  STEP 2--         **
C               **  FILL THE FIGURE  **
C               **  (IF CALLED FOR)  **
C               ***********************
C
CCCCC IF(IREFSW(1).EQ.'OFF')GOTO2190
CCCCC IPATT=IREPTY(1)
CCCCC PTHICK=PREPTH(1)
CCCCC PXGAP=PREPSP(1)
CCCCC PYGAP=PREPSP(1)
CCCCC ICOLF=IREFCO(1)
CCCCC ICOLP=IREPCO(1)
CCCCC CALL DPFIRE(PX,PY,NP,
CCCCC1IFIG,IPATT,PTHICK,PXGAP,PYGAP,ICOLF,ICOLP)
C2190 CONTINUE
C
C               *********************************
C               **  STEP 3--                   **
C               **  DRAW OUT THE FIGURE        **
C               *********************************
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'RES2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRES2--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9039)IERRG4
 9039   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRES1(IHARG,IARGT,IARG,NUMARG,IDEFSR,
     1IRECSR,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE RECIPE SIMCOV REPLICATES
C              IN THE FLOATING POINT VARIABLE IRECSR.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --IARG   (A  INTEGER POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFSR (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--IRECSR  (A  FLOATING POINT VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SIMC'.AND.IHARG(2).EQ.'REPL')
     1GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'REPL')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'REPL')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPRES1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE SIMCOV REPLICATES ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE SIMCOV REPLICATES 100000 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      IHOLD=IDEFSR
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(IHOLD.LE.1000)IHOLD=1000
      IRECSR=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECSR
 1181 FORMAT('THE NUMBER OF REPLICATIONS FOR THE SIMCOV ',
     1'COMMAND HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPRESZ(IHARG,IARGT,IARG,NUMARG,IDEFSR,
     1IRECSR,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE RECIPE SIMPVT REPLICATES
C              IN THE FLOATING POINT VARIABLE IRECSR.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --IARG   (A  INTEGER POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFSR (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--IRECSR  (A  FLOATING POINT VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SIMP'.AND.IHARG(2).EQ.'REPL')
     1GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIMC')GOTO1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'REPL')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'REPL')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPRESZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE SIMPVT REPLICATES ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE SIMPVT REPLICATES 100000 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      IHOLD=IDEFSR
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(IHOLD.LE.1000)IHOLD=1000
      IRECSR=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECSR
 1181 FORMAT('THE NUMBER OF REPLICATIONS FOR THE SIMPVT ',
     1'COMMAND HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPRESA(IHARG,NUMARG,IDEFSA,IRECSA,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--SPECIFY WHETHER THE RECIPE COMMAND USES 
C              SATTERTHWAITE APPROXIMATION TO OBTAIN CRITICAL 
C              VALUES OR NOT.
C
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFSA (A  CHARACTER VARIABLE)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--IRECSA (A CHARACTER VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFSA
      CHARACTER*4 IRECSA
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
   51 FORMAT('***** AT THE BEGINNING OF DPRESA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFSA
   53 FORMAT('IDEFSA = ',A4)
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
      IF(NUMARG.LE.1)THEN
        IHOLD=IDEFSA
      ELSEIF(NUMARG.EQ.2)THEN
        IF(IHARG(1).EQ.'SATT'.AND.IHARG(2).EQ.'APPR')THEN
          IHOLD=IDEFSA
        ELSE
          IHOLD=IDEFSA
          IF(IHARG(2).EQ.'OFF')IHOLD='OFF'
          IF(IHARG(2).EQ.'NO')IHOLD='OFF'
          IF(IHARG(2).EQ.'NONE')IHOLD='OFF'
          IF(IHARG(2).EQ.'FALS')IHOLD='OFF'
          IF(IHARG(2).EQ.'ON')IHOLD='ON'
          IF(IHARG(2).EQ.'YES')IHOLD='ON'
          IF(IHARG(2).EQ.'TRUE')IHOLD='ON'
          IF(IHARG(2).EQ.'DEFA')IHOLD=IDEFSA
        ENDIF
      ELSEIF(NUMARG.EQ.3)THEN
          IHOLD=IDEFSA
          IF(IHARG(3).EQ.'OFF')IHOLD='OFF'
          IF(IHARG(3).EQ.'NO')IHOLD='OFF'
          IF(IHARG(3).EQ.'NONE')IHOLD='OFF'
          IF(IHARG(3).EQ.'FALS')IHOLD='OFF'
          IF(IHARG(3).EQ.'ON')IHOLD='ON'
          IF(IHARG(3).EQ.'YES')IHOLD='ON'
          IF(IHARG(3).EQ.'TRUE')IHOLD='ON'
          IF(IHARG(3).EQ.'DEFA')IHOLD=IDEFSA
      ELSE
        GOTO9000
      ENDIF
C
      IFOUND='YES'
      IRECSA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECSA
 1181 FORMAT(
     1'THE RECIPE SATTERTHWAITE APPROXIMATION HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1189 CONTINUE
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRESA')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR,IDEFSA,IRECSA
 9012   FORMAT('IBUGS2,IFOUND,IERROR,IDEFSA,IRECSA = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRESE(IFOUND,IERROR)
C
C     PURPOSE--RESET ALL INTERNAL DATAPLOT SETTINGS
C              (INCLUDING DATA) AS IF ONE HAD SIGNED OFF
C              DATAPLOT AND LOGGED BACK ON.
C     INPUT  ARGUMENTS--NONE
C     OUTPUT ARGUMENTS--IFOUND ('YES' OR 'NO')
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
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1988. (REWRITE) RESET DATA, IO, PC, SU
C     UPDATED         --JANUARY   1992. RESET GRAPHICS=RESET PLOT
C     UPDATED         --JANUARY   1992. RESET I/O=RESET IO
C     UPDATED         --AUGUST    1992. SET PRED & RES TO 10000 OBS
C     UPDATED                           (NOT WORKING)
C     UPDATED         --SEPTEMBER 1993. FIX MAJOR SUBTLE BUG IN FIT
C                                       CAUSED BE REDEFINITION HEREIN
C                                       OF MAXCOL (AND THUS ICOLPR IN
C                                       DPFIT).  NEVER CHANGE
C                                       MAX...<ANYTHING>.
C                                       COMMENT OUT ALL SUCH CHANGES.
C     UPDATED         --SEPTEMBER 1993. RESET CLSB
C     UPDATED         --SEPTEMBER 1993. RESET LIMITS
C     UPDATED         --DECEMBER  2015. FOR WINDOWS, DO NOT RESET THE
C                                       QUICKWIN DEVICE
C     UPDATED         --SEPTEMBER 2016. ALLOW THE USER TO DEFINE
C                                       CERTAIN NAMES THAT WILL NOT
C                                       BE RESET
C     UPDATED         --SEPTEMBER 2016. RESET COMMAND LINE ARGUMENTS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ITEMEC
      CHARACTER*4 ITEMFE
      CHARACTER*4 ITEMPR
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IVALC
C
CCCCC CHARACTER*4 IDEFGC
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCODB.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOSO.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCONP.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOTR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCODG.INC'
      INCLUDE 'DPCOCO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRE'
      ISUBN2='SE  '
C
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRESE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,IFOUND,IERROR,NUMARG
   52   FORMAT('IBUGS2,IFOUND,IERROR,NUMARG = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMARG.GT.0)THEN
          DO55I=1,NUMARG
            WRITE(ICOUT,56)I,IHARG(I)
   56       FORMAT('I,IHARG(I) = ',I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
      ENDIF
C
C               **************************************************
C               **  TREAT THE RESET CASE                        **
C               **************************************************
C
      IFOUND='YES'
C
C               **************************************************
C               **  BRANCH TO THE APPROPRIATE CASE              **
C               **************************************************
C
      ISTEPN='10'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.0)GOTO5100
C
C     2016/09: CHECK FOR "RESET COMMAND LINE ARGUMENTS"
C
      IF(IHARG(1).EQ.'COMM' .AND. IHARG(2).EQ.'LINE' .AND.
     1   IHARG(3).EQ.'ARGU')THEN
        NMACAG=0
        NMACLA=0
        DO101II=1,50
          IMACAR(II)=' '
          IMACLA(II)=' '
  101   CONTINUE
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('ALL COMMAND LINE ARGUMENTS HAVE BEEN CLEARED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ENDIF
C
C     2016/09: CHECK FOR "RESET NO RESET NAME <ON/OFF> FIRST
C
      IF(IHARG(1).EQ.'NO  ' .AND. IHARG(2).EQ.'RESE')THEN
        IF(NUMARG.LE.2)GOTO9000
        IH=IHARG(3)
        IH2=IHARG2(3)
        IVALC='ON'
        IF(NUMARG.GE.4)THEN
          IF(IHARG(4).EQ.'ON' .OR. IHARG(4).EQ.'YES' .OR.
     1       IHARG(4).EQ.'TRUE')THEN
            IVALC='ON'
          ELSE
            IVALC='OFF'
          ENDIF
        ENDIF
C
        DO110II=1,MAXRES
          IF(IH.EQ.IRESN1(II) .AND. IH2.EQ.IRESN2(II))THEN
            IF(IVALC.EQ.'ON')THEN
C
C             NAME ALREADY IN LIST, DO NOTHING
C
              GOTO9000
            ELSE
C
C             REMOVE NAME FROM LIST
C
              IRESN1(II)='    '
              IRESN2(II)='    '
              IF(II.LT.NUMNRE)THEN
                DO120JJ=II+1,NUMNRE
                  IRESN1(JJ-1)=IRESN1(JJ)
                  IRESN2(JJ-1)=IRESN2(JJ)
  120           CONTINUE
                NUMNRE=NUMNRE-1
                IF(IFEEDB.EQ.'ON')THEN
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,123)IH,IH2
  123             FORMAT(A4,A4,' REMOVED FROM THE LIST OF NAMES THAT ',
     1                   'WILL NOT BE RESET')
                 CALL DPWRST('XXX','BUG ')
                ENDIF
              ENDIF
            ENDIF
            GOTO9000
          ENDIF
C
  110   CONTINUE
C
C       ADD NAME TO LIST
C
        IF(NUMNRE.LT.MAXRES)THEN
          NUMNRE=NUMNRE+1
          IRESN1(NUMNRE)=IH
          IRESN2(NUMNRE)=IH2
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,126)IH,IH2
  126       FORMAT(A4,A4,' ADDED TO LIST OF NAMES THAT WILL NOT BE ',
     1             'RESET')
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,128)MAXRES
  128       FORMAT('THE MAXIMUM NUMBER OF NAMES THAT WILL NOT BE ',
     1             'RESET (',I4,') REACHED.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,129)IH,IH2
  129       FORMAT(A4,A4,' WILL NOT BE ADDED TO THE NO RESET LIST.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        GOTO9000
      ENDIF
C
      DO1000I=1,NUMARG
C
      IF(IHARG(I).EQ.'DATA' .OR. IHARG(I).EQ.'DA' .OR.
     1   IHARG(I).EQ.'D')GOTO1100
      IF(IHARG(I).EQ.'VARI' .OR. IHARG(I).EQ.'VA' .OR.
     1   IHARG(I).EQ.'V')GOTO1200
      IF(IHARG(I).EQ.'PARA' .OR. IHARG(I).EQ.'PA' .OR.
     1   IHARG(I).EQ.'P')GOTO1300
      IF(IHARG(I).EQ.'FUNC' .OR. IHARG(I).EQ.'FU' .OR.
     1   IHARG(I).EQ.'F')GOTO1400
      IF(IHARG(I).EQ.'INPU' .OR. IHARG(I).EQ.'IO')GOTO2100
      IF(IHARG(I).EQ.'PLOT' .OR. IHARG(I).EQ.'GRAP' .OR.
     1   IHARG(I).EQ.'PC')GOTO2200
      IF(IHARG(I).EQ.'CLSB' .OR. IHARG(I).EQ.'LCSB')GOTO2300
      IF(IHARG(I).EQ.'LIMI')GOTO2400
      IF(IHARG(I).EQ.'SUPP')GOTO3100
      IF(IHARG(I).EQ.'SU')GOTO3100
      IF(IHARG(I).EQ.'ALL')GOTO5100
      GOTO1000
C
C               **************************************************
C               **  STEP 11--                                   **
C               **  RESET DATA (VARIABLES, PARAM, FUNC)         **
C               **************************************************
C
 1100 CONTINUE
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXNK=MAXOBW
      NK=0
      IDEMXN=MAXOBV
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXN=IDEMXN
      N=0
      IDEMXC=MAXOBW/MAXOBV
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT         SEPTEMBER 1993
CCCCC (CAUSED BIG SUBTLE PROBLEMS ELSEWHERE (E.G., FIT)) SEPT. 1993
CCCCC MAXCOL=IDEMXC
      NUMCOL=0
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXCHF=1000
      NUMCHF=0
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXFUN=100
      NUMFUN=0
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXCHM=200
      NUMCHM=0
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXCON=100
      NUMCON=0
C
      DO1110J=1,NUMNAM
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
CCCCC IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')IN(J)=1
      IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GOTO1110
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
CCCCC IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')IN(J)=1
      IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GOTO1110
      IF(IHNAME(J).EQ.'YPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO1110
      IF(IHNAME(J).EQ.'XPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO1110
      IF(IHNAME(J).EQ.'X2PL'.AND.IHNAM2(J).EQ.'OT  ')GOTO1110
      IF(IHNAME(J).EQ.'TAGP'.AND.IHNAM2(J).EQ.'LOT ')GOTO1110
      IF(IHNAME(J).EQ.'INFI'.AND.IHNAM2(J).EQ.'NITY')GOTO1110
      IF(IHNAME(J).EQ.'PI  '.AND.IHNAM2(J).EQ.'    ')GOTO1110
      IF(NUMNRE.GE.1)THEN
        DO1111K=1,NUMNRE
          IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))
     1      GOTO1110
 1111   CONTINUE
      ENDIF
      IF(IUSE(J).EQ.'V')IN(J)=(-1)
      IF(IUSE(J).EQ.'P')IN(J)=(-1)
      IF(IUSE(J).EQ.'F')IN(J)=(-1)
      IF(IUSE(J).EQ.'M')IN(J)=(-1)
 1110 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
     1IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,
     1IBUGS2,IERROR)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
 1181   FORMAT('ALL USER DATA (VARIABLES, PARAMETERS, FUNCTIONS,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182   FORMAT('AND MATRICES) HAVE JUST BEEN DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  RESET VARIABLES                             **
C               **************************************************
C
 1200 CONTINUE
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXNK=MAXOBW
      NK=0
      IDEMXN=MAXOBV
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXN=IDEMXN
      N=0
      IDEMXC=MAXOBW/MAXOBV
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC (CAUSED BIG SUBTLE PROBLEMS ELSEWHERE (E.G., FIT)) SEPT. 1993
CCCCC MAXCOL=IDEMXC
      NUMCOL=0
C
      DO1210J=1,NUMNAM
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
CCCCC IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')IN(J)=1
      IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GOTO1210
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
CCCCC IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')IN(J)=1
      IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GOTO1210
      IF(IHNAME(J).EQ.'YPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO1210
      IF(IHNAME(J).EQ.'XPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO1210
      IF(IHNAME(J).EQ.'X2PL'.AND.IHNAM2(J).EQ.'OT  ')GOTO1210
      IF(IHNAME(J).EQ.'TAGP'.AND.IHNAM2(J).EQ.'LOT ')GOTO1210
      IF(NUMNRE.GE.1)THEN
        DO1211K=1,NUMNRE
          IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))
     1      GOTO1210
 1211   CONTINUE
      ENDIF
      IF(IUSE(J).EQ.'V')IN(J)=(-1)
 1210 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
     1IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,
     1IBUGS2,IERROR)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1281)
 1281   FORMAT('ALL USER VARIABLES HAVE JUST BEEN DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 13--                                   **
C               **  RESET PARAMETERS                            **
C               **************************************************
C
 1300 CONTINUE
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1310J=1,NUMNAM
      IF(IHNAME(J).EQ.'INFI'.AND.IHNAM2(J).EQ.'NITY')GOTO1310
      IF(IHNAME(J).EQ.'PI  '.AND.IHNAM2(J).EQ.'    ')GOTO1310
      IF(NUMNRE.GE.1)THEN
        DO1311K=1,NUMNRE
          IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))
     1      GOTO1310
 1311   CONTINUE
      ENDIF
      IF(IUSE(J).EQ.'P')IN(J)=(-1)
 1310 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
     1IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,
     1IBUGS2,IERROR)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1381)
 1381   FORMAT('ALL USER PARAMETERS HAVE JUST BEEN DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 14--                                   **
C               **  RESET FUNCTIONS (STRINGS)                   **
C               **************************************************
C
 1400 CONTINUE
      ISTEPN='14'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXCHF=1000
      NUMCHF=0
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXFUN=100
      NUMFUN=0
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
CCCCC MAXCHM=200
      NUMCHM=0
C
      DO1410J=1,NUMNAM
        IF(NUMNRE.GE.1)THEN
          DO1411K=1,NUMNRE
            IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))
     1        GOTO1410
 1411     CONTINUE
        ENDIF
        IF(IUSE(J).EQ.'F')IN(J)=(-1)
 1410 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
     1IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,
     1IBUGS2,IERROR)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1481)
 1481   FORMAT('ALL USER FUNCTIONS (= STRINGS) HAVE JUST BEEN ',
     1         'DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 15--                                   **
C               **  RESET ALL MATRICES                          **
C               **************************************************
C
      ISTEPN='15'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1510J=1,NUMNAM
        IF(NUMNRE.GE.1)THEN
          DO1511K=1,NUMNRE
            IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))
     1        GOTO1510
 1511     CONTINUE
        ENDIF
        IF(IUSE(J).EQ.'M')IN(J)=(-1)
 1510 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
     1IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,
     1IBUGS2,IERROR)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1581)
 1581   FORMAT('ALL USER MATRICES HAVE JUST BEEN DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 21--                                   **
C               **  RESET INPUT/OUTPUT                          **
C               **************************************************
C
 2100 CONTINUE
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISKIP=0
      IFROW1=1
      IFROW2=I1MACH(9)
      IFCOL1=1
      IFCOL2=132
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2181)
 2181   FORMAT('ALL USER INPUT/OUPUT SKIP, ROW, AND COLUMN LIMIT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2182)
 2182   FORMAT('SETTINGS HAVE JUST BEEN SET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2183)
 2183   FORMAT('TO THEIR SIGN-ON DEFAULT STATUS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 22--                                   **
C               **  RESET PLOT CONTROL                          **
C               **************************************************
C
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL INITPC(IBUGS2)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2281)
 2281   FORMAT('ALL USER PLOT CONTROL (LINE, CHARACTER, FONT)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2282)
 2282   FORMAT('SETTINGS HAVE JUST BEEN SET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2283)
 2283   FORMAT('TO THEIR SIGN-ON DEFAULT STATUS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 23--                                   **
C               **  RESET CLSB (CHARACTERS, LINES,              **
C               **  SPIKES, AND BARS)                           **
C               **************************************************
C
 2300 CONTINUE
      ISTEPN='23'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC IDEFGC='BLAC'
C
      DO2310J=1,MAXCH2
      ICHAPA(J)='    '
CCCCC ICHAFO(J)='TEKT'
      ICHACA(J)='UPPE'
      ICHAJU(J)='CECE'
      ICHADI(J)='VERT'
      ICHAFI(J)='OFF'
CCCCC ICHACO(J)='BLAC'
      PCHAHE(J)=2.0
      PCHAWI(J)=1.0
      PCHAVG(J)=0.75
      PCHAHG(J)=0.25
CCCCC PCHATH(J)=0.1
      ACHAAN(J)=0.0
      PCHAHO(J)=0.0
      PCHAVO(J)=0.0
 2310 CONTINUE
C
      DO2320J=1,MAXLN
      ILINPA(J)='SOLI'
CCCCC ILINCO(J)='BLAC'
CCCCC PLINTH(J)=0.1
      PLINLE(J)=1.0
      PLINL2(J)=1.0
      PLINL3(J)=1.0
      PLINGA(J)=1.0
      PLING2(J)=1.0
      PLING3(J)=1.0
 2320 CONTINUE
C
      DO2330J=1,MAXSP
      ISPISW(J)='OFF'
      ISPILI(J)='SOLI'
CCCCC ISPICO(J)='BLAC'
      ISPIDI(J)='V'
CCCCC PSPITH(J)=0.1
      ASPIBA(J)=0.0
 2330 CONTINUE
C
      DO2340J=1,MAXBA
      IBARSW(J)='OFF'
      IBABLI(J)='SOLI'
CCCCC IBABCO(J)='BLAC'
      IBAFSW(J)='OFF'
CCCCC IBAFCO(J)=IDEFGC
      IBAPTY(J)='SOLI'
      IBAPLI(J)='SOLI'
CCCCC IBAPCO(J)=IDEFGC
      IBARTY(J)='2'
      IBARDI(J)='V'
      ABARBA(J)=0.0
      ABARWI(J)=CPUMIN
CCCCC PBABTH(J)=0.1
CCCCC PBAPTH(J)=0.1
      PBAPSP(J)=1.0
 2340 CONTINUE
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2381)
 2381   FORMAT('ALL USER CLSB (= CHARACTER, LINE, SPIKE, & BAR)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2382)
 2382   FORMAT('SETTINGS HAVE JUST BEEN SET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2383)
 2383   FORMAT('TO THEIR SIGN-ON DEFAULT STATUS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 24--                                   **
C               **  RESET LIMITS (ON PLOTS)                     ZZ
C               **************************************************
C
 2400 CONTINUE
      ISTEPN='24'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IX1MIN='FLOA'
      IX1MAX='FLOA'
      IY1MIN='FLOA'
      IY1MAX='FLOA'
      IZ1MIN='FLOA'
      IZ1MAX='FLOA'
C
      IX2MIN='FLOA'
      IX2MAX='FLOA'
      IY2MIN='FLOA'
      IY2MAX='FLOA'
      IZ2MIN='FLOA'
      IZ2MAX='FLOA'
C
      PDXMIN=CPUMIN
      PDXMAX=CPUMAX
      PDYMIN=CPUMIN
      PDYMAX=CPUMAX
      PDZMIN=CPUMIN
      PDZMAX=CPUMAX
C
      PGXMIN=CPUMIN
      PGXMAX=CPUMAX
      PGYMIN=CPUMIN
      PGYMAX=CPUMAX
      PGZMIN=CPUMIN
      PGZMAX=CPUMAX
C
      GX1MIN=CPUMIN
      GX1MAX=CPUMAX
      GY1MIN=CPUMIN
      GY1MAX=CPUMAX
      GZ1MIN=CPUMIN
      GZ1MAX=CPUMAX
C
      GX2MIN=CPUMIN
      GX2MAX=CPUMAX
      GY2MIN=CPUMIN
      GY2MAX=CPUMAX
      GZ2MIN=CPUMIN
      GZ2MAX=CPUMAX
C
      DX1MIN=CPUMIN
      DX1MAX=CPUMAX
      DY1MIN=CPUMIN
      DY1MAX=CPUMAX
      DZ1MIN=CPUMIN
      DZ1MAX=CPUMAX
C
      DX2MIN=CPUMIN
      DX2MAX=CPUMAX
      DY2MIN=CPUMIN
      DY2MAX=CPUMAX
      DZ2MIN=CPUMIN
      DZ2MAX=CPUMAX
C
      FX1MIN=CPUMIN
      FX1MAX=CPUMAX
      FY1MIN=CPUMIN
      FY1MAX=CPUMAX
      FZ1MIN=CPUMIN
      FZ1MAX=CPUMAX
C
      FX2MIN=CPUMIN
      FX2MAX=CPUMAX
      FY2MIN=CPUMIN
      FY2MAX=CPUMAX
      FZ2MIN=CPUMIN
      FZ2MAX=CPUMAX
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2481)
 2481   FORMAT('XLIMITS AND YLIMITS FOR PLOTS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2482)
 2482   FORMAT('HAVE JUST BEEN SET TO FLOAT WITH THE DATA')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
C               **************************************************
C               **  STEP 31--                                   **
C               **  RESET SUPPORT                               **
C               **************************************************
C
 3100 CONTINUE
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITEMEC=IECHO
      ITEMFE=IFEEDB
      ITEMPR=IPRINT
      CALL INITSU(IBUGS2)
C
      IECHO=ITEMEC
      IFEEDB=ITEMFE
      IPRINT=ITEMPR
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3181)
 3181   FORMAT('ALL USER SUPPORT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3182)
 3182   FORMAT('SETTINGS HAVE JUST BEEN SET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3183)
 3183   FORMAT('TO THEIR SIGN-ON DEFAULT STATUS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1000
C
 1000 CONTINUE
      GOTO9000
C
C               **************************************************
C               **  TREAT THE    RESET ALL    CASE              **
C               **  (WILL BE DONE BACK IN THE MAIN ROUNTINE)    **
C               **************************************************
C
 5100 CONTINUE
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5181)
 5181   FORMAT('ALL INTERNAL DATAPLOT SETTINGS HAVE JUST ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5182)
 5182   FORMAT('BEEN SET TO THEIR SIGN-ON DEFAULT STATUS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
C               ************
C               **  EXIT  **
C               ************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRESE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR,NUMARG
 9012   FORMAT('IBUGS2,IFOUND,IERROR,NUMARG = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMARG.GT.0)THEN
          DO9015I=1,NUMARG
            WRITE(ICOUT,9016)I,IHARG(I)
 9016       FORMAT('I,IHARG(I) = ',I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRESI(IHARG,IARGT,ARG,NUMARG,
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
C     PURPOSE--DRAW ONE OR MORE RESISTORS (DEPENDING ON HOW MANY NUMBERS
C              ARE PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
C              OF 0 TO 100.
C     NOTE--THE INPUT COORDINATES DEFINE THE 2 ENDS OF THE RESISTOR.
C     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2 AND THEREFORE
C          THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
C     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN RESISTOR WILL GO
C           FROM THE LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER
C           ABSOLUTE OR RELATIVE) AS DEFINED BY THE 2 NUMBERS.
C     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN RESISTOR WILL GO
C           FROM THE ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST 2
C           NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
C           AS DEFINED BY THE THIRD AND FOURTH NUMBERS.
C     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN RESISTOR WILL GO
C           FROM THE (X,Y) POSITION AS RESULTING FROM THE THIRD AND
C           FOURTH NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR
C           RELATIVE) AS DEFINED BY THE FIFTH AND SIXTH NUMBERS.
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
C     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPRESI
C                                       RATHER THAN DPRES2
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
      DIMENSION PX(1000)
      DIMENSION PY(1000)
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'RESI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRESI--')
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
      IFIG='RESI'
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
          IF(IARGT(I).NE.'NUMB')GOTO1130
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
        CALL DPRES2(X1,Y1,X2,Y2,PX,PY,
     1              IFIG,ILINPA,ILINCO,PLINTH,
     1              AREGBA,IREBLI,IREBCO,PREBTH,
     1              IREFSW,IREFCO,
     1              IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1              PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
        X1=X2
        Y1=Y2
C
        GOTO1160
 1190   CONTINUE
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
 1131 FORMAT('***** ERROR IN RESISTOR (DPRESI)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR THE RESISTOR COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW A RESISTOR ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      FROM THE POINT 20 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      TO THE POINT 40 60')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN THE ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      RESISTOR 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      RESISTOR ABSOLUTE 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('      RESISTOR RELATIVE 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'RESI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRESI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ILOCFN,NUMNUM
 9012   FORMAT('IFOUND,IERROR,ILOCFN,NUMNUM = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)X1,Y1,X2,Y2,X3,Y3
 9013   FORMAT('X1,Y1,X2,Y2,X3,Y3 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)PXSTAR,PYSTAR,PXEND,PYEND
 9015   FORMAT('PXSTAR,PYSTAR,PXEND,PYEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPREST(IFOUND,IERROR)
C
C     PURPOSE--RESTORE (= READ IN TO MEMORY) ALL INTERNAL DATAPLOT
C              SETTINGS.  THE MASS STORAGE FILE
C              IS DESIGNATED BY THE ANALYST.
C              THIS IS USEFUL WHEN A RUN MUST BE
C              INTERRUPTED (E.G., LUNCH) (SEE THE SAVE COMMAND)
C              AND IT IS DESIRED
C              TO PICK UP THE RUN LATER AT THE POINT
C              OF INTERRUPTION (SEE THE RESTORE COMMAND).
C     NOTE--THE SAVE COMMAND (AND ITS COMPLEMENT, THE RESTORE COMMAND)
C           BOTH USE UNFORMATTED FORTRAN I/O STATEMENTS
C           (FOR SPEED AND EFFICIENCY).
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
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --JANUARY   1982.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1985.
C     UPDATED         --JUNE      1986.
C     UPDATED         --NOVEMBER  1987.  DIMENSION FOR I1DATA--1100 TO 100
C     UPDATED         --DECEMBER  1987.  DIMENSION FOR V--10000 TO MAXOBW
C     UPDATED         --JANUARY   1989.  SOFT-CODE ALL (ALAN)
C     UPDATED         --OCTOBER   1991.  SUN HAS LIMIT ON # OF WORDS
C                                        FOR UNFORMATTED I/O (2,046)
C     UPDATED         --APRIL     1992.  INCLUDE DPCO3D.INC (ALAN)
C     UPDATED         --APRIL     1992.  PPEDHE TO APEDSZ (ALAN)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
CCCCC CHARACTER*80 ICANS
      CHARACTER (LEN=MAXSTR) :: ICANS
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCODB.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOSO.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCONP.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOTR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCODG.INC'
      INCLUDE 'DPCOCO.INC'
CCCCC FOLLOWING LINE WAS ADDED  APRIL 1992 (ALAN)
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRE'
      ISUBN2='ST  '
      ISUBRO='-999'
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'REST')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPREST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR
   53 FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IWIDTH
   54 FORMAT('IWIDTH = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,55)(IANSLC(I),I=1,IWIDTH)
   55 FORMAT('(IANSLC(I),I=1,IWIDTH) = ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)ISAVNU
   61 FORMAT('ISAVNU = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)ISAVNA(1:80)
   62 FORMAT('ISAVNA = ',A80)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)ISAVST
   63 FORMAT('ISAVST = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,64)ISAVFO
   64 FORMAT('ISAVFO = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,65)ISAVAC
   65 FORMAT('ISAVAC = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,66)ISAVFO
   66 FORMAT('ISAVFO = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,67)ISAVCS
   67 FORMAT('ISAVCS = ',A12)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=ISAVNU
      IFILE=ISAVNA
      ISTAT=ISAVST
      IFORM=ISAVFO
      IACCES=ISAVAC
      IPROT=ISAVPR
      ICURST=ISAVCS
C
      ISUBN0='REST'
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'REST')GOTO1199
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
C               *******************************************
C               **  STEP 12--                            **
C               **  CHECK TO SEE IF SAVE FILE MAY EXIST  **
C               *******************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')GOTO1200
      GOTO1290
 1200 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** ERROR IN DPREST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      THE DESIRED RESTORE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1213)
 1213 FORMAT('      CANNOT BE GIVEN BECAUSE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)
 1214 FORMAT('      THE REQUIRED SYSTEM MASS STORAGE FILE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('      WHICH STORES SUCH SAVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1216)
 1216 FORMAT('      IS NOT AVAILABLE AT THIS INSTALLATION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1217)ISTAT,ISAVST
 1217 FORMAT('ISTAT,ISAVST = ',A12,2X,A12)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1290 CONTINUE
C
C               ****************************
C               **  STEP 13--             **
C               **  EXTRACT THE FILE NAME **
C               **  (THE THIRD WORD)      **
C               ****************************
C
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1310I=1,MAXFNC
      IFILE(I:I)=' '
 1310 CONTINUE
C
      DO1320I=1,MAXSTR
      ICANS(I:I)=IANSLC(I)
 1320 CONTINUE
C
      ISTART=1
      ISTOP=IWIDTH
      IF(NUMARG.LE.1)
     1CALL DPW280(ICANS,ISTART,ISTOP,ICOL3,IBUGS2,ISUBRO,IERROR)
      IF(NUMARG.GE.2)
     1CALL DPW380(ICANS,ISTART,ISTOP,ICOL3,IBUGS2,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      J=0
      IF(ICOL3.GT.IWIDTH)GOTO1339
      DO1330I=ICOL3,IWIDTH
      J=J+1
      IFILE(J:J)=ICANS(I:I)
 1330 CONTINUE
 1339 CONTINUE
C
      NMAX=80
      CALL DPDB80(IFILE,JMAX,NMAX,IBUGS2,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      NCFILE=JMAX
C
      IF(NCFILE.GE.1)GOTO1349
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1341)
 1341 FORMAT('***** ERROR IN DPREST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1342)
 1342 FORMAT('      A FILE NAME IS REQUIRED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1343)
 1343 FORMAT('      IN THE RESTORE COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1344)
 1344 FORMAT('      (FOR EXAMPLE,    RESTORE MEMORY DPRUN.DAT)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1345)
 1345 FORMAT('      BUT NONE WAS GIVEN HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1346)
 1346 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,1347)(IANSLC(I),I=1,IWIDTH)
 1347 FORMAT('      ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.LE.0)WRITE(ICOUT,999)
      IF(IWIDTH.LE.0)CALL DPWRST('XXX','BUG ')
      GOTO9000
 1349 CONTINUE
C
C               *********************
C               **  STEP 31--      **
C               **  OPEN THE FILE  **
C               *********************
C
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
C               ****************************************************************
C               **  STEP 41--
C               **  READ  IN  FROM THE SAVE FILE;
C               ****************************************************************
C
      ISTEPN='41'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     -----BEGIN READING IN-----------------------
C
C     -----READ IN COMMON FOR STANDARD I/O-----
C
      READ(IOUNIT)IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      READ(IOUNIT)IFEEDB,IPRINT
C
C     -----READ IN COMMON FOR MACHINE CONSTANTS-----
C
      READ(IOUNIT)(I1MACH(I),I=1,16)
      READ(IOUNIT)(R1MACH(I),I=1,5)
      READ(IOUNIT)(D1MACH(I),I=1,5)
C
C     -----READ IN COMMON FOR BUGS-----
C
      READ(IOUNIT)(I1BUG(I),I=1,10)
      READ(IOUNIT)(IH1BUG(I),I=1,100)
C
C     -----READ IN COMMON FOR HOUSEKEEPING-----
C
CCCCC READ(IOUNIT)(I1HOUS(I),I=1,1050)
      READ(IOUNIT)(I1HOUS(I),I=1,5*MAXSTR+50)
C     READ(IOUNIT)(IH1HOU(I),I=1,2320)
      READ(IOUNIT)(IH1HOU(I),I=1,11*MAXSTR+120)
C     READ(IOUNIT)(R1HOUS(I),I=1,400)
      READ(IOUNIT)(R1HOUS(I),I=1,2*MAXSTR)
C
C     -----READ IN COMMON FOR DATA-----
C
C  OCTOBER 1991.  FOLLOWING BLOCK OF CODE HEAVILY MODIFIED TO HANDLE
C  PROBLEM ON SUN.  SUN APPEARS TO LIMIT UNFORMATTED I/O TO 2,046 WORDS.
C  NEED TO BREAK INTO CHUNKS FOR MANY OF THESE WRITE OPERATIONS.
C
      MAXWRD=100000
      IF(IHOST1.EQ.'SUN')MAXWRD=2046
      NLOOP1=(MAXOBV/MAXWRD)+1
      NLOOP2=(MAXPOP/MAXWRD)+1
      NLOOP3=(MAXOBW/MAXWRD)+1
C
CCCCC READ(IOUNIT)(I1DATA(I),I=1,1100)
CCCCC READ(IOUNIT)(I1DATA(I),I=1,MAXOBS+100)
      READ(IOUNIT)(I1DATA(I),I=1,100)
CCCCC READ(IOUNIT)(ISUB(I),I=1,MAXOBV)
      DO9112IK=1,NLOOP1
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXOBV)GOTO9117
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXOBV)JSTOP=MAXOBV
      READ(IOUNIT)(ISUB(I),I=JSTART,JSTOP)
 9112 CONTINUE
 9117 CONTINUE
CCCCC READ(IOUNIT)(IH1DAT(I),I=1,3500)
CCCCC READ(IOUNIT)(IH1DAT(I),I=1,3*MAXF1+3*MAXFN2+MAXF3)
      READ(IOUNIT)(IPARNC(I),I=1,MAXFN2)
      READ(IOUNIT)(IPANC2(I),I=1,MAXFN2)
      READ(IOUNIT)(IPAROP(I),I=1,MAXFN2)
      READ(IOUNIT)(MODEL(I),I=1,MAXF3)
      READ(IOUNIT)(IFUNC(I),I=1,MAXF1)
      READ(IOUNIT)(IFUNC2(I),I=1,MAXF1)
      READ(IOUNIT)(IFUNC3(I),I=1,MAXF1)
      READ(IOUNIT)(PARLIM(I),I=1,100)
CCCCC READ(IOUNIT)(PRED(I),I=1,MAXOBV)
      DO9122IK=1,NLOOP1
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXOBV)GOTO9127
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXOBV)JSTOP=MAXOBV
      READ(IOUNIT)(PRED(I),I=JSTART,JSTOP)
 9122 CONTINUE
 9127 CONTINUE
CCCCC READ(IOUNIT)(RES(I),I=1,MAXOBV)
      DO9132IK=1,NLOOP1
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXOBV)GOTO9137
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXOBV)JSTOP=MAXOBV
      READ(IOUNIT)(RES(I),I=JSTART,JSTOP)
 9132 CONTINUE
 9137 CONTINUE
CCCCC READ(IOUNIT)(Y(I),I=1,MAXPOP)
      DO9142IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9147
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      READ(IOUNIT)(Y(I),I=JSTART,JSTOP)
 9142 CONTINUE
 9147 CONTINUE
CCCCC READ(IOUNIT)(X(I),I=1,MAXPOP)
      DO9152IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9157
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      READ(IOUNIT)(X(I),I=JSTART,JSTOP)
 9152 CONTINUE
 9157 CONTINUE
CCCCC READ(IOUNIT)(X3D(I),I=1,MAXPOP)
      DO9162IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9167
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      READ(IOUNIT)(X3D(I),I=JSTART,JSTOP)
 9162 CONTINUE
 9167 CONTINUE
CCCCC READ(IOUNIT)(D(I),I=1,MAXPOP)
      DO9172IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9177
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      READ(IOUNIT)(D(I),I=JSTART,JSTOP)
 9172 CONTINUE
 9177 CONTINUE
CCCCC READ(IOUNIT)(YPLOT(I),I=1,MAXPOP)
      DO9182IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9187
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      READ(IOUNIT)(YPLOT(I),I=JSTART,JSTOP)
 9182 CONTINUE
 9187 CONTINUE
CCCCC READ(IOUNIT)(XPLOT(I),I=1,MAXPOP)
      DO9192IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9197
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      READ(IOUNIT)(XPLOT(I),I=JSTART,JSTOP)
 9192 CONTINUE
 9197 CONTINUE
CCCCC READ(IOUNIT)(X2PLOT(I),I=1,MAXPOP)
      DO9212IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9217
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      READ(IOUNIT)(X2PLOT(I),I=JSTART,JSTOP)
 9212 CONTINUE
 9217 CONTINUE
CCCCC READ(IOUNIT)(TAGPLO(I),I=1,MAXPOP)
      DO9222IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9227
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      READ(IOUNIT)(TAGPLO(I),I=JSTART,JSTOP)
 9222 CONTINUE
 9227 CONTINUE
CCCCC READ(IOUNIT)(V(I),I=1,MAXOBW)
      DO9232IK=1,NLOOP3
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXOBW)GOTO9237
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXOBW)JSTOP=MAXOBW
      READ(IOUNIT)(V(I),I=JSTART,JSTOP)
 9232 CONTINUE
 9237 CONTINUE
CCCCC READ(IOUNIT)((AMATR1(I,J),I=1,100),J=1,100)
      ITEMP=100*100
      IF(ITEMP.LE.MAXWRD)THEN
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=1,100)
      ELSE
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=1,10)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=11,20)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=21,30)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=31,40)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=41,50)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=51,60)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=61,70)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=71,80)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=81,90)
        READ(IOUNIT)((AMATR1(I,J),I=1,100),J=91,100)
      END IF
CCCCC READ(IOUNIT)(R1DATA(I),I=1,10200)
CCCCC READ(IOUNIT)(R1DATA(I),I=1,42200)
CCCCC READ(IOUNIT)(R1DATA(I),I=1,2*MAXOBS+8*MAXPLP+200)
CCCCC READ(IOUNIT)(V(I),I=1,10000)
CCCCC READ(IOUNIT)(V(I),I=1,MAXWS)
C
C     -----READ IN COMMON FOR SUPPORT-----
C
      READ(IOUNIT)(I1SUPP(I),I=1,50)
      READ(IOUNIT)(IH1SUP(I),I=1,70)
      READ(IOUNIT)(R1SUPP(I),I=1,60)
C
C     -----READ IN COMMON FOR SUBFILE I/O (UNIVAC ONLY)-----
C
      READ(IOUNIT)(IBUF(I),I=1,504)
C
C     -----READ IN COMMON FOR DIAGRAMMATIC GRAPHICS-----
C
      READ(IOUNIT)(IH1DIA(I),I=1,40)
      READ(IOUNIT)(R1DIAG(I),I=1,40)
C
C     -----READ IN COMMON FOR COLOR-----
C
      READ(IOUNIT)ICOLOR
      READ(IOUNIT)IPLOTF
C
C     -----READ IN COMMON FOR BUGS AND ERROR-----
C
      READ(IOUNIT)IBUGG4
      READ(IOUNIT)ISUBG4
      READ(IOUNIT)IERRG4
C
C     -----READ IN COMMON FOR HOST-----
C
      READ(IOUNIT)IHOST1
      READ(IOUNIT)IHOST2
      READ(IOUNIT)IHMOD1
      READ(IOUNIT)IHMOD2
      READ(IOUNIT)IOPSY1
      READ(IOUNIT)IOPSY2
      READ(IOUNIT)ICOMPI
      READ(IOUNIT)ISITE
C
C     -----READ IN COMMON FOR TRANSLATOR-----
C
      READ(IOUNIT)ITRANS
      READ(IOUNIT)NCTRA1
      READ(IOUNIT)NCTRA2
      READ(IOUNIT)NUMTRA
      READ(IOUNIT)ICTRA1
      READ(IOUNIT)ICTRA2
C
C     -----READ IN COMMON FOR NON-PRINTING CHARACTERS-----
C
      READ(IOUNIT)INULC
      READ(IOUNIT)ISOHC
      READ(IOUNIT)ISTXC
      READ(IOUNIT)IETXC
      READ(IOUNIT)IEOTC
      READ(IOUNIT)IENQC
      READ(IOUNIT)IACKC
      READ(IOUNIT)IBELC
      READ(IOUNIT)IBSC
      READ(IOUNIT)IHTC
      READ(IOUNIT)ILFC
      READ(IOUNIT)IVTC
      READ(IOUNIT)IFFC
      READ(IOUNIT)ICRC
      READ(IOUNIT)ISOC
      READ(IOUNIT)ISIC
      READ(IOUNIT)IDLEC
      READ(IOUNIT)IDC1C
      READ(IOUNIT)IDC2C
      READ(IOUNIT)IDC3C
      READ(IOUNIT)IDC4C
      READ(IOUNIT)INAKC
      READ(IOUNIT)ISYNC
      READ(IOUNIT)IETBC
      READ(IOUNIT)ICANC
      READ(IOUNIT)IEMC
      READ(IOUNIT)ISUBC
      READ(IOUNIT)IESCC
      READ(IOUNIT)IFSC
      READ(IOUNIT)IGSC
      READ(IOUNIT)IRSC
      READ(IOUNIT)IUSC
C
C     -----READ IN COMMON FOR GRAPHICS-----
C
      READ(IOUNIT)IMANUF
      READ(IOUNIT)IMODEL
      READ(IOUNIT)IMODE2
      READ(IOUNIT)IMODE3
      READ(IOUNIT)IGCODE
      READ(IOUNIT)IGUNIT
      READ(IOUNIT)IGCONT
      READ(IOUNIT)NUMHPP
      READ(IOUNIT)NUMVPP
      READ(IOUNIT)ANUMHP
      READ(IOUNIT)ANUMVP
      READ(IOUNIT)IGCOLO
      READ(IOUNIT)IGBAUD
      READ(IOUNIT)AGERDE
      READ(IOUNIT)AGCODE
      READ(IOUNIT)ISOFT
      READ(IOUNIT)ISOFT2
      READ(IOUNIT)ISOFT3
C
C     -----READ IN COMMON FOR FILE OPERATIONS-----
C
      READ(IOUNIT)(I1FILO(I),I=1,10)
      READ(IOUNIT)(IH1FIL(I),I=1,200)
C
C     -----READ IN COMMON FOR FILE OPERATIONS, PART 2-----
C
      READ(IOUNIT)IMESNU
      READ(IOUNIT)IMESNA
      READ(IOUNIT)IMESST
      READ(IOUNIT)IMESFO
      READ(IOUNIT)IMESAC
      READ(IOUNIT)IMESPR
      READ(IOUNIT)IMESCS
C
      READ(IOUNIT)INEWNU
      READ(IOUNIT)INEWNA
      READ(IOUNIT)INEWST
      READ(IOUNIT)INEWFO
      READ(IOUNIT)INEWAC
      READ(IOUNIT)INEWPR
      READ(IOUNIT)INEWCS
C
      READ(IOUNIT)IMAINU
      READ(IOUNIT)IMAINA
      READ(IOUNIT)IMAIST
      READ(IOUNIT)IMAIFO
      READ(IOUNIT)IMAIAC
      READ(IOUNIT)IMAIPR
      READ(IOUNIT)IMAICS
C
      READ(IOUNIT)IHELNU
      READ(IOUNIT)IHELNA
      READ(IOUNIT)IHELST
      READ(IOUNIT)IHELFO
      READ(IOUNIT)IHELAC
      READ(IOUNIT)IHELPR
      READ(IOUNIT)IHELCS
C
      READ(IOUNIT)IBUGNU
      READ(IOUNIT)IBUGNA
      READ(IOUNIT)IBUGST
      READ(IOUNIT)IBUGFO
      READ(IOUNIT)IBUGAC
      READ(IOUNIT)IBUGPR
      READ(IOUNIT)IBUGCS
C
      READ(IOUNIT)IQUENU
      READ(IOUNIT)IQUENA
      READ(IOUNIT)IQUEST
      READ(IOUNIT)IQUEFO
      READ(IOUNIT)IQUEAC
      READ(IOUNIT)IQUEPR
      READ(IOUNIT)IQUECS
C
      READ(IOUNIT)ILOGNU
      READ(IOUNIT)ILOGNA
      READ(IOUNIT)ILOGST
      READ(IOUNIT)ILOGFO
      READ(IOUNIT)ILOGAC
      READ(IOUNIT)ILOGPR
      READ(IOUNIT)ILOGCS
C
      READ(IOUNIT)IREANU
      READ(IOUNIT)IREANA
      READ(IOUNIT)IREAST
      READ(IOUNIT)IREAFO
      READ(IOUNIT)IREAAC
      READ(IOUNIT)IREAPR
      READ(IOUNIT)IREACS
C
      READ(IOUNIT)IWRINU
      READ(IOUNIT)IWRINA
      READ(IOUNIT)IWRIST
      READ(IOUNIT)IWRIFO
      READ(IOUNIT)IWRIAC
      READ(IOUNIT)IWRIPR
      READ(IOUNIT)IWRICS
C
      READ(IOUNIT)ISAVNU
      READ(IOUNIT)ISAVNA
      READ(IOUNIT)ISAVST
      READ(IOUNIT)ISAVFO
      READ(IOUNIT)ISAVAC
      READ(IOUNIT)ISAVPR
      READ(IOUNIT)ISAVCS
C
      READ(IOUNIT)ILISNU
      READ(IOUNIT)ILISNA
      READ(IOUNIT)ILISST
      READ(IOUNIT)ILISFO
      READ(IOUNIT)ILISAC
      READ(IOUNIT)ILISPR
      READ(IOUNIT)ILISCS
C
      READ(IOUNIT)ICRENU
      READ(IOUNIT)ICRENA
      READ(IOUNIT)ICREST
      READ(IOUNIT)ICREFO
      READ(IOUNIT)ICREAC
      READ(IOUNIT)ICREPR
      READ(IOUNIT)ICRECS
C
      READ(IOUNIT)ISCRNU
      READ(IOUNIT)ISCRNA
      READ(IOUNIT)ISCRST
      READ(IOUNIT)ISCRFO
      READ(IOUNIT)ISCRAC
      READ(IOUNIT)ISCRPR
      READ(IOUNIT)ISCRCS
C
      READ(IOUNIT)IDATNU
      READ(IOUNIT)IDATNA
      READ(IOUNIT)IDATST
      READ(IOUNIT)IDATFO
      READ(IOUNIT)IDATAC
      READ(IOUNIT)IDATPR
      READ(IOUNIT)IDATCS
C
      READ(IOUNIT)IPL1NU
      READ(IOUNIT)IPL1NA
      READ(IOUNIT)IPL1ST
      READ(IOUNIT)IPL1FO
      READ(IOUNIT)IPL1AC
      READ(IOUNIT)IPL1PR
      READ(IOUNIT)IPL1CS
C
      READ(IOUNIT)IPL2NU
      READ(IOUNIT)IPL2NA
      READ(IOUNIT)IPL2ST
      READ(IOUNIT)IPL2FO
      READ(IOUNIT)IPL2AC
      READ(IOUNIT)IPL2PR
      READ(IOUNIT)IPL2CS
C
      READ(IOUNIT)IPRONU
      READ(IOUNIT)IPRONA
      READ(IOUNIT)IPROST
      READ(IOUNIT)IPROFO
      READ(IOUNIT)IPROAC
      READ(IOUNIT)IPROPR
      READ(IOUNIT)IPROCS
C
      READ(IOUNIT)ICONNU
      READ(IOUNIT)ICONNA
      READ(IOUNIT)ICONST
      READ(IOUNIT)ICONFO
      READ(IOUNIT)ICONAC
      READ(IOUNIT)ICONPR
      READ(IOUNIT)ICONCS
C
      READ(IOUNIT)ISACNU
      READ(IOUNIT)ISACNA
      READ(IOUNIT)ISACST
      READ(IOUNIT)ISACFO
      READ(IOUNIT)ISACAC
      READ(IOUNIT)ISACPR
      READ(IOUNIT)ISACCS
C
      READ(IOUNIT)IEX1NU
      READ(IOUNIT)IEX1NA
      READ(IOUNIT)IEX1ST
      READ(IOUNIT)IEX1FO
      READ(IOUNIT)IEX1AC
      READ(IOUNIT)IEX1PR
      READ(IOUNIT)IEX1CS
C
      READ(IOUNIT)IEX2NU
      READ(IOUNIT)IEX2NA
      READ(IOUNIT)IEX2ST
      READ(IOUNIT)IEX2FO
      READ(IOUNIT)IEX2AC
      READ(IOUNIT)IEX2PR
      READ(IOUNIT)IEX2CS
C
      READ(IOUNIT)IEX3NU
      READ(IOUNIT)IEX3NA
      READ(IOUNIT)IEX3ST
      READ(IOUNIT)IEX3FO
      READ(IOUNIT)IEX3AC
      READ(IOUNIT)IEX3PR
      READ(IOUNIT)IEX3CS
C
      READ(IOUNIT)IEX4NU
      READ(IOUNIT)IEX4NA
      READ(IOUNIT)IEX4ST
      READ(IOUNIT)IEX4FO
      READ(IOUNIT)IEX4AC
      READ(IOUNIT)IEX4PR
      READ(IOUNIT)IEX4CS
C
      READ(IOUNIT)IEX5NU
      READ(IOUNIT)IEX5NA
      READ(IOUNIT)IEX5ST
      READ(IOUNIT)IEX5FO
      READ(IOUNIT)IEX5AC
      READ(IOUNIT)IEX5PR
      READ(IOUNIT)IEX5CS
C
      READ(IOUNIT)IFCHAR
C
C     -----READ IN COMMON FOR PLOT CONTROL-----
C
      READ(IOUNIT)(IDMANU(I),I=1,MAXDV)
      READ(IOUNIT)(IDMODE(I),I=1,MAXDV)
      READ(IOUNIT)(IDMOD2(I),I=1,MAXDV)
      READ(IOUNIT)(IDMOD3(I),I=1,MAXDV)
      READ(IOUNIT)(IDPOWE(I),I=1,MAXDV)
      READ(IOUNIT)(IDCONT(I),I=1,MAXDV)
      READ(IOUNIT)(IDCOLO(I),I=1,MAXDV)
      READ(IOUNIT)(IDSCRE(I),I=1,MAXDV)
      READ(IOUNIT)(IDSCRO(I),I=1,MAXDV)
      READ(IOUNIT)(IDPAER(I),I=1,MAXDV)
      READ(IOUNIT)(IDSEGM(I),I=1,MAXDV)
      READ(IOUNIT)(IDSOFT(I),I=1,MAXDV)
      READ(IOUNIT)(IDSOF2(I),I=1,MAXDV)
      READ(IOUNIT)(IDSOF3(I),I=1,MAXDV)
C
      READ(IOUNIT)(IDCODE(I),I=1,MAXDV)
      READ(IOUNIT)(IDUNIT(I),I=1,MAXDV)
      READ(IOUNIT)(IDNHPP(I),I=1,MAXDV)
      READ(IOUNIT)(IDNVPP(I),I=1,MAXDV)
      READ(IOUNIT)(IDBAUD(I),I=1,MAXDV)
      READ(IOUNIT)NUMDEV,MAXDEV
C
      READ(IOUNIT)IERASW,IBELSW,ISORSW,ICOPSW
      READ(IOUNIT)IPENSW
      READ(IOUNIT)IBACCO,IMARCO
      READ(IOUNIT)IDEFXC,IDEFBK,IDEFMC,IDEPEC
      READ(IOUNIT)ISEQSW
      READ(IOUNIT)IFENSW
      READ(IOUNIT)INEGSW
      READ(IOUNIT)IVISSW,IPEDSW,IPEDCO
      READ(IOUNIT)IDEFMA,IDEFMO,IDEFM2,IDEFM3
      READ(IOUNIT)IDEFPO,IDEFCN,IDEFDC
C
      READ(IOUNIT)NUMRIN,NUMCOP
      READ(IOUNIT)NUMSEQ
      READ(IOUNIT)IDEFVP,IDEFHP,IDEFUN
C
      READ(IOUNIT)BAWIDT,BARSPA,DEFBAS
      READ(IOUNIT)AORIXC,AORIYC,AORIZC
      READ(IOUNIT)AEYEXC,AEYEYC,AEYEZC
CCCCC THE FOLLOWING LINE WAS FIXED   APRIL 1992 (ALAN)
CCCCC READ(IOUNIT)PPEDHE
      READ(IOUNIT)APEDSZ
      READ(IOUNIT)DEFSZ,DEFTL
C
      READ(IOUNIT)IGRASW
C
      READ(IOUNIT)PGRAXO,PGRAYO,PGRAXC,PGRAYC,PGRAXN,PGRAYN
      READ(IOUNIT)PMARXC
      READ(IOUNIT)PGRAXF,PGRAYF
      READ(IOUNIT)PCROXC,PCROYC
C
      READ(IOUNIT)IDIASW
C
      READ(IOUNIT)PDIAXC,PDIAYC,PDIAX2,PDIAY2
      READ(IOUNIT)PDIAHE,PDIAWI,PDIAVG,PDIAHG
C
      READ(IOUNIT)PWXMIN,PWXMAX,PWYMIN,PWYMAX
      READ(IOUNIT)WWXMIN,WWXMAX,WWYMIN,WWYMAX
C
      READ(IOUNIT)IX1MIN,IX1MAX,IY1MIN,IY1MAX
      READ(IOUNIT)IX2MIN,IX2MAX,IY2MIN,IY2MAX
C
      READ(IOUNIT)PXMIN,PXMAX,PYMIN,PYMAX
      READ(IOUNIT)PDXMIN,PDXMAX,PDYMIN,PDYMAX
      READ(IOUNIT)PGXMIN,PGXMAX,PGYMIN,PGYMAX
      READ(IOUNIT)GX1MIN,GX1MAX,GY1MIN,GY1MAX
      READ(IOUNIT)GX2MIN,GX2MAX,GY2MIN,GY2MAX
      READ(IOUNIT)DX1MIN,DX1MAX,DY1MIN,DY1MAX
      READ(IOUNIT)DX2MIN,DX2MAX,DY2MIN,DY2MAX
      READ(IOUNIT)FX1MIN,FX1MAX,FY1MIN,FY1MAX
      READ(IOUNIT)FX2MIN,FX2MAX,FY2MIN,FY2MAX
C
      READ(IOUNIT)IX1FSW,IX2FSW,IY1FSW,IY2FSW
      READ(IOUNIT)IX1FPA,IX2FPA,IY1FPA,IY2FPA
      READ(IOUNIT)IX1FCO,IX2FCO,IY1FCO,IY2FCO
C
      READ(IOUNIT)PFRATH
C
      READ(IOUNIT)IX1TSW,IX2TSW,IY1TSW,IY2TSW
      READ(IOUNIT)IX1JSW,IX2JSW,IY1JSW,IY2JSW
      READ(IOUNIT)IX1NSW,IX2NSW,IY1NSW,IY2NSW
      READ(IOUNIT)IX1TSC,IX2TSC,IY1TSC,IY2TSC
      READ(IOUNIT)IX1TJU,IX2TJU,IY1TJU,IY2TJU
      READ(IOUNIT)IX1TCO,IX2TCO,IY1TCO,IY2TCO
C
      READ(IOUNIT)NMJX1T,NMJX2T,NMJY1T,NMJY2T
      READ(IOUNIT)NMNX1T,NMNX2T,NMNY1T,NMNY2T
      READ(IOUNIT)NX1COO,NX2COO,NY1COO,NY2COO
      READ(IOUNIT)NX1CMN,NX2CMN,NY1CMN,NY2CMN
      READ(IOUNIT)MAXTIC
C
      READ(IOUNIT)(PX1COO(I),I=1,MAXTC)
      READ(IOUNIT)(PX2COO(I),I=1,MAXTC)
      READ(IOUNIT)(PY1COO(I),I=1,MAXTC)
      READ(IOUNIT)(PY2COO(I),I=1,MAXTC)
      READ(IOUNIT)(X1COOR(I),I=1,MAXTC)
      READ(IOUNIT)(X2COOR(I),I=1,MAXTC)
      READ(IOUNIT)(Y1COOR(I),I=1,MAXTC)
      READ(IOUNIT)(Y2COOR(I),I=1,MAXTC)
      READ(IOUNIT)(PX1CMN(I),I=1,MAXTC)
      READ(IOUNIT)(PX2CMN(I),I=1,MAXTC)
      READ(IOUNIT)(PY1CMN(I),I=1,MAXTC)
      READ(IOUNIT)(PY2CMN(I),I=1,MAXTC)
      READ(IOUNIT)(X1COMN(I),I=1,MAXTC)
      READ(IOUNIT)(X2COMN(I),I=1,MAXTC)
      READ(IOUNIT)(Y1COMN(I),I=1,MAXTC)
      READ(IOUNIT)(Y2COMN(I),I=1,MAXTC)
      READ(IOUNIT)PX1TLE,PX2TLE,PY1TLE,PY2TLE
      READ(IOUNIT)PTICTH,PMNTFA
C
      READ(IOUNIT)IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW
      READ(IOUNIT)IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO
      READ(IOUNIT)IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA
      READ(IOUNIT)IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU
      READ(IOUNIT)IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI
      READ(IOUNIT)IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI
      READ(IOUNIT)IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO
C
      READ(IOUNIT)IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP
C
      READ(IOUNIT)PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS
      READ(IOUNIT)AX1ZAN,AX2ZAN,AY1ZAN,AY2ZAN
      READ(IOUNIT)PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG
      READ(IOUNIT)PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG
      READ(IOUNIT)PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG
      READ(IOUNIT)PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG
      READ(IOUNIT)PTIZTH
C
      READ(IOUNIT)IVGRSW,IHGRSW
      READ(IOUNIT)IVGRPA,IHGRPA
      READ(IOUNIT)IVGRCO,IHGRCO
C
      READ(IOUNIT)PVGRTH,PHGRTH
C
      READ(IOUNIT)(ITITTE(I),I=1,130)
      READ(IOUNIT)ITITFO,ITITCA,ITITFI,ITITCO
C
      READ(IOUNIT)NCTITL
C
      READ(IOUNIT)PTITHE,PTITWI,PTITVG,PTITHG,PTITTH,PTITDS
C
      READ(IOUNIT)(IX1LTE(I),I=1,MAXCH)
      READ(IOUNIT)IX1LFO,IX1LCA,IX1LFI,IX1LCO
      READ(IOUNIT)(IX2LTE(I),I=1,MAXCH)
      READ(IOUNIT)IX2LFO,IX2LCA,IX2LFI,IX2LCO
      READ(IOUNIT)(IX3LTE(I),I=1,MAXCH)
      READ(IOUNIT)IX3LFO,IX3LCA,IX3LFI,IX3LCO
      READ(IOUNIT)(IY1LTE(I),I=1,MAXCH)
      READ(IOUNIT)IY1LFO,IY1LCA,IY1LFI,IY1LCO
      READ(IOUNIT)(IY2LTE(I),I=1,MAXCH)
      READ(IOUNIT)IY2LFO,IY2LCA,IY2LFI,IY2LCO
C
      READ(IOUNIT)NCX1LA,NCX2LA,NCX3LA,NCY1LA,NCY2LA
C
      READ(IOUNIT)PX1LHE,PX1LWI,PX1LVG,PX1LHG,PX1LTH,PX1LDS
      READ(IOUNIT)PX2LHE,PX2LWI,PX2LVG,PX2LHG,PX2LTH,PX2LDS
      READ(IOUNIT)PX3LHE,PX3LWI,PX3LVG,PX3LHG,PX3LTH,PX3LDS
      READ(IOUNIT)PY1LHE,PY1LWI,PY1LVG,PY1LHG,PY1LTH,PY1LDS
      READ(IOUNIT)PY2LHE,PY2LWI,PY2LVG,PY2LHG,PY2LTH,PY2LDS
C
      READ(IOUNIT)(ILEGTE(I),I=1,MAXLG2)
      READ(IOUNIT)(ILEGFO(I),I=1,MAXLG)
      READ(IOUNIT)(ILEGCA(I),I=1,MAXLG)
      READ(IOUNIT)(ILEGJU(I),I=1,MAXLG)
      READ(IOUNIT)(ILEGDI(I),I=1,MAXLG)
      READ(IOUNIT)(ILEGFI(I),I=1,MAXLG)
      READ(IOUNIT)(ILEGCO(I),I=1,MAXLG)
      READ(IOUNIT)(ILEGNA(I),I=1,MAXLG)
C
      READ(IOUNIT)(ILEGST(I),I=1,MAXLG)
      READ(IOUNIT)(ILEGSP(I),I=1,MAXLG)
      READ(IOUNIT)NCLEG,MXCLEG
      READ(IOUNIT)NUMLEG,MAXLEG
C
      READ(IOUNIT)(PLEGXC(I),I=1,MAXLG)
      READ(IOUNIT)(PLEGYC(I),I=1,MAXLG)
      READ(IOUNIT)(PLEGHE(I),I=1,MAXLG)
      READ(IOUNIT)(PLEGWI(I),I=1,MAXLG)
      READ(IOUNIT)(PLEGVG(I),I=1,MAXLG)
      READ(IOUNIT)(PLEGHG(I),I=1,MAXLG)
      READ(IOUNIT)(PLEGTH(I),I=1,MAXLG)
      READ(IOUNIT)(ALEGAN(I),I=1,MAXLG)
C
      READ(IOUNIT)(IBOBFI(I),I=1,MAXBX)
      READ(IOUNIT)(IBOBCO(I),I=1,MAXBX)
      READ(IOUNIT)(IBOPPA(I),I=1,MAXBX)
      READ(IOUNIT)(IBOPCO(I),I=1,MAXBX)
      READ(IOUNIT)(IBOFPA(I),I=1,MAXBX)
      READ(IOUNIT)(IBOFCO(I),I=1,MAXBX)
C
      READ(IOUNIT)NUMBOX,MAXBOX
C
      READ(IOUNIT)((PBOXXC(I,J),I=1,MAXBX),J=1,2)
      READ(IOUNIT)((PBOXYC(I,J),I=1,MAXBX),J=1,2)
      READ(IOUNIT)(PBOPTH(I),I=1,MAXBX)
      READ(IOUNIT)(PBOPGA(I),I=1,MAXBX)
      READ(IOUNIT)(PBOFTH(I),I=1,MAXBX)
C
      READ(IOUNIT)(IARRPA(I),I=1,MAXAR)
      READ(IOUNIT)(IARRCO(I),I=1,MAXAR)
      READ(IOUNIT)(IARHFI(I),I=1,MAXAR)
C
      READ(IOUNIT)NUMARR,MAXARR
C
      READ(IOUNIT)((PARRXC(I,J),I=1,MAXAR),J=1,2)
      READ(IOUNIT)((PARRYC(I,J),I=1,MAXAR),J=1,2)
      READ(IOUNIT)(PARRTH(I),I=1,MAXAR)
      READ(IOUNIT)(PARHLE(I),I=1,MAXAR)
      READ(IOUNIT)(PARHWI(I),I=1,MAXAR)
C
      READ(IOUNIT)(ISEGPA(I),I=1,MAXSG)
      READ(IOUNIT)(ISEGCO(I),I=1,MAXSG)
C
      READ(IOUNIT)NUMSEG,MAXSEG
C
      READ(IOUNIT)((PSEGXC(I,J),I=1,MAXSG),J=1,2)
      READ(IOUNIT)((PSEGYC(I,J),I=1,MAXSG),J=1,2)
      READ(IOUNIT)(PSEGTH(I),I=1,MAXSG)
C
      READ(IOUNIT)(ILINPA(I),I=1,MAXLN)
      READ(IOUNIT)(ILINCO(I),I=1,MAXLN)
C
      READ(IOUNIT)MAXLIN
C
      READ(IOUNIT)(PLINTH(I),I=1,MAXLN)
      READ(IOUNIT)(PLINLE(I),I=1,MAXLN)
      READ(IOUNIT)(PLINL2(I),I=1,MAXLN)
      READ(IOUNIT)(PLINL3(I),I=1,MAXLN)
      READ(IOUNIT)(PLINGA(I),I=1,MAXLN)
      READ(IOUNIT)(PLING2(I),I=1,MAXLN)
      READ(IOUNIT)(PLING3(I),I=1,MAXLN)
C
      READ(IOUNIT)(ICHAPA(I),I=1,MAXCH2)
      READ(IOUNIT)(ICHAFO(I),I=1,MAXCH2)
      READ(IOUNIT)(ICHACA(I),I=1,MAXCH2)
      READ(IOUNIT)(ICHAJU(I),I=1,MAXCH2)
      READ(IOUNIT)(ICHADI(I),I=1,MAXCH2)
      READ(IOUNIT)(ICHAFI(I),I=1,MAXCH2)
      READ(IOUNIT)(ICHACO(I),I=1,MAXCH2)
C
      READ(IOUNIT)MAXCHA
C
      READ(IOUNIT)(PCHAHE(I),I=1,MAXCH2)
      READ(IOUNIT)(PCHAWI(I),I=1,MAXCH2)
      READ(IOUNIT)(PCHAVG(I),I=1,MAXCH2)
      READ(IOUNIT)(PCHAHG(I),I=1,MAXCH2)
      READ(IOUNIT)(PCHATH(I),I=1,MAXCH2)
      READ(IOUNIT)(ACHAAN(I),I=1,MAXCH2)
C
      READ(IOUNIT)(ITEXTE(I),I=1,MAXCH)
      READ(IOUNIT)ITEXPA,ITEXFO,ITEXCA,ITEXJU,ITEXDI,ITEXAU,ITEXFI,
     1ITEXCO
      READ(IOUNIT)IDEFPA,IDEFFO,IDEFCA,IDEFJU,IDEFDI,IDEFAU,IDEFFI,
     1IDEFCO
      READ(IOUNIT)ITEXCR,ITEXLF
      READ(IOUNIT)IDEFCR,IDEFLF
      READ(IOUNIT)ITEXSY,ITEXSP
      READ(IOUNIT)IDEFSY,IDEFSP
C
      READ(IOUNIT)NCTEXT,MXCTEX
C
      READ(IOUNIT)PTEXHE,PTEXWI,PTEXVG,PTEXHG
      READ(IOUNIT)PTEXTH,PTEXLE,ATEXAN
      READ(IOUNIT)PDEFHE,PDEFWI,PDEFVG,PDEFHG
      READ(IOUNIT)PDEFTH,PDEFLE,ADEFAN
      READ(IOUNIT)PTEXMR
      READ(IOUNIT)PDEFMR
      READ(IOUNIT)PXSTAR,PYSTAR
      READ(IOUNIT)PXEND,PYEND
C
      READ(IOUNIT)(IFILSW(I),I=1,MAXFL)
      READ(IOUNIT)(IFILPA(I),I=1,MAXFL)
      READ(IOUNIT)(IFILCO(I),I=1,MAXFL)
      READ(IOUNIT)IDEFFS
      READ(IOUNIT)IDEFFP
      READ(IOUNIT)IDEFFC
C
      READ(IOUNIT)MAXFIL
C
      READ(IOUNIT)(PFILSP(I),I=1,MAXFL)
      READ(IOUNIT)(PFILTH(I),I=1,MAXFL)
      READ(IOUNIT)(AFILBA(I),I=1,MAXFL)
      READ(IOUNIT)PDEFFG
      READ(IOUNIT)PDEFFT
      READ(IOUNIT)ADEFFB
C
      READ(IOUNIT)(IPATSW(I),I=1,MAXPT)
      READ(IOUNIT)(IPATPA(I),I=1,MAXPT)
      READ(IOUNIT)(IPATLI(I),I=1,MAXPT)
      READ(IOUNIT)(IPATCO(I),I=1,MAXPT)
      READ(IOUNIT)IDEFPS
      READ(IOUNIT)IDEFPP
      READ(IOUNIT)IDEFPL
      READ(IOUNIT)IDEFPC
C
      READ(IOUNIT)MAXPAT
C
      READ(IOUNIT)(PPATHE(I),I=1,MAXPT)
      READ(IOUNIT)(PPATWI(I),I=1,MAXPT)
      READ(IOUNIT)(PPATSP(I),I=1,MAXPT)
      READ(IOUNIT)(PPATTH(I),I=1,MAXPT)
      READ(IOUNIT)PDEFPH
      READ(IOUNIT)PDEFPW
      READ(IOUNIT)PDEFPG
      READ(IOUNIT)PDEFPT
C
      READ(IOUNIT)(ISPISW(I),I=1,MAXSP)
      READ(IOUNIT)(ISPILI(I),I=1,MAXSP)
      READ(IOUNIT)(ISPICO(I),I=1,MAXSP)
      READ(IOUNIT)IDEFSS
      READ(IOUNIT)IDEFSL
      READ(IOUNIT)IDEFSC
C
      READ(IOUNIT)MAXSPI
C
      READ(IOUNIT)(PSPITH(I),I=1,MAXSP)
      READ(IOUNIT)(ASPIBA(I),I=1,MAXSP)
      READ(IOUNIT)PDEFST
      READ(IOUNIT)ADEFSB
C
      READ(IOUNIT)(IBARSW(I),I=1,MAXBA)
      READ(IOUNIT)(IBABLI(I),I=1,MAXBA)
      READ(IOUNIT)(IBABCO(I),I=1,MAXBA)
      READ(IOUNIT)(IBAFSW(I),I=1,MAXBA)
      READ(IOUNIT)(IBAFCO(I),I=1,MAXBA)
      READ(IOUNIT)(IBAPTY(I),I=1,MAXBA)
      READ(IOUNIT)(IBAPLI(I),I=1,MAXBA)
      READ(IOUNIT)(IBAPCO(I),I=1,MAXBA)
      READ(IOUNIT)IDEBSW
      READ(IOUNIT)IDEBBL
      READ(IOUNIT)IDEBBC
      READ(IOUNIT)IDEBFS
      READ(IOUNIT)IDEBFC
      READ(IOUNIT)IDEBPT
      READ(IOUNIT)IDEBPL
      READ(IOUNIT)IDEBPC
C
      READ(IOUNIT)MAXBAR
C
      READ(IOUNIT)(ABARBA(I),I=1,MAXBA)
      READ(IOUNIT)(ABARWI(I),I=1,MAXBA)
      READ(IOUNIT)(PBABTH(I),I=1,MAXBA)
      READ(IOUNIT)(PBAPTH(I),I=1,MAXBA)
      READ(IOUNIT)(PBAPSP(I),I=1,MAXBA)
      READ(IOUNIT)ADEBBA
      READ(IOUNIT)ADEBWI
      READ(IOUNIT)PDEBBT
      READ(IOUNIT)PDEBPT
      READ(IOUNIT)PDEBPS
C
      READ(IOUNIT)(IREGSW(I),I=1,MAXRG)
      READ(IOUNIT)(IREBLI(I),I=1,MAXRG)
      READ(IOUNIT)(IREBCO(I),I=1,MAXRG)
      READ(IOUNIT)(IREFSW(I),I=1,MAXRG)
      READ(IOUNIT)(IREFCO(I),I=1,MAXRG)
      READ(IOUNIT)(IREPTY(I),I=1,MAXRG)
      READ(IOUNIT)(IREPLI(I),I=1,MAXRG)
      READ(IOUNIT)(IREPCO(I),I=1,MAXRG)
      READ(IOUNIT)IDERSW
      READ(IOUNIT)IDERBL
      READ(IOUNIT)IDERBC
      READ(IOUNIT)IDERFS
      READ(IOUNIT)IDERFC
      READ(IOUNIT)IDERPT
      READ(IOUNIT)IDERPL
      READ(IOUNIT)IDERPC
C
      READ(IOUNIT)MAXREG
C
      READ(IOUNIT)(AREGBA(I),I=1,MAXRG)
      READ(IOUNIT)(AREGWI(I),I=1,MAXRG)
      READ(IOUNIT)(PREBTH(I),I=1,MAXRG)
      READ(IOUNIT)(PREPTH(I),I=1,MAXRG)
      READ(IOUNIT)(PREPSP(I),I=1,MAXRG)
      READ(IOUNIT)ADERBA
      READ(IOUNIT)ADERWI
      READ(IOUNIT)PDERBT
      READ(IOUNIT)PDERPT
      READ(IOUNIT)PDERPS
C
      READ(IOUNIT)(IMARSW(I),I=1,MAXMR)
      READ(IOUNIT)(IMABLI(I),I=1,MAXMR)
      READ(IOUNIT)(IMABCO(I),I=1,MAXMR)
      READ(IOUNIT)(IMAFSW(I),I=1,MAXMR)
      READ(IOUNIT)(IMAFCO(I),I=1,MAXMR)
      READ(IOUNIT)(IMAPTY(I),I=1,MAXMR)
      READ(IOUNIT)(IMAPLI(I),I=1,MAXMR)
      READ(IOUNIT)(IMAPCO(I),I=1,MAXMR)
      READ(IOUNIT)IDEMSW
      READ(IOUNIT)IDEMBL
      READ(IOUNIT)IDEMBC
      READ(IOUNIT)IDEMFS
      READ(IOUNIT)IDEMFC
      READ(IOUNIT)IDEMPT
      READ(IOUNIT)IDEMPL
      READ(IOUNIT)IDEMPC
C
      READ(IOUNIT)MAXMAR
C
      READ(IOUNIT)(AMARBA(I),I=1,MAXMR)
      READ(IOUNIT)(AMARWI(I),I=1,MAXMR)
      READ(IOUNIT)(PMABTH(I),I=1,MAXMR)
      READ(IOUNIT)(PMAPTH(I),I=1,MAXMR)
      READ(IOUNIT)(PMAPSP(I),I=1,MAXMR)
      READ(IOUNIT)ADEMBA
      READ(IOUNIT)ADEMWI
      READ(IOUNIT)PDEMBT
      READ(IOUNIT)PDEMPT
      READ(IOUNIT)PDEMPS
C
      READ(IOUNIT)(ITEXSW(I),I=1,MAXTX)
      READ(IOUNIT)(ITEBLI(I),I=1,MAXTX)
      READ(IOUNIT)(ITEBCO(I),I=1,MAXTX)
      READ(IOUNIT)(ITEFSW(I),I=1,MAXTX)
      READ(IOUNIT)(ITEFCO(I),I=1,MAXTX)
      READ(IOUNIT)(ITEPTY(I),I=1,MAXTX)
      READ(IOUNIT)(ITEPLI(I),I=1,MAXTX)
      READ(IOUNIT)(ITEPCO(I),I=1,MAXTX)
      READ(IOUNIT)IDETSW
      READ(IOUNIT)IDETBL
      READ(IOUNIT)IDETBC
      READ(IOUNIT)IDETFS
      READ(IOUNIT)IDETFC
      READ(IOUNIT)IDETPT
      READ(IOUNIT)IDETPL
      READ(IOUNIT)IDETPC
C
      READ(IOUNIT)MAXTEX
C
      READ(IOUNIT)(ATEXBA(I),I=1,MAXTX)
      READ(IOUNIT)(ATEXWI(I),I=1,MAXTX)
      READ(IOUNIT)(PTEBTH(I),I=1,MAXTX)
      READ(IOUNIT)(PTEPTH(I),I=1,MAXTX)
      READ(IOUNIT)(PTEPSP(I),I=1,MAXTX)
      READ(IOUNIT)ADETBA
      READ(IOUNIT)ADETWI
      READ(IOUNIT)PDETBT
      READ(IOUNIT)PDETPT
      READ(IOUNIT)PDETPS
C
C     -----END READING IN-----------------------
C
C               ***************************
C               **  STEP 42--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      ISTEPN='42'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IERROR.EQ.'YES')GOTO4290
      IF(IFEEDB.EQ.'OFF')GOTO4290
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4211)
 4211 FORMAT('THE RESTORING OF ALL INTERNAL DATAPLOT VARIABLES,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4212)
 4212 FORMAT('    PARAMETERS, ETC. HAS JUST BEEN COMPLETED')
      CALL DPWRST('XXX','BUG ')
 4290 CONTINUE
C
C               ***********************
C               **  STEP 51--        **
C               **  CLOSE THE FILE.  **
C               ***********************
C
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'REST')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPREST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,ISUBRO,IERROR
 9012 FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DPRETA(IBUGS2,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--TREAT THE RETAIN CASE--
C              RETAIN SPECIFIED ELEMENTS OF A VARIABLE
C              AND PACK THESE RETAINED ELEMENTS
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH     1978.
C     UPDATED         --MAY       1978.
C     UPDATED         --JUNE      1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   2000. SUPPORT FOR VARIABLE LABELS
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
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
      CHARACTER*4 IVN
      CHARACTER*4 IVN2
      CHARACTER*4 IHVARJ
      CHARACTER*4 IHVRJ2
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION ILISTV(100)
      DIMENSION TEMP(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),TEMP(1))
C
      DIMENSION IVN(100)
      DIMENSION IVN2(100)
      DIMENSION IRN(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRE'
      ISUBN2='TA  '
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
C
      TEMPD=0.0
      VALD1O=0.0
      VALDNO=0.0
      VAL1O=0.0
      VALNO=0.0
C
C               *************************************************
C               **  TREAT THE RETAIN CASE                      **
C               **  RETAIN SPECIFIC ELEMENTS OF A VECTOR       **
C               **  AND PACK THOSE     ELEMENTS                **
C               **  INTO THE FIRST AVAILABLE LOCATIONS.        **
C               *************************************************
C
      IFOUND='YES'
      IERROR='NO'
C
      MAXDEL=100
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGS2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGS2,IERROR
   52 FORMAT('IBUGS2,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXNAM,NUMNAM
   53 FORMAT('MAXNAM,NUMNAM = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)MAXN,MAXCOL,NUMCOL
   54 FORMAT('MAXN,MAXCOL,NUMCOL = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO60I=1,NUMNAM
      WRITE(ICOUT,61)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)
   61 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1I8,2X,A4,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)
   62 FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',
     1I8,2X,A4,A4,6X,I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO70J=1,NUMCOL
      IJ=MAXN*(J-1)+1
      WRITE(ICOUT,71)J,MAXN,IJ,V(IJ)
   71 FORMAT('J,MAXN,IJ,V(IJ) = ',I8,I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   70 CONTINUE
   90 CONTINUE
C               *******************************************************
C               **  STEP 1--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.1)GOTO190
      IERROR='YES'
      GOTO8900
  190 CONTINUE
      IFOUND='YES'
C
C               ***********************************************************
C               **  STEP 2--                                             **
C               **  DETERMINE THE SUBCASE BASED ON THE QUALIFIER.        **
C               **  SCAN TO CHECK IF 'SUBSET' OR 'FOR' IS PRESENT.       **
C               **  IF NOT PRESENT, THEN HAVE CASE 1--                   **
C               **  EXAMPLE--RETAIN X(4) Y(1) Z(46)                      **
C               **  IF PRESENT, THEN HAVE CASE 2--                       **
C               **  EXAMPLE--RETAIN X Y Z FOR I = 1 1 10                 **
C               **  DETERMINE THE LOCATION IN THE ARGUMENT LIST          **
C               **  OF 'SUBSET' OR 'FOR'.                                **
C               **  BRANCH TO THE APPROPRIATE SUBCASE                    **
C               **  FULL VERSUS SUBSET/FOR.                              **
C               ***********************************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCQ=1
      ICASEQ='UNKN'
      IF(NUMARG.LE.0)GOTO290
      DO210J=1,NUMARG
      J2=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GOTO220
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GOTO220
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GOTO230
  210 CONTINUE
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      GOTO300
C
  220 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J2
      GOTO7000
C
  230 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J2
      GOTO7000
C
  290 CONTINUE
C
C               ***********************************************************
C               **  STEP 3--                                             **
C               **  FOR THE FULL CASE,                                   **
C               **  EXTRACT EACH VARIABLE NAME AND EACH ARGUMENT VALUE.  **
C               ***********************************************************
C
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=0
  300 CONTINUE
      IPASS=IPASS+1
C
      IF(1.LE.IPASS.AND.IPASS.LE.MAXDEL)GOTO310
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
  301 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,302)
  302 FORMAT('      THE RETAIN COMMAND REQUIRES THAT ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,303)
  303 FORMAT('      THE NUMBER OF VARIABLES WITH ELEMENTS ',
     1'TO BE RETAINED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,304)IPASS
  304 FORMAT('      BE BETWEEN 1 AND ',I8,' (INCLUSIVE);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,305)NUMDEL
  305 FORMAT('      THE SPECIFIED NUMBER WAS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,306)
  306 FORMAT('      THE INPUT COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,307)(IANS(I),I=1,IWIDTH)
  307 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
  310 CONTINUE
      IF(IPASS.GE.2)ISAVE=IENDRP
C
C               ****************************************************************
C               **  STEP 3.1--
C               **  IF THIS IS THE FIRST PASS ON THIS LINE (AND ONLY FOR PASS 1)
C               **  SEARCH FOR RETAIN (OTHERWISE SKIP THIS STEP)
C               **  SEARCH BETWEEN COLUMN 1 AND THE END OF THE LINE (INCLUSIVE).
C               ****************************************************************
C
      ISTEPN='3.1'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPASS.GE.2)GOTO319
C
      ISTAR1=1
      ISTOP1=IWIDTH
      ISTRIN='RETA'
      ISTRI2='IN  '
      INEX='II'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOUCO,IBEGCO,IENDCO,
     1             ITYPCO,IHOLCO,IHLCO2,INT1CO,FLOACO,IERRO1)
      IF(IFOUCO.EQ.'YES')GOTO319
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,311)
  311 FORMAT('***** INTERNAL ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,312)
  312 FORMAT('      THE WORD      RETAIN      NOT FOUND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,313)
  313 FORMAT('      ON THE ENTERED INPUT COMMAND LINE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,317)
  317 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,318)(IANS(I),I=1,IWIDTH)
  318 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
  319 CONTINUE
C
C               ****************************************************************
C               **  STEP 3.2--
C               **  SEARCH FOR LEFT PARENTHESIS;
C               **  IF THIS IS THE FIRST PASS FOR THIS LINE,
C               **  SEARCH BETWEEN    RETAIN     AND      END OF LINE
C               **  (IF NO LEFT PARENTHESIS FOUND AT ALL, JUMP TO 7000).
C               **  IF THIS IS THE SECOND (OR HIGHER) PASS FOR THIS LINE,
C               **  SEARCH BETWEEN    PREVIOUS RIGHT PARENTHESIS AND
C               **  END OF LINE.
C               ****************************************************************
C
      ISTEPN='3.2'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPASS.LE.1)ISTAR1=IENDCO+1
      IF(IPASS.GE.2)ISTAR1=ISAVE+1
      ISTOP1=IWIDTH
      ISTRIN='('
      INEX='II'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOULP,IBEGLP,IENDLP,
     1             ITYPLP,IHOLLP,IHLLP2,INT1LP,FLOALP,IERRO1)
      IF(IFOULP.EQ.'YES')GOTO338
      IF(IFOULP.EQ.'NO'.AND.IPASS.GE.2)GOTO399
      GOTO7000
  338 CONTINUE
C
C               ****************************************************************
C               **  STEP 3.3--
C               **  SEARCH FOR RIGHT PARENTHESIS;
C               **  SEARCH BETWEEN    LEFT PARENTHESIS     AND    END OF LINE.
C               ****************************************************************
C
      ISTEPN='3.3'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTAR1=IENDLP+1
      ISTOP1=IWIDTH
      ISTRIN=')'
      INEX='II'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOURP,IBEGRP,IENDRP,
     1             ITYPRP,IHOLRP,IHLRP2,INT1RP,FLOARP,IERRO1)
      IF(IFOURP.EQ.'YES')GOTO358
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,341)
  341 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,342)
  342 FORMAT('      WHEN THE RETAIN COMMAND IS USED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,343)
  343 FORMAT('      WITHOUT A SUBSET QUALIFICATION, OR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,344)
  344 FORMAT('      WITHOUT A FOR    QUALIFICATION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,345)
  345 FORMAT('      THEN ONLY INDIVIDUAL ELEMENTS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,346)
  346 FORMAT('      OF A VARIABLE MAY BE RETAINED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,347)
  347 FORMAT('      SUCH INDIVIDUAL ELEMENTS ARE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,348)
  348 FORMAT('      SPECIFIED BY A VARIABLE NAME FOLLOWED BY A')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,349)
  349 FORMAT('      PAIR OF PARENTHSES WITH A ROW NUMBER WITHIN;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,350)
  350 FORMAT('      HOWEVER, A RIGHT PARENTHESIS IS MISSING HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,351)
  351 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,352)(IANS(I),I=1,IWIDTH)
  352 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
  358 CONTINUE
C
C               ****************************************************************
C               **  STEP 3.4--
C               **  SEARCH FOR ROW NUMBER;
C               **  SEARCH BETWEEN    LEFT PARENTHESIS     AND     RIGHT PARENTH
C               ****************************************************************
C
      ISTEPN='3.4'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTAR1=IENDLP
      ISTOP1=IENDRP
      ISTRIN='(;)'
      INEX='EE'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOURN,IBEGRN,IENDRN,
     1             ITYPRN,IHOLRN,IHLRN2,INT1RN,FLOARN,IERRO1)
      IF(IFOURN.EQ.'YES')GOTO378
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,361)
  361 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,362)
  362 FORMAT('      WHEN THE RETAIN COMMAND IS USED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,363)
  363 FORMAT('      WITHOUT A SUBSET QUALIFICATION, OR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,364)
  364 FORMAT('      WITHOUT A FOR    QUALIFICATION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,365)
  365 FORMAT('      THEN ONLY INDIVIDUAL ELEMENTS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,366)
  366 FORMAT('      OF A VARIABLE MAY BE RETAINED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,367)
  367 FORMAT('      SUCH INDIVIDUAL ELEMENTS ARE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,368)
  368 FORMAT('      SPECIFIED BY A VARIABLE NAME FOLLOWED BY A')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,369)
  369 FORMAT('      PAIR OF PARENTHSES WITH A ROW NUMBER WITHIN;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,370)
  370 FORMAT('      HOWEVER, A ROW NUMBER IS MISSING HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,371)
  371 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,352)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO8900
C
  378 CONTINUE
C
C               ****************************************************************
C               **  STEP 3.5--
C               **  SEARCH FOR VARIABLE NAME;
C               **  IF THIS IS THE FIRST PASS FOR THIS LINE,
C               **  SEARCH BETWEEN    RETAIN     AND      LEFT PARENTHESIS;
C               **  IF THIS IS THE SECOND (OR HIGHER) PASS FOR THIS LINE,
C               **  SEARCH BETWEEN    PREVIOUS RIGHT PARENTHESIS AND
C               **  THE NEXT LEFT PARENTHESIS.
C               ****************************************************************
C
      ISTEPN='3.5'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPASS.LE.1)ISTAR1=IENDCO+1
      IF(IPASS.GE.2)ISTAR1=ISAVE+1
      ISTOP1=IENDLP
      ISTRIN='!;('
      INEX='IE'
      CALL DPTY3B(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,
     1             IFOUVN,IBEGVN,IENDVN,
     1             ITYPVN,IHOLVN,IHLVN2,INT1VN,FLOAVN,IERRO1)
      IF(IFOUVN.EQ.'YES')GOTO398
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,381)
  381 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,382)
  382 FORMAT('      WHEN THE RETAIN COMMAND IS USED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,383)
  383 FORMAT('      WITHOUT A SUBSET QUALIFICATION, OR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,384)
  384 FORMAT('      WITHOUT A FOR    QUALIFICATION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,385)
  385 FORMAT('      THEN ONLY INDIVIDUAL ELEMENTS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,386)
  386 FORMAT('      OF A VARIABLE MAY BE RETAINED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,387)
  387 FORMAT('      SUCH INDIVIDUAL ELEMENTS ARE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,388)
  388 FORMAT('      SPECIFIED BY A VARIABLE NAME FOLLOWED BY A')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,389)
  389 FORMAT('      PAIR OF PARENTHSES WITH A ROW NUMBER WITHIN;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,390)
  390 FORMAT('      HOWEVER, A VARIABLE NAME IS MISSING HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,391)
  391 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,392)(IANS(I),I=1,IWIDTH)
  392 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
  398 CONTINUE
      IVN(IPASS)=IHOLVN
      IVN2(IPASS)=IHLVN2
      IRN(IPASS)=INT1RN
C
      GOTO300
C
  399 CONTINUE
      NUMDEL=IPASS-1
C
C               *********************************************************
C               **  STEP 4--                                           **
C               **  FOR THE FULL CASE,                                 **
C               **  CHECK TO MAKE SURE ALL VARIABLES WITH RETENTIONS   **
C               **  ARE, IN FACT, IN THE INTERNAL LIST,                **
C               **  AND ARE, IN FACT, VARIABLES (AS OPPOSED TO         **
C               **  PARAMETERS).                                       **
C               *********************************************************
C
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO420J=1,NUMDEL
        J2=J
        IHVARJ=IVN(J)
        IHVRJ2=IVN2(J)
        DO430I=1,NUMNAM
          I2=I
          IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'V')GOTO440
          IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'P')GOTO450
  430   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,431)
  431   FORMAT('***** ERROR IN DPRETA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,432)
  432   FORMAT('      A VARIABLE WITH ELEMENTS TO BE RETAINED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,434)
  434   FORMAT('      WAS NOT FOUND IN THE INTERNAL NAME LIST.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,435)IHVARJ,IHVRJ2
  435   FORMAT('      THE VARIABLE NAME WAS ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,436)
  436   FORMAT('      THE COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,437)(IANS(I),I=1,MIN(100,IWIDTH))
  437     FORMAT('      ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO8900
C
  440   CONTINUE
        ILISTV(J2)=I2
        GOTO420
C
  450   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,451)
  451   FORMAT('***** ERROR IN DPRETA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,452)
  452   FORMAT('      A VARIABLE WITH ELEMENTS TO BE RETAINED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,454)
  454   FORMAT('      WAS FOUND IN THE INTERNAL NAME LIST,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,455)
  455   FORMAT('      BUT AS A PARAMETER,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,456)
  456   FORMAT('      AND NOT AS A VARIABLE AS IT SHOULD BE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,457)IHVARJ,IHVRJ2
  457   FORMAT('      THE VARIABLE NAME WAS ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,458)
  458   FORMAT('      THE COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,437)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO8900
C
  420 CONTINUE
C
C               *****************************************
C               **  STEP 5--                           **
C               **  TREAT THE FULL CASE.               **
C               **  CARRY OUT THE RETAINING,            **
C               **  AND THE SUBSEQUENT PACKING,        **
C               **  DO THE LIST UPDATING, AND          **
C               **  PRODUCE SOME INFORMATIVE PRINTING. **
C               *****************************************
C
      ISTEPN='5'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO500J=1,NUMDEL
      IHVARJ=IVN(J)
      IHVRJ2=IVN2(J)
      IROWD=IRN(J)
      ILIST2=ILISTV(J)
      NIVARJ=IN(ILIST2)
      ICOLVJ=IVALUE(ILIST2)
      IMAX=NIVARJ
      IF(1.LE.IROWD.AND.IROWD.LE.IMAX)GOTO539
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,531)
  531 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,532)IROWD
  532 FORMAT('      THE SPECIFIED ROW ELEMENT (= ROW ',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,533)IHVARJ,IHVRJ2
  533 FORMAT('      TO BE RETAINED FROM VARIABLE ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,534)
  534 FORMAT('      WAS SMALLER THAN 1, OR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,535)IMAX
  535 FORMAT('      WAS LARGER THAN THE CURRENT (= ',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,536)
  536 FORMAT('      NUMBER OF ELEMENTS IN THIS VARIABLE.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
  539 CONTINUE
C
      NS2=0
      ND2=0
      DO550I=1,IMAX
      IF(I.NE.IROWD)GOTO560
      GOTO570
C
  560 CONTINUE
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
      GOTO550
C
  570 CONTINUE
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
      GOTO550
C
  550 CONTINUE
      NIOLD=NIVARJ
      NINEW=NS2
      IROW1N=1
      IROWNN=NS2
C
      IF(NS2.GE.1)GOTO580
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,581)
  581 FORMAT('***** INTERNAL ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,582)
  582 FORMAT('      FOR THE FULL (UNQUALIFIED) CASE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,583)
  583 FORMAT('      SINCE THE RESULTING NS2 = 0,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,584)
  584 FORMAT('      THE NUMBER OF ELEMENTS RETAINED = 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,585)IHVARJ,IHVRJ2,IMAX,IROWD
  585 FORMAT('      IHVARJ, IHVRJ2, IMAX, IROWD = ',2A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,590)
  590 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,591)(IANS(I),I=1,IWIDTH)
  591 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
  580 CONTINUE
      DO600I=1,NS2
      IJ=MAXN*(ICOLVJ-1)+I
      IF(ICOLVJ.LE.MAXCOL)V(IJ)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP1)PRED(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP2)RES(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=TEMP(I)
  600 CONTINUE
C
      DO602J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLVJ)GOTO605
      GOTO602
  605 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLVJ
      VALUE(J4)=ICOLVJ
      IN(J4)=NINEW
      IVSTAR(J4)=MAXN*(ICOLVJ-1)+1
      IVSTOP(J4)=MAXN*(ICOLVJ-1)+NINEW
  602 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,611)IHVARJ,IHVRJ2,NIOLD
  611 FORMAT('VARIABLE ',2A4,'--OLD NUMBER OF ELEMENTS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,612)NINEW
  612 FORMAT('                   NEW NUMBER OF ELEMENTS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,613)VALD1O
  613 FORMAT('                   FIRST VALUE DELETED    = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,614)IROD1O
  614 FORMAT('                         (DELETED FROM ROW ',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,615)VALDNO
  615 FORMAT('                   LAST  VALUE DELETED    = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,616)IRODNO
  616 FORMAT('                         (DELETED FROM ROW ',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,617)VAL1O
  617 FORMAT('                   FIRST VALUE RETAINED   = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,618)IROW1O,IROW1N
  618 FORMAT('                         (MOVED FROM ROW ',I8,
     1' TO ROW ',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,619)VALNO
  619 FORMAT('                   LAST  VALUE RETAINED   = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,620)IROWNO,IROWNN
  620 FORMAT('                         (MOVED FROM ROW ',I8,
     1' TO ROW ',I8,')')
      CALL DPWRST('XXX','BUG ')
  629 CONTINUE
C
  500 CONTINUE
C
      GOTO8900
C
C               ***************************************************************
C               **  STEP 7--                                                 **
C               **  FOR THE SUBSET AND FOR CASES                            **
C               **  (AND WHEN RETAINING ENTIRE VARIABLES),
C               **  CHECK TO MAKE SURE ALL VARIABLES WITH RETENTIONS          **
C               **  ARE, IN FACT, IN THE INTERNAL LIST,                      **
C               **  AND ARE, IN FACT, VARIABLES (AS OPPOSED TO PARAMETERS).  **
C               ***************************************************************
C
 7000 CONTINUE
C
      ISTEPN='7'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMDEL=ILOCQ-1
      IF(1.LE.NUMDEL.AND.NUMDEL.LE.MAXDEL)GOTO7100
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7101)
 7101 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7102)
 7102 FORMAT('      THE RETAIN COMMAND REQUIRES THAT ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7103)
 7103 FORMAT('      THE NUMBER OF VARIABLES WITH ELEMENTS ',
     1'TO BE RETAINED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7104)MAXDEL
 7104 FORMAT('      BE BETWEEN 1 AND ',I8,' (INCLUSIVE);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7105)NUMDEL
 7105 FORMAT('      THE SPECIFIED NUMBER WAS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7106)
 7106 FORMAT('      THE INPUT COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,7107)(IANS(I),I=1,IWIDTH)
 7107 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
 7100 CONTINUE
      DO7200J=1,NUMDEL
      J2=J
      IHVARJ=IHARG(J)
      IHVRJ2=IHARG2(J)
      DO7300I=1,NUMNAM
      I2=I
      IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1IUSE(I).EQ.'V')GOTO7400
      IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.
     1IUSE(I).EQ.'P')GOTO7500
 7300 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7301)
 7301 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7302)
 7302 FORMAT('      A VARIABLE WITH ELEMENTS TO BE RETAINED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7304)
 7304 FORMAT('      WAS NOT FOUND IN THE INTERNAL NAME LIST.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7305)IHVARJ,IHVRJ2
 7305 FORMAT('      THE VARIABLE NAME WAS ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7306)
 7306 FORMAT('      THE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,7307)(IANS(I),I=1,MIN(IWIDTH,100))
 7307   FORMAT('      ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO8900
C
 7400 CONTINUE
      ILISTV(J2)=I2
      GOTO7200
C
 7500 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7501)
 7501 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7502)
 7502 FORMAT('      A VARIABLE WITH ELEMENTS TO BE RETAINED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7504)
 7504 FORMAT('      WAS FOUND IN THE INTERNAL NAME LIST,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7505)
 7505 FORMAT('      BUT AS A PARAMETER,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7506)
 7506 FORMAT('      AND NOT AS A VARIABLE AS IT SHOULD BE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7507)IHVARJ,IHVRJ2
 7507 FORMAT('      THE VARIABLE NAME WAS ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7508)
 7508 FORMAT('      THE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,7509)(IANS(I),I=1,IWIDTH)
 7509 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
 7200 CONTINUE
C
C               *****************************************
C               **  STEP 8--                           **
C               **  TREAT THE SUBSET AND FOR CASES     **
C               **  AND CERTAIN FULL CASES.            **
C               **  CARRY OUT THE RETAINING,            **
C               **  AND THE SUBSEQUENT PACKING,        **
C               **  DO THE LIST UPDATING, AND          **
C               **  PRODUCE SOME INFORMATIVE PRINTING. **
C               *****************************************
C
      ISTEPN='8'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      WRITE(ICOUT,8011)
 8011 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8012)
 8012 FORMAT('      THE WORD    SUBSET    WAS THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8013)
 8013 FORMAT('      FINAL WORD ON THE COMMAND LINE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8014)
 8014 FORMAT('      THE WORD    SUBSET   SHOULD HAVE BEEN FOLLOWED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8015)
 8015 FORMAT('      BY EITHER 2 OR 3 ARGUMENTS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8016)
 8016 FORMAT('      THE FIRST ARGUMENT SPECIFIES THE SUBSET ',
     1'VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8017)
 8017 FORMAT('      THE SECOND AND (IF EXISTENT) THIRD ARGUMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8018)
 8018 FORMAT('      SPECIFY THE VALUE OR INTERVAL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8019)
 8019 FORMAT('      (OF THE SUBSET VARIABLE) WHICH DEFINES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8020)
 8020 FORMAT('      THE SUBSET OF INTEREST.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8021)
 8021 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,8022)(IANS(I),I=1,IWIDTH)
 8022 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
 8030 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8031)
 8031 FORMAT('***** ERROR IN DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8032)
 8032 FORMAT('      THE WORD    FOR    WAS THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8033)
 8033 FORMAT('      FINAL WORD ON THE COMMAND LINE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8034)
 8034 FORMAT('      THE WORD    FOR    SHOULD HAVE BEEN FOLLOWED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8035)
 8035 FORMAT('      BY EXACTLY 3 OR BY EXACTLY 5    WORDS   --')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8036)
 8036 FORMAT('      1) A DUMMY VARIABLE NAME;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8037)
 8037 FORMAT('      2) AN EQUAL SIGN;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8038)
 8038 FORMAT('      3) ONE LIMIT (LOWER OR UPPER) ',
     1'FOR THE DUMMY VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8039)
 8039 FORMAT('      4) THE INCREMENT FOR THE DUMMY VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9040)
 9040 FORMAT('      5) THE OTHER LIMIT (UPPER OR LOWER) ',
     1'FOR THE DUMMY VARIABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)
 9041 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,9042)(IANS(I),I=1,IWIDTH)
 9042 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
 8100 CONTINUE
      IF(ICASEQ.EQ.'FULL')GOTO8130
      IF(ICASEQ.EQ.'FOR')GOTO8120
      IHSET=IHARG(ILQP1)
      IHSET2=IHARG2(ILQP1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      GOTO8110
C
 8110 CONTINUE
      NISET=IN(ILOC)
      CALL DPSUBS(NISET,ILOCS,NS,IBUGQ,IERROR)
      NQ=NISET
      GOTO8200
C
 8120 CONTINUE
      NIOLD=MAXN
      CALL DPFOR(NIOLD,NINEW,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NINEW
      GOTO8200
C
 8130 CONTINUE
      DO8135I=1,MAXN
      ISUB(I)=1
 8135 CONTINUE
      NQ=MAXN
      GOTO8200
C
 8200 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO8300J=1,NUMDEL
      IHVARJ=IHARG(J)
      IHVRJ2=IHARG2(J)
      ILIST2=ILISTV(J)
      NIVARJ=IN(ILIST2)
      ICOLVJ=IVALUE(ILIST2)
      NS2=0
      ND2=0
      IMAX=NQ
      IF(NIVARJ.LT.NQ)IMAX=NIVARJ
      DO8400I=1,IMAX
      IF(ISUB(I).EQ.1)GOTO8450
C
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
      GOTO8400
C
 8450 CONTINUE
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
      GOTO8400
C
 8400 CONTINUE
      NIOLD=NIVARJ
      NINEW=NS2
      IROW1N=1
      IROWNN=NS2
C
      IF(ND2.GE.1)GOTO8550
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8501)
C8501 FORMAT('***** NOTE--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8502)
C8502 FORMAT('      NO DELETING/RETAINING WAS CARRIED OUT;')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8503)
C8503 FORMAT('      POSSIBLE CAUSES--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8504)
C8504 FORMAT('      1) A NULL    SUBSET    SPECIFICATION;')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8505)
C8505 FORMAT('      2) A NULL    FOR       SPECIFICATION;')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8506)
C8506 FORMAT('      3) THE ELEMENTS TO BE RETAINED')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8507)
C8507 FORMAT('         DID NOT EXIST. ')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8510)
C8510 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IF(IWIDTH.GE.1)WRITE(ICOUT,8511)(IANS(I),I=1,IWIDTH)
C8511 FORMAT('      ',100A1)
CCCCC IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8900
C
 8550 CONTINUE
      DO8500I=1,NS2
      IJ=MAXN*(ICOLVJ-1)+I
      IF(ICOLVJ.LE.MAXCOL)V(IJ)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP1)PRED(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP2)RES(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=TEMP(I)
 8500 CONTINUE
C
      NS2P1=NS2+1
      IF(NS2P1.GT.IMAX)GOTO8569
      DO8560I=NS2P1,IMAX
      IJ=MAXN*(ICOLVJ-1)+I
      IF(ICOLVJ.LE.MAXCOL)V(IJ)=CPUMIN
      IF(ICOLVJ.EQ.MAXCP1)PRED(I)=CPUMIN
      IF(ICOLVJ.EQ.MAXCP2)RES(I)=CPUMIN
      IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=CPUMIN
      IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=CPUMIN
      IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=CPUMIN
      IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=CPUMIN
 8560 CONTINUE
 8569 CONTINUE
C
      DO8600J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLVJ)GOTO8605
      GOTO8600
 8605 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLVJ
      VALUE(J4)=ICOLVJ
      IN(J4)=NINEW
      IVSTAR(J4)=MAXN*(ICOLVJ-1)+1
      IVSTOP(J4)=MAXN*(ICOLVJ-1)+NINEW
 8600 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO8629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8611)IHVARJ,IHVRJ2,NIOLD
 8611 FORMAT('VARIABLE ',2A4,'--OLD NUMBER OF ELEMENTS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8612)NINEW
 8612 FORMAT('                   NEW NUMBER OF ELEMENTS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8613)VALD1O
 8613 FORMAT('                   FIRST VALUE DELETED    = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8614)IROD1O
 8614 FORMAT('                         (DELETED FROM ROW ',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8615)VALDNO
 8615 FORMAT('                   LAST  VALUE DELETED    = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8616)IRODNO
 8616 FORMAT('                         (DELETED FROM ROW ',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8617)VAL1O
 8617 FORMAT('                   FIRST VALUE RETAINED   = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8618)IROW1O,IROW1N
 8618 FORMAT('                         (MOVED FROM ROW ',I8,
     1' TO ROW ',I8,'  )')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8619)VALNO
 8619 FORMAT('                   LAST  VALUE RETAINED   = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8620)IROWNO,IROWNN
 8620 FORMAT('                         (MOVED FROM ROW ',I8,
     1' TO ROW ',I8,')')
      CALL DPWRST('XXX','BUG ')
 8629 CONTINUE
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
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OCTOBER 1993.  ADD IVALU2 TO ARGUMENT LIST.
CCCCC CALL DPUPDV(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
      CALL DPUPDV(IHNAME,IHNAM2,IUSE,IVALUE,IVALU2,VALUE,IN,
     1IVARLB,
     1IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,
     1IBUGS2,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPRETA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IERROR
 9012 FORMAT('IBUGS2,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXNAM,NUMNAM
 9013 FORMAT('MAXNAM,NUMNAM = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)MAXN,MAXCOL,NUMCOL
 9014 FORMAT('MAXN,MAXCOL,NUMCOL = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO9020I=1,NUMNAM
      WRITE(ICOUT,9021)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)
 9021 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1I8,2X,A4,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)
 9022 FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',
     1I8,2X,A4,A4,6X,I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9030J=1,NUMCOL
      IJ=MAXN*(J-1)+1
      WRITE(ICOUT,9031)J,MAXN,IJ,V(IJ)
 9031 FORMAT('J,MAXN,IJ,V(IJ) = ',I8,I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9030 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRETN(IHARG,NUMARG,IDEFTN,IRECTN,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--SPECIFY THE NAME OF THE VARIABLE TO PUT
C              THE TOLERANCE LIMIT VALUES CALCULATED FROM
C              A RECIPE FIT/ANOVA COMMAND
C
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFTN (A  CHARACTER VARIABLE)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--IRECTN (A CHARACTER VARIABLE)
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
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST   1997.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*8 IDEFTN
      CHARACTER*8 IRECTN
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*8 IHOLD
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
   51 FORMAT('***** AT THE BEGINNING OF DPRETN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFTN
   53 FORMAT('IDEFTN = ',A4)
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
      IF(NUMARG.LE.1)THEN
        IHOLD=IDEFTN
      ELSEIF(NUMARG.GE.2)THEN
        IHOLD=IHARG(NUMARG) 
        IF(IHARG(NUMARG).EQ.'OFF')IHOLD=IDEFTN
        IF(IHARG(NUMARG).EQ.'NO')IHOLD=IDEFTN
        IF(IHARG(NUMARG).EQ.'NONE')IHOLD=IDEFTN
        IF(IHARG(NUMARG).EQ.'FALS')IHOLD=IDEFTN
        IF(IHARG(NUMARG).EQ.'ON')IHOLD=IDEFTN
        IF(IHARG(NUMARG).EQ.'YES')IHOLD=IDEFTN
        IF(IHARG(NUMARG).EQ.'TRUE')IHOLD=IDEFTN
        IF(IHARG(NUMARG).EQ.'DEFA')IHOLD=IDEFTN
      ENDIF
C
      IFOUND='YES'
      IRECTN=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECTN
 1181 FORMAT(
     1'THE TOLERANCE LIMITS FROM SUBSEQUENT RECIPE FIT/ANOVA COMMANDS',
     1' WILL BE SAVED IN THE VARIABLE ',A8)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1189 CONTINUE
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPRETN')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR
 9012 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFTN
 9013 FORMAT('IDEFTN = ',A8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IRECTN
 9014 FORMAT('IRECTN = ',A8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,MAXNPP,
     1                CLLIMI,CLWIDT,
     1                ICONT,NUMHPP,IMANUF,
     1                XMATN,YMATN,XMITN,YMITN,
     1                ISQUAR,IVGMSW,IHGMSW,
     1                IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
     1                PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                IX3AUT,ITIAUT,ICAPSW,
     1                IBUGG2,IBUGG3,IBUGQ,
     1                IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1                IFOUND,IERROR)
C
C     PURPOSE--GENERATE A R-F SPREAD PLOT WHICH CONSISTS OF
C              THE FOLLOWING 2 SIDE-BY-SIDE PLOTS
C                 1) A PLOT OF FITTED VALUES (MINUS MEAN)
C                 2) A PLOT OF THE RESIDUALS
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/9
C     ORIGINAL VERSION--SEPTEMBER 1999
C     UPDATED         --FEBRUARY  2011 CALL LIST TO DPPERC
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES---------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICONT
      CHARACTER*4 IPOWE
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
C
      CHARACTER*4 IBUGUG
      CHARACTER*4 IBUGU2
      CHARACTER*4 IBUGU3
      CHARACTER*4 IBUGU4
C
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISQUAR
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
      CHARACTER*4 IREPCH
      CHARACTER*4 IMPSW
      CHARACTER*4 IERAS2
      CHARACTER*4 ICOPS2
      CHARACTER*24 ICHAP2
      CHARACTER*4 ILINP2
      CHARACTER*4 IFEED9
      CHARACTER*4 IANSPP
      CHARACTER*4 IANSRP
      CHARACTER*4 IMANUF
      CHARACTER*4 IX3AUT
      CHARACTER*4 ITIAUT
      CHARACTER*4 IPPTSV
      CHARACTER*4 ITUNSV
      CHARACTER*4 IY1MNS
      CHARACTER*4 IY1MXS
      CHARACTER*4 IY2MNS
      CHARACTER*4 IY2MXS
      CHARACTER*4 IY1SV
      CHARACTER*4 IY2SV
      CHARACTER*4 IY1ZSV
      CHARACTER*4 IY2ZSV
      CHARACTER*4 IWRITE
      CHARACTER*4 IH11
      CHARACTER*4 IH12
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      CHARACTER*4 ITITSV(MAXCH)
C
C-----------------------------------------------------------------
C
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
C
      DIMENSION IANSPP(20)
      DIMENSION IANSRP(20)
C
C-----COMMON------------------------------------------------------
C
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
C
      DIMENSION PREDSV(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),PREDSV(1))
C
C-----COMMON VARIABLES (GENERAL)----------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS---------------------------------------------
C
      DATA (IANSPP(I),I=1,18)
     1/'Q   ','U   ','A   ','N   ','T   ','I   ','L   ','E   ',
     1 '    ',
     1 'P   ','L   ','O   ','T   ',
     1 '    ',
     1 'P   ','R   ','E   ','D   '/
      DATA (IANSRP(I),I=1,17)
     1/'Q   ','U   ','A   ','N   ','T   ','I   ','L   ','E   ',
     1 '    ',
     1 'P   ','L   ','O   ','T   ',
     1 '    ',
     1 'R   ','E   ','S   '/
C
C-----START POINT-------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      ISUBN1='DPRF'
      ISUBN2='    '
C
      ICASPL='RFPL'
      NDONE=0
      NCPP=18
      NCRP=17
C
C               ******************************************
C               **  TREAT THE RF-PLOT ... ANALYSIS CASE **
C               ******************************************
C
      IF(IBUGG2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NUMARG,MAXNPP
   54   FORMAT('NUMARG,MAXNPP = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMARG.GE.1)THEN
          DO61I=1,NUMARG
            WRITE(ICOUT,62)I,IHARG(I),IARGT(I)
   62       FORMAT('I,IHARG(I),IARGT(I) = ',I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
      ENDIF
C
C               **************************************************
C               **   STEP 20--                                  **
C               **   SAVE INITIAL SETTINGS                      **
C               **************************************************
C
      ISTEPN='20'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      PWXMN2=PWXMIN
      PWXMX2=PWXMAX
      PWYMN2=PWYMIN
      PWYMX2=PWYMAX
      IERAS2=IERASW
      ICOPS2=ICOPSW
      ICHAP2=ICHAPA(1)
      ILINP2=ILINPA(1)
      IFEED9=IFEEDB
      DO110I=1,MAXCH
        ITITSV(I)=ITITTE(I)
  110 CONTINUE
      NCTITS=NCTITL
      PTITDZ=PTITDS
      IPPTSV=IPPTBI
      ITUNSV=ITICUN
      PX1TS1=PX1TOL
      PX1TS2=PX1TOR
C
      GY1MNS=GY1MIN
      GY1MXS=GY1MAX
      GY2MNS=GY2MIN
      GY2MXS=GY2MAX
      IY1MNS=IY1MIN
      IY1MXS=IY1MAX
      IY2MNS=IY2MIN
      IY2MXS=IY2MAX
      IY1SV=IY1TSW
      IY2SV=IY2TSW
      IY1ZSV=IY1ZSW
      IY2ZSV=IY2ZSW
C
      PXMNSV=PXMIN
      PXMXSV=PXMAX
C
      IH11='PRED'
      IH12='    '
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IH11,IH12,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      ICOL=IVALUE(ILOCV)
      N1=IN(ILOCV)
C
      DO510I=1,N1
        PREDSV(I)=PRED(I)
  510 CONTINUE
      IWRITE='OFF'
      CALL MEAN(PRED,N1,IWRITE,PMEAN,IBUGG3,IERROR)
      DO520I=1,N1
        PRED(I)=PRED(I)-PMEAN
  520 CONTINUE
C
C               ***************************************************
C               **   STEP 21--                                   **
C               **   GENERATE THE PREDICTED VALUES QUANTILE PLOT **
C               ***************************************************
C
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPRF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      PWXMIN=0.0
      PWXMAX=50.0
      PWYMIN=0.0
      PWYMAX=100.0
      PXMIN=15.0
      PXMAX=100.0
      ICOPSW='OFF'
      IPPTBI='UNBI'
      DO2105I=1,MAXCH
        IX3LTE(I)=' '
        ITITTE(I)='    '
 2105 CONTINUE
      ITITTE(1)='F'
      ITITTE(2)='i'
      ITITTE(3)='t'
      ITITTE(4)='t'
      ITITTE(5)='e'
      ITITTE(6)='d'
      ITITTE(7)=' '
      ITITTE(8)='V'
      ITITTE(9)='a'
      ITITTE(10)='l'
      ITITTE(11)='u'
      ITITTE(12)='e'
      ITITTE(13)='s'
      ITICUN='SCRE'
      NCTITL=13
      PX1TOL=5.0
      PX1TOR=5.0
      IY1TSW='ON'
      IY2TSW='OFF'
      IY1ZSW='ON'
      IY2ZSW='OFF'
      PTITDS=3.0
      NCY1SA=NCY1LA
C
      ICOM='PERC'
      IHARG(1)='POIN'
      IHARG2(1)='T   '
      IHARG(2)='PLOT'
      IHARG2(2)='    '
      IHARG(3)='PRED'
      IHARG2(3)='    '
      NUMARG=3
      CALL DPPERC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            CLLIMI,CLWIDT,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO2800
C
      J=0
      DO2111I=1,NCPP
      J=J+1
      IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSPP(I)
      IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSPP(I)
 2111 CONTINUE
      IF(IX3AUT.EQ.'ON')NCTITL=J
      IF(ITIAUT.EQ.'ON')NCTITL=J
      GOTO2500
C
C               ***************************************************
C               **   STEP 22--                                   **
C               **   GENERATE THE RESIDUAL VALUES QUANTILE PLOT  **
C               ***************************************************
C
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPRF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      PWXMIN=50.0
      PWXMAX=100.0
      PWYMIN=0.0
      PWYMAX=100.0
      PXMIN=0.0
      PXMAX=85.0
      ICOPSW='OFF'
      IERASW='OFF'
      DO2210I=1,MAXCH
      IX3LTE(I)=' '
      ITITTE(I)=' '
 2210 CONTINUE
      ITITTE(1)='R'
      ITITTE(2)='e'
      ITITTE(3)='s'
      ITITTE(4)='i'
      ITITTE(5)='d'
      ITITTE(6)='u'
      ITITTE(7)='a'
      ITITTE(8)='l'
      ITITTE(9)=' '
      ITITTE(10)='V'
      ITITTE(11)='a'
      ITITTE(12)='l'
      ITITTE(13)='u'
      ITITTE(14)='e'
      ITITTE(15)='s'
      NCTITL=15
C
      GY1MIN=FY1MNZ
      GY1MAX=FY1MXZ
      GY2MIN=FY2MNZ
      GY2MAX=FY2MXZ
      IY1MIN='FIXE'
      IY1MAX='FIXE'
      IY2MIN='FIXE'
      IY2MAX='FIXE'
      IY1TSW='OFF'
      IY2TSW='ON'
      IY1ZSW='OFF'
      IY2ZSW='ON'
      NCY1LA=0
C
      ICOM='PERC'
      IHARG(1)='POIN'
      IHARG2(1)='T   '
      IHARG(2)='PLOT'
      IHARG2(2)='    '
      IHARG(3)='RES '
      IHARG2(3)='    '
      NUMARG=3
      CALL DPPERC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            CLLIMI,CLWIDT,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO2800
C
      J=0
      DO2211I=1,NCRP
      J=J+1
      IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSRP(I)
      IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSRP(I)
 2211 CONTINUE
      GOTO2500
C
C               **************************************************
C               **   STEP 25--                                  **
C               **   PLOT THE CURRENT PLOT (OUT OF THE 4)       **
C               **************************************************
 2500 CONTINUE
C
      ICONT=IDCONT(1)
      IPOWE=IDPOWE(1)
      NUMHPP=IDNHPP(1)
      IF(IBUGG3.EQ.'ON')WRITE(ICOUT,2507)IMANUF,NUMDEV,IDMANU(1)
 2507 FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
      IF(IBUGG3.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IMPARG=2
      CALL DPGRAP(Y,X,X3D,D,N,NPLOTP,ICASPL,ICONT,IPOWE,NUMHPP,
     1XMATN,YMATN,XMITN,YMITN,
     1ISQUAR,
     1IVGMSW,IHGMSW,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1YPLOT,XPLOT,X2PLOT,TAGPLO,
     1IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
     1IMPARG,
     1PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1MAXCOL,
     1DSIZE,DSYMB,DCOLOR,DFILL,
     1ICAPSW,
     1IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1IERROR)
      IF(IERROR.EQ.'NO')IAND1=IAND2
      IF(IERROR.EQ.'YES')GOTO2800
      NDONE=NDONE+1
      IF(NDONE.LE.1)GOTO2200
      GOTO2800
C
C               **************************************************
C               **   STEP 28--                                  **
C               **   REINSTATE INITIAL SETTINGS                 **
C               **************************************************
C
 2800 CONTINUE
      ISTEPN='28'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGG3.EQ.'ON')WRITE(ICOUT,2807)IMANUF,NUMDEV,IDMANU(1)
 2807 FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
      IF(IBUGG3.EQ.'ON')CALL DPWRST('XXX','BUG ')
      PWXMIN=PWXMN2
      PWXMAX=PWXMX2
      PWYMIN=PWYMN2
      PWYMAX=PWYMX2
      IERASW=IERAS2
      ICOPSW=ICOPS2
      ICHAPA(1)=ICHAP2
      ILINPA(1)=ILINP2
      IFEEDB=IFEED9
      DO2809I=1,MAXCH
        ITITTE(I)=ITITSV(I)
 2809 CONTINUE
      NCTITL=NCTITS
      PTITDS=PTITDZ
      NCY1LA=NCY1SA
      IPPTBI=IPPTSV
      ITICUN=ITUNSV
      PX1TOL=PX1TS1
      PX1TOR=PX1TS2
C
      GY1MIN=GY1MNS
      GY1MAX=GY1MXS
      GY2MIN=GY2MNS
      GY2MAX=GY2MXS
      IY1MIN=IY1MNS
      IY1MAX=IY1MXS
      IY2MIN=IY2MNS
      IY2MAX=IY2MXS
      IY1TSW=IY1SV
      IY2TSW=IY2SV
      IY1ZSW=IY1ZSV
      IY2ZSW=IY2ZSV
      PXMIN=PXMNSV
      PXMAX=PXMXSV
      DO2820I=1,N1
        PRED(I)=PREDSV(I)
 2820 CONTINUE
      IF(IERROR.EQ.'YES')GOTO9000
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPRF--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMARG
 9014 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GOTO9029
      DO9021I=1,NUMARG
      WRITE(ICOUT,9022)I,IHARG(I),IARGT(I)
 9022 FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9029 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRFCO(IHARG,IARG,NUMARG,IDERFC,MAXREG,IREFCO,
     1                  ICASCL,IREFC2,
     1                  IBUGP2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REGION FILL COLORS = THE COLORS
C              OF THE (BACKGROUND) FILL WITHIN THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR IREFCO(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDERFC
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--IREFCO (A CHARACTER VECTOR)
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
C     UPDATED         --JULY      2013. SUPPORT FOR "RGB" COLORS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IDERFC
      CHARACTER*4 ICASCL
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IREFCO(*)
C
      INTEGER IARG(*)
      INTEGER IREFC2(100,3)
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
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
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRF'
      ISUBN2='CO  '
C
      NUMREG=0
      IHOLD1='-999'
C
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRFCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGP2,ICASCL,IFOUND,IERROR
   52   FORMAT('IBUGP2,ICASCL,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MAXREG,NUMREG,NUMARG
   53   FORMAT('MAXREG,NUMREG,NUMARG = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IDERFC
   55   FORMAT('IDERFC = ',A4)
        CALL DPWRST('XXX','BUG ')
        DO65I=1,NUMARG
          WRITE(ICOUT,66)IHARG(I)
   66     FORMAT('IHARG(I) = ',A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO75I=1,10
          WRITE(ICOUT,76)I,IREFCO(I)
   76     FORMAT('I,IREFCO(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
C
C               **************************************
C               **  STEP 1--                        **
C               **  BRANCH TO THE APPROPRIATE CASE  **
C               **************************************
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.1)GOTO9000
      IF(ICASCL.EQ.'RGB ')GOTO2000
C
      IF(NUMARG.EQ.3 .AND. IHARG(3).EQ.'ALL')THEN
        IHOLD1='    '
        GOTO1300
      ELSEIF(NUMARG.EQ.4)THEN
        IF(IHARG(3).EQ.'ALL')THEN
          IHOLD1=IHARG(4)
          GOTO1300
        ELSEIF(IHARG(4).EQ.'ALL')THEN
          IHOLD1=IHARG(3)
          GOTO1300
        ENDIF
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
C               *************************************************
C
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)THEN
        NUMREG=1
        IREFCO(1)=IDERFC
      ELSE
        NUMREG=NUMARG-2
        IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
        DO1225I=1,NUMREG
          J=I+2
          IHOLD1=IHARG(J)
          IHOLD2=IHOLD1
          IF(IHOLD1.EQ.'ON')IHOLD2=IDERFC
          IF(IHOLD1.EQ.'OFF')IHOLD2=IDERFC
          IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERFC
          IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERFC
          IREFCO(I)=IHOLD2
 1225   CONTINUE
      ENDIF
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO1278I=1,NUMREG
          WRITE(ICOUT,1276)I,IREFCO(I)
 1276     FORMAT('THE FILL COLOR OF REGION ',I6,
     1           ' HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
 1278   CONTINUE
      ENDIF
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
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMREG=MAXREG
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDERFC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDERFC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERFC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERFC
      DO1315I=1,NUMREG
        IREFCO(I)=IHOLD2
 1315 CONTINUE
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,1316)IREFCO(I)
 1316   FORMAT('THE FILL COLOR OF ALL REGIONS',
     1         ' HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IFOUND='YES'
      GOTO9000
C
C     RGB COLORS CASE: 3 COLORS SHOULD BE SATISFIED
C
C                      REGION FILL COLOR
C                      REGION FILL COLOR IRED IBLUE IGREEN
C                      REGION FILL COLOR IRED IBLUE IGREEN ALL
C                      REGION FILL COLOR ALL IRED IBLUE IGREEN
C                      REGION FILL COLOR IRED1 IBLUE1 IGREEN1 IRED2 ...
C
C                      THE "RGB" KEYWORD HAS ALREADY BEEN STRIPPED
C                      OUT.  NOTE THAT THE DEFAULT COLOR IS -999
C                      (I.E., NO RGB COLOR VALUES SPECIFIED).
C
 2000 CONTINUE
C
      JHOLD1=-999
      JHOLD2=-999
      JHOLD3=-999
C
      IF(NUMARG.EQ.3 .AND. IHARG(3).EQ.'ALL')THEN
        GOTO2300
      ELSEIF(NUMARG.EQ.6)THEN
        IF(IHARG(3).EQ.'ALL')THEN
          JHOLD1=IARG(4)
          JHOLD2=IARG(5)
          JHOLD3=IARG(6)
          GOTO2300
        ELSEIF(IHARG(6).EQ.'ALL')THEN
          JHOLD1=IARG(3)
          JHOLD2=IARG(4)
          JHOLD3=IARG(5)
          GOTO2300
        ENDIF
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
C               *************************************************
C
      ISTEPN='22'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.2)THEN
        NUMREG=1
        IREFC2(1,1)=-999
        IREFC2(1,2)=-999
        IREFC2(1,3)=-999
      ELSE
        NTEMP=NUMARG-2
        NUMREG=NTEMP/3
        IF(NUMREG.LT.1)THEN
          IREFC2(1,1)=-999
          IREFC2(1,2)=-999
          IREFC2(1,3)=-999
        ELSEIF(NUMREG.GT.MAXREG)THEN
          NUMREG=MAXREG
        ENDIF
        DO2225I=1,NUMREG
          J1=(I-1)*3+3
          J2=J1+1
          J3=J1+2
          JHOLD1=IARG(J1)
          JHOLD2=IARG(J2)
          JHOLD3=IARG(J3)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.255)JHOLD1=-999
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.255)JHOLD2=-999
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.255)JHOLD3=-999
          IREFC2(I,1)=JHOLD1
          IREFC2(I,2)=JHOLD2
          IREFC2(I,3)=JHOLD3
 2225   CONTINUE
      ENDIF
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO2278I=1,NUMREG
          WRITE(ICOUT,2276)I,IREFC2(I,1),IREFC2(I,2),IREFC2(I,3)
 2276     FORMAT('THE RGB FILL COLORS OF REGION ',I6,
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
      ISTEPN='23'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2315I=1,NUMREG
        IREFC2(I,1)=JHOLD1
        IREFC2(I,2)=JHOLD2
        IREFC2(I,3)=JHOLD3
 2315 CONTINUE
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,2316)IREFC2(I,1),IREFC2(I,2),IREFC2(I,3)
 2316   FORMAT('THE RGB FILL COLORS OF ALL REGIONS HAVE JUST ',
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
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRFCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IHOLD1,IHOLD2
 9014   FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)JHOLD1,JHOLD2,JHOLD3
 9016   FORMAT('JHOLD1,JHOLD2,JHOLD3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRFSW(IHARG,NUMARG,IDERFS,MAXREG,IREFSW,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REGION FILL SWITCHES = THE ON/OFF SWITCHES
C              OF THE (BACKGROUND) FILL WITHIN THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR IREFSW(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDERFS
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--IREFSW (A CHARACTER VECTOR)
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
C     ORIGINAL VERSION--DECEMBER  1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDERFS
      CHARACTER*4 IREFSW
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
      DIMENSION IREFSW(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRF'
      ISUBN2='SW  '
C
      NUMREG=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRFSW--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXREG,NUMREG
   53 FORMAT('MAXREG,NUMREG = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,IHOLD2
   54 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IDERFS
   55 FORMAT('IDERFS = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)IREFSW(1)
   70 FORMAT('IREFSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,IREFSW(I)
   76 FORMAT('I,IREFSW(I) = ',I8,2X,A4)
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
      NUMREG=1
      IREFSW(1)='ON'
      GOTO1270
C
 1220 CONTINUE
      NUMREG=NUMARG-2
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO1225I=1,NUMREG
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERFS
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERFS
      IREFSW(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,IREFSW(I)
 1276 FORMAT('THE FILL SWITCH FOR REGION ',I6,
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
      NUMREG=MAXREG
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERFS
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERFS
      DO1315I=1,NUMREG
      IREFSW(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)IREFSW(I)
 1316 FORMAT('THE FILL SWITCH FOR ALL REGIONS',
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
 9011 FORMAT('***** AT THE END       OF DPRFSW--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXREG,NUMREG
 9013 FORMAT('MAXREG,NUMREG = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,IHOLD2
 9014 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IDERFS
 9015 FORMAT('IDERFS = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)IREFSW(1)
 9030 FORMAT('IREFSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,IREFSW(I)
 9036 FORMAT('I,IREFSW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRING(IHARG,IARGT,IARG,NUMARG,
     1                  NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IDFONT,
     1                  IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--RING THE BELL IMMEDIATELY
C              FOR A SPECIFIED NUMBER OF RINGS.
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
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
C
C-----NON-COMMON VARIABLES----------------------------------------
C
      CHARACTER*4 IHARG
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
      CHARACTER*4 IDFONT
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPCO'
      ISUBN2='SC  '
      IFOUND='NO'
      IERROR='NO'
C
      NUMRIN=1
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
   51   FORMAT('AT THE BEGINNING OF DPRING--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,IBUGG4,IFOUND,IERROR
   53   FORMAT('IBUGD2,IBUGG4,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NUMRIN,NUMARG,NUMDEV
   55   FORMAT('NUMRIN,NUMARG,NUMDEV = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO61I=1,NUMARG
          WRITE(ICOUT,62)I,IHARG(I),IARGT(I),IARG(I)
   62     FORMAT('I,IHARG(I),IARGT(I),IARG(I) = ',
     1           I8,2(2X,A4),2X,I8)
          CALL DPWRST('XXX','BUG ')
   61   CONTINUE
        DO71I=1,NUMDEV
          WRITE(ICOUT,72)I,IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   72     FORMAT('I,IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',
     1           I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,73)I,IDPOWE(I),IDCONT(I),IDCOLO(I)
   73     FORMAT('I,IDPOWE(I),IDCONT(I),IDCOLO(I) = ',
     1           I8,3(2X,A4))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,74)I,IDNVPP(I),IDNHPP(I),IDUNIT(I)
   74     FORMAT('I,IDNVPP(I),IDNHPP(I),IDUNIT(I) = ',3(I8,2X),I8)
          CALL DPWRST('XXX','BUG ')
   71   CONTINUE
        WRITE(ICOUT,82)IMANUF,IMODEL,IMODE2,IMODE3
   82   FORMAT('IMANUF,IMODEL,IMODE2,IMODE3 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,83)IGCONT,IGCOLO
   83   FORMAT('IGCONT,IGCOLO = ',A4,2X,A4)
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
      IF(NUMARG.GE.1.AND.IARGT(NUMARG).EQ.'NUMB')GOTO1120
      GOTO1110
C
 1110 CONTINUE
      NUMRIN=1
      GOTO1150
C
 1120 CONTINUE
      NUMRIN=IARG(NUMARG)
      GOTO1150
C
 1150 CONTINUE
      IFOUND='YES'
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
        IF(IDMANU(IDEVIC).EQ.'DISC')GOTO8000
        IF(IDMANU(IDEVIC).EQ.'NULL')GOTO8000
        IF(IDMANU(IDEVIC).EQ.'NONE')GOTO8000
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
C
C               ****************************************
C               **  STEP 2.1--                        **
C               **  TREAT THE RING BELL CASE          **
C               ****************************************
C
        ISTEPN='2.1'
        IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(NUMRIN.GE.1)THEN
          DO1200I=1,NUMRIN
C
            IF(IBUGD2.EQ.'ON')THEN
              WRITE(ICOUT,1205)
 1205         FORMAT('***** THE BELL SHOULD SOUND NOW *****')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            CALL GRRIBE
 1200     CONTINUE
        ENDIF
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
 9011   FORMAT('AT THE END       OF DPRING--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IFOUND,IERROR,NUMRIN
 9014   FORMAT('IFOUND,IERROR,NUMRIN = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRK(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                PARAM,IPARN,IPARN2,IANGLU,
     1                TEMPX,TEMPY,TEMPYD,
     1                IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IERROR)
C
C     PURPOSE--TREAT THE LET CASE FOR
C              FINDING THE RUNGE-KUTTA SOLITION
C              OF A DIFFERENTIAL EQUATION
C              (FOR A FULL OR PARTIAL DATA SET)
C     EXAMPLE--LET Y = RUNGE-KUTTA EXP(X-Y) X
C            --LET Y = RUNGE-KUTTA F X
C     NOTE--THIS SUBROUTINE OPERATES ON A FUNCTION AND A VECTOR
C           AND PRODUCES A VECTOR.
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
C     ORIGINAL VERSION--SEPTEMBER 1987.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --APRIL     1992. MANY SMALL CHANGES
C     UPDATED         --SEPTEMBER 1993. ADD INPUT ARGUMENT ISUBRO
C     UPDATED         --SEPTEMBER 1993. ADD ISUBRO TO TRACE STATEMENTS
C     UPDATED         --SEPTEMBER 2015. SUPPORT FOR FUNCTION BLOCKS
C     UPDATED         --JULY      2019. CREATE SCRATCH SPACE IN CALLING
C                                       ROUTINE (DPLET)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASL7
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUND
C
      CHARACTER*4 NEWNA1
      CHARACTER*4 NEWNA2
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IWRITE
C
      CHARACTER*4 IHRI11
      CHARACTER*4 IHRI12
      CHARACTER*4 ILEF11
      CHARACTER*4 ILEF12
      CHARACTER*4 ILEF21
      CHARACTER*4 ILEF22
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
C
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 IDUMV
      CHARACTER*4 IDUMV2
      CHARACTER*4 IHPARN
      CHARACTER*4 IHPAR2
      CHARACTER*4 IWD1
      CHARACTER*4 IWD2
      CHARACTER*4 IWD12
      CHARACTER*4 IWD22
      CHARACTER*4 ILAB
      CHARACTER*4 IERRO2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IH1
      CHARACTER*4 IH2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
C
      DIMENSION IDUMV(100)
      DIMENSION IDUMV2(100)
C
      DIMENSION ILAB(10)
CCCCC THE FOLLOWING 4 LINES WERE COMMENTED OUT   APRIL 1992
CCCCC DIMENSION IOLD(10)
CCCCC DIMENSION IOLD2(10)
CCCCC DIMENSION INEW(10)
CCCCC DIMENSION INEW2(10)
C
      DIMENSION TEMPX(*)
      DIMENSION TEMPY(*)
      DIMENSION TEMPYD(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFB.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C               *******************************************
C               **  TREAT THE RUNGE-KUTTA    SUBCASE     **
C               **  OF THE LET COMMAND                   **
C               *******************************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DPRK')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGCO,IBUGEV,IBUGCO,ISUBRO,ICASL7
   52   FORMAT('IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,ICASL7 = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASL7='RK'
      ISUBN1='DPRK'
      ISUBN2='    '
      IERROR='NO'
      IFOUND='NO'
      NEWNA1='NO'
      NEWCOL='NO'
      NEWNAM='NO'
      NEWNA1='NO'
      NEWNA2='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      NIFOR=0
      ILOCMX=0
      ILOCSV=0
      NUMLIM=0
      ILOC3=0
      IP=0
      IV=0
      LOCDUM=0
      NUMVAL=1
      ICOLY=(-999)
      ICOLYD=(-999)
      MAXN2=MAXCHF
      MAXN3=MAXCHF
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=4
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1IERROR)
      IF(IERROR.EQ.'YES')GOTO19000
C
C
C               ****************************************************************
C               **  STEP 2A--                                                  *
C               **  EXAMINE THE LEFT-HAND SIDE--                               *
C               **  IS THE FIRST  VARIABLE NAME TO LEFT OF = SIGN              *
C               **  ALREADY IN THE NAME LIST?    AS A VARIABLE?                *
C               **  NOTE THAT     ILEF11     IS THE NAME OF THE VARIABLE
C               **  ON THE LEFT.                                               *
C               **  NOTE THAT     ILISL1    IS THE LINE IN THE TABLE           *
C               **  OF THE NAME ON THE LEFT.                                   *
C               **  NOTE THAT     ICOLL1    IS THE DATA COLUMN (1 TO 12)
C               **  FOR THE NAME OF THE LEFT.                                  *
C               ****************************************************************
C
      ISTEPN='2A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILEF11=IHARG(1)
      ILEF12=IHARG2(1)
      DO210I=1,NUMNAM
        I2=I
        IF(ILEF11.EQ.IHNAME(I).AND.ILEF12.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'P')THEN
            NUMVAL=1
            ILISL1=I2
            NIOLD=0
            ICOLL1=NUMCOL+1
            IF(ICOLL1.GT.MAXCOL)THEN
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,242)
  242         FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST ',
     1               'EXCEEDED')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,243)MAXCOL
  243         FORMAT('      THE MAX ALLOWABLE ',I8,'  .  SUGGESTED ',
     1               'ACTION--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,245)
  245         FORMAT('      ENTER      STAT')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,246)
  246         FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,247)
  247         FORMAT('      AND THEN DELETE SOME COLUMNS.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO19000
            ENDIF
            GOTO290
          ELSEIF(IUSE(I).EQ.'V')THEN
            NUMVAL=1
            ILISL1=I2
            ICOLL1=IVALUE(ILISL1)
            NIOLD=IN(ILISL1)
            GOTO290
          ENDIF
        ENDIF
  210 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,211)
  211 FORMAT('***** ERROR IN RUNGE-KUTTA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,212)
  212 FORMAT('      FOR RUNGE-KUTTA, THE FIRST VARIABLE TO THE LEFT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,214)ILEF11,ILEF12
  214 FORMAT('      OF THE EQUAL SIGN (IN THIS CASE,  ',2A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,215)
  215 FORMAT('      MUST PRE-EXIST, AND MUST HAVE AS ITS FIRST ELEMENT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,217)
  217 FORMAT('      YOUR DESIRED INITIAL VALUE.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
C
  290 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
        WRITE(ICOUT,291)
  291   FORMAT('AT THE END OF STEP 2A--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,292)ILEF11,ILEF12,NEWNA1,NUMNAM,ILISL1,NUMCOL,
     1                  ICOLL1,NIOLD
  292   FORMAT('ILEF11,ILEF12,NEWNA1,NUMNAM,ILISL1,NUMCOL,ICOLL1,',
     1         'NIOLD = ',A4,A4,2X,A4,2X,5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************************************
C               **  STEP 2B--                                                  *
C               **  EXAMINE THE LEFT-HAND SIDE--                               *
C               **  IS THE SECOND VARIABLE NAME TO LEFT OF = SIGN              *
C               **  ALREADY IN THE NAME LIST?    AS A VARIABLE?                *
C               **  NOTE THAT     ILEF21     IS THE NAME OF THE VARIABLE
C               **  ON THE LEFT.                                               *
C               **  NOTE THAT     ILISL2    IS THE LINE IN THE TABLE           *
C               **  OF THE NAME ON THE LEFT.                                   *
C               **  NOTE THAT     ICOLL2    IS THE DATA COLUMN (1 TO 12)
C               **  FOR THE NAME OF THE LEFT.                                  *
C               ****************************************************************
C
      ISTEPN='2B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2015/09: CHECK TO SEE IF THE FIRST ARGUMENT ON RHS IS A FUNCTION
C              BLOCK NAME.
C
      IF(IHARG(4).EQ.IFBNA1(1:4) .AND. IHARG2(4).EQ.IFBNA1(5:8))THEN
        IFLGFB=1
      ELSEIF(IHARG(4).EQ.IFBNA2(1:4) .AND. IHARG2(4).EQ.IFBNA2(5:8))THEN
        IFLGFB=2
      ELSEIF(IHARG(4).EQ.IFBNA3(1:4) .AND. IHARG2(4).EQ.IFBNA3(5:8))THEN
        IFLGFB=3
      ELSE
        IFLGFB=0
      ENDIF
C
      IF(IHARG(2).NE.'=')THEN
        ILEF21=IHARG(2)
        ILEF22=IHARG2(2)
        DO310I=1,NUMNAM
          I2=I
          IF(ILEF21.EQ.IHNAME(I).AND.ILEF22.EQ.IHNAM2(I))THEN
            IF(IUSE(I).EQ.'P')THEN
              NUMVAL=2
              ILISL2=I2
              NIOLD=0
              ICOLL2=NUMCOL+1
              IF(ICOLL2.GT.MAXCOL)THEN
                WRITE(ICOUT,211)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,242)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,243)MAXCOL
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,245)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,246)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,247)
                CALL DPWRST('XXX','BUG ')
                IERROR='YES'
                GOTO19000
              ENDIF
              GOTO390
            ELSEIF(IUSE(I).EQ.'V')THEN
              NUMVAL=2
              ILISL2=I2
              ICOLL2=IVALUE(ILISL2)
              GOTO390
            ENDIF
          ENDIF
  310   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)
  312   FORMAT('      FOR RUNGE-KUTTA, THE SECOND VARIABLE TO THE LEFT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,314)ILEF21,ILEF22
  314   FORMAT('      OF THE EQUAL SIGN (IN THIS CASE,  ',2A4,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,316)
  316   FORMAT('      MUST PRE-EXIST, AND MUST HAVE AS ITS FIRST ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,317)
  317   FORMAT('      ELEMENT YOUR DESIRED INITIAL VALUE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO19000
C
  390   CONTINUE
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DPRK')THEN
          WRITE(ICOUT,391)
  391     FORMAT('AT THE END OF STEP 2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,392)ILEF21,ILEF22,NEWNA2,NUMNAM,ILISL2,NUMCOL,
     1                    ICOLL2,NIOLD
  392     FORMAT('ILEF21,ILEF22,NEWNA1,NUMNAM,ILISL2,NUMCOL,ICOLL2,',
     1           'NIOLD = ',A4,A4,2X,A4,2X,5I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ENDIF
C
C               ***********************************************************
C               **  STEP 4--                                             **
C               **  FIND    THE VARIABLE ON THE RIGHT-HAND SIDE--        **
C               **  (THIS WILL BE THE VARIABLE OF DIFFERENTIATION AND    **
C               **  HORIZONTAL AXIS VARIABLE.  HAS THIS VARIABLE ON THE  **
C               **  RIGHT ALREADY BEEN DEFINED?  NOTE THAT    ILISR1     **
C               **  IS THE LINE IN THE TABLE OF THIS VARIABLE ON THE     **
C               **  RIGHT.  NOTE THAT     ICOLR1    IS THE DATA COLUMN   **
C               **  OF THIS VARIABLE ON THE RIGHT.                       **
C               ***********************************************************
C
C               ********************************************
C               **  STEP 4.1--                            **
C               **  DETERMINE THE LOCATION                **
C               **  OF THE VARIABLE ON THE RIGHT          **
C               ********************************************
C
      ISTEPN='4.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVAR=1
C
      DO1005I=4,NUMARG
        I2=I
        IH1=IHARG(I)
        IH2=IHARG2(I)
        IF(IH1.EQ.'SUBS'.AND.IH2.EQ.'ET  ')GOTO1007
        IF(IH1.EQ.'EXCE'.AND.IH2.EQ.'PT  ')GOTO1007
        IF(IH1.EQ.'FOR '.AND.IH2.EQ.'    ')GOTO1007
 1005 CONTINUE
      ILOCQ=NUMARG+1
      GOTO1009
 1007 CONTINUE
      ILOCQ=I2
      GOTO1009
 1009 CONTINUE
      ILOCR1=ILOCQ-1
      ILOCR2=ILOCR1+1
      ILOCR3=ILOCR1+2
      ILOCR4=ILOCR1+3
CCCCC THE FOLLOWING 2 LINES WERE ADDED   APRIL 1992
      ILOCR5=ILOCR1+4
      ILOCR6=ILOCR1+5
C
C               ***************************************
C               **  STEP 5.1--                       **
C               **  EXAMINE THIS VARIABLE            **
C               **  ON THE RIGHT.                    **
C               ***************************************
C
C
      ISTEPN='5.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHRI11=IHARG(ILOCR1)
      IHRI12=IHARG2(ILOCR1)
      DO1120I=1,NUMNAM
        I2=I
        IF(IHRI11.EQ.IHNAME(I).AND.IHRI12.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'V')THEN
            ILISR1=I2
            ICOLR1=IVALUE(ILISR1)
            NIRIG1=IN(ILISR1)
            GOTO1190
          ELSEIF(IUSE(I).EQ.'P')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1142)
 1142       FORMAT('      THE SPECIFIED DIFFERENTIATION VARIABLE ON ',
     1             'THE RIGHT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1145)
 1145       FORMAT('      OF THE = SIGN WAS FOUND IN THE INTERNAL ',
     1             'NAME LIST,')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1147)
 1147       FORMAT('      BUT AS A PARAMETER AND NOT AS A VARIABLE ',
     1             'AS IT SHOULD BE HERE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1128)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1129)(IANS(II),II=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO19000
          ENDIF
        ENDIF
 1120 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      THE SPECIFIED DIFFERENTIATION VARIABLE ON THE ',
     1       'RIGHT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      OF THE = SIGN WAS NOT FOUND IN THE INTERNAL NAME ',
     1       'LIST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      OF AVAILABLE VARIABLE NAMES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)IHRI11,IHRI12
 1127 FORMAT('      THE VARIABLE IN QUESTION WAS ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1129)(IANS(I),I=1,MIN(100,IWIDTH))
 1129   FORMAT(100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO19000
C
 1190 CONTINUE
C
C
C               *******************************
C               **  STEP 7--                 **
C               **  DETERMINE THE SUBCASE    **
C               **  AND BRANCH ACCORDINGLY.  **
C               *******************************
C
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ILOCR1.EQ.NUMARG)GOTO8000
      IF(ILOCR1.LT.NUMARG.AND.IHARG(ILOCR2).EQ.'SUBS'.AND.
     1IHARG2(ILOCR2).EQ.'ET  ')GOTO9000
      IF(ILOCR1.LT.NUMARG.AND.IHARG(ILOCR2).EQ.'EXCE'.AND.
     1IHARG2(ILOCR2).EQ.'PT  ')GOTO9000
      IF(ILOCR1.LT.NUMARG.AND.IHARG(ILOCR2).EQ.'FOR '.AND.
     1IHARG2(ILOCR2).EQ.'    ')GOTO10000
      GOTO7080
C
 7080 CONTINUE
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7082)
 7082 FORMAT('      ILLEGAL SYNTAX FOR LET COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)(IANS(I),I=1,MIN(100,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7088)ILOCR1,NUMVAR
 7088 FORMAT('ILOCR1,NUMVAR = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
C               ************************************************
C               **  STEP 8--                                  **
C               **  TREAT THE FULL VARIABLE CASE.             **
C               **  THEN JUMP TO STEP NUMBER 10 BELOW         **
C               **  FOR THE LIST UPDATING AND                 **
C               **  FOR SOME INFORMATIVE PRINTING.            **
C               ************************************************
C
C
 8000 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
        ISTEPN='8'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,8011)NINEW,NIRIG1
 8011   FORMAT('NINEW,NIRIG1 = ',2I8)
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ENDIF
C
      ICASEQ='FULL'
      NIOLD=NIRIG1
      IF(NUMVAR.GE.2.AND.NIRIG1.GT.NIOLD)NIOLD=NIRIG1
C     MORE HERE FOR NUMVAR = 3 ????? APRIL 1987
      NINEW=NIOLD
      DO8100I=1,NINEW
        ISUB(I)=1
 8100 CONTINUE
      GOTO11000
C
C               ***********************************************
C               **  STEP 9--                                 **
C               **  TREAT THE PARTIAL VARIABLE SUBSET CASE.  **
C               **  JUMP TO STEP NUMBER 11 BELOW             **
C               **  FOR THE ACTUAL MATHEMATICAL OPERATION,   **
C               **  FOR THE LIST UPDATING, AND               **
C               **  FOR SOME INFORMATIVE PRINTING.           **
C               ***********************************************
C
 9000 CONTINUE
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='SUBS'
      IF(NUMVAR.EQ.1)ILOCSV=ILOCR3
      IF(NUMVAR.EQ.2)ILOCSV=ILOCR4
      IF(NUMVAR.EQ.3)ILOCSV=ILOCR5
      IF(NUMVAR.EQ.4)ILOCSV=ILOCR6
      IHSET=IHARG(ILOCSV)
      IHSET2=IHARG2(ILOCSV)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GOTO19000
      NIOLD=IN(ILOC)
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
CCCCC NINEW=NS
      NINEW=NIOLD
      GOTO11000
C
C               **********************************************
C               **  STEP 10--                               **
C               **  TREAT THE PARTIAL VARIABLE FOR CASE.    **
C               **  JUMP TO STEP NUMBER 11 BELOW            **
C               **  FOR THE ACTUAL MATHEMATICAL OPERATION,  **
C               **  FOR THE LIST UPDATING, AND              **
C               **  FOR SOME INFORMATIVE PRINTING.          **
C               **********************************************
C
10000 CONTINUE
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FOR'
      CALL DPFOR(NIOLD,NINEW,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NIFOR=NINEW
      GOTO11000
C
C               *******************************************
C               **  STEP 11.1--                          **
C               **  FILL TEMPORARY VARIBLES.             **
C               **  EXTRACT AND EXAMINE THE FUNCTION.    **
C               **  CARRY OUT THE                        **
C               **  RUNGE-KUTTA CALCULATIONS,            **
C               **  THE LIST UPDATING, AND               **
C               **  GENERATE THE INFORMATIVE PRINTING    **
C               **  FOR STEP NUMBERS 7, 8, AND 9 ABOVE.  **
C               *******************************************
C
11000 CONTINUE
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NITEMX=NINEW
      NS1=0
      NS2=0
      NS3=0
      NS4=0
 
CCCCC IMAX=NINEW
CCCCC IF(ICASEQ.EQ.'FOR'.AND.IMAX.GT.NIFOR)IMAX=NIFOR
CCCCC DO11100I=1,IMAX
C
      DO11100I=1,NINEW
        IJ=MAXN*(ICOLR1-1)+I
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
          WRITE(ICOUT,11101)I,NS1,NINEW,ISUB(I),IJ,V(IJ)
11101     FORMAT('I,NS1,NINEW,ISUB(I),IJ,V(IJ) = ',5I8,F12.5)
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        ENDIF
C
        IF(ISUB(I).EQ.0)GOTO11100
C
        IF(I.GT.NIRIG1)GOTO11190
        NS1=NS1+1
        IJ=MAXN*(ICOLR1-1)+I
        IF(ICOLR1.LE.MAXCOL)TEMPX(NS1)=V(IJ)
        IF(ICOLR1.EQ.MAXCP1)TEMPX(NS1)=PRED(I)
        IF(ICOLR1.EQ.MAXCP2)TEMPX(NS1)=RES(I)
        IF(ICOLR1.EQ.MAXCP3)TEMPX(NS1)=YPLOT(I)
        IF(ICOLR1.EQ.MAXCP4)TEMPX(NS1)=XPLOT(I)
        IF(ICOLR1.EQ.MAXCP5)TEMPX(NS1)=X2PLOT(I)
        IF(ICOLR1.EQ.MAXCP6)TEMPX(NS1)=TAGPLO(I)
C
        IJ=MAXN*(ICOLL1-1)+I
        IF(ICOLL1.LE.MAXCOL)TEMPY(NS1)=V(IJ)
        IF(ICOLL1.EQ.MAXCP1)TEMPY(NS1)=PRED(I)
        IF(ICOLL1.EQ.MAXCP2)TEMPY(NS1)=RES(I)
        IF(ICOLL1.EQ.MAXCP3)TEMPY(NS1)=YPLOT(I)
        IF(ICOLL1.EQ.MAXCP4)TEMPY(NS1)=XPLOT(I)
        IF(ICOLL1.EQ.MAXCP5)TEMPY(NS1)=X2PLOT(I)
        IF(ICOLL1.EQ.MAXCP6)TEMPY(NS1)=TAGPLO(I)
C
        IF(NUMVAL.LE.1)GOTO11100
        IJ=MAXN*(ICOLL2-1)+I
        IF(ICOLL2.LE.MAXCOL)TEMPYD(NS1)=V(IJ)
        IF(ICOLL2.EQ.MAXCP1)TEMPYD(NS1)=PRED(I)
        IF(ICOLL2.EQ.MAXCP2)TEMPYD(NS1)=RES(I)
        IF(ICOLL2.EQ.MAXCP3)TEMPYD(NS1)=YPLOT(I)
        IF(ICOLL2.EQ.MAXCP4)TEMPYD(NS1)=XPLOT(I)
        IF(ICOLL2.EQ.MAXCP5)TEMPYD(NS1)=X2PLOT(I)
        IF(ICOLL2.EQ.MAXCP6)TEMPYD(NS1)=TAGPLO(I)
C
11100 CONTINUE
11190 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
        WRITE(ICOUT,11191)ICOLL1,ICOLL2,ICOLR1,NS1,NUMVAL
11191   FORMAT('ICOLL1,ICOLL2,ICOLR1,NS1,NUMVAL = ',5I8)
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,11192)NINEW,ICASL7,ICASEQ
11192   FORMAT('NINEW,ICASL7,ICASEQ = ',I8,2X,A4,2X,A4)
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ENDIF
C
      IWRITE='ON'
      IF(IPRINT.EQ.'OFF')IWRITE='OFF'
      IF(IFEEDB.EQ.'OFF')IWRITE='OFF'
C
C               ***************************************************************
C               **  STEP 11.2--                                              **
C               **  EXTRACT THE RIGHT-SIDE FUNCTIONAL
C               **  EXPRESSION FROM THE INPUT COMMAND LINE                   **
C               **  (STARTING WITH THE FIRST NON-BLANK LOCATION AFTER THE    **
C               **  WORD    KUTTA     AND ENDING WITH THE NEXT TO THE LAST   **
C               **  WORD.                                                    **
C               **  PLACE THE FUNCTION IN IFUNC2(.)  .                       **
C               ***************************************************************
C
      ISTEPN='11.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GT.5)THEN
C
        IWD1=IHARG(4)
        IWD12=IHARG2(4)
        IF(NUMVAL.GE.2)IWD1=IHARG(5)
        IF(NUMVAL.GE.2)IWD12=IHARG2(5)
        IWD2=IHARG(ILOCR1)
        IWD22=IHARG2(ILOCR1)
        CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1              IFUNC2,N2,IBUGA3,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO19000
        IF(IFOUND.EQ.'YES')GOTO11290
      ENDIF
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11212)
11212 FORMAT('      INVALID COMMAND FORM FOR RUNGE-KUTTA.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11213)
11213 FORMAT('      GENERAL FORM FOR FIRST ORDER--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11214)
11214 FORMAT('         LET Y = RUNGE-KUTTA F X')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11215)
11215 FORMAT('      GENERAL FORM FOR SECOND ORDER--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11216)
11216 FORMAT('         LET Y YP = RUNGE-KUTTA F X')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11217)
11217 FORMAT('      WHERE F IS A FUNCTION OF X, Y, AND YP.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11218)
11218 FORMAT('      (AND YP = Y PRIME = DERIVATIVE OF Y WRT X)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1129)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO19000
C
11290 CONTINUE
C
C               ***********************************************************
C               **  STEP 11.3--                                          **
C               **  DETERMINE IF THE EXPRESSION HAS ANY FUNCTION NAMES   **
C               **  INBEDDED.  IF SO, REPLACE THE FUNCTION NAMES         **
C               **  BY EACH FUNCTION'S DEFINITION.  DO SO REPEATEDLY     **
C               **  UNTIL ALL FUNCTION REFERENCES HAVE BEEN ANNIHILATED  **
C               **  AND THE EXPRESSION IS LEFT ONLY WITH                 **
C               **  CONSTANTS, PARAMETERS, AND VARIABLES--NO FUNCTIONS.  **
C               **  PLACE THE RESULTING FUNCTIONAL EXPRESSION INTO IFUNC3(.) **
C               ***********************************************************
C
      ISTEPN='11.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFLGFB.LE.0)THEN
        CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1              NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,
     1              N3,MAXN3,
     1              IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO19000
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
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
          WRITE(ICOUT,11311)IDUMV(1),IDUMV2(1)
11311     FORMAT('DIFFERENTIATION  = ',A4,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ENDIF
C
C               ************************************************************
C               **  STEP 11.4--                                           **
C               **  DETERMINE THE DUMMY VARIABLE IN THE DIFFERENTIATION.  **
C               ************************************************************
C
      ISTEPN='11.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IDUMV(1)=IHRI11
      IDUMV2(1)=IHRI12
      IDUMV(2)=ILEF11
      IDUMV2(2)=ILEF12
      NUMDV=2
      IF(NUMVAL.GE.2)THEN
        IDUMV(3)=ILEF21
        IDUMV2(3)=ILEF22
        NUMDV=3
      ENDIF
C
C               **********************************************************
C               **  STEP 11.5--                                         **
C               **  MAKE A NON-CALCULATING PASS AT THE FUNCTION         **
C               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.  **
C               **********************************************************
C
      ISTEPN='11.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFLGFB.LE.0)THEN
        IPASS=1
        CALL COMPIM(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1              IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1              IBUGCO,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GOTO19000
      ELSE
        GOTO11901
      ENDIF
C
C               ********************************************
C               **  STEP 11.6--                           **
C               **  TREAT THE SPECIAL CASE WHERE          **
C               **  THE HORIZONTAL AXIS AND/OR THE        **
C               **  VERTICAL AXIS VARIABLE DOES NOT       **
C               **  EXPLICITLY APPEAR IN THE FUNCTON;     **
C               **  IN SUCH CASE, AUGMENT THE PARAMETER   **
C               **  LIST WITH THE 1 (OR 2) VARIABLES.     **
C               ********************************************
C
      ISTEPN='11.6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMPV.GE.1)THEN
        DO11610I=1,NUMPV
          I2=I
          IF(IHRI11.EQ.IPARN(I).AND.IHRI12.EQ.IPARN2(I))GOTO11619
11610   CONTINUE
      ENDIF
      NUMPV=NUMPV+1
      IPARN(NUMPV)=IHRI11
      IPARN2(NUMPV)=IHRI12
11619 CONTINUE
C
      IF(NUMPV.GT.0)THEN
        DO11620I=1,NUMPV
          I2=I
          IF(ILEF11.EQ.IPARN(I).AND.ILEF12.EQ.IPARN2(I))GOTO11629
11620   CONTINUE
      ENDIF
      NUMPV=NUMPV+1
      IPARN(NUMPV)=ILEF11
      IPARN2(NUMPV)=ILEF12
11629 CONTINUE
C
      IF(NUMVAL.LE.1)GOTO11639
      IF(NUMPV.GE.1)THEN
        DO11630I=1,NUMPV
          I2=I
          IF(ILEF21.EQ.IPARN(I).AND.ILEF22.EQ.IPARN2(I))GOTO11639
11630   CONTINUE
      ENDIF
      NUMPV=NUMPV+1
      IPARN(NUMPV)=ILEF21
      IPARN2(NUMPV)=ILEF22
11639 CONTINUE
C
C               ***********************************************
C               **  STEP 11.7--                              **
C               **  CHECK THAT ALL PARAMETERS                **
C               **  IN THE FUNCTION ARE ALREADY PRESENT      **
C               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
C               ***********************************************
C
      ISTEPN='11.7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IP=0
      IV=0
      IF(NUMPV.GE.1)THEN
        DO11700J=1,NUMPV
          IHPARN=IPARN(J)
          IHPAR2=IPARN2(J)
          IF(IHPARN.EQ.IDUMV(3).AND.IHPAR2.EQ.IDUMV2(3))GOTO11730
          IF(IHPARN.EQ.IDUMV(2).AND.IHPAR2.EQ.IDUMV2(2))GOTO11730
          IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1))GOTO11730
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IHPARN,IHPAR2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
          IF(IERRO2.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11712)
11712       FORMAT('      A PARAMETER/FUNCTION HAS BEEN ENCOUNTERED')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11713)
11713       FORMAT('      IN THE FUNCTION TO BE RUNGE-KUTTA SOLVED')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11714)
11714       FORMAT('      WHICH HAS NOT YET BEEN DEFINED')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11715)IHPARN,IHPAR2
11715       FORMAT('      THE UNKNOWN PARAMETER/FUNCTION = ',A4,A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1128)
            CALL DPWRST('XXX','BUG ')
            IF(IWIDTH.GE.1)THEN
              WRITE(ICOUT,1129)(IANS(I),I=1,MIN(100,IWIDTH))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
            GOTO19000
          ENDIF
C
          IP=IP+1
          PARAM(J)=VALUE(ILOCP)
          GOTO11700
C
11730     CONTINUE
          IV=IV+1
          LOCDUM=J
11700   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11.8--                           **
C               **  DETERMINE WHERE IN THE PARAM(.) LIST  **
C               **  THE HORIZ. AXIS VARIABLE LAY,         **
C               **  THE VERT. AXIS VARIABLE LAY,          **
C               **  AND (IF AN ORDER 2 EQUATION) WHERE    **
C               **  THE DERIVATIVE AXIS VARIABLE LAY.     **
C               ********************************************
C
      ISTEPN='11.8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCHV=(-999)
      IF(NUMPV.GE.1)THEN
        DO11810I=1,NUMPV
          I2=I
          IF(IHRI11.EQ.IPARN(I).AND.IHRI12.EQ.IPARN2(I))GOTO11815
11810   CONTINUE
      ENDIF
C
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11813)IHRI11,IHRI12
11813 FORMAT('       ',A4,A4,' NOT FOUND IN COMPIM PARAMETER LIST')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
11815 CONTINUE
      ILOCHV=I2
C
      ILOCVV=(-999)
      IF(NUMPV.GE.1)THEN
        DO11820I=1,NUMPV
          I2=I
          IF(ILEF11.EQ.IPARN(I).AND.ILEF12.EQ.IPARN2(I))GOTO11825
11820   CONTINUE
      ENDIF
C
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11823)ILEF11,ILEF12
11823 FORMAT('       ',A4,A4,' NOT FOUND IN COMPIM PARAMETER LIST')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
11825 CONTINUE
      ILOCVV=I2
C
      ILOCDV=(-999)
      IF(NUMVAL.LE.1)GOTO11839
      IF(NUMPV.GE.1)THEN
        DO11830I=1,NUMPV
          I2=I
          IF(ILEF21.EQ.IPARN(I).AND.ILEF22.EQ.IPARN2(I))THEN
            ILOCDV=I2
            GOTO11839
          ENDIF
11830   CONTINUE
      ENDIF
C
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11833)ILEF21,ILEF22
11833 FORMAT('       ',A4,A4,' NOT FOUND IN COMPIM PARAMETER LIST')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
11839 CONTINUE
C
C               ******************************************
C               **  STEP 11.9--                         **
C               **  COMPUTE THE RUNGE-KUTTA SOLUTION    **
C               ******************************************
C
11901 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
        ISTEPN='11.9'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11911)
11911   FORMAT('***** FROM DPRK, IMMEDIATELY BEFORE CALLING DPRK2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11912)N3,NUMPV,NUMDV
11912   FORMAT('N3,NUMPV,NUMDV = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO11914I=1,NUMDV
          WRITE(ICOUT,11915)I,IDUMV(I),IDUMV2(I)
11915     FORMAT('I,IDUMV(I),IDUMV2(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
11914   CONTINUE
        WRITE(ICOUT,11917)ICOLL1,ICOLL2,ICOLR1,NS1,NUMVAL
11917   FORMAT('ICOLL1,ICOLL2,ICOLR1,NS1,NUMVAL = ',5I8)
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,11918)NINEW,ICASL7,ICASEQ
11918   FORMAT('NINEW,ICASL7,ICASEQ = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IWRITE='ON'
      IF(IPRINT.EQ.'OFF')IWRITE='OFF'
      IF(IFEEDB.EQ.'OFF')IWRITE='OFF'
C
      CALL DPRK2(TEMPX,TEMPY,TEMPYD,NS1,ILOCHV,ILOCVV,ILOCDV,NUMVAL,
     1           IFUNC3,N3,PARAM,IPARN,IPARN2,NUMPV,
     1           IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1           IDUMV,IDUMV2,NUMDV,
     1           IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
C
      IFOUND='YES'
C
C               *******************************************
C               **  STEP 12--                            **
C               **  COPY THE OUTPUT VARIABLE             **
C               **  TO THE PROPER WORKSHEET COLUMN       **
C               *******************************************
C
      IF(ICASEQ.EQ.'FULL')GOTO12100
      IF(ICASEQ.EQ.'SUBS')GOTO12300
      IF(ICASEQ.EQ.'FOR')GOTO12500
C
C               *******************************************
C               **  STEP 12.1--                          **
C               **  TREAT THE FULL CASE.                 **
C               *******************************************
C
12100 CONTINUE
      ISTEPN='12.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NSX=0
      IF(NITEMX.LE.0)IROW1=0
      IF(NITEMX.LE.0)IROWN=0
      IF(NITEMX.LE.0)GOTO12190
      DO12110I=1,NITEMX
      NSX=I
C
      IJ=MAXN*(ICOLL1-1)+I
      IF(ICOLL1.LE.MAXCOL)V(IJ)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP1)PRED(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP2)RES(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP3)YPLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP4)XPLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP5)X2PLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP6)TAGPLO(I)=TEMPY(NSX)
C
      IF(NUMVAL.LE.1)GOTO12110
      IJ=MAXN*(ICOLL2-1)+I
      IF(ICOLL2.LE.MAXCOL)V(IJ)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP1)PRED(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP2)RES(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP3)YPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP4)XPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP5)X2PLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP6)TAGPLO(I)=TEMPYD(NSX)
C
12110 CONTINUE
12190 CONTINUE
C
      IF(NITEMX.GE.1)IROW1=1
      IF(NITEMX.GE.1)IROWN=NITEMX
      IN(ILISL1)=NITEMX
CCCCC IN(ICOLL1)=NITEMX
      IF(NUMVAL.EQ.2)IN(ILISL2)=NITEMX
C
      DO12210J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL1)GOTO12215
      GOTO12210
12215 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL1
      VALUE(J4)=ICOLL1
      IN(J4)=NITEMX
12210 CONTINUE
C
      IF(NUMVAL.LE.1)GOTO12229
      DO12220J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL2)GOTO12225
      GOTO12220
12225 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL2
      VALUE(J4)=ICOLL2
      IN(J4)=NITEMX
12220 CONTINUE
12229 CONTINUE
C
      GOTO13000
C
C               *******************************************
C               **  STEP 12.2--                          **
C               **  TREAT THE SUBSET CASE.               **
C               *******************************************
C
12300 CONTINUE
      ISTEPN='12.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NSX=0
      IF(NITEMX.LE.0)IROW1=0
      IF(NITEMX.LE.0)IROWN=0
      IF(NITEMX.LE.0)GOTO12390
      DO12310I=1,NITEMX
      IF(ISUB(I).EQ.0)GOTO12310
      NSX=NSX+1
C
      IJ=MAXN*(ICOLL1-1)+I
      IF(ICOLL1.LE.MAXCOL)V(IJ)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP1)PRED(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP2)RES(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP3)YPLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP4)XPLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP5)X2PLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP6)TAGPLO(I)=TEMPY(NSX)
      IF(NSX.EQ.1)IROW1=I
      IROWN=I
C
      IF(NUMVAL.LE.1)GOTO12310
      IJ=MAXN*(ICOLL2-1)+I
      IF(ICOLL2.LE.MAXCOL)V(IJ)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP1)PRED(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP2)RES(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP3)YPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP4)XPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP5)X2PLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP6)TAGPLO(I)=TEMPYD(NSX)
C
12310 CONTINUE
12390 CONTINUE
C
      IN(ILISL1)=NITEMX
CCCCC IN(ICOLL1)=NITEMX
      IF(NUMVAL.EQ.2)IN(ILISL2)=NITEMX
C
      DO12410J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL1)GOTO12415
      GOTO12410
12415 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL1
      VALUE(J4)=ICOLL1
      IN(J4)=NITEMX
12410 CONTINUE
C
      IF(NUMVAL.LE.1)GOTO12429
      DO12420J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL2)GOTO12425
      GOTO12420
12425 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL2
      VALUE(J4)=ICOLL2
      IN(J4)=NITEMX
12420 CONTINUE
12429 CONTINUE
C
      GOTO13000
C
C               *******************************************
C               **  STEP 12.3--                          **
C               **  TREAT THE FOR CASE.                  **
C               *******************************************
C
12500 CONTINUE
      ISTEPN='12.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NSX=0
      IF(NITEMX.LE.0)IROW1=0
      IF(NITEMX.LE.0)IROWN=0
      IF(NITEMX.LE.0)GOTO12590
      DO12510I=1,NITEMX
      IF(I.GT.NIFOR)GOTO12550
      IF(ISUB(I).EQ.0)GOTO12510
      NSX=NSX+1
      IJ=MAXN*(ICOLL1-1)+I
      IF(ICOLL1.LE.MAXCOL)V(IJ)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP1)PRED(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP2)RES(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP3)YPLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP4)XPLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP5)X2PLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP6)TAGPLO(I)=TEMPY(NSX)
      IF(NSX.EQ.1)IROW1=I
      IROWN=I
C
      IF(NUMVAL.LE.1)GOTO12510
      IJ=MAXN*(ICOLL2-1)+I
      IF(ICOLL2.LE.MAXCOL)V(IJ)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP1)PRED(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP2)RES(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP3)YPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP4)XPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP5)X2PLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP6)TAGPLO(I)=TEMPYD(NSX)
C
12510 CONTINUE
12590 CONTINUE
12550 CONTINUE
C
      IN(ILISL1)=NITEMX
CCCCC IN(ICOLL1)=NITEMX
      IF(NUMVAL.EQ.2)IN(ILISL2)=NITEMX
C
      DO12610J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL1)GOTO12615
      GOTO12610
12615 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL1
      VALUE(J4)=ICOLL1
      IN(J4)=NITEMX
12610 CONTINUE
C
      IF(NUMVAL.LE.1)GOTO12629
      DO12620J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL2)GOTO12625
      GOTO12620
12625 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL2
      VALUE(J4)=ICOLL2
      IN(J4)=NITEMX
12620 CONTINUE
12629 CONTINUE
C
      GOTO13000
C
C               *******************************************
C               **  STEP 13--                            **
C               **  CARRY OUT THE LIST UPDATING AND      **
C               **  GENERATE THE INFORMATIVE PRINTING    **
C               **  FOR STEP NUMBERS 7, 8, AND 9 ABOVE.  **
C               *******************************************
C
13000 CONTINUE
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHNAME(ILISL1)=ILEF11
      IHNAM2(ILISL1)=ILEF12
      IUSE(ILISL1)='V'
      IVALUE(ILISL1)=ICOLL1
      VALUE(ILISL1)=ICOLL1
CCCCC IUSE(ICOLL1)='V'
CCCCC IVALUE(ICOLL1)=ICOLL1
CCCCC VALUE(ICOLL1)=ICOLL1
      IF(NEWNA1.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWNA1.EQ.'YES')NUMCOL=NUMCOL+1
C
      IF(NUMVAL.LE.1)GOTO13009
      IHNAME(ILISL2)=ILEF21
      IHNAM2(ILISL2)=ILEF22
      IUSE(ILISL2)='V'
      IVALUE(ILISL2)=ICOLL2
      VALUE(ILISL2)=ICOLL2
CCCCC IUSE(ICOLL2)='V'
CCCCC IVALUE(ICOLL2)=ICOLL2
CCCCC VALUE(ICOLL2)=ICOLL2
      IF(NEWNA2.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWNA2.EQ.'YES')NUMCOL=NUMCOL+1
13009 CONTINUE
C
      IF(IPRINT.EQ.'OFF')GOTO13090
      IF(IFEEDB.EQ.'OFF')GOTO13090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,13011)ILEF11,ILEF12,NSX
13011 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
      IJ=MAXN*(ICOLL1-1)+IROW1
      IF(ICOLL1.LE.MAXCOL)WRITE(ICOUT,13021)ILEF11,ILEF12,V(IJ),IROW1
      IF(ICOLL1.LE.MAXCOL)CALL DPWRST('XXX','BUG ')
CCCCC THE FOLLOWING 4 LINES WERE FIXED   APRIL 1992
CCCCC IF(ICOLL1.EQ.MAXCP1)WRITE(ICOUT,13021)ILEF11,ILEF12,PRED(IROW1),IROW1
CCCCC IF(ICOLL1.EQ.MAXCP1)CALL DPWRST('XXX','BUG ')
CCCCC IF(ICOLL1.EQ.MAXCP2)WRITE(ICOUT,13021)ILEF11,ILEF12,RES(IROW1),IROW1
CCCCC IF(ICOLL1.EQ.MAXCP2)CALL DPWRST('XXX','BUG ')
CCCCC IF(ICOLL1.EQ.MAXCP3)WRITE(ICOUT,13021)ILEF11,ILEF12,YPLOT(IROW1),IROW1
CCCCC IF(ICOLL1.EQ.MAXCP3)CALL DPWRST('XXX','BUG ')
CCCCC IF(ICOLL1.EQ.MAXCP4)WRITE(ICOUT,13021)ILEF11,ILEF12,XPLOT(IROW1),IROW1
CCCCC IF(ICOLL1.EQ.MAXCP4)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP1)WRITE(ICOUT,13021)ILEF11,ILEF12,PRED(IROW1),
     1IROW1
13021 FORMAT('THE FIRST           COMPUTED VALUE OF ',A4,A4,
     1' = ',E15.7,'   (ROW ',I6,')')
      IF(ICOLL1.EQ.MAXCP1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP2)WRITE(ICOUT,13021)ILEF11,ILEF12,RES(IROW1),
     1IROW1
      IF(ICOLL1.EQ.MAXCP2)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP3)WRITE(ICOUT,13021)ILEF11,ILEF12,YPLOT(IROW1),
     1IROW1
      IF(ICOLL1.EQ.MAXCP3)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP4)WRITE(ICOUT,13021)ILEF11,ILEF12,XPLOT(IROW1),
     1IROW1
      IF(ICOLL1.EQ.MAXCP4)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP5)WRITE(ICOUT,13021)ILEF11,ILEF12,X2PLOT(IROW1),
     1IROW1
      IF(ICOLL1.EQ.MAXCP5)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP6)WRITE(ICOUT,13021)ILEF11,ILEF12,TAGPLO(IROW1),
     1IROW1
      IF(ICOLL1.EQ.MAXCP6)CALL DPWRST('XXX','BUG ')
C
      IJ=MAXN*(ICOLL1-1)+IROWN
      IF(ICOLL1.LE.MAXCOL.AND.
     1NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,V(IJ),IROWN
13031 FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',A4,A4,
     1' = ',E15.7,'   (ROW ',I6,')')
      IF(ICOLL1.LE.MAXCOL.AND.
     1NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP1.AND.
     1NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,PRED(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP1.AND.
     1NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP2.AND.
     1NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,RES(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP2.AND.
     1NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP3.AND.
     1NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,YPLOT(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP3.AND.
     1NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP4.AND.
     1NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,XPLOT(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP4.AND.
     1NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP5.AND.
     1NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,X2PLOT(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP5.AND.
     1NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP6.AND.
     1NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,TAGPLO(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP6.AND.
     1NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(NSX.NE.1)GOTO13039
      WRITE(ICOUT,13032)
13032 FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,13033)
13033 FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
      CALL DPWRST('XXX','BUG ')
13039 CONTINUE
C
      IF(NUMVAL.LE.1)GOTO13079
      WRITE(ICOUT,13051)ILEF21,ILEF22,NSX
13051 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
      IJ=MAXN*(ICOLL2-1)+IROW1
      IF(ICOLL2.LE.MAXCOL)THEN
         WRITE(ICOUT,13061)ILEF21,ILEF22,V(IJ),IROW1
13061    FORMAT('THE FIRST           COMPUTED VALUE OF ',
     1   A4,A4,' = ',E15.7,'   (ROW ',I6,')')
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL2.EQ.MAXCP1)THEN
         WRITE(ICOUT,13061)ILEF21,ILEF22,PRED(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL2.EQ.MAXCP2)THEN
         WRITE(ICOUT,13061)ILEF21,ILEF22,RES(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL2.EQ.MAXCP3)THEN
         WRITE(ICOUT,13061)ILEF21,ILEF22,YPLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL2.EQ.MAXCP4)THEN
         WRITE(ICOUT,13061)ILEF21,ILEF22,XPLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL2.EQ.MAXCP5)THEN
         WRITE(ICOUT,13061)ILEF21,ILEF22,X2PLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL2.EQ.MAXCP6)THEN
         WRITE(ICOUT,13061)ILEF21,ILEF22,TAGPLO(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IJ=MAXN*(ICOLL2-1)+IROWN
      IF(NSX.NE.1)THEN
         IF(ICOLL2.LE.MAXCOL)THEN
            WRITE(ICOUT,13071)NSX,ILEF21,ILEF22,V(IJ),IROWN
13071       FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',
     1      A4,A4,' = ',E15.7,'   (ROW ',I6,')')
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL2.EQ.MAXCP1)THEN
            WRITE(ICOUT,13071)NSX,ILEF21,ILEF22,PRED(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL2.EQ.MAXCP2)THEN
            WRITE(ICOUT,13071)NSX,ILEF21,ILEF22,RES(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL2.EQ.MAXCP3)THEN
            WRITE(ICOUT,13071)NSX,ILEF21,ILEF22,YPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL2.EQ.MAXCP4)THEN
            WRITE(ICOUT,13071)NSX,ILEF21,ILEF22,XPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL2.EQ.MAXCP5)THEN
            WRITE(ICOUT,13071)NSX,ILEF21,ILEF22,X2PLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL2.EQ.MAXCP6)THEN
            WRITE(ICOUT,13071)NSX,ILEF21,ILEF12,TAGPLO(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ENDIF
      ENDIF
      IF(NSX.NE.1)GOTO13079
      WRITE(ICOUT,13072)
13072 FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,13073)
13073 FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
      CALL DPWRST('XXX','BUG ')
13079 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
13090 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
19000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19011)
19011   FORMAT('***** AT THE END       OF DPRK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19012)IFOUND,IERROR,ICASL7,IWRITE
19012   FORMAT('IFOUND,IERROR,ICASL7,IWRITE = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19015)NSX,NITEMX
19015   FORMAT('NSX,NITEMX = ',I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19021)ILEF11,ILEF12,ILISL1,ICOLL1
19021   FORMAT('ILEF11,ILEF12,ILISL1,ICOLL1 = ',A4,2X,A4,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19022)ILEF21,ILEF22,ILISL2,ICOLL2
19022   FORMAT('ILEF21,ILEF22,ILISL2,ICOLL2 = ',A4,2X,A4,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19023)NUMVAL,NEWNA1,NEWNA2,NUMVAR
19023   FORMAT('NUMVAL,NEWNA1,NEWNA2,NUMVAR = ',I8,2X,A4,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19024)ILISR1,ICOLR1
19024   FORMAT('ILISR1,ICOLR1 = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRK2(X,Y,YD,N,ILOCHV,ILOCVV,ILOCDV,IORDER,
     1                 MODEL,NUMCHA,PARAM,IPARN,IPARN2,NUMPV,
     1                 IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                 IVARN,IVARN2,NUMVAR,
     1                 IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
C
C*****COPIED OVER ON TUESDAY NIGHT OCT 13, 1987 AFTER DISAPPEARANCE OF DPRK.FOR
C     AND STRANGE PRIVILEDGE MESSAGES ABOUT DPRK2.FOR
 
C     PURPOSE--COMPUTE THE RUNGE-KUTA SOLUTIONS
C              OF A FIRST- OR SECOND-ORDER DIFFERENTIAL EQUATION
C              OVER THE RANGE OF VALUES OF THE VARIABLE X.
C     NOTE--FOR FIRST-ORDER EQUATIONS,
C           X(1) AND Y(1) ARE THE INITIAL VALUES
C           FOR THE DIFFERENTIAL EQUATION--
C           THEY MUST PRE-EXIST.
C         --FOR SECOND-ORDER EQUATIONS,
C           X(1), Y(1), AND YD(1) ARE THE INITIAL VALUES
C           FOR THE DIFFERENTIAL EQUATION--
C           THEY MUST PRE-EXIST.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/10
C     ORIGINAL VERSION--SEPTEMBER  1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 MODEL
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IANGLU
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IVARN
      CHARACTER*4 IVARN2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION YD(*)
C
      DIMENSION MODEL(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN(*)
      DIMENSION IVARN2(*)
C
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRK'
      ISUBN2='2   '
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRK2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGCO,IBUGEV,ISUBRO,IANGLU
   52   FORMAT('IBUGA3,IBUGCO,IBUGEV,ISUBRO,IANGLU = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IORDER,N,Y(1),YD(1)
   53   FORMAT('IORDER,N,Y(1),YD(1) = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO57I=1,N
          WRITE(ICOUT,58)X(I)
   58     FORMAT('X(I) = ',G15.7)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
        WRITE(ICOUT,59)ILOCHV,ILOCVV,ILOCDV
   59   FORMAT('ILOCHV,ILOCVV,ILOCDV = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)NUMCHA,NUMPV,NUMVAR
   63   FORMAT('NUMCHA,NUMPV,NUMVAR, = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)(MODEL(J),J=1,MIN(100,NUMCHA))
   64   FORMAT('MODEL(I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        DO65I=1,NUMPV
          WRITE(ICOUT,66)I,PARAM(I),IPARN(I),IPARN2(I)
   66     FORMAT('I,PARAM(I),IPARN(I),IPARN2(I) = ',I8,G15.7,A4,A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO70I=1,NUMVAR
          WRITE(ICOUT,71)I,IVARN(I),IVARN2(I)
   71     FORMAT('I, IVARN(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
C
C               **************************************************
C               **  STEP 1--                                    **
C               **  BRANCH TO THE PROPER CASE                   **
C               **************************************************
C
      IF(IORDER.EQ.1)GOTO1100
      GOTO2100
C
C               **************************************************
C               **  STEP 11--                                   **
C               **  FOR THIS FIRST ORDER CASE,                  **
C               **  WRITE OUT  PRELIMINARY SUMMARY INFORMATION  **
C               **************************************************
C
 1100 CONTINUE
      IF(IPRINT.EQ.'OFF')GOTO1109
      IF(IFEEDB.EQ.'OFF')GOTO1109
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1101)
 1101 FORMAT('RUNGE-KUTTA DIFFERENTIAL EQUATION SOLUTION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1102)(MODEL(I),I=1,NUMCHA)
 1102 FORMAT('      FUNCTION--',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1103)IPARN(ILOCHV),IPARN2(ILOCHV),X(1)
 1103 FORMAT('      INITAL VALUE FOR ',A4,A4,' = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1104)IPARN(ILOCVV),IPARN2(ILOCVV),Y(1)
 1104 FORMAT('      INITAL VALUE FOR ',A4,A4,' = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1105)IPARN(ILOCHV),IPARN2(ILOCHV)
 1105 FORMAT('TOTAL LENGTH OF VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1106)N
 1106 FORMAT('(INCLUDING INITIAL VALUE IN ELEMENT 1) = ',I8)
      CALL DPWRST('XXX','BUG ')
 1109 CONTINUE
C
C               ***********************************************
C               **  STEP 12--                                **
C               **  FOR THIS FIRST ORDER CASE,               **
C               **  STEP THROUGH THE   VALUES OF THE         **
C               **  HORIZONTAL AXIS VARIABLE                 **
C               **  (THE VARIABLE OF DIFFERENTIATION)        **
C               **  AND COMPUTE THE RUNGE-KUTTA SOLUTIONS    **
C               ***********************************************
C
      DO1200I=2,N
C
      IM1=I-1
      X0=X(IM1)
      Y0=Y(IM1)
      H=X(I)-X(IM1)
C
C     STEP 11.1--
C
      XARG=X0
      YARG=Y0
CCCCC CALL FUNC(XARG,YARG,FOUT)
CCCCC AK1=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,
     1IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      AK1=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,1211)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK1
 1211 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK1 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 11.2--
C
      XARG=X0+H/2.0
      YARG=Y0+AK1/2.0
CCCCC CALL FUNC(XARG,YARG,FOUT)
CCCCC AK2=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,
     1IBUGCO,IBUGEV,IERROR)
      AK2=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,1212)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK2
 1212 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK2 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 11.3--
C
      XARG=X0+H/2.0
      YARG=Y0+AK2/2.0
CCCCC CALL FUNC(XARG,YARG,FOUT)
CCCCC AK3=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,
     1IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      AK3=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,1213)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK3
 1213 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK3 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 11.4--
C
      XARG=X0+H
      YARG=Y0+AK3
CCCCC CALL FUNC(XARG,YARG,FOUT)
CCCCC AK4=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,
     1IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      AK4=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,1214)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK4
 1214 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK4 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
      YNEXT=Y0+(AK1/6.0)+(AK2/3.0)+(AK3/3.0)+(AK4/6.0)
C
      Y(I)=YNEXT
C
 1200 CONTINUE
C
      GOTO9000
C
C               **************************************************
C               **  STEP 21--                                   **
C               **  FOR THIS SECOND ORDER CASE,                 **
C               **  WRITE OUT  PRELIMINARY SUMMARY INFORMATION  **
C               **************************************************
C
 2100 CONTINUE
      IF(IPRINT.EQ.'OFF')GOTO2109
      IF(IFEEDB.EQ.'OFF')GOTO2109
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2101)
 2101 FORMAT('RUNGE-KUTTA DIFFERENTIAL EQUATION SOLUTION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2102)(MODEL(I),I=1,NUMCHA)
 2102 FORMAT('      FUNCTION--',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2103)IPARN(ILOCHV),IPARN2(ILOCHV),X(1)
 2103 FORMAT('      INITAL VALUE FOR ',A4,A4,' = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2104)IPARN(ILOCVV),IPARN2(ILOCVV),Y(1)
 2104 FORMAT('      INITAL VALUE FOR ',A4,A4,' = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2105)IPARN(ILOCDV),IPARN2(ILOCDV),YD(1)
 2105 FORMAT('      INITAL VALUE FOR ',A4,A4,' = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2106)IPARN(ILOCHV),IPARN2(ILOCHV)
 2106 FORMAT('TOTAL LENGTH OF VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2107)N
 2107 FORMAT('(INCLUDING INITIAL VALUE IN ELEMENT 1) = ',I8)
      CALL DPWRST('XXX','BUG ')
 2109 CONTINUE
C
C               ***********************************************
C               **  STEP 22--                                **
C               **  FOR THIS SECOND ORDER CASE,              **
C               **  STEP THROUGH THE   VALUES OF THE         **
C               **  HORIZONTAL AXIS VARIABLE                 **
C               **  (THE VARIABLE OF DIFFERENTIATION)        **
C               **  AND COMPUTE THE RUNGE-KUTTA SOLUTIONS    **
C               ***********************************************
C
      DO2200I=2,N
C
      IM1=I-1
      X0=X(IM1)
      Y0=Y(IM1)
      YD0=YD(IM1)
      H=X(I)-X(IM1)
C
C     STEP 22.1--
C
      XARG=X0
      YARG=Y0
      YDARG=YD0
CCCCC FOUT=YDARG
CCCCC AK1=H*FOUT
      FOUT=YDARG
      AK1=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,2211)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK1
 2211 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK1 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 22.2--
C
      XARG=X0
      YARG=Y0
      YDARG=YD0
CCCCC CALL FUNC(XARG,YARG,YDARG,FOUT)
CCCCC AK1=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      PARAM(ILOCDV)=YDARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,
     1IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      AL1=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,2212)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL1
 2212 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL1 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 22.3--
C
      XARG=X0+H/2.0
      YARG=Y0+AK1/2.0
      YDARG=YD0+AL1/2.0
CCCCC FOUT=YDARG
CCCCC AK2=H*FOUT
      FOUT=YDARG
      AK2=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,2213)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK2
 2213 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK2 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 22.4--
C
      XARG=X0+H/2.0
      YARG=Y0+AK1/2.0
      YDARG=YD0+AL1/2.0
CCCCC CALL FUNC(XARG,YARG,YDARG,FOUT)
CCCCC AK2=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      PARAM(ILOCDV)=YDARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,
     1IBUGCO,IBUGEV,IERROR)
      AL2=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,2214)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL2
 2214 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL2 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 22.5--
C
      XARG=X0+H/2.0
      YARG=Y0+AK2/2.0
      YDARG=YD0+AL2/2.0
CCCCC FOUT=YDARG
CCCCC AK3=H*FOUT
      FOUT=YDARG
      AK3=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,2215)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK3
 2215 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK3 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 22.6--
C
      XARG=X0+H/2.0
      YARG=Y0+AK2/2.0
      YDARG=YD0+AL2/2.0
CCCCC CALL FUNC(XARG,YARG,YDARG,FOUT)
CCCCC AK3=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      PARAM(ILOCDV)=YDARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,
     1IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      AL3=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,2216)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL3
 2216 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL3 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 22.7--
C
      XARG=X0+H
      YARG=Y0+AK3
      YDARG=YD0+AL3
CCCCC FOUT=YDARG
CCCCC AK4=H*FOUT
      FOUT=YDARG
      AK4=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,2217)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK4
 2217 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK4 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
C     STEP 22.8--
C
      XARG=X0+H
      YARG=Y0+AK3
      YDARG=YD0+AL3
CCCCC CALL FUNC(XARG,YARG,YDARG,FOUT)
CCCCC AK4=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      PARAM(ILOCDV)=YDARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,
     1IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      AL4=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1WRITE(ICOUT,2218)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL4
 2218 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL4 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')
     1CALL DPWRST('XXX','BUG ')
C
      YNEXT=Y0+(AK1/6.0)+(AK2/3.0)+(AK3/3.0)+(AK4/6.0)
      YDNEXT=YD0+(AL1/6.0)+(AL2/3.0)+(AL3/3.0)+(AL4/6.0)
C
      Y(I)=YNEXT
      YD(I)=YDNEXT
C
 2200 CONTINUE
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRK2--')
        CALL DPWRST('XXX','BUG ')
        IF(N.GE.1)THEN
          DO9020I=1,N
            IF(IORDER.EQ.1)THEN
              WRITE(ICOUT,9021)I,X(I),Y(I)
 9021         FORMAT('I,X(I),Y(I) = ',I8,3G15.7)
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IORDER.EQ.2)THEN
              WRITE(ICOUT,9022)I,X(I),Y(I),YD(I)
 9022         FORMAT('I,X(I),Y(I),YD(I) = ',I8,3G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
 9020     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
